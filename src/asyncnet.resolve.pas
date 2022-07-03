unit asyncnet.resolve;

{$Mode ObjFpc}
{$H+}      
{$TypedAddress ON}

interface

uses Classes, SysUtils, stax, AsyncNet.sockets, AsyncNet.netdb;

type
  TDNSResolveResult = record
    Address: TNetworkAddress;
    TTL: QWord;
  end;

  { TDNSResolveTask }

  TDNSResolveTask = class(specialize TRVTask<TDNSResolveResult>)
  private
    FHostName: String;
    FDNSServer: TNetworkAddress;
    FTimeOut: Integer;
    FAddressType: TAddressType;
    FRecursionDepth: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(const AHostName: String; const ADNSServer: TNetworkAddress;
                       AddressType: TAddressType = atIN4; ATimeOut: Integer = DefaultDNSTimeout;
                       ARecursionDepth: Integer = DefaultDNSRecursionDepth);
  end;

  { TResolveNameTask }

  TResolveNameTask = class(specialize TRVTask<TDNSResolveResult>)
  private
    FHostName: String;
    FPreferredType: TAddressType;

    function ResolveHostsDB(const AHostNames: TStringArray): Boolean;
    function ResolveDNS(const AHostNames: TStringArray; DNSServers: TStringList;
                        Timeout: QWord; Attempts: Integer): Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(const AHostName: String; APreferredType: TAddressType = atIN4);
  end;

// Async functions
function AsyncDNSResolve(const AHostName: String; const ADNSServer: TNetworkAddress;
                       AAddressType: TAddressType = atIN4; ATimeOut: Integer = DefaultDNSTimeout;
                       ARecursionDepth: Integer = DefaultDNSRecursionDepth): TDNSResolveTask; inline;
function AsyncResolveName(const AHostName: String; PreferredType: TAddressType = atIN4): TResolveNameTask; inline;

implementation

uses sockets, AsyncNet.dns, AsyncNet.dns.resrecords;

function AsyncDNSResolve(const AHostName: String;
  const ADNSServer: TNetworkAddress; AAddressType: TAddressType;
  ATimeOut: Integer; ARecursionDepth: Integer): TDNSResolveTask;
begin
  Result := TDNSResolveTask.Create(AHostName, ADNSServer, AAddressType, ATimeOut, ARecursionDepth);
end;

function AsyncResolveName(const AHostName: String; PreferredType: TAddressType
  ): TResolveNameTask;
begin
  Result := TResolveNameTask.Create(AHostName, PreferredType);
end;

{ TDNSResolveTask }

procedure TDNSResolveTask.Execute;
var
  Resp: TDNSResponse;
  Questions: TDNSQuestionArray;
  Start, TimeTaken, TTL: QWord;
begin
  FResult := Default(TDNSResolveResult);

  // Error checking
  if FRecursionDepth <= 0 then
    raise EMaxRecursionDepthExceeded.Create('CNAME chain length exceeded maximum recursion depth');

  // DNS query
  Questions := Default(TDNSQuestionArray);
  SetLength(Questions, 1);
  if (FAddressType = atIN6) then
    Questions[0] := DNSQuestion(FHostName, rrtAAAA)
  else
    Questions[0] := DNSQuestion(FHostName, rrtA);

  start := GetTickCount64;
  Resp := specialize Await<TDNSResponse>(AsyncDNSRequest(FDNSServer,
                                                         Questions, FTimeOut));
  TimeTaken := GetTickCount64 - Start;

  // Parse result
  if (FAddressType = atIN6) and (Length(Resp.Answers.AAAARecords) > 0) then
  begin
    FResult.Address := IN6Address(HostAddrToStr6(Resp.Answers.AAAARecords[0].Data));
    FResult.TTL := Resp.Answers.AAAARecords[0].TTL;
  end
  else if (FAddressType = atIN4) and (Length(Resp.Answers.ARecords) > 0) then
  begin
    FResult.Address := IN4Address(HostAddrToStr(Resp.Answers.ARecords[0].Data));
    FResult.TTL := Resp.Answers.ARecords[0].TTL;
  end
  else if Length(Resp.Answers.CNAMERecords) > 0 then
  begin
    TTL := Resp.Answers.CNAMERecords[0].TTL;
    FResult := specialize Await<TDNSResolveResult>(TDNSResolveTask.Create(Resp.Answers.CNAMERecords[0].Data,
                                                  FDNSServer, FAddressType, FTimeOut - TimeTaken,
                                                  FRecursionDepth - 1));
    // The TTL of the query result is the minimum of the cname chain
    if FResult.TTL > TTL then
      FResult.TTL := TTL;
  end
  else
    raise EQueryUnseccesfull.Create('Unable to resolve "' + FHostName + '" at "' + FDNSServer.Address+ '"');
end;

{ TResolveNameTask }

function TResolveNameTask.ResolveHostsDB(const AHostNames: TStringArray
  ): Boolean;
var
  HostName: String;
  Entry: THostEntry;
  Addr: TNetworkAddress;
begin
  Result := False;
  HostsDB.BeginAccess;
  try
    for HostName in AHostNames do
      if HostsDB.TryFindHost(HostName, Entry) then
      begin
        FResult.Address := Entry.Addresses[0];
        for Addr in Entry.Addresses do
          if Addr.AddressType = FPreferredType then
          begin
            FResult.Address := Addr;
            Break;
          end;
        FResult.TTL := QWord.MaxValue;
        Exit(True);
      end;
  finally
    HostsDB.EndAccess;
  end;
end;

function TResolveNameTask.ResolveDNS(const AHostNames: TStringArray;
  DNSServers: TStringList; Timeout: QWord; Attempts: Integer): Boolean;
var
  ServerIndex: Integer;
  HostName: String;
  OtherType: TAddressType;
  DNSServer: TNetworkAddress;
begin
  Result := False;
  ServerIndex := 0;
  // try Attempts many times
  while (Attempts > 0) and not Result do
  begin
    DNSServer := INAddr(DNSServers[ServerIndex]);
    // Try each of the hostnames
    for HostName in AHostNames do
    begin
      // first try preferred
      try
        FResult := specialize Await<TDNSResolveResult>(AsyncDNSResolve(HostName,
                                                                       DNSServer,
                                                                       FPreferredType,
                                                                       Timeout));
        // No exception: successful attempt
        Result := True;
        Break;
      except
        on E: ETaskTerminatedException do
          raise E; // if terminated kill by re-raising
        on E: Exception do; // Ignore all other exceptions
      end;
      // Preferred type not found, search the other type
      if FPreferredType = atIN4 then
        OtherType := atIN6
      else
        OtherType := atIN4;
      try
        FResult := specialize Await<TDNSResolveResult>(AsyncDNSResolve(HostName,
                                                                       DNSServer,
                                                                       OtherType,
                                                                       Timeout)); // No exception: successful attempt
        Result := True;
        Break;
      except
        on E: ETaskTerminatedException do
          raise E; // if terminated kill by re-raising
        on E: Exception do; // Ignore all other exceptions
      end;
    end;
    // None found/failures: retry on new server
    Dec(Attempts);
    ServerIndex := (ServerIndex + 1) mod DNSServers.Count;
  end;
end;

procedure TResolveNameTask.Execute;
var
  Attempts, Timeout: Integer;
  DNSServers: TStringList;
  HostNames: TStringArray;
begin
  FResult := Default(TDNSResolveResult);
  DNSServers := TStringList.Create;
  try
    // Retrieving parameter from ResolveDB
    ResolveDB.BeginAccess;
    try
      DNSServers.AddStrings(ResolveDB.DNSServers);
      Timeout := ResolveDB.DNSTimeOut;
      Attempts := ResolveDB.Attempts;
      HostNames := ResolveDB.ResolutionNames[FHostName];
    finally
      ResolveDB.EndAccess;
    end;
    // First try to resolve from hosts file
    if ResolveHostsDB(HostNames) then
      Exit;
    // If not found resolve using DNS
    if not ResolveDNS(HostNames, DNSServers, Timeout, Attempts) then
      raise ENoSuchEntry.Create('Unable resolve ' + FHostName);
  finally
    DNSServers.Free;
  end;
end;

constructor TResolveNameTask.Create(const AHostName: String;
  APreferredType: TAddressType);
begin
  inherited Create;
  FHostName := AHostName;
  FPreferredType := APreferredType;
end;

constructor TDNSResolveTask.Create(const AHostName: String;
  const ADNSServer: TNetworkAddress; AddressType: TAddressType;
  ATimeOut: Integer; ARecursionDepth: Integer);
begin
  inherited Create;
  FHostName := AHostName;
  FDNSServer := ADNSServer;
  FAddressType := AddressType;
  FTimeOut := ATimeOut;
  FRecursionDepth := ARecursionDepth;
end;

end.
