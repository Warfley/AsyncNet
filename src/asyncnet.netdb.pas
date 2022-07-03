{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    Implement networking routines.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$h+}
{$ModeSwitch arrayoperators}
{$TypedAddress on}

unit asyncnet.netdb;

interface

uses SysUtils, Classes, Generics.Collections, Sockets, stax, AsyncNet.sockets;

{$IFDEF OS2}
(* ETC directory location determined by environment variable ETC *)
 {$DEFINE ETC_BY_ENV}
(* Use names supported also on non-LFN drives like plain FAT-16. *)
 {$DEFINE SFN_VERSION}
{$ENDIF OS2}
{$IFDEF GO32V2}
 {$DEFINE ETC_BY_ENV}
 {$DEFINE SFN_VERSION}
{$ENDIF GO32V2}
{$IFDEF WATCOM}
 {$DEFINE ETC_BY_ENV}
 {$DEFINE SFN_VERSION}
{$ENDIF WATCOM}

type
  ENoSuchEntry = class(Exception);
  ENoDNSServer = class(Exception);
  EQueryUnseccesfull = class(Exception);
  EMaxRecursionDepthExceeded = class(Exception);

Const
  MaxResolveAddr = 10;
  DNSPort        = 53;
  SServicesFile  = 'services';
  SHostsFile     = 'hosts';
  SNetworksFile  = 'networks';
  {$IfDef WINDOWS}
  SProtocolFile  = 'protocol';
  SResolveFile   = 'resolv';
  {$Else Windows}
{$IFDEF SFN_VERSION}
  SProtocolFile  = 'protocol';
  SResolveFile   = 'resolv';
 {$IFDEF OS2}
(* Peculiarity of OS/2 - depending on the used TCP/IP version, *)
(* the file differs slightly in name and partly also content.  *)
   SResolveFile2  = 'resolv2';
 {$ENDIF OS2}
{$ELSE SFN_VERSION}
  SProtocolFile  = 'protocols';
  SResolveFile   = 'resolv.conf';
{$ENDIF SFN_VERSION}
{$EndIf Windows}

  DefaultDNSTimeout = 5000;
  DefaultDNSAttempts = 3;
  DefaultDNSRecursionDepth = 10;

type
  TServiceEntry = record
    Name     : String;
    Protocol : String;
    Port     : Word;
    Aliases  : array of String;
  end;
  TServiceList = specialize TList<TServiceEntry>;

  THostEntry = record
    Name : String;
    Addresses : TNetworkAddressArray;
    Aliases : array of String;
  end;
  THostMap = specialize TDictionary<String, THostEntry>;

  TNetworkEntry = Record
    Name : String;
    Addr : TNetworkAddress;
    Aliases : array of String;
  end;
  TNetworkMap = specialize TDictionary<String, TNetworkEntry>;

  TProtocolEntry = Record
    Name : String;
    Number : Integer;
    Aliases : array of String;
  end;
  TProtocolMap = specialize TDictionary<String, TProtocolEntry>;

  { TConfFileDB }

  TConfFileDB = class
  protected type
    TTokenizedLine = array of String;
  private
    FFileName: String;
    FLastRead: LongInt;
    FLock: TRTLCriticalSection;

    function ParseLine(const ALine: String): TTokenizedLine;
  protected 
    procedure ParseEntry(const ALine: TTokenizedLine); virtual; abstract;
    procedure Update; virtual;
  public
    constructor Create(const AFileName: String);
    destructor Destroy; override;

    procedure BeginAccess; inline;
    procedure EndAccess; inline;
  end;

  { TResolveDB }

  TResolveDB = class(TConfFileDB)
  private
    class var Singleton: TResolveDB;

    class constructor InitStatics;
    class destructor CleanupStatics;
  public
    class function Instance: TResolveDB; static; inline;
  public type
    TResolveNames = array of String;
  private
    FDNSServers: TStringList;
    FSearchDomains: TStringList;
    FDNSTimeOut: Integer;
    FAttempts: Integer;
    // Number of labels required for a direct resolve
    FLabelThreshold: Integer;

    function CountLabels(const AName: String): SizeInt;
  protected
    procedure Update; override;
    procedure ParseEntry(const ALine: TTokenizedLine); override;
  public
    constructor Create;
    destructor Destroy; override;

    function GenerateResolutionNames(const ADomain: String): TResolveNames;
         
    property DNSServers: TStringList read FDNSServers;
    property SearchDomains: TStringList read FSearchDomains;
    property DNSTimeOut: Integer read FDNSTimeOut;
    property Attempts: Integer read FAttempts;
    property LabelThreshold: Integer read FLabelThreshold;
    property ResolutionNames[const ADomain: String]: TResolveNames read GenerateResolutionNames;
  end;

  { TServiceDB }

  TServiceDB = class(TConfFileDB)
  private
    class var Singleton: TServiceDB;

    class constructor InitStatics;
    class destructor CleanupStatics;
  public
    class function Instance: TServiceDB; static; inline;
  public type
    TServiceArray = array of TServiceEntry;
  private
    FServices: TServiceList;

  protected
    procedure Update; override;
    procedure ParseEntry(const ALine: TTokenizedLine); override;
  public
    constructor Create;
    destructor Destroy; override;

    function TryFindServices(const NameOrAlias: String): TServiceArray; overload;
    function TryFindService(APort: Word): TServiceArray; overload;

  private
    function GetService(const NameOrAlias: String): TServiceArray; inline;
    function GetServiceByPort(APort: Word): TServiceArray; inline;
  public
    property Services: TServiceList read FServices;
    property Service[const NameOrAlias: String]: TServiceArray read GetService; default;
    property ServiceByPort[APort: Word]: TServiceArray read GetServiceByPort;
  end;

  { THostsDB }

  THostsDB = class(TConfFileDB)
  private
    class var Singleton: THostsDB;

    class constructor InitStatics;
    class destructor CleanupStatics;
  public
    class function Instance: THostsDB; static; inline;
  private
    FHosts: THostMap;

  protected
    procedure Update; override;
    procedure ParseEntry(const ALine: TTokenizedLine); override;
  public
    constructor Create;
    destructor Destroy; override;

    function TryFindHost(const NameOrAlias: String; out AEntry: THostEntry): Boolean; overload;
    function TryFindHost(const HostAddress: TNetworkAddress; out AEntry: THostEntry): Boolean; overload;

  private
    function GetHost(const NameOrAlias: String): THostEntry; inline;
    function GetHostByAddress(const HostAddress: TNetworkAddress): THostEntry; inline;
    function GetHosts: THostMap.TValueCollection; inline;
  public
    property Hosts: THostMap.TValueCollection read GetHosts;
    property Host[const NameOrAlias: String]: THostEntry read GetHost; default;
    property HostByAddress[const HostAddress: TNetworkAddress]: THostEntry read GetHostByAddress;
  end;

  { TNetworksDB }

  TNetworksDB = class(TConfFileDB)
  private
    class var Singleton: TNetworksDB;

    class constructor InitStatics;
    class destructor CleanupStatics;
  public
    class function Instance: TNetworksDB; static; inline;
  private
    FNetworks: TNetworkMap;

    function CompleteNetworkAddress(const Addr: String): String;
  protected
    procedure Update; override;
    procedure ParseEntry(const ALine: TTokenizedLine); override;
  public
    constructor Create;
    destructor Destroy; override;

    function TryFindNetwork(const NameOrAlias: String; out AEntry: TNetworkEntry): Boolean; overload;
    function TryFindNetwork(const NetworkAddress: TNetworkAddress; out AEntry: TNetworkEntry): Boolean; overload;

  private
    function GetNetwork(const NameOrAlias: String): TNetworkEntry; inline;
    function GetNetworkByAddress(const NetworkAddress: TNetworkAddress): TNetworkEntry; inline;
    function GetNetworks: TNetworkMap.TValueCollection; inline;
  public
    property Networks: TNetworkMap.TValueCollection read GetNetworks;
    property Network[const NameOrAlias: String]: TNetworkEntry read GetNetwork; default;
    property NetworkByAddress[const NetworkAddress: TNetworkAddress]: TNetworkEntry read GetNetworkByAddress;
  end;

  { TProtocolDB }

  TProtocolDB = class(TConfFileDB)
  private
    class var Singleton: TProtocolDB;

    class constructor InitStatics;
    class destructor CleanupStatics;
  public
    class function Instance: TProtocolDB; static; inline;
  private
    FProtocols: TProtocolMap;

  protected
    procedure Update; override;
    procedure ParseEntry(const ALine: TTokenizedLine); override;
  public
    constructor Create;
    destructor Destroy; override;

    function TryFindProtocol(const NameOrAlias: String; out AEntry: TProtocolEntry): Boolean; overload;
    function TryFindProtocol(Number: Integer; out AEntry: TProtocolEntry): Boolean; overload;

  private
    function GetProtocol(const NameOrAlias: String): TProtocolEntry; inline;
    function GetProtocolByNumber(Number: Integer): TProtocolEntry; inline;
    function GetProtocols: TProtocolMap.TValueCollection; inline;
  public
    property Protocols: TProtocolMap.TValueCollection read GetProtocols;
    property Protocol[const NameOrAlias: String]: TProtocolEntry read GetProtocol; default;
    property ProtocolByNumber[Number: Integer]: TProtocolEntry read GetProtocolByNumber;
  end;

// easy singleton access
function ResolveDB: TResolveDB; inline;
function ServiceDB: TServiceDB; inline;
function HostsDB: THostsDB; inline;
function NetworksDB: TNetworksDB; inline;
function ProtocolDB: TProtocolDB; inline;

implementation
{$IfDef WINDOWS}uses JwaIpTypes, JwaIpHlpApi, windows{$EndIf};

function ETCPath: String; inline;
begin
  Result := '';
 {$If defined(ETC_BY_ENV)}
  Result := GetEnvironmentVariable('ETC');
  if Result <> '' then
   Result := IncludeTrailingPathDelimiter(Result);
 {$ElseIf defined(UNIX)}
 Result := '/etc/';
 {$ElseIf defined(WINDOWS)}
 SetLength(Result, 256);
 SetLength(Result, GetSystemDirectory(LPSTR(Result), 256));
 Result += '\drivers\etc\';
 {$Else}
 {$WARNING Support for finding /etc/ directory not implemented for this platform!}
 {$EndIf}
end;

function ResolveDB: TResolveDB;
begin
  Result := TResolveDB.Singleton;
end;

function ServiceDB: TServiceDB;
begin
  Result := TServiceDB.Singleton;
end;

function HostsDB: THostsDB;
begin
  Result := THostsDB.Singleton;
end;

function NetworksDB: TNetworksDB;
begin
  Result := TNetworksDB.Singleton;
end;

function ProtocolDB: TProtocolDB;
begin
  Result := TProtocolDB.Singleton;
end;

{$if defined(android)}

procedure GetDNSServers(DNSList: TStringList);
var
  i: integer;
  s: string;
begin
  DNSList.Clear;
  if SystemApiLevel >= 26 then
  begin
    // Since Android 8 the net.dnsX properties can't be read.
    // Use Google Public DNS servers
    SetLength(DNSServers, Result);
    DNSList.Add('8.8.8.8');
    DNSList.Add('8.8.4.4');
    exit;
  end;

  for i:=1 to 9 do
  begin
    s:=GetSystemProperty(PAnsiChar('net.dns' + IntToStr(i)));
    if s = '' then
      break;
    DNSList.Add(s);
  end;
end;

{$ElseIf defined(Windows)}

procedure GetDNSServers(DNSList: TStringList);
var
  pFI: PFIXED_INFO;
  pIPAddr: PIPAddrString;
  OutLen: ULONG;
begin
  OutLen := SizeOf(TFixedInfo);
  GetMem(pFI, SizeOf(TFixedInfo));
  try
    if GetNetworkParams(pFI, OutLen) = ERROR_BUFFER_OVERFLOW then
    begin
      ReallocMem(pFI, OutLen);
      if GetNetworkParams(pFI, OutLen) <> NO_ERROR then Exit;
    end;
    // If there is no network available there may be no DNS servers defined
    if pFI^.DnsServerList.IpAddress.S[0] = #0 then Exit;
    // Add first server
    DNSList.Add(PChar(@pFI^.DnsServerList.IpAddress));
    // Add rest of servers
    pIPAddr := pFI^.DnsServerList.Next;
    while Assigned(pIPAddr) do
    begin
      DNSList.Add(PChar(@pIPAddr^.IpAddress));
      pIPAddr := pIPAddr^.Next;
    end;
  finally
    FreeMem(pFI);
  end;
end;

{$endif defined(android) | defined(windows)}

{ TConfFileDB }

function TConfFileDB.ParseLine(const ALine: String): TTokenizedLine;
const
  ExtendSize = 3;
var
  Start: SizeInt;
  Len: SizeInt;
  TokenCount: SizeInt;
  Token: String;
begin
  Result := Default(TTokenizedLine);
  Start := 1;
  Len := 0;
  TokenCount := 0;
  while Start + Len <= ALine.Length do
    case ALine[Start + Len] of
      #9, ' ':
        if Len > 0 then // new token finished
        begin
          Token := Copy(ALine, Start, Len);
          if Length(Result) >= TokenCount then
            SetLength(Result, Length(Result) + ExtendSize);
          Result[TokenCount] := Token;
          Inc(TokenCount);
          Start := Start + Len + 1;
          Len := 0;
        end
        else // no new token just move cursor
          Inc(Start);
      '#': Break; // comment: end of line
      otherwise Inc(len);
    end;
  // add final token
  if Len > 0 then
  begin
    Token := Copy(ALine, Start, Len);
    if Length(Result) >= TokenCount then
      SetLength(Result, TokenCount + 1);
    Result[TokenCount] := Token;
    Inc(TokenCount);
  end;
  // Shrink to fit
  SetLength(Result, TokenCount);
end;

procedure TConfFileDB.Update;
var
  ConfFile: TextFile;
  ConfLine: String;
  ParsedLine: TTokenizedLine;
begin
  Assign(ConfFile, FFileName);
  try
    Reset(ConfFile);
    // Read line  by line
    while not EOF(ConfFile) do
    begin
      ReadLn(ConfFile, ConfLine);
      // Tokenize
      ParsedLine := ParseLine(ConfLine);
      if Length(ParsedLine) = 0 then
        Continue; // skip empty lines
      // Call virtual method of child to handle line
      ParseEntry(ParsedLine);
    end;
  finally
    Close(ConfFile);
  end;
  FLastRead := DateTimeToFileDate(Now);
end;

constructor TConfFileDB.Create(const AFileName: String);
begin
  FFileName := AFileName;
  FLastRead := 0;
  InitCriticalSection(FLock);
end;

destructor TConfFileDB.Destroy;
begin
  DoneCriticalSection(FLock);
  inherited Destroy;
end;

procedure TConfFileDB.BeginAccess;
begin
  EnterCriticalSection(FLock);
  try
    // Check timestamp if we need to update
    if FileExists(FFileName) and (FileAge(FFileName) >= FLastRead) then
      Update;
  except
    LeaveCriticalSection(FLock);
    raise;
  end;
end;

procedure TConfFileDB.EndAccess;
begin
  LeaveCriticalSection(FLock);
end;

{ TResolveDB }

class constructor TResolveDB.InitStatics;
begin
  Singleton := TResolveDB.Create;
end;

class destructor TResolveDB.CleanupStatics;
begin
  Singleton.Free;
end;

class function TResolveDB.Instance: TResolveDB;
begin
  Result := Singleton;
end;

function TResolveDB.CountLabels(const AName: String): SizeInt;
var
  i: SizeInt;
begin
  Result := 1;
  for i:=1 to Length(AName) do
    if AName[i] = '.' then
      Inc(Result);
end;

procedure TResolveDB.Update;
begin
  FSearchDomains.Clear;
  FDNSServers.Clear;
  FDNSTimeOut := DefaultDNSTimeout;
  FAttempts := DefaultDNSTimeout;
  FLabelThreshold := 2;
  // no resolv file on windows and android
  {$If defined(android) or defined(WINDOWS)}
  if FDNSServers.Count = 0 then
    GetDNSServers(FDNSServers);
  {$Else}
  inherited Update;
  {$EndIf}
end;

procedure TResolveDB.ParseEntry(const ALine: TTokenizedLine);

procedure ParseOptions;
var
  i: SizeInt;
  SplitPoint: SizeInt;
  CurrOpt, OptVal, OptName: String;
begin
  for i:=1 to Length(ALine) - 1 do
  begin
    CurrOpt := ALine[i];
    SplitPoint := Pos(':', CurrOpt) - 1;
    if SplitPoint < 0 then // unrecognized option
      Continue;
    OptName := CurrOpt.Substring(0, SplitPoint);
    OptVal := CurrOpt.Substring(SplitPoint + 1);
    case OptName of
      'ndots': FLabelThreshold := OptVal.ToInteger + 1; // num labels = num dots + 1
      'attempts': FAttempts := OptVal.ToInteger;
      'timeout': FDNSTimeOut := OptVal.ToInteger * 1000; // secs to ms
    end;
  end;
end;

begin
  // Parsing according to manual: https://linux.die.net/man/5/resolv.conf
  if Length(ALine) < 2 then Exit;
  case ALine[0].ToLower of
  'nameserver': FDNSServers.Add(ALine[1]);
  'domain': FSearchDomains.Add(ALine[1]);
  'search': FSearchDomains.Add(ALine[1]);
  'options': ParseOptions;
  end;
end;

constructor TResolveDB.Create;
begin
  inherited Create(ETCPath + SResolveFile);
  FDNSServers := TStringList.Create;
  FSearchDomains := TStringList.Create;
  {$If defined(WINDOWS) or defined(android)}
  Update;
  {$EndIf}
end;

destructor TResolveDB.Destroy;
begin
  FDNSServers.Free;
  FSearchDomains.Free;
  inherited Destroy;
end;

function TResolveDB.GenerateResolutionNames(const ADomain: String): TResolveNames;
var
  i: SizeInt;
  DirectResolve: Boolean;
begin
  Result := Default(TResolveNames);
  SetLength(Result, FSearchDomains.Count + 1);
  DirectResolve := CountLabels(ADomain) >= FLabelThreshold;
  if DirectResolve then
  begin
    // Resolve the name first
    Result[0] := ADomain;
    for i := 1 to FSearchDomains.Count do
      Result[i] := ADomain + '.' + FSearchDomains[i - 1];
  end
  else
  begin
    // Resolve searchdomains first
    for i := 0 to FSearchDomains.Count - 1 do
      Result[i] := ADomain + '.' + FSearchDomains[i];
    // and this name last
    Result[FSearchDomains.Count] := ADomain;
  end;
end;

{ TServiceDB }

class constructor TServiceDB.InitStatics;
begin
  Singleton := TServiceDB.Create;
end;

class destructor TServiceDB.CleanupStatics;
begin
  Singleton.Free;
end;

class function TServiceDB.Instance: TServiceDB;
begin
  Result := Singleton;
end;

procedure TServiceDB.Update;
begin
  FServices.Clear;
  inherited Update;
end;

procedure TServiceDB.ParseEntry(const ALine: TTokenizedLine);
var
  Entry: TServiceEntry;
  i: Integer;
  PortProtoSeperator: SizeInt;
begin
  // Malformed line
  if Length(ALine) < 2 then Exit;
  Entry := Default(TServiceEntry);
  Entry.Name := ALine[0];
  PortProtoSeperator := Pos('/', ALine[1]) - 1;
  if PortProtoSeperator < 0 then Exit;
  Entry.Port := ALine[1].Substring(0, PortProtoSeperator).ToInteger;
  Entry.Protocol := ALine[1].Substring(PortProtoSeperator + 1);
  SetLength(Entry.Aliases, Length(ALine) - 2);
  for i := 2 to Length(ALine) - 1 do
    Entry.Aliases[i - 2] := ALine[i];
  FServices.Add(Entry);
end;

constructor TServiceDB.Create;
begin
  inherited Create(ETCPath + SServicesFile);
  FServices := TServiceList.Create;
end;

destructor TServiceDB.Destroy;
begin
  FServices.Free;
  inherited Destroy;
end;

function TServiceDB.TryFindServices(const NameOrAlias: String): TServiceArray;
var
  i: SizeInt;
  Entry: TServiceEntry;
begin
  Result := Default(TServiceArray);
  for Entry in FServices do
  begin
    if Entry.Name = NameOrAlias then
    begin
      Result += [Entry];
      Continue;
    end;
    for i:=0 to Length(Entry.Aliases) do
      if Entry.Aliases[i] = NameOrAlias then
      begin
        Result += [Entry];
        Break;
      end;
  end;
end;

function TServiceDB.TryFindService(APort: Word): TServiceArray;
var
  Entry: TServiceEntry;
begin
  Result := Default(TServiceArray);
  for Entry in FServices do
    if Entry.Port = APort then
      Result += [Entry];
end;

function TServiceDB.GetService(const NameOrAlias: String): TServiceArray;
begin
  Result := TryFindServices(NameOrAlias);
  if Length(Result) = 0 then
    raise ENoSuchEntry.Create('No service named: ' + NameOrAlias);
end;

function TServiceDB.GetServiceByPort(APort: Word): TServiceArray;
begin
  Result := TryFindService(APort);
  if Length(Result) = 0 then
    raise ENoSuchEntry.Create('No service with port: ' + APort.ToString);
end;

{ THostsDB }

class constructor THostsDB.InitStatics;
begin
  Singleton := THostsDB.Create;
end;

class destructor THostsDB.CleanupStatics;
begin
  Singleton.Free;
end;

class function THostsDB.Instance: THostsDB;
begin
  Result := Singleton;
end;

procedure THostsDB.Update;
begin
  FHosts.Clear;
  inherited Update;
end;

procedure THostsDB.ParseEntry(const ALine: TTokenizedLine);
var
  Entry: THostEntry;
  i: Integer;
  PrevAliasLen: SizeInt;
begin
  // Malformed line
  if Length(ALine) < 2 then Exit;
  if not FHosts.TryGetValue(ALine[1], Entry) then
    Entry := Default(THostEntry);
  Entry.Addresses += [INAddr(ALine[0])];
  Entry.Name := ALine[1];
  PrevAliasLen := Length(Entry.Aliases);
  SetLength(Entry.Aliases, PrevAliasLen + Length(ALine) - 2);
  for i := 2 to Length(ALine) - 1 do
    Entry.Aliases[PrevAliasLen + i - 2] := ALine[i];
  FHosts.AddOrSetValue(Entry.Name, Entry);
end;

constructor THostsDB.Create;
begin
  inherited Create(ETCPath + SHostsFile);
  FHosts := THostMap.Create;
end;

destructor THostsDB.Destroy;
begin
  FHosts.Free;
  inherited Destroy;
end;

function THostsDB.TryFindHost(const NameOrAlias: String; out AEntry: THostEntry
  ): Boolean;
var
  i: SizeInt;
begin
  Result := False;
  if FHosts.TryGetValue(NameOrAlias, AEntry) then
    Exit(True);
  for AEntry in FHosts.Values do
    for i:=0 to Length(AEntry.Aliases) - 1 do
      if AEntry.Aliases[i] = NameOrAlias then
        Exit(True);
end;

function THostsDB.TryFindHost(const HostAddress: TNetworkAddress; out
  AEntry: THostEntry): Boolean;
var
  Addr: TNetworkAddress;
begin
  Result := False;
  for AEntry in FHosts.Values do
    for Addr in AEntry.Addresses do
      if Addr = HostAddress then
        Exit(True);
end;

function THostsDB.GetHost(const NameOrAlias: String): THostEntry;
begin
  if not TryFindHost(NameOrAlias, Result) then
    raise ENoSuchEntry.Create('No host named: ' + NameOrAlias);
end;

function THostsDB.GetHosts: THostMap.TValueCollection;
begin
  Result := FHosts.Values;
end;

function THostsDB.GetHostByAddress(const HostAddress: TNetworkAddress
  ): THostEntry;
begin
  if not TryFindHost(HostAddress, Result) then
    raise ENoSuchEntry.Create('No host with address: ' + HostAddress.Address);
end;

{ TNetworksDB }

class constructor TNetworksDB.InitStatics;
begin
  Singleton := TNetworksDB.Create;
end;

class destructor TNetworksDB.CleanupStatics;
begin
  Singleton.Free;
end;

class function TNetworksDB.Instance: TNetworksDB;
begin
  Result := Singleton;
end;

function TNetworksDB.CompleteNetworkAddress(const Addr: String): String;
var
  i, dots: SizeInt;
begin
  Result := Addr;
  dots := 0;
  for i:=1 to Addr.Length do
    if Addr[i] = ':' then Exit // ipv6 copout
    else if Addr[i] = '.' then
      Inc(dots);
  for i := dots to 2 do
    Result += '.0';
end;

procedure TNetworksDB.Update;
begin
  FNetworks.Clear;
  inherited Update;
end;

procedure TNetworksDB.ParseEntry(const ALine: TTokenizedLine);
var
  Entry: TNetworkEntry;
  i: Integer;
begin
  // Malformed line
  if Length(ALine) < 2 then Exit;
  Entry := Default(TNetworkEntry);
  Entry.Name := ALine[0];
  Entry.Addr := INAddr(CompleteNetworkAddress(ALine[1]));
  SetLength(Entry.Aliases, Length(ALine) - 2);
  for i := 2 to Length(ALine) - 1 do
    Entry.Aliases[i - 2] := ALine[i];
  FNetworks.Add(Entry.Name, Entry);
end;

constructor TNetworksDB.Create;
begin
  inherited Create(ETCPath + SNetworksFile);
  FNetworks := TNetworkMap.Create;
end;

destructor TNetworksDB.Destroy;
begin
  FNetworks.Free;
  inherited Destroy;
end;

function TNetworksDB.TryFindNetwork(const NameOrAlias: String; out
  AEntry: TNetworkEntry): Boolean;
var
  i: SizeInt;
begin
  Result := False;
  if FNetworks.TryGetValue(NameOrAlias, AEntry) then
    Exit(True);
  for AEntry in FNetworks.Values do
    for i:=0 to Length(AEntry.Aliases) - 1 do
      if AEntry.Aliases[i] = NameOrAlias then
        Exit(True);
end;

function TNetworksDB.TryFindNetwork(const NetworkAddress: TNetworkAddress; out
  AEntry: TNetworkEntry): Boolean;
begin
  Result := False;
  for AEntry in FNetworks.Values do
    if AEntry.Addr = NetworkAddress then
      Exit(True);
end;

function TNetworksDB.GetNetwork(const NameOrAlias: String): TNetworkEntry;
begin
  if not TryFindNetwork(NameOrAlias, Result) then
    raise ENoSuchEntry.Create('No netwrok named: ' + NameOrAlias);
end;

function TNetworksDB.GetNetworkByAddress(const NetworkAddress: TNetworkAddress
  ): TNetworkEntry;
begin
  if not TryFindNetwork(NetworkAddress, Result) then
    raise ENoSuchEntry.Create('No netwrok with address: ' + NetworkAddress.Address);
end;

function TNetworksDB.GetNetworks: TNetworkMap.TValueCollection;
begin
  Result := FNetworks.Values;
end;

{ TProtocolDB }

class constructor TProtocolDB.InitStatics;
begin
  Singleton := TProtocolDB.Create;
end;

class destructor TProtocolDB.CleanupStatics;
begin
  Singleton.Free;
end;

class function TProtocolDB.Instance: TProtocolDB;
begin
  Result := Singleton;
end;

procedure TProtocolDB.Update;
begin
  FProtocols.Clear;
  inherited Update;
end;

procedure TProtocolDB.ParseEntry(const ALine: TTokenizedLine);
var
  Entry: TProtocolEntry;
  i: Integer;
begin
  // Malformed line
  if Length(ALine) < 2 then Exit;
  Entry := Default(TProtocolEntry);
  Entry.Name := ALine[0];
  Entry.Number := ALine[1].ToInteger;
  SetLength(Entry.Aliases, Length(ALine) - 2);
  for i := 2 to Length(ALine) - 1 do
    Entry.Aliases[i - 2] := ALine[i];
  FProtocols.Add(Entry.Name, Entry);
end;

constructor TProtocolDB.Create;
begin
  inherited Create(ETCPath + SProtocolFile);
  FProtocols := TProtocolMap.Create;
end;

destructor TProtocolDB.Destroy;
begin
  FProtocols.Free;;
  inherited Destroy;
end;

function TProtocolDB.TryFindProtocol(const NameOrAlias: String; out
  AEntry: TProtocolEntry): Boolean;
var
  i: SizeInt;
begin
  Result := False;
  if FProtocols.TryGetValue(NameOrAlias, AEntry) then
    Exit(True);
  for AEntry in FProtocols.Values do
    for i:=0 to Length(AEntry.Aliases) - 1 do
      if AEntry.Aliases[i] = NameOrAlias then
        Exit(True);
end;

function TProtocolDB.TryFindProtocol(Number: Integer; out AEntry: TProtocolEntry
  ): Boolean;
begin
  Result := False;
  for AEntry in FProtocols.Values do
    if AEntry.Number = Number then
      Exit(True);
end;

function TProtocolDB.GetProtocol(const NameOrAlias: String): TProtocolEntry;
begin
  if not TryFindProtocol(NameOrAlias, Result) then
    raise ENoSuchEntry.Create('No protocol named: ' + NameOrAlias);
end;

function TProtocolDB.GetProtocolByNumber(Number: Integer): TProtocolEntry;
begin
  if not TryFindProtocol(Number, Result) then
    raise ENoSuchEntry.Create('No protocol numbered: ' + Number.ToString);
end;

function TProtocolDB.GetProtocols: TProtocolMap.TValueCollection;
begin
  Result := FProtocols.Values;
end;

end.
