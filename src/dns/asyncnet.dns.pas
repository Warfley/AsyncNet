unit asyncnet.dns;

{$mode objfpc}{$H+}
{$ModeSwitch arrayoperators}
{$ModeSwitch advancedrecords}

interface

uses
  SysUtils, Sockets, stax, AsyncNet.dns.resrecords, AsyncNet.sockets;

const
  DefaultDNSPort = 53;

type
  EMalformedDNSMessage = class(Exception);
  EUnsupportedRRType = class(Exception);
  EOutOfBounds = AsyncNet.dns.resrecords.EOutOfBounds;
  EDNSServerError = class(Exception);
  EHeaderFieldMismatch = class(Exception);

  TCNAMERecordArray = specialize TArray<TCNAMERecord>;
  THINFORecordArray = specialize TArray<THINFORecord>;
  TMBRecordArray = specialize TArray<TMBRecord>;
  TMDRecordArray = specialize TArray<TMDRecord>;
  TMFRecordArray = specialize TArray<TMFRecord>;
  TMGRecordArray = specialize TArray<TMGRecord>;
  TMINFORecordArray = specialize TArray<TMINFORecord>;
  TMRRecordArray = specialize TArray<TMRRecord>;
  TMXRecordArray = specialize TArray<TMXRecord>;
  TNULLRecordArray = specialize TArray<TNULLRecord>;
  TNSRecordArray = specialize TArray<TNSRecord>;
  TPTRRecordArray = specialize TArray<TPTRRecord>;
  TSOARecordArray = specialize TArray<TSOARecord>;
  TTXTRecordArray = specialize TArray<TTXTRecord>;
  TARecordArray = specialize TArray<TARecord>;
  TWKSRecordArray = specialize TArray<TWKSRecord>;
  TAAAARecordArray = specialize TArray<TAAAARecord>;

  // Maybe use TList for geometric growth?
  TRecordArray = record
    CNAMERecords: TCNAMERecordArray;
    HINFORecords: THINFORecordArray;
    MBRecords: TMBRecordArray;
    MDRecords: TMDRecordArray;
    MFRecords: TMFRecordArray;
    MGRecords: TMGRecordArray;
    MINFORecords: TMINFORecordArray;
    MRRecords: TMRRecordArray;
    MXRecords: TMXRecordArray;
    NULLRecords: TNULLRecordArray;
    NSRecords: TNSRecordArray;
    PTRRecords: TPTRRecordArray;
    SOARecords: TSOARecordArray;
    TXTRecords: TTXTRecordArray;
    ARecords: TARecordArray;
    WKSRecords: TWKSRecordArray;
    AAAARecords: TAAAARecordArray;
  end;

  { TDNSQuestion }
  // See: https://datatracker.ietf.org/doc/html/rfc1035#section-4.1.2
  TDNSQuestion = record
    QName: String;
    QType: TRRType;
    QClass: TRRClass;

    function MemSize: SizeInt; inline;
    function WriteToBuffer(ABuffer: Pointer; BufferEnd: Pointer): Pointer; inline;
  end; 

  TDNSQuestionArray = array of TDNSQuestion;

  TDNSResponse = record
    AuthorityAnswer: Boolean;
    Recursive: Boolean;

    Questions: TDNSQuestionArray;
    Answers: TRecordArray;
    Authorities: TNSRecordArray;
    Additional: TRecordArray;
  end;

  { ETruncatedResponse }

  // A truncated response can still contain valid information
  // therefore it will still contain the result of the operation
  // so the user can decide if that is enough or a TCP request is needed
  ETruncatedResponse = class(Exception)
  private
    FResponse: TDNSResponse;
  public
    constructor Create(constref AResponse: TDNSResponse);

    property Response: TDNSResponse read FResponse;
  end;

  // DNS header structure see: https://datatracker.ietf.org/doc/html/rfc1035#section-4.1.1
  // Intermediate format, will be written to/read from buffer with helper functions
  // This ensures endianness
  TDNSOpCode = (ocQuery =  0,
                ocIQuery = 1,
                ocStatus = 2);

  TDNSResponseCode = (rcNoError = 0,
                      rcFormatError = 1,
                      rcServerFailure = 2,
                      rcNameError = 3,
                      rcNotImplemented = 4,
                      rcRefused = 5);

  TDNSHeader = record
    ID: Word;
    IsResponse: Boolean;
    OpCode: TDNSOpCode;
    AuthorativeAnswer: Boolean;
    Truncated: Boolean;
    RecursionDesired: Boolean;
    RecursionAvailable: Boolean;
    RCode: TDNSResponseCode;
    QDCount: Word;
    ANCount: Word;
    NSCount: Word;
    ARCount: Word;
  end;

  { TDNSRequestTask }

  TDNSRequestTask = class(specialize TRVTask<TDNSResponse>)
  private
    class function GenerateID: Word; static; inline;
    class constructor InitStatics;
  private
    FDNSServerAddr: TNetworkAddress;
    FUseUDP: Boolean;
    FPort: Integer;
    FRecursive: Boolean;
    FQuestions: TDNSQuestionArray;
    FTimeout: Int64;

    function RequestSize: SizeInt; inline;
    function CreateRequestHeader: TDNSHeader;
    function WriteRequestToBuffer(ABuffer, BufferEnd: Pointer): Word;
    procedure ParseResponse(ABuffer, BufferEnd: Pointer; RequestID: Word);
    procedure TCPDNSRequest;
    procedure UDPDNSRequest;
  protected
    procedure Execute; override;
  public
    constructor Create(const ADNSServer: TNetworkAddress; constref Questions: TDNSQuestionArray;
                       ATimeout: Int64 = -1; ARecursive: Boolean = True;
                       APort: Integer = DefaultDNSPort; AUseUDP: Boolean = True); overload;
    constructor Create(const ADNSServer: TNetworkAddress; constref Questions: array of TDNSQuestion;
                       ATimeout: Int64 = -1; ARecursive: Boolean = True;
                       APort: Integer = DefaultDNSPort; AUseUDP: Boolean = True); overload;

  end;

function AsyncDNSRequest(const ADNSServer: TNetworkAddress;
                         constref Questions: array of TDNSQuestion;
                         ATimeout: Int64 = -1; ARecursive: Boolean = True;
                         APort: Integer = DefaultDNSPort; AUseUDP: Boolean = True
                         ): specialize TRVTask<TDNSResponse>; overload; inline;
function AsyncDNSRequest(const ADNSServer: TNetworkAddress;
                         constref Questions: TDNSQuestionArray;
                         ATimeout: Int64 = -1; ARecursive: Boolean = True;
                         APort: Integer = DefaultDNSPort; AUseUDP: Boolean = True
                         ): specialize TRVTask<TDNSResponse>; overload; inline;

function DNSQuestion(const AName: String; AType: TRRType = rrtA; AClass: TRRClass = rrcIN): TDNSQuestion;
implementation

const
  DNSHeaderSize = 12;
  MaxDNSMessageLength = 65535;

// These functions don't do bounds checking, make sure this happend earlier
procedure WriteHeaderToBuffer(constref AHeader: TDNSHeader; Buffer: Pointer);
begin
  // ID: 16
  PWord(Buffer)[0] := htons(AHeader.ID);
  // QR: 1 OPCODE: 4, AA:1, TC: 1, RD: 1
  PByte(Buffer)[2] := ((ord(AHeader.RecursionDesired) and 1) shl 0) or // RD: 1
                      ((ord(AHeader.Truncated) and 1) shl 1) or // TC: 1
                      ((ord(AHeader.AuthorativeAnswer) and 1) shl 2) or // AA: 1
                      ((ord(AHeader.OpCode) and $0F) shl 3) or // OPCODE: 4
                      ((ord(AHeader.IsResponse) and 1) shl 7); // QR: 1
  // RA: 1, Z: 3, RCode: 4 
  PByte(Buffer)[3] := ((ord(AHeader.RCode) and $0F) shl 0) or // RCode: 4
                      ((0 and $07) shl 4) or // Z: 3 (always 0)
                      ((ord(AHeader.RecursionAvailable) and 1) shl 7); // RA: 1
  // QDCOUNT: 16
  PWord(Buffer)[2] := htons(AHeader.QDCount);
  // ANCOUNT: 16
  PWord(Buffer)[3] := htons(AHeader.ANCount);
  // NSCOUNT: 16
  PWord(Buffer)[4] := htons(AHeader.NSCount);
  // ARCOUNT: 16
  PWord(Buffer)[5] := htons(AHeader.ARCount);
end;

function ReadHeaderFromBuffer(Buffer: Pointer): TDNSHeader;
begin
  Result := Default(TDNSHeader);
  // ID: 16
  Result.ID := NToHs(PWord(Buffer)[0]);
  // QR: 1 OPCODE: 4, AA:1, TC: 1, RD: 1
  Result.RecursionDesired := (PByte(Buffer)[2] shr 0) and 1 = 1;
  Result.Truncated := (PByte(Buffer)[2] shr 1) and 1 = 1;
  Result.AuthorativeAnswer := (PByte(Buffer)[2] shr 2) and 1 = 1;
  Result.OpCode := TDNSOpCode((PByte(Buffer)[2] shr 3) and $0F);
  Result.IsResponse := (PByte(Buffer)[2] shr 7) and 1 = 1;
  // RA: 1, Z: 3, RCode: 4
  Result.RCode := TDNSResponseCode((PByte(Buffer)[3] shr 0) and $0F);
  if (PByte(Buffer)[3] shr 4) and $07 <> 0 then
    raise EMalformedDNSMessage.Create('Z bits must be 0');
  Result.RecursionAvailable := (PByte(Buffer)[3] shr 7) and 1 = 1;
  // QDCOUNT: 16
  Result.QDCount := NToHs(PWord(Buffer)[2]);
  // ANCOUNT: 16
  Result.ANCount := NToHs(PWord(Buffer)[3]);
  // NSCOUNT: 16
  Result.NSCount := NToHs(PWord(Buffer)[4]);
  // ARCOUNT: 16
  Result.ARCount := NToHs(PWord(Buffer)[5]);
end;

function ParseQuestions(QStart: Pointer; NumQuestions: SizeInt;
                        MessageStart, MessageEnd: Pointer;
                        out QArray: TDNSQuestionArray): Pointer;
var
  i: SizeInt;
  QName: String;
  QType: TRRType;
  QClass: TRRClass;
begin
  QArray := Default(TDNSQuestionArray);
  SetLength(QArray, NumQuestions);
  for i:=0 to NumQuestions - 1 do
  begin
    QName := ReadDomainName(QStart, MessageStart, MessageEnd, @QStart);
    QType := TRRType(NToHs(PWord(QStart)[0]));
    QClass := TRRClass(NToHs(PWord(QStart)[1]));
    QArray[i] := DNSQuestion(QName, QType, QClass);
    Inc(QStart, 4);
  end;
  Result := QStart;
end;

function ParseRecords(RecStart: Pointer; NumRecords: SizeInt;
  MessageStart, MessageEnd: Pointer; IgnoreUnsupportedTypes: Boolean;
  out RecArray: TRecordArray): Pointer;
var
  i: SizeInt;
  RecInfo: TResourceRecordInfo;
begin
  RecArray := Default(TRecordArray);
  for i:=0 to NumRecords - 1 do
  begin
    if RecStart > MessageEnd then
      raise EMalformedDNSMessage.Create('Invalid number of resource records');
    // Parse record header
    RecInfo := ReadResourceRecord(RecStart, MessageStart, MessageEnd);
    // Parse record data
    // While having array operators is great for reading purposos, maybe
    // this will result in very slow code
    // so this might need to be changed
    case RecInfo.RRType of
      rrtA: RecArray.ARecords += [ParseARecord(RecInfo)];
      rrtNS: RecArray.NSRecords += [ParseNSRecord(RecInfo)];
      rrtMD: RecArray.MDRecords += [ParseMDRecord(RecInfo)];
      rrtMF: RecArray.MFRecords += [ParseMFRecord(RecInfo)];
      rrtCNAME: RecArray.CNAMERecords += [ParseCNAMERecord(RecInfo)];
      rrtSOA: RecArray.SOARecords += [ParseSOARecord(RecInfo)];
      rrtMB: RecArray.MBRecords += [ParseMBRecord(RecInfo)];
      rrtMG: RecArray.MGRecords += [ParseMGRecord(RecInfo)];
      rrtMR: RecArray.MRRecords += [ParseMRRecord(RecInfo)];
      rrtNULL: RecArray.NULLRecords += [ParseNULLRecord(RecInfo)];
      rrtWKS: RecArray.WKSRecords += [ParseWKSRecord(RecInfo)];
      rrtPTR: RecArray.PTRRecords += [ParsePTRRecord(RecInfo)];
      rrtHINFO: RecArray.HINFORecords += [ParseHINFORecord(RecInfo)];
      rrtMINFO: RecArray.MINFORecords += [ParseMINFORecord(RecInfo)];
      rrtMX: RecArray.MXRecords += [ParseMXRecord(RecInfo)];
      rrtTXT: RecArray.TXTRecords += [ParseTXTRecord(RecInfo)];
      rrtAAAA: RecArray.AAAARecords += [ParseAAAARecord(RecInfo)];
      otherwise if not IgnoreUnsupportedTypes then
         raise EUnsupportedRRType.Create('RRType ' + Ord(RecInfo.RRType).ToString + ' not implemented');
    end;
    // move pointer to one after the record
    RecStart := RecInfo.RREndPtr + 1;
  end;
  // Output pointer to first byte after block
  Result := RecStart;
end;

function ParseNSRecords(RecStart: Pointer; NumRecords: SizeInt;
  MessageStart, MessageEnd: Pointer; IgnoreUnsupportedTypes: Boolean;
  out RecArray: TNSRecordArray): Pointer;
var
  i, NSRecs: SizeInt;
  RecInfo: TResourceRecordInfo;
begin
  RecArray := Default(TNSRecordArray);
  SetLength(RecArray, NumRecords);
  NSRecs := 0;
  for i:=0 to NumRecords - 1 do
  begin
    if RecStart > MessageEnd then
      raise EMalformedDNSMessage.Create('Invalid number of resource records');
    // Parse header
    RecInfo := ReadResourceRecord(RecStart, MessageStart, MessageEnd);
    // Move pointer (we might jump with continue so do it now)
    RecStart := RecInfo.RREndPtr + 1;
    // sanity check: ensure type is NS
    if not IgnoreUnsupportedTypes then
    begin
      if(RecInfo.RRType <> rrtNS) then
        raise EUnsupportedRRType.Create('NS record expected but ' + ord(RecInfo.RRType).ToString + ' found');
      // skip parsing
      Continue;
    end;
    // Parse data
    RecArray[NSRecs] := ParseNSRecord(RecInfo);
    Inc(NSRecs);
  end;
  // Shrink to fit
  SetLength(RecArray, NSRecs);
  // Output pointer to first byte after block
  Result := RecStart;
end;

function AsyncDNSRequest(const ADNSServer: TNetworkAddress; constref
  Questions: array of TDNSQuestion; ATimeout: Int64; ARecursive: Boolean;
  APort: Integer; AUseUDP: Boolean): specialize TRVTask<TDNSResponse>;
begin
  Result := TDNSRequestTask.Create(ADNSServer, Questions, ATimeout, ARecursive,
                                   APort, AUseUDP);
end;

function AsyncDNSRequest(const ADNSServer: TNetworkAddress; constref
  Questions: TDNSQuestionArray; ATimeout: Int64; ARecursive: Boolean;
  APort: Integer; AUseUDP: Boolean): specialize TRVTask<TDNSResponse>;
begin
  Result := TDNSRequestTask.Create(ADNSServer, Questions, ATimeout, ARecursive,
                                   APort, AUseUDP);
end;

function DNSQuestion(const AName: String; AType: TRRType; AClass: TRRClass
  ): TDNSQuestion;
begin
  Result := Default(TDNSQuestion);
  Result.QName := AName;
  Result.QType := AType;
  Result.QClass := AClass;
end;

{ ETruncatedResponse }

constructor ETruncatedResponse.Create(constref AResponse: TDNSResponse);
begin
  inherited Create('DNS response truncated as it did not fit a UDP package');
  FResponse := AResponse;
end;

{ TDNSQuestion }

function TDNSQuestion.MemSize: SizeInt;
begin
  Result := (QName.Length + 2) // All '.' replaced with sizes, and additional ending 0 and start size
            + 2 // Type
            + 2; // Class
end;

function TDNSQuestion.WriteToBuffer(ABuffer: Pointer; BufferEnd: Pointer): Pointer;
begin
  if ABuffer + MemSize - 1 > BufferEnd then raise
    EOutOfBounds.Create('Question does not fit buffer');
  ABuffer := WriteDomainName(QName, ABuffer, BufferEnd);
  PWord(ABuffer)[0] := htons(Ord(QType));
  PWord(ABuffer)[1] := htons(Ord(QClass));
  Result := ABuffer + 4;
end;

{ TDNSRequestTask }

class function TDNSRequestTask.GenerateID: Word;
begin
  Result := Random(Word.MaxValue + 1);
end;

class constructor TDNSRequestTask.InitStatics;
begin
  Randomize;
end;

function TDNSRequestTask.RequestSize: SizeInt;
var
  i: Integer;
begin
  Result := DNSHeaderSize;
  for i:=0 to Length(FQuestions) - 1 do
    Result += FQuestions[i].MemSize;
end;

function TDNSRequestTask.CreateRequestHeader: TDNSHeader;
begin
  Result := Default(TDNSHeader);
  Result.ID := GenerateID;
  Result.IsResponse := False;
  Result.OpCode := ocQuery;
  Result.RecursionDesired := FRecursive;
  Result.QDCount := Length(FQuestions);
end;

function TDNSRequestTask.WriteRequestToBuffer(ABuffer, BufferEnd: Pointer
  ): Word;
var
  i: Integer;
  ReqHeader: TDNSHeader;
begin
  ReqHeader := CreateRequestHeader;
  WriteHeaderToBuffer(ReqHeader, ABuffer);
  // Move pointer over header
  Inc(ABuffer, DNSHeaderSize);
  // Write each request
  for i:=0 to Length(FQuestions) - 1 do
    ABuffer := FQuestions[i].WriteToBuffer(ABuffer, BufferEnd);
  Result := ReqHeader.ID;
end;

procedure TDNSRequestTask.ParseResponse(ABuffer, BufferEnd: Pointer;
  RequestID: Word);
var
  Header: TDNSHeader;
  RecordDataPtr: Pointer;
begin
  if BufferEnd - ABuffer + 1 < DNSHeaderSize then
    raise EMalformedDNSMessage.Create('DNS response too small');
  Header := ReadHeaderFromBuffer(ABuffer);
  // Sanity check, check if all header fields correspond to request
  if Header.ID <> RequestID then
    raise EHeaderFieldMismatch.Create('Response ID does not match request');
  if not Header.IsResponse then
    raise EMalformedDNSMessage.Create('Message expected to be a response');
  if Header.OpCode <> ocQuery then
    raise EHeaderFieldMismatch.Create('Response OpCode does not match request');
  if Header.RecursionDesired <> FRecursive then
    raise EHeaderFieldMismatch.Create('Response RD does not match request');

  // Fill in header information
  FResult.AuthorityAnswer := Header.AuthorativeAnswer;
  FResult.Recursive := FRecursive and Header.RecursionDesired;

  // Error handling
  case Header.RCode of
    rcFormatError: raise EDNSServerError.Create('Request malformed');
    rcServerFailure: raise EDNSServerError.Create('Internal server failure');
    rcNameError: exit; // Name not found
    rcNotImplemented:  raise EDNSServerError.Create('Query not supported');
    rcRefused: raise EDNSServerError.Create('Service refused');
  end;

  // Load records
  RecordDataPtr := ParseQuestions(ABuffer + DNSHeaderSize, Header.QDCount, ABuffer, BufferEnd, FResult.Questions);
  RecordDataPtr := ParseRecords(RecordDataPtr, Header.ANCount, ABuffer, BufferEnd, True, FResult.Answers);
  RecordDataPtr := ParseNSRecords(RecordDataPtr, Header.NSCount, ABuffer, BufferEnd, True, FResult.Authorities);
  RecordDataPtr := ParseRecords(RecordDataPtr, Header.ARCount, ABuffer, BufferEnd, True, FResult.Additional);
  // Finally, check if truncated
  if Header.Truncated then
    raise ETruncatedResponse.Create(FResult);
end;

procedure TDNSRequestTask.TCPDNSRequest;
var
  Buffer: Pointer;
  RequestLength: SizeInt;
  Socket: TSocket;
  RequestID: Word;
  ResponseLength: SizeInt;
begin
  RequestLength := RequestSize;
  if RequestLength > MaxDNSMessageLength then
    raise EOutOfBounds.Create('Request exceeds maximum message size');
  Buffer := GetMem(RequestLength);
  if not Assigned(Buffer) then
    raise EOutOfMemory.Create('Can''t allocate ' + RequestLength.ToString + ' bytes');
  try
    RequestID := WriteRequestToBuffer(Buffer, Buffer + RequestLength - 1);
    // Transmit request
    Socket := TCPSocket(FDNSServerAddr.AddressType);
    try
      // establish TCP connection
      Await(AsyncConnect(Socket, FDNSServerAddr, FPort));
      // Send request: first the size
      Await(specialize AsyncSend<Word>(Socket, htons(RequestSize)));
      // Then the request itself
      Await(AsyncSend(Socket, @Buffer, RequestSize));
      // close writing/send RST
      fpshutdown(Socket, SHUT_WR);
      // Fetch response length: this is where the timeout happens
      ResponseLength := NToHs(specialize Await<Word>(specialize AsyncReceive<Word>(Socket), FTimeout));
      // Get new sized buffer
      FreeMemAndNil(Buffer);
      Buffer := GetMem(ResponseLength);
      if not Assigned(Buffer) then
        raise EOutOfMemory.Create('Can''t allocate ' + ResponseLength.ToString + ' bytes');
      // await full result: no timeout (do we want that?)
      Await(AsyncReceive(Socket, Buffer, ResponseLength));
      // Parse result
      ParseResponse(Buffer, Buffer + ResponseLength - 1, RequestID);
    finally
      CloseSocket(Socket);
    end;
  finally
    if Assigned(Buffer) then
      Freemem(Buffer);
  end;
end;

procedure TDNSRequestTask.UDPDNSRequest;
var
  Buffer: array[0..MaxUDPPackageSize - 1] of Byte;
  RequestLength: SizeInt;
  Socket: TSocket;
  RequestID: Word;
  Response: TUDPReceiveFromResult;
begin
  RequestLength := RequestSize;
  if RequestLength > MaxUDPPackageSize then
    raise EOutOfBounds.Create('Request does not fit UDP package, use TCP instead');
  RequestID := WriteRequestToBuffer(@Buffer, @Buffer[High(Buffer)]);
  // Transmit request
  Socket := UDPSocket(atIN4);
  try
    Await(AsyncSendTo(Socket, FDNSServerAddr, FPort, @Buffer, RequestLength));
    //Fetch response
    // reuse buffer: clean header
    FillChar(Buffer, 0, DNSHeaderSize);
    Response := specialize Await<TUDPReceiveFromResult>(AsyncReceiveFrom(Socket, @Buffer, Length(Buffer)), FTimeout);
    // Parse result
    ParseResponse(@Buffer, @Buffer[Response.Size - 1], RequestID);
  finally
    CloseSocket(Socket);
  end;
end;

procedure TDNSRequestTask.Execute;
begin
  FResult := Default(TDNSResponse);
  if FUseUDP then
    UDPDNSRequest
  else
    TCPDNSRequest;
end;

constructor TDNSRequestTask.Create(const ADNSServer: TNetworkAddress; constref
  Questions: TDNSQuestionArray; ATimeout: Int64; ARecursive: Boolean;
  APort: Integer; AUseUDP: Boolean);
begin
  inherited Create;
  FDNSServerAddr := ADNSServer;
  FQuestions := Questions;
  FTimeout := ATimeout;
  FRecursive := ARecursive;
  FPort := APort;
  FUseUDP := AUseUDP;
end;

constructor TDNSRequestTask.Create(const ADNSServer: TNetworkAddress; constref
  Questions: array of TDNSQuestion; ATimeout: Int64; ARecursive: Boolean;
  APort: Integer; AUseUDP: Boolean);
var
  TmpQuestions: TDNSQuestionArray;
  i: SizeInt;
begin
  TmpQuestions := Default(TDNSQuestionArray);
  SetLength(TmpQuestions, Length(Questions));
  for i:=0 to Length(Questions) - 1 do
    TmpQuestions[i] := Questions[i];
  Create(ADNSServer, TmpQuestions, ATimeout, ARecursive, APort, AUseUDP);
end;

end.

