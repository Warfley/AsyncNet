unit asyncnet.sockets;

// TODO: IPv6 support

{$mode objfpc}{$H+}

interface

uses
  SysUtils, stax, stax.asyncio, asyncnet.compatibility;

type
  TSocket = asyncnet.compatibility.TSocket;

  { ESocketError }

  ESocketError = class(Exception)
  private
    FCode: Integer;
  public
    constructor Create(ACode: Integer; const FunName: String);

    property Code: Integer read FCode;
  end;

  EConnectionClosedException = class(Exception);
  EUDPFragmentationException = class(Exception);

  TAcceptResult = record
    Connection: TSocket;
    PeerAddress: String;
    PeerPort: Integer;
  end;

  { TAcceptTask }

  TAcceptTask = class(specialize TRVTask<TAcceptResult>)
  private
    FServerSocket: TSocket;
  protected
    procedure Execute; override;
  public
    constructor Create(AServerSocket: Tsocket);
  end;

  { TConnectTask }

  TConnectTask = class(TTask)
  private
    FSocket: Tsocket;
    FHost: String;
    FPort: Integer;

  protected
    procedure Execute; override;
  public
    constructor Create(ASocket: Tsocket; const AHost: String; APort: Integer);
  end;

  generic TUDPReceiveResult<T> = record
    Data: T;
    Address: String;
    Port: Integer;
  end;

  { TUDPReceiveTask }

  generic TUDPReceiveTask<T> = class(specialize TRVTask<specialize TUDPReceiveResult<T>>)
  private
    FSocket: TSocket;
  protected
    procedure Execute; override;
  public
    constructor Create(ASocket: TSocket);
  end;

  { TUDPStringReceiveTask }

  TUDPStringReceiveTask = class(specialize TRVTask<specialize TUDPReceiveResult<String>>)
  private
    FSocket: TSocket;
    FMaxLength: SizeInt;
  protected
    procedure Execute; override;
  public
    constructor Create(ASocket: TSocket; AMaxLength: SizeInt);
  end;

  { TUDPArrayReceiveTask }

  generic TUDPArrayReceiveTask<T> = class(specialize TRVTask<specialize TUDPReceiveResult<specialize TArray<T>>>)
  private
    FSocket: TSocket;
    FMaxCount: SizeInt;
  protected
    procedure Execute; override;
  public
    constructor Create(ASocket: TSocket; AMaxCount: SizeInt);
  end;

  TUDPReceiveFromResult = record
    Size: SizeInt;
    Address: String;
    Port: Integer;
  end;

  { TUDPBufferReceiveTask }

  TUDPBufferReceiveTask = class(specialize TRVTask<TUDPReceiveFromResult>)
  private
    FSocket: TSocket;
    FBuffer: Pointer;
    FBufferLen: SizeInt;
  protected
    procedure Execute; override;
  public
    constructor Create(ASocket: TSocket; ABuffer: Pointer; ABufferLen: SizeInt);
  end;

  { TUDPSendTask }

  generic TUDPSendTask<T> = class(specialize TRVTask<SizeInt>)
  private
    FSocket: TSocket;
    FHost: String;
    FPort: Integer;
    FData: T;
  protected
    procedure Execute; override;
  public
    constructor Create(ASocket: TSocket; const AHost: String; APort: Integer; const AData: T);
  end;

  { TUDPStringSendTask }

  TUDPStringSendTask = class(specialize TRVTask<SizeInt>)
  private
    FSocket: TSocket;
    FHost: String;
    FPort: Integer;
    FData: String;
  protected
    procedure Execute; override;
  public
    constructor Create(ASocket: TSocket; const AHost: String; APort: Integer; const AData: String);
  end;

  { TUDPArraySendTask }

  generic TUDPArraySendTask<T> = class(specialize TRVTask<SizeInt>)
  public type
    TArrayType = array of T;
  private
    FSocket: TSocket;
    FHost: String;
    FPort: Integer;
    FData: TArrayType;
  protected
    procedure Execute; override;
  public
    constructor Create(ASocket: TSocket; const AHost: String; APort: Integer; const AData: TArrayType);
  end;

  { TUDPBufferSendTask }

  TUDPBufferSendTask = class(specialize TRVTask<SizeInt>)
  private
    FSocket: TSocket;
    FHost: String;
    FPort: Integer;
    FBuffer: Pointer;
    FCount: SizeInt;
  protected
    procedure Execute; override;
  public
    constructor Create(ASocket: TSocket; const AHost: String; APort: Integer; ABuffer: Pointer; ACount: SizeInt);
  end;

  { TNonBlockingTCPReceiver }

  TNonBlockingTCPReceiver = class(TIOReader)
  private
    FSocket: TSocket;
  public
    function TryRead(ABuffer: Pointer; ACount: SizeInt): SizeInt; override;
    constructor Create(ASocket: TSocket);
  end;

  { TNonBlockingTCPSender }

  TNonBlockingTCPSender = class(TIOWriter)
  private
    FSocket: TSocket;
  public
    function TryWrite(ABuffer: Pointer; ACount: SizeInt): SizeInt; override;
    constructor Create(ASocket: TSocket);
  end;

const
  SocketSleepingTime = 10;
  MaxUDPPackageSize = 512;

// Connection establishment
function AsyncAccept(AServerSocket: TSocket): specialize TRVTask<TAcceptResult>; inline;
function AsyncConnect(ASocket: TSocket; const AHost: string; APort: Integer): TTask; inline;

// TCP receiving
function AsyncReceive(ASocket: Tsocket; ABuffer: Pointer; ACount: SizeInt; AwaitFullData: Boolean = True): specialize TRVTask<SizeInt>; overload; inline;
generic function AsyncReceive<T>(ASocket: Tsocket): specialize TRVTask<T>; overload; inline;
generic function AsyncReceive<T>(ASocket: Tsocket; ACount: SizeInt): specialize TRVTask<specialize TArray<T>>; overload; inline;
function AsyncReceiveLn(ASocket: Tsocket): specialize TRVTask<String>; inline;
function AsyncReceiveStr(ASocket: Tsocket; ALength: SizeInt; AwaitFullData: Boolean = True): specialize TRVTask<String>; inline;

// UDP receiving
function AsyncReceiveFrom(ASocket: TSocket; ABuffer: Pointer; ABufferSize: SizeInt): specialize TRVTask<TUDPReceiveFromResult>; inline; overload;
generic function AsyncReceiveFrom<T>(ASocket: TSocket): specialize TRVTask<specialize TUDPReceiveResult<T>>; inline;
// 512 is generally considered the UDP size limit
generic function AsyncReceiveFromArray<T>(ASocket: TSocket; AMaxCount: SizeInt = MaxUDPPackageSize): specialize TRVTask<specialize TUDPReceiveResult<specialize TArray<T>>>; inline;
function AsyncReceiveFromStr(ASocket: TSocket; AMaxLength: SizeInt = MaxUDPPackageSize): specialize TRVTask<specialize TUDPReceiveResult<String>>; inline;

// TCP sending
function AsyncSend(ASocket: Tsocket; ABuffer: Pointer; ACount: SizeInt): TTask; overload; inline;
generic function AsyncSend<T>(ASocket: Tsocket; const AData: T): TTask; overload; inline;
generic function AsyncSendArray<T>(ASocket: TSocket; const AData: specialize TArray<T>): TTask; inline;
function AsyncSendLn(ASocket: Tsocket; const AData: String): TTask; inline;
function AsyncSendStr(ASocket: Tsocket; const AData: String): TTask; inline;

// UDP sending
function AsyncSendTo(ASocket: Tsocket; const AHost: String; APort: Integer; ABuffer: Pointer; ACount: SizeInt): specialize TRVTask<SizeInt>; overload; inline;
generic function AsyncSendTo<T>(ASocket: Tsocket; const AHost: String; APort: Integer; const AData: T):  specialize TRVTask<SizeInt>; overload; inline;
generic function AsyncSendToArray<T>(ASocket: TSocket; const AHost: String; APort: Integer; const AData: specialize TArray<T>):  specialize TRVTask<SizeInt>; inline;
function AsyncSendToStr(ASocket: Tsocket; const AHost: String; APort: Integer; const AData: String):  specialize TRVTask<SizeInt>; inline;

// Socket API helper
function TCPSocket: TSocket; inline;
function UDPSocket: TSocket;
function TCPServerSocket(const AHost: String; APort: Integer): TSocket;
procedure TCPServerListen(AServerSocket: TSocket; Backlog: Integer); inline;

// Internal helper
// Because generics can't reference static functions we publish them here
// please don't use them directly
procedure FillAddr(const AHost: String; APort: Integer; Addr: Pointer); inline;
function WaitingRecvFrom(AExecution: TExecutable; ASocket: TSocket; Buffer: Pointer;
                         BuffLen: SizeInt; Flags: Integer; Address: Pointer;
                         AddressLen: Pointer): SizeInt;
function WaitingSendTo(AExecution: TExecutable; ASocket: TSocket; Buffer: Pointer;
                       BuffLen: SizeInt; Flags: Integer; Address: Pointer;
                       AddressLen: SizeInt): SizeInt;

implementation
uses
  Sockets;

{$Region Async Functions}

{$Region Connections}
function AsyncAccept(AServerSocket: TSocket): specialize TRVTask<TAcceptResult>;
begin
  Result := TAcceptTask.Create(AServerSocket);
end;

function AsyncConnect(ASocket: TSocket; const AHost: string;
  APort: Integer): TTask;
begin
  Result := TConnectTask.Create(ASocket, AHost, APort);
end;
{$EndRegion Connections}

{$Region TCP Receive}
function AsyncReceive(ASocket: Tsocket; ABuffer: Pointer; ACount: SizeInt;
  AwaitFullData: Boolean): specialize TRVTask<SizeInt>;
begin
  Result := TIOBufferReadTask.Create(TNonBlockingTCPReceiver.Create(ASocket), ABuffer, ACount, AwaitFullData);
end;

generic function AsyncReceive<T>(ASocket: Tsocket): specialize TRVTask<T>;
begin
  Result := specialize TIOReadTask<T>.Create(TNonBlockingTCPReceiver.Create(ASocket));
end;

generic function AsyncReceive<T>(ASocket: Tsocket; ACount: SizeInt): specialize TRVTask<specialize TArray<T>>;
begin
  Result := specialize TIOArrayReadTask<T>.Create(TNonBlockingTCPReceiver.Create(ASocket), ACount);
end;

function AsyncReceiveLn(ASocket: Tsocket): specialize TRVTask<String>;
begin
  Result := TIOStringReadTask.Create(TNonBlockingTCPReceiver.Create(ASocket), #10);
end;

function AsyncReceiveStr(ASocket: Tsocket; ALength: SizeInt;
  AwaitFullData: Boolean): specialize TRVTask<String>;
begin
  Result := TIOStringReadTask.Create(TNonBlockingTCPReceiver.Create(ASocket), ALength, AwaitFullData);
end;
{$EndRegion}

{$Region UDP Receive}
function AsyncReceiveFrom(ASocket: TSocket; ABuffer: Pointer;
  ABufferSize: SizeInt): specialize TRVTask<TUDPReceiveFromResult>;
begin
  Result := TUDPBufferReceiveTask.Create(ASocket, ABuffer, ABufferSize);
end;

generic function AsyncReceiveFrom<T>(ASocket: TSocket): specialize TRVTask<specialize TUDPReceiveResult<T>>;
begin
  Result := specialize TUDPReceiveTask<T>.Create(ASocket);
end;

generic function AsyncReceiveFromArray<T>(ASocket: TSocket; AMaxCount: SizeInt): specialize TRVTask<specialize TUDPReceiveResult<specialize TArray<T>>>;
begin
  Result := specialize TUDPArrayReceiveTask<T>.Create(ASocket, AMaxCount);
end;

function AsyncReceiveFromStr(ASocket: TSocket; AMaxLength: SizeInt): specialize
  TRVTask<specialize TUDPReceiveResult<String>>;
begin
  Result := TUDPStringReceiveTask.Create(ASocket, AMaxLength);
end;

function AsyncSend(ASocket: Tsocket; ABuffer: Pointer; ACount: SizeInt
  ): TTask;
begin
  Result := TIOBufferWriteTask.Create(TNonBlockingTCPSender.Create(ASocket), ABuffer, ACount, True);
end;
{$EndRegion}

{$Region TCP Send}
generic function AsyncSend<T>(ASocket: Tsocket; const AData: T): TTask; overload; inline;
begin
  Result := specialize TIOWriteTask<T>.Create(TNonBlockingTCPSender.Create(ASocket), AData);
end;

generic function AsyncSendArray<T>(ASocket: TSocket; const AData: specialize TArray<T>): TTask;
begin
  Result := specialize TIOArrayWriteTask<T>.Create(TNonBlockingTCPSender.Create(ASocket), AData, True);
end;

function AsyncSendLn(ASocket: Tsocket; const AData: String): TTask;
begin
  Result := TIOStringWriteTask.Create(TNonBlockingTCPSender.Create(ASocket), AData.Trim + #10, True);
end;

function AsyncSendStr(ASocket: Tsocket; const AData: String): TTask;
begin
  Result := TIOStringWriteTask.Create(TNonBlockingTCPSender.Create(ASocket), AData, True);
end;

{$EndRegion TCP Send}

{$Region UDP Send}
function AsyncSendTo(ASocket: Tsocket; const AHost: String; APort: Integer;
  ABuffer: Pointer; ACount: SizeInt): specialize TRVTask<SizeInt>;
begin
  Result := TUDPBufferSendTask.Create(ASocket, AHost, APort, ABuffer, ACount);
end;

generic function AsyncSendTo<T>(ASocket: Tsocket; const AHost: String; APort: Integer; const AData: T):  specialize TRVTask<SizeInt>;
begin
  Result := specialize TUDPSendTask<T>.Create(ASocket, AHost, Aport, AData);
end;

generic function AsyncSendToArray<T>(ASocket: TSocket; const AHost: String; APort: Integer; const AData: specialize TArray<T>):  specialize TRVTask<SizeInt>;
begin
  Result := specialize TUDPArraySendTask<T>.Create(ASocket, AHost, Aport, AData);
end;

function AsyncSendToStr(ASocket: Tsocket; const AHost: String; APort: Integer;
  const AData: String): specialize TRVTask<SizeInt>;
begin
  Result := TUDPStringSendTask.Create(ASocket, AHost, Aport, AData);
end;
{$EndRegion UDP Send}
{$EndRegion Async Functions}

{$Region Socket Helper}

function TCPSocket: TSocket;
begin
  Result := fpsocket(AF_INET, SOCK_STREAM, 0);
  if SocketInvalid(Result) then
    raise ESocketError.Create(socketerror, 'socket');
end;

function UDPSocket: TSocket;
begin
  Result := fpsocket(AF_INET, SOCK_DGRAM, 0);
  if SocketInvalid(Result) then
    raise ESocketError.Create(socketerror, 'socket');
end;

function TCPServerSocket(const AHost: String; APort: Integer): TSocket;
var
  addr: sockaddr_in;
begin
  Result := TCPSocket;
  FillAddr(AHost, APort, @addr);
  if fpbind(Result, @addr, SizeOf(addr)) <> 0 then raise
    ESocketError.Create(socketerror, 'bind (%s:%d)'.Format([AHost, APort]));
end;

procedure TCPServerListen(AServerSocket: TSocket; Backlog: Integer);
begin
  if fplisten(AServerSocket, Backlog) <> 0 then raise
    ESocketError.Create(socketerror, 'listen');
end;

{$EndRegion Socket Helper}

{$Region Send/Receive Helper}

procedure FillAddr(const AHost: String; APort: Integer; Addr: Pointer);
begin
  psockaddr_in(Addr)^.sin_family := AF_INET;
  psockaddr_in(Addr)^.sin_port := ShortHostToNet(APort);
  psockaddr_in(Addr)^.sin_addr.s_addr := LongWord(StrToNetAddr(AHost));
end;

procedure WaitForHandshake(ATask: TTask; ASocket: Tsocket); inline;
var
  success: LongInt;
  fs: TFDSet;
  timeout: TTimeVal;
begin
  fs := Default(TFDSet);
  FD_ZERO(fs);
  FD_SET(ASocket, fs);
  timeout.tv_sec:=0;
  timeout.tv_usec:=0;
  repeat
    success := select(ASocket + 1, Nil, @fs, nil, @timeout);
    if success = 0 then
      ATask.Sleep(SocketSleepingTime)
    else if success < 0 then
      raise ESocketError.Create(socketerror, 'select');
  until success = 1;
end;

function WaitingRecvFrom(AExecution: TExecutable; ASocket: TSocket;
  Buffer: Pointer; BuffLen: SizeInt; Flags: Integer; Address: Pointer;
  AddressLen: Pointer): SizeInt;
var
  OldState, err: LongInt;
begin
  OldState := SetNonBlocking(ASocket);
  try
    repeat
      Result := fprecvfrom(ASocket, Buffer, BuffLen, Flags, Address, AddressLen);
      if Result < 0 then
      begin
        err := socketerror;
        if WasBlockingError(err) then
          AExecution.Sleep(SocketSleepingTime)
        else
          raise ESocketError.Create(err, 'recvfrom');
      end;
    until Result >= 0;
  finally
    RestoreBlocking(ASocket, OldState);
  end;
end;

function WaitingSendTo(AExecution: TExecutable; ASocket: TSocket;
  Buffer: Pointer; BuffLen: SizeInt; Flags: Integer; Address: Pointer;
  AddressLen: SizeInt): SizeInt;
var
  OldState, err: LongInt;
begin
  OldState := SetNonBlocking(ASocket);
  try
    repeat
      Result := fpsendto(ASocket, Buffer, BuffLen, Flags, Address, AddressLen);
      if Result < 0 then
      begin
        err := socketerror;
        if WasBlockingError(err) then
          AExecution.Sleep(SocketSleepingTime)
        else
          raise ESocketError.Create(err, 'sendto');
      end;
    until Result >= 0;
  finally
    RestoreBlocking(ASocket, OldState);
  end;
end;
{$EndRegion Helper}

{$Region Tasks}
{ TUDPSendTask }

procedure TUDPSendTask.Execute;
var
  addr: sockaddr_in;
begin
  FillAddr(FHost, FPort, @addr);
  FResult := WaitingSendTo(Self, FSocket, @FData, SizeOf(T), 0, @addr, SizeOf(addr));
end;

constructor TUDPSendTask.Create(ASocket: TSocket; const AHost: String;
  APort: Integer; const AData: T);
begin
  inherited Create;
  FSocket := ASocket;
  FHost := AHost;
  FPort := APort;
  FData := AData;
end;

{ TUDPStringSendTask }

procedure TUDPStringSendTask.Execute;
var
  addr: sockaddr_in;
begin
  FillAddr(FHost, FPort, @addr);
  FResult := WaitingSendTo(Self, FSocket, PChar(FData), Length(FData), 0, @addr, SizeOf(addr));
end;

constructor TUDPStringSendTask.Create(ASocket: TSocket; const AHost: String;
  APort: Integer; const AData: String);
begin
  inherited Create;
  FSocket := ASocket;
  FHost := AHost;
  FPort := APort;
  FData := AData;
end;

{ TUDPArraySendTask }

procedure TUDPArraySendTask.Execute;
var
  addr: sockaddr_in;
begin
  FillAddr(FHost, FPort, @addr);
  FResult := WaitingSendTo(Self, FSocket, @FData[0], Length(FData) * SizeOf(T), 0, @addr, SizeOf(addr));
end;

constructor TUDPArraySendTask.Create(ASocket: TSocket; const AHost: String;
  APort: Integer; const AData: TArrayType);
begin
  Inherited Create;
  FSocket := ASocket;
  FHost := AHost;
  FPort := APort;
  FData := AData;
end;

{ TUDPBufferSendTask }

procedure TUDPBufferSendTask.Execute;
var
  addr: sockaddr_in;
begin
  FillAddr(FHost, FPort, @addr);
  FResult := WaitingSendTo(Self, FSocket, FBuffer, FCount, 0, @addr, SizeOf(addr));
end;

constructor TUDPBufferSendTask.Create(ASocket: TSocket; const AHost: String;
  APort: Integer; ABuffer: Pointer; ACount: SizeInt);
begin
  inherited Create;
  FSocket := ASocket;
  FHost := AHost;
  FPort := APort;
  FBuffer := ABuffer;
  FCount := ACount;
end;

{ TUDPReceiveTask }

procedure TUDPReceiveTask.Execute;
var
  {$IfNDef WINDOWS}
  _addr: sockaddr;
  _addrLen: SizeInt;
  {$EndIf}
  addr: psockaddr_in;
  addrLen: PSizeInt;
  dataLen: SizeInt;
begin
  {$IfDef WINDOWS}
  // WinSock doesn't like the addr located on the stack, therefore we create a heap instance for it
  New(addr);
  New(addrLen);
  try
  {$Else}
  addr := @_addr;
  addrLen := @_addrLen;
  {$EndIf}
  dataLen := WaitingRecvFrom(Self, FSocket, @FResult.Data, SizeOf(T), 0, addr, addrLen);
  FResult.Port := ShortNetToHost(addr^.sin_port);
  FResult.Address := NetAddrToStr(addr^.sin_addr);
  if dataLen < SizeOf(T) then
    raise EUDPFragmentationException.Create('Receiving of fragmented data is not supported by typed receive');
  {$IfDef WINDOWS}
  finally
    Dispose(addr);
    Dispose(addrLen);
  end;
  {$EndIf}
end;

constructor TUDPReceiveTask.Create(ASocket: TSocket);
begin
  inherited Create;
  FSocket := ASocket;
end;

{ TUDPStringReceiveTask }

procedure TUDPStringReceiveTask.Execute;
var
  {$IfNDef WINDOWS}
  _addr: sockaddr;
  _addrLen: SizeInt;
  {$EndIf}
  addr: psockaddr_in;
  addrLen: PSizeInt;
  dataLen: SizeInt;
begin
  {$IfDef WINDOWS}
  // WinSock doesn't like the addr located on the stack, therefore we create a heap instance for it
  New(addr);
  New(addrLen);
  try
  {$Else}
  addr := @_addr; 
  addrLen := @_addrLen;
  {$EndIf}
  SetLength(FResult.Data, FMaxLength);
  dataLen := WaitingRecvFrom(Self, FSocket, PChar(FResult.Data), FMaxLength, 0, addr, addrLen);
  SetLength(FResult.Data, dataLen);
  FResult.Port := ShortNetToHost(addr^.sin_port);
  FResult.Address := NetAddrToStr(addr^.sin_addr);
  {$IfDef WINDOWS}
  finally
    Dispose(addr);
    Dispose(addrLen);
  end;
  {$EndIf}
end;

constructor TUDPStringReceiveTask.Create(ASocket: TSocket; AMaxLength: SizeInt);
begin
  inherited Create;
  FSocket := ASocket;
  FMaxLength := AMaxLength;
end;


{ TUDPArrayReceiveTask }

procedure TUDPArrayReceiveTask.Execute;
var
  {$IfNDef WINDOWS}
  _addr: sockaddr;
  _addrLen: SizeInt;
  {$EndIf}
  addr: psockaddr_in;
  addrLen: PSizeInt;
  dataLen: SizeInt;
begin
  {$IfDef WINDOWS}
  // WinSock doesn't like the addr located on the stack, therefore we create a heap instance for it
  New(addr);
  New(addrLen);
  try
  {$Else}
  addr := @_addr;
  addrLen := @_addrLen;
  {$EndIf}
  SetLength(FResult.Data, FMaxCount);
  dataLen := WaitingRecvFrom(Self, FSocket, @FResult.Data[0], FMaxCount * SizeOf(T), 0, addr, addrLen);
  SetLength(FResult.Data, dataLen div SizeOf(T));
  FResult.Port := ShortNetToHost(addr^.sin_port);
  FResult.Address := NetAddrToStr(addr^.sin_addr);
  if dataLen mod SizeOf(T) > 0 then
    raise EUDPFragmentationException.Create('Receiving of fragmented data is not supported by typed receive');
  {$IfDef WINDOWS}
  finally
    Dispose(addr);
    Dispose(addrLen);
  end;
  {$EndIf}
end;

constructor TUDPArrayReceiveTask.Create(ASocket: TSocket; AMaxCount: SizeInt);
begin
  inherited Create;
  FSocket := ASocket;
  FMaxCount := AMaxCount;
end;

{ TUDPBufferReceiveTask }

procedure TUDPBufferReceiveTask.Execute;
var
  {$IfNDef WINDOWS}
  _addr: sockaddr;
  _addrLen: SizeInt;
  {$EndIf}
  addr: psockaddr_in;
  addrLen: PSizeInt;
begin
  {$IfDef WINDOWS}
  // WinSock doesn't like the addr located on the stack, therefore we create a heap instance for it
  New(addr);
  New(addrLen);
  try
  {$Else}
  addr := @_addr;
  addrLen := @_addrLen;
  {$EndIf}
  FResult.Size := WaitingRecvFrom(Self, FSocket, FBuffer, FBufferLen, 0, addr, addrLen);
  FResult.Port := ShortNetToHost(addr^.sin_port);
  FResult.Address := NetAddrToStr(addr^.sin_addr);
  {$IfDef WINDOWS}
  finally
    Dispose(addr);
    Dispose(addrLen);
  end;
  {$EndIf}
end;

constructor TUDPBufferReceiveTask.Create(ASocket: TSocket; ABuffer: Pointer;
  ABufferLen: SizeInt);
begin
  inherited Create;
  FSocket := ASocket;
  FBuffer := ABuffer;
  FBufferLen := ABufferLen;
end;

{ ESocketError }

constructor ESocketError.Create(ACode: Integer; const FunName: String);
begin
  inherited CreateFmt('[Socket Error: %d] %s call failed',  [ACode, FunName]);
  FCode := ACode;
end;

{ TNonBlockingTCPSender }

function TNonBlockingTCPSender.TryWrite(ABuffer: Pointer; ACount: SizeInt
  ): SizeInt;
var
  OldState, err: LongInt;
begin
  OldState := SetNonBlocking(FSocket);
  try
    Result := fpsend(FSocket, ABuffer, ACount, 0);
    if Result < 0 then
    begin
      err := socketerror;
      if WasBlockingError(err) then
        Result := 0
      else
        raise ESocketError.Create(err, 'send');
    end;
  finally
    RestoreBlocking(FSocket, OldState);
  end;
end;

constructor TNonBlockingTCPSender.Create(ASocket: TSocket);
begin
  inherited Create(SocketSleepingTime);
  FSocket := ASocket;
end;

{ TNonBlockingTCPReceiver }

function TNonBlockingTCPReceiver.TryRead(ABuffer: Pointer; ACount: SizeInt
  ): SizeInt;
var
  OldState, err: LongInt;
begin
  OldState := SetNonBlocking(FSocket);
  try
    Result := fprecv(FSocket, ABuffer, ACount, 0);
    if Result = 0 then
      raise EConnectionClosedException.Create('The connection closed')
    else if Result < 0 then
    begin
      err := socketerror;
      if WasBlockingError(err) then
        Result := 0
      else
        raise ESocketError.Create(err, 'recv');
    end;
  finally
    RestoreBlocking(FSocket, OldState);
  end;
end;

constructor TNonBlockingTCPReceiver.Create(ASocket: TSocket);
begin
  inherited Create(SocketSleepingTime);
  FSocket := ASocket;
end;

{ TConnectTask }

procedure TConnectTask.Execute;
var
  OldState, err, success: LongInt;
  addr: TSockAddr;
begin
  OldState := SetNonBlocking(FSocket);
  try
    addr.sin_family := AF_INET;
    addr.sin_port := ShortHostToNet(FPort);
    addr.sin_addr.s_addr := LongWord(StrToNetAddr(FHost));
    success := fpconnect(FSocket, @addr, SizeOf(addr));
    if success <> 0 then
    begin
      err := socketerror;
      if WasBlockingError(err) then
        WaitForHandshake(self, FSocket)
      else
        raise ESocketError.Create(socketerror, 'connect');
    end;
  finally
    RestoreBlocking(FSocket, OldState);
  end;
end;

constructor TConnectTask.Create(ASocket: Tsocket; const AHost: String;
  APort: Integer);
begin
  inherited Create;
  FSocket := ASocket;
  FHost := AHost;
  FPort := APort;
end;

{ TAcceptTask }

procedure TAcceptTask.Execute;
var
  OldState, err: LongInt;
  Conn: TSocket;
  addr: sockaddr_in;
  addrLen: SizeInt;
begin
  addrLen := SizeOf(addr);
  OldState := SetNonBlocking(FServerSocket);
  try
    repeat
      Conn := fpaccept(FServerSocket, @addr, @addrLen);
      if SocketInvalid(Conn) then
      begin
        err := socketerror;
        if WasBlockingError(err) then
          Sleep(SocketSleepingTime)
        else
          raise ESocketError.Create(err, 'accept');
      end;
    until not SocketInvalid(Conn);
    WaitForHandshake(Self, Conn);
  finally
    RestoreBlocking(FServerSocket, OldState);
  end;
  FResult.Connection := Conn;
  FResult.PeerAddress := NetAddrToStr(addr.sin_addr);
  FResult.PeerPort := ShortNetToHost(addr.sin_port);
end;

constructor TAcceptTask.Create(AServerSocket: Tsocket);
begin
  inherited Create;
  FServerSocket := AServerSocket;
end;

{$EndRegion Tasks}

end.

