unit asyncnet.sockets;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, sockets, stax, stax.asyncio, asyncnet.compatibility;

type
  TSocket = asyncnet.compatibility.TSocket;

  TAddressType = (atIN4, atIN6);
  TNetworkAddress = record
    Address: String;
    AddressType: TAddressType;
  end;
  TNetworkAddressArray = array of TNetworkAddress;

  EUnsupportedAddress = class(Exception);

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
    PeerAddress: TNetworkAddress;
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
    FAddress: TNetworkAddress;
    FPort: Integer;

  protected
    procedure Execute; override;
  public
    constructor Create(ASocket: Tsocket; const AAddress: TNetworkAddress; APort: Integer);
  end;

  generic TUDPReceiveResult<T> = record
    Data: T;
    Address: TNetworkAddress;
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
    Address: TNetworkAddress;
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
    FAddress: TNetworkAddress;
    FPort: Integer;
    FData: T;
  protected
    procedure Execute; override;
  public
    constructor Create(ASocket: TSocket; const AAddress: TNetworkAddress; APort: Integer; const AData: T);
  end;

  { TUDPStringSendTask }

  TUDPStringSendTask = class(specialize TRVTask<SizeInt>)
  private
    FSocket: TSocket;
    FAddress: TNetworkAddress;
    FPort: Integer;
    FData: String;
  protected
    procedure Execute; override;
  public
    constructor Create(ASocket: TSocket; const AAddress: TNetworkAddress; APort: Integer; const AData: String);
  end;

  { TUDPArraySendTask }

  generic TUDPArraySendTask<T> = class(specialize TRVTask<SizeInt>)
  public type
    TArrayType = array of T;
  private
    FSocket: TSocket;
    FAddress: TNetworkAddress;
    FPort: Integer;
    FData: TArrayType;
  protected
    procedure Execute; override;
  public
    constructor Create(ASocket: TSocket; const AAddress: TNetworkAddress; APort: Integer; const AData: TArrayType);
  end;

  { TUDPBufferSendTask }

  TUDPBufferSendTask = class(specialize TRVTask<SizeInt>)
  private
    FSocket: TSocket;
    FAddress: TNetworkAddress;
    FPort: Integer;
    FBuffer: Pointer;
    FCount: SizeInt;
  protected
    procedure Execute; override;
  public
    constructor Create(ASocket: TSocket; const AAddress: TNetworkAddress; APort: Integer; ABuffer: Pointer; ACount: SizeInt);
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
function AsyncConnect(ASocket: TSocket; const AAddress: TNetworkAddress; APort: Integer): TTask; inline;

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
function AsyncSendTo(ASocket: Tsocket; const AAddress: TNetworkAddress; APort: Integer; ABuffer: Pointer; ACount: SizeInt): specialize TRVTask<SizeInt>; overload; inline;
generic function AsyncSendTo<T>(ASocket: Tsocket; const AAddress: TNetworkAddress; APort: Integer; const AData: T):  specialize TRVTask<SizeInt>; overload; inline;
generic function AsyncSendToArray<T>(ASocket: TSocket; const AAddress: TNetworkAddress; APort: Integer; const AData: specialize TArray<T>):  specialize TRVTask<SizeInt>; inline;
function AsyncSendToStr(ASocket: Tsocket; const AAddress: TNetworkAddress; APort: Integer; const AData: String):  specialize TRVTask<SizeInt>; inline;

// Socket API helper
function TCPSocket(AddressType: TAddressType): TSocket; inline;
function UDPSocket(AddressType: TAddressType): TSocket; inline;
function TCPServerSocket(const AAddress: TNetworkAddress; APort: Integer): TSocket;
procedure TCPServerListen(AServerSocket: TSocket; Backlog: Integer); inline;

// Address Management
function IN4Address(const Address: String): TNetworkAddress; inline;
function IN6Address(const Address: String): TNetworkAddress; inline;
function INAddr(const Address: String): TNetworkAddress; inline;

function IN6Equal(const A, B: String): Boolean;
operator =(const A, B: TNetworkAddress): Boolean; inline;
operator :=(const AStr: String): TNetworkAddress; inline;

// Internal helper
// Because generics can't reference static functions we publish them here
// please don't use them directly ;
function IsIPv4Mapped(const IPv6Addr: TIn6Addr): Boolean; inline;
function ExtractIPv4Address(const IPv6Addr: TIn6Addr): TNetworkAddress; inline;
procedure FillAddr(const AAddress: TNetworkAddress; APort: Integer; Addr: PAddressUnion); inline;
procedure ReadAddr(Addr: PAddressUnion; out AAddress: TNetworkAddress; out APort: Integer);
function WaitingRecvFrom(AExecution: TExecutable; ASocket: TSocket; Buffer: Pointer;
                         BuffLen: SizeInt; Flags: Integer; Address: Pointer;
                         AddressLen: Pointer): SizeInt;
function WaitingSendTo(AExecution: TExecutable; ASocket: TSocket; Buffer: Pointer;
                       BuffLen: SizeInt; Flags: Integer; Address: Pointer;
                       AddressLen: SizeInt): SizeInt;

implementation

{$Region Async Functions}

{$Region Connections}
function AsyncAccept(AServerSocket: TSocket): specialize TRVTask<TAcceptResult>;
begin
  Result := TAcceptTask.Create(AServerSocket);
end;

function AsyncConnect(ASocket: TSocket; const AAddress: TNetworkAddress;
  APort: Integer): TTask;
begin
  Result := TConnectTask.Create(ASocket, AAddress, APort);
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
function AsyncSendTo(ASocket: Tsocket; const AAddress: TNetworkAddress;
  APort: Integer; ABuffer: Pointer; ACount: SizeInt): specialize TRVTask<SizeInt
  >;
begin
  Result := TUDPBufferSendTask.Create(ASocket, AAddress, APort, ABuffer, ACount);
end;

generic function AsyncSendTo<T>(ASocket: Tsocket; const AAddress: TNetworkAddress; APort: Integer; const AData: T):  specialize TRVTask<SizeInt>;
begin
  Result := specialize TUDPSendTask<T>.Create(ASocket, AAddress, Aport, AData);
end;

generic function AsyncSendToArray<T>(ASocket: TSocket; const AAddress: TNetworkAddress; APort: Integer; const AData: specialize TArray<T>):  specialize TRVTask<SizeInt>;
begin
  Result := specialize TUDPArraySendTask<T>.Create(ASocket, AAddress, Aport, AData);
end;

function AsyncSendToStr(ASocket: Tsocket; const AAddress: TNetworkAddress;
  APort: Integer; const AData: String): specialize TRVTask<SizeInt>;
begin
  Result := TUDPStringSendTask.Create(ASocket, AAddress, Aport, AData);
end;
{$EndRegion UDP Send}
{$EndRegion Async Functions}

{$Region Socket Helper}

function TCPSocket(AddressType: TAddressType): TSocket;
var
  AFam, v6Only: Integer;
begin
  if AddressType = atIN4 then
    AFam := AF_INET
  else
    AFam := AF_INET6;
  Result := fpsocket(AFam, SOCK_STREAM, 0);
  if SocketInvalid(Result) then
    raise ESocketError.Create(socketerror, 'socket');
  // On IPv6 try to use dual stack
  v6Only := 0;
  if AddressType = atIN6 then
    fpsetsockopt(Result, IPPROTO_IP, IPV6_V6ONLY, @v6Only, SizeOf(v6Only));
end;

function UDPSocket(AddressType: TAddressType): TSocket;
var
  AFam, v6Only: Integer;
begin
  if AddressType = atIN4 then
    AFam := AF_INET
  else
    AFam := AF_INET6;
  Result := fpsocket(AFam, SOCK_DGRAM, 0);
  if SocketInvalid(Result) then
    raise ESocketError.Create(socketerror, 'socket');
  // On IPv6 try to use dual stack
  v6Only := 0;
  if AddressType = atIN6 then
    fpsetsockopt(Result, IPPROTO_IP, IPV6_V6ONLY, @v6Only, SizeOf(v6Only));
end;

function TCPServerSocket(const AAddress: TNetworkAddress; APort: Integer
  ): TSocket;
var
  addr: TAddressUnion;
begin
  Result := TCPSocket(AAddress.AddressType);
  FillAddr(AAddress, APort, @addr);
  if fpbind(Result, @addr, SizeOf(addr)) <> 0 then raise
    ESocketError.Create(socketerror, 'bind (%s:%d)'.Format([AAddress.Address, APort]));
end;

procedure TCPServerListen(AServerSocket: TSocket; Backlog: Integer);
begin
  if fplisten(AServerSocket, Backlog) <> 0 then raise
    ESocketError.Create(socketerror, 'listen');
end;

{$EndRegion Socket Helper}

{$Region Addres Management}

function IN4Address(const Address: String): TNetworkAddress;
begin
 Result := Default(TNetworkAddress);
 Result.Address := Address;
 Result.AddressType := atIN4;
end;

function IN6Address(const Address: String): TNetworkAddress;
begin
 Result := Default(TNetworkAddress);
 Result.Address := Address;
 Result.AddressType := atIN6;
end;

function INAddr(const Address: String): TNetworkAddress;
begin
 Result := Default(TNetworkAddress);
  if Pos(':', Address) = 0 then
    Result.AddressType := atIN4
  else
    Result.AddressType := atIN6;
  Result.Address := Address;
end;

function IN6Equal(const A, B: String): Boolean;
var
  AAddr, BAddr: Tin6_addr;
begin
  AAddr := StrToHostAddr6(A);
  BAddr := StrToHostAddr6(B);
  Result := (AAddr.s6_addr32[0] = BAddr.s6_addr32[0]) and
            (AAddr.s6_addr32[1] = BAddr.s6_addr32[1]) and
            (AAddr.s6_addr32[2] = BAddr.s6_addr32[2]) and
            (AAddr.s6_addr32[3] = BAddr.s6_addr32[3]);
end;

operator=(const A, B: TNetworkAddress): Boolean;
begin
  Result := (A.AddressType = B.AddressType) and (
              ((A.AddressType = atIN4) and (A.Address = B.Address)) or // IPv4: simple string equality
              ((A.AddressType = atIN6) and IN6Equal(A.Address, B.Address)) // IPv6 check binary equality
            );
end;

operator:=(const AStr: String): TNetworkAddress;
begin
  Result := INAddr(AStr);
end;

{$EndRegion Socket Helper}

{$Region Send/Receive Helper}

function IsIPv4Mapped(const IPv6Addr: TIn6Addr): Boolean;
begin
  Result := (IPv6Addr.u6_addr16[0] = 0) and
            (IPv6Addr.u6_addr16[1] = 0) and
            (IPv6Addr.u6_addr16[2] = 0) and
            (IPv6Addr.u6_addr16[3] = 0) and
            (IPv6Addr.u6_addr16[4] = 0) and
            (IPv6Addr.u6_addr16[5] = $FFFF);
end;

function ExtractIPv4Address(const IPv6Addr: TIn6Addr): TNetworkAddress;
begin
  Result := IN4Address('%d.%d.%d.%d'.Format([IPv6Addr.s6_addr8[12],
                                             IPv6Addr.s6_addr8[13],
                                             IPv6Addr.s6_addr8[14],
                                             IPv6Addr.s6_addr8[15]]));
end;

procedure FillAddr(const AAddress: TNetworkAddress; APort: Integer; Addr: PAddressUnion);
begin
  if AAddress.AddressType = atIN4 then
  begin
    Addr^.In4Addr.sin_family := AF_INET;
    Addr^.In4Addr.sin_port := HToNS(APort);
    Addr^.In4Addr.sin_addr.s_addr := LongWord(StrToNetAddr(AAddress.Address));
  end
  else if AAddress.AddressType = atIN6 then
  begin
    Addr^.In6Addr.sin6_family := AF_INET6;
    Addr^.In6Addr.sin6_port := HToNS(APort);
    Addr^.In6Addr.sin6_addr := StrToHostAddr6(AAddress.Address);
    Addr^.In6Addr.sin6_flowinfo := 0;
    Addr^.In6Addr.sin6_scope_id := 0;
  end
  else
    raise EUnsupportedAddress.Create('Address type ' + ord(AAddress.AddressType).ToString + ' not supported');
end;

procedure ReadAddr(Addr: PAddressUnion; out AAddress: TNetworkAddress; out
  APort: Integer);
begin
  if Addr^.In4Addr.sin_family = AF_INET then
  begin
    AAddress := IN4Address(NetAddrToStr(Addr^.In4Addr.sin_addr));
    APort := NToHs(Addr^.In4Addr.sin_port);
  end
  else if Addr^.In6Addr.sin6_family = AF_INET6 then
  begin
    if IsIPv4Mapped(Addr^.In6Addr.sin6_addr) then
      AAddress := ExtractIPv4Address(Addr^.In6Addr.sin6_addr)
    else
      AAddress := IN6Address(HostAddrToStr6(Addr^.In6Addr.sin6_addr));
    APort := NToHs(Addr^.In6Addr.sin6_port);
  end
  else
    raise EUnsupportedAddress.Create('Address Family ' + Addr^.In4Addr.sin_family.ToString + ' not supported');
end;

procedure WaitForHandshake(ATask: TTask; ASocket: Tsocket); //inline;
{$IfDef WINDOWS}
var
  Success, err: Integer;
begin
  repeat
    Success := fpsend(ASocket, @Success, 0, 0);
    if Success < 0 then
    begin
      err := socketerror;
      if err = EsockENOTCONN then
        ATask.Sleep(SocketSleepingTime)
      else
        raise ESocketError.Create(err, 'send');
    end;
  until Success = 0;
end;
{$Else}
var
  success, err: LongInt;
  // long enough for any kind of address
  addr: array[0..31] of byte;
  len: SizeInt;
begin
  repeat
    len := 32;
    success := fpgetpeername(ASocket, @addr, @len);
    if success < 0 then
    begin
      err := socketerror;
      if err = ESockENOTCONN then
        ATask.Sleep(SocketSleepingTime)
      else
        raise ESocketError.Create(err, 'getpeername');
    end;
  until success = 0;
end;
{$EndIf}

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
  addr: TAddressUnion;
begin
  FillAddr(FAddress, FPort, @addr);
  FResult := WaitingSendTo(Self, FSocket, @FData, SizeOf(T), 0, @addr, SizeOf(addr));
end;

constructor TUDPSendTask.Create(ASocket: TSocket; const AAddress: TNetworkAddress;
  APort: Integer; const AData: T);
begin
  inherited Create;
  FSocket := ASocket;
  FAddress := AAddress;
  FPort := APort;
  FData := AData;
end;

{ TUDPStringSendTask }

procedure TUDPStringSendTask.Execute;
var
  addr: TAddressUnion;
begin
  FillAddr(FAddress, FPort, @addr);
  FResult := WaitingSendTo(Self, FSocket, PChar(FData), Length(FData), 0, @addr, SizeOf(addr));
end;

constructor TUDPStringSendTask.Create(ASocket: TSocket; const AAddress: TNetworkAddress;
  APort: Integer; const AData: String);
begin
  inherited Create;
  FSocket := ASocket;
  FAddress := AAddress;
  FPort := APort;
  FData := AData;
end;

{ TUDPArraySendTask }

procedure TUDPArraySendTask.Execute;
var
  addr: TAddressUnion;
begin
  FillAddr(FAddress, FPort, @addr);
  FResult := WaitingSendTo(Self, FSocket, @FData[0], Length(FData) * SizeOf(T), 0, @addr, SizeOf(addr));
end;

constructor TUDPArraySendTask.Create(ASocket: TSocket; const AAddress: TNetworkAddress;
  APort: Integer; const AData: TArrayType);
begin
  Inherited Create;
  FSocket := ASocket;
  FAddress := AAddress;
  FPort := APort;
  FData := AData;
end;

{ TUDPBufferSendTask }

procedure TUDPBufferSendTask.Execute;
var
  addr: TAddressUnion;
begin
  FillAddr(FAddress, FPort, @addr);
  FResult := WaitingSendTo(Self, FSocket, FBuffer, FCount, 0, @addr, SizeOf(addr));
end;

constructor TUDPBufferSendTask.Create(ASocket: TSocket; const AAddress: TNetworkAddress;
  APort: Integer; ABuffer: Pointer; ACount: SizeInt);
begin
  inherited Create;
  FSocket := ASocket;
  FAddress := AAddress;
  FPort := APort;
  FBuffer := ABuffer;
  FCount := ACount;
end;

{ TUDPReceiveTask }

procedure TUDPReceiveTask.Execute;
var
  {$IfNDef WINDOWS}
  _addr: TAddressUnion;
  _addrLen: SizeInt;
  {$EndIf}
  addr: PAddressUnion;
  addrLen: PSizeInt;
  dataLen: SizeInt;
begin
inherited Execute;
  {$IfDef WINDOWS}
  // WinSock doesn't like the addr located on the stack, therefore we create a heap instance for it
  New(addr);
  New(addrLen);
  try
  {$Else}
  addr := @_addr;
  addrLen := @_addrLen;
  {$EndIf}
  addrLen^ := SizeOf(TAddressUnion);
  dataLen := WaitingRecvFrom(Self, FSocket, @FResult.Data, SizeOf(T), 0, addr, addrLen);
  ReadAddr(addr, FResult.Address, FResult.Port);
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
  _addr: TAddressUnion;
  _addrLen: SizeInt;
  {$EndIf}
  addr: PAddressUnion;
  addrLen: PSizeInt;
  dataLen: SizeInt;
begin
  inherited Execute;
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
  addrLen^ := SizeOf(TAddressUnion);
  dataLen := WaitingRecvFrom(Self, FSocket, PChar(FResult.Data), FMaxLength, 0, addr, addrLen);
  SetLength(FResult.Data, dataLen);
  ReadAddr(addr, FResult.Address, FResult.Port);;
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
  _addr: TAddressUnion;
  _addrLen: SizeInt;
  {$EndIf}
  addr: PAddressUnion;
  addrLen: PSizeInt;
  dataLen: SizeInt;
begin
  inherited Execute;
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
  addrLen^ := SizeOf(TAddressUnion);
  dataLen := WaitingRecvFrom(Self, FSocket, @FResult.Data[0], FMaxCount * SizeOf(T), 0, addr, addrLen);
  SetLength(FResult.Data, dataLen div SizeOf(T));
  ReadAddr(addr, FResult.Address, FResult.Port);
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
  _addr: TAddressUnion;
  _addrLen: SizeInt;
  {$EndIf}
  addr: PAddressUnion;
  addrLen: PSizeInt;
begin
  inherited Execute;
  {$IfDef WINDOWS}
  // WinSock doesn't like the addr located on the stack, therefore we create a heap instance for it
  New(addr);
  New(addrLen);
  try
  {$Else}
  addr := @_addr;
  addrLen := @_addrLen;
  {$EndIf}
  addrLen^ := SizeOf(TAddressUnion);
  FResult.Size := WaitingRecvFrom(Self, FSocket, FBuffer, FBufferLen, 0, addr, addrLen);
  ReadAddr(addr, FResult.Address, FResult.Port);
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
  addr: TAddressUnion;
begin
  OldState := SetNonBlocking(FSocket);
  try
    FillAddr(FAddress, FPort, @addr);
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

constructor TConnectTask.Create(ASocket: Tsocket; const AAddress: TNetworkAddress;
  APort: Integer);
begin
  inherited Create;
  FSocket := ASocket;
  FAddress := AAddress;
  FPort := APort;
end;

{ TAcceptTask }

procedure TAcceptTask.Execute;
var
  OldState, err: LongInt;
  Conn: TSocket;
  addr: TAddressUnion;
  addrLen: SizeInt;
begin
  inherited Execute;
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
  finally
    RestoreBlocking(FServerSocket, OldState);
  end;
  FResult.Connection := Conn;
  ReadAddr(@Addr, FResult.PeerAddress, FResult.PeerPort);
end;

constructor TAcceptTask.Create(AServerSocket: Tsocket);
begin
  inherited Create;
  FServerSocket := AServerSocket;
end;

{$EndRegion Tasks}

end.

