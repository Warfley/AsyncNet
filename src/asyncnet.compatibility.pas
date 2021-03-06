unit asyncnet.compatibility;

{$mode objfpc}{$H+}
{$TypedAddress ON}

interface

uses
  SysUtils, Sockets {$IfDef Windows}, WinSock2{$Else}, BaseUnix{$EndIf};

const
  IPPROTO_IPV6 = {$IfDef WINDOWS}41{$Else}41{$EndIf};
  IPV6_V6ONLY = {$IfDef WINDOWS}27{$Else}26{$EndIf};

type
  TSocket = {$IfDef Windows}WinSock2.TSocket{$Else}Sockets.Tsocket{$EndIf};
  TFDSet = {$IfDef Windows}WinSock2.TFDSet{$Else}BaseUnix.TFDSet{$EndIf};
  PFDSet = {$IfDef Windows}WinSock2.PFDSet{$Else}BaseUnix.PFDSet{$EndIf};
  TTimeVal = {$IfDef Windows}WinSock2.TTimeVal{$Else}BaseUnix.TTimeVal{$EndIf};
  PTimeVal = {$IfDef Windows}WinSock2.PTimeVal{$Else}BaseUnix.PTimeVal{$EndIf};

  PAddressUnion = ^TAddressUnion;
  TAddressUnion = record
  case Boolean of
  True: (In4Addr: Sockets.sockaddr_in);
  False: (In6Addr: Sockets.sockaddr_in6);
  end;

function SetNonBlocking(ASocket: Tsocket): Integer;
procedure RestoreBlocking(ASocket: Tsocket; OldState: Integer);

function SocketInvalid(ASocket: TSocket): Boolean; inline;

procedure FD_CLR(ASocket: TSocket; var FDSet: TFDSet); inline;
function FD_ISSET(ASocket: TSocket; var FDSet: TFDSet): Boolean; inline;
procedure FD_SET(ASocket: TSocket; var FDSet: TFDSet); inline;
procedure FD_ZERO(var FDSet: TFDSet); inline;
function select(nfds: Integer; ReadFDs,  WriteFDs, ExceptFDs: PFDSet; Timeout: PTimeVal): Integer; inline;

function WasBlockingError(AError: Integer): Boolean; inline;

implementation

procedure FD_CLR(ASocket: TSocket; var FDSet: TFDSet);
begin
  {$IfDef Windows}
  WinSock2.FD_CLR(ASocket, FDSet);
  {$Else}
  fpFD_CLR(ASocket, FDSet);
  {$EndIf}
end;

function FD_ISSET(ASocket: TSocket; var FDSet: TFDSet): Boolean;
begin
  {$IfDef Windows}
  Result := WinSock2.FD_ISSET(ASocket, FDSet);
  {$Else}
  Result := fpFD_ISSET(ASocket, FDSet) <> 0;
  {$EndIf}
end;

procedure FD_SET(ASocket: TSocket; var FDSet: TFDSet);
begin
  {$IfDef Windows}
  WinSock2.FD_SET(ASocket, FDSet);
  {$Else}
  fpFD_SET(ASocket, FDSet);
  {$EndIf}
end;

procedure FD_ZERO(var FDSet: TFDSet);
begin
  {$IfDef Windows}
  WinSock2.FD_ZERO(FDSet);
  {$Else}
  fpFD_ZERO(FDSet);
  {$EndIf}
end;

function select(nfds: Integer; ReadFDs,  WriteFDs, ExceptFDs: PFDSet; Timeout: PTimeVal): Integer;
begin
  {$IfDef Windows}
  Result := WinSock2.select(nfds, ReadFDs, WriteFds, ExceptFDs, Timeout);
  {$Else}
  Result := fpSelect(nfds, ReadFDs, WriteFds, ExceptFDs, Timeout);
  {$EndIf}
end;

function WasBlockingError(AError: Integer): Boolean;
begin
  {$IfDef Windows}
  Result := AError = EsockEWOULDBLOCK;
  {$Else}
  Result := (AError = EsockEWOULDBLOCK) or (AError = ESysEAGAIN) or (AError = ESysEINPROGRESS);
  {$EndIf}
end;

function SetNonBlocking(ASocket: Tsocket): Integer;
{$IfDef Windows}
var
  nonblock: u_long;
begin
  nonblock := 1;
  ioctlsocket(ASocket, LongInt(FIONBIO), @nonblock);
  Result := 0;
end;
{$Else}
begin
  Result := FpFcntl(ASocket, F_GetFl);
  FpFcntl(ASocket, F_SetFL, Result Or O_NONBLOCK);
end;
{$EndIf}

procedure RestoreBlocking(ASocket: Tsocket; OldState: Integer);
{$IfDef Windows}
var
  nonblock: u_long;
begin
  nonblock := OldState;
  ioctlsocket(ASocket, LongInt(FIONBIO), @nonblock);
end;
{$Else}
begin
  FpFcntl(ASocket, F_SetFL, OldState);
end;
{$EndIf}

function SocketInvalid(ASocket: TSocket): Boolean;
begin
  {$IfDef Windows}
  Result := ASocket = TSocket(INVALID_SOCKET);
  {$Else}
  Result := ASocket < 0;
  {$EndIf}
end;

end.

