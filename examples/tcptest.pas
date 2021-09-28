program tcptest;

{$mode objfpc}{$H+}

uses
  SysUtils, Sockets, stax, stax.functional, AsyncNet.sockets;

procedure RunServer(AExecutor: TExecutor);
var
  ServerSock: TSocket;
  Conn: TAcceptResult;
  Data: String;
begin
  // IPv6 server capable of dual stack, i.e. can also receive from IPv4
  ServerSock := TCPServerSocket('::0', 1337);
  try
    TCPServerListen(ServerSock, 10);
    Conn := specialize Await<TAcceptResult>(AsyncAccept(ServerSock));
    try
      WriteLn('Server: Connection from ', Conn.PeerAddress.Address, ':', Conn.PeerPort);
      Data := specialize Await<String>(AsyncReceiveStr(Conn.Connection, 1024, False));
      WriteLn('Server: received ', Data);
      Await(AsyncSendStr(Conn.Connection, 'Hello Client'));
    finally
      CloseSocket(Conn.Connection);
    end;
  finally
    CloseSocket(ServerSock);
  end;
end;

procedure RunClient(AExecutor: TExecutor);
var
  Sock: TSocket;
  Data: String;
begin
  AsyncSleep(100); // wait for server to start
  Sock := TCPSocket(atIN4);
  try
    Await(AsyncConnect(Sock, '127.0.0.1', 1337));
    WriteLn('Client: Connection established');
    Await(AsyncSendStr(Sock, 'Hello Server'));
    Data := specialize Await<String>(AsyncReceiveStr(Sock, 1024, False));
    WriteLn('Client: received ', Data);
  finally
    CloseSocket(Sock);
  end;
end;

var
  Exec: TExecutor;
begin
  Exec := TExecutor.Create;
  try
    Exec.RunAsync(AsyncProcedure(@RunServer));
    Exec.RunAsync(AsyncProcedure(@RunClient));
    Exec.Run;
  finally
    Exec.Free;
  end;
  ReadLn;
end.

