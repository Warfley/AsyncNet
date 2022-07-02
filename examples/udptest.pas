program udptest;

{$mode objfpc}{$H+}

uses
  SysUtils, Sockets, stax, stax.functional, AsyncNet.sockets;

// Simple UDP echo server
procedure RunServer(AExecutor: TExecutor);
var
  ServerSock: TSocket;
  Message: specialize TUDPReceiveResult<String>;
begin
  // IPv6 server capable of dual stack, i.e. can also receive from IPv4
  ServerSock := UDPServerSocket('::0', 1337);
  try
    Message := specialize Await<specialize TUDPReceiveResult<String>>(AsyncReceiveFromStr(ServerSock));
    WriteLn('Message from: ', Message.Address.Address, ':', Message.Port, ': ', Message.Data);
    Await(AsyncSendToStr(ServerSock, Message.Address, Message.Port, Message.Data));
  finally
    CloseSocket(ServerSock);
  end;
end;

procedure RunClient(AExecutor: TExecutor);
var
  Sock: TSocket;
  Message: specialize TUDPReceiveResult<String>;
begin
  AsyncSleep(100); // wait for server to start
  Sock := UDPSocket(atIN6);
  try
    // As IN6 uses dual stack we can connect to an IPv4 address
    Await(AsyncSendToStr(Sock, IN4MappedIN6Address('127.0.0.1'), 1337, 'Hello Server')); 
    Message := specialize Await<specialize TUDPReceiveResult<String>>(AsyncReceiveFromStr(Sock));
    WriteLn('Client: received ', Message.Data);
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

