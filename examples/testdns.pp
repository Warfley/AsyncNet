program testdns;

{$Mode ObjFpc}
{$H+}

uses SysUtils, AsyncNet.netdb, stax, stax.functional;

procedure TestDNSResolve;
var
  ResolveResult: TDNSResolveResult;
begin
  WriteLn('Testing DNS resolve of "google.com" at "8.8.8.8"');
  Write('  IPv4: ');
  try
    ResolveResult := specialize Await<TDNSResolveResult>(AsyncDNSResolve('google.com', INAddr('8.8.8.8')));
    Write(ResolveResult.Address.Address, ' TTL: ');
    if ResolveResult.TTL = QWord.MaxValue then
      WriteLn('forever')
    else
      WriteLn(ResolveResult.TTL);
  except
    WriteLn('Failure');
  end;
  Write('  IPv6: ');
  try
    ResolveResult := specialize Await<TDNSResolveResult>(AsyncDNSResolve('google.com', INAddr('8.8.8.8'), atIN6));
    Write(ResolveResult.Address.Address, ' TTL: ');
    if ResolveResult.TTL = QWord.MaxValue then
      WriteLn('forever')
    else
      WriteLn(ResolveResult.TTL);;
  except
    WriteLn('Failure');
  end;
  WriteLn;
end;

procedure TestResolve(const HostName: String);
var
  ResolveResult: TDNSResolveResult;
begin
  WriteLn('Resolving ', HostName);
  Write('  IPv4 pref: ');
  try
    ResolveResult := specialize Await<TDNSResolveResult>(AsyncResolveName(HostName));
    Write(ResolveResult.Address.Address, ' TTL: ');
    if ResolveResult.TTL = QWord.MaxValue then
      WriteLn('forever')
    else
      WriteLn(ResolveResult.TTL);
  except
    WriteLn('Failure');
  end;
  Write('  IPv6 pref: ');
  try
    ResolveResult := specialize Await<TDNSResolveResult>(AsyncResolveName(HostName, atIN6));
    Write(ResolveResult.Address.Address, ' TTL: ');
    if ResolveResult.TTL = QWord.MaxValue then
      WriteLn('forever')
    else
      WriteLn(ResolveResult.TTL);
  except
    WriteLn('Failure');
  end;
end;

procedure TestNetDB(AExecutor: TExecutor);
begin
  TestDNSResolve;
  TestResolve('localhost');
  TestResolve('google.com');
  TestResolve('forum.lazarus.freepascal.org');
end;

var
  Exec: TExecutor;
begin
  Exec := TExecutor.Create;
  try
    Exec.RunAsync(AsyncProcedure(@TestNetDB));
    Exec.Run;
  finally
    Exec.Free;
  end;
  ReadLn;
end.
