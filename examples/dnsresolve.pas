program dnsresolve;

{$mode objfpc}{$H+}

uses
  SysUtils, sockets, stax, stax.functional, AsyncNet.sockets, AsyncNet.dns, AsyncNet.dns.resrecords;

procedure PrintDNSResponse(AResp: TDNSResponse);
procedure PrintRecordArray(AResponseSection: TRecordArray);
var
  i: Integer;
begin
  for i:=0 to Length(AResponseSection.CNAMERecords) - 1 do
    WriteLn('  CNAME: ', AResponseSection.CNAMERecords[i].Data);
  for i:=0 to Length(AResponseSection.ARecords) - 1 do
    WriteLn('  A (', AResponseSection.ARecords[i].Name, '): ', HostAddrToStr(AResponseSection.ARecords[i].Data));
  for i:=0 to Length(AResponseSection.AAAARecords) - 1 do
    WriteLn('  AAAA (', AResponseSection.AAAARecords[i].Name, '): ', HostAddrToStr6(AResponseSection.AAAARecords[i].Data));
end;

var
  i: Integer;
begin
  WriteLn('Authorative: ', AResp.AuthorityAnswer);
  WriteLn('Recursive: ', AResp.AuthorityAnswer);
  WriteLn('Answers:');
  PrintRecordArray(AResp.Answers);
  WriteLn('Authorities:');
  for i:=0 to Length(AResp.Authorities) - 1 do
    WriteLn('  NS: ', AResp.Authorities[i].Data);
  WriteLn('Additional:');
  PrintRecordArray(AResp.Additional);
end;

procedure DNSTestUDP;
var
  DNSResponse: TDNSResponse;
begin
  WriteLn('Testing 8.8.8.8 UDP');
  try
    DNSResponse := specialize Await<TDNSResponse>(AsyncDNSRequest('8.8.8.8', [DNSQuestion('forum.lazarus.freepascal.org')]));
  except
    on E: Exception do
    begin
      WriteLn(E.Message);
      Exit;
    end;
  end;
  PrintDNSResponse(DNSResponse);
end;

procedure DNSTestTCP;
var
  DNSResponse: TDNSResponse;
begin
  WriteLn('Testing 8.8.8.8 TCP');
  try
    DNSResponse := specialize Await<TDNSResponse>(AsyncDNSRequest('8.8.8.8', [DNSQuestion('forum.lazarus.freepascal.org')], -1, True, DefaultDNSPort, False));
  except
    on E: Exception do
    begin
      WriteLn(E.Message);
      Exit;
    end;
  end;
  PrintDNSResponse(DNSResponse);
end;

procedure DNSTestNonRecursive;
var
  DNSResponse: TDNSResponse;
begin
  WriteLn('Testing root server (non recursive expected)');
  try
    // Root server can't go recuirsive
    DNSResponse := specialize Await<TDNSResponse>(AsyncDNSRequest('192.5.5.241', [DNSQuestion('forum.lazarus.freepascal.org')]));
  except
    on E: Exception do
    begin
      WriteLn(E.Message);
      Exit;
    end;
  end;

  PrintDNSResponse(DNSResponse);
end;

procedure TestDNS(AExecutor: TExecutor);
begin
  DNSTestUDP;
  DNSTestNonRecursive;
  //DNSTestTCP;
end;

var
  Exec: TExecutor;
begin
  Exec := TExecutor.Create;
  try;
    Exec.RunAsync(AsyncProcedure(@TestDNS));
    Exec.Run;
  finally
    Exec.Free;
  end;
  ReadLn;
end.

