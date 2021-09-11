program dnsresolve;

{$mode objfpc}{$H+}

uses
  SysUtils, sockets, stax, stax.functional, AsyncNet.dns, AsyncNet.dns.resrecords;

procedure DNSTestUDP(AExecutor: TExecutor);
var
  DNSResponse: TDNSResponse;
  i: Integer;
begin
  try
    DNSResponse := specialize Await<TDNSResponse>(AsyncDNSRequest('8.8.8.8', [DNSQuestion('forum.lazarus.freepascal.org')]));
  except
    on E: Exception do
    begin
      WriteLn(E.Message);
      Exit;
    end;
  end;
  for i:=0 to Length(DNSResponse.Answers.CNAMERecords) - 1 do
    WriteLn('CNAME: ', DNSResponse.Answers.CNAMERecords[i].Data);
  for i:=0 to Length(DNSResponse.Answers.ARecords) - 1 do
    WriteLn('A: ', HostAddrToStr(DNSResponse.Answers.ARecords[i].Data));
end;

procedure DNSTestTCP(AExecutor: TExecutor);
var
  DNSResponse: TDNSResponse;
  i: Integer;
begin
  try
    DNSResponse := specialize Await<TDNSResponse>(AsyncDNSRequest('8.8.8.8', [DNSQuestion('forum.lazarus.freepascal.org')], -1, True, DefaultDNSPort, False));
  except
    on E: Exception do
    begin
      WriteLn(E.Message);
      Exit;
    end;
  end;
  for i:=0 to Length(DNSResponse.Answers.CNAMERecords) - 1 do
    WriteLn('CNAME: ', DNSResponse.Answers.CNAMERecords[i].Data);
  for i:=0 to Length(DNSResponse.Answers.ARecords) - 1 do
    WriteLn('A: ', HostAddrToStr(DNSResponse.Answers.ARecords[i].Data));
end;

var
  Exec: TExecutor;
begin
  Exec := TExecutor.Create;
  try;
    Exec.RunAsync(AsyncProcedure(@DNSTestUDP));
    Exec.RunAsync(AsyncProcedure(@DNSTestTCP));
    Exec.Run;
  finally
    Exec.Free;
  end;
  ReadLn;
end.

