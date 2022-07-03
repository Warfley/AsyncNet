program netdbconftest;

{$mode objfpc}{$H+}

uses
  asyncnet.netdb;

procedure PrintResolve;
var
  s: String;
begin
  ResolveDB.BeginAccess;
  try
    WriteLn('DNS Settings:');
    WriteLn('  Timeout: ', ResolveDB.DNSTimeOut);
    WriteLn('  Attempts: ', ResolveDB.Attempts);
    WriteLn('  Label threshold: ', ResolveDB.LabelThreshold);
    WriteLn('  Servers:');
    for s in ResolveDB.DNSServers do
      WriteLn('    ', s);
    WriteLn('  Search domains:');
    for s in ResolveDB.SearchDomains do
      WriteLn('    ', s);
  finally
    ResolveDB.EndAccess;
  end;
end;

procedure PrintNetworks;
var
  net: TNetworkEntry;
begin
  NetworksDB.BeginAccess;
  try
    WriteLn('Networks:');
    for net in NetworksDB.Networks do
      WriteLn('  ', net.Name, ': ', net.Addr.Address);
  finally
    NetworksDB.EndAccess;
  end;
end;

procedure PrintServices;
var
  srv: TServiceEntry;
begin
  ServiceDB.BeginAccess;
  try
    WriteLn('Services:');
    for srv in ServiceDB.Services do
      WriteLn('  ', srv.Name, ': ', srv.Port, ' via ', srv.Protocol);
  finally
    ServiceDB.EndAccess;
  end;
end;

procedure PrintHosts;
var
  h: THostEntry;
begin
  HostsDB.BeginAccess;
  try
    WriteLn('Hosts:');
    for h in HostsDB.Hosts do
      WriteLn('  ', h.Name, ': ', h.Addresses[0].Address);
  finally
    HostsDB.EndAccess;
  end;
end;

procedure PrintProtocols;
var
  p: TProtocolEntry;
begin
  ProtocolDB.BeginAccess;
  try
    WriteLn('Protocols:');
    for p in ProtocolDB.Protocols do
      WriteLn('  ', p.Name, ': ', p.Number);
  finally
    ProtocolDB.EndAccess;
  end;
end;

begin
  PrintResolve;
  PrintHosts;
  PrintNetworks;
  PrintProtocols;
  PrintServices;
  ReadLn;
end.

