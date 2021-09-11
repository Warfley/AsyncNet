{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit AsyncNet;

{$warn 5023 off : no warning about unused units}
interface

uses
  asyncnet.compatibility, asyncnet.sockets, asyncnet.netdb, asyncnet.resolve, 
  asyncnet.sslbase, asyncnet.sslsockets, asyncnet.ssockets, 
  asyncnet.dns.resrecords, asyncnet.dns, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('AsyncNet', @Register);
end.
