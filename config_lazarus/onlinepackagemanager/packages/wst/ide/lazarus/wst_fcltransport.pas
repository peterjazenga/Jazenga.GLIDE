{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit wst_fcltransport;

{$warn 5023 off : no warning about unused units}
interface

uses
  fpc_http_protocol, fpc_tcp_protocol, fpc_http_server, fpc_tcp_server, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('wst_fcltransport', @Register);
end.
