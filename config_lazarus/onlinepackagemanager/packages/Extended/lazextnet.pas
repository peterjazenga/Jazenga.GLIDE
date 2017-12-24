{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lazextnet;

interface

uses
  u_netupdate, fonctions_net, u_mailssendbutton, u_sendmail, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('lazextnet', @Register);
end.
