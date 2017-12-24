{ This file was automatically created by Typhon IDE. Do not edit!
  This source is only used to compile and install the package.
 }

unit typhonextnet;

{$warn 5023 off : no warning about unused units}
interface

uses
  u_netupdate, fonctions_net, u_mailssendbutton, u_sendmail, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('typhonextnet', @Register);
end.
