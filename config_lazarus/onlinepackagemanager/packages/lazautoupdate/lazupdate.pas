{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lazupdate;

{$warn 5023 off : no warning about unused units}
interface

uses
  ulazautoupdate, aboutlazautoupdateunit, VersionSupport, uappisrunning, 
  lazautoupdate_httpclient, open_ssl, ushortcut, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ulazautoupdate', @ulazautoupdate.Register);
  RegisterUnit('aboutlazautoupdateunit', @aboutlazautoupdateunit.Register);
end;

initialization
  RegisterPackage('lazupdate', @Register);
end.
