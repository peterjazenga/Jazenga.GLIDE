{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit laz_chemtext;

{$warn 5023 off : no warning about unused units}
interface

uses
  chemtext, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('chemtext', @chemtext.Register);
end;

initialization
  RegisterPackage('laz_chemtext', @Register);
end.
