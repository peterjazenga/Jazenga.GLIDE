{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit JvDBLazD;

{$warn 5023 off : no warning about unused units}
interface

uses
  JvDBReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('JvDBReg', @JvDBReg.Register);
end;

initialization
  RegisterPackage('JvDBLazD', @Register);
end.
