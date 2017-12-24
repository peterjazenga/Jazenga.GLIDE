{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit exdatabase;

{$warn 5023 off : no warning about unused units}
interface

uses
  AllExDatabaseReg, TplDBTreeviewUnit, TplDBTreeviewBaseUnit, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('AllExDatabaseReg', @AllExDatabaseReg.Register);
end;

initialization
  RegisterPackage('exdatabase', @Register);
end.
