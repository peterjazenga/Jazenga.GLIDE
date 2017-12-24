{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit uGraphDSGN;

{$warn 5023 off : no warning about unused units}
interface

uses
  uReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('uReg', @uReg.Register);
end;

initialization
  RegisterPackage('uGraphDSGN', @Register);
end.
