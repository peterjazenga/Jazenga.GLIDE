{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit JvCtrlsLazD;

{$warn 5023 off : no warning about unused units}
interface

uses
  JvCtrlsReg, JvHTHintForm, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('JvCtrlsReg', @JvCtrlsReg.Register);
end;

initialization
  RegisterPackage('JvCtrlsLazD', @Register);
end.
