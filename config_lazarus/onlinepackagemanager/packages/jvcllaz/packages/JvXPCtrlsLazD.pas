{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit JvXPCtrlsLazD;

{$warn 5023 off : no warning about unused units}
interface

uses
  JvXPCtrlsReg, JvXPPropertyEditors, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('JvXPCtrlsReg', @JvXPCtrlsReg.Register);
end;

initialization
  RegisterPackage('JvXPCtrlsLazD', @Register);
end.
