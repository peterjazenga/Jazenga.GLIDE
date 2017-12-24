{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit JvValidatorsLazD;

{$warn 5023 off : no warning about unused units}
interface

uses
  JvValidatorReg, JvValidatorsEditorForm, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('JvValidatorReg', @JvValidatorReg.Register);
end;

initialization
  RegisterPackage('JvValidatorsLazD', @Register);
end.
