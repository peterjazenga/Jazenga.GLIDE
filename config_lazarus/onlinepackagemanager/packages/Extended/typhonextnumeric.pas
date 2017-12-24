{ This file was automatically created by Typhon IDE. Do not edit!
  This source is only used to compile and install the package.
 }

unit typhonextnumeric;

{$warn 5023 off : no warning about unused units}
interface

uses
  u_extmultioperation, U_ExtNumEdits, u_extconstoperation, u_extoperate, 
  U_ExtOperation, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('typhonextnumeric', @Register);
end.
