{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lazextnumeric;

interface

uses
  u_extmultioperation, U_ExtNumEdits, u_extconstoperation, u_extoperate, 
  U_ExtOperation, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('lazextnumeric', @Register);
end.
