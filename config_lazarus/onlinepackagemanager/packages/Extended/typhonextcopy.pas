{ This file was automatically created by Typhon IDE. Do not edit!
  This source is only used to compile and install the package.
 }

unit typhonextcopy;

{$warn 5023 off : no warning about unused units}
interface

uses
  u_traducefile, U_ExtFileCopy, u_extabscopy, u_extractfile, 
  unit_messagescopy, u_extfilecomp, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('typhonextcopy', @Register);
end.
