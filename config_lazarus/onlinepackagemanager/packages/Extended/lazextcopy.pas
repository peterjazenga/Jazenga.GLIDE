{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lazextcopy;

interface

uses
  u_traducefile, U_ExtFileCopy, u_extabscopy, u_extractfile, 
  unit_messagescopy, u_extfilecomp, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('lazextcopy', @Register);
end.
