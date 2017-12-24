{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lazextbase;

interface

uses
  fonctions_languages, fonctions_erreurs, u_extcomponent, 
  fonctions_components, fonctions_dbcomponents, fonctions_numedit, 
  fonctions_keyboard, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('lazextbase', @Register);
end.
