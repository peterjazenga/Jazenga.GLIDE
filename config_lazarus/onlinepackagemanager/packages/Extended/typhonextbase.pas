{ This file was automatically created by Typhon IDE. Do not edit!
  This source is only used to compile and install the package.
 }

unit typhonextbase;

{$warn 5023 off : no warning about unused units}
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
  RegisterPackage('typhonextbase', @Register);
end.
