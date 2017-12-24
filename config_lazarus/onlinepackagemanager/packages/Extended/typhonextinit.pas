{ This file was automatically created by Typhon IDE. Do not edit!
  This source is only used to compile and install the package.
 }

unit typhonextinit;

{$warn 5023 off : no warning about unused units}
interface

uses
  u_extmenucustomize, U_OnFormInfoIni, u_extmenutoolbar, U_FormAdapt, 
  U_FormMainIni, fonctions_forms, U_CustomizeMenu, fonctions_vtree, menutbar, 
  u_connection, u_scrollclones, fonctions_init, fonctions_scaledpi, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('typhonextinit', @Register);
end.
