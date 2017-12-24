{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lazextinit;

interface

uses
  u_extmenucustomize, U_OnFormInfoIni, u_extmenutoolbar, U_FormAdapt, 
  U_FormMainIni, U_CustomizeMenu, fonctions_init, fonctions_vtree, menutbar, 
  u_connection, u_scrollclones, fonctions_forms, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('lazextinit', @Register);
end.
