{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lazextweb;

interface

uses
  u_extpdbnavigator, fonctions_psearch_edit, u_extpcolorcombos, 
  U_ExtPComboInsert, u_extp_components, u_extp_dbcomponents, 
  u_extpdbdirectoryedit, u_extpformatedits, u_extpradios, u_extpsearchedit, 
  u_pscrolldbclones, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('lazextweb', @Register);
end.
