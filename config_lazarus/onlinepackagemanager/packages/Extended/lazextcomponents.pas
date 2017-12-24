{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lazextcomponents;

interface

uses
  PDBCheck, PCheck, U_ExtColorCombos, U_ExtDBNavigator, u_extradios, 
  u_extformatedits, u_framework_components, u_framework_dbcomponents, 
  u_extsearchedit, U_ExtComboInsert, fonctions_search_edit, 
  u_extDBDirectoryEdit, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('lazextcomponents', @Register);
end.
