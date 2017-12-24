{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lazextwebbase;

interface

uses
  fonctions_pcomponents, fonctions_pdbcomponents, fonctions_perreurs, 
  fonctions_planguages, u_extpcomponent, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('lazextwebbase', @Register);
end.
