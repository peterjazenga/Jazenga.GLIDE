{ This file was automatically created by Typhon IDE. Do not edit!
  This source is only used to compile and install the package.
 }

unit typhonextreports;

{$warn 5023 off : no warning about unused units}
interface

uses
  fonctions_reports, u_reportform, u_reports_rlcomponents, 
  u_reports_components, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('typhonextreports', @Register);
end.
