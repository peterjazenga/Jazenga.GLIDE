{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit exgeographic;

{$warn 5023 off : no warning about unused units}
interface

uses
  AllExGeographicRegister, flagcomponentbaseunit, flagcustomunit, flagtype, 
  TplVectorFlagUnit, upascaltz, upascaltz_types, usorters, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('AllExGeographicRegister', @AllExGeographicRegister.Register);
end;

initialization
  RegisterPackage('exgeographic', @Register);
end.
