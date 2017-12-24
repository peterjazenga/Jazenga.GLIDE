{ This file was automatically created by Typhon IDE. Do not edit!
  This source is only used to compile and install the package.
 }

unit typhonregisteribx;

{$warn 5023 off : no warning about unused units}
interface

uses
  fonctions_startibx, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('typhonregisteribx', @Register);
end.
