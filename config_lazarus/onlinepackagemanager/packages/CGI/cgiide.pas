{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit cgiide;

{$warn 5023 off : no warning about unused units}
interface

uses
  CGILazIDEIntf, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('CGILazIDEIntf', @CGILazIDEIntf.Register);
end;

initialization
  RegisterPackage('cgiide', @Register);
end.
