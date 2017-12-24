{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit attabs_package;

{$warn 5023 off : no warning about unused units}
interface

uses
  attabs_register, ATTabs, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('attabs_register', @attabs_register.Register);
end;

initialization
  RegisterPackage('attabs_package', @Register);
end.
