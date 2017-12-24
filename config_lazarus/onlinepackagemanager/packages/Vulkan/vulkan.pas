{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit vulkan;

{$warn 5023 off : no warning about unused units}
interface

uses
  vulkanapi, vulkanobjects, vulkanutils, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('vulkan', @Register);
end.
