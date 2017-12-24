{ This file was automatically created by Lazarus. do not edit ! 
  This source is only used to compile and install the package.
 }

unit smnetgradientlaz; 

interface

uses
  SMNetGradient, register_smnetgradient, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('register_smnetgradient', @register_smnetgradient.Register); 
end; 

initialization
  RegisterPackage('smnetgradientlaz', @Register); 
end.
