{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit virtualdbtreeexlaz; 

interface

uses
    virtualdbtreeex, virtualghfdbtreeex, virtualghfdbtreesreg, 
  virtualdbtreesreg, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('virtualghfdbtreesreg', @virtualghfdbtreesreg.Register); 
  RegisterUnit('virtualdbtreesreg', @virtualdbtreesreg.Register); 
end; 

initialization
  RegisterPackage('virtualdbtreeexlaz', @Register); 
end.
