{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit SdpoDynmatrix; 

interface

uses
  dynmatrix, dynmatrixutils, rotations, svd3x3, LazarusPackageIntf;

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('SdpoDynmatrix', @Register); 
end.
