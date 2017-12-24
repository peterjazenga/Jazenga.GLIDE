{ This file was automatically created by Lazarus. do not edit!
  This source is only used to compile and install the package.
 }

unit SdpoDSMLaz; 

interface

uses
SdpoDSM, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('SdpoDSM', @SdpoDSM.Register); 
end; 

initialization
  RegisterPackage('SdpoDSMLaz', @Register); 
end.
