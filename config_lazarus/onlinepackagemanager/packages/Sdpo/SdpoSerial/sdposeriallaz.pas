{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit SdpoSerialLaz; 

interface

uses
  SdpoSerial, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('SdpoSerial', @SdpoSerial.Register); 
end; 

initialization
  RegisterPackage('SdpoSerialLaz', @Register); 
end.
