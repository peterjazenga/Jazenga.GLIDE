{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit SdpoFastFormLaz; 

interface

uses
  SdpoFastForm, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('SdpoFastForm', @SdpoFastForm.Register); 
end; 

initialization
  RegisterPackage('SdpoFastFormLaz', @Register); 
end.
