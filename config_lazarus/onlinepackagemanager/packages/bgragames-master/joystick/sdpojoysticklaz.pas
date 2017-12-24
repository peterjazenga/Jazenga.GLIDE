{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit SdpoJoystickLaz; 

interface

uses
  SdpoJoystick, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('SdpoJoystick', @SdpoJoystick.Register); 
end; 

initialization
  RegisterPackage('SdpoJoystickLaz', @Register); 
end.
