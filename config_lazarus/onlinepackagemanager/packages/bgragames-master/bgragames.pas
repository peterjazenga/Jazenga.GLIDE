{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit bgragames;

interface

uses
  bgFilters, bgPanel, bgTileMap, bgTileMapGL, bgTools, SdpoJoystick, uos, 
  uos_flat, uos_libsndfile, uos_mpg123, uos_portaudio, uos_soundtouch, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('bgPanel', @bgPanel.Register);
  RegisterUnit('SdpoJoystick', @SdpoJoystick.Register);
end;

initialization
  RegisterPackage('bgragames', @Register);
end.
