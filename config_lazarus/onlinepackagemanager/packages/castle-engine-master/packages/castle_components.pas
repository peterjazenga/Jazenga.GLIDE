{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit castle_components;

{$warn 5023 off : no warning about unused units}
interface

uses
  CastleControl, CastleDialogs, CastleLCLRecentFiles, CastleLCLUtils, 
  CastlePropEdits, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('CastleControl', @CastleControl.Register);
  RegisterUnit('CastleDialogs', @CastleDialogs.Register);
  RegisterUnit('CastleLCLRecentFiles', @CastleLCLRecentFiles.Register);
  RegisterUnit('CastlePropEdits', @CastlePropEdits.Register);
end;

initialization
  RegisterPackage('castle_components', @Register);
end.
