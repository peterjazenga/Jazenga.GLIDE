{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit longtimerpackage;

interface

uses
  uLongTimer, AboutLongTimerunit, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('uLongTimer', @uLongTimer.Register);
  RegisterUnit('AboutLongTimerunit', @AboutLongTimerunit.Register);
end;

initialization
  RegisterPackage('longtimerpackage', @Register);
end.
