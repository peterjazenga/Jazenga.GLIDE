{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit callight_pkg;

{$warn 5023 off : no warning about unused units}
interface

uses
  CalendarLite, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('CalendarLite', @CalendarLite.Register);
end;

initialization
  RegisterPackage('callight_pkg', @Register);
end.
