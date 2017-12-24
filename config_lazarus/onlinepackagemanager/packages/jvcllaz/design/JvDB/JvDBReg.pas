unit JvDBReg;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

procedure Register;

implementation

uses
  Classes, JvDsgnConsts, JvDBHTLabel;

procedure Register;
begin
  RegisterComponents(RsPaletteDBVisual, [TJvDBHTLabel]);
end;

end.

