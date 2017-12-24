
{**********************************************************************
 Package pl_ExControls.pkg
 From PilotLogic Software House(http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit plUtilsForHSL;

interface

{$I AllExControlsRegister.inc}

uses
  //Windows,
  SysUtils, Graphics;

var
  HSLRange: integer = 240;

// convert a HSL value into a RGB in a TColor
// HSL values are 0.0 to 1.0 double
function HSLtoRGB(H, S, L: double): TColor;

// convert a HSL value into a RGB in a TColor
// SL values are 0 to the HSLRange variable
// H value is to HSLRange-1
function HSLRangeToRGB(H, S, L: integer): TColor;

// convert a RGB value (as TColor) into HSL
// HSL values are 0.0 to 1.0 double
procedure RGBtoHSL(RGB: TColor; var H, S, L: double);

// convert a RGB value (as TColor) into HSL
// SL values are 0 to the HSLRange variable
// H value is to HSLRange-1
procedure RGBtoHSLRange(RGB: TColor; var H, S, L: integer);

implementation

function HSLtoRGB(H, S, L: double): TColor;
var
  M1, M2: double;

  function HueToColourValue(Hue: double): byte;
  var
    V: double;
  begin
    if Hue < 0 then
      Hue := Hue + 1
    else
    if Hue > 1 then
      Hue := Hue - 1;

    if 6 * Hue < 1 then
      V := M1 + (M2 - M1) * Hue * 6
    else
    if 2 * Hue < 1 then
      V := M2
    else
    if 3 * Hue < 2 then
      V := M1 + (M2 - M1) * (2 / 3 - Hue) * 6
    else
      V := M1;
    Result := round(255 * V);
  end;

var
  R, G, B: byte;
begin
  if S = 0 then
  begin
    R := round(255 * L);
    G := R;
    B := R;
  end
  else
  begin
    if L <= 0.5 then
      M2 := L * (1 + S)
    else
      M2 := L + S - L * S;
    M1 := 2 * L - M2;
    R := HueToColourValue(H + 1 / 3);
    G := HueToColourValue(H);
    B := HueToColourValue(H - 1 / 3);
  end;

  Result := RGBToColor(R, G, B);
end;

function HSLRangeToRGB(H, S, L: integer): TColor;
begin
  Result := HSLToRGB(H / (HSLRange - 1), S / HSLRange, L / HSLRange);
end;

// Convert RGB value (0-255 range) into HSL value (0-1 values)

procedure RGBtoHSL(RGB: TColor; var H, S, L: double);

  function Max(a, b: double): double;
  begin
    if a > b then
      Result := a
    else
      Result := b;
  end;

  function Min(a, b: double): double;
  begin
    if a < b then
      Result := a
    else
      Result := b;
  end;

var
  R, G, B, D, Cmax, Cmin: double;
begin
  R := Red(RGB) / 255;
  G := Green(RGB) / 255;
  B := Blue(RGB) / 255;
  Cmax := Max(R, Max(G, B));
  Cmin := Min(R, Min(G, B));

  // calculate luminosity
  L := (Cmax + Cmin) / 2;

  if Cmax = Cmin then  // it's grey
  begin
    H := 0; // it's actually undefined
    S := 0;
  end
  else
  begin
    D := Cmax - Cmin;

    // calculate Saturation
    if L < 0.5 then
      S := D / (Cmax + Cmin)
    else
      S := D / (2 - Cmax - Cmin);

    // calculate Hue
    if R = Cmax then
      H := (G - B) / D
    else
    if G = Cmax then
      H := 2 + (B - R) / D
    else
      H := 4 + (R - G) / D;

    H := H / 6;
    if H < 0 then
      H := H + 1;
  end;
end;

procedure RGBtoHSLRange(RGB: TColor; var H, S, L: integer);
var
  Hd, Sd, Ld: double;
begin
  RGBtoHSL(RGB, Hd, Sd, Ld);
  H := round(Hd * (HSLRange - 1));
  S := round(Sd * HSLRange);
  L := round(Ld * HSLRange);
end;

end.
