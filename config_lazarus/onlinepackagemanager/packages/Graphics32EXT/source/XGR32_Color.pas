
{**********************************************************************
 Package pl_Graphics32EXT.pkg
 this unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit XGR32_Color;

{$mode objfpc}{$H+}

interface

uses
  LCLIntf, LCLType, Types, Graphics,
  GR32;

const
    XClearColor      =$00FFFFFF;
    clOrange32       = clRed32 + $00A000;

    // Some semi-transparent color constants
    clSmBlack32      = clBlack32     and $7FFFFFFF;
    clSmDimGray32    = clDimGray32   and $7FFFFFFF;
    clSmGray32       = clGray32      and $7FFFFFFF;
    clSmLightGray32  = clLightGray32 and $7FFFFFFF;
    clSmWhite32      = clWhite32     and $7FFFFFFF;
    clSmMaroon32     = clMaroon32    and $7FFFFFFF;
    clSmGreen32      = clGreen32     and $7FFFFFFF;
    clSmOlive32      = clOlive32     and $7FFFFFFF;
    clSmNavy32       = clNavy32      and $7FFFFFFF;
    clSmPurple32     = clPurple32    and $7FFFFFFF;
    clSmTeal32       = clTeal32      and $7FFFFFFF;
    clSmRed32        = clRed32       and $7FFFFFFF;
    clSmLime32       = clLime32      and $7FFFFFFF;
    clSmYellow32     = clYellow32    and $7FFFFFFF;
    clSmBlue32       = clBlue32      and $7FFFFFFF;
    clSmFuchsia32    = clFuchsia32   and $7FFFFFFF;
    clSmAqua32       = clAqua32      and $7FFFFFFF;
    clSmOrange32     = clOrange32    and $7FFFFFFF;

    // Some transparent color constants
    clTrBlack32      = clBlack32     and $00FFFFFF;
    clTrDimGray32    = clDimGray32   and $00FFFFFF;
    clTrGray32       = clGray32      and $00FFFFFF;
    clTrLightGray32  = clLightGray32 and $00FFFFFF;
    clTrWhite32      = clWhite32     and $00FFFFFF;
    clTrMaroon32     = clMaroon32    and $00FFFFFF;
    clTrGreen32      = clGreen32     and $00FFFFFF;
    clTrOlive32      = clOlive32     and $00FFFFFF;
    clTrNavy32       = clNavy32      and $00FFFFFF;
    clTrPurple32     = clPurple32    and $00FFFFFF;
    clTrTeal32       = clTeal32      and $00FFFFFF;
    clTrRed32        = clRed32       and $00FFFFFF;
    clTrLime32       = clLime32      and $00FFFFFF;
    clTrYellow32     = clYellow32    and $00FFFFFF;
    clTrBlue32       = clBlue32      and $00FFFFFF;
    clTrFuchsia32    = clFuchsia32   and $00FFFFFF;
    clTrAqua32       = clAqua32      and $00FFFFFF;
    clTrOrange32     = clOrange32    and $00FFFFFF;

type
  TColorBGRA = record // color splitting type
    b: Byte;
    g: Byte;
    r: Byte;
    a: Byte;
  end;
  PColorBGRA = ^TColorBGRA;

function RandomColor32: TColor32;
function RandomColorLow32(ALow: byte): TColor32;
function RandomColorHigh32(AHigh: byte): TColor32;
function RandomColor: TColor;
function Color32FromWinColor(WinColor: TColor; Alfa: byte): TColor32;
function Color32RgbToGray(ARGB : TColor32) : TColor32;
function Color32AddKeepAlpha(C1, C2: TColor32): TColor32;
function Color32_Similar(Const Color0,Color1:TColor32;Tolerance: integer):boolean;
function Color32_SimilarRGB(Const Color0,Color1:TColor32):boolean;

implementation

function RandomColor32: TColor32;
var i : Integer;
begin
  with TColor32Entry(Result) do
   for I := Low(Planes) to High(Planes) do
    Planes[I] := Random(255);
end;

function RandomColor: TColor;
var i : Integer;
begin
  with TColor32Entry(Result) do
   for I := Low(Planes) to High(Planes)-1 do
    Planes[I] := Random(255);
end;

function RandomColorLow32(ALow: byte): TColor32;
var i : Integer;
begin
  with TColor32Entry(Result) do
   for I := Low(Planes) to High(Planes) do
    Planes[I] := ALow - Random(ALow);
end;

function RandomColorHigh32(AHigh: byte): TColor32;
var i : Integer;
begin
  with TColor32Entry(Result) do
   for I := Low(Planes) to High(Planes) do
    Planes[I] := Random(255 - AHigh) + AHigh;
end;

function Color32FromWinColor(WinColor: TColor; Alfa: byte): TColor32;
begin
 Result := (Color32(WinColor)and $00ffffff)or Alfa shl 24;
end;

function Color32RgbToGray(ARGB : TColor32) : TColor32;
var
  Alfa : byte;
  Gray : byte;
begin
  Alfa := TColor32Entry(ARGB).A;

  Gray := Intensity(ARGB);
  Result := Gray32(Gray, Alfa);
end;

function Color32AddKeepAlpha(C1, C2: TColor32): TColor32;
var
  r, g, b: Integer;
begin
  r := C1 and $00FF00FF + C2 and $00FF00FF;
  g := C1 and $0000FF00 + C2 and $0000FF00;
  b := r and $000001FF;
  r := r and $01FF0000;
  if r > $FF0000 then r := $FF0000;
  if g > $00FF00 then g := $00FF00;
  if b > $0000FF then b := $0000FF;
  result := Integer(C1 and $FF000000) or r or g or b;
end;


function Color32_Similar(Const Color0,Color1:TColor32;Tolerance: integer):boolean;
var r0,g0,b0,a0:byte;
    r1,g1,b1,a1:byte;
begin
result:=false;

if Tolerance<=0 then
 begin
  if Color0=Color1 then result:=true;
  exit;
 end;

R0:=(Color0 and $00FF0000) shr 16;
G0:=(Color0 and $0000FF00) shr 8;
B0:=Color0 and $000000FF;
A0:=Color0 shr 24;

R1:=(Color1 and $00FF0000) shr 16;
G1:=(Color1 and $0000FF00) shr 8;
B1:=Color1 and $000000FF;
A1:=Color1 shr 24;

 if (R1>=R0-tolerance) and (R1<=r0+tolerance) and
    (G1>=G0-tolerance) and (G1<=G0+tolerance) and
    (B1>=B0-tolerance) and (B1<=B0+tolerance) and
    (A1>=A0-tolerance) and (A1<=A0+tolerance) then
     result:=true;
end;

function Color32_SimilarRGB(Const Color0,Color1:TColor32):boolean;
var r0,g0,b0:byte;
    r1,g1,b1:byte;
begin
result:=false;

R0:=(Color0 and $00FF0000) shr 16;
G0:=(Color0 and $0000FF00) shr 8;
B0:=Color0 and $000000FF;


R1:=(Color1 and $00FF0000) shr 16;
G1:=(Color1 and $0000FF00) shr 8;
B1:=Color1 and $000000FF;

 if (R1=R0) and
    (G1=G0) and
    (B1=B0) then
     result:=true;
end;




end.

