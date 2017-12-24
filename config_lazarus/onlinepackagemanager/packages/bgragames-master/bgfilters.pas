unit bgFilters;

{$mode objfpc}{$H+}

interface

uses
  BGRABitmap, BGRABitmapTypes;

procedure BlackAndWhite(Bitmap: TBGRABitmap; middle: byte = 128);
procedure GameBoy(P: PBGRAPixel; Count: integer);
procedure GameBoy(Bitmap: TBGRABitmap);
function GameBoy(c: TBGRAPixel): TBGRAPixel;
procedure GrayScale(Bitmap: TBGRABitmap);

implementation

procedure BlackAndWhite(Bitmap: TBGRABitmap; middle: byte = 128);
var
  i: NativeInt;
  p: PBGRAPixel;
  c: byte;
begin
  p := Bitmap.Data;

  for i := Bitmap.NBPixels - 1 downto 0 do
  begin
    c := (p^.red + p^.green + p^.blue) div 3;
    if c >= middle then
      c := 255
    else
      c := 0;
    p^.red := c;
    p^.green := c;
    p^.blue := c;
    {if p^.alpha > 0 then
      p^.alpha := 255;}
    Inc(p);
  end;
end;

//filter to be applied on any sequence of pixels in memory
procedure GameBoy(P: PBGRAPixel; Count: integer);
var
  c: NativeInt;
begin
  while Count > 0 do
  begin
    c := p^.red + p^.green + p^.blue;

    if c <= 382 then
    begin
      if c <= 191 then
        p^ := BGRA(0, 80, 32, p^.alpha)
      else
        p^ := BGRA(0, 104, 24, p^.alpha);
    end
    else
    begin
      if c <= 573 then
        p^ := BGRA(0, 176, 0, p^.alpha)
      else
        p^ := BGRA(112, 224, 48, p^.alpha);
    end;

    Inc(p);
    Dec(Count);
  end;
end;

//filter to be applied on a bitmap, uses the generic function
procedure GameBoy(Bitmap: TBGRABitmap);
begin
  GameBoy(Bitmap.Data, Bitmap.NbPixels);
end;

//filter on one TBGRAPixel, transmit the address of the variable to the generic function
function GameBoy(c: TBGRAPixel): TBGRAPixel;
begin
  Result := c;
  GameBoy(@Result, 1);
end;

procedure GrayScale(Bitmap: TBGRABitmap);
begin
  Bitmap.InplaceGrayscale;
end;

end.

