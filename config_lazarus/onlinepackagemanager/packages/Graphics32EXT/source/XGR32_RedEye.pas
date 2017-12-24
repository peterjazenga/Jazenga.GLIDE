
{**********************************************************************
 Package pl_Graphics32EXT.pkg
 this unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit XGR32_RedEye;

interface
 uses
  Classes, SysUtils,Graphics,Math, GR32;

procedure Effect_RemoveRedEye(src: TBitmap32);

implementation
 type
  TARGB = record // color splitting type
    b: Byte;
    g: Byte;
    r: Byte;
    a: Byte;
  end;
  PARGB = ^TARGB;

procedure Effect_RemoveRedEye(src: TBitmap32);
var
  x, y, w, h: Integer;
  pixptr: PARGB;
  nrv, bluf, redq, powr, powb, powg: Single;
begin
  if src=nil then exit;
  
  w := src.Width;
  h := src.Height;
  for y := 0 to h - 1 do begin
    for x := 0 to w - 1 do begin
      pixptr := PARGB ( src.PixelPtr [ x, y ] );
      nrv := pixptr^.g + pixptr^.b;
      if nrv < 1 then nrv := 1;
      if pixptr^.g > 1 then bluf := pixptr^.b / pixptr^.g
      else bluf := pixptr^.b;
      bluf := Max ( 0.5, Min ( 1.5, Sqrt ( bluf ) ) );
      redq := ( pixptr^.r / nrv ) * bluf;
      if redq > 0.7 then begin
        powr := 1.775 - ( redq * 0.75 + 0.25 );
        if powr < 0 then powr := 0;
        powr := powr * powr;
        powb := 1 - ( 1 - powr ) / 2;
        powg := 1 - ( 1 - powr ) / 4;
        pixptr^.r := Round ( powr * pixptr^.r );
        pixptr^.b := Round ( powb * pixptr^.b );
        pixptr^.g := Round ( powg * pixptr^.g );
      end;
    end;
  end;
  
 src.Changed;
end;

end.
