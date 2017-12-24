
{**********************************************************************
 Package pl_Graphics32EXT.pkg
 this unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit XGR32_Blending;


interface

uses
  sysutils,graphics,GR32;

procedure Arithmetic_MergeBitmaps(bmp1,bmp2: TBitmap32; operation:string);
function BM_Additive(F: TColor32; var B: TColor32; M: TColor32):Tcolor32;
function BM_Subtract(F: TColor32; var B: TColor32; M: TColor32):Tcolor32;
function BM_Multiply(F: TColor32; var B: TColor32; M: TColor32):Tcolor32;
function BM_Divide(F: TColor32; var B: TColor32; M: TColor32):Tcolor32;
function BM_Lightest(F: TColor32; var B: TColor32; M: TColor32):Tcolor32;
function BM_Darkest(F: TColor32; var B: TColor32; M: TColor32):Tcolor32;
function BM_Average(F: TColor32; var B: TColor32; M: TColor32):Tcolor32;
function BM_Difference(F: TColor32; var B: TColor32; M: TColor32):Tcolor32;

implementation

procedure Arithmetic_MergeBitmaps(bmp1,bmp2: TBitmap32; operation:string);
var
  I, J: Integer;
  Combine: TPixelCombineEvent;
 // CustomCombine: Boolean;
  P1, P2: PColor32array;
  SS:Pcolor32;
  f,b:tcolor32;

begin
  SS := @bmp2.Bits[0];

  with bmp1 do
  begin
    Combine := OnPixelCombine;
  //  CustomCombine := (DrawMode = dmCustom) and Assigned(Combine);

  end;
  with bmp2 do
  begin
    MasterAlpha := $ff;
    DrawMode := dmBlend;
  end;
  for I := 0 to bmp1.height-1 do
  begin
    p2:=bmp2.scanline[I];
    p1:=bmp1.scanline[I];
    for J := 0 to bmp1.Width-1 do
    begin
       F:=p2^[j];
       B:=p1^[j];

  if uppercase(operation)=uppercase('additive') then ss^:= BM_additive(F,B,0);
  if uppercase(operation)=uppercase('subtract') then ss^:= BM_subtract(F,B,0);
  if uppercase(operation)=uppercase('multiply') then ss^:= BM_multiply(F,B,0);
  if uppercase(operation)=uppercase('divide') then ss^:= BM_divide(F,B,0);
  if uppercase(operation)=uppercase('darkest') then ss^:= BM_darkest(F,B,0);
  if uppercase(operation)=uppercase('lightest') then ss^:= BM_lightest(F,B,0);
  if uppercase(operation)=uppercase('average') then ss^:= BM_average(F,B,0);
  if uppercase(operation)=uppercase('Difference') then ss^:= BM_difference(F,B,0);

  //$FF000000 + fr shl 16 + fg shl 8 + fb;
  inc(ss);
    end;
  end;
  bmp2.changed;
end;

function BM_Additive(F: TColor32; var B: TColor32; M: TColor32):Tcolor32;
var    fR,fG,fB, bR,bG,bB: cardinal;
begin
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;

  fR := fR + bR;
  fG := fG + bG;
  fB := fB + bB;
  if fR > 255 then fR:= 255;
  if fG > 255 then fG:= 255;
  if fB > 255 then fB:= 255;
  Result:=$FF000000 + fr shl 16 + fg shl 8 + fb;
end;

{subtract}
function BM_Subtract(F: TColor32; var B: TColor32; M: TColor32):Tcolor32;
var fR,fG,fB, bR,bG,bB: cardinal;
begin
  {Channel separation}
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  {Combine}
  fR := bR + fR - 256;
  fG := bG + fG - 256;
  fB := bB + fB - 256;
  if fR > 255 then fR:= 0; // > 255 only possible if <0
  if fG > 255 then fG:= 0;
  if fB > 255 then fB:= 0;
  Result:=$FF000000 + fr shl 16 + fg shl 8 + fb;
end;

{Multiply}
function BM_Multiply(F: TColor32; var B: TColor32; M: TColor32):Tcolor32;
var fR,fG,fB, bR,bG,bB: cardinal;
begin
  {Channel separation}
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  {Combine}
  fR := bR * fR shr 8;
  fG := bG * fG shr 8;
  fB := bB * fB shr 8;
  Result:=$FF000000 + fr shl 16 + fg shl 8 + fb;
end;

{Divide}
function BM_Divide(F: TColor32; var B: TColor32; M: TColor32):Tcolor32;
var fR,fG,fB, bR,bG,bB: cardinal;
begin
  {Channel separation}
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  {Combine}
  if fr>0 then fR := round(bR / fR) shr 8 else fr:=0;
  if fg>0 then fG := round(bG / fG) shr 8 else fg:=0;
  if fb>0 then fB := round(bB / fB) shr 8 else fb:=0;
  Result:=$FF000000 + fr shl 16 + fg shl 8 + fb;
end;

function BM_Average(F: TColor32; var B: TColor32; M: TColor32):Tcolor32;
var fR,fG,fB, bR,bG,bB: cardinal;
begin
  {Channel separation}
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  {Combine}
  fR:= fR + bR;
  fR:= fR shr 1;
  fG:= fg + bG;
  fG:= fG shr 1;
  fB:= fB + bB;
  fB:= fB shr 1;
  Result:=$FF000000 + fr shl 16 + fg shl 8 + fb;
end;

{Lighten}
function BM_Lightest(F: TColor32; var B: TColor32; M: TColor32):Tcolor32;
var fR,fG,fB, bR,bG,bB: cardinal;
begin
  {Channel separation}
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  {Combine}
  if fR < bR then fR:= bR;
  if fG < bG then fG:= bG;
  if fB < bB then fB:= bB;
  Result:=$FF000000 + fr shl 16 + fg shl 8 + fb;
end;

{Darken}
function BM_Darkest(F: TColor32; var B: TColor32; M: TColor32):Tcolor32;
var fR,fG,fB, bR,bG,bB: cardinal;
begin
  {Channel separation}
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  {Combine}
  if fR > bR then fR:= bR;
  if fG > bG then fG:= bG;
  if fB > bB then fB:= bB;
  Result:=$FF000000 + fr shl 16 + fg shl 8 + fb;
end;

function BM_Difference(F: TColor32; var B: TColor32; M: TColor32):Tcolor32;
var fR,fG,fB, bR,bG,bB: cardinal;
begin
  {Channel separation}
  fR := F shr 16 and $FF;
  fG := F shr 8  and $FF;
  fB := F        and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;
  {Combine}
  fR := abs(bR - fR);
  fG := abs(bG - fG);
  fB := abs(bB - fB);

  Result:=$FF000000 + fr shl 16 + fg shl 8 + fb;
end;





end.
