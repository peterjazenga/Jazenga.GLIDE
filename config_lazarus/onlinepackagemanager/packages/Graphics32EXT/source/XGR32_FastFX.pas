
{**********************************************************************
 Package pl_Graphics32EXT.pkg
 this unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit XGR32_FastFX;

{$MODE Delphi}

interface
{$R-}
uses
  LCLIntf, LCLType,
   math, Types, GR32;

type
  PFColor = ^TFColor;
  TFColor = packed record
    b, g, r: Byte;
  end;

  PFColorA = ^TFColorA;
  TFColorA = packed record
    case Integer of
      0: (I: DWord);
      1: (C: TFColor);
      2: (Hi, Lo: Word);
      3: (b, g, r, A: Byte);
  end;
  TLine32 = array[Word] of TFColorA;
  PLine32 = ^TLine32;
  TPixels32 = array[Word] of PLine32;
  PPixels32 = ^TPixels32;

  TLut = array[Byte] of Byte;
  PLut = ^TLut;
  TWLut = array[Word] of Word;
  PWLut = ^TWLut;
  TSLut = array[Word] of Integer;
  PSLut = ^TSLut;

  PSaturationLut = ^TSaturationLut;
  TSaturationLut = record
    Grays: array[0..767] of Integer;
    Alpha: array[Byte] of Word;
  end;

  PNormal = ^TNormal;
  TNormal = record
    X, Y: Shortint;
  end;

procedure FillLut(var Lut: TLut; x1, y1, x2, y2: Byte);
function  ContrastLut(Amount: Integer): TLut;
function  LightnessLut(Amount: Integer): TLut;
function  AdditionLut(Amount: Integer): TLut;
function  GammaLut(Amount: Extended): TLut;
function  MergeLuts(Luts: array of TLut): TLut;

procedure ApplyLutB(Bmp: TBitmap32; Lut: TLut);
procedure ApplyLut(Bmp: TBitmap32; RLut, GLut, BLut: TLut);
procedure AutoContrastB(Bmp: TBitmap32);
procedure AutoContrast(Bmp: TBitmap32);
procedure Addition(Bmp: TBitmap32; r, g, b: Integer); //ApplyLut
procedure Gamma(Bmp: TBitmap32; r, g, b: Extended); //ApplyLut
procedure Contrast(Bmp: TBitmap32; r, g, b: Integer); //ApplyLut
procedure Lightness(Bmp: TBitmap32; r, g, b: Integer); //ApplyLut
function  SaturationLut(Amount: Integer): TSaturationLut;

procedure ApplySaturationLut(Bmp: TBitmap32; Lut: TSaturationLut);
procedure ApplySaturationLutEx(Bmp: TBitmap32; Lut: TSaturationLut; RLut, GLut, BLut: TLut);
procedure Saturation(Bmp: TBitmap32; Amount: Integer);

procedure AddNoise(Bmp: TBitmap32; Amount: Byte; Mono: Boolean);
procedure Grayscale(Src, Dst: TBitmap32);

procedure FastTile(Src, Dst: TBitmap32);
procedure BumpImage(Dst, Bump, Light: TBitmap32; X, Y: Integer);
procedure RenderLightmap(Bmp: TBitmap32; Size: Integer);

procedure SplitBlur(Bmp: TBitmap32; Split: Integer);
procedure GaussianBlur(Bmp: TBitmap32; Amount: Integer);
procedure SplitConvolve(Bmp: TBitmap32; Split, nw, ne, sw, se, m: Integer);
procedure BlurEdges(Bmp: TBitmap32; Split: Integer); //SplitConvolve
procedure BleedEdges(Bmp: TBitmap32; Split: Integer); //SplitConvolve
procedure Sharpen(Bmp: TBitmap32; Split: Integer); //SplitConvolve
procedure SharpenMore(Bmp: TBitmap32; Split: Integer); //SplitConvolve
procedure EmbossEdges(Bmp: TBitmap32; Split: Integer); //SplitConvolve

procedure QuickSoft(Bmp: TBitmap32);
procedure QuickSharp(Bmp: TBitmap32);
procedure QuickEmboss(Bmp: TBitmap32);
procedure Lines(Bmp: TBitmap32; Lut: TLut);
procedure Mosaic(Bmp: TBitmap32; xAmount, yAmount: Integer); //bpp > 4

procedure RotateSize(Src, Dst: TBitmap32; Angle: Double);

procedure Transform(Src, Dst: TBitmap32; cx, cy, isin, icos: Integer; Smooth: Boolean);
procedure Rotate(Src, Dst: TBitmap32; Angle: Double; Smooth: Boolean);
procedure Rotozoom(Src, Dst: TBitmap32; Angle: Double; Zoom: Integer; Smooth: Boolean);

function Scale8(I, n: Integer): Integer;

implementation

function Scale8(I, n: Integer): Integer;
begin                                   // Result:=(i*255)div([1 shl n]-1);
  case n of
    1: if Boolean(I) then
        Result := 255
      else
        Result := 0;
    2: Result := (I shl 6) or (I shl 4) or (I shl 2) or I;
    3: Result := (I shl 5) or (I shl 2) or (I shr 1);
    4: Result := (I shl 4) or I;
    5: Result := (I shl 3) or (I shr 2);
    6: Result := (I shl 2) or (I shr 4);
    7: Result := (I shl 1) or (I shr 6);
  else
    Result := I;
  end;
end;

procedure FillLut(var Lut: TLut; x1, y1, x2, y2: Byte);
var
  X, n, I, ii: Integer;
begin
  n := x2 - x1;
  if n <> 0 then
  begin
    I := y1 shl 16;
    ii := ((y2 - y1 + 1) shl 16) div n;
    for X := x1 to x2 do
    begin
      Lut[X] := I shr 16;
      inc(I, ii);
    end;
  end;
end;

function ContrastLut(Amount: Integer): TLut;
begin
  if Amount < 0 then
    FillLut(Result, 0, -Amount, 255, 255 + Amount)
  else
  begin
    if Amount > 255 then
      Amount := 255;
    FillChar(Result, Amount, 0);
    FillLut(Result, Amount, 0, 255 - Amount, 255);
    FillChar(Result[256 - Amount], Amount, 255);
  end;
end;

function LightnessLut(Amount: Integer): TLut;
begin
  if Amount < 0 then
    FillLut(Result, 0, 0, 255, 255 + Amount)
  else
  begin
    if Amount > 255 then
      Amount := 255;
    FillLut(Result, 0, Amount, 255, 255);
  end;
end;

function AdditionLut(Amount: Integer): TLut;
var
  I, X: Integer;
begin
  if Amount < 0 then
  begin
    if Amount < -255 then
      Amount := -255;
    FillChar(Result, -Amount, 0);
    X := 0;
    for I := -Amount to 255 do
    begin
      Result[I] := X;
      inc(X);
    end;
  end
  else if Amount > 0 then
  begin
    if Amount > 255 then
      Amount := 255;
    X := Amount;
    for I := 0 to 255 - Amount do
    begin
      Result[I] := X;
      inc(X);
    end;
    FillChar(Result[256 - Amount], Amount, 255);
  end
  else
    for I := 0 to 255 do
      Result[I] := I;
end;

function GammaLut(Amount: Extended): TLut;
var
  I, z: Integer;
  Y: Extended;
begin
  if Amount > 0 then
  begin
    Result[0] := 0;
    Y := 1 / Amount;
    for I := 1 to 255 do
    begin
      z := Round(255 * Exp(Y * Ln(I / 255)));
      if z > 255 then
        z := 255
      else if z < 0 then
        z := 0;
      Result[I] := z;
    end;
  end;
end;

function MergeLuts(Luts: array of TLut): TLut;
var
  X, Y, z: Integer;
begin
  X := Low(Luts);
  for Y := 0 to 255 do
    Result[Y] := Luts[X, Y];
  for z := X + 1 to High(Luts) do
    for Y := 0 to 255 do
      Result[Y] := Luts[z, Result[Y]];
end;

function MergeWLuts(Luts: array of TWLut): TWLut;
var
  X, Y, z: Integer;
begin
  X := Low(Luts);
  for Y := 0 to 65535 do
    Result[Y] := Luts[X, Y];
  for z := X + 1 to High(Luts) do
    for Y := 0 to 65535 do
      Result[Y] := Luts[z, Result[Y]];
end;

procedure ApplyLutB(Bmp: TBitmap32; Lut: TLut);
var
  pc: PFColorA;
  I: Integer;
begin
  pc := Pointer(Bmp.Bits);
  for I := 0 to Bmp.Height * Bmp.Width - 1 do
  begin
      pc.b := Lut[pc.b];
      pc.g := Lut[pc.g];
      pc.r := Lut[pc.r];
      inc(pc);
  end;
end;

procedure ApplyLut32(Bmp: TBitmap32; RLut, GLut, BLut: TLut);
var
  pc: PFColorA;
  I: Integer;
begin
  pc := Pointer(Bmp.Bits);
    for I := 0 to Bmp.Width * Bmp.Height - 1 do
    begin
      pc.b := BLut[pc.b];
      pc.g := GLut[pc.g];
      pc.r := RLut[pc.r];
      inc(pc);
    end;
end;

procedure ApplyLut(Bmp: TBitmap32; RLut, GLut, BLut: TLut);
begin
  ApplyLut32(Bmp, RLut, GLut, BLut);
end;

procedure AutoContrastB(Bmp: TBitmap32);
var
  Lut: TLut;
  pc: PFColorA;
  Hi, Lo: Byte;
  I: Integer;
begin
  Hi := 0;
  Lo := 255;
  pc := Pointer(Bmp.Bits);
  for I := 0 to Bmp.Height * Bmp.Width - 1 do
  begin
      if pc.b > Hi then
        Hi := pc.b;
      if pc.b < Lo then
        Lo := pc.b;

      if pc.g > Hi then
        Hi := pc.g;
      if pc.g < Lo then
        Lo := pc.g;

      if pc.r > Hi then
        Hi := pc.r;
      if pc.r < Lo then
        Lo := pc.r;

      inc(pc);
    //inc(pb, Bmp.Gap);
  end;
  if (Lo <> 0) or (Hi <> 255) then
  begin
    FillLut(Lut, Lo, 0, Hi, 255);
    ApplyLutB(Bmp, Lut);
  end;
end;

procedure AutoContrast32(Bmp: TBitmap32);
var
  hr, hg, hb, lr, lg, lb: Byte;
  r, g, b: TLut;
  pc: PFColorA;
  I: Integer;
begin
  hr := 0;
  hg := 0;
  hb := 0;
  lr := 255;
  lg := 255;
  lb := 255;
  pc := Pointer(Bmp.Bits);
  for I := 0 to Bmp.Height * Bmp.Width - 1 do
  begin
      if pc.b > hb then
        hb := pc.b;
      if pc.b < lb then
        lb := pc.b;
      if pc.g > hg then
        hg := pc.g;
      if pc.g < lg then
        lg := pc.g;
      if pc.r > hr then
        hr := pc.r;
      if pc.r < lr then
        lr := pc.r;
      inc(pc);
  end;
  if ((lr or lg or lb) <> 0) or ((hr and hg and hb) <> 255) then
  begin
    FillLut(r, lr, 0, hr, 255);
    FillLut(g, lg, 0, hg, 255);
    FillLut(b, lb, 0, hb, 255);
    ApplyLut32(Bmp, r, g, b);
  end;
end;

procedure AutoContrast(Bmp: TBitmap32);
begin
  AutoContrast32(Bmp);
end;

procedure Addition(Bmp: TBitmap32; r, g, b: Integer);
begin
  ApplyLut(Bmp,
    AdditionLut(r),
    AdditionLut(g),
    AdditionLut(b));
end;

procedure Gamma(Bmp: TBitmap32; r, g, b: Extended);
begin
  ApplyLut(Bmp,
    GammaLut(r),
    GammaLut(g),
    GammaLut(b));
end;

procedure Contrast(Bmp: TBitmap32; r, g, b: Integer);
begin
  ApplyLut(Bmp,
    ContrastLut(r),
    ContrastLut(g),
    ContrastLut(b));
end;

procedure Lightness(Bmp: TBitmap32; r, g, b: Integer);
begin
  ApplyLut(Bmp,
    LightnessLut(r),
    LightnessLut(g),
    LightnessLut(b));
end;

function SaturationLut(Amount: Integer): TSaturationLut;
var
  X, Y, I: Integer;
begin
  X := 0;
  for I := 1 to 256 do
    Result.Alpha[I - 1] := (I * Amount) shr 8;
  for I := 1 to 256 do
  begin
    Y := I - Result.Alpha[I - 1];
    Result.Grays[X] := Y;
    inc(X);
    Result.Grays[X] := Y;
    inc(X);
    Result.Grays[X] := Y;
    inc(X);
  end;
end;

procedure ApplySaturationLut32(Bmp: TBitmap32; Lut: TSaturationLut);
var
  g, X, Y, z: Integer;
  pc: PFColorA;
begin
  pc := Pointer(Bmp.Bits);
  for Y := 0 to Bmp.Height - 1 do
  begin
    for X := 0 to Bmp.Width - 1 do
    begin
      g := Lut.Grays[pc.b + pc.g + pc.r];
      z := Lut.Alpha[pc.b] + g;
      if z > 255 then
        z := 255
      else if z < 0 then
        z := 0;
      pc.b := z;
      z := Lut.Alpha[pc.g] + g;
      if z > 255 then
        z := 255
      else if z < 0 then
        z := 0;
      pc.g := z;
      z := Lut.Alpha[pc.r] + g;
      if z > 255 then
        z := 255
      else if z < 0 then
        z := 0;
      pc.r := z;
      inc(pc);
    end;
  end;
end;

procedure ApplySaturationLut(Bmp: TBitmap32; Lut: TSaturationLut);
begin
  ApplySaturationLut32(Bmp, Lut);
end;

procedure ApplySaturationLutEx32(Bmp: TBitmap32; Lut: TSaturationLut; RLut, GLut, BLut: TLut);
var
  g, I, z: Integer;
  pc: PFColorA;
begin
  pc := Pointer(Bmp.Bits);
  for I := 0 to Bmp.Height * Bmp.Width - 1 do
  begin
      g := Lut.Grays[pc.b + pc.g + pc.r];
      z := Lut.Alpha[pc.b] + g;
      if z > 255 then
        z := 255
      else if z < 0 then
        z := 0;
      pc.b := BLut[z];
      z := Lut.Alpha[pc.g] + g;
      if z > 255 then
        z := 255
      else if z < 0 then
        z := 0;
      pc.g := GLut[z];
      z := Lut.Alpha[pc.r] + g;
      if z > 255 then
        z := 255
      else if z < 0 then
        z := 0;
      pc.r := RLut[z];
      inc(pc);
  end;
end;

procedure ApplySaturationLutEx(Bmp: TBitmap32; Lut: TSaturationLut; RLut, GLut, BLut: TLut);
begin
    ApplySaturationLutEx32(Bmp, Lut, RLut, GLut, BLut);
end;

procedure Saturation(Bmp: TBitmap32; Amount: Integer);
begin
  ApplySaturationLut(Bmp, SaturationLut(Amount));
end;

procedure AddNoise32(Bmp: TBitmap32; Amount: Integer; Mono: Boolean);
var
  S, A, z, X, Y: Integer;
  pa: PFColorA;
begin
  S := Amount shr 1;
  pa := Pointer(Bmp.Bits);
  if Mono then
  begin
    for Y := 0 to Bmp.Height - 1 do
    begin
      for X := 0 to Bmp.Width - 1 do
      begin
        A := Random(Amount) - S;
        z := pa.b + A;
        if z > 255 then
          pa.b := 255
        else if z < 0 then
          pa.b := 0
        else
          pa.b := z;
        z := pa.g + A;
        if z > 255 then
          pa.g := 255
        else if z < 0 then
          pa.g := 0
        else
          pa.g := z;
        z := pa.r + A;
        if z > 255 then
          pa.r := 255
        else if z < 0 then
          pa.r := 0
        else
          pa.r := z;
        inc(pa);
      end;
      //pa := Ptr(Integer(pa) + Bmp.Gap);
    end;
  end
  else
  begin
    for Y := 0 to Bmp.Height - 1 do
    begin
      for X := 0 to Bmp.Width - 1 do
      begin
        z := pa.b + Random(Amount) - S;
        if z > 255 then
          pa.b := 255
        else if z < 0 then
          pa.b := 0
        else
          pa.b := z;
        z := pa.g + Random(Amount) - S;
        if z > 255 then
          pa.g := 255
        else if z < 0 then
          pa.g := 0
        else
          pa.g := z;
        z := pa.r + Random(Amount) - S;
        if z > 255 then
          pa.r := 255
        else if z < 0 then
          pa.r := 0
        else
          pa.r := z;
        inc(pa);
      end;
      //pa := Ptr(Integer(pa) + Bmp.Gap);
    end;
  end;
end;

procedure AddNoise(Bmp: TBitmap32; Amount: Byte; Mono: Boolean);
begin
    AddNoise32(Bmp, Amount, Mono);
end;

procedure Grayscale32(Src, Dst: TBitmap32);
var
  Div3: TLut;
  I: Integer;
  pb: PFColorA;
  pc: PFColorA;
begin
  FillLut(Div3, 0, 0, 255, 255 div 3);
  pb := Pointer(Dst.Bits);
  pc := Pointer(Src.Bits);

  for I := 0 to Dst.Width * Dst.Height - 1 do
  begin
      pb^.b := Div3[pc.b] + Div3[pc.g] + Div3[pc.r];
      pb^.g := pb^.b;
      pb^.r := pb^.b;
      inc(pb);
      inc(pc);
  end;
end;

procedure Grayscale(Src, Dst: TBitmap32);
begin
  Dst.SetSize(Src.Width, Src.Height);
  //Dst.FillColors(0, 255, [tfBlack, tfWhite]);
  Grayscale32(Src, Dst);
end;

procedure FastTile(Src, Dst: TBitmap32);
var
  wd, hd: Integer;
begin
  wd := Src.Width;
  hd := Src.Height;
  Src.DrawTo(Dst, 0, 0);
  while wd < Dst.Width do
  begin
    Dst.DrawTo(Dst, wd, 0, MakeRect(0, 0, wd shl 1, Src.Height));
    inc(wd, wd);
  end;
  while hd < Dst.Height do
  begin
    Dst.DrawTo(Dst, 0, hd, MakeRect(0, 0, Dst.Width, hd shl 1));
    inc(hd, hd);
  end;
end;

procedure BumpImage32(Dst, Bump, Light: TBitmap32; X, Y: Integer);
var
  xh, yh, x2, y2, y3, bx, by: Integer;
  bm: PNormal;
  pc: PFColorA;
begin
  xh := Light.Width shr 1;
  yh := Light.Height shr 1;

  for y2 := 0 to Dst.Height - 1 do
  begin
    bm := PNormal(Bump.Scanline[y2]);
    pc := PFColorA(Dst.Scanline[y2]);

    y3 := yh + y2 - Y;
    for x2 := 0 to Dst.Width - 1 do
    begin
      bx := bm.X + xh + x2 - X;
      by := bm.Y + y3;

      if (bx < Light.Width) and (bx > 0) and (by < Light.Height) and (by > 0) then
        pc^ := TFColorA(Light[by, bx])
      else
        pc.I := 0;
      inc(bm);
      inc(pc);
    end;
  end;
end;

procedure BumpImage(Dst, Bump, Light: TBitmap32; X, Y: Integer);
begin
  BumpImage32(Dst, Bump, Light, X, Y);
end;

procedure RenderLightmap32(Bmp: TBitmap32; Size: Integer);
var
  X, Y, yy, F, r, I: Integer;
  pc: PFColorA;
begin
  r := Size shr 1;
  F := Round(65536 / (Size / ((256 / (Size / 2)) * 2)));

  pc := Pointer(Bmp.Bits);
  for Y := 0 to Bmp.Height - 1 do
  begin
    yy := (Y - r) * (Y - r);
    for X := 0 to Bmp.Width - 1 do
    begin
      I := ((X - r) * (X - r) + yy) * F shr 16;
      if I > 255 then
        I := 255;
      I := I xor -1;
      pc.b := I;
      pc.g := I;
      pc.r := I;
      inc(pc);
    end;
  end;
end;

procedure RenderLightmap(Bmp: TBitmap32; Size: Integer);
begin
    RenderLightmap32(Bmp, Size);
end;

procedure SplitBlur32(Bmp: TBitmap32; Split: Integer);
var
  n, S, E, w, X, Y: Integer;
  Lin1, Lin2: PLine32;
  pc: PFColorA;
begin
  pc := Pointer(Bmp.Bits);
  for Y := 0 to Bmp.Height - 1 do
  begin
    n := Y + Split;
    if n > Bmp.Height - 1 then
      n := Bmp.Height - 1;
    S := Y - Split;
    if S < 0 then
      S := 0;
    Lin1 := PLine32(Bmp.Scanline[S]);
    Lin2 := PLine32(Bmp.Scanline[n]);
    for X := 0 to Bmp.Width - 1 do
    begin
      E := X + Split;
      if E > Bmp.Width - 1 then
        E := Bmp.Width - 1;
      w := X - Split;
      if w < 0 then
        w := 0;
      pc.b := (Lin1[w].b + Lin1[E].b + Lin2[w].b + Lin2[E].b) shr 2;
      pc.g := (Lin1[w].g + Lin1[E].g + Lin2[w].g + Lin2[E].g) shr 2;
      pc.r := (Lin1[w].r + Lin1[E].r + Lin2[w].r + Lin2[E].r) shr 2;
      inc(pc);
    end;
  end;
end;

procedure SplitBlur(Bmp: TBitmap32; Split: Integer);
begin
    SplitBlur32(Bmp, Split);
end;

procedure GaussianBlur(Bmp: TBitmap32; Amount: Integer);
var
  I: Integer;
begin
        for I := 1 to Amount do
          SplitBlur32(Bmp, I);
end;

procedure SplitConvolve32(Bmp: TBitmap32; Split, nw, ne, sw, se, m: Integer);
var
  Sum, n, S, E, w, I, X, Y: Integer;
  Lin1, Lin2: PLine32;
  pc: PFColorA;
begin
  Sum := nw + ne + sw + se + m;
  if Sum = 0 then
    Sum := 1;
  pc := Pointer(Bmp.Bits);
  for Y := 0 to Bmp.Height - 1 do
  begin
    n := Y + Split;
    if n > Bmp.Height - 1 then
      n := Bmp.Height - 1;
    S := Y - Split;
    if S < 0 then
      S := 0;
    Lin1 := PLine32(Bmp.Scanline[S]);
    Lin2 := PLine32(Bmp.Scanline[n]);
    for X := 0 to Bmp.Width - 1 do
    begin
      E := X + Split;
      if E > Bmp.Width - 1 then
        E := Bmp.Width - 1;
      w := X - Split;
      if w < 0 then
        w := 0;
      I := (pc.b * m + Lin1[w].b * nw + Lin1[E].b * ne + Lin2[w].b * sw + Lin2[E].b * se) div Sum;
      if I > 255 then
        pc.b := 255
      else if I < 0 then
        pc.b := 0
      else
        pc.b := I;
      I := (pc.g * m + Lin1[w].g * nw + Lin1[E].g * ne + Lin2[w].g * sw + Lin2[E].g * se) div Sum;
      if I > 255 then
        pc.g := 255
      else if I < 0 then
        pc.g := 0
      else
        pc.g := I;
      I := (pc.r * m + Lin1[w].r * nw + Lin1[E].r * ne + Lin2[w].r * sw + Lin2[E].r * se) div Sum;
      if I > 255 then
        pc.r := 255
      else if I < 0 then
        pc.r := 0
      else
        pc.r := I;
      inc(pc);
    end;
  end;
end;

procedure SplitConvolve(Bmp: TBitmap32; Split, nw, ne, sw, se, m: Integer);
begin
    SplitConvolve32(Bmp, Split, nw, ne, sw, se, m);
end;

procedure BlurEdges(Bmp: TBitmap32; Split: Integer);
begin
  SplitConvolve(Bmp, Split, 2, 2, 2, 2, -3);
end;

procedure BleedEdges(Bmp: TBitmap32; Split: Integer);
begin
  SplitConvolve(Bmp, Split, 2, 2, 2, 2, -4);
end;

procedure Sharpen(Bmp: TBitmap32; Split: Integer);
begin
  SplitConvolve(Bmp, Split, 1, 1, 1, 1, -10);
end;

procedure SharpenMore(Bmp: TBitmap32; Split: Integer);
begin
  SplitConvolve(Bmp, Split, 5, 5, 5, 5, -35);
end;

procedure EmbossEdges(Bmp: TBitmap32; Split: Integer);
begin
  SplitConvolve(Bmp, Split, 4, 4, -4, -4, 10);
end;

procedure QuickSoft32(Bmp: TBitmap32);
var
  A, b, C: PFColorA;
  X, Y: Integer;
begin
  A := Pointer(Bmp.Bits);
  b := Pointer(Integer(A) + 4); { *Converted from Ptr*  }
  C := Pointer(Integer(b) + 4); { *Converted from Ptr*  }
  for Y := 0 to Bmp.Height - 1 do
  begin
    A.b := (A.b + b.b) shr 1;
    A.g := (A.g + b.g) shr 1;
    A.r := (A.r + b.r) shr 1;
    for X := 0 to Bmp.Width - 3 do
    begin
      b.b := (A.b + C.b) shr 1;
      b.g := (A.g + C.g) shr 1;
      b.r := (A.r + C.r) shr 1;
      inc(A);
      inc(b);
      inc(C);
    end;
    b.b := (b.b + A.b) shr 1;
    b.g := (b.g + A.g) shr 1;
    b.r := (b.r + A.r) shr 1;
    A := Pointer(Integer(A) + 8); { *Converted from Ptr*  }
    b := Pointer(Integer(b) + 8); { *Converted from Ptr*  }
    C := Pointer(Integer(C) + 8); { *Converted from Ptr*  }
  end;
end;

procedure QuickSoft(Bmp: TBitmap32);
begin
    QuickSoft32(Bmp);
end;

procedure QuickSharp32(Bmp: TBitmap32);
var
  A, b, C: PFColorA;
  X, Y, I: Integer;
begin
  A := Pointer(Bmp.Bits);
  b := Pointer(Integer(A) + 4); { *Converted from Ptr*  }
  C := Pointer(Integer(b) + 4); { *Converted from Ptr*  }
  for Y := 0 to Bmp.Height - 1 do
  begin
    I := ((A.b shl 2) - (A.b + b.b)) div 2;
    if I > 255 then
      A.b := 255
    else if I < 0 then
      A.b := 0
    else
      A.b := I;
    I := ((A.g shl 2) - (A.g + b.g)) div 2;
    if I > 255 then
      A.g := 255
    else if I < 0 then
      A.g := 0
    else
      A.g := I;
    I := ((A.r shl 2) - (A.r + b.r)) div 2;
    if I > 255 then
      A.r := 255
    else if I < 0 then
      A.r := 0
    else
      A.r := I;
    for X := 0 to Bmp.Width - 3 do
    begin
      I := ((b.b shl 2) - (A.b + C.b)) div 2;
      if I > 255 then
        b.b := 255
      else if I < 0 then
        b.b := 0
      else
        b.b := I;
      I := ((b.g shl 2) - (A.g + C.g)) div 2;
      if I > 255 then
        b.g := 255
      else if I < 0 then
        b.g := 0
      else
        b.g := I;
      I := ((b.r shl 2) - (A.r + C.r)) div 2;
      if I > 255 then
        b.r := 255
      else if I < 0 then
        b.r := 0
      else
        b.r := I;
      inc(A);
      inc(b);
      inc(C);
    end;
    I := ((b.b shl 2) - (b.b + A.b)) div 2;
    if I > 255 then
      b.b := 255
    else if I < 0 then
      b.b := 0
    else
      b.b := I;
    I := ((b.g shl 2) - (b.g + A.g)) div 2;
    if I > 255 then
      b.g := 255
    else if I < 0 then
      b.g := 0
    else
      b.g := I;
    I := ((b.r shl 2) - (b.r + A.r)) div 2;
    if I > 255 then
      b.r := 255
    else if I < 0 then
      b.r := 0
    else
      b.r := I;
    A := Pointer(Integer(A) + 8); { *Converted from Ptr*  }
    b := Pointer(Integer(b) + 8); { *Converted from Ptr*  }
    C := Pointer(Integer(C) + 8); { *Converted from Ptr*  }
  end;
end;

procedure QuickSharp(Bmp: TBitmap32);
begin
  QuickSharp32(Bmp);
end;

procedure QuickEmboss32(Bmp: TBitmap32);
var
  A, b: PFColorA;
  X, Y: Integer;
begin
  A := Pointer(Bmp.Bits);
  b := Pointer(Integer(A) + 4); { *Converted from Ptr*  }
  for Y := 0 to Bmp.Height - 1 do
  begin
    for X := 0 to Bmp.Width - 2 do
    begin
      A.b := (b.b + (A.b xor $FFFFFFFF) and $FF) shr 1;
      A.g := (b.g + (A.g xor $FFFFFFFF) and $FF) shr 1;
      A.r := (b.r + (A.r xor $FFFFFFFF) and $FF) shr 1;
      inc(A);
      inc(b);
    end;
    A.b := (A.b + (A.b xor $FFFFFFFF) and $FF) shr 1;
    A.g := (A.g + (A.g xor $FFFFFFFF) and $FF) shr 1;
    A.r := (A.r + (A.r xor $FFFFFFFF) and $FF) shr 1;
    A := Pointer(Integer(A) + 4); { *Converted from Ptr*  }
    b := Pointer(Integer(b) + 4); { *Converted from Ptr*  }
  end;
end;

procedure QuickEmboss(Bmp: TBitmap32);
begin
    QuickEmboss32(Bmp);
end;

procedure Lines32(Bmp: TBitmap32; Lut: TLut);
var
  X, Y: Integer;
  pc: PFColorA;
  BWidth: Integer; //number of bytes per scanline
begin
  pc := Pointer(Bmp.Bits);
	BWidth := (((Bmp.Width shl 5) + 31) and -32) shr 3;
  for Y := 0 to Bmp.Height - 1 do
  begin
    if (Y and 1) = 0 then
    begin
      for X := 0 to Bmp.Width - 1 do
      begin
        pc.b := Lut[pc.b];
        pc.g := Lut[pc.g];
        pc.r := Lut[pc.r];
        inc(pc);
      end;
    end
    else begin
      pc := Pointer(Integer(pc) + BWidth); { *Converted from Ptr*  }
    end;
  end;
end;

procedure Lines(Bmp: TBitmap32; Lut: TLut);
begin
    Lines32(Bmp, Lut);
end;

procedure Mosaic32(Bmp: TBitmap32; xAmount, yAmount: Integer);
var
  Delta, tx, ty, ix, iy, cx, cy, X, Y: Integer;
  Line: PLine32;
  pc: PFColorA;
  tc: TFColorA;
  BWidth: Integer; //number of bytes per scanline
begin
  xAmount := Abs(xAmount);
  yAmount := Abs(yAmount);
  if (xAmount = 0) or (yAmount = 0) then
    Exit;
  ix := (xAmount shr 1) + (xAmount and 1);
  iy := (yAmount shr 1) + (yAmount and 1);
	BWidth := (((Bmp.Width shl 5) + 31) and -32) shr 3;
  Y := 0;
  while Y < Bmp.Height do
  begin
    X := 0;
    cy := Y + iy;
    if cy >= Bmp.Height then
      Line := PLine32(Bmp.Scanline[Bmp.Height - 1])
    else
      Line := PLine32(Bmp.Scanline[cy]);
    if Y + yAmount - 1 > Bmp.Height - 1 then
      ty := Bmp.Height - Y
    else
      ty := yAmount;
    while X < Bmp.Width do
    begin
      cx := X + ix;
      if cx >= Bmp.Width then
        tc := Line[Bmp.Width - 1]
      else
        tc := Line[cx];
      if X + xAmount - 1 > Bmp.Width - 1 then
        tx := Bmp.Width - X
      else
        tx := xAmount;
      Delta := Integer(BWidth) - tx * 4;
      pc := PFColorA(Bmp.PixelPtr[Y, X]);
      for cy := 1 to ty do
      begin
        for cx := 1 to tx do
        begin
          pc^ := tc;
          inc(pc);
        end;
        pc := Pointer(Integer(pc) + Delta); { *Converted from Ptr*  }
      end;
      inc(X, xAmount);
    end;
    inc(Y, yAmount);
  end;
end;

procedure Mosaic(Bmp: TBitmap32; xAmount, yAmount: Integer);
begin
    Mosaic32(Bmp, xAmount, yAmount);
end;

procedure RotateSize(Src, Dst: TBitmap32; Angle: Double);
var
  Theta: Double;
  w, h: Integer;
begin
  Theta := Abs(Angle) * (Pi / 180);
  W := Round(Abs(Src.Width * Cos(Theta)) + Abs(Src.Height * Sin(Theta)) + 0.4);
  H := Round(Abs(Src.Width * Sin(Theta)) + Abs(Src.Height * Cos(Theta)) + 0.4);
  Dst.SetSize(w, h);
end;

//TODO: The Smooth do not work yet.
procedure Transform32(Src, Dst: TBitmap32; cx, cy, isin, icos: Integer; Smooth: Boolean);
var
  X, Y, t1, t2, dx, dy, xd, yd, sdx, sdy, ax, ay, ex, ey: Integer;
  c00, c01, c10, c11: TFColorA;
  pc, sp: PFColorA;
  BWidth: Integer; //number of bytes per scanline
begin
  xd := ((Src.Width shl 16) - (Dst.Width shl 16)) div 2;
  yd := ((Src.Height shl 16) - (Dst.Height shl 16)) div 2;
  ax := (cx shl 16) - (icos * cx);
  ay := (cy shl 16) - (isin * cx);
  pc := Pointer(Dst.Bits);
	BWidth := (((Src.Width shl 5) + 31) and -32) shr 3;
  if Smooth then
  begin
    for Y := 0 to Dst.Height - 1 do
    begin
      dy := cy - Y;
      sdx := (ax + (isin * dy)) + xd;
      sdy := (ay - (icos * dy)) + yd;
      for X := 0 to Dst.Width - 1 do
      begin
        dx := Smallint(sdx shr 16);
        dy := Smallint(sdy shr 16);
        if (dx >= -1) and (dy >= -1) and (dx < Src.Width) and (dy < Src.Height) then
        begin
          if (dx >= 0) and (dy >= 0) and (dx < Src.Width - 1) and (dy < Src.Height - 1) then
          begin
            sp := PFColorA(Src.PixelPtr[dx, dy]);
            c00 := sp^;
            inc(sp);
            c01 := sp^;
            sp := Pointer(Integer(sp) + BWidth - 4); { *Converted from Ptr*  }
            //sp := PFColorA(Src.PixelPtr[dy+1, dx]);
            c10 := sp^;
            inc(sp);
            c11 := sp^;
          end
          else if (dx = Src.Width - 1) and (dy = Src.Height - 1) then
          begin
            c00 := TFColorA(Src[dx, dy]);
            c01 := pc^;
            c10 := pc^;
            c11 := pc^;
          end
          else if (dx = -1) and (dy = -1) then
          begin
            c00 := pc^;
            c01 := pc^;
            c10 := pc^;
            c11 := PFColorA(Src.Bits)^;
          end
          else if (dx = -1) and (dy = Src.Height - 1) then
          begin
            c00 := pc^;
            c01 := PFColorA(Src.Scanline[dy])^;
            c10 := pc^;
            c11 := pc^;
          end
          else if (dx = Src.Width - 1) and (dy = -1) then
          begin
            c00 := pc^;
            c01 := pc^;
            c10 := TFColorA(Src[dx, 0]);
            ;
            c11 := pc^;
          end
          else if dx = -1 then
          begin
            c00 := pc^;
            sp := PFColorA(Src.PixelPtr[0, dy]);
            c01 := sp^;
            c10 := pc^;
            sp := Pointer(Integer(sp) + BWidth); { *Converted from Ptr*  }
            c11 := sp^;
          end
          else if dy = -1 then
          begin
            c00 := pc^;
            c01 := pc^;
            sp := PFColorA(Src.PixelPtr[dx, 0]);
            c10 := sp^;
            inc(sp);
            c11 := sp^;
          end
          else if dx = Src.Width - 1 then
          begin
            sp := PFColorA(Src.PixelPtr[dx, dy]);
            c00 := sp^;
            c01 := pc^;
            sp := Pointer(Integer(sp) + BWidth); { *Converted from Ptr*  }
            c10 := sp^;
            c11 := pc^;
          end
          else if dy = Src.Height - 1 then
          begin
            sp := PFColorA(Src.PixelPtr[dx, dy]);
            c00 := sp^;
            inc(sp);
            c01 := sp^;
            c10 := pc^;
            c11 := pc^;
          end;
          ex := sdx and $FFFF;
          ey := sdy and $FFFF;
          t1 := ((((c01.b - c00.b) * ex) shr 16) + c00.b) and $FF;
          t2 := ((((c11.b - c10.b) * ex) shr 16) + c10.b) and $FF;
          pc.b := (((t2 - t1) * ey) shr 16) + t1;
          t1 := ((((c01.g - c00.g) * ex) shr 16) + c00.g) and $FF;
          t2 := ((((c11.g - c10.g) * ex) shr 16) + c10.g) and $FF;
          pc.g := (((t2 - t1) * ey) shr 16) + t1;
          t1 := ((((c01.r - c00.r) * ex) shr 16) + c00.r) and $FF;
          t2 := ((((c11.r - c10.r) * ex) shr 16) + c10.r) and $FF;
          pc.r := (((t2 - t1) * ey) shr 16) + t1;
        end;
        inc(sdx, icos);
        inc(sdy, isin);
        inc(pc);
      end;
    end;
  end
  else
  begin
    for Y := 0 to Dst.Height - 1 do
    begin
      dy := cy - Y;
      sdx := (ax + (isin * dy)) + xd;
      sdy := (ay - (icos * dy)) + yd;
      for X := 0 to Dst.Width - 1 do
      begin
        dx := sdx shr 16;
        dy := sdy shr 16;
        if (dx < Src.Width) and (dy < Src.Height) then
          pc^ := TFColorA(Src[dx, dy]);
        inc(sdx, icos);
        inc(sdy, isin);
        inc(pc);
      end;
      pc := Pointer(Integer(pc)); { *Converted from Ptr*  }
    end;
  end;
end;

procedure Transform(Src, Dst: TBitmap32; cx, cy, isin, icos: Integer; Smooth: Boolean);
begin
    Transform32(Src, Dst, cx, cy, isin, icos, Smooth);
end;

procedure Rotate(Src, Dst: TBitmap32; Angle: Double; Smooth: Boolean);
begin
  Transform(Src, Dst,
    Dst.Width shr 1,
    Dst.Height shr 1,
    Round(Sin(Angle * Pi / 180) * 65536),
    Round(Cos(Angle * Pi / 180) * 65536),
    Smooth);
end;

procedure Rotozoom(Src, Dst: TBitmap32; Angle: Double; Zoom: Integer; Smooth: Boolean);
begin
  Transform(Src, Dst,
    Dst.Width shr 1,
    Dst.Height shr 1,
    Round(Sin(Angle * Pi / 180) * Zoom),
    Round(Cos(Angle * Pi / 180) * Zoom),
    Smooth);
end;


end.

