
{**********************************************************************
 Package pl_Graphics32EXT.pkg
 this unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit XGR32_Bmp32Func;


interface

uses
  sysutils,math, Graphics,
  GR32

  , GR32_Blend
  , GR32_Transforms
  , GR32_Filters
  , GR32_Resamplers,
  XGR32_Color;

function IntToByte(i:Integer):Byte;
function ByteToInt(i:Byte):Integer;

procedure Bmp32_Brightness(src:tbitmap32; brightness:real);
procedure Bmp32_Contrast(src:tbitmap32; contrast:real);
procedure Bmp32_Gamma(src:tbitmap32; gamma:real);

procedure FillLut(var Lut: TLUT8; x1, y1, x2, y2: Byte);
procedure BuildLUT(var LUT: TLUT8);
procedure Bmp32_ApplyLUTRGB(Dst, Src: TBitmap32; const LUT_R, LUT_G, LUT_B: TLUT8);

procedure Bmp32_StretchIntensity(src: TBitmap32; const LowVal, HighVal: Integer);
procedure Bmp32_Rotate(src: TBitmap32; alpha:extended);
procedure Bmp32_FlipHoriz(src: TBitmap32);
procedure Bmp32_FlipVert(src: TBitmap32);
procedure Bmp32_FloydSteinbergDither(bm32: TBitmap32; shift: Integer);
procedure Bmp32_AutoContrastRBG(Bitmap: TBitmap32);
function  Bmp32_CountColors(const bm32: TBitmap32): Integer;

procedure Bmp32_Saturation(Src: tbitmap32; Amount: Integer);
procedure Bmp32_ToBinary(src: tbitmap32; amount: integer);
procedure Bmp32_IncDecRGB(Src:tbitmap32;r0,g0,b0:integer);
procedure Bmp32_IncDecHSL(Src:tbitmap32;h0,s0,l0:integer);
procedure Bmp32_IncDecCMYK(Src:tbitmap32;c2,m2,y2,k2:integer);

Procedure Bmp32_LoadFromFile(Src:TBitmap32;const afilename:string);


implementation


function IntToByte(i:Integer):Byte;
begin
  if      i>255 then Result:=255
  else if i<0   then Result:=0
  else               Result:=i;
end;

function ByteToInt(i:Byte):Integer;
begin
  result:=i;
end;

procedure Bmp32_Contrast(src: TBitmap32;contrast:real);
//scale from -1..1
var
 F: Single;
 I, V: Integer;
 LUT: TLUT8;
 dst:tbitmap32;
begin
  dst:=tbitmap32.create;
  dst.DrawMode:=dmBlend;
  dst.SetSize(src.width,src.height);

 for I := 0 to 255 do
 begin
 F := I / 255; // Scale to 0..1 range
 F := (F - 0.5) * (1 + CONTRAST) + 0.5; // Contrast
 V := Round(F * 255); // Scale to 0..255 range

 if V < 0 then V := 0
 else if V > 255 then V := 255; // Clip to 0..255 range

 LUT[I] := V;
 end;
 ApplyLUT(dst, src, LUT);
 src.assign(dst);
 src.changed;
 dst.free;
end;

procedure Bmp32_Brightness(src: TBitmap32; brightness:real);
//scale from -1..1
var
 F: Single;
 I, V: Integer;
 LUT: TLUT8;
 dst:tbitmap32;
 begin
  dst:=tbitmap32.create;
  dst.DrawMode:=dmBlend;
  dst.SetSize(src.width,src.height);

 for I := 0 to 255 do
 begin
 F := I / 255; // Scale to 0..1 range
 F := F + BRIGHTNESS; // Brightness
 V := Round(F * 255); // Scale to 0..255 range

 if V < 0 then V := 0
 else if V > 255 then V := 255; // Clip to 0..255 range

 LUT[I] := V;
 end;
 ApplyLUT(dst, src, LUT,true);
 src.assign(dst);
 src.changed;
 dst.free;
end;

procedure Bmp32_Gamma(Src: TBitmap32; gamma:real);
//scale from -1..1
var
 F: Single;
 I, V: Integer;
 LUT: TLUT8;
 dst:tbitmap32;
 begin
  dst:=tbitmap32.create;
  dst.DrawMode:=dmBlend;
  dst.SetSize(src.width,src.height);

 for I := 0 to 255 do
 begin
 Gamma:=ensurerange(gamma,0,5);
 F:= EnsureRange(I/255, 0,1); //0..1 range
 F := Power(F, GAMMA); // Gamma
 V := Round(F * 255); // Scale to 0..255 range

 if V < 0 then V := 0
 else if V > 255 then V := 255; // Clip to 0..255 range

 LUT[I] := V;
 end;
 ApplyLUT(dst, src, LUT);
 src.assign(dst);
 src.changed;
 dst.free;
end;

procedure Bmp32_StretchIntensity(src: TBitmap32; const LowVal, HighVal: Integer);
 var
 i: Integer;
 LUT: TLUT8;
  dst:tbitmap32;
 w,h:integer;
 begin
   if LowVal + 1 > HighVal - 1 then Exit;

  dst:=tbitmap32.create;
  w:=src.width;
  h:=src.height;
  dst.width:=w;
  dst.height:=h;
 // validation


 // set values 0 to LowVal to black...
 for i := 0 to LowVal do LUT[i] := 0;

 // 'stretch' 0 - 255 values between LowVal and HighVal
 for i := LowVal + 1 to HighVal - 1 do
 LUT[i] := (i - LowVal) * 255 div (HighVal - LowVal);

 // set values HighVal to 255 to white...
 for i := HighVal to 255 do
 LUT[i] := 255;

 ApplyLUT(dst, src, LUT);
   src.assign(dst);
   src.changed;
   dst.free;
end;

procedure BuildLUT(var LUT: TLUT8);
 var
 L, M, H, Lo, Hi: Single;
 I: Integer;
 in_lo,in_mid,in_hi,out_lo,out_hi:integer;  //tobemodified <-

 function F(X: Single): Integer;
 var
 R: Single;
 begin
 in_lo:=0;
 in_mid:=0;
 in_hi:=0;

 if X < L then R := 0
 else if X < M then R := (X - L) / 2 / (M - L)
 else if X < H then R := (X - M) / 2 / (H - M) + 0.5
 else R := 1;
 R := Lo + R * (Hi - Lo);
 Result := Round(R);
 if Result < 0 then Result := 0
 else if Result > 255 then Result := 255;
 end;

 begin
 //init
 Out_Lo:=0;
 Out_Hi:=0;
 //rest
 L := IN_LO / 255;
 M := IN_MID / 255;
 H := IN_HI / 255;
 Lo := OUT_LO;
 Hi := OUT_HI;
 for I := 0 to 255 do LUT[I] := F(I / 255);
 end;

procedure FillLut(var Lut: TLUT8; x1, y1, x2, y2: Byte);
var
  x, n, i, ii: Integer;
begin
  n := x2 - x1;
  if n <> 0 then
  begin
    i := y1 shl 16;
    ii := ((y2 - y1 + 1) shl 16) div n;
    for x := x1 to x2 - 1 do
    begin
      Lut[x] := i shr 16;
        // this may create overflow of byte when i shr 16 evaluates to > 255...
      Inc(i, ii);
    end;
    // .. so we are going to force set the last cell:
    Lut[x2] := y2;
  end;
end;


procedure Bmp32_Rotate(src: TBitmap32; alpha:extended);
var
  tmp:tbitmap32;
  SrcR: Integer;
  SrcB: Integer;
  T: TAffineTransformation;
//  Sx, Sy: Single;
  dst:tbitmap32;
  w,h:integer;
begin

  dst:=tbitmap32.create;
  w:=src.width;
  h:=src.height;
  dst.width:=w;
  dst.height:=h;
  tmp:=TBitmap32.Create;
  tmp.Assign(src);
  SrcR := src.Width - 1;
  SrcB := src.Height - 1;
  T := TAffineTransformation.Create;
  T.SrcRect := FloatRect(0, 0, SrcR + 1, SrcB + 1);
  try
    T.Clear;
    // move the origin to a center for scaling and rotation
    T.Translate(-SrcR / 2, -SrcB / 2);
    T.Rotate(0, 0, Alpha);
//    Alpha := Alpha * 3.14159265358979 / 180;

    // get the width and height of rotated image (without scaling)
//    Sx := Abs(SrcR * Cos(Alpha)) + Abs(SrcB * Sin(Alpha));
//    Sy := Abs(SrcR * Sin(Alpha)) + Abs(SrcB * Cos(Alpha));

    // calculate a new scale so that the image fits in original boundaries

    // move the origin back
    T.Translate(SrcR / 2, SrcB / 2);
    // transform the bitmap
    tmp.clear;
    tmp.BeginUpdate;
    Transform(tmp,src, T);
    tmp.EndUpdate;
  finally
    T.Free;
  end;
  dst.Changed;
  src.Assign(tmp);
  tmp.free;
  dst.free;
end;

procedure Bmp32_FlipHoriz(src: TBitmap32);
var
  i, j: Integer;
  P1, P2: PColor32;
  tmp: TColor32;
  W,h, W2: Integer;
  dst:tbitmap32;
begin
  dst:=tbitmap32.create;
  w:=src.width;
  h:=src.height;
  dst.width:=w;
  dst.height:=h;
//  if (Dst = nil) or (Dst = Self) then
  begin
    { In-place flipping }
    P1 := @src.Bits[0];
    P2 := P1;
    Inc(P2, w - 1);
    W2 := w shr 1;
    for J := 0 to h - 1 do
    begin
      for I := 0 to W2 - 1 do
      begin
        tmp := P1^;
        P1^ := P2^;
        P2^ := tmp;
        Inc(P1);
        Dec(P2);
      end;
      Inc(P1, W - W2);
      Inc(P2, W + W2);
    end;
    src.Changed;
end;
end;


procedure Bmp32_FlipVert(src: Tbitmap32);
var
   dest:tbitmap32;
   w,h,x,y:integer;
   pd,ps:Pcolor32array;
begin
  w:=src.width;
  h:=src.height;
  dest:=tbitmap32.create;
  dest.width:=w;
  dest.height:=h;
  for y:=0 to h-1 do begin
   pd:=dest.scanline[y];
   ps:=src.scanline[h-1-y];
   for x:=0 to w-1 do begin
     pd[x]:=ps[x];
   end;
   end;
   src.assign(dest);
   src.changed;
  dest.free;
end;

procedure Bmp32_AutoContrastRBG(Bitmap: TBitmap32);
var
  hr, hg, hb, lr, lg, lb: Byte;
  r, g, b: TLUT8;
  pc: PColorBGRA;
  x, y, w, h: Integer;
begin
  hr := 0;
  hg := 0;
  hb := 0;
  lr := 255;
  lg := 255;
  lb := 255;
  pc := PColorBGRA(@Bitmap.Bits[0]);
  w := Bitmap.Width;
  h := Bitmap.Height;

  for y := 0 to h - 1 do
  begin
    for x := 0 to w - 1 do
    begin
      if pc^.b > hb then hb := pc^.b;
      if pc^.b < lb then lb := pc^.b;
      if pc^.g > hg then hg := pc^.g;
      if pc^.g < lg then lg := pc^.g;
      if pc^.r > hr then hr := pc^.r;
      if pc^.r < lr then lr := pc^.r;
      Inc(pc);
    end;
  end;

  if ((lr or lg or lb) <> 0) or ((hr and hg and hb) <> 255) then
  begin
    FillLut(r, lr, 0, hr, 255);
    FillLut(g, lg, 0, hg, 255);
    FillLut(b, lb, 0, hb, 255);
    Bmp32_ApplyLUTRGB(Bitmap, Bitmap, r, g, b);
  end;
end;

procedure Bmp32_ApplyLUTRGB(Dst, Src: TBitmap32; const LUT_R, LUT_G, LUT_B: TLUT8);
var
  I: Integer;
  D, S: PColor32;
  r, g, b: TColor32;
  C: TColor32;
begin
 // CheckParams(Src, Dst);
  D := @Dst.Bits[0];
  S := @Src.Bits[0];

  for I := 0 to Src.Width * Src.Height - 1 do
  begin
    C := S^;
    r := C and $00FF0000;
    g := C and $0000FF00;
    r := r shr 16;
    b := C and $000000FF;
    g := g shr 8;
    r := LUT_R[r];
    g := LUT_G[g];
    b := LUT_B[b];
    D^ := $FF000000 or r shl 16 or g shl 8 or b;
    Inc(S);
    Inc(D);
  end;
  Dst.Changed;
end;


function Bmp32_CountColors(const bm32: TBitmap32): Integer;
var
  cols: array[0..65535] of array of Byte;
  i, j, k: Integer;
  row: PColor32Array;
  pix, wl: Cardinal;
begin
  result := 0;
  wl := bm32.Width - 1;
  for j := 0 to bm32.Height - 1 do begin
    row := bm32.Scanline[j];
    for i := 0 to wl do begin
      pix := row^[i] and $FFFFFF;
      k := pix shr 8;
      if cols[k] = nil then SetLength(cols[k], 256);
      cols[k][pix and $FF] := 1;
    end;
  end;
  for j := 0 to 65535 do
      if cols[j] <> nil then begin
        for k := 0 to 255 do if cols[j][k] > 0 then Inc(result);
        SetLength(cols[j], 0);
      end;
end;



procedure Bmp32_FloydSteinbergDither(bm32: TBitmap32; shift: Integer);
var
  j, jl, k, kl, ki, ks, ke, kn: Integer;
  rowIn, rowNx: PColor32Array;
  delta, m, n: Cardinal;
  r, g, b: Cardinal;
  lutby1, lutby3, lutby5, lutby7: TLUT8;
begin
  m := $FF shr shift shl shift;
  n := not m and $FF;
  m := $FF000000 or m shl 16 or m shl 8 or m;
  for j := 0 to 255 do begin
    lutby1[j] := j shr 4;
    lutby3[j] := (j * 3) shr 4;
    lutby5[j] := (j * 5) shr 4;
    lutby7[j] := (j * 7) shr 4;
  end;
  kl := bm32.Width - 1;
  jl := bm32.Height - 1;
  ki := 1;
  rowNx := bm32.Scanline[0];
  for j := 0 to jl do begin
    rowIn := rowNx;
    if j < jl then rowNx := bm32.Scanline[j + 1];
    ki := -ki;
    if ki = -1 then begin // right to left -> odd row
      ks := kl;
      ke := 0;
    end
    else begin // left to right -> even row
      ks := 0;
      ke := kl;
    end;
    kn := ks;
    repeat
      k := kn;
      kn := k + ki;
      delta := rowIn^[k];
      r := delta shr 16 and n;
      g := delta shr 8 and n;
      b := delta and n;
      rowIn^[k] := TColor32(rowIn^[k] and m);
      if k <> ke then begin
        rowIn^[kn] := Color32AddKeepAlpha(rowIn^[kn], lutby7[r] shl 16 or lutby7[g] shl 8 or lutby7[b]);
        if j < jl then rowNx^[kn] := Color32AddKeepAlpha(rowNx^[kn], lutby1[r] shl 16 or lutby1[g] shl 8 or lutby1[b]);
      end;
      if j < jl then begin
        rowNx^[k] := Color32AddKeepAlpha(rowNx^[k], lutby5[r] shl 16 or lutby5[g] shl 8 or lutby5[b]);
        if k <> ks then rowNx^[k - ki] := Color32AddKeepAlpha(rowNx^[k - ki], lutby3[r] shl 16 or lutby3[g] shl 8 or lutby3[b]);
      end;
    until k = ke;
  end;
end;


Procedure Bmp32_LoadFromFile(Src:TBitmap32;const afilename:string);
var
  Picture: TPicture;
begin
  if Src=nil then exit;
  if afilename='' then exit;
  if Fileexists(afilename)=false then exit;

  Picture := TPicture.Create;
  try
    Picture.LoadFromFile(afilename);
    Src.Assign(Picture);
  finally
    Picture.Free;
  end;
end;


procedure Bmp32_Saturation(Src: tbitmap32; Amount: Integer);
var
p0:Pcolor32array;
x,y:integer;
r,g,b,alfa:byte;
gray:integer;
SS:Pcolor32;
begin
  SS := @Src.Bits[0];

  for y:=0 to Src.Height-1 do
  begin
    p0:=Src.scanline[y];
    for x:=0 to Src.Width-1 do
    begin
      b:=(p0^[x] and $FF);
      g:=(p0^[x] shr 8) and $FF;
      r:=(p0^[x] shr 16) and $FF;
      Gray:=(r+g+b)div 3;
      r:=IntToByte(Gray+(((r-Gray)*Amount)div 255));
      g:=IntToByte(Gray+(((g-Gray)*Amount)div 255));
      b:=IntToByte(Gray+(((b-Gray)*Amount)div 255));

    alfa:=AlphaComponent(SS^);
    SS^ :=Color32(r,g,b,alfa);
    //SS^ := $FF000000 + r shl 16 + g shl 8 + b;
    inc(SS);
    end;
  end;
  Src.Changed;
end;


procedure Bmp32_ToBinary(src: tbitmap32; amount: integer);
var x,y:integer;
    ps:Pcolor32array;
    c:integer;
    r0,g0,b0:integer;
    r,alfa:byte;
    SS:Pcolor32;
begin
  SS := @src.Bits[0];

  for y:=0 to src.height-1 do begin
   ps:=src.scanline[y];
   for x:=0 to src.width-1 do begin
      b0:=(ps^[x] and $FF);
      g0:=(ps^[x] shr 8) and $FF;
      r0:=(ps^[x] shr 16) and $FF;
      c:=(r0+g0+b0)div 3;
      if (c>amount) then r:=$ff
    else r:=$00;

      alfa:=AlphaComponent(SS^);
      SS^ :=Color32(r,r,r,alfa);

      //SS^ := $FF000000 + r shl 16 + r shl 8 + r;
    inc(SS);
    end;
   end;
   src.Changed;
end;


procedure Bmp32_IncDecRGB(Src:tbitmap32;r0,g0,b0:integer);
var
i:integer;
r,g,b,a: byte;
c:Tcolor32;
begin

with Src do
  begin
   for i:=0 to Width*Height-1 do
    begin
     c:=Bits^[i];
     R:=(c and $00FF0000) shr 16;
     G:=(c and $0000FF00) shr 8;
     B:=c and $000000FF;
     A:=c shr 24;

     R := EnsureRange(r+r0, 0,255);
     G := EnsureRange(g+g0, 0,255);
     B := EnsureRange(b+b0, 0,255);

     Bits^[i]:=Color32(R,G,B,A);
    end;
   Src.Changed;
  end;
end;


function xHSLtoRGB(H, S, L: Integer): TColor32;
var
  V, M, M1, M2, VSF: Integer;
begin
  if L <= $7F then
    V := L * (256 + S) shr 8
  else
    V := L + S - L * S div 255;
  if V <= 0 then
    Result := Color32(0, 0, 0, 0)
  else
  begin
    M := L * 2 - V;
    H := H * 6;
    VSF := (V - M) * (H and $ff) shr 8;
    M1 := M + VSF;
    M2 := V - VSF;
    case H shr 8 of
      0: Result := Color32(V, M1, M, 0);
      1: Result := Color32(M2, V, M, 0);
      2: Result := Color32(M, V, M1, 0);
      3: Result := Color32(M, M2, V, 0);
      4: Result := Color32(M1, M, V, 0);
      5: Result := Color32(V, M, M2, 0);
    end;
  end;
end;

procedure xRGBtoHSL(RGB: TColor32; out H, S, L: Byte);
var
  R, G, B, D, Cmax, Cmin, HL: Integer;
begin
  R := (RGB shr 16) and $ff;
  G := (RGB shr 8) and $ff;
  B := RGB and $ff;

  Cmax := Max(R, Max(G, B));
  Cmin := Min(R, Max(G, B));
  L := (Cmax + Cmin) div 2;

  if Cmax = Cmin then
  begin
    H := 0;
    S := 0
  end
  else
  begin
    D := (Cmax - Cmin) * 255;
    if L <= $7F then
      S := D div (Cmax + Cmin)
    else
      S := D div (255 * 2 - Cmax - Cmin);

    D := D * 6;
    if R = Cmax then
      HL := (G - B) * 255 * 255 div D
    else if G = Cmax then
      HL := 255 * 2 div 6 + (B - R) * 255 * 255 div D
    else
      HL := 255 * 4 div 6 + (R - G) * 255 * 255 div D;

    if HL < 0 then HL := HL + 255 * 2;
    H := HL;
  end;
end;


procedure Bmp32_IncDecHSL(Src:tbitmap32;h0,s0,l0:integer);
var
i: Integer;
a,hd,sd,vd:byte;
begin

with Src do
  begin
   for i:=0 to Width*Height-1 do
    begin
    a:=Bits^[i] shr 24;

     xRGBToHSL(Bits^[i],hd,sd,vd);

     sd := round(ensurerange(sd + (s0),0,255));
     vd := round(ensurerange(vd + (l0),0,255));
     hd := hd + (h0);

     if hd>255 then hd:=hd-255;
     if hd<0 then hd:=255-hd;

    Bits^[i]:=SetAlpha(xHSLtoRGB(hd,sd,vd),a);

    end;
   Src.Changed;
  end;
end;


procedure RGBTOCMYK(R : byte;
                    G : byte;
                    B : byte;
                    var C : byte;
                    var M : byte;
                    var Y : byte;
                    var K : byte);
begin
  C := 255 - R;
  M := 255 - G;
  Y := 255 - B;
  if C < M then
    K := C else
    K := M;
  if Y < K then
    K := Y;
  if k > 0 then begin
    c := c - k;
    m := m - k;
    y := y - k;
  end;
end;

procedure CMYKTORGB(C : byte;
                    M: byte;
                    Y : byte;
                    K : byte;
                    var R : byte;
                    var G : byte;
                    var B : byte);
begin
   if (Integer(C) + Integer(K)) < 255 then
     R := 255 - (C + K) else
     R := 0;
   if (Integer(M) + Integer(K)) < 255 then
     G := 255 - (M + K) else
     G := 0;
   if (Integer(Y) + Integer(K)) < 255 then
     B := 255 - (Y + K) else
     B := 0;
end;

procedure Bmp32_IncDecCMYK(Src:tbitmap32;c2,m2,y2,k2:integer);
var
 i:integer;
 r,g,b,a: byte;
 c0,m0,y0,k0:byte;
 c:Tcolor32;
begin

with Src do
  begin
   for i:=0 to Width*Height-1 do
    begin
     c:=Bits^[i];
     R:=(c and $00FF0000) shr 16;
     G:=(c and $0000FF00) shr 8;
     B:=c and $000000FF;
     A:=c shr 24;
    RGBTOCMYK(r,g,b,c0,m0,y0,k0);
     c0 := round(ensurerange(c0 + (c2),0,255));
     m0 := round(ensurerange(m0 + (m2),0,255));
     y0 := round(ensurerange(y0 + (y2),0,255));
     k0 := round(ensurerange(k0 + (k2),0,255));
    CMYKTORGB(c0,m0,y0,k0,r,g,b);

     Bits^[i]:=Color32(R,G,B,A);
    end;
   Src.Changed;
  end;
end;




end.
