
{**********************************************************************
 Package pl_Graphics32EXT.pkg
 this unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit XGR32_FlareFX;

interface
 uses
  GR32;

  procedure FlareFX32(Source, Dest: TBitmap32; const X, Y: Integer);
  procedure GlassTile32(a_Source, a_Dest: TBitmap32; const a_TileWidth, a_TileHeight: Integer);

implementation
 uses  Math;  // Hypot()

type

  TRGBFloat = record
    r: Real;
    g: Real;
    b: Real;
  end;

  TReflect = record
    CCol : TRGBFloat;
    Size : Real;
    xp   : Integer;
    yp   : Integer;
    Types: Integer;
  end;

var
  SColor, SGlow, SInner, SOuter, SHalo: Real;
  Color, Glow, Inner, Outer, Halo     : TRGBFloat;
  Ref1  : array [0 .. 18] of TReflect;
  NumRef: Integer;

procedure FixPix(var AColor: TColor32; const Procent: Real; const ColPro: TRGBFloat);
var
  a      : Cardinal;
  r, g, b: Integer;
begin
  a := AColor shr 24 and $FF;
  r := AColor shr 16 and $FF;
  g := AColor shr  8 and $FF;
  b := AColor        and $FF;

  r := r + Trunc( (255 - r) * Procent * ColPro.r );
  g := g + Trunc( (255 - g) * Procent * ColPro.g );
  b := b + Trunc( (255 - b) * Procent * ColPro.b );

  AColor := (a shl 24) or (r shl 16) or (g shl 8) or b;
end; { FixPix }

procedure MColor(var AColor: TColor32; h: Real);
var
  Procent: Real;
begin
  Procent := SColor - h;
  Procent := Procent / SColor;
  if Procent > 0.0 then
  begin
    Procent := Procent * Procent;
    FixPix(AColor, Procent, Color);
  end;
end; { MColor }

procedure MGlow(var AColor: TColor32; h: Real);
var
  Procent: Real;
begin
  Procent := SGlow - h;
  Procent := Procent / SGlow;
  if Procent > 0.0 then
  begin
    Procent := Procent * Procent;
    FixPix(AColor, Procent, Glow);
  end;
end; { MGlow }

procedure MInner(var AColor: TColor32; h: Real);
var
  Procent: Real;
begin
  Procent := SInner - h;
  Procent := Procent / SInner;
  if Procent > 0.0 then
  begin
    Procent := Procent * Procent;
    FixPix(AColor, Procent, Inner);
  end;
end; { MInner }

procedure MOuter(var AColor: TColor32; h: Real);
var
  Procent: Real;
begin
  Procent := SOuter - h;
  Procent := Procent / SOuter;
  if   Procent > 0.0
  then FixPix(AColor, Procent, Outer);
end; { MOuter }

procedure MHalo(var AColor: TColor32; h: Real);
var
  Procent: Real;
begin
  Procent := h - SHalo;
  Procent := Procent / (SHalo * 0.07);
  Procent := Abs(Procent);
  if   Procent < 1.0
  then FixPix(AColor, 1.0 - Procent, Halo);
end; { MHalo }

procedure InitRef(sx, sy, Width, Height, Matt: Integer);
var
  xh, yh, dx, dy: Integer;
begin
  xh              := Width  div 2;
  yh              := Height div 2;
  dx              := xh - sx;
  dy              := yh - sy;
  NumRef          := 19;
  Ref1[0].Types   := 1;
  Ref1[0].Size    := Matt * 0.027;
  Ref1[0].xp      := Round(0.6699 * dx + xh);
  Ref1[0].yp      := Round(0.6699 * dy + yh);
  Ref1[0].CCol.r  := 0.0;
  Ref1[0].CCol.g  := 14.0 / 255.0;
  Ref1[0].CCol.b  := 113.0 / 255.0;
  Ref1[1].Types   := 1;
  Ref1[1].Size    := Matt * 0.01;
  Ref1[1].xp      := Round(0.2692 * dx + xh);
  Ref1[1].yp      := Round(0.2692 * dy + yh);
  Ref1[1].CCol.r  := 90.0 / 255.0;
  Ref1[1].CCol.g  := 181.0 / 255.0;
  Ref1[1].CCol.b  := 142.0 / 255.0;
  Ref1[2].Types   := 1;
  Ref1[2].Size    := Matt * 0.005;
  Ref1[2].xp      := Round(-0.0112 * dx + xh);
  Ref1[2].yp      := Round(-0.0112 * dy + yh);
  Ref1[2].CCol.r  := 56.0 / 255.0;
  Ref1[2].CCol.g  := 140.0 / 255.0;
  Ref1[2].CCol.b  := 106.0 / 255.0;
  Ref1[3].Types   := 2;
  Ref1[3].Size    := Matt * 0.031;
  Ref1[3].xp      := Round(0.6490 * dx + xh);
  Ref1[3].yp      := Round(0.6490 * dy + yh);
  Ref1[3].CCol.r  := 9.0 / 255.0;
  Ref1[3].CCol.g  := 29.0 / 255.0;
  Ref1[3].CCol.b  := 19.0 / 255.0;
  Ref1[4].Types   := 2;
  Ref1[4].Size    := Matt * 0.015;
  Ref1[4].xp      := Round(0.4696 * dx + xh);
  Ref1[4].yp      := Round(0.4696 * dy + yh);
  Ref1[4].CCol.r  := 24.0 / 255.0;
  Ref1[4].CCol.g  := 14.0 / 255.0;
  Ref1[4].CCol.b  := 0.0;
  Ref1[5].Types   := 2;
  Ref1[5].Size    := Matt * 0.037;
  Ref1[5].xp      := Round(0.4087 * dx + xh);
  Ref1[5].yp      := Round(0.4087 * dy + yh);
  Ref1[5].CCol.r  := 24.0 / 255.0;
  Ref1[5].CCol.g  := 14.0 / 255.0;
  Ref1[5].CCol.b  := 0.0;
  Ref1[6].Types   := 2;
  Ref1[6].Size    := Matt * 0.022;
  Ref1[6].xp      := Round(-0.2003 * dx + xh);
  Ref1[6].yp      := Round(-0.2003 * dy + yh);
  Ref1[6].CCol.r  := 42.0 / 255.0;
  Ref1[6].CCol.g  := 19.0 / 255.0;
  Ref1[6].CCol.b  := 0.0;
  Ref1[7].Types   := 2;
  Ref1[7].Size    := Matt * 0.025;
  Ref1[7].xp      := Round(-0.4103 * dx + xh);
  Ref1[7].yp      := Round(-0.4103 * dy + yh);
  Ref1[7].CCol.b  := 17.0 / 255.0;
  Ref1[7].CCol.g  := 9.0 / 255.0;
  Ref1[7].CCol.r  := 0.0;
  Ref1[8].Types   := 2;
  Ref1[8].Size    := Matt * 0.058;
  Ref1[8].xp      := Round(-0.4503 * dx + xh);
  Ref1[8].yp      := Round(-0.4503 * dy + yh);
  Ref1[8].CCol.b  := 10.0 / 255.0;
  Ref1[8].CCol.g  := 4.0 / 255.0;
  Ref1[8].CCol.r  := 0.0;
  Ref1[9].Types   := 2;
  Ref1[9].Size    := Matt * 0.017;
  Ref1[9].xp      := Round(-0.5112 * dx + xh);
  Ref1[9].yp      := Round(-0.5112 * dy + yh);
  Ref1[9].CCol.r  := 5.0 / 255.0;
  Ref1[9].CCol.g  := 5.0 / 255.0;
  Ref1[9].CCol.b  := 14.0 / 255.0;
  Ref1[10].Types  := 2;
  Ref1[10].Size   := Matt * 0.2;
  Ref1[10].xp     := Round(-1.496 * dx + xh);
  Ref1[10].yp     := Round(-1.496 * dy + yh);
  Ref1[10].CCol.r := 9.0 / 255.0;
  Ref1[10].CCol.g := 4.0 / 255.0;
  Ref1[10].CCol.b := 0.0;
  Ref1[11].Types  := 2;
  Ref1[11].Size   := Matt * 0.5;
  Ref1[11].xp     := Round(-1.496 * dx + xh);
  Ref1[11].yp     := Round(-1.496 * dy + yh);
  Ref1[11].CCol.r := 9.0 / 255.0;
  Ref1[11].CCol.g := 4.0 / 255.0;
  Ref1[11].CCol.b := 0.0;
  Ref1[12].Types  := 3;
  Ref1[12].Size   := Matt * 0.075;
  Ref1[12].xp     := Round(0.4487 * dx + xh);
  Ref1[12].yp     := Round(0.4487 * dy + yh);
  Ref1[12].CCol.r := 34.0 / 255.0;
  Ref1[12].CCol.g := 19.0 / 255.0;
  Ref1[12].CCol.b := 0.0;
  Ref1[13].Types  := 3;
  Ref1[13].Size   := Matt * 0.1;
  Ref1[13].xp     := dx + xh;
  Ref1[13].yp     := dy + yh;
  Ref1[13].CCol.r := 14.0 / 255.0;
  Ref1[13].CCol.g := 26.0 / 255.0;
  Ref1[13].CCol.b := 0.0;
  Ref1[14].Types  := 3;
  Ref1[14].Size   := Matt * 0.039;
  Ref1[14].xp     := Round(-1.301 * dx + xh);
  Ref1[14].yp     := Round(-1.301 * dy + yh);
  Ref1[14].CCol.r := 10.0 / 255.0;
  Ref1[14].CCol.g := 25.0 / 255.0;
  Ref1[14].CCol.b := 13.0 / 255.0;
  Ref1[15].Types  := 4;
  Ref1[15].Size   := Matt * 0.19;
  Ref1[15].xp     := Round(1.309 * dx + xh);
  Ref1[15].yp     := Round(1.309 * dy + yh);
  Ref1[15].CCol.r := 9.0 / 255.0;
  Ref1[15].CCol.g := 0.0;
  Ref1[15].CCol.b := 17.0 / 255.0;
  Ref1[16].Types  := 4;
  Ref1[16].Size   := Matt * 0.195;
  Ref1[16].xp     := Round(1.309 * dx + xh);
  Ref1[16].yp     := Round(1.309 * dy + yh);
  Ref1[16].CCol.r := 9.0 / 255.0;
  Ref1[16].CCol.g := 16.0 / 255.0;
  Ref1[16].CCol.b := 5.0 / 255.0;
  Ref1[17].Types  := 4;
  Ref1[17].Size   := Matt * 0.20;
  Ref1[17].xp     := Round(1.309 * dx + xh);
  Ref1[17].yp     := Round(1.309 * dy + yh);
  Ref1[17].CCol.r := 17.0 / 255.0;
  Ref1[17].CCol.g := 4.0 / 255.0;
  Ref1[17].CCol.b := 0.0;
  Ref1[18].Types  := 4;
  Ref1[18].Size   := Matt * 0.038;
  Ref1[18].xp     := Round(-1.301 * dx + xh);
  Ref1[18].yp     := Round(-1.301 * dy + yh);
  Ref1[18].CCol.r := 17.0 / 255.0;
  Ref1[18].CCol.g := 4.0 / 255.0;
  Ref1[18].CCol.b := 0.0;
end; { InitRef }

procedure mrt1(var AColor: TColor32; const i, Col, Row: Integer);
var
  Procent: Real;
begin
  Procent := Ref1[i].Size - Hypot(Ref1[i].xp - Col, Ref1[i].yp - Row);
  Procent := Procent / Ref1[i].Size;
  if Procent > 0.0 then
  begin
    Procent := Procent * Procent;
    FixPix(AColor, Procent, Ref1[i].CCol);
  end;
end; { mrt1 }

procedure mrt2(var AColor: TColor32; const i, Col, Row: Integer);
var
  Procent: Real;
begin
  Procent := Ref1[i].Size - Hypot(Ref1[i].xp - Col, Ref1[i].yp - Row);
  Procent := Procent / (Ref1[i].Size * 0.15);
  if Procent > 0.0 then
  begin
    if   Procent > 1.0
    then Procent := 1.0;
    FixPix(AColor, Procent, Ref1[i].CCol);
  end;
end; { mrt2 }

procedure mrt3(var AColor: TColor32; const i, Col, Row: Integer);
var
  Procent: Real;
begin
  Procent := Ref1[i].Size - Hypot(Ref1[i].xp - Col, Ref1[i].yp - Row);
  Procent := Procent / (Ref1[i].Size * 0.12);
  if Procent > 0.0 then
  begin
    if   Procent > 1.0
    then Procent := 1.0 - (Procent * 0.12);
    FixPix(AColor, Procent, Ref1[i].CCol);
  end;
end; { mrt3 }

procedure mrt4(var AColor: TColor32; const i, Col, Row: Integer);
var
  Procent: Real;
begin
  Procent := Hypot(Ref1[i].xp - Col, Ref1[i].yp - Row) - Ref1[i].Size;
  Procent := Procent / (Ref1[i].Size * 0.04);
  Procent := Abs(Procent);
  if   Procent < 1.0
  then FixPix(AColor, 1.0 - Procent, Ref1[i].CCol);
end; { mrt4 }

{ Filter Function: X, Y are the coordinates of flare center. }

procedure FlareFX32(Source, Dest: TBitmap32; const X, Y: Integer);
var
  Col, Row, Matt, i: Integer;
  Width, Height    : Integer;
  hyp              : Real;
  AColor           : TColor32;
  a                : Cardinal;
  r, g, b          : Integer;
  SRow, DRow       : PColor32Array;
begin
  if   Dest.Width <> Source.Width
  then Dest.Width := Source.Width;

  if   Dest.Height <> Source.Height
  then Dest.Height := Source.Height;

  Matt   := Source.Width;
  Width  := Source.Width;
  Height := Source.Height;

  SColor := Matt * 0.0375;
  SGlow  := Matt * 0.078125;
  SInner := Matt * 0.1796875;
  SOuter := Matt * 0.3359375;
  SHalo  := Matt * 0.084375;

  Color.r := 239.0/255.0;
  Color.g := 239.0/255.0;
  Color.b := 239.0/255.0;
  Glow.r  := 245.0/255.0;
  Glow.g  := 245.0/255.0;
  Glow.b  := 245.0/255.0;
  Inner.r := 255.0/255.0;
  Inner.g := 38.0/255.0;
  Inner.b := 43.0/255.0;
  Outer.r := 69.0/255.0;
  Outer.g := 59.0/255.0;
  Outer.b := 64.0/255.0;
  Halo.r  := 80.0/255.0;
  Halo.g  := 15.0/255.0;
  Halo.b  := 4.0/255.0;

  InitRef(X, Y, Width, Height, Matt);

  {Loop through the rows}
  for Row := 0 to Source.Height - 1 do
  begin
    SRow := Source.ScanLine[Row];
    DRow := Dest.ScanLine[Row];
    for Col := 0 to Source.Width - 1 do
    begin
      AColor := SRow^[Col];

      hyp := Hypot(Col - X, Row - Y);
      MColor(AColor, hyp);  // make color
      MGlow (AColor, hyp);  // make glow
      MInner(AColor, hyp);  // make inner
      MOuter(AColor, hyp);  // make outer
      MHalo (AColor, hyp);  // make halo

      for i := 0 to NumRef - 1 do
      begin
        case Ref1[i].Types of
          1: mrt1(AColor, i, Col, Row);
          2: mrt2(AColor, i, Col, Row);
          3: mrt3(AColor, i, Col, Row);
          4: mrt4(AColor, i, Col, Row);
        end;
      end;

      a := SRow^[Col] shr 24 and $FF;
      r := AColor    shr 16 and $FF;
      g := AColor    shr  8 and $FF;
      b := AColor           and $FF;

      DRow^[Col] := (a shl 24) or (r shl 16) or (g shl 8) or b;
    end;
  end;
end;

//==============================================================

procedure GlassTile32(a_Source, a_Dest: TBitmap32; const a_TileWidth, a_TileHeight: Integer);
var
  v_i, v_j                              : Integer;
  v_HalfTileW, v_xplus, v_xmitt, v_xoffs: Integer;
  v_HalfTileH, v_yplus, v_ymitt, v_yoffs: Integer;
  v_dx, v_sx, v_sy                      : Integer;
  v_SRows, v_DRows                      : array [0 .. 2047] of PColor32Array;
begin
  if   a_Dest.Width <> a_Source.Width
  then a_Dest.Width := a_Source.Width;
  if   a_Dest.Height <> a_Source.Height
  then a_Dest.Height := a_Source.Height;

  for v_j := 0 to a_Source.Height - 1 do
  begin
    v_SRows[v_j] := a_Source.ScanLine[v_j];
    v_DRows[v_j] := a_Dest.ScanLine[v_j];
  end;

  v_HalfTileW := a_TileWidth  div 2;
  v_HalfTileH := a_TileHeight div 2;
  v_xplus     := a_TileWidth  mod 2;
  v_yplus     := a_TileHeight mod 2;

  v_ymitt := 0;
  v_yoffs := 0;
  for v_j := 0 to a_Source.Height - 1 do
  begin
    v_sy := v_ymitt + v_yoffs * 2;
    Inc(v_yoffs);
    if   v_sy < 0
    then v_sy := 0;
    if   v_sy > (a_Source.Height - 1)
    then v_sy := a_Source.Height - 1;

    if v_yoffs = v_HalfTileH then
    begin
      v_ymitt := v_ymitt + a_TileHeight;
      v_yoffs := -v_HalfTileH;
      v_yoffs := v_yoffs - v_yplus;
    end;

    v_xmitt := 0;
    v_xoffs := 0;
    for v_i := 0 to a_Source.Width - 1 do
    begin
      v_dx := v_xmitt + v_xoffs;
      v_sx := v_xmitt + v_xoffs * 2;
      if   v_sx < 0
      then v_sx := 0;

      if   v_sx < (a_Source.Width - 1)
      then v_DRows[v_j, v_dx] := v_SRows[v_sy, v_sx]
      else v_DRows[v_j, v_dx] := v_SRows[v_sy, v_dx];

      Inc(v_xoffs);
      if v_xoffs = v_HalfTileW then
      begin
        v_xmitt := v_xmitt + a_TileWidth;
        v_xoffs := -v_HalfTileW;
        v_xoffs := v_Xoffs - v_xplus;
      end;
    end;
  end;
end;

end.
