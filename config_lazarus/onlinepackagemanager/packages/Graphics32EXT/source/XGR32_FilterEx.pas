
{**********************************************************************
 Package pl_Graphics32EXT.pkg
 this unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit XGR32_FilterEx;

{$MODE Delphi}

interface

uses

  LCLIntf, LCLType,
  Classes, SysUtils,Math,
  GR32,
  GR32_Blend,
  GR32_Filters,
  GR32_Resamplers,
  GR32_Math,
  XGR32_GraphUtils;

type
  TColorFunc = function(Color32: TColor32): Integer;
  TColorMode = (cmGray,cmRed,cmGreen,cmBlue,cmColor);
  TMatrix3X3 = array [0..3*3-1] of integer;
  TMatrix5X5 = array [0..5*5-1] of integer;
  TConvolution3X3 = packed record
    Ray: TMatrix3X3;
    Z: Integer; //Division Ύν»ύ
    Offset: Integer; //Bias
  end;
  TConvolution5X5 = packed record
    Ray: TMatrix5X5;
    Z: Integer; //Division
    Offset: Integer; //Bias
  end;

const

  {*** Laplacian filters ***}
  LaplaceFilter3x3 :TConvolution3X3 = (
    Ray: (
      -1, -1, -1,
      -1,  8, -1,
      -1, -1, -1
    );
    Z: 1;
    Offset: 0
  );

  //Laplace - Origin
  LaplaceOriginFilter3x3 :TConvolution3X3 = (
    Ray: (
      -1, -1, -1,
      -1,  7, -1,
      -1, -1, -1
    );
    Z: 1;
    Offset: 0
  );

  LAP1Filter3x3: TConvolution3X3 = (
    Ray: (
       0,  1,  0,
       1, -4,  1,
       0,  1,  0
    );
    Z: 1;
    Offset: 0
  );

  LAP2Filter3x3: TConvolution3X3 = (
    Ray: (-1, -1, -1, -1,  8, -1, -1, -1, -1);
    Z: 1;
    Offset: 0
  );

  LAP3Filter3x3: TConvolution3X3 = (
    Ray: (
      1, -2,  1,
     -2,  4, -2,
      1, -2,  1
    );
    Z: 1;
    Offset: 0
  );
  //Diagonal Laplace
  LaplaceDiagonalFilter3x3: TConvolution3X3 = (
    Ray: (-1,  0, -1,  0,  4,  0, -1,  0, -1);
    Z: 1;
    Offset: 0
  );
  //Horizontal Laplace
  LaplaceHorizontalFilter3x3: TConvolution3X3 = (
    Ray: (0, -1,  0,  0,  2,  0,  0, -1,  0);
    Z: 1;
    Offset: 0
  );
  //Vertical Laplace
  LaplaceVerticalFilter3x3: TConvolution3X3 = (
    Ray: (0,  0,  0, -1,  2, -1,  0,  0,  0);
    Z: 1;
    Offset: 0
  );

  {*** Edge enhancment and detection filters ***}
  //Find Edges (top down)
  FindEdgeTopDownFilter3x3 :TConvolution3X3 = (
    Ray: (
       1,  1,  1,
       1, -2,  1,
      -1, -1, -1
    );
    Z: 1;
    Offset: 0
  );

  //Find Edges (Bottom top)
  FindEdgeBottomTopFilter3x3: TConvolution3X3 = (
    Ray: (
      -1, -1, -1,
       1, -2,  1,
       1,  1,  1
    );
    Z: 1;
    Offset: 0
  );

  {*** Low pass filters ***}
  BlurFilter3x3: TConvolution3X3 = (
    Ray: (
       3,  3,  3,
       3,  8,  3,
       3,  3,  3
    );
    Z: 32;
    Offset: 0
  );

  GaussianBlureFilter3x3: TConvolution3X3 = (
    Ray: (
       1,  2,  1,
       2,  4,  2,
       1,  2,  1
    );
    Z: 16;
    Offset: 0
  );
  //Average
  LowPassFilter3x3: TConvolution3X3 = (
    Ray: (1,  1,  1,  1,  1,  1,  1,  1,  1);
    Z: 9;
    Offset: 0
  );

  LowPass1Filter3x3: TConvolution3X3 = (
    Ray: (1,  1,  1,  1,  2,  1,  1,  1,  1);
    Z: 10;
    Offset: 0
  );

  LowPass2Filter3x3: TConvolution3X3 = (
    Ray: (1,  1,  1,  1,  4,  1,  1,  1,  1);
    Z: 12;
    Offset: 0
  );

  LowPass3Filter3x3: TConvolution3X3 = (
    Ray: (1,  1,  1,  1, 12,  1,  1,  1,  1);
    Z: 20;
    Offset: 0
  );

  {*** High pass filters ***}
  //Mean removal Hi pass
  HipassFilter3x3: TConvolution3X3 = (
    Ray: (
      -1, -1, -1,
      -1,  9, -1,
      -1, -1, -1
    );
    Z: 1;
    Offset: 0
  );

  HiPass1Filter3x3: TConvolution3X3 = (
    Ray: (0, -1,  0, -1,  5, -1,  0, -1,  0);
    Z: 1;
    Offset: 0
  );

  HiPass2Filter3x3: TConvolution3X3 = (
    Ray: (1, -2,  1, -2,  5, -2,  1, -2,  1);
    Z: 1;
    Offset: 0
  );

  HiPass3Filter3x3: TConvolution3X3 = (
    Ray: (0, -1,  0, -1, 20, -1,  0, -1,  0);
    Z: 16;
    Offset: 0
  );

  SharpenFilter3x3: TConvolution3X3 = (
    Ray: (
      -1, -1, -1,
      -1, 16, -1,
      -1, -1, -1
    );
    Z: 8;
    Offset: 0
  );
  EdgeEnhanceFilter3x3: TConvolution3X3 = (
    Ray: (
       0, -1,  0,
      -1,  5, -1,
       0, -1,  0
    );
    Z: 1;
    Offset: 0
  );

  //Color Emboss (Sorta)
  ColorEmbossFilter3x3: TConvolution3X3 = (
    Ray: (
       1,  0,  1,
       0,  0,  0,
       1,  0, -2
    );
    Z: 1;
    Offset: 0
  );
  SoftenFilter3x3: TConvolution3X3 = (
    Ray: (
       2,  2,  2,
       2,  0,  2,
       2,  2,  2
    );
    Z: 16;
    Offset: 0
  );
  HP3Filter3x3: TConvolution3X3 = (
    Ray: (
       1, -2,  1,
      -2,  5, -2,
       1, -2,  1
    );
    Z: 1;
    Offset: 0
  );

  {*** Sobel edge detection and contour filters ***}
  //Sobel Hor
  SobelHorFilter3x3: TConvolution3X3 = (
    Ray: (
       1,  2,  1,
       0,  0,  0,
      -1, -2, -1
    );
    Z: 1;
    Offset: 0
  );

  //Sobel Vert
  SobelVertFilter3x3: TConvolution3X3 = (
    Ray: (
       1,  0, -1,
       2,  0, -2,
       1,  0, -1
    );
    Z: 1;
    Offset: 0
  );

  //Previt hor
  PrevitHorFilter3x3: TConvolution3X3 = (
    Ray: (
       1,  1,  1,
       0,  0,  0,
      -1, -1, -1
    );
    Z: 1;
    Offset: 0
  );

  //Previt Vert
  PrevitVertFilter3x3: TConvolution3X3 = (
    Ray: (
       1,  0, -1,
       1,  0, -1,
       1,  0, -1
    );
    Z: 1;
    Offset: 0
  );


  //####=== the 5x5 Convolve Filters ====####
  BigEdgeFilter5x5 :TConvolution5X5 = (
    Ray: (
      -2, -2, -2, -2, -2,
      -2, -3, -3, -3, -2,
      -2, -3, 53, -3, -2,
      -2, -3, -3, -3, -2,
      -2, -2, -2, -2, -2
    );
    Z: 1;
    Offset: 0
  );
  SoftenFilter5x5 :TConvolution5X5 = (
    Ray: (
       1,  1,  1,  1,  1,
       1,  1,  1,  1,  1,
       1,  1,  1,  1,  1,
       1,  1,  1,  1,  1,
       1,  1,  1,  1,  1
    );
    Z: 25;
    Offset: 0
  );
  //USM   :  final=2*original minus SD=0.7 Gauss blur
  USM07Filter5x5: TConvolution5X5 = (
    Ray: (
       0,  0, -1,  0,  0,
       0, -8,-21, -8,  0,
      -1,-21,229,-21, -1,
       0, -8,-21, -8,  0,
       0,  0, -1,  0,  0
    );
    Z: 179;
    Offset: 0
  );


{ Summary apply the color as transplarent to the Bitmap32 by riceball. }
procedure ApplyTransparentColor(const aBMP: TBitmap32; aTransparentColor: TColor32; const aBlendValue: Byte = 0; const aQuantity: byte = 0);

{ Summary add the aColor to the aBMP by riceball. }
procedure ApplyMiddleColor(const aBMP: TBitmap32; aColor: TColor32);

{ Summary Apply the Blue Channel as the alpha value in the Src.}
{ Description
  if aInvert then the Alpha value is $FF - BlueChannel Value
}
procedure ApplyBlueChannelToAlpha(Src: TBitmap32; aInvert: Boolean = false);

{ Summary apply the specified aAlpha of the the src bitmap's color to the dst bitmap }
procedure ColorToAlpha(Src, Dst: TBitmap32; aColor: TColor32; const aAlpha: Byte = 0);

{ Summry apply the Blue Channel value in the src to the Dst's Alpha value. }
procedure BlueChannelToAlpha(Dst, Src: TBitmap32);

{ Summary Generate a TwoColor(BlackWhite) Image from aSrcBMP to aDstBMP. }
{ Description
  Note: if you wanna transparent the aWhiteColor should be the transparent color!
}
procedure GenerateBWImage(const aSrcBMP: TBitmap32; aDstBMP: TBitmap32; aWhiteColor: TColor32 = clWhite32; const aBlackColor: TColor32 = clBlack32; const aQuantity: byte = 0);
procedure ApplyBWImage(const aSrcBMP: TBitmap32; aWhiteColor: TColor32 = clWhite32; aBlackColor: TColor32 = clBlack32; const aQuantity: byte = 0);

procedure SplitConvolve(Bmp:TBitmap32; Split, nw, ne, sw, se, m:Integer);
procedure SplitBlur(Bmp:TBitmap32;Split:Integer);
procedure BlurEffect3x3(Bmp:TBitmap32; BlurLen: Byte; Split: integer = 1);
//procedure BlurEffect5x5(Bmp:TBitmap32; BlurLen: Byte; Split: integer = 1);
procedure BlurEdges(Bmp:TBitmap32;Split:Integer);//SplitConvolve
procedure BleedEdges(Bmp:TBitmap32;Split:Integer);//SplitConvolve
procedure Sharpen(Bmp:TBitmap32;Split:Integer);//SplitConvolve
procedure SharpenMore(Bmp:TBitmap32;Split:Integer);//SplitConvolve
procedure EmbossEdges(Bmp:TBitmap32;Split:Integer);//SplitConvolve

procedure BlendColorEffect(bmp : Tbitmap32; color : TColor32; alpha:byte);
procedure LightenEffect(bmp : Tbitmap32; amount :integer);

procedure AddNoise(Bmp:TBitmap32;Amount:integer; Mono:Boolean = True);

{ Summary The mirror version of a 3 x 3 convolution.}
{ Description
 The 3 x 3 convolve uses the eight surrounding pixels as part of the
 calculation.  But, for the pixels on the edges, there is nothing to use
 for the top row values.  In other words, the leftmost pixel in the 3rd
 row, or scanline, has no pixels on its left to use in the calculations.
 I compensate for this by increasing the size of the bitmap by one pixel
 on top, left, bottom, and right.  The mirror version is used in an
 application that creates seamless tiles, so I copy the opposite sides to
 maintain the seamless integrity.  

Note: 
  this function writen by "Jens Weiermann" <wexmanAT@solidsoftwareDOT.de> base on the Harm's example.
}
procedure ConvolveM(ray : array of integer; z : word; aBmp : TBitmap32; Offset: Integer = 0);

{ Summary The Expand version of a 3 x 3 convolution.}
{ Description
 This approach is similar to the mirror version, except that it copies
 or duplicates the pixels from the edges to the same edge.  This is
 probably the best version if you're interested in quality, but don't need
 a tiled (seamless) image. 

Note: 
  this function writen by "Jens Weiermann" <wexmanAT@solidsoftwareDOT.de> base on the Harm's example.
}
procedure ConvolveE(ray : array of integer; z : word; aBmp : TBitmap32; Offset: Integer = 0);

{ Summary The Ignore (basic) version of a 3 x 3 convolution.}
{ Description
 The 3 x 3 convolve uses the eight surrounding pixels as part of the
 calculation.  But, for the pixels on the edges, there is nothing to use
 for the top row values.  In other words, the leftmost pixel in the 3rd
 row, or scanline, has no pixels on its left to use in the calculations.
 This version just ignores the outermost edge of the image, and doesn't
 alter those pixels at all.  Repeated applications of filters will
 eventually cause a pronounced 'border' effect, as those pixels never
 change but all others do. However, this version is simpler, and the
 logic is easier to follow.  It's the fastest of the three in this
 application, and works great if the 'borders' are not an issue. 

 @param EffectLen the effect length.

 The ConvolveI writen by Vladimir Vasilyev
 http://www.gamedev.narod.ru
 W-develop@mtu-net.ru
 Vladimir@tometric.ru

 And Modified by riceball


 References:
 Based on Harm's example of a 3 x 3 convolution using 24-bit bitmaps and scanline
 http://www.users.uswest.net/~sharman1/
 sharman1@uswest.net
}
procedure ConvolveI(ray : array of integer; z : word; aBmp : TBitmap32; Offset: Integer = 0; EffectLen: Byte = 255);overload;
procedure ConvolveI(aFilter: TConvolution3X3; aBmp : TBitmap32; EffectLen: Byte = 255);overload;
procedure ConvolveI5x5(ray : array of integer; z : word; aBmp : TBitmap32);overload;
procedure ConvolveI5x5(aFilter: TConvolution5X5; aBmp : TBitmap32);overload;
procedure Kuwahara5x5( aBmp : TBitmap32; ColorMode : TColorMode);

procedure EnhanceDetail(Dst, Src: TBitmap32; const EnhanceAmount: Integer); overload;
procedure EnhanceDetail(Dst, Src: TBitmap32); overload;


procedure RotateSize(Src, Dst: TBitmap32; Angle: Double);
procedure Rotate(Src, Dst: TBitmap32; Angle: Double);
procedure Rotozoom(Src, Dst: TBitmap32; Angle: Double; Zoom: Integer);
procedure QuickTransform(Src, Dst: TBitmap32; cx, cy, isin, icos: Integer);

implementation

const
  CSinCosTablePrecision = 1000;

var
  GSinTable1, GCosTable1, GSinTable2, GCosTable2: array[0..(360 * CSinCosTablePrecision) - 1] of Integer;

function CosTable1(Angle: Single): Integer;
begin
  Result := GCosTable1[Trunc(Angle * CSinCosTablePrecision)];
end;

function CosTable2(Angle: Single): Integer;
begin
  Result := GCosTable2[Trunc(Angle * CSinCosTablePrecision)];
end;


function SinTable1(Angle: Single): Integer;
begin
  Result := GSinTable1[Trunc(Angle * CSinCosTablePrecision)];
end;


function SinTable2(Angle: Single): Integer;
begin
  Result := GSinTable2[Trunc(Angle * CSinCosTablePrecision)];
end;

function Set255(Clr : integer) : integer;
begin
  result:=Clr;
  if Clr<1   then result:=0;
  if Clr>254 then result:=255;
end;

procedure ApplyTransparentColor(const aBMP: TBitmap32; aTransparentColor: TColor32; const aBlendValue: Byte; const aQuantity: byte);
var
  DstP: PColor32;
  DstColor: TColor32;
  I: integer;
begin
  if (aBMP = nil) or aBMP.Empty then Exit;
  aBMP.BeginUpdate;
  try
  aTransparentColor := aTransparentColor And $00FFFFFF;
  DstP := @aBMP.Bits[0];
  if aQuantity = 0 then
    For I := 0 To aBMP.Width * aBMP.Height - 1 Do
    Begin
      DstColor := Dstp^ And $00FFFFFF;
      If DstColor = aTransparentColor Then
        DstP^ := DstColor or (aBlendValue shl 24);
      Inc(DstP);
    End
  else
    For I := 0 To aBMP.Width * aBMP.Height - 1 Do
    Begin
      if (Abs(TColor32Rec(Dstp^).rgbRed - TColor32Rec(aTransparentColor).rgbRed) < aQuantity) and
         (Abs(TColor32Rec(Dstp^).rgbGreen - TColor32Rec(aTransparentColor).rgbGreen) < aQuantity) and
         (Abs(TColor32Rec(Dstp^).rgbBlue - TColor32Rec(aTransparentColor).rgbBlue) < aQuantity) 
      then
      begin
        DstColor := Dstp^ And $00FFFFFF;
        DstP^ := DstColor or (aBlendValue shl 24);
      end;
      Inc(DstP);
    End;
  finally
    aBMP.EndUpdate;
    aBMP.Changed;
  end;
end;

procedure ApplyMiddleColor(const aBMP: TBitmap32; aColor: TColor32);
var
  I: Integer;
  DstP: PColor32;
begin
  if (aBMP = nil) or aBMP.Empty then Exit;
  aBMP.BeginUpdate;
  try
    DstP := @aBMP.Bits[0];
    For I := 0 To aBMP.Width * aBMP.Height - 1 Do
    begin
      DStP^ := ColorAverage(DStP^, aColor);
      Inc(DstP);
    end;
    EMMS;
  finally
    aBMP.EndUpdate;
    aBMP.Changed;
  end;
end;

procedure ColorToAlpha(Src, Dst: TBitmap32; aColor: TColor32; const aAlpha: Byte);
var
  I: Integer;
  D, S: PColor32Rec;
  SrcColor: TColor32;
  TransparentColor: TColor32;
begin
  CheckParams(Dst, Src);
  Dst.SetSize(Src.Width, Src.Height);
  D := @Dst.Bits[0];
  S := @Src.Bits[0];
  TransparentColor := aColor And $00FFFFFF;
  Dst.BeginUpdate;
  try
  for I := 0 to Dst.Width * Dst.Height - 1 do
  begin
    SrcColor := S^.Color And $00FFFFFF;
    if SrcColor = TransparentColor then
      TColor32Rec(D^).rgbAlpha := aAlpha;
    Inc(S);
    Inc(D);
  end;
  finally
    Dst.EndUpdate;
    Dst.Changed;
  end;
end;

procedure ApplyBlueChannelToAlpha(Src: TBitmap32; aInvert: Boolean);
var
  I: Integer;
  S: PColor32;
begin
  S := @Src.Bits[0];
  Src.BeginUpdate;
  try
  if aInvert then
  for I := 0 to Src.Width * Src.Height - 1 do
  begin
    TColor32Rec(S^).rgbAlpha := $FF - TColor32Rec(S^).rgbBlue;
    Inc(S);
  end
  else
  for I := 0 to Src.Width * Src.Height - 1 do
  begin
    TColor32Rec(S^).rgbAlpha := TColor32Rec(S^).rgbBlue;
    Inc(S);
  end;
  finally
    Src.EndUpdate;
    Src.Changed;
  end;
end;

procedure BlueChannelToAlpha(Dst, Src: TBitmap32);
var
  I: Integer;
  D, S: PColor32Rec;
begin
  CheckParams(Dst, Src);
  Dst.SetSize(Src.Width, Src.Height);
  D := @Dst.Bits[0];
  S := @Src.Bits[0];
  for I := 0 to Src.Width * Src.Height - 1 do
  begin
    D^.rgbAlpha := S^.rgbBlue;
    Inc(S); Inc(D);
  end;
  Dst.Changed;
end;

procedure ApplyBWImage(const aSrcBMP: TBitmap32; aWhiteColor: TColor32 = clWhite32; aBlackColor: TColor32 = clBlack32; const aQuantity: byte = 0);
var
  SrcP: PColor32;
  SrcColor: TColor32;
  TransparentColor: TColor32;
  I: integer;
begin
  //check and set aDstBMP size as same as aSrcBMP
  if aSrcBMP.Empty then Exit;
  TransparentColor := aBlackColor And $00FFFFFF;
  aWhiteColor := aWhiteColor And $00FFFFFF;
  //aDstBMP.Clear(aBlackColor);
  SrcP := @aSrcBMP.Bits[0];
  if aQuantity = 0 then
    For I := 0 To aSrcBMP.Width * aSrcBMP.Height - 1 Do
    Begin
      SrcColor := SrcP^ And $00FFFFFF;
      If (SrcColor <> TransparentColor) and (TColor32Rec(SrcP^).rgbAlpha > 0) Then
        SrcP^ := aWhiteColor Or (SrcP^ And $FF000000)
      else
        SrcP^ := aBlackColor;
      Inc(SrcP);
    End
  else
    For I := 0 To aSrcBMP.Width * aSrcBMP.Height - 1 Do
    Begin
      SrcColor := SrcP^ And $00FFFFFF;
      if (SrcColor <> SrcP^) and 
         ((Abs(TColor32Rec(SrcColor).rgbRed - TColor32Rec(aWhiteColor).rgbRed) >= aQuantity) and
          (Abs(TColor32Rec(SrcColor).rgbGreen - TColor32Rec(aWhiteColor).rgbGreen) >= aQuantity) and
          (Abs(TColor32Rec(SrcColor).rgbBlue - TColor32Rec(aWhiteColor).rgbBlue) >= aQuantity))
      then
        SrcP^ := aWhiteColor Or (SrcP^ And $FF000000)
      else
        SrcP^ := aBlackColor;
      Inc(SrcP);
    End;
end;

procedure GenerateBWImage(const aSrcBMP: TBitmap32; aDstBMP: TBitmap32; aWhiteColor: TColor32; const aBlackColor: TColor32; const aQuantity: byte);
var
  SrcP, DstP: PColor32;
  SrcColor: TColor32;
  TransparentColor: TColor32;
  I: integer;
begin
  //check and set aDstBMP size as same as aSrcBMP
  CheckParams(aDstBMP, aSrcBMP);
  if aSrcBMP.Empty then Exit;
  TransparentColor := aBlackColor And $00FFFFFF;
  aWhiteColor := aWhiteColor And $00FFFFFF;
  aDstBMP.Clear(aBlackColor);
  SrcP := @aSrcBMP.Bits[0];
  DstP := @aDstBMP.Bits[0];
  if aQuantity = 0 then
    For I := 0 To aSrcBMP.Width * aSrcBMP.Height - 1 Do
    Begin
      SrcColor := SrcP^ And $00FFFFFF;
      If (SrcColor <> TransparentColor) and (TColor32Rec(SrcP^).rgbAlpha > 0) Then
        DstP^ := aWhiteColor Or (SrcP^ And $FF000000);
      Inc(DstP);
      Inc(SrcP);
    End
  else
    For I := 0 To aSrcBMP.Width * aSrcBMP.Height - 1 Do
    Begin
      SrcColor := SrcP^ And $00FFFFFF;
      if (SrcColor <> SrcP^) and 
         ((Abs(TColor32Rec(SrcColor).rgbRed - TColor32Rec(aWhiteColor).rgbRed) >= aQuantity) and
          (Abs(TColor32Rec(SrcColor).rgbGreen - TColor32Rec(aWhiteColor).rgbGreen) >= aQuantity) and
          (Abs(TColor32Rec(SrcColor).rgbBlue - TColor32Rec(aWhiteColor).rgbBlue) >= aQuantity))
      then
        DstP^ := aWhiteColor Or (SrcP^ And $FF000000);
      Inc(DstP);
      Inc(SrcP);
    End;
end;

//nw: NorthWest point; m: middle point
procedure SplitConvolve(Bmp:TBitmap32; Split, nw, ne, sw, se, m:Integer);
var
  Sum,n,s,e,w,i,x,y: Integer;
  aW, aH: Integer;
  Lin1,Lin2: PColor32RecArray;
  pc: PColor32Rec;
begin
  Sum:=nw+ne+sw+se+m;
  if Sum=0 then Sum:=1;
  pc:=@Bmp.Bits[0];
  aW := Bmp.Width-1;
  aH := Bmp.Height-1;
  for y:=0 to aH do
  begin
    n:=y+Split; if n > aH then n:=aH;
    s:=y-Split; if s < 0 then s:=0;
    Lin1:=@Bmp.Bits[s*Bmp.Width];
    Lin2:=@Bmp.Bits[n*Bmp.Width];
    for x:=0 to aW do
    begin
      e:=x+Split; if e> aW then e:= aW;
      w:=x-Split; if w<0 then w:=0;
      i:=(pc.rgbBlue*m+Lin1[w].rgbBlue*nw+Lin1[e].rgbBlue*ne+Lin2[w].rgbBlue*sw+Lin2[e].rgbBlue*se)div Sum;
      pc.rgbBlue := Set255(i);
      //if i>255 then pc.rgbBlue:=255 else if i<0 then pc.rgbBlue:=0 else pc.rgbBlue:=i;
      i:=(pc.rgbGreen*m+Lin1[w].rgbGreen*nw+Lin1[e].rgbGreen*ne+Lin2[w].rgbGreen*sw+Lin2[e].rgbGreen*se)div Sum;
      pc.rgbGreen := Set255(i);
      //if i>255 then pc.rgbGreen:=255 else if i<0 then pc.rgbGreen:=0 else pc.rgbGreen:=i;
      i:=(pc.rgbRed*m+Lin1[w].rgbRed*nw+Lin1[e].rgbRed*ne+Lin2[w].rgbRed*sw+Lin2[e].rgbRed*se)div Sum;
      pc.rgbRed := Set255(i);
      //if i>255 then pc.rgbRed:=255 else if i<0 then pc.rgbRed:=0 else pc.rgbRed:=i;
      Inc(pc);
    end;
  end;
end;


procedure BlurEffect3x3(Bmp:TBitmap32; BlurLen: Byte; Split: integer = 1);
var
  n,s,e,w,x,y: Integer;
  aW, aH: Integer;
  vSum: Integer;
  Lin1,CenterLine, Lin2: PColor32RecArray;
  pc: PColor32Rec;
begin
  pc:=@Bmp.Bits[0];
  aW := Bmp.Width-1;
  aH := Bmp.Height-1;
  for y:=0 to aH do
  begin
    n:=y+Split; if n>aH then n:=aH;
    s:=y-Split; if s<0 then s:=0;
    Lin1:=@Bmp.Bits[s*Bmp.Width]; //top line
    Lin2:=@Bmp.Bits[n*Bmp.Width]; //bottom line
    CenterLine := @Bmp.Bits[y*Bmp.Width]; //center line
    for x:=0 to aW do
    begin
      e:=x+Split; if e>aW then e:=aW;
      w:=x-Split; if w<0 then w:=0;
      if BlurLen = 255 then
      begin
        pc.rgbBlue:=(CenterLine[w].rgbBlue+CenterLine[e].rgbBlue+Lin1[w].rgbBlue+Lin1[e].rgbBlue+Lin2[w].rgbBlue+Lin2[e].rgbBlue) div 6;
        pc.rgbGreen:=(CenterLine[w].rgbGreen+CenterLine[e].rgbGreen+Lin1[w].rgbGreen+Lin1[e].rgbGreen+Lin2[w].rgbGreen+Lin2[e].rgbGreen) div 6;
        pc.rgbRed:=(CenterLine[w].rgbRed+CenterLine[e].rgbRed+Lin1[w].rgbRed+Lin1[e].rgbRed+Lin2[w].rgbRed+Lin2[e].rgbRed) div 6;
        pc.rgbAlpha:=(CenterLine[w].rgbAlpha+CenterLine[e].rgbAlpha+Lin1[w].rgbAlpha+Lin1[e].rgbAlpha+Lin2[w].rgbAlpha+Lin2[e].rgbAlpha) div 6;
      end
      else begin
        vSum := CenterLine[w].rgbBlue+CenterLine[e].rgbBlue+Lin1[w].rgbBlue+Lin1[e].rgbBlue+Lin2[w].rgbBlue+Lin2[e].rgbBlue;
        pc.rgbBlue:= (vSum div 6 - pc.rgbBlue) * BlurLen shr 8 + pc.rgbBlue;
        vSum := CenterLine[w].rgbGreen+CenterLine[e].rgbGreen+Lin1[w].rgbGreen+Lin1[e].rgbGreen+Lin2[w].rgbGreen+Lin2[e].rgbGreen;
        pc.rgbGreen:=(vSum div 6 - pc.rgbGreen) * BlurLen shr 8 + pc.rgbGreen;
        vSum := CenterLine[w].rgbRed+CenterLine[e].rgbRed+Lin1[w].rgbRed+Lin1[e].rgbRed+Lin2[w].rgbRed+Lin2[e].rgbRed;
        pc.rgbRed:= (vSum div 6 - pc.rgbRed) * BlurLen shr 8 + pc.rgbRed;
        vSum := CenterLine[w].rgbAlpha+CenterLine[e].rgbAlpha+Lin1[w].rgbAlpha+Lin1[e].rgbAlpha+Lin2[w].rgbAlpha+Lin2[e].rgbAlpha;
        pc.rgbAlpha:= (vSum div 6 - pc.rgbAlpha) * BlurLen shr 8 + pc.rgbAlpha;
      end;
      Inc(pc);
    end;
  end;
end;

procedure SplitBlur(Bmp:TBitmap32;Split:Integer);
var
  n,s,e,w,x,y: Integer;
  aW, aH: Integer;
  Lin1,Lin2: PColor32RecArray;
  pc: PColor32Rec;
begin
  pc:=@Bmp.Bits[0];
  aW := Bmp.Width-1;
  aH := Bmp.Height-1;
  for y:=0 to aH do
  begin
    n:=y+Split; if n>aH then n:=aH;
    s:=y-Split; if s<0 then s:=0;
    Lin1:=@Bmp.Bits[s*Bmp.Width];
    Lin2:=@Bmp.Bits[n*Bmp.Width];
    for x:=0 to aW do
    begin
      e:=x+Split; if e>aW then e:=aW;
      w:=x-Split; if w<0 then w:=0;
      pc.rgbBlue:=(Lin1[w].rgbBlue+Lin1[e].rgbBlue+Lin2[w].rgbBlue+Lin2[e].rgbBlue)shr 2;
      pc.rgbGreen:=(Lin1[w].rgbGreen+Lin1[e].rgbGreen+Lin2[w].rgbGreen+Lin2[e].rgbGreen)shr 2;
      pc.rgbRed:=(Lin1[w].rgbRed+Lin1[e].rgbRed+Lin2[w].rgbRed+Lin2[e].rgbRed)shr 2;
      pc.rgbAlpha:=(Lin1[w].rgbAlpha+Lin1[e].rgbAlpha+Lin2[w].rgbAlpha+Lin2[e].rgbAlpha)shr 2; //}
      Inc(pc);
    end;
  end;
end;

procedure BlurEdges(Bmp:TBitmap32;Split:Integer);
begin
  SplitConvolve(Bmp,Split,2,2,2,2,-3);
end;

procedure BleedEdges(Bmp:TBitmap32;Split:Integer);
begin
  SplitConvolve(Bmp,Split,2,2,2,2,-4);
end;

procedure Sharpen(Bmp:TBitmap32;Split:Integer);
begin
  SplitConvolve(Bmp,Split,1,1,1,1,-10);
end;

procedure SharpenMore(Bmp:TBitmap32;Split:Integer);
begin
  SplitConvolve(Bmp,Split,5,5,5,5,-35);
end;

procedure EmbossEdges(Bmp:TBitmap32;Split:Integer);
begin
  SplitConvolve(Bmp,Split,4,4,-4,-4,10);
end;

procedure AddNoise(Bmp:TBitmap32;Amount:integer; Mono:Boolean);
var
  s,a,I: Integer;
  pa: PColor32Rec;
begin
  s:=Amount shr 1;
  pa:=@Bmp.Bits[0];
  if Mono then
  begin
    for I:=0 to Bmp.Width*Bmp.Height-1 do
    begin
        a:=Random(Amount+1)-s;
        pa.rgbBlue := Set255(pa.rgbBlue+a);
        pa.rgbGreen := Set255(pa.rgbGreen+a);
        pa.rgbRed := Set255(pa.rgbRed + a);
        Inc(pa);
    end;
  end else
  begin
    for I:=0 to Bmp.Width*Bmp.Height-1 do
    begin
        a:=Random(Amount+1)-s;
        pa.rgbBlue := Set255(pa.rgbBlue + a);
        a:=Random(Amount+1)-s;
        pa.rgbGreen := Set255(pa.rgbGreen + a);
        a:=Random(Amount+1)-s;
        pa.rgbRed := Set255(pa.rgbRed + a);
        Inc(pa);
    end;
  end;
end;

procedure ConvolveM(ray : array of integer; z : word; aBmp : TBitmap32; Offset: Integer);
var
  O, T, C, B : PColor32RecArray;  // Scanlines
  x, y : integer;
  tBufr : TBitmap32; // temp bitmap for 'enlarged' image
  pColor: PColor32Rec;

  //Red,Green,Blue     : Integer;
begin
  tBufr := TBitmap32.Create;
 try
  tBufr.Width  := aBmp.Width+2;  // Add a box around the outside...
  tBufr.Height := aBmp.Height+2;
//  tBufr.PixelFormat := pf24bit;
  O := PColor32RecArray(tBufr.ScanLine[0]);   // Copy top corner pixels
  T := PColor32RecArray(aBmp.ScanLine[0]);
  O[0] := T[0];  // Left
  O[tBufr.Width - 1] := T[aBmp.Width - 1];  // Right

  // Copy bottom line to our top - trying to remain seamless...
   BlockTransfer(tBufr, 1, 0, tBufr.BoundsRect, aBmp, RECT(0,aBmp.Height - 1,aBmp.Width,aBmp.Height-2), dmOpaque);

  O := PColor32RecArray(tBufr.ScanLine[tBufr.Height - 1]); // Copy bottom corner pixels
  T := PColor32RecArray(aBmp.ScanLine[aBmp.Height - 1]);
  O[0] := T[0];
  O[tBufr.Width - 1] := T[aBmp.Width - 1];
  // Copy top line to our bottom
   BlockTransfer(tBufr, 1,tBufr.Height-1, tBufr.BoundsRect, aBmp, RECT(0,0,aBmp.Width,1), dmOpaque);

  // Copy left to our right
   BlockTransfer(tBufr, tBufr.Width-1, 1, tBufr.BoundsRect, aBmp, RECT(0,0,1,aBmp.Height), dmOpaque);

  // Copy right to our left
   BlockTransfer(tBufr, 0, 1, tBufr.BoundsRect, aBmp, RECT(aBmp.Width - 1,0,aBmp.Width,aBmp.Height), dmOpaque);

  // Now copy main rectangle
   BlockTransfer(tBufr, 1, 1, tBufr.BoundsRect, aBmp, RECT(0,0,aBmp.Width,aBmp.Height), dmOpaque);

  // bmp now enlarged and copied, apply convolve
  for x := 0 to aBmp.Height - 1 do begin  // Walk scanlines
    O := PColor32RecArray(aBmp.ScanLine[x]);      // New Target (Original)
    T := PColor32RecArray(tBufr.ScanLine[x]);     //old x-1  (Top)
    C := PColor32RecArray(tBufr.ScanLine[x+1]);   //old x    (Center)
    B := PColor32RecArray(tBufr.ScanLine[x+2]);   //old x+1  (Bottom)
  // Now do the main piece
    for y := 1 to (tBufr.Width - 2) do begin  // Walk pixels
      pColor := @O[y-1];
      pColor.rgbRed:=Set255(
          (
          (T[y-1].rgbRed * ray[0]) + (T[y].rgbRed * ray[1]) + (T[y+1].rgbRed * ray[2])+
          (C[y-1].rgbRed * ray[3]) + (C[y].rgbRed * ray[4]) + (C[y+1].rgbRed * ray[5])+
          (B[y-1].rgbRed * ray[6]) + (B[y].rgbRed * ray[7]) + (B[y+1].rgbRed * ray[8])
          ) div z + Offset);

      pColor.rgbGreen:=Set255(
          (
          (T[y-1].rgbGreen * ray[0]) + (T[y].rgbGreen * ray[1]) + (T[y+1].rgbGreen * ray[2])+
          (C[y-1].rgbGreen * ray[3]) + (C[y].rgbGreen * ray[4]) + (C[y+1].rgbGreen * ray[5])+
          (B[y-1].rgbGreen * ray[6]) + (B[y].rgbGreen * ray[7]) + (B[y+1].rgbGreen * ray[8])
          ) div z + Offset );

      pColor.rgbBlue:=Set255(
          (
          (T[y-1].rgbBlue * ray[0]) + (T[y].rgbBlue * ray[1]) + (T[y+1].rgbBlue * ray[2])+
          (C[y-1].rgbBlue * ray[3]) + (C[y].rgbBlue * ray[4]) + (C[y+1].rgbBlue * ray[5])+
          (B[y-1].rgbBlue * ray[6]) + (B[y].rgbBlue * ray[7]) + (B[y+1].rgbBlue * ray[8])
          ) div z + Offset );

      //O[y-1].Color := Color32(Red, Green, Blue);
    end;
  end;
 finally
  tBufr.Free;
 end;
end;

procedure ConvolveE(ray : array of integer; z : word; aBmp : TBitmap32; Offset: Integer);
var
  O, T, C, B : PColor32RecArray;  // Scanlines
  x, y : integer;
  tBufr : TBitmap32; // temp bitmap for 'enlarged' image
  pColor: PColor32Rec;

  //Red,Green,Blue     : Integer;
begin
  tBufr := TBitmap32.Create;
 try
  tBufr.Width  := aBmp.Width+2;  // Add a box around the outside...
  tBufr.Height := aBmp.Height+2;
//  tBufr.PixelFormat := pf24bit;
  O := PColor32RecArray(tBufr.ScanLine[0]);   // Copy top corner pixels
  T := PColor32RecArray(aBmp.ScanLine[0]);
  O[0] := T[0];  // Left
  O[tBufr.Width - 1] := T[aBmp.Width - 1];  // Right

  // Copy top lines
  BlockTransfer(tBufr, 1, 0, tBufr.BoundsRect, aBmp, RECT(0,0,aBmp.Width,1), dmOpaque);

  O := PColor32RecArray(tBufr.ScanLine[tBufr.Height - 1]); // Copy bottom corner pixels
  T := PColor32RecArray(aBmp.ScanLine[aBmp.Height - 1]);
  O[0] := T[0];
  O[tBufr.Width - 1] := T[aBmp.Width - 1];

  // Copy bottoms
   BlockTransfer(tBufr, 1, tBufr.Height-1, tBufr.BoundsRect, aBmp, RECT(0,aBmp.Height-1,aBmp.Width,aBmp.Height), dmOpaque);

  // Copy rights
   BlockTransfer(tBufr, tBufr.Width-1,1, tBufr.BoundsRect, aBmp, RECT(aBmp.Width-1,0,aBmp.Width,aBmp.Height), dmOpaque);

  // Copy lefts
   BlockTransfer(tBufr, 0, 1, tBufr.BoundsRect, aBmp, RECT(0,0,1,aBmp.Height), dmOpaque);

  // Now copy main rectangle
   BlockTransfer(tBufr, 1, 1, tBufr.BoundsRect, aBmp, RECT(0,0,aBmp.Width,aBmp.Height), dmOpaque);

  // bmp now enlarged and copied, apply convolve
  for x := 0 to aBmp.Height - 1 do begin  // Walk scanlines
    O := PColor32RecArray(aBmp.ScanLine[x]);      // New Target (Original)
    T := PColor32RecArray(tBufr.ScanLine[x]);     //old x-1  (Top)
    C := PColor32RecArray(tBufr.ScanLine[x+1]);   //old x    (Center)
    B := PColor32RecArray(tBufr.ScanLine[x+2]);   //old x+1  (Bottom)
  // Now do the main piece
    for y := 1 to (tBufr.Width-2) do begin  // Walk pixels
      pColor := @O[y-1];
      pColor.rgbRed:=Set255(
          (
          (T[y-1].rgbRed * ray[0]) + (T[y].rgbRed * ray[1]) + (T[y+1].rgbRed * ray[2])+
          (C[y-1].rgbRed * ray[3]) + (C[y].rgbRed * ray[4]) + (C[y+1].rgbRed * ray[5])+
          (B[y-1].rgbRed * ray[6]) + (B[y].rgbRed * ray[7]) + (B[y+1].rgbRed * ray[8])
          ) div z + Offset);

      pColor.rgbGreen:=Set255(
          (
          (T[y-1].rgbGreen * ray[0]) + (T[y].rgbGreen * ray[1]) + (T[y+1].rgbGreen * ray[2])+
          (C[y-1].rgbGreen * ray[3]) + (C[y].rgbGreen * ray[4]) + (C[y+1].rgbGreen * ray[5])+
          (B[y-1].rgbGreen * ray[6]) + (B[y].rgbGreen * ray[7]) + (B[y+1].rgbGreen * ray[8])
          ) div z + Offset);

      pColor.rgbBlue:=Set255(
          (
          (T[y-1].rgbBlue * ray[0]) + (T[y].rgbBlue * ray[1]) + (T[y+1].rgbBlue * ray[2])+
          (C[y-1].rgbBlue * ray[3]) + (C[y].rgbBlue * ray[4]) + (C[y+1].rgbBlue * ray[5])+
          (B[y-1].rgbBlue * ray[6]) + (B[y].rgbBlue * ray[7]) + (B[y+1].rgbBlue * ray[8])
          ) div z + Offset);

      //O[y-1].Color := Color32(Red, Green, Blue);
    end;
  end;
 finally
  tBufr.Free;
 end;
end;

procedure ConvolveI(aFilter: TConvolution3X3; aBmp : TBitmap32; EffectLen: Byte);
begin
  ConvolveI(aFilter.Ray, aFilter.Z, aBmp, aFilter.Offset, EffectLen);
end;

procedure ConvolveI(ray : array of integer; z : word; aBmp : TBitmap32; Offset: Integer = 0; EffectLen: Byte = 255);
var
  O, T, C, B : PColor32RecArray; // Scanlines
  x, y       : integer;
  vSum       : integer;
  tBufr      : TBitmap32; // temp bitmap

  //Red,Green,Blue     : Integer;
begin
  tBufr := TBitmap32.Create;
 try
  CheckParams(tBufr,aBmp);
  tBufr.Assign(aBmp);

  for x := 1 to aBmp.Height - 2 do begin  // Walk scanlines
    O := PColor32RecArray(aBmp.ScanLine[x]);       // New Target (Original)
    T := PColor32RecArray(tBufr.ScanLine[x-1]);    //old x-1  (Top)
    C := PColor32RecArray(tBufr.ScanLine[x]);      //old x    (Center)
    B := PColor32RecArray(tBufr.ScanLine[x+1]);    //old x+1  (Bottom)
  // Now do the main piece

    for y := 1 to (tBufr.Width - 2) do begin  // Walk pixels

      if EffectLen = 255 then
      begin
        O[y].rgbRed:=Set255(
          (
          (T[y-1].rgbRed * ray[0]) + (T[y].rgbRed * ray[1]) + (T[y+1].rgbRed * ray[2])+
          (C[y-1].rgbRed * ray[3]) + (C[y].rgbRed * ray[4]) + (C[y+1].rgbRed * ray[5])+
          (B[y-1].rgbRed * ray[6]) + (B[y].rgbRed * ray[7]) + (B[y+1].rgbRed * ray[8])
          ) div z + Offset);

        O[y].rgbGreen:=Set255(
          (
          (T[y-1].rgbGreen * ray[0]) + (T[y].rgbGreen * ray[1]) + (T[y+1].rgbGreen * ray[2])+
          (C[y-1].rgbGreen * ray[3]) + (C[y].rgbGreen * ray[4]) + (C[y+1].rgbGreen * ray[5])+
          (B[y-1].rgbGreen * ray[6]) + (B[y].rgbGreen * ray[7]) + (B[y+1].rgbGreen * ray[8])
          ) div z + Offset);

        O[y].rgbBlue:=Set255(
          (
          (T[y-1].rgbBlue * ray[0]) + (T[y].rgbBlue * ray[1]) + (T[y+1].rgbBlue * ray[2])+
          (C[y-1].rgbBlue * ray[3]) + (C[y].rgbBlue * ray[4]) + (C[y+1].rgbBlue * ray[5])+
          (B[y-1].rgbBlue * ray[6]) + (B[y].rgbBlue * ray[7]) + (B[y+1].rgbBlue * ray[8])
          ) div z + Offset);
      end
      else begin
        vSum :=
          (T[y-1].rgbRed * ray[0]) + (T[y].rgbRed * ray[1]) + (T[y+1].rgbRed * ray[2])+
          (C[y-1].rgbRed * ray[3]) + (C[y].rgbRed * ray[4]) + (C[y+1].rgbRed * ray[5])+
          (B[y-1].rgbRed * ray[6]) + (B[y].rgbRed * ray[7]) + (B[y+1].rgbRed * ray[8])
          ;
        O[y].rgbRed:=Set255(
          (vSum div z - O[y].rgbRed)* EffectLen shr 8 + O[y].rgbRed + Offset);

        vSum :=
          (T[y-1].rgbGreen * ray[0]) + (T[y].rgbGreen * ray[1]) + (T[y+1].rgbGreen * ray[2])+
          (C[y-1].rgbGreen * ray[3]) + (C[y].rgbGreen * ray[4]) + (C[y+1].rgbGreen * ray[5])+
          (B[y-1].rgbGreen * ray[6]) + (B[y].rgbGreen * ray[7]) + (B[y+1].rgbGreen * ray[8])
          ;
        O[y].rgbGreen:=Set255(
          (vSum div z - O[y].rgbGreen) * EffectLen shr 8 + O[y].rgbGreen + Offset);

        vSum :=
          (T[y-1].rgbBlue * ray[0]) + (T[y].rgbBlue * ray[1]) + (T[y+1].rgbBlue * ray[2])+
          (C[y-1].rgbBlue * ray[3]) + (C[y].rgbBlue * ray[4]) + (C[y+1].rgbBlue * ray[5])+
          (B[y-1].rgbBlue * ray[6]) + (B[y].rgbBlue * ray[7]) + (B[y+1].rgbBlue * ray[8])
          ;
        O[y].rgbBlue:=Set255(
          (vSum div z - O[y].rgbBlue) * EffectLen shr 8 + O[y].rgbBlue + Offset);
      end;
    end;
  end;
 finally
  tBufr.Free;
 end;
end;

procedure ConvolveI5x5(aFilter: TConvolution5X5; aBmp : TBitmap32);
begin
  ConvolveI(aFilter.Ray, aFilter.Z, aBmp, aFilter.Offset);
end;

procedure ConvolveI5x5(ray : array of integer; z : word; aBmp : TBitmap32);
var
  O, T,T2, C, B, B2 : PColor32RecArray; // Scanlines
  //NS,i              : integer;
  x, y              : integer;
  tBufr             : TBitmap32; // temp bitmap
  //Red,Green,Blue    : Integer;

begin
  tBufr := TBitmap32.Create;
 try
  CheckParams(tBufr,aBmp);
  tBufr.Assign(aBmp);

  for x := 2 to aBmp.Height - 3 do begin // Walk scanlines
    O := PColor32RecArray(aBmp.ScanLine[x]);     // New Target (Original)
    T2:= PColor32RecArray(tBufr.ScanLine[x-2]);  //old x-2  (Top)
    T := PColor32RecArray(tBufr.ScanLine[x-1]);  //old x-1  (Top)
    C := PColor32RecArray(tBufr.ScanLine[x]);    //old x    (Center)
    B := PColor32RecArray(tBufr.ScanLine[x+1]);  //old x+1  (Bottom)
    B2:= PColor32RecArray(tBufr.ScanLine[x+2]);  //old x+2  (Bottom)

  // Now do the main piece
    for y := 2 to (tBufr.Width - 3) do begin  // Walk pixels

    //NS:=0;
    //for i:=0 to 4 do

      O[y].rgbRed := Set255(
         (
T2[y-2].rgbRed * ray[0] + T2[y-1].rgbRed * ray[1] + T2[y].rgbRed * ray[2] + T2[y+1].rgbRed * ray[3] + T2[y+2].rgbRed *ray[4]+
T[y-2].rgbRed * ray[5] + T[y-1].rgbRed * ray[6] + T[y].rgbRed * ray[7] + T[y+1].rgbRed * ray[8] + T[y+2].rgbRed * ray[9]+
C[y-2].rgbRed * ray[10]+ C[y-1].rgbRed * ray[11]+ C[y].rgbRed * ray[12]+ C[y+1].rgbRed * ray[13]+ C[y+2].rgbRed * ray[14]+
B[y-2].rgbRed * ray[15]+ B[y-1].rgbRed * ray[16]+ B[y].rgbRed * ray[17]+ B[y+1].rgbRed * ray[18]+ B[y+2].rgbRed * ray[19]+
B2[y-2].rgbRed * ray[20]+ B2[y-1].rgbRed * ray[21]+ B2[y].rgbRed * ray[22]+ B2[y+1].rgbRed * ray[23]+ B2[y+2].rgbRed * ray[24]
          ) div z   );

      O[y].rgbBlue := Set255(
         (
T2[y-2].rgbBlue * ray[0] + T2[y-1].rgbBlue * ray[1] + T2[y].rgbBlue * ray[2] + T2[y+1].rgbBlue * ray[3] + T2[y+2].rgbBlue * ray[4]+
 T[y-2].rgbBlue * ray[5] +  T[y-1].rgbBlue * ray[6] +  T[y].rgbBlue * ray[7] +  T[y+1].rgbBlue * ray[8] +  T[y+2].rgbBlue * ray[9]+
 C[y-2].rgbBlue * ray[10]+  C[y-1].rgbBlue * ray[11]+  C[y].rgbBlue * ray[12]+  C[y+1].rgbBlue * ray[13]+  C[y+2].rgbBlue * ray[14]+
 B[y-2].rgbBlue * ray[15]+  B[y-1].rgbBlue * ray[16]+  B[y].rgbBlue * ray[17]+  B[y+1].rgbBlue * ray[18]+  B[y+2].rgbBlue * ray[19]+
B2[y-2].rgbBlue * ray[20]+ B2[y-1].rgbBlue * ray[21]+ B2[y].rgbBlue * ray[22]+ B2[y+1].rgbBlue * ray[23]+ B2[y+2].rgbBlue * ray[24]
          ) div z );

      O[y].rgbGreen := Set255(
         (
T2[y-2].rgbGreen * ray[0] + T2[y-1].rgbGreen * ray[1] + T2[y].rgbGreen * ray[2] + T2[y+1].rgbGreen * ray[3] + T2[y+2].rgbGreen * ray[4]+
 T[y-2].rgbGreen * ray[5] +  T[y-1].rgbGreen * ray[6] +  T[y].rgbGreen * ray[7] +  T[y+1].rgbGreen * ray[8] +  T[y+2].rgbGreen * ray[9]+
 C[y-2].rgbGreen * ray[10]+  C[y-1].rgbGreen * ray[11]+  C[y].rgbGreen * ray[12]+  C[y+1].rgbGreen * ray[13]+  C[y+2].rgbGreen * ray[14]+
 B[y-2].rgbGreen * ray[15]+  B[y-1].rgbGreen * ray[16]+  B[y].rgbGreen * ray[17]+  B[y+1].rgbGreen * ray[18]+  B[y+2].rgbGreen * ray[19]+
B2[y-2].rgbGreen * ray[20]+ B2[y-1].rgbGreen * ray[21]+ B2[y].rgbGreen * ray[22]+ B2[y+1].rgbGreen * ray[23]+ B2[y+2].rgbGreen * ray[24]
          ) div z    );

      //O[y].Color:=Color32(Red, Green, Blue);

    end;
  end;

 finally
  tBufr.Free;
 end;

end;


procedure Kuwahara5x5( aBmp : TBitmap32; ColorMode : TColorMode );
Var
  O, T,T2,C,B,B2  : PColor32Array; // Scanlines
  NS,i,j,k,n      : integer;
  x, y            : integer;
  tBufr           : TBitmap32; // temp bitmap
  Red,Green,Blue  : Integer;

  Region1         : array of Double;
  Region2         : array of Double;
  Region3         : array of Double;
  Region4         : array of Double;

  Mean            : array[1..4] of Extended;
  StdDev          : array[1..4] of Extended;

  minStdDev       : Extended;

  ColorFunc       : TColorFunc;
  Color           : array [0..3] of integer;
  step            : integer;
  c1,c2           : integer;
Begin

tBufr := TBitmap32.Create;
Try

  CheckParams(tBufr,aBmp);
  tBufr.Assign(aBmp);


  for x := 2 to aBmp.Height - 3 do
  begin
    O := aBmp.ScanLine[x];     // New Target (Original)

    T2:= tBufr.ScanLine[x-2];  //old x-2  (Top)
    T := tBufr.ScanLine[x-1];  //old x-1  (Top)
    C := tBufr.ScanLine[x];    //old x    (Center)
    B := tBufr.ScanLine[x+1];  //old x+1  (Bottom)
    B2:= tBufr.ScanLine[x+2];  //old x+2  (Bottom)


    //Now slide the region 5x5
    for y := 2 to (tBufr.Width - 3) do
     begin


          //fill regions array
          SetLength(Region1,6);
          SetLength(Region2,6);
          SetLength(Region3,6);
          SetLength(Region4,6);


          Case ColorMode of
             cmColor : begin
                        c1:=1;
                        c2:=3;
                       end;
             else
                        c1:=Ord(ColorMode);
                        c2:=Ord(ColorMode);
             end;

          for step:=c1 to c2 do
          begin

             case step of
               //0 : ColorFunc:=Intensity;
               1 : ColorFunc:=RedComponent;
               2 : ColorFunc:=GreenComponent;
               3 : ColorFunc:=BlueComponent;
               else ColorFunc:=Intensity;
             end;

             for i:=0 to 2 do
               for j:=0 to 1 do
                begin
                   k:=j*3+i;
                   case j of
                     0 : Region1[k]:=ColorFunc(T[y-i]);
                     1 : Region1[k]:=ColorFunc(T2[y-i]);
                   end;
                end;

             for i:=0 to 2 do
               for j:=0 to 1 do
                begin
                   k:=j*3+i;
                   case j of
                     0 : Region4[k]:=ColorFunc(B[y+i]);
                     1 : Region4[k]:=ColorFunc(B2[y+i]);
                   end;
                end;

             for i:=0 to 1 do
               for j:=0 to 2 do
                begin
                   k:=j*2+i;
                   case j of
                     0 : Region2[k]:=ColorFunc(C[y+i]);
                     1 : Region2[k]:=ColorFunc(T[y+i]);
                     2 : Region2[k]:=ColorFunc(T2[y+i]);
                   end;
                end;

             for i:=0 to 1 do
               for j:=0 to 2 do
                begin
                   k:=j*2+i;
                   case j of
                     0 : Region3[k]:=ColorFunc(C[y-i]);
                     1 : Region3[k]:=ColorFunc(B[y-i]);
                     2 : Region3[k]:=ColorFunc(B2[y-i]);
                   end;
                end;

             MeanAndStdDev(Region1, Mean[1], StdDev[1]);
             MeanAndStdDev(Region2, Mean[2], StdDev[2]);
             MeanAndStdDev(Region3, Mean[3], StdDev[3]);
             MeanAndStdDev(Region4, Mean[4], StdDev[4]);

             minStdDev:=StdDev[1];
             n:=1;
             for i:=2 to 4 do
              if StdDev[i]<minStdDev then
               begin
                 minStdDev:=StdDev[i];
                 n:=i;
               end;

             Color[step]:=Round(Mean[n]);

          end;//step

          Case ColorMode of
             cmGray  : O[y]:=Gray32( Color[0] );
             cmRed   : O[y]:=Color32(Color[1],0,0);
             cmGreen : O[y]:=Color32(0,Color[2],0);
             cmBlue  : O[y]:=Color32(0,0,Color[3]);
             cmColor : O[y]:=Color32(Color[1],Color[2],Color[3]);
          end;

    end;//y
  end;//x

finally
 tBufr.Free;
end;

end;


procedure BlendColorEffect(bmp : Tbitmap32; color : TColor32; alpha:byte);
var
   P           : PColor32Rec;
   I           : integer;
   sum1,sum2   : TFixed;
   CB,CR,CG    : Integer;
   oCB,oCR,oCG : Integer;
   nCB,nCR,nCG : Integer;
   //tempAlpha   : integer;
   change      : Integer;
begin
  CR := TColor32Rec(color).rgbRed;
  CG := TColor32Rec(color).rgbGreen;
  CB := TColor32Rec(color).rgbBlue;
  sum1 :=  FixedDiv((CB+CR+CG) shl 16, 3 shl 16);
  with bmp do begin
    try
      P := PColor32Rec(PixelPtr[0, 0]);
      for I := 0 to Width * Height - 1 do
      begin
        //tempAlpha := P^.rgbAlpha;
        if P^.rgbAlpha <> 0 then begin
          oCR    := P^.rgbRed;
          oCG    := P^.rgbGreen;
          oCB    := P^.rgbBlue;
          sum2   := FixedDiv((oCB+oCR+oCG) shl 16, 3 shl 16);
          change := FixedRound(sum2-sum1);
          nCR    := CR + change;
          nCG    := CG + change;
          nCB    := CB + change;
          nCR    := FixedRound(FixedMul(FixedDiv(alpha shl 16, 255 shl 16), nCR shl 16) + FixedMul(FixedDiv((255-alpha) shl 16, 255 shl 16),oCR shl 16));
          nCG    := FixedRound(FixedMul(FixedDiv(alpha shl 16, 255 shl 16), nCG shl 16) + FixedMul(FixedDiv((255-alpha) shl 16, 255 shl 16),oCG shl 16));
          nCB    := FixedRound(FixedMul(FixedDiv(alpha shl 16, 255 shl 16), nCB shl 16) + FixedMul(FixedDiv((255-alpha) shl 16, 255 shl 16),oCB shl 16));
          //nCB := Set255(nCB);
          //nCG := Set255(nCG);
          //nCR := Set255(nCR);
          //P^.Color := color32(nCR,nCG,nCB,tempAlpha);
          P^.rgbBlue := Set255(nCB);
          P^.rgbGreen := Set255(nCG);
          P^.rgbRed := Set255(nCR);
        end;
        Inc(P); // proceed to the next pixel
      end;
    finally
    end;
  end;
end;

procedure LightenEffect(bmp : Tbitmap32; amount :integer);
var
  P: PColor32;
  I : integer;
begin
  with bmp do
  begin
    P := PixelPtr[0, 0];
    for I := 0 to Width * Height - 1 do
    begin
      P^ := Lighten(P^,amount);
      Inc(P); // proceed to the next pixel
    end;
  end;
end;

const
  HPrewitt: array[0..8] of Integer = (-1, -1, -1,  0,  0,  0,  1,  1,  1);
  VPrewitt: array[0..8] of Integer = ( 1,  0, -1,  1,  0, -1,  1,  0, -1);

//EnhanceAmount: 0..255.  0 gives no effect, 255 maximum effect
procedure EnhanceDetail(Dst, Src: TBitmap32; const EnhanceAmount: Integer);
var
  i, x, y: Integer;
  PixelArray: array[0..9] of PColor32;
  Col, R, G, B, Rvert, Gvert, Bvert, Rhorz, Ghorz, Bhorz,
  Rorig, Gorig, Borig: Integer;
  Rcard, Gcard, Bcard: Cardinal;
  sOrig, sEnhance: Single;
begin
  if EnhanceAmount = 0 then begin
    Dst.Assign(Src);
    Exit;
  end else if EnhanceAmount = 255 then begin
    EnhanceDetail(Dst, Src);
    Exit;
  end;
  sOrig := (255 - EnhanceAmount) / 255;
  sEnhance := EnhanceAmount / 255;
  //Dst.Assign(Src);
  with Src do
    for y := 1 to Height - 2 do begin
      PixelArray[0] := PixelPtr[0, y - 1];
      PixelArray[1] := PixelPtr[1, y - 1];
      PixelArray[2] := PixelPtr[2, y - 1];
      PixelArray[3] := PixelPtr[0, y];
      PixelArray[4] := PixelPtr[1, y];
      PixelArray[5] := PixelPtr[2, y];
      PixelArray[6] := PixelPtr[0, y + 1];
      PixelArray[7] := PixelPtr[1, y + 1];
      PixelArray[8] := PixelPtr[2, y + 1];
      PixelArray[9] := Dst.PixelPtr[1, y];
      for x := 1 to Width - 2 do begin
        Rhorz := 0;
        Ghorz := 0;
        Bhorz := 0;
        Rvert := 0;
        Gvert := 0;
        Bvert := 0;
        Col := PixelArray[4]^;
        Rorig := (Col and $00FF0000) shr 16;
        Gorig := (Col and $0000FF00) shr 8;
        Borig := Col and $000000FF;
        for i := 0 to 8 do begin
          Col := PixelArray[i]^;
          R := (Col and $00FF0000) shr 16;
          G := (Col and $0000FF00) shr 8;
          B := Col and $000000FF;
          Rhorz := Rhorz + R * HPrewitt[i];
          Ghorz := Ghorz + G * HPrewitt[i];
          Bhorz := Bhorz + B * HPrewitt[i];
          Rvert := Rvert + R * VPrewitt[i];
          Gvert := Gvert + G * VPrewitt[i];
          Bvert := Bvert + B * VPrewitt[i];
        end;
        if Rhorz > 255 then Rhorz := 255
        else if Rhorz < 0 then Rhorz := 0;
        if Ghorz > 255 then Ghorz := 255
        else if Ghorz < 0 then Ghorz := 0;
        if Bhorz > 255 then Bhorz := 255
        else if Bhorz < 0 then Bhorz := 0;
        if Rvert > 255 then Rvert := 255
        else if Rvert < 0 then Rvert := 0;
        if Gvert > 255 then Gvert := 255
        else if Gvert < 0 then Gvert := 0;
        if Bvert > 255 then Bvert := 255
        else if Bvert < 0 then Bvert := 0;
        R := Rorig - Round(Sqrt(Rhorz * Rhorz + Rvert * Rvert));
        G := Gorig - Round(Sqrt(Ghorz * Ghorz + Gvert * Gvert));
        B := Borig - Round(Sqrt(Bhorz * Bhorz + Bvert * Bvert));
        if R > 255 then R := 255
        else if R < 0 then R := 0;
        if G > 255 then G := 255
        else if G < 0 then G := 0;
        if B > 255 then B := 255
        else if B < 0 then B := 0;
        Rorig := Round(Rorig * sOrig);
        Gorig := Round(Gorig * sOrig);
        Borig := Round(Borig * sOrig);
        R := Round(R * sEnhance);
        G := Round(G * sEnhance);
        B := Round(B * sEnhance);
        Rcard := Rorig + R;
        Gcard := Gorig + G;
        Bcard := Borig + B;
        PixelArray[9]^ := $FF000000 + Rcard shl 16 + Gcard shl 8 + Bcard;
        for i := 0 to 9 do
          Inc(PixelArray[i]);
      end;
    end;
end;

procedure EnhanceDetail(Dst, Src: TBitmap32);
var
  i, x, y: Integer;
  PixelArray: array[0..9] of PColor32;
  Col, R, G, B, Rvert, Gvert, Bvert, Rhorz, Ghorz, Bhorz,
  Rorig, Gorig, Borig: Integer;
  Rcard, Gcard, Bcard: Cardinal;
begin
  //Dst.Assign(Src);
  with Src do
    for y := 1 to Height - 2 do begin
      PixelArray[0] := PixelPtr[0, y - 1];
      PixelArray[1] := PixelPtr[1, y - 1];
      PixelArray[2] := PixelPtr[2, y - 1];
      PixelArray[3] := PixelPtr[0, y];
      PixelArray[4] := PixelPtr[1, y];
      PixelArray[5] := PixelPtr[2, y];
      PixelArray[6] := PixelPtr[0, y + 1];
      PixelArray[7] := PixelPtr[1, y + 1];
      PixelArray[8] := PixelPtr[2, y + 1];
      PixelArray[9] := Dst.PixelPtr[1, y];
      for x := 1 to Width - 2 do begin
        Rhorz := 0;
        Ghorz := 0;
        Bhorz := 0;
        Rvert := 0;
        Gvert := 0;
        Bvert := 0;
        Col := PixelArray[4]^;
        Rorig := (Col and $00FF0000) shr 16;
        Gorig := (Col and $0000FF00) shr 8;
        Borig := Col and $000000FF;
        for i := 0 to 8 do begin
          Col := PixelArray[i]^;
          R := (Col and $00FF0000) shr 16;
          G := (Col and $0000FF00) shr 8;
          B := Col and $000000FF;
          Rhorz := Rhorz + R * HPrewitt[i];
          Ghorz := Ghorz + G * HPrewitt[i];
          Bhorz := Bhorz + B * HPrewitt[i];
          Rvert := Rvert + R * VPrewitt[i];
          Gvert := Gvert + G * VPrewitt[i];
          Bvert := Bvert + B * VPrewitt[i];
        end;
        if Rhorz > 255 then Rhorz := 255
        else if Rhorz < 0 then Rhorz := 0;
        if Ghorz > 255 then Ghorz := 255
        else if Ghorz < 0 then Ghorz := 0;
        if Bhorz > 255 then Bhorz := 255
        else if Bhorz < 0 then Bhorz := 0;
        if Rvert > 255 then Rvert := 255
        else if Rvert < 0 then Rvert := 0;
        if Gvert > 255 then Gvert := 255
        else if Gvert < 0 then Gvert := 0;
        if Bvert > 255 then Bvert := 255
        else if Bvert < 0then Bvert := 0;
        R := Rorig - Round(Sqrt(Rhorz * Rhorz + Rvert * Rvert));
        G := Gorig - Round(Sqrt(Ghorz * Ghorz + Gvert * Gvert));
        B := Borig - Round(Sqrt(Bhorz * Bhorz + Bvert * Bvert));
        if R > 255 then Rcard := 255
        else if R < 0 then Rcard := 0
        else Rcard := R;
        if G > 255 then Gcard := 255
        else if G < 0 then Gcard := 0
        else Gcard := G;
        if B > 255 then Bcard := 255
        else if B < 0 then Bcard := 0
        else Bcard := B;
        PixelArray[9]^ := $FF000000 + Rcard shl 16 + Gcard shl 8 + Bcard;
        for i := 0 to 9 do
          Inc(PixelArray[i]);
      end;
    end;
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

procedure QuickTransform(Src, Dst: TBitmap32; cx, cy, isin, icos: Integer);
var
  X, Y, t1, t2, dx, dy, xd, yd, sdx, sdy, ax, ay, ex, ey: Integer;
  c00, c01, c10, c11: TColor32;
  pc, sp: PColor32;
begin
  xd := ((Src.Width shl 16) - (Dst.Width shl 16)) div 2;
  yd := ((Src.Height shl 16) - (Dst.Height shl 16)) div 2;
  ax := (cx shl 16) - (icos * cx);
  ay := (cy shl 16) - (isin * cx);
  pc := PColor32(Dst.Bits);
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
        pc^ := Src[dx, dy];
      inc(sdx, icos);
      inc(sdy, isin);
      inc(pc);
    end;
    //pc := Ptr(Integer(pc));
  end;
end;

procedure Rotate(Src, Dst: TBitmap32; Angle: Double);
begin
  QuickTransform(Src, Dst,
    Dst.Width shr 1,
    Dst.Height shr 1,
    Round(Sin(Angle * Pi / 180) * 65536),
    Round(Cos(Angle * Pi / 180) * 65536)
    );
end;

procedure Rotozoom(Src, Dst: TBitmap32; Angle: Double; Zoom: Integer);
begin
  QuickTransform(Src, Dst,
    Dst.Width shr 1,
    Dst.Height shr 1,
    Round(Sin(Angle * Pi / 180) * Zoom),
    Round(Cos(Angle * Pi / 180) * Zoom)
    );
end;

{==================INITIALIZATION====================}

procedure SetupFunctions;
var
  X: Integer;
  Angle: Extended;

begin
  for X := 0 to (360 * CSinCosTablePrecision) - 1 do
  begin
    Angle := X * pi / ((360 * CSinCosTablePrecision) / 2);
    GSinTable1[X] := Ceil(Sin(Angle) * 65536);
    GCosTable1[X] := Ceil(Cos(Angle) * 65536);
    GSinTable2[X] := Ceil(Sin(Angle + (pi / 2)) * 65536);
    GCosTable2[X] := Ceil(Cos(Angle + (pi / 2)) * 65536);
  end;
end;


initialization
  SetupFunctions;
end.
