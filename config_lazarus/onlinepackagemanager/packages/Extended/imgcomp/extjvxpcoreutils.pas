unit extjvxpcoreutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, Graphics, LCLIntf, LCLType, SysUtils,
  TypInfo, JvXPCore, FPCanvas, FPImage,JvXPCoreUtils;

const
  { color constants.

    these constants are used as default colors for descendant controls
    and may be replaced with other (common) values.

    syntax: JvXPColor_[Control]_[Enabled: Enb, Dis]_[Type]_[Theme: WXP, OXP]     }

  { button colors - WindowsXP }
  dxColor_Btn_Back = TColor(clWindow and $00777777); // border line
  dxColor_Btn_Enb_Border_WXP = TColor(($00733700 and $00777777) + dxColor_Btn_Back);
  // border line
  dxColor_Btn_Dis_Border_WXP = TColor(($00BDC7CE and $00777777) + dxColor_Btn_Back);
  // border line (disabled)
  dxColor_Btn_Enb_Edges_WXP = TColor(($00AD9E7B and $00777777) + dxColor_Btn_Back); // border edges
  dxColor_Btn_Dis_Edges_WXP = TColor(($00BDC7CE and $00777777) + dxColor_Btn_Back); // border edges (disabled)
  dxColor_Btn_Enb_BgFrom_WXP = clWindow; // background from
  dxColor_Btn_Enb_BgTo_WXP = TColor(($00E7EBEF and $00777777) + dxColor_Btn_Back); // background to
  dxColor_Btn_Enb_CkFrom_WXP = TColor(($00C6CFD6 and $00777777) + dxColor_Btn_Back); // clicked from
  dxColor_Btn_Enb_CkTo_WXP = TColor(($00EBF3F7 and $00777777) + dxColor_Btn_Back); // clicked to
  dxColor_Btn_Enb_FcFrom_WXP = TColor(($00FFE7CE and $00777777) + dxColor_Btn_Back); // focused from
  dxColor_Btn_Enb_FcTo_WXP = TColor(($00EF846D and $00777777) + dxColor_Btn_Back); // focused to
  dxColor_Btn_Enb_HlFrom_WXP = TColor(($00CEF3FF and $00777777) + dxColor_Btn_Back); // highlight from
  dxColor_Btn_Enb_HlTo_WXP = TColor(($000096E7 and $00777777) + dxColor_Btn_Back); // highlight to

  { checkbox colors - WindowsXP }
  dxColor_Chk_Enb_Border_WXP = TColor($00845118); // border line
  dxColor_Chk_Enb_NmSymb_WXP = TColor($0021A621); // symbol normal
  dxColor_Chk_Enb_GraSymb_WXP = TColor($0071C671); // symbol grayed

  { misc colors - WindowsXP }
  dxColor_Msc_Dis_Caption_WXP = TColor($0094A6A5); // caption color (disabled)

  dxColor_DotNetFrame = TColor($00F7FBFF); // $00E7EBEF;
  dxColor_BorderLineOXP = TColor($00663300);
  dxColor_BgOXP = TColor($00D6BEB5);
  dxColor_BgCkOXP = TColor($00CC9999);

type
  //  TJvXPCustomStyleControl = class;

  TJvXPBoundLines = set of
    (
    blLeft,                             // left line
    blTop,                              // top line
    blRight,                            // right line
    blBottom                            // bottom line
    );


procedure JvXPCreateGradientRect(const AWidth, AHeight: Integer; const StartColor,
  EndColor: TColor; const AColors: TJvXPGradientColors; const Style: TJvXPGradientStyle;
  const Dithered: Boolean; var Bitmap: TBitmap);
procedure JvXPAdjustBoundRect(const BorderWidth: Byte;
  const ShowBoundLines: Boolean; const BoundLines: TJvXPBoundLines; var Rect: TRect);
procedure JvXPDrawBoundLines(const ACanvas: TCanvas; const BoundLines: TJvXPBoundLines;
  const AColor: TColor; const Rect: TRect);

//
// attic!
//

procedure JvXPConvertToGray2(Bitmap: TBitmap);
procedure JvXPRenderText(const AParent: TControl; const ACanvas: TCanvas;
  ACaption: TCaption; const AFont: TFont; const AEnabled, AShowAccelChar: Boolean;
  var ARect: TRect; AFlags: Integer);

procedure JvXPColorizeBitmap(const ABitmap: TBitmap; const AColor: TColor);
procedure JvXPSetDrawFlags(const AAlignment: TAlignment; const AWordWrap: Boolean;
  var Flags: Integer);
procedure JvXPPlaceText(const AParent: TControl; const ACanvas: TCanvas;
  const AText: TCaption; const AFont: TFont; const AEnabled, AShowAccelChar: Boolean;
  const AAlignment: TAlignment; const AWordWrap: Boolean; var Rect: TRect);


implementation


procedure JvXPCreateGradientRect(const AWidth, AHeight: Integer; const StartColor,
  EndColor: TColor; const AColors: TJvXPGradientColors; const Style: TJvXPGradientStyle;
  const Dithered: Boolean; var Bitmap: TBitmap);
 // Short version...
var
  gd: TGradientDirection;
begin
  if (AHeight <= 0) or (AWidth <= 0) then
    Exit;
  Bitmap.Height := AHeight;
  Bitmap.Width := AWidth;
  Bitmap.PixelFormat := pf24bit;
  if Style in [gsLeft, gsRight] then
     gd := gdHorizontal
  else
     gd := gdVertical;
  Bitmap.Canvas.GradientFill(Rect(0, 0, AWidth, AHeight), StartColor, EndColor, gd);
end;
(*
const
  PixelCountMax = 32768;
  DitherDepth = 16;
type
  TGradientBand = array [0..255] of TColor;
  TRGBMap = packed record
    case Boolean of
      True:
        (RGBVal: DWord);
      False:
        (R, G, B, D: Byte);
  end;
  PRGBTripleArray = ^TRGBTripleArray;
  TRGBTripleArray = array [0..PixelCountMax-1] of TRGBTriple;
var
  iLoop, xLoop, yLoop, XX, YY: Integer;
  iBndS, iBndE: Integer;
  GBand: TGradientBand;
  Row: PRGBTripleArray;

  procedure CalculateGradientBand ( const Style: TJvXPGradientStyle; const AColors: TJvXPGradientColors; const StartColor,
  EndColor: TColor);
  var
    rR, rG, rB: Real;
    lCol, hCol: TRGBMap;
    iStp: Integer;
  begin
    if Style in [gsLeft, gsTop] then
    begin
      lCol.RGBVal := ColorToRGB(StartColor);
      hCol.RGBVal := ColorToRGB(EndColor);
    end
    else
    begin
      lCol.RGBVal := ColorToRGB(EndColor);
      hCol.RGBVal := ColorToRGB(StartColor);
    end;
    rR := (hCol.R - lCol.R) / (AColors - 1);
    rG := (hCol.G - lCol.G) / (AColors - 1);
    rB := (hCol.B - lCol.B) / (AColors - 1);
    for iStp := 0 to (AColors - 1) do
      GBand[iStp] := RGB(
        lCol.R + Round(rR * iStp),
        lCol.G + Round(rG * iStp),
        lCol.B + Round(rB * iStp));
  end;

begin
  // Exit if Height or Width are not positive. If not, the calls would lead to
  // GDI errors about "Invalid parameter" and/or "Out Of Resources".
  if (AHeight <= 0) or (AWidth <= 0) then
    Exit;

  Bitmap.Height := AHeight;
  Bitmap.Width := AWidth;
  Bitmap.PixelFormat := pf24bit;

  CalculateGradientBand;

  with Bitmap.Canvas do
  begin
    Brush.Color := StartColor;
    FillRect(Bounds(0, 0, AWidth, AHeight));
    if Style in [gsLeft, gsRight] then
    begin
      for iLoop := 0 to AColors - 1 do
      begin
        iBndS := MulDiv(iLoop, AWidth, AColors);
        iBndE := MulDiv(iLoop + 1, AWidth, AColors);
        Brush.Color := GBand[iLoop];
        PatBlt(Handle, iBndS, 0, iBndE, AHeigth, PATCOPY);
        if (iLoop > 0) and Dithered then
          for yLoop := 0 to DitherDepth - 1 do
            if yLoop < AHeight  then
            begin
              Row := Bitmap.ScanLine[yLoop];
              for xLoop := 0 to AWidth div (AColors - 1) do
                begin
                  XX := iBndS + Random(xLoop);
                  if (XX < AWidth) and (XX > -1) then
                    with Row[XX] do
                    begin
                      rgbtRed := GetRValue(GBand[iLoop - 1]);
                      rgbtGreen := GetGValue(GBand[iLoop - 1]);
                      rgbtBlue := GetBValue(GBand[iLoop - 1]);
                    end;
                end;
            end;
      end;
      for yLoop := 1 to AHeight div DitherDepth do
        CopyRect(Bounds(0, yLoop * DitherDepth, AWidth, DitherDepth),
          Bitmap.Canvas, Bounds(0, 0, AWidth, DitherDepth));
    end
    else
    begin
      for iLoop := 0 to AColors - 1 do
      begin
        iBndS := MulDiv(iLoop, AHeight, AColors);
        iBndE := MulDiv(iLoop + 1, AHeight, AColors);
        Brush.Color := GBand[iLoop];
        PatBlt(Handle, 0, iBndS, AWidth, iBndE, PATCOPY);
        if (iLoop > 0) and Dithered then
          for yLoop := 0 to AHeight div (AColors - 1) do
          begin
            YY := iBndS + Random(yLoop);
            if (YY < AHeight) and (YY > -1) then
            begin
              Row := Bitmap.ScanLine[YY];
              for xLoop := 0 to DitherDepth - 1 do
              if xLoop < AWidth  then
                with Row[xLoop] do
                begin
                  rgbtRed := GetRValue(GBand[iLoop - 1]);
                  rgbtGreen := GetGValue(GBand[iLoop - 1]);
                  rgbtBlue := GetBValue(GBand[iLoop - 1]);
                end;
              end;
          end;
      end;
      for xLoop := 0 to AWidth div DitherDepth do
        CopyRect(Bounds(xLoop * DitherDepth, 0, DitherDepth, AHeight),
          Bitmap.Canvas, Bounds(0, 0, DitherDepth, AHeight));
    end;
  end;
end;
 *)

procedure JvXPAdjustBoundRect(const BorderWidth: Byte;
  const ShowBoundLines: Boolean; const BoundLines: TJvXPBoundLines;
  var Rect: TRect);
begin
  InflateRect(Rect, -BorderWidth, -BorderWidth);
  if not ShowBoundLines then
    Exit;
  if blLeft in BoundLines then
    Inc(Rect.Left);
  if blRight in BoundLines then
    Dec(Rect.Right);
  if blTop in BoundLines then
    Inc(Rect.Top);
  if blBottom in BoundLines then
    Dec(Rect.Bottom);
end;

procedure JvXPDrawBoundLines(const ACanvas: TCanvas; const BoundLines: TJvXPBoundLines;
  const AColor: TColor; const Rect: TRect);
begin
  with ACanvas do
  begin
    Pen.Color := AColor;
    Pen.Style := psSolid;
    if blLeft in BoundLines then
      JvXPDrawLine(ACanvas, Rect.Left, Rect.Top, Rect.Left, Rect.Bottom - 1);
    if blTop in BoundLines then
      JvXPDrawLine(ACanvas, Rect.Left, Rect.Top, Rect.Right, Rect.Top);
    if blRight in BoundLines then
      JvXPDrawLine(ACanvas, Rect.Right - 1, Rect.Top, Rect.Right - 1, Rect.Bottom - 1);
    if blBottom in BoundLines then
      JvXPDrawLine(ACanvas, Rect.Top, Rect.Bottom - 1, Rect.Right, Rect.Bottom - 1);
  end;
end;

//
// attic
//

procedure JvXPConvertToGray2(Bitmap: TBitmap);
var
  x, y, c: Integer;
  PxlColor: TColor;
begin
  for x := 0 to Bitmap.Width - 1 do
    for y := 0 to Bitmap.Height - 1 do
    begin
      PxlColor := ColorToRGB(Bitmap.Canvas.Pixels[x, y]);
      c := (PxlColor shr 16 + ((PxlColor shr 8) and $00FF) + PxlColor and $0000FF) div 3 + 100;
      if c > 255 then
        c := 255;
      Bitmap.Canvas.Pixels[x, y] := RGB(c, c, c);
    end;
end;

procedure JvXPRenderText(const AParent: TControl; const ACanvas: TCanvas;
  ACaption: TCaption; const AFont: TFont; const AEnabled, AShowAccelChar: Boolean;
  var ARect: TRect; AFlags: Integer);

  procedure DoDrawText;
  begin
    // (rom) Kludge! This will probably not work for CLX
    DrawText(ACanvas.Handle, PChar(ACaption), -1, ARect, AFlags);
  end;

begin
  if (AFlags and DT_CALCRECT <> 0) and ((ACaption = '') or AShowAccelChar and
    (ACaption[1] = '&') and (ACaption[2] = #0)) then
    ACaption := ACaption + ' ';
  if not AShowAccelChar then
    AFlags := AFlags or DT_NOPREFIX;
//  AFlags := AParent.DrawTextBiDiModeFlags(AFlags);
  with ACanvas do
  begin
    Font.Assign(AFont);
    if not AEnabled then
      Font.Color := dxColor_Msc_Dis_Caption_WXP;
    if not AEnabled then
    begin
      OffsetRect(ARect, 1, 1);
      Font.Color := clBtnHighlight;
      DoDrawText;
      OffsetRect(ARect, -1, -1);
      Font.Color := clBtnShadow;
      DoDrawText;
    end
    else
      DoDrawText;
  end;
end;

procedure JvXPColorizeBitmap(const ABitmap: TBitmap; const AColor: TColor);
var
  ColorMap: TBitmap;
  Rect: TRect;
begin
  Rect := Bounds(0, 0, ABitmap.Width, ABitmap.Height);
  ColorMap := TBitmap.Create;
  try
    ColorMap.Assign ( Abitmap );
    ColorMap.Width  := ABitmap.Width ;
    ColorMap.Height := ABitmap.Height ;
    ColorMap.Transparent := true ;
    ColorMap.TransparentColor := clBlack ;
    with ABitmap.Canvas do
    begin
{      Lock;
      for x := 0 to Width - 1 do
      for y := 0 to Height - 1 do
        Begin
          writeln ( IntToStr(Pixels[x,y]) );
          Pixels[x,y]:=Pixels[x,y]and AColor;
        end;
      Unlock;}
      Brush.Color := AColor;
      Brush.Bitmap := ABitmap;
      Brush.Style := bsImage;
      FillRect(Rect);
      Changed;
    end;
    ABitmap.Assign(ColorMap);
    ABitmap.Transparent:=True;
    ABitmap.TransparentColor:=clBlack;
  finally
    ColorMap.Free;
  end;
end;

procedure JvXPSetDrawFlags(const AAlignment: TAlignment; const AWordWrap: Boolean;
  var Flags: Integer);
begin
  Flags := DT_END_ELLIPSIS;
  case AAlignment of
    taLeftJustify:
      Flags := Flags or DT_LEFT;
    taCenter:
      Flags := Flags or DT_CENTER;
    taRightJustify:
      Flags := Flags or DT_RIGHT;
  end;
  if not AWordWrap then
    Flags := Flags or DT_SINGLELINE
  else
    Flags := Flags or DT_WORDBREAK;
end;

procedure JvXPPlaceText(const AParent: TControl; const ACanvas: TCanvas; const AText: TCaption;
  const AFont: TFont; const AEnabled, AShowAccelChar: Boolean; const AAlignment: TAlignment;
  const AWordWrap: Boolean; var Rect: TRect);
var
  Flags, DX, OH, OW: Integer;
begin
  OH := Rect.Bottom - Rect.Top;
  OW := Rect.Right - Rect.Left;
  JvXPSetDrawFlags(AAlignment, AWordWrap, Flags);
  JvXPRenderText(AParent, ACanvas, AText, AFont, AEnabled, AShowAccelChar, Rect,
    Flags or DT_CALCRECT);
  if AAlignment = taRightJustify then
    DX := OW - (Rect.Right + Rect.Left)
  else
  if AAlignment = taCenter then
    DX := (OW - Rect.Right) div 2
  else
    DX := 0;
  OffsetRect(Rect, DX, (OH - Rect.Bottom) div 2);
  JvXPRenderText(AParent, ACanvas, AText, AFont, AEnabled, AShowAccelChar, Rect, Flags);
end;

end.

