{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvXPCoreUtils.PAS, released on 2004-01-01.

The Initial Developer of the Original Code is Marc Hoffman.
Portions created by Marc Hoffman are Copyright (C) 2002 APRIORI business solutions AG.
Portions created by APRIORI business solutions AG are Copyright (C) 2002 APRIORI business solutions AG
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id: JvXPCoreUtils.pas 11400 2007-06-28 21:24:06Z ahuser $

// Ported to Lazarus (no too hard after all) by Sergio Samayoa - september 2007.
// Still dont tested on linux.

unit JvXPCoreUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, Graphics, LCLIntf, LCLType, SysUtils,
  TypInfo, JvXPCore;

function JvXPMethodsEqual(const Method1, Method2: TMethod): Boolean;

procedure JvXPDrawLine(const ACanvas: TCanvas; const X1, Y1, X2, Y2: Integer);

procedure JvXPCreateGradientRect(const AWidth, AHeight: Integer; const StartColor,
  EndColor: TColor; const AColors: TJvXPGradientColors; const Style: TJvXPGradientStyle;
  const Dithered: Boolean; var Bitmap: TBitmap);

procedure JvXPAdjustBoundRect(const BorderWidth: Byte;
  const ShowBoundLines: Boolean; const BoundLines: TJvXPBoundLines; var Rect: TRect);

procedure JvXPDrawBoundLines(const ACanvas: TCanvas; const BoundLines: TJvXPBoundLines;
  const AColor: TColor; const Rect: TRect);

procedure JvXPConvertToGray2(ABitmap: TBitmap);

procedure JvXPRenderText(const AParent: TControl; const ACanvas: TCanvas;
  ACaption: TCaption; const AFont: TFont; const AEnabled, AShowAccelChar: Boolean;
  var ARect: TRect; AFlags: Integer);

procedure JvXPFrame3D(const ACanvas: TCanvas; const ARect: TRect;
  const TopColor, BottomColor: TColor; const Swapped: Boolean = False);

procedure JvXPColorizeBitmap(ABitmap: TBitmap; const AColor: TColor);

procedure JvXPSetDrawFlags(const AAlignment: TAlignment; const AWordWrap: Boolean;
  var Flags: Integer);

procedure JvXPPlaceText(const AParent: TControl; const ACanvas: TCanvas;
  const AText: TCaption; const AFont: TFont; const AEnabled, AShowAccelChar: Boolean;
  const AAlignment: TAlignment; const AWordWrap: Boolean; var Rect: TRect);


implementation

uses
  IntfGraphics, fpCanvas, fpImage, fpImgCanv;

function JvXPMethodsEqual(const Method1, Method2: TMethod): Boolean;
begin
  Result := (Method1.Code = Method2.Code) and (Method1.Data = Method2.Data);
end;

// Ignoring "AColors" and "Dithered"
procedure JvXPCreateGradientRect(const AWidth, AHeight: Integer;
  const StartColor, EndColor: TColor; const AColors: TJvXPGradientColors;
  const Style: TJvXPGradientStyle; const Dithered: Boolean; var Bitmap: TBitmap);
begin
  if (AHeight <= 0) or (AWidth <= 0) then
    Exit;
  Bitmap.Height := AHeight;
  Bitmap.Width := AWidth;
  Bitmap.PixelFormat := pf24bit;
  case Style of
    gsLeft:
      Bitmap.Canvas.GradientFill(Rect(0, 0, AWidth, AHeight), StartColor, EndColor, gdHorizontal);
    gsRight:
      Bitmap.Canvas.GradientFill(Rect(0, 0, AWidth, AHeight), EndColor, StartColor, gdHorizontal);
    gsTop:
      Bitmap.Canvas.GradientFill(Rect(0, 0, AWidth, AHeight), StartColor, EndColor, gdVertical);
    gsBottom:
      Bitmap.Canvas.GradientFill(Rect(0, 0, AWidth, AHeight), EndColor, StartColor, gdVertical);
  end;
end;


(*
// Dithered is ignored at the moment...
procedure JvXPCreateGradientRect(const AWidth, AHeight: Integer; const StartColor,
  EndColor: TColor; const AColors: TJvXPGradientColors; const Style: TJvXPGradientStyle;
  const Dithered: Boolean; var Bitmap: TBitmap);
{ // Short version...
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
}
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
var
  iLoop, xLoop, yLoop, XX, YY: Integer;
  iBndS, iBndE: Integer;
  GBand: TGradientBand;
  intfImg: TLazIntfImage;
  cnv: TFPImageCanvas;
  clr: TFPColor;
  imgHandle, imgMaskHandle: HBitmap;
  tempBitmap: TBitmap;

  procedure CalculateGradientBand;
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

  intfImg := TLazIntfImage.Create(0, 0);
  intfImg.LoadFromBitmap(Bitmap.Handle, Bitmap.MaskHandle);
  cnv := TFPImageCanvas.Create(intfImg);
  cnv.Brush.FPColor := TColorToFPColor(StartColor);
  //cnv.FillRect(Bounds(0, 0, AWidth, AHeight));

  if Style in [gsLeft, gsRight] then
  begin
    for iLoop := 0 to AColors - 1 do begin
      iBndS := MulDiv(iLoop, AWidth, AColors);
      iBndE := MulDiv(iLoop + 1, AWidth, AColors);
      cnv.Brush.FPColor := TColorToFPColor(GBand[iLoop]);
      cnv.FillRect(iBnds, 0, iBndE, AHeight);
      {
      if Dithered and (iLoop > 0) then
      begin
        clr := TColorToFPColor(GBand[iLoop - 1]);
        for yLoop := 0 to DitherDepth - 1 do
          if yLoop < AHeight then
            for xLoop := 0 to AWidth div (AColors - 1) do
            begin
              XX := iBndS + Random(xLoop);
              if (XX < AWidth) and (XX > -1) then
                cnv.Colors[XX, yLoop] := clr;
          end;
      end;
      }
    end;
    {
    if Dithered then
      for yLoop := 1 to AHeight div DitherDepth do
        for xLoop := 0 to AWidth - 1 do
          cnv.Colors[xLoop, yLoop * DitherDepth] := cnv.Colors[xLoop, 0];
        }
  end
  else
  begin
    for iLoop := 0 to AColors - 1 do
    begin
      iBndS := MulDiv(iLoop, AHeight, AColors);
      iBndE := MulDiv(iLoop + 1, AHeight, AColors);
      cnv.Brush.FPColor := TColorToFPColor(GBand[iLoop]);
      cnv.FillRect(0, iBndS, AWidth, iBndS + iBndE);
      {
      if Dithered and (iLoop > 0) then
      begin
        clr := TColorToFPColor(GBand[iLoop - 1]);
        for yLoop := 0 to AHeight div (AColors - 1) do
        begin
          YY := iBndS + Random(yLoop);
          if (YY < AHeight) and (YY > -1) then
            for xLoop := 0 to DitherDepth - 1 do
              if xLoop < AWidth then
                cnv.Colors[xLoop, YY] := clr;
        end;
      end;
      }
    end;
    {
    for xLoop := 0 to AWidth div DitherDepth do
      for yLoop := 0 to AHeight - 1 do
        cnv.Colors[xLoop * DitherDepth, yLoop] := cnv.Colors[0, yLoop];
        }
  end;

  intfImg.CreateBitmaps(imgHandle, imgMaskHandle, false);
  tempBitmap := TBitmap.Create;
  tempBitmap.Handle := imgHandle;
  tempBitmap.MaskHandle := imgMaskHandle;
  Bitmap.Canvas.Draw(0, 0, tempBitmap);

  tempBitmap.Free;
  cnv.Free;
  intfImg.Free;
end;
  *)

procedure JvXPDrawLine(const ACanvas: TCanvas; const X1, Y1, X2, Y2: Integer);
begin
  with ACanvas do
  begin
    MoveTo(X1, Y1);
    LineTo(X2, Y2);
  end;
end;

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

procedure JvXPConvertToGray2(ABitmap: TBitmap);
var
  x, y, c: Integer;
  PxlColor: TColor;
begin
  for x := 0 to ABitmap.Width - 1 do
    for y := 0 to ABitmap.Height - 1 do
    begin
      PxlColor := ColorToRGB(ABitmap.Canvas.Pixels[x, y]);
      c := (PxlColor shr 16 + ((PxlColor shr 8) and $00FF) + PxlColor and $0000FF) div 3 + 100;
      if c > 255 then
        c := 255;
      ABitmap.Canvas.Pixels[x, y] := RGB(c, c, c);
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
  // wp: To do - bidi
  // AFlags := AParent.DrawTextBiDiModeFlags(AFlags);
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

procedure JvXPFrame3D(const ACanvas: TCanvas; const ARect: TRect;
  const TopColor, BottomColor: TColor; const Swapped: Boolean = False);
var
  ATopColor, ABottomColor: TColor;
begin
  ATopColor := TopColor;
  ABottomColor := BottomColor;
  if Swapped then
  begin
    ATopColor := BottomColor;
    ABottomColor := TopColor;
  end;
  with ACanvas do
  begin
    Pen.Color := ATopColor;
    // 21.09.07 - SESS
    Polyline([
      Classes.Point(ARect.Left, ARect.Bottom - 1),
      Classes.Point(ARect.Left, ARect.Top),
      Classes.Point(ARect.Right - 1, ARect.Top)]);
    Pen.Color := ABottomColor;
    Polyline([
      Classes.Point(ARect.Right - 1, ARect.Top + 1),
      Classes.Point(ARect.Right - 1 , ARect.Bottom - 1),
      Classes.Point(ARect.Left, ARect.Bottom - 1)]);
  end;
end;

procedure JvXPColorizeBitmap(ABitmap: TBitmap; const AColor: TColor);
var
  ColorMap: TBitmap;
  Rect: TRect;
begin
  Rect := Bounds(0, 0, ABitmap.Width, ABitmap.Height);
  ColorMap := TBitmap.Create;
  try
    ColorMap.Assign(ABitmap);
    ABitmap.FreeImage;
    with ColorMap.Canvas do
    begin
      Brush.Color := AColor;
      BrushCopy(Rect, ABitmap, Rect, clBlack);
    end;
    ABitmap.Assign(ColorMap);
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

