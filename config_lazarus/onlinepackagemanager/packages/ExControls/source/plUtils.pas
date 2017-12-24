
{**********************************************************************
 Package pl_ExControls.pkg
 From PilotLogic Software House(http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit plUtils;

{$MODE Delphi}

interface

uses
  SysUtils, Classes, Graphics, Buttons, Messages, LMessages,
  Controls, LCLtype, LCLIntf, LCLProc, types,
  plUtilsForHSL, Math;

const

  ecDarkBlue = TColor($00996633);
  ecBlue = TColor($00CF9030);
  ecLightBlue = TColor($00CFB78F);

  ecDarkRed = TColor($00302794);
  ecRed = TColor($005F58B0);
  ecLightRed = TColor($006963B6);

  ecDarkGreen = TColor($00385937);
  ecGreen = TColor($00518150);
  ecLightGreen = TColor($0093CAB1);

  ecDarkYellow = TColor($004EB6CF);
  ecYellow = TColor($0057D1FF);
  ecLightYellow = TColor($00B3F8FF);

  ecDarkBrown = TColor($00394D4D);
  ecBrown = TColor($00555E66);
  ecLightBrown = TColor($00829AA2);

  ecDarkKaki = TColor($00D3D3D3);
  ecKaki = TColor($00C8D7D7);
  ecLightKaki = TColor($00E0E9EF);

  { Encarta & FlatStyle Interface Color Constants }
  ecBtnHighlight = clWhite;
  ecBtnShadow = clBlack;
  ecBtnFace = ecLightKaki;
  ecBtnFaceDown = ecKaki;

  ecFocused = clWhite;

  ecScrollbar = ecLightKaki;
  ecScrollbarThumb = ecLightBrown;

  ecBackground = clWhite;

  ecHint = ecYellow;
  ecHintArrow = clBlack;

  ecDot = clBlack;
  ecTick = clBlack;

  ecMenuBorder = ecDarkBrown;
  ecMenu = clBlack;
  ecMenuSelected = ecDarkYellow;

  ecProgressBlock = ecBlue;
  ecUnselectedTab = ecBlue;

  ecSelection = clNavy;

  ecCaptionBackground = clBlack;
  ecActiveCaption = clWhite;
  ecInactiveCaption = ecLightBrown;



  MSH_MOUSEWHEEL = 'MSWHEEL_ROLLMSG';
  WM_MOUSEWHEEL = $020A;
  WHEEL_DELTA = 120;
  WHEEL_PAGESCROLL = $FFFFFFFF;

  SM_MOUSEWHEELPRESENT = 75;
  SPI_GETWHEELSCROLLLINES = 104;
  SPI_SETWHEELSCROLLLINES = 105;

type
  TScrollType = (up, down);
  TColorCalcType = (lighten, darken);
  TCheckBoxLayout = (checkboxLeft, checkboxRight);
  TRadioButtonLayout = (radioLeft, radioRight);

  TProgressBarOrientation = (pbHorizontal, pbVertical);

  TplTabPosition = (tpxTop, tpxBottom);
  TArrowPos = (NE, NW, SE, SW);

  TAdvColors = 0..100;
  TGroupBoxBorder = (brFull, brOnlyTopLine);
  TplTransparentMode = (tmAlways, tmNotFocused, tmNone);

  TFillDirection = (fdTopToBottom, fdBottomToTop, fdLeftToRight, fdRightToLeft);

function  GetShaddowColor(color: TColor): TColor;
procedure Frame3DBorder(Canvas: TCanvas; Rect: TRect; TopColor, BottomColor: TColor; Width: integer);
procedure DrawParentImage(Control: TControl; Dest: TCanvas);
function  CreateDisabledBitmap(FOriginal: TBitmap; OutlineColor, BackColor, HighlightColor, ShadowColor: TColor;
                              DrawHighlight: boolean): TBitmap;
function  CalcAdvancedColor(ParentColor, OriginalColor: TColor; Percent: byte; ColorType: TColorCalcType): TColor;
procedure CalcButtonLayout(aCanvas: TCanvas; const aClient: TRect; const aOffset: TPoint; aLayout: TButtonLayout;
                           aSpacing, aMargin: integer; FGlyph: TBitmap; FNumGlyphs: integer; const
                           aCaption: string; var aTextBounds: TRect; var aGlyphPos: TPoint);
function Min(val1, val2: word): word;
function GetFontMetrics(Font: TFont): TTextMetric;
function GetFontHeight(Font: TFont): integer;
function RectInRect(R1, R2: TRect): boolean;
function PointInRect(const pt: TPoint; const MER: TRect): boolean;
function RectHeight(ARect: Trect): integer;
function RectWidth(ARect: Trect): integer;
function WidthOf(R: TRect): integer;
function HeightOf(R: TRect): integer;
procedure InflateRect(R: TRect; Hor, Ver: integer);
procedure GradientFillRect(Canvas: TCanvas; ARect: TRect; StartColor, EndColor: TColor; Direction: TFillDirection; Colors: Byte);
procedure DefiSetDefaultColors;
//Text
procedure DrawTextInRect(aCanvas: TCanvas; R: TRect; Text: string);
//bitmaps
procedure DrawTransparentBmp(Cnv: TCanvas; x, y: integer; Bmp: TBitmap; clTransparent: TColor);


var
  DefiDrawFlat: boolean = False;
  //insate color
  DefiColorFlat: Tcolor = clWhite;
  //Backround Color
  DefiControlsBkColor: Tcolor = clBtnFace; //clSkyBlue clBtnFace

  DefiColorFocused: Tcolor = $00FBEEE3; //lite clSkyBlue
  DefiColorUnFocused: Tcolor = clCream;
  DefiColorBorder: Tcolor = $008396A0;
  DefiColorDown: Tcolor = clLime;

  DefiColorCheck: Tcolor = clBlack;
  DefiColorArrow: Tcolor = clBlack;
  DefiColorArrowBackground: Tcolor = clSilver;

  //........ General Advance colors ..............
  DefiUseAdvColors: boolean = False;
  DefiAdvColorBorder: TAdvColors = 50;
  DefiAdvColorArrowBackground: TAdvColors = 10;
  DefiAdvColorFocused: TAdvColors = 10;
  DefiAdvColorDown: TAdvColors = 10;
  DefiAdvColorHighlight: TAdvColors = 50;

  DefiItemsRectColor: Tcolor = clWhite;
  DefiItemsSelectColor: Tcolor = $009CDEF7;
  DefiColorHighlight: Tcolor = clHighlight;

  DefiPanelColorHighLight: Tcolor = $008396A0;
  DefiPanelColorShadow: Tcolor = $008396A0;
  //......... Page Control ..................
  DefiTabSheetBkColor: Tcolor = clBtnFace; //clSkyBlue clBtnFace
  DefiTabSheetBkColorTo: Tcolor = clNone;

  DefiTabSheetHoverColor: Tcolor = clWhite; //same with DefiColorFocused
  DefiTabSheetHoverColorTo: TColor = clNone;
  DefiTabSheetHoverBorder: TColor = clNone;
  DefiTabSheetHoverGradientDir: TGradientDirection = gdVertical;

  DefiPageControlFocusColor: TColor = clBtnFace;
  DefiPageControlNoFocusColor: TColor = clBtnFace;

  DefiTabSheetTabColor: Tcolor = clwhite;
  DefiTabSheetTabColorTo: Tcolor = clSilver;
  DefiTabSheetGradientDir: TGradientDirection = gdVertical;

  DefiPageControlActiveTextColor: TColor = clwhite;
  DefiPageControlActiveColor: TColor = clSkyBlue;
  DefiPageControlActiveColorTo: Tcolor = clBlack;

  //........... TFlatButton,TFlatSpeedButton ..........................
  DefiBtnFromColor: TColor = clWhite;
  DefiBtnToColor: TColor = clSilver;
  DefiBtnFocusedToColor: TColor = clSilver;
  DefiBtnDownToColor: TColor = clLime;
  DefiBtnDownFocusedToColor: TColor = clLime;
  DefiBtnDisabledToColor: TColor = clGray;
  DefiBtnBorderColor: TColor = $008396A0;
  DefiBtnColorHighlight: TColor = clWhite;
  DefiBtnColorShadow: TColor = clBlack;

  //...........TFlatScrollbar.............
  DefiScrollbarColor: TColor = clWhite;

implementation

procedure DefiSetDefaultColors;
begin
  DefiDrawFlat := False;
  //insate color
  DefiColorFlat := clWhite;
  //Backround Color
  DefiControlsBkColor := clBtnFace; //clSkyBlue clBtnFace

  DefiColorFocused := $00FBEEE3;
  DefiColorUnFocused := clCream;
  DefiColorBorder := $008396A0;
  DefiColorDown := clLime;

  DefiColorCheck := clBlack;
  DefiColorArrow := clBlack;
  DefiColorArrowBackground := clSilver;

  DefiItemsRectColor := clWhite;
  DefiItemsSelectColor := $009CDEF7;
  DefiColorHighlight := clHighlight;

  DefiUseAdvColors := False;
  DefiAdvColorBorder := 50;
  DefiAdvColorArrowBackground := 10;
  DefiAdvColorFocused := 10;
  DefiAdvColorDown := 10;
  DefiAdvColorHighlight := 50;

  DefiPanelColorHighLight := $008396A0;
  DefiPanelColorShadow := $008396A0;
  //......... Page Control ..................
  DefiTabSheetBkColor := clBtnFace;
  DefiTabSheetBkColorTo := clNone;

  DefiTabSheetTabColor := clSilver;
  DefiTabSheetTabColorTo := clNone;
  DefiTabSheetGradientDir := gdVertical;

  DefiTabSheetHoverColor := $00FBEEE3; //same with DefiColorFocused
  DefiTabSheetHoverColorTo := clNone;
  DefiTabSheetHoverBorder := clNone;
  DefiTabSheetHoverGradientDir := gdVertical;

  DefiPageControlActiveColor := clYellow;
  DefiPageControlActiveColorTo := clNone;

  DefiPageControlFocusColor := clBtnFace;
  DefiPageControlNoFocusColor := clBtnFace;
end;


function GetShaddowColor(color: TColor): TColor;
type
  ColorConvert = record
    case byte of
      0: (z: TColor);
      1: (a, b, c, d: byte);
  end;
begin
  ColorConvert(Result).a := ColorConvert(color).a div 2;
  ColorConvert(Result).b := ColorConvert(color).b div 2;
  ColorConvert(Result).c := ColorConvert(color).c div 2;
  ColorConvert(Result).d := ColorConvert(color).d div 2;
end;


procedure Frame3DBorder(Canvas: TCanvas; Rect: TRect; TopColor, BottomColor: TColor; Width: integer);

  procedure DoRect;
  var
    TopRight, BottomLeft: TPoint;
  begin
    with Canvas, Rect do
    begin
      TopRight.X := Right;
      TopRight.Y := Top;
      BottomLeft.X := Left;
      BottomLeft.Y := Bottom;
      Pen.Color := TopColor;
      PolyLine([BottomLeft, TopLeft, TopRight]);
      Pen.Color := BottomColor;
      Dec(BottomLeft.X);
      PolyLine([TopRight, BottomRight, BottomLeft]);
    end;
  end;

begin
  Canvas.Pen.Width := 1;
  Dec(Rect.Bottom);
  Dec(Rect.Right);
  while Width > 0 do
  begin
    Dec(Width);
    DoRect;
    InflateRect(Rect, -1, -1);
  end;
  Inc(Rect.Bottom);
  Inc(Rect.Right);
end;

procedure DrawTransparentBmp(Cnv: TCanvas; x, y: integer; Bmp: TBitmap; clTransparent: TColor);
var
  bmpXOR, bmpAND, bmpINVAND, bmpTarget: TBitmap;
  oldcol: longint;
begin
  bmpAND := TBitmap.Create;
  bmpINVAND := TBitmap.Create;
  bmpXOR := TBitmap.Create;
  bmpTarget := TBitmap.Create;
  try
    bmpAND.Width := Bmp.Width;
    bmpAND.Height := Bmp.Height;
    bmpAND.Monochrome := True;
    oldcol := SetBkColor(Bmp.Canvas.Handle, ColorToRGB(clTransparent));
    BitBlt(bmpAND.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
    SetBkColor(Bmp.Canvas.Handle, oldcol);

    bmpINVAND.Width := Bmp.Width;
    bmpINVAND.Height := Bmp.Height;
    bmpINVAND.Monochrome := True;
    BitBlt(bmpINVAND.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, bmpAND.Canvas.Handle, 0, 0, NOTSRCCOPY);

    bmpXOR.Width := Bmp.Width;
    bmpXOR.Height := Bmp.Height;
    BitBlt(bmpXOR.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
    BitBlt(bmpXOR.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, bmpINVAND.Canvas.Handle, 0, 0, SRCAND);

    bmpTarget.Width := Bmp.Width;
    bmpTarget.Height := Bmp.Height;
    BitBlt(bmpTarget.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, Cnv.Handle, x, y, SRCCOPY);
    BitBlt(bmpTarget.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, bmpAND.Canvas.Handle, 0, 0, SRCAND);
    BitBlt(bmpTarget.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, bmpXOR.Canvas.Handle, 0, 0, SRCINVERT);

    BitBlt(Cnv.Handle, x, y, Bmp.Width, Bmp.Height, bmpTarget.Canvas.Handle, 0, 0, SRCCOPY);
  finally
    bmpXOR.Free;
    bmpAND.Free;
    bmpINVAND.Free;
    bmpTarget.Free;
  end;
end;
function CreateDisabledBitmap(FOriginal: TBitmap; OutlineColor, BackColor, HighlightColor, ShadowColor: TColor;
  DrawHighlight: boolean): TBitmap;
const
  ROP_DSPDxax = $00E20746;
var
  MonoBmp: TBitmap;
  IRect: TRect;
begin
  IRect := Rect(0, 0, FOriginal.Width, FOriginal.Height);
  Result := TBitmap.Create;
  try
    Result.Width := FOriginal.Width;
    Result.Height := FOriginal.Height;
    MonoBmp := TBitmap.Create;
    try
      with MonoBmp do
      begin
        Width := FOriginal.Width;
        Height := FOriginal.Height;
        Canvas.CopyRect(IRect, FOriginal.Canvas, IRect);

        HandleType := bmDDB;

        Canvas.Brush.Color := OutlineColor;
        if Monochrome then
        begin
          Canvas.Font.Color := clWhite;
          Monochrome := False;
          Canvas.Brush.Color := clWhite;
        end;
        Monochrome := True;
      end;
      with Result.Canvas do
      begin
        Brush.Color := BackColor;
        FillRect(IRect);
        if DrawHighlight then
        begin
          Brush.Color := HighlightColor;
          SetTextColor(Handle, clBlack);
          SetBkColor(Handle, clWhite);
          BitBlt(Handle, 1, 1, IRect.Right - IRect.Left, IRect.Bottom - IRect.Top, MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
        end;
        Brush.Color := ShadowColor;
        SetTextColor(Handle, clBlack);
        SetBkColor(Handle, clWhite);
        BitBlt(Handle, 0, 0, IRect.Right - IRect.Left, IRect.Bottom - IRect.Top, MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
      end;
    finally
      MonoBmp.Free;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function CalcAdvancedColor(ParentColor, OriginalColor: TColor; Percent: byte; ColorType: TColorCalcType): TColor;
var
  H, S, L: integer;
begin
  if Percent <> 0 then
  begin
    RGBtoHSLRange(ColorToRGB(ParentColor), H, S, L);
    Inc(L, 10);
    if ColorType = lighten then
      if L + Percent > 100 then
        L := 100
      else
        Inc(L, Percent)
    else
    if L - Percent < 0 then
      L := 0
    else
      Dec(L, Percent);

    Result := HSLRangeToRGB(H, S, L);
  end
  else
    Result := OriginalColor;
end;

procedure CalcButtonLayout(aCanvas: TCanvas; const aClient: TRect; const aOffset: TPoint; aLayout: TButtonLayout;
                           aSpacing, aMargin: integer; FGlyph: TBitmap; FNumGlyphs: integer; const
                           aCaption: string; var aTextBounds: TRect; var aGlyphPos: TPoint);
var
  TextPos: TPoint;
  ClientSize, GlyphSize, TextSize: TPoint;
  TotalSize: TPoint;
begin
  // calculate the item sizes
  ClientSize := Point(aClient.Right - aClient.Left, aClient.Bottom - aClient.Top);

  if FGlyph <> nil then
    GlyphSize := Point(FGlyph.Width div FNumGlyphs, FGlyph.Height)
  else
    GlyphSize := Point(0, 0);

  if Length(aCaption) > 0 then
  begin
    aTextBounds := Rect(0, 0, aClient.Right - aClient.Left, 0);
    DrawText(aCanvas.Handle, @aCaption, Length(aCaption), aTextBounds, DT_CALCRECT or DT_SINGLELINE);
    TextSize := Point(aTextBounds.Right - aTextBounds.Left, aTextBounds.Bottom - aTextBounds.Top);
  end
  else
  begin
    aTextBounds := Rect(0, 0, 0, 0);
    TextSize := Point(0, 0);
  end;

  // If the layout has the glyph on the right or the left, then both the
  // text and the glyph are centered vertically.  If the glyph is on the top
  // or the bottom, then both the text and the glyph are centered horizontally.
  if aLayout in [blGlyphLeft, blGlyphRight] then
  begin
    aGlyphPos.Y := (ClientSize.Y - GlyphSize.Y + 1) div 2;
    TextPos.Y := (ClientSize.Y - TextSize.Y + 1) div 2;
  end
  else
  begin
    aGlyphPos.X := (ClientSize.X - GlyphSize.X + 1) div 2;
    TextPos.X := (ClientSize.X - TextSize.X + 1) div 2;
  end;

  // if there is no text or no bitmap, then Spacing is irrelevant
  if (TextSize.X = 0) or (GlyphSize.X = 0) then
    aSpacing := 0;

  // adjust Margin and Spacing
  if aMargin = -1 then
  begin
    if aSpacing = -1 then
    begin
      TotalSize := Point(GlyphSize.X + TextSize.X, GlyphSize.Y + TextSize.Y);
      if aLayout in [blGlyphLeft, blGlyphRight] then
        aMargin := (ClientSize.X - TotalSize.X) div 3
      else
        aMargin := (ClientSize.Y - TotalSize.Y) div 3;
      aSpacing := aMargin;
    end
    else
    begin
      TotalSize := Point(GlyphSize.X + aSpacing + TextSize.X, GlyphSize.Y + aSpacing + TextSize.Y);
      if aLayout in [blGlyphLeft, blGlyphRight] then
       aMargin := (ClientSize.X - TotalSize.X + 1) div 2
      else
        aMargin := (ClientSize.Y - TotalSize.Y + 1) div 2;
    end;
  end
  else
  begin
    if aSpacing = -1 then
    begin
      TotalSize := Point(ClientSize.X - (aMargin + GlyphSize.X), ClientSize.Y - (aMargin + GlyphSize.Y));
      if aLayout in [blGlyphLeft, blGlyphRight] then
        aSpacing := (TotalSize.X - TextSize.X) div 2
      else
        aSpacing := (TotalSize.Y - TextSize.Y) div 2;
    end;
  end;

  case aLayout of
    blGlyphLeft:
    begin
      aGlyphPos.X := aMargin;
      TextPos.X := aGlyphPos.X + GlyphSize.X + aSpacing;
    end;
    blGlyphRight:
    begin
      aGlyphPos.X := ClientSize.X - aMargin - GlyphSize.X;
      TextPos.X := aGlyphPos.X - aSpacing - TextSize.X;
    end;
    blGlyphTop:
    begin
      aGlyphPos.Y := aMargin;
      TextPos.Y := aGlyphPos.Y + GlyphSize.Y + aSpacing;
    end;
    blGlyphBottom:
    begin
      aGlyphPos.Y := ClientSize.Y - aMargin - GlyphSize.Y;
      TextPos.Y := aGlyphPos.Y - aSpacing - TextSize.Y;
    end;
  end;

  // fixup the result variables
  with aGlyphPos do
  begin
    Inc(X, aClient.Left + aOffset.X);
    Inc(Y, aClient.Top + aOffset.Y);
  end;
  OffsetRect(aTextBounds, TextPos.X +aClient.Left + aOffset.X, TextPos.Y + aClient.Top + aOffset.X);
end;

function Min(val1, val2: word): word;
begin
  Result := val1;
  if val1 > val2 then
    Result := val2;
end;

function GetFontMetrics(Font: TFont): TTextMetric;
var
  DC: HDC;
  SaveFont: HFont;
begin
  DC := GetDC(0);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Result);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
end;

function GetFontHeight(Font: TFont): integer;
begin
  with GetFontMetrics(Font) do
    Result := Round(tmHeight + tmHeight / 8);
end;

function RectInRect(R1, R2: TRect): boolean;
begin
  Result := IntersectRect(R1, R1, R2);
end;

function RectHeight(ARect: Trect): integer;
begin
  Result := ARect.Bottom - ARect.Top + 1;
end;

function RectWidth(ARect: Trect): integer;
begin
  Result := ARect.Right - ARect.Left + 1;
end;

function WidthOf(R: TRect): integer;
begin
  Result := R.Right - R.Left;
end;

function HeightOf(R: TRect): integer;
begin
  Result := R.Bottom - R.Top;
end;


function PointInRect(const pt: TPoint; const MER: TRect): boolean;
begin
  Result := (pt.Y > MER.Top) and (pt.Y < MER.Top + MER.Bottom);
  if Result then
  begin
    Result := (pt.X > MER.Left) and (pt.X < MER.Left + MER.Right);
  end;
end;

procedure InflateRect(R: TRect; Hor, Ver: integer);
begin
  R.left := R.left + Hor;
  R.Right := R.Right - Hor;
  R.Top := R.Top + Ver;
  R.Bottom := R.Bottom - Ver;
end;

procedure DrawTextInRect(aCanvas: TCanvas; R: TRect; Text: string);
var
  ix, iy: integer;
begin
  iy := Round(aCanvas.TextHeight(Text) / 2);
  ix := Round(aCanvas.TextWidth(Text) / 2);
  aCanvas.TextOut(Round((R.Right - R.left) / 2 - ix),
    Round((R.Bottom - R.top) / 2 - iy),
    Text);
end;

procedure GradientFillRect(Canvas: TCanvas; ARect: TRect; StartColor, EndColor: TColor; Direction: TFillDirection; Colors: Byte);
var
  StartRGB: array [0..2] of Byte; { Start RGB values }
  RGBDelta: array [0..2] of Integer;
  { Difference between start and end RGB values }
  ColorBand: TRect; { Color band rectangular coordinates }
  I, Delta: Integer;
  Brush: HBRUSH;
  TmpColor: TColor;
begin
  Canvas.Lock;
  try
    if (StartColor = clNone) and (EndColor = clNone) then
      Exit;
    if not (IsRectEmpty(ARect) and (GetMapMode(Canvas.Handle) = MM_TEXT)) then
    begin

      StartColor := ColorToRGB(StartColor);
      EndColor := ColorToRGB(EndColor);

      if Direction in [fdBottomToTop, fdRightToLeft] then
      begin
        // just swap the colors
        TmpColor := StartColor;
        StartColor := EndColor;
        EndColor := TmpColor;
        if Direction = fdBottomToTop then
          Direction := fdTopToBottom
        else
          Direction := fdLeftToRight;
      end;
      if (Colors < 2) or (StartColor = EndColor) then
      begin
        Brush := CreateSolidBrush(ColorToRGB(StartColor));
        FillRect(Canvas.Handle, ARect, Brush);
        DeleteObject(Brush);
        Exit;
      end;
          { Set the Red, Green and Blue colors }
      StartRGB[0] := GetRValue(StartColor);
      StartRGB[1] := GetGValue(StartColor);
      StartRGB[2] := GetBValue(StartColor);
          { Calculate the difference between begin and end RGB values }
      RGBDelta[0] := GetRValue(EndColor) - StartRGB[0];
      RGBDelta[1] := GetGValue(EndColor) - StartRGB[1];
      RGBDelta[2] := GetBValue(EndColor) - StartRGB[2];
      { Calculate the color band's coordinates }
      ColorBand := ARect;
      if Direction = fdTopToBottom then
      begin
        Colors := Max(2, Min(Colors, RectHeight(ARect)));
        Delta := RectHeight(ARect) div Colors;
      end
      else
      begin
        Colors := Max(2, Min(Colors, RectWidth(ARect)));
        Delta := RectWidth(ARect) div Colors;
      end;
      with Canvas.Pen do
      begin { Set the pen style and mode }
        Style := psSolid;
        Mode := pmCopy;
      end;
      { Perform the fill }
      if Delta > 0 then
      begin
        for I := 0 to Colors - 1 do
        begin
          if Direction = fdTopToBottom then
          { Calculate the color band's top and bottom coordinates }
          begin
            ColorBand.Top := ARect.Top + I * Delta;
            ColorBand.Bottom := ColorBand.Top + Delta;
          end
          { Calculate the color band's left and right coordinates }
          else
          begin
            ColorBand.Left := ARect.Left + I * Delta;
            ColorBand.Right := ColorBand.Left + Delta;
          end;
        { Calculate the color band's color }
          Brush := CreateSolidBrush(RGB(
            StartRGB[0] + MulDiv(I, RGBDelta[0], Colors - 1),
            StartRGB[1] + MulDiv(I, RGBDelta[1], Colors - 1),
            StartRGB[2] + MulDiv(I, RGBDelta[2], Colors - 1)));
          FillRect(Canvas.Handle, ColorBand, Brush);
          DeleteObject(Brush);
        end;
      end;
      if Direction = fdTopToBottom then
        Delta := RectHeight(ARect) mod Colors
      else
        Delta := RectWidth(ARect) mod Colors;
      if Delta > 0 then
      begin
        if Direction = fdTopToBottom then
        { Calculate the color band's top and bottom coordinates }
        begin
          ColorBand.Top := ARect.Bottom - Delta;
          ColorBand.Bottom := ColorBand.Top + Delta;
        end
        else
        { Calculate the color band's left and right coordinates }
        begin
          ColorBand.Left := ARect.Right - Delta;
          ColorBand.Right := ColorBand.Left + Delta;
        end;
        Brush := CreateSolidBrush(EndColor);
        FillRect(Canvas.Handle, ColorBand, Brush);
        DeleteObject(Brush);
      end;
    end; //  if Not (IsRectEmpty(ARect) and ...
  finally
    Canvas.Unlock;
  end;
end;

//===========================================================================
//===========================================================================
//===========================================================================

procedure DrawParentImage(Control: TControl; Dest: TCanvas);
var
  SaveIndex: integer;
  DC: HDC;
  Position: TPoint;
begin
  with Control do
  begin
    if Parent = nil then Exit;
    DC := Dest.Handle;
    SaveIndex := SaveDC(DC);
    GetViewportOrgEx(DC, @Position);
    SetViewportOrgEx(DC, Position.X - Left, Position.Y - Top, nil);
    IntersectClipRect(DC, 0, 0, Parent.ClientWidth, Parent.ClientHeight);
    Parent.Perform(LM_ERASEBKGND, DC, 0);
    Parent.Perform(LM_PAINT, DC, 0);
    RestoreDC(DC, SaveIndex);
  end;
end;

       {
procedure DrawParentImage(Control: TControl; Dest: TCanvas);
var
SaveIndex: Integer;
DC: HDC;
Position: TPoint;
begin
with Control do
begin
if Parent = nil then
Exit;
DC := Dest.Handle;
SaveIndex := SaveDC(DC);

IntersectClipRect(DC, 0, 0, Parent.ClientWidth, Parent.ClientHeight);

Parent.Perform(LM_ERASEBKGND, PtrInt(DC), PtrInt(0));
Parent.Perform(LM_PAINT, PtrInt(DC), PtrInt(0));

RestoreDC(DC, SaveIndex);

end;
end;  }




end.
