
{**********************************************************************
 Package pl_ExControls.pkg
 From PilotLogic Software House(http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit TplRulerUnit;

interface

uses
  LCLIntf, LCLType, LMessages, Messages,
  SysUtils, Classes, Graphics, Controls, Forms, ExtCtrls;

const
  Centi: String = 'cm';
  Milli: String = 'mm';
  Inch: String = 'in';
  Pixel: String = 'px';
  None: String = '';
  Sec: String = 'cm';

type
  TRulerDir = (rdTop, rdLeft, rdRight, rdBottom);
  TRulerUnit = (ruCenti, ruMilli, ruInch, ruPixel, ruNone,ruSec);
  TCornerPos = (cpLeftTop, cpRightTop, cpLeftBottom, cpRightBottom);
  THairLineStyle = (hlsLine, hlsRect);


TplCustomRuler = class(TGraphicControl)
  private
    fFlat: Boolean;
    fScaleColor: TColor;
    fTickColor: TColor;
    fUnits: TRulerUnit;
    procedure SetFlat(const Value: Boolean);
    procedure SetScaleColor(const Value: TColor);
    procedure SetTickColor(const Value: TColor);
  protected
    LeftSideLF, RightSideLF, NormLF: TLogFont;
    OldFont, NormFont, LeftSideFont, RightSideFont: HFont;
    FirstTime: Boolean;
    procedure Paint; override;
    procedure SetUnit(const Value: TRulerUnit); virtual;
    procedure FontChange(Sender: TObject);
    procedure ChangeFonts;
    procedure DeleteFonts;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Units: TRulerUnit read fUnits write SetUnit;
    property Flat: Boolean read fFlat write SetFlat;
    property ScaleColor: TColor read fScaleColor write SetScaleColor;
    property TickColor: TColor read fTickColor write SetTickColor;
  end;


TplRuler = class(TplCustomRuler)
  private
    fDirection: TRulerDir;
    fScale: Integer;
    fScaleFactor: Double;
    fAdvance: Double;
    fHairLine: Boolean;
    fHairLinePos: Integer;
    fHairLineStyle: THairLineStyle;
    fOffset: Double;
    fShowMinus: Boolean;
    procedure SetDirection(const Value: TRulerDir);
    procedure SetScale(const Value: Integer);
    procedure SetHairLine(const Value: Boolean);
    procedure SetHairLinePos(const Value: Integer);
    procedure SetHairLineStyle(const Value: THairLineStyle);
    procedure SetOffset(const Value: Double);
    procedure SetShowMinus(const Value: Boolean);
  protected
    procedure SetUnit(const Value: TRulerUnit); override;
    procedure DrawHairLine;
    procedure CalcAdvance;
    procedure PaintScaleTics;
    procedure PaintScaleLabels;
    procedure Paint; override;
    function ConvertOffset(ToUnit: TRulerUnit): Double;
  public
    constructor Create(AOwner: TComponent); override;
    function Pos2Unit(APos: Integer): Double;
  published
    property Direction: TRulerDir read fDirection write SetDirection;
    property Units;
    property Scale: Integer read fScale write SetScale;
    property HairLine: Boolean read fHairLine write SetHairLine;
    property HairLinePos: Integer read fHairLinePos write SetHairLinePos;
    property HairLineStyle: THairLineStyle read fHairLineStyle write SetHairLineStyle;
    property ScaleColor;
    property TickColor;
    property Offset: Double read fOffset write SetOffset;
    property ShowMinus: Boolean read fShowMinus write SetShowMinus;
    property Align;
    property Font;
    property Color;
    property Height;
    property Width;
    property Visible;
    property Hint;
    property ShowHint;
    property Tag;
    property ParentFont;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnClick;
    property OnDblClick;
    property OnResize;
  end;

TplRulerCorner = class(TplCustomRuler)
  private
    fPosition: TCornerPos;
    procedure SetPosition(const Value: TCornerPos);
  protected
    fUStr: String;
    procedure Paint; override;
    procedure SetUnit(const Value: TRulerUnit); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align;
    property Position: TCornerPos read fPosition write SetPosition;
    property Flat;
    property ScaleColor;
    property TickColor;
    property Font;
    property Color;
    property Units;
    property Visible;
    property Hint;
    property ShowHint;
    property Tag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnClick;
    property OnDblClick;
    property OnResize;
  end;

implementation   

//============== TplCustomRuler =================================

constructor TplCustomRuler.Create(AOwner: TComponent);
begin
  inherited;
  // Initialize vars:
  fFlat := False;
  fUnits := ruCenti;
  fScaleColor := clWindow;
  fTickColor := clWindowText;
  FirstTime := True;
  OldFont := 0;
  NormFont := 0;
  LeftSideFont := 0;
  RightSideFont := 0;
  Font.OnChange := @FontChange;
end;

procedure TplCustomRuler.ChangeFonts;
begin
  DeleteFonts;
  // Fill LogFont structures:
  with LeftSideLF do
  begin
    FillChar(LeftSideLF, SizeOf(LeftSideLF), 0);
    lfEscapement := 900;
    lfOrientation := 900;
    StrPCopy(lfFaceName, Font.Name);
    lfHeight := -Font.Height;
    lfWeight := FW_BOLD * Integer(fsBold in Font.Style);
    lfItalic := Integer(fsItalic in Font.Style);
  end;
  with RightSideLF do
  begin
    FillChar(RightSideLF, SizeOf(RightSideLF), 0);
    lfEscapement := 2700;
    lfOrientation := 2700;
    StrPCopy(lfFaceName, Font.Name);
    lfHeight := -Font.Height;
    lfWeight := FW_BOLD * Integer(fsBold in Font.Style);
    lfItalic := Integer(fsItalic in Font.Style);
  end;
  with NormLF do
  begin
    FillChar(NormLF, SizeOf(NormLF), 0);
    StrPCopy(lfFaceName, Font.Name);
    lfHeight := -Font.Height;
    lfWeight := FW_BOLD * Integer(fsBold in Font.Style);
    lfItalic := Integer(fsItalic in Font.Style);
  end;
  Canvas.Font.Color := Font.Color;
  LeftSideFont := CreateFontIndirect(LeftSideLF);
  RightSideFont := CreateFontIndirect(RightSideLF);
  NormFont := CreateFontIndirect(NormLF);
end;

procedure TplCustomRuler.DeleteFonts;
begin
  if NormFont <> 0 then DeleteObject(NormFont);
  if LeftSideFont <> 0 then DeleteObject(LeftSideFont);
  if RightSideFont <> 0 then DeleteObject(RightSideFont);
end;

destructor TplCustomRuler.Destroy;
begin
  DeleteFonts;
  inherited;
end;

procedure TplCustomRuler.FontChange(Sender: TObject);
begin
  ChangeFonts;
  Invalidate;
end;

procedure TplCustomRuler.Paint;
begin
  Canvas.Brush.Color := Color;
  Canvas.FillRect(Rect(0, 0, Width, Height));
  if FirstTime then
  // setup fonts, cannot be done in Create method,
  // so do it when Ruler gets painted...
  begin
    FirstTime := False;
    ChangeFonts;
    OldFont := Canvas.Font.Handle;
  end;
end;

procedure TplCustomRuler.SetFlat(const Value: Boolean);
begin
  if Value <> fFlat then
  begin
    fFlat := Value;
    Invalidate;
  end;
end;

procedure TplCustomRuler.SetScaleColor(const Value: TColor);
begin
  if Value <> fScaleColor then
  begin
    fScaleColor := Value;
    Invalidate;
  end;
end;

procedure TplCustomRuler.SetTickColor(const Value: TColor);
begin
  if Value <> fTickColor then
  begin
    fTickColor := Value;
    Invalidate;
  end;
end;

procedure TplCustomRuler.SetUnit(const Value: TRulerUnit);
begin
  // method is empty, see descendants
end;


//=================== TplRuler ==============================
constructor TplRuler.Create(AOwner: TComponent);
begin
  inherited;
  fDirection := rdTop;
  fScale := 100;
  Height := 33;
  Width := 200;
  fScaleFactor := 1;
  fAdvance := 1;
  fOffset := 0.0;
  fHairLinePos := -1;
  fHairLine := False;
  fHairLineStyle := hlsLine;
  fShowMinus := True;
end;

procedure TplRuler.CalcAdvance;
begin
  fAdvance := Screen.PixelsPerInch / 10 * Scale / 100;
  if fUnits <> ruInch then fAdvance := fAdvance / 2.54;
  if fUnits = ruPixel then fAdvance := 5 * Scale / 100;
  case Scale of
    1: fScaleFactor := 100;
    2: fScaleFactor := 50;
    3..5: fScaleFactor := 25;
    6..8: fScaleFactor := 20;
    9..12: fScaleFactor := 10;
    13..25: fScaleFactor := 5;
    26..35: fScaleFactor := 4;
    36..50: fScaleFactor := 2;
    51..125: fScaleFactor := 1;
    126..300: fScaleFactor :=  0.5;
    301..400: fScaleFactor := 0.25;
    401..500: fScaleFactor := 0.2;
    501..1000: fScaleFactor := 0.1;
  end;
  fAdvance := fAdvance * fScaleFactor;
end;

procedure TplRuler.PaintScaleTics;
var
  Pos: Double;
  N, Last, LongTick: Integer;
begin
  if (fDirection = rdTop) or (fDirection = rdBottom) then Last := Width else Last := Height;
  Pos := 0;
  N := 0;
  Canvas.Pen.Color := fTickColor;
  while Pos < Last do with Canvas do
  begin
    LongTick := 2 * (3 + Integer(N mod 5 = 0));
    if (fDirection = rdTop) or (fDirection = rdBottom) then
    begin
      if fDirection = rdTop then
      begin
        MoveTo(Trunc(Pos), Height - 1);
        LineTo(Trunc(Pos), Height - LongTick);
      end;
      if fDirection = rdBottom then
      begin
        MoveTo(Trunc(Pos), 0);
        LineTo(Trunc(Pos), LongTick - 1);
      end;
    end else
    begin
      if fDirection = rdLeft then
      begin
        MoveTo(Width - 1, Trunc(Pos));
        LineTo(Width - LongTick, Trunc(Pos));
      end;
      if fDirection = rdRight then
      begin
        MoveTo(0, Trunc(Pos));
        LineTo(LongTick - 1, Trunc(Pos));
      end;
    end;
    Inc(N);
    Pos := Pos + 2 * fAdvance; // always advance two units to next ticmark
  end;
end;

procedure TplRuler.PaintScaleLabels;
var
  Pos, Number, ScaleN: Double;
  N, Last, Wi, He, Center: Integer;
  S: String;
begin
  if (fDirection = rdTop) or (fDirection = rdBottom) then Last := Width else Last := Height;
  Pos := 0;
  N := 0;
  Canvas.Pen.Color := Font.Color;
  while Pos < Last do with Canvas do
  begin
    Number := fScaleFactor * N / 10;
    if Units = ruMilli then Number := 10 * Number;
    if Units = ruPixel then Number := 50 * Number;
    ScaleN := Number + fOffset;
    if fUnits = ruPixel then ScaleN := Round(ScaleN);
    if fUnits = ruInch then ScaleN := Round(100 * ScaleN) / 100;
    if fShowMinus then S := FormatFloat('0.##', ScaleN) else S := FormatFloat('0.##', Abs(ScaleN));
    Wi := TextWidth(S);
    He := TextHeight(S);
    if (fDirection = rdTop) or (fDirection = rdBottom) then
    begin
      MoveTo(Trunc(Pos), 1);  // only Pos is important
      if fDirection = rdTop then
      begin
        // draw number..
        if (N > 0) and (N mod 10 = 0) then TextOut(PenPos.X - Wi div 2, Height - He - 8, S)
        else if (N > 0) and (N mod 5 = 0) then
        begin
          // or just a notch
          Center := Height + (-(He + 6) - 8) div 2;
          MoveTo(Trunc(Pos), Center - 1);
          LineTo(Trunc(Pos), Center + 2);
        end;
      end;
      if fDirection = rdBottom then
      begin
        // draw number..
        if (N > 0) and (N mod 10 = 0) then TextOut(PenPos.X - Wi div 2, 8, S)
        else if (N > 0) and (N mod 5 = 0) then
        begin
          // or just a notch
          Center := ((He + 6) + 8) div 2;
          MoveTo(Trunc(Pos), Center - 2);
          LineTo(Trunc(Pos), Center + 1);
        end;
      end;
    end else
    begin
      MoveTo(1, Trunc(Pos));
      if fDirection = rdLeft then
      begin
        // draw number..
        if (N > 0) and (N mod 10 = 0) then TextOut(Width - He - 7, PenPos.Y + Wi div 2, S)
        else if (N > 0) and (N mod 5 = 0) then
        begin
          // or just a notch
          Center := Width + (-(He + 6) - 8) div 2;
          MoveTo(Center - 1, Trunc(Pos));
          LineTo(Center + 2, Trunc(Pos));
        end;
      end;
      if fDirection = rdRight then
      begin
        if (N > 0) and (N mod 10 = 0) then TextOut(He + 7, PenPos.Y - Wi div 2, S)
        else if (N > 0) and (N mod 5 = 0) then
        begin
          // or just a notch
          Center := ((He + 6) + 8) div 2;
          MoveTo(Center - 2, Trunc(Pos));
          LineTo(Center + 1, Trunc(Pos));
        end;
      end;
    end;
    Inc(N);
    Pos := Pos + fAdvance;
  end;
end;

procedure TplRuler.Paint;
var
  Rect: TRect;
  He, d: Integer;
begin
  inherited;
  fHairLinePos := -1;
  Rect := ClientRect;
  if Not Flat then DrawEdge(Canvas.Handle, Rect, EDGE_RAISED, BF_RECT);
  d := 2 - Integer(Flat);
  SelectObject(Canvas.Handle, NormFont);
  He := Canvas.TextHeight('0') + 6;
  if (fDirection = rdTop) or (fDirection = rdBottom) then
  begin
    if fDirection = rdTop then SetRect(Rect, d, Height - He - 1, Width - d, Height - 8);
    if (fDirection = rdBottom) then SetRect(Rect, d, 8, Width - d, He + 1);
    SelectObject(Canvas.Handle, NormFont);
  end else
  begin
    if fDirection = rdLeft then
    begin
      SetRect(Rect, Width - He, d, Width - 8, Height - d);
      SelectObject(Canvas.Handle, LeftSideFont);
    end;
    if fDirection = rdRight then
    begin
      SetRect(Rect, He, d, 8, Height - d);
      SelectObject(Canvas.Handle, RightSideFont);
    end;
  end;
  Canvas.Brush.Color := fScaleColor;
  Canvas.FillRect(Rect);
  CalcAdvance;
  SetBKMode(Canvas.Handle, TRANSPARENT);
  PaintScaleTics;
  PaintScaleLabels;
  SetBKMode(Canvas.Handle, OPAQUE);
  SelectObject(Canvas.Handle, OldFont);
end;

procedure TplRuler.SetDirection(const Value: TRulerDir);
var
  Dim: TPoint;
  OldDir: TRulerDir;
begin
  OldDir := fDirection;
  if Value <> fDirection then
  begin
    if ((OldDir = rdTop) or (OldDir = rdBottom)) and ((Value = rdLeft) or (Value = rdRight))
    or ((OldDir = rdLeft) or (OldDir = rdRight)) and ((Value = rdTop) or (Value = rdBottom)) then
    begin
      Dim := Point(Width, Height);
      Width := Dim.Y;
      Height := Dim.X;
    end;
    fDirection := Value;
    Invalidate;
  end;
end;

procedure TplRuler.SetScale(const Value: Integer);
begin
  if (Value <> fScale) and (Value > 0) then
  begin
    fScale := Value;
    Invalidate;
  end;
end;

procedure TplRuler.SetUnit(const Value: TRulerUnit);
begin
  if Value <> fUnits then
  begin
    fOffSet := ConvertOffset(Value);
    fUnits := Value;
    Invalidate;
  end;
end;


procedure TplRuler.SetHairLine(const Value: Boolean);
begin
  if Value <> fHairLine then
  begin
    fHairLine := Value;
    Invalidate;
  end;
end;

procedure TplRuler.SetHairLinePos(const Value: Integer);
begin
  if Value <> fHairLinePos then
  begin
    DrawHairLine; // erase old position
    fHairLinePos := Value;
    DrawHairLine; // draw new position
  end;
end;

procedure TplRuler.DrawHairLine;
var
  He: Integer;
  //-------------------------------------------------------------  ct9999
   Procedure InvertRect(aHandle:THandle; const aRect:Trect);
   begin

   end;
  //-------------------------------------------------------------
begin
  if fHairLine then if fHairLinePos <> -1 then with Canvas do
  begin
    Pen.Mode := pmNotXOr;
    SelectObject(Canvas.Handle, NormFont);
    He := TextHeight('0') + 6;
    SelectObject(Canvas.Handle, OldFont);
    if fDirection = rdTop then
    begin
      if fHairLineStyle = hlsLine
      then InvertRect(Canvas.Handle, Rect(fHairLinePos - 1, Height - He - 1, fHairLinePos, Height - 8))
      else InvertRect(Canvas.Handle, Rect(1, Height - He - 1, fHairLinePos, Height - 8));
    end;
    if fDirection = rdBottom then
    begin
      if fHairLineStyle = hlsLine
      then InvertRect(Canvas.Handle, Rect(fHairLinePos - 1, 8, fHairLinePos, He))
      else InvertRect(Canvas.Handle, Rect(1, 8, fHairLinePos, He + 1));
    end;
    if fDirection = rdLeft then
    begin
      if fHairLineStyle = hlsLine
      then InvertRect(Canvas.Handle, Rect(Width - He, fHairLinePos - 1, Width - 8, fHairLinePos))
      else InvertRect(Canvas.Handle, Rect(Width - He, 1, Width - 8, fHairLinePos));
    end;
    if fDirection = rdRight then
    begin
      if fHairLineStyle = hlsLine
      then InvertRect(Canvas.Handle, Rect(8, fHairLinePos - 1, He, fHairLinePos))
      else InvertRect(Canvas.Handle, Rect(8, 1, He, fHairLinePos));
    end;
    Pen.Mode := pmCopy;
  end;
end;

procedure TplRuler.SetHairLineStyle(const Value: THairLineStyle);
begin
  if Value <> fHairLineStyle then
  begin
    fHairLineStyle := Value;
    Invalidate;
  end;
end;

function TplRuler.Pos2Unit(APos: Integer): Double;
begin
  Result := fOffset;
  if fUnits = ruPixel then Result := Trunc(Result) + Trunc(APos / Scale * 100); // zero-based counting of pixels
  if fUnits = ruInch  then Result := Result + APos / Scale * 100 / Screen.PixelsPerInch;
  if fUnits = ruCenti then Result := Result + APos / Scale * 100 / Screen.PixelsPerInch * 2.54;
  if fUnits = ruMilli then Result := Result + APos / Scale * 100 / Screen.PixelsPerInch * 25.4;
end;

procedure TplRuler.SetOffset(const Value: Double);
begin
  if Value <> fOffset then
  begin
    fOffset := Value;
    Invalidate;
  end;
end;

procedure TplRuler.SetShowMinus(const Value: Boolean);
begin
  if Value <> fShowMinus then
  begin
    fShowMinus := Value;
    Invalidate;
  end;
end;

function TplRuler.ConvertOffset(ToUnit: TRulerUnit): Double;
var
  DivFactor, MulFactor: Double;
begin
  DivFactor := 1; // std: ruMilli
  if (fUnits = ruCenti) then DivFactor := 0.1;
  if (fUnits = ruInch) then DivFactor := 1 / 25.4;
  if (fUnits = rusec) then DivFactor := 1 / 25.4;
  if (fUnits = ruPixel) then DivFactor := Screen.PixelsPerInch / 25.4;
  MulFactor := 1;
  if (ToUnit = ruCenti) then MulFactor := 0.1;
  if (ToUnit = ruMilli) then MulFactor := 1;
  if (ToUnit = ruInch) then MulFactor := 1 / 25.4;
  if (ToUnit = rusec) then MulFactor := 1 / 25.4;
  if (ToUnit = ruPixel) then MulFactor := Screen.PixelsPerInch / 25.4;
  Result := fOffset / DivFactor * MulFactor;
end;

{ TplRulerCorner }

constructor TplRulerCorner.Create(AOwner: TComponent);
begin
  inherited;
  fPosition := cpLeftTop;
  fUStr := Centi;
  Width := 24;
  Height := 24;
  Hint := 'centimeter';
end;

procedure TplRulerCorner.Paint;
var
   Wi, He, d: Integer;
  R: TRect;
begin
  inherited;
  R := ClientRect;
  SelectObject(Canvas.Handle, NormFont);
  with Canvas do
  begin
    if Not Flat then DrawEdge(Handle, R, EDGE_RAISED, BF_RECT);
    Brush.Color := fScaleColor;
    He := TextHeight('0') + 6;
    SetBKMode(Handle, TRANSPARENT);
    Canvas.Font.Color := Font.Color;
    Wi := TextWidth(fUStr);
    d := 2 - Integer(Flat);
    if fPosition = cpLeftTop then
    begin
      FillRect(Rect(Width - He, Height - He - 1, Width - d, Height - 8));
      FillRect(Rect(Width - He, Height - He, Width - 8, Height - d));
      TextOut(Width - He + 1 + (He - 2 - Wi) div 2, Height - He - 1, fUStr);
    end;
    if fPosition = cpRightTop then
    begin
      FillRect(Rect(d, Height - He - 1, He, Height - 8));
      FillRect(Rect(8, Height - He, He, Height - d));
      TextOut(2 + (He - Wi) div 2, Height - He, fUStr);
    end;
    if fPosition = cpLeftBottom then
    begin
      FillRect(Rect(Width - He, 8, Width - d, He + 1));
      FillRect(Rect(Width - He, d, Width - 8, He));
      TextOut(Width - He + 1 + (He - 2 - Wi) div 2, 8, fUStr);
    end;
    if fPosition = cpRightBottom then
    begin
      FillRect(Rect(d, 8, He, He + 1));
      FillRect(Rect(8, d, He, He));
      TextOut(2 + (He - Wi) div 2, 8, fUStr);
    end;
  end;
  //Canvas.Font.Height := OrgH;
  SetBKMode(Canvas.Handle, OPAQUE);
  SelectObject(Canvas.Handle, OldFont);
end;



procedure TplRulerCorner.SetPosition(const Value: TCornerPos);
begin
  if Value <> fPosition then
  begin
    fPosition := Value;
    Invalidate;
  end;
end;

procedure TplRulerCorner.SetUnit(const Value: TRulerUnit);
begin
  if Value <> fUnits then
  begin
    fUnits := Value;
    if fUnits = ruCenti then begin fUStr := Centi; Hint := 'centimeter'; end;
    if fUnits = ruMilli then begin fUStr := Milli; Hint := 'millimeter'; end;
    if fUnits = ruInch then begin fUStr := Inch; Hint := 'inch'; end;
    if fUnits = ruPixel then begin fUStr := Pixel; Hint := 'pixel'; end;
    if fUnits = ruNone then begin fUStr := None; Hint := ''; end;
    if fUnits = ruSec then begin fUStr := 'Sec'; Hint := 'sec'; end;
    Invalidate;
  end;
end;



end.
