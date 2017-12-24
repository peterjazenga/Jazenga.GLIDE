
{**********************************************************************
 Package pl_ExControls.pkg
 From PilotLogic Software House(http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit TplGradientUnit;

interface

uses
  Classes, Messages,LMessages, SysUtils, LResources, Forms, Controls, Graphics,
  lcltype, LCLIntf, Dialogs;

type

  TsBevelStyle = (bvNone, bvLowered, bvRaised);
  TGradDirection = (gdDownRight, gdUpLeft);
  TGradStyle = (gsCornerTopLeft, gsCornerTopRight,
    gsCornerBottomRight, gsCornerBottomLeft,
    gsDiagonalRising, gsDiagonalFalling,
    gsEllipse, gsHorizontal, gsPyramid, gsVertical);
  TStepWidth = 1..10;

  TplGradient = class(TGraphicControl)
  private
    FBC: array[0..255] of longint;
    FBevelWidth: TStepWidth;
    FBevelStyle: TsBevelStyle;
    FBitmap: TBitmap;
    FBuffered: boolean;
    FDirection: TGradDirection;
    FColorStart: TColor;
    FColorEnd: TColor;
    FOldWidth, FOldHeight: integer;
    FStepWidth: TStepWidth;
    FStyle: TGradStyle;
    procedure LoadColors;
    procedure DrawBevel(ACanvas: TCanvas; var ARect: TRect);
    procedure DrawGradient(ACanvas: TCanvas; const ARect: TRect);
    procedure SetBevelWidth(newValue: TStepWidth);
    procedure SetBevelStyle(newValue: TsBevelStyle);
    procedure SetBuffered(newValue: boolean);
    procedure SetDirection(newValue: TGradDirection);
    procedure SetColorEnd(newValue: TColor);
    procedure SetColorStart(newValue: TColor);
    procedure SetStepWidth(newValue: TStepWidth);
    procedure SetStyle(newValue: TGradStyle);
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
  protected
    procedure Paint; override;
  public
    constructor Create(AComponent: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
  published
    property Align;
    property Anchors;
    property BevelWidth: TStepWidth read FBevelWidth write SetBevelWidth;
    property BevelStyle: TsBevelStyle read FBevelStyle write SetBevelStyle;
    property Buffered: boolean read FBuffered write SetBuffered;
    property Direction: TGradDirection read FDirection write SetDirection;
    property ColorStart: TColor read FColorStart write SetColorStart;
    property ColorEnd: TColor read FColorEnd write SetColorEnd;
    property StepWidth: TStepWidth read FStepWidth write SetStepWidth;
    property Style: TGradStyle read FStyle write SetStyle;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;


implementation


constructor TplGradient.Create(AComponent: TComponent);
begin
  inherited Create(AComponent);

  FBevelWidth := 1;
  FBevelStyle := bvNone;
  FBuffered := True;
  FColorEnd := clBlack;
  FDirection := gdDownRight;
  FColorStart := clBlue;
  FStepWidth := 1;
  FStyle := gsVertical;
  Width := 100;
  Height := 80;
  FOldWidth := 0;
  FOldHeight := 0;
  FBitmap := TBitmap.Create;
  LoadColors;
end;

destructor TplGradient.Destroy;
begin
  if FBuffered and assigned(FBitmap) then
  begin
    FBitmap.Free;
    FBitmap := nil;
  end;
  inherited Destroy;
end;

procedure TplGradient.DrawBevel(ACanvas: TCanvas; var ARect: TRect);
var
  i: TStepWidth;
begin

  if (FBevelStyle <> bvNone) and (FBevelWidth > 0) then
  begin
    if (FBevelStyle = bvLowered) or (FBevelStyle = bvRaised) then
    begin
      with ACanvas do
      begin
        Pen.Style := psSolid;
        if FBevelStyle = bvLowered then
          Pen.Color := clBtnShadow
        else
          Pen.Color := clBtnHighlight;
        for i := 1 to FBevelWidth do
        begin
          MoveTo(ARect.Right - i, ARect.Top + i - 1);
          LineTo(ARect.Left + i - 1, ARect.Top + i - 1);
          LineTo(ARect.Left + i - 1, ARect.Bottom - i);
        end;
        if FBevelStyle = bvLowered then
          Pen.Color := clBtnHighlight
        else
          Pen.Color := clBtnShadow;
        for i := 1 to FBevelWidth do
        begin
          MoveTo(ARect.Right - i, ARect.Top + i - 1);
          LineTo(ARect.Right - i, ARect.Bottom - i);
          LineTo(ARect.Left + i - 1, ARect.Bottom - i);
        end;
      end;
    end;
    InflateRect(ARect, -FBevelWidth, -FBevelWidth);
  end;
end;

procedure TplGradient.DrawGradient(ACanvas: TCanvas; const ARect: TRect);
var
  TempRect: TRect;
  TempStepV, TempStepH: single;
  ColorCode, TempLeft, TempTop, TempHeight, TempWidth, AWidth, AHeight, ECount, i: integer;
  CornerPnts: array [0..5] of TPoint;
  DiagArray: array [0..255, 0..3] of TPoint;
begin
  AWidth := ARect.Right - ARect.Left;
  AHeight := ARect.Bottom - ARect.Top;
  if FBuffered and (FStyle = gsEllipse) then
  begin
    with ACanvas do
    begin
      Brush.Color := clSilver;
      FillRect(ARect);
    end;
  end;
  if FStyle in [gsHorizontal, gsVertical, gsCornerTopLeft, gsCornerTopRight,
    gsCornerBottomRight, gsCornerBottomLeft] then
  begin
    TempStepH := AWidth / 255;
    TempStepV := AHeight / 255;
    TempHeight := Trunc(TempStepV + 1);
    TempWidth := Trunc(TempStepH + 1);
    with ACanvas do
    begin
      TempLeft := ARect.Left;
      TempTop := ARect.Top;
      TempRect := ARect;
      if not (Fstyle in [gsVertical, gsHorizontal]) then
        pen.Style := psClear;
      for ColorCode := 0 to 255 do
      begin
        Brush.Color := FBC[ColorCode];

        if FStyle = gsVertical then
        begin
          TempRect.Top := TempTop;
          TempRect.Bottom := TempTop + TempHeight;
        end

        else if FStyle = gsHorizontal then
        begin
          TempRect.Left := TempLeft;
          TempRect.Right := TempLeft + TempWidth;
        end

        else if FStyle = gsCornerTopLeft then
        begin
          TempTop := ARect.Top + Trunc(TempStepV * (255 - ColorCode));
          TempLeft := ARect.Left + Trunc(TempStepH * (255 - ColorCode));
          CornerPnts[0] := Point(ARect.Left, TempTop);
          CornerPnts[1] := Point(TempLeft, TempTop);
          CornerPnts[2] := Point(TempLeft, ARect.Top);
          CornerPnts[3] := Point(TempLeft + TempWidth, ARect.Top);
          CornerPnts[4] := Point(TempLeft + TempWidth, TempTop + TempHeight);
          CornerPnts[5] := Point(ARect.Left, TempTop + TempHeight);
        end

        else if FStyle = gsCornerTopRight then
        begin
          TempTop := ARect.Top + Trunc(TempStepV * (255 - ColorCode));
          TempLeft := ARect.Left + Trunc(TempStepH * ColorCode);
          CornerPnts[0] := Point(TempLeft + TempWidth, ARect.Top);
          CornerPnts[1] := Point(TempLeft + TempWidth, TempTop);
          CornerPnts[2] := Point(ARect.Right, TempTop);
          CornerPnts[3] := Point(ARect.Right, TempTop + TempHeight);
          CornerPnts[4] := Point(TempLeft, TempTop + TempHeight);
          CornerPnts[5] := Point(TempLeft, ARect.Top);
        end

        else if FStyle = gsCornerBottomRight then
        begin
          TempTop := ARect.Top + Trunc(TempStepV * ColorCode);
          TempLeft := ARect.Left + Trunc(TempStepH * ColorCode);
          CornerPnts[0] := Point(ARect.Right, TempTop + TempHeight);
          CornerPnts[1] := Point(TempLeft + TempWidth, TempTop + TempHeight);
          CornerPnts[2] := Point(TempLeft + TempWidth, ARect.Bottom);
          CornerPnts[3] := Point(TempLeft, ARect.Bottom);
          CornerPnts[4] := Point(TempLeft, TempTop);
          CornerPnts[5] := Point(ARect.Right, TempTop);
        end

        else if FStyle = gsCornerBottomLeft then
        begin
          TempTop := ARect.Top + Trunc(TempStepV * ColorCode);
          TempLeft := ARect.Left + Trunc(TempStepH * (255 - ColorCode));
          CornerPnts[0] := Point(TempLeft, ARect.Bottom);
          CornerPnts[1] := Point(TempLeft, TempTop + TempHeight);
          CornerPnts[2] := Point(ARect.Left, TempTop + TempHeight);
          CornerPnts[3] := Point(ARect.Left, TempTop);
          CornerPnts[4] := Point(TempLeft + TempWidth, TempTop);
          CornerPnts[5] := Point(TempLeft + TempWidth, ARect.Bottom);
        end;

        if FStyle in [gsVertical, gsHorizontal] then
          FillRect(TempRect)
        else
          Polygon(CornerPnts);

        if FStyle = gsVertical then
          TempTop := ARect.Top + Trunc(TempStepV * ColorCode)
        else if FStyle = gsHorizontal then
          TempLeft := ARect.Left + Trunc(TempStepH * ColorCode);
      end;
    end;
  end;
  if FStyle in [gsDiagonalFalling, gsDiagonalRising] then
  begin
    TempStepH := AWidth / 127;
    TempStepV := AHeight / 127;
    TempHeight := Trunc(TempStepV + 1);
    TempWidth := Trunc(TempStepH + 1);

    if FStyle = gsDiagonalFalling then
    begin
      for i := 0 to 127 do
      begin
        TempLeft := ARect.Left + Trunc(TempStepH * i);
        Diagarray[i, 0] := Point(TempLeft, ARect.Top);
        Diagarray[i, 1] := Point(TempLeft + TempWidth, ARect.Top);
        Diagarray[i + 128, 3] := Point(TempLeft, ARect.Bottom);
        Diagarray[i + 128, 2] := Point(TempLeft + TempWidth, ARect.Bottom);
      end;
      for i := 0 to 127 do
      begin
        TempTop := ARect.Top + Trunc(TempStepV * i);
        Diagarray[i, 3] := Point(ARect.Left, TempTop);
        Diagarray[i, 2] := Point(ARect.Left, TempTop + TempHeight);
        Diagarray[i + 128, 0] := Point(ARect.Right, TempTop);
        Diagarray[i + 128, 1] := Point(ARect.Right, TempTop + TempHeight);
      end;
    end

    else
    begin
      for i := 0 to 127 do
      begin
        TempLeft := ARect.Left + Trunc(TempStepH * i);
        Diagarray[i, 0] := Point(TempLeft, ARect.Bottom);
        Diagarray[i, 1] := Point(TempLeft + TempWidth, ARect.Bottom);
        Diagarray[i + 128, 3] := Point(TempLeft, ARect.Top);
        Diagarray[i + 128, 2] := Point(TempLeft + TempWidth, ARect.Top);
      end;
      for i := 0 to 127 do
      begin
        TempTop := ARect.Top + Trunc(TempStepV * (127 - i));
        Diagarray[i, 3] := Point(ARect.Left, TempTop + TempHeight);
        Diagarray[i, 2] := Point(ARect.Left, TempTop);
        Diagarray[i + 128, 0] := Point(ARect.Right, TempTop + TempHeight);
        Diagarray[i + 128, 1] := Point(ARect.Right, TempTop);
      end;
    end;

    with ACanvas do
    begin
      Pen.Style := psClear;
      for ColorCode := 0 to 255 do
      begin
        Brush.Color := FBC[ColorCode];
        Polygon(Diagarray[ColorCode]);
      end;
    end;
  end;

  if FStyle = gsEllipse then
  begin
    with ACanvas do
    begin
      TempLeft := ARect.Left + 1;
      TempTop := ARect.Top + 1;
      Pen.Width := 1;
      Pen.Style := psClear;
      ECount := (AWidth div 2) - 2;
      TempStepV := AHeight / AWidth;
      TempStepH := 255 / ECount;
      i := 2;
      while i < ECount do
      begin
        ColorCode := trunc(TempStepH * i);
        Brush.Color := FBC[ColorCode];
        Ellipse(TempLeft, TempTop, Width - TempLeft, Height - TempTop);
        TempTop := ARect.Top + Trunc(TempStepV * i);
        TempLeft := ARect.Left + i;
        i := i + FStepWidth;
      end;
    end;
  end;

  if FStyle = gsPyramid then
  begin
    with ACanvas do
    begin
      TempLeft := AWidth div 2;
      TempTop := AHeight div 2;
      Pen.Width := FStepWidth;
      Pen.Style := psSolid;
      ECount := AWidth + AHeight;
      TempStepH := 255 / ECount;
      i := 0;
      while i <= AWidth do
      begin
        ColorCode := trunc(TempStepH * i);
        Pen.Color := FBC[ColorCode];
        MoveTo(i, 0);
        LineTo(TempLeft, TempTop);
        ColorCode := trunc(TempStepH * (i + AHeight));
        Pen.Color := FBC[ColorCode];
        LineTo(i, AHeight);
        i := i + FStepWidth;
      end;
      i := 0;
      while i <= AHeight do
      begin
        ColorCode := trunc(TempStepH * (i + AWidth));
        Pen.Color := FBC[ColorCode];
        MoveTo(AWidth, i);
        LineTo(TempLeft, TempTop);
        ColorCode := trunc(TempStepH * i);
        Pen.Color := FBC[ColorCode];
        LineTo(-1, i);
        i := i + FStepWidth;
      end;
    end;
  end;
end;

procedure TplGradient.Loaded;
begin
  inherited Loaded;
end;

procedure TplGradient.LoadColors;
var
  X, YR, YG, YB, SR, SG, SB, DR, DG, DB, StartClr, EndClr: integer;
begin
  StartClr := ColorToRGB(FColorStart);
  YR := GetRValue(StartClr);
  YG := GetGValue(StartClr);
  YB := GetBValue(StartClr);
  SR := YR;
  SG := YG;
  SB := YB;
  EndClr := ColorToRGB(FColorEnd);
  DR := GetRValue(EndClr) - SR;
  DG := GetGValue(EndClr) - SG;
  DB := GetBValue(EndClr) - SB;
  if (FDirection = gdDownRight) then
    for X := 0 to 255 do
    begin
      FBC[X] := RGB(YR, YG, YB);
      YR := SR + round(DR / 255 * X);
      YG := SG + round(DG / 255 * X);
      YB := SB + round(DB / 255 * X);
    end
  else
    for X := 255 downto 0 do
    begin
      FBC[X] := RGB(YR, YG, YB);
      YR := SR + round(DR / 255 * (255 - X));
      YG := SG + round(DG / 255 * (255 - X));
      YB := SB + round(DB / 255 * (255 - X));
    end;
end;

procedure TplGradient.Paint;
var
  BmpRect, ARect: TRect;
begin
  ARect := GetClientRect;
  if FBuffered and assigned(FBitmap) then
  begin
    if (FOldWidth <> Width) or (FOldHeight <> Height) then
    begin
      FOldWidth := Width;
      FOldHeight := Height;
      FBitmap.Width := Width;
      FBitmap.Height := Height;
      DrawBevel(FBitmap.Canvas, ARect);
      DrawGradient(FBitmap.Canvas, ARect);
    end;
    if FStyle = gsEllipse then
    begin
      BmpRect := Rect(0, 0, Self.Width - 1, Self.Height - 1);
      with Self.Canvas do
      begin
        Brush.Style := bsClear;
        FillRect(BmpRect);
        // BrushCopy(BmpRect, FBitmap, BmpRect, clSilver);
      end;
    end
    else
      BitBlT(Self.Canvas.Handle,
        0, 0, Width, Height,
        FBitmap.Canvas.Handle,
        0, 0, SrcCopy);
  end
  else
  begin
    DrawBevel(Self.Canvas, ARect);
    DrawGradient(Self.Canvas, ARect);
  end;
end;

procedure TplGradient.SetBevelWidth(newValue: TStepWidth);
begin
  if FBevelWidth <> newValue then
  begin
    FBevelWidth := newValue;
    FOldWidth := 0;
    Invalidate;
  end;
end;

procedure TplGradient.SetBevelStyle(newValue: TsBevelStyle);
begin
  if FBevelStyle <> newValue then
  begin
    FBevelStyle := newValue;
    FOldWidth := 0;
    Invalidate;
  end;
end;

procedure TplGradient.SetBuffered(newValue: boolean);
begin
  if FBuffered <> newValue then
  begin
    FBuffered := newValue;
    if FBuffered and not assigned(FBitmap) then
      FBitmap := TBitmap.Create;
    if not FBuffered and assigned(FBitmap) then
    begin
      FBitmap.Free;
      FBitmap := nil;
    end;
    FOldWidth := 0;
    Invalidate;
  end;
end;

procedure TplGradient.SetDirection(newValue: TGradDirection);
begin
  if FDirection <> newValue then
  begin
    FDirection := newValue;
    FOldWidth := 0;
    LoadColors;
    Invalidate;
  end;
end;

procedure TplGradient.SetColorEnd(newValue: TColor);
begin
  if FColorEnd <> newValue then
  begin
    FColorEnd := newValue;
    FOldWidth := 0;
    LoadColors;
    Invalidate;
  end;
end;

procedure TplGradient.SetColorStart(newValue: TColor);
begin
  if FColorStart <> newValue then
  begin
    FColorStart := newValue;
    FOldWidth := 0;
    LoadColors;
    Invalidate;
  end;
end;

procedure TplGradient.SetStepWidth(newValue: TStepWidth);
begin
  if (FStepWidth <> newValue) and (newValue >= 1) and (newValue <= 10) then
  begin
    FStepWidth := newValue;
    FOldWidth := 0;
    Invalidate;
  end;
end;

procedure TplGradient.SetStyle(newValue: TGradStyle);
begin
  if FStyle <> newValue then
  begin
    FStyle := newValue;
    FOldWidth := 0;
    Invalidate;
  end;
end;

procedure TplGradient.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
  Message.Result := 1;
end;

end.
