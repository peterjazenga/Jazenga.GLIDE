
{**********************************************************************
 Package pl_ExControls.pkg
 From PilotLogic Software House(http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit TplGaugeUnit;

interface

{$I AllExControlsRegister.inc}

uses
  SysUtils, Messages, LMessages, Classes, Graphics, Controls,
  Forms, StdCtrls, ExtCtrls, LCLType, plUtils;

type
  TplGauge = class(TGraphicControl)
  private
    FTransparent: boolean;
    FUseAdvColors: boolean;
    FAdvColorBorder: TAdvColors;
    FBarColor, FBorderColor: TColor;
    FMinValue, FMaxValue, FProgress: longint;
    FShowText: boolean;
    procedure SetShowText(Value: boolean);
    procedure SetMinValue(Value: longint);
    procedure SetMaxValue(Value: longint);
    procedure SetProgress(Value: longint);
    procedure SetColors(Index: integer; Value: TColor);
    procedure SetAdvColors(Index: integer; Value: TAdvColors);
    procedure SetUseAdvColors(Value: boolean);
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMParentColorChanged(var Message: TLMessage); message CM_PARENTCOLORCHANGED;
    procedure SetTransparent(const Value: boolean);
  protected
    procedure CalcAdvColors;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published

    property Transparent: boolean read FTransparent write SetTransparent default False;
    property Color default $00E0E9EF;
    property BarColor: TColor index 1 read FBarColor write SetColors default $00996633;
    property MinValue: longint read FMinValue write SetMinValue default 0;
    property MaxValue: longint read FMaxValue write SetMaxValue default 100;
    property Progress: longint read FProgress write SetProgress;
    property ShowText: boolean read FShowText write SetShowText default True;
    property Align;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property ShowHint;
    property Visible;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property Anchors;
    property BiDiMode write SetBidiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;

  end;

implementation

constructor TplGauge.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 145;
  Height := 15;
  MinValue := 0;
  MaxValue := 100;
  Progress := 25;
  ShowText := True;
  BarColor := $00996633;

  FBorderColor := DefiColorBorder;
  ParentColor := False;

  FUseAdvColors := DefiUseAdvColors;
  FAdvColorBorder := DefiAdvColorBorder;
end;

procedure TplGauge.Paint;
var
  DrawRect, BarRect, r: TRect;
  PercentText: string;
begin
  DrawRect := ClientRect;

  // Clear Background
  if not FTransparent then
  begin
    Canvas.Brush.Color := Color;
    Canvas.FillRect(DrawRect);
  end;

  if DefiDrawFlat then
  begin
    Frame3DBorder(Canvas, ClientRect, FBorderColor, FBorderColor, 1);
  end
  else
  begin
    //----------- Draw 3D ------------------
    r := ClientRect;
    canvas.pen.Color := clGray;
    canvas.MoveTo(0, 0);
    canvas.LineTo(R.Right, 0);
    canvas.MoveTo(0, 0);
    canvas.LineTo(0, r.Bottom);

    canvas.pen.Color := clWhite;
    canvas.MoveTo(0, r.Bottom + 1);
    canvas.LineTo(R.Right + 2, r.Bottom + 1);
    canvas.MoveTo(R.Right + 1, 0);
    canvas.LineTo(r.Right + 1, r.Bottom + 1);
  end;


  // Calculate the Rect
  InflateRect(DrawRect, -2, -2);

  if BidiMode = bdRightToLeft then
    BarRect := Rect(DrawRect.right - Trunc((DrawRect.right - DrawRect.left) / (FMaxValue - FMinValue) * FProgress),
      DrawRect.top, DrawRect.right, DrawRect.bottom)
  else
    BarRect := Rect(DrawRect.left, DrawRect.top, DrawRect.left + Trunc((DrawRect.right - DrawRect.left) /
      (FMaxValue - FMinValue) * FProgress), DrawRect.bottom);

  // Fill the Rect
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := FBarColor;
  Canvas.FillRect(BarRect);

  // Draw Text
  if FShowText then
  begin
    PercentText := IntToStr(Trunc(((FProgress - FMinValue) / (FMaxValue - FMinValue)) * 100)) + ' %';

    Canvas.Font.Assign(Self.Font);
    Canvas.Brush.Style := bsClear;
    DrawTextInRect(Canvas, DrawRect, PercentText);
  end;
end;

procedure TplGauge.SetShowText(Value: boolean);
begin
  if FShowText <> Value then
  begin
    FShowText := Value;
    Repaint;
  end;
end;

procedure TplGauge.SetMinValue(Value: longint);
begin
  if Value <> FMinValue then
  begin
    if Value > FMaxValue then
      FMinValue := FMaxValue
    else
      FMinValue := Value;
    if FProgress < Value then
      FProgress := Value;
    Repaint;
  end;
end;

procedure TplGauge.SetMaxValue(Value: longint);
begin
  if Value <> FMaxValue then
  begin
    if Value < FMinValue then
      FMaxValue := FMinValue
    else
      FMaxValue := Value;
    if FProgress > Value then
      FProgress := Value;
    Repaint;
  end;
end;

procedure TplGauge.SetProgress(Value: longint);
begin
  if Value < FMinValue then
    Value := FMinValue
  else
  if Value > FMaxValue then
    Value := FMaxValue;
  if FProgress <> Value then
  begin
    FProgress := Value;
    Repaint;
  end;
end;

procedure TplGauge.SetColors(Index: integer; Value: TColor);
begin
  case Index of
    0: FBorderColor := Value;
    1: FBarColor := Value;
  end;
  Invalidate;
end;

procedure TplGauge.CalcAdvColors;
begin
  if FUseAdvColors then
  begin
    FBorderColor := CalcAdvancedColor(Color, FBorderColor, FAdvColorBorder, darken);
  end;
end;

procedure TplGauge.SetAdvColors(Index: integer; Value: TAdvColors);
begin
  case Index of
    0: FAdvColorBorder := Value;
  end;
  CalcAdvColors;
  Invalidate;
end;

procedure TplGauge.SetUseAdvColors(Value: boolean);
begin
  if Value <> FUseAdvColors then
  begin
    FUseAdvColors := Value;
    ParentColor := Value;
    CalcAdvColors;
    Invalidate;
  end;
end;

procedure TplGauge.CMSysColorChange(var Message: TMessage);
begin
  if FUseAdvColors then
  begin
    ParentColor := True;
    CalcAdvColors;
  end;
  Invalidate;
end;

procedure TplGauge.CMParentColorChanged(var Message: TLMessage);
begin
  inherited;
  if FUseAdvColors then
  begin
    ParentColor := True;
    CalcAdvColors;
  end;
  Invalidate;
end;

procedure TplGauge.SetTransparent(const Value: boolean);
begin
  FTransparent := Value;
  Invalidate;
end;


end.
