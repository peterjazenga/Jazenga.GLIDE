
{**********************************************************************
 Package pl_ExControls.pkg
 From PilotLogic Software House(http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit TplGnouMeterUnit;

{$mode objfpc}{$H+}

interface

{$I AllExControlsRegister.inc}

uses
  Classes, Controls, Graphics, SysUtils, Messages, LMessages, Types, LCLType, LCLIntf;

type
  TplGnouMeter = class(TGraphicControl)
  private
    fValue: double;
    fColorFore: TColor;
    fColorBack: TColor;
    fSignalUnit: ShortString;
    fValueMax: double;
    fValueMin: double;
    fDigits: byte;
    fIncrement: double;
    fShowIncrements: boolean;
    fGapTop: word;
    fGapBottom: word;
    fBarThickness: word;
    fMarkerColor: TColor;
    fShowMarker: boolean;
    //Variables used internallly
    TopTextHeight: word;
    LeftMeter: word;
    DisplayValue: string;
    DrawStyle: integer;
    TheRect: TRect;
    //End of variables used internallly
    procedure SetValue(val: double);
    procedure SetColorBack(val: TColor);
    procedure SetColorFore(val: TColor);
    procedure SetSignalUnit(val: ShortString);
    procedure SetValueMin(val: double);
    procedure SetValueMax(val: double);
    procedure SetDigits(val: byte);
    procedure SetTransparent(val: boolean);
    function GetTransparent: boolean;
    procedure SetIncrement(val: double);
    procedure SetShowIncrements(val: boolean);
    procedure SetGapTop(val: word);
    procedure SetGapBottom(val: word);
    procedure SetBarThickness(val: word);
    procedure SetMarkerColor(val: TColor);
    procedure SetShowMarker(val: boolean);
    procedure DrawTopText;
    procedure DrawMeterBar;
    procedure DrawIncrements;
    function ValueToPixels(val: double): integer;
    procedure DrawValueMax;
    procedure DrawValueMin;
    procedure DrawMarker;
  protected
    procedure Paint; override;
    procedure CMTextChanged(var Message: TLMessage); message CM_TEXTCHANGED;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property Caption;
    property Visible;
    property Value: double read fValue write SetValue;
    property Color;
    property Font;
    property ParentColor;
    property ColorFore: Tcolor read fColorFore write SetColorFore;
    property ColorBack: Tcolor read fColorBack write SetColorBack;
    property SignalUnit: ShortString read fSignalUnit write SetSignalUnit;
    property ValueMin: double read fValueMin write SetValueMin;
    property ValueMax: double read fValueMax write SetValueMax;
    property Digits: byte read fDigits write SetDigits;
    property Increment: double read fIncrement write SetIncrement;
    property ShowIncrements: boolean read fShowIncrements write SetShowIncrements;
    property Transparent: boolean read GetTransparent write SetTransparent;
    property GapTop: word read fGapTop write SetGapTop;
    property GapBottom: word read fGapBottom write SetGapBottom;
    property BarThickness: word read fBarThickness write SetBarThickness;
    property MarkerColor: TColor read fMarkerColor write SetMarkerColor;
    property ShowMarker: boolean read fShowMarker write SetShowMarker;

    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnStartDock;
    property OnStartDrag;
  end;


implementation

constructor TplGnouMeter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable, csSetCaption];
  Width := 100;
  Height := 200;
  fColorFore := clRed;
  fColorBack := clBtnFace;
  fMarkerColor := clBlue;
  fValueMin := 0;
  fValueMax := 100;
  fIncrement := 10;
  fShowIncrements := True;
  fShowMarker := True;
  fValue := 0;
  fGapTop := 20;
  fGapBottom := 10;
  fBarThickness := 5;
  fSignalUnit := 'Units';
end;

destructor TplGnouMeter.Destroy;
begin
  inherited Destroy;
end;

procedure TplGnouMeter.CMTextChanged(var Message: TMessage);
begin
  Invalidate;
end;

procedure TplGnouMeter.SetValue(val: double);
begin
  if (val <> fValue) and (val >= fValueMin) and (val <= fValueMax) then
  begin
    fValue := val;
    Invalidate;
  end;
end;

procedure TplGnouMeter.SetColorFore(val: TColor);
begin
  if val <> fColorFore then
  begin
    fColorFore := val;
    Invalidate;
  end;
end;

procedure TplGnouMeter.SetColorBack(val: TColor);
begin
  if val <> fColorBack then
  begin
    fColorBack := val;
    Invalidate;
  end;
end;

procedure TplGnouMeter.SetSignalUnit(val: ShortString);
begin
  if val <> fSignalUnit then
  begin
    fSignalUnit := val;
    Invalidate;
  end;
end;

procedure TplGnouMeter.SetValueMin(val: double);
begin
  if (val <> fValueMin) and (val <= fValue) then
  begin
    fValueMin := val;
    Invalidate;
  end;
end;

procedure TplGnouMeter.SetValueMax(val: double);
begin
  if (val <> fValueMax) and (val >= fValue) then
  begin
    fValueMax := val;
    Invalidate;
  end;
end;

procedure TplGnouMeter.SetDigits(val: byte);
begin
  if (val <> fDigits) then
  begin
    fDigits := val;
    Invalidate;
  end;
end;

procedure TplGnouMeter.SetIncrement(val: double);
begin
  if (val <> fIncrement) and (val > 0) then
  begin
    fIncrement := val;
    Invalidate;
  end;
end;

procedure TplGnouMeter.SetShowIncrements(val: boolean);
begin
  if (val <> fShowIncrements) then
  begin
    fShowIncrements := val;
    Invalidate;
  end;
end;

function TplGnouMeter.GetTransparent: boolean;
begin
  Result := not (csOpaque in ControlStyle);
end;

procedure TplGnouMeter.SetTransparent(Val: boolean);
begin
  if Val <> Transparent then
  begin
    if Val then
      ControlStyle := ControlStyle - [csOpaque]
    else
      ControlStyle := ControlStyle + [csOpaque];
    Invalidate;
  end;
end;

procedure TplGnouMeter.SetGapTop(val: word);
begin
  if (val <> fGapTop) then
  begin
    fGapTop := val;
    Invalidate;
  end;
end;

procedure TplGnouMeter.SetGapBottom(val: word);
begin
  if (val <> fGapBottom) then
  begin
    fGapBottom := val;
    Invalidate;
  end;
end;

procedure TplGnouMeter.SetBarThickness(val: word);
begin
  if (val <> fBarThickness) and (val > 0) then
  begin
    fBarThickness := val;
    Invalidate;
  end;
end;

procedure TplGnouMeter.SetMarkerColor(val: TColor);
begin
  if (val <> fMarkerColor) then
  begin
    fMarkerColor := val;
    Invalidate;
  end;
end;

procedure TplGnouMeter.SetShowMarker(val: boolean);
begin
  if (val <> fShowMarker) then
  begin
    fShowMarker := val;
    Invalidate;
  end;
end;

procedure TplGnouMeter.DrawIncrements;
var
  i: double;
  PosPixels: word;
begin
  if fShowIncrements then
  begin
    with Canvas do
    begin
      i := fValueMin;
      while i <= fValueMax do
      begin
        PosPixels := ValueToPixels(i);
        pen.color := clGray;
        MoveTo(LeftMeter + BarThickness + 3, PosPixels - 1);
        LineTo(LeftMeter + BarThickness + 7, PosPixels - 1);
        pen.color := clWhite;
        MoveTo(LeftMeter + BarThickness + 3, PosPixels);
        LineTo(LeftMeter + BarThickness + 7, PosPixels);
        i := i + fIncrement;
      end;
    end;
  end;
end;

procedure TplGnouMeter.DrawMarker;
begin
  if fShowMarker then
  begin
    with Canvas do
    begin
      pen.color := clWhite;
      Brush.Style := bsClear;
      MoveTo(LeftMeter - 2, ValueToPixels(fValue));
      LineTo(LeftMeter - 6, ValueToPixels(fValue) - 4);
      LineTo(LeftMeter - 6, ValueToPixels(fValue) + 4);
      pen.color := clGray;
      LineTo(LeftMeter - 2, ValueToPixels(fValue));

      pen.color := fMarkerColor;
      Brush.color := fMarkerColor;
      Brush.Style := bsSolid;
      Polygon([Point(LeftMeter - 3, ValueToPixels(fValue)), Point(LeftMeter - 5, ValueToPixels(fValue) - 2),
        Point(LeftMeter - 5, ValueToPixels(fValue) + 2), Point(LeftMeter - 3, ValueToPixels(fValue))]);
    end;
  end;
end;

procedure TplGnouMeter.DrawTopText;
begin
  with Canvas do
  begin
    DisplayValue := Caption;
    Brush.Style := bsClear;
    TheRect := ClientRect;
    DrawStyle := DT_SINGLELINE + DT_NOPREFIX + DT_CENTER + DT_TOP;
    Font.Style := [fsBold];
    TopTextHeight := DrawText(Handle, PChar(DisplayValue),
      Length(DisplayValue), TheRect, DrawStyle);

    Font.Style := [];
    TheRect.Top := TopTextHeight;
    DisplayValue := FloatToStrF(Value, ffFixed, 8, fDigits) + ' ' + fSignalUnit;
    TopTextHeight := TopTextHeight + DrawText(Handle, PChar(DisplayValue), Length(DisplayValue), TheRect, DrawStyle);
    TopTextHeight := TopTextHeight + fGapTop;
  end;
end;

procedure TplGnouMeter.DrawValueMin;
begin
  with Canvas do
  begin
    TheRect := ClientRect;
    TheRect.Left := LeftMeter + BarThickness + 10;
    TheRect.Top := TopTextHeight;
    TheRect.Bottom := Height - fGapBottom + 6;
    Brush.Style := bsClear;
    DrawStyle := DT_SINGLELINE + DT_NOPREFIX + DT_LEFT + DT_BOTTOM;
    DisplayValue := FloatToStrF(ValueMin, ffFixed, 8, fDigits) + ' ' + fSignalUnit;
    DrawText(Handle, PChar(DisplayValue), Length(DisplayValue), TheRect, DrawStyle);
  end;
end;

procedure TplGnouMeter.DrawValueMax;
begin
  with Canvas do
  begin
    TheRect := ClientRect;
    TheRect.Left := LeftMeter + BarThickness + 10;
    TheRect.Top := TopTextHeight - 6;
    Brush.Style := bsClear;
    DrawStyle := DT_SINGLELINE + DT_NOPREFIX + DT_LEFT + DT_TOP;
    DisplayValue := FloatToStrF(ValueMax, ffFixed, 8, fDigits) + ' ' + fSignalUnit;
    DrawText(Handle, PChar(DisplayValue), Length(DisplayValue), TheRect, DrawStyle);
  end;
end;

procedure TplGnouMeter.DrawMeterBar;
begin
  with Canvas do
  begin
    pen.Color := fColorBack;
    Brush.Color := fColorBack;
    Brush.Style := bsSolid;
    Rectangle(LeftMeter, ValueToPixels(fValueMax), LeftMeter + fBarThickness, ValueToPixels(fValueMin));

    pen.Color := fColorFore;
    Brush.Color := fColorFore;
    Brush.Style := bsSolid;
    Rectangle(LeftMeter + 1, ValueToPixels(fValue), LeftMeter + fBarThickness, ValueToPixels(fValueMin));

    pen.color := clWhite;
    Brush.Style := bsClear;
    MoveTo(LeftMeter + fBarThickness - 1, ValueToPixels(fValueMax));
    LineTo(LeftMeter, ValueToPixels(fValueMax));
    LineTo(LeftMeter, ValueToPixels(fValueMin) - 1);

    pen.color := clGray;
    LineTo(LeftMeter + fBarThickness, ValueToPixels(fValueMin) - 1);
    LineTo(LeftMeter + fBarThickness, ValueToPixels(fValueMax));

    if (fValue > fValueMin) and (fValue < fValueMax) then
    begin
      pen.color := clWhite;
      MoveTo(LeftMeter + 1, ValueToPixels(fValue));
      LineTo(LeftMeter + fBarThickness, ValueToPixels(fValue));
      pen.color := clGray;
      MoveTo(LeftMeter + 1, ValueToPixels(fValue) - 1);
      LineTo(LeftMeter + fBarThickness, ValueToPixels(fValue) - 1);
    end;

  end;
end;

function TplGnouMeter.ValueToPixels(val: double): integer;
var
  factor: double;
begin
  Result := 0;
  if fValueMax > fValueMin then
  begin
    Factor := (Height - fGapBottom - TopTextHeight) / (fValueMin - fValueMax);
    Result := Round(Factor * val - Factor * fValueMax + TopTextHeight);
  end;
end;

procedure TplGnouMeter.Paint;
begin
  LeftMeter := (Width div 2) - 10 - fBarThickness;
  with Canvas do
  begin
    if not Transparent then
    begin
      Brush.Color := Self.Color;
      Brush.Style := bsSolid;
      FillRect(ClientRect);
    end;
    Brush.Style := bsClear;
    DrawTopText;
    DrawValueMin;
    DrawValueMax;
    DrawMeterBar;
    DrawMarker;
    DrawIncrements;
  end;
end;

end.
