
{**********************************************************************
 Package pl_ExControls.pkg
 From PilotLogic Software House(http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit TplSimplePieUnit;

interface

uses
  Classes, SysUtils, Messages, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, plUtils;

type
  TplSimplePie = class(TGraphicControl)
  private
    FDrawBuffer: TBitmap;
    FShaddow1, FShaddow2, FBasecolor, FUsedColor: TColor;
    FPosition: integer;
    procedure SetPosition(Value: integer);
    procedure SetBasecolor(Value: TColor);
    procedure SetUsedcolor(Value: TColor);
  protected
    procedure Paint; override;
    procedure UpdateDrawBuffer(h, w: integer);
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: integer); override;
  public
    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;
    procedure Free;
  published
    property Position: integer read FPosition write SetPosition;
    property Basecolor: TColor read FBasecolor write SetBasecolor;
    property Usedcolor: TColor read FUsedcolor write SetUsedcolor;
    property Color;
    property ParentColor;

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

constructor TplSimplePie.Create(AnOwner: TComponent);
begin
  inherited Create(AnOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  Color := clwhite;
  Height := 64;
  Width := 128;
  FDrawBuffer := TBitmap.Create;
  FDrawBuffer.Width := Width;
  FDrawBuffer.Height := Height;
  FPosition := 64;
  FBaseColor := $00FF00FF;
  FUsedColor := $00FF0000;
  FShaddow1 := GetShaddowColor(FBaseColor);
  FShaddow2 := GetShaddowColor(FUsedColor);
  UpdateDrawBuffer(Height, Width);
end;

destructor TplSimplePie.Destroy;
begin
  if FDrawBuffer <> nil then
    FDrawBuffer.Destroy;
  inherited Destroy;
end;

procedure TplSimplePie.Free;
begin
  FDrawBuffer.Free;
  inherited Free;
end;

procedure TplSimplePie.SetPosition(Value: integer);
begin
  if Value > 100 then
    Value := 100;
  if Value < 0 then
    Value := 0;
  FPosition := Value;
  self.Invalidate;
end;

procedure TplSimplePie.UpdateDrawBuffer(h, w: integer);
var
  x, y: integer;
begin
  with FDrawBuffer.Canvas do
  begin

    h := h - 12;
    w := w - 1;
    Brush.Color := self.Color;//clBtnFace;
    Pen.Color := self.Color;//clBtnFace;
    Rectangle(0, 0, w + 2, h + 14);
    // Top
    Pen.Color := clBlack;
    Brush.Color := FBasecolor;
    Ellipse(1, 1, 1 + w, 1 + h);
    Arc(1, 12, 1 + w, 12 + h, 1, 12 + (h div 2), 1 + w, 12 + (h div 2));
    Moveto(1, 12 + (h div 2));
    LineTo(1, 1 + (h div 2));
    LineTo(1 + w div 2, 1 + (h div 2));
    Moveto(w, 12 + (h div 2));
    LineTo(w, 1 + (h div 2));
    // Calc point on ellipse using position
    x := round(1 + (w div 2) - cos(2 * pi / 100 * position) * (w div 2));
    y := round(1 + (h div 2) - sin(2 * pi / 100 * position) * (h div 2));
    // Draw line from center to point
    MoveTo(1 + (w div 2), 1 + (h div 2));
    LineTo(x, y);
    // Fill area
    Brush.Color := FUsedcolor;
    if position > 0 then
      FloodFill(2, h div 2, clBlack, fsBorder);
    if position > 51 then
    begin
      LineTo(x, 11 + y);
      Brush.Color := FShaddow2;
      FloodFill(w - 1, 12 + (h div 2), clBlack, fsBorder);
    end;
    if position < 98 then
    begin
      Brush.Color := FShaddow1;
      FloodFill(2, 12 + (h div 2), clBlack, fsBorder);
    end;
  end;
end;

procedure TplSimplePie.Paint;
begin
  inherited Paint;
  UpdateDrawBuffer(Height, Width);
  Canvas.Draw(0, 0, FDrawBuffer);
end;

procedure TplSimplePie.SetBounds(ALeft, ATop, AWidth, AHeight: integer);
begin
  inherited SetBounds(aLeft, aTop, aWidth, aHeight);
  if FDrawBuffer <> nil then
  begin
    FDrawBuffer.Destroy;
    FDrawBuffer := TBitmap.Create;
    FDrawBuffer.Height := aHeight;
    FDrawBuffer.Width := aWidth;
    UpdateDrawBuffer(aHeight, aWidth);

  end;
end;

procedure TplSimplePie.SetBasecolor(Value: TColor);
begin
  FBasecolor := Value;
  FShaddow1 := GetShaddowColor(FBaseColor);
  self.Invalidate;
end;

procedure TplSimplePie.SetUsedColor(Value: TColor);
begin
  FUsedColor := Value;
  FShaddow2 := GetShaddowColor(FUsedColor);
  self.Invalidate;
end;


end.
