
{**********************************************************************
 Package pl_ExControls.pkg
 From PilotLogic Software House(http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit TplScopeUnit;

interface

uses
  Classes, SysUtils, Messages, Graphics, Controls, Forms,
  Dialogs, ExtCtrls;

type

  TplScope = class(TGraphicControl)
  private
    fAllowed: boolean;
    fOnUpdate: TNotifyEvent;
    DrawBuffer: TBitmap;
    DrawTimer: TTimer;
    fActive: boolean;
    fBaseColor,           { Baseline color }
    fColor,               { Background color }
    fGridColor,           { Grid line color }
    fLineColor: TColor;  { Position line color }
    fBaseLine, fGridSize, fPosition,            { Value to plot }
    fInterval: integer; { Update speed in 1/10 seconds }
    procedure SetActive(const Value: boolean);
    procedure SetGridSize(const Value: integer);
    procedure SetBaseLine(const Value: integer);
    procedure SetInterval(const Value: integer);
  protected
    Oldpos, PrevPos: integer;
    CalcBase, Counter: integer;
    procedure UpdateScope(Sender: TObject);
    procedure Loaded; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: integer); override;
  public
    procedure Paint; override;
    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;
    procedure Free;
    procedure Clear;
  published
    property Baseline: integer read fBaseline write SetBaseLine;
    property Gridsize: integer read fGridSize write SetGridSize;
    property Active: boolean read fActive write SetActive;
    property Position: integer read fPosition write fPosition;
    property Interval: integer read fInterval write SetInterval;
    property Color: TColor read fColor write fColor;
    property Gridcolor: TColor read fGridColor write fGridColor;
    property Linecolor: TColor read fLineColor write fLineColor;
    property Basecolor: TColor read fBaseColor write fBaseColor;
    property OnUpdate: TNotifyEvent read fOnUpdate write fOnUpdate;
    property Height;
    property Width;

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

constructor TplScope.Create(AnOwner: TComponent);
begin
  inherited Create(AnOwner);

  ControlStyle := [csDesignInteractive, csFramed, csOpaque];

  fAllowed := False;

  fBaseLine := 50;
  fGridSize := 16;
  DrawBuffer := TBitmap.Create;
  DrawBuffer.Canvas.Brush.Color := FColor;
  DrawBuffer.Canvas.Brush.Style := bsSolid;
  DrawBuffer.Canvas.Pen.Width := 1;
  DrawBuffer.Canvas.Pen.Style := psSolid;

  DrawTimer := TTimer.Create(SELF);
  DrawTimer.Enabled := False;
  DrawTimer.OnTimer := @UpdateScope;
  DrawTimer.Interval := 500;

  Height := 120;
  Width := 208;

  Color := clBlack;
  GridColor := clGreen;
  LineColor := clLime;
  BaseColor := clRed;

  Position := 50;
  Interval := 50;
  Counter := 1;


 // fAllowed := True;
end;

procedure TplScope.Loaded;
begin
  inherited Loaded;
  fAllowed := True;
end;

procedure TplScope.Clear;
var
  a: integer;
begin
  CalcBase := (Height - round(Height / 100 * FBaseline));
  with DrawBuffer.Canvas do
  begin
    Brush.Color := FColor;
    Pen.Style := psClear;
    Rectangle(0, 0, Width + 1, Height + 1);
    Pen.Style := psSolid;
    Pen.Color := FGridColor;
    Pen.Width := 1;
    { Vertical lines }
    a := Width;
    while a > 0 do
    begin
      MoveTo(a - 1, 0);
      LineTo(a - 1, Height);
      Dec(a, FGridSize);
    end;
    { Horizontal lines - above Baseline }
    a := CalcBase;
    while a < Height do
    begin
      Inc(a, FGridSize);
      MoveTo(0, a);
      LineTo(Width, a);
    end;
    { Horizontal lines - below Baseline }
    a := CalcBase;
    while a > 0 do
    begin
      Dec(a, FGridSize);
      MoveTo(0, a);
      LineTo(Width, a);
    end;
    { Baseline }
    Pen.Color := FBaseColor;
    MoveTo(0, CalcBase);
    LineTo(Width, CalcBase);

    { Start new position-line on baseline... }
    OldPos := CalcBase;
    PrevPos := CalcBase;
    {
    // Draws a line from 0,baseline to width, new pos
    Pen.Color:=FLineColor;
    MoveTo(0,height);
    LineTo(Width,height-round(height/100*position));
    }
    counter := 1;
  end;
end;

procedure TplScope.Free;
begin
  DrawTimer.Free;
  DrawBuffer.Free;
  inherited Free;
end;

destructor TplScope.Destroy;
begin
  if DrawTimer <> nil then
    DrawTimer.Destroy;
  if DrawBuffer <> nil then
    DrawBuffer.Destroy;
  inherited Destroy;
end;

procedure TplScope.SetBaseLine(const Value: integer);
begin
  if csLoading in ComponentState then exit;

  if Value = 0 then
    fBaseLine := 1
  else
    fBaseLine := Value;

  CalcBase := (Height - round(Height / 100 * FBaseline));
  if fAllowed then
  begin
    Clear;
    if parent <> nil then
      Paint;
  end;
end;

procedure TplScope.SetInterval(const Value: integer);
begin
  DrawTimer.Enabled := False;
  CalcBase := (Height - round(Height / 100 * FBaseline));
  DrawTimer.Interval := Value * 10;
  fInterval := Value;
  DrawTimer.Enabled := FActive;
end;

procedure TplScope.SetGridSize(const Value: integer);
begin
  fGridSize := (Value div 2) * 2;
  if fAllowed then
  begin
    Clear;
    if parent <> nil then
      Paint;
  end;
end;

procedure TplScope.SetActive(const Value: boolean);
begin
  CalcBase := (Height - round(Height / 100 * FBaseline));
  DrawTimer.Interval := Interval * 10;
  DrawTimer.Enabled := Value;
  fActive := Value;
end;

procedure TplScope.UpdateScope(Sender: TObject);
var
  a: int64;
  Des, Src: TRect;
begin
  with DrawBuffer.Canvas do
  begin
    Pen.Color := FGridColor;

    Des.Top := 0;
    Des.Left := 0;
    Des.Right := Width - 2;
    Des.Bottom := Height;

    Src.Top := 0;
    Src.Left := 2;
    Src.Right := Width;
    Src.Bottom := Height;
    { Copy bitmap leftwards }
    CopyRect(Des, DrawBuffer.Canvas, Src);

    { Draw new area }
    Pen.Color := FColor;
    Pen.Width := 2;
    MoveTo(Width - 1, 0);
    LineTo(Width - 1, Height);
    Pen.Color := FGridColor;
    Pen.Width := 1;
    { Draw vertical line if needed }
    if counter = (GridSize div 2) then
    begin
      MoveTo(Width - 1, 0);
      LineTo(Width - 1, Height);
      counter := 0;
    end;
    Inc(counter);
    { Horizontal lines - above Baseline }
    a := CalcBase;
    while a < Height do
    begin
      Inc(a, FGridSize);
      MoveTo(Width - 2, a);
      LineTo(Width, a);
    end;
    { Horizontal lines - below Baseline }
    a := CalcBase;
    while a > 0 do
    begin
      Dec(a, FGridSize);
      MoveTo(Width - 2, a);
      LineTo(Width, a);
    end;
    { Baseline }
    Pen.Color := FBaseColor;
    MoveTo(Width - 2, CalcBase);
    LineTo(Width, CalcBase);
    { Draw position for line }
    Pen.Color := FLineColor;
    a := Height - round(Height / 100 * position);
    MoveTo(Width - 4, OldPos);
    LineTo(Width - 2, PrevPos);
    LineTo(Width - 0, a);
    OldPos := PrevPos;
    PrevPos := a;
  end;
  paint;
  if assigned(FOnUpdate) then
    fOnUpdate(SELF);
end;

procedure TplScope.Paint;
var
  Rect: TRect;
begin
  inherited Paint;

  if (DrawBuffer.Width <> Width) or (DrawBuffer.Height <> Height) then
    DrawBuffer.SetSize(Width, Height);

  Rect.Top := 0;
  Rect.Left := 0;
  Rect.Right := Width;
  Rect.Bottom := Height;
  Canvas.CopyRect(Rect, DrawBuffer.Canvas, Rect);
  fAllowed := True;
end;

procedure TplScope.SetBounds(ALeft, ATop, AWidth, AHeight: integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  DrawBuffer.Height := Height;
  DrawBuffer.Width := Width;
  if (csDesigning in ComponentState) and (fAllowed) then
  begin
    Clear;
  end;
end;


end.
