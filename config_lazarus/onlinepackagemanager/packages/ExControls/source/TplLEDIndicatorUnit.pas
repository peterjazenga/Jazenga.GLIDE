
{**********************************************************************
 Package pl_ExControls.pkg
 From PilotLogic Software House(http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit TplLEDIndicatorUnit;

interface

uses
  Classes, SysUtils, Messages, Graphics, Controls, Forms, LCLIntf,
  Dialogs, ExtCtrls, plUtils;

type

  TplLEDIndicator = class(TGraphicControl)
  private
    FDrawBuffer: TBitmap;
    FShaddow, FForeground, FBackground: TColor;
    FPosition: integer;
    procedure SetPosition(Value: integer);
    procedure SetForeground(Value: TColor);
    procedure SetBackground(Value: TColor);
  protected
    procedure Paint; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: integer); override;
    procedure UpdateDrawBuffer;
  public
    constructor Create(anOwner: TComponent); override;
    destructor Destroy; override;
    procedure Free;
  published
    property Position: integer read FPosition write SetPosition;
    property Background: TColor read FBackground write SetBackground;
    property Foreground: TColor read fForeground write SetForeground;
    property Height;
    property Width;
    property Color;
    property ParentColor;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

implementation


constructor TplLEDIndicator.Create(anOwner: TComponent);
begin
  inherited Create(anOwner);
  ControlStyle := ControlStyle + [csReplicatable];

  Height := 128;
  Width := 32;
  FPosition := 0;

  FForeground := clLime;
  FBackground := clBlack;
  FShaddow := GetShaddowColor(FForeground);
  FDrawBuffer := TBitmap.Create;
end;

destructor TplLEDIndicator.Destroy;
begin
  if FDrawBuffer <> nil then
    FDrawBuffer.Free;
  inherited Destroy;
end;

procedure TplLEDIndicator.Free;
begin
  FDrawBuffer.Free;
  inherited Free;
end;

procedure TplLEDIndicator.SetBounds(ALeft, ATop, AWidth, AHeight: integer);
begin
  inherited;
  if FDrawBuffer <> nil then
  begin
    FDrawBuffer.Destroy;
    FDrawBuffer := TBitmap.Create;
    FDrawBuffer.Height := aHeight;
    FDrawBuffer.Width := aWidth;
  end;
end;

procedure TplLEDIndicator.UpdateDrawBuffer;
var
  a, b, c, d, n: integer;
begin
  with FDrawBuffer.Canvas do
  begin
    Brush.Color := self.Color;
    FillRect(ClientRect);

    Brush.Color := FBackground;
    Pen.Color := FBackground;
    Rectangle(4, 0, Width - 4, Height);

    n := (Height) div 3 - 2;
    b := (Width div 2);
    d := round(n / 100 * (100 - position));
    Pen.Color := FShaddow;
    for a := 0 to n do
    begin
      if a = d then
        Pen.Color := FForeground;
      c := 3 * a + 2;
      MoveTo(b - 1, c);
      LineTo(4, c);
      MoveTo(b + 1, c);
      LineTo(Width - 5, c);
      c := 3 * a + 3;
      MoveTo(b + 1, c);
      LineTo(Width - 5, c);
      MoveTo(b - 1, c);
      LineTo(4, c);
    end;
  end;
end;

procedure TplLEDIndicator.Paint;
begin
  UpdateDrawBuffer;
  Canvas.Draw(0, 0, FDrawbuffer);
end;

procedure TplLEDIndicator.SetForeground(Value: TColor);
begin
  FShaddow := GetShaddowColor(Value);
  FForeground := Value;
  UpdateDrawBuffer;
  Paint;
end;

procedure TplLEDIndicator.SetBackground(Value: TColor);
begin
  FBackground := Value;
  UpdateDrawBuffer;
  Paint;
end;

procedure TplLEDIndicator.SetPosition(Value: integer);
begin
  if Value > 100 then
    Value := 100;
  if Value < 0 then
    Value := 0;
  FPosition := Value;
  Paint;
end;

end.
