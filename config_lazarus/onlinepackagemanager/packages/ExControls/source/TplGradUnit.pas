
{**********************************************************************
 Package pl_ExControls.pkg
 From PilotLogic Software House(http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit TplGradUnit;

interface

uses
  LCLIntf, LCLType, LMessages,
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls;

type

  TColorArray = array[1..20] of TColor;

  TplGrad = class(TGraphicControl)
  private
    c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20: TColor;
    cco: TColorArray;
    ges: integer;
    procedure WC1(Value: TColor);
    procedure WC2(Value: TColor);
    procedure WC3(Value: TColor);
    procedure WC4(Value: TColor);
    procedure WC5(Value: TColor);
    procedure WC6(Value: TColor);
    procedure WC7(Value: TColor);
    procedure WC8(Value: TColor);
    procedure WC9(Value: TColor);
    procedure WC10(Value: TColor);
    procedure WC11(Value: TColor);
    procedure WC12(Value: TColor);
    procedure WC13(Value: TColor);
    procedure WC14(Value: TColor);
    procedure WC15(Value: TColor);
    procedure WC16(Value: TColor);
    procedure WC17(Value: TColor);
    procedure WC18(Value: TColor);
    procedure WC19(Value: TColor);
    procedure WC20(Value: TColor);
    procedure WG1(Value: integer);
    procedure malen;
  public
    constructor Create(aComponent: TComponent); override;
    procedure Paint; override;
  published
    property Color101: TColor read c1 write wc1;
    property Color102: TColor read c2 write wc2;
    property Color103: TColor read c3 write wc3;
    property Color104: TColor read c4 write wc4;
    property Color105: TColor read c5 write wc5;
    property Color106: TColor read c6 write wc6;
    property Color107: TColor read c7 write wc7;
    property Color108: TColor read c8 write wc8;
    property Color109: TColor read c9 write wc9;
    property Color110: TColor read c10 write wc10;
    property Color111: TColor read c11 write wc11;
    property Color112: TColor read c12 write wc12;
    property Color113: TColor read c13 write wc13;
    property Color114: TColor read c14 write wc14;
    property Color115: TColor read c15 write wc15;
    property Color116: TColor read c16 write wc16;
    property Color117: TColor read c17 write wc17;
    property Color118: TColor read c18 write wc18;
    property Color119: TColor read c19 write wc19;
    property Color120: TColor read c20 write wc20;
    property ColoursUsed: integer read ges write wg1;

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

procedure TplGrad.WC1(Value: TColor);
begin
  c1 := Value;
  cco[1] := Value;
  malen;
end;

procedure TplGrad.WC2(Value: TColor);
begin
  c2 := Value;
  cco[2] := Value;
  malen;
end;

procedure TplGrad.WC3(Value: TColor);
begin
  c3 := Value;
  cco[3] := Value;
  malen;
end;

procedure TplGrad.WC4(Value: TColor);
begin
  c4 := Value;
  cco[4] := Value;
  malen;
end;

procedure TplGrad.WC5(Value: TColor);
begin
  c5 := Value;
  cco[5] := Value;
  malen;
end;

procedure TplGrad.WC6(Value: TColor);
begin
  c6 := Value;
  cco[6] := Value;
  malen;
end;

procedure TplGrad.WC7(Value: TColor);
begin
  c7 := Value;
  cco[7] := Value;
  malen;
end;

procedure TplGrad.WC8(Value: TColor);
begin
  c8 := Value;
  cco[8] := Value;
  malen;
end;

procedure TplGrad.WC9(Value: TColor);
begin
  c9 := Value;
  cco[9] := Value;
  malen;
end;

procedure TplGrad.WC10(Value: TColor);
begin
  c10 := Value;
  cco[10] := Value;
  malen;
end;

procedure TplGrad.WC11(Value: TColor);
begin
  c11 := Value;
  cco[11] := Value;
  malen;
end;

procedure TplGrad.WC12(Value: TColor);
begin
  c12 := Value;
  cco[12] := Value;
  malen;
end;

procedure TplGrad.WC13(Value: TColor);
begin
  c13 := Value;
  cco[13] := Value;
  malen;
end;

procedure TplGrad.WC14(Value: TColor);
begin
  c14 := Value;
  cco[14] := Value;
  malen;
end;

procedure TplGrad.WC15(Value: TColor);
begin
  c15 := Value;
  cco[15] := Value;
  malen;
end;

procedure TplGrad.WC16(Value: TColor);
begin
  c16 := Value;
  cco[16] := Value;
  malen;
end;

procedure TplGrad.WC17(Value: TColor);
begin
  c17 := Value;
  cco[17] := Value;
  malen;
end;

procedure TplGrad.WC18(Value: TColor);
begin
  c18 := Value;
  cco[18] := Value;
  malen;
end;

procedure TplGrad.WC19(Value: TColor);
begin
  c19 := Value;
  cco[19] := Value;
  malen;
end;

procedure TplGrad.WC20(Value: TColor);
begin
  c20 := Value;
  cco[20] := Value;
  malen;
end;

procedure TplGrad.WG1(Value: integer);
begin
  ges := Value;
  malen;
end;

constructor TplGrad.Create(aComponent: TComponent);
begin
  inherited Create(aComponent);
  c1 := clBlue;
  cco[1] := clBlue;

  ges := 2;
  Width := 150;
  Height := 25;

end;

procedure TplGrad.Paint;
begin
  inherited Paint;

  malen;
end;

procedure TplGrad.malen;
var
  c: TColorArray;
  a, cc: integer;
  r, g, b, t1, t2, t3: double;
  r2, g2, b2, tt: integer;
begin
  inherited;
  tt := 1;
  c := cco;
  for cc := 1 to ges - 1 do
  begin
    r := GetRValue(c[cc]);
    g := GetGValue(c[cc]);
    b := GetBValue(c[cc]);
    r2 := GetRValue(c[cc + 1]);
    g2 := GetGValue(c[cc + 1]);
    b2 := GetBValue(c[cc + 1]);
    t1 := abs(r - r2) / (Width div (ges - 1));
    t2 := abs(g - g2) / (Width div (ges - 1));
    t3 := abs(b - b2) / (Width div (ges - 1));
    for a := tt to tt + Width div (ges - 1) do
    begin
      if r < r2 then
        r := r + t1;
      if r > r2 then
        r := r - t1;
      if g < g2 then
        g := g + t2;
      if g > g2 then
        g := g - t2;
      if b < b2 then
        b := b + t3;
      if b > b2 then
        b := b - t3;
      c[cc] := RGB(trunc(r), trunc(g), trunc(b));
      canvas.pen.Color := c[cc];
      canvas.pen.Style := psSolid;

      canvas.moveTo(a, 0);
      canvas.LineTo(a, Height);
    end;
    tt := tt + Width div (ges - 1);
  end;
end;


end.
