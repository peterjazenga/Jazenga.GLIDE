
{**********************************************************************
 Package pl_ExControls.pkg
 From PilotLogic Software House(http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit TplKnobUnit;

interface

uses
  SysUtils, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, Math, ComCtrls;

type

  TSuperLineStyle = (alsLine, alsFullLine, alsCircle);
  TSuperFaceStyle = (afsType1, afsType2);
  TSuperKnobStyle = (aksNormal, aksAnglefull, aksAngleSymmetric);

  TplSuperKnob = class(TGraphicControl)
  private
    fMaxValue: single;
    fMinValue: single;
    fCurValue: single;
    fPosition: single;
    fColor: TColor;
    fLineColor: TColor;
    fDrag: boolean;
    fKnobChangeEvent: TNotifyEvent;
    FollowMouse: boolean;
    fLineStyle: TSuperLineStyle;
    fFaceStyle: TSuperFaceStyle;
    fKnobStyle: TSuperKnobStyle;
    fRotationEffect: boolean;
    FDecimals: integer;
    fCharSet: set of char;
    procedure SetMaxValue(Value: single);
    procedure SetMinValue(Value: single);
    procedure SetCurValue(Value: single);
    procedure SetPosition(Value: single);
    function  GetPosition: single;
    procedure SetPositionStr(Value: string);
    function  GetPositionStr: string;
    procedure SetDecimals(Value: integer);
    procedure SetColor(aColor: TColor);
    procedure SetLineColor(Value: TColor);
    procedure SetLineStyle(Value: TSuperLineStyle);
    procedure SetFaceStyle(Value: TSuperFaceStyle);
    procedure SetDrag(Value: boolean);
    procedure SetKnobStyle(Value: TSuperKnobStyle);
    procedure UpdatePosition(X, Y: integer);
    function ConstrainValue(Minimum, Maximum, Value: single): single;
    function IsValidValue(Value: string): boolean;
  protected
    procedure KnobChange;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure Paint; override;
    property CurValue: single read fCurValue write SetCurValue;
  public
    constructor Create(AOwner: TComponent); override;
    property PositionStr: string read GetPositionStr write SetPositionStr;
  published
    property Align;
    property AllowUserDrag: boolean read FDrag write SetDrag default True;
    property KnobStyle: TSuperKnobStyle read fKnobStyle write SetKnobStyle;
    property FaceColor: TColor read fColor write SetColor;
    property FaceStyle: TSuperFaceStyle read fFaceStyle write SetFaceStyle;
    property LineColor: TColor read fLineColor write SetLineColor;
    property LineStyle: TSuperLineStyle read fLineStyle write SetLineStyle;
    property RotationEffect: boolean read fRotationEffect write fRotationEffect;
    property Max: single read fMaxValue write SetMaxValue;
    property Min: single read fMinValue write SetMinValue;
    property Decimals: integer read fDecimals write SetDecimals;
    property Position: single read fPosition write SetPosition;
    property OnChange: TNotifyEvent read fKnobChangeEvent write fKnobChangeEvent;
    property Enabled;
    property Color;
    property ParentColor;
    property ParentShowHint;
    property ShowHint;
    property Visible;
  end;


implementation

const
  NumChars: set of char = ['0'..'9'];

constructor TplSuperKnob.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];

  fCharSet := NumChars + ['-'] + [DecimalSeparator] + ['E'];
  Width := 60;
  Height := 60;
  fMaxValue := 360;
  fMinValue := 0;
  fCurValue := 0;

  fRotationEffect := False;
  fLineStyle := alsFullLine;
  fFaceStyle := afsType2;
  fLineColor := ClBlack;
  FollowMouse := False;
  FDecimals := 2;
  SetColor(clBtnface);
  SetDrag(True);
  KnobStyle := aksAnglefull;

end;

procedure TplSuperKnob.SetKnobStyle(Value: TSuperKnobStyle);
begin
  if Value = fKnobStyle then
    exit;
  fKnobStyle := Value;

  case fKnobStyle of
    aksNormal:
    begin
      fMinValue := 0;
      fMaxValue := 100;
    end;
    aksAnglefull:
    begin
      fMinValue := 0;
      fMaxValue := 360;
    end;
    aksAngleSymmetric:
    begin
      fMinValue := -180;
      fMaxValue := 180;
    end;
  end;

  Refresh;
end;

procedure TplSuperKnob.SetMaxValue(Value: single);
begin
  if fKnobStyle <> aksNormal then
    exit;

  if Value <> fMaxValue then
  begin
    fMaxValue := Value;
    Refresh;
  end;
end;

procedure TplSuperKnob.SetMinValue(Value: single);
begin
  if fKnobStyle <> aksNormal then
    exit;

  if Value <> fMinValue then
  begin
    fMinValue := Value;
    Refresh;
  end;
end;

function TplSuperKnob.ConstrainValue(Minimum, Maximum, Value: single): single;
begin
  Result := Value;
  if Value < 0 then
    Result := 0;
  if Value > 360 then
    Result := 360;
end;

procedure TplSuperKnob.SetFaceStyle(Value: TSuperFaceStyle);
begin
  if Value <> fFaceStyle then
  begin
    fFaceStyle := Value;
    Refresh;
  end;
end;

procedure TplSuperKnob.SetCurValue(Value: single);
begin
  if Value <> FCurValue then
  begin
    FCurValue := ConstrainValue(fMinValue, fMaxValue, Value);
    fPosition := GetPosition;
    Refresh;
    KnobChange;
  end;
end;

procedure TplSuperKnob.SetDecimals(Value: integer);
begin
  if Value < 1 then
    exit;
  if Value <> fDecimals then
  begin
    fDecimals := Value;
    Refresh;
  end;
end;

procedure TplSuperKnob.SetLineColor(Value: TColor);
begin
  if Value <> fLineColor then
  begin
    fLineColor := Value;
    Refresh;
  end;

end;

procedure TplSuperKnob.SetColor(aColor: TColor);
begin
  FColor := aColor;
  Refresh;
end;

procedure TplSuperKnob.SetDrag(Value: boolean);
begin
  if Value <> FDrag then
  begin
    FDrag := Value;
    Refresh;
  end;
end;

procedure TplSuperKnob.KnobChange;
begin
  if Assigned(fKnobChangeEvent) then
    fKnobChangeEvent(Self);
end;

procedure TplSuperKnob.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if FDrag = True then
  begin
    FollowMouse := True;
    UpdatePosition(X, Y);
    Refresh;
  end;
end;

procedure TplSuperKnob.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  FollowMouse := False;
end;

procedure TplSuperKnob.MouseMove(Shift: TShiftState; X, Y: integer);

begin
  inherited MouseMove(Shift, X, Y);
  if FollowMouse = True then
  begin
    UpdatePosition(X, Y);
  end;
end;

function TplSuperKnob.IsValidValue(Value: string): boolean;
var
  N: byte;
  ss: string;
begin
  Result := False;

  ss := Trim(Value);
  if Length(ss) > 0 then
  begin
    for N := 1 to Length(ss) do
      if not (ss[N] in fCharSet) then
        Result := True;
    if not Result then
    begin
      N := Pos('-', Text);
      if (N > 1) or ((N > 0) and (Length(Text) < 2)) then
        Result := True;
    end;
  end
  else
    Result := True;
end;

procedure TplSuperKnob.SetPositionStr(Value: string);
begin
  if ISValidValue(Value) = True then
    try
      Position := StrToFloat(Value);
    finally
    end;
end;

function TplSuperKnob.GetPositionStr: string;
begin
  Result := Format('%4.' + IntToStr(fDecimals) + 'f', [Position]);
end;

procedure TplSuperKnob.SetLineStyle(Value: TSuperLineStyle);
begin
  if Value <> fLineStyle then
  begin
    fLineStyle := Value;
    Refresh;
  end;
end;
//=========================================================
procedure TplSuperKnob.SetPosition(Value: single);
var
  a: single;
begin
  case fKnobStyle of
    aksNormal: if (Value < min) or (Value > max) then
        exit;
    aksAnglefull: if (Value < 0) or (Value > 360) then
        exit;
    aksAngleSymmetric: if (Value < -180) or (Value > 180) then
        exit;
  end;

  a := (abs(min) + abs(max)) / 360;
  case fKnobStyle of
    aksNormal: fCurValue := Value / a - min;
    aksAnglefull: fCurValue := Value / a - min;
    aksAngleSymmetric:
    begin

      if Value < 0 then
        fCurValue := 360 + Value
      else
        fCurValue := Value / a - min - 180;
    end;
  end;

  fPosition := GetPosition;
  Refresh;
  KnobChange;
end;

function TplSuperKnob.GetPosition: single;
var
  a: single;
begin
  a := (abs(min) + abs(max)) / 360;

  case fKnobStyle of
    aksNormal: Result := FCurValue * a + min;
    aksAnglefull: Result := FCurValue * a + min;
    aksAngleSymmetric:
    begin
      Result := FCurValue * a + min + 180;
      if Result > max then
        Result := -(360 - Result);
    end;
  end;

end;

//========================================================
procedure TplSuperKnob.Paint;
var
  R: TRect;
  Co, Si, Angle: double;
  X, Y, W, H: integer;
  dx, dy, gx, gy: double;
  OuterPoint: TPoint;
begin

  Width := Height;

  R.Left := 0;
  R.Top := 0;
  R.Right := Width;
  R.Bottom := Height;
  W := R.Right - R.Left - 4;
  H := R.Bottom - R.Top - 4;

  if fRotationEffect then
    if (CurValue / 2) <> 0 then
      h := h + 1;

  with Canvas do
  begin
     {
  if not ParentColor then
  begin
    Canvas.Brush.Color := Color;
    Canvas.FillRect(clientRect);
  end;
     }
    case fFaceStyle of
      afsType1:
      begin

        Brush.Color := FaceColor;
        Pen.Width := 2;

        if FaceColor = Cl3dlight then
          Pen.Color := Cl3dlight
        else
          Pen.Color := clBtnFace;

        ellipse(1, 1, W - 1, H - 1);
        Pen.Color := Clbtnshadow;
        ellipse(3, 3, W + 2, H + 2);
        Pen.Color := Clbtnface;
        Pen.Width := 1;
        RoundRect(2, 2, w, h, w, h);
      end;
      afsType2:
      begin
        Pen.Color := clBtnFace;
        Brush.Color := FaceColor;
        Brush.Style := bsSolid;
        Ellipse(2, 2, W - 2, H - 2);

        Pen.Width := 2;
        Pen.Color := clBtnHighlight;
        Brush.Color := clBtnFace;

        Arc(2, 2, w, h, w, 0, 0, h);
        Pen.Color := clBtnShadow;
        Arc(1, 1, w - 1, h - 1, 0, h, w, 0);
        //..PaintState .................
        Pen.Width := 1;
        Brush.Style := bsClear;
        if Enabled then
          Pen.Color := clBlack
        else
          Pen.Color := clBtnShadow;
        Ellipse(0, 0, w, h);
        // if focused then Pen.Color := clBlack else
        Pen.Color := clBlack;//clBtnShadow;
        Arc(0, 0, w, h, w, 0, 0, h);
      end;
    end;
    //........ For line ........................

    Pen.Width := 2;
    Pen.Color := LineColor;

    if CurValue >= 0 then
    begin
      Brush.Color := FaceColor;
      X := W div 2;
      Y := H div 2;
      dX := W / 6;
      dY := H / 6;
      gX := W / 32;
      gY := H / 32;

      case fKnobStyle of
        aksNormal: Angle := ((fCurValue - 180) / 360) * 5;
        aksAnglefull: Angle := DegToRad(fCurValue);
        aksAngleSymmetric: Angle := DegToRad(fCurValue);
      end;

      Si := Sin(Angle);
      Co := Cos(Angle);

      OuterPoint.X := Round(X + Si * (X - dx));
      OuterPoint.Y := Round(Y - Co * (Y - dy));

      case LineStyle of
        alsLine:
        begin
          MoveTo(OuterPoint.X, OuterPoint.y);
          LineTo(Round(X + Si * (X - gx)), Round(Y - Co * (Y - gy)));
        end;
        alsFullLine:
        begin
          MoveTo(X, Y);
          LineTo(Round(X + Si * (X - gx)), Round(Y - Co * (Y - gy)));

        end;
        alsCircle:
        begin
          Brush.Color := LineColor;
          RoundRect(OuterPoint.X - 3, OuterPoint.Y - 3,
            OuterPoint.X + 3, OuterPoint.Y + 3,
            OuterPoint.X + 3, OuterPoint.Y + 3);
        end;
      end;
    end;
  end;

end;

procedure TplSuperKnob.UpdatePosition(X, Y: integer);
var
  CX, CY: integer;
  R: double;
  Angle: double;
begin
  CX := Width div 2;
  CY := Height div 2;

  R := sqrt(sqr(CX - X) + sqr(CY - Y));
  if R = 0 then
    R := 0.0001;

  case fKnobStyle of
    aksAnglefull,
    aksAngleSymmetric: //................................................
    begin

      if (Y < CY) and (X > CX) then           //top right OK
      begin
        Angle := ArcSin((X - CX) / R);
        Angle := Angle + Pi;
        CurValue := RadToDeg(Angle) - 180;
      end;

      if (Y < CY) and (X <= CX) then         //top left
      begin
        Angle := ArcSin((X - CX) / R);
        Angle := Angle + Pi;
        CurValue := RadToDeg(Angle) + 180;
      end;

      if (Y >= CY) and (X > CX) then           //bottom right OK
      begin
        Angle := arcsin((CX - X) / R);
        Angle := Angle + Pi;
        CurValue := RadToDeg(Angle);
      end;


      if (Y >= CY) and (X <= CX) then         //bottom left
      begin
        Angle := arcsin((CX - X) / R);
        Angle := Angle - Pi;
        CurValue := RadToDeg(Angle) + 360;
      end;

      if CurValue = 360 then
        CurValue := 0;

    end;
    aksNormal:  //................................................
    begin

      if Y < CY then
        Angle := ArcSin((X - CX) / R)
      else
      begin
        Angle := arcsin((CX - X) / R);
        if X > CX then
          Angle := Angle + Pi
        else
          Angle := Angle - Pi;
      end;

      CurValue := ((Angle * 360) / 5) + 180;
    end;
  end;    //................................................

  Refresh;
end;

end.
