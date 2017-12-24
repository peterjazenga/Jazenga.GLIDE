
{**********************************************************************
 Package pl_ExControls.pkg
 From PilotLogic Software House(http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit TplCircleProgressUnit;

interface

{$MODE Delphi}

uses
  LCLIntf, LCLType, LMessages,
  SysUtils, Classes, Controls, Messages, Graphics;

type

  TcirProgressAlign = (paCenter, paLeftOrTop, paRightOrBottom);
  TcirGradientMode = (gmNone, gmPosition, gmAngle);
  TcirStringEvent = procedure(Sender: TObject; var Text: string) of object;

  TplCircleProgress = class(TCustomControl)
  private
    FGradient: TBitmap;
    FProgressAlign: TcirProgressAlign;
    FColorDoneMin: TColor;
    FColorDoneMax: TColor;
    FColorRemain: TColor;
    FColorInner: TColor;
    FStartAngle: integer;
    FMin: integer;
    FMax: integer;
    FPosition: integer;
    FInnerSize: integer;
    FShowText: boolean;
    FGradientMode: TcirGradientMode;
    FOnText: TcirStringEvent;
    procedure SetProgressAlign(const Value: TcirProgressAlign);
    procedure SetColorDoneMin(const Value: TColor);
    procedure SetColorDoneMax(const Value: TColor);
    procedure SetColorRemain(const Value: TColor);
    procedure SetColorInner(const Value: TColor);
    procedure SetStartAngle(const Value: integer);
    procedure SetMin(const Value: integer);
    procedure SetMax(const Value: integer);
    procedure SetPosition(const Value: integer);
    procedure SetInnerSize(const Value: integer);
    procedure SetShowText(const Value: boolean);
    procedure SetGradientMode(const Value: TcirGradientMode);
    function  GradientColor(ColorBegin, ColorEnd: TColor; AMin, AMax, APosition: integer): TColor;
    function  AnglePosition(R: TRect; AMin, AMax, APosition: integer): TPoint;
    procedure UpdateAngleGradientBrush;
  protected
    procedure WndProc(var Message: TLMessage); override;
    procedure Loaded; override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ProgressAlign: TcirProgressAlign read FProgressAlign write SetProgressAlign default paCenter;
    property ColorDoneMin: TColor read FColorDoneMin write SetColorDoneMin default clMaroon;
    property ColorDoneMax: TColor read FColorDoneMax write SetColorDoneMax default clRed;
    property ColorRemain: TColor read FColorRemain write SetColorRemain default clSilver;
    property ColorInner: TColor read FColorInner write SetColorInner default clWhite;
    property StartAngle: integer read FStartAngle write SetStartAngle default 0;
    property Min: integer read FMin write SetMin default 0;
    property Max: integer read FMax write SetMax default 100;
    property Position: integer read FPosition write SetPosition default 0;
    property InnerSize: integer read FInnerSize write SetInnerSize default 75;
    property ShowText: boolean read FShowText write SetShowText default True;
    property GradientMode: TcirGradientMode read FGradientMode write SetGradientMode default gmNone;
    property OnText: TcirStringEvent read FOnText write FOnText;
    property Align;
    property Anchors;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Font;
    property ParentFont;
    property Color;
    property ParentColor;
    property ParentShowHint;
    property ShowHint;
    property PopupMenu;
    property Visible;
    property UseDockManager;
  end;

implementation

const
  BufScale = 3;

constructor TplCircleProgress.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csOpaque];

  SetInitialBounds(0,0,100,100);

  FColorDoneMin := clMaroon;
  FColorDoneMax := clRed;
  FColorRemain := clSilver;
  FColorInner := clWhite;
  FMax := 100;
  FInnerSize := 75;
  FShowText := True;
//  DoubleBuffered := True;

end;

destructor TplCircleProgress.Destroy;
begin
  if Assigned(FGradient) then
    FGradient.Free;
  inherited;
end;

procedure TplCircleProgress.SetProgressAlign(const Value: TcirProgressAlign);
begin
  if FProgressAlign <> Value then
  begin
    FProgressAlign := Value;
    UpdateAngleGradientBrush;
    Invalidate;
  end;
end;

procedure TplCircleProgress.SetColorDoneMin(const Value: TColor);
begin
  if FColorDoneMin <> Value then
  begin
    FColorDoneMin := Value;
    UpdateAngleGradientBrush;
    Invalidate;
  end;
end;

procedure TplCircleProgress.SetColorDoneMax(const Value: TColor);
begin
  if FColorDoneMax <> Value then
  begin
    FColorDoneMax := Value;
    UpdateAngleGradientBrush;
    Invalidate;
  end;
end;

procedure TplCircleProgress.SetColorRemain(const Value: TColor);
begin
  if FColorRemain <> Value then
  begin
    FColorRemain := Value;
    UpdateAngleGradientBrush;
    Invalidate;
  end;
end;

procedure TplCircleProgress.SetColorInner(const Value: TColor);
begin
  if FColorInner <> Value then
  begin
    FColorInner := Value;
    UpdateAngleGradientBrush;
    Invalidate;
  end;
end;

procedure TplCircleProgress.SetStartAngle(const Value: integer);
var
  V: integer;
begin
  V := Value;
  if V < 0 then
    V := 0;
  if V > 359 then
    V := 359;
  if FStartAngle <> V then
  begin
    FStartAngle := V;
    UpdateAngleGradientBrush;
    Invalidate;
  end;
end;

procedure TplCircleProgress.SetMin(const Value: integer);
var
  V: integer;
begin
  V := Value;
  if V >= FMax then
    V := Pred(MaxInt);
  if FMin <> V then
  begin
    FMin := V;
    if FPosition < FMin then
      FPosition := FMin;
    UpdateAngleGradientBrush;
    Invalidate;
  end;
end;

procedure TplCircleProgress.SetMax(const Value: integer);
var
  V: integer;
begin
  V := Value;
  if V <= FMin then
    V := Succ(FMin);
  if FMax <> V then
  begin
    FMax := V;
    if FPosition > FMax then
      FPosition := FMax;
    UpdateAngleGradientBrush;
    Invalidate;
  end;
end;

procedure TplCircleProgress.SetPosition(const Value: integer);
var
  V: integer;
begin
  V := Value;
  if V < Min then
    V := Min;
  if V > Max then
    V := Max;
  if FPosition <> V then
  begin
    FPosition := V;
    Invalidate;
  end;
end;

procedure TplCircleProgress.SetInnerSize(const Value: integer);
var
  V: integer;
begin
  V := Value;
  if V < 0 then
    V := 0;
  if V > 99 then
    V := 99;
  if FInnerSize <> V then
  begin
    FInnerSize := V;
    Invalidate;
  end;
end;

procedure TplCircleProgress.SetShowText(const Value: boolean);
begin
  if FShowText <> Value then
  begin
    FShowText := Value;
    Invalidate;
  end;
end;

procedure TplCircleProgress.SetGradientMode(const Value: TcirGradientMode);
begin
  if FGradientMode <> Value then
  begin
    FGradientMode := Value;
    UpdateAngleGradientBrush;
    Invalidate;
  end;
end;

function TplCircleProgress.GradientColor(ColorBegin, ColorEnd: TColor; AMin, AMax, APosition: integer): TColor;
var
  B, E, B1, B2, B3, E1, E2, E3: integer;
  P: double;
begin
  B := ColorToRGB(ColorBegin);
  B1 := B and $FF;
  B2 := (B shr 8) and $FF;
  B3 := (B shr 16) and $FF;
  E := ColorToRGB(ColorEnd);
  E1 := E and $FF;
  E2 := (E shr 8) and $FF;
  E3 := (E shr 16) and $FF;
  if AMax - AMin <> 0 then
    P := (APosition - AMin) / (AMax - AMin)
  else
    P := 0;
  Result := Round(B1 + (E1 - B1) * P) + Round(B2 + (E2 - B2) * P) shl 8 + Round(B3 + (E3 - B3) * P) shl 16;
end;

function TplCircleProgress.AnglePosition(R: TRect; AMin, AMax, APosition: integer): TPoint;
var
  a: double;
begin
  a := (StartAngle - 90) + 360 * (APosition / (AMax - AMin) - AMin);
  a := pi * a / 180;
  with R do
  begin
    Result.X := Round(Cos(a) * (Right - Left) / 2 + (Left + Right) / 2);
    Result.Y := Round(Sin(a) * (Bottom - Top) / 2 + (Bottom + Top) / 2);
  end;
end;

procedure TplCircleProgress.UpdateAngleGradientBrush;
var
  R: TRect;
  i: integer;
  P1, P2: TPoint;
begin
  if GradientMode <> gmAngle then
    FreeAndNil(FGradient)
  else
  begin
    FGradient := TBitmap.Create;
    with FGradient do
    begin
      if ClientWidth < ClientHeight then
      begin
        Width := ClientWidth * BufScale;
        Height := ClientWidth * BufScale;
      end
      else
      begin
        Width := ClientHeight * BufScale;
        Height := ClientHeight * BufScale;
      end;
      R := Rect(0, 0, Width, Height);
      with Canvas do
      begin
        Pen.Style := psClear;
        for i := 0 to 99 do
          with R do
          begin
            P1 := AnglePosition(R, 0, 100, i);
            P2 := AnglePosition(R, 0, 100, Succ(i));
            Brush.Color := GradientColor(ColorDoneMin, ColorDoneMax, 0, 100, i);
            Pie(Left, Top, Right, Bottom, P2.X, P2.Y, P1.X, P1.Y);
          end;
      end;
    end;

  end;
end;

procedure TplCircleProgress.WndProc(var Message: TLMessage);
begin
  with Message do
    case Msg of
      WM_ERASEBKGND: Result := 1;
      WM_SIZE:
      begin
        UpdateAngleGradientBrush;
        inherited;
      end;
      else
        inherited;
    end;
end;

procedure TplCircleProgress.Loaded;
begin
  inherited;
  UpdateAngleGradientBrush;
end;

procedure TplCircleProgress.Paint;
var
  Buf: TBitmap;
  R, RR: TRect;
  P1, P2: TPoint;
  S: String;
begin
  if Width > Height then
    R := Rect(0, 0, Height, Height)
  else
    R := Rect(0, 0, Width, Width);
  case ProgressAlign of
    paCenter: OffsetRect(R, (Width - R.Right) div 2, (Height - R.Bottom) div 2);
    paRightOrBottom: OffsetRect(R, Width - R.Right, Height - R.Bottom);
  end;
    // Create Buffer
    Buf := TBitmap.Create;
    Buf.Width := BufScale * ClientWidth;
    Buf.Height := BufScale * ClientHeight;

    // ------------------------------------------------------------
    // Draw to Buffer

    Buf.Canvas.Pen.Color :=self.Color;
    Buf.Canvas.Brush.Color :=self.Color;
    Buf.Canvas.FillRect(Rect(0, 0, Buf.Width, Buf.Height));  //Clear

    Buf.Canvas.Pen.Style := psClear;
    Buf.Canvas.Brush.Color := ColorRemain;

      with R do
      begin
        Left := Left * BufScale;
        Right := Right * BufScale;
        Top := Top * BufScale;
        Bottom := Bottom * BufScale;
        RR := Rect(Left + (Right - Left) div 4, Top + (Bottom - Top) div 4, Right - (Right - Left) div 4, Bottom - (Bottom - Top) div 4);
      end;

      with R do
      begin
        Buf.Canvas.Ellipse(Left, Top, Right, Bottom);
        if Position > Min then
        begin
          P1 := AnglePosition(R, Min, Max, 0);
          P2 := AnglePosition(R, Min, Max, Position);
          case GradientMode of
            gmNone: Buf.Canvas.Brush.Color := ColorDoneMax;
            gmPosition: Buf.Canvas.Brush.Color := GradientColor(ColorDoneMin, ColorDoneMax, Min, Max, Position);
            gmAngle:
            begin
              Buf.Canvas.Brush.Bitmap := FGradient;
              {case ProgressAlign of
                paCenter: SetBrushOrgEx(Handle,(Width-FGradient.Width-BufScale) div 2,(Height-FGradient.Height-BufScale) div 2,nil);
                paRightOrBottom: SetBrushOrgEx(Handle,Width-FGradient.Width,Height-FGradient.Height,nil);
              else SetBrushOrgEx(Handle,0,0,nil);   }
            end;
          end;
        end;
        Buf.Canvas.Pie(Left, Top, Right, Bottom, P2.X, P2.Y, P1.X, P1.Y);
      end;

      if InnerSize > 0 then
      begin
        with R do
        begin
          RR := Rect(Left + (100 - InnerSize) * (Right - Left) div 200, Top + (100 - InnerSize) * (Bottom - Top) div
            200, Right - (100 - InnerSize) * (Right - Left) div 200, Bottom - (100 - InnerSize) * (Bottom - Top) div 200);
          Buf.Canvas.Brush.Color := ColorInner;
          with RR do
            Buf.Canvas.Ellipse(Left, Top, Right, Bottom);

        end;
      end;

    // ------------------------------------------------------------
    //Copy Buffer to Canvas

    SetStretchBltMode(Canvas.Handle, HALFTONE);
    Canvas.CopyRect(Rect(0, 0, ClientWidth, ClientHeight), Buf.Canvas, Rect(0, 0, Buf.Width, Buf.Height));

   // ------------------------------------------------------------
    //Draw Textr to Canvas
    if ShowText then
    begin
      S := IntToStr(100 * (Position - Min) div (Max - Min)) + '%';

      if Assigned(OnText) then OnText(Self, S);

      Canvas.Font.Assign(Self.Font);
      Canvas.Brush.Style := bsClear;

      R := ClientRect;
      DrawText(Canvas.Handle, PChar(S), -1, R, DT_NOPREFIX or DT_CENTER or DT_CALCRECT);

      with R do
        R := Rect((ClientWidth - Right + Left) div 2, (ClientHeight - Bottom + Top) div 2, ClientWidth -
          (ClientWidth - (Right - Left)) div 2, ClientHeight - (ClientHeight - (Bottom - Top)) div 2);

      OffsetRect(R, 0, -InnerSize * ClientHeight div 6000);
      DrawText(Canvas.Handle, PChar(S), -1, R, DT_NOPREFIX or DT_CENTER);
    end;


  Buf.Free;  // finally

end;

end.
