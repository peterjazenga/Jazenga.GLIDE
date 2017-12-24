{**********************************************************************
 Package pl_ExControls.pkg
 From PilotLogic Software House(http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit TplA3nalogGaugeUnit;

interface

uses
  Messages, LMessages, SysUtils, Classes, Graphics, IntfGraphics, LCLIntf,
  LCLType, Controls;

type
  TStyle = (LeftStyle, RightStyle, CenterStyle);
  TFaceOption = (ShowMargin, ShowCircles, ShowMainTicks, ShowSubTicks,
    ShowIndicatorMin, ShowIndicatorMid, ShowIndicatorMax,
    ShowValues, ShowCenter, ShowFrame, Show3D, ShowCaption);
  TFaceOptions = set of TFaceOption;
  TAntialiased = (aaNone, aaBiline, aaTriline, aaQuadral);

  TplA3nalogGauge = class(TCustomControl)
  private
    // face elements colors
    FMinColor: TColor;
    FMidColor: TColor;
    FMaxColor: TColor;
    FFaceColor: TColor;
    FTicksColor: TColor;
    FValueColor: TColor;
    FCaptionColor: TColor;
    FArrowColor: TColor;
    FMarginColor: TColor;
    FCenterColor: TColor;
    FCircleColor: TColor;
    // face elements sizes, etc.
    FCenterRadius: integer;
    FCircleRadius: integer;
    FScaleAngle: integer;
    FMargin: integer;
    FStyle: TStyle;
    FArrowWidth: integer;
    FNumMainTicks: integer;
    FLengthMainTicks: integer;
    FLengthSubTicks: integer;
    FFaceOptions: TFaceOptions;
    // values
    FPosition: single;
    FScaleValue: integer;
    FMinimum: integer;
    FMaximum: integer;
    FCaption: string;
    // event handlers
    FOverMax: TNotifyEvent;
    FOverMin: TNotifyEvent;
    // anti-aliasing mode
    FAntiAliased: TAntialiased;
    // internal bitmaps
    FBackBitmap: TBitmap;
    FFaceBitmap: TBitmap;
    FAABitmap: TBitmap;
    // set properties
    procedure SetFMinColor(C: TColor);
    procedure SetFMidColor(C: TColor);
    procedure SetFMaxColor(C: TColor);
    procedure SetFFaceColor(C: TColor);
    procedure SetFTicksColor(C: TColor);
    procedure SetFValueColor(C: TColor);
    procedure SetFCaptionColor(C: TColor);
    procedure SetFArrowColor(C: TColor);
    procedure SetFMarginColor(C: TColor);
    procedure SetFCenterColor(C: TColor);
    procedure SetFCircleColor(C: TColor);
    procedure SetFCenterRadius(I: integer);
    procedure SetFCircleRadius(I: integer);
    procedure SetFScaleAngle(I: integer);
    procedure SetFMargin(I: integer);
    procedure SetFStyle(S: TStyle);
    procedure SetFArrowWidth(I: integer);
    procedure SetFNumMainTicks(I: integer);
    procedure SetFLengthMainTicks(I: integer);
    procedure SetFLengthSubTicks(I: integer);
    procedure SetFFaceOptions(O: TFaceOptions);
    procedure SetFPosition(V: single);
    procedure SetFScaleValue(I: integer);
    procedure SetFMaximum(I: integer);
    procedure SetFMinimum(I: integer);
    procedure SetFCaption(const S: string);
    procedure SetFAntiAliased(V: TAntialiased);
    function GetAAMultipler: integer;
  protected
    procedure DrawScale(Bitmap: TBitmap; K: integer);
    procedure DrawArrow(Bitmap: TBitmap; K: integer);
    procedure DrawALL;
    procedure RedrawArrow;
    procedure FastAntiAliasPicture;
    procedure Paint; override;
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
    procedure CMFontChanged(var Msg: TLMessage); message CM_FontChanged;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Font;
    property MinColor: TColor read FMinColor write SetFMinColor;
    property MidColor: TColor read FMidColor write SetFMidColor;
    property MaxColor: TColor read FMaxColor write SetFMaxColor;
    property FaceColor: TColor read FFaceColor write SetFFaceColor;
    property TicksColor: TColor read FTicksColor write SetFTicksColor;
    property ValueColor: TColor read FValueColor write SetFValueColor;
    property CaptionColor: TColor read FCaptionColor write SetFCaptionColor;
    property ArrowColor: TColor read FArrowColor write SetFArrowColor;
    property MarginColor: TColor read FMarginColor write SetFMarginColor;
    property CenterColor: TColor read FCenterColor write SetFCenterColor;
    property CircleColor: TColor read FCircleColor write SetFCircleColor;
    property CenterRadius: integer read FCenterRadius write SetFCenterRadius;
    property CircleRadius: integer read FCircleRadius write SetFCircleRadius;
    property Angle: integer read FScaleAngle write SetFScaleAngle;
    property Margin: integer read FMargin write SetFMargin;
    property Style: TStyle read FStyle write SetFStyle;
    property ArrowWidth: integer read FArrowWidth write SetFArrowWidth;
    property NumberMainTicks: integer read FNumMainTicks write SetFNumMainTicks;
    property LengthMainTicks: integer read FLengthMainTicks write SetFLengthMainTicks;
    property LengthSubTicks: integer read FLengthSubTicks write SetFLengthSubTicks;
    property FaceOptions: TFaceOptions read FFaceOptions write SetFFaceOptions;
    property Position: single read FPosition write SetFPosition;
    property Scale: integer read FScaleValue write SetFScaleValue;
    property IndMaximum: integer read FMaximum write SetFMaximum;
    property IndMinimum: integer read FMinimum write SetFMinimum;
    property Caption: string read FCaption write SetFCaption;
    property AntiAliased: TAntialiased read FAntiAliased write SetFAntiAliased;
    property OnOverMax: TNotifyEvent read FOverMax write FOverMax;
    property OnOverMin: TNotifyEvent read FOverMin write FOverMin;
  end;


implementation

{ ========================================================================= }
constructor TplA3nalogGauge.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csReplicatable, csOpaque];

  fAntiAliased := aaNone;
  FBackBitmap := TBitmap.Create;
  FFaceBitmap := TBitmap.Create;
  FAABitmap := nil;

  setbounds(0, 0, 225, 180);

  fCaption := 'mV';

  FFaceColor := clBtnFace;
  FTicksColor := clBlack;
  FValueColor := clBlack;
  FCaptionColor := clBlack;
  FArrowColor := clBlack;
  FMarginColor := clBlack;
  FCenterColor := clDkGray;
  FCircleColor := clBlue;
  FMinColor := clGreen;
  FMidColor := clYellow;
  FMaxColor := clRed;
  FArrowWidth := 1;
  FPosition := 0;
  FMargin := 10;
  FStyle := CenterStyle;
  FScaleValue := 140;
  FMaximum := 80;
  FMinimum := 30;
  FScaleAngle := 120;
  FCircleRadius := 3;
  FCenterRadius := 8;
  FNumMainTicks := 7;
  FLengthMainTicks := 15;
  FLengthSubTicks := 8;

  FFaceOptions := [ShowMargin, ShowMainTicks, ShowSubTicks, ShowIndicatorMax, ShowValues,
    ShowCenter, ShowFrame, Show3D, ShowCaption];


  FFaceBitmap.Canvas.Brush.Style := bsSolid;//bsClear;
  FFaceBitmap.Canvas.Brush.Color := FFaceColor;
  FFaceBitmap.Width := Width;
  FFaceBitmap.Height := Height;

  FBackBitmap.Canvas.Brush.Style := bsSolid;//bsClear;
  FBackBitmap.Canvas.Brush.Color := Self.Color;
  FBackBitmap.Width := Width;
  FBackBitmap.Height := Height;

end;

destructor TplA3nalogGauge.Destroy;
begin
  FBackBitmap.Free;
  FFaceBitmap.Free;
  FAABitmap.Free;
  inherited;
end;

{ ------------------------------------------------------------------------- }
procedure SetPenStyles(aPen: TPen; aWidth: integer; aColor: TColor);
begin
  aPen.Width := aWidth;
  aPen.Color := aColor;
end;

procedure TplA3nalogGauge.DrawScale(Bitmap: TBitmap; K: integer);
var
  I, J, X, Y, N, M, W, H, R: integer;
  Max, Min: int64;
  A, C: single;
begin
  Bitmap.Canvas.Font := Font;
  W := Bitmap.Width;
  H := Bitmap.Height;
  Max := FMaximum;
  Min := FMinimum;
  N := FNumMainTicks * 5;
  M := FMargin * K;
  R := FCircleRadius * K;
  with Bitmap do
  begin

    Canvas.Brush.Style := bsSolid;

    Canvas.Brush.Color := FFaceColor;
    Canvas.FillRect(Canvas.ClipRect);
    Canvas.Font.Height := Canvas.Font.Height * K;
    // ***************************** Out Frame **************************
    if ShowFrame in fFaceOptions then
    begin
      if Show3D in fFaceOptions then
      begin
        Canvas.Pen.Width := 2 * K;
        Canvas.Pen.Color := clBtnShadow;
        Canvas.MoveTo(W, 0);
        Canvas.LineTo(0, 0);
        Canvas.LineTo(0, H);
        Canvas.Pen.Color := clBtnHighlight;
        Canvas.LineTo(W, H);
        Canvas.LineTo(W, 0);
      end
      else
      begin
        Canvas.Pen.Width := K;
        Canvas.Pen.Color := clBtnText;
        Canvas.Rectangle(0, 0, W, H);
      end;
    end;
    //************************* Out Margins **************************
    if ShowMargin in fFaceOptions then
    begin
      Canvas.Pen.Color := FMarginColor;
      Canvas.Pen.Width := K;
      Canvas.Rectangle(M, M, W - M, H - M);
    end;
    //****************************************************************
    case fStyle of
      RightStyle:
      begin
        A := 0;
        C := W - M;
        X := W - M;
        Y := H - M;
        if fScaleAngle > 90 then
          fScaleAngle := 90;
        J := W - 2 * M;
      end;
      LeftStyle:
      begin
        A := 90;
        C := M;
        X := M;
        Y := H - M;
        if fScaleAngle > 90 then
          fScaleAngle := 90;
        J := W - 2 * M;
      end;
      else
      begin
        X := W div 2;
        A := (180 - fScaleAngle) / 2;
        C := W / 2;
        if fScaleAngle >= 180 then
        begin
          J := (W - 2 * M) div 2;
          Y := H div 2;
        end
        else
        begin
          J := Round(((W - 2 * M) / 2) / Cos(A * 2 * pi / 360));
          if J > H - 2 * M then
            J := H - 2 * M;
          Y := (H - J) div 2 + J;
        end;
      end;
    end;{case}

{    // ************************************ base formula **********************************************
    Canvas.MoveTo(X, Y);
    Canvas.LineTo(Round(C-J*Cos((A+I*(FScaleAngle)/FNumMainTicks)*2*Pi/360)),
                  Round(Y-J*Sin((A+I*(FScaleAngle)/FNumMainTicks)*2*Pi/360))); }
    //******************************** Out Caption *******************
    if (ShowCaption in FFaceOptions) then
    begin
      Canvas.Font.Color := FCaptionColor;
      Canvas.TextOut(Round(C - J / 2 * Cos((A + FScaleAngle / 2) * 2 * Pi / 360)) - Canvas.TextWidth(FCaption) div 2,
        Round(Y - J / 2 * Sin((A + FScaleAngle / 2) * 2 * Pi / 360)),
        FCaption);
    end;
    //********************************** Out MinMaxLines *************************************
    if (ShowIndicatorMax in FFaceOptions) then
    begin
      SetPenStyles(Canvas.Pen, 4 * K, FMaxColor);
      Canvas.Arc(X - J, Y - J, X + J, Y + J,
        Round(C - J * Cos((A + FScaleAngle) * 2 * Pi / 360)),
        Round(Y - J * Sin((A + FScaleAngle) * 2 * Pi / 360)),
        Round(C - J * Cos((A + Max * FScaleAngle / FScaleValue) * 2 * pi / 360)),
        Round(Y - J * Sin((A + Max * FScaleAngle / FScaleValue) * 2 * pi / 360)));
    end;
    if (ShowIndicatorMid in FFaceOptions) and (FMinimum < FMaximum) then
    begin
      SetPenStyles(Canvas.Pen, 4 * K, FMidColor);
      Canvas.Arc(X - J, Y - J, X + J, Y + J,
        Round(C - J * Cos((A + Max * FScaleAngle / FScaleValue) * 2 * pi / 360)),
        Round(Y - J * Sin((A + Max * FScaleAngle / FScaleValue) * 2 * pi / 360)),
        Round(C - J * Cos((A + Min * FScaleAngle / FScaleValue) * 2 * pi / 360)),
        Round(Y - J * Sin((A + Min * FScaleAngle / FScaleValue) * 2 * pi / 360)));
    end;
    if (ShowIndicatorMin in FFaceOptions) then
    begin
      SetPenStyles(Canvas.Pen, 4 * K, FMinColor);
      Canvas.Arc(X - J, Y - J, X + J, Y + J,
        Round(C - J * Cos((A + Min * FScaleAngle / FScaleValue) * 2 * Pi / 360)),
        Round(Y - J * Sin((A + Min * FScaleAngle / FScaleValue) * 2 * Pi / 360)),
        Round(C - J * Cos(A * 2 * Pi / 360)),
        Round(Y - J * Sin(A * 2 * Pi / 360)));
    end;
    Canvas.Font.Color := FValueColor;
    Canvas.Pen.Color := FTicksColor;
    Canvas.Pen.Width := K;
    //********************************** Out SubTicks *************************************
    if ShowSubTicks in fFaceOptions then
      for I := 0 to N do
      begin
        Canvas.MoveTo(Round(C - (J - FLengthSubTicks * K) * Cos((A + I * (FScaleAngle) / N) * 2 * pi / 360)),
          Round(Y - (J - FLengthSubTicks * K) * Sin((A + I * (FScaleAngle) / N) * 2 * pi / 360)));
        Canvas.LineTo(round(C - (J) * Cos((A + I * (FScaleAngle) / N) * 2 * pi / 360)),
          round(Y - (J) * Sin((A + I * (FScaleAngle) / N) * 2 * pi / 360)));
      end;
    //********************************** Out Main Ticks ************************************
    for I := 0 to FNumMainTicks do
    begin
      if ShowMainTicks in fFaceOptions then
      begin
        Canvas.MoveTo(Round(C - (J - FLengthMainTicks * K) * Cos((A + I * (FScaleAngle) / FNumMainTicks) * 2 * Pi / 360)),
          Round(Y - (J - FLengthMainTicks * K) * Sin((A + I * (FScaleAngle) / FNumMainTicks) * 2 * Pi / 360)));
        Canvas.LineTo(Round(C - (J) * Cos((A + I * (FScaleAngle) / FNumMainTicks) * 2 * Pi / 360)),
          Round(Y - (J) * Sin((A + I * (FScaleAngle) / FNumMainTicks) * 2 * Pi / 360)));
      end;
      //************************************* Out Circles ************************************
      if ShowCircles in fFaceOptions then
      begin
        Canvas.Brush.Color := FCircleColor;
        Canvas.Ellipse(Round(C - J * Cos((A + I * (FScaleAngle) / FNumMainTicks) * 2 * Pi / 360)) - R,
          Round(Y - J * Sin((A + I * (FScaleAngle) / FNumMainTicks) * 2 * Pi / 360)) - R,
          Round(C - J * Cos((A + I * (FScaleAngle) / FNumMainTicks) * 2 * Pi / 360)) + R,
          Round(Y - J * Sin((A + I * (FScaleAngle) / FNumMainTicks) * 2 * Pi / 360)) + R);
      end;
      // ************************************* Out Values *************************************
      if ShowValues in fFaceOptions then
      begin
        Canvas.Brush.Color := FFaceColor;
        Canvas.TextOut(Round(C - (J - fLengthMainTicks * K - 5 - I) * cos((A + i * (fScaleAngle) / fNumMainTicks) * 2 * pi / 360)) -
          Canvas.TextWidth(IntToStr(i * fScaleValue div fNumMainTicks)) div 2,
          Round(Y - (J - fLengthMainTicks * K - 5) * sin((A + i * (fScaleAngle) / fNumMainTicks) * 2 * pi / 360)),
          IntToStr(i * fScaleValue div fNumMainTicks));
      end;
    end;
  end;
end;

procedure TplA3nalogGauge.DrawArrow(Bitmap: TBitmap; K: integer);
var
  J, X, Y, M, W, H, R: integer;
  A, C: single;
begin
  M := FMargin * K;
  R := FCenterRadius * K;
  W := Bitmap.Width;
  H := Bitmap.Height;
  with Bitmap do
  begin
    case FStyle of
      RightStyle:
      begin
        A := 0;
        C := W - M;
        X := W - M;
        Y := H - M;
        if FScaleAngle > 90 then
          FScaleAngle := 90;
        J := W - 2 * M;
      end;
      LeftStyle:
      begin
        A := 90;
        C := M;
        X := M;
        Y := H - M;
        if FScaleAngle > 90 then
          FScaleAngle := 90;
        J := W - 2 * M;
      end;
      else
      begin
        X := W div 2;
        A := (180 - fScaleAngle) / 2;
        C := W / 2;
        if FScaleAngle >= 180 then
        begin
          J := (W - 2 * M) div 2;
          Y := H div 2;
        end
        else
        begin
          J := Round(((W - 2 * M) / 2) / Cos(A * 2 * pi / 360));
          if J > H - 2 * M then
            J := H - 2 * M;
          Y := (H - J) div 2 + J;
        end;
      end;
    end;{case}
    Canvas.Pen.Width := FArrowWidth * K;
    Canvas.Pen.Color := FArrowColor;
    Canvas.MoveTo(X, Y);
    Canvas.LineTo(Round(C - J * Cos((A + FPosition * FScaleAngle / FScaleValue) * 2 * pi / 360)),
      Round(Y - J * Sin((A + FPosition * FScaleAngle / FScaleValue) * 2 * pi / 360)));
    //********************************* Out Center ***************************************
    if ShowCenter in FFaceOptions then
    begin
      Canvas.Brush.Color := FCenterColor;
      Canvas.Ellipse(X - R, Y - R, X + R, Y + R);
    end;
  end;
end;

procedure TplA3nalogGauge.RedrawArrow;
begin
  BitBlt(FFaceBitmap.Canvas.Handle, 0, 0, FBackBitmap.Width, FBackBitmap.Height, FBackBitmap.Canvas.Handle, 0, 0, SRCCOPY);
  DrawArrow(FFaceBitmap, GetAAMultipler);

  if FAntialiased <> aaNone then
    FastAntiAliasPicture;

  if FAntiAliased = aaNone then
    BitBlt(Canvas.Handle, 0, 0, FFaceBitmap.Width, FFaceBitmap.Height, FFaceBitmap.Canvas.Handle, 0, 0, SRCCOPY)
  else
    BitBlt(Canvas.Handle, 0, 0, FAABitmap.Width, FAABitmap.Height, FAABitmap.Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure TplA3nalogGauge.DrawALL;
begin
  DrawScale(FBackBitmap, GetAAMultipler);
  RedrawArrow;
end;

const
  MaxPixelCount = MaxInt div SizeOf(TRGBTriple);

type
  PRGBArray = ^TRGBArray;
  TRGBArray = array[0..MaxPixelCount - 1] of TRGBTriple;

procedure TplA3nalogGauge.FastAntiAliasPicture;
var
  x, y, cx, cy, cxi: integer;
  totr, totg, totb: integer;
  Row1, Row2, Row3, Row4, DestRow: PRGBArray;
  i, k: integer;
begin
  // For each row
  K := GetAAMultipler;
  Row2 := nil;
  Row3 := nil;
  Row4 := nil;
  //============================================


  for Y := 0 to FAABitmap.Height - 1 do
  begin
    // We compute samples of K x K pixels
    cy := y * K;
    // Get pointers to actual, previous and next rows in supersampled bitmap

    Row1 := FFaceBitmap.ScanLine[cy];
    // Row1 := RowBmp1.GetDataLineStart(cy);

    if K > 1 then
      Row2 := FFaceBitmap.ScanLine[cy + 1];
    if K > 2 then
      Row3 := FFaceBitmap.ScanLine[cy + 2];
    if K > 3 then
      Row4 := FFaceBitmap.ScanLine[cy + 3];
    // Get a pointer to destination row in output bitmap
    DestRow := FAABitmap.ScanLine[y];

    // For each column...
    for x := 0 to FAABitmap.Width - 1 do
    begin
      // We compute samples of 3 x 3 pixels
      cx := x * K;
      // Initialize result color
      totr := 0;
      totg := 0;
      totb := 0;
      if K > 3 then
      begin
        for i := 0 to 3 do
        begin
          cxi := cx + i;
          totr := totr + Row1^[cxi].rgbtRed + Row2^[cxi].rgbtRed + Row3^[cxi].rgbtRed + Row4^[cxi].rgbtRed;
          totg := totg + Row1^[cxi].rgbtGreen + Row2^[cxi].rgbtGreen + Row3^[cxi].rgbtGreen + Row4^[cxi].rgbtGreen;
          totb := totb + Row1^[cxi].rgbtBlue + Row2^[cxi].rgbtBlue + Row3^[cxi].rgbtBlue + Row4^[cxi].rgbtBlue;
        end;
        DestRow^[x].rgbtRed := totr div 16;
        DestRow^[x].rgbtGreen := totg div 16;
        DestRow^[x].rgbtBlue := totb div 16;
      end
      else if K > 2 then
      begin
        for i := 0 to 2 do
        begin
          cxi := cx + i;
          totr := totr + Row1^[cxi].rgbtRed + Row2^[cxi].rgbtRed + Row3^[cxi].rgbtRed;
          totg := totg + Row1^[cxi].rgbtGreen + Row2^[cxi].rgbtGreen + Row3^[cxi].rgbtGreen;
          totb := totb + Row1^[cxi].rgbtBlue + Row2^[cxi].rgbtBlue + Row3^[cxi].rgbtBlue;
        end;
        DestRow^[x].rgbtRed := totr div 9;
        DestRow^[x].rgbtGreen := totg div 9;
        DestRow^[x].rgbtBlue := totb div 9;
      end
      else if K > 1 then
      begin
        for i := 0 to 1 do
        begin
          cxi := cx + i;
          totr := totr + Row1^[cxi].rgbtRed + Row2^[cxi].rgbtRed;
          totg := totg + Row1^[cxi].rgbtGreen + Row2^[cxi].rgbtGreen;
          totb := totb + Row1^[cxi].rgbtBlue + Row2^[cxi].rgbtBlue;
        end;
        DestRow^[x].rgbtRed := totr div 4;
        DestRow^[x].rgbtGreen := totg div 4;
        DestRow^[x].rgbtBlue := totb div 4;
      end
      else
      begin
        DestRow^[x].rgbtRed := Row1^[cx].rgbtRed;
        DestRow^[x].rgbtGreen := Row1^[cx].rgbtGreen;
        DestRow^[x].rgbtBlue := Row1^[cx].rgbtBlue;
      end;
    end;
  end;

end;

procedure TplA3nalogGauge.Paint;
begin
  inherited Paint;
    {
  if FAntiAliased = aaNone then
    BitBlt(Canvas.Handle, 0, 0, FFaceBitmap.Width,FFaceBitmap.Height,FFaceBitmap.Canvas.Handle, 0, 0, SRCCOPY)
    else
    BitBlt(Canvas.Handle, 0, 0, FAABitmap.Width,FAABitmap.Height, FAABitmap.Canvas.Handle, 0, 0, SRCCOPY);
     }
  DrawALL;
end;

procedure TplA3nalogGauge.WMSize(var Message: TLMSize);
var
  K: integer;
begin
  inherited;

  if Width < 60 then
    Width := 60;
  if Height < 50 then
    Height := 50;
  if FAntiAliased = aaNone then
  begin
    FBackBitmap.Width := Width;
    FBackBitmap.Height := Height;
    FFaceBitmap.Width := Width;
    FFaceBitmap.Height := Height;
  end
  else
  begin
    K := GetAAMultipler;
    FBackBitmap.Width := Width * K;
    FBackBitmap.Height := Height * K;
    FFaceBitmap.Width := Width * K;
    FFaceBitmap.Height := Height * K;
    FAABitmap.Width := Width;
    FAABitmap.Height := Height;
  end;

  DrawALL;
end;

procedure TplA3nalogGauge.CMFontChanged(var Msg: TLMessage);
begin
  DrawALL;
end;

{ ------------------------------------------------------------------------- }
procedure TplA3nalogGauge.SetFMinColor(C: TColor);
begin
  if C <> FMinColor then
  begin
    FMinColor := C;
    DrawALL;
  end;
end;

procedure TplA3nalogGauge.SetFMidColor(C: TColor);
begin
  if C <> FMidColor then
  begin
    FMidColor := C;
    DrawALL;
  end;
end;

procedure TplA3nalogGauge.SetFMaxColor(C: TColor);
begin
  if C <> FMaxColor then
  begin
    FMaxColor := C;
    DrawALL;
  end;
end;

procedure TplA3nalogGauge.SetFFaceColor(C: TColor);
begin
  if C <> FFaceColor then
  begin
    FFaceColor := C;
    DrawALL;
  end;
end;

procedure TplA3nalogGauge.SetFTicksColor(C: TColor);
begin
  if C <> FTicksColor then
  begin
    FTicksColor := C;
    DrawALL;
  end;
end;

procedure TplA3nalogGauge.SetFValueColor(C: TColor);
begin
  if C <> FValueColor then
  begin
    FValueColor := C;
    DrawALL;
  end;
end;

procedure TplA3nalogGauge.SetFCaptionColor(C: TColor);
begin
  if C <> FCaptionColor then
  begin
    FCaptionColor := C;
    DrawALL;
  end;
end;

procedure TplA3nalogGauge.SetFArrowColor(C: TColor);
begin
  if C <> FArrowColor then
  begin
    FArrowColor := C;
    RedrawArrow;
  end;
end;

procedure TplA3nalogGauge.SetFMarginColor(C: TColor);
begin
  if C <> FMarginColor then
  begin
    FMarginColor := C;
    DrawALL;
  end;
end;

procedure TplA3nalogGauge.SetFCenterColor(C: TColor);
begin
  if C <> FCenterColor then
  begin
    FCenterColor := C;
    DrawALL;
  end;
end;

procedure TplA3nalogGauge.SetFCircleColor(C: TColor);
begin
  if C <> FCircleColor then
  begin
    FCircleColor := C;
    DrawALL;
  end;
end;

procedure TplA3nalogGauge.SetFCenterRadius(I: integer);
begin
  if I <> FCenterRadius then
  begin
    FCenterRadius := I;
    DrawALL;
  end;
end;

procedure TplA3nalogGauge.SetFCircleRadius(I: integer);
begin
  if I <> FCircleRadius then
  begin
    FCircleRadius := I;
    DrawALL;
  end;
end;

procedure TplA3nalogGauge.SetFScaleAngle(I: integer);
begin
  if I <> FScaleAngle then
  begin
    if (I > 10) and (I <= 360) then
      FScaleAngle := I;
    DrawALL;
  end;
end;

procedure TplA3nalogGauge.SetFMargin(I: integer);
begin
  if I <> FMargin then
  begin
    FMargin := I;
    DrawALL;
  end;
end;

procedure TplA3nalogGauge.SetFStyle(S: TStyle);
begin
  if S <> FStyle then
  begin
    FStyle := S;
    DrawALL;
  end;
end;

procedure TplA3nalogGauge.SetFArrowWidth(I: integer);
begin
  if I <> FArrowWidth then
  begin
    if I < 1 then
      fArrowWidth := 1
    else
    if I > 5 then
      fArrowWidth := 5
    else
      fArrowWidth := i;
    RedrawArrow;
  end;
end;

procedure TplA3nalogGauge.SetFNumMainTicks(I: integer);
begin
  if I <> FNumMainTicks then
  begin
    FNumMainTicks := I;
    DrawALL;
  end;
end;

procedure TplA3nalogGauge.SetFLengthMainTicks(I: integer);
begin
  if I <> FLengthMainTicks then
  begin
    FLengthMainTicks := I;
    DrawALL;
  end;
end;

procedure TplA3nalogGauge.SetFLengthSubTicks(I: integer);
begin
  if I <> FLengthSubTicks then
  begin
    FLengthSubTicks := I;
    DrawALL;
  end;
end;

procedure TplA3nalogGauge.SetFFaceOptions(O: TFaceOptions);
begin
  if O <> FFaceOptions then
  begin
    FFaceOptions := O;
    DrawALL;
  end;
end;

procedure TplA3nalogGauge.SetFPosition(V: single);
begin
  if V <> FPosition then
  begin
    FPosition := V;
    if (FPosition > fMaximum) and Assigned(FOverMax) then
      OnOverMax(Self);
    if (FPosition < fMinimum) and Assigned(FOverMin) then
      OnOverMin(Self);
    RedrawArrow;
  end;
end;

procedure TplA3nalogGauge.SetFScaleValue(I: integer);
begin
  if I <> FScaleValue then
  begin
    if I > 1 then
    begin
      FScaleValue := I;
      if FMaximum >= FScaleValue then
        FMaximum := FScaleValue - 1;
      if FMinimum > FScaleValue - FMaximum then
        FMinimum := FScaleValue - fMaximum;
    end;
    DrawALL;
  end;
end;

procedure TplA3nalogGauge.SetFMaximum(I: integer);
begin
  if I <> FMaximum then
  begin
    if (I > 0) and (I < FScaleValue) then
      FMaximum := I;
    DrawALL;
  end;
end;

procedure TplA3nalogGauge.SetFMinimum(I: integer);
begin
  if I <> FMinimum then
  begin
    if (I > 0) and (I < FScaleValue) then
      FMinimum := I;
    DrawALL;
  end;
end;

procedure TplA3nalogGauge.SetFCaption(const S: string);
begin
  if S <> FCaption then
  begin
    Canvas.Font := Font;
    FCaption := S;
    DrawALL;
  end;
end;

procedure TplA3nalogGauge.SetFAntiAliased(V: TAntialiased);
var
  K: integer;
begin

  FAntiAliased := V;

  if FAntiAliased = aaNone then
  begin
    FreeAndNil(FAABitmap);
    FreeAndNil(FBackBitmap);
    FreeAndNil(FFaceBitmap);

    FBackBitmap := TBitmap.Create;
    FFaceBitmap := TBitmap.Create;
    FBackBitmap.Width := Width;
    FFaceBitmap.Width := Width;
    FBackBitmap.Height := Height;
    FFaceBitmap.Height := Height;
  end
  else
  begin
    K := GetAAMultipler;
    FBackBitmap.PixelFormat := pf24bit;
    FFaceBitmap.PixelFormat := pf24bit;
    FBackBitmap.Width := Width * K;
    FFaceBitmap.Width := Width * K;
    FBackBitmap.Height := Height * K;
    FFaceBitmap.Height := Height * K;
    if not Assigned(FAABitmap) then
      FAABitmap := TBitmap.Create;
    FAABitmap.PixelFormat := pf24bit;
    FAABitmap.Width := Width;
    FAABitmap.Height := Height;
  end;

  DrawALL;

end;

function TplA3nalogGauge.GetAAMultipler: integer;
begin
  case FAntiAliased of
    aaBiline: Result := 2;
    aaTriline: Result := 3;
    aaQuadral: Result := 4;
    else
      Result := 1
  end;
end;

end.
