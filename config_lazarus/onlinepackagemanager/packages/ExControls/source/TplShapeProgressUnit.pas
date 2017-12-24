
{**********************************************************************
 Package pl_ExControls.pkg
 From PilotLogic Software House(http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit TplShapeProgressUnit;

interface

uses
  LResources, Classes, FPCanvas, SysUtils, Controls, Graphics, Math;

type
  TplShapeType = (stText, stPie, stImage,
    stBarHor, stBarVer,
    stRoundBarVer, stRoundBarHor);


  TplShapeProgress = class(TGraphicControl)
  private
    FShapeType: TplShapeType;
    FBackColor: TColor;
    FBorderStyle: TBorderStyle;
    FCurValue: longint;
    FFillColor: TColor;
    FFillStyle: TFPBrushStyle;
    FFillImage: TBitmap;
    FMaxValue: longint;
    FMinValue: longint;
    FShowText: boolean;
    function GetPercentDone: longint;
    procedure SetBackColor(AValue: TColor);
    procedure SetFillColor(AValue: TColor);
    procedure SetFillStyle(AValue: TFPBrushStyle);
    procedure SetFillImage(AValue: TBitmap);
    procedure SetShapeType(const AValue: TplShapeType);
    procedure SetMaxValue(AValue: longint);
    procedure SetMinValue(AValue: longint);
    procedure SetProgress(AValue: longint);
    procedure SetShowText(AValue: boolean);
    procedure DrawRoundBar;
    procedure DrawBar;
    procedure DrawText;
    procedure DrawPie;
    procedure DrawImage;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure AddProgress(AValue: longint);
    property PercentDone: longint read GetPercentDone;
  published
    property BackColor: TColor read FBackColor write SetBackColor default clWhite;
    property FillColor: TColor read FFillColor write SetFillColor default clBlack;
    property FillStyle: TFPBrushStyle read FFillStyle write SetFillStyle default bsSolid;
    property FillImage: TBitmap read FFillImage write SetFillImage;
    property ShapeType: TplShapeType read FShapeType write SetShapeType default stPie;
    property MinValue: longint read FMinValue write SetMinValue default 0;
    property MaxValue: longint read FMaxValue write SetMaxValue default 100;
    property Progress: longint read FCurValue write SetProgress;
    property ShowText: boolean read FShowText write SetShowText default True;
    property Visible;
    property Font;
    property ShowHint;
    property Align;
    property Anchors;
    property Color;
    property Constraints;
    property Enabled;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;

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

constructor TplShapeProgress.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  FBorderStyle := bsSingle;
  FFillColor := clBlack;
  FBackColor := clWhite;
  FShowText := True;
  FShapeType := stPie;
  Width := 100;
  Height := 100;
  FFillImage := TBitmap.Create;
  FMinValue := 0;
  FMaxValue := 100;
  FCurValue := 0;
end;

destructor TplShapeProgress.Destroy;
begin
  FFillImage.Free;
  inherited Destroy;
end;

procedure TplShapeProgress.Paint;
begin
  case ShapeType of
    stText: DrawText;
    stBarHor,
    stBarVer: DrawBar;
    stRoundBarHor,
    stRoundBarVer: DrawroundBar;
    stPie: DrawPie;
    stImage: DrawImage;
  end;

  if ShowText and (ShapeType <> stText) then
    DrawText;

  inherited Paint;
end;

procedure TplShapeProgress.SetFillColor(AValue: TColor);
begin
  if AValue = FFillColor then
    exit;
  FFillColor := AValue;
  Refresh;
end;

procedure TplShapeProgress.SetProgress(AValue: longint);
begin
  if AValue < FMinValue then
    AValue := FMinValue
  else if AValue > FMaxValue then
    AValue := FMaxValue;
  if FCurValue <> AValue then
  begin
    FCurValue := AValue;
    Refresh;
  end;
end;

procedure TplShapeProgress.AddProgress(AValue: longint);
begin
  Progress := FCurValue + AValue;
  Refresh;
end;

function TplShapeProgress.GetPercentDone: longint;
begin
  Result := trunc((FCurValue / FMaxValue) * 100);
end;

procedure TplShapeProgress.SetBackColor(AValue: TColor);
begin
  if AValue = FBackColor then
    exit;

  FBackColor := AValue;
  Refresh;
end;

procedure TplShapeProgress.SetMinValue(AValue: longint);
begin
  if AValue = FMinValue then
    exit;

  FMinValue := AValue;
  Refresh;
end;

procedure TplShapeProgress.SetMaxValue(AValue: longint);
begin
  if AValue = FMaxValue then
    exit;

  FMaxValue := AValue;
  Refresh;
end;

procedure TplShapeProgress.SetShowText(AValue: boolean);
begin
  if AValue = FShowText then
    exit;

  FShowText := AValue;
  Refresh;
end;

procedure TplShapeProgress.SetFillStyle(AValue: TFPBrushStyle);
begin
  if AValue = FFillStyle then
    exit;

  FFillStyle := AValue;
  Refresh;
end;

procedure TplShapeProgress.SetShapeType(const AValue: TplShapeType);
begin
  if AValue = FShapeType then
    exit;

  FShapeType := AValue;
  Refresh;
end;

procedure TplShapeProgress.SetFillImage(AValue: TBitmap);
var
  ximg: TBitmap;
begin
  if AValue <> FFillImage then
  begin
    ximg := TBitmap.Create;
    ximg.Assign(AValue);
    FFillImage.Height := self.Height;
    FFillImage.Width := self.Width;
    FFillImage.Canvas.StretchDraw(Rect(0, 0, Width, Height), ximg);
    ximg.Free;
    Refresh;
  end;
end;

//----------- Draw Functions ------------------------

procedure TplShapeProgress.DrawRoundBar;
var
  FillSize: longint;
  MinSize: longint;
begin

  with Canvas do
  begin
    Brush.Color := BackColor;
    Brush.Style := bsSolid;
    MinSize := Min(self.Width, self.Height);
    Pen.Color := clGray;
    RoundRect(0, 0, self.Width - 1, self.Height - 1, MinSize div 4, MinSize div 4);
    Pen.Color := clSilver;
    RoundRect(1, 1, self.Width - 2, self.Height - 2, MinSize div 4, MinSize div 4);
    Brush.Style := FillStyle;
    Brush.Color := FillColor;

    if percentdone > 0 then
      case ShapeType of
        stRoundBarHor:
        begin
          FillSize := Trunc((self.Width - 2) * (PercentDone / 100));
          RoundRect(Rect(1, 1, FillSize, self.Height - 2), MinSize div 4, MinSize div 4);
        end;

        stRoundBarVer:
        begin
          FillSize := Trunc((self.Height - 3) * (PercentDone / 100));
          RoundRect(Rect(1, Self.Height - 2 - FillSize, Self.Width - 2, Self.Height - 2),
            MinSize div 4, MinSize div 4);
        end;
      end;
  end;
end;

procedure TplShapeProgress.DrawBar;
var
  xfs: longint;
begin

  with Canvas do
  begin
    Brush.Color := BackColor;
    Brush.Style := bsSolid;
    Pen.Color := clGray;
    Rectangle(0, 0, self.Width, self.Height);
    Pen.Color := clSilver;
    Rectangle(1, 1, self.Width - 1, self.Height - 1);
    Brush.Style := FillStyle;
    Brush.Color := FillColor;

    if percentdone > 0 then
      case ShapeType of
        stBarHor:
        begin
          xfs := Trunc((self.Width - 4) * (PercentDone / 100));
          FillRect(Rect(2, 2, xfs + 2, self.Height - 2));
        end;
        stBarVer:
        begin
          xfs := Trunc((self.Height - 4) * (PercentDone / 100));
          FillRect(Rect(2, Self.Height - 2 - xfs, Self.Width - 2, Self.Height - 2));
        end;
      end;
  end;
end;

procedure TplShapeProgress.DrawText;
var
  X, Y: integer;
  S: string;
begin
  with Canvas do
  begin

    if ShapeType = stText then
    begin
      Brush.Color := BackColor;
      Brush.Style := bsSolid;
      Pen.Color := clGray;
      Rectangle(0, 0, self.Width, self.Height);
      Pen.Color := clSilver;
      Rectangle(1, 1, self.Width - 1, self.Height - 1);
    end;

    Font := Self.Font;

    S := format('%d%%', [PercentDone]);
    Y := self.Height div 2 - TextHeight(S) div 2;
    X := self.Width div 2 - TextWidth(S) div 2;

    TextRect(self.ClientRect, X, Y, S);
  end;
end;

procedure TplShapeProgress.DrawPie;
var
  MiddleX, MiddleY: integer;
  Angle: double;
begin
  with Canvas do
  begin
    Brush.Color := BackColor;
    Brush.Style := bsSolid;
    Pen.Color := clGray;
    Ellipse(0, 0, self.Width - 1, self.Height - 1);
    Pen.Color := clSilver;
    Ellipse(1, 1, self.Width - 2, self.Height - 2);
    Brush.Style := FillStyle;
    Brush.Color := FillColor;
    if PercentDone > 0 then
    begin
      MiddleX := (self.Width - 2) div 2;
      MiddleY := (self.Height - 2) div 2;
      Angle := (Pi * ((PercentDone / 50) + 0.5));
      Pie(1, 1, self.Width - 2, self.Height - 2,
        integer(Round(MiddleX * (1 - Cos(Angle)))),
        integer(Round(MiddleY * (1 - Sin(Angle)))), MiddleX + 1, 1);
    end;
  end;
end;

procedure TplShapeProgress.DrawImage;
var
  ximg: TBitmap;
  xfs, xms: longint;
  SRect, DRect: TRect;
begin
  with Canvas do
  begin
    ximg := TBitmap.Create;

    ximg.Width := self.Width;
    ximg.Height := Self.Height;
    ximg.Canvas.StretchDraw(rect(0, 0, self.Width, self.Height), FFillImage);
    Brush.Style := bsclear;
    xms := Min(self.Width + 2, self.Height + 2);
    xfs := Trunc((self.Width - 4) * (PercentDone / 100));
    DRect := Rect(2, 2, xfs + 2, self.Height + 2);
    CopyMode := cmSrcCopy;
    CopyRect(DRect, ximg.Canvas, DRect);
    Pen.Color := clGray;
    RoundRect(0, 0, self.Width - 1, self.Height - 1, xms div 4, xms div 4);
    Pen.Color := clSilver;
    RoundRect(1, 1, self.Width - 2, self.Height - 2, xms div 4, xms div 4);

    ximg.Free;
  end;
end;

end.
