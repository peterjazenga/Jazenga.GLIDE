
{**********************************************************************
 Package pl_ExControls.pkg
 From PilotLogic Software House(http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit TplPaintGridUnit;

interface

uses
  Messages, SysUtils, Classes, Controls, ExtCtrls, Graphics, Dialogs;

type

  TAGridMode = (mNone, mVer, mHor, mBoth);

  TAGLabelEvent = procedure(Sender: TObject; var LabelStr: string; Count, Pixel: integer;
    Value: extended) of object;

  TplPaintBoxGrid = class(TGraphicControl)
  private
    FMarginTop, FMarginBottom, FMarginLeft, FMarginRight, FXGridRange, FYGridRange, FXLabelRange, FYLabelRange: integer;
    FAGridMode: TAGridMode;
    FGridColor: TColor;
    FXMin, FXMax, FYMin, FYMax: extended;
    FXInterlabelGrid, FYInterlabelGrid, FXInteger, FYInteger: boolean;
    FOnPaint: TNotifyEvent;
    FOnXLabel, FOnYLabel: TAGLabelEvent;
  protected
    InPaint: boolean;
    XCoeficient, YCoeficient: extended;
    procedure SetMarginTop(V: integer);
    procedure SetMarginBottom(V: integer);
    procedure SetMarginLeft(V: integer);
    procedure SetMarginRight(V: integer);
    procedure SetXMin(V: extended);
    procedure SetXMax(V: extended);
    procedure SetYMin(V: extended);
    procedure SetYMax(V: extended);
    procedure SetXGridRange(V: integer);
    procedure SetYGridRange(V: integer);
    procedure SetXLabelRange(V: integer);
    procedure SetYLabelRange(V: integer);
    procedure SetAGridMode(GM: TAGridMode);
    procedure SetGridColor(GC: TColor);
    procedure SetXInterlabelGrid(ILG: boolean);
    procedure SetYInterlabelGrid(ILG: boolean);
    procedure SetXInteger(I: boolean);
    procedure SetYInteger(I: boolean);
    procedure Paint; override;
  public
    property Canvas;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetXPoint(X: extended): integer;
    function GetYPoint(Y: extended): integer;
    procedure RepaintGrid;
  published
    property Align;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property MarginTop: integer read FMarginTop write SetMarginTop;
    property MarginBottom: integer read FMarginBottom write SetMarginBottom;
    property MarginLeft: integer read FMarginLeft write SetMarginLeft;
    property MarginRight: integer read FMarginRight write SetMarginRight;
    property XMin: extended read FXMin write SetXMin;
    property XMax: extended read FXMax write SetXMax;
    property YMin: extended read FYMin write SetYMin;
    property YMax: extended read FYMax write SetYMax;
    property XGridRange: integer read FXGridRange write SetXGridRange;
    property YGridRange: integer read FYGridRange write SetYGridRange;
    property XLabelRange: integer read FXLabelRange write SetXLabelRange;
    property YLabelRange: integer read FYLabelRange write SetYLabelRange;
    property GridMode: TAGridMode read FAGridMode write SetAGridMode;
    property GridColor: TColor read FGridColor write SetGridColor;
    property XInterlabelGrid: boolean read FXInterlabelGrid write SetXInterlabelGrid;
    property YInterlabelGrid: boolean read FYInterlabelGrid write SetYInterlabelGrid;
    property XInteger: boolean read FXInteger write SetXInteger;
    property YInteger: boolean read FYInteger write SetYInteger;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property OnXLabel: TAGLabelEvent read FOnXLabel write FOnXLabel;
    property OnYLabel: TAGLabelEvent read FOnYLabel write FOnYLabel;
  end;

  TplBitmapGrid = class(TComponent)
  private
    FWidth, FHeight, FMarginTop, FMarginBottom, FMarginLeft, FMarginRight, FXGridRange, FYGridRange,
    FXLabelRange, FYLabelRange: integer;
    FAGridMode: TAGridMode;
    FFont: TFont;
    FColor, FGridColor: TColor;
    FXMin, FXMax, FYMin, FYMax: extended;
    FXInterlabelGrid, FYInterlabelGrid, FXInteger, FYInteger: boolean;
    FOnXLabel, FOnYLabel: TAGLabelEvent;
  protected
    XCoeficient, YCoeficient: extended;
    procedure SetWidth(V: integer);
    procedure SetHeight(V: integer);
    procedure SetMarginTop(V: integer);
    procedure SetMarginBottom(V: integer);
    procedure SetMarginLeft(V: integer);
    procedure SetMarginRight(V: integer);
    procedure SetXGridRange(V: integer);
    procedure SetYGridRange(V: integer);
    procedure SetXLabelRange(V: integer);
    procedure SetYLabelRange(V: integer);
  public
    Bitmap: TBitmap;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetXPoint(X: extended): integer;
    function GetYPoint(Y: extended): integer;
    procedure PaintGrid;
  published
    property Width: integer read FWidth write SetWidth;
    property Height: integer read FHeight write SetHeight;
    property MarginTop: integer read FMarginTop write SetMarginTop;
    property MarginBottom: integer read FMarginBottom write SetMarginBottom;
    property MarginLeft: integer read FMarginLeft write SetMarginLeft;
    property MarginRight: integer read FMarginRight write SetMarginRight;
    property XMin: extended read FXMin write FXMin;
    property XMax: extended read FXMax write FXMax;
    property YMin: extended read FYMin write FYMin;
    property YMax: extended read FYMax write FYMax;
    property XGridRange: integer read FXGridRange write SetXGridRange;
    property YGridRange: integer read FYGridRange write SetYGridRange;
    property XLabelRange: integer read FXLabelRange write SetXLabelRange;
    property YLabelRange: integer read FYLabelRange write SetYLabelRange;
    property GridMode: TAGridMode read FAGridMode write FAGridMode;
    property Font: TFont read FFont write FFont;
    property Color: TColor read FColor write FColor;
    property GridColor: TColor read FGridColor write FGridColor;
    property XInterlabelGrid: boolean read FXInterlabelGrid write FXInterlabelGrid;
    property YInterlabelGrid: boolean read FYInterlabelGrid write FYInterlabelGrid;
    property XInteger: boolean read FXInteger write FXInteger;
    property YInteger: boolean read FYInteger write FYInteger;
    property OnXLabel: TAGLabelEvent read FOnXLabel write FOnXLabel;
    property OnYLabel: TAGLabelEvent read FOnYLabel write FOnYLabel;
  end;


procedure PaintAutoGrid(Sender: TObject; Canvas: TCanvas;
  Width, Height, MarginTop, MarginBottom, MarginLeft, MarginRight, XGridRange, YGridRange,
  XLabelRange, YLabelRange: integer; GridMode: TAGridMode; GridColor: TColor;
  XMin, XMax, YMin, YMax: extended; XInterlabelGrid, YInterlabelGrid, XInteger, YInteger: boolean;
  var XCoeficient, YCoeficient: extended; Color: TColor; Font: TFont;
  OnXLabel, OnYLabel: TAGLabelEvent);

implementation

const
  SweepCoefficient = 10000;

//======================== TplPaintBoxGrid} ====================================

constructor TplPaintBoxGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  InPaint := False;
  Width := 400;
  Height := 300;
  FMarginTop := 10;
  FMarginBottom := 30;
  FMarginLeft := 50;
  FMarginRight := 20;
  FXMin := -30;
  FXMax := 30;
  FYMin := 0;
  FYMax := 100;
  FXGridRange := 7;
  FYGridRange := 7;
  FXLabelRange := 20;
  FYLabelRange := 11;
  FAGridMode := mBoth;
  FGridColor := clGray;
  FXInterlabelGrid := True;
  FYInterlabelGrid := True;
  FXInteger := False;
  FYInteger := False;
  XCoeficient := 0;
  YCoeficient := 0;
end;

destructor TplPaintBoxGrid.Destroy;
begin
  inherited Destroy;
end;

procedure TplPaintBoxGrid.SetMarginTop(V: integer);
begin
  if V < 0 then
    V := 0;
  FMarginTop := V;
  if csDesigning in ComponentState then
    RepaintGrid;
end;

procedure TplPaintBoxGrid.SetMarginBottom(V: integer);
begin
  if V < 0 then
    V := 0;
  FMarginBottom := V;
  if csDesigning in ComponentState then
    RepaintGrid;
end;

procedure TplPaintBoxGrid.SetMarginLeft(V: integer);
begin
  if V < 0 then
    V := 0;
  FMarginLeft := V;
  if csDesigning in ComponentState then
    RepaintGrid;
end;

procedure TplPaintBoxGrid.SetMarginRight(V: integer);
begin
  if V < 0 then
    V := 0;
  FMarginRight := V;
  if csDesigning in ComponentState then
    RepaintGrid;
end;

procedure TplPaintBoxGrid.SetXMin(V: extended);
begin
  FXMin := V;
  if csDesigning in ComponentState then
    RepaintGrid;
end;

procedure TplPaintBoxGrid.SetXMax(V: extended);
begin
  FXMax := V;
  if csDesigning in ComponentState then
    RepaintGrid;
end;

procedure TplPaintBoxGrid.SetYMin(V: extended);
begin
  FYMin := V;
  if csDesigning in ComponentState then
    RepaintGrid;
end;

procedure TplPaintBoxGrid.SetYMax(V: extended);
begin
  FYMax := V;
  if csDesigning in ComponentState then
    RepaintGrid;
end;

procedure TplPaintBoxGrid.SetXGridRange(V: integer);
begin
  if V < 1 then
    V := 1;
  FXGridRange := V;
  if csDesigning in ComponentState then
    RepaintGrid;
end;

procedure TplPaintBoxGrid.SetYGridRange(V: integer);
begin
  if V < 1 then
    V := 1;
  FYGridRange := V;
  if csDesigning in ComponentState then
    RepaintGrid;
end;

procedure TplPaintBoxGrid.SetXLabelRange(V: integer);
begin
  if V < 10 then
    V := 10;
  FXLabelRange := V;
  if csDesigning in ComponentState then
    RepaintGrid;
end;

procedure TplPaintBoxGrid.SetYLabelRange(V: integer);
begin
  if V < 10 then
    V := 10;
  FYLabelRange := V;
  if csDesigning in ComponentState then
    RepaintGrid;
end;

procedure TplPaintBoxGrid.SetAGridMode(GM: TAGridMode);
begin
  FAGridMode := GM;
  if csDesigning in ComponentState then
    RepaintGrid;
end;

procedure TplPaintBoxGrid.SetGridColor(GC: TColor);
begin
  FGridColor := GC;
  if csDesigning in ComponentState then
    RepaintGrid;
end;

procedure TplPaintBoxGrid.SetXInterlabelGrid(ILG: boolean);
begin
  FXInterlabelGrid := ILG;
  if csDesigning in ComponentState then
    RepaintGrid;
end;

procedure TplPaintBoxGrid.SetYInterlabelGrid(ILG: boolean);
begin
  FYInterlabelGrid := ILG;
  if csDesigning in ComponentState then
    RepaintGrid;
end;

procedure TplPaintBoxGrid.SetXInteger(I: boolean);
begin
  FXInteger := I;
  if csDesigning in ComponentState then
    RepaintGrid;
end;

procedure TplPaintBoxGrid.SetYInteger(I: boolean);
begin
  FYInteger := I;
  if csDesigning in ComponentState then
    RepaintGrid;
end;

function TplPaintBoxGrid.GetXPoint(X: extended): integer;
begin
  Result := Round((X - FXMin) * XCoeficient) + FMarginLeft;
end;

function TplPaintBoxGrid.GetYPoint(Y: extended): integer;
begin
  Result := Height - FMarginBottom - Round((Y - FYMin) * YCoeficient);
end;

procedure TplPaintBoxGrid.RepaintGrid;
begin
  PaintAutoGrid(Self, Canvas, Width, Height,
    FMarginTop, FMarginBottom, FMarginLeft, FMarginRight,
    FXGridRange, FYGridRange, FXLabelRange, FYLabelRange,
    FAGridMode, FGridColor, FXMin, FXMax, FYMin, FYMax,
    FXInterlabelGrid, FYInterlabelGrid, FXInteger, FYInteger,
    XCoeficient, YCoeficient, Color, Font, OnXLabel, OnYLabel);
end;

procedure TplPaintBoxGrid.Paint;
begin
  inherited Paint;
  if not InPaint then
  begin
    InPaint := True;
    try
      RepaintGrid;
      if Assigned(OnPaint) then
        OnPaint(Self);
    finally
      InPaint := False;
    end;
  end;
end;

//================= TFlatBitmapGrid ========================================

constructor TplBitmapGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Bitmap := TBitmap.Create;
  FFont := TFont.Create;
  FWidth := 400;
  FHeight := 300;
  FMarginTop := 10;
  FMarginBottom := 30;
  FMarginLeft := 50;
  FMarginRight := 20;
  FXMin := -30;
  FXMax := 30;
  FYMin := 0;
  FYMax := 100;
  FXGridRange := 7;
  FYGridRange := 7;
  FXLabelRange := 20;
  FYLabelRange := 13;
  FAGridMode := mBoth;
  FColor := clWhite;
  FGridColor := clGray;
  FXInterlabelGrid := True;
  FYInterlabelGrid := True;
  FXInteger := False;
  FYInteger := False;
  XCoeficient := 0;
  YCoeficient := 0;
end;

destructor TplBitmapGrid.Destroy;
begin
  BitMap.Free;
  inherited Destroy;
end;

procedure TplBitmapGrid.SetWidth(V: integer);
begin
  if V < 0 then
    V := 0;
  FWidth := V;
end;

procedure TplBitmapGrid.SetHeight(V: integer);
begin
  if V < 0 then
    V := 0;
  FHeight := V;
end;

procedure TplBitmapGrid.SetMarginTop(V: integer);
begin
  if V < 0 then
    V := 0;
  FMarginTop := V;
end;

procedure TplBitmapGrid.SetMarginBottom(V: integer);
begin
  if V < 0 then
    V := 0;
  FMarginBottom := V;
end;

procedure TplBitmapGrid.SetMarginLeft(V: integer);
begin
  if V < 0 then
    V := 0;
  FMarginLeft := V;
end;

procedure TplBitmapGrid.SetMarginRight(V: integer);
begin
  if V < 0 then
    V := 0;
  FMarginRight := V;
end;

procedure TplBitmapGrid.SetXGridRange(V: integer);
begin
  if V < 1 then
    V := 1;
  FXGridRange := V;
end;

procedure TplBitmapGrid.SetYGridRange(V: integer);
begin
  if V < 1 then
    V := 1;
  FYGridRange := V;
end;

procedure TplBitmapGrid.SetXLabelRange(V: integer);
begin
  if V < 10 then
    V := 10;
  FXLabelRange := V;
end;

procedure TplBitmapGrid.SetYLabelRange(V: integer);
begin
  if V < 10 then
    V := 10;
  FYLabelRange := V;
end;

function TplBitmapGrid.GetXPoint(X: extended): integer;
begin
  Result := Round((X - FXMin) * XCoeficient) + FMarginLeft;
end;

function TplBitmapGrid.GetYPoint(Y: extended): integer;
begin
  Result := Height - FMarginBottom - Round((Y - FYMin) * YCoeficient);
end;

procedure TplBitmapGrid.PaintGrid;
begin
  Bitmap.Width := Width;
  Bitmap.Height := Height;
  PaintAutoGrid(Self, Bitmap.Canvas, Width, Height,
    FMarginTop, FMarginBottom, FMarginLeft, FMarginRight,
    FXGridRange, FYGridRange, FXLabelRange, FYLabelRange,
    FAGridMode, FGridColor, FXMin, FXMax, FYMin, FYMax,
    FXInterlabelGrid, FYInterlabelGrid, FXInteger, FYInteger,
    XCoeficient, YCoeficient, FColor, FFont, OnXLabel, OnYLabel);
end;

//======================= Utils ===============================================


function GetLowerBound(Value: extended; var Step: integer): extended;
var
  CurUnit, Sweep: extended;
begin
  Sweep := Abs(Value / SweepCoefficient);
  Result := Value;
  CurUnit := 1;
  if Result - Sweep > CurUnit then
    while True do
    begin
      if Result + Sweep < CurUnit * 2 then
      begin
        Result := CurUnit;
        Step := 0;
        Exit;
      end;
      if Result + Sweep < CurUnit * 5 then
      begin
        if Step > 1 then
        begin
          Result := CurUnit * 2;
          Step := 2;
        end
        else
        begin
          Result := CurUnit;
          Step := 0;
        end;
        Exit;
      end;
      if Result + Sweep < CurUnit * 10 then
      begin
        if Step > 0 then
        begin
          Result := CurUnit * 5;
          Step := 1;
        end
        else
        begin
          Result := CurUnit;
          Step := 0;
        end;
        Exit;
      end;
      CurUnit := CurUnit * 10;
    end
  else
    while True do
    begin
      if Step > 0 then
        if Result + Sweep >= CurUnit / 2 then
        begin
          Result := CurUnit / 2;
          Step := 1;
          Exit;
        end;
      if Step > 1 then
        if Result + Sweep >= CurUnit / 5 then
        begin
          Result := CurUnit / 5;
          Step := 2;
          Exit;
        end;
      if Result + Sweep >= CurUnit / 10 then
      begin
        Result := CurUnit / 10;
        Step := 0;
        Exit;
      end;
      CurUnit := CurUnit / 10;
    end;
end;

function GetUpperBound(Value: extended; var Step: integer): extended;
var
  CurUnit, Sweep: extended;
begin
  Sweep := Abs(Value / SweepCoefficient);
  Result := Value;
  CurUnit := 1;
  if Result - Sweep > CurUnit then
    while True do
    begin
      if Step > 1 then
        if Result - Sweep <= CurUnit * 2 then
        begin
          Result := CurUnit * 2;
          Step := 2;
          Exit;
        end;
      if Step > 0 then
        if Result - Sweep <= CurUnit * 5 then
        begin
          Result := CurUnit * 5;
          Step := 1;
          Exit;
        end;
      if Result - Sweep <= CurUnit * 10 then
      begin
        Result := CurUnit * 10;
        Step := 0;
        Exit;
      end;
      CurUnit := CurUnit * 10;
    end
  else
    while True do
    begin
      if Result - Sweep > CurUnit / 2 then
      begin
        Result := CurUnit;
        Step := 0;
        Exit;
      end;
      if Result - Sweep > CurUnit / 5 then
      begin
        if Step > 0 then
        begin
          Result := CurUnit / 2;
          Step := 1;
        end
        else
        begin
          Result := CurUnit;
          Step := 0;
        end;
        Exit;
      end;
      if Result - Sweep > CurUnit / 10 then
      begin
        if Step > 1 then
        begin
          Result := CurUnit / 5;
          Step := 2;
        end
        else
        if Step > 0 then
        begin
          Result := CurUnit / 2;
          Step := 1;
        end
        else
        begin
          Result := CurUnit;
          Step := 0;
        end;
        Exit;
      end;
      CurUnit := CurUnit / 10;
    end;
end;

function GetBound(Value: extended; var Step: integer; Lower: boolean): extended;
var
  Sweep: extended;
begin
  Sweep := Abs(Value / SweepCoefficient);
  if Value + Sweep < 0 then
    if Lower then
      Result := -1 * GetUpperBound(Abs(Value), Step)
    else
      Result := -1 * GetLowerBound(Abs(Value), Step)
  else if Lower then
    Result := GetLowerBound(Value, Step)
  else
    Result := GetUpperBound(Value, Step);
end;

function GetLowerMark(Value, Delta: extended): extended;
var
  Sweep: extended;
begin
  Sweep := Abs(Delta / SweepCoefficient);
  Result := 0;
  if Value + Sweep >= 0 then
    while True do
    begin
      if Result + Delta - Sweep > Value then
        Exit;
      Result := Result + Delta;
    end
  else
    while True do
    begin
      if Result - Sweep <= Value then
        Exit;
      Result := Result - Delta;
    end;
end;

function GetUpperMark(Value, Delta: extended): extended;
var
  Sweep: extended;
begin
  Sweep := Abs(Delta / SweepCoefficient);
  Result := 0;
  if Value + Sweep >= 0 then
    while True do
    begin
      if Result + Sweep >= Value then
        Exit;
      Result := Result + Delta;
    end
  else
    while True do
    begin
      if Result - Delta + Sweep < Value then
        Exit;
      Result := Result - Delta;
    end;
end;

function GetCoefficient(OnlyInteger: boolean; Min, Max, MinDis: integer; var MinVal, MaxVal, Delta: extended;
  var Step: integer): extended;
begin
  Result := (Max - Min) / (MaxVal - MinVal);
  Delta := GetBound(Abs(MinDis * (MaxVal - MinVal) / (Max - Min)), Step, False);
  if OnlyInteger then
    if Delta < 1 then
      Delta := 1;
end;

function GetNumStrSize(NumStr: string; FontSize: integer): integer;
var
  i, Sum: integer;
begin
  Sum := 0;
  for i := 1 to Length(NumStr) do
    if (NumStr[i] = '.') or (NumStr[i] = ',') or (NumStr[i] = '-') or (NumStr[i] = '+') then
      Sum := Sum + 3
    else
      Sum := Sum + 6;
  Result := Round(Sum * FontSize / 8);
end;

procedure PaintAutoGrid(Sender: TObject; Canvas: TCanvas;
  Width, Height, MarginTop, MarginBottom, MarginLeft, MarginRight, XGridRange, YGridRange, XLabelRange, YLabelRange: integer;
  GridMode: TAGridMode; GridColor: TColor; XMin, XMax, YMin, YMax: extended;
  XInterlabelGrid, YInterlabelGrid, XInteger, YInteger: boolean; var XCoeficient, YCoeficient: extended;
  Color: TColor; Font: TFont; OnXLabel, OnYLabel: TAGLabelEvent);
var
  D, MinVal, MinVal1, Sweep: extended;
  i, NumPart, Step, CurPoint, Num: integer;
  LabelStr: string;
begin
  Canvas.Pen.Style := psSolid;
  Canvas.Brush.Style := bsClear;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(Rect(0, 0, Width, Height));
  Canvas.Font := Font;
  Canvas.Pen.Color := GridColor;
  Canvas.Pen.Width := 1;
  XCoeficient := 0;
  YCoeficient := 0;
  if (MarginLeft + MarginRight < Width) and (MarginTop + MarginBottom < Height) then
    with Canvas do
    begin
      if XMax > XMin then
      begin
        Step := 2;
        XCoeficient := GetCoefficient(XInteger, MarginLeft, Width - MarginRight, XLabelRange, XMin, XMax, D, Step);
        MinVal1 := GetUpperMark(XMin, D);
        Sweep := Abs(D / SweepCoefficient);
        NumPart := 1;
        case Step of
          0: if D * XCoeficient / 10 + Sweep >= XGridRange then
              NumPart := 10
            else if D * XCoeficient / 5 + Sweep >= XGridRange then
              NumPart := 5
            else if D * XCoeficient / 2 + Sweep >= XGridRange then
              NumPart := 2;
          1: if D * XCoeficient / 5 + Sweep >= XGridRange then
              NumPart := 5;
          2: if D * XCoeficient / 2 + Sweep >= XGridRange then
              NumPart := 2;
        end;
        D := D / NumPart;
        Sweep := Abs(D / SweepCoefficient);
        MinVal := GetUpperMark(XMin, D);
        i := NumPart - Round((MinVal1 - MinVal) / D);
        Num := 0;
        while MinVal - Sweep <= XMax do
        begin
          CurPoint := Round(Abs(MinVal - XMin) * XCoeficient) + MarginLeft;
          if XInterlabelGrid or (i mod NumPart = 0) then
          begin
            if (GridMode = mVer) or (GridMode = mBoth) then
            begin
              MoveTo(CurPoint, Height - MarginBottom);
              LineTo(CurPoint, MarginTop);
            end;
            MoveTo(CurPoint, Height - MarginBottom + 2);
            LineTo(CurPoint, Height - MarginBottom);
          end;
          if i mod NumPart = 0 then
          begin
            Inc(Num);
            MoveTo(CurPoint, Height - MarginBottom);
            LineTo(CurPoint, Height - MarginBottom + 5);
            LabelStr := FloatToStr(StrToFloat(FloatToStrF(MinVal, ffFixed, 14, 14)));
            if Assigned(OnXLabel) then
              OnXLabel(Sender, LabelStr, Num, CurPoint, MinVal);
            TextOut(CurPoint - GetNumStrSize(LabelStr, Font.Size) div 2, Height - MarginBottom + 7, LabelStr);
          end;
          Inc(i);
          MinVal := MinVal + D;
        end;
      end;
      if YMax > YMin then
      begin
        Step := 2;
        YCoeficient := GetCoefficient(YInteger, MarginTop, Height - MarginBottom, YLabelRange, YMin, YMax, D, Step);
        MinVal1 := GetUpperMark(YMin, D);
        Sweep := Abs(D / SweepCoefficient);
        NumPart := 1;
        case Step of
          0: if D * YCoeficient / 10 + Sweep >= YGridRange then
              NumPart := 10
            else if D * YCoeficient / 5 + Sweep >= YGridRange then
              NumPart := 5
            else if D * YCoeficient / 2 + Sweep >= YGridRange then
              NumPart := 2;
          1: if D * YCoeficient / 5 + Sweep >= YGridRange then
              NumPart := 5;
          2: if D * YCoeficient / 2 + Sweep >= YGridRange then
              NumPart := 2;
        end;
        D := D / NumPart;
        Sweep := Abs(D / SweepCoefficient);
        MinVal := GetUpperMark(YMin, D);
        i := NumPart - Round((MinVal1 - MinVal) / D);
        Num := 0;
        while MinVal - Sweep <= YMax do
        begin
          CurPoint := Height - MarginBottom - Round(Abs(MinVal - YMin) * YCoeficient);
          if YInterlabelGrid or (i mod NumPart = 0) then
          begin
            if (GridMode = mHor) or (GridMode = mBoth) then
            begin
              MoveTo(MarginLeft, CurPoint);
              LineTo(Width - MarginRight, CurPoint);
            end;
            MoveTo(MarginLeft - 2, CurPoint);
            LineTo(MarginLeft, CurPoint);
          end;
          if i mod NumPart = 0 then
          begin
            Inc(Num);
            MoveTo(MarginLeft, CurPoint);
            LineTo(MarginLeft - 5, CurPoint);
            LabelStr := FloatToStr(StrToFloat(FloatToStrF(MinVal, ffFixed, 14, 14)));
            if Assigned(OnYLabel) then
              OnYLabel(Sender, LabelStr, Num, CurPoint, MinVal);
            TextOut(MarginLeft - GetNumStrSize(LabelStr, Font.Size) - 7,
              CurPoint - Font.Size * 3 div 4, LabelStr);
          end;
          Inc(i);
          MinVal := MinVal + D;
        end;
      end;
    end;
end;

end.
