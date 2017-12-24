
{**********************************************************************
 Package pl_ExControls.pkg
 From PilotLogic Software House(http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit TplSimpleChartUnit;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages,
  Messages, Classes, Graphics, Forms, Controls, ExtCtrls, SysUtils,
  BSplines, Math, ClipBrd;

const
  UndefinedValueChart: double = MaxDouble;

type
  TplSimpleChart = class;

  TSeriesKind = (skLine, skSmooth, skBar);

  TplSimpleSeries = class(TObject)
  private
    Top: integer;
    Values: TList;
    Chart: TplSimpleChart;
    FCaption: string;
    Spline: TBSpline;
    FKind: TSeriesKind;
    procedure SetCaption(const Value: string);
    function GetMaxXValue: double;
    function GetMinXValue: double;
    function GetMinYValue: double;
    function GetMaxYValue: double;
    procedure SetKind(const Value: TSeriesKind);
  protected
    procedure InternalClear;
  public
    constructor Create(AChart: TplSimpleChart; AKind: TSeriesKind);
    destructor Destroy; override;
    function  AddXY(AX, AY: double; AHint: string = ''): integer;
    procedure Remove(Index: integer);
    procedure Clear;
    property Caption: string read FCaption write SetCaption;
    property Kind: TSeriesKind read FKind write SetKind;
  end;

  TValueTranslator = record
    MinValue: double;
    Scale: double;
    Base: integer;
  end;

  TMarkerProc = procedure(ACanvas: TCanvas; X, Y, Size: integer);

  TplSimpleChart = class(TCustomPanel)
  private
    Brushes: array [0..15] of TBitmap;
    Temp: TStringList;
    MarkSize: integer;
    Marker: TMarkerProc;
    BarCount: integer;
    BarWidth: integer;
    DestWidth, DestHeight: integer;
    YZero: integer;
    ChartEmpty: boolean;
    List: TList;
    XAxis: TList;
    YAxis: TList;
    FShowLegend: boolean;
    FShowTitle: boolean;
    FTitle: string;
    FTitleFont: TFont;
    FNormalFont: TFont;
    FUpdating: boolean;
    RcChart, RcLegend, RcTitle: TRect;
    FXTranslate: TValueTranslator;
    FYTranslate: TValueTranslator;
    FAxisXOnePerValue: boolean;
    FAxisYTitle: string;
    FAxisXTitle: string;
    FShowYGrid: boolean;
    FShowXGrid: boolean;
    FAxisYScale: single;
    FAxisXScale: single;
    FMonochrome: boolean;
    FSoftColors: boolean;
    procedure InternalClear;
    procedure InternalPaint(ACanvas: TCanvas);
    procedure Calculate(AWidth, AHeight: integer);
    procedure DoPaint;
    procedure SetShowLegend(const Value: boolean);
    procedure SetShowTitle(const Value: boolean);
    procedure SetTitle(const Value: string);
    procedure SetTitleFont(const Value: TFont);
    procedure TitleFontChanged(Sender: TObject);
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
    procedure DrawLegend(ACanvas: TCanvas);
    procedure DrawTitle(ACanvas: TCanvas);
    procedure SetAxisXTitle(const Value: string);
    procedure SetAxisYTitle(const Value: string);
    procedure BuildYAxis;
    procedure DrawYAxis(ACanvas: TCanvas);
    procedure DrawXAxis(ACanvas: TCanvas);
    procedure DrawChart(ACanvas: TCanvas);
    procedure BuildXAxis;
    procedure ClearAxis;
    procedure AdjustYAxis;
    procedure SetAxisXOnePerValue(const Value: boolean);
    procedure SetShowXGrid(const Value: boolean);
    procedure SetShowYGrid(const Value: boolean);
    procedure CalculateSeries;
    procedure DrawSeries(ACanvas: TCanvas; Index: integer);
    procedure AutoColors(ACanvas: TCanvas; Index: integer; IsBar: boolean);
    procedure SetAxisXScale(const Value: single);
    procedure SetAxisYScale(const Value: single);
    procedure SetMonochrome(const Value: boolean);
    procedure SetSoftColors(const Value: boolean);
    function GetLabel(Value: double): string;
    function GetSeries(Index: integer): TplSimpleSeries;
    function GetSeriesCount: integer;
  protected
    procedure Paint; override;
    procedure Changed;
    procedure ChartToClient(const AX, AY: double; var X, Y: integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    function  ClientToChart(const X, Y: integer; var AX, AY: double): boolean;
    function  AddSeries(AKind: TSeriesKind): TplSimpleSeries;
    procedure RemoveSeries(ASeries: TplSimpleSeries);
    procedure Clear;
    property Series[Index: integer]: TplSimpleSeries read GetSeries;
    property SeriesCount: integer read GetSeriesCount;
    //function CreateMetafile: TMetafile;
    procedure CopyToClipboard;
  published
    property ShowLegend: boolean read FShowLegend write SetShowLegend;
    property ShowTitle: boolean read FShowTitle write SetShowTitle;
    property ShowXGrid: boolean read FShowXGrid write SetShowXGrid;
    property ShowYGrid: boolean read FShowYGrid write SetShowYGrid;
    property Title: string read FTitle write SetTitle;
    property TitleFont: TFont read FTitleFont write SetTitleFont;
    property AxisXTitle: string read FAxisXTitle write SetAxisXTitle;
    property AxisYTitle: string read FAxisYTitle write SetAxisYTitle;
    property AxisXOnePerValue: boolean read FAxisXOnePerValue write SetAxisXOnePerValue;
    property AxisXScale: single read FAxisXScale write SetAxisXScale;
    property AxisYScale: single read FAxisYScale write SetAxisYScale;
    property Monochrome: boolean read FMonochrome write SetMonochrome;
    property SoftColors: boolean read FSoftColors write SetSoftColors;
    property BorderStyle;
    property BevelInner;
    property BevelOuter;
    property Align;
    property Anchors;
    property PopupMenu;

    property OnClick;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnGetDockCaption;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

procedure CalculateAxis(AMin, AMax: double; Count: integer; out Delta, Lowest: double);

implementation

{$R TplSimpleChart.res}

const
  OUTER_MARGIN = 20;
  INNER_MARGIN = 10;
  SMALL_MARGIN = 2;
  LEGEND_ITEM = 20;
  AXIS_DEFSIZE = 50;

  Formatter = '0.##';

  Colors1: array [0..13] of TColor = (
    clRed, clBlue, clGreen, clFuchsia, clNavy, clMaroon, clBlack, clOlive,
    clPurple, clTeal, clGray, clLime, clYellow, clAqua
    );
  Colors2: array [0..13] of TColor = (
    $0066C2FF, $005AFADA, $00F4C84D, $00B54DF4, $00669FFF, $00F44D5A,
    $0066E0FF, $0066FFFF, $00F44DAE, $006863FE, $004DF474, $00F4934D,
    clSilver, clGray
    );

type
  PXYInfo = ^TXYInfo;

  TXYInfo = record
    X, Y: double;
    Px, Py: integer;
    Rc: TRect;
    Hint: string;
  end;

  PAxisInfo = ^TAxisInfo;

  TAxisInfo = record
    Value: double;
    Px, Py: integer;
    Caption: string;
  end;

  PDoubleList = ^TDoubleList;
  TDoubleList = array [0..0] of double;



procedure MarkerRectangle(ACanvas: TCanvas; X, Y, Size: integer);
begin
  ACanvas.Rectangle(X - Size, Y - Size, X + Size, Y + Size);
end;

procedure MarkerCircle(ACanvas: TCanvas; X, Y, Size: integer);
begin
  ACanvas.Ellipse(X - Size, Y - Size, X + Size, Y + Size);
end;

procedure MarkerTriangle1(ACanvas: TCanvas; X, Y, Size: integer);
begin
  ACanvas.Polygon([Point(x, y - Size), Point(x + Size, y + Size), Point(x - Size, y + Size)]);
end;

procedure MarkerTriangle2(ACanvas: TCanvas; X, Y, Size: integer);
begin
  ACanvas.Polygon([Point(x + Size, y - Size), Point(x - Size, y - Size), Point(x, y + Size)]);
end;

procedure MarkerDiamond(ACanvas: TCanvas; X, Y, Size: integer);
begin
  ACanvas.Polygon([Point(x, y - Size), Point(x + Size, y), Point(x, y + Size), Point(x - Size, y)]);
end;

const
  Markers: array [0..4] of TMarkerProc = (
    @MarkerRectangle, @MarkerCircle, @MarkerTriangle1, @MarkerTriangle2, @MarkerDiamond);

procedure QuickSortDouble(SortList: PDoubleList; L, R: integer);
var
  I, J: integer;
  P, T: double;
begin
  repeat
    I := L;
    J := R;
    P := SortList^[(L + R) shr 1];
    repeat
      while (SortList^[I] < P) do
        Inc(I);
      while (SortList^[J] > P) do
        Dec(J);
      if I <= J then
      begin
        T := SortList^[I];
        SortList^[I] := SortList^[J];
        SortList^[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSortDouble(SortList, L, J);
    L := I;
  until I >= R;
end;

function GetMan10(Value: double): double;
var
  Str: string;
begin
  Str := UpperCase(Format('%E', [Value]));
  Result := StrToFloat('1E' + Copy(Str, Pos('E', Str) + 1, Length(Str)));
end;

procedure CalculateAxis(AMin, AMax: double; Count: integer; out Delta, Lowest: double);
label
  Retry;
var
  c, n, m10: double;
begin
  c := Max(2, Count - 1);
  n := (Abs(AMax - AMin) / c);
  m10 := GetMan10(n);
  Delta := 0;
  while (Delta < n) do
    Delta := Delta + (0.5 * m10);
  if (Delta = 0) then
  begin
    Delta := 1;
    Lowest := AMin - (Count div 2);
    Exit;
  end;
  Retry:
    Lowest := Trunc(AMin / Delta) * Delta;
  if (Lowest > AMin) then
    Lowest := Lowest - Delta;
  if ((Lowest + (Delta * c)) < AMax) then
  begin
    Delta := Delta + (0.5 * m10);
    goto Retry;
  end;
end;

procedure RotTextOut(ACanvas: TCanvas; x, y, Angle: integer; Txt: string);
var
  RotFont, OldFont: HGDIOBJ;        // ct9999
  FBold, FItalic, FUnderline, FStrikeOut: integer;
begin
  if (Txt = '') then
    Exit;

  SetBkMode(ACanvas.Handle, TRANSPARENT);

  if (fsItalic in ACanvas.Font.Style) then
    FItalic := 1
  else
    FItalic := 0;

  if (fsUnderline in ACanvas.Font.Style) then
    FUnderline := 1
  else
    FUnderline := 0;

  if (fsStrikeOut in ACanvas.Font.Style) then
    FStrikeOut := 1
  else
    FStrikeOut := 0;

  if (fsBold in ACanvas.Font.Style) then
    FBold := FW_BOLD
  else
    FBold := FW_NORMAL;

  RotFont := CreateFont(ACanvas.Font.Height, 0, Angle * 10, 0, FBold, FItalic, FUnderline, FStrikeOut, 1, 4,
    $10, ANTIALIASED_QUALITY, 4, PChar(ACanvas.Font.Name));

  OldFont := SelectObject(ACanvas.Handle, RotFont);
  TextOut(ACanvas.Handle, x, y, PChar(Txt), Length(Txt));
  SelectObject(ACanvas.Handle, OldFont);

  DeleteObject(RotFont);
end;

//=========================== TplSimpleSeries ===================================

constructor TplSimpleSeries.Create(AChart: TplSimpleChart; AKind: TSeriesKind);
begin
  inherited Create;
  Chart := AChart;
  Values := TList.Create;
  FCaption := 'Series';
  Spline := TBSpline.Create;
  FKind := AKind;

end;

destructor TplSimpleSeries.Destroy;
begin
  Spline.Free;
  InternalClear;
  Values.Free;
  inherited Destroy;
end;

procedure TplSimpleSeries.InternalClear;
var
  x: integer;
begin
  for x := 0 to Values.Count - 1 do
    Dispose(PXYInfo(Values[x]));
  Values.Clear;
end;

procedure TplSimpleSeries.Clear;
begin
  InternalClear;
  Chart.Changed;
end;

function TplSimpleSeries.AddXY(AX, AY: double; AHint: string): integer;
var
  Info: PXYInfo;
begin
  Info := New(PXYInfo);
  Info^.X := AX;
  Info^.Y := AY;
  Info^.Px := 0;
  Info^.Py := 0;
  Info^.Rc := Rect(0, 0, 0, 0);
  Info^.Hint := AHint;
  Result := Values.Add(Info);
  Chart.Changed;
end;

procedure TplSimpleSeries.Remove(Index: integer);
var
  P: PXYInfo;
begin
  if (Index >= 0) and (Index < Values.Count) then
  begin
    P := Values[Index];
    Values.Remove(P);
    Dispose(P);
    Chart.Changed;
  end;
end;

function TplSimpleSeries.GetMaxXValue: double;
var
  x: integer;
begin
  Result := -MaxDouble;
  for x := 0 to Values.Count - 1 do
    Result := Max(Result, PXYInfo(Values[x])^.X);
end;

function TplSimpleSeries.GetMinXValue: double;
var
  x: integer;
begin
  Result := MaxDouble;
  for x := 0 to Values.Count - 1 do
    Result := Min(Result, PXYInfo(Values[x])^.X);
end;

function TplSimpleSeries.GetMaxYValue: double;
var
  x: integer;
begin
  Result := -MaxDouble;
  for x := 0 to Values.Count - 1 do
    Result := Max(Result, PXYInfo(Values[x])^.Y);
end;

function TplSimpleSeries.GetMinYValue: double;
var
  x: integer;
begin
  Result := MaxDouble;
  for x := 0 to Values.Count - 1 do
    Result := Min(Result, PXYInfo(Values[x])^.Y);
end;

procedure TplSimpleSeries.SetCaption(const Value: string);
begin
  if (FCaption <> Value) then
  begin
    FCaption := Value;
    Chart.Changed;
  end;
end;

procedure TplSimpleSeries.SetKind(const Value: TSeriesKind);
begin
  if (FKind <> Value) then
  begin
    FKind := Value;
    Chart.Changed;
  end;
end;

//============== TplSimpleChart ========================================

constructor TplSimpleChart.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csNeedsBorderPaint];
  ParentColor := False;
  ParentFont := False;
  Temp := TStringList.Create;
  BevelOuter := bvNone;
  BevelInner := bvNone;
  BorderStyle := bsSingle;
  List := TList.Create;
  FShowLegend := True;
  FShowTitle := True;
  FShowXGrid := True;
  FShowYGrid := True;
  FMonochrome := False;
  FTitle := 'Chart Title';
  FTitleFont := TFont.Create;
  FTitleFont.Name := 'Arial';
  FTitleFont.Size := 14;
  FTitleFont.Style := [];
  FTitleFont.OnChange := TitleFontChanged;
  FNormalFont := TFont.Create;
  FNormalFont.Name := 'Arial';
  FAxisXTitle := 'X Axis';
  FAxisYTitle := 'Y Axis';
  FAxisXScale := 1;
  FAxisYScale := 1;
  XAxis := TList.Create;
  YAxis := TList.Create;
  FUpdating := False;
  Color := clWhite;
  Width := 300;
  Height := 200;
end;

destructor TplSimpleChart.Destroy;
var
  x: integer;
begin
  for x := 0 to 15 do
  begin
    if Assigned(Brushes[x]) then
      Brushes[x].Free;
  end;
  InternalClear;
  List.Free;
  FTitleFont.Free;
  FNormalFont.Free;
  XAxis.Free;
  YAxis.Free;
  Temp.Free;
  inherited Destroy;
end;

procedure TplSimpleChart.InternalClear;
var
  x: integer;
begin
  for x := 0 to List.Count - 1 do
    TplSimpleSeries(List[x]).Free;

  ClearAxis;
  List.Clear;
end;

procedure TplSimpleChart.Paint;
begin
  if HandleAllocated then
    DoPaint;
end;

procedure TplSimpleChart.DoPaint;
begin
  InternalPaint(Canvas);
end;

procedure TplSimpleChart.SetMonochrome(const Value: boolean);
begin
  if (FMonochrome = Value) then exit;

  FMonochrome := Value;
  Changed;
  Invalidate;
end;

procedure TplSimpleChart.SetSoftColors(const Value: boolean);
begin
  if (FSoftColors = Value) then
    exit;

  FSoftColors := Value;
  Changed;
end;

procedure TplSimpleChart.SetShowLegend(const Value: boolean);
begin
  if (FShowLegend = Value) then
    exit;

  FShowLegend := Value;
  Changed;
end;

procedure TplSimpleChart.SetAxisXOnePerValue(const Value: boolean);
begin
  if (FAxisXOnePerValue = Value) then
    exit;

  FAxisXOnePerValue := Value;
  Changed;
end;

procedure TplSimpleChart.SetShowTitle(const Value: boolean);
begin
  if (FShowTitle = Value) then
    exit;

  FShowTitle := Value;
  Changed;
end;

procedure TplSimpleChart.SetTitle(const Value: string);
begin
  if (FTitle = Value) then
    exit;

  FTitle := Value;
  Changed;
end;

procedure TplSimpleChart.SetTitleFont(const Value: TFont);
begin
  FTitleFont.Assign(Value);
end;

procedure TplSimpleChart.SetAxisXTitle(const Value: string);
begin
  if (FAxisXTitle = Value) then
    exit;

  FAxisXTitle := Value;
  Changed;
end;

procedure TplSimpleChart.SetAxisYTitle(const Value: string);
begin
  if (FAxisYTitle = Value) then
    exit;

  FAxisYTitle := Value;
  Changed;
end;

procedure TplSimpleChart.SetAxisXScale(const Value: single);
begin
  if (FAxisXScale = Value) then
    exit;

  FAxisXScale := Value;
  if (FAxisXScale = 0) then
    FAxisXScale := 1;
  Changed;
end;

procedure TplSimpleChart.SetAxisYScale(const Value: single);
begin
  if (FAxisYScale = Value) then
    exit;

  FAxisYScale := Value;
  if (FAxisYScale = 0) then
    FAxisYScale := 1;
  Changed;
end;

procedure TplSimpleChart.SetShowXGrid(const Value: boolean);
begin
  if (FShowXGrid = Value) then
    exit;

  FShowXGrid := Value;
  DoPaint;
end;

procedure TplSimpleChart.SetShowYGrid(const Value: boolean);
begin
  if (FShowYGrid = Value) then
    exit;

  FShowYGrid := Value;
  DoPaint;
end;

procedure TplSimpleChart.TitleFontChanged(Sender: TObject);
begin
  Changed;
end;

procedure TplSimpleChart.Changed;
begin
  if FUpdating then
    exit;
  Calculate(ClientWidth, ClientHeight);
  DoPaint;
end;

procedure TplSimpleChart.BeginUpdate;
begin
  FUpdating := True;
end;

procedure TplSimpleChart.EndUpdate;
begin
  FUpdating := False;
  Changed;
end;

procedure TplSimpleChart.WMSize(var Message: TLMSize);
begin
  inherited;
  Changed;
end;

procedure TplSimpleChart.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
  Message.Result := 1;
end;

function TplSimpleChart.GetSeries(Index: integer): TplSimpleSeries;
begin
  Result := TplSimpleSeries(List[Index]);
end;

function TplSimpleChart.AddSeries(AKind: TSeriesKind): TplSimpleSeries;
begin
  Result := TplSimpleSeries.Create(Self, AKind);
  List.Add(Result);
end;

procedure TplSimpleChart.Clear;
begin
  InternalClear;
  Changed;
end;

procedure TplSimpleChart.RemoveSeries(ASeries: TplSimpleSeries);
begin
  if Assigned(ASeries) then
  begin
    List.Remove(ASeries);
    ASeries.Free;
    Changed;
  end;
end;

function TplSimpleChart.GetSeriesCount: integer;
begin
  Result := List.Count;
end;

procedure TplSimpleChart.DrawLegend(ACanvas: TCanvas);
var
  x, y, l, t: integer;
  th, g: integer;
begin
  with ACanvas do
  begin
    Pen.Width := 1;
    Pen.Style := psSolid;
    Font.Assign(FNormalFont);
    g := TextHeight('Ag');
    th := (LEGEND_ITEM - g) div 2;
    Brush.Style := bsSolid;
    Brush.Color := clBlack;
    FillRect(Rect(RcLegend.Right, RcLegend.Top + 3, RcLegend.Right + 3, RcLegend.Bottom + 3));
    FillRect(Rect(RcLegend.Left + 3, RcLegend.Bottom, RcLegend.Right + 3, RcLegend.Bottom + 3));
    Brush.Style := bsClear;
    Rectangle(RcLegend);
    Brush.Style := bsClear;
    l := RcLegend.Left + INNER_MARGIN + LEGEND_ITEM + SMALL_MARGIN;
    for x := 0 to List.Count - 1 do
    begin
      Temp.Text := Trim(TplSimpleSeries(List[x]).FCaption);
      t := RcLegend.Top + TplSimpleSeries(List[x]).Top;
      for y := 0 to Temp.Count - 1 do
      begin
        TextOut(l, t + th, Trim(Temp[y]));
        Inc(t, g);
      end;
    end;
  end;
end;

procedure TplSimpleChart.DrawTitle(ACanvas: TCanvas);
begin
  with ACanvas do
  begin
    Brush.Style := bsClear;
    Font.Assign(FTitleFont);
    DrawText(Handle, PChar(FTitle), Length(FTitle), RcTitle, DT_CENTER or DT_VCENTER or DT_WORDBREAK);
  end;
end;

procedure TplSimpleChart.InternalPaint(ACanvas: TCanvas);
begin
  with ACanvas do
  begin
    Pen.Color := clBlack;
    Pen.Width := 1;
    Brush.Style := bsSolid;
    Brush.Color := Color;
    FillRect(Rect(0, 0, DestWidth, DestHeight));
  end;
  if FShowLegend and (List.Count > 0) then
    DrawLegend(ACanvas);
  if FShowTitle and (FTitle <> '') then
    DrawTitle(ACanvas);
  DrawXAxis(ACanvas);
  DrawYAxis(ACanvas);
  DrawChart(ACanvas);
end;

procedure TplSimpleChart.Calculate(AWidth, AHeight: integer);
var
  x, w, h, y, g: integer;
  Titled: boolean;
begin

  ClearAxis;

  DestWidth := AWidth;
  DestHeight := AHeight;
  RcChart := Rect(0, 0, DestWidth, DestHeight);
  MarkSize := Max(1, Round(DestWidth * 0.004));

  InflateRect(RcChart, -OUTER_MARGIN, -OUTER_MARGIN);

  Titled := False;
  if FShowTitle and (FTitle <> '') then
  begin
    Canvas.Font.Assign(TitleFont);
    w := Canvas.TextHeight(FTitle);
    RcTitle := Rect(RcChart.Left, RcChart.Top, RcChart.Right, RcChart.Left + w);
    DrawText(Canvas.Handle, PChar(FTitle), Length(FTitle), RcTitle,
      DT_CENTER or DT_VCENTER or DT_WORDBREAK or DT_CALCRECT);
    RcChart.Top := RcTitle.Bottom + INNER_MARGIN;
    Titled := True;
  end
  else
    SetRectEmpty(RcTitle);

  Canvas.Font.Assign(FNormalFont);
  h := Canvas.TextHeight('Ag');
  RcChart.Bottom := RcChart.Bottom - (2 * h) - INNER_MARGIN - (2 * SMALL_MARGIN);

  BuildYAxis;
  w := 0;
  for x := 0 to YAxis.Count - 1 do
    w := Max(w, Canvas.TextWidth(PAxisInfo(YAxis[x])^.Caption));
  RcChart.Left := RcChart.Left + h + INNER_MARGIN + w + (2 * SMALL_MARGIN);
  RcTitle.Left := RcChart.Left;
  RcTitle.Right := RcChart.Right;
  AdjustYAxis;

  if FShowLegend and (List.Count > 0) then
  begin
    Canvas.Font.Assign(FNormalFont);
    w := 0;
    h := INNER_MARGIN;
    g := Canvas.TextHeight('Ag');
    for x := 0 to List.Count - 1 do
    begin
      TplSimpleSeries(List[x]).Top := h;
      Temp.Text := Trim(TplSimpleSeries(List[x]).FCaption);
      for y := 0 to Temp.Count - 1 do
        w := Max(w, Canvas.TextWidth(Trim(Temp[y])));
      h := h + Max(LEGEND_ITEM, Temp.Count * g);
      if (x <> List.Count - 1) then
        h := h + SMALL_MARGIN;
    end;
    w := w + (2 * INNER_MARGIN) + LEGEND_ITEM + SMALL_MARGIN;
    h := h + INNER_MARGIN;
    RcLegend := Rect(RcChart.Right - w, RcChart.Top, RcChart.Right, RcChart.Top + h);
    RcChart.Right := RcLegend.Left - (2 * INNER_MARGIN);
    if Titled then
      RcTitle.Right := RcChart.Right;
  end
  else
    SetRectEmpty(RcLegend);

  BuildXAxis;

  CalculateSeries;

end;

procedure TplSimpleChart.ClearAxis;
var
  x: integer;
begin
  for x := 0 to XAxis.Count - 1 do
    Dispose(PAxisInfo(XAxis[x]));

  XAxis.Clear;

  for x := 0 to YAxis.Count - 1 do
    Dispose(PAxisInfo(YAxis[x]));

  YAxis.Clear;
end;

function TplSimpleChart.GetLabel(Value: double): string;
begin
  if (Value = UndefinedValueChart) then
    Result := '~'
  else
    Result := FormatFloat(Formatter, Value);
end;

procedure TplSimpleChart.BuildXAxis;
var
  x, y, w: integer;
  mi, ma: double;
  Cnt, i, n: integer;
  Delta, Lowest, l: double;
  P: PAxisInfo;
  xTemp: PDoubleList;
  Vals: TList;
  Last: double;
  Scale: double;
  dx: integer;

begin

  if (List.Count = 0) or ChartEmpty then
    Exit;

  BarCount := 0;
  for x := 0 to List.Count - 1 do
  begin
    if (TplSimpleSeries(List[x]).FKind = skBar) then
      Inc(BarCount);
  end;
  if (BarCount > 0) then
    FAxisXOnePerValue := True;

  if FAxisXOnePerValue then
  begin
    w := RcChart.Right - RcChart.Left;
    Cnt := 0;
    for x := 0 to List.Count - 1 do
      Cnt := Cnt + Series[x].Values.Count;
    GetMem(xTemp, Cnt * SizeOf(double));
    i := 0;
    for x := 0 to List.Count - 1 do
    begin
      Vals := TplSimpleSeries(List[x]).Values;
      for y := 0 to Vals.Count - 1 do
      begin
        xTemp^[i] := PXYInfo(Vals[y])^.X;
        Inc(i);
      end;
    end;
    QuickSortDouble(xTemp, 0, Cnt - 1);
    n := 0;
    Last := MaxDouble;
    for x := 0 to Cnt - 1 do
    begin
      l := xTemp^[x];
      if (l = Last) then
        Continue;
      Inc(n);
      Last := l;
    end;
    if (BarCount > 0) then
    begin
      Scale := w / n;
      dx := Round(Scale / 2);
      BarWidth := Round(Scale);
    end
    else
    begin
      Scale := w / (n - 1);
      dx := 0;
    end;
    Last := MaxDouble;
    i := 0;
    for x := 0 to Cnt - 1 do
    begin
      l := xTemp^[x];
      if (l = Last) then
        Continue;
      P := New(PAxisInfo);
      P^.Value := l;
      P^.Py := RcChart.Bottom;
      P^.Px := RcChart.Left + dx + Round(i * Scale);
      P^.Caption := GetLabel(l / FAxisXScale);
      XAxis.Add(P);
      Last := l;
      Inc(i);
    end;
    FreeMem(xTemp);
  end
  else
  begin
    w := RcChart.Right - RcChart.Left;
    Cnt := (w div AXIS_DEFSIZE) + 1;
    mi := MaxDouble;
    ma := -MaxDouble;
    for x := 0 to List.Count - 1 do
    begin
      mi := Min(mi, Series[x].GetMinXValue);
      ma := Max(ma, Series[x].GetMaxXValue);
    end;
    CalculateAxis(mi, ma, Cnt, Delta, Lowest);
    Scale := w / (Delta * Max(1, Cnt - 1));
    for x := 0 to Cnt - 1 do
    begin
      l := x * Delta;
      P := New(PAxisInfo);
      P^.Py := RcChart.Bottom;
      P^.Px := RcChart.Left + Round(l * Scale);
      P^.Caption := GetLabel((Lowest + l) / FAxisXScale);
      XAxis.Add(P);
    end;
    FXTranslate.MinValue := Lowest;
    FXTranslate.Scale := Scale;
    FXTranslate.Base := RcChart.Left;
  end;

end;

procedure TplSimpleChart.BuildYAxis;
var
  x, w: integer;
  mi, ma: double;
  Cnt: integer;
  Delta, Lowest, t: double;
  P: PAxisInfo;
  Scale: double;
begin
  if (List.Count = 0) then
    Exit;
  w := RcChart.Bottom - RcChart.Top;
  Cnt := (w div AXIS_DEFSIZE) + 1;
  ChartEmpty := True;
  mi := MaxDouble;
  ma := -MaxDouble;
  for x := 0 to List.Count - 1 do
  begin
    if (Series[x].Values.Count > 0) then
    begin
      mi := Min(mi, Series[x].GetMinYValue);
      ma := Max(ma, Series[x].GetMaxYValue);
      ChartEmpty := False;
    end;
  end;
  if ChartEmpty then
    Exit;
  CalculateAxis(mi, ma, Cnt, Delta, Lowest);
  Scale := w / (Delta * Max(1, Cnt - 1));
  for x := 0 to Cnt - 1 do
  begin
    t := x * Delta;
    P := New(PAxisInfo);
    P^.Value := Lowest + t;
    P^.Py := Round(t * Scale);
    P^.Caption := GetLabel((Lowest + t) / FAxisYScale);
    YAxis.Add(P);
  end;
  FYTranslate.MinValue := Lowest;
  FYTranslate.Scale := Scale;
end;

procedure TplSimpleChart.AdjustYAxis;
var
  x: integer;
  P: PAxisInfo;
  l: integer;
begin
  l := RcChart.Left;
  YZero := -1;
  for x := 0 to YAxis.Count - 1 do
  begin
    P := PAxisInfo(YAxis[x]);
    P^.Px := l;
    P^.Py := RcChart.Bottom - P^.Py;
    if (P^.Value = 0) then
      YZero := P^.Py;
  end;
  if (YZero = -1) then
    YZero := RcChart.Bottom;
  FYTranslate.Base := RcChart.Bottom;
end;

procedure TplSimpleChart.DrawXAxis(ACanvas: TCanvas);
var
  l, t, w, x: integer;
  P: PAxisInfo;
  Str: string;
  Last: integer;
begin
  with ACanvas do
  begin
    Pen.Style := psSolid;
    Pen.Width := 3;
    MoveTo(RcChart.Left, RcChart.Bottom);
    LineTo(RcChart.Right, RcChart.Bottom);
    Font.Assign(FNormalFont);
    Font.Style := [fsBold];
    w := RcChart.Right - RcChart.Left;
    t := RcChart.Bottom + INNER_MARGIN + (2 * SMALL_MARGIN) + TextHeight('Ag');
    l := RcChart.Left + ((w - TextWidth(FAxisXTitle)) div 2);
    TextOut(l, t, FAxisXTitle);
    Font.Assign(FNormalFont);
    Pen.Color := clBlack;
    Pen.Width := 1;
    Pen.Style := psSolid;
    t := RcChart.Bottom + (2 * SMALL_MARGIN);
    Last := 0;
    for x := 0 to XAxis.Count - 1 do
    begin
      P := PAxisInfo(XAxis[x]);
      Str := P^.Caption;
      w := TextWidth(Str);
      l := P^.Px - (w div 2);
      if (Last < l) then
      begin
        TextOut(l, t, Str);
        Last := l + w;
      end;
      MoveTo(P^.Px, P^.Py);
      LineTo(P^.Px, P^.Py + SMALL_MARGIN);
    end;
    if FShowXGrid then
    begin
      Pen.Style := psDot;
      Pen.Color := clGray;
      t := RcChart.Top;
      for x := 1 to XAxis.Count - 2 do
      begin
        P := PAxisInfo(XAxis[x]);
        MoveTo(P^.Px, P^.Py);
        LineTo(P^.px, t);
      end;
      Pen.Color := clBlack;
    end;
  end;
end;

procedure TplSimpleChart.DrawYAxis(ACanvas: TCanvas);
var
  l, t, h, w: integer;
  x: integer;
  Str: string;
  P: PAxisInfo;
begin
  with ACanvas do
  begin
    Pen.Style := psSolid;
    Pen.Width := 3;
    MoveTo(RcChart.Left, RcChart.Top);
    LineTo(RcChart.Left, RcChart.Bottom);
    h := RcChart.Bottom - RcChart.Top;
    l := OUTER_MARGIN;
    Font.Assign(FNormalFont);
    Font.Style := [fsBold];
    t := RcChart.Bottom - ((h - TextWidth(FAxisYTitle)) div 2);
    RotTextOut(ACanvas, l, t, 90, FAxisYTitle);
    Font.Assign(FNormalFont);
    Pen.Color := clBlack;
    Pen.Width := 1;
    Pen.Style := psSolid;
    l := RcChart.Left - (2 * SMALL_MARGIN);
    for x := 0 to YAxis.Count - 1 do
    begin
      P := PAxisInfo(YAxis[x]);
      Str := P^.Caption;
      w := TextWidth(Str);
      h := TextHeight(Str);
      t := P^.Py - (h div 2);
      TextOut(l - w, t, Str);
      MoveTo(P^.Px - SMALL_MARGIN, P^.Py);
      LineTo(P^.Px, P^.Py);
    end;
    if FShowYGrid then
    begin
      l := RcChart.Right;
      for x := 1 to YAxis.Count - 2 do
      begin
        P := PAxisInfo(YAxis[x]);
        if (P^.Value = 0) then
        begin
          Pen.Style := psSolid;
          Pen.Color := clBlack;
        end
        else
        begin
          Pen.Style := psDot;
          Pen.Color := clGray;
        end;
        MoveTo(P^.Px, P^.Py);
        LineTo(l, P^.Py);
      end;
      Pen.Color := clBlack;
    end;
  end;
end;

procedure TplSimpleChart.DrawChart(ACanvas: TCanvas);
var
  x: integer;
begin
  with ACanvas do
  begin
    Brush.Style := bsClear;
    Pen.Style := psSolid;
    Pen.Width := 1;
    MoveTo(RcChart.Left, RcChart.Top);
    LineTo(RcChart.Right, RcChart.Top);
    LineTo(RcChart.Right, RcChart.Bottom);
  end;
  for x := 0 to List.Count - 1 do
  begin
    if (TplSimpleSeries(List[x]).FKind = skBar) then
      DrawSeries(ACanvas, x);
  end;
  for x := 0 to List.Count - 1 do
  begin
    if (TplSimpleSeries(List[x]).FKind <> skBar) then
      DrawSeries(ACanvas, x);
  end;
end;

procedure TplSimpleChart.AutoColors(ACanvas: TCanvas; Index: integer; Isbar: boolean);
var
  cl: TColor;
  Idx: integer;
  Bmp: TBitMap;
begin

  if FMonochrome then
    cl := clBlack
  else
  if FSoftColors then
    cl := Colors2[Index mod 14]
  else
    cl := Colors1[Index mod 14];

  Marker := Markers[Index mod 5];

  with ACanvas do
  begin
    Pen.Color := cl;
    Brush.Bitmap := nil;
    Brush.Style := bsSolid;

    if IsBar then
      Brush.Color := cl
    else
      Brush.Color := clWhite;

    if IsBar and FMonochrome then
    begin
      Idx := Index mod 16;
      if not Assigned(Brushes[Idx]) then
      begin
        Bmp := TBitMap.Create;
        Bmp.LoadFromResourceName(hInstance, Format('brush%.2d', [Idx + 1]));
        Brushes[Idx] := Bmp;
      end;
      Brush.Bitmap := Brushes[Idx];
    end;
  end;
end;

procedure TplSimpleChart.CalculateSeries;
var
  x, y: integer;
  Values: TList;
  P: PXYInfo;
  S: TBSpline;
  Vertex: TVertex;
  sr: TplSimpleSeries;
  bw, rw, bi, dx, l: integer;
begin
  if (List.Count = 0) or ChartEmpty then
    Exit;
  bi := 0;
  bw := 0;
  if (BarCount > 0) then
    bw := Round(BarWidth / (BarCount + 1));
  for x := 0 to List.Count - 1 do
  begin
    sr := TplSimpleSeries(List[x]);
    s := sr.Spline;
    s.Clear;
    Values := sr.Values;
    case sr.FKind of
      skBar:
      begin
        dx := Round(-(BarWidth / 2) + (bw / 2) + (bi * bw) + (bw * 0.1));
        rw := Round(bw * 0.8);
        for y := 0 to Values.Count - 1 do
        begin
          P := PXYInfo(Values[y]);
          ChartToClient(P^.X, P^.Y, P^.Px, P^.Py);
          l := P^.Px + dx;
          if (P^.Y < 0) then
            P^.Rc := Rect(l, YZero, l + rw, P^.Py)
          else
            P^.Rc := Rect(l, P^.Py, l + rw, YZero);
        end;
        Inc(bi);
      end;
      skLine:
      begin
        for y := 0 to Values.Count - 1 do
        begin
          P := PXYInfo(Values[y]);
          ChartToClient(P^.X, P^.Y, P^.Px, P^.Py);
          P^.Rc := Rect(P^.Px - MarkSize, P^.Py - MarkSize, P^.Px + MarkSize, P^.Py + MarkSize);
        end;
      end;
      skSmooth:
      begin
        for y := 0 to Values.Count - 1 do
        begin
          P := PXYInfo(Values[y]);
          ChartToClient(P^.X, P^.Y, P^.Px, P^.Py);
          P^.Rc := Rect(P^.Px - MarkSize, P^.Py - MarkSize, P^.Px + MarkSize, P^.Py + MarkSize);
          Vertex.X := P^.Px;
          Vertex.Y := P^.Py;
          s.AddPoint(Vertex);
        end;
        s.Interpolated := True;
        s.Fragments := s.NumberOfPoints * 20;
      end;
    end;
  end;
end;


procedure TplSimpleChart.DrawSeries(ACanvas: TCanvas; Index: integer);
var
  x: integer;
  P: PXYInfo;
  l, t, t2: integer;
  Sr: TplSimpleSeries;
  Rc: TRect;
begin
  Sr := TplSimpleSeries(List[Index]);
  AutoColors(ACanvas, Index, sr.FKind = skBar);
  with ACanvas do
  begin
    if (sr.FKind = skBar) then
    begin
      for x := 0 to Sr.Values.Count - 1 do
      begin
        P := PXYInfo(Sr.Values[x]);
        Rectangle(P^.Rc);
      end;
    end
    else
    begin
      if (sr.FKind = skLine) then
      begin
        for x := 0 to Sr.Values.Count - 1 do
        begin
          P := PXYInfo(Sr.Values[x]);
          if (x = 0) then
            MoveTo(P^.Px, P^.Py)
          else
            LineTo(P^.Px, P^.Py);
        end;
      end
      else
      if (sr.FKind = skSmooth) then
        sr.Spline.Draw(ACanvas);
      for x := 0 to Sr.Values.Count - 1 do
      begin
        P := PXYInfo(Sr.Values[x]);
        Marker(ACanvas, P^.Px, P^.Py, MarkSize);
      end;
    end;
    if FShowLegend then
    begin
      l := RcLegend.Left + INNER_MARGIN;
      t := RcLegend.Top + Sr.Top;
      if (sr.FKind = skBar) then
      begin
        Rc := Rect(l, t, l + LEGEND_ITEM, t + LEGEND_ITEM);
        InflateRect(Rc, -2, -2);
        Rectangle(Rc);
      end
      else
      begin
        t2 := t + (LEGEND_ITEM div 2);
        MoveTo(l, t2);
        LineTo(l + LEGEND_ITEM, t2);
        Marker(ACanvas, l + (LEGEND_ITEM div 2), t2, MarkSize);
      end;
    end;
  end;
end;

procedure TplSimpleChart.ChartToClient(const AX, AY: double; var X, Y: integer);
var
  i: integer;
begin
  if FAxisXOnePerValue then
  begin
    for i := 0 to XAxis.Count - 1 do
    begin
      if (AX = PAxisInfo(XAxis[i])^.Value) then
      begin
        X := PAxisInfo(XAxis[i])^.Px;
        Break;
      end;
    end;
  end
  else
    X := FXTranslate.Base + Round((AX - FXTranslate.MinValue) * FXTranslate.Scale);
  Y := FYTranslate.Base - Round((AY - FYTranslate.MinValue) * FYTranslate.Scale);
end;

function TplSimpleChart.ClientToChart(const X, Y: integer; var AX, AY: double): boolean;
var
  i: integer;
  n, d: integer;
begin
  Result := PtInRect(RcChart, Point(X, Y));
  if Result then
  begin
    if FAxisXOnePerValue then
    begin
      n := MaxInt;
      for i := 0 to XAxis.Count - 1 do
      begin
        d := Abs(X - PAxisInfo(XAxis[i])^.Px);
        if (d < n) then
        begin
          AX := PAxisInfo(XAxis[i])^.Value;
          n := d;
        end;
      end;
    end
    else
      AX := FXTranslate.MinValue + ((X - FXTranslate.Base) / FXTranslate.Scale);
    AY := FYTranslate.MinValue + ((FYTranslate.Base - Y) / FYTranslate.Scale);
  end;
end;

//================================================

     {
function TplSimpleChart.CreateMetafile: TMetafile;
const
  InitWidth = 800;
  InitHeight = 600;
var
  mc: TMetafileCanvas;
  AWidth, AHeight: Integer;
begin
  AWidth := InitWidth;
  AHeight := InitHeight;
  Calculate(AWidth, AHeight);
  if (RcLegend.Bottom > (AHeight - OUTER_MARGIN))
    then AHeight := RcLegend.Bottom + OUTER_MARGIN;
  if ((RcChart.Right - RcChart.Left) < (RcChart.Bottom - RcChart.Top))
    then AWidth := AWidth + ((RcChart.Bottom - RcChart.Top) - (RCChart.Right - RcChart.Left));
  if (AWidth <> InitWidth) or (AHeight <> InitHeight)
    then Calculate(AWidth, AHeight);
  Result := TMetafile.Create;
  Result.Width := AWidth;
  Result.Height := AHeight;
  mc := TMetafileCanvas.Create(Result, 0);
  InternalPaint(mc);
  mc.Free;
  Calculate(ClientWidth, ClientHeight);
end;
      }
procedure TplSimpleChart.CopyToClipboard;
//var
//  Wmf: TMetafile;
begin
  {Wmf := CreateMetafile;
  Clipboard.Assign(Wmf);
  Wmf.Free;  }
end;

end.
