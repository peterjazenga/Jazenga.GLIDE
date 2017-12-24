
{**********************************************************************
 Package pl_ExControls.pkg
 From PilotLogic Software House(http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit TplMultiGraphUnit;

interface

uses
  StdCtrls, SysUtils, Controls, Classes, Graphics, Types;

type
  PItem = ^TItem;

  TItem = record
    Data: int64;
    Next: PItem;
  end;

  TLinkedList = class
  private
    procedure DelAllFromItem(var Item: PItem);
    function GetItem(pos: integer): PItem;
  protected
    fValueAdded: TNotifyEvent;
    StartItem: PItem;
    ItemCount: integer;
  public
    property OnValueAddedd: TNotifyEvent read fValueAdded write fValueAdded;
    constructor Create;
    destructor Destroy; override;
    procedure Add(val: int64; pos: integer);
    procedure AddEnd(val: int64);
    procedure AddFirst(val: int64);
    procedure Clear;
    function Count: integer;
    procedure DelAll;
    procedure DelAllFromPos(pos: integer);
    procedure Delete(pos: integer);
    function FIFODequeue: int64;
    procedure FIFOEnqueue(val: int64);
    function GetData(pos: integer): int64;
    function GetItemCount: integer;
    function GetPos(val: int64): integer;
    function High: integer;
    function LIFOPop: int64;
    procedure LIFOPush(val: int64);
    procedure LoadFromFile(Filename: string);
    procedure SaveToFile(Filename: string);
    procedure Show(var Memo: TMemo);
  end;

  TRAGGraphType = (gtLines, gtBarsAbsolute, gtBarsRelative, gtArea);

  TRAGGraph = class
  private
    fRenderRequest: TNotifyEvent;
    fCalculationRequest: TNotifyEvent;
    _values: TLinkedList;
    _length: integer;
    _maxValue: int64;
    _type: TRAGGraphType;
    _color: TColor;
    _lineWidth: integer;
    _LowerThresholdColor: TColor;
    _UpperThresholdColor: TColor;
    _LowerThreshold: integer;
    _UpperThreshold: integer;
    _visible: boolean;
    _points: array of TPoint;
    procedure SetValues(values: TLinkedList);
    function GetValues(): TLinkedList;
    function GetGraphLength(): integer;
    function GetMaxValue(): int64;
    procedure SetType(typ: TRAGGraphType);
    function GetType(): TRAGGraphType;
    procedure SetColor(color: TColor);
    function GetColor(): TColor;
    procedure SetLineWidth(Width: integer);
    function GetLineWidth(): integer;
    procedure SetLowerThresholdColor(color: TColor);
    function GetLowerThresholdColor(): TColor;
    procedure SetUpperThresholdColor(color: TColor);
    function GetUpperThresholdColor(): TColor;
    procedure SetLowerThreshold(threshold: int64);
    function GetLowerThreshold(): int64;
    procedure SetUpperThreshold(threshold: int64);
    function GetUpperThreshold(): int64;
    procedure SetVisible(Visible: boolean);
    function GetVisible(): boolean;
  public
    constructor Create(length: integer);
    procedure Render(renderArea: TCanvas; yScalar: real; xScalar: real; Height: integer);
    procedure Calculate(Width, Height: integer; yScalar: real; xScalar: real);
    procedure AddValue(Value: int64);
    procedure Clear();
    property RenderRequest: TNotifyEvent read fRenderRequest write fRenderRequest;
    property CalculationRequest: TNotifyEvent read fCalculationRequest write fCalculationRequest;
    property Values: TLinkedList read GetValues write SetValues;
    property Length: integer read GetGraphLength;
    property MaxValue: int64 read GetMaxValue;
    property GraphType: TRAGGraphType read GetType write SetType;
    property Color: TColor read GetColor write SetColor;
    property LineWidth: integer read GetLineWidth write SetLineWidth;
    property LowerThresholdColor: TColor read GetLowerThresholdColor write SetLowerThresholdColor;
    property UpperThresholdColor: TColor read GetUpperThresholdColor write SetUpperThresholdColor;
    property LowerThreshold: int64 read GetLowerThreshold write SetLowerThreshold;
    property UpperThreshold: int64 read GetUpperThreshold write SetUpperThreshold;
    property Visible: boolean read GetVisible write SetVisible;
  end;

  TGraphArray = array of TRAGGraph;

  TRAG = class
  private
    _graphsCount: integer;
    _yScalar: real;
    _bitmap: TBitmap;
    _gridBitmap: TBitmap;
    _picture: TPicture;
    _gridOffset: integer;
    _gridStepHorz: integer;
    _gridStepVert: integer;
    _gridColor: TColor;
    _gridWidth: integer;
    _backColor: TColor;
    _xScalar: real;
    _graphs: TGraphArray;
    _valueOffsetPercentage: integer;
    _textFont: TFont;
    _textColor: TColor;
    _textBackground: boolean;
    _textLeftTop, _textRightTop, _textLeftBottom, _textRightBottom: string;
    function GetGridStepHorz(): integer;
    procedure SetGridStepHorz(gridStep: integer);
    function GetGridStepVert(): integer;
    procedure SetGridStepVert(gridStep: integer);
    function GetGridColor(): TColor;
    procedure SetGridColor(color: TColor);
    function GetGridWidth(): integer;
    procedure SetGridWidth(Width: integer);
    function GetBackColor(): TColor;
    procedure SetBackColor(color: TColor);
    function GetXScalar(): real;
    procedure SetXScalar(xscalar: real);
    function GetPicture(): TPicture;
    function GetGraphs(): TGraphArray;
    function GetValueOffsetPercentage(): integer;
    procedure SetValueOffsetPercentage(percentage: integer);
    function GetTextFont(): TFont;
    procedure SetTExtFont(font: TFont);
    function GetTextColor(): TColor;
    procedure SetTextColor(color: TColor);
    function GetTextBackground(): boolean;
    procedure SetTextBackground(background: boolean);
    function GetTextLeftTop(): string;
    procedure SetTextLeftTop(Text: string);
    function GetTextRightTop(): string;
    procedure SetTextRightTop(Text: string);
    function GetTextLeftBottom(): string;
    procedure SetTextLeftBottom(Text: string);
    function GetTextRightBottom(): string;
    procedure SetTextRightBottom(Text: string);
  public
    constructor Create(Width, Height: integer);
    procedure RedrawGraphs(recalculate: boolean);
    procedure AddGraph(graph: TRAGGraph); overload;
    procedure AddGraph(length: integer; color: TColor; graphType: TRAGGraphType); overload;
    procedure AddValues(values: array of int64);
    procedure AddValue(graphIndex: integer; Value: int64);
    property Graphs: TGraphArray read GetGraphs;
    property Picture: TPicture read GetPicture;

    property GridStepHorz: integer read GetGridStepHorz write SetGridStepHorz;
    property GridStepVert: integer read GetGridStepVert write SetGridStepVert;
    property GridColor: TColor read GetGridColor write SetGridColor;
    property GridWidth: integer read GetGridWidth write SetGridWidth;
    property BackColor: TColor read GetBackColor write SetBackColor;
    property XScalar: real read GetXScalar write SetXScalar;
    property ValueOffsetPercentage: integer read GetValueOffsetPercentage write SetValueOffsetPercentage;
    property TextFont: TFont read GetTextFont write SetTextFont;
    property TextColor: TColor read GetTextColor write SetTextColor;
    property TextBackground: boolean read GetTextBackground write SetTextBackground;
    property TextLeftTop: string read GetTextLeftTop write SetTextLeftTop;
    property TextRightTop: string read GetTextRightTop write SetTextRightTop;
    property TextLeftBottom: string read GetTextLeftBottom write SetTextLeftBottom;
    property TextRightBottom: string read GetTextRightBottom write SetTextRightBottom;
  end;

  TplMultiGraph = class(TCustomControl)
  protected
    frag: TRAG;

    fGridStepHorz: integer;
    fGridStepVert: integer;
    fGridColor: TColor;
    fGridWidth: integer;
    fXScalar: real;
    fValueOffsetPercentage: integer;
    fTextColor: TColor;
    fTextBackground: boolean;
    fTextLeftTop: string;
    fTextRightTop: string;
    fTextLeftBottom: string;
    fTextRightBottom: string;

    procedure SetGridStepHorz(val: integer);
    procedure SetGridStepVert(val: integer);
    procedure SetGridColor(val: TColor);
    procedure SetGridWidth(val: integer);
    procedure SetXScalar(val: real);
    procedure SetValueOffsetPercentage(val: integer);
    procedure SetTextColor(val: TColor);
    procedure SetTextBackground(val: boolean);
    procedure SetTextLeftTop(val: string);
    procedure SetTextRightTop(val: string);
    procedure SetTextLeftBottom(val: string);
    procedure SetTextRightBottom(val: string);

    procedure FontChanged(Sender: TObject); override;
    procedure SetColor(Value: TColor); override;
    procedure Paint; override;
    procedure Resize; override;
    procedure CreateGraph;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property TheGraph: TRAG read frag;

  published
    property GridStepHorz: integer read fGridStepHorz write SetGridStepHorz;
    property GridStepVert: integer read fGridStepVert write SetGridStepVert;
    property GridColor: TColor read fGridColor write SetGridColor;
    property GridWidth: integer read fGridWidth write SetGridWidth;
    property GridXScalar: real read fXScalar write SetXScalar;
    property GridValueOffsetPercentage: integer read fValueOffsetPercentage write SetValueOffsetPercentage;
    property GridTextColor: TColor read fTextColor write SetTextColor;
    property GridTextBackground: boolean read fTextBackground write SetTextBackground;
    property GridTextLeftTop: string read fTextLeftTop write SetTextLeftTop;
    property GridTextRightTop: string read fTextRightTop write SetTextRightTop;
    property GridTextLeftBottom: string read fTextLeftBottom write SetTextLeftBottom;
    property GridTextRightBottom: string read fTextRightBottom write SetTextRightBottom;

    property Align;
    property Visible;
    property Color;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property ShowHint;
    property Anchors;
    property BiDiMode write SetBidiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;

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

//============== TplMultiGraph ==============================================
constructor TplMultiGraph.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque, csReplicatable];

  SetBounds(0, 0, 200, 150);
  //...............................
  fgridColor := clGray;
  fgridWidth := 1;
  fvalueOffsetPercentage := 10;
  fgridStepHorz := 10;
  fgridStepVert := 10;
  fxScalar := 2;
  ftextColor := clBlack;
  ftextLeftTop := '';
  ftextRightTop := '';
  ftextLeftBottom := '';
  ftextRightBottom := '';
  //....................................
  CreateGraph;
end;

procedure TplMultiGraph.CreateGraph;
begin
  frag := TRAG.Create(Width, Height);
  frag.BackColor := self.color;
  frag.TextFont := self.Font;

  frag.GridStepHorz := fGridStepHorz;
  frag.GridStepVert := fGridStepVert;
  frag.GridColor := fGridColor;
  frag.GridWidth := fGridWidth;
  frag.XScalar := fXScalar;
  frag.ValueOffsetPercentage := fValueOffsetPercentage;
  frag.TextColor := fTextColor;
  frag.TextBackground := fTextBackground;
  frag.TextLeftTop := fTextLeftTop;
  frag.TextRightTop := fTextRightTop;
  frag.TextLeftBottom := fTextLeftBottom;
  frag.TextRightBottom := fTextRightBottom;

end;

destructor TplMultiGraph.Destroy;
begin
  frag.Free;
  inherited Destroy;
end;

procedure TplMultiGraph.Paint;
begin
  inherited;
  frag.RedrawGraphs(True);
  Canvas.StretchDraw(ClientRect, frag.Picture.Graphic);
end;

procedure TplMultiGraph.Resize;
begin
  inherited;
  if frag <> nil then
    FreeAndNil(frag);
  CreateGraph;
  Invalidate;
end;

procedure TplMultiGraph.FontChanged(Sender: TObject);
begin
  inherited;
  frag.TextFont := self.Font;
  Invalidate;
end;

procedure TplMultiGraph.SetColor(Value: TColor);
begin
  inherited;
  frag.BackColor := self.color;
  Invalidate;
end;

//.......

procedure TplMultiGraph.SetGridStepHorz(val: integer);
begin
  if val = fGridStepHorz then
    exit;
  fGridStepHorz := val;
  frag.GridStepHorz := val;
  Invalidate;
end;

procedure TplMultiGraph.SetGridStepVert(val: integer);
begin
  if val = fGridStepVert then
    exit;
  fGridStepVert := val;
  frag.GridStepVert := val;
  Invalidate;
end;

procedure TplMultiGraph.SetGridColor(val: TColor);
begin
  if val = fGridColor then
    exit;
  fGridColor := val;
  frag.GridColor := val;
  Invalidate;
end;

procedure TplMultiGraph.SetGridWidth(val: integer);
begin
  if val = fGridWidth then
    exit;
  fGridWidth := val;
  frag.GridWidth := val;
  Invalidate;
end;

procedure TplMultiGraph.SetXScalar(val: real);
begin
  if val = fXScalar then
    exit;
  fXScalar := val;
  frag.XScalar := val;
  Invalidate;
end;

procedure TplMultiGraph.SetValueOffsetPercentage(val: integer);
begin
  if val = fValueOffsetPercentage then
    exit;
  fValueOffsetPercentage := val;
  frag.ValueOffsetPercentage := val;
  Invalidate;
end;

procedure TplMultiGraph.SetTextColor(val: TColor);
begin
  if val = fTextColor then
    exit;
  fTextColor := val;
  frag.TextColor := val;
  Invalidate;
end;

procedure TplMultiGraph.SetTextBackground(val: boolean);
begin
  if val = fTextBackground then
    exit;
  fTextBackground := val;
  frag.TextBackground := val;
  Invalidate;
end;

procedure TplMultiGraph.SetTextLeftTop(val: string);
begin
  if val = fTextLeftTop then
    exit;
  fTextLeftTop := val;
  frag.TextLeftTop := val;
  Invalidate;
end;

procedure TplMultiGraph.SetTextRightTop(val: string);
begin
  if val = fTextRightTop then
    exit;
  fTextRightTop := val;
  frag.TextRightTop := val;
  Invalidate;
end;

procedure TplMultiGraph.SetTextLeftBottom(val: string);
begin
  if val = fTextLeftBottom then
    exit;
  fTextLeftBottom := val;
  frag.TextLeftBottom := val;
  Invalidate;
end;

procedure TplMultiGraph.SetTextRightBottom(val: string);
begin
  if val = fTextRightBottom then
    exit;
  fTextRightBottom := val;
  frag.TextRightBottom := val;
  Invalidate;
end;



//=========== TRAG ==========================================================

constructor TRAG.Create(Width, Height: integer);
begin
  SetLength(_graphs, 0);

  _gridColor := clGray;
  _gridWidth := 1;
  _backColor := clWhite;

  _valueOffsetPercentage := 10;

  _picture := TPicture.Create();
  _bitmap := TBitmap.Create();
  _bitmap.Width := Width;
  _bitmap.Height := Height;
  _bitmap.PixelFormat := pf32Bit;
  _picture.Bitmap := _bitmap;

  _gridBitmap := TBitmap.Create();
  _gridBitmap.Width := Width;
  _gridBitmap.Height := Height;
  _gridBitmap.PixelFormat := pf32Bit;

  self._gridOffset := 0;
  self._gridStepHorz := 10;
  self._gridStepVert := 10;
  self._xScalar := 2;

  _textColor := clBlack;
  _textFont := TFont.Create();
  _textFont.Color := _textColor;
  _textFont.Name := 'Verdana';
  _textFont.Size := 10;

  _textLeftTop := '';
  _textRightTop := '';
  _textLeftBottom := '';
  _textRightBottom := '';
end;

procedure TRAG.RedrawGraphs(recalculate: boolean);
var
  i: integer;
var
  highest: int64;
var
  textWidth, textHeight: integer;
begin

  //Scalar Calcualting
  if (recalculate) then
  begin

    highest := 0;

    for i := 0 to High(_graphs) do
    begin
      if (_graphs[i].MaxValue > highest) and (_graphs[i].Visible) and (_graphs[i].GraphType <> gtBarsRelative) then
        highest := _graphs[i].MaxValue;
    end;

    if (_valueOffsetPercentage > 0) then
      Inc(highest, Round((Ord(highest{_bitmap.Height}) / Ord(100) * Ord(_valueOffsetPercentage))));

    if (highest > 0) then
    begin
      _yScalar := (_bitmap.Height) / highest;
    end
    else
    begin
      _yScalar := 1;
    end;
  end;


  //Grid Calculating
  _gridBitmap.Canvas.Brush.Color := _backColor;
  _gridBitmap.Canvas.Rectangle(0, 0, _bitmap.Width, _bitmap.Height);

  _gridBitmap.Canvas.Pen.Color := _gridColor;
  i := 0; //self.Width - _gridOffset;
  while (i <= _bitmap.Width + Round(_gridOffset * _xScalar){>= 0}) do
  begin

    _gridBitmap.Canvas.MoveTo(i - Round(_gridOffset * _xScalar), 0);
    _gridBitmap.Canvas.LineTo(i - Round(_gridOffset * _xScalar), _bitmap.Height);

    i := Round((i + _gridStepHorz * _xScalar));

  end;

  i := _bitmap.Height;
  while (i >= 0) do
  begin
    _gridBitmap.Canvas.MoveTo(0, i);
    _gridBitmap.Canvas.LineTo(_bitmap.Width, i);

    i := Round((i - _gridStepVert * _yScalar));
  end;

  _bitmap.Canvas.Draw(0, 0, _gridBitmap);


  //Graph Calculating
  for i := 0 to High(_graphs) do
  begin

    if (recalculate) then
    begin
      _graphs[i].Calculate(_bitmap.Width, _bitmap.Height, _yScalar, _xScalar);
    end;

    _graphs[i].Render(_bitmap.Canvas, _yScalar, _xScalar, _bitmap.Height);
  end;


  if (_textBackground) then
  begin
    _bitmap.Canvas.Brush.Style := bsSolid;
    _bitmap.Canvas.Brush.Color := _backColor;
  end
  else
  begin
    _bitmap.Canvas.Brush.Style := bsClear;
  end;

  _textFont.Color := _textColor;
  _bitmap.Canvas.Font := _textFont;

  if (_textLeftTop <> '') then
  begin
    _bitmap.Canvas.TextOut(1, 1, _textLeftTop);
  end;

  if (_textRightTop <> '') then
  begin
    textWidth := _bitmap.Canvas.TextWidth(_textRightTop);
    _bitmap.Canvas.TextOut(_bitmap.Width - textWidth - 1, 1, _textRightTop);
  end;

  if (_textLeftBottom <> '') then
  begin
    textHeight := _bitmap.Canvas.TextHeight(_textLeftBottom);
    _bitmap.Canvas.TextOut(1, _bitmap.Height - textHeight - 1, _textLeftBottom);
  end;

  if (_textRightBottom <> '') then
  begin
    textHeight := _bitmap.Canvas.TextHeight(_textRightBottom);
    textWidth := _bitmap.Canvas.TextWidth(_textRightBottom);
    _bitmap.Canvas.TextOut(_bitmap.Width - textWidth - 1, _bitmap.Height - textHeight - 1, _textRightBottom);
  end;


  _picture.Bitmap := _bitmap;
end;

procedure TRAG.AddValues(values: array of int64);
var
  i: integer;
var
  max: integer;
var
  prevLength: integer;
begin
  prevLength := _graphs[0].Values.Count;
  max := High(_graphs);
  if (High(values) < max) then
    max := High(values);

  for i := 0 to max do
  begin
    _graphs[i].AddValue(values[i]);
  end;

  if (prevLength = _graphs[0].Values.Count) then
  begin
    Inc(_gridOffset);
  end;

  if (_gridOffset >= _gridStepHorz) then
  begin
    _gridOffset := 0;
  end;
end;

procedure TRAG.AddValue(graphIndex: integer; Value: int64);
var
  prevLength: integer;
begin
  if (graphIndex >= 0) and (graphIndex <= High(_graphs)) then
  begin
    prevLength := _graphs[0].Values.Count;

    _graphs[graphIndex].AddValue(Value);

    if (High(_graphs) = 0) and (prevLength = _graphs[0].Values.Count) then
    begin

      Inc(_gridOffset);

      if (_gridOffset >= _gridStepHorz) then
      begin
        _gridOffset := 0;
      end;

    end;
  end;
end;

procedure TRAG.AddGraph(graph: TRAGGraph);
begin
  Inc(_graphsCount, 1);
  SetLength(_graphs, _graphsCount);
  _graphs[_graphsCount - 1] := graph;
end;

procedure TRAG.AddGraph(length: integer; color: TColor; graphType: TRAGGraphType);
var
  graph: TRAGGraph;
begin
  graph := TRAGGraph.Create(length);
  AddGraph(graph);
  _graphs[_graphsCount - 1].Color := color;
  _graphs[_graphsCount - 1].GraphType := graphType;
end;

procedure TRAG.SetGridStepHorz(gridStep: integer);
begin
  _gridStepHorz := gridStep;
end;

function TRAG.GetGridStepHorz(): integer;
begin
  GetGridStepHorz := _gridStepHorz;
end;

procedure TRAG.SetGridStepVert(gridStep: integer);
begin
  _gridStepVert := gridStep;
end;

function TRAG.GetGridStepVert(): integer;
begin
  GetGridStepVert := _gridStepVert;
end;

procedure TRAG.SetGridColor(color: TColor);
begin
  _gridColor := color;
end;

function TRAG.GetGridColor(): TColor;
begin
  GetGridColor := _gridColor;
end;

function TRAG.GetGridWidth(): integer;
begin
  GetGridWidth := _gridWidth;
end;

procedure TRAG.SetGridWidth(Width: integer);
begin
  _gridWidth := Width;
end;

procedure TRAG.SetBackColor(color: TColor);
begin
  _backColor := color;
end;

function TRAG.GetBackColor(): TColor;
begin
  GetBackColor := _backColor;
end;

procedure TRAG.SetXScalar(xscalar: real);
begin
  _xScalar := xscalar;
end;

function TRAG.GetXScalar(): real;
begin
  GetXScalar := _xScalar;
end;

function TRAG.GetPicture(): TPicture;
begin
  GetPicture := _picture;
end;

function TRAG.GetGraphs(): TGraphArray;
begin
  GetGraphs := _graphs;
end;

function TRAG.GetValueOffsetPercentage(): integer;
begin
  GetValueOffsetPercentage := _valueOffsetPercentage;
end;

procedure TRAG.SetValueOffsetPercentage(percentage: integer);
begin
  _valueOffsetpercentage := percentage;
end;


function TRAG.GetTextFont(): TFont;
begin
  GetTextFont := _textFont;
end;

procedure TRAG.SetTextFont(font: TFont);
begin
  _textFont := font;
end;

function TRAG.GetTextColor(): TColor;
begin
  GetTextColor := _textColor;
end;

procedure TRAG.SetTextColor(color: TColor);
begin
  _textColor := color;
end;

function TRAG.GetTextBackground(): boolean;
begin
  GetTextBackground := _textBackground;
end;

procedure TRAG.SetTextBackground(background: boolean);
begin
  _textBackground := background;
end;

function TRAG.GetTextLeftTop(): string;
begin
  GetTextLeftTop := _textLeftTop;
end;

procedure TRAG.SetTextLeftTop(Text: string);
begin
  _textLeftTop := Text;
end;

function TRAG.GetTextRightTop(): string;
begin
  GetTextRightTop := _textRightTop;
end;

procedure TRAG.SetTextRightTop(Text: string);
begin
  _textRightTop := Text;
end;

function TRAG.GetTextLeftBottom(): string;
begin
  GetTextLeftBottom := _textLeftBottom;
end;

procedure TRAG.SetTextLeftBottom(Text: string);
begin
  _textLeftBottom := Text;
end;

function TRAG.GetTextRightBottom(): string;
begin
  GetTextRightBottom := _textRightBottom;
end;

procedure TRAG.SetTextRightBottom(Text: string);
begin
  _textRightBottom := Text;
end;


//================TRAGGraph========================================================

constructor TRAGGraph.Create(length: integer);
begin
  _length := length;
  _values := TLinkedList.Create();
  SetLength(_points, _length);
  _visible := True;
  _color := clGreen;
  _LowerThresholdColor := clYellow;
  _UpperThresholdColor := clRed;
  _LowerThreshold := 40;
  _UpperThreshold := 70;
end;


procedure TRAGGraph.Render(renderArea: TCanvas; yScalar: real; xScalar: real; Height: integer);
var
  i: integer;
var
  pt: TPoint;
var
  percentage: integer;
var
  y: extended;
begin
  if (not _visible) then
    exit;

  if (_type = gtLines) then
  begin
    renderArea.Pen.Color := _color;
    renderArea.Brush.Color := _color;
    renderArea.Pen.Width := _lineWidth;
    renderArea.Polyline(_points);
  end
  else if (_type = gtArea) then
  begin

    renderArea.Brush.Color := _color;
    renderArea.Polygon(_points);
    renderArea.Pen.Color := clBlack;
    renderArea.Pen.Width := _lineWidth;
    renderArea.Polyline(_points);
  end
  else if (_type = gtBarsAbsolute) then
  begin
    renderArea.Pen.Width := _lineWidth;
    for i := 0 to High(_points) do
    begin
      pt := _points[i];

      renderArea.MoveTo(pt.x, Height - pt.y);

      if (_values.GetData(i) < _LowerThreshold) then //pt.Y < _LowerThreshold)then
      begin
        renderArea.Pen.Color := _UpperThresholdColor;//_color;
      end
      else if (_values.GetData(i) >= _LowerThreshold) and (_values.GetData(i) <= _UpperThreshold) then
        //pt.Y >= _LowerThreshold) and (pt.Y <= _threshold2)then
      begin
        renderArea.Pen.Color := _LowerThresholdColor;
      end
      else if (_values.GetData(i) > _UpperThreshold) then //pt.Y > _threshold2)then
      begin
        renderArea.Pen.Color := _color;//_UpperThresholdColor;
      end;

      renderArea.MoveTo(pt.x, Height);
      renderArea.LineTo(pt.X, Height - pt.Y);
    end;

  end
  else if (_type = gtBarsRelative) then
  begin

    renderArea.Pen.Width := _lineWidth;

    for i := 0 to High(_points) do
    begin
      pt := _points[i];

      renderArea.MoveTo(pt.x, Height - pt.y);

      y := pt.Y / (Height * yScalar);

      percentage := Round(y * 100);

      if (percentage <= _LowerThreshold) then
      begin
        renderArea.Pen.Color := _color;
      end
      else if (percentage > _LowerThreshold) and (percentage <= _UpperThreshold) then
      begin
        renderArea.Pen.Color := _LowerThresholdColor;
      end
      else if (percentage > _UpperThreshold) then
      begin
        renderArea.Pen.Color := _UpperThresholdColor;
      end;

      renderArea.MoveTo(pt.x, Height);
      renderArea.LineTo(pt.X, Height - pt.Y);
    end;

  end;
end;

procedure TRAGGraph.Calculate(Width, Height: integer; yScalar: real; xScalar: real);
var
  i: integer;
var
  startValue: integer;
var
  endValue: integer;
begin
  SetLength(_points, _values.Count);

  if (_type = gtArea) then
  begin

    SetLength(_points, _values.Count + 1);
    _points[0] := Point(0, Height);
    _points[High(_points)] := Point(Round(xScalar * _values.Count), Height);

    startValue := _values.Count - 2;
    endValue := 1;
  end
  else
  begin
    startValue := _values.Count - 1;
    endValue := 0;
  end;

  for i := startValue downto endValue do
  begin
    if (_type <> gtBarsRelative) then
    begin
      _points[i] := Point(Round(xScalar * i), Height - Round(yScalar * _values.GetData(i)) - 3);
    end
    else
    begin
      _points[i] := Point(Round(xScalar * i), Round((_values.GetData(i) / 100) * (Height - 6)) + 3);
    end;
  end;

  if Assigned(fRenderRequest) and _visible then
  begin
    fRenderRequest(self);
  end;
end;

procedure TRAGGraph.AddValue(Value: int64);
var
  i: integer;
begin
  _values.FIFOEnqueue(Value);

  if (Value > _maxValue) then
  begin
    _maxValue := Value;
  end;

  if (_values.Count > _length) then
  begin
    _values.FIFODequeue();

    _maxValue := 0;

    for i := 0 to _values.Count - 1 do
    begin
      if (_values.GetData(i) > _maxValue) then
      begin
        _maxValue := _values.GetData(i);
      end;
    end;
  end;

  if Assigned(fCalculationRequest) then
  begin
    fCalculationRequest(self);
  end;
end;

procedure TRAGGraph.SetValues(values: TLinkedList);
var
  i: integer;
begin
  _values := values;

  _maxValue := 0;

  for i := 0 to _values.Count - 1 do
  begin
    if (_values.GetData(i) > _maxValue) then
    begin
      _maxValue := _values.GetData(i);
    end;
  end;

  if Assigned(fCalculationRequest) then
    fCalculationRequest(self);
end;

function TRAGGraph.GetValues(): TLinkedList;
begin
  GetValues := _values;
end;

procedure TRAGGraph.Clear();
begin
  _values.Clear();
  _maxValue := 0;
end;

function TRAGGraph.GetGraphLength(): integer;
begin
  GetGraphLength := _length;
end;

function TRAGGraph.GetMaxValue(): int64;
begin
  GetMaxValue := _maxValue;
end;

procedure TRAGGraph.SetType(typ: TRAGGraphType);
begin
  _type := typ;

  if Assigned(fCalculationRequest) then
    fCalculationRequest(self);
end;

function TRAGGraph.GetType(): TRAGGraphType;
begin
  GetType := _type;
end;

procedure TRAGGraph.SetColor(color: TColor);
begin
  _color := color;

  if Assigned(fRenderRequest) then
    fRenderRequest(self);
end;

function TRAGGraph.GetColor(): TColor;
begin
  GetColor := _color;
end;

procedure TRAGGraph.SetLineWidth(Width: integer);
begin
  _lineWidth := Width;
end;

function TRAGGraph.GetLineWidth(): integer;
begin
  GetLineWidth := _linewidth;
end;

procedure TRAGGraph.SetLowerThresholdColor(color: TColor);
begin
  _LowerThresholdColor := color;

  if Assigned(fRenderRequest) then
    fRenderRequest(self);
end;

function TRAGGraph.GetLowerThresholdColor(): TColor;
begin
  GetLowerThresholdColor := _LowerThresholdColor;
end;

procedure TRAGGraph.SetUpperThresholdColor(color: TColor);
begin
  _UpperThresholdColor := color;

  if Assigned(fRenderRequest) then
    fRenderRequest(self);
end;

function TRAGGraph.GetUpperThresholdColor(): TColor;
begin
  GetUpperThresholdColor := _UpperThresholdColor;
end;

procedure TRAGGraph.SetLowerThreshold(threshold: int64);
begin
  _LowerThreshold := threshold;

  if Assigned(fCalculationRequest) then
    fCalculationRequest(self);
end;

function TRAGGraph.GetLowerThreshold(): int64;
begin
  GetLowerThreshold := _LowerThreshold;
end;

procedure TRAGGraph.SetUpperThreshold(threshold: int64);
begin
  _UpperThreshold := threshold;

  if Assigned(fCalculationRequest) then
    fCalculationRequest(self);
end;

function TRAGGraph.GetUpperThreshold(): int64;
begin
  GetUpperThreshold := _UpperThreshold;
end;

procedure TRAGGraph.SetVisible(Visible: boolean);
begin
  _visible := Visible;

  if Assigned(fRenderRequest) then
    fRenderRequest(self);
end;

function TRAGGraph.GetVisible(): boolean;
begin
  GetVisible := _visible;
end;


//================= TLinkedList =========================================
procedure TLinkedList.DelAllFromItem(var Item: PItem);
begin
  if Item <> nil then
  begin
    if Item^.Next <> nil then
      DelAllFromItem(Item^.Next);
    Dispose(Item);
    Item := nil;
  end;
end;

function TLinkedList.GetItem(pos: integer): PItem;
var
  akt_item: PItem;
begin
  Result := nil;
  if pos = 0 then
    Result := StartItem
  else
  if pos <= ItemCount then
  begin
    akt_item := StartItem;
    repeat
      akt_item := akt_item^.Next;
      Dec(pos);
    until pos = 0;
    Result := akt_item;
  end;
end;


constructor TLinkedList.Create;
begin
  inherited;
  StartItem := nil;
  ItemCount := 0;
end;

destructor TLinkedList.Destroy;
begin
  inherited;
  DelAll;
end;

procedure TLinkedList.Add(val: int64; pos: integer);
var
  akt_item, neu_item: PItem;
var
  added: boolean;
begin
  added := False;
  New(neu_item);
  neu_item^.Data := val;

  if pos = 0 then
  begin
    akt_item := StartItem;
    neu_item^.Next := akt_item;
    StartItem := neu_item;
    Inc(ItemCount);
    added := True;
  end
  else
  if pos <= ItemCount then
  begin
    akt_item := GetItem(pos - 1);
    neu_item^.Next := akt_item^.Next;
    akt_item^.Next := neu_item;
    Inc(ItemCount);
    added := True;
  end
  else
    Dispose(neu_item);

  if (added) and (Assigned(fValueAdded)) then
    fValueAdded(self);
end;

procedure TLinkedList.AddEnd(val: int64);
begin
  Add(val, ItemCount);
end;

procedure TLinkedList.AddFirst(val: int64);
begin
  Add(val, 0);
end;

procedure TLinkedList.Clear;
begin
  DelAll;
end;

function TLinkedList.Count: integer;
begin
  Result := High + 1;
  ItemCount := Result;
end;

procedure TLinkedList.DelAll;
begin
  if StartItem <> nil then
    DelAllFromItem(StartItem);
  ItemCount := 0;
end;

procedure TLinkedList.DelAllFromPos(pos: integer);
var
  akt_item: PItem;
begin
  if pos = 0 then
    DelAllFromItem(StartItem)
  else
  begin
    akt_item := GetItem(pos - 1);
    DelAllFromItem(akt_item^.Next);
    akt_item^.Next := nil;
  end;
  ItemCount := Count;
end;

procedure TLinkedList.Delete(pos: integer);
var
  del_item, akt_item: PItem;
begin
  if StartItem <> nil then
    if pos = 0 then
    begin
      del_item := StartItem;
      StartItem := StartItem^.Next;
      Dispose(del_item);
      Dec(ItemCount);
    end
    else
    if pos < ItemCount then
    begin
      akt_item := GetItem(pos - 1);
      del_item := akt_item^.Next;
      akt_item^.Next := del_item^.Next;
      Dispose(del_item);
      Dec(ItemCount);
    end;
end;

function TLinkedList.FIFODequeue: int64;
begin
  Result := GetData(0);
  Delete(0);
end;

procedure TLinkedList.FIFOEnqueue(val: int64);
begin
  AddEnd(val);
end;

function TLinkedList.GetData(pos: integer): int64;
begin
  if pos > ItemCount - 1 then
    Result := -1
  else
    Result := GetItem(pos)^.Data;
end;

function TLinkedList.GetItemCount: integer;
begin
  Result := ItemCount;
end;

function TLinkedList.GetPos(val: int64): integer;
var
  akt_item: PItem;
begin
  Result := -1;
  akt_item := StartItem;
  if akt_item <> nil then
  begin
    Result := 0;
    while akt_item <> nil do
    begin
      if akt_item^.Data = val then
        Exit
      else
      begin
        Inc(Result);
        akt_item := akt_item^.Next;
      end;
    end;
    Result := -1;
  end;
end;

function TLinkedList.High: integer;
var
  akt_item: PItem;
begin
  akt_item := StartItem;
  Result := -1;
  if akt_item <> nil then
  begin
    Inc(Result);
    while akt_item^.Next <> nil do
    begin
      akt_item := akt_item^.Next;
      Inc(Result);
    end;
  end;
end;

function TLinkedList.LIFOPop: int64;
begin
  Result := GetData(0);
  Delete(0);
end;

procedure TLinkedList.LIFOPush(val: int64);
begin
  AddFirst(val);
end;

procedure TLinkedList.LoadFromFile(FileName: string);
var
  MyFile: Textfile;
  ctemp: int64;
begin
  if FileExists(Filename) then
  begin
    DelAll;
    Assign(MyFile, Filename);
    Reset(MyFile);
    while not EOF(MyFile) do
    begin
      ReadLn(MyFile, ctemp);
      AddEnd(ctemp);
    end;
    CloseFile(MyFile);
  end;
end;

procedure TLinkedList.SaveToFile(Filename: string);
var
  MyFile: Textfile;
  akt_item: PItem;
begin
  Assign(MyFile, Filename);
  akt_item := StartItem;
  if akt_item <> nil then
  begin
    ReWrite(MyFile);
    while akt_item <> nil do
    begin
      WriteLn(MyFile, akt_item^.Data);
      akt_item := akt_item^.Next;
    end;
    CloseFile(MyFile);
  end;
end;

procedure TLinkedList.Show(var Memo: TMemo);
var
  akt_item: PItem;
begin
  akt_item := StartItem;
  Memo.Clear;
  while akt_item <> nil do
  begin
    Memo.Lines.Add(IntToStr(akt_item^.Data));
    akt_item := akt_item^.Next;
  end;
end;



end.
