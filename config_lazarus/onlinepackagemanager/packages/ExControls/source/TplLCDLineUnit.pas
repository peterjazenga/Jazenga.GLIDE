
{**********************************************************************
 Package pl_ExControls.pkg
 From PilotLogic Software House(http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}
unit TplLCDLineUnit;

interface

uses
  SysUtils, Classes, Controls, Graphics, TplLCDLineUnit_Char;

type
  TFrameStyle = (Relief, None, Lowered, Raised);
  TFrameColorStyle = (stWindows, stColor);
  TFrameHeight = (double, single);
  TCellShape = (stSquare, stRound);

type

  TplLCDLine = class(TGraphicControl)
  private
    BitMap: TBitMap;
    FCellSize: integer;
    FCellSpace: integer;
    FColCount: integer;
    FRowCount: integer;
    FCharCount: integer;
    FGlobalColCount: integer;
    FCharWidth: integer;

    FFrameSize: integer;
    FBoardWidth: integer;
    FBoardHeight: integer;
    FLEDWidth: integer;
    FLEDHeight: integer;

    FFrameColor: TColor;
    FBoardColor: TColor;
    FCellColorOn: TColor;
    FCellColorOff: TColor;

    FBWidth: integer;
    FBHeight: integer;

    FOffset: longint;
    FLenText: integer;
    FText: string;
    FCharSpace: boolean;
    FCellsOn: array of array of boolean;
    FCountOn: integer;
    FAutoSize: boolean;
    FFrameStyle: TFrameStyle;
    FFrameHeight: TFrameHeight;
    FFrameColorStyle: TFrameColorStyle;
    FCellShape: TCellShape;

    procedure SetCellShape(const Value: TCellShape);
    procedure SetFrameColorStyle(const Value: TFrameColorStyle);
    procedure SetFrameHeight(const Value: TFrameHeight);
    procedure SetFrameStyle(const Value: TFrameStyle);
    procedure SetLAutoSize(const Value: boolean);
    function GetCellsOn(Row, Col: integer): boolean;
    procedure SetCellsOn(Row, Col: integer; const Value: boolean);
    procedure SetCharSpace(const Value: boolean);
    function GetCharCount: longint;
    function GetGlobalColCount: longint;
    procedure SetOffset(const Value: longint);
    procedure SetText(const Value: string);
    procedure SetCellColorOff(const Value: TColor);
    procedure SetCelColorlOn(const Value: TColor);
    procedure SetBoardColor(const Value: TColor);
    procedure SetFrameColor(const Value: TColor);
    procedure SetFrameSize(const Value: integer);
    procedure SetCellSize(const Value: integer);
    procedure SetCellSpace(const Value: integer);

    procedure DrawCalc();
    procedure DrawBorder();
    procedure DrawGrid();
    procedure DrawSpace();
    procedure DrawText();
    procedure DrawChar(Row, Col, NChar: integer);
    procedure DrawCell(Row, Col: integer; ColorCell: TColor);
    procedure DrawShadow(StartP, EndP: TPoint; LineColor1, LineColor2: TColor);
    procedure OutBorder();
    procedure DrawCellsOn();

  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Offset: longint read FOffset write SetOffset;
    property CharCount: longint read GetCharCount;
    property GlobalColCount: longint read GetGlobalColCount;
    property ColCount: integer read FColCount;
    property RowCount: integer read FRowCount;
    property CellsOn[Row, Col: integer]: boolean read GetCellsOn write SetCellsOn;

  published
    property ShowHint;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;

    property CellSize: integer read FCellSize write SetCellSize;
    property CellSpace: integer read FCellSpace write SetCellSpace;
    property FrameSize: integer read FFrameSize write SetFrameSize;

    property FrameColor: TColor read FFrameColor write SetFrameColor;
    property BoardColor: TColor read FBoardColor write SetBoardColor;
    property CellColorOn: TColor read FCellColorOn write SetCelColorlOn;
    property CellColorOff: TColor read FCellColorOff write SetCellColorOff;

    property Text: string read FText write SetText;
    property CharSpace: boolean read FCharSpace write SetCharSpace default False;
    property AutoSize: boolean read FAutoSize write SetLAutoSize default False;
    property FrameStyle: TFrameStyle read FFrameStyle write SetFrameStyle default Relief;
    property FrameHeight: TFrameHeight read FFrameHeight write SetFrameHeight default double;
    property FrameColorStyle: TFrameColorStyle read FFrameColorStyle write SetFrameColorStyle default stWindows;
    property CellShape: TCellShape read FCellShape write SetCellShape default stSquare;

  end;


implementation

uses
  Dialogs;

constructor TplLCDLine.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];

  Width := 156;
  Height := 76;

  FColCount := 5;
  FRowCount := 7;

  CellSize := 4;
  CellSpace := 1;
  FrameSize := 8;
  FBoardWidth := 4;

  Offset := 0;
  FrameColor := clBtnFace;
  BoardColor := clBlack;
  CellColorOn := clLime;
  CellColorOff := clGreen;

  BitMap := TBitMap.Create;
  LoadChar();
  FCountOn := 255;
end;
//********************************************
destructor TplLCDLine.Destroy;
begin
  BitMap.Free;
  FCellsOn := nil;
  inherited Destroy;
end;
//********************************************

procedure TplLCDLine.DrawCalc();
var
  WidthMin: integer;
  HeightMin: integer;
  Len: integer;
begin

  FCharWidth := (CellSize * FColCount) + (CellSpace * (FColCount + 1));


  if AutoSize then
  begin
    Len := Length(Text);
    if Len = 0 then
      Len := 1;
    FCharCount := Len;
  end
  else
    FCharCount := ((Width - (2 * FrameSize)) + CellSize) div (FCharWidth + CellSize);


  FGlobalColCount := (FCharCount * FColCount) + (FCharCount - 1);

  FLEDWidth := (FGlobalColCount * CellSize) + ((FGlobalColCount - 1) * CellSpace);
  FLEDHeight := (FRowCount * (CellSize + CellSpace)) - 1;

  if AutoSize then
  begin
    FBoardWidth := 1;
    FBoardHeight := 1;
    Width := FLEDWidth + (2 * FrameSize) + (2 * FBoardWidth);
    Height := FLEDHeight + (2 * FrameSize) + (2 * FBoardWidth);
  end
  else
  begin
    FBoardWidth := (Width - (2 * FrameSize) - FLEDWidth) div 2;
    FBoardHeight := (Height - (2 * FrameSize) - FLEDHeight) div 2;
  end;


  WidthMin := (2 * FrameSize) + FCharWidth;
  if Width < WidthMin then
    Width := WidthMin;

  HeightMin := (2 * FrameSize) + ((FRowCount * (CellSize + CellSpace)) - 1) + 2;
  if Height < HeightMin then
    Height := HeightMin;

  FBWidth := FLEDWidth + (2 * FBoardWidth);
  FBHeight := (fRowCount * (CellSize + CellSpace)) + (2 * FBoardHeight);

  BitMap.Width := Width;
  BitMap.Height := Height;

  SetLength(FCellsOn, FRowCount, FCountOn);
end;
//*************************************************

procedure TplLCDLine.DrawBorder();
var
  FStart, FEnd: TPoint;
  BStart, BEnd: TPoint;
  Color1, Color2, Color3, Color4: TColor;
  C: longint;
  R, G, B: integer;
const
  K1 = 4.637;
  K2 = 1.364;
  K3 = 1.372093;
  K4 = 2.088495;
begin

  BStart.X := FrameSize;
  BStart.Y := FrameSize;
  BEnd.X := Width - FrameSize;
  BEnd.Y := Height - FrameSize;

  with BitMap.Canvas do
  begin
    Brush.Color := FrameColor;
    Pen.Color := FrameColor;
    Rectangle(0, 0, Width, Height);
    Brush.Color := BoardColor;
    Pen.Color := BoardColor;
    Rectangle(BStart.X, BStart.Y, BEnd.X, BEnd.Y);
  end;

  if FrameStyle = None then
    Exit;

  if FrameColorStyle = stWindows then
    C := ColorToRGB(clBtnFace)
  else
    C := ColorToRGB(FrameColor);

  R := RED(C);
  G := GREEN(C);
  B := BLUE(C);

  if FrameColorStyle = stWindows then
    Color1 := clWhite
  else
    Color1 := RGBToColor(Round(R / K1 + 200), Round(G / K1 + 200), Round(B / K1 + 200));

  Color2 := RGBToColor(Round(R / K2 + 68), Round(G / K2 + 68), Round(B / K2 + 68));
  Color3 := RGBToColor(Round(R / K3), Round(G / K3), Round(B / K3));

  if FrameHeight = double then
    Color4 := RGBToColor(Round(R / K4), Round(G / K4), Round(B / K4))
  else
    Color4 := Color3;

  FStart.X := 0;
  FStart.Y := 0;
  FEnd.X := Width - 1;
  FEnd.Y := Height - 1;

  BStart.X := FrameSize;
  BStart.Y := FrameSize;
  BEnd.X := Width - FrameSize - 1;
  BEnd.Y := Height - FrameSize - 1;

  if (FrameStyle = Raised) or (FrameStyle = Relief) then
  begin
    DrawShadow(FStart, FEnd, Color1, Color4);

    if FrameHeight = double then
    begin
      FStart.X := FStart.X + 1;
      FStart.Y := FStart.Y + 1;
      FEnd.X := FEnd.X - 1;
      FEnd.Y := FEnd.Y - 1;
      DrawShadow(FStart, FEnd, Color2, Color3);
    end;
  end;

  if (FrameStyle = Lowered) or (FrameStyle = Relief) then
  begin
    DrawShadow(BStart, BEnd, Color3, Color1);
    if FrameHeight = double then
    begin
      BStart.X := BStart.X + 1;
      BStart.Y := BStart.Y + 1;
      BEnd.X := BEnd.X - 1;
      BEnd.Y := BEnd.Y - 1;
      DrawShadow(BStart, BEnd, Color4, Color2);
    end;
  end;
end;
//*************************************************
procedure TplLCDLine.DrawShadow(StartP, EndP: TPoint; LineColor1, LineColor2: TColor);
begin
  with BitMap.Canvas do
  begin
    Pen.Color := LineColor1;
    MoveTo(EndP.X, StartP.Y);
    LineTo(StartP.X, StartP.Y);
    LineTo(StartP.X, EndP.Y);

    Pen.Color := LineColor2;
    MoveTo(EndP.X, StartP.Y);
    LineTo(EndP.X, EndP.Y);
    LineTo(StartP.X - 1, EndP.Y);
  end;
end;
//*********************************************

procedure TplLCDLine.DrawGrid();
var
  y, x: integer;
  NRow, NCol: integer;
begin

  NRow := FRowCount;
  NCol := (FColCount + 1) * FCharCount - 1;

  for y := 0 to NRow - 1 do
    for x := 0 to NCol - 1 do
      DrawCell(y, x, CellColorOff);

end;
//*************************************************

procedure TplLCDLine.DrawSpace();
var
  y, x: integer;
  NRow, NCol: integer;
  i: integer;
begin

  NRow := FRowCount;
  NCol := (FColCount + 1) * FCharCount - 1;

  for y := 0 to NRow - 1 do
  begin
    i := 0;
    for x := 0 to NCol - 1 do
    begin
      if i = 5 then
      begin
        DrawCell(y, x, BoardColor);
        i := 0;
      end
      else
        i := i + 1;
    end;
  end;

end;
//*************************************************

procedure TplLCDLine.DrawText();
var
  x, y, c: integer;
  CodChar: integer;
  CellRow: integer;
  CellOn: boolean;
begin

  FLenText := Length(Text);

  for c := 1 to FLenText do
  begin
    CodChar := Ord(Text[c]);

    for y := 0 to 6 do
    begin
      CellRow := CharAr[CodChar, y];
      for x := 0 to 4 do
      begin
        CellOn := CellRow and (1 shl (5 - x - 1)) > 0;
        if CellOn then
          DrawChar(y, x + FOffset, c);
      end;   // for x
    end;     // for y
  end;       // for c

  if CharSpace then
    DrawSpace();
end;
//***************************************************

procedure TplLCDLine.DrawChar(Row, Col, NChar: integer);
begin

  Col := Col + ((FColCount + 1) * (NChar - 1));
  if Col > FGlobalColCount - 1 then
    Exit;
  if Col < 0 then
    Exit;
  DrawCell(Row, Col, CellColorOn);

end;
//************************************************

procedure TplLCDLine.DrawCellsOn();
var
  x, y: integer;
  Col: integer;
begin

  for y := 0 to (FRowCount - 1) do
    for x := 0 to (FCountOn - 1) do
      if CellsOn[y, x] then
      begin
        Col := x + Offset;
        if (Col >= 0) and (Col < FGlobalColCount) then
          DrawCell(y, Col, CellColorOn);
      end;
end;
//*******************************************

procedure TplLCDLine.DrawCell(Row, Col: integer; ColorCell: TColor);
var
  CellR: TRect;
begin
  CellR.Left := FrameSize + FBoardWidth + (CellSize + CellSpace) * Col;
  CellR.Top := FrameSize + FBoardHeight + (CellSize + CellSpace) * Row;
  CellR.Right := CellR.Left + CellSize;
  CellR.Bottom := CellR.Top + CellSize;

  with BitMap.Canvas do
  begin
    Pen.Color := ColorCell;
    Brush.Color := ColorCell;

    if CellShape = stSquare then
      FillRect(CellR)
    else
      Ellipse(CellR.Left, CellR.Top, CellR.Right, CellR.Bottom);
  end;
end;
//*********************************************

procedure TplLCDLine.OutBorder();
begin
  Canvas.Draw(0, 0, BitMap);
end;
//********************************************

procedure TplLCDLine.Paint();
begin
  inherited;
  DrawCalc();
  DrawBorder();
  DrawGrid();
  DrawText();
  DrawCellsOn();
  OutBorder();
end;
//********************************************
procedure TplLCDLine.SetBoardColor(const Value: TColor);
begin
  if Value = BoardColor then
    Exit;
  FBoardColor := Value;
  invalidate;
end;
//********************************************
procedure TplLCDLine.SetCellColorOff(const Value: TColor);
begin
  if Value = CellColorOff then
    Exit;
  FCellColorOff := Value;
  invalidate;
end;
//********************************************
procedure TplLCDLine.SetCelColorlOn(const Value: TColor);
begin
  if Value = CellColorOff then
    Exit;
  FCellColorOn := Value;
  invalidate;
end;
//********************************************
procedure TplLCDLine.SetCellSize(const Value: integer);
begin
  if Value = CellSize then
    Exit;
  FCellSize := Value;
  invalidate;
end;
//**********************************************
procedure TplLCDLine.SetCellSpace(const Value: integer);
begin
  if Value = CellSpace then
    Exit;
  FCellSpace := Value;
  invalidate;
end;
//**********************************************
procedure TplLCDLine.SetCellShape(const Value: TCellShape);
begin
  if Value = CellShape then
    Exit;
  FCellShape := Value;
  invalidate;
end;
//**********************************************
procedure TplLCDLine.SetFrameColor(const Value: TColor);
begin
  if Value = FrameColor then
    Exit;
  FFrameColor := Value;
  invalidate;
end;
//************************************************
procedure TplLCDLine.SetFrameColorStyle(const Value: TFrameColorStyle);
begin
  if Value = FrameColorStyle then
    Exit;
  FFrameColorStyle := Value;
  invalidate;
end;
//**********************************************
procedure TplLCDLine.SetFrameHeight(const Value: TFrameHeight);
begin
  if Value = FrameHeight then
    Exit;
  FFrameHeight := Value;
  invalidate;
end;
//**********************************************
procedure TplLCDLine.SetFrameSize(const Value: integer);
begin
  if Value = FrameSize then
    Exit;
  FFrameSize := Value;
  invalidate;
end;
//**********************************************
procedure TplLCDLine.SetFrameStyle(const Value: TFrameStyle);
begin
  if Value = FrameStyle then
    Exit;
  FFrameStyle := Value;
  invalidate;
end;
//**********************************************
procedure TplLCDLine.SetCharSpace(const Value: boolean);
begin
  if Value = CharSpace then
    Exit;
  FCharSpace := Value;
  invalidate;
end;
//**********************************************
procedure TplLCDLine.SetOffset(const Value: longint);
begin
  if Value = Offset then
    Exit;
  FOffset := Value;
  invalidate;
end;
//**********************************************
procedure TplLCDLine.SetText(const Value: string);
begin
  if Value = Text then
    Exit;
  FText := Value;
  invalidate;
end;
//*********************************************
procedure TplLCDLine.SetLAutoSize(const Value: boolean);
begin
  if Value = AutoSize then
    Exit;
  FAutoSize := Value;
  invalidate;
end;
//*********************************************
function TplLCDLine.GetCharCount: longint;
begin
  DrawCalc();
  Result := FCharCount;
end;
//*********************************************
function TplLCDLine.GetGlobalColCount: longint;
begin
  DrawCalc();
  Result := FGlobalColCount;
end;
//**********************************************
function TplLCDLine.GetCellsOn(Row, Col: integer): boolean;
begin
  Result := FCellsOn[Row, Col];
end;
//**********************************************
procedure TplLCDLine.SetCellsOn(Row, Col: integer; const Value: boolean);
begin
  if (Row < 0) or (Row >= FRowCount) then
    raise Exception.Create('Index of a Row outside an array');
  if (Col < 0) or (Col >= FCountOn) then
    raise Exception.Create('Index of a Col outside an array');
  FCellsOn[Row, Col] := Value;
end;
//**********************************************

end.
