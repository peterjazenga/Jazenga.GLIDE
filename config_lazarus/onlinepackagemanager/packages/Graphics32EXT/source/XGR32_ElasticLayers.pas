
{**********************************************************************
 Package pl_Graphics32EXT.pkg
 this unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit XGR32_ElasticLayers;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType,
  Classes, SysUtils, Controls, Forms, Graphics, Math,
  GR32, GR32_Image, GR32_Blend, GR32_LowLevel, GR32_Math,
  GR32_Bindings, GR32_Layers, GR32_Transforms, GR32_Polygons,
  GR32_Resamplers,
  XGR32_ExtLayers_cursors, XGR32_Blendmodes;

const
  // These states can be entered by the rubber band layer.
  {
    WIND DIRECTION:         INDEX:            normal cursor:     rotated cursor:
                                                  ^                   ^
    NW    N   NE          0   4   1           \   ^   /             \   E
    W     C   E           7   8   5           <-W   E->          <-       ->
    SW    S   SE          3   6   2           /   v   \             W   \
                                                  v                   v
   9 CELLS:
   --------------
      0   1   2
      3   4   5
      6   7   8
  }
  ZoneToClockwiseIndex: array[0..8] of integer = (0, 4, 1, 7, 8, 5, 3, 6, 2);

type

  TTicDragState = (
    tdsMoveLayer, tdsMovePivot,
    tdsResizeCorner, tdsRotate,
    tdsResizeSide, tdsSkew,
    tdsDistortion, tdsPerspective,
    tdsNone
    );
  TCursorDirection = (
    cdNotUsed,
    cdNorthWest,
    cdNorth,
    cdNorthEast,
    cdEast,
    cdSouthEast,
    cdSouth,
    cdSouthWest,
    cdWest
    );

  TExtRubberBandOptions = set of (
    rboAllowPivotMove,
    rboAllowCornerResize,
    rboAllowEdgeResize,
    rboAllowMove,
    rboAllowRotation,
    rboShowFrame,
    rboShowHandles
    );

const
  DefaultRubberbandOptions = [rboAllowCornerResize, rboAllowEdgeResize,
                              rboAllowMove, rboAllowRotation, rboShowFrame, rboShowHandles];

type

  TTicTransformation = class;

  TElasticLayer = class(TCustomLayer)
  private
    FScaled: boolean;
    FCropped: boolean;
    function GetTic(index: integer): TFloatPoint;
    procedure SetTic(index: integer; const Value: TFloatPoint);
    procedure SetScaled(const Value: boolean);
    procedure SetCropped(const Value: boolean);

    procedure SetEdges(const Value: TArrayOfFloatPoint);
    function GetSourceRect: TFloatRect;
    procedure SetSourceRect(const Value: TFloatRect);
    function GetEdges: TArrayOfFloatPoint;
  protected
    FTransformation: TTicTransformation ;
    FInViewPortTransformation: TTicTransformation;
    procedure DoSetEdges(const Value: TArrayOfFloatPoint); virtual;

  public
    constructor Create(ALayerCollection: TLayerCollection); override;
    destructor Destroy; override;

    function GetScaledRect(const R: TFloatRect): TFloatRect; virtual;
    function GetScaledEdges: TArrayOfFloatPoint;
    procedure SetBounds(APosition: TFloatPoint; ASize: TFloatPoint); overload;
    procedure SetBounds(ABoundsRect: TFloatRect); overload;

    property Tic[index: integer]: TFloatPoint read GetTic write SetTic;
    property SourceRect: TFloatRect read GetSourceRect write SetSourceRect;
  published
    property Edges: TArrayOfFloatPoint read GetEdges write SetEdges;
    property Scaled: boolean read FScaled write SetScaled;
    property Cropped: boolean read FCropped write SetCropped;
  end;


  TElasticBitmapLayer = class(TElasticLayer)
  private
    FBitmap: TBitmap32;
    FBlendMode: TBlendMode32;
    procedure BitmapChanged(Sender: TObject);
    procedure SetBitmap(const Value: TBitmap32);
    procedure SetBlendMode(const Value: TBlendMode32);
  protected
    function DoHitTest(X, Y: integer): boolean; override;
    procedure Paint(Buffer: TBitmap32); override;
  public
    constructor Create(ALayerCollection: TLayerCollection); override;
    destructor Destroy; override;
  published
    property Bitmap: TBitmap32 read FBitmap write SetBitmap;
    property BlendMode: TBlendMode32 read FBlendMode write SetBlendMode;
  end;

  TElasticRubberBandLayer = class(TElasticLayer)
  private
    FChildLayer: TElasticLayer;
    FIsDragging: boolean;
    FOldEdges: TArrayOfFloatPoint;
    FDragState: TTicDragState;
    FCompass: integer;
    FDragPos: TPoint;
    FOriginDragPos, FOldOriginPivot, FOldInViewPortPivot, FOldTransformedPivot: TFloatPoint;
    FMouseOverPos: TPoint;
    FThreshold: integer;
    FPivotPoint: TFloatPoint;
    FOptions: TExtRubberBandOptions;
    FHandleSize: integer;
    FHandleFrame: TColor;
    FHandleFill: TColor;

    procedure SetChildLayer(const Value: TElasticLayer);
    procedure SetOptions(const Value: TExtRubberBandOptions);
    procedure SetHandleFill(const Value: TColor);
    procedure SetHandleFrame(const Value: TColor);
    procedure SetHandleSize(Value: integer);
    procedure SetPivotOrigin(Value: TFloatPoint);
  protected
    procedure DoSetEdges(const Value: TArrayOfFloatPoint); override;
    function GetRotatedCompass(LocalCompas: integer): integer;
    function GetPivotOrigin: TFloatPoint;
    function GetPivotTransformed: TFloatPoint;
    function GetRotatedEdges(AEdges: TArrayOfFloatPoint; dx, dy: TFloat): TArrayOfFloatPoint;

    procedure Paint(Buffer: TBitmap32); override;
    procedure SetLayerOptions(Value: cardinal); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
  public
    constructor Create(ALayerCollection: TLayerCollection); override;
    property ChildLayer: TElasticLayer read FChildLayer write SetChildLayer;
    property Options: TExtRubberBandOptions read FOptions write SetOptions default DefaultRubberbandOptions;
    property HandleSize: integer read FHandleSize write SetHandleSize default 3;
    property HandleFill: TColor read FHandleFill write SetHandleFill default clWhite;
    property HandleFrame: TColor read FHandleFrame write SetHandleFrame default clBlack;

    property PivotPoint: TFloatPoint read FPivotPoint write FPivotPoint;
    property Threshold: integer read FThreshold write FThreshold default 8;

  end;

  TTicTransformation = class(T3x3Transformation)
  private
    FEdges: TArrayOfFloatPoint;
    FMiddleEdges: TArrayOfFloatPoint;
    FMiddleEdgesValid: boolean;
    procedure SetEdges(const Value: TArrayOfFloatPoint);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure PrepareTransform; override;
    procedure ReverseTransformFixed(DstX, DstY: TFixed; out SrcX, SrcY: TFixed); override;
    procedure ReverseTransformFloat(DstX, DstY: TFloat; out SrcX, SrcY: TFloat); override;
    procedure TransformFixed(SrcX, SrcY: TFixed; out DstX, DstY: TFixed); override;
    procedure TransformFloat(SrcX, SrcY: TFloat; out DstX, DstY: TFloat); override;
  public
    constructor Create; virtual;
    function GetTransformedBounds(const ASrcRect: TFloatRect): TFloatRect; override;
    function GetMiddleEdges: TArrayOfFloatPoint;
    property Edges: TArrayOfFloatPoint read FEdges write SetEdges;
  end;


implementation

type
  TLayerCollectionAccess = class(TLayerCollection);
  TImage32Access = class(TCustomImage32);

var
  UPivotBitmap: TBitmap32 = nil;

function GetPivotBitmap: TBitmap32;
begin
  if not Assigned(UPivotBitmap) then
  begin
    UPivotBitmap := TBitmap32.Create;
    UPivotBitmap.SetSize(16, 16);
    UPivotBitmap.Clear(0);
    UPivotBitmap.DrawMode := dmBlend;
           { TODO : Stef fix This }
   // DrawIcon(UPivotBitmap.Handle, 0, 0, Screen.Cursors[crGrCircleCross], 0, 0, 0, 0, DI_NORMAL);
  end;

  Result := UPivotBitmap;
end;

function SafelyGetEdgeIndex(AIndex: integer): integer;
begin
  Result := AIndex;

  while Result < 0 do
    Inc(Result, 4);
  while Result > 3 do
    Dec(Result, 4);
end;

function EdgesToFloatRect(AEdges: TArrayOfFloatPoint): TFloatRect;
begin
  Result.Left := Min(Min(AEdges[0].X, AEdges[1].X), Min(AEdges[2].X, AEdges[3].X));
  Result.Right := Max(Max(AEdges[0].X, AEdges[1].X), Max(AEdges[2].X, AEdges[3].X));
  Result.Top := Min(Min(AEdges[0].Y, AEdges[1].Y), Min(AEdges[2].Y, AEdges[3].Y));
  Result.Bottom := Max(Max(AEdges[0].Y, AEdges[1].Y), Max(AEdges[2].Y, AEdges[3].Y));
end;


function MiddlePointOf2Lines(const x1, x2: TFloat; divo: integer = 2; mulo: integer = 1): TFloat; overload;
begin

  if x1 < x2 then
    Result := x1 + (x2 - x1) / divo * mulo
  else
    Result := x2 + (x1 - x2) / divo * mulo;

end;

function MiddlePointOf2Lines(const x1, y1, x2, y2: TFloat; divo: integer = 2; mulo: integer = 1): TFloatPoint; overload;
begin

  if x1 < x2 then
    Result.X := x1 + (x2 - x1) / divo * mulo
  else
    Result.X := x2 + (x1 - x2) / divo * mulo;

  if y1 < y2 then
    Result.Y := y1 + (y2 - y1) / divo * mulo
  else
    Result.Y := y2 + (y1 - y2) / divo * mulo;
end;

function MiddlePointOf2Lines(const P1, P2: TFloatPoint; divo: integer = 2; mulo: integer = 1): TFloatPoint; overload;
begin
  Result := MiddlePointOf2Lines(P1.X, P1.Y, P2.X, P2.Y, divo, mulo);
end;

function CentroidOf(AEdges: TArrayOfFloatPoint): TFloatPoint;
begin
  with EdgesToFloatRect(AEdges) do
  begin
    Result.X := Left + (Right - Left) / 2;
    Result.Y := Top + (Bottom - Top) / 2;
  end;
end;

function CenterOf(MiddleEdges: TArrayOfFloatPoint): TFloatPoint;
begin
  Result.X := MiddlePointOf2Lines(MiddleEdges[0].X, MiddleEdges[2].X);
  Result.Y := MiddlePointOf2Lines(MiddleEdges[1].Y, MiddleEdges[3].Y);
end;

function DegreeOfBuggy(MidPoint, OppositePoint: TFloatPoint): TFloat;
var
  Radians: TFloat;
begin
  Radians := ArcTan2(OppositePoint.Y - MidPoint.Y, MidPoint.X - OppositePoint.X);
  Result := RadToDeg(Radians);
end;

function DegreeOf(Center, OppositePoint: TFloatPoint): TFloat;
var
  Radians: TFloat;
begin
  Radians := ArcTan2(Center.Y - OppositePoint.Y, OppositePoint.X - Center.X);

  Result := RadToDeg(Radians);
  while Result < 0 do
    Result := Result + 360;
  while Result > 360 do
    Result := Result - 360;
end;

function PtIn9Zones(P: TFloatPoint; TT: TTicTransformation; var MouseInside: boolean): integer; overload;
const
  FTreshold: TFloat = 8;
var
  Mids, LEdges: TArrayOfFloatPoint;
  Xdeg, Ydeg, prevDeg, nextDeg, dx, dy, compassDeg, half, pDeg: TFloat;
  X, Y, Center: TFloatPoint;
  AT: TAffineTransformation;
  prev, Next, cx, cy, Zone, Compass: integer;
begin
  AT := TAffineTransformation.Create;
  Mids := TT.GetMiddleEdges;
  LEdges := TT.Edges;
  Xdeg := DegreeOf(Mids[3], Mids[1]);

  AT.Rotate(Mids[3].X, Mids[3].Y, -Xdeg);
  Y := AT.Transform(P);
  Dy := Mids[3].Y - Y.Y;
  if Abs(dy) <= FTreshold then
    cy := 1
  else
  if Dy > 0 then
    cy := 0
  else
    cy := 2;

  Ydeg := DegreeOf(Mids[2], Mids[0]);

  AT.Clear;
  AT.Rotate(Mids[2].X, Mids[2].Y, -Ydeg);
  X := AT.Transform(P);
  Dx := Mids[2].Y - X.Y;
  if Abs(Dx) <= FTreshold then
    cx := 1
  else
  if Dx > 0 then
    cx := 0
  else
    cx := 2;

  // get index in: left-to-right-then-bottom order
  Zone := cx + cy * 3;
  // get index
  Compass := ZoneToClockwiseIndex[Zone];

  Result := Compass;
  MouseInside := PtInRect(TT.SrcRect, TT.ReverseTransform(P));

  // I am worry only about the range of Edges; its too wide.
  // It also report as "false positive" due the middleEdges is too narrow,
  // while calculated by FTreshold. So, this is the correction
  if Compass <= 3 then
  begin
    Center := CenterOf(Mids);
    pDeg := Abs(DegreeOf(Center, P));
    compassDeg := Abs(DegreeOf(Center, LEdges[Compass]));


    if pDeg > compassDeg then
    begin

      // compas = edge; prev&next = mid
      prev := SafelyGetEdgeIndex(Compass - 1);
      prevDeg := Abs(DegreeOf(Center, Mids[prev]));
      half := Abs(compassDeg - prevDeg) / 2;

      if pDeg > compassDeg + half then
        Result := prev + 4;

    end
    else
    begin
      Next := SafelyGetEdgeIndex(Compass);
      nextDeg := Abs(DegreeOf(Center, Mids[Next]));

      half := Abs(compassDeg - nextDeg) / 2;

      if pDeg < nextDeg + half then
      begin
        Result := Next + 4;
      end;
    end;

  end;

end;


function PtIn9ZonesBuggy(P: TPoint; SrcRect: TRect; var MouseInside: boolean): integer; overload;

  // Non rotated world, cells clamped to range of [0..2]
  {   0   1   2
      3   4   5
      6   7   8   }
var
  W, H, X, Y, cx, cy: integer;
  dx, dy: TFloat;
begin

  // get bounds of whole grid
  with SrcRect do
  begin
    W := Right - Left;
    H := Bottom - Top;

    // in case Transformation.SrcRect.TopLeft < Point(0,0)
    X := P.X - Left;
    Y := P.Y - Top;
  end;

  // get bounds of a cell.
  // Precisely. don't div here
  dx := W / 3;
  dy := H / 3;

  //get cell contained XY
  cx := Floor(X / dx);
  cy := Floor(Y / dy);

  //detect wether mouse in rect
  MouseInside := (cx in [0..2]) and (cy in [0..2]);

  cx := Clamp(cx, 0, 2);
  cy := Clamp(cy, 0, 2);

  // get index in: left-to-right-then-bottom order
  Result := cx + cy * 3;
end;


function LineDistance(const A, B: TFloatPoint; UsingRadius: boolean = False): TFloat;
var
  i, j, c: TFloat;
begin
  i := A.X - B.X;
  j := A.Y - B.Y;
  if i < 0 then
    i := i * -1;
  if j < 0 then
    j := j * -1;
  if UsingRadius then
  begin
    c := sqr(i) + sqr(j);
    //if c > 0 then
    Result := sqrt(c);
    //else
    //Result := 0;
  end
  else
    Result := Max(i, j);
end;


procedure IncF(var P: TFloatPoint; dx, dy: TFloat); overload;
begin
  P.X := P.X + dx;
  P.Y := P.Y + dy;
end;

function IncOf(APointF: TFloatPoint; dx, dy: TFloat): TFloatPoint;
begin
  Result.X := APointF.X + dx;
  Result.Y := APointF.Y + dy;
end;

function MoveEdges(AEdges: TArrayOfFloatPoint; dx, dy: TFloat): TArrayOfFloatPoint;
var
  i: integer;
begin
  Result := Copy(AEdges, 0, 4);

  for i := 0 to 3 do
  begin
    Result[i].X := Result[i].X + dx;
    Result[i].Y := Result[i].Y + dy;
  end;
end;

function SlopeOf(I, Opposite: TFloatPoint): TFloatPoint;
begin
  Result := FloatPoint(I.X - Opposite.X, I.Y - Opposite.Y);
end;

function Straight90degreeAt(MidPoint, OppositePoint, MousePoint: TFloatPoint): TFloatPoint;
var
  Radians, Angle: TFloat;
  hypotenuse, opposite, adjacent: TFloat;
  TT: TAffineTransformation;
  M: TFloatPoint;
begin
  Result := MousePoint;

  Radians := ArcTan2(OppositePoint.Y - MidPoint.Y, MidPoint.X - OppositePoint.X);
  Angle := RadToDeg(Radians);

  with OppositePoint do
    //inc(MousePoint, -X, -Y);
    MousePoint := IncOf(MousePoint, -X, -Y);


  TT := TAffineTransformation.Create;
  try
    TT.Rotate(-Angle);

    M := TT.Transform(MousePoint);
    M.Y := 0;

    TT.Clear;
    TT.Rotate(Angle);
    TT.Translate(OppositePoint.X, OppositePoint.Y);
    Result := TT.Transform(M);
    //Result := M;
  finally
    TT.Free;
  end;
end;

function StraightPointWithTailDegrees(MidPoint, OppositePoint, TailPoint, MousePoint: TFloatPoint): TFloatPoint;
  // P is the point of intersection
  function Intersect(const A1, A2, B1, B2: TFloatPoint; out P: TFloatPoint): boolean;
  var
    Adx, Ady, Bdx, Bdy, ABy, ABx: TFloat;
    t, ta, tb: TFloat;
  begin
    Result := False;
    Adx := A2.X - A1.X;
    Ady := A2.Y - A1.Y;
    Bdx := B2.X - B1.X;
    Bdy := B2.Y - B1.Y;
    t := (Bdy * Adx) - (Bdx * Ady);

    if t = 0 then
      Exit; // lines are parallell

    ABx := A1.X - B1.X;
    ABy := A1.Y - B1.Y;
    ta := Bdx * ABy - Bdy * ABx;
    tb := Adx * ABy - Ady * ABx;
    //  if InSignedRange(ta, 0, t) and InSignedRange(tb, 0, t) then
    begin
      Result := True;
      ta := ta / t;
      P.X := A1.X + ta * Adx;
      P.Y := A1.Y + ta * Ady;
    end;
  end;

var
  Radians, Angle, Angle1, Angle2: TFloat;
  hypotenuse, opposite, adjacent: TFloat;
  TT: TAffineTransformation;
  M, M1, M2, distance: TFloatPoint;
begin
  Radians := ArcTan2(OppositePoint.Y - MidPoint.Y, MidPoint.X - OppositePoint.X);
  Angle1 := RadToDeg(Radians);

  Radians := ArcTan2(TailPoint.Y - MidPoint.Y, MidPoint.X - TailPoint.X);
  Angle2 := RadToDeg(Radians) - 90;

  with MidPoint do
    M := IncOf(MousePoint, -X, -Y);

  TT := TAffineTransformation.Create;
  try

    TT.Clear;
    TT.Rotate(0, 0, -Angle2);
    M2 := TT.Transform(M);
    M2.Y := 0;

    TT.Clear;
    TT.Rotate(0, 0, Angle2);
    TT.Translate(MidPoint.X, MidPoint.Y);
    M2 := TT.Transform(M2); // X oke here

    //Result := M1;
    if not Intersect(MousePoint, M2, MidPoint, OppositePoint, Result) then
      //Result := FloatPoint(M2.Y, M1.Y);
    begin
      TT.Clear;
      TT.Rotate(0, 0, -Angle1);
      M1 := TT.Transform(M);
      M1.Y := 0;

      TT.Clear;
      TT.Rotate(0, 0, Angle1);
      TT.Translate(MidPoint.X, MidPoint.Y);
      M1 := TT.Transform(M1);

      Result := M1;
    end;
  finally
    TT.Free;
  end;
end;

function Move2Edges(AEdges: TArrayOfFloatPoint; Start: integer; dx, dy: TFloat): TArrayOfFloatPoint;
var
  i, s: integer;
begin
  Result := Copy(AEdges, 0, 4);

  for i := Start to Start + 1 do
  begin
    s := SafelyGetEdgeIndex(i);
    IncF(Result[s], dx, dy);
  end;
end;

function ResizeBySide(AEdges: TArrayOfFloatPoint; Start: integer; dx, dy: TFloat; AStraight: boolean): TArrayOfFloatPoint;

  // RESIZE

var
  i, k: integer;
  m1, m2, mid, ops, pair, needed, adjusted: TFloatPoint;
  ax, ay: TFloat;
begin

  if Odd(Start) then
    // locally horizontal resize
  begin
    m1 := MiddlePointOf2Lines(Aedges[0], AEdges[3]);
    m2 := MiddlePointOf2Lines(Aedges[1], AEdges[2]);
  end
  else
    // locally vertical resize
  begin
    m1 := MiddlePointOf2Lines(Aedges[0], AEdges[1]);
    m2 := MiddlePointOf2Lines(Aedges[2], AEdges[3]);
  end;

  if Start in [1, 2] then
  begin
    mid := m2;
    ops := m1;
  end
  else  //start = 3
  begin
    mid := m1;
    ops := m2;
  end;

  needed := IncOf(mid, dx, dy);

  if AStraight then
  begin
    //adjusted := StraightLinePointAt(mid, ops, needed);
    adjusted := StraightPointWithTailDegrees(mid, ops, AEdges[start], needed);
  end
  else
  begin
    adjusted := needed;
  end;

  dx := adjusted.X - mid.X;
  dy := adjusted.Y - mid.Y;

  Result := Move2Edges(AEdges, Start, dx, dy);

end;

function SkewBySide(AEdges: TArrayOfFloatPoint; Start: integer; dx, dy: TFloat; AStraight: boolean): TArrayOfFloatPoint;

  // SKEW

var
  pair, i, k: integer;
  mid, opposite, needed, adjusted: TFloatPoint;
begin
  pair := SafelyGetEdgeIndex(Start + 1);
  mid := MiddlePointOf2Lines(AEdges[start], AEdges[pair]);
  opposite := AEdges[pair];

  needed := IncOf(mid, dx, dy);

  if AStraight then
  begin
    adjusted := Straight90degreeAt(mid, opposite, needed);
  end
  else
  begin
    adjusted := needed;
  end;

  dx := adjusted.X - mid.X;
  dy := adjusted.Y - mid.Y;

  Result := Move2Edges(AEdges, Start, dx, dy);
end;

function MirrorPoint(const P, Axis: TFloatPoint): TFloatPoint;
var
  ZeroP: TFloatPoint;
begin
  ZeroP := IncOf(P, -Axis.X, -Axis.Y); // get distance

  Result := IncOf(Axis, -ZeroP.X, -ZeroP.Y);
end;

function PerspectiveByCorner(AEdges: TArrayOfFloatPoint; Start: integer; dx, dy: TFloat; AStraight: boolean): TArrayOfFloatPoint;

  // PERSPECTIVE

var
  pair, iPrev, iNext, i, k: integer;
  mid, node, prev, Next, draggedPrev, draggedNext, opposite, needed, adjusted: TFloatPoint;
begin
  Result := Copy(AEdges, 0, 4);

  iPrev := SafelyGetEdgeIndex(Start - 1);
  iNext := SafelyGetEdgeIndex(Start + 1);

  node := AEdges[start];
  prev := AEdges[iPrev];
  Next := Aedges[iNext];

  needed := IncOf(node, dx, dy);

  draggedPrev := Straight90degreeAt(node, prev, needed);
  draggedNext := Straight90degreeAt(node, Next, needed);

  // which XY is dragged further ?
  if LineDistance(node, draggedPrev, True) > LineDistance(node, draggedNext, True) then
  begin
    Result[Start] := draggedPrev;
    mid := MiddlePointOf2Lines(AEdges[start], AEdges[iPrev]);
    Result[iPrev] := MirrorPoint(Result[Start], mid);
    //    reduction := FloatPoint()
  end
  else
  begin
    Result[Start] := draggedNext;
    mid := MiddlePointOf2Lines(AEdges[start], AEdges[iNext]);
    Result[iNext] := MirrorPoint(Result[Start], mid);
  end;
end;

function SkewBySideBuggy(AEdges: TArrayOfFloatPoint; Start: integer; dx, dy: TFloat): TArrayOfFloatPoint;

  // SKEW

var
  opposite, i, k: integer;
  slope: TFloatPoint;
begin
  opposite := SafelyGetEdgeIndex(Start + 1);

  // calc slope
  slope := SlopeOf(AEdges[Opposite], AEdges[Start]);




  if Odd(Start) then  // locally vertical skew
  begin
    with slope do
      //TODO : what if ax = 0 ?
      dx := dx + (x / y) * dy;
  end
  else
  begin                // locally horizontal skew
    with slope do
      //TODO : what if ay = 0 ?
      dy := dy + (y / x) * dx;
  end;




  Result := Move2Edges(AEdges, Start, dx, dy);
end;

function Move2CornersBuggy(AEdges: TArrayOfFloatPoint; Start: integer; dx, dy: TFloat): TArrayOfFloatPoint;

  // RESIZE

var
  i, k: integer;
  m1, m2, mid, ops, needed, adjusted: TFloatPoint;
  ax, ay: TFloat;
begin
  Result := Copy(AEdges, 0, 4);

  //find the opposite side
  if Odd(Start) then
    // locally horizontal resize
  begin
    m1 := MiddlePointOf2Lines(Aedges[0], AEdges[3]);
    m2 := MiddlePointOf2Lines(Aedges[1], AEdges[2]);

    if Start = 1 then
    begin
      mid := m2;
      ops := m1;
    end
    else
    begin
      mid := m1;
      ops := m2;
    end;

    needed := IncOf(AEdges[start], dx, dy);
    adjusted := Straight90degreeAt(mid, ops, needed);
    dx := adjusted.X - mid.X;
    dy := adjusted.Y - mid.Y;



    // calc slope
    ax := (m1.X - m2.X);
    ay := (m1.Y - m2.Y);

    //TODO : what if ax = 0 ?
    dy := dy + (ay / ax) * dx;
  end
  else
    // locally vertical resize
  begin
    m1 := MiddlePointOf2Lines(Aedges[0], AEdges[1]);
    m2 := MiddlePointOf2Lines(Aedges[2], AEdges[3]);

    // calc slope
    ax := (m1.X - m2.X);
    ay := (m1.Y - m2.Y);

    //TODO : what if ay = 0 ?
    dx := dx + (ax / ay) * dy;
  end;

  for k := Start to Start + 1 do
  begin
    i := k;
    if i > 3 then i := 0;
    IncF(Result[i], dx, dy);
  end;
end;

function ResizeByCorner(Sender: TElasticLayer; AEdges: TArrayOfFloatPoint; Mid: integer; dx, dy: TFloat;
                        Straight, OddCompass: boolean): TArrayOfFloatPoint;
var
  LEdges: TArrayOfFloatPoint;
  function RatioOf(I, Opposite: integer): TFloatPoint;
  begin
    Result := FloatPoint(LEdges[I].X - LEdges[Opposite].X, LEdges[I].Y - LEdges[Opposite].Y);
  end;

var
  I, Prev, Next, Opposite: integer;
  MidPoint, MidPointOrigin, newMidPointOrigin, MousePoint, newMidPoint, MidPointRotatedOrigin, newMidPointRotatedOrigin,
  MidPointRotated, newMidPointRotated, OppositeOrigin, pivotBefore, pivotAfter, Scale, slopePrev, slopeNext,
  slopeMid, d, ScaleRatio, slopeMouse, mouseInSlope: TFloatPoint;
  diagonalScale, Radius, Ratio, newRadius, A, B, C, R: TFloat;
  TT: TTicTransformation;

  Affine: TAffineTransformation;
  Radians, Angle1, NewAngle: TFloat;

begin
  LEdges := Copy(AEdges, 0, 4);

  MidPoint := Ledges[Mid];
  Prev := SafelyGetEdgeIndex(Mid - 1);
  Next := SafelyGetEdgeIndex(Mid + 1);
  Opposite := SafelyGetEdgeIndex(Mid - 2);

  Radius := LineDistance(Ledges[Mid], LEdges[Opposite]);

  pivotBefore := MiddlePointOf2Lines(Ledges[Mid], LEdges[Opposite]);
  slopePrev := SlopeOf(LEdges[Prev], pivotBefore);
  slopeNext := SlopeOf(LEdges[Next], pivotBefore);

  Radians := ArcTan2(LEdges[Opposite].Y - Ledges[Mid].Y, Ledges[Mid].X - LEdges[Opposite].X);
  Angle1 := RadToDeg(Radians);

  newMidPoint := IncOf(LEdges[Mid], dx, dy);
  if Straight then
  begin
    newMidPoint := Straight90degreeAt(MidPoint, Ledges[Opposite], newMidPoint);
  end;


  newRadius := LineDistance(newMidPoint, LEdges[Opposite]);

  Radians := ArcTan2(LEdges[Opposite].Y - newMidPoint.Y, newMidPoint.X - LEdges[Opposite].X);
  NewAngle := RadToDeg(Radians);

  Affine := TAffineTransformation.Create;

  Affine.Translate(-LEdges[Opposite].X, -LEdges[Opposite].Y);

  diagonalScale := newRadius / Radius;
  TT := TTicTransformation.Create;
  TT.Assign(Sender.FTransformation);
  MidPointOrigin := TT.ReverseTransform(Ledges[Mid]);
  newMidPointOrigin := TT.ReverseTransform(newMidPoint);
  OppositeOrigin := TT.ReverseTransform(LEdges[Opposite]);

  ScaleRatio := FloatPoint(Abs(newMidPointOrigin.X - OppositeOrigin.X) / Abs(MidPointOrigin.X - OppositeOrigin.X),
    Abs(newMidPointOrigin.Y - OppositeOrigin.Y) / Abs(MidPointOrigin.Y - OppositeOrigin.Y));
  ScaleRatio := FloatPoint(Abs(newMidPoint.X - LEdges[Opposite].X) / Abs(Ledges[Mid].X - LEdges[Opposite].X),
    Abs(newMidPoint.Y - LEdges[Opposite].Y) / Abs(Ledges[Mid].Y - LEdges[Opposite].Y));

  Affine.Rotate(0, 0, NewAngle - Angle1);


  MidPointRotated := Affine.Transform(MidPoint);
  newMidPointRotated := FloatPoint(newMidPoint.X - LEdges[Opposite].X, newMidPoint.Y - LEdges[Opposite].Y);


  ScaleRatio := FloatPoint(Abs(newMidPointRotated.X / MidPointRotated.X), Abs(newMidPointRotated.Y / MidPointRotated.Y));

  Affine.Scale(ScaleRatio.X, ScaleRatio.Y);

  Affine.Translate(LEdges[Opposite].X, LEdges[Opposite].Y);

  for i := 0 to 3 do
  begin
    LEdges[i] := Affine.Transform(Ledges[i]);
  end;

  Result := LEdges;
end;

function Move3EdgesBuggy(AEdges: TArrayOfFloatPoint; Mid: integer; dx, dy: TFloat; Straight, OddCompass: boolean): TArrayOfFloatPoint;
var
  LEdges: TArrayOfFloatPoint;

  function RatioOf(I, Opposite: integer): TFloatPoint;
  begin
    Result := FloatPoint(LEdges[I].X - LEdges[Opposite].X, LEdges[I].Y - LEdges[Opposite].Y);
  end;

var
  Prev, Next, Opposite: integer;
  MousePoint, pivotBefore, pivotAfter, slopePrev, slopeNext, slopeMid, slopeMouse, mouseInSlope: TFloatPoint;
  Angle, Radius, Ratio, newRadius, A, B, C, R: TFloat;
begin
  LEdges := Copy(AEdges, 0, 4);

  Prev := SafelyGetEdgeIndex(Mid - 1);
  Next := SafelyGetEdgeIndex(Mid + 1);
  Opposite := SafelyGetEdgeIndex(Mid - 2);

  Radius := LineDistance(Ledges[Mid], LEdges[Opposite]);

  pivotBefore := MiddlePointOf2Lines(Ledges[Mid], LEdges[Opposite]);
  slopePrev := SlopeOf(LEdges[Prev], pivotBefore);
  slopeNext := SlopeOf(LEdges[Next], pivotBefore);

  if Straight then
  begin

    MousePoint := Ledges[Mid];
    IncF(MousePoint, dx, dy);

    slopeMid := SlopeOf(Ledges[Mid], LEdges[Opposite]);
    with slopeMid do
    begin

      mouseInSlope.Y := MousePoint.Y;
      mouseInSlope.X := X / Y * MousePoint.Y;
      Angle := ArcTan2(Y, X);
      C := LineDistance(mouseInSlope, MousePoint, True);
      A := Cos(Angle) * C;

      dx := mouseInSlope.X + A * Cos(Angle);
      dy := mouseInSlope.Y + A * Sin(Angle);

      LEdges[Mid].X := mouseInSlope.X + Cos(Angle) * C;
      LEdges[Mid].Y := mouseInSlope.Y + Sin(Angle) * C;

    end;
  end
  else
    IncF(LEdges[Mid], dx, dy);

  newRadius := LineDistance(Ledges[Mid], LEdges[Opposite]);
  Ratio := newRadius / Radius;

  pivotAfter := MiddlePointOf2Lines(Ledges[Mid], LEdges[Opposite]);

  //PREV
  LEdges[Prev].X := pivotAfter.X + slopePrev.X * Ratio;
  LEdges[Prev].Y := pivotAfter.Y + slopePrev.Y * Ratio;

  //NEXT
  LEdges[Next].X := pivotAfter.X + slopeNext.X * Ratio;
  LEdges[Next].Y := pivotAfter.Y + slopeNext.Y * Ratio;

  Result := LEdges;
end;

function MostLeftEdge(AEdges: TArrayOfFloatPoint): integer;
var
  i: integer;
begin
  Result := 0;
  for i := Length(AEdges) - 1 downto 1 do
  begin
    if AEdges[i].X < AEdges[0].X then
      Result := i;
  end;
end;

//============================= TElasticLayer ==========================================

constructor TElasticLayer.Create(ALayerCollection: TLayerCollection);
begin
  inherited;
  LayerOptions := LOB_VISIBLE or LOB_MOUSE_EVENTS;
  FTransformation := TTicTransformation.Create;
  FInViewPortTransformation := TTicTransformation.Create;

end;

destructor TElasticLayer.Destroy;
begin
  FTransformation.Free;
  inherited;
end;

procedure TElasticLayer.DoSetEdges(const Value: TArrayOfFloatPoint);
begin
  FTransformation.Edges := Value;
end;

function TElasticLayer.GetScaledEdges: TArrayOfFloatPoint;
var
  ScaleX, ScaleY, ShiftX, ShiftY: TFloat;
  i: integer;
begin
  Result := Copy(Edges, 0, 4);

  if Scaled and Assigned(LayerCollection) then
  begin
    LayerCollection.GetViewportShift(ShiftX, ShiftY);
    LayerCollection.GetViewportScale(ScaleX, ScaleY);

    for i := 0 to Length(Result) - 1 do
    begin
      Result[i].X := Result[i].X * ScaleX + ShiftX;
      Result[i].Y := Result[i].Y * ScaleY + ShiftY;
    end;
  end;
end;

function TElasticLayer.GetScaledRect(const R: TFloatRect): TFloatRect;
var
  ScaleX, ScaleY, ShiftX, ShiftY: TFloat;
begin
  if Scaled and Assigned(LayerCollection) then
  begin
    LayerCollection.GetViewportShift(ShiftX, ShiftY);
    LayerCollection.GetViewportScale(ScaleX, ScaleY);

    with Result do
    begin
      Left := R.Left * ScaleX + ShiftX;
      Top := R.Top * ScaleY + ShiftY;
      Right := R.Right * ScaleX + ShiftX;
      Bottom := R.Bottom * ScaleY + ShiftY;
    end;
  end
  else
    Result := R;
end;

function TElasticLayer.GetEdges: TArrayOfFloatPoint;
begin
  Result := FTransformation.Edges;
end;

function TElasticLayer.GetSourceRect: TFloatRect;
begin
  Result := FTransformation.SrcRect;
end;

function TElasticLayer.GetTic(index: integer): TFloatPoint;
begin
  Result := Edges[index];
end;


procedure TElasticLayer.SetBounds(APosition, ASize: TFloatPoint);
begin
  SetBounds(FloatRect(APosition, FloatPoint(APosition.X + ASize.X, APosition.Y + ASize.Y)));
end;

procedure TElasticLayer.SetBounds(ABoundsRect: TFloatRect);
begin
  BeginUpdate;
  try
    with ABoundsRect do
    begin
      Tic[0] := TopLeft;
      Tic[1] := FloatPoint(Right, Top);
      Tic[2] := BottomRight;
      Tic[3] := FloatPoint(Left, Bottom);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TElasticLayer.SetCropped(const Value: boolean);
begin
  if Value <> FCropped then
  begin
    FCropped := Value;
    Changed;
  end;
end;

procedure TElasticLayer.SetEdges(const Value: TArrayOfFloatPoint);
begin
  if Edges <> Value then
  begin
    Changing;
    DoSetEdges(Value);
    Changed;
  end;
end;

procedure TElasticLayer.SetScaled(const Value: boolean);
begin
  if Value <> FScaled then
  begin
    Changing;
    FScaled := Value;
    Changed;
  end;
end;


procedure TElasticLayer.SetSourceRect(const Value: TFloatRect);
begin
  Changing;
  FTransformation.SrcRect := Value;
  Changed;
end;

procedure TElasticLayer.SetTic(index: integer; const Value: TFloatPoint);
begin
  Changing;
  Edges[index] := Value;
  Changed;
end;


//============================ TTicTransformation =================================

procedure TTicTransformation.AssignTo(Dest: TPersistent);
begin
  if Dest is TTicTransformation then
  begin
    TTicTransformation(Dest).SrcRect := Self.SrcRect;
    TTicTransformation(Dest).Edges := Copy(Self.Edges, 0, 4);
  end
  else
    inherited;

end;

constructor TTicTransformation.Create;
begin
  inherited;
  SetLength(FEdges, 4);
end;

function TTicTransformation.GetMiddleEdges: TArrayOfFloatPoint;
var
  i, Next: integer;
begin
  //use cache because it seem called several times
  if not FMiddleEdgesValid then
  begin
    SetLength(FMiddleEdges, 4);

    for i := 0 to 3 do
    begin
      Next := i + 1;
      if Next > 3 then
        Next := 0;

      FMiddleEdges[i].X := Min(FEdges[i].X, FEdges[Next].X) + Abs(FEdges[i].X - FEdges[Next].X) / 2;
      FMiddleEdges[i].Y := Min(FEdges[i].Y, FEdges[Next].Y) + Abs(FEdges[i].Y - FEdges[Next].Y) / 2;
    end;
    FMiddleEdgesValid := True;
  end;

  Result := FMiddleEdges;

end;

function TTicTransformation.GetTransformedBounds(const ASrcRect: TFloatRect): TFloatRect;
begin
  Result.Left := Min(Min(FEdges[0].X, FEdges[1].X), Min(FEdges[2].X, FEdges[3].X));
  Result.Right := Max(Max(FEdges[0].X, FEdges[1].X), Max(FEdges[2].X, FEdges[3].X));
  Result.Top := Min(Min(FEdges[0].Y, FEdges[1].Y), Min(FEdges[2].Y, FEdges[3].Y));
  Result.Bottom := Max(Max(FEdges[0].Y, FEdges[1].Y), Max(FEdges[2].Y, FEdges[3].Y));
end;

procedure TTicTransformation.PrepareTransform;
var
  dx1, dx2, px, dy1, dy2, py: TFloat;
  g, h, k: TFloat;
  R: TFloatMatrix;
  LQuadX, LQuadY: array[0..3] of TFloat;
  i: integer;
begin
  for i := 0 to 3 do
    with {FOwner.}FEdges[i] do
    begin
      LQuadX[i] := X;
      LQuadY[i] := Y;
    end;



  px := LQuadX[0] - LQuadX[1] + LQuadX[2] - LQuadX[3];
  py := LQuadY[0] - LQuadY[1] + LQuadY[2] - LQuadY[3];

  if (px = 0) and (py = 0) then
  begin
    // affine mapping
    FMatrix[0, 0] := LQuadX[1] - LQuadX[0];
    FMatrix[1, 0] := LQuadX[2] - LQuadX[1];
    FMatrix[2, 0] := LQuadX[0];

    FMatrix[0, 1] := LQuadY[1] - LQuadY[0];
    FMatrix[1, 1] := LQuadY[2] - LQuadY[1];
    FMatrix[2, 1] := LQuadY[0];

    FMatrix[0, 2] := 0;
    FMatrix[1, 2] := 0;
    FMatrix[2, 2] := 1;
  end
  else
  begin
    // projective mapping
    dx1 := LQuadX[1] - LQuadX[2];
    dx2 := LQuadX[3] - LQuadX[2];
    dy1 := LQuadY[1] - LQuadY[2];
    dy2 := LQuadY[3] - LQuadY[2];
    k := dx1 * dy2 - dx2 * dy1;
    if k <> 0 then
    begin
      k := 1 / k;
      g := (px * dy2 - py * dx2) * k;
      h := (dx1 * py - dy1 * px) * k;

      FMatrix[0, 0] := LQuadX[1] - LQuadX[0] + g * LQuadX[1];
      FMatrix[1, 0] := LQuadX[3] - LQuadX[0] + h * LQuadX[3];
      FMatrix[2, 0] := LQuadX[0];

      FMatrix[0, 1] := LQuadY[1] - LQuadY[0] + g * LQuadY[1];
      FMatrix[1, 1] := LQuadY[3] - LQuadY[0] + h * LQuadY[3];
      FMatrix[2, 1] := LQuadY[0];

      FMatrix[0, 2] := g;
      FMatrix[1, 2] := h;
      FMatrix[2, 2] := 1;
    end
    else
    begin
      FillChar(FMatrix, SizeOf(FMatrix), 0);
    end;
  end;

  // denormalize texture space (u, v)
  R := IdentityMatrix;
  if IsRectEmpty(SrcRect) then
  begin
    R[0, 0] := 1;
    R[1, 1] := 1;
  end
  else
  begin
    R[0, 0] := 1 / (SrcRect.Right - SrcRect.Left);
    R[1, 1] := 1 / (SrcRect.Bottom - SrcRect.Top);
  end;
  FMatrix := Mult(FMatrix, R);

  R := IdentityMatrix;
  R[2, 0] := -SrcRect.Left;
  R[2, 1] := -SrcRect.Top;
  FMatrix := Mult(FMatrix, R);

  inherited;
end;


procedure TTicTransformation.ReverseTransformFixed(DstX, DstY: TFixed; out SrcX, SrcY: TFixed);
var
  Z: TFixed;
  Zf: TFloat;
begin
  Z := FixedMul(FInverseFixedMatrix[0, 2], DstX) + FixedMul(FInverseFixedMatrix[1, 2], DstY) + FInverseFixedMatrix[2, 2];

  if Z = 0 then
    Exit;

  {$IFDEF UseInlining}
  SrcX := FixedMul(DstX, FInverseFixedMatrix[0, 0]) + FixedMul(DstY, FInverseFixedMatrix[1, 0]) + FInverseFixedMatrix[2, 0];
  SrcY := FixedMul(DstX, FInverseFixedMatrix[0, 1]) + FixedMul(DstY, FInverseFixedMatrix[1, 1]) + FInverseFixedMatrix[2, 1];
  {$ELSE}
  inherited;
  {$ENDIF}

  if Z <> FixedOne then
  begin
    EMMS;
    Zf := FixedOne / Z;
    SrcX := Round(SrcX * Zf);
    SrcY := Round(SrcY * Zf);
  end;
end;


procedure TTicTransformation.ReverseTransformFloat(DstX, DstY: TFloat; out SrcX, SrcY: TFloat);
var
  Z: TFloat;
begin
  EMMS;
  Z := FInverseMatrix[0, 2] * DstX + FInverseMatrix[1, 2] * DstY + FInverseMatrix[2, 2];

  if Z = 0 then
    Exit;

  {$IFDEF UseInlining}
  SrcX := DstX * FInverseMatrix[0, 0] + DstY * FInverseMatrix[1, 0] + FInverseMatrix[2, 0];
  SrcY := DstX * FInverseMatrix[0, 1] + DstY * FInverseMatrix[1, 1] + FInverseMatrix[2, 1];
  {$ELSE}
  inherited;
  {$ENDIF}

  if Z <> 1 then
  begin
    Z := 1 / Z;
    SrcX := SrcX * Z;
    SrcY := SrcY * Z;
  end;
end;

procedure TTicTransformation.SetEdges(const Value: TArrayOfFloatPoint);
begin
  FEdges := Value;
  TransformValid := False;
  FMiddleEdgesValid := False;
end;

procedure TTicTransformation.TransformFixed(SrcX, SrcY: TFixed; out DstX, DstY: TFixed);
var
  Z: TFixed;
  Zf: TFloat;
begin
  Z := FixedMul(FFixedMatrix[0, 2], SrcX) + FixedMul(FFixedMatrix[1, 2], SrcY) + FFixedMatrix[2, 2];

  if Z = 0 then
    Exit;

  {$IFDEF UseInlining}
  DstX := FixedMul(SrcX, FFixedMatrix[0, 0]) + FixedMul(SrcY, FFixedMatrix[1, 0]) + FFixedMatrix[2, 0];
  DstY := FixedMul(SrcX, FFixedMatrix[0, 1]) + FixedMul(SrcY, FFixedMatrix[1, 1]) + FFixedMatrix[2, 1];
  {$ELSE}
  inherited;
  {$ENDIF}

  if Z <> FixedOne then
  begin
    EMMS;
    Zf := FixedOne / Z;
    DstX := Round(DstX * Zf);
    DstY := Round(DstY * Zf);
  end;
end;


procedure TTicTransformation.TransformFloat(SrcX, SrcY: TFloat; out DstX, DstY: TFloat);
var
  Z: TFloat;
begin
  EMMS;
  Z := FMatrix[0, 2] * SrcX + FMatrix[1, 2] * SrcY + FMatrix[2, 2];

  if Z = 0 then
    Exit;

  {$IFDEF UseInlining}
  DstX := SrcX * Matrix[0, 0] + SrcY * Matrix[1, 0] + Matrix[2, 0];
  DstY := SrcX * Matrix[0, 1] + SrcY * Matrix[1, 1] + Matrix[2, 1];
  {$ELSE}
  inherited;
  {$ENDIF}

  if Z <> 1 then
  begin
    Z := 1 / Z;
    DstX := DstX * Z;
    DstY := DstY * Z;
  end;
end;

//========================== TElasticBitmapLayer =======================================

procedure TElasticBitmapLayer.BitmapChanged(Sender: TObject);
begin
  SourceRect := FloatRect(0, 0, Bitmap.Width - 1, Bitmap.Height - 1);
end;

constructor TElasticBitmapLayer.Create(ALayerCollection: TLayerCollection);
begin
  inherited;
  FBitmap := TBitmap32.Create;
  FBitmap.OnChange := BitmapChanged;
end;

destructor TElasticBitmapLayer.Destroy;
begin
  FBitmap.Free;
  inherited;
end;


function TElasticBitmapLayer.DoHitTest(X, Y: integer): boolean;

var
  B: TPoint;
begin
  B := FInViewPortTransformation.ReverseTransform(Point(X, Y));


  Result := PtInRect(Rect(0, 0, Bitmap.Width, Bitmap.Height), B);
  if Result and {AlphaHit and} (Bitmap.PixelS[B.X, B.Y] and $FF000000 = 0) then
    Result := False;
end;


procedure TElasticBitmapLayer.Paint(Buffer: TBitmap32);
var
  ImageRect: TRect;
  DstRect, ClipRect, TempRect: TRect;
  //LTransformer : TTicTransformation;
  ShiftX, ShiftY, ScaleX, ScaleY: single;
begin
  if Bitmap.Empty then
    Exit;

  // Scale to viewport if activated.
  FInViewPortTransformation.Edges := GetScaledEdges;
  FInViewPortTransformation.SrcRect := FTransformation.SrcRect;

  DstRect := MakeRect(FInViewPortTransformation.GetTransformedBounds);
  ClipRect := Buffer.ClipRect;
  IntersectRect(ClipRect, ClipRect, DstRect);
  if IsRectEmpty(ClipRect) then
    Exit;

  if Cropped and (LayerCollection.Owner is TCustomImage32) and not (TImage32Access(LayerCollection.Owner).PaintToMode) then
  begin
    ImageRect := TCustomImage32(LayerCollection.Owner).GetBitmapRect;
    IntersectRect(ClipRect, ClipRect, ImageRect);
    if IsRectEmpty(ClipRect) then
      Exit;
  end;

  Transform(Buffer, FBitmap, FInViewPortTransformation, ClipRect);

end;

procedure TElasticBitmapLayer.SetBitmap(const Value: TBitmap32);
begin
  Changing;
  FBitmap.Assign(Value);
  Changed;
end;

procedure TElasticBitmapLayer.SetBlendMode(const Value: TBlendMode32);
begin
  if FBlendMode <> Value then
  begin
    FBlendMode := Value;
    case Value of
      bbmNormal32:
      begin
        Bitmap.OnPixelCombine := nil;
        Bitmap.DrawMode := dmBlend;
      end;
      else
      begin
        Bitmap.DrawMode := dmCustom;
        Bitmap.OnPixelCombine := GetBlendMode(Ord(FblendMode));
      end;
    end;
    Changed;
  end;
end;

//========================== TElasticRubberBandLayer ======================================

constructor TElasticRubberBandLayer.Create(ALayerCollection: TLayerCollection);
begin
  inherited;
  FThreshold := 8;
  FHandleFrame := clBlack;
  FHandleFill := clWhite;
  FOptions := DefaultRubberbandOptions;
  FHandleSize := 3;
  FPivotPoint := FloatPoint(0.5, 0.5);
end;

procedure TElasticRubberBandLayer.DoSetEdges(const Value: TArrayOfFloatPoint);
begin
  inherited;
  if Assigned(FChildLayer) then
    FChildLayer.Edges := Value;
end;

function TElasticRubberBandLayer.GetPivotOrigin: TFloatPoint;
var
  W, H: TFloat;
begin

  with FInViewPortTransformation.SrcRect do
  begin
    W := Right - Left + 1;
    H := Bottom - Top + 1;

    Result.X := Left - 1 + W * FPivotPoint.X;
    Result.Y := Top - 1 + H * FPivotPoint.Y;
  end;
end;

function TElasticRubberBandLayer.GetPivotTransformed: TFloatPoint;
begin
  Result := FInViewPortTransformation.Transform(GetPivotOrigin);
end;

function TElasticRubberBandLayer.GetRotatedCompass(LocalCompas: integer): integer;
var
  P, Center: TFloatPoint;
  Radians, Angle: TFloat;
  Compass: integer;
  LEdges: TArrayOfFloatPoint;
begin
  if LocalCompas <= 3 then
  begin
    LEdges := FInViewPortTransformation.Edges;
    Compass := LocalCompas;
  end
  else
  begin
    LEdges := FInViewPortTransformation.GetMiddleEdges;
    Compass := LocalCompas - 4;
  end;

  P := LEdges[Compass];
  Center := CentroidOf(LEdges);

  Radians := ArcTan2(Center.Y - P.Y, P.X - Center.X);
  Angle := Round(RadToDeg(Radians));

  // invert to clockwise
  Angle := 180 - Angle;

  // set NorthWest as zero axis
  Angle := Angle - 45;

  // add degree treshold around (+/-)
  Angle := Angle + 22.5;

  //clamp
  while Angle < 0 do
    Angle := Angle + 360;
  while Angle > 360 do
    Angle := Angle - 360;

  // force div with 8 zone
  Result := Floor(Angle / 45);

  if Result < 0 then
    Result := 8 - Result;

end;


function TElasticRubberBandLayer.GetRotatedEdges(AEdges: TArrayOfFloatPoint; dx, dy: TFloat): TArrayOfFloatPoint;
var
  LocalPivot, LPivot, P1, P2: TFloatPoint;
  LEdges: TArrayOfFloatPoint;
  Affine: TAffineTransformation;
  Radians, Angle1, NewAngle: TFloat;
  ShiftX, ShiftY, ScaleX, ScaleY: single;
  i: integer;
begin
  LPivot := FOldTransformedPivot;
  LocalPivot := FOldInViewPortPivot;

  P1 := FloatPoint(FDragPos);
  P2 := IncOf(P1, dx, dy);

  Radians := ArcTan2(LocalPivot.Y - P1.Y, P1.X - LocalPivot.X);
  Angle1 := RadToDeg(Radians);

  Radians := ArcTan2(LocalPivot.Y - P2.Y, P2.X - LocalPivot.X);
  NewAngle := RadToDeg(Radians);

  LEdges := Copy(AEdges, 0, 4);


  Affine := TAffineTransformation.Create;
  if Scaled and Assigned(LayerCollection) then
  begin
    LayerCollection.GetViewportScale(ScaleX, ScaleY);
    LayerCollection.GetViewportShift(ShiftX, ShiftY);
  end;
  Affine.Rotate(LPivot.X, LPivot.Y, NewAngle - Angle1);

  for i := 0 to 3 do
  begin
    LEdges[i] := Affine.Transform(Ledges[i]);
  end;

  Result := LEdges;

  Affine.Free;
end;

procedure TElasticRubberBandLayer.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if FIsDragging then
    Exit;
  FDragPos := Point(X, Y);
  FOldInViewPortPivot := GetPivotTransformed();

  FOriginDragPos := FInViewPortTransformation.ReverseTransform(FloatPoint(FDragPos));

  FOldOriginPivot := GetPivotOrigin;
  FOldTransformedPivot := FTransformation.Transform(FOldOriginPivot);

  FOldEdges := Copy(Edges, 0, 4); //TODO: shall we use Copy(e,0,4) instead ??
  FIsDragging := True;
  inherited;
end;


procedure TElasticRubberBandLayer.MouseMove(Shift: TShiftState; X, Y: integer);

const
  MoveCursor: array [0..7] of TCursor = (
    crGrMovePointNWSE,  // cdNorthWest
    crGrMovePointNS,    // cdNorth
    crGrMovePointNESW,  // cdNorthEast
    crGrMovePointWE,    // cdEast
    crGrMovePointNWSE,  // cdSouthEast
    crGrMovePointNS,    // cdSouth
    crGrMovePointNESW,  // cdSouthWest
    crGrMovePointWE     // cdWest
    );

  RotateCursor: array [0..7] of TCursor = (
    crGrRotateNW,       // cdNorthWest
    crGrRotateN,        // cdNorth
    crGrRotateNE,       // cdNorthEast
    crGrRotateE,        // cdEast
    crGrRotateSE,       // cdSouthEast
    crGrRotateS,        // cdSouth
    crGrRotateSW,       // cdSouthWest
    crGrRotateW         // cdWest
    );

  SheerCursor: array [0..7] of TCursor = (
    crGrArrowMoveNWSE,  // cdNorthWest
    crGrArrowMoveWE,    // cdNorth
    crGrArrowMoveNESW,  // cdNorthEast
    crGrArrowMoveNS,    // cdEast
    crGrArrowMoveNWSE,  // cdSouthEast
    crGrArrowMoveWE,    // cdSouth
    crGrArrowMoveNESW,  // cdSouthWest
    crGrArrowMoveNS     // cdWest
    );

  function GetNonViewport(P: TFloatPoint): TFloatPoint;
  var
    ScaleX, ScaleY, ShiftX, ShiftY: single;
  begin
    Result := P;
    // Scale to non viewport if activated.
    if Scaled and Assigned(LayerCollection) then
    begin
      LayerCollection.GetViewportScale(ScaleX, ScaleY);
      LayerCollection.GetViewportShift(ShiftX, ShiftY);
      Result.X := Result.X / ScaleX - ShiftX;
      Result.Y := Result.Y / ScaleY - ShiftY;
    end;
  end;

var
  dx, dy, ScaleX, ScaleY: TFloat;
  Zone: integer;
  ZeroAxis: integer; //possibly 1 or 0 for straight drag
  Local: TPoint;
  P: TFloatPoint;
  LStraight, MouseInside: boolean;
  LEdges: TArrayOfFloatPoint;
begin

  FMouseOverPos := Point(X, Y); //used to correct cursor by KeyDown

  if not FIsDragging then
  begin
    P := FloatPoint(X, Y);

    // Transform coordinates into local space.
    Local := FInViewPortTransformation.ReverseTransform(Point(X, Y));
    Zone := PtIn9Zones(P, FInViewPortTransformation, MouseInside);
    FCompass := Zone;

    if (rboAllowPivotMove in FOptions) {and (FCompass = 8)} and (LineDistance(P, GetPivotTransformed) <= FThreshold) then
      FDragState := tdsMovePivot
    else
    begin
      // initial assumed pos
      if MouseInside then
        FDragState := tdsMoveLayer
      else
        FDragState := tdsNone;  // outside and too far = no selection

      // real detection. Distance is  using inView space
      // Note:  We don't need to search the nearest distance for each edges+middle_Edge,
      //        because the compass was already known.
      if FCompass < 8 then
      begin
        //if MouseInside then //DEBUG
        with FInViewPortTransformation do
        begin

          if (FCompass <= 3) then
          begin
            if LineDistance(P, Edges[FCompass]) <= FThreshold then
              FDragState := tdsResizeCorner;
          end
          else
          if LineDistance(P, GetMiddleEdges[FCompass - 4]) <= FThreshold then
            FDragState := tdsResizeSide;
        end;

        // If the user holds down the control key then sheering becomes active (only for sides).
        if (FDragState = tdsResizeSide) and (ssCtrl in Shift) then
        begin
          FDragState := tdsSkew;
        end
        // If the user holds down the control key then distorting becomes active (only for corners).
        else if (FDragState = tdsResizeCorner) then
        begin
          if (ssCtrl in Shift) then
            FDragState := tdsDistortion
          else
          if (ssShift in Shift) then
            FDragState := tdsPerspective;
        end;
      end;

      //TODO: Add treshold for rotation
      if FDragState = tdsNone then  // outside ? currently is always rotation
        FDragState := tdsRotate;

    end;

    case FDragState of
      tdsNone:
        Cursor := crDefault;
      tdsRotate:
        Cursor := RotateCursor[GetRotatedCompass(FCompass)];
      tdsMoveLayer:
        Cursor := crGrArrow;
      tdsMovePivot:
        Cursor := crGrMoveCenter;

      tdsDistortion, tdsPerspective,
      tdsResizeCorner, tdsResizeSide:
        Cursor := MoveCursor[GetRotatedCompass(FCompass)];

      tdsSkew: Cursor := SheerCursor[GetRotatedCompass(FCompass)];
      else
        Cursor := crDefault;
    end;

  end
  else
    //if FIsDragging then
  begin
    // If the user holds down the control key then sheering becomes active (only for sides).
    if (FDragState in [tdsResizeSide, tdsSkew]) then
    begin
      if (ssCtrl in Shift) then
        FDragState := tdsSkew
      else
        FDragState := tdsResizeSide;
    end
    // If the user holds down the control key then distorting becomes active (only for corners).
    else if (FDragState in [tdsResizeCorner, tdsDistortion]) then
    begin
      if ssCtrl in Shift then
        FDragState := tdsDistortion
      else
        FDragState := tdsResizeCorner;
    end;

    if (ssAlt in Shift) then
      ZeroAxis := 1
    else
      ZeroAxis := 0;

    LStraight := not (ssAlt in Shift);

    dx := X - FDragPos.X;
    dy := Y - FDragPos.Y;
    if Scaled then
    begin
      LayerCollection.GetViewportScale(ScaleX, ScaleY);
      dx := dx / ScaleX;
      dy := dy / ScaleY;
    end;

    case FDragState of
      tdsMoveLayer:
        Edges := MoveEdges(FOldEdges, dx, dy);

      tdsMovePivot:
      begin
        P := FInViewPortTransformation.ReverseTransform(FloatPoint(X, Y));
        SetPivotOrigin(P);
      end;

      tdsResizeSide:
      begin
        Edges := ResizeBySide(FOldEdges, FCompass - 4, dx, dy, LStraight);
        Cursor := MoveCursor[GetRotatedCompass(FCompass)];
      end;

      tdsSkew:
      begin
        Edges := SkewBySide(FOldEdges, FCompass - 4, dx, dy, LStraight);
      end;

      tdsResizeCorner:
      begin
        Edges := ResizeByCorner(Self, FOldEdges, FCompass, dx, dy, LStraight, Odd(GetRotatedCompass(FCompass)));
        Cursor := MoveCursor[GetRotatedCompass(FCompass)];
      end;

      tdsDistortion:
      begin
        P := GetNonViewport(FloatPoint(X, Y));

        LEdges := Copy(FOldEdges, 0, 4);
        LEdges[FCompass] := P;

        Edges := LEdges;

        Cursor := MoveCursor[GetRotatedCompass(FCompass)];
      end;

      tdsRotate:
      begin
        dx := X - FDragPos.X;
        dy := Y - FDragPos.Y;

        Edges := GetRotatedEdges(FOldEdges, dx, dy);
        Cursor := RotateCursor[GetRotatedCompass(FCompass)];
      end;

      tdsPerspective:
      begin
        Edges := PerspectiveByCorner(FOldEdges, FCompass, dx, dy, LStraight);
      end;

    end;
  end;
  inherited;

end;

procedure TElasticRubberBandLayer.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if FIsDragging then
  begin
    FIsDragging := False;
  end;
  inherited;

end;

procedure TElasticRubberBandLayer.Paint(Buffer: TBitmap32);
var
  LEdges: TArrayOfFloatPoint;

  procedure DrawLineP(A, B: TFloatPoint);
  begin
    with Buffer, MakeRect(FloatRect(A, B)) do
    begin
      if Top = Bottom then
        HorzLineTSP(Left, Top, Right)
      else
      if Left = Right then
        VertLineTSP(Left, Top, Bottom)
      else
      begin
        MoveToF(A.X, A.Y);
        LineToFSP(B.X, B.Y);
      end;
    end;
  end;

  procedure DrawContour;

  begin
    with Buffer do
    begin
      DrawLineP(LEdges[0], LEdges[1]);
      DrawLineP(LEdges[1], LEdges[2]);
      DrawLineP(LEdges[2], LEdges[3]);
      DrawLineP(LEdges[3], LEdges[0]);
    end;
  end;

  //---------------------------------------------------------------------------

  procedure DrawHandle(XY: TFloatPoint);
  var
    R: TRect;
  begin
    with Point(XY) do
      R := MakeRect(X, Y, X, Y);

    InflateRect(R, FHandleSize, FHandleSize);

    Buffer.FillRectS(R, FHandleFill);
    Buffer.FrameRectS(R, FHandleFrame);
  end;
  //---------------------------------------------------------------------------

  procedure DrawHandles(AEdges: TArrayOfFloatPoint);
  var
    i: integer;
  begin
    for i := 0 to 3 do
    begin
      DrawHandle(AEdges[i]);
    end;
  end;

  //---------------------------------------------------------------------------

  procedure DrawPivot();
  begin
    with GetPivotTransformed() do
      Buffer.Draw(Round(X - 8), Round(Y - 8), GetPivotBitmap());
  end;


  //--------------- end local functions ---------------------------------------
var
  NewStipple: array of TColor32;
begin
  LEdges := GetScaledEdges;
  FInViewPortTransformation.Edges := LEdges;
  FInViewPortTransformation.SrcRect := FTransformation.SrcRect;


  if rboShowFrame in FOptions then
  begin
    SetLength(NewStipple, 4);
    NewStipple[0] := clWhite32; NewStipple[1] := clWhite32; NewStipple[2] := clBlack32; NewStipple[3] := clBlack32;
    Buffer.SetStipple(NewStipple);
    Buffer.StippleCounter := 0;
    Buffer.StippleStep := 1;
    DrawContour;
  end;

  if rboShowHandles in FOptions then
  begin
    DrawHandles(LEdges);

    LEdges := FInViewPortTransformation.GetMiddleEdges;
    DrawHandles(LEdges);
  end;

  if rboAllowPivotMove in FOptions then
    DrawPivot();
end;

procedure TElasticRubberBandLayer.SetChildLayer(const Value: TElasticLayer);
begin
  if Assigned(FChildLayer) then
    RemoveNotification(FChildLayer);

  FChildLayer := Value;
  if Assigned(Value) then
  begin
    FTransformation.Assign(Value.FTransformation);
    Scaled := Value.Scaled;
    AddNotification(FChildLayer);
  end;
end;

procedure TElasticRubberBandLayer.SetHandleFill(const Value: TColor);
begin
  if FHandleFill <> Value then
  begin
    FHandleFill := Value;
    TLayerCollectionAccess(LayerCollection).GDIUpdate;
  end;
end;

procedure TElasticRubberBandLayer.SetHandleFrame(const Value: TColor);
begin
  if FHandleFrame <> Value then
  begin
    FHandleFrame := Value;
    TLayerCollectionAccess(LayerCollection).GDIUpdate;
  end;
end;

procedure TElasticRubberBandLayer.SetHandleSize(Value: integer);
begin
  if Value < 1 then
    Value := 1;
  if FHandleSize <> Value then
  begin
    FHandleSize := Value;
    TLayerCollectionAccess(LayerCollection).GDIUpdate;
  end;
end;

procedure TElasticRubberBandLayer.SetLayerOptions(Value: cardinal);
begin
  Value := Value and not LOB_NO_UPDATE; // workaround for changed behaviour
  inherited SetLayerOptions(Value);
end;

procedure TElasticRubberBandLayer.SetOptions(const Value: TExtRubberBandOptions);
begin
  if FOptions <> Value then
  begin
    Changing;
    FOptions := Value;
    Changed; // Layer collection.
    //DoChange;
  end;
end;

procedure TElasticRubberBandLayer.SetPivotOrigin(Value: TFloatPoint);
var
  W, H: TFloat;
begin
  with FInViewPortTransformation.SrcRect do
  begin
    W := Right - Left + 1;
    H := Bottom - Top + 1;
    Changing;
    FPivotPoint.X := Value.X / W;
    FPivotPoint.Y := Value.Y / H;
    Changed;
  end;
end;

//==============================================================

initialization

finalization
  if Assigned(UPivotBitmap) then
    UPivotBitmap.Free;
end.
