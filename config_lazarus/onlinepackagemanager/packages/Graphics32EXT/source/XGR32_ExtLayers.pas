
{**********************************************************************
 Package pl_Graphics32EXT.pkg
 this unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit XGR32_ExtLayers;

 {$MODE Delphi}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  LCLIntf, LCLType, LMessages, Messages, Types,
  Classes, Controls, Forms, Graphics,ExtCtrls,
  GR32,
  GR32_Resamplers,
  GR32_Containers ,
  GR32_Layers,
  GR32_RepaintOpt,
  GR32_Image,
  GR32_Transforms,
  GR32_Polygons,
  GR32_MicroTiles,  GR32_LowLevel,
  XGR32_ExtLayers_transform, XGR32_ExtLayers_cursors;

type

TRubberbandDragState =
   (rdsNone, rdsMoveLayer, rdsMovePivot, rdsResizeN, rdsResizeNE, rdsResizeE, rdsResizeSE,
    rdsResizeS, rdsResizeSW, rdsResizeW, rdsResizeNW,
    rdsSheerN, rdsSheerE, rdsSheerS, rdsSheerW,
    rdsRotate);

  TExtRubberBandOptions = set of (
    rboAllowPivotMove,
    rboAllowCornerResize,
    rboAllowEdgeResize,
    rboAllowMove,
    rboAllowSheering,
    rboAllowRotation,
    rboShowFrame,
    rboShowHandles,
    rboShowPivot
  );

const
  DefaultRubberbandOptions = [rboAllowCornerResize,rboAllowEdgeResize,rboAllowMove,rboAllowSheering,
                              rboAllowRotation,rboShowFrame,rboShowHandles,rboAllowPivotMove,
                              rboShowPivot];

type
  TExtGridLayer = class;

  // Need access to protected properties and for some extensions.
  TExtAffineTransformation = class(TXAffineTransformation)
  private
    FInverseMatrix: TFloatMatrix;
  protected
    procedure PrepareTransform; override;
  public
    procedure AddTransformation(Transformation: TExtAffineTransformation);
    function  TransformNormal(const P: TFloatPoint): TFloatPoint;
  end;


TTransformationLayer = class(TPositionedLayer)//(TCustomLayer)
  private
    FRotAngle: Single;                            // Given in degrees.
    FAlphaHit: Boolean;
    FTransformation: TExtAffineTransformation;
    FTempTransformation:TExtAffineTransformation;
    FSkew: TFloatPoint;
    FPosition: TFloatPoint;
    FScaling: TFloatPoint;
    FScaled: Boolean;                            // Scaled with the viewport of a possible owner ImgView32.
    FPivotPoint: TFloatPoint;                    // Center of rotation and proportional scaling.
    FGridLayer: TExtGridLayer;                   // Used to snap/align coordinates.
    FOnChange: TNotifyEvent;                     // For individual change events.
    procedure SetRotAngle(Value: Single);
    procedure SeTExtGridLayer(const Value: TExtGridLayer);
    procedure SetPivot(const Value: TFloatPoint);
    procedure SetPosition(const Value: TFloatPoint);
    procedure SetScaled(const Value: Boolean);
    procedure SetScaling(const Value: TFloatPoint);
    procedure SetSkew(const Value: TFloatPoint);
  protected
    procedure DoChange; virtual;
    function  GetNativeSize: TSize; virtual;
    procedure Notification(ALayer: TCustomLayer); override;
  public
    constructor Create(ALayerCollection: TLayerCollection); override;
    destructor Destroy; override;
    Procedure MovePivotTo(const Pos:integer);Virtual;
    Function  GetFinalSize:TPoint; Virtual;
    procedure GetLayerTransformation(var Transformation: TExtAffineTransformation);
    function  GetTransformedTargetRect: TFloatRect;
    Procedure GetAbsoluteToLocal(var X,Y:integer);
    Procedure TransformPoint(var X,Y:integer);
    Function  GetViewPortScale:TFloatPoint;
    procedure ResetTransformation; virtual;
    procedure UpdateTransformation; virtual;

    property AlphaHit: Boolean read FAlphaHit write FAlphaHit;
    property RotAngle: Single read FRotAngle write SetRotAngle;
    property GridLayer: TExtGridLayer read FGridLayer write SeTExtGridLayer;
    property Position: TFloatPoint read FPosition write SetPosition;
    property PivotPoint: TFloatPoint read FPivotPoint write SetPivot;
    property ScaledViewport: Boolean read FScaled write SetScaled; // Do not use Scaled as name as this is alredy taken by other VCL controls.
    property Scaling: TFloatPoint read FScaling write SetScaling;
    property Skew: TFloatPoint read FSkew write SetSkew;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;


  // The grid elements determine what will be painted of the grid layer.
  TGridElements = set of (
    geLines,
    geHalfTicks,
    geGuides,
    geQuarterTicks
  );

  TSnapOptions = set of (
    soSnapBorders,
    soSnapGuides,
    soSnapGrid
  );


TRectRegionNode = class(TObject)
    private
      FStartPt, FEndPt: TPoint;
    public
      constructor Create;

      property StartPoint: TPoint read FStartPt write FStartPt;
      property EndPoint  : TPoint read FEndPt   write FEndPt;
    end;


TRectRegionNodeList = class(TList)
    public
      destructor Destroy; override;
      procedure DeleteAll;
    end;

TExtGridLayer = class(TTransformationLayer)
  private
    FGridSize: Integer;
    FElements: TGridElements;
    FMainGridColor,
    FHalfTickColor,
    FQuaterTickColor,
    FGuidesColor: TColor32;
    FSnapOptions: TSnapOptions;
    FSnapThreshold: Integer;
    FHorizontalGuides,
    FVerticalGuides: TList;
    procedure SetColor(const Index: Integer; const Value: TColor32);
    procedure SetElements(const Value: TGridElements);
    procedure SetGridSize(const Value: Integer);
    procedure SetSnapThreshold(const Value: Integer);
  protected
    function  DoHitTest(X, Y: Integer): Boolean; override;
    function  GetNativeSize: TSize; override;
    procedure Paint(Buffer: TBitmap32); override;
  public
    constructor Create(ALayerCollection: TLayerCollection); override;
    destructor Destroy; override;

    procedure AddHorizontalGuide(Y: Integer);
    procedure AddVerticalGuide(X: Integer);
    procedure ClearGuides;
    function  Snap(var P: TFloatPoint): Boolean;
    procedure RemoveHorizontalGuide(Y: Integer);
    procedure RemoveVerticalGuide(X: Integer);
    procedure SetDefaultValues;

    property Elements: TGridElements read FElements write SetElements default [geLines..geQuarterTicks];
    property GridSize: Integer read FGridSize write SetGridSize default 15;
    property GuidesColor: TColor32 index 3 read FGuidesColor write SetColor default clBlue32;
    property HalfTickColor: TColor32 index 1 read FHalfTickColor write SetColor default clWhite32;
    property MainGridColor: TColor32 index 0 read FMainGridColor write SetColor default clMaroon32;
    property QuaterTickColor: TColor32 index 2 read FQuaterTickColor write SetColor default clWhite32;
    property SnapOptions: TSnapOptions read FSnapOptions write FSnapOptions default [soSnapBorders..soSnapGrid];
    property SnapThreshold: Integer read FSnapThreshold write SetSnapThreshold default 8;
  end;

  TCursorDirection = (
    cdNotUsed,
    cdNorth,
    cdNorthEast,
    cdEast,
    cdSouthEast,
    cdSouth,
    cdSouthWest,
    cdWest,
    cdNorthWest
  );

  TContour = array[0..3] of TFixedPoint;


  // TExtBitmapLayer provides some special properties as used for the image editor, like the ability for affine
  // transformation, name, lock state and other things.
  TLayerDrawMode = (
    ldmBlend,           // Can also be opaque if opacity is 100%.
    ldmAdd,
    ldmSubtract,
    ldmModulate,
    ldmMax,
    ldmMin
  );

TPropertyLayer = class(TTransformationLayer)
  private
    FName: WideString;
    FLocked: Boolean;
    FDrawMode: TLayerDrawMode;
    procedure SetName(const Value: WideString);
    procedure SetDrawMode(const Value: TLayerDrawMode);
  public
    property DrawMode: TLayerDrawMode read FDrawMode write SetDrawMode;
    property Locked: Boolean read FLocked write FLocked;
    property Name: WideString read FName write SetName;
  end;

TExtTextLayer = class(TPropertyLayer)
  private
    FText: WideString;
    FTextColor: TColor32;
    procedure SetText(const Value: WideString);
    procedure SetTextColor(const Value: TColor32);
  public
    constructor Create(LayerCollection: TLayerCollection); override;
    property Text: WideString read FText write SetText;
    property TextColor: TColor32 read FTextColor write SetTextColor default clBlack32;
  end;

TExtBitmapLayer = class(TPropertyLayer)
  private
    FBitmap: TBitmap32;
    FCropped: Boolean;
    procedure BitmapChanged(Sender: TObject);
    procedure SetCropped(Value: Boolean);
    procedure SetBitmap(Value: TBitmap32);
  protected
    function  DoHitTest(X, Y: Integer): Boolean; override;
    function  GetNativeSize: TSize; override;
    procedure Paint(Buffer: TBitmap32); override;
  public
    constructor Create(ALayerCollection: TLayerCollection); override;
    destructor Destroy; override;
    Procedure MovePivotTo(const Pos:integer);override;
    Function  GetFinalSize:TPoint; override;
    procedure PaintTo(Buffer: TBitmap32); Overload;
    procedure PaintTo(Buffer: TBitmap32; const X,Y:integer);Overload;
    procedure PaintTo(Buffer: TBitmap32; const R: TRect); Overload;
    property  Bitmap: TBitmap32 read FBitmap write SetBitmap;
    property  Cropped: Boolean read FCropped write SetCropped;
  end;


TExtCustomRBLayer = class(TTransformationLayer)
  private
    FFrameStipplePattern: TArrayOfColor32;
    FFrameStippleStep: Single;
    FFrameStippleCounter: Single;
    FSize: TSize;                      // Real (untransformed) size of the child layer (if there is one).
                                       // Otherwise the current (unscaled) size of the rubber band.
    FOptions: TExtRubberBandOptions;
    FHandleSize: Integer;
    FHandleFrame: TColor32;
    FHandleFill: TColor32;
    FThreshold: Integer;               // Distance from a point, which still considers this point as hit.
    FOuterColor: TColor32;             // If the alpha value of this color is > 0 then the color is used
                                       // to blend everything outside the rubberband rect by this color.

    // Drag/resize support
    FIsDragging: Boolean;
    fTempPivotVisible: Boolean;       //if pivot is not visible at options then if mouse is on pivot this
                                      //temp var used to draw
    FDragState: TRubberbandDragState;
    FDragPos: TPoint;
    FOldPosition: TFloatPoint;         // Keep the old values to restore in case of a cancellation.
    FOldScaling: TFloatPoint;
    FOldPivot: TFloatPoint;
    FOldSkew: TFloatPoint;
    FOldAngle: Single;
    procedure SetFrameStippleStep(const Value: Single);
    procedure SetFrameStippleCounter(const Value: Single);
    procedure SetHandleFill(const Value: TColor32);
    procedure SetHandleFrame(const Value: TColor32);
    procedure SetHandleSize(Value: Integer);
    procedure SetOptions(const Value: TExtRubberBandOptions);
    procedure SetOuterColor(const Value: TColor32);
    procedure SetSize(const Value: TSize);
  protected
    function  DoHitTest(X, Y: Integer): Boolean; override;
    procedure FillOuter(Buffer: TBitmap32; OuterRect: TRect; Contour: TContour);
    function  GetCursorDirection(X, Y: Integer; AxisTolerance: Integer; State: TRubberbandDragState): TCursorDirection;
    function  GetHitCode(X, Y: Integer; Shift: TShiftState): TRubberbandDragState;
    function  GetNativeSize: TSize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Notification(ALayer: TCustomLayer); override;
    procedure Paint(Buffer: TBitmap32); override;
    function  SnapPosition: Boolean;
    procedure UpdateChildLayer; virtual;
  public
    constructor Create(LayerCollection: TLayerCollection); override;
    destructor Destroy; override;
    procedure SetFrameStipple(const Value: Array of TColor32);
    procedure Cancel; //Retunt to old Transformations
    procedure ResetTransformation; override;
    Procedure TotalUpdate; virtual;
    Procedure MovePivotTo(const Pos:integer);override;
    Function  GetFinalSize:TPoint; override;
    property  DragState: TRubberbandDragState read FDragState;
    property  FrameStippleStep: Single read FFrameStippleStep write SetFrameStippleStep;
    property  FrameStippleCounter: Single read FFrameStippleCounter write SetFrameStippleCounter;
    property  HandleSize: Integer read FHandleSize write SetHandleSize default 3;
    property  HandleFill: TColor32 read FHandleFill write SetHandleFill default clWhite32;
    property  HandleFrame: TColor32 read FHandleFrame write SetHandleFrame default clBlack32;
    property  IsDragging: Boolean read FIsDragging;
    property  Options: TExtRubberBandOptions read FOptions write SetOptions default DefaultRubberbandOptions;
    property  OuterColor: TColor32 read FOuterColor write SetOuterColor;
    property  Size: TSize read FSize write SetSize;
    property  Threshold: Integer read FThreshold write FThreshold default 8;
  end;


TExtRubberBandLayer = class(TExtCustomRBLayer)
  private
    FChildLayer: TTransformationLayer;
    procedure SetChildLayer(Value: TTransformationLayer);
  protected
    procedure Notification(ALayer: TCustomLayer);
    procedure UpdateChildLayer; override;
  public
    constructor Create(LayerCollection: TLayerCollection); override;
    destructor Destroy; override;
    property  ChildLayer: TTransformationLayer read FChildLayer write SetChildLayer;
  end;




TExtCropLayer = class(TExtCustomRBLayer)
  private
    fTimer:TTimer;
    FActive:Boolean;
    FImage: TImgView32;
    procedure SetImage(Value: TImgView32);
    procedure SetActive(Value: Boolean);
    Procedure DoOnTime(Sender: TObject);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(LayerCollection: TLayerCollection); override;
    destructor Destroy; override;

    procedure Execute_Crop;
    property  Image: TImgView32 read FImage write SetImage;
    property  Active: Boolean read FActive write SetActive;
  end;


TExtSelectionRubberBandLayer = class(TExtRubberBandLayer)
  private
    fTimer:TTimer;
    FActive:Boolean;
    procedure SetActive(Value: Boolean);
    Procedure DoOnTime(Sender: TObject);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(LayerCollection: TLayerCollection); override;
    destructor Destroy; override;
    property  Active: Boolean read FActive write SetActive;
  end;


TExtMouseLayerShape = (mlsCircle,mlsRetracle);

TExtMouseLayer = class(TTransformationLayer)
  private
    FShape:TExtMouseLayerShape;
    FCenter: TPoint;
    FRadius: Integer;
    FDrawCenterDots:Boolean;
    procedure SetCenter(const Value: TPoint);
    procedure SetRadius(const Value: Integer);
    procedure SetShape(const Value: TExtMouseLayerShape);
  protected
    procedure Paint(Buffer: TBitmap32); override;
    procedure PaintCicle(Buffer: TBitmap32);
    procedure PaintRetracle(Buffer: TBitmap32);
  public
    constructor Create(ALayerCollection: TLayerCollection); override;
    Property Shape:TExtMouseLayerShape read fShape  write SetShape;
    property Radius: Integer read FRadius write SetRadius;
    property Center: TPoint read FCenter write SetCenter;
    property DrawCenterDots:Boolean read FDrawCenterDots write FDrawCenterDots;
  end;

TExtSelectionShapeLayer = class(TExtCustomRBLayer)
  private
    fTimer:TTimer;
    FActive:Boolean;
    procedure SetActive(Value: Boolean);
    Procedure DoOnTime(Sender: TObject);
  protected
    procedure Paint(Buffer: TBitmap32); override;
  public
    FLayerTopLeft    : TPoint;
    FMaskBorderStart : TPoint;     // The start point of the selection.
    FMarchingAntsLineList: TRectRegionNodeList;
    constructor Create(ALayerCollection: TLayerCollection); override;
    destructor Destroy; override;
    property  Active: Boolean read FActive write SetActive;
  end;


 // angle is degrees X0,Y0 is rotation point
Procedure RotateAroundPoint(var X,Y:Extended;const X0,Y0,Angle:Extended);

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  Math;

const
  Epsilon = 1E-4; // Minimal changes are not taken into account. This is the threshold.

Procedure RotateAroundPoint(var X,Y:Extended;const X0,Y0,Angle:Extended);
 Var xnew,ynew,acos,asin,ang:Extended;
 begin
   ang:=DegToRad(Angle);
   SinCos(ang, asin, acos);
   xnew:= (X-X0)*acos+(Y-Y0)*asin+ X0;
   ynew:=-(X-X0)*asin+(Y-Y0)*acos+ Y0;
   X:=xnew;
   Y:=ynew;
 end;

//----------------- TExTransformation -------

procedure TExtAffineTransformation.PrepareTransform;

begin
  inherited;
  FInverseMatrix := Matrix;
  Invert(FInverseMatrix);
end;

// Used concatenate transformations. The given transformation is applied to the current transformation
// by simply multiplying the existing matrices.

procedure TExtAffineTransformation.AddTransformation(Transformation: TExtAffineTransformation);
  function Mult(const M1, M2: TFloatMatrix): TFloatMatrix;
  var
    I, J: Integer;

  begin
    for I := 0 to 2 do
      for J := 0 to 2 do
        Result[I, J] := M1[0, J] * M2[I, 0] + M1[1, J] * M2[I, 1] + M1[2, J] * M2[I, 2];
  end;

begin
  Matrix := Mult(Transformation.Matrix, Matrix);
end;

// Transforms the given point using the current transformation matrix.
// The strange name is caused by the fact that there is already a "Transform" method in the base class, which however
// does a reverse transformation and hence should be called "ReverseTransform". I cannot rename it however, without
// losing compatibility. Additionally, it works (for speed reasons) with fixed point math.

function TExtAffineTransformation.TransformNormal(const P: TFloatPoint): TFloatPoint;
begin
  Result.X := Matrix[0, 0] * P.X + Matrix[1, 0] * P.Y + Matrix[2, 0];
  Result.Y := Matrix[0, 1] * P.X + Matrix[1, 1] * P.Y + Matrix[2, 1];
end;

//=============== TTransformationLayer ====================================================

constructor TTransformationLayer.Create(ALayerCollection: TLayerCollection);
begin
  inherited;
  LayerOptions := LOB_VISIBLE or LOB_MOUSE_EVENTS;
  FTransformation:=TExtAffineTransformation.Create;
  FTempTransformation:=TExtAffineTransformation.Create;
  ResetTransformation;
end;

destructor TTransformationLayer.Destroy;
begin
  if Assigned(FGridLayer) then FGridLayer.RemoveNotification(Self);

  FTransformation.Free;
  FTempTransformation.Free;
  inherited;
end;

procedure TTransformationLayer.SetRotAngle(Value: Single);
begin
  Changing;
  FRotAngle := Value;
  Changed; // Layer collection.
  DoChange; // Layer only.
end;

procedure TTransformationLayer.SetPivot(const Value: TFloatPoint);
begin
  Changing;
  FPivotPoint := Value;
  Changed;

  DoChange;
end;

procedure TTransformationLayer.SeTExtGridLayer(const Value: TExtGridLayer);
begin
  if Value <> FGridLayer then
  begin
    if Assigned(FGridLayer) then FGridLayer.RemoveNotification(Self);
    FGridLayer := Value;
    if Assigned(FGridLayer) then FGridLayer.AddNotification(Self);
  end;
end;

procedure TTransformationLayer.SetPosition(const Value: TFloatPoint);
begin
  Changing;
  FPosition := Value;
  Changed;

  DoChange;
end;

procedure TTransformationLayer.SetScaled(const Value: Boolean);
begin
  if FScaled <> Value then
  begin
    Changing;
    FScaled := Value;
    Changed;

    DoChange;
  end;
end;

procedure TTransformationLayer.SetScaling(const Value: TFloatPoint);
begin
  Changing;
  FScaling := Value;
  Changed;

  DoChange;
end;

procedure TTransformationLayer.SetSkew(const Value: TFloatPoint);
begin
  Changing;
  FSkew := Value;
  Changed;

  DoChange;
end;

procedure TTransformationLayer.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

// Returns the untransformed size of the content. Must be overriden by descentants.
function TTransformationLayer.GetNativeSize: TSize;
begin
  Result.cx := 1;
  Result.cy := 1;
end;

procedure TTransformationLayer.Notification(ALayer: TCustomLayer);
begin
  inherited;
  if ALayer = FGridLayer then FGridLayer := nil;
end;

// Creates Transformation if it does not exist yet and applies the current layer transformations.
// This does not include viewport scaling.
// The caller is responsible for freeing Transformation!

procedure TTransformationLayer.GetLayerTransformation(var Transformation: TExtAffineTransformation);
begin
  if Transformation = nil then
    Transformation := TExtAffineTransformation.Create
  else
    Transformation.Clear;

  Transformation.Translate(-FPivotPoint.X, -FPivotPoint.Y);
  Transformation.Scale(FScaling.X, FScaling.Y);
  Transformation.Skew(FSkew.X, FSkew.Y);
  TransFormation.Rotate(0, 0, FRotAngle);
  Transformation.Translate(FPosition.X + FPivotPoint.X, FPosition.Y + FPivotPoint.Y);
end;

// Helper method to transform position, size and scale factor into a rectangle which
// determines the location in a container like TImgView32.
// The rotation is not taken into account here because it is meant for special handling.

function TTransformationLayer.GetTransformedTargetRect: TFloatRect;
var xSize: TSize;
    xScaleX, xScaleY: TFloat;
    xShiftX, xShiftY: TFloat;
begin
  UpdateTransformation;

  xSize := GetNativeSize;
  with FPosition do
    Result := FloatRect(X, Y, FScaling.X * (X + xSize.cx), FScaling.Y * (Y + xSize.cy));

  if FScaled and Assigned(LayerCollection) and Assigned(LayerCollection) then
    begin
      LayerCollection.GetViewportScale(xScaleX, xScaleY);
      LayerCollection.GetViewportShift(xShiftX, xShiftY);

      Result.Left := Result.Left * xScaleX / 65536 + xShiftX;
      Result.Top := Result.Top * xScaleY / 65536 + xShiftY;
      Result.Right := Result.Right * xScaleX / 65536 + xShiftX;
      Result.Bottom := Result.Bottom * xScaleY / 65536 + xShiftY;
    end;
end;

Procedure TTransformationLayer.GetAbsoluteToLocal(var X,Y:integer);
 var FP:TFloatPoint;
 begin
  FTempTransformation.Clear;
  FTempTransformation.Translate(-FPivotPoint.X,-FPivotPoint.Y);
  FTempTransformation.Rotate(0, 0, -FRotAngle);
  FTempTransformation.Skew(-FSkew.X, -FSkew.Y);
  FTempTransformation.Scale(1/FScaling.X, 1/FScaling.Y);
  FTempTransformation.Translate(FPivotPoint.X,FPivotPoint.Y);

  FP:=FTempTransformation.TransformNormal(FloatPoint(x,y));
  X:=round(FP.X);
  Y:=round(FP.Y);
 end;

Procedure TTransformationLayer.TransformPoint(var X,Y:integer);
 var FP:TFloatPoint;
     Transf: TExtAffineTransformation;
 begin
  Transf := TExtAffineTransformation.Create;

  Transf.Translate(-FPivotPoint.X,-FPivotPoint.Y);
  Transf.Scale(FScaling.X, FScaling.Y);
  Transf.Skew(FSkew.X, FSkew.Y);
  Transf.Rotate(0, 0, FRotAngle);
  Transf.Translate(FPivotPoint.X,FPivotPoint.Y);

  FP:=Transf.TransformNormal(FloatPoint(X,Y));
  Transf.Free;

  FP.X:=FP.X+fPosition.X-FPivotPoint.X/2;
  FP.Y:=FP.Y+fPosition.Y-FPivotPoint.Y/2;

  X:=round(FP.X);
  Y:=round(FP.Y);
 end;

Function TTransformationLayer.GetViewPortScale:TFloatPoint;
 var xScaleX, xScaleY: TFloat;
 begin
   result:=FloatPoint(0,0);
   if Assigned(LayerCollection) and Assigned(LayerCollection) then
    begin
      LayerCollection.GetViewportScale(xScaleX, xScaleY);
      result:=FloatPoint(xScaleX/65536,xScaleY/65536);
    end;
 end;

procedure TTransformationLayer.ResetTransformation;
begin
  Changing;
  FTransformation.Clear;
  FSkew := FloatPoint(0, 0);
  FPosition := FloatPoint(0, 0);
  FScaling := FloatPoint(1, 1);
  FPivotPoint:=FloatPoint(0, 0);
  FRotAngle := 0;
  Changed;
  DoChange;
end;

procedure TTransformationLayer.UpdateTransformation;
 var
    xScaleX, xScaleY: TFloat;
    xShiftX, xShiftY: TFloat;
begin
  FTransformation.Clear;

  FTransformation.Translate(-FPivotPoint.X, -FPivotPoint.Y);
  FTransformation.Scale(FScaling.X, FScaling.Y);
  FTransformation.Skew(FSkew.X, FSkew.Y);
  FTransFormation.Rotate(0, 0, FRotAngle);
  FTransformation.Translate(FPosition.X + FPivotPoint.X, FPosition.Y + FPivotPoint.Y);

  // Scale to viewport if activated.
  if FScaled and Assigned(LayerCollection) and Assigned(LayerCollection) then
    begin
      LayerCollection.GetViewportScale(xScaleX, xScaleY);
      LayerCollection.GetViewportShift(xShiftX, xShiftY);
      FTransformation.Scale(xScaleX / 65536, xScaleY / 65536);
      FTransformation.Translate(xShiftX, xShiftY);
    end;
end;

Procedure TTransformationLayer.MovePivotTo(const Pos:integer);
 begin
  //Nothing at this level
 end;

Function  TTransformationLayer.GetFinalSize:TPoint;
 begin
  Result:=Point(0,0);
 end;


//==================== TExtGridLayer ======================================================


constructor TExtGridLayer.Create(ALayerCollection: TLayerCollection);
begin
  inherited;
  FHorizontalGuides := TList.Create;
  FVerticalGuides := TList.Create;
  SetDefaultValues;
end;

destructor TExtGridLayer.Destroy;
begin
  FHorizontalGuides.Free;
  FVerticalGuides.Free;

  inherited;
end;

procedure TExtGridLayer.SetDefaultValues;
 begin
  FElements := [geLines..geQuarterTicks];
  FGridSize := 15;
  FMainGridColor := clMaroon32;
  FHalfTickColor := clWhite32;
  FQuaterTickColor := clWhite32;
  FSnapOptions :=[];
  FSnapThreshold := 8;
  FGuidesColor := clBlue32;
  visible:=false;
 end;

procedure TExtGridLayer.SetColor(const Index: Integer; const Value: TColor32);
begin
  Changing;
  case Index of
    0:
      FMainGridColor := Value;
    1:
      FHalfTickColor := Value;
    2:
      FQuaterTickColor := Value;
    3:
      FGuidesColor := Value;
  end;
  Changed;
end;

procedure TExtGridLayer.SetElements(const Value: TGridElements);
begin
  if FElements <> Value then
  begin
    Changing;
    FElements := Value;
    Changed;

    DoChange;
  end;
end;

procedure TExtGridLayer.SetGridSize(const Value: Integer);
begin
  if FGridSize <> Value then
  begin
    Changing;
    FGridSize := Value;
    Changed;

    DoChange;
  end;
end;

procedure TExtGridLayer.SetSnapThreshold(const Value: Integer);
begin
  FSnapThreshold := Value;
  if FSnapThreshold < 1 then
    FSnapThreshold := 1;
end;


function TExtGridLayer.DoHitTest(X, Y: Integer): Boolean;
begin
  // The grid layer is currently fully transparent for the mouse.
  // TODO: Ability to manipulate guides.
  Result := False;
end;

type
  // To access protected properties and methods.
  TLayerCollectionCast = class(TLayerCollection);

function TExtGridLayer.GetNativeSize: TSize;
var Layers: TLayerCollectionCast;
begin
  Layers := TLayerCollectionCast(LayerCollection);
  if Layers.GetOwner is TCustomImage32 then
    with TCustomImage32(Layers.GetOwner) do
    begin
      Result.cx := Bitmap.Width;
      Result.cy := Bitmap.Height;
    end
  else
  begin
    Result.cx := 1;
    Result.cy := 1;
  end;
end;

procedure TExtGridLayer.Paint(Buffer: TBitmap32);
var
  R: TFloatRect;
  IntR: TRect;
  X, Y,
  LocalGridSize,
  HalfGridSize,
  QuaterGridSize: Single;
  Stipple: TArrayOfColor32;
  I: Integer;
  TickSize: Integer;
  Scale,
  Shift: Single;
  xScaleX, xScaleY: TFloat;
  xShiftX, xShiftY: TFloat;
  NewStiple: array of TColor32;
begin
  R := GetTransformedTargetRect;
  IntR := MakeRect(R);
  LocalGridSize := FGridSize;
  if FScaled and Assigned(LayerCollection) and Assigned(LayerCollection) then
    begin
      // Currently it is assumed the viewport of the container window is scaled proportionally
      // (X and Y scale factors are equal).
      LayerCollection.GetViewportScale(xScaleX, xScaleY);
      LocalGridSize := LocalGridSize * xScaleX / 65536;
    end;

  // Set a minimum size for the grid. No need to paint it finer.
  if LocalGridSize > 0 then
  begin
    while LocalGridSize < 8 do
      LocalGridSize := 2 * LocalGridSize;
    HalfGridSize := LocalGridSize / 2;
    QuaterGridSize := HalfGridSize / 2;

    Buffer.StippleStep := FGridSize / LocalGridSize;

    with Buffer do
    begin
      // Quater distance grid.
      if (geQuarterTicks in FElements) and (AlphaComponent(FQuaterTickColor) > 0) then
      begin
        // Create a stipple pattern which takes the first "grid size" / 8th and the last "grid size" / 8th pixels
        // and makes everything else fully transparent.
        SetLength(Stipple, FGridSize);
        TickSize := Round(FGridSize / 8);
        for I := 0 to High(Stipple) do
          if (I < TickSize) or (I > FGridSize - TickSize) then
            Stipple[I] := FQuaterTickColor
          else
            Stipple[I] := 0;
        Buffer.SetStipple(Stipple);

        Y := R.Top + QuaterGridSize;
        while Y < R.Bottom do
        begin
          Buffer.StippleCounter := 0;
          HorzLineTSP(IntR.Left, Round(Y), IntR.Right);
          Y := Y + HalfGridSize;
        end;

        X := R.Left + QuaterGridSize;
        while X <= R.Right do
        begin
          Buffer.StippleCounter := 0;
          Buffer.VertLineTSP(Round(X), IntR.Top, IntR.Bottom);
          X := X + HalfGridSize;
        end;
      end;

      // Half distance grid.
      if (geHalfTicks in FElements) and (AlphaComponent(FHalfTickColor) > 0) then
      begin
        // Create a stipple pattern which takes the first "grid size" / 4th and the last "grid size" / 4th pixels
        // and makes everything else fully transparent.
        SetLength(Stipple, FGridSize);
        TickSize := Round(FGridSize / 4);
        for I := 0 to High(Stipple) do
          if (I < TickSize) or (I > FGridSize - TickSize) then
            Stipple[I] := FHalfTickColor
          else
            Stipple[I] := 0;
        Buffer.SetStipple(Stipple);

        Y := R.Top + HalfGridSize;
        while Y <= R.Bottom do
        begin
          Buffer.StippleCounter := 0;
          Buffer.HorzLineTSP(IntR.Left, Round(Y), IntR.Right);
          Y := Y + LocalGridSize;
        end;

        X := R.Left + HalfGridSize;
        while X <= R.Right do
        begin
          Buffer.StippleCounter := 0;
          Buffer.VertLineTSP(Round(X), IntR.Top, IntR.Bottom);
          X := X + LocalGridSize;
        end;
      end;

      // Main grid.
      if (geLines in FElements) and (AlphaComponent(FMainGridColor) > 0) then
      begin
        SetLength(NewStiple, 2);
        NewStiple[0] := FMainGridColor;
        NewStiple[1] := 0;
        Buffer.SetStipple(NewStiple);
        Buffer.StippleStep := 1;

        Y := R.Top;
        while Y <= R.Bottom do
        begin
          Buffer.StippleCounter := 0;
          Buffer.HorzLineTSP(IntR.Left, Round(Y), IntR.Right);
          Y := Y + LocalGridSize;
        end;
        // Draw a line as border too.
        if Y <> R.Bottom then
          Buffer.HorzLineTSP(IntR.Left, IntR.Bottom, IntR.Right);

        X := R.Left;
        while X <= R.Right do
        begin
          Buffer.StippleCounter := 0;
          Buffer.VertLineTSP(Round(X), IntR.Top, IntR.Bottom);
          X := X + LocalGridSize;
        end;
        // Draw a line as border too.
        if X <> R.Right then
          Buffer.VertLineTSP(IntR.Right, IntR.Top, IntR.Bottom);
      end;
    end;
  end;

  // Guides.
  if (geGuides in FElements) and ((FVerticalGuides.Count > 0) or (FHorizontalGuides.Count > 0)) then
  begin
    SetLength(NewStiple, 2);
    NewStiple[0] := FGuidesColor;
    NewStiple[1] := 0;
    Buffer.SetStipple(NewStiple);
    Buffer.StippleStep := 1;

    Buffer.StippleCounter := 0;
    Scale := 1;
    Shift := 0;
    if FScaled and Assigned(LayerCollection) and Assigned(LayerCollection) then
      begin
        LayerCollection.GetViewportScale(xScaleX, xScaleY);
        LayerCollection.GetViewportShift(xShiftX, xShiftY);
        Scale := xScaleX / $10000;
        Shift := xShiftX;
      end;
    for I := 0 to FVerticalGuides.Count - 1 do
      Buffer.VertLineTSP(Round(Integer(FVerticalGuides[I]) * Scale + Shift), 0, Buffer.Height);

    Buffer.StippleCounter := 0;
    Scale := 1;
    Shift := 0;
    if FScaled and Assigned(LayerCollection) and Assigned(LayerCollection) then
      begin
        LayerCollection.GetViewportScale(xScaleX, xScaleY);
        LayerCollection.GetViewportShift(xShiftX, xShiftY);
        Scale := xScaleY / $10000;
        Shift := xShiftY;
      end;
    for I := 0 to FHorizontalGuides.Count - 1 do
      Buffer.HorzLineTSP(0, Round(Integer(FHorizontalGuides[I]) * Scale + Shift), Buffer.Width);
  end;
end;

procedure TExtGridLayer.AddHorizontalGuide(Y: Integer);
begin
  Changing;
  FHorizontalGuides.Add(Pointer(Y));
  Changed;
end;

procedure TExtGridLayer.AddVerticalGuide(X: Integer);
begin
  Changing;
  FVerticalGuides.Add(Pointer(X));
  Changed;
end;

procedure TExtGridLayer.ClearGuides;
begin
  Changing;
  FHorizontalGuides.Clear;
  FVerticalGuides.Clear;
  Changed;
end;

// This method takes the given coordinates and looks for a border, guide or grid line which is within
// snap threshold distance. It returnes True if something was found and modifies the coordinates which
// belong to the snap point.
// Coordinates must be in layer space.
function TExtGridLayer.Snap(var P: TFloatPoint): Boolean;
var
  I: Integer;
  Size: TSize;
  XFound,
  YFound: Boolean;
  LocalX,
  LocalY: Integer;

begin
  XFound := False;
  YFound := False;

  // Check the image borders first.
  if soSnapBorders in FSnapOptions then
  begin
    Size := GetNativeSize;
    if Abs(P.X) <= FSnapThreshold then
    begin
      P.X := 0;
      XFound := True;
    end
    else
      if Abs(P.X - Size.cx) <= FSnapThreshold then
      begin
        P.X := Size.cx;
        XFound := True;
      end;

    if Abs(P.Y) <= FSnapThreshold then
    begin
      P.Y := 0;
      YFound := True;
    end
    else
      if Abs(P.Y - Size.cy) <= FSnapThreshold then
      begin
        P.Y := Size.cy;
        YFound := True;
      end;
  end;

  // Check guides for snap.
  if (soSnapGuides in FSnapOptions) and not (XFound and YFound) then
  begin
    for I := 0 to FHorizontalGuides.Count - 1 do
      if Abs(Integer(FHorizontalGuides[I]) - P.Y) <= SnapThreshold then
      begin
        P.Y := Integer(FHorizontalGuides[I]);
        YFound := True;
        Break;
      end;

    for I := 0 to FVerticalGuides.Count - 1 do
      if Abs(Integer(FVerticalGuides[I]) - P.X) <= SnapThreshold then
      begin
        P.X := Integer(FVerticalGuides[I]);
        XFound := True;
        Break;
      end;
  end;

  // If no snap was found yet then try the grid.
  if (soSnapGrid in FSnapOptions) and not (XFound and YFound) then
  begin
    if not XFound then
    begin
      LocalX := Round(P.X) mod FGridSize;
      if LocalX <= SnapThreshold then
      begin
        P.X := Round(P.X) div FGridSize * FGridSize;
        XFound := True;
      end;
      if (FGridSize - LocalX) <= SnapThreshold then
      begin
        P.X := ((Round(P.X) div FGridSize) + 1) * FGridSize;
        XFound := True;
      end;
    end;

    if not YFound then
    begin
      LocalY := Round(P.Y) mod FGridSize;
      if LocalY <= SnapThreshold then
      begin
        P.Y := Round(P.Y) div FGridSize * FGridSize;
        YFound := True;
      end;
      if (FGridSize - LocalY) <= SnapThreshold then
      begin
        P.Y := ((Round(P.Y) div FGridSize) + 1) * FGridSize;
        YFound := True;
      end;
    end;
  end;
  Result := XFound or YFound;
end;

procedure TExtGridLayer.RemoveHorizontalGuide(Y: Integer);
begin
  Changing;
  FHorizontalGuides.Remove(Pointer(Y));
  Changed;
end;

procedure TExtGridLayer.RemoveVerticalGuide(X: Integer);
begin
  Changing;
  FVerticalGuides.Remove(Pointer(X));
  Changed;
end;

//=================== TPropertyLayer ==================================================

procedure TPropertyLayer.SetDrawMode(const Value: TLayerDrawMode);
begin
  if FDrawMode <> Value then
  begin
    Changing;
    FDrawMode := Value;
    Changed;
  end;
end;

procedure TPropertyLayer.SetName(const Value: WideString);
begin
  if FName <> Value then
  begin
    Changing;
    FName := Value;
    Changed;
  end;
end;

//================ TExtTextLayer ===========================================================

constructor TExtTextLayer.Create(LayerCollection: TLayerCollection);
begin
  inherited;
  FTextColor := clBlack32;
end;

procedure TExtTextLayer.SetText(const Value: WideString);
begin
  if FText <> Value then
  begin
    Changing;
    FText := Value;
    Changed;
  end;
end;

procedure TExtTextLayer.SetTextColor(const Value: TColor32);
begin
  if FTextColor <> Value then
  begin
    Changing;
    FTextColor := Value;
    Changed;
  end;
end;


//==================== TExtBitmapLayer ===================================================

constructor TExtBitmapLayer.Create(ALayerCollection: TLayerCollection);
begin
  inherited;
  FBitmap := TBitmap32.Create;
  FBitmap.OnChange := BitmapChanged;
end;

destructor TExtBitmapLayer.Destroy;
begin
  FBitmap.Free;
  inherited;
end;

procedure TExtBitmapLayer.BitmapChanged(Sender: TObject);
begin
  Changing;
  FTransformation.SrcRect := FloatRect(0, 0, Bitmap.Width - 1, Bitmap.Height - 1);
  Changed;
end;

procedure TExtBitmapLayer.SetBitmap(Value: TBitmap32);
begin
  Changing;
  FBitmap.Assign(Value);
  Changed;
end;

procedure TExtBitmapLayer.SetCropped(Value: Boolean);
begin
  if Value <> FCropped then
  begin
    Changing;
    FCropped := Value;
    Changed;
  end;
end;

function TExtBitmapLayer.DoHitTest(X, Y: Integer): Boolean;
var  BX, BY: Integer;
begin
  if not FTransformation.TransformValid then
    FTransformation.PrepareTransform;
  with FTransformation do
    Transform(X, Y, BX, BY); // BX,BY - in 'FBitmap' coordinates

  Result := PtInRect(Rect(0, 0, Bitmap.Width, Bitmap.Height), Point(BX, BY));
  if Result and AlphaHit and (Bitmap.PixelS[BX, BY] and $FF000000 = 0) then
    Result := False;
  // TODO: cropping
end;

function TExtBitmapLayer.GetNativeSize: TSize;
begin
  Result.cx := FBitmap.Width;
  Result.cy := FBitmap.Height;
end;

procedure TExtBitmapLayer.Paint(Buffer: TBitmap32);
begin
  UpdateTransformation;

  // TODO: cropping
  if not FTransformation.TransformValid then FTransformation.PrepareTransform;

  XTransform(Buffer, FBitmap, FTransformation);
end;

procedure TExtBitmapLayer.PaintTo(Buffer: TBitmap32);
// Paints the bitmap to the given buffer.
var
  Transformation: TExtAffineTransformation;
begin
  Transformation := nil;
  try
    GetLayerTransformation(Transformation);
    Transformation.SrcRect := FloatRect(0, 0, Bitmap.Width - 1, Bitmap.Height - 1);
    Transformation.PrepareTransform;
    XTransform(Buffer, FBitmap, Transformation);
  finally
    Transformation.Free;
  end;
end;

procedure TExtBitmapLayer.PaintTo(Buffer: TBitmap32; const X,Y:integer);
// Paints the bitmap to the given buffer.
var
  Transformation: TExtAffineTransformation;
begin
  Transformation := nil;
  try
    GetLayerTransformation(Transformation);
    Transformation.SrcRect := FloatRect(0, 0, Bitmap.Width - 1, Bitmap.Height - 1);
    Transformation.PrepareTransform;
    Transformation.Translate(X,Y);
    XTransform(Buffer, FBitmap, Transformation);
  finally
    Transformation.Free;
  end;
end;

procedure TExtBitmapLayer.PaintTo(Buffer: TBitmap32; const R: TRect);
// Paints the bitmap to the given buffer using the position and size/location given in R.
var
  Transformation: TExtAffineTransformation;
begin
  Transformation := nil;
  try
    GetLayerTransformation(Transformation);
    Transformation.SrcRect := FloatRect(0, 0, Bitmap.Width - 1, Bitmap.Height - 1);
    Transformation.PrepareTransform;
    Transformation.Scale((R.Right - R.Left) / Bitmap.Width, (R.Bottom - R.Top) / Bitmap.Height);
    Transformation.Translate(R.Left, R.Top);
    XTransform(Buffer, FBitmap, Transformation);
  finally
    Transformation.Free;
  end;
end;

Procedure TExtBitmapLayer.MovePivotTo(const Pos:integer);
 var W,H:integer;
 begin
  W:=Bitmap.Width;
  H:=Bitmap.Height;
   case pos of
      0:PivotPoint:=FloatPoint(W div 2,H div 2);  //center
      1:PivotPoint:=FloatPoint(0,0);              //Top-left
      2:PivotPoint:=FloatPoint(W div 2,0);
      3:PivotPoint:=FloatPoint(W,0);
      4:PivotPoint:=FloatPoint(W,H div 2);
      5:PivotPoint:=FloatPoint(W,H );
      6:PivotPoint:=FloatPoint(W div 2,H);
      7:PivotPoint:=FloatPoint(0,H);
      8:PivotPoint:=FloatPoint(0,H div 2);
  end;
 end;

Function  TExtBitmapLayer.GetFinalSize:TPoint;
var  Radians: Extended;
     ScaledWidth, ScaledHeight: Extended;
     W,H:integer;
begin
  W:=Bitmap.Width;
  H:=Bitmap.Height;

  Radians := DegToRad(-RotAngle);
  ScaledWidth := W * Scaling.X;
  ScaledHeight := H * Scaling.Y;

  Result := Point(Ceil(Abs(ScaledWidth * Cos(Radians)) + Abs(ScaledHeight * Sin(Radians))),
                  Ceil(Abs(ScaledWidth * Sin(Radians)) + Abs(ScaledHeight * Cos(Radians))));

  Result.X := Result.X - (Result.X mod 2);
  Result.Y := Result.Y - (Result.Y mod 2);
end;

//====================== TExtCustomRBLayer ===================================================

constructor TExtCustomRBLayer.Create(LayerCollection: TLayerCollection);
begin
  inherited ;
  FHandleFrame := clBlack;
  FHandleFill := clWhite;
  SetFrameStipple([clWhite32, clWhite32, clBlack32, clBlack32]);
  FFrameStippleStep := 1;
  FFrameStippleCounter := 0;
  FOptions := DefaultRubberbandOptions;
  FHandleSize := 3;
  LayerOptions := LOB_VISIBLE or LOB_MOUSE_EVENTS;
  FThreshold := 8;
  FSize.cx := 1;
  FSize.cy := 1;
end;


destructor TExtCustomRBLayer.Destroy;
begin
  inherited;
end;

procedure TExtCustomRBLayer.SetHandleFill(const Value: TColor32);
begin
  if FHandleFill <> Value then
  begin
    FHandleFill := Value;
    TLayerCollectionCast(LayerCollection).GDIUpdate;
  end;
end;

procedure TExtCustomRBLayer.SetHandleFrame(const Value: TColor32);
begin
  if FHandleFrame <> Value then
  begin
    FHandleFrame := Value;
    TLayerCollectionCast(LayerCollection).GDIUpdate;
  end;
end;

procedure TExtCustomRBLayer.SetHandleSize(Value: Integer);
begin
  if Value < 1 then
    Value := 1;
  if FHandleSize <> Value then
  begin
    FHandleSize := Value;
    TLayerCollectionCast(LayerCollection).GDIUpdate;
  end;
end;

procedure TExtCustomRBLayer.SetOptions(const Value: TExtRubberBandOptions);
begin
  if FOptions <> Value then
  begin
    Changing;
    FOptions := Value;
    Changed;
    DoChange;
  end;
end;

procedure TExtCustomRBLayer.SetOuterColor(const Value: TColor32);
begin
  if FOuterColor <> Value then
  begin
    Changing;
    FOuterColor := Value;
    Changed;
    DoChange;
  end;
end;

procedure TExtCustomRBLayer.SetSize(const Value: TSize);
begin
  Changing;
  FSize := Value;
  Changed;
  DoChange;
end;

function TExtCustomRBLayer.DoHitTest(X, Y: Integer): Boolean;
begin
  Result := Visible and (GetHitCode(x,y,[])<>rdsNone);
end;

procedure TExtCustomRBLayer.FillOuter(Buffer: TBitmap32; OuterRect: TRect; Contour: TContour);
var Polygon: TArrayOfArrayOfFixedPoint;

begin
  SetLength(Polygon, 2);
  SetLength(Polygon[0], 5);
  SetLength(Polygon[1], 5);

  // The outer rectangle. It must be given with counter clock winding.
  Polygon[0][0] := FixedPoint(OuterRect.TopLeft);
  Polygon[0][1] := FixedPoint(OuterRect.Right, OuterRect.Top);
  Polygon[0][2] := FixedPoint(OuterRect.BottomRight);
  Polygon[0][3] := FixedPoint(OuterRect.Left, OuterRect.Bottom);
  Polygon[0][4] := FixedPoint(OuterRect.TopLeft);

  // The inner rectangle is drawn with reverse winding.
  Polygon[1][0] := Contour[3];
  Polygon[1][1] := Contour[2];
  Polygon[1][2] := Contour[1];
  Polygon[1][3] := Contour[0];
  Polygon[1][4] := Contour[3];
  PolyPolygonTS(Buffer, Polygon, FOuterColor, pfWinding);
end;


function TExtCustomRBLayer.GetCursorDirection(X, Y: Integer; AxisTolerance: Integer;
                                              State: TRubberbandDragState): TCursorDirection;

// Returns, depending on X and Y as well as the current transformation, the direction either relative to
// the image bounds or the rotation pivot (if not within the bounds).
// State is used to determine the pivot point (virtual center), relative to which the orientation is
// to be calculated.
// AxisTolerance determines which angle difference from a coordinate axis is still to be considered as
// aligned.

const
  Directions: array[0..18] of TCursorDirection = (
    cdNorth, cdNorthEast, cdEast, cdSouthEast, cdSouth, cdSouthWest,
    cdWest, cdNorthWest, cdNorth, cdNorthEast, cdEast,
    cdSouthEast, cdSouth, cdSouthWest, cdWest, cdNorthWest,
    cdNorth, cdNorthEast, cdEast);

  SheerDirections: array[0..3] of TCursorDirection = (cdNorth, cdEast, cdSouth, cdWest);

var
  dX, dY: Integer;
  PivotX,
  PivotY: Integer;
  Angle: Integer;
  Index: Integer;

begin
  Result := cdNotUsed;

  case State of
    rdsSheerN..rdsSheerW:
      Result := SheerDirections[Ord(State) - 11];
    rdsResizeN..rdsResizeNW:
      begin
        Index := Ord(State) + 5;
        Angle := Round(FRotAngle) mod 180;
        if Angle > AxisTolerance then
        begin
          Dec(Index);
          if Angle > 90 - AxisTolerance then
          begin
            Dec(Index);
            if Angle > 90 + AxisTolerance then
            begin
              Dec(Index);
              if Angle > 180 - AxisTolerance then
                Index := Ord(State) + 5;
            end;
          end;
        end
        else
          if Angle < -AxisTolerance then
          begin
            Inc(Index);
            if Angle < -90 + AxisTolerance then
            begin
              Inc(Index);
              if Angle < -90 - AxisTolerance then
              begin
                Inc(Index);
                if Angle < -180 + AxisTolerance then
                  Index := Ord(State) + 5;
              end;
            end;
          end;
        Result := Directions[Index];
      end;
    rdsRotate:
      begin
        // Transform coordinates into local space.
        if not FTransformation.TransformValid then FTransformation.PrepareTransform;

        with FTransformation do
        begin
          PivotX := Round(Matrix[0, 0] * FPivotPoint.X + Matrix[1, 0] * FPivotPoint.Y + Matrix[2, 0]);
          PivotY := Round(Matrix[0, 1] * FPivotPoint.X + Matrix[1, 1] * FPivotPoint.Y + Matrix[2, 1]);
        end;

        dX := Round(X - PivotX);
        dY := Round(Y - PivotY);
        if dX = 0 then
        begin
          if dY < 0 then
            Result := cdNorth
          else
            Result := cdSouth;
        end
        else
          if dY = 0 then
          begin
            if dX > 0 then
              Result := cdEast
            else
              Result := cdWest;
          end
          else
          begin
            // Everything within AxisTolerance from an axis is considered as would the axis have been hit.
            // Check the axes (with tolerance) first before checking all other possible directions.
            Angle := Round(RadToDeg(ArcTan2(dY, dX)));
            if (-180 <= Angle) and (Angle < -180 + AxisTolerance) then
              Result := cdWest
            else
              if (-90 - AxisTolerance <= Angle) and (Angle < -90 + AxisTolerance) then
                Result := cdNorth
              else
                if (-AxisTolerance <= Angle) and (Angle < AxisTolerance) then
                  Result := cdEast
                else
                  if (90 - AxisTolerance <= Angle) and (Angle < 90 + AxisTolerance) then
                    Result := cdSouth
                  else
                    if (180 - AxisTolerance <= Angle) and (Angle < 180) then
                      Result := cdWest
                    else // No axis aligned direction, check the others.
                      if (-180 + AxisTolerance <= Angle) and (Angle < -90 - AxisTolerance) then
                        Result := cdNorthWest
                      else
                        if (-90 + AxisTolerance <= Angle) and (Angle < -AxisTolerance) then
                          Result := cdNorthEast
                        else
                          if (AxisTolerance <= Angle) and (Angle < 90 - AxisTolerance) then
                            Result := cdSouthEast
                          else
                            if (90 + AxisTolerance <= Angle) and (Angle < 180 - AxisTolerance) then
                              Result := cdSouthWest
                            else
                              Result := cdNotUsed;
          end;
      end;
  end;
end;

// Determines the possible drag state, which the layer could enter.
function TExtCustomRBLayer.GetHitCode(X, Y: Integer; Shift: TShiftState): TRubberbandDragState;
var
  dX, dY: Single;
  LocalX,
  LocalY: Integer;
  LocalThresholdX,
  LocalThresholdY: Integer;
  NearTop,
  NearRight,
  NearBottom,
  NearLeft: Boolean;
  xScaleX, xScaleY: TFloat;

begin
  Result := rdsNone;

  // Transform coordinates into local space.
  if not FTransformation.TransformValid then
    FTransformation.PrepareTransform;

  FTransformation.Transform(X, Y, LocalX, LocalY);

  LocalThresholdX := Round(FThreshold / FScaling.X);
  LocalThresholdY := Round(FThreshold / FScaling.Y);

  // ct9999  ref to RevScaleX ,RevScaleY  must test

  if FScaled and Assigned(LayerCollection) and Assigned(LayerCollection) then
    begin
      LayerCollection.GetViewportScale(xScaleX, xScaleY);

      LocalThresholdX := Round(LocalThresholdX * xScaleX/$10000);
      LocalThresholdY := Round(LocalThresholdY * xScaleY/$10000);
    end;

  // Check rotation Pivot first.
  dX := Round(LocalX - FPivotPoint.X);
  if Abs(dX) < LocalThresholdX then
    dX := 0;
  dY := Round(LocalY - FPivotPoint.Y);
  if Abs(dY) < LocalThresholdY then
    dY := 0;

  // Special case: rotation Pivot is hit.
  if (dX = 0) and (dY = 0) and (rboAllowPivotMove in FOptions) then
    Result := rdsMovePivot
  else
  begin
    // Check if the mouse is within the bounds.
    if (LocalX >= -LocalThresholdX) and
       (LocalX <= FSize.cx + LocalThresholdX) and
       (LocalY >= -LocalThresholdY) and
       (LocalY <= FSize.cy + LocalThresholdY) then
    begin
      Result := rdsMoveLayer;    //==============================================================================

      NearLeft := Abs(LocalX) <= LocalThresholdX;
      NearRight := Abs(FSize.cx - LocalX) <= LocalThresholdX;
      NearTop := Abs(LocalY) <= LocalThresholdY;
      NearBottom := Abs(FSize.cy - LocalY) <= LocalThresholdY;

      if rboAllowCornerResize in FOptions then
      begin
        // Check borders.
        if NearTop then
        begin
          if NearRight then
            Result := rdsResizeNE
          else
            if NearLeft then
              Result := rdsResizeNW;
        end
        else
          if NearBottom then
          begin
            if NearRight then
              Result := rdsResizeSE
            else
              if NearLeft then
                Result := rdsResizeSW;
          end;
      end;

      if (Result = rdsMoveLayer) and (rboAllowEdgeResize in FOptions) then
      begin
        // Check for border if no corner hit.
        if NearTop then
          Result := rdsResizeN
        else
          if NearBottom then
            Result := rdsResizeS
          else
            if NearRight then
              Result := rdsResizeE
            else
              if NearLeft then
                Result := rdsResizeW;
      end;

      // If the user holds down the control key then sheering becomes active (only for edges).
      if (ssCtrl in Shift) and (rboAllowSheering in FOptions)then
      begin
        case Result of
          rdsResizeN: Result := rdsSheerN;
          rdsResizeE: Result := rdsSheerE;
          rdsResizeS: Result := rdsSheerS;
          rdsResizeW: Result := rdsSheerW;
        end;

      end;
    end
    else
    begin
      // Mouse is not within the bounds. So if rotating is allowed we can return the rotation state.

      if rboAllowRotation in FOptions then
       begin
          if (LocalX >= -LocalThresholdX-FHandleSize-3) and
             (LocalX <= FSize.cx + LocalThresholdX+FHandleSize+3) and
             (LocalY >= -LocalThresholdY-FHandleSize-3) and
             (LocalY <= FSize.cy + LocalThresholdY+FHandleSize+3) then
            begin
              Result := rdsRotate;
            end;
       end;

    end;
  end;
end;

function TExtCustomRBLayer.GetNativeSize: TSize;
begin
  Result := FSize;
end;


procedure TExtCustomRBLayer.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FIsDragging := FDragState <> rdsNone;
  if FIsDragging then
  begin
    FOldPosition := FPosition;
    FOldScaling := FScaling;
    FOldPivot := FPivotPoint;
    FOldSkew := FSkew;
    FOldAngle := FRotAngle;
    FDragPos := Point(X, Y);
  end;
  inherited;
end;

procedure TExtCustomRBLayer.MouseMove(Shift: TShiftState; X, Y: Integer);
const
  MoveCursor: array [TCursorDirection] of TCursor = (
    crDefault,
    crGrMovePointNS,    // cdNorth
    crGrMovePointNESW,  // cdNorthEast
    crGrMovePointWE,    // cdEast
    crGrMovePointNWSE,  // cdSouthEast
    crGrMovePointNS,    // cdSouth
    crGrMovePointNESW,  // cdSouthWest
    crGrMovePointWE,    // cdWest
    crGrMovePointNWSE   // cdNorthWest
  );

  RotateCursor: array [TCursorDirection] of TCursor = (
    crDefault,
    crGrRotateN,        // cdNorth
    crGrRotateNE,       // cdNorthEast
    crGrRotateE,        // cdEast
    crGrRotateSE,       // cdSouthEast
    crGrRotateS,        // cdSouth
    crGrRotateSW,       // cdSouthWest
    crGrRotateW,        // cdWest
    crGrRotateNW        // cdNorthWest
  );

  SheerCursor: array [TCursorDirection] of TCursor = (
    crDefault,
    crGrArrowMoveWE,    // cdNorth
    crDefault,          // cdNorthEast
    crGrArrowMoveNS,    // cdEast
    crDefault,          // cdSouthEast
    crGrArrowMoveWE,    // cdSouth
    crDefault,          // cdSouthWest
    crGrArrowMoveNS,    // cdWest
    crDefault           // cdNorthWest
  );

var
  ScaleRatioX,
  ScaleRatioY,
  dX, dY,
  PivotX,
  PivotY,
  TransX,
  TransY: Single;

  Angle,
  Sine,                      // Sine and cosine of the current rotation angle.
  Cosine,
  RevSine,                   // Sine and cosine of the negative current rotation angle.
  RevCosine,
  T: Extended;
  DirX,
  DirY: Integer;             // Used to calculate the correct direction of scale/translation.
  Snapped: Boolean;

  LastPosition: TFloatPoint;
  LastRotation: Single;
  LastScaling: TFloatPoint;
  xScaleX, xScaleY: TFloat;

begin
  if not FTransformation.TransformValid then FTransformation.PrepareTransform;

if not FIsDragging then   //========================= IsDragging=false mouse only move to layer ======================
  begin
    FDragState := GetHitCode(X, Y, Shift);

    //....Stop TempPivotVisible function ...............
    if (fTempPivotVisible) and (FDragState<>rdsMovePivot) then
     begin
      fTempPivotVisible:=false;
      Update;
    end;
    //.................................................

    case FDragState of
      rdsNone:      Cursor := crDefault;
      rdsRotate:    Cursor := RotateCursor[GetCursorDirection(X, Y, 15, FDragState)];
      rdsMoveLayer: Cursor := crGrArrow;
      rdsSheerN..rdsSheerW:
                    Cursor := SheerCursor[GetCursorDirection(X, Y, 15, FDragState)];

      rdsMovePivot: begin
                      //..... Start TempPivotVisible function ................
                      //if pivot is not visible at options then if mouse is on pivot pivot must be visible
                      if NOT (rboShowPivot in Foptions) then
                       if fTempPivotVisible=false then
                        begin
                          fTempPivotVisible:=true;
                          Update;
                        end;
                      //......................................................
                      Cursor := crGrMoveCenter;
                    end;
    else
      Cursor := MoveCursor[GetCursorDirection(X, Y, 15, FDragState)];
    end;
  end else             //========================= IsDragging=true mouse left button is down =============================
   begin

     //..... Start TempPivotVisible function ..................
     if NOT (rboShowPivot in Foptions) then
      if fTempPivotVisible=false then fTempPivotVisible:=true;
     //........................................................

    // Store the current values to learn at the end whether there really were changes.
    // Because of the coordinate snapping it could well be that nothing changes for several mouse moves.
    LastPosition := FPosition;
    LastRotation := FRotAngle;
    LastScaling := FScaling;

    Changing;

    dX := X - FDragPos.X;
    dY := Y - FDragPos.Y;
    // Account for the viewport scale.
    // ct9999 ref to RefScaleX
    if FScaled and Assigned(LayerCollection) and Assigned(LayerCollection) then
      begin
        LayerCollection.GetViewportScale(xScaleX, xScaleY);
        dX := dX * xScaleX / $10000;
        dY := dY * xScaleY / $10000;
      end;
    // Calculate sine and cosine values in advance that are used to convert mouse coordinates to image coordinates.
    // Keep in mind that under Windows -Y is up so the rotations have to be mirrored.
    SinCos(DegToRad(-FRotAngle), Sine, Cosine);
    RevSine := -Sine;
    RevCosine := Cosine;

    // Transform mouse coordinates into layer space.
    TransX := Cosine * dX + Sine * dY;
    TransY := -Sine * dX + Cosine * dY;

    // Scale values for local coordinates determined by the ratio between top/left border to pivot position
    // (which is the center when the image gets scaled). Note: the pivot point is local to the image, so the
    // image's position doesn't matter.
    ScaleRatioX := FOldPivot.X / FSize.cx;
    ScaleRatioY := FOldPivot.Y / FSize.cy;

    DirX := 1;
    DirY := 1;

    if ssAlt in Shift then FPosition := FOldPosition;

  //....... FDragState case .......................
  case FDragState of
      rdsMoveLayer:
        begin
          FPosition.X := FOldPosition.X + dX;
          FPosition.Y := FOldPosition.Y + dY;
        end;
      rdsMovePivot:
        begin
          FPivotPoint.X := FOldPivot.X + dX;
          FPivotPoint.Y := FOldPivot.Y + dY;
        end;
      rdsResizeN:
        begin
          // Remove horizontal part.
          TransX := 0;
          DirY := -1;
          ScaleRatioY := 1 - ScaleRatioY;
        end;
      rdsResizeNE:
        begin
          DirY := -1;
          ScaleRatioY := 1 - ScaleRatioY;
        end;
      rdsResizeE:
        TransY := 0;
      rdsResizeSE: // Nothing special to do here.
        ;
      rdsResizeS:
        TransX := 0;
      rdsResizeSW:
        begin
          ScaleRatioX := 1 - ScaleRatioX;
          DirX := -1;
        end;
      rdsResizeW:
        begin
          // Remove vertical part.
          TransY := 0;
          ScaleRatioX := 1 - ScaleRatioX;
          DirX := -1;
        end;
      rdsResizeNW:
        begin
          DirX := -1;
          DirY := -1;
          ScaleRatioX := 1 - ScaleRatioX;
          ScaleRatioY := 1 - ScaleRatioY;
        end;
      rdsSheerS,
      rdsSheerN:
        begin
          if FDragState = rdsSheerN then
            DirX := -1;
          FSkew.X := FOldSkew.X + DirX * TransX / FSize.cx;
          if not (ssAlt in Shift) then
          begin
            dX := RevCosine * TransX * ScaleRatioX;
            dY := -RevSine * TransX * ScaleRatioX;

            FPosition.X := FOldPosition.X + dX;
            FPosition.Y := FOldPosition.Y + dY;
          end;
        end;
      rdsSheerW,
      rdsSheerE:
        begin
          if FDragState = rdsSheerW then
            DirY := -1;
          FSkew.Y := FOldSkew.Y + DirY * TransY / FSize.cy;
          if not (ssAlt in Shift) then
          begin
            dX := RevSine * TransY * ScaleRatioY;
            dY := RevCosine * TransY * ScaleRatioY;

            FPosition.X := FOldPosition.X + dX;
            FPosition.Y := FOldPosition.Y + dY;
          end;
        end;
      rdsRotate:
        begin

          // Update cursor properly.
          Cursor := RotateCursor[GetCursorDirection(X, Y, 15, FDragState)];
          // Calculate the angle opened by the old position, the new position and the pivot point.
          with FTransformation do
          begin
            PivotX := Matrix[0, 0] * FPivotPoint.X + Matrix[1, 0] * FPivotPoint.Y + Matrix[2, 0];
            PivotY := Matrix[0, 1] * FPivotPoint.X + Matrix[1, 1] * FPivotPoint.Y + Matrix[2, 1];
          end;

          Angle := RadToDeg(ArcTan2(FDragPos.Y - PivotY, FDragPos.X - PivotX) - ArcTan2(Y - PivotY, X - PivotX));
          FRotAngle := FOldAngle + Angle;

          // Limit rotations to multiple of the angle raster if the shift key is pressed.
          if ssShift in Shift then  FRotAngle := Round(FRotAngle / 15) * 15;
          if FRotAngle <= -180 then    FRotAngle := FRotAngle + 360;
          if FRotAngle >= 180 then     FRotAngle := FRotAngle - 360;

        end;
    end;
  //...... End Case of FDragState ..........................

    if FDragState in [rdsResizeN..rdsResizeNW] then
    begin
      // Recalculate transformed coordinates if the user requests a constant ratio.
      if (ssShift in Shift) and (FDragState in [rdsResizeNE, rdsResizeSE, rdsResizeSW, rdsResizeNW]) then
      begin
        // To achieve a constant ratio we calculate the projection of the actual mouse position onto the
        // diagonal vector depending on the four corners/directions.
        T := (DirX * TransX * FSize.cx + DirY * TransY * FSize.cy) / (FSize.cx * FSize.cx + FSize.cy * FSize.cy);
        TransX := DirX * T * FSize.cx;
        TransY := DirY * T * FSize.cy;
      end;

      if not (ssAlt in Shift) then
      begin
        // Transform local coordinates back to mouse space. ScaleRatioX/Y are used to weight the coordinates
        // in the same manner as the ratio between the pivot point and the layer bounds.
        // This is also necessary to let the bounds visually follow the mouse correctly.
        dX := RevCosine * TransX * ScaleRatioX + RevSine * TransY * ScaleRatioY;
        dY := -RevSine * TransX * ScaleRatioX + RevCosine * TransY * ScaleRatioY;

        FPosition.X := FOldPosition.X + dX;
        FPosition.Y := FOldPosition.Y + dY;
        FScaling.X := FOldScaling.X + DirX * TransX / FSize.cx;
        FScaling.Y := FOldScaling.Y + DirY * TransY / FSize.cy;
      end
      else
      begin
        if ScaleRatioX < 1 then
          FScaling.X := FOldScaling.X + DirX * TransX / FSize.cx / (1 - ScaleRatioX);
        if ScaleRatioY < 1 then
          FScaling.Y := FOldScaling.Y + DirY * TransY / FSize.cy / (1 - ScaleRatioY);
      end;
    end;

    if FDragState = rdsMoveLayer then
    begin
      // Snap movements to grid. Resizing is not yet covered here.
      if Assigned(FGridLayer) then
      begin
        Snapped := SnapPosition;
        if FDragState = rdsMoveLayer then
          if Snapped then
            Cursor := crGrArrowHollow else
            Cursor := crGrArrow;
      end;
    end;

    Changing;
    UpdateChildLayer;
    Changed;
    DoChange;

end;  //========================= of IsDragging if =====================================

inherited;
end;

procedure TExtCustomRBLayer.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FIsDragging := False;
  inherited;
end;

procedure TExtCustomRBLayer.Notification(ALayer: TCustomLayer);
begin
  inherited;
end;

procedure TExtCustomRBLayer.Paint(Buffer: TBitmap32);
var Contour: TContour;

  //------------ local functions --------------------------------------------
  // Constructs four vertex points from the given coordinates and sizes and
  // transforms them into a contour structure, which corresponds to the
  // current transformations.
  procedure CalculateContour(X, Y, W, H: Single);
   var R: TFloatRect;
  begin
    R.TopLeft := FloatPoint(X, Y);
    R.BottomRight := FloatPoint(X + W, Y + H);

    with FTransformation do
    begin
      // Upper left
      Contour[0].X := Fixed(Matrix[0, 0] * R.Left + Matrix[1, 0] * R.Top + Matrix[2, 0]);
      Contour[0].Y := Fixed(Matrix[0, 1] * R.Left + Matrix[1, 1] * R.Top + Matrix[2, 1]);

      // Upper right
      Contour[1].X := Fixed(Matrix[0, 0] * R.Right + Matrix[1, 0] * R.Top + Matrix[2, 0]);
      Contour[1].Y := Fixed(Matrix[0, 1] * R.Right + Matrix[1, 1] * R.Top + Matrix[2, 1]);

      // Lower right
      Contour[2].X := Fixed(Matrix[0, 0] * R.Right + Matrix[1, 0] * R.Bottom + Matrix[2, 0]);
      Contour[2].Y := Fixed(Matrix[0, 1] * R.Right + Matrix[1, 1] * R.Bottom + Matrix[2, 1]);

      // Lower left
      Contour[3].X := Fixed(Matrix[0, 0] * R.Left + Matrix[1, 0] * R.Bottom + Matrix[2, 0]);
      Contour[3].Y := Fixed(Matrix[0, 1] * R.Left + Matrix[1, 1] * R.Bottom + Matrix[2, 1]);
    end;
  end;

  //...........................................................................
  procedure DrawContour;
  begin
    with Buffer do
    begin
      MoveToX(Contour[0].X, Contour[0].Y);
      LineToXSP(Contour[1].X, Contour[1].Y);
      LineToXSP(Contour[2].X, Contour[2].Y);
      LineToXSP(Contour[3].X, Contour[3].Y);
      LineToXSP(Contour[0].X, Contour[0].Y);
    end;
  end;

  //...........................................................................
  // Special version for handle vertex calculation. Handles are fixed sized and not rotated.
  procedure DrawHandle(X, Y: Single);
  var XNew, YNew: Single;
  begin
    with FTransformation do
    begin
      XNew := Matrix[0, 0] * X + Matrix[1, 0] * Y + Matrix[2, 0];
      YNew := Matrix[0, 1] * X + Matrix[1, 1] * Y + Matrix[2, 1];
    end;

    Buffer.FillRectS(Round(XNew - FHandleSize), Round(YNew - FHandleSize),
                     Round(XNew + FHandleSize), Round(YNew + FHandleSize),
                     FHandleFill);

    Buffer.FrameRectS(Round(XNew - FHandleSize), Round(YNew - FHandleSize),
                      Round(XNew + FHandleSize), Round(YNew + FHandleSize),
                      FHandleFrame);
  end;

  //...........................................................................
  // Special version for the pivot image. Also this image is neither rotated nor scaled.
  procedure DrawPivot(const X, Y: Single);
  var XNew, YNew: Single;
      xScaleX, xScaleY: TFloat;
      xShiftX, xShiftY: TFloat;
  begin
    if FScaled and Assigned(LayerCollection) and Assigned(LayerCollection) then
      begin
        LayerCollection.GetViewportScale(xScaleX, xScaleY);
        LayerCollection.GetViewportShift(xShiftX, xShiftY);
        XNew := X * xScaleX / $10000 + xShiftX;
        YNew := Y * xScaleY / $10000 + xShiftY;
      end
    else
    begin
      XNew := X;
      YNew := Y;
    end;
    {$IFDEF MSWINDOWS}
      DrawIconEx(Buffer.Handle, Round(XNew - 8), Round(YNew - 8), Screen.Cursors[crGrCircleCross], 0, 0, 0, 0, DI_NORMAL);
    {$ELSE}

    {$ENDIF}
  end;

  //----------- end local functions -----------------------------------------

var Cx, Cy: Single;
begin

  UpdateTransformation;
  CalculateContour(0, 0, FSize.cx, FSize.cy);

  if AlphaComponent(FOuterColor) > 0 then  //FillOuter
    FillOuter(Buffer, Rect(0, 0, Buffer.Width, Buffer.Height), Contour);

  if rboShowFrame in FOptions then    //Frame
  begin
    Buffer.SetStipple(FFrameStipplePattern);
    Buffer.StippleCounter := 0;
    Buffer.StippleStep := FFrameStippleStep;
    Buffer.StippleCounter := FFrameStippleCounter;
    DrawContour;
  end;

  if rboShowHandles in FOptions then  // Corners Handles
  begin
    DrawHandle(0, 0);
    DrawHandle(FSize.cx, 0);
    DrawHandle(FSize.cx, FSize.cy);
    DrawHandle(0, FSize.cy);
  end;

  if rboShowHandles in FOptions then  // midle Handles
  begin
    Cx := FSize.cx / 2;
    Cy := FSize.cy / 2;
    DrawHandle(Cx, 0);
    DrawHandle(FSize.cx, Cy);
    DrawHandle(Cx, FSize.cy);
    DrawHandle(0, Cy);
  end;

  if rboAllowRotation in FOptions then  // Pivot
    if (rboShowPivot in FOptions) or fTempPivotVisible then
      DrawPivot(FPosition.X+FPivotPoint.X,FPosition.y+FPivotPoint.Y);

end;


 // This method is called if there is a grid layer assigned to the rubber band layer and transformations took place
// which require alignment to the grid.
// Result is True if there was an alignment, otherwise it is False.
// The caller is responsible to invalidate the container and the child layer (if any) to cause correct display.

function TExtCustomRBLayer.SnapPosition: Boolean;
var Transformation: TExtAffineTransformation;

  //--------------- local function --------------------------------------------

  function TrySnap(const Point: TFloatPoint): Boolean;

  var
    POld,
    PNew: TFloatPoint;
    dX,
    dY: Single;

  begin
    POld := Transformation.TransformNormal(Point);
    PNew := POld;
    Result := FGridLayer.Snap(PNew);
    dX := PNew.X - POld.X;
    dY := PNew.Y - POld.Y;
    if Result and ((Abs(dX) > Epsilon) or (Abs(dY) > Epsilon)) then
    begin
      // Apply difference of transformed point and the snapped equivalent to the main position.
      FPosition.X := FPosition.X + dX;
      FPosition.Y := FPosition.Y + dY;
      // Update transformation matrix, so following tests take the modification done here into account.
      Transformation.Translate(dX, dY);
    end;
  end;

  //--------------- end local function ----------------------------------------

var
  Size: TSize;

begin
  Transformation := nil;
  try
    // We use a local transformation to avoid frequent invalidation of the main transformation.
    GetLayerTransformation(Transformation);
    Transformation.PrepareTransform;
    Size := GetNativeSize;

    // Indepedent of the current transformations we always have to check 5 points.
    // These are the four corners and the center of the layer. Photoshop however snaps the center only
    // if the pivot point is also there. We always snap the center too.
    // We need to make all tests because there could be more than one snap point. Tests are done in reversed order.
    // That means e.g. the left upper corner is tested later than the lower right corner having so the last word
    // about snapping (which is likely what the user wants).
    // 1.) Center.
    Result := TrySnap(FloatPoint(Size.cx / 2, Size.cy / 2));
    // 2.) Lower left corner.
    if TrySnap(FloatPoint(0, Size.cy)) then
      Result := True;
    // 3.) Lower right corner.
    if TrySnap(FloatPoint(Size.cx, Size.cy)) then
      Result := True;
    // 4.) Top right corner.
    if TrySnap(FloatPoint(Size.cx, 0)) then
      Result := True;
    // 5.) Top left corner.
    if TrySnap(FloatPoint(0, 0)) then
      Result := True;
  finally
    Transformation.Free;
  end;
end;

procedure TExtCustomRBLayer.UpdateChildLayer;
begin
 // Nothing yet....
end;

procedure TExtCustomRBLayer.Cancel;
begin
    FIsDragging := False;
    FPosition := FOldPosition;
    FScaling := FOldScaling;
    FPivotPoint := FOldPivot;
    FSkew := FOldSkew;
    FRotAngle := FOldAngle;
    TotalUpdate;
end;

procedure TExtCustomRBLayer.ResetTransformation;
 begin
  inherited;
  UpdateChildLayer;
 end;

Procedure TExtCustomRBLayer.TotalUpdate;
 begin
    Changing;
    UpdateChildLayer;
    Changed;
    DoChange;
 end;

Procedure TExtCustomRBLayer.MovePivotTo(const Pos:integer);
 var W,H:integer;
 begin
  W:=Size.cx;
  H:=Size.cy;
   case pos of
      0:PivotPoint:=FloatPoint(W div 2,H div 2);
      1:PivotPoint:=FloatPoint(0,0);
      2:PivotPoint:=FloatPoint(W div 2,0);
      3:PivotPoint:=FloatPoint(W,0);
      4:PivotPoint:=FloatPoint(W,H div 2);
      5:PivotPoint:=FloatPoint(W,H );
      6:PivotPoint:=FloatPoint(W div 2,H);
      7:PivotPoint:=FloatPoint(0,H);
      8:PivotPoint:=FloatPoint(0,H div 2);
  end;
 end;

Function  TExtCustomRBLayer.GetFinalSize:TPoint;
 var  Radians: Extended;
      ScaledWidth, ScaledHeight: Extended;
      W,H:integer;
begin
  W:=Size.cx;
  H:=Size.cy;

  Radians := DegToRad(-RotAngle);
  ScaledWidth := W * Scaling.X;
  ScaledHeight := H * Scaling.Y;

  Result := Point(Ceil(Abs(ScaledWidth * Cos(Radians)) + Abs(ScaledHeight * Sin(Radians))),
                  Ceil(Abs(ScaledWidth * Sin(Radians)) + Abs(ScaledHeight * Cos(Radians))));

  Result.X := Result.X - (Result.X mod 2);
  Result.Y := Result.Y - (Result.Y mod 2);
end;

procedure TExtCustomRBLayer.SetFrameStipple(const Value: Array of TColor32);
var
  L: Integer;
begin
  L := High(Value) + 1;
  SetLength(FFrameStipplePattern, L);
  MoveLongword(Value[0], FFrameStipplePattern[0], L);
end;

procedure TExtCustomRBLayer.SetFrameStippleStep(const Value: Single);
begin
  if Value <> FFrameStippleStep then
  begin
    FFrameStippleStep := Value;
    TLayerCollectionCast(LayerCollection).GDIUpdate;
  end;
end;

procedure TExtCustomRBLayer.SetFrameStippleCounter(const Value: Single);
begin
  if Value <> FFrameStippleCounter then
  begin
    FFrameStippleCounter := Value;
    TLayerCollectionCast(LayerCollection).GDIUpdate;
  end;
end;


//====================== TExtRubberBandLayer ===================================================

constructor TExtRubberBandLayer.Create(LayerCollection: TLayerCollection);
begin
  inherited;
  FchildLayer:=nil;
end;


destructor TExtRubberBandLayer.Destroy;
begin
  if Assigned(FChildLayer) then FChildLayer.RemoveNotification(Self);
  inherited;
end;

procedure TExtRubberBandLayer.SetChildLayer(Value: TTransformationLayer);
begin
  if Assigned(FChildLayer) then
   begin
    FChildLayer.RemoveNotification(Self);
   end;
  FChildLayer := Value;
  if Assigned(Value) then
  begin
    FSize := Value.GetNativeSize;
    FRotAngle := Value.RotAngle;
    FPosition := Value.Position;
    FPivotPoint := Value.PivotPoint;
    FScaled := Value.ScaledViewport;
    FScaling := Value.Scaling;
    FSkew := Value.Skew;
    FChildLayer.AddNotification(Self);
  end
  else
  begin
    FSize.cx := 1;
    FSize.cy := 1;
    FRotAngle := 0;
    FPosition := FloatPoint(0, 0);
    FPivotPoint := FloatPoint(0, 0);
    FScaled := False;
    FScaling := FloatPoint(1, 1);
    FSkew := FloatPoint(0, 0);
  end;

  if FChildLayer <> nil then
    LayerOptions := LayerOptions or LOB_NO_UPDATE else
    LayerOptions := LayerOptions and not LOB_NO_UPDATE;
end;

procedure TExtRubberBandLayer.Notification(ALayer: TCustomLayer);
begin
  inherited;
  if ALayer = FChildLayer then  FChildLayer := nil;
end;

procedure TExtRubberBandLayer.UpdateChildLayer;
begin
  if Assigned(FChildLayer) then
  begin
    FChildLayer.Changing;
    FChildLayer.FRotAngle := FRotAngle;
    FChildLayer.FSkew := FSkew;
    FChildLayer.FPosition := FPosition;
    FChildLayer.FScaling := FScaling;
    FChildLayer.FPivotPoint := FPivotPoint;
    FChildLayer.Changed;
  end;
end;

//====================== TExtCropLayer ===================================================

constructor TExtCropLayer.Create(LayerCollection: TLayerCollection);
begin
  inherited;
  FHandleFrame := clBlack32;
  FHandleFill := clWhite32;
  SetFrameStipple([clRed32,clRed32,clRed32,clRed32,clBlack32,clBlack32,clBlack32,clBlack32]);
  FFrameStippleStep := 1;
  FFrameStippleCounter := 0;
  FOptions := [rboAllowCornerResize,rboAllowEdgeResize,rboAllowMove,rboShowFrame,rboShowHandles];
  FHandleSize := 3;
  LayerOptions := LOB_VISIBLE or LOB_MOUSE_EVENTS;
  FThreshold := 8;
  FSize.cx := 1;
  FSize.cy := 1;
  fTimer:=Ttimer.Create(nil);
  fTimer.Enabled:=false;
  fTimer.Interval:=10;
  fTimer.OnTimer:=DoOnTime;
  fActive:=false;
end;


destructor TExtCropLayer.Destroy;
begin
  fTimer.Enabled:=false;
  fTimer.Free;
  inherited;
end;

procedure TExtCropLayer.SetActive(Value: Boolean);
 begin
   fActive:=Value;
   visible:=fActive;
   fTimer.Enabled:=fActive;
   fimage.Invalidate;
 end;

Procedure TExtCropLayer.DoOnTime(Sender: TObject);
begin
  FrameStippleCounter := FFrameStippleCounter-0.3;
  if fimage=nil then exit;
  fimage.Invalidate;
end;

procedure TExtCropLayer.SetImage(Value: TImgView32);
 var Fp:TfloatPoint;
     TP:Tpoint;
begin

  FImage := Value;

  //default values
  FSize.cx := 1;
  FSize.cy := 1;
  FRotAngle := 0;
  FPosition := FloatPoint(0, 0);
  FPivotPoint := FloatPoint(0, 0);
  FScaled :=true;
  FScaling := FloatPoint(1, 1);
  FSkew := FloatPoint(0, 0);
  LayerOptions := LayerOptions and not LOB_NO_UPDATE;

  if Assigned(FImage) then
  begin

    FSize := Value.GetBitmapSize;
    FSize.cx:=Round(FSize.cx/FImage.Scale);
    FSize.cy:=Round(FSize.cy/FImage.Scale);
    LayerOptions := LayerOptions or LOB_NO_UPDATE
  end ;

end;

procedure TExtCropLayer.Execute_Crop;
var tp:Tpoint;
    cc:Tbitmap32;
 begin
  cc:=nil;
   try
    cc:=Tbitmap32.Create;
    cc.Assign(image.Bitmap);
    //..... Set Size ...............
    tp:=GetFinalSize;
    image.Bitmap.SetSize(tp.X,tp.Y);
    image.Bitmap.Clear($00FFFFFF);
    //.... set position ............
    tp:=Point(FPosition);
    cc.DrawMode := dmTransparent;
    cc.CombineMode := cmMerge;
    cc.DrawTo(image.Bitmap,-tp.X,-tp.Y);

    image.Invalidate;
    image:=Fimage;
   finally
    cc.Free;
   end;
 end;


procedure TExtCropLayer.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FIsDragging := FDragState <> rdsNone;
  if FIsDragging then
  begin
    FOldPosition := FPosition;
    FOldScaling := FScaling;
    FOldPivot := FPivotPoint;
    FOldSkew := FSkew;
    FOldAngle := FRotAngle;
    FDragPos := Point(X, Y);
  end;
  inherited;
  if ssDouble in Shift then Execute_Crop;
end;

//================= TExtSelectionRubberBandLayer ======================================================
 constructor TExtSelectionRubberBandLayer.Create(LayerCollection: TLayerCollection);
begin
  inherited;

  SetFrameStipple([clBlack32,clBlack32,clBlack32,clBlack32,clRed32,clRed32,clRed32,clRed32]);
  FFrameStippleStep := 1;
  FFrameStippleCounter := 0;
  FOptions := [rboAllowCornerResize,rboAllowEdgeResize,rboAllowMove,rboAllowSheering,
               rboAllowRotation,rboShowFrame,rboShowHandles,rboAllowPivotMove];

  LayerOptions := LOB_VISIBLE or LOB_MOUSE_EVENTS;
  FThreshold := 8;
  FSize.cx := 1;
  FSize.cy := 1;
  fTimer:=Ttimer.Create(nil);
  fTimer.Enabled:=false;
  fTimer.Interval:=10;
  fTimer.OnTimer:=DoOnTime;
  fActive:=false;

  FHandleSize := 3;
  FHandleFrame := clRed32;//clMedGray;
  FHandleFill := clTrWhite32;

end;


destructor TExtSelectionRubberBandLayer.Destroy;
begin
  fTimer.Enabled:=false;
  fTimer.Free;
  inherited;
end;

procedure TExtSelectionRubberBandLayer.SetActive(Value: Boolean);
 begin
   fActive:=Value;
   visible:=fActive;
   fTimer.Enabled:=fActive;
   self.Changed;
 end;

Procedure TExtSelectionRubberBandLayer.DoOnTime(Sender: TObject);
begin
  FrameStippleCounter := FFrameStippleCounter-0.3;
  if fChildLayer=nil then exit;
  self.Changed;
end;


procedure TExtSelectionRubberBandLayer.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if factive=true then
   begin
    fTimer.Enabled:=false;
    FOptions := [rboAllowCornerResize,rboAllowEdgeResize,rboAllowMove,rboAllowSheering,
                 rboAllowRotation,rboAllowPivotMove];
   end;
  inherited;
end;

procedure TExtSelectionRubberBandLayer.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;

procedure TExtSelectionRubberBandLayer.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if factive=true then
  begin
   FOptions :=[rboAllowCornerResize,rboAllowEdgeResize,rboAllowMove,rboAllowSheering,
               rboAllowRotation,rboShowFrame,rboShowHandles,rboAllowPivotMove];
   fTimer.Enabled:=true;
  end;

  inherited;
end;

//=================== TExtMouseLayer ==============================================

constructor TExtMouseLayer.Create(ALayerCollection: TLayerCollection);
begin
  inherited;
  FDrawCenterDots:=true;
  FRadius:=5;
end;


procedure TExtMouseLayer.SetCenter(const Value: TPoint);
begin
  if (Value.X <> FCenter.X) or (Value.Y <> FCenter.Y) then
  begin

    FCenter := Value;
    // Painting new position
    with FCenter do
      Changed(Rect(X - fRadius, Y - fRadius, X + fRadius + 1, Y + fRadius + 1));
  end;

end;

procedure TExtMouseLayer.SetRadius(const Value: Integer);
begin
  FRadius := Value;
  with FCenter do
    Changed(Rect(X - fRadius, Y - fRadius, X + fRadius + 1, Y + fRadius + 1));
end;

procedure TExtMouseLayer.SetShape(const Value: TExtMouseLayerShape);
begin
  fShape:=Value;
  self.Changed;
end;

procedure TExtMouseLayer.PaintCicle(Buffer: TBitmap32);
var
  I: Integer;
  P: TFixedPoint;
  Steps: Integer;
  xxScale: Single;
  xxRadius: Integer;
  fp:TFloatPoint;
begin
   if FScaled then
    begin
      fp:=GetViewPortScale;
      xxRadius:=round(fRadius*((fp.X+fp.Y )/4));
    end else
    begin
      xxRadius:=round(fRadius/2);
    end;

  Buffer.MoveToF(fCenter.X + xxRadius, fCenter.Y);
  Steps := xxRadius + 4;
  If Odd(Steps) then Inc(Steps);
  xxScale := 2*PI/STEPS;
  for I := 1 to STEPS do
  begin
    Buffer.PenColor := (not Buffer.PenColor) and $00ffffff + $A0000000;
    P.X := Fixed(fCenter.X + Cos(I * xxSCALE) * xxRadius);
    P.Y := Fixed(fCenter.Y + Sin(I * xxSCALE) * xxRadius);
    Buffer.LineToXS(P.X, P.Y);
  end;
end;

procedure TExtMouseLayer.PaintRetracle(Buffer: TBitmap32);
var
  xxCenter: TPoint;
  xxScale: Single;
  xxRadius: Integer;
  fp:TFloatPoint;
begin

  if FScaled then
    begin
      fp:=GetViewPortScale;
      xxRadius:=round(fRadius*((fp.X+fp.Y )/4));
    end else
    begin
      xxRadius:=round(fRadius/2);
    end;


   xxCenter:=fCenter;
   Dec(xxCenter.X);
   Dec(xxCenter.Y);
  //....................................................
  Buffer.FrameRectTSP(xxCenter.X- xxRadius, xxCenter.Y- xxRadius,
                      xxCenter.X+ xxRadius, xxCenter.Y+ xxRadius);


end;

procedure TExtMouseLayer.Paint(Buffer: TBitmap32);
var
  NewStipple: array of TColor32;
begin
 SetLength(NewStipple, 6);
 NewStipple[0] := $A0000000; NewStipple[1] := $A0000000; NewStipple[2] := $A0000000;
 NewStipple[3] := $A0ffffff; NewStipple[4] := $A0ffffff; NewStipple[5] := $A0ffffff;
 Buffer.BeginUpdate;
 Buffer.SetStipple(NewStipple);
 Buffer.StippleCounter := 0;
 Buffer.StippleStep := 1;
 Buffer.StippleCounter := 0;
 Buffer.PenColor := $A0ffffff;

 if fRadius>4 then
 case fshape of
  mlsCircle:PaintCicle(Buffer);
  mlsRetracle:PaintRetracle(Buffer);
 end;

  if FDrawCenterDots=false then exit;

  Buffer.SetPixelTS(Center.X-1,Center.Y-1,$A0000000);
  Buffer.SetPixelTS(Center.X+1,Center.Y+1,$A0000000);
  Buffer.SetPixelTS(Center.X-1,Center.Y+1,$A0000000);
  Buffer.SetPixelTS(Center.X+1,Center.Y-1,$A0000000);
  Buffer.SetPixelTS(Center.X,Center.Y+1,$A0ffffff);
  Buffer.SetPixelTS(Center.X,Center.Y-1,$A0ffffff);
  Buffer.SetPixelTS(Center.X+1,Center.Y,$A0ffffff);
  Buffer.SetPixelTS(Center.X-1,Center.Y,$A0ffffff);

 Buffer.EndUpdate;
end;


//=================== TExtSelectionShapeLayer ==============================================

constructor TExtSelectionShapeLayer.Create(ALayerCollection: TLayerCollection);
begin
  inherited;
  FMarchingAntsLineList:=nil;
  FHandleFrame := clBlack32;
  FHandleFill := clWhite32;
  SetFrameStipple([clBlack32,clBlack32,$00FFFFFF,$00FFFFFF]);
  FFrameStippleStep := 0.7;
  FFrameStippleCounter := 0;
  FOptions := [rboAllowCornerResize,rboAllowEdgeResize,rboAllowMove,rboShowFrame,rboShowHandles];
  FHandleSize := 3;
  LayerOptions := LOB_VISIBLE or LOB_MOUSE_EVENTS;
  FThreshold := 8;
  FSize.cx := 1;
  FSize.cy := 1;
  fTimer:=Ttimer.Create(nil);
  fTimer.Enabled:=false;
  fTimer.Interval:=10;
  fTimer.OnTimer:=DoOnTime;
  fActive:=false;
  FScaled:=true;

end;

destructor TExtSelectionShapeLayer.Destroy;
begin
  fTimer.Enabled:=false;
  fTimer.Free;
  inherited;
end;


procedure TExtSelectionShapeLayer.SetActive(Value: Boolean);
 begin
   fActive:=Value;
   visible:=fActive;
   fTimer.Enabled:=fActive;
   self.Changed;
 end;

Procedure TExtSelectionShapeLayer.DoOnTime(Sender: TObject);
begin
  FrameStippleCounter := FFrameStippleCounter-0.3;
  Changed;
end;

procedure TExtSelectionShapeLayer.Paint(Buffer: TBitmap32);
var
  MarchingAntsLine: TRectRegionNode;
  i               : integer;
  StartX, StartY  : Single;
  EndX, EndY      : Single;
  XOffset, YOffset: Single;
  fp:TFloatPoint;
  xScale:Single;
begin
  if FMarchingAntsLineList=nil then exit;
  MarchingAntsLine    := nil;
  xScale:=1;

 Buffer.BeginUpdate;

  if (FMarchingAntsLineList.Count > 0) then
  begin

     if FScaled then
    begin
      fp:=GetViewPortScale;
      xScale:= (fp.X+fp.Y )/2;
    end;

    Buffer.SetStipple(FFrameStipplePattern);
    Buffer.StippleCounter := 0;
    Buffer.StippleStep := FFrameStippleStep;
    Buffer.StippleCounter := FFrameStippleCounter;

    XOffset := FMaskBorderStart.X * xScale;
    YOffset := FMaskBorderStart.Y * xScale;



    for i := 0 to FMarchingAntsLineList.Count - 1 do
    begin
      // Get the current Marching Ants line.
      MarchingAntsLine := TRectRegionNode(FMarchingAntsLineList.Items[i]);

      StartX := FLayerTopLeft.X + (MarchingAntsLine.StartPoint.X * xScale);
      StartY := FLayerTopLeft.Y + (MarchingAntsLine.StartPoint.Y * xScale);
      EndX   := FLayerTopLeft.X + (MarchingAntsLine.EndPoint.X   * xScale);
      EndY   := FLayerTopLeft.Y + (MarchingAntsLine.EndPoint.Y   * xScale);

      Buffer.MoveToF(StartX + XOffset, StartY + YOffset);
      Buffer.LineToFSP(EndX + XOffset, EndY + YOffset);

      MarchingAntsLine := nil;
    end;
  end;

  Buffer.EndUpdate;
end;


//== TRectRegionNode ===========================================================

constructor TRectRegionNode.Create;
begin
  inherited Create;
  FStartPt := Point(0, 0);
  FEndPt   := Point(0, 0);
end; { Create }

//== TRectRegionNodeList =======================================================

destructor TRectRegionNodeList.Destroy;
begin
  DeleteAll;
  inherited Destroy;
end; { Destroy }

procedure TRectRegionNodeList.DeleteAll;
var
  i   : Integer;
  Node: TRectRegionNode;
begin
  if Self.Count > 0 then
  begin
    for i := Self.Count - 1 downto 0 do
    begin
      Node := TRectRegionNode(Self.Items[i]);
      Self.Delete(i);
      Node.Free;
      Node := nil;
    end;
  end;
end; { DeleteAll }


end.
