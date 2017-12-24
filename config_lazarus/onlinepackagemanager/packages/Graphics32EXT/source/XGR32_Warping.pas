
{**********************************************************************
 Package pl_Graphics32EXT.pkg
 this unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit XGR32_Warping;

interface
 uses
  SysUtils, Classes, Graphics,Forms, Controls,ExtCtrls,Math,
  GR32, GR32_Blend,
  GR32_Filters, GR32_Image,
  GR32_LowLevel, GR32_Math, GR32_MicroTiles,
  GR32_Rasterizers,
  GR32_Resamplers, GR32_System, GR32_Transforms, GR32_VectorMaps;


const
  // Provide two attractive fast resampling modes for the realtime brush warping
  WARPRESAMPLERS: array [Boolean] of TCustomResamplerClass =(TNearestResampler, TLinearResampler);


  // Pick some attractive kernels for the antialiasing methods
  WARPKERNELS: array [0..6] of TCustomKernelClass = (TBoxKernel, TLinearKernel, TSplineKernel,
                                                 TMitchellKernel, TSinshKernel, TGaussianKernel,
                                                 TCubicKernel);
var
  WARPKernelIndex : 0..6 = 6; //TCubicKernel

type

  TWarpTool = (btWarp, btZoom, btTwirl, btFlower);
  TWarpToolMode = (btmLeft,btmRight);
  TWarpToolProc = procedure(var D, R:Single; Param: Single);

TWarpBrush = class
  private
    FPressure: Single;
    FPinch: Single;
    FFeather: Single;
    procedure SetPinch(const Value: Single);
    procedure SetPressure(const Value: Single);
    procedure SetFeather(const Value: Single);
  public
    constructor Create;
    property Pressure : Single read FPressure write SetPressure;
    property Pinch: Single read FPinch write SetPinch;
    property Feather: Single read FFeather write SetFeather;
    function Weight(X, Y: Single): Single;
  end;

TWarpControler = class(TPersistent)
  private
   fTempBMP: TBitmap32;
   fWarpTool:TWarpTool;
   fTimer:TTimer;
   FIsActive:Boolean;

   FScale:integer;
   FSize:integer;
   FParam:integer;
   fPressure:integer;
   fPinch:integer;
   fFeather:integer;
   fRate:integer;
   //..........................
   fMouseDown: Boolean;
   fRemapper: TRemapTransformation;
   fGenericBrush: TWarpBrush;
   fCurrentBrush: array [TWarpToolMode] of TVectorMap;
   fWarpToolMode: TWarpToolMode;
   fTempMap: TVectorMap;
   fLastPos: TPoint;
   fLastDelta: TFixedPoint;
   fSampleClipRect: TRect;
   fSamplingGridSize: Byte;
   fKernelMode: TKernelMode;
   fBilinearWarp:boolean;
   //.....................
   FFirstBmp:TBitmap32;
   fSrcBmp:TBitmap32;
   fBrushMeshPreview:TPaintBox32;
  protected
    procedure SetSrcBmp(val:TBitmap32);
    procedure SetBrushMeshPreview(val:TPaintBox32);
    procedure SetBilinearWarp(const val:boolean);
    procedure SetKernelMode(const val:TKernelMode);
    procedure SetSamplingGridSize(const val:Byte);

    procedure SetScale(const val:integer);
    procedure SetSize(const val:integer);
    procedure SetParam(const val:integer);
    procedure SetPressure(const val:integer);
    procedure SetPinch(const val:integer);
    procedure SetFeather(const val:integer);
    procedure SetRate(const val:integer);
    procedure SetWarpTool(const val:TWarpTool);
    Procedure SetActive(const val:boolean);
    procedure DoOnTimer(Sender: TObject);
  public

    procedure DrawMappedBrush(Pos: TPoint);
    procedure PrecalcfCurrentBrush;
    procedure UpdateBrush;
    procedure DrawBrushMeshPreview;
    function  GetParam: Single;
    function  SetWarpToolMode(Shift: TShiftState): Boolean;
    procedure ResetMesh;
    procedure IntScrBmp;
    procedure InitfTempBMP;

    //..........................................
    constructor Create; Virtual;
    destructor Destroy; override;
    Procedure Clear;
    Procedure SaveMeshTofile(const Afile:string);
    Procedure LoadMeshFromfile(const Afile:string);

    procedure DoSuperSample;
    procedure DoOnMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoOnMouseMove(Shift: TShiftState; X, Y: Integer);
    procedure DoOnMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    Property  SrcBmp:TBitmap32 read fSrcBmp write SetSrcBmp;
    Property  BrushMeshPreview:TPaintBox32 read fBrushMeshPreview write SetBrushMeshPreview;
    Property  WarpTool:TWarpTool read fWarpTool write SetWarpTool;
    Property  IsActive:Boolean read FIsActive write SetActive;

    Property  BilinearWarp:Boolean read FBilinearWarp write SetBilinearWarp;
    Property  KernelMode:TKernelMode read FKernelMode write SetKernelMode;
    Property  SamplingGridSize: Byte read FSamplingGridSize write SetSamplingGridSize;

    Property  Size:integer read fSize write SetSize;
    Property  Param:integer read fParam write SetParam;
    Property  Scale:integer read fScale write SetScale;
    Property  Pressure:integer read fPressure write SetPressure;
    Property  Pinch:integer read fPinch write SetPinch;
    Property  Feather:integer read fFeather write SetFeather;
    Property  Rate:integer read fRate write SetRate;
  end;   

function PinchPop(X, Pinch: Single): Single;
function FeatherFunc(R, Feather: Single): Single;

implementation

//===================================================================
procedure WarpDummy(var D, R: Single; Param: Single);
begin
  // do nothing
end;

procedure ZoomIn(var D, R: Single; Param: Single);
begin
  D := D - 0.1 * (1 - (1 - D) * Param);
end;

procedure ZoomOut(var D, R: Single; Param: Single);
begin
  D := D + 0.1 * (1 - (1 - D) * Param);
end;

procedure TwirlCW(var D, R: Single; Param: Single);
begin
  R := R + Param;
end;

procedure TwirlCCW(var D, R: Single; Param: Single);
begin
  R := R - Param;
end;

procedure FlowerOut(var D, R: Single; Param: Single);
begin
  D := D * Sin(R * Param);
end;

procedure FlowerIn(var D, R: Single; Param: Single);
begin
  D := D * Max(Sin(R * Param), 0);
end;


//===================================================================
constructor TWarpControler.Create;
 begin
   inherited;
   fSrcBmp:=nil;
   fBrushMeshPreview:=nil;
   fTimer:=TTimer.Create(nil);
   fTimer.Enabled:=false;
   fMouseDown:=false;
   fTempBMP := TBitmap32.Create;
   FFirstBmp:= TBitmap32.Create;
   Clear;
   fTimer.OnTimer:=@DoOnTimer;
end;

destructor TWarpControler.Destroy;
var I : TWarpToolMode;
 begin
 // fSrcBmp:=nil;
  fTimer.Enabled:=false;
  fTimer.OnTimer:=nil;
  fTimer.Free;
  fGenericBrush.Free;
  fRemapper.Free;
  for I := btmLeft to btmRight do fCurrentBrush[I].Free;
  fTempMap.Free;
  FFirstBmp.Free;
  inherited;
 end;

Procedure TWarpControler.Clear;
 begin
   fKernelMode := kmTableLinear;
   fSamplingGridSize := 3;
   fTimer.Interval:=50;
   FScale:=100;
   FSize:=30;
   FParam:=20;
   fPressure:=50;
   fPinch:=0;
   fFeather:=12;
   fRate:=350;
   fBilinearWarp:=true;
   fWarpTool:=btWarp;
 end;

Procedure TWarpControler.SetActive(const val:boolean);
 begin
   FIsActive:=Val; 
   UpdateBrush;
   fTimer.Enabled:=FIsActive;
 end;

procedure TWarpControler.SetParam(const val:integer);
 begin
  fParam:=val;
  DrawBrushMeshPreview;
 end;

procedure TWarpControler.SetBrushMeshPreview(val:TPaintBox32);
 begin
   fBrushMeshPreview:=val;
   fBrushMeshPreview.Buffer.SetSize(fBrushMeshPreview.Width,fBrushMeshPreview.Height);
   DrawBrushMeshPreview;
 end;

procedure TWarpControler.SetBilinearWarp(const val:boolean);
 begin
  fBilinearWarp:=val;
  WARPRESAMPLERS[fBilinearWarp].Create(fTempBMP);
  Transform(fSrcBmp, fTempBMP, fRemapper);
 end;

procedure TWarpControler.SetKernelMode(const val:TKernelMode);
 begin
  fKernelMode:=val;
  fSampleClipRect := fRemapper.VectorMap.GetTrimmedBounds;
  DoSuperSample;
 end;

procedure TWarpControler.SetSamplingGridSize(const val:Byte);
 begin
  fSamplingGridSize:=val;
 end;

procedure TWarpControler.InitfTempBMP;
var
  I : TWarpToolMode;
  J: Integer;
begin
   fTempBMP.OuterColor := 0;
   fTempBMP.DrawMode := dmTransparent;//dmBlend;
   fTempBMP.CombineMode := cmMerge;
   SetBorderTransparent(fTempBMP, fTempBMP.BoundsRect);
   
  fRemapper := TRemapTransformation.Create;
  fRemapper.VectorMap.SetSizeFrom(fTempBMP);
  fRemapper.SrcRect := FloatRect(fTempBMP.BoundsRect);
  fRemapper.MappingRect := FloatRect(fSrcBmp.BoundsRect);

  for I := btmLeft to btmRight do fCurrentBrush[I] := TVectorMap.Create;

  fTempMap := TVectorMap.Create;

  fGenericBrush := TWarpBrush.Create;
  WARPRESAMPLERS[fBilinearWarp].Create(fTempBMP);

  fSampleClipRect := Rect(MaxInt, MaxInt, -MaxInt, -MaxInt);
 end;


procedure TWarpControler.IntScrBmp;
 begin
   SetBorderTransparent(fTempBMP, fTempBMP.BoundsRect); //Fix against border issues
   fRemapper.VectorMap.SetSizeFrom(fTempBMP);
   fRemapper.SrcRect := FloatRect(fTempBMP.BoundsRect);
   fRemapper.MappingRect := FloatRect(fSrcBmp.BoundsRect);
 end;


procedure TWarpControler.DrawMappedBrush(Pos: TPoint);
const
  PI2 = PI * 2;
var
  fTempBMPR, fTempBMPB, X, Y, I, J, ClipLeft, ClipTop, ClipRight, ClipBottom: Integer;
  Vertex: TFixedPoint;
  Dst: TVectorMap;
  DeltaX, DeltaY, Zx, Zy: TFixed;
  P: TFixedPoint;
  DstClip: TRect;
begin
  X := Pos.X;
  Y := Pos.Y;
  fTempBMPR := fCurrentBrush[fWarpToolMode].Width - 1;
  fTempBMPB := fCurrentBrush[fWarpToolMode].Height - 1;
  Dst := fRemapper.VectorMap;
  ClipLeft := Abs(Min(0, X));
  ClipRight := Min(fTempBMPR + X, Dst.Width - 1) - X;
  ClipTop := Abs(Min(0, Y));
  ClipBottom := Min(fTempBMPB + Y, Dst.Height - 1) - Y;

  if fWarpTool=btWarp then
  begin
    DeltaX := Round((Fixed(Integer(fLastPos.X - X)) + fLastDelta.X) / 2);
    DeltaY := Round((Fixed(Integer(fLastPos.Y - Y)) + fLastDelta.Y) / 2);
    fLastDelta.X := DeltaX;
    fLastDelta.Y := DeltaY;
  end
  else
  begin
    DeltaX := FIXEDONE;
    DeltaY := FIXEDONE;
  end;

  for J := ClipTop to ClipBottom do
    for I := ClipLeft to ClipRight do
    begin
      Zx := Fixed(Integer(X + I));
      Zy := Fixed(Integer(Y + J));

      P := fCurrentBrush[fWarpToolMode].FixedVector[I, J];
      P.X := FixedMul(DeltaX, P.X);
      P.Y := FixedMul(DeltaY, P.Y);

      Vertex := Dst.FixedVectorXS[Zx + P.X, Zy + P.Y];
      Inc(Vertex.X, P.X);
      Inc(Vertex.Y, P.Y);
      fTempMap.FixedVector[I, J] := Vertex;
    end;

  for J := ClipTop to ClipBottom do
    for I := ClipLeft to ClipRight do
      Dst.FixedVector[X + I, Y + J] := fTempMap.FixedVector[I, J];

  DstClip := Rect(X, Y, X + fCurrentBrush[fWarpToolMode].Width, Y + fCurrentBrush[fWarpToolMode].Height);
  fSrcBmp.FillRectS(DstClip, 0);
  Transform(fSrcBmp, fTempBMP, fRemapper, DstClip);
  fSrcBmp.Changed;
  UnionRect(fSampleClipRect, fSampleClipRect, DstClip);
  fLastPos := Point(X, Y);
end;


procedure TWarpControler.PrecalcfCurrentBrush;
const
  TOOLPROCS: array [TWarpToolMode, TWarpTool] of TWarpToolProc =
    ((nil, @ZoomIn,@TwirlCW, @FlowerOut),(nil, @ZoomOut, @TwirlCCW, @FlowerIn));
var
  I,J: Integer;
  w, rx, ry, nrx, nry, x, y: double;
  d, r, xParam,xs,ys: single;
  Tool: TWarpTool;
  ToolMode: TWarpToolMode;
  Proc: TWarpToolProc;
begin
  Tool := fWarpTool;

  for ToolMode := btmLeft to btmRight do with fCurrentBrush[ToolMode] do
  begin
    rx := Width - 1;
    ry := Height - 1;
    nrx := 2 / rx;
    nry := 2 / ry;

    Proc := TOOLPROCS[ToolMode, Tool];
    case Tool of
      btWarp:
        begin
          for J := 0 to Height - 1 do
            for I := 0 to Width - 1 do
            begin
              x := I * nrx - 1;
              y := J * nry - 1;
              w := fGenericBrush.Weight(x, y);
              FixedVector[I, J] := FixedPoint(w, w);
            end;
          Exit;
        end;
    end;
    xParam := GetParam;

    for J := 0 to Height - 1 do
      for I := 0 to Width - 1 do
      begin
        x := I * nrx - 1;
        y := J * nry - 1;
        w := fGenericBrush.Weight(x, y);

        d := Hypot(x, y);
        r := ArcTan2(y, x);
        Proc(d,r, xParam);

        xs:=x;
        ys:=y;
        SinCos(r, d, ys, xs);
        x := (x + 1) * rx * 0.5 - I;
        y := (y + 1) * ry * 0.5 - J;

        FixedVector[I, J] := FixedPoint(x * w, y * w);
      end;
  end;
end;

procedure TWarpControler.UpdateBrush;
var
  I: TWarpToolMode;
begin
  for I := btmLeft to btmRight do
    fCurrentBrush[I].SetSize(fSize,fSize);

  fTempMap.SetSizeFrom(fCurrentBrush[btmLeft]);
  PrecalcfCurrentBrush;
  DrawBrushMeshPreview;
end;

procedure TWarpControler.DrawBrushMeshPreview;
// Render some sort of preview of the brush mesh...
var
  I, J, rx, ry: Integer;
  Proc: TWarpToolProc;
  Tool: TWarpTool;
  D, R, x, y, xParam,Sx, Sy, w: Single;
  xe,ye:double;
const
  Colors: array [Boolean] of TColor32 = ($FFE0E0E0, $FF000000);
  TOOLPROCS: array [TWarpToolMode, TWarpTool] of TWarpToolProc =
    ((@WarpDummy, @ZoomIn, @TwirlCW, @FlowerOut),
     (@WarpDummy, @ZoomOut, @TwirlCCW, @FlowerIn));

  GridSize = 8;

begin
  if fBrushMeshPreview=nil then Exit;
  if fBrushMeshPreview.Buffer.Empty then Exit;

  Tool := fWarpTool;
  Proc := TOOLPROCS[fWarpToolMode, Tool];

  with fBrushMeshPreview do
  begin
    xParam := GetParam;
    Buffer.Clear($FF000000);

    rx := Width - 1;
    ry := (Height - 1) div GridSize;
    Sx := 2/rx;
    Sy := 2/((Height - 1) / GridSize);
    for J := 0 to ry do
    begin
      Buffer.MoveToF(0, J * GridSize);
      for I := 0 to rx do
      begin
        x := I * Sx - 1;
        y := J * Sy - 1;
        xe:=x;
        ye:=y;
        d := Hypot(xe, ye);
        r := ArcTan2(y, x);
        Proc(d, r, xParam);
        w := fGenericBrush.Weight(x,y);
        SinCos(r, d, y, x);
        x := I - ((x + 1) * rx * 0.5 - I) * w;
        y := J - ((y + 1) * ry * 0.5 - J) * w;
        y := y * GridSize;
        Buffer.PenColor := Gray32(15 + Round(240 * (1 - Sqr(1 - w))));
        Buffer.LineToFS(x,y);
      end;
    end;

    rx := (Width - 1) div GridSize;
    ry := Height - 1;
    Sx := 2/((Width - 1) / GridSize);
    Sy := 2/ry;
    for I := 0 to rx do
    begin
      Buffer.MoveToF(I * GridSize, 0);
      for J := 0 to ry do
      begin
        x := I * Sx - 1;
        y := J * Sy - 1;
        xe:=x;
        ye:=y;
        d := Hypot(xe, ye);
        r := ArcTan2(y, x);
        Proc(d, r, xParam);
        w := fGenericBrush.Weight(x,y);
        SinCos(r, d, y, x);
        x := I - ((x + 1) * rx * 0.5 - I) * w;
        x := x * GridSize;
        y := J - ((y + 1) * ry * 0.5 - J) * w;
        Buffer.PenColor := Gray32(15 + Round(240 * (1 - Sqr(1 - w))) );
        Buffer.LineToFS(x,y);
      end;
    end;
    Buffer.FrameRectS(GetViewPortRect , $FFFFFFFF); //Draw Frame
    Repaint;
  end;
end;

function TWarpControler.GetParam: Single;
begin
  case fWarpTool of
    btFlower: Result := Round(fParam / 100 * 16 +1);
    btZoom: Result := 1 - fParam / 100;
  else Result := fParam / 100
  end
end;

function TWarpControler.SetWarpToolMode(Shift: TShiftState): Boolean;
begin
  Result := False;
  if [ssRight, ssLeft] * Shift <> [] then
  begin
    Result := True;
    if ssLeft in Shift then
      fWarpToolMode := btmLeft  //Higher priority
    else
      fWarpToolMode := btmRight;
  end;
end;

procedure TWarpControler.DoOnMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  P, Q : Integer;
begin
  fMouseDown := True;

  if SetWarpToolMode(Shift) then
    begin
      P := fCurrentBrush[fWarpToolMode].Width div 2;
      Q := fCurrentBrush[fWarpToolMode].Height div 2;
      fLastPos := Point(X - P, Y - Q);
      fLastDelta := FixedPoint(0,0);
      with fLastPos do
        UnionRect(fSampleClipRect, fSampleClipRect, Rect(X, Y, X + P, Y + Q));
    end;

end;

procedure TWarpControler.DoOnMouseMove(Shift: TShiftState; X, Y: Integer);
  function Color32ToStr(C: TColor32): string;
  begin
    Result := 'A: ' + IntToStr(C shr 24);
    Result := Result + ' R: ' + IntToStr(C shr 16 and $FF);
    Result := Result + ' G: ' + IntToStr(C shr 8 and $FF);
    Result := Result + ' B: ' + IntToStr(C and $FF);
  end;

begin

  if SetWarpToolMode(Shift) then
      DrawMappedBrush(Point(X - fCurrentBrush[fWarpToolMode].Width div 2,
        Y - fCurrentBrush[fWarpToolMode].Height div 2));
end;

procedure TWarpControler.DoOnMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  fMouseDown := False;
end;

procedure TWarpControler.SetScale(const val:integer);
 var   S: Single;
begin
  S := val / 100;
  fRemapper.Scale(S, S);
  fSampleClipRect := fRemapper.VectorMap.GetTrimmedBounds;
  Transform(fSrcBmp, fTempBMP, fRemapper);
end;

procedure TWarpControler.SetPressure(const val:integer);
 begin
  fPressure:=val;
  fGenericBrush.Pressure := fPressure / 100;
  UpdateBrush;
 end;

procedure TWarpControler.SetSize(const val:integer);
 begin
   fSize:=val;
   UpdateBrush;
 end;

procedure TWarpControler.SetPinch(const val:integer);
 begin
  fPinch:=val;
  fGenericBrush.Pinch := fPinch / 100;
  UpdateBrush;
 end;

procedure TWarpControler.SetFeather(const val:integer);
 begin
  fFeather:=val;
  fGenericBrush.Feather := fFeather / 100;
  UpdateBrush;
 end;

procedure TWarpControler.SetRate(const val:integer);
  begin
  fRate:=val;
  if fRate = 0 then
   begin
    fTimer.Enabled := False;
  end else
  begin
    fTimer.Enabled := True;
    fTimer.Interval := Round(400 - fRate);
  end;
 end;

procedure TWarpControler.SetWarpTool(const val:TWarpTool);
 begin
  fWarpTool:=val;

  case fWarpTool of
    btWarp:
      begin
        fTimer.Enabled := False;
      end;
    btZoom, btTwirl, btFlower:
      begin
        fTimer.Enabled := True;
      end;
  end;

  UpdateBrush;
 end;

 procedure TWarpControler.SetSrcBmp(val:TBitmap32);
  begin
    fSrcBmp:=val;
    fTempBMP.Assign(fSrcBmp);
    FFirstBmp.Assign(fSrcBmp);
    InitfTempBMP;
    UpdateBrush;
  end;

procedure TWarpControler.ResetMesh;
begin
  fRemapper.VectorMap.Clear;
  fScale := 100;
  fRemapper.Scale(1,1);
  WarpTool:=btWarp;
end;

Procedure TWarpControler.SaveMeshTofile(const Afile:string);
 var s:string;
 begin
   s:=Afile;
   if Lowercase(ExtractFileExt(Afile)) <> '.msh' then s:= Afile + '.msh';
   fRemapper.VectorMap.SaveToFile(s);
end;

Procedure TWarpControler.LoadMeshFromfile(const Afile:string);
 begin
   fRemapper.VectorMap.LoadFromFile(Afile);
   Transform(fSrcBmp, fTempBMP, fRemapper);
   fSampleClipRect := fRemapper.VectorMap.GetTrimmedBounds;
   fSrcBmp.Changed;
end;

procedure TWarpControler.DoSuperSample;
var
  Rasterizer: TRasterizer;
  Transformer: TTransformer;
  SuperSampler: TSuperSampler;
  KernelResampler : TKernelResampler;
begin
  Screen.Cursor := crHourGlass;
  KernelResampler := TKernelResampler.Create(fTempBMP);
  KernelResampler.KernelMode := fKernelMode;

  // Normally this should be set higher.
  // It is set low here to display perceptual consequences
  KernelResampler.TableSize := 4;

  KernelResampler.Kernel := WARPKERNELS[WARPKernelIndex].Create;

  Transformer := TTransformer.Create(fTempBMP.Resampler, fRemapper);
  SuperSampler := TSuperSampler.Create(Transformer);
  Rasterizer := TRegularRasterizer.Create;
  try
    Rasterizer.Sampler := SuperSampler;
    SuperSampler.SamplingX := fSamplingGridSize;
    SuperSampler.SamplingY := fSamplingGridSize;
    fSrcBmp.FillRectS(fSampleClipRect, 0);
    Rasterizer.Rasterize(fSrcBmp, fSampleClipRect, fTempBMP);
    fSampleClipRect := Rect(MaxInt, MaxInt, -MaxInt, -MaxInt);
  finally
    Rasterizer.Free;
    SuperSampler.Free;
    Transformer.Free;
    WARPRESAMPLERS[fBilinearWarp].Create(fTempBMP);
    Screen.Cursor := crDefault;
    fSrcBmp.Changed;
  end;
end;

procedure TWarpControler.DoOnTimer(Sender: TObject);
 begin
  if fMouseDown then DrawMappedBrush(fLastPos);
end;


//===================================================================
 
function PinchPop(X, Pinch: Single): Single;
begin
  if (X <= -1) or (X >= 1) then
  begin
    Result := 0;
    Exit;
  end
  else if Pinch = 1 then
  begin
    Result := 1;
  end
  else if Pinch = -1 then
  begin
    if Fixed(X) = 0 then
      Result := 1
    else
      Result := 0;
  end
  else
  begin
    if Pinch > 0 then
      Pinch := 1 / (1 - Pinch)
    else
      Pinch := Pinch + 1;
    Result := Power(Abs(1 - Abs(Power(X, Pinch))), 1 / Pinch);
  end;
end;

function FeatherFunc(R, Feather: Single): Single;
begin
  if Feather <= 0 then
    Result := 1
  else
  begin
    Feather := 1 / Feather;
    Result := (1 - R) * Feather;
    Result := Constrain(Result, 0, 1);
  end;
end;

//========================== TWarpBrush =====================================

function TWarpBrush.Weight(X, Y: Single): Single;
var
  R: Single;
  xe,ye:double;
begin
  xe:=x;
  ye:=y;
  R := Hypot(Xe, Ye);
  Result := PinchPop(R, Pinch);
  Result := Result * FeatherFunc(R, Feather);
  Result := Constrain(Result * Pressure, 0, 1);
end;

constructor TWarpBrush.Create;
begin
  inherited;
  FPinch := 0;
  FPressure := 1.0;
  FFeather := 0;
end;

procedure TWarpBrush.SetPinch(const Value: Single);
begin
  FPinch := EnsureRange(Value, -1, 1);
end;

procedure TWarpBrush.SetPressure(const Value: Single);
begin
  FPressure := EnsureRange(Value, 0, 1);
end;

procedure TWarpBrush.SetFeather(const Value: Single);
begin
  FFeather := EnsureRange(Value, 0, 1);
end;

end.
