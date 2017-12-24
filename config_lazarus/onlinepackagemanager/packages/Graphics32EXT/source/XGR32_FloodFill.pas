{**********************************************************************
 Package pl_Graphics32EXT.pkg
 this unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit XGR32_FloodFill;

interface

uses
  LCLIntf, LCLType,
  Classes, Math, Graphics, GR32, GR32_LowLevel;

type
  TFFCompareFunc = function(Color, RefColor: TColor32; Tolerance: integer): boolean;
  TFFAlpha = (ffaUpdate, ffaPreserve, ffaAlphaOnly);

  //internal use only ...
  TFFDir = (ffdUp, ffdDown, ffdBoth);
  PFFInfo = ^TFFInfo;

  TFFInfo = record
    Next: PFFInfo;
    Prev: PFFInfo;
    Y: integer;
    XL: integer;
    XR: integer;
    Dir: TFFDir;
  end;

  TFloodFill = class
  private
    FBmp32: TBitmap32;
    FMask: TBitmap32;
    FStack: PFFInfo; // Double linked list of TFFInfo records
    FBounds: TRect;
    FUseBounds: boolean;
    FRefColor: TColor32;
    FNewColor: TColor32;
    FNewColorAlpha: TColor32;
    FNewColorNoAlpha: TColor32;
    FTolerance: integer;
    FAlphaAction: TFFAlpha;
    FCompareFunc: TFFCompareFunc;
    procedure Push(Y, XL, XR: integer; Dir: TFFDir);
    function Pop(out Y, XL, XR: integer; out Dir: TFFDir): boolean;
    procedure ProcessLine;
    procedure SetBounds(const Bounds: TRect);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute(Bmp32: TBitmap32; const Pt: TPoint; NewColor: TColor32);
    property Bounds: TRect read FBounds write SetBounds;
    property UseBounds: boolean read FUseBounds write FUseBounds;
    property Tolerance: integer read FTolerance write FTolerance;
    property Alpha: TFFAlpha read FAlphaAction write FAlphaAction;
    property CompareFunc: TFFCompareFunc read FCompareFunc write FCompareFunc;
  end;

function IsSimilarColorRGB(Color1, Color2: TColor32; Tolerance: integer): boolean; {$IFDEF USEINLINING} inline; {$ENDIF}
function IsSimilarColorAlpha(Color1, Color2: TColor32; Tolerance: integer): boolean; {$IFDEF USEINLINING} inline; {$ENDIF}

implementation

function IsSimilarColorRGB(Color1, Color2: TColor32; Tolerance: integer): boolean;
var
  C1: TColor32Entry ABSOLUTE Color1;
  C2: TColor32Entry ABSOLUTE Color2;
begin
  Result := (Abs(C1.B - C2.B) < Tolerance) and (Abs(C1.G - C2.G) < Tolerance) and (Abs(C1.R - C2.R) < Tolerance);
end;

function IsSimilarColorAlpha(Color1, Color2: TColor32; Tolerance: integer): boolean;
var
  C1: TColor32Entry ABSOLUTE Color1;
  C2: TColor32Entry ABSOLUTE Color2;
begin
  Result := (Abs(C1.A - C2.A) < Tolerance);
end;

//============== TFloodFill ============================================

constructor TFloodFill.Create;
begin
  inherited Create;
  FMask := TBitmap32.Create;
  FCompareFunc := @IsSimilarColorRGB;
end;

destructor TFloodFill.Destroy;
begin
  FMask.Free;
  inherited;
end;

procedure TFloodFill.Push(Y, XL, XR: integer; Dir: TFFDir);
var
  FFInfo: PFFInfo;
begin
  New(FFInfo);
  FFInfo^.Prev := FStack;
  if assigned(FStack) then
  begin
    FFInfo^.Prev := FStack^.Prev;
    FFInfo^.Prev^.Next := FFInfo;
    FFInfo^.Next := FStack;
    FStack^.Prev := FFInfo;
  end
  else
  begin
    FFInfo^.Next := FFInfo;
    FFInfo^.Prev := FFInfo;
    FStack := FFInfo;
  end;
  FFInfo^.Y := Y;
  FFInfo^.XL := XL;
  FFInfo^.XR := XR;
  FFInfo^.Dir := Dir;
end;

function TFloodFill.Pop(out Y, XL, XR: integer; out Dir: TFFDir): boolean;
var
  SP: PFFInfo;
begin
  Result := assigned(FStack);
  if not Result then
    Exit;
  SP := FStack^.Prev;
  Y := SP^.Y;
  XL := SP^.XL;
  XR := SP^.XR;
  Dir := SP^.Dir;
  if SP <> FStack then
  begin
    SP^.Prev^.Next := FStack;
    FStack^.Prev := SP^.Prev;
  end
  else
    FStack := nil;
  Dispose(SP);
end;

procedure TFloodFill.SetBounds(const Bounds: TRect);
begin
  FBounds.Left := Max(Bounds.Left, 0);
  FBounds.Top := Max(Bounds.Top, 0);
  FBounds.Right := Max(Bounds.Right, FBounds.Left);
  FBounds.Bottom := Max(Bounds.Bottom, FBounds.Top);
  FUseBounds :=
    (FBounds.Right > FBounds.Left) and (FBounds.Bottom > FBounds.Top);
end;

{$R-}
procedure TFloodFill.ProcessLine;
var
  Y, XL, XR, XL2, XR2: integer;
  Dir: TFFDir;
  Pixels, MaskPxls: PColor32Array;
begin
  if not Pop(Y, XL, XR, Dir) or (Y < FBounds.Top) or (Y > FBounds.Bottom) then
    Exit;
  Pixels := FBmp32.ScanLine[Y];
  MaskPxls := FMask.ScanLine[Y];
  XL2 := XL;
  if (MaskPxls^[XL2] = 0) and FCompareFunc(Pixels^[XL2], FRefColor, FTolerance) then
  begin
    while (XL2 > FBounds.Left) and FCompareFunc(Pixels^[XL2 - 1], FRefColor, FTolerance) do
      Dec(XL2);
    if XL2 < XL - 1 then //flood-fill in the reverse direction ...
      case Dir of
        ffdUp: Push(Y + 1, XL2, XL - 2, ffdDown);
        ffdDown: Push(Y - 1, XL2, XL - 2, ffdUp);
        else
        begin
          Push(Y - 1, XL2, XL - 2, ffdUp);
          Push(Y + 1, XL2, XL - 2, ffdDown);
        end;
      end;
  end
  else
  begin
    Inc(XL2);
    while (XL2 <= XR) and ((MaskPxls^[XL2] <> 0) or not FCompareFunc(Pixels^[XL2], FRefColor, FTolerance)) do
      Inc(XL2);
    if (XL2 > XR) then
      Exit;
  end;
  XR2 := XL2;
  while True do
  begin
    while (XR2 <= FBounds.Right) and (MaskPxls^[XR2] = 0) and FCompareFunc(Pixels^[XR2], FRefColor, FTolerance) do
    begin
      case FAlphaAction of
        ffaUpdate: Pixels^[XR2] := FNewColor;
        ffaPreserve: Pixels^[XR2] := (Pixels^[XR2] and $FF000000) or FNewColorNoAlpha;
        else
          {ffaAlphaOnly} Pixels^[XR2] := (Pixels^[XR2] and $FFFFFF) or FNewColorAlpha;
      end;
      MaskPxls^[XR2] := 1;
      Inc(XR2);
    end;

    case Dir of
      ffdUp: Push(Y - 1, XL2, XR2, ffdUp);
      ffdDown: Push(Y + 1, XL2, XR2, ffdDown);
      else
      begin
        Push(Y - 1, XL2, XR2, ffdUp);
        Push(Y + 1, XL2, XR2, ffdDown);
      end;
    end;
    if (XR2 > XR + 2) then
      case Dir of //flood-fill in the reverse direction ...
        ffdUp: Push(Y + 1, XR + 2, XR2 - 1, ffdDown);
        ffdDown: Push(Y - 1, XR + 2, XR2 - 1, ffdUp);
        else
        begin
          Push(Y - 1, XR + 2, XR2 - 1, ffdUp);
          Push(Y + 1, XR + 2, XR2 - 1, ffdDown);
        end;
      end
    else if (XR2 >= XR) then
      Exit;
    while (XR2 <= XR) and ((MaskPxls^[XR2] <> 0) or not FCompareFunc(Pixels^[XR2], FRefColor, FTolerance)) do
      Inc(XR2);
    if (XR2 > XR) then
      Exit;
    XL2 := XR2;
  end;
end;

procedure TFloodFill.Execute(Bmp32: TBitmap32; const Pt: TPoint; NewColor: TColor32);
var
  XL, XR: integer;
  Pixels: PColor32Array;
begin
  if FUseBounds and ((FBounds.Right = FBounds.Left) or (FBounds.Bottom = FBounds.Top)) then
    FUseBounds := False;
  if not FUseBounds then
    FBounds := Bmp32.BoundsRect;
  FBounds.Right := Min(Bmp32.Width - 1, FBounds.Right);
  FBounds.Bottom := Min(Bmp32.Height - 1, FBounds.Bottom);
  if (Pt.X < FBounds.Left) or (Pt.X > FBounds.Right) or (Pt.Y < FBounds.Top) or (Pt.Y > FBounds.Bottom) then
    Exit;
  FRefColor := Bmp32.Pixel[Pt.X, Pt.Y];
  FNewColor := NewColor;
  FNewColorAlpha := FNewColor and $FF000000;
  FNewColorNoAlpha := FNewColor and $FFFFFF;
  FBmp32 := Bmp32;
  FMask.SetSize(FBmp32.Width, FBmp32.Height);
  with FMask do
    FillLongword(Bits[0], Width * Height, 0);
  Pixels := FBmp32.ScanLine[Pt.Y];
  XL := Pt.X;
  while (XL > FBounds.Left) and FCompareFunc(Pixels^[XL - 1], FRefColor, FTolerance) do
    Dec(XL);
  XR := Pt.X;
  while (XR < FBounds.Right) and FCompareFunc(Pixels^[XR + 1], FRefColor, FTolerance) do
    Inc(XR);
  Push(Pt.Y, XL, XR, ffdBoth);
  while assigned(FStack) do
    ProcessLine;
  Bmp32.Changed;
end;

{$R+}


end.
