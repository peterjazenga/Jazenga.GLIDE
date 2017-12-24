
{**********************************************************************
 Package pl_Graphics32EXT.pkg
 this unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit XGR32_ExtLayers_transform;

  {$MODE Delphi}

interface

uses

  SysUtils, Classes,
  GR32 , GR32_Blend,
  GR32_Rasterizers, GR32_Transforms,GR32_LowLevel;

type

 TsStretchFilter = (sfNearest, sfDraft, sfLinear, sfCosine, sfSpline, sfLanczos, sfMitchell);

  TXTransformation = class(TObject)
  private
    FSrcRect: TFloatRect;
    procedure SetSrcRect(const Value: TFloatRect);
    procedure Transform(DstX, DstY: Integer; out SrcX, SrcY: Integer); virtual; abstract;
    procedure Transform256(DstX, DstY: Integer; out SrcX256, SrcY256: Integer); virtual; abstract;
  protected
    TransformValid: Boolean;
    procedure PrepareTransform; virtual; abstract;
  public
    function  GetTransformedBounds: TRect; virtual; abstract;
    property SrcRect: TFloatRect read FSrcRect write SetSrcRect;
  end;

  TXAffineTransformation = class(TXTransformation)
  protected
    A, B, C: Integer;
    D, E, F: Integer;
    procedure PrepareTransform; override;
    procedure Transform(DstX, DstY: Integer; out SrcX, SrcY: Integer); override;
    procedure Transform256(DstX, DstY: Integer; out SrcX256, SrcY256: Integer); override;
  public
    Matrix: TFloatMatrix;
    constructor Create; virtual;
    function  GetTransformedBounds: TRect; override;
    procedure Clear;
    procedure Rotate(Cx, Cy, Alpha: Single); // In Degrees
    procedure Skew(Fx, Fy: Single);
    procedure Scale(Sx, Sy: Single);
    procedure Translate(Dx, Dy: Single);
  end;

procedure XTransform(Dst, Src: TBitmap32; Transformation: TXTransformation);

var
  FullEdge: Boolean = True;

implementation

{$R-}{$Q-}  // switch off overflow and range checking

uses GR32_System, Math;

type
  {provides access to proctected members of TBitmap32 by typecasting}
  TBitmap32Access = class(TBitmap32);

const
  SDstEmpty = 'Destination bitmap is nil or empty';
  SSrcEmpty = 'Source bitmap is nil or empty';
  SSrcInvalid = 'Source rectangle is invalid';

var
 BlockAverage : function (Dlx, Dly, RowSrc, OffSrc: Cardinal): TColor32;
 LinearInterpolator: function(PWX_256, PWY_256: Cardinal; C11, C21: PColor32): TColor32;


procedure CheckBitmaps(Dst, Src: TBitmap32);
begin
  if not Assigned(Dst) or Dst.Empty then raise ETransformError.Create(SDstEmpty);
  if not Assigned(Src) or Src.Empty then raise ETransformError.Create(SSrcEmpty);
end;

function CheckSrcRect(Src: TBitmap32; const SrcRect: TRect): Boolean;
begin
  Result := False;
  if IsRectEmpty(SrcRect) then Exit;
  if (SrcRect.Left < 0) or (SrcRect.Right > Src.Width) or
    (SrcRect.Top < 0) or (SrcRect.Bottom > Src.Height) then
    raise ETransformError.Create(SSrcInvalid);
  Result := True;
end;

procedure BlendBlock(
  Dst: TBitmap32; DstRect: TRect;
  Src: TBitmap32; SrcX, SrcY: Integer;
  CombineOp: TDrawMode; CombineCallBack: TPixelCombineEvent);
var
  SrcP, DstP: PColor32;
  SP, DP: PColor32;
  W, I, DstY: Integer;
  BlendLine: TBlendLine;
  BlendLineEx: TBlendLineEx;
begin
  { Internal routine }
  W := DstRect.Right - DstRect.Left;
  SrcP := Src.PixelPtr[SrcX, SrcY];
  DstP := Dst.PixelPtr[DstRect.Left, DstRect.Top];

  case CombineOp of
    dmOpaque:
      begin
        for DstY := DstRect.Top to DstRect.Bottom - 1 do
        begin
          //Move(SrcP^, DstP^, W*4); // for FastCode
          MoveLongWord(SrcP^, DstP^, W);
          Inc(SrcP, Src.Width);
          Inc(DstP, Dst.Width);
        end;
      end;
    dmBlend:
      if Src.MasterAlpha >= 255 then
      begin
        BlendLine := TBlendLine(BLEND_LINE[Src.CombineMode]);
        for DstY := DstRect.Top to DstRect.Bottom - 1 do
        begin
          BlendLine(SrcP, DstP, W);
          Inc(SrcP, Src.Width);
          Inc(DstP, Dst.Width);
        end
      end
      else
      begin
        BlendLineEx := TBlendLineEX(BLEND_LINE_EX[Src.CombineMode]);
        for DstY := DstRect.Top to DstRect.Bottom - 1 do
        begin
          BlendLineEx(SrcP, DstP, W, Src.MasterAlpha);
          Inc(SrcP, Src.Width);
          Inc(DstP, Dst.Width);
        end
      end
    else //  dmCustom:
      begin
        for DstY := DstRect.Top to DstRect.Bottom - 1 do
        begin
          SP := SrcP;
          DP := DstP;
          for I := 0 to W - 1 do
          begin
            CombineCallBack(SP^, DP^, Src.MasterAlpha);
            Inc(SP); Inc(DP);
          end;
          Inc(SrcP, Src.Width);
          Inc(DstP, Dst.Width);
        end;
      end;
    end;
end;

procedure BlockTransfer(
  Dst: TBitmap32; DstX: Integer; DstY: Integer; DstClip: TRect;
  Src: TBitmap32; SrcRect: TRect;
  CombineOp: TDrawMode; CombineCallBack: TPixelCombineEvent);
var
  SrcX, SrcY: Integer;
begin
  if Src.Empty then Exit;
  CheckBitmaps(Src, Dst);

  if (CombineOp = dmCustom) and not Assigned(CombineCallBack) then
    CombineOp := dmOpaque;

  if (CombineOp = dmBlend) and (Src.MasterAlpha = 0) then Exit;

  SrcX := SrcRect.Left;
  SrcY := SrcRect.Top;

  IntersectRect(DstClip, DstClip, Dst.BoundsRect);
  IntersectRect(SrcRect, SrcRect, Src.BoundsRect);
  OffsetRect(SrcRect, DstX - SrcX, DstY - SrcY);
  IntersectRect(SrcRect, DstClip, SrcRect);
  DstClip := SrcRect;
  OffsetRect(SrcRect, SrcX - DstX, SrcY - DstY);

  if not IsRectEmpty(SrcRect) then 
  try
    BlendBlock(Dst, DstClip, Src, SrcRect.Left, SrcRect.Top, CombineOp, CombineCallBack);
  finally
    EMMS;
  end;
end;


procedure StretchNearest(
  Dst: TBitmap32; DstRect, DstClip: TRect;
  Src: TBitmap32; SrcRect: TRect;
  CombineOp: TDrawMode; CombineCallBack: TPixelCombineEvent);
var
  R: TRect;
  SrcW, SrcH, DstW, DstH, DstClipW, DstClipH: Integer;
  SrcY, OldSrcY: Integer;
  I, J: Integer;
  MapHorz: array of Integer;
  SrcLine, DstLine: PColor32Array;
  Buffer: TArrayOfColor32;
  Scale: Single;
  BlendLine: TBlendLine;
  BlendLineEx: TBlendLineEx;
begin
  IntersectRect(DstClip, DstClip, MakeRect(0, 0, Dst.Width, Dst.Height));
  IntersectRect(DstClip, DstClip, DstRect);
  if IsRectEmpty(DstClip) then Exit;
  IntersectRect(R, DstClip, DstRect);
  if IsRectEmpty(R) then Exit;
  if (SrcRect.Left < 0) or (SrcRect.Top < 0) or (SrcRect.Right > Src.Width) or
    (SrcRect.Bottom > Src.Height) then raise Exception.Create('Invalid SrcRect');

  SrcW := SrcRect.Right - SrcRect.Left;
  SrcH := SrcRect.Bottom - SrcRect.Top;
  DstW := DstRect.Right - DstRect.Left;
  DstH := DstRect.Bottom - DstRect.Top;
  DstClipW := DstClip.Right - DstClip.Left;
  DstClipH := DstClip.Bottom - DstClip.Top;
  try
    if (SrcW = DstW) and (SrcH = DstH) then
    begin
      { Copy without resampling }
      BlendBlock(Dst, DstClip, Src, SrcRect.Left + DstClip.Left - DstRect.Left,
        SrcRect.Top + DstClip.Top - DstRect.Top, CombineOp, CombineCallBack);
    end
    else
    begin
      SetLength(MapHorz, DstClipW);

      if DstW > 1 then
      begin
        if FullEdge then
        begin
          Scale := SrcW / DstW;
          for I := 0 to DstClipW - 1 do
            MapHorz[I] := Trunc(SrcRect.Left + (I + DstClip.Left - DstRect.Left) * Scale);
        end
        else
        begin
          Scale := (SrcW - 1) / (DstW - 1);
          for I := 0 to DstClipW - 1 do
            MapHorz[I] := Round(SrcRect.Left + (I + DstClip.Left - DstRect.Left) * Scale);
        end;
        Assert(MapHorz[0] >= SrcRect.Left);
        Assert(MapHorz[DstClipW - 1] < SrcRect.Right);
      end
      else
        MapHorz[0] := (SrcRect.Left + SrcRect.Right - 1) div 2;

      if DstH <= 1 then Scale := 0
      else if FullEdge then Scale := SrcH / DstH
      else Scale := (SrcH - 1) / (DstH - 1);

      if CombineOp = dmOpaque then
      begin
        DstLine := PColor32Array(Dst.PixelPtr[DstClip.Left, DstClip.Top]);
        OldSrcY := -1;
        for J := 0 to DstClipH - 1 do
        begin
          if DstH <= 1 then
            SrcY := (SrcRect.Top + SrcRect.Bottom - 1) div 2
          else if FullEdge then
            SrcY := Trunc(SrcRect.Top + (J + DstClip.Top - DstRect.Top) * Scale)
          else
            SrcY := Round(SrcRect.Top + (J + DstClip.Top - DstRect.Top) * Scale);
          if SrcY <> OldSrcY then
          begin
            SrcLine := Src.ScanLine[SrcY];
            for I := 0 to DstClipW - 1 do DstLine[I] := SrcLine[MapHorz[I]];
            OldSrcY := SrcY;
          end
          else
            MoveLongWord(DstLine[-Dst.Width], DstLine[0], DstClipW);
          Inc(DstLine, Dst.Width);
        end;
      end
      else
      begin
        SetLength(Buffer, DstClipW);
        DstLine := PColor32Array(Dst.PixelPtr[DstClip.Left, DstClip.Top]);
        OldSrcY := -1;

        if Src.MasterAlpha >= 255 then
        begin
          BlendLine := TBlendLine(BLEND_LINE[Src.CombineMode]);
          BlendLineEx := nil; // stop compiler warnings...
        end
        else
        begin
          BlendLineEx := TBlendLineEX(BLEND_LINE_EX[Src.CombineMode]);
          BlendLine := nil; // stop compiler warnings...
        end;

        for J := 0 to DstClipH - 1 do
        begin
          if DstH > 1 then
          begin
            EMMS;
            if FullEdge then
              SrcY := Trunc(SrcRect.Top + (J + DstClip.Top - DstRect.Top) * Scale)
            else
              SrcY := Round(SrcRect.Top + (J + DstClip.Top - DstRect.Top) * Scale);
          end
          else
            SrcY := (SrcRect.Top + SrcRect.Bottom - 1) div 2;
          if SrcY <> OldSrcY then
          begin
            SrcLine := Src.ScanLine[SrcY];
            for I := 0 to DstClipW - 1 do Buffer[I] := SrcLine[MapHorz[I]];
            OldSrcY := SrcY;
          end;

          if CombineOp = dmBlend then
          begin
            if Src.MasterAlpha >= 255 then
              BlendLine(@Buffer[0], @DstLine[0], DstClipW)
            else
              BlendLineEx(@Buffer[0], @DstLine[0], DstClipW, Src.MasterAlpha);
          end
          else
            for I := 0 to DstClipW - 1 do
              CombineCallBack(Buffer[I], DstLine[I], Src.MasterAlpha);

          Inc(DstLine, Dst.Width);
        end;
      end;
    end;
  finally
    EMMS;
  end;
end;

type
  TPointRec = record
    Pos: Integer;
    Weight: Cardinal;
  end;

procedure StretchHorzStretchVertLinear(
  Dst: TBitmap32; DstRect, DstClip: TRect;
  Src: TBitmap32; SrcRect: TRect;
  CombineOp: TDrawMode; CombineCallBack: TPixelCombineEvent);
//Assure DstRect is >= SrcRect, otherwise quality loss will occur
var
  SrcW, SrcH, DstW, DstH, DstClipW, DstClipH: Integer;
  MapHorz, MapVert: array of TPointRec;
  t2, Scale: Single;
  SrcLine, DstLine: PColor32Array;
  SrcIndex: Integer;
  I, J: Integer;
  WY: Cardinal;
  C: TColor32;
  BlendMemEx: TBlendMemEx;
begin
  SrcW := SrcRect.Right - SrcRect.Left;
  SrcH := SrcRect.Bottom - SrcRect.Top;
  DstW := DstRect.Right - DstRect.Left;
  DstH := DstRect.Bottom - DstRect.Top;
  DstClipW := DstClip.Right - DstClip.Left;
  DstClipH := DstClip.Bottom - DstClip.Top;

  SetLength(MapHorz, DstClipW);
  if FullEdge then Scale := SrcW / DstW
  else Scale := (SrcW - 1) / (DstW - 1);
  for I := 0 to DstClipW - 1 do
  begin
    if FullEdge then t2 := SrcRect.Left - 0.5 + (I + DstClip.Left - DstRect.Left + 0.5) * Scale
    else t2 := SrcRect.Left + (I + DstClip.Left - DstRect.Left) * Scale;
    if t2 < 0 then t2 := 0
    else if t2 > Src.Width - 1 then t2 := Src.Width - 1;
    MapHorz[I].Pos := Floor(t2);
    MapHorz[I].Weight := 256 - Round(Frac(t2) * 256);
    //Pre-pack weights to reduce MMX Reg. setups per pixel:
    MapHorz[I].Weight:= MapHorz[I].Weight shl 16 + MapHorz[I].Weight;
  end;
  I := DstClipW - 1;
  while MapHorz[I].Pos = SrcRect.Right - 1 do
  begin
    Dec(MapHorz[I].Pos);
    MapHorz[I].Weight := 0;
    Dec(I);
  end;

  SetLength(MapVert, DstClipH);
  if FullEdge then Scale := SrcH / DstH
  else Scale := (SrcH - 1) / (DstH - 1);
  for I := 0 to DstClipH - 1 do
  begin
    if FullEdge then t2 := SrcRect.Top - 0.5 + (I + DstClip.Top - DstRect.Top + 0.5) * Scale
    else t2 := SrcRect.Top + (I + DstClip.Top - DstRect.Top) * Scale;
    if t2 < 0 then t2 := 0
    else if t2 > Src.Height - 1 then t2 := Src.Height - 1;
    MapVert[I].Pos := Floor(t2);
    MapVert[I].Weight := 256 - Round(Frac(t2) * 256);
    //Pre-pack weights to reduce MMX Reg. setups per pixel:
    MapVert[I].Weight := MapVert[I].Weight shl 16 + MapVert[I].Weight;
  end;
  I := DstClipH - 1;
  while MapVert[I].Pos = SrcRect.Bottom - 1 do
  begin
    Dec(MapVert[I].Pos);
    MapVert[I].Weight := 0;
    Dec(I);
  end;

  DstLine := PColor32Array(Dst.PixelPtr[DstClip.Left, DstClip.Top]);
  case CombineOp of
    dmOpaque:
      for J := 0 to DstClipH - 1 do
      begin
        SrcLine := Src.ScanLine[MapVert[J].Pos];
        WY := MapVert[J].Weight;
        for I := 0 to DstClipW - 1 do
        begin
          SrcIndex := MapHorz[I].Pos;
          DstLine[I] := LinearInterpolator( MapHorz[I].Weight, WY, @SrcLine[SrcIndex],
                                            @SrcLine[SrcIndex + Src.Width]);
        end;
        Inc(DstLine, Dst.Width);
      end;
    dmBlend:
      begin
        BlendMemEx := TBlendMemEx(BLEND_MEM_EX[Src.CombineMode]);
        for J := 0 to DstClipH - 1 do
        begin
          SrcLine := Src.ScanLine[MapVert[J].Pos];
          WY := MapVert[J].Weight;
          for I := 0 to DstClipW - 1 do
          begin
            SrcIndex := MapHorz[I].Pos;
            C := LinearInterpolator( MapHorz[I].Weight, WY, @SrcLine[SrcIndex],
                                     @SrcLine[SrcIndex + Src.Width]);
            BlendMemEx(C, DstLine[I], Src.MasterAlpha)
          end;
          Inc(DstLine, Dst.Width);
        end
      end
  else // cmCustom
    for J := 0 to DstClipH - 1 do
    begin
      SrcLine := Src.ScanLine[MapVert[J].Pos];
      WY := MapVert[J].Weight;
      for I := 0 to DstClipW - 1 do
      begin
        SrcIndex := MapHorz[I].Pos;
        C := LinearInterpolator( MapHorz[I].Weight, WY, @SrcLine[SrcIndex],
                                 @SrcLine[SrcIndex + Src.Width]);
        CombineCallBack(C, DstLine[I], Src.MasterAlpha);
      end;
      Inc(DstLine, Dst.Width);
    end;
  end;
  EMMS;
end;

{ StretchFlt }

type
  TCluster = array of TPointRec;
  TMappingTable = array of TCluster;
  TFilterFunc = function(Value: Single): Single;

function NearestFilter(Value: Single): Single;
begin
  if (Value > -0.5) and (Value <= 0.5) then Result := 1
  else Result := 0;
end;

function LinearFilter(Value: Single): Single;
begin
  if Value < -1 then Result := 0
  else if Value < 0 then Result := 1 + Value
  else if Value < 1 then Result := 1 - Value
  else Result := 0;
end;

function DraftFilter(Value: Single): Single;
//This function is only present to keep compatibility
//Draft resampling is handled separately, and this function will never be used.
//But since draft resampling is closest to linear, this function is provided.
begin
  if Value < -1 then Result := 0
  else if Value < 0 then Result := 1 + Value
  else if Value < 1 then Result := 1 - Value
  else Result := 0;
end;

function CosineFilter(Value: Single): Single;
begin
  Result := 0;
  if Abs(Value) < 1 then
    Result := (Cos(Value * Pi) + 1) / 2;
end;

function SplineFilter(Value: Single): Single;
var
  tt: Single;
begin
  Value := Abs(Value);
  if Value < 1 then
  begin
    tt := Sqr(Value);
    Result := 0.5 * tt * Value - tt + 2 / 3;
  end
  else if Value < 2 then
  begin
    Value := 2 - Value;
    Result := 1 / 6 * Sqr(Value) * Value;
  end
  else Result := 0;
end;

function LanczosFilter(Value: Single): Single;
  function Sinc(Value: Single): Single;
  begin
    if Value <> 0 then
    begin
      Value := Value * Pi;
      Result := Sin(Value) / Value;
    end
    else Result := 1;
  end;
begin
  Value := Abs(Value);
  if Value < 3 then Result := Sinc(Value) * Sinc(Value / 3)
  else Result := 0;
end;

function MitchellFilter(Value: Single): Single;
var
  tt, ttt: Single;
begin
  Value := Abs(Value);
  tt := Sqr(Value);
  ttt := tt * Value;
  if Value < 1 then Result := (7 * ttt + -12 * tt + 16 / 3) / 6
  else if Value < 2 then Result := (-7 / 3 * ttt + 12 * tt - 20 * Value + 32 / 3) / 6
  else Result := 0;
end;


function BuildMappingTable(
  DstLo, DstHi: Integer;
  ClipLo, ClipHi: Integer;
  SrcLo, SrcHi: Integer;
  StretchFilter: TsStretchFilter): TMappingTable;
const
  { the first filter from these arrays is never used since the nearest and linear
    filter is implemented separately. This also applies to draft-resampling }
  FILTERS: array[TsStretchFilter] of TFilterFunc = (NearestFilter, DraftFilter, LinearFilter,
    CosineFilter, SplineFilter, LanczosFilter, MitchellFilter);
  FILTERWIDTHS: array [TsStretchFilter] of Single = (1, 1, 1, 1, 2, 3, 2);
var
  SrcW, DstW, ClipW: Integer;
  Filter: TFilterFunc;
  FilterWidth: Single;
  Scale, OldScale: Single;
  Center: Single;
  Count: Integer;
  Left, Right: Integer;
  I, J, K: Integer;
  Weight: Integer;
begin
  SrcW := SrcHi - SrcLo;
  DstW := DstHi - DstLo;
  ClipW := ClipHi - ClipLo;
  if SrcW = 0 then
  begin
    Result := nil;
    Exit;
  end
  else if SrcW = 1 then
  begin
    SetLength(Result, ClipW);
    for I := 0 to ClipW - 1 do
    begin
      SetLength(Result[I], 1);
      Result[I][0].Pos := 0;
      Result[I][0].Weight := 256;
    end;
    Exit;
  end;
  SetLength(Result, ClipW);
  if ClipW = 0 then Exit;

  if FullEdge then Scale := DstW / SrcW
  else Scale := (DstW - 1) / (SrcW - 1);

  Filter := FILTERS[StretchFilter];
  FilterWidth := FILTERWIDTHS[StretchFilter];
  K := 0;

  if Scale = 0 then
  begin
    Assert(Length(Result) = 1);
    SetLength(Result[0], 1);
    Result[0][0].Pos := (SrcLo + SrcHi) div 2;
    Result[0][0].Weight := 256;
  end
  else if Scale < 1 then
  begin
    OldScale := Scale;
    Scale := 1 / Scale;
    FilterWidth := FilterWidth * Scale;
    for I := 0 to ClipW - 1 do
    begin
      if FullEdge then
        Center := SrcLo - 0.5 + (I - DstLo + ClipLo + 0.5) * Scale
      else
        Center := SrcLo + (I - DstLo + ClipLo) * Scale;
      Left := Floor(Center - FilterWidth);
      Right := Ceil(Center + FilterWidth);
      Count := -256;
      for J := Left to Right do
      begin
        Weight := Round(256 * Filter((Center - J) * OldScale) * OldScale);
        if Weight <> 0 then
        begin
          Inc(Count, Weight);
          K := Length(Result[I]);
          SetLength(Result[I], K + 1);
          Result[I][K].Pos := Constrain(J, SrcLo, SrcHi - 1);
          Result[I][K].Weight := Weight;
        end;
      end;
      if Length(Result[I]) = 0 then
      begin
        SetLength(Result[I], 1);
        Result[I][0].Pos := Floor(Center);
        Result[I][0].Weight := 256;
      end
      else if Count <> 0 then
        Dec(Result[I][K div 2].Weight, Count);
    end;
  end
  else // scale > 1
  begin
    Scale := 1 / Scale;
    for I := 0 to ClipW - 1 do
    begin
      if FullEdge then
        Center := SrcLo - 0.5 + (I - DstLo + ClipLo + 0.5) * Scale
      else
        Center := SrcLo + (I - DstLo + ClipLo) * Scale;
      Left := Floor(Center - FilterWidth);
      Right := Ceil(Center + FilterWidth);
      Count := -256;
      for J := Left to Right do
      begin
        Weight := Round(256 * Filter(Center - j));
        if Weight <> 0 then
        begin
          Inc(Count, Weight);
          K := Length(Result[I]);
          SetLength(Result[I], k + 1);
          Result[I][K].Pos := Constrain(j, SrcLo, SrcHi - 1);
          Result[I][K].Weight := Weight;
        end;
      end;
      if Count <> 0 then
        Dec(Result[I][K div 2].Weight, Count);
    end;
  end;
end;

{$WARNINGS OFF}
procedure Resample(
  Dst: TBitmap32; DstRect: TRect; DstClip: TRect;
  Src: TBitmap32; SrcRect: TRect;
  StretchFilter: TsStretchFilter;
  CombineOp: TDrawMode; CombineCallBack: TPixelCombineEvent);
type
  TBufferEntry = record
    R, G, B, A: Integer;
  end;
var
  SrcW, SrcH: Single;
  DstW, DstH: Integer;
  DstClipW, DstClipH: Integer;
  t: Single;
  MapX, MapY: TMappingTable;
  I, J, X, Y, Index: Integer;
  MapXLoPos, MapXHiPos: Integer;
  HorzBuffer: array of TBufferEntry;
  ClusterX, ClusterY: TCluster;
  ClusterXSize, ClusterYSize: Integer;
  C, Wt, Cr, Cg, Cb, Ca: Integer;
  ClustYP, ClustYW, ClustXP, ClustXW: Integer;
  SrcP: PColor32;
  DstLine: PColor32Array;
  RangeCheck: Boolean;
  BlendMemEx: TBlendMemEx;
begin
  if (CombineOp = dmCustom) and not Assigned(CombineCallBack) then
    CombineOp := dmOpaque;

  { check source and destination }
  if (CombineOp = dmBlend) and (Src.MasterAlpha = 0) then Exit;

  BlendMemEx := TBlendMemEx(BLEND_MEM_EX[Src.CombineMode]); // store in local variable

  SrcW := SrcRect.Right - SrcRect.Left;
  SrcH := SrcRect.Bottom - SrcRect.Top;
  DstW := DstRect.Right - DstRect.Left;
  DstH := DstRect.Bottom - DstRect.Top;
  DstClipW := DstClip.Right - DstClip.Left;
  DstClipH := DstClip.Bottom - DstClip.Top;

  // mapping tables
  MapX := BuildMappingTable(DstRect.Left, DstRect.Right, DstClip.Left, DstClip.Right, SrcRect.Left, SrcRect.Right, StretchFilter);
  MapY := BuildMappingTable(DstRect.Top, DstRect.Bottom, DstClip.Top, DstClip.Bottom, SrcRect.Top, SrcRect.Bottom, StretchFilter);
  ClusterX := nil;
  ClusterY := nil;
  try
    RangeCheck := StretchFilter in [sfLanczos, sfMitchell];
    if (MapX = nil) or (MapY = nil) then Exit;

    MapXLoPos := MapX[0][0].Pos;
    MapXHiPos := MapX[DstClipW - 1][High(MapX[DstClipW - 1])].Pos;
    SetLength(HorzBuffer, MapXHiPos - MapXLoPos + 1);

    { transfer pixels }
    for J := DstClip.Top to DstClip.Bottom - 1 do
    begin
      ClusterY := MapY[J - DstClip.Top];
      for X := MapXLoPos to MapXHiPos do
      begin
        Ca := 0; Cr := 0; Cg := 0; Cb := 0;
        for Y := 0 to Length(ClusterY) - 1 do
        begin
          C := Src.Bits[X + ClusterY[Y].Pos * Src.Width];
          ClustYW := ClusterY[Y].Weight;
          Inc(Ca, C shr 24 * ClustYW);
          Inc(Cr, (C and $00FF0000) shr 16 * ClustYW);
          Inc(Cg, (C and $0000FF00) shr 8 * ClustYW);
          Inc(Cb, (C and $000000FF) * ClustYW);
        end;
        with HorzBuffer[X - MapXLoPos] do
        begin
          R := Cr;
          G := Cg;
          B := Cb;
          A := Ca;
        end;
      end;

      DstLine := Dst.ScanLine[J];
      for I := DstClip.Left to DstClip.Right - 1 do
      begin
        ClusterX := MapX[I - DstClip.Left];
        Ca := 0; Cr := 0; Cg := 0; Cb := 0;
        for X := 0 to Length(ClusterX) - 1 do
        begin
          Wt := ClusterX[X].Weight;
          with HorzBuffer[ClusterX[X].Pos - MapXLoPos] do
          begin
            Inc(Ca, A * Wt);
            Inc(Cr, R * Wt);
            Inc(Cg, G * Wt);
            Inc(Cb, B * Wt);
          end;
        end;

        if RangeCheck then
        begin
          if Ca > $FF0000 then Ca := $FF0000
          else if Ca < 0 then Ca := 0
          else Ca := Ca and $00FF0000;

          if Cr > $FF0000 then Cr := $FF0000
          else if Cr < 0 then Cr := 0
          else Cr := Cr and $00FF0000;

          if Cg > $FF0000 then Cg := $FF0000
          else if Cg < 0 then Cg := 0
          else Cg := Cg and $00FF0000;

          if Cb > $FF0000 then Cb := $FF0000
          else if Cb < 0 then Cb := 0
          else Cb := Cb and $00FF0000;

          C := (Ca shl 8) or Cr or (Cg shr 8) or (Cb shr 16);
        end
        else
          C := ((Ca and $00FF0000) shl 8) or (Cr and $00FF0000) or ((Cg and $00FF0000) shr 8) or ((Cb and $00FF0000) shr 16);

        // combine it with the background
        case CombineOp of
          dmOpaque: DstLine[I] := C;
          dmBlend: BlendMemEx(C, DstLine[I], Src.MasterAlpha);
          dmCustom: CombineCallBack(C, DstLine[I], Src.MasterAlpha);
        end;
      end;
    end;
  finally
    EMMS;
    MapX := nil;
    MapY := nil;
  end;
end;
{$WARNINGS ON}


function BlockAverage_IA32(Dlx, Dly, RowSrc, OffSrc: Cardinal): TColor32;
type
 PCardinal = ^Cardinal;
 PRGBA = ^TRGBA;
 TRGBA = record B,G,R,A: Byte end;
var
 C: PRGBA;
 ix, iy, iA, iR, iG, iB, Area: Cardinal;
begin
  iR := 0;  iB := iR;  iG := iR;  iA := iR;
  for iy := 1 to Dly do
  begin
    C:= PRGBA(RowSrc);
    for ix := 1 to Dlx do
    begin
      inc(iB, C.B);
      inc(iG, C.G);
      inc(iR, C.R);
      inc(iA, C.A);
      inc(C);
    end;
    inc(RowSrc, OffSrc);
  end;

  Area := Dlx * Dly;
  Area := $1000000 div Area;
  Result := iA * Area and $FF000000 or
            iR * Area shr  8 and $FF0000 or
            iG * Area shr 16 and $FF00 or
            iB * Area shr 24 and $FF;
end;

procedure DraftResample(Dst: TBitmap32; DstRect: TRect; DstClip: TRect;
                        Src: TBitmap32; SrcRect: TRect;
                        CombineOp: TDrawMode; CombineCallBack: TPixelCombineEvent);
var
  SrcW, SrcH,
  DstW, DstH,
  DstClipW, DstClipH: Cardinal;
  RowSrc, OffSrc,
  dy, dx,
  c1, c2, r1, r2,
  xs, xsrc, M: Cardinal;
  C: TColor32;
  DstLine: PColor32Array;
  ScaleFactor,lx, fe: Single;
  FSrcTop,I,J,ly,
  sc, sr, cx, cy: integer;
  Y_256: TFixed;
  BlendMemEx: TBlendMemEx;
begin
 { rangechecking and rect intersection done by caller }

  SrcW := SrcRect.Right  - SrcRect.Left;
  SrcH := SrcRect.Bottom - SrcRect.Top;

  DstW := DstRect.Right  - DstRect.Left;
  DstH := DstRect.Bottom - DstRect.Top;

  DstClipW := DstClip.Right - DstClip.Left;
  DstClipH := DstClip.Bottom - DstClip.Top;

  BlendMemEx := TBlendMemEx(BLEND_MEM_EX[Src.CombineMode]);

  if (DstW > SrcW)or(DstH > SrcH) then begin
    if (SrcW < 2) or (SrcH < 2) then
      Resample(Dst, DstRect, DstClip, Src, SrcRect, sfLinear, CombineOp, CombineCallBack)
    else
      StretchHorzStretchVertLinear(Dst, DstRect, DstClip, Src, SrcRect, CombineOp, CombineCallBack);
    end
  else
    begin //Full Scaledown, ignores Fulledge - cannot be integrated into this resampling method
      OffSrc := Src.Width * 4;

      ScaleFactor:= SrcW / DstW;
      cx := Trunc( (DstClip.Left - DstRect.Left) * ScaleFactor);
      r2 := Trunc(ScaleFactor);
      sr := Trunc( $10000 * ScaleFactor );

      ScaleFactor:= SrcH / DstH;
      cy := Trunc( (DstClip.Top - DstRect.Top) * ScaleFactor);
      c2 := Trunc(ScaleFactor);
      sc := Trunc( $10000 * ScaleFactor );

      DstLine := PColor32Array(Dst.PixelPtr[0, DstClip.Top]);
      RowSrc := Cardinal(Src.PixelPtr[ SrcRect.Left +  cx, SrcRect.Top + cy ]);

      xs := r2;
      c1 := 0;
      Dec(DstClip.Left, 2);
      Inc(DstClipW);
      Inc(DstClipH);

      for J := 2  to DstClipH do
      begin
        dy := c2 - c1;
        c1 := c2;
        c2 := J * sc shr 16;
        r1 := 0;
        r2 := xs;
        xsrc := RowSrc;

        case CombineOp of
          dmOpaque:
            for I := 2  to DstClipW do
            begin
              dx := r2 - r1;  r1 := r2;
              r2 := I * sr shr 16;
              DstLine[DstClip.Left + I]:= BlockAverage(dx, dy, xsrc, OffSrc);
              xsrc := xsrc + dx shl 2;
            end;
          dmBlend:
            for I := 2  to DstClipW do
            begin
              dx := r2 - r1;  r1 := r2;
              r2 := I * sr shr 16;
              BlendMemEx(BlockAverage(dx, dy, xsrc, OffSrc), DstLine[DstClip.Left + I], Src.MasterAlpha);
              xsrc := xsrc + dx shl 2;
            end;
          dmCustom:
            for I := 2  to DstClipW do
            begin
              dx := r2 - r1;  r1 := r2;
              r2 := I * sr shr 16;
              CombineCallBack(BlockAverage(dx, dy, xsrc, OffSrc), DstLine[DstClip.Left + I], Src.MasterAlpha);
              xsrc := xsrc + dx shl 2;
            end;
        end;

        Inc(DstLine, Dst.Width);
        Inc(RowSrc, OffSrc * dy);
      end;
    end;
  EMMS;
end;

{ Stretch Transfer }

{$WARNINGS OFF}
procedure StretchTransfer(
  Dst: TBitmap32; DstRect: TRect; DstClip: TRect;
  Src: TBitmap32; SrcRect: TRect;
  StretchFilter: TsStretchFilter;
  CombineOp: TDrawMode; CombineCallBack: TPixelCombineEvent);
var
  SrcW, SrcH: Single;
  DstW, DstH: Integer;
  R: TRect;
begin
  if Src.Empty then Exit;
  CheckBitmaps(Dst, Src);
  if not CheckSrcRect(Src, SrcRect) then Exit;
  IntersectRect(DstClip, DstClip, MakeRect(0, 0, Dst.Width, Dst.Height));
  IntersectRect(DstClip, DstClip, DstRect);
  if IsRectEmpty(DstClip) then Exit;
  IntersectRect(R, DstClip, DstRect);
  if IsRectEmpty(R) then Exit;

  if (CombineOp = dmCustom) and not Assigned(CombineCallBack) then CombineOp := dmOpaque;
  if (CombineOp = dmBlend) and (Src.MasterAlpha = 0) then Exit;

  SrcW := SrcRect.Right - SrcRect.Left;
  SrcH := SrcRect.Bottom - SrcRect.Top;
  DstW := DstRect.Right - DstRect.Left;
  DstH := DstRect.Bottom - DstRect.Top;

  try
    if (SrcW = DstW) and (SrcH = DstH) then
      BlendBlock(Dst, DstClip, Src, SrcRect.Left + DstClip.Left - DstRect.Left,
        SrcRect.Top + DstClip.Top - DstRect.Top, CombineOp, CombineCallBack)
    else case StretchFilter of
      sfNearest: StretchNearest(Dst, DstRect, DstClip, Src, SrcRect, CombineOp, CombineCallBack);
      sfDraft: DraftResample(Dst, DstRect, DstClip, Src, SrcRect, CombineOp, CombineCallBack);
      sfLinear:
        if (DstW > SrcW) and (DstH > SrcH) and (SrcW > 1) and (SrcH > 1) then
          StretchHorzStretchVertLinear(Dst, DstRect, DstClip, Src, SrcRect, CombineOp, CombineCallBack)
        else
          Resample(Dst, DstRect, DstClip, Src, SrcRect, sfLinear, CombineOp, CombineCallBack);
    else
      Resample(Dst, DstRect, DstClip, Src, SrcRect, StretchFilter, CombineOp, CombineCallBack);
    end;

  finally
    EMMS;
  end;
end;
{$WARNINGS ON}

//============================================================================================
{ Transformation functions }

procedure XTransform(Dst, Src: TBitmap32; Transformation: TXTransformation);
var
  C, SrcAlpha: TColor32;
  R, SrcRectI, DstRect, SrcRect256: TRect;
  Pixels: PColor32Array;
  I, J, X, Y: Integer;
  DrawMode: TDrawMode;
  CombineCallBack: TPixelCombineEvent;

  function GET_S256(X256, Y256: Integer; out C: TColor32): Boolean;
  begin
   X := SAR_8(X256);
   Y := SAR_8(Y256);
   if (x > SrcRectI.Left) and (x < SrcRectI.Right - 1) and
      (y > SrcRectI.Top) and (y < SrcRectI.Bottom - 1) then
    begin
      // everything is ok interpolate between four neighbors
      C:= Src.PixelX[X256 shl 8, Y256 shl 8];
      Result := True;
    end
    else if (x < SrcRectI.Left - 1) or (y < SrcRectI.Top - 1) or
            (x >= SrcRectI.Right) or (y >= SrcRectI.Bottom) then
    begin
      // (X,Y) coordinate is out of the SrcRect, do not interpolate
      C := 0; // just write something to disable compiler warnings
      Result := False;
    end
    else
    begin
      // handle edge
      C:= Src.PixelXS[X256 shl 8, Y256 shl 8];
      Result := True;
    end;
  end;

begin
  if not Transformation.TransformValid then Transformation.PrepareTransform;

  CombineCallBack := Src.OnPixelCombine; // store it into a local variable
  DrawMode := Src.DrawMode;    // store it into a local variable
  SrcAlpha := Src.MasterAlpha;
  if (DrawMode = dmCustom) and not Assigned(CombineCallBack) then
    DrawMode := dmOpaque;

  // clip SrcRect

  // workaround C++ Builder throwing exceptions:
  R := MakeRect(Round(Transformation.SrcRect.Left), Round(Transformation.SrcRect.Top),
                Round(Transformation.SrcRect.Right), Round(Transformation.SrcRect.Bottom));

  IntersectRect(SrcRectI, R, MakeRect(0, 0, Src.Width - 1, Src.Height - 1));

  with Transformation.SrcRect do
  begin
    IntersectRect(SrcRect256, MakeRect(Round(Left * 256), Round(Top * 256),
      Round(Right * 256), Round(Bottom * 256)), MakeRect(0, 0, (Src.Width - 1) * 256,
      (Src.Height - 1) * 256));
  end;

  // clip DstRect
  R := Transformation.GetTransformedBounds;
  IntersectRect(DstRect, R, MakeRect(0, 0, Dst.Width - 1, Dst.Height - 1));
  if IsRectEmpty(DstRect) then Exit;

  try
  {  if Src.StretchFilter <> sfNearest then          // SOS 9999
      for J := DstRect.Top to DstRect.Bottom do
      begin
        Pixels := Dst.ScanLine[J];
        for I := DstRect.Left to DstRect.Right do
        begin
          Transformation.Transform256(I, J, X, Y);
          if GET_S256(X, Y, C) then
            case DrawMode of
              dmOpaque: Pixels[I] := C;
              dmBlend: BlendMemEx(C, Pixels[I], SrcAlpha);
            else // dmCustom:
              CombineCallBack(C, Pixels[I], SrcAlpha);
            end;
        end;
      end
    else // nearest filter  }
      for J := DstRect.Top to DstRect.Bottom do
      begin
        Pixels := Dst.ScanLine[J];
        for I := DstRect.Left to DstRect.Right do
        begin
          Transformation.Transform(I, J, X, Y);
          if (X >= SrcRectI.Left) and (X <= SrcRectI.Right) and
            (Y >= SrcRectI.Top) and (Y <= SrcRectI.Bottom) then
          case DrawMode of
            dmOpaque: Pixels[I] := Src.Pixel[X, Y];
            dmBlend: BlendMemEx(Src.Pixel[X, Y], Pixels[I], SrcAlpha);
          else // dmCustom:
            CombineCallBack(Src.Pixel[X, Y], Pixels[I], SrcAlpha);
          end;
        end;
      end;
  finally
    EMMS;
  end;
  Dst.Changed;
end;


//=================================  TXTransformation =====================================

procedure TXTransformation.SetSrcRect(const Value: TFloatRect);
begin
  FSrcRect := Value;
  TransformValid := False;
end;


// ================================= TXAffineTransformation ===============================

procedure TXAffineTransformation.Clear;
begin
  Matrix := IdentityMatrix;
  TransformValid := False;
end;

constructor TXAffineTransformation.Create;
begin
  Clear;
end;

function TXAffineTransformation.GetTransformedBounds: TRect;
var
  V1, V2, V3, V4: TVector3f;
begin
  V1[0] := FSrcRect.Left;  V1[1] := FSrcRect.Top;    V1[2] := 1;
  V2[0] := FSrcRect.Right; V2[1] := V1[1];           V2[2] := 1;
  V3[0] := V1[0];          V3[1] := FSrcRect.Bottom; V3[2] := 1;
  V4[0] := V2[0];          V4[1] := V3[1];           V4[2] := 1;
  V1 := VectorTransform(Matrix, V1);
  V2 := VectorTransform(Matrix, V2);
  V3 := VectorTransform(Matrix, V3);
  V4 := VectorTransform(Matrix, V4);
  Result.Left   := Round(Min(Min(V1[0], V2[0]), Min(V3[0], V4[0])) - 0.5);
  Result.Right  := Round(Max(Max(V1[0], V2[0]), Max(V3[0], V4[0])) + 0.5);
  Result.Top    := Round(Min(Min(V1[1], V2[1]), Min(V3[1], V4[1])) - 0.5);
  Result.Bottom := Round(Max(Max(V1[1], V2[1]), Max(V3[1], V4[1])) + 0.5);
end;

procedure TXAffineTransformation.PrepareTransform;
var
  M: TFloatMatrix;
begin
  M := Matrix;
  Invert(M);

  // calculate a fixed point (4096) factors
  A := Round(M[0,0] * 4096); B := Round(M[1,0] * 4096); C := Round(M[2,0] * 4096);
  D := Round(M[0,1] * 4096); E := Round(M[1,1] * 4096); F := Round(M[2,1] * 4096);

  TransformValid := True;
end;

procedure TXAffineTransformation.Rotate(Cx, Cy, Alpha: Single);
var
  S, C: Single;
  M: TFloatMatrix;
begin
  TransformValid := False;
  if (Cx <> 0) or (Cy <> 0) then Translate(-Cx, -Cy);
  Alpha := DegToRad(Alpha);
  S := Sin(Alpha); C := Cos(Alpha);
  M := IdentityMatrix;
  M[0,0] := C;   M[1,0] := S;
  M[0,1] := -S;  M[1,1] := C;
  Matrix := Mult(M, Matrix);
  if (Cx <> 0) or (Cy <> 0) then Translate(Cx, Cy);
end;

procedure TXAffineTransformation.Scale(Sx, Sy: Single);
var
  M: TFloatMatrix;
begin
  TransformValid := False;
  M := IdentityMatrix;
  M[0,0] := Sx;
  M[1,1] := Sy;
  Matrix := Mult(M, Matrix);
end;

procedure TXAffineTransformation.Skew(Fx, Fy: Single);
var
  M: TFloatMatrix;
begin
  TransformValid := False;
  M := IdentityMatrix;
  M[1, 0] := Fx;
  M[0, 1] := Fy;
  Matrix := Mult(M, Matrix);
end;

procedure TXAffineTransformation.Transform(
  DstX, DstY: Integer;
  out SrcX, SrcY: Integer);
begin
  SrcX := SAR_12(DstX * A + DstY * B + C + 2047);
  SrcY := SAR_12(DstX * D + DstY * E + F + 2047);
end;

procedure TXAffineTransformation.Transform256(
  DstX, DstY: Integer;
  out SrcX256, SrcY256: Integer);
begin
  SrcX256 := SAR_4(DstX * A + DstY * B + C + 7);
  SrcY256 := SAR_4(DstX * D + DstY * E + F + 7);
end;

procedure TXAffineTransformation.Translate(Dx, Dy: Single);
var
  M: TFloatMatrix;
begin
  TransformValid := False;
  M := IdentityMatrix;
  M[2,0] := Dx;
  M[2,1] := Dy;
  Matrix := Mult(M, Matrix);
end;

function _LinearInterpolator(PWX_256, PWY_256: Cardinal; C11, C21: PColor32): TColor32;
var
  C1, C3: TColor32;
begin
  PWX_256:= PWX_256 shr 16; if PWX_256 > $FF then PWX_256:= $FF;
  PWY_256:= PWY_256 shr 16; if PWY_256 > $FF then PWY_256:= $FF;
  C1 := C11^; Inc(C11);
  C3 := C21^; Inc(C21);
  Result := CombineReg(CombineReg(C1, C11^, PWX_256),
                       CombineReg(C3, C21^, PWX_256), PWY_256);
end;


procedure SetupFunctions;
begin
   BlockAverage:= BlockAverage_IA32;
   LinearInterpolator:= _LinearInterpolator;
end;

//====================================================

initialization
 SetupFunctions;

end.
