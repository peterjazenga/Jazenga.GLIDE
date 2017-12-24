
{**********************************************************************
 Package pl_Graphics32EXT.pkg
 this unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit XGR32_RasterizerEx;

interface

uses
  Classes, GR32, GR32_Rasterizers,GR32_Resamplers;

type

  TBoxRasterizer = class(TRasterizer)
  private
    FBoxSize: Integer;
    procedure SetBoxSize(const Value: Integer);
  protected
    procedure DoRasterize(Dst: TBitmap32; DstRect: TRect); Virtual;
  public
    constructor Create; override;
  published
    property BoxSize: Integer read FBoxSize write SetBoxSize default 4;
  end;


implementation

uses Math;

type
  TThreadPersistentAccess = class(TThreadPersistent);


constructor TBoxRasterizer.Create;
begin
  inherited;
  FBoxSize := 4;
end;

procedure TBoxRasterizer.DoRasterize(Dst: TBitmap32; DstRect: TRect);
var
  I, J, B: Integer;
  GetSample: TGetSampleInt;
begin
  GetSample := @Sampler.GetSampleInt;
  Dst.BeginUpdate;
//  W := DstRect.Right - DstRect.Left;
//  H := DstRect.Bottom - DstRect.Top;
  J := DstRect.Top;
  while J < DstRect.Bottom do
  begin
    I := DstRect.Left;
    B := Min(J + FBoxSize, DstRect.Bottom);
    while I < DstRect.Right - FBoxSize do
    begin
      Dst.FillRect(I, J, I + FBoxSize, B, GetSample(I, J));
      Inc(I, FBoxSize);
    end;
    Dst.FillRect(I, J, DstRect.Right, B, GetSample(I, J));
    Inc(J, FBoxSize);
  end;
  if (TThreadPersistentAccess(Dst).UpdateCount = 0) and Assigned(Dst.OnAreaChanged) then
    Dst.OnAreaChanged(Dst, DstRect, AREAINFO_RECT);
  Dst.EndUpdate;
end;

procedure TBoxRasterizer.SetBoxSize(const Value: Integer);
begin
  if (FBoxSize <> Value) and (Value > 1) then
  begin
    FBoxSize := Value;
    Changed;
  end;
end;

end.
