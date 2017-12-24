unit kaaj_progressbar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpg_base, fpg_main, fpg_progressbar, kaaj_theme,
  BGRABitmap, BGRABitmapTypes, BGRAGradients, BGRAGraphics;

type

  { TKaajProgressBar }

  TKaajProgressBar = class(TfpgProgressBar)
  private
    FAlpha: byte;
    FBGRA: TBGRABitmap;
    FRandSeed: integer;
    procedure SetAlpha(AValue: byte);
  protected
    procedure HandlePaint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Alpha: byte read FAlpha write SetAlpha default 255;
  end;

implementation

{ TKaajProgressBar }

procedure TKaajProgressBar.SetAlpha(AValue: byte);
begin
  if FAlpha = AValue then
    Exit;
  FAlpha := AValue;
end;

procedure TKaajProgressBar.HandlePaint;
var
  content: TRect;
  xpos, y, tx, ty: integer;
  grayValue: integer;

  function ApplyLightness(c: TBGRAPixel; lightness: word): TBGRAPixel;
  begin
    Result := GammaCompression(SetLightness(GammaExpansion(c), lightness));
  end;

  procedure DrawBar(bounds: TRect);
  var
    lCol: TBGRAPixel;
  begin
    lCol := BGRA(102, 163, 226);

    DoubleGradientAlphaFill(FBGRA, bounds,
      ApplyLightness(lCol, 37000), ApplyLightness(lCol, 29000),
      ApplyLightness(lCol, 26000), ApplyLightness(lCol, 18000),
      gdVertical, gdVertical, gdVertical, 0.53);

    InflateRect(bounds, -1, -1);

    DoubleGradientAlphaFill(FBGRA, bounds,
      ApplyLightness(lCol, 28000), ApplyLightness(lCol, 22000),
      ApplyLightness(lCol, 19000), ApplyLightness(lCol, 11000),
      gdVertical, gdVertical, gdVertical, 0.53);
  end;

begin
  tx := Width;
  ty := Height;
  if (Width <> FBGRA.Width) or (Height <> FBGRA.Height) then
    FBGRA.SetSize(Width, Height);

  FBGRA.Fill(BGRA(83, 83, 83));
  FBGRA.Rectangle(0, 0, tx, ty, BGRA(255, 255, 255, 6), dmDrawWithTransparency);
  if (tx > 2) and (ty > 2) then
    FBGRA.Rectangle(1, 1, tx - 1, ty - 1, BGRA(29, 29, 29), dmSet);

  if (tx > 4) and (ty > 4) then
  begin
    content := Rect(2, 2, tx - 2, ty - 2);
    randseed := FRandSeed;
    for y := content.Top to content.Bottom - 1 do
    begin
      if y = content.Top then
        grayValue := 33
      else
      if y = content.Top + 1 then
        grayValue := 43
      else
        grayValue := 47 + random(50 - 47 + 1);
      FBGRA.SetHorizLine(content.Left, y, content.Right - 1, BGRA(
        grayValue, grayValue, grayValue));
    end;
    if tx >= 6 then
      FBGRA.DrawVertLine(content.Right - 1, content.Top, content.Bottom - 1,
        BGRA(0, 0, 0, 32));
    if Max > Min then
    begin
      xpos := round((Position - Min) / (Max - Min) *
        (content.right - content.left)) + content.left;
      if xpos > content.left then
      begin
        DrawBar(rect(content.left, content.top, xpos, content.bottom));
        if xpos < content.right then
        begin
          FBGRA.SetPixel(xpos, content.top, BGRA(62, 62, 62));
          FBGRA.SetVertLine(xpos, content.top + 1, content.bottom - 1, BGRA(40, 40, 40));
        end;
      end;
    end;
  end;
  if Alpha <> 255 then
    FBGRA.ApplyGlobalOpacity(Alpha);
  FBGRA.Draw(Canvas, 0, 0, False);
end;

constructor TKaajProgressBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBGRA := TBGRABitmap.Create;
  randomize;
  FRandSeed := randseed;
  Alpha := 255;
end;

destructor TKaajProgressBar.Destroy;
begin
  if FBGRA <> nil then
    FreeAndNil(FBGRA);
  inherited Destroy;
end;

end.
