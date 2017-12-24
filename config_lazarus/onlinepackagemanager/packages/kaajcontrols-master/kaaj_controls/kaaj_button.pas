unit kaaj_button;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpg_base, fpg_main, fpg_button, kaaj_theme,
  BGRABitmap, BGRABitmapTypes;

type

  { TKaajButton }

  TKaajButton = class(TfpgButton)
  private
    FAlpha: byte;
    FBGRA: TBGRABitmap;
    procedure SetAlpha(AValue: byte);
  protected
    procedure HandlePaint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Alpha: byte read FAlpha write SetAlpha default 255;
  end;

implementation

{ TKaajButton }

procedure TKaajButton.SetAlpha(AValue: byte);
begin
  if FAlpha = AValue then
    Exit;
  FAlpha := AValue;
end;

procedure TKaajButton.HandlePaint;
var
  ImageX, ImageY, TextX, TextY: TfpgCoord;
begin
  if (Width <> FBGRA.Width) or (Height <> FBGRA.Height) then
    FBGRA.SetSize(Width, Height);

  if Enabled then
  begin
    if FDown then
    begin
      { Button Down }
      FBGRA.Rectangle(0, 0, Width, Height - 1, BGRA(48, 48, 48),
        BGRA(61, 61, 61), dmSet);
      FBGRA.Rectangle(1, 1, Width - 1, Height - 2, BGRA(55, 55, 55),
        BGRA(61, 61, 61), dmSet);
      FBGRA.SetHorizLine(0, Height - 1, Width - 1, BGRA(115, 115, 115));
    end
    else
    begin
      if FState = 0 then
      begin
        { Button Normal }
        FBGRA.GradientFill(0, 0, Width, Height, BGRA(107, 107, 107),
          BGRA(84, 84, 84), gtLinear, PointF(0, 0), PointF(0, Height), dmSet);
        FBGRA.Rectangle(0, 0, Width, Height - 1, BGRA(48, 48, 48), dmSet);
        FBGRA.SetHorizLine(1, 1, Width - 2, BGRA(130, 130, 130));
        FBGRA.SetHorizLine(0, Height - 1, Width - 1, BGRA(115, 115, 115));
        { Button Focused }
        if FFocused then
        begin
          FBGRA.Rectangle(1, 2, Width - 1, Height - 2, BGRA(80, 111, 172), dmSet);
        end;
      end
      else
      begin
        { Button Hovered }
        FBGRA.GradientFill(0, 0, Width, Height, BGRA(132, 132, 132),
          BGRA(109, 109, 109), gtLinear, PointF(0, 0), PointF(0, Height), dmSet);
        FBGRA.Rectangle(0, 0, Width, Height - 1, BGRA(48, 48, 48), dmSet);
        FBGRA.SetHorizLine(1, 1, Width - 2, BGRA(160, 160, 160));
        FBGRA.SetHorizLine(0, Height - 1, Width - 1, BGRA(115, 115, 115));
      end;
    end;
  end
  else
  begin
    { Button Disabled }
    FBGRA.Rectangle(0, 0, Width, Height - 1, BGRA(48, 48, 48),
      BGRA(61, 61, 61), dmSet);
    FBGRA.SetHorizLine(0, Height - 1, Width - 1, BGRA(115, 115, 115));
  end;

  if Alpha <> 255 then
    FBGRA.ApplyGlobalOpacity(Alpha);
  FBGRA.Draw(Canvas, 0, 0, False);

  ImageX := 0;
  ImageY := 0;
  TextX := 0;
  TextY := 0;

  CalculatePositions(ImageX, ImageY, TextX, TextY);

  TKaajStyle(fpgStyle)._DrawButtonString(Canvas, TextX, TextY, Text, Enabled);
end;

constructor TKaajButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBGRA := TBGRABitmap.Create;
  Alpha := 255;
end;

destructor TKaajButton.Destroy;
begin
  if FBGRA <> nil then
    FreeAndNil(FBGRA);
  inherited Destroy;
end;

end.
