unit kaaj_combobox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpg_base, fpg_main, fpg_combobox, kaaj_theme,
  BGRABitmap, BGRABitmapTypes;

type

  { TKaajComboBox }

  TKaajComboBox = class(TfpgComboBox)
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

{ TKaajComboBox }

procedure TKaajComboBox.SetAlpha(AValue: byte);
begin
  if FAlpha = AValue then
    Exit;
  FAlpha := AValue;
end;

procedure TKaajComboBox.HandlePaint;
begin
  if (Width <> FBGRA.Width) or (Height <> FBGRA.Height) then
    FBGRA.SetSize(Width, Height);

  FBGRA.Fill(BGRA(83, 83, 83));

  if Enabled then
  begin
    if not FBtnPressed then
    begin
      FBGRA.GradientFill(2, 2, Width - 2, Height - 3, BGRA(117, 117, 117),
        BGRA(98, 98, 98), gtLinear, PointF(0, 0), PointF(0, Height), dmSet);
      FBGRA.Rectangle(1, 1, Width - 1, Height - 2, BGRA(39, 39, 39), dmSet);
      FBGRA.SetHorizLine(2, 2, Width - 3, BGRA(145, 145, 145));
      FBGRA.SetHorizLine(0, Height - 2, Width - 2, BGRA(106, 106, 106));
    end
    else
    begin
      FBGRA.GradientFill(2, 2, Width - 2, Height - 3, BGRA(63, 63, 63),
        BGRA(55, 55, 55), gtLinear, PointF(0, 0), PointF(0, Height), dmSet);
      FBGRA.Rectangle(1, 1, Width - 1, Height - 2, BGRA(39, 39, 39), dmSet);
    end;
  end
  else
  begin
    FBGRA.GradientFill(2, 2, Width - 2, Height - 3, BGRA(100, 100, 100),
      BGRA(91, 91, 91), gtLinear, PointF(0, 0), PointF(0, Height), dmSet);
    FBGRA.Rectangle(1, 1, Width - 1, Height - 2, BGRA(61, 61, 61), dmSet);
    FBGRA.SetHorizLine(2, 2, Width - 3, BGRA(116, 116, 116));
    FBGRA.SetHorizLine(0, Height - 2, Width - 2, BGRA(94, 94, 94));
  end;

  if Focused then
  begin
    FBGRA.Rectangle(0, 0, Width, Height - 1, BGRA(80, 111, 172), dmSet);
  end;

  if Alpha <> 255 then
    FBGRA.ApplyGlobalOpacity(Alpha);
  FBGRA.Draw(Canvas, 0, 0, False);

  if Enabled then
  begin
    Canvas.SetColor($FFE5E5E5);
    Canvas.SetTextColor($FFE5E5E5);
  end
  else
  begin
    Canvas.SetColor($FFAAAAAA);
    Canvas.SetTextColor($FFAAAAAA);
  end;

  fpgStyle.DrawDirectionArrow(Canvas, Width - 8 - 6, (Height - 6) div 2, 8, 6, adDown);

  TKaajStyle(fpgStyle)._DrawEditString(Canvas, Margin,
    (Height - Canvas.Font.Height) div 2, Text, Enabled);
end;

constructor TKaajComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBGRA := TBGRABitmap.Create;
  Margin := 6;
  Alpha := 255;
end;

destructor TKaajComboBox.Destroy;
begin
  if FBGRA <> nil then
    FreeAndNil(FBGRA);
  inherited Destroy;
end;

end.
