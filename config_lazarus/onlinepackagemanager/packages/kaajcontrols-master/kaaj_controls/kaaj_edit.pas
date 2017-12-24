unit kaaj_edit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpg_base, fpg_main, fpg_edit, kaaj_theme,
  BGRABitmap, BGRABitmapTypes;

type

  { TKaajEdit }

  TKaajEdit = class(TfpgEdit)
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

{ TKaajEdit }

procedure TKaajEdit.SetAlpha(AValue: byte);
begin
  if FAlpha = AValue then
    Exit;
  FAlpha := AValue;
end;

procedure TKaajEdit.HandlePaint;
var
  r: TRect;
begin
  if (Width <> FBGRA.Width) or (Height <> FBGRA.Height) then
    FBGRA.SetSize(Width, Height);

  if not Focused then
    FBGRA.Rectangle(1, 1, Width - 1, Height - 1, BGRA(41, 41, 41),
      BGRA(58, 58, 58), dmSet)
  else
    FBGRA.Fill(BGRAWhite);

  if not Enabled then
    FBGRA.Rectangle(1, 1, Width - 1, Height - 1, BGRA(66, 66, 66),
      BGRA(71, 71, 71), dmSet);

  r := Rect(FVisSelStartPx, (Height - Canvas.Font.Height) div 2,
    FVisSelEndPx - FVisSelStartPx, FFont.Height);
  r.Right := r.Left + r.Right;
  r.Bottom := r.Top + r.Bottom;

  if (FSelOffset <> 0) and Focused then
    FBGRA.FillRect(r, BGRA(195, 195, 195), dmSet);

  if Alpha <> 255 then
    FBGRA.ApplyGlobalOpacity(Alpha);
  FBGRA.Draw(Canvas, 0, 0);

  if Enabled then
    Canvas.SetTextColor($FFE5E5E5)
  else
    Canvas.SetTextColor($FFAAAAAA);

  if Focused then
    Canvas.SetTextColor(clBlack);

  TKaajStyle(fpgStyle)._DrawEditString(Canvas, FSideMargin,
    (Height - Canvas.Font.Height) div 2, FVisibleText, Enabled);

  if Focused then
    fpgCaret.SetCaret(Canvas, FCursorPx, (Height - Canvas.Font.Height) div 2,
      fpgCaret.Width, FFont.Height)
  else
    fpgCaret.UnSetCaret(Canvas);

  FBGRA.Fill(BGRAPixelTransparent);

  if Enabled then
  begin
    if not Focused then
    begin
      FBGRA.Rectangle(0, 0, Width, Height, BGRA(83, 83, 83), dmSet);
      FBGRA.Rectangle(1, 1, Width - 1, Height - 1, BGRA(41, 41, 41), dmSet);
      FBGRA.SetHorizLine(1, Height - 1, Width - 2, BGRA(105, 105, 105));
    end
    else
    begin
      FBGRA.Rectangle(0, 0, Width, Height, BGRA(80, 111, 172), dmSet);
      FBGRA.Rectangle(1, 1, Width - 1, Height - 1, BGRA(41, 41, 41), dmSet);
    end;
  end
  else
  begin
    FBGRA.Rectangle(0, 0, Width, Height, BGRA(83, 83, 83), dmSet);
    FBGRA.Rectangle(1, 1, Width - 1, Height - 1, BGRA(66, 66, 66), dmSet);
    FBGRA.SetHorizLine(1, Height - 1, Width - 2, BGRA(94, 94, 94));
  end;

  if Alpha <> 255 then
    FBGRA.ApplyGlobalOpacity(Alpha);
  FBGRA.Draw(Canvas, 0, 0, False);
end;

constructor TKaajEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBGRA := TBGRABitmap.Create;
  SideMargin := 6;
  HeightMargin := 2;
  Alpha := 255;
end;

destructor TKaajEdit.Destroy;
begin
  if FBGRA <> nil then
    FreeAndNil(FBGRA);
  inherited Destroy;
end;

end.
