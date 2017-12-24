unit kaaj_panel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpg_base, fpg_main, fpg_panel, kaaj_theme,
  BGRABitmap, BGRABitmapTypes;

type

  TBGRARedrawEvent = procedure(Sender: TObject; Bitmap: TBGRABitmap) of object;

  { TKaajPanel }

  TKaajPanel = class(TfpgPanel)
  private
    FAlpha: byte;
    FBGRA: TBGRABitmap;
    FOnRedraw: TBGRARedrawEvent;
    procedure SetAlpha(AValue: byte);
  protected
    procedure HandlePaint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Alpha: byte read FAlpha write SetAlpha default 255;
    property OnRedraw: TBGRARedrawEvent Read FOnRedraw Write FOnRedraw;
    procedure DiscardBitmap;
  end;

implementation

{ TKaajPanel }

procedure TKaajPanel.SetAlpha(AValue: byte);
begin
  if FAlpha = AValue then
    Exit;
  FAlpha := AValue;
end;

procedure TKaajPanel.HandlePaint;
begin
  if (Width <> FBGRA.Width) or (Height <> FBGRA.Height) then
    FBGRA.SetSize(Width, Height);

  FBGRA.Rectangle(0, 0, Width, Height, BGRA(40, 40, 40), BGRA(83, 83, 83), dmSet);
  FBGRA.SetHorizLine(1, 1, Width - 2, BGRA(106, 106, 106));

  if Assigned(FOnRedraw) then
    FOnRedraw(self, FBGRA);

  if Alpha <> 255 then
    FBGRA.ApplyGlobalOpacity(Alpha);

  FBGRA.Draw(Canvas, 0, 0, False);
end;

constructor TKaajPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBGRA := TBGRABitmap.Create;
  Alpha := 255;
end;

destructor TKaajPanel.Destroy;
begin
  if FBGRA <> nil then
    FreeAndNil(FBGRA);
  inherited Destroy;
end;

procedure TKaajPanel.DiscardBitmap;
begin
  InvalidateRect(fpgRect(0, 0, Width, Height));
end;

end.
