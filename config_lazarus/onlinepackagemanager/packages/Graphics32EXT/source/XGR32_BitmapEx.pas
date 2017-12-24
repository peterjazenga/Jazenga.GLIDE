
{**********************************************************************
 Package pl_Graphics32EXT.pkg
 this unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit XGR32_BitmapEx;


interface

uses

  LCLIntf, LCLType, LMessages,
  SysUtils, Classes
  , Graphics
  , GR32
  , XGR32_Graphics
  , XGR32_GraphUtils
  ;

type
  TBitmap32Ex = class(TBitmap32)
  private
    function GetFont: TFont32;
    procedure SetFont(Value: TFont32);
  public
    class function CreateFont: TFont; VIRTUAL;
    procedure DrawGraphic(Graphic: TGraphic; DstRect: TRect);
    procedure DrawText(Text: string; var aRect: TRect; aFormat: LongWord);
    procedure DrawTextW(Text: widestring; var aRect: TRect; aFormat: LongWord);
    procedure RenderText(X, Y: Integer; const Text: String); overload;
    procedure RenderTextW(X, Y: Integer; const Text: WideString); overload;
    property Font: TFont32 read GetFont write SetFont;
  end;
  

implementation

class function TBitmap32Ex.CreateFont: TFont;
begin
  Result := TFont32.Create;
end;

procedure TBitmap32Ex.DrawGraphic(Graphic: TGraphic; DstRect: TRect);
var
  LBitmap32: TBItmap32;
begin
  LBitmap32 := TBitmap32.Create;
  try
    LBitmap32.Assign(Graphic);
    LBitmap32.DrawTo(Self, DstRect);
  finally
    LBitmap32.Free;
  end;
end;

procedure TBitmap32Ex.DrawText(Text: string; var aRect: TRect; aFormat: LongWord);
begin
    Font.DrawText(Self, Text, aRect, aFormat);
end;

procedure TBitmap32Ex.DrawTextW(Text: widestring; var aRect: TRect; aFormat: LongWord);
begin
  Font.DrawText(Self, Text, aRect, aFormat);
end;

function TBitmap32Ex.GetFont: TFont32;
begin
  Result := TFont32(inherited Font);
end;

procedure TBitmap32Ex.RenderText(X, Y: Integer; const Text: String);
begin
  Font.RenderText(Self, X, Y, Text);
  {with Font do
    if Background.Enabled and not Background.Empty then
    begin
      vTexture := nil;
      vTextBMP := nil;
      try
        vTexture := TBitmap32.Create;
        vTextBMP := TBitmap32Ex.Create;
        PaddedText := Text + ' ';
        //if Assigned(TBitmap32Ex(vTextBMP).Font.Background) then
          vTextBMP.Font := Font;
        Sz := vTextBMP.TextExtent(PaddedText);
        vTextBMP.SetSize(Sz.cx, Sz.cy);
        vTexture.SetSize(Sz.cx, Sz.cy);
        vTexture.DrawMode := dmBlend;
        vTexture.CombineMode := cmMerge;
        Background.PaintTo(vTexture, vTexture.BoundsRect);
        vTextBMP.Clear(clBlack32);
        vColor.Color := clWhite32;
        //vColor.rgbAlpha := $FF;
        vTextBMP.RenderText(0, 0, Text, Integer(Quality), vColor.Color);
        if Outline then
        begin
          ConvolveI(LaplaceFilter3x3, vTextBMP);
        end;
        BlueChannelToAlpha(vTexture, vTextBMP);
        vTexture.DrawTo(Self, X, Y);
      finally
        FreeAndNil(vTexture);
        FreeAndNil(vTextBMP);
      end;
    end
    else begin
      vColor.Color := Color32(Color);
      vColor.rgbAlpha := Alpha;
      Self.RenderText(X, Y, Text, Integer(Quality), vColor.Color);
    end;
  //}
end;

procedure TBitmap32Ex.RenderTextW(X, Y: Integer; const Text: WideString);
begin
  Font.RenderTextW(Self, X, Y, Text);
  
  {with Font do
    if Background.Enabled and not Background.Empty then
    begin
      vTexture := nil;
      vTextBMP := nil;
      try
        vTexture := TBitmap32.Create;
        vTextBMP := TBitmap32Ex.Create;
        PaddedText := Text + ' ';
        //if Assigned(TBitmap32Ex(vTextBMP).Font.Background) then
          vTextBMP.Font := Font;
        Sz := vTextBMP.TextExtentW(PaddedText);
        vTextBMP.SetSize(Sz.cx, Sz.cy);
        vTexture.SetSize(Sz.cx, Sz.cy);
        vTexture.DrawMode := dmBlend;
        vTexture.CombineMode := cmMerge;
        Background.PaintTo(vTexture, vTexture.BoundsRect);
        vTextBMP.Clear(clBlack32);
        vColor.Color := clWhite32;
        //vColor.rgbAlpha := $FF;
        vTextBMP.RenderTextW(0, 0, Text, Integer(Quality), vColor.Color);
        if Outline then
        begin
          ConvolveI(LaplaceFilter3x3, vTextBMP);
        end;
        BlueChannelToAlpha(vTexture, vTextBMP);
        vTexture.DrawTo(Self, X, Y);
      finally
        FreeAndNil(vTexture);
        FreeAndNil(vTextBMP);
      end;
    end
    else begin
      vColor.Color := Color32(Color);
      vColor.rgbAlpha := Alpha;
      Self.RenderTextW(X, Y, Text, Integer(Quality), vColor.Color);
    end;
  //}
end;

procedure TBitmap32Ex.SetFont(Value: TFont32);
begin
  inherited Font.Assign(Value);
end;


end.
