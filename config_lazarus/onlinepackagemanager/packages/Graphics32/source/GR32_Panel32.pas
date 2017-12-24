{**********************************************************************
                PilotLogic Software House.

 Package pl_Graphics32.pkg
 This unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit GR32_Panel32;

interface
uses
  LCLIntf, LCLType, LMessages,
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  GR32,GR32_Image,GR32_LowLevel;
type

 TGR32Panel = class(TCustomImage32)
  private
    FChange: integer;
    FFillColor: TColor32;
    FFillCubeSize: integer;
    
    FLineEnable:Boolean;
    FLineColor:TColor32;
    procedure SetFillColor(const Value: TColor32);
    procedure SetFillCubeSize(const Value: integer);
    procedure SetLineEnable(const Value: Boolean);
    procedure SetLineColor(const Value: TColor32);
  protected

    procedure xPaintStage(Sender: TObject; aBuffer: TBitmap32; StageNum: Cardinal);
    procedure Paint; override;
    procedure BitmapPaint;
    procedure BitmapResize;
  public
    constructor Create(AOwner : TComponent); override;
    procedure Resize; override;
  published
    property FillColor : TColor32 read FFillColor write SetFillColor;
    property FillCubeSize : integer read FFillCubeSize write SetFillCubeSize;
    property LineEnable : Boolean read FLineEnable write SetLineEnable;
    property LineColor : TColor32 read FLineColor write SetLineColor;
    property Enabled;
    property Cursor;
    property ShowHint;
    property Align;
    property Anchors;
    property AutoSize;
    property Bitmap;
    property BitmapAlign;
    property Color;
    property Constraints;
    property DragCursor;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property RepaintMode;
    property Scale;
    property ScaleMode;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnBitmapResize;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnGDIOverlay;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnInitStages;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnPaintStage;
    property OnResize;
    property OnStartDrag;
  end;


implementation

constructor TGR32Panel.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);

 with PaintStages[0]^ do
  begin
    if Stage = PST_CLEAR_BACKGND then Stage := PST_CUSTOM;
  end;

   FChange := 0;
   bitmap.DrawMode:=dmBlend;
   FLineEnable:=True;
   FFillColor := clTrWhite32;
   FFillCubeSize:=3;
   FLineColor:=clRed32;
   Enabled := true;
   OnPaintStage:=@xPaintStage;
end;

procedure TGR32Panel.Paint;
 begin
  BitmapResize;
  inherited Paint;  
 end;

procedure TGR32Panel.Resize;
 begin
   inherited Resize;
   BitmapResize;
 end;

procedure TGR32Panel.BitmapPaint;
 begin
  bitmap.Clear(FFillColor);
  if FLineEnable=false then exit;

  bitmap.FrameRectS(rect(0,0,bitmap.Width-1,bitmap.Height-1),FLineColor);
  bitmap.LineTS(bitmap.Width-1,1,bitmap.Width-1,bitmap.Height,clGray32,true);
  bitmap.LineTS(1,bitmap.Height-1,bitmap.Width-1,bitmap.Height-1,clGray32,true);
 end;

procedure TGR32Panel.BitmapResize;
 begin
  if bitmap=nil then exit;

  if (Width<>bitmap.Width) or (Height<>bitmap.Height) then
    begin
     bitmap.SetSize(Width,Height);
     BitmapPaint;
    end;
 end;

procedure TGR32Panel.SetFillColor(const Value: TColor32);
begin
  if (Value=FFillColor) then exit;
  FFillColor := Value;
  BitmapPaint;
end;

procedure TGR32Panel.SetFillCubeSize(const Value: integer);
begin
  if (Value<=0) or (Value>10) then exit;
  if (Value=FFillCubeSize) then exit;

  FFillCubeSize := Value;
  BitmapPaint;
end;

procedure TGR32Panel.SetLineEnable(const Value: Boolean);
begin
  if (Value=FLineEnable) then exit;
  FLineEnable := Value;
  BitmapPaint;
end;

procedure TGR32Panel.SetLineColor(const Value: TColor32);
begin   
  if (Value=FLineColor) then exit;
  FLineColor := Value;
  BitmapPaint;
end;

procedure TGR32Panel.xPaintStage(Sender: TObject; aBuffer: TBitmap32; StageNum: Cardinal);
const
  Colors: array [0..1] of TColor32 = ($FFFFFFFF, $FFB0B0B0);
var
  W, I, J, Parity: Integer;
  Line1, Line2: TArrayOfColor32; // a buffer for a couple of scanlines
begin
  with aBuffer do
  begin
    W := Width;
    SetLength(Line1, W);
    SetLength(Line2, W);
    for I := 0 to W - 1 do
    begin
      Parity := I shr FFillCubeSize and $1;
      Line1[I] := Colors[Parity];
      Line2[I] := Colors[1 - Parity];
    end;
    for J := 0 to Height - 1 do
    begin
      Parity := J shr FFillCubeSize and $1;
      if Boolean(Parity) then MoveLongword(Line1[0], ScanLine[J]^, W)
      else MoveLongword(Line2[0], ScanLine[J]^, W);
    end;
  end;
end;



end.
