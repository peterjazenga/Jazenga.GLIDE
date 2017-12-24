
{**********************************************************************
 Package pl_AGGPasVS.pkg
 This unit is part of CodeTyphon Studio (http://www.pilotlogic.com)
***********************************************************************}
unit agg_lclpaintbox;

interface

{$I agg_vsmode.inc }

uses
  LCLIntf, LMessages,

  {$IFDEF Windows}
  Windows,
  {$ELSE}
  LCLType, LCLProc, Types,
  {$ENDIF}

  Classes,
  Controls, Messages, Graphics, SysUtils,
  agg_basics, agg_color,
  agg_pixelmap, agg_platform_support,
  agg_pixfmt, agg_pixfmt_rgb,
  agg_render_scanlines, agg_renderer_base, agg_rendering_buffer,
  agg_rasterizer_scanline_aa, agg_renderer_scanline, agg_scanline_p,
  agg_ctrl, agg_slider_ctrl, agg_2D, agg_gsv_text, agg_conv_stroke;

type
  TAggPaintBoxBuffer = class(TInterfacedPersistent, IStreamPersist)
  private
    FPixelMap: pixel_map;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromFile(FileName: TFileName);
    procedure SaveToFile(FileName: TFileName);
  end;


  TAggCustomPaintBox = class(TCustomControl)
  private
    FAggEngine: Agg2D;
    FAggColor: aggclr;
    FBuffer: TAggPaintBoxBuffer;
    FPixelMap: pixel_map;
    FBufferValid: Boolean;
    FMouseInControl: Boolean;

    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    procedure CMColorChanged(var Message: TLMessage); message CM_COLORCHANGED;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Paint; override;
    procedure Resize; override;

    procedure DrawTo(aHandle: HDC; X, Y: Integer); overload;
    procedure DrawTo(HandleDC: HDC; DeviceRect: PRect = nil; BitmapRect: PRect = nil); overload;
    procedure Invalidate; override;
    procedure Loaded; override;
    procedure AssignTo(Dest: TPersistent); override;

    property  AggEngine: Agg2D read FAggEngine;
    property  Buffer: TAggPaintBoxBuffer read FBuffer;
  end;

TAggPaintBox = class(TAggCustomPaintBox)
  private
    FOnPaint: TNotifyEvent;
  protected
    procedure Paint; override;
  public
  published
    property Align;
    property Anchors;
    property Color;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
    property OnDockDrop;
    property OnDockOver;
    property OnUnDock;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

resourcestring
  RCStrStreamIsNotAValid = 'Stream is not a valid Bitmap';


function ColorToAggColor(WinColor: TColor): aggclr; overload;
begin
  if WinColor < 0 then WinColor := GetSysColor(WinColor and $000000FF);

  Result.R := WinColor and $FF;
  Result.G := (WinColor and $FF00) shr 8;
  Result.B := (WinColor and $FF0000) shr 16;
  Result.A := $FF;
end;


//=================== TAggPaintBoxBuffer ===================================

constructor TAggPaintBoxBuffer.Create;
begin
  inherited;
  FPixelMap.Construct;
end;

destructor TAggPaintBoxBuffer.Destroy;
begin
  FPixelMap.destroy;
  inherited;
end;

procedure TAggPaintBoxBuffer.LoadFromFile(FileName: TFileName);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadFromStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

type
  TPixelMapAccess = pixel_map;

procedure TAggPaintBoxBuffer.LoadFromStream(Stream: TStream);
var
  BitmapFileHeader: TBitmapFileHeader;
  NewBitmapInfo: PBitmapInfo;
  BitmapSize: Cardinal;
begin
  with Stream do
  begin
    Read(BitmapFileHeader, SizeOf(TBitmapFileHeader));
    if BitmapFileHeader.BfType <> $4D42 then
      raise Exception.Create(RCStrStreamIsNotAValid);

    BitmapSize := BitmapFileHeader.BfSize - SizeOf(TBitmapFileHeader);

    agg_getmem(Pointer(NewBitmapInfo), BitmapSize);

    try
      if Read(NewBitmapInfo^, BitmapSize) <> BitmapSize then
        raise Exception.Create(RCStrStreamIsNotAValid);

      TPixelMapAccess(FPixelMap).destroy;

      TPixelMapAccess(FPixelMap).m_bpp := NewBitmapInfo^.bmiHeader.BiBitCount;

      TPixelMapAccess(FPixelMap).create_from_bmp(NewBitmapInfo);
      TPixelMapAccess(FPixelMap).m_is_internal := True;
    except
      if NewBitmapInfo <> nil then
        agg_freemem(Pointer(NewBitmapInfo), BitmapSize);
    end;
  end;
end;

procedure TAggPaintBoxBuffer.SaveToFile(FileName: TFileName);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

procedure TAggPaintBoxBuffer.SaveToStream(Stream: TStream);
var
  BitmapFileHeader: TBitmapFileHeader;
begin
  with Stream do
  begin
    if TPixelMapAccess(FPixelMap).m_bmp <> nil then
    begin
      BitmapFileHeader.BfType := $4D42;
      BitmapFileHeader.BfOffBits := TPixelMapAccess(FPixelMap).calc_header_size(TPixelMapAccess(FPixelMap).m_bmp) +
        SizeOf(TBitmapFileHeader);
      BitmapFileHeader.BfSize := BitmapFileHeader.BfOffBits + TPixelMapAccess(FPixelMap).m_img_size;
      BitmapFileHeader.BfReserved1 := 0;
      BitmapFileHeader.BfReserved2 := 0;

      Write(BitmapFileHeader, SizeOf(BitmapFileHeader));
      Write(TPixelMapAccess(FPixelMap).m_bmp^, TPixelMapAccess(FPixelMap).m_full_size);
    end;
  end;
end;


//============== TAggCustomPaintBox =============================

constructor TAggCustomPaintBox.Create(AOwner: TComponent);
begin
  inherited;

  ControlStyle := ControlStyle + [csOpaque];
  DoubleBuffered := True;

  Width := 128;
  Height := 128;

  FBuffer := TAggPaintBoxBuffer.Create;
  FPixelMap := FBuffer.FPixelMap;
  FPixelMap.create(Width, Height, org_color32);

  FAggEngine.Construct;
  FAggEngine.Attach(FPixelMap._buf, FPixelMap._width, FPixelMap._height, -FPixelMap._stride);

  FAggColor := ColorToAggColor(Color);
  FAggEngine.ClearAll(FAggColor.r ,FAggColor.g ,FAggColor.b, FAggColor.a);
end;

destructor TAggCustomPaintBox.Destroy;
begin
  FAggEngine.Destruct;
  FBuffer.Free;

  inherited;
end;

procedure TAggCustomPaintBox.DrawTo(aHandle: HDC; X, Y: Integer);
begin
  FPixelMap.Draw(aHandle, X, Y);
end;

procedure TAggCustomPaintBox.DrawTo(HandleDC: HDC; DeviceRect,
  BitmapRect: PRect);
begin
  FPixelMap.Draw(Handle, DeviceRect, BitmapRect);
end;

procedure TAggCustomPaintBox.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);

  if Dest is TAggCustomPaintBox then
  begin
   // FPixelMap.Assign(TAggCustomPaintBox(Dest).FPixelMap);
    TAggCustomPaintBox(Dest).FBufferValid := FBufferValid;
    TAggCustomPaintBox(Dest).FOnMouseEnter := FOnMouseEnter;
    TAggCustomPaintBox(Dest).FOnMouseLeave := FOnMouseLeave;
  end;
end;

procedure TAggCustomPaintBox.CMColorChanged(var Message: TMessage);
begin
  FAggColor := ColorToAggColor(Color);
  FAggEngine.ClearAll(FAggColor.r ,FAggColor.g ,FAggColor.b, FAggColor.a);
end;

procedure TAggCustomPaintBox.Invalidate;
begin
  FBufferValid := False;
  inherited;
end;

procedure TAggCustomPaintBox.Loaded;
begin
  FBufferValid := False;
  inherited;
end;

procedure TAggCustomPaintBox.Paint;
begin
  if not Assigned(Parent) then
    Exit;

  FPixelMap.Draw(Canvas.Handle);
end;

procedure TAggCustomPaintBox.Resize;
begin

  with FPixelMap do
    if (Self.Width <> Width) or (Self.Height <> Height) then
    begin
      FPixelMap.create(Self.Width, Self.Height,org_color32);
      FAggEngine.Attach(FPixelMap._buf, _Width, _Height, -_Stride);
      FAggEngine.Viewport(0, 0, Width, Height, 0, 0, Width, Height, XMinYMin);
      FAggEngine.ClearAll(FAggColor.r ,FAggColor.g ,FAggColor.b, FAggColor.a);
    end;

  inherited;
end;


//============== TAggPaintBox ==========================================

procedure TAggPaintBox.Paint;
begin
  if Assigned(FOnPaint)
    then FOnPaint(Self);

  inherited;
end;

end.