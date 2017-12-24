{**********************************************************************
 Package pl_Graphics32.pkg
 this file is part of
 CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit GR32_Backends_LCL_Qt;

interface

{$I GR32.inc}

uses
  LCLIntf, LCLType, types, Controls, SysUtils, Classes,
  
  Qt4, QtObjects,

  GraphType, FPImage, IntfGraphics, LCLProc,

  Graphics, GR32, GR32_Backends, GR32_Containers, GR32_Image;

type

  { TLCLBackend }

  TLCLBackend = class(
    TCustomBackend,
    IPaintSupport,
    ITextSupport,
    IFontSupport,
    IDeviceContextSupport,
    ICanvasSupport
  )
  private
    FFont: TFont;
    FCanvas: TCanvas;
    FCanvasHandle: HDC;
    FOnFontChange: TNotifyEvent;
    FOnCanvasChange: TNotifyEvent;

    FWidth, FHeight: Cardinal;

    FRawImage: TRawImage;
    FBitmap: TBitmap;

    procedure CanvasChangedHandler(Sender: TObject);
  protected
    { BITS_GETTER }
    function GetBits: PColor32Array;

    procedure InitializeSurface(NewWidth, NewHeight: Integer; ClearBuffer: Boolean); override;
    procedure FinalizeSurface; override;

    procedure WidgetSetStretchDrawRGB32Bitmap(Dest: HDC; DstX, DstY, DstWidth,
                                              DstHeight: Integer; SrcX, SrcY, SrcWidth, SrcHeight: Integer;
                                              SrcBitmap: TBitmap32);

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Changed; override;

    function Empty: Boolean; override;
  public
    procedure SwapRedBlue;
    { IPaintSupport }
    procedure DoPaint(ABuffer: TBitmap32; AInvalidRects: TRectList; ACanvas: TCanvas; APaintBox: TCustomPaintBox32);
    procedure ImageNeeded;
    procedure CheckPixmap;

    { IDeviceContextSupport }
    function GetHandle: HDC;
    procedure Draw(const DstRect, SrcRect: TRect; hSrc: HDC); overload;
    procedure DrawTo(hDst: HDC; DstX, DstY: Integer); overload;
    procedure DrawTo(hDst: HDC; const DstRect, SrcRect: TRect); overload;
    property Handle: HDC read GetHandle;

    { ITextSupport }
    procedure Textout(X, Y: Integer; const Text: string); overload;
    procedure Textout(X, Y: Integer; const ClipRect: TRect; const Text: string); overload;
    procedure Textout(var DstRect: TRect; const Flags: Cardinal; const Text: string); overload;
    function  TextExtent(const Text: string): TSize;

    procedure TextoutW(X, Y: Integer; const Text: Widestring); overload;
    procedure TextoutW(X, Y: Integer; const ClipRect: TRect; const Text: Widestring); overload;
    procedure TextoutW(var DstRect: TRect; const Flags: Cardinal; const Text: Widestring); overload;
    function  TextExtentW(const Text: Widestring): TSize;

    { IFontSupport }
    function GetOnFontChange: TNotifyEvent;
    procedure SetOnFontChange(Handler: TNotifyEvent);
    function GetFont: TFont;
    procedure SetFont(const Font: TFont);

    procedure UpdateFont;
    property Font: TFont read GetFont write SetFont;
    property OnFontChange: TNotifyEvent read FOnFontChange write FOnFontChange;

    { ICanvasSupport }
    function GetCanvasChange: TNotifyEvent;
    procedure SetCanvasChange(Handler: TNotifyEvent);
    function GetCanvas: TCanvas;

    procedure DeleteCanvas;
    function CanvasAllocated: Boolean;

    property Canvas: TCanvas read GetCanvas;
    property OnCanvasChange: TNotifyEvent read GetCanvasChange write SetCanvasChange;
  end;

implementation

uses
  GR32_LowLevel;

{ TLCLBackend }

constructor TLCLBackend.Create;
begin
  inherited;
  FBitmap := TBitmap.Create;
  FBitmap.Canvas.OnChange := CanvasChangedHandler;
  FFont := TFont.Create;
end;

destructor TLCLBackend.Destroy;
begin
  inherited;
  FFont.Free;
  FBitmap.Free;
end;

procedure TLCLBackend.CanvasChangedHandler(Sender: TObject);
begin
  if Assigned(FOnCanvasChange) then
    FOnCanvasChange(Sender);
end;

function TLCLBackend.GetBits: PColor32Array;
begin
  Result := FBits;
end;

procedure TLCLBackend.InitializeSurface(NewWidth, NewHeight: Integer; ClearBuffer: Boolean);
var
  CDBitmap: TBitmap;
  LazImage: TLazIntfImage;
begin
  { We allocate our own memory for the image }

  FRawImage.Description.Init_BPP32_B8G8R8A8_BIO_TTB(NewWidth, NewHeight);

  FRawImage.CreateData(ClearBuffer);
  FBits := PColor32Array(FRawImage.Data);

  if FBits = nil then
    raise Exception.Create('[TLCLBackend.InitializeSurface] ERROR FBits = nil');

  LazImage := TLazIntfImage.Create(FRawImage, False);

  if FBitmap=nil then
   begin
     FBitmap := TBitmap.Create;
   end;

  FBitmap.LoadFromIntfImage(LazImage);

  FWidth := NewWidth;
  FHeight := NewHeight;

end;

procedure TLCLBackend.FinalizeSurface;
begin
  if Assigned(FBits) then
  begin
    FRawImage.FreeData;
    FBits := nil;

    FBitmap.Handle := HBITMAP(0);
  end;
  FBits := nil;
end;

procedure TLCLBackend.Changed;
begin
  inherited;
end;

function TLCLBackend.Empty: Boolean;
begin
  Result := FBits = nil;
end;

{ IPaintSupport }

procedure TLCLBackend.ImageNeeded;
begin

end;

procedure TLCLBackend.CheckPixmap;
begin

end;

procedure TLCLBackend.SwapRedBlue;
var
  n:    integer;
  temp: longword;
  p:    PLongword;
begin

  p := PLongword(FBits);
  n := FWidth*FHeight;
  if n = 0 then
    exit;
  repeat
    temp := LEtoN(p^);
    p^   := NtoLE(((temp and $FF) shl 16) or ((temp and $FF0000) shr 16) or temp and $FF00FF00);
    Inc(p);
    Dec(n);
  until n = 0;
end;

procedure TLCLBackend.WidgetSetStretchDrawRGB32Bitmap(Dest: HDC;
                                                      DstX, DstY, DstWidth, DstHeight: Integer;
                                                      SrcX, SrcY, SrcWidth, SrcHeight: Integer;
                                                      SrcBitmap: TBitmap32);
var
  DstQDC: TQtDeviceContext absolute Dest;
  SrcRect, DstRect: TRect;
  Image: TQtImage;
begin
  DstRect := Bounds(DstX, DstY, DstWidth, DstHeight);
  SrcRect := Bounds(SrcX, SrcY, SrcWidth, SrcHeight);

  Image := TQtImage.Create(pbyte(SrcBitmap.Bits), SrcBitmap.Width, SrcBitmap.Height, QImageFormat_RGB32);
  try
    QPainter_drawImage(DstQDC.Widget, PRect(@DstRect), Image.Handle, @SrcRect, QtAutoColor);
  finally
    Image.Free;
  end;
end;

procedure TLCLBackend.DoPaint(ABuffer: TBitmap32; AInvalidRects: TRectList; ACanvas: TCanvas; APaintBox: TCustomPaintBox32);
begin

  WidgetSetStretchDrawRGB32Bitmap(ACanvas.Handle,
                                  0,0,ABuffer.Width, ABuffer.Height,
                                  0,0,ABuffer.Width, ABuffer.Height,
                                  ABuffer);

end;


{ IDeviceContextSupport }

function TLCLBackend.GetHandle: HDC;
begin
  Result := Canvas.Handle;
end;

procedure TLCLBackend.Draw(const DstRect, SrcRect: TRect; hSrc: HDC);
var
  SrcQDC: TQtDeviceContext absolute hSrc;
  bmp: TBitmap;
  Ofs: TPoint;
  dcSource, dcDest: TQtDeviceContext;
  B: Boolean;
begin

  bmp    := TBitmap.Create;
  bmp.PixelFormat := pf32bit;
  bmp.Width := Canvas.Width;
  bmp.Height := Canvas.Height;

  dcDest := TQtDeviceContext(bmp.Canvas.handle);

  dcSource := TQtDeviceContext(SrcQDC.Widget);

  if (dcSource.vImage <> nil) and (dcSource.vImage.Handle <> nil) then
  begin

    B := QPainter_isActive(dcDest.Widget);
    if B then
      QPainter_end(dcDest.Widget);
    TQtImage(bmp.Handle).CopyFrom(dcSource.vImage.Handle,
                                  SrcRect.Left,
                                  SrcRect.Top,
                                  SrcRect.Right - SrcRect.Left,
                                  SrcRect.Bottom - SrcRect.Top);
    if B then
      QPainter_begin(dcDest.Widget, TQtImage(bmp.Handle).Handle);
  end;

  Canvas.StretchDraw(DstRect,bmp) ;

  bmp.Free;

end;

{
procedure TLCLBackend.Draw(const DstRect, SrcRect: TRect; hSrc: HDC);

begin
  StretchMaskBlt(
    Canvas.Handle,
    DstRect.Left,
    DstRect.Top,
    DstRect.Right - DstRect.Left,
    DstRect.Bottom - DstRect.Top,
    hSrc,
    SrcRect.Left,
    SrcRect.Top,
    SrcRect.Right - SrcRect.Left,
    SrcRect.Bottom - SrcRect.Top,
    0,
    0,
    0,
    Canvas.CopyMode
  );
end;
}

procedure TLCLBackend.DrawTo(hDst: HDC; DstX, DstY: Integer);
var
  DstQDC: TQtDeviceContext absolute hDst;
  SrcRect, DstRect: TRect;
  Image: TQtImage;
begin
  DstRect := Bounds(DstX, DstY, fWidth, fHeight);
  SrcRect := Bounds(0   , 0   , fWidth, fHeight);

  Image := TQtImage.Create(pbyte(Bits), SrcRect.Right - SrcRect.Left, SrcRect.Bottom - SrcRect.Top , QImageFormat_RGB32);
  try
    QPainter_drawImage(DstQDC.Widget, PRect(@DstRect), Image.Handle, @SrcRect, QtAutoColor);
  finally
    Image.Free;
  end;

{
  StretchMaskBlt(
    hDst,
    DstX,
    DstY,
    FWidth,
    FHeight,
    Canvas.Handle,
    0,
    0,
    FWidth,
    FHeight,
    0,
    0,
    0,
    Canvas.CopyMode
  );   }
end;

procedure TLCLBackend.DrawTo(hDst: HDC; const DstRect, SrcRect: TRect);
var
  DstQDC: TQtDeviceContext absolute hDst;
  Image: TQtImage;
begin

  Image := TQtImage.Create(pbyte(Bits), SrcRect.Right - SrcRect.Left, SrcRect.Bottom - SrcRect.Top , QImageFormat_RGB32);
  try
    QPainter_drawImage(DstQDC.Widget, PRect(@DstRect), Image.Handle, @SrcRect, QtAutoColor);
  finally
    Image.Free;
  end;

  {
  StretchMaskBlt(
    hDst,
    DstRect.Left,
    DstRect.Top,
    DstRect.Right - DstRect.Left,
    DstRect.Bottom - DstRect.Top,
    Canvas.Handle,
    SrcRect.Left,
    SrcRect.Top,
    SrcRect.Right - SrcRect.Left,
    SrcRect.Bottom - SrcRect.Top,
    0,
    0,
    0,
    Canvas.CopyMode
  );    }
end;

{ ITextSupport }

procedure TLCLBackend.Textout(X, Y: Integer; const Text: string);
begin
  if not Assigned(FCanvas) then GetCanvas;

  UpdateFont;

  if not FOwner.MeasuringMode then
    FCanvas.TextOut(X, Y, Text);

//  FOwner.Changed(DstRect);
end;

procedure TLCLBackend.Textout(X, Y: Integer; const ClipRect: TRect; const Text: string);
begin
  if not Assigned(FCanvas) then GetCanvas;

  UpdateFont;

  LCLIntf.ExtTextOut(FCanvas.Handle, X, Y, ETO_CLIPPED, @ClipRect, PChar(Text), Length(Text), nil);
end;

procedure TLCLBackend.Textout(var DstRect: TRect; const Flags: Cardinal; const Text: string);
begin
  UpdateFont;

  LCLIntf.DrawText(FCanvas.Handle, PChar(Text), Length(Text), DstRect, Flags);
end;

function TLCLBackend.TextExtent(const Text: string): TSize;
begin
  if not Assigned(FCanvas) then GetCanvas;

  UpdateFont;

  Result := FCanvas.TextExtent(Text);
end;

procedure TLCLBackend.TextoutW(X, Y: Integer; const Text: Widestring);
begin
  Canvas.TextOut(X, Y, Text);
end;

procedure TLCLBackend.TextoutW(X, Y: Integer; const ClipRect: TRect; const Text: Widestring);
begin
  Canvas.ClipRect := ClipRect;;
  Canvas.TextOut(X, Y, Text);
end;

procedure TLCLBackend.TextoutW(var DstRect: TRect; const Flags: Cardinal; const Text: Widestring);
begin
  TextOut(DstRect, Flags, Text);
end;

function TLCLBackend.TextExtentW(const Text: Widestring): TSize;
begin
  Result := TextExtent(Text);
end;

{ IFontSupport }

function TLCLBackend.GetOnFontChange: TNotifyEvent;
begin
  Result := FFont.OnChange;
end;

procedure TLCLBackend.SetOnFontChange(Handler: TNotifyEvent);
begin
  FFont.OnChange := Handler;
end;

function TLCLBackend.GetFont: TFont;
begin
  Result := FFont;
end;

procedure TLCLBackend.SetFont(const Font: TFont);
begin
  FFont.Assign(Font);
end;

procedure TLCLBackend.UpdateFont;
begin
  FFont.OnChange := FOnFontChange;

  if Assigned(FCanvas) then FCanvas.Font := FFont;
end;

{ ICanvasSupport }

function TLCLBackend.GetCanvasChange: TNotifyEvent;
begin
  Result := FOnCanvasChange;
end;

procedure TLCLBackend.SetCanvasChange(Handler: TNotifyEvent);
begin
  FOnCanvasChange := Handler;
end;

function TLCLBackend.GetCanvas: TCanvas;
begin
  Result := FBitmap.Canvas;
end;

procedure TLCLBackend.DeleteCanvas;
begin
end;

function TLCLBackend.CanvasAllocated: Boolean;
begin
  Result := (Canvas <> nil);
end;

end.
