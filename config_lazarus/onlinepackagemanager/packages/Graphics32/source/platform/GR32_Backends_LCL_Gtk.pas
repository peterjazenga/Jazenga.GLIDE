{**********************************************************************
 Package pl_Graphics32.pkg
 this file is part of
 CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit GR32_Backends_LCL_Gtk;

interface

{$I GR32.inc}

uses
  LCLIntf, LCLType, types, Controls, SysUtils, Classes,
{$IFDEF LCLGtk2}
  gdk2, gtk2def, gdk2pixbuf, glib2,
{$ELSE}
  gdk, gtkdef, gtkProc, gdkpixbuf, glib,
{$ENDIF}

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

    procedure GetImageFromHDC(hSrc: HDC;
                              src_x, src_y:longint;
                              dest_x, dest_y, awidth, aheight:longint);

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
  P: TPoint;
begin
  P := TGtkDeviceContext(Dest).Offset;

  Inc(DstX, P.X);
  Inc(DstY, P.Y);

  gdk_draw_rgb_32_image(
                        TGtkDeviceContext(Dest).Drawable,
                        TGtkDeviceContext(Dest).GC,
                        DstX, DstY,
                        SrcWidth, SrcHeight,
                        GDK_RGB_DITHER_NONE,
                        pguchar(SrcBitmap.Bits),
                        SrcBitmap.Width* 4
                        );
end;

procedure TLCLBackend.DoPaint(ABuffer: TBitmap32; AInvalidRects: TRectList; ACanvas: TCanvas; APaintBox: TCustomPaintBox32);
begin

 SwapRedBlue;

  WidgetSetStretchDrawRGB32Bitmap(ACanvas.Handle,
                                  0,0,ABuffer.Width, ABuffer.Height,
                                  0,0,ABuffer.Width, ABuffer.Height,
                                  ABuffer);


 SwapRedBlue;

end;


{ IDeviceContextSupport }

function TLCLBackend.GetHandle: HDC;
begin
  Result := Canvas.Handle;
end;

procedure TLCLBackend.DrawTo(hDst: HDC; DstX, DstY: Integer);
var
  P: TPoint;
begin
  P := TGtkDeviceContext(hDst).Offset;

  Inc(DstX, P.X);
  Inc(DstY, P.Y);

  SwapRedBlue;

  gdk_draw_rgb_32_image(
                        TGtkDeviceContext(hDst).Drawable,
                        TGtkDeviceContext(hDst).GC,
                        DstX, DstY,
                        FWidth, FHeight,
                        GDK_RGB_DITHER_NONE,
                        pguchar(FRawImage.Data),
                        FWidth* 4
                        );

  SwapRedBlue;
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
  );

BitBlt(
    hDst,
    DstX,
    DstY,
    FWidth,
    FHeight,

    Canvas.Handle,
    0,
    0,
    Canvas.CopyMode
  );
     }
end;

procedure TLCLBackend.DrawTo(hDst: HDC; const DstRect, SrcRect: TRect);
var
  P: TPoint;
  DR:TRect;
begin

  P := TGtkDeviceContext(hDst).Offset;

  DR:=DstRect;

  Inc(DR.Left , P.X);
  Inc(DR.Right, P.X);

  SwapRedBlue;

  gdk_draw_rgb_32_image(
                        TGtkDeviceContext(hDst).Drawable,
                        TGtkDeviceContext(hDst).GC,
                        DR.Left, DR.Top,
                        SrcRect.Right  - SrcRect.Left,
                        SrcRect.Bottom - SrcRect.Top,
                        GDK_RGB_DITHER_NONE,
                        pguchar(FRawImage.Data),
                        FWidth* 4
                        );

  SwapRedBlue;

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
  );
        }
   {
  BitBlt(
    hDst,
    DstRect.Left,
    DstRect.Top,
    DstRect.Right - DstRect.Left,
    DstRect.Bottom - DstRect.Top,

    Canvas.Handle,
    SrcRect.Left,
    SrcRect.Top,
    Canvas.CopyMode
  );
   }
end;



procedure TLCLBackend.GetImageFromHDC(hSrc: HDC;
                                      src_x, src_y:longint;
                                      dest_x, dest_y, awidth, aheight:longint);
var
  P: TPoint;
begin


  P := Point(src_x,src_y);
  DpToLP(hSrc, P, 1);

  gdk_pixbuf_get_from_drawable(
                               TGtkDeviceContext(Canvas.Handle).GC,
                               TGtkDeviceContext(hSrc).Drawable,
                               nil,
                               src_x, src_y,
                               dest_x, dest_y, awidth, aheight);
  SwapRedBlue;

end;


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

 {
   BitBlt(
    Canvas.Handle,
    DstRect.Left,
    DstRect.Top,
    DstRect.Right - DstRect.Left,
    DstRect.Bottom - DstRect.Top,

    hSrc,
    SrcRect.Left,
    SrcRect.Top,
    Canvas.CopyMode );
     }
  {
  GetImageFromHDC(
                  hSrc,
                  SrcRect.Left,
                  SrcRect.Top,
                  DstRect.Left,
                  DstRect.Top,
                  DstRect.Right - DstRect.Left,
                  DstRect.Bottom - DstRect.Top
                  );
                 }

end;

{ ITextSupport }

procedure TLCLBackend.Textout(X, Y: Integer; const Text: string);    // ct9999
begin
  if not Assigned(FCanvas) then GetCanvas;

  UpdateFont;

  if FCanvas=nil then exit;

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

function TLCLBackend.TextExtent(const Text: string): TSize;   // ct9999
begin
  result.cx:=0;
  result.cx:=0;

  if not Assigned(FCanvas) then GetCanvas;

  UpdateFont;

  if FCanvas=nil then exit;

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
