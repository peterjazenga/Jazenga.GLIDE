{**********************************************************************
 Package pl_OpenGL.pkg
 This unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit OpenGLCanvas;

{$MODE objfpc}{$H+}

interface

uses
  Classes, Controls, Graphics, LCLType, GraphType, SysUtils, Types,
  fgl,
  GL, GLU, GLext,
  OpenGLPanel;

const
  DefaultTransparentColor = clFuchsia;

type

TOGLCFloat = Single;

TOpenGLCanvas = class(TCustomControl)
  private
    FOldCanvas: TCanvas;
    FControl: TControl;
  private
    procedure DoOnPaint(ASender: TObject);
  protected
    procedure PaintWindow(DC: HDC); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property BorderSpacing;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property Constraints;
    property Cursor;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property Color;
    property Height;
    property Hint;
    property Left;
    property ParentFont;
    property ParentShowHint;
    property ParentBidiMode;
    property ParentColor;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Tag;
    property Top;
    property Visible;
    property Width;
    property UseDockManager default True;

    property OnClick;
    property OnContextPopup;
    property OnChangeBounds;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnGetDockCaption;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnPaint;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    property OnUTF8KeyPress;
  end;

  TOGLCanvas = class(TCanvas)
    protected
      procedure DoMoveTo(X, Y: Integer); override;
      procedure DoLineTo(X, Y: Integer); override;
      procedure DoLine(X1, Y1, X2, Y2: Integer); override;
      procedure DrawAsTexture(const ARect: TRect; const ABitmap: TBitmap);
      procedure SetPixel(X, Y: Integer; AColor: TColor); override;
    public
      function  TextExtent(const Text: String): TSize; override;
      procedure Draw(X, Y: Integer; AGraphic: TGraphic); override;
      procedure Ellipse(X1, Y1, X2, Y2: Integer); override;
      procedure FillRect(const ARect: TRect); override;
      procedure Polyline(APoints: PPoint; ACount: Integer); override;
      procedure Polygon(APoints: PPoint; ACount: Integer; AWinding: Boolean = False); override;
      procedure Rectangle(X1, Y1, X2, Y2: Integer); override;
      procedure StretchDraw(const ARect: TRect; AGraphic: TGraphic); override;
      procedure TextOut(X, Y: Integer; const Text: String); override;
      procedure DrawFocusRect(const ARect: TRect); override;
      procedure Frame3D(var ARect: TRect; const AFrameWidth: Integer; const AStyle: TGraphicsBevelCut); override;
      procedure FrameRect(const ARect: TRect); override;
    end;

  TOGLTexture = class(IFPObserver)
  strict private
    FBitmap: TBitmap;
    FTexture: Integer;
  public
    procedure FPOObservedChanged(ASender: TObject; AOperation: TFPObservedOperation;  AData: Pointer);
  public
    constructor Create(const ABitmap: TBitmap);
    destructor Destroy; override;
  public
    procedure Release;
    procedure Generate(const ABitmap: TBitmap);
    procedure FromBitmap(const ABitmap: TBitmap);
  public
    property TextureId: Integer read FTexture;
  end;

  TTextures = specialize TFPGMap<Pointer, TOGLTexture>;


//--------------------------------------------

procedure oglcBitmapColorKey(const ABitmap: TBitmap; const ATransparentColor: TColorRef);
function  oglcBitmapCreate32(const ABitmap: TBitmap): TBitmap;
function  oglcSetColor(AColor: TColor): Boolean;
function  oglcSetPen(APen: TPen): Boolean;
function  oglcSetBrush(ABrush: TBrush): Boolean;
function  oglcGetPixelFormat(const APixelFormat: TPixelFormat): Integer;
procedure oglcDrawLine(const X1, Y1, X2, Y2: TOGLCFloat);
procedure oglcDrawRectangle(const AMode: GLenum; const X1, Y1, X2, Y2: TOGLCFloat);
procedure oglcDrawFocusRect(const X1, Y1, X2, Y2: TOGLCFloat);
procedure oglcDrawEllipse(const AMode: GLenum; const X1, Y1, X2, Y2: TOGLCFloat);
procedure oglcDrawPolyline(const AMode: GLenum; const APoints: PPoint; const ACount: Integer);
procedure oglcDrawBitmap(const ABitmap: TBitmap; const ARect: TRect);
procedure oglcDrawTexture(const ATexture: GLuint; const ARect: TRect);
procedure oglcClearOrtho2D(const ARect: TRect; const AColor: TColor);

var
  GTextures: TTextures;

implementation

//===================== TOpenGLCanvas ===================================

constructor TOpenGLCanvas.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetInitialBounds(0, 0, 120, 120);
  if not (csDesigning in ComponentState) then
  begin
    FOldCanvas := Canvas;
    Canvas := TOGLCanvas.Create;
    FControl := TOpenGLPanel.Create(Self);
    FControl.Align := alClient;
    FControl.Parent := Self;
    TOpenGLPanel(FControl).OnPaint := @DoOnPaint;
  end;
  Color := clBtnFace;
end;

destructor TOpenGLCanvas.Destroy;
begin
  if not (csDesigning in ComponentState) then
  begin
    Canvas.Free;
    Canvas := FOldCanvas;
  end;
  inherited Destroy;
end;

procedure TOpenGLCanvas.PaintWindow(DC: HDC);
begin
  if csDesigning in ComponentState then
  begin
    inherited PaintWindow(DC);
  end;
end;

procedure TOpenGLCanvas.DoOnPaint(ASender: TObject);
begin
  oglcClearOrtho2D(FControl.ClientRect, ColorToRGB(Color));
  inherited Paint;
  TOpenGLPanel(FControl).SwapBuffers;
end;

//===================== TOGLCanvas ===================================

procedure TOGLCanvas.DrawAsTexture(const ARect: TRect; const ABitmap: TBitmap);
var
  LIndex: Integer;
  LTexture: TOGLTexture;
begin
  if GTextures.Find(ABitmap, LIndex) then
  begin
    LTexture := GTextures.Data[LIndex];
  end else begin
    LTexture := TOGLTexture.Create(ABitmap);
  end;
  oglcDrawTexture(LTexture.TextureId, ARect);
end;

procedure TOGLCanvas.SetPixel(X, Y: Integer; AColor: TColor);
begin
  if oglcSetColor(AColor) then
  begin
    glBegin(GL_POINTS);
    glVertex2i(X, Y);
    glEnd();
  end;
end;

procedure TOGLCanvas.StretchDraw(const ARect: TRect; AGraphic: TGraphic);
var
  LBitmap: TBitmap;
  LTemp: TBitmap;
begin
  if Assigned(AGraphic) then
  begin
    LBitmap := AGraphic as TBitmap;

    if (LBitmap.Transparent and (LBitmap.Canvas.Font.Quality <> fqNonAntialiased)) or
      (ARect.Left < 0) or (ARect.Top < 0) then
    begin
      DrawAsTexture(ARect, LBitmap);
    end else
    begin

      if LBitmap.Transparent and (LBitmap.PixelFormat <> pf32bit) then
      begin
        LTemp := oglcBitmapCreate32(LBitmap);
        try
          oglcDrawBitmap(LTemp, ARect);
        finally
          FreeAndNil(LTemp);
        end;
      end else
      begin
        oglcDrawBitmap(LBitmap, ARect);
      end;

    end;
  end;
end;

procedure TOGLCanvas.Draw(X, Y: Integer; AGraphic: TGraphic);
begin
  if Assigned(AGraphic) then
  begin
    StretchDraw(Bounds(X, Y, AGraphic.Width, AGraphic.Height), AGraphic);
  end;
end;

procedure TOGLCanvas.DoMoveTo(X, Y: Integer);
begin
  // NOTHING TODO HERE
end;

procedure TOGLCanvas.DoLineTo(X, Y: Integer);
begin
  if oglcSetPen(Pen) then
  begin
    oglcDrawLine(FPenPos.X, FPenPos.Y, X, Y);
    FPenPos.X := X;
    FPenPos.Y := Y;
  end;
end;

procedure TOGLCanvas.DoLine(X1, Y1, X2, Y2: Integer);
begin
  if oglcSetPen(Pen) then
  begin
    oglcDrawLine(X1, Y1, X2, Y2);
  end;
end;

procedure TOGLCanvas.FillRect(const ARect: TRect);
begin
  if oglcSetBrush(Brush) then
  begin
    oglcDrawRectangle(GL_TRIANGLE_FAN, ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
  end;
end;

procedure TOGLCanvas.Rectangle(X1, Y1, X2, Y2: Integer);
begin
  FillRect(Rect(X1, Y1, X2, Y2));
  if oglcSetPen(Pen) then
  begin
    oglcDrawRectangle(GL_LINE_LOOP, X1, Y1, X2, Y2);
  end;
end;

function TOGLCanvas.TextExtent(const Text: String): TSize;
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.Canvas.Font := Font;
    Result := LBitmap.Canvas.TextExtent(Text);
  finally
    FreeAndNil(LBitmap);
  end;
end;

procedure TOGLCanvas.TextOut(X, Y: Integer; const Text: String);
var
  LSize: TSize;
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.Canvas.Brush := Brush;
    LBitmap.Canvas.Pen := Pen;
    LBitmap.Canvas.Font := Font;
    LBitmap.Canvas.Font.Quality := fqNonAntialiased;
    if LBitmap.Canvas.Brush.Style = bsClear then
    begin
      LBitmap.Transparent := True;
      LBitmap.TransparentColor := DefaultTransparentColor;
      LBitmap.Canvas.Brush.Color := DefaultTransparentColor;
    end;
    LSize := LBitmap.Canvas.TextExtent(Text);
    LBitmap.PixelFormat := pf32bit;
    LBitmap.SetSize(LSize.cx, LSize.cy);
    LBitmap.Canvas.TextOut(0, 0, Text);
    oglcBitmapColorKey(LBitmap, DefaultTransparentColor);
    Draw(X, Y, LBitmap);
  finally
    FreeAndNil(LBitmap);
  end;
end;

procedure TOGLCanvas.Polyline(APoints: PPoint; ACount: Integer);
begin
  if oglcSetPen(Pen) then
  begin
    oglcDrawPolyline(GL_LINE_STRIP, APoints, ACount);
  end;
end;

procedure TOGLCanvas.Polygon(APoints: PPoint; ACount: Integer; AWinding: Boolean = False);
begin
  // TODO: AWinding support.
  if oglcSetBrush(Brush) then
  begin
    oglcDrawPolyline(GL_POLYGON, APoints, ACount);
  end;
  if oglcSetPen(Pen) then
  begin
    oglcDrawPolyline(GL_LINE_LOOP, APoints, ACount);
  end;
end;

procedure TOGLCanvas.Ellipse(X1, Y1, X2, Y2: Integer);
begin
  if oglcSetBrush(Brush) then
  begin
    oglcDrawEllipse(GL_TRIANGLE_FAN, X1, Y1, X2, Y2);
  end;
  if oglcSetPen(Pen) then
  begin
    oglcDrawEllipse(GL_LINE_STRIP, X1, Y1, X2, Y2);
  end;
end;

procedure TOGLCanvas.DrawFocusRect(const ARect: TRect);
begin
  oglcDrawFocusRect(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
end;

procedure TOGLCanvas.Frame3D(var ARect: TRect; const AFrameWidth: Integer;
  const AStyle: TGraphicsBevelCut);
begin
  Pen.Width := 1;
  case AStyle of
    bvLowered: begin
      inherited Frame3D(ARect, clBtnShadow, clBtnHighlight, AFrameWidth);
    end;
    bvRaised: begin
      inherited Frame3D(ARect, clBtnHighlight, clBtnShadow, AFrameWidth);
    end;
  end;
end;

procedure TOGLCanvas.FrameRect(const ARect: TRect);
begin
  if oglcSetBrush(Brush) then
  begin
    oglcDrawRectangle(GL_LINE_LOOP, ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
  end;
end;

//================== TOGLTexture ==============================

constructor TOGLTexture.Create(const ABitmap: TBitmap);
begin
  inherited Create;
  Assert(Assigned(ABitmap));
  ABitmap.FPOAttachObserver(Self);
  FBitmap := ABitmap;
  GTextures.Add(ABitmap, Self);
  FromBitmap(ABitmap);
end;

destructor TOGLTexture.Destroy;
begin
  Release;
  GTextures.Remove(FBitmap);
  inherited Destroy;
end;

procedure TOGLTexture.FPOObservedChanged(ASender: TObject; AOperation: TFPObservedOperation;
  AData: Pointer);
begin
  if AOperation = ooFree then
  begin
    Free;
  end;
end;

procedure TOGLTexture.Release;
begin
  if FTexture <> 0 then
  begin
    glDeleteTextures(1, @FTexture);
    FTexture := 0;
  end;
end;

procedure TOGLTexture.Generate(const ABitmap: TBitmap);
begin
  Release;
  glGenTextures(1, @FTexture);
  glBindTexture(GL_TEXTURE_2D, FTexture);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
  gluBuild2DMipmaps(GL_TEXTURE_2D, GL_RGBA, ABitmap.Width, ABitmap.Height,
  oglcGetPixelFormat(ABitmap.PixelFormat), GL_UNSIGNED_BYTE, ABitmap.RawImage.Data);
end;

procedure TOGLTexture.FromBitmap(const ABitmap: TBitmap);
var
  LBitmap: TBitmap;
begin
  if ABitmap.Transparent and (ABitmap.PixelFormat <> pf32bit) then
  begin
    LBitmap := oglcBitmapCreate32(ABitmap);
    try
      Generate(LBitmap);
    finally
      FreeAndNil(LBitmap);
    end;
  end else begin
    Generate(ABitmap);
  end;
end;

//========== OpenGL 2D Functions =====================================

procedure oglcBitmapColorKey(const ABitmap: TBitmap; const ATransparentColor: TColorRef);
var
  LIndex: PtrUInt;
  LPixel: PByte;
begin
  if ABitmap.PixelFormat = pf32bit then
  begin
    LPixel := ABitmap.RawImage.Data;
    LIndex := 0;
    while LIndex < ABitmap.RawImage.DataSize do
    begin
      if (PDWord(LPixel)^ and clWhite) = ATransparentColor then
      begin
        LPixel[3] := 0;
      end else begin
        LPixel[3] := 255;
      end;
      LPixel += 4;
      LIndex += 4;
    end;
  end;
end;

function oglcBitmapCreate32(const ABitmap: TBitmap): TBitmap;
begin
  Result := TBitmap.Create;
  try
    Result.PixelFormat := pf32bit;
    Result.SetSize(ABitmap.Width, ABitmap.Height);
    Result.Canvas.Brush.Color := DefaultTransparentColor;
    Result.Canvas.FillRect(0, 0, ABitmap.Width, ABitmap.Height);
    Result.Canvas.Draw(0, 0, ABitmap);
    oglcBitmapColorKey(Result, DefaultTransparentColor);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function oglcSetColor(AColor: TColor): Boolean;
begin
  Result := AColor <> clNone;
  if Result then
  begin
    AColor := ColorToRGB(AColor);
    glColor3ub(Red(AColor), Green(AColor), Blue(AColor));
  end;
end;

function oglcSetPen(APen: TPen): Boolean;
begin
  glLineWidth(APen.Width);
  Result := oglcSetColor(APen.Color);
end;

function oglcSetBrush(ABrush: TBrush): Boolean;
begin
  Result := (ABrush.Style <> bsClear) and oglcSetColor(ABrush.Color);
end;

function oglcGetPixelFormat(const APixelFormat: TPixelFormat): Integer;
begin
  case APixelFormat of
    pf24bit: begin
      Result := GL_BGR;
    end;
    pf32bit: begin
      Result := GL_BGRA;
    end;
  end;
end;

procedure oglcDrawLine(const X1, Y1, X2, Y2: TOGLCFloat);
begin
  glBegin(GL_LINES);
  glVertex2f(X1, Y1);
  glVertex2f(X2, Y2);
  glEnd;
end;

procedure oglcDrawRectangle(const AMode: GLenum; const X1, Y1, X2, Y2: TOGLCFloat);
begin
  glBegin(AMode);
  glVertex2f(X1 + 1, Y1 + 1);
  glVertex2f(X2, Y1 + 1);
  glVertex2f(X2, Y2);
  glVertex2f(X1 + 1, Y2);
  glEnd;
end;

procedure oglcDrawFocusRect(const X1, Y1, X2, Y2: TOGLCFloat);
begin
  glLineWidth(1);
  glEnable(GL_COLOR_LOGIC_OP);
  glLogicOp(GL_XOR);
  oglcSetColor(clRed);
  oglcDrawRectangle(GL_LINE_LOOP, X1, Y1, X2, Y2);
  glDisable(GL_COLOR_LOGIC_OP);
end;

procedure oglcDrawEllipse(const AMode: GLenum; const X1, Y1, X2, Y2: TOGLCFloat);
var
  I: Integer;
  X, Y, W, H: TOGLCFloat;
begin
  X := (X1 + X2) * +0.5;
  Y := (Y1 + Y2) * +0.5;
  W := (X2 - X1) * +0.5;
  H := (Y2 - Y1) * -0.5;
  glBegin(AMode);
  for I := 0 to 360 do
  begin
    glVertex2f(X + Sin(I * PI / 180) * W, Y + Cos(I * PI / 180) * H);
  end;
  glEnd;
end;

procedure oglcDrawPolyline(const AMode: GLenum; const APoints: PPoint; const ACount: Integer);
var
  LIndex: Integer;
begin
  glBegin(AMode);
  for LIndex := 0 to ACount - 1 do
  begin
    glVertex2iv(@APoints[LIndex]);
  end;
  glEnd;
end;

procedure oglcDrawBitmap(const ABitmap: TBitmap; const ARect: TRect);
begin
  if ABitmap.PixelFormat = pf32bit then
  begin
    glEnable(GL_BLEND);
  end;
  glPixelZoom((ARect.Right - ARect.Left) / ABitmap.Width, -(ARect.Bottom - ARect.Top) / ABitmap.Height);
  glRasterPos2f(ARect.Left, ARect.Top);
  glDrawPixels(ABitmap.Width, ABitmap.Height, oglcGetPixelFormat(ABitmap.PixelFormat), GL_UNSIGNED_BYTE, ABitmap.RawImage.Data);
  glDisable(GL_BLEND);
end;

procedure oglcDrawTexture(const ATexture: GLuint; const ARect: TRect);
begin
  glBindTexture(GL_TEXTURE_2D, ATexture);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glBegin(GL_TRIANGLE_FAN);
  try
    glColor3f(1, 1, 1);
    glTexCoord2f(0, 0);
    glVertex2f(ARect.Left, ARect.Top);
    glTexCoord2f(1, 0);
    glVertex2f(ARect.Right, ARect.Top);
    glTexCoord2f(1, 1);
    glVertex2f(ARect.Right, ARect.Bottom);
    glTexCoord2f(0, 1);
    glVertex2f(ARect.Left, ARect.Bottom);
  finally
    glEnd;
    glDisable(GL_TEXTURE_2D);
    glDisable(GL_BLEND);
  end;
end;

procedure oglcClearOrtho2D(const ARect: TRect; const AColor: TColor);
begin
  glClearColor(Red(AColor) / 255, Green(AColor) / 255, Blue(AColor) / 255, 1.0);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glViewport(ARect.Left, ARect.Top, ARect.Right - ARect.Left, ARect.Bottom - ARect.Top);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  gluOrtho2D(ARect.Left, ARect.Right, ARect.Bottom, ARect.Top);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
end;

//===============================================

initialization
  GTextures := TTextures.Create;
finalization
  if GTextures<>nil then GTextures.Free;

end.
