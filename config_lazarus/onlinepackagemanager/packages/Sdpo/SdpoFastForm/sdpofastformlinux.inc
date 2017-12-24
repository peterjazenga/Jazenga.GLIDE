type
  PPoint = ^TPoint;

  TSdpoFastFormCanvasHelper = class
  private
    { private declarations }
    FColor: TGdkColor;
    FDepth: Integer;
    FWidth: Integer;

    procedure SetColor(col: TColor);
    function GetColor: TColor;
    procedure SetWidth(value: integer);
  public
    { public declarations }
    constructor Create; virtual;

    property ColorDepth: integer read FDepth write FDepth;
    property GdkColor: TGdkColor read FColor;
    property Color: TColor read GetColor write SetColor;
    property Width: Integer read FWidth write SetWidth;
  end;

  TSdpoFastFormCanvasBrush = class(TSdpoFastFormCanvasHelper)
  private
    { private declarations }
    FStyle: TBrushStyle;
  public
    { public declarations }
    constructor Create; override;

    property Style: TBrushStyle read FStyle write FStyle;
  end;

  { TSdpoFastFormCanvas }

  TSdpoFastFormCanvas = class
  private
    { private declarations }
    FPixmap: PGdkPixmap;
    FGc: PGdkGC;
    FIntColor: integer;
    FGdkFont: PGdkFont;
    FPenPos: TPoint;

    FPen: TSdpoFastFormCanvasHelper;
    FBrush: TSdpoFastFormCanvasBrush;
    FFont: TSdpoFastFormCanvasHelper;

    procedure SetFgColor(fgColor: PGdkColor);
    procedure SetFgWidth(fgWidth: longint);
  public
    { public declarations }
    constructor Create;
    destructor Destroy;

    procedure Init(window: PGdkWindow; pix: PGdkPixmap; font: PGdkFont);

    procedure Arc(ALeft, ATop, ARight, ABottom, angle1, angle2: Integer);
    procedure FillArc(ALeft, ATop, ARight, ABottom, angle1, angle2: Integer);
    procedure Ellipse(const ARect: TRect);
    procedure Ellipse(x1, y1, x2, y2: Integer);
    procedure Rectangle(const Bounds: TRect);
    procedure Rectangle(X1,Y1,X2,Y2: Integer);
    procedure FillRect(const Bounds: TRect);
    procedure FillRect(X1,Y1,X2,Y2: Integer);
    procedure Line(X1,Y1,X2,Y2: Integer);
    procedure Line(const p1,p2: TPoint);
    procedure Line(const Points: TRect);
    procedure LineTo(X1,Y1: Integer);
    procedure MoveTo(X1,Y1: Integer);
    procedure Polyline(const Points: array of TPoint;
                       StartIndex: Integer;
                       NumPts: Integer = -1);
    procedure Polyline(Points: PPoint; NumPts: Integer);
    procedure Polyline(const Points: array of TPoint);
    procedure Polygon(const Points: array of TPoint;
                      StartIndex: Integer = 0;
                      NumPts: Integer = -1);
    procedure Polygon(Points: PPoint; NumPts: Integer);
    procedure TextOut(X,Y: Integer; Text: String);

    property Font: TSdpoFastFormCanvasHelper read FFont write FFont;
    property Pen: TSdpoFastFormCanvasHelper read FPen write FPen;
    property Brush: TSdpoFastFormCanvasBrush read FBrush;
    //property BgColor: TColor read ...
  end;


type
  TSdpoFastFormOnMouseDownEvent = procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer) of object;
  TSdpoFastFormOnMouseUpEvent = procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer) of object;
  TSdpoFastFormOnMouseMoveEvent = procedure(Sender: TObject; Shift: TShiftState; X, Y: Integer) of object;
  TSdpoFastFormOnKeyDownEvent = procedure(Sender: TObject; Key: LongWord; Shift: TShiftState) of object;
  TSdpoFastFormOnKeyUpEvent = procedure(Sender: TObject; Key: LongWord; Shift: TShiftState) of object;
  TSdpoFastFormOnCloseEvent = procedure(Sender: TObject; var CloseAction: TCloseAction) of object;
  TSdpoFastFormOnShowEvent = procedure(Sender: TObject) of object;

type
  TSdpoFastForm = class(TComponent)
  private
    { private declarations }
    FOnMouseDown: TSdpoFastFormOnMouseDownEvent;
    FOnMouseMove: TSdpoFastFormOnMouseMoveEvent;
    FOnMouseUp: TSdpoFastFormOnMouseUpEvent;
    FOnKeyDown: TSdpoFastFormOnKeyDownEvent;
    FOnKeyUp: TSdpoFastFormOnKeyUpEvent;
    FOnClose: TSdpoFastFormOnCloseEvent;
    FOnShow: TSdpoFastFormOnShowEvent;

    FVisible: boolean;
    //FCanClose: boolean;
    FWidth: integer;
    FHeight: integer;
    FPosition: TFFPosition;
    FCaption: string;
    FImageType: TFFImageType;
    FPos_X,FPos_Y,FWin_X,FWin_Y: Integer;
    FTopBarSize: boolean;
    FAlwaysOnTop: boolean;

    FCanvas: TSdpoFastFormCanvas;

    // structures to hold window parameters
    DebugList: TStrings;

    FWindow: PGtkWidget;
    FImage: PGdkImage;
    FWinGeom: TGdkGeometry;
    FPixmap: PGdkPixmap;

    FIconFileName: string;

    procedure SetHeight(const AValue: integer);
    procedure SetWidth(const AValue: integer);
    procedure WindowOpen;
    procedure WindowClose;
    procedure SetVisible(state: boolean);

    procedure ReleaseImage;
    procedure AllocImage;
    procedure AllocWindow;
    procedure ResizeWindow;
    function GetHandle: HWND;
  protected
    { protected declarations }

    function GetData: pointer;
    function GetWindow: PGtkWindow;
    function GetCanvas: TSdpoFastFormCanvas;
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Show;
    procedure Close;

    procedure Paint;
    procedure Sync;

    procedure DrawToPixmap;  // DrawToPixmap + canvas + PaintFromPixmap
    procedure PaintFromPixmap;

    function GetDepth: integer; //PGtkStyle(PGtkWidget(FastForm.Window)^.thestyle)^.depth

    procedure Init;

    procedure SetCaption(cap: string);
    procedure SetAlwaysOnTop(setting: boolean);

    procedure BringToFront;

    procedure SetDebugList(dl: TStrings);
    procedure AddDebug(s: string);

    property Data: pointer read GetData;  //Data: pointer
    property Image: PGdkImage read FImage;
    property Window: PGtkWindow read GetWindow;
    property Canvas: TSdpoFastFormCanvas read GetCanvas;
    property Visible: boolean read FVisible;
    property Handle: HWND read GetHandle;
  published
    //property Visible: boolean read FVisible write SetVisible;
    //property CanClose: boolean read FCanClose write FCanClose; //CanClose
    property Width: integer read FWidth write SetWidth;
    property Height: integer read FHeight write SetHeight;
    property TopPos: integer read FPos_Y write FPos_Y;
    property LeftPos: integer read FPos_X write FPos_X;
    property Position: TFFPosition read FPosition write FPosition;
    property Caption: string read FCaption write SetCaption;
    property ImageType: TFFImageType read FImageType write FImageType;
    property AlwaysOnTop: boolean read FAlwaysOnTop write SetAlwaysOnTop;
    property Icon: string read FIconFileName write FIconFileName ;

    property OnMouseDown: TSdpoFastFormOnMouseDownEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseMove: TSdpoFastFormOnMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseUp: TSdpoFastFormOnMouseUpEvent read FOnMouseUp write FOnMouseUp;
    property OnKeyUp: TSdpoFastFormOnKeyUpEvent read FOnKeyUp write FOnKeyUp;
    property OnKeyDown: TSdpoFastFormOnKeyDownEvent read FOnKeyDown write FOnKeyDown;
    property OnClose: TSdpoFastFormOnCloseEvent read FOnClose write FOnClose;
    property OnShow: TSdpoFastFormOnShowEvent read FOnShow write FOnShow;
  end;


// -----------------------------------------------
//     callback prototypes
procedure MotionEvent(widget: PGtkWidget; motion: PGdkEventMotion; localform: TSdpoFastForm); cdecl;
procedure ButtonPressEvent(widget: PGtkWidget; button: PGdkEventButton; localform: TSdpoFastForm); cdecl;
procedure ButtonReleaseEvent(widget: PGtkWidget; button: PGdkEventButton; localform: TSdpoFastForm); cdecl;
procedure KeyPressEvent(widget: PGtkWidget; key: PGdkEventKey; localform: TSdpoFastForm); cdecl;
procedure KeyReleaseEvent(widget: PGtkWidget; key: PGdkEventKey; localform: TSdpoFastForm); cdecl;
procedure ConfigureEvent(widget: PGtkWidget; config: PGdkEventConfigure; localform: TSdpoFastForm); cdecl;
function DeleteEvent(widget: PGtkWidget; event: PGdkEvent; localform: TSdpoFastForm): gBoolean; cdecl;

procedure Register;

implementation

//TSdpoFastFormCanvas
constructor TSdpoFastFormCanvas.Create;
begin
  //inherited Create(AOwner);
  FGc:=nil;
  FPixmap:=nil;
  FGdkFont:=nil;
  FPen:=TSdpoFastFormCanvasHelper.Create;
  FBrush:=TSdpoFastFormCanvasBrush.Create;
  FFont:=TSdpoFastFormCanvasHelper.Create;
end;

destructor TSdpoFastFormCanvas.Destroy;
begin
  //inherited Destroy;
end;

procedure TSdpoFastFormCanvas.SetFgColor(fgColor: PGdkColor);
begin
  if FGc = nil then exit;
  if fgColor^.pixel = FIntColor then exit;
  gdk_gc_set_foreground(FGc,fgColor);
end;

procedure TSdpoFastFormCanvas.SetFgWidth(fgWidth: longint);
begin
  if FGc = nil then exit;
  //if fgColor^.pixel = FIntColor then exit;
  gdk_gc_set_line_attributes(FGc,fgWidth,GDK_LINE_SOLID,GDK_CAP_NOT_LAST,GDK_JOIN_MITER);
end;

procedure TSdpoFastFormCanvas.Init(window: PGdkWindow; pix: PGdkPixmap; font: PGdkFont);
var
  blackColor: TGdkColor;
begin
  FPixmap:=pix;
  FGc:=gdk_gc_new(window);
  FIntColor:=0;
  blackColor.pixel:=FIntColor;
  gdk_gc_set_foreground(FGc,@blackColor);
  FGdkFont:=font;
end;

constructor TSdpoFastFormCanvasBrush.Create;
begin
  inherited;
  FStyle:=bsSolid;
end;

constructor TSdpoFastFormCanvasHelper.Create;
var
  red,green,blue: byte;
begin
  RedGreenBlue(clWhite,red,green,blue);
  FColor.pixel:=(red shl 16)+(green shl 8)+blue;
  FWidth:=1;
end;

procedure TSdpoFastFormCanvasHelper.SetColor(col: TColor);
var
  red,green,blue: byte;
  tempcolor: integer;
begin
  RedGreenBlue(col,red,green,blue);
  if FDepth = 16 then
    tempcolor:=((red and $F8) shl 8) + ((green and $FC) shl 3) + ((blue and $F8) shr 3)
  else
    tempcolor:=(red shl 16)+(green shl 8)+blue;

  if FColor.pixel=tempcolor then exit;
  FColor.pixel:=tempcolor;
end;

function TSdpoFastFormCanvasHelper.GetColor: TColor;
begin
  result:=RGBToColor(((FColor.pixel shr 16) and $FF),((FColor.pixel shr 8) and $FF),
                     (FColor.pixel and $FF));
end;

procedure TSdpoFastFormCanvasHelper.SetWidth(value: integer);
begin
  if value < 0 then FWidth := 1
  else FWidth:=value;
end;

procedure TSdpoFastFormCanvas.Arc(ALeft, ATop, ARight, ABottom, angle1, angle2: Integer);
begin
  if FGc=nil then exit;

  if Brush.Style = bsSolid then begin
    SetFgColor(@Brush.GdkColor);
    gdk_draw_arc(FPixmap, FGc, 1, ALeft, ATop, (ARight-ALeft), (ABottom-ATop), angle1, angle2);
  end;

  SetFgColor(@Pen.GdkColor);
  SetFgWidth(Pen.Width);
  gdk_draw_arc(FPixmap, FGc, 0, ALeft, ATop, (ARight-ALeft), (ABottom-ATop), angle1, angle2);
end;

procedure TSdpoFastFormCanvas.FillArc(ALeft, ATop, ARight, ABottom, angle1, angle2: Integer);
begin
  if FGc=nil then exit;

  if Brush.Style = bsSolid then begin
    SetFgColor(@Brush.GdkColor);
    gdk_draw_arc(FPixmap, FGc, 1, ALeft, ATop, (ARight-ALeft), (ABottom-ATop), angle1, angle2);
  end;
end;

procedure TSdpoFastFormCanvas.Ellipse(const ARect: TRect);
begin
  Ellipse(ARect.Left,ARect.Top,ARect.Right,ARect.Bottom);
end;

procedure TSdpoFastFormCanvas.Ellipse(x1, y1, x2, y2: Integer);
begin
  if FGc=nil then exit;

  Arc(x1, y1, x2, y2, 0, 360*16*4);
end;

procedure TSdpoFastFormCanvas.Rectangle(const Bounds: TRect);
begin
  Rectangle(Bounds.Left, Bounds.Top, Bounds.Right, Bounds.Bottom);
end;

procedure TSdpoFastFormCanvas.Rectangle(X1,Y1,X2,Y2: Integer);
begin
  if FGc=nil then exit;

  if Brush.Style = bsSolid then begin
    SetFgColor(@Brush.GdkColor);
    gdk_draw_rectangle(FPixmap, FGc, 1, X1, Y1, (X2-X1), (Y2-Y1));
  end;

  SetFgColor(@Pen.GdkColor);
  SetFgWidth(Pen.Width);
  gdk_draw_rectangle(FPixmap, FGc, 0, X1, Y1, (X2-X1), (Y2-Y1));
end;

procedure TSdpoFastFormCanvas.FillRect(const Bounds: TRect);
begin
  FillRect(Bounds.Left, Bounds.Top, Bounds.Right, Bounds.Bottom);
end;

procedure TSdpoFastFormCanvas.FillRect(X1,Y1,X2,Y2: Integer);
begin
  if FGc=nil then exit;

  if Brush.Style = bsSolid then begin
    SetFgColor(@Brush.GdkColor);
    gdk_draw_rectangle(FPixmap, FGc, 1, X1, Y1, (X2-X1), (Y2-Y1));
  end;
end;

procedure TSdpoFastFormCanvas.Line(X1,Y1,X2,Y2: Integer);
begin
  if FGc=nil then exit;

  SetFgColor(@Pen.GdkColor);
  SetFgWidth(Pen.Width);
  gdk_draw_line(FPixmap,FGc,X1,Y1,X2,Y2);
end;

procedure TSdpoFastFormCanvas.Line(const p1,p2: TPoint);
begin
  Line(p1.x,p1.y,p2.x,p2.y);
end;

procedure TSdpoFastFormCanvas.Line(const Points: TRect);
begin
  with Points do
    Line(Left,Top,Right,Bottom);
end;

procedure TSdpoFastFormCanvas.MoveTo(X1,Y1: Integer);
begin
  FPenPos.X:=X1;
  FPenPos.Y:=Y1;
end;

procedure TSdpoFastFormCanvas.LineTo(X1,Y1: Integer);
begin
  Line(FPenPos.X,FPenPos.Y,X1,Y1);
  MoveTo(X1,Y1);
end;

procedure TSdpoFastFormCanvas.Polyline(const Points: array of TPoint; StartIndex: Integer;
  NumPts: Integer);
var
  NPoints : integer;
begin
  if NumPts<0 then
    NPoints:=High(Points)-StartIndex+1
  else
    NPoints:=NumPts;
  if NPoints<=0 then exit;
  Polyline(@Points[StartIndex], NPoints);
end;

procedure TSdpoFastFormCanvas.Polyline(Points: PPoint; NumPts: Integer);
var
  idx: integer;
begin
  //if FGc=nil then exit;
  //DrawPolygon(Points,NumPts,0);
  if FGc=nil then exit;

  SetFgColor(@Pen.GdkColor);
  SetFgWidth(Pen.Width);
  for idx:=0 to NumPts-2 do
    gdk_draw_line(FPixmap,FGc,Points[idx].X,Points[idx].Y,Points[idx+1].X,Points[idx+1].Y);
end;

procedure TSdpoFastFormCanvas.Polyline(const Points: array of TPoint);
begin
  Polyline(Points, Low(Points), High(Points) - Low(Points) + 1);
end;

procedure TSdpoFastFormCanvas.Polygon(const Points: array of TPoint;
  StartIndex: Integer; NumPts: Integer);
var
  NPoints: integer;
begin
  if NumPts<0 then
    NPoints:=High(Points)-StartIndex+1
  else
    NPoints:=NumPts;
  if NPoints<=0 then exit;
  Polygon(@Points[StartIndex],NPoints);
end;

procedure TSdpoFastFormCanvas.Polygon(Points: PPoint; NumPts: Integer);
var
  localPoints, tempPoints: PGdkPoint;
  idx: integer;
begin
  if FGc=nil then exit;
  {$ifdef LCLGTK2}
  if Brush.Style = bsSolid then begin
    SetFgColor(@Brush.GdkColor);
    gdk_draw_polygon(FPixmap,FGc,1,PGdkPoint(Points),NumPts);
  end;

  SetFgColor(@Pen.GdkColor);
  SetFgWidth(Pen.Width);
  gdk_draw_polygon(FPixmap,FGc,0,PGdkPoint(Points),NumPts);
  {$endif}
  {$ifdef LCLGTK}
  GetMem(localPoints, sizeof(TGdkPoint)*NumPts);
  try
    tempPoints:=localPoints;
    for idx:=0 to NumPts-1 do begin
      tempPoints^.x:=Points^.X;
      tempPoints^.y:=Points^.Y;
      inc(tempPoints);
      inc(Points);
    end;

    if Brush.Style = bsSolid then begin
      SetFgColor(@Brush.GdkColor);
      gdk_draw_polygon(FPixmap,FGc,1,localPoints,NumPts);
    end;

    SetFgColor(@Pen.GdkColor);
    SetFgWidth(Pen.Width);
    gdk_draw_polygon(FPixmap,FGc,0,localPoints,NumPts);
  finally
    Freemem(localPoints);
  end;
  {$endif}
end;

procedure TSdpoFastFormCanvas.TextOut(X,Y: Integer; Text: String);
begin
  if FGc=nil then exit;

  SetFgColor(@Font.GdkColor);
  gdk_draw_string(FPixmap,FGdkFont,FGc,X,Y,Pgchar(Text));
end;

//TSdpoFastForm

constructor TSdpoFastForm.Create(AOwner: TComponent);
begin
  FWidth:=100;
  FHeight:=100;
  FPos_X:=0;
  FPos_Y:=0;
  FWin_X:=0;
  FWin_Y:=0;
  inherited Create(AOwner);
  FWindow:=nil;
  FImage:=nil;
  FPixmap:=nil;
  FVisible:=false;
  //FCanClose:=false;
  FPosition:=poNone;
  FImageType:=itNormal;
  DebugList:=nil;
  FTopBarSize:=false;
  FAlwaysOnTop:=false;
  {$ifdef LCLGTK2}
  FIconFileName:='';
  {$endif}

  FCanvas := TSdpoFastFormCanvas.Create;

  //AllocWindow;
  //AllocImage;
end;

destructor TSdpoFastForm.Destroy;
begin
  inherited Destroy;

  if FWindow<>nil then
    gtk_widget_destroy(FWindow);
end;

procedure TSdpoFastForm.Paint;
begin
  if (FImage = nil) or (FWindow = nil) then exit;

  {$ifdef LCLGTK2}
  gdk_draw_image(FWindow^.window,FWindow^.style^.white_gc,FImage,0,0,0,0,FWidth,FHeight);
  {$endif}
  {$ifdef LCLGTK}
  gdk_draw_image(FWindow^.window,PGtkStyle(FWindow^.thestyle)^.white_gc,FImage,0,0,0,0,FWidth,FHeight);
  {$endif}
end;

procedure TSdpoFastForm.DrawToPixmap;
begin
  if (FImage = nil) or (FWindow = nil) then exit;

  {$ifdef LCLGTK2}
  gdk_draw_image(FPixmap,FWindow^.style^.white_gc,FImage,0,0,0,0,FWidth,FHeight);
  {$endif}
  {$ifdef LCLGTK}
  gdk_draw_image(FPixmap,PGtkStyle(FWindow^.thestyle)^.white_gc,FImage,0,0,0,0,FWidth,FHeight);
  {$endif}
end;

procedure TSdpoFastForm.PaintFromPixmap;
begin
  if (FImage = nil) or (FWindow = nil) then exit;

  {$ifdef LCLGTK2}
  gdk_draw_pixmap(FWindow^.window,FWindow^.style^.white_gc,FPixmap,0,0,0,0,FWidth,FHeight);
  {$endif}
  {$ifdef LCLGTK}
  gdk_draw_pixmap(FWindow^.window,PGtkStyle(FWindow^.thestyle)^.white_gc,FPixmap,0,0,0,0,FWidth,FHeight);
  {$endif}
end;

procedure TSdpoFastForm.Sync;
var
  dis: xlib.PDisplay;
begin
  {$ifdef LCLGTK2}
  dis := GDK_WINDOW_XDISPLAY(FWindow^.window);
  {$endif}
  {$ifdef LCLGTK}
  dis := GDK_WINDOW_XDISPLAY(PGdkWindowPrivate(FWindow^.window));
  {$endif}
  XSync(dis,false);
end;

procedure TSdpoFastForm.SetDebugList(dl: TStrings);
begin
  DebugList:=dl;
end;

procedure TSdpoFastForm.AddDebug(s: string);
begin
  if DebugList<>nil then DebugList.Add(s);
end;

function TSdpoFastForm.GetDepth: integer;
begin
  result := -1;
  if FWindow<>nil then
  {$ifdef LCLGTK2}
    result := FWindow^.style^.depth;
  {$endif}
  {$ifdef LCLGTK}
    result := PGtkStyle(FWindow^.thestyle)^.depth;
  {$endif}
end;

procedure TSdpoFastForm.SetVisible(state: boolean);
begin
  if FVisible=state then exit;

  if state then WindowOpen
  else WindowClose;
end;

procedure TSdpoFastForm.ReleaseImage;
begin
  if FImage = nil then exit;
  try
  //{$ifdef LCLGTK2}
    gdk_image_destroy(FImage);
    g_object_unref(FPixmap);
    FImage:=nil;
    FPixmap:=nil;
    //FImage := gdk_image_new(TGdkImageType(FFImageTypeConsts[FImageType]), gtk_widget_get_visual(FWindow),FWidth,FHeight);
    //FPixmap := gdk_pixmap_new(FWindow^.window,FWidth,FHeight,FWindow^.style^.depth);
    //Canvas.Init(FWindow^.window,FPixmap,gdk_font_from_description(FWindow^.style^.font_desc));
  //{$endif}
  //{$ifdef LCLGTK}
  //  FImage := gdk_image_new(FFImageTypeConsts[FImageType], gtk_widget_get_visual(FWindow),FWidth,FHeight);
  //  FPixmap := gdk_pixmap_new(FWindow^.window,FWidth,FHeight,PGtkStyle(FWindow^.thestyle)^.depth);
  //  Canvas.Init(FWindow^.window,FPixmap,PGtkStyle(FWindow^.thestyle)^.font);
  //{$endif}
  except
    raise Exception.Create('could not free gdk image');
    FImage:=nil;
    FPixmap:=nil;
  end;

end;

procedure TSdpoFastForm.Show;
begin
  SetVisible(true);
end;

procedure TSdpoFastForm.Close;
begin
  SetVisible(false);
end;

procedure TSdpoFastForm.AllocImage;
begin
  if FImage <> nil then exit;
  try
  {$ifdef LCLGTK2}
    FImage := gdk_image_new(TGdkImageType(FFImageTypeConsts[FImageType]), gtk_widget_get_visual(FWindow),FWidth,FHeight);
    FPixmap := gdk_pixmap_new(FWindow^.window,FWidth,FHeight,FWindow^.style^.depth);
    Canvas.Init(FWindow^.window,FPixmap,gdk_font_from_description(FWindow^.style^.font_desc));
  {$endif}
  {$ifdef LCLGTK}
    FImage := gdk_image_new(FFImageTypeConsts[FImageType], gtk_widget_get_visual(FWindow),FWidth,FHeight);
    FPixmap := gdk_pixmap_new(FWindow^.window,FWidth,FHeight,PGtkStyle(FWindow^.thestyle)^.depth);
    Canvas.Init(FWindow^.window,FPixmap,PGtkStyle(FWindow^.thestyle)^.font);
  {$endif}
  except
    raise Exception.Create('could not set gdk image');
    FImage:=nil;
  end;

  if FImage <> nil then begin
  {$ifdef LCLGTK2}
    if DebugList<>nil then AddDebug('window depth: '+inttostr(FWindow^.style^.depth));
    if DebugList<>nil then AddDebug('created image type: '+inttostr(Integer(FImage^._type)));
  {$endif}
  {$ifdef LCLGTK}
    if DebugList<>nil then AddDebug('window depth: '+inttostr(PGtkStyle(FWindow^.thestyle)^.depth));
    if DebugList<>nil then AddDebug('created image type: '+inttostr(FImage^.thetype));
  {$endif}
  end;

  with Canvas do begin
    Pen.ColorDepth:=GetDepth;
    Font.ColorDepth:=GetDepth;
    Brush.ColorDepth:=GetDepth;
  end;

end;

{$ifdef LCLGTK2}
function CreatePixbuf(filename: string): PGdkPixbuf;
var pixbuf: PGdkPixbuf;
    gtkimage: PGtkImage;
begin
  gtkimage := PGtkImage(gtk_image_new_from_file(Pgchar(filename)));
  result := gtk_image_get_pixbuf(gtkimage);
end;
{$endif}

procedure TSdpoFastForm.AllocWindow;
begin
  if FWindow <> nil then exit;

  try
    FWindow:=gtk_window_new(GTK_WINDOW_TOPLEVEL);
    if FWindow=nil then raise Exception.Create('could not create gtk window');
    if DebugList<>nil then AddDebug('window created');

    with FWinGeom do begin
      min_height:=FHeight;
      max_height:=FHeight;
      min_width:=FWidth;
      max_width:=FWidth;
      base_width:=FWidth;
      base_height:=FHeight;
    end;

    gtk_window_set_geometry_hints(GTK_WINDOW(FWindow),FWindow,@FWinGeom,
                                  (GDK_HINT_MIN_SIZE+GDK_HINT_MAX_SIZE+
                                   GDK_HINT_BASE_SIZE));
    if DebugList<>nil then AddDebug('geometry set: '+inttostr(FWidth)+','+inttostr(FHeight));

    gtk_window_set_title(GTK_WINDOW(FWindow),Pgchar(FCaption));

    SetAlwaysOnTop(FAlwaysOnTop);

    if FPosition = TFFPosition(FFPoSelectable) then
      gtk_window_set_position(GTK_WINDOW(FWindow),FFPoNone)
    else
      gtk_window_set_position(GTK_WINDOW(FWindow),FFPositionConsts[FPosition]);

    {$ifdef LCLGTK2}
    if FIconFileName<>'' then begin
      if not FileExists(FIconFileName) then begin
        raise Exception.Create('Icon file does not exist');
        if DebugList<>nil then AddDebug('Icon file does not exist');
      end else begin
        gtk_window_set_icon(GTK_WINDOW(FWindow),CreatePixbuf(FIconFileName));
      end;
    end;
    {$endif}

    gtk_widget_set_events(FWindow,(GDK_POINTER_MOTION_MASK + GDK_BUTTON_PRESS_MASK +
                                   GDK_BUTTON_RELEASE_MASK +
                                   GDK_KEY_PRESS_MASK + GDK_KEY_RELEASE_MASK)); // needed for GetKeyShiftState

    gtk_signal_connect(PGtkObject(FWindow), 'motion_notify_event', GTK_SIGNAL_FUNC(@MotionEvent), Self);
    gtk_signal_connect(PGtkObject(FWindow), 'button_press_event', GTK_SIGNAL_FUNC(@ButtonPressEvent), Self);
    gtk_signal_connect(PGtkObject(FWindow), 'button_release_event', GTK_SIGNAL_FUNC(@ButtonReleaseEvent), Self);
    gtk_signal_connect(PGtkObject(FWindow), 'key_press_event', GTK_SIGNAL_FUNC(@KeyPressEvent), Self);
    gtk_signal_connect(PGtkObject(FWindow), 'key_release_event', GTK_SIGNAL_FUNC(@KeyReleaseEvent), Self);
    gtk_signal_connect(PGtkObject(FWindow), 'configure_event', GTK_SIGNAL_FUNC(@ConfigureEvent), Self);
    gtk_signal_connect(PGtkObject(FWindow), 'delete_event', GTK_SIGNAL_FUNC(@DeleteEvent), Self);
    if DebugList<>nil then AddDebug('events connected');

    gtk_widget_realize(FWindow);

    if FPosition = TFFPosition(FFPoSelectable) then
      gtk_widget_set_uposition(FWindow,FPos_X,FPos_Y);
  except on
    E: exception do begin
      if FWindow<>nil then
        gtk_widget_destroy(FWindow);
      FWindow:=nil;
      if DebugList<>nil then
        AddDebug(E.Message)
      else
        raise;
    end;
  end;
end;

procedure TSdpoFastForm.ResizeWindow;
begin
  if FWindow = nil then exit;

  try
    with FWinGeom do begin
      min_height:=FHeight;
      max_height:=FHeight;
      min_width:=FWidth;
      max_width:=FWidth;
      base_width:=FWidth;
      base_height:=FHeight;
    end;

    gtk_window_set_geometry_hints(GTK_WINDOW(FWindow),FWindow,@FWinGeom,
                                  (GDK_HINT_MIN_SIZE+GDK_HINT_MAX_SIZE+
                                   GDK_HINT_BASE_SIZE));
    if DebugList<>nil then AddDebug('geometry set: '+inttostr(FWidth)+','+inttostr(FHeight));
  except on
    E: exception do begin
      if FWindow<>nil then
        gtk_widget_destroy(FWindow);
      FWindow:=nil;
      if DebugList<>nil then
        AddDebug(E.Message)
      else
        raise;
    end;
  end;
end;

function TSdpoFastForm.GetHandle: HWND;
begin
  Result := HWND(GetWindow);
end;

procedure TSdpoFastForm.WindowOpen;
begin
  if FWindow = nil then
    AllocWindow;

  if FWindow<>nil then begin
    try
      if FPosition = TFFPosition(FFPoSelectable) then
        gtk_widget_set_uposition(FWindow,FPos_X,FPos_Y);
      gtk_widget_show(FWindow);
    except
      raise Exception.Create('could not show gtk window');
      if DebugList<>nil then AddDebug('could not show gtk window');
    end;
  end;

  if Assigned(FOnShow) then FOnShow(Self);
  FVisible:=true;
end;

procedure TSdpoFastForm.SetHeight(const AValue: integer);
begin
  if FHeight=AValue then exit;
  FHeight:=AValue;

  if FImage = nil then exit;
  ResizeWindow;
  ReleaseImage;
  AllocImage;
end;

procedure TSdpoFastForm.SetWidth(const AValue: integer);
begin
  if FWidth=AValue then exit;
  FWidth:=AValue;

  if FImage = nil then exit;
  ResizeWindow;
  ReleaseImage;
  AllocImage;
end;

procedure TSdpoFastForm.WindowClose;
var CloseAction: TCloseAction;
begin
  if FWindow = nil then exit;
  CloseAction := caHide;
  if Assigned(FOnClose) then FOnClose(Self,CloseAction);
  if CloseAction = caHide then begin
    FVisible:=False;
    try
      gtk_widget_hide(FWindow);
    except
      raise Exception.Create('could not hide gtk window');
      if DebugList<>nil then AddDebug('could not hide gtk window');
    end;
  end;
end;

procedure TSdpoFastForm.SetAlwaysOnTop(setting: boolean);
begin
  {$ifdef LCLGTK2}
  if FWindow <> nil then
    gtk_window_set_keep_above(GTK_WINDOW(FWindow),setting);
  {$endif}
  FAlwaysOnTop:=setting;
end;

procedure TSdpoFastForm.BringToFront;
{$ifdef LCLGTK2}
var
  dis: PDisplay;
  win: TXID;
{$endif}
begin
  if FWindow <> nil then begin
  {$ifdef LCLGTK2}
    dis := GDK_WINDOW_XDISPLAY(FWindow^.window);
    win := GDK_WINDOW_XWINDOW(FWindow^.window);
    XRaiseWindow(dis,win);
  {$endif}
  {$ifdef LCLGTK}
     gdk_window_raise(FWindow^.Window);
  {$endif}
  end;
end;

procedure TSdpoFastForm.SetCaption(cap: string);
begin
  if FWindow <> nil then
    gtk_window_set_title(GTK_WINDOW(FWindow),Pgchar(cap));

  FCaption:=cap;
end;

procedure TSdpoFastForm.Init;
begin
  if FWindow=nil then
    AllocWindow;
  if FImage=nil then
    AllocImage;
end;

function TSdpoFastForm.GetData: pointer;
begin
  result := nil;
  try
    if FWindow=nil then
      AllocWindow;
    if FImage=nil then
      AllocImage;

    result:=FImage^.mem;
  except on
    E: exception do begin
      writeln(E.Message);
      raise;
    end;
  end;
end;

function TSdpoFastForm.GetWindow: PGtkWindow;
begin
  result := nil;
  try
    if FWindow=nil then
      AllocWindow;

    result:=GTK_WINDOW(FWindow);
  except on
    E: exception do raise;
  end;
end;

function TSdpoFastForm.GetCanvas: TSdpoFastFormCanvas;
begin
  result := nil;
  try
    if FImage=nil then
      AllocImage;

    result:=FCanvas;
  except on
    E: exception do begin
      writeln(E.Message);
      raise;
    end;
  end;
end;

procedure MotionEvent(widget: PGtkWidget; motion: PGdkEventMotion; localform: TSdpoFastForm); cdecl;
begin
  if Assigned(localform.FOnMouseMove) then begin
    localform.FOnMouseMove(localform, GetKeyShiftState, round(motion^.x), round(motion^.y));
  end;
end;

procedure ButtonPressEvent(widget: PGtkWidget; button: PGdkEventButton; localform: TSdpoFastForm); cdecl;
var
  FMouseButton: TMouseButton;
  shift: TShiftState;
begin
  if Assigned(localform.FOnMouseDown) and (button^.button<4) then begin
    shift:=GetKeyShiftState;
    // 3 button mouse - scroll button click
    case button^.button of
      1: begin FMouseButton := mbLeft; shift:=shift + [ssLeft]; end;
      2: begin FMouseButton := mbMiddle; shift:=shift + [ssMiddle]; end;
      3: begin FMouseButton := mbRight; shift:=shift + [ssRight]; end;
    end;

    localform.FOnMouseDown(localform, FMouseButton, shift, round(button^.x), round(button^.y));
  end;
end;

procedure ButtonReleaseEvent(widget: PGtkWidget; button: PGdkEventButton; localform: TSdpoFastForm); cdecl;
var
  FMouseButton: TMouseButton;
  shift: TShiftState;
begin
  if Assigned(localform.FOnMouseUp) and (button^.button<4) then begin
    shift:=GetKeyShiftState;
    // 3 button mouse - scroll button click
    case button^.button of
      1: begin FMouseButton := mbLeft; shift:=shift + [ssLeft]; end;
      2: begin FMouseButton := mbMiddle; shift:=shift + [ssMiddle]; end;
      3: begin FMouseButton := mbRight; shift:=shift + [ssRight]; end;
    end;
    localform.FOnMouseUp(localform, FMouseButton, shift, round(button^.x), round(button^.y));
  end;
end;

procedure KeyPressEvent(widget: PGtkWidget; key: PGdkEventKey; localform: TSdpoFastForm); cdecl;
begin
  if Assigned(localform.FOnKeyDown) then begin
    localform.FOnKeyDown(localform, key^.keyval, GetKeyShiftState);  // returns ASCII code and not VK
  end;
end;

procedure KeyReleaseEvent(widget: PGtkWidget; key: PGdkEventKey; localform: TSdpoFastForm); cdecl;
begin
  if Assigned(localform.FOnKeyUp) then begin
    localform.FOnKeyUp(localform, key^.keyval, GetKeyShiftState);   // returns ASCII code and not VK
  end;
end;

procedure ConfigureEvent(widget: PGtkWidget; config: PGdkEventConfigure; localform: TSdpoFastForm); cdecl;
begin
  with localform do begin
    if not FTopBarSize then begin
      gdk_window_get_position(FWindow^.window,@FWin_X,@FWin_Y);
      if (FWin_X=FPos_X) and (FWin_Y=FPos_Y) then exit;

      FWin_X:=FWin_X-FPos_X;
      FWin_Y:=FWin_Y-FPos_Y;
      FTopBarSize:=true;
    end else begin
      gdk_window_get_position(FWindow^.window,@FPos_X,@FPos_Y);
      FPos_X:=FPos_X-FWin_X;
      if FPos_X<0 then FPos_X:=0;
      FPos_Y:=FPos_Y-FWin_Y;
    end;
  end;
end;

function DeleteEvent(widget: PGtkWidget; event: PGdkEvent; localform: TSdpoFastForm): gBoolean; cdecl;
begin
  localform.SetVisible(false);
  result:=true;
end;

