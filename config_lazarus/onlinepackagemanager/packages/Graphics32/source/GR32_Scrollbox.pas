{**********************************************************************
                PilotLogic Software House.

 Package pl_Graphics32.pkg
 This unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit GR32_Scrollbox;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages,
  Classes, Controls, Messages, SysUtils,Graphics,
  Gr32, Gr32_Resamplers, 
  Forms, Math;

type

  TsdBoxPlacement = (
    bpCenter,
    bpLeftTop
  );
  
  // resampler mode
  TsdResampler =
   (rsNearest,
    rsLinear,
    rsDraft,
    rsKernel);
    
 TsdVirtualPaintEvent = procedure(Sender: TObject; ACanvas: TCanvas) of object;

  // The TsdVirtualScrollBox is a windowed control that can virtually scroll over its
  // scrollable area, indicated by ScrollWidth and ScrollHeight. The left and top
  // position of the visible part is indicated by ScrollLeft and ScrollTop. Set
  // them all together using SetScrollBounds() in order to avoid flicker
  TVirtualScrollBox = class(TCustomControl)
  private
    FAutoScroll: Boolean;    // If set, the control will automatically scroll
    FBorderStyle: TBorderStyle; // Border style for this scrollbox (bsNone or bsSingle)
    FBoxPlacement: TsdBoxPlacement; // Default placement when scrollbox is smaller than client
    FScrollLeft: integer;    // Left position of scroll window on virtual window
    FScrollTop: integer;     // Top position of scroll window on virtual window
    FScrollWidth: integer;   // Total width of scrollable area
    FScrollHeight: integer;  // Total height of scrollable area
    FScrollScale: single;    // Scale on scrolling in case Width or Height > 32767 (handled automatically)
    FTracking: boolean;      // If set (default), the window updates immediately when scrollbars are moved
    FIncrement: integer;     // Increment (in pixels) when arrows on scrollbar are clicked
    FOnUpdateScrollPosition: TNotifyEvent;
    FOnPaint: TsdVirtualPaintEvent;
    procedure SetAutoScroll(const Value: Boolean);
    procedure ScrollMessage(const AMessage: TWMHScroll; ACode: word; var APos: integer; ASize, AClient: integer);
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
    procedure WMHScroll(var Message: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure SetBorderStyle(const Value: TBorderStyle);
    function  CalculateThumbPosition(const Requested, Size, Client: integer): integer;
    procedure SetBoxPlacement(const Value: TsdBoxPlacement);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
    procedure RemoveScrollbars;
    procedure UpdateScrollbars;
    procedure UpdateScrollPosition; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ScrollBy(DeltaX, DeltaY: integer); virtual;
    // Use ClientToBox to determine the position of mouse coordinates X and Y in
    // box coordinates. If the mouse is outside the box, the function returns False.
    function  ClientToBox(X, Y: integer; var BoxX, BoxY: integer): boolean;
    procedure SetScrollBounds(ALeft, ATop, AWidth, AHeight: integer); virtual;
    property AutoScroll: Boolean read FAutoScroll write SetAutoScroll default True;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property BoxPlacement: TsdBoxPlacement read FBoxPlacement write SetBoxPlacement default bpCenter;
    property Increment: integer read FIncrement write FIncrement default 8;
    property ScrollLeft: integer read FScrollLeft;
    property ScrollTop: integer read FScrollTop;
    property ScrollWidth: integer read FScrollWidth;
    property ScrollHeight: integer read FScrollHeight;
    property Tracking: boolean read FTracking write FTracking default True;
    // Event OnUpdateScrollPosition is fired whenever the user has scrolled.
    property OnUpdateScrollPosition: TNotifyEvent read FOnUpdateScrollPosition write FOnUpdateScrollPosition;
    property OnPaint: TsdVirtualPaintEvent read FOnPaint write FOnPaint;
  end;
     
  TBuffer32Scrollbox = class(TVirtualScrollBox)
  private
    FClipRect: TRect;
    FBuffer: TBitmap32;
  protected
    procedure Paint; override;
    procedure PaintBuffer; virtual;
    procedure PaintGDIOverlay; virtual;
    property  ClipRect: TRect read FClipRect;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Buffer: TBitmap32 read FBuffer;
  end;  

  TBitmap32ScrollBox = class(TBuffer32Scrollbox)
  private
    FAllowMouseDrag: boolean;
    FAllowArrowKeys: boolean;
    FBitmap: TBitmap32;
    FScale: single;
    FResampler: TsdResampler;
    FOldDragPos: TPoint;
    procedure SetBitmap(const Value: TBitmap32);
    procedure SetScale(const Value: single);
    procedure UpdateBounds;
    procedure BitmapChange(Sender: TObject);
    procedure BitmapResize(Sender: TObject);
    procedure SetResampler(const Value: TsdResampler);
    procedure CMWantSpecialKey(var Message: TMessage); message CM_WANTSPECIALKEY;
    procedure WMLButtonDown(var Message: TLMLButtonDown); message LM_LBUTTONDOWN;
    procedure WMLButtonUp(var Message: TLMLButtonUp); message LM_LBUTTONUP;
    procedure WMMouseMove(var Message: TLMMouseMove); message LM_MOUSEMOVE;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure PaintBuffer; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ClientToBitmap(X, Y: integer; var BmpX, BmpY: integer): boolean;
    property Bitmap: TBitmap32 read FBitmap write SetBitmap;
    property ScrollLeft;
    property ScrollTop;
    property ScrollWidth;
    property ScrollHeight;
  published
    // If AllowArrowKeys is true, the arrow keys will also scroll the image (default)
    property AllowArrowKeys: boolean read FAllowArrowKeys write FAllowArrowKeys default True;
    // If AllowMouseDrag is true, the user can drag the image around with the mouse
    // inside the control (default).
    property AllowMouseDrag: boolean read FAllowMouseDrag write FAllowMouseDrag default True;
    property Scale: single read FScale write SetScale;
    property Resampler: TsdResampler read FResampler write SetResampler;
    property Align;
    property Anchors;
    property AutoScroll;
    property BorderStyle;
    property BoxPlacement;
    property Color;
    property Constraints;
    property DragCursor;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Tracking;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnKeyPress;
    property OnKeyDown;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDrag;
    property OnUnDock;
    property OnUpdateScrollPosition;
  end;

implementation


{ TBitmap32ScrollBox }

procedure TBitmap32ScrollBox.BitmapChange(Sender: TObject);
begin
  invalidate;
end;

procedure TBitmap32ScrollBox.BitmapResize(Sender: TObject);
begin
  UpdateBounds;
end;

function TBitmap32ScrollBox.ClientToBitmap(X, Y: integer; var BmpX,
  BmpY: integer): boolean;
begin
  Result := ClientToBox(X, Y, BmpX, BmpY);
  if Scale <= 0 then
    exit;
  BmpX := round(BmpX / Scale);
  BmpY := round(BmpY / Scale);
end;

procedure TBitmap32ScrollBox.CMWantSpecialKey(var Message: TMessage);
begin
  Message.Result := 1;
end;

constructor TBitmap32ScrollBox.Create(AOwner: TComponent);
begin
  inherited;
  FBitmap := TBitmap32.Create;
  FBitmap.OnChange := BitmapChange;
  FBitmap.OnResize := BitmapResize;
  FAllowArrowKeys := True;
  FAllowMouseDrag := True;
  FScale := 1.0;
  FResampler := rsNearest;
end;

destructor TBitmap32ScrollBox.Destroy;
begin
  FBitmap.OnChange := nil;
  FBitmap.OnResize := nil;
  FreeAndNil(FBitmap);
  inherited;
end;

procedure TBitmap32ScrollBox.KeyDown(var Key: Word; Shift: TShiftState);
var
  DeltaX, DeltaY: integer;
begin
  if FAllowArrowKeys then
  begin
    DeltaX := 0;
    DeltaY := 0;
    case Key of
    VK_LEFT:  DeltaX := -Increment;
    VK_RIGHT: DeltaX :=  Increment;
    VK_UP:    DeltaY := -Increment;
    VK_DOWN:  DeltaY :=  Increment;
    end;//case
    if (DeltaX <> 0) or (DeltaY <> 0) then
      ScrollBy(DeltaX, DeltaY);
  end;
  inherited;
end;

procedure TBitmap32ScrollBox.PaintBuffer;
var
  DR: TRect;
  Resampler: TCustomResampler;
begin
  inherited;

  // Destination rectangle
  DR := Rect(0, 0, ScrollWidth, ScrollHeight);
  OffsetRect(DR, -ScrollLeft, -ScrollTop);

  // select resampler
  case FResampler of
  rsNearest: Resampler := TNearestResampler.Create;
  rsLinear:  Resampler := TLinearResampler.Create;
  rsDraft:   Resampler := TDraftResampler.Create;
  rsKernel:  Resampler := TKernelResampler.Create;
  else
    Resampler := TLinearResampler.Create;
  end;
  try
    // StretchTransfer to buffer
    StretchTransfer(Buffer, DR, ClipRect, FBitmap, FBitmap.BoundsRect,
      Resampler, dmOpaque);
  finally
    Resampler.Free;
  end;
end;

procedure TBitmap32ScrollBox.SetBitmap(const Value: TBitmap32);
begin
  FBitmap.Assign(Value);
  UpdateBounds;
  if not (csDesigning in ComponentState) then
    SetFocus;
end;

procedure TBitmap32ScrollBox.SetScale(const Value: single);
var
  MidX, MidY, NewLeft, NewTop: integer;
begin
  if FScale <> Value then
  begin
    // Make sure to scale from the middle of the control
    MidX := Width div 2;
    MidY := Height div 2;
    NewLeft := round((ScrollLeft + MidX) * (Value / FScale) - MidX);
    NewTop  := round((ScrollTop  + MidY) * (Value / FScale) - MidY);
    FScale := Value;
    SetScrollBounds(NewLeft, NewTop, ceil(FBitmap.Width * FScale), ceil(FBitmap.Height * FScale));
    Invalidate;
  end;
end;

procedure TBitmap32ScrollBox.SetResampler(
  const Value: TsdResampler);
begin
  if FResampler <> Value then
  begin
    FResampler := Value;
    Invalidate;
  end;
end;

procedure TBitmap32ScrollBox.UpdateBounds;
begin
  SetScrollBounds(ScrollLeft, ScrollTop, ceil(FBitmap.Width * FScale), ceil(FBitmap.Height * FScale));
  Invalidate;
end;

procedure TBitmap32ScrollBox.WMLButtonDown(var Message: TLMLButtonDown);
begin
  inherited;
  SetFocus;
  if not FAllowMouseDrag then
    exit;
  FOldDragPos.X := Message.XPos;
  FOldDragPos.Y := Message.YPos;
  Screen.Cursor := crHandPoint;
end;

procedure TBitmap32ScrollBox.WMLButtonUp(var Message: TLMLButtonUp);
var
  DeltaX, DeltaY: integer;
begin
  inherited;
  if not FAllowMouseDrag then
    exit;
  DeltaX := round(FOldDragPos.X - Message.XPos);
  DeltaY := round(FOldDragPos.Y - Message.YPos);
  ScrollBy(DeltaX, DeltaY);
  Screen.Cursor := crDefault;
end;

procedure TBitmap32ScrollBox.WMMouseMove(var Message: TLMMouseMove);
var
  DeltaX, DeltaY: integer;
begin
  inherited;
  if not FAllowMouseDrag then
    exit;
  // Left mouse button down?
  if not (Message.Keys AND MK_LBUTTON <> 0) then
    exit;

  DeltaX := round(FOldDragPos.X - Message.XPos);
  DeltaY := round(FOldDragPos.Y - Message.YPos);
  ScrollBy(DeltaX, DeltaY);
  FOldDragPos.X := Message.XPos;
  FOldDragPos.Y := Message.YPos;
end;

{ TBuffer32Scrollbox }

constructor TBuffer32Scrollbox.Create(AOwner: TComponent);
begin
  inherited;
  FBuffer := TBitmap32.Create;
end;

destructor TBuffer32Scrollbox.Destroy;
begin
  FreeAndNil(FBuffer);
  inherited;
end;

procedure TBuffer32Scrollbox.Paint;
begin
  // Check buffer size
  if (FBuffer.Width <> Width) or (FBuffer.Height <> Height) then
    FBuffer.SetSize(Width, Height);

  // Clipping rectangle
  FClipRect := Canvas.ClipRect;
  if IsRectEmpty(FClipRect) then
    exit;

  // Paint the buffer
  PaintBuffer;

  // Copy buffer to the control's canvas
  BitBlt(
    Canvas.Handle,
    FClipRect.Left, FClipRect.Top,
    FClipRect.Right - FClipRect.Left, FClipRect.Bottom - FClipRect.Top,
    FBuffer.Handle,
    FClipRect.Left, FClipRect.Top, SRCCOPY);

  // Paint any GDI
  PaintGDIOverlay;
end;

procedure TBuffer32Scrollbox.PaintBuffer;
// Default paints the background around scrollbox on the buffer.
// Override this method in descendants. Call "inherited" if you want to automatically
// clear the area that is outside of the scrollbox.
var
  R, B, Dest: TRect;
  BackgroundColor: TColor32;
// local
procedure PaintRect;
begin
  if IsRectEmpty(R) then
    exit;
  IntersectRect(Dest, R, ClipRect);
  if IsRectEmpty(Dest) then
    exit;
  FBuffer.FillRect(Dest.Left, Dest.Top, Dest.Right, Dest.Bottom, BackgroundColor);
end;
// main
begin
  BackgroundColor := Color32(Color);
  B := Rect(0, 0, ScrollWidth, ScrollHeight);
  OffsetRect(B, -ScrollLeft, -ScrollTop);
  R := Rect(0, 0, ClientWidth, B.Top);
  PaintRect;
  R := Rect(0, B.Top, B.Left, B.Bottom);
  PaintRect;
  R := Rect(B.Right, B.Top, ClientWidth, B.Bottom);
  PaintRect;
  R := Rect(0, B.Bottom, ClientWidth, ClientHeight);
  PaintRect;
end;

procedure TBuffer32Scrollbox.PaintGDIOverlay;
begin
// default does nothing
end;


{ TVirtualScrollBox }

function TVirtualScrollBox.CalculateThumbPosition(const Requested, Size, Client: integer): integer;
var
  OverShoot: integer;
begin
  if FBoxPlacement = bpCenter then
  begin
    if Size = 0 then
      OverShoot := 0
    else
      OverShoot := Max(0, Client - Size);
  end else
    OverShoot := 0;
  Result := Max(-OverShoot div 2, Min(Requested, Size - Client));
end;

function TVirtualScrollBox.ClientToBox(X, Y: integer; var BoxX, BoxY: integer): boolean;
begin
  BoxX := 0;
  BoxY := 0;
  Result := False;
  if (FScrollWidth <= 0) or (FScrollHeight <= 0) then
    exit;
  BoxX := X + FScrollLeft;
  BoxY := Y + FScrollTop;
  if (BoxX >= 0) and (BoxX < FScrollWidth) and (BoxY >= 0) and (BoxY < FScrollHeight) then
    Result := True;
end;

constructor TVirtualScrollBox.Create(AOwner: TComponent);
begin
  inherited;
  FAutoScroll := True;
  FIncrement := 9;
  FScrollScale  := 1.0;
  FTracking := True;
  Color := clAppWorkspace;
  Width := 150;
  Height := 250;
  FBorderStyle := bsSingle;
end;

procedure TVirtualScrollBox.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    with WindowClass do
      style := style and not (CS_HREDRAW or CS_VREDRAW);
    Style := Style or BorderStyles[FBorderStyle];
    if NewStyleControls and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
  end;
end;

procedure TVirtualScrollBox.Paint;
// Override this method in descendants. Call "inherited" if you want to automatically
// clear the area that is outside of the scrollbox
var
  R, B, C, Dest: TRect;
  // local
  procedure PaintRect;
  begin
    if IsRectEmpty(R) then
      exit;
    IntersectRect(Dest, R, C);
    if IsRectEmpty(Dest) then
      exit;
    Canvas.FillRect(Dest);
  end;
// main
begin
  // Onpaint event
  if assigned(FOnPaint) then
    FOnPaint(Self, Canvas);
  // Paint area around scrolled area (if any is visible)
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := Color;
  C := Canvas.ClipRect;
  B := Rect(0, 0, FScrollWidth, FScrollHeight);
  OffsetRect(B, -FScrollLeft, -FScrollTop);
  R := Rect(0, 0, ClientWidth, B.Top);
  PaintRect;
  R := Rect(0, B.Top, B.Left, B.Bottom);
  PaintRect;
  R := Rect(B.Right, B.Top, ClientWidth, B.Bottom);
  PaintRect;
  R := Rect(0, B.Bottom, ClientWidth, ClientHeight);
  PaintRect;
end;

procedure TVirtualScrollBox.RemoveScrollbars;
var
  ScrollInfo: TScrollInfo;
begin
  if not HandleAllocated then
    exit;
  // Horizontal scrollbar
  FillChar(ScrollInfo, SizeOf(ScrollInfo), 0);
  ScrollInfo.cbSize := SizeOf(ScrollInfo);
  ScrollInfo.fMask := SIF_ALL;
  SetScrollInfo(Handle, SB_HORZ, ScrollInfo, True);
  // Vertical scrollbar
  SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
end;

procedure TVirtualScrollBox.ScrollBy(DeltaX, DeltaY: integer);
// Call this procedure to scroll the window and update the scrollbars, all in
// one command
var
  NewX, NewY: integer;
  ThumbPosX, ThumbPosY: integer;
begin
  // Calculate new position in X and Y
  NewX := CalculateThumbPosition(FScrollLeft + DeltaX, FScrollWidth, ClientWidth);
  DeltaX := NewX - FScrollLeft;
  NewY := CalculateThumbPosition(FScrollTop  + DeltaY, FScrollHeight, ClientHeight);
  DeltaY := NewY - FScrollTop;
  if (DeltaX = 0) and (DeltaY = 0) then
    exit; // no changes

  FScrollLeft := NewX;
  FScrollTop  := newY;
  UpdateScrollPosition;

  // Scroll the window
  ScrollWindow(Handle, -DeltaX, -DeltaY, nil, nil);

  // Set scrollbar positions
  ThumbPosX := round(NewX * FScrollScale);
  ThumbPosY := round(NewY * FScrollScale);
  if GetScrollPos(Handle, SB_HORZ) <> ThumbPosX then
    SetScrollPos(Handle, SB_HORZ, ThumbPosX, True);
  if GetScrollPos(Handle, SB_VERT) <> ThumbPosY then
    SetScrollPos(Handle, SB_VERT, ThumbPosY, True);
end;

procedure TVirtualScrollBox.ScrollMessage(const AMessage: TWMHScroll; ACode: word;
  var APos: integer; ASize, AClient: integer);
  // local
  procedure SetPosition(NewPos: single);
  var
    ANewPos: single;
    ADelta: integer;
    AIntPos: integer;
  begin
//    DoDebugOut(Self, wsInfo, 'ScrollMessage.SetPosition called');
    // Calculate new position
    ANewPos := Min(Max(0, NewPos), Max(0, ASize - AClient));
    ADelta := round(ANewPos - APos);
    if ADelta = 0 then
      exit; // no changes

    APos := round(ANewPos);
    UpdateScrollPosition;

    // Scroll the window
    case ACode of
    SB_HORZ: ScrollWindow(Handle, -ADelta, 0, nil, nil);
    SB_VERT: ScrollWindow(Handle, 0, -ADelta, nil, nil);
    end;//case

    // Set scrollbar position
    AIntPos := round(NewPos * FScrollScale);
    if GetScrollPos(Handle, ACode) <> AIntPos then
      SetScrollPos(Handle, ACode, AIntPos, True);
  end;
// main
begin
  if not AutoScroll then
    exit;
  with AMessage do
  begin
    case ScrollCode of
    SB_LINEUP:        SetPosition(APos - Increment);
    SB_LINEDOWN:      SetPosition(APos + Increment);
    SB_PAGEUP:        SetPosition(APos - AClient);
    SB_PAGEDOWN:      SetPosition(APos + AClient);
    SB_THUMBPOSITION: SetPosition(Pos / FScrollScale);
    SB_THUMBTRACK:    if Tracking then
                        SetPosition(Pos / FScrollScale);
    SB_TOP:           SetPosition(0);
    SB_BOTTOM:        SetPosition(ASize - AClient);
    SB_ENDSCROLL:     ;
    end;//case
  end;
end;

procedure TVirtualScrollBox.SetAutoScroll(const Value: Boolean);
begin
  if FAutoScroll <> Value then
  begin
    FAutoScroll := Value;
    if Value then
      UpdateScrollBars
    else
    begin
      RemoveScrollbars;
      FScrollLeft := 0;
      FScrollTop  := 0;
    end;
  end;
end;

procedure TVirtualScrollBox.SetBorderStyle(const Value: TBorderStyle);
begin
  if Value <> FBorderStyle then
  begin
    FBorderStyle := Value;
    self.Invalidate;// RecreateWnd;  ct9999
  end;
end;

procedure TVirtualScrollBox.SetBoxPlacement(
  const Value: TsdBoxPlacement);
begin
  if FBoxPlacement <> Value then
  begin
    FBoxPlacement := Value;
    UpdateScrollBars;
  end;
end;

procedure TVirtualScrollBox.SetScrollBounds(ALeft, ATop, AWidth, AHeight: integer);
begin
  if (FScrollLeft <> ALeft) or (FScrollTop <> ATop) or
     (FScrollWidth <> AWidth) or (FScrollHeight <> AHeight) then
  begin
    if (FScrollLeft <> ALeft) or (FScrollTop <> ATop) then
    begin
      FScrollLeft   := ALeft;
      FScrollTop    := ATop;
      UpdateScrollPosition;
    end;
    FScrollWidth  := AWidth;
    FScrollHeight := AHeight;
    UpdateScrollbars;
  end;
end;

procedure TVirtualScrollBox.UpdateScrollbars;
var
  ScrollInfo: TScrollInfo;
  AMax: integer;
  AScrollLeft, AScrollTop: integer;
begin
  if not HandleAllocated then
    exit;
  // Adjust scale
  AMax := Max(FScrollWidth, FScrollHeight);
  if AMax > 30000 then
    FScrollScale := 30000 / AMax
  else
    FScrollScale := 1.0;

  // Check limits on Pos
  AScrollLeft := CalculateThumbPosition(FScrollLeft, FScrollWidth, ClientWidth);
  AScrollTop := CalculateThumbPosition(FScrollTop, FScrollHeight, ClientHeight);
  if (AScrollLeft <> FScrollLeft) or (AScrollTop <> FScrollTop) then
  begin
    FScrollLeft := AScrollLeft;
    FScrollTop  := AScrollTop;
    UpdateScrollPosition;
    // We need an extra invalidate here, the standard WinControl seems to
    // forget this case
    Invalidate;
  end;
  if not AutoScroll then
    exit;

  // Horizontal scrollbar
  ScrollInfo.cbSize := SizeOf(ScrollInfo);
  ScrollInfo.fMask := SIF_ALL;
  ScrollInfo.nMin  := 0;
  ScrollInfo.nMax  := round(FScrollWidth * FScrollScale);
  ScrollInfo.nPage := round(ClientWidth  * FScrollScale);
  ScrollInfo.nPos  := round(FScrollLeft  * FScrollScale);
  ScrollInfo.nTrackPos := ScrollInfo.nPos;
  SetScrollInfo(Handle, SB_HORZ, ScrollInfo, True);

  // Vertical scrollbar
  ScrollInfo.nMin  := 0;
  ScrollInfo.nMax  := round(FScrollHeight * FScrollScale);
  ScrollInfo.nPage := round(ClientHeight  * FScrollScale);
  ScrollInfo.nPos  := round(FScrollTop    * FScrollScale);
  ScrollInfo.nTrackPos := ScrollInfo.nPos;
  SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
end;

procedure TVirtualScrollBox.UpdateScrollPosition;
// Override in descendants to update a label that displays scroll position etc
begin
  // Default fires event
  if assigned(FOnUpdateScrollPosition) then
    FOnUpdateScrollPosition(Self);
end;

procedure TVirtualScrollBox.WMEraseBkgnd(var Message: TLMEraseBkgnd);
// This message handler is called when windows is about to work on the background
// of the window, and this procedure signals not to "erase" (or fill) it, to
// avoid flicker
begin
  // No automatic erase of background
  Message.Result := LRESULT(False);
end;

procedure TVirtualScrollBox.WMHScroll(var Message: TWMHScroll);
begin
  ScrollMessage(Message, SB_HORZ, FScrollLeft, FScrollWidth, ClientWidth);
end;

procedure TVirtualScrollBox.WMSize(var Message: TLMSize);
// React to a resize
begin
  // use the info to update the scrollbars
  UpdateScrollbars;
  // and call inherited method
  inherited;
end;

procedure TVirtualScrollBox.WMVScroll(var Message: TWMVScroll);
begin
  ScrollMessage(Message, SB_VERT, FScrollTop, FScrollHeight, ClientHeight);
end;

end.


