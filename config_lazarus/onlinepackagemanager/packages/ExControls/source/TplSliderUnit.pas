
{**********************************************************************
 Package pl_ExControls.pkg
 From PilotLogic Software House(http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit TplSliderUnit;


interface

uses
    LCLIntf, LCLType, LMessages,
    Controls, ExtCtrls, Classes, Graphics, Messages, plUtils;

type

  TSliderOrientation = (soxHorizontal, soxVertical);
  TSliderOption = (soShowFocus, soShowPoints, soSmooth);
  TSliderOptions = set of TSliderOption;

  TJumpMode = (jmNone, jmHome, jmEnd, jmNext, jmPrior);

 TplSliderThumb = class;   

 TplSlider = class(TCustomControl)
  private
    FThumb: TplSliderThumb;
    FPointsRect: TRect;
    FOrientation: TSliderOrientation;
    FOptions: TSliderOptions;
    FCurrentlySeeking : Boolean;
    FMinValue: Longint;
    FMaxValue: Longint;
    FIncrement: Longint;
    FValue: Longint;
    FHit: Integer;
    FActive: Boolean;
    FSliding: Boolean;
    FTracking: Boolean;
    FTimerActive: Boolean;
    FMousePos: TPoint;
    FStartJump: TJumpMode;
    FOnChange: TNotifyEvent;
    FOnDrawPoints: TNotifyEvent;
    FOnStopTracking : TNotifyEvent;
    FEdgeSize: Integer;
    fNumThumbStates: Integer;
    procedure SetOrientation(Value: TSliderOrientation);
    procedure SetOptions(Value: TSliderOptions);
    procedure SetMinValue(Value: Longint);
    procedure SetMaxValue(Value: Longint);
    procedure SetIncrement(Value: Longint);
    function  GetThumbOffset: Integer;
    procedure SetThumbOffset(Value: Integer);
    procedure SetValue(Value: Longint);
    procedure ThumbJump(Jump: TJumpMode);
    function  JumpTo(X, Y: Integer): TJumpMode;
    procedure StopTracking;
    procedure TimerTrack;
    procedure CreateElements;
    procedure AdjustElements;
    procedure ImageChanged;
    function  GetValueByOffset(Offset: Integer): Longint;
    function  GetOffsetByValue(Value: Longint): Integer;
    function  GetRulerLength: Integer;
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
    procedure WMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
  //  procedure WMTimer(var Message: TMessage); message WM_TIMER;
    property  ThumbOffset: Integer read GetThumbOffset write SetThumbOffset;
  protected
    procedure Change; dynamic;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure ThumbMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure ThumbMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer); virtual;
    procedure ThumbMouseUp(Sender: TObject; Thumb: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DefaultDrawPoints(PointsStep, PointsHeight, ExtremePointsHeight: Integer); virtual;
    property Canvas;
  published
    property EdgeSize: Integer read FEdgeSize write fEdgeSize;  //Is Face
    property NumThumbStates: Integer read FNumThumbStates write fNumThumbStates;  //Is Face
    property CurrentlySeeking: Boolean read FCurrentlySeeking;
    property Orientation: TSliderOrientation read FOrientation write SetOrientation default soxHorizontal;
    property Options: TSliderOptions read FOptions write SetOptions;
    property Increment: Longint read FIncrement write SetIncrement;
    property MinValue: Longint read FMinValue write SetMinValue;
    property MaxValue: Longint read FMaxValue write SetMaxValue;
    property Value: Longint read FValue write SetValue;
    property Color;
    property Align;
    property Visible;
    property Enabled; 
    property Cursor;
    property DragMode;
    property DragCursor;
    property ParentColor;
    property ParentShowHint;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDrawPoints: TNotifyEvent read FOnDrawPoints write FOnDrawPoints;
    property OnStopTracking : TNotifyEvent read FOnStopTracking write FOnStopTracking;
    property OnClick;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
    property OnDragOver;
    property OnDragDrop;
    property OnEndDrag;
    property OnStartDrag;
  end;

{ TplSliderThumb }

  TplSliderThumb = class(TCustomControl)
  private
    FDown: Boolean;
    FOrientation: TSliderOrientation;
    procedure SetDown(Value: Boolean);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;  
    property Down: Boolean read FDown write SetDown default False;
    property Orientation: TSliderOrientation read FOrientation write FOrientation;
  end;


implementation
 
{$R TplSliderUnit.res}

uses Forms, SysUtils;
const
  JumpInterval = 10;
var
  imgH,
  imgHD,
  imgV,
  ImgVD:TBitmap; 

//================================ TplSliderThumb =============================================
constructor TplSliderThumb.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csCaptureMouse];
  FDown := False;
  FOrientation:=soxHorizontal;
end;

destructor TplSliderThumb.Destroy;
begin
  inherited Destroy;
end;

procedure TplSliderThumb.Paint;
var  R: TRect;
     b:TBitmap;
     xToC:TColor;
begin

 if fDown then
     xToC:=clLime else
     xToC:=$00FF8080;//clFuchsia;//clInactiveCaption;

  GradientFillRect(Canvas,ClientRect,clWhite,xToC,fdTopToBottom,32);
  Frame3DBorder(canvas, ClientRect, DefiBtnColorHighlight, clBlack, 1);  
  
  //...........................................
  {
  R := Rect(0, 0, Width-1, Height-1);
  try
    if FOrientation=soxHorizontal then
    begin
      if fDown then
         b:=imgHD else
         b:=imgH;
    end else
    begin
      if fDown then
         b:=imgVD else
         b:=imgV;
    end;
    DrawBitmapTransparent(Canvas,0,0,b,clFuchsia);
  finally
  end;
  //..............................................
        }
end;

procedure TplSliderThumb.SetDown(Value: Boolean);
begin
  if FDown <> Value then begin
    FDown := Value;
    Invalidate;
  end;
end;

//================================ TSlider =============================================

constructor TplSlider.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csClickEvents, csCaptureMouse];
  ControlState := ControlState + [csCreating];
  Width := 150;
  Height := 17;
  FOrientation := soxHorizontal;
  FOptions := [soShowFocus, soSmooth];

  FMinValue := 0;
  FMaxValue := 100;
  FIncrement := 1;
  TabStop := True;
  CreateElements;
  ControlState := ControlState - [csCreating];
  ParentColor:=True;
end;

destructor TplSlider.Destroy;
begin
  inherited Destroy;
end;

procedure TplSlider.Paint;
var R: TRect;
begin
  R := ClientRect;

  Canvas.Pen.Color:=clBlack;
  Canvas.Pen.Width:=1;

  if (soShowFocus in Options) and FActive and
    not (csDesigning in ComponentState) then
     begin
       Canvas.DrawFocusRect(ClientRect);
       Canvas.Brush.Color:=DefiColorFocused;
     end else
     begin
       Canvas.Brush.Color:=DefiColorUnFocused;
     end;


 if Self.Orientation=soxVertical then
   begin
     R:=Rect(R.Left+4,R.Top+2,R.left+10,R.Bottom-2);
     Canvas.FillRect(r);
     Frame3DBorder(Canvas,r,clgray,clWhite,1);
   end else
   begin
     R:=Rect(R.Left+2,R.Top+4,R.Right-2,R.top+10);
     Canvas.FillRect(r);
     Frame3DBorder(Canvas,r,clgray,clWhite,1);
   end;


  if (soShowPoints in Options) then
   begin
    if Assigned(FOnDrawPoints) then
        FOnDrawPoints(Self) else
        DefaultDrawPoints(Increment, 3, 4);
   end;
end;

procedure TplSlider.AdjustElements;
var SaveValue: Longint;
begin
  SaveValue := Value;

  if Orientation = soxHorizontal then
  begin
      FThumb.SetBounds(1, 1,imgH.Width,imgH.Height);
      FPointsRect := Rect(2, 11, Width-2, Height-4);
  end else
  begin
      FThumb.SetBounds(1,1,imgV.Width,imgV.Height);
      FPointsRect := Rect(11, 2, Width-8, Height-2);
  end;
  Value := SaveValue;
end;

procedure TplSlider.DefaultDrawPoints(PointsStep, PointsHeight, ExtremePointsHeight: Integer);
const
  MinInterval = 3;
var
  RulerLength: Integer;
  Interval, Scale, PointsCnt, X, H: Integer;
  X1, X2, Y1, Y2: Integer;
  I: Longint;
begin
  RulerLength := GetRulerLength;
  Scale := 0;
  repeat
    Inc(Scale);
    PointsCnt := (MaxValue - MinValue) div (Scale * PointsStep) + 1;
    if PointsCnt > 1 then
      Interval := (RulerLength - PointsCnt) div (PointsCnt - 1) else
      Interval := RulerLength;
  until (Interval >= MinInterval) or (Interval = RulerLength);

  I := MinValue;
  while not (I > MaxValue) do
  begin
    H := PointsHeight;
    if (I = MinValue) or (I = MaxValue) then H := ExtremePointsHeight;
    X := GetOffsetByValue(I);
    if Orientation = soxHorizontal then
    begin
      X1 := X + FThumb.Width div 2;
      Y1 := FPointsRect.Top;
      X2 := X1 + 1;
      Y2 := Y1 + H;
    end else
    begin
      X1 := FPointsRect.Left;
      Y1 := X + FThumb.Height div 2;
      X2 := X1 + H;
      Y2 := Y1 + 1;
    end;
    //.... draw .....................
    Canvas.Pen.Color:=clGray;
    Canvas.Pen.Width:=1;
    Canvas.Rectangle(X1, Y1, X2, Y2);
    Inc(I, Scale * PointsStep);
  end;
end;

procedure TplSlider.CreateElements;
var
  I: Integer;
begin
  FThumb := TplSliderThumb.Create(Self);
  with FThumb do
  begin
    Parent := Self;
    OnMouseDown := @ThumbMouseDown;
    OnMouseMove := @ThumbMouseMove;
    OnMouseUp := @ThumbMouseUp;
  end;

  AdjustElements;
end;


procedure TplSlider.ImageChanged;
begin
  AdjustElements;
  Invalidate;
end;

procedure TplSlider.Loaded;
var  I : Integer;
begin
  inherited Loaded;
end;

procedure TplSlider.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;


procedure TplSlider.SetOrientation(Value: TSliderOrientation);
var  iw,ih: Integer;
begin
  if Orientation <> Value then
  begin
    FOrientation := Value;
    FThumb.Orientation := FOrientation;
    AdjustElements;
    Invalidate;
  end;
end;

procedure TplSlider.SetOptions(Value: TSliderOptions);
begin
  if Value <> FOptions then begin
    FOptions := Value;
    Invalidate;
  end;
end;

procedure TplSlider.SetMinValue(Value: Longint);
begin
  if FMinValue <> Value then begin
    if Value <= MaxValue - Increment then begin
      FMinValue := Value;
      if (soShowPoints in Options) then Invalidate;
    end;
  end;
end;

procedure TplSlider.SetMaxValue(Value: Longint);
begin
  if FMaxValue <> Value then begin
    if Value >= MinValue + Increment then begin
      FMaxValue := Value;
      if (soShowPoints in Options) then Invalidate;
    end;
  end;
end;

procedure TplSlider.SetIncrement(Value: Longint);
begin
  if (Value > 0) and (FIncrement <> Value) then begin
    FIncrement := Value;
    Self.Value := FValue;
    Invalidate;
  end;
end;

function TplSlider.GetValueByOffset(Offset: Integer): Longint;
begin
  if Orientation = soxVertical then Offset := ClientHeight - Offset - FThumb.Height;
  Result := Round((Offset - 1) * (MaxValue - MinValue) / GetRulerLength);
  if not (soSmooth in Options) then
    Result := (Result div Increment) * Increment;
  Result := MinValue + Result;
end;

function TplSlider.GetOffsetByValue(Value: Longint): Integer;
begin
  Result := Round((Value - MinValue) * GetRulerLength /
    (MaxValue - MinValue)) + 1;
  if Orientation = soxVertical then Result := ClientHeight - Result - FThumb.Height;
end;

function TplSlider.GetThumbOffset: Integer;
begin
  if Orientation = soxHorizontal then Result := FThumb.Left
  else Result := FThumb.Top;
end;

procedure TplSlider.SetThumbOffset(Value: Integer);
var
  RulerLength: Integer;
  ValueBefore: Longint;
begin
  ValueBefore := FValue;
  RulerLength := GetRulerLength;
  if Value < 1 then Value := 1
  else if Value > 1 + RulerLength then
    Value := 1 + RulerLength;
  if not (soSmooth in Options) then
    Value := GetOffsetByValue(GetValueByOffset(Value));
  if Orientation = soxHorizontal then FThumb.Left := Value
  else FThumb.Top := Value;
  if FSliding then begin
    FValue := GetValueByOffset(Value);
    if ValueBefore <> FValue then Change;
  end;
end;

function TplSlider.GetRulerLength: Integer;
begin
  if Orientation = soxHorizontal then
  begin
    Result := Width-1;
    Dec(Result, FThumb.Width);
  end
  else
  begin
    Result := Height-1;
    Dec(Result, FThumb.Height);
  end;
end;

procedure TplSlider.SetValue(Value: Longint);
var
  xChanged: Boolean;
begin
 If FCurrentlySeeking=FALSE then
 begin
  if Value > MaxValue then Value := MaxValue;
  if Value < MinValue then Value := MinValue;
  xChanged := FValue <> Value;
  FValue := Value;
  ThumbOffset := GetOffsetByValue(Value);
  if xChanged and Assigned(FOnChange) then FOnChange(Self);
 end;
end;

procedure TplSlider.ThumbJump(Jump: TJumpMode);
begin
  if Jump <> jmNone then begin
    case Jump of
      jmHome: Value := MinValue;
      jmPrior: Value := ((Value div Increment) * Increment) - Increment;
      jmNext: Value := ((Value div Increment) * Increment) + Increment;
      jmEnd: Value := MaxValue;
    end;
  end;
end;

function TplSlider.JumpTo(X, Y: Integer): TJumpMode;
begin
  Result := jmNone;
  if (Orientation = soxHorizontal) then begin
    if (FThumb.Left > X) then Result := jmPrior
    else if (FThumb.Left + FThumb.Width < X) then Result := jmNext;
  end
  else if (Orientation = soxVertical) then begin
    if (FThumb.Top > Y) then Result := jmNext
    else if (FThumb.Top + FThumb.Height < Y) then Result := jmPrior;
  end;
end;
       {
procedure TplSlider.WMTimer(var Message: TMessage);
begin
  TimerTrack;
end;   }

procedure TplSlider.WMSetFocus(var Message: TLMSetFocus);
var
  Active: Boolean;
begin
  with Message do Active := true;
  if Active <> FActive then begin
    FActive := Active;
    if (soShowFocus in Options) then Invalidate;
  end;
  inherited;
end;

procedure TplSlider.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  Msg.Result := DLGC_WANTARROWS;
end;

procedure TplSlider.WMSize(var Message: TWMSize);
begin
  inherited;
  if not (csReading in ComponentState) then ImageChanged;
end;

procedure TplSlider.StopTracking;
begin
  if FTracking then begin
    if FTimerActive then begin
      KillTimer(Handle, 1);
      FTimerActive := False;
    end;
    FTracking := False;
    MouseCapture := False;
  end;
end;

procedure TplSlider.TimerTrack;
var
  Jump: TJumpMode;
begin
  Jump := JumpTo(FMousePos.X, FMousePos.Y);
  if Jump = FStartJump then begin
    ThumbJump(Jump);
    if not FTimerActive then begin
      SetTimer(Handle, 1, JumpInterval, nil);
      FTimerActive := True;
    end;
  end;
end;

procedure TplSlider.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) and not (ssDouble in Shift) then begin
    SetFocus;
    MouseCapture := True;
    FTracking := True;
    FMousePos := Point(X, Y);
    FStartJump := JumpTo(X, Y);
    TimerTrack;
  end;
end;

procedure TplSlider.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if FTracking then FMousePos := Point(X, Y);
  inherited MouseMove(Shift, X, Y);
end;

procedure TplSlider.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  StopTracking;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TplSlider.KeyDown(var Key: Word; Shift: TShiftState);
var
  Jump: TJumpMode;
begin
  Jump := jmNone;
  if Shift = [] then begin
    if Key = VK_HOME then Jump := jmHome
    else if Key = VK_END then Jump := jmEnd;
    if Orientation = soxHorizontal then begin
      if Key = VK_LEFT then Jump := jmPrior
      else if Key = VK_RIGHT then Jump := jmNext;
    end
    else begin
      if Key = VK_UP then Jump := jmNext
      else if Key = VK_DOWN then Jump := jmPrior;
    end;
  end;
  if Jump <> jmNone then
  begin
    Key := 0;
    ThumbJump(Jump);
  end;
  inherited KeyDown(Key, Shift);
end;

procedure TplSlider.ThumbMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SetFocus;
  if Button = mbLeft then
  begin
    if Orientation = soxHorizontal
       then FHit := X
       else FHit := Y;
    if Orientation = soxHorizontal then
    begin

     ImageChanged;
    end else
    begin

     ImageChanged;
    end;
    FCurrentlySeeking:=TRUE;
    FSliding := True;
    FThumb.Down := True;
  end;
end;

var Offset: Integer;

procedure TplSlider.ThumbMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
Var P: TPoint;
begin
  P := ScreenToClient(FThumb.ClientToScreen(Point(X, Y)));
  if csLButtonDown in FThumb.ControlState then
  begin
    if Orientation = soxHorizontal
       then Offset := P.X
       else Offset := P.Y;
    Dec(Offset, FHit);
    ThumbOffset := Offset;
  end;
end;

procedure TplSlider.ThumbMouseUp(Sender: TObject; Thumb: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

  if Assigned(FOnStopTracking) then FOnStopTracking(Self);
  FCurrentlySeeking:=False;
  FSliding := False;
  FThumb.Down := False;
end;

//=====================================================
procedure ImagesGreate;
 begin
 if imgH=nil then
  begin
   imgH:=TBitmap.Create;
   imgH.LoadFromResourceName(hInstance,'TplSlider_HTB');
  end;

 if imgHD=nil then
  begin
   imgHD:=TBitmap.Create;
   imgHD.LoadFromResourceName(hInstance,'TplSlider_HTBP');
  end;

 if imgV=nil then
  begin
   imgV:=TBitmap.Create;
   imgV.LoadFromResourceName(hInstance,'TplSlider_VTB');
  end;

  if ImgVD=nil then
   begin
    ImgVD:=TBitmap.Create;
    ImgVD.LoadFromResourceName(hInstance,'TplSlider_VTBP');
   end;

 end;

procedure ImagesFree;
 begin
   if imgH<> nil then begin imgH.Free;  imgH:=nil;  end;
   if imgHD<>nil then begin imgHD.Free; imgHD:=nil; end;
   if imgV<> nil then begin imgV.Free;  ImgV:=nil;  end;
   if ImgVD<>nil then begin ImgVD.Free; ImgVD:=nil; end;
 end;

initialization
  ImagesGreate;
finalization
  ImagesFree;
end.
