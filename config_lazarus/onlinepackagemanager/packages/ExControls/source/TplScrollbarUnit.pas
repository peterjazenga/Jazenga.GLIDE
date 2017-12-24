unit TplScrollbarUnit;

interface

uses
  LCLIntf, LCLType, LMessages, Messages,
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, TplButtonUnit, plUtils;


{ TplScrollbarThumb }

type
  TplScrollbarThumb = class(TplButton)
  private
    FDown: Boolean;
    FOldX, FOldY: Integer;
    FTopLimit: Integer;
    FBottomLimit: Integer;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Color;
  end;

{ TplScrollbarTrack }


  TplScrollbarTrack = class (TCustomControl)
  private
    FThumb: TplScrollbarThumb;
    FKind: TScrollBarKind;
    FSmallChange: Integer;
    FLargeChange: Integer;
    FMin: Integer;
    FMax: Integer;
    FPosition: Integer;
    procedure SetSmallChange(Value: Integer);
    procedure SetLargeChange(Value: Integer);
    procedure SetMin(Value: Integer);
    procedure SetMax(Value: Integer);
    procedure SetPosition(Value: Integer);
    procedure SetKind(Value: TScrollBarKind);
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
    function  ThumbFromPosition: Integer;
    function  PositionFromThumb: Integer;
    procedure DoPositionChange;

    procedure DoThumbColor(Value: TColor);
    procedure DoHScroll(var Message: TWMScroll);
    procedure DoVScroll(var Message: TWMScroll);
    procedure DoGetPos(var Message: TMessage);
    procedure DoGetRange(var Message: TMessage);
    procedure DoSetPos(var Message: TMessage);
    procedure DoSetRange(var Message: TMessage);
    procedure DoKeyDown(var Message: TLMKeyDown);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    property Align;
    property Color;
    property ParentColor;
    property Min: Integer read FMin write SetMin;
    property Max: Integer read FMax write SetMax;
    property SmallChange: Integer read FSmallChange write SetSmallChange;
    property LargeChange: Integer read FLargeChange write SetLargeChange;
    property Position: Integer read FPosition write SetPosition;
    property Kind: TScrollBarKind read FKind write SetKind;
  end;


  TplScrollbarButton = class (TplButton)
  private
    FNewDown: Boolean;
    FTimer: TTimer;
    FOnDown: TNotifyEvent;
    procedure DoTimer(Sender: TObject);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property OnDown: TNotifyEvent read FOnDown write FOnDown;
  end;

  TplOnScroll = procedure (Sender: TObject; ScrollPos: Integer) of object;

  TplScrollbar = class(TCustomControl)
  private
    FTrack: TplScrollbarTrack;
    FBtnOne: TplScrollbarButton;
    FBtnTwo: TplScrollbarButton;
    FMin: Integer;
    FMax: Integer;
    FSmallChange: Integer;
    FLargeChange: Integer;
    FPosition: Integer;
    FKind: TScrollBarKind;
    FOnScroll: TplOnScroll;
    procedure SetSmallChange(Value: Integer);
    procedure SetLargeChange(Value: Integer);
    procedure SetMin(Value: Integer);
    procedure SetMax(Value: Integer);
    procedure SetPosition(Value: Integer);
    procedure SetKind(Value: TScrollBarKind);
    procedure BtnOneClick(Sender: TObject);
    procedure BtnTwoClick(Sender: TObject);
    procedure EnableBtnOne(Value: Boolean);
    procedure EnableBtnTwo(Value: Boolean);
    procedure DoScroll;
    procedure DoFindSizes;

    procedure CNHScroll(var Message: TWMScroll); message WM_HSCROLL;
    procedure CNVScroll(var Message: TWMScroll); message WM_VSCROLL;
    procedure WMKeyDown(var Message: TLMKeyDown); message LM_KEYDOWN;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    property Min: Integer read FMin write SetMin default 0;
    property Max: Integer read FMax write SetMax default 100;
    property SmallChange: Integer read FSmallChange write SetSmallChange default 1;
    property LargeChange: Integer read FLargeChange write SetLargeChange default 1;
    property Position: Integer read FPosition write SetPosition default 0;
    property Kind: TScrollBarKind read FKind write SetKind default sbHorizontal;
    property OnScroll: TplOnScroll read FOnScroll write FOnScroll;

    property Align;
    property ParentColor;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyUp;
    property OnStartDrag;
  end;


implementation

{$R TplScrollbarUnit.res}

Const
  cnBtnSize=17;

//===================== TplScrollbarTrackThumb =========================================

constructor TplScrollbarThumb.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

procedure TplScrollbarThumb.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  iTop: Integer;
begin
  if TplScrollbarTrack(Parent).Kind = sbVertical then
  begin
    FTopLimit := 0;
    FBottomLimit := TplScrollbarTrack(Parent).Height;
    if FDown = True then
    begin
      iTop := Top + Y - FOldY;
      if iTop < FTopLimit then
      begin
        iTop := FTopLimit;
      end;
      if (iTop > FBottomLimit) or ((iTop + Height) > FBottomLimit) then
      begin
        iTop := FBottomLimit - Height;
      end;
      Top := iTop;
    end;
  end
  else
  begin
    FTopLimit := 0;
    FBottomLimit := TplScrollbarTrack(Parent).Width;
    if FDown = True then
    begin
      iTop := Left + X - FOldX;
      if iTop < FTopLimit then
      begin
        iTop := FTopLimit;
      end;
      if (iTop > FBottomLimit) or ((iTop + Width) > FBottomLimit) then
      begin
        iTop := FBottomLimit - Width;
      end;
      Left := iTop;
    end;
  end;
  TplScrollbarTrack(Parent).FPosition := TplScrollbarTrack(Parent).PositionFromThumb;
  TplScrollbarTrack(Parent).DoPositionChange;
  inherited MouseMove(Shift,X,Y);
end;

procedure TplScrollbarThumb.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FDown := False;
  inherited MouseUp(Button,Shift,X,Y);
end;

procedure TplScrollbarThumb.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbleft) and not FDown then FDown := True;
  FOldX := X;
  FOldy := Y;
  inherited MouseDown(Button,Shift,X,Y);
end;

// =========================== TplScrollbarTrack ===========================================

constructor TplScrollbarTrack.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Color := clSilver;

  FThumb := TplScrollbarThumb.Create(Self);
  FThumb.Width:=cnBtnSize;
  FThumb.Height := cnBtnSize;
  InsertControl(FThumb);

  FMin := 0;
  FMax := 100;
  FSmallChange := 1;
  FLargeChange := 1;
  FPosition := 0;
  FThumb.Top := ThumbFromPosition;
end;

destructor TplScrollbarTrack.Destroy;
begin
  FThumb.Free;
  inherited Destroy;
end;

procedure TplScrollbarTrack.Paint;
begin
    Canvas.Brush.Color := DefiScrollbarColor;
    Canvas.FillRect(ClientRect);
    Frame3DBorder(Canvas,ClientRect,clGray,clwhite,1);
end;

procedure TplScrollbarTrack.SetSmallChange(Value: Integer);
begin
  if Value <> FSmallChange then
  begin
    FSmallChange := Value;
  end;
end;

procedure TplScrollbarTrack.SetLargeChange(Value: Integer);
begin
  if Value <> FLargeChange then
  begin
    FLargeChange := Value;
  end;
end;

procedure TplScrollbarTrack.SetMin(Value: Integer);
begin
  if Value <> FMin then
  begin
    FMin := Value;
    FThumb.Top := ThumbFromPosition;
  end;
end;

procedure TplScrollbarTrack.SetMax(Value: Integer);
begin
  if Value <> FMax then
  begin
    FMax := Value;
    FThumb.Top := ThumbFromPosition;
  end;
end;

procedure TplScrollbarTrack.SetPosition(Value: Integer);
begin
  FPosition := Value;
  if Position > Max then
  begin
    Position := Max;
  end;
  if Position < Min then
  begin
    Position := Min;
  end;
  case FKind of
    sbVertical: FThumb.Top := ThumbFromPosition;
    sbHorizontal: FThumb.Left := ThumbFromPosition;
  end;
end;

procedure TplScrollbarTrack.SetKind(Value: TScrollBarKind);
begin
  if Value <> FKind then
  begin
    FKind:= Value;
    case FKind of
      sbVertical: FThumb.Height := cnBtnSize;
      sbHorizontal: FThumb.Width := cnBtnSize;
    end;
  end;
  Position := FPosition;
end;

procedure TplScrollbarTrack.WMSize(var Message: TLMSize);
begin
  if FKind = sbVertical then
  begin
    FThumb.Width := Width;
  end
  else
  begin
    FThumb.Height := Height;
   end;
end;

function TplScrollbarTrack.ThumbFromPosition: Integer;
var
  iHW, iMin, iMax, iPosition, iResult: Integer;
begin
  iHW := 0;
  case FKind of
    sbVertical: iHW := Height - FThumb.Height;
    sbHorizontal: iHW := Width - FThumb.Width;
  end;
  iMin := FMin;
  iMax := FMax;
  iPosition := FPosition;
  iResult := Round((iHW / (iMax - iMin)) * iPosition);
  Result := iResult;
end;

function TplScrollbarTrack.PositionFromThumb: Integer;
var
  iHW, iMin, iMax, iPosition, iResult: Integer;
begin
  iHW := 0;
  case FKind of
    sbVertical: iHW :=  Height - FThumb.Height;
    sbHorizontal: iHW := Width - FThumb.Width;
  end;
  iMin := FMin;
  iMax := FMax;
  iPosition := 0;
  case FKind of
    sbVertical: iPosition := FThumb.Top;
    sbHorizontal: iPosition := FThumb.Left;
  end;
  iResult := Round(iPosition / iHW * (iMax - iMin));
  Result := iResult;
end;

procedure TplScrollbarTrack.DoPositionChange;
begin
  TplScrollbar(Parent).FPosition := Position;
  TplScrollbar(Parent).DoScroll;
end;


procedure TplScrollbarTrack.DoThumbColor(Value: TColor);
begin
 FThumb.Color := Value;
end;

procedure TplScrollbarTrack.DoHScroll(var Message: TWMScroll);
var
  iPosition: Integer;
begin
  case Message.ScrollCode of
    SB_BOTTOM: Position := Max;
    SB_LINELEFT: begin
                iPosition := Position;
                Dec(iPosition,SmallChange);
                Position := iPosition;
               end;
    SB_LINERIGHT: begin
                 iPosition := Position;
                 Inc(iPosition,SmallChange);
                 Position := iPosition;
                end;
    SB_PAGELEFT: begin
                iPosition := Position;
                Dec(iPosition,LargeChange);
                Position := iPosition;
               end;
    SB_PAGERIGHT: begin
                 iPosition := Position;
                 Inc(iPosition,LargeChange);
                 Position := iPosition;
                end;
    SB_THUMBPOSITION, SB_THUMBTRACK: Position := Message.Pos;
    SB_TOP: Position := Min;
  end;
  Message.Result := 0;
end;

procedure TplScrollbarTrack.DoVScroll(var Message: TWMScroll);
var
  iPosition: Integer;
begin
  case Message.ScrollCode of
    SB_BOTTOM: Position := Max;
    SB_LINEUP: begin
                iPosition := Position;
                Dec(iPosition,SmallChange);
                Position := iPosition;
               end;
    SB_LINEDOWN: begin
                 iPosition := Position;
                 Inc(iPosition,SmallChange);
                 Position := iPosition;
                end;
    SB_PAGEUP: begin
                iPosition := Position;
                Dec(iPosition,LargeChange);
                Position := iPosition;
               end;
    SB_PAGEDOWN: begin
                 iPosition := Position;
                 Inc(iPosition,LargeChange);
                 Position := iPosition;
                end;
    SB_THUMBPOSITION, SB_THUMBTRACK: Position := Message.Pos;
    SB_TOP: Position := Min;
  end;
  Message.Result := 0;
end;


procedure TplScrollbarTrack.DoGetPos(var Message: TMessage);
begin
  Message.Result := Position;
end;

procedure TplScrollbarTrack.DoGetRange(var Message: TMessage);
begin
  Message.WParam := Min;
  Message.LParam := Max;
end;

procedure TplScrollbarTrack.DoSetPos(var Message: TMessage);
begin
  Position := Message.WParam;
end;

procedure TplScrollbarTrack.DoSetRange(var Message: TMessage);
begin
  Min := Message.WParam;
  Max := Message.LParam;
end;

procedure TplScrollbarTrack.DoKeyDown(var Message: TLMKeyDown);
var
  iPosition: Integer;
begin
  iPosition := Position;
  case Message.CharCode of
    VK_PRIOR: Dec(iPosition,LargeChange);
    VK_NEXT: Inc(iPosition,LargeChange);
    VK_UP: if FKind = sbVertical then Dec(iPosition,SmallChange);
    VK_DOWN: if FKind = sbVertical then Inc(iPosition,SmallChange);
    VK_LEFT: if FKind = sbHorizontal then Dec(iPosition,SmallChange);
    VK_RIGHT: if FKind = sbHorizontal then Inc(iPosition,SmallChange);
  end;
  Position := iPosition;
end;

//======================== TplScrollbarButton =================================================

constructor TplScrollbarButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := false;
  FTimer.Interval := 10;
  FTimer.OnTimer := @DoTimer;
end;

destructor TplScrollbarButton.Destroy;
begin
  FTimer.Enabled := False;
  FTimer.Free;
  inherited Destroy;
end;

procedure TplScrollbarButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button,Shift,X,Y);
  FNewDown := True;
  FTimer.Enabled := True;
end;

procedure TplScrollbarButton.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift,X,Y);
end;

procedure TplScrollbarButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button,Shift,X,Y);
  FNewDown := False;
  FTimer.Enabled := False;
end;

procedure TplScrollbarButton.DoTimer(Sender: TObject);
begin
  if FNewDown = True then
  begin
    if Assigned(FOnDown) then FOnDown(Self);
    TplScrollbar(Parent).DoScroll;
  end;
end;

//=========================== TplScrollbar =====================================================

constructor TplScrollbar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  self.SetInitialBounds(0,0,150,12);

  FTrack := TplScrollbarTrack.Create(Self);
  InsertControl(FTrack);

  FBtnOne := TplScrollbarButton.Create(Self);
  FBtnOne.Glyph.LoadFromResourceName(hInstance,'THUMB_LEFT_ENABLED');
  FBtnOne.OnDown := @BtnOneClick;
  InsertControl(FBtnOne);

  FBtnTwo := TplScrollbarButton.Create(Self);
  FBtnTwo.Glyph.LoadFromResourceName(hInstance,'THUMB_RIGHT_ENABLED');
  FBtnTwo.OnDown := @BtnTwoClick;
  InsertControl(FBtnTwo);

  FBtnOne.Enabled := True;
  FBtnTwo.Enabled := True;

  Color := ecLightKaki;
  Kind := sbHorizontal;

  Min := 0;
  Max := 100;
  Position := 0;
  SmallChange := 1;
  LargeChange := 1;
end;

destructor TplScrollbar.Destroy;
begin
  FTrack.Free;
  FBtnOne.Free;
  FBtnTwo.Free;
  inherited Destroy;
end;

procedure TplScrollbar.SetSmallChange(Value: Integer);
begin
  if Value <> FSmallChange then
  begin
    FSmallChange := Value;
    FTrack.SmallChange := FSmallChange;
  end;
end;

procedure TplScrollbar.SetLargeChange(Value: Integer);
begin
  if Value <> FLargeChange then
  begin
    FLargeChange := Value;
    FTrack.LargeChange := FLargeChange;
  end;
end;

procedure TplScrollbar.SetMin(Value: Integer);
begin
  if Value <> FMin then
  begin
    FMin := Value;
    FTrack.Min := FMin;
  end;
end;

procedure TplScrollbar.SetMax(Value: Integer);
begin
  if Value <> FMax then
  begin
    FMax := Value;
    FTrack.Max := FMax;
  end;
end;

procedure TplScrollbar.SetPosition(Value: Integer);
begin
  FPosition := Value;
  if Position < Min then
  begin
    Position := Min;
  end;
  if Position > Max then
  begin
    Position := Max;
  end;
  FTrack.Position := FPosition;
end;

procedure TplScrollbar.SetKind(Value: TScrollBarKind);
var
  iw,ih: Integer;
begin

if FKind = Value then exit;

iw:=Height;
ih:=Width;

FKind := Value;

if (csDesigning in ComponentState) or (csLoading in ComponentState) then
begin
  FTrack.Kind := FKind;

    if FKind = sbVertical then
    begin
      FBtnOne.Glyph.LoadFromResourceName(hInstance,'THUMB_UP_ENABLED');
      FBtnOne.Refresh;
      FBtnTwo.Glyph.LoadFromResourceName(hInstance,'THUMB_DOWN_ENABLED');
      FBtnTwo.Refresh;
    end else
    begin
      FBtnOne.Glyph.LoadFromResourceName(hInstance,'THUMB_LEFT_ENABLED');
      FBtnOne.Refresh;
      FBtnTwo.Glyph.LoadFromResourceName(hInstance,'THUMB_RIGHT_ENABLED');
      FBtnTwo.Refresh;
    end;

end else
begin
   if FKind = sbVertical then
    begin
      FBtnOne.Glyph.LoadFromResourceName(hInstance,'THUMB_UP_ENABLED');
      FBtnTwo.Glyph.LoadFromResourceName(hInstance,'THUMB_DOWN_ENABLED');
    end else
    begin
      FBtnOne.Glyph.LoadFromResourceName(hInstance,'THUMB_LEFT_ENABLED');
      FBtnTwo.Glyph.LoadFromResourceName(hInstance,'THUMB_RIGHT_ENABLED');
    end;

  SetBounds(left,top,iw,ih);

  FBtnOne.Refresh;
  FBtnOne.Repaint;

  FBtnTwo.Repaint;
  FBtnTwo.Refresh;

  FTrack.Kind := FKind;
  FTrack.Repaint;
end;

end;

procedure TplScrollbar.BtnOneClick(Sender: TObject);
var
  iPosition: Integer;
begin
  iPosition := Position;
  Dec(iPosition,SmallChange);
  Position := iPosition;
end;

procedure TplScrollbar.BtnTwoClick(Sender: TObject);
var
  iPosition: Integer;
begin
  iPosition := Position;
  Inc(iPosition,SmallChange);
  Position := iPosition;
end;

procedure TplScrollbar.EnableBtnOne(Value: Boolean);
begin
  if Value = True then
  begin
    FBtnOne.Enabled := True;
    case FKind of
      sbVertical: FBtnOne.Glyph.LoadFromResourceName(hInstance,'THUMB_UP_ENABLED');
      sbHorizontal: FBtnOne.Glyph.LoadFromResourceName(hInstance,'THUMB_LEFT_ENABLED');
    end;
  end
  else
  begin
    case FKind of
      sbVertical: FBtnOne.Glyph.LoadFromResourceName(hInstance,'THUMB_UP_DISABLED');
      sbHorizontal: FBtnOne.Glyph.LoadFromResourceName(hInstance,'THUMB_LEFT_DISABLED');
    end;
    FBtnOne.Enabled := False;
  end;
end;

procedure TplScrollbar.EnableBtnTwo(Value: Boolean);
begin
  if Value = True then
  begin
    FBtnTwo.Enabled := True;
    case FKind of
      sbVertical: FBtnTwo.Glyph.LoadFromResourceName(hInstance,'THUMB_DOWN_ENABLED');
      sbHorizontal: FBtnTwo.Glyph.LoadFromResourceName(hInstance,'THUMB_RIGHT_ENABLED');
    end;
  end
  else
  begin
    case FKind of
      sbVertical: FBtnTwo.Glyph.LoadFromResourceName(hInstance,'THUMB_DOWN_DISABLED');
      sbHorizontal: FBtnTwo.Glyph.LoadFromResourceName(hInstance,'THUMB_RIGHT_DISABLED');
    end;
    FBtnTwo.Enabled := False;
  end;
end;

procedure TplScrollbar.DoFindSizes;
begin
  if FKind = sbVertical then
  begin
    FTrack.SetBounds(0,cnBtnSize-1,Width, ABS(Height - (2*cnBtnSize)+2));
    FBtnOne.SetBounds(0,0,Width,cnBtnSize);
    FBtnTwo.SetBounds(0,ABS(Height - cnBtnSize),Width,cnBtnSize);
  end
  else
  begin
    FTrack.SetBounds(cnBtnSize-1,0,ABS(Width - (2*cnBtnSize)+2), Height);
    FBtnOne.SetBounds(0,0,cnBtnSize,Height);
    FBtnTwo.SetBounds(ABS(Width - cnBtnSize),0,cnBtnSize,Height);
  end;
  Position := FPosition;
end;

procedure TplScrollbar.DoScroll;
begin
  if Assigned(FOnScroll) then FOnScroll(Self,Position);
end;

procedure TplScrollbar.CNHScroll(var Message: TWMScroll);
begin
  FTrack.DoHScroll(Message);
end;

procedure TplScrollbar.CNVScroll(var Message: TWMScroll);
begin
  FTrack.DoVScroll(Message);
end;

procedure TplScrollbar.WMKeyDown(var Message: TLMKeyDown);
begin
  FTrack.DoKeyDown(Message);
end;

procedure TplScrollbar.Paint;
begin
   DoFindSizes;
   inherited;
end;

end.
