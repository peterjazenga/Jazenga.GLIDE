
{**********************************************************************
 Package pl_ExControls.pkg
 From PilotLogic Software House(http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit TplRadioButtonUnit;

interface

uses
  LCLIntf, LCLType, LMessages, Messages,
  Classes, Graphics, Controls, Forms, ExtCtrls, plUtils;

type
  TplRadioButton = class(TCustomControl)
  private
    FUseAdvColors: Boolean;
    FAdvColorFocused: TAdvColors;
    FAdvColorDown: TAdvColors;
    FAdvColorBorder: TAdvColors;
    FMouseInControl: Boolean;
    MouseIsDown: Boolean;
    fFocused: Boolean;
    FGroupIndex: Integer;
    FLayout: TRadioButtonLayout;
    FChecked: Boolean;
    FFocusedColor: TColor;
    FDownColor: TColor;
    FDotColor: TColor;
    FBorderColor: TColor;
    FFlatColor: TColor;
    FTransparent: Boolean;
    procedure SetColors (Index: Integer; Value: TColor);
    procedure SetAdvColors (Index: Integer; Value: TAdvColors);
    procedure SetUseAdvColors (Value: Boolean);
    procedure SetLayout (Value: TRadioButtonLayout);
    procedure SetChecked (Value: Boolean);
    procedure SetTransparent(const Value: Boolean);
    procedure CMEnabledChanged(var Message: TLMessage); message CM_ENABLEDCHANGED;
    procedure CMTextChanged(var Message: TLMessage); message CM_TEXTCHANGED;
    procedure CMDialogChar (var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CNCommand(var TheMessage: TLMCommand); message CN_Command;
    procedure CMSysColorChange (var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMParentColorChanged(var Message: TLMessage); message CM_PARENTCOLORCHANGED;
    procedure CMDesignHitTest(var Message: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure WMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
    procedure WMKillFocus(var Message: TLMKillFocus); message LM_KILLFOCUS;
    procedure WMMove(var Message: TLMMove); message LM_MOVE;
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
    procedure RemoveMouseTimer;
    procedure MouseTimerHandler (Sender: TObject);
  protected
    procedure CalcAdvColors;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure MouseDown (Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp (Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove (Shift: TShiftState; X, Y: Integer); override;
    procedure CreateWnd; override;
    procedure DrawRadio;
    procedure DrawRadioText;
    procedure Paint; override;
    procedure SetBiDiMode(Value: TBiDiMode); override;

  public
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;
    procedure MouseEnter;
    procedure MouseLeave;

    property ColorFocused: TColor index 0 read FFocusedColor write SetColors ;
    property ColorDown: TColor index 1 read FDownColor write SetColors ;
    property ColorDot: TColor index 2 read FDotColor write SetColors ;
    property ColorBorder: TColor index 3 read FBorderColor write SetColors ;
    property ColorFlat: TColor index 4 read FFlatColor write SetColors;
    property AdvColorFocused: TAdvColors index 0 read FAdvColorFocused write SetAdvColors ;
    property AdvColorDown: TAdvColors index 1 read FAdvColorDown write SetAdvColors ;
    property AdvColorBorder: TAdvColors index 2 read FAdvColorBorder write SetAdvColors ;
    property UseAdvColors: Boolean read FUseAdvColors write SetUseAdvColors default false;

  published
    property Transparent: Boolean read FTransparent write SetTransparent default false;
    property Caption;
    property Checked: Boolean read FChecked write SetChecked default false;
    property Color;
    
    property Enabled;
    property Font;
    property GroupIndex: Integer read FGroupIndex write FGroupIndex default 0;
    property Layout: TRadioButtonLayout read FLayout write SetLayout default radioLeft;
    property ParentColor;
    property ParentFont;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property Action;
    property Anchors;
    property BiDiMode write SetBidiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
  end;

var
  MouseInControl: TplRadioButton = nil;

implementation

var
  MouseTimer: TTimer = nil;
  ControlCounter: Integer = 0;

procedure TplRadioButton.CMDesignHitTest(var Message: TCMDesignHitTest);
begin
  case FLayout of
    radioLeft:
      if PtInRect(Rect(ClientRect.Left + 1, ClientRect.Top + 3, ClientRect.Left + 11, ClientRect.Top + 13), Point(message.XPos, message.YPos)) then
        Message.Result := 1
      else
        Message.Result := 0;
    radioRight:
      if PtInRect(Rect(ClientRect.Right - 11, ClientRect.Top + 3, ClientRect.Right - 1, ClientRect.Top + 13), Point(message.XPos, message.YPos)) then
        Message.Result := 1
      else
        Message.Result := 0;
  end;
end;

constructor TplRadioButton.Create (AOwner: TComponent);
begin
  inherited Create(AOwner);
  if MouseTimer = nil then
  begin
    MouseTimer := TTimer.Create(nil);
    MouseTimer.Enabled := False;
    MouseTimer.Interval := 100; // 8 times a second
  end;
  ParentColor := True;
  ParentFont := True;
  Font.Color := clBlack;
  Color:=DefiControlsBkColor;
  
  FFocusedColor := DefiColorFocused;
  FDownColor := DefiColorDown;
  FBorderColor := DefiColorBorder;
  fFlatColor:=DefiColorFlat; 
  FDotColor := DefiColorCheck;
  fTransparent:=false;
  
  FLayout := radioLeft;
  FChecked := false;
  FGroupIndex := 0;
  Enabled := true;
  Visible := true;
  SetBounds(0, 0, 121, 17); 

  FUseAdvColors := DefiUseAdvColors;
  FAdvColorFocused := DefiAdvColorFocused;
  FAdvColorDown := DefiAdvColorDown;
  FAdvColorBorder := DefiAdvColorBorder;
  
  Inc(ControlCounter);
end;

destructor TplRadioButton.Destroy;
begin
  RemoveMouseTimer;
  Dec(ControlCounter);
  if ControlCounter = 0 then
  begin
    MouseTimer.Free;
    MouseTimer := nil;
  end;
  inherited;
end;

procedure TplRadioButton.SetColors (Index: Integer; Value: TColor);
begin
  case Index of
    0: FFocusedColor := Value;
    1: FDownColor := Value;
    2: FDotColor := Value;
    3: FBorderColor := Value;
    4: fFLatColor:= Value;
  end;
  Invalidate;
end;

procedure TplRadioButton.CalcAdvColors;
begin
  if FUseAdvColors then
  begin
    FFocusedColor := CalcAdvancedColor(Color, FFocusedColor, FAdvColorFocused, lighten);
    FDownColor := CalcAdvancedColor(Color, FDownColor, FAdvColorDown, darken);
    FBorderColor := CalcAdvancedColor(Color, FBorderColor, FAdvColorBorder, darken);
  end;
end;

procedure TplRadioButton.SetAdvColors (Index: Integer; Value: TAdvColors);
begin
  case Index of
    0: FAdvColorFocused := Value;
    1: FAdvColorDown := Value;
    2: FAdvColorBorder := Value;
  end;
  CalcAdvColors;
  Invalidate;
end;

procedure TplRadioButton.SetUseAdvColors (Value: Boolean);
begin
  if Value <> FUseAdvColors then
  begin
    FUseAdvColors := Value;
    ParentColor := Value;
    CalcAdvColors;
    Invalidate;
  end;
end;

procedure TplRadioButton.SetLayout (Value: TRadioButtonLayout);
begin
  FLayout := Value;
  Invalidate;
end;

procedure TplRadioButton.SetChecked (Value: Boolean);
var
  I: Integer;
  Sibling: TplRadioButton;
begin
  if FChecked <> Value then
  begin
    TabStop := Value;
    FChecked := Value;
    if Value then
    begin
      if Parent <> nil then
        for i := 0 to Parent.ControlCount-1 do
          if Parent.Controls[i] is TplRadioButton then
          begin
            Sibling := TplRadioButton(Parent.Controls[i]);
            if (Sibling <> Self) and (Sibling.GroupIndex = GroupIndex) then
              Sibling.SetChecked(False);
          end;
      Click;
      if csDesigning in ComponentState then
        if (GetParentForm(self) <> nil) and (GetParentForm(self).Designer <> nil) then
          GetParentForm(self).Designer.Modified;
    end;
    DrawRadio;
  end;
end;

procedure TplRadioButton.CMEnabledChanged (var Message: TMessage);
begin
  inherited;
  if not Enabled then
  begin
    FMouseInControl := False;
    MouseIsDown := False;
    RemoveMouseTimer;
  end;
  Invalidate;
end;

procedure TplRadioButton.CMTextChanged(var Message: TLMessage);
begin
  inherited;
  Invalidate;
end;

procedure TplRadioButton.MouseEnter;
begin
  if Enabled and not FMouseInControl then
  begin
    FMouseInControl := True;
    DrawRadio;
  end;
end;

procedure TplRadioButton.MouseLeave;
begin
  if Enabled and FMouseInControl and not MouseIsDown then
  begin
    FMouseInControl := False;
    RemoveMouseTimer;
    DrawRadio;
  end;
end;

procedure TplRadioButton.CMDialogChar (var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(Message.CharCode, Caption) and CanFocus then
    begin
      SetFocus;
      Result := 1;
    end
    else
      inherited;
end;

procedure TplRadioButton.CNCommand(var TheMessage: TLMCommand);
begin
  if TheMessage.NotifyCode = LM_CLICKED then Click;
end;

procedure TplRadioButton.WMSetFocus(var Message: TLMSetFocus);
begin
  inherited;
  if Enabled then
  begin
    fFocused := True;
    DrawRadio;
  end;
end;

procedure TplRadioButton.WMKillFocus(var Message: TLMKillFocus);
begin
  inherited;
  if Enabled then
  begin
    FMouseInControl := False;
    fFocused := False;
    DrawRadio;
  end;
end;


procedure TplRadioButton.CMSysColorChange (var Message: TMessage);
begin
  if FUseAdvColors then
  begin
    ParentColor := True;
    CalcAdvColors;
  end;
  Invalidate;
end;

procedure TplRadioButton.CMParentColorChanged(var Message: TLMessage);
begin
  inherited;
  if FUseAdvColors then
  begin
    ParentColor := True;
    CalcAdvColors;
  end;
  Invalidate;
end;

procedure TplRadioButton.DoEnter;
begin
  inherited DoEnter;
  if MouseIsDown and FMouseInControl then
    Checked := True;
  fFocused := True;
  DrawRadio;
end;

procedure TplRadioButton.DoExit;
begin
  inherited DoExit;
  fFocused := False;
  DrawRadio;
end;

procedure TplRadioButton.MouseDown (Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and Enabled then
  begin
    SetFocus;
    MouseIsDown := true;
    DrawRadio;
    inherited MouseDown(Button, Shift, X, Y);
  end;
end;

procedure TplRadioButton.MouseUp (Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and Enabled then
  begin
    MouseIsDown := false;
    if (X>=0) and (X<=Width) and (Y>=0) and (Y<=Height) and not Checked then
      Checked := True;
    DrawRadio;
    inherited MouseUp(Button, Shift, X, Y);
  end;
end;

procedure TplRadioButton.MouseMove (Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  inherited;
  // mouse is in control ?
  P := ClientToScreen(Point(X, Y));
  if (MouseInControl <> Self) and (FindDragTarget(P, True) = Self) then
  begin
    if Assigned(MouseInControl) then
      MouseInControl.MouseLeave;
    // the application is active ?
    if (GetActiveWindow <> 0) then
    begin
      if MouseTimer.Enabled then
        MouseTimer.Enabled := False;
      MouseInControl := Self;
      MouseTimer.OnTimer := @MouseTimerHandler;
      MouseTimer.Enabled := True;
      MouseEnter;
    end;
  end;
end;

procedure TplRadioButton.CreateWnd;
begin
  inherited CreateWnd;
  SendMessage(Handle, LM_CHANGED, Cardinal(FChecked), 0);
end;

procedure TplRadioButton.DrawRadio;
var  RadioRect: TRect;
begin
  case FLayout of
    radioLeft:
      RadioRect := Rect(ClientRect.Left + 1, ClientRect.Top + 3, ClientRect.Left + 11, ClientRect.Top + 13);
    radioRight:
      RadioRect := Rect(ClientRect.Right - 11, ClientRect.Top + 3, ClientRect.Right - 1, ClientRect.Top + 13);
  end;

  // Radio
  canvas.pen.style := psSolid;
  canvas.pen.width := 1;
  // Background + Border
  if Focused or FMouseInControl then
    if not MouseIsDown then
    begin
      canvas.brush.color := FFocusedColor;
      canvas.pen.color := FBorderColor;
    end
    else
    begin
      canvas.brush.color := FDownColor;
      canvas.pen.color := FBorderColor;
    end
  else
  begin
    canvas.brush.color := fFlatColor;
    canvas.pen.color := FBorderColor;
  end;
  Ellipse(canvas.handle, radioRect.left, radioRect.top, radioRect.Right, radioRect.bottom);

  // Dot
  if Checked then
  begin
    if Enabled then
    begin
      canvas.brush.color := FDotColor;
      canvas.pen.color := FDotColor;
    end
    else
    begin
      canvas.brush.color := clBtnShadow;
      canvas.pen.color := clBtnShadow;
    end;
    Ellipse(canvas.handle, radioRect.left+3, radioRect.top+3, radioRect.left+7, radioRect.top+7);
  end;
end;

procedure TplRadioButton.DrawRadioText;
var
  TextBounds: TRect;
  Format: UINT;
begin
  Format := DT_WORDBREAK;
  case FLayout of
    radioLeft:
    begin
      TextBounds := Rect(ClientRect.Left + 16, ClientRect.Top + 1, ClientRect.Right - 1, ClientRect.Bottom - 1);
      Format := Format or DT_LEFT;
    end;
    radioRight:
    begin
      TextBounds := Rect(ClientRect.Left + 1, ClientRect.Top + 1, ClientRect.Right - 16, ClientRect.Bottom - 1);
      Format := Format or DT_RIGHT;
    end;
  end;

  with Canvas do
  begin
    Brush.Style := bsClear;
    Font := Self.Font;
    if not Enabled then
    begin
      OffsetRect(TextBounds, 1, 1);
      Font.Color := clBtnHighlight;
      DrawText(Handle, PChar(Caption), Length(Caption), TextBounds, Format);
      OffsetRect(TextBounds, -1, -1);
      Font.Color := clBtnShadow;
      DrawText(Handle, PChar(Caption), Length(Caption), TextBounds, Format);
    end
    else
      DrawText(Handle, PChar(Caption), Length(Caption), TextBounds, Format);
  end;
end;

procedure TplRadioButton.Paint;
begin
  if FTransparent then
    DrawParentImage(Self, Self.Canvas) else
    if Color<>DefiControlsBkColor then Color:=DefiControlsBkColor;


  DrawRadio;
  DrawRadioText;
end;

procedure TplRadioButton.WMSize (var Message: TWMSize);
begin
  inherited;
  if FTransparent then
    Invalidate;
end;

procedure TplRadioButton.WMMove(var Message: TLMMove);
begin
  inherited;
  if FTransparent then
    Invalidate;
end;

procedure TplRadioButton.MouseTimerHandler (Sender: TObject);
var
  P: TPoint;
begin
  GetCursorPos (P);
  if FindDragTarget(P, True) <> Self then
    MouseLeave;
end;

procedure TplRadioButton.RemoveMouseTimer;
begin
  if MouseInControl = Self then
  begin
    MouseTimer.Enabled := False;
    MouseInControl := nil;
  end;
end;

procedure TplRadioButton.SetTransparent(const Value: Boolean);
begin
  FTransparent := Value;
  Invalidate;
end;

procedure TplRadioButton.SetBiDiMode(Value: TBiDiMode);
begin
  inherited;
  if BidiMode = bdRightToLeft then
    Layout := radioRight
  else
    Layout := radioLeft;
end;


end.
