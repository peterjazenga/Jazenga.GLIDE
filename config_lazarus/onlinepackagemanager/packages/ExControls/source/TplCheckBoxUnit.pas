{**********************************************************************
 Package pl_ExControls.pkg
 From PilotLogic Software House(http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit TplCheckBoxUnit;

interface

uses
  LCLIntf, LCLType, Messages,LMessages,
  Classes, Graphics, Controls, Forms, ExtCtrls, plUtils;

type

  TplCheckBox = class(TCustomControl)
  private
    procedure SetColors (Index: Integer; Value: TColor);
    procedure SetAdvColors (Index: Integer; Value: TAdvColors);
    procedure SetUseAdvColors (Value: Boolean);
    procedure SetLayout (Value: TCheckBoxLayout);
    procedure SetChecked (Value: Boolean);
    procedure SetTransparent(const Value: Boolean);
    procedure CMEnabledChanged(var Message: TLMessage); message CM_ENABLEDCHANGED;
    procedure CMTextChanged (var Message: TWmNoParams); message CM_TEXTCHANGED;
    procedure CMDialogChar (var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CNCommand(var TheMessage: TLMCommand); message CN_Command;
    procedure WMSetFocus (var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus (var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMParentColorChanged(var Message: TLMessage); message CM_PARENTCOLORCHANGED;
    procedure RemoveMouseTimer;
    procedure MouseTimerHandler (Sender: TObject);
    procedure CMDesignHitTest (var Message: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure WMMove(var Message: TLMMove); message LM_MOVE;
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
  protected
    FUseAdvColors: Boolean;
    FAdvColorFocused: TAdvColors;
    FAdvColorDown: TAdvColors;
    FAdvColorBorder: TAdvColors;
    FMouseInControl: Boolean;
    MouseIsDown: Boolean;
    fFocused: Boolean;
    FLayout: TCheckBoxLayout;
    FChecked: Boolean;
    FFocusedColor: TColor;
    FDownColor: TColor;
    FCheckColor: TColor;
    FBorderColor: TColor;
    FFlatColor: TColor;
    FHighlightColor: TColor;
    FTransparent: Boolean;
    procedure CalcAdvColors;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure MouseDown (Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp (Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove (Shift: TShiftState; X, Y: Integer); override;
    procedure CreateWnd; override;
    procedure DrawCheckRect;
    procedure DrawCheckText;
    procedure Paint; override;
    procedure SetBiDiMode(Value: TBiDiMode); override;
  public
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;
    procedure MouseEnter;
    procedure MouseLeave;


    property ColorFocused: TColor index 0 read FFocusedColor write SetColors;
    property ColorDown: TColor index 1 read FDownColor write SetColors;
    property ColorCheck: TColor index 2 read FCheckColor write SetColors;
    property ColorBorder: TColor index 3 read FBorderColor write SetColors;
    property ColorFlat: TColor index 4 read FFlatColor write SetColors;

    property AdvColorFocused: TAdvColors index 0 read FAdvColorFocused write SetAdvColors ;
    property AdvColorDown: TAdvColors index 1 read FAdvColorDown write SetAdvColors ;
    property AdvColorBorder: TAdvColors index 2 read FAdvColorBorder write SetAdvColors ;
    property UseAdvColors: Boolean read FUseAdvColors write SetUseAdvColors ;

  published
    property Transparent: Boolean read FTransparent write SetTransparent default false;
    property Caption;
    property Checked: Boolean read FChecked write SetChecked default false;
    property Color; 
    property Enabled;
    property Font;
    property Layout: TCheckBoxLayout read FLayout write SetLayout default checkBoxLeft;
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
  MouseInControl: TplCheckBox = nil;

implementation

var
  MouseTimer: TTimer = nil;
  ControlCounter: Integer = 0;

procedure TplCheckBox.CMDesignHitTest(var Message: TCMDesignHitTest);
begin
  case FLayout of
    checkboxLeft:
      if PtInRect(Rect(ClientRect.Left + 1, ClientRect.Top + 3, ClientRect.Left + 12, ClientRect.Top + 14), Point(message.XPos, message.YPos)) then
        Message.Result := 1
      else
        Message.Result := 0;
    checkboxRight:
      if PtInRect(Rect(ClientRect.Right - 12, ClientRect.Top + 3, ClientRect.Right - 1, ClientRect.Top + 14), Point(message.XPos, message.YPos)) then
        Message.Result := 1
      else
        Message.Result := 0;
  end;
end;

constructor TplCheckBox.Create (AOwner: TComponent);
begin
  inherited Create(AOwner);
  if MouseTimer = nil then
  begin
    MouseTimer := TTimer.Create(nil);
    MouseTimer.Enabled := False;
    MouseTimer.Interval := 100; // 10 times a second
  end;
  ParentColor := true;
  ParentFont := True;
  Font.Color := clBlack;
  Color:=DefiControlsBkColor;

  FFocusedColor := DefiColorFocused;
  FDownColor := DefiColorDown;
  FCheckColor := DefiColorCheck;
  FBorderColor := DefiColorBorder;
  fFlatColor:=DefiColorFlat;
  FHighlightColor := DefiColorHighlight;

  fTransparent:=false;

  FLayout := checkboxLeft;
  TabStop := True;
  FChecked := false;
  Enabled := true;
  Visible := true;
  SetBounds(0, 0, 121, 17);

  FUseAdvColors := DefiUseAdvColors;
  FAdvColorFocused := DefiAdvColorFocused;
  FAdvColorDown := DefiAdvColorDown;
  FAdvColorBorder := DefiAdvColorBorder;

  Inc(ControlCounter);
end;

destructor TplCheckBox.Destroy;
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

procedure TplCheckBox.SetColors (Index: Integer; Value: TColor);
begin
  case Index of
    0: FFocusedColor := Value;
    1: FDownColor := Value;
    2: FCheckColor := Value;
    3: FBorderColor := Value;
    4: fFLatColor:= Value;
  end;
  Invalidate;
end;

procedure TplCheckBox.CalcAdvColors;
begin
  if FUseAdvColors then
  begin
    FFocusedColor := CalcAdvancedColor(Color, FFocusedColor, FAdvColorFocused, lighten);
    FDownColor := CalcAdvancedColor(Color, FDownColor, FAdvColorDown, darken);
    FBorderColor := CalcAdvancedColor(Color, FBorderColor, FAdvColorBorder, darken);
  end;
end;

procedure TplCheckBox.SetAdvColors (Index: Integer; Value: TAdvColors);
begin
  case Index of
    0: FAdvColorFocused := Value;
    1: FAdvColorDown := Value;
    2: FAdvColorBorder := Value;
  end;
  CalcAdvColors;
  Invalidate;
end;

procedure TplCheckBox.SetUseAdvColors (Value: Boolean);
begin
  if Value <> FUseAdvColors then
  begin
    FUseAdvColors := Value;
    ParentColor := Value;
    CalcAdvColors;
    Invalidate;
  end;
end;

procedure TplCheckBox.SetLayout (Value: TCheckBoxLayout);
begin
  FLayout := Value;
  Invalidate;
end;

procedure TplCheckBox.SetChecked (Value: Boolean);
begin
  if FChecked <> Value then
  begin
    FChecked := Value;
    Click;
    DrawCheckRect;
    if csDesigning in ComponentState then
      if (GetParentForm(self) <> nil) and (GetParentForm(self).Designer <> nil) then
        GetParentForm(self).Designer.Modified;
  end;
end;

procedure TplCheckBox.CMEnabledChanged (var Message: TMessage);
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

procedure TplCheckBox.CMTextChanged (var Message: TWmNoParams);
begin
  inherited;
  Invalidate;
end;

procedure TplCheckBox.MouseEnter;
begin
  if Enabled and not FMouseInControl then
  begin
    FMouseInControl := True;
    DrawCheckRect;
  end;
end;

procedure TplCheckBox.MouseLeave;
begin
  if Enabled and FMouseInControl and not MouseIsDown then
  begin
    FMouseInControl := False;
    RemoveMouseTimer;
    DrawCheckRect;
  end;
end;

procedure TplCheckBox.CMDialogChar (var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(Message.CharCode, Caption) and CanFocus then
    begin
      SetFocus;
      if Checked then
        Checked := False
      else
        Checked := True;
      Result := 1;
    end
    else
      if (CharCode = VK_SPACE) and Focused then
      begin
        if Checked then
          Checked := False
        else
          Checked := True;
      end
      else
        inherited;
end;

procedure TplCheckBox.CNCommand(var TheMessage: TLMCommand);
begin
  if TheMessage.NotifyCode = LM_CLICKED then Click;  // ct9999
end;

procedure TplCheckBox.WMSetFocus (var Message: TWMSetFocus);
begin
  inherited;
  if Enabled then
  begin
    fFocused := True;
    DrawCheckRect;
  end;
end;

procedure TplCheckBox.WMKillFocus (var Message: TWMKillFocus);
begin
  inherited;
  if Enabled then
  begin
    FMouseInControl := False;
    fFocused := False;
    DrawCheckRect;
  end;
end;

procedure TplCheckBox.CMSysColorChange (var Message: TMessage);
begin
  if FUseAdvColors then
  begin
    ParentColor := True;
    CalcAdvColors;
  end;
  Invalidate;
end;

procedure TplCheckBox.CMParentColorChanged(var Message: TLMessage);
begin
  inherited;
  if FUseAdvColors then
  begin
    ParentColor := True;
    CalcAdvColors;
  end;
  Invalidate;
end;

procedure TplCheckBox.DoEnter;
begin
  inherited DoEnter;
  fFocused := True;
  DrawCheckRect;
end;

procedure TplCheckBox.DoExit;
begin
  inherited DoExit;
  fFocused := False;
  DrawCheckRect;
end;

procedure TplCheckBox.MouseDown (Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and Enabled then
  begin
    SetFocus;
    MouseIsDown := true;
    DrawCheckRect;
    inherited MouseDown(Button, Shift, X, Y);
  end;
end;

procedure TplCheckBox.MouseUp (Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and Enabled then
  begin
    MouseIsDown := false;
    if FMouseInControl then
      if Checked then
        Checked := False
      else
        Checked := True;
    DrawCheckRect;
    inherited MouseUp(Button, Shift, X, Y);
  end;
end;

procedure TplCheckBox.MouseMove(Shift: TShiftState; X, Y: Integer);
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

procedure TplCheckBox.CreateWnd;
begin
  inherited CreateWnd;
  SendMessage(Handle, LM_CHANGED, Cardinal(FChecked), 0);
end;

procedure TplCheckBox.DrawCheckRect;
var
  CheckboxRect: TRect;
begin
  case FLayout of
    checkboxLeft:
      CheckboxRect := Rect(ClientRect.Left + 1, ClientRect.Top + 3, ClientRect.Left + 12, ClientRect.Top + 14);
    checkboxRight:
      CheckboxRect := Rect(ClientRect.Right - 12, ClientRect.Top + 3, ClientRect.Right - 1, ClientRect.Top + 14);
  end;

  canvas.pen.style := psSolid;
  canvas.pen.width := 1;
  // Background
  if Focused or FMouseInControl then
    if not MouseIsDown then
    begin
      canvas.brush.color := FFocusedColor;
      canvas.pen.color := FFocusedColor;
    end
    else
    begin
      canvas.brush.color := FDownColor;
      canvas.brush.color := FDownColor;
    end
  else
  begin
    canvas.brush.color := fFLatColor;
    canvas.pen.color := fFLatColor;
  end;
  canvas.FillRect(CheckboxRect);
  // Tick
  if Checked then
  begin
    if Enabled then
      canvas.pen.color := FCheckColor
    else
      canvas.pen.color := clBtnShadow;
    canvas.penpos := Point(CheckboxRect.left+2, CheckboxRect.top+4);
    canvas.lineto(CheckboxRect.left+6, CheckboxRect.top+8);
    canvas.penpos := Point(CheckboxRect.left+2, CheckboxRect.top+5);
    canvas.lineto(CheckboxRect.left+5, CheckboxRect.top+8);
    canvas.penpos := Point(CheckboxRect.left+2, CheckboxRect.top+6);
    canvas.lineto(CheckboxRect.left+5, CheckboxRect.top+9);
    canvas.penpos := Point(CheckboxRect.left+8, CheckboxRect.top+2);
    canvas.lineto(CheckboxRect.left+4, CheckboxRect.top+6);
    canvas.penpos := Point(CheckboxRect.left+8, CheckboxRect.top+3);
    canvas.lineto(CheckboxRect.left+4, CheckboxRect.top+7);
    canvas.penpos := Point(CheckboxRect.left+8, CheckboxRect.top+4);
    canvas.lineto(CheckboxRect.left+5, CheckboxRect.top+7);
  end;
  // Border
  canvas.brush.color := FBorderColor;
  canvas.FrameRect(CheckboxRect);
end;

procedure TplCheckBox.DrawCheckText;
var
  TextBounds: TRect;
  Format: UINT;
begin
  Format := DT_WORDBREAK;
  case FLayout of
    checkboxLeft:
    begin
      TextBounds := Rect(ClientRect.Left + 16, ClientRect.Top + 1, ClientRect.Right - 1, ClientRect.Bottom - 1);
      Format := Format or DT_LEFT;
    end;
    checkboxRight:
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

procedure TplCheckBox.Paint;
begin
  if FTransparent then
    DrawParentImage(Self, Self.Canvas) else
    if Color<>DefiControlsBkColor then Color:=DefiControlsBkColor;

  DrawCheckRect;
  DrawCheckText;
end;

procedure TplCheckBox.MouseTimerHandler (Sender: TObject);
var
  P: TPoint;
begin
  GetCursorPos (P);
  if FindDragTarget(P, True) <> Self then
    MouseLeave;
end;

procedure TplCheckBox.RemoveMouseTimer;
begin
  if MouseInControl = Self then
  begin
    MouseTimer.Enabled := False;
    MouseInControl := nil;
  end;
end;

procedure TplCheckBox.SetTransparent(const Value: Boolean);
begin
  FTransparent := Value;
  Invalidate;
end;

procedure TplCheckBox.WMMove(var Message: TLMMove);
begin
  inherited;
  if FTransparent then
    Invalidate;
end;

procedure TplCheckBox.WMSize(var Message: TLMSize);
begin
  inherited;
  if FTransparent then
    Invalidate;
end;

procedure TplCheckBox.SetBiDiMode(Value: TBiDiMode);
begin
  inherited;
  if BidiMode = bdRightToLeft then
    Layout := checkboxRight
  else
    Layout := checkboxLeft;
end;

end.
