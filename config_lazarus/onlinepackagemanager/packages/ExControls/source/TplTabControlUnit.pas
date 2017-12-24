
{**********************************************************************
 Package pl_ExControls.pkg
 From PilotLogic Software House(http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit TplTabControlUnit;

{$MODE Delphi}

interface


uses
  LCLIntf, LCLType, LMessages, Messages,
  Classes, Controls, Forms, Graphics, StdCtrls, SysUtils, plUtils;

type

  { TplTabControl }

  TplTabControl = class(TCustomControl)
  private
    FUseAdvColors: Boolean;
    FAdvColorBorder: TAdvColors;
    FTabPosition: TplTabPosition;
    FTabs: TStringList;
    FTabsRect: TList;
    FTabHeight: Integer;
    FTabSpacing: Integer;
    FActiveTab: Integer;
    FUnselectedColor: TColor;
    FBorderColor: TColor;
    FOnTabChanged: TNotifyEvent;
    FBorderWidth: Integer;
    procedure SetTabs (Value: TStringList);
    procedure SetTabPosition (Value: TplTabPosition);
    procedure SetTabHeight (Value: Integer);
    procedure SetTabSpacing (Value: Integer);
    procedure SetActiveTab (Value: Integer);
    procedure SetColors (Index: Integer; Value: TColor);
    procedure SetAdvColors (Index: Integer; Value: TAdvColors);
    procedure SetUseAdvColors (Value: Boolean);
    procedure SetTabRect;
    procedure ClearTabsRect;
    procedure CMDialogChar (var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMEnabledChanged(var Message: TLMessage); message CM_ENABLEDCHANGED;
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
    procedure CMSysColorChange (var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMParentColorChanged(var Message: TLMessage); message CM_PARENTCOLORCHANGED;
    procedure CMDesignHitTest(var Message: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure WMMove(var Message: TLMMove); message LM_MOVE;
    procedure SetBorderWidth(const Value: Integer);
  protected
    procedure CalcAdvColors;
    procedure MouseDown (Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Loaded; override;
    procedure Paint; override;
    procedure AlignControls (AControl: TControl; var Rect: TRect); override;
    procedure TabsChanged (Sender: TObject);
    procedure SetBiDiMode (Value: TBiDiMode); override;
  public
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property BorderWidth: Integer read FBorderWidth write SetBorderWidth default 0;
    property ColorBorder: TColor index 0 read FBorderColor write SetColors default $008396A0;
    property ColorUnselectedTab: TColor index 1 read FUnselectedColor write SetColors default $00996633;
    property AdvColorBorder: TAdvColors index 0 read FAdvColorBorder write SetAdvColors default 50;
    property UseAdvColors: Boolean read FUseAdvColors write SetUseAdvColors default false;
    property Tabs: TStringList read FTabs write SetTabs;
    property TabHeight: Integer read FTabHeight write SetTabHeight default 16;
    property TabSpacing: Integer read FTabSpacing write SetTabSpacing default 4;
    property TabPosition: TplTabPosition read FTabPosition write SetTabPosition ;
    property ActiveTab: Integer read FActiveTab write SetActiveTab default 0;
    property Font;
    property Color default clWhite;
    property ParentColor;
    property Enabled;
    property Visible;
    property Cursor;
    property ParentShowHint;
    property ParentFont;
    property ShowHint;
    property TabOrder;
    property TabStop;

    property OnEnter;
    property OnExit;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
    property OnTabChanged: TNotifyEvent read FOnTabChanged write FOnTabChanged;
    property Anchors;
    property BiDiMode write SetBidiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
  end;

implementation

procedure TplTabControl.CMDesignHitTest(var Message: TCMDesignHitTest);
begin
  case FTabPosition of
    tpxTop:
      if PtInRect(Rect(ClientRect.Left, ClientRect.Top, ClientRect.Right, ClientRect.Top + FTabHeight + 1), Point(message.XPos, message.YPos)) then
        Message.Result := 1
      else
        Message.Result := 0;
    tpxBottom:
      if PtInRect(Rect(ClientRect.Left, ClientRect.Bottom - FTabHeight, ClientRect.Right, ClientRect.Bottom), Point(message.XPos, message.YPos)) then
        Message.Result := 1
      else
        Message.Result := 0;
  end;
end;

constructor TplTabControl.Create (AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csAcceptsControls, csOpaque];
  SetBounds(Left, Top, 289, 193);
  FTabs := TStringList.Create;
  FTabs.OnChange := TabsChanged;
  FTabsRect := TList.Create;
  FTabHeight := 16;
  FTabSpacing := 4;
  FTabPosition := tpxTop;
  FActiveTab := 0;
  FBorderColor := $008396A0;
  FUnselectedColor := $00996633;
  FUseAdvColors := false;
  FAdvColorBorder := 50;

  Color := $00E1EAEB;
  ParentColor := true;
  ParentFont := true;
end;

destructor TplTabControl.Destroy;
begin
  FTabs.Free;
  ClearTabsRect;
  FTabsRect.Free;
  inherited;
end;

procedure TplTabControl.CalcAdvColors;
begin
  if FUseAdvColors then
  begin
    FBorderColor := CalcAdvancedColor(Color, FBorderColor, FAdvColorBorder, darken);
  end;
end;

procedure TplTabControl.SetAdvColors (Index: Integer; Value: TAdvColors);
begin
  case Index of
    0: FAdvColorBorder := Value;
  end;
  CalcAdvColors;
  Invalidate;
end;

procedure TplTabControl.SetUseAdvColors (Value: Boolean);
begin
  if Value <> FUseAdvColors then
  begin
    FUseAdvColors := Value;
    ParentColor := Value;
    CalcAdvColors;
    Invalidate;
  end;
end;

procedure TplTabControl.CMSysColorChange (var Message: TMessage);
begin
  if FUseAdvColors then
  begin
    ParentColor := True;
    CalcAdvColors;
  end;
  Invalidate;
end;

procedure TplTabControl.CMParentColorChanged(var Message: TLMessage);
begin
  inherited;
  if FUseAdvColors then
  begin
    ParentColor := True;
    CalcAdvColors;
  end;
  Invalidate;
end;

procedure TplTabControl.Loaded;
begin
  inherited;
  SetTabRect;
  Invalidate;
end;

procedure TplTabControl.WMSize(var Message: TLMSize);
begin
  inherited;
  SetTabRect;
  Invalidate;
end;

procedure TplTabControl.CMEnabledChanged(var Message: TLMessage);
begin
  inherited;
  Invalidate;
end;

procedure TplTabControl.SetTabPosition (Value: TplTabPosition);
var
  I: Integer;
  r: TRect;
begin
  if Value <> FTabPosition then
  begin
    for I := 0 to ControlCount - 1 do // reposition client-controls
    begin
      if Value = tpxTop then
        Controls[I].Top := Controls[I].Top + TabHeight
      else
        Controls[I].Top := Controls[I].Top - TabHeight;
    end;
    FTabPosition := Value;
    SetTabRect;
    Invalidate;
    r := ClientRect;
    AlignControls(self, r);
  end;
end;

procedure TplTabControl.SetActiveTab (Value: Integer);
begin
  if FTabs <> nil then
  begin
    if Value > (FTabs.Count - 1) then
      Value := FTabs.Count - 1
    else
      if Value < 0 then
        Value := 0;

    FActiveTab := Value;
    if Assigned(FOnTabChanged) then
      FOnTabChanged(Self);
    Invalidate;
  end
  else
    FActiveTab := 0;
  if csDesigning in ComponentState then
    if (GetParentForm(self) <> nil) and (GetParentForm(self).Designer <> nil) then
      GetParentForm(self).Designer.Modified;
end;

procedure TplTabControl.SetColors (Index: Integer; Value: TColor);
begin
  case Index of
    0: FBorderColor := Value;
    1: FUnselectedColor := Value;
  end;
  Invalidate;
end;

procedure TplTabControl.SetTabs (Value: TStringList);
var
  counter: Integer;
begin
  FTabs.Assign(Value);
  if FTabs.Count = 0 then // no tabs? then active tab = 0
    FActiveTab := 0
  else
    begin
      if (FTabs.Count - 1) < FActiveTab then // if activeTab > last-tab the activeTab = last-tab
        FActiveTab := FTabs.Count - 1;
      for counter := 0 to FTabs.Count - 1 do
        FTabs[counter] := Trim(FTabs[counter]); // delete all spaces at left and right
    end;
  SetTabRect;
  Invalidate;
end;

procedure TplTabControl.SetTabHeight (Value: Integer);
var
  r: TRect;
begin
  if Value < 0 then Value := 0; // TabHeigh can't negative
  FTabHeight := Value;
  SetTabRect;
  r := ClientRect;
  AlignControls(self, r);
  Invalidate;
end;

procedure TplTabControl.SetTabSpacing (Value: Integer);
begin
  if Value < 1 then Value := 1; // minimal tabspacing = 1 dot
  FTabSpacing := Value;
  SetTabRect;
  Invalidate;
end;

procedure TplTabControl.SetTabRect;
var
  TabCount: Integer;
  TabRect: ^TRect;
  position: TPoint;
  CaptionTextWidth: Integer;
  CaptionTextString: string;
begin
  // set the font and clear the tab-rect-list
  canvas.font := self.font;
  //FTabsRect.Clear;
  ClearTabsRect; // + frees every tabrect's memory

  // set left/top position for the the first tab
  case FTabPosition of
    tpxTop:
      if BidiMode = bdRightToLeft then
        position := Point(ClientRect.right, ClientRect.top)
      else
        position := Point(ClientRect.left, ClientRect.top);

    tpxBottom:
      if BidiMode = bdRightToLeft then
        position := Point(ClientRect.right, ClientRect.bottom - FTabHeight)
      else
        position := Point(ClientRect.left, ClientRect.bottom - FTabHeight);

  end;

  for TabCount := 0 to (FTabs.Count - 1) do
  begin
    New(TabRect); // create a new Tab-Rect
    if Pos('&', FTabs[TabCount]) <> 0 then // if & in an caption
    begin
      CaptionTextString := FTabs[TabCount]; // read the caption text
      Delete(CaptionTextString, Pos('&', FTabs[TabCount]), 1); // delete the &
      CaptionTextWidth := canvas.TextWidth(CaptionTextString); // calc the caption-width withou the &
    end
    else // else calc the caption-width
      CaptionTextWidth := canvas.TextWidth(FTabs[TabCount]);


    if BidiMode = bdRightToLeft then
    begin
      case FTabPosition of // set the rect
        tpxTop:
          TabRect^ := Rect(position.x - CaptionTextWidth - 20, position.y, position.x, FTabHeight);
        tpxBottom:
          TabRect^ := Rect(position.x - CaptionTextWidth - 20, position.y, position.x, position.y + FTabHeight);
      end;
      position := Point(position.x - CaptionTextWidth - 20 - FTabSpacing, position.y); // set left/top position for next rect
    end
    else
    begin
      case FTabPosition of // set the rect
        tpxTop:
          TabRect^ := Rect(position.x, position.y, position.x + CaptionTextWidth + 20, FTabHeight);
        tpxBottom:
          TabRect^ := Rect(position.x, position.y, position.x + CaptionTextWidth + 20, position.y + FTabHeight);
      end;
      position := Point(position.x + CaptionTextWidth + 20 + FTabSpacing, position.y); // set left/top position for next rect
    end;

    FTabsRect.Add(TabRect); // add the tab-rect to the tab-rect-list
  end;
end;

procedure TplTabControl.ClearTabsRect;
var
  i: integer;
  TabRect: ^TRect;
begin
  for i := 0 to FTabsRect.Count - 1 do
    begin
      TabRect := FTabsRect.Items[i];
      Dispose(TabRect);
    end;
  FTabsRect.Clear;
end;

procedure TplTabControl.CMDialogChar (var Message: TCMDialogChar);
var
  currentTab: Integer;
begin
  with Message do
  begin
    if FTabs.Count > 0 then
    begin
      for currentTab := 0 to FTabs.Count - 1 do
      begin
        if IsAccel(CharCode, FTabs[currentTab]) then
        begin
          if (FActiveTab <> currentTab) then
          begin
            SetActiveTab(currentTab);
            SetFocus;
          end;
          Result := 1; 
          break;
        end;
      end;
    end
    else
      inherited;
  end;
end;

procedure TplTabControl.Paint;
var
  TabCount: Integer;
  TempRect: ^TRect;
begin
    // Clear Background
    if FTabs.Count > 0 then
      DrawParentImage(Self, Canvas)
    else
    begin
      Canvas.Brush.Color := Self.Color;
      Canvas.FillRect(ClientRect);
    end;

    // Draw Border
    if FTabs.Count = 0 then
    begin
      Canvas.Brush.Color := FBorderColor;
      Canvas.FrameRect(ClientRect)
    end
    else
    begin
      Canvas.Pen.Color := FBorderColor;
      TempRect := FTabsRect.Items[FActiveTab];
      if ClientRect.left <> TempRect^.left then // if Active Tab not first tab then __|Tab|___
      begin
        case FTabPosition of
          tpxTop:
          begin
            Canvas.Polyline([Point(ClientRect.left, ClientRect.top + FTabHeight), Point(TempRect^.Left, ClientRect.top + FTabHeight)]);
            Canvas.Polyline([Point(TempRect^.Right-1, ClientRect.top + FTabHeight), Point(ClientRect.right, ClientRect.top + FTabHeight)]);
          end;
          tpxBottom:
          begin
            Canvas.Polyline([Point(ClientRect.left, ClientRect.bottom - FTabHeight - 1), Point(TempRect^.Left, ClientRect.bottom - FTabHeight - 1)]);
            Canvas.Polyline([Point(TempRect^.Right-1, ClientRect.bottom - FTabHeight - 1), Point(ClientRect.right, ClientRect.bottom - FTabHeight - 1)]);
          end;
        end;
      end
      else // else |Tab|___
        case FTabPosition of
          tpxTop:
            Canvas.Polyline([Point(TempRect^.Right-1, ClientRect.top + FTabHeight), Point(ClientRect.right, ClientRect.top + FTabHeight)]);
          tpxBottom:
            Canvas.Polyline([Point(TempRect^.Right-1, ClientRect.bottom - FTabHeight - 1), Point(ClientRect.right, ClientRect.bottom - FTabHeight - 1)]);
        end;
      // border of the control
      case FTabPosition of
        tpxTop:
          Canvas.Polyline([Point(ClientRect.left, ClientRect.top + FTabHeight), Point(ClientRect.left, ClientRect.bottom - 1), Point(ClientRect.right - 1, ClientRect.bottom - 1), Point(ClientRect.right - 1, ClientRect.top + FTabHeight)]);
        tpxBottom:
          Canvas.Polyline([Point(ClientRect.left, ClientRect.bottom - FTabHeight - 1), Point(ClientRect.left, ClientRect.top), Point(ClientRect.right - 1, ClientRect.top), Point(ClientRect.right - 1, ClientRect.bottom - FTabHeight - 1)]);
      end;
    end;

    case FTabPosition of
      tpxTop:
        begin
          Canvas.brush.color := Color;
          Canvas.FillRect(Rect(ClientRect.left + 1, ClientRect.top + FTabHeight + 1, ClientRect.right - 1, ClientRect.bottom - 1));
        end;
      tpxBottom:
        begin
          Canvas.brush.color := Color;
          Canvas.FillRect(Rect(ClientRect.left + 1, ClientRect.top + 1, ClientRect.right - 1, ClientRect.bottom - FTabHeight - 1));
        end;
    end;

    // Draw Tabs
    for TabCount := 0 to FTabs.Count - 1 do
    begin
      TempRect := FTabsRect.Items[TabCount];
      Canvas.brush.style := bsclear;
      Canvas.pen.color := clBlack;
      if TabCount = FActiveTab then // if Active Tab not first tab then draw border |^^^|
      begin
        Canvas.font.color := self.font.color;
        Canvas.brush.color := Color;
        Canvas.pen.color := FBorderColor;
        case FTabPosition of
          tpxTop:
            begin
              Canvas.FillRect(Rect(TempRect^.left, TempRect^.top, TempRect^.right - 1, TempRect^.bottom + 1));
              Canvas.Polyline([Point(TempRect^.Left, TempRect^.Bottom), Point(TempRect^.Left, TempRect^.Top), Point(TempRect^.Right-1, TempRect^.Top), Point(TempRect^.Right-1, TempRect^.Bottom)]);
            end;
          tpxBottom:
            begin
              Canvas.FillRect(Rect(TempRect^.left, TempRect^.top - 1, TempRect^.right - 1, TempRect^.bottom));
              Canvas.Polyline([Point(TempRect^.Left, TempRect^.top - 1), Point(TempRect^.Left, TempRect^.bottom - 1), Point(TempRect^.Right-1, TempRect^.bottom - 1), Point(TempRect^.Right-1, TempRect^.top - 1)]);
            end;
        end;
      end
      else // else only fill the tab
      begin
        Canvas.font.color := color;
        Canvas.brush.color := FUnselectedColor;
        Canvas.FillRect(TempRect^);
      end;
      Canvas.Brush.Style := bsClear;
      if (TabCount = FActiveTab) and not Enabled then
       begin
        Canvas.Font.Color := FUnselectedColor;
        DrawText(Canvas.Handle, PChar(FTabs[TabCount]), Length(FTabs[TabCount]), TempRect^, DT_CENTER or DT_VCENTER or DT_SINGLELINE)
       end
      else
        DrawText(Canvas.Handle, PChar(FTabs[TabCount]), Length(FTabs[TabCount]), TempRect^, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
    end;

end;

procedure TplTabControl.MouseDown (Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  cursorPos: TPoint;
  currentTab: Integer;
  currentRect: ^TRect;
begin
  GetCursorPos(cursorPos);
  cursorPos := ScreenToClient(cursorPos);

  if FTabs.Count > 0 then
  begin
    for currentTab := 0 to FTabs.Count - 1 do
    begin
      currentRect := FTabsRect.Items[currentTab];
      if PtInRect(currentRect^, cursorPos) then
        if (FActiveTab <> currentTab) then // only change when new tab selected
        begin
          SetActiveTab(currentTab);
          SetFocus;
        end;
    end;
  end;
  inherited;
end;

procedure TplTabControl.AlignControls (AControl: TControl; var Rect: TRect);
begin
  case FTabPosition of
    tpxTop:
      SetRect(Rect, clientRect.left + 1 + FBorderWidth, clientRect.top +
        TabHeight + 1 + FBorderWidth, clientRect.right - 1 - FBorderWidth,
        clientRect.bottom - 1 - FBorderWidth);
    tpxBottom:
      SetRect(Rect, clientRect.left + 1 + FBorderWidth, clientRect.top + 1 +
        FBorderWidth, clientRect.right - 1 - FBorderWidth, clientRect.bottom -
        TabHeight - 1 - FBorderWidth);
  end;
  inherited;
end;

procedure TplTabControl.WMMove(var Message: TLMMove);
begin
  inherited;
  Invalidate;
end;

procedure TplTabControl.SetBiDiMode(Value: TBiDiMode);
begin
  inherited;
  SetTabRect;
  Invalidate;
end;

procedure TplTabControl.TabsChanged (Sender: TObject);
begin
  SetTabRect;
  Invalidate;
end;

procedure TplTabControl.SetBorderWidth(const Value: Integer);
var
  r: TRect;
begin
  if Value <> FBorderWidth then
  begin
    FBorderWidth := Value;
    r := ClientRect;
    AlignControls(self, r);
  end;
end;

end.
