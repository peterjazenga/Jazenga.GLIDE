{**********************************************************************
 Package pl_ExControls.pkg
 From PilotLogic Software House(http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit TplCheckListBoxUnit;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, Messages, LMessages,
  SysUtils, Classes, Graphics, Controls, ExtCtrls,plUtils;

type
  TplCheckListBox = class(TCustomControl)
  private
    FSelected: Integer;
    FTransparent: TplTransparentMode;
    FOnClickCheck: TNotifyEvent;
    cWheelMessage: Cardinal;
    scrollType: TScrollType;
    firstItem: Integer;
    maxItems: Integer;
    FSorted: Boolean;
    FItems: TStringList;
    FItemsRect: TList;
    FItemsHeight: Integer;
    FChecked: set of Byte;
    FScrollBars: Boolean;
    FUseAdvColors: Boolean;
    FAdvColorBorder: TAdvColors;
    FArrowColor: TColor;
    FCheckColor: TColor;
    FBorderColor: TColor;
    FItemsRectColor: TColor;
    FItemsSelectColor: TColor;
    procedure SetColors (Index: Integer; Value: TColor);
    procedure SetAdvColors (Index: Integer; Value: TAdvColors);
    procedure SetUseAdvColors (Value: Boolean);
    procedure SetSorted (Value: Boolean);
    procedure SetItems (Value: TStringList);
    procedure SetItemsRect;
    procedure SetItemsHeight (Value: Integer);
    function  GetChecked (Index: Integer): Boolean;
    procedure SetChecked (Index: Integer; Value: Boolean);
    function  GetSelCount: Integer;
    procedure SetScrollBars (Value: Boolean);
    function  GetItemIndex: Integer;
    procedure SetItemIndex (Value: Integer);
    procedure WMMove(var Message: TLMMove); message LM_MOVE;
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
    procedure CMEnabledChanged(var Message: TLMessage); message CM_ENABLEDCHANGED;
    procedure CMSysColorChange (var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMParentColorChanged(var Message: TLMessage); message CM_PARENTCOLORCHANGED;
    procedure ScrollTimerHandler (Sender: TObject);
    procedure ItemsChanged (Sender: TObject);
    procedure WMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
    procedure WMKillFocus(var Message: TLMKillFocus); message LM_KILLFOCUS;
    procedure CNKeyDown(var Message: TLMKeyDown); message CN_KEYDOWN;
    procedure WMMouseWheel(var Message: TLMMouseEvent); message LM_MOUSEWHEEL;
    procedure SetTransparent (const Value: TplTransparentMode);
  protected
    procedure CalcAdvColors;
    procedure DrawCheckRect (canvas: TCanvas; start: TPoint; checked: Boolean);
    procedure DrawScrollBar (canvas: TCanvas);
    procedure Paint; override;
    procedure Loaded; override;
    procedure MouseDown (Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp (Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure WndProc (var Message: TMessage); override;
    procedure SetBiDiMode(Value: TBiDiMode); override;

  public
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;
    property  Checked [Index: Integer]: Boolean read GetChecked write SetChecked;
    property  SelCount: Integer read GetSelCount;
    procedure Clear;
    property  ItemIndex: Integer read GetItemIndex write SetItemIndex;


    property ColorArrow: TColor index 0 read FArrowColor write SetColors ;
    property ColorBorder: TColor index 1 read FBorderColor write SetColors ;

    property ColorItemsRect: TColor index 2 read FItemsRectColor write SetColors ;
    property ColorItemsSelect: TColor index 3 read FItemsSelectColor write SetColors ;
    property ColorCheck: TColor index 4 read FCheckColor write SetColors ;

    property AdvColorBorder: TAdvColors index 0 read FAdvColorBorder write SetAdvColors ;
    property UseAdvColors: Boolean read FUseAdvColors write SetUseAdvColors ;

  published
    property Items: TStringList read FItems write SetItems;
    property ItemHeight: Integer read FItemsHeight write SetItemsHeight default 17;
    property ScrollBars: Boolean read FScrollBars write SetScrollBars default true;
    property Color default $00D6BBA3;
  
    property Sorted: Boolean read FSorted write SetSorted default false;
    property TransparentMode: TplTransparentMode read FTransparent write SetTransparent default tmNone;
    property Align;
    property Font;
    property ParentFont;
    property ParentColor;
    property ParentShowHint;
    property Enabled;
    property Visible;
    property PopupMenu;
    property ShowHint;

    property OnClick;
    property OnClickCheck: TNotifyEvent read FOnClickCheck write FOnClickCheck;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;

    property Anchors;
    property BiDiMode write SetBidiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;

  end;

implementation

var
  ScrollTimer: TTimer = nil;

const
  FTimerInterval = 600; 
  FScrollSpeed = 100;

constructor TplCheckListBox.Create (AOwner: TComponent);
begin
  inherited;
  if ScrollTimer = nil then
  begin
    ScrollTimer := TTimer.Create(nil);
    ScrollTimer.Enabled := False;
    ScrollTimer.Interval := FTimerInterval;
  end;
  ControlStyle := ControlStyle + [csOpaque];
  SetBounds(Left, Top, 137, 99);
  FItems := TStringList.Create;
  FItemsRect := TList.Create;
  FItemsHeight := 17;

  TStringList(FItems).OnChange := ItemsChanged;

  FScrollBars := true;
  firstItem := 0;

  FArrowColor := DefiColorArrow;
  FBorderColor := DefiColorBorder;

  FItemsRectColor := DefiItemsRectColor;
  FItemsSelectColor := defiItemsSelectColor;

  FCheckColor := DefiColorCheck;
  ParentColor := True;
  ParentFont := True;
  Enabled := true;
  Visible := true;

  FUseAdvColors := defiUseAdvColors;
  FAdvColorBorder := 40;

  FSorted := false;
  FTransparent := tmNone;
  FSelected := -1;

  color:=$00D6BBA3;
end;

destructor TplCheckListBox.Destroy;
var
  counter: Integer;
begin
  ScrollTimer.Free;
  ScrollTimer := nil;
  FItems.Free;
  for counter := 0 to FItemsRect.Count - 1 do
    Dispose(FItemsRect.Items[counter]);
  FItemsRect.Free;
  inherited;
end;

procedure TplCheckListBox.WndProc (var Message: TMessage);
begin
  if Message.Msg = cWheelMessage then
  begin
    SendMessage (Self.Handle, WM_MOUSEWHEEL, Message.wParam, Message.lParam);
  end;
  inherited;
end;

procedure TplCheckListBox.WMMouseWheel(var Message: TLMMouseEvent);
var
  fScrollLines: Integer;
begin
  if not(csDesigning in ComponentState) then
  begin
    SystemParametersInfo(SPI_GETWHEELSCROLLLINES, 0, @fScrollLines, 0);

    if (fScrollLines = 0) then
      fScrollLines := maxItems;

    if ShortInt(Message.Msg) = -WHEEL_DELTA then
      if firstItem + maxItems + fScrollLines <= FItems.Count then
        Inc(firstItem, fScrollLines)
      else
        if FItems.Count - maxItems < 0 then
          firstItem := 0
        else
          firstItem := FItems.Count - maxItems
    else
      if ShortInt(Message.Msg) = WHEEL_DELTA then
        if firstItem - fScrollLines < 0 then
          firstItem := 0
        else
          dec(firstItem, fScrollLines);
    Invalidate;
  end;
end;

procedure TplCheckListBox.ItemsChanged (Sender: TObject);
begin
  if Enabled then
  begin
    FChecked := FChecked - [0..High(Byte)];
    Invalidate;
  end;
end;

procedure TplCheckListBox.SetColors (Index: Integer; Value: TColor);
begin
  case Index of
    0: FArrowColor := Value;
    1: FBorderColor := Value;
    2: FItemsRectColor := Value;
    3: FItemsSelectColor := Value;
    4: FCheckColor := Value;
  end;
  Invalidate;       
end;

procedure TplCheckListBox.CalcAdvColors;
begin
  if FUseAdvColors then
  begin
    FBorderColor := CalcAdvancedColor(Color, FBorderColor, FAdvColorBorder, darken);
  end;
end;

procedure TplCheckListBox.SetAdvColors (Index: Integer; Value: TAdvColors);
begin
  case Index of
    0: FAdvColorBorder := Value;
  end;
  CalcAdvColors;
  Invalidate;
end;

procedure TplCheckListBox.SetUseAdvColors (Value: Boolean);
begin
  if Value <> FUseAdvColors then
  begin
    FUseAdvColors := Value;
    ParentColor := Value;
    CalcAdvColors;
    Invalidate;
  end;
end;

procedure TplCheckListBox.SetSorted (Value: Boolean);
begin
  if Value <> FSorted then
  begin
    FSorted := Value;
    FItems.Sorted := Value;
    FChecked := FChecked - [0..High(Byte)];
    Invalidate;
  end;
end;

procedure TplCheckListBox.SetItems (Value: TStringList);
var
  counter: Integer;
begin
  if Value.Count - 1 > High(Byte) then
    Exit;

  // delete all spaces at left and right
  for counter := 0 to Value.Count - 1 do
    Value[counter] := Trim(Value[counter]);

  FItems.Assign(Value);

  Invalidate;
end;

procedure TplCheckListBox.SetItemsRect;
var
  counter: Integer;
  ItemRect: ^TRect;
  position: TPoint;
begin
  // Delete all curent Rects
  FItemsRect.Clear;

  // calculate the maximum items to draw
  if ScrollBars then
    maxItems := (Height - 24) div (FItemsHeight + 2)
  else
    maxItems := (Height - 4) div (FItemsHeight + 2);

  // set left/top position for the the first item
  if ScrollBars then
   position := Point(ClientRect.left + 3, ClientRect.top + 13)
  else
    position := Point(ClientRect.left + 3, ClientRect.top + 3);

  for counter := 0 to maxItems - 1 do
  begin
    // create a new Item-Rect
    New(ItemRect);
    // calculate the Item-Rect
    ItemRect^ := Rect(position.x, position.y, ClientRect.Right - 3, position.y + FItemsHeight);
    // set left/top position for next Item-Rect
    position := Point(position.x, position.y + FItemsHeight + 2);
    // add the Item-Rect to the Items-Rect-List
    FItemsRect.Add(ItemRect);
  end;
  Invalidate;
end;

procedure TplCheckListBox.SetItemsHeight (Value: Integer);
begin
  if Value < 1 then
    Value := 1;

  FItemsHeight := Value;

  if not (csLoading in ComponentState) then
    if ScrollBars then
      SetBounds(Left, Top, Width, maxItems * (FItemsHeight + 2) + 24)
    else
      SetBounds(Left, Top, Width, maxItems * (FItemsHeight + 2) + 4);
      
  SetItemsRect;
end;

function TplCheckListBox.GetChecked (Index: Integer): Boolean;
begin
  Result := Index in FChecked;
end;

procedure TplCheckListBox.SetChecked (Index: Integer; Value: Boolean);
begin
  if Value then
    Include(FChecked, Index)
  else
    Exclude(FChecked, Index);
  Invalidate;
end;

procedure TplCheckListBox.SetBiDiMode(Value: TBiDiMode);
begin
  inherited;
  Invalidate;
end;

function TplCheckListBox.GetSelCount: Integer;
var
  counter: Integer;
begin
  Result := 0;
  for counter := 0 to High(Byte) do
    if counter in FChecked then
      Inc(Result);
end;

procedure TplCheckListBox.SetScrollBars (Value: Boolean);
begin
  if FScrollBars <> Value then
  begin
    FScrollBars := Value;
    if not (csLoading in ComponentState) then
      if Value then
        Height := Height + 20
      else
        Height := Height - 20;
    SetItemsRect;
  end;
end;

procedure TplCheckListBox.DrawScrollBar (canvas: TCanvas);
var
  x, y: Integer;
begin
  // Draw the ScrollBar background
  canvas.Brush.Color := Color;
  canvas.FillRect(Rect(ClientRect.Left, ClientRect.Top, ClientRect.Right, ClientRect.Top + 11));       // top part
  canvas.FillRect(Rect(ClientRect.Left, ClientRect.Bottom - 11, ClientRect.Right, ClientRect.Bottom)); // bottom part

  // Draw the ScrollBar border
  canvas.Brush.Color := FBorderColor;
  canvas.FrameRect(Rect(ClientRect.Left, ClientRect.Top, ClientRect.Right, ClientRect.Top + 11));       // top part
  canvas.FrameRect(Rect(ClientRect.Left, ClientRect.Bottom - 11, ClientRect.Right, ClientRect.Bottom)); // bottom part

  // Draw top-up arrow
  x := (ClientRect.Right - ClientRect.Left) div 2 - 5;
  y := ClientRect.Top + 4;

  if (firstItem <> 0) and Enabled then
  begin
    canvas.Brush.Color := FArrowColor;
    canvas.Pen.Color := FArrowColor;
    canvas.Polygon([Point(x + 4, y + 2), Point(x + 8, y + 2), Point(x + 6, y)]);
  end
  else
  begin
    canvas.Brush.Color := clWhite;
    canvas.Pen.Color := clWhite;
    Inc(x); Inc(y);
    canvas.Polygon([Point(x + 4, y + 2), Point(x + 8, y + 2), Point(x + 6, y)]);
    Dec(x); Dec(y);
    canvas.Brush.Color := clGray;
    canvas.Pen.Color := clGray;
    canvas.Polygon([Point(x + 4, y + 2), Point(x + 8, y + 2), Point(x + 6, y)]);
  end;

  // Draw the bottom-down arrow
  y := ClientRect.Bottom - 7;
  if (firstItem + maxItems + 1 <= FItems.Count) and Enabled then
  begin
    canvas.Brush.Color := FArrowColor;
    canvas.Pen.Color := FArrowColor;
    canvas.Polygon([Point(X + 4, Y), Point(X + 8, Y), Point(X + 6, Y + 2)]);
  end
  else
  begin
    canvas.Brush.Color := clWhite;
    canvas.Pen.Color := clWhite;
    Inc(x); Inc(y);
    canvas.Polygon([Point(X + 4, Y), Point(X + 8, Y), Point(X + 6, Y + 2)]);
    Dec(x); Dec(y);
    canvas.Brush.Color := clGray;
    canvas.Pen.Color := clGray;
    canvas.Polygon([Point(X + 4, Y), Point(X + 8, Y), Point(X + 6, Y + 2)]);
  end;
end;

procedure TplCheckListBox.DrawCheckRect (canvas: TCanvas; start: TPoint; checked: Boolean);
var
  CheckboxRect: TRect;
begin

  if BidiMode = bdRightToLeft then
    CheckboxRect := Rect(start.x - 14, start.y + 3, start.x - 3, start.y + 14) else
    CheckboxRect := Rect(start.x + 3, start.y + 3, start.x + 14, start.y + 14);

  canvas.pen.style := psSolid;
  canvas.pen.width := 1;

  // Background
  canvas.brush.color := FItemsRectColor;
  canvas.pen.color := FItemsRectColor;
  canvas.FillRect(CheckboxRect);

  // Tick
  if Checked then
  begin
    canvas.pen.color := FCheckColor;

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

procedure TplCheckListBox.Paint;
var
  counterRect, counterItem: Integer;
  itemRect: ^TRect;
  itemRectText: TRect;
  Format: UINT;
  r:trect;
begin

  if BidiMode = bdRightToLeft then
    Format := DT_RIGHT or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX  else
    Format := DT_LEFT or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX;

    // Clear Background
    case FTransparent of
      tmAlways:
        DrawParentImage(Self, Canvas);
      tmNone:
        begin
          Canvas.Brush.Color := FItemsRectColor;
          Canvas.FillRect(ClientRect);
        end;
      tmNotFocused:
        if Focused then
        begin
          Canvas.Brush.Color := FItemsRectColor;
          Canvas.FillRect(ClientRect);
        end
        else
          DrawParentImage(Self, Canvas);
    end;

    if DefiDrawFlat then
     begin
      // Draw Border
      Canvas.Brush.Color := FBorderColor;
      Canvas.FrameRect(ClientRect);
     end else
     begin
      //----------- Draw 3D ------------------
      r:=ClientRect;
      Canvas.pen.Color:=clGray;
      Canvas.MoveTo(0, 0);
      Canvas.LineTo(R.Right,0);
      Canvas.MoveTo(0, 0);
      Canvas.LineTo(0, r.Bottom);

      Canvas.pen.Color:=clWhite;
      Canvas.MoveTo(0, r.Bottom+1);
      Canvas.LineTo(R.Right+2, r.Bottom+1);
      Canvas.MoveTo(R.Right+1, 0);
      Canvas.LineTo(r.Right+1, r.Bottom+1);
     end;

    // Draw ScrollBars
    if ScrollBars then
      DrawScrollBar(Canvas);

    // Initialize the counter for the Items
    counterItem := firstItem;

    // Draw Items
    for counterRect := 0 to maxItems - 1 do
    begin
      itemRect := FItemsRect.Items[counterRect];
      if (counterItem <= FItems.Count - 1) then
      begin
        // Item is selected
        if counterItem = FSelected then
        begin
          // Fill ItemRect
          Canvas.brush.color := FItemsSelectColor;
          Canvas.FillRect(itemRect^);
          // Draw ItemBorder
          Canvas.brush.color := FBorderColor;
          Canvas.FrameRect(itemRect^);
        end;
        if counterItem in FChecked then
          if BidiMode = bdRightToLeft then
            DrawCheckRect(Canvas, Point(itemRect^.Right, itemRect^.top), true) else
            DrawCheckRect(Canvas, Point(itemRect^.left, itemRect^.top), true)
        else
          if BidiMode = bdRightToLeft then
            DrawCheckRect(Canvas, Point(itemRect^.Right, itemRect^.top), false) else
            DrawCheckRect(Canvas, Point(itemRect^.left, itemRect^.top), false);

        // Draw Item Text

        Canvas.brush.style := bsClear;

        itemRectText:=itemRect^;
        itemRectText.Left:=20;

        if Enabled then
          DrawText(Canvas.Handle, PChar(FItems[counterItem]), Length(FItems[counterItem]), itemRectText, Format)
        else
          begin
            OffsetRect(itemRect^, 1, 1);
            Canvas.Font.Color := clBtnHighlight;
            DrawText(Canvas.Handle, PChar(FItems[counterItem]), Length(FItems[counterItem]), itemRectText, Format);
            OffsetRect(itemRect^, -1, -1);
            Canvas.Font.Color := clBtnShadow;
            DrawText(Canvas.Handle, PChar(FItems[counterItem]), Length(FItems[counterItem]), itemRectText, Format);
          end;

        Inc(counterItem);
      end;
    end;

end;

procedure TplCheckListBox.MouseDown (Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  cursorPos: TPoint;
  counterRect: Integer;
  currentRect: ^TRect;
  checkRect: TRect;
begin
  GetCursorPos(cursorPos);
  cursorPos := ScreenToClient(cursorPos);

  if (FItems.Count > 0) and (Button = mbLeft) then
  begin
    for counterRect := 0 to FItemsRect.Count - 1 do
    begin
      currentRect := FItemsRect.Items[counterRect];

      if BidiMode = bdRightToLeft then
        checkRect := Rect(currentRect.right - 14, currentRect.top + 3, currentRect.right - 3, currentRect.Top + 14)
      else
        checkRect := Rect(currentRect.left + 3, currentRect.top + 3, currentRect.left + 14, currentRect.Top + 14);

      if PtInRect(checkRect, cursorPos) then
      begin
        if (firstItem + counterRect) in FChecked then
          Exclude(FChecked, firstItem + counterRect)
        else
          Include(FChecked, firstItem + counterRect);
        SetFocus;
        if Assigned(FOnClickCheck) then
          FOnClickCheck(Self);
        Invalidate;
        Exit;
      end
      else
        if PtInRect(currentRect^, cursorPos) then
        begin
          FSelected := firstItem + counterRect;
          SetFocus;
          Invalidate;
          Exit;
        end;
    end;
  end;

  if ScrollBars then
  begin
    if PtInRect(Rect(ClientRect.Left, ClientRect.Top, ClientRect.Right, ClientRect.Top + 11), cursorPos) then
    begin
      if (firstItem - 1) < 0 then
        firstItem := 0
      else
        Dec(firstItem);
      SetFocus;
      Invalidate;
      scrollType := up;
      if ScrollTimer.Enabled then
        ScrollTimer.Enabled := False;
      ScrollTimer.OnTimer := ScrollTimerHandler;
      ScrollTimer.Enabled := True;
    end;
    if PtInRect(Rect(ClientRect.Left, ClientRect.Bottom - 11, ClientRect.Right, ClientRect.Bottom), cursorPos) then
    begin
      if firstItem + maxItems + 1 <= FItems.Count then
        Inc(firstItem);
      SetFocus;
      Invalidate;
      scrollType := down;
      if ScrollTimer.Enabled then
        ScrollTimer.Enabled := False;
      ScrollTimer.OnTimer := ScrollTimerHandler;
      ScrollTimer.Enabled := True;
    end;
  end;
  Inherited;
end;

procedure TplCheckListBox.MouseUp (Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ScrollTimer.Enabled := False;
  ScrollTimer.Interval := FTimerInterval;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TplCheckListBox.ScrollTimerHandler (Sender: TObject);
begin
  ScrollTimer.Interval := FScrollSpeed;
  if scrollType = up then
    if (firstItem - 1) < 0 then
    begin
      firstItem := 0;
      ScrollTimer.Enabled := False;
    end
    else
      Dec(firstItem)
  else
    if firstItem + maxItems + 1 <= FItems.Count then
      Inc(firstItem)
    else
      ScrollTimer.Enabled := False;
  Invalidate;
end;

procedure TplCheckListBox.Loaded;
begin
  inherited;
  SetItemsRect;
end;

procedure TplCheckListBox.WMSize(var Message: TWMSize);
begin
  inherited;
  // Calculate the maximum items to draw
  if ScrollBars then
    maxItems := (Height - 24) div (FItemsHeight + 2) else
    maxItems := (Height - 4) div (FItemsHeight + 2);

  // Set the new Bounds
  if ScrollBars then
    SetBounds(Left, Top, Width, maxItems * (FItemsHeight + 2) + 24) else
    SetBounds(Left, Top, Width, maxItems * (FItemsHeight + 2) + 4);

  // Recalculate the itemRects
  SetItemsRect;

  if not (FTransparent = tmNone) then Invalidate;
end;

procedure TplCheckListBox.WMMove(var Message: TLMMove);
begin
  inherited;
  if not (FTransparent = tmNone) then Invalidate;
end;

procedure TplCheckListBox.CMEnabledChanged (var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TplCheckListBox.CMSysColorChange (var Message: TMessage);
begin
  if FUseAdvColors then
  begin
    ParentColor := True;
    CalcAdvColors;
  end;
  Invalidate;
end;

procedure TplCheckListBox.CMParentColorChanged(var Message: TLMessage);
begin
  inherited;
  if FUseAdvColors then
  begin
    ParentColor := True;
    CalcAdvColors;
  end;
  Invalidate;
end;

procedure TplCheckListBox.Clear;
begin
  FItems.Clear;
  FChecked := FChecked - [0..High(Byte)];
  FSelected := -1;
  Invalidate;
end;

procedure TplCheckListBox.SetTransparent (const Value: TplTransparentMode);
begin
  FTransparent := Value;
  Invalidate;
end;

procedure TplCheckListBox.WMSetFocus(var Message: TLMSetFocus);
begin
  inherited;
  if not (FTransparent = tmNone) then
    Invalidate;
end;

procedure TplCheckListBox.WMKillFocus(var Message: TLMKillFocus);
begin
  inherited;
  FSelected := -1;
  Invalidate;
end;

procedure TplCheckListBox.CNKeyDown(var Message: TLMKeyDown);
begin
  case Message.CharCode of
    VK_UP: 
      if (firstItem - 1) < 0 then
        firstItem := 0
      else
        Dec(firstItem);
    VK_DOWN:
      if firstItem + maxItems + 1 <= FItems.Count then
        Inc(firstItem);
    VK_PRIOR:
      if (firstItem - maxItems) < 0 then
        firstItem := 0
      else
        Dec(firstItem, maxItems);
    VK_NEXT:
      if firstItem + (maxItems * 2) <= FItems.Count then
        Inc(firstItem, maxItems)
      else
        firstItem := FItems.Count - maxItems;
    VK_SPACE:
      if FSelected in FChecked then
          Exclude(FChecked, FSelected)
        else
          Include(FChecked, FSelected);
  else                                          
    inherited;
  end;
  Invalidate;
end;

function TplCheckListBox.GetItemIndex: Integer;
begin
  Result := FSelected;
end;

procedure TplCheckListBox.SetItemIndex(Value: Integer);
begin
  if GetItemIndex <> Value then
  begin
    FSelected := Value;
    Invalidate;
  end;
end;

end.
