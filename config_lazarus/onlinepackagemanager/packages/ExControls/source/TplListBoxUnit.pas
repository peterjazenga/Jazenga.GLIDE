
{**********************************************************************
 Package pl_ExControls.pkg
 From PilotLogic Software House(http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit TplListBoxUnit;

{$MODE Delphi}

interface


uses
  LCLIntf, LCLType, LMessages, Messages,
  SysUtils, Classes, Graphics, Controls, ExtCtrls, plUtils;

type
  TplListBox = class(TCustomControl)
  private
    FTransparent: TplTransparentMode;
    cWheelMessage: Cardinal;
    scrollType: TScrollType;
    firstItem: Integer;
    maxItems: Integer;
    FSorted: Boolean;
    FItems: TStringList;
    FItemsRect: TList;
    FItemsHeight: Integer;
    FItemIndex: Integer;
    FSelected: set of Byte;
    FMultiSelect: Boolean;
    FScrollBars: Boolean;
    FUseAdvColors: Boolean;
    FAdvColorBorder: TAdvColors;
    FArrowColor: TColor;
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
    function  GetSelected (Index: Integer): Boolean;
    procedure SetSelected (Index: Integer; Value: Boolean);
    function  GetSelCount: Integer;
    procedure SetScrollBars (Value: Boolean);
    function  GetItemIndex: Integer;
    procedure SetItemIndex (Value: Integer);
    procedure SetMultiSelect (Value: Boolean);
    procedure CMEnabledChanged(var Message: TLMessage); message CM_ENABLEDCHANGED;
    procedure CMSysColorChange (var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMParentColorChanged(var Message: TLMessage); message CM_PARENTCOLORCHANGED;
    procedure CNKeyDown(var Message: TLMKeyDown); message CN_KEYDOWN;
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
    procedure WMMove(var Message: TLMMove); message LM_MOVE;
    procedure WMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
    procedure WMKillFocus(var Message: TLMKillFocus); message LM_KILLFOCUS;
    procedure WMMouseWheel(var Message: TLMMouseEvent); message LM_MOUSEWHEEL;
    procedure ScrollTimerHandler (Sender: TObject);
    procedure ItemsChanged (Sender: TObject);
    procedure SetTransparent (const Value: TplTransparentMode);
  protected
    procedure CalcAdvColors;
    procedure DrawScrollBar (aCanvas: TCanvas);
    procedure Paint; override;
    procedure Loaded; override;
    procedure MouseDown (Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp (Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure WndProc (var Message: TMessage); override;
    procedure SetBiDiMode(Value: TBiDiMode); override;
  public
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;
    property Selected [Index: Integer]: Boolean read GetSelected write SetSelected;
    property SelCount: Integer read GetSelCount;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;


    property ColorArrow: TColor index 0 read FArrowColor write SetColors ;
    property ColorBorder: TColor index 1 read FBorderColor write SetColors ;

    property ColorItemsRect: TColor index 2 read FItemsRectColor write SetColors ;
    property ColorItemsSelect: TColor index 3 read FItemsSelectColor write SetColors ;

    property AdvColorBorder: TAdvColors index 0 read FAdvColorBorder write SetAdvColors ;
    property UseAdvColors: Boolean read FUseAdvColors write SetUseAdvColors ;

  published
    property TransparentMode: TplTransparentMode read FTransparent write SetTransparent default tmNone;
    property Align;
    property Items: TStringList read FItems write SetItems;
    property ItemHeight: Integer read FItemsHeight write SetItemsHeight default 14;
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect default false;
    property ScrollBars: Boolean read FScrollBars write SetScrollBars default true;
    property Color default $00D6BBA3;
   
    property Sorted: Boolean read FSorted write SetSorted default false;
    property Font;
    property ParentFont;
    property ParentColor;
    property ParentShowHint;
    property Enabled;
    property Visible;
    property PopupMenu;
    property ShowHint;

    property OnClick;
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

constructor TplListBox.Create (AOwner: TComponent);
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
  FItemsHeight := 14;

  TStringList(FItems).OnChange := ItemsChanged;

  FMultiSelect := false;
  FScrollBars := true;
  firstItem := 0;
  FItemIndex := -1;

  FArrowColor := DefiColorArrow;
  FBorderColor := DefiColorBorder;

  FItemsRectColor := DefiItemsRectColor;
  FItemsSelectColor := defiItemsSelectColor;

  ParentColor := True;
  ParentFont := True;
  Enabled := true;
  Visible := true;

  FUseAdvColors := defiUseAdvColors;
  FAdvColorBorder := 40;

  FSorted := false;
  FTransparent := tmNone;

  color:=$00D6BBA3;

 // cWheelMessage:= RegisterWindowMessage(MSH_MOUSEWHEEL);   // ct9999
end;

destructor TplListBox.Destroy;
var
  i: Integer;
begin
  ScrollTimer.Free;
  ScrollTimer := nil;
  FItems.Free;

  for i := 0 to FItemsRect.Count - 1 do
    Dispose(FItemsRect.Items[i]);

  FItemsRect.Free;
  inherited;
end;

procedure TplListBox.WndProc (var Message: TMessage);
begin
  if Message.Msg = cWheelMessage then
  begin
    SendMessage (Self.Handle, WM_MOUSEWHEEL, Message.wParam, Message.lParam);
  end;
  inherited;
end;

procedure TplListBox.WMMouseWheel(var Message: TLMMouseEvent);
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

procedure TplListBox.ItemsChanged (Sender: TObject);
begin
  if Enabled then
  begin
    FSelected := FSelected - [0..High(Byte)];
    Invalidate;
  end;
end;

procedure TplListBox.SetColors (Index: Integer; Value: TColor);
begin
  case Index of
    0: FArrowColor := Value;
    1: FBorderColor := Value;
    2: FItemsRectColor := Value;
    3: FItemsSelectColor := Value;
  end;
  Invalidate;       
end;

procedure TplListBox.CalcAdvColors;
begin
  if FUseAdvColors then
  begin
    FBorderColor := CalcAdvancedColor(Color, FBorderColor, FAdvColorBorder, darken);
  end;
end;

procedure TplListBox.SetAdvColors (Index: Integer; Value: TAdvColors);
begin
  case Index of
    0: FAdvColorBorder := Value;
  end;
  CalcAdvColors;
  Invalidate;
end;

procedure TplListBox.SetUseAdvColors (Value: Boolean);
begin
  if Value <> FUseAdvColors then
  begin
    FUseAdvColors := Value;
    ParentColor := Value;
    CalcAdvColors;
    Invalidate;
  end;
end;

procedure TplListBox.SetSorted (Value: Boolean);
begin
  if Value <> FSorted then
  begin
    FSorted := Value;
    FItems.Sorted := Value;
    FSelected := FSelected - [0..High(Byte)];
    Invalidate;
  end;
end;

procedure TplListBox.SetItems (Value: TStringList);
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

procedure TplListBox.SetItemsRect;
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

procedure TplListBox.SetItemsHeight (Value: Integer);
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

function TplListBox.GetSelected (Index: Integer): Boolean;
begin
  Result := Index in FSelected;
end;

procedure TplListBox.SetSelected (Index: Integer; Value: Boolean);
begin
  if MultiSelect then
    if Value then
      Include(FSelected, Index)
    else
      Exclude(FSelected, Index)
  else
    begin
      FSelected := FSelected - [0..High(Byte)];
      if Value then
        Include(FSelected, Index)
      else
        Exclude(FSelected, Index);
    end;
  Invalidate;
end;

function TplListBox.GetSelCount: Integer;
var
  counter: Integer;
begin
  if MultiSelect then
    begin
      Result := 0;
      for counter := 0 to High(Byte) do
        if counter in FSelected then
          Inc(Result);
    end
  else
    Result := -1;
end;

procedure TplListBox.SetScrollBars (Value: Boolean);
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

procedure TplListBox.DrawScrollBar (aCanvas: TCanvas);
var
  x, y: Integer;
begin
  // Draw the ScrollBar background
  aCanvas.Brush.Color := Color;
  aCanvas.FillRect(Rect(ClientRect.Left, ClientRect.Top, ClientRect.Right, ClientRect.Top + 11));
  aCanvas.FillRect(Rect(ClientRect.Left, ClientRect.Bottom - 11, ClientRect.Right, ClientRect.Bottom));

  // Draw the ScrollBar border
  aCanvas.Brush.Color := FBorderColor;
  aCanvas.FrameRect(Rect(ClientRect.Left, ClientRect.Top, ClientRect.Right, ClientRect.Top + 11));
  aCanvas.FrameRect(Rect(ClientRect.Left, ClientRect.Bottom - 11, ClientRect.Right, ClientRect.Bottom));

  // Draw the up arrow
  x := (ClientRect.Right - ClientRect.Left) div 2 - 6;
  y := ClientRect.Top + 4;

  if (firstItem <> 0) and Enabled then
  begin
    aCanvas.Brush.Color := FArrowColor;
    aCanvas.Pen.Color := FArrowColor;
    aCanvas.Polygon([Point(x + 4, y + 2), Point(x + 8, y + 2), Point(x + 6, y)]);
  end
  else
  begin
    aCanvas.Brush.Color := clWhite;
    aCanvas.Pen.Color := clWhite;
    Inc(x); Inc(y);
    aCanvas.Polygon([Point(x + 4, y + 2), Point(x + 8, y + 2), Point(x + 6, y)]);
    Dec(x); Dec(y);
    aCanvas.Brush.Color := clGray;
    aCanvas.Pen.Color := clGray;
    aCanvas.Polygon([Point(x + 4, y + 2), Point(x + 8, y + 2), Point(x + 6, y)]);
  end;

  // Draw the down arrow
  y := ClientRect.Bottom - 7;
  if (firstItem + maxItems + 1 <= FItems.Count) and Enabled then
  begin
    aCanvas.Brush.Color := FArrowColor;
    aCanvas.Pen.Color := FArrowColor;
    aCanvas.Polygon([Point(X + 4, Y), Point(X + 8, Y), Point(X + 6, Y + 2)]);
  end
  else
  begin
    aCanvas.Brush.Color := clWhite;
    aCanvas.Pen.Color := clWhite;
    Inc(x); Inc(y);
    aCanvas.Polygon([Point(X + 4, Y), Point(X + 8, Y), Point(X + 6, Y + 2)]);
    Dec(x); Dec(y);
    aCanvas.Brush.Color := clGray;
    aCanvas.Pen.Color := clGray;
    aCanvas.Polygon([Point(X + 4, Y), Point(X + 8, Y), Point(X + 6, Y + 2)]);
  end;
end;

procedure TplListBox.Paint;
var
  counterRect, counterItem: Integer;
  itemRect: ^TRect;
  r:Trect;
  Format: UINT;
begin
  {$IFDEF DFS_COMPILER_4_UP}
  if BidiMode = bdRightToLeft then
    Format := DT_RIGHT or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX
  else
    Format := DT_LEFT or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX;
  {$ELSE}
  Format := DT_LEFT or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX;
  {$ENDIF}


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
        if counterItem in FSelected then
        begin
          // Fill ItemRect
          Canvas.brush.color := FItemsSelectColor;
          Canvas.FillRect(itemRect^);
          // Draw ItemBorder
          Canvas.brush.color := FBorderColor;
          Canvas.FrameRect(itemRect^);
        end;
        // Draw ItemText
        Canvas.brush.style := bsClear;
        InflateRect(itemRect^, -3, 0);
        if Enabled then
          DrawText(Canvas.Handle, PChar(FItems[counterItem]), Length(FItems[counterItem]), itemRect^, Format)
        else
          begin
            OffsetRect(itemRect^, 1, 1);
            Canvas.Font.Color := clBtnHighlight;
            DrawText(Canvas.Handle, PChar(FItems[counterItem]), Length(FItems[counterItem]), itemRect^, Format);
            OffsetRect(itemRect^, -1, -1);
            Canvas.Font.Color := clBtnShadow;
            DrawText(Canvas.Handle, PChar(FItems[counterItem]), Length(FItems[counterItem]), itemRect^, Format);
          end;
        InflateRect(itemRect^, 3, 0);
        Inc(counterItem);
      end;
    end;

end;

procedure TplListBox.MouseDown (Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  cursorPos: TPoint;
  counterRect: Integer;
  currentRect: ^TRect;
begin
  GetCursorPos(cursorPos);
  cursorPos := ScreenToClient(cursorPos);

  if (FItems.Count > 0) and (Button = mbLeft) then
  begin
    for counterRect := 0 to FItemsRect.Count - 1 do
    begin
      currentRect := FItemsRect.Items[counterRect];
      if PtInRect(currentRect^, cursorPos) then
      begin
        if MultiSelect then
        begin
          if (firstItem + counterRect) in FSelected then
            Exclude(FSelected, firstItem + counterRect)
          else
            Include(FSelected, firstItem + counterRect);
          FItemIndex := firstItem + counterRect;
        end
        else
        begin
          FSelected := FSelected - [0..High(Byte)];
          Include(FSelected, firstItem + counterRect);
          FItemIndex := firstItem + counterRect;
        end;
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

procedure TplListBox.MouseUp (Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ScrollTimer.Enabled := False;
  ScrollTimer.Interval := FTimerInterval;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TplListBox.ScrollTimerHandler (Sender: TObject);
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

procedure TplListBox.Loaded;
begin
  inherited;
  SetItemsRect;
end;

procedure TplListBox.WMSize (var Message: TWMSize);
begin
  inherited;
  // Calculate the maximum items to draw
  if ScrollBars then
    maxItems := (Height - 24) div (FItemsHeight + 2)
  else
    maxItems := (Height - 4) div (FItemsHeight + 2);

  // Set the new Bounds
  if ScrollBars then
    SetBounds(Left, Top, Width, maxItems * (FItemsHeight + 2) + 24)
  else
    SetBounds(Left, Top, Width, maxItems * (FItemsHeight + 2) + 4);

  // Recalculate the itemRects
  SetItemsRect;
end;

procedure TplListBox.CMEnabledChanged (var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TplListBox.CMSysColorChange (var Message: TMessage);
begin
  if FUseAdvColors then
  begin
    ParentColor := True;
    CalcAdvColors;
  end;
  Invalidate;
end;

procedure TplListBox.CMParentColorChanged(var Message: TLMessage);
begin
  inherited;
  if FUseAdvColors then
  begin
    ParentColor := True;
    CalcAdvColors;
  end;
  Invalidate;
end;

procedure TplListBox.SetTransparent (const Value: TplTransparentMode);
begin
  FTransparent := Value;
  Invalidate;
end;

procedure TplListBox.WMMove(var Message: TLMMove);
begin
  inherited;
  if not (FTransparent = tmNone) then
    Invalidate;
end;

procedure TplListBox.WMKillFocus(var Message: TLMKillFocus);
begin
  inherited;
  if not (FTransparent = tmNone) then
    Invalidate;
end;

procedure TplListBox.WMSetFocus(var Message: TLMSetFocus);
begin
  inherited;
  if not (FTransparent = tmNone) then
    Invalidate;
end;

procedure TplListBox.CNKeyDown(var Message: TLMKeyDown);
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
  else
    inherited;
  end;
  Invalidate;
end;

function TplListBox.GetItemIndex: Integer;
begin
  Result := FItemIndex;
end;

procedure TplListBox.SetItemIndex (Value: Integer);
begin
  if GetItemIndex <> Value then
  begin
    FItemIndex := Value;
    if MultiSelect then
    begin
      if (Value) in FSelected then
        Exclude(FSelected, Value)
      else
        Include(FSelected, Value);
    end
    else
    begin
      FSelected := FSelected - [0..High(Byte)];
      Include(FSelected, Value);
    end;
    Invalidate;
  end;
end;

procedure TplListBox.SetMultiSelect (Value: Boolean);
begin
  FMultiSelect := Value;
  if Value then
    FItemIndex := 0;
end;

procedure TplListBox.SetBiDiMode(Value: TBiDiMode);
begin
  inherited;
  Invalidate;
end;

end.
