
{**********************************************************************
 Package pl_ExControls.pkg
 From PilotLogic Software House(http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit TplGroupBoxUnit;

interface


uses
  LCLIntf, LCLType, LMessages, Messages,
  SysUtils, Forms, Classes, Graphics, Controls, ExtCtrls, plUtils;

type
  TplGroupBox = class(TCustomControl)
  private
    FTransparent: Boolean;
    FUseAdvColors: Boolean;
    FAdvColorBorder: TAdvColors;
    FBorderColor: TColor;
    FBorder: TGroupBoxBorder;
    procedure SetAdvColors (Index: Integer; Value: TAdvColors);
    procedure SetUseAdvColors (Value: Boolean);
    procedure CMEnabledChanged(var Message: TLMessage); message CM_ENABLEDCHANGED;
    procedure CMTextChanged(var Message: TLMessage); message CM_TEXTCHANGED;
    procedure SetColors(const Index: Integer; const Value: TColor);
    procedure SetBorder(const Value: TGroupBoxBorder);
    procedure CMSysColorChange (var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMParentColorChanged(var Message: TLMessage); message CM_PARENTCOLORCHANGED;
    procedure CMDialogChar (var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure WMMove(var Message: TLMMove); message LM_MOVE;
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
    procedure SetTransparent (const Value: Boolean);
  protected
    procedure CalcAdvColors;
    procedure Paint; override;
   {$IFDEF DFS_COMPILER_4_UP}
    procedure SetBiDiMode(Value: TBiDiMode); override;
   {$ENDIF}
  public
    constructor Create (AOwner: TComponent); override;

    property ColorBorder: TColor index 0 read FBorderColor write SetColors ;
    property AdvColorBorder: TAdvColors index 0 read FAdvColorBorder write SetAdvColors ;
    property UseAdvColors: Boolean read FUseAdvColors write SetUseAdvColors ;

  published
    property Transparent: Boolean read FTransparent write SetTransparent default false;
    property Align;
    property Cursor;
    property Caption;
    property Font;
    property ParentFont;
    property ParentColor;
    property PopupMenu;
    property ShowHint;
    property ParentShowHint;
    property Enabled;
    property Visible;
    property TabOrder;
    property TabStop;
    property Hint;
    property HelpContext;
   
    property Border: TGroupBoxBorder read FBorder write SetBorder default brFull;

   {$IFDEF DFS_COMPILER_4_UP}
    property Anchors;
    property BiDiMode write SetBidiMode;
    property Constraints;
    property DragKind;
    property DragMode;
    property DragCursor;
    property ParentBiDiMode;
    property DockSite;
    property OnEndDock;
    property OnStartDock;
    property OnDockDrop;
    property OnDockOver;
    property OnGetSiteInfo;
    property OnUnDock;
   {$ENDIF}
   {$IFDEF DFS_DELPHI_5_UP}
    property OnContextPopup;
   {$ENDIF}
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

implementation

{ TplGroupBox }

constructor TplGroupBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls, csOpaque];
  FBorderColor := defiColorBorder;

  fUseAdvColors:=   defiUseAdvColors;
  FAdvColorBorder :=defiAdvColorBorder;
  Color:=DefiControlsBkColor;
  SetBounds(0, 0, 185, 105);
end;

procedure TplGroupBox.Paint;
var
  borderRect, textBounds: TRect;
  textHeight, textWidth: integer;
  Format: UINT;
begin
  borderRect := ClientRect;
  {$IFDEF DFS_COMPILER_4_UP}
  if BidiMode = bdRightToLeft then
    Format := DT_TOP or DT_RIGHT or DT_SINGLELINE
  else
    Format := DT_TOP or DT_LEFT or DT_SINGLELINE;
  {$ELSE}
  Format := DT_TOP or DT_LEFT or DT_SINGLELINE;
  {$ENDIF}


    textHeight := Canvas.TextHeight(caption);
    textWidth := Canvas.TextWidth(caption);

    {$IFDEF DFS_COMPILER_4_UP}
    if BidiMode = bdRightToLeft then
      textBounds := Rect(ClientRect.Right - 10 - textWidth, ClientRect.Top,
        ClientRect.Right - 10 , ClientRect.Top + textHeight)
    else
      textBounds := Rect(ClientRect.Left + 10, ClientRect.Top,
        ClientRect.Left + 10 + textWidth,
        ClientRect.Top + textHeight);
    {$ELSE}
    textBounds := Rect(ClientRect.Left + 10, ClientRect.Top,
      ClientRect.Left + 10 + textWidth,
      ClientRect.Top + textHeight);
    {$ENDIF}
    textBounds := Rect(ClientRect.Left + 10, ClientRect.Top,
      ClientRect.Right - 10,
      ClientRect.Top + textHeight);

    // Draw Background
    if FTransparent then
      DrawParentImage(Self, Canvas)
    else
    begin
      Canvas.Brush.Color :=DefiControlsBkColor;
      Canvas.FillRect(ClientRect);
    end;

    // Draw Border
    Canvas.Pen.Color := FBorderColor;
    case FBorder of
      brFull:
        {$IFDEF DFS_COMPILER_4_UP}
        if BidiMode = bdRightToLeft then
          Canvas.Polyline([Point(ClientRect.Right - 15 - textWidth, ClientRect.top + (textHeight div 2)),
            Point(ClientRect.left, ClientRect.top + (textHeight div 2)),
            Point(ClientRect.left, ClientRect.bottom-1), Point(ClientRect.right-1, ClientRect.bottom-1),
            Point(ClientRect.right-1, ClientRect.top + (textHeight div 2)),
            Point(ClientRect.Right - 7 , ClientRect.top + (textHeight div 2))])
        else
          Canvas.Polyline([Point(ClientRect.left + 5, ClientRect.top + (textHeight div 2)),
            Point(ClientRect.left, ClientRect.top + (textHeight div 2)),
            Point(ClientRect.left, ClientRect.bottom-1), Point(ClientRect.right-1, ClientRect.bottom-1),
            Point(ClientRect.right-1, ClientRect.top + (textHeight div 2)),
            Point(ClientRect.left + 12 + textWidth, ClientRect.top + (textHeight div 2))]);
        {$ELSE}
        Canvas.Polyline([Point(ClientRect.left + 5, ClientRect.top + (textHeight div 2)),
          Point(ClientRect.left, ClientRect.top + (textHeight div 2)),
          Point(ClientRect.left, ClientRect.bottom-1), Point(ClientRect.right-1, ClientRect.bottom-1),
          Point(ClientRect.right-1, ClientRect.top + (textHeight div 2)),
          Point(ClientRect.left + 12 + textWidth, ClientRect.top + (textHeight div 2))]);
        {$ENDIF}
      brOnlyTopLine:
        {$IFDEF DFS_COMPILER_4_UP}
        if BidiMode = bdRightToLeft then
        begin
          Canvas.Polyline([Point(ClientRect.right - 5, ClientRect.top + (textHeight div 2)), Point(ClientRect.right, ClientRect.top + (textHeight div 2))]);
          Canvas.Polyline([Point(ClientRect.left+1, ClientRect.top + (textHeight div 2)), Point(ClientRect.right - 12 - textWidth, ClientRect.top + (textHeight div 2))]);
        end
        else
        begin
          Canvas.Polyline([Point(ClientRect.left + 5, ClientRect.top + (textHeight div 2)), Point(ClientRect.left, ClientRect.top + (textHeight div 2))]);
          Canvas.Polyline([Point(ClientRect.right-1, ClientRect.top + (textHeight div 2)), Point(ClientRect.left + 12 + textWidth, ClientRect.top + (textHeight div 2))]);
        end;
        {$ELSE}
        begin
          Canvas.Polyline([Point(ClientRect.left + 5, ClientRect.top + (textHeight div 2)), Point(ClientRect.left, ClientRect.top + (Canvas.textHeight(caption) div 2))]);
          Canvas.Polyline([Point(ClientRect.right-1, ClientRect.top + (textHeight div 2)), Point(ClientRect.left + 12 + textWidth, ClientRect.top + (textHeight div 2))]);
        end;
        {$ENDIF}
    end;

    // Draw Text
    Canvas.Brush.Style := bsClear;
    if not Enabled then
    begin
      OffsetRect(textBounds, 1, 1);
      Canvas.Font.Color := clBtnHighlight;
      DrawText(Canvas.Handle, PChar(Caption), Length(Caption), textBounds, Format);
      OffsetRect(textBounds, -1, -1);
      Canvas.Font.Color := clBtnShadow;
      DrawText(Canvas.Handle, PChar(Caption), Length(Caption), textBounds, Format);
    end
    else
      DrawText(Canvas.Handle, PChar(Caption), Length(Caption), textBounds, Format);

end;

procedure TplGroupBox.CMTextChanged(var Message: TLMessage);
begin
  inherited;
  Invalidate;
end;

procedure TplGroupBox.SetColors(const Index: Integer;
  const Value: TColor);
begin
  case Index of
    0: FBorderColor := Value;
  end;
  Invalidate;
end;

procedure TplGroupBox.SetBorder(const Value: TGroupBoxBorder);
begin
  FBorder := Value;
  Invalidate;
end;

procedure TplGroupBox.SetAdvColors(Index: Integer; Value: TAdvColors);
begin
  case Index of
    0: FAdvColorBorder := Value;
  end;
  CalcAdvColors;
  Invalidate;
end;

procedure TplGroupBox.SetUseAdvColors(Value: Boolean);
begin
  if Value <> FUseAdvColors then
  begin
    FUseAdvColors := Value;
    ParentColor := Value;
    CalcAdvColors;
    Invalidate;
  end;
end;

procedure TplGroupBox.CalcAdvColors;
begin
  if FUseAdvColors then
  begin
    FBorderColor := CalcAdvancedColor(Color, FBorderColor, FAdvColorBorder, darken);
  end;
end;

procedure TplGroupBox.CMParentColorChanged(var Message: TLMessage);
begin
  inherited;
  if FUseAdvColors then
  begin
    ParentColor := True;
    CalcAdvColors;
  end;
  Invalidate;
end;

procedure TplGroupBox.CMSysColorChange(var Message: TMessage);
begin
  if FUseAdvColors then
  begin
    ParentColor := True;
    CalcAdvColors;
  end;
  Invalidate;
end;

procedure TplGroupBox.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(Message.CharCode, Caption) and CanFocus then
    begin
      SetFocus; 
      Result := 1;
    end;
end;

procedure TplGroupBox.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TplGroupBox.SetTransparent(const Value: Boolean);
begin
  FTransparent := Value;
  Invalidate;
end;

procedure TplGroupBox.WMMove(var Message: TLMMove);
begin
  inherited;
  if FTransparent then
    Invalidate;
end;

procedure TplGroupBox.WMSize(var Message: TLMSize);
begin
  inherited;
  if FTransparent then
    Invalidate;
end;

{$IFDEF DFS_COMPILER_4_UP}
procedure TplGroupBox.SetBiDiMode(Value: TBiDiMode);
begin
  inherited;
  Invalidate;
end;
{$ENDIF}

end.


