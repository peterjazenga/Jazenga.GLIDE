


unit cyBaseExtCtrls;

{$MODE Delphi}

{$I cyCompilerDefines.inc}

{$DEFINED CLR}

interface

uses LCLIntf, LCLType, LMessages,
  Themes,
  contnrs, types, SysUtils, math,
  Classes,  Graphics, StdCtrls, ExtCtrls, Controls, Messages, Dialogs,
  cyTypes, cyClasses, cyGraphics;

type

TCustomPanelExt = class(TCustomPanel)
  public
    constructor Create(AOwner: TComponent); override;
  end;


  TFlowStyle = (fsLeftRightTopBottom, fsRightLeftTopBottom, fsLeftRightBottomTop, fsRightLeftBottomTop,
                fsTopBottomLeftRight, fsBottomTopLeftRight, fsTopBottomRightLeft, fsBottomTopRightLeft);

  TCustomFlowPanel = class(TCustomPanelExt)
  private
    FControlList: TObjectList;
    FAutoWrap: Boolean;
    FFlowStyle: TFlowStyle;
    procedure SetAutoWrap(Value: Boolean);
    procedure SetFlowStyle(Value: TFlowStyle);
  //  procedure CMControlListChanging(var Message: TCMControlListChanging); message CM_CONTROLLISTCHANGING;
  protected
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function GetControlIndex(AControl: TControl): Integer;
    procedure SetControlIndex(AControl: TControl; Index: Integer);
    property AutoWrap: Boolean read FAutoWrap write SetAutoWrap default True;
    property FlowStyle: TFlowStyle read FFlowStyle write SetFlowStyle default fsLeftRightTopBottom;
  end;


  TSizeStyle = (ssAbsolute, ssPercent, ssAuto);

  TCustomGridPanel = class;
  EGridPanelException = class(Exception);

  TCellItem = class(TCollectionItem)
  private
    FSizeStyle: TSizeStyle;
    FValue: Double;
    FSize: Integer;
    FAutoAdded: Boolean;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure SetSizeStyle(Value: TSizeStyle);
    procedure SetValue(Value: Double);
    property Size: Integer read FSize write FSize;
    property AutoAdded: Boolean read FAutoAdded write FAutoAdded;
  public
    constructor Create(Collection: TCollection); override;
  published
    property SizeStyle: TSizeStyle read FSizeStyle write SetSizeStyle default ssPercent;
    property Value: Double read FValue write SetValue;
  end;

  TRowItem = class(TCellItem);

  TColumnItem = class(TCellItem);

  TCellCollection = class(TOwnedCollection)
  protected
    function GetAttrCount: Integer; override;
    function GetAttr(Index: Integer): string; override;
    function GetItemAttr(Index, ItemIndex: Integer): string; override;
    function GetItem(Index: Integer): TCellItem;
    procedure SetItem(Index: Integer; Value: TCellItem);
    procedure Update(Item: TCollectionItem); override;
  public
    function Owner: TCustomGridPanel;
    property Items[Index: Integer]: TCellItem read GetItem write SetItem; default;
  end;

  TCellSpan = 1..MaxInt;

  TRowCollection = class(TCellCollection)
  protected
    function GetItemAttr(Index, ItemIndex: Integer): string; override;
    procedure Notify(Item: TCollectionItem; Action: Classes.TCollectionNotification); override;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TRowItem;
  end;

  TColumnCollection = class(TCellCollection)
  protected
    function GetItemAttr(Index, ItemIndex: Integer): string; override;
    procedure Notify(Item: TCollectionItem; Action: Classes.TCollectionNotification); override;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TColumnItem;
  end;

  TControlItem = class(TCollectionItem)
  private
    FControl: TControl;
    FColumn, FRow: Integer;
    FColumnSpan, FRowSpan: TCellSpan;
    FPushed: Integer;
    function GetGridPanel: TCustomGridPanel;
    function GetPushed: Boolean;
    procedure SetColumn(Value: Integer);
    procedure SetColumnSpan(Value: TCellSpan);
    procedure SetControl(Value: TControl);
    procedure SetRow(Value: Integer);
    procedure SetRowSpan(Value: TCellSpan);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure InternalSetLocation(AColumn, ARow: Integer; APushed: Boolean; MoveExisting: Boolean);
    property GridPanel: TCustomGridPanel read GetGridPanel;
    property Pushed: Boolean read GetPushed;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure SetLocation(AColumn, ARow: Integer; APushed: Boolean = False);
  published
    property Column: Integer read FColumn write SetColumn;
    property ColumnSpan: TCellSpan read FColumnSpan write SetColumnSpan default 1;
    property Control: TControl read FControl write SetControl;
    property Row: Integer read FRow write SetRow;
    property RowSpan: TCellSpan read FRowSpan write SetRowSpan default 1;
  end;

  TControlCollection = class(TOwnedCollection)
  protected
    function GetControl(AColumn, ARow: Integer): TControl;
    function GetControlItem(AColumn, ARow: Integer): TControlItem;
    function GetItem(Index: Integer): TControlItem;
    procedure SetControl(AColumn, ARow: Integer; Value: TControl);
    procedure SetItem(Index: Integer; Value: TControlItem);
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TControlItem;
    procedure AddControl(AControl: TControl; AColumn: Integer = -1; ARow: Integer = -1);
    procedure RemoveControl(AControl: TControl);
    function IndexOf(AControl: TControl): Integer;
    function Owner: TCustomGridPanel;
    property Controls[AColumn, ARow: Integer]: TControl read GetControl write SetControl;
    property ControlItems[AColumn, ARow: Integer] : TControlItem read GetControlItem;
    property Items[Index: Integer]: TControlItem read GetItem write SetItem; default;
  end;

  TExpandStyle = (emAddRows, emAddColumns, emFixedSize);

  TCustomGridPanel = class(TCustomPanelExt)
  private
    FRowCollection: TRowCollection;
    FColumnCollection: TColumnCollection;
    FControlCollection: TControlCollection;
    FRecalcCellSizes: Boolean;
    FExpandStyle: TExpandStyle;
    procedure WMWindowPosChanged(var Message: TLMWindowPosChanged); message LM_WINDOWPOSCHANGED; 
    function GetCellCount: Integer;
    function GetCellSizes(AColumn, ARow: Integer): TPoint;
    function GetCellRect(AColumn, ARow: Integer): TRect;
    function GetColumnSpanIndex(AColumn, ARow: Integer): Integer;
    function GetRowSpanIndex(AColumn, ARow: Integer): Integer;
    procedure SetColumnCollection(const Value: TColumnCollection);
    procedure SetControlCollection(const Value: TControlCollection);
    procedure SetRowCollection(const Value: TRowCollection);
    procedure RecalcCellDimensions(const Rect: TRect);
{$IF NOT DEFINED(CLR)}
    procedure CMControlChange(var Message: TCMControlChange); message CM_CONTROLCHANGE;
{$ENDIF}
  protected
    function AutoAddColumn: TColumnItem;
    function AutoAddRow: TRowItem;
    function CellToCellIndex(AColumn, ARow: Integer): Integer;
    procedure CellIndexToCell(AIndex: Integer; var AColumn, ARow: Integer);
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    procedure Loaded; override;
    procedure Paint; override;
    procedure RemoveEmptyAutoAddColumns;
    procedure RemoveEmptyAutoAddRows;
    procedure UpdateControlOriginalParentSize(AControl: TControl; var AOriginalParentSize: TPoint);// override;
{$IF DEFINED(CLR)}
    procedure ControlChange(Inserting: Boolean; Child: TControl); override;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function IsColumnEmpty(AColumn: Integer): Boolean;
    function IsRowEmpty(ARow: Integer): Boolean;
    procedure UpdateControlsColumn(AColumn: Integer);
    procedure UpdateControlsRow(ARow: Integer);
    property ColumnSpanIndex[AColumn, ARow: Integer]: Integer read GetColumnSpanIndex;
    property CellCount: Integer read GetCellCount;
    property CellSize[AColumn, ARow: Integer]: TPoint read GetCellSizes;
    property CellRect[AColumn, ARow: Integer]: TRect read GetCellRect;
    property ColumnCollection: TColumnCollection read FColumnCollection write SetColumnCollection;
    property ControlCollection: TControlCollection read FControlCollection write SetControlCollection;
    property ExpandStyle: TExpandStyle read FExpandStyle write FExpandStyle default emAddRows;
    property RowCollection: TRowCollection read FRowCollection write SetRowCollection;
    property RowSpanIndex[AColumn, ARow: Integer]: Integer read GetRowSpanIndex;
  end;

implementation

const
    sCellMember = 'Member';
    sCellSizeType = 'Size Type';
    sCellValue = 'Value';
    sCellAutoSize = 'Auto';
    sCellPercentSize = 'Percent';
    sCellAbsoluteSize = 'Absolute';
    sCellColumn = 'Column%d';
    sCellRow = 'Row%d';


  sCannotAddFixedSize = 'Cannot add columns or rows while expand style is fixed size';
  sInvalidSpan = '''%d'' is not a valid span';
  sInvalidRowIndex = 'Row index, %d, out of bounds';
  sInvalidColumnIndex = 'Column index, %d, out of bounds';
  sInvalidControlItem = 'ControlItem.Control cannot be set to owning GridPanel';
  sCannotDeleteColumn = 'Cannot delete a column that contains controls';
  sCannotDeleteRow = 'Cannot delete a row that contains controls';


//======================== TCustomPanelExt ==========================

constructor TCustomPanelExt.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls];
end;

//======================== TCustomFlowPanel ==========================

constructor TCustomFlowPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoWrap := True;
  FControlList := TObjectList.Create(False);
end;

destructor TCustomFlowPanel.Destroy;
begin
  FControlList.Free;
  inherited;
end;

procedure TCustomFlowPanel.AlignControls(AControl: TControl; var Rect: TRect);
const
  XIncDir: array[TFlowStyle] of Integer = (1, -1, 1, -1, 1, 1, -1, -1);
  YIncDir: array[TFlowStyle] of Integer = (1, 1, -1, -1, 1, -1, 1, -1);
  YDeltaConst: array[TFlowStyle] of Integer = (0, 0, -1, -1, 0, 0, 0, 0);
  XDeltaConst: array[TFlowStyle] of Integer = (0, 0, 0, 0, 0, 0, -1, -1);
var
  I, Count: Integer;
  MaxHeight, MaxWidth: Integer;
  YDelta, XDelta: Integer;
  Position: TPoint;
  Size: TSize;
  Control: TControl;
begin
  if ControlCount > 0 then
  begin
    MaxHeight := 0;
    MaxWidth := 0;
    AdjustClientRect(Rect);
    if AutoSize then
      Rect := types.Rect(
        Rect.Left,
        Rect.Top,
        Rect.Left + ({ExplicitWidth - }(Width - (Rect.Right - Rect.Left))),
        Rect.Top + ({ExplicitHeight - }(Height - (Rect.Bottom - Rect.Top))));
    case FFlowStyle of
      fsLeftRightTopBottom,
      fsTopBottomLeftRight:
        Position := Rect.TopLeft;
      fsRightLeftTopBottom,
      fsTopBottomRightLeft:
        Position := Point(Rect.Right, Rect.Top);
      fsLeftRightBottomTop,
      fsBottomTopLeftRight:
        Position := Point(Rect.Left, Rect.Bottom);
      fsRightLeftBottomTop,
      fsBottomTopRightLeft:
        Position := Rect.BottomRight;
    end;

      Count := FControlList.Count - 1;

    for I := 0 to Count do
    begin

      Control := TControl(FControlList[I]);
      if not Control.Visible and not (csDesigning in ComponentState) then
        Continue;

    //  Size.cx := Control.Margins.ControlWidth;
    //  Size.cy := Control.Margins.ControlHeight;

      Size.cx := Control.Width;
      Size.cy := Control.Height;


      case FFlowStyle of
        fsLeftRightTopBottom,
        fsLeftRightBottomTop:
          if (MaxHeight > 0) and (Position.X + Size.cx >= Rect.Right) and FAutoWrap then
          begin
            Inc(Position.Y, MaxHeight * YIncDir[FFlowStyle]);
            MaxHeight := 0;
            Position.X := Rect.Left;
          end;
        fsRightLeftTopBottom,
        fsRightLeftBottomTop:
          begin
            Dec(Position.X, Size.cx);
            if (MaxHeight > 0) and (Position.X <= 0) and FAutoWrap then
            begin
              Inc(Position.Y, MaxHeight * YIncDir[FFlowStyle]);
              MaxHeight := 0;
              Position.X := Rect.Right - Size.cx;
            end;
          end;
        fsTopBottomLeftRight,
        fsTopBottomRightLeft:
          if (MaxWidth > 0) and (Position.Y + Size.cy >= Rect.Bottom) and FAutoWrap then
          begin
            Inc(Position.X, MaxWidth * XIncDir[FFlowStyle]);
            MaxWidth := 0;
            Position.Y := Rect.Top;
          end;
        fsBottomTopLeftRight,
        fsBottomTopRightLeft:
          begin
            Dec(Position.Y, Size.cy);
            if (MaxWidth > 0) and (Position.Y <= 0) and FAutoWrap then
            begin
              Inc(Position.X, MaxWidth * XIncDir[FFlowStyle]);
              MaxWidth := 0;
              Position.Y := Rect.Bottom - Size.cy;
            end;
          end;
      end;
      YDelta := YDeltaConst[FFlowStyle] * Size.cy;
      XDelta := XDeltaConst[FFlowStyle] * Size.cx;
      if Size.cy > MaxHeight then
        MaxHeight := Size.cy;
      if Size.cx > MaxWidth then
        MaxWidth := Size.cx;

     // Control.Margins.SetControlBounds(Position.X + XDelta, Position.Y + YDelta, Size.cx, Size.cy);

      Control.SetBounds(Position.X + XDelta, Position.Y + YDelta, Size.cx, Size.cy);

      if FFlowStyle in [fsLeftRightTopBottom, fsLeftRightBottomTop] then
        Inc(Position.X, Size.cx * XIncDir[FFlowStyle])
      else if FFlowStyle in [fsTopBottomLeftRight, fsTopBottomRightLeft] then
        Inc(Position.Y, Size.cy + YIncDir[FFlowStyle]);
    end;
    ControlsAligned;
  end;
//  if Showing then AdjustSize;
end;


{
procedure TCustomFlowPanel.CMControlListChanging(var Message: TCMControlListChanging);
begin
  inherited;
  if Message.Inserting and (Message.ControlListItem.Parent = Self) then
  begin
    if FControlList.IndexOf(Message.ControlListItem.Control) < 0 then
      FControlList.Add(Message.ControlListItem.Control);
  end else
    FControlList.Remove(Message.ControlListItem.Control);
end;
}


procedure TCustomFlowPanel.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
  Control: TControl;
begin
  for I := 0 to FControlList.Count - 1 do
  begin
    Control := TControl(FControlList[I]);
    if Control.Owner = Root then Proc(Control);
  end;
end;

function TCustomFlowPanel.GetControlIndex(AControl: TControl): Integer;
begin
  Result := FControlList.IndexOf(AControl);
end;

procedure TCustomFlowPanel.SetAutoWrap(Value: Boolean);
begin
  if FAutoWrap <> Value then
  begin
    FAutoWrap := Value;
    Realign;
  end;
end;

procedure TCustomFlowPanel.SetControlIndex(AControl: TControl; Index: Integer);
var
  CurIndex: Integer;
begin
  CurIndex := GetControlIndex(AControl);
  if (CurIndex > -1) and (CurIndex <> Index) and (Index < FControlList.Count) then
  begin
    FControlList.Move(CurIndex, Index);
    Realign;
  end;
end;

procedure TCustomFlowPanel.SetFlowStyle(Value: TFlowStyle);
begin
  if FFlowStyle <> Value then
  begin
    FFlowStyle := Value;
    Realign;
  end;
end;

//================================================

{ TCellItem }

procedure TCellItem.AssignTo(Dest: TPersistent);
begin
  if Dest is TCellItem then
    with TCellItem(Dest) do
    begin
      FSizeStyle := Self.FSizeStyle;
      FValue := Self.FValue;
      FSize := Self.FSize;
    end;
end;

constructor TCellItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FSizeStyle := ssPercent;
end;

procedure TCellItem.SetSizeStyle(Value: TSizeStyle);
begin
  if Value <> FSizeStyle then
  begin
    FSizeStyle := Value;
    Changed(False);
  end;
end;

procedure TCellItem.SetValue(Value: Double);
begin
  if Value <> FValue then
  begin
    if FSizeStyle = ssAbsolute then
    begin
      FSize := Trunc(Value);
      FValue := FSize;
    end else
      FValue := Value;
    Changed(False);
  end;
end;

//======================== TCellCollection ==============================

function TCellCollection.GetAttr(Index: Integer): string;
begin
  case Index of
    0: Result := sCellMember;
    1: Result := sCellSizeType;
    2: Result := sCellValue;
  else
    Result := '';
  end;
end;

function TCellCollection.GetAttrCount: Integer;
begin
  Result := 3;
end;

function TCellCollection.GetItem(Index: Integer): TCellItem;
begin
  Result := TCellItem(inherited GetItem(Index));
end;

function TCellCollection.GetItemAttr(Index, ItemIndex: Integer): string;

  function GetSizeStyleString(Index: Integer): string;
  begin
    with Items[Index] do
      case SizeStyle of
        ssAbsolute: Result := sCellAbsoluteSize;
        ssPercent: Result := sCellPercentSize;
        ssAuto: Result := sCellAutoSize;
      else
        Result := '';
      end;
  end;

  function GetValueString(Index: Integer): string;
  begin
    with Items[Index] do
      if SizeStyle = ssAbsolute then
        Result := IntToStr(Trunc(Value))
      else if SizeStyle = ssPercent then
        Result := Format('%3.2f%%', [Value]) // do not localize
      else Result := sCellAutoSize;
  end;

begin
  case Index of
    1: Result := GetSizeStyleString(ItemIndex);
    2: Result := GetValueString(ItemIndex);
  else
    Result := '';
  end;
end;

function TCellCollection.Owner: TCustomGridPanel;
begin
  Result := GetOwner as TCustomGridPanel;
end;

procedure TCellCollection.SetItem(Index: Integer; Value: TCellItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TCellCollection.Update(Item: TCollectionItem);
begin
  inherited;
  if Owner <> nil then
    with Owner do
    begin
      FRecalcCellSizes := True;
      Invalidate;
      Realign;
    end;
end;

//======================== TRowCollection ====================================

constructor TRowCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TRowItem);
end;

function TRowCollection.Add: TRowItem;
begin
  Result := TRowItem(inherited Add);
end;

function TRowCollection.GetItemAttr(Index, ItemIndex: Integer): string;
begin
  if Index = 0 then
    Result := Format(sCellRow, [ItemIndex])
  else
    Result := inherited GetItemAttr(Index, ItemIndex);
end;

procedure TRowCollection.Notify(Item: TCollectionItem;
  Action: Classes.TCollectionNotification);
begin
  inherited;
  if (Action = Classes.TCollectionNotification.cnExtracting) and not (csDestroying in Owner.ComponentState) and
     not (csUpdating in Owner.ComponentState) then
    if not Owner.IsRowEmpty(Item.Index) then
      raise EGridPanelException.Create(sCannotDeleteRow)
    else Owner.UpdateControlsRow(Item.Index);
end;

//====================== TColumnCollection =====================================

constructor TColumnCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TColumnItem);
end;

function TColumnCollection.Add: TColumnItem;
begin
  Result := TColumnItem(inherited Add);
end;

function TColumnCollection.GetItemAttr(Index, ItemIndex: Integer): string;
begin
  if Index = 0 then
    Result := Format(sCellColumn, [ItemIndex])
  else
    Result := inherited GetItemAttr(Index, ItemIndex);
end;

procedure TColumnCollection.Notify(Item: TCollectionItem;
  Action: Classes.TCollectionNotification);
begin
  inherited;
  if (Action = Classes.TCollectionNotification.cnExtracting) and not (csDestroying in Owner.ComponentState) and
     not (csUpdating in Owner.ComponentState) then
    if not Owner.IsColumnEmpty(Item.Index) then
      raise EGridPanelException.Create(sCannotDeleteColumn)
    else
      Owner.UpdateControlsColumn(Item.Index);
end;

//==================== TControlCollection ==========================================

function TControlCollection.Add: TControlItem;
begin
  Result := TControlItem(inherited Add);
end;

procedure TControlCollection.AddControl(AControl: TControl; AColumn, ARow: Integer);

  procedure PlaceInCell(ControlItem: TControlItem);
  var
    I, J: Integer;
  begin
    with ControlItem do
    try
      Control := AControl;
      FRow := -1;
      FColumn := -1;
      if (ARow = -1) and (AColumn > -1) then
      begin
        for I := 0 to Owner.RowCollection.Count - 1 do
          if Controls[AColumn, I] = nil then
          begin
            Row := I;
            Column := AColumn;
            Exit;
          end;
        AColumn := -1;
      end;
      if (AColumn = -1) and (ARow > -1) then
      begin
        for I := 0 to Owner.ColumnCollection.Count - 1 do
          if Controls[I, ARow] = nil then
          begin
            Column := I;
            Row := ARow;
            Exit;
          end;
        ARow := -1;
      end;
      if (AColumn > -1) and (ARow > -1) then
      begin
        if Controls[AColumn, ARow] = nil then
        begin
          Column := AColumn;
          Row := ARow;
          Exit;
        end;
        AColumn := -1;
        ARow := -1;
      end;
      if (ARow = -1) and (AColumn = -1) then
      begin
        for J := 0 to Owner.RowCollection.Count - 1 do
          for I := 0 to Owner.ColumnCollection.Count - 1 do
            if Controls[I, J] = nil then
            begin
              Row := J;
              Column := I;
              Exit;
            end;
      end;
      if (Row = -1) or (Column = -1) then
        if (Owner <> nil) and (Owner.ExpandStyle <> emFixedSize) then
        begin
          if Owner.ExpandStyle = emAddRows then
            Owner.AutoAddRow
          else
            Owner.AutoAddColumn;
          PlaceInCell(ControlItem);
        end else
          raise EGridPanelException.Create(sCannotAddFixedSize);
    except
      Control := nil;
      Free;
      raise;
    end;
  end;

begin
  if IndexOf(AControl) < 0 then
    PlaceInCell(Add);
end;

constructor TControlCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TControlItem);
end;

function TControlCollection.GetControl(AColumn, ARow: Integer): TControl;
var
  ControlItem: TControlItem;
begin
  ControlItem := GetControlItem(AColumn, ARow);
  if ControlItem <> nil then
    Result := ControlItem.Control
  else
    Result := nil;
end;

function TControlCollection.GetControlItem(AColumn, ARow: Integer): TControlItem;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := TControlItem(Items[I]);
    if (ARow >= Result.Row) and (ARow <= Result.Row + Result.RowSpan - 1) and
      (AColumn >= Result.Column) and (AColumn <= Result.Column + Result.ColumnSpan - 1) then
      Exit;
  end;
  Result := nil;
end;

function TControlCollection.GetItem(Index: Integer): TControlItem;
begin
  Result := TControlItem(inherited GetItem(Index));
end;

function TControlCollection.IndexOf(AControl: TControl): Integer;
begin
  for Result := 0 to Count - 1 do
    if TControlItem(Items[Result]).Control = AControl then
      Exit;
  Result := -1;
end;

function TControlCollection.Owner: TCustomGridPanel;
begin
  Result := TCustomGridPanel(GetOwner);
end;

procedure TControlCollection.RemoveControl(AControl: TControl);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if Items[I].Control = AControl then
    begin
      Items[I].Control := nil;
      Delete(I);
      Exit;
    end;
end;

procedure TControlCollection.SetControl(AColumn, ARow: Integer; Value: TControl);
var
  Index: Integer;
  ControlItem: TControlItem;
begin
  if Owner <> nil then
  begin
    if (AColumn < 0) or (AColumn >= Owner.ColumnCollection.Count) then
      raise EGridPanelException.CreateFmt(sInvalidColumnIndex, [AColumn]);
    if (ARow < 0) or (ARow >= Owner.RowCollection.Count) then
      raise EGridPanelException.CreateFmt(sInvalidRowIndex, [ARow]);
    Index := IndexOf(Value);
    if Index > -1 then
    begin
      ControlItem := Items[Index];
      ControlItem.FColumn := AColumn;
      ControlItem.FRow := ARow;
    end else
      AddControl(Value, AColumn, ARow);
  end;
end;

procedure TControlCollection.SetItem(Index: Integer; Value: TControlItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TControlCollection.Update(Item: TCollectionItem);
begin
  inherited;
  if Owner <> nil then
  begin
    Owner.FRecalcCellSizes := True;
    Owner.Invalidate;
    Owner.Realign;
  end;
end;

//====================== TControlItem ======================================

procedure TControlItem.AssignTo(Dest: TPersistent);
begin
  if Dest is TControlItem then
    with TControlItem(Dest) do
    begin
      FControl := Self.Control;
      FRow := Self.Row;
      FColumn := Self.Column;
      Changed(False);
    end;
end;

constructor TControlItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FRowSpan := 1;
  FColumnSpan := 1;
end;

destructor TControlItem.Destroy;
var
  LControl: TControl;
begin
  if Assigned(FControl) and not (csLoading in GridPanel.ComponentState) and
     not (csUpdating in GridPanel.ComponentState) and
     not (csDestroying in GridPanel.ComponentState) then
  begin
    LControl := FControl;
    FControl := nil;
    GridPanel.RemoveControl(LControl);
    LControl.Free;
  end;
  inherited;
end;

function TControlItem.GetGridPanel: TCustomGridPanel;
var
  Owner: TControlCollection;
begin
  Owner := TControlCollection(GetOwner);
  if Owner <> nil then
    Result := Owner.Owner
  else
    Result := nil;
end;

procedure TControlItem.SetControl(Value: TControl);
begin
  if FControl <> Value then
  begin
{$IF DEFINED(CLR)}
    if Assigned(Value) and Value.Equals(GridPanel) then
{$ELSE}
    if Value = GridPanel then
{$ENDIF}
      raise EGridPanelException.Create(sInvalidControlItem);
    FControl := Value;
    Changed(False);
  end;
end;

procedure TControlItem.SetColumn(Value: Integer);
begin
  if FColumn <> Value then
  begin
    if not (csLoading in GridPanel.ComponentState) then
    begin
      if (Value < 0)or (Value > GridPanel.ColumnCollection.Count - 1) then
        raise EGridPanelException.CreateFmt(sInvalidColumnIndex, [Value]);
      InternalSetLocation(Value, FRow, False, True);
    end
    else
      FColumn := Value;
  end;
end;

procedure TControlItem.SetRow(Value: Integer);
begin
  if FRow <> Value then
  begin
    if not (csLoading in GridPanel.ComponentState) then
    begin
      if (Value < 0) or (Value > GridPanel.RowCollection.Count - 1) then
        raise EGridPanelException.CreateFmt(sInvalidRowIndex, [Value]);
      InternalSetLocation(FColumn, Value, False, True);
    end
    else
      FRow := Value;
  end;
end;

type
  TNewLocationRec = record
    ControlItem: TControlItem;
    NewColumn, NewRow: Integer;
    Pushed: Boolean;
  end;
  TNewLocationRecs = array of TNewLocationRec;

  TNewLocations = class
  private
    FNewLocations: TNewLocationRecs;
    FCount: Integer;
  public
    function AddNewLocation(AControlItem: TControlItem; ANewColumn, ANewRow: Integer; APushed: Boolean = False): Integer;
    procedure ApplyNewLocations;
    property Count: Integer read FCount;
    property NewLocations: TNewLocationRecs read FNewLocations;
  end;

function TNewLocations.AddNewLocation(AControlItem: TControlItem; ANewColumn, ANewRow: Integer; APushed: Boolean): Integer;
begin
  if FCount = Length(FNewLocations) then
    SetLength(FNewLocations, Length(FNewLocations) + 10);
  Result := FCount;
  Inc(FCount);
  with FNewLocations[Result] do
  begin
    ControlItem := AControlItem;
    NewColumn := ANewColumn;
    NewRow := ANewRow;
    Pushed := APushed;
  end;
end;

procedure TNewLocations.ApplyNewLocations;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
    with FNewLocations[I] do
      if ControlItem <> nil then
        ControlItem.InternalSetLocation(NewColumn, NewRow, Pushed, False);
end;

procedure TControlItem.SetRowSpan(Value: TCellSpan);
var
  I, Delta: Integer;
  Collection: TControlCollection;
  ControlItem: TControlItem;
  NumToAdd, NumRows, MoveBy, LColumn, LRow, IndexDelta: Integer;
  Span: TCellSpan;
  NewLocations: TNewLocations;
begin
  if FRowSpan <> Value then
  begin
    if Value < 1 then
      raise EGridPanelException.CreateFmt(sInvalidSpan, [Value]);
    Collection := TControlCollection(GetOwner);
    if Collection = nil then Exit;
    GridPanel.DisableAlign;
    try
      NewLocations := TNewLocations.Create;
      try
        if FRowSpan > Value then
        begin
          Delta := FRowSpan - Value;
          FRowSpan := Value;
          if GridPanel.ExpandStyle in [emAddRows, emFixedSize] then
          begin
            NumRows := GridPanel.RowCollection.Count;
            // Move the controls below up to fill in the space.
            for I := FRow + FRowSpan + Delta to NumRows - 1 do
            begin
              ControlItem := Collection.ControlItems[FColumn, I];
              if ControlItem <> nil then
                if ControlItem.Pushed then
                  NewLocations.AddNewLocation(ControlItem, FColumn, I - Delta, False)
                else
                  Break;
            end;
            NewLocations.ApplyNewLocations;
            GridPanel.RemoveEmptyAutoAddRows;
          end else
          begin
            // Scan forward in row primary order, removing Delta from position of each
            // control item, unwrapping where nessesary, until the last control is reached
            // or a non "pushed" control is found.
            for I := GridPanel.RowSpanIndex[FColumn, FRow] to GridPanel.CellCount - 1 do
            begin
              GridPanel.CellIndexToCell(I, LColumn, LRow);
              ControlItem := Collection.ControlItems[LColumn, LRow];
              if ControlItem <> nil then
              begin
                if ControlItem.Pushed then
                begin
                  if (ControlItem.Column = LColumn) and (ControlItem.Row = LRow) then
                  begin
                    GridPanel.CellIndexToCell(I - Delta, LColumn, LRow);
                    if (LRow > 0) and (LRow + ControlItem.RowSpan > GridPanel.RowCollection.Count) then
                    begin
                      Inc(Delta, (LRow + ControlItem.RowSpan) - GridPanel.RowCollection.Count);
                      GridPanel.CellIndexToCell(I - Delta, LColumn, LRow);
                    end;
                    NewLocations.AddNewLocation(ControlItem, LColumn, LRow, False);
                  end;
                end else if ControlItem <> Self then
                  Break
                else
                  NewLocations.AddNewLocation(ControlItem, LColumn, LRow, False);
              end;
            end;
            NewLocations.ApplyNewLocations;
            GridPanel.RemoveEmptyAutoAddRows;
          end;
        end else
        begin
          NumRows := GridPanel.RowCollection.Count;
          Delta := Value - FRowSpan;
          // first check if there is room down to expand and if so remove those
          // rows from the Delta
          for I := Min(FRow + FRowSpan, NumRows) to Min(FRow + Value - 1, NumRows - 1) do
            if Collection.Controls[FColumn, I] = nil then
              Dec(Delta)
            else
              Break;
          MoveBy := Delta;
          // Now find out how many rows to add, if any
          for I := NumRows - 1 downto NumRows - MoveBy do
            if Collection.Controls[FColumn, I] = nil then
              Dec(Delta)
            else
              Break;
          NumToAdd := Delta;
          // Add the rows
          if GridPanel.ExpandStyle in [emAddRows, emFixedSize] then
          begin
            if (GridPanel.ExpandStyle = emFixedSize) and (NumToAdd > 0) then
              raise EGridPanelException.Create(sCannotAddFixedSize);
            while NumToAdd > 0 do
            begin
              GridPanel.AutoAddRow;
              Dec(NumToAdd);
            end;
            NumRows := GridPanel.RowCollection.Count;
            for I := NumRows - 1 downto NumRows - Delta do
            begin
              ControlItem := Collection.ControlItems[FColumn, I - MoveBy];
              if (ControlItem <> nil) and (ControlItem <> Self) then
                NewLocations.AddNewLocation(ControlItem, FColumn, I, True);
            end;
            NewLocations.ApplyNewLocations;
          end else if (NumToAdd + MoveBy) > 0 then
          begin
            IndexDelta := Max(NumToAdd, Min(MoveBy, NumRows));
            for I := GridPanel.RowSpanIndex[FColumn, FRow] to GridPanel.CellCount - 1 do
            begin
              GridPanel.CellIndexToCell(I, LColumn, LRow);
              ControlItem := Collection.ControlItems[LColumn, LRow];
              if (ControlItem <> nil) and (ControlItem.Column = LColumn) and (ControlItem.Row = LRow) then
              begin
                if ControlItem = Self then
                begin
                  Span := Value;
                  LColumn := FColumn;
                  LRow := FRow;
                end else
                begin
                  Span := ControlItem.RowSpan;
                  GridPanel.CellIndexToCell(I + IndexDelta, LColumn, LRow);
                end;
                if LRow + Span > GridPanel.RowCollection.Count then
                begin
                  if LRow > 0 then
                  begin
                    Inc(IndexDelta, GridPanel.RowCollection.Count - LRow);
                    GridPanel.CellIndexToCell(I + IndexDelta - NumToAdd, LColumn, LRow);
                  end else if ControlItem <> Self then
                  begin
                    Inc(IndexDelta, Min(Span, GridPanel.RowCollection.Count));
                    GridPanel.CellIndexToCell(I + IndexDelta, LColumn, LRow);
                  end else if (ControlItem = Self) and (LRow = 0) then
                    Exit;
                end;
                NumToAdd := 0;
                NewLocations.AddNewLocation(ControlItem, LColumn, LRow, True);
              end;
            end;
            for I := 0 to NewLocations.Count - 1 do
              if NewLocations.NewLocations[I].NewColumn > GridPanel.ColumnCollection.Count - 1 then
                GridPanel.AutoAddColumn;
            NewLocations.ApplyNewLocations;
          end;
          FRowSpan := Value;
        end;
        Changed(False);
      finally
        NewLocations.Free;
      end;
    finally
      GridPanel.EnableAlign;
    end;
  end;
end;

procedure TControlItem.SetColumnSpan(Value: TCellSpan);
var
  I, Delta: Integer;
  Collection: TControlCollection;
  ControlItem: TControlItem;
  NumToAdd, NumColumns, MoveBy, LColumn, LRow, IndexDelta: Integer;
  Span: TCellSpan;
  NewLocations: TNewLocations;
begin
  if FColumnSpan <> Value then
  begin
    if Value < 1 then
      raise EGridPanelException.CreateFmt(sInvalidSpan, [Value]);
    Collection := TControlCollection(GetOwner);
    if Collection = nil then Exit;
    GridPanel.DisableAlign;
    try
      NewLocations := TNewLocations.Create;
      try
        if FColumnSpan > Value then
        begin
          Delta := FColumnSpan - Value;
          FColumnSpan := Value;
          if GridPanel.ExpandStyle in [emAddColumns, emFixedSize] then
          begin
            NumColumns := GridPanel.ColumnCollection.Count;
            // Move the controls to the right back to fill in the space.
            for I := FColumn + FColumnSpan + Delta to NumColumns - 1 do
            begin
              ControlItem := Collection.ControlItems[I, FRow];
              if ControlItem <> nil then
                if ControlItem.Pushed then
                  NewLocations.AddNewLocation(ControlItem, I - Delta, FRow, False)
                else
                  Break;
            end;
            NewLocations.ApplyNewLocations;
            GridPanel.RemoveEmptyAutoAddColumns;
          end else
          begin
            // Scan forward in column primary order, removing Delta from position of each
            // control item, unwrapping where nessesary, until the last control is reached
            // or a non "pushed" control is found.
            for I := GridPanel.ColumnSpanIndex[FColumn, FRow] to GridPanel.CellCount - 1 do
            begin
              GridPanel.CellIndexToCell(I, LColumn, LRow);
              ControlItem := Collection.ControlItems[LColumn, LRow];
              if ControlItem <> nil then
              begin
                if ControlItem.Pushed then
                begin
                  if (ControlItem.Column = LColumn) and (ControlItem.Row = LRow) then
                  begin
                    GridPanel.CellIndexToCell(I - Delta, LColumn, LRow);
                    if (LColumn > 0) and (LColumn + ControlItem.ColumnSpan > GridPanel.ColumnCollection.Count) then
                    begin
                      Inc(Delta, (LColumn + ControlItem.ColumnSpan) - GridPanel.ColumnCollection.Count);
                      GridPanel.CellIndexToCell(I - Delta, LColumn, LRow);
                    end;
                    NewLocations.AddNewLocation(ControlItem, LColumn, LRow, False);
                  end;
                end else if ControlItem <> Self then
                  Break
                else
                  NewLocations.AddNewLocation(ControlItem, LColumn, LRow, False);
              end;
            end;
            NewLocations.ApplyNewLocations;
            GridPanel.RemoveEmptyAutoAddRows;
          end;
        end else
        begin
          NumColumns := GridPanel.ColumnCollection.Count;
          Delta := Value - FColumnSpan;
          // first check if there is room to the right to expand and if so remove those
          // columns from the Delta
          for I := Min(FColumn + FColumnSpan, NumColumns) to Min(FColumn + Value - 1, NumColumns - 1) do
            if Collection.Controls[I, FRow] = nil then
              Dec(Delta)
            else
              Break;
          MoveBy := Delta;
          // Now find out how many columns to add, if any
          for I := NumColumns - 1 downto NumColumns - MoveBy do
            if Collection.Controls[I, FRow] = nil then
              Dec(Delta)
            else
              Break;
          NumToAdd := Delta;
          // Add the columns
          if GridPanel.ExpandStyle in [emAddColumns, emFixedSize] then
          begin
            if (GridPanel.ExpandStyle = emFixedSize) and (NumToAdd > 0) then
              raise EGridPanelException.Create(sCannotAddFixedSize);
            while NumToAdd > 0 do
            begin
              GridPanel.AutoAddColumn;
              Dec(NumToAdd);
            end;
            NumColumns := GridPanel.ColumnCollection.Count;
            for I := NumColumns - 1 downto NumColumns - Delta do
            begin
              ControlItem := Collection.ControlItems[I - MoveBy, FRow];
              if (ControlItem <> nil) and (ControlItem <> Self) then
                NewLocations.AddNewLocation(ControlItem, I, FRow, True);
            end;
            NewLocations.ApplyNewLocations;
          end else if (NumToAdd + MoveBy) > 0 then
          begin
            IndexDelta := Max(NumToAdd, Min(MoveBy, NumColumns));
            for I := GridPanel.ColumnSpanIndex[FColumn, FRow] to GridPanel.CellCount - 1 do
            begin
              GridPanel.CellIndexToCell(I, LColumn, LRow);
              ControlItem := Collection.ControlItems[LColumn, LRow];
              if (ControlItem <> nil) and (ControlItem.Column = LColumn) and (ControlItem.Row = LRow) then
              begin
                if ControlItem = Self then
                begin
                  Span := Value;
                  LColumn := FColumn;
                  LRow := FRow;
                end else
                begin
                  Span := ControlItem.ColumnSpan;
                  GridPanel.CellIndexToCell(I + IndexDelta, LColumn, LRow);
                end;
                if LColumn + Span > GridPanel.ColumnCollection.Count then
                begin
                  if LColumn > 0 then
                  begin
                    Inc(IndexDelta, GridPanel.ColumnCollection.Count - LColumn);
                    GridPanel.CellIndexToCell(I + IndexDelta - NumToAdd, LColumn, LRow);
                  end else if ControlItem <> Self then
                  begin
                    Inc(IndexDelta, Min(Span, GridPanel.ColumnCollection.Count));
                    GridPanel.CellIndexToCell(I + IndexDelta, LColumn, LRow);
                  end else if (ControlItem = Self) and (LColumn = 0) then
                    Exit;
                end;
                NumToAdd := 0;
                NewLocations.AddNewLocation(ControlItem, LColumn, LRow, True);
              end;
            end;
            for I := 0 to NewLocations.Count - 1 do
              if NewLocations.NewLocations[I].NewRow > GridPanel.RowCollection.Count - 1 then
                GridPanel.AutoAddRow;
            NewLocations.ApplyNewLocations;
          end;
          FColumnSpan := Value;
        end;
        Changed(False);
      finally
        NewLocations.Free;
      end;
    finally
      GridPanel.EnableAlign;
    end;
  end;
end;

procedure TControlItem.InternalSetLocation(AColumn, ARow: Integer; APushed, MoveExisting: Boolean);
var
  Collection: TControlCollection;
  CurrentItem: TControlItem;
begin
  if (AColumn <> FColumn) or (ARow <> FRow) then
  begin
    if MoveExisting then
    begin
      Collection := TControlCollection(GetOwner);
      if Collection <> nil then
        CurrentItem := Collection.ControlItems[AColumn, ARow]
      else
        CurrentItem := nil;
      if CurrentItem <> nil then
        CurrentItem.InternalSetLocation(FColumn, FRow, False, False);
    end;
    FColumn := AColumn;
    FRow := ARow;
    if APushed then
      Inc(FPushed)
    else if FPushed > 0 then
      Dec(FPushed);
    Changed(False);
  end;
end;

procedure TControlItem.SetLocation(AColumn, ARow: Integer; APushed: Boolean);
begin
  InternalSetLocation(AColumn, ARow, APushed, True);
end;

function TControlItem.GetPushed: Boolean;
begin
  Result := FPushed > 0;
end;

//========================== TCustomGridPanel ==================================

constructor TCustomGridPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FRowCollection := TRowCollection.Create(Self);
  FColumnCollection := TColumnCollection.Create(Self);
  FControlCollection := TControlCollection.Create(Self);
  FRecalcCellSizes := True;
  FRowCollection.Add;
  FRowCollection.Add;
  FColumnCollection.Add;
  FColumnCollection.Add;
end;

destructor TCustomGridPanel.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FRowCollection);
  FreeAndNil(FColumnCollection);
  FreeAndNil(FControlCollection);
end;

procedure TCustomGridPanel.AlignControls(AControl: TControl; var Rect: TRect);

  procedure ArrangeControlInCell(AControl: TControl; CellRect: TRect; const AlignInfo: TAlignInfo);
  var
    NewBounds: TRect;
    AnchorSubset: TAnchors;
  begin
  {  if AControl.Align <> alNone then
        ArrangeControl(AControl, Point(CellRect.Right - CellRect.Left, CellRect.Bottom - CellRect.Top),
          AControl.Align, AlignInfo, CellRect, True)
    else
    begin }
      AnchorSubset := AControl.Anchors * [akLeft, akRight];
      if AnchorSubset = [akLeft] then
        NewBounds.Left := CellRect.Left
      else if AnchorSubset = [akRight] then
        NewBounds.Left := Max(CellRect.Left, CellRect.Right - AControl.Width)
      else
        NewBounds.Left := Max(CellRect.Left, CellRect.Left + ((CellRect.Right - CellRect.Left) - AControl.Width) div 2);
      NewBounds.Right := NewBounds.Left + Min(CellRect.Right - CellRect.Left, AControl.Width);
      AnchorSubset := AControl.Anchors * [akTop, akBottom];
      if AnchorSubset = [akTop] then
        NewBounds.Top := CellRect.Top
      else if AnchorSubset = [akBottom] then
        NewBounds.Top := Max(CellRect.Top, CellRect.Bottom - AControl.Height)
      else
        NewBounds.Top := Max(CellRect.Top, CellRect.Top + ((CellRect.Bottom - CellRect.Top) - AControl.Height) div 2);
      NewBounds.Bottom := NewBounds.Top + Min(CellRect.Bottom - CellRect.Top, AControl.Height);
      AControl.SetBounds(NewBounds.Left,NewBounds.Top,NewBounds.Right-NewBounds.Left,NewBounds.Bottom-NewBounds.Top );
  //  end;
  end;

  procedure AdjustCellRect(var Rect: TRect);
  begin
   { Inc(Rect.Left, Padding.Left);
    Inc(Rect.Top, Padding.Top);
    Dec(Rect.Right, Padding.Right);
    Dec(Rect.Bottom, Padding.Bottom);  }
  end;

  procedure ArrangeControls;
  var
    I, J, K: Integer;
    CellRect: TRect;
    SpanRect: TRect;
    ControlItem: TControlItem;
    AlignInfo: TAlignInfo;
  begin
    AlignInfo.ControlIndex := 0;
    AlignInfo.AlignList := TFPList.Create;
    try
      CellRect.Top := Rect.Top;
      for J := 0 to FRowCollection.Count - 1 do
      begin
        CellRect.Left := Rect.Left;
        CellRect.Bottom := CellRect.Top + FRowCollection[J].Size;
        for I := 0 to FColumnCollection.Count - 1 do
        begin
          ControlItem := FControlCollection.ControlItems[I, J];
          CellRect.Right := CellRect.Left + FColumnCollection[I].Size;
          if (ControlItem <> nil) and (ControlItem.Control <> nil) and
             (ControlItem.Column = I) and (ControlItem.Row = J) then
          begin
            AlignInfo.AlignList.Clear;
            AlignInfo.AlignList.Add(ControlItem.Control);
            AlignInfo.Align := ControlItem.Control.Align;
            SpanRect := CellRect;
            if ControlItem.ColumnSpan > 1 then
              for K := I + 1 to Min(I + ControlItem.ColumnSpan - 1, FColumnCollection.Count - 1) do
                Inc(SpanRect.Right, FColumnCollection[K].Size);
            if ControlItem.RowSpan > 1 then
              for K := J + 1 to Min(J + ControlItem.RowSpan - 1, FRowCollection.Count - 1 ) do
                Inc(SpanRect.Bottom, FRowCollection[K].Size);
            AdjustCellRect(SpanRect);
            ArrangeControlInCell(ControlItem.Control, SpanRect, AlignInfo);
          end;
          CellRect.Left := CellRect.Right;
        end;
        CellRect.Top := CellRect.Bottom;
      end;
    finally
      AlignInfo.AlignList.Free;
    end;
  end;

begin
  AdjustClientRect(Rect);
  if FRecalcCellSizes then
    RecalcCellDimensions(Rect);
  if ControlCount > 0 then
    ArrangeControls;
end;

function TCustomGridPanel.AutoAddColumn: TColumnItem;
begin
  Result := FColumnCollection.Add;
  Result.SizeStyle := ssAuto;
  Result.AutoAdded := True;
end;

function TCustomGridPanel.AutoAddRow: TRowItem;
begin
  Result := FRowCollection.Add;
  Result.SizeStyle := ssAuto;
  Result.AutoAdded := True;
end;

procedure TCustomGridPanel.CellIndexToCell(AIndex: Integer; var AColumn, ARow: Integer);
begin
  if FExpandStyle in [emAddColumns, emFixedSize] then
  begin
    AColumn := AIndex div FRowCollection.Count;
    ARow := AIndex mod FRowCollection.Count;
  end else
  begin
    ARow := AIndex div FColumnCollection.Count;
    AColumn := AIndex mod FColumnCollection.Count;
  end;
end;

function TCustomGridPanel.CellToCellIndex(AColumn, ARow: Integer): Integer;
begin
  if FExpandStyle in [emAddColumns, emFixedSize] then
    Result := ColumnSpanIndex[AColumn, ARow]
  else
    Result := RowSpanIndex[AColumn, ARow];
end;

{$IF DEFINED(CLR)}
procedure TCustomGridPanel.ControlChange(Inserting: Boolean; Child: TControl);
begin
  inherited ControlChange(Inserting, Child);
  if not (csLoading in ComponentState) then
    if Inserting and (Child.Parent = Self) then
    begin
      DisableAlign;
      try
        Child.Anchors := [];
        FControlCollection.AddControl(Child);
      finally
        EnableAlign;
      end;
    end else
      FControlCollection.RemoveControl(Child);
end;
{$ENDIF}

{$IF NOT DEFINED(CLR)}
procedure TCustomGridPanel.CMControlChange(var Message: TCMControlChange);
begin
  inherited;
  if not (csLoading in ComponentState) then
    if Message.Inserting and (Message.Control.Parent = Self) then
    begin
      DisableAlign;
      try
        Message.Control.Anchors := [];
        FControlCollection.AddControl(Message.Control);
      finally
        EnableAlign;
      end;
    end else
      FControlCollection.RemoveControl(Message.Control);
end;
{$ENDIF}

function TCustomGridPanel.GetCellCount: Integer;
begin
  Result := FRowCollection.Count * FColumnCollection.Count;
end;

function TCustomGridPanel.GetCellRect(AColumn, ARow: Integer): TRect;
var
  I: Integer;
begin
  Result.Left := 0;
  Result.Top := 0;
  for I := 0 to AColumn - 1 do
    Inc(Result.Left, FColumnCollection[I].Size);
  for I := 0 to ARow - 1 do
    Inc(Result.Top, FRowCollection[I].Size);
  Result.BottomRight := CellSize[AColumn, ARow];
  Inc(Result.Bottom, Result.Top);
  Inc(Result.Right, Result.Left);
end;

function TCustomGridPanel.GetCellSizes(AColumn, ARow: Integer): TPoint;
begin
  Result := Point(FColumnCollection[AColumn].Size, FRowCollection[ARow].Size);
end;

procedure TCustomGridPanel.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
  ControlItem: TControlItem;
begin
  for I := 0 to FControlCollection.Count - 1 do
  begin
    ControlItem := TControlItem(FControlCollection.Items[I]);
    if (ControlItem.Control <> nil) and (ControlItem.Control.Owner = Root) then
      Proc(ControlItem.Control);
  end;
end;

function TCustomGridPanel.GetColumnSpanIndex(AColumn, ARow: Integer): Integer;
begin
  Result := AColumn + (ARow * FColumnCollection.Count);
end;

function TCustomGridPanel.GetRowSpanIndex(AColumn, ARow: Integer): Integer;
begin
  Result := ARow + (AColumn * FRowCollection.Count);
end;

function TCustomGridPanel.IsColumnEmpty(AColumn: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  if (AColumn > -1) and (AColumn < FColumnCollection.Count) then
  begin
    for I := 0 to FRowCollection.Count -1 do
      if ControlCollection.Controls[AColumn, I] <> nil then
        Exit;
    Result := True;
  end else
    raise EGridPanelException.CreateFmt(sInvalidColumnIndex, [AColumn]);
end;

function TCustomGridPanel.IsRowEmpty(ARow: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  if (ARow > -1) and (ARow < FRowCollection.Count) then
  begin
    for I := 0 to FColumnCollection.Count -1 do
      if ControlCollection.Controls[I, ARow] <> nil then
        Exit;
    Result := True;
  end else
    raise EGridPanelException.CreateFmt(sInvalidRowIndex, [ARow]);
end;

procedure TCustomGridPanel.Loaded;
var
  LItem: TCollectionItem;
begin
  inherited;
  for LItem in ControlCollection do
    with TControlItem(LItem) do
      TControlItem(LItem).InternalSetLocation(Column, Row, False, True);
end;

procedure TCustomGridPanel.Paint;
var
  I: Integer;
  LinePos, Size: Integer;
  ClientRect: TRect;
begin
  inherited;
  if csDesigning in ComponentState then
  begin
    LinePos := 0;
    Canvas.Pen.Style := psDot;
    Canvas.Pen.Color := clBlack;
    ClientRect := GetClientRect;
    for I := 0 to FColumnCollection.Count - 2 do
    begin
      Size := FColumnCollection[I].Size;
      Canvas.MoveTo(LinePos + Size, ClientRect.Top);
      Canvas.LineTo(LinePos + Size, ClientRect.Bottom);
      Inc(LinePos, Size);
    end;
    LinePos := 0;
    for I := 0 to FRowCollection.Count - 2 do
    begin
      Size := FRowCollection[I].Size;
      Canvas.MoveTo(ClientRect.Left, LinePos + Size);
      Canvas.LineTo(ClientRect.Right, LinePos + Size);
      Inc(LinePos, Size);
    end;
  end;
end;

procedure TCustomGridPanel.RecalcCellDimensions(const Rect: TRect);
var
  I, J: Integer;
  LSize, XSize, YSize, RemainingX, RemainingY: Integer;
  MaxSize, PercentXCount, PercentYCount: Integer;
  PercentX, PercentY: Double;
  ControlItem: TControlItem;
begin
  // Subtract the size of absolute size columns and rows and also calculate the total size of
  // the percentage rows and columns
  // Also subtract the width of the widest control in each autosize column
  // or the tallest control in each autosize row and set the column or row size to that value
  XSize := Rect.Right - Rect.Left;
  YSize := Rect.Bottom - Rect.Top;
  PercentX := 0.0;
  PercentY := 0.0;
  PercentXCount := 0;
  for I := 0 to FColumnCollection.Count - 1 do
    with FColumnCollection[I] do
      if SizeStyle = ssAbsolute then
        Dec(XSize, Trunc(Value))
      else if SizeStyle = ssPercent then
      begin
        PercentX := PercentX + Value;
        Inc(PercentXCount);
      end
      else
      begin
        MaxSize := 0;
        for J := 0 to FRowCollection.Count - 1 do
        begin
          ControlItem := FControlCollection.ControlItems[I, J];
          if (ControlItem <> nil) and (ControlItem.Control <> nil) and
             (ControlItem.Column = I) and (ControlItem.Row = J) then
          begin
            LSize := ControlItem.Control.Width + Self.Left + Self.Width;
            if LSize > MaxSize then
              MaxSize := LSize;
          end;
        end;
        Dec(XSize, MaxSize);
        Size := MaxSize;
      end;
  PercentYCount := 0;
  for I := 0 to FRowCollection.Count - 1 do
    with FRowCollection[I] do
      if SizeStyle = ssAbsolute then
        Dec(YSize, Trunc(Value))
      else if SizeStyle = ssPercent then
      begin
        PercentY := PercentY + Value;
        Inc(PercentYCount);
      end else
      begin
        MaxSize := 0;
        for J := 0 to FColumnCollection.Count - 1 do
        begin
          ControlItem := FControlCollection.ControlItems[J, I];
          if (ControlItem <> nil) and (ControlItem.Control <> nil) and
             (ControlItem.Column = J) and (ControlItem.Row = I) then
          begin
            LSize := ControlItem.Control.Height + Self.Top + Self.Height;
            if LSize > MaxSize then
              MaxSize := LSize;
          end;
        end;
        Dec(YSize, MaxSize);
        Size := MaxSize;
      end;
  // Finally Calculate the size of the percentage-based columns and rows based on the remaining
  // X and Y sizes
  RemainingX := XSize;
  RemainingY := YSize;
  for I := 0 to FColumnCollection.Count - 1 do
    with FColumnCollection[I] do
      if SizeStyle = ssPercent then
      begin
        if IsZero(PercentX) then
          Value := 100.0 / PercentXCount
        else
          Value := (Value / PercentX) * 100.0;
        Size := Trunc(XSize * (Value / 100.0));
        Dec(RemainingX, Size);
        if (RemainingX > 0) and (I = FColumnCollection.Count - 1) then
          Size := Size + RemainingX;
      end;
  for I := 0 to FRowCollection.Count - 1 do
    with FRowCollection[I] do
      if SizeStyle = ssPercent then
      begin
        if IsZero(PercentY) then
          Value := 100.0 / PercentYCount
        else
          Value := (Value / PercentY) * 100.0;
        Size := Trunc(YSize * (Value / 100.0));
        Dec(RemainingY, Size);
        if (RemainingY > 0) and (I = FRowCollection.Count - 1) then
          Size := Size + RemainingY;
      end;
  FRecalcCellSizes := False;
end;

procedure TCustomGridPanel.RemoveEmptyAutoAddColumns;
var
  I: Integer;
begin
  for I := FColumnCollection.Count - 1 downto 0 do
    if FColumnCollection[I].AutoAdded and IsColumnEmpty(I) then
      FColumnCollection.Delete(I)
    else
      Exit;
end;

procedure TCustomGridPanel.RemoveEmptyAutoAddRows;
var
  I: Integer;
begin
  for I := FRowCollection.Count - 1 downto 0 do
    if FRowCollection[I].AutoAdded and IsRowEmpty(I) then
      FRowCollection.Delete(I)
    else
      Exit;
end;

procedure TCustomGridPanel.SetControlCollection(const Value: TControlCollection);
begin
  FControlCollection.Assign(Value);
end;

procedure TCustomGridPanel.SetRowCollection(const Value: TRowCollection);
begin
  FRowCollection.Assign(Value);
end;

procedure TCustomGridPanel.SetColumnCollection(const Value: TColumnCollection);
begin
  FColumnCollection.Assign(Value);
end;

procedure TCustomGridPanel.UpdateControlsColumn(AColumn: Integer);
var
  I, J: Integer;
  AControlItem: TControlItem;
begin
  for I := AColumn + 1 to FColumnCollection.Count - 1 do
    for J := 0 to FRowCollection.Count - 1 do
    begin
      AControlItem := FControlCollection.ControlItems[I, J];
      if (AControlItem <> nil) and (AControlItem.Column = I) and
         (AControlItem.Row = J) then
        AControlItem.SetColumn(AControlItem.Column - 1);
    end;
end;

procedure TCustomGridPanel.UpdateControlsRow(ARow: Integer);
var
  I, J: Integer;
  AControlItem: TControlItem;
begin
  for I := 0 to FColumnCollection.Count - 1 do
    for J := ARow + 1 to FRowCollection.Count - 1 do
    begin
      AControlItem := FControlCollection.ControlItems[I, J];
      if (AControlItem <> nil) and (AControlItem.Column = I) and
         (AControlItem.Row = J) then
        AControlItem.SetRow(AControlItem.Row - 1);
    end;
end;

procedure TCustomGridPanel.UpdateControlOriginalParentSize(AControl: TControl; var AOriginalParentSize: TPoint);
var
  Rect: TRect;
  Index: Integer;
  ControlItem: TControlItem;
begin
  if FRecalcCellSizes and HandleAllocated then
  begin
    Rect := GetClientRect;
    AdjustClientRect(Rect);
    RecalcCellDimensions(Rect);
  end;
  Index := FControlCollection.IndexOf(AControl);
  if Index > -1 then
  begin
    ControlItem := FControlCollection[Index];
    AOriginalParentSize := CellSize[ControlItem.Column, ControlItem.Row]
  end {else
    inherited UpdateControlOriginalParentSize(AControl, AOriginalParentSize)};
end;

procedure TCustomGridPanel.WMWindowPosChanged(var Message: TLMWindowPosChanged); 
begin
  inherited;
  FRecalcCellSizes := True;
end;

End.
