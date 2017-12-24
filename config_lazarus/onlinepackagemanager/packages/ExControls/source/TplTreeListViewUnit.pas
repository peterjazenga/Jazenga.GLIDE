
{**********************************************************************
 Package pl_ExControls.pkg
 From PilotLogic Software House(http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit TplTreeListViewUnit;

{$MODE DELPHI}

  {$define allowHeaderDragging}
  {$define allowHeaderVisible}
  {$define openOwnPopupMenu}
  {$define useRealClipping}


interface

uses
  LCLType,LCLIntf, LMessages,
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,comctrls,stdctrls,ExtCtrls, Menus,math,
  {$ifdef lclqt} qtwidgets,  {$endif}
  {$ifdef lclqt5}qt5widgets, {$endif}
  {$ifdef clr}types,{$endif}
  TplSearchPanelUnit;

type
  {$TYPEINFO ON}

  TObjectList=class;
  TplTreeListRecordItem=class;
  TplTreeListItem=class;
  TplTreeListView = class;

  TListEventTyp=(levBeginEdit,levEndEdit,levAdd,levInsert,levClear,levDelete,levExchange,levMove,levSort,levAssign);
  TListEvent = procedure (list: TObjectList; typ: TListEventTyp) of object;

  TObjectList= class(TList)
    public
      constructor Create(listEvent: TListEvent);
      destructor Destroy;override;
      function  AddObject(Item: TObject): Integer;
      function  RemoveObject(Item: TObject): Integer;
      procedure BeginEdit;
      procedure EndEdit;
      procedure InsertObject(Index: Integer; Item: TObject);
      procedure Clear; override;
      procedure Delete(Index: Integer);
      procedure Exchange(Index1, Index2: Integer);
      procedure Move(CurIndex, NewIndex: Integer);
      procedure Sort(Compare: TListSortCompare);
      procedure Assign(list:TList);
    protected
      onListEvent: TListEvent;
      procedure FreeObjects;
      procedure Insert(Index: Integer; Item: TObject);
      function  Add(Item: TObject): Integer;
      function  Remove(Item: TObject): Integer;
  end;


  TRealItemCounting=set of (ricCountCollapsedsubItems{,ricCountExpandItems,ricCountEndNodes});


  pbool=pboolean;
  TplTreeListItemCompare = function (i1, i2: TplTreeListItem): longint of object;

  TplTreeListItems=class(TObjectList)
  private
    procedure Put(Index: Integer; const AValue: TplTreeListItem);
  protected
    F_Parent:TplTreeListItem;
    F_TreeListView:TplTreeListView;
    function Get(Index: Integer): TplTreeListItem;
    procedure Sort(CompareFunc: TplTreeListItemCompare);
  public
    constructor create(parent:TplTreeListItem;const TreeListView:TplTreeListView);
    function Add(caption:string=''):TplTreeListItem;overload;
    function Add(Parent:TplTreeListItem;caption:string=''):TplTreeListItem;overload;
    function GetRealItemCount(const countTyp:TRealItemCounting ) :integer;
    function RealIndexOf(const item:TplTreeListItem;const countTyp:TRealItemCounting):integer;
    function GetItemWithRealIndex(index:integer;const countTyp:TRealItemCounting):TplTreeListItem;
    function FindItemWithText(caption: string): TplTreeListItem;
    function FindItemWithRecordText(text: string;pos: longint=0):TplTreeListItem;
    function find(searchFor: string; searchFields:cardinal;
                                     backward: boolean=false;
                                     loopAround: PBOOL=nil;
                                     startItem: TplTreeListItem=nil;
                                     startColumn: longint=0;
                                     startTextPosition:longint=0): TplTreeListRecordItem; overload;

    property Items[Index: Integer]: TplTreeListItem read Get write Put; default;
  end;

  TRecordItemList=class(TObjectList)
  private
    Owner: TplTreeListItem;
    function Get(Index: Integer): TplTreeListRecordItem;
    procedure Put(Index: Integer; const AValue: TplTreeListRecordItem);
  public
    function Add:TplTreeListRecordItem;overload;
    function Add(s: string):TplTreeListRecordItem;overload;
    procedure AddItem(recordItem: TplTreeListRecordItem);
    property Items[Index: Integer]: TplTreeListRecordItem read Get write Put; default;
  end;

  TplTreeListRecordItem=class(TPersistent)
    protected
      F_Parent: TplTreeListItem;
      F_Text:string;
      procedure SetText(caption:string);
      function getIndex(): longint;
    public
      constructor Create(aparent:TplTreeListItem); overload;
      constructor Create(aparent:TplTreeListItem;caption:string); overload;
      destructor Destroy;override;
      procedure selectFont(can: TCanvas);
      function GetNecessaryWidth(listView:TplTreeListView=nil): longint;
    published
      property Text:string read F_Text write setText;
      property Index: longint read getIndex;
      property Parent:TplTreeListItem read F_Parent;
  end;

  TItemHierarchyStack=record
    size:longint;
    stack:array of record
      list: TplTreeListItems;
      index: longint;
    end;
  end;


  TItemDataRec = packed record
     case integer of
       0 : (i64: int64);
       1 : (lo32, hi32 : Cardinal);
       2 : (Words : Array[0..3] of Word);
       3 : (Bytes : Array[0..7] of Byte);
       4 : (p : pointer);
       5 : (obj : TObject);
  end;

  TplTreeListItem=class(TPersistent)
    private
      function  GetText: string;
      procedure SetText(const AValue: string);
    protected
      F_ImageIndex:longint;
      F_ImageBitmap:graphics.TBitmap;
      F_SubItems:TplTreeListItems;
      F_RecordItems:TRecordItemList;
      F_Expanded,F_Selected,F_MouseSelected:boolean;
      F_Indent:integer;
      F_Parent:TplTreeListItem;
      F_TreeListView:TplTreeListView;
      function  GetExtraTextIndentation(column: longint): longint;
      function  GetExtendingButtonPos: longint;
      procedure SetSelected(newSelected: boolean);
      procedure SetMouseSelected(newSelected: boolean);
      procedure SetSelections(realSelected, mouseSelection:boolean);
      procedure SetSubItems(const value:TplTreeListItems);
      procedure SetRecordItems(const value:TRecordItemList);
      procedure SetExpand(const expanded:boolean);
      function  GetRecordItemsText(i: Integer): string;
      procedure SetRecordItemsText(i: Integer; const AValue: string);
      procedure SheduleInternRepaint();
    public
      data:TItemDataRec;
      constructor Create(const parent:TplTreeListItem;const TreeListView:TplTreeListView;const ACaption:string=''); overload;
      destructor Destroy;override;

      function  getBounds(column: longint):TRect;
      function  getMaxTextBounds(column: longint):TRect;
      function  GetItemAtPos(const listView:TplTreeListView;const TestY:integer;var startY:integer):TplTreeListItem;
      function  GetRecordItemAtPos(const listView:TplTreeListView;TestX:integer):TplTreeListRecordItem;
      function  GetMaxColumnWidth(const id:longint): longint;
      function  GetNextItemIgnoringChildren:TplTreeListItem;
      function  GetLastVisibleSubSubItem:TplTreeListItem;
      function  GetLastSubSubItem:TplTreeListItem;
      function  GetNextVisibleItem(Delta:longint=1):TplTreeListItem;
      function  GetPrevVisibleItem(Delta:longint=1):TplTreeListItem;
      function  GetNextItem():TplTreeListItem;
      function  GetPrevItem():TplTreeListItem;
      function  GetParentInList(List: TplTreeListItems=nil):TplTreeListItem;
      function  GetNextFromHierarchyStack(var stack: TItemHierarchyStack; const mustBeVisible: boolean=false): TplTreeListItem;
      function  ParentItems: TplTreeListItems;
      procedure GetParentHierarchyStack(out stack:TItemHierarchyStack);
      procedure Paint(const hierarchyStack: TItemHierarchyStack);
      procedure Expand;
      procedure Collapse;

      property Parent:TplTreeListItem read F_parent;
      property TreeListView:TplTreeListView read F_TreeListview;
      property Indent:integer read F_Indent;
      function SeemsSelected:boolean;
      property Expanded:boolean read F_expanded write SetExpand;
      property MouseSelected: boolean read F_MouseSelected write SetMouseSelected;
      property RecordItemsText[i: Integer]:string read GetRecordItemsText write SetRecordItemsText;
    published
      property RecordItems:TRecordItemList read F_RecordItems write SetRecordItems;
      property SubItems:TplTreeListItems read F_SubItems write SetSubItems;
      property ImageIndex:longint read F_ImageIndex write F_ImageIndex;
      property ImageBitmap:graphics.TBitmap read F_ImageBitmap  write F_ImageBitmap;
      property Text:string read GetText write SetText;
      property Selected: boolean read F_Selected write SetSelected;
  end;

  // Options
  TplTreeListViewOption = ( tlvoMultiSelect,              // Specifies if multiple items can be selected
                            tlvoToolTips,                 // Specifies if tooltips are shown when item text is longer than the column
                            tlvoRightMouseSelects,        // Determines if you can select items using the right mouse button
                            tlvoHotTrackRecordTextItems,  // Determines if the record items are hot tracked
                            tlvoStriped,                  // Determines if the item background is drawn alternating
                            tlvoStripInvisibleItems,      // Controls if invisible items are counted when the control determines if a item is odd or even
                            tlvoColumnsDragable,          // Controls if the columns of the header control can be reordered
                            tlvoSorted,                   // Controls of the items should be @noAutoLink sorted @br Notice that items are only sorted in endUpdate
                            tlvoAlwaysFullRepaint,        // Repaints everything even after a small change
                            tlvoDragScrolling             // The list can be scrolled by dragging an item up and down (like standard Android lists)
                          );
  TplTreeListViewOptions=set of TplTreeListViewOption;

  TTreeListInternOptions=set of (tlioDeleting, tlioUpdating);
  TExpandMode=(emExpandByClick,emExpandByDoubleClick,emExpandNot);
  TLineMode=(lmNone,lmSolid,lmDot);
  TExpandItemEvent = procedure (Sender: TObject; Item: TplTreeListItem);
  TCustomDrawEventTyp=(cdetPrePaint,cdetPostPaint);
  TCustomBackgroundDrawEvent=procedure (sender:TObject;eventTyp_cdet:TCustomDrawEventTyp;var defaultDraw:Boolean) of object;
  TCustomItemDrawEvent=procedure (sender:TObject;eventTyp_cdet:TCustomDrawEventTyp;item:TplTreeListItem;var defaultDraw:Boolean) of object;
  TCustomRecordItemDrawEvent=procedure (sender:TObject;eventTyp_cdet:TCustomDrawEventTyp;recordItem:TplTreeListRecordItem;var defaultDraw:Boolean) of object;
  TCustomRecordItemPositioningEvent = procedure (sender:TObject; visualColumnIndex: integer; recordItem:TplTreeListRecordItem; var aposition: TRect) of object;
  TItemEvent=procedure (sender:TObject;item:TplTreeListItem) of object;
  TRecordItemEvent=procedure (sender:TObject;recorditem:TplTreeListRecordItem) of object;
  TCompareTreeListItemsEvent=procedure (sender: TObject; item1, item2: TplTreeListItem; var result: longint)of object;
  TUserSortItemsEvent=procedure (sender: TObject; var sortColumn: longint; var invertSorting: boolean) of object;


  TEventHeaderControl=TCustomHeaderControl;

  TplTreeListView = class(TCustomControl)
  private
    F_HeaderVisible: boolean;
    F_ScrollStyle: TScrollStyle;
    function GetTopItemVisualIndex: integer;
    procedure SetBgColor(const AValue: TColor);
    procedure SetButtonColor(const AValue: TColor);
    procedure SetColorSearchMark(const AValue: tcolor);
    procedure SetColorSearchMarkField(const AValue: tcolor);
    procedure SetExpandMode(const AValue: TExpandMode);
    procedure SetHeaderVisible(AValue: boolean);
    procedure SetHorizontalLineColor(const AValue: TColor);
    procedure SetHorizontalLines(const AValue: TLineMode);
    procedure SetRootLineColor(const AValue: TColor);
    procedure SetRootLines(const AValue: TLineMode);
    procedure SetScrollStyle(AValue: TScrollStyle);
    procedure SetSelectBackColor(const AValue: TColor);
    procedure SetStripedEvenColor(const AValue: TColor);
    procedure SetStripedOddColor(const AValue: TColor);
    procedure SetVerticalLineColor(const AValue: TColor);
    procedure SetVerticalLines(const AValue: TLineMode);
  protected
    InternOptions_tlio:TTreeListInternOptions;
    doubleBuffer:graphics.TBitmap;
    F_LastPaintedWidth, F_LastPaintedHeight: integer;
    f_RedrawBlock: longint;
    f_invalidatedItems: TList;
    f_invalidateAll: boolean;
    f_bufferComplete: boolean;
    TreeColumnIndentation:integer;
    F_TopItem: TplTreeListItem;
    F_TopItemEven: boolean;
    F_SortColumn: longint;
    F_SortColumnInverted: boolean;
    F_OnCompareItems: TCompareTreeListItemsEvent;
    F_OnUserSortItems: TUserSortItemsEvent;
    F_Items:TplTreeListItems;
    F_Header:THeaderControl;
    F_HeaderColumnPopupMenu: TPopupMenu;
    F_VScroll:TScrollBar;
    F_HScroll:TScrollBar;
    F_RowHeight:integer;
    F_ImageList:TImageList;
    F_ExpandMode:TExpandMode;
    F_SelCount: longint;
    F_Focused: TplTreeListItem;
    F_BaseSelect: TplTreeListItem;
    F_Options: TplTreeListViewOptions;
    F_TreeSectionPos: TRect;
    F_HotTrackFont:TFont;
    F_SelectedFont:TFont;
    F_SelectedHotTrackFont:TFont;
    F_SelectBackColor:TColor;
    F_ButtonColor:TColor;
    F_BgColor:TColor;
    F_Striped:boolean;
    F_StripedOddColor:TColor;
    F_StripedEvenColor:TColor;
    F_StripInvisibleItems: boolean;
    F_HorizontalLines:TLineMode;
    F_HorizontalLineColor:TColor;
    F_VerticalLines:TLineMode;
    F_VerticalLineColor:TColor;
    F_RootLines:TLineMode;
    F_RootLineColor:TColor;
    F_HeaderSectionResize:TCustomSectionNotifyEvent;
    F_HeaderSectionTrack:TCustomSectionTrackEvent;
    F_VScrollBarChange:TNotifyEvent;
    F_HScrollBarChange:TNotifyEvent;
    F_MouseWheelDelta: longint;
    F_CustomBgDraw:TCustomBackgroundDrawEvent;
    F_CustomItemDraw:TCustomItemDrawEvent;
    F_CustomRecordItemDraw:TCustomRecordItemDrawEvent;
    F_CustomRecordItemPositioningEvent: TCustomRecordItemPositioningEvent;
    F_DrawingEvenItem: boolean;
    F_DrawingYPos: longint;
    F_DrawingRecordItemRect: TRect;
    F_SheduledRepaint: DWord;
    F_SheduledHScroll: DWord;
    F_RealClickPos, F_RealMousePos: TPoint;
    F_ScrollClickPos: integer;
    F_LastMouseMove: cardinal;
    F_ClickedItem: TplTreeListItem;
    F_MouseSelecting: (msNone,msLeft,msRight);
    F_MouseSelectingFocusRectDraw: boolean;
    F_MouseSelectingFocusRect: TRect;
    F_ClickAtItem:TItemEvent;
    F_ItemCollapsed:TItemEvent;
    F_ItemExpanded:TItemEvent;
    F_ClickAtRecordItem:TRecordItemEvent;
    F_OnSelect:TItemEvent;
    F_OnItemExpanded: TItemEvent;
    F_OnItemCollapsed: TItemEvent;
    F_OnItemsSorted: TNotifyEvent;
    f_searchMarkItem: TplTreeListItem;
    f_searchMarkCol,f_searchMarkStart,f_searchMarkLen: longint;
    f_searchMarkVisible,f_searchActivated:boolean;
    f_colorSearchMark: tcolor;
    f_colorSearchMarkField: tcolor;
    F_SearchBar: TplSearchPanel;
    F_NewSearchBarFindState: TFindState;
    F_HighlightAll: boolean;

    procedure SearchBarSearch(sender: TObject; incremental, backwards: boolean);
    procedure SearchBarClose(Sender: TObject);
    procedure SearchBarShow(Sender: TObject);
    procedure SearchBarKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure F_SearchBarHighlightChanged(Sender: TObject);
    function  DoCustomBackgroundDrawEvent (eventTyp_cdet:TCustomDrawEventTyp):boolean;
    function  DoCustomItemDrawEvent(const eventTyp_cdet:TCustomDrawEventTyp;const item:TplTreeListItem):boolean;
    function  DoCustomRecordItemDrawEvent(const eventTyp_cdet:TCustomDrawEventTyp;const RecordItem:TplTreeListRecordItem;const outrec: TRect):boolean;
    procedure removeSelection(list: TplTreeListItems);
    procedure removeMouseSelection(list: TplTreeListItems);
    procedure setMouseSelection(list: TplTreeListItems);
    procedure DoSelect(item: TplTreeListItem);virtual;
    procedure selectRange(a,b: TplTreeListItem;mouseSelect:boolean=false);
    procedure SetOptions(const AValue: TplTreeListViewOptions);
    procedure SetOption(const Option: TplTreeListViewOption; const active:boolean);
    procedure SetItems(const value:TplTreeListItems);
    procedure SetFocused(const AValue: TplTreeListItem);
    procedure SetSelected(const AValue: TplTreeListItem);
    procedure setTopItem(item: TplTreeListItem);
    function  GetTopItem:TplTreeListItem;
    function  GetTopItemEven: boolean;
    function  GetTopPos:integer;
    procedure SetSortColumn(const AValue: longint);
    procedure SetColumns(const value:THeaderSections);
    function  GetColumns: THeaderSections;
    procedure setImageList(const images:TImageList);
    procedure SetRowHeight(const newHeight:integer);
    procedure SetHotTrackFont(const value:TFont);
    procedure SetSelectedFont(const value:TFont);
    procedure SetSelectedHotTrackFont(const value:TFont);
    function  RealControlHeight(c: Twincontrol): longint;
    function  RealBaseClientWidth: longint;
    function  RealBaseClientHeight: longint;
    function  RealClientHeight: longint;
    procedure DrawAlignDotLine(x,y:integer;const x2,y2:integer;const color:TColor);
    procedure drawTextRect(s:string;extraIndentation:longint;align:TAlignment; const rec: TRect; searchDraw: boolean);
    function  CompareItems(i1, i2: TplTreeListItem): longint;
    procedure BeginMultipleUpdate;
    procedure EndMultipleUpdate;
    procedure updateAll();
    procedure _SubItemListEvent(list: TObjectList; typ: TListEventTyp);
    procedure _RecordItemListEvent(list: TObjectList; typ: TListEventTyp);
    procedure _HeaderSectionTrack( HeaderControl: TEventHeaderControl;  Section: THeaderSection;  Width: Integer;  State: TSectionTrackState);
    procedure _HeaderSectionResize( HeaderControl: TEventHeaderControl;  Section: THeaderSection);
    procedure _HeaderSectionClick( HeaderControl: TEventHeaderControl;  Section: THeaderSection);
    procedure _HeaderSectionDblClick( HeaderControl: TEventHeaderControl;  Section: THeaderSection);
    procedure _HeaderSectionEndDrag(Sender: TObject);
    {$ifdef allowHeaderVisible}
    procedure ColumnPopupMenuClick(Sender: TObject);
    {$endif}
    procedure _HScrollChange(Sender: TObject);
    procedure _VScrollChange(Sender: TObject);
    procedure UpdateScrollBarPos; virtual;
  protected
    {$ifdef android}
    F_PostMessage: TLMessage;
    F_PostMessageTimer: TTimer;
    procedure PostMessageTimerTimer(Sender: TObject);
    {$endif}
    procedure internPostMessage(Msg: Cardinal; WParam: WParam);
  public
    hotTrackedRecordItem:TplTreeListRecordItem;
    constructor Create(aowner:TComponent);override;
    destructor Destroy;override;

    function search(searchFor: string; searchFields:cardinal; backward: boolean=false;extendSelection: boolean=false): TFindState; overload;
    function ColumnFromOriginalIndex(index: longint):  THeaderSection;
    function GetItemAtPos(const y:integer):TplTreeListItem;
    function GetRecordItemAtPos(const x,y:integer):TplTreeListRecordItem;
    function VisibleRowCount:longint;
    function serializeColumnWidths: string;
    function serializeColumnOrder: string;
    function serializeColumnVisibility: string;
    procedure UpdateScrollSizeH;
    procedure UpdateScrollSizeV;
    procedure UpdateScrollSize;
    procedure loaded;override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure sort;
    procedure ensureVisibility(item: TplTreeListItem;column: longint=-1);
    procedure CreateUserColumnVisibilityPopupMenu();
    procedure deserializeColumnWidths(s: string);
    procedure deserializeColumnOrder(s: string);
    procedure deserializeColumnVisibility(s: string);
    procedure CreateSearchBar();
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure WndProc(var message:TLMessage);override;
    procedure sheduleInternRepaint();
    procedure internRepaint();
    procedure invalidateItem(item: TplTreeListItem=nil);
    procedure invalidateAll();
    procedure internDraw();
    procedure internPaint(calledFromPaintingEvent: boolean=false);
    procedure Paint; override;

    property selCount: longint read F_SelCount;
    {$warnings off}
    property focused:TplTreeListItem read F_Focused write SetFocused;
    {$warnings on}
    property Selected:TplTreeListItem read F_Focused write SetSelected;
    property SortColumn: longint read F_SortColumn write SetSortColumn;
    property SearchBar: TplSearchPanel read F_SearchBar;
    property TopPos:integer read GetTopPos;
    property TopItem: TplTreeListItem read GetTopItem write SetTopItem;
    property TopItemIsEvenItem: boolean read GetTopItemEven;
    property TopItemVisualIndex: integer read GetTopItemVisualIndex;
    property DrawingEvenItem: boolean read F_DrawingEvenItem;
    property DrawingYPos: longint read F_DrawingYPos;
    property DrawingRecordItemRect: TRect read F_DrawingRecordItemRect;
  published
    property Options: TplTreeListViewOptions read F_Options write SetOptions;
    property Columns:THeaderSections read GetColumns write SetColumns;
    property RowHeight:integer read F_RowHeight write SetRowHeight;
    property Images:TImageList read F_ImageList write setImageList;
    property HorizontalLineMode:TLineMode read F_HorizontalLines write SetHorizontalLines;
    property HorizontalLineColor:TColor read F_HorizontalLineColor write SetHorizontalLineColor;
    property VerticalLineMode:TLineMode read F_VerticalLines write SetVerticalLines;
    property VerticalLineColor:TColor read F_VerticalLineColor write SetVerticalLineColor;
    property RootLineMode:TLineMode read F_RootLines write SetRootLines;
    property RootLineColor:TColor read F_RootLineColor write SetRootLineColor;
    property ColorSearchMark: tcolor read F_ColorSearchMark write SetColorSearchMark;
    property ColorSearchMarkField: tcolor read F_ColorSearchMarkField write SetColorSearchMarkField;
    property ExpandMode:TExpandMode read F_ExpandMode write SetExpandMode;
    property HotTrackFont:TFont read F_HotTrackFont write SetHotTrackFont;
    property SelectedFont:TFont read F_SelectedFont write SetSelectedFont;
    property SelectedHotTrackFont:TFont read F_SelectedHotTrackFont write SetSelectedHotTrackFont;
    property StripedOddColor:TColor read F_StripedOddColor write SetStripedOddColor;
    property StripedEvenColor:TColor read F_StripedEvenColor write SetStripedEvenColor;
    property SelectBackColor:TColor read F_SelectBackColor write SetSelectBackColor;
    property ButtonColor:TColor read F_ButtonColor write SetButtonColor;
    property BackGroundColor:TColor read F_BgColor write SetBgColor;
    property Items:TplTreeListItems read F_Items write SetItems;
    property Scrollbars: TScrollStyle read F_ScrollStyle write SetScrollStyle;
    property HeaderVisible: boolean read F_HeaderVisible write SetHeaderVisible;
    property OnCompareItems: TCompareTreeListItemsEvent read F_OnCompareItems write F_OnCompareItems;
    property OnUserSortItemsEvent: TUserSortItemsEvent read F_OnUserSortItems write F_OnUserSortItems;
    property OnItemsSortedEvent: TNotifyEvent read F_OnItemsSorted write F_OnItemsSorted;
    property OnVScrollBarChange:TNotifyEvent read F_VScrollBarChange write F_VScrollBarChange;
    property OnHScrollBarChange:TNotifyEvent read F_HScrollBarChange write F_HScrollBarChange;
    property OnCustomBgDraw:TCustomBackgroundDrawEvent read F_CustomBgDraw write F_CustomBgDraw;
    property OnCustomItemDraw:TCustomItemDrawEvent read F_CustomItemDraw write F_CustomItemDraw;
    property OnCustomRecordItemDraw:TCustomRecordItemDrawEvent read F_CustomRecordItemDraw write F_CustomRecordItemDraw;
    property OnCustomRecordItemPositioning: TCustomRecordItemPositioningEvent read F_CustomRecordItemPositioningEvent write F_CustomRecordItemPositioningEvent;
    property OnClickAtRecordItem:TRecordItemEvent read F_ClickAtRecordItem write F_ClickAtRecordItem;
    property OnClickAtItem:TItemEvent read F_ClickAtItem write F_ClickAtItem;
    property OnSelect:TItemEvent read F_OnSelect write F_OnSelect;
    property OnItemCollapsed:TItemEvent read F_OnItemCollapsed write F_OnItemCollapsed;
    property OnItemExpanded:TItemEvent read F_OnItemExpanded write F_OnItemExpanded;
    property OnHeaderSectionResize:TCustomSectionNotifyEvent read F_HeaderSectionResize write F_HeaderSectionResize;
    property OnHeaderSectionTrack:TCustomSectionTrackEvent read F_HeaderSectionTrack write F_HeaderSectionTrack;

    property Font;
    property TabStop default true;
    property TabOrder;
    property Align;
    property Anchors;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Hint;
    property PopupMenu;
    property ShowHint;
    property OnDockDrop;
    property OnDockOver;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnUnDock;
    property OnClick;
    property OnConstrainedResize;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation


const HeaderItemDistance=2; //Distance between Header and first drawn item
      LEFT_TEXT_PADDING=3;
      LINE_DISTANCE=15;
      LM_USER_SHEDULED_EVENT = LM_USER+1125;
      EVENT_REPAINT = 1;
      EVENT_MOUSE_SCROLL = 2;
      EVENT_HSCROLL = 3;



constructor TObjectList.Create(listEvent: TListEvent);
begin
  inherited Create;
  self.onListEvent:=listEvent;
end;

procedure TObjectList.BeginEdit;
begin
  if assigned(onListEvent) then onListEvent(self,levBeginEdit);
end;

procedure TObjectList.EndEdit;
begin
  if assigned(onListEvent) then onListEvent(self,levEndEdit);
end;

function TObjectList.AddObject(Item: TObject): Integer;
begin
  result:=add(item);
end;

procedure TObjectList.InsertObject(Index: Integer; Item: TObject);
begin
  Insert(index,item);
end;

function TObjectList.RemoveObject(Item: TObject): Integer;
begin
  result:=remove(item);
end;

function TObjectList.Add(Item: TObject): Integer;
begin
  result:=inherited add(item);
  if assigned(onListEvent) then onListEvent(self,levAdd);
end;
procedure TObjectList.Clear;
begin
  BeginEdit;
  FreeObjects;
  count:=0;
  if assigned(onListEvent) then onListEvent(self,levClear);
  EndEdit;
end;
procedure TObjectList.Delete(Index: Integer);
begin
  inherited;
  if assigned(onListEvent) then onListEvent(self,levDelete);
end;
procedure TObjectList.Exchange(Index1, Index2: Integer);
begin
  inherited;
  if assigned(onListEvent) then onListEvent(self,levExchange);
end;
procedure TObjectList.Insert(Index: Integer; Item: TObject);
begin
  inherited insert(index,item);
  if assigned(onListEvent) then onListEvent(self,levInsert);
end;
procedure TObjectList.Move(CurIndex, NewIndex: Integer);
begin
  inherited;
  if assigned(onListEvent) then onListEvent(self,levMove);
end;
function TObjectList.Remove(Item: TObject): Integer;
begin
  result:=inherited remove(item);
  item.free;
  if assigned(onListEvent) then onListEvent(self,levDelete);
end;
procedure TObjectList.Sort(Compare: TListSortCompare);
begin
  inherited;
  if assigned(onListEvent) then onListEvent(self,levSort);
end;
procedure TObjectList.Assign(list:TList);
var i:integer;
begin
  Count:=list.Count;
  for i:=0 to Count-1 do
    Items[i]:=list.Items[i];
  if assigned(onListEvent) then onListEvent(self,levAssign);
end;
procedure TObjectList.FreeObjects;
var i:integer;
begin
  for i:=0 to Count-1 do begin
    TObject(Items[i]).free;
    Items[i]:=nil;
  end;
  count:=0;
end;
destructor TObjectList.Destroy;
begin
  FreeObjects;
  inherited;
end;

//===================================== TplTreeListItems ==============================

procedure TplTreeListItems.Put(Index: Integer; const AValue: TplTreeListItem);
begin
  inherited put(Index, AValue);
end;

function TplTreeListItems.Get(Index: Integer): TplTreeListItem;
begin
  result:=TplTreeListItem(inherited get(index));
end;

procedure TplTreeListItems.Sort(CompareFunc: TplTreeListItemCompare);
var temp: array of TplTreeListItem;

  procedure mergeSort(f,t:longint);
  var a,b,d,p: longint;
  begin
    if f>=t then exit;
    d:=(f+t) div 2;
    mergeSort(f,d);
    mergeSort(d+1,t);

    system.move(List^[f],temp[f],(t-f+1)*sizeof(TplTreeListItem));

    p:=f;
    a:=f;
    b:=d+1;
    while (a<=d) and (b<=t) do begin
      if CompareFunc(temp[a],temp[b])<=0 then begin
        List[p]:=temp[a];
        inc(a);
      end else begin
        List[p]:=temp[b];
        inc(b);
      end;
      inc(p);
    end;
    if a<=d then begin
      f:=a;
      t:=d;
    end else if b<=t then f:=b
    else exit;

      system.move(temp[f],List[p],(t-f+1)*sizeof(TplTreeListItem));

  end;
var i:longint;
begin

  for i:=0 to count-1 do
    Items[i].SubItems.Sort(CompareFunc);
  SetLength(temp,count);
  mergeSort(0,count-1);
  if assigned(onListEvent) then onListEvent(self,levSort);
end;

constructor TplTreeListItems.Create(parent:TplTreeListItem;const TreeListView:TplTreeListView);
begin
  inherited Create(TreeListView._SubItemListEvent);
  F_Parent:=parent;
  F_TreeListView:=TreeListView;
end;

function TplTreeListItems.Add(caption:string):TplTreeListItem;
begin
  Result:=TplTreeListItem.Create(F_Parent,F_TreeListView, caption);
  if F_Parent<>nil then result.f_Indent:=F_Parent.Indent+1;
  inherited add(result);
end;

function TplTreeListItems.Add(Parent:TplTreeListItem;caption:string):TplTreeListItem;
begin
  if Parent=nil then result:=Add(caption)
  else result:=Parent.SubItems.Add(caption);
end;

function TplTreeListItems.GetRealItemCount(const countTyp:TRealItemCounting ) :integer;
var i:integer;
begin
  result:=1;
  for i:=0 to Count-1 do
    if ((TObject(items[i]) as TplTreeListItem).expanded) or (ricCountCollapsedSubItems in countTyp) then
      result:= Result+TplTreeListItem(items[i]).SubItems.GetRealItemCount(countTyp)
     else
      inc(result);
end;

function TplTreeListItems.RealIndexOf(const item:TplTreeListItem;const countTyp:TRealItemCounting):integer;
var pos:integer;
    gefunden:boolean;
  procedure RealIndexOfRek(const newself:TplTreeListItem);
  var i:integer;
  begin
    for i:=0 to newself.SubItems.Count-1 do begin
      inc(pos);
      if newself.SubItems[i]=item then begin
        gefunden:=true;
        break;
      end;
      if (TObject(newself.SubItems[i]) as TplTreeListItem).Expanded or (ricCountCollapsedSubItems in counttyp) then
        RealIndexOfRek(newself.SubItems[i]);
      if gefunden then break;
    end;
  end;
var i:integer;
begin
  pos:=-1;
  gefunden:=false;
    for i:=0 to Count-1 do begin
      inc(pos);
      if Items[i]=item then begin
        gefunden:=true;
        break;
      end;
      if (TObject(Items[i]) as TplTreeListItem).Expanded or (ricCountCollapsedSubItems in counttyp) then
        RealIndexOfRek(Items[i]);
      if gefunden then break;
    end;
  if gefunden then Result:=pos
  else Result:=-1;
end;

function TplTreeListItems.FindItemWithText(caption: string): TplTreeListItem;
var i:longint;
begin
  result:=nil;
  for i:=0 to count-1 do
    if Items[i].Text=caption then begin
      result:=Items[i];
      exit;
    end else begin
      result:=Items[i].SubItems.FindItemWithText(caption);
      if result<>nil then exit;
    end;
end;

function TplTreeListItems.FindItemWithRecordText(text: string;pos: longint=0
  ): TplTreeListItem;
var i:longint;
begin
  result:=nil;
  for i:=0 to count-1 do
    if Items[i].RecordItemsText[pos]=text then begin
      result:=Items[i];
      exit;
    end else begin
      result:=Items[i].SubItems.FindItemWithRecordText(text,pos);
      if result<>nil then exit;
    end;
end;

function TplTreeListItems.find(searchFor: string; searchFields: cardinal;
  backward: boolean; loopAround: PBOOL; startItem: TplTreeListItem;
  startColumn: longint; startTextPosition: longint): TplTreeListRecordItem;
  function checkStr(item:TplTreeListItem; col: integer):TplTreeListRecordItem;
  var stp:integer;
      currentText:string;
  begin
    result:=nil;
    currentText:=item.RecordItemsText[col];
    stp:=pos(searchFor,lowercase(currentText));
    if stp>0 then begin
      if (item=startItem) and
         ((col<startColumn)or((col=startColumn)and(stp<startTextPosition))) then
           exit;

      result:=item.RecordItems[col];
    end;
  end;

  function checkItem(item:TplTreeListItem): TplTreeListRecordItem;
  var i:longint;
  begin
    Result:=nil;
    for i:=0 to F_TreeListView.Columns.count-1 do
      if {$ifdef allowHeaderDragging}F_TreeListView.Columns[i].OriginalIndex{$else}i{$endif}<32 then
        if (1 shl {$ifdef allowHeaderDragging}F_TreeListView.Columns[i].OriginalIndex{$else}i{$endif}) and searchFields <> 0 then begin
          result:=checkStr(item,{$ifdef allowHeaderDragging}F_TreeListView.Columns[i].OriginalIndex{$else}i{$endif});
          if result<>nil then exit;
        end;
  end;


  function searchRemainingForward(items: TplTreeListItems):TplTreeListRecordItem;
  var i:longint;
  begin
    Result:=nil;
    for i:=0 to items.Count-1 do begin
      result:=checkItem(items[i]);
      if result<>nil then exit;
      result:=searchRemainingForward(items[i].SubItems);
      if Result<>nil then exit;
    end;
  end;

  function searchBackwards(items: TplTreeListItems):TplTreeListRecordItem;
  var i:longint;
  begin
    Result:=nil;
    for i:=items.Count-1 downto 0 do begin
      result:=searchBackwards(items[i].SubItems);
      if Result<>nil then exit;
      result:=checkItem(items[i]);
      if result<>nil then exit;
    end;
  end;

var curItem: TplTreeListItem;
    stack: TItemHierarchyStack;
    lastStackSize:longint;

begin
  Result:=nil;
  if count=0 then exit;
  if loopAround<>nil then loopAround^:=false;
  if searchFor='' then exit;
  if startItem=nil then startItem:=Items[0];
  searchFor:=LowerCase(searchFor);

  curItem:=startItem;
  curItem.GetParentHierarchyStack(stack);
  if not backward then begin
    repeat
      result:=checkItem(curItem);
      if result<>nil then exit;
      curItem:=curItem.GetNextFromHierarchyStack(stack);
    until curItem=nil;

    result:=searchRemainingForward(self);
    if (result <>nil) and (loopAround<>nil) then loopAround^:=true;
  end else begin
    laststacksize:=stack.size+1;
    while stack.size>0 do begin
      if stack.size=laststacksize then begin
         result:=searchBackwards(stack.stack[stack.size-1].list[stack.stack[stack.size-1].index].SubItems);
         if result<>nil then exit;
      end;
      result:=checkItem(stack.stack[stack.size-1].list[stack.stack[stack.size-1].index]);
      if result<>nil then exit;
      laststacksize:=stack.size;
      stack.stack[stack.size-1].index:=stack.stack[stack.size-1].index-1;
      if stack.stack[stack.size-1].index<0 then begin
        dec(stack.size);
        if stack.size=0 then break;
      end;
    end;
    result:=searchBackwards(self);
    if (result<>nil) and (loopAround<>nil) then loopAround^:=true;
  end;
end;

function TplTreeListItems.GetItemWithRealIndex(index:integer;const countTyp:TRealItemCounting):TplTreeListItem;
  function GetItemWithRealIndexRek(const nself:TplTreeListItems;var index:integer):TplTreeListItem;
  var i:integer;
  begin
    result:=nil;
    for i:=0 to nself.count-1 do begin
      dec(index);
      if index<=0 then begin result:=nself[i];exit;end
      else begin
        result:=GetItemWithRealIndexRek(nself[i].SubItems,index);
        if index<=0 then exit;
      end;
    end;
  end;
  function GetItemWithRealIndexRekVisible(const nself:TplTreeListItems;var index:integer):TplTreeListItem;
  var i:integer;
  begin
    result:=nil;
    for i:=0 to nself.count-1 do begin
      dec(index);
      if index=0 then begin result:=nself[i];exit;end
      else if nself[i].Expanded then begin
        result:=GetItemWithRealIndexRekVisible(nself[i].SubItems,index);
        if index<=0 then exit;
      end;
    end;
  end;
var i:integer;
begin
  if index<0 then begin
    result:=nil;
    exit;
  end;
  i:=index+1;
  if ricCountCollapsedsubItems in countTyp then result:=GetItemWithRealIndexRek(self,i)
  else result:=GetItemWithRealIndexRekVisible(self,i);
end;


//====================== TRecordItemList ==========================

function TRecordItemList.Get(Index: Integer): TplTreeListRecordItem;
begin
  result:=TplTreeListRecordItem(inherited Get(Index));
end;

procedure TRecordItemList.Put(Index: Integer;
  const AValue: TplTreeListRecordItem);
begin
  inherited Put(Index, AValue);
end;

function TRecordItemList.Add:TplTreeListRecordItem;
begin
  Result:=TplTreeListRecordItem.Create(owner);
  inherited add(result);
end;

procedure TRecordItemList.AddItem(recordItem: TplTreeListRecordItem);
begin
  inherited add(recordItem);
end;

function TRecordItemList.Add(s: string): TplTreeListRecordItem;
begin
  result:=Add;
  result.Text:=s;
end;


// ================= TplTreeListRecordItem =============================================


function TplTreeListRecordItem.getIndex(): longint;
begin
  result:=F_Parent.RecordItems.IndexOf(self);
end;

procedure TplTreeListRecordItem.SetText(caption: string);
begin
  F_Text:=caption;
  if F_Parent <> nil then
    F_Parent.SheduleInternRepaint();
end;

procedure TplTreeListRecordItem.selectFont(can: TCanvas);
begin
  if F_Parent.TreeListView.hotTrackedRecordItem = self then begin
    if F_Parent.SeemsSelected then
     can.Font.Assign(F_Parent.TreeListView.SelectedHotTrackFont)
    else
     can.Font.Assign(F_Parent.TreeListView.HotTrackFont);
   end else if F_Parent.SeemsSelected then
    can.Font.Assign(F_Parent.TreeListView.SelectedFont)
   else
    can.Font.Assign(F_Parent.TreeListView.Font);
end;


function TplTreeListRecordItem.GetNecessaryWidth(listView:TplTreeListView=nil): longint;
begin
  if listView=nil then listView:=F_Parent.F_TreeListView;
  selectFont(listView.Canvas);
  Result:=listView.Canvas.TextWidth(Text);
end;

constructor TplTreeListRecordItem.Create(aparent:TplTreeListItem);
begin
  inherited Create;
  F_Parent:=aparent;
end;

constructor TplTreeListRecordItem.Create(aparent:TplTreeListItem; caption: string);
begin
  inherited Create;
  F_Parent:=aparent;
  SetText(caption);
end;

destructor TplTreeListRecordItem.Destroy;
begin
  inherited Destroy;
end;

//======================= TplTreeListItem ============================

constructor TplTreeListItem.Create(const parent: TplTreeListItem; const TreeListView: TplTreeListView; const ACaption: string);
begin
  assert(TreeListView<>nil);
  inherited Create;
  F_Parent:=parent;
  if Parent=nil then F_Indent:=0
  else F_Indent:=Parent.F_Indent+1;
  F_TreeListView:=TreeListView;
  f_Selected:=false;
  F_RecordItems:=TRecordItemList.Create(TreeListView._RecordItemListEvent);
  F_RecordItems.Owner:=self;
  F_RecordItems.Add(ACaption);
  F_SubItems:=TplTreeListItems.Create(self,TreeListView);
  Expanded:=true;
end;

function TplTreeListItem.getBounds(column: longint): TRect;
begin
  result.Top:=(F_TreeListView.Items.RealIndexOf(self,[]))*F_TreeListView.RowHeight+F_TreeListView.TopPos;
  result.bottom:=Result.top+F_TreeListView.RowHeight;
  if column=-1 then begin
    result.Left:=0;
    Result.right:=F_TreeListView.F_Header.Width;
  end else if column<TreeListView.Columns.Count then begin
    Result.left:=TreeListView.ColumnFromOriginalIndex(column).Left-F_TreeListView.F_HScroll.Position;
    Result.right:=result.left+TreeListView.ColumnFromOriginalIndex(column).Width;
  end else begin
    Result.Left:=0;
    Result.Right:=0;
  end;
end;

function TplTreeListItem.getMaxTextBounds(column: longint): TRect;
begin
  Result:=getBounds(column);
  if (column>0) and (column<TreeListView.Columns.Count) then begin
    case TreeListView.Columns[column].Alignment of
      taLeftJustify: Result.Left:=result.Left+LEFT_TEXT_PADDING;
      taRightJustify: Result.Right:=result.Right-LEFT_TEXT_PADDING;
    end;
  end else if column=0 then
    Result.Left:=Result.Left+LEFT_TEXT_PADDING+GetExtraTextIndentation(column);
  if result.Left>result.right then Result.Left:=Result.Right;
end;

function TplTreeListItem.GetRecordItemsText(i: Integer): string;
begin
  if i<RecordItems.Count then result:=RecordItems[i].Text
  else result:='';
end;

procedure TplTreeListItem.SetRecordItemsText(i: Integer; const AValue: string);
var j:longint;
begin
  if i<RecordItems.Count then RecordItems[i].Text:=AValue
  else begin
    for j:=RecordItems.Count to i-1 do RecordItems.Add('');
    RecordItems.Add(AValue);
    SheduleInternRepaint();
  end;
end;

procedure TplTreeListItem.SheduleInternRepaint;
begin
  if F_TreeListView <> nil then
  begin
    F_TreeListView.invalidateItem(Self);
    F_TreeListView.sheduleInternRepaint();
  end;
end;

function TplTreeListItem.GetText: string;
begin
  if RecordItems.Count>0 then result:=RecordItems[0].Text
  else result:='';
end;

procedure TplTreeListItem.SetText(const AValue: string);
begin
  if RecordItems.Count>0 then RecordItems[0].Text:=AValue
  else RecordItems.Add(AValue);
end;
function TplTreeListItem.GetExtraTextIndentation(column: longint): longint;
begin
  result:=0;
  if column<>0 then exit;
  result:=F_Indent*LINE_DISTANCE+TreeListView.TreeColumnIndentation;
  if ImageBitmap<>nil then Result:=Result+ImageBitmap.width+LEFT_TEXT_PADDING
  else if (ImageIndex>-1) and (TreeListView.Images<>nil) then Result:=Result+TreeListView.Images.Width+LEFT_TEXT_PADDING;
end;

function TplTreeListItem.GetExtendingButtonPos: longint;
begin
  result:=TreeListView.F_TreeSectionPos.left+(F_Indent+1)*LINE_DISTANCE-10;
end;

procedure TplTreeListItem.SetMouseSelected(newSelected: boolean);
begin
  if F_MouseSelected = newSelected then exit;
  F_MouseSelected:=newSelected;
  TreeListView.invalidateItem(self);
end;

procedure TplTreeListItem.SetSelected(newSelected: boolean);
begin
  if Selected = newSelected then exit;
  F_Selected:=newSelected;
  if F_Selected then begin
    if not (tlvoMultiSelect in TreeListView.F_Options) then TreeListView.focused:=self;
    TreeListView.F_SelCount:=TreeListView.F_SelCount+1;
  end else TreeListView.F_SelCount:=TreeListView.F_SelCount-1;
  TreeListView.DoSelect(self);
  TreeListView.invalidateItem(self);
  TreeListView.internPaint;
end;

procedure TplTreeListItem.SetSelections(realSelected, mouseSelection:boolean);
begin
  if realSelected xor mouseSelection <> Selected xor F_MouseSelected then
    TreeListView.invalidateItem(self);
  F_Selected:=realSelected;
  F_MouseSelected:=mouseSelection;
end;


function TplTreeListItem.SeemsSelected: boolean;
begin
  if TreeListView.F_MouseSelecting<>msNone then result:=Selected xor F_MouseSelected
  else result:=Selected;
end;

procedure TplTreeListItem.SetRecordItems(const value:TRecordItemList);
begin
  F_RecordItems.Assign(value);
end;

procedure TplTreeListItem.SetExpand(const expanded:boolean);
begin
  if expanded then expand else Collapse;
end;

procedure TplTreeListItem.Expand;
begin
  if F_Expanded then exit;
  F_Expanded:=true;
  TreeListView.internRepaint();
  if assigned(TreeListView.F_OnItemExpanded) then
    TreeListView.F_OnItemExpanded(TreeListView,self);

end;
procedure TplTreeListItem.Collapse;
begin
  if not F_Expanded then exit;
  F_Expanded:=false;
  TreeListView.internRepaint();
  if assigned(TreeListView.F_OnItemCollapsed) then
    TreeListView.F_OnItemCollapsed(TreeListView,self);
end;


procedure TplTreeListItem.SetSubItems(const value: TplTreeListItems);
begin
  F_SubItems.Assign(value);
end;

function TplTreeListItem.GetItemAtPos(const listView:TplTreeListView;const TestY:integer;var startY:integer):TplTreeListItem;
var i:integer;
begin
  result:=nil;
  if (TestY>startY)and(TestY<startY+listView.RowHeight) then begin
    result:=self;
    exit;
  end;
  startY:=startY+listView.RowHeight;
  if Expanded then
    for i:=0 to SubItems.Count-1 do begin
      result:=(TObject(SubItems[i]) as TplTreeListItem).GetItemAtPos(listView,TestY,startY);
      if result<>nil then exit;
    end ;
end;

function TplTreeListItem.GetRecordItemAtPos(const listView:TplTreeListView;TestX:integer):TplTreeListRecordItem;
var i,x:integer;
begin
  Result:=nil;
  x:=0;
  TestX+=listView.F_HScroll.Position;
  for i:=0 to min(listView.F_Header.Sections.Count-1,RecordItems.Count-1) do begin
    if TestX<x then exit;
    if (x+listView.F_Header.Sections[i].Width>TestX) then begin

        x:=listView.F_Header.Sections[i].OriginalIndex;

      if (x>=0)and(x<RecordItems.Count) then  result:=RecordItems[x]
      else Result:=nil;
      exit;
    end;
    x:=x+listView.F_Header.Sections[i].Width;
  end;
end;

function TplTreeListItem.GetMaxColumnWidth(const id: longint): longint;
var i,temp:longint;
begin
  if id=0 then result:=RecordItems[0].GetNecessaryWidth()+LINE_DISTANCE*F_Indent+13+LEFT_TEXT_PADDING
  else if id<RecordItems.Count then result:=RecordItems[id].GetNecessaryWidth()
  else result:=0;
  for i:=0 to SubItems.Count-1 do begin
    temp:=SubItems[i].GetMaxColumnWidth(id);
    if temp>result then result:=temp;
  end;
end;

function TplTreeListItem.GetNextItemIgnoringChildren: TplTreeListItem;
var temp:integer;
begin
  if (parent=nil) then begin
    temp:=TreeListView.Items.IndexOf(self);
    if temp<TreeListView.Items.Count-1 then Result:=TreeListView.Items[temp+1]
    else Result:=nil;
  end else begin
    temp:=Parent.SubItems.IndexOf(self);
    if temp<Parent.SubItems.Count-1 then Result:=parent.SubItems[temp+1]
    else result:=parent.GetNextItemIgnoringChildren;
  end;
end;


function TplTreeListItem.GetLastVisibleSubSubItem:TplTreeListItem;
begin
  result:=self;
  while (result.SubItems.Count<>0)and(result.Expanded) do
    Result:=Result.SubItems[Result.SubItems.count-1];
end;

function TplTreeListItem.GetLastSubSubItem: TplTreeListItem;
begin
  result:=self;
  while (result.SubItems.Count<>0) do
    Result:=Result.SubItems[Result.SubItems.count-1];
end;

function TplTreeListItem.GetNextVisibleItem(Delta: longint=1): TplTreeListItem;
var next: TplTreeListItem;
begin
  if Delta<0 then begin
    result:=GetPrevVisibleItem(-Delta);
    exit;
  end;
  Result:=self;
  while delta>0 do begin
    if (result.SubItems.Count>0) and (result.Expanded) then
      next:=result.SubItems[0]
    else begin
      next:=result.GetNextItemIgnoringChildren;
      if next=nil then next:=result;
      if next=result then exit;
    end;
    result:=next;
    dec(delta,1);
  end;
end;

function TplTreeListItem.GetPrevVisibleItem(Delta: longint): TplTreeListItem;
var temp:longint;
begin
  if Delta<0 then begin
    result:=GetNextVisibleItem(-Delta);
    exit;
  end;
  Result:=self;
  while delta>0 do begin
    if result.Parent=nil then begin
      temp:=result.TreeListView.Items.IndexOf(result);
      if temp>0 then Result:=result.TreeListView.Items[temp-1].GetLastVisibleSubSubItem;
    end else begin
      temp:=Result.Parent.SubItems.IndexOf(Result);
      if temp=0 then result:=result.parent
      else result:=result.parent.SubItems[temp-1].GetLastVisibleSubSubItem;
    end;
    dec(Delta);
  end
end;

function TplTreeListItem.GetNextItem(): TplTreeListItem;
begin
  if SubItems.Count>0 then result:=SubItems[0]
  else result:=GetNextItemIgnoringChildren;
  if result=nil then result:=self;
end;

function TplTreeListItem.GetPrevItem(): TplTreeListItem;
var temp:longint;
begin
  result:=self;
  if result.Parent=nil then begin
    temp:=result.TreeListView.Items.IndexOf(result);
    if temp>0 then Result:=result.TreeListView.Items[temp-1].GetLastSubSubItem;
  end else begin
    temp:=Result.Parent.SubItems.IndexOf(Result);
    if temp=0 then result:=result.parent
    else result:=result.parent.SubItems[temp-1].GetLastSubSubItem;
  end;
  if result=nil then result:=self;
end;

function TplTreeListItem.GetParentInList(List: TplTreeListItems
  ): TplTreeListItem;
begin
  if list=nil then list:=TreeListView.Items;
  if ParentItems = list then result:=self
  else if Parent = nil then result:=nil
  else result:=parent.GetParentInList(list);
end;

procedure TplTreeListItem.GetParentHierarchyStack(out stack:TItemHierarchyStack);
var curItem:TplTreeListItem;
begin

  SetLength(stack.stack,8);
  stack.size:=0;
  curItem:=self;
  while curItem <> nil do begin
    stack.size:=stack.size+1;
    if stack.size>length(stack.stack) then SetLength(stack.stack,length(stack.stack)*2);
    system.move(stack.stack[0],stack.stack[1],(stack.size-1)*sizeof(stack.stack[0]));
    stack.stack[0].list:=curItem.ParentItems;
    stack.stack[0].index:=curItem.ParentItems.IndexOf(curItem);
    curItem:=curItem.Parent;
  end;
end;
function TplTreeListItem.GetNextFromHierarchyStack(var stack: TItemHierarchyStack; const mustBeVisible: boolean): TplTreeListItem;
begin
  assert(stack.size>0);
  assert(self=stack.stack[stack.size-1].list[stack.stack[stack.size-1].index]);
  if (self.SubItems.Count>0) and (Expanded or not mustBeVisible) then begin
    if length(stack.stack)<=stack.size then setlength(stack.stack,length(stack.stack)*2);
    stack.stack[stack.size].list:=F_SubItems;
    stack.stack[stack.size].index:=0;
    inc(stack.size);
    result:=SubItems[0];
  end else begin
    stack.stack[stack.size-1].index:=stack.stack[stack.size-1].index+1;
    while stack.stack[stack.size-1].index>=stack.stack[stack.size-1].list.count do begin
      dec(stack.size);
      if stack.size=0 then begin
        result:=nil;
        exit;
      end;
      stack.stack[stack.size-1].index:=stack.stack[stack.size-1].index+1;
    end;
    result:=stack.stack[stack.size-1].list[stack.stack[stack.size-1].index];
  end;
end;

function TplTreeListItem.ParentItems: TplTreeListItems;
begin
  if Parent = nil then result:=TreeListView.Items
  else result:=Parent.SubItems;
end;

procedure TplTreeListItem.Paint(const hierarchyStack: TItemHierarchyStack);
var i,ynew,yold,recordId:integer;
    defaultDraw:boolean;
    rec:Trect;

  procedure drawTreeColumnText;
  var textStartX,imageX: longint;
      drawSearchMark: boolean;
      {$ifdef useRealClipping}
      oldClipping:boolean;
      oldClippingRect:TRect;
      {$ELSE}
      tempBitmap: graphics.TBitmap;
      {$ENDIF}
  begin
    textStartX:=F_Indent*LINE_DISTANCE+F_TreeListView.TreeColumnIndentation;
    drawSearchMark:=TreeListView.f_searchMarkVisible and (self=TreeListView.f_searchMarkItem) and (TreeListView.f_searchMarkCol=0);
    imageX:=textStartX+rec.left+LEFT_TEXT_PADDING;
    if ImageBitmap<>nil then begin

      if imageX+ImageBitmap.Width<=rec.Right then
        F_TreeListView.Canvas.Draw(imageX,rec.top,ImageBitmap)
       else if imageX<rec.right then
        F_TreeListView.Canvas.CopyRect(rect(imageX,rec.top,rec.Right,rec.top+ImageBitmap.Height),ImageBitmap.Canvas,rect(0,0,rec.right-imageX,ImageBitmap.Height));
      F_TreeListView.drawTextRect(Text,textStartX+ImageBitmap.Width+LEFT_TEXT_PADDING,taLeftJustify, rec, drawSearchMark);
    end else if (F_TreeListView.Images<>nil) and (ImageIndex>-1) then begin

      if imageX+F_TreeListView.Images.Width<=rec.Right then
         F_TreeListView.Images.draw(F_TreeListView.Canvas,imageX,rec.Top,ImageIndex)
       else if imageX<rec.right then begin
         {$ifdef useRealClipping}
         oldClippingRect:=F_TreeListView.Canvas.ClipRect;
         oldClipping:=F_TreeListView.Canvas.Clipping;
         F_TreeListView.Canvas.Clipping:=true;
         F_TreeListView.Canvas.ClipRect:=rect(rec.left,0,rec.Right, rec.top+F_TreeListView.Images.Height);
         F_TreeListView.Images.draw(F_TreeListView.Canvas,imageX,rec.Top,ImageIndex);
         F_TreeListView.Canvas.ClipRect:=oldClippingRect;
         F_TreeListView.Canvas.Clipping:=oldClipping;
         {$else}
         tempBitmap:=graphics.TBitmap.create;
         try
           F_TreeListView.Images.GetBitmap(ImageIndex,tempBitmap);
           F_TreeListView.Canvas.CopyRect(rect(imageX,rec.top,rec.Right,rec.top+tempBitmap.Height),tempBitmap.Canvas,rect(0,0,rec.right-imageX,tempBitmap.Height));
         finally
           tempBitmap.free;
         end;
         {$endif}
       end;
      TreeListView.drawTextRect(Text,textStartX+F_TreeListView.Images.Width+LEFT_TEXT_PADDING,taLeftJustify,rec,drawSearchMark);
    end else

      TreeListView.drawTextRect(Text,textStartX,taLeftJustify,rec,drawSearchMark);
  end;

  procedure drawTreeColumn;
  var i,tempX:longint;
      tempColor:TColor;
  begin
    tempX:=rec.left;
    if F_TreeListView.RootLineMode <> lmNone then begin
      tempX:=tempX+LINE_DISTANCE div 2+1;

      for i:=0 to hierarchyStack.size-1 do begin
        if tempx>rec.right then break;
        if hierarchyStack.stack[i].index<>hierarchyStack.stack[i].list.Count-1 then
          if F_TreeListView.RootLineMode=lmDot then
            F_TreeListView.DrawAlignDotLine(tempX,yold,tempX,ynew-1,F_TreeListView.F_RootLineColor)
           else with F_TreeListView.canvas do begin
             pen.color:=F_TreeListView.F_RootLineColor;
             pen.Style:=psSolid;
             MoveTo(tempX,yold);
             LineTo(tempX,ynew);
           end;
        tempX:=tempX+LINE_DISTANCE;
      end;

      tempX:=tempX-LINE_DISTANCE div 2-3;

      case F_TreeListView.RootLineMode of
        lmDot:  begin
          F_TreeListView.DrawAlignDotLine(tempX-5,yold+F_TreeListView.RowHeight div 2,min(tempX,rec.right),yold + F_TreeListView.RowHeight div 2,F_TreeListView.F_RootLineColor);
          F_TreeListView.DrawAlignDotLine(tempX-5,yold,tempX-5,yold+F_TreeListView.RowHeight div 2,F_TreeListView.F_RootLineColor);
        end;
        lmSolid: with F_TreeListView.canvas do begin
           pen.color:=F_TreeListView.F_RootLineColor;
           pen.Style:=psSolid;
           MoveTo(tempX-5,yold+F_TreeListView.RowHeight div 2);
           LineTo(min(tempX,rec.right),yold + F_TreeListView.RowHeight div 2);
             MoveTo(tempX-5,yold);
             LineTo(tempX-5,yold+F_TreeListView.RowHeight div 2);
         end;
      end;
    end;

    if not defaultDraw then exit;

    tempX:=GetExtendingButtonPos;
    if (yold>F_TreeListView.F_VScroll.Top)
       and(tempX+9<=rec.right)
       and(SubItems.Count>0)  then
       begin
      tempColor:=F_TreeListView.Canvas.brush.Color;
      F_TreeListView.Canvas.pen.Style:=psSolid;
      F_TreeListView.Canvas.pen.Color:=clBlack;
      F_TreeListView.Canvas.brush.Style:=bsSolid;
      F_TreeListView.Canvas.brush.Color:=F_TreeListView.ButtonColor;

      F_TreeListView.canvas.Rectangle(tempX,yold+F_TreeListView.RowHeight div 2-5,tempX+9,yold+F_TreeListView.RowHeight div 2+4);
      F_TreeListView.canvas.moveTo(tempX+2,yold+F_TreeListView.RowHeight div 2-1);
      F_TreeListView.canvas.LineTo(tempX+7,yold+F_TreeListView.RowHeight div 2-1);
      if not Expanded then
      begin
        F_TreeListView.canvas.moveTo(tempX+4,yold+F_TreeListView.RowHeight div 2-3);
        F_TreeListView.canvas.LineTo(tempX+4,yold+F_TreeListView.RowHeight div 2+2);
      end;
      F_TreeListView.Canvas.brush.Color:=tempColor;
    end;
  end;

begin
  if SeemsSelected then
  begin
    F_TreeListView.Canvas.Brush.color:=F_TreeListView.SelectBackColor;
    F_TreeListView.Canvas.Brush.style:=bsSolid;
  end else if tlvoStriped in F_TreeListView.F_Options then
  begin
    F_TreeListView.canvas.Brush.Style:=bsSolid;
    if F_TreeListView.F_DrawingEvenItem then F_TreeListView.canvas.Brush.Color:=F_TreeListView.StripedEvenColor
    else F_TreeListView.canvas.Brush.Color:=F_TreeListView.StripedOddColor;
  end else F_TreeListView.Canvas.Brush.style:=bsClear;
  defaultDraw:=F_TreeListView.DoCustomItemDrawEvent(cdetPrePaint,self);
  yold:=F_TreeListView.DrawingYPos;
  ynew:=yold+F_TreeListView.RowHeight;

  if defaultDraw then
    if ynew>F_TreeListView.F_VScroll.Top then
      begin

      F_TreeListView.Canvas.FillRect(rect(0,yold,F_TreeListView.doubleBuffer.width,ynew));

      rec.top:=yold;
      rec.Bottom:=ynew;

      for i:=0 to F_TreeListView.F_Header.Sections.Count-1 do begin
        {$IFDEF allowHeaderDragging}
          recordId:=F_TreeListView.F_Header.Sections[i].OriginalIndex;
        {$ELSE}
          recordId:=i;
        {$ENDIF}
        if recordId>RecordItems.Count-1 then continue;
        rec.left:=F_TreeListView.F_Header.Sections[i].Left-F_TreeListView.F_HScroll.Position;
        rec.right:=rec.left+F_TreeListView.F_Header.Sections[i].Width;
        if assigned(F_TreeListView.F_CustomRecordItemPositioningEvent) then
          begin
          rec.top:=yold;
          rec.Bottom:=ynew;
          F_TreeListView.F_CustomRecordItemPositioningEvent(F_TreeListView, i, RecordItems[recordId], rec);
        end;
        RecordItems[recordId].selectFont(F_TreeListView.Canvas);
        if F_TreeListView.DoCustomRecordItemDrawEvent(cdetPrePaint,RecordItems[recordId],rec) then
          begin
          if recordId=0 then
            begin
            drawTreeColumnText;
            drawTreeColumn;
          end else
            F_TreeListView.drawTextRect(RecordItems[recordId].Text,0,F_TreeListView.F_Header.Sections[i].Alignment,rec,TreeListView.f_searchMarkVisible and (self=TreeListView.f_searchMarkItem) and (TreeListView.f_searchMarkCol=recordId));
          if not F_TreeListView.DoCustomRecordItemDrawEvent(cdetPostPaint,RecordItems[recordId],rec) then
            break;
        end;
      end;

      if F_TreeListView.focused = self then
        DrawFocusRect( F_TreeListView.Canvas.Handle,rect(0,yold,F_TreeListView.F_VScroll.Left,ynew));

      case F_TreeListView.HorizontalLineMode of
        lmSolid: with F_TreeListView.canvas do
                 begin
                   pen.color:=F_TreeListView.F_HorizontalLineColor;
                   pen.Style:=psSolid;
                   MoveTo(0,ynew-1);
                   LineTo(F_TreeListView.doubleBuffer.Width,ynew-1);
                 end;
        lmDot:   F_TreeListView.DrawAlignDotLine(0,ynew-1,F_TreeListView.doubleBuffer.Width,ynew-1,F_TreeListView.F_HorizontalLineColor);
      end;
    end;

  F_TreeListView.DoCustomItemDrawEvent(cdetPostPaint,self)
end;

destructor TplTreeListItem.Destroy;
begin
  TreeListView.BeginMultipleUpdate;
  if self=TreeListView.focused then TreeListView.focused:=nil;
  if self=TreeListView.F_TopItem then TreeListView.F_TopItem:=nil;
  if Selected then dec(TreeListView.f_selCount);
  F_RecordItems.onListEvent := nil; 
  F_RecordItems.free;
  F_SubItems.onListEvent := nil;    
  F_SubItems.free;
  TreeListView.EndMultipleUpdate;
  inherited;
end;

//======================== TplTreeListView ===========================

constructor TplTreeListView.Create(aowner:TComponent);
var temp:tbitmap;
begin
  inherited;

  F_Items:=TplTreeListItems.Create(nil,self);
  F_InvalidatedItems:=tlist.create;
  f_invalidateAll:=true;

  F_Options:=[tlvoToolTips,tlvoRightMouseSelects,tlvoStriped];
  ControlStyle:=ControlStyle+[csClickEvents,csFramed,csOpaque,csDoubleClicks];

  doubleBuffer:=graphics.TBitmap.Create;


  F_HotTrackFont:=TFont.Create;
  F_HotTrackFont.Color:=clBlue;
  F_HotTrackFont.Style:=[fsUnderline];

  F_SelectedHotTrackFont:=TFont.Create;
  F_SelectedHotTrackFont.Color:=clHighlightText;
  F_SelectedHotTrackFont.Style:=[fsBold,fsUnderline];

  F_SelectedFont:=TFont.Create;
  F_SelectedFont.Color:=clHighlightText;
  F_SelectedFont.Style:=[];

  Font.Color:=clWindowText;
  Font.Style:=[];

  F_SelectBackColor:=clHighlight;
  F_BgColor:=clWindow;
  F_ButtonColor:=clWindow;

  F_StripedOddColor:=clWindow;
  F_StripedEvenColor:= $00E0FFFF;

  colorSearchMark:=clAqua;
  colorSearchMarkField:=(clBlue + clAqua) div 2;
  f_searchMarkItem:=nil;
  f_searchMarkVisible:=false;

  F_RootLines:=lmDot;
  F_RootLineColor:=clWindowFrame;
  F_HorizontalLines:=lmNone;
  F_HorizontalLineColor:=clWindowFrame;
  F_VerticalLines:=lmDot;
  F_VerticalLineColor:=clWindowFrame;

  InternOptions_tlio:=[];
  self.SetInitialBounds(0,0,199,199);

  TabStop:=true;

  F_Header:=THeaderControl.Create(self);
  F_Header.Align:=alNone;
  F_Header.Visible:=true;
  F_HeaderVisible:=true;
  with F_Header.Sections.Add do
 begin
    Caption:='';
    Width:=1000;
  end;
  F_Header.Left:=0;
  F_Header.Width:=9999;

  if font.Height=0 then
  begin
    temp:=tbitmap.create;
    temp.canvas.font.Assign(font);
    temp.canvas.TextOut(0,0,'a');
    F_Header.Height:=temp.canvas.TextHeight('ABC,')+2*GetSystemMetrics(SM_CYEDGE);
    temp.free;

   end else
    F_Header.Height:=abs(font.height)+2*GetSystemMetrics(SM_CYEDGE);

  F_Header.OnSectionTrack:=_HeaderSectionTrack;
  F_Header.OnSectionResize:=_HeaderSectionResize;
  F_Header.OnSectionClick:=_HeaderSectionClick;
  {$ifdef allowHeaderDragging}
  F_Header.OnSectionSeparatorDblClick:=_HeaderSectionDblClick();
  F_Header.OnSectionEndDrag:=_HeaderSectionEndDrag;
  {$endif}
  F_Header.parent:=Self;

  F_ScrollStyle:=ssBoth;

  //Scrollbar initialisieren
  F_VScroll:=TScrollbar.Create(self);
  F_VScroll.Enabled:=false;
  F_VScroll.Visible:=true;
  F_VScroll.Kind:=sbVertical;
  F_VScroll.OnChange:=_VScrollChange;
  F_VScroll.TabStop:=false;
  F_VScroll.parent:=self;


  //Scrollbar initialisieren
  F_HScroll:=TScrollbar.Create(self);
  F_HScroll.Enabled:=false;
  F_HScroll.Visible:=true;
  F_HScroll.Kind:=sbHorizontal;
  F_HScroll.Left:=0;
  F_HScroll.SmallChange:=5;
  F_HScroll.OnChange:=_HScrollChange;
  F_HScroll.TabStop:=false;
  F_HScroll.parent:=self;

  RowHeight:=F_Header.Height-2*GetSystemMetrics(SM_CYEDGE);
  if font.Height>RowHeight then RowHeight:=font.Height+1;
    {
  if csDesigning in ComponentState then begin
    BeginUpdate;
    with items.Add('Constant example tree') do begin
      SubItems.Add('1').SubItems.add('1.1');
      SubItems.Add('2').RecordItemsText[1]:='record item';
      SubItems.Add('3');
    end;
    items.Add('More...').SubItems.Add('More...').SubItems.Add('More...');
    EndUpdate;
  end;     }

  {$ifdef android}
  F_PostMessageTimer := TTimer.Create(self);
  F_PostMessageTimer.Interval:=25;
  F_PostMessageTimer.Enabled:=false;
  F_PostMessageTimer.OnTimer:=PostMessageTimerTimer;

  F_ScrollStyle := ssNone;
  F_VScroll.Visible := false;
  F_HScroll.Visible := false;
  F_HeaderVisible := false;
  F_Header.Visible := false;
  F_Options := F_Options + [tlvoDragScrolling];
  {$endif}

end;


procedure TplTreeListView.loaded;
begin
  inherited loaded;
  UpdateScrollBarPos;

end;

procedure TplTreeListView.SetFocused(const AValue: TplTreeListItem);
begin
  if AValue=F_Focused then exit;
  if tlioDeleting in InternOptions_tlio then exit;
  invalidateItem(F_Focused);
  invalidateItem(AValue);
  F_Focused:=AValue;
  DoSelect(F_Focused);
  if focused<>nil then
    ensureVisibility(focused);
  if f_searchMarkVisible then begin
    f_searchMarkVisible:=false;
    invalidateAll();
  end;
  
  internPaint;
end;

procedure TplTreeListView.SetSelected(const AValue: TplTreeListItem);
begin
  if AValue = Selected then exit;
  BeginMultipleUpdate;
  removeSelection(items);
  if Avalue<>nil then
    AValue.Selected:=true;
  SetFocused(AValue);
  EndMultipleUpdate;
end;

procedure TplTreeListView.setTopItem(item: TplTreeListItem);
begin
//todo
end;

function TplTreeListView.GetTopItem: TplTreeListItem;
begin
  if (F_TopItem=nil) then
  begin
    F_TopItem:=Items.GetItemWithRealIndex(F_VScroll.Position,[]);
    if tlvoStripInvisibleItems in f_options then
      F_TopItemEven:=Items.RealIndexOf(F_TopItem,[ricCountCollapsedsubItems]) mod 2=0;
  end;
  result:=F_TopItem;
end;

function TplTreeListView.GetTopItemEven: boolean;
begin
  if tlvoStripInvisibleItems in f_options then begin
    GetTopItem;
    result:=F_TopItemEven;
  end else result:=F_VScroll.Position mod 2 = 0;
end;

procedure TplTreeListView.SetSortColumn(const AValue: longint);
begin
  if SortColumn=AValue then exit;
  F_SortColumn:=AValue;
  F_SortColumnInverted:=false;
  if tlvoSorted in F_Options then sort;
end;

{$ifdef allowHeaderVisible}
procedure TplTreeListView.ColumnPopupMenuClick(Sender: TObject);
var mi: TMenuItem;
begin
  if not (sender is tmenuitem) then exit;
  mi:=TMenuItem(sender);
  if (mi.tag<0) or (mi.tag>=Columns.Count) then exit;
  mi.Checked:=not mi.Checked;
  Columns[mi.Tag].visible:=mi.Checked;
  internRepaint;
end;
{$endif}

procedure TplTreeListView.SearchBarKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var temp:TLMKeyDown;
begin
  case key of
    VK_DOWN,VK_UP,VK_NEXT,VK_PRIOR: begin
      temp.msg:=LM_KEYDOWN;
      temp.CharCode:=key;
      temp.KeyData:=0;
      WndProc(TLMessage(temp));
      key:=0;
    end;
  end;
end;

procedure TplTreeListView.F_SearchBarHighlightChanged(Sender: TObject);
begin
  F_HighlightAll:=F_SearchBar.Highlighting;
  sheduleInternRepaint;
end;

procedure TplTreeListView.SearchBarClose(Sender: TObject);
begin
  UpdateScrollBarPos;
  UpdateScrollSizeV;
  SetFocus;
end;

procedure TplTreeListView.SetOptions(const AValue: TplTreeListViewOptions);
var toAdd,toRemove: TplTreeListViewOptions;
    needRepaint: boolean;
begin
  if F_Options=AValue then exit;
  toRemove:=F_Options-AValue;
  toAdd:=AValue-F_Options;
  F_Options:=AValue;
  needRepaint:=false;

  if tlvoMultiSelect in toRemove then Selected:=focused;


  if (tlvoHotTrackRecordTextItems in toRemove) and (hotTrackedRecordItem<>nil) then
  begin
    needRepaint:=true;
    hotTrackedRecordItem:=nil;
  end;

  if tlvoStripInvisibleItems in toRemove then needRepaint:=true
  else if tlvoStripInvisibleItems in toAdd then
  begin
    needRepaint:=true;
    F_TopItem:=nil;
    GetTopItem;
  end;

  if not needRepaint then
    needRepaint:=tlvoStriped in (toRemove+toAdd);

  {$ifdef allowHeaderDragging}
  if tlvoColumnsDragable in toRemove then F_Header.DragReorder:=false
  else if tlvoColumnsDragable in toAdd then F_Header.DragReorder:=true;
  {$endif}

  if tlvoSorted in toAdd then sort;

  if needRepaint then internRepaint;
end;

procedure TplTreeListView.SetOption(const Option: TplTreeListViewOption;
  const active: boolean);
begin
  if active then SetOptions(F_Options+[Option])
  else SetOptions(F_Options-[Option]);
end;

procedure TplTreeListView.SearchBarShow(Sender: TObject);
begin
  UpdateScrollBarPos;
  UpdateScrollSizeV;
  F_SearchBar.SetFocus;
end;


procedure TplTreeListView.SetBgColor(const AValue: TColor);
begin
  if F_BgColor=AValue then exit;
  F_BgColor:=AValue;
  sheduleInternRepaint();
end;

function TplTreeListView.GetTopItemVisualIndex: integer;
begin
  result := F_VScroll.Position;
end;

procedure TplTreeListView.SetButtonColor(const AValue: TColor);
begin
  if F_ButtonColor=AValue then exit;
  F_ButtonColor:=AValue;
  sheduleInternRepaint();
end;

procedure TplTreeListView.SetColorSearchMark(const AValue: tcolor);
begin
  if f_colorSearchMark=AValue then exit;
  f_colorSearchMark:=AValue;
  sheduleInternRepaint();
end;

procedure TplTreeListView.SetColorSearchMarkField(const AValue: tcolor);
begin
  if f_colorSearchMarkField=AValue then exit;
  f_colorSearchMarkField:=AValue;
  sheduleInternRepaint();
end;

procedure TplTreeListView.SetExpandMode(const AValue: TExpandMode);
begin
  if F_ExpandMode=AValue then exit;
  F_ExpandMode:=AValue;
  sheduleInternRepaint();
end;

procedure TplTreeListView.SetHeaderVisible(AValue: boolean);
begin
  if F_HeaderVisible=AValue then Exit;
  F_HeaderVisible := AValue;
  F_Header.Visible := F_HeaderVisible;
  UpdateScrollBarPos;
  UpdateScrollSize;
  sheduleInternRepaint();
end;

procedure TplTreeListView.SetHorizontalLineColor(const AValue: TColor);
begin
  if F_HorizontalLineColor=AValue then exit;
  F_HorizontalLineColor:=AValue;
  sheduleInternRepaint();
end;

procedure TplTreeListView.SetHorizontalLines(const AValue: TLineMode);
begin
  if F_HorizontalLines=AValue then exit;
  F_HorizontalLines:=AValue;
  sheduleInternRepaint();
end;

procedure TplTreeListView.SetRootLineColor(const AValue: TColor);
begin
  if F_RootLineColor=AValue then exit;
  F_RootLineColor:=AValue;
  sheduleInternRepaint();
end;

procedure TplTreeListView.SetRootLines(const AValue: TLineMode);
begin
  if F_RootLines=AValue then exit;
  F_RootLines:=AValue;
  sheduleInternRepaint();
end;

procedure TplTreeListView.SetScrollStyle(AValue: TScrollStyle);
begin
  if F_ScrollStyle=AValue then Exit;
  F_ScrollStyle:=AValue;
  UpdateScrollBarPos;
  UpdateScrollSize;
  sheduleInternRepaint();
end;

procedure TplTreeListView.SetSelectBackColor(const AValue: TColor);
begin
  if F_SelectBackColor=AValue then exit;
  F_SelectBackColor:=AValue;
  sheduleInternRepaint();
end;

procedure TplTreeListView.SetStripedEvenColor(const AValue: TColor);
begin
  if F_StripedEvenColor=AValue then exit;
  F_StripedEvenColor:=AValue;
  sheduleInternRepaint();
end;

procedure TplTreeListView.SetStripedOddColor(const AValue: TColor);
begin
  if F_StripedOddColor=AValue then exit;
  F_StripedOddColor:=AValue;
  sheduleInternRepaint();
end;

procedure TplTreeListView.SetVerticalLineColor(const AValue: TColor);
begin
  if F_VerticalLineColor=AValue then exit;
  F_VerticalLineColor:=AValue;
  sheduleInternRepaint();
end;

procedure TplTreeListView.SetVerticalLines(const AValue: TLineMode);
begin
  if F_VerticalLines=AValue then exit;
  F_VerticalLines:=AValue;
  sheduleInternRepaint();
end;

procedure TplTreeListView.SearchBarSearch(sender: TObject; incremental,
  backwards: boolean);
var searchLocations: TStrings;
    searchOptions: cardinal;
begin
  if sender<>F_SearchBar then exit;
  if F_SearchBar.SearchLocation<0 then F_SearchBar.SearchLocation:=0;
  searchLocations:=F_SearchBar.SearchLocations;
  if F_SearchBar.SearchLocation>=searchLocations.count then
    exit;
  searchOptions:=cardinal(searchLocations.Objects[F_SearchBar.SearchLocation]);
  F_NewSearchBarFindState:=search(F_SearchBar.SearchText,searchOptions,backwards,incremental);
end;

function TplTreeListView.DoCustomBackgroundDrawEvent (eventTyp_cdet:TCustomDrawEventTyp):boolean;
begin
  Result:=true;
  if assigned(F_CustomBgDraw) then F_CustomBgDraw(self,eventTyp_cdet,result);
end;
function TplTreeListView.DoCustomItemDrawEvent(const eventTyp_cdet:TCustomDrawEventTyp;const item:TplTreeListItem):boolean;
begin
  Result:=true;
  if assigned(F_CustomItemDraw) then F_CustomItemDraw(self,eventTyp_cdet,item,result);
end;
function TplTreeListView.DoCustomRecordItemDrawEvent(const eventTyp_cdet:TCustomDrawEventTyp;const RecordItem:TplTreeListRecordItem;const outrec: TRect):boolean;
begin
  Result:=true;
  if assigned(F_CustomRecordItemDraw) then begin
    F_DrawingRecordItemRect:=outrec;
    F_CustomRecordItemDraw(self,eventTyp_cdet,recordItem,result);
  end;
end;

procedure TplTreeListView.removeSelection(list: TplTreeListItems);
var i:longint;
begin
  BeginMultipleUpdate;
  for i:=0 to list.count-1 do begin
    list[i].Selected:=false;
    if list[i].SubItems.count>0 then removeSelection(list[i].SubItems);
  end;
  EndMultipleUpdate;
end;

procedure TplTreeListView.removeMouseSelection(list: TplTreeListItems);
var i:longint;
begin
  for i:=0 to list.count-1 do begin
    list[i].SetSelections(list[i].Selected, false);
    removeMouseSelection(list[i].SubItems);
  end;
end;

procedure TplTreeListView.setMouseSelection(list: TplTreeListItems);
var i:longint;
begin
  BeginMultipleUpdate;
  for i:=0 to list.count-1 do begin
    list[i].SetSelections(list[i].Selected xor list[i].MouseSelected,false);
    setMouseSelection(list[i].SubItems);
  end;
  EndMultipleUpdate;
end;


procedure TplTreeListView.DoSelect(item: TplTreeListItem);
begin
  if assigned(F_OnSelect) and (item<>nil) then
    F_OnSelect(self,item);
end;

procedure TplTreeListView.SetItems(const value:TplTreeListItems);
begin
  F_Items.Assign(value);
end;

function TplTreeListView.GetTopPos:integer;
begin
  result:=HeaderItemDistance+F_VScroll.Top-F_VScroll.Position*RowHeight;
end;

procedure TplTreeListView.SetColumns(const value:THeaderSections);
begin
  F_Header.Sections.Assign(value);
end;
function TplTreeListView.GetColumns:THeaderSections;
begin
  Result:=F_Header.Sections;
end;

function TplTreeListView.GetItemAtPos(const y:integer):TplTreeListItem;
var i,startY:integer;
begin
  startY:=TopPos;
  result:=nil;
  for i:=0 to Items.Count-1 do begin
    result:=(TObject(Items[i]) as TplTreeListItem).GetItemAtPos(self,y,startY);
    if result<>nil then exit;
  end;
end;

function TplTreeListView.GetRecordItemAtPos(const x,y:integer):TplTreeListRecordItem;
var item:TplTreeListItem;
begin
  result:=nil;
  item:=getItemAtPos(y);
  if item<>nil then
   result:= item.GetRecordItemAtPos(self,x);

end;


procedure TplTreeListView.SetHotTrackFont(const value:TFont);
begin
  F_HotTrackFont.Assign(value);
end;
procedure TplTreeListView.SetSelectedFont(const value:TFont);
begin
  F_SelectedFont.Assign(value);
end;
procedure TplTreeListView.SetSelectedHotTrackFont(const value:TFont);
begin
  F_SelectedHotTrackFont.Assign(value);
end;

procedure TplTreeListView.BeginUpdate;
begin
  include(InternOptions_tlio,tlioUpdating)
end;

procedure TplTreeListView.EndUpdate;
begin
  exclude(InternOptions_tlio,tlioUpdating);
  updateAll;
  if tlvoSorted in F_Options then sort;
end;

function TplTreeListView.VisibleRowCount:longint;
begin
  if RowHeight=0 then result:=0
  else  result:=RealClientHeight div RowHeight;
end;

procedure TplTreeListView.sort;
begin
  BeginMultipleUpdate;
  Items.Sort(CompareItems);
  invalidateAll();
  EndMultipleUpdate;
  if assigned(F_OnItemsSorted) then F_OnItemsSorted(self);
end;

procedure TplTreeListView.ensureVisibility(item: TplTreeListItem;column: longint);
var rindex:longint;
    temp: TplTreeListItem;
    rl,np: longint;
begin
  temp:=item.Parent;
  while temp<>nil do begin
    if not temp.Expanded then temp.Expand;
    temp:=temp.Parent;
  end;
  rindex:=Items.RealIndexOf(item,[]);
  if rindex<F_VScroll.Position then F_VScroll.Position:=rindex
  else if rindex>F_VScroll.Position+VisibleRowCount-1 then F_VScroll.Position:=rindex-VisibleRowCount+1;
  if column<>-1 then begin
    rl:=ColumnFromOriginalIndex(column).Left;
    np:=F_HScroll.Position;
    if rl+ ColumnFromOriginalIndex(column).Width - np>ClientWidth then
      np:=rl+ ColumnFromOriginalIndex(column).Width - ClientWidth; //move right
    if rl-np<0 then np:=rl; //move left (can revert right moving)
    F_HScroll.Position:=np;
  end;
end;

function TplTreeListView.search(searchFor: string; searchFields: cardinal;
  backward: boolean; extendSelection: boolean): TFindState;
var startItem: TplTreeListItem;
    found: TplTreeListRecordItem;
    loopAround: boolean;

    i,column,textPos: longint;
begin
 if  f_searchActivated then exit;
 f_searchActivated:=true;
 try
  result:=[];
  if f_searchMarkVisible and (f_searchMarkItem<>nil) then startItem:=f_searchMarkItem
  else if F_Focused<>nil then startItem:=F_Focused
  else if Items.count>0 then startItem:=items[0]
  else exit;

  if not extendSelection then
    if not backward then begin
      if startItem<>Items[items.count-1].GetLastSubSubItem then startItem:=startItem.GetNextItem()
      else startItem:=Items[0];
    end else begin
      if startItem<>items[0] then startItem:=startItem.GetPrevItem()
      else startItem:=Items[items.count-1];
    end;

  //only search existing/visible columns
  for i:=0 to 31 do
    if (ColumnFromOriginalIndex(i)=nil)
       {$ifdef allowHeaderVisible}or not ColumnFromOriginalIndex(i).Visible{$endif} then
      searchFields:=searchFields and not (1 shl i);

  if extendSelection then
    found:=items.find(searchFor,searchFields,backward,@loopAround, startItem, f_searchMarkCol,f_searchMarkStart)
   else
    found:=items.find(searchFor,searchFields,backward,@loopAround, startItem, 0, 0);

  Result:=[];
  if loopAround then include(Result,fsLoopAround);

  if found=nil then begin
    if f_searchMarkVisible or F_HighlightAll then begin
      sheduleInternRepaint;
      f_searchMarkVisible:=false;
    end;
  end else begin
    include(Result,fsFound);
    column:=found.Index;
    textPos:=pos(lowercase(searchFor),lowercase(found.text));

    if selCount<=1 then selected:=found.F_Parent;
    f_searchMarkItem:=found.F_Parent;
    f_searchMarkCol:=column;
    f_searchMarkStart:=textPos;
    f_searchMarkLen:=length(searchFor);
    f_searchMarkVisible:=true;
    ensureVisibility(f_searchMarkItem,f_searchMarkCol);

    sheduleInternRepaint();

   end;
 finally
   f_searchActivated:=false;
 end;
end;

function TplTreeListView.ColumnFromOriginalIndex(index: longint): THeaderSection;
{$IFDEF allowHeaderDragging}var i:longint;{$endif}
begin
  result:=nil;
  {$IFDEF allowHeaderDragging}
    for i:=0 to F_Header.Sections.count-1 do
      if F_Header.Sections[i].OriginalIndex = index then
        exit(F_Header.Sections[i]);
  {$ELSE}
    if index <Columns.count then
      result:=Columns[index];
  {$ENDIF}
end;

procedure TplTreeListView.CreateUserColumnVisibilityPopupMenu();
{$ifdef allowHeaderVisible}
var mi: TMenuItem;
    i:longint;
{$endif}
begin
  {$ifdef allowHeaderVisible}
  if F_HeaderColumnPopupMenu=nil then
    F_HeaderColumnPopupMenu:=TPopupMenu.Create(self);
  if F_Header.PopupMenu<>F_HeaderColumnPopupMenu then F_Header.PopupMenu:=F_HeaderColumnPopupMenu;
  F_HeaderColumnPopupMenu.Items.Clear;
  for i:=0 to Columns.count-1 do begin
    mi:=TMenuItem.Create(F_HeaderColumnPopupMenu);
    mi.OnClick:=ColumnPopupMenuClick;
    mi.Caption:=Columns[i].Text;
    mi.Checked:=Columns[i].visible;
    mi.tag:=i;
    F_HeaderColumnPopupMenu.Items.Add(mi);
  end;
  {$endif}
end;

function TplTreeListView.serializeColumnWidths: string;
var i:longint;
    {$ifdef allowHeaderVisible}
    vis:boolean;
    {$endif}
    sec:THeaderSection;
begin
  Result:='';
  for i:=0 to F_Header.Sections.Count-1 do begin
    sec:=ColumnFromOriginalIndex(i);
    {$ifdef allowHeaderVisible}
      vis:=sec.Visible;
      if not vis then
        sec.Visible :=true;
    {$endif}
    result:=result+IntToStr(sec.Width)+',';
    {$ifdef allowHeaderVisible}
    if not vis then sec.Visible :=false;
    {$endif}
  end;
end;

function TplTreeListView.serializeColumnOrder: string;
{$IFDEF allowHeaderDragging}
var i:longint;
{$endif}
begin
{$ifdef allowHeaderDragging}
  result:='';
  for i:=0 to F_Header.Sections.Count-1 do
    result:=result+ IntToStr(ColumnFromOriginalIndex(i).Index)+',';
{$endif}
end;

function TplTreeListView.serializeColumnVisibility: string;
var i:longint;
begin
  result:='';
  for i:=0 to F_Header.Sections.count-1 do
    if not ColumnFromOriginalIndex(i).Visible then
      result+='-' else
      result:=result+'+';
end;

procedure TplTreeListView.deserializeColumnWidths(s: string);
var i:longint;
    sep: longint;
begin
  i:=0;
  while i<F_Header.Sections.Count do begin
    sep:=pos(',',s);
    if (sep=0) then break;
    ColumnFromOriginalIndex(i).Width:=StrToInt(trim(copy(s,1,sep-1)));
    delete(s,1,sep);
    inc(i);
  end;
end;

procedure TplTreeListView.deserializeColumnOrder(s: string);
{$ifdef allowHeaderDragging}
var i:longint;
    sep: longint;
    tempOrder: array[0..20] of longint;
{$endif}
begin
{$ifdef allowHeaderDragging}
  for i:=0 to high(tempOrder) do
    tempOrder[i]:=i;
  i:=0;
  while i<F_Header.Sections.Count do begin
    sep:=pos(',',s);
    if (sep=0) then break;
    tempOrder[i]:=StrToInt(trim(copy(s,1,sep-1)));
    delete(s,1,sep);
    inc(i);
  end;
  for i:=0 to F_Header.Sections.count-1 do
    ColumnFromOriginalIndex(tempOrder[i]).Index:=i;
{$endif}
end;

procedure TplTreeListView.deserializeColumnVisibility(s: string);
{$ifdef allowHeaderVisible}
var i:longint;
{$endif}
begin
{$ifdef allowHeaderVisible}
  for i:=0 to min(length(s)-1,F_Header.Sections.Count-1) do
    ColumnFromOriginalIndex(i).Visible:=s[i+1]='+';
{$endif}
end;

procedure TplTreeListView.CreateSearchBar();
var i:longint;
begin
  if F_SearchBar = nil then begin
    F_SearchBar:=TplSearchPanel.Create(self);
    F_SearchBar.Parent:=self;
    F_SearchBar.OnSearch:=SearchBarSearch;
    F_SearchBar.OnKeyDown:=SearchBarKeyDown;
    F_SearchBar.OnHighlightChanged:=F_SearchBarHighlightChanged;
    F_SearchBar.OnClose:=SearchBarClose;
    F_SearchBar.OnShow:=SearchBarShow;
  end;
  F_NewSearchBarFindState := SearchBar.FindState;
  F_SearchBar.SubComponents:=[fscCloseButton, fscLabelText, fscSelectLocation,
                              fscSearchForward, fscSearchBackwards, fscHighlight, fscStatus];
  F_SearchBar.SearchLocations.Clear;
  F_SearchBar.SearchLocations.AddObject('all',tobject(-1));
  for i:=0 to Columns.Count-1 do
    F_SearchBar.SearchLocations.AddObject(ColumnFromOriginalIndex(i).Text,tobject(1 shl i));
  F_SearchBar.SearchLocation:=0;

  UpdateScrollBarPos;
end;

procedure TplTreeListView.setImageList(const images:TImageList);
begin
  F_ImageList:=images;
end;

procedure TplTreeListView.SetRowHeight(const newHeight:integer);
begin
  if newHeight and $1=$1 then F_RowHeight:=newHeight+1
  else F_RowHeight:=newHeight;
end;


function TplTreeListView.RealControlHeight(c: Twincontrol): longint;
var r:TRect;
begin
  if not c.IsVisible then exit(0);
  {$ifdef android}
  exit(c.Height);
  {$endif}
  GetWindowRect(c.Handle,r);
  result:=r.bottom-r.top;
end;

function TplTreeListView.RealBaseClientWidth: longint;
begin
  result := {$ifdef android}Width{$else}ClientWidth{$endif};
end;

function TplTreeListView.RealBaseClientHeight: longint;
begin
  result := {$ifdef android}Height{$else}ClientHeight{$endif};
end;

function TplTreeListView.RealClientHeight: longint;
begin
  result := RealBaseClientHeight - RealControlHeight(F_Header)-HeaderItemDistance;
  if F_HScroll.Visible then result := result - RealControlHeight(F_HScroll);
  if F_SearchBar <>nil then result := result - RealControlHeight(F_SearchBar);
end;

procedure TplTreeListView.DrawAlignDotLine(x,y:integer;const x2,y2:integer;const color:TColor);
var F_HeaderHeight:integer;
begin
  F_HeaderHeight:=F_VScroll.Top;
  if y2<F_HeaderHeight then exit;
  if y<F_HeaderHeight then y:=F_HeaderHeight;
  {$R-}
  if x=x2 then begin
    while (y<=y2) do begin
      canvas.Pixels[x,y]:=color;
      inc(y,2);
    end;
  end else begin
    while (x<=x2) do begin
      canvas.Pixels[x,y]:=color;
      inc(x,2);
    end;
  end;
end;

procedure TplTreeListView.drawTextRect(s: string; extraIndentation: longint;
  align:TAlignment;const rec: TRect; searchDraw: boolean);

var temp: TRect;
    flags: longint;

 procedure drawTextDef(s: string);
 begin
   SetBkMode(canvas.Handle, TRANSPARENT);
   DrawText(Canvas.Handle,{$IFNDEF CLR}pchar{$endif}(s),length(s), temp,flags);
 end;

var
    searched: string;
    stpos,i: longint;
    highlightText: boolean;
    parts: array of string;
    textStart: longint;
begin
  temp:=rec;
  temp.left:=temp.left+extraindentation;
  highlightText:=searchDraw or F_HighlightAll;
  stpos:=0;
  if highlightText then
    if F_SearchBar = nil then highlightText := false
    else begin
      searched:=lowercase(F_SearchBar.SearchText);
      stpos:=pos(searched,lowercase(s));
      highlightText:=stpos>0;
    end;
  if canvas.TextWidth(s)+LEFT_TEXT_PADDING>=rec.Right-rec.Left then
    align:=taLeftJustify; //justified because draw text always clips on the right side (TODO:??implement own clipping)
  flags:=DT_NOPREFIX or DT_SINGLELINE or DT_VCENTER or DT_END_ELLIPSIS;
  case align of
    taRightJustify:
      temp.Right:=temp.right-LEFT_TEXT_PADDING;
    taLeftJustify:
      temp.Left:=temp.left+LEFT_TEXT_PADDING;
  end;

  if (temp.Left >= temp.Right) or (temp.Right <= 0) then exit;

  if not highlightText then begin
    case align of
      taCenter: flags:=flags or DT_CENTER;
      taRightJustify: flags:=flags or DT_RIGHT;
      else flags:=flags or DT_LEFT;
    end;
    drawTextDef(s);
  end else begin
     case align of
       taCenter: textStart:=(temp.right+temp.left-canvas.TextWidth(s)) div 2;
       taRightJustify: textStart:=temp.Right-Canvas.TextWidth(s);
       else textStart:=temp.Left;
     end;

     //split
     SetLength(parts,0);
     repeat
       SetLength(parts,length(parts)+2);
       parts[high(parts)-1]:=copy(s,1,stpos-1);
       parts[high(parts)]:=copy(s,stpos,length(searched));
       delete(s,1,stpos+Length(searched)-1);
       stpos:=pos(searched,lowercase(s));
     until (stpos<=0) or searchDraw;  //search marks only one
     setlength(parts,length(parts)+1);
     parts[high(parts)]:=s;

     //draw background
     canvas.brush.style:=bsSolid;
     temp.right:=textStart;
     for i:=0 to high(parts) do begin
       temp.left:=temp.right;
       temp.right:=temp.left+canvas.TextWidth(parts[i]);
       if temp.right>rec.right then temp.Right:=rec.right
       else if temp.left>rec.right then break;
       if i and 1 = 0 then begin
         if searchDraw then begin
           canvas.brush.color:=ColorSearchMarkField;
           canvas.FillRect(temp);
         end;
       end else begin
         canvas.brush.color:=colorSearchMark;
         canvas.FillRect(temp);
       end;
     end;

     //draw text
     temp.left:=textStart;
     temp.right:=rec.right;
     canvas.brush.Style:=bsClear;
     for i:=0 to high(parts) do begin
       if temp.left>=rec.right then exit;
       drawTextDef(parts[i]);
       temp.left:=temp.left+canvas.TextWidth(parts[i]);
     end;
   end;

end;

function striicmp(s1,s2: string):longint;
var t1,t2:string;
    i,j,ib,jb,p: longint;
begin
  t1:=lowercase(s1);
  t2:=lowercase(s2);
  i:=1;
  j:=1;
  while (i<=length(t1)) and (j<=length(t2)) do begin
    if (t1[i] in ['0'..'9']) and (t2[j] in ['0'..'9']) then begin
      ib:=i;
      jb:=j;
      while (t1[i] in ['0'..'9']) and (i<=length(t1)) do inc(i);
      while (t2[j] in ['0'..'9']) and (j<=length(t2)) do inc(j);
      if i-ib<j-jb then begin
        result:=-1;
        exit;
      end;
      if i-ib>j-jb then begin
        result:=1;
        exit;
      end;
      for p:=0 to i-ib-1 do
        if t1[ib+p]<t2[jb+p] then begin
          result:=-1;
          exit;
        end else if t1[ib+p]>t2[jb+p] then begin
          result:=1;
          exit;
        end;
    end else begin
      if t1[i]<t2[j] then begin
        result:=-1;
        exit;
      end;
      if t1[i]>t2[j] then begin
        result:=1;
        exit;
      end;
      inc(i);
      inc(j);
    end;
  end;
  if length(t1)<length(t2) then begin
    result:=-1;
    exit;
  end;
  if length(t1)>length(t2) then begin
    result:=1;
    exit;
  end;
  result:=0;
end;

function TplTreeListView.CompareItems(i1, i2: TplTreeListItem): longint;
begin
  Result:=striicmp(i1.RecordItemsText[F_SortColumn],i2.RecordItemsText[F_SortColumn]);
  if assigned(F_OnCompareItems) then F_OnCompareItems(self,i1,i2,result);
  if F_SortColumnInverted then result:=-result;
end;

procedure TplTreeListView.BeginMultipleUpdate;
begin
  inc(f_RedrawBlock);
end;

procedure TplTreeListView.EndMultipleUpdate;
begin
  dec(f_RedrawBlock);
  if f_RedrawBlock=0 then updateAll;
end;

procedure TplTreeListView._SubItemListEvent(list: TObjectList; typ: TListEventTyp);
begin
  case typ of
    levBeginEdit: BeginMultipleUpdate;
    levEndEdit: EndMultipleUpdate;
    else sheduleInternRepaint();
  end;
end;

procedure TplTreeListView._RecordItemListEvent(list: TObjectList;
  typ: TListEventTyp);
begin
  case typ of
    levBeginEdit,levEndEdit: {ignore};
    else begin
      invalidateItem(TRecordItemList(list).Owner);
      internPaint;
    end;
  end;
end;

procedure TplTreeListView.updateAll();
begin
  if not HandleAllocated then exit;
  UpdateScrollBarPos;
  UpdateScrollSize;
  internPaint;
end;

procedure TplTreeListView._HeaderSectionTrack(HeaderControl: TEventHeaderControl; Section: THeaderSection; Width: Integer; State: TSectionTrackState);
begin
  if assigned(F_HeaderSectionTrack) then F_HeaderSectionTrack(HeaderControl,Section,Width,State);
  sheduleInternRepaint;
end;
procedure TplTreeListView._HeaderSectionResize(HeaderControl: TEventHeaderControl; Section: THeaderSection);
begin
  UpdateScrollSizeH;
  if assigned(F_HeaderSectionResize) then F_HeaderSectionResize(HeaderControl,Section);
  sheduleInternRepaint;
end;

procedure TplTreeListView._HeaderSectionClick(HeaderControl: TEventHeaderControl; Section: THeaderSection);
var NewSortColumn,i:Longint;
    cursor: TPoint;
begin
  if not (tlvoSorted in F_Options) then exit;
  if Section=nil then begin
    GetCursorPos(cursor);
    cursor:=HeaderControl.ScreenToClient(cursor);
    for i := 0 to HeaderControl.Sections.Count - 1 do
      if (HeaderControl.Sections[i].Left<cursor.x)and
        (HeaderControl.Sections[i].right>cursor.x) then begin
          section:=HeaderControl.Sections[i];
          break;
        end;
    if Section=nil  then exit;
  end;
  {$IFDEF allowHeaderDragging}
  NewSortColumn:=Section.OriginalIndex;
  {$ELSE}
  NewSortColumn:=Section.Index;
  {$ENDIF}
  if F_SortColumn=NewSortColumn then F_SortColumnInverted:=not F_SortColumnInverted
  else begin
    F_SortColumn:=NewSortColumn;
    F_SortColumnInverted:=false;
  end;
  if assigned(F_OnUserSortItems) then F_OnUserSortItems(self,F_SortColumn,F_SortColumnInverted);
  sort;
end;

procedure TplTreeListView._HeaderSectionDblClick(
  HeaderControl: TEventHeaderControl; Section: THeaderSection);
var i,w,maxw:longint;
begin
  maxw:=0;
  for i:=0 to items.count-1 do begin
    {$ifdef allowHeaderDragging}
    w:=Items[i].GetMaxColumnWidth(section.OriginalIndex);
    {$else}
    w:=Items[i].GetMaxColumnWidth(section.index);
    {$endif}
    if w>maxw then maxw:=w;
  end;
  if maxw+5>section.Width then section.width:=maxw+5
  else if Section.width+10>maxw+5 then section.width:=maxw+5;
end;

procedure TplTreeListView._HeaderSectionEndDrag(Sender: TObject);
begin
  sheduleInternRepaint;
end;

procedure TplTreeListView._HScrollChange(Sender: TObject);
begin
  if F_SheduledHScroll=0 then begin
    F_SheduledHScroll:=GetTickCount;
    internPostMessage(LM_USER_SHEDULED_EVENT,EVENT_HSCROLL);
  end else if F_SheduledHScroll+20<GetTickCount then begin

    SendMessage(Handle,LM_USER_SHEDULED_EVENT,EVENT_HSCROLL,0);
  end;
end;

procedure TplTreeListView._VScrollChange(Sender: TObject);
begin
  UpdateScrollBarPos;
  hotTrackedRecordItem:=nil;
  F_TopItem:=nil;
  sheduleInternRepaint;
  if assigned(F_VScrollBarChange) then F_VScrollBarChange(F_VScroll);
end;

procedure TplTreeListView.UpdateScrollBarPos;
var realHeight: longint;
begin
  if (tlioDeleting in InternOptions_tlio) or
     (tlioUpdating in InternOptions_tlio) or
     (f_RedrawBlock>0) then exit;

  F_HScroll.Visible := (F_ScrollStyle in [ssHorizontal, ssBoth])
                       or ((F_ScrollStyle in [ssAutoHorizontal, ssAutoBoth]) and F_HScroll.Enabled);
  F_VScroll.Visible := (F_ScrollStyle in [ssVertical, ssBoth])
                       or ((F_ScrollStyle in [ssAutoVertical, ssAutoBoth]) and F_VScroll.Enabled);

  RealHeight := RealBaseClientHeight;
  if F_SearchBar<>nil then if F_SearchBar.Visible then
    realheight:=realheight - F_SearchBar.Height;
  if F_HScroll.Visible then realheight := realheight - F_HScroll.Height;

  if F_VScroll.Visible then F_VScroll.Left:=RealBaseClientWidth-F_VScroll.Width
  else F_VScroll.Left := RealBaseClientWidth;
  if F_HeaderVisible then F_VScroll.Top:=F_Header.Height
  else F_VScroll.Top:=0;
  F_VScroll.Height:=max(1, realHeight - F_VScroll.top);

  F_HScroll.Top:=realHeight;
  F_HScroll.Width:=Max(1, F_VScroll.Left);

  if F_Header.left<>-F_HScroll.Position then
    F_Header.left:=-F_HScroll.Position;
end;

procedure TplTreeListView.internPostMessage(Msg: Cardinal; WParam: WParam);
begin
  {$ifndef android}
  PostMessage(Handle, Msg, WParam, 0);
  {$else}
  F_PostMessage.Msg := Msg;
  F_PostMessage.WParam := WParam;
  F_PostMessage.LParam := 0;
  F_PostMessageTimer.Enabled := true;
  {$endif}
end;

{$ifdef android}
procedure TplTreeListView.PostMessageTimerTimer(Sender: TObject);
var temp: TLMessage;
begin
  if F_PostMessage.Msg <> 0 then begin
    WndProc(F_PostMessage);
    F_PostMessage.Msg:=0;
    F_PostMessageTimer.Enabled:=false;
  end;
end;
{$endif}

procedure TplTreeListView.UpdateScrollSizeH;
var
  i,j: Integer;
begin
  if (tlioDeleting in InternOptions_tlio) or
     (tlioUpdating in InternOptions_tlio) or
     (f_RedrawBlock>0) then exit;
  j:=0;
  for i:=0 to F_Header.Sections.Count-1 do begin
    j:=j+F_Header.Sections[i].Width;
  end;
  if j>=F_HScroll.width then begin
    F_HScroll.Enabled:=true;
    F_HScroll.max:=j;

    F_HScroll.PageSize:=F_HScroll.max*F_HScroll.width div j;
    F_HScroll.LargeChange:=F_HScroll.width;
  end else if F_HScroll.Enabled then begin
    F_HScroll.Enabled:=true;
    F_HScroll.Position:=0;
    F_HScroll.Enabled:=false;
  end;
  if F_HScroll.Enabled <> F_HScroll.Visible then UpdateScrollBarPos;
end;

procedure TplTreeListView.UpdateScrollSizeV;
var i:integer;
begin
  if (tlioDeleting in InternOptions_tlio) or
     (tlioUpdating in InternOptions_tlio) or
     (f_RedrawBlock>0) then exit;

  i:=Items.GetRealItemCount([])-VisibleRowCount;
  if i-1>F_VScroll.Min then
  begin
    F_VScroll.Enabled:=true;
    F_VScroll.Max:=i-1+VisibleRowCount;
    F_VScroll.PageSize:=VisibleRowCount;
    F_VScroll.LargeChange:=VisibleRowCount;
  end else if F_VScroll.Enabled then begin
    F_VScroll.Enabled:=true;
    F_VScroll.Position:=0;
    F_VScroll.Enabled:=false;
  end;
  if F_VScroll.Enabled <> F_VScroll.Visible then UpdateScrollBarPos;
end;

procedure TplTreeListView.selectRange(a, b: TplTreeListItem;mouseSelect:boolean=false);
var meetA, meetB: boolean;
  procedure setSelection(list: TplTreeListItems);
  var i:longint;
  begin
    for i:=0 to list.count-1 do begin
      if meetB then begin
        if mouseSelect then list[i].MouseSelected:=false
        else list[i].Selected:=false
      end else if meetA or (list[i]=a) then begin
        meetA:=true;
        if mouseSelect then list[i].MouseSelected:=true
        else list[i].Selected:=true;
        if list[i]=b then meetB:=true;
      end else
        if mouseSelect then list[i].MouseSelected:=false
        else list[i].Selected:=false;

      setSelection(list[i].SubItems);
    end;
  end;

var temp: TplTreeListItem;
begin
  if items.count=0 then exit;
  if a=nil then a:=items[0];
  if b=nil then b:=items[items.count-1];
  if items.RealIndexOf(a,[ricCountCollapsedsubItems])>items.RealIndexOf(b,[ricCountCollapsedsubItems]) then begin
    temp:=a;
    a:=b;
    b:=temp;
  end;
  BeginMultipleUpdate;
  meetA:=false;
  meetB:=false;
  try
    setSelection(Items);
  finally
    EndMultipleUpdate;
  end;
end;

procedure TplTreeListView.UpdateScrollSize;
begin
  UpdateScrollSizeH;
  UpdateScrollSizeV;
end;

procedure TplTreeListView.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (AComponent=F_ImageList) and (Operation=opRemove) then
    F_ImageList:=nil;
  inherited;
end;

procedure TplTreeListView.WndProc(var message:TLMessage);
var tempRecordItem:TplTreeListRecordItem;
    itemAtPos,nextToFocus: TplTreeListItem;
    shiftState: TShiftState;
    cursorPos: tpoint;
begin
  nextToFocus:=nil;
  case message.Msg of
    LM_GETDLGCODE:  message.Result:=DLGC_WANTARROWS or DLGC_WANTCHARS;
    LM_MOUSEWHEEL: if F_VScroll.Visible and F_VScroll.Enabled then begin

      F_MouseWheelDelta:=F_MouseWheelDelta+(PLMMouseEvent(@message))^.WheelDelta;

      internPostMessage(LM_USER_SHEDULED_EVENT,EVENT_MOUSE_SCROLL);
    end;
    LM_USER_SHEDULED_EVENT: begin
      case message.WParam of
        EVENT_REPAINT:
          if F_SheduledRepaint <> 0 then
            internRepaint;
        EVENT_MOUSE_SCROLL:
          if (F_MouseWheelDelta<=-120) or (F_MouseWheelDelta>=120) then begin
            F_VScroll.Position:=F_VScroll.Position-F_MouseWheelDelta div 120;
            F_MouseWheelDelta:=0;
          end;
        EVENT_HSCROLL: begin
          hotTrackedRecordItem:=nil;
          UpdateScrollBarPos;
          internRepaint;
          F_SheduledHScroll:=0;
          if assigned(F_HScrollBarChange) then F_HScrollBarChange(F_HScroll);
        end;
      end;
    end;
    LM_MOUSEMOVE: begin
      inherited;
      if (GetTickCount>F_LastMouseMove) and (GetTickCount-F_LastMouseMove<20) then exit;
      F_LastMouseMove:=GetTickCount;
      shiftState:=KeyDataToShiftState(TLMMouseMove(message).Keys);
      if (F_ClickedItem<>nil) and (tlvoMultiSelect in F_Options) and
          ((MK_LBUTTON and message.wParam <> 0)<>(MK_RBUTTON and message.wParam <> 0))
          and ((MK_LBUTTON and message.wParam <> 0) or
               ((tlvoRightMouseSelects in F_Options) and (MK_RBUTTON and message.wParam <> 0))) then begin
        F_RealMousePos:=point(TLMMouseMove(message).XPos+F_HScroll.Position,
                              TLMMouseMove(message).YPos+F_VScroll.Position*RowHeight);
        if F_MouseSelecting=msNone then begin
          if sqr(F_RealClickPos.x-F_RealMousePos.x)+ sqr(F_RealClickPos.y-F_RealMousePos.y)>100 then
            if MK_RBUTTON and message.wParam <> 0 then F_MouseSelecting:=msRight
            else F_MouseSelecting:=msLeft;
          if (F_ClickedItem<>nil)  and (F_MouseSelecting<>msNone) and (ssCtrl in shiftState) then
             F_ClickedItem.Selected := not F_ClickedItem.Selected;
        end;
        if F_MouseSelecting<>msNone then begin
          itemAtPos:=GetItemAtPos(TLMMouseMove(message).YPos);
          if itemAtPos<>nil then begin
            selectRange(F_ClickedItem,itemAtPos,(ssCtrl in shiftState));
            ensureVisibility(itemAtPos);
          end;
        end;
      end;


      if (TLMMouseMove(message).XPos<F_VScroll.Left) and (TLMMouseMove(message).YPos>F_VScroll.Top)
         and (TLMMouseMove(message).YPos<F_HScroll.Top) then
        tempRecordItem:=GetRecordItemAtPos(TLMMouseMove(message).XPos,TLMMouseMove(message).YPos)
       else
        tempRecordItem:=nil;
      if tempRecordItem<>nil then begin
        if tlvoToolTips in F_Options then
          if (ColumnFromOriginalIndex(tempRecordItem.Index)<>nil) and
            (tempRecordItem.GetNecessaryWidth(self)+10>ColumnFromOriginalIndex(tempRecordItem.Index).Width) then begin
            hint:=tempRecordItem.Text;
            ShowHint:=true;
          end else ShowHint:=false;
      end;
      IF tlvoHotTrackRecordTextItems in F_Options THEN
        if tempRecordItem<>hotTrackedRecordItem then begin
          if hotTrackedRecordItem<>nil then
            invalidateItem(hotTrackedRecordItem.Parent);
          hotTrackedRecordItem:=tempRecordItem;
          if hotTrackedRecordItem<>nil then begin
            invalidateItem(hotTrackedRecordItem.Parent);
            Cursor:=crHandPoint;
          end else Cursor:=crDefault;
          internPaint;
        end;
      if (tlvoDragScrolling in F_Options) {$ifndef android}and (MK_LBUTTON and message.wParam <> 0){$endif} then begin
         F_VScroll.Position := F_ScrollClickPos - (TLMMouseMove(message).YPos - RowHeight div 2 + F_ScrollClickPos*RowHeight - F_RealClickPos.y) div RowHeight
      end;
    end;
    LM_LBUTTONDOWN,LM_RBUTTONDOWN: begin
      if not (csDesigning in ComponentState) then
        SetFocus;
      if TLMLBUTTONDOWN(message).YPos<F_HScroll.top then begin
        shiftState:=KeyDataToShiftState(TLMLBUTTONDOWN(message).Keys);
        itemAtPos:=GetItemAtPos(TLMLBUTTONDOWN(message).YPos);
        if (message.msg=LM_LBUTTONDOWN) and (ExpandMode=emExpandByClick) then
          if (itemAtPos<>nil) and
             (TLMLBUTTONDOWN(message).XPos<itemAtPos.GetExtendingButtonPos+9) and
             (TLMLBUTTONDOWN(message).XPos>itemAtPos.GetExtendingButtonPos) then begin
            itemAtPos.Expanded:=not itemAtPos.Expanded;
          end;
        if (message.msg=LM_LBUTTONDOWN) or (tlvoRightMouseSelects in F_Options) then begin
          F_ClickedItem:=itemAtPos;
          F_ScrollClickPos := F_VScroll.Position;
          F_RealClickPos:=point(TLMLBUTTONDOWN(message).XPos+F_HScroll.Position,TLMLBUTTONDOWN(message).YPos+F_VScroll.Position*RowHeight);
          if itemAtPos <> nil then begin
            if (shiftState <> []) or not (tlvoDragScrolling in Options) then begin
              if (message.Msg = LM_LBUTTONDOWN) or not (itemAtPos.Selected) then
                nextToFocus:=itemAtPos;
              if (tlvoMultiSelect in F_Options) and (ssCtrl in shiftState) then
                nextToFocus.Selected:=not nextToFocus.Selected;
            end;
          end else if message.Msg = LM_LBUTTONDOWN then
              nextToFocus := nil;
          if (F_MouseSelecting<>msNone) and (nextToFocus = itemAtPos) then begin
            F_MouseSelecting:=msNone;
            setMouseSelection(Items);
          end;
        end;
      end;
      inherited;
    end;
    LM_LBUTTONUP,LM_RBUTTONUP: begin
      if F_MouseSelecting<>msNone then begin
        F_ClickedItem:=nil;
        F_MouseSelecting:=msNone;
        setMouseSelection(Items);
      end;
      if message.msg=LM_LBUTTONUP then begin
        if (F_ClickedItem <> nil) and (F_ClickedItem = GetItemAtPos(TLMLButtonUp(message).YPos)) then begin
          if assigned(OnClickAtRecordItem) then begin
            tempRecordItem:=F_ClickedItem.GetRecordItemAtPos(self,TLMLButtonUp(message).XPos);
            if tempRecordItem<>nil then OnClickAtRecordItem(self,tempRecordItem);
          end;
          if assigned(OnClickAtItem) then OnClickAtItem(self,F_ClickedItem);
          if tlvoDragScrolling in Options then
            nextToFocus := F_ClickedItem;
        end;
      end {$ifdef openOwnPopupMenu} else if message.msg=LM_RBUTTONUP then begin
        GetCursorPos(cursorPos);
        if assigned(PopupMenu) then PopupMenu.PopUp(cursorPos.x,cursorPos.Y);

      end{$endif};
      inherited;
    end;
    LM_LBUTTONDBLCLK:begin
      if (TLMLButtonDblClk(message).YPos<F_HScroll.top) and (ExpandMode=emExpandByDoubleClick) then begin
        itemAtPos:=GetItemAtPos(TLMLButtonDblClk(message).YPos);
        if (itemAtPos<>nil) and
           (TLMLButtonDblClk(message).XPos<itemAtPos.GetExtendingButtonPos+9) and
           (TLMLButtonDblClk(message).XPos>itemAtPos.GetExtendingButtonPos) then begin
            itemAtPos.Expanded:=not itemAtPos.Expanded;
          end;
      end;
      inherited;
    end;
    LM_KEYDOWN: begin
      shiftState:=KeyDataToShiftState(TLMKeyDown(message).KeyData);
      case TLMKeyDown(message).CharCode of
        VK_UP: begin
          if focused=nil then nextToFocus:=Items[0]
          else nextToFocus:=focused.GetPrevVisibleItem;
          message.Result:=1;
        end;
        VK_DOWN: begin
          if focused<>nil then nextToFocus:=focused.GetNextVisibleItem()
          else if items.count>0 then nextToFocus:=Items[0];
          message.Result:=1;
        end;

        VK_HOME:
          if items.count>0 then nextToFocus:=Items[0];

        VK_END:
          if items.count>0 then nextToFocus:=Items[items.count-1].GetLastVisibleSubSubItem;

        VK_PRIOR:
          if focused<>nil then nextToFocus:=focused.GetPrevVisibleItem(VisibleRowCount)
          else if items.count>0 then nextToFocus:=Items[0];

        VK_NEXT:
          if focused<>nil then nextToFocus:=focused.GetNextVisibleItem(VisibleRowCount)
          else if items.count>0 then nextToFocus:=Items[items.count-1];

        VK_RIGHT: begin
          if focused<>nil then begin
            if not focused.Expanded then focused.Expand;
            if focused.SubItems.Count>0 then nextToFocus:=focused.SubItems[0]
            else nextToFocus:=focused;
          end;
          message.Result:=1;
        end;

        VK_LEFT: begin
          if focused<>nil then begin
            if (focused.Expanded) and (focused.SubItems.Count>0) then focused.Collapse
            else if focused.Parent<>nil then nextToFocus:=focused.Parent;
            if nextToFocus=nil then nextToFocus:=focused;
          end;
          message.Result:=1;
        end;
        VK_BACK:
          if (focused<>nil) and (focused.Parent<>nil) then nextToFocus:=focused.Parent;

        VK_SPACE:
          if (focused <> nil) and (ssCtrl in shiftState) then begin
            if ssShift in shiftState then Selected:=focused
            else focused.Selected:=not focused.Selected;
            F_BaseSelect:=focused;
          end;
        else inherited;
      end;
    end;
    LM_SETFOCUS:    begin
                      internPaint;
                      inherited;
                    end;
    LM_KILLFOCUS:   begin
                      internPaint;
                      inherited;
                    end;
    LM_SIZE:        begin
                      UpdateScrollBarPos;
                      UpdateScrollSize;
                      internPaint(true);
                      inherited;
                    end;
    LM_ERASEBKGND: message.Result:=1;
    LM_KEYUP:
      if (TLMKeyUp(message).CharCode = VK_APPS) and assigned(PopupMenu) then begin
        GetCursorPos(cursorPos);
        PopupMenu.PopUp(cursorPos.X,cursorPos.y);
      end else if (TLMKeyUp(message).CharCode = ord('F')) and (F_SearchBar<>nil) then begin
        shiftState:=KeyDataToShiftState(TLMKeyDown(message).KeyData);
        if ssCtrl in shiftState then
          F_SearchBar.Show
        else inherited;
      end else if (TLMKeyUp(message).CharCode = VK_ESCAPE) and (F_SearchBar<>nil) then
        F_SearchBar.Hide
      else inherited;
    else inherited;
  end;
  if nextToFocus<>nil then begin
    if not (tlvoMultiSelect in F_Options) or (shiftState=[]) then begin
      Selected:=nextToFocus;
      F_BaseSelect:=Selected;
    end else if shiftState=[ssCtrl] then focused:=nextToFocus
    else if ssShift in shiftState then begin
      F_Focused:=nextToFocus;
      SelectRange(F_BaseSelect,nextToFocus);
      ensureVisibility(nextToFocus);
    end;
  end;

end;

procedure TplTreeListView.sheduleInternRepaint();
begin
  if not HandleAllocated then
     exit;
  if F_SheduledRepaint=0 then begin
    F_SheduledRepaint:=GetTickCount;
    internPostMessage(LM_USER_SHEDULED_EVENT,EVENT_REPAINT);
  end else if F_SheduledRepaint+20<GetTickCount then begin
    internRepaint();
  end;
end;

procedure TplTreeListView.internRepaint();
begin
  invalidateAll();
  F_SheduledRepaint:=0;
  internPaint;
end;

procedure TplTreeListView.invalidateItem(item: TplTreeListItem);
begin
  if f_invalidateAll then exit;
  if tlvoAlwaysFullRepaint in F_Options then
    f_invalidateAll:=true
   else if f_invalidatedItems.IndexOf(item)<0 then begin
    f_invalidatedItems.Add(item);
    if f_invalidatedItems.Count>height div RowHeight then begin
      f_invalidateAll:=true;
      f_invalidatedItems.Clear;
    end;
  end;
end;

procedure TplTreeListView.invalidateAll();
begin
  f_invalidateAll:=true;
  f_TopItem:=nil;
  f_invalidatedItems.Clear;
end;


procedure TplTreeListView.internDraw();
  var RealCanvasHandle:HDC;    // ct9999
//--------------------------------------------
  procedure switchDoubleBuffer(paintToBuffer: boolean);
  begin
    if paintToBuffer then begin
      RealCanvasHandle:=canvas.handle;
      canvas.handle:=doubleBuffer.Canvas.Handle;
    end else begin
      canvas.Handle:=RealCanvasHandle;
    end;
  end;
//--------------------------------------------
var i,xpos:longint;
    defaultDraw:boolean;
    curItem: TplTreeListItem;
    stack: TItemHierarchyStack;
begin
  f_bufferComplete:= f_invalidateAll;
//  if f_invalidateAll then Beep;
  switchDoubleBuffer(true);
  try
    {$IFDEF allowHeaderDragging}
      for i:=0 to F_Header.Sections.Count-1 do
        if F_Header.Sections[i].OriginalIndex=0 then begin
          F_TreeSectionPos:=rect(F_Header.Sections[i].Left-F_HScroll.Position,0,F_Header.Sections[i].Right-F_HScroll.Position,0);
          break;
        end;
    {$ELSE}
      F_TreeSectionPos:=rect(F_Header.Sections[0].Left-F_HScroll.Position,0,F_Header.Sections[0].Right-F_HScroll.Position,0);
    {$ENDIF}
    F_DrawingEvenItem:=GetTopItemEven;

    with Canvas do begin
      //Background
      defaultDraw:=DoCustomBackgroundDrawEvent(cdetPrePaint);
      if defaultDraw then begin
        pen.Style:=psClear;
        brush.Style:=bsSolid;
        brush.color:=F_BgColor;
        FillRect(rect(0,F_VScroll.Top,doubleBuffer.Width,doubleBuffer.Height));
      end;

      //Items
      if TopItem <>nil then begin
        TreeColumnIndentation:=13;

        F_DrawingYPos:=TopPos+F_VScroll.Position*RowHeight;
        curItem:=TopItem;
        curitem.GetParentHierarchyStack(stack);
        while (curItem<>nil) and (F_DrawingYPos<=height) do begin
          if f_invalidateAll or (f_invalidatedItems.IndexOf(curItem)>=0) then
            curItem.Paint(stack);
          F_DrawingEvenItem:=not F_DrawingEvenItem;
          if (tlvoStripInvisibleItems in F_Options) and not curitem.Expanded then
            if (curitem.SubItems.GetRealItemCount([ricCountCollapsedSubItems]) and $1=0) then
              F_DrawingEvenItem:=not F_DrawingEvenItem;
          curItem:=curItem.GetNextFromHierarchyStack(stack,true);
          F_DrawingYPos:=F_DrawingYPos+RowHeight;
        end;
      end;

      //Lines
      if defaultDraw then begin
        xpos:=-F_HScroll.Position;
        for i:=0 to F_Header.Sections.Count-1 do begin
          inc(xpos,F_Header.Sections[i].Width);
          case VerticalLineMode of
            lmSolid: begin
                       pen.color:=VerticalLineColor;
                       pen.Style:=psSolid;
                       MoveTo(xpos,F_VScroll.Top);
                       LineTo(xpos,doubleBuffer.Height);
                     end;
            lmDot:   DrawAlignDotLine(xpos,F_VScroll.Top,xpos,doubleBuffer.Height,VerticalLineColor);
          end;
        end;
      end;
      DoCustomBackgroundDrawEvent(cdetPostPaint);
    end;
  finally
    switchDoubleBuffer(false);
  end;
end;


procedure TplTreeListView.internPaint(calledFromPaintingEvent: boolean=false);
  function sortedrect(x1,y1,x2,y2:longint):TRect;
  begin
    if x1<=x2 then begin
      result.left:=x1;
      result.right:=x2;
    end else begin
      result.left:=x2;
      result.right:=x1;
    end;
    if y1<=y2 then begin
      result.top:=y1;
      result.bottom:=y2;
    end else begin
      result.top:=y2;
      result.bottom:=y1;
    end;
  end;
var ypos:integer;
    curItem: TplTreeListItem;
    stack: TItemHierarchyStack;
    outRect: Trect;
    newWidth, newHeight: longint;
begin
  if (tlioUpdating in InternOptions_tlio) or
     (tlioDeleting in InternOptions_tlio) or
     (f_items=nil) or
     (f_RedrawBlock>0) or (parent=nil) or
     (F_SheduledRepaint<>0)  or
     (Width <= 0) or (Height <= 0)
     then exit;

{$IFNDEF WINDOWS}
 {$IFNDEF WIN32}
   {$IFNDEF LCLGTK2}
  if not calledFromPaintingEvent then begin
   {$IF DEFINED(LCLQT) or DEFINED(LCLQT5)} //=== ct9999 =====
    TQtWidget(Handle).setAttribute(4 {QtWA_OpaquePaintEvent}, true);
    TQtWidget(Handle).setAttribute(9 {QtWA_NoSystemBackground}, true);
    Update;
    {$ELSE}
    f_invalidateAll:=true;
    Repaint; //needed for android?, not sure about the other OS
    {$ENDIF}
    exit;
  end;
   {$ENDIF}
 {$ENDIF}
{$ENDIF}

  f_RedrawBlock:=1000;
  newWidth:=width +128 - Width mod 128;
  newHeight:=height +128 - height mod 128;
  if (newWidth<>doubleBuffer.Width) or (newHeight <> doubleBuffer.Height) then begin
    {if (newWidth>doubleBuffer.Width) or (newHeight > doubleBuffer.Height) then }f_invalidateAll:=true;

    doubleBuffer.SetSize(newwidth,newheight);

  end else if ((F_LastPaintedWidth<>width) or (F_LastPaintedHeight<>height)) and not f_invalidateAll then begin
    f_invalidateAll:=assigned(F_CustomBgDraw) or Assigned(F_CustomItemDraw);
  end;
  F_LastPaintedWidth:=width;
  F_LastPaintedHeight:=height;
  if F_MouseSelectingFocusRectDraw then begin
    canvas.Brush.Style:=bsSolid;
    canvas.pen.style:=psSolid;
    canvas.Brush.Color:=clWhite;
    canvas.font.color:=clBlack;
    canvas.pen.Color:=clBlack;
    canvas.TextOut(width+10,10,' ');
    canvas.DrawFocusRect(F_MouseSelectingFocusRect);
    F_MouseSelectingFocusRectDraw:=false;
  end;
  if (f_invalidatedItems.count>0) or f_invalidateAll or (Assigned(F_SearchBar) and (F_NewSearchBarFindState <> F_SearchBar.FindState)) then
    internDraw();

  canvas.pen.Style:=psClear;
  canvas.brush.Style:=bsSolid;
  canvas.brush.Color:=clBtnFace;
  outRect:=rect(F_HScroll.left+F_HScroll.Width,F_VScroll.top+F_VScroll.Height,RealBaseClientWidth,RealBaseClientHeight);
  if (F_SearchBar<>nil) and (f_searchbar.Visible) then dec(outRect.Bottom, F_SearchBar.Height);
  canvas.FillRect(outRect);

  outRect:=rect(0,F_VScroll.Top,F_VScroll.Left ,F_HScroll.top);
  if Assigned(F_SearchBar) and (F_NewSearchBarFindState <> F_SearchBar.FindState) then
    F_SearchBar.FindState := F_NewSearchBarFindState;
  if f_bufferComplete then
    canvas.CopyRect(outRect,doubleBuffer.canvas,outRect)
  else begin
    curItem:=TopItem;
    curitem.GetParentHierarchyStack(stack);
    ypos:=TopPos+F_VScroll.Position*RowHeight;
    while (curItem<>nil) and (ypos<=F_HScroll.Top) do begin
      if (f_invalidatedItems.IndexOf(curItem)>=0) then begin
        outRect.Top:=ypos;
        outRect.bottom:=min(ypos+RowHeight,F_HScroll.Top);
        canvas.CopyRect(outRect,doubleBuffer.canvas,outRect);
      end;
      curItem:=curItem.GetNextFromHierarchyStack(stack,true);
      ypos:=ypos+RowHeight;
    end;
  end;
  F_MouseSelectingFocusRectDraw:=F_MouseSelecting<>msNone;
  if F_MouseSelectingFocusRectDraw then begin
    F_MouseSelectingFocusRect:=sortedrect(F_RealClickPos.x-F_HScroll.Position,F_RealClickPos.y-F_VScroll.Position*RowHeight,
                                          F_RealMousePos.x-F_HScroll.Position,F_RealMousePos.y-F_VScroll.Position*RowHeight);

    canvas.Brush.Style:=bsSolid;
    canvas.pen.style:=psSolid;
    canvas.Brush.Color:=clWhite;
    canvas.font.color:=clBlack;
    canvas.pen.Color:=clBlack;
    canvas.TextOut(width+10,10,' ');
    canvas.DrawFocusRect(F_MouseSelectingFocusRect);
  end;
  f_invalidatedItems.Clear;
  f_invalidateAll:=false;
  f_RedrawBlock:=0;
end;

procedure TplTreeListView.Paint;
begin
  inherited;
  if not f_bufferComplete then  f_invalidateAll:=true;
  internPaint(true);
  if F_SearchBar<>nil then F_SearchBar.Invalidate;
end;

destructor TplTreeListView.Destroy;
begin
  Include(InternOptions_tlio,tlioDeleting);
  F_HotTrackFont.free;
  F_SelectedHotTrackFont.free;
  F_SelectedFont.free;

  F_HScroll.free;
  F_VScroll.free;
  if F_HeaderColumnPopupMenu<>nil then F_HeaderColumnPopupMenu.free;
  F_Header.free;
  F_Items.free;
  f_invalidatedItems.free;
  DoubleBuffer.Free;
  F_SearchBar.free;F_SearchBar:=nil;
  inherited;
end;

end.




