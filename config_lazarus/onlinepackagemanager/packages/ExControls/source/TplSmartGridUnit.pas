
{**********************************************************************
 Package pl_ExControls.pkg
 From PilotLogic Software House(http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit TplSmartGridUnit;

interface

uses
  LCLType, LCLIntf, LMessages, LCLProc,
  Forms, Controls, Messages,
  SysUtils, Classes, Graphics,
  StdCtrls, ExtCtrls, Clipbrd, Math;

type
  PHeaderInfo = ^THeaderInfo;

  THeaderInfo = record
    Str: string;
    Rc: TRect;
  end;

  THorzAlign = (haLeft, haCenter, haRight);
  TVertAlign = (vaTop, vaCenter, vaBottom);
  TGutterKind = (gkNone, gkBlank, gkPointer, gkNumber, gkString);
  TGridHittest = (gtNone, gtLeftTop, gtLeft, gtTop, gtCell, gtColSizing, gtSmallBox);


  TOnDrawCellEvent = procedure(Sender: TObject; ACanvas: TCanvas; X, Y: integer; Rc: TRect; var Handled: boolean) of object;
  TOnDrawHeaderEvent = procedure(Sender: TObject; ACanvas: TCanvas; Rc: TRect; Str: string; var Handled: boolean) of object;
  TOnFilterChar = procedure(Sender: TObject; Col: integer; Row: integer; Chr: char; var Allowed: boolean) of object;
  TOnHeaderClick = procedure(Sender: TObject; Col: integer; Button: TMouseButton; Shift: TShiftState) of object;
  TOnGutterClick = procedure(Sender: TObject; Row: integer; Button: TMouseButton; Shift: TShiftState) of object;
  TOnCellAssignment = procedure(Sender: TObject; Col, Row: integer; var Str: string) of object;
  TOnCellChange = procedure(Sender: TObject; Col, Row: integer; var Str: string) of object;
  TOnCellChanging = procedure(Sender: TObject; Col, Row: integer; var CanChange: boolean) of object;
  TOnRowEvent = procedure(Sender: TObject; ARow: integer) of object;
  TOnColRowChanged = procedure(Sender: TObject; Col, Row: integer) of object;

  TplSmartGrid = class;

  TplSmartColumn = class(TCollectionItem)
  private
    FTitle: string;
    FFooter: string;
    FWidth: integer;
    FFont: TFont;
    FColor: TColor;
    FHorzAlign: THorzAlign;
    FVertAlign: TVertAlign;
    FVisible: boolean;
    FStrings: TStrings;
    FTag: integer;
    FTag2: integer;
    FCanResize: boolean;
    FHint: string;
    FReadOnly: boolean;
    function GetGrid: TplSmartGrid;
    function IsFontStored: boolean;
    procedure FontChange(Sender: TObject);
    procedure SetTitle(Value: string);
    procedure SetWidth(Value: integer);
    procedure SetFont(Value: TFont);
    procedure SetColor(Value: TColor);
    procedure SetHorzAlign(Value: THorzAlign);
    procedure SetVertAlign(Value: TVertAlign);
    procedure SetVisible(Value: boolean);
    procedure SetStrings(Value: TStrings);
    procedure SetFooter(const Value: string);
  protected
    function GetDisplayName: string; //override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Grid: TplSmartGrid read GetGrid;
    property Title: string read FTitle write SetTitle;
    property Footer: string read FFooter write SetFooter;
    property Width: integer read FWidth write SetWidth;
    property Font: TFont read FFont write SetFont stored IsFontStored;
    property Color: TColor read FColor write SetColor default clWindow;
    property HorzAlign: THorzAlign read FHorzAlign write SetHorzAlign default haLeft;
    property VertAlign: TVertAlign read FVertAlign write SetVertAlign default vaCenter;
    property Visible: boolean read FVisible write SetVisible default True;
    property Tag: integer read FTag write FTag default 0;
    property Tag2: integer read FTag2 write FTag2 default 0;
    property Hint: string read FHint write FHint;
    property Strings: TStrings read FStrings write SetStrings;
    property CanResize: boolean read FCanResize write FCanResize default True;
    property ReadOnly: boolean read FReadOnly write FReadOnly default False;
  end;


  TplSmartColumns = class(TCollection)
  private
    FGrid: TplSmartGrid;
    function GetItem(Index: integer): TplSmartColumn;
    procedure SetItem(Index: integer; Value: TplSmartColumn);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AGrid: TplSmartGrid);
    property Grid: TplSmartGrid read FGrid;
    property Items[Index: integer]: TplSmartColumn read GetItem write SetItem; default;
    function Add: TplSmartColumn;
    function AddItem(Item: TplSmartColumn; Index: integer): TplSmartColumn;
    function Insert(Index: integer): TplSmartColumn;
  end;


  TplSmartInplace = class(TEdit)
  private
    FGrid: TplSmartGrid;
    FAlignment: THorzAlign;
    CellX, CellY: integer;
    procedure SetAlignment(Value: THorzAlign);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Change; override;
    procedure KeyDown(var Key: word; Shift: TShiftState); override;
    procedure KeyPress(var Key: char); override;
  public
    constructor Create(Grid: TplSmartGrid); reintroduce;
    procedure ShowEdit(X, Y: integer);
    procedure HideEdit;
  end;

  TMergeCell = class(TObject)
  public
    Caption: string;
    Rc: TRect;
    Color: TColor;
    Font: TFont;
    HorzAlign: THorzAlign;
    VertAlign: TVertAlign;
    constructor Create;
    destructor Destroy; override;
  end;

  TplSmartGridSync = class;

  TplSmartGrid = class(TCustomPanel)
  private
    ForcedColumn: integer;
    FixedWidth, FixedHeight: integer;
    BodyWidth, BodyHeight: integer;
    AllWidth, AllHeight: integer;
    FooterTop: integer;
    CellBox: TRect;
    FHorzOffset: integer;
    FVertOffset: integer;
    FMaxHScroll: integer;
    FMaxVScroll: integer;
    FSmallChange: integer;
    FLargeChange: integer;
    FAutoAddRow: boolean;
    FRowCount: integer;
    FDefRowHeight: integer;
    FDefColWidth: integer;
    FFlat: boolean;
    FHeaderLine: integer;
    FHeaderInfos: TList;
    FUpdating: boolean;
    FColor: TColor;
    FAlternateColor: TColor;
    FGridColor: TColor;
    FShowGrid: boolean;
    FHeaderColor: TColor;
    FHeaderLightColor: TColor;
    FHeaderDarkColor: TColor;
    FSelectionColor: TColor;
    FHeaderFont: TFont;
    FGutterFont: TFont;
    FGutterKind: TGutterKind;
    FGutterWidth: integer;
    FFitToWidth: boolean;
    FAutoColWidth: boolean;
    FReadOnly: boolean;
    FColumns: TplSmartColumns;
    ValidationEnabled: boolean;
    FEdit: TplSmartInplace;
    FCol: integer;
    FRow: integer;
    FCol2, FRow2: integer; // Selection
    FSelectArea: TRect;
    SmallBox: TRect;
    SmallBoxArea: TRect;
    SmallBoxPos: byte;
    BuffString: string;
    IsEditing: boolean;
    SizingCol: integer;
    SizingColX: integer;
    LastHover: integer;
    Sync: TplSmartGridSync;
    Mergeds: TList;

    FOnDrawCell: TOnDrawCellEvent;
    FOnDrawHeader: TOnDrawHeaderEvent;
    FOnDrawGutter: TOnDrawHeaderEvent;
    FOnDrawFooter: TOnDrawHeaderEvent;
    FOnFilterChar: TOnFilterChar;
    FOnHeaderClick: TOnHeaderClick;
    FOnGutterClick: TOnGutterClick;
    FOnCellChange: TOnCellChange;
    FOnCellChanging: TOnCellChanging;
    FOnColRowChanged: TOnColRowChanged;
    FOnInsertRow: TOnRowEvent;
    FOnDeleteRow: TOnRowEvent;
    FOnCellAssignment: TOnCellAssignment;
    FGutterStrings: TStrings;
    FShowFooter: boolean;
    FFooterFont: TFont;
    FEnabled: boolean;
    FAutoFillRight: boolean;
    FAutoFillDown: boolean;

    procedure WMUnknown(var Msg: TLMessage); message WM_USER + $B902;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure WMMouseWheel(var Message: TLMMouseEvent); message LM_MOUSEWHEEL;
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
    procedure WMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
    procedure WMKillFocus(var Message: TLMKillFocus); message LM_KILLFOCUS;
    procedure WMKeyDown(var Message: TLMKeyDown); message LM_KEYDOWN;

    procedure CMFontChanged(var Msg: TLMessage); message CM_FONTCHANGED;

    function TotalWidth: integer;
    procedure ClearHeaderInfos;

    procedure ClearUnused;
    procedure RenderGutter;
    procedure RenderHeader;
    procedure DrawSelection;

    procedure SetHorzOffset(Value: integer);
    procedure SetVertOffset(Value: integer);
    function GetColCount: integer;
    procedure SetColCount(Value: integer);
    procedure SetRowCount(Value: integer);
    procedure SetDefColWidth(Value: integer);
    procedure SetDefRowHeight(Value: integer);
    procedure SetFlat(Value: boolean);
    procedure SetColor(Value: TColor);
    procedure SetAlternateColor(Value: TColor);
    procedure SetGridColor(Value: TColor);
    procedure SetShowGrid(Value: boolean);
    procedure SetHeaderLine(Value: integer);
    procedure SetHeaderColor(Value: TColor);
    procedure SetHeaderLightColor(Value: TColor);
    procedure SetHeaderDarkColor(Value: TColor);
    procedure SetHeaderFont(Value: TFont);
    procedure SetSelectionColor(Value: TColor);
    procedure SetFitToWidth(Value: boolean);
    procedure SetAutoColWidth(Value: boolean);
    procedure SetReadOnly(Value: boolean);
    procedure InternalSetCell(X, Y: integer; Value: string; FireOnChange: boolean);
    procedure SetCell(X, Y: integer; Value: string);
    function GetColWidths(Index: integer): integer;
    procedure SetColWidths(Index: integer; Value: integer);
    procedure SetColumns(Value: TplSmartColumns);
    procedure SetCol(Value: integer);
    procedure SetRow(Value: integer);
    procedure AdjustSelection(Value: TRect; Force: boolean);
    procedure SetSelectArea(Value: TRect);
    procedure SetGutterKind(Value: TGutterKind);
    procedure SetGutterWidth(Value: integer);
    procedure SetGutterFont(Value: TFont);
    procedure HeaderFontChange(Sender: TObject);
    procedure GutterFontChange(Sender: TObject);

    function CreateColumn: TplSmartColumn;
    procedure UpdateColumn(Index: integer);
    procedure UpdateColumns;
    procedure UpdateHeader;

    function GetCellRect(x, y: integer): TRect;
    function CellRectToClient(R: TRect): TRect;
    function GetCellAtPos(X, Y: integer): TPoint;
    function GetColFromX(X: integer): integer;
    function GetRowFromY(Y: integer): integer;
    function GetColCoord(I: integer): integer;
    function GetCell(X, Y: integer): string;
    function SafeGetCell(X, Y: integer): string;
    function GetCellColor(X, Y: integer): TColor;
    procedure DrawCell(X, Y: integer);
    function FastDrawCell(X, Y: integer; aIsEditing: boolean): TPoint;
    procedure ForceHideCaret;
    procedure ForceShowCaret;
    procedure NormalizeVertOffset;
    procedure InvalidateCells;
    procedure InvalidateRightWard(aLeft: integer);
    procedure InvalidateDownWard(aTop: integer);
    procedure InvalidateHeader;
    procedure InvalidateGutter;
    function GetFirstVisible: integer;
    function GetLastVisible: integer;
    function GetNextVisible(Index: integer): integer;
    function GetPrevVisible(Index: integer): integer;
    procedure ColRowChanged;
    procedure SetGutterStrings(const Value: TStrings);
    function GetObject(X, Y: integer): TObject;
    procedure SetObject(X, Y: integer; const Value: TObject);
    procedure BuildMergeData;
    procedure DrawMergedCell(Index: integer);
    procedure SetShowFooter(const Value: boolean);
    procedure RenderFooter;
    procedure SetFooterFont(const Value: TFont);
    procedure FooterFontChange(Sender: TObject);
    procedure DrawFixCell(Rc: TRect; Str: string; AFont: TFont; AEvent: TOnDrawHeaderEvent);
    procedure SetEnabled(const Value: boolean); reintroduce;

  protected
    function GetMergedCellsData: TList;
    function GetHeaderInfo: TList;
    procedure SetScrollBar(AKind, AMax, APos, AMask: integer); virtual;
    procedure ShowHideScrollBar(HorzVisible, VertVisible: boolean); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Recalculate; virtual;
    procedure CreateWnd; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure KeyDown(var Key: word; Shift: TShiftState); override;
    procedure KeyPress(var Key: char); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetHitTestInfo(X, Y: integer): TGridHitTest;
    function HeaderCellsCount: integer;
    function HeaderCells(I: integer): THeaderInfo;
    function AddRow: integer;
    function MergeCells(const X1, Y1, X2, Y2: integer; ACaption: string): TMergeCell;

    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Clear;
    procedure EnsureVisible(X, Y: integer); overload;
    procedure CutToClipboard;
    procedure CopyToClipboard;
    procedure PasteFromClipboard;
    procedure DeleteRow(ARow: integer);
    procedure InsertRow(ARow: integer);
    procedure ClearMergeCells;

    property HorzOffset: integer read FHorzOffset write SetHorzOffset;
    property VertOffset: integer read FVertOffset write SetVertOffset;
    property Col: integer read FCol write SetCol;
    property Row: integer read FRow write SetRow;
    property SelectArea: TRect read FSelectArea write SetSelectArea;
    property Cells[X, Y: integer]: string read GetCell write SetCell; default;
    property Objects[X, Y: integer]: TObject read GetObject write SetObject;
    property ColWidths[Index: integer]: integer read GetColWidths write SetColWidths;

  published
    property Enabled: boolean read FEnabled write SetEnabled default True;
    property ColCount: integer read GetColCount write SetColCount;
    property RowCount: integer read FRowCount write SetRowCount default 5;
    property AutoAddRow: boolean read FAutoAddRow write FAutoAddRow default False;
    property AutoFillDown: boolean read FAutoFillDown write FAutoFillDown default False;
    property AutoFillRight: boolean read FAutoFillRight write FAutoFillRight default False;
    property DefRowHeight: integer read FDefRowHeight write SetDefRowHeight default 18;
    property DefColWidth: integer read FDefColWidth write SetDefColWidth default 80;
    property Flat: boolean read FFlat write SetFlat default True;
    property Color: TColor read FColor write SetColor default clWindow;
    property AlternateColor: TColor read FAlternateColor write SetAlternateColor default clWindow;
    property GridColor: TColor read FGridColor write SetGridColor default clBtnFace;
    property ShowGrid: boolean read FShowGrid write SetShowGrid default True;
    property HeaderLine: integer read FHeaderLine write SetHeaderLine default 1;
    property HeaderColor: TColor read FHeaderColor write SetHeaderColor default clBtnFace;
    property HeaderLightColor: TColor read FHeaderLightColor write SetHeaderLightColor default clBtnHighlight;
    property HeaderDarkColor: TColor read FHeaderDarkColor write SetHeaderDarkColor default clBtnShadow;
    property HeaderFont: TFont read FHeaderFont write SetHeaderFont;
    property FooterFont: TFont read FFooterFont write SetFooterFont;
    property SelectionColor: TColor read FSelectionColor write SetSelectionColor default $00CAFFFF;
    property FitToWidth: boolean read FFitToWidth write SetFitToWidth default False;
    property AutoColWidth: boolean read FAutoColWidth write SetAutoColWidth default False;
    property ReadOnly: boolean read FReadOnly write SetReadOnly default False;
    property Columns: TplSmartColumns read FColumns write SetColumns;
    property GutterKind: TGutterKind read FGutterKind write SetGutterKind default gkBlank;
    property GutterWidth: integer read FGutterWidth write SetGutterWidth default 20;
    property GutterFont: TFont read FGutterFont write SetGutterFont;
    property GutterStrings: TStrings read FGutterStrings write SetGutterStrings;
    property ShowFooter: boolean read FShowFooter write SetShowFooter;

    property OnDrawCell: TOnDrawCellEvent read FOnDrawCell write FOnDrawCell;
    property OnDrawHeader: TOnDrawHeaderEvent read FOnDrawHeader write FOnDrawHeader;
    property OnDrawGutter: TOnDrawHeaderEvent read FOnDrawGutter write FOnDrawGutter;
    property OnDrawFooter: TOnDrawHeaderEvent read FOnDrawFooter write FOnDrawFooter;
    property OnFilterChar: TOnFilterChar read FOnFilterChar write FOnFilterChar;
    property OnHeaderClick: TOnHeaderClick read FOnHeaderClick write FOnHeaderClick;
    property OnGutterClick: TOnGutterClick read FOnGutterClick write FOnGutterClick;
    property OnCellChange: TOnCellChange read FOnCellChange write FOnCellChange;
    property OnCellChanging: TOnCellChanging read FOnCellChanging write FOnCellChanging;
    property OnColRowChanged: TOnColRowChanged read FOnColRowChanged write FOnColRowChanged;
    property OnInsertRow: TOnRowEvent read FOnInsertRow write FOnInsertRow;
    property OnDeleteRow: TOnRowEvent read FOnDeleteRow write FOnDeleteRow;
    property OnCellAssignment: TOnCellAssignment read FOnCellAssignment write FOnCellAssignment;

    property Font;
    property Anchors;
    property Align;
    property BorderStyle default bsSingle;
    property BevelOuter default bvNone;
    property BevelInner;
    property TabOrder;
    property TabStop default True;
    property Tag;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnKeyPress;
    property OnKeyDown;
    property OnKeyUp;
    property PopupMenu;
  end;

  TplSmartGridSync = class(TplSmartGrid)
  private
    FGrid: TplSmartGrid;
    procedure SetGrid(const Value: TplSmartGrid);
    procedure SyncDeleteRow(Sender: TObject; ARow: integer);
    procedure SyncInsertRow(Sender: TObject; ARow: integer);
    procedure SyncColRow(Sender: TObject; ACol, ARow: integer);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetScrollBar(AKind, AMax, APos, AMask: integer); override;
    procedure ShowHideScrollBar(HorzVisible, VertVisible: boolean); override;
    property OnDeleteRow;
    property OnInsertRow;
    property OnColRowChanged;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Grid: TplSmartGrid read FGrid write SetGrid;
  end;

function DrawString(Canvas: TCanvas; Str: string; Rc: TRect; HorzAlign: THorzAlign; VertAlign: TVertAlign; IsEditing: boolean): TPoint;
procedure DrawStringMulti(Canvas: TCanvas; Str: string; Rc: TRect; HorzAlign: THorzAlign; VertAlign: TVertAlign);

implementation

{$R TplSmartGridCursors.res}

const
  crPlus = 101;
  crSmallCross = 102;
  crRight = 103;
  crDown = 104;
  crLeftTop = 105;

  CursorArray: array [TGridHitTest] of TCursor =
    (crDefault, crLeftTop, crRight, crDown, crPlus, crHSplit, crSmallCross);

  MergeID = -2;


//====================================================================

function DrawString(Canvas: TCanvas; Str: string; Rc: TRect; HorzAlign: THorzAlign; VertAlign: TVertAlign; IsEditing: boolean): TPoint;
var
  w, h, x, y: integer;
  rw: integer;
begin
  w := Canvas.TextWidth(Str);
  h := Canvas.TextHeight('gM');
  x := 0;
  y := 0;
  rw := Rc.Right - rc.Left;
  case HorzAlign of
    haLeft:
    begin
      x := Rc.Left;
      if (w > rw) and IsEditing then
        x := Rc.Left - (w - rw);
    end;
    haCenter: x := Rc.Left + ((rw - w) div 2);
    haRight: x := Rc.Right - w;
  end;
  case VertAlign of
    vaTop: y := Rc.Top;
    vaCenter: y := Rc.Top + (((Rc.Bottom - Rc.Top) - h) div 2);
    vaBottom: y := Rc.Bottom - h;
  end;
  Canvas.TextRect(Rc, x, y, Str);
  // Return next cursor position
  Result := Point(Min(x + w + 1, Rc.Right), Rc.Top - 1);
end;

procedure DrawStringMulti(Canvas: TCanvas; Str: string; Rc: TRect; HorzAlign: THorzAlign; VertAlign: TVertAlign);
var
  w, h, x, y: integer;
  t: TStringList;
  i: integer;
  dh: integer;

begin
  if Pos(';', Str) = 0 then
  begin
    DrawString(Canvas, Str, Rc, HorzAlign, VertAlign, False);
    Exit;
  end;

  t := TStringList.Create;
  t.Text := StringReplace(Str, ';', #13, [rfReplaceAll]);
  h := Canvas.TextHeight('gM');
  dh := Rc.Top + (((Rc.Bottom - Rc.Top) - (h * t.Count)) div 2);
  for i := 0 to t.Count - 1 do
  begin
    w := Canvas.TextWidth(t[i]);
    x := 0;
    y := 0;
    case HorzAlign of
      haLeft: x := Rc.Left;
      haCenter: x := Rc.Left + (((Rc.Right - Rc.Left) - w) div 2);
      haRight: x := Rc.Right - w;
    end;
    case VertAlign of
      vaTop: y := Rc.Top + (i * h);
      vaCenter: y := dh + (i * h);
      vaBottom: y := Rc.Bottom - (h * (t.Count - i));
    end;
    Canvas.TextRect(Rc, x, y, t[i]);
  end;
  t.Free;
end;

//==================== TplSmartGrid =============================================

constructor TplSmartGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 200;
  Height := 200;
  inherited Color := clWindow;
  BevelOuter := bvNone;
  BorderStyle := bsSingle;
  TabStop := True;
  TabOrder := 0;
  ParentColor := False;
  ParentFont := False;

  {$IFDEF VER150}
  ControlStyle := ControlStyle + [csNeedsBorderPaint];
  {$ENDIF}

  FFlat := True;
  FEnabled := True;
  FColor := clWindow;
  FAlternateColor := clWindow;
  FGridColor := clBtnFace;
  FShowGrid := True;
  FHeaderColor := clBtnface;
  FHeaderLightColor := clBtnHighlight;
  FHeaderDarkColor := clBtnShadow;
  FHeaderFont := TFont.Create;
  FHeaderFont.OnChange := @HeaderFontChange;
  FSelectionColor := $00CAFFFF;

  FFooterFont := TFont.Create;
  FFooterFont.OnChange := @FooterFontChange;

  FDefRowHeight := 18;
  FDefColWidth := 80;
  FRowCount := 5;
  FAutoAddRow := False;
  FGutterKind := gkBlank;
  FGutterWidth := 20;
  FGutterFont := TFont.Create;
  FGutterFont.OnChange := @GutterFontChange;

  FHorzOffset := 0;
  FVertOffset := 0;
  FMaxHScroll := 0;
  FMaxVScroll := 0;
  FSmallChange := FDefRowHeight;
  FLargeChange := FDefRowHeight * 5;
  ForcedColumn := -1;
  AllWidth := 200;
  AllHeight := 200;

  FHeaderLine := 1;
  FHeaderInfos := TList.Create;

  ValidationEnabled := True;
  CellBox := Rect(0, 0, 0, 0);
  FCol := 0;
  FRow := 0;
  FCol2 := 0;
  FRow2 := 0;
  FSelectArea := Rect(0, 0, 0, 0);
  IsEditing := False;
  BuffString := '';
  SmallBox := Rect(-1, -1, -1, -1);
  SmallBoxArea := Rect(-1, -1, -1, -1);
  SmallBoxPos := 0;
  SizingCol := -1;
  SizingColX := -1;

  Screen.Cursors[crPlus] := LoadCursor(hinstance, 'CR_PLUS');
  Screen.Cursors[crSmallCross] := LoadCursor(hInstance, 'CR_CROSS');
  Screen.Cursors[crRight] := LoadCursor(hinstance, 'CR_RIGHT');
  Screen.Cursors[crDown] := LoadCursor(hinstance, 'CR_DOWN');
  Screen.Cursors[crLeftTop] := LoadCursor(hinstance, 'CR_LEFTTOP');
  Cursor := crPlus;

  FColumns := TplSmartColumns.Create(Self);
  FEdit := TplSmartInplace.Create(Self);
  FGutterStrings := TStringList.Create;
  Mergeds := TList.Create;

end;

destructor TplSmartGrid.Destroy;
begin
  ClearMergeCells;
  Mergeds.Free;
  FGutterStrings.Free;
  FEdit.Free;
  FColumns.Free;
  ClearHeaderInfos;
  FHeaderInfos.Free;
  FHeaderFont.Free;
  FFooterFont.Free;
  FGutterFont.Free;
  inherited Destroy;
end;

procedure TplSmartGrid.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or WS_HSCROLL or WS_VSCROLL;
end;

procedure TplSmartGrid.CreateWnd;
begin
  inherited CreateWnd;
  ShowHideScrollBar(False, False);
  Recalculate;
end;

procedure TplSmartGrid.SetScrollBar(AKind, AMax, APos, AMask: integer);
var
  Info: TScrollInfo;
begin
  FillChar(Info, SizeOf(TScrollInfo), 0);
  Info.cbSize := SizeOf(TScrollInfo);
  Info.nMin := 0;
  Info.nMax := AMax;
  Info.nPos := APos;
  Info.fMask := AMask;
  SetScrollInfo(Handle, AKind, Info, True);
  if (AKind = SB_VERT) and Assigned(Sync) then
  begin
    if ((AMask and SIF_RANGE) <> 0) then
      Sync.FMaxVScroll := AMax;
    if ((AMask and SIF_POS) <> 0) then
      Sync.VertOffset := APos;
  end;
end;

procedure TplSmartGrid.ShowHideScrollBar(HorzVisible, VertVisible: boolean);
begin
  ShowScrollBar(Handle, SB_HORZ, HorzVisible);
  ShowScrollBar(Handle, SB_VERT, VertVisible);
end;

procedure TplSmartGrid.WMHScroll(var Msg: TWMVScroll);
var
  Old: integer;
begin
  ForceHideCaret;
  Old := FHorzOffset;

  case Msg.ScrollCode of
    SB_LINELEFT:
      FHorzOffset := FHorzOffset - FSmallChange;
    SB_LINERIGHT:
      FHorzOffset := FHorzOffset + FSmallChange;
    SB_PAGELEFT:
      FHorzOffset := FHorzOffset - FLargeChange;
    SB_PAGERIGHT:
      FHorzOffset := FHorzOffset + FLargeChange;
    SB_THUMBTRACK:
      FHorzOffset := Msg.Pos;
    SB_THUMBPOSITION:
      FHorzOffset := Msg.Pos;
  end;

  FHorzOffset := Max(0, Min(FMaxHScroll, FHorzOffset));

  if (FHorzOffset <> Old) then
  begin
    SetScrollBar(SB_HORZ, 0, FHorzOffset, SIF_POS);
    InvalidateRightWard(FixedWidth);
  end;
end;

procedure TplSmartGrid.WMVScroll(var Msg: TWMHScroll);
var
  Old: integer;
begin
  ForceHideCaret;
  Old := FVertOffset;

  case Msg.ScrollCode of
    SB_LINEUP:
      FVertOffset := FVertOffset - FSmallChange;
    SB_LINEDOWN:
      FVertOffset := FVertOffset + FSmallChange;
    SB_PAGEUP:
      FVertOffset := FVertOffset - FLargeChange;
    SB_PAGEDOWN:
      FVertOffset := FVertOffset + FLargeChange;
    SB_THUMBTRACK:
      FVertOffset := Msg.Pos;
    SB_THUMBPOSITION:
      FVertOffset := Msg.Pos;
  end;

  FVertOffset := Max(0, Min(FMaxVScroll, FVertOffset));
  NormalizeVertOffset;

  if (FVertOffset <> Old) then
  begin
    SetScrollBar(SB_VERT, 0, FVertOffset, SIF_POS);
    InvalidateDownWard(FixedHeight);
  end;
end;

procedure TplSmartGrid.SetColCount(Value: integer);
begin
  if (ColCount <> Value) then
  begin
    FColumns.BeginUpdate;
    while (ColCount > Value) do
      FColumns.Delete(FColumns.Count - 1);
    while (ColCount < Value) do
      FColumns.Add;
    FHorzOffset := 0;
    FVertOffset := 0;
    FCol := Max(0, Min(FCol, ColCount - 1));
    FRow := Max(0, Min(FRow, FRowCount - 1));
    if (FRowCount = 0) or (ColCount = 0) then
    begin
      FCol := -1;
      FRow := -1;
    end;
    FSelectArea := Rect(FCol, FRow, FCol, FRow);
    FColumns.EndUpdate;
    ColRowChanged;
  end;
end;

procedure TplSmartGrid.SetRowCount(Value: integer);
begin
  if (FRowCount <> Value) then
  begin
    FRowCount := Value;
    FCol := Max(0, Min(FCol, ColCount - 1));
    FRow := Max(0, Min(FRow, FRowCount - 1));
    if (FRowCount = 0) or (ColCount = 0) then
    begin
      FCol := -1;
      FRow := -1;
    end;
    FSelectArea := Rect(FCol, FRow, FCol, FRow);
    Recalculate;
    Invalidate;
    ColRowChanged;
  end;
end;

procedure TplSmartGrid.ClearHeaderInfos;
var
  x: integer;
  P: PHeaderInfo;
begin
  for x := 0 to FHeaderInfos.Count - 1 do
  begin
    P := PHeaderInfo(FHeaderInfos[x]);
    Dispose(P);
  end;
  FHeaderInfos.Clear;
end;

procedure TplSmartGrid.Recalculate;
var
  x: integer;
  HVisible, VVisible: boolean;
  VisCount: integer;
  WidthAvail, HeightAvail: integer;
  v: integer;
  LastBodyWidth: integer;

  function GetColAutoWidth(i: integer): integer;
  var
    n: integer;
    t: TStrings;
  begin
    Result := 0;
    t := Columns[i].FStrings;
    for n := 0 to t.Count - 1 do
      Result := Max(Result, Canvas.TextWidth(t[n]) + 7);
    Result := Max(Result, 20);
  end;

begin
  BuildMergeData;

  VisCount := 0;
  for x := 0 to FColumns.Count - 1 do
  begin
    if FColumns[x].FVisible then
      Inc(VisCount);
  end;

  if (VisCount = 0) then
  begin
    FixedHeight := 0;
    FixedWidth := 0;
    BodyWidth := 0;
    BodyHeight := 0;
    ShowHideScrollBar(False, False);
    Exit;
  end;

  if FAutoColWidth then
  begin
    Canvas.Font.Assign(Font);
    for x := 0 to FColumns.Count - 1 do
      FColumns[x].FWidth := Max(FDefColWidth, GetColAutoWidth(x));
  end;

  FixedWidth := 0;
  if (FGutterKind <> gkNone) then
    FixedWidth := FGutterWidth;

  FixedHeight := FHeaderLine * FDefRowHeight;
  BodyHeight := FRowCount * FDefRowHeight;

  WidthAvail := ClientWidth - FixedWidth;
  HeightAvail := ClientHeight - FixedHeight;
  if FShowFooter then
    HeightAvail := HeightAvail - FDefRowHeight;

  BodyWidth := 0;
  for x := 0 to FColumns.Count - 1 do
  begin
    if FColumns[x].FVisible then
      BodyWidth := BodyWidth + FColumns[x].FWidth;
  end;

  if FFitToWidth then
  begin
    if (BodyWidth < WidthAvail) then
    begin
      LastBodyWidth := BodyWidth;
      x := 0;
      while (BodyWidth < WidthAvail) do
      begin
        if (x > ColCount - 1) then
        begin
          if (BodyWidth = LastBodyWidth) then
            Break
          else
            x := 0;
        end;
        if FColumns[x].FVisible and FColumns[x].FCanResize then
        begin
          FColumns[x].FWidth := FColumns[x].FWidth + 1;
          Inc(BodyWidth);
        end;
        Inc(x);
      end;
    end;
    if (BodyWidth > WidthAvail) then
    begin
      LastBodyWidth := BodyWidth;
      x := 0;
      while (BodyWidth > WidthAvail) do
      begin
        if (x > ColCount - 1) then
        begin
          if (BodyWidth = LastBodyWidth) then
            Break
          else
            x := 0;
        end;
        if FColumns[x].FVisible and (x <> ForcedColumn) and FColumns[x].FCanResize then
        begin
          FColumns[x].FWidth := FColumns[x].FWidth - 1;
          Dec(BodyWidth);
        end;
        Inc(x);
      end;
    end;
    ForcedColumn := -1;
  end;

  if (BodyWidth < WidthAvail) then
    FHorzOffset := 0;

  if (BodyHeight < HeightAvail) then
    FVertOffset := 0;

  HVisible := BodyWidth > WidthAvail;
  VVisible := BodyHeight > HeightAvail;

  ShowHideScrollBar(HVisible, VVisible);

  FMaxHScroll := Max(0, BodyWidth - ClientWidth + FixedWidth);
  if FShowFooter then
    FMaxVScroll := Max(0, BodyHeight - ClientHeight + FixedHeight + FDefRowHeight)
  else
    FMaxVScroll := Max(0, BodyHeight - ClientHeight + FixedHeight);

  // Align to FDefRowHeight
  v := FMaxVScroll div FDefRowHeight;
  if (FMaxVScroll mod FDefRowHeight) > 0 then
    Inc(v);
  FMaxVScroll := v * FDefRowHeight;

  if FShowFooter then
  begin
    if VVisible then
      FooterTop := (((ClientHeight div FDefRowHeight) - 1) * FDefRowHeight) - 1
    else
      FooterTop := (FDefRowHeight * (FHeaderLine + FRowCount)) - 1;
  end;

  FHorzOffset := Max(0, Min(FHorzOffset, FMaxHScroll));
  FVertOffset := Max(0, Min(FVertOffset, FMaxVScroll));

  SetScrollBar(SB_HORZ, FMaxHScroll, FHorzOffset, SIF_POS or SIF_RANGE);
  SetScrollBar(SB_VERT, FMaxVScroll, FVertOffset, SIF_POS or SIF_RANGE);

  AllWidth := Min(ClientWidth, BodyWidth + FixedWidth);
  if FShowFooter then
  begin
    AllHeight := Min(ClientHeight, BodyHeight + FixedHeight + FDefRowHeight);
    CellBox := Rect(FixedWidth, FixedHeight, ClientWidth, FooterTop);
  end
  else
  begin
    AllHeight := Min(ClientHeight, BodyHeight + FixedHeight);
    CellBox := Rect(FixedWidth, FixedHeight, ClientWidth, ClientHeight);
  end;
end;

function TplSmartGrid.GetCellColor(X, Y: integer): TColor;
var
  cl: TColor;
  R: TRect;
begin
  cl := FColumns[x].Color;
  if Odd(Y) then
  begin
    if (cl = FColor) then
      cl := FAlternateColor;
  end;
  if FEnabled then
  begin
    with FSelectArea do
      R := Rect(Left, Top, Right + 1, Bottom + 1);
    if PtInRect(R, Point(X, Y)) then
    begin
      if not ((X = FCol) and (y = FRow)) then
        cl := FSelectionColor;
    end;
  end;
  Result := cl;
end;

procedure TplSmartGrid.DrawFixCell(Rc: TRect; Str: string; AFont: TFont; AEvent: TOnDrawHeaderEvent);
var
  Rt: TRect;
  Handled: boolean;
begin
  Handled := False;
  with Canvas do
  begin
    // Clear area
    if FFlat then
      Pen.Color := FHeaderDarkColor
    else
      Pen.Color := clBlack;
    Brush.Style := bsSolid;
    Brush.Color := FHeaderColor;
    Font.Assign(AFont);
    if not FEnabled then
      Font.Color := FHeaderDarkColor;
    if Assigned(AEvent) then
      AEvent(Self, Canvas, Rc, Str, Handled);
    if Handled then
      Exit;
    Rectangle(Rc);
    // Draw text immediately
    Brush.Style := bsClear;
    Rt := Rect(Rc.Left + 2, Rc.Top + 2, Rc.Right - 3, Rc.Bottom - 3);
    DrawStringMulti(Canvas, Str, Rt, haCenter, vaCenter);
    // cosmetics
    Pen.Color := FHeaderLightColor;
    MoveTo(Rc.Left + 1, Rc.Bottom - 2);
    LineTo(Rc.Left + 1, Rc.Top + 1);
    LineTo(Rc.Right - 1, Rc.Top + 1);
    if not FFlat then
    begin
      Pen.Color := FHeaderDarkColor;
      MoveTo(Rc.Right - 2, Rc.Top + 1);
      LineTo(Rc.Right - 2, Rc.Bottom - 2);
      LineTo(Rc.Left, Rc.Bottom - 2);
    end;
  end;
end;

procedure TplSmartGrid.RenderGutter;
const
  ArrowWidth = 8;
var
  x: integer;
  R, Dummy: TRect;
  Str: string;
  l, t, m: integer;
  GutterBox: TRect;
begin
  if (FGutterKind = gkNone) then
    Exit;
  CopyRect(GutterBox, CellBox);
  GutterBox.Left := 0;
  for x := 0 to FRowCount - 1 do
  begin
    R := Rect(-1, (x * FDefRowHeight) - 1, FGutterWidth, ((x + 1) * FDefRowHeight));
    OffsetRect(R, 0, -FVertOffset + FixedHeight);
    if IntersectRect(Dummy, R, GutterBox) then
    begin
      case FGutterKind of
        gkBlank, gkPointer:
          Str := '';
        gkNumber:
          Str := IntToStr(x + 1);
        gkString:
          if (x > FGutterStrings.Count - 1) then
            Str := ''
          else
            Str := FGutterStrings[x];
      end;
      DrawFixCell(R, Str, FGutterFont, FOnDrawGutter);
      // Draw pointer triangle
      if (FGutterKind = gkpointer) and (x = FRow) then
      begin
        with Canvas do
        begin
          l := (FGutterWidth - ArrowWidth) div 2;
          t := (FDefRowHeight - ArrowWidth) div 2;
          m := R.Top + (FDefRowHeight div 2);
          Pen.Color := FHeaderDarkColor;
          MoveTo(l, R.Bottom - t);
          LineTo(l, R.Top + t);
          LineTo(l + ArrowWidth, m);
          Pen.Color := FHeaderLightColor;
          LineTo(l, R.Bottom - t);
        end;
      end;
    end;
  end;
end;

procedure TplSmartgrid.RenderHeader;
var
  x: integer;
  R, Dummy: TRect;
  P: PHeaderInfo;
begin
  Canvas.Font.Assign(FHeaderFont);
  for x := 0 to FHeaderInfos.Count - 1 do
  begin
    P := PHeaderInfo(FHeaderInfos[x]);
    R := Rect(GetColCoord(P^.Rc.Left) - 1, (FDefRowHeight * P^.Rc.Top) - 1,
      GetColCoord(P^.Rc.Right + 1), FDefRowHeight * (P^.Rc.Bottom + 1));
    OffsetRect(R, -FHorzOffset + FixedWidth, 0);
    if IntersectRect(Dummy, R, ClientRect) then
      DrawFixCell(R, P^.Str, FHeaderFont, FOnDrawHeader);
  end;
  R := Rect(-1, -1, FixedWidth, FixedHeight);
  DrawFixCell(R, '', FHeaderFont, FOnDrawHeader);
end;

procedure TplSmartGrid.RenderFooter;
var
  x: integer;
  R, Dummy: TRect;
  FooterBottom: integer;
  Right: integer;
begin
  Canvas.Font.Assign(FFooterFont);
  FooterBottom := FooterTop + FDefRowHeight + 1;
  for x := 0 to FColumns.Count - 1 do
  begin
    R := Rect(GetColCoord(x) - 1, FooterTop, GetColCoord(x + 1), FooterBottom);
    OffsetRect(R, -FHorzOffset + FixedWidth, 0);
    if IntersectRect(Dummy, R, ClientRect) then
      DrawFixCell(R, FColumns[x].FFooter, FFooterFont, FOnDrawFooter);
  end;
  R := Rect(-1, FooterTop, FixedWidth, FooterBottom);
  DrawFixCell(R, '', FFooterFont, FOnDrawFooter);
  Right := Min(AllWidth, ClientWidth);
  R := Rect(-1, FooterBottom - 1, Right, ClientHeight);
  DrawFixCell(R, '', FFooterFont, FOnDrawFooter);
end;

procedure TplSmartGrid.DrawCell(X, Y: integer);
var
  R, Rc, Dummy: TRect;
  Column: TplSmartColumn;
  Handled: boolean;
begin
  Handled := False;
  Rc := GetCellRect(x, y);
  OffsetRect(Rc, -FHorzOffset + FixedWidth, -FVertOffset + FixedHeight);
  R := Rc;
  if IntersectRect(Dummy, Rc, CellBox) then
  begin
    Column := FColumns[x];
    with Canvas do
    begin
      Font.Assign(Column.Font);
      if not FEnabled then
        Font.Color := FGridColor;
      Pen.Color := FGridColor;
      Brush.Color := GetCellColor(X, Y);

      if Assigned(FOnDrawCell) then
        FOnDrawCell(Self, Canvas, X, Y, Rc, Handled);

      if not Handled then
      begin
        Brush.Style := bsSolid;
        if FShowGrid then
          Rectangle(Rc)
        else
          FillRect(Rc);
        Brush.Style := bsClear;
        InflateRect(Rc, -4, -2);
        DrawString(Canvas, SafeGetCell(x, y), Rc, Column.HorzAlign,
          Column.VertAlign, False);
      end;

    end;
  end;
end;

function TplSmartGrid.FastDrawCell(X, Y: integer; aIsEditing: boolean): TPoint;
var
  R, Dummy: TRect;
  Handled: boolean;
  Column: TplSmartColumn;
begin
  Handled := False;
  Result := Point(-1, -1);
  R := GetCellRect(x, y);
  OffsetRect(R, -FHorzOffset + FixedWidth, -FVertOffset + FixedHeight);
  if IntersectRect(Dummy, R, CellBox) then
  begin
    Column := FColumns[x];
    with Canvas do
    begin
      Brush.Color := GetCellColor(X, Y);
      Font.Assign(Column.Font);
    end;
    if Assigned(FOnDrawCell) then
      FOnDrawCell(Self, Canvas, X, Y, R, Handled);
    if not Handled then
    begin
      with Canvas do
      begin
        Brush.Style := bsSolid;
        InflateRect(R, -4, -2);
        FillRect(R);
        Brush.Style := bsClear;
      end;
      Result := DrawString(Canvas, SafeGetCell(x, y), R, Column.HorzAlign, Column.VertAlign, IsEditing);
    end;
  end;
end;

procedure TplSmartGrid.DrawSelection;
var
  R, R1, R2: TRect;
  HOffset, VOffset: integer;

begin

  if (FCol = -1) or (FRow = -1) then
    Exit;

  HOffset := -FHorzOffset + FixedWidth;
  VOffset := -FVertOffset + FixedHeight;

  R1 := GetCellRect(FSelectArea.Left, FSelectArea.Top);
  R2 := GetCellRect(FSelectArea.Right, FSelectArea.Bottom);
  R := Rect(R1.Left, R1.Top, R2.Right, R2.Bottom);
  OffsetRect(R, HOffset, VOffset);

  with Canvas do
  begin

    if Focused then
      Pen.Color := clBlack
    else
      Pen.Color := FGridColor;

    Pen.Width := 3;
    Brush.Style := bsClear;
    Rectangle(R);

    Pen.Width := 1;
    Brush.Style := bsSolid;
    if Focused then
      Brush.Color := clBlack
    else
      Brush.Color := FGridColor;
    Pen.Color := clWhite;

    case SmallBoxPos of
      0: SmallBox := Rect(R.Right - 3, R.Bottom - 3, R.Right + 3, R.Bottom + 3);
      1: SmallBox := Rect(R.Right - 3, R.Top - 3 + 5, R.Right + 3, R.Top + 3 + 5);
      2: SmallBox := Rect(R.Left - 3 + 5, R.Bottom - 3, R.Left + 3 + 5, R.Bottom + 3);
    end;

    Rectangle(SmallBox);
    SmallBoxPos := 0;  // Reset to Right Bottom

  end;

  if (SmallBoxArea.Left <> -1) then
  begin
    R1 := GetCellRect(SmallBoxArea.Left, SmallBoxArea.Top);
    OffsetRect(R1, HOffset, VOffset);
    R2 := GetCellRect(SmallBoxArea.Right, SmallBoxArea.Bottom);
    OffsetRect(R2, HOffset, VOffset);
    R := Rect(R1.Left, R1.Top, R2.Right, R2.Bottom);

    with Canvas do
    begin
      Pen.Color := clBlack;
      Pen.Width := 1;
      Pen.Style := psDot;
      Brush.Style := bsClear;
      Rectangle(R);
      Pen.Style := psSolid;
      Pen.Width := 1;
    end;

  end;

end;

procedure TplSmartGrid.ClearUnused;
var
  t: integer;
begin
  if (AllWidth < ClientWidth) then
  begin
    with Canvas do
    begin
      Brush.Style := bsSolid;
      Brush.Color := FColor;
      FillRect(Rect(AllWidth, 0, ClientWidth, ClientHeight));
    end;
  end;
  if FShowFooter then
    Exit;
  if (AllHeight < ClientHeight) then
  begin
    with Canvas do
    begin
      Brush.Style := bsSolid;
      Brush.Color := FColor;
      FillRect(Rect(0, AllHeight, ClientWidth, ClientHeight));
    end;
  end;
  if ((FMaxVScroll - FVertOffset) < FDefRowHeight) then
  begin
    with Canvas do
    begin
      Brush.Style := bsSolid;
      Brush.Color := FColor;
      t := FixedHeight + (((ClientHeight - FixedHeight) div FDefRowHeight) * FDefRowHeight);
      FillRect(Rect(0, t, ClientWidth, ClientHeight));
    end;
  end;
end;

procedure TplSmartGrid.Paint;
var
  x, y: integer;
  RgnInv, RgnAll, RgnBody, RgnSel, Temp: HRGN;
  HOffset, VOffset: integer;
  R, R1, R2: TRect;

begin

  if FUpdating then
    Exit;
  if not HandleAllocated then
    Exit;

  if (ColCount = 0) then
  begin
    with Canvas do
    begin
      Brush.Style := bsSolid;
      Brush.Color := FColor;
      FillRect(Rect(0, 0, ClientWidth, ClientHeight));
    end;
    Exit;
  end;

  if (FRowCount > 0) then
  begin

    // Calculating area that will be covered by selection rectangle
    HOffset := -FHorzOffset + FixedWidth;
    VOffset := -FVertOffset + FixedHeight;
    R1 := GetCellRect(FSelectArea.Left, FSelectArea.Top);
    R2 := GetCellRect(FSelectArea.Right, FSelectArea.Bottom);
    R := Rect(R1.Left, R1.Top, R2.Right, R2.Bottom);
    OffsetRect(R, HOffset, VOffset);

    // Creating region, excluding selection rectangle to reduce flicker
    RgnSel := CreateRectRgn(R.Left - 1, R.Top - 1, R.Right + 1, R.Bottom + 1);
    Temp := CreateRectRgn(R.Left + 2, R.Top + 2, R.Right - 2, R.Bottom - 2);
    CombineRgn(RgnSel, RgnSel, Temp, RGN_XOR);

    if FShowFooter then
      RgnInv := CreateRectRgn(FixedWidth, FixedHeight, ClientWidth, FooterTop)
    else
      RgnInv := CreateRectRgn(FixedWidth, FixedHeight, ClientWidth, ClientHeight);
    if FEnabled then
      CombineRgn(RgnInv, RgnInv, RgnSel, RGN_DIFF);
    SelectClipRgn(Canvas.Handle, RgnInv);

    for x := 0 to ColCount - 1 do
    begin
      if FColumns[x].FVisible then
      begin
        for y := 0 to FRowCount - 1 do
        begin
          if (ptrint(GetObject(x, y)) <> MergeID) then
            DrawCell(X, Y);
        end;
      end;
    end;

    for x := 0 to Mergeds.Count - 1 do
      DrawMergedCell(x);

    RgnAll := CreateRectRgn(0, 0, ClientWidth, ClientHeight);
    if FEnabled then
      CombineRgn(RgnAll, RgnAll, RgnSel, RGN_DIFF);
    SelectClipRgn(Canvas.Handle, RgnAll);
    ClearUnused;

    if FShowFooter then
      RgnBody := CreateRectRgn(FixedWidth, FixedHeight, ClientWidth, FooterTop)
    else
      RgnBody := CreateRectRgn(FixedWidth, FixedHeight, ClientWidth, ClientHeight);
    SelectClipRgn(Canvas.Handle, RgnBody);
    if FEnabled then
      DrawSelection;

    SelectClipRgn(Canvas.Handle, 0);

    DeleteObject(RgnInv);
    DeleteObject(RgnAll);
    DeleteObject(RgnBody);
    DeleteObject(RgnSel);
    DeleteObject(Temp);

  end
  else

    ClearUnused;

  RenderGutter;
  RenderHeader;
  if FShowFooter then
    RenderFooter;

end;

procedure TplSmartGrid.UpdateHeader;
var
  P: PHeaderInfo;
  x, y: integer;
  t: TStringList;
  s: string;
  LastX: TList;
  LastY: PHeaderInfo;
  Blank: PHeaderInfo;

begin

  ClearHeaderInfos;

  LastX := TList.Create;
  t := TStringList.Create;

  Blank := New(PHeaderInfo);
  Blank^.Str := '^%%%%%^******^';

  while (LastX.Count < FHeaderLine) do
    LastX.Add(Blank);

  P := nil;
  for x := 0 to FColumns.Count - 1 do
  begin
    if not FColumns[x].FVisible then
    begin
      for y := 0 to FHeaderLine - 1 do
        LastX[y] := Blank;
      Continue;
    end;
    t.Text := StringReplace(FColumns[x].Title, '|', #13, [rfReplaceAll]);
    while (t.Count < FHeaderLine) do
    begin
      if (t.Count = 0) then
        t.Add('')
      else
        t.Add(t[t.Count - 1]);
    end;
    LastY := Blank;
    for y := 0 to FHeaderLine - 1 do
    begin
      s := t[y];
      if (s = LastY^.Str) then
      begin
        LastY^.Rc.Bottom := Min(FHeaderLine - 1, Max(LastY^.Rc.Bottom, y));
      end
      else
      begin
        if (s = PHeaderInfo(LastX[y])^.Str) then
        begin
          P := PHeaderInfo(LastX[y]);
          P^.Rc.Right := P^.Rc.Right + 1;
        end
        else
        begin
          P := New(PHeaderInfo);
          P^.Rc := Rect(x, y, x, y);
          P^.Str := s;
          FHeaderInfos.Add(P);
        end;
        LastX[y] := P;
      end;
      LastY := P;
    end;
  end;

  LastX.Free;
  t.Free;
  Dispose(Blank);

  Recalculate;
end;

function TplSmartGrid.GetColCoord(I: integer): integer;
var
  x: integer;
  Column: TplSmartColumn;
begin
  Result := 0;
  for x := 0 to I - 1 do
  begin
    Column := FColumns[x];
    if Column.FVisible then
      Result := Result + Column.FWidth;
  end;
end;

function TplSmartGrid.GetCellRect(x, y: integer): TRect;
var
  l, t, w, h: integer;
begin
  if (x = -1) or (y = -1) then
  begin
    Result := Rect(0, 0, 0, 0);
    Exit;
  end;
  l := GetColCoord(x);
  t := FDefRowheight * y;
  w := 0;
  if (FColumns[x].FVisible) then
    w := FColumns[x].FWidth;
  h := FDefRowHeight;
  Result := Rect(l - 1, t - 1, l + w, t + h);
end;

function TplSmartGrid.CellRectToClient(R: TRect): TRect;
begin
  Result := R;
  OffsetRect(Result, -FHorzOffset + FixedWidth, -FVertOffset + FixedHeight);
end;

function TplSmartGrid.GetCellAtPos(X, Y: integer): TPoint;
var
  ax, ay: integer;
begin
  ax := (FHorzOffset + X) - FixedWidth;
  ay := (FVertOffset + Y) - FixedHeight;
  Result.X := 0;
  while (GetColCoord(Result.X) < ax) do
  begin
    Result.X := Result.X + 1;
    if (Result.X > FColumns.Count - 1) then
      Break;
  end;
  Result.X := Max(0, Result.X - 1);
  Result.Y := Max(0, Min(ay div FDefRowHeight, FRowCount - 1));
end;

function TplSmartGrid.GetColFromX(X: integer): integer;
var
  ax: integer;
begin
  if (X < FixedWidth) then
  begin
    Result := -1;
    Exit;
  end;
  Result := 0;
  ax := (FHorzOffset + X) - FixedWidth;
  while (GetColCoord(Result) < ax) do
  begin
    Result := Result + 1;
    if (Result > FColumns.Count - 1) then
      Break;
  end;
  Result := Result - 1;
  if (Result > FColumns.Count - 1) or (Result < 0) then
    Result := -1;
end;

function TplSmartGrid.GetRowFromY(Y: integer): integer;
var
  ay: integer;
begin
  if (Y < FixedHeight) then
  begin
    Result := -1;
    Exit;
  end;
  ay := (FVertOffset + Y) - FixedHeight;
  Result := ay div FDefRowHeight;
  if (Result > FRowCount - 1) then
    Result := -1;
end;

function TplSmartGrid.SafeGetCell(X, Y: integer): string;
var
  t: TStringList;
begin
  Result := '';
  t := TStringList(Columns[X].FStrings);
  if (Y < t.Count) then
    Result := t[Y];
end;

function TplSmartGrid.GetCell(X, Y: integer): string;
var
  t: TStrings;
begin
  Result := '';
  if (X > ColCount - 1) or (Y > FRowCount - 1) then
    raise Exception.Create('Cell Index out of bound.');
  t := Columns[X].FStrings;
  if (Y < t.Count) then
    Result := t[Y];
end;

procedure TplSmartGrid.InternalSetCell(X, Y: integer; Value: string; FireOnChange: boolean);
var
  t: TStringList;
  s: string;
  CanChange: boolean;
begin
  if (ColCount = 0) or (FRowCount = 0) then
    Exit;
  if FireOnChange and FColumns[X].FReadOnly then
    Exit;
  if (X > ColCount - 1) or (Y > FRowCount - 1) then
    raise Exception.Create('Cell Index out of bound.');
  t := TStringList(FColumns[X].FStrings);
  while (Y > t.Count - 1) do
    t.Add('');
  if (t[Y] = Value) then
    Exit;
  if FireOnChange then
  begin
    s := Value;
    CanChange := True;
    if Assigned(FOnCellChanging) then
      FOnCellChanging(Self, X, Y, CanChange);
    if not CanChange then
      Exit;
    if Assigned(FOnCellChange) then
      FOnCellChange(Self, X, Y, s);
    t[Y] := s;
  end
  else
    t[Y] := Value;
  if not FUpdating then
    FastDrawCell(X, Y, False);
end;

procedure TplSmartGrid.SetCell(X, Y: integer; Value: string);
begin
  InternalSetCell(X, Y, Value, False);
end;

procedure TplSmartGrid.BeginUpdate;
begin
  FUpdating := True;
  ForceHideCaret;
end;

procedure TplSmartGrid.EndUpdate;
begin
  FUpdating := False;
  UpdateHeader;
  Invalidate;
end;

procedure TplSmartGrid.SetFlat(Value: boolean);
begin
  if (FFlat <> Value) then
  begin
    FFlat := Value;
    Invalidate;
  end;
end;

procedure TplSmartGrid.SetColor(Value: TColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;
    inherited Color := Value;
    Invalidate;
  end;
end;

procedure TplSmartGrid.SetAlternateColor(Value: TColor);
begin
  if (FAlternateColor <> Value) then
  begin
    FAlternateColor := Value;
    InvalidateCells;
  end;
end;

procedure TplSmartGrid.SetGridColor(Value: TColor);
begin
  if (FGridColor <> Value) then
  begin
    FGridColor := Value;
    InvalidateCells;
  end;
end;

function TplSmartGrid.GetColWidths(Index: integer): integer;
begin
  Result := FColumns[Index].FWidth;
end;

procedure TplSmartGrid.SetColWidths(Index, Value: integer);
begin
  if not FAutoColWidth then
  begin
    if (ColWidths[Index] <> Value) then
      FColumns[Index].Width := Value;
  end;
end;

procedure TplSmartGrid.SetAutoColWidth(Value: boolean);
begin
  if (FAutoColWidth <> Value) then
  begin
    FAutoColWidth := Value;
    Recalculate;
    Invalidate;
  end;
end;

procedure TplSmartGrid.SetDefColWidth(Value: integer);
begin
  if (FDefColWidth <> Value) then
  begin
    FDefColWidth := Value;
    if not FAutoColWidth then
    begin
      Recalculate;
      Invalidate;
    end;
  end;
end;

procedure TplSmartGrid.SetDefRowHeight(Value: integer);
begin
  if (FDefRowHeight <> Value) then
  begin
    FDefRowHeight := Value;
    FSmallChange := Value;
    FLargeChange := Value * 5;
    Recalculate;
    Invalidate;
  end;
end;

procedure TplSmartGrid.SetFitToWidth(Value: boolean);
begin
  if (FFitToWidth <> Value) then
  begin
    FFitToWidth := Value;
    FHorzOffset := 0;
    Recalculate;
    Invalidate;
  end;
end;

procedure TplSmartGrid.SetHeaderColor(Value: TColor);
begin
  if (FHeaderColor <> Value) then
  begin
    FHeaderColor := Value;
    Invalidate;
  end;
end;

procedure TplSmartGrid.SetHeaderDarkColor(Value: TColor);
begin
  if (FHeaderDarkColor <> Value) then
  begin
    FHeaderDarkColor := Value;
    Invalidate;
  end;
end;

procedure TplSmartGrid.SetHeaderLightColor(Value: TColor);
begin
  if (FHeaderLightColor <> Value) then
  begin
    FHeaderLightColor := Value;
    Invalidate;
  end;
end;

procedure TplSmartGrid.SetHeaderLine(Value: integer);
begin
  if (FHeaderLine <> Value) then
  begin
    FHeaderLine := Value;
    UpdateHeader;
    Invalidate;
  end;
end;

procedure TplSmartGrid.SetSelectionColor(Value: TColor);
begin
  if (FSelectionColor <> Value) then
  begin
    FSelectionColor := Value;
    InvalidateCells;
  end;
end;

procedure TplSmartGrid.KeyDown(var Key: word; Shift: TShiftState);
var
  l, t, r, b: integer;
  x, y: integer;
  Empty: boolean;
  Str: string;
  FillDown: boolean;
  FillRight: boolean;
  Old: integer;
  OldS: string;

  procedure UpdateColRow;
  begin
    ForceHideCaret;
    FUpdating := True;
    BuffString := '';
    FCol2 := FCol;
    FRow2 := FRow;
    EnsureVisible(FCol, FRow);
    FUpdating := False;
    SetSelectArea(Rect(FCol, FRow, FCol, FRow));
    ColRowChanged;
  end;

  procedure UpdateSelectArea;
  begin
    l := Min(FCol2, FCol);
    t := Min(FRow2, FRow);
    r := Max(FCol2, FCol);
    b := Max(FRow2, FRow);
    SetSelectArea(Rect(l, t, r, b));
    EnsureVisible(FCol2, FRow2);
  end;

begin

  if not FEnabled then
    Exit;

  if (ColCount = 0) or (FRowCount = 0) then
    Exit;

  if (ssCtrl in Shift) then
  begin

    case Key of

      Ord('X'), Ord('x'):
        if not FReadOnly then
          CutToClipboard;

      Ord('C'), Ord('c'):
        CopyToClipboard;

      Ord('V'), Ord('v'):
        if not FReadOnly then
          PasteFromClipboard;

      VK_HOME:
      begin
        FCol := GetFirstVisible;
        FRow := 0;
        UpdateColRow;
      end;

      VK_END:
      begin
        FCol := GetLastVisible;
        FRow := FRowCount - 1;
        UpdateColRow;
      end;

      VK_DELETE:
      begin
        if not FReadOnly and (FRowCount > 1) then
        begin
          Old := FRow;
          DeleteRow(FRow);
          if Assigned(FOnDeleteRow) then
            FOnDeleteRow(Self, Old);
          UpdateColRow;
        end;
      end;

    end;

  end
  else

  if (ssShift in Shift) then
  begin

    case Key of

      VK_LEFT:
      begin
        FCol2 := Max(GetPrevVisible(FCol2), GetFirstVisible);
        UpdateSelectArea;
      end;

      VK_RIGHT:
      begin
        FCol2 := Min(GetNextVisible(FCol2), GetLastVisible);
        UpdateSelectArea;
      end;

      VK_UP:
      begin
        FRow2 := Max(FRow2 - 1, 0);
        UpdateSelectArea;
      end;

      VK_DOWN:
      begin
        FRow2 := Min(FRow2 + 1, FRowCount - 1);
        UpdateSelectArea;
      end;

      VK_RETURN:
        if (FSelectArea.Left = FSelectArea.Right) and (FSelectArea.Top = FSelectArea.Bottom) then
        begin
          FRow := Max(0, FRow - 1);
          UpdateColRow;
        end
        else
        begin
          if (FCol = FSelectArea.Left) and (FRow = FSelectArea.Top) then
          begin
            FCol := FSelectArea.Right;
            FRow := FSelectArea.Bottom;
          end
          else
          if (FRow = FSelectArea.Top) then
          begin
            FCol := FCol - 1;
            FRow := FSelectArea.Bottom;
          end
          else
          begin
            FRow := Row - 1;
          end;
          ForceHideCaret;
          BuffString := '';
          EnsureVisible(FCol, FRow);
          InvalidateCells;
          ColRowChanged;
        end;

    end;

  end
  else
  begin

    case Key of

      VK_HOME:
      begin
        FCol := GetFirstVisible;
        UpdateColRow;
      end;

      VK_END:
      begin
        FCol := GetLastVisible;
        UpdateColRow;
      end;

      VK_PRIOR:
      begin
        FRow := 0;
        UpdateColRow;
      end;

      VK_NEXT:
      begin
        FRow := FRowCount - 1;
        UpdateColRow;
      end;

      VK_LEFT:
      begin
        FCol := Max(GetPrevVisible(FCol), GetFirstVisible);
        UpdateColRow;
      end;

      VK_RIGHT:
      begin
        FCol := Min(GetNextVisible(FCol), GetLastVisible);
        UpdateColRow;
      end;

      VK_UP:
      begin
        if FAutoAddRow and (FRow = (FRowCount - 1)) and (FRow > 0) and not FReadOnly then
        begin
          Empty := True;
          for x := 0 to ColCount - 1 do
          begin
            if (SafeGetCell(x, FRowCount - 1) <> '') then
            begin
              Empty := False;
              Break;
            end;
          end;
          if Empty then
          begin
            RowCount := RowCount - 1;
            FRow := FRowCount - 1;
            if Assigned(FOnDeleteRow) then
              FOnDeleteRow(Self, FRowCount);
          end
          else
            FRow := Max(0, FRow - 1);
        end
        else
          FRow := Max(0, FRow - 1);
        UpdateColRow;
      end;

      VK_DOWN:
      begin
        if FAutoAddRow and (FRow = (FRowCount - 1)) and not FReadOnly then
        begin
          Inc(FRow);
          RowCount := RowCount + 1;
          if Assigned(FOnInsertRow) then
            FOnInsertRow(Self, FRow);
        end
        else
          FRow := Min(FRowCount - 1, FRow + 1);
        UpdateColRow;
      end;

      VK_RETURN:
      begin
        OldS := GetCell(Col, Row);
        Str := OldS;

        if Assigned(FOnCellAssignment) then
          FOnCellAssignment(Self, Col, Row, Str);

        if (Str <> Olds) then
          InternalSetCell(Col, Row, Str, True);

        FillDown := FAutoFillDown and (Copy(Str, Length(Str), 1) = '*');
        FillRight := FAutoFillRight and (Copy(Str, Length(Str), 1) = '>');

        if (FSelectArea.Left = FSelectArea.Right) and (FSelectArea.Top = FSelectArea.Bottom) then
        begin
          if FillDown then
          begin
            BuffString := '';
            ForceHideCaret;
            Str := Copy(Str, 1, Length(Str) - 1);
            for y := Row to FRowCount - 1 do
              Cells[Col, y] := Str;
          end
          else
          if FillRight then
          begin
            BuffString := '';
            ForceHideCaret;
            Str := Copy(Str, 1, Length(Str) - 1);
            for x := Col to ColCount - 1 do
              Cells[x, Row] := Str;
          end
          else
          begin
            FRow := Min(FRowCount - 1, FRow + 1);
            UpdateColRow;
          end;
        end
        else
        begin
          if FillDown then
          begin
            BuffString := '';
            ForceHideCaret;
            Str := Copy(Str, 1, Length(Str) - 1);
            for y := Row to FSelectArea.Bottom do
              Cells[Col, y] := Str;
          end
          else
          if FillRight then
          begin
            BuffString := '';
            ForceHideCaret;
            Str := Copy(Str, 1, Length(Str) - 1);
            for x := Col to FSelectArea.Right do
              Cells[x, Row] := Str;
          end
          else
          begin
            if (FCol = FSelectArea.Right) and (FRow = FSelectArea.Bottom) then
            begin
              FCol := FSelectArea.Left;
              FRow := FSelectArea.Top;
            end
            else
            if (FRow = FSelectArea.Bottom) then
            begin
              FCol := FCol + 1;
              FRow := FSelectArea.Top;
            end
            else
            begin
              FRow := Row + 1;
            end;
            ForceHideCaret;
            BuffString := '';
            EnsureVisible(FCol, FRow);
            InvalidateCells;
            ColRowChanged;
          end;
        end;
      end;

      VK_DELETE:
      begin
        if (BuffString = '') then
        begin
          if not FReadOnly then
          begin
            FUpdating := True;
            for x := SelectArea.Left to SelectArea.Right do
            begin
              for y := SelectArea.Top to SelectArea.Bottom do
                InternalSetCell(X, Y, '', True);
            end;
            FUpdating := False;
            InvalidateCells;
          end;
        end;
      end;

      VK_INSERT:
      begin
        if not FReadOnly then
        begin
          InsertRow(Max(0, FRow));
          if Assigned(FOnInsertRow) then
            FOnInsertRow(Self, FRow);
          UpdateColRow;
        end;
      end;

    end;

  end;

  inherited;

end;

procedure TplSmartGrid.KeyPress(var Key: char);
var
  Pt: TPoint;
  Allowed: boolean;

begin

  inherited;

  if not FEnabled then
    Exit;

  if (ColCount = 0) or (FRowCount = 0) then
    Exit;

  if not FReadOnly then
  begin

    case Key of

      Chr(VK_BACK):
      begin
        ForceHideCaret;
        BuffString := Copy(BuffString, 1, Length(BuffString) - 1);
        InternalSetCell(FCol, FRow, BuffString, True);
        EnsureVisible(FCol, FRow);
        Pt := FastDrawCell(FCol, FRow, True);
        SetCaretPos(Pt.X, Pt.Y);
        ForceShowCaret;
      end;

      Chr($20)..Chr($FF):
      begin
        Allowed := True;
        if Assigned(FOnFilterChar) then
          FOnFilterChar(Self, FCol, FRow, Key, Allowed);
        if Allowed then
        begin
          ForceHideCaret;
          BuffString := BuffString + Key;
          InternalSetCell(FCol, FRow, BuffString, True);
          EnsureVisible(FCol, FRow);
          Pt := FastDrawCell(FCol, FRow, True);
          SetCaretPos(Pt.X, Pt.Y);
          ForceShowCaret;
        end;

      end;

    end;

  end;

end;

function TplSmartGrid.GetHitTestInfo(X, Y: integer): TGridHitTest;
var
  a, i1, i2: integer;
  ax, ay: integer;
  IsSizing: boolean;

begin
  Result := gtNone;
  IsSizing := False;

  ax := (FHorzOffset + X) - FixedWidth;
  ay := (FVertOffset + Y) - FixedHeight;

  if not FAutoColWidth then
  begin
    for a := 1 to ColCount do
    begin
      i1 := GetColCoord(a);
      i2 := X + FHorzOffset - FixedWidth;
      if (i2 > (i1 - 2)) and (i2 < (i1 + 2)) then
      begin
        SizingCol := a - 1;
        IsSizing := FColumns[SizingCol].FCanResize;
        Break;
      end;
    end;
  end;

  if PtInRect(SmallBox, Point(X, Y)) then
    Result := gtSmallBox
  else
  if IsSizing then
    Result := gtColSizing
  else
  if ((X < FixedWidth) and (Y < FixedHeight)) then
    Result := gtLeftTop
  else
  if ((X < FixedWidth) and (Y > FixedHeight) and (ay < BodyHeight)) then
    Result := gtLeft
  else
  if ((Y < FixedHeight) and (X > FixedWidth) and (ax < BodyWidth)) then
    Result := gtTop
  else
  if ((X > FixedWidth) and (Y > FixedHeight) and (ax < BodyWidth) and (ay < BodyHeight)) then
    Result := gtCell;

end;

procedure TplSmartGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  Pt: TPoint;

begin

  if not FEnabled then
  begin
    inherited;
    Exit;
  end;

  if (Cursor = crHSplit) then
  begin
    ForceHideCaret;
    SizingColX := GetColCoord(SizingCol);
  end
  else

  if (Cursor = crSmallCross) then
  begin
    ForceHideCaret;
    SmallBoxArea := FSelectArea;
  end
  else

  if (Cursor = crLeftTop) then
  begin
    FRow := 0;
    FCol := 0;
    BuffString := '';
    EnsureVisible(0, 0);
    FCol2 := ColCount - 1;
    FRow2 := FRowCount - 1;
    SetSelectArea(Rect(0, 0, ColCount - 1, FRowCount - 1));
    ColRowChanged;
  end
  else

  if (Cursor = crRight) then
  begin
    FRow := GetRowFromY(Y);
    FCol := 0;
    LastHover := FRow;
    BuffString := '';
    EnsureVisible(FCol, FRow);
    FCol2 := ColCount - 1;
    FRow2 := FRow;
    SmallBoxPos := 2;
    AdjustSelection(Rect(0, FRow, ColCount - 1, FRow), True);
    ColRowChanged;
    if Assigned(OnGutterClick) then
      FOnGutterClick(Self, FRow, Button, Shift);
  end
  else

  if (Cursor = crDown) then
  begin
    FCol := GetColFromX(X);
    FRow := 0;
    LastHover := FCol;
    BuffString := '';
    EnsureVisible(FCol, FRow);
    FCol2 := FCol;
    FRow2 := FRowCount - 1;
    SmallBoxPos := 1;
    AdjustSelection(Rect(FCol, 0, FCol, FRowCount - 1), True);
    ColRowChanged;
    if Assigned(FOnHeaderClick) then
      FOnHeaderClick(Self, FCol, Button, Shift);
  end
  else

  if (Cursor = crPlus) then
  begin
    BuffString := '';
    Pt := GetCellAtPos(X, Y);
    if (Pt.X = FCol) and (Pt.Y = FRow) then
    begin
      EnsureVisible(FCol, FRow);
      if (not FReadOnly) and (not FColumns[FCol].FReadOnly) then
      begin
        IsEditing := True;
        FEdit.ShowEdit(FCol, FRow);
      end;
    end
    else
    if (Pt.X <> -1) and (pt.Y <> -1) then
    begin
      EnsureVisible(Pt.X, Pt.Y);
      FCol := Pt.X;
      FRow := Pt.Y;
      BuffString := '';
      FCol2 := FCol;
      FRow2 := FRow;
      SetSelectArea(Rect(FCol, FRow, FCol, FRow));
    end;
    ColRowChanged;
  end;

  SetCapture(Handle);
  SetFocus;

  inherited;

end;

procedure TplSmartGrid.MouseMove(Shift: TShiftState; X, Y: integer);
var
  Total2Col: integer;
  Suggested: integer;
  Pt: TPoint;
  l, t, r, b: integer;
  i: integer;

begin

  if not FEnabled then
  begin
    Cursor := crDefault;
    inherited;
    Exit;
  end;

  if (ssLeft in Shift) then
  begin

    if (Cursor = crPlus) then
    begin
      Pt := GetCellAtPos(X, Y);
      if (Pt.X <> -1) and (Pt.Y <> -1) then
      begin
        l := Min(Pt.X, FCol);
        t := Min(Pt.Y, FRow);
        r := Max(Pt.X, FCol);
        b := Max(Pt.Y, FRow);
        FCol2 := Pt.X;
        FRow2 := Pt.Y;
        SetSelectArea(Rect(l, t, r, b));
        EnsureVisible(FCol2, FRow2);
      end;
    end
    else

    if (Cursor = crSmallCross) then
    begin
      Pt := GetCellAtPos(X, Y);
      if (Pt.X <> -1) and (Pt.Y <> -1) then
      begin
        l := Min(Pt.X, SmallBoxArea.Left);
        t := Min(Pt.Y, SmallBoxArea.Top);
        r := Max(Pt.X, SmallBoxArea.Right);
        b := Max(Pt.Y, SmallBoxArea.Bottom);
        FCol2 := Pt.X;
        FRow2 := Pt.Y;
        SetSelectArea(Rect(l, t, r, b));
        EnsureVisible(FCol2, FRow2);
      end;
    end
    else

    if (Cursor = crRight) then
    begin
      i := GetRowFromY(Y);
      if (i <> -1) and (i <> LastHover) then
      begin
        LastHover := i;
        t := Min(i, FRow);
        b := Max(i, FRow);
        FRow2 := i;
        SmallBoxPos := 2;
        AdjustSelection(Rect(0, t, ColCount - 1, b), True);
      end;
    end
    else

    if (Cursor = crDown) then
    begin
      i := GetColFromX(X);
      if (i <> -1) and (i <> LastHover) then
      begin
        LastHover := i;
        l := Min(i, FCol);
        r := Max(i, FCol);
        FCol2 := i;
        SmallBoxPos := 1;
        AdjustSelection(Rect(l, 0, r, FRowCount - 1), True);
      end;
    end
    else

    if (Cursor = crHSplit) then
    begin
      Suggested := Max(5, X + FHorzOffset - SizingColX - FixedWidth);
      if FFitToWidth then
      begin
        if (SizingCol = ColCount - 1) or (SizingCol = -1) then
        begin
          inherited;
          Exit;
        end;
        Total2Col := (ClientWidth - FixedWidth) - (TotalWidth - Columns[SizingCol].FWidth - Columns[SizingCol + 1].FWidth);
        if (Total2Col > 10) then
        begin
          Columns[SizingCol].FWidth := Suggested;
          Columns[SizingCol + 1].FWidth := Total2Col - Suggested;
        end;
        if (Columns[SizingCol + 1].FWidth < 5) then
        begin
          Columns[SizingCol].FWidth := Total2Col - 5;
          Columns[SizingCol + 1].FWidth := 5;
        end;
      end
      else
      begin
        Columns[SizingCol].FWidth := Suggested;
      end;
      Recalculate;
      InvalidateRightWard(FixedWidth);
    end;

  end
  else
  begin

    Cursor := CursorArray[GetHitTestInfo(X, Y)];

  end;

  inherited;

end;

procedure TplSmartGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  Ls: TList;
  ax, ay: integer;
  l, t, w, h: integer;

  function GetCopy(nx, ny: integer): string;
  var
    ix, iy: integer;
  begin
    ix := nx;
    iy := ny;
    while (ix < l) do
      ix := ix + w;
    while (iy < t) do
      iy := iy + h;
    ix := ((ix - l) mod w) + l;
    iy := ((iy - t) mod h) + t;
    Result := SafeGetCell(TplSmartColumn(Ls[ix]).Index, iy);
  end;

begin

  if (Cursor = crSmallCross) then
  begin
    if FReadOnly then
    begin
      SmallBoxArea := Rect(-1, -1, -1, -1);
      InvalidateCells;
    end
    else
    begin
      FUpdating := True;
      Ls := TList.Create;
      for ax := FSelectArea.Left to FSelectArea.Right do
        if FColumns[ax].FVisible then
          Ls.Add(FColumns[ax]);
      l := 0;
      for ax := 0 to Ls.Count - 1 do
      begin
        if (TplSmartColumn(Ls[ax]).Index = SmallBoxArea.Left) then
        begin
          l := ax;
          Break;
        end;
      end;
      t := SmallBoxArea.Top;
      w := (SmallBoxArea.Right - SmallBoxArea.Left) + 1;
      h := (SmallBoxArea.Bottom - SmallBoxArea.Top) + 1;
      for ax := 0 to Ls.Count - 1 do
        for ay := FSelectArea.Top to FSelectArea.Bottom do
          InternalSetCell(TplSmartColumn(Ls[ax]).Index, ay, GetCopy(ax, ay), True);
      Ls.Free;
      SmallBoxArea := Rect(-1, -1, -1, -1);
      BuffString := '';
      FUpdating := False;
      InvalidateCells;
    end;
  end;

  Cursor := CursorArray[GetHitTestInfo(X, Y)];
  ReleaseCapture;
  LastHover := -1;

  inherited;
end;

procedure TplSmartGrid.SetColumns(Value: TplSmartColumns);
begin
  FColumns.Assign(Value);
end;

function TplSmartGrid.CreateColumn: TplSmartColumn;
begin
  Result := TplSmartColumn.Create(Columns);
end;

procedure TplSmartGrid.UpdateColumn(Index: integer);
var
  l, w: integer;
  Rc: TRect;
begin
  l := GetColCoord(Index);
  w := FColumns[Index].FWidth;
  Rc := Rect(l - 3, 0, l + w + 3, ClientHeight);
  InvalidateRect(Handle, @Rc, False);
end;

procedure TplSmartGrid.UpdateColumns;
begin
  UpdateHeader;
  Invalidate;
end;

function TplSmartGrid.GetColCount: integer;
begin
  Result := FColumns.Count;
end;

function TplSmartGrid.TotalWidth: integer;
var
  x: integer;
begin
  Result := 0;
  for x := 0 to FColumns.Count - 1 do
  begin
    if FColumns[x].FVisible then
      Result := Result + FColumns[x].FWidth;
  end;
end;

procedure TplSmartGrid.CMFontChanged(var Msg: TMessage);
var
  x: integer;
begin
  inherited;
  for x := 0 to FColumns.Count - 1 do
    FColumns[x].Font.Assign(Font);
end;

procedure TplSmartGrid.WMSize(var Message: TLMSize);
begin
  inherited;
  Recalculate;
  if (FColumns.Count > 0) then
    EnsureVisible(FCol, FRow);
end;

procedure TplSmartGrid.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TplSmartGrid.WMKeyDown(var Message: TLMKeyDown);
begin

  with Message do
    case CharCode of
      VK_LEFT, VK_UP, VK_RIGHT, VK_DOWN: Result := 1;
    else
      inherited;   // send msg to form...
    end;
end;

procedure TplSmartGrid.SetShowGrid(Value: boolean);
begin
  if (FShowGrid <> Value) then
  begin
    FShowGrid := Value;
    InvalidateCells;
  end;
end;

procedure TplSmartGrid.SetShowFooter(const Value: boolean);
begin
  if (FShowFooter <> Value) then
  begin
    FShowFooter := Value;
    Recalculate;
    Invalidate;
  end;
end;

procedure TplSmartGrid.Clear;
var
  x: integer;
begin
  for x := 0 to ColCount - 1 do
    FColumns[x].FStrings.Clear;
  InvalidateCells;
end;

procedure TplSmartGrid.SetHorzOffset(Value: integer);
begin
  if (FHorzOffset <> Value) then
  begin
    FHorzOffset := Max(0, Min(FMaxHScroll, Value));
    SetScrollBar(SB_HORZ, 0, FHorzOffset, SIF_POS);
    InvalidateRightWard(FixedWidth);
  end;
end;

procedure TplSmartGrid.SetVertOffset(Value: integer);
begin
  if (FVertOffset <> Value) then
  begin
    FVertOffset := Max(0, Min(FMaxVScroll, Value));
    NormalizeVertOffset;
    SetScrollBar(SB_VERT, 0, FVertOffset, SIF_POS);
    InvalidateDownWard(FixedHeight);
  end;
end;

procedure TplSmartGrid.EnsureVisible(X, Y: integer);
var
  t, b, h: integer;
  l, r: integer;
  Horz, Vert: boolean;
  SuggestedHorz, SuggestedVert: integer;

begin

  if (X = -1) or (Y = -1) then
    Exit;

  if (AllWidth < ClientWidth) and (AllHeight < ClientHeight) then
    Exit;

  SuggestedVert := FVertOffset;
  t := FVertOffset div FDefRowHeight;
  h := ((ClientHeight - FixedHeight) div FDefRowHeight) - 1;
  if FShowFooter then
    h := h - 1;
  b := t + h;
  Vert := (Y < t) or (Y > b);
  if (Y < t) then
    SuggestedVert := Y * FDefRowHeight;
  if (Y > b) then
    SuggestedVert := (Y - h) * FDefRowHeight;

  SuggestedHorz := FHorzOffset;
  l := GetColCoord(X) - FHorzOffset + FixedWidth;
  r := l + FColumns[x].FWidth;
  Horz := (l < FixedWidth) or (r > ClientWidth);
  if (l < FixedWidth) then
    SuggestedHorz := Max(0, SuggestedHorz + (l - FixedWidth));
  if (r > ClientWidth) then
    SuggestedHorz := Min(FMaxHScroll, SuggestedHorz - (ClientWidth - r) + 1);

  if Vert and not Horz then
    SetVertOffset(SuggestedVert)
  else

  if Horz and not Vert then
    SetHorzOffset(SuggestedHorz)
  else

  if Horz and Vert then
  begin
    FHorzOffset := SuggestedHorz;
    FVertOffset := SuggestedVert;
    SetScrollBar(SB_HORZ, 0, FHorzOffset, SIF_POS);
    SetScrollBar(SB_VERT, 0, FVertOffset, SIF_POS);
    Invalidate;
  end;
end;

function TplSmartGrid.HeaderCells(I: integer): THeaderInfo;
begin
  Result := PHeaderInfo(FHeaderInfos[I])^;
end;

function TplSmartGrid.HeaderCellsCount: integer;
begin
  Result := FHeaderInfos.Count;
end;

procedure TplSmartGrid.SetReadOnly(Value: boolean);
begin
  if (FReadOnly <> Value) then
  begin
    FReadOnly := Value;
  end;
end;

procedure TplSmartGrid.SetCol(Value: integer);
begin
  if (FCol <> Value) then
  begin
    ForceHideCaret;
    FCol := Value;
    FCol2 := Value;
    FRow2 := FRow;
    BuffString := '';
    SetSelectArea(Rect(FCol, FRow, FCol, FRow));
    InvalidateRightWard(FixedWidth);
    ColRowChanged;
  end;
end;

procedure TplSmartGrid.SetRow(Value: integer);
begin
  if (FRow <> Value) then
  begin
    ForceHideCaret;
    FRow := Value;
    FRow2 := Value;
    FCol2 := FCol;
    BuffString := '';
    SetSelectArea(Rect(FCol, FRow, FCol, FRow));
    InvalidateDownWard(FixedHeight);
    ColRowChanged;
  end;
end;

procedure TplSmartGrid.AdjustSelection(Value: TRect; Force: boolean);
var
  Old, Rc: TRect;
  R1, R2, R: TRect;
begin

  if EqualRect(FSelectArea, Value) and not Force then
    Exit;

  ForceHideCaret;
  Old := FSelectArea;
  FSelectArea := Value;

  Rc.Left := Min(Old.Left, FSelectArea.Left);
  Rc.Top := Min(Old.Top, FSelectArea.Top);
  Rc.Right := Max(Old.Right, FselectArea.Right);
  Rc.Bottom := Max(Old.Bottom, FSelectArea.Bottom);

  R1 := GetCellRect(Rc.Left, Rc.Top);
  R2 := GetCellRect(Rc.Right, Rc.Bottom);
  R := Rect(R1.Left, R1.Top, R2.Right, R2.Bottom);
  OffsetRect(R, -FHorzOffset + FixedWidth, -FVertOffset + FixedHeight);

  InflateRect(R, 3, 3);
  InvalidateRect(Handle, @R, False);

  if (FGutterKind = gkPointer) then
  begin
    R := Rect(0, FixedHeight, FixedWidth, ClientHeight);
    InvalidateRect(Handle, @R, False);
  end;

end;

procedure TplSmartGrid.SetSelectArea(Value: TRect);
begin
  AdjustSelection(Value, False);
end;


var
  CaretVisible: boolean = False;


procedure TplSmartGrid.ForceHideCaret;
begin
  if (csDestroying in ComponentState) then exit; // ct9999

  if CaretVisible then  HideCaret(Handle);
  CaretVisible := False;
  FEdit.HideEdit;
end;

procedure TplSmartGrid.ForceShowCaret;
begin
  if not CaretVisible then
    ShowCaret(Handle);
  CaretVisible := True;
end;

procedure TplSmartGrid.WMKillFocus(var Message: TLMKillFocus);
begin
  if Not (csDestroying in ComponentState) then   // ct9999
    if (Message.FocusedWnd <> FEdit.Handle) then ForceHideCaret;

  CaretVisible := False;
  DestroyCaret(self.Handle); // ct9999
  if not IsEditing then  InvalidateCells;
end;

procedure TplSmartGrid.WMSetFocus(var Message: TLMSetFocus);
begin
  CreateCaret(Handle, 0, 1, FDefRowHeight - 2);
  CaretVisible := False;
  InvalidateCells;
end;

procedure TplSmartGrid.SetGutterKind(Value: TGutterKind);
var
  Rc: TRect;
  RedrawAll: boolean;
  Old: TGutterKind;
begin
  Old := FGutterKind;
  if (FGutterKind <> Value) then
  begin
    FGutterKind := Value;
    Recalculate;
    RedrawAll := (Old = gkNone) or (Value = gkNone);
    if RedrawAll then
    begin
      Invalidate;
    end
    else
    begin
      Rc := Rect(0, FixedHeight, FixedWidth, ClientHeight);
      InvalidateRect(Handle, @Rc, False);
    end;
  end;
end;

procedure TplSmartGrid.SetGutterWidth(Value: integer);
begin
  if (FGutterWidth <> Value) then
  begin
    FGutterWidth := Value;
    Recalculate;
    Invalidate;
  end;
end;

procedure TplSmartGrid.CopyToClipboard;
var
  s: string;
  t: TStringList;
  x, y: integer;
begin
  t := TStringList.Create;
  for y := FSelectArea.Top to FSelectArea.Bottom do
  begin
    s := '';
    for x := FSelectArea.Left to FSelectArea.Right do
    begin
      if FColumns[x].FVisible then
      begin
        if (x = FSelectArea.Left) then
          s := SafeGetCell(X, Y)
        else
          s := s + #9 + SafeGetCell(X, Y);
      end;
    end;
    t.Add(s);
  end;
  Clipboard.AsText := t.Text;
  t.Free;
end;

procedure TplSmartGrid.CutToClipboard;
var
  s: string;
  t: TStringList;
  x, y: integer;
begin
  FUpdating := True;
  t := TStringList.Create;
  for y := FSelectArea.Top to FSelectArea.Bottom do
  begin
    s := '';
    for x := FSelectArea.Left to FSelectArea.Right do
    begin
      if FColumns[x].FVisible then
      begin
        if (x = FSelectArea.Left) then
          s := SafeGetCell(X, Y)
        else
          s := s + #9 + SafeGetCell(X, Y);
        InternalSetCell(X, Y, '', True);
      end;
    end;
    t.Add(s);
  end;
  Clipboard.AsText := t.Text;
  t.Free;
  FUpdating := False;
  InvalidateCells;
end;

procedure TplSmartGrid.PasteFromClipboard;
var
  tr, tc: TStringList;
  x, y: integer;
  s: string;
  n: integer;
  TabCnt: integer;
  ax, ay: integer;
  ColCnt: integer;

begin

  if not Clipboard.HasFormat(CF_TEXT) then
    Exit;

  ForceHideCaret;

  FUpdating := True;
  tr := TStringList.Create;
  tc := TStringList.Create;
  tr.Text := Clipboard.AsText;
  TabCnt := 1;

  for y := 0 to tr.Count - 1 do
  begin
    n := 1;
    s := tr[y];
    for x := 1 to Length(s) do
      if (s[x] = #9) then
        Inc(n);
    TabCnt := Max(TabCnt, n);
  end;

  ColCnt := ColCount; // Just to make it fast

  if (FSelectArea.Left = FSelectArea.Right) and (FSelectArea.Top = FSelectArea.Bottom) then
  begin

    for y := 0 to tr.Count - 1 do
    begin
      tc.Text := StringReplace(tr[y], #9, #13#10, [rfReplaceAll]);
      while (tc.Count < TabCnt) do
        tc.Add('');
      x := 0;
      ax := FCol;
      while (x < tc.Count) do
      begin
        ay := FRow + y;
        if FColumns[ax].FVisible then
        begin
          if (ax < ColCnt) and (ay < FRowCount) then
            InternalSetCell(ax, ay, tc[x], True);
          Inc(x);
        end;
        Inc(ax);
      end;
    end;

  end
  else
  begin

    ay := FSelectArea.Top;
    while (ay <= FSelectArea.Bottom) do
    begin
      tc.Text := StringReplace(tr[(ay - FSelectArea.Top) mod tr.Count], #9, #13#10, [rfReplaceAll]);
      while (tc.Count < TabCnt) do
        tc.Add('');
      ax := FSelectArea.Left;
      x := 0;
      while (ax <= FSelectArea.Right) do
      begin
        if FColumns[ax].FVisible then
        begin
          InternalSetCell(ax, ay, tc[x], True);
          Inc(x);
          if (x = tc.Count) then
            x := 0;
        end;
        Inc(ax);
      end;
      Inc(ay);
    end;

  end;

  tr.Free;
  tc.Free;

  FUpdating := False;
  InvalidateCells;

end;

procedure TplSmartGrid.InvalidateCells;
var
  Rc: TRect;
begin
  Rc := Rect(FixedWidth - 2, FixedHeight - 2, ClientWidth, ClientHeight);
  InvalidateRect(Handle, @Rc, False);
end;

procedure TplSmartGrid.InvalidateDownWard(aTop: integer);
var
  Rc: TRect;
begin
  Rc := Rect(0, aTop, ClientWidth, ClientHeight);
  InvalidateRect(Handle, @Rc, False);
end;

procedure TplSmartGrid.InvalidateRightWard(aLeft: integer);
var
  Rc: TRect;
begin
  Rc := Rect(aLeft, 0, ClientWidth, ClientHeight);
  InvalidateRect(Handle, @Rc, False);
end;

procedure TplSmartGrid.NormalizeVertOffset;
begin
  FVertOffset := (FVertOffset div FDefRowHeight) * FDefRowHeight;
end;

procedure TplSmartGrid.SetGutterFont(Value: TFont);
begin
  FGutterFont.Assign(Value);
  InvalidateGutter;
end;

procedure TplSmartGrid.SetHeaderFont(Value: TFont);
begin
  FHeaderFont.Assign(Value);
  InvalidateHeader;
end;

procedure TplSmartGrid.SetFooterFont(const Value: TFont);
begin
  FFooterFont.Assign(Value);
  Invalidate;
end;

procedure TplSmartGrid.InvalidateGutter;
var
  Rc: TRect;
begin
  Rc := Rect(0, FixedHeight, FixedWidth, ClientHeight);
  InvalidateRect(Handle, @Rc, False);
end;

procedure TplSmartGrid.InvalidateHeader;
var
  Rc: TRect;
begin
  Rc := Rect(0, 0, ClientWidth, FixedHeight);
  InvalidateRect(Handle, @Rc, False);
end;

procedure TplSmartGrid.HeaderFontChange(Sender: TObject);
begin
  InvalidateHeader;
end;

procedure TplSmartGrid.GutterFontChange(Sender: TObject);
begin
  InvalidateGutter;
end;

procedure TplSmartGrid.FooterFontChange(Sender: TObject);
begin
  Invalidate;
end;

function TplSmartGrid.GetFirstVisible: integer;
var
  x: integer;
begin
  Result := -1;
  if (ColCount > 0) then
  begin
    for x := 0 to ColCount - 1 do
    begin
      if Columns[x].Visible then
      begin
        Result := x;
        Break;
      end;
    end;
  end;
end;

function TplSmartGrid.GetLastVisible: integer;
var
  x: integer;
begin
  Result := -1;
  if (ColCount > 0) then
  begin
    for x := ColCount - 1 downto 0 do
    begin
      if Columns[x].Visible then
      begin
        Result := x;
        Break;
      end;
    end;
  end;
end;

function TplSmartGrid.GetNextVisible(Index: integer): integer;
var
  x: integer;
begin
  Result := Index;
  if (ColCount > 0) and (Index < ColCount) then
  begin
    for x := (Index + 1) to (ColCount - 1) do
    begin
      if Columns[x].Visible then
      begin
        Result := x;
        Break;
      end;
    end;
  end;
end;

function TplSmartGrid.GetPrevVisible(Index: integer): integer;
var
  x: integer;
begin
  Result := Index;
  if (ColCount > 0) and (Index > 0) then
  begin
    for x := (Index - 1) downto 0 do
    begin
      if Columns[x].Visible then
      begin
        Result := x;
        Break;
      end;
    end;
  end;
end;

procedure TplSmartGrid.DeleteRow(ARow: integer);
var
  x, y: integer;
begin
  ForceHideCaret;
  if (ARow >= 0) and (ARow < FRowCount) then
  begin
    for x := 0 to ColCount - 1 do
    begin
      with FColumns[x].Strings do
      begin
        if (Count > ARow) then
        begin
          for y := ARow to Count - 2 do
            Strings[y] := Strings[y + 1];
          Strings[Count - 1] := '';
        end;
      end;
    end;
    if (FRow = FRowCount - 1) then
      Dec(FRow);
    RowCount := RowCount - 1;
  end;
end;

procedure TplSmartGrid.InsertRow(ARow: integer);
var
  x: integer;
begin
  ForceHideCaret;
  if (ARow >= 0) and (ARow < FRowCount) then
  begin
    for x := 0 to ColCount - 1 do
    begin
      with FColumns[x].Strings do
      begin
        while (Count < ARow) do
          Add('');
        Insert(ARow, '');
      end;
    end;
    RowCount := RowCount + 1;
  end;
end;

function TplSmartGrid.AddRow: integer;
var
  x: integer;
  n: integer;
begin
  ForceHideCaret;
  n := FRowCount + 1;
  for x := 0 to ColCount - 1 do
  begin
    with FColumns[x].Strings do
    begin
      while (Count < n) do
        Add('');
      Strings[FRowCount] := '';
    end;
  end;
  RowCount := RowCount + 1;
  Result := FRowCount - 1;
end;

procedure TplSmartGrid.WMUnknown(var Msg: TMessage);
begin
  Msg.Result := 0;
end;

procedure TplSmartGrid.WMMouseWheel(var Message: TLMMouseEvent);
var
  Old: integer;
begin
  Old := FVertOffset;
  FVertOffset := Max(0, Min(FMaxVScroll, FVertOffset - Message.WheelDelta));
  if (FVertOffset <> Old) then
  begin
    SetScrollBar(SB_VERT, 0, FVertOffset, SIF_POS);
    Invalidate;
  end;
end;

procedure TplSmartGrid.ColRowChanged;
begin
  if Assigned(Sync) then
    Sync.Row := FRow;
  if Assigned(FOnColRowChanged) then
    FOnColRowChanged(Self, FCol, FRow);
end;

procedure TplSmartGrid.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (AComponent = Sync) and (Operation = opRemove) then
    Sync := nil;
  inherited;
end;

procedure TplSmartGrid.SetGutterStrings(const Value: TStrings);
begin
  FGutterStrings.Assign(Value);
  if (FGutterKind = gkString) then
    InvalidateGutter;
end;

function TplSmartGrid.GetObject(X, Y: integer): TObject;
var
  t: TStrings;
begin
  Result := nil;
  if (X > ColCount - 1) or (Y > FRowCount - 1) then
    raise Exception.Create('Cell Index out of bound.');
  t := Columns[X].FStrings;
  if (Y < t.Count) then
    Result := t.Objects[Y];
end;

procedure TplSmartGrid.SetObject(X, Y: integer; const Value: TObject);
var
  t: TStrings;
begin
  if (X > ColCount - 1) or (Y > FRowCount - 1) then
    raise Exception.Create('Cell Index out of bound.');
  t := Columns[X].FStrings;
  while (Y > t.Count - 1) do
    t.Add('');
  t.Objects[Y] := Value;
end;

procedure TplSmartGrid.ClearMergeCells;
var
  x, y: integer;
  List: TStrings;
begin
  for x := 0 to FColumns.Count - 1 do
  begin
    List := FColumns[x].FStrings;
    for y := 0 to List.Count - 1 do
      List.Objects[y] := nil;
  end;
  for x := 0 to Mergeds.Count - 1 do
    TMergeCell(Mergeds[x]).Free;
  Mergeds.Clear;
end;

function TplSmartGrid.MergeCells(const X1, Y1, X2, Y2: integer; ACaption: string): TMergeCell;
begin
  Result := TMergeCell.Create;
  Result.Font.Assign(Font);
  Result.Color := Color;
  Result.Caption := ACaption;
  Result.HorzAlign := haCenter;
  Result.VertAlign := vaCenter;
  Result.Rc := Rect(Min(X1, X2), Min(Y1, Y2), Max(X1, X2), Max(Y1, Y2));
  Mergeds.Add(Result);
  if not FUpdating then
  begin
    Recalculate;
    Invalidate;
  end;
end;

procedure TplSmartGrid.BuildMergeData;
var
  Rc: TRect;
  x, y, z: integer;
begin
  for x := 0 to Mergeds.Count - 1 do
  begin
    CopyRect(Rc, TMergeCell(Mergeds[x]).Rc);
    for y := Rc.Left to Rc.Right do
    begin
      if (y >= FColumns.Count) then
        Continue;
      for z := Rc.Top to Rc.Bottom do
      begin
        InternalSetCell(y, z, '', False);
        SetObject(y, z, TObject(MergeID));
      end;
    end;
  end;
end;

procedure TplSmartGrid.DrawMergedCell(Index: integer);
var
  Data: TMergeCell;
  R, Rc, Dummy: TRect;
  l1, l2, t, h: integer;
begin
  Data := TMergeCell(Mergeds[Index]);
  l1 := GetColCoord(Data.Rc.Left);
  l2 := GetColCoord(Data.Rc.Right + 1);
  t := FDefRowHeight * Data.Rc.Top;
  h := FDefRowHeight * (Data.Rc.Bottom - Data.Rc.Top + 1);
  Rc := Rect(l1 - 1, t - 1, l2, t + h);
  OffsetRect(Rc, -FHorzOffset + FixedWidth, -FVertOffset + FixedHeight);
  R := Rc;
  if IntersectRect(Dummy, Rc, CellBox) then
  begin
    with Canvas do
    begin
      Font.Assign(Data.Font);
      if not FEnabled then
        Font.Color := FGridColor;
      Pen.Color := FGridColor;
      Brush.Color := Data.Color;
      Brush.Style := bsSolid;
      if FShowGrid then
        Rectangle(Rc)
      else
        FillRect(Rc);
      Brush.Style := bsClear;
      InflateRect(Rc, -4, -2);
      DrawString(Canvas, Data.Caption, Rc, Data.HorzAlign, Data.VertAlign, False);
    end;
  end;
end;

function TplSmartGrid.GetHeaderInfo: TList;
begin
  Result := FHeaderInfos;
end;

function TplSmartGrid.GetMergedCellsData: TList;
begin
  Result := Mergeds;
end;

procedure TplSmartGrid.SetEnabled(const Value: boolean);
begin
  if (FEnabled <> Value) then
  begin
    FEnabled := Value;
    Invalidate;
  end;
end;

//==================== TplSmartColumn ==================================

constructor TplSmartColumn.Create(ACollection: TCollection);
begin
  FStrings := TStringList.Create;
  FFont := TFont.Create;
  FHorzAlign := haLeft;
  FVertAlign := vaCenter;
  FVisible := True;
  FCanResize := True;
  FReadOnly := False;
  FTag := 0;
  FTag2 := 0;
  with TplSmartColumns(ACollection).Grid do
  begin
    Self.FFont.Assign(Font);
    Self.FWidth := DefColWidth;
    Self.FColor := Color;
  end;
  FFont.OnChange := @FontChange;
  inherited Create(ACollection);
end;

destructor TplSmartColumn.Destroy;
begin
  FFont.OnChange := nil;
  FStrings.Free;
  FFont.Free;
  inherited Destroy;
end;

procedure TplSmartColumn.Assign(Source: TPersistent);
begin
  if (Source is TplSmartColumn) then
  begin
    Title := TplSmartColumn(Source).Title;
    Footer := TplSmartColumn(Source).Footer;
    Width := TplSmartColumn(Source).Width;
    Font := TplSmartColumn(Source).Font;
    Color := TplSmartColumn(Source).Color;
    HorzAlign := TplSmartColumn(Source).HorzAlign;
    VertAlign := TplSmartColumn(Source).VertAlign;
    Visible := TplSmartColumn(Source).Visible;
    Tag := TplSmartColumn(Source).Tag;
    Tag2 := TplSmartColumn(Source).Tag2;
    Hint := TplSmartColumn(Source).Hint;
    CanResize := TplSmartColumn(Source).CanResize;
    ReadOnly := TplSmartColumn(Source).ReadOnly;
    Strings.Assign(TplSmartColumn(Source).Strings);
    Changed(False);
  end;
end;

procedure TplSmartColumn.SetColor(Value: TColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;
    Changed(False);
  end;
end;

procedure TplSmartColumn.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
  Changed(False);
end;

procedure TplSmartColumn.SetHorzAlign(Value: THorzAlign);
begin
  if (FHorzAlign <> Value) then
  begin
    FHorzAlign := Value;
    Changed(False);
  end;
end;

procedure TplSmartColumn.SetTitle(Value: string);
begin
  if (FTitle <> Value) then
  begin
    FTitle := Value;
    Changed(True);
  end;
end;

procedure TplSmartColumn.SetFooter(const Value: string);
begin
  if (FFooter <> Value) then
  begin
    FFooter := Value;
    Changed(False);
  end;
end;

procedure TplSmartColumn.SetVertAlign(Value: TVertAlign);
begin
  if (FVertAlign <> Value) then
  begin
    FVertAlign := Value;
    Changed(False);
  end;
end;

procedure TplSmartColumn.SetWidth(Value: integer);
begin
  if (FWidth <> Value) then
  begin
    FWidth := Value;
    Changed(True);
  end;
end;

procedure TplSmartColumn.SetVisible(Value: boolean);
begin
  if (FVisible <> Value) then
  begin
    FVisible := Value;
    TplSmartColumns(Collection).FGrid.ForcedColumn := Index;
    Changed(True);
  end;
end;

procedure TplSmartColumn.SetStrings(Value: TStrings);
begin
  FStrings.Assign(Value);
  Changed(False);
end;

procedure TplSmartColumn.FontChange(Sender: TObject);
begin
  Changed(False);
end;

function TplSmartColumn.IsFontStored: boolean;
begin
  Result := True;
  with TplSmartColumns(Collection).FGrid.Font do
  begin
    if (Charset = FFont.Charset) and (Color = FFont.Color) and (Height = FFont.Height) and
      (Name = FFont.Name) and (Pitch = FFont.Pitch) and (PixelsPerInch = FFont.PixelsPerInch) and
      (Size = FFont.Size) and (Style = FFont.Style) then
      Result := False;
  end;
end;

function TplSmartColumn.GetGrid: TplSmartGrid;
begin
  Result := TplSmartColumns(Collection).FGrid;
end;

function TplSmartColumn.GetDisplayName: string;
begin
  if (FTitle <> '') then
    Result := FTitle
  else
    Result := 'Column ' + IntToStr(Index);
end;

//==================== TplSmartColumns ====================

constructor TplSmartColumns.Create(AGrid: TplSmartGrid);
begin
  inherited Create(TplSmartColumn);
  FGrid := AGrid;
end;

function TplSmartColumns.Add: TplSmartColumn;
begin
  Result := TplSmartColumn(inherited Add);
end;

function TplSmartColumns.GetItem(Index: integer): TplSmartColumn;
begin
  Result := TplSmartColumn(inherited GetItem(Index));
end;

procedure TplSmartColumns.SetItem(Index: integer; Value: TplSmartColumn);
begin
  inherited SetItem(Index, Value);
end;

function TplSmartColumns.GetOwner: TPersistent;
begin
  Result := FGrid;
end;

function TplSmartColumns.Insert(Index: integer): TplSmartColumn;
begin
  Result := AddItem(nil, Index);
end;

function TplSmartColumns.AddItem(Item: TplSmartColumn; Index: integer): TplSmartColumn;
begin
  if (Item = nil) then
    Result := FGrid.CreateColumn
  else
  begin
    Result := Item;
    if Assigned(Item) then
    begin
      Result.Collection := Self;
      if (Index < 0) then
        Index := Count - 1;
      Result.Index := Index;
    end;
  end;
end;

procedure TplSmartColumns.Update(Item: TCollectionItem);
begin
  if (Item <> nil) then
    FGrid.UpdateColumn(Item.Index)
  else
    FGrid.UpdateColumns;
end;


//==================== TAlignedEdit ================================================

constructor TplSmartInplace.Create(Grid: TplSmartGrid);
begin
  inherited Create(FGrid);
  FGrid := Grid;
  FAlignment := haLeft;
  Parent := FGrid;
  ParentColor := False;
  BorderStyle := bsNone;
  Left := -200;
  Top := -200;
  Visible := False;
end;

procedure TplSmartInplace.CreateParams(var Params: TCreateParams);
const
  Alignments: array [THorzAlign] of cardinal = (ES_LEFT, ES_CENTER, ES_RIGHT);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or Alignments[FAlignment];
end;

procedure TplSmartInplace.SetAlignment(Value: THorzAlign);
begin
  if (FAlignment <> Value) then
  begin
    FAlignment := Value;
    RecreateWnd(Self); // ct9999
  end;
end;

procedure TplSmartInplace.ShowEdit(X, Y: integer);
var
  Rc: TRect;
  Column: TplSmartColumn;
  l, t, w, h: integer;
begin

  if CaretVisible then
    HideCaret(Handle);
  CaretVisible := False;

  CellX := X;
  CellY := Y;
  Column := FGrid.FColumns[x];
  Color := FGrid.GetCellColor(X, Y);
  SetAlignment(Column.FHorzAlign);
  Text := FGrid.SafeGetCell(X, Y);
  Font.Assign(Column.FFont);

  Rc := FGrid.GetCellRect(X, Y);
  Rc := FGrid.CellRectToClient(Rc);

  if (FAlignment = haRight) then
    Rc.Right := Rc.Right + 1;
  InflateRect(Rc, -4, -3);

  l := Rc.Left;
  w := Rc.Right - Rc.Left;
  t := 0;
  h := FGrid.Canvas.TextHeight('gM');
  case Column.FVertAlign of
    vaTop: t := Rc.Top - 1;
    vaCenter: t := Rc.Top + (((Rc.Bottom - Rc.Top) - h) div 2);
    vaBottom: t := Rc.Bottom - h + 1;
  end;

  SetBounds(l, t, w, h);
  Show;

end;

procedure TplSmartInplace.HideEdit;
begin
  if Visible then
    Hide;
  FGrid.IsEditing := False;
end;

procedure TplSmartInplace.Change;
begin
  inherited;
  FGrid.InternalSetCell(CellX, CellY, Text, True);
end;

procedure TplSmartInplace.KeyDown(var Key: word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE, VK_RETURN, VK_UP, VK_DOWN:
    begin
      HideEdit;
      FGrid.SetFocus;
    end;
    else
      inherited;
  end;
end;

procedure TplSmartInplace.KeyPress(var Key: char);
var
  Allowed: boolean;
begin
  Allowed := True;
  if Assigned(FGrid.FOnFilterChar) then
    FGrid.FOnFilterChar(Self, CellX, CellY, Key, Allowed);
  if (not Allowed) and (Key <> Chr(VK_BACK)) then
    Key := Chr(0);
  inherited;
end;

//==================== TplSmartGridSync ===========================================

constructor TplSmartGridSync.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOnDeleteRow := @SyncDeleteRow;
  FOnInsertRow := @SyncInsertRow;
  FOnColRowChanged := @SyncColRow;
end;

procedure TplSmartGridSync.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (AComponent = FGrid) and (Operation = opRemove) then
    FGrid := nil;
  inherited;
end;

procedure TplSmartGridSync.SetGrid(const Value: TplSmartGrid);
begin
  if (FGrid <> Value) then
  begin
    FGrid := Value;
    FGrid.Sync := Self;
    FGrid.RowCount := RowCount;
  end;
end;

procedure TplSmartGridSync.SetScrollBar(AKind, AMax, APos, AMask: integer);
begin
  if (AKind = SB_VERT) and Assigned(FGrid) then
  begin
    if ((AMask and SIF_POS) <> 0) then
      FGrid.VertOffset := APos;
  end;
end;

procedure TplSmartGridSync.ShowHideScrollBar(HorzVisible, VertVisible: boolean);
begin
  ShowScrollBar(Handle, SB_HORZ, True);
  ShowScrollBar(Handle, SB_VERT, False);
  EnableScrollBar(Handle, SB_HORZ, SB_BOTH);  // ct9999
end;

procedure TplSmartGridSync.SyncColRow(Sender: TObject; ACol, ARow: integer);
begin
  if Assigned(FGrid) then
    FGrid.Row := ARow;
end;

procedure TplSmartGridSync.SyncDeleteRow(Sender: TObject; ARow: integer);
begin
  if Assigned(FGrid) then
    FGrid.DeleteRow(ARow);
end;

procedure TplSmartGridSync.SyncInsertRow(Sender: TObject; ARow: integer);
begin
  if Assigned(FGrid) then
  begin
    if (ARow = FGrid.RowCount) then
      FGrid.AddRow
    else
      FGrid.InsertRow(ARow);
  end;
end;

//==================== TMergeCell =========================

constructor TMergeCell.Create;
begin
  inherited Create;
  Font := TFont.Create;
end;

destructor TMergeCell.Destroy;
begin
  Font.Free;
  inherited Destroy;
end;

end.
