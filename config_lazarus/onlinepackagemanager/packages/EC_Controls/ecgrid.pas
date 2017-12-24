{**************************************************************************************************
 This file is part of the Eye Candy Controls (EC-C)

  Copyright (C) 2016, 2017 Vojtěch Čihák, Czech Republic

  This library is free software; you can redistribute it and/or modify it under the terms of the
  GNU Library General Public License as published by the Free Software Foundation; either version
  2 of the License, or (at your option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you permission to link this
  library with independent modules to produce an executable, regardless of the license terms of
  these independent modules,and to copy and distribute the resulting executable under terms of
  your choice, provided that you also meet, for each linked independent module, the terms and
  conditions of the license of that module. An independent module is a module which is not derived
  from or based on this library. If you modify this library, you may extend this exception to your
  version of the library, but you are not obligated to do so. If you do not wish to do so, delete
  this exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
  without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See
  the GNU Library General Public License for more details.

  You should have received a copy of the GNU Library General Public License along with this
  library; if not, write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
  Boston, MA 02111-1307, USA.

**************************************************************************************************}

unit ECGrid;
{$mode objfpc}{$H+}

//{$DEFINE DBGGRID}  {don't remove, just comment}

{ ThemeServices.DrawText to BMP.Canvas is slower than to Canvas, strings are not clipped }

interface

uses
  Classes, SysUtils, Graphics, Controls, Forms, ImgList, {$IFDEF DBGGRID} LCLProc,{$ENDIF}
  LCLType, LMessages, Math, Menus, Messages, Themes, Types, Clipbrd, LazFileUtils,
  Laz2_DOM, Laz2_XMLRead, Laz2_XMLWrite, ECTypes;

type
  {$PACKENUM 2}
  TCFlag = (ecfEnlarged,             { column is enlarged }
            ecfRedrawData,           { redraw data area of column bitmap }
            ecfRedrawHeader,         { redraw header of of column bitmap }
            ecfRedrawPrevSel,        { redraw previously selected cell }
            ecfRedrawSelection,      { redraw selected cell }
            ecfRedrawTitle);         { redraw title of of column bitmap }
  TCFlags = set of TCFlag;
  TCOption = (ecoCellToHint,         { content of data cells is shown as a Hint }
              ecoEnlargePixels,      { False = [%], True = [px] }
              ecoReadOnly,           { column is ReadOnly (never shows editor) }
              ecoSizing,             { column can be resized }
              ecoSorting,            { column header has up/down arrow and pushed look when clicked }
              ecoVisible);           { column is Visible }
  TCOptions = set of TCOption;
  TGFlag = (egfCalcBoundCols,
            egfCalcBoundRows,
            egfCalcColors,
            egfCalcColsLeft,
            egfCalcFixedCellIndent,
            egfCalcOrderVisi,
            egfCorrectEditorPosX,    { make correction of Editor Left and Width after h-scroll etc. }
            egfCorrectEditorPosY,    { make correction of Editor Top and Height after change RowHeight etc. }
            egfDeletedInDesignTime,  { column was deleted in design-time, via TCollection.Delete }
            egfDoingSelection,       { for correct drawing of selection }
            egfFirstRowFullyVisi,    { first non-fixed row is fully visible }
            egfLastRowFullyVisi,     { last non-fixed row is fully visible }
            egfLockCursor,           { locks cursor on internal changes (moving/sizing Columns) }
            egfLockHint,             { locks hint on internal changes (title/column/general hint) }
            egfMoveEditor,           { editor needs to be moved after egfCalcColsLeft }
            egfMoving,               { some column is moved, threshold was exceeded }
            egfRedrawData,           { redraw data area }
            egfRedrawFixedCols,      { redraw fixed columns except their headers }
            egfRedrawFixedHeader,    { redraw headers of fixed columns }
            egfRedrawFixedRows,      { redraw fixed rows except headers of fixed columns }
            egfResizeBMPs,           { resize bitmaps }
            egfRightToLeft,          { for less calls of IsRightToLeft }
            egfSelectCol,            { force DoSelection (after deleting Column) }
            egfSizing,               { some column is sized }
            egfUpdateRAHeight,       { update RequiredAreaHeight }
            egfUpdateRAWidth);       { update RequiredAreaWidth }
  TGFlags = set of TGFlag;
  TGOption = (egoAlwaysShowEditor,   { Editor is shown always }
              egoColMoving,          { columns can be moved }
              egoColSizing,          { columns can be resized }
              egoDottedGrid,         { grid line is dotted }
              egoEnlargeAlways,      { column is enlarged even if editor is not opened }
              egoHeaderPushedLook,   { header has pushed look when clicked }
              egoHilightCol,         { whole column is highlighted }
              egoHilightRow,         { whole row is hilighted }
              egoHorizontalLines,    { show horizontal lines of grid }
            //  egoMultiSelect,        { allows multiselect - NOT IMPLEMENTED }
              egoReadOnly,           { grid is ReadOnly }
              egoScrollKeepVisible,  { selection remains visible while scrolling (scrollbars/mouse wheel) }
              egoSortArrow,          { sorting arrow in title is visible }
              egoTabs,               { tab goes through cell instead of controls }
              egoUseOrder,           { OrderedCols defined; excfOrder for Load/SaveColumnsToXML avail. }
              egoVerticalLines,      { show vertical lines of grid }
              egoWheelScrollsGrid);  { mouse wheel scrolls the grid, selection stays (i.e. it scrolls away) }
  TGOptions = set of TGOption;
  TSelectionMode = (esmNative,       { depends on Selection position and egoAlwaysShowEditor }
                    esmDontSelect,
                    esmSelect);
  TGridStyle = (egsFlat, egsPanel, egsStandard, egsFinePanelA, egsFinePanelB, egsFinePanelC, egsThemed);
  TSelectionArea = (esaCell, esaColumn, esaRow, esaRange, esaAll);
  TSelectionItem = record
    FromCell: TPoint;
    ToCell: TPoint;
    Kind: TSelectionArea;
  end;
  TXMLColFlag = (excfOrder, excfVisible, excfWidth);
  TXMLColFlags = set of TXMLColFlag;

  TECGColumn = class;

  { events }
  TDrawDataCell = procedure (Sender: TObject; ACanvas: TCanvas; ACol, ARow: Integer; var AHandled: Boolean) of object;
  TGetDataCellText = procedure (AColumn: TECGColumn; ADataRow: Integer; out AText: string) of object;
  TGetDataRowCount = procedure (Sender: TObject; var ADataRowCount: Integer) of object;
  TGetHeaderText = procedure (Sender: TObject; ACol, ARow: Integer; out AText: string) of object;
  TSelectEditor = procedure (Sender: TObject; ACol, ADataRow: Integer; var AEditor: TWinControl; AKey: Word = 0) of object;
  TSelection = procedure (Sender: TObject; ACol, ARow: Integer) of object;

const
  cDefAlignment = taLeftJustify;

type
  TVal = object
  public
    Er: longint; static;
  end;

  { TECGTitleFontOptions }
  TECGTitleFontOptions = class(TFontOptions)
  published
    property FontColor default clBtnText;
    property FontStyles default [fsBold];
  end;

  { TECGColTitle }
  TECGColTitle = class(TPersistent)
  private
    FAlignment: TAlignment;
    FFontOptions: TECGTitleFontOptions;
    FHint: TTranslateString;
    FImageIndex: SmallInt;
    FPopupMenu: TPopupMenu;
    FTag: PtrInt;
    FText: TCaption;
    procedure SetAlignment(AValue: TAlignment);
    procedure SetImageIndex(AValue: SmallInt);
    procedure SetText(AValue: TCaption);
  protected const
    cDefFontStyles = [fsBold];
  protected
    Column: TECGColumn;
    procedure RedrawHeader;
    procedure RedrawTitle;
  public
    Data: TObject;
    constructor Create(AColumn: TECGColumn);
    destructor Destroy; override;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment default cDefAlignment;
    property FontOptions: TECGTitleFontOptions read FFontOptions write FFontOptions;
    property Hint: TTranslateString read FHint write FHint;
    property ImageIndex: SmallInt read FImageIndex write SetImageIndex default -1;
    property PopupMenu: TPopupMenu read FPopupMenu write FPopupMenu;
    property Tag: PtrInt read FTag write FTag default 0;
    property Text: TCaption read FText write SetText;
  end;

  TCustomECGrid = class;

  { TECGColumn }
  TECGColumn = class(TCollectionItem)
  private
    FAlignment: TAlignment;
    FColor: TColor;
    FColorTint: SmallInt;
    FEnlargeWidth: SmallInt;
    FFontOptions: TFontOptions;
    FHint: TTranslateString;
    FLeft: Integer;
    FMaxWidth: SmallInt;
    FMinWidth: SmallInt;
    FOnGetDataCellText: TGetDataCellText;
    FOptions: TCOptions;
    FOrder: Integer;
    FPopupMenu: TPopupMenu;
    FTag: PtrInt;
    FTitle: TECGColTitle;
    FWidth: SmallInt;
    function GetCells(ADataRow: Integer): string;
    function GetGrid: TCustomECGrid;
    function GetRight: Integer;
    function GetWidth: SmallInt;
    procedure SetAlignment(AValue: TAlignment);
    procedure SetColor(AValue: TColor);
    procedure SetColorTint(AValue: SmallInt);
    procedure SetMaxWidth(AValue: SmallInt);
    procedure SetMinWidth(AValue: SmallInt);
    procedure SetOptions(AValue: TCOptions);
    procedure SetWidth(AValue: SmallInt);
  protected const
    cDefColWidth = 80;
    cDefFontStyles = [];
    cDefOptions = [ecoVisible];
    cDefText = 'Column';
  protected
    Flags: TCFlags;
    function GetDisplayName: string; override;
    procedure RecalcRedraw;
    procedure Redraw(AFlags: TCFlags);
    procedure RedrawColumnData;
    procedure SetIndex(Value: Integer); override;
    property Grid: TCustomECGrid read GetGrid;
  public
    Data: TObject;
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    property Cells[ADataRow: Integer]: string read GetCells;
    property Right: Integer read GetRight;
    property Order: Integer read FOrder;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment default cDefAlignment;
    property Color: TColor read FColor write SetColor default clDefault;
    property ColorTint: SmallInt read FColorTint write SetColorTint default 0;         { [%] }
    property EnlargeWidth: SmallInt read FEnlargeWidth write FEnlargeWidth default 0;  { [%] or [px] }
    property FontOptions: TFontOptions read FFontOptions write FFontOptions;
    property Hint: TTranslateString read FHint write FHint;
    property Left: Integer read FLeft;
    property MaxWidth: SmallInt read FMaxWidth write SetMaxWidth default -1;
    property MinWidth: SmallInt read FMinWidth write SetMinWidth default -1;
    property Options: TCOptions read FOptions write SetOptions default cDefOptions;
    property PopupMenu: TPopupMenu read FPopupMenu write FPopupMenu;
    property Tag: PtrInt read FTag write FTag default 0;
    property Title: TECGColTitle read FTitle write FTitle;
    property Width: SmallInt read GetWidth write SetWidth default cDefColWidth;
    property OnGetDataCellText: TGetDataCellText read FOnGetDataCellText write FOnGetDataCellText;
  end;

  { TECGColumns }
  TECGColumns = class(TCollection)
  private
    function GetItems(Index: Integer): TECGColumn;
    procedure SetItems(Index: Integer; AValue: TECGColumn);
  protected
    Grid: TCustomECGrid;
    function GetOwner: TPersistent; override;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AECGrid: TCustomECGrid);
    function Add: TECGColumn;
    procedure Clear; reintroduce;
    procedure Delete(Index: Integer); reintroduce;
    property Items[Index: Integer]: TECGColumn read GetItems write SetItems; default;
  end;

  { TCustomECGrid }
  TCustomECGrid = class(TBaseScrollControl)
  private
    FAlternateColor: TColor;
    FAlternateTint: SmallInt;
    FCol: Integer;
    FColumns: TECGColumns;
    FEditorMode: Boolean;
    FFixedCols: SmallInt;
    FFixedRowHeight: SmallInt;
    FFixedRows: SmallInt;
    FGridLineColor: TColor;
    FGridLineWidth: SmallInt;
    FImages: TCustomImageList;
    FOnDrawDataCell: TDrawDataCell;
    FOnGetDataRowCount: TGetDataRowCount;
    FOnGetHeaderText: TGetHeaderText;
    FOnHeaderClick: TSelection;
    FOnSelectEditor: TSelectEditor;
    FOnSelection: TSelection;
    FOptions: TGOptions;
    FRow: Integer;
    FRowHeight: SmallInt;
    FSizableCol: Integer;
    FSortAscendent: Boolean;
    FSortIndex: Integer;
    FStyle: TGridStyle;
    function GetCells(ACol, ARow: Integer): string;
    function GetCellsOrd(AColOrder, ARow: Integer): string;
    function GetColCount: Integer;
    function GetColOrd(ACol: Integer): Integer;
    function GetRowCount: Integer;
    procedure SetAlternateColor(AValue: TColor);
    procedure SetAlternateTint(AValue: SmallInt);
    procedure SetCol(AValue: Integer);
    procedure SetEditorMode(AValue: Boolean);
    procedure SetFixedCols(AValue: SmallInt);
    procedure SetFixedRowHeight(AValue: SmallInt);
    procedure SetFixedRows(AValue: SmallInt);
    procedure SetGridLineColor(AValue: TColor);
    procedure SetGridLineWidth(AValue: SmallInt);
    procedure SetImages(AValue: TCustomImageList);
    procedure SetOptions(AValue: TGOptions);
    procedure SetRow(AValue: Integer);
    procedure SetRowHeight(AValue: SmallInt);
    procedure SetSizableCol(AValue: Integer);
    procedure SetSortAscendent(AValue: Boolean);
    procedure SetSortIndex(AValue: Integer);
    procedure SetStyle(AValue: TGridStyle);
  protected const
    cBaseFlags = DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX or DT_END_ELLIPSIS;
    cDefGridLineWidth = 1;
    cDefOptions = [egoHorizontalLines, egoUseOrder, egoVerticalLines];
    cDefRowHeight = 20;
    cDefFixedRowHeight = cDefRowHeight+2;
    cDefStyle = egsPanel;
    cIndent: SmallInt = 3;
    cMergeHilight: Single = 0.9;
    cRoot: DOMString = 'CONFIG';
    cColumn: DOMString = 'Column';
    cCount: DOMString = 'Count';
    cOrder: DOMString = 'Order';
    cVisible: DOMString = 'Visible';
    cWidth: DOMString = 'Width';
    cXMLColFlagsAll = [excfOrder, excfVisible, excfWidth];
  protected type
    TBkgndColors = array[Boolean, Boolean] of TColor;  { Color/AltColor, Hilighted }
    TPointDiff = record
      X, Y, PrevX, PrevY: Integer;
    end;
    TPaintFlag = (pfEnabled, pfEditorMode, pfHasColEvent, pfHilighted, pfHilightRow,  pfImages,
                  pfRightToLeft, pfSelectedCell, pfSelectedCol, pfTopRow, pfBottomRow, pfNonHorScroll);
    TPaintFlags = set of TPaintFlag;
  protected
    BkgndColors: array[Boolean, Boolean, Boolean] of TColor;  { alternate, hilighted, enabled }
    BkSelColors: array[Boolean, Boolean] of TColor;           { focused, enabled }
    BMPHead, BMPFixedRows, BMPFixedCols, BMPData: TBitmap;
    DataAreaRows: SmallInt;
    DefCursor: TCursor;
    DefHint: string;
    Details: TThemedElementDetails;
    DetailsText: array[Boolean] of TThemedElementDetails;
    EditorDeltaRect: TRect;
    FEditor: TWinControl;
    FirstVisiCell, LastVisiCell: TPointDiff;  { first/last (partially) visible column and row }
    FixedCellIndent: SmallInt;
    FixedColsWidth: Integer;
    FixedVisiCols: SmallInt;
    Flags: TGFlags;
    FRowCountHelp: SmallInt;
    MovableCol: Integer;
    MoveEditorX: SmallInt;
    HoveredCol, HoveredRow: Integer;
    PrevClientArea: TPoint;
    PrevClientSize: TSize;
    PrevSel: TPoint;
    PushedCol, PushedRow: Integer;
    ScrollIncX: SmallInt;
    SizeEdColXPos, SizeEdColWidth, SizeInitX: Integer;
    OrderedCols, VisiCols: array of Integer;
    procedure CalcBackgroundColors;
    procedure CalcBoundColumns;
    procedure CalcBoundRows;
    procedure CalcColumnsLeft;
    procedure CalcOrderAndVisiColumns;
    procedure CellRectLeftRight(ACol: Integer; out ALeft, ARight: Integer);
    procedure ChangeCursor(ASizing: Boolean);
    procedure ChangeHint(ACol, ARow: Integer);
    procedure CMBiDiModeChanged(var Message: TLMessage); message CM_BIDIMODECHANGED;
    procedure CMColorChanged(var {%H-}Message: TLMessage); message CM_COLORCHANGED;
    procedure CMEnter(var Message: TLMessage); message CM_ENTER;
    procedure CMExit(var Message: TLMessage); message CM_EXIT;
    procedure CorrectEditorPosX;
    procedure CorrectEditorPosY;
    procedure CreateHandle; override;
    procedure DoChanges;
    procedure DoColIndexChanged(ANew, APrevious: Integer);
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;
    procedure DoEnlargement(AEnlargeCol, AShrinkBackCol: Integer);
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    procedure DoSelectEditor(ACol: Integer; AFocus: Boolean = True; AKey: Word = 0);
    procedure DoSelection(ACol, ARow: Integer; ASelectEditor: TSelectionMode = esmNative;
                AForceFocus: Boolean = True);
    procedure DoUpdated; override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;
    procedure DrawColumnLines(ACanvas: TCanvas; ARowHeight: SmallInt; APFlags: TPaintFlags);
    procedure DrawDataCell(ACol, ARow: Integer; ASelection: TSelectionMode = esmNative);
    procedure DrawDataCellContent(ACol, ARow: Integer; APFlags: TPaintFlags; const ABkgndColors: TBkgndColors);
    procedure DrawDataColumn(ACol: Integer);
    procedure DrawDataColVertScrolled(ACol, ARowShift: Integer);
    procedure DrawFixedCellBkgnd(ACanvas: TCanvas; const ARect: TRect; APushed: Boolean);
    procedure DrawFixedColumn(ACol, AFirstRow, ALastRow: Integer);
    procedure DrawFixedColsVertScrolled(ARowShift: Integer);
    procedure DrawGridHorScrolled(ACliAreaLeftShift: Integer);
    procedure DrawHeaderCell(ACol, ARow: Integer);
    procedure DrawHeaderCellContent(ACanvas: TCanvas; ACol, ARow: Integer; ARect: TRect; APFlags: TPaintFlags);
    procedure EnabledChanged; override;
    procedure GetColumnBkgndColors(ACol: Integer; APFlags: TPaintFlags; out AColors: TBkgndColors);
    function GetCommonPaintFlags: TPaintFlags;
    procedure GetDataPaintFlags(ASelectedCol: Boolean; var APFlags: TPaintFlags);
    class function GetControlClassDefaultSize: TSize; override;
    function GetIncrementX: Integer; override;
    function GetIncrementY: Integer; override;
    function GetPageSizeX: Integer; override;
    function GetPageSizeY: Integer; override;
    procedure GoToRowRelative(ARows: Integer; AScroll: Boolean = False; AForceFocus: Boolean = True);
    procedure GoToVisiColRelative(ACols: Integer; AScroll: Boolean = False);
    function IndexToVisiColIndex(ACol: Integer): Integer;
    procedure InitializeWnd; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseLeave; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MoveEditor;
    procedure Paint; override;
    procedure PrepareEditorDeltaRect(AX, AY: Boolean);
    procedure PreparePenStyle(ACanvas: TCanvas);
    procedure Redraw(AGFlags: TGFlags);
    procedure ResizeBMPs;
    procedure SetBorderStyle(NewStyle: TBorderStyle); override;
    procedure SetCursor(Value: TCursor); override;
    procedure SetHint(const Value: TTranslateString); override;
    procedure UpdateRequiredAreaHeight; override;
    procedure UpdateRequiredAreaWidth; override;
    procedure WMHScroll(var Msg: TWMScroll); message WM_HSCROLL;
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
    procedure WMVScroll(var Msg: TWMScroll); message WM_VSCROLL;
    property SizableCol: Integer read FSizableCol write SetSizableCol;
  public
    Data: TObject;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate; override;
    function CellRect(ACol, ARow: Integer): TRect;
    function CellRectEditor(ACol, ADataRow: Integer): TRect;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    procedure EndUpdate; override;
    function IsCellFullyVisible(ACol, ARow: Integer): Boolean;
    function IsCellVisible(ACol, ARow: Integer): Boolean;
    function IsColFullyVisible(ACol: Integer): Boolean;
    function IsColVisible(ACol: Integer): Boolean;
    function IsRowFullyVisible(ARow: Integer): Boolean;
    function IsRowVisible(ARow: Integer): Boolean;
    procedure LoadColumnsFromXML(AColumnsNode: TDOMNode; AXMLFlags: TXMLColFlags = cXMLColFlagsAll); overload;
    procedure LoadColumnsFromXML(AFileName: string; AColumnsNode: DOMString;
                AXMLFlags: TXMLColFlags = cXMLColFlagsAll); overload;
    function MakeCellFullyVisible(ACol, ARow: Integer; AForce: Boolean): Boolean;
    procedure MouseToCell(AMouse: TPoint; out ACol, ARow: Integer);
    procedure SaveColumnsToXML(AXMLDoc: TXMLDocument; AColumnsNode: TDOMNode;
                AXMLFlags: TXMLColFlags = cXMLColFlagsAll); overload;
    procedure SaveColumnsToXML(AFileName: string; AColumnsNode: DOMString;
                AXMLFlags: TXMLColFlags = cXMLColFlagsAll); overload;
    procedure SaveToCSVFile(AFileName: string; ADelimiter: Char = ',';
                AHeaders: Boolean = True; AVisibleColsOnly: Boolean = False);
    procedure SelectCell(ACol, ARow: Integer; ASelectEditor: TSelectionMode; AForce: Boolean = False);
    procedure SetFocus; override;
    procedure UpdateCell(ACol, ARow: Integer);
    procedure UpdateColumn(ACol: Integer; AHeader: Boolean = False; AData: Boolean = True);
    procedure UpdateRow(ARow: Integer; AFixedCols: Boolean = False; AData: Boolean = True);
    procedure UpdateRowCount;
    property AlternateColor: TColor read FAlternateColor write SetAlternateColor default clDefault;
    property AlternateTint: SmallInt read FAlternateTint write SetAlternateTint default 0;  { [%] }
    property Cells[ACol, ARow: Integer]: string read GetCells;
    property CellsOrd[AColOrd, ARow: Integer]: string read GetCellsOrd;
    property Col: Integer read FCol write SetCol;
    property ColCount: Integer read GetColCount;
    property ColOrd[ACol: Integer]: Integer read GetColOrd;
    property Columns: TECGColumns read FColumns write FColumns;
    property Editor: TWinControl read FEditor;
    property EditorMode: Boolean read FEditorMode write SetEditorMode;
    property FixedCols: SmallInt read FFixedCols write SetFixedCols default 0;
    property FixedRowHeight: SmallInt read FFixedRowHeight write SetFixedRowHeight default cDefFixedRowHeight;
    property FixedRows: SmallInt read FFixedRows write SetFixedRows default 1;
    property GridLineColor: TColor read FGridLineColor write SetGridLineColor default clDefault;
    property GridLineWidth: SmallInt read FGridLineWidth write SetGridLineWidth default cDefGridLineWidth;
    property Images: TCustomImageList read FImages write SetImages;
    property Options: TGOptions read FOptions write SetOptions default cDefOptions;
    property OnDrawDataCell: TDrawDataCell read FOnDrawDataCell write FOnDrawDataCell;
    property OnGetDataRowCount: TGetDataRowCount read FOnGetDataRowCount write FOnGetDataRowCount;
    property OnGetHeaderText: TGetHeaderText read FOnGetHeaderText write FOnGetHeaderText;
    property OnHeaderClick: TSelection read FOnHeaderClick write FOnHeaderClick;
    property OnSelectEditor: TSelectEditor read FOnSelectEditor write FOnSelectEditor;
    property OnSelection: TSelection read FOnSelection write FOnSelection;
    property Row: Integer read FRow write SetRow;
    property RowCount: Integer read GetRowCount;
    property RowHeight: SmallInt read FRowHeight write SetRowHeight default cDefRowHeight;
    property SortAscendent: Boolean read FSortAscendent write SetSortAscendent;
    property SortIndex: Integer read FSortIndex write SetSortIndex;
    property Style: TGridStyle read FStyle write SetStyle default cDefStyle;
  end;

  { TECGrid }
  TECGrid = class(TCustomECGrid)
  published
    property Align;
    property AlternateColor;
    property AlternateTint;
    property Anchors;
    property BiDiMode;
    property BorderSpacing;
    property BorderStyle default bsSingle;
    property ColCount;
    property Color default clWindow;
    property Columns;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FixedCols;
    property FixedRowHeight;
    property FixedRows;
    property Font;
    property GridLineColor;
    property GridLineWidth;
    property Images;
    property Options;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RowCount;
    property RowHeight;
    property ScrollBars;
    property ShowHint;
    property Style;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDrawDataCell;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetDataRowCount;
    property OnGetHeaderText;
    property OnHeaderClick;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnSelectEditor;
    property OnSelection;
    property OnStartDrag;
    property OnUTF8KeyPress;
  end;

implementation

const cFlagsAlign: array[TAlignment, Boolean] of Cardinal = ((DT_LEFT, DT_RIGHT or DT_RTLREADING),
        (DT_RIGHT, DT_LEFT or DT_RTLREADING), (DT_CENTER, DT_CENTER or DT_RTLREADING));
      cHorFlags = [egfCalcBoundCols, egfCalcColsLeft, egfCalcOrderVisi, egfUpdateRAWidth];
      cRedrawColumn = [ecfRedrawData, ecfRedrawHeader, ecfRedrawTitle];
      cRedrawGrid = [egfRedrawFixedHeader, egfRedrawFixedRows, egfRedrawFixedCols, egfRedrawData];

{ TECGColTitle }

constructor TECGColTitle.Create(AColumn: TECGColumn);
begin
  Column:=AColumn;
  FFontOptions:=TECGTitleFontOptions.Create(Column.Grid);
  with FFontOptions do
   begin
     FontColor:=clBtnText;
     FontStyles:=cDefFontStyles;
     OnRecalcRedraw:=@RedrawHeader;
     OnRedraw:=@RedrawHeader;
   end;
  FImageIndex:=-1;
  if not (csLoading in AColumn.Grid.ComponentState) then FText:=TECGColumn.cDefText+inttostr(Column.ID);
end;

destructor TECGColTitle.Destroy;
begin
  FreeAndNil(FFontOptions);
  inherited Destroy;
end;

procedure TECGColTitle.RedrawHeader;
begin
  include(Column.Flags, ecfRedrawHeader);
  Column.Grid.InvalidateNonUpdated;
end;

procedure TECGColTitle.RedrawTitle;
begin
  include(Column.Flags, ecfRedrawTitle);
  Column.Grid.InvalidateNonUpdated;
end;

{ TECGColTitle.Setters }

procedure TECGColTitle.SetAlignment(AValue: TAlignment);
begin
  if FAlignment=AValue then exit;
  FAlignment:=AValue;
  RedrawHeader;
end;

procedure TECGColTitle.SetImageIndex(AValue: SmallInt);
begin
  if FImageIndex=AValue then exit;
  FImageIndex:=AValue;
  if assigned(Column.Grid.Images) then RedrawTitle;
end;

procedure TECGColTitle.SetText(AValue: TCaption);
begin
  if FText=AValue then exit;
  FText:=AValue;
  RedrawTitle;
end;

{ TECGColumn }

constructor TECGColumn.Create(ACollection: TCollection);
begin
  if assigned(ACollection) then FOrder:=ACollection.Count;
  FColor:=clDefault;
  FOptions:=cDefOptions;
  FMaxWidth:=-1;
  FMinWidth:=-1;
  FWidth:=cDefColWidth;
  inherited Create(ACollection);
  FFontOptions:=TFontOptions.Create(Grid);
  with FFontOptions do
     begin
       FontStyles:=cDefFontStyles;
       OnRecalcRedraw:=@RedrawColumnData;
       OnRedraw:=@RedrawColumnData;
     end;
  FTitle:=TECGColTitle.Create(self);
  Flags:=[ecfRedrawData, ecfRedrawHeader];
end;

destructor TECGColumn.Destroy;
begin
  FreeAndNil(FFontOptions);
  FreeAndNil(FTitle);
  inherited Destroy;
end;

function TECGColumn.GetDisplayName: string;
begin
  Result:=Title.Text;
  if Result='' then Result:=cDefText+inttostr(ID);
end;

procedure TECGColumn.RecalcRedraw;
begin
  Flags:=Flags+cRedrawColumn;
  Grid.Redraw(cHorFlags+[egfRedrawData, egfRedrawFixedRows]);
end;

procedure TECGColumn.Redraw(AFlags: TCFlags);
begin
  Flags:=Flags+AFlags;
  Grid.InvalidateNonUpdated;
end;

procedure TECGColumn.RedrawColumnData;
begin
  include(Flags, ecfRedrawData);
  Grid.InvalidateNonUpdated;
end;

procedure TECGColumn.SetIndex(Value: Integer);
var aGFlags: TGFlags;
begin
  aGFlags:=[egfCalcBoundCols, egfCalcColsLeft, egfCalcOrderVisi, egfRedrawData, egfRedrawFixedRows];
  if (Value<Grid.FixedCols) or (Index<Grid.FixedCols) then
    aGFlags:=aGFlags+[egfRedrawFixedCols, egfRedrawFixedHeader];
  Grid.DoColIndexChanged(Value, Index);
  inherited SetIndex(Value);
  Grid.Redraw(aGFlags);
end;

{ TECGColumn.G/Setters }

function TECGColumn.GetCells(ADataRow: Integer): string;
begin
  if assigned(OnGetDataCellText)
    then OnGetDataCellText(self, ADataRow, Result)
    else Result:='';
end;

function TECGColumn.GetGrid: TCustomECGrid;
begin
  Result:=TECGColumns(Collection).Grid;
end;

function TECGColumn.GetRight: Integer;
begin
  Result:=FLeft+Width;
end;

function TECGColumn.GetWidth: SmallInt;
begin
  Result:=FWidth;
  if (ecfEnlarged in Flags) and (EnlargeWidth>=0) then
    if ecoEnlargePixels in Options
      then inc(Result, EnlargeWidth)                  { px }
      else inc(Result, EnlargeWidth*Result div 100);  { % }
end;

procedure TECGColumn.SetAlignment(AValue: TAlignment);
begin
  if FAlignment=AValue then exit;
  FAlignment:=AValue;
  Redraw([ecfRedrawData]);
end;

procedure TECGColumn.SetColor(AValue: TColor);
begin
  if FColor=AValue then exit;
  FColor:=AValue;
  Redraw([ecfRedrawData]);
end;

procedure TECGColumn.SetColorTint(AValue: SmallInt);
begin
  if AValue<0
    then AValue:=0
    else if AValue>100 then AValue:=100;
  if FColorTint=AValue then exit;
  FColorTint:=AValue;
  Redraw([ecfRedrawData]);
end;

procedure TECGColumn.SetMaxWidth(AValue: SmallInt);
begin
  if FMaxWidth=AValue then exit;
  FMaxWidth:=AValue;
  if not (csLoading in Grid.ComponentState) then
    if (AValue>=0) and (FWidth>AValue) then
      begin
        FWidth:=AValue;
        RecalcRedraw;
      end;
end;

procedure TECGColumn.SetMinWidth(AValue: SmallInt);
begin
  if FMinWidth=AValue then exit;
  FMinWidth:=AValue;
  if not (csLoading in Grid.ComponentState) then
    if (AValue>=0) and (FWidth<AValue) then
      begin
        FWidth:=AValue;
        RecalcRedraw;
      end;
end;

procedure TECGColumn.SetOptions(AValue: TCOptions);
var aChangedOpts: TCOptions;
begin
  aChangedOpts:= FOptions><AValue;
  if FOptions=AValue then exit;
  FOptions:=AValue;
  if ecoVisible in aChangedOpts then
    begin
      RecalcRedraw;
    end;
end;

procedure TECGColumn.SetWidth(AValue: SmallInt);
begin
  if AValue<0 then exit;
  if not (csLoading in Grid.ComponentState) then
    begin
      if (MinWidth>=0) and (AValue<MinWidth)
        then AValue:=MinWidth
        else if (MaxWidth>=0) and (AValue>MaxWidth) then AValue:=MaxWidth;
    end;
  if FWidth=AValue then exit;
  FWidth:=AValue;
  RecalcRedraw;
end;

{ TECGColumns }

constructor TECGColumns.Create(AECGrid: TCustomECGrid);
begin
  inherited Create(TECGColumn);
  Grid:=AECGrid;
end;

function TECGColumns.Add: TECGColumn;
begin
  Result:=TECGColumn(inherited Add);
end;

procedure TECGColumns.Clear;
begin
  inherited Clear;
  Grid.FCol:=-1;
  Grid.EditorMode:=False;
  Grid.FSortIndex:=-1;
  Grid.Flags:=Grid.Flags+cHorFlags;
  Grid.InvalidateNonUpdated;
end;

procedure TECGColumns.Delete(Index: Integer);
var i, aCol, aOrder: Integer;
begin
  aCol:=Grid.Col;
  if (aCol>Index) and Grid.EditorMode then Grid.PrepareEditorDeltaRect(True, False);
  aOrder:=Items[Index].Order;
  inherited Delete(Index);
  if aCol=Index then
    begin
      if Grid.EditorMode then
        begin
          Grid.FCol:=-1;
          Grid.EditorMode:=False;
        end;
      i:=aCol-1;
      while (i>=0) and not (ecoVisible in Items[i].Options) do
        dec(i);
      Grid.FCol:=i;
      include(Grid.Flags, egfSelectCol);
    end else
    if aCol>Index then Grid.FCol:=aCol-1;
  aCol:=Grid.SortIndex;
  if aCol=Index
    then Grid.FSortIndex:=-1
    else if aCol>Index then Grid.FSortIndex:=aCol-1;
  if egoUseOrder in Grid.Options then
    for i:=0 to Count-1 do
      if Items[i].Order>aOrder then dec(Items[i].FOrder);
  Grid.Flags:=Grid.Flags+cHorFlags;
  if Grid.UpdateCount=0 then  { ~3% faster than InvalidateNonUpdated; }
    begin
      Grid.DoChanges;
      Grid.Invalidate;
    end;
end;

function TECGColumns.GetOwner: TPersistent;
begin
  Result:=Grid;
end;

procedure TECGColumns.Notify(Item: TCollectionItem; Action: TCollectionNotification);
begin
  inherited Notify(Item, Action);
  case Action of
    cnAdded:
      begin
        Grid.Flags:=Grid.Flags+cHorFlags;
        if Grid.UpdateCount=0 then  { ~3% faster than InvalidateNonUpdated; }
          begin
            if Grid.HandleAllocated then Grid.DoChanges;
            Grid.Invalidate;
          end;
      end;
    cnExtracting: if csDesigning in Grid.ComponentState then include(Grid.Flags, egfDeletedInDesignTime);
  end;
end;

procedure TECGColumns.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if egfDeletedInDesignTime in Grid.Flags then
    begin
      Grid.Flags:=Grid.Flags+cHorFlags-[egfDeletedInDesignTime];
      Grid.InvalidateNonUpdated;
    end;
end;

{ TECGColumns.G/Setters }

function TECGColumns.GetItems(Index: Integer): TECGColumn;
begin
  Result:=TECGColumn(inherited Items[Index]);
end;

procedure TECGColumns.SetItems(Index: Integer; AValue: TECGColumn);
begin
  Items[Index].Assign(AValue);
end;

{ TCustomECGrid }

constructor TCustomECGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle:=ControlStyle+[csOpaque, csClickEvents, csCaptureMouse, csParentBackground]
                            +csMultiClicks-[csParentBackground, csAcceptsControls, csNoFocus, csSetCaption];
  FAlternateColor:=clDefault;
  FCol:=-1;
  FColumns:=TECGColumns.Create(self);
  FFixedRows:=1;
  FGridLineColor:=clDefault;
  FGridLineWidth:=cDefGridLineWidth;
  FOptions:=cDefOptions;
  FRow:=-1;
  FRowHeight:=cDefRowHeight;
  FSortAscendent:=True;
  FSortIndex:=-1;
  FStyle:=cDefStyle;
  FFixedRowHeight:=cDefFixedRowHeight;
  BorderStyle:=bsSingle;
  Color:=clWindow;
  DefCursor:=Cursor;
  SizableCol:=-1;
  ParentColor:=False;
  TabStop:=True;
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, cx, cy);
  BMPHead:=TBitmap.Create;
  BMPHead.Canvas.Clipping:=True;
  BMPFixedRows:=TBitmap.Create;
  BMPFixedRows.Canvas.Clipping:=True;
  BMPFixedCols:=TBitmap.Create;
  BMPFixedCols.Canvas.Clipping:=True;
  BMPData:=TBitmap.Create;
  BMPData.Canvas.Clipping:=True;
  Details:=ThemeServices.GetElementDetails(thHeaderItemNormal);
  DetailsText[False]:=ThemeServices.GetElementDetails(tbPushButtonDisabled);
  DetailsText[True]:=ThemeServices.GetElementDetails(tbPushButtonNormal);
  Flags:=[egfCalcBoundCols, egfCalcBoundRows, egfCalcFixedCellIndent, egfCalcColors,
          egfCalcOrderVisi, egfResizeBMPs, egfUpdateRAHeight, egfUpdateRAWidth];
  PrevSel:=Point(-1, -1);
  PushedCol:=-1;
end;

destructor TCustomECGrid.Destroy;
begin
  FreeAndNil(FColumns);
  FreeAndNil(BMPHead);
  FreeAndNil(BMPFixedRows);
  FreeAndNil(BMPFixedCols);
  FreeAndNil(BMPData);
  inherited Destroy;
end;

procedure TCustomECGrid.BeginUpdate;
begin
  ControlStyle:=ControlStyle+[csNoStdEvents];
  inherited BeginUpdate;
end;

procedure TCustomECGrid.CalcBackgroundColors;
const cMergeFocus: Single = 0.67;
var aColor: TColor;
    bAlt, bHilight: Boolean;
begin  { [alternate, hilight, enabled]; [focused, enabled] }
  aColor:=GetColorResolvingDefault(Color, clWindow);
  BkgndColors[False, False, True]:=aColor;
  if AlternateColor=clDefault
    then BkgndColors[True, False, True]:=aColor
    else BkgndColors[True, False, True]:=GetMergedColor(AlternateColor, aColor, 0.01*AlternateTint);
  BkgndColors[False, True, True]:=GetMergedColor(aColor, clHighlight, cMergeHilight);
  BkgndColors[True, True, True]:=GetMergedColor(BkgndColors[True, False, True], clHighlight, cMergeHilight);
  for bAlt:=False to True do
    for bHilight:=False to True do
      BkgndColors[bAlt, bHilight, False]:=GetMonochromaticColor(BkgndColors[bAlt, bHilight, True]);
  BkSelColors[True, True]:=clHighlight;
  BkSelColors[True, False]:=GetMonochromaticColor(clHighlight);
  BkSelColors[False, True]:=GetMergedColor(clHighlight, aColor, cMergeFocus);
  BkSelColors[False, False]:=GetMonochromaticColor(BkSelColors[False, True]);
  exclude(Flags, egfCalcColors);
end;

procedure TCustomECGrid.CalcBoundColumns;
var i, aClientAreaX, aCnt, aMax, aMin, aVisiCountM1: Integer;
begin
  aClientAreaX:=FixedColsWidth+ClientAreaLeft;
  aMin:=FixedVisiCols;
  aVisiCountM1:=high(VisiCols);
  if aVisiCountM1>=aMin then
    begin
      aMax:=aVisiCountM1;
      aCnt:=2;
      if aMax>0 then inc(aCnt, round(log2(aMax)));
      while aCnt>0 do
        begin
          i:=(aMax+aMin) div 2;
          if Columns[VisiCols[i]].Left<=aClientAreaX then
            begin
              aMin:=i;
              if Columns[VisiCols[i]].Right>aClientAreaX then break;
            end else
            aMax:=i;
          dec(aCnt);
        end;
      FirstVisiCell.X:=i;
      aMin:=Columns[VisiCols[i]].Left;
      ScrollIncX:=Columns[VisiCols[i]].Width;
      aClientAreaX:=Math.min(FRequiredArea.X, ClientAreaLeft+ClientWidth);
      while (i<aVisiCountM1) and (Columns[VisiCols[i]].Right<aClientAreaX) do
        inc(i);
      LastVisiCell.X:=i;
    end else
    begin
      FirstVisiCell.X:=-2;
      LastVisiCell.X:=-3;
    end;
  exclude(Flags, egfCalcBoundCols);
end;

procedure TCustomECGrid.CalcBoundRows;
var aDataArea, aLastVisiRow, aRowHeight, aRowHelp: Integer;
begin
  Flags:=Flags-[egfCalcBoundRows, egfFirstRowFullyVisi, egfLastRowFullyVisi];
  aRowHeight:=RowHeight;
  FirstVisiCell.Y:=ClientAreaTop div aRowHeight +FixedRows;
  aRowHelp:=ClientAreaTop mod aRowHeight;
  if aRowHelp=0 then include(Flags, egfFirstRowFullyVisi);
  aDataArea:=ClientHeight-FixedRows*FixedRowHeight+aRowHelp;
  aLastVisiRow:=aDataArea div aRowHeight;
  DataAreaRows:=aLastVisiRow;
  inc(aLastVisiRow, FirstVisiCell.Y);
  aRowHelp:=RowCount;
  if aLastVisiRow<aRowHelp then
    begin
      if (aDataArea mod aRowHeight)=0 then
        begin
          dec(aLastVisiRow);
          include(Flags, egfLastRowFullyVisi);
        end;
      LastVisiCell.Y:=aLastVisiRow;
    end else
    begin
      LastVisiCell.Y:=aRowHelp-1;
      include(Flags, egfLastRowFullyVisi);
    end;
end;

procedure TCustomECGrid.CalcColumnsLeft;
var i, aLeft: Integer;
begin
  aLeft:=0;
  for i:=0 to high(VisiCols) do
    begin
      Columns[VisiCols[i]].FLeft:=aLeft;
      inc(aLeft, Columns[VisiCols[i]].Width);
    end;
  aLeft:=0;  { calc. visible fixed columns }
  for i:=0 to Math.min(FixedCols, Columns.Count)-1 do
    if ecoVisible in Columns[i].Options then inc(aLeft);
  FixedVisiCols:=aLeft;
  if aLeft>0
    then FixedColsWidth:=Columns[VisiCols[aLeft-1]].Right
    else FixedColsWidth:=0;
  exclude(Flags, egfCalcColsLeft);
end;

procedure TCustomECGrid.CalcOrderAndVisiColumns;
var i, aCount, aVisibles: Integer;
begin
  aCount:=Columns.Count;
  aVisibles:=aCount;
  if egoUseOrder in Options then
    begin
      SetLength(OrderedCols, aCount);
      dec(aCount);
      for i:=0 to aCount do
        begin
          OrderedCols[Columns[i].Order]:=i;
          if not (ecoVisible in Columns[i].Options) then dec(aVisibles);
        end
    end else
    begin
      SetLength(OrderedCols, 0);
      dec(aCount);
      for i:=0 to aCount do
        if not (ecoVisible in Columns[i].Options) then dec(aVisibles);
    end;
  SetLength(VisiCols, aVisibles);
  aVisibles:=0;
  for i:=0 to aCount do
    begin
      if ecoVisible in Columns[i].Options then
        begin
          VisiCols[aVisibles]:=i;
          inc(aVisibles);
        end;
    end;
  exclude(Flags, egfCalcOrderVisi);
end;

function TCustomECGrid.CellRect(ACol, ARow: Integer): TRect;
begin
  CellRectLeftRight(ACol, Result.Left, Result.Right);
  if ARow<FixedRows then
    begin
      Result.Top:=ARow*FixedRowHeight;
      Result.Bottom:=Result.Top+FixedRowHeight;
    end else
    begin
      Result.Top:=FixedRows*FixedRowHeight+(ARow-FixedRows)*RowHeight-ClientAreaTop;
      Result.Bottom:=Result.Top+RowHeight;
    end;
end;

function TCustomECGrid.CellRectEditor(ACol, ADataRow: Integer): TRect;
begin
  CellRectLeftRight(ACol, Result.Left, Result.Right);
  Result.Top:=FixedRows*FixedRowHeight+ADataRow*RowHeight-ClientAreaTop;
  Result.Bottom:=Result.Top+RowHeight;
  if GridLineWidth=1 then
    begin
      if not (egfRightToLeft in Flags)
        then dec(Result.Right)
        else inc(Result.Left);
      dec(Result.Bottom);
    end;
end;

procedure TCustomECGrid.CellRectLeftRight(ACol: Integer; out ALeft, ARight: Integer);
begin
  if not (egfRightToLeft in Flags) then
    begin
      ALeft:=Columns[ACol].Left;
      if ACol>=FixedCols then dec(ALeft, ClientAreaLeft);
      ARight:=ALeft+Columns[ACol].Width;
    end else
    begin
      ARight:=ClientWidth-Columns[ACol].Left;
      if ACol>=FixedCols then inc(ARight, ClientAreaLeft);
      ALeft:=ARight-Columns[ACol].Width;
    end;
end;

procedure TCustomECGrid.ChangeCursor(ASizing: Boolean);
begin
  include(Flags, egfLockCursor);
  if not ASizing
    then Cursor:=DefCursor
    else Cursor:=crHSplit;
  exclude(Flags, egfLockCursor);
end;

procedure TCustomECGrid.ChangeHint(ACol, ARow: Integer);
var aOldHint: string;
begin
  include(Flags, egfLockHint);
  aOldHint:=Hint;
  if (ACol>=0) and (ARow>=0) then
    begin
      if ecoCellToHint in Columns[ACol].Options then
        begin
          if ARow>=FixedRows
            then Hint:=Columns[ACol].Cells[ARow-FixedRows]
            else Hint:=Columns[ACol].Title.Hint;
        end else
        if (ARow<FixedRows) and (Columns[ACol].Title.Hint<>'')
          then Hint:=Columns[ACol].Title.Hint
          else if Columns[ACol].Hint<>''
                 then Hint:=Columns[ACol].Hint
                 else Hint:=DefHint;
    end else
    Hint:=DefHint;
  exclude(Flags, egfLockHint);
  if aOldHint<>Hint then
    begin
      Application.CancelHint;
      Application.ActivateHint(Mouse.CursorPos);
    end;
end;

procedure TCustomECGrid.CMBiDiModeChanged(var Message: TLMessage);
begin
  if not IsRightToLeft
    then exclude(Flags, egfRightToLeft)
    else include(Flags, egfRightToLeft);
  Flags:=Flags+cRedrawGrid;
  inherited CMBidiModeChanged(Message);
end;

procedure TCustomECGrid.CMColorChanged(var Message: TLMessage);
begin
  include(Flags, egfCalcColors);
end;

procedure TCustomECGrid.CMEnter(var Message: TLMessage);
begin
  inherited CMEnter(Message);
  if EditorMode and Editor.CanFocus
    then Editor.SetFocus
    else if IsCellVisible(Col, Row) then DrawDataCell(Col, Row);
end;

procedure TCustomECGrid.CMExit(var Message: TLMessage);
begin
  inherited CMExit(Message);
  if not (egoAlwaysShowEditor in Options) then
    begin
      EditorMode:=False;
      if IsCellVisible(Col, Row) then DrawDataCell(Col, Row);
    end;
end;

procedure TCustomECGrid.CorrectEditorPosX;
var aLeft, aRight: Integer;
    bCellFullyVisi: Boolean;
begin
  CellRectLeftRight(Col, aLeft, aRight);
  if not (egfRightToLeft in Flags)
    then bCellFullyVisi:= ((aLeft>=FixedColsWidth) and (aRight<=ClientWidth))
    else bCellFullyVisi:= ((aLeft>=0) and (aRight<=(ClientWidth-FixedColsWidth)));
  if bCellFullyVisi then
    begin
      Editor.Left:=aLeft+EditorDeltaRect.Left;
      Editor.Width:=aRight-aLeft+EditorDeltaRect.Right;
    end else
    EditorMode:=False;
  exclude(Flags, egfCorrectEditorPosX);
end;

procedure TCustomECGrid.CorrectEditorPosY;
var aHeaderHeight, aTop, aBottom: Integer;
begin
  aHeaderHeight:=FixedRows*FixedRowHeight;
  aTop:=aHeaderHeight+(Row-FixedRows)*RowHeight-ClientAreaTop;
  aBottom:=aTop+RowHeight;
  if (aTop>=aHeaderHeight) and (aBottom<=ClientHeight) then
    begin
      Editor.Top:=aTop+EditorDeltaRect.Top;
      Editor.Height:=aBottom-aTop+EditorDeltaRect.Bottom;
    end else
    EditorMode:=False;
  exclude(Flags, egfCorrectEditorPosY);
end;

procedure TCustomECGrid.CreateHandle;
begin
  Flags:=Flags+cRedrawGrid;
  inherited CreateHandle;
end;

procedure TCustomECGrid.DoChanges;
var aGFlags: TGFlags;
    aCol, aRow: Integer;
    bUpdate, bValidCol, bValidRow: Boolean;
begin
  {$IFDEF DBGGRID} DebugLn('TCustomECGrid.DoChanges'); {$ENDIF}
  aGFlags:=Flags;
  bUpdate:= ([egfUpdateRAHeight, egfUpdateRAWidth]*aGFlags<>[]);
  if egfCalcColors in Flags then CalcBackgroundColors;
  if egfCalcFixedCellIndent in aGFlags then
    begin
      FixedCellIndent:=cIndent;
      case Style of
        egsFlat: inc(FixedCellIndent, GridLineWidth div 2);
        egsFinePanelB, egsFinePanelC: inc(FixedCellIndent);
      end;
      exclude(Flags, egfCalcFixedCellIndent);
    end;
  if egfUpdateRAHeight in aGFlags then UpdateRequiredAreaHeight;
  if egfCalcOrderVisi in aGFlags then CalcOrderAndVisiColumns;
  if egfCalcColsLeft in aGFlags then CalcColumnsLeft;
  if egfSelectCol in aGFlags then
    begin
      Flags:=Flags-[egfCorrectEditorPosX, egfSelectCol];
      aCol:=FCol;
      FCol:=-1;
      Col:=aCol;
      exclude(aGFlags, egfCorrectEditorPosX);
    end;
  if egfMoveEditor in aGFlags then MoveEditor;
  if egfUpdateRAWidth in aGFlags then UpdateRequiredAreaWidth;
  if bUpdate then UpdateScrollBars;
  if egfResizeBMPs in Flags then ResizeBMPs;
  if egfCalcBoundCols in aGFlags then CalcBoundColumns;
  if egfCalcBoundRows in aGFlags then CalcBoundRows;
  if egfCorrectEditorPosX in aGFlags then CorrectEditorPosX;
  if egfCorrectEditorPosY in aGFlags then CorrectEditorPosY;
  if bUpdate then
    begin
      aRow:=Row;
      bValidRow:=False;
      if RowCount>FixedRows then
        begin
          if aRow<FixedRows
            then aRow:=FixedRows
            else if aRow>=RowCount
                   then aRow:=RowCount-1
                   else bValidRow:=True;
        end;
      aCol:=Col;
      bValidCol:=False;
      if length(VisiCols)>FixedVisiCols then
        begin
          if aCol<VisiCols[FixedVisiCols]
            then aCol:=VisiCols[FixedVisiCols]
            else if aCol>VisiCols[high(VisiCols)]
                   then aCol:=VisiCols[high(VisiCols)]
                   else bValidCol:=True;
        end;
      if not (bValidCol and bValidRow) and (bValidRow or (aRow<>Row)) and (bValidCol or (aCol<>Col))
        then DoSelection(aCol, aRow, esmNative, False);
    end;
end;

procedure TCustomECGrid.DoColIndexChanged(ANew, APrevious: Integer);
var aCol: Integer;
begin
  aCol:=Col;
  if aCol>=0 then
    begin
      MoveEditorX:=Columns[aCol].Left;
      if APrevious=aCol
        then FCol:=ANew
        else if APrevious>aCol then
               begin
                 if ANew<=aCol then FCol:=aCol+1;
               end else
               if ANew>=aCol then FCol:=aCol-1;
      if EditorMode then include(Flags, egfMoveEditor);
    end;
  aCol:=SortIndex;
  if aCol>=FixedCols then
    begin
      if APrevious=aCol
        then SortIndex:=ANew
        else if APrevious>aCol then
               begin
                 if ANew<=aCol then SortIndex:=aCol+1;
               end else
               if ANew>=aCol then SortIndex:=aCol-1;
    end;
end;

procedure TCustomECGrid.DoContextPopup(MousePos: TPoint; var Handled: Boolean);
begin
  if IsEnabled and (HoveredCol>=0) and (HoveredRow>=0) then
    begin
      if (HoveredRow<FixedRows) and assigned(Columns[HoveredCol].Title.PopupMenu) then
        begin
          Columns[HoveredCol].Title.PopupMenu.PopUp;
          Handled:=True;
        end else
        if assigned(Columns[HoveredCol].PopupMenu) then
          begin
            Columns[HoveredCol].PopupMenu.PopUp;
            Handled:=True;
          end;
    end;
  inherited DoContextPopup(MousePos, Handled);
end;

procedure TCustomECGrid.DoEnlargement(AEnlargeCol, AShrinkBackCol: Integer);
var bEnlarge, bShrink: Boolean;
begin
  bShrink:= ((AShrinkBackCol>=0) and (ecfEnlarged in Columns[AShrinkBackCol].Flags));
  bEnlarge:= ((AEnlargeCol>=0) and (Columns[AEnlargeCol].EnlargeWidth<>0));
  if bShrink then exclude(Columns[AShrinkBackCol].Flags, ecfEnlarged);
  if bEnlarge then include(Columns[AEnlargeCol].Flags, ecfEnlarged);
  if bShrink or bEnlarge then
    begin
      Flags:=Flags+[egfCalcBoundCols, egfCalcColsLeft, egfRedrawData, egfRedrawFixedRows, egfUpdateRAWidth];
      InvalidateNonUpdated;
    end;
end;

function TCustomECGrid.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
var aSelectionPos: Integer;
begin
  Result:=inherited DoMouseWheelDown(Shift, MousePos);
  if not Result then
    begin
      if not (egoWheelScrollsGrid in Options) xor (ssModifier in Shift)
        then GoToRowRelative(1, False, False)
        else
        begin
          aSelectionPos:=-1;
          if egoScrollKeepVisible in Options then
            if IsRowVisible(Row) then aSelectionPos:=Row-FirstVisiCell.Y;
          ClientAreaTop:=ClientAreaTop+RowHeight;
          UpdateRowCount;
          if aSelectionPos>=0 then Row:=FirstVisiCell.Y+aSelectionPos;
        end;
      Result:=True;
    end;
end;

function TCustomECGrid.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
var aSelectionPos: Integer;
begin
  Result:=inherited DoMouseWheelUp(Shift, MousePos);
  if not Result then
    begin
      if not (egoWheelScrollsGrid in Options) xor (ssModifier in Shift)
        then GoToRowRelative(-1, False, False)
        else
         begin
          aSelectionPos:=-1;
          if egoScrollKeepVisible in Options then
            if IsRowVisible(Row) then aSelectionPos:=Row-FirstVisiCell.Y;
          ClientAreaTop:=ClientAreaTop-RowHeight;
          UpdateRowCount;
          if aSelectionPos>=0 then Row:=FirstVisiCell.Y+aSelectionPos;
        end;
      Result:=True;
    end;
end;

procedure TCustomECGrid.DoSelectEditor(ACol: Integer; AFocus: Boolean = True; AKey: Word = 0);
var aEditor: TWinControl;
begin
  if assigned(OnSelectEditor) then
    begin
      aEditor:=nil;
      if not (egoEnlargeAlways in Options) then DoEnlargement(ACol, -1);
      OnSelectEditor(self, ACol, Row-FixedRows, aEditor, AKey);
      if assigned(aEditor) then
        begin
          FEditor:=aEditor;
          FEditorMode:=True;
          if not assigned(aEditor.PopupMenu) then aEditor.PopupMenu:=PopupMenu;
          aEditor.Visible:=True;
          aEditor.Parent:=self;
          if AFocus and aEditor.CanFocus then aEditor.SetFocus;
        end else
        if not (egoEnlargeAlways in Options) then DoEnlargement(-1, ACol);
    end;
end;

procedure TCustomECGrid.DoSelection(ACol, ARow: Integer; ASelectEditor: TSelectionMode = esmNative;
            AForceFocus: Boolean = True);
var aPrevCol: Integer;
begin
  include(Flags, egfDoingSelection);
  aPrevCol:=Col;
  if (ACol=aPrevCol) and (ARow=Row) then
    begin
      if not EditorMode and not (ASelectEditor=esmDontSelect) and not (egoReadOnly in Options)
        and not (ecoReadOnly in Columns[ACol].Options) then
        begin
          DoSelectEditor(ACol, AForceFocus);
          if EditorMode then DrawDataCell(aPrevCol, Row, esmDontSelect);
        end;
    end else
    begin
      AForceFocus:= (AForceFocus or ((EditorMode and Editor.Focused) or Focused));
      EditorMode:=False;
      FCol:=ACol;
      FRow:=ARow;
      if (ACol<>aPrevCol) and (egoEnlargeAlways in Options) then DoEnlargement(ACol, aPrevCol);
      if assigned(OnSelection) then OnSelection(self, ACol, ARow);
      if ASelectEditor<>esmDontSelect then
        if ((ASelectEditor=esmSelect) or (egoAlwaysShowEditor in Options)) and not (egoReadOnly in Options)
          and not (ecoReadOnly in Columns[ACol].Options) then
          if assigned(OnSelectEditor) then
            begin
              DoEnlargement(ACol, -1);
              DoSelectEditor(ACol, AForceFocus);
            end;
      InvalidateNonUpdated;
    end;
  exclude(Flags, egfDoingSelection);
end;

procedure TCustomECGrid.DoUpdated;
begin
  if HandleAllocated then DoChanges;
  inherited DoUpdated;
end;

procedure TCustomECGrid.DragDrop(Source: TObject; X, Y: Integer);
begin
  inherited DragDrop(Source, X, Y);
  if HoveredCol>=0 then
    begin
      Columns[MovableCol].Index:=HoveredCol;
      Flags:=Flags-[egfMoving];
      InvalidateNonUpdated;
      MovableCol:=HoveredCol;
      if EditorMode and Editor.CanFocus then Editor.SetFocus;
    end;
end;

procedure TCustomECGrid.DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
var aCol, aRow: Integer;
begin
  inherited DragOver(Source, X, Y, State, Accept);
  include(Flags, egfMoving);
  MouseToCell(Point(X, Math.min(Y, ClientHeight-1)), aCol, aRow);
  HoveredCol:=aCol;
  Accept:= ((aRow>=0) and (aCol>=FixedCols) and (aCol<>MovableCol));
end;

procedure TCustomECGrid.DrawColumnLines(ACanvas: TCanvas; ARowHeight: SmallInt; APFlags: TPaintFlags);
var x, aHelp, aLeft, aTop, aRight, aBottom: Integer;
    aGridLWTL, aGridLWBR: SmallInt;
begin
  aGridLWTL:=GridLineWidth div 2;
  aGridLWBR:=aGridLWTL+ GridLineWidth mod 2;
  if aGridLWBR>0 then
    begin
      PreparePenStyle(ACanvas);
      aLeft:=ACanvas.ClipRect.Left;
      aTop:=ACanvas.ClipRect.Top;
      aRight:=ACanvas.ClipRect.Right;
      aBottom:=ACanvas.ClipRect.Bottom;
      ACanvas.Pen.Width:=aGridLWBR;
      if not (pfNonHorScroll in APFlags)
        then aHelp:=Columns[high(VisiCols)].Right-ClientAreaLeft-FixedColsWidth
        else aHelp:=ACanvas.Width;
      if not (pfRightToLeft in APFlags) then
        begin
          if (egoVerticalLines in Options) or (aRight=aHelp) then
            begin
              x:=aRight- (aGridLWBR+1) div 2;
              ACanvas.Line(x, aTop, x, aBottom);
            end;
          if (aLeft=0) and ((pfNonHorScroll in APFlags) or ((ClientAreaLeft=0) and (FixedCols=0))) then
            begin
              x:=aLeft+ aGridLWBR div 2;
              ACanvas.Line(x, aTop, x, aBottom);
            end else
            if (aGridLWTL>0) and (egoVerticalLines in Options) then
              begin
                ACanvas.Pen.Width:=aGridLWTL;
                x:=aLeft+ aGridLWTL div 2;
                ACanvas.Line(x, aTop, x, aBottom);
              end;
        end else
        begin
          if (egoVerticalLines in Options) or (aLeft=(ACanvas.Width-aHelp)) then
            begin
              x:=aLeft+ aGridLWBR div 2;
              ACanvas.Line(x, aTop, x, aBottom);
            end;
          if (aRight=ACanvas.Width) and ((pfNonHorScroll in APFlags) or ((ClientAreaLeft=0) and (FixedCols=0))) then
            begin
              x:=aRight- (aGridLWBR+1) div 2;
              ACanvas.Line(x, aTop, x, aBottom);
            end else
            if (aGridLWTL>0) and (egoVerticalLines in Options) then
              begin
                ACanvas.Pen.Width:=aGridLWTL;
                x:=aRight- (aGridLWTL+1) div 2;
                ACanvas.Line(x, aTop, x, aBottom);
              end;
        end;
      if (aTop=0) and ((pfTopRow in APFlags) or ((aGridLWTL>0) and not (egoHorizontalLines in Options))) then
        begin
          include(APFlags, pfTopRow);
          ACanvas.Pen.Width:=aGridLWBR;
          aHelp:=aTop+ aGridLWBR div 2;
          ACanvas.Line(aLeft, aHelp, aRight, aHelp);
        end;
      if (aGridLWTL>0) and (egoHorizontalLines in Options) then
        begin
          aHelp:=aGridLWTL div 2;
          if pfTopRow in APFlags then inc(aHelp, ARowHeight);
          ACanvas.Pen.Width:=aGridLWTL;
          while aHelp<aBottom do
            begin
              ACanvas.Line(aLeft, aHelp, aRight, aHelp);
              inc(aHelp, ARowHeight);
            end;
        end;
      aHelp:=(-aGridLWBR-1) div 2;
      if egoHorizontalLines in Options
        then inc(aHelp, aTop+ARowHeight)
        else if pfBottomRow in APFlags
               then inc(aHelp, (aBottom div ARowHeight)*ARowHeight)
               else aBottom:=aHelp-1;  { no lines needed }
      ACanvas.Pen.Width:=aGridLWBR;
      while aHelp<aBottom do
        begin
          ACanvas.Line(aLeft, aHelp, aRight, aHelp);
          inc(aHelp, ARowHeight);
        end;
    end;
end;

procedure TCustomECGrid.DrawDataCell(ACol, ARow: Integer; ASelection: TSelectionMode);
var aBkgndColors: TBkgndColors;
    aPFlags: TPaintFlags;
begin
  aPFlags:=GetCommonPaintFlags;
  GetDataPaintFlags((ASelection=esmSelect) or (Col=ACol), aPFlags);
  GetColumnBkgndColors(ACol, aPFlags, aBkgndColors);
  Columns[ACol].FontOptions.ApplyTo(BMPData.Canvas.Font, clWindowText);
  DrawDataCellContent(ACol, ARow, aPFlags, aBkgndColors);
  BMPData.Canvas.Pen.Color:=GetColorResolvingDefAndEnabled(GridLineColor, clBtnFace, pfEnabled in aPFlags);
  if ARow=0 then include(aPFlags, pfTopRow);
  if ARow=(RowCount-1) then include(aPFlags, pfBottomRow);
  DrawColumnLines(BMPData.Canvas, RowHeight, aPFlags);
end;

procedure TCustomECGrid.DrawDataCellContent(ACol, ARow: Integer; APFlags: TPaintFlags;
            const ABkgndColors: TBkgndColors);
var aColor: TColor;
    aColumn: TECGColumn;
    aDataRow: Integer;
    aHandled: Boolean;
    aHelp: SmallInt;
    aRect: TRect;
    aText: string;
begin                      /////////////////////////
  aColumn:=Columns[ACol];
  aRect.Left:=aColumn.Left-ClientAreaLeft-FixedColsWidth;
  if pfRightToLeft in APFlags then aRect.Left:=BMPData.Width-aRect.Left-aColumn.Width;
  aDataRow:=ARow-FixedRows;
  aHelp:=RowHeight;
  aRect.Top:=(aDataRow-ClientAreaTop div aHelp)*aHelp;
//  writeln('DDCC aCol, aR.Top ',  aCol, ' ', aRect.Top);
  aRect.Right:=aRect.Left+aColumn.Width;
  aRect.Bottom:=aRect.Top+aHelp;
  BMPData.Canvas.ClipRect:=aRect;
  if ((pfSelectedCol in APFlags) and (ARow=Row) and not (pfEditorMode in APFlags))
    then include(APFlags, pfSelectedCell)
    else exclude(APFlags, pfSelectedCell);
  if not (pfSelectedCell in APFlags)
    then BMPData.Canvas.Brush.Color:=ABkgndColors[(aDataRow mod 2)>0,
           (pfHilighted in APFlags) or ((pfHilightRow in APFlags) and (ARow=Row))]
    else BMPData.Canvas.Brush.Color:=BkSelColors[Focused, pfEnabled in APFlags];
  BMPData.Canvas.Brush.Style:=bsSolid;
  BMPData.Canvas.FillRect(aRect);
  aHandled:=False;
  if assigned(OnDrawDataCell) then OnDrawDataCell(self, BMPData.Canvas, ACol, ARow, aHandled);
  if not aHandled and assigned(aColumn.OnGetDataCellText) and
    not ((pfSelectedCol in APFlags) and (ARow=Row) and (pfEditorMode in APFlags) and Editor.Visible) then
    begin
      aColumn.OnGetDataCellText(aColumn, aDataRow, aText);
      if aText<>'' then
        begin
          aHelp:=cIndent+ GridLineWidth div 2;
          inc(aRect.Left, aHelp);
          dec(aRect.Right, aHelp);
          if not (pfSelectedCell in APFlags)
            then aColor:=GetColorResolvingDefault(aColumn.FontOptions.FontColor, clWindowText)
            else aColor:=clHighlightText;
          if not (pfEnabled in APFlags) then aColor:=GetMonochromaticColor(aColor);
          BMPData.Canvas.Font.Color:=aColor;
          ThemeServices.DrawText(BMPData.Canvas, DetailsText[True], aText, aRect,
            cBaseFlags or cFlagsAlign[aColumn.Alignment, pfRightToLeft in APFlags], 0);
        end;
    end;
end;

procedure TCustomECGrid.DrawDataColumn(ACol: Integer);
var j, aRowCount: Integer;
    aBkgndColors: TBkgndColors;
    aPFlags: TPaintFlags;
begin
  aPFlags:=GetCommonPaintFlags;
  GetDataPaintFlags(ACol=Col, aPFlags);
  GetColumnBkgndColors(ACol, aPFlags, aBkgndColors);
  Columns[ACol].FontOptions.ApplyTo(BMPData.Canvas.Font, clWindowText);
  aRowCount:=RowCount;
  for j:=FirstVisiCell.Y to Math.min(FirstVisiCell.Y+ BMPData.Height div RowHeight, aRowCount)-1 do
    DrawDataCellContent(ACol, j, aPFlags, aBkgndColors);
  BMPData.Canvas.Pen.Color:=GetColorResolvingDefAndEnabled(GridLineColor, clBtnFace, pfEnabled in aPFlags);
  if FirstVisiCell.Y=0 then include(aPFlags, pfTopRow);
  if LastVisiCell.Y=(aRowCount-1) then include(aPFlags, pfBottomRow);
  with BMPData.Canvas do  { ClipRect remains set from DrawDataCellContent(); }
    ClipRect:=Rect(ClipRect.Left, 0, ClipRect.Right, ClipRect.Bottom);
  DrawColumnLines(BMPData.Canvas, RowHeight, aPFlags);
  exclude(Columns[ACol].Flags, ecfRedrawData);
end;

procedure TCustomECGrid.DrawDataColVertScrolled(ACol, ARowShift: Integer);
var j, aFrom, aTo: Integer;
    aCanvas: TCanvas;
    aRectA, aRectB: TRect;
begin
  aCanvas:=BMPData.Canvas;
  aFrom:=Columns[ACol].Left-ClientAreaLeft-FixedColsWidth;
  aTo:=Columns[ACol].Width;
  if egfRightToLeft in Flags then aFrom:=aCanvas.Width-aFrom-aTo;
  aRectA:=Rect(Math.max(aFrom, 0), 0, Math.min(aFrom+aTo, aCanvas.Width), aCanvas.Height);
  aCanvas.ClipRect:=aRectA;
  aRectB:=aRectA;
  j:=abs(ARowShift*RowHeight);
  dec(aRectA.Bottom, j);
  inc(aRectB.Top, j);
  if ARowShift<0 then
    begin  { Up }
      aCanvas.CopyRect(aRectB, aCanvas, aRectA);
      aFrom:=FirstVisiCell.Y;
      aTo:=aFrom-ARowShift-1;
    end else
    begin  { Down }
      aCanvas.CopyRect(aRectA, aCanvas, aRectB);
      j:=aCanvas.Height div RowHeight;
      aTo:=Math.min(FirstVisiCell.Y+j, RowCount)-1;
      aFrom:=aTo-ARowShift+1;
      if (FirstVisiCell.PrevY+j)>aFrom then inc(aFrom);
    end;
  for j:=aFrom to aTo do
    DrawDataCell(ACol, j);
end;

procedure TCustomECGrid.DrawFixedCellBkgnd(ACanvas: TCanvas; const ARect: TRect; APushed: Boolean);
const cBevel: array[egsPanel..egsFinePanelC] of SmallInt = (1, 2, 2, 3, 4);
begin
  if not APushed then
    case Style of
      egsPanel, egsStandard: ACanvas.DrawPanelBackground
        (aRect, bvNone, bvRaised, cBevel[Style], clBtnFace);
      egsFinePanelA, egsFinePanelB, egsFinePanelC: ACanvas.DrawFinePanelBkgnd
        (aRect, bvRaised, cBevel[Style], clDefault, clDefault, clBtnFace, True);
      egsThemed: ThemeServices.DrawElement(ACanvas.Handle, Details, aRect);
    end else
    case Style of
      egsPanel..egsFinePanelC: ACanvas.DrawPanelBackground
        (aRect, bvNone, bvLowered, 0, 1, clDefault, clBtnShadow, clBtnFace);
      egsThemed: ThemeServices.DrawElement
        (ACanvas.Handle, ThemeServices.GetElementDetails(thHeaderItemPressed), aRect);
    end;
end;

procedure TCustomECGrid.DrawFixedColumn(ACol, AFirstRow, ALastRow: Integer);
var j: Integer;
    aCanvas: TCanvas;
    aColumn: TECGColumn;
    aFlags: Cardinal;
    aIndent: SmallInt;
    aPFlags: TPaintFlags;
    aRect: TRect;
    aText: string;
begin
  aColumn:=Columns[ACol];
  aRect.Left:=aColumn.Left;
  if egfRightToLeft in Flags then aRect.Left:=BMPFixedCols.Width-aRect.Left-aColumn.Width;
  aRect.Top:=(AFirstRow-FixedRows)*RowHeight-(ClientAreaTop div RowHeight)*RowHeight;
  aRect.Right:=aRect.Left+aColumn.Width;
  aRect.Bottom:=aRect.Top+(ALastRow-AFirstRow+1)*RowHeight;
  aCanvas:=BMPFixedCols.Canvas;
  aCanvas.ClipRect:=aRect;
  aPFlags:=GetCommonPaintFlags;
  if AFirstRow=0 then include(aPFlags, pfTopRow);
  if ALastRow=(RowCount-1) then include(aPFlags, pfBottomRow);
  if Style=egsFlat then
    begin
      include(aPFlags, pfNonHorScroll);
      aCanvas.Brush.Color:=GetColorResolvingEnabled(clBtnFace, pfEnabled in aPFlags);
      aCanvas.Brush.Style:=bsSolid;
      aCanvas.FillRect(aRect);
      aCanvas.Pen.Color:=GetColorResolvingEnabled(clBtnText, pfEnabled in aPFlags);
      DrawColumnLines(aCanvas, RowHeight, aPFlags);
    end;
  if assigned(aColumn.OnGetDataCellText)
    then include(aPFlags, pfHasColEvent)
    else exclude(aPFlags, pfHasColEvent);
  aFlags:=cBaseFlags or cFlagsAlign[aColumn.Alignment, pfRightToLeft in aPFlags];
  aCanvas.Font.Assign(Font);
  aColumn.FontOptions.ApplyTo(aCanvas.Font, clBtnText);
  aIndent:=FixedCellIndent;
  for j:=AFirstRow to ALastRow do
    begin
      aRect.Bottom:=aRect.Top+RowHeight;
      DrawFixedCellBkgnd(aCanvas, aRect, False);
      if pfHasColEvent in aPFlags then
        begin
          inc(aRect.Left, aIndent);
          dec(aRect.Right, aIndent);
          aColumn.OnGetDataCellText(aColumn, j-FixedRows, aText);
          ThemeServices.DrawText(aCanvas, DetailsText[pfEnabled in APFlags], aText, aRect, AFlags, 0);
          dec(aRect.Left, aIndent);
          inc(aRect.Right, aIndent);
        end;
      aRect.Top:=aRect.Bottom;
    end;
end;

procedure TCustomECGrid.DrawFixedColsVertScrolled(ARowShift: Integer);
var k, aFrom, aTo: Integer;
    aCanvas: TCanvas;
    aRectA, aRectB: TRect;
begin
  aCanvas:=BMPFixedCols.Canvas;
  aRectA:=Rect(0, 0, aCanvas.Width, aCanvas.Height);
  aCanvas.ClipRect:=aRectA;
  aRectB:=aRectA;
  k:=abs(ARowShift*RowHeight);
  dec(aRectA.Bottom, k);
  inc(aRectB.Top, k);
  if ARowShift<0 then
    begin  { Up }
      aCanvas.CopyRect(aRectB, aCanvas, aRectA);
      aFrom:=FirstVisiCell.Y;
      aTo:=aFrom-ARowShift-1;
    end else
    begin  { Down }
      aCanvas.CopyRect(aRectA, aCanvas, aRectB);
      k:=aCanvas.Height div RowHeight;
      aTo:=Math.min(FirstVisiCell.Y+k, RowCount)-1;
      aFrom:=aTo-ARowShift+1;
      if (FirstVisiCell.PrevY+k)>aFrom then inc(aFrom);
    end;
  for k:=0 to FixedCols-1 do
    DrawFixedColumn(k, aFrom, aTo);
end;

procedure TCustomECGrid.DrawGridHorScrolled(ACliAreaLeftShift: Integer);
var i, k: Integer;
    aRectA, aRectB: TRect;
begin
  BMPData.Canvas.ClipRect:=Rect(0, 0, BMPData.Width, BMPData.Height);
  BMPFixedRows.Canvas.ClipRect:=Rect(0, 0, BMPFixedRows.Width, BMPFixedRows.Height);
  i:=abs(ACliAreaLeftShift);
  k:=ord(egfRightToLeft in Flags);
  aRectA:=Rect(k*i, 0, BMPData.Width-(k xor 1)*i, BMPData.Height);
  aRectB:=Rect((k xor 1)*i, 0, BMPData.Width-k*i, BMPData.Height);
  if ACliAreaLeftShift<0
    then i:=Columns[VisiCols[FirstVisiCell.PrevX]].Left+ACliAreaLeftShift-FixedColsWidth
    else i:=Columns[VisiCols[LastVisiCell.PrevX]].Right+ACliAreaLeftShift-ClientWidth;
  k:=ord(i=ClientAreaLeft);
  if (FirstVisiCell.Y<=LastVisiCell.Y) and not (egfRedrawData in Flags) then
    if ACliAreaLeftShift<0 then
      begin
        BMPData.Canvas.CopyRect(aRectB, BMPData.Canvas, aRectA);
        for i:=FirstVisiCell.X to FirstVisiCell.PrevX-k do
          DrawDataColumn(i);
      end else
      begin
        BMPData.Canvas.CopyRect(aRectA, BMPData.Canvas, aRectB);
        for i:=LastVisiCell.PrevX+k to LastVisiCell.X do
          DrawDataColumn(i);
      end;
  if (FixedRows>0) and not (egfRedrawFixedRows in Flags) then
    begin
      aRectA.Bottom:=FixedRows*FixedRowHeight;
      aRectB.Bottom:=aRectA.Bottom;
      if ACliAreaLeftShift<0 then
        begin
          BMPFixedRows.Canvas.CopyRect(aRectB, BMPFixedRows.Canvas, aRectA);
          for i:=FirstVisiCell.X to FirstVisiCell.PrevX-k do
            for k:=0 to FixedRows-1 do
              DrawHeaderCell(i, k);
        end else
        begin
          BMPFixedRows.Canvas.CopyRect(aRectA, BMPFixedRows.Canvas, aRectB);
          for i:=LastVisiCell.PrevX+k to LastVisiCell.X do
            for k:=0 to FixedRows-1 do
              DrawHeaderCell(i, k);
        end;
    end;
end;

procedure TCustomECGrid.DrawHeaderCell(ACol, ARow: Integer);
var aCanvas: TCanvas;
    aPFlags: TPaintFlags;
    aRect: TRect;
begin
  if IsColVisible(ACol) then
    begin
      aPFlags:=[];
      aRect.Left:=Columns[ACol].Left;
      if egfRightToLeft in Flags then include(aPFlags, pfRightToLeft);
      if ACol>=FixedCols then
        begin
          aCanvas:=BMPFixedRows.Canvas;
          dec(aRect.Left, ClientAreaLeft+FixedColsWidth);
          if pfRightToLeft in aPFlags then aRect.Left:=BMPFixedRows.Width-aRect.Left-Columns[ACol].Width;
        end else
        begin
          aCanvas:=BMPHead.Canvas;
          if pfRightToLeft in aPFlags then aRect.Left:=BMPHead.Width-aRect.Left-Columns[ACol].Width;
          include(aPFlags, pfNonHorScroll);
        end;
      aRect.Right:=aRect.Left+Columns[ACol].Width;
      aRect.Top:=ARow*FixedRowHeight;
      aRect.Bottom:=aRect.Top+FixedRowHeight;
      aCanvas.ClipRect:=aRect;
      if IsEnabled then include(aPFlags, pfEnabled);
      if assigned(Images) then include(aPFlags, pfImages);
      if Style=egsFlat then
        begin
          aCanvas.Brush.Color:=GetColorResolvingEnabled(clBtnFace, pfEnabled in aPFlags);
          aCanvas.Brush.Style:=bsSolid;
          aCanvas.FillRect(aRect);
          aCanvas.Pen.Color:=GetColorResolvingEnabled(clBtnText, pfEnabled in aPFlags);
          if ARow=0 then include(aPFlags, pfTopRow);
          if ARow=FixedRows-1 then include(aPFlags, pfBottomRow);
          DrawColumnLines(aCanvas, FixedRowHeight, aPFlags);
        end else
        DrawFixedCellBkgnd(aCanvas, aRect,
          (PushedCol=ACol) and (PushedRow=ARow) and (egoHeaderPushedLook in Options));
      inc(aRect.Left, FixedCellIndent);
      dec(aRect.Right, FixedCellIndent);
      DrawHeaderCellContent(aCanvas, ACol, ARow, aRect, aPFlags);
    end;
end;

procedure TCustomECGrid.DrawHeaderCellContent(ACanvas: TCanvas; ACol, ARow: Integer; ARect: TRect; APFlags: TPaintFlags);
const cFlagsAlign: array[TAlignment, Boolean] of Cardinal = ((DT_LEFT, DT_RIGHT or DT_RTLREADING),
        (DT_RIGHT, DT_LEFT or DT_RTLREADING), (DT_CENTER, DT_CENTER or DT_RTLREADING));
      cSortArrows: array[Boolean] of TGlyphDesign = (egdSizeArrUp, egdSizeArrDown);
      cGlyphIndent: SmallInt = 12;
var y: Integer;
    aFlags: Cardinal;
    aText: string;
begin
  if ARow=0 then
    begin  { draw title images and assign text }
      aText:=Columns[ACol].Title.Text;
      if (pfImages in APFlags) and (Columns[ACol].Title.ImageIndex>=0) then
        begin
          y:=(ARect.Top+ARect.Bottom-Images.Height) div 2;
          if not (pfRightToLeft in APFlags) then
            begin
              ThemeServices.DrawIcon(ACanvas, DetailsText[pfEnabled in APFlags], Point(ARect.Left, y),
                                     Images, Columns[ACol].Title.ImageIndex);
              inc(ARect.Left, Images.Width+cIndent);
            end else
            begin
              dec(ARect.Right, Images.Width);
              ThemeServices.DrawIcon(ACanvas, DetailsText[pfEnabled in APFlags], Point(ARect.Right, y),
                                     Images, Columns[ACol].Title.ImageIndex);
              dec(ARect.Right, cIndent);
            end;
        end;
    end else
    begin
      aText:='';
      if assigned(OnGetHeaderText) then OnGetHeaderText(self, ACol, ARow, aText);
    end;
  if aText<>'' then
    begin  { draw text }
      aFlags:=cBaseFlags or cFlagsAlign[Columns[ACol].Title.Alignment, pfRightToLeft in APFlags];
      ACanvas.Font.Assign(Font);
      Columns[ACol].Title.FontOptions.ApplyTo(ACanvas.Font, clBtnText);
      ThemeServices.DrawText(ACanvas, DetailsText[pfEnabled in APFlags], aText, ARect, aFlags, 0);
    end;
  if (ARow=0) and (ACol=SortIndex) and (egoSortArrow in Options) and (ecoSorting in Columns[ACol].Options) then
    begin  { draw sort-arrow }
      if not (pfRightToLeft in APFlags)
        then ARect.Left:=ARect.Right-cGlyphIndent
        else ARect.Right:=ARect.Left+cGlyphIndent;
      ACanvas.Pen.Color:=GetColorResolvingEnabled(clBtnText, pfEnabled in APFlags);
      ACanvas.DrawGlyph(ARect, cSortArrows[SortAscendent]);
    end;
end;

procedure TCustomECGrid.EnabledChanged;
begin
  Flags:=Flags+cRedrawGrid;
  inherited EnabledChanged;
end;

procedure TCustomECGrid.EndUpdate;
begin
  inherited EndUpdate;
  ControlStyle:=ControlStyle-[csNoStdEvents];
end;

procedure TCustomECGrid.GetColumnBkgndColors(ACol: Integer; APFlags: TPaintFlags; out AColors: TBkgndColors);
var bColor, bHighlighted: Boolean;
begin
  if Columns[ACol].Color=clDefault then
    begin
      for bColor:=False to True do
        AColors[bColor, False]:=BkgndColors[bColor, False, pfEnabled in APFlags];
      if (pfHilighted in APFlags) or (pfHilightRow in APFlags) then
        for bColor:=False to True do
          AColors[bColor, True]:=BkgndColors[bColor, True, pfEnabled in APFlags];
    end else
    begin
      AColors[False, False]:=GetMergedColor(Columns[ACol].Color,
        BkgndColors[False, False, pfEnabled in APFlags], 0.01*Columns[ACol].ColorTint);
      AColors[True, False]:=GetMergedColor(GetColorResolvingDefault(AlternateColor, Color),
                                                AColors[False, False], 0.01*AlternateTint);
      if (pfHilighted in APFlags) or (egoHilightRow in Options) then
        for bColor:=False to True do
          AColors[bColor, True]:=GetMergedColor(AColors[bColor, False], clHighlight, cMergeHilight);
      if not (pfEnabled in APFlags) then
        for bColor:=False to True do
          for bHighlighted:=False to True do
            AColors[bColor, bHighlighted]:=GetMonochromaticColor(AColors[bColor, bHighlighted]);
    end;
end;

function TCustomECGrid.GetCommonPaintFlags: TPaintFlags;
begin
  Result:=[];
  if IsEnabled then include(Result, pfEnabled);
  if assigned(Images) then include(Result, pfImages);
  if egfRightToLeft in Flags then include(Result, pfRightToLeft);
end;

class function TCustomECGrid.GetControlClassDefaultSize: TSize;
begin
  Result:=Size(320, 160);
end;

procedure TCustomECGrid.GetDataPaintFlags(ASelectedCol: Boolean; var APFlags: TPaintFlags);
begin
  if EditorMode and Editor.Visible then include(APFlags, pfEditorMode);
  if egoHilightRow in Options then include(APFlags, pfHilightRow);
  if ASelectedCol then
    begin
      include(APFlags, pfSelectedCol);
      if egoHilightCol in Options then include(APFlags, pfHilighted);
    end;
end;

function TCustomECGrid.GetIncrementX: Integer;
begin
  Result:=ScrollIncX;
end;

function TCustomECGrid.GetIncrementY: Integer;
begin
  Result:=RowHeight;
end;

function TCustomECGrid.GetPageSizeX: Integer;
begin
  Result:=ClientWidth-FixedColsWidth;
end;

function TCustomECGrid.GetPageSizeY: Integer;
begin
  Result:=DataAreaRows*RowHeight;
end;

procedure TCustomECGrid.GoToRowRelative(ARows: Integer; AScroll: Boolean = False; AForceFocus: Boolean = True);
var aRow, aRowCountM1: Integer;
begin
  if not AScroll then
    begin
      aRow:=Row;
      aRowCountM1:=RowCount-1;
      if ((ARows>0) and (aRow<aRowCountM1)) or ((ARows<0) and (aRow>FixedRows)) then
        begin
          inc(aRow, ARows);
          if ARows>0
            then aRow:=Math.min(aRow, aRowCountM1)
            else aRow:=Math.max(aRow, FixedRows);
          if not IsRowFullyVisible(aRow) then
            begin
              if aRow>=LastVisiCell.Y
                then ClientAreaTop:=(aRow-FixedRows-DataAreaRows+1)*RowHeight
                else ClientAreaTop:=(aRow-FixedRows)*RowHeight;
              Redraw([egfCalcBoundRows]);
            end;
          DoSelection(Col, aRow, esmNative, AForceFocus);
        end;
    end else
    begin
      ClientAreaTop:=ClientAreaTop+ARows*RowHeight;
      Redraw([egfCalcBoundRows]);
    end;
end;

procedure TCustomECGrid.GoToVisiColRelative(ACols: Integer; AScroll: Boolean = False);
var aNewCol: Integer;
begin
  aNewCol:=IndexToVisiColIndex(Col)+ACols;
  if ACols>0
    then aNewCol:=Math.min(high(VisiCols), aNewCol)
    else aNewCol:=Math.max(FixedVisiCols, aNewCol);
  aNewCol:=VisiCols[aNewCol];
  if not AScroll then
    begin
      if aNewCol<>Col then
        begin
          if not IsColFullyVisible(aNewCol) then
            begin
              if aNewCol>=LastVisiCell.X
                then ClientAreaLeft:=Columns[aNewCol].Right-ClientWidth
                else ClientAreaLeft:=Columns[aNewCol].Left-FixedColsWidth;
              Flags:=Flags+[egfCalcBoundCols, egfUpdateRAWidth];
              InvalidateNonUpdated;
            end;
          DoSelection(aNewCol, Row);
        end;
    end else
    begin
      if ACols<0
        then ClientAreaLeft:=ClientAreaLeft+ACols*Columns[FirstVisiCell.X].Width
        else ClientAreaLeft:=ClientAreaLeft+ACols*Columns[LastVisiCell.X].Width;
      Flags:=Flags+[egfCalcBoundCols, egfCalcColsLeft, egfUpdateRAWidth];
      InvalidateNonUpdated;
    end;
end;

function TCustomECGrid.IndexToVisiColIndex(ACol: Integer): Integer;
var i, aCnt, aMax, aMin: Integer;
begin
  Result:=-1;
  aMin:=0;
  aMax:=high(VisiCols);
  aCnt:=2;
  if aMax>0 then inc(aCnt, round(log2(aMax)));
  while aCnt>0 do
    begin
      i:=(aMax+aMin) div 2;
      if VisiCols[i]<=ACol then
        begin
          aMin:=i;
          if VisiCols[i]=ACol then
            begin
              Result:=i;
              break;
            end;
          if (aMax-aMin)=1 then inc(aMin);
        end else
        aMax:=i;
      dec(aCnt);
    end;
end;

procedure TCustomECGrid.InitializeWnd;
begin
  inherited InitializeWnd;
  if csDesigning in ComponentState
    then FRowCountHelp:=4
    else FRowCountHelp:=0;
  PrevClientSize:=Size(ClientWidth, ClientHeight);
  DoChanges;
end;

function TCustomECGrid.IsCellFullyVisible(ACol, ARow: Integer): Boolean;
begin
  Result:= (IsRowFullyVisible(ARow) and IsColFullyVisible(ACol));
end;

function TCustomECGrid.IsCellVisible(ACol, ARow: Integer): Boolean;
begin
  Result:= (IsRowVisible(ARow) and IsColVisible(ACol));
end;

function TCustomECGrid.IsColFullyVisible(ACol: Integer): Boolean;
begin
  Result:= ((ACol>=0) and (ecoVisible in Columns[ACol].Options));
  if Result then
    begin
      Result:= (Columns[ACol].Right<=(ClientAreaLeft+ClientWidth));
      if Result and (ACol>=FixedCols) then Result:= ((Columns[ACol].Left-FixedColsWidth)>=ClientAreaLeft);
    end;
end;

function TCustomECGrid.IsColVisible(ACol: Integer): Boolean;
begin
  if ACol>=FixedCols
    then Result:= (ACol>=VisiCols[FirstVisiCell.X]) and (ACol<=VisiCols[LastVisiCell.X])
    else Result:= (Columns[ACol].Left<ClientWidth);
  Result:=Result and (ecoVisible in Columns[ACol].Options);
end;

function TCustomECGrid.IsRowFullyVisible(ARow: Integer): Boolean;
var y: Integer;
begin
  if ARow<FixedRows
    then Result:= ((ARow>=0) and (((ARow+1)*FixedRowHeight)<=ClientHeight))
    else
    begin
      y:=FirstVisiCell.Y;
      if not (egfFirstRowFullyVisi in Flags) then inc(y);
      Result:= (ARow>=y);
      if Result then
        begin
          y:=LastVisiCell.Y;
          if not (egfLastRowFullyVisi in Flags) then dec(y);
          Result:= (ARow<=y);
        end;
    end;
end;

function TCustomECGrid.IsRowVisible(ARow: Integer): Boolean;
begin
  if ARow<FixedRows
    then Result:= ((ARow>=0) and ((ARow*FixedRowHeight)<ClientHeight))
    else Result:= ((ARow>=FirstVisiCell.Y) and (ARow<=LastVisiCell.Y));
end;

procedure TCustomECGrid.KeyDown(var Key: Word; Shift: TShiftState);
var aStr: string;
    bSelectEd: Boolean;
begin
  inherited KeyDown(Key, Shift);
  if ((length(VisiCols)-FixedVisiCols)>0) and ((RowCount-FixedRows)>0) then
    begin
      bSelectEd:=False;
      case Key of
        VK_BACK: bSelectEd:=True;
        VK_TAB:
          if egoTabs in Options then
            begin
              if not (ssShift in Shift) then
                begin
                  if Col<VisiCols[high(VisiCols)]
                    then GoToVisiColRelative(1)
                    else if Row<(RowCount-1) then
                           begin
                             Col:=VisiCols[FixedCols];
                             GoToRowRelative(1);
                           end;
                end else
                begin
                  if Col>VisiCols[FixedVisiCols]
                    then GoToVisiColRelative(-1)
                    else if Row>FixedRows then
                           begin
                             Col:=VisiCols[high(VisiCols)];
                             GoToRowRelative(-1);
                           end;
                end;
              Key:=0;
            end;
        VK_RETURN, VK_SPACE: bSelectEd:=True;
        VK_PRIOR: GoToRowRelative(-DataAreaRows+1, ssModifier in Shift);
        VK_NEXT:  GoToRowRelative(DataAreaRows-1, ssModifier in Shift);
        VK_END:   if not (ssModifier in Shift)
                    then Col:=VisiCols[high(VisiCols)]
                    else GoToRowRelative(RowCount-Row-1);
        VK_HOME:  if not (ssModifier in Shift)
                    then Col:=VisiCols[FixedVisiCols]
                    else GoToRowRelative(-Row+FixedRows);
        VK_LEFT:  if not (egfRightToLeft in Flags)
                    then GoToVisiColRelative(-1, ssModifier in Shift)
                    else GoToVisiColRelative(1, ssModifier in Shift);
        VK_UP:    GoToRowRelative(-1, ssModifier in Shift);
        VK_RIGHT: if not (egfRightToLeft in Flags)
                    then GoToVisiColRelative(1, ssModifier in Shift)
                    else GoToVisiColRelative(-1, ssModifier in Shift);
        VK_DOWN:  GoToRowRelative(1, ssModifier in Shift);
        VK_DELETE:  bSelectEd:=True;
        VK_0..VK_9: bSelectEd:=True;
        VK_A, VK_B: bSelectEd:=True;
        VK_C:
          if ssModifier in Shift then
            begin
              if (Row>=FixedRows) and (Col>=FixedCols) then
                if assigned(Columns[Col].OnGetDataCellText) then
                  begin
                    aStr:=Columns[Col].Cells[Row];
                    if aStr<>'' then Clipboard.AsText:=aStr;
                    Key:=0;
                  end;
            end else
            bSelectEd:=True;
        VK_D..VK_Z: bSelectEd:=True;
        VK_F2: bSelectEd:=True;
      end;  {case}
      if Key in [VK_PRIOR..VK_DOWN]
        then Key:=0
        else if bSelectEd and not EditorMode and not (egoReadOnly in Options) and
               not (ecoReadOnly in Columns[Col].Options) then
                 begin
                   MakeCellFullyVisible(Col, Row, False);
                   DoSelectEditor(Col, True, Key);
                 end;
    end;
end;

procedure TCustomECGrid.LoadColumnsFromXML(AFileName: string; AColumnsNode: DOMString; AXMLFlags: TXMLColFlags);
var XMLDoc: TXMLDocument;
    aNode: TDOMNode;
begin
  XMLDoc:=nil;
  if FileExistsUTF8(AFileName) then
    ReadXMLFile(XMLDoc, AFileName, [xrfAllowSpecialCharsInAttributeValue]);
  if assigned(XMLDoc) then
    try
      if assigned(XMLDoc.DocumentElement) then
        begin
          if AColumnsNode='' then AColumnsNode:=Name+'_'+cColumn+'s';
          aNode:=XMLDoc.DocumentElement.FindNode(AColumnsNode);
          if assigned(aNode) then LoadColumnsFromXML(aNode, AXMLFlags);
        end;
    finally
      XMLDoc.Free;
    end;
end;

procedure TCustomECGrid.LoadColumnsFromXML(AColumnsNode: TDOMNode; AXMLFlags: TXMLColFlags);
var i, j, aCount, aOrder: Integer;
    aNode: TDOMNode;
begin
  BeginUpdate;
  try
    aCount:=Math.min(strToInt(TDOMElement(AColumnsNode).GetAttribute(cCount)), ColCount);
    if (egoUseOrder in Options) and (excfOrder in AXMLFlags) then
      begin
        aNode:=AColumnsNode.FirstChild;
        for i:=0 to aCount-1 do
          begin
            aOrder:=strToInt(TDOMElement(aNode).GetAttribute(cOrder));
            if Columns[i].Order<>aOrder then
              for j:=i+1 to aCount-1 do
                if Columns[j].Order=aOrder then
                  begin
                    Columns[j].Index:=i;
                    break;
                  end;
            aNode:=aNode.NextSibling;
          end;
      end;
    if excfVisible in AXMLFlags then
      begin
        aNode:=AColumnsNode.FirstChild;
        for i:=0 to aCount-1 do
          begin
            if not strToBool(TDOMElement(aNode).GetAttribute(cVisible))
              then Columns[i].Options:=Columns[i].Options-[ecoVisible]
              else Columns[i].Options:=Columns[i].Options+[ecoVisible];
             aNode:=aNode.NextSibling;
          end;
      end;
    if excfWidth in AXMLFlags then
      begin
        aNode:=AColumnsNode.FirstChild;
        for i:=0 to aCount-1 do
          begin
            Columns[i].Width:=strToInt(TDOMElement(aNode).GetAttribute(cWidth));
            aNode:=aNode.NextSibling;
          end;
      end;
  finally
    EndUpdate;
  end;
end;

function TCustomECGrid.MakeCellFullyVisible(ACol, ARow: Integer; AForce: Boolean): Boolean;
var bInvalidate, bScrolled: Boolean;
begin
  bInvalidate:=False;
  if AForce then
    begin
      Columns[ACol].Options:=Columns[ACol].Options+[ecoVisible];
      DoChanges;
      bInvalidate:=True;
      Result:=True;
    end else
    Result:= (ecoVisible in Columns[ACol].Options);
  if Result then
    begin
      if ARow>=FixedRows then
        begin
          bScrolled:=False;
          if (ARow<FirstVisiCell.Y) or ((ARow=FirstVisiCell.Y) and not (egfFirstRowFullyVisi in Flags)) then
            begin
              ClientAreaTop:=(ARow-FixedRows)*RowHeight;
              bScrolled:=True;
            end else
            if (ARow>LastVisiCell.Y) or ((ARow=LastVisiCell.Y) and not (egfLastRowFullyVisi in Flags)) then
              begin
                ClientAreaTop:=(ARow-FixedRows+1)*RowHeight-ClientHeight+FixedRows*FixedRowHeight;
                bScrolled:=True;
              end;
          if bScrolled then
            begin
              Flags:=Flags+[egfCalcBoundRows, egfUpdateRAHeight];
              bInvalidate:=True;
            end;
        end;
      if ACol>=FixedCols then
        begin
          bScrolled:=False;
          if (ACol<FirstVisiCell.X) or ((Columns[ACol].Left-FixedColsWidth)<ClientAreaLeft) then
            begin
              ClientAreaLeft:=Columns[ACol].Left-FixedColsWidth;
              bScrolled:=True;
            end else
            if (ACol>LastVisiCell.X) or (Columns[ACol].Right>(ClientAreaLeft+ClientWidth)) then
              begin
                ClientAreaLeft:=Columns[ACol].Right-ClientWidth;
                bScrolled:=True;
              end;
          if bScrolled then
            begin
              Flags:=Flags+[egfCalcBoundCols, egfCalcColsLeft, egfUpdateRAWidth];
              bInvalidate:=True;
            end;
        end;
      if not IsColFullyVisible(ACol) then ClientAreaLeft:=Columns[ACol].Left-ClientWidth+FixedColsWidth;
      if bInvalidate then InvalidateNonUpdated;
    end;
end;

procedure TCustomECGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const cDragThreshold: SmallInt = 8;
      cSelect: array[Boolean] of TSelectionMode = (esmDontSelect, esmNative);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button=mbLeft) and ((SizableCol>=0) or ((HoveredCol>=0) and (HoveredRow>=0))) then
    begin
      if HoveredRow<FixedRows then
        begin
          if SizableCol>=0 then
            begin
              MouseCapture:=True;
              if egfRightToLeft in Flags then X:=-X;
              SizeInitX:=Columns[SizableCol].FWidth-X;
              if EditorMode then
                begin
                  if not (egfRightToLeft in Flags)
                    then SizeEdColXPos:=Columns[Col].Left
                    else SizeEdColXPos:=Columns[Col].Right;
                  SizeEdColWidth:=Columns[Col].Width;
                  Editor.Visible:=False;
                end;
              include(Flags, egfSizing);
            end else
            begin
              if MovableCol>=0 then BeginDrag(False, cDragThreshold);
              PushedCol:=HoveredCol;
              PushedRow:=HoveredRow;
              if (egoHeaderPushedLook in Options) and (Style<>egsFlat) then
                begin
                  DrawHeaderCell(PushedCol, PushedRow);
                  InvalidateNonUpdated;
                end;
            end;
        end else
        if HoveredCol>=FixedCols then
          begin
            MakeCellFullyVisible(HoveredCol, HoveredRow, False);
            DoSelection(HoveredCol, HoveredRow);
          end else
          DoSelection(Col, HoveredRow, cSelect[egoAlwaysShowEditor in Options]);
    end;
  SetFocus;
end;

procedure TCustomECGrid.MouseLeave;
begin
  inherited MouseLeave;
  SizableCol:=-1;
  MovableCol:=-1;
end;

procedure TCustomECGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
const cSizeGrip: SmallInt = 6;
var aCol, aRow, aSizeCol, aVisiColsM1: Integer;
begin
  inherited MouseMove(Shift, X, Y);
  if [egfMoving, egfSizing]*Flags=[] then
    begin
      MouseToCell(Point(X, Math.min(Y, ClientHeight-1)), aCol, aRow);
      if ((HoveredCol<>aCol) or (HoveredRow<>aRow)) and ShowHint then ChangeHint(aCol, aRow);
      if InRangeCO(aRow, 0, FixedRows) then
        begin
          if egfRightToLeft in Flags then X:=ClientWidth-X;
          aSizeCol:=-1;
          if egoColSizing in Options then
            begin
              inc(X, ClientAreaLeft);
              if aCol>=0 then
                begin
                  if X<(Columns[aCol].Left+cSizeGrip) then
                    begin
                      aSizeCol:=aCol;
                      repeat
                        dec (aSizeCol);
                      until (aSizeCol<0) or (ecoVisible in Columns[aSizeCol].Options);
                    end else
                    if X>(Columns[aCol].Right-cSizeGrip) then aSizeCol:=aCol;
                end else
                begin
                  aVisiColsM1:=high(VisiCols);
                  if (aVisiColsM1>=0) and Math.InRange(X-Columns[VisiCols[aVisiColsM1]].Right, 0, cSizeGrip)
                    then aSizeCol:=VisiCols[aVisiColsM1];
                end;
              if (aSizeCol>=0) and not (ecoSizing in Columns[aSizeCol].Options) then aSizeCol:=-1;
              SizableCol:=aSizeCol;
            end;
          if (aSizeCol<0) and not Dragging then
            if (egoColMoving in Options) and (aCol>=FixedCols)
              then MovableCol:=aCol
              else MovableCol:=-1;
        end else
        begin
          SizableCol:=-1;
          if not Dragging then MovableCol:=-1;
        end;
      HoveredCol:=aCol;
      HoveredRow:=aRow;
    end else
    begin
      if egfSizing in Flags then
        begin
          if egfRightToLeft in Flags then X:=-X;
          Columns[SizableCol].Width:=X+SizeInitX;
        end;
    end;
end;

procedure TCustomECGrid.MouseToCell(AMouse: TPoint; out ACol, ARow: Integer);
var i, aCnt, aMax, aMin: Integer;
begin
  ACol:=-1;
  aMax:=high(VisiCols);
  if aMax>=0 then
    begin
      if egfRightToLeft in Flags then AMouse.X:=ClientWidth-AMouse.X;
      if AMouse.X>=0 then
        begin
          inc(AMouse.X, ClientAreaLeft);
          if AMouse.X<Columns[VisiCols[aMax]].Right then
            begin
              aMin:=0;
              aCnt:=2;
              if aMax>0 then inc(aCnt, round(log2(aMax)));
              while aCnt>0 do
                begin
                  i:=(aMax+aMin) div 2;
                  if Columns[VisiCols[i]].Left<=AMouse.X then
                    begin
                      aMin:=i;
                      ACol:=VisiCols[i];
                      if Columns[ACol].Right>AMouse.X then break;
                      if (aMax-aMin)=1 then inc(aMin);
                    end else
                    aMax:=i;
                  dec(aCnt);
                end;
            end;
        end;
    end;
  if InRangeCO(AMouse.Y, 0, FRequiredArea.Y) then
    begin
      if AMouse.Y>=(FixedRows*FixedRowHeight)
        then ARow:=FixedRows+ (AMouse.Y+ClientAreaTop-FixedRows*FixedRowHeight) div RowHeight
        else ARow:=AMouse.Y div FixedRowHeight;
    end else
    ARow:=-1;
end;

procedure TCustomECGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var aPushedCol: Integer;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Button=mbLeft then
    begin
      if not (egfSizing in Flags) then
        begin
          aPushedCol:=PushedCol;
          if aPushedCol>=0 then
            begin
              if not (egfMoving in Flags) then
                begin
                  if (aPushedCol=HoveredCol) and (PushedRow=HoveredRow) then
                    begin
                      if ecoSorting in Columns[aPushedCol].Options then
                        begin
                          if SortIndex=VisiCols[aPushedCol]
                            then SortAscendent:= not SortAscendent
                            else SortIndex:=VisiCols[aPushedCol];
                        end;
                      if assigned(OnHeaderClick) then OnHeaderClick(self, aPushedCol, PushedRow);
                    end;
                end else
                exclude(Flags, egfMoving);
              if (egoHeaderPushedLook in Options) and (Style<>egsFlat) then
                begin
                  PushedCol:=-1;
                  DrawHeaderCell(aPushedCol, PushedRow);
                  InvalidateNonUpdated;
                end;
            end;
        end else
        begin
          ChangeCursor(False);
          MouseCapture:=False;
          Flags:=Flags-[egfMoving, egfSizing];
          if EditorMode then
            if IsColFullyVisible(Col) then
              begin
                if not (egfRightToLeft in Flags)
                  then FEditor.Left:=FEditor.Left-SizeEdColXPos+Columns[Col].Left
                  else FEditor.Left:=FEditor.Left+SizeEdColXPos-Columns[Col].Right;
                FEditor.Width:=FEditor.Width-SizeEdColWidth+Columns[Col].Width;
                Editor.Visible:=True;
                if Editor.CanFocus then Editor.SetFocus;
              end else
              EditorMode:=False;
          FSizableCol:=-1;
        end;
    end;
end;

procedure TCustomECGrid.MoveEditor;
var aShift: Integer;
begin
  if IsColFullyVisible(Col) then
    begin
      aShift:=Columns[Col].Left-MoveEditorX;
      if egfRightToLeft in Flags then aShift:=-aShift;
      Editor.Left:=Editor.Left+aShift;
      MoveEditorX:=0;
    end else
    EditorMode:=False;
  exclude(Flags, egfMoveEditor);
end;

{$IFDEF DBGGRID} var PaintCnt: Integer; {$ENDIF}

procedure TCustomECGrid.Paint;
var i, j, k, aHeaderHeight, aRight, aShift: Integer;
    aColumn: TECGColumn;
    bHasDataRows, bR2L: Boolean;
    {$IFDEF DBGGRID} aTD: TDateTime; {$ENDIF}
begin
  inherited Paint;
  {$IFDEF DBGGRID} DebugLn('Paint Flags: ',
    boolToStr(egfRedrawFixedHeader in Flags, 'FH ', '-- '), boolToStr(egfRedrawFixedCols in Flags, 'FC ', '-- '),
    boolToStr(egfRedrawFixedRows in Flags, 'FR ', '-- '), boolToStr(egfRedrawData in Flags, 'DA ', '-- '));
  aTD:=Now; inc(PaintCnt); {$ENDIF}
  if length(VisiCols)>0 then
    begin
      aHeaderHeight:=FixedRows*FixedRowHeight;
      bHasDataRows:= (RowCount>FixedRows);
      aShift:=ClientAreaLeft-PrevClientArea.X;
      if abs(aShift)>=BMPData.Width then Flags:=Flags+[egfRedrawFixedRows, egfRedrawData];
      if abs(ClientAreaTop-PrevClientArea.Y)>=(ClientHeight-aHeaderHeight) then Flags:=Flags+[egfRedrawFixedCols, egfRedrawData];
      if egfRedrawData in Flags then
        for i:=FirstVisiCell.X to LastVisiCell.X do
          include(Columns[VisiCols[i]].Flags, ecfRedrawData);
      if egfRedrawFixedCols in Flags then
        for i:=0 to FixedVisiCols-1 do
          include(Columns[VisiCols[i]].Flags, ecfRedrawData);
      if egfRedrawFixedRows in Flags then
        for i:=FirstVisiCell.X to LastVisiCell.X do
          include(Columns[VisiCols[i]].Flags, ecfRedrawHeader);
      if egfRedrawFixedHeader in Flags then
        for i:=0 to FixedVisiCols-1 do
          include(Columns[VisiCols[i]].Flags, ecfRedrawHeader);
      if (aShift<>0) and (aShift<abs(BMPData.Width)) then DrawGridHorScrolled(aShift);
      aShift:=FirstVisiCell.Y-FirstVisiCell.PrevY;
      if (aShift<>0) and bHasDataRows and not (egfRedrawFixedCols in Flags) then DrawFixedColsVertScrolled(aShift);
      for i:=0 to FixedVisiCols-1 do
        begin
          aColumn:=Columns[VisiCols[i]];
          if ecfRedrawHeader in aColumn.Flags
            then for j:=0 to FixedRows-1 do
                   DrawHeaderCell(VisiCols[i], j)
            else if ecfRedrawTitle in aColumn.Flags then DrawHeaderCell(VisiCols[i], 0);
          if bHasDataRows and (ecfRedrawData in aColumn.Flags) then
            DrawFixedColumn(VisiCols[i], FirstVisiCell.Y, Math.min(FirstVisiCell.Y+BMPFixedCols.Height div RowHeight, RowCount-1));
          aColumn.Flags:=[];
        end;
      for i:=FirstVisiCell.X to LastVisiCell.X do
        begin
          aColumn:=Columns[VisiCols[i]];
          if ecfRedrawHeader in aColumn.Flags
            then for j:=0 to FixedRows-1 do
                   DrawHeaderCell(VisiCols[i], j)
            else if ecfRedrawTitle in aColumn.Flags then DrawHeaderCell(VisiCols[i], 0);
          if bHasDataRows then
            if not (ecfRedrawData in aColumn.Flags) then
              begin
                if aShift<>0 then DrawDataColVertScrolled(VisiCols[i], aShift);
                if ecfRedrawPrevSel in Columns[VisiCols[i]].Flags then DrawDataCell(PrevSel.X, PrevSel.Y);
                if ecfRedrawSelection in Columns[VisiCols[i]].Flags then DrawDataCell(Col, Row);
              end else
              DrawDataColumn(VisiCols[i]);
          aColumn.Flags:=[];
        end;
      i:=PrevSel.X;
      j:=PrevSel.Y;
      if (Col<>i) or (Row<>j) then
        begin
          if (i>=VisiCols[FirstVisiCell.X]) and (i<=VisiCols[LastVisiCell.X]) {and (j>=FirstVisiCell.Y)
            and (j<=LastVisiCell.Y)} and (ecoVisible in Columns[i].Options) then
              begin
                if not (egoHilightCol in Options) or (i=Col)
                  then include(Columns[i].Flags, ecfRedrawPrevSel)
                  else include(Columns[i].Flags, ecfRedrawData);
                if (egoHilightRow in Options) and (j<>Row) then
                  for k:=FirstVisiCell.X to LastVisiCell.X do
                    if VisiCols[k]<>i then DrawDataCell(VisiCols[k], j, esmDontSelect);
              end;
          i:=Col;
          j:=Row;
          if (i>=VisiCols[FirstVisiCell.X]) and (i<=VisiCols[LastVisiCell.X]) {and (j>=FirstVisiCell.Y)
            and (j<=LastVisiCell.Y)} and (ecoVisible in Columns[i].Options) then
              begin
                if not (egoHilightCol in Options) or (PrevSel.X=i)
                   then include(Columns[i].Flags, ecfRedrawSelection)
                   else include(Columns[i].Flags, ecfRedrawData);
                if (egoHilightRow in Options) and (j<>PrevSel.Y) then
                  for k:=FirstVisiCell.X to LastVisiCell.X do
                    if VisiCols[k]<>i then DrawDataCell(VisiCols[k], j, esmNative);
              end;
        end;
      PrevSel:=Point(Col, Row);
      Canvas.Clipping:=False;
      i:=FixedColsWidth;
      aRight:=Math.min(FRequiredArea.X, ClientWidth);
      bR2L:= (egfRightToLeft in Flags);
      if not bR2L then
        begin
          Canvas.Draw(0, 0, BMPHead);
          Canvas.CopyRect(Rect(i, 0, aRight, aHeaderHeight), BMPFixedRows.Canvas, Rect(0, 0, aRight-i, aHeaderHeight));
        end else
        begin
          Canvas.Draw(ClientWidth-i, 0, BMPHead);
          Canvas.CopyRect(Rect(ClientWidth-aRight, 0, ClientWidth-i, aHeaderHeight), BMPFixedRows.Canvas,
            Rect(BMPFixedRows.Width-aRight+i, 0, BMPFixedRows.Width, aHeaderHeight));
        end;
      aShift:=ClientAreaTop mod RowHeight;
      j:=Math.min(FRequiredArea.Y, ClientHeight);
      if not bR2L
        then Canvas.CopyRect(Rect(0, aHeaderHeight, i, j), BMPFixedCols.Canvas,
               Rect(0, aShift, i, j-aHeaderHeight+aShift))
        else Canvas.CopyRect(Rect(ClientWidth-i, aHeaderHeight, ClientWidth, j),
               BMPFixedCols.Canvas, Rect(0, aShift, i, j-aHeaderHeight+aShift));
      if not bR2L
        then Canvas.CopyRect(Rect(i, aHeaderHeight, aRight, j), BMPData.Canvas,
               Rect(0, aShift, aRight-i, j-aHeaderHeight+aShift))
        else Canvas.CopyRect(Rect(ClientWidth-aRight, aHeaderHeight, ClientWidth-i, j), BMPData.Canvas,
               Rect(BMPData.Width-aRight+i, aShift, BMPData.Width, j-aHeaderHeight+aShift));
      Flags:=Flags-cRedrawGrid;
      i:=ClientWidth;
      FirstVisiCell.PrevX:=FirstVisiCell.X;
      LastVisiCell.PrevX:=LastVisiCell.X;
      FirstVisiCell.PrevY:=FirstVisiCell.Y;
      LastVisiCell.PrevY:=LastVisiCell.Y;
      PrevClientArea.X:=ClientAreaLeft;
      PrevClientArea.Y:=ClientAreaTop;
    end;
  {$IFDEF DBGGRID} DebugLn('TCustomECGrid.Paint: ', inttostr(PaintCnt), 'x, ',
                     floatToStrF(24*3600*(Now-aTD), ffFixed, 0, 7)); {$ENDIF}
end;

procedure TCustomECGrid.PrepareEditorDeltaRect(AX, AY: Boolean);
var aCellRect: TRect;
begin
  aCellRect:=CellRect(Col, Row);
  if AX then
    begin
      EditorDeltaRect.Left:=Editor.Left-aCellRect.Left;
      EditorDeltaRect.Right:=Editor.Width-(aCellRect.Right-aCellRect.Left);
      include(Flags, egfCorrectEditorPosX);
    end;
  if AY then
    begin
      EditorDeltaRect.Top:=Editor.Top-aCellRect.Top;
      EditorDeltaRect.Bottom:=Editor.Height-(aCellRect.Bottom-aCellRect.Top);
      include(Flags, egfCorrectEditorPosY);
    end;
end;

procedure TCustomECGrid.PreparePenStyle(ACanvas: TCanvas);
begin
  if not (egoDottedGrid in Options)
    then ACanvas.Pen.Style:=psSolid
    else
    begin
      ACanvas.Pen.Style:=psDot;
      ACanvas.Brush.Style:=bsClear;
    end;
end;

procedure TCustomECGrid.Redraw(AGFlags: TGFlags);
begin
  Flags:=Flags+AGFlags;
  InvalidateNonUpdated;
end;

procedure TCustomECGrid.ResizeBMPs;
var aWidth, aHeight, aDataHeight, aDataWidth, aHeaderHeight: Integer;
begin
  aWidth:=BMPHead.Width;  { header }
  aHeight:=BMPHead.Height;
  aHeaderHeight:=FixedRows*FixedRowHeight;
  BMPHead.Width:=FixedColsWidth;
  BMPHead.Height:=aHeaderHeight;
  if (aWidth<>BMPHead.Width) or (aHeight<>BMPHead.Height) then include(Flags, egfRedrawFixedHeader);
  aDataWidth:=ClientWidth-FixedColsWidth;
  aWidth:=BMPFixedRows.Width;  { fixed rows }
  aHeight:=BMPFixedRows.Height;
  BMPFixedRows.Width:=aDataWidth;
  BMPFixedRows.Height:=aHeaderHeight;
  if (aWidth<>BMPFixedRows.Width) or (aHeight<>BMPFixedRows.Height) then include(Flags, egfRedrawFixedRows);
  aHeight:=ClientHeight-aHeaderHeight;
  aDataHeight:=aHeight div RowHeight +1;
  if (aHeight mod RowHeight)>=2 then inc(aDataHeight);
  aDataHeight:=aDataHeight*RowHeight;
  aWidth:=BMPFixedCols.Width;  { fixed cols }
  aHeight:=BMPFixedCols.Height;
  BMPFixedCols.Width:=FixedColsWidth;
  BMPFixedCols.Height:=aDataHeight;
  if (aWidth<>BMPFixedCols.Width) or (aHeight<>BMPFixedCols.Height) then include(Flags, egfRedrawFixedCols);
  aWidth:=BMPData.Width;  { data area }
  aHeight:=BMPData.Height;
  BMPData.Width:=aDataWidth;
  BMPData.Height:=aDataHeight;
  if (aWidth<>BMPData.Width) or (aHeight<>BMPData.Height) then include(Flags, egfRedrawData);
  exclude(Flags, egfResizeBMPs);
end;

{ saves to a file, i.e. xml file contains columns-config in <CONFIG><AColumnsNode>... }
procedure TCustomECGrid.SaveColumnsToXML(AFileName: string; AColumnsNode: DOMString; AXMLFlags: TXMLColFlags);
var XMLDoc: TXMLDocument;
    aNode: TDOMNode;
begin
  XMLDoc:=nil;
  if FileExistsUTF8(AFileName) then
    ReadXMLFile(XMLDoc, AFileName, [xrfAllowSpecialCharsInAttributeValue]);
  if not assigned(XMLDoc) then XMLDoc:=TXMLDocument.Create;
  try
    with XMLDoc do
      begin
        if not assigned(DocumentElement) then
          begin
            aNode:=CreateElement(cRoot);
            AppendChild(aNode);
          end;
        if AColumnsNode='' then AColumnsNode:=Name+'_'+cColumn+'s';
        aNode:=DocumentElement.FindNode(AColumnsNode);
        if not assigned(aNode) then
          begin
            aNode:=CreateElement(AColumnsNode);
            DocumentElement.AppendChild(aNode);
          end;
        SaveColumnsToXML(XMLDoc, aNode, AXMLFlags);
        WriteXMLFile(XMLDoc, AFileName);
      end;
  finally
    XMLDoc.Free;
  end;
end;

procedure TCustomECGrid.SaveColumnsToXML(AXMLDoc: TXMLDocument; AColumnsNode: TDOMNode; AXMLFlags: TXMLColFlags);
var aNode, aColumnNode: TDOMNode;
    i, aCount: Integer;
begin
  aColumnNode:=AColumnsNode.FirstChild;
  while assigned(aColumnNode) do  { delete old children }
    begin
      aNode:=aColumnNode;
      aColumnNode:=aColumnNode.NextSibling;
      aNode:=AColumnsNode.RemoveChild(aNode);
      aNode.Free;
    end;
  aCount:=Columns.Count;
  TDOMElement(AColumnsNode).SetAttribute(cCount, intToStr(aCount));
  for i:=0 to aCount-1 do  { create new children and fill them }
    begin
      aColumnNode:=AXMLDoc.CreateElement(cColumn+intToStr(i));
      AColumnsNode.AppendChild(aColumnNode);
      if excfOrder in AXMLFlags then
        TDOMElement(aColumnNode).SetAttribute(cOrder, intToStr(Columns[i].Order));
      if excfVisible in AXMLFlags then
        TDOMElement(aColumnNode).SetAttribute(cVisible, boolToStr(ecoVisible in Columns[i].Options, True));
      if excfWidth in AXMLFlags then
        TDOMElement(aColumnNode).SetAttribute(cWidth, intToStr(Columns[i].Width));
    end;
end;

procedure TCustomECGrid.SaveToCSVFile(AFileName: string; ADelimiter: Char = ',';
            AHeaders: Boolean = True; AVisibleColsOnly: Boolean = False);
var i, j, aFirstRow: Integer;
    aLine, aLines: TStringList;
begin
  if (RowCount>0) and (ColCount>0) then
    begin
      aLines:=TStringList.Create;
      aLine:=TStringList.Create;
      aLine.Delimiter:=ADelimiter;
      aLine.StrictDelimiter:=False;
      if AHeaders and (FixedRows>0)
        then aFirstRow:=0
        else aFirstRow:=FixedRows;
      try
        if AVisibleColsOnly then
          begin
            for i:=0 to high(VisiCols) do
              aLine.Add(Cells[VisiCols[i], aFirstRow]);
            aLines.Add(aLine.DelimitedText);
            for j:=aFirstRow+1 to RowCount-1 do
              begin
                for i:=0 to high(VisiCols) do
                  aLine[i]:=Cells[VisiCols[i], j];
                aLines.Add(aLine.DelimitedText);
              end;
          end else
          begin
            for i:=0 to Columns.Count-1 do
              aLine.Add(Cells[i, aFirstRow]);
            aLines.Add(aLine.DelimitedText);
            for j:=aFirstRow+1 to RowCount-1 do
              begin
                for i:=0 to Columns.Count-1 do
                  aLine[i]:=Cells[i, j];
                aLines.Add(aLine.DelimitedText);
              end;
          end;
        aLines.SaveToFile(AFileName);
      finally
        aLine.Free;
        aLines.Free;
      end;
    end;
end;

procedure TCustomECGrid.SelectCell(ACol, ARow: Integer; ASelectEditor: TSelectionMode; AForce: Boolean);
begin
  if (ACol<FixedCols) or (ARow<FixedRows) then exit;
  if MakeCellFullyVisible(ACol, ARow, AForce) then DoSelection(ACol, ARow, ASelectEditor, False);
end;

procedure TCustomECGrid.SetBorderStyle(NewStyle: TBorderStyle);
begin
  inherited SetBorderStyle(NewStyle);
  Flags:=Flags+[egfCalcBoundCols, egfCalcBoundRows];
  InvalidateNonUpdated;
end;

procedure TCustomECGrid.SetCursor(Value: TCursor);
begin
  inherited SetCursor(Value);
  if not (egfLockCursor in Flags) then DefCursor:=Value;
end;

procedure TCustomECGrid.SetFocus;
begin
  if EditorMode and Editor.CanFocus
    then Editor.SetFocus
    else inherited SetFocus;
end;

procedure TCustomECGrid.SetHint(const Value: TTranslateString);
begin
  inherited SetHint(Value);
  if not (egfLockHint in Flags) then DefHint:=Value;
end;

procedure TCustomECGrid.UpdateCell(ACol, ARow: Integer);
begin
  DrawDataCell(ACol, ARow, esmNative);
  InvalidateNonUpdated;
end;

procedure TCustomECGrid.UpdateColumn(ACol: Integer; AHeader: Boolean = False; AData: Boolean = True);
begin
  if AHeader then include(Columns[ACol].Flags, ecfRedrawHeader);
  if AData then include(Columns[ACol].Flags, ecfRedrawData);
  InvalidateNonUpdated;
end;

procedure TCustomECGrid.UpdateRequiredAreaHeight;
begin
  FRequiredArea.Y:=FixedRows*FixedRowHeight+(RowCount-FixedRows)*RowHeight;
  if (ClientAreaTop+ClientHeight)>FullAreaHeight then ClientAreaTop:=FRequiredArea.Y-ClientHeight;
  UpdateScrollInfoVert;
  exclude(Flags, egfUpdateRAHeight);
end;

procedure TCustomECGrid.UpdateRequiredAreaWidth;
var i, aWidth: Integer;
begin
  aWidth:=0;
  i:=high(VisiCols);
  if i>=0 then aWidth:=Columns[VisiCols[i]].Right;
  FRequiredArea.X:=aWidth;
  if (ClientAreaLeft+ClientWidth)>FullAreaWidth then ClientAreaLeft:=FRequiredArea.X-ClientWidth;
  UpdateScrollInfoHor;
  exclude(Flags, egfUpdateRAWidth);
end;

procedure TCustomECGrid.UpdateRow(ARow: Integer; AFixedCols: Boolean = False; AData: Boolean = True);
var i: Integer;
begin
  if AFixedCols then
    for i:=0 to FixedCols-1 do
      DrawFixedColumn(i, ARow, ARow);
  if AData then
    for i:=FirstVisiCell.X to LastVisiCell.X-1 do
      DrawDataCell(VisiCols[i], ARow, esmNative);
  InvalidateNonUpdated;
end;

procedure TCustomECGrid.UpdateRowCount;
begin
  Redraw([egfCalcBoundRows, egfRedrawFixedCols, egfRedrawData, egfUpdateRAHeight]);
end;

procedure TCustomECGrid.WMHScroll(var Msg: TWMScroll);
var aClientAreaLeft, aEdLeft, aSelectionPos: Integer;
    bEdVisible: Boolean;
begin
  include(Flags, egfCalcBoundCols);
  aSelectionPos:=-1;
  aClientAreaLeft:=-1;
  if egoScrollKeepVisible in Options then
    begin
      if IsColVisible(Col) then aSelectionPos:=Col-FirstVisiCell.X;
    end else
    if EditorMode then aClientAreaLeft:=ClientAreaLeft;
  inherited WMHScroll(Msg);
  if aSelectionPos>=0 then
    begin
      inc(aSelectionPos, FirstVisiCell.X);
      if EditorMode and (aSelectionPos=Col) then PrepareEditorDeltaRect(True, False);
      Col:=aSelectionPos;
    end else
    if aClientAreaLeft>=0 then
      begin
        dec(aClientAreaLeft, ClientAreaLeft);
        if aClientAreaLeft<>0 then
          begin
            aEdLeft:=Editor.Left;
            if not (egfRightToLeft in Flags) then
              begin
                inc(aEdLeft, aClientAreaLeft);
                Editor.Left:=aEdLeft;
                bEdVisible:= ((aEdLeft>=FixedColsWidth) and ((aEdLeft+Editor.Width)<=ClientWidth));
              end else
              begin
                dec(aEdLeft, aClientAreaLeft);
                Editor.Left:=aEdLeft;
                bEdVisible:= ((aEdLeft>=0) and ((aEdLeft+Editor.Width)<=(ClientWidth-FixedColsWidth)));
              end;
            Editor.Visible:=bEdVisible;  { False calls CMExit }
            if bEdVisible and Editor.CanFocus then Editor.SetFocus;
          end;
      end;
end;

procedure TCustomECGrid.WMSize(var Message: TLMSize);
begin
  inherited WMSize(Message);
  if PrevClientSize.cx<>ClientWidth then include(Flags, egfCalcBoundCols);
  if PrevClientSize.cy<>ClientHeight then include(Flags, egfCalcBoundRows);
  if [egfCalcBoundCols, egfCalcBoundRows]*Flags<>[] then
    begin
      Flags:=Flags+cRedrawGrid+[egfResizeBMPs];
      InvalidateNonUpdated;
      PrevClientSize:=Size(ClientWidth, ClientHeight);
    end;
end;

procedure TCustomECGrid.WMVScroll(var Msg: TWMScroll);
var aClientAreaTop, aEdTop, aSelectionPos: Integer;
    bEdVisible: Boolean;
begin
  include(Flags, egfCalcBoundRows);
  aSelectionPos:=-1;
  aClientAreaTop:=-1;
  if egoScrollKeepVisible in Options then
    begin
      if IsRowVisible(Row) then aSelectionPos:=Row-FirstVisiCell.Y;
    end else
    if EditorMode then aClientAreaTop:=ClientAreaTop;
  inherited WMVScroll(Msg);
  if aSelectionPos>=0 then
    begin
      inc(aSelectionPos, FirstVisiCell.Y);
      if EditorMode and (aSelectionPos=Row) then PrepareEditorDeltaRect(False, True);
      Row:=aSelectionPos;
    end else
    if aClientAreaTop>=0 then
      begin
        dec(aClientAreaTop, ClientAreaTop);
        if aClientAreaTop<>0 then
          begin
            aEdTop:=Editor.Top+aClientAreaTop;
            Editor.Top:=aEdTop;
            bEdVisible:= ((aEdTop>=(FixedRows*FixedRowHeight)) and ((aEdTop+Editor.Height)<=ClientHeight));
            Editor.Visible:=bEdVisible;  { False calls CMExit }
            if bEdVisible and Editor.CanFocus then Editor.SetFocus;
          end;
      end;
end;

{ TCustomECGrid.G/Setters }

function TCustomECGrid.GetCells(ACol, ARow: Integer): string;
begin
  if ARow>=FixedRows then
    begin
      if assigned(Columns[ACol].OnGetDataCellText)
        then Columns[ACol].OnGetDataCellText(Columns[ACol], ARow-FixedRows, Result)
        else Result:='';
    end else
    if ARow=0
        then Result:=Columns[ACol].Title.Text
        else if assigned(OnGetHeaderText)
               then OnGetHeaderText(self, ACol, ARow, Result)
               else Result:='';
end;

function TCustomECGrid.GetCellsOrd(AColOrder, ARow: Integer): string;
begin
  Result:=Cells[OrderedCols[AColOrder], ARow];
end;

function TCustomECGrid.GetColCount: Integer;
begin
  Result:=Columns.Count;
end;

function TCustomECGrid.GetColOrd(ACol: Integer): Integer;
begin
  Result:=OrderedCols[ACol];
end;

function TCustomECGrid.GetRowCount: Integer;
begin
  Result:=FRowCountHelp;
  if assigned(OnGetDataRowCount) then OnGetDataRowCount(self, Result);
  inc(Result, FixedRows);
end;

procedure TCustomECGrid.SetAlternateColor(AValue: TColor);
begin
  if FAlternateColor=AValue then exit;
  FAlternateColor:=AValue;
  Redraw([egfCalcColors, egfRedrawData]);
end;

procedure TCustomECGrid.SetAlternateTint(AValue: SmallInt);
begin
  if AValue<0
    then AValue:=0
    else if AValue>100 then AValue:=100;
  if FAlternateTint=AValue then exit;
  FAlternateTint:=AValue;
  Redraw([egfCalcColors, egfRedrawData]);
end;

procedure TCustomECGrid.SetCol(AValue: Integer);
begin
  if FCol=AValue then exit;
  if AValue>=FixedCols then
    if MakeCellFullyVisible(AValue, Row, False) then DoSelection(AValue, Row, esmNative, False);
end;

procedure TCustomECGrid.SetEditorMode(AValue: Boolean);
var bFocused: Boolean;
begin
  if FEditorMode=AValue then exit;
  if not AValue then
    begin
      FEditorMode:=AValue;
      if assigned(Editor) then
        begin
          if Editor.PopupMenu=PopupMenu then Editor.PopupMenu:=nil;
          bFocused:=Editor.Focused;
          Editor.Visible:=False;
          FEditor:=nil;
          if bFocused and CanFocus then SetFocus;
          if not (egoEnlargeAlways in Options) then DoEnlargement(-1, Col);
          if not (egfDoingSelection in  Flags) then DrawDataCell(Col, Row);
        end;
    end else
    if MakeCellFullyVisible(Col, Row, False) then DoSelection(Col, Row, esmSelect, False);
end;

procedure TCustomECGrid.SetFixedCols(AValue: SmallInt);
begin
  if AValue<0 then AValue:=0;
  if FFixedCols=AValue then exit;
  FFixedCols:=AValue;
  Redraw([egfCalcBoundCols, egfCalcColsLeft, egfResizeBMPs, egfUpdateRAWidth]+cRedrawGrid);
end;

procedure TCustomECGrid.SetFixedRowHeight(AValue: SmallInt);
begin
  if FFixedRowHeight=AValue then exit;
  if EditorMode then PrepareEditorDeltaRect(False, True);
  FFixedRowHeight:=AValue;
  Redraw([egfCalcBoundRows, egfResizeBMPs, egfUpdateRAHeight]+cRedrawGrid);
end;

procedure TCustomECGrid.SetFixedRows(AValue: SmallInt);
begin
  if FFixedRows=AValue then exit;
  if Row>=FFixedRows then
    begin
      FRow:=FRow+AValue-FFixedRows;
      if EditorMode then
        begin
          PrepareEditorDeltaRect(False, True);
          inc(EditorDeltaRect.Top, (AValue-FFixedRows)*RowHeight);
        end;
    end;
  FFixedRows:=AValue;
  Redraw([egfCalcBoundRows, egfResizeBMPs, egfUpdateRAHeight]+cRedrawGrid);
end;

procedure TCustomECGrid.SetGridLineColor(AValue: TColor);
begin
  if FGridLineColor=AValue then exit;
  FGridLineColor:=AValue;
  Redraw([egfRedrawData]);
end;

procedure TCustomECGrid.SetGridLineWidth(AValue: SmallInt);
begin
  if FGridLineWidth=AValue then exit;
  FGridLineWidth:=AValue;
  if Style<>egsFlat
    then Redraw([egfRedrawData])
    else Redraw([egfCalcFixedCellIndent]+cRedrawGrid);
end;

procedure TCustomECGrid.SetImages(AValue: TCustomImageList);
begin
  if FImages=AValue then exit;
  FImages:=AValue;
  Redraw([egfRedrawFixedHeader, egfRedrawFixedRows]);
end;

procedure TCustomECGrid.SetOptions(AValue: TGOptions);
const cInvOpts = [egoDottedGrid, egoHilightCol, egoHilightRow, egoHorizontalLines,
                  egoSortArrow, egoVerticalLines];
var aChangedOpts: TGOptions;
    aCol: Integer;
begin
  if FOptions=AValue then exit;
  aChangedOpts:=FOptions><AValue;
  aCol:=Col;
  FOptions:=AValue;
  if egoReadOnly in aChangedOpts then EditorMode:=False;
  if (egoEnlargeAlways in aChangedOpts) and (aCol>=0) then
    if egoEnlargeAlways in AValue
      then DoEnlargement(aCol, -1)
      else if not EditorMode then DoEnlargement(-1, aCol);
  if egoAlwaysShowEditor in aChangedOpts then
    if egoAlwaysShowEditor in AValue then
      begin
        if IsCellFullyVisible(aCol, Row) and not (egoReadOnly in AValue) and
          not (ecoReadOnly in Columns[aCol].Options) then
            begin
              DoSelectEditor(aCol, False);
            end;
      end else
      if EditorMode and not Editor.Focused then EditorMode:=False;
  if aChangedOpts*cInvOpts<>[] then Redraw(cRedrawGrid);
end;

procedure TCustomECGrid.SetRow(AValue: Integer);
begin
  if FRow=AValue then exit;
  if (AValue>=FixedRows) and (AValue<RowCount) then
    if MakeCellFullyVisible(Col, AValue, False) then DoSelection(Col, AValue, esmNative, False);
end;

procedure TCustomECGrid.SetRowHeight(AValue: SmallInt);
begin
  if AValue<1 then AValue:=1;
  if FRowHeight=AValue then exit;
  if EditorMode then PrepareEditorDeltaRect(False, True);
  FRowHeight:=AValue;
  Redraw([egfCalcBoundRows, egfUpdateRAHeight, egfRedrawFixedCols, egfRedrawData, egfResizeBMPs]);
end;

procedure TCustomECGrid.SetSizableCol(AValue: Integer);
begin
  if FSizableCol=AValue then exit;
  FSizableCol:=AValue;
  ChangeCursor(AValue>=0)
end;

procedure TCustomECGrid.SetSortAscendent(AValue: Boolean);
begin
  if FSortAscendent=AValue then exit;
  FSortAscendent:=AValue;
  if (egoSortArrow in Options) and (Flags*[egfRedrawFixedHeader, egfRedrawFixedRows]=[]) then
    begin
      DrawHeaderCell(SortIndex, 0);
      InvalidateNonUpdated;
    end;
end;

procedure TCustomECGrid.SetSortIndex(AValue: Integer);
var aOldSortIndex: Integer;
begin
  aOldSortIndex:=FSortIndex;
  if aOldSortIndex=AValue then exit;
  FSortIndex:=AValue;
  FSortAscendent:=True;
  if (egoSortArrow in Options) and (Flags*[egfRedrawFixedHeader, egfRedrawFixedRows]=[]) then
    begin
      if InRangeCO(aOldSortIndex, 0, Columns.Count) then DrawHeaderCell(aOldSortIndex, 0);
      DrawHeaderCell(AValue, 0);
      InvalidateNonUpdated;
    end;
end;

procedure TCustomECGrid.SetStyle(AValue: TGridStyle);
begin
  if FStyle=AValue then exit;
  FStyle:=AValue;
  Redraw([egfCalcFixedCellIndent, egfRedrawFixedCols, egfRedrawFixedHeader, egfRedrawFixedRows]);
end;

end.


