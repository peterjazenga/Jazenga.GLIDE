
{**********************************************************************
 Package pl_ExControls.pkg
 From PilotLogic Software House(http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit TplPageControlUnit;

{$MODE Delphi}

{$T-}

interface

uses
  Messages, LCLIntf, LCLType, LMessages,
  SysUtils, Classes, Controls, Forms,
  Graphics, ImgList, ComCtrls, Math, Dialogs ,
  ExtCtrls ,types ,plUtils;

const
  CloseButtonWidth = 14;
  CloseButtonHeight = 13;

  // 1.5.0.0 : Added TabStyle tsDelphi to simulate Delphi 2005 tabs

type
  
  TTabStyle = (tsClassic, tsDotNet, tsDelphi);

  TMarginSize = -MaxInt..MaxInt;

  TMarginChange = procedure(NewValue: TMarginSize; OldValue: TMarginSize; Index: integer) of object;

  TTabMargin = class(TPersistent)
  private
    FLeftMargin: TMarginSize;
    FTopMargin: TMarginSize;
    FRightMargin: TMarginSize;
    FOnMarginChange: TMarginChange;
    procedure SetMargin(Index: integer; Value: TMarginSize);
  protected
    property OnMarginChange: TMarginChange read FOnMarginChange write FOnMarginChange;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property LeftMargin: TMarginSize index 0 read FLeftMargin write SetMargin default 0;
    property TopMargin: TMarginSize index 1 read FTopMargin write SetMargin default 0;
    property RightMargin: TMarginSize index 2 read FRightMargin write SetMargin;
  end;

  TplPageControl = class;

  TImageIndex = integer;


  TCanCloseEvent = procedure(Sender: TObject; var CanClose: Boolean) of object;
  TCanChangeEvent = procedure(Sender: TObject; FromPage, ToPage: Integer; var AllowChange: Boolean) of object;


TplTabSheet = class(TCustomControl)
  private
    FTabCaption: string;
    FTextColor: TColor;
    FShowClose: Boolean;
    FImageIndex: TImageIndex;
    FImageIndexDummy: TImageIndex;
    FAdvPageControl: TplPageControl;
    FTabVisible: Boolean;
    FTabShowing: Boolean;
    FHighlighted: Boolean;           
    FOnHide: TNotifyEvent;
    FOnShow: TNotifyEvent;
    FOnClose: TNotifyEvent;
    FOnCanClose: TCanCloseEvent;
    FTabEnable: boolean;
    function  GetCaption: TCaption;
    procedure SetCaption(Value: TCaption);
    function  GetPageIndex: Integer;
    function  GetTabIndex: Integer;
    procedure SetHighlighted(Value: Boolean);
    function  GetImageIndex: TImageIndex;
    procedure SetImageIndex(Value: TImageIndex);
    procedure SeTplPageControl(AAdvPageControl: TplPageControl);
    procedure SetPageIndex(Value: Integer);
    procedure SetTabShowing(Value: Boolean);
    procedure SetTabVisible(Value: Boolean);
    procedure UpdateTabShowing;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CloseButtonClick(Sender: TObject);
    function  CanCloseClick(Sender: TObject): Boolean;
    procedure SetShowClose(value: Boolean);
    procedure SetTextColor(const Value: TColor);
    procedure SetTabEnable(const Value: boolean);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DoHide; dynamic;
    procedure DoShow; dynamic;
    procedure ReadState(Reader: TReader); override;
    procedure Paint; override;
    property  TabCaption: string read FTabCaption;
    property  ImageIndexDummy: TImageIndex read FImageIndexDummy;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property  AdvPageControl: TplPageControl read FAdvPageControl write SeTplPageControl;
    property  TabIndex: Integer read GetTabIndex;
    procedure SelectFirstControl;
  published
    property BorderWidth;
    property Caption: TCaption read GetCaption write SetCaption;

    property Highlighted: Boolean read FHighlighted write SetHighlighted default False;
    property ImageIndex: TImageIndex read GetImageIndex write SetImageIndex default 0;
    property TextColor: TColor read FTextColor write SetTextColor default clBlack;
    property ShowClose: Boolean read FShowClose write SetShowClose default False;
    property TabEnable: boolean read FTabEnable write SetTabEnable default true;
    property TabVisible: Boolean read FTabVisible write SetTabVisible default True;
    property DragMode;
    property Enabled;
    property Font;
    property Height stored False;
    property Left stored False;
    property Constraints;
    property PageIndex: Integer read GetPageIndex write SetPageIndex stored False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;        
    property Top stored False;
    property Visible stored False;
    property Width stored False;
    property OnCanClose: TCanCloseEvent read FOnCanClose write FOnCanClose;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnHide: TNotifyEvent read FOnHide write FOnHide;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property OnStartDrag;

  end;

 TOnDelOrCloseTabEvent = procedure(Sender: TObject;Page: TplTabSheet; const Index: Integer;var Allow:boolean) of object;
 TTabOverlapSize = 0..15;

TplPageControl = class(TCustomTabControl)
  private
    FCanvas: TCanvas;
    FPages: TList;
    FActivePage: TplTabSheet;
    FNewDockSheet: TplTabSheet;
    FUndockingPage: TplTabSheet;
    FTabMargin: TTabMargin;
    FClosedTabList: TStringList;
    FOnChange: TNotifyEvent;
    FImages: TCustomImageList;
    FDummyImages: TCustomImageList;
    FDefaultTextColor: TColor;
    FTabBorder3D: Boolean;
    FTabBorderColor: TColor;
    FTabHoverColor: TColor;
    FTabHoverColorTo: TColor;
    FTabHoverBorder: TColor;
    FShowFocus: Boolean;
    FHoverClosedButton: Boolean;
    FTabOverlap: TTabOverlapSize;
    FTabSheet3D: Boolean;
    FTabSheetBorderColor: TColor;
    FTabBackGroundColor: TColor;
    FTabSplitLine: Boolean;
    FRoundEdges: Boolean;
    FHoverTab: Integer;
    FTabBackGround: TBitmap;
    FTabBackGroundActive: TBitmap;
    FTabStyle: TTabStyle;
    FLowerActive: integer;
    FPropertiesLoaded: Boolean;
    FFreeOnClose: Boolean;
    FActiveFont: TFont;
    FOnCanChange: TCanChangeEvent;
    FCloseGlyph: TBitmap;
    FFullRefresh: Boolean;

    //......................
    FAutoFocus: Boolean;
    FNextDialogOnEnter: Boolean;
    FDialogOnCursorKeys: Boolean;
    FNextPriorStep: Integer;
    FFocusWidthInc: Integer;
    FFocusColor: TColor;
    FNoFocusColor: TColor;
    IsLoaded: Boolean;
    FTabDrawOutLine:boolean;
    fOnDelOrCloseTabEvent:TOnDelOrCloseTabEvent;

    procedure ActiveFontChangeEvent(Sender: TObject);
    procedure UpdateTabForActiveFont(Page: TplTabSheet);
    procedure ChangeActivePage(Page: TplTabSheet);
    procedure DeleteTab(Page: TplTabSheet; Index: Integer);
    function  GetActivePageIndex: Integer;
    //function  GetDockClientFromMousePos(MousePos: TPoint): TControl;
    function  GetPage(Index: Integer): TplTabSheet;
    function  GetPageCount: Integer;
    procedure InsertPage(Page: TplTabSheet);
    procedure InsertTab(Page: TplTabSheet);
    procedure MoveTab(CurIndex, NewIndex: Integer);
    procedure RemovePage(Page: TplTabSheet);
    procedure SetActivePage(Page: TplTabSheet);
    procedure SetActivePageIndex(const Value: Integer); 
    procedure SetDefaultTextColor(const Value: TColor);

    procedure SetTabBorder3D(Value: Boolean);
    procedure SetTabBorderColor(const Value: TColor);
    procedure SetTabBackGround(const Value: TBitmap);
    procedure SetTabBackGroundActive(const Value: TBitmap);
    procedure SetImages(value: TCustomImageList);
    procedure SetTabMargin(Value: TTabMargin);
    procedure SetTabOverlap(Value: TTabOverlapSize);

    procedure SetTabSheet3D(Value: Boolean);
    procedure SetTabSheetBorderColor(Value: TColor);
    procedure SetTabBackGroundColor(Value: TColor);

    procedure SetTabSplitLine(Value: Boolean);
    procedure SetRoundEdges(Value: Boolean);

    procedure SetTabStyle(Value: TTabStyle);
    procedure SetLowerActive(Value: integer);

    function  GetTabPosition: TTabPosition;
    procedure SetTabPosition(Value: TTabPosition);

    procedure SetTabDrawOutLine(Const Value:boolean);

    procedure SetTabMargins;

    procedure DrawCloseGlyph(P: TPoint);
    procedure DrawCloseButton(Rect: TRect; Active: Boolean);
    procedure DrawHoverCloseButton(Rect: TRect);
    procedure DrawDownCloseButton(Rect: TRect);
    function  IsOnButton(TabIndex, X, Y: integer): Boolean;
    procedure TabMarginChange(NewValue, OldValue: TMarginSize; Index: integer);

    procedure UpdateTab(Page: TplTabSheet);
    procedure UpdateTabHighlights;

    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
    procedure WMPaint(var Msg: TLMPaint); message LM_PAINT;
    procedure CMDesignHitTest(var Message: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;
    procedure WMLButtonDown(var Message: TLMLButtonDown); message LM_LBUTTONDOWN;
    procedure WMLButtonDBLCLK(var Message: TLMLButtonDblClk); message LM_LBUTTONDBLCLK;
 //   procedure CMDockClient(var Message: TCMDockClient); message CM_DOCKCLIENT;
 //   procedure CMDockNotification(var Message: TCMDockNotification); message CM_DOCKNOTIFICATION;
 //   procedure CMUnDockClient(var Message: TCMUnDockClient); message CM_UNDOCKCLIENT;

    procedure SetActiveFont(const Value: TFont);
    procedure SetCloseGlyph(const Value: TBitmap);
  protected
    procedure KeyPress(var Key: Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    function  CanShowTab(TabIndex: Integer): Boolean; //override;
    procedure Change; override;
    procedure DoAddDockClient(Client: TControl; const ARect: TRect); override;
    procedure DockOver(Source: TDragDockObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;
    procedure DoRemoveDockClient(Client: TControl); override;
    function  GetImageIndex(TabIndex: Integer): Integer; override;
    function  GetPageFromDockClient(Client: TControl): TplTabSheet;
    procedure GetSiteInfo(Client: TControl; var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean); override;
    procedure Loaded; override;
    procedure SetChildOrder(Child: TComponent; Order: Integer); override;
    procedure ShowControl(AControl: TControl); override;
    procedure UpdateActivePage; virtual;

    procedure WndProc(var Message: TMessage); override;
    procedure TabChange(Sender: TObject);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure DrawAllTabs(Canvas: TCanvas);
    function  TabRectEx(i: Integer): TRect;
    function  IndexOfTabAtEx(X, Y: Integer): Integer;
    function  CanChange: Boolean; override;
  public

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function  FindNextPage(CurPage: TplTabSheet; GoForward, CheckTabVisible: Boolean): TplTabSheet;
    procedure SelectNextPage(GoForward: Boolean);
    procedure OpenAllClosedTabs;
    function  OpenClosedTab(TabName: string): Boolean;
    property  ActivePageIndex: Integer read GetActivePageIndex write SetActivePageIndex;
    property  PageCount: Integer read GetPageCount;
    property  Pages[Index: Integer]: TplTabSheet read GetPage;
    property  FullRefresh: Boolean read FFullRefresh write FFullRefresh;
    property  Canvas: TCanvas read FCanvas;
  published
    property ActivePage: TplTabSheet read FActivePage write SetActivePage;
    property ActiveFont: TFont read FActiveFont write SetActiveFont;
    property Align;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property CloseGlyph: TBitmap read FCloseGlyph write SetCloseGlyph;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HotTrack;
    property Images: TCustomImageList read FImages write SetImages;
    property MultiLine;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RaggedRight;
    property ScrollOpposite;
    property ShowHint;
    property FreeOnClose: Boolean read FFreeOnClose write FFreeOnClose default false;
    property ShowFocus: Boolean read FShowFocus write FShowFocus default false;   
    property DefaultTextColor: TColor read FDefaultTextColor write SetDefaultTextColor default clBlack;
    property TabBorder3D: Boolean read FTabBorder3D write SetTabBorder3D default false;
    property TabBorderColor: TColor read FTabBorderColor write SetTabBorderColor default clGray;
    property TabSheet3D: Boolean read FTabSheet3D write SetTabSheet3D default false;
    property TabSheetBorderColor: TColor read FTabSheetBorderColor write SetTabSheetBorderColor default clGray; 
    property TabBackGroundColor: TColor read FTabBackGroundColor write SetTabBackGroundColor;
    property TabBackGround: TBitmap read FTabBackGround write SetTabBackGround;
    property TabBackGroundActive: TBitmap read FTabBackGroundActive write SetTabBackGroundActive;
    property TabMargin: TTabMargin read FTabMargin write SetTabMargin;
    property TabOverlap: TTabOverlapSize read FTabOverlap write SetTabOverlap;
    property TabSplitLine: Boolean read FTabSplitLine write SetTabSplitLine default false;
    property RoundEdges: Boolean read FRoundEdges write SetRoundEdges default false;
    property TabStyle: TTabStyle read FTabStyle write SetTabStyle default tsClassic;
    property LowerActive: integer read FLowerActive write SetLowerActive default 2;
    property AutoFocus: Boolean read FAutoFocus write FAutoFocus;
    property FocusWidthInc: Integer read FFocusWidthInc write FFocusWidthInc;
    property NextDialogOnEnter: Boolean read FNextDialogOnEnter write FNextDialogOnEnter;
    property DialogOnCursorKeys: Boolean read FDialogOnCursorKeys write FDialogOnCursorKeys;
    property NextPriorStep: Integer read FNextPriorStep write FNextPriorStep;
    property TabDrawOutLine: Boolean read FTabDrawOutLine write SetTabDrawOutLine;
    
    property TabHeight;
    property TabOrder;
    property TabPosition read GetTabPosition write SetTabPosition;
    property TabStop;
    property TabWidth;
    property Visible;
    property OnDelOrCloseTabEvent:TOnDelOrCloseTabEvent read FOnDelOrCloseTabEvent write FOnDelOrCloseTabEvent;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging;
    property OnCanChange: TCanChangeEvent read FOnCanChange write FOnCanChange;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetImageIndex;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

implementation

uses
  Printers, ActnList, StdActns;

//=============================================================

procedure DrawGradient(Canvas: TCanvas; FromColor, ToColor: TColor; Steps: Integer; R: TRect; Direction: Boolean);
 begin
  if Direction then
   GradientFillRect(Canvas,R,FromColor,ToColor,fdLeftToRight,32) else
   GradientFillRect(Canvas,R,FromColor,ToColor,fdTopToBottom,32);
 end;

  {
procedure DrawGradient(Canvas: TCanvas; FromColor, ToColor: TColor; Steps: Integer; R: TRect; Direction: Boolean);
var
  diffr, startr, endr: Integer;
  diffg, startg, endg: Integer;
  diffb, startb, endb: Integer;
  rstepr, rstepg, rstepb, rstepw: Real;
  i, stepw: Word;
begin

  if (FromColor=ToColor) or (ToColor=clNone) then
   begin
    Canvas.Brush.Color := FromColor;
    Canvas.FillRect(r);
    Exit;
   end;

 //......................................
  if Steps = 0 then Steps := 1;        

  FromColor := ColorToRGB(FromColor);
  ToColor := ColorToRGB(ToColor);

  startr := (FromColor and $0000FF);
  startg := (FromColor and $00FF00) shr 8;
  startb := (FromColor and $FF0000) shr 16;
  endr := (ToColor and $0000FF);
  endg := (ToColor and $00FF00) shr 8;
  endb := (ToColor and $FF0000) shr 16;

  diffr := endr - startr;
  diffg := endg - startg;
  diffb := endb - startb;

  rstepr := diffr / steps;
  rstepg := diffg / steps;
  rstepb := diffb / steps;

  if Direction then
    rstepw := (R.Right - R.Left) / Steps
  else
    rstepw := (R.Bottom - R.Top) / Steps;

  with Canvas do
  begin
    for i := 0 to steps - 1 do
    begin
      endr := startr + Round(rstepr * i);
      endg := startg + Round(rstepg * i);
      endb := startb + Round(rstepb * i);
      stepw := Round(i * rstepw);
      Pen.Color := endr + (endg shl 8) + (endb shl 16);
      Brush.Color := Pen.Color;
      if Direction then
        Rectangle(R.Left + stepw, R.Top, R.Left + stepw + Round(rstepw) + 1, R.Bottom)
      else
        Rectangle(R.Left, R.Top + stepw, R.Right, R.Top + stepw + Round(rstepw) + 1);
    end;
  end;
end;
     }
procedure DrawTransparentBitmap(hdc: THandle; hBitmap: THandle; xStart, yStart: Integer;
  width, height, offsx, offsy: Integer; cTransparentColor: TColor);
// The function draws a bitmap with a transparent background.
var
  cColor: TColor;
  bmAndBack, bmAndObject, bmAndMem, bmSave: THandle;
  bmBackOld, bmObjectOld, bmMemOld, bmSaveOld: THandle;
  hdcMem, hdcBack, hdcObject, hdcTemp, hdcSave: THandle;
  ptSize: TPoint;
  {.$IFDEF TMSDOTNET}
  ap: array of TPoint;  
   
begin
  hdcTemp := CreateCompatibleDC(hdc);
  SelectObject(hdcTemp, hBitmap);

  ptSize.x := width;
  ptSize.y := height;

  {.$IFDEF TMSDOTNET}
  SetLength(ap,1);
  ap[0] := ptSize;
  dptolp(hdcTemp,ap,1);
  ptSize := ap[0];
   

  {.$IFNDEF TMSDOTNET}
  DPtoLP(hdcTemp, ptSize, 1);
   

  hdcBack := CreateCompatibleDC(hdc);
  hdcObject := CreateCompatibleDC(hdc);
  hdcMem := CreateCompatibleDC(hdc);
  hdcSave := CreateCompatibleDC(hdc);

  bmAndBack := CreateBitmap(ptSize.x, ptSize.y, 1, 1, nil);
  bmAndObject := CreateBitmap(ptSize.x, ptSize.y, 1, 1, nil);

  bmAndMem := CreateCompatibleBitmap(hdc, ptSize.x, ptSize.y);
  bmSave := CreateCompatibleBitmap(hdc, ptSize.x, ptSize.y);

  bmBackOld := SelectObject(hdcBack, bmAndBack);
  bmObjectOld := SelectObject(hdcObject, bmAndObject);
  bmMemOld := SelectObject(hdcMem, bmAndMem);
  bmSaveOld := SelectObject(hdcSave, bmSave);

  SetMapMode(hdcTemp, GetMapMode(hdc));

  BitBlt(hdcSave, 0, 0, ptSize.x, ptSize.y, hdcTemp, offsx, offsy, SRCCOPY);

  cColor := SetBkColor(hdcTemp, cTransparentColor);

  BitBlt(hdcObject, 0, 0, ptSize.x, ptSize.y, hdcTemp, offsx, offsy, SRCCOPY);

  SetBkColor(hdcTemp, cColor);

  BitBlt(hdcBack, 0, 0, ptSize.x, ptSize.y, hdcObject, 0, 0, NOTSRCCOPY);

  // take copy of existing canvas
  BitBlt(hdcMem, 0, 0, ptSize.x, ptSize.y, hdc, xStart, yStart, SRCCOPY);
  // and existing canvas with copy
  BitBlt(hdcMem, 0, 0, ptSize.x, ptSize.y, hdcObject, 0, 0, SRCAND);

  BitBlt(hdcTemp, offsx, offsy, ptSize.x, ptSize.y, hdcBack, 0, 0, SRCAND);
  BitBlt(hdcMem, 0, 0, ptSize.x, ptSize.y, hdcTemp, offsx, offsy, SRCPAINT);
  BitBlt(hdc, xStart, yStart, ptSize.x, ptSize.y, hdcMem, 0, 0, SRCCOPY);
  BitBlt(hdcTemp, 0, 0, ptSize.x, ptSize.y, hdcSave, 0, 0, SRCCOPY);


  DeleteObject(SelectObject(hdcBack, bmBackOld));
  DeleteObject(SelectObject(hdcObject, bmObjectOld));
  DeleteObject(SelectObject(hdcMem, bmMemOld));
  DeleteObject(SelectObject(hdcSave, bmSaveOld));

  DeleteDC(hdcMem);
  DeleteDC(hdcBack);

  DeleteDC(hdcObject);
  DeleteDC(hdcSave);
  DeleteDC(hdcTemp);
end;

procedure StretchTransparentBitmap(hdc: THandle; hBitmap: THandle; xStart, yStart: Integer;
  width, height, offsx, offsy, bmpw, bmph: Integer; cTransparentColor: TColor);
// The function draws a bitmap with a transparent background.
var
  cColor: TColor;
  bmAndBack, bmAndObject, bmAndMem, bmSave: THandle;
  bmBackOld, bmObjectOld, bmMemOld, bmSaveOld: THandle;
  hdcMem, hdcBack, hdcObject, hdcTemp, hdcSave: THandle;
  ptSize: TPoint;
  {.$IFDEF TMSDOTNET}
  ap: array of TPoint;
   
begin
  hdcTemp := CreateCompatibleDC(hdc);
  SelectObject(hdcTemp, hBitmap);

  ptSize.x := width;
  ptSize.y := height;

  {.$IFDEF TMSDOTNET}
  SetLength(ap,1);
  ap[0] := ptSize;
  dptolp(hdcTemp,ap,1);
  ptSize := ap[0];
   

  {.$IFNDEF TMSDOTNET}
  DPtoLP(hdcTemp, ptSize, 1);
   

  hdcBack := CreateCompatibleDC(hdc);
  hdcObject := CreateCompatibleDC(hdc);
  hdcMem := CreateCompatibleDC(hdc);
  hdcSave := CreateCompatibleDC(hdc);

  bmAndBack := CreateBitmap(ptSize.x, ptSize.y, 1, 1, nil);
  bmAndObject := CreateBitmap(ptSize.x, ptSize.y, 1, 1, nil);

  bmAndMem := CreateCompatibleBitmap(hdc, ptSize.x, ptSize.y);
  bmSave := CreateCompatibleBitmap(hdc, ptSize.x, ptSize.y);

  bmBackOld := SelectObject(hdcBack, bmAndBack);
  bmObjectOld := SelectObject(hdcObject, bmAndObject);
  bmMemOld := SelectObject(hdcMem, bmAndMem);
  bmSaveOld := SelectObject(hdcSave, bmSave);

  SetMapMode(hdcTemp, GetMapMode(hdc));

  StretchBlt(hdcSave, 0, 0, ptSize.x, ptSize.y, hdcTemp, offsx, offsy, bmpw, bmph, SRCCOPY);

  cColor := SetBkColor(hdcTemp, cTransparentColor);

  StretchBlt(hdcObject, 0, 0, ptSize.x, ptSize.y, hdcTemp, offsx, offsy, bmpw, bmph, SRCCOPY);

  SetBkColor(hdcTemp, cColor);

  BitBlt(hdcBack, 0, 0, ptSize.x, ptSize.y, hdcObject, 0, 0, NOTSRCCOPY);

  // take copy of existing canvas
  BitBlt(hdcMem, 0, 0, ptSize.x, ptSize.y, hdc, xStart, yStart, SRCCOPY);
  // and existing canvas with copy
  BitBlt(hdcMem, 0, 0, ptSize.x, ptSize.y, hdcObject, 0, 0, SRCAND);
  StretchBlt(hdcTemp, offsx, offsy, bmpw, bmph, hdcback, 0, 0, ptsize.x, ptsize.y, SRCAND);
  StretchBlt(hdcMem, 0, 0, ptSize.X, ptSize.Y, hdctemp, offsx, offsy, bmpw, bmph, SRCPAINT);
  BitBlt(hdc, xStart, yStart, ptSize.x, ptSize.y, hdcMem, 0, 0, SRCCOPY);
  StretchBlt(hdcTemp, offsx, offsy, bmpw, bmph, hdcSave, 0, 0, ptsize.x, ptsize.y, SRCCOPY);

  DeleteObject(SelectObject(hdcBack, bmBackOld));
  DeleteObject(SelectObject(hdcObject, bmObjectOld));
  DeleteObject(SelectObject(hdcMem, bmMemOld));
  DeleteObject(SelectObject(hdcSave, bmSaveOld));

  DeleteDC(hdcMem);
  DeleteDC(hdcBack);

  DeleteDC(hdcObject);
  DeleteDC(hdcSave);
  DeleteDC(hdcTemp);
end;

procedure BitmapStretch(bmp: tbitmap; canvas: tcanvas; x, y, height: integer);
var
  mid: integer;
  fillh: integer;
  c: TColor;
begin
  mid := bmp.height div 2;
  fillh := height - bmp.height;
  c := bmp.Canvas.Pixels[0, bmp.Height - 1];
  DrawTransparentBitmap(canvas.handle, bmp.handle, x, y, bmp.Width, bmp.Height div 2, 0, 0, c);
  StretchTransparentBitmap(canvas.handle, bmp.Handle, x, y + mid, bmp.width, fillh, 0, mid - 1, bmp.Width, 2, c);
  DrawTransparentBitmap(canvas.handle, bmp.handle, x, y + mid + fillh, bmp.width, bmp.Height div 2, 0, mid, c);
end;

procedure BitmapStretchInWidth(bmp: tbitmap; canvas: tcanvas; x, y, width: integer);
var
  mid: integer;
  fillw: integer;
  c: TColor;
  ofs: Integer;
begin
  mid := bmp.Width div 2;
  fillw := width - bmp.Width;
  ofs := 0;
//  if odd(Width) then inc(ofs);

//  if P = 'T' then
//    c := bmp.Canvas.Pixels[bmp.Width - 1, 0]
//  else
  c := bmp.Canvas.Pixels[0, bmp.Height - 1];

  DrawTransparentBitmap(canvas.handle, bmp.handle, x, y, bmp.Width div 2, bmp.Height, 0, 0, c);
  StretchTransparentBitmap(canvas.handle, bmp.Handle, x + mid, y, fillw, bmp.height, mid - 1, 0, 2, bmp.height, c);
  DrawTransparentBitmap(canvas.handle, bmp.handle, x + mid + fillw, y, bmp.width div 2, bmp.Height, mid + ofs, 0, c);
end;

//================= TTabMargin ==============================

procedure TTabMargin.Assign(Source: TPersistent);
begin
  if Source is TTabMargin then
  begin
    FLeftMargin := TTabMargin(Source).LeftMargin;
    FTopMargin := TTabMargin(Source).TopMargin;
    FRightMargin := TTabMargin(Source).RightMargin;
    inherited Assign(Source);
  end;
end;

procedure TTabMargin.SetMargin(Index: integer; Value: TMarginSize);
begin
  case Index of
    0:
      if Value <> FLeftMargin then
      begin
        if Assigned(FOnMarginChange) then FOnMarginChange(Value, FLeftMargin, 0);
        FLeftMargin := Value;
      end;
    1:
      if Value <> FTopMargin then
      begin
        if Assigned(FOnMarginChange) then FOnMarginChange(Value, FTopMargin, 1);
        FTopMargin := Value;
      end;
    2:
      if Value <> FRightMargin then
      begin
        if Assigned(FOnMarginChange) then FOnMarginChange(Value, FRightMargin, 2);
        FRightMargin := Value;
      end;
  end;
end;

//====================== TplTabSheet =======================================

constructor TplTabSheet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Color:=DefiTabSheetBkColor;
  Align := alClient;
  ControlStyle := ControlStyle + [csAcceptsControls, csNoDesignVisible];
  Visible := False;
  FTabVisible := True;
  FHighlighted := False;
  FTabEnable := true;

  FShowClose := False;
  FTextColor := clBlack;
  FImageIndexDummy := 0;
end;

destructor TplTabSheet.Destroy;
begin
  if FAdvPageControl <> nil then
  begin
    if FAdvPageControl.FUndockingPage = Self then FAdvPageControl.FUndockingPage := nil;
    FAdvPageControl.RemovePage(Self);
  end;
  inherited Destroy;
end;

procedure TplTabSheet.SelectFirstControl;
begin
  SelectFirst;
end;

function TplTabSheet.GetCaption: TCaption;
begin
  Result := inherited Caption;
end;

procedure TplTabSheet.SetCaption(Value: TCaption);
begin
  if not FShowClose then
    FTabCaption := Copy(Value, 1, length(Value) - 2)
  else
    FTabCaption := Value + ' ';

  inherited Caption := Value;
  
  if AdvPageControl <> nil then
    AdvPageControl.UpdateTab(self);
end;

procedure TplTabSheet.SetShowClose(value: Boolean);
begin
  FShowClose := value;
  SetCaption(Caption);
  if AdvPageControl <> nil then
  begin
    AdvPageControl.Invalidate;
  end;
end;

procedure TplTabSheet.SetTextColor(const Value: TColor);
begin
  FTextColor := Value;
  if AdvPageControl <> nil then
  begin
    AdvPageControl.Invalidate;
  end;
end;

 
procedure TplTabSheet.DoHide;
begin
  if Assigned(FOnHide) then FOnHide(Self);
end;

procedure TplTabSheet.Paint;
var  r: trect;
begin
  Color:=DefiTabSheetBkColor;
  inherited;
  r := clientrect;
  inflaterect(r,-1,-1);
  if DefiTabSheetBkColorTo <> clNone then
  begin
    DrawGradient(Canvas, Color, DefiTabSheetBkColorTo, 64, clientRect, false);
  end;
end;

procedure TplTabSheet.DoShow;
begin
  if Assigned(FOnShow) then FOnShow(Self);
end;

function TplTabSheet.GetPageIndex: Integer;
begin
  if FAdvPageControl <> nil then
    Result := FAdvPageControl.FPages.IndexOf(Self) else
    Result := -1;
end;

function TplTabSheet.GetTabIndex: Integer;
var
  I: Integer;
begin
  Result := 0;
  if not FTabShowing then Dec(Result) else
    for I := 0 to PageIndex - 1 do
      if TplTabSheet(FAdvPageControl.FPages[I]).FTabShowing then
        Inc(Result);
end;

procedure TplTabSheet.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params.WindowClass do
    Style := Style and not (CS_HREDRAW or CS_VREDRAW);
end;

procedure TplTabSheet.ReadState(Reader: TReader);
begin
  inherited ReadState(Reader);
  if Reader.Parent is TplPageControl then
    AdvPageControl := TplPageControl(Reader.Parent);
end;

function TplTabSheet.GetImageIndex: TImageIndex;
begin
  Result := FImageIndex;
end;

procedure TplTabSheet.SetImageIndex(Value: TImageIndex);
begin
  if Value < 0 then
  begin
    FImageIndex := -1;
    FImageIndexDummy := 0;
  end
  else
  begin
    FImageIndex := Value;
    FImageIndexDummy := Value;
  end;
  SetCaption(Caption);
end;

procedure TplTabSheet.SeTplPageControl(AAdvPageControl: TplPageControl);
begin
  if FAdvPageControl <> AAdvPageControl then
  begin
    if FAdvPageControl <> nil then FAdvPageControl.RemovePage(Self);
    Parent := AAdvPageControl;
    if AAdvPageControl <> nil then
    begin
      AAdvPageControl.InsertPage(Self);

      if FTextColor = clBlack then
        FTextColor := AAdvPageControl.DefaultTextColor;

    end;
  end;
end;

procedure TplTabSheet.SetPageIndex(Value: Integer);
var
  I, MaxPageIndex: Integer;
begin
  if FAdvPageControl <> nil then
  begin
    MaxPageIndex := FAdvPageControl.FPages.Count - 1;

    if Value > MaxPageIndex then exit;

 {   if Value > MaxPageIndex then          ct9999
      raise EListError.CreateResFmt(@SPageIndexError, [Value, MaxPageIndex]);
 

      raise EListError.CreateFmt(SPageIndexError, [Value, MaxPageIndex]);   }
 
 
    I := TabIndex;
    FAdvPageControl.FPages.Move(PageIndex, Value);
    if I >= 0 then FAdvPageControl.MoveTab(I, TabIndex);
  end;
end;

procedure TplTabSheet.SetTabShowing(Value: Boolean);
var Index: Integer;
begin
  if FTabShowing = Value then exit;

    if Value then
    begin
      FTabShowing := True;
      FAdvPageControl.InsertTab(Self);
    end else
    begin
      Index := TabIndex;
      FTabShowing := False;
      FAdvPageControl.DeleteTab(Self, Index);
    end;
end;

procedure TplTabSheet.SetTabVisible(Value: Boolean);
begin
  if FTabVisible <> Value then
  begin
    FTabVisible := Value;
    UpdateTabShowing;

    if AdvPageControl <> nil then
      AdvPageControl.UpdateTab(self);
  end;
end;

procedure TplTabSheet.UpdateTabShowing;
begin
  SetTabShowing((FAdvPageControl <> nil) and FTabVisible);
end;

procedure TplTabSheet.CMTextChanged(var Message: TMessage);
begin
  if FTabShowing then FAdvPageControl.UpdateTab(Self);
end;

procedure TplTabSheet.CMShowingChanged(var Message: TMessage);
begin
  inherited;
  if Showing then
  begin
    try
      DoShow
    except
      Application.HandleException(Self);
    end;
  end else if not Showing then
  begin
    try
      DoHide;
    except
      Application.HandleException(Self);
    end;
  end;
end;

procedure TplTabSheet.CloseButtonClick(Sender: TObject);
begin
  if Assigned(FOnClose) then FOnClose(Self);
end;

function TplTabSheet.CanCloseClick(Sender: TObject): Boolean;
var
  CanClose: Boolean;
begin
  CanClose := True;
  if Assigned(FOnCanClose) then
    FOnCanClose(Self, CanClose);
  Result := CanClose;
end;


procedure TplTabSheet.SetHighlighted(Value: Boolean);
begin
  if not (csReading in ComponentState) then
    SendMessage(AdvPageControl.Handle, LM_ACTIVATEITEM, TabIndex,
      MakeLong(Word(Value), 0));
  FHighlighted := Value;
end;


procedure TplTabSheet.SetTabEnable(const Value: boolean);
begin
  if FTabEnable <> Value then
  begin
    if AdvPageControl <> nil then
    begin
      if AdvPageControl.ActivePageIndex = TabIndex then
        raise exception.Create('Can not disable active tab.');

      FTabEnable := Value;
      AdvPageControl.Invalidate;
    end;
    FTabEnable := Value;
  end;
end;

procedure TplTabSheet.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if AdvPageControl <> nil then
  begin
    AdvPageControl.Invalidate;
  end;
end;

//=================== TplPageControl ==============================================

constructor TplPageControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := [csDoubleClicks, csOpaque];

  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;

  FPages := TList.Create;
  FClosedTabList := TStringList.Create;

  FDummyImages := TCustomImageList.Create(self);
  FDummyImages.Width := 1; //5;
  FDummyImages.Height := 1; //5;

  FDefaultTextColor := clBlack;

  FFreeOnClose := true;
  FTabSheet3D := False;
  FTabSplitLine := False;
  FTabBorderColor := clGray; 

  FTabBackGround := TBitmap.Create;
  FTabBackGroundActive := TBitmap.Create;

  FHoverTab := -1;
  FShowFocus := false;

  FHoverClosedButton := false;

  FTabOverlap := 0;

  FTabSheetBorderColor := clGray;
  FTabBackGroundColor := clBtnFace;

  FTabBorder3D := False;
  inherited Images := FDummyImages;
  OwnerDraw := not FTabBorder3D;

  FRoundEdges := false;

  FTabStyle := tsClassic;
  FLowerActive := 0;//2

  FCloseGlyph := TBitmap.Create;

  // make sure to use a Truetype font
  Font.Name := 'Tahoma';

  FTabMargin := TTabMargin.Create;
  FTabMargin.LeftMargin := 0;
  FTabMargin.TopMargin := 0;
  FTabMargin.RightMargin := 0; //5;
  FTabMargin.OnMarginChange := TabMarginChange;

  FActiveFont := TFont.Create;
  FActiveFont.Name := 'Tahoma';
  FActiveFont.OnChange:= ActiveFontChangeEvent;
  inherited OnChange := TabChange;

  FFullRefresh := False;


  FNextDialogOnEnter:=True;
  FDialogOnCursorKeys:=True;
  FAutoFocus:=False;

 
  FFocusWidthInc:=0;

  ParentFont:=False;
  IsLoaded:=False;
  FTabDrawOutLine:=true;
end;

destructor TplPageControl.Destroy;
var
  I: Integer;
begin

  FCloseGlyph.Free;
  FTabBackGround.Free;
  FTabBackGroundActive.Free;

  for I := 0 to FPages.Count - 1 do
    TplTabSheet(FPages[I]).FAdvPageControl := nil;

  if FDummyImages <> nil then
    FDummyImages.Free;

  if FClosedTabList <> nil then
    FClosedTabList.Free;

  FPages.Free;
  FTabMargin.Free;
  FActiveFont.Free;

  FreeAndNil(FCanvas);

  inherited Destroy;
end;


procedure TplPageControl.KeyPress(var Key: Char);
begin
 // if (Key = #13) and FNextDialogOnEnter then
 //   (Owner as TControl).Perform(LM_NEXTDLGCTL,0,0);

  inherited;
end;

procedure TplPageControl.KeyDown(var Key: Word; Shift: TShiftState);
var
  CtlDir: Word;
begin
  if FDialogOnCursorKeys and ((Key = VK_UP) or (Key = VK_DOWN)) then
  begin
 //   if Key = VK_UP then CtlDir:=1 else CtlDir:=0;
 //   (Owner as TControl).Perform(LM_NEXTDLGCTL,CtlDir,0);

  end
  else inherited KeyDown(Key,Shift);
end;


procedure TplPageControl.UpdateTabHighlights;
var
  I: Integer;
begin
  for I := 0 to PageCount - 1 do
    Pages[I].SetHighlighted(Pages[I].FHighlighted);
end;

procedure TplPageControl.Loaded;
var
  i: integer;
begin
  inherited Loaded;
  UpdateTabHighlights;
  SetTabMargins;
  FPropertiesLoaded := true;

  for i := PageCount - 1 downto 0 do
  begin
    UpdateTab(Pages[I]);
  end;

  if FActivePage <> nil then
    UpdateTabForActiveFont(FActivePage);
end;


function TplPageControl.CanShowTab(TabIndex: Integer): Boolean;
begin
  Result := TplTabSheet(FPages[TabIndex]).Enabled;
end;

procedure TplPageControl.Change;
var
  Form: TCustomForm;
begin
  UpdateActivePage;
  if csDesigning in ComponentState then
  begin
    Form := GetParentForm(Self);
    if (Form <> nil) and (Form.Designer <> nil) then
      Form.Designer.Modified;
  end;
  inherited Change;
end;

function TplPageControl.CanChange: Boolean;
var
  pt: TPoint;
  AllowChange: Boolean;
  NewPage: Integer;
begin
  GetCursorPos(pt);
  pt := ScreenToClient(pt);

  NewPage := IndexOfTabAtEx(pt.X,pt.Y);

  AllowChange := true;

  if Assigned(OnCanChange) then
    OnCanChange(Self, ActivePageIndex, NewPage, AllowChange);

  if not AllowChange then
    Result := False
  else
    Result := inherited CanChange;
end;


procedure TplPageControl.ChangeActivePage(Page: TplTabSheet);
var
  ParentForm: TCustomForm;
  OldActivePage: TplTabSheet;
begin
  if (FActivePage <> Page) and Assigned(Page) then
  begin
    if Assigned(Page) and not (csDesigning in ComponentState) and (not Page.TabEnable) then
      Exit;

    ParentForm := GetParentForm(Self);
    if (ParentForm <> nil) and (FActivePage <> nil) and
      FActivePage.ContainsControl(ParentForm.ActiveControl) then
    begin
      ParentForm.ActiveControl := FActivePage;
      if ParentForm.ActiveControl <> FActivePage then
      begin
        TabIndex := FActivePage.TabIndex;
        Exit;
      end;
    end;

    if Page <> nil then
    begin
      Page.BringToFront;
      Page.Visible := True;
      if (ParentForm <> nil) and (FActivePage <> nil) and
        (ParentForm.ActiveControl = FActivePage) then
        if Page.CanFocus then
          ParentForm.ActiveControl := Page else
          ParentForm.ActiveControl := Self;
    end;

    OldActivePage := FActivePage;

    if FActivePage <> nil then
      FActivePage.Visible := False;

    FActivePage := Page;

    UpdateTabForActiveFont(OldActivePage);
    UpdateTabForActiveFont(FActivePage);

    if (ParentForm <> nil) and (FActivePage <> nil) and
      (ParentForm.ActiveControl = FActivePage) then
      FActivePage.SelectFirstControl;
  end;
end;

procedure TplPageControl.DeleteTab(Page: TplTabSheet;Index: Integer);
var UpdateIndex: Boolean;  
begin

  UpdateIndex := Page = ActivePage;
  Tabs.Delete(Index);

  if UpdateIndex then
  begin
    if Index >= Tabs.Count then Index := Tabs.Count - 1;
    TabIndex := Index;
  end;
  UpdateActivePage;
end;

procedure TplPageControl.DoAddDockClient(Client: TControl; const ARect: TRect);
begin
  if FNewDockSheet <> nil then Client.Parent := FNewDockSheet;
end;

procedure TplPageControl.DockOver(Source: TDragDockObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  R: TRect;
begin
  GetWindowRect(Handle, R);
  Source.DockRect := R;
  DoDockOver(Source, X, Y, State, Accept);
end;

procedure TplPageControl.DoRemoveDockClient(Client: TControl);
begin
  if (FUndockingPage <> nil) and not (csDestroying in ComponentState) then
  begin
    SelectNextPage(True);
    FUndockingPage.Free;
    FUndockingPage := nil;
  end;
end;

function TplPageControl.FindNextPage(CurPage: TplTabSheet; GoForward, CheckTabVisible: Boolean): TplTabSheet;
var
  I, StartIndex: Integer;
begin
  if FPages.Count <> 0 then
  begin
    StartIndex := FPages.IndexOf(CurPage);
    if StartIndex = -1 then
      if GoForward then StartIndex := FPages.Count - 1 else StartIndex := 0;
    I := StartIndex;
    repeat
      if GoForward then
      begin
        Inc(I);
        if I = FPages.Count then I := 0;
      end else
      begin
        if I = 0 then I := FPages.Count;
        Dec(I);
      end;

      Result := TplTabSheet(FPages[I]);

      if not CheckTabVisible or Result.TabVisible then Exit;
    until I = StartIndex;
  end;
  Result := nil;
end;

procedure TplPageControl.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
begin
  for I := 0 to FPages.Count - 1 do Proc(TComponent(FPages[I]));
end;

function TplPageControl.GetImageIndex(TabIndex: Integer): Integer;
var
  I,
    Visible,
    NotVisible: Integer;
begin
  if Assigned(OnGetImageIndex) then
    Result := inherited GetImageIndex(TabIndex) else
  begin
     { For a AdvPageControl, TabIndex refers to visible tabs only. The control
     doesn't store }
    Visible := 0;
    NotVisible := 0;
    for I := 0 to FPages.Count - 1 do
    begin
      if not GetPage(I).TabVisible then Inc(NotVisible)
      else Inc(Visible);
      if Visible = TabIndex + 1 then Break;
    end;

    Result := GetPage(TabIndex + NotVisible).ImageIndexDummy;
  end;
end;

function TplPageControl.GetPageFromDockClient(Client: TControl): TplTabSheet;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to PageCount - 1 do
  begin
    if (Client.Parent = Pages[I]) and (Client.HostDockSite = Self) then
    begin
      Result := Pages[I];
      Exit;
    end;
  end;
end;

function TplPageControl.GetPage(Index: Integer): TplTabSheet;
begin
  Result := TplTabSheet(FPages[Index]);
end;

function TplPageControl.GetPageCount: Integer;
begin
  Result := FPages.Count;
end;

procedure TplPageControl.GetSiteInfo(Client: TControl; var InfluenceRect: TRect;
  MousePos: TPoint; var CanDock: Boolean);
begin
  CanDock := GetPageFromDockClient(Client) = nil;
  inherited GetSiteInfo(Client, InfluenceRect, MousePos, CanDock);
end;

procedure TplPageControl.InsertPage(Page: TplTabSheet);
begin
  FPages.Add(Page);
  Page.FAdvPageControl := Self;
  Page.UpdateTabShowing;
end;

procedure TplPageControl.InsertTab(Page: TplTabSheet);
begin
  Tabs.InsertObject(Page.TabIndex, Page.TabCaption {Caption}, Page);
  UpdateActivePage;
end;

procedure TplPageControl.MoveTab(CurIndex, NewIndex: Integer);
begin
  Tabs.Move(CurIndex, NewIndex);
end;

procedure TplPageControl.RemovePage(Page: TplTabSheet);
var
  NextSheet: TplTabSheet;
begin
  NextSheet := FindNextPage(Page, True, not (csDesigning in ComponentState));
  if NextSheet = Page then NextSheet := nil;
  Page.SetTabShowing(False);
  Page.FAdvPageControl := nil;
  FPages.Remove(Page);
  SetActivePage(NextSheet);
end;

procedure TplPageControl.SelectNextPage(GoForward: Boolean);
var
  Page: TplTabSheet;
begin
  Page := FindNextPage(ActivePage, GoForward, True);
  if (Page <> nil) and (Page <> ActivePage) and CanChange then
  begin
    TabIndex := Page.TabIndex;
    Change;
  end;
end;

procedure TplPageControl.SetActivePage(Page: TplTabSheet);
begin
  if (Page <> nil) and (Page.AdvPageControl <> Self) then Exit;
  ChangeActivePage(Page);
  if Page = nil then
  begin
    TabIndex := -1;
    FActivePage:= nil;
  end
  else if Page = FActivePage then
    TabIndex := Page.TabIndex;
end;

procedure TplPageControl.SetChildOrder(Child: TComponent; Order: Integer);
begin
  TplTabSheet(Child).PageIndex := Order;
end;

procedure TplPageControl.ShowControl(AControl: TControl);
begin
  if (AControl is TplTabSheet) and (TplTabSheet(AControl).AdvPageControl = Self) then
    SetActivePage(TplTabSheet(AControl));
  inherited ShowControl(AControl);
end;

procedure TplPageControl.DrawHoverCloseButton(Rect: TRect);
var
  P: TPoint;
begin
  if not FCloseGlyph.Empty then
  begin
    case TabPosition of
      tpLeft:
      begin
        Rect.Left := Rect.Left + FTabMargin.LeftMargin + FLowerActive {2} {Active};
        Rect.Bottom := Rect.Bottom - FTabMargin.TopMargin;
        if not FTabBackGroundActive.Empty then
          Rect.Bottom := Rect.Bottom + TabOverlap;
        P.X:= Rect.Left + 2;
        P.Y:= Rect.Bottom - CloseButtonHeight;
      end;
      tpRight:
      begin
        Rect.Left := Rect.Left + FTabMargin.LeftMargin;
        Rect.Top := Rect.Top + FTabMargin.TopMargin;
        P.X:= Rect.Left + 2;
        P.Y:= Rect.Top + 2;
      end;
      tpTop:
      begin
        Rect.Left := Rect.Left + FTabMargin.LeftMargin;
        Rect.Top := Rect.Top + FTabMargin.TopMargin + {2} FLowerActive;
        P.X:= Rect.Left + 2;
        P.Y:= Rect.Top + 2;
      end;
      tpBottom:
      begin
        Rect.Left := Rect.Left + FTabMargin.LeftMargin;
        Rect.Top := Rect.Top + FTabMargin.TopMargin + {2} FLowerActive;
        P.X:= Rect.Left + 2;
        P.Y:= Rect.Top + 2;
      end;
    end;
    DrawCloseGlyph(P);
  end
  else
  begin
    if TabPosition = tpLeft then
    begin
      Rect.Left := Rect.Left + FTabMargin.LeftMargin + FLowerActive {2} {Active};
      Rect.Bottom := Rect.Bottom - FTabMargin.TopMargin;
      if not FTabBackGroundActive.Empty then
        Rect.Bottom := Rect.Bottom + TabOverlap;

      with Canvas do
      begin
        Brush.Color := clWhite;
        Pen.Color := clBlack; //clGray;
        Rectangle(Rect.Left + 2, Rect.Bottom - 2, Rect.Left + CloseButtonWidth, Rect.Bottom - CloseButtonHeight);
        Pen.Color := clGray; //clBlack;
                      {/}
        MoveTo(Rect.Left + 2 + 3, Rect.Bottom - 2 - 3 - 4 - 1);
        LineTo(Rect.Left + 2 + 3 + 6, Rect.Bottom - 2 - 2 - 1); //-1
        MoveTo(Rect.Left + 2 + 4, Rect.Bottom - 2 - 3 - 4 - 1);
        LineTo(Rect.Left + 2 + 3 + 5, Rect.Bottom - 2 - 2 - 1);
                      {\}
        MoveTo(Rect.Left + 2 + 3, Rect.Bottom - 2 - 3 - 1);
        LineTo(Rect.Left + 2 + 3 + 6, Rect.Bottom - 2 - 3 - 5 - 1);
        MoveTo(Rect.Left + 2 + 4, Rect.Bottom - 2 - 3 - 1);
        LineTo(Rect.Left + 2 + 3 + 5, Rect.Bottom - 2 - 3 - 5 - 1);
      end;
    end
    else
    begin
      if TabPosition = tpRight then
      begin
        Rect.Left := Rect.Left + FTabMargin.LeftMargin;
        Rect.Top := Rect.Top + FTabMargin.TopMargin;
      end
      else if TabPosition = tpTop then
      begin
        Rect.Left := Rect.Left + FTabMargin.LeftMargin;
        Rect.Top := Rect.Top + FTabMargin.TopMargin + {2} FLowerActive;
      end
      else // Bottom
      begin
        Rect.Left := Rect.Left + FTabMargin.LeftMargin;
        Rect.Top := Rect.Top + FTabMargin.TopMargin + {2} FLowerActive;
      end;

      with canvas do
      begin
        Brush.Color := clWhite;
        Pen.Color := clBlack; //clGray;
        Rectangle(Rect.Left + 2, Rect.Top + 2, Rect.Left + CloseButtonWidth, Rect.Top + CloseButtonHeight);
        Pen.Color := clGray; //clBlack;
                      {/}
        MoveTo(Rect.Left + 2 + 3, Rect.Top + 2 + 3 + 4);
        LineTo(Rect.Left + 2 + 3 + 6, Rect.Top + 2 + 2); //-1
        MoveTo(Rect.Left + 2 + 4, Rect.Top + 2 + 3 + 4);
        LineTo(Rect.Left + 2 + 3 + 5, Rect.Top + 2 + 2);
                      {\}
        MoveTo(Rect.Left + 2 + 3, Rect.Top + 2 + 3);
        LineTo(Rect.Left + 2 + 3 + 6, Rect.Top + 2 + 3 + 5);
        MoveTo(Rect.Left + 2 + 4, Rect.Top + 2 + 3);
        LineTo(Rect.Left + 2 + 3 + 5, Rect.Top + 2 + 3 + 5);
      end;
    end;
  end;
end;

procedure TplPageControl.DrawDownCloseButton(Rect: TRect);
var
  P: TPoint;
begin
  if not FCloseGlyph.Empty then
  begin
    case TabPosition of
      tpLeft:
      begin
        Rect.Left := Rect.Left + FTabMargin.LeftMargin + {2} FLowerActive {active};
        Rect.Bottom := Rect.Bottom - FTabMargin.TopMargin;
        if not FTabBackGroundActive.Empty then
          Rect.Bottom := Rect.Bottom + TabOverlap;
        P.X:= Rect.Left + 2;
        P.y:= Rect.Bottom - CloseButtonHeight;
      end;
      tpRight:
      begin
        Rect.Left := Rect.Left + FTabMargin.LeftMargin - FLowerActive + 2;
        Rect.Top := Rect.Top + FTabMargin.TopMargin;
        P.X:= Rect.Left + 2;
        P.y:= Rect.Top + 2;
      end;
      tpTop:
      begin
        Rect.Left := Rect.Left + FTabMargin.LeftMargin;
        Rect.Top := Rect.Top + FTabMargin.TopMargin + FLowerActive;
        P.X:= Rect.Left + 2;
        P.y:= Rect.Top + 2;
      end;
      tpBottom:
      begin
        Rect.Left := Rect.Left + FTabMargin.LeftMargin;
        Rect.Top := Rect.Top + FTabMargin.TopMargin +  FLowerActive;
        P.X:= Rect.Left + 2;
        P.y:= Rect.Top + 2;
      end;
    end;
    DrawCloseGlyph(P);
  end
  else
  begin
    if TabPosition = tpLeft then
    begin
      Rect.Left := Rect.Left + FTabMargin.LeftMargin + {2} FLowerActive {active};
      Rect.Bottom := Rect.Bottom - FTabMargin.TopMargin;
      if not FTabBackGroundActive.Empty then
        Rect.Bottom := Rect.Bottom + TabOverlap;

      with Canvas do
      begin
        Brush.Color := clSilver; //clWhite;
        Pen.Color := clBlack; //clGray;
        Rectangle(Rect.Left + 2, Rect.Bottom - 2, Rect.Left + CloseButtonWidth, Rect.Bottom - CloseButtonHeight);
        Pen.Color := clBlack;
                      {/}
        MoveTo(Rect.Left + 2 + 3, Rect.Bottom - 2 - 3 - 4 - 1);
        LineTo(Rect.Left + 2 + 3 + 6, Rect.Bottom - 2 - 2 - 1); //-1
        MoveTo(Rect.Left + 2 + 4, Rect.Bottom - 2 - 3 - 4 - 1);
        LineTo(Rect.Left + 2 + 3 + 5, Rect.Bottom - 2 - 2 - 1);
                      {\}
        MoveTo(Rect.Left + 2 + 3, Rect.Bottom - 2 - 3 - 1);
        LineTo(Rect.Left + 2 + 3 + 6, Rect.Bottom - 2 - 3 - 5 - 1);
        MoveTo(Rect.Left + 2 + 4, Rect.Bottom - 2 - 3 - 1);
        LineTo(Rect.Left + 2 + 3 + 5, Rect.Bottom - 2 - 3 - 5 - 1);
      end;
    end
    else
    begin
      if TabPosition = tpRight then
      begin
        Rect.Left := Rect.Left + FTabMargin.LeftMargin - FLowerActive + 2;
        Rect.Top := Rect.Top + FTabMargin.TopMargin;
      end
      else if TabPosition = tpTop then
      begin
        Rect.Left := Rect.Left + FTabMargin.LeftMargin;
        Rect.Top := Rect.Top + FTabMargin.TopMargin + {2} FLowerActive;
      end
      else // bottom
      begin
        Rect.Left := Rect.Left + FTabMargin.LeftMargin;
        Rect.Top := Rect.Top + FTabMargin.TopMargin + {2} FLowerActive;
      end;

      with canvas do
      begin
        Brush.Color := clSilver; //clWhite;
        Pen.Color := clBlack; //clGray;
        Rectangle(Rect.Left + 2, Rect.Top + 2, Rect.Left + CloseButtonWidth, Rect.Top + CloseButtonHeight);
        Pen.Color := clBlack;
                      {/}
        MoveTo(Rect.Left + 2 + 3, Rect.Top + 2 + 3 + 4);
        LineTo(Rect.Left + 2 + 3 + 6, Rect.Top + 2 + 2); //-1
        MoveTo(Rect.Left + 2 + 4, Rect.Top + 2 + 3 + 4);
        LineTo(Rect.Left + 2 + 3 + 5, Rect.Top + 2 + 2);
                      {\}
        MoveTo(Rect.Left + 2 + 3, Rect.Top + 2 + 3);
        LineTo(Rect.Left + 2 + 3 + 6, Rect.Top + 2 + 3 + 5);
        MoveTo(Rect.Left + 2 + 4, Rect.Top + 2 + 3);
        LineTo(Rect.Left + 2 + 3 + 5, Rect.Top + 2 + 3 + 5);
      end;
    end;
  end;
end;


procedure TplPageControl.DrawCloseGlyph(P: TPoint);
var
  GlRgn: HRGN;
begin
  if not CloseGlyph.Empty then
  begin
    GlRgn := CreateRectRgn(P.X, P.Y, P.X + CloseButtonWidth-2, P.Y + CloseButtonHeight - 2);
    SelectClipRgn(Canvas.Handle,GlRgn);
    CloseGlyph.Transparent:= true;
    CloseGlyph.TransparentMode:= tmAuto;
    Canvas.Draw(P.X, P.Y, CloseGlyph);
    
    SelectClipRgn(Canvas.Handle,0);
    DeleteObject(GlRgn);
  end;
end;


procedure TplPageControl.DrawCloseButton(Rect: TRect; Active: Boolean);
var
  a: integer;
  P: TPoint;
begin
  if Active then a := {2} FLowerActive
  else a := 0;

  if not FCloseGlyph.Empty then
  begin
    case TabPosition of
      tpLeft:
      begin
        Rect.Left := Rect.Left + FTabMargin.LeftMargin + a;
        Rect.Bottom := Rect.Bottom - FTabMargin.TopMargin;
        if not FTabBackGroundActive.Empty then
          Rect.Bottom := Rect.Bottom + TabOverlap;

        P.X:= Rect.Left + 2;
        P.Y:= Rect.Bottom - CloseButtonHeight;
      end;
      tpRight:
      begin
        Rect.Left := Rect.Left + FTabMargin.LeftMargin + 2 - a;
        Rect.Top := Rect.Top + FTabMargin.TopMargin;

        P.X:= Rect.Left + 2;
        P.Y:= Rect.Top + 2;
      end;
      tpTop:
      begin
        Rect.Left := Rect.Left + FTabMargin.LeftMargin;
        Rect.Top := Rect.Top + FTabMargin.TopMargin + a;

        P.X:= Rect.Left + 2;
        P.Y:= Rect.Top + 2;
      end;
      tpBottom:
      begin
        Rect.Left := Rect.Left + FTabMargin.LeftMargin;
        Rect.Top := Rect.Top + FTabMargin.TopMargin + a;
        
        P.X:= Rect.Left + 2;
        P.Y:= Rect.Top + 2;
      end;
    end;
    DrawCloseGlyph(P);
  end
  else
  begin
    if TabPosition = tpLeft then
    begin
      Rect.Left := Rect.Left + FTabMargin.LeftMargin + a;
      Rect.Bottom := Rect.Bottom - FTabMargin.TopMargin;
      if not FTabBackGroundActive.Empty then
        Rect.Bottom := Rect.Bottom + TabOverlap;

      with canvas do
      begin
        Brush.Color := clWhite;
        Pen.Color := clGray;
        Rectangle(Rect.Left + 2, Rect.Bottom - 2, Rect.Left + CloseButtonWidth, Rect.Bottom - CloseButtonHeight);
        Pen.Color := clBlack;
                      {/}
        MoveTo(Rect.Left + 2 + 3, Rect.Bottom - 2 - 3 - 4 - 1);
        LineTo(Rect.Left + 2 + 3 + 6, Rect.Bottom - 2 - 2 - 1); //-1
        MoveTo(Rect.Left + 2 + 4, Rect.Bottom - 2 - 3 - 4 - 1);
        LineTo(Rect.Left + 2 + 3 + 5, Rect.Bottom - 2 - 2 - 1);
                      {\}
        MoveTo(Rect.Left + 2 + 3, Rect.Bottom - 2 - 3 - 1);
        LineTo(Rect.Left + 2 + 3 + 6, Rect.Bottom - 2 - 3 - 5 - 1);
        MoveTo(Rect.Left + 2 + 4, Rect.Bottom - 2 - 3 - 1);
        LineTo(Rect.Left + 2 + 3 + 5, Rect.Bottom - 2 - 3 - 5 - 1);
      end;
    end
    else
    begin
      if TabPosition = tpRight then
      begin
        Rect.Left := Rect.Left + FTabMargin.LeftMargin + 2 - a;
        Rect.Top := Rect.Top + FTabMargin.TopMargin;
      end
      else if TabPosition = tpTop then
      begin
        Rect.Left := Rect.Left + FTabMargin.LeftMargin;
        Rect.Top := Rect.Top + FTabMargin.TopMargin + a;
      end
      else // Bottom
      begin
        Rect.Left := Rect.Left + FTabMargin.LeftMargin;
        Rect.Top := Rect.Top + FTabMargin.TopMargin + a;
      end;

      with canvas do
      begin
        Brush.Color := clWhite;
        Pen.Color := clGray;
        Rectangle(Rect.Left + 2, Rect.Top + 2, Rect.Left + CloseButtonWidth, Rect.Top + CloseButtonHeight);
        Pen.Color := clBlack;
                      {/}
        MoveTo(Rect.Left + 2 + 3, Rect.Top + 2 + 3 + 4);
        LineTo(Rect.Left + 2 + 3 + 6, Rect.Top + 2 + 2); //-1
        MoveTo(Rect.Left + 2 + 4, Rect.Top + 2 + 3 + 4);
        LineTo(Rect.Left + 2 + 3 + 5, Rect.Top + 2 + 2);
                      {\}
        MoveTo(Rect.Left + 2 + 3, Rect.Top + 2 + 3);
        LineTo(Rect.Left + 2 + 3 + 6, Rect.Top + 2 + 3 + 5);
        MoveTo(Rect.Left + 2 + 4, Rect.Top + 2 + 3);
        LineTo(Rect.Left + 2 + 3 + 5, Rect.Top + 2 + 3 + 5);
      end;
    end;
  end;
end;

function TplPageControl.IsOnButton(TabIndex, X, Y: integer): Boolean;
var
  r: TRect;
begin
  Result := false;
  r := TabRectEx(TabIndex);
  if TabPosition = tpLeft then
  begin
    r.Left := r.Left + FTabMargin.LeftMargin {Active};
    r.Bottom := r.Bottom - FTabMargin.TopMargin;
    if not FTabBackGroundActive.Empty then
    begin
      r.Left := r.Left + 2;
      r.Bottom := r.Bottom + TabOverlap;
    end;

    r := Rect(r.Left + 2, r.Bottom - CloseButtonHeight, r.Left + CloseButtonWidth, r.Bottom - 2);
    if PtInRect(r, Point(X, Y)) then
    begin
      Result := true;
    end;
  end
  else
  begin
    r.Left := r.Left + FTabMargin.LeftMargin;
    r.Top := r.Top + FTabMargin.TopMargin;
    r := Rect(r.Left + 2, r.Top + 2, r.Left + CloseButtonWidth, r.Top + CloseButtonHeight);
    if PtInRect(r, Point(X, Y)) then
    begin
      Result := true;
    end;
  end;
end;

procedure TplPageControl.UpdateTabForActiveFont(Page: TplTabSheet);
var
  aFont: TFont;
  tw, Atw: integer;
  s: String;
begin
  if not TabBorder3D and (Page <> nil) then
  begin
    if (FActivePage = Page) and ((fsBold in ActiveFont.Style) or (fsItalic in ActiveFont.Style)) then
    begin
      aFont:= TFont.Create;
      aFont.Assign(Canvas.Font);
      Canvas.Font.Assign(FActiveFont);
      Atw:= Canvas.TextWidth(Page.TabCaption);

      Canvas.Font.Assign(Font);
      tw:= Canvas.TextWidth(Page.TabCaption);

      Atw:= Atw - tw;
      s:= '';
      while Canvas.TextWidth(s) < Atw  do
        S:= S + ' ';

      if (Page.TabIndex >= 0) and (Page.TabIndex < Tabs.Count) then
        Tabs[Page.TabIndex] := Page.TabCaption + S;

      Canvas.Font.Assign(aFont);
      aFont.Free;
    end
    else
    begin
      if (Page.TabIndex >= 0) and (Page.TabIndex < Tabs.Count) then
        Tabs[Page.TabIndex] := Page.TabCaption;
    end;
  end;
end;

procedure TplPageControl.UpdateTab(Page: TplTabSheet);
begin
  if Page.TabIndex >= 0 then
  begin
    if TabBorder3D then
      Tabs[Page.TabIndex] := Page.Caption
    else
    begin
      if (FActivePage = Page) and ((fsBold in ActiveFont.Style) or (fsItalic in ActiveFont.Style)) then
        UpdateTabForActiveFont(Page)
      else
        Tabs[Page.TabIndex] := Page.TabCaption;
    end;  
  end;   
end;

procedure TplPageControl.UpdateActivePage;
begin
  if TabIndex >= 0 then
    SetActivePage(TplTabSheet(Tabs.Objects[TabIndex]))
  else
    SetActivePage(nil);
end;

procedure TplPageControl.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_PAINT: begin

               if Focused then
                    TabBackGroundColor:=DefiPageControlFocusColor else
                    TabBackGroundColor:=DefiPageControlNoFocusColor;
              end;

    WM_SETFOCUS  : Repaint;
    WM_KILLFOCUS : Repaint;
    cm_MouseEnter: if FAutoFocus then SetFocus;
  
  end;
  inherited;
  if (Message.Msg = WM_PAINT) and (not FTabBorder3D) then
  begin


    Message.Result := 0;
    DrawAllTabs(Canvas);
  end;
end;

procedure TplPageControl.TabChange(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnChange(Sender);
  Invalidate;
end;

procedure TplPageControl.TabMarginChange(NewValue, OldValue: TMarginSize; Index: integer);
var
  ImgW, ImgH: integer;
begin
  if FPropertiesLoaded or (csDesigning in ComponentState) then
  begin
    if FImages <> nil then
    begin
      ImgW := FImages.Width;
      ImgH := FImages.Height;
    end
    else
    begin
      ImgW := 0;
      ImgH := 0;
    end;

    case Index of
      0: // Left
        begin
          if TabPosition in [tpTop, tpBottom] then
          begin
            FDummyImages.Width := ImgW + TabMargin.RightMargin + NewValue;
          end;
        end;
      1: // Top
        begin
          if TabPosition in [tpLeft, tpRight] then
          begin
            FDummyImages.Height := ImgH + TabMargin.RightMargin + NewValue;
          end;
        end;
      2: // Right
        begin
          if FPropertiesLoaded or (csDesigning in ComponentState) then
            if TabPosition in [tpLeft, tpRight] then
            begin
              FDummyImages.Height := FDummyImages.Height - OldValue;
              FDummyImages.Height := FDummyImages.Height + NewValue;
            end
            else // tpTop, tpBottom
            begin
              FDummyImages.Width := FDummyImages.Width - OldValue;
              FDummyImages.Width := FDummyImages.Width + NewValue;
            end;
        end;
    end;
  end;
  Invalidate;
end;

procedure TplPageControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var   tabIndex: Integer;
      aAdvTabSheet: TplTabSheet;
      cAllow:Boolean;
begin
  inherited;
  if (csDesigning in ComponentState) then  Exit;

  if FTabBorder3D then  Exit;

  if (Button = mbLeft) then
  begin
    tabIndex := GetActivePageIndex;

   if (TplTabSheet(Pages[TabIndex]).ShowClose) and IsOnButton(tabIndex, X, Y) then
    begin
      aAdvTabSheet := FindNextPage(ActivePage, true, true);

        //... Send msg For close/Delete from PageControl ...........
           cAllow:=true;
           if Assigned(fOnDelOrCloseTabEvent) then
              fOnDelOrCloseTabEvent(self,ActivePage,ActivePage.TabIndex,cAllow);
           if cAllow=False then exit;
         //..........................................................  

      if TplTabSheet(Pages[TabIndex]).CanCloseClick(TplTabSheet(Pages[TabIndex])) then
      begin
        TplTabSheet(Pages[TabIndex]).CloseButtonClick(TplTabSheet(Pages[TabIndex]));
        ActivePage := aAdvTabSheet;
        TabChange(self);

        if FFreeOnClose then
          TplTabSheet(Pages[TabIndex]).Free
        else
        begin
          FClosedTabList.AddObject(TplTabSheet(Pages[TabIndex]).Name, TplTabSheet(Pages[TabIndex]));
          TplTabSheet(Pages[TabIndex]).AdvPageControl := nil;
        end;
        Invalidate;
      end;
    end;
  end;
end;

procedure TplPageControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var  TabIndex: integer;
     aAdvTabSheet: TplTabSheet;
     cAllow:Boolean;
begin
  inherited;
  if (csDesigning in ComponentState) then  Exit;
  if FTabBorder3D then  Exit;

  TabIndex := ActivePageIndex;

  if TplTabSheet(Pages[TabIndex]).ShowClose then    
    if IsOnButton(TabIndex, X, Y) then
    begin

      if DockSite then  // As Mouse Up event does not triger when DockSite
      begin
        aAdvTabSheet := FindNextPage(ActivePage, true, true);

         //... Send msg For close/Delete from PageControl ...........
           cAllow:=true;
           if Assigned(fOnDelOrCloseTabEvent) then
              fOnDelOrCloseTabEvent(self,ActivePage,ActivePage.TabIndex,cAllow);
           if cAllow=False then exit;
         //..........................................................

        if TplTabSheet(Pages[TabIndex]).CanCloseClick(TplTabSheet(Pages[TabIndex])) then
        begin
          TplTabSheet(Pages[TabIndex]).CloseButtonClick(TplTabSheet(Pages[TabIndex]));
          ActivePage := aAdvTabSheet;
          TabChange(self);


          if FFreeOnClose then
            TplTabSheet(Pages[TabIndex]).Free
          else
          begin
            FClosedTabList.AddObject(TplTabSheet(Pages[TabIndex]).Name, TplTabSheet(Pages[TabIndex]));
            TplTabSheet(Pages[TabIndex]).AdvPageControl := nil;
          end;

          Invalidate;
        end;
      end else // Other wise show down Close Button
        DrawDownCloseButton(TabRectEx(TabIndex));
    end;
end;

procedure TplPageControl.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  tabIndex: Integer;
  R: TRect;
begin
  inherited;

  if (csDesigning in ComponentState) then  Exit;
  if (DefiTabSheetHoverColor = clNone) then Exit;  
  if FTabBorder3D then   Exit;

  if FTabBackGround.Empty then
  begin
    tabIndex := IndexOfTabAtEx(X, Y);
    if (FHoverTab <> tabIndex) and ((DefiTabSheetHoverColor <> clNone) or (DefiTabSheetHoverColorTo <> clNone)) then
    begin
      r := TabRectEx(FHoverTab);
      FHoverTab := tabindex;

      InvalidateRect(self.Handle, @r, true);

      r := TabRectEx(FHoverTab);
      FHoverTab := tabindex;

      InvalidateRect(self.Handle, @r, true);

    end;
  end;

  TabIndex := ActivePageIndex;
  if TplTabSheet(Pages[TabIndex]).ShowClose then
  begin
    if not FHoverClosedButton then
    begin
      if IsOnButton(TabIndex, X, Y) then
      begin
        DrawHoverCloseButton(TabRectEx(TabIndex));
        FHoverClosedButton := True;
      end;
    end
    else
    begin
      if not IsOnButton(TabIndex, X, Y) then
      begin
        DrawCloseButton(TabRectEx(TabIndex), True);
        FHoverClosedButton := false;
      end;
    end;
  end
end;

procedure TplPageControl.CMMouseLeave(var Message: TMessage);
var
  r: TRect;
begin

  if FTabBackGround.Empty then
  begin
    if FHoverTab <> -1 then
    begin
      r := TabRectEx(FHoverTab);
      InvalidateRect(Handle, @r, true);

    end;
  end;
  FHoverTab := -1;
end;

         {

function TabCtrl_GetItemRect(hwnd: HWND; i: Integer; var prc: TRect): BOOL;
begin
  //Result := BOOL(SendMessage(hwnd, TCM_GETITEMRECT, i, LPARAM(@prc)));
end;

function TabCtrl_HitTest(hwndTC: HWND; pinfo: PTCHitTestInfo): Integer;
begin
  Result := SendMessage(hwndTC, TCM_HITTEST, 0, LPARAM(pinfo));
end;

function TabCtrl_IndexOfTabAt(Handle: HWND;R: TRect; X, Y: Integer): Integer;
var HitTest: TTCHitTestInfo;
begin
  Result := -1;

  if PtInRect(R, Point(X, Y)) then
    with HitTest do
    begin
      pt.X := X;
      pt.Y := Y;
      Result := TabCtrl_HitTest(Handle, @HitTest);
    end;
end;     }

 


function TplPageControl.IndexOfTabAtEx(X, Y: Integer): Integer;
var
  i, j: Integer;
begin

  i := IndexOfTabAt(X, Y);

  for j := 0 to PageCount - 1 do
  begin
    if (j <= i) and not TplTabSheet(FPages[j]).TabVisible then
      inc(i);
  end;
  Result := i;
end;

function TplPageControl.TabRectEx(i: Integer): TRect;
var
  j, k: Integer;
begin
  k := i;
  for j := 0 to PageCount - 1 do
  begin
    if (j < i) and not TplTabSheet(FPages[j]).TabVisible then
      dec(k);
  end;

  Result := TabRect(k);

end;

procedure TplPageControl.DrawAllTabs(Canvas: TCanvas);
var
  clr, clrto: TColor;
  tf: TFont;
  lf: TLogFont;
  tbmp: TBitmap;
  TextX, TextY, {df,} i: integer;
  HorizontalGradient: Boolean;
  TabIndex, th: Integer;
  Rect, OrignalRect, TextR: TRect;
  Active: Boolean;
  ActiveTabIndex: integer;
  OverLapActiveRect: TRect;
  ActiveTabX, ActiveTabY: integer;
  r2: TRect;
  ActBottom, MaxTop, MaxLeft, MaxRight, j: integer;
  SplitLineDifference, LX: integer;
  isNextSelected, isLast: Boolean;
  HasTabs: boolean;
  FinalRgn,RgnTop,RgnLeft, RgnRight, RgnBottom: HRGN;
  IsColoredBackGround: Boolean;
  tc:tcolor;


  procedure DrawFocusRectangle(aCanvas: TCanvas; aRect: TRect; Active: Boolean; OverLapDiff: integer);
  begin
    //------- Draw Focus
    if not (csDesigning in ComponentState) then
    begin
      with Canvas do
      begin
        if FShowFocus and Active and Focused then
        begin
          Brush.Style := bsClear;
          Pen.Color := clBlack;
          Pen.Style := psDot;
          if ((TabPosition = tpTop) or (TabPosition = tpBottom)) then
            Rectangle(Rect.Left + 5, Rect.Top + 3, Rect.Right - 5 - OverLapDiff, Rect.Bottom - 3)
          else
            Rectangle(Rect.Left + 5, Rect.Top + 3, Rect.Right - 5, Rect.Bottom - 3 - OverLapDiff);
          Pen.Style := pssolid;
        end;
      end;
    end;
  end;

begin
//  DoubleBuffered := True;

  if Parent <> nil then
    Canvas.Brush.Color := Parent.Brush.Color
  else
    Canvas.Brush.Color := clBtnFace;


  if FTabDrawOutLine then
            tc := FTabSheetBorderColor else
            tc := Color;

  Canvas.Font.Assign(Font);

  ActBottom := 0;
  MaxTop := TabRectEx(0).Top;
  MaxLeft := Width;
  MaxRight := 0;

  SplitLineDifference := 0;

  HasTabs := false;

  for i := PageCount - 1 downto 0 do
  begin
    if TplTabSheet(Pages[I]).TabVisible then
      HasTabs := true;
  end;

  for i := PageCount - 1 downto 0 do
  begin
    Rect := TabRectEx(i);
    if TplTabSheet(Pages[I]).TabVisible then
    begin
      if ActBottom < Rect.Bottom then ActBottom := Rect.Bottom;
      if MaxTop > Rect.Top then MaxTop := Rect.Top;
      if MaxLeft > Rect.Left then MaxLeft := Rect.Left;
      if MaxRight < Rect.Right then MaxRight := Rect.Right;

      Rect.Right := Rect.Right + 2;
      case TabPosition of
        tpLeft:
          begin
            Rect.Left := Rect.Left - 2;
            Rect.Bottom := Rect.Bottom + 2;
            Rect.Right := Rect.Right - 2;
            Rect.Top := Rect.Top - 2;
          end;
        tpRight:
          begin
            Rect.Bottom := Rect.Bottom + 2;
            Rect.Top := Rect.Top - 2;
          end;
        tpTop:
          begin
            Rect.Left := Rect.Left - 2;
            Rect.Top := Rect.Top - 2;
          end;
        tpBottom:
          begin
            Rect.Left := Rect.Left - 2;
            Rect.Bottom := Rect.Bottom + 2;
          end;
      end;
      Canvas.FillRect(Rect);
    end;
  end;
      

  if not HasTabs then
    case TabPosition of
      tpTop: ActBottom := 2;
      tpBottom: MaxTop := Height - 0;
      tpRight: MaxLeft := Width - 0;
      tpLeft: MaxRight := 2;
    end;


  r2.Left := Width - 35;
  r2.Right := Width;

  if not MultiLine then
  begin
    if TabPosition = tpTop then
    begin
      r2.Top := 0;
      r2.Bottom := {TabHeight} ActBottom;
      Canvas.Brush.Color := Color;
      Canvas.FillRect(r2);
      Canvas.Pen.Color := clWhite;

      LX := max(r2.Right - 35, MaxRight);
      Canvas.MoveTo(LX {r2.Right-35}, r2.Bottom);
      Canvas.LineTo(r2.Right, r2.Bottom);
    end
    else if TabPosition = tpBottom then
    begin
      r2.Top := MaxTop; //Height - TabHeight-2;
      r2.Bottom := Height;
      Canvas.Brush.Color := Color;
      Canvas.FillRect(r2);
      Canvas.Pen.Color := cl3DDkShadow;

      LX := max(r2.Right - 35, MaxRight);
      Canvas.MoveTo(LX {r2.Right-35}, r2.Top - 1);
      Canvas.LineTo(r2.Right, r2.Top - 1);
    end;
  end;

  Canvas.Brush.Color := FTabBackGroundColor;

  if TabPosition = tpLeft then
  begin
    Rect.Left := 0;
    Rect.Right := MaxRight;
    Rect.Top := 0;
    
    if FullRefresh then         // FF: Bottom BlackLine Issue
      Rect.Bottom := Height -1
    else
      Rect.Bottom := Height;      
    Canvas.FillRect(Rect);
  end
  else if TabPosition = tpRight then
  begin
    Rect.Left := MaxLeft;
    Rect.Right := Width;
    Rect.Top := 0;

    if FullRefresh then         // FF: Bottom BlackLine Issue
      Rect.Bottom := Height -1
    else
      Rect.Bottom := Height;
    Canvas.FillRect(Rect);
  end
  else if TabPosition = tpTop then
  begin
    Rect.Left := 0;
    Rect.Right := width;
    Rect.Top := 0;
    Rect.Bottom := ActBottom;
    Canvas.FillRect(Rect);
  end
  else if TabPosition = tpBottom then
  begin
    Rect.Left := 0;
    Rect.Right := width;
    Rect.Top := MaxTop;
    Rect.Bottom := Height;
    Canvas.FillRect(Rect);
  end;

  IsColoredBackGround:= false;
  j:= 0;
  if ActivePage <> nil then
  begin
    if (DefiTabSheetTabColorTo <> clNone) or (DefiTabSheetTabColor <> clNone) {due to Reduce flickering change} then
    begin
      if TabSheet3D then
        j:= 1;

      IsColoredBackGround:= true;

      case TabPosition of
        tpLeft:
          begin
            FinalRgn := CreateRectRgn(1, MaxRight, 1, ActBottom);
            RgnLeft := CreateRectRgn(MaxRight+j, 1, MaxRight+4, Height-1);
            RgnTop := CreateRectRgn(MaxRight+j, 1, Width-1, MaxRight + 4);
            CombineRgn(FinalRgn,RgnLeft, RgnTop,RGN_OR);
            RgnRight := CreateRectRgn(Width-4, 1, Width-1, Height-1);
            CombineRgn(FinalRgn,FinalRgn, RgnRight,RGN_OR);
            RgnBottom := CreateRectRgn(MaxRight+j, Height-4, Width-1, Height-1);
            CombineRgn(FinalRgn,FinalRgn, RgnBottom,RGN_OR);

            SelectClipRgn(Canvas.Handle,FinalRgn);
            if DefiTabSheetTabColorTo <> clNone then
            begin
              DrawGradient(Canvas, ActivePage.Color, DefiTabSheetTabColorTo, 16, clientRect, false);
              Canvas.Brush.Color:= ActivePage.Color;
              Canvas.Pen.Color:= ActivePage.Color;
              //Canvas.Rectangle(MaxRight+j, 1, MaxRight+4, Height -1);
              Canvas.Rectangle(MaxRight+j, 1, Width-1, 4);
            end
            else //if (ActivePage.Color <> Color) then
            begin
              Canvas.Brush.Color:= ActivePage.Color;
              Canvas.Pen.Color:= ActivePage.Color;
              Canvas.Rectangle(ClientRect.Left, ClientRect.Top, ClientRect.Right, ClientRect.Bottom);
            end;

            SelectClipRgn(Canvas.Handle,0);
            DeleteObject(RgnLeft);
            DeleteObject(RgnTop);
            DeleteObject(RgnRight);
            DeleteObject(RgnBottom);
            DeleteObject(FinalRgn);

          end;
        tpRight:
          begin
            FinalRgn := CreateRectRgn(1, MaxLeft, 1, ActBottom);
            RgnLeft := CreateRectRgn(1, 1, 4, Height-2);
            RgnTop := CreateRectRgn(1, 1, MaxLeft-j, 4);
            CombineRgn(FinalRgn,RgnLeft, RgnTop,RGN_OR);
            RgnRight := CreateRectRgn(MaxLeft-4, 1, MaxLeft-j, Height-2);
            CombineRgn(FinalRgn,FinalRgn, RgnRight,RGN_OR);
            RgnBottom := CreateRectRgn(1, Height-4, MaxLeft-j, Height-1);
            CombineRgn(FinalRgn,FinalRgn, RgnBottom,RGN_OR);

            SelectClipRgn(Canvas.Handle,FinalRgn);
            if DefiTabSheetTabColorTo <> clNone then
            begin
              DrawGradient(Canvas, ActivePage.Color, DefiTabSheetTabColorTo, 16, clientRect, false);
              Canvas.Brush.Color:= ActivePage.Color;
              Canvas.Pen.Color:= ActivePage.Color;
              Canvas.Rectangle(1, 1, MaxLeft-j, 4);
            end
            else //if (ActivePage.Color <> Color) then
            begin
              Canvas.Brush.Color:= ActivePage.Color;
              Canvas.Pen.Color:= ActivePage.Color;
              Canvas.Rectangle(ClientRect.Left, ClientRect.Top, ClientRect.Right, ClientRect.Bottom);
            end;

            SelectClipRgn(Canvas.Handle,0);
            DeleteObject(RgnLeft);
            DeleteObject(RgnTop);
            DeleteObject(RgnRight);
            DeleteObject(RgnBottom);
            DeleteObject(FinalRgn);
          end;
        tpTop:
          begin
            FinalRgn := CreateRectRgn(1, ActBottom, 1, ActBottom);
            RgnLeft := CreateRectRgn(1, ActBottom+j, 4, Height-1);
            RgnTop := CreateRectRgn(1, ActBottom+j, Width-1, ActBottom + 4);
            CombineRgn(FinalRgn,RgnLeft, RgnTop,RGN_OR);
            RgnRight := CreateRectRgn(Width-4, ActBottom+j, Width-1, Height-1);
            CombineRgn(FinalRgn,FinalRgn, RgnRight,RGN_OR);
            RgnBottom := CreateRectRgn(1, Height-4, Width-1, Height-1);
            CombineRgn(FinalRgn,FinalRgn, RgnBottom,RGN_OR);

            SelectClipRgn(Canvas.Handle,FinalRgn);
            if DefiTabSheetTabColorTo <> clNone then
            begin
              DrawGradient(Canvas, ActivePage.Color, DefiTabSheetTabColorTo, 16, clientRect, false);
              Canvas.Brush.Color:= ActivePage.Color;
              Canvas.Pen.Color:= ActivePage.Color;
              Canvas.Rectangle(1, ActBottom+1, Width-1, ActBottom + 4);
            end
            else //if (ActivePage.Color <> Color) then
            begin
              Canvas.Brush.Color:= ActivePage.Color;
              Canvas.Pen.Color:= ActivePage.Color;
              Canvas.Rectangle(ClientRect.Left, ClientRect.Top, ClientRect.Right, ClientRect.Bottom);
            end;

            SelectClipRgn(Canvas.Handle,0);
            DeleteObject(RgnLeft);
            DeleteObject(RgnTop);
            DeleteObject(RgnRight);
            DeleteObject(RgnBottom);
            DeleteObject(FinalRgn);
          end;
        tpBottom:
          begin
            FinalRgn := CreateRectRgn(1, MaxTop, 1, MaxTop);
            RgnLeft := CreateRectRgn(1, MaxTop-j, 4, 1);
            RgnTop := CreateRectRgn(1, MaxTop-j, Width-1, MaxTop - 4);
            CombineRgn(FinalRgn,RgnLeft, RgnTop,RGN_OR);
            RgnRight := CreateRectRgn(Width-4, MaxTop-j, Width-1, 1);
            CombineRgn(FinalRgn,FinalRgn, RgnRight,RGN_OR);
            RgnBottom := CreateRectRgn(1, 1, Width-1, 4);
            CombineRgn(FinalRgn,FinalRgn, RgnBottom,RGN_OR);

            SelectClipRgn(Canvas.Handle,FinalRgn);
            if DefiTabSheetTabColorTo <> clNone then
            begin
              DrawGradient(Canvas, ActivePage.Color, DefiTabSheetTabColorTo, 16, clientRect, false);
              Canvas.Brush.Color:= ActivePage.Color;
              Canvas.Pen.Color:= ActivePage.Color;
              Canvas.Rectangle(1, 1, Width-1, 4);
            end
            else //if (ActivePage.Color <> Color) then
            begin
              Canvas.Brush.Color:= ActivePage.Color;
              Canvas.Pen.Color:= ActivePage.Color;
              Canvas.Rectangle(ClientRect.Left, ClientRect.Top, ClientRect.Right, ClientRect.Bottom);
            end;

            SelectClipRgn(Canvas.Handle,0);
            DeleteObject(RgnLeft);
            DeleteObject(RgnTop);
            DeleteObject(RgnRight);
            DeleteObject(RgnBottom);
            DeleteObject(FinalRgn);
          end;
      end;
    end;
  end;


  if not FTabSheet3D  then
  begin
    case TabPosition of
      tpLeft:
        begin
          Canvas.Pen.Color := tc;
             // |
          Canvas.MoveTo(MaxRight, 0);
          Canvas.LineTo(MaxRight, Height);
             // _
          Canvas.MoveTo(MaxRight, Height - 2);
          Canvas.LineTo(Width - 2, Height - 2);
             //  |
          Canvas.MoveTo(Width - 2, Height);
          Canvas.LineTo(Width - 2, 0);
             // -
          Canvas.MoveTo(Width - 2, 0);
          Canvas.LineTo(MaxRight, 0);

          //erase 3d
          Canvas.Pen.Color := Color;
          Canvas.MoveTo(MaxRight, Height - 1);
          Canvas.LineTo(Width - 1, Height - 1);
          Canvas.MoveTo(Width - 1, Height - 1);
          Canvas.LineTo(Width - 1, -1);
        end;
      tpRight:
        begin
           Canvas.Pen.Color := tc;
             // |
          Canvas.MoveTo(0, 0);
          Canvas.LineTo(0, Height);
             // _
          Canvas.MoveTo(0, Height - 2);
          Canvas.LineTo(MaxLeft, Height - 2);
             //  |
          Canvas.MoveTo(MaxLeft - 1, Height);
          Canvas.LineTo(MaxLeft - 1, 0);
             // -
          Canvas.MoveTo(MaxLeft - 1, 0);
          Canvas.LineTo(0, 0);

          Canvas.Pen.Color := Color;
          Canvas.MoveTo(0, Height - 1);
          Canvas.LineTo(MaxLeft, Height - 1);
          if not IsColoredBackGround then
          begin
            Canvas.MoveTo(MaxLeft - 2, Height - 3);
            Canvas.LineTo(MaxLeft - 2, 0);
          end;
        end;
      tpTop:
        begin
          Canvas.Pen.Color := tc;
             // |
          Canvas.MoveTo(0, ActBottom);
          Canvas.LineTo(0, Height);
             // _
          Canvas.MoveTo(0, Height - 1);
          Canvas.LineTo(Width, Height - 1);
             //  |
          Canvas.MoveTo(Width - 1, Height - 2);
          Canvas.LineTo(Width - 1, ActBottom);
             // -
          Canvas.MoveTo(0, ActBottom);
          Canvas.LineTo(Width, ActBottom);

          if not IsColoredBackGround then
          begin
            Canvas.Pen.Color := Color;
            Canvas.MoveTo(1, Height - 2);
            Canvas.LineTo(Width - 1, Height - 2);
            Canvas.MoveTo(Width - 2, Height - 2);
            Canvas.LineTo(Width - 2, ActBottom);
          end;

        end;
      tpBottom:
        begin
           Canvas.Pen.Color := tc;
             // |
          Canvas.MoveTo(0, 0);
          Canvas.LineTo(0, MaxTop);
             // _
          Canvas.MoveTo(0, MaxTop - 1);
          Canvas.LineTo(Width, MaxTop - 1);
             //  |
          Canvas.MoveTo(Width - 1, MaxTop - 1);
          Canvas.LineTo(Width - 1, 0);
             // -
          Canvas.MoveTo(0, 0);
          Canvas.LineTo(Width, 0);

          if not IsColoredBackGround then
          begin
            Canvas.Pen.Color := Color;
            Canvas.MoveTo(1, MaxTop - 2);
            Canvas.LineTo(Width - 1, MaxTop - 2);
            Canvas.MoveTo(Width - 2, MaxTop - 2);
            Canvas.LineTo(Width - 2, 0);
          end;
        end;
    end;


  if FRoundEdges and FTabDrawOutLine then
    begin
      case TabPosition of
        tpLeft:
          begin
            Canvas.Pen.Color := Color;
               // _|
            Canvas.MoveTo(Width - 2 - 3, Height - 2);
            Canvas.LineTo(Width - 2, Height - 2);
            Canvas.LineTo(Width - 2, Height - 2 - 4);
               // -|
            Canvas.MoveTo(Width - 2, 0 + 3);
            Canvas.LineTo(Width - 2, 0);
            Canvas.LineTo(Width - 2 - 4, 0);

            Canvas.Pen.Color := tc;
               // _)
            Canvas.MoveTo(Width - 2 - 3, Height - 3);
            Canvas.LineTo(Width - 3, Height - 3);
            Canvas.MoveTo(Width - 3, Height - 4);
            Canvas.LineTo(Width - 3, Height - 6);
               // -)
            Canvas.MoveTo(Width - 3, 0 + 3);
            Canvas.LineTo(Width - 3, 0 + 1);
            Canvas.MoveTo(Width - 4, 0 + 1);
            Canvas.LineTo(Width - 6, 0 + 1);
          end;
        tpRight:
          begin
            Canvas.Pen.Color := Color;
               // |-
            Canvas.MoveTo(0, 0 + 3);
            Canvas.LineTo(0, 0);
            Canvas.LineTo(5, 0);
               // |_
            Canvas.MoveTo(0, Height - 2 - 3);
            Canvas.LineTo(0, Height - 2);
            Canvas.LineTo(4, Height - 2);

            Canvas.Pen.Color := tc;
               // (-
            Canvas.MoveTo(1, 0 + 3);
            Canvas.LineTo(1, 0 + 1);
            Canvas.MoveTo(2, 0 + 1);
            Canvas.LineTo(5, 0);
               // (_
            Canvas.MoveTo(1, Height - 2 - 3);
            Canvas.LineTo(1, Height - 3);
            Canvas.MoveTo(2, Height - 3);
            Canvas.LineTo(4, Height - 3);
          end;
        tpTop:
          begin
            Canvas.Pen.Color := Color;
                // |_
            Canvas.MoveTo(0, Height - 1 - 3);
            Canvas.LineTo(0, Height - 1);
            Canvas.LineTo(4, Height - 1);
               // _|
            Canvas.MoveTo(Width - 1 - 3, Height - 1);
            Canvas.LineTo(Width - 1, Height - 1);
            Canvas.LineTo(Width - 1, Height - 1 - 4);
               // -|

            Canvas.Pen.Color := tc;
               // (_
            Canvas.MoveTo(1, Height - 1 - 3);
            Canvas.LineTo(1, Height - 2);
            Canvas.MoveTo(2, Height - 2);
            Canvas.LineTo(4, Height - 2);
               // _)
            Canvas.MoveTo(Width - 1 - 3, Height - 2);
            Canvas.LineTo(Width - 2, Height - 2);
            Canvas.MoveTo(Width - 2, Height - 3);
            Canvas.LineTo(Width - 2, Height - 5);
          end;
        tpBottom:
          begin
            Canvas.Pen.Color := Color;
               // |-
            Canvas.MoveTo(0, 0 + 3);
            Canvas.LineTo(0, 0);
            Canvas.LineTo(5, 0);
               // -|
            Canvas.MoveTo(Width - 1, 0 + 3);
            Canvas.LineTo(Width - 1, 0);
            Canvas.LineTo(Width - 1 - 4, 0);

            Canvas.Pen.Color := tc; 
               // (-
            Canvas.MoveTo(1, 0 + 3);
            Canvas.LineTo(1, 0 + 1);
            Canvas.MoveTo(2, 0 + 1);
            Canvas.LineTo(5, 0);
               // -)
            Canvas.MoveTo(Width - 2, 0 + 3);
            Canvas.LineTo(Width - 2, 0 + 1);
            Canvas.MoveTo(Width - 3, 0 + 1);
            Canvas.LineTo(Width - 5, 0 + 1);
          end;
      end;
    end;

  if not FTabSplitLine and (PageCount > 0) then
    begin
      SplitLineDifference := 1;

      if IsColoredBackGround then
      begin
        if TabPosition = tpBottom then
        begin
          if DefiTabSheetTabColorTo = clNone then
            Canvas.Pen.Color := ActivePage.Color
          else
            Canvas.Pen.Color := DefiTabSheetTabColorTo;
        end
        else
          Canvas.Pen.Color := ActivePage.Color;
      end
      else
        Canvas.Pen.Color := Color;

      case TabPosition of
        tpLeft:
          begin
            Canvas.MoveTo(MaxRight, 2);
            if TabStyle = tsClassic then
            begin
              if not FTabBackGround.Empty then
                Canvas.LineTo(MaxRight, ActBottom + FTabOverlap + 2)
              else
                Canvas.LineTo(MaxRight, ActBottom + FTabOverlap);
            end
            else
              Canvas.LineTo(MaxRight, ActBottom + FTabOverlap);
          end;
        tpRight:
          begin
            Canvas.MoveTo(MaxLeft - 1, ActBottom - 1 + FTabOverlap);
            Canvas.LineTo(MaxLeft - 1, 2);
          end;
        tpTop:
          begin
            Canvas.MoveTo(2, ActBottom);
            Canvas.LineTo(MaxRight + FTabOverlap, ActBottom);
          end;
        tpBottom:
          begin
            Canvas.MoveTo(2, MaxTop - 1);
            Canvas.LineTo(MaxRight + FTabOverlap, MaxTop - 1);
          end;
      end;
    end;
  end;


  ActiveTabIndex := -1;
  ActiveTabX := -10;
  ActiveTabY := -10;


 tbmp := TBitmap.Create;

 for i := PageCount - 1 downto 0 do
  begin
    if TplTabSheet(Pages[i]).TabVisible then
    begin
      TabIndex := i;
      OrignalRect := TabRectEx(i);

      Rect := OrignalRect;
      Active := (i = GetActivePageIndex());

      TextX := 0;
      TextY := 0;

      if not FTabBackGround.Empty then
      begin
        tbmp.Width := FTabBackGround.Width;
        tbmp.Height := FTabBackGround.Height;
      end;

      HorizontalGradient := true;
      //df := 0;
      if Active and (DefiPageControlActiveColor <> clNone) then
      begin
       // df := 3;
        clr := DefiPageControlActiveColor;
        clrto := DefiPageControlActiveColorTo;
        if DefiTabSheetGradientDir = gdVertical then HorizontalGradient := false;
      end
      else
      begin
        if (FHoverTab = TabIndex) and ((DefiTabSheetHoverColorTo <> clNone) or (DefiTabSheetHoverColor <> clNone)) then
        begin
          clr := DefiTabSheetHoverColor;
          clrto := DefiTabSheetHoverColorTo;
          if DefiTabSheetHoverGradientDir = gdVertical then HorizontalGradient := false;
        end
        else
        begin
          clr := DefiTabSheetTabColor;
          clrto := DefiTabSheetTabColorTo; 
          if DefiTabSheetGradientDir = gdVertical then HorizontalGradient := false;
        end;
      end;

      with Canvas do
      begin
        Brush.Color := clr;
        if TabPosition = tpLeft then
        begin
          if TplTabSheet(FPages[TabIndex]).ShowClose then TextY := Rect.Bottom - 4 - CloseButtonHeight
          else TextY := Rect.Bottom - 4;

          if Active then TextX := Rect.Left + {3} FLowerActive + 1
          else TextX := Rect.Left + 1;

          if not FTabBackGround.Empty then
          begin
            TextX := TextX - 2;
            TextY := TextY - 3;
            if Active and not FTabBackGroundActive.Empty then
            begin
              tbmp.Canvas.Draw(0, 0, FTabBackGroundActive);
              ActiveTabIndex := tabIndex;
              OverLapActiveRect := Rect;
            end
            else
            begin
              tbmp.Canvas.Draw(0, 0, FTabBackGround);
              BitmapStretch(tbmp, Canvas, Rect.Left { - 2}, Rect.Top, Rect.Bottom - Rect.Top + 3 + FTabOverlap);
            end
          end
          else
          begin
            Rect.Left := Rect.Left - 1;
            if clrto = clNone then
              FillRect(Rect)
            else
            begin
              Rect.Right := Rect.Right - 1;
              if not HorizontalGradient then Rect.Bottom := Rect.Bottom - 1;
              DrawGradient(Canvas, clr, clrto, 16, Rect, HorizontalGradient);
              if not HorizontalGradient then Rect.Bottom := Rect.Bottom + 1;
              Rect.Right := Rect.Right + 1;
            end;
            Brush.Style := bsClear;

            if TabStyle = tsClassic then
            begin
              Pen.Color := FTabBorderColor;
              if Active then
              begin
                MoveTo(Rect.Right - 1, Rect.Top);
                LineTo(Rect.Left, Rect.Top);
                LineTo(Rect.Left, Rect.Bottom);
                LineTo(Rect.Right, Rect.Bottom);
              end
              else
              begin
                if SplitLineDifference > 0 then
                  if Rect.Right < MaxRight then
                    SplitLineDifference := 0;

                if Rect.Bottom > Height - 5 then Rectangle(Rect.Left, Rect.Top, Rect.Right + SplitLineDifference, Rect.Bottom)
                else Rectangle(Rect.Left, Rect.Top, Rect.Right + SplitLineDifference, Rect.Bottom + 1);

                if not FTabSheet3D and not FTabSplitLine then
                  SplitLineDifference := 1;
              end;
            end
            else if TabStyle = tsDelphi then
            begin
              if Active then
              begin
                Pen.Color := FTabBorderColor; //clBlack;
                MoveTo(Rect.Right, Rect.Top);
                LineTo(Rect.Left, Rect.Top);
                MoveTo(Rect.Left, Rect.Bottom);
                LineTo(Rect.Right, Rect.Bottom);
              end
              else
              begin
                isLast := Rect.Bottom > ActBottom - 3;
                isNextSelected := FindNextPage(Pages[i], true, true).TabIndex = GetActivePageIndex; //Rect.Right > TabRectEx(GetActivePageIndex).Left-4 ;
                Pen.Color := FTabBorderColor; 
                MoveTo(Rect.Right, Rect.Top);
                LineTo(Rect.Right, Rect.Bottom);

                if not isNextSelected or isLast then
                begin
                  Pen.Color := FTabBorderColor;
                  MoveTo(Rect.Left, Rect.Bottom - 2);
                  LineTo(Rect.Right, Rect.Bottom - 2);
                end;
              end;
            end
            else // TabStyle = tsDotNet
            begin
              if Active then
              begin
                Pen.Color := FTabBorderColor; //clBlack;
                MoveTo(Rect.Right, Rect.Top);
                LineTo(Rect.Left, Rect.Top);
                LineTo(Rect.Left, Rect.Bottom);
                LineTo(Rect.Right, Rect.Bottom);
              end
              else
              begin
                isLast := Rect.Bottom > ActBottom - 3;
                isNextSelected := FindNextPage(Pages[i], true, true).TabIndex = GetActivePageIndex; //Rect.Right > TabRectEx(GetActivePageIndex).Left-4 ;
                Pen.Color := FTabBorderColor; //clBlack;
                MoveTo(Rect.Right, Rect.Top);
                LineTo(Rect.Right, Rect.Bottom);

                if not isNextSelected or isLast then
                begin
                  Pen.Color := DefiPageControlActiveColor;
                  MoveTo(Rect.Left + 2, Rect.Bottom - 2);
                  LineTo(Rect.Right - 2, Rect.Bottom - 2);
                  MoveTo(Rect.Left + 2, Rect.Bottom - 3);
                  LineTo(Rect.Right - 2, Rect.Bottom - 3);
                end;
              end;
            end; // tsDotNet end
          end;
        end
        else if TabPosition = tpRight then
        begin
          if TplTabSheet(FPages[TabIndex]).ShowClose then TextY := Rect.Top + 4 + CloseButtonHeight
          else TextY := Rect.Top + 4;

          if Active then TextX := Rect.Right - {3} FLowerActive - 1
          else TextX := Rect.Right - 1;

          if not FTabBackGround.Empty then
          begin
            TextX := TextX - TabHeight div 3;

            TextY := TextY - 3;
            if Active and not FTabBackGroundActive.Empty then
            begin
              tbmp.Canvas.Draw(0, 0, FTabBackGroundActive);
              ActiveTabIndex := tabIndex;
              OverLapActiveRect := Rect;
            end
            else
            begin
              tbmp.Canvas.Draw(0, 0, FTabBackGround);
              BitmapStretch(tbmp, Canvas, Rect.Left - 1, Rect.Top - 2, Rect.Bottom - Rect.Top + 3 + TabOverlap);
            end
          end
          else
          begin
            if ClrTo = clNone then
              FillRect(Rect)
            else
            begin
              if Active then
              begin
                DrawGradient(Canvas, clr, clrto, 16, Rect, HorizontalGradient);
              end
              else
              begin
                if not HorizontalGradient then Rect.Bottom := Rect.Bottom - 1;
                DrawGradient(Canvas, clr, clrto, 16, Rect, HorizontalGradient);
                if not HorizontalGradient then Rect.Bottom := Rect.Bottom + 1;
              end;
            end;
            Rect.Right := Rect.Right + 1;
            Brush.Style := bsClear;

            if TabStyle = tsClassic then
            begin
              Pen.Color := FTabBorderColor;
              if Active then
              begin
                MoveTo(Rect.Left, Rect.Top);
                LineTo(Rect.Right - 1, Rect.Top);
                LineTo(Rect.Right - 1, Rect.Bottom);
                LineTo(Rect.Left - 1, Rect.Bottom);
              end
              else
              begin
                if SplitLineDifference > 0 then
                  if Rect.Left > MaxLeft then
                    SplitLineDifference := 0;

                if Rect.Bottom > Height - 5 then Rectangle(Rect.Left - SplitLineDifference, Rect.Top, Rect.Right, Rect.Bottom)
                else Rectangle(Rect.Left - SplitLineDifference, Rect.Top, Rect.Right, Rect.Bottom + 1);

                if not FTabSheet3D and not FTabSplitLine then
                  SplitLineDifference := 1;
              end;
            end
            else if TabStyle = tsDelphi then
            begin
              if Active then
              begin
                Pen.Color := FTabBorderColor; //clBlack;
                MoveTo(Rect.Left - 1, Rect.Top);
                LineTo(Rect.Right - 1, Rect.Top);
                MoveTo(Rect.Right - 1, Rect.Bottom);
                LineTo(Rect.Left - 1, Rect.Bottom);
              end
              else
              begin
                isLast := Rect.Bottom > ActBottom - 3;
                isNextSelected := FindNextPage(Pages[i], true, true).TabIndex = GetActivePageIndex; //Rect.Right > TabRectEx(GetActivePageIndex).Left-4 ;
                Pen.Color := FTabBorderColor; //clBlack;
                MoveTo(Rect.Left - 1, Rect.Top);
                LineTo(Rect.Left - 1, Rect.Bottom);

                if not isNextSelected or isLast then
                begin
                  Pen.Color := FTabBorderColor;
                  MoveTo(Rect.Left, Rect.Bottom - 2);
                  LineTo(Rect.Right, Rect.Bottom - 2);
                  //MoveTo(Rect.Left + 2, Rect.Bottom - 3);
                  //LineTo(Rect.Right - 2, Rect.Bottom - 3);
                end;
              end;
            end
            else // TabStyle = tsDotNet
            begin
              if Active then
              begin
                Pen.Color := FTabBorderColor; //clBlack;
                MoveTo(Rect.Left - 1, Rect.Top);
                LineTo(Rect.Right - 1, Rect.Top);
                LineTo(Rect.Right - 1, Rect.Bottom);
                LineTo(Rect.Left - 1, Rect.Bottom);
              end
              else
              begin
                isLast := Rect.Bottom > ActBottom - 3;
                isNextSelected := FindNextPage(Pages[i], true, true).TabIndex = GetActivePageIndex; //Rect.Right > TabRectEx(GetActivePageIndex).Left-4 ;
                Pen.Color := FTabBorderColor; //clBlack;
                MoveTo(Rect.Left - 1, Rect.Top);
                LineTo(Rect.Left - 1, Rect.Bottom);

                if not isNextSelected or isLast then
                begin
                  Pen.Color := DefiPageControlActiveColor;
                  MoveTo(Rect.Left + 2, Rect.Bottom - 2);
                  LineTo(Rect.Right - 2, Rect.Bottom - 2);
                  MoveTo(Rect.Left + 2, Rect.Bottom - 3);
                  LineTo(Rect.Right - 2, Rect.Bottom - 3);
                end;
              end;
            end; // tsDotNet end
          end;
        end
        else if TabPosition = tpTop then
        begin
          if TplTabSheet(FPages[TabIndex]).ShowClose then TextX := Rect.Left + 4 + CloseButtonWidth
          else TextX := Rect.Left + 4;

          if Active then TextY := Rect.Top + {df} FLowerActive + 1
          else TextY := Rect.Top + 1;

          if not FTabBackGround.Empty then
          begin
            if Active and not FTabBackGroundActive.Empty then
            begin
              tbmp.Canvas.Draw(0, 0, FTabBackGroundActive);
              ActiveTabIndex := tabIndex;
              OverLapActiveRect := Rect;
            end
            else
            begin
              tbmp.Canvas.Draw(0, 0, FTabBackGround);
              BitmapStretchInWidth(tbmp, Canvas, Rect.Left - 1, Rect.Top - 2, Rect.Right - Rect.Left + 2 + FTabOverlap);
            end
          end
          else
          begin
            Rect.Top := Rect.Top - 1;
            if ClrTo = clNone then
              FillRect(Rect)
            else
            begin
              Rect.Bottom := Rect.Bottom - 1;
              if HorizontalGradient then Rect.Right := Rect.Right - 1;
              DrawGradient(Canvas, clr, clrto, 16, Rect, HorizontalGradient);
              if HorizontalGradient then Rect.Right := Rect.Right + 1;
              Rect.Bottom := Rect.Bottom + 1;
            end;

            Brush.Style := bsClear;

            if TabStyle = tsClassic then
            begin
              Pen.Color := FTabBorderColor;

              if Active then
              begin
                MoveTo(Rect.Left, Rect.Bottom - 1);
                LineTo(Rect.Left, Rect.Top);
                LineTo(Rect.Right, Rect.Top);
                LineTo(Rect.Right, Rect.Bottom);
              end
              else
              begin
                if SplitLineDifference > 0 then
                  if Rect.Bottom < ActBottom then
                    SplitLineDifference := 0;

                if Rect.Right > Width - 5 then Rectangle(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom + SplitLineDifference)
                else Rectangle(Rect.Left, Rect.Top, Rect.Right + 1, Rect.Bottom + SplitLineDifference);

                if not FTabSheet3D and not FTabSplitLine then
                  SplitLineDifference := 1;
              end;
            end
            else if TabStyle = tsDelphi then
            begin
              if Active then
              begin
                Pen.Color := FTabBorderColor; //clBlack;
                MoveTo(Rect.Left, Rect.Bottom);
                LineTo(Rect.Left, Rect.Top);
                MoveTo(Rect.Right, Rect.Top);
                LineTo(Rect.Right, Rect.Bottom);
              end
              else
              begin
                isLast := Rect.Right > MaxRight - 3;
                isNextSelected := FindNextPage(Pages[i], true, true).TabIndex = GetActivePageIndex; //Rect.Right > TabRectEx(GetActivePageIndex).Left-4 ;
                Pen.Color := FTabBorderColor; //clBlack;
                MoveTo(Rect.Left, Rect.Bottom);
                LineTo(Rect.Right, Rect.Bottom);

                if not isNextSelected or isLast then
                begin
                  Pen.Color := FTabBorderColor;
                  MoveTo(Rect.Right - 2, Rect.Bottom);
                  LineTo(Rect.Right - 2, Rect.Top);
                  //MoveTo(Rect.Right - 3, Rect.Bottom - 3);
                  //LineTo(Rect.Right - 3, Rect.Top + 2);
                end;
              end;
            end
            else // TabStyle = tsDotNet
            begin
              if Active then
              begin
                Pen.Color := FTabBorderColor; //clBlack;
                MoveTo(Rect.Left, Rect.Bottom);
                LineTo(Rect.Left, Rect.Top);
                LineTo(Rect.Right, Rect.Top);
                LineTo(Rect.Right, Rect.Bottom);
              end
              else
              begin
                isLast := Rect.Right > MaxRight - 3;
                isNextSelected := FindNextPage(Pages[i], true, true).TabIndex = GetActivePageIndex; //Rect.Right > TabRectEx(GetActivePageIndex).Left-4 ;
                Pen.Color := FTabBorderColor; //clBlack;
                MoveTo(Rect.Left, Rect.Bottom);
                LineTo(Rect.Right, Rect.Bottom);

                if not isNextSelected or isLast then
                begin
                  Pen.Color := DefiPageControlActiveColor;
                  MoveTo(Rect.Right - 2, Rect.Bottom - 3);
                  LineTo(Rect.Right - 2, Rect.Top + 2);
                  MoveTo(Rect.Right - 3, Rect.Bottom - 3);
                  LineTo(Rect.Right - 3, Rect.Top + 2);
                end;
              end;
            end; // tsDotNet end
          end;
        end
        else if TabPosition = tpBottom then
        begin
          Rect.Bottom := Rect.Bottom + 1;

          if TplTabSheet(FPages[TabIndex]).ShowClose then TextX := Rect.Left + 4 + CloseButtonWidth
          else TextX := Rect.Left + 4;

          if Active then TextY := Rect.Top + {3} FLowerActive + 1
          else TextY := Rect.Top;

          if not FTabBackGround.Empty then
          begin
            TextY := TextY + 3;

            if Active and not FTabBackGroundActive.Empty then
            begin
              tbmp.Canvas.Draw(0, 0, FTabBackGroundActive);
              ActiveTabIndex := tabIndex;
              OverLapActiveRect := Rect;
            end
            else
            begin
              tbmp.Canvas.Draw(0, 0, FTabBackGround);
              BitmapStretchInWidth(tbmp, Canvas, Rect.Left - 2, Rect.Top - 1, Rect.Right - Rect.Left + 2 + FTabOverlap);
            end
          end
          else
          begin
            if ClrTo = clNone then
              FillRect(Rect)
            else
            begin
              if HorizontalGradient then Rect.Right := Rect.Right - 1 else Rect.Bottom := Rect.Bottom - 1;
              DrawGradient(Canvas, clr, clrto, 16, Rect, HorizontalGradient);
              if HorizontalGradient then Rect.Right := Rect.Right + 1 else Rect.Bottom := Rect.Bottom + 1;
            end;
            Brush.Style := bsClear;

            if TabStyle = tsClassic then
            begin
              Pen.Color := FTabBorderColor;
              if Active then
              begin
                MoveTo(Rect.Left, Rect.Top);
                LineTo(Rect.Left, Rect.Bottom);
                LineTo(Rect.Right, Rect.Bottom);
                LineTo(Rect.Right, Rect.Top - 1);
              end
              else
              begin
                if SplitLineDifference > 0 then
                  if Rect.Top > MaxTop then
                    SplitLineDifference := 0;

                if Rect.Right > Width - 5 then Rectangle(Rect.Left, Rect.Top - SplitLineDifference, Rect.Right, Rect.Bottom)
                else Rectangle(Rect.Left, Rect.Top - SplitLineDifference, Rect.Right + 1, Rect.Bottom);
                Texty := TextY + 2;

                if not FTabSheet3D and not FTabSplitLine then
                  SplitLineDifference := 1;
              end;
            end
            else if TabStyle = tsDelphi then
            begin
              if Active then
              begin
                Pen.Color := FTabBorderColor; //clBlack;
                MoveTo(Rect.Left, Rect.Top - 1);
                LineTo(Rect.Left, Rect.Bottom);
                MoveTo(Rect.Right, Rect.Bottom-1);
                LineTo(Rect.Right, Rect.Top - 1);
              end
              else
              begin
                isLast := Rect.Right > MaxRight - 3;
                isNextSelected := FindNextPage(Pages[i], true, true).TabIndex = GetActivePageIndex; //Rect.Right > TabRectEx(GetActivePageIndex).Left-4 ;
                Pen.Color := FTabBorderColor; //clBlack;
                MoveTo(Rect.Left, Rect.Top - 1);
                LineTo(Rect.Right, Rect.Top - 1);

                if not isNextSelected or isLast then
                begin
                  Pen.Color := FTabBorderColor;
                  MoveTo(Rect.Right - 2, Rect.Bottom-1);
                  LineTo(Rect.Right - 2, Rect.Top-1);
                  //MoveTo(Rect.Right - 3, Rect.Bottom - 3);
                  //LineTo(Rect.Right - 3, Rect.Top + 2);
                end;
              end;
            end
            else // TabStyle = tsDotNet
            begin
              if Active then
              begin
                Pen.Color := FTabBorderColor; //clBlack;
                MoveTo(Rect.Left, Rect.Top - 1);
                LineTo(Rect.Left, Rect.Bottom);
                LineTo(Rect.Right, Rect.Bottom);
                LineTo(Rect.Right, Rect.Top - 1);
              end
              else
              begin
                isLast := Rect.Right > MaxRight - 3;
                isNextSelected := FindNextPage(Pages[i], true, true).TabIndex = GetActivePageIndex; //Rect.Right > TabRectEx(GetActivePageIndex).Left-4 ;
                Pen.Color := FTabBorderColor; //clBlack;
                MoveTo(Rect.Left, Rect.Top - 1);
                LineTo(Rect.Right, Rect.Top - 1);

                if not isNextSelected or isLast then
                begin
                  Pen.Color := DefiPageControlActiveColor;
                  MoveTo(Rect.Right - 2, Rect.Bottom - 3);
                  LineTo(Rect.Right - 2, Rect.Top + 2);
                  MoveTo(Rect.Right - 3, Rect.Bottom - 3);
                  LineTo(Rect.Right - 3, Rect.Top + 2);
                end;
              end;
            end; // tsDotNet end
          end;
        end;

        //if TplTabSheet(FPages[TabIndex]).ShowClose then DrawCloseButton(OrignalRect, Active);

        // Make sure to use a truetype font!
        // Font.Name := 'Tahoma';

        tf := TFont.Create;
        try
          if (TabPosition = tpLeft) or (TabPosition = tpRight) then
          begin

            FillChar(lf, SizeOf(lf), 0);

            if Active then
              tf.Assign(FActiveFont)
            else
              tf.Assign(self.Font);

            GetObject(tf.Handle, SizeOf(Lf), @Lf);   

            if TabPosition = tpLeft then lf.lfEscapement := -2700
            else lf.lfEscapement := -900;
            lf.lfOrientation := 30;

            tf.Handle := CreateFontIndirect(Lf);
            Canvas.Font.Assign(tf);
          end;
        finally
          tf.Free;
        end;

        if TabPosition = tpLeft then
        begin
          TextX := TextX + FTabMargin.LeftMargin;
          TextY := TextY - FTabMargin.TopMargin;
          if not FTabBackGroundActive.Empty then TextY := TextY + TabOverlap + 3;
        end
        else
        begin
          TextX := TextX + FTabMargin.LeftMargin;
          TextY := TextY + FTabMargin.TopMargin;
        end;

        if ((ActiveTabIndex >= 0) and (ActiveTabX = -10) and (ActiveTabY = -10)) then
        begin
          ActiveTabX := TextX;
          ActiveTabY := TextY;
        end
        else
        begin
          if TplTabSheet(FPages[TabIndex]).ShowClose then DrawCloseButton(OrignalRect, Active);

          if (Images <> nil) and (TplTabSheet(FPages[TabIndex]).ImageIndex >= 0) then
          begin
            if (TabPosition = tpLeft) then Images.Draw(Canvas, TextX, TextY - 14, TplTabSheet(FPages[TabIndex]).ImageIndex)
            else if (TabPosition = tpRight) then Images.Draw(Canvas, TextX - 14, TextY, TplTabSheet(FPages[TabIndex]).ImageIndex)
            else Images.Draw(Canvas, TextX - 2, TextY, TplTabSheet(FPages[TabIndex]).ImageIndex);
            if (TabPosition = tpLeft) then TextY := TextY - Images.Height;
            if (TabPosition = tpRight) then TextY := TextY + Images.Height;
            if (TabPosition = tpTop) or (TabPosition = tpBottom) then TextX := TextX + Images.width;
          end;


          //------- Displaying text
          if Active {and ((TabPosition = tpTop) or (TabPosition = tpBottom))} then
          begin
            if ((TabPosition = tpTop) or (TabPosition = tpBottom)) then
            begin
             Canvas.Font.Assign(FActiveFont);
             Canvas.Font.Color := DefiPageControlActiveTextColor;
            end else
            begin
              // do nothing Already Assigned while making orientation of the text
            end;
          end
          else
          begin
            if ((TabPosition = tpTop) or (TabPosition = tpBottom)) then Canvas.Font.Assign(self.Font);
            Canvas.Font.Color := TplTabSheet(FPages[TabIndex]).TextColor;
          end;

          if not TplTabSheet(FPages[TabIndex]).TabEnable then Canvas.Font.Color := clGray;

          if not TplTabSheet(FPages[TabIndex]).Enabled then Canvas.Font.Color := clGrayText;

          Brush.Style := bsClear;
          if (TabPosition = tpTop) or (TabPosition = tpBottom) then
          begin
            TextR := OrignalRect;
            TextR.Left := TextX;
            TextR.Top := TextY;

            DrawText(Canvas.Handle, Pchar(TplTabSheet(FPages[TabIndex]).Caption), Length(TplTabSheet(FPages[TabIndex]).Caption), TextR, DT_LEFT or DT_SINGLELINE {or DT_END_ELLIPSIS } or DT_NOCLIP);
 

          end
          else
          begin
            TextOut(TextX, TextY, TplTabSheet(FPages[TabIndex]).Caption);
          end;

          if Active then
            Canvas.Font.Assign(self.Font);

          DrawFocusRectangle(Canvas, Rect, Active, 0);
        end;
      end;
    end;
  end;

  if ActiveTabIndex >= 0 then
  begin
    tbmp.Canvas.Draw(0, 0, FTabBackGroundActive);
    Rect := OverLapActiveRect;

    if FLowerActive > 0 then
    begin
      case TabPosition of
        tpLeft: BitmapStretch(tbmp, Canvas, Rect.Left + 1, Rect.Top, Rect.Bottom - Rect.Top + 3 + FTabOverlap);
        tpRight: BitmapStretch(tbmp, Canvas, Rect.Left - 3, Rect.Top - 2, Rect.Bottom - Rect.Top + 3 + FTabOverlap);
        tpTop: BitmapStretchInWidth(tbmp, Canvas, Rect.Left - 2, Rect.Top - 1, Rect.Right - Rect.Left + 2 + FTabOverlap);
        tpBottom: BitmapStretchInWidth(tbmp, Canvas, Rect.Left - 2, Rect.Top - 3, Rect.Right - Rect.Left + 2 + FTabOverlap);
      end;
    end
    else
    begin
      case TabPosition of
        tpLeft: BitmapStretch(tbmp, Canvas, Rect.Left { +1}, Rect.Top, Rect.Bottom - Rect.Top + 3 + FTabOverlap);
        tpRight: BitmapStretch(tbmp, Canvas, Rect.Left - 1 {3}, Rect.Top - 2, Rect.Bottom - Rect.Top + 3 + FTabOverlap);
        tpTop: BitmapStretchInWidth(tbmp, Canvas, Rect.Left - 2, Rect.Top - 2 {1}, Rect.Right - Rect.Left + 2 + FTabOverlap);
        tpBottom: BitmapStretchInWidth(tbmp, Canvas, Rect.Left - 2, Rect.Top - 1 {3}, Rect.Right - Rect.Left + 2 + FTabOverlap);
      end;
    end;

    if TplTabSheet(FPages[ActiveTabIndex]).ShowClose then
      DrawCloseButton(OverLapActiveRect, true {Active});

    if (Images <> nil) and (TplTabSheet(FPages[ActiveTabIndex]).ImageIndex >= 0) then
    begin
      if (TabPosition = tpLeft) then Images.Draw(Canvas, ActiveTabX, ActiveTabY - 14, TplTabSheet(FPages[ActiveTabIndex]).ImageIndex)
      else if (TabPosition = tpRight) then Images.Draw(Canvas, ActiveTabX - 14, ActiveTabY, TplTabSheet(FPages[ActiveTabIndex]).ImageIndex)
      else Images.Draw(Canvas, ActiveTabX - 2, ActiveTabY, TplTabSheet(FPages[ActiveTabIndex]).ImageIndex);
      if (TabPosition = tpLeft) then ActiveTabY := ActiveTabY - Images.Height;
      if (TabPosition = tpRight) then ActiveTabY := ActiveTabY + Images.Height;
      if (TabPosition = tpTop) or (TabPosition = tpBottom) then ActiveTabX := ActiveTabX + Images.width;
    end;


    //------- Displaying text
    with Canvas do
    begin
      Canvas.Brush.Style := bsClear;

      if {Active and }(TabPosition = tpTop) or (TabPosition = tpBottom) then
      begin
        Canvas.Font.Assign(FActiveFont);
      end
      else
      begin


        tf := TFont.Create;
        try
          if (TabPosition = tpLeft) or (TabPosition = tpRight) then
          begin

            FillChar(lf, SizeOf(lf), 0);

            tf.Assign(FActiveFont);

            GetObject(tf.Handle, SizeOf(Lf), @Lf);



            if TabPosition = tpLeft then lf.lfEscapement := -2700
            else lf.lfEscapement := -900;
            lf.lfOrientation := 30;

            tf.Handle := CreateFontIndirect(Lf);
            Canvas.Font.Assign(tf);
          end;
        finally
          tf.Free;
        end;

      end;

      if not TplTabSheet(FPages[ActiveTabIndex]).Enabled then
        Canvas.Font.Color:= clGrayText;
        
      if (TabPosition = tpTop) or (TabPosition = tpBottom) then
      begin
        TextR := Rect;
        TextR.Left := ActiveTabX;
        if TabPosition = tpBottom then
          TextR.Top := ActiveTabY - 2
        else
          TextR.Top := ActiveTabY;

        DrawText(Handle, PChar(TplTabSheet(FPages[ActiveTabIndex]).Caption), Length(TplTabSheet(FPages[ActiveTabIndex]).Caption), TextR, DT_LEFT or DT_SINGLELINE {or DT_END_ELLIPSIS} or DT_NOCLIP);
 

      end
      else
      begin
        Canvas.TextOut(ActiveTabX, ActiveTabY, TplTabSheet(FPages[ActiveTabIndex]).Caption);
      end;

      Canvas.Font.Assign(Font);
    end;

    DrawFocusRectangle(Canvas, Rect, True, TabOverlap);

  end;

  if (FHoverTab >= 0) and (DefiTabSheetHoverBorder <> clNone) and (FHoverTab <> ActivePageIndex) then
  begin
    Rect := TabRectEx(FHoverTab);
    Canvas.Pen.Color := DefiTabSheetHoverBorder;
    with Canvas do
    begin
      case TabPosition of
        tpLeft:
          begin
            Rect.Left := Rect.Left - 1;
            Rect.Top := Rect.Top - 2;
            if Rect.Bottom > Height - 5 then Rect.Bottom := Rect.Bottom - 1;

            MoveTo(Rect.Right - 1, Rect.Top + 2);
            LineTo(Rect.Left, Rect.Top + 2);
            LineTo(Rect.Left, Rect.Bottom);
            LineTo(Rect.Right, Rect.Bottom);
          end;
        tpRight:
          begin
            Rect.Left := Rect.Left + 1;
            if Rect.Bottom > Height - 5 then Rect.Bottom := Rect.Bottom - 1;
            MoveTo(Rect.Left, Rect.Top);
            LineTo(Rect.Right, Rect.Top);
            LineTo(Rect.Right, Rect.Bottom);
            LineTo(Rect.Left - 1, Rect.Bottom);
          end;
        tpTop:
          begin
            Rect.Top := Rect.Top - 1;
            if Rect.Right > Width - 5 then Rect.Right := Rect.Right - 1;
            MoveTo(Rect.Left, Rect.Bottom - 2);
            LineTo(Rect.Left, Rect.Top);
            LineTo(Rect.Right, Rect.Top);
            LineTo(Rect.Right, Rect.Bottom - 1);
          end;
        tpBottom:
          begin
            if Rect.Right > Width - 5 then Rect.Right := Rect.Right - 1;
            MoveTo(Rect.Left, Rect.Top + 1);
            LineTo(Rect.Left, Rect.Bottom);
            LineTo(Rect.Right, Rect.Bottom);
            LineTo(Rect.Right, Rect.Top);
          end;
      end;
    end;
  end;

  tbmp.Free;

  if FTabDrawOutLine=false then
   begin

     Rect:=clientrect;
     Canvas.Pen.Color:=tc;

     Canvas.MoveTo(0, 0);
     Canvas.LineTo(0, Rect.Bottom);
     Canvas.MoveTo(1, 0);
     Canvas.LineTo(1, Rect.Bottom);
    

     Canvas.MoveTo(0, 0);
     Canvas.LineTo(rect.Right, 0);
     Canvas.MoveTo(0, 1);
     Canvas.LineTo(rect.Right, 1);

   end;
   
end;

procedure TplPageControl.OpenAllClosedTabs;
var
  i: integer;
begin
  for i := 0 to FClosedTabList.Count - 1 do
  begin
    TplTabSheet(FClosedTabList.Objects[i]).AdvPageControl := self;
  end;
end;

function TplPageControl.OpenClosedTab(TabName: string): Boolean;
var
  i: integer;
begin
  Result := false;
  i := FClosedTabList.IndexOf(TabName);
  if i >= 0 then
  begin
    TplTabSheet(FClosedTabList.Objects[i]).AdvPageControl := self;
    Result := true;
  end;
end;

procedure TplPageControl.CMDesignHitTest(var Message: TCMDesignHitTest);
var
  HitIndex: Integer;
 // HitTestInfo: TTCHitTestInfo;
begin
 { HitTestInfo.pt := SmallPointToPoint(Message.Pos);
 
  HitIndex := SendMessage(Handle, TCM_HITTEST, 0, PtrInt(@HitTestInfo));
 

  if (HitIndex >= 0) and (HitIndex <> TabIndex) then Message.Result := 1;   }
end;

procedure TplPageControl.CMDialogKey(var Message: TCMDialogKey);
begin {
  if (Focused or Windows.IsChild(Handle, Windows.GetFocus)) and
    (Message.CharCode = VK_TAB) and (GetKeyState(VK_CONTROL) < 0) then
  begin
    SelectNextPage(GetKeyState(VK_SHIFT) >= 0);
    Message.Result := 1;
  end else
    inherited; }
end;
                    {

procedure TplPageControl.CMDockClient(var Message: TCMDockClient);
var
  IsVisible: Boolean;
  DockCtl: TControl;
begin
  Message.Result := 0;
  FNewDockSheet := TplTabSheet.Create(Self);
  try
    try
      DockCtl := Message.DockSource.Control;
      if DockCtl is TCustomForm then
        FNewDockSheet.Caption := TCustomForm(DockCtl).Caption;
      FNewDockSheet.AdvPageControl := Self;
      DockCtl.Dock(Self, Message.DockSource.DockRect);
    except
      FNewDockSheet.Free;
      raise;
    end;
    IsVisible := DockCtl.Visible;
    FNewDockSheet.TabVisible := IsVisible;
    if IsVisible then ActivePage := FNewDockSheet;
    DockCtl.Align := alClient;
  finally
    FNewDockSheet := nil;
  end;
end;
 


procedure TplPageControl.CMDockNotification(var Message: TCMDockNotification);
var
  I: Integer;
  S: string;
  Page: TplTabSheet;
begin
  Page := GetPageFromDockClient(Message.Client);
  if Page <> nil then
    case Message.NotifyRec.ClientMsg of
      WM_SETTEXT:
        begin
 
          S := PChar(Message.NotifyRec.MsgLParam);
 

          //Search for first CR/LF and end string there
          for I := 1 to Length(S) do
            if S[I] in [#13, #10] then
            begin
              SetLength(S, I - 1);
              Break;
            end;
          Page.Caption := S;
        end;
      CM_VISIBLECHANGED:
        Page.TabVisible := Boolean(Message.NotifyRec.MsgWParam);
    end;
  inherited;
end;

 

procedure TplPageControl.CMUnDockClient(var Message: TCMUnDockClient);
var
  Page: TplTabSheet;
begin
  Message.Result := 0;
  Page := GetPageFromDockClient(Message.Client);
  if Page <> nil then
  begin
    FUndockingPage := Page;
    Message.Client.Align := alNone;
  end;
end;
 


function TplPageControl.GetDockClientFromMousePos(MousePos: TPoint): TControl;
var
  i, HitIndex: Integer;
  HitTestInfo: TTCHitTestInfo;
  Page: TplTabSheet;
begin
  Result := nil;
  if DockSite then
  begin
    HitTestInfo.pt := MousePos;

    HitIndex := SendMessage(Handle, TCM_HITTEST, 0, Longint(@HitTestInfo));


    if HitIndex >= 0 then
    begin
      Page := nil;
      for i := 0 to HitIndex do
        Page := FindNextPage(Page, True, True);
      if (Page <> nil) and (Page.ControlCount > 0) then
      begin
        Result := Page.Controls[0];
        if Result.HostDockSite <> Self then Result := nil;
      end;
    end;
  end;
end;  }

procedure TplPageControl.WMLButtonDown(var Message: TLMLButtonDown);
var
  DockCtl: TControl;
begin
  inherited;
 // DockCtl := GetDockClientFromMousePos(SmallPointToPoint(Message.Pos));
 // if (DockCtl <> nil) and (Style = tsTabs) then DockCtl.BeginDrag(False);
end;

procedure TplPageControl.WMLButtonDBLCLK(var Message: TLMLButtonDblClk);
var
  DockCtl: TControl;
begin
  inherited;
 // DockCtl := GetDockClientFromMousePos(SmallPointToPoint(Message.Pos));
 // if DockCtl <> nil then DockCtl.ManualDock(nil, nil, alNone);
end;

function TplPageControl.GetActivePageIndex: Integer;
begin
  if ActivePage <> nil then
    Result := ActivePage.GetPageIndex
  else
    Result := -1;
end;

procedure TplPageControl.SetActivePageIndex(const Value: Integer);
begin
  if (Value > -1) and (Value < PageCount) then
    ActivePage := Pages[Value]
  else
    ActivePage := nil;
end;

procedure TplPageControl.SetDefaultTextColor(const Value: TColor);
var
  i: integer;
begin
  for i := 0 to PageCount - 1 do
  begin
    if TplTabSheet(FPages[i]).TextColor = FDefaultTextColor then TplTabSheet(FPages[i]).TextColor := Value;
  end;
  FDefaultTextColor := Value;
  Invalidate;
end;

procedure TplPageControl.SetTabBorder3D(Value: Boolean);
var
  i: integer;
begin
  if FTabBorder3D <> Value then
  begin
    FTabBorder3D := Value;
    if Value then
    begin
      inherited Images := FImages;
    end
    else
    begin
      inherited Images := FDummyImages;
    end;
    OwnerDraw := not TabBorder3D;

    for i := PageCount - 1 downto 0 do
    begin
      UpdateTab(Pages[I]);
    end;

    if not FTabBorder3D then
      UpdateTabForActiveFont(FActivePage);

    Invalidate;
  end;
end;

procedure TplPageControl.SetTabBorderColor(const Value: TColor);
begin
  FTabBorderColor := Value;
  Invalidate;
end;

procedure TplPageControl.SetTabSheet3D(Value: Boolean);
begin
  if Value <> FTabSheet3D then
  begin
    FTabSheet3D := Value;
    Invalidate;
  end;
end;

procedure TplPageControl.SetTabSheetBorderColor(Value: TColor);
begin
  if Value <> FTabSheetBorderColor then
  begin
    FTabSheetBorderColor := Value;
    Invalidate;
  end;
end;

procedure TplPageControl.SetTabBackGroundColor(Value: TColor);
begin
  if Value <> FTabBackGroundColor then
  begin
    FTabBackGroundColor := Value;
    Invalidate;
  end;
end;

procedure TplPageControl.SetTabSplitLine(Value: Boolean);
begin
  if Value <> FTabSplitLine then
  begin
    if Value then
      if (TabStyle = tsDotNet) or (TabStyle = tsDelphi) then
        raise exception.Create('TabSplitLine must be false when TabStyle is tsDotNet/tsDelphi');

    FTabSplitLine := Value;
    Invalidate;
  end;
end;

procedure TplPageControl.SetRoundEdges(Value: Boolean);
begin
  if Value <> FRoundEdges then
  begin
    FRoundEdges := Value;
    Invalidate;
  end;
end;

procedure TplPageControl.SetTabStyle(Value: TTabStyle);
begin
  if Value <> FTabStyle then
  begin
    FTabStyle := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TplPageControl.SetLowerActive(Value: integer);
begin
  if Value <> FLowerActive then
  begin
    FLowerActive := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TplPageControl.SetTabBackGround(const Value: TBitmap);
begin
  FTabBackGround.Assign(Value);

  if (csDesigning in ComponentState) and Assigned(Value) then
  begin
    if not Value.Empty then
    begin
      if TabPosition in [tpTop, tpBottom] then
        TabHeight := Value.Height - 3
      else
        TabHeight := Value.Width - 3
    end;
  end;
  Invalidate;
end;

procedure TplPageControl.SetTabBackGroundActive(const Value: TBitmap);
begin
  FTabBackGroundActive.Assign(Value);
  Invalidate;
end;

procedure TplPageControl.SetTabMargins;
var
  ImgW, ImgH: integer;
begin
  if FImages <> nil then
  begin
    ImgW := FImages.Width;
    ImgH := FImages.Height;
  end
  else
  begin
    ImgW := 0;
    ImgH := 0;
  end;

  if TabPosition in [tpLeft, tpRight] then
  begin
    FDummyImages.Height := ImgH + TabMargin.RightMargin + TabMargin.TopMargin;
  end
  else // tpTop, tpBottom
  begin
    FDummyImages.Width := ImgW + TabMargin.RightMargin + TabMargin.LeftMargin;
  end;
end;

function TplPageControl.GetTabPosition: TTabPosition;
begin
  Result := inherited TabPosition;
end;

procedure TplPageControl.SetTabPosition(Value: TTabPosition);
var
  Img: TCustomImageList;
begin
  Img := nil;
  if FImages <> nil then
  begin
    Img := Fimages;
    Images := nil;
  end;
  inherited TabPosition := Value;
  if Img <> nil then Images := Img;
  SetTabMargins;
end;

procedure TplPageControl.SetTabDrawOutLine(Const Value:boolean);
 begin
   FTabDrawOutLine:=value;
   Invalidate;
 end;

procedure TplPageControl.SetImages(value: TCustomImageList);
begin
  if FImages <> nil then
  begin
    if TabPosition in [tpTop, tpBottom] then
      FDummyImages.Width := FDummyImages.Width - FImages.Width
    else
      FDummyImages.Height := FDummyImages.Height - FImages.Height;
  end;

  FImages := Value;

  if FImages <> nil then
  begin
    if TabPosition in [tpTop, tpBottom] then
      FDummyImages.Width := FDummyImages.Width + FImages.Width
    else // tpLeft, tpRight
      FDummyImages.Height := FDummyImages.Height + FImages.Height;
  end;

  if FTabBorder3D then
  begin
    inherited Images := FImages;
  end;
  Invalidate;
end;

procedure TplPageControl.SetTabMargin(Value: TTabMargin);
begin
  if Assigned(Value) then
    FTabMargin.Assign(Value);
end;

procedure TplPageControl.SetTabOverlap(Value: TTabOverlapSize);
begin
  FTabOverlap := Value;
  Invalidate;
end;


procedure TplPageControl.SetActiveFont(const Value: TFont);
begin
  FActiveFont.Assign(Value);
  Invalidate;
end;

procedure TplPageControl.ActiveFontChangeEvent(Sender: TObject);
begin
  UpdateTabForActiveFont(FActivePage);
end;

procedure TplPageControl.SetCloseGlyph(const Value: TBitmap);
begin
  FCloseGlyph.Assign(Value);
  Invalidate;
end;

procedure TplPageControl.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
  if (csDesigning in ComponentState) or FTabBorder3D or FullRefresh or (Tabs.Count = 0) then
    inherited;
end;

procedure TplPageControl.WMPaint(var Msg: TLMPaint);
var
  DC, MemDC: HDC;
  MemBitmap, OldBitmap: HBITMAP;
  PS: TPaintStruct;
  lCanvas : TCanvas;
begin
  if (csDesigning in ComponentState) or FullRefresh or (Tabs.Count = 0) then
  begin
    Inherited;
    Exit;
  end; 
  
  if (Msg.Msg = WM_PAINT) and (not FTabBorder3D) then
  begin
    DC := GetDC(0);
    MemBitmap := CreateCompatibleBitmap(DC, ClientRect.Right, ClientRect.Bottom);
    ReleaseDC(0, DC);
    MemDC := CreateCompatibleDC(0);
    OldBitmap := SelectObject(MemDC, MemBitmap);
    try
      DC := BeginPaint(Handle, PS);
    //  Perform(WM_ERASEBKGND, MemDC, MemDC);
      Msg.DC := MemDC;

      inherited;

      //Create a temporary canvas to draw the tabs on
      lCanvas := TCanvas.Create;
      lCanvas.Handle := MemDC;
      DrawAllTabs(lCanvas);

      lCanvas.Free;

      Msg.DC := 0;
      BitBlt(DC, 0, 0, ClientRect.Right, ClientRect.Bottom, MemDC, 0, 0, SRCCOPY);
      EndPaint(Handle, PS);
    finally
      SelectObject(MemDC, OldBitmap);
      DeleteDC(MemDC);
      DeleteObject(MemBitmap);
    end;
  end
  else
    inherited;
end;

end.
