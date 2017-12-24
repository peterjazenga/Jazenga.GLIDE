{------------------------------------------------------------------------------}
{                                                                              }
{  TExtDBNavigator                                                     }
{  by Matthieu GIROUX                                                          }
{                                                                              }
{------------------------------------------------------------------------------}

unit u_extpdbnavigator;

{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}

{$I ..\DLCompilers.inc}
{$I ..\extends.inc}

interface

uses
    DB, Controls, fonctions_db, sysutils,
    Classes, Graphics, LCLType,
    LCLIntf,
    Messages, DBCtrls, ExtCtrls,
    u_extimgbutton,
    {$IFDEF FPC}
    unite_messages,
    LMessages,
    {$ELSE}
    unite_messages_delphi,
    {$ENDIF}
  {$IFDEF UseRuntime}
    Ext, ExtPascal, ExtForm,
    ExtData, ExtGrid, ExtUtil, ExtAir, ExtDd,
    ExtMenu,  ExtState;

  type
    {$M+}
    TExtPanel_Tab = TExtPanel;
    TExtFormTextField_Grid = TExtFormTextField;
    TExtFormNumberField_Grid = TExtFormNumberField;
    TExtFormDateField_Grid = TExtFormDateField;
    TExtFormTimeField_Grid = TExtFormTimeField;
    TExtFormCheckbox_Grid = TExtFormCheckbox;
    TExtFormComboBox_Grid = TExtFormComboBox;
    {$M-}

  {$ELSE}
    ExtP_Design_Ctrls, ExtP_Design_Grid;
  {$ENDIF}

{$IFDEF VERSIONS}
  const
    gVer_TExtDBNavigator : T_Version = ( Component : 'Composant TExtDBNavigator' ;
                                               FileUnit : 'U_ExtDBNavigator' ;
                                               Owner : 'Matthieu Giroux' ;
                                               Comment : 'Navigator with search, record moving, bookmark buttons.' ;
                                               BugsStory : '1.1.0.0 : Large restructure with resize debugging.' +#13#10
                                                         + '1.0.0.2 : UTF 8.' +#13#10
                                                         + '1.0.0.1 : Optimising source with last lazarus.' +#13#10
                                                         + '1.0.0.0 : Mise en place des images et tests.' +#13#10
                                                         + '0.9.0.0 : En place sans les images à tester.';
                                               UnitType : 3 ;
                                               Major : 1 ; Minor : 1 ; Release : 0 ; Build : 0 );
{$ENDIF}
type

  TNavigatorOrientation = (noHorizontal, noVertical);

  TExtNavigateBtn = (nbEFirst, nbEPrior, nbENext, nbELast,
    nbEInsert, nbEDelete, nbEEdit, nbEPost, nbECancel, nbERefresh,
    nbESearch, nbEMovePrior, nbEMoveNext, nbESetBookmark, nbEGotoBookMark);

  TGlyphSize = (gsSmall, gsLarge);


  EExtBookmarkError = procedure (Sender: TObject; Dataset: TDataset ; Bookmark : {$IFDEF WITH_TBOOKMARK}TBookmark{$ELSE}TBookmarkStr{$ENDIF} ) of object;

  TExtButtonSet = set of TExtNavigateBtn;

  EExtNavClick = procedure (Sender: TObject; Button: TExtNavigateBtn) of object;

{$IFDEF FPC}
const CST_DBNav_Pause = 15 ;
{$ENDIF}

type
  TExtNavButton = class ;

  { TExtDBNavigator }

  TExtDBNavigator = class(TExtPanel, IDatalinkOwner)
  private
    FHints: TStrings;
    FDefHints: TStrings;
    FDataLink: TDataLinkOwnered;
    FSortTable ,
    FSortField : String ;
    FSortAsc : Boolean ;
    {$IFNDEF FPC}
    ResInstance             : THandle      ;
    {$ENDIF}
    FFlat: Boolean;
    FBookmark: {$IFDEF WITH_TBOOKMARK}TBookMark{$ELSE}string{$ENDIF};
    FDataset : TDataset ;
    FOnNavigateClick: EExtNavClick;
    MinButtonSize: TPoint;
    FOnBookmarkError : EExtBookmarkError ;
    FDeleteRecordQuestion : WideString ;
    FConfirmDelete : Boolean ;
    FBeforeAction: EExtNavClick;
    ButtonSize: TPoint;
    FOrientation: TNavigatorOrientation;
    FocusedButton: TExtNavigateBtn;
    FGlyphSize: TGlyphSize;
    FOnNavClick: EExtNavClick;
    FOnBtnMovePrior : TNotiFyEvent;
    FOnBtnMoveNext : TNotiFyEvent;
    FOnBtnPrior : TNotiFyEvent;
    FOnBtnNext : TNotiFyEvent;
    FOnBtnFirst : TNotiFyEvent;
    FOnBtnLast : TNotiFyEvent;
    FOnBtnInsert : TNotiFyEvent;
    FOnBtnEdit : TNotiFyEvent;
    FOnBtnCancel : TNotiFyEvent;
    FOnBtnPost : TNotiFyEvent;
    FOnBtnRefresh : TNotiFyEvent;
    FOnBtnDelete : TNotiFyEvent;
    FOnBtnSearch: TNotifyEvent;
    FOnBtnGotoBookmark: TNotifyEvent;
    FOnBtnSetBookmark: TNotifyEvent;
    FVisibleButtons: TExtButtonSet;
    fTransparent: Boolean;
    function GetSortField: string;
    procedure SetDeleteRecordQuestion ( AValue : WideString );
    function GetField: TField;
    procedure SetSortField(const Value: string);
    function GetDataSource: TDataSource;
    procedure SetDataSource(Value: TDataSource);
    procedure SetFlat(Value: Boolean);
    function GetGlyphs(Index: TExtNavigateBtn): TBitmap;
    procedure SetGlyphs(Index: TExtNavigateBtn; const AValue: TBitmap);
    procedure SwapButtons;
    procedure SwapGlyphs;
    function Swaped: Boolean;
    procedure SetTransparent(Value: Boolean);
    procedure SetGlyphSize(const Value: TGlyphSize);
    procedure SetOrientation(const Value: TNavigatorOrientation);
    function GetHints: TStrings;
    procedure HintsChanged(Sender: TObject);
    procedure SetHints(Value: TStrings);
  protected
    FButtons: array[TExtNavigateBtn] of TExtNavButton;
    FGlyphs : array[TExtNavigateBtn] of TBitmap;
    procedure SetButtonsSize(const W, H: Integer); virtual;
    procedure SetVisible(Value: TExtButtonSet); overload; virtual;
    procedure DataSetChanged; virtual;
    procedure InitHints; virtual;
    procedure EditingChanged; virtual;
    procedure ActiveChanged; virtual;
    function  LoadImageButton ( Btn: TExtNavButton ):Boolean; virtual ;
    function  MoveNextPrior ( const FPrior : Boolean ): Boolean ; virtual;
    function  MoveNext : Boolean ; virtual ;
    function  MovePrior: Boolean ; virtual ;
    procedure UpdateButtons;
    procedure CalcMinSize(const W, H: Integer); virtual ;
    procedure ButtonClickHandler(Sender: TObject); virtual ;
    procedure ButtonMouseDown(Sender: TObject; Button: TMouseButton;
            Shift: TShiftState; X, Y: Integer); virtual ;
    procedure LoadTable; virtual;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure Loaded; override;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure CMBiDiModeChanged(var Msg: TMessage); message CM_BIDIMODECHANGED;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    procedure BtnOnClick(Index: TExtNavigateBtn); virtual;
    constructor Create(AOwner: TComponent); override;
    destructor destroy ; override ;
    property Glyphs[Index: TExtNavigateBtn]: TBitmap read GetGlyphs write SetGlyphs;
    property BookMark : {$IFDEF WITH_TBOOKMARK}TBookMark{$ELSE}string{$ENDIF} read FBookMark write FBookMark ;
    property Field : TField read GetField ;
  published
    property Flat: Boolean read FFlat write SetFlat default False;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DeleteQuestion : WideString read FDeleteRecordQuestion write SetDeleteRecordQuestion ;
    property ConfirmDelete: Boolean read FConfirmDelete write FConfirmDelete default True;
    property Align;
    property Anchors;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled default True;
    {$IFNDEF FPC}
    property Ctl3D;
    property ParentCtl3D;
    {$ENDIF}
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;

    property SortAscendant : Boolean read FSortAsc write FSortAsc default True ;
    property OnButtonsClick: EExtNavClick read FOnNavigateClick write FOnNavigateClick;
    property BeforeAction: EExtNavClick read FBeforeAction write FBeforeAction;
    property Orientation: TNavigatorOrientation read FOrientation write SetOrientation default noHorizontal;
    property VisibleButtons: TExtButtonSet read FVisibleButtons write SetVisible default [nbEFirst, nbEPrior, nbENext, nbELast, nbEInsert, nbEDelete, nbEEdit, nbEPost, nbECancel, nbERefresh];
    property SortField: string read GetSortField write SetSortField;
    property SortTable: string read FSortTable write FSortTable;
    property BiDiMode;
//    property ParentTextMode;
    property GlyphSize: TGlyphSize read FGlyphSize write SetGlyphSize;
    property GlyphFirst: TBitmap index nbEFirst read GetGlyphs write SetGlyphs;
    property GlyphPrior: TBitmap index nbEPrior read GetGlyphs write SetGlyphs;
    property GlyphNext: TBitmap index nbENext read GetGlyphs write SetGlyphs;
    property GlyphLast: TBitmap index nbELast read GetGlyphs write SetGlyphs;
    property GlyphInsert: TBitmap index nbEInsert read GetGlyphs write SetGlyphs;
    property GlyphDelete: TBitmap index nbEDelete read GetGlyphs write SetGlyphs;
    property GlyphEdit: TBitmap index nbEEdit read GetGlyphs write SetGlyphs;
    property GlyphPost: TBitmap index nbEPost read GetGlyphs write SetGlyphs;
    property GlyphCancel: TBitmap index nbECancel read GetGlyphs write SetGlyphs;
    property GlyphRefresh: TBitmap index nbERefresh read GetGlyphs write SetGlyphs;
    property GlyphMoveNext: TBitmap index nbEMoveNext read GetGlyphs write SetGlyphs;
    property GlyphMovePrior: TBitmap index nbEMovePrior read GetGlyphs write SetGlyphs;
    property GlyphSetBookMark: TBitmap index nbESetBookMark read GetGlyphs write SetGlyphs ;
    property GlyphGotoBookMark: TBitmap index nbEGotoBookMark read GetGlyphs write SetGlyphs;
    property GlyphSearch: TBitmap index nbESearch read GetGlyphs write SetGlyphs;
    property OnBtnPrior : TNotifyEvent read FOnBtnPrior write FOnBtnPrior;
    property OnBtnNext : TNotifyEvent read FOnBtnNext write FOnBtnNext;
    property OnBtnMovePrior : TNotifyEvent read FOnBtnPrior write FOnBtnPrior;
    property OnBtnMoveNext : TNotifyEvent read FOnBtnNext write FOnBtnNext;
    property OnBtnFirst : TNotifyEvent read FOnBtnFirst write FOnBtnFirst;
    property OnBtnLast : TNotifyEvent read FOnBtnLast write FOnBtnLast;
    property OnBtnInsert : TNotifyEvent read FOnBtnInsert write FOnBtnInsert;
    property OnBtnEdit : TNotifyEvent read FOnBtnEdit write FOnBtnEdit;
    property OnBtnCancel : TNotifyEvent read FOnBtnCancel write FOnBtnCancel;
    property OnBtnPost : TNotifyEvent read FOnBtnPost write FOnBtnPost;
    property OnBtnRefresh : TNotifyEvent read FOnBtnRefresh write FOnBtnRefresh;
    property OnBtnDelete : TNotifyEvent read FOnBtnDelete write FOnBtnDelete;
    property OnBtnSearch: TNotifyEvent read FOnBtnSearch write FOnBtnSearch;
    property OnBtnSetBookmark: TNotifyEvent read FOnBtnSetBookmark write FOnBtnSetBookmark;
    property OnBtnGotoBookmark: TNotifyEvent read FOnBtnGotoBookmark write FOnBtnGotoBookmark;
    property OnBookmarkError : EExtBookmarkError read FOnBookmarkError write FOnBookmarkError;
    property Transparent: Boolean read fTransparent write SetTransparent default False;
    property OnClick: EExtNavClick read FOnNavClick write FOnNavClick;
    property Hints: TStrings read GetHints write SetHints;
  end;

  { TExtNavButton }

  TExtNavButton = class(TExtImgButton)
  private
    FMouseDragged : Boolean ;
    FIndex: TExtNavigateBtn;
    FBtnStyle:{$IFDEF FPC}TDBNavButtonStyle{$ELSE}TNavButtonStyle{$ENDIF};
    FRepeatTimer: TTimer;
    procedure TimerExpired(Sender: TObject);
    procedure WMSetFocus(var Message: {$IFDEF FPC}TLMSetFocus); message LM_SETFOCUS{$ELSE}TWMSetFocus); message WM_SETFOCUS{$ENDIF};
  protected
    constructor Create (AOwner:TComponent);override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    property NavStyle: {$IFDEF FPC}TDBNavButtonStyle{$ELSE}TNavButtonStyle{$ENDIF} read FBtnStyle write FBtnStyle;
    property Index : TExtNavigateBtn read FIndex write FIndex;
  end;


const
  BtnTypeNames: array[TExtNavigateBtn] of String = (CST_RESSOURCENAV + 'FIRST', CST_RESSOURCENAV + 'PREVIOUS', CST_RESSOURCENAV + 'NEXT',
    CST_RESSOURCENAV + 'LAST', CST_RESSOURCENAV + 'INSERT', CST_RESSOURCENAV + 'DELETE', CST_RESSOURCENAV + 'EDIT',
    CST_RESSOURCENAV + 'POST', CST_RESSOURCENAV + 'CANCEL', CST_RESSOURCENAV + 'REFRESH',
    CST_RESSOURCENAV + 'SEARCH', CST_RESSOURCENAV + CST_RESSOURCENAVMOVE + 'PREVIOUS', CST_RESSOURCENAV + CST_RESSOURCENAVMOVE + 'NEXT',

    CST_RESSOURCENAV + 'SET' + CST_RESSOURCENAVBOOKMARK,CST_RESSOURCENAV + 'GOTO' + CST_RESSOURCENAVBOOKMARK);


implementation

uses fonctions_proprietes, fonctions_system, fonctions_dbobjects,Math, fonctions_dialogs ;

{$IFDEF DELPHI}
  {$R *.res}
{$ENDIF}

var
  ExtBtnHintId: array[TExtNavigateBtn] of String ;
{$IFDEF DELPHI}
  Alignments: array [TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
{$ENDIF}
type
  TParentControl = class(TWinControl);

{ This procedure is copied from RxLibrary VCLUtils }
// transparency
procedure CopyParentImage(Control: TControl; Dest: TCanvas);
var
  I, Count,
{$IFDEF DELPHI}
  X, Y,
{$ENDIF}
  SaveIndex: Integer;
  DC: HDC;
  R, SelfR, CtlR: TRect;
begin
  if (Control = nil) or (Control.Parent = nil) then Exit;
  Count := Control.Parent.ControlCount;
  DC := Dest.Handle;
{$IFDEF WIN32}
  with Control.Parent do ControlState := ControlState + [csPaintCopy];
  try
{$ENDIF}
    with Control do begin
      SelfR := Bounds(Left, Top, Width, Height);
      {$IFDEF DELPHI}
      X := -Left; Y := -Top;
      {$ENDIF}
    end;
    { Copy parent control image }
    SaveIndex := SaveDC(DC);
    try
{$IFDEF DELPHI}
      SetViewportOrgEx(DC, X, Y, nil);
{$ENDIF}
      IntersectClipRect(DC, 0, 0, Control.Parent.ClientWidth,
        Control.Parent.ClientHeight);
      with TParentControl(Control.Parent) do begin
        Perform(WM_ERASEBKGND, DC, 0);
        PaintWindow(DC);
      end;
    finally
      RestoreDC(DC, SaveIndex);
    end;
    { Copy images of graphic controls }
    for I := 0 to Count - 1 do begin
      if Control.Parent.Controls[I] = Control then Break
      else if (Control.Parent.Controls[I] <> nil) and
        (Control.Parent.Controls[I] is TGraphicControl) then
      begin
        with TGraphicControl(Control.Parent.Controls[I]) do begin
          CtlR := Bounds(Left, Top, Width, Height);
          if Bool(IntersectRect(R, SelfR, CtlR)) and Visible then begin
{$IFDEF WIN32}
            ControlState := ControlState + [csPaintCopy];
{$ENDIF}
            SaveIndex := SaveDC(DC);
            try
{$IFDEF DELPHI}
              SetViewportOrgEx(DC, Left + X, Top + Y, nil);
{$ENDIF}
              IntersectClipRect(DC, 0, 0, Width, Height);
{$IFDEF FPC}
              Perform(LM_PAINT, DC, 0);
{$ELSE}
              Perform(WM_PAINT, DC, 0);
{$ENDIF}
            finally
              RestoreDC(DC, SaveIndex);
{$IFDEF WIN32}
              ControlState := ControlState - [csPaintCopy];
{$ENDIF}
            end;
          end;
        end;
      end;
    end;
{$IFDEF WIN32}
  finally
    with Control.Parent do ControlState := ControlState - [csPaintCopy];
  end;
{$ENDIF}
end;

{ TExtDBNavigator }


// order dataset loading
procedure TExtDBNavigator.LoadTable;
begin
  FDataset.Free;
  FDataset := nil ;
  if  ( FSortField <> '' )
  and assigned ( FDataLink.DataSet )
   then
    Begin
      fb_SortADataset(FDataLink.DataSet,FSortField,False);

      if  ( FSortTable <> '' )
        Then
         Begin
           FDataset := fdat_CloneDatasetWithoutSQL ( FDataLink.DataSet, Owner );
           FDataset.Name:=Self.Name + FSortTable;
           p_SetSQLQuery(FDataset, CST_SQL_SELECT+FSortField+CST_SQL_FROM+FSortTable+CST_SQL_ORDER_BY + FSortField);
           p_SetComponentProperty(FDataset, 'Table', FSortTable);
           fb_SortADataset ( FDataset, FSortField, False );
         end;
    end;
end;

// Loading sorting table - left to right or right to left - Hints - Loading if datasource opened
procedure TExtDBNavigator.Loaded;
begin
  inherited Loaded;
  SetButtonsSize(ClientWidth,ClientHeight);
  LoadTable;
  if UseRightToLeftAlignment and not Swaped then
    SwapButtons;
  InitHints;
  ActiveChanged;
end;

// resizing buttons
procedure TExtDBNavigator.WMSize(var Msg: TWMSize);
begin
  inherited;
  if not ( csLoading in ComponentState ) Then
   Begin
    SetButtonsSize(ClientWidth,ClientHeight);
    if UseRightToLeftAlignment and not Swaped then
       SwapButtons;
   end;
end;

// bidimode : Left to right or right to left
procedure TExtDBNavigator.CMBiDiModeChanged(var Msg: TMessage);
var b : TExtNavigateBtn ;
begin
  for B := Low(FButtons) to High(FButtons) do
    FButtons[B].BidiMode := BiDiMode;             // for right to left hint
  if UseRightToLeftAlignment <> Swaped then
  begin
    SwapButtons;
    if not (csReading in ComponentState) and not (csLoading in ComponentState) then
      SwapGlyphs;
  end;

end;

// Set Glyph button from resource
function TExtDBNavigator.LoadImageButton ( Btn: TExtNavButton ):Boolean;
var
{$IFDEF FPC}
  ResName: Ansistring;
{$ELSE}
  ResName: string;
{$ENDIF}
begin
  Result := False ;
  if GlyphSize = gsSmall then
    FmtStr(ResName, '%sT', [BtnTypeNames[Btn.Index]])
  else
    FmtStr(ResName, '%sL', [BtnTypeNames[Btn.Index]]);

  Btn.Glyph:= GetAppDir + ResName;
End ;

// Create and update buttons
procedure TExtDBNavigator.UpdateButtons;
var
  I: TExtNavigateBtn;
  Btn: TExtNavButton;
begin
  if GlyphSize = gsSmall then
    MinButtonSize := Point(20, 20)
  else
    MinButtonSize := Point(32, 30);
{$IFNDEF FPC}
  if Parent = nil
    Then CalcMinSize(Width,Height)
    Else
{$ENDIF}
      CalcMinSize(ClientWidth,ClientHeight);
{$IFDEF DELPHI}
  ResInstance:= FindResourceHInstance(HInstance);
{$ENDIF}
  for I := Low(FButtons) to High(FButtons) do
  begin
    if (FButtons [ i ] = nil )
     Then
       FButtons [ i ] := TExtNavButton.Create (Self);
    Btn := FButtons [ i ];
    Btn.Parent := Self;
    Btn.Index := I;
    LoadImageButton ( Btn );
    Btn.Visible := I in FVisibleButtons;

    Btn.Enabled := False;

    Btn.OnClick := ButtonClickHandler;
    Btn.OnMouseDown := ButtonMouseDown;
  end;

  FButtons[nbEPrior].NavStyle := FButtons[nbEPrior].NavStyle + [nsAllowTimer];
  FButtons[nbENext].NavStyle  := FButtons[nbENext].NavStyle + [nsAllowTimer];
end;

// initing property and datalink on create
constructor TExtDBNavigator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataset:=nil;
  Enabled:=True;
  FDatalink := TDataLinkOwnered.Create ( Self );
  ControlStyle:=ControlStyle-[csAcceptsControls,csSetCaption]+[csOpaque];
  FDeleteRecordQuestion := GS_SUPPRIMER_QUESTION ;

  // BUTTONS
  FVisibleButtons := [nbEFirst, nbEPrior, nbENext, nbELast,
    nbEInsert, nbEDelete, nbEEdit, nbEPost, nbECancel, nbERefresh];
  UpdateButtons;

  FHints := TStringList.Create;
  TStringList(FHints).OnChange := HintsChanged;
  InitHints;

  FSortAsc   := True ;
  FTransparent := False ;
  FocusedButton := nbEFirst;
  FConfirmDelete := True;
  FOrientation:=noHorizontal;
  Color := clBtnFace;

  FBookmark := {$IFDEF WITH_TBOOKMARK}nil{$ELSE}''{$ENDIF};

end;

// helping
procedure TExtDBNavigator.InitHints;
var
  I: Integer;
  J: TExtNavigateBtn;
begin
  if not Assigned(FDefHints) then
  begin
    FDefHints := TStringList.Create;
    for J := Low(FButtons) to High(FButtons) do
      FDefHints.Add(ExtBtnHintId[J]);
  end;
  for J := Low(FButtons) to High(FButtons) do
    FButtons[J].Hint := FDefHints[Ord(J)];
  J := Low(FButtons);
  for I := 0 to (FHints.Count - 1) do
  begin
    if FHints.Strings[I] <> '' then FButtons[J].Hint := FHints.Strings[I];
    if J = High(FButtons) then Exit;
    Inc(J);
  end;
end;


// left to right or right to left
procedure TExtDBNavigator.SwapButtons;
var
  X: Integer;
  LB, RB: TExtNavigateBtn;
begin
  LB := Low(FButtons);
  RB := High(FButtons);
  repeat
    while not (LB in FVisibleButtons) and (LB < High(FButtons)) do Inc(LB);
    while not (RB in FVisibleButtons) and (RB > Low(FButtons)) do Dec(RB);
    if LB < RB then
    begin
      X := FButtons[LB].Left;
      FButtons[LB].Left := FButtons[RB].Left;
      FButtons[RB].Left := X;
      Inc(LB);
      Dec(RB);
    end;
  until LB >= RB;
end;

// left to right or right to left
function TExtDBNavigator.Swaped: Boolean;
var
  LB, RB: TExtNavigateBtn;
begin
  LB := Low(FButtons);
  RB := High(FButtons);
  while not (LB in FVisibleButtons) and (LB < High(FButtons)) do Inc(LB);
  while not (RB in FVisibleButtons) and (RB > Low(FButtons)) do Dec(RB);
  Result := FButtons[LB].Left > FButtons[RB].Left;
end;

// for property
function TExtDBNavigator.GetGlyphs(Index: TExtNavigateBtn): TBitmap;
begin
  Result := FGlyphs[Index];
end;

// clicking event
procedure TExtDBNavigator.ButtonClickHandler(Sender: TObject);
begin
  BtnOnClick(TExtNavButton(Sender).Index);
end;

// setting focused button without focusing totally to button
procedure TExtDBNavigator.ButtonMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  OldFocus: TExtNavigateBtn;
begin
  OldFocus := FocusedButton;
  FocusedButton := TExtNavButton (Sender).Index;
  if TabStop and (GetFocus <> Handle) and CanFocus then
  begin
    SetFocus;
    if (GetFocus <> Handle) then
      Exit;
  end
  else if TabStop and (GetFocus = Handle) and (OldFocus <> FocusedButton) then
  begin
    FButtons[OldFocus].Invalidate;
    FButtons[FocusedButton].Invalidate;
  end;
end;

// calculate min button size
procedure TExtDBNavigator.CalcMinSize(const W, H: Integer);
var
  Count: Integer;
  I: TExtNavigateBtn;
begin
  if (csLoading in ComponentState) then Exit;
  if FButtons[nbEFirst] = nil then Exit;

  Count := 0;
  for I := Low(FButtons) to High(FButtons) do
    if FButtons[I].Visible then
      Inc(Count);
  if Count = 0 then Inc(Count);

  with MinButtonSize do
  if Orientation = noHorizontal then
  begin
    X := Min(W, Count * X) div Count - 1;
    Y := Min(H, Y);
  end
  else
  begin
    X := Min(W, X);
    Y := Min(H, Count * Y) div Count - 1;
  end;
end;

// adapt each button to panel size
procedure TExtDBNavigator.SetButtonsSize(const W, H: Integer);
var
  Count: Integer;
  I: TExtNavigateBtn;
  Space: Integer;
  LCoord: Integer;
  LIsFirst : Boolean;
begin
  if FButtons[nbEFirst] = nil then
    Exit;

  CalcMinSize(W, H);

  Count := 0;
  for I := Low(FButtons) to High(FButtons) do
    if FButtons[I].Visible then
      Inc(Count);
  if Count = 0 then Inc(Count);
  LIsFirst := True;
  LCoord := 0;
  if Orientation = noHorizontal then
  begin
    ButtonSize.X := W div Count - 1;
    ButtonSize.Y := H;

    for I := Low(FButtons) to High(FButtons) do
     if FButtons[I].Visible then
      begin
        if LIsFirst
         then Space := W - ( Count * ( ButtonSize.X + 1 ))
         Else Space := 0;
        FButtons[I].SetBounds(LCoord, 0, ButtonSize.X + Space, H);
        Inc(LCoord, ButtonSize.X + Space + 1);
        LIsFirst := False;
      end;
  end
  else
  begin
   ButtonSize.X := W;
   ButtonSize.Y := H div Count - 1;
    for I := Low(FButtons) to High(FButtons) do
     if FButtons[I].Visible then
      begin
        if LIsFirst
         then Space := H - ( Count * ( ButtonSize.Y + 1 ))
         Else Space := 0;
        FButtons[I].SetBounds(0, LCoord, W, ButtonSize.Y + Space);
        Inc(LCoord, ButtonSize.Y + Space + 1);
        LIsFirst := False;
      end;
  end;
end;

// setting visibilty of a button
procedure TExtDBNavigator.SetVisible(Value: TExtButtonSet);
var
  I: TExtNavigateBtn;
begin
  if Value <> FVisibleButtons then
   Begin
    FVisibleButtons := Value;
    for I := Low(FButtons) to High(FButtons) do
      FButtons[I].Visible := I in FVisibleButtons;
    SetButtonsSize(ClientWidth, ClientHeight);
    Invalidate;
   end;
end;

// horizontal or vertical buttons
procedure TExtDBNavigator.SetOrientation(const Value: TNavigatorOrientation);
begin
  if (FOrientation <> Value) then
  begin
    FOrientation := Value;
    SetButtonsSize(ClientWidth,ClientHeight);
  end;
end;

// auto width of buttons
procedure TExtDBNavigator.SetGlyphSize(const Value: TGlyphSize);
var
  I: TExtNavigateBtn;
begin
  if FGlyphSize <> Value then
  begin
    FGlyphSize := Value;

    if GlyphSize = gsSmall then
      MinButtonSize := Point(20, 18)
    else
      MinButtonSize := Point(32, 30);

    for i := Low ( FButtons ) to high ( FButtons ) do
      LoadImageButton ( FButtons [ i ] );

  end;
end;

// move record to next with order field
function TExtDBNavigator.MoveNextPrior ( const FPrior : Boolean ): Boolean ;
begin
  Result := False ;
  if  ( FDataLink.Active ) Then
    Begin
      if assigned ( FDataLink.DataSet.FindField ( FSortField )) Then
        Begin
          if assigned ( FDataset ) Then
            Begin
              try
               FDataset.Open;
              except
              end;
//              FDataSet.Bookmark := FDataLink.DataSet.Bookmark;
              FDataSet.Locate ( FSortField, FDataLink.DataSet.FieldByName(FSortField).Value, [] );
              Result := fb_IntervertitPositions2Champs ( FDataSet, FSortField, FPrior, FSortAsc, False );
              fb_RefreshDataset(FDataLink.DataSet);
//              FDataLink.DataSet.Bookmark := FDataSet.Bookmark;
              FDataLink.DataSet.Locate ( FSortField, FDataSet.FieldByName(FSortField).Value, [] );
            end
           Else
            Result := fb_IntervertitPositions2Champs ( FDataLink.DataSet, FSortField, FPrior, FSortAsc, True )

        end
      Else
        ShowMessage ( 'Fill SortField.' );
    End ;
end;

// move record to next with order field
function TExtDBNavigator.MoveNext : Boolean ;
begin
  Result := MoveNextPrior(False);
End ;

// move record to prior with order field
function TExtDBNavigator.MovePrior : Boolean ;
begin
  Result := MoveNextPrior(True);
End ;

// autocliks
procedure TExtDBNavigator.BtnOnClick(Index: TExtNavigateBtn);
begin
  if (DataSource <> nil) and (DataSource.DataSet <> nil) and (DataSource.DataSet.State <> dsInactive) then
  begin
    if not (csDesigning in ComponentState) and Assigned(FBeforeAction) then
      FBeforeAction(Self, Index);
    with DataSource.DataSet do
    begin
      case Index of
      nbEPrior:
        if Assigned(FOnBtnPrior) then
          FOnBtnPrior(Self)
        else
          Prior;
      nbENext:
        if Assigned(FOnBtnNext) then
          FOnBtnNext(Self)
        else
          Next;
      nbEFirst:
        if Assigned(FOnBtnFirst) then
          FOnBtnFirst(Self)
        else
          First;
      nbELast:
        if Assigned(FOnBtnLast) then
          FOnBtnLast(Self)
        else
          Last;
      nbEInsert:
        if Assigned(FOnBtnInsert) then
          FOnBtnInsert(Self)
        else
          Insert;
      nbEEdit:
        if Assigned(FOnBtnEdit) then
          FOnBtnEdit(Self)
        else
          Edit;
      nbECancel:
        if Assigned(FOnBtnCancel) then
          FOnBtnCancel(Self)
        else
          Cancel;
      nbEPost:
        if Assigned(FOnBtnPost) then
          FOnBtnPost(Self)
        else
         if (State in [dsInsert,dsEdit]) Then
          Post;
      nbERefresh:
        if Assigned(FOnBtnRefresh) then
          FOnBtnRefresh(Self)
        else
          Refresh;
      nbEDelete:
        if Assigned(FOnBtnDelete) then
          FOnBtnDelete(Self)
        else
        begin
          if not FConfirmDelete or
            (MessageDlg(FDeleteRecordQuestion, mtConfirmation,
              mbOKCancel, 0) <> idCancel) then
            Delete;
        end;
      nbESearch:
        if Assigned(FOnBtnSearch) then
          FOnBtnSearch(Self);
      nbEMoveNext:
        if Assigned(FOnBtnMoveNext) then
          FOnBtnMoveNext(Self)
        else
          MoveNext;
      nbEMovePrior:
        if Assigned(FOnBtnMovePrior) then
          FOnBtnMovePrior(Self)
        else
          MovePrior;
      nbESetBookmark:
        if Assigned(FOnBtnSetBookmark) then
          FOnBtnSetBookmark(Self)
        else
        if assigned ( FDataLink.DataSet ) then
          begin
            FBookmark := FDataLink.DataSet.Bookmark;
            FButtons[nbEGotoBookmark].Enabled := True;
          end;
      nbEGotoBookmark:
        if Assigned(FOnBtnGotoBookmark) then
          FOnBtnGotoBookmark(Self)
        else
        if assigned ( FDataLink.DataSet ) then
          begin
            if ( FBookmark <> {$IFDEF WITH_TBOOKMARK}nil{$ELSE}''{$ENDIF} )
            and FDataLink.DataSet.BookmarkValid ( @FBookMark ) then
              try
                FDataLink.DataSet.Bookmark := FBookmark
              Except
                if Assigned(FOnBookmarkError) then
                  FOnBookmarkError(Self, FDataLink.DataSet, FBookMark );
              End ;
          end;
      end;
    end;
  end;
  if not (csDesigning in ComponentState) and Assigned(FOnNavigateClick) then
    FOnNavigateClick(Self, Index);
end;

// flat graphics
procedure TExtDBNavigator.SetFlat(Value: Boolean);
var
  I: TExtNavigateBtn;
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
    for I := Low(FButtons) to High(FButtons) do
      FButtons[I].Flat := Value;
  end;
end;

// deleting object's properties when destroying objects
procedure TExtDBNavigator.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

// for property
function TExtDBNavigator.GetDataSource: TDataSource;
begin
  if assigned ( FDataLink ) Then
    Result := FDataLink.DataSource
  Else
    Result := nil ;

end;

// for property
procedure TExtDBNavigator.SetDataSource(Value: TDataSource);
begin
  if assigned ( FDataLink ) Then
    FDataLink.DataSource := Value
  Else
    Exit ;
  if not (csLoading in ComponentState) then
    ActiveChanged;
{$IFDEF DELPHI}
  if Value <> nil then Value.FreeNotification(Self);
{$ENDIF}
  if assigned ( FDataLink.DataSource )
   Then
    LoadTable;
end;

// setting buttons on dataset changed
procedure TExtDBNavigator.DataSetChanged;
var
  UpEnable, DnEnable: Boolean;
begin
  if not assigned ( FDataLink ) Then
    Exit ;

  UpEnable := Enabled and FDataLink.Active and not FDataLink.DataSet.BOF;
  DnEnable := Enabled and FDataLink.Active and not FDataLink.DataSet.EOF;
  FButtons[nbEFirst].Enabled := UpEnable;
  FButtons[nbEPrior].Enabled := UpEnable;
  FButtons[nbENext].Enabled := DnEnable;
  FButtons[nbEMovePrior].Enabled := UpEnable;
  FButtons[nbEMoveNext].Enabled := DnEnable;
  FButtons[nbELast].Enabled := DnEnable;
  FButtons[nbESearch].Enabled := FDataLink.DataSet.State = dsBrowse;
  FButtons[nbEDelete].Enabled := Enabled and FDataLink.Active and
    FDataLink.DataSet.CanModify and
    not (FDataLink.DataSet.BOF and FDataLink.DataSet.EOF);
  FButtons[nbESetBookMark].Enabled := Enabled and FDataLink.Active and
    not (FDataLink.DataSet.BOF and FDataLink.DataSet.EOF);
  FButtons[nbESetBookMark].Enabled := Enabled and FDataLink.Active and
    not (FDataLink.DataSet.BOF and FDataLink.DataSet.EOF)
    and ( FBookmark <> {$IFDEF WITH_TBOOKMARK}nil{$ELSE}''{$ENDIF} )
    and  FDataLink.DataSet.BookmarkValid( @FBookmark );
end;

// setting buttons on dataset changed
procedure TExtDBNavigator.EditingChanged;
var
  CanModify: Boolean;
begin
  if not assigned ( FDataLink ) Then
    Exit ;

  CanModify := Enabled and FDataLink.Active and FDataLink.DataSet.CanModify;
  FButtons[nbEInsert].Enabled := CanModify;
  FButtons[nbEEdit].Enabled := CanModify and not FDataLink.Editing;
  FButtons[nbEPost].Enabled := CanModify and FDataLink.Editing;
  FButtons[nbECancel].Enabled := CanModify and FDataLink.Editing;
  FButtons[nbERefresh].Enabled := CanModify;
end;

// setting buttons on dataset changed
procedure TExtDBNavigator.ActiveChanged;
var
  I: TExtNavigateBtn;
begin
  if not assigned ( FDataLink ) Then
    Exit ;

  if not (Enabled and FDataLink.Active) then
    for I := Low(FButtons) to High(FButtons) do
      FButtons[I].Enabled := False
  else
  begin
    DataSetChanged;
    EditingChanged;
  end;

end;

// for property
function TExtDBNavigator.GetField: TField;
begin
   Result := nil ;
   if assigned ( FDataLink.DataSet ) then
     Result := FDataLink.DataSet.FindField ( FSortField );
end;
 // for property
function TExtDBNavigator.GetSortField: string;
begin
  Result := FSortField;
end;

// getting help
function TExtDBNavigator.GetHints: TStrings;
begin
  if (csDesigning in ComponentState) and not (csWriting in ComponentState) and
     not (csReading in ComponentState) and (FHints.Count = 0) then
    Result := FDefHints else
    Result := FHints;
end;

// help setting
procedure TExtDBNavigator.SetHints(Value: TStrings);
begin
  if Value.Text = FDefHints.Text then
    FHints.Clear else
    FHints.Assign(Value);
end;

// helping initing
procedure TExtDBNavigator.HintsChanged(Sender: TObject);
begin
  InitHints;
end;

// sorting
procedure TExtDBNavigator.SetSortField(const Value: string);
begin
  FSortField := Value;

end;

destructor TExtDBNavigator.destroy;
var li_i : TExtNavigateBtn ;
begin

  FDefHints.Free;
//  FDataLink := nil;
  FHints.Free;
  for li_i := low ( TExtNavigateBtn ) to high ( TExtNavigateBtn ) do
    if assigned ( FGlyphs [ li_i ] ) Then
      Begin
        {$IFDEF DELPHI}
        FGlyphs [ li_i ].Dormant;
        {$ENDIF}
        FGlyphs [ li_i ].FreeImage;
        FGlyphs [ li_i ].Free;
      End ;
  inherited;
  FDataLink.Free;
  FDataLink:=nil;
end;

// setting deleting question
procedure TExtDBNavigator.SetDeleteRecordQuestion(AValue: WideString);
begin
  if AValue = '' Then
    FDeleteRecordQuestion := GS_SUPPRIMER_QUESTION
  Else
    FDeleteRecordQuestion := AValue ;
end;

{TExtNavButton}

// timer creating
procedure TExtNavButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown (Button, Shift, X, Y);
  if nsAllowTimer in FBtnStyle then
  begin
    if FRepeatTimer = nil then
      FRepeatTimer := TTimer.Create(Self);

    FRepeatTimer.OnTimer := TimerExpired;
{$IFDEF FPC}
    FRepeatTimer.Interval := CST_DBNav_Pause;
{$ELSE}
    FRepeatTimer.Interval := InitRepeatPause;
{$ENDIF}
    FRepeatTimer.Enabled  := True;
  end;
end;

// timer inactivate
procedure TExtNavButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
                                  X, Y: Integer);
begin
  inherited MouseUp (Button, Shift, X, Y);
  if FRepeatTimer <> nil then
    FRepeatTimer.Enabled  := False;
end;

// timer for clicking
procedure TExtNavButton.TimerExpired(Sender: TObject);
begin
{$IFDEF FPC}
  FRepeatTimer.Interval := CST_DBNav_Pause;
{$ELSE}
  FRepeatTimer.Interval := RepeatPause;
{$ENDIF}
  if (FState = bsDown)
  and MouseCapture then
  begin
    try
      Click;
    except
      FRepeatTimer.Enabled := False;
      raise;
    end;
  end;
end;

// painting button
constructor TExtNavButton.Create(AOwner: TComponent);
var
  R: TRect;
begin
  with SelfSession do begin
    SetCodePress;
    SetStyle('.new-tab{background-image:url(' + fs_getExtPath + '/examples/feed-viewer/images/new_tab.gif) !important}');
    SetStyle('.tabs{background:url(' + ExtPath + '/examples/desktop/images/tabs.gif)}');
  end;
  with TExtButton.Create do begin
    RenderTo := 'body';
    Text     := 'Add Tab using AJAX!';
    IconCls  := 'new-tab';
    Handler  := Ajax(AddTab);
    OnClick  := HandleExtButtonClick; // Delphi style event handler
  end;
end;

// no button focus
procedure TExtNavButton.WMSetFocus(var Message: {$IFDEF FPC}TLMSetFocus{$ELSE}TWMSetFocus{$ENDIF});
begin
  if ( csDesigning in ComponentState ) Then
    Begin
      Parent.SetFocus;
    End
   Else
    Begin
     inherited ;
    End ;

end;


// initing resources, version and hints
initialization
{$IFDEF FPC}
  {$i U_ExtPDBNavigator2.res}
{$ELSE}
  ExtBtnHintId [ nbEFirst ] := SFirstRecord;
  ExtBtnHintId [ nbEPrior ] := SPriorRecord;
  ExtBtnHintId [ nbENext  ] := SNextRecord;
  ExtBtnHintId [ nbELast ] := SLastRecord;
  ExtBtnHintId [ nbERefresh ] := SRefreshRecord;
  ExtBtnHintId [ nbEInsert ] := SInsertRecord;
  ExtBtnHintId [ nbEDelete ] := SDeleteRecord;
  ExtBtnHintId [ nbEEdit ] := SEditRecord;
  ExtBtnHintId [ nbEPost ] := SPostEdit;
  ExtBtnHintId [ nbECancel] := SCancelEdit;
  ExtBtnHintId [ nbESearch ] := GS_SearchRecord;
  ExtBtnHintId [ nbEMovePrior ] := GS_MovePreviousRecord;
  ExtBtnHintId [ nbEMoveNext ] := GS_MoveNextRecord;
  ExtBtnHintId [ nbESetBookmark ] := GS_SetBookmarkRecord;
  ExtBtnHintId [ nbEGotoBookmark ] := GS_GotoBookmarkRecord;
{$ENDIF}
{$IFDEF VERSIONS}

  p_ConcatVersion ( gVer_TExtDBNavigator );
{$ENDIF}
end.
