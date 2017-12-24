unit u_buttons_appli;

{$I ..\DLCompilers.inc}
{$I ..\extends.inc}
{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}

interface

uses
{$IFNDEF FPC}
  Windows, Messages,
{$ENDIF}
  Classes,
{$IFDEF VERSIONS}
  fonctions_version,
{$ENDIF}
  Controls,
  u_buttons_defs;

{$IFDEF VERSIONS}
const
  gVer_buttons_appli: T_Version = (Component: 'Customized Buttons';
    FileUnit: 'u_buttons_appli';
    Owner: 'Matthieu Giroux';
    Comment: 'Customized Buttons components.';
    BugsStory: '1.0.1.1 : Testing GlyphSize.' +
      #13#10 + '1.0.1.0 : Changing setting of names.' +
      #13#10 + '1.0.0.2 : Date and Folder Buttons.' +
      #13#10 + '1.0.0.1 : UTF 8.' +
      #13#10 + '1.0.0.0 : Version OK.' +
      #13#10 + '0.8.0.1 : Group view buttons better.' +
      #13#10 + '0.8.0.0 : To test.';
    UnitType: 3;
    Major: 1; Minor: 0; Release: 1; Build: 1);
{$ENDIF}


type

  { TFWClose }

  TFWClose = class(TFWButton)
  public
    constructor Create(AOwner: TComponent); override;
    procedure LoadBitmap; override;
    procedure Loaded; override;
    procedure Click; override;
  published

    property Width default CST_FWWIDTH_CLOSE_BUTTON;
  end;

  { TFWCancel }
  TFWCancel = class(TFWButton)
  public
    procedure Loaded; override;
    procedure LoadBitmap; override;
  end;


  { TFWOK }
  TFWOK = class(TFWButton)
  public
    procedure Loaded; override;
    procedure LoadBitmap; override;
  published

  end;


  { TFWInsert }
  TFWInsert = class(TFWButton)
  public
    procedure Loaded; override;
    procedure LoadBitmap; override;
  published

  end;

  { TFWAdd }
  TFWAdd = class(TFWButton)
  public
    procedure LoadBitmap; override;
  end;

  { TFWMedia }

  TFWMedia = class(TFWButtonGlyphs)
  public
    procedure Loaded; override;
  end;

  { TFWDelete }
  TFWDelete = class(TFWButton)
  public
    procedure LoadBitmap; override;
  end;

  { TFWDocument }
  TFWDocument = class(TFWButton)
  public
    procedure LoadBitmap; override;
  end;

  { TFWFolder }
  TFWFolder = class(TFWButton)
  public
    procedure LoadBitmap; override;
  end;

  { TFWDate }
  TFWDate = class(TFWButton)
  public
    procedure LoadBitmap; override;
  end;

  { TFWQuit }
  TFWQuit = class(TFWButton)
  public
    procedure Loaded; override;
    procedure LoadBitmap; override;
  end;

  { TFWErase }
  TFWErase = class(TFWButton)
  public
    procedure Loaded; override;
    procedure LoadBitmap; override;
  end;

  { TFWSaveAs }
  TFWSaveAs = class(TFWButton)
  public
    procedure Loaded; override;
    procedure LoadBitmap; override;
  end;

  { TFWLoad }
  TFWLoad = class(TFWButton)
  public
    procedure Loaded; override;
    procedure LoadBitmap; override;
  end;

  { TFWPrint }
  TFWPrint = class(TFWButton)
  public
    procedure Loaded; override;
    procedure LoadBitmap; override;
  end;

  { TFWPreview }
  TFWPreview = class(TFWButton)
  public
    procedure LoadBitmap; override;
  end;

  { TFWNext }
  TFWNext = class(TFWButton)
  public
    procedure LoadBitmap; override;
  end;

  { TFWPrior }
  TFWPrior = class(TFWButton)
  public
    procedure LoadBitmap; override;
  end;

  { TFWPrior }
  TFWRefresh = class(TFWButton)
  public
    procedure LoadBitmap; override;
  end;

  { TFWCopy }
  TFWCopy = class(TFWButton)
  public
    procedure Loaded; override;
    procedure LoadBitmap; override;
  end;

  { TFWInit }
  TFWInit = class(TFWButton)
  public
    procedure LoadBitmap; override;
  end;

  { TFWConfig }
  TFWConfig = class(TFWButton)
  public
    procedure LoadBitmap; override;
  end;

  { TFWImport }
  TFWImport = class(TFWButton)
  public
    procedure LoadBitmap; override;
  end;

  { TFWTrash }
  TFWTrash = class(TFWButton)
  public
    procedure LoadBitmap; override;
  end;

  { TFWExport }
  TFWExport = class(TFWButton)
  public
    procedure LoadBitmap; override;
  end;

  { TFWSearch }
  TFWSearch = class(TFWButton)
  public
    procedure LoadBitmap; override;
  end;

  { TFWZoomIn }
  TFWZoomIn = class(TFWButton)
  public
    procedure LoadBitmap; override;
  end;

  { TFWZoomOut }
  TFWZoomOut = class(TFWButton)
  public
    procedure LoadBitmap; override;
  end;

{$IFDEF GROUPVIEW}

  { TFWGroupButton }

  { TFWGroupButtonActions }

  TFWGroupButtonActions = class(TFWButton)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Width default CST_WIDTH_BUTTONS_ACTIONS;
    property Height default CST_HEIGHT_BUTTONS_ACTIONS;
  end;


  { TFWBasket }

  TFWBasket = class(TFWGroupButtonActions)
  public
    procedure Loaded; override;
    procedure LoadBitmap; override;

  published

    property Caption stored False;
  end;

  { TFWRecord }

  TFWRecord = class(TFWGroupButtonActions)
  public
    procedure Loaded; override;
    procedure LoadBitmap; override;

  published

    property Caption stored False;
  end;

  { TFWGroupButtonMoving }

  TFWGroupButtonMoving = class(TFWButton)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property GlyphSize default CST_SIZE_BUTTONS_MOVING;
    property Width default CST_WIDTH_BUTTONS_MOVING;
    property Height default CST_HEIGHT_BUTTONS_MOVING;
  end;

  { TFWBigGroupButtonMoving }

  TFWBigGroupButtonMoving = class(TFWButton)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property GlyphSize default CST_SIZE_BUTTONS_MOVING;
    property Width default CST_WIDTH_BUTTONS_MOVING;
    property Height default CST_HEIGHT_BIG_BUTTONS_MOVING;
  end;

  { TFWGroupButtonMovingV }

  TFWGroupButtonMovingV = class(TFWButton)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property GlyphSize default CST_SIZE_BUTTONS_MOVINGV;
    property Width default CST_WIDTH_BUTTONS_MOVINGV;
    property Height default CST_HEIGHT_BUTTONS_MOVINGV;
  end;

  { TFWBigGroupButtonMovingV }

  TFWBigGroupButtonMovingV = class(TFWButton)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property GlyphSize default CST_SIZE_BUTTONS_MOVINGV;
    property Width default CST_WIDTH_BIG_BUTTONS_MOVINGV;
    property Height default CST_HEIGHT_BUTTONS_MOVINGV;
  end;

  { TFWOutSelect }
  TFWOutSelect = class(TFWGroupButtonMoving)
  public
    procedure LoadBitmap; override;
  published

  end;

  { TFWOutAll }


  TFWOutAll = class(TFWBigGroupButtonMoving)
  public
    procedure LoadBitmap; override;
  published

  end;

  { TFWInSelect }
  TFWInSelect = class(TFWGroupButtonMoving)
  public
    procedure LoadBitmap; override;
  published

  end;

  { TFWInAll }
  TFWInAll = class(TFWBigGroupButtonMoving)
  public
    procedure LoadBitmap; override;
  published

  end;

  { TFWOutSelectV }

  TFWOutSelectV = class(TFWGroupButtonMovingV)
  public
    procedure LoadBitmap; override;
  published

  end;

  { TFWOutAllV }

  TFWOutAllV = class(TFWBigGroupButtonMovingV)
  public
    procedure LoadBitmap; override;
  published

  end;

  { TFWInSelectV }

  TFWInSelectV = class(TFWGroupButtonMovingV)
  public
    procedure LoadBitmap; override;
  published

  end;

  { TFWInAllV }

  TFWInAllV = class(TFWBigGroupButtonMovingV)
  public
    procedure LoadBitmap; override;
  published

  end;

{$ENDIF}

implementation

uses {$IFDEF FPC}ObjInspStrConsts, lclstrconsts,
     {$ELSE}Consts, VDBConsts, {$ENDIF}
  {$IFDEF FPC}
  unite_messages,
  {$ELSE}
  unite_messages_delphi,
  {$ENDIF}
  fonctions_proprietes,
  Graphics,
  Forms;

{ TFWBigGroupButtonMovingV }

constructor TFWBigGroupButtonMovingV.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  GlyphSize := CST_SIZE_BUTTONS_MOVINGV;
  p_setControlCaption ( Self, '' );
  Height := CST_HEIGHT_BUTTONS_MOVINGV;
  Width := CST_WIDTH_BIG_BUTTONS_MOVINGV;
end;

{ TFWBigGroupButtonMoving }

constructor TFWBigGroupButtonMoving.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  GlyphSize := CST_SIZE_BUTTONS_MOVING;
  p_setControlCaption ( Self, '' );
  Height := CST_HEIGHT_BIG_BUTTONS_MOVING;
  Width := CST_WIDTH_BUTTONS_MOVING;
end;

{ TFWGroupButtonMovingV }

constructor TFWGroupButtonMovingV.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  GlyphSize := CST_SIZE_BUTTONS_MOVINGV;
  p_setControlCaption ( Self, '' );
  Height := CST_HEIGHT_BUTTONS_MOVINGV;
  Width := CST_WIDTH_BUTTONS_MOVINGV;
end;

{ TFWInSelectV }

procedure TFWInSelectV.LoadBitmap;
begin
  p_Load_Buttons_Appli(Glyph, CST_FWINSELECTV, Self);
end;

{ TFWInAllV }

procedure TFWInAllV.LoadBitmap;
begin
  p_Load_Buttons_Appli(Glyph, CST_FWINALLV, Self);
end;

{ TFWOutAllV }

procedure TFWOutAllV.LoadBitmap;
begin
  p_Load_Buttons_Appli(Glyph, CST_FWOUTALLV, Self);
end;

{ TFWOutSelectV }

procedure TFWOutSelectV.LoadBitmap;
begin
  p_Load_Buttons_Appli(Glyph, CST_FWOUTSELECTV, Self);
end;

{ TFWMedia }

procedure TFWMedia.Loaded;
begin
  LoadImageList( CST_FWMEDIABUTTONS, clBlack);
  with Images do
   Begin
    Width :=24;
    Height:=24;
   end;
  Inherited;
end;

{$IFNDEF FPC}
var
  Buttons_Appli_ResInstance: THandle = 0;

{$ENDIF}


{ TFWClose }


procedure TFWClose.Click;
begin
  if not assigned(OnClick) and (Owner is TCustomForm) then
    with Owner as TCustomForm do
    begin
      Close;
      Exit;
    end;
  inherited;

end;

constructor TFWClose.Create(AOwner: TComponent);
begin
  inherited;
  Width := CST_FWWIDTH_CLOSE_BUTTON;
end;

procedure TFWClose.LoadBitmap;
begin
  p_Load_Buttons_Appli(Glyph, CST_FWCLOSE, Self);
End;

procedure TFWClose.Loaded;
Begin
  inherited;
  p_setControlCaption ( Self, SCloseButton);
end;


{ TFWCancel }

procedure TFWCancel.LoadBitmap;
begin
  p_Load_Buttons_Appli(Glyph, CST_FWCANCEL, Self);
End;

procedure TFWCancel.Loaded;
Begin
  inherited;
  {$IFDEF FPC}
  p_setControlCaption ( Self, oiStdActDataSetCancel1Hint);
  {$ELSE}
  p_setControlCaption ( Self, SMsgDlgCancel);
  {$ENDIF}
end;


{ TFWOK }

procedure TFWOK.LoadBitmap;
begin
  p_Load_Buttons_Appli(Glyph, CST_FWOK, Self);
End;

procedure TFWOK.Loaded;
Begin
  inherited;
  {$IFDEF FPC}
  p_setControlCaption ( Self, oisOk2);
  {$ELSE}
  p_setControlCaption ( Self, SMsgDlgOK);
  {$ENDIF}
end;

{ TFWSearch }

procedure TFWSearch.LoadBitmap;
begin
  p_Load_Buttons_Appli(Glyph, CST_FWSEARCH, Self);
end;

{ TFWZoomOut }

procedure TFWZoomOut.LoadBitmap;
begin
  p_Load_Buttons_Appli(Glyph, CST_FWZOOMOUT, Self);
end;

{ TFWZoomIn }

procedure TFWZoomIn.LoadBitmap;
begin
  p_Load_Buttons_Appli(Glyph, CST_FWZOOMIN, Self);
end;

{ TFWFolder }

procedure TFWFolder.LoadBitmap;
begin
  p_Load_Buttons_Appli(Glyph, CST_FWFOLDER, Self);
end;

{ TFWTrash }

procedure TFWTrash.LoadBitmap;
begin
  p_Load_Buttons_Appli(Glyph, CST_FWTRASH, Self);
end;


{ TFWDate }

procedure TFWDate.LoadBitmap;
begin
  p_Load_Buttons_Appli(Glyph, CST_FWDATE, Self);
end;

{ TFWLoad }

procedure TFWLoad.LoadBitmap;
begin
  p_Load_Buttons_Appli(Glyph, CST_FWLOAD, Self);
End;
procedure TFWLoad.Loaded;
Begin
  inherited;
  {$IFDEF FPC}
  p_setControlCaption ( Self, oiStdActFileOpenHint );
  {$ENDIF}
end;

{ TFWDocument }

procedure TFWDocument.LoadBitmap;
begin
  p_Load_Buttons_Appli(Glyph, CST_FWDOCUMENT, Self);
end;

{ TFWDelete }

procedure TFWDelete.LoadBitmap;
begin
  p_Load_Buttons_Appli(Glyph, CST_FWDELETE, Self);
end;

{ TFWInsert }

procedure TFWInsert.LoadBitmap;
begin
  p_Load_Buttons_Appli(Glyph, CST_FWINSERT, Self);
End;
procedure tfwinsert.Loaded;
Begin
  inherited;
  {$IFDEF FPC}
  p_setControlCaption ( Self, ifsVK_INSERT );
  {$ELSE}
  p_setControlCaption ( Self, SInsertRecord );
  {$ENDIF}
end;

{ TFWAdd }
procedure TFWAdd.LoadBitmap;
begin
  p_Load_Buttons_Appli(Glyph, CST_FWINSERT, Self);
end;

{ TFWSaveAs }

procedure TFWSaveAs.LoadBitmap;
begin
  p_Load_Buttons_Appli(Glyph, CST_FWSAVEAS, Self);
End;
procedure TFWSAveAs.Loaded;
Begin
  inherited;
  {$IFDEF FPC}
  p_setControlCaption ( Self, oiStdActFileSaveAsHint );
  {$ENDIF}
end;

{ TFWQuit }

procedure TFWQuit.LoadBitmap;
begin
  p_Load_Buttons_Appli(Glyph, CST_FWQUIT, Self);
End;
procedure TFWQuit.Loaded;
Begin
  inherited;
  p_setControlCaption ( Self, SCloseButton );
end;


{ TFWerase }

procedure TFWErase.LoadBitmap;
begin
  p_Load_Buttons_Appli(Glyph, CST_FWERASE, Self);
End;

procedure TFWErase.Loaded;
Begin
  inherited;
  {$IFDEF FPC}
  p_setControlCaption ( Self, oisDelete );
  {$ELSE}
  //p_setControlCaption ( Self, SDeleteRecord;
  {$ENDIF}
end;

{ TFWPrint }

procedure TFWPrint.LoadBitmap;
begin
  p_Load_Buttons_Appli(Glyph, CST_FWPRINT, Self);
End;
procedure TFWPrint.Loaded;
Begin
  inherited;
  {$IFDEF FPC}
  p_setControlCaption ( Self, ifsVK_PRINT );
  {$ENDIF}
end;

{ TFWNext }

procedure TFWNext.LoadBitmap;
begin
  p_Load_Buttons_Appli(Glyph, CST_FWNEXT, Self);
end;

{ TFWPrior }

procedure TFWPrior.LoadBitmap;
begin
  p_Load_Buttons_Appli(Glyph, CST_FWPRIOR, Self);
end;

{ TFWRefresh }

procedure TFWRefresh.LoadBitmap;
begin
  p_Load_Buttons_Appli(Glyph, CST_FWREFRESH, Self);
end;

{ TFWPreview }

procedure TFWPreview.LoadBitmap;
begin
  p_Load_Buttons_Appli(Glyph, CST_FWPREVIEW, Self);
end;

{ TFWInit }

procedure TFWInit.LoadBitmap;
begin
  p_Load_Buttons_Appli(Glyph, CST_FWINIT, Self);
end;

{ TFWConfig }

procedure TFWConfig.LoadBitmap;
begin
  p_Load_Buttons_Appli(Glyph, CST_FWCONFIG, Self);
end;

{ TFWImport }

procedure TFWImport.LoadBitmap;
begin
  p_Load_Buttons_Appli(Glyph, CST_FWIMPORT, Self);
end;

{ TFWExport }

procedure TFWExport.LoadBitmap;
begin
  p_Load_Buttons_Appli(Glyph, CST_FWEXPORT, Self);
end;

{ TFWCopy }


procedure TFWCopy.LoadBitmap;
begin
  p_Load_Buttons_Appli(Glyph, CST_FWCOPY, Self);
End;
procedure TFWCopy.Loaded;
Begin
  inherited;
  {$IFDEF FPC}
  p_setControlCaption ( Self, oiStdActEditCopyShortHint );
  {$ENDIF}
end;


{$IFDEF GROUPVIEW}

{ TFWOutSelect }

procedure TFWOutSelect.LoadBitmap;
begin
  p_Load_Buttons_Appli(Glyph, CST_FWOUTSELECT, Self);
end;

{ TFWBasket }

procedure TFWBasket.LoadBitmap;
begin
  p_Load_Buttons_Appli(Glyph, CST_FWBASKET, Self);
End;

procedure TFWBasket.Loaded;
Begin
  inherited;
  {$IFDEF FPC}
  p_setControlCaption ( Self, oisUndo );
  {$ELSE}
  p_setControlCaption ( Self, Gs_GROUPVIEW_Basket );
  {$ENDIF}
end;

{ TFWRecord }


procedure TFWRecord.LoadBitmap;
begin
  p_Load_Buttons_Appli(Glyph, CST_FWOK, Self);
End;

procedure TFWRecord.Loaded;
Begin
  inherited;
  {$IFDEF FPC}
  p_setControlCaption ( Self, oisRecord );
  {$ELSE}
  p_setControlCaption ( Self, Gs_GROUPVIEW_Record );
  {$ENDIF}
end;


{ TFWOutAll }

procedure TFWOutAll.LoadBitmap;
begin
  p_Load_Buttons_Appli(Glyph, CST_FWOUTALL, Self);
end;

{ TFWInSelect }

procedure TFWInSelect.LoadBitmap;
begin
  p_Load_Buttons_Appli(Glyph, CST_FWINSELECT, Self);
end;

{ TFWInAll }

procedure TFWInAll.LoadBitmap;
begin
  p_Load_Buttons_Appli(Glyph, CST_FWINALL, Self);
end;

{ TFWGroupButtonActions }

constructor TFWGroupButtonActions.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := CST_WIDTH_BUTTONS_ACTIONS;
  Height := CST_HEIGHT_BUTTONS_ACTIONS;
end;

{ TFWGroupButtonMoving }

constructor TFWGroupButtonMoving.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  GlyphSize := CST_SIZE_BUTTONS_MOVING;
  p_setControlCaption ( Self, '' );
  Height := CST_HEIGHT_BUTTONS_MOVING;
  Width := CST_WIDTH_BUTTONS_MOVING;
end;
{$ENDIF}

initialization
{$IFDEF VERSIONS}
p_ConcatVersion(gVer_buttons_appli);
{$ENDIF}
{$IFDEF MEMBUTTONS}
{$IFDEF FPC}
{$I u_buttons_appli.lrs}
{$ENDIF}
{$ENDIF}

end.
