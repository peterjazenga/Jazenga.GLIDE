unit u_buttons_speed;

{$I ..\DLCompilers.inc}
{$I ..\extends.inc}
{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}

interface

uses
{$IFDEF FPC}
   lresources, JvXPButtons,
{$ELSE}
   Windows, Messages, JvXPButtons,
{$ENDIF}
  Classes,
{$IFDEF VERSIONS}
   fonctions_version,
{$ENDIF}
  Controls, u_buttons_defs,
   Graphics,
  Menus, Buttons;

{$IFDEF VERSIONS}
const
    gVer_buttons_speed : T_Version = ( Component : 'Customized Buttons' ;
                                       FileUnit : 'u_buttons_speed' ;
                                       Owner : 'Matthieu Giroux' ;
                                       Comment : 'Customized Buttons components.' ;
                                       BugsStory : '1.0.0.0 : Compiled.'+#13#10+
                                                   '0.9.0.0 : To test.';
                                       UnitType : 3 ;
                                       Major : 1 ; Minor : 0 ; Release : 0 ; Build : 0 );
{$ENDIF}

type

    { TSBButton }

    TSBButton = class ( TSpeedButton, IFWButton )
      public
       constructor Create ( AOwner : TComponent ) ; override;
      published
       property Height default 25;
       property Width default 25;
       property Glyph stored False;
     End;

    { TSBMiniButton }

    TSBMiniButton = class ( TSpeedButton, IFWButton )
      public
       constructor Create ( AOwner : TComponent ) ; override;
      published
       property Height default 17;
       property Width default 17;
       property Glyph stored False;
     End;


   { TSBClose }

  TSBClose = class ( TSBButton )
     public
      constructor Create ( AOwner : TComponent ) ; override;
      procedure Loaded; override;
      procedure Click; override;
     published

      property Width default CST_FWWIDTH_CLOSE_BUTTON ;
    End;

{ TSBOK }
   TSBOK = class ( TSBButton )
      public
       procedure Loaded; override;
      published
       
     End;

{ TSBInsert }
   TSBInsert = class ( TSBButton )
      public
       procedure Loaded; override;
      published
       
     End;
   { TSBAdd }
   TSBAdd = class ( TSBButton )
      public
       procedure Loaded; override;
     End;
   { TSBMAdd }
   TSBMAdd = class ( TSBButton )
      public
       procedure Loaded; override;
     End;


 { TSBDelete }
   TSBDelete = class ( TSBButton )
      public
       procedure Loaded; override;
     End;
 { TSBMDelete }
   TSBMDelete = class ( TSBMiniButton )
      public
       procedure Loaded; override;
     End;
  { TSBDocument }
     TSBDocument = class ( TSBButton )
        public
         procedure Loaded; override;
       End;


   { TSBDate }
    TSBDate = class ( TSBButton )
       public
        procedure Loaded; override;
      End;
  { TSBMDate }
   TSBMDate = class ( TSBButton )
      public
       procedure Loaded; override;
     End;

{ TSBQuit }
   TSBQuit = class ( TSBMiniButton )
      public
       procedure Loaded; override;
     End;

{ TSBErase }
   TSBErase = class ( TSBButton )
      public
       procedure Loaded; override;
     End;

{ TSBSaveAs }
   TSBSaveAs = class ( TSBButton )
      public
       procedure Loaded; override;
     End;

   { TSBLoad }
      TSBLoad = class ( TSBMiniButton )
         public
          procedure Loaded; override;
        End;

{ TSBPrint }
   TSBPrint = class ( TSBMiniButton )
      public
       procedure Loaded; override;

      published
       
     End;

{ TSBCancel }
   TSBCancel = class ( TSBButton )
      public
       procedure Loaded; override;
     End;


{ TSBPreview }
   TSBPreview = class ( TSBMiniButton )
      public
       procedure Loaded; override;
     End;

{ TSBNext }
   TSBNext = class ( TSBButton )
      public
       procedure Loaded; override;
     End;

   { TSBPrior }
      TSBPrior= class ( TSBButton )
         public
          procedure Loaded; override;
        End;
{ TSBPrior }
   TSBRefresh= class ( TSBButton )
      public
       procedure Loaded; override;
     End;

{ TSBCopy }
   TSBCopy = class ( TSBMiniButton )
      public
       procedure Loaded; override;
     End;

{ TSBInit }
   TSBInit = class ( TSBButton )
      public
       procedure Loaded; override;
     End;

{ TSBConfig }
   TSBConfig = class ( TSBButton )
      public
       procedure Loaded; override;
     End;

{ TSBImport }
   TSBImport = class ( TSBButton )
      public
       procedure Loaded; override;
     End;
{ TSBTrash }
   TSBTrash = class ( TSBButton )
      public
       procedure Loaded; override;
     End;

{ TSBExport }
   TSBExport = class ( TSBButton )
      public
       procedure Loaded; override;
     End;

{$IFDEF GROUPVIEW}

{ TSBGroupButton }

    { TSBGroupButtonActions }

    TSBGroupButtonActions = class ( TSBButton )
     public
      constructor Create ( AOwner : TComponent ) ; override;
     published
      property Width  default CST_WIDTH_BUTTONS_ACTIONS;
      property Height default CST_HEIGHT_BUTTONS_ACTIONS;
    end;


   { TSBBasket }

   TSBBasket = class ( TSBGroupButtonActions )
      public
       procedure Loaded; override;
     End;

   { TSBRecord }

   TSBRecord = class ( TSBGroupButtonActions )
      public
       procedure Loaded; override;
     End;

   { TSBGroupButtonMoving }

   TSBGroupButtonMoving = class ( TSBButton )
   public
    constructor Create ( AOwner : TComponent ) ; override;
   published
    property Width  default CST_WIDTH_BUTTONS_MOVING;
    property Height default CST_HEIGHT_BUTTONS_MOVING;
   end;
   { TSBOutSelect }
    TSBOutSelect = class ( TSBGroupButtonMoving )
       public
        procedure Loaded; override;
      End;

   { TSBOutAll }


   TSBOutAll = class ( TSBGroupButtonMoving )
      public
       procedure Loaded; override;
     End;

{ TSBInSelect }
   TSBInSelect = class ( TSBGroupButtonMoving )
      public
       procedure Loaded; override;
     End;

{ TSBInAll }
   TSBInAll = class ( TSBGroupButtonMoving )
      public
       procedure Loaded; override;
     End;

{$ENDIF}

implementation

uses {$IFDEF FPC}ObjInspStrConsts,lclstrconsts,
     {$ELSE}Consts, VDBConsts, {$ENDIF}
  {$IFDEF FPC}
  unite_messages,
  {$ELSE}
  unite_messages_delphi,
  {$ENDIF}
     fonctions_images,
     Forms ;


{$IFNDEF FPC}
var Buttons_Appli_ResInstance             : THandle      = 0 ;
{$ENDIF}


{ TSBMiniButton }

constructor TSBMiniButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Height:=17;
  Width :=17;
end;

{ TSBButton }

constructor TSBButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Height:=25;
  Width :=25;
end;



{ TSBTrash }

procedure TSBTrash.Loaded;
begin
  p_Load_Bitmap_Appli ( Glyph, CST_FWTRASH, Self );
  inherited Loaded;
end;


{ TSBDate }

procedure TSBDate.Loaded;
begin
  p_Load_Bitmap_Appli ( Glyph, CST_FWDATE, Self );
  inherited Loaded;
end;

{ TSBMDate }

procedure TSBMDate.Loaded;
begin
  p_Load_Bitmap_Appli ( Glyph, CST_FWDATE, Self );
  p_ChangeTailleBitmap ( Glyph,16,16,True);
  inherited Loaded;
end;

{ TSBLoad }

procedure TSBLoad.Loaded;
begin
  p_Load_Bitmap_Appli ( Glyph, CST_FWLOAD, Self );
  inherited Loaded;
  {$IFDEF FPC}
  p_setControlCaption ( Self, oiStdActFileOpenHint );
  {$ENDIF}
end;

{ TSBDocument }

procedure TSBDocument.Loaded;
begin
  p_Load_Bitmap_Appli ( Glyph, CST_FWDOCUMENT, Self );
  inherited Loaded;
end;

{ TSBDelete }

procedure TSBDelete.Loaded;
begin
  p_Load_Bitmap_Appli ( Glyph, CST_FWDELETE, Self );
  inherited Loaded;
end;

{ TSBMDelete }

procedure TSBMDelete.Loaded;
begin
  p_Load_Bitmap_Appli ( Glyph, CST_FWDELETE, Self );
  p_ChangeTailleBitmap ( Glyph,16,16,True);
  inherited Loaded;
end;

{ TSBClose }


procedure TSBClose.Click;
begin
  if not assigned ( OnClick )
  and ( Owner is TCustomForm ) then
    with Owner as TCustomForm do
     Begin
      Close;
      Exit;
     End;
  inherited;

end;

constructor TSBClose.Create(AOwner: TComponent);
begin
  inherited;
  Width := CST_FWWIDTH_CLOSE_BUTTON;
end;

procedure TSBClose.Loaded;
begin
  p_Load_Bitmap_Appli ( Glyph, CST_FWCLOSE, Self );
  inherited Loaded;
  {$IFDEF FPC}
  p_setControlCaption ( Self, SCloseButton );
  {$ENDIF}
end;

{ TSBCancel }

procedure TSBCancel.Loaded;
begin
  p_Load_Bitmap_Appli ( Glyph, CST_FWCANCEL, Self );
  inherited Loaded;
  {$IFDEF FPC}
  p_setControlCaption ( Self, oiStdActDataSetCancel1Hint );
  {$ENDIF}
end;


{ TSBOK }

procedure TSBOK.Loaded;
begin
  p_Load_Bitmap_Appli ( Glyph, CST_FWOK, Self );
  inherited Loaded;
  {$IFDEF FPC}
  p_setControlCaption ( Self, oisOk2 );
  {$ENDIF}
end;

{ TSBInsert }

procedure TSBInsert.Loaded;
begin
  p_Load_Bitmap_Appli ( Glyph, CST_FWINSERT, Self );
  inherited Loaded;
  {$IFDEF FPC}
  p_setControlCaption ( Self, ifsVK_INSERT );
  {$ENDIF}
end;
{ TSBAdd }
procedure TSBAdd.Loaded;
begin
  p_Load_Bitmap_Appli ( Glyph, CST_FWINSERT, Self );
  inherited Loaded;
end;
{ TSBMAdd }
procedure TSBMAdd.Loaded;
begin
  p_Load_Bitmap_Appli ( Glyph, CST_FWINSERT, Self );
  p_ChangeTailleBitmap ( Glyph,16,16,True);
  inherited Loaded;
end;

{ TSBSaveAs }


procedure TSBSaveAs.Loaded;
begin
  p_Load_Bitmap_Appli ( Glyph, CST_FWSAVEAS, Self );
  inherited Loaded;
  {$IFDEF FPC}
  p_setControlCaption ( Self, oiStdActFileSaveAsHint );
  {$ENDIF}
end;

{ TSBQuit }

procedure TSBQuit.Loaded;
begin
  p_Load_Bitmap_Appli ( Glyph, CST_FWQUIT, Self );
  inherited Loaded;
  {$IFDEF FPC}
  p_setControlCaption ( Self, SCloseButton );
  {$ENDIF}
end;


{ TSBerase }

procedure TSBErase.Loaded;
begin
  p_Load_Bitmap_Appli ( Glyph, CST_FWERASE, Self );
  inherited Loaded;
  {$IFDEF FPC}
  p_setControlCaption ( Self, oisDelete );
  {$ENDIF}
end;

{ TSBPrint }

procedure TSBPrint.Loaded;
begin
  p_Load_Bitmap_Appli ( Glyph, CST_FWPRINT, Self );
  inherited Loaded;
  {$IFDEF FPC}
  p_setControlCaption ( Self, ifsVK_PRINT );
  {$ENDIF}
end;

{ TSBNext }

procedure TSBNext.Loaded;
begin
  p_Load_Bitmap_Appli ( Glyph, CST_FWNEXT, Self );
  inherited Loaded;
end;
{ TSBPrior }

procedure TSBPrior.Loaded;
begin
  p_Load_Bitmap_Appli ( Glyph, CST_FWPRIOR, Self );
  inherited Loaded;
end;

{ TSBRefresh }

procedure TSBRefresh.Loaded;
begin
  p_Load_Bitmap_Appli ( Glyph, CST_FWREFRESH, Self );
  inherited Loaded;
end;

{ TSBPreview }

procedure TSBPreview.Loaded;
begin
  p_Load_Bitmap_Appli ( Glyph, CST_FWPREVIEW, Self );
  inherited Loaded;
end;

{ TSBInit }

procedure TSBInit.Loaded;
begin
  p_Load_Bitmap_Appli ( Glyph, CST_FWINIT, Self );
  inherited Loaded;
end;

{ TSBConfig }

procedure TSBConfig.Loaded;
begin
  p_Load_Bitmap_Appli ( Glyph, CST_FWCONFIG, Self );
  inherited Loaded;
end;

{ TSBImport }

procedure TSBImport.Loaded;
begin
  p_Load_Bitmap_Appli ( Glyph, CST_FWIMPORT, Self );
  inherited Loaded;
end;

{ TSBExport }

procedure TSBExport.Loaded;
begin
  p_Load_Bitmap_Appli ( Glyph, CST_FWEXPORT, Self );
  inherited Loaded;
end;

{ TSBCopy }

procedure TSBCopy.Loaded;
begin
  p_Load_Bitmap_Appli ( Glyph, CST_FWCOPY, Self );
  inherited Loaded;
  {$IFDEF FPC}
  p_setControlCaption ( Self, oiStdActEditCopyShortHint );
  {$ENDIF}
end;


{$IFDEF GROUPVIEW}

{ TSBOutSelect }

procedure TSBOutSelect.Loaded;
begin
  p_Load_Bitmap_Appli ( Glyph, CST_FWOUTSELECT, Self );
  inherited Loaded;
end;

{ TSBBasket }

procedure TSBBasket.Loaded;
begin
  p_Load_Bitmap_Appli ( Glyph, CST_FWBASKET, Self );
  inherited Loaded;
  {$IFDEF FPC}
  p_setControlCaption ( Self, oisUndo );
  {$ENDIF}
end;

{ TSBRecord }

procedure TSBRecord.Loaded;
begin
  p_Load_Bitmap_Appli ( Glyph, CST_FWOK, Self );
  inherited Loaded;
  {$IFDEF FPC}
  p_setControlCaption ( Self, oisRecord );
  {$ENDIF}
end;


{ TSBOutAll }

procedure TSBOutAll.Loaded;
begin
  p_Load_Bitmap_Appli ( Glyph, CST_FWOUTALL, Self );
  inherited Loaded;
end;

{ TSBInSelect }

procedure TSBInSelect.Loaded;
begin
  p_Load_Bitmap_Appli ( Glyph, CST_FWINSELECT, Self );
  inherited Loaded;
end;

{ TSBInAll }

procedure TSBInAll.Loaded;
begin
  p_Load_Bitmap_Appli ( Glyph, CST_FWINALL, Self );
  inherited Loaded;
end;

{ TSBGroupButtonActions }

constructor TSBGroupButtonActions.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := CST_WIDTH_BUTTONS_ACTIONS;
  Height := CST_HEIGHT_BUTTONS_ACTIONS;
end;

{ TSBGroupButtonMoving }

constructor TSBGroupButtonMoving.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := CST_WIDTH_BUTTONS_MOVING;
  Height := CST_HEIGHT_BUTTONS_MOVING;
  Caption := '';
end;


{$ENDIF}



initialization
{$IFDEF VERSIONS}
  p_ConcatVersion ( gVer_buttons_speed  );
{$ENDIF}

end.

