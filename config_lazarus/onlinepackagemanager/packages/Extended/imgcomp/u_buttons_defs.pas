unit u_buttons_defs;

{$I ..\DLCompilers.inc}
{$I ..\extends.inc}
{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}

interface

uses
{$IFNDEF FPC}
  Windows, Messages,JvXPButtons,
{$ELSE}
  JvXPButtons,
{$ENDIF}
  Classes,
{$IFDEF VERSIONS}
   fonctions_version,
{$ENDIF}
  Controls,
  Graphics,
  u_extcomponent, ImgList;


const
{$IFDEF VERSIONS}
    gVer_buttons_defs : T_Version = ( Component : 'Customized Buttons' ;
                                       FileUnit : 'u_buttons_appli' ;
                                       Owner : 'Matthieu Giroux' ;
                                       Comment : 'Customized Buttons components.' ;
                                       BugsStory : '1.0.1.0 : Delphi 2009 and Lazarus compatible.'+ #13#10
                                                 + '1.0.0.5 : Creating LoadBitmap.'+ #13#10
                                                 + '1.0.0.4 : Better Popup.'+ #13#10
                                                 + '1.0.0.3 : Testing Popup.'+ #13#10
                                                 + '1.0.0.2 : Date and Folder Buttons.'+ #13#10
                                                 + '1.0.0.1 : UTF 8.'+ #13#10
                                                 + '1.0.0.0 : Version OK.'+ #13#10
                                                 + '0.8.0.1 : Group view buttons better.'+ #13#10
                                                 + '0.8.0.0 : To test.';
                                       UnitType : 3 ;
                                       Major : 1 ; Minor : 0 ; Release : 1 ; Build : 0 );
{$ENDIF}
  CST_FWWIDTH_CLOSE_BUTTON = 80 ;
  CST_SIZE_BUTTONS_MOVING  = 60;
  CST_WIDTH_BUTTONS_MOVING  = 60;
  CST_HEIGHT_BUTTONS_MOVING = 40;
  CST_HEIGHT_BIG_BUTTONS_MOVING = 46;
  CST_SIZE_BUTTONS_MOVINGV  = 44;
  CST_WIDTH_BUTTONS_MOVINGV  = 44;
  CST_WIDTH_BIG_BUTTONS_MOVINGV  = 48;
  CST_HEIGHT_BUTTONS_MOVINGV = 44;
  CST_WIDTH_BUTTONS_ACTIONS  = 120;
  CST_HEIGHT_BUTTONS_ACTIONS = 20;
  CST_IMAGE_SOFT_BITMAP = '.bmp';
  CST_IMAGES_SOFT_EXTENSIONS : array [ 0 .. {$IFDEF FPC}2{$ELSE}0{$ENDIF} ] of String  = (CST_IMAGE_SOFT_BITMAP{$IFDEF FPC},'.xpm','.png'{$ENDIF});
  CST_FWCANCEL='tfwcancel';
  CST_FWCLOSE='tfwclose';
  CST_FWMEDIABUTTONS='tfwmediabuttons';
  CST_FWOK='tfwok';
  CST_FWBASKET = 'tfwbasket';
  CST_FWDATE = 'tfwdate';
  CST_FWDOCUMENT = 'tfwdocument';
  CST_FWFOLDER = 'tfwfolder';
  CST_FWINSERT = 'tfwinsert';
  CST_FWDELETE = 'tfwdelete';
  CST_FWIMPORT = 'tfwimport';
  CST_FWEXPORT = 'tfwexport';
  CST_FWCOPY = 'tfwcopy';
  CST_FWQUIT = 'tfwquit';
  CST_FWERASE = 'tfwerase';
  CST_FWSAVEAS = 'tfwsaveas';
  CST_FWPRINT = 'tfwprint';
  CST_FWPREVIEW = 'tfwpreview';
  CST_FWNEXT = 'tfwnext';
  CST_FWREFRESH = 'tfwrefresh';
  CST_FWPRIOR = 'tfwprior';
  CST_FWINIT = 'tfwinit';
  CST_FWCONFIG = 'tfwconfig';
  CST_FWLOAD = 'tfwload';
  CST_FWSEARCH = 'tfwsearch';
  CST_FWZOOMIN = 'tfwzoomin';
  CST_FWZOOMOUT = 'tfwzoomout';
  CST_FWTRASH = 'tfwtrash';
  CST_ExtSendMails = 'textsendmails';
{$IFDEF GROUPVIEW}
CST_FWOUTSELECT = 'tfwoutselect';
CST_FWINSELECT = 'tfwinselect';
CST_FWOUTALL = 'tfwoutall';
CST_FWINALL = 'tfwinall';
CST_FWOUTSELECTV = 'tfwoutselectv';
CST_FWINSELECTV = 'tfwinselectv';
CST_FWOUTALLV = 'tfwoutallv';
CST_FWINALLV = 'tfwinallv';
{$ENDIF}


procedure p_Load_Buttons_Appli ( const FGLyph : {$IFDEF USEJVCL}TJvPicture{$ELSE}TPicture{$ENDIF USEJVCL}; const as_Resource : String ; const acon_control :TControl);
procedure p_Load_Bitmap_Appli ( const FGLyph : TBitmap; const as_Resource : String ; const acon_control :TControl );
procedure p_setControlCaption ( const AControl : TControl ; const as_Caption : String );

type
   IFWButton = interface
   ['{620AE27F-98C1-8A6D-E54F-FE57A16207E5}']
       procedure Paint;
   end;

    { TFWXPButton }

    TFWXPButton = class ( TJvXPButton, IFWButton )
      private
       FColor           ,
       FGlyphSize: Integer;
       FColorFrameFocus : TColor;
       FOnPopup : TNotifyEvent;
      protected
       procedure HookMouseEnter; override;
       procedure HookMouseLeave; override;
       procedure SetGlyphSize(AValue: Integer); virtual;
       procedure AdaptGlyph (const ASize : Integer ); virtual;
       procedure LoadBitmap; virtual;
     public
      constructor Create ( AOwner : TComponent ) ; override;
      procedure Click; override;
      procedure Loaded; override;
     published
       property ColorFrameFocus : TColor read FColorFrameFocus write FColorFrameFocus default clCream;
       property GlyphSize : Integer read FGlyphSize write SetGlyphSize default 0;
       property OnPopup : TNotifyEvent read FOnPopup write FOnPopup;
     End;
    { TFWButton }

    TFWButton = class ( TFWXPButton )
      public
       constructor Create ( AOwner : TComponent ) ; override;
      published
       property Glyph stored False;
       property Height default 26;
       property Width default 80;
     End;

    { TFWButtonList }

    TFWButtonList = class ( TFWXPButton )
      private
       FImagesList : TCustomImageList;
       FImageSize  ,
       FImageIndex : Integer;
       procedure p_setImagesList ( const AValue : TCustomImageList );
       procedure p_setImageIndex ( const AValue : Integer );
      public
       constructor Create ( AOwner : TComponent ) ; override;
       procedure LoadBitmap; override;
      published
       property Images: TCustomImageList read FImagesList write p_setImagesList;
       property ImageIndex: Integer read FImageIndex write p_setImageIndex default -1;
       property Glyph stored False;
       property Height default 18;
       property Width default 18;
    End;

    { TFWButtonGlyphs }

    TFWButtonGlyphs = class(TFWButtonList)
    private
      FGlyphs : TImageList;
    public
     constructor Create ( AOwner : TComponent ) ; override;
     procedure LoadImageList(const as_Resource: String;
       const AMaskColor: TColor);
    published
      property Images stored False;
      property Height default 24;
      property Width default 24;
      property GlyphSize default 24;
    end;

implementation

uses {$IFDEF FPC}
     LazFileUtils, unite_messages,
     {$ELSE}
     Consts, VDBConsts, unite_messages_delphi,
     {$ENDIF}
     fonctions_images,
     Dialogs,
     fonctions_system, sysutils,
     fonctions_proprietes,
     fonctions_string;


{$IFNDEF FPC}
var Buttons_Appli_ResInstance             : THandle      = 0 ;
{$ENDIF}

// auto caption
procedure p_setControlCaption ( const AControl : TControl ; const as_Caption : String );
Begin
  with AControl do
    if  name = fs_getComponentProperty ( AControl, CST_PROPERTY_CAPTION ) Then
      p_setComponentProperty ( AControl, CST_PROPERTY_CAPTION, as_Caption );
end;

// procedure p_Load_Bitmap_Appli
// loads a picture into a Button with Bitmap
procedure p_Load_Bitmap_Appli ( const FGLyph : TBitmap; const as_Resource : String ; const acon_control :TControl );
var ls_imagePath : String;
begin
  {$IFNDEF MEMBUTTONS}
  if csDesigning in acon_control.ComponentState Then
    Begin
  {$ENDIF}
    {$IFDEF FPC}
      FGlyph.LoadFromLazarusResource( as_Resource );
    {$ELSE}
      if ( Buttons_Appli_ResInstance = 0 ) Then
        Buttons_Appli_ResInstance:= FindResourceHInstance(HInstance);
      FGlyph.LoadFromResourceName(Buttons_Appli_ResInstance, as_Resource );
    {$ENDIF}
    {$IFNDEF MEMBUTTONS}
    end
   else
    Begin
     ls_imagePath := fs_getImagesSoftDir + as_Resource + CST_IMAGE_SOFT_BITMAP;
     if FileExistsUTF8( ls_imagePath ) Then
       try
        FGLyph.LoadFromFile( ls_imagePath );
      Except
      end
     else
      writeln( fs_RemplaceMsg(GS_SOFT_IMAGE_NOT_FOUND, [ls_imagePath]));

    {$ENDIF}
    end;
  if not ( csCreating in acon_control.ControlState ) then
    acon_control.Invalidate;
end;


// procedure p_Load_Buttons_Appli
// loads a picture into a Button with Picture
procedure p_Load_Buttons_Appli ( const FGLyph : {$IFDEF USEJVCL}TJvPicture{$ELSE}TPicture{$ENDIF USEJVCL}; const as_Resource : String ; const acon_control :TControl);
var n : Integer;
    lb_Found : Boolean;
    ls_imagePath : String;
begin
  with FGLyph{$IFNDEF FPC}.Bitmap{$ENDIF} do
   Begin
    {$IFDEF FPC}
    Clear;
    {$ELSE}
    FreeImage;
    {$ENDIF}
    {$IFNDEF MEMBUTTONS}
    if csDesigning in acon_control.ComponentState Then
      Begin
    {$ENDIF}
      {$IFDEF FPC}
        LoadFromLazarusResource(as_Resource);
        FGLyph.Bitmap.TransparentMode:=tmAuto;
        FGLyph.Bitmap.Transparent:=True;
      {$ELSE}
        if ( Buttons_Appli_ResInstance = 0 ) Then
          Buttons_Appli_ResInstance:= FindResourceHInstance(HInstance);
        LoadFromResourceName(Buttons_Appli_ResInstance, as_Resource );
      {$ENDIF}
      {$IFNDEF MEMBUTTONS}
      end
     Else
      Begin
        lb_Found := False;
        for n := high ( CST_IMAGES_SOFT_EXTENSIONS ) downto 0 do
         Begin
          ls_imagePath := fs_getImagesSoftDir + as_Resource + CST_IMAGES_SOFT_EXTENSIONS [ n ];

         if FileExistsUTF8( ls_imagePath ) Then
           try
            LoadFromFile( ls_imagePath );
            lb_Found := True;
            Break;

           Except
           end;
         end;
        if not lb_Found
         then
           writeln( fs_RemplaceMsg(GS_SOFT_IMAGE_NOT_FOUND, [ls_imagePath]));

      end;
      {$ENDIF}

    End;
  acon_control.Invalidate;
end;

{ TFWButtonGlyphs }

procedure TFWButtonGlyphs.LoadImageList(const as_Resource : String; const AMaskColor : TColor );
var ABitmap:TBitmap;
begin
  ABitmap:=TBitmap.Create;
  try
    p_Load_Bitmap_Appli(ABitmap, as_Resource, Self);
    Images.AddMasked(ABitmap,AMaskColor);
  finally
    {$IFNDEF FPC}
    ABitmap.Dormant;
    {$ENDIF}
    ABitmap.FreeImage;
    ABitmap.Destroy;
  end;
end;



constructor TFWButtonGlyphs.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fGlyphs:=TImageList.Create(Self); //will be destroyed with Self
  Images:=FGlyphs;
  Height:=24;
  Width :=24;
  GlyphSize:=24;
end;

{ TFWButtonList }

procedure TFWButtonList.p_setImagesList(const AValue: TCustomImageList);
begin
  if FImagesList <> AValue Then
    Begin
     FImagesList:=AValue;
     AdaptGlyph(FGlyphSize);
    end;
end;

procedure TFWButtonList.p_setImageIndex(const AValue: Integer);
begin
  if FImageIndex <> AValue Then
    Begin
     FImageIndex:=AValue;
     AdaptGlyph(FGlyphSize);
    end;
end;

constructor TFWButtonList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FImageSize:=0;
  FImageIndex:=-1;
  FImagesList:=nil;
  Height:=18;
  Width :=18;
end;

procedure TFWButtonList.LoadBitmap;
begin
  if assigned ( FImagesList )
  and ( FImageIndex >= 0 )
  and ( FImageIndex < FImagesList.Count )
   then FImagesList.GetBitmap ( FImageIndex , Glyph.Bitmap )
   Else Glyph.Bitmap.Assign(nil);
end;

{ TFWButton }

constructor TFWButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGlyphSize:=0;
  Height:=25;
  Width :=80;
end;


{ TFWXPButton }

procedure TFWXPButton.Loaded;
begin
  AdaptGlyph(FGlyphSize);
  inherited Loaded;
end;

procedure TFWXPButton.AdaptGlyph(const ASize: Integer);
begin
  if ( csCreating in ControlState )
  or ( Parent = nil ) Then
    Exit;
  BeginUpdate;
  LoadBitmap;
  with Glyph,Bitmap do
   if  ( ASize  > 0 )
   and ( Handle <> 0 )
   and (( ASize < Height ) or ( ASize < Width )) Then
    Begin
      p_ChangeTailleBitmap(Bitmap,ASize,Asize,True);
      TransparentMode:=tmAuto;
      Transparent:=True;
    end;
  EndUpdate;
  Invalidate;
end;

procedure TFWXPButton.HookMouseEnter;
begin
  FColor:=Color;
  Color := FColorFrameFocus;
  inherited;
end;

procedure TFWXPButton.HookMouseLeave;
begin
  Color := FColor;
  inherited;
end;

procedure TFWXPButton.SetGlyphSize(Avalue: Integer);
begin
  if Avalue <> FGlyphSize Then
   Begin
     FGlyphSize:=Avalue;
     AdaptGlyph(FGlyphSize);
   end;
end;

procedure TFWXPButton.Click;
begin
  fb_ShowPopup (Self,PopUpMenu,OnContextPopup,FOnPopup);
  inherited Click;
end;

procedure TFWXPButton.LoadBitmap;
begin
  // can load no picture
end;

constructor TFWXPButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColorFrameFocus:=clCream;
  FOnPopup := nil;
  FGlyphSize := 0;
end;


{$IFDEF VERSIONS}
initialization
  p_ConcatVersion ( gVer_buttons_defs  );
{$ENDIF}
end.

