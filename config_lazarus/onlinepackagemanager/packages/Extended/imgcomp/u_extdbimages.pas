unit u_extdbimages;

interface

{$i ..\extends.inc}
{$IFDEF FPC}
{$Mode Delphi}
{$ENDIF}

uses
{$IFDEF VERSIONS}
  fonctions_version,
{$ENDIF}
  Classes,
  U_ExtCustomImages,
  U_ExtImage,
  Controls,
{$IFDEF TNT}
     TntExtCtrls, TntStdCtrls,
{$ELSE}
     ExtCtrls, StdCtrls,
{$ENDIF}
     u_extcomponent,
     u_framework_components,
     DB, ExtDlgs,
     DBCtrls;

const
  CST_DEFAULT_PANEL_WIDTH = 150;
  CST_DEFAULT_PANEL_HEIGHT = 100;
  CST_DEFAULT_PER_LINE = 4;
{$IFDEF VERSIONS}
  gVer_TExtImages : T_Version = ( Component : 'Composant TExtImages' ;
                                  FileUnit : 'U_ExtImages' ;
                                  Owner : 'Matthieu Giroux' ;
                                  Comment : 'Présentation d''images de tous types avec résumé intégré dans le répertoire.' ;
                                  BugsStory : '1.0.0.0 : Scrolling and working.' + #13#10 +
                                              '0.9.0.0 : Creating from TExtImage.';
                                  UnitType : 3 ;
                                  Major : 1 ; Minor : 0 ; Release : 0 ; Build : 0 );

{$ENDIF}

type
  TExtPanelDBImage = class;
  TExtCollectionDBImages = class;

 { TExtMapImageIndex }

  { TExtPanelDBImage }

  TExtPanelDBImage = class(TExtCustomPanelImage)
  private
    FKey : Variant;
    function  BeginModfiy : Boolean;
    procedure EndModify( const ABookmark : {$IFDEF WITH_TBOOKMARK}TBookMark{$ELSE}TBookMarkStr{$ENDIF} );
  protected
    function CreateMemo:{$IFDEF TNT}TTntCustomMemo{$ELSE}TCustomMemo{$ENDIF}; override;
    function CreateImage: TExtImage; override;
  public
    procedure InitComponentsPanelLine; override;
    procedure OnClickImage ( Sender : TObject ); virtual;
    procedure OnQuitMemo ( Sender : TObject ); virtual;
    procedure LoadFromStream ( const astream : TStream ); virtual;
    function  LoadFromFile   ( const afile   : String ):Boolean; virtual;
    function  SavetoFile   ( const afile   : String ):Boolean; virtual;
  End;

  TExtPanelDBImageClass = class of TExtPanelDBImage;

  { TExtCollectionImages }

  TExtCollectionDBImages = class(TExtCustomCollectionImages)
  protected
    function MemoLineCreate:{$IFDEF TNT}TTntCustomMemo{$ELSE}TCustomMemo{$ENDIF}; override;
  End;

{ TExtImages }

   { TExtDBImages }

   TExtDBImages = class( TExtCustomImages )
   private
     FRecNo : Int64;
     FDataLink: TFieldDataLink;
     FCounterTopErased : int64;
     FShowErrors : Boolean ;
     FDatasetAfterPost   ,
     FDatasetAfterDelete : TDatasetNotifyEvent;
     FDataText, FDataKey : String;
     FDialog   : TPreviewFileDialog;
     procedure p_SetDatafield  ( const Value : String );
     procedure p_SetDataText   ( const Value : String );
     procedure p_SetDataKey    ( const Value : String );
     procedure p_SetDatasource ( const Value : TDatasource );
     function  fds_GetDatasource : TDatasource;
     function  fs_GetDatafield : String;
     function  ff_Getfield : TField;
   protected
     procedure p_ActiveChange(Sender: TObject); virtual;
     procedure p_DatasetPost(Sender: TDataset); virtual;
     procedure p_DatasetDelete(Sender: TDataset); virtual;
     procedure p_UpdateData(Sender: TObject); virtual;
     function  DeleteImagesTopPanel:Boolean; override;
     procedure DeleteImagesDownPanel; override;
     procedure DeleteLinesDownPanel; override;
     function DeleteLinesTopPanel: Boolean; override;
     function CanScroll: Boolean; override;
     procedure AddDownPanel; override;
     procedure SetFieldsToPanel; virtual;
     function  AddImagesDownPanel:Boolean; override;
     function  AddLinesTopPanel:Boolean; override;
     function CreateImagesMap : TExtCustomCollectionImages; override;
     procedure Notification(AComponent: TComponent; Operation: TOperation); override;
   public
     procedure CreateSomeLine; override;
     constructor Create(AOwner: TComponent); override;
     destructor  Destroy; override;
     procedure DestroyAllAndCreate; virtual;
     property  RecNumber : Int64 read FRecNo;
     property CounterTop  : Int64 read FCounterTopErased write FCounterTopErased;
   published
     property Datafield : String read fs_GetDatafield write p_SetDatafield ;
     property DataText  : String read fDataText  write p_SetDataText ;
     property DataKey   : String read FDataKey  write p_SetDataKey ;
     property Dialog    : TPreviewFileDialog read FDialog  write FDialog ;
     property Datasource : TDatasource read fds_GetDatasource write p_SetDatasource ;
     property ShowErrors : Boolean read FShowErrors write FShowErrors default True ;
   end;


implementation

uses fonctions_images,
{$IFDEF FPC}
     unite_messages,
     LazFileUtils,
{$ELSE}
      unite_messages_delphi,
      fonctions_system,
{$ENDIF}
     sysutils,
     fonctions_file;

{ TExtDBImages }


procedure TExtDBImages.p_ActiveChange(Sender: TObject);
begin
  if FDataLink.Active
   Then DestroyAllAndCreate
   Else DestroyComponents;
end;

constructor TExtDBImages.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink.Create ;
  FDataLink.DataSource := nil ;
  FDataLink.FieldName  := '' ;
  FDataLink.Control := Self;
  FDataLink.OnUpdateData := p_UpdateData;
  FDataLink.OnActiveChange := p_ActiveChange;
  FDatasetAfterDelete := nil;
  FDatasetAfterPost   := nil;
  FDataKey := '';
  FDataText:= '';
  FShowErrors:= True;
  FDialog := nil;
  FCounterTopErased := 0;
end;

procedure TExtDBImages.p_DatasetPost(Sender: TDataset);
begin
  DestroyAllAndCreate;
  if assigned ( FDatasetAfterPost )
   Then FDatasetAfterPost(Sender);
end;

procedure TExtDBImages.p_DatasetDelete(Sender: TDataset);
begin
  DestroyAllAndCreate;
  if assigned ( FDatasetAfterDelete )
   Then FDatasetAfterDelete(Sender);
end;

destructor TExtDBImages.Destroy;
begin
  inherited;
  Datasource:=nil;
  FDataLink.Destroy;
end;

function TExtDBImages.fs_GetDatafield: String;
begin
  if assigned ( FDataLink ) then
    Begin
      Result := FDataLink.FieldName ;
    End
   Else
    Result := '';

end;

function TExtDBImages.ff_Getfield: TField;
begin
  Result:=FDataLink.Field;
end;

procedure TExtDBImages.DestroyAllAndCreate;
begin
  DestroyComponents;
  if not FDataLink.Active
  or not assigned(FDataLink.DataSet)
  or FDataLink.DataSet.IsEmpty
  or ((DataText = '') and (Datafield = ''))
   Then Exit;
  FDataLink.Dataset.First;
  CreateSomeLine;
end;

procedure TExtDBImages.CreateSomeLine;
begin
  with Columns,FDataLink.Dataset do
   begin
    if Counter > 0 Then
      Begin
       RecNo := Counter+1;
       Next;
      end;
    while not FDataLink.Eof do
    with Add as TExtPanelDBImage do
     Begin
       FRecNo:=RecNo;
       SetFieldsToPanel;

       if (( ImagesPerLine = 0 ) and ( PanelLine.Left - Self.HorzScrollBar.Position > Self.Width ))
       or (( ImagesPerLine > 0 ) and ( PanelLine.Top  - Self.VertScrollBar.Position > Self.Height ) and (ImagesPerLine=CounterPerLine))
        Then
          Exit;
       Next;
     end;

   end;
end;


function TExtDBImages.fds_GetDatasource: TDatasource;
begin
  if assigned ( FDataLink ) then
    Begin
      Result := FDataLink.Datasource ;
    End
   Else
    Result := Datasource;

end;

procedure TExtDBImages.p_SetDatafield(const Value: String);
begin
  if assigned ( FDataLink )
  and ( Value <> FDataLink.FieldName ) then
    Begin
      FDataLink.FieldName := Value;
    End;
end;

procedure TExtDBImages.p_SetDataText(const Value: String);
begin
  FDataText:=Value;
end;

procedure TExtDBImages.p_SetDataKey(const Value: String);
begin
  if FDataKey <> Value
   Then
    FDataKey:=Value;
end;

// procedure DeleteImagesDown
// Have got ErasedFiles to Delete when down
// When there is one line only
function TExtDBImages.DeleteImagesTopPanel:Boolean;
var Lparent: TWinControl;
begin
  with Columns do
  if  ( Count > VisibleLines * ImagesPerLine )  Then
   Begin
     Result:=True;
     Delete(0);
     CounterTop := FRecNo;
     Inherited; // Have Really deleted
   end
  else
   Result:=False;
end;

// procedure DeleteImagesUp
// Have got Directories and CurrentFile to Delete when up
// When there is one line only
procedure TExtDBImages.DeleteImagesDownPanel;
begin
  if  ( Columns.Count > VisibleLines * ImagesPerLine )  Then
   Begin
    Columns.Delete(Columns.Count-1);
    Inherited; // Have Really deleted
   end;
end;

// procedure DeleteLinesDown
// Have got ErasedFiles to Delete when down
// Deleting one line of lines only
function TExtDBImages.DeleteLinesTopPanel:Boolean;
var LParentTag,
    li_i : Integer;
    LParent : TWinControl;
begin
  with Columns do
   Begin
    // Testing if can delete lines
    /// deleting
    Result := Count > VisibleLines * ImagesPerLine;
     if Result Then
      with Items [0] do
       Begin
         // Panel of the line
         LParent := Panel.Parent;
         LParentTag := Panel.Parent.Tag;
         //Deleting Panel of line
         for li_i := 0 to Count - 1 do
          with Items [0] as TExtPanelDBImage do
          Begin
            // Deleting one line only
            if LParentTag <> Panel.Parent.Tag
             Then Break;
            // Adding erased image to future added image
            Delete(0);// deleting and destroying
            // Decreasing counter
            Counter:=Counter-1;
          end;
         CounterTop:=FRecNo;
         // Destroying panel of line
         LParent.Destroy;
         Inherited; // Have Really deleted
       end;
   end;
end;

function TExtDBImages.CanScroll: Boolean;
begin
  Result := FDataLink.Active;
end;

procedure TExtDBImages.SetFieldsToPanel;
Begin
  with Columns, Items [ Count -1 ] as TExtPanelDBImage, FDataLink.DataSet do
   Begin
    Image.ShowErrors:=ShowErrors;
    if (FDataKey > '') then
      FKey:=FieldByName(FDataKey).Value;
    if ( FDataText > '' )
     Then
       Begin
         if not FieldByName(FDataText).IsNull Then
           Memo.Lines.Text := FieldByName(FDataText).AsString;
       end
     Else PanelMemo.Hide;
    if DataField > ''
    Then
      Begin
        if not FieldByName(Datafield).IsNull Then
          p_FieldToImage ( FDataLink.Field, Image.Picture.Bitmap, 0, 0, True, ShowErrors );
      end
     Else Image.Hide;
   end;
end;

procedure TExtDBImages.AddDownPanel;
var li_i : Integer;
begin
  with Columns, FDataLink.DataSet do
   Begin
    RecNo:=CounterTop-1;
    CounterPerLine:=0;
    for li_i := CounterTop-1 downto CounterTop-ImagesPerLine do
     if not FDataLink.BOF Then
       with Columns,Add as TExtPanelDBImage do
        Begin

          SetFieldsToPanel;

          if (( ImagesPerLine = 0 ) and ( PanelLine.Left - Self.HorzScrollBar.Position > Self.Width ))
          or (( ImagesPerLine > 0 ) and ( PanelLine.Top  - Self.VertScrollBar.Position > Self.Height ))
           Then
             Exit;
          Prior;
        end;
    CounterTop:=CounterTop-ImagesPerLine;
    PanelLine:=Items[Count-1].Panel.Parent as TPanel;
   end;
End;

function TExtDBImages.AddImagesDownPanel:Boolean;
begin
  with Columns do
  if  ( CounterTop>0)
  Then
    Begin
      Result:=True;
      CounterPerLine := 0;
      AddDownPanel;
    end
  Else
      Result:=False;
 Inherited;
end;

function TExtDBImages.AddLinesTopPanel: Boolean;
begin
  with Columns do
  if  (CounterTop>0)
  Then
    Begin
     CreatePanelLine;
     PanelLine.Top:=0;
     CounterPerLine := 0;
     AddDownPanel;
     Result := True;
    end
   Else
    Result := False;
  Inherited;
end;

// procedure DeleteLinesUp
// Have got Directories and CurrentFile to Delete when up
// Deleting one line of lines only
procedure TExtDBImages.DeleteLinesDownPanel;
var LParentTag,
    li_i : Integer;
    LParent : TWinControl;
begin
  with Columns do
  // Testing if should delete lines
   if Count > VisibleLines * ImagesPerLine Then
    with Items [Count - 1] do
     Begin
       // Panel of the line
       LParent := Panel.Parent;
       LParentTag := Panel.Parent.Tag;
       /// deleting
       while Count > 1 do
        with Items [Count - 1] as TExtPanelDBImage do
        Begin
          // Deleting one line only
          if LParentTag <> Panel.Parent.Tag
           Then
             Break;
          Delete(Count - 1);
          // Decreasing counter
          Counter:=Counter-1;
        end;
       // Will add a line next
       CounterPerLine:=ImagesPerLine;
       // Destroying panel of line
       LParent.Destroy;
       Inherited; // Have Really deleted
     end;
end;

procedure TExtDBImages.p_SetDatasource(const Value: TDatasource);
begin
  if assigned ( FDataLink )
  and ( Value <> FDataLink.Datasource ) then
    Begin
      if  assigned ( FDataLink.DataSet )
       Then
         Begin
           FDataLink.DataSet.AfterPost   := FDatasetAfterPost;
           FDataLink.DataSet.AfterDelete := FDatasetAfterDelete;
         end;
      FDataLink.Datasource := Value;
      if  assigned ( FDataLink.DataSet )
       Then
         Begin
           FDatasetAfterPost:=FDataLink.DataSet.AfterPost;
           FDataLink.DataSet.AfterPost   := p_DatasetPost;
           FDataLink.DataSet.AfterDelete := p_DatasetDelete;
         end;
    End;
end;

procedure TExtDBImages.p_UpdateData(Sender: TObject);
begin
  //DestroyAllAndCreate;
end;

function TExtDBImages.CreateImagesMap : TExtCustomCollectionImages;
begin
  Result := TExtCollectionDBImages.Create(Self,TExtPanelDBImage);
  // adding editing component
  if csDesigning in ComponentState Then
    Result.Add;
end;

procedure TExtDBImages.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation <> opRemove Then
    Exit;
  if AComponent = FDataLink.DataSource Then
    Datasource:=nil;
  if AComponent = FDialog Then
    Dialog:=nil;
end;

{ TExtCollectionDBImages }

function TExtCollectionDBImages.MemoLineCreate: TCustomMemo;
begin
  Result := TFWMemo.Create (Component);
end;


{ TExtPanelDBImage }

function TExtPanelDBImage.CreateMemo: TCustomMemo;
begin
  Result := Inherited;
  Result.OnExit:=OnQuitMemo;
end;

function TExtPanelDBImage.CreateImage: TExtImage;
begin
  Result := Inherited;
  Result.OnClick:=OnClickImage;
end;

procedure TExtPanelDBImage.InitComponentsPanelLine;
begin
  inherited;
end;

procedure TExtPanelDBImage.OnClickImage ( Sender : TObject );
var LBookMark : {$IFDEF WITH_TBOOKMARK}TBookMark{$ELSE}TBookMarkStr{$ENDIF};
begin
  with (Collection as TExtCollectionDBImages).Component as TExtDBImages, FDataLink.Dataset do
  if  FDataLink.CanModify
  and assigned ( Dialog )
  and not (State in [dsEdit,dsInsert])
  and Dialog.Execute Then
   try
     LBookMark := Bookmark;
     if BeginModfiy Then
      Begin
       p_ImageFileToField(Dialog.FileName,FieldByName(Datafield),ShowErrors);
       Image.LoadFromFile(Dialog.FileName);
       Post;
      end;
   finally
     EndModify(LBookMark);
   end;
end;
function TExtPanelDBImage.BeginModfiy:Boolean;
Begin
  with (Collection as TExtCollectionDBImages).Component as TExtDBImages, FDataLink.Dataset do
   Begin
     Result := True;
     AfterPost:=FDatasetAfterPost;
     if FDataKey > ''
      Then Result := Locate(FDataKey,FKey,[])
      Else RecNo := FRecNo;
     Edit;
   end;

end;

procedure TExtPanelDBImage.EndModify( const ABookmark : {$IFDEF WITH_TBOOKMARK}TBookMark{$ELSE}TBookMarkStr{$ENDIF} );
Begin
  with (Collection as TExtCollectionDBImages).Component as TExtDBImages, FDataLink.Dataset do
   Begin
    BookMark := ABookMark;
    AfterPost:=p_datasetPost;
   end;

end;

procedure TExtPanelDBImage.OnQuitMemo(Sender: TObject);
var LBookMark : {$IFDEF WITH_TBOOKMARK}TBookMark{$ELSE}TBookMarkStr{$ENDIF};
begin
  with (Collection as TExtCollectionDBImages).Component as TExtDBImages, FDataLink.Dataset do
  if  FDataLink.CanModify
  and not (State in [dsEdit,dsInsert]) Then
    try
      LBookMark := Bookmark;
      if BeginModfiy Then
       Begin
        FieldByName(DataText).AsString:=Memo.Text;
        Post;
       end;
    finally
      EndModify(LBookMark);
    end;

end;

procedure TExtPanelDBImage.LoadFromStream(const astream: TStream);
begin
  Image.LoadFromStream ( astream );
end;

function TExtPanelDBImage.LoadFromFile(const afile: String): Boolean;
begin
  Image.LoadFromFile ( afile );

end;

function TExtPanelDBImage.SavetoFile(const afile: String): Boolean;
begin
  Image.SaveToFile ( afile );

end;

{ TExtDBImages }



{$IFDEF VERSIONS}
initialization
  p_ConcatVersion ( gVer_TExtDBImages );
{$ENDIF}
end.
