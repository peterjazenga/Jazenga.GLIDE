unit U_ExtDBImage;

interface

{$i ..\extends.inc}
{$IFDEF FPC}
{$Mode Delphi}
{$ENDIF}

uses Graphics,
{$IFDEF VERSIONS}
  fonctions_version,
{$ENDIF}
{$IFDEF BGRA}
  bgrabitmap,bgrabitmaptypes,
{$ELSE}
  ImagingTypes,
  ImagingComponents,
  Imaging,
{$ENDIF}
     DB, DBCtrls,
     Classes, U_ExtImage;

{$IFDEF VERSIONS}
const
    gVer_TExtDBImage : T_Version = ( Component : 'Composant TExtDBImage' ;
                                               FileUnit : 'U_ExtDBImage' ;
                                               Owner : 'Matthieu Giroux' ;
                                               Comment : 'Gestion d''images de tous types dans les données.' ;
                                               BugsStory : 'Version 1.0.2.1 : Testing.' + #13#10
                                                         + 'Version 1.0.2.0 : TExtPathDBImage.' + #13#10
                                                         + 'Version 1.0.1.0 : BGRABitmap.' + #13#10
                                                         + 'Version 1.0.0.3 : UTF 8.' + #13#10
                                                         + 'Version 1.0.0.2 : Upgrading from tested functions.' + #13#10
                                                         + 'Version 1.0.0.1 : Creating ExtImage.' + #13#10
                                                         + 'Version 1.0.0.0 : En place, tout a été testé.' + #13#10
                                                         + 'Version 0.9.0.1 : En place, tout n''a pas été testé.' + #13#10
                                                         + '0.9.0.0 : Simple affiche de toute image en données.';
                                               UnitType : 3 ;
                                               Major : 1 ; Minor : 0 ; Release : 2 ; Build : 1 );

{$ENDIF}
type

   { TExtCustomDBImage }

    TExtCustomDBImage = class( TExtImage)
      private
        FDataLink: TFieldDataLink;
        function  fds_GetDatasource : TDatasource;
        function  fs_GetDatafield : String;
        function  ff_Getfield : TField;
      protected
        procedure p_ActiveChange(Sender: TObject); virtual;
        procedure p_DataChange(Sender: TObject); virtual;
        procedure p_SetDatasource ( const Value : TDatasource ); virtual;
        procedure p_SetDatafield  ( const Value : String );virtual;
        procedure p_UpdateData(Sender: TObject); virtual;
      public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy ; override;
        property Field : TField read ff_Getfield;
        procedure SetImage; virtual; abstract;
      published
        property Datafield : String read fs_GetDatafield write p_SetDatafield ;
        property Datasource : TDatasource read fds_GetDatasource write p_SetDatasource ;
      end;

    { TExtDBImage }

     TExtDBImage = class(TExtCustomDBImage)
       private
         FLocalDir:String;
         FFilesDir:String;
       public
         procedure AssignExtBitmap ( const ABitmap : {$IFDEF BGRA}TBGRACustomBitmap{$ELSE}TImageData{$ENDIF}); override;
         procedure LoadFromStream ( const astream : TStream ); override;
         function  LoadFromFile   ( const afile   : String ):Boolean;  override;
         procedure SaveToStream ( const astream : TMemoryStream ); virtual;
         function  SaveToFile   ( const afile   : String ):Boolean; overload; override;
         procedure SetImage; override;
       end;

    { TExtPathDBImage }

    TExtPathDBImage = class(TExtCustomDBImage)
      private
        FLocalDir:String;
        FFilesDir:String;
        procedure SetFileField ( const Avalue : String );
        procedure VerifyField;
        procedure AssignExtBitmap ( const ABitmap : {$IFDEF BGRA}TBGRACustomBitmap{$ELSE}TImageData{$ENDIF}); override;
      protected
         procedure SetFilesDir ( const Avalue : String ); virtual;
         procedure UpdateData ( AObject : TObject ); virtual;
         procedure EditingChanged ( AObject : TObject ); virtual;
         procedure DeleteFile; virtual;
         procedure p_SetDataSource(const AValue: TDataSource); override;
         procedure p_SetDataField(const AValue: string); override;
      public
        constructor Create(AOwner: TComponent); override;
        function  LoadFromFile   ( const afile   : String ):Boolean;  override;
        function  SaveToFile   ( const afile   : String ):Boolean; overload; override;
        procedure SetImage; override;
        {$IFDEF DELPHI}
        procedure Changed; override;
        {$ELSE}
        procedure TextChanged; override;
        {$ENDIF}
        property LocalDir: string read FLocalDir write SetFileField;
      published
        property FilesDir: string read FFilesDir write SetFilesDir;
      end;


implementation

uses fonctions_images,
     Controls,
     FileUtil,
     {$IFDEF FPC}
     LazFileUtils,
     {$ELSE}
     fonctions_system,
     {$ENDIF}
     fonctions_filepath,
     sysutils;

{ TExtDBImage }

procedure TExtCustomDBImage.p_ActiveChange(Sender: TObject);
begin
  SetImage;
end;

constructor TExtCustomDBImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink.Create ;
  FDataLink.DataSource := nil ;
  FDataLink.FieldName  := '' ;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := p_DataChange;
  FDataLink.OnUpdateData := p_UpdateData;
  FDataLink.OnActiveChange := p_ActiveChange;
end;

procedure TExtCustomDBImage.p_DataChange(Sender: TObject);
begin
  SetImage;
end;

destructor TExtCustomDBImage.Destroy;
begin
  FDataLink.OnDataChange := nil;
  FDataLink.OnUpdateData := nil;
  FDataLink.OnActiveChange := nil;
  inherited;
  FDataLink.Free;
end;

function TExtCustomDBImage.fs_GetDatafield: String;
begin
  if assigned ( FDataLink ) then
    Begin
      Result := FDataLink.FieldName ;
    End
   Else
    Result := '';

end;

procedure TExtCustomDBImage.p_UpdateData(Sender: TObject);
begin
  SetImage;
end;

function TExtCustomDBImage.ff_Getfield: TField;
begin
  Result:=FDataLink.Field;
end;

function TExtCustomDBImage.fds_GetDatasource: TDatasource;
begin
  if assigned ( FDataLink ) then
    Begin
      Result := FDataLink.Datasource ;
    End
   Else
    Result := Datasource;

end;

procedure TExtCustomDBImage.p_SetDatafield(const Value: String);
begin
  if assigned ( FDataLink )
  and ( Value <> FDataLink.FieldName ) then
    Begin
      FDataLink.FieldName := Value;
    End;
end;

procedure TExtCustomDBImage.p_SetDatasource(const Value: TDatasource);
begin
  if assigned ( FDataLink )
  and ( Value <> FDataLink.Datasource ) then
    Begin
      FDataLink.Datasource := Value;
    End;
end;

procedure TExtDBImage.SetImage;
begin
  if assigned ( FDataLink )
  and assigned ( FDataLink.Field ) Then
  if FDataLink.Field.IsNull
   Then
    Picture.Bitmap.Assign(nil)
   Else
    p_FieldToImage ( FDataLink.Field, Self.Picture.Bitmap, 0, 0, False, ShowErrors );
end;

function TExtDBImage.LoadFromFile(const afile: String):Boolean;
begin
  Result := False;
  FFileName:=afile;
  if  assigned ( FDataLink.Field )
  and FDataLink.CanModify
  and FileExistsUTF8(FFileName) then
    Begin
      p_ImageFileToField(FFileName, FDataLink.Field, ShowErrors);
      Result := True;
    End;
end;

procedure TExtDBImage.LoadFromStream(const astream: TStream);
begin
  if  assigned ( FDataLink.Field ) then
    Begin
      p_StreamToField( astream, FDataLink.Field, ShowErrors );
    End;
end;

procedure TExtDBImage.AssignExtBitmap(const ABitmap: {$IFDEF BGRA}TBGRACustomBitmap{$ELSE}TImageData{$ENDIF});
var LStream : TMemoryStream;
begin
  Field.DataSet.Edit;
  LStream:=TMemoryStream.Create;
  try
    p_ImageToStreamJpeg ( ABitmap, LStream );
    LStream.Position:=0;
    p_StreamToField( LStream, FDataLink.Field, ShowErrors );
  finally
    LStream.Destroy;
  end;
end;

procedure TExtDBImage.SaveToStream(const astream: TMemoryStream);
begin
  if  assigned ( FDataLink.Field ) then
    Begin
      p_ImageFieldToStream ( FDataLink.Field, astream, ShowErrors );
    end;
end;

function TExtDBImage.SaveToFile(const afile: String): Boolean;
begin
  if  assigned ( FDataLink.Field ) then
    Begin
      fb_ImageFieldToFile ( FDataLink.Field, afile, 0, 0, True, ShowErrors );
    end;
end;

procedure TExtPathDBImage.SetFilesDir(const Avalue: String);
begin
  FFilesDir:=IncludeTrailingPathDelimiter(Avalue);

end;

procedure TExtPathDBImage.UpdateData(AObject: TObject);
begin
  with Field do
  if AsString <> Text Then
    Begin
      Text:=AsString;
    end;
end;

procedure TExtPathDBImage.EditingChanged(AObject: TObject);
begin
  with Field do
  if AsString <> Text Then
    Begin
      DeleteFile;
      Text:=AsString;
    end;
end;

procedure TExtPathDBImage.SetFileField(const Avalue: String);
begin
  FLocalDir:=fs_GetFileField (Avalue);

end;

procedure TExtPathDBImage.VerifyField;
begin
  p_VerifyField(Field);
end;

procedure TExtPathDBImage.DeleteFile;
begin
  p_DeleteFile ( FFilesDir, Text );
end;

procedure TExtPathDBImage.p_SetDataSource(const AValue: TDataSource);
begin
  inherited p_SetDataSource(AValue);
  VerifyField;
end;

procedure TExtPathDBImage.p_SetDataField(const AValue: string);
begin
  inherited p_SetDataField(AValue);
  VerifyField;
end;

procedure TExtPathDBImage.AssignExtBitmap(const ABitmap: TBGRACustomBitmap);
begin
end;

constructor TExtPathDBImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink.OnUpdateData:=UpdateData;
  FDataLink.OnEditingChange:=EditingChanged;
end;

function TExtPathDBImage.LoadFromFile(const afile: String): Boolean;
begin
  Result := False;
  if FileExistsUTF8(afile) Then
    Begin
      FLocalDir:=IncludeTrailingPathDelimiter(ExtractFileDir(afile));
      Text:=fs_SaveFile(FLocalDir,FFilesDir,ExtractFileName(afile),Text);
      Result := True;
    end;
end;

function TExtPathDBImage.SaveToFile(const afile: String): Boolean;
begin
  Result:=False;
  if DirectoryExistsUTF8(ExtractFileDir(afile))
  and assigned(field.DataSet)
  and (field.AsString>'') Then
    Begin
      copyfile(FFilesDir+field.AsString,afile);
      Result := True;
    end;
end;

procedure TExtPathDBImage.TextChanged;
begin
  if Field = nil Then
    Exit;
  with Field do
   Begin
    if Self.Text <> AsString
     Then
      Begin
        DataSet.Edit;
        AsString:=Self.Text;
      end;
    SetImage;
   end;
end;

procedure TExtPathDBImage.SetImage;
begin
  if Field = nil Then
    Exit;
  with Field do
   Begin
    if FileExistsUTF8(FFilesDir+AsString)
    {$IFNDEF WINDOWS}
    and not DirectoryExistsUTF8(FFilesDir+AsString)
    {$ENDIF}
     then
      p_FileToImage(FFilesDir+AsString,Picture,ShowErrors)
     else
      Picture.Bitmap.Assign(nil);
   end;
end;




{$IFDEF VERSIONS}
initialization
  p_ConcatVersion ( gVer_TExtDBImage );
{$ENDIF}
end.
