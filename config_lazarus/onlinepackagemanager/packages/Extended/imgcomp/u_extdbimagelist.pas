unit U_ExtDBImageList;

interface

{$i ..\extends.inc}
{$IFDEF FPC}
{$Mode Delphi}
{$ENDIF}

uses Graphics,
{$IFDEF VERSIONS}
  fonctions_version,
{$ENDIF}
{$IFDEF FPC}
  LCLType,
{$ELSE}
  Windows,
{$ENDIF}
  DB, DBCtrls, u_extimagelist,
  Classes, U_ExtMapImageIndex;

{$IFDEF VERSIONS}
const
  gVer_TExtDBImageList: T_Version = (Component: 'Composant TExtDBImageList';
    FileUnit: 'U_ExtDBImageList';
    Owner: 'Matthieu Giroux';
    Comment:
    'Gestion de liste d''images dans les données.';
    BugsStory : '1.1.0.0 : Creating ExtImageList.' + #13#10 +
                '1.0.0.0 : Growing the component.' + #13#10 +
                '0.9.9.1 : UTF 8.' + #13#10 +
                '0.9.9.0 : Tested and optimised.' + #13#10 +
                '0.9.0.0 : Non testée.';
    UnitType: 3;
    Major: 1; Minor: 1; Release: 0; Build: 0);

{$ENDIF}

type
  { TExtDBImageList }

  TExtDBImageList = class(TExtImageList)
  private
    FMapImages: TExtMapImages;
    FDataLink: TFieldDataLink;
    procedure p_SetDatafield(const Value: string);
    procedure p_SetDatasource(const Value: TDatasource);
    function fds_GetDatasource: TDatasource;
    function fs_GetDatafield: string;
    procedure p_SetImagesMap(const Value: TExtMapImages);
  protected
    procedure p_ActiveChange(Sender: TObject); virtual;
    procedure p_DataChange(Sender: TObject); virtual;
    procedure p_SetImage; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ImageIndex;
  published
    property Datafield: string read fs_GetDatafield write p_SetDatafield;
    property Datasource: TDatasource read fds_GetDatasource write p_SetDatasource;
    property ImagesMap: TExtMapImages read FMapImages write p_SetImagesMap;
  end;


implementation

uses SysUtils;

{ TExtDBImageList }

procedure TExtDBImageList.p_ActiveChange(Sender: TObject);
begin
  p_SetImage;
end;

constructor TExtDBImageList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink.Create;
  FDataLink.DataSource := nil;
  FDataLink.FieldName := '';
  FDataLink.Control := Self;
  FDataLink.OnDataChange := p_DataChange;
  FDataLink.OnActiveChange := p_ActiveChange;
end;

procedure TExtDBImageList.p_DataChange(Sender: TObject);
begin
  p_SetImage;
end;

procedure TExtDBImageList.p_SetImage;
begin
  if assigned(FDataLink)
   and FDataLink.Active and assigned(FDataLink.Field) and not
    FDataLink.Field.IsNull
   Then
     Begin
       if assigned ( FMapImages ) then
          ImageIndex := FMapImages.ImageIndexOf ( FDataLink.Field.AsString )
       else
        if FDataLink.Field is TNumericField
         Then ImageIndex := FDataLink.Field.AsInteger
         Else ImageIndex := -1;
      End
     Else ImageIndex := -1;

   Inherited;

end;


destructor TExtDBImageList.Destroy;
begin
  FDataLink.Free;
  inherited;
end;

function TExtDBImageList.fs_GetDatafield: string;
begin
  if assigned(FDataLink) then
  begin
    Result := FDataLink.FieldName;
  end
  else
    Result := '';

end;

procedure TExtDBImageList.p_SetImagesMap(const Value: TExtMapImages);
begin
  if FMapImages <> Value Then
    Begin
      FMapImages := Value;
      p_SetImage;
    End;
end;

function TExtDBImageList.fds_GetDatasource: TDatasource;
begin
  if assigned(FDataLink) then
  begin
    Result := FDataLink.Datasource;
  end
  else
    Result := Datasource;

end;

procedure TExtDBImageList.p_SetDatafield(const Value: string);
begin
  if assigned(FDataLink) and (Value <> FDataLink.FieldName) then
  begin
    FDataLink.FieldName := Value;
  end;
end;

procedure TExtDBImageList.p_SetDatasource(const Value: TDatasource);
begin
  if assigned(FDataLink) and (Value <> FDataLink.Datasource) then
  begin
    FDataLink.Datasource := Value;
    ImageIndex:=-1;
  end;
end;


{$IFDEF VERSIONS}
initialization
  p_ConcatVersion(gVer_TExtDBImageList);
{$ENDIF}
end.
