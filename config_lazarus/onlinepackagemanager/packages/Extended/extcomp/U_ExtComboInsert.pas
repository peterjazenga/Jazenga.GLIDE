{*********************************************************************}
{                                                                     }
{                                                                     }
{             TExtDBComboInsert :                               }
{             Objet issu d'un TCustomComboBox qui associe les         }
{             avantages de la DBComoBox et de la DBLookUpComboBox     }
{             Créateur : Matthieu Giroux                          }
{             31 Mars 2005                                            }
{             Version 1.0                                             }
{                                                                     }
{                                                                     }
{*********************************************************************}

unit U_ExtComboInsert;

{$I ..\DLCompilers.inc}
{$I ..\extends.inc}
{$IFDEF FPC}
{$Mode Delphi}
{$ENDIF}

interface

uses Classes,
  {$IFDEF FPC}
     LCLType,
  {$ELSE}
     Windows, Mask, JvDBLookup, Messages,
  {$ENDIF}
     DB,DBCtrls,
  {$IFDEF VERSIONS}
    fonctions_version,
  {$ENDIF}
  u_extsearchedit;

{$IFDEF VERSIONS}
const
  gVer_TDBLookupComboInsert : T_Version = ( Component : 'Composant TDBComboBoxInsert' ;
                                             FileUnit : 'U_DBComboBoxInsert' ;
                                             Owner : 'Matthieu Giroux' ;
                                             Comment : 'Insertion automatique dans une DBComboLookupEdit.' ;
                                             BugsStory : '1.2.0.1 : Unfating.'
                                                       + '1.2.0.0 : TCustomSearchEdit inherit.'
                                                       + '1.1.0.0 : ExtSearchDbEdit inherit.' +#13#10
                                                       + '1.0.1.5 : MyLabel unset correctly.' +#13#10
                                                       + '1.0.1.4 : Better component testing.' +#13#10
                                                       + '1.0.1.3 : Compiling on lazarus.' +#13#10
                                                       + '1.0.1.2 : Bug validation au post.' +#13#10
                                                       + '1.0.1.1 : Bug rafraîchissement quand pas de focus.' +#13#10
                                                       + '1.0.1.0 : Propriété Modify.' +#13#10
                                                       + '1.0.0.0 : Version bêta inadaptée, réutilisation du code de la TJvDBLookupComboEdit.' +#13#10
                                                       + '0.9.0.0 : En place à tester.';
                                             UnitType : 3 ;
                                             Major : 1 ; Minor : 2 ; Release : 0 ; Build : 1 );

{$ENDIF}
type

{ TExtDBComboInsert }
  TExtDBComboInsert = class(TCustomSearchEdit)
   private

    // On est en train d'écrire dans la combo
    FSearchKey   : String;
    FFieldKey    : TFieldDatalink;
    // Focus sur le composant
    // En train de mettre à jour ou pas
    FUpdate,
    FNotFound : Boolean;

    //look
    FNotifyOrder : TNotifyEvent;
    function fs_getDataSource: TDataSource;
    function GetFieldKey: String;
    procedure p_setDataSource(const AValue: TDataSource);
    procedure SetFieldKey(const AValue: String);
  protected
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure AutoInsert; virtual;
    procedure NotFound; override ;
    procedure Locating; override ;
    procedure DoEnter; override ;
    procedure DoExit; override ;
    procedure SetFieldKeyValue; virtual;
    procedure DataChange ( ADataLink :TObject ); virtual;
    procedure ClosePopupEvent; override;
  public
    constructor Create ( AOwner : TComponent ); override;
    destructor Destroy ; override;
    function Field: TField; virtual;
    procedure Loaded; override ;
    procedure LoadSourceKey; virtual;
    procedure AssignListValue;virtual;
    function Modify:Boolean;virtual;
  published
    property OnOrder : TNotifyEvent read FNotifyOrder write FNotifyOrder;
    property DataSource : TDatasource read fs_getDataSource write p_setDataSource ;
    property SearchKey: String read FSearchKey write FSearchKey;
    property DataField: String read GetFieldKey write SetFieldKey;
  end;

implementation

uses
  {$IFDEF FPC}
  {$ELSE}
  JvConsts, JvToolEdit,
  {$ENDIF}
  fonctions_db,fonctions_dbobjects;

{ TExtDBComboInsert }



////////////////////////////////////////////////////////////////////////////////
// Constructeur : Create
// description  : Initialisation du composant
////////////////////////////////////////////////////////////////////////////////
constructor TExtDBComboInsert.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // Pas de modification ni de mise à jour à la création
  FUpdate := False ;
  FNotFound := False;
  // look
  FSearchKey := '';
  FFieldKey := TFieldDataLink.Create;
  FFieldKey.OnDataChange := DataChange;
end;

destructor TExtDBComboInsert.Destroy;
begin
  inherited Destroy;
  FFieldKey.Destroy;
end;

function TExtDBComboInsert.Field: TField;
begin
  Result := FFieldKey.Field;
end;

procedure TExtDBComboInsert.Loaded;
begin
  LoadSourceKey;
  inherited Loaded;
end;

procedure TExtDBComboInsert.LoadSourceKey;
begin

end;

///////////////////////////////////////////////////////////////////////////
// fonction    : AssignListValue
// description : Récupère la valeur affichée
////////////////////////////////////////////////////////////////////////////////
procedure TExtDBComboInsert.AssignListValue;
Begin
    // Verify Text value or locate
  If  assigned ( FFieldKey.Field )
  and not FFieldKey.Field.IsNull
  and assigned ( SearchSource )
  and assigned ( SearchSource.DataSet )
  Then
    with SearchSource.DataSet do
     if not (State in [dsEdit,dsInsert]) Then
       Begin
        Open;
        if  assigned ( FindField ( SearchDisplay ))
        and ( FindField ( SearchDisplay ).AsString <> Text )
        and assigned ( FindField ( SearchKey   ))
         Then
          try
            DisableControls;
            if Locate ( SearchKey, Field.Value, [] ) Then
              // récupération à partir de la liste
              Text := FindField ( SearchDisplay ).AsString ;

          finally
            EnableControls;
          end;
       end;
End ;

function TExtDBComboInsert.Modify: Boolean;
begin
  Result:= fb_DatasourceModifying ( Datasource );
end;




////////////////////////////////////////////////////////////////////////////////
// procédure   : DoEnter
// description : Attribue le focus au composant
////////////////////////////////////////////////////////////////////////////////
procedure TExtDBComboInsert.DoEnter;
begin
  inherited DoEnter;
  // Sélectionne le texte
  SelectAll ;
end;

////////////////////////////////////////////////////////////////////////////////
// procédure   : InsertLookup
// description : Insertion automatique
// paramètre   : Update : validation du champ si pas en train de valider
////////////////////////////////////////////////////////////////////////////////
procedure TExtDBComboInsert.NotFound;
var LText : String;
begin
  inherited;
  FNotFound := True;
End ;

procedure TExtDBComboInsert.Locating;
begin
  inherited Locating;
  FNotFound:=False;
end;

////////////////////////////////////////////////////////////////////////////////
// procédure   : DoExit
// description : Défocus du composant
////////////////////////////////////////////////////////////////////////////////
procedure TExtDBComboInsert.DoExit;
begin
  // Auto-insertion
  AutoInsert;
  inherited DoExit;
end;

procedure TExtDBComboInsert.DataChange(ADataLink: TObject);
begin
  if ( SearchSource = nil )
  or ( SearchDisplay = '' )
  or ( SearchSource.DataSet = nil ) Then
   Begin
    Text:= '';
    Exit;
   End;
  with SearchSource.DataSet do
   Begin
    Open;
    if Locate ( FSearchKey, Field.Value, [] )
     Then Text:= FindField ( SearchDisplay ).AsString
     Else Text:= '';
   End;
end;

procedure TExtDBComboInsert.ClosePopupEvent;
begin
  inherited;
  Field.DataSet.Edit;
  Field.Value := SearchSource.DataSet.FieldByName ( FSearchKey ).Value;
End;

procedure TExtDBComboInsert.SetFieldKeyValue;
Begin
  with FFieldKey do
  if Assigned ( Field )
  and ( FSearchKey > '' )
   Then
    with SearchSource.DataSet do
     if Field.IsNull
     or (FieldByName ( FSearchKey ).Value <> Field.Value)
      Then
        Begin
         Dataset.Edit;
         Field.Value := FieldByName ( FSearchKey ).Value;
        end;
end;


procedure TExtDBComboInsert.AutoInsert;
var LText : String;
begin
  // Auto-insertion
  if FNotFound Then
    with SearchSource,DataSet do
     try
      if assigned ( FFieldKey.DataSet )
        Then FFieldKey.DataSet.DisableControls;
      DisableControls;
      LText := Text;
      Insert ;
      FieldByName ( SearchDisplay ).Value := LText;
      Post ;
      FUpdate := True ;
      fb_RefreshDataset(DataSet);
      if Locate ( SearchDisplay, LText, [] ) Then
        Begin
          Text := FindField ( SearchDisplay ).AsString;
          SetFieldKeyValue;
          if assigned ( OnSet ) Then
            OnSet ( Self );
        end;
     finally
      if assigned ( FFieldKey.DataSet ) 
        Then  FFieldKey.DataSet.EnableControls;
      EnableControls;
     end
   else
    SetFieldKeyValue;
end;


// function TCustomSearchEdit.fs_getSearchSource
// Getting the Search source
function TExtDBComboInsert.fs_getDataSource: TDataSource;
begin
  Result := FFieldKey.DataSource;
end;

// procedure TCustomSearchEdit.p_setSearchSource
// Setting the Search source
procedure TExtDBComboInsert.p_setDataSource(const AValue: TDataSource);
begin
  FFieldKey.DataSource := AValue;
end;

function TExtDBComboInsert.GetFieldKey: String;
begin
  Result:=FFieldKey.FieldName;
end;

procedure TExtDBComboInsert.SetFieldKey(const AValue: String);
begin
  FFieldKey.FieldName:=AValue;
end;

procedure TExtDBComboInsert.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN Then
   AutoInsert;
  if (Key = VK_ESCAPE )
  and FNotFound Then
   Begin
     FNotFound := False;
     Text:=Field.AsString ;
   end;
  inherited KeyUp(Key, Shift);
end;



{$IFDEF VERSIONS}
initialization
  p_ConcatVersion ( gVer_TDBLookupComboInsert );
{$ENDIF}
end.
