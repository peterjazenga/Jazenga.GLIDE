unit U_ExtMapImageIndex;

interface

{$i ..\extends.inc}
{$IFDEF FPC}
{$Mode Delphi}
{$ENDIF}

uses Graphics,
{$IFDEF TNT}
     TntExtCtrls,
{$ELSE}
     ExtCtrls,
{$ENDIF}
{$IFDEF VERSIONS}
  fonctions_version,
{$ENDIF}
     DBCtrls,
     Classes;

{$IFDEF VERSIONS}
  const
    gVer_TExtMapImageIndex : T_Version = ( Component : 'Collection TExtMapImagecolumns' ;
                                               FileUnit : 'U_ExtMapImageIndex' ;
                                               Owner : 'Matthieu Giroux' ;
                                               Comment : 'Gestion de liste d''images dans les données.' ;
                                               BugsStory : '1.0.0.0 : adding usefull methods.' + #13#10 +
                                                           '0.9.9.0 : Tested and new component.' + #13#10 +
                                                           '0.9.0.0 : Not tested.';
                                               UnitType : 3 ;
                                               Major : 1 ; Minor : 0 ; Release : 0 ; Build : 0 );

{$ENDIF}

type
  TExtMapImageIndex = class;
  TExtMapImagesColumns = class;

 { TExtMapImageIndex }
  TExtMapImageIndex = class(TCollectionItem)
  private
    s_Value : String;
    i_ImageIndex : Integer ;
  published
    property Value : String read s_Value write s_Value;
    property ImageIndex : Integer read i_ImageIndex write i_ImageIndex;
  End;

  TExtMapImageIndexClass = class of TExtMapImageIndex;

 { TExtMapImagesColumns }
  TExtMapImagesColumns = class(TCollection)
  private
    FComponent: TComponent;
    function GetImageMap( Index: Integer): TExtMapImageIndex;
    procedure SetImageMap( Index: Integer; Value: TExtMapImageIndex);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(Component: TComponent; ColumnClass: TExtMapImageIndexClass); virtual;
    function Add: TExtMapImageIndex; virtual;
    property Component : TComponent read FComponent;
    property Count;
    property Items[Index: Integer]: TExtMapImageIndex read GetImageMap write SetImageMap; default;
  End;

  IMapImageComponent = interface
    procedure CreateImagesMap;
  end;

  { TExtMapImages }

  TExtMapImages = class(TComponent, IMapImageComponent)
      private
        FMapImagesColumns : TExtMapImagesColumns;
        procedure CreateImagesMap; virtual;
        procedure SetColumns ( AValue : TExtMapImagesColumns ); virtual;
      public
        constructor Create(AOwner: TComponent); override;
        function IndexOf ( const AValue : String ): Integer; virtual;
        function ImageIndexOf ( const AValue : String ): Integer; virtual;
      published
      { Published declarations }
        property Columns : TExtMapImagesColumns read FMapImagesColumns write SetColumns ;
    end;

implementation

uses sysutils;

{ TExtMapImages }

// creates the columns
procedure TExtMapImages.CreateImagesMap;
begin
  FMapImagesColumns := TExtMapImagesColumns.Create(Self,TExtMapImageIndex);
end;

// Sets the columns property
procedure TExtMapImages.SetColumns(AValue: TExtMapImagesColumns);
begin
  FMapImagesColumns.Assign(AValue);
end;

// creates the invisible component with the columns
constructor TExtMapImages.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateImagesMap;
end;

// Get the Index of Image in Collection
function TExtMapImages.IndexOf(const AValue: String): Integer;
var i : Integer;
begin
  Result:=-1;
  for i := 0 to FMapImagesColumns.Count - 1 do
   if AValue = FMapImagesColumns [ i ].Value Then
    Begin
      Result := i ;
    end;
end;

// Get the ImageIndex of Image
function TExtMapImages.ImageIndexOf(const AValue: String): Integer;
begin
  Result := IndexOf(AValue);
  if Result >= 0 Then
    Result:=Columns [ Result ].ImageIndex;
end;

{ TExtMapImagesColumns }

// add a column image property
function TExtMapImagesColumns.Add: TExtMapImageIndex;
begin
  Result := TExtMapImageIndex(inherited Add);
end;

// creates the columns with the component
constructor TExtMapImagesColumns.Create(Component: TComponent;
  ColumnClass: TExtMapImageIndexClass);
begin
  inherited Create(ColumnClass);
  FComponent := Component;
end;

// gets an item of a column
function TExtMapImagesColumns.GetImageMap(Index: Integer): TExtMapImageIndex;
begin
  Result := TExtMapImageIndex(inherited Items[Index]);
end;

// returns component, owner of columns
function TExtMapImagesColumns.GetOwner: TPersistent;
begin
  Result := FComponent;
end;


// Sets Columns Property
procedure TExtMapImagesColumns.SetImageMap(Index: Integer;
  Value: TExtMapImageIndex);
begin
  Items[Index].Assign(Value);
end;

{$IFDEF VERSIONS}
initialization
  p_ConcatVersion ( gVer_TExtMapImageIndex );
{$ENDIF}
end.
