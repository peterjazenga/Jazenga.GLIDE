unit u_reports_rlcomponents;

{$I ..\DLCompilers.inc}
{$I ..\extends.inc}
{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}

interface

uses
    SysUtils, RLReport, DB,
    u_extdbgrid,U_ExtMapImageIndex,
{$IFDEF VERSIONS}
  fonctions_version,
{$ENDIF}
    ImgList, Graphics,
    Classes;

{$IFDEF VERSIONS}
const
  gVer_reports_rlcomponents: T_Version = (Component: 'Customized Reports Buttons';
    FileUnit: 'u_reports_rlcomponents';
    Owner: 'Matthieu Giroux';
    Comment: 'Customized Fortes Reports components.';
    BugsStory: '1.0.2.0 : Adding TExtReport.' + #13#10 +
               '1.0.1.0 : TRLImageList.' + #13#10 +
               '1.0.0.0 : From fonctions_reports.' + #13#10 +
               '0.9.0.0 : To test.';
    UnitType: 3;
    Major: 1; Minor: 0; Release: 2; Build: 0);
{$ENDIF}

type TBoolArray = Array of Boolean;
  {@class TRLCustomDBExtImage - Classe base para caixa de imagem ligada a campo de dataset.
   @ancestor TRLCustomImage. }

  { TExtReport }

  TExtReport = class ( TRLReport )
    private
      FPrintComponent : TComponent;
    public
      constructor Create(aOwner:TComponent); override;
      procedure p_BeforePrintTexts (Sender:TObject; var OutputText:string; var PrintIt:boolean); virtual;
    published
      property PrintComponent : TComponent read FPrintComponent write FPrintComponent;
    End;


  { TRLCustomImageList }

  TRLCustomImageList=class(TRLCustomImage)
  private

    // variables

    fImages :TCustomImageList;
    fImageIndex:Integer;
    procedure SetImages( const AValue: TCustomImageList);

  protected
    // custom methods

    procedure   LoadPicture; virtual;


    // override methods

    procedure   Notification(aComponent:TComponent; Operation:TOperation); override;
    procedure   InternalPrint; override;

  public

    // constructors & destructors

    constructor Create(aOwner:TComponent); override;

    // custom properties

    {@prop DataField - Nome do campo associado.
     @links TRLDataFieldProperty. :/}
    property    Images :TCustomImageList read fImages   write SetImages;

    {@prop DataSource - Referência ao DataSource que controle utiliza para se conectar ao DataSet. :/}
    property    ImageIndex:Integer         read fImageIndex write fImageIndex default -1;
  end;

  {@class TRLCustomDBExtImage - Classe base para caixa de imagem ligada a campo de dataset.
   @ancestor TRLCustomImage. }
  TRLCustomDBExtImage=class(TRLCustomImage)
  private

    // variables

    fDataField :TRLDataFieldProperty;
    fDataSource:TDataSource;

    // assign methods

    function    GetField:TField;
    function    GetDataSet:TDataSet;
    procedure   SetDataField(const aValue:TRLDataFieldProperty);
    procedure   SetDataSource(const aValue:TDataSource);

  protected
    // custom methods

    procedure   LoadPicture; virtual;


    // override methods

    procedure   Notification(aComponent:TComponent; Operation:TOperation); override;
    procedure   InternalPrint; override;

  public

    // constructors & destructors

    constructor Create(aOwner:TComponent); override;

    // custom properties

    {@prop Field - Referência para o objeto TField determinado pelas props DataField e DataSource. :/}
    property    Field     :TField               read GetField;

    {@prop DataSet - Referência para o objeto TDataSet determinado pela prop DataSource. :/}
    property    DataSet   :TDataSet             read GetDataSet;

    {@prop DataField - Nome do campo associado.
     @links TRLDataFieldProperty. :/}
    property    DataField :TRLDataFieldProperty read fDataField  write SetDataField;

    {@prop DataSource - Referência ao DataSource que controle utiliza para se conectar ao DataSet. :/}
    property    DataSource:TDataSource          read fDataSource write SetDataSource;
  end;

  { TRLCustomDBExtImageList }

  TRLCustomDBExtImageList=class(TRLCustomDBExtImage)
  private
    FGetImageIndex : TFieldIndexEvent;
    FMapImages : TExtMapImages;
    FImageList : TCustomImageList;
    procedure SetImages ( const Avalue : TCustomImageList );
  protected
    // custom methods

    procedure   LoadPicture; override;
    procedure   Notification(aComponent:TComponent; Operation:TOperation); override;
  public
    constructor Create(aOwner:TComponent); override;
    property    OnGetImageIndex:TFieldIndexEvent   read FGetImageIndex write FGetImageIndex;
    property    MapImages      :TExtMapImages      read FMapImages     write FMapImages ;
    property    Images         :TCustomImageList   read FImageList     write SetImages;
  end;

  TRLDBExtImage=class(TRLCustomDBExtImage)
  published

    // properties

    {@prop Align = ancestor /}
    property    Align;
    {@prop Anchors = ancestor /}
    property    Anchors;
    {@prop AutoSize = ancestor /}
    property    AutoSize;
    {@prop Behavior = ancestor /}
    property    Behavior;
    {@prop Borders = ancestor /}
    property    Borders;
    {@prop Center = ancestor /}
    property    Center;
    {@prop DataField = ancestor /}
    property    DataField;
    {@prop DataSource = ancestor /}
    property    DataSource;
    {@prop Font = ancestor /}
    property    Font;
    {@prop FriendlyName = ancestor /}
    property    FriendlyName;
    {@prop Holder = ancestor /}
    property    Holder;
    {@prop HoldStyle = ancestor /}
    property    HoldStyle;
    {@prop RealBounds = ancestor /}
    property    RealBounds;
    {@prop Scaled = ancestor /}
    property    Scaled;
    {@prop SecondHolder = ancestor /}
    property    SecondHolder;
    {@prop SecondHoldStyle = ancestor /}
    property    SecondHoldStyle;
    {@prop Stretch = ancestor /}
    property    Stretch;
    {@prop Transparent = ancestor /}
    property    Transparent;
    {@prop Visible = ancestor /}
    property    Visible;

    // events

    {@event AfterPrint = ancestor /}
    property    AfterPrint;
    {@event BeforePrint = ancestor /}
    property    BeforePrint;
    {@event OnMeasureHeight = ancestor /}
    property    OnMeasureHeight;

  end;

  TRLExtImageList=class(TRLCustomImageList)
  published

    // properties

    {@prop Align = ancestor /}
    property    Align;
    {@prop Anchors = ancestor /}
    property    Anchors;
    {@prop AutoSize = ancestor /}
    property    AutoSize;
    {@prop Behavior = ancestor /}
    property    Behavior;
    {@prop Borders = ancestor /}
    property    Borders;
    {@prop Center = ancestor /}
    property    Center;
    {@prop Images = ancestor /}
    property    Images;
    {@prop ImageIndex = ancestor /}
    property    ImageIndex;
    {@prop Font = ancestor /}
    property    Font;
    {@prop FriendlyName = ancestor /}
    property    FriendlyName;
    {@prop Holder = ancestor /}
    property    Holder;
    {@prop HoldStyle = ancestor /}
    property    HoldStyle;
    {@prop RealBounds = ancestor /}
    property    RealBounds;
    {@prop Scaled = ancestor /}
    property    Scaled;
    {@prop SecondHolder = ancestor /}
    property    SecondHolder;
    {@prop SecondHoldStyle = ancestor /}
    property    SecondHoldStyle;
    {@prop Stretch = ancestor /}
    property    Stretch;
    {@prop Transparent = ancestor /}
    property    Transparent;
    {@prop Visible = ancestor /}
    property    Visible;

    // events

    {@event AfterPrint = ancestor /}
    property    AfterPrint;
    {@event BeforePrint = ancestor /}
    property    BeforePrint;
    {@event OnMeasureHeight = ancestor /}
    property    OnMeasureHeight;

  end;

  TRLDBExtImageList=class(TRLCustomDBExtImageList)
  published

    // properties

    {@prop Align = ancestor /}
    property    Align;
    {@prop Anchors = ancestor /}
    property    Anchors;
    {@prop AutoSize = ancestor /}
    property    AutoSize;
    {@prop Behavior = ancestor /}
    property    Behavior;
    {@prop Borders = ancestor /}
    property    Borders;
    {@prop Center = ancestor /}
    property    Center;
    {@prop DataField = ancestor /}
    property    DataField;
    {@prop DataSource = ancestor /}
    property    DataSource;
    {@prop Font = ancestor /}
    property    Font;
    {@prop FriendlyName = ancestor /}
    property    FriendlyName;
    {@prop Holder = ancestor /}
    property    Holder;
    {@prop HoldStyle = ancestor /}
    property    HoldStyle;
    {@prop RealBounds = ancestor /}
    property    RealBounds;
    {@prop Scaled = ancestor /}
    property    Scaled;
    {@prop SecondHolder = ancestor /}
    property    SecondHolder;
    {@prop SecondHoldStyle = ancestor /}
    property    SecondHoldStyle;
    {@prop Stretch = ancestor /}
    property    Stretch;
    {@prop Transparent = ancestor /}
    property    Transparent;
    {@prop Visible = ancestor /}
    property    Visible;

    // events

    {@event AfterPrint = ancestor /}
    property    AfterPrint;
    {@event BeforePrint = ancestor /}
    property    BeforePrint;
    {@event OnMeasureHeight = ancestor /}
    property    OnMeasureHeight;
    property    OnGetImageIndex;
    property    MapImages      ;
    property    Images         ;

  end;

  TPrintDrawReportImage= procedure ( Sender:TObject; var PrintIt:boolean) of object;
  { TExtPrintColumn }

  TExtPrintColumn = class(TCollectionItem)
   private
     FLineBreak,
     FWidth   : Integer ;
     FResize   ,
     FVisible  : Boolean;
     FBreakCaption,
     FFormat ,
     FDBTitle ,
     FFieldName : String;
     FDatasource : TDatasource;
     FImages : TCustomImageList;
     FMapImages : TExtMapImages;
     procedure SetImages( const AValue : TCustomImageList );
     procedure SetMapImages( const AValue : TExtMapImages );
     function  GetFieldName: String;
   public
     constructor Create(ACollection: TCollection); override;
     property Datasource : TDataSource read FDatasource write FDatasource;
   published
     property Width     : Integer  read FWidth   write FWidth default 40;
     property Resize    : Boolean  read FResize  write FResize  default False;
     property Visible   : Boolean  read FVisible write FVisible default true;
     property LineBreak : Integer  read FLineBreak write FLineBreak default -1;
     property BreakCaption : String  read FBreakCaption write FBreakCaption;
     property DBTitle   : string   read FDBTitle write FDBTitle;
     property DisplayFormat    : string   read FFormat  write FFormat;
     property FieldName : string   read GetFieldName write FFieldName;
     property Images    : TCustomImageList read FImages    write SetImages;
     property MapImages : TExtMapImages    read FMapImages write SetMapImages;
   end;

  { TExtPrintColumns }
  TExtPrintColumns = class(TCollection)
  private
    FOwner : TComponent;
    function  GetColumn ( Index: Integer): TExtPrintColumn;
    procedure SetColumn ( Index: Integer; Value: TExtPrintColumn);
  public
    procedure SetDatasource(const AColumn: TExtPrintColumn); overload; virtual;
    procedure SetDatasource; overload; virtual;
    constructor Create(const AOwner: TComponent; const aItemClass: TCollectionItemClass); virtual;
    function Add: TExtPrintColumn;
    property Owner : TComponent read FOwner;
  {$IFDEF FPC}
  published
  {$ENDIF}
    property Items[Index: Integer]: TExtPrintColumn read GetColumn write SetColumn; default;
  end;


implementation

uses RLConsts,
     fonctions_proprietes,
     fonctions_reports,
{$IFDEF RX}
     rxdbgrid,
{$ENDIF}
{$IFDEF JEDI}
  jvDBGrid,
{$ENDIF}
     fonctions_images;

{ TExtReport }

procedure TExtReport.p_BeforePrintTexts(Sender:TObject; var OutputText:string; var PrintIt:boolean);
var ACellProps : TGetCellPropsEvent;
    ABack : TColor;
begin
  if  Assigned(FPrintComponent)
  and Assigned(fmet_getComponentMethodProperty(FPrintComponent,'OnGetCellProps').Code)
   Then
    with Sender as TRLCustomControl do
      Begin
        ACellProps:=TGetCellPropsEvent(fmet_getComponentMethodProperty(FPrintComponent,'OnGetCellProps'));
        ABack := Brush.Color;
        ACellProps ( Sender,
                     (fobj_getComponentObjectProperty(FPrintComponent,CST_DBPROPERTY_DATASOURCE) as TDataSource).DataSet.
                         FieldByName(fs_getComponentProperty(Sender,CST_DBPROPERTY_DATAFIELD)),
                     Font,
                     ABack);
        Brush.Color:=fcol_GetPrintedColor (ABack);
      end;
end;

constructor TExtReport.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FPrintComponent:=nil;
end;

{ TRLCustomImageList }

procedure TRLCustomImageList.SetImages( const AValue: TCustomImageList);
begin
  if fImages=AValue then Exit;
  fImages:=AValue;
end;

procedure TRLCustomImageList.LoadPicture;
begin
  p_DrawEventualImageFromListToBitmap ( Picture.Bitmap, fImages, fImageIndex, ClientWidth, ClientHeight, Color );
end;

procedure TRLCustomImageList.Notification(aComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(aComponent, Operation);
  if Operation <> opRemove
   Then Exit;
  if aComponent = fImages Then
   Images := nil;
end;

procedure TRLCustomImageList.InternalPrint;
begin
  LoadPicture;
  inherited InternalPrint;
end;

constructor TRLCustomImageList.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  fImages     := nil;
  fImageIndex := -1;
end;

{ TRLCustomDBExtImageList }

procedure TRLCustomDBExtImageList.SetImages(const Avalue: TCustomImageList);
begin
  if Avalue <> FImageList Then
   Begin
     FImageList:=Avalue;
   end;
end;

procedure TRLCustomDBExtImageList.LoadPicture;
var aimageIndex : Integer;
  f:TField;
begin
  f := GetField;
  if f = nil Then Exit;

  aimageIndex:=-1;
  if assigned ( FGetImageIndex )
   Then aimageIndex := FGetImageIndex ( Self, f )
   Else
    if Assigned(FMapImages) Then
     aimageIndex := FMapImages.ImageIndexOf ( f.Asstring )
    else
     if f is tNumericField Then
      Begin
        aimageIndex := f.AsInteger;
      end;

  p_DrawEventualImageFromListToBitmap ( Picture.Bitmap, FImageList, aimageIndex, ClientWidth, ClientHeight, Color );

end;

procedure TRLCustomDBExtImageList.Notification(aComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(aComponent, Operation);
  if Operation <> opRemove
   Then Exit;
  if aComponent = FImageList Then
   Images := nil;
  if aComponent = FMapImages Then
   MapImages := nil;
end;

constructor TRLCustomDBExtImageList.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FMapImages:=nil;
  FImageList   := nil;
  FGetImageIndex:=nil;
end;

{ TRLCustomDBExtImage }

constructor TRLCustomDBExtImage.Create(aOwner:TComponent);
begin
  fDataField :=emptystr;
  //fDataSource:=nil;
  //
  inherited Create(aOwner);
end;

procedure TRLCustomDBExtImage.LoadPicture;
var
  f:TField;
begin
  Picture.Graphic:=nil;
  f:=GetField;
  if (f<>nil) and not f.DataSet.Eof then
    p_FieldToImage ( f, Picture.Bitmap, Width, Height );
end;

procedure TRLCustomDBExtImage.Notification(aComponent:TComponent; Operation:TOperation);
begin
  inherited;
  //
  if Operation=opRemove then
    if aComponent=fDataSource then
    begin
      LoadPicture;
      Invalidate;
      fDataSource:=nil;
    end;
end;

procedure TRLCustomDBExtImage.SetDataSource(const aValue:TDataSource);
begin
  if aValue=fDataSource then
    Exit;
  fDataSource:=aValue;
  if aValue<>nil then
    aValue.FreeNotification(Self);
  LoadPicture;
  Invalidate;
end;

procedure TRLCustomDBExtImage.SetDataField(const aValue:TRLDataFieldProperty);
begin
  if aValue=fDataField then
    Exit;
  fDataField:=aValue;
  LoadPicture;
  AdjustBounds;
  Invalidate;
end;

function TRLCustomDBExtImage.GetDataSet:TDataSet;
begin
  if Assigned(fDataSource) then
    Result:=fDataSource.DataSet
  else
    Result:=nil;
end;

function TRLCustomDBExtImage.GetField:TField;
begin
  if (DataSet<>nil) and (DataSet.Active) and (fDataField<>emptystr) then
  begin
    Result:=DataSet.FindField(fDataField);
    if Result=nil then
      raise Exception.Create({$IFNDEF FPC}LocaleStrings.{$ENDIF}LS_NotFoundStr+': '+Name+'.DataField "'+fDataField+'"');
  end
  else
    Result:=nil;
end;

procedure TRLCustomDBExtImage.InternalPrint;
begin
  LoadPicture;
  //
  inherited;
end;


{ TExtPrintColumns }

function TExtPrintColumns.GetColumn(Index: Integer): TExtPrintColumn;
begin
  result := TExtPrintColumn( inherited Items[Index] );
end;

procedure TExtPrintColumns.SetColumn(Index: Integer;
   Value: TExtPrintColumn);
begin
  Items[Index].Assign( Value );
end;

procedure TExtPrintColumns.SetDatasource ( const AColumn: TExtPrintColumn );
begin
  AColumn.Datasource := fobj_getComponentObjectProperty(FOwner,CST_DBPROPERTY_DATASOURCE) as TDataSource;
end;

procedure TExtPrintColumns.SetDatasource;
var i : Integer;
begin
  for i := 0 to Count - 1 do
    SetDatasource(Items[i]);
end;

constructor TExtPrintColumns.Create(const AOwner: TComponent;
  const aItemClass: TCollectionItemClass);
begin
  Inherited create ( aItemClass );
  FOwner:=AOwner;
end;

function TExtPrintColumns.Add: TExtPrintColumn;
begin
  result := TExtPrintColumn (inherited Add);
  SetDatasource ( Result );
end;

{ TExtPrintColumn }

procedure TExtPrintColumn.SetImages(const AValue: TCustomImageList);
begin
  if AValue<> FImages then
   FImages := AValue;
end;

procedure TExtPrintColumn.SetMapImages(const AValue: TExtMapImages);
begin
  if AValue<> FMapImages then
   FMapImages := AValue;
end;

function TExtPrintColumn.GetFieldName: String;
begin
  if FFieldName <> ''
   Then Result:=FFieldName
   Else
    if    Assigned(FDatasource)
    and   Assigned(FDatasource.DataSet) Then
     with FDatasource.DataSet do
      if Active
      and ( Index < FieldDefs.Count )
       then
        Begin
          Result := FieldDefs [ Index ].Name;
          FFieldName:=Result;
        end;
end;

constructor TExtPrintColumn.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FWidth  :=40;
  FResize :=False;
  FVisible:=True;
  FImages := nil;
  FMapImages := nil;
  FDBTitle:='';
  FFieldName:='';
  FDatasource := nil;
  FLineBreak:=-1;
end;

{$IFDEF VERSIONS}
initialization
  p_ConcatVersion(gVer_reports_rlcomponents);
{$ENDIF}
end.
