unit u_extcustomimages;

interface

{$i ..\extends.inc}
{$IFDEF FPC}
{$Mode Delphi}
{$ENDIF}

uses
{$IFDEF VERSIONS}
  fonctions_version,
{$ENDIF}
  Classes, U_ExtMapImageIndex,
  Controls, U_ExtImage,
{$IFDEF TNT}
  TntExtCtrls, TntStdCtrls,
{$ELSE}
  ExtCtrls, StdCtrls,
  u_framework_components,
  fonctions_ini,
{$ENDIF}
{$IFDEF FPC}
  LMessages, Dialogs,
{$ELSE}
  Messages,
{$ENDIF}
  u_extcomponent, Forms;

const
  CST_DEFAULT_PANEL_WIDTH = 130;
  CST_DEFAULT_PANEL_HEIGHT = 130;
  CST_DEFAULT_PER_LINE = 4;
  CST_DEFAULT_VISIBLE_LINES = 4;
  CST_INI_EXT_IN    = 'In';
{$IFDEF VERSIONS}
  gVer_TExtImages : T_Version = ( Component : 'Composant TExtCustomImages' ;
                                  FileUnit : 'U_ExtCustomImages' ;
                                  Owner : 'Matthieu Giroux' ;
                                  Comment : 'Présentation d''images de tous types avec résumé intégré dans le répertoire.' ;
                                  BugsStory : '1.0.0.0 : Scrolling and working.' + #13#10 +
                                              '0.9.0.0 : Creating from TExtImage.';
                                  UnitType : 3 ;
                                  Major : 1 ; Minor : 0 ; Release : 0 ; Build : 0 );

{$ENDIF}

type
  TImageForm = class;
  TExtDummyPanelImage =  class(TExtMapImageIndex)
   private
    FForm : TImageForm;
  end;

  TExtCustomCollectionImages = class;

  { TImageForm }

  TImageForm = class(TForm)
  private
    FOwner : TExtDummyPanelImage;
    procedure WMKillFocus(var Message: {$IFDEF FPC}TLMKillFocus{$ELSE}TWMKillFocus{$ENDIF}); message {$IFDEF FPC}LM_KILLFOCUS{$ELSE}WM_KillFocus{$ENDIF};
  public
    destructor Destroy; override;
  end;

  { TExtCustomPanelImage }

  TExtCustomPanelImage = class(TExtDummyPanelImage)
  private
    FForm: TImageForm;
    FMemo  : {$IFDEF TNT}TTntCustomMemo{$ELSE}TCustomMemo{$ENDIF};
    FPanelMemo,
    FPanel : {$IFDEF TNT}TTntPanel{$ELSE}TPanel{$ENDIF};
    FImage : TExtImage;
    procedure SetMemo ( AValue : TCustomMemo ); virtual;
    procedure SetPanel ( AValue : {$IFDEF TNT}TTntPanel{$ELSE}TPanel{$ENDIF} ); virtual;
    procedure SetPanelMemo ( AValue : {$IFDEF TNT}TTntPanel{$ELSE}TPanel{$ENDIF} ); virtual;
    procedure SetImage ( AValue : TExtImage ); virtual;
  protected
    function CreateMemo:{$IFDEF TNT}TTntCustomMemo{$ELSE}TCustomMemo{$ENDIF}; virtual;
    function CreateImage: TExtImage; virtual;
    procedure EraseMemo; virtual;
    procedure OnClickImage ( Sender : TObject ); virtual;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure CreateComponentsPanelLine; virtual;
    procedure InitComponentsPanelLine; virtual;
    property Form : TImageForm read FForm;
  published
    property Memo  : {$IFDEF TNT}TTntCustomMemo{$ELSE}TCustomMemo{$ENDIF} read FMemo write SetMemo;
    property Panel : {$IFDEF TNT}TTntPanel{$ELSE}TPanel{$ENDIF} read FPanel write SetPanel;
    property PanelMemo : {$IFDEF TNT}TTntPanel{$ELSE}TPanel{$ENDIF} read FPanelMemo write SetPanelMemo;
    property Image : TExtImage read FImage write SetImage;
  End;

  TExtPanelImageClass = class of TExtCustomPanelImage;

  { TExtCustomCollectionImages }

  TExtCustomCollectionImages = class(TExtMapImagesColumns)
  private
    FMemoLine  : {$IFDEF TNT}TTntCustomMemo{$ELSE}TCustomMemo{$ENDIF};
    FPanelLine: TPanel;
    FCounter,
    FCounterPerLine : Integer;
    procedure SetMemoLine ( AValue : {$IFDEF TNT}TTntCustomMemo{$ELSE}TCustomMemo{$ENDIF} ); virtual;
    procedure SetPanelLine ( AValue : {$IFDEF TNT}TTntPanel{$ELSE}TPanel{$ENDIF} ); virtual;
    function GetPanelImage( Index: Integer): TExtCustomPanelImage;
    procedure SetPanelImage( Index: Integer; Value: TExtCustomPanelImage);
  protected
    function MemoLineCreate:{$IFDEF TNT}TTntCustomMemo{$ELSE}TCustomMemo{$ENDIF}; virtual; abstract;
  public
    constructor Create(AComponent: TComponent; ColumnClass: TExtMapImageIndexClass); override;
    procedure DestroyComponents; virtual;
    procedure CreatePanelLine; virtual;
    procedure InitPanelLine; virtual;
    function Add: TExtCustomPanelImage; virtual;
    property Items[Index: Integer]: TExtCustomPanelImage read GetPanelImage write SetPanelImage; default;
    property CounterPerLine  : Integer read FCounterPerLine write FCounterPerLine;
    property Counter  : Integer read FCounter write FCounter;
  published
    property MemoLine  : {$IFDEF TNT}TTntCustomMemo{$ELSE}TCustomMemo{$ENDIF} read FMemoLine write SetMemoLine;
    property PanelLine : {$IFDEF TNT}TTntPanel{$ELSE}TPanel{$ENDIF} read FPanelLine write SetPanelLine;
  End;

{ TExtCustomImages }

   TExtCustomImages = class( {$IFDEF TNT}TTntScrollBox{$ELSE}TScrollBox{$ENDIF}, IFWComponent )
     private
       FScrolling,
       FClickEdit,
       FIniFileActive,
       FShowErrors:Boolean;
       FImagesFormat : Single ;
       FPerLine, FVisibleLines,
       FPanelWidth, FPanelHeight : Integer;
       FImagesColumns : TExtCustomCollectionImages;
       FExtensionsIn : TStrings;
       FOnImagesDestroy,
       FOnImagesCreate,
       FOnImagesAdd ,
       FOnImagesDelete : TNotifyEvent;
       procedure SetColumns ( AValue : TExtCustomCollectionImages );
       procedure p_SetImagesFormat ( AValue : Single  );
       procedure p_SetPerLine      ( AValue : Integer );
       procedure p_SetExtensionsIn ( AValue : TStrings );
       procedure p_SetPanelWidth   ( AValue : Integer );
       procedure p_SetPanelHeight  ( AValue : Integer );
       procedure p_SetVisibleLines ( AValue : Integer );
       procedure SetExtensionsIn(AValue: TStrings);
       procedure WMHScroll(var Message : TLMHScroll); message LM_HScroll;
       procedure WMVScroll(var Message : TLMVScroll); message LM_VScroll;
       procedure WMSize(var Message: TLMSize); message LM_Size;
      protected
       procedure ReSizeImages(var Message: TLMSize); virtual;
       procedure ScrollVBefore(var Message: TLMVScroll); virtual;
       procedure ScrollVSetPos(var Message: TLMVScroll); virtual;
       procedure ScrollHBefore(var Message: TLMVScroll); virtual;
       procedure ScrollHSetPos(var Message: TLMVScroll); virtual;
       function MemoCreate:{$IFDEF TNT}TTntCustomMemo{$ELSE}TCustomMemo{$ENDIF}; virtual; abstract;
       function CreateImagesMap : TExtCustomCollectionImages; virtual; abstract;
       function  DeleteImagesTopPanel:Boolean; virtual;
       procedure DeleteImagesDownPanel; virtual;
       function  DeleteLinesTopPanel:Boolean; virtual;
       function  CanScroll:Boolean; virtual;
       procedure DeleteLinesDownPanel; virtual;
       function  AddImagesDownPanel:Boolean; virtual;
       function  AddLinesTopPanel:Boolean; virtual;
       procedure AddDownPanel; virtual; abstract;
     public
       procedure CreateSomeLine; virtual;
       procedure Loaded;override;
       procedure Click;override;
       procedure ClickDoEdit;virtual;
       procedure RenameImage(const AOldPath, ANewPath : String ; const AnOptionalPanelImage : TExtCustomPanelImage);virtual;
       procedure CreateResume(const As_PathOfImage, As_Resume : String );virtual;
       constructor Create(AOwner: TComponent); override;
       destructor Destroy; override;
       procedure DestroyComponents; virtual;
       procedure EraseMemos; virtual;
       function IndexOf ( const AValue : String ): Integer; virtual;
       function ImageIndexOf ( const AValue : String ): Integer; virtual;
       function IsValidFile ( as_filename : String ):Boolean;
     published
     { Published declarations }
       property Columns : TExtCustomCollectionImages read FImagesColumns write SetColumns ;
       property ExtensionsIn : TStrings read FExtensionsIn write SetExtensionsIn ;
       property OnImagesDestroy: TNotifyEvent read FOnImagesDestroy write FOnImagesDestroy ;
       property OnImagesCreate : TNotifyEvent read FOnImagesCreate  write FOnImagesCreate ;
       property OnImagesAdd    : TNotifyEvent read FOnImagesAdd     write FOnImagesAdd ;
       property OnImagesDelete : TNotifyEvent read FOnImagesDelete  write FOnImagesDelete ;
       property ShowErrors : Boolean read FShowErrors write FShowErrors default True ;
       property ClickEdit : Boolean read FClickEdit write FClickEdit default True ;
       property ImageRatio : Single read FImagesFormat write p_SetImagesFormat;
       property ImagesPerLine  : Integer read FPerLine write p_SetPerLine default CST_DEFAULT_PER_LINE;
       property VisibleLines  : Integer read FVisibleLines write p_SetVisibleLines default CST_DEFAULT_VISIBLE_LINES;
       property IniFileActive : Boolean read FIniFileActive write FIniFileActive default True ;
       property PanelWidth  : Integer read FPanelWidth write p_SetPanelWidth default CST_DEFAULT_PANEL_WIDTH;
       property PanelHeight : Integer read FPanelHeight write p_SetPanelHeight default CST_DEFAULT_PANEL_HEIGHT;
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
     strutils,
     fonctions_file,
     ExtDlgs;

{ TImageForm }

procedure TImageForm.WMKillFocus(var Message: TLMKillFocus);
begin
  inherited;
  Destroy;
end;

destructor TImageForm.Destroy;
begin
  inherited Destroy;
  FOwner.FForm:=nil;
end;

{ TExtCustomCollectionImages }

procedure TExtCustomCollectionImages.SetMemoLine(AValue: TCustomMemo);
begin
  FMemoLine:=AValue;
end;


procedure TExtCustomCollectionImages.DestroyComponents;
begin
  while Count > 0 do
    Items [ 0 ].Destroy;
  FCounter:=0;
  FCounterPerLine:=0;
end;

procedure TExtCustomCollectionImages.CreatePanelLine;
begin
  with Component as TExtCustomImages do
    Begin
      FCounterPerLine := 0;
      FPanelLine :=  {$IFDEF TNT}TTntPanel{$ELSE}TPanel{$ENDIF}.Create(Component);
      InitPanelLine;
    end;
end;

procedure TExtCustomCollectionImages.InitPanelLine;
begin
  with Component as TExtCustomImages,FPanelLine do
    Begin
      Parent := Component as TWinControl;
      Visible := True;
      if FPerLine = 0
       Then Top := 0
       Else Top := ( FCounter div FPerLine ) * PanelHeight + 5;
      Height := PanelHeight;
      Align := alTop;
      // Creating an unique Tag
      if ( Count = 0 )
       Then Tag := 0
       Else Tag := FImagesColumns [ Count - 1 ].Panel.Parent.Tag + 1;
    end;
end;

function TExtCustomCollectionImages.Add: TExtCustomPanelImage;
begin
  with Component as TExtCustomImages do
   if  (FPerLine>0)
   and (FCounterPerLine = FPerLine)
    Then CreatePanelLine;
  Result := TExtCustomPanelImage(inherited Add);
end;


procedure TExtCustomCollectionImages.SetPanelLine(AValue: TPanel);
begin
  FPanelLine:=AValue;

end;

function TExtCustomCollectionImages.GetPanelImage(Index: Integer
  ): TExtCustomPanelImage;
begin
  Result := TExtCustomPanelImage(inherited Items[Index]);

end;

procedure TExtCustomCollectionImages.SetPanelImage(Index: Integer;
  Value: TExtCustomPanelImage);
begin
  Items[Index].Assign(Value);
end;

constructor TExtCustomCollectionImages.Create(AComponent: TComponent; ColumnClass: TExtMapImageIndexClass);
begin
  inherited;
  FPanelLine := nil;
  FMemoLine  := nil;
  FCounterPerLine := 0;
  FCounter := 0;
end;

{ TExtCustomPanelImage }

procedure TExtCustomPanelImage.SetMemo(AValue: TCustomMemo);
begin
  FMemo.Assign(AValue);
end;

procedure TExtCustomPanelImage.SetPanel(AValue: {$IFDEF TNT}TTntPanel{$ELSE}TPanel{$ENDIF});
begin
  FPanel.Assign(AValue);

end;

procedure TExtCustomPanelImage.SetPanelMemo(AValue: {$IFDEF TNT}TTntPanel{$ELSE}TPanel{$ENDIF});
begin
  FPanelMemo.Assign(AValue);

end;

procedure TExtCustomPanelImage.SetImage(AValue: TExtImage);
begin
  FImage.Assign(AValue);

end;

function TExtCustomPanelImage.CreateMemo: TCustomMemo;
begin
  Result := TFWMemo.Create ((Collection as TExtCustomCollectionImages).Component);
end;

// function TExtPanelImage.CreateImage
// creates un image
function TExtCustomPanelImage.CreateImage: TExtImage;
begin
  Result := TExtImage.Create ((Collection as TExtCustomCollectionImages).Component);
end;

procedure TExtCustomPanelImage.EraseMemo;
begin
  Memo.Lines.Text := '';
end;

procedure TExtCustomPanelImage.OnClickImage(Sender: TObject);
var APoint : TPoint;
begin
  if not assigned (FForm) Then
   Begin
    FForm := TImageForm.Create ( Collection.Owner as TComponent );
    FForm.FOwner:=Self;
    FForm.AutoSize:=True;
   end;
  APoint.X := FImage.Left;
  APoint.Y := FImage.Top;
  APoint := FImage.ClientToScreen(APoint);
  FForm.Left:=APoint.X;
  FForm.Top :=APoint.Y;
end;

constructor TExtCustomPanelImage.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  CreateComponentsPanelLine;
  InitComponentsPanelLine;
  FForm := nil;
end;

procedure TExtCustomPanelImage.CreateComponentsPanelLine;
begin
  with Collection as TExtCustomCollectionImages,Component as TExtCustomImages do
   Begin
    FPanel := {$IFDEF TNT}TTntPanel{$ELSE}TPanel{$ENDIF}.Create(Component);
    with FPanel do
      Begin
       Parent:=FPanelLine;
       Visible:=True;
      end;
    FPanelMemo := {$IFDEF TNT}TTntPanel{$ELSE}TPanel{$ENDIF}.Create(Component);
    with FPanelMemo do
      Begin
       Parent:=FPanel;
       Visible:=True;
      end;
    FMemo := CreateMemo;
    with FMemo do
      Begin
       Parent:=FPanelMemo;
       Visible:=True;
      end;
    FImage := CreateImage;
    with FImage do
      Begin
       Parent:=FPanel;
       Visible:=True;
       OnClick:=OnClickImage;
      end;
    inc (FCounterPerLine);
    inc (FCounter);
   end;
end;


procedure TExtCustomPanelImage.InitComponentsPanelLine;
begin
  with (Collection as TExtCustomCollectionImages),Component as TExtCustomImages do
   Begin
    with FMemo do
      Begin
       Align:=alClient;
       Font.Size := 10;
      end;
    with FPanelMemo do
      Begin
       Align:=alClient;
      end;
    with FPanel do
      Begin
       Align:=alLeft;
       Width := PanelWidth;
      end;
    with FImage do
      Begin
       Align:=alTop;
       Height := Round(PanelWidth / ImageRatio);
       Stretch:=True;
       Proportional:=True;
      end;
   End;
end;

destructor TExtCustomPanelImage.Destroy;
begin
  inherited Destroy;
  FMemo .Destroy;
  FPanel.Destroy;
  FImage.Destroy;
end;

{ TExtCustomImages }

procedure TExtCustomImages.Click;
begin
  if Assigned(OnClick) Then
    inherited Click
   Else
     if not ( csDesigning in ComponentState)
     and FClickEdit
      Then
        try
        finally
        end;
end;

procedure TExtCustomImages.ClickDoEdit;
begin

end;

// Renaming
procedure TExtCustomImages.RenameImage(const AOldPath, ANewPath: String; const AnOptionalPanelImage : TExtCustomPanelImage);
begin

end;

function TExtCustomImages.DeleteImagesTopPanel:Boolean;
begin
  if assigned ( FOnImagesDelete )
   Then         FOnImagesDelete ( Self);
end;

procedure TExtCustomImages.DeleteImagesDownPanel;
begin
  if assigned ( FOnImagesDelete )
   Then         FOnImagesDelete ( Self);
end;

function TExtCustomImages.DeleteLinesTopPanel:Boolean;
begin
  Result := False;
  if assigned ( FOnImagesDelete )
   Then         FOnImagesDelete ( Self);
end;

function TExtCustomImages.CanScroll: Boolean;
begin
  Result := True;
end;

procedure TExtCustomImages.DeleteLinesDownPanel;
begin
  if assigned ( FOnImagesDelete )
   Then         FOnImagesDelete ( Self);
end;
procedure TExtCustomImages.ScrollHBefore(var Message: TLMVScroll);
Begin
  with Message do
   if ImagesPerLine = 0 Then
    Begin
       // Défilement vers le bas
      if ScrollCode mod 2 = 1 Then
        Begin
          if  ( Pos > HorzScrollBar.Size / 2 ) Then
              CreateSomeLine;
        end
      Else
       if Pos <  HorzScrollBar.Size / 2
        Then
         DeleteImagesDownPanel;
    end;
End;
procedure TExtCustomImages.ScrollHSetPos(var Message: TLMVScroll);
Begin
  with Message do
   if ImagesPerLine = 0 Then
    Begin
       // Défilement vers le bas
      if ScrollCode mod 2 = 1 Then
        Begin
          if ( Pos > HorzScrollBar.Size / 2 ) Then
           Begin
            if Pos > PanelWidth Then
             if DeleteImagesTopPanel
              Then
               with HorzScrollBar do
                 Position := Position - PanelWidth;
            End
             Else
              if (Pos = 0)
              and AddImagesDownPanel then
                with HorzScrollBar do
                  Position := PanelWidth;
       end;

    end;
end;

procedure TExtCustomImages.WMHScroll(var Message: TLMHScroll);
begin
  if FScrolling Then
   Exit;
  FScrolling:=True;
  try
    if CanScroll Then
     Begin
      ScrollHBefore ( Message );
      inherited;
      ScrollHSetPos ( Message );
     end;
  finally
   FScrolling := False;
  end;
end;

procedure TExtCustomImages.ScrollVBefore(var Message: TLMVScroll);
Begin
  with Message do
   if ImagesPerLine > 0 Then
    Begin
       // Défilement vers le bas
      if ScrollCode mod 2 = 1 Then
        Begin
          if  ( Pos > VertScrollBar.Size / 2 ) Then
             CreateSomeLine;
        end
      Else
       if Pos <  VertScrollBar.Size / 2
        Then
           DeleteLinesDownPanel;
    end;
end;
procedure TExtCustomImages.ScrollVSetPos(var Message: TLMVScroll);
Begin
  with Message do
   if ImagesPerLine > 0 Then
    Begin
       // Défilement vers le bas
      if ScrollCode mod 2 = 1 Then
        Begin
          if ( Pos > VertScrollBar.Size / 2 ) Then
           Begin
             if DeleteLinesTopPanel
              Then
               with VertScrollBar do
                 Position := Position - PanelHeight;
           end;
       end;
     if (Pos = 0)
     and AddLinesTopPanel then
       with VertScrollBar do
         Position := PanelHeight;
    end;
End;
procedure TExtCustomImages.WMVScroll(var Message: TLMVScroll);
var li_i : Integer;
begin
  if FScrolling Then
   Exit;
  FScrolling:=True;
  try
    if CanScroll Then
     Begin
      ScrollVBefore ( Message );
      inherited;
      ScrollVSetPos ( Message );

     end;
  finally
   FScrolling := False;
  end;
end;

procedure TExtCustomImages.WMSize(var Message: TLMSize);
begin
  ReSizeImages(Message);
  inherited;
end;

procedure TExtCustomImages.ReSizeImages(var Message: TLMSize);
begin
  if FScrolling Then
    Exit;
  with Columns do
  if (ImagesPerLine>0) Then
    try
      FScrolling := True;
      if Message.Height < Counter / ImagesPerLine * PanelHeight * VisibleLines
       Then CreateSomeLine
       Else DeleteLinesTopPanel;
    finally
     FScrolling := False;
    end
  Else
  try
    FScrolling := True;
    if Message.Width < Counter * PanelWidth * VisibleLines
     Then CreateSomeLine
     Else DeleteImagesTopPanel;
  finally
   FScrolling := False;
  end;
end;

function TExtCustomImages.AddImagesDownPanel:Boolean;
begin
  if assigned ( FOnImagesAdd )
   Then         FOnImagesAdd ( Self);
end;

function TExtCustomImages.AddLinesTopPanel:Boolean;
begin
  Result := False;
  if assigned ( FOnImagesAdd )
   Then         FOnImagesAdd ( Self);
end;

procedure TExtCustomImages.CreateSomeLine;
begin
  if assigned ( FOnImagesCreate )
   Then         FOnImagesCreate ( Self);
end;

procedure TExtCustomImages.Loaded;
begin
  if fIniFileActive then
   Begin
    f_GetMemIniFile();
    LitTstringsDeIni(FIniFile,Owner.Name+'.'+Name+CST_INI_EXT_IN,FExtensionsIn);
   end;
  inherited Loaded;
  VertScrollBar.Increment:=FPanelHeight;
  HorzScrollBar.Increment:=FPanelWidth;
end;

// initing panel and images' columns
constructor TExtCustomImages.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FShowErrors:=True;
  FClickEdit:=True;
  FImagesFormat := 16/9;
  FScrolling := False;
  FPanelWidth  := CST_DEFAULT_PANEL_WIDTH;
  FPanelHeight := CST_DEFAULT_PANEL_HEIGHT;
  FPerLine     := CST_DEFAULT_PER_LINE;
  FImagesColumns := CreateImagesMap;
  FVisibleLines:=CST_DEFAULT_VISIBLE_LINES;
  FExtensionsIn := TStringList.Create;
  FExtensionsIn.Add ( 'jpg' );
  FExtensionsIn.Add ( 'jpeg' );
  FExtensionsIn.Add ( 'png' );
  FExtensionsIn.Add ( 'gif' );
  FIniFileActive := True;
  Caption := '';
end;

destructor TExtCustomImages.Destroy;
begin
  inherited Destroy;
  FExtensionsIn.Destroy;
end;

procedure TExtCustomImages.DestroyComponents;
begin
  FImagesColumns.DestroyComponents;
  while ControlCount > 0 do
    Controls [ 0 ].Destroy;
  FImagesColumns.CreatePanelLine;
  VertScrollBar.Position:=0;
  HorzScrollBar.Position:=0;
  if assigned ( FOnImagesDestroy )
   Then
     FOnImagesDestroy ( Self );
end;

procedure TExtCustomImages.EraseMemos;
var li_i : Integer;
begin
  for li_i := 0 to FImagesColumns.Count - 1 do
    FImagesColumns [ li_i ].EraseMemo;
end;

// Get the Index of Image in Collection
function TExtCustomImages.IndexOf(const AValue: String): Integer;
var i : Integer;
begin
  Result:=-1;
  for i := 0 to FImagesColumns.Count - 1 do
   if AValue = FImagesColumns [ i ].Value Then
    Begin
      Result := i ;
    end;
end;

// Get the ImageIndex of Image
function TExtCustomImages.ImageIndexOf(const AValue: String): Integer;
begin
  Result := IndexOf(AValue);
  if Result >= 0 Then
    Result:=Columns [ Result ].ImageIndex;
end;

function TExtCustomImages.IsValidFile(as_filename: String): Boolean;
var li_iposext : Integer;
begin
  if as_filename = '' Then
   Exit;
  Result := False;
  as_filename := lowercase (as_filename);
  for li_iposext := 0 to FExtensionsIn.Count - 1 do
   if posex ( '.'+FExtensionsIn [ li_iposext ], as_filename, Length(as_filename)-6 ) > 0 Then
    Begin
     Result := True;
     Exit;
    end;
end;

procedure TExtCustomImages.CreateResume(const As_PathOfImage, As_Resume: String);
begin
end;

procedure TExtCustomImages.SetColumns(AValue: TExtCustomCollectionImages);
begin
  FImagesColumns.Assign(AValue);
end;

procedure TExtCustomImages.p_SetImagesFormat(AValue: Single);
begin
  if Avalue <= 0
   Then
    Begin
     if csDesigning in ComponentState Then
       MessageDlg('ImagesFormat can''t be less or equal to zero.', mtWarning, [mbOK], 0 );
     Avalue := 0 / 1;
    end;
  FImagesFormat := Avalue;
end;

procedure TExtCustomImages.p_SetPerLine(AValue: Integer);
begin
  if Avalue < 0
   Then
    Begin
     if csDesigning in ComponentState Then
       MessageDlg('PerLine can''t be less to zero.', mtWarning, [mbOK], 0 );
     Avalue := 0;
    end;
  FPerLine := Avalue;
end;

procedure TExtCustomImages.p_SetExtensionsIn(AValue: TStrings);
begin
  FExtensionsIn.Assign ( Avalue );
end;

procedure TExtCustomImages.p_SetPanelWidth(AValue: Integer);
begin
  if Avalue < 1
   Then
    Begin
     if csDesigning in ComponentState Then
       MessageDlg('PanelWidth can''t be less or equal to zero.', mtWarning, [mbOK], 0 );
     Avalue := 1;
    end;
  FPanelWidth := Avalue;
end;

procedure TExtCustomImages.p_SetPanelHeight(AValue: Integer);
begin
  if Avalue < 1
   Then
    Begin
     if csDesigning in ComponentState Then
       MessageDlg('PanelHeight can''t be less or equal to zero.', mtWarning, [mbOK], 0 );
     Avalue := 1;
    end;
  FPanelHeight := Avalue;
end;

procedure TExtCustomImages.p_SetVisibleLines(AValue: Integer);
begin
  if AValue < 2
   Then
    Begin
      if csDesigning in ComponentState Then
        MessageDlg('VisibleLines must be equal or higher to 2.', mtWarning, [mbOK], 0);
      AValue:=2;
    end;
  FVisibleLines:=AValue;
end;

procedure TExtCustomImages.SetExtensionsIn(AValue: TStrings);
begin
  if ( AValue = nil)
  or (FExtensionsIn=AValue) then Exit;
  FExtensionsIn.Text:=lowercase(AValue.Text);
  if not ( csDesigning in ComponentState )
  and finifileactive Then
   Begin
    f_GetMemIniFile();
    if assigned ( FInifile ) Then
     Begin
       SauveTStringsDansIni(FIniFile,Owner.Name+'.'+Name+CST_INI_EXT_IN, FExtensionsIn);
       fb_iniWriteFile( FIniFile );
     end;
   end;
end;


{$IFDEF VERSIONS}
initialization
  p_ConcatVersion ( gVer_TExtImages );
{$ENDIF}
end.
