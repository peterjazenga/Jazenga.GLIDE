unit u_extDBDirectoryEdit;

{$IFDEF FPC}
{$mode Delphi}{$H+}
{$ENDIF}

{$I ..\DLCompilers.inc}
{$I ..\extends.inc}

interface

uses
  Classes,
{$IFDEF VERSIONS}
  fonctions_version,
{$ENDIF}
  {$IFDEF FPC}
  LMessages, LCLType,EditBtn,
  {$ELSE}
  Messages, Windows,
  {$ENDIF}
  SysUtils, DB,
  DbCtrls, Graphics,
  u_extcomponent,
  StdCtrls,
  u_framework_components;


{$IFDEF VERSIONS}
const
    gVer_TExtSearchDBEdit : T_Version = ( Component : 'Composant TExtSearchDBEdit' ;
                                          FileUnit : 'U_TExtSearchDBEdit' ;
                                          Owner : 'Matthieu Giroux' ;
                                          Comment : 'Searching in a dbedit.' ;
                                          BugsStory :'1.0.0.0 : MyLabel unset correctly.' + #13#10
                                                    +'0.9.9.0 : Testing and adding dev image.' + #13#10
                                                    +'0.9.0.0 : Creating ExtDBDierectoryEdit.';
                                          UnitType : 3 ;
                                          Major : 1 ; Minor : 0 ; Release : 0 ; Build : 0 );

{$ENDIF}

type

  { TExtDBDirectoryEdit }

  TExtDBDirectoryEdit = class(TDirectoryEdit, IFWComponent, IFWComponentEdit)

  private
    FBeforeEnter, FBeforeExit : TNotifyEvent;
    FLabel : TLabel ;
    FOldColor ,
    FColorFocus ,
    FColorReadOnly,
    FColorEdit ,
    FColorLabel : TColor;
    FAlwaysSame : Boolean;
    FNotifyOrder : TNotifyEvent;
    FReadOnly:boolean;
    FDataLink: TFieldDataLink;
    procedure p_setLabel ( const alab_Label: TLabel );
    procedure WMPaint(var Message: {$IFDEF FPC}TLMPaint{$ELSE}TWMPaint{$ENDIF}); message {$IFDEF FPC}LM_PAINT{$ELSE}WM_PAINT{$ENDIF};
    function GetDataField: string;
    function GetDataSource: TDataSource;
    procedure SetDataField (const Value: string);
    procedure SetDataSource (Value: TDataSource);
    procedure DataChange (Sender: TObject);
    procedure UpdateData(Sender: TObject);
    procedure SetReadOnly(value:boolean);
  protected

    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure UTF8KeyPress(var UTF8Key: TUTF8Char); override;

  public
    {$IFDEF SPECIALEDIT}
    procedure EditChange; override;
    {$ELSE}
    procedure Change; override;
    {$ENDIF}
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure Loaded; override;
    procedure SetOrder ; virtual;
    property Directory;
   published
    property FWBeforeEnter : TnotifyEvent read FBeforeEnter write FBeforeEnter stored False;
    property FWBeforeExit  : TnotifyEvent read FBeforeExit  write FBeforeExit stored False ;
    property ColorLabel : TColor read FColorLabel write FColorLabel default CST_LBL_SELECT ;
    property ColorFocus : TColor read FColorFocus write FColorFocus default CST_EDIT_SELECT ;
    property ColorEdit : TColor read FColorEdit write FColorEdit default CST_EDIT_STD ;
    property ColorReadOnly : TColor read FColorReadOnly write FColorReadOnly default CST_EDIT_READ ;
    property Color stored False ;
    property MyLabel : TLabel read FLabel write p_setLabel;
    property AlwaysSame : Boolean read FAlwaysSame write FAlwaysSame default true;
    property OnOrder : TNotifyEvent read FNotifyOrder write FNotifyOrder;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly:boolean read FReadOnly write SetReadOnly default False;
  end;

implementation

uses fonctions_db,
     fonctions_components;

{ TExtDBDirectoryEdit }


procedure TExtDBDirectoryEdit.p_setLabel(const alab_Label: TLabel);
begin
  p_setMyLabel ( FLabel, alab_Label, Self );
end;

procedure TExtDBDirectoryEdit.SetOrder;
begin
  if assigned ( FNotifyOrder ) then
    FNotifyOrder ( Self );
end;

procedure TExtDBDirectoryEdit.DoEnter;
begin
  if assigned ( FBeforeEnter ) Then
    FBeforeEnter ( Self );
  // Si on arrive sur une zone de saisie, on met en valeur son tlabel par une couleur
  // de fond bleu et son libellé en marron (sauf si le libellé est sélectionné
  // avec la souris => cas de tri)
  p_setLabelColorEnter ( FLabel, FColorLabel, FAlwaysSame );
  p_setCompColorEnter  ( Self, FColorFocus, FAlwaysSame );
  inherited DoEnter;
end;

procedure TExtDBDirectoryEdit.DoExit;
begin
  if assigned ( FBeforeExit ) Then
    FBeforeExit ( Self );
  inherited DoExit;
  p_setLabelColorExit ( FLabel, FAlwaysSame );
  p_setCompColorExit ( Self, FOldColor, FAlwaysSame );

end;

procedure TExtDBDirectoryEdit.Loaded;
begin
  inherited Loaded;
  FOldColor := Color;
  if  FAlwaysSame
   Then
    Color := gCol_Edit ;
end;

procedure TExtDBDirectoryEdit.WMPaint(var Message: {$IFDEF FPC}TLMPaint{$ELSE}TWMPaint{$ENDIF});
Begin
  p_setCompColorReadOnly ( Self,FColorEdit,FColorReadOnly, FAlwaysSame, ReadOnly );
  inherited;
End;



function TExtDBDirectoryEdit.GetDataField: string;
begin
  Result := FDataLink.Fieldname;
end;

function TExtDBDirectoryEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TExtDBDirectoryEdit.SetDataField(const Value: string);
begin
  FDataLink.Fieldname := Value;
end;

procedure TExtDBDirectoryEdit.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

procedure TExtDBDirectoryEdit.UTF8KeyPress(var UTF8Key: TUTF8Char);

var
  CharKey: Char;
begin
  inherited UTF8KeyPress(UTF8Key);
  //If the pressed key is unicode then map the char to #255
  //Necessary to keep the TField.IsValidChar check
  if Length(UTF8Key) = 1 then
    CharKey := UTF8Key[1]
  else
    CharKey := #255;
  case CharKey of
    #8: // special keys
      if not FDatalink.Edit then
        UTF8Key:='';

    #32..#255: //standard keys
      if not FieldCanAcceptKey(FDataLink.Field, CharKey) or not FDatalink.Edit then
        UTF8Key:='';
  end;//case
  UpdateData(Self);
end;

procedure TExtDBDirectoryEdit.DataChange(Sender: TObject);
var DataLinkField : TField;
begin
  DataLinkField := FDataLink.Field;
  if DataLinkField <> nil then begin
    //use the right EditMask if any
    //EditMask := FDataLink.Field.EditMask; doesn't exist yet
    Alignment := DataLinkField.Alignment;

    //if we are focused its possible to edit,
    //if the field is currently modifiable
    {$IFNDEF SPECIALEDIT}
    if Focused and FDataLink.CanModify then begin
      //display the real Directory since we can modify it
      RestoreMask(DatalinkField.Text);
    end else
      //otherwise display the pretified/formated text since we can't
      DisableMask(DataLinkField.DisplayText);
    {$ENDIF}
    if (DataLinkField.DataType in [ftString, ftFixedChar, ftWidestring, ftFixedWideChar])
      and (MaxLength = 0) then
      MaxLength := DatalinkField.Size;
    Directory := DataLinkField.Text;
  end
  else begin
    //todo: uncomment this when TField implements EditMask
    //EditMask := ''
    Directory := '';
    MaxLength := 0;
  end;
end;

procedure TExtDBDirectoryEdit.UpdateData(Sender: TObject);
begin
  //the field is being updated, probably for post
  //so we are getting called to make sure its
  //up-to-date and matches any modifications
  //since its possible to have a mask for say
  //date or currency we need to make sure the
  //Directory is valid before we update this is in
  //case for instance they call table.post via
  //a keyboard shortcut while still focused, before
  //the changes have been validated
  ValidateEdit;
  FDataLink.Field.Text := Directory;
end;

procedure TExtDBDirectoryEdit.SetReadOnly(value: boolean);
begin
  if value<>FReadOnly then
    FReadOnly:=value;
end;

procedure TExtDBDirectoryEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation <> opRemove Then
    Exit;
  if AComponent = FLabel     then FLabel := nil;
  if (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

{$IFDEF SPECIALEDIT}
procedure TExtDBDirectoryEdit.EditChange;
{$ELSE}
procedure TExtDBDirectoryEdit.Change;
{$ENDIF}
begin
  inherited;
  UpdateData(Self);
end;

constructor TExtDBDirectoryEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink:=TFieldDataLink.Create;
  FDataLink.OnUpdateData:=UpdateData;
  FDataLink.OnDataChange:=DataChange;
  FAlwaysSame := True;
  FColorLabel := CST_LBL_SELECT;
  FColorEdit  := CST_EDIT_STD;
  FColorFocus := CST_EDIT_SELECT;
  FColorReadOnly := CST_EDIT_READ;
end;

destructor TExtDBDirectoryEdit.Destroy;
begin
  inherited Destroy;
  FDataLink.Destroy;
end;

end.

