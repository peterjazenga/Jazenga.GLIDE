unit u_extp_dbcomponents;

{*********************************************************************}
{                                                                     }
{                                                                     }
{             TFWDBEdit, TFWDBDateEdit, TFWDBLookupCombo,             }
{             TFWDBDateTimePicker, TFWDBMemo :                        }
{             Composants avec couleurs de focus, d'édition            }
{             Créateur : Matthieu Giroux                              }
{             31 Mars 2011                                            }
{                                                                     }
{                                                                     }
{*********************************************************************}


{$I ..\DLCompilers.inc}
{$I ..\extends.inc}
{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////
//
////////////////////////////////////////////////////////////////////////////////

interface

uses
  SysUtils, Classes, db, FileUtil,
{$IFDEF UseRuntime}
  Ext, ExtPascal, ExtForm,
  ExtData, ExtGrid, ExtUtil, ExtAir, ExtDd,
  ExtMenu,  ExtState;

type
  {$M+}
  TExtPanel_Tab = TExtPanel;
  TExtFormTextField_Grid = TExtFormTextField;
  TExtFormNumberField_Grid = TExtFormNumberField;
  TExtFormDateField_Grid = TExtFormDateField;
  TExtFormTimeField_Grid = TExtFormTimeField;
  TExtFormCheckbox_Grid = TExtFormCheckbox;
  TExtFormComboBox_Grid = TExtFormComboBox;
  {$M-}

{$ELSE}
  ExtP_Design_Ctrls, ExtP_Design_Grid;
{$ENDIF}

{$IFDEF VERSIONS}
const
    gVer_framework_DBcomponents : T_Version = ( Component : 'Data interactivity components' ;
                                               FileUnit : 'u_framework_dbcomponents' ;
                                               Owner : 'Matthieu Giroux' ;
                                               Comment : 'U_CustomFrameWork Data interactivity.' ;
                                               BugsStory : '0.9.9.9 : ExtPascal.'
                                                         + '0.9.9.0 : MyLabel unset correctly.'
                                                         + '0.9.0.6 : FWDBSpinEdit working.'
                                                         + '0.9.0.5 : UTF 8.'
                                                         + '0.9.0.4 : Added FWDBCombobox.'
                                                         + '0.9.0.3 : Using RXLookupCombo on FPC.'
                                                         + '0.9.0.2 : Paint Edits or not on FWDBGrid.'
                                                         + '0.9.0.1 : FWDBGrid tested on Delphi, with Controls on Columns.'
                                                         + '0.9.0.0 : Création à partir de u_framework_components.';
                                               UnitType : 3 ;
                                               Major : 0 ; Minor : 9 ; Release : 9 ; Build : 9 );

{$ENDIF}
type

{ TFWDBEdit }

   TFWDBEdit = class ( TExtFormTextField, IFWComponent, IFWComponentEdit )
      private
       FBeforeEnter, FBeforeExit : TNotifyEvent;
       FLabel : TLabel ;
       FOldColor ,
       FColorFocus ,
       FColorReadOnly,
       FColorEdit ,
       FColorLabel : TColor;
       FOnPopup : TNotifyEvent;
       FAlwaysSame : Boolean;
       FNotifyOrder : TNotifyEvent;
       procedure p_setLabel ( const alab_Label: TLabel );
       procedure WMPaint(var Message: {$IFDEF FPC}TLMPaint{$ELSE}TWMPaint{$ENDIF}); message {$IFDEF FPC}LM_PAINT{$ELSE}WM_PAINT{$ENDIF};
      protected
       procedure MouseDown( Button : TMouseButton; Shift : TShiftState; X,Y : Integer); override;
       procedure Notification(AComponent: TComponent; Operation: TOperation); override;
      public

       constructor Create ( AOwner : TComponent ); override;
       procedure DoEnter; override;
       procedure DoExit; override;
       procedure Loaded; override;
       procedure SetOrder ; virtual;
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
       property OnPopup : TNotifyEvent read FOnPopup write FOnPopup;
       property OnMouseEnter;
       property OnMouseLeave;
       property PopupMenu;
       property Align;
     End;

   { TFWDBDateEdit }

   TFWDBDateEdit = class ( TExtFormDateField, IFWComponent, IFWComponentEdit )
      private
       FBeforeEnter, FBeforeExit : TNotifyEvent;
       FLabel: TLabel ;
       FOldColor ,
       FColorFocus ,
       FColorReadOnly,
       FColorEdit ,
       FColorLabel : TColor;
       FAlwaysSame : Boolean;
       FNotifyOrder : TNotifyEvent;
       FOnPopup : TNotifyEvent;
       procedure p_setLabel ( const alab_Label: TLabel );
       procedure WMPaint(var Message: {$IFDEF FPC}TLMPaint{$ELSE}TWMPaint{$ENDIF}); message {$IFDEF FPC}LM_PAINT{$ELSE}WM_PAINT{$ENDIF};
      protected
       procedure MouseDown( Button : TMouseButton; Shift : TShiftState; X,Y : Integer); override;
       procedure Notification(AComponent: TComponent; Operation: TOperation); override;
      public

       constructor Create ( AOwner : TComponent ); override;
       procedure DoEnter; override;
       procedure DoExit; override;
       procedure Loaded; override;
       procedure SetOrder ; virtual;
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
       property OnPopup : TNotifyEvent read FOnPopup write FOnPopup;
       property OnMouseEnter;
       property OnMouseLeave;
       property PopupMenu;
       property Align;
     End;





    { TFWDBFileEdit }

    TFWDBFileEdit = class ( TFWFileEdit )
       private
          FReadOnly : Boolean;
          FOnPopup : TNotifyEvent;
          FDataLink: TFieldDataLink;
          function GetDataField: string;
          function GetDataSource: TDataSource;
          function GetField: TField;
          procedure WMCut(var Message: TMessage); message {$IFDEF FPC} LM_CUT {$ELSE} WM_CUT {$ENDIF};
          procedure WMPaste(var Message: TMessage); message {$IFDEF FPC} LM_PASTE {$ELSE} WM_PASTE {$ENDIF};
          procedure CMExit(var Message: {$IFDEF FPC} TLMExit {$ELSE} TCMExit {$ENDIF}); message CM_EXIT;
          procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
       protected
          procedure Refresh; virtual;
          {$IFDEF FPC}
          procedure UTF8KeyPress(var UTF8Key: TUTF8Char); override;
          {$ELSE}
          procedure KeyPress(var UTF8Key: Char); override;
          {$ENDIF}
          procedure SetDataSource(AValue: TDataSource); virtual;
          procedure SetDataField(const AValue: string); virtual;
          procedure MouseDown( Button : TMouseButton; Shift : TShiftState; X,Y : Integer); override;
          {$IFDEF SPECIALEDIT}
          procedure EditChange; override;
          {$ELSE}
          procedure Change; override;
          {$ENDIF}
          procedure ActiveChange(Sender: TObject); virtual;
          procedure DataChange(Sender: TObject); virtual;
          procedure UpdateData(Sender: TObject); virtual;
          procedure Notification(AComponent: TComponent; Operation: TOperation); override;
          function GetReadOnly: Boolean; virtual;
          procedure SetReadOnly(AValue: Boolean); virtual;
        public
          procedure Loaded; override;
          constructor Create(AOwner: TComponent); override;
          destructor Destroy; override;
          function ExecuteAction(AAction: TBasicAction): Boolean; override;
          function UpdateAction(AAction: TBasicAction): Boolean; override;
          property Field: TField read GetField;
        published
          property filename stored False ;
          property DataField: string read GetDataField write SetDataField stored True;
          property DataSource: TDataSource read GetDataSource write SetDataSource stored True;
          property OnPopup : TNotifyEvent read FOnPopup write FOnPopup;
          property PopupMenu;
          property ReadOnly: Boolean read GetReadOnly write SetReadOnly default false;
          property Align;
      End;



   { TFWDBLookupCombo }
   TFWDBLookupCombo = class ( TExtFormComboBox, IFWComponent, IFWComponentEdit )
      private
       FBeforeEnter, FBeforeExit : TNotifyEvent;
       FLabel: TLabel ;
       FOldColor ,
       FColorReadOnly,
       FColorFocus ,
       FColorEdit ,
       FColorLabel : TColor;
       FAlwaysSame : Boolean;
       FNotifyOrder : TNotifyEvent;
       FOnPopup : TNotifyEvent;
       procedure p_setLabel ( const alab_Label: TLabel );
       procedure WMPaint(var Message: {$IFDEF FPC}TLMPaint{$ELSE}TWMPaint{$ENDIF}); message {$IFDEF FPC}LM_PAINT{$ELSE}WM_PAINT{$ENDIF};
       {$IFDEF FPC}
       function GetField:TField ;
       {$ENDIF}
      protected
       procedure MouseDown( Button : TMouseButton; Shift : TShiftState; X,Y : Integer); override;
       procedure Notification(AComponent: TComponent; Operation: TOperation); override;
      public

       constructor Create ( AOwner : TComponent ); override;
       procedure DoEnter; override;
       procedure DoExit; override;
       procedure Loaded; override;
       procedure SetOrder ; virtual;
       {$IFDEF FPC}
       property Field : TField read GetField ;
       {$ENDIF}
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
       property OnPopup : TNotifyEvent read FOnPopup write FOnPopup;
       property OnMouseEnter;
       property OnMouseLeave;
       property PopupMenu;
       property Align;
     End;

   { TFWDBComboBox }
   TFWDBComboBox = class ( TDBComboBox, IFWComponent, IFWComponentEdit )
      private
       FBeforeEnter, FBeforeExit : TNotifyEvent;
       FLabel: TLabel ;
       FOldColor ,
       FColorReadOnly,
       FColorFocus ,
       FColorEdit ,
       FColorLabel : TColor;
       FAlwaysSame : Boolean;
       FNotifyOrder : TNotifyEvent;
       FOnPopup : TNotifyEvent;
       procedure p_setLabel ( const alab_Label: TLabel );
       procedure WMPaint(var Message: {$IFDEF FPC}TLMPaint{$ELSE}TWMPaint{$ENDIF}); message {$IFDEF FPC}LM_PAINT{$ELSE}WM_PAINT{$ENDIF};
      protected
       procedure MouseDown( Button : TMouseButton; Shift : TShiftState; X,Y : Integer); override;
       procedure Notification(AComponent: TComponent; Operation: TOperation); override;
      public

       constructor Create ( AOwner : TComponent ); override;
       procedure DoEnter; override;
       procedure DoExit; override;
       procedure Loaded; override;
       procedure SetOrder ; virtual;
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
       property OnPopup : TNotifyEvent read FOnPopup write FOnPopup;
       property OnMouseEnter;
       property OnMouseLeave;
       property PopupMenu;
       property Align;
     End;

   { TFWDBMemo }

   TFWDBMemo = class ( TExtFormComboBox, IFWComponent, IFWComponentEdit )
      private
       FBeforeEnter, FBeforeExit : TNotifyEvent;
       FLabel: TLabel ;
       FOldColor ,
       FColorFocus ,
       FColorReadOnly,
       FColorEdit ,
       FColorLabel : TColor;
       FAlwaysSame : Boolean;
       FNotifyOrder : TNotifyEvent;
       FOnPopup : TNotifyEvent;
       procedure p_setLabel ( const alab_Label: TLabel );
       procedure WMPaint(var Message: {$IFDEF FPC}TLMPaint{$ELSE}TWMPaint{$ENDIF}); message {$IFDEF FPC}LM_PAINT{$ELSE}WM_PAINT{$ENDIF};
      protected
       procedure MouseDown( Button : TMouseButton; Shift : TShiftState; X,Y : Integer); override;
       procedure Notification(AComponent: TComponent; Operation: TOperation); override;
      public

       constructor Create ( AOwner : TComponent ); override;
       procedure DoEnter; override;
       procedure DoExit; override;
       procedure Loaded; override;
       procedure SetOrder ; virtual;
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
       property OnPopup : TNotifyEvent read FOnPopup write FOnPopup;
       property OnMouseEnter;
       property OnMouseLeave;
       property PopupMenu;
       property Align;
     End;


implementation

uses fonctions_db,
     fonctions_components;

{ TFWDBEdit }

procedure TFWDBEdit.p_setLabel(const alab_Label: TLabel);
begin
  p_setMyLabel ( FLabel, alab_Label, Self );
end;

procedure TFWDBEdit.SetOrder;
begin
  if assigned ( FNotifyOrder ) then
    FNotifyOrder ( Self );
end;

constructor TFWDBEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAlwaysSame := True;
  FColorLabel := CST_LBL_SELECT;
  FColorEdit  := CST_EDIT_STD;
  FColorFocus := CST_EDIT_SELECT;
  FColorReadOnly := CST_EDIT_READ;
  FOnPopup:=nil;
end;

procedure TFWDBEdit.DoEnter;
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

procedure TFWDBEdit.DoExit;
begin
  if assigned ( FBeforeExit ) Then
    FBeforeExit ( Self );
  inherited DoExit;
  p_setLabelColorExit ( FLabel, FAlwaysSame );
  p_setCompColorExit ( Self, FOldColor, FAlwaysSame );

end;

procedure TFWDBEdit.Loaded;
begin
  inherited Loaded;
  FOldColor := Color;
  if  FAlwaysSame
   Then
    Color := gCol_Edit ;
end;

procedure TFWDBEdit.WMPaint(var Message: {$IFDEF FPC}TLMPaint{$ELSE}TWMPaint{$ENDIF});
Begin
  p_setCompColorReadOnly ( Self,FColorEdit,FColorReadOnly, FAlwaysSame, ReadOnly );
  inherited;
End;

procedure TFWDBEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbRight Then
   fb_ShowPopup (Self,PopUpMenu,OnContextPopup,FOnPopup);
end;

procedure TFWDBEdit.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if  ( Operation  = opRemove )
  and ( AComponent = FLabel   )
   Then FLabel := nil;
end;

{ TFWDBFileEdit }

constructor TFWDBFileEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink.Create ;
  FDataLink.DataSource := nil ;
  FDataLink.FieldName  := '' ;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
  FDataLink.OnActiveChange := ActiveChange;
  ControlStyle := ControlStyle + [csReplicatable];
end;

destructor TFWDBFileEdit.Destroy;
begin
  inherited Destroy;
  FDataLink.Free ;
end;

procedure TFWDBFileEdit.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then
    Begin
      DataChange(Self);
    End ;
end;

procedure TFWDBFileEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation <> opRemove) Then Exit;
  if (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

{$IFDEF FPC}
procedure TFWDBFileEdit.UTF8KeyPress(var UTF8Key: TUTF8Char);
{$ELSE}
procedure TFWDBFileEdit.KeyPress(var UTF8Key: Char);
{$ENDIF}

var
  CharKey: Char;
begin
  inherited;
  //If the pressed key is unicode then map the char to #255
  //Necessary to keep the TField.IsValidChar check
{$IFDEF FPC}
  if Length(UTF8Key) = 1 then
    CharKey := UTF8Key[1]
  else
    CharKey := #255;
{$ENDIF}
  case CharKey of
    #8: // special keys
      if not FDatalink.Edit then
        UTF8Key:={$IFDEF FPC}''{$ELSE}#0{$ENDIF};

    #32..#255: //standard keys
      if not FieldCanAcceptKey(FDataLink.Field, CharKey) or not FDatalink.Edit then
        UTF8Key:={$IFDEF FPC}''{$ELSE}#0{$ENDIF};
  end;//case
  UpdateData(Self);
end;


{$IFDEF SPECIALEDIT}
procedure TFWDBFileEdit.EditChange;
{$ELSE}
procedure TFWDBFileEdit.Change;
{$ENDIF}
begin
  inherited;
  UpdateData(Self);
end;

function TFWDBFileEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TFWDBFileEdit.SetDataSource(AValue: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := AValue;
  if AValue <> nil then AValue.FreeNotification(Self);
end;

function TFWDBFileEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TFWDBFileEdit.SetDataField(const AValue: string);
begin
  if  assigned ( FDataLink.DataSet )
  and FDataLink.DataSet.Active Then
    Begin
      if assigned ( FDataLink.DataSet.FindField ( AValue ))
      and ( FDataLink.DataSet.FindField ( AValue ) is TNumericField ) Then
        FDataLink.FieldName := AValue;
    End
  Else
    FDataLink.FieldName := AValue;
end;

function TFWDBFileEdit.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly or FReadOnly;
end;

procedure TFWDBFileEdit.SetReadOnly(AValue: Boolean);
begin
  FReadOnly := AValue;
end;

function TFWDBFileEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TFWDBFileEdit.Refresh;
begin
  if FDataLink.Field <> nil then
    filename := FDataLink.Field.AsString;
end;

procedure TFWDBFileEdit.ActiveChange(Sender: TObject);
begin
  DataChange(Sender);
end;

procedure TFWDBFileEdit.DataChange(Sender: TObject);
var DataLinkField : TField;
begin
  DataLinkField := FDataLink.Field;
  if DataLinkField <> nil then begin
    //use the right EditMask if any
    //EditMask := FDataLink.Field.EditMask; doesn't exist yet
    Alignment := DataLinkField.Alignment;

    //if we are focused its possible to edit,
    //if the field is currently modifiable
    {$IFDEF FPC}
    {$IFNDEF SPECIALEDIT}
    if Focused and FDataLink.CanModify then begin
      //display the real Directory since we can modify it
      RestoreMask(DatalinkField.Text);
    end else
      //otherwise display the pretified/formated text since we can't
      DisableMask(DataLinkField.DisplayText);
    {$ENDIF}
    {$ENDIF}
    if (DataLinkField.DataType in [ftString, ftFixedChar, ftWidestring, ftFixedWideChar])
      and (MaxLength = 0) then
      MaxLength := DatalinkField.Size;
    FileName := DataLinkField.Text;
  end
  else begin
    //todo: uncomment this when TField implements EditMask
    //EditMask := ''
    FileName := '';
    MaxLength := 0;
  end;
end;


procedure TFWDBFileEdit.UpdateData(Sender: TObject);
begin
  ValidateEdit;
  with FDataLink do
   if Field.Text <> FileName Then
    Begin
      Edit ;
      Field.Value := filename ;
    End ;
end;

procedure TFWDBFileEdit.WMPaste(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TFWDBFileEdit.WMCut(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TFWDBFileEdit.CMExit(var Message: {$IFDEF FPC} TLMExit {$ELSE} TCMExit {$ENDIF});
begin
  try
    FDataLink.UpdateRecord;
  except
    on e: Exception do
      Begin
        SetFocus;
        f_GereException ( e, FDataLink.DataSet, nil , False )
      End ;
  end;
  DoExit;
end;

procedure TFWDBFileEdit.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

procedure TFWDBFileEdit.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Button = mbRight Then
   fb_ShowPopup (Self,PopUpMenu,OnContextPopup,FOnPopup);
end;

function TFWDBFileEdit.ExecuteAction(AAction: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(AAction){$IFDEF DELPHI}  or (FDataLink <> nil) and
    FDataLink.ExecuteAction(AAction){$ENDIF};
end;

function TFWDBFileEdit.UpdateAction(AAction: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(AAction) {$IFDEF DELPHI}  or (FDataLink <> nil) and
    FDataLink.UpdateAction(AAction){$ENDIF};
end;


{ TFWDBDateEdit }

procedure TFWDBDateEdit.p_setLabel(const alab_Label: TLabel);
begin
  p_setMyLabel ( FLabel, alab_Label, Self );
end;

procedure TFWDBDateEdit.SetOrder;
begin
  if assigned ( FNotifyOrder ) then
    FNotifyOrder ( Self );
end;

constructor TFWDBDateEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAlwaysSame := True;
  FColorLabel := CST_LBL_SELECT;
  FColorEdit  := CST_EDIT_STD;
  FColorFocus := CST_EDIT_SELECT;
  FColorReadOnly := CST_EDIT_READ;
end;

procedure TFWDBDateEdit.DoEnter;
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

procedure TFWDBDateEdit.DoExit;
begin
  if assigned ( FBeforeExit ) Then
    FBeforeExit ( Self );
  inherited DoExit;
  p_setLabelColorExit ( FLabel, FAlwaysSame );
  p_setCompColorExit ( Self, FOldColor, FAlwaysSame );

end;

procedure TFWDBDateEdit.Loaded;
begin
  inherited Loaded;
  FOldColor := Color;
  if  FAlwaysSame
   Then
    Color := gCol_Edit ;
end;

procedure TFWDBDateEdit.WMPaint(var Message: {$IFDEF FPC}TLMPaint{$ELSE}TWMPaint{$ENDIF});
Begin
  p_setCompColorReadOnly ( Self,FColorEdit,FColorReadOnly, FAlwaysSame, ReadOnly );
  inherited;
End;

procedure TFWDBDateEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbRight Then
   fb_ShowPopup (Self,PopUpMenu,OnContextPopup,FOnPopup);
end;

procedure TFWDBDateEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if  ( Operation  = opRemove )
  and ( AComponent = FLabel   )
   Then FLabel := nil;
end;

{ TFWDBLookupCombo }

procedure TFWDBLookupCombo.p_setLabel(const alab_Label: TLabel);
begin
  p_setMyLabel ( FLabel, alab_Label, Self );
end;

procedure TFWDBLookupCombo.SetOrder;
begin
  if assigned ( FNotifyOrder ) then
    FNotifyOrder ( Self );
end;

constructor TFWDBLookupCombo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAlwaysSame := True;
  FColorLabel := CST_LBL_SELECT;
  FColorEdit  := CST_EDIT_STD;
  FColorFocus := CST_EDIT_SELECT;
  FColorReadOnly := CST_EDIT_READ;
end;

procedure TFWDBLookupCombo.DoEnter;
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

procedure TFWDBLookupCombo.DoExit;
begin
  if assigned ( FBeforeExit ) Then
    FBeforeExit ( Self );
  inherited DoExit;
  p_setLabelColorExit ( FLabel, FAlwaysSame );
  p_setCompColorExit ( Self, FOldColor, FAlwaysSame );

end;

procedure TFWDBLookupCombo.Loaded;
begin
  inherited Loaded;
  FOldColor := Color;
  if  FAlwaysSame
   Then
    Color := gCol_Edit ;
end;

procedure TFWDBLookupCombo.WMPaint(var Message: {$IFDEF FPC}TLMPaint{$ELSE}TWMPaint{$ENDIF});
Begin
  p_setCompColorReadOnly ( Self,FColorEdit,FColorReadOnly, FAlwaysSame, ReadOnly );
  inherited;
End;

{$IFDEF FPC}
function TFWDBLookupCombo.GetField: TField;
begin
  Result:=DataSource.DataSet.FieldByName(DataField);
end;
{$ENDIF}

procedure TFWDBLookupCombo.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbRight Then
   fb_ShowPopup (Self,PopUpMenu,OnContextPopup,FOnPopup);
end;

procedure TFWDBLookupCombo.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if  ( Operation  = opRemove )
  and ( AComponent = FLabel   )
   Then FLabel := nil;
end;

{ TFWDBComboBox }

procedure TFWDBComboBox.p_setLabel(const alab_Label: TLabel);
begin
  p_setMyLabel ( FLabel, alab_Label, Self );
end;

procedure TFWDBComboBox.SetOrder;
begin
  if assigned ( FNotifyOrder ) then
    FNotifyOrder ( Self );
end;

constructor TFWDBComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAlwaysSame := True;
  FColorLabel := CST_LBL_SELECT;
  FColorEdit  := CST_EDIT_STD;
  FColorFocus := CST_EDIT_SELECT;
  FColorReadOnly := CST_EDIT_READ;
end;

procedure TFWDBComboBox.DoEnter;
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

procedure TFWDBComboBox.DoExit;
begin
  if assigned ( FBeforeExit ) Then
    FBeforeExit ( Self );
  inherited DoExit;
  p_setLabelColorExit ( FLabel, FAlwaysSame );
  p_setCompColorExit ( Self, FOldColor, FAlwaysSame );

end;

procedure TFWDBComboBox.Loaded;
begin
  inherited Loaded;
  FOldColor := Color;
  if  FAlwaysSame
   Then
    Color := gCol_Edit ;
end;

procedure TFWDBComboBox.WMPaint(var Message: {$IFDEF FPC}TLMPaint{$ELSE}TWMPaint{$ENDIF});
Begin
  p_setCompColorReadOnly ( Self,FColorEdit,FColorReadOnly, FAlwaysSame, ReadOnly );
  inherited;
End;

procedure TFWDBComboBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbRight Then
   fb_ShowPopup (Self,PopUpMenu,OnContextPopup,FOnPopup);
end;

procedure TFWDBComboBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if  ( Operation  = opRemove )
  and ( AComponent = FLabel   )
   Then FLabel := nil;
end;


{ TFWDBMemo }

procedure TFWDBMemo.p_setLabel(const alab_Label: TLabel);
begin
  p_setMyLabel ( FLabel, alab_Label, Self );
end;

procedure TFWDBMemo.SetOrder;
begin
  if assigned ( FNotifyOrder ) then
    FNotifyOrder ( Self );
end;

constructor TFWDBMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAlwaysSame := True;
  FColorLabel := CST_LBL_SELECT;
  FColorEdit  := CST_EDIT_STD;
  FColorFocus := CST_EDIT_SELECT;
  FColorReadOnly := CST_EDIT_READ;
end;

procedure TFWDBMemo.DoEnter;
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

procedure TFWDBMemo.DoExit;
begin
  if assigned ( FBeforeExit ) Then
    FBeforeExit ( Self );
  inherited DoExit;
  p_setLabelColorExit ( FLabel, FAlwaysSame );
  p_setCompColorExit ( Self, FOldColor, FAlwaysSame );

end;

procedure TFWDBMemo.Loaded;
begin
  inherited Loaded;
  FOldColor := Color;
  if  FAlwaysSame
   Then
    Color := gCol_Edit ;
end;

procedure TFWDBMemo.WMPaint(var Message: {$IFDEF FPC}TLMPaint{$ELSE}TWMPaint{$ENDIF});
Begin
  p_setCompColorReadOnly ( Self,FColorEdit,FColorReadOnly, FAlwaysSame, ReadOnly );
  inherited;
End;

procedure TFWDBMemo.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbRight Then
   fb_ShowPopup (Self,PopUpMenu,OnContextPopup,FOnPopup);
end;

procedure TFWDBMemo.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if  ( Operation  = opRemove )
  and ( AComponent = FLabel   )
   Then FLabel := nil;
end;

{ TFWDBSpinEdit }

procedure TFWDBSpinEdit.p_setLabel(const alab_Label: TLabel);
begin
  p_setMyLabel ( FLabel, alab_Label, Self );
end;

procedure TFWDBSpinEdit.SetOrder;
begin
  if assigned ( FNotifyOrder ) then
    FNotifyOrder ( Self );
end;

constructor TFWDBSpinEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAlwaysSame := True;
  FColorLabel := CST_LBL_SELECT;
  FColorEdit  := CST_EDIT_STD;
  FColorFocus := CST_EDIT_SELECT;
  FColorReadOnly := CST_EDIT_READ;
end;

procedure TFWDBSpinEdit.DoEnter;
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

procedure TFWDBSpinEdit.DoExit;
begin
  if assigned ( FBeforeExit ) Then
    FBeforeExit ( Self );
  inherited DoExit;
  p_setLabelColorExit ( FLabel, FAlwaysSame );
  p_setCompColorExit ( Self, FOldColor, FAlwaysSame );

end;

procedure TFWDBSpinEdit.Loaded;
begin
  inherited Loaded;
  FOldColor := Color;
  if  FAlwaysSame
   Then
    Color := gCol_Edit ;
end;

procedure TFWDBSpinEdit.WMPaint(var Message: {$IFDEF FPC}TLMPaint{$ELSE}TWMPaint{$ENDIF});
Begin
  p_setCompColorReadOnly ( Self,FColorEdit,FColorReadOnly, FAlwaysSame, ReadOnly );
  inherited;
End;

procedure TFWDBSpinEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbRight Then
   fb_ShowPopup (Self,PopUpMenu,OnContextPopup,FOnPopup);
end;

procedure TFWDBSpinEdit.Notification(AComponent: TComponent; Operation: TOperation
  );
begin
  inherited Notification(AComponent, Operation);
  if  ( Operation  = opRemove )
  and ( AComponent = FLabel   )
   Then FLabel := nil;
end;




{$IFDEF VERSIONS}
initialization
  // Gestion de version
  p_ConcatVersion(gVer_framework_DBcomponents);
{$ENDIF}
end.

