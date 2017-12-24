unit u_extp_components;

{$I ..\DLCompilers.inc}
{$I ..\extends.inc}
{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}

interface

uses
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
    gVer_framework_components : T_Version = ( Component : 'Composants d''interactivité' ;
                                               FileUnit : 'u_framework_components' ;
                                               Owner : 'Matthieu Giroux' ;
                                               Comment : 'Composants d''interactivité de U_CustomFrameWork.' ;
                                               BugsStory : '1.0.0.1 : MyLabel unset correctly.'
                                                         + '1.0.0.0 : FileEdit.'
                                                         + '0.9.0.2 : UTF 8.'
                                                         + '0.9.0.1 : Working on Lazarus.'
                                                         + '0.9.0.0 : Creating u_framework_dbcomponents.'
                                                         + '0.8.0.0 : Gestion à tester.';
                                               UnitType : 3 ;
                                               Major : 1 ; Minor : 0 ; Release : 0 ; Build : 1 );

{$ENDIF}
type

   TFWLabel = class ;
{ TFWEdit }

   TFWEdit = class ( {$IFDEF TNT}TTntMaskEdit{$ELSE}TExtEdit{$ENDIF}, IFWComponent, IFWComponentEdit )
      private
       FBeforeEnter, FBeforeExit : TNotifyEvent;
       FLabel : TCustomLabel ;
       FOldColor ,
       FColorFocus ,
       FColorReadOnly,
       FColorEdit ,
       FColorLabel : TColor;
       FAlwaysSame : Boolean;
       FNotifyOrder : TNotifyEvent;
       FOnPopup: TNotifyEvent;
       procedure p_setLabel ( const alab_Label: TCustomLabel );
       procedure WMPaint(var Message: {$IFDEF FPC}TLMPaint{$ELSE}TWMPaint{$ENDIF}); message {$IFDEF FPC}LM_PAINT{$ELSE}WM_PAINT{$ENDIF};
      protected
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
       property MyLabel : TCustomLabel read FLabel write p_setLabel;
       property AlwaysSame : Boolean read FAlwaysSame write FAlwaysSame default true;
       property OnOrder : TNotifyEvent read FNotifyOrder write FNotifyOrder;
       property OnPopup : TNotifyEvent read FOnPopup write FOnPopup;
       property OnMouseEnter;
       property OnMouseLeave;
       property PopupMenu;
       property Align;
     End;

   { TFWEdit }

    TFWFileEdit = class ( TFWEdit, IFWComponent, IFWComponentEdit )
       private
        FBeforeEnter, FBeforeExit : TNotifyEvent;
        FLabel: TCustomLabel ;
        FOldColor ,
        FColorFocus ,
        FColorReadOnly,
        FColorEdit ,
        FColorLabel : TColor;
        FAlwaysSame : Boolean;
        FNotifyOrder : TNotifyEvent;
        FOnPopup: TNotifyEvent;
        procedure p_setLabel ( const alab_Label: TCustomLabel );
        procedure WMPaint(var Message: {$IFDEF FPC}TLMPaint{$ELSE}TWMPaint{$ENDIF}); message {$IFDEF FPC}LM_PAINT{$ELSE}WM_PAINT{$ENDIF};
       protected
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
        property MyLabel : TCustomLabel read FLabel write p_setLabel;
        property AlwaysSame : Boolean read FAlwaysSame write FAlwaysSame default true;
        property OnOrder : TNotifyEvent read FNotifyOrder write FNotifyOrder;
        property OnPopup : TNotifyEvent read FOnPopup write FOnPopup;
        property OnMouseEnter;
        property OnMouseLeave;
        property PopupMenu;
        property Align;
      End;


    { TFWSpinEdit }

    TFWSpinEdit = class ( TExtFormNumberField, IFWComponent, IFWComponentEdit )
       private
        FBeforeEnter, FBeforeExit : TNotifyEvent;
        FLabel: TCustomLabel ;
        FOldColor ,
        FColorFocus ,
        FColorReadOnly,
        FColorEdit ,
        FColorLabel : TColor;
        FAlwaysSame : Boolean;
        FNotifyOrder : TNotifyEvent;
        FOnPopup : TNotifyEvent;
        procedure p_setLabel ( const alab_Label: TCustomLabel );
        procedure WMPaint(var Message: {$IFDEF FPC}TLMPaint{$ELSE}TWMPaint{$ENDIF}); message {$IFDEF FPC}LM_PAINT{$ELSE}WM_PAINT{$ENDIF};
      protected
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
        property MyLabel : TCustomLabel read FLabel write p_setLabel;
        property AlwaysSame : Boolean read FAlwaysSame write FAlwaysSame default true;
        property OnOrder : TNotifyEvent read FNotifyOrder write FNotifyOrder;
        property OnPopup : TNotifyEvent read FOnPopup write FOnPopup;
        property OnMouseEnter;
        property OnMouseLeave;
        property PopupMenu;
        property Align;
      End;

     { TFWDateEdit }

     TFWDateEdit = class ( TExtFormDateField, IFWComponent, IFWComponentEdit )
        private
         FBeforeEnter, FBeforeExit : TNotifyEvent;
         FLabel: TCustomLabel ;
         FOldColor ,
         FColorFocus ,
         FColorReadOnly,
         FColorEdit ,
         FColorLabel : TColor;
         FAlwaysSame : Boolean;
         FNotifyOrder : TNotifyEvent;
         FOnPopup : TNotifyEvent;
         procedure p_setLabel ( const alab_Label: TCustomLabel );
         procedure WMPaint(var Message: {$IFDEF FPC}TLMPaint{$ELSE}TWMPaint{$ENDIF}); message {$IFDEF FPC}LM_PAINT{$ELSE}WM_PAINT{$ENDIF};
        protected
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
         property MyLabel : TCustomLabel read FLabel write p_setLabel;
         property AlwaysSame : Boolean read FAlwaysSame write FAlwaysSame default true;
         property OnOrder : TNotifyEvent read FNotifyOrder write FNotifyOrder;
         property OnPopup : TNotifyEvent read FOnPopup write FOnPopup;
         property OnMouseEnter;
         property OnMouseLeave;
         property PopupMenu;
         property Align;
       End;

   { TFWComboBox }

   TFWComboBox = class ( TExtFormComboBox, IFWComponent, IFWComponentEdit )
      private
       FBeforeEnter, FBeforeExit : TNotifyEvent;
       FLabel: TCustomLabel ;
       FOldColor ,
       FColorFocus ,
       FColorReadOnly,
       FColorEdit ,
       FColorLabel : TColor;
       FAlwaysSame : Boolean;
       FNotifyOrder : TNotifyEvent;
       FOnPopup : TNotifyEvent;
       procedure p_setLabel ( const alab_Label: TCustomLabel );
       procedure WMPaint(var Message: {$IFDEF FPC}TLMPaint{$ELSE}TWMPaint{$ENDIF}); message {$IFDEF FPC}LM_PAINT{$ELSE}WM_PAINT{$ENDIF};
      protected
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
       property MyLabel : TCustomLabel read FLabel write p_setLabel;
       property AlwaysSame : Boolean read FAlwaysSame write FAlwaysSame default true;
       property OnOrder : TNotifyEvent read FNotifyOrder write FNotifyOrder;
       property OnPopup : TNotifyEvent read FOnPopup write FOnPopup;
       property OnMouseEnter;
       property OnMouseLeave;
       property PopupMenu;
       property Align;
     End;

   { TFWLabel }

   TFWLabel = class ( TExtLabel, IFWComponent )
      private
       FColorFocus ,
       FOldColor   : TColor;
       FAlwaysSame : Boolean;
       FEditComponent : TControl;
       procedure CMMouseEnter(var Message: TMessage); message {$IFDEF FPC}LM_MOUSEENTER{$ELSE}CM_MOUSEENTER{$ENDIF};
       procedure CMMouseLeave(var Message: TMessage); message {$IFDEF FPC}LM_MOUSELEAVE{$ELSE}CM_MOUSELEAVE{$ENDIF};
      protected
       procedure Notification(AComponent: TComponent; Operation: TOperation); override;
      public

       procedure Loaded; override;
       constructor Create ( AOwner : TComponent ); override;
      published
       property ColorFocus : TColor read FColorFocus write FColorFocus default CST_LBL_ACTIVE ;
       property OldColor   : TColor read FOldColor stored False default CST_LBL_STD ;
       property AlwaysSame : Boolean read FAlwaysSame write FAlwaysSame default true;
       property MyEdit : TControl read FEditComponent write FEditComponent stored false;
       property Alignment default taRightJustify;
       {$IFDEF FPC}
       property OptimalFill default True ;
       {$ENDIF}
       property Align;
       property Font;
     End;
   { TFWDBGrid }

   { TFWGrid }

   TFWGrid = class ( TExtGridEditorGridPanel, IFWComponent )
      private
       FBeforeEnter, FBeforeExit : TNotifyEvent;
       FColorEdit     ,
       FColorFocus    ,
       FOldFixedColor : TColor;
       FAlwaysSame : Boolean;
       FFieldsTags : Array of Integer ;
       function  fi_getFieldTags ( li_i : LongInt ):Integer;
       procedure p_setFieldTags ( li_i : LongInt ; const a_value : Integer );
      public
       constructor Create ( AOwner : TComponent ); override;
       procedure DoEnter; override;
       procedure DoExit; override;
       procedure Loaded; override;
       property FieldsTags [ Index : Longint ] : Integer read fi_getFieldTags write p_setFieldTags;
      published
       property FWBeforeEnter : TnotifyEvent read FBeforeEnter write FBeforeEnter stored False;
       property FWBeforeExit  : TnotifyEvent read FBeforeExit  write FBeforeExit stored False ;
       property ColorEdit : TColor read FColorEdit write FColorEdit default CST_GRID_STD ;
       property Color stored False ;
       property FixedColor default CST_GRID_STD ;
       property ColorFocus : TColor read FColorFocus write FColorFocus default CST_GRID_SELECT ;
       property AlwaysSame : Boolean read FAlwaysSame write FAlwaysSame default true;
     End;

   { TFWMemo }

   TFWMemo = class ( TExtFormTextField, IFWComponent, IFWComponentEdit )
      private
       FBeforeEnter, FBeforeExit : TNotifyEvent;
       FLabel: TCustomLabel ;
       FOldColor ,
       FColorFocus ,
       FColorReadOnly,
       FColorEdit ,
       FColorLabel : TColor;
       FAlwaysSame : Boolean;
       FNotifyOrder : TNotifyEvent;
       FOnPopup : TNotifyEvent;
       procedure p_setLabel ( const alab_Label: TCustomLabel );
       procedure WMPaint(var Message: {$IFDEF FPC}TLMPaint{$ELSE}TWMPaint{$ENDIF}); message {$IFDEF FPC}LM_PAINT{$ELSE}WM_PAINT{$ENDIF};
      protected
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
       property MyLabel : TCustomLabel read FLabel write p_setLabel;
       property AlwaysSame : Boolean read FAlwaysSame write FAlwaysSame default true;
       property OnOrder : TNotifyEvent read FNotifyOrder write FNotifyOrder;
       property OnPopup : TNotifyEvent read FOnPopup write FOnPopup;
       property OnMouseEnter;
       property OnMouseLeave;
       property PopupMenu;
       property Align;
     End;




implementation

{ TFWEdit }

procedure TFWEdit.p_setLabel(const alab_Label: TCustomLabel);
begin
  p_setMyLabel ( FLabel, alab_Label, Self );
end;

procedure TFWEdit.SetOrder;
begin
  if assigned ( FNotifyOrder ) then
    FNotifyOrder ( Self );
end;

constructor TFWEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAlwaysSame := True;
  FColorLabel := CST_LBL_SELECT;
  FColorEdit  := CST_EDIT_STD;
  FColorFocus := CST_EDIT_SELECT;
  FColorReadOnly := CST_EDIT_READ;
end;

procedure TFWEdit.DoEnter;
begin
  if assigned ( FBeforeEnter ) Then
    FBeforeEnter ( Self );
  // Si on arrive sur une zone de saisie, on met en valeur son TCustomLabel par une couleur
  // de fond bleu et son libellé en marron (sauf si le libellé est sélectionné
  // avec la souris => cas de tri)
  p_setLabelColorEnter ( FLabel, FColorLabel, FAlwaysSame );
  p_setCompColorEnter  ( Self, FColorFocus, FAlwaysSame );
  inherited DoEnter;
end;

procedure TFWEdit.DoExit;
begin
  if assigned ( FBeforeExit ) Then
    FBeforeExit ( Self );
  inherited DoExit;
  p_setLabelColorExit ( FLabel, FAlwaysSame );
  p_setCompColorExit ( Self, FOldColor, FAlwaysSame );

end;

procedure TFWEdit.Loaded;
begin
  inherited Loaded;
  FOldColor := Color;
  if  FAlwaysSame
   Then
    Color := gCol_Edit ;
end;

procedure TFWEdit.WMPaint(var Message: {$IFDEF FPC}TLMPaint{$ELSE}TWMPaint{$ENDIF});
Begin
  p_setCompColorReadOnly ( Self,FColorEdit,FColorReadOnly, FAlwaysSame, ReadOnly );
  inherited;
End;

procedure TFWEdit.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if  ( Operation  = opRemove )
  and ( AComponent = FLabel   )
   Then FLabel := nil;
end;


{ TFWFileEdit }

procedure TFWFileEdit.p_setLabel(const alab_Label: TCustomLabel);
begin
  p_setMyLabel ( FLabel, alab_Label, Self );
end;

procedure TFWFileEdit.SetOrder;
begin
  if assigned ( FNotifyOrder ) then
    FNotifyOrder ( Self );
end;

constructor TFWFileEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAlwaysSame := True;
  FColorLabel := CST_LBL_SELECT;
  FColorEdit  := CST_EDIT_STD;
  FColorFocus := CST_EDIT_SELECT;
  FColorReadOnly := CST_EDIT_READ;
end;

procedure TFWFileEdit.DoEnter;
begin
  if assigned ( FBeforeEnter ) Then
    FBeforeEnter ( Self );
  // Si on arrive sur une zone de saisie, on met en valeur son TCustomLabel par une couleur
  // de fond bleu et son libellé en marron (sauf si le libellé est sélectionné
  // avec la souris => cas de tri)
  p_setLabelColorEnter ( FLabel, FColorLabel, FAlwaysSame );
  p_setCompColorEnter  ( Self, FColorFocus, FAlwaysSame );
  inherited DoEnter;
end;

procedure TFWFileEdit.DoExit;
begin
  if assigned ( FBeforeExit ) Then
    FBeforeExit ( Self );
  inherited DoExit;
  p_setLabelColorExit ( FLabel, FAlwaysSame );
  p_setCompColorExit ( Self, FOldColor, FAlwaysSame );

end;

procedure TFWFileEdit.Loaded;
begin
  inherited Loaded;
  FOldColor := Color;
  if  FAlwaysSame
   Then
    Color := gCol_Edit ;
end;

procedure TFWFileEdit.WMPaint(var Message: {$IFDEF FPC}TLMPaint{$ELSE}TWMPaint{$ENDIF});
Begin
  p_setCompColorReadOnly ( Self,FColorEdit,FColorReadOnly, FAlwaysSame, ReadOnly );
  inherited;
End;

procedure TFWFileEdit.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if  ( Operation  = opRemove )
  and ( AComponent = FLabel   )
   Then FLabel := nil;
end;

{ TFWSpinEdit }

procedure TFWSpinEdit.p_setLabel(const alab_Label: TCustomLabel);
begin
  p_setMyLabel ( FLabel, alab_Label, Self );
end;

procedure TFWSpinEdit.SetOrder;
begin
  if assigned ( FNotifyOrder ) then
    FNotifyOrder ( Self );
end;

constructor TFWSpinEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAlwaysSame := True;
  FColorLabel := CST_LBL_SELECT;
  FColorEdit  := CST_EDIT_STD;
  FColorFocus := CST_EDIT_SELECT;
  FColorReadOnly := CST_EDIT_READ;
end;

procedure TFWSpinEdit.DoEnter;
begin
  if assigned ( FBeforeEnter ) Then
    FBeforeEnter ( Self );
  // Si on arrive sur une zone de saisie, on met en valeur son TCustomLabel par une couleur
  // de fond bleu et son libellé en marron (sauf si le libellé est sélectionné
  // avec la souris => cas de tri)
  p_setLabelColorEnter ( FLabel, FColorLabel, FAlwaysSame );
  p_setCompColorEnter  ( Self, FColorFocus, FAlwaysSame );
  inherited DoEnter;
end;

procedure TFWSpinEdit.DoExit;
begin
  if assigned ( FBeforeExit ) Then
    FBeforeExit ( Self );
  inherited DoExit;
  p_setLabelColorExit ( FLabel, FAlwaysSame );
  p_setCompColorExit ( Self, FOldColor, FAlwaysSame );

end;

procedure TFWSpinEdit.Loaded;
begin
  inherited Loaded;
  FOldColor := Color;
  if  FAlwaysSame
   Then
    Color := gCol_Edit ;
end;

procedure TFWSpinEdit.WMPaint(var Message: {$IFDEF FPC}TLMPaint{$ELSE}TWMPaint{$ENDIF});
Begin
  p_setCompColorReadOnly ( Self,FColorEdit,FColorReadOnly, FAlwaysSame, ReadOnly );
  inherited;
End;

procedure TFWSpinEdit.Notification(AComponent: TComponent; Operation: TOperation
  );
begin
  inherited Notification(AComponent, Operation);
  if  ( Operation  = opRemove )
  and ( AComponent = FLabel   )
   Then FLabel := nil;
end;

{ TFWDateEdit }

procedure TFWDateEdit.p_setLabel(const alab_Label: TCustomLabel);
begin
  p_setMyLabel ( FLabel, alab_Label, Self );
end;

procedure TFWDateEdit.SetOrder;
begin
  if assigned ( FNotifyOrder ) then
    FNotifyOrder ( Self );
end;

constructor TFWDateEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAlwaysSame := True;
  FColorLabel := CST_LBL_SELECT;
  FColorEdit  := CST_EDIT_STD;
  FColorFocus := CST_EDIT_SELECT;
  FColorReadOnly := CST_EDIT_READ;
end;

procedure TFWDateEdit.DoEnter;
begin
  if assigned ( FBeforeEnter ) Then
    FBeforeEnter ( Self );
  // Si on arrive sur une zone de saisie, on met en valeur son TCustomLabel par une couleur
  // de fond bleu et son libellé en marron (sauf si le libellé est sélectionné
  // avec la souris => cas de tri)
  p_setLabelColorEnter ( FLabel, FColorLabel, FAlwaysSame );
  p_setCompColorEnter  ( Self, FColorFocus, FAlwaysSame );
  inherited DoEnter;
end;

procedure TFWDateEdit.DoExit;
begin
  if assigned ( FBeforeExit ) Then
    FBeforeExit ( Self );
  inherited DoExit;
  p_setLabelColorExit ( FLabel, FAlwaysSame );
  p_setCompColorExit ( Self, FOldColor, FAlwaysSame );

end;

procedure TFWDateEdit.Loaded;
begin
  inherited Loaded;
  FOldColor := Color;
  if  FAlwaysSame
   Then
    Color := gCol_Edit ;
end;

procedure TFWDateEdit.WMPaint(var Message: {$IFDEF FPC}TLMPaint{$ELSE}TWMPaint{$ENDIF});
Begin
  p_setCompColorReadOnly ( Self,FColorEdit,FColorReadOnly, FAlwaysSame, {$IFDEF FPC}ReadOnly{$ELSE}False{$ENDIF});
  inherited;
End;

procedure TFWDateEdit.Notification(AComponent: TComponent; Operation: TOperation
  );
begin
  inherited Notification(AComponent, Operation);
  if  ( Operation  = opRemove )
  and ( AComponent = FLabel   )
   Then FLabel := nil;
end;


{ TFWComboBox }

procedure TFWComboBox.p_setLabel(const alab_Label: TCustomLabel);
begin
  p_setMyLabel ( FLabel, alab_Label, Self );
end;

procedure TFWComboBox.SetOrder;
begin
  if assigned ( FNotifyOrder ) then
    FNotifyOrder ( Self );
end;

constructor TFWComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAlwaysSame := True;
  FColorLabel := CST_LBL_SELECT;
  FColorEdit  := CST_EDIT_STD;
  FColorFocus := CST_EDIT_SELECT;
  FColorReadOnly := CST_EDIT_READ;
end;

procedure TFWComboBox.DoEnter;
begin
  if assigned ( FBeforeEnter ) Then
    FBeforeEnter ( Self );
  // Si on arrive sur une zone de saisie, on met en valeur son TCustomLabel par une couleur
  // de fond bleu et son libellé en marron (sauf si le libellé est sélectionné
  // avec la souris => cas de tri)
  p_setLabelColorEnter ( FLabel, FColorLabel, FAlwaysSame );
  p_setCompColorEnter  ( Self, FColorFocus, FAlwaysSame );
  inherited DoEnter;
end;

procedure TFWComboBox.DoExit;
begin
  if assigned ( FBeforeExit ) Then
    FBeforeExit ( Self );
  inherited DoExit;
  p_setLabelColorExit ( FLabel, FAlwaysSame );
  p_setCompColorExit ( Self, FOldColor, FAlwaysSame );

end;

procedure TFWComboBox.Loaded;
begin
  inherited Loaded;
  FOldColor := Color;
  if  FAlwaysSame
   Then
    Color := gCol_Edit ;
end;

procedure TFWComboBox.WMPaint(var Message: {$IFDEF FPC}TLMPaint{$ELSE}TWMPaint{$ENDIF});
Begin
  p_setCompColorReadOnly ( Self,FColorEdit,FColorReadOnly, FAlwaysSame, {$IFDEF FPC}ReadOnly{$ELSE}False{$ENDIF});
  inherited;
End;

procedure TFWComboBox.Notification(AComponent: TComponent; Operation: TOperation
  );
begin
  inherited Notification(AComponent, Operation);
  if  ( Operation  = opRemove )
  and ( AComponent = FLabel   )
   Then FLabel := nil;
end;



{ TFWLabel }

procedure TFWLabel.Loaded;
begin
  inherited Loaded;
  FOldColor:=Font.Color;
  if  FAlwaysSame
   Then
    Font.Color := gCol_Label ;
end;

constructor TFWLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAlwaysSame := True;
  FColorFocus := CST_LBL_SELECT;
  Alignment := taRightJustify;
  {$IFDEF FPC}
  OptimalFill := True ;
  {$ENDIF}
end;

procedure TFWLabel.CMMouseEnter(var Message: TMessage);
begin
  if FAlwaysSame
   Then
    Font.Color := gCol_LabelActive
   Else
    Font.Color := FColorFocus;
  Cursor := crHandPoint;
  inherited;
end;

procedure TFWLabel.CMMouseLeave(var Message: TMessage);
begin
  Cursor := crDefault;
  if FAlwaysSame
   Then
    Font.Color := gCol_Label
   Else
    Font.Color := FOldColor;
  inherited;
end;

procedure TFWLabel.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if  ( Operation  = opRemove )
  and ( AComponent = FEditComponent   )
   Then FEditComponent := nil;
end;

{ TFWDBGrid }

constructor TFWGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAlwaysSame := True;
  FColorFocus := CST_GRID_SELECT;
  FColorEdit  := CST_GRID_STD;
end;

procedure TFWGrid.Loaded;
begin
  inherited Loaded;
  FOldFixedColor := FixedColor;
  if  FAlwaysSame
   Then
    FixedColor := gCol_Grid ;
End;

procedure TFWGrid.p_setFieldTags( li_i:Longint; const a_value: Integer);
begin
  if li_i > high ( FFieldsTags ) then
   Setlength ( FFieldsTags, li_i + 1 );
  FFieldsTags [ li_i ] := a_value;
end;

function TFWGrid.fi_getFieldTags( li_i: Longint): Integer;
begin
  Result := -1 ;
  if li_i < high ( FFieldsTags ) then
    Result := FFieldsTags [ li_i ];

end;


procedure TFWGrid.DoEnter;
begin
  if assigned ( FBeforeEnter ) Then
    FBeforeEnter ( Self );
  // Rétablit le focus si on l'a perdu
  if FAlwaysSame
   Then
    FixedColor := gCol_GridSelect
   else
    FixedColor := FColorFocus ;
  inherited DoEnter;
end;

procedure TFWGrid.DoExit;
begin
  if assigned ( FBeforeExit ) Then
    FBeforeExit ( Self );
  inherited DoExit;
  // Rétablit la couleur du focus
  if FAlwaysSame
   Then
    FixedColor := gCol_Grid
   else
    FixedColor := FOldFixedColor ;
end;


{ TFWMemo }

procedure TFWMemo.p_setLabel(const alab_Label: TCustomLabel);
begin
  p_setMyLabel ( FLabel, alab_Label, Self );
end;

procedure TFWMemo.SetOrder;
begin
  if assigned ( FNotifyOrder ) then
    FNotifyOrder ( Self );
end;

constructor TFWMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAlwaysSame := True;
  FColorLabel := CST_LBL_SELECT;
  FColorEdit  := CST_EDIT_STD;
  FColorFocus := CST_EDIT_SELECT;
  FColorReadOnly := CST_EDIT_READ;
end;

procedure TFWMemo.DoEnter;
begin
  if assigned ( FBeforeEnter ) Then
    FBeforeEnter ( Self );
  // Si on arrive sur une zone de saisie, on met en valeur son TCustomLabel par une couleur
  // de fond bleu et son libellé en marron (sauf si le libellé est sélectionné
  // avec la souris => cas de tri)
  p_setLabelColorEnter ( FLabel, FColorLabel, FAlwaysSame );
  p_setCompColorEnter  ( Self, FColorFocus, FAlwaysSame );
  inherited DoEnter;
end;

procedure TFWMemo.DoExit;
begin
  if assigned ( FBeforeExit ) Then
    FBeforeExit ( Self );
  inherited DoExit;
  p_setLabelColorExit ( FLabel, FAlwaysSame );
  p_setCompColorExit ( Self, FOldColor, FAlwaysSame );

end;

procedure TFWMemo.Loaded;
begin
  inherited Loaded;
  FOldColor := Color;
  if  FAlwaysSame
   Then
    Color := gCol_Edit ;
end;

procedure TFWMemo.WMPaint(var Message: {$IFDEF FPC}TLMPaint{$ELSE}TWMPaint{$ENDIF});
Begin
  p_setCompColorReadOnly ( Self,FColorEdit,FColorReadOnly, FAlwaysSame, ReadOnly );
  inherited;
End;

procedure TFWMemo.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if  ( Operation  = opRemove )
  and ( AComponent = FLabel   )
   Then FLabel := nil;
end;


{$IFDEF VERSIONS}
initialization
  // Gestion de version
  p_ConcatVersion(gVer_framework_components);
{$ENDIF}
end.

