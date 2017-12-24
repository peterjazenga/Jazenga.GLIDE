{*********************************************************************}
{                                                                     }
{                                                                     }
{             Matthieu Giroux                                         }
{             TExtNumEdit  :                                       }
{             Composant edit de nombre              }
{             TExtDBNumEdit :                                       }
{             Composant dbedit de nombre }
{             22 Avril 2006                                           }
{                                                                     }
{                                                                     }
{*********************************************************************}

unit U_ExtNumEdits;

{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}

interface

{$I ..\DLCompilers.inc}
{$I ..\extends.inc}

uses
  Messages,  SysUtils, Classes, Graphics, Controls,
{$IFDEF FPC}
  LCLType, MaskEdit, lmessages, lresources, sqldb,
{$ELSE}
  Windows, Mask, DBTables,
{$ENDIF}
{$IFDEF ADO}
   ADODB,
{$ENDIF}
{$IFDEF VERSIONS}
  fonctions_version,
{$ENDIF}
{$IFDEF TNT}
   TntStdCtrls,
{$ENDIF}
  Forms, Dialogs,
  Db, StdCtrls,
  rxcurredit,
  rxdbcurredit,
  DBCtrls, fonctions_numedit,
  u_extcomponent ;

  const
{$IFDEF VERSIONS}
    gVer_TExtNumEdit : T_Version = ( Component : 'Composant TExtNumEdit' ;
                                     FileUnit : 'U_NumEdits' ;
                                     Owner : 'Matthieu Giroux' ;
                                     Comment : 'Edition de nombres.' ;
                                     BugsStory : '1.2.0.0 : TCurrencyEdit inherit.' + #13#10
                                               + '1.1.0.0 : Testing largely.' + #13#10
                                               + '1.0.1.3 : Testing on LAZARUS' + #13#10
                                               + '1.0.1.2 : Mask on num edit' + #13#10
                                               + '1.0.1.1 : NumRounded property not tested' + #13#10
                                               + '1.0.1.0 : Better ExtNumEdit with good colors' + #13#10
                                               + '1.0.0.1 : Bug rafraîchissement de AValue' + #13#10
                                               + '1.0.0.0 : Gestion en place.';
                                     UnitType : 3 ;
                                     Major : 1 ; Minor : 2 ; Release : 0 ; Build : 0 );
    gVer_TExtDBNumEdit : T_Version = ( Component : 'Composant TExtDBNumEdit' ;
                                       FileUnit : 'U_NumEdits' ;
                                       Owner : 'Matthieu Giroux' ;
                                       Comment : 'Edition de nombres en données.' ;
                                       BugsStory : '1.2.0.0 : TRXDBCurrEdit inherit.' + #13#10
                                                 + '1.1.0.0 : Uprading parent Not Tested.' + #13#10
                                                 + '1.0.1.4 : Testing on LAZARUS.' + #13#10
                                                 + '1.0.1.3 : Better num edit.' + #13#10
                                                 + '1.0.1.2 : NumRounded property not tested.' + #13#10
                                                 + '1.0.1.1 : Less methods with good colors.' + #13#10
                                                 + '1.0.1.0 : Améliorations sur la gestion des erreurs.' + #13#10
                                                 + '1.0.0.0 : Gestion en place.';
                                       UnitType : 3 ;
                                       Major : 1 ; Minor : 2 ; Release : 0 ; Build : 0 );
{$ENDIF}
    CST_NUM_NEGATIVE = True ;
    CST_NUM_POSITIVE = True ;
    CST_NUM_BEFORECOMMA = 42 ;
    CST_NUM_AFTERCOMMA  = 2 ;




  { TExtNumEdit }

type
  TExtNumEdit   = class(TCurrencyEdit, IFWComponent, IFWComponentEdit)
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
    FOnPopup: TNotifyEvent;
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

  { TExtDBNumEdit }

  TExtDBNumEdit = class(TRxDBCurrEdit, IFWComponent, IFWComponentEdit )
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

uses
{$IFDEF FPC}
    LCLIntf, tmschema,
{$ENDIF}
    fonctions_erreurs, fonctions_string,
    fonctions_components,
    fonctions_proprietes, Math ;

{ TExtDBNumEdit }

procedure TExtDBNumEdit.p_setLabel(const alab_Label: TLabel);
begin
  p_setMyLabel ( FLabel, alab_Label, Self );
end;

procedure TExtDBNumEdit.SetOrder;
begin
  if assigned ( FNotifyOrder ) then
    FNotifyOrder ( Self );
end;

constructor TExtDBNumEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAlwaysSame := True;
  FColorLabel := CST_LBL_SELECT;
  FColorEdit  := CST_EDIT_STD;
  FColorFocus := CST_EDIT_SELECT;
  FColorReadOnly := CST_EDIT_READ;
end;

procedure TExtDBNumEdit.DoEnter;
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

procedure TExtDBNumEdit.DoExit;
begin
  if assigned ( FBeforeExit ) Then
    FBeforeExit ( Self );
  inherited DoExit;
  p_setLabelColorExit ( FLabel, FAlwaysSame );
  p_setCompColorExit ( Self, FOldColor, FAlwaysSame );

end;

procedure TExtDBNumEdit.Loaded;
begin
  inherited Loaded;
  FOldColor := Color;
  if  FAlwaysSame
   Then
    Color := gCol_Edit ;
end;

procedure TExtDBNumEdit.WMPaint(var Message: {$IFDEF FPC}TLMPaint{$ELSE}TWMPaint{$ENDIF});
Begin
  p_setCompColorReadOnly ( Self,FColorEdit,FColorReadOnly, FAlwaysSame, ReadOnly );
  inherited;
End;

procedure TExtDBNumEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbRight Then
   fb_ShowPopup (Self,PopUpMenu,OnContextPopup,FOnPopup);
end;

procedure TExtDBNumEdit.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if  ( Operation  = opRemove )
  and ( AComponent = FLabel   )
   Then FLabel := nil;
end;


{ TExtNumEdit }

procedure TExtNumEdit.p_setLabel(const alab_Label: TLabel);
begin
  p_setMyLabel ( FLabel, alab_Label, Self );
end;

procedure TExtNumEdit.SetOrder;
begin
  if assigned ( FNotifyOrder ) then
    FNotifyOrder ( Self );
end;

constructor TExtNumEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAlwaysSame := True;
  FColorLabel := CST_LBL_SELECT;
  FColorEdit  := CST_EDIT_STD;
  FColorFocus := CST_EDIT_SELECT;
  FColorReadOnly := CST_EDIT_READ;
end;

procedure TExtNumEdit.DoEnter;
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

procedure TExtNumEdit.DoExit;
begin
  if assigned ( FBeforeExit ) Then
    FBeforeExit ( Self );
  inherited DoExit;
  p_setLabelColorExit ( FLabel, FAlwaysSame );
  p_setCompColorExit ( Self, FOldColor, FAlwaysSame );

end;

procedure TExtNumEdit.Loaded;
begin
  inherited Loaded;
  FOldColor := Color;
  if  FAlwaysSame
   Then
    Color := gCol_Edit ;
end;

procedure TExtNumEdit.WMPaint(var Message: {$IFDEF FPC}TLMPaint{$ELSE}TWMPaint{$ENDIF});
Begin
  p_setCompColorReadOnly ( Self,FColorEdit,FColorReadOnly, FAlwaysSame, ReadOnly );
  inherited;
End;

procedure TExtNumEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbRight Then
   fb_ShowPopup (Self,PopUpMenu,OnContextPopup,FOnPopup);
end;

procedure TExtNumEdit.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if  ( Operation  = opRemove )
  and ( AComponent = FLabel   )
   Then FLabel := nil;
end;


{$IFDEF VERSIONS}
initialization
  p_ConcatVersion ( gVer_TExtNumEdit  );
  p_ConcatVersion ( gVer_TExtDBNumEdit );
{$ENDIF}
end.
