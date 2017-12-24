{*********************************************************************}
{                                                                     }
{                                                                     }
{             Matthieu Giroux                                         }
{             TExtPictCombo :                                        }
{             Objet de choix de couleur                               }
{             qui permet de personnalisé la couleur du titre          }
{             de l'onglet actif                                       }
{             10 Mars 2006                                            }
{                                                                     }
{                                                                     }
{*********************************************************************}

unit U_ExtDBPictCombo;

{$I ..\DLCompilers.inc}
{$I ..\extends.inc}
{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}

{$I ..\DLCompilers.inc}

interface

uses
{$IFDEF FPC}
  LCLIntf, lMessages,
{$ELSE}
  Windows, Variants,
{$ENDIF}
{$IFDEF TNT}
   TntStdCtrls,
{$ENDIF}
  Messages, SysUtils, Classes, Controls,
  DB, DBCtrls, fonctions_erreurs,
{$IFDEF VERSIONS}
  fonctions_version,
{$ENDIF}
  U_ExtPictCombo ;

{$IFDEF VERSIONS}
const

    gVer_TExtDbPictCombo : T_Version = ( Component : 'Composant TExtDbPictCombo' ;
                                               FileUnit : 'U_ExtPictCombo' ;
                                               Owner : 'Matthieu Giroux' ;
                                               Comment : 'Choisir une image dans une liste.' ;
                                               BugsStory : '0.9.9.0 : Tested more.' + #13#10 +
                                                           '0.9.0.0 : Tested and optimised.' + #13#10 +
                                                           '0.8.0.0 : Not tested.';
                                               UnitType : 3 ;
                                               Major : 0 ; Minor : 9 ; Release : 9 ; Build : 0 );

{$ENDIF}

type


  { TExtDBPictCombo }

  TExtDBPictCombo  = class( TExtPictCombo )
    private
      FDataLink: TFieldDataLink;
      function GetDataField: string;
      function GetDataSource: TDataSource;
      function GetField: TField;
      procedure SetDataField(const AValue: string);
      procedure SetDataSource(AValue: TDataSource);
      procedure WMCut(var Message: TMessage); message {$IFDEF FPC} LM_CUT {$ELSE} WM_CUT {$ENDIF};
      procedure WMPaste(var Message: TMessage); message {$IFDEF FPC} LM_PASTE {$ELSE} WM_PASTE {$ENDIF};
    {$IFDEF FPC}
    {$ELSE}
      procedure WMUndo(var Message: TMessage); message WM_UNDO;
    {$ENDIF}
      procedure CMExit(var Message: {$IFDEF FPC} TLMExit {$ELSE} TCMExit {$ENDIF}); message CM_EXIT;
      procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
      procedure CNCommand(var TheMessage: {$IFDEF FPC}TLMCommand{$ELSE}TWMCommand{$ENDIF}); message {$IFDEF FPC}CN_Command{$ELSE}WM_COMMAND{$ENDIF};
    protected
      function GetReadOnly: Boolean; override;
      procedure ActiveChange(Sender: TObject); virtual;
      procedure DataChange(Sender: TObject); virtual;
      procedure UpdateData(Sender: TObject); virtual;
      procedure p_SetValue(const AValue: String); override ;
      procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    public
      procedure DoEnter; override;
      procedure Loaded; override;
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      function ExecuteAction(AAction: TBasicAction): Boolean; override;
      function UpdateAction(AAction: TBasicAction): Boolean; override;
      property Field: TField read GetField;
    published
      property Value stored False ;
      property DataField: string read GetDataField write SetDataField stored True;
      property DataSource: TDataSource read GetDataSource write SetDataSource stored True;
    end;


implementation


{ TExtDBPictCombo }
constructor TExtDBPictCombo.Create(AOwner: TComponent);
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

destructor TExtDBPictCombo.Destroy;
begin
  inherited Destroy;
  FDataLink.Free ;
end;

procedure TExtDBPictCombo.Loaded;
begin
  inherited Loaded;
  Invalidate;
end;

procedure TExtDBPictCombo.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

function TExtDBPictCombo.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TExtDBPictCombo.SetDataSource(AValue: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := AValue;
  if AValue <> nil then AValue.FreeNotification(Self);
end;

function TExtDBPictCombo.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TExtDBPictCombo.SetDataField(const AValue: string);
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

function TExtDBPictCombo.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TExtDBPictCombo.ActiveChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
    begin
      p_SetValue ( FDataLink.Field.AsString );
    end;
end;

procedure TExtDBPictCombo.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
    begin
      p_SetValue ( FDataLink.Field.AsString );
    end;
end;


procedure TExtDBPictCombo.UpdateData(Sender: TObject);
begin
 FDataLink.Edit ;
  if Value <> '' Then
    Begin
      FDataLink.Field.Value := Value ;
    End
   else
    FDataLink.Field.Value := Null ;
end;

{$IFDEF DELPHI}
procedure TExtDBPictCombo.WMUndo(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;
{$ENDIF}

procedure TExtDBPictCombo.WMPaste(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TExtDBPictCombo.WMCut(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TExtDBPictCombo.CMExit(var Message: {$IFDEF FPC} TLMExit {$ELSE} TCMExit {$ENDIF});
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

procedure TExtDBPictCombo.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

procedure TExtDBPictCombo.CNCommand(var TheMessage: {$IFDEF FPC}TLMCommand{$ELSE}TWMCommand{$ENDIF});
begin
  if ( GetReadOnly or not FDataLink.CanModify )
  and ( TheMessage.NotifyCode = CBN_DROPDOWN )
   Then exit;
  inherited;
end;

function TExtDBPictCombo.GetReadOnly: Boolean;
begin
  Result:=inherited GetReadOnly or FDataLink.ReadOnly;
end;

procedure TExtDBPictCombo.DoEnter;
begin
  if ( GetReadOnly or not FDataLink.CanModify ) Then Exit;
  inherited DoEnter;
end;

function TExtDBPictCombo.ExecuteAction(AAction: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(AAction){$IFDEF DELPHI}  or (FDataLink <> nil) and
    FDataLink.ExecuteAction(AAction){$ENDIF};
end;

function TExtDBPictCombo.UpdateAction(AAction: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(AAction) {$IFDEF DELPHI}  or (FDataLink <> nil) and
    FDataLink.UpdateAction(AAction){$ENDIF};
end;

procedure TExtDBPictCombo.p_SetValue(const AValue: String);
begin
 inherited p_SetValue ( AValue );
 if assigned ( FDataLink.Field )
 and ( FDataLink.Field.AsString <> AValue ) Then
  Begin
    FDataLink.Dataset.Edit ;
    FDataLink.Field.Value := AValue ;
  End;
end;

{$IFDEF VERSIONS}
initialization
  p_ConcatVersion ( gVer_TExtDBPictCombo   );
{$ENDIF}
end.
