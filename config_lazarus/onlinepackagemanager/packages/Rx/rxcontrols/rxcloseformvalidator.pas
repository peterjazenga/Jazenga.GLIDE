{ RxCloseFormValidator unit

  Copyright (C) 2005-2017 Lagunov Aleksey alexs75@yandex.ru and Lazarus team
  original conception from rx library for Delphi (c)

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit RxCloseFormValidator;

{$I rx.inc}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, DB;

type
  TRxCloseFormValidator = class;

  TValidateEvent = procedure(AOwner:TRxCloseFormValidator; AControl:TWinControl; var Validate:boolean) of object;

  { TValidateItem }

  TValidateItem = class(TCollectionItem)
  private
    FControl: TWinControl;
    FEnabled: boolean;
    FFieldCaption: string;
    FOnValidate: TValidateEvent;
    procedure SetControl(AValue: TWinControl);
    procedure SetEnabled(AValue: boolean);
    procedure SetFieldCaption(AValue: string);
    function DBComponentField:TField;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    function CheckClose(AForm:TCustomForm):boolean;
    function ErrorMessage:string;
    procedure SetFocus;
  published
    property Control:TWinControl read FControl write SetControl;
    property Enabled:boolean read FEnabled write SetEnabled default true;
    property FieldCaption:string read FFieldCaption write SetFieldCaption;
    property OnValidate:TValidateEvent read FOnValidate write FOnValidate;
  end;

  { TValidateItems }

  TValidateItems = class(TOwnedCollection)
  private
    function GetItems(Index: Integer): TValidateItem;
    procedure SetItems(Index: Integer; AValue: TValidateItem);
  public
    property Items[Index: Integer]: TValidateItem read GetItems write SetItems; default;
  end;

  { TRxCloseFormValidator }

  TRxCloseFormValidator = class(TComponent)
  private
    FErrorMsgCaption: string;
    FIgnoreDisabled: boolean;
    FOnCloseQuery : TCloseQueryEvent;
    FItems:TValidateItems;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    function GetItems: TValidateItems;
    procedure SetCloseQueryHandler;
    procedure SetItems(AValue: TValidateItems);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CheckCloseForm:boolean;
    function ByControl(AControl: TWinControl):TValidateItem;
  published
    property ErrorMsgCaption:string read FErrorMsgCaption write FErrorMsgCaption;
    property Items:TValidateItems read GetItems write SetItems;
    property IgnoreDisabled:boolean read FIgnoreDisabled write FIgnoreDisabled default false;
  end;

implementation
uses LCLType, StdCtrls, DbCtrls, typinfo, ComCtrls, ExtCtrls, rxconst;

{ TValidateItems }

function TValidateItems.GetItems(Index: Integer): TValidateItem;
begin
  result := TValidateItem( inherited Items[Index] );
end;

procedure TValidateItems.SetItems(Index: Integer; AValue: TValidateItem);
begin
  Items[Index].Assign( AValue );
end;

{constructor TValidateItems.Create;
begin
  inherited Create(TValidateItem);
end;}

{ TValidateItem }

procedure TValidateItem.SetControl(AValue: TWinControl);
var
  i:integer;
  OwnForm, P:TComponent;
  F:TField;
begin
  if FControl=AValue then Exit;
  FControl:=AValue;

  if Assigned(FControl) and (FFieldCaption = '') then
  begin
    //Установим название поля по текст компоненты
    if FControl is TCustomRadioGroup then
      FFieldCaption:=TCustomRadioGroup(FControl).Caption
    else
    if FControl is TCustomCheckBox then
      FFieldCaption:=TCustomCheckBox(FControl).Caption
    else
    if Assigned(FControl.Owner) then
    begin
      OwnForm:=FControl.Owner;
      //Попробуем найти название поле - по тексту метки, которая связана с данным полем
      for i:=0 to OwnForm.ComponentCount-1 do
      begin
        P:=OwnForm.Components[i];
        if P is TLabel then
          if TLabel(P).FocusControl = FControl then
          begin
            FFieldCaption:=TLabel(P).Caption;
            break;
          end;
      end;
    end;

    if FFieldCaption = '' then
    begin
      F:=DBComponentField;
      if Assigned(F) then
        FFieldCaption:=F.DisplayLabel;
    end;
  end
end;

procedure TValidateItem.SetEnabled(AValue: boolean);
begin
  if FEnabled=AValue then Exit;
  FEnabled:=AValue;
end;

procedure TValidateItem.SetFieldCaption(AValue: string);
begin
  if FFieldCaption=AValue then Exit;
  FFieldCaption:=AValue;
end;

function TValidateItem.DBComponentField: TField;
var
  P:TObject;
  PI1, PI2:PPropInfo;
  FiName:string;
  DS:TDataSet;
begin
  Result:=nil;
  if not Assigned(FControl) then exit;
  //Сначала проверим - вдруги это завязки на работу с БД
  PI1:=GetPropInfo(Control, 'DataSource');
  PI2:=GetPropInfo(Control, 'DataField');
  if Assigned(PI1) and Assigned(PI2) then
  begin
     //Точно - БД
     P:=GetObjectProp(Control, 'DataSource');
     FiName:=GetPropValue(Control, 'DataField');
     if Assigned(P) and (FiName<>'') then
     begin
       DS:=(P as TDataSource).DataSet;
       if Assigned(DS) then
         Result:=DS.FieldByName(FiName);
     end;
  end
end;

function TValidateItem.GetDisplayName: string;
begin
  if Assigned(FControl) then
  begin
    if FEnabled then
      Result:=FControl.Name + ' - validate'
    else
      Result:=FControl.Name + ' - disabled'
  end
  else
    Result:=inherited GetDisplayName;
end;

constructor TValidateItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FEnabled:=true;
end;

destructor TValidateItem.Destroy;
begin
  inherited Destroy;
end;

function TValidateItem.CheckClose(AForm: TCustomForm): boolean;
var
  P:TObject;
  PI1, PI2:PPropInfo;
  FiName:string;
  DS:TDataSet;
begin
  Result:=true;
  if not Assigned(FControl) then exit;

  if (not FControl.Enabled) and (TRxCloseFormValidator(TValidateItems(Collection).Owner).IgnoreDisabled) then
    exit;

  if Assigned(FOnValidate) then
    FOnValidate( TRxCloseFormValidator(TValidateItems(Collection).Owner), FControl, Result)
  else
  begin
    if FControl = AForm.ActiveControl then
    begin
      AForm.SelectNext(FControl, true, true);
    end;
    //Сначала проверим - вдруги это завязки на работу с БД
    PI1:=GetPropInfo(Control, 'DataSource');
    PI2:=GetPropInfo(Control, 'DataField');
    if Assigned(PI1) and Assigned(PI2) then
    begin
       //Точно - БД
       //Проверка выполняется если только указан источник данных и поле в нём
       P:=GetObjectProp(Control, 'DataSource');
       FiName:=GetPropValue(Control, 'DataField');
       if Assigned(P) and (FiName<>'') then
       begin
         DS:=(P as TDataSource).DataSet;
         if Assigned(DS) then
           Result:=not DS.FieldByName(FiName).IsNull;
       end;
    end
    else
    if Control is TCustomEdit then
      Result:=TCustomEdit(Control).Text<>'';
  end;
end;

function TValidateItem.ErrorMessage: string;
begin
  Result:=Format(sReqValue, [FFieldCaption]);
end;

procedure TValidateItem.SetFocus;
var
  P:TWinControl;
begin

  if FControl is TWinControl then
  begin
    P:=TWinControl(FControl).Parent;
    //Необходимо обработать случай нахождения компоненты на PageControl-e
    while Assigned(P) and not (P is TCustomForm) do
    begin
      if P is TTabSheet then
        TTabSheet(P).PageControl.ActivePage:=TTabSheet(P);
      P:=P.Parent;
    end;
    TWinControl(FControl).SetFocus;
  end;
end;

{ TRxCloseFormValidator }

procedure TRxCloseFormValidator.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  if Sender is TCustomForm then
  begin
    if TForm(Sender).ModalResult = mrOk then
    begin
     if CanClose and Assigned(FOnCloseQuery) then
       FOnCloseQuery(Sender, CanClose);
     if CanClose then
       CanClose:=CheckCloseForm;
    end;
  end;
end;

function TRxCloseFormValidator.CheckCloseForm: boolean;
var
  i:integer;
  F:TComponent;
begin
  F:=Owner;
  while Assigned(F) and not (F is TCustomForm) do
    F:=F.Owner;

  Result:=false;

  if not Assigned(F) then exit;

  for i:=0 to FItems.Count-1 do
  begin
    if FItems[i].Enabled and (not FItems[i].CheckClose(F as TCustomForm)) then
    begin
      FItems[i].SetFocus;
      Application.MessageBox(PChar(FItems[i].ErrorMessage), PChar(FErrorMsgCaption), MB_OK + MB_ICONERROR);
      exit;
    end;
  end;
  Result:=true;
end;

function TRxCloseFormValidator.ByControl(AControl: TWinControl): TValidateItem;
var
  i:integer;
begin
  Result:=nil;
  for i:=0 to FItems.Count - 1 do
  begin
    if FItems[i].FControl = AControl then
    begin
      Result:=FItems[i];
      exit;
    end;
  end;
  raise Exception.CreateFmt(sExptControlNotFound, [Name]);
end;

function TRxCloseFormValidator.GetItems: TValidateItems;
begin
  Result:=FItems;
end;

procedure TRxCloseFormValidator.SetCloseQueryHandler;
begin
  if (csDesigning in ComponentState) or (not Assigned(Owner)) then exit;
  if Owner is TCustomForm then
  begin
    FOnCloseQuery:=TForm(Owner).OnCloseQuery;
    TForm(Owner).OnCloseQuery:=@FormCloseQuery;
  end;
end;

procedure TRxCloseFormValidator.SetItems(AValue: TValidateItems);
begin
  FItems.Assign(AValue);
end;

procedure TRxCloseFormValidator.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  i:integer;
begin
  inherited Notification(AComponent, Operation);
  if AComponent = Self then exit;
  if Operation = opRemove then
  begin
    for i:=0 to FItems.Count - 1 do
      if FItems[i].Control = AComponent then
        FItems[i].Control := nil;
  end;
end;

procedure TRxCloseFormValidator.Loaded;
begin
  inherited Loaded;
  SetCloseQueryHandler;
end;

constructor TRxCloseFormValidator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FErrorMsgCaption:=sCloseValidError;
  FItems:=TValidateItems.Create(Self, TValidateItem);
end;

destructor TRxCloseFormValidator.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

end.
