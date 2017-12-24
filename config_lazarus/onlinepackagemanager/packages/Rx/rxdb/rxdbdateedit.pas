{ dbdateedit unit

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

unit rxdbdateedit;

{$I rx.inc}

interface

uses
  Classes, SysUtils, LResources, LMessages, LCLType, Controls, Graphics,
  DB, DbCtrls, EditBtn, rxtooledit;

type

  { TRxDBDateEdit }
  TRxDBDateEdit = class(TRxDateEdit)
  private
    FDataLink:TFieldDataLink;
    FDefaultToday: Boolean;
    procedure DoCheckEnable;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetReadOnly: Boolean;
    procedure SetDataField(const AValue: string);
    procedure SetDataSource(const AValue: TDataSource);
    procedure SetReadOnly(const AValue: Boolean);
    procedure CMGetDataLink(var Message: TLMessage); message CM_GETDATALINK;
  protected
    procedure ActiveChange(Sender:TObject);
    procedure DataChange(Sender:TObject);
    procedure EditingChange(Sender: TObject);
    procedure UpdateData(Sender:TObject);
    procedure CMExit(var Message:TLMessage); message CM_EXIT;
    procedure LMCut(var Message: TLMessage); message LM_CUT;
    procedure LMPaste(var Message: TLMessage); message LM_PASTE;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure ButtonClick; override;
    procedure EditChange; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure EditingDone; override;
    Procedure RunDialog; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property DefaultToday: Boolean read FDefaultToday write FDefaultToday
      default False;
  end;


  { TRxDBCalcEdit }

  TRxDBCalcEdit = class(TCalcEdit)
  private
    FDataLink: TFieldDataLink;
    procedure DoCheckEnable;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetReadOnly: Boolean;
    procedure SetDataField(const AValue: string);
    procedure SetDataSource(const AValue: TDataSource);
    procedure SetReadOnly(const AValue: Boolean);
    procedure CMGetDataLink(var Message: TLMessage); message CM_GETDATALINK;
  protected
    procedure ActiveChange(Sender:TObject);
    procedure DataChange(Sender:TObject);
    procedure EditingChange(Sender: TObject);
    procedure UpdateData(Sender:TObject);
    procedure CMExit(var Message:TLMessage); message CM_EXIT;
    procedure LMCut(var Message: TLMessage); message LM_CUT;
    procedure LMPaste(var Message: TLMessage); message LM_PASTE;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure EditChange; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure EditingDone; override;
    Procedure RunDialog; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
  end;
  
implementation
uses rxdateutil;

{ TRxDBDateEdit }

procedure TRxDBDateEdit.DoCheckEnable;
begin
  Enabled:=FDataLink.Active and (FDataLink.Field<>nil) and (not FDataLink.Field.ReadOnly);
end;

function TRxDBDateEdit.GetDataField: string;
begin
  Result:=FDataLink.FieldName;
end;

function TRxDBDateEdit.GetDataSource: TDataSource;
begin
  Result:=FDataLink.DataSource;
end;

function TRxDBDateEdit.GetReadOnly: Boolean;
begin
  Result:=FDataLink.ReadOnly;
end;

procedure TRxDBDateEdit.SetDataField(const AValue: string);
begin
  try
    FDataLink.FieldName:=AValue;
  finally
    DoCheckEnable;
  end;
end;

procedure TRxDBDateEdit.SetDataSource(const AValue: TDataSource);
begin
  FDataLink.DataSource:=AValue;
  DoCheckEnable;
end;

procedure TRxDBDateEdit.SetReadOnly(const AValue: Boolean);
begin
  inherited ReadOnly:=AValue;
  FDataLink.ReadOnly:=AValue;
end;

procedure TRxDBDateEdit.CMGetDataLink(var Message: TLMessage);
begin
  Message.Result := PtrUInt(FDataLink);
end;

procedure TRxDBDateEdit.CMExit(var Message: TLMessage);
begin
  try
    FDataLink.UpdateRecord;
  except
    SetFocus;
    SelectAll;
    raise;
  end;
  inherited;
end;

procedure TRxDBDateEdit.LMCut(var Message: TLMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TRxDBDateEdit.LMPaste(var Message: TLMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TRxDBDateEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if Key=VK_ESCAPE then
  begin
    //cancel out of editing by reset on esc
    FDataLink.Reset;
    SelectAll;
    Key := VK_UNKNOWN;
  end
  else
  if (Key<>VK_UNKNOWN) then
  begin
    //make sure we call edit to ensure the datset is in edit,
    //this is for where the datasource is in autoedit, so we aren't
    //read only even though the dataset isn't realy in edit
    FDataLink.Edit;
  end;
end;

procedure TRxDBDateEdit.EditChange;
begin
  if Assigned(FDataLink) then
    FDataLink.Modified;
  inherited EditChange;
end;

procedure TRxDBDateEdit.Notification(AComponent: TComponent; Operation: TOperation
  );
begin
  inherited Notification(AComponent, Operation);
  // if the datasource is being removed then we need to make sure
  // we are updated or we can get AV/Seg's *cough* as I foolishly
  // discovered firsthand....
  if (Operation=opRemove) then
  begin
    if (FDataLink<>nil) and (AComponent=DataSource) then
      DataSource:=nil;
  end;
end;

procedure TRxDBDateEdit.EditingDone;
begin
  inherited EditingDone;
  if FDataLink.CanModify then
    FDataLink.UpdateRecord;
end;

procedure TRxDBDateEdit.RunDialog;
begin
  if FDataLink.CanModify then
    FDataLink.UpdateRecord;
end;

procedure TRxDBDateEdit.ButtonClick;
begin
  inherited ButtonClick;
  RunDialog;
end;

procedure TRxDBDateEdit.ActiveChange(Sender: TObject);
begin
  DoCheckEnable;
end;

procedure TRxDBDateEdit.DataChange(Sender: TObject);
begin
  if Assigned(FDataLink.Field) and
    (FDataLink.Field is TDateTimeField) then
  begin
    if FDataLink.Field.IsNull then
      Text:=''
    else
      Date:=FDataLink.Field.AsDateTime
  end
  else Text:='';
end;

procedure TRxDBDateEdit.EditingChange(Sender: TObject);
begin
  inherited ReadOnly := not FDataLink.Editing;
  if FDataLink.Editing and DefaultToday and (FDataLink.Field <> nil) and
    (FDataLink.Field.AsDateTime = NullDate) then
    FDataLink.Field.AsDateTime := SysUtils.Now;
end;

procedure TRxDBDateEdit.UpdateData(Sender: TObject);
var
  D: TDateTime;
begin
  if Assigned(FDataLink.Field) then
  begin
    D := Self.Date;
    if (D <> NullDate) then
      FDataLink.Field.AsDateTime := D + Frac(FDataLink.Field.AsDateTime)
    else
      FDataLink.Field.Clear;
  end;
end;

constructor TRxDBDateEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink:=TFieldDataLink.Create;
  FDataLink.Control:=Self;
  FDataLink.OnActiveChange:=@ActiveChange;
  FDataLink.OnDataChange:=@DataChange;
  FDataLink.OnUpdateData:=@UpdateData;
  Text:='';
  //UpdateMask;
end;

destructor TRxDBDateEdit.Destroy;
begin
  FreeAndNil(FDataLink);
  inherited Destroy;
end;


{ TRxDBCalcEdit }

procedure TRxDBCalcEdit.DoCheckEnable;
begin
  Enabled:=FDataLink.Active and (FDataLink.Field<>nil) and (not FDataLink.Field.ReadOnly);
end;

function TRxDBCalcEdit.GetDataField: string;
begin
  Result:=FDataLink.FieldName;
end;

function TRxDBCalcEdit.GetDataSource: TDataSource;
begin
  Result:=FDataLink.DataSource;
end;

function TRxDBCalcEdit.GetReadOnly: Boolean;
begin
  Result:=FDataLink.ReadOnly;
end;

procedure TRxDBCalcEdit.SetDataField(const AValue: string);
begin
  try
    FDataLink.FieldName:=AValue;
  finally
    DoCheckEnable;
  end;
end;

procedure TRxDBCalcEdit.SetDataSource(const AValue: TDataSource);
begin
  FDataLink.DataSource:=AValue;
  DoCheckEnable;
end;

procedure TRxDBCalcEdit.SetReadOnly(const AValue: Boolean);
begin
  FDataLink.ReadOnly:=AValue;
end;

procedure TRxDBCalcEdit.CMGetDataLink(var Message: TLMessage);
begin
  Message.Result := PtrUInt(FDataLink);
end;

procedure TRxDBCalcEdit.ActiveChange(Sender: TObject);
begin
  DoCheckEnable;
end;

procedure TRxDBCalcEdit.DataChange(Sender: TObject);
begin
  if Assigned(FDataLink.Field) and
    (FDataLink.Field is TNumericField) then
  begin
    if FDataLink.Field.IsNull then
      Text:=''
    else
      Self.AsFloat:=FDataLink.Field.AsFloat;
  end
  else Text:='';
end;

procedure TRxDBCalcEdit.EditingChange(Sender: TObject);
begin
  inherited ReadOnly := not FDataLink.Editing;
{  if FDataLink.Editing and DefaultToday and (FDataLink.Field <> nil) and
    (FDataLink.Field.AsDateTime = NullDate) then
    FDataLink.Field.AsDateTime := SysUtils.Now;}
end;

procedure TRxDBCalcEdit.UpdateData(Sender: TObject);
begin
  if Assigned(FDataLink.Field) and FDataLink.Edit then
  begin
    if Self.Text<>'' then
      FDataLink.Field.AsFloat := Self.AsFloat
    else
      FDataLink.Field.Clear;
  end;
end;

procedure TRxDBCalcEdit.CMExit(var Message: TLMessage);
begin
  try
    FDataLink.UpdateRecord;
  except
    SetFocus;
    SelectAll;
    raise;
  end;
  inherited;
end;

procedure TRxDBCalcEdit.LMCut(var Message: TLMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TRxDBCalcEdit.LMPaste(var Message: TLMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TRxDBCalcEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if Key=VK_ESCAPE then
  begin
    //cancel out of editing by reset on esc
    FDataLink.Reset;
    SelectAll;
    Key := VK_UNKNOWN;
  end
  else
  if (Key<>VK_UNKNOWN) then
  begin
    //make sure we call edit to ensure the datset is in edit,
    //this is for where the datasource is in autoedit, so we aren't
    //read only even though the dataset isn't realy in edit
    FDataLink.Edit;
  end;
end;

procedure TRxDBCalcEdit.EditChange;
begin
  FDataLink.Modified;
  inherited EditChange;
end;

procedure TRxDBCalcEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  // if the datasource is being removed then we need to make sure
  // we are updated or we can get AV/Seg's *cough* as I foolishly
  // discovered firsthand....
  if (Operation=opRemove) then
  begin
    if (FDataLink<>nil) and (AComponent=DataSource) then
      DataSource:=nil;
  end;
end;

procedure TRxDBCalcEdit.EditingDone;
begin
  inherited EditingDone;
  if FDataLink.CanModify then
    FDataLink.UpdateRecord;
end;

procedure TRxDBCalcEdit.RunDialog;
begin
  inherited RunDialog;
  if FDataLink.CanModify then
    FDataLink.UpdateRecord;
end;

constructor TRxDBCalcEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink:=TFieldDataLink.Create;
  FDataLink.Control:=Self;
  FDataLink.OnActiveChange:=@ActiveChange;
  FDataLink.OnDataChange:=@DataChange;
  FDataLink.OnUpdateData:=@UpdateData;
end;

destructor TRxDBCalcEdit.Destroy;
begin
  FreeAndNil(FDataLink);
  inherited Destroy;
end;

end.
