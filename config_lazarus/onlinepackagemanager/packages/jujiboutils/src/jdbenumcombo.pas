{ JDbEnumcombo

  Copyright (C) 2013 Julio Jiménez Borreguero
  Contact: jujibo at gmail dot com

  This library is free software; you can redistribute it and/or modify it
  under the same terms as the Lazarus Component Library (LCL)

  See the file license-jujiboutils.txt and COPYING.LGPL, included in this distribution,
  for details about the license.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}


unit JDbEnumCombo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, StdCtrls, Graphics, Dialogs,
  DBCtrls, DB, LCLType, LMessages;

type

  { TJDbEnumCombo }

  TJDbEnumCombo = class(TCustomComboBox)
  private
    FDataLink: TFieldDataLink;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: boolean;
    procedure SetDataField(const AValue: string);
    procedure SetDataSource(const AValue: TDataSource);
    procedure SetReadOnly(const AValue: boolean);
    procedure CMGetDataLink(var Message: TLMessage); message CM_GETDATALINK;
  protected
    { Protected declarations }
    procedure DataChange(Sender: TObject);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Change; override;
    procedure DropDown; override;
    procedure UpdateData(Sender: TObject);
  public
    { Public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure EditingDone; override;
    property Field: TField read GetField;
    property Text;
    property ItemIndex;
  published
    { Published declarations }
    property Anchors;
    property ArrowKeysTraverseList;
    property AutoDropDown;
    property AutoComplete;
    property AutoSize;
    property BiDiMode;
    property BorderSpacing;
    property BorderStyle;
    property CharCase;
    property Color;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DragCursor;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property ItemHeight;
    property Items;
    property ItemWidth;
    property MaxLength default -1;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnCloseUp;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnSelect;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property ReadOnly: boolean read GetReadOnly write SetReadOnly default False;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
  end;

procedure Register;

implementation

uses
  jdbutils;

procedure Register;
begin
  {$I jdbenumcombo_icon.lrs}
  RegisterComponents('JujiboDB', [TJDbEnumCombo]);
end;

{ TJDbEnumCombo }

function TJDbEnumCombo.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TJDbEnumCombo.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TJDbEnumCombo.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TJDbEnumCombo.GetReadOnly: boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TJDbEnumCombo.SetDataField(const AValue: string);
begin
  FDataLink.FieldName := AValue;
end;

procedure TJDbEnumCombo.SetDataSource(const AValue: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    ChangeDataSource(Self, FDataLink, AValue);
end;

procedure TJDbEnumCombo.SetReadOnly(const AValue: boolean);
begin
  FDataLink.ReadOnly := AValue;
end;

procedure TJDbEnumCombo.CMGetDataLink(var Message: TLMessage);
begin
  Message.Result := PtrUInt(FDataLink);
end;

procedure TJDbEnumCombo.DataChange(Sender: TObject);
var
  DataLinkField: TField;
begin
  DataLinkField := FDataLink.Field;
  if Assigned(DataLinkField) then
    ItemIndex := DataLinkField.AsInteger
  else
    ItemIndex := -1;
end;

procedure TJDbEnumCombo.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
  begin
    if (FDataLink <> nil) and (AComponent = DataSource) then
      DataSource := nil;
  end;
end;

procedure TJDbEnumCombo.Change;
begin
  if not FieldIsEditable(Field) or ReadOnly then
    exit;
  FDataLink.Edit;
  FDataLink.Modified;
  FDataLink.UpdateRecord;
  inherited Change;
end;

procedure TJDbEnumCombo.DropDown;
begin
  if not FieldIsEditable(Field) or ReadOnly then
    abort;
  inherited DropDown;
end;

procedure TJDbEnumCombo.UpdateData(Sender: TObject);
begin
  if not FieldIsEditable(Field) or ReadOnly then
    exit;
  FDataLink.Field.AsInteger := ItemIndex;
end;

constructor TJDbEnumCombo.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := @DataChange;
  FDataLink.OnUpdateData := @UpdateData;
  Style := csDropDownList;
end;

destructor TJDbEnumCombo.Destroy;
begin
  FDataLink.Destroy;
  inherited Destroy;
end;

procedure TJDbEnumCombo.EditingDone;
begin
  FDataLink.UpdateRecord;
  inherited EditingDone;
end;

{ TJDbEnumCombo }



end.
