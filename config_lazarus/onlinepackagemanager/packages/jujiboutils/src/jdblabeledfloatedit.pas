{ JDBLabeledFloatEdit

  Copyright (C) 2011 Julio Jiménez Borreguero
  Contact: jujibo at gmail dot com

  This library is free software; you can redistribute it and/or modify it
  under the same terms as the Lazarus Component Library (LCL)

  See the file license-jujiboutils.txt and COPYING.LGPL, included in this distribution,
  for details about the license.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}

unit JDBLabeledFloatEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, LResources, Controls, ExtCtrls, DB, DBCtrls,
  LMessages, LCLType, Dialogs,
  SysUtils, jinputconsts;

type

  { TJDBLabeledFloatEdit }

  TJDBLabeledFloatEdit = class(TCustomLabeledEdit)
  private
    fFormat: string;
    fEFormat: string;
    FDataLink: TFieldDataLink;
    fDecimales: integer;
    fNull: boolean;

    procedure DataChange(Sender: TObject);
    function getDecimals: integer;
    procedure setDecimals(AValue: integer);
    procedure UpdateData(Sender: TObject);
    procedure FocusRequest(Sender: TObject);

    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;

    function IsReadOnly: boolean;

    function getFormat: string;
    procedure setFormat(const AValue: string);
    procedure formatInput;

    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure CMGetDataLink(var Message: TLMessage); message CM_GETDATALINK;

    function IsValidFloat(const Value: string): boolean;
    function ScaleTo(const AValue: double; const NDecimals: integer): double;

  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ActiveChange(Sender: TObject); virtual;
    procedure KeyDown(var Key: word; Shift: TShiftState); override;
    procedure KeyPress(var Key: char); override;
    procedure DoEnter; override;
    function GetReadOnly: boolean; override;
    procedure SetReadOnly(Value: boolean); override;

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure EditingDone; override;
    property Field: TField read GetField;

  published
    property DisplayFormat: string read getFormat write setFormat;
    property EditFormat: string read fEFormat write fEFormat;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property Decimals: integer read getDecimals write setDecimals;
    property ReadOnly: boolean read GetReadOnly write SetReadOnly default False;
    property AllowNull: boolean read fNull write fNull default False;

    property Action;
    property Align;
    property Alignment;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property DragCursor;
    property DragMode;
    property EditLabel;
    property Enabled;
    property Font;
    property LabelPosition;
    property LabelSpacing;
    property MaxLength;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
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
    property OnStartDrag;
    property OnUTF8KeyPress;
  end;

procedure Register;

implementation

uses
  Math, jdbutils;

procedure Register;
begin
  {$I jdblabeledfloatedit_icon.lrs}
  RegisterComponents('JujiboDB', [TJDBLabeledFloatEdit]);
end;


procedure TJDBLabeledFloatEdit.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
  begin
    if not Focused then
      formatInput
    else
    if Length(EditFormat) > 0 then
      Caption := FormatFloat(EditFormat, FDataLink.Field.AsFloat)
    else
      Caption := FloatToStr(FDataLink.Field.AsFloat);
  end
  else
    Text := '';
end;

function TJDBLabeledFloatEdit.getDecimals: integer;
begin
  Result := fDecimales;
end;

procedure TJDBLabeledFloatEdit.setDecimals(AValue: integer);
begin
  if AValue >= 0 then
    fDecimales := AValue;
end;


procedure TJDBLabeledFloatEdit.UpdateData(Sender: TObject);
var
  theValue: double;
begin
  if FDataLink.Field <> nil then
  begin
    if fNull and (Length(Caption) = 0) then
      FDataLink.Field.Value := Null
    else
    if IsValidFloat(Text) then
    begin
      theValue := StrToFloat(Text);
      if fDecimales > 0 then
        theValue := ScaleTo(theValue, fDecimales);
      if Length(EditFormat) > 0 then
        Caption := FormatFloat(EditFormat, theValue)
      else
        Caption := FloatToStr(theValue);
      FDataLink.Field.Value := theValue;
    end
    else
    begin
      if FDataLink.Field <> nil then
      begin
        ShowMessage(Format(SInvalidNumber, [Caption]));
        if Length(EditFormat) > 0 then
          Caption := FormatFloat(EditFormat, FDataLink.Field.AsFloat)
        else
          Caption := FloatToStr(FDataLink.Field.AsFloat);
        SelectAll;
        SetFocus;
      end;
    end;
  end
  else
    Text := '';
end;

procedure TJDBLabeledFloatEdit.FocusRequest(Sender: TObject);
begin
  SetFocus;
end;

function TJDBLabeledFloatEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TJDBLabeledFloatEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TJDBLabeledFloatEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TJDBLabeledFloatEdit.IsReadOnly: boolean;
begin
  if FDatalink.Active then
    Result := not FDatalink.CanModify
  else
    Result := False;
end;

function TJDBLabeledFloatEdit.getFormat: string;
begin
  Result := fFormat;
end;

procedure TJDBLabeledFloatEdit.setFormat(const AValue: string);
begin
  fFormat := AValue;
  if not Focused then
    formatInput;
end;

procedure TJDBLabeledFloatEdit.formatInput;
begin
  if FDataLink.Field <> nil then
    //FDataLink.Field.DisplayText -> formatted  (tdbgridcolumns/persistent field DisplayFormat
    if FDataLink.Field.IsNull then
      Caption := ''
    else
    if fFormat <> '' then
      Caption := FormatFloat(fFormat, FDataLink.Field.AsFloat)
    else
      Caption := FDataLink.Field.DisplayText
  else
    Caption := 'nil';
end;

function TJDBLabeledFloatEdit.GetReadOnly: boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TJDBLabeledFloatEdit.SetReadOnly(Value: boolean);
begin
  inherited;
  FDataLink.ReadOnly := Value;
end;

procedure TJDBLabeledFloatEdit.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

procedure TJDBLabeledFloatEdit.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    ChangeDataSource(Self, FDataLink, Value);
end;

procedure TJDBLabeledFloatEdit.CMGetDataLink(var Message: TLMessage);
begin
  Message.Result := PtrUInt(FDataLink); // Delphi dbctrls compatibility?
end;

function TJDBLabeledFloatEdit.IsValidFloat(const Value: string): boolean;
begin
  if StrToFloatDef(Value, MaxDouble) = MaxDouble then
    Result := False
  else
    Result := True;
end;

function TJDBLabeledFloatEdit.ScaleTo(const AValue: double;
  const NDecimals: integer): double;
begin
  Result := round(AValue * power(10, NDecimals)) / power(10, NDecimals);
end;

procedure TJDBLabeledFloatEdit.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then
    DataChange(Self);
end;

procedure TJDBLabeledFloatEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  // clean up
  if (Operation = opRemove) then
  begin
    if (FDataLink <> nil) and (AComponent = DataSource) then
      DataSource := nil;
  end;
end;

procedure TJDBLabeledFloatEdit.ActiveChange(Sender: TObject);
begin
  if FDatalink.Active then
    datachange(Sender)
  else
    Text := '';
end;

procedure TJDBLabeledFloatEdit.KeyDown(var Key: word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if Key = VK_ESCAPE then
  begin
    FDataLink.Reset;
    SelectAll;
    Key := VK_UNKNOWN;
  end
  else
  if Key in [VK_DELETE, VK_BACK] then
  begin
    if not IsReadOnly then
      FDatalink.Edit
    else
      Key := VK_UNKNOWN;
  end;
end;

procedure TJDBLabeledFloatEdit.KeyPress(var Key: char);
begin
  if not FieldIsEditable(Field) or not FDatalink.Edit then
    Key := #0;
  if (Key in ['.', ',']) then
    Key := Decimalseparator;
  if (key = DecimalSeparator) and (Pos(key, Text) > 0) then
    key := #0;
  if not (Key in ['0'..'9', DecimalSeparator, '+', '-', #8, #9]) then
    Key := #0;
  if (Key <> #0) and (not IsReadOnly) then
    FDatalink.Edit;
  inherited KeyPress(Key);
end;

procedure TJDBLabeledFloatEdit.DoEnter;
begin
  if not FieldIsEditable(Field) or IsReadOnly then
    exit;
  if FDataLink.Field <> nil then
    if Length(EditFormat) > 0 then
      Caption := FormatFloat(EditFormat, FDataLink.Field.AsFloat)
    else
      Caption := FloatToStr(FDataLink.Field.AsFloat);  //FDataLink.Field.AsString;
  inherited DoEnter;
end;

constructor TJDBLabeledFloatEdit.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := @DataChange;
  FDataLink.OnUpdateData := @UpdateData;
  FDataLInk.OnActiveChange := @ActiveChange;
  fEFormat := '';
  // Set default values
  //fDecimales := 2;
  //fFormat := '0.00';
end;

destructor TJDBLabeledFloatEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TJDBLabeledFloatEdit.EditingDone;
begin
  if not FieldIsEditable(Field) or IsReadOnly then
    exit;
  if DataSource.State in [dsEdit, dsInsert] then
    UpdateData(self)
  else
    formatInput;
  inherited EditingDone;
end;


end.

