unit JDBLabeledTimeEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, LResources, Controls, ExtCtrls, DB, DBCtrls, LMessages, LCLType, Dialogs,
  SysUtils, jcontrolutils, jinputconsts, jdbutils;

type
  TJDBLabeledTimeEdit = class(TCustomLabeledEdit)
  private
    { Private declarations }
    fFormat: string;
    FDataLink: TFieldDataLink;

    procedure DataChange(Sender: TObject);
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
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: boolean read GetReadOnly write SetReadOnly default False;

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

procedure Register;
begin
  {$I jdblabeledtimeedit_icon.lrs}
  RegisterComponents('JujiboDB', [TJDBLabeledTimeEdit]);
end;

procedure TJDBLabeledTimeEdit.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
  begin
    if not Focused then
      formatInput
    else
      Caption := FDataLink.Field.AsString;
  end
  else
    Text := '';
end;

procedure TJDBLabeledTimeEdit.UpdateData(Sender: TObject);
var
  theValue: string;
begin
  if FDataLink.Field <> nil then
  begin
    theValue := NormalizeTime(Text, FDataLink.Field.AsDateTime);
    if Text = '' then
    begin
      Field.Value := Null;
    end
    else
    if IsValidTimeString(theValue) then
    begin
      FDataLink.Field.Text := theValue;
    end
    else
    begin
      ShowMessage(Format(SInvalidTime, [Text]));
      Caption := FDataLink.Field.AsString;
      SelectAll;
      SetFocus;
    end;
  end
  else
    Text := '';
end;

procedure TJDBLabeledTimeEdit.FocusRequest(Sender: TObject);
begin
  SetFocus;
end;

function TJDBLabeledTimeEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TJDBLabeledTimeEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TJDBLabeledTimeEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TJDBLabeledTimeEdit.IsReadOnly: boolean;
begin
  if FDatalink.Active then
    Result := not FDatalink.CanModify
  else
    Result := False;
end;

function TJDBLabeledTimeEdit.getFormat: string;
begin
  Result := fFormat;
end;

procedure TJDBLabeledTimeEdit.setFormat(const AValue: string);
begin
  fFormat := AValue;
  if not Focused then
    formatInput;
end;

procedure TJDBLabeledTimeEdit.formatInput;
begin
  if FDataLink.Field <> nil then
    if (fFormat <> '') and (not FDataLink.Field.IsNull) then
      Caption := FormatDateTime(fFormat, FDataLink.Field.AsDateTime)
    else
      Caption := FDataLink.Field.DisplayText
  else
    Caption := 'nil';
end;

function TJDBLabeledTimeEdit.GetReadOnly: boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TJDBLabeledTimeEdit.SetReadOnly(Value: boolean);
begin
  inherited;
  FDataLink.ReadOnly := Value;
end;

procedure TJDBLabeledTimeEdit.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

procedure TJDBLabeledTimeEdit.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    ChangeDataSource(Self, FDataLink, Value);
end;

procedure TJDBLabeledTimeEdit.CMGetDataLink(var Message: TLMessage);
begin
  Message.Result := PtrUInt(FDataLink); // Delphi dbctrls compatibility?
end;

procedure TJDBLabeledTimeEdit.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then
    DataChange(Self);
end;

procedure TJDBLabeledTimeEdit.Notification(AComponent: TComponent;
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

procedure TJDBLabeledTimeEdit.ActiveChange(Sender: TObject);
begin
  if FDatalink.Active then
    datachange(Sender)
  else
    Text := '';
end;

procedure TJDBLabeledTimeEdit.KeyDown(var Key: word; Shift: TShiftState);
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

procedure TJDBLabeledTimeEdit.KeyPress(var Key: char);
begin
  if not FieldIsEditable(Field) or not FDatalink.Edit then
    Key := #0;
  if not (Key in ['0'..'9', #8, #9, ':']) then
    Key := #0;
  inherited KeyPress(Key);
end;

procedure TJDBLabeledTimeEdit.DoEnter;
begin
  if not FieldIsEditable(Field) or IsReadOnly then
    exit;
  if FDataLink.Field <> nil then
    Caption := FDataLink.Field.AsString;
  inherited DoEnter;
end;

constructor TJDBLabeledTimeEdit.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := @DataChange;
  FDataLink.OnUpdateData := @UpdateData;
  FDataLInk.OnActiveChange := @ActiveChange;
  // Set default values
  //fFormat := ShortDateFormat;
end;

destructor TJDBLabeledTimeEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TJDBLabeledTimeEdit.EditingDone;
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

