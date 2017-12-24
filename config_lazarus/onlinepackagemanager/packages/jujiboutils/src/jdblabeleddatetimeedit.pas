{ JDBLabeledDateTimeEdit

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

unit JDBLabeledDateTimeEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, LResources, Controls, ExtCtrls, DB, DBCtrls, LMessages, LCLType, Dialogs,
  SysUtils, jinputconsts, CalendarPopup, Calendar, Buttons;

type

  { TJDBLabeledDateTimeEdit }

  TJDBLabeledDateTimeEdit = class(TCustomLabeledEdit)
  private
    fFormat: string;
    FDataLink: TFieldDataLink;

    FButton: TSpeedButton;
    FButtonNeedsFocus: boolean;
    function GetButtonWidth: integer;
    procedure SetButtonWidth(AValue: integer);
    procedure WMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
    procedure WMKillFocus(var Message: TLMKillFocus); message LM_KILLFOCUS;


    procedure DataChange(Sender: TObject);
    procedure UpdateData(Sender: TObject);
    procedure FocusRequest(Sender: TObject);

    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;

    function IsReadOnly: boolean;

    function EditText: string;
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

    procedure SetParent(AParent: TWinControl); override;
    procedure DoPositionButton; virtual;
    procedure CheckButtonVisible;
    procedure CMVisibleChanged(var Msg: TLMessage); message CM_VISIBLECHANGED;
    procedure CMEnabledChanged(var Msg: TLMessage); message CM_ENABLEDCHANGED;
    procedure CMBiDiModeChanged(var Message: TLMessage); message CM_BIDIMODECHANGED;
    procedure ShowCalendar(Sender: TObject);
    procedure CalendarPopupReturnDate(Sender: TObject; const ADate: TDateTime);

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

    property Button: TSpeedButton read FButton;
    property ButtonWidth: integer read GetButtonWidth write SetButtonWidth;

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
  jcontrolutils, dateutils, jdbutils;

procedure Register;
begin
  {$I jdblabeleddatetimeedit_icon.lrs}
  RegisterComponents('JujiboDB', [TJDBLabeledDateTimeEdit]);
end;

function TJDBLabeledDateTimeEdit.GetButtonWidth: integer;
begin
  Result := FButton.Width;
end;

procedure TJDBLabeledDateTimeEdit.SetButtonWidth(AValue: integer);
begin
  FButton.Width := AValue;
end;

procedure TJDBLabeledDateTimeEdit.WMSetFocus(var Message: TLMSetFocus);
begin
  CheckButtonVisible;
  inherited;
end;

procedure TJDBLabeledDateTimeEdit.WMKillFocus(var Message: TLMKillFocus);
begin
  CheckButtonVisible;
  inherited;
end;

procedure TJDBLabeledDateTimeEdit.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
  begin
    if not Focused then
      formatInput
    else
      Caption := EditText;
  end
  else
    Text := '';
end;

procedure TJDBLabeledDateTimeEdit.UpdateData(Sender: TObject);
var
  theValue: string;
begin
  if FDataLink.Field <> nil then
  begin
    theValue := NormalizeDateTime(Text, FDataLink.Field.AsDateTime);
    if Text = '' then
      FDataLink.Field.Text := Text
    else
    if IsValidDateTimeString(theValue) then
    begin
      FDataLink.Field.Text := theValue;
    end
    else
    begin
      ShowMessage(Format(SInvalidDateTime, [Caption]));
      Caption := EditText;
      SelectAll;
      SetFocus;
    end;
  end
  else
    Text := '';
end;

procedure TJDBLabeledDateTimeEdit.FocusRequest(Sender: TObject);
begin
  SetFocus;
end;

function TJDBLabeledDateTimeEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TJDBLabeledDateTimeEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TJDBLabeledDateTimeEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TJDBLabeledDateTimeEdit.IsReadOnly: boolean;
begin
  if FDatalink.Active then
    Result := not FDatalink.CanModify
  else
    Result := False;
end;

function TJDBLabeledDateTimeEdit.EditText: string;
begin
  if Field.IsNull then
    Result := ''
  else
    Result := FormatDateTime(ShortDateFormat, FDataLink.Field.AsDateTime) +
      ' ' + FormatDateTime(ShortTimeFormat, FDataLink.Field.AsDateTime);
end;

function TJDBLabeledDateTimeEdit.getFormat: string;
begin
  Result := fFormat;
end;

procedure TJDBLabeledDateTimeEdit.setFormat(const AValue: string);
begin
  fFormat := AValue;
  if not Focused then
    formatInput;
end;

procedure TJDBLabeledDateTimeEdit.formatInput;
begin
  if FDataLink.Field <> nil then
    if (fFormat <> '') and (not FDataLink.Field.IsNull) then
      Caption := FormatDateTime(fFormat, FDataLink.Field.AsDateTime)
    else
      Caption := EditText
  else
    Caption := 'nil';
end;

function TJDBLabeledDateTimeEdit.GetReadOnly: boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TJDBLabeledDateTimeEdit.SetReadOnly(Value: boolean);
begin
  inherited;
  FDataLink.ReadOnly := Value;
end;

procedure TJDBLabeledDateTimeEdit.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if FButton <> nil then
  begin
    DoPositionButton;
    CheckButtonVisible;
  end;
end;

procedure TJDBLabeledDateTimeEdit.DoPositionButton;
begin
  if FButton = nil then
    exit;
  FButton.Parent := Parent;
  FButton.Visible := True;
  if BiDiMode = bdLeftToRight then
    FButton.AnchorToCompanion(akLeft, 0, Self)
  else
    FButton.AnchorToCompanion(akRight, 0, Self);
end;

procedure TJDBLabeledDateTimeEdit.CheckButtonVisible;
begin
  if Assigned(FButton) then
    FButton.Visible := True;
end;

procedure TJDBLabeledDateTimeEdit.CMVisibleChanged(var Msg: TLMessage);
begin
  inherited CMVisibleChanged(Msg);
  CheckButtonVisible;
end;

procedure TJDBLabeledDateTimeEdit.CMEnabledChanged(var Msg: TLMessage);
begin
  inherited CMEnabledChanged(Msg);
  if (FButton <> nil) then
    FButton.Enabled := True;
end;

procedure TJDBLabeledDateTimeEdit.CMBiDiModeChanged(var Message: TLMessage);
begin
  inherited;
  DoPositionButton;
end;

procedure TJDBLabeledDateTimeEdit.ShowCalendar(Sender: TObject);
var
  PopupOrigin: TPoint;
  ADate: TDateTime;
begin
  if (not Assigned(FDataLink.Field)) or IsReadOnly then
    exit;
  PopupOrigin := Self.ControlToScreen(Point(0, Self.Height));
  if FDataLink.Field.IsNull then
    ADate := now
  else
    ADate := FDataLink.Field.AsDateTime;
  ShowCalendarPopup(PopupOrigin, ADate, [dsShowHeadings, dsShowDayNames],
    @CalendarPopupReturnDate, nil);
end;

procedure TJDBLabeledDateTimeEdit.CalendarPopupReturnDate(Sender: TObject;
  const ADate: TDateTime);
var
  bufdate: TDateTime;
begin
  if not (DataSource.State in [dsEdit, dsInsert]) then
    DataSource.Edit;
  if FDataLink.Field.IsNull then
    bufdate := now
  else
    bufdate := FDataLink.Field.AsDateTime;
  FDataLink.Field.AsDateTime :=
    EncodeDateTime(YearOf(ADate), MonthOf(ADate), DayOf(ADate),
    HourOf(bufdate), MinuteOf(bufdate), SecondOf(bufdate), MilliSecondOf(bufdate));
end;

procedure TJDBLabeledDateTimeEdit.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

procedure TJDBLabeledDateTimeEdit.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    ChangeDataSource(Self, FDataLink, Value);
end;

procedure TJDBLabeledDateTimeEdit.CMGetDataLink(var Message: TLMessage);
begin
  Message.Result := PtrUInt(FDataLink); // Delphi dbctrls compatibility?
end;

procedure TJDBLabeledDateTimeEdit.Loaded;
begin
  inherited Loaded;
  DoPositionButton;
  CheckButtonVisible;
  if (csDesigning in ComponentState) then
    DataChange(Self);
end;

procedure TJDBLabeledDateTimeEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FButton) and (Operation = opRemove) then
    FButton := nil;
  // clean up
  if (Operation = opRemove) then
  begin
    if (FDataLink <> nil) and (AComponent = DataSource) then
      DataSource := nil;
  end;
end;

procedure TJDBLabeledDateTimeEdit.ActiveChange(Sender: TObject);
begin
  if FDatalink.Active then
    datachange(Sender)
  else
    Text := '';
end;

procedure TJDBLabeledDateTimeEdit.KeyDown(var Key: word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if (ssAlt in Shift) and (key = 40) then
    ShowCalendar(Self);
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

procedure TJDBLabeledDateTimeEdit.KeyPress(var Key: char);
begin
  if not FieldIsEditable(Field) or not FDatalink.Edit then
    Key := #0;
  if (not Assigned(FDataLink.Field)) or IsReadOnly then
    key := #0;
  if not (Key in ['0'..'9', #8, #9, '.', '-', '/', ',', ':', ' ']) then
    Key := #0;
  inherited KeyPress(Key);
end;

procedure TJDBLabeledDateTimeEdit.DoEnter;
begin
  if not FieldIsEditable(Field) or IsReadOnly then
    exit;
  if FDataLink.Field <> nil then
    Caption := EditText;
  inherited DoEnter;
end;

constructor TJDBLabeledDateTimeEdit.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := @DataChange;
  FDataLink.OnUpdateData := @UpdateData;
  FDataLInk.OnActiveChange := @ActiveChange;

  FButton := TSpeedButton.Create(self);
  FButton.Height := Self.Height;
  FButton.FreeNotification(Self);
  CheckButtonVisible;
  FButton.Cursor := crArrow;
  FButton.Flat := False;

  FButton.OnClick := @ShowCalendar;
  FButton.ControlStyle := FButton.ControlStyle + [csNoDesignSelectable];
  FButton.LoadGlyphFromLazarusResource('JCalendarIcon');
  ControlStyle := ControlStyle - [csSetCaption];
end;

destructor TJDBLabeledDateTimeEdit.Destroy;
begin
  FreeAndNil(FDataLink);
  FreeAndNil(FButton);
  inherited Destroy;
end;

procedure TJDBLabeledDateTimeEdit.EditingDone;
begin
  inherited EditingDone;
  if not FieldIsEditable(Field) or IsReadOnly then
    exit;
  if (not Assigned(FDataLink.Field)) or IsReadOnly then
    exit;
  if DataSource.State in [dsEdit, dsInsert] then
    UpdateData(self)
  else
    formatInput;
  inherited EditingDone;
end;


end.
