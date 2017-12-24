{ TJDateTimeEdit

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



unit JLabeledDateTimeEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, LResources, Controls, ExtCtrls, LCLType, Dialogs,
  SysUtils, jinputconsts, CalendarPopup, Calendar, Buttons, LMessages;

type

  { TJLabeledDateTimeEdit }

  TJLabeledDateTimeEdit = class(TCustomLabeledEdit)
  private
    { Private declarations }
    theValue: TDateTime;
    fFormat: string;
    FButton: TSpeedButton;
    FButtonNeedsFocus: boolean;
    function GetButtonWidth: integer;
    function getFormat: string;
    function getValue: TDateTime;
    procedure formatInput;
    procedure SetButtonWidth(AValue: integer);
    procedure setFormat(const AValue: string);
    procedure setValue(const AValue: TDateTime);
    procedure WMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
    procedure WMKillFocus(var Message: TLMKillFocus); message LM_KILLFOCUS;
  protected
    { Protected declarations }
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure KeyDown(var Key: word; Shift: TShiftState); override;
    procedure KeyPress(var Key: char); override;
    procedure SetParent(AParent: TWinControl); override;
    procedure DoPositionButton; virtual;
    procedure CheckButtonVisible;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CMVisibleChanged(var Msg: TLMessage); message CM_VISIBLECHANGED;
    procedure CMEnabledChanged(var Msg: TLMessage); message CM_ENABLEDCHANGED;
    procedure CMBiDiModeChanged(var Message: TLMessage); message CM_BIDIMODECHANGED;
    procedure Loaded; override;
    procedure ShowCalendar(Sender: TObject);
    procedure CalendarPopupReturnDate(Sender: TObject; const ADate: TDateTime);

  public
    { Public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    function isNull: boolean;
    property DisplayFormat: string read getFormat write setFormat;
    property Value: TDateTime read getValue write setValue;
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
  jcontrolutils, dateutils;

procedure Register;
begin
  {$I jlabeleddatetimeedit_icon.lrs}
  RegisterComponents('Jujibo', [TJLabeledDateTimeEdit]);
end;

function TJLabeledDateTimeEdit.GetButtonWidth: integer;
begin
  Result := FButton.Width;
end;

function TJLabeledDateTimeEdit.getFormat: string;
begin
  Result := fFormat;
end;

function TJLabeledDateTimeEdit.getValue: TDateTime;
begin
  Result := theValue;
end;

procedure TJLabeledDateTimeEdit.formatInput;
begin
  if theValue <> 0 then
    Text := FormatDateTime(DisplayFormat, theValue)
  else
    Text := '';
end;

procedure TJLabeledDateTimeEdit.SetButtonWidth(AValue: integer);
begin
  FButton.Width := AValue;
end;

procedure TJLabeledDateTimeEdit.setFormat(const AValue: string);
begin
  fFormat := AValue;
  formatInput;
end;

procedure TJLabeledDateTimeEdit.setValue(const AValue: TDateTime);
begin
  theValue := AValue;
  formatInput;
end;

procedure TJLabeledDateTimeEdit.WMSetFocus(var Message: TLMSetFocus);
begin
  CheckButtonVisible;
  inherited;
end;

procedure TJLabeledDateTimeEdit.WMKillFocus(var Message: TLMKillFocus);
begin
  CheckButtonVisible;
  inherited;
end;

procedure TJLabeledDateTimeEdit.DoEnter;
begin
  inherited DoEnter;
  if ReadOnly then
    exit;
  if theValue <> 0 then
    Text := FormatDateTime(DisplayFormat, theValue)
  else
    Text := '';
  SelectAll;
end;

procedure TJLabeledDateTimeEdit.DoExit;
var
  bufText: string;
begin
  inherited DoExit;
  if ReadOnly then
    exit;
  bufText := Text;
  Text := NormalizeDateTime(Text, theValue);
  if (Length(bufText) > 0) and (Length(Text) = 0) then
  begin
    ShowMessage(Format(SInvalidDateTime, [bufText]));
    SetFocus;
  end
  else
  if Length(Text) = 0 then
    theValue := 0
  else
  if IsValidDateTimeString(Text) then
    theValue := StrToDateTime(Text)
  else
  begin
    ShowMessage(Format(SInvalidDateTime, [Text]));
    SetFocus;
  end;
  formatInput;
end;

procedure TJLabeledDateTimeEdit.KeyDown(var Key: word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if (ssAlt in Shift) and (key = 40) then
    ShowCalendar(Self);
end;

procedure TJLabeledDateTimeEdit.KeyPress(var Key: char);
begin
  if not (Key in ['0'..'9', #8, #9, '.', '-', '/', ',', ':', ' ']) then
    Key := #0;
  inherited KeyPress(Key);
end;

procedure TJLabeledDateTimeEdit.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if FButton <> nil then
  begin
    DoPositionButton;
    CheckButtonVisible;
  end;
end;

procedure TJLabeledDateTimeEdit.DoPositionButton;
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

procedure TJLabeledDateTimeEdit.CheckButtonVisible;
begin
  if Assigned(FButton) then
    FButton.Visible := True;
end;

procedure TJLabeledDateTimeEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FButton) and (Operation = opRemove) then
    FButton := nil;
end;

procedure TJLabeledDateTimeEdit.CMVisibleChanged(var Msg: TLMessage);
begin
  inherited CMVisibleChanged(Msg);
  CheckButtonVisible;
end;

procedure TJLabeledDateTimeEdit.CMEnabledChanged(var Msg: TLMessage);
begin
  inherited CMEnabledChanged(Msg);
  if (FButton <> nil) then
    FButton.Enabled := True;
end;

procedure TJLabeledDateTimeEdit.CMBiDiModeChanged(var Message: TLMessage);
begin
  inherited;
  DoPositionButton;
end;

procedure TJLabeledDateTimeEdit.Loaded;
begin
  inherited Loaded;
  DoPositionButton;
  CheckButtonVisible;
end;

procedure TJLabeledDateTimeEdit.ShowCalendar(Sender: TObject);
var
  PopupOrigin: TPoint;
  ADate: TDateTime;
begin
  PopupOrigin := Self.ControlToScreen(Point(0, Self.Height));
  if isNull then
    ADate := Now
  else
    ADate := Value;
  ShowCalendarPopup(PopupOrigin, ADate, [dsShowHeadings, dsShowDayNames],
    @CalendarPopupReturnDate, nil);
end;

procedure TJLabeledDateTimeEdit.CalendarPopupReturnDate(Sender: TObject;
  const ADate: TDateTime);
var
  bufdate: TDateTime;
begin
  if isNull then
    bufdate := now
  else
    bufdate := Value;
  Value := EncodeDateTime(YearOf(ADate), MonthOf(ADate), DayOf(ADate),
    HourOf(bufdate), MinuteOf(bufdate), SecondOf(bufdate), MilliSecondOf(bufdate));
end;

constructor TJLabeledDateTimeEdit.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Text := '';
  fFormat := ShortDateFormat + ' ' + ShortTimeFormat;
  theValue := 0;
  formatInput;
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

destructor TJLabeledDateTimeEdit.Destroy;
begin
  FreeAndNil(FButton);
  inherited Destroy;
end;

function TJLabeledDateTimeEdit.isNull: boolean;
begin
  Result := theValue = 0;
end;


end.
