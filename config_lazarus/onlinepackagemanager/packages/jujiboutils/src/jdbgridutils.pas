{ jdbgridutils

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

unit jdbgridutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Grids, Dialogs, LCLType, DBGrids, Controls, DB,
  jcontrolutils, jinputconsts, CalendarPopup, Calendar, Buttons;

type

  TJStringCellEditor = class(TStringCellEditor);

  { TJDbGridStringCtrl }

  TJDbGridStringCtrl = class(TObject)
  private
    Field: TField;
    updated: boolean;
    fMaxLength: integer;
    EditingFieldNo: longint;
    EditingRecNo: longint;
    procedure myEditEnter(Sender: TObject);
    procedure myEditOnEditingDone(Sender: TObject);
    procedure OnKeyPress(Sender: TObject; var key: char);
    procedure OnKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
  public
    CellEditor: TStringCellEditor;
    theGrid: TDBGrid;
    constructor Create;
    destructor Destroy; override;
    function Editor(aGrid: TDBGrid; aMaxLength: integer = 0): TStringCellEditor;
    function CanDefocus: boolean;
  end;

  { TJDbGridDateTimeCtrl }

  TJDbGridDateTimeCtrl = class(TObject)
  private
    Field: TField;
    updated: boolean;
    theValue: TDateTime;
    fFormat: string;
    EditingFieldNo: longint;
    EditingRecNo: longint;
    function getFormat: string;
    function EditText: string;
    procedure myEditEnter(Sender: TObject);
    procedure myEditOnEditingDone(Sender: TObject);
    procedure formatInput;
    procedure setFormat(const AValue: string);
    procedure OnKeyPress(Sender: TObject; var key: char);
    procedure OnKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
  protected
    procedure ShowCalendar(Sender: TObject);
    procedure CalendarPopupReturnDate(Sender: TObject; const ADate: TDateTime);
  public
    CellEditor: TStringCellEditor;
    theGrid: TDBGrid;
    function isNull: boolean;
    property DisplayFormat: string read getFormat write setFormat;
    constructor Create;
    destructor Destroy; override;
    function Editor(aGrid: TDBGrid): TStringCellEditor;
    function CanDefocus: boolean;
  end;

  { TJDbGridTimeCtrl }

  TJDbGridTimeCtrl = class(TObject)
  private
    Field: TField;
    updated: boolean;
    theValue: TTime;
    fFormat: string;
    EditingFieldNo: longint;
    EditingRecNo: longint;
    function getFormat: string;
    procedure myEditEnter(Sender: TObject);
    procedure myEditOnEditingDone(Sender: TObject);
    procedure formatInput;
    procedure setFormat(const AValue: string);
    procedure OnKeyPress(Sender: TObject; var key: char);
    procedure OnKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
  public
    CellEditor: TStringCellEditor;
    theGrid: TDBGrid;
    function isNull: boolean;
    property DisplayFormat: string read getFormat write setFormat;
    constructor Create;
    destructor Destroy; override;
    function Editor(aGrid: TDBGrid): TStringCellEditor;
    function CanDefocus: boolean;
  end;

  { TJDbGridDateCtrl }

  TJDbGridDateCtrl = class(TObject)
  private
    Field: TField;
    updated: boolean;
    theValue: TDateTime;
    fFormat: string;
    EditingFieldNo: longint;
    EditingRecNo: longint;
    function getFormat: string;
    procedure myEditEnter(Sender: TObject);
    procedure myEditOnEditingDone(Sender: TObject);
    procedure formatInput;
    procedure setFormat(const AValue: string);
    procedure OnKeyPress(Sender: TObject; var key: char);
    procedure OnKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
  protected
    procedure ShowCalendar(Sender: TObject);
    procedure CalendarPopupReturnDate(Sender: TObject; const ADate: TDateTime);
  public
    CellEditor: TStringCellEditor;
    theGrid: TDBGrid;
    function isNull: boolean;
    property DisplayFormat: string read getFormat write setFormat;
    constructor Create;
    destructor Destroy; override;
    function Editor(aGrid: TDBGrid): TStringCellEditor;
    function CanDefocus: boolean;
  end;

  { TJDbGridIntegerCtrl }

  TJDbGridIntegerCtrl = class(TObject)
  private
    theValue: integer;
    updated: boolean;
    Field: TField;
    EditingFieldNo: longint;
    EditingRecNo: longint;
    procedure myEditOnEnter(Sender: TObject);
    procedure OnKeyPress(Sender: TObject; var key: char);
    procedure OnKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure myEditOnEditingDone(Sender: TObject);
    function IsValidInteger(const Value: string): boolean;
  public
    CellEditor: TStringCellEditor;
    theGrid: TDBGrid;
    constructor Create;
    destructor Destroy; override;
    function Editor(aGrid: TDBGrid): TStringCellEditor;
    function CanDefocus: boolean;
  end;

  { TJDbGridDoubleCtrl }

  TJDbGridDoubleCtrl = class(TObject)
  private
    Field: TField;
    updated: boolean;
    theValue: double;
    fDecimals: integer;
    fEFormat: string;
    EditingFieldNo: longint;
    EditingRecNo: longint;
    function getDecimals: integer;
    procedure myEditOnEnter(Sender: TObject);
    procedure myEditOnEditingDone(Sender: TObject);
    procedure setDecimals(const AValue: integer);
    procedure OnKeyPress(Sender: TObject; var key: char);
    procedure OnKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    function IsValidFloat(const Value: string): boolean;
    function ScaleTo(const AValue: double; const NDecimals: integer): double;
  public
    CellEditor: TStringCellEditor;
    theGrid: TDBGrid;
    property decimals: integer read getDecimals write setDecimals;
    constructor Create;
    destructor Destroy; override;
    function Editor(aGrid: TDBGrid; aDecimals: integer = 2;
      aEFormat: string = ''): TStringCellEditor;
    function CanDefocus: boolean;
  end;


implementation

uses
  Math, dateutils;

{ TJDbGridStringCtrl }

procedure TJDbGridStringCtrl.myEditEnter(Sender: TObject);
begin
  Field := theGrid.SelectedField;
  if not Assigned(Field) then
    abort;
  CellEditor.BoundsRect := theGrid.SelectedFieldRect;
  CellEditor.Text := Field.AsString;
  CellEditor.OnKeyPress := @OnKeyPress;  // Recuperamos el control :-p
  CellEditor.OnKeyDown := @OnKeyDown;
  CellEditor.MaxLength := fMaxLength;
  updated := False;
  CellEditor.SelectAll;
  EditingFieldNo := Field.FieldNo;
  EditingRecNo := Field.DataSet.RecNo;
end;

procedure TJDbGridStringCtrl.myEditOnEditingDone(Sender: TObject);
begin
  if not Assigned(Field) or not Assigned(Field.Dataset) or not Field.DataSet.Active then
    exit;
  if (EditingRecNo <> Field.DataSet.RecNo) or (EditingFieldNo <> Field.FieldNo) then
    exit; // avoid update wrong record/field because dataset scrolling
  if (not updated) then
  begin
    if CellEditor.Text <> Field.AsString then
    begin
      Field.DataSet.DisableControls;
      Field.DataSet.Edit;
      Field.AsString := CellEditor.Text;
      field.DataSet.EnableControls;
      updated := True;
    end;
  end;
end;

procedure TJDbGridStringCtrl.OnKeyPress(Sender: TObject; var key: char);
begin
  // nothing right now
end;

procedure TJDbGridStringCtrl.OnKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if key = VK_ESCAPE then
  begin
    CellEditor.Text := Field.AsString;
    updated := True;
    theGrid.SetFocus; // No perder el foco
  end
  else
  //if Key in [VK_UP, VK_DOWN] then
  //begin
  //  Key := VK_UNKNOWN;
  //end
  //else
  if Key in [VK_RETURN, VK_TAB, VK_UP, VK_DOWN] then
  begin
    Field.DataSet.Edit;
    Field.AsString := CellEditor.Text;
    CellEditor.SelectAll;
    updated := True;
  end;
end;

constructor TJDbGridStringCtrl.Create;
begin
  inherited Create;
  CellEditor := TJStringCellEditor.Create(nil);
  CellEditor.OnEnter := @myEditEnter;
  CellEditor.OnKeyDown := @OnKeyDown;
  CellEditor.OnEditingDone := @myEditOnEditingDone;
  CellEditor.OnKeyPress := @OnKeyPress;
end;

destructor TJDbGridStringCtrl.Destroy;
begin
  CellEditor.Free;
  inherited Destroy;
end;

function TJDbGridStringCtrl.Editor(aGrid: TDBGrid;
  aMaxLength: integer): TStringCellEditor;
begin
  theGrid := aGrid;
  fMaxLength := aMaxLength;
  Result := CellEditor;
end;

function TJDbGridStringCtrl.CanDefocus: boolean;
begin
  Result := True;
  if not CellEditor.Focused then
    exit;
  if Result and Assigned(Field) and Assigned(Field.Dataset) and
    (Field.DataSet.State in dsEditModes) then
    myEditOnEditingDone(nil);
end;

{ TJDbGridDateTimeCtrl }

function TJDbGridDateTimeCtrl.getFormat: string;
begin
  Result := fFormat;
end;

function TJDbGridDateTimeCtrl.EditText: string;
begin
  if Field.IsNull then
    Result := ''
  else
    Result := FormatDateTime(ShortDateFormat, Field.AsDateTime) +
      ' ' + FormatDateTime(ShortTimeFormat, Field.AsDateTime);
end;

procedure TJDbGridDateTimeCtrl.myEditEnter(Sender: TObject);
begin
  Field := theGrid.SelectedField;
  if not Assigned(Field) then
    abort;
  CellEditor.BoundsRect := theGrid.SelectedFieldRect;
  CellEditor.Text := EditText;
  CellEditor.OnKeyPress := @OnKeyPress;  // Recuperamos el control :-p
  CellEditor.OnKeyDown := @OnKeyDown;
  theValue := Field.AsDateTime;
  updated := False;
  CellEditor.SelectAll;
  EditingFieldNo := Field.FieldNo;
  EditingRecNo := Field.DataSet.RecNo;
end;

procedure TJDbGridDateTimeCtrl.myEditOnEditingDone(Sender: TObject);
begin
  if not Assigned(Field) or not Assigned(Field.Dataset) or not Field.DataSet.Active then
    exit;
  if (EditingRecNo <> Field.DataSet.RecNo) or (EditingFieldNo <> Field.FieldNo) then
    exit; // avoid update wrong record/field because dataset scrolling
  if Length(CellEditor.Caption) = 0 then
  begin
    if Field.Value <> Null then
    begin
      Field.DataSet.DisableControls;
      Field.DataSet.Edit;
      Field.Value := Null;
      theValue := 0;
      updated := True;
      Field.DataSet.EnableControls;
    end;
  end
  else
  begin
    CellEditor.Caption := NormalizeDateTime(CellEditor.Caption, theValue);
    if IsValidDateTimeString(CellEditor.Caption) then
    begin
      if (not updated) then
      begin
        theValue := StrToDateTime(CellEditor.Caption);
        if FormatDateTime(DisplayFormat, theValue) <>
          FormatDateTime(DisplayFormat, Field.AsDateTime) then
        begin
          // Field.DataSet.DisableControls;
          Field.DataSet.Edit;
          Field.AsDateTime := theValue;
          Field.DataSet.EnableControls;
          updated := True;
        end;
      end;
    end
    else
    begin
      ShowMessage(Format(SInvalidDateTime, [CellEditor.Caption]));
      if Field.IsNull then
        CellEditor.Text := ''
      else
        CellEditor.Text := FormatDateTime(DisplayFormat, Field.AsDateTime);
    end;

  end;
end;

procedure TJDbGridDateTimeCtrl.formatInput;
begin
  if theValue <> 0 then
    CellEditor.Caption := FormatDateTime(DisplayFormat, theValue);
end;

procedure TJDbGridDateTimeCtrl.setFormat(const AValue: string);
begin
  fFormat := AValue;
  formatInput;
end;

procedure TJDbGridDateTimeCtrl.OnKeyPress(Sender: TObject; var key: char);
begin
  if not (Key in ['0'..'9', #8, #9, '.', '-', '/', ':', ' ']) then
    Key := #0;
end;

procedure TJDbGridDateTimeCtrl.OnKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if (ssAlt in Shift) and (key = 40) then
  begin
    ShowCalendar(Self);
    key := 0;
  end;
  if Length(CellEditor.Caption) = 0 then
  begin
    if Field.Value <> Null then
    begin
      Field.DataSet.Edit;
      Field.Value := Null;
      theValue := 0;
      updated := True;
    end;
  end
  else
  if Length(CellEditor.Caption) <> 0 then
    if (Key in [VK_RETURN, VK_TAB, VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT]) and
      (not IsValidDateTimeString(NormalizeDateTime(CellEditor.Caption, theValue))) then
    begin
      ShowMessage(Format(SInvalidDateTime, [CellEditor.Caption]));
      if Field.IsNull then
        CellEditor.Text := ''
      else
        CellEditor.Text := FormatDateTime(DisplayFormat, theValue);
      CellEditor.SelectAll;
      Key := VK_UNKNOWN;
    end
    else
    if key = VK_ESCAPE then
    begin
      if Field.IsNull then
        CellEditor.Text := ''
      else
        CellEditor.Text := FormatDateTime(DisplayFormat, Field.AsDateTime);
      updated := True;
      theGrid.SetFocus; // No perder el foco
    end
    else
    //if Key in [VK_UP, VK_DOWN] then
    //begin
    //  Key := VK_UNKNOWN;
    //end
    //else
    if Key in [VK_RETURN, VK_TAB, VK_UP, VK_DOWN] then
    begin
      CellEditor.Caption := NormalizeDateTime(CellEditor.Caption, theValue);
      if Length(CellEditor.Caption) = 0 then
        theValue := 0
      else
      if IsValidDateTimeString(CellEditor.Caption) then
      begin
        theValue := StrToDateTime(CellEditor.Caption);
        Field.DataSet.Edit;
        Field.AsDateTime := theValue;
        CellEditor.SelectAll;
        updated := True;
      end;
    end;
end;

procedure TJDbGridDateTimeCtrl.ShowCalendar(Sender: TObject);
var
  PopupOrigin: TPoint;
  ADate: TDateTime;
begin
  if (not Assigned(Field)) then
    exit;
  PopupOrigin := CellEditor.ControlToScreen(Point(0, CellEditor.Height));
  if Field.IsNull then
    ADate := now
  else
    ADate := Field.AsDateTime;
  ShowCalendarPopup(PopupOrigin, ADate, [dsShowHeadings, dsShowDayNames],
    @CalendarPopupReturnDate, nil);
end;

procedure TJDbGridDateTimeCtrl.CalendarPopupReturnDate(Sender: TObject;
  const ADate: TDateTime);
var
  bufdate: TDateTime;
begin
  if not (Field.DataSet.State in [dsEdit, dsInsert]) then
    Field.DataSet.Edit;
  if Field.IsNull then
    bufdate := now
  else
    bufdate := Field.AsDateTime;
  Field.AsDateTime :=
    EncodeDateTime(YearOf(ADate), MonthOf(ADate), DayOf(ADate),
    HourOf(bufdate), MinuteOf(bufdate), SecondOf(bufdate), MilliSecondOf(bufdate));
  CellEditor.Text := EditText;
end;

function TJDbGridDateTimeCtrl.isNull: boolean;
begin
  Result := theValue = 0;
end;

constructor TJDbGridDateTimeCtrl.Create;
begin
  inherited Create;
  CellEditor := TJStringCellEditor.Create(nil);
  CellEditor.OnEnter := @myEditEnter;
  CellEditor.OnKeyDown := @OnKeyDown;
  CellEditor.OnEditingDone := @myEditOnEditingDone;
  CellEditor.OnKeyPress := @OnKeyPress;
  DisplayFormat := ShortDateFormat + ' ' + ShortTimeFormat;
end;

destructor TJDbGridDateTimeCtrl.Destroy;
begin
  CellEditor.Free;
  inherited Destroy;
end;

function TJDbGridDateTimeCtrl.Editor(aGrid: TDBGrid): TStringCellEditor;
begin
  theGrid := aGrid;
  Result := CellEditor;
end;

function TJDbGridDateTimeCtrl.CanDefocus: boolean;
begin
  Result := True;
  if not CellEditor.Focused then
    exit;
  if (Length(CellEditor.Text) = 0) then
    exit;
  Result := IsValidDateTimeString(NormalizeDateTime(CellEditor.Caption, theValue));
  if not Result then
  begin
    ShowMessage(Format(SInvalidDateTime, [CellEditor.Text]));
    CellEditor.Text := EditText;
  end;
  if Result and Assigned(Field) and Assigned(Field.Dataset) and
    (Field.DataSet.State in dsEditModes) then
    myEditOnEditingDone(nil);
end;

{ TJDbGridTimeCtrl }

function TJDbGridTimeCtrl.getFormat: string;
begin
  Result := fFormat;
end;

procedure TJDbGridTimeCtrl.myEditEnter(Sender: TObject);
begin
  Field := theGrid.SelectedField;
  if not Assigned(Field) then
    abort;
  CellEditor.BoundsRect := theGrid.SelectedFieldRect;
  CellEditor.Text := Field.AsString;
  CellEditor.OnKeyPress := @OnKeyPress;  // Recuperamos el control :-p
  CellEditor.OnKeyDown := @OnKeyDown;
  theValue := Field.AsDateTime;
  updated := False;
  CellEditor.SelectAll;
  EditingFieldNo := Field.FieldNo;
  EditingRecNo := Field.DataSet.RecNo;
end;

procedure TJDbGridTimeCtrl.myEditOnEditingDone(Sender: TObject);
begin
  if not Assigned(Field) or not Assigned(Field.Dataset) or not Field.DataSet.Active then
    exit;
  if (EditingRecNo <> Field.DataSet.RecNo) or (EditingFieldNo <> Field.FieldNo) then
    exit; // avoid update wrong record/field because dataset scrolling
  if Length(CellEditor.Caption) = 0 then
  begin
    if Field.Value <> Null then
    begin
      Field.DataSet.DisableControls;
      Field.DataSet.Edit;
      Field.Value := Null;
      theValue := 0;
      updated := True;
      Field.DataSet.EnableControls;
    end;
  end
  else
  begin
    CellEditor.Caption := NormalizeTime(CellEditor.Caption, theValue);
    if IsValidTimeString(CellEditor.Caption) then
    begin
      if (not updated) then
      begin
        theValue := StrToTime(CellEditor.Caption);
        if FormatDateTime(DisplayFormat, theValue) <>
          FormatDateTime(DisplayFormat, Field.AsDateTime) then
        begin
          Field.DataSet.DisableControls;
          Field.DataSet.Edit;
          Field.Text := NormalizeTime(CellEditor.Caption, Field.AsDateTime);
          Field.DataSet.EnableControls;
          updated := True;
        end;
      end;
    end
    else
    begin
      ShowMessage(Format(SInvalidTime, [CellEditor.Caption]));
      if Field.IsNull then
        CellEditor.Text := ''
      else
        CellEditor.Text := FormatDateTime(DisplayFormat, Field.AsDateTime);
    end;
  end;
end;

procedure TJDbGridTimeCtrl.formatInput;
begin
  if theValue <> 0 then
    CellEditor.Caption := FormatDateTime(DisplayFormat, theValue);
end;

procedure TJDbGridTimeCtrl.setFormat(const AValue: string);
begin
  fFormat := AValue;
  formatInput;
end;

procedure TJDbGridTimeCtrl.OnKeyPress(Sender: TObject; var key: char);
begin
  if not (Key in ['0'..'9', #8, #9, ':']) then
    Key := #0;
end;

procedure TJDbGridTimeCtrl.OnKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if Length(CellEditor.Caption) = 0 then
  begin
    if Field.Value <> Null then
    begin
      Field.DataSet.Edit;
      Field.Value := Null;
      theValue := 0;
      updated := True;
    end;
  end
  else
  if Length(CellEditor.Caption) <> 0 then
    if (Key in [VK_RETURN, VK_TAB, VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT]) and
      (not IsValidTimeString(NormalizeTime(CellEditor.Caption, theValue))) then
    begin
      ShowMessage(Format(SInvalidTime, [CellEditor.Caption]));
      if Field.IsNull then
        CellEditor.Text := ''
      else
        CellEditor.Text := FormatDateTime(DisplayFormat, theValue);
      CellEditor.SelectAll;
      Key := VK_UNKNOWN;
    end
    else
    if key = VK_ESCAPE then
    begin
      if Field.IsNull then
        CellEditor.Text := ''
      else
        CellEditor.Text := FormatDateTime(DisplayFormat, Field.AsDateTime);
      updated := True;
      theGrid.SetFocus; // No perder el foco
    end
    else
    //if Key in [VK_UP, VK_DOWN] then
    //begin
    //  Key := VK_UNKNOWN;
    //end
    //else
    if Key in [VK_RETURN, VK_TAB, VK_UP, VK_DOWN] then
    begin
      CellEditor.Caption := NormalizeTime(CellEditor.Caption, theValue);
      if Length(CellEditor.Caption) = 0 then
        theValue := 0
      else
      if IsValidTimeString(CellEditor.Caption) then
      begin
        theValue := StrToTime(CellEditor.Caption);
        Field.DataSet.Edit;
        Field.Text := NormalizeTime(CellEditor.Caption, Field.AsDateTime);
        CellEditor.SelectAll;
        updated := True;
      end;
    end;
end;

function TJDbGridTimeCtrl.isNull: boolean;
begin
  Result := theValue = 0;
end;

constructor TJDbGridTimeCtrl.Create;
begin
  inherited Create;
  CellEditor := TJStringCellEditor.Create(nil);
  CellEditor.OnEnter := @myEditEnter;
  CellEditor.OnKeyDown := @OnKeyDown;
  CellEditor.OnEditingDone := @myEditOnEditingDone;
  CellEditor.OnKeyPress := @OnKeyPress;   // se sobreescribe por el Grid :(
  DisplayFormat := ShortTimeFormat;
end;

destructor TJDbGridTimeCtrl.Destroy;
begin
  CellEditor.Free;
  inherited Destroy;
end;

function TJDbGridTimeCtrl.Editor(aGrid: TDBGrid): TStringCellEditor;
begin
  theGrid := aGrid;
  Result := CellEditor;
end;

function TJDbGridTimeCtrl.CanDefocus: boolean;
begin
  Result := True;
  if not CellEditor.Focused then
    exit;
  if (Length(CellEditor.Text) = 0) then
    exit;
  Result := IsValidDateTimeString(NormalizeTime(CellEditor.Caption, theValue));
  if not Result then
  begin
    ShowMessage(Format(SInvalidTime, [CellEditor.Text]));
    CellEditor.Text := Field.AsString;
  end;
  if Result and Assigned(Field) and Assigned(Field.Dataset) and
    (Field.DataSet.State in dsEditModes) then
    myEditOnEditingDone(nil);
end;

{ TJDbGridDateCtrl }

function TJDbGridDateCtrl.getFormat: string;
begin
  Result := fFormat;
end;

procedure TJDbGridDateCtrl.myEditEnter(Sender: TObject);
begin
  Field := theGrid.SelectedField;
  if not Assigned(Field) then
    abort;
  CellEditor.BoundsRect := theGrid.SelectedFieldRect;
  CellEditor.Text := Field.AsString;
  CellEditor.OnKeyPress := @OnKeyPress;  // Recuperamos el control :-p
  CellEditor.OnKeyDown := @OnKeyDown;
  theValue := Field.AsDateTime;
  updated := False;
  CellEditor.SelectAll;
  EditingFieldNo := Field.FieldNo;
  EditingRecNo := Field.DataSet.RecNo;
end;

procedure TJDbGridDateCtrl.myEditOnEditingDone(Sender: TObject);
begin
  if not Assigned(Field) or not Assigned(Field.Dataset) or not Field.DataSet.Active then
    exit;
  if (EditingRecNo <> Field.DataSet.RecNo) or (EditingFieldNo <> Field.FieldNo) then
    exit; // avoid update wrong record/field because dataset scrolling
  if Length(CellEditor.Caption) = 0 then
  begin
    if Field.Value <> Null then
    begin
      Field.DataSet.DisableControls;
      Field.DataSet.Edit;
      Field.Value := Null;
      theValue := 0;
      updated := True;
      Field.DataSet.EnableControls;
    end;
  end
  else
  begin
    CellEditor.Caption := NormalizeDate(CellEditor.Caption, theValue);
    if IsValidDateString(CellEditor.Caption) then
    begin
      if (not updated) then
      begin
        theValue := StrToDate(CellEditor.Caption);
        if FormatDateTime(DisplayFormat, theValue) <>
          FormatDateTime(DisplayFormat, Field.AsDateTime) then
        begin
          Field.DataSet.DisableControls;
          Field.DataSet.Edit;
          Field.AsDateTime := theValue;
          field.DataSet.EnableControls;
          updated := True;
        end;
      end;
    end
    else
    begin
      ShowMessage(Format(SInvalidDate, [CellEditor.Caption]));
      if Field.IsNull then
        CellEditor.Text := ''
      else
        CellEditor.Text := FormatDateTime(DisplayFormat, Field.AsDateTime);
    end;
  end;
end;

procedure TJDbGridDateCtrl.formatInput;
begin
  if theValue <> 0 then
    CellEditor.Caption := FormatDateTime(DisplayFormat, theValue);
end;

procedure TJDbGridDateCtrl.setFormat(const AValue: string);
begin
  fFormat := AValue;
  formatInput;
end;

procedure TJDbGridDateCtrl.OnKeyPress(Sender: TObject; var key: char);
begin
  if not (Key in ['0'..'9', #8, #9, '.', '-', '/']) then
    Key := #0;
end;

procedure TJDbGridDateCtrl.OnKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if (ssAlt in Shift) and (key = 40) then
  begin
    ShowCalendar(Self);
    key := 0;
  end;
  if Length(CellEditor.Caption) = 0 then
  begin
    if Field.Value <> null then
    begin
      Field.DataSet.Edit;
      Field.Value := Null;
      theValue := 0;
      updated := True;
    end;
  end
  else
  if Length(CellEditor.Caption) <> 0 then
    if (Key in [VK_RETURN, VK_TAB, VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT]) and
      (not IsValidDateString(NormalizeDate(CellEditor.Caption, theValue))) then
    begin
      ShowMessage(Format(SInvalidDate, [CellEditor.Caption]));
      if Field.IsNull then
        CellEditor.Text := ''
      else
        CellEditor.Text := FormatDateTime(DisplayFormat, theValue);
      CellEditor.SelectAll;
      Key := VK_UNKNOWN;
    end
    else
    if key = VK_ESCAPE then
    begin
      if Field.IsNull then
        CellEditor.Text := ''
      else
        CellEditor.Text := FormatDateTime(DisplayFormat, Field.AsDateTime);
      updated := True;
      theGrid.SetFocus; // No perder el foco
    end
    else
    //if Key in [VK_UP, VK_DOWN] then
    //begin
    //  Key := VK_UNKNOWN;
    //end
    //else
    if Key in [VK_RETURN, VK_TAB, VK_UP, VK_DOWN] then
    begin
      CellEditor.Caption := NormalizeDate(CellEditor.Caption, theValue);
      if Length(CellEditor.Caption) = 0 then
        theValue := 0
      else
      if IsValidDateString(CellEditor.Caption) then
      begin
        theValue := StrToDate(CellEditor.Caption);
        Field.DataSet.Edit;
        Field.AsDateTime := theValue;
        CellEditor.SelectAll;
        updated := True;
      end;
    end;
end;

procedure TJDbGridDateCtrl.ShowCalendar(Sender: TObject);
var
  PopupOrigin: TPoint;
  ADate: TDateTime;
begin
  if (not Assigned(Field)) then
    exit;
  PopupOrigin := CellEditor.ControlToScreen(Point(0, CellEditor.Height));
  if Field.IsNull then
    ADate := now
  else
    ADate := Field.AsDateTime;
  ShowCalendarPopup(PopupOrigin, ADate, [dsShowHeadings, dsShowDayNames],
    @CalendarPopupReturnDate, nil);
end;

procedure TJDbGridDateCtrl.CalendarPopupReturnDate(Sender: TObject;
  const ADate: TDateTime);
var
  bufdate: TDateTime;
begin
  if not (Field.DataSet.State in [dsEdit, dsInsert]) then
    Field.DataSet.Edit;
  if Field.IsNull then
    bufdate := now
  else
    bufdate := Field.AsDateTime;
  Field.AsDateTime :=
    EncodeDateTime(YearOf(ADate), MonthOf(ADate), DayOf(ADate),
    HourOf(bufdate), MinuteOf(bufdate), SecondOf(bufdate), MilliSecondOf(bufdate));
  CellEditor.Text := Field.AsString;
end;


function TJDbGridDateCtrl.isNull: boolean;
begin
  Result := theValue = 0;
end;

constructor TJDbGridDateCtrl.Create;
begin
  inherited Create;
  CellEditor := TJStringCellEditor.Create(nil);
  CellEditor.OnEnter := @myEditEnter;
  CellEditor.OnKeyDown := @OnKeyDown;
  CellEditor.OnEditingDone := @myEditOnEditingDone;
  CellEditor.OnKeyPress := @OnKeyPress;   // se sobreescribe por el Grid :(
  DisplayFormat := ShortDateFormat;
end;

destructor TJDbGridDateCtrl.Destroy;
begin
  CellEditor.Free;
  inherited Destroy;
end;

function TJDbGridDateCtrl.Editor(aGrid: TDBGrid): TStringCellEditor;
begin
  theGrid := aGrid;
  Result := CellEditor;
end;

function TJDbGridDateCtrl.CanDefocus: boolean;
begin
  Result := True;
  if not CellEditor.Focused then
    exit;
  if (Length(CellEditor.Text) = 0) then
    exit;
  Result := IsValidDateTimeString(NormalizeDateTime(CellEditor.Caption, theValue));
  if not Result then
  begin
    ShowMessage(Format(SInvalidDate, [CellEditor.Text]));
    CellEditor.Text := Field.AsString;
  end;
  if Result and Assigned(Field) and Assigned(Field.Dataset) and
    (Field.DataSet.State in dsEditModes) then
    myEditOnEditingDone(nil);
end;

{ TJDbGridDoubleCtrl }

function TJDbGridDoubleCtrl.getDecimals: integer;
begin
  Result := fDecimals;
end;

procedure TJDbGridDoubleCtrl.myEditOnEnter(Sender: TObject);
begin
  Field := theGrid.SelectedField;
  if not Assigned(Field) then
    Abort;
  CellEditor.BoundsRect := theGrid.SelectedFieldRect;
  if Length(fEFormat) > 0 then
    CellEditor.Text := FormatFloat(fEFormat, Field.AsFloat)
  else
    CellEditor.Text := Field.AsString;
  CellEditor.OnKeyPress := @OnKeyPress;  // Recuperamos el control :-p
  CellEditor.OnKeyDown := @OnKeyDown;
  theValue := Field.AsFloat;
  updated := False;
  CellEditor.SelectAll;
  EditingFieldNo := Field.FieldNo;
  EditingRecNo := Field.DataSet.RecNo;
end;

procedure TJDbGridDoubleCtrl.myEditOnEditingDone(Sender: TObject);
begin
  if not Assigned(Field) or not Assigned(Field.Dataset) or not Field.DataSet.Active then
    exit;
  if (EditingRecNo <> Field.DataSet.RecNo) or (EditingFieldNo <> Field.FieldNo) then
    exit; // avoid update wrong record/field because dataset scrolling
  if IsValidFloat(CellEditor.Caption) then
  begin
    if (not updated) then
    begin
      theValue := StrToFloat(CellEditor.Caption);
      if theValue <> Field.AsFloat then
      begin
        Field.DataSet.DisableControls;
        Field.DataSet.Edit;
        if decimals > 0 then
          theValue := ScaleTo(theValue, fDecimals);
        Field.Value := theValue;
        Field.DataSet.EnableControls;
        updated := True;
      end;
    end;
  end
  else
  begin
    ShowMessage(Format(SInvalidNumber, [CellEditor.Caption]));
    CellEditor.Text := FloatToStr(theValue);
  end;
end;

procedure TJDbGridDoubleCtrl.setDecimals(const AValue: integer);
begin
  if (AValue >= 0) and (AValue < 11) then
    fDecimals := AValue;
end;

procedure TJDbGridDoubleCtrl.OnKeyPress(Sender: TObject; var key: char);
begin
  if (Key in ['.', ',']) then
    Key := Decimalseparator;
  if (key = DecimalSeparator) and (Pos(key, CellEditor.Caption) > 0) then
    key := #0;
  if not (Key in ['0'..'9', DecimalSeparator, '+', '-', #8, #9]) then
    Key := #0;
  //if (Key = DecimalSeparator) and (fDecimals = 0) then
  //  Key := #0;    // Note: decimal=0 avoids rounding
end;

procedure TJDbGridDoubleCtrl.OnKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if (Key in [VK_RETURN, VK_TAB, VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT]) and
    (not IsValidFloat(CellEditor.Caption)) then
  begin
    ShowMessage(Format(SInvalidNumber, [CellEditor.Caption]));
    CellEditor.Text := FloatToStr(theValue);
    CellEditor.SelectAll;
    Key := VK_UNKNOWN;
  end
  else
  if key = VK_ESCAPE then
  begin
    if Field.IsNull then
      CellEditor.Text := ''
    else
      CellEditor.Text := FloatToStr(Field.AsFloat);
    updated := True;
    theGrid.SetFocus; // No perder el foco
  end
  else
  //if key in [VK_UP, VK_DOWN] then
  //begin
  //  Key := VK_UNKNOWN;
  //end
  //else
  if Key in [VK_RETURN, VK_TAB, VK_UP, VK_DOWN] then
  begin
    if IsValidFloat(CellEditor.Caption) then
    begin
      theValue := StrToFloat(CellEditor.Caption);
      Field.DataSet.Edit;
      if decimals > 0 then
        theValue := ScaleTo(theValue, fDecimals);
      Field.Value := theValue;
      CellEditor.Text := Field.AsString;
      updated := True;
    end;
  end;
end;

function TJDbGridDoubleCtrl.IsValidFloat(const Value: string): boolean;
begin
  if StrToFloatDef(Value, MaxDouble) = MaxDouble then
    Result := False
  else
    Result := True;
end;

function TJDbGridDoubleCtrl.ScaleTo(const AValue: double;
  const NDecimals: integer): double;
begin
  // rounding halfup
  if AValue > 0 then
    Result := trunc(AValue * power(10, NDecimals) + 0.5) / power(10, NDecimals)
  else
    Result := trunc(AValue * power(10, NDecimals) - 0.5) / power(10, NDecimals);
end;

constructor TJDbGridDoubleCtrl.Create;
begin
  inherited Create;
  CellEditor := TJStringCellEditor.Create(nil);
  CellEditor.OnEnter := @myEditOnEnter;
  CellEditor.OnKeyDown := @OnKeyDown;
  CellEditor.OnEditingDone := @myEditOnEditingDone;
  fDecimals := 2;
end;

destructor TJDbGridDoubleCtrl.Destroy;
begin
  CellEditor.Free;
  inherited Destroy;
end;

function TJDbGridDoubleCtrl.Editor(aGrid: TDBGrid; aDecimals: integer;
  aEFormat: string): TStringCellEditor;
begin
  decimals := aDecimals;
  fEFormat := aEFormat;
  theGrid := aGrid;
  Result := CellEditor;
end;

function TJDbGridDoubleCtrl.CanDefocus: boolean;
begin
  Result := True;
  if not CellEditor.Focused then
    exit;
  Result := IsValidFloat(CellEditor.Text);
  if not Result then
  begin
    ShowMessage(Format(SInvalidNumber, [CellEditor.Text]));
    if Length(fEFormat) > 0 then
      CellEditor.Text := FormatFloat(fEFormat, Field.AsFloat)
    else
      CellEditor.Text := Field.AsString;
  end;
  if Result and Assigned(Field) and Assigned(Field.Dataset) and
    (Field.DataSet.State in dsEditModes) then
    myEditOnEditingDone(nil);
end;

{ TJDbGridIntegerCtrl }

procedure TJDbGridIntegerCtrl.myEditOnEnter(Sender: TObject);
begin
  Field := theGrid.SelectedField;
  CellEditor.BoundsRect := theGrid.SelectedFieldRect;
  CellEditor.Text := Field.AsString;
  CellEditor.OnKeyPress := @OnKeyPress;  // Recuperamos el control :-p
  CellEditor.OnKeyDown := @OnKeyDown;
  theValue := Field.AsInteger;
  CellEditor.SelectAll;
  updated := False;
  EditingFieldNo := Field.FieldNo;
  EditingRecNo := Field.DataSet.RecNo;
end;

procedure TJDbGridIntegerCtrl.OnKeyPress(Sender: TObject; var key: char);
begin
  if not (Key in ['0'..'9', #8, #9, '-']) then
    Key := #0;
end;

procedure TJDbGridIntegerCtrl.OnKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if (Key in [VK_RETURN, VK_TAB, VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT]) and
    (not IsValidInteger(CellEditor.Caption)) then
  begin
    ShowMessage(Format(SInvalidNumber, [CellEditor.Caption]));
    CellEditor.Text := IntToStr(theValue);
    CellEditor.SelectAll;
    Key := VK_UNKNOWN;
  end
  else
  if (Key = VK_ESCAPE) then
  begin
    if Field.IsNull then
      CellEditor.Text := ''
    else
      CellEditor.Text := IntToStr(Field.AsInteger);
    updated := True;
    theGrid.SetFocus; // No perder el foco
  end
  else
  //if key in [VK_UP, VK_DOWN] then
  //begin
  //  Key := VK_UNKNOWN;
  //end
  //else
  if Key in [VK_RETURN, VK_TAB, VK_UP, VK_DOWN] then
  begin
    if IsValidInteger(CellEditor.Caption) then
    begin
      theValue := StrToInt(CellEditor.Caption);
      Field.DataSet.Edit;
      Field.AsInteger := theValue;
      updated := True;
    end;
  end;
end;

procedure TJDbGridIntegerCtrl.myEditOnEditingDone(Sender: TObject);
begin
  if not Assigned(Field) or not Assigned(Field.Dataset) or not Field.DataSet.Active then
    exit;
  if (EditingRecNo <> Field.DataSet.RecNo) or (EditingFieldNo <> Field.FieldNo) then
    exit; // avoid update wrong record/field because dataset scrolling
  if IsValidInteger(CellEditor.Caption) then
  begin
    if (not updated) then
    begin
      theValue := StrToInt(CellEditor.Caption);
      if theValue <> Field.AsInteger then
      begin
        Field.DataSet.DisableControls;
        Field.DataSet.Edit;
        Field.AsInteger := theValue;
        field.DataSet.EnableControls;
        updated := True;
      end;
    end;
  end
  else
  begin
    ShowMessage(Format(SInvalidNumber, [CellEditor.Caption]));
    CellEditor.Text := IntToStr(theValue);
  end;
end;

function TJDbGridIntegerCtrl.IsValidInteger(const Value: string): boolean;
begin
  if StrToIntDef(Value, MaxInt) = MaxInt then
    Result := False
  else
    Result := True;
end;

constructor TJDbGridIntegerCtrl.Create;
begin
  inherited Create;
  CellEditor := TJStringCellEditor.Create(nil);
  CellEditor.OnEnter := @myEditOnEnter;
  CellEditor.OnKeyDown := @OnKeyDown;
  CellEditor.OnEditingDone := @myEditOnEditingDone;
  CellEditor.OnKeyPress := @OnKeyPress;   // se sobreescribe por el Grid :(
end;

destructor TJDbGridIntegerCtrl.Destroy;
begin
  CellEditor.Free;
  inherited Destroy;
end;

function TJDbGridIntegerCtrl.Editor(aGrid: TDBGrid): TStringCellEditor;
begin
  theGrid := aGrid;
  Result := CellEditor;
end;

function TJDbGridIntegerCtrl.CanDefocus: boolean;
begin
  Result := True;
  if not CellEditor.Focused then
    exit;
  Result := IsValidInteger(CellEditor.Text);
  if not Result then
  begin
    ShowMessage(Format(SInvalidNumber, [CellEditor.Text]));
    CellEditor.Text := Field.AsString;
  end;
  if Result and Assigned(Field) and Assigned(Field.Dataset) and
    (Field.DataSet.State in dsEditModes) then
    myEditOnEditingDone(nil);
end;

end.
