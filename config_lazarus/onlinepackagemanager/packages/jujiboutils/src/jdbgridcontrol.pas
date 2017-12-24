{ JDBGridControl

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

unit JDBGridControl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, DB, Grids, DBGrids,
  Dialogs, LCLType, LMessages, jdbgridutils;

type

  { TJDBColumn }

  TJDBColumn = class(TColumn)
  private
    fDecimals: integer;
    fMaxLength: integer;
    fEFormat: string;
    function getDecimals: integer;
    function getMaxLength: integer;
    procedure setDecimals(AValue: integer);
    procedure setMaxLength(AValue: integer);
  published
    property Decimals: integer read getDecimals write setDecimals;
    property MaxLength: integer read getMaxLength write setMaxLength;
    property EditFormat: string read fEFormat write fEFormat;
  end;

  { TJDBGridColumns }

  TJDBGridColumns = class(TDBGridColumns)
  private
    function GetColumn(Index: integer): TJDBColumn;
    procedure SetColumn(Index: integer; AValue: TJDBColumn);
  public
    function add: TJDBColumn;
    property Items[Index: integer]: TJDBColumn read GetColumn write SetColumn; default;
  published

  end;

  { TJDBGridControl }

  TJDBGridControl = class(TDBGrid)
  private
    { Private declarations }
    stringDbGridControl: TJDbGridStringCtrl;
    dateDbGridControl: TJDbGridDateCtrl;
    timeDbGridControl: TJDbGridTimeCtrl;
    integerDbGridControl: TJDbGridIntegerCtrl;
    doubleDbGridControl: TJDbGridDoubleCtrl;
    dateTimeDbGridControl: TJDbGridDateTimeCtrl;
    function GetColumns: TJDBGridColumns;
    procedure SetColumns(AValue: TJDBGridColumns);
  protected
    { Protected declarations }
    function CreateColumns: TGridColumns; override;
    function GetDefaultEditor(Column: integer): TWinControl; override;
    procedure UpdateData; override;
    property Columns: TJDBGridColumns read GetColumns write SetColumns;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
      override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): boolean; override;
    procedure WMVScroll(var Message: TLMVScroll); message LM_VScroll;
    procedure KeyDown(var Key: word; Shift: TShiftState); override;
  public
    { Public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }

  end;

procedure Register;

implementation

procedure Register;
begin
  {$I jdbgridcontrol_icon.lrs}
  RegisterComponents('JujiboDB', [TJDBGridControl]);
end;

function TJDBColumn.getDecimals: integer;
begin
  Result := fDecimals;
end;

function TJDBColumn.getMaxLength: integer;
begin
  Result := fMaxLength;
end;

procedure TJDBColumn.setDecimals(AValue: integer);
begin
  if (AValue >= 0) and (AValue <= 10) then
    fDecimals := AValue;
end;

procedure TJDBColumn.setMaxLength(AValue: integer);
begin
  if AValue >= 0 then
    fMaxLength := AValue;
end;

{ TJDBGridColumns }

function TJDBGridColumns.GetColumn(Index: integer): TJDBColumn;
begin
  Result := TJDBColumn(inherited Items[Index]);
end;

procedure TJDBGridColumns.SetColumn(Index: integer; AValue: TJDBColumn);
begin
  Items[Index].Assign(AValue);
end;

function TJDBGridColumns.add: TJDBColumn;
begin
  Result := TJDBColumn(inherited add);
end;

{ TJDBGridControl }

function TJDBGridControl.GetColumns: TJDBGridColumns;
begin
  Result := TJDBGridColumns(inherited Columns);
end;

procedure TJDBGridControl.SetColumns(AValue: TJDBGridColumns);
begin
  inherited Columns := TDBGridColumns(AValue);
end;

function TJDBGridControl.CreateColumns: TGridColumns;
begin
  Result := TJDBGridColumns.Create(Self, TJDBColumn);
end;

function TJDBGridControl.GetDefaultEditor(Column: integer): TWinControl;
var
  C: TGridColumn;
  bs: TColumnButtonStyle;
  aField: TField;
begin
  Result := inherited GetDefaultEditor(Column);
  if Result <> nil then
  begin
    C := ColumnFromGridColumn(Column);
    if C <> nil then
    begin
      bs := C.ButtonStyle;
      aField := GetFieldFromGridColumn(Column);
      if (aField <> nil) and (bs = cbsAuto) then
      begin
        case aField.DataType of
          ftSmallint, ftInteger:
            Result := integerDbGridControl.Editor(Self);
          ftDate:
            Result := dateDbGridControl.Editor(Self);
          ftTime:
            Result := timeDbGridControl.Editor(Self);
          ftDateTime:
            Result := dateTimeDbGridControl.Editor(Self);
          ftCurrency, ftFloat:
            Result :=
              doubleDbGridControl.Editor(Self, Columns[Column - 1].Decimals,
              Columns[Column - 1].EditFormat);
          ftBCD, ftFMTBCD:
            Result := doubleDbGridControl.Editor(Self, aField.Size,
              Columns[Column - 1].EditFormat);
          ftString:
            Result := stringDbGridControl.Editor(Self, Columns[Column - 1].MaxLength);
        end;
      end;
    end;
  end;
end;

procedure TJDBGridControl.UpdateData;
begin
  if not (Editor is TJStringCellEditor) then
    inherited UpdateData;
end;

procedure TJDBGridControl.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  if not (integerDbGridControl.CanDefocus and doubleDbGridControl.CanDefocus and
    dateTimeDbGridControl.CanDefocus and stringDbGridControl.CanDefocus and
    dateDbGridControl.CanDefocus and timeDbGridControl.CanDefocus) then

    abort
  else
    inherited MouseDown(Button, Shift, X, Y);
end;

function TJDBGridControl.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): boolean;
begin
  if not (integerDbGridControl.CanDefocus and doubleDbGridControl.CanDefocus and
    dateTimeDbGridControl.CanDefocus and stringDbGridControl.CanDefocus and
    dateDbGridControl.CanDefocus and timeDbGridControl.CanDefocus) then
    abort
  else
    Result := inherited DoMouseWheelDown(Shift, MousePos);
end;

function TJDBGridControl.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): boolean;
begin
  if not (integerDbGridControl.CanDefocus and doubleDbGridControl.CanDefocus and
    dateTimeDbGridControl.CanDefocus and stringDbGridControl.CanDefocus and
    dateDbGridControl.CanDefocus and timeDbGridControl.CanDefocus) then
    abort
  else
    Result := inherited DoMouseWheelUp(Shift, MousePos);
end;

procedure TJDBGridControl.WMVScroll(var Message: TLMVScroll);
begin
  if not (integerDbGridControl.CanDefocus and doubleDbGridControl.CanDefocus and
    dateTimeDbGridControl.CanDefocus and stringDbGridControl.CanDefocus and
    dateDbGridControl.CanDefocus and timeDbGridControl.CanDefocus) then
    abort
  else
    inherited WMVScroll(Message);
end;

procedure TJDBGridControl.KeyDown(var Key: word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
end;

constructor TJDBGridControl.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  stringDbGridControl := TJDbGridStringCtrl.Create;
  dateDbGridControl := TJDbGridDateCtrl.Create;
  timeDbGridControl := TJDbGridTimeCtrl.Create;
  integerDbGridControl := TJDbGridIntegerCtrl.Create;
  doubleDbGridControl := TJDbGridDoubleCtrl.Create;
  dateTimeDbGridControl := TJDbGridDateTimeCtrl.Create;
end;

destructor TJDBGridControl.Destroy;
begin
  stringDbGridControl.Free;
  dateDbGridControl.Free;
  timeDbGridControl.Free;
  integerDbGridControl.Free;
  doubleDbGridControl.Free;
  dateTimeDbGridControl.Free;
  inherited Destroy;
end;

end.
