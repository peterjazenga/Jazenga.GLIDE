{ JLabeledCurrencyEdit

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

unit JLabeledCurrencyEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, ExtCtrls, Graphics,
  Dialogs, jinputconsts;

type

  { TJLabeledCurrencyEdit }

  TJLabeledCurrencyEdit = class(TCustomLabeledEdit)
  private
    theValue: currency;
    fFormat: string;
    fDecimals: integer;
    function getDecimals: integer;
    function getFormat: string;
    function getValue: currency;
    procedure formatInput;
    procedure setDecimals(const AValue: integer);
    procedure setFormat(const AValue: string);
    function scaleTo(const AValue: currency; const NDecimals: integer): currency;
    function IsValidFloat(const Value: string): boolean;
    procedure setValue(const AValue: currency);
  protected
    { Protected declarations }
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure KeyPress(var Key: char); override;
  public
    { Public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property DisplayFormat: string read getFormat write setFormat;
    property Decimals: integer read getDecimals write setDecimals;
    property Value: currency read getValue write setValue;

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
    property ReadOnly;
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
  Math;

procedure Register;
begin
  {$I jlabeledcurrencyedit_icon.lrs}
  RegisterComponents('Jujibo', [TJLabeledCurrencyEdit]);
end;

{ TJLabeledCurrencyEdit }

function TJLabeledCurrencyEdit.getDecimals: integer;
begin
  Result := fDecimals;
end;

function TJLabeledCurrencyEdit.getFormat: string;
begin
  Result := fFormat;
end;

function TJLabeledCurrencyEdit.getValue: currency;
begin
  Result := theValue;
end;

procedure TJLabeledCurrencyEdit.formatInput;
begin
  Caption := FormatFloat(DisplayFormat, theValue);
end;

procedure TJLabeledCurrencyEdit.setDecimals(const AValue: integer);
begin
  if (AValue >= 0) and (AValue < 5) then
    fDecimals := AValue;
end;

procedure TJLabeledCurrencyEdit.setFormat(const AValue: string);
begin
  fFormat := AValue;
  formatInput;
end;

function TJLabeledCurrencyEdit.scaleTo(const AValue: currency;
  const NDecimals: integer): currency;
begin
  Result := round(AValue * power(10, NDecimals)) / power(10, NDecimals);
end;

function TJLabeledCurrencyEdit.IsValidFloat(const Value: string): boolean;
begin
  if StrToCurrDef(Value, MaxCurrency) = MaxCurrency then
    Result := False
  else
    Result := True;
end;

procedure TJLabeledCurrencyEdit.setValue(const AValue: currency);
begin
  if fDecimals > 0 then
    theValue := scaleTo(AValue, fDecimals)
  else
    theValue := AValue;
  formatInput;
end;

procedure TJLabeledCurrencyEdit.DoEnter;
begin
  inherited DoEnter;
  if ReadOnly then
    exit;
  Text := FloatToStr(theValue);
  SelectAll;
end;

procedure TJLabeledCurrencyEdit.DoExit;
begin
  inherited DoExit;
  if ReadOnly then
    exit;
  if IsValidFloat(Text) then
    theValue := StrToCurr(Text)
  else
  begin
    ShowMessage(Format(SInvalidNumber, [Text]));
    SetFocus;
  end;
  if fDecimals > 0 then
    theValue := scaleTo(theValue, fDecimals);
  formatInput;
end;

procedure TJLabeledCurrencyEdit.KeyPress(var Key: char);
begin
  if (Key in ['.', ',']) then
    Key := Decimalseparator;
  if (key = DecimalSeparator) and (Pos(key, Text) > 0) then
    key := #0;
  if not (Key in ['0'..'9', DecimalSeparator, '+', '-', #8, #9]) then
    Key := #0;
  if (Key = DecimalSeparator) and (fDecimals = 0) then
    Key := #0;
  inherited KeyPress(Key);
end;

constructor TJLabeledCurrencyEdit.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Text := '';
  fFormat := '#,0.00';
  fDecimals := 2;
  formatInput;
end;

destructor TJLabeledCurrencyEdit.Destroy;
begin
  inherited Destroy;
end;

end.
