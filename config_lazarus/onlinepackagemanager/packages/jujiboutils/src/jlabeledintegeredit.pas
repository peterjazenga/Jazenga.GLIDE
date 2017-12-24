{ JLabeledIntegerEdit

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

unit JLabeledIntegerEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, ExtCtrls, Graphics,
  Dialogs, jinputconsts;

type

  { TJLabeledIntegerEdit }

  TJLabeledIntegerEdit = class(TCustomLabeledEdit)
  private
    fNull: boolean;
    { Private declarations }
    theValue: integer;
    fFormat: string;
    function getFormat: string;
    function getValue: integer;
    function getCurrentValue: integer;
    procedure setFormat(const AValue: string);
    procedure setValue(const AValue: integer);
    function IsValidInteger(const Value: string): boolean;
    procedure FormatInput;
  protected
    { Protected declarations }
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure KeyPress(var Key: char); override;
  public
    { Public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function isNull: boolean;
    property CurrentValue: integer read getCurrentValue;
  published
    { Published declarations }
    property DisplayFormat: string read getFormat write setFormat;
    property Value: integer read getValue write setValue;
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


procedure Register;
begin
  {$I jlabeledintegeredit_icon.lrs}
  RegisterComponents('Jujibo', [TJLabeledIntegerEdit]);
end;

{ TJLabeledIntegerEdit }

function TJLabeledIntegerEdit.getFormat: string;
begin
  Result := fFormat;
end;

function TJLabeledIntegerEdit.getValue: integer;
begin
  Result := theValue;
end;

function TJLabeledIntegerEdit.getCurrentValue: integer;
begin
  Result := StrToIntDef(Text, Value);
end;

procedure TJLabeledIntegerEdit.setFormat(const AValue: string);
begin
  fFormat := AValue;
  formatInput;
end;

procedure TJLabeledIntegerEdit.setValue(const AValue: integer);
begin
  theValue := AValue;
  formatInput;
end;

function TJLabeledIntegerEdit.IsValidInteger(const Value: string): boolean;
begin
  if StrToIntDef(Value, MaxInt) = MaxInt then
    Result := False
  else
    Result := True;
end;

procedure TJLabeledIntegerEdit.FormatInput;
begin
  if isNull then
    Text := ''
  else
    Text := FormatFloat(fFormat, theValue);
end;

procedure TJLabeledIntegerEdit.DoEnter;
begin
  inherited DoEnter;
  if ReadOnly then
    exit;
  if not isNull then
    Text := IntToStr(theValue);
  SelectAll;
end;

procedure TJLabeledIntegerEdit.DoExit;
begin
  inherited DoExit;
  if ReadOnly then
    exit;
  if fNull and (Length(Caption) = 0) then
    theValue := Low(integer)  // min integer value means null
  else
  if IsValidInteger(Text) then
    theValue := StrToInt(Text)
  else
  begin
    ShowMessage(Format(SInvalidNumber, [Text]));
    SetFocus;
  end;
  formatInput;
end;

procedure TJLabeledIntegerEdit.KeyPress(var Key: char);
begin
  if not (Key in ['0'..'9', #8, #9, '-']) then
    Key := #0;
  inherited KeyPress(Key);
end;

constructor TJLabeledIntegerEdit.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  // Set initial values
  Text := '';
  DisplayFormat := '0';
  Value := 0;
end;

destructor TJLabeledIntegerEdit.Destroy;
begin
  inherited Destroy;
end;

function TJLabeledIntegerEdit.isNull: boolean;
begin
  Result := fNull and (theValue = Low(integer));
end;

end.

