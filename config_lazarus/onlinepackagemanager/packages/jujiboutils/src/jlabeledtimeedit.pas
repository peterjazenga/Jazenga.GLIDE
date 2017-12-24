{ TJLabeledTimeEdit

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

unit JLabeledTimeEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, ExtCtrls, Graphics,
  Dialogs, jcontrolutils, jinputconsts;

type
  TJLabeledTimeEdit = class(TCustomLabeledEdit)
  private
    { Private declarations }
    theValue: TTime;
    hasValue: boolean;
    fFormat: string;
    function getFormat: string;
    function getValue: TTime;
    procedure setFormat(const AValue: string);
    procedure setValue(const AValue: TTime);
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
  published
    { Published declarations }
    function isNull: boolean;
    property DisplayFormat: string read getFormat write setFormat;
    property Value: TTime read getValue write setValue;

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
  {$I jlabeledtimeedit_icon.lrs}
  RegisterComponents('Jujibo', [TJLabeledTimeEdit]);
end;


function TJLabeledTimeEdit.getFormat: string;
begin
  Result := fFormat;
end;

function TJLabeledTimeEdit.getValue: TTime;
begin
  Result := theValue;
end;

procedure TJLabeledTimeEdit.setFormat(const AValue: string);
begin
  fFormat := AValue;
  formatInput;
end;

procedure TJLabeledTimeEdit.setValue(const AValue: TTime);
begin
  theValue := AValue;
  hasValue := True;
  formatInput;
end;

procedure TJLabeledTimeEdit.FormatInput;
begin
  if hasValue then
    Text := FormatDateTime(fFormat, theValue)
  else
    Text := '';
end;

procedure TJLabeledTimeEdit.DoEnter;
begin
  inherited DoEnter;
  if ReadOnly then
    exit;
  if not hasValue then
    Text := ''
  else
    Text := TimeToStr(theValue);
  SelectAll;
end;

procedure TJLabeledTimeEdit.DoExit;
begin
  inherited DoExit;
  if ReadOnly then
    exit;
  Text := NormalizeTime(Text, theValue);
  if Length(Text) = 0 then
  begin
    theValue := 0;
    hasValue := False;
  end
  else
  if IsValidTimeString(Text) then
  begin
    theValue := StrToTime(Text);
    hasValue := True;
  end
  else
  begin
    ShowMessage(Format(SInvalidTime, [Text]));
    SetFocus;
  end;
  formatInput;
end;

procedure TJLabeledTimeEdit.KeyPress(var Key: char);
begin
  if not (Key in ['0'..'9', #8, #9, ':']) then
    Key := #0;
  inherited KeyPress(Key);
end;

constructor TJLabeledTimeEdit.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Text := '';
  DisplayFormat := 'hh:mm:ss';
  Value := 0;
  hasValue := True;
end;

destructor TJLabeledTimeEdit.Destroy;
begin
  inherited Destroy;
end;

function TJLabeledTimeEdit.isNull: boolean;
begin
  Result := not hasValue;
end;

end.

