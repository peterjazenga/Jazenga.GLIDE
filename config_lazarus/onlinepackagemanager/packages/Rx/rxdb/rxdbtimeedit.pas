{ RxDBTimeEdit unit

  Copyright (C) 2005-2017 Lagunov Aleksey alexs75@yandex.ru and Lazarus team
  original conception from rx library for Delphi (c)

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit RxDBTimeEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  RxTimeEdit, DB, DbCtrls, LMessages, LCLType;

type

  { TCustomRxDBTimeEdit }

  TCustomRxDBTimeEdit = class(TCustomRxTimeEdit)
  private
    FDataLink: TFieldDataLink;

    procedure DataChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    procedure SetDataField(const AValue: string);
    procedure SetDataSource(const AValue: TDataSource);

    procedure UpdateData(Sender: TObject);
    procedure FocusRequest(Sender: TObject);
    procedure ActiveChange(Sender: TObject);
    procedure LayoutChange(Sender: TObject);
    procedure CMGetDataLink(var Message: TLMessage); message CM_GETDATALINK;
    function IsReadOnly: boolean;
    procedure WMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
    procedure WMKillFocus(var Message: TLMKillFocus); message LM_KILLFOCUS;
  protected
    function GetReadOnly: Boolean;override;
    procedure SetReadOnly(AValue: Boolean);override;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    //property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Change; override;

    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Field: TField read GetField;
  end;

  TRxDBTimeEdit = class(TCustomRxDBTimeEdit)
  published
    property DataField;
    property DataSource;
    property ReadOnly;

    property AutoSize;
    property AutoSelect;
    property Align;
    property Anchors;
    property BorderSpacing;
    property ButtonOnlyWhenFocused;
    property ButtonHint;
    property CharCase;
    property Color;
//    property DirectInput;
    property DragCursor;
    property DragMode;
    property EchoMode;
    property Enabled;
//    property Flat;
    property Font;
//    property Glyph;
    property MaxLength;
//    property NumGlyphs;
    property OnButtonClick;
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
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
  end;


implementation
uses rxdbutils, LCLVersion;

type
  TFieldDataLinkHack = class(TFieldDataLink)
  end;

{ TCustomRxDBTimeEdit }

procedure TCustomRxDBTimeEdit.DataChange(Sender: TObject);
begin
  if Assigned(FDataLink.Field) and (FDataLink.Field.DataType in DataTimeTypes) then
    Self.Time:=FDatalink.Field.AsDateTime
  else
    Text := '';
end;

function TCustomRxDBTimeEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TCustomRxDBTimeEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TCustomRxDBTimeEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TCustomRxDBTimeEdit.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TCustomRxDBTimeEdit.SetDataField(const AValue: string);
begin
  FDataLink.FieldName := AValue;
end;

procedure TCustomRxDBTimeEdit.SetDataSource(const AValue: TDataSource);
begin
  ChangeDataSource(Self,FDataLink,AValue);
end;

procedure TCustomRxDBTimeEdit.SetReadOnly(AValue: Boolean);
begin
  inherited SetReadOnly(AValue);
  FDataLink.ReadOnly := AValue;
end;

procedure TCustomRxDBTimeEdit.UpdateData(Sender: TObject);
begin
  if Assigned(FDataLink.Field) and (FDataLink.Field.DataType in DataTimeTypes) then
  begin
    FDataLink.Field.AsDateTime := Self.Time;
  end;
end;

procedure TCustomRxDBTimeEdit.FocusRequest(Sender: TObject);
begin
  SetFocus;
end;

procedure TCustomRxDBTimeEdit.ActiveChange(Sender: TObject);
begin
  if FDatalink.Active then
    DataChange(Sender)
  else
  begin
    Text := '';
    FDataLink.Reset;
  end;
end;

procedure TCustomRxDBTimeEdit.LayoutChange(Sender: TObject);
begin
  DataChange(Sender);
end;

procedure TCustomRxDBTimeEdit.CMGetDataLink(var Message: TLMessage);
begin
  Message.Result := PtrUInt(FDataLink);
end;

function TCustomRxDBTimeEdit.IsReadOnly: boolean;
begin
  result := true;
  if FDatalink.Active and not Self.ReadOnly then
    result := (Field=nil) or Field.ReadOnly;
end;

procedure TCustomRxDBTimeEdit.WMSetFocus(var Message: TLMSetFocus);
begin
  inherited WMSetFocus(Message);
  if not FDatalink.Editing then
    FDatalink.Reset;
end;

procedure TCustomRxDBTimeEdit.WMKillFocus(var Message: TLMKillFocus);
begin
  inherited WMKillFocus(Message);
  if not FDatalink.Editing then
    FDatalink.Reset
  else
    TFieldDataLinkHack(FDatalink).UpdateData;
end;

procedure TCustomRxDBTimeEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if Key=VK_ESCAPE then
  begin
    //cancel out of editing by reset on esc
    FDataLink.Reset;
    SelectAll;
    Key := VK_UNKNOWN;
  end
  else
  if Key=VK_DELETE then
  begin
    if not IsReadOnly then
      FDatalink.Edit;
  end
  else
  if Key=VK_TAB then
  begin
    if FDataLink.CanModify and FDatalink.Editing then
      FDataLink.UpdateRecord;
  end;
end;

procedure TCustomRxDBTimeEdit.Change;
begin
  if Assigned(FDatalink) then
  begin
    FDatalink.Edit;
    FDataLink.Modified;
  end;
  inherited Change;
end;

procedure TCustomRxDBTimeEdit.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then
    DataChange(Self);
end;

procedure TCustomRxDBTimeEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) then
  begin
    if (FDataLink<>nil) and (AComponent=DataSource) then
      DataSource:=nil;
  end;
end;

constructor TCustomRxDBTimeEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := @DataChange;
  FDataLink.OnUpdateData := @UpdateData;
  FDataLink.OnActiveChange := @ActiveChange;
  {$if (lcl_major = 0) and (lcl_release <= 30)}
  FDataLink.OnLayoutChange := @LayoutChange;
  {$endif}
end;

destructor TCustomRxDBTimeEdit.Destroy;
begin
  FreeAndNil(FDataLink);
  inherited Destroy;
end;

end.
