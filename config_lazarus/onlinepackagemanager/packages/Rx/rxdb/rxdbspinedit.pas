{ RxDBSpinEdit unit

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

unit RxDBSpinEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Spin,
  DbCtrls, DB, LMessages, LCLType, LCLVersion;


type

  { TCustomRxDBSpinEdit }

  TCustomRxDBSpinEdit = class(TCustomFloatSpinEdit)
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
    procedure WMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
    procedure WMKillFocus(var Message: TLMKillFocus); message LM_KILLFOCUS;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Field: TField read GetField;
  published
    { Published declarations }
  end;

  TRxDBSpinEdit = class(TCustomRxDBSpinEdit)
  published
    property DataField;
    property DataSource;
    property ReadOnly;

    property Align;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BorderSpacing;
    property Constraints;
    property DecimalPlaces;
    property Enabled;
    property Font;
    property Increment;
    property MaxValue;
    property MinValue;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnEditingDone;
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
    property OnResize;
    property OnUTF8KeyPress;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabStop;
    property TabOrder;
    property Value;
    property Visible;
  end;

implementation
uses rxdbutils;

type
  TFieldDataLinkHack = class(TFieldDataLink)
  end;

{ TCustomRxDBSpinEdit }

procedure TCustomRxDBSpinEdit.DataChange(Sender: TObject);
begin
  if Assigned(FDataLink.Field) and (FDataLink.Field.DataType in NumericDataTypes) then
  begin
    if (FDataLink.Field.DataType in IntegerDataTypes) then
      DecimalPlaces:=0
    else
    begin
      if FDataLink.Field.DataType = ftBCD then
        DecimalPlaces:=(FDatalink.Field as TBCDField).Precision
      else
        DecimalPlaces:=(FDatalink.Field as TFloatField).Precision;
    end;
    Value:=FDatalink.Field.AsFloat;
  end
  else
  begin
    Text := '';
  end;
end;

function TCustomRxDBSpinEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TCustomRxDBSpinEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TCustomRxDBSpinEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TCustomRxDBSpinEdit.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TCustomRxDBSpinEdit.SetDataField(const AValue: string);
begin
  FDataLink.FieldName := AValue;
end;

procedure TCustomRxDBSpinEdit.SetDataSource(const AValue: TDataSource);
begin
  ChangeDataSource(Self,FDataLink,AValue);
end;

procedure TCustomRxDBSpinEdit.SetReadOnly(AValue: Boolean);
begin
  inherited;
  FDataLink.ReadOnly := AValue;
end;

procedure TCustomRxDBSpinEdit.UpdateData(Sender: TObject);
begin
  FDataLink.Field.Value := Value;
end;

procedure TCustomRxDBSpinEdit.FocusRequest(Sender: TObject);
begin
  SetFocus;
end;

procedure TCustomRxDBSpinEdit.ActiveChange(Sender: TObject);
begin
  if FDatalink.Active then
    DataChange(Sender)
  else
  begin
    Text := '';
    FDataLink.Reset;
  end;
end;

procedure TCustomRxDBSpinEdit.LayoutChange(Sender: TObject);
begin
  DataChange(Sender);
end;

procedure TCustomRxDBSpinEdit.CMGetDataLink(var Message: TLMessage);
begin
  Message.Result := PtrUInt(FDataLink);
end;

procedure TCustomRxDBSpinEdit.KeyDown(var Key: Word; Shift: TShiftState);
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

procedure TCustomRxDBSpinEdit.Change;
begin
  FDatalink.Edit;
  FDataLink.Modified;
  inherited Change;
end;

function TCustomRxDBSpinEdit.IsReadOnly: boolean;
begin
  Result := true;
  if FDatalink.Active and (not Self.ReadOnly) then
    Result := (Field = nil) or Field.ReadOnly;
end;

procedure TCustomRxDBSpinEdit.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then
    DataChange(Self);
end;

procedure TCustomRxDBSpinEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) then
  begin
    if (FDataLink<>nil) and (AComponent=DataSource) then
      DataSource:=nil;
  end;
end;

procedure TCustomRxDBSpinEdit.WMSetFocus(var Message: TLMSetFocus);
begin
  inherited WMSetFocus(Message);
  if not FDatalink.Editing then
    FDatalink.Reset;
end;

procedure TCustomRxDBSpinEdit.WMKillFocus(var Message: TLMKillFocus);
begin
  inherited WMKillFocus(Message);
  if not FDatalink.Editing then
    FDatalink.Reset
  else
    TFieldDataLinkHack(FDatalink).UpdateData;
end;

constructor TCustomRxDBSpinEdit.Create(AOwner: TComponent);
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

destructor TCustomRxDBSpinEdit.Destroy;
begin
  FreeAndNil(FDataLink);
  inherited Destroy;
end;

end.
