{ RxDBCtrls unit

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

unit RxDBCtrls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, DB, DbCtrls, LMessages, LCLType;

type

  { TCustomRxDBProgressBar }

  TCustomRxDBProgressBar = class(TCustomProgressBar)
  private
    FDataLink: TFieldDataLink;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    procedure SetDataField(const AValue: string);
    procedure SetDataSource(const AValue: TDataSource);
    procedure DataChange(Sender: TObject);
    procedure ActiveChange(Sender: TObject);
    procedure LayoutChange(Sender: TObject);
    procedure CMGetDataLink(var Message: TLMessage); message CM_GETDATALINK;
  protected
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Field: TField read GetField;
  end;

  TRxDBProgressBar = class(TCustomRxDBProgressBar)
  published
    property DataField;
    property DataSource;
    property Align;
    property Anchors;
    property BorderSpacing;
    property BorderWidth;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Hint;
    property Max;
    property Min;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnStartDock;
    property OnStartDrag;
    property Orientation;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Smooth;
    property Step;
    property TabOrder;
    property TabStop;
    property Visible;
    property BarShowText;
  end;

type

  { TCustomRxDBTrackBar }

  TCustomRxDBTrackBar = class(TCustomTrackBar)
  private
    FDataLink: TFieldDataLink;
    FInScrollEvent:boolean;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    procedure SetDataField(const AValue: string);
    procedure SetDataSource(const AValue: TDataSource);
    procedure DataChange(Sender: TObject);
    procedure ActiveChange(Sender: TObject);
    procedure LayoutChange(Sender: TObject);
    procedure CMGetDataLink(var Message: TLMessage); message CM_GETDATALINK;
    procedure WMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
    procedure WMKillFocus(var Message: TLMKillFocus); message LM_KILLFOCUS;
    function GetReadOnly: Boolean;
    procedure SetReadOnly(const AValue: Boolean);
    function IsReadOnly: boolean;
    procedure UpdateData(Sender: TObject);
  protected
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    procedure DoChange(var msg); message LM_CHANGED;
//    procedure Change; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Field: TField read GetField;
  end;

  TRxDBTrackBar = class(TCustomRxDBTrackBar)
  published
    property DataField;
    property DataSource;
    property ReadOnly;

    property Align;
    property Anchors;
    property BorderSpacing;
    property Constraints;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Frequency;
    property Hint;
    property LineSize;
    property Max;
    property Min;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnResize;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property Orientation;
    property PageSize;
    property ParentShowHint;
    property PopupMenu;
    property ScalePos;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property TickMarks;
    property TickStyle;
    property Visible;
  end;

type

  { TRxDBRadioGroup }

  TRxDBRadioGroup = class(TDBRadioGroup)
  private
    function GetItemEnabled(Index: integer): boolean;
    procedure SetItemEnabled(Index: integer; AValue: boolean);
  public
    property ItemEnabled[Index: integer]: boolean read GetItemEnabled write SetItemEnabled;
  published
    property AutoSize;
  end;

implementation
uses rxdbutils, LCLVersion, rxvclutils, StdCtrls;

{ TRxDBRadioGroup }

function TRxDBRadioGroup.GetItemEnabled(Index: integer): boolean;
var
  R:TRadioButton;
begin
  if (Index < -1) or (Index >= Items.Count) then
    RaiseIndexOutOfBounds(Self, Items, Index);
  R:=FindComponent('RadioButton'+IntToStr(Index)) as TRadioButton;
  if Assigned(R) then
    Result:=R.Enabled
  else
    Result:=False;
end;

procedure TRxDBRadioGroup.SetItemEnabled(Index: integer; AValue: boolean);
var
  R:TRadioButton;
begin
  if (Index < -1) or (Index >= Items.Count) then
    RaiseIndexOutOfBounds(Self, Items, Index);
  R:=FindComponent('RadioButton'+IntToStr(Index)) as TRadioButton;
  if Assigned(R) then
    R.Enabled:=AValue;
end;

{ TCustomRxDBProgressBar }

function TCustomRxDBProgressBar.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TCustomRxDBProgressBar.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TCustomRxDBProgressBar.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TCustomRxDBProgressBar.ActiveChange(Sender: TObject);
begin
  if FDatalink.Active then
    DataChange(Sender)
  else
  begin
    Text := '';
    FDataLink.Reset;
  end;
end;

procedure TCustomRxDBProgressBar.LayoutChange(Sender: TObject);
begin
  DataChange(Sender);
end;

procedure TCustomRxDBProgressBar.CMGetDataLink(var Message: TLMessage);
begin
  Message.Result := PtrUInt(FDataLink);
end;

procedure TCustomRxDBProgressBar.SetDataField(const AValue: string);
begin
  FDataLink.FieldName := AValue;
end;

procedure TCustomRxDBProgressBar.SetDataSource(const AValue: TDataSource);
begin
  ChangeDataSource(Self,FDataLink,AValue);
end;

procedure TCustomRxDBProgressBar.DataChange(Sender: TObject);
begin
  if Assigned(FDataLink.Field) and (FDataLink.Field.DataType in IntegerDataTypes) then
    Self.Position:=FDatalink.Field.AsInteger
  else
    Position:=Min
end;

constructor TCustomRxDBProgressBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := @DataChange;
  FDataLink.OnActiveChange := @ActiveChange;
  {$if (lcl_major = 0) and (lcl_release <= 30)}
  FDataLink.OnLayoutChange := @LayoutChange;
  {$endif}
end;

destructor TCustomRxDBProgressBar.Destroy;
begin
  FreeAndNil(FDataLink);
  inherited Destroy;
end;

{ TCustomRxDBTrackBar }

function TCustomRxDBTrackBar.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TCustomRxDBTrackBar.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TCustomRxDBTrackBar.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TCustomRxDBTrackBar.SetDataField(const AValue: string);
begin
  FDataLink.FieldName := AValue;
end;

procedure TCustomRxDBTrackBar.SetDataSource(const AValue: TDataSource);
begin
  ChangeDataSource(Self,FDataLink,AValue);
end;

procedure TCustomRxDBTrackBar.DataChange(Sender: TObject);
begin
  FInScrollEvent:=true;
  if Assigned(FDataLink.Field) and (FDataLink.Field.DataType in IntegerDataTypes) then
    Self.Position:=FDatalink.Field.AsInteger
  else
    Self.Position:=0;
  FInScrollEvent:=false;
end;

procedure TCustomRxDBTrackBar.ActiveChange(Sender: TObject);
begin
  if FDatalink.Active then
    DataChange(Sender)
  else
  begin
    Position:=0;
    FDataLink.Reset;
  end;
end;

procedure TCustomRxDBTrackBar.LayoutChange(Sender: TObject);
begin
  DataChange(Sender);
end;

procedure TCustomRxDBTrackBar.CMGetDataLink(var Message: TLMessage);
begin
  Message.Result := PtrUInt(FDataLink);
end;

procedure TCustomRxDBTrackBar.WMSetFocus(var Message: TLMSetFocus);
begin
  inherited WMSetFocus(Message);
  if not FDatalink.Editing then
    FDatalink.Reset;
end;

procedure TCustomRxDBTrackBar.WMKillFocus(var Message: TLMKillFocus);
begin
  inherited WMKillFocus(Message);
  if not FDatalink.Editing then
    FDatalink.Reset
  else
    FDatalink.UpdateRecord;
end;

function TCustomRxDBTrackBar.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TCustomRxDBTrackBar.SetReadOnly(const AValue: Boolean);
begin
  inherited;
  FDataLink.ReadOnly := AValue;
end;

function TCustomRxDBTrackBar.IsReadOnly: boolean;
begin
  result := true;
  if FDatalink.Active and not Self.ReadOnly then
    result := (Field=nil) or Field.ReadOnly;
end;

procedure TCustomRxDBTrackBar.UpdateData(Sender: TObject);
begin
  if Assigned(FDataLink.Field) and (FDataLink.Field.DataType in IntegerDataTypes) then
    FDataLink.Field.AsInteger := Self.Position;
end;

procedure TCustomRxDBTrackBar.DoChange(var msg);
begin
  inherited DoChange(Msg);
  if not FInScrollEvent then
  begin
    FDatalink.Edit;
    FDataLink.Modified;
  end;
end;

{
procedure TCustomRxDBTrackBar.Change;
begin
  FDatalink.Edit;
  FDataLink.Modified;
  inherited Change;
end;
}
procedure TCustomRxDBTrackBar.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then
    DataChange(Self);
end;

procedure TCustomRxDBTrackBar.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) then
  begin
    if (FDataLink<>nil) and (AComponent=DataSource) then
      DataSource:=nil;
  end;
end;

constructor TCustomRxDBTrackBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInScrollEvent:=false;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := @DataChange;
  FDataLink.OnUpdateData := @UpdateData;
  FDataLink.OnActiveChange := @ActiveChange;
  {$if (lcl_major = 0) and (lcl_release <= 30)}
  FDataLink.OnLayoutChange := @LayoutChange;
  {$endif}
end;

destructor TCustomRxDBTrackBar.Destroy;
begin
  FreeAndNil(FDataLink);
  inherited Destroy;
end;

end.

