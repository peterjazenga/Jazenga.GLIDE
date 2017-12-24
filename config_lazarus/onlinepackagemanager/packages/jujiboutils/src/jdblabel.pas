{ TJDbLabel

  Copyright (C) 2013 Julio Jiménez Borreguero
  Contact: jujibo at gmail dot com

  This library is free software; you can redistribute it and/or modify it
  under the same terms as the Lazarus Component Library (LCL)

  See the file license-jujiboutils.txt and COPYING.LGPL, included in this distribution,
  for details about the license.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}

unit JDbLabel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  JLabel, DB, DBCtrls, LMessages;

type

  { TJDbLabel }

  TJDbLabel = class(TJCustomLabel)
  private
    { Private declarations }
    FDataLink: TFieldDataLink;

    procedure DataChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;

    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure CMGetDataLink(var Message: TLMessage); message CM_GETDATALINK;
  protected
    { Protected declarations }
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Loaded; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Field: TField read GetField;
  published
    { Published declarations }
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;

    property ShadowColor;
    property ShadowColor2;
    property LabelStyle;
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FocusControl;
    property Font;
    property Layout;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property Transparent;
    property Visible;
    property WordWrap;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnChangeBounds;
    property OnContextPopup;
    property OnResize;
    property OnStartDrag;
    property OptimalFill;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I jdblabel_icon.lrs}
  RegisterComponents('JujiboDB', [TJDbLabel]);
end;

{ TJDbLabel }

procedure TJDbLabel.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
    Caption := FDataLink.Field.DisplayText
  else if csDesigning in ComponentState then
    Caption := Name
  else
    Caption := '';
end;

function TJDbLabel.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TJDbLabel.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TJDbLabel.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TJDbLabel.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

procedure TJDbLabel.SetDataSource(Value: TDataSource);
begin
  ChangeDataSource(Self, FDataLink, Value);
end;

procedure TJDbLabel.CMGetDataLink(var Message: TLMessage);
begin
  Message.Result := PtrUInt(FDataLink);
end;

procedure TJDbLabel.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
  begin
    if (FDataLink <> nil) and (AComponent = DataSource) then
      DataSource := nil;
  end;
end;

procedure TJDbLabel.Loaded;
begin
  inherited Loaded;
  if csDesigning in ComponentState then
    Caption := Name;
end;

constructor TJDbLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := @DataChange;
end;

destructor TJDbLabel.Destroy;
begin
  FDataLink.Free;
  inherited Destroy;
end;

end.
