{ JDBImageBlob

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

unit JDBImageBlob;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, DB, DBCtrls,
  ExtCtrls, LMessages;

type

  { TJDBImageBlob }

  TJDBImageBlob = class(TCustomImage)
  private
    FAutoDisplay: boolean;
    { Private declarations }
    FDataLink: TFieldDataLink;

    procedure DataChange(Sender: TObject);
    procedure SetAutoDisplay(AValue: boolean);

    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;

    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure CMGetDataLink(var Message: TLMessage); message CM_GETDATALINK;
  protected
    { Protected declarations }
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure LoadPicture; virtual;
  public
    { Public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property Field: TField read GetField;
  published
    { Published declarations }

    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property AutoDisplay: boolean read FAutoDisplay write SetAutoDisplay default True;

    property Align;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property Center;
    property Constraints;
    property DragCursor;
    property DragMode;
    property OnClick;
    property OnDblClick;
    property PopupMenu;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property Proportional;
    property Stretch;
    property Transparent;
    property Visible;

  end;

procedure Register;

implementation

procedure Register;
begin
  {$I jdbimageblob_icon.lrs}
  RegisterComponents('JujiboDB', [TJDBImageBlob]);
end;

{ TJDBImageBlob }

procedure TJDBImageBlob.DataChange(Sender: TObject);
begin
  if AutoDisplay then
    LoadPicture;
end;

procedure TJDBImageBlob.SetAutoDisplay(AValue: boolean);
begin
  if FAutoDisplay = AValue then
    Exit;
  FAutoDisplay := AValue;
  if FAutoDisplay then
    LoadPicture;
end;

function TJDBImageBlob.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TJDBImageBlob.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TJDBImageBlob.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TJDBImageBlob.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

procedure TJDBImageBlob.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    ChangeDataSource(Self, FDataLink, Value);
end;

procedure TJDBImageBlob.CMGetDataLink(var Message: TLMessage);
begin
  Message.Result := PtrUInt(FDataLink);
end;

procedure TJDBImageBlob.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  // clean up
  if (Operation = opRemove) then
  begin
    if (FDataLink <> nil) and (AComponent = DataSource) then
      DataSource := nil;
  end;
end;

procedure TJDBImageBlob.LoadPicture;
var
  s: TStream;
begin
  if not assigned(FDatalink.Field) then
    Picture.Assign(FDatalink.Field)
  else
  if FDatalink.field.IsBlob then
  begin
    if FDatalink.field is TBlobField then
    begin
      if FDatalink.Field.IsNull then
      begin
        Picture.Clear;
        exit;
      end;
      s := FDataLink.DataSet.CreateBlobStream(FDataLink.Field, bmRead);
      if (s = nil) or (s.Size = 0) then
      begin
        Picture.Clear;
        exit;
      end;
      try
        try
          Picture.LoadFromStream(s);
        finally
          s.Free;
        end;
      except
        Picture.Clear;  // not a valid image
      end;
    end;
  end;
end;

constructor TJDBImageBlob.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  FAutoDisplay:=True;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := @DataChange;
end;

destructor TJDBImageBlob.Destroy;
begin
  FDataLink.Destroy;
  FDataLink := nil;
  inherited Destroy;
end;

{ TJDBImageBlob }



end.
