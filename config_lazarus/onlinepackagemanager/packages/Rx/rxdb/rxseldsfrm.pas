{ seldsfrm unit

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

unit rxseldsfrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ComponentEditors, DB, ButtonPanel;

type

  { TSelectDataSetForm }

  TSelectDataSetForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    CheckBox1: TCheckBox;
    Label1: TLabel;
    DataSetList: TListBox;
    procedure CheckBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
    procedure ListBox1KeyPress(Sender: TObject; var Key: char);
  private
    FDesigner: TComponentEditorDesigner;
    FExclude: string;
    procedure FillDataSetList(ExcludeDataSet: TDataSet);
    procedure AddDataSet(const S: string);
  public
    { public declarations }
  end; 

  { TMemDataSetEditor }

  TMemDataSetEditor = class(TComponentEditor)
  private
    DefaultEditor: TBaseComponentEditor;
    function UniqueName(Field: TField): string;
    procedure BorrowStructure;
  protected
    function CopyStructure(Source, Dest: TDataSet): Boolean; virtual;
  public
    constructor Create(AComponent: TComponent; ADesigner: TComponentEditorDesigner); override;
    destructor Destroy; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

function SelectDataSet(ADesigner: TComponentEditorDesigner; const ACaption: string;
  ExcludeDataSet: TDataSet): TDataSet;

var
  SelectDataSetForm: TSelectDataSetForm;

implementation
uses rxmemds, rxdconst;

{$R *.lfm}

function SelectDataSet(ADesigner: TComponentEditorDesigner; const ACaption: string;
  ExcludeDataSet: TDataSet): TDataSet;
begin
  Result := nil;
  with TSelectDataSetForm.Create(Application) do
  try
    if ACaption <> '' then Caption := ACaption;
    FDesigner := ADesigner;
    FillDataSetList(ExcludeDataSet);
    if ShowModal = mrOk then
      if DataSetList.ItemIndex >= 0 then
      begin
        with DataSetList do
          Result := FDesigner.Form.FindComponent(Items[ItemIndex]) as TDataSet;
      end;
  finally
    Free;
  end;
end;

{ TSelectDataSetForm }

procedure TSelectDataSetForm.CheckBox1Change(Sender: TObject);
begin
  Label1.Enabled:=not CheckBox1.Checked;
  DataSetList.Enabled:=not CheckBox1.Checked;
end;

procedure TSelectDataSetForm.FormCreate(Sender: TObject);
begin
  Caption:=sRxSelectDatasetStruct;
  CheckBox1.Caption:=sRxCopyOnlyMetadata;
  Label1.Caption:=sRxSourceDataset;
end;

procedure TSelectDataSetForm.ListBox1DblClick(Sender: TObject);
begin
  if DataSetList.ItemIndex >= 0 then ModalResult := mrOk;
end;

procedure TSelectDataSetForm.ListBox1KeyPress(Sender: TObject; var Key: char);
begin
  if (Key = #13) and (DataSetList.ItemIndex >= 0) then
    ModalResult := mrOk;
end;

procedure TSelectDataSetForm.FillDataSetList(ExcludeDataSet: TDataSet);
var
  I: Integer;
  Component: TComponent;
begin
  DataSetList.Items.BeginUpdate;
  try
    DataSetList.Clear;
    FExclude := '';
    if ExcludeDataSet <> nil then FExclude := ExcludeDataSet.Name;
    for I := 0 to FDesigner.Form.ComponentCount - 1 do
    begin
      Component := FDesigner.Form.Components[I];
      if (Component is TDataSet) and (Component <> ExcludeDataSet) then
        AddDataSet(Component.Name);
    end;
    with DataSetList do
    begin
      if Items.Count > 0 then ItemIndex := 0;
      Enabled := Items.Count > 0;
      ButtonPanel1.OKButton.Enabled:= (ItemIndex >= 0);
    end;
  finally
    DataSetList.Items.EndUpdate;
  end;
end;

procedure TSelectDataSetForm.AddDataSet(const S: string);
begin
  if (S <> '') and (S <> FExclude) then DataSetList.Items.Add(S);
end;

{ TMemDataSetEditor }

function TMemDataSetEditor.UniqueName(Field: TField): string;
const
  AlphaNumeric = ['A'..'Z', 'a'..'z', '_'] + ['0'..'9'];
var
  Temp: string;
  Comp: TComponent;
  I: Integer;
begin
  Result := '';
  if (Field <> nil) then begin
    Temp := Field.FieldName;
    for I := Length(Temp) downto 1 do
      if not (Temp[I] in AlphaNumeric) then System.Delete(Temp, I, 1);
    if (Temp = '') or not IsValidIdent(Temp) then begin
      Temp := Field.ClassName;
      if (UpCase(Temp[1]) = 'T') and (Length(Temp) > 1) then
        System.Delete(Temp, 1, 1);
    end;
  end
  else Exit;
  Temp := Component.Name + Temp;
  I := 0;
  repeat
    Result := Temp;
    if I > 0 then Result := Result + IntToStr(I);
    Comp := Designer.Form.FindComponent(Result);
    Inc(I);
  until (Comp = nil) or (Comp = Field);
end;

procedure TMemDataSetEditor.BorrowStructure;
var
  DataSet: TDataSet;
  I: Integer;
  Caption: string;
begin
  Caption := Component.Name;
  if (Component.Owner <> nil) and (Component.Owner.Name <> '') then
    Caption := Format('%s.%s', [Component.Owner.Name, Caption]);
  DataSet := SelectDataSet(Designer, Caption, TDataSet(Component));
  if DataSet <> nil then
  begin
//    StartWait;
    try
      if not CopyStructure(DataSet, Component as TDataSet) then Exit;
      with TDataSet(Component) do
      begin
        for I := 0 to FieldCount - 1 do
          if Fields[I].Name = '' then
            Fields[I].Name := UniqueName(Fields[I]);
      end;
      Modified;
    finally
//      StopWait;
    end;
    Designer.Modified;
  end;
end;

function TMemDataSetEditor.CopyStructure(Source, Dest: TDataSet): Boolean;
begin
  Result := Dest is TRxMemoryData;
  if Result then
    TRxMemoryData(Dest).CopyStructure(Source);
end;

type
  PClass = ^TClass;

constructor TMemDataSetEditor.Create(AComponent: TComponent;
  ADesigner: TComponentEditorDesigner);
var
  CompClass: TClass;
begin
  inherited Create(AComponent, ADesigner);
  CompClass := PClass(Acomponent)^;
  try
    PClass(AComponent)^ := TDataSet;
    DefaultEditor := GetComponentEditor(AComponent, ADesigner);
  finally
    PClass(AComponent)^ := CompClass;
  end;
end;

destructor TMemDataSetEditor.Destroy;
begin
  DefaultEditor.Free;
  inherited Destroy;
end;

procedure TMemDataSetEditor.ExecuteVerb(Index: Integer);
begin
  if Index < DefaultEditor.GetVerbCount then
    DefaultEditor.ExecuteVerb(Index)
  else
  begin
    case Index - DefaultEditor.GetVerbCount of
      0:BorrowStructure;
    end;
  end;
end;

function TMemDataSetEditor.GetVerb(Index: Integer): string;
begin
  if Index < DefaultEditor.GetVerbCount then
    Result := DefaultEditor.GetVerb(Index)
  else
  begin
    case Index - DefaultEditor.GetVerbCount of
      0:Result:=sRxBorrowStructure;
    end;
  end;
end;

function TMemDataSetEditor.GetVerbCount: Integer;
begin
  Result:=DefaultEditor.GetVerbCount + 1;
end;

end.

