{
    This file is part of the Web Service Toolkit
    Copyright (c) 2007 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
unit umoduleedit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, StdCtrls, ActnList, Buttons,
  pastree, pascal_parser_intf, xsd_consts,
  edit_helper;

type

  { TfModuleEdit }

  TfModuleEdit = class(TForm)
    actOK: TAction;
    AL: TActionList;
    Button1: TButton;
    Button2: TButton;
    edtName: TEdit;
    edtNamespace: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    edtDefaultElementForm: TRadioGroup;
    edtDefaultAttributeForm: TRadioGroup;
    TabSheet1: TTabSheet;
    procedure actOKExecute(Sender: TObject);
    procedure actOKUpdate(Sender: TObject);
  private
    FUpdateType : TEditType;
    FObject : TPasModule;
    FSymbolTable : TwstPasTreeContainer;
  private
    property UpdateType : TEditType read FUpdateType;
  private
    procedure LoadFromObject();
    procedure SaveToObject();
  public
    function UpdateObject(
      var   AObject     : TPasModule;
      const AUpdateType : TEditType;
            ASymbolTable : TwstPasTreeContainer
    ):Boolean;
  end; 

var
  fModuleEdit: TfModuleEdit;

implementation

{$R *.lfm}

uses parserutils;

{ TfModuleEdit }

procedure TfModuleEdit.actOKUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := not ( IsStrEmpty(edtName.Text) or IsStrEmpty(edtNamespace.Text));
end;

function FormStringToIndex(const AStr : string) : Integer;
begin
  if (AStr = s_qualified) then
    Result := 1
  else if (AStr = s_unqualified) then
    Result := 2
  else
    Result := 0;
end;

procedure TfModuleEdit.LoadFromObject();
var
  s : string;
begin
  edtName.Text := FSymbolTable.GetExternalName(FObject);
  edtNamespace.Text := FSymbolTable.GetExternalName(FObject);
  s := FSymbolTable.Properties.GetValue(FObject,s_elementFormDefault);
  edtDefaultElementForm.ItemIndex := FormStringToIndex(s);
  s := FSymbolTable.Properties.GetValue(FObject,s_attributeFormDefault);
  edtDefaultAttributeForm.ItemIndex := FormStringToIndex(s);
end;

procedure TfModuleEdit.SaveToObject();
var
  s : string;
begin
  FObject.Name := ExtractIdentifier(edtName.Text);
  FSymbolTable.RegisterExternalAlias(FObject,Trim(edtNamespace.Text));
  s := '';
  if (edtDefaultElementForm.ItemIndex > 0) then
    s := edtDefaultElementForm.Items[edtDefaultElementForm.ItemIndex];
  FSymbolTable.Properties.SetValue(FObject,s_elementFormDefault,s);
  s := '';
  if (edtDefaultAttributeForm.ItemIndex > 0) then
    s := edtDefaultAttributeForm.Items[edtDefaultAttributeForm.ItemIndex];
  FSymbolTable.Properties.SetValue(FObject,s_attributeFormDefault,s);
end;

function TfModuleEdit.UpdateObject(
  var AObject: TPasModule;
  const AUpdateType: TEditType;
        ASymbolTable: TwstPasTreeContainer
): Boolean;
begin
  Assert(Assigned(ASymbolTable));
  FSymbolTable := ASymbolTable;
  FUpdateType := AUpdateType;
  FObject := AObject;
  LoadFromObject();
  Result := ( ShowModal() = mrOK );
  if Result then begin
    SaveToObject();
  end;
end;

procedure TfModuleEdit.actOKExecute(Sender: TObject);
begin
  ModalResult := mrOK;
end;

end.

