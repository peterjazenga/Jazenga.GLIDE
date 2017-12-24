{
    This file is part of the Web Service Toolkit
    Copyright (c) 2007 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
unit ubindingedit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, StdCtrls, ActnList, Buttons,
  pastree, pascal_parser_intf,
  edit_helper;

type

  { TfBindingEdit }

  TfBindingEdit = class(TForm)
    actOK: TAction;
    AL: TActionList;
    Button1: TButton;
    Button2: TButton;
    edtName: TEdit;
    edtAddress: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    edtStyle: TRadioGroup;
    TabSheet1: TTabSheet;
    procedure actOKExecute(Sender: TObject);
    procedure actOKUpdate(Sender: TObject);
  private
    FUpdateType : TEditType;
    FObject : TwstBinding;
    FSymbolTable : TwstPasTreeContainer;
  private
    property UpdateType : TEditType read FUpdateType;
  private
    procedure LoadFromObject();
    procedure SaveToObject();
  public
    function UpdateObject(
      var   AObject     : TwstBinding;
      const AUpdateType : TEditType;
            ASymbolTable : TwstPasTreeContainer
    ):Boolean;
  end; 

var
  fBindingEdit: TfBindingEdit;

implementation

{$R *.lfm}

uses parserutils;

{ TfBindingEdit }

procedure TfBindingEdit.actOKUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := ( not IsStrEmpty(edtName.Text) ) and ( edtStyle.ItemIndex >= 0 );
end;

procedure TfBindingEdit.actOKExecute(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TfBindingEdit.LoadFromObject();
var
  i : Integer;
begin
  edtName.Text := FSymbolTable.GetExternalName(FObject);
  Caption := edtName.Text;
  edtAddress.Text := FObject.Address;
  i := Ord(FObject.BindingStyle);
  if ( i > Pred(edtStyle.Items.Count) ) then
    i := Pred(edtStyle.Items.Count);
  edtStyle.ItemIndex := i;
end;

procedure TfBindingEdit.SaveToObject();
begin
  FObject.Name := ExtractIdentifier(edtName.Text);
  FObject.Address := Trim(edtAddress.Text);
  FObject.BindingStyle := TBindingStyle(edtStyle.ItemIndex);
end;

function TfBindingEdit.UpdateObject(
  var   AObject: TwstBinding;
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

end.

