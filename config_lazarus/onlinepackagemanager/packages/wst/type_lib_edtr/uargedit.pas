{
    This file is part of the Web Service Toolkit
    Copyright (c) 2007 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
unit uargedit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, StdCtrls, ActnList,
  pastree, pascal_parser_intf, edit_helper, Buttons;

type

  { TfArgEdit }

  TfArgEdit = class(TForm)
    actOK: TAction;
    AL: TActionList;
    Button1: TButton;
    Button2: TButton;
    edtType: TComboBox;
    edtName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    edtModifier: TRadioGroup;
    TabSheet1: TTabSheet;
    procedure actOKExecute(Sender: TObject);
    procedure actOKUpdate(Sender: TObject);
  private
    FUpdateType : TEditType;
    FObject : TPasArgument;
    FSymbolTable : TwstPasTreeContainer;
  private
    property UpdateType : TEditType read FUpdateType;
  private
    procedure LoadFromObject();
    procedure SaveToObject();
  public
    function UpdateObject(
      var   AObject      : TPasArgument;
      const AUpdateType  : TEditType;
            ASymbolTable : TwstPasTreeContainer
    ) : Boolean;
  end; 

var
  fArgEdit: TfArgEdit;

implementation

{$R *.lfm}

uses parserutils;

{ TfArgEdit }

procedure TfArgEdit.actOKUpdate(Sender: TObject);
var
  b : Boolean;
  i : Integer;
  locArg : TPasArgument;
  argList : TList2;
  locName : string;
begin
  locName := edtName.Text;
  b := ( not IsStrEmpty(locName) ) and
       ( edtType.ItemIndex > -1 );
  if b then begin
    argList := TPasProcedureType(FObject.Parent).Args;
    for i := 0 to Pred(argList.Count) do begin
      locArg := TPasArgument(argList[i]);
      if ( locArg <> FObject ) and
         ( AnsiSameText(locArg.Name,locName) or
           AnsiSameText(FSymbolTable.GetExternalName(locArg),locName)
         )
      then begin
        b := False;
        Break;
      end;
    end;
  end;
  TAction(Sender).Enabled := b;
end;

procedure TfArgEdit.actOKExecute(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TfArgEdit.LoadFromObject();
begin
  edtName.Text := FSymbolTable.GetExternalName(FObject);
  edtModifier.ItemIndex := Ord(FObject.Access);
  Caption := edtName.Caption;
  edtType.Items.BeginUpdate();
  try
    edtType.Items.Clear();
    FillTypeList(edtType.Items,FSymbolTable);
    edtType.ItemIndex := edtType.Items.IndexOfObject(FObject.ArgType);
  finally
    edtType.Items.EndUpdate();
  end;
end;

procedure TfArgEdit.SaveToObject();
var
  locObj : TPasArgument;
  typExtName, typIntName : string;
  propType : TPasType;
begin
  locObj := nil;
  typExtName := ExtractIdentifier(edtName.Text);
  typIntName := MakeInternalSymbolNameFrom(typExtName);
  propType := edtType.Items.Objects[edtType.ItemIndex] as TPasType;
  locObj := FObject;
  if ( propType <> locObj.ArgType ) then begin
    if ( locObj.ArgType <> nil ) then
      locObj.ArgType.Release();
    locObj.ArgType := propType;
    locObj.ArgType.AddRef();
  end;
  locObj.Name := typIntName;
  FSymbolTable.RegisterExternalAlias(locObj,typExtName);
  locObj.Access := TArgumentAccess(edtModifier.ItemIndex);
end;

function TfArgEdit.UpdateObject(
  var   AObject: TPasArgument;
  const AUpdateType: TEditType;
        ASymbolTable: TwstPasTreeContainer
): Boolean;
begin
  FSymbolTable := ASymbolTable;
  FUpdateType := AUpdateType;
  FObject := AObject;
  LoadFromObject();
  ActiveControl := edtName;
  Result := ( ShowModal() = mrOK );
  if Result then begin
    SaveToObject();
    AObject := FObject;
  end;
end;

end.

