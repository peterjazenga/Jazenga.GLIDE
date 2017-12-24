{
    This file is part of the Web Service Toolkit
    Copyright (c) 2007 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$INCLUDE wst_global.inc}
unit uinterfaceedit;

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ActnList,
  ExtCtrls, ComCtrls, StdCtrls, Buttons,
  pastree, pascal_parser_intf,
  edit_helper, Menus;

type

  { TfInterfaceEdit }

  TfInterfaceEdit = class(TForm)
    actDeleteOperation: TAction;
    actBindingEdit: TAction;
    actUpdateOperation: TAction;
    actNewMethod: TAction;
    actOK: TAction;
    AL: TActionList;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    edtName: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    PC: TPageControl;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    TabSheet1: TTabSheet;
    trvMethods: TTreeView;
    procedure actBindingEditExecute(Sender: TObject);
    procedure actBindingEditUpdate(Sender: TObject);
    procedure actDeleteOperationExecute(Sender: TObject);
    procedure actNewMethodExecute(Sender: TObject);
    procedure actOKExecute(Sender: TObject);
    procedure actOKUpdate(Sender: TObject);
    procedure actUpdateOperationExecute(Sender: TObject);
    procedure actUpdateOperationUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure trvMethodsDblClick(Sender : TObject);
  private
    FUpdateType : TEditType;
    FObject : TPasClassType;
    FSymbolTable : TwstPasTreeContainer;
  private
    property UpdateType : TEditType read FUpdateType;
  private
    procedure LoadMethod(AMthDef : TPasProcedure);
    procedure LoadFromObject();
    procedure SaveToObject();
  public
    function UpdateObject(
      var   AObject     : TPasClassType;
      const AUpdateType : TEditType;
            ASymbolTable : TwstPasTreeContainer
    ):Boolean;
  end; 

var
  fInterfaceEdit: TfInterfaceEdit;

implementation

{$R *.lfm}

uses view_helper, parserutils, udm;

{ TfInterfaceEdit }

procedure TfInterfaceEdit.actOKUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := not IsStrEmpty(ExtractIdentifier(edtName.Text));
end;

procedure TfInterfaceEdit.actUpdateOperationExecute(Sender: TObject);
var
  node, newNode : TTreeNode;
  locObj : TPasProcedure;
begin
  node := trvMethods.Selected;
  locObj := TPasProcedure(node.Data);
  edit_helper.UpdateObject(TPasElement(locObj),FSymbolTable);
  trvMethods.BeginUpdate();
  try
    newNode := FindPainter(locObj).Paint(FSymbolTable,locObj,node.Parent);
    newNode.MoveTo(node,naInsert);
    node.Free();
  finally
    trvMethods.EndUpdate();
  end;
end;

procedure TfInterfaceEdit.actUpdateOperationUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled :=
    ( trvMethods.Selected <> nil ) and
    ( trvMethods.Selected.Data <> nil ) and
    ( TPasElement(trvMethods.Selected.Data).InheritsFrom(TPasProcedure) );
end;

procedure TfInterfaceEdit.FormCreate(Sender: TObject);
begin
  trvMethods.Images := DM.IM;
end;

procedure TfInterfaceEdit.trvMethodsDblClick(Sender : TObject);
begin
  if actUpdateOperation.Enabled then begin
    actUpdateOperation.Execute();
  end else if actNewMethod.Enabled then begin
    actNewMethod.Execute();
  end;
end;

procedure TfInterfaceEdit.actOKExecute(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TfInterfaceEdit.actNewMethodExecute(Sender: TObject);
var
  prp : TPasProcedure;
begin
  prp := CreateMethod(FObject,FSymbolTable);
  if Assigned(prp) then begin
    LoadMethod(prp);
  end;
end;

procedure TfInterfaceEdit.actDeleteOperationExecute(Sender: TObject);
var
  node : TTreeNode;
  locObj : TPasProcedure;
begin
  node := trvMethods.Selected;
  locObj := TPasProcedure(node.Data);
  node.Data := nil;
  trvMethods.BeginUpdate();
  try
    node.Free();
    (locObj.Parent as TPasClassType).Members.Extract(locObj);
    locObj.Release();
  finally
    trvMethods.EndUpdate();
  end;
end;

procedure TfInterfaceEdit.actBindingEditUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled :=
    ( trvMethods.Selected <> nil ) and
    ( trvMethods.Selected.Data <> nil ) and
    ( TPasElement(trvMethods.Selected.Data).InheritsFrom(TwstBinding) );
end;

procedure TfInterfaceEdit.actBindingEditExecute(Sender: TObject);
var
  node, newNode : TTreeNode;
  locObj : TwstBinding;
begin
  node := trvMethods.Selected;
  locObj := TwstBinding(node.Data);
  edit_helper.UpdateObject(TPasElement(locObj),FSymbolTable);
  trvMethods.BeginUpdate();
  try
    newNode := FindPainter(locObj).Paint(FSymbolTable,locObj,node.Parent);
    newNode.MoveTo(node,naInsert);
    node.Free();
  finally
    trvMethods.EndUpdate();
  end;
end;

procedure TfInterfaceEdit.LoadMethod(AMthDef: TPasProcedure);
var
  topNode : TTreeNode;
begin
  topNode := trvMethods.Items[0];
  FindPainter(AMthDef).Paint(FSymbolTable,AMthDef,topNode);
end;

procedure TfInterfaceEdit.LoadFromObject();
var
  i : Integer;
  mthd : TPasProcedure;
  extName : string;
  bindingsNode : TTreeNode;
  b : TwstBinding;
begin
  edtName.Text := '';
  trvMethods.BeginUpdate();
  try
    trvMethods.Items.Clear();
    trvMethods.Items.AddFirst(nil,'Methods');
    if Assigned(FObject) then begin
      extName := FSymbolTable.GetExternalName(FObject);
      Self.Caption := extName;
      edtName.Text := extName;
      for i := 0 to Pred(FObject.Members.Count) do begin
        if TPasElement(FObject.Members[i]).InheritsFrom(TPasProcedure) then begin
          mthd := TPasProcedure(FObject.Members[i]);
          LoadMethod(mthd);
        end;
      end;
    end else begin
      Self.Caption := 'New';
    end;

    i := 0;
    bindingsNode := trvMethods.Items.AddChild(nil,'Bindings');
      while True do begin
        b := FSymbolTable.FindBinding(FObject,i);
        if ( b = nil ) then
          Break;
        Inc(i);
        FindPainter(b).Paint(FSymbolTable,b,bindingsNode);
      end;
    trvMethods.Items[0].Expand(False);
  finally
    trvMethods.EndUpdate();
  end;
end;

procedure TfInterfaceEdit.SaveToObject();
var
  typExtName, typIntName : string;
  b : TwstBinding;
begin
  typExtName := ExtractIdentifier(edtName.Text);
  typIntName := MakeInternalSymbolNameFrom(typExtName);
  FObject.Name := typIntName;
  FSymbolTable.RegisterExternalAlias(FObject,typExtName);
  b := FSymbolTable.FindBinding(FObject,0);
  if ( b = nil ) then begin
    FSymbolTable.AddBinding(Format('%sBinding',[typExtName]),FObject).BindingStyle := bsRPC;
  end else begin
    b.Name := Format('%sBinding',[typExtName]);
  end
end;

function TfInterfaceEdit.UpdateObject(
  var   AObject      : TPasClassType;
  const AUpdateType  : TEditType;
        ASymbolTable : TwstPasTreeContainer
): Boolean;
var
  intName : string;
  i : Integer;
  b : TwstBinding;
  g : TGuid;
begin
  Assert(Assigned(ASymbolTable));
  FSymbolTable := ASymbolTable;
  FUpdateType := AUpdateType;
  FObject := AObject;
  if ( FUpdateType = etCreate ) then begin
    i := 1;
    intName := 'ISampleService';
    while ( FSymbolTable.FindElementInModule(intName,FSymbolTable.CurrentModule) <> nil ) do begin
      intName := 'ISampleService' + IntToStr(i);
      Inc(i);
    end;
    FObject := TPasClassType(FSymbolTable.CreateElement(TPasClassType,intName,FSymbolTable.CurrentModule.InterfaceSection,visDefault,'',0));
    FObject.ObjKind := okInterface;
    if ( CreateGUID(g) = 0 ) then begin
{$IFDEF HAS_EXP_TREE}
      FreeAndNil(FObject.GUIDExpr);
      FObject.GUIDExpr:=TPrimitiveExpr.Create(FObject,pekString,GUIDToString(g));
{$ELSE HAS_EXP_TREE}
      FObject.InterfaceGUID := GUIDToString(g);
{$ENDIF HAS_EXP_TREE}
    end;
    FSymbolTable.CurrentModule.InterfaceSection.Declarations.Add(FObject);
    FSymbolTable.CurrentModule.InterfaceSection.Types.Add(FObject);
    FSymbolTable.AddBinding(Format('%sBinding',[FObject.Name]),FObject).BindingStyle := bsRPC;
  end;
  try
    LoadFromObject();
    ActiveControl := edtName;
    Result := ( ShowModal() = mrOK );
    if Result then begin
      SaveToObject();
      if ( AUpdateType = etCreate ) then begin
        AObject := FObject;
      end;
    end;
  except
    if ( FUpdateType = etCreate ) then begin
      b := FSymbolTable.FindBinding(FObject,0);
      if ( b <> nil ) then
        FSymbolTable.DeleteBinding(b);
      FSymbolTable.CurrentModule.InterfaceSection.Declarations.Extract(FObject);
      FSymbolTable.CurrentModule.InterfaceSection.Types.Extract(FObject);
      FObject.Release();
      AObject := nil;
    end;
    raise;
  end;
end;

end.

