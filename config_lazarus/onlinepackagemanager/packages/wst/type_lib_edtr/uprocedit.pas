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
unit uprocedit;

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, ActnList, Menus,
  pastree, PScanner, pascal_parser_intf,
  edit_helper, Buttons;

type

  { TfProcEdit }

  TfProcEdit = class(TForm)
    actDeleteArgument: TAction;
    actUpdateArgument: TAction;
    actNewArg: TAction;
    actOk: TAction;
    AL: TActionList;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    edtFunction: TCheckBox;
    edtResultType: TComboBox;
    edtName: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    edtParams: TListView;
    PageControl1: TPageControl;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    TabSheet1: TTabSheet;
    procedure actDeleteArgumentExecute(Sender: TObject);
    procedure actNewArgExecute(Sender: TObject);
    procedure actOkExecute(Sender: TObject);
    procedure actOkUpdate(Sender: TObject);
    procedure actUpdateArgumentExecute(Sender: TObject);
    procedure actUpdateArgumentUpdate(Sender: TObject);
    procedure edtFunctionClick(Sender: TObject);
    procedure edtFunctionEditingDone(Sender: TObject);
    procedure edtParamsDblClick(Sender : TObject);
  private
    FUpdateType : TEditType;
    FObject : TPasProcedure;
    FSymbolTable : TwstPasTreeContainer;
    FOldReturnType : TPasType;
  private
    property UpdateType : TEditType read FUpdateType;
  private
    procedure PrepareResultCombo();
    procedure LoadArgument(AArg : TPasArgument);
    procedure LoadFromObject();
    procedure SaveToObject();
  public
    function UpdateObject(
      var   AObject     : TPasProcedure;
      const AUpdateType : TEditType;
            ASymbolTable : TwstPasTreeContainer
    ):Boolean;
  end; 

var
  fProcEdit: TfProcEdit;

implementation

{$R *.lfm}

uses parserutils, common_gui_utils;

{ TfProcEdit }

procedure TfProcEdit.actOkExecute(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TfProcEdit.actNewArgExecute(Sender: TObject);
var
  prp : TPasArgument;
begin
  prp := CreateArgument(FObject.ProcType,FSymbolTable);
  if Assigned(prp) then begin
    if ( FObject.ProcType.Args.IndexOf(prp) = -1 ) then begin
      FObject.ProcType.Args.Add(prp);
    end;
    LoadArgument(prp);
  end;
end;

procedure TfProcEdit.actDeleteArgumentExecute(Sender: TObject);
var
  prop : TPasArgument;
begin
  prop := TPasArgument(edtParams.Items[edtParams.ItemIndex].Data);
  FObject.ProcType.Args.Extract(prop);
  prop.Release();
  edtParams.Items[edtParams.ItemIndex].Free();
end;

procedure TfProcEdit.actOkUpdate(Sender: TObject);
var
  b : Boolean;
  i : Integer;
  locProc : TPasProcedure;
  memberList : TList2;
  locName : string;
begin
  locName := edtName.Text;
  b := ( not IsStrEmpty(locName) ) and
       ( ( not edtFunction.Checked ) or ( edtResultType.ItemIndex > -1 ) );
  if b then begin
    memberList := TPasClassType(FObject.Parent).Members;
    for i := 0 to Pred(memberList.Count) do begin
      if TPasElement(memberList[i]).InheritsFrom(TPasProcedure) then begin
        locProc := TPasProcedure(memberList[i]);
        if ( locProc <> FObject ) and
           ( AnsiSameText(locProc.Name,locName) or
             ( ( Self.UpdateType = etUpdate ) and
               AnsiSameText(FSymbolTable.GetExternalName(locProc),locName)
             )
           )
        then begin
          b := False;
          Break;
        end;
      end;
    end;
  end;
  TAction(Sender).Enabled := b;
end;

procedure TfProcEdit.actUpdateArgumentExecute(Sender: TObject);
var
  prp : TPasArgument;
  itm : TListItem;
begin
  itm := edtParams.Items[edtParams.ItemIndex];
  if Assigned(itm) then begin
    prp := TPasArgument(itm.Data);
    if edit_helper.UpdateObject(TPasElement(prp),FSymbolTable) then begin
      itm.Free();
      LoadArgument(prp);
    end;
  end;
end;

procedure TfProcEdit.actUpdateArgumentUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (edtParams.ItemIndex >= 0);
end;

procedure TfProcEdit.edtFunctionClick(Sender: TObject);
begin
  //edtResultType.Enabled := edtFunction.Checked;
end;

procedure TfProcEdit.edtFunctionEditingDone(Sender: TObject);
begin
  edtResultType.Enabled := edtFunction.Checked;
end;

procedure TfProcEdit.edtParamsDblClick(Sender : TObject);
begin
  if actUpdateArgument.Enabled then begin
    actUpdateArgument.Execute();
  end else if actNewArg.Enabled then begin
    actNewArg.Execute();
  end;
end;

procedure TfProcEdit.PrepareResultCombo();
begin
  edtResultType.Items.BeginUpdate();
  try
    FillTypeList(edtResultType.Items,FSymbolTable);
  finally
    edtResultType.Items.EndUpdate();
  end;
end;

procedure TfProcEdit.LoadArgument(AArg: TPasArgument);
var
  itm : TListItem;
  extName : string;
begin
  extName := FSymbolTable.GetExternalName(AArg);
  itm := FindItem(extName,edtParams.Items);
  if ( itm = nil ) then begin
    itm := edtParams.Items.Add();
  end;
  itm.Caption := extName;
  itm.SubItems.Add(FSymbolTable.GetExternalName(AArg.ArgType));
  itm.SubItems.Add(AccessNames[AArg.Access]);
  itm.Data := AArg;
end;

procedure TfProcEdit.LoadFromObject();
var
  i : Integer;
  prp : TPasArgument;
  extName : string;
  argList : TList2;
begin
  edtName.Text := '';
  edtParams.Clear();
  if Assigned(FObject) then begin
    extName := FSymbolTable.GetExternalName(FObject);
    Self.Caption := extName;
    edtName.Text := extName;
    edtFunction.Checked := FObject.InheritsFrom(TPasFunction);
    edtResultType.Enabled := edtFunction.Checked;
    if FObject.InheritsFrom(TPasFunction) then begin
      edtResultType.ItemIndex := edtResultType.Items.IndexOfObject(TPasFunctionType(FObject.ProcType).ResultEl.ResultType);
    end;
    argList := FObject.ProcType.Args;
    for i := 0 to Pred(argList.Count) do begin
      prp := TPasArgument(argList[i]);
      LoadArgument(prp);
    end;
  end else begin
    Self.Caption := 'New';
  end;
end;

procedure TfProcEdit.SaveToObject();

  procedure CopyArgs(AFrom,ATo  : TPasProcedureType);
  var
    argList : TList2;
    arg, newArg : TPasArgument;
    k, ck : Integer;
  begin
    argList := AFrom.Args;
    ck := argList.Count;
    for k := 0 to Pred(ck) do begin
      arg := TPasArgument(argList[k]);
      newArg := TPasArgument(FSymbolTable.CreateElement(TPasArgument,arg.Name,ATo,visDefault,'',0));
      ATo.Args.Add(newArg);
      newArg.Access := arg.Access;
      newArg.ArgType := arg.ArgType;
        newArg.ArgType.AddRef();
      //newArg.Value := arg.Value;
    end;
  end;
  
  procedure CreateFunctionObject();
  var
    prt : TPasClassType;
    newObj : TPasFunction;
    newObjType : TPasFunctionType;
  begin
    prt := FObject.Parent as TPasClassType;
    prt.Members.Extract(FObject);
    try
      newObj := TPasFunction(FSymbolTable.CreateElement(TPasFunction,FObject.Name,prt,visPublic,'',0));
      prt.Members.Add(newObj);
  {$IFDEF WST_TPASSOURCEPOS}
      newObjType := FSymbolTable.CreateFunctionType('','result',newObj,False,Default(TPasSourcePos));
  {$ELSE WST_TPASSOURCEPOS}
      newObjType := FSymbolTable.CreateFunctionType('','result',newObj,False,'',0);
  {$ENDIF WST_TPASSOURCEPOS}
      newObj.ProcType := newObjType;
      CopyArgs(FObject.ProcType,newObjType);
      newObjType.ResultEl.ResultType := edtResultType.Items.Objects[edtResultType.ItemIndex] as TPasType;
      newObjType.ResultEl.ResultType.AddRef();
    finally
      FObject.Release();
      FObject := newObj;
    end;
  end;

  procedure CreateProcObject();
  var
    prt : TPasClassType;
    newObj : TPasProcedure;
    newObjType : TPasProcedureType;
  begin
    prt := FObject.Parent as TPasClassType;
    prt.Members.Extract(FObject);
    try
      newObj := TPasProcedure(FSymbolTable.CreateElement(TPasProcedure,FObject.Name,prt,visPublic,'',0));
      prt.Members.Add(newObj);
      newObjType := TPasProcedureType(FSymbolTable.CreateElement(TPasProcedureType,'',newObj,visDefault,'',0));
      newObj.ProcType := newObjType;
      CopyArgs(FObject.ProcType,newObjType);
    finally
      FObject.Release();
      FObject := newObj;
    end;
  end;
  
  procedure CheckObjectType();
  var
    newRetType : TPasType;
  begin
    if FObject.InheritsFrom(TPasFunction) then begin
      newRetType := edtResultType.Items.Objects[edtResultType.ItemIndex] as TPasType;
      if ( FOldReturnType <> nil ) and
         ( FOldReturnType <> newRetType )
      then begin
        FOldReturnType.Release();
        TPasFunctionType(FObject.ProcType).ResultEl.ResultType := newRetType;
        newRetType.AddRef();
      end;
    end;
    if edtFunction.Checked and ( not FObject.InheritsFrom(TPasFunction) ) then begin
      CreateFunctionObject();
    end else if ( not edtFunction.Checked ) and ( FObject.InheritsFrom(TPasFunction) ) then begin
      CreateProcObject();
    end;
  end;
  
var
  typExtName, typIntName : string;
  locObj : TPasProcedure;
begin
  CheckObjectType();
  typExtName := ExtractIdentifier(edtName.Text);
  typIntName := MakeInternalSymbolNameFrom(typExtName);
  locObj := FObject;
  locObj.Name := typIntName;
  FSymbolTable.RegisterExternalAlias(locObj,typExtName);
end;

function TfProcEdit.UpdateObject(
  var   AObject      : TPasProcedure;
  const AUpdateType  : TEditType;
        ASymbolTable : TwstPasTreeContainer
): Boolean;
begin
  Assert(Assigned(ASymbolTable));
  Assert(Assigned(AObject));
  FSymbolTable := ASymbolTable;
  FUpdateType := AUpdateType;
  FObject := AObject;
  if FObject.InheritsFrom(TPasFunction) then
    FOldReturnType := TPasFunctionType(FObject.ProcType).ResultEl.ResultType;
  PrepareResultCombo();
  LoadFromObject();
  ActiveControl := edtName;
  Result := ( ShowModal() = mrOK );
  if Result then begin
    try
      SaveToObject();
      AObject := FObject;
    except
      Result := False;
      raise;
    end;
  end;
end;

end.

