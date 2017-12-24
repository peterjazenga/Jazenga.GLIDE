{
    This file is part of the Web Service Toolkit
    Copyright (c) 2007 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
unit ufrecordedit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, ActnList, Menus, Contnrs,
  pastree, pascal_parser_intf,
  edit_helper, SynHighlighterXML, SynEdit;

type

  { TfRecordEdit }

  TfRecordEdit = class(TForm)
    actApply : TAction;
    actMoveUp: TAction;
    actMoveDown: TAction;
    actPropAdd : TAction;
    actPropEdit : TAction;
    actPropDelete : TAction;
    actOK : TAction;
    ActionList1 : TActionList;
    Button1 : TButton;
    Button2 : TButton;
    Button3 : TButton;
    Button4 : TButton;
    Button5 : TButton;
    Button6 : TButton;
    Button7: TButton;
    Button8: TButton;
    edtDocumentation : TMemo;
    edtName : TEdit;
    edtSourceXSD : TSynEdit;
    GroupBox1 : TGroupBox;
    Label1 : TLabel;
    edtFields : TListView;
    MenuItem1 : TMenuItem;
    MenuItem2 : TMenuItem;
    MenuItem3 : TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    PC : TPageControl;
    Panel1 : TPanel;
    PopupMenu1 : TPopupMenu;
    SynXMLSyn1 : TSynXMLSyn;
    TabSheet1 : TTabSheet;
    tsDocumentation : TTabSheet;
    tsDependencies : TTabSheet;
    tsSourceXSD : TTabSheet;
    tvDependency : TTreeView;
    procedure actApplyExecute(Sender : TObject);
    procedure actMoveDownExecute(Sender: TObject);
    procedure actMoveDownUpdate(Sender: TObject);
    procedure actMoveUpExecute(Sender: TObject);
    procedure actMoveUpUpdate(Sender: TObject);
    procedure actOKExecute(Sender : TObject);
    procedure actOKUpdate(Sender : TObject);
    procedure actPropAddExecute(Sender : TObject);
    procedure actPropDeleteExecute(Sender : TObject);
    procedure actPropEditExecute(Sender : TObject);
    procedure actPropEditUpdate(Sender : TObject);
    procedure edtFieldsDblClick(Sender : TObject);
    procedure PCChange(Sender : TObject);
  private
    FUpdateType : TEditType;
    FObject : TPasRecordType;
    FSymbolTable : TwstPasTreeContainer;
    FApplied : Boolean;
    FDependencyList : TObjectList;
  private
    property UpdateType : TEditType read FUpdateType;
  private
    procedure MovePropertyItem(AItem : TPasVariable; const ANewIndex : Integer);
    function LoadField(AFieldDef : TPasVariable; const AIndex : Integer) : TListItem;
    procedure LoadFromObject();
    procedure SaveToObject();
    
    procedure ShowSourceXSD();
    procedure ShowDependencies();
    procedure ShowDocumentation();
  public
    destructor Destroy();override;
    function UpdateObject(
      var   AObject     : TPasRecordType;
      const AUpdateType : TEditType;
            ASymbolTable : TwstPasTreeContainer
    ):Boolean;
  end; 

var
  fRecordEdit : TfRecordEdit;

implementation

{$R *.lfm}

uses common_gui_utils, parserutils, ufpropedit, view_helper, xsd_consts;

{ TfRecordEdit }

procedure TfRecordEdit.actOKUpdate(Sender : TObject);
begin
  TAction(Sender).Enabled := not IsStrEmpty(ExtractIdentifier(edtName.Text));
end;

procedure TfRecordEdit.actPropAddExecute(Sender : TObject);
var
  prp : TPasVariable;
begin
  prp := CreateProperty(FObject,FSymbolTable);
  if Assigned(prp) then begin
    LoadField(prp,-1);
  end;
end;

procedure TfRecordEdit.actPropDeleteExecute(Sender : TObject);
var
  prop : TPasVariable;
begin
  prop := TPasVariable(edtFields.ItemFocused.Data);
  FObject.Members.Extract(prop);
  prop.Release();
  edtFields.ItemFocused.Free();
end;

procedure TfRecordEdit.actPropEditExecute(Sender : TObject);
var
  prp : TPasVariable;
  itm : TListItem;
  oldPos : Integer;
begin
  itm := edtFields.ItemFocused;
  if Assigned(itm) then begin
    prp := TPasVariable(itm.Data);
    if UpdateProperty(prp,FSymbolTable) then begin
      oldPos := itm.Index;
      itm.Free();
      LoadField(prp,oldPos);
    end;
  end;
end;

procedure TfRecordEdit.actPropEditUpdate(Sender : TObject);
begin
  TAction(Sender).Enabled := Assigned(edtFields.ItemFocused);
end;

procedure TfRecordEdit.edtFieldsDblClick(Sender : TObject);
begin
  if actPropEdit.Enabled then begin
    actPropEdit.Execute();
  end else if actPropAdd.Enabled then begin
    actPropAdd.Execute();
  end;
end;

procedure TfRecordEdit.PCChange(Sender : TObject);
begin
  if ( PC.ActivePage = tsSourceXSD ) then begin
    if actApply.Enabled then
      actApply.Execute();
    ShowSourceXSD();
  end else if ( PC.ActivePage = tsDependencies ) then begin
    ShowDependencies();
  end else if ( PC.ActivePage = tsDocumentation ) then begin
    ShowDocumentation();
  end;
end;

procedure TfRecordEdit.MovePropertyItem(AItem: TPasVariable; const ANewIndex: Integer);

  function FindNewMemberPosition() : Integer;
  var
    k, kcounter : Integer;
    mlist : TList2;
  begin
    Result := 0;
    kcounter := 0;
    mlist := FObject.Members;
    for k := 0 to Pred(mlist.Count) do begin
      if TPasElement(mlist[k]).InheritsFrom(TPasVariable) then begin
        Inc(kcounter);
        if ( kcounter = ANewIndex ) then begin
          Result := k;
          Break;
        end;
      end;
    end;
  end;

var
  locItem : TListItem;
begin
  if ( AItem <> nil ) and
     ( ( ANewIndex >= 0 ) and ( ANewIndex < edtFields.Items.Count ) )
  then begin
    locItem := FindItem(FSymbolTable.GetExternalName(AItem),edtFields.Items);
    if ( locItem <> nil ) then
      locItem.Free();
    FObject.Members.Exchange(FObject.Members.IndexOf(AItem),FindNewMemberPosition());
    locItem := LoadField(AItem,ANewIndex);
    edtFields.ItemFocused := locItem;
    edtFields.Selected := locItem;
    FApplied := True;
  end;
end;

procedure TfRecordEdit.actOKExecute(Sender : TObject);
begin
  ModalResult := mrOk;
end;

procedure TfRecordEdit.actApplyExecute(Sender : TObject);
begin
  SaveToObject();
  if ( PC.ActivePage = tsSourceXSD ) then
    ShowSourceXSD();
end;

procedure TfRecordEdit.actMoveDownExecute(Sender: TObject);
begin
  MovePropertyItem(TPasVariable(edtFields.ItemFocused.Data),(edtFields.ItemFocused.Index + 1));
end;

procedure TfRecordEdit.actMoveDownUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(edtFields.ItemFocused) and ( edtFields.ItemFocused.Index < Pred(edtFields.Items.Count) );
end;

procedure TfRecordEdit.actMoveUpExecute(Sender: TObject);
begin
  MovePropertyItem(TPasVariable(edtFields.ItemFocused.Data),(edtFields.ItemFocused.Index - 1));
end;

procedure TfRecordEdit.actMoveUpUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(edtFields.ItemFocused) and ( edtFields.ItemFocused.Index > 0 );
end;

function TfRecordEdit.LoadField(AFieldDef : TPasVariable; const AIndex : Integer) : TListItem;
var
  itm : TListItem;
  s, extName : string;
begin
  extName := FSymbolTable.GetExternalName(AFieldDef);
  itm := FindItem(extName,edtFields.Items);
  if ( itm = nil ) then begin
    if ( AIndex >= 0 ) and ( AIndex < edtFields.Items.Count ) then
      itm := edtFields.Items.Insert(AIndex)
    else
      itm := edtFields.Items.Add();
  end;
  itm.Caption := extName;
  itm.SubItems.Add(FSymbolTable.GetExternalName(AFieldDef.VarType));
  itm.SubItems.Add(BOOL_STR[FSymbolTable.IsAttributeProperty(AFieldDef)]);
  itm.Data := AFieldDef;
  Result := itm;
end;

procedure TfRecordEdit.LoadFromObject();
var
  i : Integer;
  prp : TPasVariable;
  extName : string;
begin
  edtName.Text := '';
  edtFields.Clear();
  if Assigned(FObject) then begin
    extName := FSymbolTable.GetExternalName(FObject);
    Self.Caption := extName;
    edtName.Text := extName;
    for i := 0 to Pred(FObject.Members.Count) do begin
      if TPasElement(FObject.Members[i]).InheritsFrom(TPasVariable) then begin
        prp := TPasVariable(FObject.Members[i]);
        LoadField(prp,-1);
      end;
    end;
  end else begin
    Self.Caption := 'New';
  end;
end;

procedure TfRecordEdit.SaveToObject();
var
  typExtName, typIntName : string;
  locObj : TPasRecordType;
begin
  locObj := nil;
  typExtName := ExtractIdentifier(edtName.Text);
  typIntName := MakeInternalSymbolNameFrom(typExtName);
  locObj := FObject;
  locObj.Name := typIntName;
  FSymbolTable.RegisterExternalAlias(locObj,typExtName);
  FSymbolTable.Properties.GetList(FObject).Values[s_documentation] :=
    EncodeLineBreak(StringReplace(edtDocumentation.Lines.Text,sLineBreak,#10,[rfReplaceAll]));
  FApplied := True;
end;

procedure TfRecordEdit.ShowSourceXSD();
begin
  edtSourceXSD.Lines.Text := XsdGenerateSourceForObject(FObject,FSymbolTable);
end;

procedure TfRecordEdit.ShowDependencies();
begin
  if ( FDependencyList = nil ) then
    FDependencyList := TObjectList.Create(True);
  FDependencyList.Clear();
  FindDependencies(FObject,FSymbolTable,FDependencyList);
  DrawDependencies(tvDependency,FDependencyList);
end;

procedure TfRecordEdit.ShowDocumentation();
var
  props : TStrings;
begin
  props := FSymbolTable.Properties.FindList(FObject);
  if ( props <> nil ) then
    edtDocumentation.Lines.Text :=
      StringReplace(DecodeLineBreak(props.Values[s_documentation]),#10,sLineBreak,[rfReplaceAll]);
end;

destructor TfRecordEdit.Destroy();
begin
  FDependencyList.Free();
  inherited Destroy();
end;

function TfRecordEdit.UpdateObject(
  var   AObject      : TPasRecordType;
  const AUpdateType  : TEditType;
        ASymbolTable : TwstPasTreeContainer
) : Boolean;
begin
  Assert(Assigned(ASymbolTable));
  FSymbolTable := ASymbolTable;
  FUpdateType := AUpdateType;
  FObject := AObject;
  if ( UpdateType = etCreate ) and ( FObject = nil ) then begin
    FObject := TPasRecordType(FSymbolTable.CreateElement(TPasRecordType,'new_record',FSymbolTable.CurrentModule.InterfaceSection,visDefault,'',0));
    FSymbolTable.CurrentModule.InterfaceSection.Declarations.Add(FObject);
    FSymbolTable.CurrentModule.InterfaceSection.Types.Add(FObject);
  end;
  try
    LoadFromObject();
    ActiveControl := edtName;
    Result := FApplied or ( ShowModal() = mrOK );
    if Result then begin
      try
        SaveToObject();
        if ( AUpdateType = etCreate ) then begin
          AObject := FObject;
        end;
      except
        Result := False;
        raise;
      end;
    end;
  finally
    if ( not Result ) and ( UpdateType = etCreate ) and ( AObject = nil ) then begin
      FSymbolTable.CurrentModule.InterfaceSection.Declarations.Extract(FObject);
      FSymbolTable.CurrentModule.InterfaceSection.Types.Extract(FObject);
      FObject.Release();
      FObject := nil;
    end;
  end;
end;

end.

