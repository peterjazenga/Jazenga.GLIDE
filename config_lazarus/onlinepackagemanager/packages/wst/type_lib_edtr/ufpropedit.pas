{
    This file is part of the Web Service Toolkit
    Copyright (c) 2007 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
unit ufpropedit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ActnList,
  ExtCtrls, Buttons, ComCtrls, StdCtrls,
  pastree, pascal_parser_intf,
  edit_helper;

type

  { TfPropEdit }

  TfPropEdit = class(TForm)
    ActionList1: TActionList;
    actOK: TAction;
    Button1: TButton;
    Button2: TButton;
    Button4: TButton;
    edtOptional : TCheckBox;
    edtAttribute: TCheckBox;
    edtType: TComboBox;
    edtName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    TabSheet1: TTabSheet;
    procedure actOKExecute(Sender: TObject);
    procedure actOKUpdate(Sender: TObject);
  private
    FClassObject: TPasType;
    FUpdateType : TEditType;
    FObject : TPasVariable;
    FSymbolTable : TwstPasTreeContainer;
  private
    property UpdateType : TEditType read FUpdateType;
    property ClassObject : TPasType read FClassObject;
  private
    procedure LoadFromObject();
    procedure SaveToObject();
    function GetMembers() : TList2;
    function IsClassType() : Boolean;
  public
    function UpdateObject(
      var   AObject      : TPasVariable;
      const AUpdateType  : TEditType;
            ASymbolTable : TwstPasTreeContainer
    ) : Boolean;
  end; 

var
  fPropEdit: TfPropEdit;

  function CreateProperty(AClass : TPasType; ASymboltable : TwstPasTreeContainer):TPasVariable ;
  function UpdateProperty(AProp : TPasVariable; ASymboltable : TwstPasTreeContainer):Boolean;
  
implementation

{$R *.lfm}

uses parserutils;

function CreateProperty(AClass : TPasType; ASymboltable : TwstPasTreeContainer):TPasVariable ;
var
  f : TfPropEdit;
begin
  if ( AClass = nil ) or
     ( not ( AClass.InheritsFrom(TPasClassType) or AClass.InheritsFrom(TPasRecordType) ) )
  then begin
    raise Exception.Create('Class or record expected.');
  end;
  Result := nil;
  f := TfPropEdit.Create(Application);
  try
    f.FClassObject := AClass;
    f.UpdateObject(Result,etCreate,ASymboltable);
  finally
    f.Release();
  end;
end;

function UpdateProperty(AProp : TPasVariable; ASymboltable : TwstPasTreeContainer):Boolean;
var
  f : TfPropEdit;
begin
  f := TfPropEdit.Create(Application);
  try
    f.FClassObject := AProp.Parent as TPasType;
    Result := f.UpdateObject(AProp,etUpdate,ASymboltable);
  finally
    f.Release();
  end;
end;

{ TfPropEdit }

procedure TfPropEdit.actOKUpdate(Sender: TObject);
var
  internalName : string;
begin
  internalName := ExtractIdentifier(edtName.Text);
  TAction(Sender).Enabled :=
    ( not IsStrEmpty(internalName) ) and
    ( edtType.ItemIndex >= 0 ) and
    ( ( UpdateType = etUpdate ) or
      ( ( ClassObject.InheritsFrom(TPasClassType) and ( FindMember(TPasClassType(ClassObject),internalName) = nil ) ) or
        ( ClassObject.InheritsFrom(TPasRecordType) and ( FindMember(TPasRecordType(ClassObject),internalName) = nil ) )
      )
    );
end;

procedure TfPropEdit.actOKExecute(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TfPropEdit.LoadFromObject();
var
  i : Integer;
  ls : TStrings;
begin
  edtName.Text := '';
  edtType.Clear();
  if Assigned(FClassObject) and
     FClassObject.InheritsFrom(TPasClassType) and
     Assigned(TPasClassType(FClassObject).AncestorType) and
     TPasClassType(FClassObject).AncestorType.InheritsFrom(TPasNativeSimpleContentClassType)
  then begin
    edtAttribute.Checked := True;
    edtAttribute.Enabled := False;
  end;
  edtType.Items.BeginUpdate();
  try
    edtType.Items.Clear();
    FillTypeList(edtType.Items,FSymbolTable);
    ls := edtType.Items;
    if IsClassType() then begin
      i := ls.Count - 1;
      while ( i > 0 ) do begin
        if ls.Objects[i].InheritsFrom(TPasRecordType) then begin
          ls.Delete(i);
        end;
        Dec(i);
      end;
    end;
  finally
    edtType.Items.EndUpdate();
  end;
  if Assigned(FObject) then begin
    Self.Caption := FSymbolTable.GetExternalName(FObject);
    edtName.Text := FSymbolTable.GetExternalName(FObject);
    edtType.ItemIndex := edtType.Items.IndexOfObject(FObject.VarType);
    edtAttribute.Checked := FSymbolTable.IsAttributeProperty(FObject);
    if IsClassType() then begin
      edtOptional.Checked := (AnsiPos(sWST_PROP_STORE_PREFIX,TPasProperty(FObject).StoredAccessorName) = 1) ;
    end else begin
      edtOptional.Checked := True;
      edtOptional.Enabled := False;
    end;
  end else begin
    Self.Caption := 'New';
  end;
end;

procedure TfPropEdit.SaveToObject();
var
  locObj : TPasVariable;
  locObjProp : TPasProperty;
  typExtName, typIntName : string;
  propType : TPasType;
  eltClass : TPTreeElement;
  locIsClass : Boolean;
begin
  locObj := nil;
  locIsClass := IsClassType();
  if locIsClass then
    eltClass := TPasProperty
  else
    eltClass := TPasVariable;
  typExtName := ExtractIdentifier(edtName.Text);
  typIntName := MakeInternalSymbolNameFrom(typExtName);
  propType := edtType.Items.Objects[edtType.ItemIndex] as TPasType;
  if ( UpdateType = etCreate ) then begin
    locObj := TPasVariable(FSymbolTable.CreateElement(eltClass,typIntName,ClassObject,visPublished,'',0));
    FreeAndNil(FObject);
    FObject := locObj;
    locObj.VarType := propType;
    locObj.VarType.AddRef();
    GetMembers().Add(FObject);
  end else begin
    locObj := FObject;
    if ( propType <> locObj.VarType ) then begin
      if ( locObj.VarType <> nil ) then
        locObj.VarType.Release();
      locObj.VarType := propType;
      locObj.VarType.AddRef();
    end;
    locObj.Name := typIntName;
  end;
  if IsClassType() then begin
    locObjProp := locObj as TPasProperty;
    if edtOptional.Checked then
      locObjProp.StoredAccessorName := sWST_PROP_STORE_PREFIX + locObjProp.Name
    else
      locObjProp.StoredAccessorName := 'True';
    locObjProp.ReadAccessorName := 'F' + locObjProp.Name;
    locObjProp.WriteAccessorName := 'F' + locObjProp.Name;
  end;
  FSymbolTable.RegisterExternalAlias(locObj,typExtName);
  //if ( edtAttribute.Checked <> FSymbolTable.IsAttributeProperty(locObj) ) then
    FSymbolTable.SetPropertyAsAttribute(locObj,edtAttribute.Checked);
end;

function TfPropEdit.GetMembers() : TList2;
begin
  if ClassObject.InheritsFrom(TPasClassType) then
    Result := TPasClassType(ClassObject).Members
  else if ClassObject.InheritsFrom(TPasRecordType) then
    Result := TPasRecordType(ClassObject).Members
  else
    Result := nil;
end;

function TfPropEdit.IsClassType() : Boolean;
begin
  Result := ClassObject.InheritsFrom(TPasClassType);
end;

function TfPropEdit.UpdateObject(
  var   AObject       : TPasVariable;
  const AUpdateType   : TEditType;
        ASymbolTable  : TwstPasTreeContainer
): Boolean;
begin
  FSymbolTable := ASymbolTable;
  FUpdateType := AUpdateType;
  FObject := AObject;
  LoadFromObject();
  Self.ActiveControl := edtName;
  Result := ( ShowModal() = mrOK );
  if Result then begin
    SaveToObject();
    if ( AUpdateType = etCreate ) then begin
      AObject := FObject;
    end;
  end;
end;

end.

