unit ufarrayedit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, StdCtrls, ActnList, Buttons, Contnrs,
  pastree, pascal_parser_intf, edit_helper, SynHighlighterXML, SynEdit;

type

  { TfArrayEdit }

  TfArrayEdit = class(TForm)
    actApply : TAction;
    actOK : TAction;
    AL : TActionList;
    Button1 : TButton;
    Button2 : TButton;
    Button6 : TButton;
    edtCollection : TCheckBox;
    edtDocumentation : TMemo;
    edtEmbedded : TCheckBox;
    edtElementName : TEdit;
    edtElementType : TComboBox;
    edtName : TEdit;
    edtSourceXSD : TSynEdit;
    Label1 : TLabel;
    Label2 : TLabel;
    Label3 : TLabel;
    PC : TPageControl;
    Panel1 : TPanel;
    SynXMLSyn1 : TSynXMLSyn;
    TabSheet1 : TTabSheet;
    tsDocumentation : TTabSheet;
    tsDependencies : TTabSheet;
    tsSourceXSD : TTabSheet;
    tvDependency : TTreeView;
    procedure actApplyExecute(Sender : TObject);
    procedure actOKExecute(Sender : TObject);
    procedure actOKUpdate(Sender : TObject);
    procedure PCChange(Sender : TObject);
  private
    FUpdateType : TEditType;
    FObject : TPasArrayType;
    FSymbolTable : TwstPasTreeContainer;
    FApplied : Boolean;
    FDependencyList : TObjectList;
  private
    property UpdateType : TEditType read FUpdateType;
  private
    procedure LoadFromObject();
    procedure SaveToObject();

    procedure ShowSourceXSD();
    procedure ShowDependencies();
    procedure ShowDocumentation();
  public
    destructor Destroy();override;
    function UpdateObject(
      var   AObject      : TPasArrayType;
      const AUpdateType  : TEditType;
            ASymbolTable : TwstPasTreeContainer
    ) : Boolean;
  end;

var
  fArrayEdit : TfArrayEdit;

implementation

{$R *.lfm}

uses
  parserutils, view_helper, xsd_consts;

{ TfArrayEdit }

procedure TfArrayEdit.actOKUpdate(Sender : TObject);
var
  internalName : string;
begin
  internalName := ExtractIdentifier(edtName.Text);
  TAction(Sender).Enabled :=
    ( not IsStrEmpty(internalName) ) and
    ( not IsStrEmpty(ExtractIdentifier(edtElementName.Text)) ) and
    ( edtElementType.ItemIndex >= 0 ) ;
end;

procedure TfArrayEdit.PCChange(Sender : TObject);
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

procedure TfArrayEdit.actOKExecute(Sender : TObject);
var
  eltType : TPasType;
  ok : Boolean;
begin
  ok := True;
  if edtCollection.Checked then begin
    eltType := edtElementType.Items.Objects[edtElementType.ItemIndex] as TPasType;
    if eltType.InheritsFrom(TPasUnresolvedTypeRef) then
      eltType := FSymbolTable.FindElement(FSymbolTable.GetExternalName(eltType)) as TPasType;
    if eltType.InheritsFrom(TPasNativeSimpleType) or eltType.InheritsFrom(TPasNativeSimpleContentClassType) then begin
      ok := False;
      ShowMessage('Collections for simple types are not supported.');
    end;
  end;
  if ok then
    ModalResult := mrOK;
end;

procedure TfArrayEdit.actApplyExecute(Sender : TObject);
begin
  SaveToObject();
  if ( PC.ActivePage = tsSourceXSD ) then
    ShowSourceXSD();
end;

procedure TfArrayEdit.LoadFromObject();
begin
  edtElementType.Clear();
  edtElementType.Items.BeginUpdate();
  try
    edtElementType.Items.Clear();
    FillTypeList(edtElementType.Items,FSymbolTable);
  finally
    edtElementType.Items.EndUpdate();
  end;
  if Assigned(FObject) then begin
    Self.Caption := FSymbolTable.GetExternalName(FObject);
    edtName.Text := FSymbolTable.GetExternalName(FObject);
    edtElementName.Text := FSymbolTable.GetArrayItemExternalName(FObject);
    if ( FObject.ElType <> nil ) and ( not FObject.ElType.InheritsFrom(TPasUnresolvedTypeRef) ) then
      edtElementType.ItemIndex := edtElementType.Items.IndexOfObject(FObject.ElType);
    if ( edtElementType.ItemIndex < 0 ) then
      edtElementType.ItemIndex := edtElementType.Items.IndexOf(FSymbolTable.GetExternalName(FObject.ElType));
    edtEmbedded.Checked := ( FSymbolTable.GetArrayStyle(FObject) = asEmbeded );
    edtCollection.Checked:= FSymbolTable.IsCollection(FObject);
  end else begin
    Self.Caption := 'NewArray';
  end;
end;

procedure TfArrayEdit.SaveToObject();
var
  locObj : TPasArrayType;
  typExtName, typIntName : string;
  eltExtName, eltIntName : string;
  eltType : TPasType;
  arrStyle : TArrayStyle;
begin
  locObj := nil;
  typExtName := ExtractIdentifier(edtName.Text);
  typIntName := MakeInternalSymbolNameFrom(typExtName);
  eltExtName := ExtractIdentifier(edtElementName.Text);
  eltIntName := MakeInternalSymbolNameFrom(eltExtName);
  if edtEmbedded.Checked then
    arrStyle := asEmbeded
  else
    arrStyle := asScoped;
  eltType := edtElementType.Items.Objects[edtElementType.ItemIndex] as TPasType;
  if ( UpdateType = etCreate ) and ( FObject = nil ) then begin
    locObj := FSymbolTable.CreateArray(typIntName,eltType,eltExtName,eltIntName,arrStyle);
    FSymbolTable.CurrentModule.InterfaceSection.Declarations.Add(locObj);
    FSymbolTable.CurrentModule.InterfaceSection.Types.Add(locObj);
    FSymbolTable.CurrentModule.InterfaceSection.Classes.Add(locObj);
    FreeAndNil(FObject);
    FObject := locObj;
  end else begin
    locObj := FObject;
    if ( eltType <> locObj.ElType ) then begin
      if ( locObj.ElType <> nil ) then
        locObj.ElType.Release();
      locObj.ElType := eltType;
      locObj.ElType.AddRef();
    end;
    locObj.Name := typIntName;
    FSymbolTable.SetArrayStyle(locObj,arrStyle);
    FSymbolTable.SetArrayItemExternalName(locObj,eltExtName);
  end;
  if ( edtCollection.Checked <> FSymbolTable.IsCollection(FObject) ) then
    FSymbolTable.SetCollectionFlag(FObject,edtCollection.Checked);
  FSymbolTable.RegisterExternalAlias(locObj,typExtName);
  FSymbolTable.Properties.GetList(FObject).Values[s_documentation] :=
    EncodeLineBreak(StringReplace(edtDocumentation.Lines.Text,sLineBreak,#10,[rfReplaceAll]));
  FApplied := True;
end;

procedure TfArrayEdit.ShowSourceXSD();
begin
  edtSourceXSD.Lines.Text := XsdGenerateSourceForObject(FObject,FSymbolTable);
end;

procedure TfArrayEdit.ShowDependencies();
begin
  if ( FDependencyList = nil ) then
    FDependencyList := TObjectList.Create(True);
  FDependencyList.Clear();
  FindDependencies(FObject,FSymbolTable,FDependencyList);
  DrawDependencies(tvDependency,FDependencyList);
end;

procedure TfArrayEdit.ShowDocumentation();
var
  props : TStrings;
begin
  props := FSymbolTable.Properties.FindList(FObject);
  if ( props <> nil ) then
    edtDocumentation.Lines.Text :=
      StringReplace(DecodeLineBreak(props.Values[s_documentation]),#10,sLineBreak,[rfReplaceAll]);
end;

destructor TfArrayEdit.Destroy();
begin
  FDependencyList.Free();
  inherited Destroy();
end;

function TfArrayEdit.UpdateObject(
  var   AObject : TPasArrayType;
  const AUpdateType : TEditType;
        ASymbolTable : TwstPasTreeContainer
) : Boolean;
begin
  FSymbolTable := ASymbolTable;
  FUpdateType := AUpdateType;
  FObject := AObject;
  LoadFromObject();
  ActiveControl := edtName;
  Result := ( ShowModal() = mrOK );
  if Result or FApplied then begin
    SaveToObject();
    if ( AUpdateType = etCreate ) then begin
      AObject := FObject;
    end;
  end;
end;

end.

