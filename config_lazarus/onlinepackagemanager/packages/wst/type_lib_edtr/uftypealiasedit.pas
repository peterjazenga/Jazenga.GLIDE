unit uftypealiasedit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, StdCtrls, Buttons, Contnrs,
  pastree, pascal_parser_intf, edit_helper, ActnList, SynHighlighterXML,
  SynEdit;

type

  { TfTypeAliasEdit }

  TfTypeAliasEdit = class(TForm)
    actApply : TAction;
    actOK : TAction;
    AL : TActionList;
    Button1 : TButton;
    Button2 : TButton;
    Button3 : TButton;
    edtBaseType : TComboBox;
    edtDocumentation : TMemo;
    edtName : TEdit;
    edtSourceXSD : TSynEdit;
    Label1 : TLabel;
    Label2 : TLabel;
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
    FObject : TPasAliasType;
    FSymbolTable : TwstPasTreeContainer;
    FOldBaseType : TPasType;
    FApplied : Boolean;
    FDependencyList : TObjectList;
  private
    property UpdateType : TEditType read FUpdateType;
  private
    procedure PrepareParentCombo();
    procedure LoadFromObject();
    procedure SaveToObject();
    
    procedure ShowSourceXSD();
    procedure ShowDependencies();
    procedure ShowDocumentation();
  public
    destructor Destroy();override;
    function UpdateObject(
      var   AObject     : TPasAliasType;
      const AUpdateType : TEditType;
            ASymbolTable : TwstPasTreeContainer
    ):Boolean;

  end; 

var
  fTypeAliasEdit : TfTypeAliasEdit;

implementation

{$R *.lfm}

uses parserutils, view_helper, xsd_consts;

{ TfTypeAliasEdit }

procedure TfTypeAliasEdit.actOKUpdate(Sender : TObject);
begin
  TAction(actOK).Enabled := ( not IsStrEmpty(edtName.Text) ) and ( edtBaseType.ItemIndex >= 0 );
end;

procedure TfTypeAliasEdit.PCChange(Sender : TObject);
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

procedure TfTypeAliasEdit.actOKExecute(Sender : TObject);
begin
  ModalResult := mrOK;
end;

procedure TfTypeAliasEdit.actApplyExecute(Sender : TObject);
begin
  SaveToObject();
  if ( PC.ActivePage = tsSourceXSD ) then
    ShowSourceXSD();
end;

procedure TfTypeAliasEdit.PrepareParentCombo();
begin
  edtBaseType.Items.BeginUpdate();
  try
    edtBaseType.Items.Clear();
    FillTypeList(edtBaseType.Items,FSymbolTable);
  finally
    edtBaseType.Items.EndUpdate();
  end;
end;

procedure TfTypeAliasEdit.LoadFromObject();
var
  extName : string;
begin
  edtName.Text := '';
  if Assigned(FObject) then begin
    extName := FSymbolTable.GetExternalName(FObject);
    Self.Caption := extName;
    edtName.Text := extName;
    if Assigned(FObject.DestType) then begin
      edtBaseType.ItemIndex := edtBaseType.Items.IndexOfObject(FObject.DestType);
    end;
  end else begin
    Self.Caption := 'New';
  end;
end;

procedure TfTypeAliasEdit.SaveToObject();
var
  typExtName, typIntName : string;
  locObj : TPasAliasType;
  baseType : TPasType;
begin
  locObj := nil;
  typExtName := ExtractIdentifier(edtName.Text);
  typIntName := MakeInternalSymbolNameFrom(typExtName);

  if ( FObject = nil ) then begin
    FObject := TPasAliasType(FSymbolTable.CreateElement(TPasAliasType,typIntName,FSymbolTable.CurrentModule.InterfaceSection,visDefault,'',0));
    FSymbolTable.CurrentModule.InterfaceSection.Declarations.Add(FObject);
    FSymbolTable.CurrentModule.InterfaceSection.Types.Add(FObject);
    FSymbolTable.CurrentModule.InterfaceSection.Classes.Add(FObject);
  end;

  locObj := FObject;
  locObj.Name := typIntName;
  FSymbolTable.RegisterExternalAlias(locObj,typExtName);
  baseType := edtBaseType.Items.Objects[edtBaseType.ItemIndex] as TPasType;
  if ( baseType <> FOldBaseType ) then begin
    if ( FOldBaseType <> nil ) then
      FOldBaseType.Release();
    locObj.DestType := baseType;
    locObj.DestType.AddRef();
  end;
  FOldBaseType := locObj.DestType;
  FSymbolTable.Properties.GetList(FObject).Values[s_documentation] :=
    EncodeLineBreak(StringReplace(edtDocumentation.Lines.Text,sLineBreak,#10,[rfReplaceAll]));
  FApplied := True;
end;

procedure TfTypeAliasEdit.ShowSourceXSD();
begin
  edtSourceXSD.Lines.Text := XsdGenerateSourceForObject(FObject,FSymbolTable);
end;

procedure TfTypeAliasEdit.ShowDependencies();
begin
  if ( FDependencyList = nil ) then
    FDependencyList := TObjectList.Create(True);
  FDependencyList.Clear();
  FindDependencies(FObject,FSymbolTable,FDependencyList);
  DrawDependencies(tvDependency,FDependencyList);
end;

procedure TfTypeAliasEdit.ShowDocumentation();
var
  props : TStrings;
begin
  props := FSymbolTable.Properties.FindList(FObject);
  if ( props <> nil ) then
    edtDocumentation.Lines.Text :=
      StringReplace(DecodeLineBreak(props.Values[s_documentation]),#10,sLineBreak,[rfReplaceAll]);
end;

destructor TfTypeAliasEdit.Destroy();
begin
  FDependencyList.Free();
  inherited Destroy();
end;

function TfTypeAliasEdit.UpdateObject(
  var   AObject : TPasAliasType;
  const AUpdateType : TEditType;
        ASymbolTable : TwstPasTreeContainer
) : Boolean;
begin
  Assert(Assigned(ASymbolTable));
  FSymbolTable := ASymbolTable;
  FUpdateType := AUpdateType;
  FObject := AObject;
  if ( FObject <> nil ) then
    FOldBaseType := FObject.DestType;
  try
    PrepareParentCombo();
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
    if ( not Result ) and ( UpdateType = etCreate ) and ( AObject = nil ) and ( FObject <> nil ) then begin
      FSymbolTable.CurrentModule.InterfaceSection.Declarations.Extract(FObject);
      FSymbolTable.CurrentModule.InterfaceSection.Types.Extract(FObject);
      FSymbolTable.CurrentModule.InterfaceSection.Classes.Extract(FObject);
      FObject.Release();
      FObject := nil;
    end;
  end;
end;

end.

