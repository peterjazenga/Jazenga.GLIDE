{
    This file is part of the Web Service Toolkit
    Copyright (c) 2007 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
unit ufEnumedit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, ActnList, Buttons, Contnrs,
  pastree, pascal_parser_intf, edit_helper, SynEdit, SynHighlighterXML;

type

  { TfEnumEdit }

  TfEnumEdit = class(TForm)
    actApply : TAction;
    actOK: TAction;
    ActionList1: TActionList;
    Button1: TButton;
    Button2: TButton;
    Button3 : TButton;
    edtDocumentation : TMemo;
    edtName: TEdit;
    edtSourceXSD : TSynEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    edtItems: TMemo;
    PC: TPageControl;
    Panel1: TPanel;
    SynXMLSyn1 : TSynXMLSyn;
    TabSheet1: TTabSheet;
    tsDocumentation : TTabSheet;
    tsDependencies : TTabSheet;
    tsSourceXSD : TTabSheet;
    tvDependency : TTreeView;
    procedure actApplyExecute(Sender : TObject);
    procedure actOKExecute(Sender: TObject);
    procedure actOKUpdate(Sender: TObject);
    procedure PCChange(Sender : TObject);
  private
    FUpdateType : TEditType;
    FObject : TPasEnumType;
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
      var   AObject     : TPasEnumType;
      const AUpdateType : TEditType;
            ASymbolTable : TwstPasTreeContainer
    ):Boolean;
  end; 

var
  fEnumEdit: TfEnumEdit;

implementation

{$R *.lfm}

uses parserutils, view_helper, xsd_consts;

function ParseEnum(
  const AName   : string;
        AItems  : TStrings;
        AObject : TPasEnumType;
        ASymbolTable : TwstPasTreeContainer
) : TPasEnumType ;
var
  buffer : string;
  i : Integer;
  typExtName, typIntName, itmExtName, itmIntName : string;
  itm : TPasEnumValue;
begin
  typExtName := ExtractIdentifier(AName);
  typIntName := MakeInternalSymbolNameFrom(typExtName);
  if IsStrEmpty(typExtName) then begin
    raise ESymbolException.CreateFmt('Invalid enumeration name : "%s"',[AName]);
  end;
  Result := AObject;
  if ( Result = nil ) then begin
    Result := TPasEnumType(ASymbolTable.CreateElement(TPasEnumType,typIntName,ASymbolTable.CurrentModule.InterfaceSection,visDefault,'',0));
    ASymbolTable.CurrentModule.InterfaceSection.Declarations.Add(Result);
    ASymbolTable.CurrentModule.InterfaceSection.Types.Add(Result);
  end;
  ASymbolTable.RegisterExternalAlias(Result,typExtName);
  try
    while ( Result.Values.Count > 0 ) do begin
      itm := TPasEnumValue(Result.Values[0]);
      Result.Values.Extract(itm);
      itm.Release();
    end;
    for i := 0 to Pred(AItems.Count) do begin
      buffer := AItems[i];
      if IsStrEmpty(buffer) then
        itmIntName := Format('%s_EmptyItem',[typIntName])
      else
        itmIntName := MakeInternalSymbolNameFrom(buffer);
      itm := TPasEnumValue(ASymbolTable.CreateElement(TPasEnumValue,itmIntName,Result,visDefault,'',0));
      Result.Values.Add(itm);
      itmExtName := ExtractIdentifier(buffer);
      if not AnsiSameText(itm.Name,itmExtName) then
        ASymbolTable.RegisterExternalAlias(itm,itmExtName);
    end;
  except
    if ( AObject = nil ) then begin
      Result.Release();
    end;
    raise;
  end;
end;

{ TfEnumEdit }

procedure TfEnumEdit.actOKUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled :=
    ( not IsStrEmpty(ExtractIdentifier(edtName.Text)) ) and
    ( not IsStrEmpty(edtItems.Lines.Text) );
end;

procedure TfEnumEdit.PCChange(Sender : TObject);
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

procedure TfEnumEdit.actOKExecute(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TfEnumEdit.actApplyExecute(Sender : TObject);
begin
  SaveToObject();
  if ( PC.ActivePage = tsSourceXSD ) then
    ShowSourceXSD();
end;

procedure TfEnumEdit.LoadFromObject();
var
  i : Integer;
begin
  if ( FObject = nil ) then begin
    edtName.Text := '';
    edtItems.Lines.Clear();
    Self.Caption := 'New enumeration';
  end else begin
    edtName.Text := FSymbolTable.GetExternalName(FObject);
    Self.Caption := edtName.Text;
    edtItems.Lines.Clear();
    for i := 0 to Pred(FObject.Values.Count) do begin
      edtItems.Lines.Add(FSymbolTable.GetExternalName(TPasElement(FObject.Values[i])));
    end;
  end;
end;

procedure TfEnumEdit.SaveToObject();
var
  locObj : TPasEnumType;
begin
  locObj := nil;
  if ( UpdateType = etCreate ) and ( FObject = nil ) then begin
    locObj := ParseEnum(edtName.Text,edtItems.Lines,nil,FSymbolTable);
    //FreeAndNil(FObject);
    FObject := locObj;
  end else begin
    ParseEnum(edtName.Text,edtItems.Lines,FObject,FSymbolTable);
  end;
  FSymbolTable.Properties.GetList(FObject).Values[s_documentation] :=
    EncodeLineBreak(StringReplace(edtDocumentation.Lines.Text,sLineBreak,#10,[rfReplaceAll]));
  FApplied := True;
end;

procedure TfEnumEdit.ShowSourceXSD();
begin
  edtSourceXSD.Lines.Text := XsdGenerateSourceForObject(FObject,FSymbolTable);
end;

procedure TfEnumEdit.ShowDependencies();
begin
  if ( FDependencyList = nil ) then
    FDependencyList := TObjectList.Create(True);
  FDependencyList.Clear();
  FindDependencies(FObject,FSymbolTable,FDependencyList);
  DrawDependencies(tvDependency,FDependencyList);
end;

procedure TfEnumEdit.ShowDocumentation();
var
  props : TStrings;
begin
  props := FSymbolTable.Properties.FindList(FObject);
  if ( props <> nil ) then
    edtDocumentation.Lines.Text :=
      StringReplace(DecodeLineBreak(props.Values[s_documentation]),#10,sLineBreak,[rfReplaceAll]);
end;

destructor TfEnumEdit.Destroy();
begin
  FDependencyList.Free();
  inherited Destroy();
end;

function TfEnumEdit.UpdateObject(
  var   AObject     : TPasEnumType;
  const AUpdateType : TEditType;
        ASymbolTable : TwstPasTreeContainer
) : Boolean;
begin
  FSymbolTable := ASymbolTable;
  FUpdateType := AUpdateType;
  FObject := AObject;
  LoadFromObject();
  ActiveControl := edtName;
  Result := FApplied or ( ShowModal() = mrOK );
  if Result then begin
    SaveToObject();
    if ( AUpdateType = etCreate ) then begin
      AObject := FObject;
    end;
  end;
end;

end.

