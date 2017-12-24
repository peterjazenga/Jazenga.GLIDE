unit register_rxctrl;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, LResources, LazarusPackageIntf, DBPropEdits, PropEdits,
  DB, ComponentEditors;

type

  { TRxCollumsSortFieldsProperty }

  TRxCollumsSortFieldsProperty = class(TDBGridFieldProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
    procedure FillValues(const Values: TStringList); override;
  end;

  { TPopUpColumnFieldProperty }

  TPopUpColumnFieldProperty = class(TFieldProperty)
  public
    procedure FillValues(const Values: TStringList); override;
  end;

type

  { THistoryButtonProperty }

  THistoryButtonProperty = class(TStringPropertyEditor)
  public
    function  GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

type

  { TRxLoginDialogEditor }

  TRxLoginDialogEditor = class(TComponentEditor)
  public
    DefaultEditor: TBaseComponentEditor;
    constructor Create(AComponent: TComponent; ADesigner: TComponentEditorDesigner); override;
    destructor Destroy; override;
    function GetVerbCount:integer;override;
    function GetVerb(Index:integer):string;override;
    procedure ExecuteVerb(Index:integer);override;
  end;

(*
  { TRxAppIcon }

  TRxAppIconEditor = class(TComponentEditor)
  public
    DefaultEditor: TBaseComponentEditor;
    constructor Create(AComponent: TComponent; ADesigner: TComponentEditorDesigner); override;
    destructor Destroy; override;
    function GetVerbCount:integer;override;
    function GetVerb(Index:integer):string;override;
    procedure ExecuteVerb(Index:integer);override;
  end;
*)
procedure Register;
implementation
uses RxLogin, Dialogs, rxconst, RxHistoryNavigator, rxpopupunit,
  rxceEditLookupFields, rxdbgrid, rxdconst, rxduallist, rxstrutils, Forms;

resourcestring
  sTestTRxLoginDialog = 'Test TRxLoginDialog';
//  sLoadIcon        = 'Load icon';


{ TRxLoginDialogEditor }

constructor TRxLoginDialogEditor.Create(AComponent: TComponent;
  ADesigner: TComponentEditorDesigner);
var
  CompClass: TClass;
begin
  inherited Create(AComponent, ADesigner);
  CompClass := PClass(Acomponent)^;
  try
    PClass(AComponent)^ := TComponent;
    DefaultEditor := GetComponentEditor(AComponent, ADesigner);
  finally
    PClass(AComponent)^ := CompClass;
  end;
end;

destructor TRxLoginDialogEditor.Destroy;
begin
  DefaultEditor.Free;
  inherited Destroy;
end;

function TRxLoginDialogEditor.GetVerbCount: integer;
begin
  Result:=DefaultEditor.GetVerbCount + 1;
end;

function TRxLoginDialogEditor.GetVerb(Index: integer): string;
begin
  if Index < DefaultEditor.GetVerbCount then
    Result := DefaultEditor.GetVerb(Index)
  else
  begin
    case Index - DefaultEditor.GetVerbCount of
      0:Result:=sTestTRxLoginDialog;
    end;
  end;
end;

procedure TRxLoginDialogEditor.ExecuteVerb(Index: integer);
begin
  if Index < DefaultEditor.GetVerbCount then
    DefaultEditor.ExecuteVerb(Index)
  else
  begin
    case Index - DefaultEditor.GetVerbCount of
      0:(Component as TRxLoginDialog).Login;
    end;
  end;
end;
(*
{ TRxAppIcon }

type
  PClass = ^TClass;

constructor TRxAppIconEditor.Create(AComponent: TComponent;
  ADesigner: TComponentEditorDesigner);
var
  CompClass: TClass;
begin
  inherited Create(AComponent, ADesigner);
  CompClass := PClass(Acomponent)^;
  try
    PClass(AComponent)^ := TComponent;
    DefaultEditor := GetComponentEditor(AComponent, ADesigner);
  finally
    PClass(AComponent)^ := CompClass;
  end;
end;

destructor TRxAppIconEditor.Destroy;
begin
  DefaultEditor.Free;
  inherited Destroy;
end;

function TRxAppIconEditor.GetVerbCount: integer;
begin
  Result:=DefaultEditor.GetVerbCount + 1;
end;

function TRxAppIconEditor.GetVerb(Index: integer): string;
begin
  if Index < DefaultEditor.GetVerbCount then
    Result := DefaultEditor.GetVerb(Index)
  else
  begin
    case Index - DefaultEditor.GetVerbCount of
      0:Result:=sLoadIcon;
    end;
  end;
end;

procedure TRxAppIconEditor.ExecuteVerb(Index: integer);
var
  OpenDialog1: TOpenDialog;
begin
  if Index < DefaultEditor.GetVerbCount then
    DefaultEditor.ExecuteVerb(Index)
  else
  begin
    case Index - DefaultEditor.GetVerbCount of
      0:begin
          OpenDialog1:=TOpenDialog.Create(nil);
          OpenDialog1.Filter:=sWindowsIcoFiles;
          try
            if OpenDialog1.Execute then
              (Component as TRxAppIcon).LoadFromFile(OpenDialog1.FileName);
          finally
            OpenDialog1.Free;
          end;
          Modified;
        end;
    end;
  end;
end;
*)
{ THistoryButtonProperty }

function THistoryButtonProperty.GetAttributes: TPropertyAttributes;
begin
  Result:= [paValueList, paSortList, paMultiSelect];
end;

procedure THistoryButtonProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  Navigator:TRxHistoryNavigator;
begin
  Navigator:=TRxHistoryNavigator(GetComponent(0));
  if Assigned(Navigator) then
  begin
    if Assigned(Navigator.ToolPanel) then
    begin
      for i:=0 to Navigator.ToolPanel.Items.Count - 1 do
      begin
        if Assigned(Navigator.ToolPanel.Items[i].Action) then
          Proc(Navigator.ToolPanel.Items[i].Action.Name);
      end;
    end;
  end;
end;

{ TPopUpColumnFieldProperty }

procedure TPopUpColumnFieldProperty.FillValues(const Values: TStringList);
var
  Column: TPopUpColumn;
  DataSource: TDataSource;
begin
  Column:=TPopUpColumn(GetComponent(0));
  if not (Column is TPopUpColumn) then exit;
  DataSource := TPopUpFormColumns(Column.Collection).PopUpFormOptions.DataSource;
  if Assigned(DataSource) and Assigned(DataSource.DataSet) then
    DataSource.DataSet.GetFieldNames(Values);
end;


{ TRxCollumsSortFieldsProperty }

function TRxCollumsSortFieldsProperty.GetAttributes: TPropertyAttributes;
begin
  Result:= [paValueList, paSortList, paMultiSelect, paDialog];
end;

procedure TRxCollumsSortFieldsProperty.Edit;
var
  DualListDialog1: TDualListDialog;
  FCol:TRxColumn;
 /// FGrid:TRxDBGrid;

procedure DoInitFill;
var
  i,j:integer;
  LookupDisplay:string;
begin
  LookupDisplay:=FCol.SortFields;
  if LookupDisplay<>'' then
  begin
    StrToStrings(LookupDisplay, DualListDialog1.List2, ';');
    for i:=DualListDialog1.List1.Count-1 downto 0 do
    begin
      j:=DualListDialog1.List2.IndexOf(DualListDialog1.List1[i]);
      if j>=0 then
        DualListDialog1.List1.Delete(i);
    end;
  end;
end;

function DoFillDone:string;
var
  i:integer;
begin
  for i:=0 to DualListDialog1.List2.Count-1 do
    Result:=Result + DualListDialog1.List2[i]+';';
  if Result<>'' then
    Result:=Copy(Result, 1, Length(Result)-1);
end;

procedure DoSetCaptions;
begin
  DualListDialog1.Label1Caption:=sRxAllFields;
  DualListDialog1.Label2Caption:=sRxSortFieldsDisplay;
  DualListDialog1.Title:=sRxFillSortFieldsDisp;
end;

begin
  FCol:=nil;

  if GetComponent(0) is TRxColumn then
    FCol:=TRxColumn(GetComponent(0))
  else
    exit;

  DualListDialog1:=TDualListDialog.Create(Application);
  try
    DoSetCaptions;
    FillValues(DualListDialog1.List1 as TStringList);
    DoInitFill;
    if DualListDialog1.Execute then
      FCol.SortFields:=DoFillDone;
  finally
    FreeAndNil(DualListDialog1);
  end;
end;

procedure TRxCollumsSortFieldsProperty.FillValues(const Values: TStringList);
var
  Column: TRxColumn;
  Grid: TRxDBGrid;
  DataSource: TDataSource;
begin
  Column:=TRxColumn(GetComponent(0));
  if not (Column is TRxColumn) then exit;
  Grid:=TRxDBGrid(Column.Grid);
  if not (Grid is TRxDBGrid) then exit;
//  LoadDataSourceFields(Grid.DataSource, Values);

  DataSource := Grid.DataSource;
  if (DataSource is TDataSource) and Assigned(DataSource.DataSet) then
    DataSource.DataSet.GetFieldNames(Values);

end;


procedure Register;
begin
  //
  RegisterComponentEditor(TRxLoginDialog, TRxLoginDialogEditor);
  //RegisterComponentEditor(TRxAppIcon, TRxAppIconEditor);
  //
  RegisterPropertyEditor(TypeInfo(string), TPopUpColumn, 'FieldName', TPopUpColumnFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TRxHistoryNavigator, 'BackBtn', THistoryButtonProperty);
  RegisterPropertyEditor(TypeInfo(string), TRxHistoryNavigator, 'ForwardBtn', THistoryButtonProperty);

  RegisterPropertyEditor(TypeInfo(string), TRxColumn, 'SortFields', TRxCollumsSortFieldsProperty);

  RegisterCEEditLookupFields;
  //
end;
end.

