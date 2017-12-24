unit OWClassProperty;

{$MODE DELPHI}{$H+}

interface

uses
  ComponentEditors, PropEdits,
  Classes, TypInfo, Contnrs, OWDesignTypes;

type
  IProperty = TPropertyEditor;

type
  TOWComponentEditorEvent = procedure of object;

type
  TOWComponentEditorItem = class
  private
    FMenuText: string;
    FPropertyName: string;
    FFilter: TTypeKinds;
    FCallback: TOWComponentEditorEvent;

  public
    constructor Create(AMenuText: string; ACallback: TOWComponentEditorEvent; AFilter: TTypeKinds; APropertyName: string);

  end;

type
  TOWComponentEditorItems = class
  private
    FList: TObjectList;

  private
    function GetCount(): integer;
    function GetItem(AIndex: integer): TOWComponentEditorItem;
    procedure SetItem(AIndex: integer; AValue: TOWComponentEditorItem);

  public
    procedure Add(AMenuText: string; ACallback: TOWComponentEditorEvent; AFilter: TTypeKinds; APropertyName: string);

  public
    constructor Create();
    destructor Destroy(); override;

  public
    property Count: integer read GetCount;
    property Items[Index: integer]: TOWComponentEditorItem read GetItem write SetItem; default;

  end;

type
  TOWComponentEditor = class(TComponentEditor)
  protected
    LastIProp: IProperty;
    TargetProperty: string;
    FMenuItems: TOWComponentEditorItems;

  protected
    procedure AGetPropProc(Prop: TPropertyEditor);

    function GetIProperty(comp: TComponent; AFilter: TTypeKinds; PropertyName: string): IProperty;
    procedure EditProperty(PropertyName: string); overload;
    procedure EditProperty(AFilter: TTypeKinds; PropertyName: string); overload;
    procedure AddMenuItem(AMenuText: string; ACallback: TOWComponentEditorEvent); overload;
    procedure AddMenuItem(AMenuText: string; APropertyName: string); overload;
    procedure AddMenuItem(AMenuText: string; AFilter: TTypeKinds; APropertyName: string); overload;
    procedure InitMenu(); virtual;

  public
    procedure ExecuteVerb(Index: integer); override;
    function GetVerb(Index: integer): string; override;
    function GetVerbCount(): integer; override;

  public
    procedure AfterConstruction(); override;

  public
    constructor Create(AComponent: TComponent; ADesigner: IOWDesigner); override;

    destructor Destroy(); override;

  end;

implementation

function TOWComponentEditor.GetIProperty(comp: TComponent; AFilter: TTypeKinds; PropertyName: string): IProperty;
var
  SelectionList: TPersistentSelectionList;
  APropertyHook: TPropertyEditorHook;
begin
  LastIProp := nil;
  Result := nil;

  if (comp = nil) then
    Exit;

  if (PropertyName = '') then
    Exit;

  APropertyHook := TPropertyEditorHook.Create();
  APropertyHook.LookupRoot := comp;
  SelectionList := TPersistentSelectionList.Create();
  SelectionList.Add(comp);
  TargetProperty := PropertyName;

  GetPersistentProperties(SelectionList, AFilter, APropertyHook, AGetPropProc, nil);
  SelectionList.Free();
  APropertyHook.Free();
  Result := LastIProp;
end;

constructor TOWComponentEditor.Create(AComponent: TComponent; ADesigner: IOWDesigner);
begin
  inherited;
  FMenuItems := TOWComponentEditorItems.Create();
end;

destructor TOWComponentEditor.Destroy();
begin
  FMenuItems.Free();
  inherited;
end;

procedure TOWComponentEditor.AfterConstruction();
begin
  inherited;
  InitMenu();
end;

procedure TOWComponentEditor.InitMenu();
begin
end;

procedure TOWComponentEditor.EditProperty(PropertyName: string);
begin
  EditProperty(tkProperties, PropertyName);
end;

procedure TOWComponentEditor.EditProperty(AFilter: TTypeKinds; PropertyName: string);
var
  PropIProp: IProperty;

begin
  PropIProp := GetIProperty(Component, AFilter, PropertyName);
  if (PropIProp <> nil) then
    PropIProp.Edit();

end;

procedure TOWComponentEditor.AGetPropProc(Prop: TPropertyEditor);
begin
  if (Prop.GetName() = TargetProperty) then
    LastIProp := Prop;
end;

procedure TOWComponentEditor.AddMenuItem(AMenuText: string; ACallback: TOWComponentEditorEvent);
begin
  FMenuItems.Add(AMenuText, ACallback, [], '');
end;

procedure TOWComponentEditor.AddMenuItem(AMenuText: string; APropertyName: string);
begin
  AddMenuItem(AMenuText, tkProperties, APropertyName);
end;

procedure TOWComponentEditor.AddMenuItem(AMenuText: string; AFilter: TTypeKinds; APropertyName: string);
begin
  FMenuItems.Add(AMenuText, nil, AFilter, APropertyName);
end;

procedure TOWComponentEditor.ExecuteVerb(Index: integer);
begin
  if (Assigned(FMenuItems[Index].FCallback)) then
    FMenuItems[Index].FCallback()

  else
    EditProperty(FMenuItems[Index].FFilter, FMenuItems[Index].FPropertyName);

end;

function TOWComponentEditor.GetVerb(Index: integer): string;
begin
  Result := FMenuItems[Index].FMenuText;
end;

function TOWComponentEditor.GetVerbCount(): integer;
begin
  Result := FMenuItems.Count;
end;

constructor TOWComponentEditorItems.Create();
begin
  inherited;
  FList := TObjectList.Create(True);
end;

destructor TOWComponentEditorItems.Destroy();
begin
  FList.Free();
  inherited;
end;

function TOWComponentEditorItems.GetCount(): integer;
begin
  Result := FList.Count;
end;

function TOWComponentEditorItems.GetItem(AIndex: integer): TOWComponentEditorItem;
begin
  Result := TOWComponentEditorItem(FList[AIndex]);
end;

procedure TOWComponentEditorItems.SetItem(AIndex: integer; AValue: TOWComponentEditorItem);
begin
  FList[AIndex] := AValue;
end;

procedure TOWComponentEditorItems.Add(AMenuText: string; ACallback: TOWComponentEditorEvent; AFilter: TTypeKinds; APropertyName: string);
begin
  FList.Add(TOWComponentEditorItem.Create(AMenuText, ACallback, AFilter, APropertyName));
end;

constructor TOWComponentEditorItem.Create(AMenuText: string; ACallback: TOWComponentEditorEvent; AFilter: TTypeKinds; APropertyName: string);
begin
  inherited Create;
  FMenuText := AMenuText;
  FCallback := ACallback;
  FFilter := AFilter;
  FPropertyName := APropertyName;
end;

end.
