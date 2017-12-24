unit OWDesignTypes;

{$MODE DELPHI}{$H+}

interface

uses
  LCLIntf, LMessages, LResources, PropEdits, ComponentEditors, Graphics,
  Forms, Messages, Classes, Contnrs, OWPins, OWStdTypes;

type

  IOWDesigner = TComponentEditorDesigner;
  TOWPropNameString = ShortString;
  TOWPropertyDesigner = TPropertyEditorHook;


type
  TOWEPinEntry = class
  public
    Pin: TOWBasicPin;
    PinName: string;
    OwnerName: string;

    SavedChecked: boolean;
    Checked: boolean;

    Dispatcher: TOWStateDispatcher;

  end;

  TOWEPinsList = class(TObjectList)
  protected
    function GetItem(Index: integer): TOWEPinEntry;

  public
    property Items[Index: integer]: TOWEPinEntry read GetItem; default;

  end;

  TOWModulesColection = class(TStringList)
  public
    procedure GetModules(const FileName, UnitName, FormName, DesignClass: string; CoClasses: TStrings);

  end;


function OWCanAccessRootFromName(Designer: TOWPropertyDesigner; RootName: string): boolean;
procedure OWLinkAwaitsLinkingAllForms();
procedure OWRequestDesignerRefresh();
procedure OWGetPinValueList(OwnerComponent: TComponent; Pin: TOWPin; List: TStrings; FilterPins: boolean);
function OWGetMainDesignOwner(Component: TComponent): TComponent;
procedure OWRequestRefreshEx(Designer: TOWPropertyDesigner);

procedure OWResetObjectInspector(Designer: TOWPropertyDesigner);

procedure OWRegisterStreamColorThickness(AStreamTypeID: TGUID; AColor: TColor; AThickness: single);
function OWGetStreamThicknessColorFromID(AStreamTypeID: TGUID; var Color: TColor; var Thickness: single): boolean;

procedure Register;

const

  WM_APP = $8000;

  OWM_UPDATE = WM_APP + 10;
  OWMSG_UPDATE_INSPECTOR = WM_APP + 11;

var
  GOWInRefresh: boolean;

var
  GOWRefreshForm: TForm;

implementation

uses SysUtils, Math;

type
  TADesignerSelectionList = TPersistentSelectionList;

const
  SEPARATOR = '.';

var
  InOppening: boolean;


function TOWEPinsList.GetItem(Index: integer): TOWEPinEntry;
begin
  Result := TOWEPinEntry(inherited Items[Index]);
end;

procedure TOWModulesColection.GetModules(const FileName, UnitName, FormName, DesignClass: string; CoClasses: TStrings);
begin
  if (FormName <> '') then
    Add(FormName);

end;


function OWCanAccessRootFromName(Designer: TOWPropertyDesigner; RootName: string): boolean;
var
  Component: TComponent;
begin
  Result := True;
end;

procedure OWLinkAwaitsLinkingAllForms();
var
  I: integer;
  ModuleIndex: integer;
  ModuleFileExt: string;

begin
  if (not PinsNeedRefresh) then
    Exit;

  if (OWGetAllLinked()) then
    Exit;

  InOppening := True;
  PinsNeedRefresh := False;
  InOppening := False;
end;

procedure OWRequestDesignerRefresh();
begin
  if (InOppening) then
    Exit;

  if (OWGetAllLinked()) then
    Exit;

  if (PinsNeedRefresh) then
    Exit;

  PinsNeedRefresh := True;

  if (GOWRefreshForm <> nil) then
    PostMessage(GOWRefreshForm.Handle, OWM_UPDATE, 0, 0);

end;

procedure OWGetPinValueList(OwnerComponent: TComponent; Pin: TOWPin; List: TStrings; FilterPins: boolean);
var
  Filters: TOWPinValueFilters;

begin
  if (Pin <> nil) then
  begin
    if (FilterPins) then
      Filters := []

    else
      Filters := [pvoFullList, pvoExcludeDirectDependency];

    if (OWGetMainOwnerComponent(Pin.Owner) <> OwnerComponent) then
      OWGetPinsValueListSingleRoot(List, OwnerComponent, Pin, '.', OwnerComponent.Name, Filters)

    else
      OWGetPinsValueListSingleRoot(List, OwnerComponent, Pin, '.', '', Filters);

  end;
end;

function OWGetMainDesignOwner(Component: TComponent): TComponent;
begin
  if (Component.Owner = nil) then
    Result := Component

  else
    Result := OWGetMainOwnerComponent(Component.Owner);

end;

procedure OWRequestRefreshEx(Designer: TOWPropertyDesigner);
var
  FormNames: TOWModulesColection;
  I: integer;

begin
  OWRequestDesignerRefresh();
end;

procedure OWResetObjectInspector(Designer: TOWPropertyDesigner);
begin
end;

type
  TOWStreamInfoOWEditorExtention = class;

  IOWStreamInfoOWEditorExtention = interface
    ['{21C15026-CF32-4579-AE17-4EA6A065A7C5}']
    function GetInstance(): TOWStreamInfoOWEditorExtention;

  end;

  TOWStreamInfoOWEditorExtention = class(TOWStreamInfoExtention, IOWStreamInfoOWEditorExtention)
  protected
    FColor: TColor;
    FThickness: single;

  protected
    function GetInstance(): TOWStreamInfoOWEditorExtention;

  public
    constructor Create(AExtentionId: TGUID; AColor: TColor; AThickness: single);

  public
    property Color: TColor read FColor;
    property Thickness: single read FThickness;

  end;

constructor TOWStreamInfoOWEditorExtention.Create(AExtentionId: TGUID; AColor: TColor; AThickness: single);
begin
  inherited Create(AExtentionId);
  FColor := AColor;
  FThickness := AThickness;
end;

function TOWStreamInfoOWEditorExtention.GetInstance(): TOWStreamInfoOWEditorExtention;
begin
  Result := Self;
end;

procedure OWRegisterStreamColorThickness(AStreamTypeID: TGUID; AColor: TColor; AThickness: single);
begin
  OWRegisterStreamExtention(AStreamTypeID, TOWStreamInfoOWEditorExtention.Create(IOWStreamInfoOWEditorExtention, AColor, AThickness));
end;

function OWGetStreamThicknessColorFromID(AStreamTypeID: TGUID; var Color: TColor; var Thickness: single): boolean;
var
  AExtention: IOWStreamInfoOWEditorExtention;

begin
  AExtention := (OWGetStreamExtentionFromID(AStreamTypeID, IOWStreamInfoOWEditorExtention) as IOWStreamInfoOWEditorExtention);
  if (AExtention = nil) then
  begin
    Result := False;
    Exit;
  end;

  Color := AExtention.GetInstance().Color;
  Thickness := AExtention.GetInstance().Thickness;

  Result := True;
end;

procedure Register;
begin
  OWRegisterStreamColorThickness(IOWIntegerStream, clFuchsia, 1);
  OWRegisterStreamColorThickness(IOWInt64Stream, clFuchsia, 1);
  OWRegisterStreamColorThickness(IOWFloatStream, clRed, 1);
  OWRegisterStreamColorThickness(IOWRealStream, clRed, 1);
  OWRegisterStreamColorThickness(IOWRealComplexStream, clAqua, 1);
  OWRegisterStreamColorThickness(IOWBoolStream, clBlue, 1);
  OWRegisterStreamColorThickness(IOWCharStream, clTeal, 1);
  OWRegisterStreamColorThickness(IOWStringStream, clTeal, 2);
  OWRegisterStreamColorThickness(IOWIntRangedStream, clFuchsia, 1);
  OWRegisterStreamColorThickness(IOWInt64RangedStream, clFuchsia, 1);
  OWRegisterStreamColorThickness(IOWRealRangedStream, clRed, 1);
end;

initialization
  InOppening := False;
  GOWInRefresh := False;

finalization
  OWFreeStreamInfo(IOWIntegerStream);
  OWFreeStreamInfo(IOWInt64Stream);
  OWFreeStreamInfo(IOWFloatStream);
  OWFreeStreamInfo(IOWRealStream);
  OWFreeStreamInfo(IOWRealComplexStream);
  OWFreeStreamInfo(IOWBoolStream);
  OWFreeStreamInfo(IOWCharStream);
  OWFreeStreamInfo(IOWStringStream);
  OWFreeStreamInfo(IOWIntRangedStream);
  OWFreeStreamInfo(IOWInt64RangedStream);
  OWFreeStreamInfo(IOWRealRangedStream);

end.
