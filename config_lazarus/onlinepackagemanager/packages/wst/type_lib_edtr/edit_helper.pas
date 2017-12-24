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
unit edit_helper;

interface

uses
  Classes, SysUtils,
  pastree, pscanner, pascal_parser_intf;

type

  EWstEditException = class(Exception)
  end;

  TEditType = ( etCreate, etUpdate, etDelete, etClone );
  
  { TObjectUpdater }

  TObjectUpdater = class
  public
    class function CanHandle(AObject : TObject; const AEditAction : TEditType):Boolean;virtual;
    class function UpdateObject(
      var AObject : TPasElement;
      ASymbolTable : TwstPasTreeContainer
    ):Boolean;virtual;abstract;
    class procedure DeleteObject(
      AObject : TPasElement;
      ASymbolTable : TwstPasTreeContainer
    );virtual;
    class function CloneObject(
      AObject : TPasElement;
      ASymbolTable : TwstPasTreeContainer
    ) : TPasElement;virtual; abstract;
  end;
  TObjectUpdaterClass = class of TObjectUpdater;
  
  function CreateEnum(AContainer : TwstPasTreeContainer) : TPasEnumType;
  function CreateClassObject(ASymbolTable : TwstPasTreeContainer) : TPasClassType;
  function CreateRecordObject(ASymbolTable : TwstPasTreeContainer) : TPasRecordType;
  function CreateArray(ASymbolTable : TwstPasTreeContainer) : TPasArrayType;
  function CreateInterface(ASymbolTable : TwstPasTreeContainer) : TPasClassType;
  function CreateMethod(
    AOwner       : TPasClassType;
    ASymbolTable : TwstPasTreeContainer
  ) : TPasProcedure;
  function CreateArgument(
    AOwner       : TPasProcedureType;
    ASymbolTable : TwstPasTreeContainer
  ) : TPasArgument;
  function CreateAliasType(ASymbolTable : TwstPasTreeContainer) : TPasAliasType;



  function HasEditor(AObject : TObject; const AEditAction : TEditType):Boolean; overload;
  function HasEditor(
          AObject : TObject; 
    const AEditAction : TEditType;
    out   AHandler : TObjectUpdaterClass  
  ): Boolean; overload;
  function UpdateObject(
    var AObject : TPasElement;
        ASymbolTable : TwstPasTreeContainer
  ):Boolean;
  procedure DeleteObject(
    AObject : TPasElement;
    ASymbolTable : TwstPasTreeContainer
  );
  
  procedure FillList(ALs : TStrings;AContainer : TwstPasTreeContainer);
  procedure FillTypeList(
    ALs : TStrings;
    ASymbol : TwstPasTreeContainer
  );

resourcestring
  s_CantDeleteMainModule = 'Can not delete the main module : "%s".';
  s_CantDeleteStillReferencedObject = 'Can not delete a still referenced Object : "%s".';
  s_NoHandlerFound = 'No handler found.';


implementation
uses Contnrs, Forms, ufEnumedit, ufclassedit, uinterfaceedit, uprocedit,
     uargedit, umoduleedit, ubindingedit, ufarrayedit, uftypealiasedit,
     ufrecordedit;

type

  { TUpdaterRegistry }

  TUpdaterRegistry = class
  private
    FList : TClassList;
  private
    function FindHanlderIndex(AObj : TObject; const AEditAction : TEditType):Integer;
  public
    constructor Create();
    destructor Destroy();override;
    procedure RegisterHandler(AHandlerClass : TObjectUpdaterClass);
    function FindHandler(AObj : TObject; const AEditAction : TEditType; out AHandler : TObjectUpdaterClass) : Boolean;
  end;

var UpdaterRegistryInst : TUpdaterRegistry;

function CreateInterface(ASymbolTable: TwstPasTreeContainer): TPasClassType;
var
  f : TfInterfaceEdit;
begin
  Result := nil;
  f := TfInterfaceEdit.Create(Application);
  try
    f.UpdateObject(Result,etCreate,ASymbolTable);
  finally
    f.Release();
  end;
end;

function HasEditor(
        AObject : TObject; 
  const AEditAction : TEditType;
  out   AHandler : TObjectUpdaterClass  
): Boolean;
begin
  Result := UpdaterRegistryInst.FindHandler(AObject,AEditAction,AHandler) and AHandler.CanHandle(AObject,AEditAction);
end;

function HasEditor(AObject : TObject; const AEditAction : TEditType): Boolean;
var
  h : TObjectUpdaterClass;
begin
  Result := HasEditor(AObject,AEditAction,h);
end; 

function UpdateObject(
  var AObject : TPasElement;
      ASymbolTable : TwstPasTreeContainer
):Boolean;
var
  h : TObjectUpdaterClass;
begin
  if not UpdaterRegistryInst.FindHandler(AObject,etUpdate,h) then begin
    raise EWstEditException.Create('No handler found.');
  end;
  Result := h.UpdateObject(AObject,ASymbolTable);
end;

procedure DeleteObject(
  AObject : TPasElement;
  ASymbolTable : TwstPasTreeContainer
);
var
  h : TObjectUpdaterClass;
begin
  if not UpdaterRegistryInst.FindHandler(AObject,etDelete,h) then begin
    raise EWstEditException.Create('No handler found.');
  end;
  h.DeleteObject(AObject,ASymbolTable);
end;

type
  { TEnumUpdater }

  TEnumUpdater = class(TObjectUpdater)
  public
    class function CanHandle(AObject : TObject; const AEditAction : TEditType):Boolean;override;
    class function UpdateObject(
      var AObject : TPasElement;
          ASymbolTable : TwstPasTreeContainer
    ):Boolean;override;
  end;

  { TClassUpdater }

  TClassUpdater = class(TObjectUpdater)
  public
    class function CanHandle(AObject : TObject; const AEditAction : TEditType):Boolean;override;
    class function UpdateObject(
      var AObject : TPasElement;
          ASymbolTable : TwstPasTreeContainer
    ):Boolean;override;
    class function CloneObject(
      AObject : TPasElement;
      ASymbolTable : TwstPasTreeContainer
    ) : TPasElement; override;
  end;

  { TRecordUpdater }

  TRecordUpdater = class(TObjectUpdater)
  public
    class function CanHandle(AObject : TObject; const AEditAction : TEditType):Boolean;override;
    class function UpdateObject(
      var AObject : TPasElement;
          ASymbolTable : TwstPasTreeContainer
    ):Boolean;override;
  end;
  
  { TTypeAliasUpdater }

  TTypeAliasUpdater = class(TObjectUpdater)
  public
    class function CanHandle(AObject : TObject; const AEditAction : TEditType):Boolean;override;
    class function UpdateObject(
      var AObject : TPasElement;
          ASymbolTable : TwstPasTreeContainer
    ):Boolean;override;
  end;
  
  { TArrayUpdater }

  TArrayUpdater = class(TObjectUpdater)
  public
    class function CanHandle(AObject : TObject; const AEditAction : TEditType):Boolean;override;
    class function UpdateObject(
      var AObject : TPasElement;
          ASymbolTable : TwstPasTreeContainer
    ):Boolean;override;
  end;
  
  { TInterfaceUpdater }

  TInterfaceUpdater = class(TObjectUpdater)
  public
    class function CanHandle(AObject : TObject; const AEditAction : TEditType):Boolean;override;
    class function UpdateObject(
      var AObject : TPasElement;
          ASymbolTable : TwstPasTreeContainer
    ):Boolean;override;
  end;

  { TMethodUpdater }

  TMethodUpdater = class(TObjectUpdater)
  public
    class function CanHandle(AObject : TObject; const AEditAction : TEditType):Boolean;override;
    class function UpdateObject(
      var AObject : TPasElement;
          ASymbolTable : TwstPasTreeContainer
    ):Boolean;override;
    class procedure DeleteObject(
      AObject : TPasElement;
      ASymbolTable : TwstPasTreeContainer
    );override;
  end;

  { TArgumentUpdater }

  TArgumentUpdater = class(TObjectUpdater)
  public
    class function CanHandle(AObject : TObject; const AEditAction : TEditType):Boolean;override;
    class function UpdateObject(
      var AObject : TPasElement;
          ASymbolTable : TwstPasTreeContainer
    ):Boolean;override;
  end;

  { TModuleUpdater }

  TModuleUpdater = class(TObjectUpdater)
  public
    class function CanHandle(AObject : TObject; const AEditAction : TEditType):Boolean;override;
    class function UpdateObject(
      var AObject : TPasElement;
          ASymbolTable : TwstPasTreeContainer
    ):Boolean;override;
    class procedure DeleteObject(
      AObject : TPasElement;
      ASymbolTable : TwstPasTreeContainer
    );override;
  end;

  { TBindingUpdater }

  TBindingUpdater = class(TObjectUpdater)
  public
    class function CanHandle(AObject : TObject; const AEditAction : TEditType):Boolean;override;
    class function UpdateObject(
      var AObject : TPasElement;
          ASymbolTable : TwstPasTreeContainer
    ):Boolean;override;
  end;

{ TRecordUpdater }

class function TRecordUpdater.CanHandle(AObject : TObject; const AEditAction : TEditType) : Boolean;
begin
  Result := ( inherited CanHandle(AObject,AEditAction) ) and AObject.InheritsFrom(TPasRecordType) ;
end;

class function TRecordUpdater.UpdateObject(
  var AObject : TPasElement;
      ASymbolTable : TwstPasTreeContainer
) : Boolean;
var
  f : TfRecordEdit;
  e : TPasRecordType;
begin
  e := AObject as TPasRecordType;
  f := TfRecordEdit.Create(Application);
  try
    Result := f.UpdateObject(e,etUpdate,ASymbolTable);
    AObject := e;
  finally
    f.Release();
  end;
end;

{ TTypeAliasUpdater }

class function TTypeAliasUpdater.CanHandle(AObject : TObject; const AEditAction : TEditType) : Boolean;
begin
  Result := ( inherited CanHandle(AObject,AEditAction) ) and AObject.InheritsFrom(TPasAliasType);
end;

class function TTypeAliasUpdater.UpdateObject(
  var AObject : TPasElement;
      ASymbolTable : TwstPasTreeContainer
): Boolean;
var
  f : TfTypeAliasEdit;
  e : TPasAliasType;
begin
  e := AObject as TPasAliasType;
  f := TfTypeAliasEdit.Create(Application);
  try
    Result := f.UpdateObject(e,etUpdate,ASymbolTable);
    AObject := e;
  finally
    f.Release();
  end;
end;

{ TArrayUpdater }

class function TArrayUpdater.CanHandle(AObject : TObject; const AEditAction : TEditType) : Boolean;
begin
  Result := ( inherited CanHandle(AObject,AEditAction) ) and AObject.InheritsFrom(TPasArrayType);
end;

class function TArrayUpdater.UpdateObject(
  var AObject : TPasElement;
      ASymbolTable : TwstPasTreeContainer
): Boolean;
var
  f : TfArrayEdit;
  e : TPasArrayType;
begin
  e := AObject as TPasArrayType;
  f := TfArrayEdit.Create(Application);
  try
    Result := f.UpdateObject(e,etUpdate,ASymbolTable);
    AObject := e;
  finally
    f.Release();
  end;
end;

{ TBindingUpdater }

class function TBindingUpdater.CanHandle(AObject : TObject; const AEditAction : TEditType): Boolean;
begin
  Result := ( inherited CanHandle(AObject,AEditAction) ) and  AObject.InheritsFrom(TwstBinding);
end;

class function TBindingUpdater.UpdateObject(
  var AObject: TPasElement;
      ASymbolTable: TwstPasTreeContainer
): Boolean;
var
  f : TfBindingEdit;
  e : TwstBinding;
begin
  e := AObject as TwstBinding;
  f := TfBindingEdit.Create(Application);
  try
    Result := f.UpdateObject(e,etUpdate,ASymbolTable);
    AObject := e;
  finally
    f.Release();
  end;
end;
  
{ TModuleUpdater }

class function TModuleUpdater.CanHandle(AObject : TObject; const AEditAction : TEditType): Boolean;
begin
  Result := (AObject <> nil) and (AEditAction <> etClone) and
            AObject.InheritsFrom(TPasModule);
end;

class function TModuleUpdater.UpdateObject(
  var AObject: TPasElement;
      ASymbolTable: TwstPasTreeContainer
): Boolean;
var
  f : TfModuleEdit;
  e : TPasModule;
begin
  e := AObject as TPasModule;
  f := TfModuleEdit.Create(Application);
  try
    Result := f.UpdateObject(e,etUpdate,ASymbolTable);
    AObject := e;
  finally
    f.Release();
  end;
end;

function IsDependentOn(AType : TPasType; AUnit : TPasModule) : Boolean;
var
  locElement : TPasElement;
  locList : TList2;
  i, k : Integer;
  locVar : TPasVariable;
  locProcType : TPasProcedureType;
  locArg : TPasArgument;
begin
  Result := False;
  if (AType = nil) then
    exit;
  if AType.InheritsFrom(TPasEnumType) then
    exit;
  if (AType.Parent = AUnit.InterfaceSection) then
    exit(True);
  if AType.InheritsFrom(TPasAliasType) then begin
    locElement := TPasAliasType(AType).DestType;
    if (locElement <> nil) and IsDependentOn(TPasType(locElement),AUnit) then
      exit(True);
  end;

  if AType.InheritsFrom(TPasClassType) then begin
    locElement := TPasClassType(AType).AncestorType;
    if (locElement <> nil) and IsDependentOn(TPasType(locElement),AUnit) then
      exit(True);
    locList := TPasClassType(AType).Members;
    for i := 0 to locList.Count-1 do begin
      locElement := TPasElement(locList[i]);
      if locElement.InheritsFrom(TPasVariable) then begin
        locVar := TPasVariable(locElement);
        if (locVar.VarType <> nil) and IsDependentOn(locVar.VarType,AUnit) then
          exit(True);
      end else if locElement.InheritsFrom(TPasProcedure) then begin
        locProcType := TPasProcedure(locElement).ProcType;
        for k := 0 to locProcType.Args.Count-1 do begin
          locArg := TPasArgument(locProcType.Args[k]);
          if (locArg.ArgType <> nil) and IsDependentOn(locArg.ArgType,AUnit) then
            exit(True);
        end;
        if locProcType.InheritsFrom(TPasFunctionType) then begin
          if IsDependentOn(TPasFunctionType(locProcType).ResultEl.ResultType,AUnit) then
            exit(True);
        end;
      end;
    end;
  {$IFDEF WST_TCLASS_MEMBERS}
    locList := TPasClassType(AType).Members;
  {$ELSE WST_TCLASS_MEMBERS}
    locList := TPasClassType(AType).ClassVars;
  {$ENDIF WST_TCLASS_MEMBERS}
    for i := 0 to locList.Count-1 do begin
      locElement := TPasElement(locList[i]);
      if locElement.InheritsFrom(TPasVariable) then begin
        locVar := TPasVariable(locElement);
        if (locVar.VarType <> nil) and IsDependentOn(locVar.VarType,AUnit) then
          exit(True);
      end;
    end;
  end;
  if AType.InheritsFrom(TPasRecordType) then begin
    locList := TPasClassType(AType).Members;
    for i := 0 to locList.Count-1 do begin
      locElement := TPasElement(locList[i]);
      if locElement.InheritsFrom(TPasVariable) then begin
        locVar := TPasVariable(locElement);
        if (locVar.VarType <> nil) and IsDependentOn(locVar.VarType,AUnit) then
          exit(True);
      end;
    end;
  end;
end;

class procedure TModuleUpdater.DeleteObject(
  AObject      : TPasElement;
  ASymbolTable : TwstPasTreeContainer
);
var
  locModule : TPasModule;
  i : Integer;
  locTypes : TList2;
begin
  locModule := AObject as TPasModule;
  if (locModule = ASymbolTable.CurrentModule) then
    raise EWstEditException.CreateFmt(s_CantDeleteMainModule,[locModule.Name]);
  if (ASymbolTable.CurrentModule = nil) then
    exit;
  locTypes := ASymbolTable.CurrentModule.InterfaceSection.Types;
  for i := 0 to locTypes.Count-1 do begin
    if IsDependentOn(TPasType(locTypes[i]),locModule) then
      raise EWstEditException.CreateFmt(s_CantDeleteStillReferencedObject,[locModule.Name]);
  end;
  if (locModule.RefCount > 0) and
     (ASymbolTable.CurrentModule.InterfaceSection.UsesList.IndexOf(locModule) >= 0)
  then begin
    ASymbolTable.CurrentModule.InterfaceSection.UsesList.Extract(locModule);
    locModule.Release();
    if (locModule.RefCount = 0) and
       (ASymbolTable.Package.Modules.IndexOf(locModule) >= 0)
    then begin
      ASymbolTable.Package.Modules.Extract(locModule);
      locModule.Release();
    end;
  end;
end;
  
{ TArgumentUpdater }

class function TArgumentUpdater.CanHandle(AObject : TObject; const AEditAction : TEditType): Boolean;
begin
  Result := ( inherited CanHandle(AObject,AEditAction) ) and  AObject.InheritsFrom(TPasArgument);
end;

class function TArgumentUpdater.UpdateObject(
  var AObject      : TPasElement;
      ASymbolTable : TwstPasTreeContainer
): Boolean;
var
  f : TfArgEdit;
  e : TPasArgument;
begin
  e := AObject as TPasArgument;
  f := TfArgEdit.Create(Application);
  try
    Result := f.UpdateObject(e,etUpdate,ASymbolTable);
    AObject := e;
  finally
    f.Release();
  end;
end;

{ TMethodUpdater }

class function TMethodUpdater.CanHandle(AObject : TObject; const AEditAction : TEditType): Boolean;
begin
  Result := ( inherited CanHandle(AObject,AEditAction) ) and  AObject.InheritsFrom(TPasProcedure);
end;

class function TMethodUpdater.UpdateObject(
  var AObject: TPasElement;
      ASymbolTable: TwstPasTreeContainer
): Boolean;
var
  f : TfProcEdit;
  e : TPasProcedure;
begin
  e := AObject as TPasProcedure;
  f := TfProcEdit.Create(Application);
  try
    Result := f.UpdateObject(e,etUpdate,ASymbolTable);
    AObject := e;
  finally
    f.Release();
  end;
end;

class procedure TMethodUpdater.DeleteObject(
  AObject      : TPasElement;
  ASymbolTable : TwstPasTreeContainer
);
var
  owner : TPasClassType;
begin
  if (AObject = nil) then
    exit;
  if (AObject.RefCount > 0) then
    raise EWstEditException.CreateFmt(s_CantDeleteStillReferencedObject,[AObject.Name]);
  owner := AObject.Parent as TPasClassType;
  owner.Members.Extract(AObject);
  AObject.Release();
end;

{ TInterfaceUpdater }

class function TInterfaceUpdater.CanHandle(AObject : TObject; const AEditAction : TEditType): Boolean;
begin
  Result := ( inherited CanHandle(AObject,AEditAction) ) and
            ( AObject.InheritsFrom(TPasClassType) and ( TPasClassType(AObject).ObjKind = okInterface ) );
end;

class function TInterfaceUpdater.UpdateObject(
  var AObject: TPasElement;
      ASymbolTable: TwstPasTreeContainer
) : Boolean;
var
  f : TfInterfaceEdit;
  e : TPasClassType;
begin
  e := AObject as TPasClassType;
  f := TfInterfaceEdit.Create(Application);
  try
    Result := f.UpdateObject(e,etUpdate,ASymbolTable);
    AObject := e;
  finally
    f.Release();
  end;
end;
  
{ TClassUpdater }

class function TClassUpdater.CanHandle(AObject : TObject; const AEditAction : TEditType): Boolean;
begin
  Result := ( AObject <> nil ) and
            ( AObject.InheritsFrom(TPasClassType) and ( TPasClassType(AObject).ObjKind = okClass ) );
end;

class function TClassUpdater.UpdateObject(
  var AObject : TPasElement;
      ASymbolTable : TwstPasTreeContainer
): Boolean;
var
  f : TfClassEdit;
  e : TPasClassType;
begin
  e := AObject as TPasClassType;
  f := TfClassEdit.Create(Application);
  try
    Result := f.UpdateObject(e,etUpdate,ASymbolTable);
    AObject := e;
  finally
    f.Release();
  end;
end;

class function TClassUpdater.CloneObject(
  AObject : TPasElement;  
  ASymbolTable : TwstPasTreeContainer
) : TPasElement;

  function MakeNewName(const ABase : string) : string;
  var
    k : Integer;
  begin
    k := 1;
    while True do begin
      Result := Format('%s_%d',[ABase,k]);
      if ( ASymbolTable.FindElement(Result) = nil ) then
        Break;
      Inc(k);
    end;
  end;
  
  procedure CloneProperties(ASource, ADest : TPasClassType);
  var
    ls : TList2;
    k : Integer;
    locSource, locDest : TPasProperty;
  begin
    ls := ASource.Members;
    if ( ls.Count > 0 ) then begin
      for k := 0 to Pred(ls.Count) do begin
        if TObject(ls[k]).InheritsFrom(TPasProperty) then begin
          locSource := TPasProperty(ls[k]);
          locDest := TPasProperty(ASymbolTable.CreateElement(TPasProperty,locSource.Name,ADest,visPublished,'',0));  
          ADest.Members.Add(locDest);
          if ( locSource.VarType <> nil ) then begin
            locDest.VarType := locSource.VarType;
            locDest.VarType.AddRef();
            locDest.StoredAccessorName := locSource.StoredAccessorName;
            locDest.ReadAccessorName := locSource.ReadAccessorName;
            locDest.WriteAccessorName := locSource.WriteAccessorName;            
            ASymbolTable.RegisterExternalAlias(locDest,ASymbolTable.GetExternalName(locSource));
            ASymbolTable.SetPropertyAsAttribute(locDest,ASymbolTable.IsAttributeProperty(locSource));
          end;
        end;
      end;
    end;
  end;
  
var
  locSource, locRes : TPasClassType;
  locNewName : string;
begin
  locSource := AObject as TPasClassType;
  locNewName := MakeNewName(locSource.Name);
  locRes := TPasClassType(
              ASymbolTable.CreateElement(
                TPTreeElement(locSource.ClassType), locNewName,
                ASymbolTable.CurrentModule.InterfaceSection,visDefault,'',0)
            );
  try
    locRes.ObjKind := okClass;
    ASymbolTable.CurrentModule.InterfaceSection.Declarations.Add(locRes);
    ASymbolTable.CurrentModule.InterfaceSection.Types.Add(locRes);
    ASymbolTable.CurrentModule.InterfaceSection.Classes.Add(locRes);
    if ( locSource.AncestorType <> nil ) then begin
      locRes.AncestorType := locSource.AncestorType;
      locRes.AncestorType.AddRef();
    end;
    CloneProperties(locSource,locRes);
  except
    locRes.Free();
    raise;
  end;
  Result := locRes;
end;
  
{ TUpdaterRegistry }

function TUpdaterRegistry.FindHanlderIndex(AObj : TObject; const AEditAction : TEditType): Integer;
var
  i : Integer;
begin
  for i := 0 to Pred(FList.Count) do begin
    if TObjectUpdaterClass(FList[i]).CanHandle(AObj,AEditAction) then begin
      Result := i;
      Exit;
    end;
  end;
  Result := -1;
end;

constructor TUpdaterRegistry.Create();
begin
  FList := TClassList.Create();
end;

destructor TUpdaterRegistry.Destroy();
begin
  FreeAndNil(FList);
  inherited Destroy();
end;

procedure TUpdaterRegistry.RegisterHandler(AHandlerClass : TObjectUpdaterClass);
begin
  if ( FList.IndexOf(AHandlerClass) < 0 ) then begin
    FList.Add(AHandlerClass);
  end;
end;

function TUpdaterRegistry.FindHandler(
        AObj      : TObject;
  const AEditAction : TEditType;
  out   AHandler  : TObjectUpdaterClass
): Boolean;
var
  i : Integer;
begin
  AHandler := nil;
  i := FindHanlderIndex(AObj,AEditAction);
  Result := ( i >= 0 );
  if Result then begin
    AHandler := TObjectUpdaterClass(FList[i]);
  end;
end;

{ TEnumUpdater }

class function TEnumUpdater.CanHandle(AObject : TObject; const AEditAction : TEditType): Boolean;
begin
  Result := ( inherited CanHandle(AObject,AEditAction) ) and AObject.InheritsFrom(TPasEnumType);
end;

class function TEnumUpdater.UpdateObject(
  var AObject : TPasElement;
      ASymbolTable : TwstPasTreeContainer
): Boolean;
var
  f : TfEnumEdit;
  e : TPasEnumType;
begin
  e := AObject as TPasEnumType;
  f := TfEnumEdit.Create(Application);
  try
    Result := f.UpdateObject(e,etUpdate,ASymbolTable);
    AObject := e;
  finally
    f.Release();
  end;
end;

function CreateEnum(AContainer : TwstPasTreeContainer) : TPasEnumType;
var
  f : TfEnumEdit;
begin
  Result := nil;
  f := TfEnumEdit.Create(Application);
  try
    f.UpdateObject(Result,etCreate,AContainer);
  finally
    f.Release();
  end;
end;

function CreateClassObject(ASymbolTable : TwstPasTreeContainer) : TPasClassType;
var
  f : TfClassEdit;
begin
  Result := nil;
  f := TfClassEdit.Create(Application);
  try
    f.UpdateObject(Result,etCreate,ASymbolTable);
  finally
    f.Release();
  end;
end;

function CreateRecordObject(ASymbolTable : TwstPasTreeContainer) : TPasRecordType;
var
  f : TfRecordEdit;
begin
  Result := nil;
  f := TfRecordEdit.Create(Application);
  try
    f.UpdateObject(Result,etCreate,ASymbolTable);
  finally
    f.Release();
  end;
end;

function CreateArray(ASymbolTable : TwstPasTreeContainer) : TPasArrayType;
var
  f : TfArrayEdit;
begin
  Result := nil;
  f := TfArrayEdit.Create(Application);
  try
    f.UpdateObject(Result,etCreate,ASymbolTable);
  finally
    f.Release();
  end;
end;

function CreateMethod(
  AOwner       : TPasClassType;
  ASymbolTable : TwstPasTreeContainer
) : TPasProcedure;
var
  f : TfProcEdit;
begin
  Result := TPasProcedure(ASymbolTable.CreateElement(TPasProcedure,'new_proc',AOwner,visPublic,'',0));
  Result.ProcType := TPasProcedureType(ASymbolTable.CreateElement(TPasProcedureType,'',Result,visDefault,'',0));
  AOwner.Members.Add(Result);
  f := TfProcEdit.Create(Application);
  try
    f.UpdateObject(Result,etCreate,ASymbolTable);
  finally
    f.Release();
  end;
end;

function CreateArgument(
  AOwner       : TPasProcedureType;
  ASymbolTable : TwstPasTreeContainer
) : TPasArgument;
var
  f : TfArgEdit;
begin
  Result := TPasArgument(ASymbolTable.CreateElement(TPasArgument,'AValue',AOwner,visPublic,'',0));
  Result.ArgType := ASymbolTable.FindElement('string') as TPasType;
  f := TfArgEdit.Create(Application);
  try
    f.UpdateObject(Result,etCreate,ASymbolTable);
  finally
    f.Release();
  end;
end;

function CreateAliasType(ASymbolTable : TwstPasTreeContainer) : TPasAliasType;
var
  f : TfTypeAliasEdit;
begin
  Result := nil;
  f := TfTypeAliasEdit.Create(Application);
  try
    f.UpdateObject(Result,etCreate,ASymbolTable);
  finally
    f.Release();
  end;
end;

{ TObjectUpdater }

class function TObjectUpdater.CanHandle(
        AObject : TObject; 
  const AEditAction : TEditType  
) : Boolean;
begin
  Result :=
    (Assigned(AObject) and (AEditAction <> etClone)) and
    (not(AObject.InheritsFrom(TPasModule)) or (AEditAction = etUpdate));
end;

class procedure TObjectUpdater.DeleteObject (
  AObject : TPasElement;
  ASymbolTable : TwstPasTreeContainer
);
var
  sct : TPasSection;
begin
  if (AObject = nil) then
    exit;
  if (AObject.RefCount > 0) then
    raise EWstEditException.CreateFmt(s_CantDeleteStillReferencedObject,[AObject.Name]);
  sct := ASymbolTable.CurrentModule.InterfaceSection;
  sct.Declarations.Extract(AObject);
  sct.Types.Extract(AObject);
  sct.Classes.Extract(AObject);
  AObject.Release();
end;

procedure InternalFillList(
  ALs : TStrings;
  AContainer : TwstPasTreeContainer
);
var
  i : Integer;
  sym : TPasElement;
  decList : TList2;
begin
  decList := AContainer.CurrentModule.InterfaceSection.Declarations;
  for i := 0 to Pred(decList.Count) do begin
    sym := TPasElement(decList[i]);
    if sym.InheritsFrom(TPasType) and
       ( sym.InheritsFrom(TPasClassType) or
         sym.InheritsFrom(TPasNativeSimpleType) or
         ( sym.InheritsFrom(TPasAliasType) and
           Assigned(TPasAliasType(sym).DestType) and
           ( TPasAliasType(sym).DestType.InheritsFrom(TPasClassType) or
             TPasAliasType(sym).DestType.InheritsFrom(TPasNativeSimpleType)
           )
         )
       ) and
       ( not sym.InheritsFrom(TPasNativeSimpleContentClassType) )
    then begin
      if ( ALs.IndexOfObject(sym) = -1 ) then begin
        ALs.AddObject(AContainer.GetExternalName(sym),sym);
      end;
    end;
  end;
end;

procedure FillList(
  ALs : TStrings;
  AContainer : TwstPasTreeContainer
);
var
  locLST : TStringList;
begin
  locLST := TStringList.Create();
  try
    locLST.Assign(ALs);
    locLST.Duplicates := dupAccept;
    InternalFillList(locLST,AContainer);
    locLST.Sort();
    ALs.Assign(locLST);
  finally
    FreeAndNil(locLST);
  end;
end;

procedure InternalFillTypeList(ALs : TStrings; AContainer : TwstPasTreeContainer);
var
  i, j : Integer;
  sym : TPasElement;
  moduleList, decList : TList2;
  mdl : TPasModule;
  locExtName : string;
begin
  moduleList := AContainer.Package.Modules;
  for i := 0 to Pred(moduleList.Count) do begin
    mdl := TPasModule(moduleList[i]);
    decList := mdl.InterfaceSection.Declarations;
    for j := 0 to Pred(decList.Count) do begin
      sym := TPasElement(decList[j]);
      if sym.InheritsFrom(TPasType) {and ( not sym.InheritsFrom(TPasNativeSimpleContentClassType) )} then begin
        if ( ALs.IndexOfObject(sym) = -1 ) then begin
          if sym.InheritsFrom(TPasNativeSpecialSimpleType) or
             sym.InheritsFrom(TPasNativeSpecialSimpleContentClassType)
          then begin
            //ALs.AddObject(sym.Name,sym);
            locExtName := AContainer.GetExternalName(sym);
            if ( sym.Name = locExtName ) then
              ALs.AddObject(sym.Name,sym)
            else
              ALs.AddObject(Format('%s - ( %s )',[sym.Name,AContainer.GetExternalName(sym)]),sym);
          end else begin
            //ALs.AddObject(AContainer.GetExternalName(sym),sym);
            locExtName := AContainer.GetExternalName(sym);   
            if ( sym.Name = locExtName ) then
              ALs.AddObject(AContainer.GetExternalName(sym),sym)
            else
              ALs.AddObject(Format('%s - ( %s )',[AContainer.GetExternalName(sym),sym.Name]),sym);  
          end;
        end;
      end;
    end;
  end;
end;

procedure FillTypeList(
  ALs : TStrings;
  ASymbol : TwstPasTreeContainer
);
var
  locLST : TStringList;
begin
  locLST := TStringList.Create();
  try
    locLST.Assign(ALs);
    locLST.Duplicates := dupAccept;
    InternalFillTypeList(locLST,ASymbol);
    locLST.Sort();
    ALs.Assign(locLST);
  finally
    FreeAndNil(locLST);
  end;
end;

initialization
  UpdaterRegistryInst := TUpdaterRegistry.Create();
  UpdaterRegistryInst.RegisterHandler(TEnumUpdater);
  UpdaterRegistryInst.RegisterHandler(TClassUpdater);
  UpdaterRegistryInst.RegisterHandler(TInterfaceUpdater);
  UpdaterRegistryInst.RegisterHandler(TMethodUpdater);
  UpdaterRegistryInst.RegisterHandler(TArgumentUpdater);
  UpdaterRegistryInst.RegisterHandler(TModuleUpdater);
  UpdaterRegistryInst.RegisterHandler(TBindingUpdater);
  UpdaterRegistryInst.RegisterHandler(TArrayUpdater);
  UpdaterRegistryInst.RegisterHandler(TTypeAliasUpdater);
  UpdaterRegistryInst.RegisterHandler(TRecordUpdater);
  
finalization
  FreeAndNil(UpdaterRegistryInst);
  
end.
