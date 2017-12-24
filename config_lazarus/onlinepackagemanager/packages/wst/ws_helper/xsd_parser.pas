{
    This file is part of the Web Service Toolkit
    Copyright (c) 2007-2014 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$INCLUDE wst_global.inc}
unit xsd_parser;

interface
uses
  Classes, SysUtils,
  {$IFNDEF FPC}xmldom, wst_delphi_xml{$ELSE}DOM{$ENDIF},
  cursor_intf, rtti_filters,
  pastree, pascal_parser_intf, logger_intf, locators;

type

  EXsdParserException = class(Exception)
  end;

  EXsdParserAssertException = class(EXsdParserException)
  end;

  EXsdTypeNotFoundException = class(EXsdParserException)
  end;

  EXsdInvalidDefinitionException = class(EXsdParserException)
  end;

  EXsdInvalidTypeDefinitionException = class(EXsdInvalidDefinitionException)
  end;

  EXsdInvalidElementDefinitionException = class(EXsdInvalidDefinitionException)
  end;

  TOnParserMessage = TOnLogMessageEvent;

  IDocumentLocator = locators.IDocumentLocator;

  TParserOption = (
    poEnumAlwaysPrefix, // Always prefix enum item with the enum name
    poParsingIncludeSchema
  );
  TParserOptions = set of TParserOption;
  IXsdPaser = interface;
  IParserContext = interface
    ['{F400BA9E-41AC-456C-ABF9-CEAA75313685}']
    function GetXsShortNames() : TStrings;
    function GetSymbolTable() : TwstPasTreeContainer;
    function FindNameSpace(const AShortName : string; out AResult : string) : Boolean;
    function FindShortNamesForNameSpace(const ANameSpace : string) : TStrings;
    function GetTargetNameSpace() : string;
    function GetTargetModule() : TPasModule;
    function GetDocumentLocator() : IDocumentLocator;
    procedure SetDocumentLocator(ALocator : IDocumentLocator);
    function GetSimpleOptions() : TParserOptions;
    procedure SetSimpleOptions(const AValue : TParserOptions);
    procedure AddTypeToCheck(AType : TPasType);

    procedure AddIncludedDoc(ADocLocation : string);
    function IsIncludedDoc(ADocLocation : string) : Boolean;
    function FindParser(const ANamespace : string) : IXsdPaser;
  end;

  IXsdPaser = interface
    ['{F0CEC726-A068-4CCC-B1E7-D31F018415B2}']
    function FindParser(const ANamespace : string) : IXsdPaser; 
    function ParseType(
      const AName,
            ATypeKind : string { ATypeKind "ComplexType", "SimpleType", "Element" }
    ) : TPasType; overload;
    function ParseType(
      const AName     : string;
      const ATypeNode : TDOMNode
    ) : TPasType; overload;
    procedure ParseTypes();
    procedure SetNotifier(ANotifier : TOnParserMessage);
  end;

  { TCustomXsdSchemaParser }

  TCustomXsdSchemaParser = class(TInterfacedObject, IInterface, IParserContext, IXsdPaser)
  private
    FDoc : TXMLDocument;
    FParentContext : Pointer;//IParserContext;
    FSymbols : TwstPasTreeContainer;
    FModuleName : string;
    FModule : TPasModule;
    FTargetNameSpace : string;
    FSchemaNode : TDOMNode;
  private
    FNameSpaceList : TStringList;
    FXSShortNames : TStrings;
    FChildCursor : IObjectCursor;
    FOnMessage: TOnParserMessage;
    FDocumentLocator : IDocumentLocator;
    FSimpleOptions : TParserOptions;
    FCheckedTypes : TList2;
    FImportParsed : Boolean;
    FXsdParsers : TStringList;
    FIncludeList : TStringList;
    FIncludeParsed : Boolean;
    FPrepared : Boolean;
    FOldNameKinds : TElementNameKinds;
  private
    procedure DoOnMessage(const AMsgType : TMessageType; const AMsg : string);
  private
    function FindNamedNode(AList : IObjectCursor; const AName : WideString; const AOrder : Integer = 0):TDOMNode;
    function GetParentContext() : IParserContext;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function HasParentContext() : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure Prepare(const AMustSucceed : Boolean);
    function FindElement(const AName: String) : TPasElement; overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function FindElement(const AName: String; const ANameKinds : TElementNameKinds) : TPasElement; overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
  protected
    function GetXsShortNames() : TStrings;
    function GetSymbolTable() : TwstPasTreeContainer;
    function FindNameSpace(const AShortName : string; out AResult : string) : Boolean;
    function FindShortNamesForNameSpaceLocal(const ANameSpace : string) : TStrings;
    function FindShortNamesForNameSpace(const ANameSpace : string) : TStrings;
    function GetDocumentLocator() : IDocumentLocator;
    procedure SetDocumentLocator(ALocator : IDocumentLocator);
    function GetSimpleOptions() : TParserOptions;
    procedure SetSimpleOptions(const AValue : TParserOptions);
    procedure AddTypeToCheck(AType : TPasType);
    procedure AddIncludedDoc(ADocLocation : string);
    function IsIncludedDoc(ADocLocation : string) : Boolean;

    procedure SetNotifier(ANotifier : TOnParserMessage);
    function InternalParseType(
      const AName : string;
      const ATypeNode : TDOMNode
    ) : TPasType;
    procedure CreateImportParsers(); 
    procedure ParseImportDocuments(); virtual;
    procedure CreateIncludeList();
    procedure ParseIncludeDocuments(); virtual;
  public
    constructor Create(
      ADoc           : TXMLDocument;
      ASchemaNode    : TDOMNode;
      ASymbols       : TwstPasTreeContainer;
      AParentContext : IParserContext
    ); virtual;
    destructor Destroy();override;
    function FindParser(const ANamespace : string) : IXsdPaser; 
    function ParseType(
      const AName,
            ATypeKind : string { ATypeKind "ComplexType", "SimpleType", "Element" }
    ) : TPasType; overload;
    function ParseType(
      const AName     : string;
      const ATypeNode : TDOMNode
    ) : TPasType; overload;

    procedure ParseTypes();

    function GetTargetNameSpace() : string;
    function GetTargetModule() : TPasModule;

    property SymbolTable : TwstPasTreeContainer read FSymbols;
    property Module : TPasModule read FModule;
    property OnMessage : TOnParserMessage read FOnMessage write FOnMessage;
  end;
  TCustomXsdSchemaParserClass = class of TCustomXsdSchemaParser;

  TXsdParser = class(TCustomXsdSchemaParser)
  public
    constructor Create(
            ADoc : TXMLDocument;
            ASymbols : TwstPasTreeContainer;
      const AModuleName : string;
      const ANotifier : TOnParserMessage = nil
    );
  end;

  procedure CheckDuplicatedProperties(
    AClassList   : TList2;
    ASymbolTable : TwstPasTreeContainer
  );

implementation
uses ws_parser_imp, dom_cursors, parserutils, xsd_consts, wst_consts
{$IFDEF FPC}
     ,wst_fpc_xml
{$ENDIF}
     ;

function NodeValue(const ANode : TDOMNode) : DOMString;{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  if ( ANode = nil ) then
    Result := ''
  else
    Result := ANode.NodeValue;
end;

function HasProp(AClass : TPasClassType; const AProp : string) : Boolean;
var
  i : Integer;
  ml : TList2;
  e : TPasElement;
begin
  Result := False;
  ml := AClass.Members;
  for i := 0 to ml.Count - 1 do begin
    e := TPasElement(ml[i]);
    if e.InheritsFrom(TPasProperty) and SameText(e.Name,AProp) then begin
      Result := True;
      Break;
    end;
  end;
end;

procedure CheckDuplicatedProperties(
  AClassList   : TList2;
  ASymbolTable : TwstPasTreeContainer
);
var
  i, k : Integer;
  locItem : TPasClassType;
  locAncestor : TPasType;
  e : TPasElement;
begin
  for i := 0 to AClassList.Count-1 do begin
    locItem := TPasClassType(AClassList[i]);
    if (locItem.Members.Count = 0) then
      Continue;
    locAncestor := locItem.AncestorType;
    while (locAncestor <> nil) do begin
      if locAncestor.InheritsFrom(TPasUnresolvedTypeRef) then begin
        e := ASymbolTable.FindElement(ASymbolTable.GetExternalName(locAncestor));
        if (e = nil) or not(e.InheritsFrom(TPasType)) then
          Break;
        locAncestor := e as TPasType;
      end;
      if not locAncestor.InheritsFrom(TPasClassType) then
        Break;
      if (TPasClassType(locAncestor).Members.Count = 0) then
        Break;
      k := 0;
      while (k < locItem.Members.Count) do begin
        e := TPasElement(locItem.Members[k]);
        if not e.InheritsFrom(TPasProperty) then
          Continue;
        if HasProp(TPasClassType(locAncestor),e.Name) then begin
          locItem.Members.Delete(k);
          e.Release();
          Continue;
        end;
        k := k + 1;
      end;
      locAncestor := TPasClassType(locAncestor).AncestorType;
    end;
  end;
end;


{ TCustomXsdSchemaParser }

constructor TCustomXsdSchemaParser.Create(
  ADoc           : TXMLDocument;
  ASchemaNode    : TDOMNode;
  ASymbols       : TwstPasTreeContainer;
  AParentContext : IParserContext
);
begin
  if ( ADoc = nil ) then
    raise EXsdParserAssertException.Create(SERR_InvalidDomDocument);
  if ( ASchemaNode = nil ) then
    raise EXsdParserAssertException.Create(SERR_InvalidSchemaNode);
  if ( ASymbols = nil ) then
    raise EXsdParserAssertException.Create(SERR_InvalidSymbolTable);

  FDoc := ADoc;
  FParentContext := Pointer(AParentContext);
  FSymbols := ASymbols;
  FOldNameKinds := FSymbols.DefaultSearchNameKinds;
  FSymbols.DefaultSearchNameKinds := [elkDeclaredName];
  FSchemaNode := ASchemaNode;

  FNameSpaceList := TStringList.Create();
  FNameSpaceList.Duplicates := dupError;
  FNameSpaceList.Sorted := True;

  Prepare(False);
end;

destructor TCustomXsdSchemaParser.Destroy();

  procedure FreeList(AList : TStrings);
  var
    j : Integer;
  begin
    if Assigned(AList) and (AList.Count > 0) then begin
      for j := Pred(AList.Count)  downto 0 do begin
        AList.Objects[j].Free();
        AList.Objects[j] := nil;
      end;
      AList.Free();  
    end;
  end; 
  
begin
  if (FSymbols <> nil) then
    FSymbols.DefaultSearchNameKinds := FOldNameKinds;
  FParentContext := nil;
  FreeAndNil(FIncludeList);
  FreeList(FNameSpaceList);
  FreeList(FXsdParsers);
  FCheckedTypes.Free();
  inherited;
end;

function TCustomXsdSchemaParser.FindParser(const ANamespace : string) : IXsdPaser; 
var
  i : Integer;
  p, p1 : IXsdPaser;
begin
  Prepare(True);
  Result := nil;
  if (ANamespace = FTargetNameSpace) then begin
    Result := Self;
    Exit;
  end;
  if (FXsdParsers = nil) then 
    CreateImportParsers();
  if (FXsdParsers = nil) then
    Exit;
  i := FXsdParsers.IndexOf(ANamespace); 
  if ( i >= 0 ) then begin
    Result := (FXsdParsers.Objects[i] as TIntfObjectRef).Intf as IXsdPaser; 
  end else begin
    for i := 0 to Pred(FXsdParsers.Count) do begin
      p := (FXsdParsers.Objects[i] as TIntfObjectRef).Intf as IXsdPaser;
      p1 := p.FindParser(ANamespace);
      if (p1 <> nil) then begin
        Result := p1;
        Break;
      end;
    end;
  end;  
end;

procedure TCustomXsdSchemaParser.DoOnMessage(
  const AMsgType: TMessageType;
  const AMsg: string
);
begin
  if Assigned(FOnMessage) then begin
    FOnMessage(AMsgType,AMsg);
  end else if IsConsole and HasLogger() then begin
    GetLogger().Log(AMsgType, AMsg);
  end;
end;

function TCustomXsdSchemaParser.FindElement(const AName: String): TPasElement;
begin
  Result := SymbolTable.FindElementInModule(AName,FModule);
  if ( Result = nil ) then
    Result := SymbolTable.FindElement(AName);
end;

function TCustomXsdSchemaParser.FindElement(
  const AName      : String;
  const ANameKinds : TElementNameKinds
) : TPasElement;
begin
  Result := SymbolTable.FindElementInModule(AName,FModule,ANameKinds);
  if ( Result = nil ) then
    Result := SymbolTable.FindElement(AName,ANameKinds);
end;

procedure TCustomXsdSchemaParser.ParseImportDocuments();
var
  locOldCurrentModule : TPasModule; 
  i : Integer;
  p : IXsdPaser;
begin
  if FImportParsed then
    Exit;
  CreateImportParsers();
  if (FXsdParsers = nil) then
    Exit;

  FImportParsed := True;
  if Assigned(FChildCursor) then begin
    locOldCurrentModule := SymbolTable.CurrentModule;
    try
      for i := 0 to FXsdParsers.Count - 1 do begin
        p := TIntfObjectRef(FXsdParsers.Objects[i]).Intf as IXsdPaser;
        p.ParseTypes();
      end;
    finally
      SymbolTable.SetCurrentModule(locOldCurrentModule);
    end;
  end;
end;

procedure TCustomXsdSchemaParser.CreateIncludeList();
begin
  if (FIncludeList = nil) then begin
    FIncludeList := TStringList.Create();
    FIncludeList.Duplicates := dupIgnore;
    FIncludeList.Sorted := True;
  end;
end;

procedure TCustomXsdSchemaParser.ParseIncludeDocuments();
var
  crsSchemaChild : IObjectCursor;
  strFilter, locFileName : string;
  includeNode : TDOMElement;
  includeDoc : TXMLDocument;
  locParser : IXsdPaser;
  locOldCurrentModule : TPasModule;
  locLocator, locTempLocator : IDocumentLocator;
  locContext : IParserContext;
  locUsesList : TList2;
  locModule : TPasModule;
  locName, s : string;
  i : Integer;
begin
  if FIncludeParsed then
    exit;
  Prepare(True);
  if (poParsingIncludeSchema in FSimpleOptions) then begin
    locContext := GetParentContext();
    if (locContext = nil) then
      raise EXsdParserAssertException.CreateFmt(SERR_InvalidParserState,['"poParsingIncludeSchema" require a parent context']);
    if not(IsStrEmpty(FTargetNameSpace)) and (FTargetNameSpace <> locContext.GetTargetNameSpace()) then
      raise EXsdParserAssertException.Create(SERR_InvalidIncludeDirectiveNS);
  end;

  FIncludeParsed := True;
  locLocator := GetDocumentLocator();
  if (locLocator = nil) then
    Exit;

  if Assigned(FChildCursor) then begin
    locOldCurrentModule := SymbolTable.CurrentModule;
    try
      locUsesList := FModule.InterfaceSection.UsesList;
      crsSchemaChild := FChildCursor.Clone() as IObjectCursor;
      strFilter := CreateQualifiedNameFilterStr(s_include,FXSShortNames);
      crsSchemaChild := CreateCursorOn(crsSchemaChild,ParseFilter(strFilter,TDOMNodeRttiExposer));
      crsSchemaChild.Reset();
      while crsSchemaChild.MoveNext() do begin
        includeNode := (crsSchemaChild.GetCurrent() as TDOMNodeRttiExposer).InnerObject as TDOMElement;
        if (includeNode.Attributes <> nil) and (includeNode.Attributes.Length > 0) then begin
          locFileName := NodeValue(includeNode.Attributes.GetNamedItem(s_schemaLocation));
          if not(IsStrEmpty(locFileName) or IsIncludedDoc(locFileName)) then begin
            if locLocator.Find(locFileName,includeDoc) then begin
              AddIncludedDoc(locFileName);
              locParser := TCustomXsdSchemaParserClass(Self.ClassType).Create(
                             includeDoc,
                             includeDoc.DocumentElement,
                             SymbolTable,
                             Self as IParserContext
                           );
              locContext := locParser as IParserContext;
              locContext.SetSimpleOptions(locContext.GetSimpleOptions() + [poParsingIncludeSchema]);
              locTempLocator := locLocator.Clone();
              locTempLocator.SetBasePath(locLocator.FindPath(locFileName));
              locContext.SetDocumentLocator(locTempLocator);
              locParser.SetNotifier(FOnMessage);
              locParser.ParseTypes();
              locModule := locContext.GetTargetModule();
              if (ExtractIdentifier(locContext.GetTargetNameSpace()) = locModule.Name) then begin
                s := ChangeFileExt(ExtractFileName(locFileName),'');
                i := 1;
                locName := s;
                while (FSymbols.FindModule(locName) <> nil) do begin
                  locName := Format('%s%d',[s,i]); 
                  Inc(i);
                end;
                locModule.Name := locName;
              end;  
              if (locModule <> FModule) and (locUsesList.IndexOf(locModule) = -1) then begin
                locModule.AddRef();
                locUsesList.Add(locModule);
              end;
            end else begin
              DoOnMessage(mtError,Format(SERR_FileNotFound,[locFileName]));
            end;
          end;
        end;
      end;
    finally
      SymbolTable.SetCurrentModule(locOldCurrentModule);
    end;
  end;
end;

function TCustomXsdSchemaParser.FindNamedNode(
        AList : IObjectCursor;
  const AName : WideString;
  const AOrder : Integer
): TDOMNode;
var
  attCrs, crs : IObjectCursor;
  curObj : TDOMNodeRttiExposer;
  fltr : IObjectFilter;
  locOrder : Integer;
begin
  Result := nil;
  if Assigned(AList) then begin
    fltr := ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_name)]),TDOMNodeRttiExposer);
    AList.Reset();
    locOrder := AOrder;
    while AList.MoveNext() do begin
      curObj := AList.GetCurrent() as TDOMNodeRttiExposer;
      attCrs := CreateAttributesCursor(curObj.InnerObject,cetRttiNode);
      if Assigned(attCrs) then begin
        crs := CreateCursorOn(attCrs,fltr);
        crs.Reset();
        if crs.MoveNext() and AnsiSameText(AName,TDOMNodeRttiExposer(crs.GetCurrent()).NodeValue) then begin
          Dec(locOrder);
          if ( locOrder <= 0 ) then begin
            Result := curObj.InnerObject;
            exit;
          end;
        end;
      end;
    end;
  end;
end;

function TCustomXsdSchemaParser.FindNameSpace(
  const AShortName : string;
  out   AResult : string
) : Boolean;
var
  i : Integer;
  ls : TStrings;
  pc : IParserContext;
begin
  AResult := '';
  Result := False;
  for i := 0 to Pred(FNameSpaceList.Count) do begin
    ls := FNameSpaceList.Objects[i] as TStrings;
    if ( ls.IndexOf(AShortName) >= 0 ) then begin
      AResult := FNameSpaceList[i];
      Result := True;
      Break;
    end;
  end;
  if not Result then begin
    pc := GetParentContext();
    if ( pc <> nil ) then
      Result := GetParentContext().FindNameSpace(AShortName,AResult);
  end;
end;

function TCustomXsdSchemaParser.FindShortNamesForNameSpace(const ANameSpace: string): TStrings;
var
  prtCtx : IParserContext;
begin
  Result := FindShortNamesForNameSpaceLocal(ANameSpace);
  if ( Result = nil ) then begin
    prtCtx := GetParentContext();
    if Assigned(prtCtx) then
      Result := prtCtx.FindShortNamesForNameSpace(ANameSpace);
  end;
end;

function TCustomXsdSchemaParser.GetDocumentLocator(): IDocumentLocator;
begin
  Result := FDocumentLocator;
  if (Result = nil) and (FParentContext <> nil) then
    Result := GetParentContext().GetDocumentLocator();
end;

procedure TCustomXsdSchemaParser.SetDocumentLocator(ALocator: IDocumentLocator);
begin
  FDocumentLocator := ALocator;
end;

function TCustomXsdSchemaParser.GetSimpleOptions(): TParserOptions;
begin
  Result := FSimpleOptions;
end;

procedure TCustomXsdSchemaParser.SetSimpleOptions(const AValue: TParserOptions);
begin
  if ( AValue <> FSimpleOptions ) then
    FSimpleOptions := AValue;
end;

procedure TCustomXsdSchemaParser.AddTypeToCheck(AType: TPasType);
begin
  if (AType = nil) then
    exit;
  if HasParentContext() then begin
    GetParentContext().AddTypeToCheck(AType);
    exit;
  end;
  if (FCheckedTypes = nil) then
    FCheckedTypes := TList2.Create();
  if (FCheckedTypes.IndexOf(AType) = -1) then
    FCheckedTypes.Add(AType);
end;

procedure TCustomXsdSchemaParser.AddIncludedDoc(ADocLocation : string);
begin
  if (poParsingIncludeSchema in FSimpleOptions) then begin
    GetParentContext().AddIncludedDoc(ADocLocation);
    exit;
  end;

  if (FIncludeList = nil) then
    CreateIncludeList();
  FIncludeList.Add(ADocLocation);
end;

function TCustomXsdSchemaParser.IsIncludedDoc(ADocLocation : string) : Boolean;
begin
  Result := False;
  if (poParsingIncludeSchema in FSimpleOptions) then
    Result := GetParentContext().IsIncludedDoc(ADocLocation);
  if not Result then
    Result := (FIncludeList <> nil) and (FIncludeList.IndexOf(ADocLocation) <> -1);
end;

procedure TCustomXsdSchemaParser.SetNotifier(ANotifier: TOnParserMessage);
begin
  FOnMessage := ANotifier;
end;

function TCustomXsdSchemaParser.FindShortNamesForNameSpaceLocal(const ANameSpace: string): TStrings;
var
  i : Integer;
begin
  i := FNameSpaceList.IndexOf(ANameSpace);
  if ( i >= 0 ) then
    Result := FNameSpaceList.Objects[i] as TStrings
  else
    Result := nil;
end;

function TCustomXsdSchemaParser.GetParentContext() : IParserContext;
begin
  Result := IParserContext(FParentContext);
end;

function TCustomXsdSchemaParser.HasParentContext() : Boolean;
begin
  Result := (FParentContext <> nil);
end;

function TCustomXsdSchemaParser.GetSymbolTable() : TwstPasTreeContainer;
begin
  Result := FSymbols;
end;

function TCustomXsdSchemaParser.GetTargetModule() : TPasModule;
begin
  Prepare(True);
  Result := FModule;
end;

function TCustomXsdSchemaParser.GetTargetNameSpace() : string;
begin
  Prepare(True);
  Result := FTargetNameSpace;
end;

function TCustomXsdSchemaParser.GetXsShortNames() : TStrings;
begin
  Result := FXSShortNames;
end;

function TCustomXsdSchemaParser.ParseType(const AName, ATypeKind : string): TPasType;
begin
  Result := InternalParseType(AName,nil);
end;

function TCustomXsdSchemaParser.ParseType(
  const AName : string;  
  const ATypeNode : TDOMNode
) : TPasType; 
begin
  Result := InternalParseType(AName,ATypeNode);
end;

function TCustomXsdSchemaParser.InternalParseType(
  const AName : string;
  const ATypeNode : TDOMNode
): TPasType;
var
  crsSchemaChild : IObjectCursor;
  typNd : TDOMNode;
  typName : string;
  embededType : Boolean;
  localTypeName : string;

  procedure Init();
  begin
    crsSchemaChild := FChildCursor.Clone() as IObjectCursor;
  end;

  function ExtractTypeHint(AElement: TDOMNode): string;
  begin
    if not wst_findCustomAttributeXsd(FXSShortNames,AElement,s_WST_typeHint,Result) then
      Result := '';
  end;

  function FindTypeNode(out ASimpleTypeAlias : TPasType; out AIsAlias : Boolean) : Boolean;
  var
    nd, oldTypeNode : TDOMNode;
    crs : IObjectCursor;
    locStrFilter, locTypeHint : string;
    locHintedType : TPasType;
  begin
    ASimpleTypeAlias := nil;
    AIsAlias := False;
    Result := True;
    if ( ATypeNode <> nil ) then
      typNd := ATypeNode
    else
      typNd := FindNamedNode(crsSchemaChild,localTypeName);
    if not Assigned(typNd) then
      raise EXsdTypeNotFoundException.CreateFmt(SERR_TypeDefinitionNotFound,['1',AName]);
    if AnsiSameText(ExtractNameFromQName(typNd.NodeName),s_element) or
       AnsiSameText(ExtractNameFromQName(typNd.NodeName),s_attribute)
    then begin
      crs := CreateCursorOn(CreateAttributesCursor(typNd,cetRttiNode),ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_type)]),TDOMNodeRttiExposer));
      crs.Reset();
      if crs.MoveNext() then begin
        AIsAlias := True;
        nd := (crs.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
        ASimpleTypeAlias := FindElement(ExtractNameFromQName(nd.NodeValue)) as TPasType;
        if Assigned(ASimpleTypeAlias) then begin
          if ASimpleTypeAlias.InheritsFrom(TPasNativeSimpleType) then begin
            locTypeHint := ExtractTypeHint(typNd);
            if not IsStrEmpty(locTypeHint) then begin
              locHintedType := FindElement(locTypeHint,[elkName]) as TPasType;
              if ( locHintedType <> nil ) then
                ASimpleTypeAlias := locHintedType;
            end;
          end;
          Result := False;
        end else begin
          oldTypeNode := typNd;
          typNd := FindNamedNode(crsSchemaChild,ExtractNameFromQName(nd.NodeValue));
          if not Assigned(typNd) then
            raise EXsdTypeNotFoundException.CreateFmt(SERR_TypeDefinitionNotFound,['2',AName]);
          embededType := False;
          if ( typNd = oldTypeNode ) then begin
            typNd := FindNamedNode(crsSchemaChild,ExtractNameFromQName(nd.NodeValue),2);
            if not Assigned(typNd) then
              raise EXsdTypeNotFoundException.CreateFmt(SERR_TypeDefinitionNotFound,['2.1',AName]);
          end;
        end;
      end else begin
        //locStrFilter := Format('%s = %s or %s = %s ',[s_NODE_NAME,QuotedStr(s_complexType),s_NODE_NAME,QuotedStr(s_simpleType)]);
        locStrFilter := CreateQualifiedNameFilterStr(s_complexType,FXSShortNames) + ' or ' +
                        CreateQualifiedNameFilterStr(s_simpleType,FXSShortNames);
        crs := CreateCursorOn(CreateChildrenCursor(typNd,cetRttiNode),ParseFilter(locStrFilter,TDOMNodeRttiExposer));
        crs.Reset();
        if not crs.MoveNext() then begin
          raise EXsdTypeNotFoundException.CreateFmt(SERR_TypeDefinitionNotFound,['3',AName]);
        end;
        typNd := (crs.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
        typName := ExtractNameFromQName(AName);
        embededType := True;
      end;
    end;
  end;

  function ParseComplexType():TPasType;
  var
    locParser : TComplexTypeParser;
  begin
    locParser := TComplexTypeParser.Create(Self,typNd,typName,embededType);
    try
      Result := locParser.Parse();
    finally
      FreeAndNil(locParser);
    end;
  end;

  function ParseSimpleType():TPasType;
  var
    locParser : TSimpleTypeParser;
  begin
    locParser := TSimpleTypeParser.Create(Self,typNd,typName,embededType);
    try
      Result := locParser.Parse();
    finally
      FreeAndNil(locParser);
    end;
  end;

  function CreateTypeAlias(const ABase : TPasType): TPasType;
  var
    hasInterName : Boolean;
    baseName,internalName : string;
  begin
    baseName := ExtractNameFromQName(AName);
    internalName := ExtractIdentifier(baseName);
    hasInterName := IsReservedKeyWord(internalName) or
                    not(IsValidIdent(internalName)) or
                    SameText(internalName,Self.Module.Name);
    if hasInterName then begin
      internalName := '_' + internalName;
    end;
    Result := TPasType(SymbolTable.CreateElement(TPasAliasType,internalName,Self.Module.InterfaceSection,visDefault,'',0));
    SymbolTable.RegisterExternalAlias(Result,baseName);
    TPasAliasType(Result).DestType := ABase;
    ABase.AddRef();
  end;

  function CreateUnresolveType(): TPasType;
  var
    hasInternameName : Boolean;
    internameName, baseName : string;
  begin
    baseName := ExtractNameFromQName(AName);
    internameName := ExtractIdentifier(baseName);
    hasInternameName := IsReservedKeyWord(baseName) or
                        (not IsValidIdent(internameName));
    if hasInternameName then begin
      internameName := '_' + internameName;
    end;
    Result := TPasUnresolvedTypeRef(SymbolTable.CreateElement(TPasUnresolvedTypeRef,internameName,SymbolTable.CurrentModule.InterfaceSection,visDefault,'',0));
    if not AnsiSameText(internameName,baseName) then
      SymbolTable.RegisterExternalAlias(Result,baseName);
  end;

var
  frwType, aliasType : TPasType;
  sct : TPasSection;
  shortNameSpace, longNameSpace : string;
  typeModule : TPasModule;
  locTypeNodeFound, IsAlias : Boolean;
  locNodeTag : string;
begin
  Prepare(True);
  if not FImportParsed then
    ParseImportDocuments();
  if not FIncludeParsed then
    ParseIncludeDocuments();
  sct := nil;
  DoOnMessage(mtInfo, Format(SERR_Parsing,[AName]));
  try
    embededType := False;
    aliasType := nil;
    ExplodeQName(AName,localTypeName,shortNameSpace);
    if IsStrEmpty(shortNameSpace) then begin
      typeModule := FModule;
    end else begin
      if not FindNameSpace(shortNameSpace,longNameSpace) then
        raise EXsdParserAssertException.CreateFmt(SERR_UnableToResolveNamespace,[shortNameSpace]);
      typeModule := SymbolTable.FindModule(longNameSpace);
    end;
    if ( typeModule = nil ) then
      raise EXsdTypeNotFoundException.Create(AName);
    Result := SymbolTable.FindElementInModule(localTypeName,typeModule) as TPasType;
    if (Result <> nil) and (not Result.InheritsFrom(TPasUnresolvedTypeRef)) then
      Exit;
    Init();
    locTypeNodeFound := FindTypeNode(aliasType,IsAlias);
    if ( Result <> nil ) and ( typeModule = FModule ) and
       ( not Result.InheritsFrom(TPasUnresolvedTypeRef) )
    then begin                              
      if locTypeNodeFound and ( embededType <> ( SymbolTable.Properties.GetValue(Result,sEMBEDDED_TYPE) = '1' ) ) then
        Result := nil;
    end;
    if ( ( Result = nil ) or Result.InheritsFrom(TPasUnresolvedTypeRef) ) and
       ( typeModule = FModule )
    then begin
      sct := FModule.InterfaceSection;
      frwType := Result;
      Result := nil;
      Init();
      if locTypeNodeFound {FindTypeNode(aliasType)} then begin
        locNodeTag := ExtractNameFromQName(typNd.NodeName);
        if (locNodeTag = s_complexType) or (locNodeTag = s_group) or
           (locNodeTag = s_attributeGroup)
        then begin
          Result := ParseComplexType();
        end else if (locNodeTag = s_simpleType) then begin
          Result := ParseSimpleType();
        end;
        if Assigned(Result) then begin
          if Assigned(frwType) and AnsiSameText(SymbolTable.GetExternalName(Result),SymbolTable.GetExternalName(frwType)) then begin
            Result.Name := frwType.Name;
            SymbolTable.RegisterExternalAlias(Result,SymbolTable.GetExternalName(frwType));
          end;
        end else begin
          raise EXsdTypeNotFoundException.CreateFmt(SERR_TypeNodeFoundButUnableToParseIt,[AName]);
        end;
      end else begin
        Result := CreateTypeAlias(aliasType);
      end;
      if ( frwType <> nil ) then begin
        sct.Declarations.Extract(frwType);
        sct.Types.Extract(frwType);
        frwType.Release();
      end;
      sct.Declarations.Add(Result);
      sct.Types.Add(Result);
      if Result.InheritsFrom(TPasClassType) then
        sct.Classes.Add(Result);
      if IsAlias and (aliasType = nil) then begin
        Result := CreateTypeAlias(Result);
        sct.Declarations.Add(Result);
        sct.Types.Add(Result);
      end;
    end;
  except
    on e : EXsdTypeNotFoundException do begin
      Result := CreateUnresolveType();
      if ( sct = nil ) then
        sct := FModule.InterfaceSection;
      sct.Declarations.Add(Result);
      sct.Types.Add(Result);
    end;
  end;
end;

procedure TCustomXsdSchemaParser.CreateImportParsers(); 
var
  crsSchemaChild : IObjectCursor;
  strFilter, locFileName, locNameSpace : string;
  importNode : TDOMElement;
  importDoc : TXMLDocument;
  locParser : IXsdPaser;
  locOldCurrentModule : TPasModule;
  locContinue : Boolean;
  locLocator, loctempLocator : IDocumentLocator;
  locContext : IParserContext; 
  locUsesList : TList2;
  locModule : TPasModule;
  locName, s : string;
  i : Integer;             
begin
  if FImportParsed then
    Exit;
  Prepare(True);
  locLocator := GetDocumentLocator();
  if (locLocator = nil) then
    Exit;

  if Assigned(FChildCursor) then begin
    locOldCurrentModule := SymbolTable.CurrentModule;
    try
      locUsesList := FModule.InterfaceSection.UsesList;
      crsSchemaChild := FChildCursor.Clone() as IObjectCursor;
      strFilter := CreateQualifiedNameFilterStr(s_import,FXSShortNames);
      crsSchemaChild := CreateCursorOn(crsSchemaChild,ParseFilter(strFilter,TDOMNodeRttiExposer));
      crsSchemaChild.Reset();
      while crsSchemaChild.MoveNext() do begin
        importNode := (crsSchemaChild.GetCurrent() as TDOMNodeRttiExposer).InnerObject as TDOMElement;
        if ( importNode.Attributes <> nil ) and ( importNode.Attributes.Length > 0 ) then begin
          locFileName := NodeValue(importNode.Attributes.GetNamedItem(s_schemaLocation));
          if not IsStrEmpty(locFileName) then begin
            if locLocator.Find(locFileName,importDoc) then begin
              locNameSpace := NodeValue(importNode.Attributes.GetNamedItem(s_namespace));
              locContinue := IsStrEmpty(locNameSpace) or (FXsdParsers = nil) or (FXsdParsers.IndexOf(locNameSpace) = -1);//( SymbolTable.FindModule(locNameSpace) = nil );
              if locContinue then begin
                if (FXsdParsers = nil) then begin
                  FXsdParsers := TStringList.Create();
                  FXsdParsers.Duplicates := dupIgnore;
                  FXsdParsers.Sorted := True;
                end;
                locParser := TCustomXsdSchemaParserClass(Self.ClassType).Create(
                               importDoc,
                               importDoc.DocumentElement,
                               SymbolTable,
                               Self as IParserContext
                             );
                locContext := locParser as IParserContext;
                loctempLocator := locLocator.Clone();
                loctempLocator.SetBasePath(locLocator.FindPath(locFileName));
                locContext.SetDocumentLocator(loctempLocator);
                FXsdParsers.AddObject(locNameSpace,TIntfObjectRef.Create(locParser));
                locParser.SetNotifier(FOnMessage);
                //locParser.ParseTypes();
                locModule := locContext.GetTargetModule();
                if (locModule <> FModule) and (locUsesList.IndexOf(locModule) = -1) then begin
                  s := ChangeFileExt(ExtractFileName(locFileName),'');
                  s := ExtractIdentifier(s);
                  i := 1;
                  locName := s;
                  if (locModule.Name <> locName) then begin
                    while (FSymbols.FindModule(locName) <> nil) do begin
                      locName := Format('%s%d',[s,i]);
                      Inc(i);
                    end;
                    locModule.Name := locName;
                  end;
                  locModule.AddRef();
                  locUsesList.Add(locModule);
                  if (FSymbols.Properties.GetValue(locModule,sFILE_NAME) = '') then
                    FSymbols.Properties.SetValue(locModule,sFILE_NAME,locFileName);
                end;                          
              end;
            end else begin
              DoOnMessage(mtError,Format(SERR_FileNotFound,[locFileName]));
            end;
          end;
        end;
      end;
    finally
      SymbolTable.SetCurrentModule(locOldCurrentModule);
    end;
  end;
end;

procedure TCustomXsdSchemaParser.ParseTypes();
var
  crsSchemaChild, typTmpCrs : IObjectCursor;
  typFilterStr : string;
  typNode : TDOMNode;
begin
  Prepare(True);
  ParseImportDocuments();
  ParseIncludeDocuments();
  if Assigned(FChildCursor) then begin
    crsSchemaChild := FChildCursor.Clone() as IObjectCursor;
    typFilterStr := Format(
                      '%s or %s or %s or %s or %s or %s',
                      [ CreateQualifiedNameFilterStr(s_complexType,FXSShortNames),
                        CreateQualifiedNameFilterStr(s_simpleType,FXSShortNames),
                        CreateQualifiedNameFilterStr(s_element,FXSShortNames),
                        CreateQualifiedNameFilterStr(s_attribute,FXSShortNames),
                        CreateQualifiedNameFilterStr(s_group,FXSShortNames),
                        CreateQualifiedNameFilterStr(s_attributeGroup,FXSShortNames)
                      ]
                    );
    crsSchemaChild := CreateCursorOn(crsSchemaChild,ParseFilter(typFilterStr,TDOMNodeRttiExposer));
    crsSchemaChild.Reset();
    while crsSchemaChild.MoveNext() do begin
      typNode := (crsSchemaChild.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
      typTmpCrs := CreateAttributesCursor(typNode,cetRttiNode);
      if Assigned(typTmpCrs) then begin
        typTmpCrs.Reset();
        typTmpCrs := CreateCursorOn(typTmpCrs,ParseFilter(Format('%s=%s',[s_NODE_NAME,QuotedStr(s_name)]),TDOMNodeRttiExposer));
        typTmpCrs.Reset();
        if typTmpCrs.MoveNext() then begin
          InternalParseType(
            (typTmpCrs.GetCurrent() as TDOMNodeRttiExposer).NodeValue,
            typNode
          );
        end;
      end;
    end;
  end;
  if (FCheckedTypes <> nil) and (FCheckedTypes.Count > 0) then
    CheckDuplicatedProperties(FCheckedTypes,FSymbols);
end;

procedure TCustomXsdSchemaParser.Prepare(const AMustSucceed : Boolean);
var
  locAttCursor : IObjectCursor;
  prntCtx : IParserContext;
  nd : TDOMNode;
  i : Integer;
  ls : TStrings;
  ok : Boolean;
  eltForm, attForm : string;
begin
  if FPrepared then
    exit;

  FTargetNameSpace := '';
  eltForm := '';
  attForm := '';
  ok := False;
  if (FSchemaNode.Attributes <> nil) and (GetNodeListCount(FSchemaNode.Attributes) > 0) then begin
    nd := FSchemaNode.Attributes.GetNamedItem(s_targetNamespace);
    if (nd <> nil) then begin
      FTargetNameSpace := nd.NodeValue;
      ok := True;
    end;
    eltForm := Trim(NodeValue(FSchemaNode.Attributes.GetNamedItem(s_elementFormDefault)));
    attForm := Trim(NodeValue(FSchemaNode.Attributes.GetNamedItem(s_attributeFormDefault)));
  end;
  prntCtx := GetParentContext();
  if not ok then begin
    if (poParsingIncludeSchema in FSimpleOptions) and (prntCtx <> nil) then begin
      FTargetNameSpace := prntCtx.GetTargetNameSpace();
      ok := True;
    end else begin
      if (prntCtx <> nil) then begin
        FTargetNameSpace := prntCtx.GetTargetNameSpace();
        ok := (FTargetNameSpace <> '');
      end;
      if not ok then begin
        if not AMustSucceed then
          exit;
        raise EXsdParserAssertException.CreateFmt(SERR_SchemaNodeRequiredAttribute,[s_targetNamespace]);
      end;
    end;
  end;

  FPrepared := True;
  if IsStrEmpty(FModuleName) then
    FModuleName := ExtractIdentifier(FTargetNameSpace);
  if ( SymbolTable.FindModule(s_xs) = nil ) then begin
    CreateWstInterfaceSymbolTable(SymbolTable);
  end;
  FChildCursor := CreateChildrenCursor(FSchemaNode,cetRttiNode);

  locAttCursor := CreateAttributesCursor(FSchemaNode,cetRttiNode);
  BuildNameSpaceList(locAttCursor,FNameSpaceList);
  FXSShortNames := FindShortNamesForNameSpaceLocal(s_xs);
  if ( FXSShortNames = nil ) then begin
    if ( prntCtx = nil ) then
      raise EXsdParserAssertException.CreateFmt(SERR_InvalidSchemaDoc_NamespaceNotFound,[s_xs]);
    FXSShortNames := prntCtx.FindShortNamesForNameSpace(s_xs);
    if ( FXSShortNames = nil ) then
      raise EXsdParserAssertException.CreateFmt(SERR_InvalidSchemaDoc_NamespaceNotFoundShort,[s_xs]);
  end;

  if Assigned(prntCtx) then begin
    for i:= 0 to Pred(FNameSpaceList.Count) do begin
      ls := prntCtx.FindShortNamesForNameSpace(FNameSpaceList[i]);
      if Assigned(ls) then
        (FNameSpaceList.Objects[i] as TStrings).AddStrings(ls);
    end;
  end;

  FModule := SymbolTable.FindModule(FTargetNameSpace);
  if ( FModule = nil ) then begin
    FModule := TPasModule(SymbolTable.CreateElement(TPasModule,FModuleName,SymbolTable.Package,visDefault,'',0));
    SymbolTable.Package.Modules.Add(FModule);
    SymbolTable.RegisterExternalAlias(FModule,FTargetNameSpace);
    FModule.InterfaceSection := TInterfaceSection(SymbolTable.CreateElement(TInterfaceSection,'',FModule,visDefault,'',0));
  end;
  if (eltForm <> '') then
    SymbolTable.Properties.SetValue(FModule,s_elementFormDefault,eltForm);
  if (attForm <> '') then
    SymbolTable.Properties.SetValue(FModule,s_attributeFormDefault,attForm);
end;

{ TXsdParser }

constructor TXsdParser.Create(
        ADoc : TXMLDocument;
        ASymbols : TwstPasTreeContainer;
  const AModuleName : string;
  const ANotifier : TOnParserMessage
);
var
  locName : string;
begin
  inherited Create(ADoc,ADoc.DocumentElement,ASymbols,nil);
  if Assigned(ANotifier) then
    FOnMessage := ANotifier;
  if not IsStrEmpty(AModuleName) then begin
    locName := ExtractIdentifier(AModuleName);
    if not IsStrEmpty(locName) then begin
      FModuleName := locName;
      if (Module <> nil) then
        Module.Name := FModuleName;
    end;
  end;
end;

end.
