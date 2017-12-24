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
unit wsdl_parser;

interface
uses
  Classes, SysUtils,
  {$IFNDEF FPC}xmldom, wst_delphi_xml{$ELSE}DOM{$ENDIF},
  cursor_intf, rtti_filters,
  pastree, PScanner, pascal_parser_intf, logger_intf, xsd_parser;

const
  s_TRANSPORT  = 'TRANSPORT';
  s_FORMAT     = 'FORMAT';
  
type

  TWsdlSchemaParser = class(TCustomXsdSchemaParser)
  end;

  TParserMode = ( pmUsedTypes, pmAllTypes );

  IParser = interface
    ['{DE9D8592-150A-4FEC-BCB8-9EDB702EC8E7}']
    procedure Execute(const AMode : TParserMode; const AModuleName : string);
  end;

  { TWsdlParser }

  TWsdlParser = class(TInterfacedObject, IInterface, IParserContext, IParser)
  private
    FDoc : TXMLDocument;
    FSymbols : TwstPasTreeContainer;
    FModule : TPasModule;
    FDocumentLocator : IDocumentLocator;
  private
    FTargetNameSpace : string;
    FNameSpaceList : TStringList;
    FXsdParsers : TStringList;
    FWsdlShortNames : TStrings;
    FSoapShortNames : TStrings;
    FXSShortNames : TStrings;
    FChildCursor : IObjectCursor;
    FServiceCursor : IObjectCursor;
    FBindingCursor : IObjectCursor;
    FPortTypeCursor : IObjectCursor;
    FMessageCursor : IObjectCursor;
    FTypesCursor : IObjectCursor;
    FSchemaCursor : IObjectCursor;
    FOnMessage: TOnParserMessage;
    FSimpleOptions : TParserOptions;
    FCheckedTypes : TList2;
    FIncludeList : TStringList;
  private
    procedure DoOnMessage(const AMsgType : TMessageType; const AMsg : string);
    function AddNameSpace(const AValue : string) : TStrings;
    procedure CreateIncludeList();
  private
    function CreateWsdlNameFilter(const AName : WideString):IObjectFilter;
    function FindNamedNode(AList : IObjectCursor; const AName : WideString; const AOrder : Integer = 0):TDOMNode;
    procedure Prepare(const AModuleName : string);
    procedure ParseService(ANode : TDOMNode);
    procedure ParsePort(ANode : TDOMNode);
    function ParsePortType(
            ANode, ABindingNode : TDOMNode;
      const ABindingStyle : string
    ) : TPasClassType;
    function ParseOperation(
            AOwner : TPasClassType;
            ANode  : TDOMNode;
      const ASoapBindingStyle : string
    ) : TPasProcedure;
    function FindParser(const ANamespace : string) : IXsdPaser;
    function GetParser(const ANamespace : string) : IXsdPaser;
    function ParseType(
      const AName : string; 
      const AHint : string = '';
      const ATypeOrElement : string = ''
    ) : TPasType;
    procedure ParseTypes();
  protected
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
  public
    constructor Create(
            ADoc : TXMLDocument;
            ASymbols : TwstPasTreeContainer;
      const ANotifier : TOnParserMessage = nil
    );
    destructor Destroy();override;
    procedure Execute(const AMode : TParserMode; const AModuleName : string);
    property SymbolTable : TwstPasTreeContainer read FSymbols;

    property OnMessage : TOnParserMessage read FOnMessage write FOnMessage;
  end;

implementation
uses
  ws_parser_imp, dom_cursors, parserutils, StrUtils, xsd_consts, TypInfo;


function StrToBindingStyle(const AStr : string):TBindingStyle;
begin
  if IsStrEmpty(AStr) then begin
    Result := bsDocument;
  end else if AnsiSameText(AStr,s_document) then begin
    Result := bsDocument;
  end else if AnsiSameText(AStr,s_rpc) then begin
    Result := bsRPC;
  end else begin
    Result := bsUnknown;
  end;
end;

{ TWsdlParser }

function TWsdlParser.AddNameSpace(const AValue: string): TStrings;
var
  i : Integer;
  s : string;
  ls : TStringList;
begin
  s := AValue;//Trim(AValue);
  i := FNameSpaceList.IndexOf(s);
  if ( i < 0 ) then begin
    ls := TStringList.Create();
    FNameSpaceList.AddObject(s,ls);
    ls.Duplicates := dupIgnore;
    ls.Sorted := True;
    Result := ls;
  end else begin
    Result := FNameSpaceList.Objects[i] as TStrings;
  end;
end;

procedure TWsdlParser.CreateIncludeList();
begin
  if (FIncludeList = nil) then begin
    FIncludeList := TStringList.Create();
    FIncludeList.Duplicates := dupIgnore;
    FIncludeList.Sorted := True;
  end;
end;

constructor TWsdlParser.Create(
            ADoc : TXMLDocument;
            ASymbols : TwstPasTreeContainer;
      const ANotifier : TOnParserMessage
);
begin
  Assert(Assigned(ADoc));
  Assert(Assigned(ASymbols));
  inherited Create();
  FDoc := ADoc;
  if Assigned(ANotifier) then
    FOnMessage := ANotifier;

  FNameSpaceList := TStringList.Create();
  FNameSpaceList.Duplicates := dupIgnore;
  FNameSpaceList.Sorted := True;

  FXsdParsers := TStringList.Create();
  FXsdParsers.Duplicates := dupIgnore;
  FXsdParsers.Sorted := True;

  FSymbols := ASymbols;
  FCheckedTypes := TList2.Create();
end;

function TWsdlParser.CreateWsdlNameFilter(const AName: WideString): IObjectFilter;
begin
  Result := ParseFilter(CreateQualifiedNameFilterStr(AName,FWsdlShortNames),TDOMNodeRttiExposer);
end;

destructor TWsdlParser.Destroy();

  procedure FreeList(AList : TStrings);
  var
    j : Integer;
  begin
    if Assigned(AList) and (AList.Count > 0) then begin
      for j := Pred(AList.Count)  downto 0 do begin
        AList.Objects[j].Free();
        AList.Objects[j] := nil;
      end;
    end;
    FreeAndNil(AList);
  end;
  
begin
  FCheckedTypes.Free();
  FreeAndNil(FIncludeList);
  FreeList(FXsdParsers);
  FreeList(FNameSpaceList);
  inherited;
end;

procedure TWsdlParser.DoOnMessage(const AMsgType: TMessageType; const AMsg: string);
begin
  if Assigned(FOnMessage) then begin
    FOnMessage(AMsgType,AMsg);
  end else if IsConsole then begin
    if HasLogger() then
      GetLogger().Log(AMsgType, AMsg);
  end;
end;

function TWsdlParser.FindNamedNode(
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

function TWsdlParser.FindNameSpace(const AShortName: string; out AResult: string): Boolean;
var
  i : Integer;
  ls : TStrings;
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
end;

function TWsdlParser.FindShortNamesForNameSpace(const ANameSpace: string): TStrings;
var
  i : Integer;
begin
  i := FNameSpaceList.IndexOf(ANameSpace);
  if ( i >= 0 ) then
    Result := FNameSpaceList.Objects[i] as TStrings
  else
    Result := nil;
end;

function TWsdlParser.GetSymbolTable() : TwstPasTreeContainer;
begin
  Result := FSymbols;
end;

function TWsdlParser.GetTargetModule() : TPasModule;
begin
  Result := FModule;
end;

function TWsdlParser.GetDocumentLocator(): IDocumentLocator;
begin
  Result := FDocumentLocator;
end;

procedure TWsdlParser.SetDocumentLocator(ALocator: IDocumentLocator);
begin
  FDocumentLocator := ALocator;
end;

function TWsdlParser.GetSimpleOptions(): TParserOptions;
begin
  Result := FSimpleOptions;
end;

procedure TWsdlParser.SetSimpleOptions(const AValue: TParserOptions);
begin
  if ( AValue <> FSimpleOptions ) then
    FSimpleOptions := AValue;
end;

procedure TWsdlParser.AddTypeToCheck(AType: TPasType);
begin
  if (AType = nil) then
    exit;
  if (FCheckedTypes = nil) then
    FCheckedTypes := TList2.Create();
  if (FCheckedTypes.IndexOf(AType) = -1) then
    FCheckedTypes.Add(AType);
end;

procedure TWsdlParser.AddIncludedDoc(ADocLocation : string);
begin
  if (FIncludeList = nil) then
    CreateIncludeList();
  FIncludeList.Add(ADocLocation);
end;

function TWsdlParser.IsIncludedDoc(ADocLocation : string) : Boolean;
begin
  Result := (FIncludeList <> nil) and (FIncludeList.IndexOf(ADocLocation) <> -1);
end;

function TWsdlParser.GetTargetNameSpace() : string;
begin
  Result := FTargetNameSpace;
end;

function TWsdlParser.GetXsShortNames() : TStrings;
begin
  Result := FXSShortNames;
end;

procedure TWsdlParser.Execute(const AMode: TParserMode; const AModuleName: string);

  procedure ParseForwardDeclarations();
  var
    i, c : Integer;
    sym, symNew : TPasElement;
    typeCursor : IObjectCursor;
    schmNode, tmpNode : TDOMNode;
    s : string;
    typeList : TList2;
    locXsdParser : IXsdPaser;
  begin
    if Assigned(FSchemaCursor) then begin
      FSchemaCursor.Reset();
      if FSchemaCursor.MoveNext() then begin
        schmNode := (FSchemaCursor.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
        if schmNode.HasChildNodes() then begin
          typeCursor := CreateChildrenCursor(schmNode,cetRttiNode);
          s := CreateQualifiedNameFilterStr(s_complexType,FXSShortNames) + ' or ' +
               CreateQualifiedNameFilterStr(s_simpleType,FXSShortNames) + ' or ' +
               CreateQualifiedNameFilterStr(s_element,FXSShortNames);
          typeCursor := CreateCursorOn(typeCursor,ParseFilter(s,TDOMNodeRttiExposer));
          typeCursor.Reset();
          if typeCursor.MoveNext() then begin
            typeList := FSymbols.CurrentModule.InterfaceSection.Declarations;
            c := typeList.Count;
            i := 0;
            while ( i < c ) do begin
              sym := TPasElement(typeList[i]);
              if sym.InheritsFrom(TPasUnresolvedTypeRef) then begin
                typeCursor.Reset();
                tmpNode := FindNamedNode(typeCursor,FSymbols.GetExternalName(sym));
                if Assigned(tmpNode) then begin
                  //symNew := ParseType(FSymbols.GetExternalName(sym));
                  locXsdParser := GetParser(schmNode.Attributes.GetNamedItem(s_targetNamespace).NodeValue);
                  symNew := locXsdParser.ParseType(FSymbols.GetExternalName(sym),tmpNode);
                  //symNew := ParseType(tmpNode.Attributes.GetNamedItem(s_name).NodeValue);
                  if ( sym <> symNew ) then begin
                    FModule.InterfaceSection.Declarations.Extract(sym);
                    FModule.InterfaceSection.Types.Extract(sym);
                    symNew.Name := sym.Name;
                    DoOnMessage(mtInfo,Format('forward type paring %s.',[symNew.Name]));
                    //sym.Release();
                  end;
                  i := 0; //Dec(i);
                  c := typeList.Count;
                end else begin
                  DoOnMessage(mtInfo, 'unable to find the node of this type : ' + sym.Name);
                end;
              end;
              Inc(i);
            end;
          end;
        end;
      end;
    end;
  end;

  procedure ExtractNameSpace();
  var
    tmpCrs : IObjectCursor;
    nd : TDOMNode;
    s : string;
  begin
    nd := FDoc.DocumentElement;
    if Assigned(nd.Attributes) and ( nd.Attributes.Length > 0 ) then begin
      tmpCrs := CreateCursorOn(
                  CreateAttributesCursor(nd,cetRttiNode),
                  ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_targetNamespace)]),TDOMNodeRttiExposer)
                );
      tmpCrs.Reset();
      if tmpCrs.MoveNext() then begin
        s := (tmpCrs.GetCurrent() as TDOMNodeRttiExposer).NodeValue;
        if not IsStrEmpty(s) then begin
          FSymbols.RegisterExternalAlias(FSymbols.CurrentModule,s);
        end;
      end;
    end;
  end;
  
  procedure FixUsesList();
  var
    locPrs : IParserContext;
    k : Integer;
    locModule : TPasModule;
    locIntfUsesList : TList2;
  begin
    FSymbols.Properties.SetValue(FModule,sNS_COUNT,IntToStr(FXsdParsers.Count));
    locIntfUsesList := FModule.InterfaceSection.UsesList;
    for k := 0 to Pred(FXsdParsers.Count) do begin
      FSymbols.Properties.SetValue(FModule,(sNS_ITEM+IntToStr(k+1)),FXsdParsers[k]);
      locPrs := (FXsdParsers.Objects[k] as TIntfObjectRef).Intf as IParserContext;
      locModule := locPrs.GetTargetModule();
      if (locModule <> nil) and (locModule <> FModule) and
         (locIntfUsesList.IndexOf(locModule) = -1) 
      then begin
        locModule.AddRef();
        locIntfUsesList.Add(locModule);
      end;  
    end; 
  end;

var
  locSrvcCrs : IObjectCursor;
  locObj : TDOMNodeRttiExposer;
  locOldNameKinds : TElementNameKinds;
begin
  locOldNameKinds := FSymbols.DefaultSearchNameKinds;
  FSymbols.DefaultSearchNameKinds := [elkDeclaredName];
  try
    Prepare(AModuleName);

    locSrvcCrs := FServiceCursor.Clone() as IObjectCursor;
    locSrvcCrs.Reset();
    while locSrvcCrs.MoveNext() do begin
      locObj := locSrvcCrs.GetCurrent() as TDOMNodeRttiExposer;
      ParseService(locObj.InnerObject);
    end;

    if ( AMode = pmAllTypes ) then begin
      ParseTypes();
    end;

    ParseForwardDeclarations();
    SymbolTable.SetCurrentModule(FModule);
    ExtractNameSpace();
    FixUsesList();
    if (FCheckedTypes.Count > 0) then
      CheckDuplicatedProperties(FCheckedTypes,SymbolTable);
  finally
    FSymbols.DefaultSearchNameKinds := locOldNameKinds;
  end;
end;

function TWsdlParser.ParseOperation(
        AOwner : TPasClassType;
        ANode  : TDOMNode;
  const ASoapBindingStyle : string
) : TPasProcedure;

  function ExtractOperationName(out AName : string):Boolean;
  var
    attCrs, crs : IObjectCursor;
  begin
    Result := False;
    AName := '';
    attCrs := CreateAttributesCursor(ANode,cetRttiNode);
    if Assigned(attCrs) then begin
      crs := CreateCursorOn(attCrs,ParseFilter(s_NODE_NAME + '=' + QuotedStr(s_name) ,TDOMNodeRttiExposer));
      crs.Reset();
      if crs.MoveNext() then begin
        AName := TDOMNodeRttiExposer(crs.GetCurrent()).NodeValue;
        Result := True;
        exit;
      end;
    end;
  end;

  function ExtractMsgName(const AMsgType : string; out AName : string) : Boolean;
  var
    chldCrs, crs : IObjectCursor;
  begin
    chldCrs := CreateChildrenCursor(ANode,cetRttiNode);
    if ( chldCrs <> nil ) then begin
      //crs := CreateCursorOn(chldCrs,ParseFilter(s_NODE_NAME + '=' + QuotedStr(AMsgType) ,TDOMNodeRttiExposer));
      crs := CreateCursorOn(chldCrs,CreateWsdlNameFilter(AMsgType));
      crs.Reset();
      if crs.MoveNext() then begin
        chldCrs := CreateAttributesCursor(TDOMNodeRttiExposer(crs.GetCurrent()).InnerObject,cetRttiNode);
        if ( chldCrs <> nil ) then begin
          crs := CreateCursorOn(chldCrs,ParseFilter(s_NODE_NAME + '=' + QuotedStr(s_message) ,TDOMNodeRttiExposer));
          crs.Reset();
          if crs.MoveNext() then begin
            AName := TDOMNodeRttiExposer(crs.GetCurrent()).NodeValue;
            Result := True;
            exit;
          end;
        end;
      end;
    end;
    Result := False;
  end;
  
  function FindMessageNode(const AName : string) : TDOMNode;
  begin
    Result := FindNamedNode(FMessageCursor.Clone() as IObjectCursor,ExtractNameFromQName(AName));
  end;
  
  function CreatePartCursor(AMsgNode : TDOMNode):IObjectCursor ;
  begin
    Result := CreateChildrenCursor(AMsgNode,cetRttiNode);
    if Assigned(Result) then
      Result := CreateCursorOn(Result,CreateWsdlNameFilter(s_part));
  end;

  function ExtractTypeHint(AElement : TDOMNode) : string;
  begin
    if not wst_findCustomAttributeXsd(FXSShortNames,AElement,s_WST_typeHint,Result) then
      Result := '';
  end;
  
  function GetDataType(const AName, ATypeOrElement : string; const ATypeHint : string = ''):TPasType;
  begin
    try
      Result := ParseType(AName,ATypeHint,ATypeOrElement);
    except
      on e : Exception do begin
        DoOnMessage(mtError, e.Message + ' ' + AName + ' ' + ATypeOrElement);
        raise;
      end;
    end;
  end;
  
  procedure ParseParamAccess(AMessageNode : TDOMNode; AAccessList : TStrings);
  var
    nd : TDOMNode;
    tmpCrs : IObjectCursor;
    strBuffer, strToken : string;
  begin
    AAccessList.Clear();
    tmpCrs := CreateCursorOn(
                CreateChildrenCursor(AMessageNode,cetRttiNode),
                ParseFilter(CreateQualifiedNameFilterStr(s_documentation,FWsdlShortNames),TDOMNodeRttiExposer)
              );
    tmpCrs.Reset();
    if tmpCrs.MoveNext() then begin
      nd := (tmpCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
      if nd.HasChildNodes() then begin
        tmpCrs := CreateCursorOn(
                    CreateChildrenCursor(nd,cetRttiNode),
                    ParseFilter(Format('%s=%s',[s_NODE_NAME,QuotedStr(s_paramAccess)]),TDOMNodeRttiExposer)
                  );
        tmpCrs.Reset();
        if tmpCrs.MoveNext() then begin
          nd := (tmpCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
          if ( nd.Attributes <> nil ) then begin
            nd := nd.Attributes.GetNamedItem(s_value);
            if Assigned(nd) then
              strBuffer := Trim(nd.NodeValue);
          end;
        end;
      end;
    end;
    if ( Length(strBuffer) > 0 ) then begin
      while True do begin
        strToken := Trim(GetToken(strBuffer,';'));
        if ( Length(strToken) = 0 ) then
          Break;
        if ( Pos('=',strToken) < 1 ) then
          Break;
        AAccessList.Add(strToken);
      end;
    end;
  end;

  procedure ExtractMethod(
    const AMthdName : string;
    out   AMthd     : TPasProcedure
  );
  var
    tmpMthd : TPasProcedure;
    tmpMthdType : TPasProcedureType;

    procedure ParseInputMessage();
    var
      inMsg, strBuffer : string;
      inMsgNode, tmpNode : TDOMNode;
      crs, tmpCrs : IObjectCursor;
      prmName, prmTypeName, prmTypeType, prmTypeInternalName : string;
      prmInternameName : string;
      prmHasInternameName : Boolean;
      prmDef : TPasArgument;
      prmTypeDef : TPasType;
      prmAccess : TStringList;
      intBuffer : Integer;
    begin
      tmpMthdType := TPasProcedureType(SymbolTable.CreateElement(TPasProcedureType,'',tmpMthd,visDefault,'',0));
      tmpMthd.ProcType := tmpMthdType;
      if ExtractMsgName(s_input,inMsg) then begin
        inMsgNode := FindMessageNode(inMsg);
        if ( inMsgNode <> nil ) then begin
          crs := CreatePartCursor(inMsgNode);
          if ( crs <> nil ) then begin
            crs.Reset();
            prmAccess := TStringList.Create();
            try
              ParseParamAccess(inMsgNode,prmAccess);
              while crs.MoveNext() do begin
                tmpNode := TDOMNodeRttiExposer(crs.GetCurrent()).InnerObject;
                if ( tmpNode.Attributes = nil ) or ( tmpNode.Attributes.Length < 1 ) then begin
                  raise EXsdInvalidDefinitionException.CreateFmt('Invalid message part : "%s"',[tmpNode.NodeName]);
                end;
                strBuffer := s_NODE_NAME + '=' + QuotedStr(s_name);
                tmpCrs := CreateCursorOn(
                            CreateAttributesCursor(tmpNode,cetRttiNode),
                            ParseFilter(strBuffer,TDOMNodeRttiExposer)
                          );
                tmpCrs.Reset();
                if not tmpCrs.MoveNext() then begin
                  raise EXsdInvalidDefinitionException.CreateFmt('Invalid message part : "%s"',[tmpNode.NodeName]);
                end;
                prmName := TDOMNodeRttiExposer(tmpCrs.GetCurrent()).NodeValue;
                strBuffer := s_NODE_NAME + '=' + QuotedStr(s_element) + ' or ' + s_NODE_NAME + ' = ' + QuotedStr(s_type);
                tmpCrs := CreateCursorOn(
                            CreateAttributesCursor(tmpNode,cetRttiNode),
                            ParseFilter(strBuffer,TDOMNodeRttiExposer)
                          );
                tmpCrs.Reset();
                if not tmpCrs.MoveNext() then begin
                  raise EXsdInvalidDefinitionException.CreateFmt('Invalid message part : "%s"',[tmpNode.NodeName]);
                end;
                prmTypeName := TDOMNodeRttiExposer(tmpCrs.GetCurrent()).NodeValue;
                prmTypeType := TDOMNodeRttiExposer(tmpCrs.GetCurrent()).NodeName;
                if IsStrEmpty(prmName) or IsStrEmpty(prmTypeName) or IsStrEmpty(prmTypeType) then begin
                  raise EXsdInvalidDefinitionException.CreateFmt('Invalid message part : "%s"',[tmpNode.NodeName]);
                end;
                if SameText(s_document,ASoapBindingStyle) and
                   AnsiSameText(prmTypeType,s_element)
                then begin
                  prmName := ExtractNameFromQName(prmTypeName);
                end;
                prmInternameName := Trim(prmName);
                if AnsiSameText(prmInternameName,tmpMthd.Name) or 
                   AnsiSameText(prmInternameName,ExtractNameFromQName(prmTypeName)) 
                then begin
                  prmInternameName := prmInternameName + 'Param';
                end;
                prmInternameName := ExtractIdentifier(prmInternameName);
                prmHasInternameName := IsReservedKeyWord(prmInternameName) or
                                       ( not IsValidIdent(prmInternameName) ) or
                                       ( GetParameterIndex(tmpMthdType,prmInternameName) >= 0 );
                if prmHasInternameName then begin
                  prmInternameName := '_' + prmInternameName;
                end;
                prmHasInternameName := not AnsiSameText(prmInternameName,prmName);
                prmTypeDef := GetDataType(prmTypeName,prmTypeType,ExtractTypeHint(tmpNode));
                prmDef := TPasArgument(SymbolTable.CreateElement(TPasArgument,prmInternameName,tmpMthdType,visDefault,'',0));
                tmpMthdType.Args.Add(prmDef);
                prmDef.ArgType := prmTypeDef;
                prmTypeDef.AddRef();
                prmDef.Access := argConst;
                strBuffer := Trim(prmAccess.Values[prmName]);
                if ( Length(strBuffer) > 0 ) then begin
                  intBuffer := GetEnumValue(TypeInfo(TArgumentAccess),strBuffer);
                  if ( intBuffer > -1 ) then
                    prmDef.Access := TArgumentAccess(intBuffer);
                end;
                if prmHasInternameName or ( not AnsiSameText(prmName,prmInternameName) ) then begin
                  SymbolTable.RegisterExternalAlias(prmDef,prmName);
                end;
                if AnsiSameText(tmpMthd.Name,prmTypeDef.Name) then begin
                  prmTypeInternalName := prmTypeDef.Name + '_Type';
                  while Assigned(FSymbols.FindElement(prmTypeInternalName)) do begin
                    prmTypeInternalName := '_' + prmTypeInternalName;
                  end;
                  SymbolTable.RegisterExternalAlias(prmTypeDef,SymbolTable.GetExternalName(prmTypeDef));
                  prmTypeDef.Name := prmTypeInternalName;
                end;
              end;
            finally
              prmAccess.Free();
            end;
          end;
        end;
      end;
    end;

    procedure ParseOutputMessage();
    
      function FindIndexOfResultArg(AArgList : TList2) : Integer;
      const RESULT_ARG_NAMES : array[0..5] of string = ( 'result', 'return', '_result', 'result_', '_return', 'return_' );
      var
        p, q : Integer;
        idx_found : Boolean;
        resItemName : string;
        arg : TPasArgument;
      begin
        Result := -1;
        idx_found := False;
        p := Low(RESULT_ARG_NAMES);
        while ( not idx_found ) and ( p <= High(RESULT_ARG_NAMES) ) do begin
          resItemName := RESULT_ARG_NAMES[p];
          for q := 0 to Pred(AArgList.Count) do begin
            arg := TPasArgument(AArgList[q]);
            if ( arg.Access = argOut ) and ( LowerCase(arg.Name) = resItemName ) then begin
              idx_found := True;
              Break;
            end;
          end;
          Inc(p);
        end;
        if idx_found then
          Result := q
        else
          Result := AArgList.Count - 1;
      end;
      
    var
      outMsg, strBuffer : string;
      outMsgNode, tmpNode : TDOMNode;
      crs, tmpCrs : IObjectCursor;
      prmName, prmTypeName, prmTypeType : string;
      prmDef : TPasArgument;
      prmInternameName : string;
      prmHasInternameName : Boolean;
      locProcType : TPasProcedureType;
      locFunc : TPasFunction;
      locFuncType : TPasFunctionType;
      j : Integer;
      arg_a, arg_b : TPasArgument;
      resArgIndex : Integer;
      prmNameColisionWithInputParam : Boolean;
      prmTypeEntity : TPasType;
    begin
      if ExtractMsgName(s_output,outMsg) then begin
        outMsgNode := FindMessageNode(outMsg);
        if ( outMsgNode <> nil ) then begin
          crs := CreatePartCursor(outMsgNode);
          if ( crs <> nil ) then begin
            prmDef := nil;
            crs.Reset();
            while crs.MoveNext() do begin
              tmpNode := TDOMNodeRttiExposer(crs.GetCurrent()).InnerObject;
              if ( tmpNode.Attributes = nil ) or ( tmpNode.Attributes.Length < 1 ) then
                raise EXsdInvalidDefinitionException.CreateFmt('Invalid message part : "%s"',[tmpNode.NodeName]);
              strBuffer := s_NODE_NAME + '=' + QuotedStr(s_name);
              tmpCrs := CreateCursorOn(CreateAttributesCursor(tmpNode,cetRttiNode),ParseFilter(strBuffer,TDOMNodeRttiExposer));
              tmpCrs.Reset();
              if not tmpCrs.MoveNext() then
                raise EXsdInvalidDefinitionException.CreateFmt('Invalid message part : "%s"',[tmpNode.NodeName]);
              prmName := (tmpCrs.GetCurrent() as TDOMNodeRttiExposer).NodeValue;
              strBuffer := s_NODE_NAME + '=' + QuotedStr(s_element) + ' or ' + s_NODE_NAME + ' = ' + QuotedStr(s_type);
              tmpCrs := CreateCursorOn(
                          CreateAttributesCursor(tmpNode,cetRttiNode),
                          ParseFilter(strBuffer,TDOMNodeRttiExposer)
                        );
              tmpCrs.Reset();
              if not tmpCrs.MoveNext() then
                raise EXsdInvalidDefinitionException.CreateFmt('Invalid message part : "%s"',[tmpNode.NodeName]);
              prmTypeName := TDOMNodeRttiExposer(tmpCrs.GetCurrent()).NodeValue;
              prmTypeType := TDOMNodeRttiExposer(tmpCrs.GetCurrent()).NodeName;
              if IsStrEmpty(prmName) or IsStrEmpty(prmTypeName) or IsStrEmpty(prmTypeType) then
                raise EXsdInvalidDefinitionException.CreateFmt('Invalid message part : "%s"',[tmpNode.NodeName]);
              if SameText(s_document,ASoapBindingStyle) and
                 AnsiSameText(prmTypeType,s_element)
              then begin
                prmName := ExtractNameFromQName(prmTypeName);
              end;
              prmInternameName := Trim(prmName); 
              if AnsiSameText(prmInternameName,tmpMthd.Name) or 
                 AnsiSameText(prmInternameName,ExtractNameFromQName(prmTypeName)) 
              then begin
                prmInternameName := prmInternameName + 'Param';
              end;                            
                                                
              prmInternameName := ExtractIdentifier(prmInternameName);
              prmHasInternameName := IsReservedKeyWord(prmInternameName) or
                                     ( not IsValidIdent(prmInternameName) );
              if prmHasInternameName then
                prmInternameName := '_' + prmInternameName;
              prmNameColisionWithInputParam := ( GetParameterIndex(tmpMthdType,prmInternameName) >= 0 );
              prmTypeEntity := GetDataType(prmTypeName,prmTypeType,ExtractTypeHint(tmpNode));
              prmHasInternameName := not AnsiSameText(prmInternameName,prmName);
              prmDef := FindParameter(tmpMthdType,prmInternameName);
              if ( prmDef = nil ) then begin
                prmDef := TPasArgument(SymbolTable.CreateElement(TPasArgument,prmInternameName,tmpMthdType,visDefault,'',0));
                tmpMthdType.Args.Add(prmDef);
                prmDef.ArgType := prmTypeEntity;
                prmDef.ArgType.AddRef();
                prmDef.Access := argOut;
                if prmHasInternameName then begin
                  SymbolTable.RegisterExternalAlias(prmDef,prmName);
                end;
              end else begin
                if prmNameColisionWithInputParam and ( prmDef.ArgType = prmTypeEntity ) then begin
                  prmDef.Access := argVar;
                end else begin
                  prmInternameName := '_' + prmInternameName;
                  prmDef := TPasArgument(SymbolTable.CreateElement(TPasArgument,prmInternameName,tmpMthdType,visDefault,'',0));
                  prmDef.ArgType := prmTypeEntity;
                  prmDef.ArgType.AddRef();
                  prmDef.Access := argOut;
                  tmpMthdType.Args.Add(prmDef);
                  SymbolTable.RegisterExternalAlias(prmDef,prmName);
                end;
              end;
            end;
            if ( SameText(ASoapBindingStyle,s_rpc) and
                 ( prmDef <> nil ) and ( prmDef.Access = argOut ) and
                 ( prmDef = TPasArgument(tmpMthdType.Args[Pred(tmpMthdType.Args.Count)]) )
               ) or
               ( SameText(ASoapBindingStyle,s_document) and
                 ( prmDef <> nil ) and
                 ( prmDef.Access = argOut ) and
                 ( prmDef = TPasArgument(tmpMthdType.Args[Pred(tmpMthdType.Args.Count)]) )
               )
            then begin
              locProcType := tmpMthd.ProcType;
              locFunc := TPasFunction(SymbolTable.CreateElement(TPasFunction,tmpMthd.Name,AOwner,visDefault,'',0));
              SymbolTable.RegisterExternalAlias(locFunc,SymbolTable.GetExternalName(tmpMthd));
            {$IFDEF WST_TPASSOURCEPOS}
              locFuncType := SymbolTable.CreateFunctionType('','Result',locFunc,False,Default(TPasSourcePos));
            {$ELSE WST_TPASSOURCEPOS}
              locFuncType := SymbolTable.CreateFunctionType('','Result',locFunc,False,'',0);
            {$ENDIF WST_TPASSOURCEPOS}
              locFunc.ProcType := locFuncType;
              resArgIndex := FindIndexOfResultArg(locProcType.Args);
              for j := 0 to ( locProcType.Args.Count - 1 ) do begin
                if ( j <> resArgIndex ) then begin
                  arg_a := TPasArgument(locProcType.Args[j]);
                  arg_b := TPasArgument(SymbolTable.CreateElement(TPasArgument,arg_a.Name,locFuncType,visDefault,'',0));
                  locFuncType.Args.Add(arg_b);
                  arg_b.Access := arg_a.Access;
                  arg_b.ArgType := arg_a.ArgType;
                  arg_b.ArgType.AddRef();
                  SymbolTable.RegisterExternalAlias(arg_b,SymbolTable.GetExternalName(arg_a));
                end;
              end;
              j := resArgIndex;
              arg_a := TPasArgument(locProcType.Args[j]);
              locFuncType.ResultEl.ResultType := arg_a.ArgType;
              SymbolTable.RegisterExternalAlias(locFuncType.ResultEl,SymbolTable.GetExternalName(arg_a));
              locFuncType.ResultEl.ResultType.AddRef();
              tmpMthd.Release();
              tmpMthd := locFunc;
            end;
          end;
        end;
      end;
    end;
    
  begin
    AMthd := nil;
    tmpMthd :=
      TPasProcedure(
        SymbolTable.CreateElement(TPasProcedure,ExtractIdentifier(AMthdName),
        AOwner,visDefault,'',0)
      );
    try
      SymbolTable.RegisterExternalAlias(tmpMthd,AMthdName);
      ParseInputMessage();
      ParseOutputMessage();
    except
      FreeAndNil(tmpMthd);
      AMthd := nil;
      raise;
    end;
    AMthd := tmpMthd;
  end;

var
  locMthd : TPasProcedure;
  mthdName : string;
begin
  locMthd := nil;
  if not ExtractOperationName(mthdName) then
    raise EXsdParserAssertException.CreateFmt('Operation Attribute not found : "%s"',[s_name]);
  DoOnMessage(mtInfo,Format('Parsing operation "%s"',[mthdName]));
  if SameText(s_document,ASoapBindingStyle) then begin
    ExtractMethod(mthdName,locMthd);
    if ( locMthd <> nil ) then begin
      AOwner.Members.Add(locMthd);
    end;
  end else if SameText(s_rpc,ASoapBindingStyle) then begin
    ExtractMethod(mthdName,locMthd);
    if ( locMthd <> nil ) then begin
      AOwner.Members.Add(locMthd);
    end;
  end;
  Result := locMthd;
end;

function TWsdlParser.FindParser(const ANamespace: string): IXsdPaser;
var
  i : Integer;
  p, p1 : IXsdPaser;
begin
  Result := nil;
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

procedure TWsdlParser.ParsePort(ANode: TDOMNode);

  function FindBindingNode(const AName : WideString):TDOMNode;
  var
    crs : IObjectCursor;
    s : string;
  begin
    Result := FindNamedNode(FBindingCursor,AName);
    if Assigned(Result) then begin
      crs := CreateChildrenCursor(Result,cetRttiNode);
      if Assigned(crs) then begin
        s := Format('%s = %s and %s = %s',[s_LOCAL_NAME,QuotedStr(s_binding), s_NS_URI, QuotedStr(s_soap)]);
        crs := CreateCursorOn(crs,ParseFilter(s,TDOMNodeRttiExposer));
        crs.Reset();
        if not crs.MoveNext() then begin
          Result := nil;
        end;
      end else begin
        Result := nil;
      end;
    end;
  end;
  
  function ExtractBindingQName(out AName : WideString):Boolean ;
  var
    attCrs, crs : IObjectCursor;
  begin
    Result := False;
    attCrs := CreateAttributesCursor(ANode,cetRttiNode);
    if Assigned(attCrs) then begin
      crs := CreateCursorOn(attCrs,ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_binding)]),TDOMNodeRttiExposer));
      crs.Reset();
      if crs.MoveNext() then begin
        AName := TDOMNodeRttiExposer(crs.GetCurrent()).NodeValue;
        Result := True;
        exit;
      end;
    end;
  end;

  function ExtractTypeQName(ABndgNode : TDOMNode; out AName : WideString):Boolean ;
  var
    attCrs, crs : IObjectCursor;
  begin
    Result := False;
    attCrs := CreateAttributesCursor(ABndgNode,cetRttiNode);
    if Assigned(attCrs) then begin
      crs := CreateCursorOn(attCrs,ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_type)]),TDOMNodeRttiExposer));
      crs.Reset();
      if crs.MoveNext() then begin
        AName := TDOMNodeRttiExposer(crs.GetCurrent()).NodeValue;
        Result := True;
        exit;
      end;
    end;
  end;

  function FindTypeNode(const AName : WideString):TDOMNode;
  begin
    Result := FindNamedNode(FPortTypeCursor,AName);
  end;

  function ExtractAddress() : string;
  var
    tmpCrs : IObjectCursor;
    nd : TDOMNode;
    s : string;
  begin
    Result := '';
    if ANode.HasChildNodes() then begin
      s := Format('%s = %s and %s = %s',[s_LOCAL_NAME,QuotedStr(s_address),s_NS_URI,QuotedStr(s_soap)]);
      tmpCrs := CreateCursorOn(
                  CreateChildrenCursor(ANode,cetRttiNode),
                  ParseFilter(s,TDOMNodeRttiExposer)
                );
      tmpCrs.Reset();
      if tmpCrs.MoveNext() then begin
        nd := (tmpCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
        tmpCrs := CreateCursorOn(
                    CreateAttributesCursor(nd,cetRttiNode),
                    ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_location)]),TDOMNodeRttiExposer)
                  );
        if Assigned(tmpCrs) and tmpCrs.MoveNext() then begin
          Result := (tmpCrs.GetCurrent() as TDOMNodeRttiExposer).NodeValue;
        end;
      end;
    end;
  end;
  
  function ExtractSoapBindingStyle(ABindingNode : TDOMNode;out AName : WideString):Boolean ;
  var
    childrenCrs, crs, attCrs : IObjectCursor;
    s : string;
  begin
    AName := '';
    Result := False;
    childrenCrs := CreateChildrenCursor(ABindingNode,cetRttiNode);
    if Assigned(childrenCrs) then begin
      s := Format('%s = %s and %s = %s',[s_LOCAL_NAME,QuotedStr(s_binding), s_NS_URI, QuotedStr(s_soap)]);
      crs := CreateCursorOn(childrenCrs,ParseFilter(s,TDOMNodeRttiExposer));
      crs.Reset();
      if crs.MoveNext() then begin
        attCrs := CreateAttributesCursor(TDOMNodeRttiExposer(crs.GetCurrent()).InnerObject,cetRttiNode);
        if Assigned(attCrs) then begin
          s := s_NODE_NAME + ' = ' + QuotedStr(s_style);
          crs := CreateCursorOn(attCrs,ParseFilter(s,TDOMNodeRttiExposer));
          crs.Reset();
          if crs.MoveNext() then begin
            AName := TDOMNodeRttiExposer(crs.GetCurrent()).NodeValue;
            Result := True;
            exit;
          end;
        end;
      end;
    end;
  end;

var
  bindingName, typeName : WideString;
  i : Integer;
  bindingNode, typeNode : TDOMNode;
  intfDef : TPasClassType;
  bdng : TwstBinding;
  locSoapBindingStyle : string;
  locWStrBuffer : WideString;
begin
  if ExtractBindingQName(bindingName) then begin
    i := Pos(':',bindingName);
    bindingName := Copy(bindingName,( i + 1 ), MaxInt);
    if ( SymbolTable.FindBinding(bindingName) = nil ) then begin
      bindingNode := FindBindingNode(bindingName);
      if Assigned(bindingNode) then begin
        if ExtractTypeQName(bindingNode,typeName) then begin
          i := Pos(':',typeName);
          typeName := Copy(typeName,( i + 1 ), MaxInt);
          typeNode := FindTypeNode(typeName);
          if Assigned(typeNode) then begin
            ExtractSoapBindingStyle(bindingNode,locWStrBuffer);
            locSoapBindingStyle := locWStrBuffer;
            if IsStrEmpty(locSoapBindingStyle) then
              locSoapBindingStyle := s_document;
            intfDef := ParsePortType(typeNode,bindingNode,locSoapBindingStyle);
            bdng := SymbolTable.AddBinding(bindingName,intfDef);
            bdng.Address := ExtractAddress();
            bdng.BindingStyle := StrToBindingStyle(locSoapBindingStyle);
          end;
        end;
      end;    
    end;
  end;
end;

function TWsdlParser.ParsePortType(
        ANode, ABindingNode : TDOMNode;
  const ABindingStyle : string
) : TPasClassType;
var
  s : string;
  ws : widestring;
    
  function ExtractBindingOperationCursor() : IObjectCursor ;
  begin
    Result := nil;
    if ABindingNode.HasChildNodes() then begin
      Result := CreateCursorOn(
                  CreateChildrenCursor(ABindingNode,cetRttiNode),
                  ParseFilter(CreateQualifiedNameFilterStr(s_operation,FWsdlShortNames),TDOMNodeRttiExposer)
                );
    end;
  end;
  
  procedure ParseOperation_EncodingStyle(ABndngOpCurs : IObjectCursor; AOp : TPasProcedure);
  var
    nd, ndSoap : TDOMNode;
    tmpCrs, tmpSoapCrs, tmpXcrs : IObjectCursor;
    in_out_count : Integer;
    strBuffer : string;
  begin
    nd := FindNamedNode(ABndngOpCurs,SymbolTable.GetExternalName(AOp));
    if Assigned(nd) and nd.HasChildNodes() then begin
      tmpCrs := CreateCursorOn(
                  CreateChildrenCursor(nd,cetRttiNode),
                  ParseFilter(
                   CreateQualifiedNameFilterStr(s_input,FWsdlShortNames) + ' or ' +
                     CreateQualifiedNameFilterStr(s_output,FWsdlShortNames)
                   ,
                   TDOMNodeRttiExposer
                  )
                );
      tmpCrs.Reset();
      in_out_count := 0;
      while tmpCrs.MoveNext() and ( in_out_count < 2 ) do begin
        Inc(in_out_count);
        nd := (tmpCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
        if nd.HasChildNodes() then begin
          strBuffer := Format('%s = %s and %s = %s',[s_LOCAL_NAME,QuotedStr(s_body),s_NS_URI,QuotedStr(s_soap)]);
          tmpSoapCrs := CreateCursorOn(
                          CreateChildrenCursor(nd,cetRttiNode),
                          ParseFilter(strBuffer,TDOMNodeRttiExposer)
                        );
          tmpSoapCrs.Reset();
          if tmpSoapCrs.MoveNext() then begin
            ndSoap := (tmpSoapCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
            if Assigned(ndSoap.Attributes) and ( ndSoap.Attributes.Length > 0 ) then begin
              tmpXcrs := CreateCursorOn(
                          CreateAttributesCursor(ndSoap,cetRttiNode),
                          ParseFilter(
                            Format('%s = %s',[s_NODE_NAME,QuotedStr(s_use)]),
                            TDOMNodeRttiExposer
                          )
                        );
              tmpXcrs.Reset();
              if tmpXcrs.MoveNext() then begin
                if AnsiSameText(s_input,ExtractNameFromQName(nd.NodeName)) then begin
                  strBuffer := s_soapInputEncoding;
                end else begin
                  strBuffer := s_soapOutputEncoding;
                end;
                SymbolTable.Properties.SetValue(AOp,s_FORMAT + '_' + strBuffer,(tmpXcrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject.NodeValue);
              end;
            end;
          end;
        end;
      end;
    end;
  end;

  procedure ParseOperationAttributes(ABndngOpCurs : IObjectCursor; AOp : TPasProcedure);
  var
    nd : TDOMNode;
    tmpCrs : IObjectCursor;
    locStrBuffer : string;
  begin
    ws := '';
    s := SymbolTable.GetExternalName(AOp);
    ws := s;
    nd := FindNamedNode(ABndngOpCurs,ws);
    if Assigned(nd) and nd.HasChildNodes() then begin
      locStrBuffer := Format('%s = %s and %s = %s',[s_LOCAL_NAME,QuotedStr(s_operation),s_NS_URI,QuotedStr(s_soap)]);
      tmpCrs := CreateCursorOn(
                  CreateChildrenCursor(nd,cetRttiNode),
                  ParseFilter(locStrBuffer,TDOMNodeRttiExposer)
                );
      tmpCrs.Reset();
      if tmpCrs.MoveNext() then begin
        nd := (tmpCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
        if Assigned(nd.Attributes) and ( nd.Attributes.Length > 0 ) then begin
          tmpCrs := CreateCursorOn(
                      CreateAttributesCursor(nd,cetRttiNode),
                      ParseFilter(
                        Format( '%s = %s or %s = %s',
                                [ s_NODE_NAME,QuotedStr(s_soapAction),
                                  s_NODE_NAME,QuotedStr(s_style)
                                ]
                        ),
                        TDOMNodeRttiExposer
                      )
                    );
          tmpCrs.Reset();
          while tmpCrs.MoveNext() do begin
            nd := (tmpCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
            if AnsiSameText(nd.NodeName,s_style) then begin
              SymbolTable.Properties.SetValue(AOp,s_soapStyle,nd.NodeValue);
            end else if AnsiSameText(nd.NodeName,s_soapAction) then begin
              SymbolTable.Properties.SetValue(AOp,s_TRANSPORT + '_' + s_soapAction,nd.NodeValue);
            end;
          end;
        end;
      end;
      ParseOperation_EncodingStyle(ABndngOpCurs,AOp);
    end;
  end;

  function ParseIntfGuid() : string;
  var
    nd : TDOMNode;
    tmpCrs : IObjectCursor;
  begin
    Result := '';
    tmpCrs := CreateCursorOn(
                CreateChildrenCursor(ANode,cetRttiNode),
                ParseFilter(CreateQualifiedNameFilterStr(s_documentation,FWsdlShortNames),TDOMNodeRttiExposer)
              );
    tmpCrs.Reset();
    if tmpCrs.MoveNext() then begin
      nd := (tmpCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
      if nd.HasChildNodes() then begin
        tmpCrs := CreateCursorOn(
                    CreateChildrenCursor(nd,cetRttiNode),
                    ParseFilter(Format('%s=%s',[s_NODE_NAME,QuotedStr(s_guid)]),TDOMNodeRttiExposer)
                  );
        tmpCrs.Reset();
        if tmpCrs.MoveNext() then begin
          nd := (tmpCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
          if ( nd.Attributes <> nil ) then begin
            nd := nd.Attributes.GetNamedItem(s_value);
            if Assigned(nd) then
              Result := Trim(nd.NodeValue);
          end;
        end;
      end;
    end;
  end;

var
  locIntf : TPasClassType;
  locAttCursor : IObjectCursor;
  locCursor, locOpCursor, locBindingOperationCursor : IObjectCursor;
  locObj : TDOMNodeRttiExposer;
  locMthd : TPasProcedure;
  inft_guid : TGuid;
  ansiStrBuffer : ansistring;
  elt : TPasElement;
begin
  locAttCursor := CreateAttributesCursor(ANode,cetRttiNode);
  locCursor := CreateCursorOn(locAttCursor,ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_name)]),TDOMNodeRttiExposer));
  locCursor.Reset();
  if not locCursor.MoveNext() then
    raise EXsdParserAssertException.CreateFmt('PortType Attribute not found : "%s"',[s_name]);
  locObj := locCursor.GetCurrent() as TDOMNodeRttiExposer;
  ansiStrBuffer := locObj.NodeValue;
  elt := SymbolTable.FindElementInModule(ansiStrBuffer,SymbolTable.CurrentModule);
  if ( elt = nil ) then begin
    DoOnMessage(mtInfo,Format('Parsing the port type "%s"',[ansiStrBuffer]));
    locIntf := TPasClassType(SymbolTable.CreateElement(TPasClassType,ansiStrBuffer,SymbolTable.CurrentModule.InterfaceSection,visDefault,'',0));
    FModule.InterfaceSection.Declarations.Add(locIntf);
    FModule.InterfaceSection.Types.Add(locIntf);
    FModule.InterfaceSection.Classes.Add(locIntf);
    locIntf.ObjKind := okInterface;
    Result := locIntf;
{$IFDEF HAS_EXP_TREE}
    locIntf.GUIDExpr:=TPrimitiveExpr.Create(locIntf,pekString,ParseIntfGuid());
{$ELSE HAS_EXP_TREE}
    locIntf.InterfaceGUID := ParseIntfGuid();
{$ENDIF HAS_EXP_TREE}
    if IsStrEmpty(locIntf.InterfaceGUID) and ( CreateGUID(inft_guid) = 0 ) then begin
{$IFDEF HAS_EXP_TREE}
      FreeAndNil(locIntf.GUIDExpr);
      locIntf.GUIDExpr:=TPrimitiveExpr.Create(locIntf,pekString,GUIDToString(inft_guid) );
{$ELSE HAS_EXP_TREE}
      locIntf.InterfaceGUID := GUIDToString(inft_guid);
{$ENDIF HAS_EXP_TREE}
    end;
    locCursor := CreateChildrenCursor(ANode,cetRttiNode);
    if Assigned(locCursor) then begin
      locOpCursor := CreateCursorOn(locCursor,ParseFilter(CreateQualifiedNameFilterStr(s_operation,FWsdlShortNames),TDOMNodeRttiExposer));
      locOpCursor.Reset();
      locBindingOperationCursor := ExtractBindingOperationCursor();
      while locOpCursor.MoveNext() do begin
        locObj := locOpCursor.GetCurrent() as TDOMNodeRttiExposer;
        locMthd := ParseOperation(locIntf,locObj.InnerObject,ABindingStyle);
        if Assigned(locMthd) then begin
          ParseOperationAttributes(locBindingOperationCursor,locMthd);
        end;
      end;
    end;
  end else begin
    if elt.InheritsFrom(TPasClassType) and ( TPasClassType(elt).ObjKind = okInterface ) then begin
      Result := TPasClassType(elt);
    end else begin
      raise EXsdInvalidDefinitionException.CreateFmt('Invalid element definition : "%s".',[elt.Name]);
    end;
  end;
end;

procedure TWsdlParser.ParseService(ANode: TDOMNode);
var
  locCursor, locPortCursor : IObjectCursor;
  locObj : TDOMNodeRttiExposer;
begin
  locCursor := CreateChildrenCursor(ANode,cetRttiNode);
  if Assigned(locCursor) then begin
    locPortCursor := CreateCursorOn(
                       locCursor,
                       ParseFilter(CreateQualifiedNameFilterStr(s_port,FWsdlShortNames),TDOMNodeRttiExposer)
                     );
    locPortCursor.Reset();
    while locPortCursor.MoveNext() do begin
      locObj := locPortCursor.GetCurrent() as TDOMNodeRttiExposer;
      ParsePort(locObj.InnerObject);
    end;
  end;
end;

function TWsdlParser.ParseType(
  const AName : string; 
  const AHint : string;
  const ATypeOrElement : string
) : TPasType;
var
  localName, spaceShort, spaceLong : string;
  locPrs : IXsdPaser;
  xsdModule : TPasModule;
  locTypeKind : string;
begin
  ExplodeQName(AName,localName,spaceShort);
  if ( FXSShortNames.IndexOf(spaceShort) >= 0 ) then begin
    xsdModule := SymbolTable.FindModule(s_xs);
    Result := nil;
    if not IsStrEmpty(AHint) then
      Result := SymbolTable.FindElementInModule(AHint,xsdModule,[elkName]) as TPasType;
    if ( Result = nil ) then
      Result := SymbolTable.FindElementInModule(localName,xsdModule) as TPasType;
    if ( Result = nil ) then
      raise EXsdTypeNotFoundException.CreateFmt('Type not found : "%s".',[AName]);
  end else begin
    if not FindNameSpace(spaceShort,spaceLong) then
      raise EXsdParserAssertException.CreateFmt('Unable to resolve the namespace : "%s".',[spaceShort]);
    locPrs := GetParser(spaceLong);
    if ( ATypeOrElement = s_element ) then
      locTypeKind := s_element
    else
      locTypeKind := '';
    Result := locPrs.ParseType(AName,locTypeKind);
  end;
end;

procedure TWsdlParser.ParseTypes();
var
  locPrs : IXsdPaser;
  i : Integer;
begin
  for i := 0 to Pred(FXsdParsers.Count) do begin
    locPrs := (FXsdParsers.Objects[i] as TIntfObjectRef).Intf as IXsdPaser;
    locPrs.ParseTypes();
  end;
end;

procedure TWsdlParser.Prepare(const AModuleName: string);

  function ExtractTargetNameSpace(ANode : TDOMNode) : string;
  var
    locDomObj : TDOMNode;
  begin
    locDomObj := ANode;
    if ( locDomObj.Attributes = nil ) then
      raise EXsdParserAssertException.Create('Invalid document.');
    locDomObj := locDomObj.Attributes.GetNamedItem(s_targetNamespace);
    if Assigned(locDomObj) then
      Result := locDomObj.NodeValue;
  end;

  procedure CreateXsdParsers();
  var
    locDomObj : TDOMNode;
    locPrs : IXsdPaser;
    locPrsCtx : IParserContext;
    ns : string;
    locDocLocator : IDocumentLocator;
  begin
    if Assigned(FSchemaCursor) then begin
      locDocLocator := GetDocumentLocator();
      FSchemaCursor.Reset();
      while FSchemaCursor.MoveNext() do begin
        locDomObj := (FSchemaCursor.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
        locPrs := TWsdlSchemaParser.Create(FDoc,locDomObj,FSymbols,Self);
        locPrs.SetNotifier(FOnMessage);
        locPrsCtx := locPrs as IParserContext;
        locPrsCtx.SetDocumentLocator(locDocLocator);
        locPrsCtx.SetSimpleOptions(Self.GetSimpleOptions());
        ns := (locPrs as IParserContext).GetTargetNameSpace();
        FXsdParsers.AddObject(ns,TIntfObjectRef.Create(locPrs));
      end;
    end;
  end;

var
  locAttCursor : IObjectCursor;
  locObj : TDOMNodeRttiExposer;
begin
  locAttCursor := CreateAttributesCursor(FDoc.DocumentElement,cetRttiNode);
  FChildCursor := CreateChildrenCursor(FDoc.DocumentElement,cetRttiNode);

  FTargetNameSpace := ExtractTargetNameSpace(FDoc.DocumentElement);
  CreateWstInterfaceSymbolTable(SymbolTable);

  FModule := TPasModule(SymbolTable.CreateElement(TPasModule,AModuleName,SymbolTable.Package,visDefault,'',0));
  SymbolTable.Package.Modules.Add(FModule);
  SymbolTable.RegisterExternalAlias(FModule,FTargetNameSpace);
  FModule.InterfaceSection := TInterfaceSection(SymbolTable.CreateElement(TInterfaceSection,'',FModule,visDefault,'',0));

  FPortTypeCursor := nil;

  FWsdlShortNames := AddNameSpace(s_wsdl);
    ExtractNameSpaceShortNames(locAttCursor,FWsdlShortNames,s_wsdl,nfaRaiseException,True,EXsdParserException);
  FSoapShortNames := AddNameSpace(s_soap);
    ExtractNameSpaceShortNames(locAttCursor,FSoapShortNames,s_soap,nfaRaiseException,False,EXsdParserException);
  FXSShortNames := AddNameSpace(s_xs);
    ExtractNameSpaceShortNames(locAttCursor,FXSShortNames,s_xs,nfaNone,True,EXsdParserException);

  BuildNameSpaceList(locAttCursor,FNameSpaceList);
  FServiceCursor := CreateCursorOn(
                      FChildCursor.Clone() as IObjectCursor,
                      ParseFilter(CreateQualifiedNameFilterStr(s_service,FWsdlShortNames),TDOMNodeRttiExposer)
                    );
  FServiceCursor.Reset();

  FBindingCursor := CreateCursorOn(
                      FChildCursor.Clone() as IObjectCursor,
                      ParseFilter(CreateQualifiedNameFilterStr(s_binding,FWsdlShortNames),TDOMNodeRttiExposer)
                    );
  FBindingCursor.Reset();

  FPortTypeCursor := CreateCursorOn(
                       FChildCursor.Clone() as IObjectCursor,
                       ParseFilter(CreateQualifiedNameFilterStr(s_portType,FWsdlShortNames),TDOMNodeRttiExposer)
                     );
  FPortTypeCursor.Reset();

  FSchemaCursor := nil;
  FTypesCursor := CreateCursorOn(
                    FChildCursor.Clone() as IObjectCursor,
                    ParseFilter(CreateQualifiedNameFilterStr(s_types,FWsdlShortNames),TDOMNodeRttiExposer)
                  );
  FTypesCursor.Reset();
  if FTypesCursor.MoveNext() then begin
    locObj := FTypesCursor.GetCurrent() as TDOMNodeRttiExposer;
    if locObj.InnerObject.HasChildNodes() then begin
      FSchemaCursor := CreateChildrenCursor(locObj.InnerObject,cetRttiNode);
      FSchemaCursor.Reset();
      FSchemaCursor := CreateCursorOn(
                         FSchemaCursor,
                         TAggregatedFilter.Create(
                           ParseFilter(CreateQualifiedNameFilterStr(s_schema,FXSShortNames),TDOMNodeRttiExposer),
                           TQualifiedNameObjectFilter.Create(s_schema,s_xs),
                           fcOr
                         )
                       );
     { FSchemaCursor := CreateCursorOn(
                         FSchemaCursor,
                         ParseFilter(CreateQualifiedNameFilterStr(s_schema,FXSShortNames),TDOMNodeRttiExposer)
                       );}
      FSchemaCursor.Reset();
      if FSchemaCursor.MoveNext() then begin
        FSchemaCursor.Reset();
      end else begin
        FSchemaCursor := CreateCursorOn(
                           CreateChildrenCursor(locObj.InnerObject,cetRttiNode),
                           ParseFilter(Format('%s=%s',[s_NODE_NAME,QuotedStr(s_schema)]),TDOMNodeRttiExposer)
                         );
        FSchemaCursor.Reset();
      end;
    end;
  end;

  FMessageCursor := CreateCursorOn(
                      FChildCursor.Clone() as IObjectCursor,
                      ParseFilter(CreateQualifiedNameFilterStr(s_message,FWsdlShortNames),TDOMNodeRttiExposer)
                    );
  FMessageCursor.Reset();
  CreateXsdParsers();
end;

function TWsdlParser.GetParser(const ANamespace: string): IXsdPaser;
begin
  Result := FindParser(ANamespace);
  if (Result = nil) then
    raise EXsdParserAssertException.CreateFmt('Unable to find the parser, namespace : "%s".',[ANamespace]);
end;


end.
