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
unit ws_parser_imp;

interface
uses
  Classes, SysUtils, Contnrs,
  {$IFNDEF FPC}xmldom, wst_delphi_xml{$ELSE}DOM, wst_fpc_xml{$ENDIF},
  cursor_intf, rtti_filters,
  pastree, pascal_parser_intf, logger_intf,
  xsd_parser, wst_types;

type

  TNameSpaceValueType = ( nvtExpandValue, nvtShortSynonym );
  TSearchSpace = ( ssCurrentModule, ssGlobal );

  TAbstractTypeParserClass = class of TAbstractTypeParser;

  { TAbstractTypeParser }

  TAbstractTypeParser = class
  private
    FContext : IParserContext;
    FTypeNode : TDOMNode;
    FSymbols : TwstPasTreeContainer;
    FTypeName : string;
    FEmbededDef : Boolean;
  private
    function GetModule: TPasModule;{$IFDEF USE_INLINE}inline;{$ENDIF}
  protected
    function FindElementNS(
      const ANameSpace,
            ALocalName : string;
      const ASpaceType : TNameSpaceValueType
    ) : TPasElement;
    function FindElement(
      const ALocalName : string;
      const ANameKinds : TElementNameKinds = [elkDeclaredName,elkName]
    ) : TPasElement;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function FindElementWithHint(const AName, AHint : string; const ASpace : TSearchSpace) : TPasElement;
    function ExtractTypeHint(AElement : TDOMNode) : string;{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure SetAsEmbeddedType(AType : TPasType; const AValue : Boolean);
    function IsEmbeddedType(AType : TPasType) : Boolean;
{$IFDEF WST_HANDLE_DOC}
    procedure ParseDocumentation(AType : TPasType);
{$ENDIF WST_HANDLE_DOC}
  public
    constructor Create(
            AOwner       : IParserContext;
            ATypeNode    : TDOMNode;
      const ATypeName    : string;
      const AEmbededDef  : Boolean
    );
    class function ExtractEmbeddedTypeFromElement(
            AOwner       : IParserContext;
            AEltNode     : TDOMNode;
            ASymbols     : TwstPasTreeContainer;
      const ATypeName    : string
    ) : TPasType;
    class function GetParserSupportedStyle():string;virtual;abstract;
    class procedure RegisterParser(AParserClass : TAbstractTypeParserClass);
    class function GetRegisteredParserCount() : Integer;
    class function GetRegisteredParser(const AIndex : Integer):TAbstractTypeParserClass;
    function Parse():TPasType;virtual;abstract;
    property Module : TPasModule read GetModule;
    property Context : IParserContext read FContext;
  end;

  TDerivationMode = ( dmNone, dmExtension, dmRestriction );
  TSequenceType = ( stElement, stAll );
  TParserTypeHint = ( pthDeriveFromSoapArray );
  TParserTypeHints = set of TParserTypeHint;

  { TPropInfoReference }

  TPropInfoReference = class
  private
    FIsCollection : Boolean;
    FProp : TPasProperty;
  public
    property Prop : TPasProperty read FProp;
    property IsCollection : Boolean read FIsCollection;
  end;
  
  { TPropInfoReferenceList }

  TPropInfoReferenceList = class
  private
    FList : TObjectList;
  public
    constructor Create();
    destructor Destroy();override;
    function Add(AProp : TPasProperty) : TPropInfoReference;
    function GetItem(const AIndex : Integer) : TPropInfoReference;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function IndexOf(const AProp : TPasProperty) : Integer;
    function GetCount() : Integer;{$IFDEF USE_INLINE}inline;{$ENDIF}
  end;

  TComplexTypeKind = (ctkComplexType, ctkGroup, ctkAttributeGroup);

  { TComplexTypeParser }

  TComplexTypeParser = class(TAbstractTypeParser)
  private
    FAttCursor : IObjectCursor;
    FChildCursor : IObjectCursor;
    FContentNode : TDOMNode;
    FContentType : string;
    FBaseType : TPasType;
    FDerivationMode : TDerivationMode;
    FDerivationNode : TDOMNode;
    FSequenceType : TSequenceType;
    FHints : TParserTypeHints;
    FKind : TComplexTypeKind;
    FMixed : Boolean;
  private
    //helper routines
    function ExtractElementCursor(
          AParentNode : TDOMNode;
      out AAttCursor, AGroupCursor, AAttGroupCursor : IObjectCursor;
      out AAnyNode, AAnyAttNode : TDOMNode
    ):IObjectCursor;
    procedure ExtractExtendedMetadata(const AItem : TPasElement; const ANode : TDOMNode);
    procedure GenerateArrayTypes(
      const AClassName : string;
            AArrayPropList : TPropInfoReferenceList
    );
    function ExtractSoapArray(
      const ATypeName : string;
      const AInternalName : string;
      const AHasInternalName : Boolean
    ) : TPasArrayType;
    function IsHeaderBlock() : Boolean;
    function IsSimpleContentHeaderBlock() : Boolean;
    procedure SetAsGroupType(AType : TPasType; const AValue : Boolean);
    procedure AddGroup(
            ADest,
            AGroup           : TPasClassType;
      const AMultiOccurrence : Boolean;
            AArrayItems      : TPropInfoReferenceList
    );
    procedure ParseGroups(
      AClassDef    : TPasClassType;
      AGroupCursor : IObjectCursor;
      AArrayItems  : TPropInfoReferenceList
    );
  private
    procedure CreateNodeCursors();
    procedure ExtractTypeName();
    procedure ExtractMixedStatus();
    procedure ExtractContentType();
    procedure ExtractBaseType();
    function ParseSimpleContent(const ATypeName : string):TPasType;
    function ParseEmptyContent(const ATypeName : string):TPasType;
    function ParseComplexContent(const ATypeName : string):TPasType;virtual;
  public
    class function GetParserSupportedStyle():string;override;
    function Parse():TPasType;override;
  end;

  { TSimpleTypeParser }

  TSimpleTypeParser = class(TAbstractTypeParser)
  private
    FAttCursor : IObjectCursor;
    FChildCursor : IObjectCursor;
    FBaseName : string;
    FBaseNameSpace : string;
    FRestrictionNode : TDOMNode;
    FIsEnum : Boolean;
  private
    procedure CreateNodeCursors();
    procedure ExtractTypeName();
    function ExtractContentType() : Boolean;
    function ParseEnumContent():TPasType;
    function ParseOtherContent():TPasType;
  public
    class function GetParserSupportedStyle():string;override;
    function Parse():TPasType;override;
  end;


implementation
uses 
  dom_cursors, parserutils, StrUtils, xsd_consts, wst_consts;

type
  TOccurrenceRec = record
    Valid      : Boolean;
    MinOccurs  : Integer;
    MaxOccurs  : Integer;
    Unboundded : Boolean;
  end;

procedure ExtractOccurences(
      AEntityName,
      AItemName      : string;
      AAttCursor     : IObjectCursor;
  var AMinOccurs,
      AMaxOccurs     : Integer;
  var AMaxUnboundded : Boolean
);overload;
var
  locPartCursor : IObjectCursor;
  locMin, locMax : Integer;
  locMaxOccurUnbounded : Boolean;
  locStrBuffer : string;
begin
  if (AAttCursor = nil) then begin
    AMinOccurs := 1;
    AMaxOccurs := 1;
    AMaxUnboundded := False;
    exit;
  end;

  locMin := 1;
  locPartCursor :=
    CreateCursorOn(
      AAttCursor.Clone() as IObjectCursor,
      ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_minOccurs)]),
      TDOMNodeRttiExposer)
    );
  locPartCursor.Reset();
  if locPartCursor.MoveNext() then begin
    locStrBuffer := (locPartCursor.GetCurrent() as TDOMNodeRttiExposer).NodeValue;
    if not TryStrToInt(locStrBuffer,locMin) then
      raise EXsdParserException.CreateFmt(SERR_InvalidMinOccursValue,[AEntityName,AItemName]);
    if ( locMin < 0 ) then
      raise EXsdParserException.CreateFmt(SERR_InvalidMinOccursValue,[AEntityName,AItemName]);
  end;

  locMax := 1;
  locMaxOccurUnbounded := False;
  locPartCursor :=
    CreateCursorOn(
      AAttCursor.Clone() as IObjectCursor,
      ParseFilter(
        Format('%s = %s',[s_NODE_NAME,QuotedStr(s_maxOccurs)]),
        TDOMNodeRttiExposer
      )
    );
  locPartCursor.Reset();
  if locPartCursor.MoveNext() then begin
    locStrBuffer := (locPartCursor.GetCurrent() as TDOMNodeRttiExposer).NodeValue;
    if AnsiSameText(locStrBuffer,s_unbounded) then begin
      locMaxOccurUnbounded := True;
    end else begin
      if not TryStrToInt(locStrBuffer,locMax) then
        raise EXsdParserException.CreateFmt(SERR_InvalidMaxOccursValue,[AEntityName,AItemName]);
      if ( locMin < 0 ) then
        raise EXsdParserException.CreateFmt(SERR_InvalidMaxOccursValue,[AEntityName,AItemName]);
    end;
  end;

  AMinOccurs := locMin;
  AMaxOccurs := locMax;
  AMaxUnboundded := locMaxOccurUnbounded;
end;

procedure ExtractOccurences(
      AEntityName,
      AItemName      : string;
      AAttCursor     : IObjectCursor;
  var AResult        : TOccurrenceRec
);overload;
begin
  ExtractOccurences(
    AEntityName,AItemName,AAttCursor,
    AResult.MinOccurs,AResult.MaxOccurs,AResult.Unboundded
  );
  AResult.Valid := True;
end;

{ TAbstractTypeParser }

constructor TAbstractTypeParser.Create(
        AOwner       : IParserContext;
        ATypeNode    : TDOMNode;
  const ATypeName    : string;
  const AEmbededDef  : Boolean
);
var
  symtbl : TwstPasTreeContainer;
begin
  Assert(Assigned(AOwner));
  Assert(Assigned(ATypeNode));
  symtbl := AOwner.GetSymbolTable();
  Assert(Assigned(symtbl));
  FContext := AOwner;
  FTypeNode := ATypeNode;
  FSymbols := symtbl;
  FTypeName := ATypeName;
  FEmbededDef := AEmbededDef;
end;

class function TAbstractTypeParser.ExtractEmbeddedTypeFromElement(
        AOwner       : IParserContext;
        AEltNode     : TDOMNode;
        ASymbols     : TwstPasTreeContainer;
  const ATypeName    : string
): TPasType;

  function ExtractTypeName() : string;
  var
    locCrs : IObjectCursor;
  begin
    locCrs := CreateCursorOn(
                CreateAttributesCursor(AEltNode,cetRttiNode),
                ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_name)]),TDOMNodeRttiExposer)
              );
    locCrs.Reset();
    if not locCrs.MoveNext() then
      raise EXsdParserException.Create(SERR_UnableToFindNameTagInNode);
    Result := (locCrs.GetCurrent() as TDOMNodeRttiExposer).NodeValue;
    if IsStrEmpty(Result) then begin
      raise EXsdParserException.Create(SERR_InvalidTypeName);
    end;
  end;
  
  function FindParser(out AFoundTypeNode : TDOMNode):TAbstractTypeParserClass;
  var
    k : Integer;
    locPrsClss : TAbstractTypeParserClass;
    locFilter : string;
    locCrs : IObjectCursor;
  begin
    Result := nil;
    AFoundTypeNode := nil;
    for k := 0 to Pred(GetRegisteredParserCount()) do begin
      locPrsClss := GetRegisteredParser(k);
      locFilter := locPrsClss.GetParserSupportedStyle();
      if not IsStrEmpty(locFilter) then begin
        locFilter := CreateQualifiedNameFilterStr(locFilter,AOwner.GetXsShortNames());
        locCrs := CreateCursorOn(CreateChildrenCursor(AEltNode,cetRttiNode),ParseFilter(locFilter,TDOMNodeRttiExposer));
        locCrs.Reset();
        if locCrs.MoveNext() then begin
          AFoundTypeNode := (locCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
          Result := locPrsClss;
          Break;
        end;
      end;
    end;
  end;
  
var
  typName : string;
  prsClss : TAbstractTypeParserClass;
  prs : TAbstractTypeParser;
  typNode : TDOMNode;
begin
  if not AEltNode.HasChildNodes() then begin;
    raise EXsdParserException.CreateFmt('%s : Type Name = "%s", NodeName = "%s" .',[SERR_InvalidTypeDef_NoChild,ATypeName,AEltNode.NodeName]);
  end;
  typName := ATypeName;
  if IsStrEmpty(typName) then begin
    typName := ExtractTypeName();
  end;
  prsClss := FindParser(typNode);
  if ( prsClss = nil ) then begin;
    raise EXsdInvalidTypeDefinitionException.CreateFmt(SERR_TypeStyleNotSupported,[typName]);
  end;
  prs := prsClss.Create(AOwner,typNode,typName,True);
  try
    Result := prs.Parse();
  finally
    FreeAndNil(prs);
  end;
end;

var
  FTypeParserList : TClassList = nil;
class procedure TAbstractTypeParser.RegisterParser(AParserClass: TAbstractTypeParserClass);
begin
  if ( FTypeParserList = nil ) then begin
    FTypeParserList := TClassList.Create();
  end;
  if ( FTypeParserList.IndexOf(AParserClass) < 0 ) then begin
    FTypeParserList.Add(AParserClass);
  end;
end;

class function TAbstractTypeParser.GetRegisteredParserCount(): Integer;
begin
  if Assigned(FTypeParserList) then begin
    Result := FTypeParserList.Count;
  end else begin
    Result := 0;
  end;
end;

class function TAbstractTypeParser.GetRegisteredParser(const AIndex: Integer): TAbstractTypeParserClass;
begin
  Result := TAbstractTypeParserClass(FTypeParserList[AIndex]);
end;

function TAbstractTypeParser.FindElementNS(
  const ANameSpace,
        ALocalName : string;
  const ASpaceType : TNameSpaceValueType
) : TPasElement;
var
  locNS : string;
begin
  if ( ASpaceType = nvtExpandValue ) then begin
    locNS := ANameSpace
  end else begin
    if not Context.FindNameSpace(ANameSpace,locNS) then
      raise EXsdParserAssertException.CreateFmt(SERR_CannotResolveNamespace,[ANameSpace]);
  end;
  Result := FSymbols.FindElementNS(ALocalName,locNS);
end;

function TAbstractTypeParser.GetModule : TPasModule;
begin
  Result := Context.GetTargetModule();
end;

function TAbstractTypeParser.FindElement(
  const ALocalName: string;
  const ANameKinds : TElementNameKinds
) : TPasElement;
begin
  Result := FSymbols.FindElementInModule(ALocalName,Module,ANameKinds);
end;

function TAbstractTypeParser.FindElementWithHint(
  const AName,
        AHint      : string;
  const ASpace : TSearchSpace
) : TPasElement;
begin
  Result := nil;
  if ( ASpace = ssCurrentModule ) then begin
    if ( Length(AHint) > 0 ) then
      Result := FindElement(AHint,[elkName]);
    if ( Result = nil ) then
      Result := FindElement(AName);
  end else if ( ASpace = ssGlobal ) then begin
    if ( Length(AHint) > 0 ) then
      Result := FSymbols.FindElement(AHint,[elkName]);
    if ( Result = nil ) then
      Result := FSymbols.FindElement(AName);
  end;
end;

function TAbstractTypeParser.ExtractTypeHint(AElement: TDOMNode): string;
begin
  if not wst_findCustomAttributeXsd(Context.GetXsShortNames(),AElement,s_WST_typeHint,Result) then
    Result := '';
end;

procedure TAbstractTypeParser.SetAsEmbeddedType(AType : TPasType; const AValue : Boolean);
var
  s : string;
begin
  if AValue then
    s := '1'
  else
    s := '';
  FSymbols.Properties.SetValue(AType,sEMBEDDED_TYPE,s);
end;

function TAbstractTypeParser.IsEmbeddedType(AType : TPasType) : Boolean; 
begin
  Result := ( FSymbols.Properties.GetValue(AType,sEMBEDDED_TYPE) = '1' );
end;

{$IFDEF WST_HANDLE_DOC}
procedure TAbstractTypeParser.ParseDocumentation(AType : TPasType);
var
  tmpCursor : IObjectCursor;
  props : TStrings;
  docString : string;
  i : Integer;
  tempNode : TDOMNode;
begin
  if FTypeNode.HasChildNodes() then begin
    tmpCursor := CreateCursorOn(
                   CreateChildrenCursor(FTypeNode,cetRttiNode),
                   ParseFilter(CreateQualifiedNameFilterStr(s_annotation,Context.GetXsShortNames()),TDOMNodeRttiExposer)
                 );
    if ( tmpCursor <> nil ) then begin
      tmpCursor.Reset();
      if tmpCursor.MoveNext() then begin
        tmpCursor := CreateCursorOn(
                       CreateChildrenCursor(TDOMNodeRttiExposer(tmpCursor.GetCurrent()).InnerObject,cetRttiNode),
                       ParseFilter(CreateQualifiedNameFilterStr(s_documentation,Context.GetXsShortNames()),TDOMNodeRttiExposer)
                     );
        if ( tmpCursor <> nil ) then begin
          tmpCursor.Reset();
          if tmpCursor.MoveNext() then begin
            tempNode := TDOMNodeRttiExposer(tmpCursor.GetCurrent()).InnerObject.FirstChild;
            if ( tempNode <> nil ) then
              docString := tempNode.NodeValue
            else
              docString := '';
            props := FSymbols.Properties.FindList(AType);
            if IsStrEmpty(docString) then begin
              if ( props <> nil ) then begin
                i := props.IndexOfName(s_documentation);
                if ( i >= 0 ) then
                  props.Values[s_documentation] := '';
              end
            end else begin
              if ( props = nil ) then
                props := FSymbols.Properties.GetList(AType);
              props.Values[s_documentation] := EncodeLineBreak(docString);
            end;
          end;
        end;
      end;
    end;
  end;
end;
{$ENDIF WST_HANDLE_DOC}

{ TComplexTypeParser }

function TComplexTypeParser.ExtractElementCursor(
      AParentNode     : TDOMNode;
  out AAttCursor,
      AGroupCursor,
      AAttGroupCursor : IObjectCursor;
  out AAnyNode,
      AAnyAttNode     : TDOMNode
) : IObjectCursor;
var
  frstCrsr : IObjectCursor;

  function ParseContent_ALL() : IObjectCursor;
  var
    locTmpCrs : IObjectCursor;
    locTmpNode : TDOMNode;
  begin
    Result := nil;
    locTmpCrs := CreateCursorOn(
                   frstCrsr.Clone() as IObjectCursor,
                   ParseFilter(CreateQualifiedNameFilterStr(s_all,Context.GetXsShortNames()),TDOMNodeRttiExposer)
                 );
    locTmpCrs.Reset();
    if locTmpCrs.MoveNext() then begin
      FSequenceType := stElement;
      locTmpNode := (locTmpCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
      if  locTmpNode.HasChildNodes() then begin
        locTmpCrs := CreateCursorOn(
                       CreateChildrenCursor(locTmpNode,cetRttiNode),
                       ParseFilter(CreateQualifiedNameFilterStr(s_element,Context.GetXsShortNames()),TDOMNodeRttiExposer)
                     );
        Result := locTmpCrs;
        AGroupCursor := CreateCursorOn(
                          CreateChildrenCursor(locTmpNode,cetRttiNode),
                          ParseFilter(CreateQualifiedNameFilterStr(s_group,Context.GetXsShortNames()),TDOMNodeRttiExposer)
                        );
      end;
    end;
  end;

  function ParseContent_SEQUENCE(out ARes : IObjectCursor) : Boolean;
  var
    tmpCursor : IObjectCursor;
    tmpNode : TDOMNode;
    tmpFilter : IObjectFilter;
  begin
    ARes := nil;
    tmpFilter := ParseFilter(CreateQualifiedNameFilterStr(s_sequence,Context.GetXsShortNames()),TDOMNodeRttiExposer);
    tmpFilter := TAggregatedFilter.Create(
                   tmpFilter,
                   ParseFilter(CreateQualifiedNameFilterStr(s_choice,Context.GetXsShortNames()),TDOMNodeRttiExposer),
                   fcOr
                 ) as IObjectFilter;
    tmpCursor := CreateCursorOn(
                   frstCrsr.Clone() as IObjectCursor,
                   tmpFilter
                 );
    tmpCursor.Reset();
    Result := tmpCursor.MoveNext();
    if Result then begin
      FSequenceType := stElement;
      tmpNode := (tmpCursor.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
      if  tmpNode.HasChildNodes() then begin
        tmpFilter := ParseFilter(CreateQualifiedNameFilterStr(s_element,Context.GetXsShortNames()),TDOMNodeRttiExposer);
        tmpFilter := TAggregatedFilter.Create(
                       tmpFilter,
                       ParseFilter(CreateQualifiedNameFilterStr(s_choice,Context.GetXsShortNames()),TDOMNodeRttiExposer),
                       fcOr
                     ) as IObjectFilter;
        tmpCursor := CreateCursorOn(
                       CreateChildrenCursor(tmpNode,cetRttiNode),
                       tmpFilter
                     );
        ARes := tmpCursor;
        tmpCursor := CreateCursorOn(
                       CreateChildrenCursor(tmpNode,cetRttiNode),
                       ParseFilter(CreateQualifiedNameFilterStr(s_any,Context.GetXsShortNames()),TDOMNodeRttiExposer)
                     );
        tmpCursor.Reset();
        if tmpCursor.MoveNext() then
          AAnyNode := TDOMNodeRttiExposer(tmpCursor.GetCurrent()).InnerObject;
        tmpCursor := CreateCursorOn(
                       CreateChildrenCursor(tmpNode,cetRttiNode),
                       ParseFilter(CreateQualifiedNameFilterStr(s_group,Context.GetXsShortNames()),TDOMNodeRttiExposer)
                     );
        tmpCursor.Reset();
        AGroupCursor := tmpCursor;
      end;
    end
  end;

var
  parentNode : TDOMNode;
  crs : IObjectCursor;
begin
  Result := nil;
  AAttCursor := nil;
  AGroupCursor := nil;
  AAttGroupCursor := nil;
  AAnyNode := nil;
  AAnyAttNode := nil;
  if FMixed then
    exit;
  parentNode := AParentNode;
  if (parentNode = nil) then begin
    case FDerivationMode of
      dmNone          : parentNode := FContentNode;
      dmRestriction,
      dmExtension     : parentNode := FDerivationNode;
    end;
  end;
  if parentNode.HasChildNodes() then begin;
    AAttCursor :=
      CreateCursorOn(
        CreateChildrenCursor(parentNode,cetRttiNode),
        ParseFilter(CreateQualifiedNameFilterStr(s_attribute,Context.GetXsShortNames()),TDOMNodeRttiExposer)
      );
    AAttGroupCursor :=
      CreateCursorOn(
        CreateChildrenCursor(parentNode,cetRttiNode),
        ParseFilter(CreateQualifiedNameFilterStr(s_attributeGroup,Context.GetXsShortNames()),TDOMNodeRttiExposer)
      );
    crs := CreateChildrenCursor(parentNode,cetRttiNode);
    if ( crs <> nil ) then begin
      crs := CreateCursorOn(
               crs,
               ParseFilter(CreateQualifiedNameFilterStr(s_anyAttribute,Context.GetXsShortNames()),TDOMNodeRttiExposer)
             );
      if ( crs <> nil ) then begin
        crs.Reset();
        if crs.MoveNext() then
          AAnyAttNode := TDOMNodeRttiExposer(crs.GetCurrent()).InnerObject;
      end;
    end;
    frstCrsr := CreateChildrenCursor(parentNode,cetRttiNode);
    if not ParseContent_SEQUENCE(Result) then
      Result := ParseContent_ALL();
  end;
end;

procedure TComplexTypeParser.ExtractExtendedMetadata(
  const AItem : TPasElement;
  const ANode : TDOMNode
);
var
  ls : TDOMNamedNodeMap;
  e : TDOMNode;
  k, q : Integer;
  ns_short, ns_long, localName, locBuffer, locBufferNS, locBufferNS_long, locBufferLocalName : string;
begin
  if ( ANode.Attributes <> nil ) and ( GetNodeListCount(ANode.Attributes) > 0 ) then begin
    ls := ANode.Attributes;
    q := GetNodeListCount(ANode.Attributes);
    for k := 0 to ( q - 1 ) do begin
      e := ls.Item[k];
      if ( Pos(':', e.NodeName) > 1 ) then begin
        ExplodeQName(e.NodeName,localName,ns_short);
        if Context.FindNameSpace(ns_short, ns_long) then begin
          locBuffer := e.NodeValue;
          ExplodeQName(locBuffer,locBufferLocalName,locBufferNS);
          if IsStrEmpty(locBufferNS) then
            locBuffer := locBufferLocalName
          else if Context.FindNameSpace(locBufferNS, locBufferNS_long) then
            locBuffer := Format('%s#%s',[locBufferNS_long,locBufferLocalName]);
          FSymbols.Properties.SetValue(AItem,Format('%s#%s',[ns_long,localName]),locBuffer);
        end;
      end;
    end;
  end;
end;

procedure TComplexTypeParser.GenerateArrayTypes(
  const AClassName : string;
        AArrayPropList : TPropInfoReferenceList
);
var
  propRef : TPropInfoReference;
  locPropTyp : TPasProperty;
  k : Integer;
  locString : string;
  locSym : TPasElement;
begin
  for k := 0 to Pred(AArrayPropList.GetCount()) do begin
    propRef := AArrayPropList.GetItem(k);
    locPropTyp := propRef.Prop;
    locString := Format('%s_%sArray',[AClassName,locPropTyp.Name]);
    locSym := FSymbols.FindElement(locString);
    if ( locSym = nil ) then begin
      locSym := FSymbols.CreateArray(
        locString,
        locPropTyp.VarType,
        locPropTyp.Name,
        FSymbols.GetExternalName(locPropTyp),
        asEmbeded
      );
      Self.Module.InterfaceSection.Declarations.Add(locSym);
      Self.Module.InterfaceSection.Types.Add(locSym);
      if propRef.IsCollection then
        FSymbols.SetCollectionFlag(TPasArrayType(locSym),True);
    end;
  end;
end;

function TComplexTypeParser.ExtractSoapArray(
  const ATypeName : string;
  const AInternalName : string;
  const AHasInternalName : Boolean
) : TPasArrayType;
var
  ls : TStringList;
  crs, locCrs : IObjectCursor;
  s : string;
  i : Integer;
  locSym : TPasElement;
  ok : Boolean;
  nd : TDOMNode;
  locDoRelease : Boolean;
begin
  if not FDerivationNode.HasChildNodes then begin
    raise EXsdInvalidTypeDefinitionException.CreateFmt(SERR_InvalidTypeDef_AttributeNotFound,[FTypeName]);
  end;
  crs := CreateCursorOn(
           CreateChildrenCursor(FDerivationNode,cetRttiNode),
           ParseFilter(CreateQualifiedNameFilterStr(s_attribute,Context.GetXsShortNames()),TDOMNodeRttiExposer)
         );
  ls := TStringList.Create();
  try
    ok := False;
    crs.Reset();
    while crs.MoveNext() do begin
      nd := (crs.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
      if Assigned(nd.Attributes) and ( nd.Attributes.Length > 0 ) then begin
        ls.Clear();
        ExtractNameSpaceShortNamesNested(nd,ls,s_wsdl);
        locCrs := CreateAttributesCursor(nd,cetRttiNode);
        locCrs := CreateCursorOn(
                    locCrs,
                    ParseFilter(CreateQualifiedNameFilterStr(s_arrayType,ls),TDOMNodeRttiExposer)
                  );
        if Assigned(locCrs) then begin
          locCrs.Reset();
          if locCrs.MoveNext() then begin
            ok := True;
            Break;
          end;
        end;
      end;
    end;
  finally
    FreeAndNil(ls);
  end;
  if not ok then begin
    raise EXsdInvalidTypeDefinitionException.CreateFmt(SERR_InvalidTypeDef_NamedAttributeNotFound,[s_arrayType,FTypeName]);
  end;
  s := ExtractNameFromQName((locCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject.NodeValue);
  i := Pos('[',s);
  if ( i < 1 ) then begin
    i := MaxInt;
  end;
  s := Copy(s,1,Pred(i));
  locDoRelease := False;
  locSym := FSymbols.FindElement(s);
  if not Assigned(locSym) then begin
    locSym := TPasUnresolvedTypeRef(FSymbols.CreateElement(TPasUnresolvedTypeRef,s,nil,visDefault,'',0));
    locDoRelease := True;
  end;
  if not locSym.InheritsFrom(TPasType) then
    raise EXsdInvalidTypeDefinitionException.CreateFmt(SERR_InvalidArrayItemType,[FTypeName]);
  Result := FSymbols.CreateArray(AInternalName,locSym as TPasType,s_item,s_item,asScoped);
  if locDoRelease then
    locSym.Release();
  if AHasInternalName then
    FSymbols.RegisterExternalAlias(Result,ATypeName);
end;

function TComplexTypeParser.IsHeaderBlock() : Boolean;
var
  strBuffer : string;
begin
  Result := wst_findCustomAttributeXsd(Context.GetXsShortNames(),FTypeNode,s_WST_headerBlock,strBuffer) and AnsiSameText('true',Trim(strBuffer));
end;

function TComplexTypeParser.IsSimpleContentHeaderBlock() : Boolean;
var
  strBuffer : string;
begin
  Result := wst_findCustomAttributeXsd(Context.GetXsShortNames(),FTypeNode,s_WST_headerBlockSimpleContent,strBuffer) and AnsiSameText('true',Trim(strBuffer));
end;

procedure TComplexTypeParser.SetAsGroupType(
        AType  : TPasType;
  const AValue : Boolean
);
var
  s : string;
begin
  if AValue then
    s := '1'
  else
    s := '';
  FSymbols.Properties.SetValue(AType,sIS_GROUP,s);
end;

function MakeUniqueMemberName(
  ABaseName : string;
  AParent   : TPasClassType;
  ASuffix   : string
) : string;
var
  locInternalEltName : string;
  k : Integer;
begin
  locInternalEltName := ABaseName;
  if (FindMember(AParent,locInternalEltName) <> nil) then begin
    k := 0;
    while True do begin
      locInternalEltName := Format('%s%s',[ABaseName,ASuffix]);
      if (k > 0) then
        locInternalEltName := locInternalEltName+IntToStr(k);
      if (FindMember(AParent,locInternalEltName) = nil) then
        break;
      k := k+1;
    end;
  end;
  Result := locInternalEltName;
end;

procedure TComplexTypeParser.AddGroup(
        ADest,
        AGroup           : TPasClassType;
  const AMultiOccurrence : Boolean;
        AArrayItems      : TPropInfoReferenceList
);
var
  i : Integer;
  src, dest : TPasProperty;
  locIsAttribute, locHasInternalName : Boolean;
  locInternalEltName, locStrBuffer : string;
begin
  for i := 0 to AGroup.Members.Count-1 do begin
    if TObject(AGroup.Members[i]).InheritsFrom(TPasProperty) then begin
      src := TPasProperty(AGroup.Members[i]);
      locIsAttribute := FSymbols.IsAttributeProperty(src);
      if locIsAttribute then
        locStrBuffer := 'Att'
      else
        locStrBuffer := 'Elt';
      locInternalEltName := MakeUniqueMemberName(src.Name,ADest,locStrBuffer);
      locHasInternalName := not SameText(src.Name,locInternalEltName);
      dest := TPasProperty(FSymbols.CreateElement(TPasProperty,locInternalEltName,ADest,visPublished,'',0));
      ADest.Members.Add(dest);
      dest.VarType := src.VarType;
      dest.VarType.AddRef();
      if locHasInternalName or FSymbols.HasExternalName(src) then
        FSymbols.RegisterExternalAlias(dest,FSymbols.GetExternalName(src));
      if not locHasInternalName then begin
        dest.ReadAccessorName := src.ReadAccessorName;
        dest.WriteAccessorName := src.WriteAccessorName;
        dest.StoredAccessorName := src.StoredAccessorName;
      end else begin
        dest.ReadAccessorName := StringReplace(src.ReadAccessorName,src.Name,dest.Name,[rfReplaceAll]);
        dest.WriteAccessorName := StringReplace(src.WriteAccessorName,src.Name,dest.Name,[rfReplaceAll]);
        dest.StoredAccessorName := StringReplace(src.StoredAccessorName,src.Name,dest.Name,[rfReplaceAll]);
      end;
      if locIsAttribute then
        FSymbols.SetPropertyAsAttribute(dest,True);
      if FSymbols.IsChoiceProperty(src) then
        FSymbols.SetPropertyAsChoice(dest,True);
      if not(locIsAttribute) and AMultiOccurrence and
         (AArrayItems <> nil) and not(dest.VarType.InheritsFrom(TPasArrayType))
      then begin
        AArrayItems.Add(dest);
      end;
    {$IFDEF HAS_EXP_TREE}
      if (src.DefaultExpr <> nil) and
         src.DefaultExpr.InheritsFrom(TPrimitiveExpr)
      then begin
        dest.DefaultExpr :=
          TPrimitiveExpr.Create(dest,pekString,TPrimitiveExpr(src.DefaultExpr).Value);
      end;
    {$ENDIF HAS_EXP_TREE}
    end;
  end;
end;

procedure TComplexTypeParser.ParseGroups(
  AClassDef    : TPasClassType;
  AGroupCursor : IObjectCursor;
  AArrayItems  : TPropInfoReferenceList
);
var
  locNode : TDOMNode;
  locAttCursor, locRefCursor : IObjectCursor;
  s, locNS, locLN, locLongNS : string;
  elt : TPasElement;
  locParser : IXsdPaser;
  locOccurrenceInfos : TOccurrenceRec;
  locMultiOccurrence : Boolean;
begin
  if (AGroupCursor <> nil) then begin
    FillChar(locOccurrenceInfos,SizeOf(locOccurrenceInfos),#0);
    locMultiOccurrence := False;
    AGroupCursor.Reset();
    while AGroupCursor.MoveNext() do begin
      locNode := (AGroupCursor.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
      locAttCursor := CreateAttributesCursor(locNode,cetRttiNode);
      locRefCursor :=
        CreateCursorOn(
          locAttCursor.Clone() as IObjectCursor,
          ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_ref)]),TDOMNodeRttiExposer)
        );
      locRefCursor.Reset();
      if locRefCursor.MoveNext() then begin
        s := (locRefCursor.GetCurrent() as TDOMNodeRttiExposer).NodeValue;
        ExplodeQName(s,locLN,locNS);
        if not Context.FindNameSpace(locNS,locLongNS) then
          locLongNS := locNS;
        elt := FSymbols.FindElementNS(locLN,locLongNS);
        if (elt = nil) then begin
          locParser := Context.FindParser(locLongNS);
          if (locParser <> nil) then
            elt := locParser.ParseType(locLN,ExtractNameFromQName(locNode.NodeName));
        end;
        if (elt <> nil) then begin
          if not elt.InheritsFrom(TPasClassType) then
            raise EXsdInvalidElementDefinitionException.CreateFmt(SERR_UnableToResolveGroupRef,[FTypeName,elt.Name]);
          if (AArrayItems <> nil) then begin
            ExtractOccurences(AClassDef.Name,elt.Name,locAttCursor,locOccurrenceInfos);
            locMultiOccurrence :=
              locOccurrenceInfos.Valid and
              (locOccurrenceInfos.Unboundded or (locOccurrenceInfos.MaxOccurs > 1));
          end;
          AddGroup(AClassDef,elt as TPasClassType,locMultiOccurrence,AArrayItems);
        end;
      end
    end;
  end;
end;

procedure TComplexTypeParser.CreateNodeCursors();
begin
  FAttCursor := CreateAttributesCursor(FTypeNode,cetRttiNode);
  FChildCursor := CreateChildrenCursor(FTypeNode,cetRttiNode);
end;

procedure TComplexTypeParser.ExtractTypeName();
var
  locCrs : IObjectCursor;
begin
  if not FEmbededDef then begin
    locCrs := CreateCursorOn(
                FAttCursor.Clone() as IObjectCursor,
                ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_name)]),TDOMNodeRttiExposer)
              );
    locCrs.Reset();
    if not locCrs.MoveNext() then
      raise EXsdParserException.Create(SERR_UnableToFindNameTagInNode);
    FTypeName := (locCrs.GetCurrent() as TDOMNodeRttiExposer).NodeValue;
  end;
  if IsStrEmpty(FTypeName) then
    raise EXsdParserException.Create(SERR_InvalidTypeName);
end;

procedure TComplexTypeParser.ExtractMixedStatus();
var
  locCrs : IObjectCursor;
  locValue : string;
begin
  FMixed := False;
  if (FAttCursor <> nil) then begin
    locCrs := CreateCursorOn(
                FAttCursor.Clone() as IObjectCursor,
                ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_mixed)]),TDOMNodeRttiExposer)
              );
    locCrs.Reset();
    if locCrs.MoveNext() then begin
      locValue := Trim((locCrs.GetCurrent() as TDOMNodeRttiExposer).NodeValue);
      if (locValue = 'true') then
        FMixed := True;
    end;
  end;
end;

procedure TComplexTypeParser.ExtractContentType();
var
  locCrs : IObjectCursor;
begin
  FContentType := '';
  if Assigned(FChildCursor) then begin
    locCrs := CreateCursorOn(
                FChildCursor.Clone() as IObjectCursor,
                ParseFilter(CreateQualifiedNameFilterStr(s_complexContent,Context.GetXsShortNames()),TDOMNodeRttiExposer)
              );
    if Assigned(locCrs) then begin
      locCrs.Reset();
      if locCrs.MoveNext() then begin
        FContentNode := (locCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
        FContentType := FContentNode.NodeName;
      end else begin
        locCrs := CreateCursorOn(
                    FChildCursor.Clone() as IObjectCursor,
                    ParseFilter(CreateQualifiedNameFilterStr(s_simpleContent,Context.GetXsShortNames()),TDOMNodeRttiExposer)
                  );
        locCrs.Reset();
        if locCrs.MoveNext() then begin
          FContentNode := (locCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
          FContentType := FContentNode.NodeName;
        end else begin
          FContentNode := FTypeNode;
          FContentType := s_complexContent;
        end;
      end;
      FContentType := ExtractNameFromQName(FContentType);
    end;
  end;
end;

procedure TComplexTypeParser.ExtractBaseType();
var
  locContentChildCrs, locCrs : IObjectCursor;
  locSymbol : TPasElement;
  locBaseTypeLocalSpace, locBaseTypeLocalName, locBaseTypeInternalName, locFilterStr : string;
  locBaseTypeLocalSpaceExpanded : string;
begin
  if FMixed then begin
    FDerivationMode := dmNone;
    FDerivationNode := nil;
    exit;
  end;
  locFilterStr := CreateQualifiedNameFilterStr(s_extension,Context.GetXsShortNames());
  locContentChildCrs := CreateChildrenCursor(FContentNode,cetRttiNode);
  locCrs := CreateCursorOn(
              locContentChildCrs.Clone() as IObjectCursor,
              ParseFilter(locFilterStr,TDOMNodeRttiExposer)
            );
  locCrs.Reset();
  if locCrs.MoveNext() then begin
    FDerivationMode := dmExtension;
    FDerivationNode := (locCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
  end else begin
    locFilterStr := CreateQualifiedNameFilterStr(s_restriction,Context.GetXsShortNames());
    locCrs := CreateCursorOn(
                locContentChildCrs.Clone() as IObjectCursor,
                ParseFilter(locFilterStr,TDOMNodeRttiExposer)
              );
    locCrs.Reset();
    if locCrs.MoveNext() then begin
      FDerivationMode := dmRestriction;
      FDerivationNode := (locCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
    end else begin
      FDerivationMode := dmNone;
      FDerivationNode := nil;
   end;
  end;
  if ( FDerivationMode > dmNone ) then begin
    locCrs := CreateCursorOn(
      CreateAttributesCursor(FDerivationNode,cetRttiNode),
      ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_base)]),TDOMNodeRttiExposer)
    );
    locCrs.Reset();
    if not locCrs.MoveNext() then
      raise EXsdParserException.CreateFmt(SERR_InvalidTypeDef_BaseAttributeNotFound,[FTypeName]);
    ExplodeQName((locCrs.GetCurrent() as TDOMNodeRttiExposer).NodeValue,locBaseTypeLocalName,locBaseTypeLocalSpace);
    locSymbol := FindElementNS(locBaseTypeLocalSpace,locBaseTypeLocalName,nvtShortSynonym);
    if Assigned(locSymbol) then begin
      if locSymbol.InheritsFrom(TPasType) then begin
        FBaseType := locSymbol as TPasType;
        while Assigned(FBaseType) and FBaseType.InheritsFrom(TPasAliasType) do begin
          FBaseType := (FBaseType as TPasAliasType).DestType;
        end;
        if FBaseType.InheritsFrom(TPasNativeSimpleType) then begin
          Assert(Assigned(TPasNativeSimpleType(FBaseType).ExtendableType));
          FBaseType := TPasNativeSimpleType(FBaseType).ExtendableType;
        end else if FBaseType.InheritsFrom(TPasNativeClassType) then begin
          if Assigned(TPasNativeClassType(FBaseType).ExtendableType) then
            FBaseType := TPasNativeClassType(FBaseType).ExtendableType;
        end;
      end else begin
        raise EXsdParserException.CreateFmt(SERR_ExpectedTypeDefinition,[locSymbol.Name]);
      end;
    end else begin
      if ( FDerivationMode = dmRestriction ) and
         ( locBaseTypeLocalName = 'Array' ) and
         ( Context.FindNameSpace(locBaseTypeLocalSpace,locBaseTypeLocalSpaceExpanded) and
           ( locBaseTypeLocalSpaceExpanded = s_soapEncodingNameSpace )
         )
      then begin
        FHints := FHints + [pthDeriveFromSoapArray];
      end else begin
        locBaseTypeInternalName := ExtractIdentifier(locBaseTypeLocalName);
        if IsReservedKeyWord(locBaseTypeInternalName) then
          locBaseTypeInternalName := '_' + locBaseTypeInternalName ;
        FBaseType := TPasUnresolvedTypeRef(FSymbols.CreateElement(TPasUnresolvedTypeRef,locBaseTypeInternalName,nil,visDefault,'',0));
        if not AnsiSameText(locBaseTypeInternalName,locBaseTypeLocalName) then
          FSymbols.RegisterExternalAlias(FBaseType,locBaseTypeLocalName);
      end;
    end;
  end;
end;

function TComplexTypeParser.ParseComplexContent(const ATypeName : string) : TPasType;
var
  classDef : TPasClassType;
  isArrayDef : Boolean;
  arrayItems : TPropInfoReferenceList;

  function IsCollectionArray(AElement : TDOMNode) : Boolean;
  var
    strBuffer : string;
  begin
    Result := wst_findCustomAttributeXsd(Context.GetXsShortNames(),AElement,s_WST_collection,strBuffer) and AnsiSameText('true',Trim(strBuffer));
  end;

  procedure ParseElement(
          AElement        : TDOMNode;
    const ABoundInfos     : TOccurrenceRec;
    const AIsChoiceParent : Boolean
  );
  var
    locAttCursor, locPartCursor : IObjectCursor;
    locName, locTypeName, locTypeInternalName : string;
    locType : TPasElement;
    locInternalEltName : string;
    locProp : TPasProperty;
    locHasInternalName : Boolean;
    locMinOccur, locMaxOccur : Integer;
    locMaxOccurUnbounded : Boolean;
    locStrBuffer : string;
    locIsRefElement : Boolean;
    locTypeHint : string;
    locTypeAddRef : Boolean;
    locIsAttribute : Boolean;
    k : Integer;
  begin
    locType := nil;
    locTypeName := '';
    locTypeHint := '';
    locInternalEltName := '';
    locAttCursor := CreateAttributesCursor(AElement,cetRttiNode);
    locPartCursor := CreateCursorOn(locAttCursor.Clone() as IObjectCursor,ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_name)]),TDOMNodeRttiExposer));
    locPartCursor.Reset();
    locIsRefElement := False;
    locTypeAddRef := True;
    if not locPartCursor.MoveNext() then begin
      locPartCursor := CreateCursorOn(locAttCursor.Clone() as IObjectCursor,ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_ref)]),TDOMNodeRttiExposer));
      locPartCursor.Reset();
      if not locPartCursor.MoveNext() then begin
        raise EXsdParserException.Create(SERR_InvalidElementDef_MissingNameOrRef);
      end;
      locIsRefElement := True;
    end;
    locName := (locPartCursor.GetCurrent() as TDOMNodeRttiExposer).NodeValue;
    if locIsRefElement then
      locName := ExtractNameFromQName(locName);
    locInternalEltName := ExtractIdentifier(locName);
    if IsStrEmpty(locName) then
      raise EXsdParserException.Create(SERR_InvalidElementDef_EmptyName);
    if locIsRefElement then begin
      locTypeName := locName;
    end else begin
      locPartCursor := CreateCursorOn(locAttCursor.Clone() as IObjectCursor,ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_type)]),TDOMNodeRttiExposer));
      locPartCursor.Reset();
      if locPartCursor.MoveNext() then begin
        locTypeName := ExtractNameFromQName(TDOMNodeRttiExposer(locPartCursor.GetCurrent()).NodeValue);
        locTypeHint := ExtractTypeHint(AElement);
      end else begin
        locTypeName := Format('%s_%s_Type',[FTypeName,locName]);
        if AElement.HasChildNodes() then begin
          locType := TAbstractTypeParser.ExtractEmbeddedTypeFromElement(Context,AElement,FSymbols,locTypeName);
          if ( locType = nil ) then begin
            raise EXsdInvalidElementDefinitionException.CreateFmt(SERR_InvalidElementDef_Type,[FTypeName,locName]);
          end;
          Self.Module.InterfaceSection.Declarations.Add(locType);
          Self.Module.InterfaceSection.Types.Add(locType);
          if locType.InheritsFrom(TPasClassType) then begin
            Self.Module.InterfaceSection.Classes.Add(locType);
          end;
        end else begin
          locTypeName := 'anyType';
        end;
      end;
    end;
    if IsStrEmpty(locTypeName) then
      raise EXsdInvalidElementDefinitionException.Create(SERR_InvalidElementDef_EmptyType);
    locType := FindElementWithHint(locTypeName,locTypeHint,ssGlobal);
    if Assigned(locType) then begin
      if locIsRefElement then begin
        locTypeInternalName := locTypeName;
        locTypeInternalName := locTypeInternalName + '_Type';
        locType.Name := locTypeInternalName;
        FSymbols.RegisterExternalAlias(locType,locTypeName);
      end;
    end else begin
      locTypeInternalName := ExtractIdentifier(locTypeName);
      if locIsRefElement or AnsiSameText(locTypeInternalName,locInternalEltName) then begin
        locTypeInternalName := locTypeInternalName + '_Type';
      end;
      if IsReservedKeyWord(locTypeInternalName) then begin
        locTypeInternalName := '_' + locTypeInternalName;
      end;
      locType := TPasUnresolvedTypeRef(FSymbols.CreateElement(TPasUnresolvedTypeRef,locTypeInternalName,nil{Self.Module.InterfaceSection},visDefault,'',0));
      locTypeAddRef := False;
      //Self.Module.InterfaceSection.Declarations.Add(locType);
      //Self.Module.InterfaceSection.Types.Add(locType);
      if not AnsiSameText(locTypeInternalName,locTypeName) then
        FSymbols.RegisterExternalAlias(locType,locTypeName);
    end;
    
    locInternalEltName := ExtractIdentifier(locName);
    locHasInternalName := (locInternalEltName <> locName);
    if IsReservedKeyWord(locInternalEltName) then begin
      locHasInternalName := True; 
      locInternalEltName := Format('_%s',[locInternalEltName]);
    end;

    locIsAttribute := AnsiSameText(s_attribute,ExtractNameFromQName(AElement.NodeName));
    if (FindMember(classDef,locInternalEltName) <> nil) then begin
      locHasInternalName := True;
      k := 0;
      while True do begin
        if locIsAttribute then
          locInternalEltName := Format('%sAtt',[locInternalEltName])
        else
          locInternalEltName := Format('%sElt',[locInternalEltName]);
        if (k > 0) then
          locInternalEltName := locInternalEltName+IntToStr(k);
        if (FindMember(classDef,locInternalEltName) = nil) then
          break;
        k := k+1;
      end;
    end;
    locProp := TPasProperty(FSymbols.CreateElement(TPasProperty,locInternalEltName,classDef,visPublished,'',0));
    classDef.Members.Add(locProp);
    locProp.VarType := locType as TPasType;
    if locTypeAddRef then
      locType.AddRef();
    if locHasInternalName then
      FSymbols.RegisterExternalAlias(locProp,locName);
    {if AnsiSameText(locType.Name,locProp.Name) then begin
      FSymbols.RegisterExternalAlias(locType,FSymbols.GetExternalName(locType));
      TPasEmentCrack(locType).SetName(locType.Name + '_Type');
    end;}

    if locIsAttribute then begin
      locPartCursor := CreateCursorOn(locAttCursor.Clone() as IObjectCursor,ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_use)]),TDOMNodeRttiExposer));
      locPartCursor.Reset();
      if locPartCursor.MoveNext() then begin
        locStrBuffer := ExtractNameFromQName((locPartCursor.GetCurrent() as TDOMNodeRttiExposer).NodeValue);
        if IsStrEmpty(locStrBuffer) then
          raise EXsdInvalidDefinitionException.Create(SERR_InvalidAttributeDef_EmptyUse);
        case AnsiIndexText(locStrBuffer,[s_required,s_optional,s_prohibited]) of
          0 : locMinOccur := 1;
          1 : locMinOccur := 0;
          2 : locMinOccur := -1;
          else
            raise EXsdInvalidDefinitionException.CreateFmt(SERR_InvalidAttributeDef_InvalidUse,[locStrBuffer]);
        end;
      end else begin
        locMinOccur := 0;
      end;
    end else begin
      if ABoundInfos.Valid then
        locMinOccur := ABoundInfos.MinOccurs
      else
        locMinOccur := 1;
      locPartCursor := CreateCursorOn(locAttCursor.Clone() as IObjectCursor,ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_minOccurs)]),TDOMNodeRttiExposer));
      locPartCursor.Reset();
      if locPartCursor.MoveNext() then begin
        if not TryStrToInt((locPartCursor.GetCurrent() as TDOMNodeRttiExposer).NodeValue,locMinOccur) then
          raise EXsdParserException.CreateFmt(SERR_InvalidMinOccursValue,[FTypeName,locName]);
        if ( locMinOccur < 0 ) then
          raise EXsdParserException.CreateFmt(SERR_InvalidMinOccursValue,[FTypeName,locName]);
      end;
    end;
    locProp.ReadAccessorName := 'F' + locProp.Name;
    locProp.WriteAccessorName := 'F' + locProp.Name;
    if ( locMinOccur = 0 ) then begin
      locProp.StoredAccessorName := sWST_PROP_STORE_PREFIX + locProp.Name;
    end else if ( locMinOccur = -1 ) then begin
      locProp.StoredAccessorName := 'False';
    end else begin
      locProp.StoredAccessorName := 'True';
    end;

    if locIsAttribute then begin
      locMaxOccur := 1;
      locMaxOccurUnbounded := False;
    end else begin
      if ABoundInfos.Valid then begin
        locMaxOccur := ABoundInfos.MaxOccurs;
        locMaxOccurUnbounded := ABoundInfos.Unboundded;
      end else begin
        locMaxOccur := 1;
        locMaxOccurUnbounded := False;
      end;
      locPartCursor := CreateCursorOn(locAttCursor.Clone() as IObjectCursor,ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_maxOccurs)]),TDOMNodeRttiExposer));
      locPartCursor.Reset();
      if locPartCursor.MoveNext() then begin
        locStrBuffer := (locPartCursor.GetCurrent() as TDOMNodeRttiExposer).NodeValue;
        if AnsiSameText(locStrBuffer,s_unbounded) then begin
          locMaxOccurUnbounded := True;
        end else begin
          if not TryStrToInt(locStrBuffer,locMaxOccur) then
            raise EXsdParserException.CreateFmt(SERR_InvalidMaxOccursValue,[FTypeName,locName]);
          if ( locMinOccur < 0 ) then
            raise EXsdParserException.CreateFmt(SERR_InvalidMaxOccursValue,[FTypeName,locName]);
        end;
      end;
    end;
    isArrayDef := not(locIsAttribute) and (locMaxOccurUnbounded or (locMaxOccur > 1));
    if isArrayDef then begin
      arrayItems.Add(locProp).FIsCollection := IsCollectionArray(AElement);
    end;
    if locIsAttribute then
      FSymbols.SetPropertyAsAttribute(locProp,True);
    locPartCursor := CreateCursorOn(locAttCursor.Clone() as IObjectCursor,ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_default)]),TDOMNodeRttiExposer));
    locPartCursor.Reset();
    if locPartCursor.MoveNext() then
    {$IFDEF HAS_EXP_TREE}
      locProp.DefaultExpr:=TPrimitiveExpr.Create(locProp,pekString,(locPartCursor.GetCurrent() as TDOMNodeRttiExposer).NodeValue);
    {$ELSE HAS_EXP_TREE}
      locProp.DefaultValue := (locPartCursor.GetCurrent() as TDOMNodeRttiExposer).NodeValue;
    {$ENDIF HAS_EXP_TREE}
    if AIsChoiceParent then
      FSymbols.SetPropertyAsChoice(locProp,AIsChoiceParent);
    ExtractExtendedMetadata(locProp,AElement);
  end;

  function IsRecordType() : Boolean;
  var
    strBuffer : string;
  begin
    Result := wst_findCustomAttributeXsd(Context.GetXsShortNames(),FTypeNode,s_WST_record,strBuffer) and AnsiSameText('true',Trim(strBuffer));
  end;
  
  procedure ParseElementsAndAttributes(
          AEltCrs,
          AEltAttCrs      : IObjectCursor;
          ABoundInfos     : TOccurrenceRec;
    const AIsChoiceParent : Boolean
  );

    function ExtractElement(ANode : TDOMNode) : IObjectCursor;
    var
      tmpFilter : IObjectFilter;
    begin
      tmpFilter := ParseFilter(CreateQualifiedNameFilterStr(s_element,Context.GetXsShortNames()),TDOMNodeRttiExposer);
      tmpFilter := TAggregatedFilter.Create(
                     tmpFilter,
                     ParseFilter(CreateQualifiedNameFilterStr(s_choice,Context.GetXsShortNames()),TDOMNodeRttiExposer),
                     fcOr
                   ) as IObjectFilter;
      Result := CreateCursorOn(
                  CreateChildrenCursor(ANode,cetRttiNode),
                  tmpFilter
                );
    end;

  var
    locNode : TDOMNode;
    locNS, locLN : string;
    locEltCrs, locEltAttCrs : IObjectCursor;
    locBoundInfos : TOccurrenceRec;
  begin
    FillChar(locBoundInfos,SizeOf(locBoundInfos),#0);
    if Assigned(AEltCrs) then begin
      locEltAttCrs := nil;
      AEltCrs.Reset();
      while AEltCrs.MoveNext() do begin
        locNode := (AEltCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
        ExplodeQName(locNode.NodeName,locLN,locNS);
        if (locLN = s_choice) then begin
          locEltCrs := ExtractElement(locNode);
          if (locEltCrs <> nil) then begin
            locEltAttCrs := CreateAttributesCursor(locNode,cetRttiNode);
            FillChar(locBoundInfos,SizeOf(locBoundInfos),#0);
            ExtractOccurences(FTypeName,s_choice,locEltAttCrs,locBoundInfos.MinOccurs,locBoundInfos.MaxOccurs,locBoundInfos.Unboundded);
            locBoundInfos.MinOccurs := 0;
            locBoundInfos.Valid := True;
            ParseElementsAndAttributes(locEltCrs,nil,locBoundInfos,True);
          end;
        end else begin
          ParseElement(locNode,ABoundInfos,AIsChoiceParent);
        end;
      end;
    end;
    if Assigned(AEltAttCrs) then begin
      AEltAttCrs.Reset();
      while AEltAttCrs.MoveNext() do begin
        ParseElement(
          (AEltAttCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject,
          ABoundInfos,AIsChoiceParent
        );
      end;
    end;
  end;

  procedure CopyExtendedMetaData(ASource,ADesc : TPasElement);
  var
    ls : TStrings;
  begin
    ls := FSymbols.Properties.FindList(ASource);
    if ( ls <> nil ) then
      FSymbols.Properties.GetList(ADesc).Assign(ls);
  end;

  procedure ProcessXsdAnyDeclarations(AAnyNode, AAnyAttNode : TDOMNode; AType : TPasType);
  var
    anyElt : TDOMElement;
    ls : TStringList;
    anyDec : string;
  begin
    if ( AAnyNode <> nil ) then begin
      anyElt := AAnyNode as TDOMElement;
      ls := TStringList.Create();
      try
        if anyElt.hasAttribute(s_processContents) then
          ls.Values[s_processContents] := anyElt.GetAttribute(s_processContents);
        if anyElt.hasAttribute(s_minOccurs) then
          ls.Values[s_minOccurs] := anyElt.GetAttribute(s_minOccurs);
        if anyElt.hasAttribute(s_maxOccurs) then
          ls.Values[s_maxOccurs] := anyElt.GetAttribute(s_maxOccurs);
        if ( ls.Count > 0 ) then begin
          ls.Delimiter := ';';
          anyDec := ls.DelimitedText;
        end;
      finally
        ls.Free();
      end;
      FSymbols.Properties.SetValue(AType,Format('%s#%s',[s_xs,s_any]),anyDec);
    end;
    if ( AAnyAttNode <> nil ) then begin
      anyDec := '';
      anyElt := AAnyAttNode as TDOMElement;
      if anyElt.hasAttribute(s_processContents) then
        anyDec := anyElt.GetAttribute(s_processContents);
      FSymbols.Properties.SetValue(AType,Format('%s#%s',[s_xs,s_anyAttribute]),Format('%s=%s',[s_processContents,anyDec]));
    end;
  end;

var
  eltCrs, eltAttCrs, grpCrs, attGrpCrs : IObjectCursor;
  internalName : string;
  hasInternalName : Boolean;
  arrayDef : TPasArrayType;
  propTyp, tmpPropTyp : TPasProperty;
  tmpClassDef : TPasClassType;
  i : Integer;
  recordType : TPasRecordType;
  tmpRecVar : TPasVariable;
  locStrBuffer : string;
  locAnyNode, locAnyAttNode : TDOMNode;
  locDefaultAncestorUsed : Boolean;
  locBoundInfos : TOccurrenceRec;
  locTempNode : TDOMNode;
  locIsChoiceParent : Boolean;
begin
  ExtractBaseType();
  eltCrs := ExtractElementCursor(nil,eltAttCrs,grpCrs,attGrpCrs,locAnyNode,locAnyAttNode);

  internalName := ExtractIdentifier(ATypeName);
  hasInternalName := IsReservedKeyWord(internalName) or
                     ( not IsValidIdent(internalName) ) or
                     ( FSymbols.FindElementInModule(internalName,Self.Module,[elkName]) <> nil );
  if hasInternalName then begin
    internalName := Format('%s_Type',[internalName]);
  end;
  hasInternalName := hasInternalName or not(AnsiSameText(internalName,ATypeName));

  if ( pthDeriveFromSoapArray in FHints ) or
     ( ( FDerivationMode = dmRestriction ) and FSymbols.SameName(FBaseType,s_array) )
  then begin
    Result := ExtractSoapArray(ATypeName,internalName,hasInternalName);
  end else begin
    arrayItems := TPropInfoReferenceList.Create();
    try
      classDef := TPasClassType(FSymbols.CreateElement(TPasClassType,internalName,Self.Module.InterfaceSection,visDefault,'',0));
      try
        classDef.ObjKind := okClass;
        Result := classDef;
        if hasInternalName then
          FSymbols.RegisterExternalAlias(classDef,ATypeName);
        if ( FDerivationMode in [dmExtension, dmRestriction] ) then begin
          classDef.AncestorType := FBaseType;
        end;
        locDefaultAncestorUsed := False;
        if ( classDef.AncestorType = nil ) then begin
          if FMixed then begin
            classDef.AncestorType := FSymbols.FindElementInModule('TStringBufferRemotable',FSymbols.FindModule('base_service_intf') as TPasModule) as TPasType
          end else if IsHeaderBlock() then begin
            classDef.AncestorType := FSymbols.FindElementInModule('THeaderBlock',FSymbols.FindModule('base_service_intf') as TPasModule) as TPasType
          end else if IsSimpleContentHeaderBlock() then begin
            classDef.AncestorType := FSymbols.FindElementInModule('TSimpleContentHeaderBlock',FSymbols.FindModule('base_service_intf') as TPasModule) as TPasType
          end else begin
            locDefaultAncestorUsed := True;
            classDef.AncestorType := FSymbols.FindElementInModule('TBaseComplexRemotable',FSymbols.FindModule('base_service_intf') as TPasModule) as TPasType;
          end;
        end;
        //if not classDef.AncestorType.InheritsFrom(TPasUnresolvedTypeRef) then
        classDef.AncestorType.AddRef();
        if Assigned(eltCrs) or Assigned(eltAttCrs) then begin
          isArrayDef := False;
          locIsChoiceParent := False;
          FillChar(locBoundInfos,SizeOf(locBoundInfos),#0);
          if (eltCrs <> nil) then begin
            eltCrs.Reset();
            if eltCrs.MoveNext() then begin
              locTempNode := (eltCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
              locTempNode := locTempNode.ParentNode;
              if (ExtractNameFromQName(locTempNode.NodeName) = s_choice) then begin
                ExtractOccurences(
                  FTypeName,s_choice,
                  CreateAttributesCursor(locTempNode,cetRttiNode),
                  locBoundInfos.MinOccurs,locBoundInfos.MaxOccurs,locBoundInfos.Unboundded
                );
                locBoundInfos.MinOccurs := 0;
                locBoundInfos.Valid := True;
                locIsChoiceParent := True;
              end;
            end;
          end;
          ParseElementsAndAttributes(eltCrs,eltAttCrs,locBoundInfos,locIsChoiceParent);
          ParseGroups(classDef,grpCrs,arrayItems);
          ParseGroups(classDef,attGrpCrs,nil);
          if ( arrayItems.GetCount() > 0 ) then begin
            if ( arrayItems.GetCount() = 1 ) and locDefaultAncestorUsed and
               ( GetElementCount(classDef.Members,TPasProperty) = 1 )
            then begin
              Result := nil;
              propTyp := arrayItems.GetItem(0).Prop;
              arrayDef := FSymbols.CreateArray(internalName,propTyp.VarType,propTyp.Name,FSymbols.GetExternalName(propTyp),asScoped);
              FSymbols.FreeProperties(classDef);
              FreeAndNil(classDef);
              Result := arrayDef;
              if hasInternalName then
                FSymbols.RegisterExternalAlias(arrayDef,ATypeName);
              if arrayItems.GetItem(0).IsCollection then
                FSymbols.SetCollectionFlag(arrayDef,True);
            end else begin
              GenerateArrayTypes(internalName,arrayItems);
              tmpClassDef := classDef;
              classDef := TPasClassType(FSymbols.CreateElement(TPasClassType,tmpClassDef.Name,Self.Module.InterfaceSection,visPublic,'',0));
              classDef.ObjKind := okClass;
              Result := classDef;
              classDef.AncestorType := tmpClassDef.AncestorType;
              classDef.AncestorType.AddRef();
              if hasInternalName then
                FSymbols.RegisterExternalAlias(classDef,ATypeName);
              for i := 0 to Pred(tmpClassDef.Members.Count) do begin
                if TPasElement(tmpClassDef.Members[i]).InheritsFrom(TPasProperty) then begin
                  propTyp := TPasProperty(tmpClassDef.Members[i]);
                  if ( arrayItems.IndexOf(propTyp) = -1 ) then begin
                    tmpPropTyp := TPasProperty(FSymbols.CreateElement(TPasProperty,propTyp.Name,classDef,visPublished,'',0));
                    if FSymbols.IsAttributeProperty(propTyp) then begin
                      FSymbols.SetPropertyAsAttribute(tmpPropTyp,True);
                    end;
                    tmpPropTyp.VarType := propTyp.VarType;
                    tmpPropTyp.VarType.AddRef();
                    tmpPropTyp.StoredAccessorName := propTyp.StoredAccessorName;
                    FSymbols.RegisterExternalAlias(tmpPropTyp,FSymbols.GetExternalName(propTyp));
                    CopyExtendedMetaData(propTyp,tmpPropTyp);
                    classDef.Members.Add(tmpPropTyp);
                  end else begin
                    tmpPropTyp := TPasProperty(FSymbols.CreateElement(TPasProperty,propTyp.Name,classDef,visPublished,'',0));
                    tmpPropTyp.StoredAccessorName := propTyp.StoredAccessorName;
                    tmpPropTyp.VarType := FSymbols.FindElement(Format('%s_%sArray',[internalName,propTyp.Name])) as TPasType;
                    tmpPropTyp.VarType.AddRef();
                    FSymbols.RegisterExternalAlias(tmpPropTyp,FSymbols.GetExternalName(propTyp));
                    CopyExtendedMetaData(propTyp,tmpPropTyp);
                    classDef.Members.Add(tmpPropTyp);
                  end;
                end;
              end;
              FSymbols.FreeProperties(tmpClassDef);
              FreeAndNil(tmpClassDef);
            end;
          end;
        end;

        //check for record
        if ( FDerivationMode = dmNone ) and
           Result.InheritsFrom(TPasClassType) and
           IsRecordType()
        then begin
          tmpClassDef := classDef;
          classDef := nil;
          recordType := TPasRecordType(FSymbols.CreateElement(TPasRecordType,tmpClassDef.Name,Self.Module.InterfaceSection,visPublic,'',0));
          Result := recordType;
          if hasInternalName then
            FSymbols.RegisterExternalAlias(recordType,ATypeName);
          for i := 0 to Pred(tmpClassDef.Members.Count) do begin
            if TPasElement(tmpClassDef.Members[i]).InheritsFrom(TPasProperty) then begin
              propTyp := TPasProperty(tmpClassDef.Members[i]);
              tmpRecVar := TPasVariable(FSymbols.CreateElement(TPasVariable,propTyp.Name,recordType,visPublic,'',0));
              tmpRecVar.VarType := propTyp.VarType;
              tmpRecVar.VarType.AddRef();
              FSymbols.RegisterExternalAlias(tmpRecVar,FSymbols.GetExternalName(propTyp));
              recordType.Members.Add(tmpRecVar);
              if FSymbols.IsAttributeProperty(propTyp) then begin
                FSymbols.SetPropertyAsAttribute(tmpRecVar,True);
              end;
              if AnsiSameText(propTyp.StoredAccessorName,'False') then
                locStrBuffer := s_prohibited
              else if AnsiSameText(Copy(propTyp.StoredAccessorName,1,Length(sWST_PROP_STORE_PREFIX)),sWST_PROP_STORE_PREFIX) then
                locStrBuffer := s_optional
              else
                locStrBuffer := s_required;
              FSymbols.Properties.SetValue(tmpRecVar,s_WST_storeType,locStrBuffer);    
            end;
          end;
          FSymbols.FreeProperties(tmpClassDef);
          FreeAndNil(tmpClassDef);
        end;

        if (FDerivationMode = dmRestriction) and Result.InheritsFrom(TPasClassType) then
          Context.AddTypeToCheck(Result);

        if ( locAnyNode <> nil ) or ( locAnyAttNode <> nil ) then
          ProcessXsdAnyDeclarations(locAnyNode,locAnyAttNode,Result);
      except
        FSymbols.FreeProperties(Result);
        FreeAndNil(Result);
        raise;
      end;
    finally
      FreeAndNil(arrayItems);
    end;
  end;
end;

function TComplexTypeParser.ParseSimpleContent(const ATypeName : string) : TPasType;

  function ExtractAttributeCursor():IObjectCursor;
  var
    frstCrsr, tmpCursor : IObjectCursor;
    parentNode, tmpNode : TDOMNode;
    locFilterStr : string;
    xsShortNameList : TStrings;
  begin
    Result := nil;
    parentNode := FContentNode;
    if parentNode.HasChildNodes() then begin;
      xsShortNameList := Context.GetXsShortNames();
      frstCrsr := CreateChildrenCursor(parentNode,cetRttiNode);
      locFilterStr := CreateQualifiedNameFilterStr(s_extension,xsShortNameList) + ' or ' +
                      CreateQualifiedNameFilterStr(s_restriction,xsShortNameList) ;
      tmpCursor := CreateCursorOn(frstCrsr.Clone() as IObjectCursor,ParseFilter(locFilterStr,TDOMNodeRttiExposer));
      if Assigned(tmpCursor) then begin
        tmpCursor.Reset();
        if tmpCursor.MoveNext() then begin
          tmpNode := (tmpCursor.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
          if tmpNode.HasChildNodes() then begin
            locFilterStr := CreateQualifiedNameFilterStr(s_attribute,xsShortNameList);
            tmpCursor := CreateCursorOn(CreateChildrenCursor(tmpNode,cetRttiNode),ParseFilter(locFilterStr,TDOMNodeRttiExposer));
            if Assigned(tmpCursor) then begin
              Result := tmpCursor;
              Result.Reset();
            end;
          end;
        end;
      end;
    end else begin
      Result := nil;
    end;
  end;

var
  locClassDef : TPasClassType;

  procedure ParseAttribute(AElement : TDOMNode);
  var
    locAttCursor, locPartCursor : IObjectCursor;
    locName, locTypeName, locStoreOpt : string;
    locType : TPasElement;
    locStoreOptIdx : Integer;
    locAttObj : TPasProperty;
    locInternalEltName : string;
    locHasInternalName : boolean;
    locIsRefElement : Boolean;
    locTypeInternalName : string;
    locTypeAddRef : Boolean;
  begin
    locIsRefElement := False;
    locTypeAddRef := True;
    locAttCursor := CreateAttributesCursor(AElement,cetRttiNode);
    locPartCursor := CreateCursorOn(locAttCursor.Clone() as IObjectCursor,ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_name)]),TDOMNodeRttiExposer));
    locPartCursor.Reset();
    if not locPartCursor.MoveNext() then begin
      locPartCursor := CreateCursorOn(locAttCursor.Clone() as IObjectCursor,ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_ref)]),TDOMNodeRttiExposer));
      locPartCursor.Reset();
      if not locPartCursor.MoveNext() then
        raise EXsdInvalidDefinitionException.Create(SERR_InvalidAttributeDef_MissingName);
      locIsRefElement := True;
    end;
    locName := (locPartCursor.GetCurrent() as TDOMNodeRttiExposer).NodeValue;
    if locIsRefElement then
      locName := ExtractNameFromQName(locName);
    if IsStrEmpty(locName) then
      raise EXsdInvalidDefinitionException.Create(SERR_InvalidAttributeDef_EmptyName);
    locInternalEltName := ExtractIdentifier(locName);

    if locIsRefElement then begin
      locTypeName := locName;
    end else begin
      locPartCursor := CreateCursorOn(locAttCursor.Clone() as IObjectCursor,ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_type)]),TDOMNodeRttiExposer));
      locPartCursor.Reset();
      if not locPartCursor.MoveNext() then
        raise EXsdInvalidDefinitionException.Create(SERR_InvalidAttributeDef_MissingType);
      locTypeName := ExtractNameFromQName((locPartCursor.GetCurrent() as TDOMNodeRttiExposer).NodeValue);
    end;
    if IsStrEmpty(locTypeName) then
      raise EXsdInvalidDefinitionException.Create(SERR_InvalidAttributeDef_EmptyType);
    locType := FSymbols.FindElement(locTypeName) as TPasType;
    if Assigned(locType) then begin
      if locIsRefElement then begin
        locTypeInternalName := locTypeName;
        locTypeInternalName := locTypeInternalName + '_Type';
        locType.Name := locTypeInternalName;
        FSymbols.RegisterExternalAlias(locType,locTypeName);
      end;
    end else begin
      locTypeInternalName := ExtractIdentifier(locTypeName);
      if locIsRefElement or AnsiSameText(locTypeInternalName,locInternalEltName) then begin
        locTypeInternalName := locTypeInternalName + '_Type';
      end;
      if IsReservedKeyWord(locTypeInternalName) then begin
        locTypeInternalName := '_' + locTypeInternalName;
      end;
      locType := TPasUnresolvedTypeRef(FSymbols.CreateElement(TPasUnresolvedTypeRef,locTypeInternalName,nil{Self.Module.InterfaceSection},visDefault,'',0));
      locTypeAddRef := False;
      if not AnsiSameText(locTypeInternalName,locTypeName) then
        FSymbols.RegisterExternalAlias(locType,locTypeName);
    end;
    
    locPartCursor := CreateCursorOn(locAttCursor.Clone() as IObjectCursor,ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_use)]),TDOMNodeRttiExposer));
    locPartCursor.Reset();
    if locPartCursor.MoveNext() then begin
      locStoreOpt := ExtractNameFromQName((locPartCursor.GetCurrent() as TDOMNodeRttiExposer).NodeValue);
      if IsStrEmpty(locStoreOpt) then
        raise EXsdInvalidDefinitionException.Create(SERR_InvalidAttributeDef_EmptyUse);
      locStoreOptIdx := AnsiIndexText(locStoreOpt,[s_required,s_optional,s_prohibited]);
      if ( locStoreOptIdx < 0 ) then
        raise EXsdInvalidDefinitionException.CreateFmt(SERR_InvalidAttributeDef_InvalidUse,[locStoreOpt]);
    end else begin
      locStoreOptIdx := 1{optional by default!}; //0;
    end;

    locInternalEltName := ExtractIdentifier(locName);
    locHasInternalName := IsReservedKeyWord(locInternalEltName);
    if locHasInternalName then
      locInternalEltName := Format('_%s',[locInternalEltName]);
      
    locAttObj := TPasProperty(FSymbols.CreateElement(TPasProperty,locInternalEltName,locClassDef,visPublished,'',0));
    locClassDef.Members.Add(locAttObj);
    locAttObj.VarType := locType as TPasType;
    if locTypeAddRef then
      locType.AddRef();
    if locHasInternalName then
      FSymbols.RegisterExternalAlias(locAttObj,locName);
    FSymbols.SetPropertyAsAttribute(locAttObj,True);
    case locStoreOptIdx of
      0 : locAttObj.StoredAccessorName := 'True';
      1 : locAttObj.StoredAccessorName := sWST_PROP_STORE_PREFIX + locAttObj.Name;
      2 : locAttObj.StoredAccessorName := 'False';
    end;
  end;

var
  locAttCrs : IObjectCursor;
  internalName : string;
  hasInternalName : Boolean;
begin
  ExtractBaseType();
  if not ( FDerivationMode in [dmExtension, dmRestriction] ) then
    raise EXsdInvalidTypeDefinitionException.Create(SERR_InvalidComplexSimpleTypeDef_NoRestOrExt);

  internalName := ATypeName;
  hasInternalName := IsReservedKeyWord(internalName) or
                     ( not IsValidIdent(internalName) );{ or
                     ( FSymbols.IndexOf(internalName) <> -1 );}
  if hasInternalName then
    internalName := Format('_%s',[internalName]);

  locAttCrs := ExtractAttributeCursor();
  locClassDef := TPasClassType(FSymbols.CreateElement(TPasClassType,Trim(internalName),Self.Module.InterfaceSection,visDefault,'',0));
  try
    locClassDef.ObjKind := okClass;
    Result := locClassDef;
    if hasInternalName then
      FSymbols.RegisterExternalAlias(locClassDef,ATypeName);
    if ( FDerivationMode in [dmExtension, dmRestriction] ) then begin
      locClassDef.AncestorType := FBaseType;
    end;
    if ( locClassDef.AncestorType = nil ) then begin
      locClassDef.AncestorType := FSymbols.FindElementInModule('TBaseComplexRemotable',FSymbols.FindModule('base_service_intf') as TPasModule) as TPasType;
    end;
    locClassDef.AncestorType.AddRef();
    if ( locAttCrs <> nil ) then begin
      locAttCrs.Reset();
      while locAttCrs.MoveNext() do begin
        ParseAttribute((locAttCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject);
      end;
    end;
  except
    FSymbols.FreeProperties(Result);
    FreeAndNil(Result);
    raise;
  end;
end;

function TComplexTypeParser.ParseEmptyContent(const ATypeName: string): TPasType;
var
  internalName : string;
  hasInternalName : Boolean;
begin
  internalName := ATypeName;
  hasInternalName := IsReservedKeyWord(internalName) or
                     ( not IsValidIdent(internalName) );{ or
                     ( FSymbols.IndexOf(internalName) <> -1 );}
  if hasInternalName then
    internalName := Format('_%s',[internalName]);
  Result := TPasClassType(FSymbols.CreateElement(TPasClassType,internalName,Self.Module.InterfaceSection,visDefault,'',0));
  TPasClassType(Result).ObjKind := okClass;
  if hasInternalName then
    FSymbols.RegisterExternalAlias(Result,ATypeName);
  if IsHeaderBlock() then
    TPasClassType(Result).AncestorType := FSymbols.FindElementInModule('THeaderBlock',FSymbols.FindModule('base_service_intf') as TPasModule) as TPasType
  else if IsSimpleContentHeaderBlock() then
    TPasClassType(Result).AncestorType := FSymbols.FindElementInModule('TSimpleContentHeaderBlock',FSymbols.FindModule('base_service_intf') as TPasModule) as TPasType
  else
    TPasClassType(Result).AncestorType := FSymbols.FindElementInModule('TBaseComplexRemotable',FSymbols.FindModule('base_service_intf') as TPasModule) as TPasType;
  TPasClassType(Result).AncestorType.AddRef();
end;

class function TComplexTypeParser.GetParserSupportedStyle(): string;
begin
  Result := s_complexType;
end;

function TComplexTypeParser.Parse() : TPasType;
var
  locSym : TPasElement;
  locContinue : Boolean;
  locTagName : string;
begin
  locTagName := ExtractNameFromQName(FTypeNode.NodeName);
  if (locTagName = s_complexType) then
    FKind := ctkComplexType
  else if (locTagName = s_group) then
    FKind := ctkGroup
  else if (locTagName = s_attributeGroup) then
    FKind := ctkAttributeGroup
  else
    raise EXsdParserAssertException.CreateFmt(SERR_ExpectedButFound,[s_complexType,ExtractNameFromQName(FTypeNode.NodeName)]);
  Result := nil;
  CreateNodeCursors();
  ExtractTypeName();
  if (FKind = ctkComplexType) then
    ExtractMixedStatus();
  locContinue := True;
  locSym := FSymbols.FindElement(FTypeName);
  if Assigned(locSym) then begin
    if not locSym.InheritsFrom(TPasType) then
      raise EXsdParserException.CreateFmt(SERR_ExpectedTypeDefinition,[FTypeName]);
    locContinue := locSym.InheritsFrom(TPasUnresolvedTypeRef) or
                   ( IsEmbeddedType(TPasType(locSym)) <> FEmbededDef );
    if not locContinue then;
      Result := locSym as TPasType;
  end;
  if locContinue then begin
    ExtractContentType();
    if IsStrEmpty(FContentType) and (FKind = ctkComplexType) then begin
      Result := ParseEmptyContent(FTypeName);
    end else begin
      if (FContentType = s_complexContent) or (FKind in [ctkGroup,ctkAttributeGroup]) then
        Result := ParseComplexContent(FTypeName)
      else
        Result := ParseSimpleContent(FTypeName);
    end;
    if ( Result <> nil ) then begin
      if ( IsEmbeddedType(Result) <> FEmbededDef ) then
        SetAsEmbeddedType(Result,FEmbededDef);
      if (FKind in [ctkGroup,ctkAttributeGroup]) then
        SetAsGroupType(Result,True);
    end;
{$IFDEF WST_HANDLE_DOC}
    if ( Result <> nil ) then
      ParseDocumentation(Result);
{$ENDIF WST_HANDLE_DOC}
  end;
end;

{ TSimpleTypeParser }

procedure TSimpleTypeParser.CreateNodeCursors();
begin
  FAttCursor := CreateAttributesCursor(FTypeNode,cetRttiNode);
  FChildCursor := CreateChildrenCursor(FTypeNode,cetRttiNode);
end;

procedure TSimpleTypeParser.ExtractTypeName();
var
  locCrs : IObjectCursor;
begin
  if not FEmbededDef then begin
    locCrs := CreateCursorOn(
                FAttCursor.Clone() as IObjectCursor,
                ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_name)]),TDOMNodeRttiExposer)
              );
    locCrs.Reset();
    if not locCrs.MoveNext() then
      raise EXsdParserAssertException.Create(SERR_UnableToFindNameTagInNode);
    FTypeName := (locCrs.GetCurrent() as TDOMNodeRttiExposer).NodeValue;
  end;
  if IsStrEmpty(FTypeName) then
    raise EXsdParserAssertException.Create(SERR_InvalidTypeName);
end;

function TSimpleTypeParser.ExtractContentType() : Boolean;
var
  locCrs, locAttCrs : IObjectCursor;
  tmpNode : TDOMNode;
  spaceShort : string;
begin
  locCrs := CreateCursorOn(
              FChildCursor.Clone() as IObjectCursor,
              ParseFilter(CreateQualifiedNameFilterStr(s_restriction,Context.GetXsShortNames()),TDOMNodeRttiExposer)
            );
  locCrs.Reset();
  if locCrs.MoveNext() then begin
    FRestrictionNode := (locCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
    tmpNode := nil;
    locAttCrs := CreateAttributesCursor(FRestrictionNode,cetRttiNode);
    if Assigned(locAttCrs) then begin
      locAttCrs := CreateCursorOn(locAttCrs,ParseFilter(Format('%s=%s',[s_NODE_NAME,QuotedStr(s_base)]),TDOMNodeRttiExposer));
      locAttCrs.Reset();
      if locAttCrs.MoveNext() then begin
        tmpNode := (locAttCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
      end;
    end;
    FBaseName := '';
    FBaseNameSpace := '';
    if Assigned(tmpNode) then begin
      ExplodeQName(tmpNode.NodeValue,FBaseName,spaceShort);
      if not Context.FindNameSpace(spaceShort,FBaseNameSpace) then
        raise EXsdParserAssertException.CreateFmt(SERR_CannotResolveNamespace,[spaceShort]);
    end;
    locCrs := CreateChildrenCursor(FRestrictionNode,cetRttiNode) as IObjectCursor;
    if Assigned(locCrs) then begin
      locCrs := CreateCursorOn(
                  locCrs,
                  ParseFilter(CreateQualifiedNameFilterStr(s_enumeration,Context.GetXsShortNames()),TDOMNodeRttiExposer)
                );
      locCrs.Reset();
      if locCrs.MoveNext() then begin
        FIsEnum := True;
      end else begin
        if IsStrEmpty(FBaseName) then
          raise EXsdParserAssertException.CreateFmt(SERR_BaseTypeNotSpecfifiedForSimpleType,[FTypeName]);
        FIsEnum := False
      end;
    end else begin
      if IsStrEmpty(FBaseName) then
        raise EXsdParserAssertException.CreateFmt(SERR_BaseTypeNotSpecfifiedForSimpleType,[FTypeName]);
      FIsEnum := False
    end;
    Result := True;
  end else begin
    //raise EWslParserException.CreateFmt('The parser only support "Restriction" mode simple type derivation, parsing : "%s".',[FTypeName]);
    Result := False;
  end;
end;

function TSimpleTypeParser.ParseEnumContent(): TPasType;

  function ExtractEnumCursor():IObjectCursor ;
  begin
    Result := CreateCursorOn(
                CreateChildrenCursor(FRestrictionNode,cetRttiNode),
                ParseFilter(CreateQualifiedNameFilterStr(s_enumeration,Context.GetXsShortNames()),TDOMNodeRttiExposer)
              );
  end;
  
var
  locRes : TPasEnumType;
  //locOrder : Integer;
  prefixItems : Boolean;
  
  procedure ParseEnumItem(AItemNode : TDOMNode);
  var
    tmpNode : TDOMNode;
    locItemName, locInternalItemName : string;
    locCrs : IObjectCursor;
    locItem : TPasEnumValue;
    locHasInternalName : Boolean;
    locBuffer : string;
  begin
    locCrs := CreateCursorOn(CreateAttributesCursor(AItemNode,cetRttiNode),ParseFilter(Format('%s=%s',[s_NODE_NAME,QuotedStr(s_value)]),TDOMNodeRttiExposer)) as IObjectCursor;
    if not Assigned(locCrs) then
      raise EXsdInvalidDefinitionException.CreateFmt(SERR_InvalidEnumItemNode_NoValueAttribute,[FTypeName]);
    locCrs.Reset();
    if not locCrs.MoveNext() then
      raise EXsdInvalidDefinitionException.CreateFmt(SERR_InvalidEnumItemNode_NoValueAttribute,[FTypeName]);
    tmpNode := (locCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
    locItemName := tmpNode.NodeValue;
    { (26-06-2008) empty string "" can be valid enum item!
    if IsStrEmpty(locItemName) then
      raise EXsdInvalidDefinitionException.CreateFmt('Invalid "enum" item node : the value attribute is empty, type = "%s".',[FTypeName]);
    }
    locInternalItemName := ExtractIdentifier(locItemName);
    if IsStrEmpty(locInternalItemName) then
      locInternalItemName := 'EmptyItem';
    locHasInternalName := prefixItems or
                          IsReservedKeyWord(locInternalItemName) or
                          ( not IsValidIdent(locInternalItemName) ) or
                          ( FSymbols.FindElementInModule(locInternalItemName,Self.Module) <> nil ) or
                          FSymbols.IsEnumItemNameUsed(locInternalItemName,Self.Module) or
                          ( not AnsiSameText(locInternalItemName,locItemName) );
    if locHasInternalName then begin
      locBuffer := ExtractIdentifier(FSymbols.GetExternalName(locRes));
      if ( not IsStrEmpty(locBuffer) ) and ( locBuffer[Length(locBuffer)] <> '_' ) then begin
        locInternalItemName := Format('%s_%s',[locBuffer,locInternalItemName]);
      end else begin
        locInternalItemName := Format('%s%s',[locBuffer,locInternalItemName]);
      end;
    end;
    locItem := TPasEnumValue(FSymbols.CreateElement(TPasEnumValue,locInternalItemName,locRes,visDefault,'',0));
    //locItem.Value := locOrder;
    locRes.Values.Add(locItem);
    if locHasInternalName then
      FSymbols.RegisterExternalAlias(locItem,locItemName);
    //Inc(locOrder);
  end;
  
var
  locEnumCrs : IObjectCursor;
  intrName : string;
  hasIntrnName : Boolean;
begin
  prefixItems := ( poEnumAlwaysPrefix in Context.GetSimpleOptions() );
  locEnumCrs := ExtractEnumCursor();

  intrName := FTypeName;
  hasIntrnName := IsReservedKeyWord(FTypeName) or
                  ( ( FindElement(intrName) <> nil ) and ( not FindElement(intrName).InheritsFrom(TPasUnresolvedTypeRef) ) );
  if hasIntrnName then
    intrName := '_' + intrName;

  locRes := TPasEnumType(FSymbols.CreateElement(TPasEnumType,Trim(intrName),Self.Module.InterfaceSection,visDefault,'',0));
  try
    Result := locRes;
    if hasIntrnName then
      FSymbols.RegisterExternalAlias(locRes,FTypeName);
    locEnumCrs.Reset();
    //locOrder := 0;
    while locEnumCrs.MoveNext() do begin
      ParseEnumItem((locEnumCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject);
    end;
  except
    FSymbols.FreeProperties(Result);
    FreeAndNil(Result);
    raise;
  end;
end;

function TSimpleTypeParser.ParseOtherContent(): TPasType;
var
  intrName : string;
  hasIntrnName : Boolean;
  tmpElement : TPasElement;
  locBaseType : TPasType;
  locBaseTypeInternalName : string;
begin  // todo : implement TSimpleTypeParser.ParseOtherContent
  if IsStrEmpty(FBaseName) then
    raise EXsdInvalidTypeDefinitionException.CreateFmt(SERR_BaseTypeNotSpecfifiedForSimpleType,[FTypeName]);
  intrName := ExtractIdentifier(FTypeName);
  hasIntrnName := ( intrName <> FTypeName ) or
                  IsReservedKeyWord(intrName);
  if not hasIntrnName then begin
    tmpElement := FindElement(intrName);
    if ( tmpElement <> nil ) and ( not tmpElement.InheritsFrom(TPasUnresolvedTypeRef) ) then
      hasIntrnName := True;
  end;
  if IsReservedKeyWord(intrName){hasIntrnName} then
    intrName := '_' + intrName;
  Result := TPasTypeAliasType(FSymbols.CreateElement(TPasTypeAliasType,intrName,Self.Module.InterfaceSection,visDefault,'',0));
  if ( intrName <> FTypeName ) then
    FSymbols.RegisterExternalAlias(Result,FTypeName);
  tmpElement := FindElementNS(FBaseNameSpace,FBaseName,nvtExpandValue);
  if (tmpElement <> nil) then begin
    if not tmpElement.InheritsFrom(TPasType) then
      raise EXsdInvalidTypeDefinitionException.CreateFmt(SERR_ExpectedTypeDefinition,[FBaseName]);
    locBaseType := tmpElement as TPasType;
    locBaseType.AddRef();
  end else begin
    locBaseTypeInternalName := ExtractIdentifier(FBaseName);
    if IsReservedKeyWord(locBaseTypeInternalName) then
      locBaseTypeInternalName := '_' + locBaseTypeInternalName;
    locBaseType := TPasUnresolvedTypeRef(FSymbols.CreateElement(TPasUnresolvedTypeRef,locBaseTypeInternalName,nil,visDefault,'',0));
    if not AnsiSameText(locBaseTypeInternalName,FBaseName) then
      FSymbols.RegisterExternalAlias(locBaseType,FBaseName);
  end;
  TPasTypeAliasType(Result).DestType := locBaseType;
end;

class function TSimpleTypeParser.GetParserSupportedStyle(): string;
begin
  Result := s_simpleType;
end;

function TSimpleTypeParser.Parse(): TPasType;
var
  locSym : TPasElement;
  locContinue : Boolean;
begin
  if not AnsiSameText(ExtractNameFromQName(FTypeNode.NodeName),s_simpleType) then
    raise EXsdParserAssertException.CreateFmt(SERR_ExpectedButFound,[s_simpleType,ExtractNameFromQName(FTypeNode.NodeName)]);
  Result := nil;
  CreateNodeCursors();
  ExtractTypeName();
  locContinue := True;
  locSym := FindElement(FTypeName);
  if Assigned(locSym) then begin
    if not locSym.InheritsFrom(TPasType) then
      raise EXsdParserAssertException.CreateFmt(SERR_ExpectedTypeDefinition,[FTypeName]);
    locContinue := locSym.InheritsFrom(TPasUnresolvedTypeRef);
    if not locContinue then begin
      Result := locSym as TPasType;
    end;
  end;
  if locContinue then begin
    if ExtractContentType() then begin
      if FIsEnum then begin
        Result := ParseEnumContent()
      end else begin
        Result := ParseOtherContent();
      end;
    end else begin
      FBaseName := 'string';
      FBaseNameSpace := s_xs;
      Result := ParseOtherContent();
    end;
    if ( Result <> nil ) then begin
      if ( IsEmbeddedType(Result) <> FEmbededDef ) then
        SetAsEmbeddedType(Result,FEmbededDef);
    end;
{$IFDEF WST_HANDLE_DOC}
    if ( Result <> nil ) then
      ParseDocumentation(Result);
{$ENDIF WST_HANDLE_DOC}
  end;
end;

{ TPropInfoReferenceList }

constructor TPropInfoReferenceList.Create();
begin
  FList := TObjectList.Create(True);
end;

destructor TPropInfoReferenceList.Destroy();
begin
  FList.Free();
  inherited Destroy();
end;

function TPropInfoReferenceList.Add(AProp : TPasProperty) : TPropInfoReference;
var
  i : Integer;
begin
  i := IndexOf(AProp);
  if ( i = -1 ) then begin
    Result := TPropInfoReference.Create();
    Result.FProp := AProp;
    FList.Add(Result);
  end else begin
    Result := TPropInfoReference(FList[i]);
  end;
end;

function TPropInfoReferenceList.GetItem(const AIndex: Integer): TPropInfoReference;
begin
  Result := TPropInfoReference(FList[AIndex]);
end;

function TPropInfoReferenceList.IndexOf(const AProp: TPasProperty): Integer;
var
  i : Integer;
begin
  Result := -1;
  for i := 0 to Pred(FList.Count) do begin
    if ( TPropInfoReference(FList[i]).Prop = AProp ) then begin
      Result := i;
      Break;
    end;
  end;
end;

function TPropInfoReferenceList.GetCount: Integer;
begin
  Result := FList.Count;
end;

initialization
  TAbstractTypeParser.RegisterParser(TSimpleTypeParser);
  TAbstractTypeParser.RegisterParser(TComplexTypeParser);

finalization
  FreeAndNil(FTypeParserList);
  
end.
