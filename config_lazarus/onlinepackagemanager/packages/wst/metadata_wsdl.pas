{
    This file is part of the Web Service Toolkit
    Copyright (c) 2006 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$INCLUDE wst_global.inc}
unit metadata_wsdl;

{$RANGECHECKS OFF}

interface

uses
  Classes, SysUtils, TypInfo,
  {$IFNDEF FPC}xmldom, wst_delphi_xml{$ELSE}DOM{$ENDIF},
  base_service_intf, metadata_repository;

type

  IWsdlTypeHandler = interface
    ['{DA9AF8B1-392B-49A8-91CC-6B5C5131E6FA}']
    procedure Generate(
      const APascalTypeName : string;
            AWsdlDocument   : TXMLDocument;
            ATypeRegistry : TTypeRegistry
    );
  end;

  IWsdlTypeHandlerRegistry = Interface
    ['{A2FA2FE4-933D-44CC-B266-BF48674DECE9}']
    function Find(const APascalTypeName : string):IWsdlTypeHandler;
    procedure Register(
      const APascalTypeName : string;
            AFactory        : IItemFactory
    );
    procedure RegisterDefaultHandler(
      const ATypeKind : TTypeKind;
            AFactory  : IItemFactory
    );
  End;

  { TEnumTypeHandler }

  TEnumTypeHandler = class(TSimpleFactoryItem,IWsdlTypeHandler)
  protected
    procedure Generate(
      const APascalTypeName : string;
            AWsdlDocument   : TXMLDocument;
            ATypeRegistry : TTypeRegistry
    );
  end;

  { TBaseComplexRemotable_TypeHandler }

  TBaseComplexRemotable_TypeHandler = class(TSimpleFactoryItem,IWsdlTypeHandler)
  protected
    procedure Generate(
      const APascalTypeName : string;
            AWsdlDocument   : TXMLDocument;
            ATypeRegistry : TTypeRegistry
    );
  end;

  { TBaseObjectArrayRemotable_TypeHandler }

  TBaseArrayRemotable_TypeHandler = class(TSimpleFactoryItem,IWsdlTypeHandler)
  protected
    procedure Generate(
      const APascalTypeName : string;
            AWsdlDocument   : TXMLDocument;
            ATypeRegistry : TTypeRegistry
    );
  end;

  { TRecord_TypeHandler }

  TRecord_TypeHandler = class(TSimpleFactoryItem,IWsdlTypeHandler)
  protected
    procedure Generate(
      const APascalTypeName : string;
            AWsdlDocument   : TXMLDocument;
            ATypeRegistry : TTypeRegistry
    );
  end;
  
  
  procedure GenerateWSDL(
    AMdtdRep : PServiceRepository;
    ADoc : TXMLDocument;
    ATypeRegistry : TTypeRegistry;
    AHandlerRegistry : IWsdlTypeHandlerRegistry
  );overload;
  procedure GenerateWSDL(AMdtdRep : PServiceRepository; ADoc : TXMLDocument);overload;
  function GenerateWSDL(const ARepName, ARootAddress : string):string;overload;

  function GetWsdlTypeHandlerRegistry():IWsdlTypeHandlerRegistry;
  function CreateWsdlTypeHandlerRegistry(ATypeRegistry : TTypeRegistry):IWsdlTypeHandlerRegistry;
  procedure RegisterFondamentalTypesHandler(ARegistry : IWsdlTypeHandlerRegistry);
  
implementation
uses
  wst_types
{$IFDEF WST_DELPHI}
  , wst_delphi_rtti_utils
{$ENDIF}
{$IFDEF FPC}
  , wst_fpc_xml, XmlWrite
{$ENDIF}
  , record_rtti;

const
  sWSDL_NS       = 'http://schemas.xmlsoap.org/wsdl/';
  sSOAP_NS       = 'http://schemas.xmlsoap.org/wsdl/soap/';
  sSOAP          = 'soap';
  sXMLNS         = 'xmlns';
  sXSD_NS        = 'http://www.w3.org/2001/XMLSchema';
  sXSD           = 'xsd';
  sTNS           = 'tns';

  sSOAP_ACTION      = 'soapAction';
  sSOAP_ENCODING_STYLE = 'encodingStyle';
  sSOAP_RPC         = 'rpc';
  sSOAP_TRANSPORT   = 'http://schemas.xmlsoap.org/soap/http';
  sSOAP_USE         = 'use';
  
  sADDRESS            = 'address';
  sATTRIBUTE          = 'attribute';
  sBASE               = 'base';
  sBINDING            = 'binding';
  sBODY               = 'body';
  sCOMPLEX_CONTENT    = 'complexContent';
  sCOMPLEX_TYPE       = 'complexType';
  sELEMENT            = 'element';
  sENUMERATION        = 'enumeration';
  sEXTENSION          = 'extension';
  sITEM               = 'item';
  sLOCATION           = 'location';
  sMIN_OCCURS         = 'minOccurs';
  sMAX_OCCURS         = 'maxOccurs';
  sNAME               = 'name';
  sNAME_SPACE         = 'namespace';
  sPORT_TYPE          = 'portType';
  sRESTRICTION        = 'restriction';
  sSEQUENCE           = 'sequence';
  sSERVICE           = 'service';
  sSIMPLE_TYPE        = 'simpleType';
  sSTYLE              = 'style';
  sTRANSPORT          = 'transport';
  sTRUE_LOWERCASE     = 'true';
  sTYPE               = 'type';
  sUNBOUNDED          = 'unbounded';
  sUSE                = 'use';
  sVALUE              = 'value';

  sWSDL_DEFINITIONS        = 'definitions';
  sWSDL_INPUT              = 'input';
  sWSDL_MESSAGE            = 'message';
  sWSDL_NAME               = 'name';
  sWSDL_OPERATION          = 'operation';
  sWSDL_OUTPUT             = 'output';
  sWSDL_PART               = 'part';
  sWSDL_PORT               = 'port';
  sWSDL_PORT_TYPE          = sPORT_TYPE;
  sWSDL_SCHEMA             = 'schema';
  sWSDL_TARGET_NS          = 'targetNamespace';
  sWSDL_TYPE               = sTYPE;
  sWSDL_TYPES              = 'types';

  sWST_HEADER_BLOCK        = 'wst_headerBlock';
  
  sFORMAT_Input_EncodingStyle = 'FORMAT_Input_EncodingStyle';
  sFORMAT_Input_EncodingStyleURI = 'FORMAT_Input_EncodingStyleURI';

  sFORM_attributeFormDefault = 'attributeFormDefault';
  sFORM_elementFormDefault   = 'elementFormDefault';

var
  WsdlTypeHandlerRegistryInst : IWsdlTypeHandlerRegistry;
  
type

  { TWsdlTypeHandlerRegistry }

  TWsdlTypeHandlerRegistry = class(TBaseFactoryRegistry,IWsdlTypeHandlerRegistry)
  private
    FTypeRegistry : TTypeRegistry;
    FDefaultHandlerTable : Array[TTypeKind] of IItemFactory;
  private
    function FindNearestClass(const AClassType : TClass):IItemFactory;
  protected
    function Find(const APascalTypeName : string):IWsdlTypeHandler;
    procedure RegisterDefaultHandler(
      const ATypeKind : TTypeKind;
            AFactory  : IItemFactory
    );
  public
    constructor Create(ATypeRegistry : TTypeRegistry);
    destructor Destroy();override;
  End;

{ TWsdlTypeHandlerRegistry }

function DistanceFromChildToParent(AChildClass,AParentClass : TClass):Integer;
var
  ch : TClass;
begin
  if Assigned(AChildClass) and Assigned(AParentClass) then begin
    Result := 0;
    ch := AChildClass;
    while Assigned(ch) do begin
      if ( ch = AParentClass ) then
        Exit;
      Inc(Result);
      ch := ch.ClassParent;
    end;
  end;
  Result := MaxInt;
end;

function TWsdlTypeHandlerRegistry.FindNearestClass(const AClassType : TClass):IItemFactory;
var
  i,c, foundIndex,tmpScore, score : Integer;
  itm : TBaseFactoryRegistryItem;
  typData : PTypeData;
  r : TTypeRegistry;
  ri : TTypeRegistryItem;
begin
  Result := nil;
  foundIndex := -1;
  score := MaxInt;
  r := FTypeRegistry;
  c := Count;
  for i := 0 to Pred(c) do begin
    itm := Item[i];
    ri := r.Find(itm.Name);
    if Assigned(ri) and ( ri.DataType^.Kind = tkClass ) then begin
      typData := GetTypeData(ri.DataType);
      tmpScore := DistanceFromChildToParent(AClassType,typData^.ClassType);
      if ( tmpScore < score ) then begin
        foundIndex := i;
        score := tmpScore;
      end;
    end;
  end;
  if ( foundIndex >= 0 ) then begin
    Result := Item[foundIndex].Factory;
  end;
end;

function TWsdlTypeHandlerRegistry.Find(const APascalTypeName: string): IWsdlTypeHandler;
Var
  fct : IItemFactory;
  ri : TTypeRegistryItem;
begin
  Result := nil;
  fct := FindFactory(APascalTypeName);
  if not Assigned(fct) then begin
    ri := FTypeRegistry.Find(APascalTypeName);
    if Assigned(ri) then begin
      if ( ri.DataType^.Kind = tkClass ) then
        fct := FindNearestClass(GetTypeData(ri.DataType)^.ClassType);
      if not Assigned(fct) then
        fct := FDefaultHandlerTable[ri.DataType^.Kind];
    end;
  end;
  if Assigned(fct) then
    Result := fct.CreateInstance() as IWsdlTypeHandler;
end;

procedure TWsdlTypeHandlerRegistry.RegisterDefaultHandler(
  const ATypeKind: TTypeKind;
  AFactory: IItemFactory
);
begin
  FDefaultHandlerTable[ATypeKind] := AFactory;
end;

constructor TWsdlTypeHandlerRegistry.Create(ATypeRegistry : TTypeRegistry);
begin
  Assert(ATypeRegistry <> nil);
  inherited Create();
  FTypeRegistry := ATypeRegistry;
end;

destructor TWsdlTypeHandlerRegistry.Destroy();
var
  i : TTypeKind;
begin
  for i := Low(TTypeKind) to High(TTypeKind) do
    FDefaultHandlerTable[i] := nil;
  inherited Destroy();
end;

function CreateElement(const ANodeName : DOMString; AParent : TDOMNode; ADoc : TXMLDocument):TDOMElement;//inline;
begin
  Result := ADoc.CreateElement(ANodeName);
  AParent.AppendChild(Result);
end;

function FindAttributeByValueInNode(
  const AAttValue        : string;
  const ANode            : TDOMNode;
  out   AResAtt          : string;
  const AStartIndex      : Integer = 0;
  const AStartingWith    : string = ''
):boolean;
var
  i,c : Integer;
  b : Boolean;
begin
  AResAtt := '';
  if Assigned(ANode) and Assigned(ANode.Attributes) then begin
    b := ( Length(AStartingWith) = 0);
    c := Pred(ANode.Attributes.Length);
    if ( AStartIndex >= 0 ) then
      i := AStartIndex
    else
      i := 0;
    for i := i to c do begin
      if AnsiSameText(AAttValue,ANode.Attributes.Item[i].NodeValue) and
         ( b or ( Pos(AStartingWith,ANode.Attributes.Item[i].NodeName) = 1 ))
      then begin
        AResAtt := ANode.Attributes.Item[i].NodeName;
        Result := True;
        Exit;
      end;
    end;
  end;
  Result := False;
end;

function GetNameSpaceShortName(
  const ANameSpace    : string;
        ANode : TDOMElement;
  const APreferedShortName : string = ''
) : string; overload;
begin
  if FindAttributeByValueInNode(ANameSpace,ANode,Result,0,sXMLNS) then begin
    Result := Copy(Result,Length(sXMLNS+':')+1,MaxInt);
  end else begin
    Result := Trim(APreferedShortName);
    if ( Length(Result) = 0 ) then
      Result := Format('ns%d',[GetNodeListCount(ANode.Attributes)]) ;
    ANode.SetAttribute(Format('%s:%s',[sXMLNS,Result]),ANameSpace);
  end;
end;

function GetNameSpaceShortName(
  const ANameSpace    : string;
        AWsdlDocument : TXMLDocument;
  const APreferedShortName : string = ''
) : string; overload;
begin
  Result := GetNameSpaceShortName(ANameSpace,AWsdlDocument.DocumentElement,APreferedShortName);
end;

type TServiceElementType = ( setPortType, setBinding, setPort, setService,setAddress );
function GetServicePartName(AService : PService; const AServicePart : TServiceElementType):string;
const PART_NAME_MAP : array[TServiceElementType] of shortstring = ('', 'Binding', 'Port', '','');
begin
  Result := AService^.Name + PART_NAME_MAP[AServicePart];
end;

procedure GenerateWSDL(AMdtdRep : PServiceRepository; ADoc : TXMLDocument);
begin
  GenerateWSDL(AMdtdRep,ADoc,GetTypeRegistry(),GetWsdlTypeHandlerRegistry());
end;

procedure GenerateWSDL(
  AMdtdRep : PServiceRepository;
  ADoc : TXMLDocument;
  ATypeRegistry : TTypeRegistry;
  AHandlerRegistry : IWsdlTypeHandlerRegistry
);
  procedure GenerateServiceMessages(
          AService   : PService;
          ARootNode  : TDOMElement
  );
  
    procedure GenerateOperationMessage(AOperation : PServiceOperation);
    
      procedure GenerateParam(APrm : POperationParam; AMsgNode : TDOMElement);
      var
        tmpNode : TDOMElement;
        typItm : TTypeRegistryItem;
        ns_shortName, s : string;
      begin
        tmpNode := CreateElement(sWSDL_PART,AMsgNode,ADoc);
        tmpNode.SetAttribute(sWSDL_NAME,APrm^.Name);
        typItm := ATypeRegistry.Find(APrm^.TypeName);
        if not Assigned(typItm) then
          raise EMetadataException.CreateFmt('Type not registered : "%s".',[APrm^.TypeName]);
        //Assert(Assigned(typItm),APrm^.TypeName);
        ns_shortName := GetNameSpaceShortName(typItm.NameSpace,ADoc);
        s := Format('%s:%s',[ns_shortName,typItm.DeclaredName]);
        tmpNode.SetAttribute(sWSDL_TYPE,s);
      end;
      
    var
      qryNode, rspNode : TDOMElement;
      ii, cc : Integer;
      pp : POperationParam;
    begin
      qryNode := CreateElement(sWSDL_MESSAGE,ARootNode,ADoc);
      qryNode.SetAttribute(sWSDL_NAME,Format('%s',[AOperation^.Name]));
      rspNode := CreateElement(sWSDL_MESSAGE,ARootNode,ADoc);
      rspNode.SetAttribute(sWSDL_NAME,Format('%sResponse',[AOperation^.Name]));
      cc := AOperation^.ParamsCount;
      if ( cc > 0 ) then begin
        pp := AOperation^.Params;
        for ii := 0 to Pred(cc) do begin
          if ( pp^.Modifier in [opfNone, opfIn] ) then
            GenerateParam(pp,qryNode)
          else if ( pp^.Modifier in [opfVar, opfOut] ) then
            GenerateParam(pp,rspNode);
          Inc(pp);
        end;
      end;
    end;
    
  Var
    j, k : Integer;
    po : PServiceOperation;
  begin
    k := AService^.OperationsCount;
    if ( k > 0 ) then begin
      po := AService^.Operations;
      for j := 0 to pred(k) do begin
        GenerateOperationMessage(po);
        Inc(po);
      end;
    end;
  end;

  procedure GenerateServicePortType(AService : PService; ARootNode : TDOMElement);

    procedure GenerateOperation(AOperation : PServiceOperation; APrtTypeNode : TDOMElement);
    var
      opNode, inNode, outNode : TDOMElement;
    begin
      opNode := CreateElement(sWSDL_OPERATION,APrtTypeNode,ADoc);
      opNode.SetAttribute(sWSDL_NAME,AOperation^.Name);
      inNode := CreateElement(sWSDL_INPUT,opNode,ADoc);
      inNode.SetAttribute(sWSDL_MESSAGE,Format('%s:%s',[sTNS,AOperation^.Name]));
      outNode := CreateElement(sWSDL_OUTPUT,opNode,ADoc);
      outNode.SetAttribute(sWSDL_MESSAGE,Format('%s:%sResponse',[sTNS,AOperation^.Name]));
    end;

  var
    prtTypeNode : TDOMElement;
    j, k : Integer;
    po : PServiceOperation;
  begin
    prtTypeNode := CreateElement(sWSDL_PORT_TYPE,ARootNode,ADoc);
    prtTypeNode.SetAttribute(sWSDL_NAME,GetServicePartName(AService,setPortType));
    k := AService^.OperationsCount;
    if ( k > 0 ) then begin
      po := AService^.Operations;
      for j := 0 to pred(k) do begin
        GenerateOperation(po,prtTypeNode);
        Inc(po);
      end;
    end;
  end;

  procedure GenerateServiceBinding(AService : PService; ARootNode : TDOMElement);

    procedure GenerateOperation(AOperation : PServiceOperation; ABndngNode : TDOMElement);
    var
      opNode, inNode, outNode, bdyNode : TDOMElement;
      strBuff : string;
      propData : PPropertyData;
      encdStyl,encdStylURI : string;
    begin
      strBuff := Format('%s:%s',[sSOAP,sWSDL_OPERATION]);
      //CreateElement(strBuff,ABndngNode,ADoc).SetAttribute(sSOAP_ACTION,Format('%s/%s%s',[AMdtdRep^.NameSpace,AService^.Name,AOperation^.Name]));
      opNode := CreateElement(sWSDL_OPERATION,ABndngNode,ADoc);
      opNode.SetAttribute(sWSDL_NAME,AOperation^.Name);
        CreateElement(strBuff,opNode,ADoc).SetAttribute(sSOAP_ACTION,Format('%s/%s%s',[AMdtdRep^.NameSpace,AService^.Name,AOperation^.Name]));
        inNode := CreateElement(sWSDL_INPUT,opNode,ADoc);
          strBuff := Format('%s:%s',[sSOAP,sBODY]);
          bdyNode := CreateElement(strBuff,inNode,ADoc);
          encdStyl := 'literal';
          encdStylURI := '';
          propData := Find(AOperation^.Properties,sFORMAT_Input_EncodingStyle);
          if Assigned(propData) and ( Length(Trim(propData^.Data)) > 0 ) then begin
            encdStyl := Trim(propData^.Data);
          end;
          bdyNode.SetAttribute(sSOAP_USE,encdStyl);
          bdyNode.SetAttribute(sNAME_SPACE,Format('%s',[AMdtdRep^.NameSpace]));
          propData := Find(AOperation^.Properties,sFORMAT_Input_EncodingStyleURI);
          if Assigned(propData) and ( Length(Trim(propData^.Data)) > 0 ) then begin
            encdStylURI := Trim(propData^.Data);
          end;
          if ( Length(encdStylURI) > 0 ) then
            bdyNode.SetAttribute(sSOAP_ENCODING_STYLE,encdStylURI);

        outNode := CreateElement(sWSDL_OUTPUT,opNode,ADoc);
          strBuff := Format('%s:%s',[sSOAP,sBODY]);
          bdyNode := CreateElement(strBuff,outNode,ADoc);
          bdyNode.SetAttribute(sSOAP_USE,encdStyl);
          bdyNode.SetAttribute(sNAME_SPACE,Format('%s',[AMdtdRep^.NameSpace]));
          if ( Length(encdStylURI) > 0 ) then
            bdyNode.SetAttribute(sSOAP_ENCODING_STYLE,encdStylURI);
    end;

  var
    bndgNode, soapbndgNode : TDOMElement;
    j, k : Integer;
    po : PServiceOperation;
    strBuf : string;
  begin
    bndgNode := CreateElement(sBINDING,ARootNode,ADoc);
    bndgNode.SetAttribute(sWSDL_NAME,GetServicePartName(AService,setBinding));
    bndgNode.SetAttribute(sWSDL_TYPE,Format('%s:%s',[sTNS,GetServicePartName(AService,setPortType)]));

    strBuf := Format('%s:%s',[sSOAP,sBINDING]);
    soapbndgNode := CreateElement(strBuf,bndgNode,ADoc);
    soapbndgNode.SetAttribute(sSTYLE,sSOAP_RPC);
    soapbndgNode.SetAttribute(sTRANSPORT,sSOAP_TRANSPORT);

    k := AService^.OperationsCount;
    if ( k > 0 ) then begin
      po := AService^.Operations;
      for j := 0 to pred(k) do begin
        GenerateOperation(po,bndgNode);
        Inc(po);
      end;
    end;
  end;

  procedure GenerateServicePublication(AService : PService; ARootNode : TDOMElement);
  var
    srvcNode, portNode, soapAdrNode : TDOMElement;
    strBuf : string;
  begin
    srvcNode := CreateElement(sSERVICE,ARootNode,ADoc);
    srvcNode.SetAttribute(sWSDL_NAME,GetServicePartName(AService,setService));

    strBuf := Format('%s',[sWSDL_PORT]);
    portNode := CreateElement(strBuf,srvcNode,ADoc);
    portNode.SetAttribute(sWSDL_NAME,GetServicePartName(AService,setPort));
    portNode.SetAttribute(sBINDING,Format('%s:%s',[sTNS,GetServicePartName(AService,setBinding)]));

    strBuf := Format('%s:%s',[sSOAP,sADDRESS]);
    soapAdrNode := CreateElement(strBuf,portNode,ADoc);
    soapAdrNode.SetAttribute(sLOCATION,Format('%s%s',[AMdtdRep^.RootAddress,GetServicePartName(AService,setAddress)]));
  end;
  
  procedure GenerateServiceTypes();
  var
    j, k : Integer;
    tr : TTypeRegistry;
    tri : TTypeRegistryItem;
    g : IWsdlTypeHandler;
    gr : IWsdlTypeHandlerRegistry;
  begin
    tr := ATypeRegistry;
    gr := AHandlerRegistry;
    k := tr.Count;
    for j := 0 to Pred(k) do begin
      tri := tr[j];
      if ( not ( trioNonVisibleToMetadataService in tri.Options ) ) and
         AnsiSameText(AMdtdRep^.NameSpace,tri.NameSpace)
      then begin
        g := gr.Find(tri.DataType^.Name);
        if assigned(g) then
          g.Generate(tri.DataType^.Name,ADoc,tr);
      end;
    end;
  end;

  function CreateRootNode():TDOMElement;
  begin
    Result := CreateElement(sWSDL_DEFINITIONS,ADoc,ADoc);
    Result.SetAttribute(sWSDL_NAME,AMdtdRep^.Name);
    
    Result.SetAttribute(sWSDL_TARGET_NS,AMdtdRep^.NameSpace);
    Result.SetAttribute(Format('%s:%s',[sXMLNS,sSOAP]),sSOAP_NS);
    Result.SetAttribute(Format('%s:%s',[sXMLNS,sXSD]),sXSD_NS);
    Result.SetAttribute(Format('%s:%s',[sXMLNS,sTNS]),AMdtdRep^.NameSpace);
    Result.SetAttribute(sXMLNS,sWSDL_NS);
  end;
  
  function CreateTypesRootNode(ARootNode :  TDOMNode):TDOMElement;
  begin
    Result := CreateElement(sWSDL_TYPES,ARootNode,ADoc);
    //Result.SetAttribute(sWSDL_TARGET_NS,AMdtdRep^.NameSpace);
  end;
  
var
  defNode, typesNode, schNode : TDOMElement;
  i, c : Integer;
  ps : PService;
  propData : PPropertyData;
begin
  if not ( Assigned(AMdtdRep) and Assigned(ADoc)) then
    Exit;

  defNode := CreateRootNode();
  typesNode := CreateTypesRootNode(defNode);
  schNode := CreateElement(sXSD + ':' + sWSDL_SCHEMA,typesNode,ADoc);
  schNode.SetAttribute(sXMLNS,sXSD_NS);
  schNode.SetAttribute(sWSDL_TARGET_NS,AMdtdRep^.NameSpace);
  propData := Find(AMdtdRep^.Properties,sFORM_elementFormDefault);
  if (propData <> nil) and (propData^.Data <> '') then
    schNode.SetAttribute(sFORM_elementFormDefault,Trim(propData^.Data));
  propData := Find(AMdtdRep^.Properties,sFORM_attributeFormDefault);
  if (propData <> nil) and (propData^.Data <> '') then
    schNode.SetAttribute(sFORM_attributeFormDefault,Trim(propData^.Data));

  GenerateServiceTypes();

  c := AMdtdRep^.ServicesCount;
  if ( c > 0 ) then begin
    ps := AMdtdRep^.Services;
    for i := 0 to Pred(c) do begin
      GenerateServiceMessages(ps,defNode);
      Inc(ps);
    end;
    ps := AMdtdRep^.Services;
    for i := 0 to Pred(c) do begin
      GenerateServicePortType(ps,defNode);
      Inc(ps);
    end;
    ps := AMdtdRep^.Services;
    for i := 0 to Pred(c) do begin
      GenerateServiceBinding(ps,defNode);
      Inc(ps);
    end;
    ps := AMdtdRep^.Services;
    for i := 0 to Pred(c) do begin
      GenerateServicePublication(ps,defNode);
      Inc(ps);
    end;
  end;
  
end;

function GenerateWSDL(const ARepName, ARootAddress : string):string;overload;
var
  strm : TMemoryStream;
  rep : PServiceRepository;
  doc :TXMLDocument;
  i : SizeInt;
  s : string;
begin
  Result := '';
  rep := nil;
  doc := Nil;
  i := GetModuleMetadataMngr().IndexOfName(ARepName);
  if ( i < 0 ) then
    Exit;
  strm := TMemoryStream.Create();
  try
    s := GetModuleMetadataMngr().GetRepositoryName(i);
    GetModuleMetadataMngr().LoadRepositoryName(s,ARootAddress,rep);
    strm.Clear();
    doc := CreateDoc();
    GenerateWSDL(rep,doc);
    WriteXMLFile(doc,strm);
    i := strm.Size;
    if ( i > 0 ) then begin
      SetLength(Result,i);
      Move(strm.memory^,Result[1],i);
    end;
  finally
    ReleaseDomNode(doc);
    strm.Free();
    GetModuleMetadataMngr().ClearRepository(rep);
  end;
end;

function GetWsdlTypeHandlerRegistry():IWsdlTypeHandlerRegistry;
begin
  Result := WsdlTypeHandlerRegistryInst;
end;

type

  { TFakeTypeHandler }

  TFakeTypeHandler = class(TSimpleFactoryItem,IWsdlTypeHandler)
  protected
    procedure Generate(
      const APascalTypeName : string;
            AWsdlDocument   : TXMLDocument;
            ATypeRegistry : TTypeRegistry
    );
  end;

{ TBaseComplexRemotable_TypeHandler }

procedure TBaseComplexRemotable_TypeHandler.Generate(
  const APascalTypeName : string;
        AWsdlDocument   : TXMLDocument;
        ATypeRegistry : TTypeRegistry
);

  function FindHighestRegisteredParent(AClass : TClass) : TTypeRegistryItem;
  var
    locRes : TTypeRegistryItem;
  begin
    locRes := nil;
    if ( AClass <> nil ) and ( AClass.ClassParent <> nil ) then begin
      locRes := ATypeRegistry.Find(PTypeInfo(AClass.ClassParent.ClassInfo),False);
    end;
    {if ( locRes <> nil ) then begin
      if ( locRes.NameSpace = sSOAP_ENV ) or
         ( GetTypeData(locRes.DataType)^.ClassType = TBaseComplexRemotable )
      then
        locRes := nil;
    end;}
    Result := locRes;
  end;

  function IsParentVisible(const AParentRegItem : TTypeRegistryItem) : Boolean;
  begin
    Result :=
      ( AParentRegItem <> nil ) and
      ( ( AParentRegItem.NameSpace <> sSOAP_ENV ) and
        ( GetTypeData(AParentRegItem.DataType)^.ClassType <> TBaseComplexRemotable )
      )
  end;

  function IsInheritedPropertyAlreadyPublished(
    const AProperty    : PPropInfo;
    const AParentClass : TClass
  ) : Boolean;
  begin
    Result :=
      ( AParentClass = THeaderBlock ) and
      ( SameText('mustUnderstand',AProperty^.Name) );
  end;

var
  typItm, propTypItm : TTypeRegistryItem;
  s, prop_ns_shortName : string;
  defTypesNode, defSchemaNode, cplxNode, sqcNode, propNode, cplxContentNode, extNode : TDOMElement;
  i : Integer;
  propList : PPropList;
  propCount, propListLen : Integer;
  p : PPropInfo;
  persistType : TPropStoreType;
  objTypeData : PTypeData;
  clsTyp : TBaseComplexRemotableClass;
  attProp : Boolean;
  parentRegItem : TTypeRegistryItem;
  parentClss : TClass;
begin
  typItm := ATypeRegistry.Find(APascalTypeName);
  if Assigned(typItm) and
     ( typItm.DataType^.Kind = tkClass )
  then begin
    GetNameSpaceShortName(typItm.NameSpace,AWsdlDocument);
    defTypesNode :=  FindNode(AWsdlDocument.DocumentElement,sWSDL_TYPES) as TDOMElement;
    Assert(Assigned(defTypesNode));
    defSchemaNode := defTypesNode.FirstChild as TDOMElement;
    
    objTypeData := GetTypeData(typItm.DataType);
    clsTyp := TBaseComplexRemotableClass(objTypeData^.ClassType);
    parentRegItem := FindHighestRegisteredParent(clsTyp);
    if ( parentRegItem <> nil ) then
      parentClss := GetTypeData(parentRegItem.DataType)^.ClassType
    else
      parentClss := nil;

    s := Format('%s:%s',[sXSD,sCOMPLEX_TYPE]);
    cplxNode := CreateElement(s,defSchemaNode,AWsdlDocument);
    cplxNode.SetAttribute(sNAME, typItm.DeclaredName);
    extNode := cplxNode;
    if ( parentClss <> nil ) then begin
      if ( parentClss = THeaderBlock ) then begin
        s := Format('%s:%s',[GetNameSpaceShortName(sWST_BASE_NS,defSchemaNode,sWST_BASE_NS_ABR),sWST_HEADER_BLOCK]);
        cplxNode.SetAttribute(s, sTRUE_LOWERCASE);
      end;
      if IsParentVisible(parentRegItem) then begin
        if ( parentRegItem.NameSpace = typItm.NameSpace ) then begin
          s := Format('%s:%s',[sTNS,parentRegItem.DeclaredName]);
        end else begin
          s := Format('%s:%s',[GetNameSpaceShortName(parentRegItem.NameSpace,AWsdlDocument),parentRegItem.DeclaredName]);
        end;
        cplxContentNode := CreateElement(Format('%s:%s',[sXSD,sCOMPLEX_CONTENT]),cplxNode,AWsdlDocument);
        extNode := CreateElement(Format('%s:%s',[sXSD,sEXTENSION]),cplxContentNode,AWsdlDocument);
        extNode.SetAttribute(sBASE,s);
      end;
    end;

      sqcNode := CreateElement(Format('%s:%s',[sXSD,sSEQUENCE]),extNode,AWsdlDocument);
      propCount := objTypeData^.PropCount;
      if ( propCount > 0 ) then begin
        propListLen := GetPropList(typItm.DataType,propList);
        try
          for i := 0 to Pred(propCount) do begin
            p := propList^[i];
            if ( parentClss = nil ) or
               ( not ( IsPublishedProp(parentClss,p^.Name) or IsInheritedPropertyAlreadyPublished(p,parentClss) )

               )
            then begin
              persistType := IsStoredPropClass(objTypeData^.ClassType,p);
              if ( persistType in [pstOptional,pstAlways] ) then begin
                attProp := clsTyp.IsAttributeProperty(p^.Name);
                if attProp then begin
                  s := Format('%s:%s',[sXSD,sATTRIBUTE]);
                  propNode := CreateElement(s,extNode,AWsdlDocument)
                end else begin
                  s := Format('%s:%s',[sXSD,sELEMENT]);
                  propNode := CreateElement(s,sqcNode,AWsdlDocument);
                end;
                propNode.SetAttribute(sNAME,typItm.GetExternalPropertyName(p^.Name));
                propTypItm := ATypeRegistry.Find(p^.PropType^.Name);
                if Assigned(propTypItm) then begin
                  prop_ns_shortName := GetNameSpaceShortName(propTypItm.NameSpace,AWsdlDocument);
                  propNode.SetAttribute(sTYPE,Format('%s:%s',[prop_ns_shortName,propTypItm.DeclaredName]));
                  if attProp then begin
                    if ( persistType = pstOptional ) then
                      propNode.SetAttribute(sUSE,'optional')
                    else
                      propNode.SetAttribute(sUSE,'required');
                  end else begin
                    if ( persistType = pstOptional ) then
                      propNode.SetAttribute(sMIN_OCCURS,'0')
                    else
                      propNode.SetAttribute(sMIN_OCCURS,'1');
                    propNode.SetAttribute(sMAX_OCCURS,'1');
                  end;
                end;
              end;
            end;
          end;
        finally
          Freemem(propList,propListLen*SizeOf(Pointer));
        end;
      end;
  end;
end;
  
{ TEnumTypeHandler }

procedure TEnumTypeHandler.Generate(
  const APascalTypeName: string;
        AWsdlDocument: TXMLDocument;
        ATypeRegistry : TTypeRegistry
);
var
  typItm : TTypeRegistryItem;
  ns_shortName, s : string;
  defTypesNode, defSchemaNode, resNode, restrictNode : TDOMElement;
  i, c : Integer;
begin
  typItm := ATypeRegistry.Find(APascalTypeName);
  if Assigned(typItm) and
     ( typItm.DataType^.Kind = tkEnumeration )
  then begin
    if FindAttributeByValueInNode(typItm.NameSpace,AWsdlDocument.DocumentElement,ns_shortName) then begin
      ns_shortName := Copy(ns_shortName,Length(sXMLNS+':')+1,MaxInt);
    end else begin
      ns_shortName := Format('ns%d',[GetNodeListCount(AWsdlDocument.DocumentElement.Attributes)]) ;
      AWsdlDocument.DocumentElement.SetAttribute(Format('%s:%s',[sXMLNS,ns_shortName]),typItm.NameSpace);
    end;
    defTypesNode := FindNode(AWsdlDocument.DocumentElement,sWSDL_TYPES) as TDOMElement;
    Assert(Assigned(defTypesNode));
    defSchemaNode := defTypesNode.FirstChild as TDOMElement;

    //s := Format('%s:%s',[sXSD,sELEMENT]);
    //eltNode := CreateElement(s,defSchemaNode,AWsdlDocument);
    //eltNode.SetAttribute(sNAME, typItm.DeclaredName) ;
    s := Format('%s:%s',[sXSD,sSIMPLE_TYPE]);
    resNode := CreateElement(s,defSchemaNode,AWsdlDocument);
    resNode.SetAttribute(sNAME, typItm.DeclaredName) ;
      s := Format('%s:%s',[sXSD,sRESTRICTION]);
      restrictNode := CreateElement(s,resNode,AWsdlDocument);
      restrictNode.SetAttribute(sBASE,Format('%s:%s',[sXSD,'string'])) ;
      c := GetEnumNameCount(typItm.DataType);
      for i := 0 to pred(c) do begin
        s := Format('%s:%s',[sXSD,sENUMERATION]);
        //CreateElement(s,restrictNode,AWsdlDocument).SetAttribute(sVALUE,GetEnumName(typItm.DataType,i));
        CreateElement(s,restrictNode,AWsdlDocument).SetAttribute(
          sVALUE,
          typItm.GetExternalPropertyName(GetEnumName(typItm.DataType,i))
        );
      end;
  end;
end;
  

{ TFakeTypeHandler }

procedure TFakeTypeHandler.Generate(
  const APascalTypeName: string;
        AWsdlDocument: TXMLDocument;
        ATypeRegistry : TTypeRegistry
);
begin
end;

function CreateWsdlTypeHandlerRegistry(ATypeRegistry : TTypeRegistry):IWsdlTypeHandlerRegistry;
begin
  Result := TWsdlTypeHandlerRegistry.Create(ATypeRegistry);
end;

procedure RegisterFondamentalTypesHandler(ARegistry : IWsdlTypeHandlerRegistry);
var
  r : IWsdlTypeHandlerRegistry;
begin
  r := ARegistry;
  r.RegisterDefaultHandler(tkInteger,TSimpleItemFactory.Create(TFakeTypeHandler));
  r.RegisterDefaultHandler(tkInt64,TSimpleItemFactory.Create(TFakeTypeHandler));

{$IFDEF FPC}
  r.RegisterDefaultHandler(tkQWord,TSimpleItemFactory.Create(TFakeTypeHandler));
  r.RegisterDefaultHandler(tkSString,TSimpleItemFactory.Create(TFakeTypeHandler));
  r.RegisterDefaultHandler(tkAString,TSimpleItemFactory.Create(TFakeTypeHandler));
  r.RegisterDefaultHandler(tkBool,TSimpleItemFactory.Create(TFakeTypeHandler));
{$ENDIF}

  r.RegisterDefaultHandler(tkLString,TSimpleItemFactory.Create(TFakeTypeHandler));
  r.RegisterDefaultHandler(tkWString,TSimpleItemFactory.Create(TFakeTypeHandler));

  r.RegisterDefaultHandler(tkWString,TSimpleItemFactory.Create(TFakeTypeHandler));

  r.RegisterDefaultHandler(tkEnumeration,TSimpleItemFactory.Create(TEnumTypeHandler));

  r.RegisterDefaultHandler(tkClass,TSimpleItemFactory.Create(TBaseComplexRemotable_TypeHandler));

  r.Register('TBaseArrayRemotable',TSimpleItemFactory.Create(TBaseArrayRemotable_TypeHandler));
  
  r.RegisterDefaultHandler(tkRecord,TSimpleItemFactory.Create(TRecord_TypeHandler));

{  r.Register('Integer',TSimpleItemFactory.Create(TFakeTypeHandler));
  r.Register('LongWord',TSimpleItemFactory.Create(TFakeTypeHandler));

  r.Register('string',TSimpleItemFactory.Create(TFakeTypeHandler));
  r.Register('shortstring',TSimpleItemFactory.Create(TFakeTypeHandler));
  r.Register('ansistring',TSimpleItemFactory.Create(TFakeTypeHandler));
  r.Register('boolean',TSimpleItemFactory.Create(TFakeTypeHandler));

  r.Register('Byte',TSimpleItemFactory.Create(TFakeTypeHandler));
  r.Register('ShortInt',TSimpleItemFactory.Create(TFakeTypeHandler));
  r.Register('Word',TSimpleItemFactory.Create(TFakeTypeHandler));
  r.Register('SmallInt',TSimpleItemFactory.Create(TFakeTypeHandler));
  r.Register('Int64',TSimpleItemFactory.Create(TFakeTypeHandler));
  r.Register('QWord',TSimpleItemFactory.Create(TFakeTypeHandler));

  r.Register('Single',TSimpleItemFactory.Create(TFakeTypeHandler));
  r.Register('Currency',TSimpleItemFactory.Create(TFakeTypeHandler));
  r.Register('Comp',TSimpleItemFactory.Create(TFakeTypeHandler));
  r.Register('Double',TSimpleItemFactory.Create(TFakeTypeHandler));
  r.Register('Extended',TSimpleItemFactory.Create(TFakeTypeHandler));
}
end;


{ TBaseArrayRemotable_TypeHandler }

procedure TBaseArrayRemotable_TypeHandler.Generate(
  const APascalTypeName: string;
        AWsdlDocument: TXMLDocument;
        ATypeRegistry : TTypeRegistry
);
var
  typItm, propTypItm : TTypeRegistryItem;
  s, prop_ns_shortName : string;
  defTypesNode, defSchemaNode, cplxNode, sqcNode, propNode : TDOMElement;
  arrayTypeData : PTypeData;
  arrayTypeClass : TBaseArrayRemotableClass;
begin
  typItm := ATypeRegistry.Find(APascalTypeName);
  if not Assigned(typItm) then
    Exit;
  arrayTypeData := GetTypeData(typItm.DataType);
  if Assigned(typItm) and
     ( typItm.DataType^.Kind = tkClass ) and
     ( arrayTypeData^.ClassType.InheritsFrom(TBaseArrayRemotable) )
  then begin
    GetNameSpaceShortName(typItm.NameSpace,AWsdlDocument);
    defTypesNode := FindNode(AWsdlDocument.DocumentElement,sWSDL_TYPES) as TDOMElement;
    Assert(Assigned(defTypesNode));
    defSchemaNode := defTypesNode.FirstChild as TDOMElement;

    s := Format('%s:%s',[sXSD,sCOMPLEX_TYPE]);
    cplxNode := CreateElement(s,defSchemaNode,AWsdlDocument);
    cplxNode.SetAttribute(sNAME, typItm.DeclaredName) ;

      s := Format('%s:%s',[sXSD,sSEQUENCE]);
      sqcNode := CreateElement(s,cplxNode,AWsdlDocument);
      arrayTypeClass := TBaseArrayRemotableClass(arrayTypeData^.ClassType);
      propTypItm := ATypeRegistry.Find(arrayTypeClass.GetItemTypeInfo()^.Name);
      s := Format('%s:%s',[sXSD,sELEMENT]);
      propNode := CreateElement(s,sqcNode,AWsdlDocument);
      s := Trim(typItm.GetExternalPropertyName(sARRAY_ITEM));
      if ( s = '' ) then
        s := sITEM;
      propNode.SetAttribute(sNAME,s);
      if Assigned(propTypItm) then begin
        prop_ns_shortName := GetNameSpaceShortName(propTypItm.NameSpace,AWsdlDocument);
        propNode.SetAttribute(sTYPE,Format('%s:%s',[prop_ns_shortName,propTypItm.DeclaredName]));
        propNode.SetAttribute(sMIN_OCCURS,'0');
        propNode.SetAttribute(sMAX_OCCURS,sUNBOUNDED);
        if arrayTypeClass.InheritsFrom(TObjectCollectionRemotable) then begin
          propNode.SetAttribute(
            Format('%s:wst_collection',[GetNameSpaceShortName(sWST_BASE_NS,defSchemaNode,sWST_BASE_NS_ABR)]),
            sTRUE_LOWERCASE
          );
        end;
      end;
  end;
end;

{ TRecord_TypeHandler }

procedure TRecord_TypeHandler.Generate(
  const APascalTypeName : string;
        AWsdlDocument : TXMLDocument;
        ATypeRegistry : TTypeRegistry
);
var
  typItm, propTypItm : TTypeRegistryItem;
  s, prop_ns_shortName : string;
  defTypesNode, defSchemaNode, cplxNode, sqcNode, propNode : TDOMElement;
  i : PtrUInt;
  p : TRecordFieldInfo;
  objTypeData : PRecordTypeData;
  persistType : TPropStoreType;
begin
  typItm := ATypeRegistry.Find(APascalTypeName);
  if Assigned(typItm) and
     ( typItm.DataType^.Kind = tkRecord )
  then begin
    GetNameSpaceShortName(typItm.NameSpace,AWsdlDocument);
    defTypesNode :=  FindNode(AWsdlDocument.DocumentElement,sWSDL_TYPES) as TDOMElement;
    Assert(Assigned(defTypesNode));
    defSchemaNode := defTypesNode.FirstChild as TDOMElement;

    objTypeData := TRecordRttiDataObject(typItm.GetObject(FIELDS_STRING)).GetRecordTypeData();
    persistType := pstOptional;

    s := Format('%s:%s',[sXSD,sCOMPLEX_TYPE]);
    cplxNode := CreateElement(s,defSchemaNode,AWsdlDocument);
    cplxNode.SetAttribute(sNAME, typItm.DeclaredName);
    cplxNode.SetAttribute(Format('%s:wst_record',[GetNameSpaceShortName(sWST_BASE_NS,defSchemaNode,'wst')]),sTRUE_LOWERCASE);

      sqcNode := CreateElement(Format('%s:%s',[sXSD,sSEQUENCE]),cplxNode,AWsdlDocument);
      if ( objTypeData^.FieldCount > 0 ) then begin
        for i := 0 to Pred(objTypeData^.FieldCount) do begin
          p := objTypeData^.Fields[i];
          if p.Visible then begin
            if p.IsAttribute then begin
              s := Format('%s:%s',[sXSD,sATTRIBUTE]);
              propNode := CreateElement(s,cplxNode,AWsdlDocument)
            end else begin
              s := Format('%s:%s',[sXSD,sELEMENT]);
              propNode := CreateElement(s,sqcNode,AWsdlDocument);
            end;
            propNode.SetAttribute(sNAME,typItm.GetExternalPropertyName(p.Name));
            propTypItm := ATypeRegistry.Find(p.TypeInfo^^.Name);
            if Assigned(propTypItm) then begin
              prop_ns_shortName := GetNameSpaceShortName(propTypItm.NameSpace,AWsdlDocument);
              propNode.SetAttribute(sTYPE,Format('%s:%s',[prop_ns_shortName,propTypItm.DeclaredName]));
              if p.IsAttribute then begin
                if ( persistType = pstOptional ) then
                  propNode.SetAttribute(sUSE,'optional')
                else
                  propNode.SetAttribute(sUSE,'required');
              end else begin
                if ( persistType = pstOptional ) then
                  propNode.SetAttribute(sMIN_OCCURS,'0')
                else
                  propNode.SetAttribute(sMIN_OCCURS,'1');
                propNode.SetAttribute(sMAX_OCCURS,'1');
              end;
            end;
          end;
        end;
      end;
  end;
end;

initialization
  WsdlTypeHandlerRegistryInst := CreateWsdlTypeHandlerRegistry(GetTypeRegistry());
  RegisterFondamentalTypesHandler(WsdlTypeHandlerRegistryInst);

finalization
  WsdlTypeHandlerRegistryInst := nil;
  
end.
