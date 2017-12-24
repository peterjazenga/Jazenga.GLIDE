{
    This file is part of the Web Service Toolkit
    Copyright (c) 2006-2014 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$INCLUDE wst_global.inc}
unit wsdl_generator;

interface

uses
  Classes, SysUtils, TypInfo,
  {$IFNDEF FPC}xmldom, wst_delphi_xml{$ELSE}DOM{$ENDIF},
  pastree, pascal_parser_intf, xsd_generator, locators, logger_intf;

type

  EWsdlGeneratorException = class(EXsdGeneratorException) end;
  
  { TWsdlTypechemaGenerator }

  TWsdlTypechemaGenerator = class(TCustomXsdGenerator)
  private
    FSchemaNode : TDOMElement;
    FTypesNode : TDOMElement;
  protected
    function GetSchemaNode(ADocument : TDOMDocument) : TDOMNode;override;
    procedure Prepare(ASymTable : TwstPasTreeContainer; AModule : TPasModule);override;
  end;

  { TWsdlGenerator }

  TWsdlGenerator = class(TInterfacedObject, IInterface, IGenerator)
  private
    FDocument : TDOMDocument;
    FTypesNode : TDOMElement;
    FDefinitionsNode : TDOMElement;
    FDocumentLocator : IDocumentLocator;
    FMessageHandler : TOnLogMessageEvent;
  private
    procedure GenerateTypes(ASymTable : TwstPasTreeContainer; AModule : TPasModule);
    procedure GenerateServiceMessages(
      ASymTable : TwstPasTreeContainer;
      AModule   : TPasModule;
      AContract : TPasClassType;
      ARootNode : TDOMElement
    );
    procedure GenerateServicePortType(
      ASymTable : TwstPasTreeContainer;
      AModule   : TPasModule;
      AContract : TPasClassType;
      ARootNode : TDOMElement
    );
    procedure GenerateServiceBinding(
      ASymTable : TwstPasTreeContainer;
      AModule   : TPasModule;
      ABinding  : TwstBinding;
      ARootNode : TDOMElement
    );
    procedure GenerateServicePublication(
      ASymTable : TwstPasTreeContainer;
      AModule   : TPasModule;
      ABinding  : TwstBinding;
      ARootNode : TDOMElement
    );
  protected
    function GetNotificationHandler() : TOnLogMessageEvent;
    procedure SetNotificationHandler(const AValue : TOnLogMessageEvent);
    procedure Prepare(ASymTable : TwstPasTreeContainer; AModule : TPasModule);
    function GetDocumentLocator() : IDocumentLocator;
    procedure SetDocumentLocator(ALocator : IDocumentLocator);
    procedure Execute(
      ASymTable   : TwstPasTreeContainer;
      AModuleName : string
    );
    property Document : TDOMDocument read FDocument;
  public
    constructor Create(ADocument : TDOMDocument);
  end;
  
implementation
uses xsd_consts, wst_types;

{ TWsdlTypechemaGenerator }

function TWsdlTypechemaGenerator.GetSchemaNode(ADocument : TDOMDocument) : TDOMNode;
begin
  Result := FSchemaNode;
end;

procedure TWsdlTypechemaGenerator.Prepare(ASymTable : TwstPasTreeContainer; AModule : TPasModule);

  function FindNode(AParent : TDOMNode; const ANodeName : string) : TDOMNode;
  var
    nd : TDOMNode;
  begin
    Result := nil;
    nd := AParent.FirstChild;
    while Assigned(nd) do begin
      if AnsiSameText(ANodeName,nd.NodeName) then begin
        Result := nd;
        Break;
      end;
      nd := nd.NextSibling;
    end;
  end;

  function FindNamedNode(AParent : TDOMNode; const AElementName, ANodeName : string) : TDOMNode;
  var
    ndE, nd : TDOMNode;
  begin
    Result := nil;
    ndE := AParent.FirstChild;
    while Assigned(ndE) do begin
      if AnsiSameText(AElementName,ndE.NodeName) and Assigned(ndE.Attributes) then begin
        nd := ndE.Attributes.GetNamedItem(s_name);
        if Assigned(nd) and AnsiSameText(ANodeName,nd.NodeValue) then begin
          Result := nd;
          Break;
        end;
      end;
      ndE := ndE.NextSibling;
    end;
  end;

var
  unitNamespace : string;
begin
  inherited Prepare(ASymTable, AModule);
  FTypesNode := FindNode(Document.DocumentElement,s_types) as TDOMElement;
  if ( FTypesNode = nil ) then
    raise EWsdlGeneratorException.Create('Unable to find "types" node.');
  unitNamespace := ASymTable.GetExternalName(AModule);
  FSchemaNode := FindNamedNode(FTypesNode,s_xs_short,unitNamespace) as TDOMElement;
  if ( FSchemaNode = nil ) then begin
    FSchemaNode := CreateElement(s_xs_short + ':' + s_schema,FTypesNode,Document);
    FSchemaNode.SetAttribute(s_xmlns,s_xs);
    FSchemaNode.SetAttribute(s_targetNamespace,unitNamespace);
    FSchemaNode.SetAttribute(Format('%s:%s',[s_xmlns,s_tns]),unitNamespace);
  end;
end;

{ TWsdlGenerator }

procedure TWsdlGenerator.GenerateTypes(
   ASymTable : TwstPasTreeContainer;
   AModule   : TPasModule
);
var
  i, c : Integer;
  mdl : TPasModule;
  mdlLs : TList2;
  g : IGenerator;
  nsList : TStringList;
  s : string;
  locLocator : IDocumentLocator;
begin
  mdlLs := ASymTable.Package.Modules;
  if ( mdlLs.Count > 0 ) then begin
    nsList := TStringList.Create();
    try
      c := StrToIntDef(ASymTable.Properties.GetValue(AModule,sNS_COUNT),0);
      if (c > 0) then begin
        for i := 1 to c do begin
          s := ASymTable.Properties.GetValue(AModule,sNS_ITEM+IntToStr(i));
          nsList.Add(s);
        end;
      end;
      g := TWsdlTypechemaGenerator.Create(Document) as IGenerator;
      locLocator := GetDocumentLocator();
      if (locLocator <> nil) then
        g.SetDocumentLocator(locLocator);
      if not Assigned(g.GetNotificationHandler()) then
        g.SetNotificationHandler(Self.GetNotificationHandler());
      for i := 0 to Pred(mdlLs.Count) do begin
        mdl := TPasModule(mdlLs[i]);
        if (mdl <> AModule) then begin
          if mdl.InheritsFrom(TPasNativeModule) then
            Continue;
          s := ASymTable.GetExternalName(mdl);
          if (nsList.IndexOf(s) = -1) then
            Continue;
        end;
        g.Execute(ASymTable,mdl.Name);
      end;
    finally
      nsList.Free();
    end;
  end;
end;

procedure TWsdlGenerator.GenerateServiceMessages(
  ASymTable : TwstPasTreeContainer;
  AModule   : TPasModule;
  AContract : TPasClassType;
  ARootNode : TDOMElement
);

    procedure GenerateOperationMessage(AOperation : TPasProcedure);

      procedure GenerateParam(APrm : TPasArgument; AMsgNode : TDOMElement);
      var
        tmpNode : TDOMElement;
        ns_shortName, s : string;
        typItm : TPasType;
        typeHelper : IXsdSpecialTypeHelper;
      begin
        tmpNode := CreateElement(s_part,AMsgNode,Document);
        tmpNode.SetAttribute(s_name,ASymTable.GetExternalName(APrm));
        typItm := APrm.ArgType;
        if Assigned(typItm.Parent) and Assigned(typItm.Parent.Parent) then
          s := ASymTable.GetExternalName(typItm.Parent.Parent)
        else
          s := ASymTable.GetExternalName(AModule);
        ns_shortName := GetNameSpaceShortName(s,Document,nil);
        s := Format('%s:%s',[ns_shortName,ASymTable.GetExternalName(typItm)]);
        tmpNode.SetAttribute(s_type,s);
        if typItm.InheritsFrom(TPasNativeSpecialSimpleType) then begin
          if GetXsdTypeHandlerRegistry().FindHelper(typItm,typeHelper) then
            typeHelper.HandleTypeUsage(tmpNode,ARootNode);
        end;
      end;

      procedure GenerateResultParam(APrm : TPasResultElement; AMsgNode : TDOMElement);
      var
        tmpNode : TDOMElement;
        ns_shortName, s : string;
        typItm : TPasType;
        typeHelper : IXsdSpecialTypeHelper;
      begin
        tmpNode := CreateElement(s_part,AMsgNode,Document);
        tmpNode.SetAttribute(s_name,ASymTable.GetExternalName(APrm));
        typItm := APrm.ResultType;
        if Assigned(typItm.Parent) and Assigned(typItm.Parent.Parent) then
          s := ASymTable.GetExternalName(typItm.Parent.Parent)
        else
          s := ASymTable.GetExternalName(AModule);
        ns_shortName := GetNameSpaceShortName(s,Document,nil);
        s := Format('%s:%s',[ns_shortName,ASymTable.GetExternalName(typItm)]);
        tmpNode.SetAttribute(s_type,s);
        if typItm.InheritsFrom(TPasNativeSpecialSimpleType) then begin
          if GetXsdTypeHandlerRegistry().FindHelper(typItm,typeHelper) then
            typeHelper.HandleTypeUsage(tmpNode,ARootNode);
        end;
      end;

    var
      qryNode, rspNode : TDOMElement;
      ii, cc : Integer;
      pp : TPasArgument;
      prmAccessList : TStringList;
      prmAccessStr : string;
      docNode : TDOMNode;
    begin
      prmAccessList := TStringList.Create();
      try
        qryNode := CreateElement(s_message,ARootNode,Document);
        qryNode.SetAttribute(s_name,Format('%s',[ASymTable.GetExternalName(AOperation)]));
        rspNode := CreateElement(s_message,ARootNode,Document);
        rspNode.SetAttribute(s_name,Format('%sResponse',[ASymTable.GetExternalName(AOperation)]));
        cc := AOperation.ProcType.Args.Count;
        for ii := 0 to Pred(cc) do begin
          pp := TPasArgument(AOperation.ProcType.Args[ii]);
          if ( pp.Access in [argDefault, argConst, argVar] ) then begin
            GenerateParam(pp,qryNode);
            if ( pp.Access = argDefault ) then
              prmAccessList.Add(Format('%s=%s',[ASymTable.GetExternalName(pp),GetEnumName(TypeInfo(TArgumentAccess),Ord(pp.Access))]));
          end;
          if ( pp.Access in [argVar, argOut] ) then begin
            GenerateParam(pp,rspNode);
          end;
        end;
        if AOperation.InheritsFrom(TPasFunction) then begin
          GenerateResultParam(TPasFunctionType(AOperation.ProcType).ResultEl,rspNode);
        end;
        if ( prmAccessList.Count > 0 ) then begin
          docNode := Document.CreateElement(s_documentation);
          if qryNode.HasChildNodes() then
            qryNode.InsertBefore(docNode,qryNode.FirstChild)
          else
            qryNode.AppendChild(docNode);
          prmAccessStr := '';
          for ii := 0 to Pred(prmAccessList.Count) do begin
            prmAccessStr := prmAccessStr + ';' +
                            prmAccessList.Names[ii] + '=' + prmAccessList.ValueFromIndex[ii];
          end;
          Delete(prmAccessStr,1,1);
          CreateElement(s_paramAccess,docNode,Document).SetAttribute(s_value,prmAccessStr);
        end;
      finally
        prmAccessList.Free();
      end;
    end;

Var
  j, k : Integer;
  po : TPasProcedure;
begin
  k := AContract.Members.Count;
  if ( k > 0 ) then begin
    for j := 0 to pred(k) do begin
      if TPasElement(AContract.Members[j]).InheritsFrom(TPasProcedure) then begin
        po := TPasProcedure(AContract.Members[j]);
        GenerateOperationMessage(po);
      end;
    end;
  end;
end;

procedure TWsdlGenerator.GenerateServicePortType(
  ASymTable : TwstPasTreeContainer;
  AModule   : TPasModule;
  AContract : TPasClassType;
  ARootNode : TDOMElement
);

  procedure GenerateOperation(AOperation : TPasProcedure; APrtTypeNode : TDOMElement);
  var
    opNode, inNode, outNode : TDOMElement;
  begin
    opNode := CreateElement(s_operation,APrtTypeNode,Document);
    opNode.SetAttribute(s_name,ASymTable.GetExternalName(AOperation));
    inNode := CreateElement(s_input,opNode,Document);
    inNode.SetAttribute(s_message,Format('%s:%s',[s_tns,ASymTable.GetExternalName(AOperation)]));
    outNode := CreateElement(s_output,opNode,Document);
    outNode.SetAttribute(s_message,Format('%s:%sResponse',[s_tns,ASymTable.GetExternalName(AOperation)]));
  end;

var
  prtTypeNode, docNode : TDOMElement;
  j, k : Integer;
  po : TPasProcedure;
begin
  prtTypeNode := CreateElement(s_portType,ARootNode,Document);
  if ( Length(AContract.InterfaceGUID) > 0 ) then begin
    docNode := CreateElement(s_documentation,prtTypeNode,Document);
    CreateElement(s_guid,docNode,Document).SetAttribute(s_value,AContract.InterfaceGUID);
  end else begin
    docNode := nil;
  end;
  prtTypeNode.SetAttribute(s_name,ASymTable.GetExternalName(AContract));
  k := AContract.Members.Count;
  if ( k > 0 ) then begin
    for j := 0 to pred(k) do begin
      if TPasElement(AContract.Members[j]).InheritsFrom(TPasProcedure) then begin
        po := TPasProcedure(AContract.Members[j]);
        GenerateOperation(po,prtTypeNode);
      end;
    end;
  end;
end;

procedure TWsdlGenerator.GenerateServiceBinding(
  ASymTable : TwstPasTreeContainer;
  AModule   : TPasModule;
  ABinding  : TwstBinding;
  ARootNode : TDOMElement
);
  procedure GenerateOperation(AOperation : TPasProcedure; ABndngNode : TDOMElement);
  var
    opNode, inNode, outNode, bdyNode : TDOMElement;
    strBuff, strSoapActBuffer : string;
    encdStyl{,encdStylURI} : string;
  begin
    strBuff := Format('%s:%s',[s_soap_short_name,s_operation]);
    opNode := CreateElement(s_operation,ABndngNode,Document);
    opNode.SetAttribute(s_name,ASymTable.GetExternalName(AOperation));
      strSoapActBuffer := Trim(ASymTable.Properties.GetValue(AOperation,s_transport + '_' + s_soapAction));
      {if ( Length(strSoapActBuffer) = 0 ) then begin
        strSoapActBuffer := Format('%s/%s/%s',[ASymbolTable.GetExternalName(ASymbolTable.CurrentModule),ASymbolTable.GetExternalName(ABinding.Intf),ASymbolTable.GetExternalName(AOperation)]);
      end;}
      CreateElement(strBuff,opNode,Document).SetAttribute(s_soapAction,strSoapActBuffer);
      inNode := CreateElement(s_input,opNode,Document);
        strBuff := Format('%s:%s',[s_soap_short_name,s_body]);
        bdyNode := CreateElement(strBuff,inNode,Document);
        encdStyl := s_literal;
        {encdStylURI := '';
        propData := Find(AOperation^.Properties,sFORMAT_Input_EncodingStyle);
        if Assigned(propData) and ( Length(Trim(propData^.Data)) > 0 ) then begin
          encdStyl := Trim(propData^.Data);
        end;}
        bdyNode.SetAttribute(s_use,encdStyl);
        bdyNode.SetAttribute(s_namespace,Format('%s',[ASymTable.GetExternalName(AModule)]));
        {propData := Find(AOperation^.Properties,sFORMAT_Input_EncodingStyleURI);
        if Assigned(propData) and ( Length(Trim(propData^.Data)) > 0 ) then begin
          encdStylURI := Trim(propData^.Data);
        end;
        if ( Length(encdStylURI) > 0 ) then
          bdyNode.SetAttribute(sSOAP_ENCODING_STYLE,encdStylURI); }

      outNode := CreateElement(s_output,opNode,Document);
        strBuff := Format('%s:%s',[s_soap_short_name,s_body]);
        bdyNode := CreateElement(strBuff,outNode,Document);
        bdyNode.SetAttribute(s_use,encdStyl);
        bdyNode.SetAttribute(s_namespace,Format('%s',[ASymTable.GetExternalName(AModule)]));
        {if ( Length(encdStylURI) > 0 ) then
          bdyNode.SetAttribute(sSOAP_ENCODING_STYLE,encdStylURI);}
  end;

var
  bndgNode, soapbndgNode : TDOMElement;
  j, k : Integer;
  po : TPasProcedure;
  strBuf : string;
begin
  bndgNode := CreateElement(s_binding,ARootNode,Document);
  bndgNode.SetAttribute(s_name,ABinding.Name);
  bndgNode.SetAttribute(s_type,Format('%s:%s',[s_tns,ASymTable.GetExternalName(ABinding.Intf)]));

  strBuf := Format('%s:%s',[s_soap_short_name,s_binding]);
  soapbndgNode := CreateElement(strBuf,bndgNode,Document);
  soapbndgNode.SetAttribute(s_style,s_rpc);
  soapbndgNode.SetAttribute(s_transport,s_soapTransport);

  k := ABinding.Intf.Members.Count;
  if ( k > 0 ) then begin
    for j := 0 to pred(k) do begin
      if TPasElement(ABinding.Intf.Members[j]).InheritsFrom(TPasProcedure) then begin
        po := TPasProcedure(ABinding.Intf.Members[j]);
        GenerateOperation(po,bndgNode);
      end;
    end;
  end;
end;

procedure TWsdlGenerator.GenerateServicePublication(
  ASymTable : TwstPasTreeContainer;
  AModule   : TPasModule;
  ABinding  : TwstBinding;
  ARootNode : TDOMElement
);
var
  srvcNode, portNode, soapAdrNode : TDOMElement;
  strBuf : string;
begin
  srvcNode := CreateElement(s_service,ARootNode,Document);
  srvcNode.SetAttribute(s_name,ASymTable.GetExternalName(ABinding.Intf));

  strBuf := Format('%s',[s_port]);
  portNode := CreateElement(strBuf,srvcNode,Document);
  portNode.SetAttribute(s_name,ASymTable.GetExternalName(ABinding.Intf) + 'Port');
  portNode.SetAttribute(s_binding,Format('%s:%s',[s_tns,ABinding.Name]));

  strBuf := Format('%s:%s',[s_soap_short_name,s_address]);
  soapAdrNode := CreateElement(strBuf,portNode,Document);
  soapAdrNode.SetAttribute(s_location,ABinding.Address);
end;

function TWsdlGenerator.GetNotificationHandler: TOnLogMessageEvent;
begin
  Result := FMessageHandler;
end;

procedure TWsdlGenerator.SetNotificationHandler(const AValue: TOnLogMessageEvent);
begin
  FMessageHandler := AValue;
end;

procedure TWsdlGenerator.Prepare(ASymTable : TwstPasTreeContainer; AModule : TPasModule);

  function CreateRootNode():TDOMElement;
  var
    extName : string;
  begin
    extName := ASymTable.GetExternalName(AModule);
    Result := CreateElement(s_definitions,Document,Document);
    Result.SetAttribute(s_name,extName);

    Result.SetAttribute(s_targetNamespace,extName);
    Result.SetAttribute(Format('%s:%s',[s_xmlns,s_soap_short_name]),s_soap);
    Result.SetAttribute(Format('%s:%s',[s_xmlns,s_xs_short]),s_xs);
    Result.SetAttribute(Format('%s:%s',[s_xmlns,s_tns]),extName);
    Result.SetAttribute(s_xmlns,s_wsdl);
  end;

  function CreateTypesRootNode(ARootNode :  TDOMNode):TDOMElement;
  begin
    Result := CreateElement(s_types,ARootNode,Document);
  end;
  
begin
  FDefinitionsNode := CreateRootNode();
  FTypesNode := CreateTypesRootNode(FDefinitionsNode);

end;

function TWsdlGenerator.GetDocumentLocator : IDocumentLocator;
begin
  Result := FDocumentLocator;
end;

procedure TWsdlGenerator.SetDocumentLocator(ALocator : IDocumentLocator);
begin
  FDocumentLocator := ALocator;
end;

procedure TWsdlGenerator.Execute(ASymTable : TwstPasTreeContainer; AModuleName : string);
var
  locMainModule : TPasModule;
  decList : TList2;
  j, c : Integer;
  sym : TPasElement;
  ps : TPasClassType;
  bndg : TwstBinding;
begin
  locMainModule := ASymTable.FindModule(AModuleName);
  if ( locMainModule = nil ) then
    locMainModule := ASymTable.CurrentModule;
  if ( locMainModule = nil ) then
    raise EWsdlGeneratorException.Create('Invalid symbol table.');
  Prepare(ASymTable,locMainModule);

  GenerateTypes(ASymTable,locMainModule);
  
  decList := locMainModule.InterfaceSection.Declarations;
  c := decList.Count;
  for j := 0 to Pred(c) do begin
    sym := TPasElement(decList[j]);
    if sym.InheritsFrom(TPasClassType) and ( TPasClassType(sym).ObjKind = okInterface ) then begin
      ps := TPasClassType(sym);
      GenerateServiceMessages(ASymTable, locMainModule, ps, FDefinitionsNode);
      GenerateServicePortType(ASymTable, locMainModule, ps, FDefinitionsNode);
    end;
  end;
  
  for j := 0 to Pred(ASymTable.BindingCount) do begin
    bndg := ASymTable.Binding[j];
    GenerateServiceBinding(ASymTable, locMainModule, bndg,FDefinitionsNode);
      GenerateServicePublication(ASymTable, locMainModule, bndg, FDefinitionsNode);
  end;
end;

constructor TWsdlGenerator.Create(ADocument : TDOMDocument);
begin
  if ( ADocument = nil ) then
    raise EWsdlGeneratorException.Create('Invalid not assigned.');
  FDocument := ADocument;
end;

end.
