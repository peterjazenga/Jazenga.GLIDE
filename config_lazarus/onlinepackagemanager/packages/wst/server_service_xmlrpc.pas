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
unit server_service_xmlrpc;

interface

uses
  Classes, SysUtils, TypInfo,
  {$IFNDEF FPC}xmldom, wst_delphi_xml{$ELSE}DOM{$ENDIF},
  base_service_intf, server_service_intf,
  base_xmlrpc_formatter;

type

{$M+}

  { TXmlRpcFormatter }

  TXmlRpcFormatter = class(TXmlRpcBaseFormatter,IFormatterBase,IFormatterResponse)
  private
    FCallProcedureName : string;
    FCallTarget : String;
    FCallContext : ICallContext;
  public

    procedure BeginCallResponse(Const AProcName,ATarget:string);
    procedure EndCallResponse();

    procedure BeginCallRead(ACallContext : ICallContext);
    function GetCallProcedureName():String;
    function GetCallTarget():String;

    procedure BeginExceptionList(
      const AErrorCode : string;
      const AErrorMsg  : string
    );
    procedure EndExceptionList();
  End;

  procedure Server_service_RegisterXmlRpcFormat();

implementation
{$IFDEF FPC}uses wst_fpc_xml;{$ENDIF}

{ TXmlRpcFormatter }

procedure TXmlRpcFormatter.BeginCallResponse(Const AProcName,ATarget:string);
var
  mthdNode, prmsNode : TDOMNode;
  doc : TXMLDocument;
begin
  Clear();
  doc := Self.GetXmlDoc();
  mthdNode := doc.CreateElement(sMETHOD_RESPONSE);
  doc.AppendChild(mthdNode);
    prmsNode := doc.CreateElement(sPARAMS);
    mthdNode.AppendChild(prmsNode);
    PushStackParams(prmsNode);
end;

procedure TXmlRpcFormatter.EndCallResponse();
begin
  EndScope();
end;

procedure TXmlRpcFormatter.BeginCallRead(ACallContext : ICallContext);
var
  callNode : TDOMElement;
  tmpNode : TDOMNode;
  doc : TXMLDocument;
begin
  FCallContext := ACallContext;
  ClearStack();
  doc := GetXmlDoc();
  callNode := doc.DocumentElement;
  if not SameText(sMETHOD_CALL,callNode.NodeName) then
    Error('XML root node must be "%s".',[sMETHOD_CALL]);

  tmpNode := FindNode(callNode,sMETHOD_NAME);
  if not Assigned(tmpNode) then
    Error('Node not found : "%s".',[sMETHOD_NAME]);
  if not tmpNode.HasChildNodes() then
    Error('"%s" does not provide value node.',[sMETHOD_NAME]);
  FCallProcedureName := Trim(tmpNode.FirstChild.NodeValue);
  
  tmpNode := FindNode(callNode,sPARAMS);
  if not Assigned(tmpNode) then
    Error('Node not found : "%s".',[sPARAMS]);
  PushStackParams(tmpNode);

  //FCallTarget := tmpNode.NodeValue;
end;

function TXmlRpcFormatter.GetCallProcedureName(): String;
begin
  Result := FCallProcedureName;
end;

function TXmlRpcFormatter.GetCallTarget(): String;
begin
  Result := FCallTarget;
end;

procedure TXmlRpcFormatter.BeginExceptionList(
  const AErrorCode: string;
  const AErrorMsg: string
);
var
  c,m : string;
  i : Integer;
  memberNode, mthdNode, faultNode, structNode,
  valueNode, nameNode,
  internalValueNode, lastValNode : TDOMNode;
  doc : TXMLDocument;
begin
  c := Trim(AErrorCode);
  if not TryStrToInt(c,i) then
    c := '123';
  m := AErrorMsg;
  Clear();
  doc := Self.GetXmlDoc();
  mthdNode := doc.CreateElement(sMETHOD_RESPONSE);
  doc.AppendChild(mthdNode);
    //fault node
    faultNode := doc.CreateElement(sFAULT);
    mthdNode.AppendChild(faultNode);
      // value node
      valueNode := doc.CreateElement(sVALUE);
      faultNode.AppendChild(valueNode);
        // structNode
        structNode := doc.CreateElement(XmlRpcDataTypeNames[xdtStruct]);
        valueNode.AppendChild(structNode);
          //faultCode member node
          memberNode := doc.CreateElement(sMEMBER);
          structNode.AppendChild(memberNode);
            //name node
            nameNode := doc.CreateElement(sNAME);
            memberNode.AppendChild(nameNode);
            nameNode.AppendChild(doc.CreateTextNode(sFAULT_CODE));
            //value node
            internalValueNode := doc.CreateElement(sVALUE);
            memberNode.AppendChild(internalValueNode);
            lastValNode := doc.CreateElement(XmlRpcDataTypeNames[xdtInt]);
            internalValueNode.AppendChild(lastValNode);
            lastValNode.AppendChild(doc.CreateTextNode(c));
          //faultString member node
          memberNode := doc.CreateElement(sMEMBER);
          structNode.AppendChild(memberNode);
            //name node
            nameNode := doc.CreateElement(sNAME);
            memberNode.AppendChild(nameNode);
            nameNode.AppendChild(doc.CreateTextNode(sFAULT_STRING));
            //value node
            internalValueNode := doc.CreateElement(sVALUE);
            memberNode.AppendChild(internalValueNode);
            lastValNode := doc.CreateElement(XmlRpcDataTypeNames[xdtString]);
            internalValueNode.AppendChild(lastValNode);
            lastValNode.AppendChild(doc.CreateTextNode(m));
end;

procedure TXmlRpcFormatter.EndExceptionList();
begin
end;

procedure Server_service_RegisterXmlRpcFormat();
begin
  GetFormatterRegistry().Register(sPROTOCOL_NAME,sXMLRPC_CONTENT_TYPE,TSimpleItemFactory.Create(TXmlRpcFormatter));
end;

end.

