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
unit server_service_soap;

interface

uses
  Classes, SysUtils, TypInfo,
  {$IFNDEF FPC}xmldom, wst_delphi_xml{$ELSE}DOM{$ENDIF},
  base_service_intf, server_service_intf, server_service_imputils,
  base_soap_formatter;

type

  { TSOAPFormatter }

{$M+}
  TSOAPFormatter = class(TSOAPBaseFormatter,IFormatterBase,IFormatterResponse)
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

  procedure Server_service_RegisterSoapFormat();
  
implementation

Const NAMESPACE_SEPARATOR = ':';
function ExtractNamespacePart( Const AQualifiedName : string):String;
Var
  i : Integer;
begin
  Result := '';
  i := Pos(NAMESPACE_SEPARATOR,AQualifiedName);
  If ( i <= 0 ) Then
    Exit;
  Result := Copy(AQualifiedName,1,Pred(i));
end;

function ExtractNamePart(Const AQualifiedName : string):String;
Var
  i : Integer;
begin
  i := Pos(NAMESPACE_SEPARATOR,AQualifiedName);
  If ( i <= 0 ) Then
    i := 0;
  Result := Copy(AQualifiedName,Succ(i),MaxInt);
end;

{ TSOAPFormatter }

procedure TSOAPFormatter.BeginCallResponse(Const AProcName,ATarget:string);
var
  locOldStyle : TSOAPDocumentStyle;
begin
  if ( FCallContext = nil ) then
    FCallContext := TSimpleCallContext.Create();
  Clear();
  Prepare();
    WriteHeaders(FCallContext);
    locOldStyle := Style;
    Style := Document;
      BeginScope('Body',sSOAP_ENV,'',stObject,asNone);
    Style := locOldStyle;
      BeginScope(AProcName + 'Response',ATarget,'',stObject,asNone);
end;

procedure TSOAPFormatter.EndCallResponse();
begin
      EndScope(); //BeginScope(AProcName,ATarget);
    EndScope();   //BeginScope('Body','http://schemas.xmlsoap.org/soap/envelope/');
  EndScope();     //BeginScope('Envelope','http://schemas.xmlsoap.org/soap/envelope/','SOAP-ENV');
end;

procedure TSOAPFormatter.BeginCallRead(ACallContext : ICallContext);
Var
  envNd : TDOMElement;
  hdrNd, bdyNd, mthdNd : TDOMNode;
  s,nsShortName,eltName : string;
  doc : TXMLDocument;
begin
  FCallContext := ACallContext;
  ClearStack();
  doc := GetXmlDoc();
  If FindAttributeByValueInNode(sSOAP_ENV,doc.DocumentElement,nsShortName) Then Begin
    nsShortName := Copy(nsShortName,1 + Pos(':',nsShortName),MaxInt);
    If Not IsStrEmpty(nsShortName) Then
      nsShortName := nsShortName + ':';
  End Else
    nsShortName := '';
  eltName := nsShortName + 'Envelope';
  envNd := doc.DocumentElement;
  If Not SameText(eltName,envNd.NodeName) Then
    Error('XML root node must be "Envelope".');
  PushStack(envNd).SetNameSpace(sSOAP_ENV);

  bdyNd := envNd.FirstChild;
  If Not Assigned(bdyNd) Then
    Error('Node not found : "Body".');

  eltName := nsShortName + 'Body';
  if not SameText(bdyNd.NodeName,eltName) then begin
    eltName := nsShortName + 'Header';
    hdrNd := bdyNd;
    bdyNd := hdrNd.NextSibling;
    if SameText(hdrNd.NodeName,eltName) then begin
      PushStack(hdrNd,asScoped,'').SetNameSpace(sSOAP_ENV);
      ReadHeaders(FCallContext);
      PopStack().Free();
    end;
  end;
  
  eltName := nsShortName + 'Body';
  If Not Assigned(bdyNd) Then
    Error('Node not found : "Body".');
  PushStack(bdyNd).SetNameSpace(sSOAP_ENV);
  If Not Assigned(bdyNd.FirstChild) Then
    Error('Method Node not found.');
  mthdNd := bdyNd.FirstChild;
  PushStack(mthdNd);
  s := mthdNd.NodeName;
  FCallProcedureName := ExtractNamePart(s);
  If IsStrEmpty(FCallProcedureName) Then
    Error('No Method name.');
  nsShortName := ExtractNamespacePart(s);
  if IsStrEmpty(nsShortName) then
    FCallTarget := FindAttributeByNameInScope(sXML_NS)
  else
    FCallTarget := FindAttributeByNameInScope(sXML_NS + ':' + nsShortName);
  If IsStrEmpty(FCallTarget) Then
    Error('Method Node must have a qualified name.');
end;

function TSOAPFormatter.GetCallProcedureName(): String;
begin
  Result := FCallProcedureName;
end;

function TSOAPFormatter.GetCallTarget(): String;
begin
  Result := FCallTarget;
end;

procedure TSOAPFormatter.BeginExceptionList(
  const AErrorCode: string;
  const AErrorMsg: string
);
Var
  c,m :string;
begin
  If IsStrEmpty(AErrorCode) Then
    c := 'SOAP-ENV:Server'
  Else
    c := AErrorCode;
  If IsStrEmpty(AErrorMsg) Then
    m := 'Server Error'
  Else
    m := AErrorMsg;
  Clear();
  Style := Document;
  BeginScope('Envelope',sSOAP_ENV,'SOAP-ENV',stObject,asNone);
    AddScopeAttribute('xmlns:xsi',sXSI_NS);
    AddScopeAttribute('xmlns:'+sXSD, sXSD_NS);
    BeginScope('Body',sSOAP_ENV,'',stObject,asNone);
      BeginScope('Fault',sSOAP_ENV,'',stObject,asNone);
        Put('faultcode',TypeInfo(string),c);
        Put('faultstring',TypeInfo(string),m);
end;

procedure TSOAPFormatter.EndExceptionList();
begin
      EndScope(); //BeginScope('Fault',sSOAP_ENV);
    EndScope();   //BeginScope('Body','http://schemas.xmlsoap.org/soap/envelope/');
  EndScope();     //BeginScope('Envelope','http://schemas.xmlsoap.org/soap/envelope/','SOAP-ENV');
end;

procedure Server_service_RegisterSoapFormat();
begin
  GetFormatterRegistry().Register(sPROTOCOL_NAME,sSOAP_CONTENT_TYPE,TSimpleItemFactory.Create(TSOAPFormatter));
  RegisterStdTypes();
end;

end.
