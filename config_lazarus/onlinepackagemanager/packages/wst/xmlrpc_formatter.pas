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
unit xmlrpc_formatter;

interface

uses
  Classes, SysUtils, TypInfo,
  {$IFNDEF FPC}xmldom, wst_delphi_xml{$ELSE}DOM{$ENDIF},
  base_service_intf, service_intf, imp_utils, base_xmlrpc_formatter;

{$INCLUDE wst.inc}
{$INCLUDE wst_delphi.inc}

type


  { TXmlRpcFormatter }
{$M+}
  TXmlRpcFormatter = class(TXmlRpcBaseFormatter,IFormatterClient)
  private
    FCallProcedureName : string;
    FCallTarget : String;
  public
    procedure BeginCall(
      const AProcName,
            ATarget      : string;
            ACallContext : ICallContext
    );
    procedure EndCall();
    procedure BeginCallRead(ACallContext : ICallContext);

    function GetCallProcedureName():String;
    function GetCallTarget():String;
  end;

  { TXmlRpcCallMaker }

  TXmlRpcCallMaker = class(TSimpleFactoryItem,ICallMaker)
  private
    FPropMngr : IPropertyManager;
    FUniqueAddress: Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
    function GetPropertyManager():IPropertyManager;
    procedure MakeCall(
      ASerializer : IFormatterClient;
      ATransport  : ITransport
    );
  published
    property UniqueAddress : Boolean read FUniqueAddress Write FUniqueAddress;
  end;
{$M-}

implementation
{$IFDEF FPC}uses wst_fpc_xml;{$ENDIF}

{ TXmlRpcFormatter }

procedure TXmlRpcFormatter.BeginCall(
  const AProcName,
        ATarget      : string;
        ACallContext : ICallContext
);
var
  mthdNode, mthNameNode, prmsNode : TDOMNode;
  doc : TXMLDocument;
begin
  Clear();
  doc := Self.GetXmlDoc();
  
  FCallTarget := ATarget;
  FCallProcedureName := AProcName;
  
  // methodCall
  mthdNode := doc.CreateElement(sMETHOD_CALL);
  doc.AppendChild(mthdNode);

    //methodName
    mthNameNode := doc.CreateElement(sMETHOD_NAME);
    mthdNode.AppendChild(mthNameNode);
    mthNameNode.AppendChild(doc.CreateTextNode(FCallProcedureName));
    
    //params
    prmsNode := doc.CreateElement(sPARAMS);
    mthdNode.AppendChild(prmsNode);
    PushStackParams(prmsNode);
end;

procedure TXmlRpcFormatter.EndCall();
begin
  EndScope();
end;

procedure TXmlRpcFormatter.BeginCallRead(ACallContext : ICallContext);
var
  callNode : TDOMElement;
  prmsNode, faultNode, tmpNode : TDOMNode;
  doc : TXMLDocument;
  errCode : Integer;
  eltName, errMsg : string;
  excpt_Obj : EXmlRpcException;
begin
  ClearStack();
  doc := GetXmlDoc();
  callNode := doc.DocumentElement;
  if not SameText(sMETHOD_RESPONSE,callNode.NodeName) then
    Error('XML root node must be "%s".',[sMETHOD_RESPONSE]);

  prmsNode := FindNode(callNode,sPARAMS);
  if ( prmsNode <> nil ) then begin
    PushStackParams(prmsNode);
  end else begin
    faultNode := FindNode(callNode,sFAULT);
    if ( faultNode = nil ) then begin
      raise EServiceException.CreateFmt('Invalid XmlRPC response message, "%s" or "%s" are not present.',[sPARAMS,sFAULT]);
    end;
    tmpNode := FindNode(faultNode,sVALUE);
    if ( tmpNode = nil ) then begin
      raise EServiceException.CreateFmt('Invalid XmlRPC fault response message, "%s"  is not present.',[sVALUE]);
    end;
    tmpNode := FindNode(tmpNode,XmlRpcDataTypeNames[xdtStruct]);
    if ( tmpNode = nil ) then begin
      raise EServiceException.CreateFmt('Invalid XmlRPC fault response message, "%s"  is not present.',[XmlRpcDataTypeNames[xdtStruct]]);
    end;
    PushStack(tmpNode);
    eltName := sFAULT_CODE;
    errCode := 0;
    Get(TypeInfo(Integer),eltName,errCode) ;
    eltName := sFAULT_STRING;
    errMsg := '';
    Get(TypeInfo(string),eltName,errMsg);
    excpt_Obj := EXmlRpcException.Create('');
    excpt_Obj.FaultCode := IntToStr(errCode);
    excpt_Obj.FaultString := errMsg;                   ;
    excpt_Obj.Message := Format(
      'Service exception :%s   Code = "%s"%s   Message = "%s"',
      [LineEnding,excpt_Obj.FaultCode,LineEnding,excpt_Obj.FaultString]
    );
    raise excpt_Obj;
  end;
end;

function TXmlRpcFormatter.GetCallProcedureName(): String;
begin
  Result := FCallProcedureName;
end;

function TXmlRpcFormatter.GetCallTarget(): String;
begin
  Result := FCallTarget;
end;

{ TXmlRpcCallMaker }

constructor TXmlRpcCallMaker.Create();
begin
  FUniqueAddress := True;
  FPropMngr := TPublishedPropertyManager.Create(Self);
end;

destructor TXmlRpcCallMaker.Destroy();
begin
  FPropMngr := Nil;
  inherited Destroy();
end;

function TXmlRpcCallMaker.GetPropertyManager(): IPropertyManager;
begin
  Result := FPropMngr;
end;

procedure TXmlRpcCallMaker.MakeCall(
  ASerializer : IFormatterClient;
  ATransport  : ITransport
);
var
  rqt, rsps : TMemoryStream;
  propMngr : IPropertyManager;
begin
  Assert(Assigned(ASerializer));
  Assert(Assigned(ATransport));
  propMngr := ATransport.GetPropertyManager();
  propMngr.SetProperty(
    sCONTENT_TYPE,
    ASerializer.GetPropertyManager().GetProperty(sCONTENT_TYPE)
  );
  propMngr.SetProperty(
    sFORMAT,
    sPROTOCOL_NAME
  );
  rsps := nil;
  rqt := TMemoryStream.Create();
  try
    rsps := TMemoryStream.Create();
    ASerializer.SaveToStream(rqt);
    rqt.Position := 0;
    ATransport.SendAndReceive(rqt,rsps);
    rqt.Clear();
    rsps.Position := 0;
    ASerializer.Clear();
    ASerializer.LoadFromStream(rsps);
  finally
    rsps.Free();
    rqt.Free();
  end;
end;

procedure RegisterXmlRpcProtocol();
begin
  RegisterStdTypes();
  GetFormaterRegistry().Register(
    sPROTOCOL_NAME,
    TSimpleItemFactory.Create(TXmlRpcFormatter),
    TSimpleItemFactory.Create(TXmlRpcCallMaker)
  );
end;

Initialization
  RegisterXmlRpcProtocol();

end.
