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
unit soap_formatter;

interface

uses
  Classes, SysUtils, TypInfo,
  {$IFNDEF FPC}xmldom, wst_delphi_xml{$ELSE}DOM{$ENDIF},
  wst_types, base_service_intf, service_intf, imp_utils, base_soap_formatter;

type


  { TSOAPFormatter }
{$M+}
  TSOAPFormatter = class(TSOAPBaseFormatter,IFormatterClient)
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
  End;
  
  { TSOAPCallMaker }

  TSOAPCallMaker = class(TSimpleFactoryItem,ICallMaker)
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


{ TSOAPFormatter }

procedure TSOAPFormatter.BeginCall(
  const AProcName,
        ATarget      : string;
        ACallContext : ICallContext
);
var
  locOldStyle : TSOAPDocumentStyle;
begin
  Prepare();
  WriteHeaders(ACallContext);
  locOldStyle := Style;
  Style := Document;
    BeginScope('Body',sSOAP_ENV,'',stObject,asNone);
    if (locOldStyle = RPC) then
      BeginScope(AProcName,ATarget,'',stObject,asNone);
  Style := locOldStyle;
      
  FCallTarget := ATarget;
  FCallProcedureName := AProcName;
end;

procedure TSOAPFormatter.EndCall();
begin
      if ( Style = RPC ) then
        EndScope(); //BeginScope(AProcName,ATarget);
    EndScope();   //BeginScope('Body','http://schemas.xmlsoap.org/soap/envelope/');
  EndScope();     //BeginScope('Envelope','http://schemas.xmlsoap.org/soap/envelope/','SOAP-ENV');
end;

procedure TSOAPFormatter.BeginCallRead(ACallContext : ICallContext);
Var
  envNd : TDOMElement;
  bdyNd, fltNd, hdrNd : TDOMNode;
  nsShortName,eltName, msgBuff : string;
  excpt_Obj : ESOAPException;
  doc : TXMLDocument;
  oldStyle : TSOAPDocumentStyle;
begin
  ClearStack();
  doc := GetXmlDoc();
  If FindAttributeByValueInNode(sSOAP_ENV,doc.DocumentElement,nsShortName) or
     FindAttributeByValueInNode('"' + sSOAP_ENV + '"',doc.DocumentElement,nsShortName)
  Then Begin
    nsShortName := Copy(nsShortName,1 + Pos(':',nsShortName),MaxInt);
    If Not IsStrEmpty(nsShortName) Then
      nsShortName := nsShortName + ':';
  End Else
    nsShortName := '';
  eltName := nsShortName + sENVELOPE;
  envNd := doc.DocumentElement;
  If Not SameText(eltName,envNd.NodeName) Then
    Error('XML root node must be "Envelope", found : "%s"',[envNd.NodeName + ':::' + nsShortName]);
  PushStack(envNd);

  bdyNd := envNd.FirstChild;
  if not Assigned(bdyNd) then
    Error('Node not found : "Body".');

  eltName := nsShortName + 'Body';
  if not SameText(bdyNd.NodeName,eltName) then begin
    eltName := nsShortName + 'Header';
    hdrNd := bdyNd;
    bdyNd := hdrNd.NextSibling;
    if SameText(hdrNd.NodeName,eltName) then begin
      PushStack(hdrNd,asScoped,'').SetNameSpace(sSOAP_ENV);
      ReadHeaders(ACallContext);
      PopStack().Free();
    end;
  end;

  eltName := nsShortName + 'Body';
  bdyNd := envNd.FirstChild;
  If Not Assigned(bdyNd) Then
    Error('Node not found : "Body"');
  If Not SameText(bdyNd.NodeName,eltName) Then
    bdyNd := bdyNd.NextSibling;
  If Not Assigned(bdyNd) Then
    Error('Node not found : "Body"');
  PushStack(bdyNd);
  If Not Assigned(bdyNd.FirstChild) Then
    Error('Response Node not found');
  if ( Style = RPC ) then begin
    PushStack(bdyNd.FirstChild);
  end;
  eltName := nsShortName + 'Fault';
  If SameText(eltName,bdyNd.FirstChild.NodeName) Then Begin
    oldStyle := Self.Style;
    Self.Style := RPC;
    try
      fltNd := bdyNd.FirstChild;
      PushStack(fltNd);
      excpt_Obj := ESOAPException.Create('');
      try
        eltName := 'faultcode';
        Get(TypeInfo(string),eltName,msgBuff);
        excpt_Obj.FaultCode := msgBuff;
        eltName := 'faultstring';
        Get(TypeInfo(string),eltName,msgBuff);
        excpt_Obj.FaultString := msgBuff;                   ;
        excpt_Obj.Message := Format(
          'Service exception :%s   Code = "%s"%s   Message = "%s"',
          [LineEnding,excpt_Obj.FaultCode,LineEnding,excpt_Obj.FaultString]
        );
      except
        FreeAndNil(excpt_Obj);
        raise;
      end;
      raise excpt_Obj;
    finally
      Self.Style := oldStyle;
    end;
  End;
end;

function TSOAPFormatter.GetCallProcedureName(): String;
begin
  Result := FCallProcedureName;
end;

function TSOAPFormatter.GetCallTarget(): String;
begin
  Result := FCallTarget;
end;

{ TSOAPCallMaker }

constructor TSOAPCallMaker.Create();
begin
  FUniqueAddress := True;
  FPropMngr := TPublishedPropertyManager.Create(Self);
end;

destructor TSOAPCallMaker.Destroy();
begin
  FPropMngr := Nil;
  inherited Destroy();
end;

function TSOAPCallMaker.GetPropertyManager(): IPropertyManager;
begin
  Result := FPropMngr;
end;

procedure TSOAPCallMaker.MakeCall(
  ASerializer : IFormatterClient;
  ATransport  : ITransport
);
Var
  rqt, rsps : TMemoryStream;
  propMngr : IPropertyManager;
  {$IFDEF WST_DBG}
  s : string;
  {$ENDIF WST_DBG}
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
  rsps := Nil;
  rqt := TMemoryStream.Create();
  Try
    rsps := TMemoryStream.Create();
    ASerializer.SaveToStream(rqt);
    rqt.Position := 0;
    ATransport.SendAndReceive(rqt,rsps);
    rqt.Clear();
    rsps.Position := 0;
    ASerializer.Clear();
    ASerializer.LoadFromStream(rsps);
  Finally
    rsps.Free();
    rqt.Free();
  End;
end;

procedure RegisterSoapProtocol();
begin
  RegisterStdTypes();
  GetFormaterRegistry().Register(
    sPROTOCOL_NAME,
    TSimpleItemFactory.Create(TSOAPFormatter),
    TSimpleItemFactory.Create(TSOAPCallMaker)
  );
end;

Initialization
  RegisterSoapProtocol();
  
end.
