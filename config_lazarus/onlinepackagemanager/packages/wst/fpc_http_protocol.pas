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
unit fpc_http_protocol;

//{$DEFINE WST_DBG}

interface

uses
  Classes, SysUtils,
  wst_types, service_intf, imp_utils, base_service_intf, client_utils,
  fphttpclient;

Const
  sTRANSPORT_NAME = 'HTTP';

Type

  { TFPCCookieManager }

  TFPCCookieManager = class(TInterfacedObject,ICookieManager)
  private
    FReferencedObject : TStrings;
  protected
    property ReferencedObject : TStrings read FReferencedObject;
  protected  
    function GetCount() : Integer;
    function GetName(const AIndex : Integer) : string;
    function GetValue(const AIndex : Integer) : string; overload;
    function GetValue(const AName : string) : string; overload;
    procedure SetValue(const AIndex : Integer; const AValue : string); overload;
    procedure SetValue(const AName : string; const AValue : string); overload;
  public
    constructor Create(AReferencedObject : TStrings);
  end;
  
{$M+}
  { THTTPTransport }
  THTTPTransport = class(TBaseTransport,ITransport)
  Private
    FConnection : TFPHTTPClient;
    FAddress : string;
    FFormat : string;
    FCookieManager : ICookieManager;
  private  
    function GetAddress: string;
    function GetContentType: string;
    function GetSoapAction : string;
    procedure SetAddress(const AValue: string);
    procedure SetContentType(const AValue: string);
    procedure DoSendAndReceive(ARequest,AResponse:TStream); override;
    procedure SetSoapAction(const AValue : string);
  Public
    constructor Create();override;
    destructor Destroy();override;      
    function GetTransportName() : string; override;
    function GetCookieManager() : ICookieManager; override;
  Published
    property ContentType : string Read GetContentType Write SetContentType;
    property Address : string Read GetAddress Write SetAddress;
    property SoapAction : string read GetSoapAction write SetSoapAction;
    property Format : string read FFormat write FFormat;
  End;
{$M+}

  procedure FPC_RegisterHTTP_Transport();

implementation
uses
  wst_consts;

const
  s_soapAction_Header = 'soapAction';

{ THTTPTransport }

function THTTPTransport.GetAddress: string;
begin
  Result := FAddress;
end;

function THTTPTransport.GetContentType: string;
begin
  Result := FConnection.GetHeader('Content-type');
end;

function THTTPTransport.GetSoapAction : string;
begin
  Result := FConnection.GetHeader(s_soapAction_Header);
end;

procedure THTTPTransport.SetAddress(const AValue: string);
begin
  FAddress := AValue;
end;

procedure THTTPTransport.SetContentType(const AValue: string);
begin
  FConnection.AddHeader('Content-type',AValue);
end;

constructor THTTPTransport.Create();
begin
  inherited Create();
  FConnection:=TFPHTTPClient.Create(Nil);
  FConnection.HTTPVersion := '1.1';
end;

destructor THTTPTransport.Destroy();
begin
  FreeAndNil(FConnection);
  inherited Destroy();
end;

function THTTPTransport.GetTransportName() : string;  
begin
  Result := sTRANSPORT_NAME;
end;

procedure THTTPTransport.DoSendAndReceive(ARequest, AResponse: TStream);
var
  EMsg : String;
  i : Integer;
begin
  try
    ARequest.position:=0;
    FConnection.RequestBody:=ARequest;
    i := FConnection.IndexOfHeader('Content-length');
    if (i >= 0) then
      FConnection.RequestHeaders.Delete(i);
    FConnection.Post(FAddress,AResponse);
  except
    On E : Exception do
    EMsg:=E.Message;
  end;
  if (EMsg<>'') then
    raise ETransportExecption.CreateFmt(SERR_FailedTransportRequest,[sTRANSPORT_NAME,FAddress]);
end;

procedure THTTPTransport.SetSoapAction(const AValue : string);
begin
  FConnection.AddHeader(s_soapAction_Header,AValue);
end;

function THTTPTransport.GetCookieManager() : ICookieManager; 
begin
  if (FCookieManager=nil) then
    FCookieManager:=TFPCCookieManager.Create(FConnection.Cookies);
  Result:=FCookieManager;
end;

procedure FPC_RegisterHTTP_Transport();
begin
  GetTransportRegistry().Register(sTRANSPORT_NAME,TSimpleItemFactory.Create(THTTPTransport));
end;

{ TFPCCookieManager }

function TFPCCookieManager.GetCount() : Integer; 
begin
  Result := ReferencedObject.Count;
end;

function TFPCCookieManager.GetName(const AIndex : Integer) : string; 
begin
  Result := ReferencedObject.Names[AIndex];
end;

function TFPCCookieManager.GetValue(const AIndex : Integer) : string; 
begin
  Result := ReferencedObject.ValueFromIndex[AIndex];
end;

function TFPCCookieManager.GetValue(const AName : string) : string; 
begin
  Result := ReferencedObject.Values[AName];
end;

procedure TFPCCookieManager.SetValue(
  const AIndex : Integer;  
  const AValue : string
); 
begin
  ReferencedObject.ValueFromIndex[AIndex] := AValue;
end;

procedure TFPCCookieManager.SetValue(
  const AName : string;  
  const AValue : string
); 
begin
  ReferencedObject.Values[AName] := AValue;
end;

constructor TFPCCookieManager.Create(AReferencedObject : TStrings); 
begin
  if (AReferencedObject = nil) then
    raise ETransportExecption.CreateFmt(SERR_InvalidParameter,['AReferencedObject']); 
  FReferencedObject := AReferencedObject;
end;

end.
