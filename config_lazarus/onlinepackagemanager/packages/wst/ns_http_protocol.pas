{
    This file is part of the Web Service Toolkit
    Copyright (c) 2006 by Inoussa OUEDRAOGO

    This file is provided under modified LGPL licence
    (the files COPYING.modifiedLGPL and COPYING.LGPL).

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{
  NOTE: This unit adds support to WST for iOS and Free Pascal on
   iOS devices and iOS Simulator. It requires the ns_url_request
   and NSHelpers units, available here:

     http://web.me.com/macpgmr/ObjP/Xcode4/ns_url_request.pas
     http://web.me.com/macpgmr/ObjP/Xcode4/NSHelpers.pas

   These two units require the Free Pascal iPhoneAll unit that
   provides access to iOS Foundation classes.
}

unit ns_http_protocol;

{$INCLUDE wst_global.inc}

//{$DEFINE WST_DBG}

interface

uses
  Classes, SysUtils,
  wst_types, service_intf, imp_utils, base_service_intf, client_utils,
  ns_url_request;

const
  sTRANSPORT_NAME = 'HTTP';

type

  { TNSCookieManager }

  TNSCookieManager = class(TInterfacedObject, ICookieManager)
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
  
  { THTTPTransport }

  THTTPTransport = class(TBaseTransport, ITransport)
  private
    FAddress : string;
    FFormat : string;
    FCookieManager : ICookieManager;
    Headers : TStringList;
    Cookies : TStringList;
  private  
    function GetAddress: string;
    function GetContentType: string;
    function GetSoapAction : string;
    procedure SetAddress(const AValue: string);
    procedure SetContentType(const AValue: string);
    procedure SetSoapAction(const AValue : string);
  protected
    procedure DoSendAndReceive(ARequest, AResponse: TStream); override;
  public
    constructor Create();override;
    destructor Destroy();override;      
    function GetTransportName() : string; override;
    function GetCookieManager() : ICookieManager; override;
  published
    property ContentType : string Read GetContentType Write SetContentType;
    property Address : string Read GetAddress Write SetAddress;
    property SoapAction : string read GetSoapAction write SetSoapAction;
    property Format : string read FFormat write FFormat;
  end;

  procedure NS_RegisterHTTP_Transport();


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
  Result := Headers.Values['Content-type'];
end;

function THTTPTransport.GetSoapAction : string;
begin
  Result := Headers.Values[s_soapAction_Header];
end;

procedure THTTPTransport.SetAddress(const AValue: string);
begin
  FAddress := AValue;
end;

procedure THTTPTransport.SetContentType(const AValue: string);
begin
  Headers.Add('Content-type=' + AValue);
end;

procedure THTTPTransport.SetSoapAction(const AValue : string);
begin
  Headers.Add(s_soapAction_Header + '=' + AValue);
end;

procedure THTTPTransport.DoSendAndReceive(ARequest, AResponse: TStream);
var
  aNSHTTP : TNSHTTPSendAndReceive;
begin
  aNSHTTP := TNSHTTPSendAndReceive.Create;
  try
    aNSHTTP.Method := 'POST';
    aNSHTTP.Address := Address;
    if not aNSHTTP.SendAndReceive(ARequest, AResponse, Headers) then
      raise ETransportExecption.CreateFmt(SERR_FailedTransportRequest,
                                          [sTRANSPORT_NAME, Address]);
  finally
    aNSHTTP.Free;
  end;
end;

constructor THTTPTransport.Create();
begin
  inherited Create();
  Headers := TStringList.Create;
end;

destructor THTTPTransport.Destroy();
begin
  Headers.Free;
  Cookies.Free;  //?
  inherited Destroy();
end;

function THTTPTransport.GetTransportName() : string;  
begin
  Result := sTRANSPORT_NAME;
end;

function THTTPTransport.GetCookieManager() : ICookieManager; 
begin
  Cookies := TStringList.Create;  //?
  if (FCookieManager=nil) then
    FCookieManager:=TNSCookieManager.Create(Cookies);
  Result:=FCookieManager;
end;


procedure NS_RegisterHTTP_Transport();
begin
  GetTransportRegistry().Register(sTRANSPORT_NAME,TSimpleItemFactory.Create(THTTPTransport));
end;


{ TNSCookieManager }

function TNSCookieManager.GetCount() : Integer; 
begin
  Result := ReferencedObject.Count;
end;

function TNSCookieManager.GetName(const AIndex : Integer) : string; 
begin
  Result := ReferencedObject.Names[AIndex];
end;

function TNSCookieManager.GetValue(const AIndex : Integer) : string; 
begin
  Result := ReferencedObject.ValueFromIndex[AIndex];
end;

function TNSCookieManager.GetValue(const AName : string) : string; 
begin
  Result := ReferencedObject.Values[AName];
end;

procedure TNSCookieManager.SetValue(
  const AIndex : Integer;  
  const AValue : string); 
begin
  ReferencedObject.ValueFromIndex[AIndex] := AValue;
end;

procedure TNSCookieManager.SetValue(
  const AName : string;  
  const AValue : string); 
begin
  ReferencedObject.Values[AName] := AValue;
end;

constructor TNSCookieManager.Create(AReferencedObject : TStrings); 
begin
  if (AReferencedObject = nil) then
    raise ETransportExecption.CreateFmt(SERR_InvalidParameter,['AReferencedObject']); 
  FReferencedObject := AReferencedObject;
end;


end.
