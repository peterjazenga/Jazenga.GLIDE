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
unit ics_http_protocol;

//{$DEFINE WST_DBG}

interface

uses
  Classes, SysUtils, {$IFDEF WST_DBG}Dialogs,{$ENDIF}
  service_intf, imp_utils, base_service_intf,
  HttpProt;

Const
  sTRANSPORT_NAME = 'HTTP';

Type

{$M+}
  { THTTPTransport }
  THTTPTransport = class(TSimpleFactoryItem,ITransport)
  Private
    FPropMngr : IPropertyManager;
    FConnection : THttpCli;
    FSoapAction: string;
    function GetAddress: string;
    function GetContentType: string;
    function GetProxyPassword: string;
    function GetProxyPort: Integer;
    function GetProxyServer: string;
    function GetProxyUsername: string;
    procedure SetAddress(const AValue: string);
    procedure SetContentType(const AValue: string);
    procedure SetProxyPassword(const AValue: string);
    procedure SetProxyPort(const AValue: Integer);
    procedure SetProxyServer(const AValue: string);
    procedure SetProxyUsername(const AValue: string);
  private
    FFormat : string;
    procedure HttpBeforeHeaderSendHandler(
            Sender   : TObject;
      const Method   : String;
            Headers  : TStrings
    );
  Public
    constructor Create();override;
    destructor Destroy();override;
    function GetPropertyManager():IPropertyManager;
    procedure SendAndReceive(ARequest,AResponse:TStream);
  Published
    property ContentType : string Read GetContentType Write SetContentType;
    property Address : string Read GetAddress Write SetAddress;
    property ProxyServer : string Read GetProxyServer Write SetProxyServer;
    property ProxyPort : Integer Read GetProxyPort Write SetProxyPort;
    property ProxyUsername : string read GetProxyUsername write SetProxyUsername;
    property ProxyPassword : string read GetProxyPassword write SetProxyPassword;
    property SoapAction : string read FSoapAction write FSoapAction;
    property Format : string read FFormat write FFormat;
  End;
{$M+}

  procedure ICS_RegisterHTTP_Transport();
  
implementation

{ THTTPTransport }

function THTTPTransport.GetAddress: string;
begin
  Result := FConnection.URL;
end;

function THTTPTransport.GetContentType: string;
begin
  Result := FConnection.ContentTypePost;
end;

function THTTPTransport.GetProxyPassword: string;
begin
  Result := FConnection.ProxyPassword;
end;

function THTTPTransport.GetProxyPort: Integer;
begin
  Result := StrToIntDef(FConnection.ProxyPort,0);
end;

function THTTPTransport.GetProxyServer: string;
begin
  Result := FConnection.Proxy;
end;

function THTTPTransport.GetProxyUsername: string;
begin
  Result := FConnection.ProxyUsername;
end;

procedure THTTPTransport.SetAddress(const AValue: string);
begin
  FConnection.URL := AValue;
end;

procedure THTTPTransport.SetContentType(const AValue: string);
begin
  FConnection.ContentTypePost := AValue;
end;

procedure THTTPTransport.SetProxyPassword(const AValue: string);
begin
  FConnection.ProxyPassword := AValue;
end;

procedure THTTPTransport.SetProxyPort(const AValue: Integer);
begin
  FConnection.ProxyPort := IntToStr(AValue);
end;

procedure THTTPTransport.SetProxyServer(const AValue: string);
begin
  FConnection.Proxy := AValue;
end;

procedure THTTPTransport.SetProxyUsername(const AValue: string);
begin
  FConnection.ProxyUsername := AValue;
end;

procedure THTTPTransport.HttpBeforeHeaderSendHandler(
        Sender: TObject;
  const Method: String;
        Headers: TStrings
);
begin
  Headers.Add('soapAction:' + SoapAction);
end;

constructor THTTPTransport.Create();
begin
  FPropMngr := TPublishedPropertyManager.Create(Self);
  FConnection := THttpCli.Create(Nil);
end;

destructor THTTPTransport.Destroy();
begin
  FreeAndNil(FConnection);
  FPropMngr := Nil;
  inherited Destroy();
end;

function THTTPTransport.GetPropertyManager(): IPropertyManager;
begin
  Result := FPropMngr;
end;

procedure THTTPTransport.SendAndReceive(ARequest, AResponse: TStream);
{$IFDEF WST_DBG}
Var
  s : TBinaryString;
  i : Int64;
{$ENDIF WST_DBG}
begin
  {$IFDEF WST_DBG}
  i := ARequest.Position;
  ARequest.Position := 0;
  SetLength(s,ARequest.Size);
  ARequest.Read(s[1],ARequest.Size);
  TMemoryStream(AResponse).SaveToFile('request.log');
  ARequest.Position := i;
  {$ENDIF WST_DBG}

  FConnection.SendStream := ARequest;
  FConnection.RcvdStream := AResponse;
  FConnection.Post();

  {$IFDEF WST_DBG}
  TMemoryStream(AResponse).SaveToFile('request.log');
  i := AResponse.Position;
  SetLength(s,AResponse.Size);
  AResponse.Read(s[1],AResponse.Size);
  TMemoryStream(AResponse).SaveToFile('response.log');
  if IsConsole then
    WriteLn(s)
  else
    ShowMessage(s);
  {$ENDIF WST_DBG}
end;

procedure ICS_RegisterHTTP_Transport();
begin
  GetTransportRegistry().Register(sTRANSPORT_NAME,TSimpleItemFactory.Create(THTTPTransport));
end;

end.
