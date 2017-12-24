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
unit synapse_http_protocol;

//{$DEFINE WST_DBG}

interface

uses
  Classes, SysUtils,
  service_intf, base_service_intf, client_utils,
  httpsend;

Const
  sTRANSPORT_NAME = 'HTTP';

Type

  { TSynapseCookieManager }

  TSynapseCookieManager = class(TInterfacedObject,ICookieManager)
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
    FConnection : THTTPSend;
    FAddress : string;
    FFormat : string;
    FCookieManager : ICookieManager;
  private
    function GetConnectTimeout : Integer;
    function GetReadTimeout : Integer;
    function IndexOfHeader(const AHeader : string) :Integer;
    function GetAddress: string;
    function GetContentType: string;
    function GetProxyPassword: string;
    function GetProxyPort: Integer;
    function GetProxyServer: string;
    function GetProxyUsername: string;
    function GetSoapAction : string;
    procedure SetAddress(const AValue: string);
    procedure SetConnectTimeout(AValue : Integer);
    procedure SetContentType(const AValue: string);
    procedure SetProxyPassword(const AValue: string);
    procedure SetProxyPort(const AValue: Integer);
    procedure SetProxyServer(const AValue: string);
    procedure SetProxyUsername(const AValue: string);
    procedure SetReadTimeout(AValue : Integer);
    procedure SetSoapAction(const AValue : string);
  protected
    procedure DoSendAndReceive(ARequest,AResponse:TStream); override;
  Public
    constructor Create();override;
    destructor Destroy();override;      
    function GetTransportName() : string; override;
    function GetCookieManager() : ICookieManager; override;
  Published
    property ContentType : string Read GetContentType Write SetContentType;
    property Address : string Read GetAddress Write SetAddress;
    property ProxyServer : string Read GetProxyServer Write SetProxyServer;
    property ProxyPort : Integer Read GetProxyPort Write SetProxyPort;
    property ProxyUsername : string read GetProxyUsername write SetProxyUsername;
    property ProxyPassword : string read GetProxyPassword write SetProxyPassword;
    property SoapAction : string read GetSoapAction write SetSoapAction;
    property Format : string read FFormat write FFormat;
    property ConnectTimeout: Integer read GetConnectTimeout write SetConnectTimeout;
    property ReadTimeout: Integer read GetReadTimeout write SetReadTimeout;
  End;
{$M+}

  procedure SYNAPSE_RegisterHTTP_Transport();

implementation
uses
  wst_consts;

const
  s_soapAction_Header = 'soapAction:';

{ THTTPTransport }

function THTTPTransport.GetConnectTimeout : Integer;
begin
  Result := FConnection.Sock.ConnectionTimeout;
end;

function THTTPTransport.GetReadTimeout : Integer;
begin
  Result := FConnection.Timeout;
end;

function THTTPTransport.IndexOfHeader(const AHeader : string) : Integer;
var
  i : Integer;
  locList : TStringList;
  s : string;
begin
  Result := -1;
  locList := FConnection.Headers;
  if (locList.Count > 0) then begin
    s := LowerCase(AHeader);
    for i := 0 to locList.Count - 1 do
      if (Pos(s,LowerCase(locList[i])) = 1) then begin
        Result := i;
        Break;
      end;
  end;
end;

function THTTPTransport.GetAddress: string;
begin
  Result := FAddress;
end;

function THTTPTransport.GetContentType: string;
begin
  Result := FConnection.MimeType;
end;

function THTTPTransport.GetProxyPassword: string;
begin
  Result := FConnection.ProxyPass;
end;

function THTTPTransport.GetProxyPort: Integer;
begin
  Result := StrToInt(FConnection.ProxyPort);
end;

function THTTPTransport.GetProxyServer: string;
begin
  Result := FConnection.ProxyHost;
end;

function THTTPTransport.GetProxyUsername: string;
begin
  Result := FConnection.ProxyUser;
end;

function THTTPTransport.GetSoapAction : string;
var
  i : Integer;
begin
  i := IndexOfHeader(s_soapAction_Header);
  if (i >= 0) then begin
    Result := FConnection.Headers[i];
    Result := Copy(Result,(Length(s_soapAction_Header)+1),Length(Result));
  end else begin
    Result := '';
  end;
end;

procedure THTTPTransport.SetAddress(const AValue: string);
begin
  FAddress := AValue;
end;

procedure THTTPTransport.SetConnectTimeout(AValue : Integer);
begin
  FConnection.Sock.ConnectionTimeout := AValue;
end;

procedure THTTPTransport.SetContentType(const AValue: string);
begin
  FConnection.MimeType := AValue;
end;

procedure THTTPTransport.SetProxyPassword(const AValue: string);
begin
  FConnection.ProxyPass := AValue;
end;

procedure THTTPTransport.SetProxyPort(const AValue: Integer);
begin
  FConnection.ProxyPort := IntToStr(AValue);
end;

procedure THTTPTransport.SetProxyServer(const AValue: string);
begin
  FConnection.ProxyHost := AValue;
end;

procedure THTTPTransport.SetProxyUsername(const AValue: string);
begin
  FConnection.ProxyUser := AValue;
end;

procedure THTTPTransport.SetReadTimeout(AValue : Integer);
begin
  FConnection.Timeout := AValue;
end;

procedure THTTPTransport.SetSoapAction(const AValue : string);
var
  i : Integer;
  s : string;
begin
  i := IndexOfHeader(s_soapAction_Header);
  s := s_soapAction_Header + AValue;
  if (i >= 0) then
    FConnection.Headers[i] := s
  else
    FConnection.Headers.Insert(0,s);
end;

procedure THTTPTransport.DoSendAndReceive(ARequest, AResponse : TStream);
var
  s, s2 : string;
  ans : AnsiString;
  e : ETransportExecption;
begin
  FConnection.Document.Clear();
  FConnection.Document.CopyFrom(ARequest,0);
  if not FConnection.HTTPMethod('POST',FAddress) then begin
    s := sysutils.Format(SERR_FailedTransportRequest,[sTRANSPORT_NAME,FAddress]);
    s := s+sysutils.Format('Result code: %d, message: "%s"',[FConnection.ResultCode,Fconnection.ResultString]);
    s2 := '';
    if (FConnection.Document.Size > 0) then begin
      SetLength(ans,FConnection.Document.Size);
      Move(FConnection.Document.Memory^,ans[1],FConnection.Document.Size);
      try
        s2 := ans;
      except
        s2 := '';
      end;
    end;
    e := ETransportExecption.Create(s);
    e.ExtendedErrorInfo := s2;
    raise e;
  end;
  AResponse.CopyFrom(FConnection.Document,0);
  FConnection.Document.Clear();
  FConnection.Headers.Clear();
end;

constructor THTTPTransport.Create();
begin
  inherited Create();
  FConnection := THTTPSend.Create();
  FConnection.Protocol := '1.1';
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

function THTTPTransport.GetCookieManager() : ICookieManager;
begin
  if (FCookieManager = nil) then
    FCookieManager := TSynapseCookieManager.Create(FConnection.Cookies);
  Result := FCookieManager;
end;

procedure SYNAPSE_RegisterHTTP_Transport();
begin
  GetTransportRegistry().Register(sTRANSPORT_NAME,TSimpleItemFactory.Create(THTTPTransport));
end;

{ TSynapseCookieManager }

function TSynapseCookieManager.GetCount() : Integer; 
begin
  Result := ReferencedObject.Count;
end;

function TSynapseCookieManager.GetName(const AIndex : Integer) : string; 
begin
  Result := ReferencedObject.Names[AIndex];
end;

function TSynapseCookieManager.GetValue(const AIndex : Integer) : string; 
begin
  Result := ReferencedObject.ValueFromIndex[AIndex];
end;

function TSynapseCookieManager.GetValue(const AName : string) : string; 
begin
  Result := ReferencedObject.Values[AName];
end;

procedure TSynapseCookieManager.SetValue(
  const AIndex : Integer;  
  const AValue : string
); 
begin
  ReferencedObject.ValueFromIndex[AIndex] := AValue;
end;

procedure TSynapseCookieManager.SetValue(
  const AName : string;  
  const AValue : string
); 
begin
  ReferencedObject.Values[AName] := AValue;
end;

constructor TSynapseCookieManager.Create(AReferencedObject : TStrings); 
begin
  if (AReferencedObject = nil) then
    raise ETransportExecption.CreateFmt(SERR_InvalidParameter,['AReferencedObject']); 
  FReferencedObject := AReferencedObject;
end;

end.
