unit wstmodule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, httpdefs, fphttp, server_service_imputils, websession,
  server_service_intf;

const
  sWSDL = 'WSDL';

Type
  { TCustomWSTModule }
  TWSTServiceEvent = procedure(Sender : TObject; Const AServiceName : String) of object;
  TServiceRequestEvent = procedure(Sender : TObject; Const AServiceName : String; Var Handled : Boolean)of object;
  TSessionPropertiesEvent = Procedure(Sender : TObject; const Props : TStrings) of object;

  TCustomWSTModule = Class(TSessionHTTPModule)
  private
    FAfterGenerateWSDL: TWSTServiceEvent;
    FAfterGenerateWSDLTable: TNotifyEvent;
    FAfterService: TWSTServiceEvent;
    FBeforeGenerateWSDL: TWSTServiceEvent;
    FBeforeGenerateWSDLTable: TNotifyEvent;
    FBeforeService: TServiceRequestEvent;
    FOnSetSessionProperties: TSessionPropertiesEvent;
    FRequest: TRequest;
    FResponse: TResponse;
    function GenerateWSDLTable(Const ABaseURL : String): string;
    procedure ProcessServiceRequest(ARequest: TRequest; AResponse: TResponse; AFormat : String);
    procedure ProcessWSDLRequest(ARequest: TRequest; AResponse: TResponse);
  Protected
    procedure AddSessionProperties(ARequest: TRequest; WSTRequest: IRequestBuffer); virtual;
    property BeforeGenerateWSDLTable : TNotifyEvent Read FBeforeGenerateWSDLTable Write FBeforeGenerateWSDLTable;
    property AfterGenerateWSDLTable : TNotifyEvent Read FAfterGenerateWSDLTable Write FAfterGenerateWSDLTable;
    property BeforeGenerateWSDL : TWSTServiceEvent Read FBeforeGenerateWSDL Write FBeforeGenerateWSDL;
    property AfterGenerateWSDL : TWSTServiceEvent Read FAfterGenerateWSDL Write FAfterGenerateWSDL;
    Property BeforeServiceRequest : TServiceRequestEvent Read FBeforeService Write FBeforeService;
    Property AfterServiceRequest : TWSTServiceEvent Read FAfterService Write FAfterService;
    Property OnSetSessionProperties : TSessionPropertiesEvent Read FOnSetSessionProperties Write FOnSetSessionProperties;
    // Access to request
    Property Request: TRequest Read FRequest;
    // Access to response
    Property Response: TResponse Read FResponse;
  Public
    Constructor CreateNew(AOwner : TComponent; CreateMode : Integer); override;
    Procedure HandleRequest(ARequest : TRequest; AResponse : TResponse); override;
  end;

  TWSTModule = Class(TCustomWSTModule)
  Public
    Property Request;
    Property Response;
  Published
    property BeforeGenerateWSDLTable;
    property AfterGenerateWSDLTable;
    property BeforeGenerateWSDL;
    property AfterGenerateWSDL;
    Property BeforeServiceRequest;
    Property AfterServiceRequest;
  end;

implementation

{ $define wmdebug}

uses {$ifdef wmdebug}dbugintf,{$endif}base_service_intf,
     metadata_repository, metadata_wsdl, dom,xmlwrite,
      metadata_service, metadata_service_binder;

function GetWSDL(const ARepName, ARootAddress: shortstring):string;
var
  M : TStringStream;
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
  s := GetModuleMetadataMngr().GetRepositoryName(i);
  GetModuleMetadataMngr().LoadRepositoryName(s,ARootAddress,rep);
  doc := TXMLDocument.Create();
  try
    GenerateWSDL(rep,doc);
    M := TStringStream.Create('');
    try
      WriteXMLFile(doc,M);
      Result:=M.DataString;
    finally
      M.Free();
      GetModuleMetadataMngr().ClearRepository(rep);
    end;
  finally
    Doc.free;
  end;
end;

Const
  STableStart='<html><head><title>%s</title></head><body>' +
              '<p BGCOLOR="#DDEEFF"><FONT FACE="Arial" COLOR="#0000A0" SIZE="+2">%s</FONT></p>'+
              '<table width="100%%">';
  STableRow = '<tr><td><a href="%s%s">%s</a></td></tr>';
  STableEnd = '</tr></table></body></html>';


Resourcestring
  SWSDLTablePageTitle = 'Available services';
  SWSDLTableIntro = 'The following services are available. Click on the link to view the corresponding WSDL.';

function TCustomWSTModule.GenerateWSDLTable(Const ABaseURL : String): string;

var
  r : IModuleMetadataMngr;
  i : Integer;
  N : String;

begin
  If Assigned(FBeforeGenerateWSDLTable) then
    FBeforeGenerateWSDLTable(Self);
  r := GetModuleMetadataMngr();
  Result := Format(STableStart,[SWSDLTablePageTitle,SWSDLTableIntro]);
  for i := 0 to R.GetCount-1 do
    begin
    N:=r.GetRepositoryName(i);
    Result:=Result+Format(STableRow,[ABaseURL,N,N]);
    end;
  Result:=Result+STableEnd;
  If Assigned(FAfterGenerateWSDLTable) then
    FAfterGenerateWSDLTable(Self);
end;


procedure TCustomWSTModule.ProcessWSDLRequest(ARequest : TRequest; AResponse : TResponse);

var
  ServiceName,Buf : string;

begin
{$ifdef wmdebug}SendDebug('Entering ProcessWSDLRequest');{$endif}
{$ifdef wmdebug}SendDebug(Format('ProcessWSDLSRequest: BaseURL = "%s"',[BaseURL]));{$endif}
  ServiceName:=ARequest.GetNextPathInfo;
{$ifdef wmdebug}SendDebug(Format('ProcessWSDLSRequest: Servicename = "%s"',[ServiceName]));{$endif}
  If (ServiceName<>'') then
    begin
    If Assigned(FBeforeGenerateWSDL) then
      FBeforeGenerateWSDL(Self,ServiceName);
    Buf:=GetWSDL(ServiceName,ARequest.URI);
    If Assigned(FAfterGenerateWSDL) then
      FAfterGenerateWSDL(Self,ServiceName);
    end;
  if (Length(Buf)>0) then
    begin
    AResponse.ContentType:='text/xml';
    AResponse.Content:= Buf;
    end
  else
    begin
    AResponse.Content := GenerateWSDLTable(BaseURL+'WSDL/');
    AResponse.ContentType := 'text/html';
    end;
{$ifdef wmdebug}SendDebug('Exiting ProcessWSDLRequest');{$endif}
end;

Procedure TCustomWSTModule.AddSessionProperties(ARequest : TRequest; WSTRequest : IRequestBuffer);

Var
  P : IPropertyManager;
  L : TStringList;
  I : Integer;
  N,V : String;

begin
  P:=WSTRequest.GetPropertyManager();
  If CreateSession and Assigned(Session) then
    P.SetProperty('SessionID',Self.Session.SessionID);
  P.SetProperty(SRemote_IP,ARequest.RemoteAddress);
  P.SetProperty('RemoteHost',ARequest.RemoteHost);
  If Assigned(FOnSetSessionProperties) then
    begin
    L:=TStringList.Create;
    try
      FOnSetSessionProperties(Self,L);
      For I:=0 to L.Count-1 do
        begin
        L.GetNameValue(I,N,V);
        P.SetProperty(N,V);
        end;
    finally
      L.Free;
    end;
    end;
end;

Procedure TCustomWSTModule.ProcessServiceRequest(ARequest : TRequest; AResponse : TResponse; AFormat : String);
var
  ServiceName,ContentType : string;
  rqst : IRequestBuffer;
  inStream : TStringStream;
  outStream : TMemoryStream;
  B : Boolean;
begin
{$ifdef wmdebug}SendDebug('Entering ProcessServiceRequest');{$endif}
  ServiceName:=ARequest.GetNextPathInfo;
{$ifdef wmdebug}SendDebug(Format('ProcessServiceRequest: Servicename = "%s"',[ServiceName]));{$endif}
  B:=False;
  If Assigned(FBeforeService) then
    FBeforeService(Self,ServiceName,B);
  If Not B then
    begin
    inStream := TStringStream.Create(ARequest.Content);
    try
      outStream := TMemoryStream.Create();
      try
        ContentType:= ARequest.ContentType;
        Response.ContentType := ContentType;
        rqst := TRequestBuffer.Create(ServiceName,ContentType,inStream,outStream,AFormat);
        AddSessionProperties(ARequest,Rqst);
       {$ifdef wmdebug}SendDebug('Handling request');{$endif}
        HandleServiceRequest(rqst);
       {$ifdef wmdebug}SendDebug('Handled request');{$endif}
        AResponse.ContentStream := outStream;
       AResponse.SendContent();
       AResponse.ContentStream := nil;
      finally
        OutStream.Free;
      end;
    finally
      InStream.Free();
    end;
    end;
  If Assigned(FAfterService) then
    FAfterService(Self,ServiceName);
{$ifdef wmdebug}SendDebug('Exiting ProcessServiceRequest');{$endif}
end;


{ TCustomWSTModule }

constructor TCustomWSTModule.CreateNew(AOwner: TComponent; CreateMode: Integer
  );
begin
  inherited CreateNew(AOwner, CreateMode);
end;

procedure TCustomWSTModule.HandleRequest(ARequest: TRequest;
  AResponse: TResponse);

Var
  AFormat : String;
begin
{$ifdef wmdebug}SendDebug('Entering HandleRequest');{$endif}
  try
    FRequest:=ARequest;
    FResponse:=AResponse;
    try
      AFormat:=Request.GetNextPathInfo;
     {$ifdef wmdebug}SendDebug(Format('HandleRequest: Format = "%s"',[AFormat]));{$endif}
      If SameText(AFormat,SWSDL) or (AFormat='') then
        ProcessWSDLRequest(ARequest,AResponse)
      else
        ProcessServiceRequest(ARequest,AResponse,AFormat);
    finally
      FRequest:=Nil;
      FResponse:=Nil;
    end;
  except
    On E : Exception do
      begin
       {$ifdef wmdebug}SendDebug('Error during HandleRequest : '+E.Message);{$endif}
        raise;
      end;
  end;
{$ifdef wmdebug}SendDebug('Exiting HandleRequest');{$endif}
end;


end.