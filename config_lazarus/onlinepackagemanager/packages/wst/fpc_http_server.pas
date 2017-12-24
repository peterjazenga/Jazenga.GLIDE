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

unit fpc_http_server;

interface

uses
  Classes, SysUtils, httpdefs, fphttpserver, server_listener, wst_types;

type

  IObjectRef = interface
    ['{B62EC733-999D-4DEC-A69F-B7546A16F661}']
    function GetObject() : TObject;
  end;

  { TFPWorkerObject }

  TFPWorkerObject = class(TInterfacedObject,IObjectRef)
  private
    FHTTPServerObject: TFPHTTPServer;
    FRootAddress : string;
    FServerSoftware : String;
    FOnNotifyMessage : TListnerNotifyMessage;
  private
    function GetHandleRequestInThread : Boolean;
    function GetListeningPort : Integer;
    procedure SetHandleRequestInThread(const AValue : Boolean);
    procedure SetListeningPort(const AValue : Integer);

    procedure ProcessWSDLRequest(
          ARequest  : TRequest;
          AResponse : TResponse;
      var APath     : string
    );
    procedure ProcessServiceRequest(
          ARequest  : TRequest;
          AResponse : TResponse;
      var APath     : string
    );
  private
    procedure RequestHandler(
          Sender    : TObject;
      Var ARequest  : TFPHTTPConnectionRequest;
      Var AResponse : TFPHTTPConnectionResponse
    );
  protected
    function GetObject() : TObject;
  public
    constructor Create();
    destructor Destroy(); override;
    procedure Start();
    procedure Stop();
    function IsActive : Boolean;

    property RootAddress : string read FRootAddress write FRootAddress;
    property ServerSoftware : string read FServerSoftware write FServerSoftware;
    property ListeningPort : Integer read GetListeningPort write SetListeningPort;
    property OnNotifyMessage : TListnerNotifyMessage read FOnNotifyMessage write FOnNotifyMessage;
    property HandleRequestInThread : Boolean read GetHandleRequestInThread write SetHandleRequestInThread;
  end;

  { TServerListnerThread }

  TServerListnerThread = class(TThread)
  private
    FWorkerObject : IObjectRef;
  public
    constructor Create(AWorkerObject : TFPWorkerObject);
    procedure Execute(); override;
  end;

  TListenerOption = (loExecuteInThread, loHandleRequestInThread);
  TListenerOptions = set of TListenerOption;

  { TwstFPHttpListener }

  TwstFPHttpListener = class(TwstListener)
  private
    FOptions : TListenerOptions;
    FWorkerObjectRef : IObjectRef;
    FWorkerObject : TFPWorkerObject;
  protected
    procedure SetOnNotifyMessage(const AValue : TListnerNotifyMessage);override;
  public
    constructor Create(
      const AServerIpAddress   : string  = '127.0.0.1';
      const AListningPort      : Integer = 8000;
      const ADefaultClientPort : Integer = 25000;
      const AServerSoftware    : string  = 'Web Service Toolkit Application'
    );
    destructor Destroy(); override;
    class function GetDescription() : string;override;
    procedure Start();override;
    procedure Stop();override;
    function IsActive : Boolean; override;

    property Options : TListenerOptions read FOptions write FOptions;
  end;

implementation
uses
  wst_consts,
  base_service_intf, server_service_intf, server_service_imputils, metadata_wsdl;

{$IFDEF WST_DBG}
procedure Display(const AMsg : string);
begin
  if IsConsole then
    WriteLn(AMsg);
end;
{$ENDIF}

function ExtractNextPathElement(var AFullPath : string):string;
var
  i : SizeInt;
begin
  Result := '';
  if ( Length(AFullPath) > 0 ) then begin
    while ( Length(AFullPath) > 0 ) and ( AFullPath[1] = sSEPARATOR ) do begin
      Delete(AFullPath,1,1);
    end;
    i := Pos(sSEPARATOR,AFullPath);
    if ( i < 1 ) then begin
      Result := AFullPath;
      AFullPath := '';
    end else begin
      Result := Copy(AFullPath,1,Pred(i));
      Delete(AFullPath,1,i);
    end;
  end;
end;

{ TServerListnerThread }

constructor TServerListnerThread.Create(AWorkerObject : TFPWorkerObject);
begin
  FreeOnTerminate := True;
  FWorkerObject := AWorkerObject;
  inherited Create(False);
end;

procedure TServerListnerThread.Execute();
var
  locObject : TFPWorkerObject;
begin
  try
    locObject := TFPWorkerObject(FWorkerObject.GetObject());
    locObject.Start();
  except
  end;
end;

{ TFPWorkerObject }

procedure TFPWorkerObject.ProcessWSDLRequest(
      ARequest  : TRequest;
      AResponse : TResponse;
  var APath     : string
);
var
  locRepName, strBuff : string;
  i : Integer;
begin
  locRepName := ExtractNextPathElement(APath);
  if AnsiSameText(sWSDL,locRepName) then
    locRepName := ExtractNextPathElement(APath);
  strBuff := GenerateWSDL(locRepName,FRootAddress);
  i:=Length(strBuff);
  if (StrBuff<>'') then
    begin
    AResponse.ContentType := 'text/xml';
    AResponse.Content:=strBuff;
    end
  else
    begin
    AResponse.ContentType := 'text/html';
    AResponse.Content := GenerateWSDLHtmlTable();
    end;
  if AResponse.ContentLength=0 then
    AResponse.ContentLength:=Length(AResponse.Content);
end;

procedure TFPWorkerObject.ProcessServiceRequest(
      ARequest  : TRequest;
      AResponse : TResponse;
  var APath     : string
);
var
  trgt,ctntyp, frmt : string;
  rqst : IRequestBuffer;
  inStream : TStringStream;
begin
  trgt := ExtractNextPathElement(APath);
  if AnsiSameText(sWSDL,trgt) then
    begin
    ProcessWSDLRequest(ARequest,AResponse,APath);
    Exit;
    end;
  inStream := nil;
  try
    inStream := TStringStream.Create(ARequest.Content);
    try
      AResponse.ContentStream := TMemoryStream.Create();
      ctntyp := ARequest.ContentType;
      AResponse.ContentType := ctntyp;
      frmt := Trim(ARequest.QueryFields.Values['format']);
      rqst := TRequestBuffer.Create(trgt,ctntyp,inStream,AResponse.ContentStream,frmt);
      rqst.GetPropertyManager().SetProperty(sREMOTE_IP,ARequest.RemoteAddress);
      HandleServiceRequest(rqst);
      AResponse.ContentLength:=AResponse.ContentStream.Size;
    finally
      inStream.Free();
    end;
  except
    on e : Exception do begin
      if Assigned(FOnNotifyMessage) then
        FOnNotifyMessage(Self,'ProcessData()>> Exception = '+e.Message);
      raise;
    end;
  end;
end;

function TFPWorkerObject.GetHandleRequestInThread : Boolean;
begin
  Result := FHTTPServerObject.Threaded;
end;

function TFPWorkerObject.GetListeningPort : Integer;
begin
  Result := FHTTPServerObject.Port;
end;

procedure TFPWorkerObject.RequestHandler(
      Sender    : TObject;
  var ARequest  : TFPHTTPConnectionRequest;
  var AResponse : TFPHTTPConnectionResponse
);
var
{$IFDEF WST_DBG}
  s : string;
  j : SizeInt;
{$ENDIF}
  locPath, locPathPart : string;
begin
  AResponse.Server:=FServerSoftware;
  locPath := ARequest.URL;
  locPathPart := ExtractNextPathElement(locPath);
  if AnsiSameText(sSERVICES_PREFIXE,locPathPart)  then
    ProcessServiceRequest(ARequest,AResponse,locPath)
  else
    ProcessWSDLRequest(ARequest,AResponse,locPath);
  try
    AResponse.SendContent;
  finally
    if Assigned(AResponse.ContentStream) then begin
      AResponse.ContentStream.Free();
      AResponse.ContentStream := nil;
    end;
  end;
end;

function TFPWorkerObject.GetObject : TObject;
begin
  Result := Self;
end;

procedure TFPWorkerObject.SetHandleRequestInThread(const AValue : Boolean);
begin
  if FHTTPServerObject.Active then
    raise Exception.CreateFmt(SERR_ObjectStateDoesNotAllowOperation,['SetHandleRequestInThread']);
  FHTTPServerObject.Threaded := AValue;
end;

procedure TFPWorkerObject.SetListeningPort(const AValue : Integer);
begin
  if FHTTPServerObject.Active then
    raise Exception.CreateFmt(SERR_ObjectStateDoesNotAllowOperation,['SetListeningPort']);
  FHTTPServerObject.Port := AValue;
end;

constructor TFPWorkerObject.Create();
begin
  inherited Create();
  FHTTPServerObject := TFPHTTPServer.Create(nil);
  FHTTPServerObject.OnRequest := @RequestHandler;
end;

destructor TFPWorkerObject.Destroy();
begin
  if (FHTTPServerObject <> nil) then
    FHTTPServerObject.Active := False;
  FreeAndNil(FHTTPServerObject);
  inherited Destroy();
end;

procedure TFPWorkerObject.Start();
begin
  if not FHTTPServerObject.Active then
    FHTTPServerObject.Active := True;
end;

procedure TFPWorkerObject.Stop();
begin
  if FHTTPServerObject.Active then
    FHTTPServerObject.Active := False;
end;

function TFPWorkerObject.IsActive : Boolean;
begin
  Result := FHTTPServerObject.Active;
end;

{ TwstFPHttpListener }

procedure TwstFPHttpListener.SetOnNotifyMessage(const AValue : TListnerNotifyMessage);
begin
  inherited SetOnNotifyMessage(AValue);
  if (FWorkerObject <> nil) then
    FWorkerObject.OnNotifyMessage := AValue;
end;

constructor TwstFPHttpListener.Create(
      const AServerIpAddress   : string;
      const AListningPort      : Integer;
      const ADefaultClientPort : Integer;
      const AServerSoftware    : string
);

begin
  inherited Create();
  FWorkerObjectRef := TFPWorkerObject.Create();
  FWorkerObject := TFPWorkerObject(FWorkerObjectRef.GetObject());
  FWorkerObject.RootAddress := AServerIpAddress;
  FWorkerObject.ServerSoftware := AServerSoftware;
  FWorkerObject.ListeningPort := AListningPort;
end;

destructor TwstFPHttpListener.Destroy();
begin
  if (FWorkerObject <> nil) then
    Stop();
  FWorkerObjectRef := nil;
  inherited Destroy();
end;

procedure TwstFPHttpListener.Start();
begin
  if not FWorkerObject.IsActive() then begin
    FWorkerObject.HandleRequestInThread := (loHandleRequestInThread in Options);
    if (loExecuteInThread in Options) then begin
      // The thread is create with "FreeOnTerminate := True"
      TServerListnerThread.Create(FWorkerObject);
    end else begin
      FWorkerObject.Start();
    end;
  end;
end;

procedure TwstFPHttpListener.Stop();
begin
  if FWorkerObject.IsActive() then begin
    //In case of the thread(loExecuteInThread in Options),
    //this will make the thread exit and free itself as "FreeOnTerminate := True"
    FWorkerObject.Stop();
  end;
end;

class function TwstFPHttpListener.GetDescription() : string;
begin
  Result := 'WST FP HTTP Listener';
end;

function TwstFPHttpListener.IsActive: Boolean;
begin
  Result := FWorkerObject.IsActive();
end;

initialization


end.
