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
{$IFDEF FPC}
  //{$UNDEF INDY_9}
  //{$DEFINE INDY_10}
{$ELSE}
  //{$UNDEF INDY_10}
  //{$DEFINE INDY_9}
{$ENDIF}

unit indy_http_server;

interface

uses
  Classes, SysUtils,
  IdCustomHTTPServer,
  IdHTTPServer,
{$IFDEF INDY_10}
  IdContext,
{$ENDIF}
{$IFDEF INDY_9}
  IdTCPServer,
{$ENDIF}
  IdSocketHandle,
  server_listener, wst_types;

type

  { TwstIndyHttpListener }

  TwstIndyHttpListener = class(TwstListener)
  private
    FRootAddress : string;
  protected
    FHTTPServerObject: TIdHTTPServer;
  private
    procedure ProcessWSDLRequest(
          {$IFDEF INDY_10}
          AContext        : TIdContext;
          {$ENDIF}
          ARequestInfo    : TIdHTTPRequestInfo;
          AResponseInfo   : TIdHTTPResponseInfo;
      var APath           : string
    );
    procedure ProcessServiceRequest(
          {$IFDEF INDY_10}
          AContext        : TIdContext;
          {$ENDIF}
          ARequestInfo    : TIdHTTPRequestInfo;
          AResponseInfo   : TIdHTTPResponseInfo;
      var APath           : string
    );
  private
    procedure Handler_CommandGet(
    {$IFDEF INDY_10}
      AContext        : TIdContext;
    {$ENDIF}
    {$IFDEF INDY_9}
      AThread: TIdPeerThread;
    {$ENDIF}
      ARequestInfo    : TIdHTTPRequestInfo;
      AResponseInfo   : TIdHTTPResponseInfo
    );
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
  end;


implementation
uses
{$IFDEF WST_DELPHI}
     ActiveX,
{$ENDIF WST_DELPHI}
{$IFDEF INDY_9}
     wst_indy9_utils,
{$ENDIF INDY_9}
{$IFDEF INDY_10}
     wst_indy10_utils, IdSchedulerOfThread, IdSchedulerOfThreadDefault,
{$ENDIF}
     base_service_intf,
     server_service_intf, server_service_imputils,
     metadata_wsdl;

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

{ TwstIndyHttpListener }

procedure TwstIndyHttpListener.ProcessWSDLRequest(
      {$IFDEF INDY_10}
      AContext        : TIdContext;
      {$ENDIF}
      ARequestInfo    : TIdHTTPRequestInfo;
      AResponseInfo   : TIdHTTPResponseInfo;
  var APath           : string
);
var
  locRepName, strBuff : string;
  i : Integer;
begin
  locRepName := ExtractNextPathElement(APath);
  if AnsiSameText(sWSDL,locRepName) then
    locRepName := ExtractNextPathElement(APath);
  strBuff := GenerateWSDL(locRepName,FRootAddress);
  i := Length(strBuff);
  if ( i > 0 ) then begin
    AResponseInfo.ContentType := 'text/xml';
    if not Assigned(AResponseInfo.ContentStream) then
      AResponseInfo.ContentStream := TMemoryStream.Create();
    AResponseInfo.ContentStream.Write(strBuff[1],i);
    Exit;
  end;
  AResponseInfo.ContentText := GenerateWSDLHtmlTable();
  AResponseInfo.ContentType := 'text/html';
end;

procedure TwstIndyHttpListener.ProcessServiceRequest(
      {$IFDEF INDY_10}
      AContext        : TIdContext;
      {$ENDIF}
      ARequestInfo    : TIdHTTPRequestInfo;
      AResponseInfo   : TIdHTTPResponseInfo;
  var APath           : string
);
var
  trgt,ctntyp, frmt : string;
  rqst : IRequestBuffer;
  inStream : TMemoryStream;
begin
  trgt := ExtractNextPathElement(APath);
  if AnsiSameText(sWSDL,trgt) then begin
    ProcessWSDLRequest({$IFDEF INDY_10}AContext,{$ENDIF}ARequestInfo,AResponseInfo,APath);
    Exit;
  end;
  inStream := nil;
  try
    try
      inStream := TMemoryStream.Create();
      AResponseInfo.ContentStream := TMemoryStream.Create();

      ctntyp := ARequestInfo.ContentType;
      inStream.CopyFrom(ARequestInfo.PostStream,0);
      inStream.Position := 0;
      AResponseInfo.ContentType := ctntyp;
      frmt := Trim(ARequestInfo.Params.Values['format']);
      rqst := TRequestBuffer.Create(trgt,ctntyp,inStream,AResponseInfo.ContentStream,frmt);
      rqst.GetPropertyManager().SetProperty(sREMOTE_IP,ARequestInfo.RemoteIP);
      HandleServiceRequest(rqst);
    finally
      inStream.Free();
    end;
  except
    on e : Exception do begin
      NotifyMessage('ProcessData()>> Exception = '+e.Message);
      raise;
    end;
  end;
end;

procedure TwstIndyHttpListener.Handler_CommandGet(
  {$IFDEF INDY_10}
    AContext        : TIdContext;
  {$ENDIF}
  {$IFDEF INDY_9}
    AThread: TIdPeerThread;
  {$ENDIF}
  ARequestInfo   : TIdHTTPRequestInfo;
  AResponseInfo  : TIdHTTPResponseInfo
);
var
{$IFDEF WST_DBG}
  s : string;
  j : SizeInt;
{$ENDIF}
  locPath, locPathPart : string;
begin
{$IFDEF WST_DBG}
  if Assigned(ARequestInfo.PostStream) and ( ARequestInfo.PostStream.Size > 0 ) then begin
    j := ARequestInfo.PostStream.Size;
    SetLength(s,j);
    ARequestInfo.PostStream.Read(s[1],j);
    NotifyMessage('----------- QUERY ----------------------');
    Display(s);
  end;
{$ENDIF}
  locPath := ARequestInfo.Document;
  locPathPart := ExtractNextPathElement(locPath);
  if AnsiSameText(sSERVICES_PREFIXE,locPathPart)  then begin
    ProcessServiceRequest({$IFDEF INDY_10}AContext,{$ENDIF}ARequestInfo,AResponseInfo,locPath);
  {$IFDEF WST_DBG}
    if Assigned(AResponseInfo.ContentStream) and ( AResponseInfo.ContentStream.Size > 0 ) then begin
      j := AResponseInfo.ContentStream.Size;
      SetLength(s,j);
      AResponseInfo.ContentStream.Position := 0;
      AResponseInfo.ContentStream.Read(s[1],j);
      Display('--------- RESPONSE ------------------------');
      Display(s);
    end;
  {$ENDIF}
    Exit;
  end;

  ProcessWSDLRequest({$IFDEF INDY_10}AContext,{$ENDIF}ARequestInfo,AResponseInfo,locPath);
end;

constructor TwstIndyHttpListener.Create(
      const AServerIpAddress   : string;
      const AListningPort      : Integer;
      const ADefaultClientPort : Integer;
      const AServerSoftware    : string
);
var
  b : TIdSocketHandle;
begin
  inherited Create();
  FHTTPServerObject := TIdHTTPServer.Create({$IFDEF INDY_9}nil{$ENDIF});
{$IFDEF INDY_9}
  FHTTPServerObject.ThreadClass := TwstIndy9Thread;
{$ENDIF INDY_9}
{$IFDEF INDY_10}
  FHTTPServerObject.Scheduler := TIdSchedulerOfThreadDefault.Create(FHTTPServerObject);
  TIdSchedulerOfThread(FHTTPServerObject.Scheduler).ThreadClass := TwstIndy10Thread;
{$ENDIF INDY_10}
  b := FHTTPServerObject.Bindings.Add();
  b.IP := AServerIpAddress;
  b.port := AListningPort;
  FRootAddress := Format('http://%s:%d/',[AServerIpAddress,AListningPort]);

  FHTTPServerObject.DefaultPort := ADefaultClientPort;
  FHTTPServerObject.ServerSoftware := AServerSoftware;
  //FHTTPServerObject.Active := True;
  FHTTPServerObject.OnCommandGet := {$IFDEF FPC}@{$ENDIF}Handler_CommandGet;
end;

destructor TwstIndyHttpListener.Destroy();
begin
  if ( FHTTPServerObject <> nil ) then
    Stop();
  FreeAndNil(FHTTPServerObject);
  inherited Destroy();
end;

procedure TwstIndyHttpListener.Start();
begin
  if not FHTTPServerObject.Active then
    FHTTPServerObject.Active := True;
end;

procedure TwstIndyHttpListener.Stop();
begin
  if FHTTPServerObject.Active then
    FHTTPServerObject.Active := False;
end;

class function TwstIndyHttpListener.GetDescription: string;
begin
  Result := 'WST Indy HTTP Listener';
end;

function TwstIndyHttpListener.IsActive: Boolean;
begin
  Result := FHTTPServerObject.Active;
end;

initialization


end.
