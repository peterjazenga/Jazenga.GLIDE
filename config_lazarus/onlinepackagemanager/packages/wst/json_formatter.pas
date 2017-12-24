{
    This file is part of the Web Service Toolkit
    Copyright (c) 2007 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$INCLUDE wst_global.inc}
unit json_formatter;

interface

uses
  Classes, SysUtils, TypInfo,
  base_service_intf, service_intf, imp_utils,
  base_json_formatter, fpjson, SyncObjs;

type

{$M+}

  { TJsonRpcFormatter }

  TJsonRpcFormatter = class(TJsonRpcBaseFormatter,IFormatterClient)
  private
    FCallProcedureName : string;
    FCallTarget : string;
    FVersion : string;
    FVersionEnum : TJonRPCVersion;
  private
    procedure SetVersion(const AValue : string);
  public
    constructor Create();override;

    procedure BeginCall(
      const AProcName,
            ATarget      : string;
            ACallContext : ICallContext
    );
    procedure EndCall();
    procedure BeginCallRead(ACallContext : ICallContext);

    function GetCallProcedureName():string;
    function GetCallTarget():string;
    property VersionEnum : TJonRPCVersion read FVersionEnum;
  published
    property Version : string read FVersion write SetVersion;
  end;
  
  { TJsonRpcCallMaker }

  TJsonRpcCallMaker = class(TSimpleFactoryItem,ICallMaker)
  Private
    FPropMngr : IPropertyManager;
  Public
    constructor Create();override;
    destructor Destroy();override;
    function GetPropertyManager():IPropertyManager;
    procedure MakeCall(
      ASerializer : IFormatterClient;
      ATransport  : ITransport
    );
  End;

  TJsonRpcCustomIdManager = class
  public
    function GetNewID() : PtrInt;virtual;abstract;
  end;
  
  { TJsonRpcSequencedIdManager }

  TJsonRpcSequencedIdManager = class(TJsonRpcCustomIdManager)
  private
    FIdSequence : PtrInt;
    FIdSequenceLock : TCriticalSection;
  public
    constructor Create();
    destructor Destroy();override;
    function GetNewID() : PtrInt;override;
  end;
  
  procedure RegisterJsonProtocol();
  procedure SetIdManager(AValue : TJsonRpcCustomIdManager);{$IFDEF USE_INLINE}inline;{$ENDIF}
  function GetIdManager():TJsonRpcCustomIdManager ;{$IFDEF USE_INLINE}inline;{$ENDIF}
  
implementation
uses
  StrUtils;
  
var
  FIdManager : TJsonRpcCustomIdManager;

function GetIdManager():TJsonRpcCustomIdManager ;
begin
  Result := FIdManager;
end;

procedure SetIdManager(AValue : TJsonRpcCustomIdManager);
begin
  FreeAndNil(FIdManager);
  FIdManager := AValue;
end;
  
{ TJsonRpcFormatter }

procedure TJsonRpcFormatter.SetVersion(const AValue : string);
var
  i : Integer;
begin
  if ( FVersion = AValue ) then
    Exit;
  i := AnsiIndexStr(AValue,[s_json_rpc_version_10,s_json_rpc_version_11]);
  if ( i < 0 ) then
    Error('JSON-RPC version not supported : %s',[AValue]);
  FVersion := AValue;
  FVersionEnum := TJonRPCVersion(i);
end;

constructor TJsonRpcFormatter.Create();
begin
  inherited Create();
  SetVersion(s_json_rpc_version_10);
end;

procedure TJsonRpcFormatter.BeginCall(
  const AProcName, ATarget : string;
        ACallContext : ICallContext
);
begin
  FCallProcedureName := AProcName;
  FCallTarget := ATarget;

  case VersionEnum of
    jsonRPC_10 :
      begin
        BeginObject('',Nil);
          Put(s_json_method,TypeInfo(string),FCallProcedureName);
          BeginArray(s_json_params,Nil,nil,[0,0],asScoped);
      end;
    jsonRPC_11 :
      begin
        BeginObject('',Nil);
          Put(s_json_version,TypeInfo(string),Version);
          Put(s_json_method,TypeInfo(string),FCallProcedureName);
          BeginArray(s_json_params,Nil,nil,[0,0],asScoped);
      end;
    else
      Error('JSON-RPC version not supported : %s',[Version]);
  end;
end;

procedure TJsonRpcFormatter.EndCall();
var
  i : PtrInt;
begin
    EndScope(); // params
    if ( VersionEnum = jsonRPC_10 ) then begin
      if Assigned(FIdManager) then
        i := FIdManager.GetNewID()
      else
        i := 0;
      Put(s_json_id,TypeInfo(PtrInt),i);
    end;
  EndScope();   // Root object
end;

procedure TJsonRpcFormatter.BeginCallRead(ACallContext : ICallContext);
Var
  errCode, errMsg : string;
  e : EJsonRpcException;
  elt : TJSONData;
  remoteErr : TJSONObject;
  i : Integer;
begin
  ClearStack();
  PushStack(GetRootData(),stObject);
  elt := GetRootData().Find(s_json_error);
  if Assigned(elt) and elt.InheritsFrom(TJSONObject) then begin
    remoteErr := TJSONObject(elt);
    i := remoteErr.IndexOfName(s_json_code);
    if ( i > -1 ) then
      errCode := remoteErr.Items[i].AsString
    else
      errCode := '';
    i := remoteErr.IndexOfName(s_json_message);
    if ( i > -1 ) then
      errMsg := remoteErr.Items[i].AsString
    else
      errMsg := remoteErr.AsJSON;
    e := EJsonRpcException.Create(errMsg);
    e.FaultCode := errCode;
    e.FaultString := errMsg;
    raise e;
  end;
end;

function TJsonRpcFormatter.GetCallProcedureName() : string;
begin
  Result := FCallProcedureName;
end;

function TJsonRpcFormatter.GetCallTarget() : string;
begin
  Result := FCallTarget;
end;

{ TJsonRpcCallMaker }

constructor TJsonRpcCallMaker.Create();
begin
  FPropMngr := TPublishedPropertyManager.Create(Self);
end;

destructor TJsonRpcCallMaker.Destroy();
begin
  FPropMngr := Nil;
  inherited Destroy();
end;

function TJsonRpcCallMaker.GetPropertyManager() : IPropertyManager;
begin
  Result:= FPropMngr;
end;

procedure TJsonRpcCallMaker.MakeCall(
  ASerializer : IFormatterClient;
  ATransport : ITransport
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
    s_json
  );
  propMngr.SetProperty(
    sFORMAT,
    s_json
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

procedure RegisterJsonProtocol();
begin
  GetFormaterRegistry().Register(
    s_json,
    TSimpleItemFactory.Create(TJsonRpcFormatter),
    TSimpleItemFactory.Create(TJsonRpcCallMaker)
  );
end;

{ TJsonRpcSequencedIdManager }

constructor TJsonRpcSequencedIdManager.Create();
begin
  FIdSequenceLock := TCriticalSection.Create();
end;

destructor TJsonRpcSequencedIdManager.Destroy();
begin
  FreeAndNil(FIdSequenceLock);
  inherited Destroy();
end;

function TJsonRpcSequencedIdManager.GetNewID() : PtrInt;
begin
  FIdSequenceLock.Acquire();
  try
    Inc(FIdSequence);
    Result := FIdSequence;
  finally
    FIdSequenceLock.Release();
  end;
end;

initialization
  SetIdManager(TJsonRpcSequencedIdManager.Create());
  RegisterJsonProtocol();
  
finalization
  FreeAndNil(FIdManager);
  
end.
