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
unit server_service_json;

interface

uses
  Classes, SysUtils, TypInfo,
  base_service_intf, server_service_intf,
  fpjson, base_json_formatter;

type

  { TJsonRpcFormatter }

  TJsonRpcFormatter = class(TJsonRpcBaseFormatter,IFormatterBase,IFormatterResponse)
  Private
    FCallProcedureName : string;
    FCallTarget : string;
    FIDObject : TJSONData;
    FVersion : string;
    FVersionEnum : TJonRPCVersion;
  protected
    procedure SetVersion(const AValue : string);
    procedure BeginCallResponse(Const AProcName,ATarget:string);
    procedure EndCallResponse();
    procedure BeginCallRead(ACallContext : ICallContext);
    function GetCallProcedureName():String;
    function GetCallTarget():String;
    procedure BeginExceptionList(
      const AErrorCode : string;
      const AErrorMsg  : string
    );
    procedure EndExceptionList();
  public
    constructor Create();override;
    destructor Destroy();override;
    property Version : string read FVersion;
    property VersionEnum : TJonRPCVersion read FVersionEnum;
  end;

  procedure Server_service_RegisterJsonFormat();

implementation
uses
  jsonparser, StrUtils;

procedure Server_service_RegisterJsonFormat();
begin
  GetFormatterRegistry().Register(s_json,s_json_ContentType,TSimpleItemFactory.Create(TJsonRpcFormatter));
end;

{$IFDEF HAS_JSON_CLONE}
function Clone(const AValue : TJSONData) : TJSONData; inline;
begin
  if ( AValue = nil ) then
    Result := nil
  else  
    Result := AValue.Clone();
end;

{$ELSE HAS_JSON_CLONE}
function Clone(const AValue : TJSONData) : TJSONData;
var
  locParser : TJSONParser;
begin
  if Assigned(AValue) then begin
    case AValue.JSONType() of
      jtNumber :
        begin
          if ( TJSONNumber(AValue).NumberType() = ntInteger ) then
            Result := TJSONIntegerNumber.Create(AValue.AsInteger)
          else
            Result := TJSONFloatNumber.Create(AValue.AsFloat);
        end;
      jtString  : Result := TJSONString.Create(AValue.AsString);
      jtBoolean : Result := TJSONBoolean.Create(AValue.AsBoolean);
      jtNull    : Result := TJSONNull.Create();
      jtArray,
      jtObject  :
        begin
          locParser := TJSONParser.Create(AValue.AsJSON);
          try
            Result := locParser.Parse();
          finally
            locParser.Free();
          end;
        end;
      else
        raise Exception.Create('Invalid JSON object type.');
    end;
  end else begin
    Result := nil;
  end;
end;
{$ENDIF HAS_JSON_CLONE}

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

procedure TJsonRpcFormatter.BeginCallResponse(const AProcName, ATarget : string);
var
  locBuffer : string;
begin
  Clear();
  BeginObject('',nil);
    if ( VersionEnum = jsonRPC_11 ) then begin
      locBuffer := s_json_rpc_version_11;
      Put(s_json_version,TypeInfo(string),locBuffer);
    end;
end;

procedure TJsonRpcFormatter.EndCallResponse();
var
  locRoot : TJSONObject;
begin
    locRoot := GetRootData();
    if ( locRoot.IndexOfName(s_json_result) < 0 ) then
      locRoot.Elements[s_json_result] := TJSONNull.Create();
    if ( VersionEnum = jsonRPC_10 ) then
      locRoot.Elements[s_json_error] := TJSONNull.Create();
    if Assigned(FIDObject) then begin
      locRoot.Elements[s_json_id] := FIDObject;
      FIDObject := nil;
    end else begin
      if ( VersionEnum = jsonRPC_10 ) then
        locRoot.Elements[s_json_id] := TJSONNull.Create();
    end;
  EndScope();
end;

procedure TJsonRpcFormatter.BeginCallRead(ACallContext : ICallContext);
var
  nameBuffer, strBuffer : string;
  rootObj : TJSONObject;
  tmpObj : TJSONData;
  i : Integer;
  paramsAsArray : Boolean;
begin
  ClearStack();
  FreeAndNil(FIDObject);
  rootObj := GetRootData();
  PushStack(rootObj,stObject);
  
  i := rootObj.IndexOfName(s_json_version);
  strBuffer := s_json_rpc_version_10; // Assume 1.0 by default
  if ( i > -1 ) then begin
    tmpObj := rootObj.Items[i];
    if not rootObj.Items[i].IsNull then
      strBuffer := tmpObj.AsString;
  end;
  SetVersion(strBuffer);
  
  nameBuffer := s_json_method;
  Get(TypeInfo(string),nameBuffer,FCallProcedureName);
  i := rootObj.IndexOfName(s_json_id);
  if ( i > -1 ) then
    FIDObject := Clone(rootObj.Items[i]);

  if ( VersionEnum = jsonRPC_11 ) then begin
    i := rootObj.IndexOfName(s_json_params);
    if ( i > 0 ) then begin
      paramsAsArray := ( rootObj.Items[i].JSONType() = jtArray );
    end else begin
      rootObj.Add(s_json_params,TJSONArray.Create());
      paramsAsArray := True;
    end;
  end else begin
    paramsAsArray := True;
  end;
  nameBuffer := s_json_params;
  if paramsAsArray then
    BeginArrayRead(nameBuffer,nil,asScoped,'')
  else
    BeginObjectRead(nameBuffer,nil);
end;

function TJsonRpcFormatter.GetCallProcedureName() : String;
begin
  Result := FCallProcedureName;
end;

function TJsonRpcFormatter.GetCallTarget() : String;
begin
  Result := FCallTarget;
end;

procedure TJsonRpcFormatter.BeginExceptionList(
  const AErrorCode : string;
  const AErrorMsg : string
);
var
  locRoot, locError : TJSONObject;
begin
  Clear();
  BeginObject('',nil);

    locRoot := GetRootData();
    case VersionEnum of
      jsonRPC_10 : locRoot.Elements[s_json_result] := TJSONNull.Create();
      jsonRPC_11 : locRoot.Add(s_json_version,s_json_rpc_version_11)
    end;
    locError := TJSONObject.Create();
    locRoot.Elements[s_json_error] := locError;
    locError.Add(s_json_name,'');
    locError.Add(s_json_code,StrToIntDef(AErrorCode,0));
    locError.Add(s_json_message,AErrorMsg);
    if Assigned(FIDObject) then begin
      locRoot.Elements[s_json_id] := FIDObject;
      FIDObject := nil;
    end else begin
      locRoot.Elements[s_json_id] := TJSONNull.Create();
    end;
end;

procedure TJsonRpcFormatter.EndExceptionList();
begin
  EndScope();
end;

constructor TJsonRpcFormatter.Create();
begin
  inherited Create();
  SetVersion(s_json_rpc_version_10);
end;

destructor TJsonRpcFormatter.Destroy();
begin
  FreeAndNil(FIDObject);
  inherited Destroy();
end;

end.

