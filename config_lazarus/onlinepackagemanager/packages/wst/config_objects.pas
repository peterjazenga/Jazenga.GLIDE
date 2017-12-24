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
unit config_objects;

interface
uses SysUtils, Classes, base_service_intf, wst_types;

type

  TwstConfigItem = class(TBaseComplexRemotable);

  TwstConfigService = class(TwstConfigItem)
  private
    FPooled: Boolean;
    FPoolMax: PtrInt;
    FPoolMin: PtrInt;
    FTimeOut: PtrUInt;
    FName: string;
  public
    function GetConfigText() : string;
  published
    property Name : string read FName write FName;
    property PoolMax : PtrInt read FPoolMax write FPoolMax;
    property PoolMin : PtrInt read FPoolMin write FPoolMin;
    property Pooled : Boolean read FPooled write FPooled;
    property TimeOut : PtrUInt read FTimeOut write FTimeOut;
  end;

  TwstConfigServiceArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TwstConfigService;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TwstConfigService Read GetItem;Default;
  end;

  TWstConfigurationObject = class(TBaseComplexRemotable)
  private
    FServices: TwstConfigServiceArray;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property Services : TwstConfigServiceArray read FServices write FServices;
  end;


  {$IFNDEF FPC}
  function GetAppConfigDir(const AGlobal : Boolean) : string;
  {$ENDIF}

  function wst_GetConfigFileName():string;
  function wst_GetConfigObject() : TWstConfigurationObject;
  function wst_GetServiceConfig(const AName : string) : TwstConfigService ;
  function wst_GetServiceConfigText(const AName : string) : string ;
  procedure wst_CreateDefaultFile();overload;
  procedure wst_CreateDefaultFile(const AFileName : string);overload;
  procedure wst_CreateDefaultFile(ADest : TStream; AConfigObj : TWstConfigurationObject);overload;
  procedure wst_CreateDefaultFile(const AFileName : string; AConfigObj : TWstConfigurationObject);overload;

implementation
uses
  base_soap_formatter, server_service_intf
{$IFNDEF FPC}
  , xmldom, wst_delphi_xml
{$ENDIF};

const
  sCONFIG_FILE_NAME = 'wst_config.xml';
  sAPPLICATION = 'Application';
  sWST_CONFIG_PATH = 'WST_Configuration';
var
  ConfigurationObjectInstance : TWstConfigurationObject = nil;

{$IFNDEF FPC}
function GetAppConfigDir(const AGlobal : Boolean) : string;
begin
  Result := ExtractFilePath(ParamStr(0));
end;

function GetAppConfigFile(const AGlobal : Boolean) : string;
begin
  Result := ChangeFileExt(ParamStr(0),'.xml');
end;
{$ENDIF}

function wst_GetConfigFileName():string;
begin
{$IFDEF WINDOWS}
  Result := ChangeFileExt(ParamStr(0),'.' + sCONFIG_FILE_NAME);
{$ELSE}
  Result := ChangeFileExt(GetAppConfigFile(False,True),'.' + sCONFIG_FILE_NAME);
{$ENDIF WINDOWS}
end;

procedure wst_LoadConfigObject(AConfig: TWstConfigurationObject; AStream : TStream);overload;
var
  frmt : IFormatterBase;
  locPrmName : string;
begin
  frmt := TSOAPBaseFormatter.Create();
  frmt.LoadFromStream(AStream);
  frmt.SetSerializationStyle(ssNodeSerialization);
  locPrmName := sWST_CONFIG_PATH;
  frmt.Get(TypeInfo(TWstConfigurationObject),locPrmName,AConfig);
end;

procedure wst_LoadConfigObject(AConfig : TWstConfigurationObject; const AFileName : string);overload;
var
  locStream : TMemoryStream;
begin
  if not FileExists(AFileName) then
    raise Exception.CreateFmt('File not found : "%s".',[AFileName]);
  locStream := TMemoryStream.Create();
  try
    locStream.LoadFromFile(AFileName);
    locStream.Position := 0;
    wst_LoadConfigObject(AConfig,locStream);
  finally
    FreeAndNil(locStream);
  end;
end;

function wst_CreateDefaultConfigObject() : TWstConfigurationObject;
var
  c, i : Integer;
  servReg : IServerServiceRegistry;
begin
  Result := TWstConfigurationObject.Create();
  try
    servReg := GetServerServiceRegistry();
    c := servReg.GetCount();
    Result.Services.SetLength(0);
    if ( c > 0 ) then begin
      Result.Services.SetLength(c);
      for i := 0 to Pred(c) do begin
        Result.Services[i].Name := servReg.GetName(i);
      end;
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure wst_CreateDefaultFile();
begin
  wst_CreateDefaultFile(wst_GetConfigFileName());
end;

procedure wst_CreateDefaultFile(const AFileName : string);
begin
  wst_CreateDefaultFile(AFileName,nil);
end;

procedure wst_CreateDefaultFile(ADest : TStream; AConfigObj : TWstConfigurationObject);overload;
var
  locObj : TWstConfigurationObject;
  frmt : IFormatterBase;
  createdHere : Boolean;
begin
  createdHere := ( AConfigObj = nil );
  if ( AConfigObj <> nil ) then
    locObj := AConfigObj
  else
    locObj := wst_CreateDefaultConfigObject();
  try
    frmt := TSOAPBaseFormatter.Create();
    frmt.SetSerializationStyle(ssNodeSerialization);
    frmt.BeginObject(sAPPLICATION,TypeInfo(TWstConfigurationObject));
      frmt.Put(sWST_CONFIG_PATH,TypeInfo(TWstConfigurationObject),locObj);
    frmt.EndScope();
    frmt.SaveToStream(ADest);
  finally
    if createdHere then
      FreeAndNil(locObj);
  end;
end;

procedure wst_CreateDefaultFile(const AFileName : string; AConfigObj : TWstConfigurationObject);overload;
var
  locStream : TMemoryStream;
begin
  locStream := TMemoryStream.Create();
  try
    wst_CreateDefaultFile(locStream,AConfigObj);
    locStream.SaveToFile(AFileName);
  finally
    FreeAndNil(locStream);
  end;
end;

function wst_GetConfigObject() : TWstConfigurationObject;
var
  locFileName : string;
begin
  if ( ConfigurationObjectInstance = nil ) then begin
    ConfigurationObjectInstance := wst_CreateDefaultConfigObject();
    locFileName := wst_GetConfigFileName();
    if FileExists(locFileName) then
      wst_LoadConfigObject(ConfigurationObjectInstance,locFileName)
    {else
      wst_CreateDefaultFile(locFileName,ConfigurationObjectInstance);}
  end;
  Result := ConfigurationObjectInstance;
end;

function wst_GetServiceConfig(const AName : string) : TwstConfigService ;
var
  i, c : Integer;
  servs : TwstConfigServiceArray;
begin
  Result := nil;
  servs := wst_GetConfigObject().Services;
  c := servs.Length;
  for i := 0 to Pred(c) do begin
    if AnsiSameText(AName,servs[i].Name) then begin
      Result := servs[i];
      Break;
    end;
  end;
end;

function wst_GetServiceConfigText(const AName : string) : string ;
var
  s : TwstConfigService;
begin
  s := wst_GetServiceConfig(AName);
  if ( s <> nil ) then
    Result := s.GetConfigText()
  else
    Result := '';
end;

{ TwstConfigServiceArray }

function TwstConfigServiceArray.GetItem(AIndex: Integer): TwstConfigService;
begin
  Result := Inherited GetItem(AIndex) As TwstConfigService;
end;

class function TwstConfigServiceArray.GetItemClass() : TBaseRemotableClass;
begin
  Result := TwstConfigService;
end;

{ TWstConfigurationObject }

constructor TWstConfigurationObject.Create;
begin
  inherited;
  FServices := TwstConfigServiceArray.Create();
end;

destructor TWstConfigurationObject.Destroy;
begin
  FreeAndNil(FServices);
  inherited;
end;

{ TwstConfigService }

function TwstConfigService.GetConfigText() : string;
begin
  Result := Format('PoolMin=%d;PoolMax=%d;Pooled=',[PoolMin,PoolMax]);
  if Pooled then
    Result := Result + 'True'
  else
    Result := Result + 'False';
end;

procedure initialize_config_objects();
begin
  TwstConfigService.RegisterAttributeProperty('Name');
  GetTypeRegistry().Register(sWST_BASE_NS,TypeInfo(TwstConfigService),'Service');
  GetTypeRegistry().Register(sWST_BASE_NS,TypeInfo(TwstConfigServiceArray),'Services');
  GetTypeRegistry().Register(sWST_BASE_NS,TypeInfo(TWstConfigurationObject),'WST_Configuration');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(TwstConfigServiceArray)].RegisterExternalPropertyName('Item','service');
end;

procedure finalize_config_objects();
begin
  {if ( ConfigurationObjectInstance <> nil ) and
     ( GetServerServiceRegistry.GetCount() <> ConfigurationObjectInstance.Services.Length )
  then begin
    wst_CreateDefaultFile(wst_GetConfigFileName(),nil);
  end;}
  FreeAndNil(ConfigurationObjectInstance);
end;

initialization
  initialize_config_objects();
  
finalization
  finalize_config_objects();
  
end.
