{
This unit has been produced by ws_helper.
  Input unit name : "metadata_service".
  This unit name  : "metadata_service_binder".
  Date            : "23/08/2010 16:10:07".
}
unit metadata_service_binder;
{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}
interface

uses SysUtils, Classes, base_service_intf, server_service_intf, metadata_service;


type
  TWSTMetadataService_ServiceBinder = class(TBaseServiceBinder)
  protected
    procedure GetRepositoryListHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure GetRepositoryInfoHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
  public
    constructor Create();
  end;

type
  TWSTMetadataService_ServiceBinderFactory = class(TInterfacedObject,IItemFactory)
  private
    FInstance : IInterface;
  protected
    function CreateInstance():IInterface;
  public
    constructor Create();
    destructor Destroy();override;
  end;

  procedure Server_service_RegisterWSTMetadataServiceService();

Implementation
uses TypInfo, wst_resources_imp,metadata_repository;

{ TWSTMetadataService_ServiceBinder implementation }
procedure TWSTMetadataService_ServiceBinder.GetRepositoryListHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IWSTMetadataService;
  callCtx : ICallContext;
  locStrPrmName : string;
  procName,trgName : string;
  returnVal : TArrayOfStringRemotable;
begin
  callCtx := AContext;
  Fillchar(returnVal,SizeOf(TArrayOfStringRemotable),#0);
  
  
  tmpObj := GetFactory().CreateInstance() as IWSTMetadataService;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.GetRepositoryList();
    if Assigned(TObject(returnVal)) then
      callCtx.AddObjectToFree(TObject(returnVal));
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('Result',TypeInfo(TArrayOfStringRemotable),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TWSTMetadataService_ServiceBinder.GetRepositoryInfoHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IWSTMetadataService;
  callCtx : ICallContext;
  locStrPrmName : string;
  procName,trgName : string;
  AName : String;
  returnVal : TWSTMtdRepository;
begin
  callCtx := AContext;
  Fillchar(returnVal,SizeOf(TWSTMtdRepository),#0);
  
  locStrPrmName := 'AName';  AFormatter.Get(TypeInfo(String),locStrPrmName,AName);
  
  tmpObj := GetFactory().CreateInstance() as IWSTMetadataService;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.GetRepositoryInfo(AName);
    if Assigned(TObject(returnVal)) then
      callCtx.AddObjectToFree(TObject(returnVal));
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('Result',TypeInfo(TWSTMtdRepository),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    GetFactory().ReleaseInstance(tmpObj);
  end;
end;


constructor TWSTMetadataService_ServiceBinder.Create();
begin
  inherited Create(GetServiceImplementationRegistry().FindFactory('IWSTMetadataService'));
  RegisterVerbHandler('GetRepositoryList',{$IFDEF FPC}@{$ENDIF}GetRepositoryListHandler);
  RegisterVerbHandler('GetRepositoryInfo',{$IFDEF FPC}@{$ENDIF}GetRepositoryInfoHandler);
end;


{ TWSTMetadataService_ServiceBinderFactory }

function TWSTMetadataService_ServiceBinderFactory.CreateInstance():IInterface;
begin
  Result := FInstance;
end;

constructor TWSTMetadataService_ServiceBinderFactory.Create();
begin
  FInstance := TWSTMetadataService_ServiceBinder.Create();
end;

destructor TWSTMetadataService_ServiceBinderFactory.Destroy();
begin
  FInstance := nil;
  inherited Destroy();
end;


procedure Server_service_RegisterWSTMetadataServiceService();
Begin
  GetServerServiceRegistry().Register('IWSTMetadataService',TWSTMetadataService_ServiceBinderFactory.Create());
End;

initialization

  {$i metadata_service.wst}

  {$IF DECLARED(Register_metadata_service_ServiceMetadata)}
  Register_metadata_service_ServiceMetadata();
  {$IFEND}

End.
