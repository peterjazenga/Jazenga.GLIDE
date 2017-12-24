{
This unit has been produced by ws_helper.
  Input unit name : "metadata_service".
  This unit name  : "metadata_service".
  Date            : "23/08/2010 16:10:07".
}
unit metadata_service;
{$IFDEF FPC}
  {$mode objfpc} {$H+}
{$ENDIF}
{$IFNDEF FPC}
  {$DEFINE WST_RECORD_RTTI}
{$ENDIF}
interface

uses 
  SysUtils, Classes, TypInfo, base_service_intf, service_intf, 
  metadata_repository;

const
  sNAME_SPACE = 'metadata_service';
  sUNIT_NAME = 'metadata_service';

type

  TWSTMtdOperationParam = class;
  TWSTMtdServiceOperation = class;
  TWSTMtdService = class;
  TWSTMtdRepository = class;
  TWSTMtdOperationParamArray = class;
  TWSTMtdServiceOperationArray = class;
  TWSTMtdServiceArray = class;

  TWSTMtdOperationParam = class(TBaseComplexRemotable)
  private
    FName : String;
    FTypeName : String;
    FModifier : TOperationParamFlag;
  published
    property Name : String read FName write FName;
    property TypeName : String read FTypeName write FTypeName;
    property Modifier : TOperationParamFlag read FModifier write FModifier;
  end;

  TWSTMtdServiceOperation = class(TBaseComplexRemotable)
  private
    FName : String;
    FParams : TWSTMtdOperationParamArray;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Name : String read FName write FName;
    property Params : TWSTMtdOperationParamArray read FParams write FParams;
  end;

  TWSTMtdService = class(TBaseComplexRemotable)
  private
    FName : String;
    FOperations : TWSTMtdServiceOperationArray;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Name : String read FName write FName;
    property Operations : TWSTMtdServiceOperationArray read FOperations write FOperations;
  end;

  TWSTMtdRepository = class(TBaseComplexRemotable)
  private
    FName : String;
    FNameSpace : String;
    FServices : TWSTMtdServiceArray;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Name : String read FName write FName;
    property NameSpace : String read FNameSpace write FNameSpace;
    property Services : TWSTMtdServiceArray read FServices write FServices;
  end;

  TWSTMtdOperationParamArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TWSTMtdOperationParam;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TWSTMtdOperationParam Read GetItem;Default;
  end;

  TWSTMtdServiceOperationArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TWSTMtdServiceOperation;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TWSTMtdServiceOperation Read GetItem;Default;
  end;

  TWSTMtdServiceArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TWSTMtdService;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TWSTMtdService Read GetItem;Default;
  end;

  IWSTMetadataService = interface(IInvokable)
    ['{804A3825-ADA5-4499-87BF-CF5491BFD674}']
    function GetRepositoryList():TArrayOfStringRemotable;
    function GetRepositoryInfo(
      const  AName : String
    ):TWSTMtdRepository;
  end;

  procedure Register_metadata_service_ServiceMetadata();

Implementation
uses 
  record_rtti, wst_types;

{ TWSTMtdServiceOperation }

constructor TWSTMtdServiceOperation.Create();
begin
  inherited Create();
  FParams := TWSTMtdOperationParamArray.Create();
end;

procedure TWSTMtdServiceOperation.FreeObjectProperties();
begin
  if Assigned(FParams) then
    FreeAndNil(FParams);
  inherited FreeObjectProperties();
end;

{ TWSTMtdService }

constructor TWSTMtdService.Create();
begin
  inherited Create();
  FOperations := TWSTMtdServiceOperationArray.Create();
end;

procedure TWSTMtdService.FreeObjectProperties();
begin
  if Assigned(FOperations) then
    FreeAndNil(FOperations);
  inherited FreeObjectProperties();
end;

{ TWSTMtdRepository }

constructor TWSTMtdRepository.Create();
begin
  inherited Create();
  FServices := TWSTMtdServiceArray.Create();
end;

procedure TWSTMtdRepository.FreeObjectProperties();
begin
  if Assigned(FServices) then
    FreeAndNil(FServices);
  inherited FreeObjectProperties();
end;

{ TWSTMtdOperationParamArray }

function TWSTMtdOperationParamArray.GetItem(AIndex: Integer): TWSTMtdOperationParam;
begin
  Result := TWSTMtdOperationParam(Inherited GetItem(AIndex));
end;

class function TWSTMtdOperationParamArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TWSTMtdOperationParam;
end;

{ TWSTMtdServiceOperationArray }

function TWSTMtdServiceOperationArray.GetItem(AIndex: Integer): TWSTMtdServiceOperation;
begin
  Result := TWSTMtdServiceOperation(Inherited GetItem(AIndex));
end;

class function TWSTMtdServiceOperationArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TWSTMtdServiceOperation;
end;

{ TWSTMtdServiceArray }

function TWSTMtdServiceArray.GetItem(AIndex: Integer): TWSTMtdService;
begin
  Result := TWSTMtdService(Inherited GetItem(AIndex));
end;

class function TWSTMtdServiceArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TWSTMtdService;
end;


procedure Register_metadata_service_ServiceMetadata();
var
  mm : IModuleMetadataMngr;
begin
  mm := GetModuleMetadataMngr();
  mm.SetRepositoryNameSpace(sUNIT_NAME, sNAME_SPACE);
  mm.SetServiceCustomData(
    sUNIT_NAME,
    'IWSTMetadataService',
    'FORMAT_Style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IWSTMetadataService',
    'GetRepositoryList',
    '_E_N_',
    'GetRepositoryList'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IWSTMetadataService',
    'GetRepositoryInfo',
    '_E_N_',
    'GetRepositoryInfo'
  );
end;


var
  typeRegistryInstance : TTypeRegistry = nil;
initialization
  typeRegistryInstance := GetTypeRegistry();

  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TWSTMtdOperationParam),'TWSTMtdOperationParam');
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TWSTMtdServiceOperation),'TWSTMtdServiceOperation');
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TWSTMtdService),'TWSTMtdService');
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TWSTMtdRepository),'TWSTMtdRepository');
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TWSTMtdOperationParamArray),'TWSTMtdOperationParamArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TWSTMtdOperationParamArray)].RegisterExternalPropertyName(sARRAY_ITEM,'Item');
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TWSTMtdServiceOperationArray),'TWSTMtdServiceOperationArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TWSTMtdServiceOperationArray)].RegisterExternalPropertyName(sARRAY_ITEM,'_Item');
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TWSTMtdServiceArray),'TWSTMtdServiceArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TWSTMtdServiceArray)].RegisterExternalPropertyName(sARRAY_ITEM,'Item');



End.
