{
This unit has been produced by ws_helper.
  Input unit name : "metadata_service".
  This unit name  : "metadata_service_proxy".
  Date            : "23/08/2010 16:10:07".
}

Unit metadata_service_proxy;
{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}
Interface

Uses SysUtils, Classes, TypInfo, base_service_intf, service_intf, metadata_service;

Type


  TWSTMetadataService_Proxy=class(TBaseProxy,IWSTMetadataService)
  Protected
    class function GetServiceType() : PTypeInfo;override;
    function GetRepositoryList():TArrayOfStringRemotable;
    function GetRepositoryInfo(
      const  AName : String
    ):TWSTMtdRepository;
  End;

  Function wst_CreateInstance_IWSTMetadataService(const AFormat : string = 'SOAP:'; const ATransport : string = 'HTTP:'; const AAddress : string = ''):IWSTMetadataService;

Implementation
uses wst_resources_imp, metadata_repository;


Function wst_CreateInstance_IWSTMetadataService(const AFormat : string; const ATransport : string; const AAddress : string):IWSTMetadataService;
Var
  locAdr : string;
Begin
  locAdr := AAddress;
  if ( locAdr = '' ) then
    locAdr := GetServiceDefaultAddress(TypeInfo(IWSTMetadataService));
  Result := TWSTMetadataService_Proxy.Create('IWSTMetadataService',AFormat+GetServiceDefaultFormatProperties(TypeInfo(IWSTMetadataService)),ATransport + 'address=' + locAdr);
End;

{ TWSTMetadataService_Proxy implementation }

class function TWSTMetadataService_Proxy.GetServiceType() : PTypeInfo;
begin
  result := TypeInfo(IWSTMetadataService);
end;

function TWSTMetadataService_Proxy.GetRepositoryList():TArrayOfStringRemotable;
Var
  locSerializer : IFormatterClient;
  locStrPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('GetRepositoryList', GetTarget(),Self);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(Self);
      TObject(Result) := Nil;
      locStrPrmName := 'Result';
      locSerializer.Get(TypeInfo(TArrayOfStringRemotable), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TWSTMetadataService_Proxy.GetRepositoryInfo(
  const  AName : String
):TWSTMtdRepository;
Var
  locSerializer : IFormatterClient;
  locStrPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('GetRepositoryInfo', GetTarget(),Self);
      locSerializer.Put('AName', TypeInfo(String), AName);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(Self);
      TObject(Result) := Nil;
      locStrPrmName := 'Result';
      locSerializer.Get(TypeInfo(TWSTMtdRepository), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;


initialization
  {$i metadata_service.wst}

  {$IF DECLARED(Register_metadata_service_ServiceMetadata)}
  Register_metadata_service_ServiceMetadata();
  {$IFEND}
End.
