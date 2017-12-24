{
This unit has been produced by ws_helper.
  Input unit name : "metadata_service".
  This unit name  : "metadata_service_imp".
  Date            : "01/07/2006 22:14".
}
{$INCLUDE wst_global.inc}
Unit metadata_service_imp;

Interface

Uses SysUtils, Classes, 
     base_service_intf, server_service_intf, server_service_imputils, metadata_service;

Type

  TWSTMetadataService_ServiceImp=class(TSimpleFactoryItem,IWSTMetadataService)
  Protected
    function GetRepositoryList():TArrayOfStringRemotable;
    function GetRepositoryInfo(
      Const AName : string
    ):TWSTMtdRepository;
  End;


  procedure RegisterWSTMetadataServiceImplementationFactory();

Implementation
uses metadata_repository;

{ TWSTMetadataService_ServiceImp implementation }
function TWSTMetadataService_ServiceImp.GetRepositoryList():TArrayOfStringRemotable;
var
  i, c : Integer;
  mn : IModuleMetadataMngr;
Begin
  Result := TArrayOfStringRemotable.Create();
  try
    mn := GetModuleMetadataMngr();
    c := mn.GetCount();
    Result.SetLength(c);
    for i := 0 to Pred(c) do
      Result[i] := mn.GetRepositoryName(i);
  except
    FreeAndNil(Result);
    raise;
  end;
End;

function TWSTMetadataService_ServiceImp.GetRepositoryInfo(Const AName : string):TWSTMtdRepository;

  procedure LoadService(ARawServ : PService; AObjServ : TWSTMtdService);

    procedure LoadOperation(ARawOper : PServiceOperation; AObjOper : TWSTMtdServiceOperation);

      procedure LoadParam(ARawParam : POperationParam; AObjPrm : TWSTMtdOperationParam);
      begin
        if Assigned(ARawParam) and Assigned(AObjPrm) then begin
          AObjPrm.Name :=ARawParam^.Name;
          AObjPrm.TypeName := ARawParam^.TypeName;
          AObjPrm.Modifier := ARawParam^.Modifier;
        end;
      end;

    var
      ii, cc : Integer;
      pprm : POperationParam;
    begin
      if Assigned(ARawOper) and Assigned(AObjOper) then begin
        AObjOper.Name :=ARawOper^.Name;
        cc := ARawOper^.ParamsCount;
        AObjOper.Params.SetLength(cc);
        if ( cc > 0 ) then begin
          pprm := ARawOper^.Params;
          for ii := 0 to Pred(cc) do begin
            LoadParam(pprm,AObjOper.Params[ii]);
            Inc(pprm);
          end;
        end;
      end;
    end;

  var
    k, d : Integer;
    pservOP : PServiceOperation;
  begin
    if Assigned(ARawServ) and Assigned(AObjServ) then begin
      AObjServ.Name :=ARawServ^.Name;
      d := ARawServ^.OperationsCount;
      AObjServ.Operations.SetLength(d);
      if ( d > 0 ) then begin
        pservOP := ARawServ^.Operations;
        for k := 0 to Pred(d) do begin
          LoadOperation(pservOP,AObjServ.Operations[k]);
          Inc(pservOP);
        end;
      end;
    end;
  end;

var
  repData : PServiceRepository;
  mn : IModuleMetadataMngr;
  i, c : Integer;
  pserv : PService;
Begin
  Result := nil;
  mn := GetModuleMetadataMngr();
  mn.LoadRepositoryName(AName,'/',repData);
  if Assigned(repData) then begin
    try
      try
        Result := TWSTMtdRepository.Create();
        Result.Name := repData^.Name;
        Result.NameSpace := repData^.NameSpace;
        c := repData^.ServicesCount;
        Result.Services.SetLength(c);
        if ( c > 0 ) then begin
          pserv := repData^.Services;
          for i := 0 to Pred(c) do begin
            LoadService(pserv,Result.Services[i]);
            Inc(pserv);
          end;
        end;
      except
        FreeAndNil(Result);
        raise;
      end;
    finally
      mn.ClearRepository(repData);
    end;
  end;
End;


procedure RegisterWSTMetadataServiceImplementationFactory();
Begin
  GetServiceImplementationRegistry().Register('IWSTMetadataService',TImplementationFactory.Create(TWSTMetadataService_ServiceImp));
End;

End.
