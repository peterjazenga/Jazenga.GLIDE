{
This unit has been produced by ws_helper.
  Input unit name : "user_service_intf".
  This unit name  : "user_service_intf_proxy".
  Date            : "24-3-10 14:38:55".
}

Unit user_service_intf_proxy;
{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}
Interface

Uses SysUtils, Classes, TypInfo, base_service_intf, service_intf, user_service_intf;

Type


  TUserService_Proxy=class(TBaseProxy,UserService)
  Protected
    class function GetServiceType() : PTypeInfo;override;
    function GetList():TUserArray;
    procedure Add(
      const  AUser : TUser
    );
    procedure Update(
      const  AUser : TUser
    );
    function Find(
      const  AName : string
    ):TUser;
    function Delete(
      const  AName : string
    ):boolean;
  End;

  Function wst_CreateInstance_UserService(const AFormat : string = 'SOAP:'; const ATransport : string = 'HTTP:'; const AAddress : string = ''):UserService;

Implementation
uses wst_resources_imp, metadata_repository;


Function wst_CreateInstance_UserService(const AFormat : string; const ATransport : string; const AAddress : string):UserService;
Var
  locAdr : string;
Begin
  locAdr := AAddress;
  if ( locAdr = '' ) then
    locAdr := GetServiceDefaultAddress(TypeInfo(UserService));
  Result := TUserService_Proxy.Create('UserService',AFormat+GetServiceDefaultFormatProperties(TypeInfo(UserService)),ATransport + 'address=' + locAdr);
End;

{ TUserService_Proxy implementation }

class function TUserService_Proxy.GetServiceType() : PTypeInfo;
begin
  result := TypeInfo(UserService);
end;

function TUserService_Proxy.GetList():TUserArray;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('GetList', GetTarget(),locCallContext);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      TObject(Result) := Nil;
      locStrPrmName := 'result';
      locSerializer.Get(TypeInfo(TUserArray), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

procedure TUserService_Proxy.Add(
  const  AUser : TUser
);
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('Add', GetTarget(),locCallContext);
      locSerializer.Put('AUser', TypeInfo(TUser), AUser);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);

  Finally
    locSerializer.Clear();
  End;
End;

procedure TUserService_Proxy.Update(
  const  AUser : TUser
);
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('Update', GetTarget(),locCallContext);
      locSerializer.Put('AUser', TypeInfo(TUser), AUser);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);

  Finally
    locSerializer.Clear();
  End;
End;

function TUserService_Proxy.Find(
  const  AName : string
):TUser;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('Find', GetTarget(),locCallContext);
      locSerializer.Put('AName', TypeInfo(string), AName);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      TObject(Result) := Nil;
      locStrPrmName := 'result';
      locSerializer.Get(TypeInfo(TUser), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TUserService_Proxy.Delete(
  const  AName : string
):boolean;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('Delete', GetTarget(),locCallContext);
      locSerializer.Put('AName', TypeInfo(string), AName);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      locStrPrmName := 'result';
      locSerializer.Get(TypeInfo(boolean), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;


initialization
  {$i user_service_intf.wst}

  {$IF DECLARED(Register_user_service_intf_ServiceMetadata)}
  Register_user_service_intf_ServiceMetadata();
  {$IFEND}
End.
