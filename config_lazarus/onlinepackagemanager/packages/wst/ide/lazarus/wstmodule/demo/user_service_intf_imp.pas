{
This unit has been produced by ws_helper.
  Input unit name : "user_service_intf".
  This unit name  : "user_service_intf_imp".
  Date            : "30/04/2007 00:07".
}
Unit user_service_intf_imp;
{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}
Interface

Uses SysUtils, Classes, 
     base_service_intf, server_service_intf, server_service_imputils,
     user_service_intf, cursor_intf;

Type

  { TUserService_ServiceImp }

  TUserService_ServiceImp=class(TActivableServiceImplementation,UserService)
  protected
    procedure Activate();override;
    procedure Deactivate();override;
    function CanBePooled() : Boolean;override;
  Protected
    function GetList():TUserArray;
    procedure Add(
      Const AUser : TUser
    );
    procedure Update(
      Const AUser : TUser
    );
    function Find(
      Const AName : string
    ):TUser;
    function Delete(
      Const AName : string
    ):boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  End;

  const sDATA_FILE_NAME = 'sample.data';

  procedure RegisterUserServiceImplementationFactory();
  procedure SaveDataToFile(const AFileName : string);

Implementation
uses Contnrs, std_cursors, rtti_filters, imp_helper, config_objects;

var
  FUserList : TObjectList = nil;
  FUserCursor : IObjectCursor = nil;

procedure FillArrayFromCursor(ACursor: IObjectCursor;ARes: TUserArray);
var
  i, c : Integer;
begin
  ACursor.Reset();
  c := 0;
  while ACursor.MoveNext() do begin
    Inc(c);
  end;
  ARes.SetLength(c);
  i := 0;
  ACursor.Reset();
  while ACursor.MoveNext() do begin
    ARes[i].Assign(ACursor.GetCurrent() as TUser);
    Inc(i);
  end;
end;

{ TUserService_ServiceImp implementation }
function TUserService_ServiceImp.GetList():TUserArray;
Begin
  Result := TUserArray.Create();
  try
    FillArrayFromCursor(FUserCursor.Clone() as IObjectCursor,Result);
  except
    FreeAndNil(Result);
    raise;
  end;
  SaveDataToFile(sDATA_FILE_NAME);
End;

procedure TUserService_ServiceImp.Add(Const AUser : TUser);
var
  locObj : TUser;
Begin
  locObj := Find(AUser.UserName);
  if ( locObj <> nil ) then
    raise Exception.CreateFmt('Duplicated user : "%s"',[AUser.UserName]);
  locObj := TUser.Create();
  locObj.Assign(AUser);
  FUserList.Add(locObj);
End;

procedure TUserService_ServiceImp.Update(const AUser: TUser);
var
  locCrs : IObjectCursor;
Begin
  locCrs := FUserCursor.Clone() as IObjectCursor;
  locCrs.Reset();
  locCrs := CreateCursorOn(locCrs,ParseFilter(Format('%s=%s',['UserName',QuotedStr(AUser.UserName)]),TUser));
  if locCrs.MoveNext() then begin
    (locCrs.GetCurrent() as TUser).Assign(AUser);
  end;
end;

function TUserService_ServiceImp.Find(Const AName : string):TUser;
var
  locCrs : IObjectCursor;
Begin
  Result := nil;
  locCrs := FUserCursor.Clone() as IObjectCursor;
  locCrs.Reset();
  locCrs := CreateCursorOn(locCrs,ParseFilter(Format('%s=%s',['UserName',QuotedStr(AName)]),TUser));
  if locCrs.MoveNext() then begin
    Result := TUser.Create();
    Result.Assign(locCrs.GetCurrent() as TUser);
  end;
End;

function TUserService_ServiceImp.Delete(Const AName : string):boolean;
var
  locCrs : IObjectCursor;
Begin
  Result := False;
  locCrs := FUserCursor.Clone() as IObjectCursor;
  locCrs.Reset();
  locCrs := CreateCursorOn(locCrs,ParseFilter(Format('%s=%s',['UserName',QuotedStr(AName)]),TUser));
  if locCrs.MoveNext() then begin
    FUserList.Delete(FUserList.IndexOf(locCrs.GetCurrent() as TUser));
    Result := True;
  end;
End;

procedure RegisterUserServiceImplementationFactory();
Begin
  GetServiceImplementationRegistry().Register('UserService',TImplementationFactory.Create(TUserService_ServiceImp,wst_GetServiceConfigText('UserService')) as IServiceImplementationFactory);
End;

procedure FillSampleData();
var
  locUser : TUser;
begin
  locUser := TUser.Create();
  locUser.UserName := 'Lazarus FreePascal';
  locUser.eMail := 'Lazarus@FreePascal.wst';
  FUserList.Add(locUser);
  locUser := TUser.Create();
  locUser.UserName := 'Inoussa OUEDRAOGO';
  locUser.eMail := 'sample@example.wst';
  FUserList.Add(locUser);
end;

procedure FillDataFromFile(const AFileName : string);
var
  objArray : TUserArray;
  i : Integer;
  obj : TUser;
begin
  objArray := TUserArray.Create();
  try
    LoadObjectFromFile(objArray,AFileName);
    FUserList.Clear();
    for i := 0 to Pred(objArray.Length) do begin
      obj := TUser.Create();
      FUserList.Add(obj);
      obj.Assign(objArray[i]);
    end;
  finally
    FreeAndNil(objArray);
  end;
end;

procedure SaveDataToFile(const AFileName : string);
var
  objArray : TUserArray;
begin
  objArray := TUserArray.Create();
  try
    FUserCursor.Reset();
    FillArrayFromCursor(FUserCursor,objArray);
    SaveObjectToFile(objArray,AFileName);
  finally
    FreeAndNil(objArray);
  end;
end;

constructor TUserService_ServiceImp.Create;
begin
  inherited;
  WriteLn('TUserService_ServiceImp.Create();');
end;

procedure TUserService_ServiceImp.Activate;
begin
  inherited;
  WriteLn(Format('TUserService_ServiceImp.Activate(), Self = %p',[Pointer(Self)]));
end;

function TUserService_ServiceImp.CanBePooled: Boolean;
begin
  Result := True;
end;

procedure TUserService_ServiceImp.Deactivate;
begin
  WriteLn(Format('TUserService_ServiceImp.Deactivate(), Self = %p',[Pointer(Self)]));
  inherited;
end;

destructor TUserService_ServiceImp.Destroy;
begin
  WriteLn('TUserService_ServiceImp.Destroy();');
  inherited;
end;

initialization
  FUserList := TObjectList.Create(True);
  FUserCursor := TObjectListCursor.Create(FUserList);
  if FileExists(sDATA_FILE_NAME) then
    FillDataFromFile(sDATA_FILE_NAME)
  else
    FillSampleData();

finalization
  if Assigned(FUserCursor) then
    SaveDataToFile(sDATA_FILE_NAME);
  FUserCursor := nil;
  FreeAndNil(FUserList);
  
end.
