unit user_service_intf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, base_service_intf;

type

  TUserCategory = ( Normal, Admin );
  
  { TUser }

  TUser = class(TBaseComplexRemotable)
  private
    FCategory: TUserCategory;
    FeMail: string;
    FName: string;
    FPreferences: string;
  published
    property Category : TUserCategory read FCategory write FCategory;
    property Name : string read FName write FName;
    property eMail : string read FeMail write FeMail;
    property Preferences : string read FPreferences write FPreferences;
  end;
  
  { TUserArray }

  TUserArray = class(TBaseObjectArrayRemotable)
  private
    function GetUser(AIndex: Integer): TUser;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    Property Item[AIndex:Integer] : TUser Read GetUser;Default;
  End;

  UserService = interface(IInvokable)
    ['{101F2CA7-19FC-4A73-AA98-F13FCDA75EE1}']
    function GetList():TUserArray;
    procedure Add(AUser : TUser);
    function Find(const AName : string):TUser;
    function Delete(const AName : string):Boolean;
  end;
  
  procedure Register_user_service_intf_NameSpace();
  
implementation
uses metadata_repository;

procedure Register_user_service_intf_NameSpace();
begin
  GetModuleMetadataMngr().SetRepositoryNameSpace('user_service_intf','urn:UserService');
end;


{ TUserArray }

function TUserArray.GetUser(AIndex: Integer): TUser;
begin
  Result := GetItem(AIndex) as TUser;
end;

class function TUserArray.GetItemClass(): TBaseRemotableClass;
begin
  Result := TUser;
end;

initialization
  GetTypeRegistry().Register('urn:UserService',TypeInfo(TUserCategory),'TUserCategory');
  GetTypeRegistry().Register('urn:UserService',TypeInfo(TUser),'TUser');
  GetTypeRegistry().Register('urn:UserService',TypeInfo(TUserArray),'TUserArray');

end.
