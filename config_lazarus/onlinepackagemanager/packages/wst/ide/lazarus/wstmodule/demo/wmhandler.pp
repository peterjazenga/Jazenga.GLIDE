unit wmhandler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, HTTPDefs, websession, fpHTTP, 
  wstmodule;

type
  TWSTModule1 = class(TWSTModule)
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  WSTModule1: TWSTModule1; 

implementation

uses
  user_service_intf_binder,
  user_service_intf_imp,
  server_service_soap,
  server_service_json,
  server_service_xmlrpc;

initialization
  {$I wmhandler.lrs}
  RegisterUserServiceImplementationFactory;
  Server_service_RegisterUserServiceService;
  Server_service_RegisterSoapFormat;
  Server_service_RegisterJSONFormat;
  Server_service_RegisterXMLRPCFormat;
  RegisterHTTPModule('WST', TWSTModule1);
end.

