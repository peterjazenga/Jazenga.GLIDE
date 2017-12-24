program demoservice;

{$mode objfpc}{$H+}

uses
  fpCGI, wmhandler, imp_helper, user_service_intf, user_service_intf_binder,
  user_service_intf_imp, user_service_intf_proxy, wst_core;

{$R *.res}

begin
  Application.Initialize;
  Application.AllowDefaultModule:=True;
  Application.Run;
end.

