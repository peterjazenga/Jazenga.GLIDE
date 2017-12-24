unit RxSortSqlDB;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs;

type
  TRxSortSqlDB = class(TComponent)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
  published
    { Published declarations }
  end;

procedure Register;

implementation
uses exsortsql;

procedure Register;
begin
  RegisterComponents('RX DBAware',[TRxSortSqlDB]);
end;

end.
