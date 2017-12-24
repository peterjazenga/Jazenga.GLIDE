unit RxLazReport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs;

type
  TRxLazReport = class(TComponent)
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
uses lrRxControls;

procedure Register;
begin
  RegisterComponents('LazReport',[TRxLazReport]);
end;

end.
