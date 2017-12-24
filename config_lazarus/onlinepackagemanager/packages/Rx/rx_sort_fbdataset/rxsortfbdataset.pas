unit RxSortFBDataSet;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TRxSortFBDataSet = class(TComponent)
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
uses exsortfb;

procedure Register;
begin
  RegisterComponents('RX DBAware',[TRxSortFBDataSet]);
end;

end.
