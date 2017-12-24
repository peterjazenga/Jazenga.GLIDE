unit UCZEOSConnReg;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses Classes, LResources;

procedure Register;

implementation

uses UCZEOSConn;

procedure Register;
begin
  RegisterComponents('Data Access', [TUCZEOSConn]);
end;

initialization
  {$I uczeosconn.lrs}

end.
