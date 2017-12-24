unit attabs_register;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, 
  ATTabs,
  LResources;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Misc', [TATTabs]);
end;

initialization
  {$I res/icons.lrs}

end.

