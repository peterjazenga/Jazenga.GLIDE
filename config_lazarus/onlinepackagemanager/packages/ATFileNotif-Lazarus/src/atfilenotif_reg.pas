unit ATFileNotif_Reg;

interface

uses
  SysUtils, Classes, Controls, LResources, ATFileNotif;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Misc', [TATFileNotif]);
end;

initialization
  //{$I res/icon.lrs}

end.
