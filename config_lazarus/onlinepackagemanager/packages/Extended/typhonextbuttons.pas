{ This file was automatically created by Typhon IDE. Do not edit!
  This source is only used to compile and install the package.
 }

unit typhonextbuttons;

{$warn 5023 off : no warning about unused units}
interface

uses
  u_buttons_appli, u_buttons_speed, u_form_msg, u_form_working, 
  fonctions_graphic_dialogs, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('typhonextbuttons', @Register);
end.
