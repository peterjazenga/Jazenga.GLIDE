{ This file was automatically created by Typhon IDE. Do not edit!
  This source is only used to compile and install the package.
 }

unit typhonregisterextcomp;

{$warn 5023 off : no warning about unused units}
interface

uses
  u_regextcomponents, u_regextfilecopy, u_regextnumeric, u_regextracomponents, 
  u_regfwbuttons, u_regfwbuttons_appli, u_regimagecomponents, 
  u_reginicomponents, u_registerforms, U_RegisterGroupView, u_register_net, 
  u_regreports_components, u_regsbbuttons, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('u_regextcomponents', @u_regextcomponents.Register);
  RegisterUnit('u_regextfilecopy', @u_regextfilecopy.Register);
  RegisterUnit('u_regextnumeric', @u_regextnumeric.Register);
  RegisterUnit('u_regextracomponents', @u_regextracomponents.Register);
  RegisterUnit('u_regfwbuttons', @u_regfwbuttons.Register);
  RegisterUnit('u_regfwbuttons_appli', @u_regfwbuttons_appli.Register);
  RegisterUnit('u_regimagecomponents', @u_regimagecomponents.Register);
  RegisterUnit('u_reginicomponents', @u_reginicomponents.Register);
  RegisterUnit('u_registerforms', @u_registerforms.Register);
  RegisterUnit('U_RegisterGroupView', @U_RegisterGroupView.Register);
  RegisterUnit('u_register_net', @u_register_net.Register);
  RegisterUnit('u_regreports_components', @u_regreports_components.Register);
  RegisterUnit('u_regsbbuttons', @u_regsbbuttons.Register);
end;

initialization
  RegisterPackage('typhonregisterextcomp', @Register);
end.
