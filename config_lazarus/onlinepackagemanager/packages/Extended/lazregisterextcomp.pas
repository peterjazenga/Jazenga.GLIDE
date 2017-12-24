{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lazregisterextcomp;

interface

uses
  u_reginicomponents, u_regextnumeric, u_regextfilecopy, u_regimagecomponents, 
  u_regfwbuttons, u_regfwbuttons_appli, u_regsbbuttons, 
  u_regreports_components, u_regextracomponents, u_registerforms, 
  u_register_net, u_regextcomponents, U_RegisterGroupView, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('u_reginicomponents', @u_reginicomponents.Register);
  RegisterUnit('u_regextnumeric', @u_regextnumeric.Register);
  RegisterUnit('u_regextfilecopy', @u_regextfilecopy.Register);
  RegisterUnit('u_regimagecomponents', @u_regimagecomponents.Register);
  RegisterUnit('u_regfwbuttons', @u_regfwbuttons.Register);
  RegisterUnit('u_regfwbuttons_appli', @u_regfwbuttons_appli.Register);
  RegisterUnit('u_regsbbuttons', @u_regsbbuttons.Register);
  RegisterUnit('u_regreports_components', @u_regreports_components.Register);
  RegisterUnit('u_regextracomponents', @u_regextracomponents.Register);
  RegisterUnit('u_registerforms', @u_registerforms.Register);
  RegisterUnit('u_register_net', @u_register_net.Register);
  RegisterUnit('u_regextcomponents', @u_regextcomponents.Register);
  RegisterUnit('U_RegisterGroupView', @U_RegisterGroupView.Register);
end;

initialization
  RegisterPackage('lazregisterextcomp', @Register);
end.
