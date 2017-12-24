{ This file was automatically created by Typhon IDE. Do not edit!
  This source is only used to compile and install the package.
 }

unit typhonextcomponentsimg;

{$warn 5023 off : no warning about unused units}
interface

uses
  U_ExtDBImage, U_ExtImage, U_ExtPictCombo, U_ExtMapImageIndex, 
  u_extimagelist, U_ExtDBPictCombo, U_ExtDBImageList, u_buttons_defs, 
  u_extdbgrid, U_DBListView, U_GroupView, u_extcustomimages, u_extdbimages, 
  U_ExtImages, ExtJvXPCheckCtrls, extjvxpcoreutils, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('typhonextcomponentsimg', @Register);
end.
