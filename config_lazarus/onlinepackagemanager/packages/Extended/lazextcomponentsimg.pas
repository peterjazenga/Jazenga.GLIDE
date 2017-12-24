{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lazextcomponentsimg;

interface

uses
  U_ExtDBImage, U_ExtImage, U_ExtPictCombo, U_ExtMapImageIndex, 
  u_extimagelist, U_ExtDBPictCombo, U_ExtDBImageList, u_buttons_defs, 
  u_extdbgrid, u_extcustomimages, u_extdbimages, U_ExtImages, U_DBListView, 
  U_GroupView, fonctions_images, ExtJvXPCheckCtrls, extjvxpcoreutils, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('lazextcomponentsimg', @Register);
end.
