{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit graphics32_ext;

{$warn 5023 off : no warning about unused units}
interface

uses
  AllGraphics32ExtRegister, XGR32_Algebra, XGR32_AniEffects, 
  XGR32_AniGEffetcts, XGR32_Animation, XGR32_BitmapEx, XGR32_Blending, 
  XGR32_Blendmodes, XGR32_Bmp32Draw, XGR32_Bmp32Func, XGR32_Color, 
  XGR32_DesktopControl, XGR32_Effects, XGR32_ElasticLayers, XGR32_Emboss, 
  XGR32_ExtLayers, XGR32_ExtLayers_cursors, XGR32_ExtLayers_Panel, 
  XGR32_ExtLayers_transform, XGR32_FastFX, XGR32_FilterEx, XGR32_FlareFX, 
  XGR32_FloodFill, XGR32_GausianBlur, XGR32_Graphics, XGR32_GraphUtils, 
  XGR32_ImageList, XGR32_ParticleAniEffects, XGR32_ParticleSnow, 
  XGR32_ParticleStar, XGR32_RasterizerEx, XGR32_RedEye, XGR32_Sprites, 
  XGR32_StackBlur, XGR32_TransformsWarp, XGR32_Warping, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('AllGraphics32ExtRegister', @AllGraphics32ExtRegister.Register);
end;

initialization
  RegisterPackage('graphics32_ext', @Register);
end.
