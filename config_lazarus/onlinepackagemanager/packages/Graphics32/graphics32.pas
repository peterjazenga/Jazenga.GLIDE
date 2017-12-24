{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit graphics32;

{$warn 5023 off : no warning about unused units}
interface

uses
  AllGR32Register, G32_ProgressBar, GR32, GR32_ArrowHeads, GR32_Blend, 
  GR32_Blurs, GR32_Brushes, GR32_ColorGradients, GR32_ColorPicker, 
  GR32_Containers, GR32_Dsgn_Bitmap, GR32_Dsgn_Color, GR32_Dsgn_Misc, 
  GR32_ExtImage, GR32_Filters, GR32_Geometry, GR32_Image, GR32_Layers, 
  GR32_LowLevel, GR32_Math, GR32_MicroTiles, GR32_OrdinalMaps, GR32_Panel32, 
  GR32_Paths, GR32_Polygons, GR32_RangeBars, GR32_Rasterizers, 
  GR32_RepaintOpt, GR32_Resamplers, GR32_Scrollbox, GR32_System, 
  GR32_Transforms, GR32_VectorMaps, GR32_VectorUtils, GR32_VPR, GR32_VPR2, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('AllGR32Register', @AllGR32Register.Register);
end;

initialization
  RegisterPackage('graphics32', @Register);
end.
