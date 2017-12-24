{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit aggpasvs;

{$warn 5023 off : no warning about unused units}
interface

uses
  agg_AllRegister, agg_fpimage, agg_lcl, agg_lclcontrols, agg_lclpaintbox, 
  agx_canvas, agx_lclpaintbox, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('agg_AllRegister', @agg_AllRegister.Register);
end;

initialization
  RegisterPackage('aggpasvs', @Register);
end.
