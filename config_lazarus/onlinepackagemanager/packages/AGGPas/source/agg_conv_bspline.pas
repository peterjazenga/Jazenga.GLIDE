
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}
unit
agg_conv_bspline;

interface

{$I agg_mode.inc }

uses
  agg_basics,
  agg_vcgen_bspline,
  agg_conv_adaptor_vcgen,
  agg_vertex_source;

type
  conv_bspline = object(conv_adaptor_vcgen)
    the_generator: vcgen_bspline;

    constructor Construct(vs: vertex_source_ptr);
    destructor Destruct; virtual;

    procedure interpolation_step_(v: double);
    function _interpolation_step: double;

  end;

implementation

constructor conv_bspline.Construct(vs: vertex_source_ptr);
begin
  the_generator.Construct;

  inherited Construct(vs, @the_generator);

end;

{ DESTRUCT }
destructor conv_bspline.Destruct;
begin
  inherited Destruct;

  the_generator.Destruct;

end;

{ INTERPOLATION_STEP_ }
procedure conv_bspline.interpolation_step_(v: double);
begin
  vcgen_bspline_ptr(generator)^.interpolation_step_(v);

end;

{ _INTERPOLATION_STEP }
function conv_bspline._interpolation_step: double;
begin
  Result := vcgen_bspline_ptr(generator)^._interpolation_step;

end;

end.

