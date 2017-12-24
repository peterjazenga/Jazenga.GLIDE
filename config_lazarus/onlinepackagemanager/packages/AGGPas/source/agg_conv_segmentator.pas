
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}
unit
agg_conv_segmentator;

interface

{$I agg_mode.inc }

uses
  agg_basics,
  agg_vertex_source,
  agg_conv_adaptor_vpgen,
  agg_vpgen_segmentator;

type

  conv_segmentator = object(conv_adaptor_vpgen)
    the_generator: vpgen_segmentator;
    constructor Construct(vs: vertex_source_ptr);
    destructor Destruct; virtual;
    procedure approximation_scale_(s: double);
    function _approximation_scale: double;
  end;

implementation

constructor conv_segmentator.Construct(vs: vertex_source_ptr);
begin
  the_generator.Construct;
  inherited Construct(vs, @the_generator);
end;

destructor conv_segmentator.Destruct;
begin
  inherited Destruct;
  the_generator.Destruct;
end;

procedure conv_segmentator.approximation_scale_(s: double);
begin
  vpgen^.approximation_scale_(s);
end;

function conv_segmentator._approximation_scale: double;
begin
  Result := vpgen^._approximation_scale;
end;

end.

