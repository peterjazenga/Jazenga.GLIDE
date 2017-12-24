
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}
unit
agg_conv_shorten_path;

interface

{$I agg_mode.inc }

uses
  agg_basics,
  agg_conv_adaptor_vcgen,
  agg_vcgen_vertex_sequence,
  agg_vertex_source;

type
  conv_shorten_path = object(conv_adaptor_vcgen)
    the_generator: vcgen_vertex_sequence;
    constructor Construct(vs: vertex_source_ptr);
    destructor Destruct; virtual;
    procedure shorten_(s: double);
    function _shorten: double;
  end;

implementation

constructor conv_shorten_path.Construct(vs: vertex_source_ptr);
begin
  the_generator.Construct;

  inherited Construct(vs, @the_generator);
end;

destructor conv_shorten_path.Destruct;
begin
  inherited Destruct;

  the_generator.Destruct;
end;

procedure conv_shorten_path.shorten_(s: double);
begin
  the_generator.shorten_(s);
end;

function conv_shorten_path._shorten: double;
begin
  Result := the_generator._shorten;
end;

end.


