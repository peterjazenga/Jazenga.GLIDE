
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}
unit
agg_conv_dash;

interface

{$I agg_mode.inc }

uses
  agg_basics,
  agg_vertex_source,
  agg_conv_adaptor_vcgen,
  agg_vcgen_dash;

type

  conv_dash = object(conv_adaptor_vcgen)
    the_generator: vcgen_dash;
    constructor Construct(vs: vertex_source_ptr);
    destructor Destruct; virtual;
    procedure remove_all_dashes;
    procedure add_dash(dash_len, gap_len: double);
    procedure dash_start(ds: double);
    procedure shorten_(s: double);
    function _shorten: double;
  end;

implementation

constructor conv_dash.Construct(vs: vertex_source_ptr);
begin
  the_generator.Construct;
  inherited Construct(vs, @the_generator);
end;

destructor conv_dash.Destruct;
begin
  inherited Destruct;
  the_generator.Destruct;
end;

procedure conv_dash.remove_all_dashes;
begin
  vcgen_dash_ptr(generator)^.remove_all_dashes;
end;

procedure conv_dash.add_dash(dash_len, gap_len: double);
begin
  vcgen_dash_ptr(generator)^.add_dash(dash_len, gap_len);
end;

procedure conv_dash.dash_start(ds: double);
begin
  vcgen_dash_ptr(generator)^.dash_start(ds);
end;

procedure conv_dash.shorten_(s: double);
begin
  vcgen_dash_ptr(generator)^.shorten_(s);
end;

function conv_dash._shorten: double;
begin
  Result := vcgen_dash_ptr(generator)^._shorten;
end;

end.

