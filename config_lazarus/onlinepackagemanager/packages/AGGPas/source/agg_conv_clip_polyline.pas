
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}
unit agg_conv_clip_polyline;

interface

{$I agg_mode.inc }

uses
  agg_basics,
  agg_conv_adaptor_vpgen,
  agg_vpgen_clip_polyline,
  agg_vertex_source;

type

  conv_clip_polyline = object(conv_adaptor_vpgen)
    the_generator: vpgen_clip_polyline;
    constructor Construct(vs: vertex_source_ptr);
    destructor Destruct; virtual;
    procedure clip_box_(x1, y1, x2, y2: double);
    function _x1: double;
    function _y1: double;
    function _x2: double;
    function _y2: double;
  end;


implementation

constructor conv_clip_polyline.Construct(vs: vertex_source_ptr);
begin
  the_generator.Construct;
  inherited Construct(vs, @the_generator);
end;

destructor conv_clip_polyline.Destruct;
begin
  inherited Destruct;
  the_generator.Destruct;
end;

procedure conv_clip_polyline.clip_box_(x1, y1, x2, y2: double);
begin
  vpgen_clip_polyline_ptr(vpgen)^.clip_box_(x1, y1, x2, y2);
end;

function conv_clip_polyline._x1: double;
begin
  Result := vpgen_clip_polyline_ptr(vpgen)^._x1;
end;

function conv_clip_polyline._y1: double;
begin
  Result := vpgen_clip_polyline_ptr(vpgen)^._y1;
end;

function conv_clip_polyline._x2: double;
begin
  Result := vpgen_clip_polyline_ptr(vpgen)^._x2;
end;

function conv_clip_polyline._y2: double;
begin
  Result := vpgen_clip_polyline_ptr(vpgen)^._y2;
end;

end.


