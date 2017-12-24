
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}
unit
agg_conv_contour;

interface

{$I agg_mode.inc }

uses
  agg_basics,
  agg_vcgen_contour,
  agg_conv_adaptor_vcgen,
  agg_vertex_source;

type

  conv_contour = object(conv_adaptor_vcgen)
    the_generator: vcgen_contour;
    constructor Construct(vs: vertex_source_ptr);
    destructor Destruct; virtual;
    procedure line_join_(lj: unsigned);
    procedure inner_join_(ij: unsigned);
    procedure width_(w: double);
    procedure miter_limit_(ml: double);
    procedure miter_limit_theta_(t: double);
    procedure inner_miter_limit_(ml: double);
    procedure approximation_scale_(_as_: double);
    procedure auto_detect_orientation_(v: boolean);
    function _line_join: unsigned;
    function _inner_join: unsigned;
    function _width: double;
    function _miter_limit: double;
    function _inner_miter_limit: double;
    function _approximation_scale: double;
    function _auto_detect_orientation: boolean;

  end;


implementation

constructor conv_contour.Construct(vs: vertex_source_ptr);
begin
  the_generator.Construct;
  inherited Construct(vs, @the_generator);
end;

destructor conv_contour.Destruct;
begin
  inherited Destruct;

  the_generator.Destruct;
end;

procedure conv_contour.line_join_(lj: unsigned);
begin
  vcgen_contour_ptr(generator)^.line_join_(lj);
end;

procedure conv_contour.inner_join_(ij: unsigned);
begin
  vcgen_contour_ptr(generator)^.inner_join_(ij);
end;

procedure conv_contour.width_(w: double);
begin
  vcgen_contour_ptr(generator)^.width_(w);
end;

procedure conv_contour.miter_limit_(ml: double);
begin
  vcgen_contour_ptr(generator)^.miter_limit_(ml);
end;

procedure conv_contour.miter_limit_theta_(t: double);
begin
  vcgen_contour_ptr(generator)^.miter_limit_theta_(t);
end;

procedure conv_contour.inner_miter_limit_(ml: double);
begin
  vcgen_contour_ptr(generator)^.inner_miter_limit_(ml);
end;

procedure conv_contour.approximation_scale_(_as_: double);
begin
  vcgen_contour_ptr(generator)^.approximation_scale_(_as_);
end;

procedure conv_contour.auto_detect_orientation_(v: boolean);
begin
  vcgen_contour_ptr(generator)^.auto_detect_orientation_(v);
end;

function conv_contour._line_join: unsigned;
begin
  Result := vcgen_contour_ptr(generator)^._line_join;
end;

function conv_contour._inner_join: unsigned;
begin
  Result := vcgen_contour_ptr(generator)^._inner_join;
end;

function conv_contour._width: double;
begin
  Result := vcgen_contour_ptr(generator)^._width;
end;

function conv_contour._miter_limit: double;
begin
  Result := vcgen_contour_ptr(generator)^._miter_limit;
end;

function conv_contour._inner_miter_limit: double;
begin
  Result := vcgen_contour_ptr(generator)^._inner_miter_limit;
end;

function conv_contour._approximation_scale: double;
begin
  Result := vcgen_contour_ptr(generator)^._approximation_scale;
end;

function conv_contour._auto_detect_orientation: boolean;
begin
  Result := vcgen_contour_ptr(generator)^._auto_detect_orientation;

end;

end.

