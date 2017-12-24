
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}
unit
agg_conv_stroke;

interface

{$I agg_mode.inc }

uses
  agg_basics,
  agg_vertex_source,
  agg_vcgen_stroke,
  agg_conv_adaptor_vcgen;

type

  conv_stroke_ptr = ^conv_stroke;

  conv_stroke = object(conv_adaptor_vcgen)
    the_generator: vcgen_stroke;
    constructor Construct(vs: vertex_source_ptr);
    destructor Destruct; virtual;
    procedure line_cap_(lc: unsigned);
    procedure line_join_(lj: unsigned);
    procedure inner_join_(ij: unsigned);
    function _line_cap: unsigned;
    function _line_join: unsigned;
    function _inner_join: unsigned;
    procedure width_(w: double);
    procedure miter_limit_(ml: double);
    procedure miter_limit_theta_(t: double);
    procedure inner_miter_limit_(ml: double);
    procedure approximation_scale_(_as_: double);
    function _width: double;
    function _miter_limit: double;
    function _inner_miter_limit: double;
    function _approximation_scale: double;
    procedure shorten_(s: double);
    function _shorten: double;
  end;

  conv_stroke_math = object(conv_adaptor_vcgen)
    the_generator: vcgen_stroke_math;
    constructor Construct(vs: vertex_source_ptr);
    destructor Destruct; virtual;
    procedure line_cap_(lc: unsigned);
    procedure line_join_(lj: unsigned);
    procedure inner_join_(ij: unsigned);
    function _line_cap: unsigned;
    function _line_join: unsigned;
    function _inner_join: unsigned;
    procedure width_(w: double);
    procedure miter_limit_(ml: double);
    procedure miter_limit_theta_(t: double);
    procedure inner_miter_limit_(ml: double);
    procedure approximation_scale_(_as_: double);
    function _width: double;
    function _miter_limit: double;
    function _inner_miter_limit: double;
    function _approximation_scale: double;
    procedure shorten_(s: double);
    function _shorten: double;
  end;


implementation

constructor conv_stroke.Construct(vs: vertex_source_ptr);
begin
  the_generator.Construct;

  inherited Construct(vs, @the_generator);

end;

destructor conv_stroke.Destruct;
begin
  inherited Destruct;
  the_generator.Destruct;
end;

procedure conv_stroke.line_cap_(lc: unsigned);
begin
  vcgen_stroke_ptr(generator)^.line_cap_(lc);
end;

procedure conv_stroke.line_join_(lj: unsigned);
begin
  vcgen_stroke_ptr(generator)^.line_join_(lj);
end;

procedure conv_stroke.inner_join_(ij: unsigned);
begin
  vcgen_stroke_ptr(generator)^.inner_join_(ij);
end;

function conv_stroke._line_cap: unsigned;
begin
  Result := vcgen_stroke_ptr(generator)^._line_cap;
end;

function conv_stroke._line_join: unsigned;
begin
  Result := vcgen_stroke_ptr(generator)^._line_join;
end;

function conv_stroke._inner_join: unsigned;
begin
  Result := vcgen_stroke_ptr(generator)^._inner_join;
end;

procedure conv_stroke.width_(w: double);
begin
  vcgen_stroke_ptr(generator)^.width_(w);
end;

procedure conv_stroke.miter_limit_(ml: double);
begin
  vcgen_stroke_ptr(generator)^.miter_limit_(ml);
end;

procedure conv_stroke.miter_limit_theta_(t: double);
begin
  vcgen_stroke_ptr(generator)^.miter_limit_theta_(t);
end;

procedure conv_stroke.inner_miter_limit_(ml: double);
begin
  vcgen_stroke_ptr(generator)^.inner_miter_limit_(ml);
end;

procedure conv_stroke.approximation_scale_(_as_: double);
begin
  vcgen_stroke_ptr(generator)^.approximation_scale_(_as_);
end;

function conv_stroke._width: double;
begin
  Result := vcgen_stroke_ptr(generator)^._width;
end;

function conv_stroke._miter_limit: double;
begin
  Result := vcgen_stroke_ptr(generator)^._miter_limit;
end;

function conv_stroke._inner_miter_limit: double;
begin
  Result := vcgen_stroke_ptr(generator)^._inner_miter_limit;
end;

function conv_stroke._approximation_scale: double;
begin
  Result := vcgen_stroke_ptr(generator)^._approximation_scale;
end;

procedure conv_stroke.shorten_(s: double);
begin
  vcgen_stroke_ptr(generator)^.shorten_(s);
end;

function conv_stroke._shorten: double;
begin
  Result := vcgen_stroke_ptr(generator)^._shorten;
end;

constructor conv_stroke_math.Construct(vs: vertex_source_ptr);
begin
  the_generator.Construct;
  inherited Construct(vs, @the_generator);
end;

destructor conv_stroke_math.Destruct;
begin
  inherited Destruct;
  the_generator.Destruct;
end;

procedure conv_stroke_math.line_cap_(lc: unsigned);
begin
  vcgen_stroke_math_ptr(generator)^.line_cap_(lc);
end;

procedure conv_stroke_math.line_join_(lj: unsigned);
begin
  vcgen_stroke_math_ptr(generator)^.line_join_(lj);
end;

procedure conv_stroke_math.inner_join_(ij: unsigned);
begin
  vcgen_stroke_math_ptr(generator)^.inner_join_(ij);
end;

function conv_stroke_math._line_cap: unsigned;
begin
  Result := vcgen_stroke_math_ptr(generator)^._line_cap;
end;

function conv_stroke_math._line_join: unsigned;
begin
  Result := vcgen_stroke_math_ptr(generator)^._line_join;
end;

function conv_stroke_math._inner_join: unsigned;
begin
  Result := vcgen_stroke_math_ptr(generator)^._inner_join;
end;

procedure conv_stroke_math.width_(w: double);
begin
  vcgen_stroke_math_ptr(generator)^.width_(w);
end;

procedure conv_stroke_math.miter_limit_(ml: double);
begin
  vcgen_stroke_math_ptr(generator)^.miter_limit_(ml);
end;

procedure conv_stroke_math.miter_limit_theta_(t: double);
begin
  vcgen_stroke_math_ptr(generator)^.miter_limit_theta_(t);
end;

procedure conv_stroke_math.inner_miter_limit_(ml: double);
begin
  vcgen_stroke_math_ptr(generator)^.inner_miter_limit_(ml);
end;

procedure conv_stroke_math.approximation_scale_(_as_: double);
begin
  vcgen_stroke_math_ptr(generator)^.approximation_scale_(_as_);
end;

function conv_stroke_math._width: double;
begin
  Result := vcgen_stroke_math_ptr(generator)^._width;
end;

function conv_stroke_math._miter_limit: double;
begin
  Result := vcgen_stroke_math_ptr(generator)^._miter_limit;
end;

function conv_stroke_math._inner_miter_limit: double;
begin
  Result := vcgen_stroke_math_ptr(generator)^._inner_miter_limit;
end;

function conv_stroke_math._approximation_scale: double;
begin
  Result := vcgen_stroke_math_ptr(generator)^._approximation_scale;
end;

procedure conv_stroke_math.shorten_(s: double);
begin
  vcgen_stroke_math_ptr(generator)^.shorten_(s);
end;

function conv_stroke_math._shorten: double;
begin
  Result := vcgen_stroke_math_ptr(generator)^._shorten;

end;

end.
