
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}
unit
agg_conv_smooth_poly1;

interface

{$I agg_mode.inc }

uses
  agg_basics,
  agg_vcgen_smooth_poly1,
  agg_conv_adaptor_vcgen,
  agg_conv_curve,
  agg_vertex_source;

type

  conv_smooth_poly1 = object(conv_adaptor_vcgen)
    the_generator: vcgen_smooth_poly1;
    constructor Construct(vs: vertex_source_ptr);
    destructor Destruct; virtual;
    procedure smooth_value_(v: double);
    function _smooth_value: double;
  end;

  conv_smooth_poly1_curve = object(conv_curve)
    m_smooth: conv_smooth_poly1;
    constructor Construct(vs: vertex_source_ptr);
    destructor Destruct; virtual;
    procedure smooth_value_(v: double);
    function _smooth_value: double;
  end;

implementation

constructor conv_smooth_poly1.Construct(vs: vertex_source_ptr);
begin
  the_generator.Construct;
  inherited Construct(vs, @the_generator);
end;

{ DESTRUCT }
destructor conv_smooth_poly1.Destruct;
begin
  inherited Destruct;
  the_generator.Destruct;
end;

{ SMOOTH_VALUE_ }
procedure conv_smooth_poly1.smooth_value_(v: double);
begin
  vcgen_smooth_poly1_ptr(generator)^.smooth_value_(v);
end;

{ _SMOOTH_VALUE }
function conv_smooth_poly1._smooth_value: double;
begin
  Result := vcgen_smooth_poly1_ptr(generator)^._smooth_value;
end;

{ CONSTRUCT }
constructor conv_smooth_poly1_curve.Construct(vs: vertex_source_ptr);
begin
  m_smooth.Construct(vs);
  inherited Construct(@m_smooth);
end;

{ DESTRUCT }
destructor conv_smooth_poly1_curve.Destruct;
begin
  inherited Destruct;
  m_smooth.Destruct;
end;

{ SMOOTH_VALUE_ }
procedure conv_smooth_poly1_curve.smooth_value_(v: double);
begin
  vcgen_smooth_poly1_ptr(m_smooth.generator)^.smooth_value_(v);
end;

{ _SMOOTH_VALUE }
function conv_smooth_poly1_curve._smooth_value: double;
begin
  Result := vcgen_smooth_poly1_ptr(m_smooth.generator)^._smooth_value;
end;

end.

