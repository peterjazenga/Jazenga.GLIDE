
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}
unit
agg_span_gradient_alpha;

interface

{$I agg_mode.inc }
{$Q- }
{$R- }
uses
  agg_basics,
  agg_color,
  agg_array,
  agg_span_gradient,
  agg_span_interpolator_linear,
  agg_span_converter;

type
  gradient_alpha_ptr = ^gradient_alpha;

  gradient_alpha = object(array_base)
  end;

  span_gradient_alpha = object(span_convertor)
    downscale_shift: unsigned;

    m_interpolator: span_interpolator_ptr;
    m_gradient_function: gradient_ptr;
    m_alpha_function: gradient_alpha_ptr;

    m_d1, m_d2: int;

    constructor Construct; overload;
    constructor Construct(inter: span_interpolator_ptr; gradient_fnc: gradient_ptr;
      alpha_fnc: gradient_alpha_ptr; d1, d2: double); overload;

    function _interpolator: span_interpolator_ptr;
    function _gradient_function: gradient_ptr;
    function _alpha_function: gradient_alpha_ptr;
    function _d1: double;
    function _d2: double;

    procedure interpolator_(i: span_interpolator_ptr);
    procedure gradient_function_(gf: gradient_ptr);
    procedure alpha_function_(af: gradient_alpha_ptr);

    procedure d1_(v: double);
    procedure d2_(v: double);

    procedure convert(span: aggclr_ptr; x, y: int; len: unsigned); virtual;

  end;

  gradient_alpha_x = object
    function array_operator(x: aggclr): aggclr;

  end;

  gradient_alpha_x_u8 = object
    function array_operator(x: int): int8u;

  end;

  gradient_alpha_one_munus_x_u8 = object
    function array_operator(x: int): int8u;

  end;

implementation

constructor span_gradient_alpha.Construct;
begin
  m_interpolator := nil;
  m_gradient_function := nil;
  m_alpha_function := nil;

  downscale_shift := 0;

  m_d1 := 0;
  m_d2 := 0;

end;

{ CONSTRUCT }
constructor span_gradient_alpha.Construct(inter: span_interpolator_ptr; gradient_fnc: gradient_ptr;
  alpha_fnc: gradient_alpha_ptr; d1, d2: double);
begin
  m_interpolator := inter;
  m_gradient_function := gradient_fnc;
  m_alpha_function := alpha_fnc;

  downscale_shift := m_interpolator^.subpixel_shift - gradient_subpixel_shift;

  m_d1 := trunc(d1 * gradient_subpixel_size);
  m_d2 := trunc(d2 * gradient_subpixel_size);

end;

{ _INTERPOLATOR }
function span_gradient_alpha._interpolator: span_interpolator_ptr;
begin
  Result := m_interpolator;

end;

{ _GRADIENT_FUNCTION }
function span_gradient_alpha._gradient_function: gradient_ptr;
begin
  Result := m_gradient_function;

end;

{ _ALPHA_FUNCTION }
function span_gradient_alpha._alpha_function: gradient_alpha_ptr;
begin
  Result := m_alpha_function;

end;

{ _D1 }
function span_gradient_alpha._d1: double;
begin
  Result := m_d1 / gradient_subpixel_size;

end;

{ _D2 }
function span_gradient_alpha._d2: double;
begin
  Result := m_d2 / gradient_subpixel_size;

end;

{ INTERPOLATOR_ }
procedure span_gradient_alpha.interpolator_(i: span_interpolator_ptr);
begin
  m_interpolator := i;

end;

{ GRADIENT_FUNCTION_ }
procedure span_gradient_alpha.gradient_function_(gf: gradient_ptr);
begin
  m_gradient_function := gf;

end;

{ ALPHA_FUNCTION_ }
procedure span_gradient_alpha.alpha_function_(af: gradient_alpha_ptr);
begin
  m_alpha_function := af;

end;

{ D1_ }
procedure span_gradient_alpha.d1_(v: double);
begin
  m_d1 := trunc(v * gradient_subpixel_size);

end;

{ D2_ }
procedure span_gradient_alpha.d2_(v: double);
begin
  m_d2 := trunc(v * gradient_subpixel_size);

end;

{ CONVERT }
procedure span_gradient_alpha.convert(span: aggclr_ptr; x, y: int; len: unsigned);
var
  dd, d: int;

begin
  dd := m_d2 - m_d1;

  if dd < 1 then
    dd := 1;

  m_interpolator^.begin_(x + 0.5, y + 0.5, len);

  repeat
    m_interpolator^.coordinates(@x, @y);

    d :=
      m_gradient_function^.calculate(shr_int32(x, downscale_shift), shr_int32(y, downscale_shift), m_d2);

    d := ((d - m_d1) * m_alpha_function^.size) div dd;

    if d < 0 then
      d := 0;

    if d >= m_alpha_function^.size then
      d := m_alpha_function^.size - 1;

    span^.a := int8u_ptr(m_alpha_function^.array_operator(d))^;

    Inc(ptrcomp(span), sizeof(aggclr));

    m_interpolator^.inc_operator;

    Dec(len);

  until len = 0;

end;

{ ARRAY_OPERATOR }
function gradient_alpha_x.array_operator(x: aggclr): aggclr;
begin
  Result := x;

end;

{ ARRAY_OPERATOR }
function gradient_alpha_x_u8.array_operator(x: int): int8u;
begin
  Result := int8u(x);

end;

{ ARRAY_OPERATOR }
function gradient_alpha_one_munus_x_u8.array_operator(x: int): int8u;
begin
  Result := int8u(255 - x);

end;

end.

