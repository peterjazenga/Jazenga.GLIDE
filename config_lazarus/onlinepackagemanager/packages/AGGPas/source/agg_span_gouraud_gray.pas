
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}
unit
agg_span_gouraud_gray;

interface

{$I agg_mode.inc }

uses
  agg_basics,
  agg_color,
  agg_dda_line,
  agg_span_gouraud,
  agg_span_allocator,
  agg_math;

const
  subpixel_shift = 4;
  subpixel_size = 1 shl subpixel_shift;

type
  gray_calc_ptr = ^gray_calc;

  gray_calc = object
    m_x1, m_y1, m_dx, m_1dy: double;

    m_v1, m_a1, m_dv, m_da, m_v, m_a, m_x: int;

    function round(v: double): int;
    procedure init(c1, c2: coord_type_ptr);
    procedure calc(y: double);

  end;

  span_gouraud_gray = object(span_gouraud)
    m_swap: boolean;
    m_y2: int;

    m_c1, m_c2, m_c3: gray_calc;

    constructor Construct(alloc: span_allocator_ptr); overload;
    constructor Construct(alloc: span_allocator_ptr; c1, c2, c3: aggclr_ptr;
      x1, y1, x2, y2, x3, y3, d: double); overload;

    procedure prepare(max_span_len: unsigned); virtual;
    function generate(x, y: int; len: unsigned): aggclr_ptr; virtual;

  end;

implementation

function gray_calc.round(v: double): int;
begin
  if v < 0.0 then
    Result := trunc(v - 0.5)
  else
    Result := trunc(v + 0.5);

end;

{ INIT }
procedure gray_calc.init(c1, c2: coord_type_ptr);
var
  dy: double;

begin
  m_x1 := c1^.x - 0.5;
  m_y1 := c1^.y - 0.5;
  m_dx := c2^.x - c1^.x;

  dy := c2^.y - c1^.y;

  if Abs(dy) < 1e-10 then
    m_1dy := 1e10
  else
    m_1dy := 1.0 / dy;

  m_v1 := c1^.color.v;
  m_a1 := c1^.color.a;
  m_dv := c2^.color.v - m_v1;
  m_da := c2^.color.a - m_a1;

end;

{ CALC }
procedure gray_calc.calc(y: double);
var
  k: double;

begin
  k := (y - m_y1) * m_1dy;

  if k < 0.0 then
    k := 0.0;

  if k > 1.0 then
    k := 1.0;

  m_v := m_v1 + self.round(m_dv * k);
  m_a := m_a1 + self.round(m_da * k);
  m_x := self.round((m_x1 + m_dx * k) * subpixel_size);

end;

{ CONSTRUCT }
constructor span_gouraud_gray.Construct(alloc: span_allocator_ptr);
begin
  inherited Construct(alloc);

end;

{ CONSTRUCT }
constructor span_gouraud_gray.Construct(alloc: span_allocator_ptr;
  c1, c2, c3: aggclr_ptr; x1, y1, x2, y2, x3, y3, d: double);
begin
  inherited Construct(alloc, c1, c2, c3, x1, y1, x2, y2, x3, y3, d);

end;

{ PREPARE }
procedure span_gouraud_gray.prepare(max_span_len: unsigned);
var
  coord: array[0..2] of coord_type;

begin
  inherited prepare(max_span_len);

  arrange_vertices(@coord);

  m_y2 := trunc(coord[1].y);

  m_swap :=
    calc_point_location(coord[0].x, coord[0].y, coord[2].x, coord[2].y, coord[1].x, coord[1].y) < 0.0;

  m_c1.init(@coord[0], @coord[2]);
  m_c2.init(@coord[0], @coord[1]);
  m_c3.init(@coord[1], @coord[2]);

end;

{ GENERATE }
function span_gouraud_gray.generate(x, y: int; len: unsigned): aggclr_ptr;
const
  lim = agg_color.base_mask;

var
  pc1, pc2, t: gray_calc_ptr;

  nlen, start, vv, va: int;

  v, a: dda_line_interpolator;
  span: aggclr_ptr;

begin
  m_c1.calc(y);

  pc1 := @m_c1;
  pc2 := @m_c2;

  if y < m_y2 then
    // Bottom part of the triangle (first subtriangle)
    m_c2.calc(y + m_c2.m_1dy)
  else
  begin
    // Upper part (second subtriangle)
    m_c3.calc(y - m_c3.m_1dy);

    pc2 := @m_c3;

  end;

  // It means that the triangle is oriented clockwise,
  // so that we need to swap the controlling structures
  if m_swap then
  begin
    t := pc2;
    pc2 := pc1;
    pc1 := t;

  end;

  // Get the horizontal length with subpixel accuracy
  // and protect it from division by zero
  nlen := Abs(pc2^.m_x - pc1^.m_x);

  if nlen <= 0 then
    nlen := 1;

  v.Construct(pc1^.m_v, pc2^.m_v, nlen, 14);
  a.Construct(pc1^.m_a, pc2^.m_a, nlen, 14);

  // Calculate the starting point of the gradient with subpixel
  // accuracy and correct (roll back) the interpolators.
  // This operation will also clip the beginning of the span
  // if necessary.
  start := pc1^.m_x - (x shl subpixel_shift);

  v.dec_operator(start);
  a.dec_operator(start);

  Inc(nlen, start);

  span := _allocator^.span;

  // Beginning part of the span. Since we rolled back the
  // interpolators, the color values may have overflow.
  // So that, we render the beginning part with checking
  // for overflow. It lasts until "start" is positive;
  // typically it's 1-2 pixels, but may be more in some cases.
  while (len <> 0) and (start > 0) do
  begin
    vv := v._y;
    va := a._y;

    if vv < 0 then
      vv := 0;

    if vv > lim then
      vv := lim;

    if va < 0 then
      va := 0;

    if va > lim then
      va := lim;

    span^.v := int8u(vv);
    span^.a := int8u(va);

    v.inc_operator(subpixel_size);
    a.inc_operator(subpixel_size);

    Dec(nlen, subpixel_size);
    Dec(start, subpixel_size);
    Inc(ptrcomp(span), sizeof(aggclr));
    Dec(len);

  end;

  // Middle part, no checking for overflow.
  // Actual spans can be longer than the calculated length
  // because of anti-aliasing, thus, the interpolators can
  // overflow. But while "nlen" is positive we are safe.
  while (len <> 0) and (nlen > 0) do
  begin
    span^.v := int8u(v._y);
    span^.a := int8u(a._y);

    v.inc_operator(subpixel_size);
    a.inc_operator(subpixel_size);

    Dec(nlen, subpixel_size);
    Inc(ptrcomp(span), sizeof(aggclr));
    Dec(len);

  end;

  // Ending part; checking for overflow.
  // Typically it's 1-2 pixels, but may be more in some cases.
  while len <> 0 do
  begin
    vv := v._y;
    va := a._y;

    if vv < 0 then
      vv := 0;

    if vv > lim then
      vv := lim;

    if va < 0 then
      va := 0;

    if va > lim then
      va := lim;

    span^.v := int8u(vv);
    span^.a := int8u(va);

    v.inc_operator(subpixel_size);
    a.inc_operator(subpixel_size);

    Inc(ptrcomp(span), sizeof(aggclr));
    Dec(len);

  end;

  Result := _allocator^.span;

end;

end.

