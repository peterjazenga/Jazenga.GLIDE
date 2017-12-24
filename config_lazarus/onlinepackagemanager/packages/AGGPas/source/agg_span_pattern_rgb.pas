
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}
unit
agg_span_pattern_rgb;

interface

{$I agg_mode.inc }
{$Q- }
{$R- }
uses
  agg_basics,
  agg_color,
  agg_pixfmt,
  agg_pixfmt_rgb,
  agg_span_pattern,
  agg_span_allocator,
  agg_rendering_buffer;

const
  base_shift = agg_color.base_shift;
  base_mask = agg_color.base_mask;

type
  span_pattern_rgb_ptr = ^span_pattern_rgb;

  span_pattern_rgb = object(span_pattern_base)
    m_wrap_mode_x, m_wrap_mode_y: wrap_mode_ptr;

    m_order: order_type;

    constructor Construct(alloc: span_allocator_ptr; WX, WY: wrap_mode_ptr; order: order_type); overload;
    constructor Construct(alloc: span_allocator_ptr; src: rendering_buffer_ptr;
      offset_x, offset_y: unsigned; WX, WY: wrap_mode_ptr; order: order_type;
      alpha: int8u = base_mask); overload;

    procedure source_image_(src: rendering_buffer_ptr);

    function generate(x, y: int; len: unsigned): aggclr_ptr; virtual;

  end;

implementation

constructor span_pattern_rgb.Construct(alloc: span_allocator_ptr; WX, WY: wrap_mode_ptr; order: order_type);
begin
  inherited Construct(alloc);

  m_order := order;

  m_wrap_mode_x := WX;
  m_wrap_mode_y := WY;

  m_wrap_mode_x^.init(1);
  m_wrap_mode_y^.init(1);

end;

{ CONSTRUCT }
constructor span_pattern_rgb.Construct(alloc: span_allocator_ptr; src: rendering_buffer_ptr;
  offset_x, offset_y: unsigned; WX, WY: wrap_mode_ptr; order: order_type;
  alpha: int8u = base_mask);
begin
  inherited Construct(alloc, src, offset_x, offset_y, alpha);

  m_order := order;

  m_wrap_mode_x := WX;
  m_wrap_mode_y := WY;

  m_wrap_mode_x^.init(src^._width);
  m_wrap_mode_y^.init(src^._height);

end;

{ SOURCE_IMAGE_ }
procedure span_pattern_rgb.source_image_(src: rendering_buffer_ptr);
begin
  inherited source_image_(src);

  m_wrap_mode_x^.init(src^._width);
  m_wrap_mode_y^.init(src^._height);

end;

{ GENERATE }
function span_pattern_rgb.generate(x, y: int; len: unsigned): aggclr_ptr;
var
  span: aggclr_ptr;

  sx: unsigned;

  row_ptr, p: int8u_ptr;

begin
  span := _allocator^.span;
  sx := m_wrap_mode_x^.func_operator(_offset_x + x);

  row_ptr := _source_image^.row(m_wrap_mode_y^.func_operator(_offset_y + y));

  repeat
    p := int8u_ptr(ptrcomp(row_ptr) + (sx + sx + sx) * sizeof(int8u));

    span^.r := int8u_ptr(ptrcomp(p) + m_order.R * sizeof(int8u))^;
    span^.g := int8u_ptr(ptrcomp(p) + m_order.G * sizeof(int8u))^;
    span^.b := int8u_ptr(ptrcomp(p) + m_order.B * sizeof(int8u))^;
    span^.a := _alpha_int;

    sx := m_wrap_mode_x^.inc_operator;

    Inc(ptrcomp(span), sizeof(aggclr));
    Dec(len);

  until len = 0;

  Result := _allocator^.span;

end;

end.









