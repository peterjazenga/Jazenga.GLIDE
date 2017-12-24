
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}
unit
agg_span_pattern;

interface

{$I agg_mode.inc }
{$Q- }
{$R- }

uses
  agg_basics,
  agg_color,
  agg_rendering_buffer,
  agg_span_generator,
  agg_span_allocator;

const
  base_mask = agg_color.base_mask;

type
  span_pattern_base = object(span_generator)
    m_src: rendering_buffer_ptr;
    m_offset_x, m_offset_y: unsigned;
    m_alpha: int8u;

    constructor Construct(alloc: span_allocator_ptr); overload;
    constructor Construct(alloc: span_allocator_ptr; src: rendering_buffer_ptr;
      offset_x, offset_y: unsigned; alpha: double); overload;

    function _source_image: rendering_buffer_ptr;
    function _offset_x: unsigned;
    function _offset_y: unsigned;
    function _alpha: double;
    function _alpha_int: int8u;

    procedure source_image_(v: rendering_buffer_ptr);

    procedure offset_x_(v: unsigned);
    procedure offset_y_(v: unsigned);
    procedure alpha_(v: double);

  end;

  wrap_mode_ptr = ^wrap_mode;

  wrap_mode = object
    constructor Construct;

    procedure init(size: unsigned); virtual; abstract;

    function func_operator(v: int): unsigned; virtual; abstract;
    function inc_operator: unsigned; virtual; abstract;

  end;

  wrap_mode_repeat = object(wrap_mode)
    m_size, m_add, m_value: unsigned;

    procedure init(size: unsigned); virtual;

    function func_operator(v: int): unsigned; virtual;
    function inc_operator: unsigned; virtual;

  end;

  wrap_mode_repeat_pow2 = object(wrap_mode)
    m_mask, m_value: unsigned;

    procedure init(size: unsigned); virtual;

    function func_operator(v: int): unsigned; virtual;
    function inc_operator: unsigned; virtual;

  end;

  wrap_mode_repeat_auto_pow2 = object(wrap_mode)
    m_size, m_add, m_mask, m_value: unsigned;

    procedure init(size: unsigned); virtual;

    function func_operator(v: int): unsigned; virtual;
    function inc_operator: unsigned; virtual;

  end;

  wrap_mode_reflect = object(wrap_mode)
    m_size, m_size2, m_add, m_value: unsigned;

    procedure init(size: unsigned); virtual;

    function func_operator(v: int): unsigned; virtual;
    function inc_operator: unsigned; virtual;

  end;

  wrap_mode_reflect_pow2 = object(wrap_mode)
    m_size, m_mask, m_value: unsigned;

    procedure init(size: unsigned); virtual;

    function func_operator(v: int): unsigned; virtual;
    function inc_operator: unsigned; virtual;

  end;

  wrap_mode_reflect_auto_pow2 = object(wrap_mode)
    m_size, m_size2, m_add, m_mask, m_value: unsigned;

    procedure init(size: unsigned); virtual;

    function func_operator(v: int): unsigned; virtual;
    function inc_operator: unsigned; virtual;

  end;

implementation

constructor span_pattern_base.Construct(alloc: span_allocator_ptr);
begin
  inherited Construct(alloc);

  m_src := nil;
  m_offset_x := 0;
  m_offset_y := 0;
  m_alpha := 0;

end;

{ CONSTRUCT }
constructor span_pattern_base.Construct(alloc: span_allocator_ptr; src: rendering_buffer_ptr;
  offset_x, offset_y: unsigned; alpha: double);
begin
  inherited Construct(alloc);

  m_src := src;
  m_offset_x := offset_x;
  m_offset_y := offset_y;
  m_alpha := int8u(trunc(alpha * base_mask));

end;

{ _SOURCE_IMAGE }
function span_pattern_base._source_image: rendering_buffer_ptr;
begin
  Result := m_src;

end;

{ _OFFSET_X }
function span_pattern_base._offset_x: unsigned;
begin
  Result := m_offset_x;

end;

{ _OFFSET_Y }
function span_pattern_base._offset_y: unsigned;
begin
  Result := m_offset_y;

end;

{ _ALPHA }
function span_pattern_base._alpha: double;
begin
  Result := m_alpha / base_mask;

end;

{ _ALPHA_INT }
function span_pattern_base._alpha_int: int8u;
begin
  Result := m_alpha;

end;

{ SOURCE_IMAGE_ }
procedure span_pattern_base.source_image_(v: rendering_buffer_ptr);
begin
  m_src := v;

end;

{ OFFSET_X_ }
procedure span_pattern_base.offset_x_(v: unsigned);
begin
  m_offset_x := v;

end;

{ OFFSET_Y_ }
procedure span_pattern_base.offset_y_(v: unsigned);
begin
  m_offset_y := v;

end;

{ ALPHA_ }
procedure span_pattern_base.alpha_(v: double);
begin
  m_alpha := int8u(trunc(v * base_mask));

end;

{ CONSTRUCT }
constructor wrap_mode.Construct;
begin
end;

{ INIT }
procedure wrap_mode_repeat.init(size: unsigned);
begin
  m_size := size;
  m_add := size * ($3FFFFFFF div size);
  m_value := 0;

end;

{ FUNC_OPERATOR }
function wrap_mode_repeat.func_operator(v: int): unsigned;
begin
  m_value := (unsigned(v) + m_add) mod m_size;
  Result := m_value;

end;

{ INC_OPERATOR }
function wrap_mode_repeat.inc_operator: unsigned;
begin
  Inc(m_value);

  if m_value >= m_size then
    m_value := 0;

  Result := m_value;

end;

{ INIT }
procedure wrap_mode_repeat_pow2.init(size: unsigned);
begin
  m_mask := 1;

  while m_mask < size do
    m_mask := (m_mask shl 1) or 1;

  m_mask := m_mask shr 1;

end;

{ FUNC_OPERATOR }
function wrap_mode_repeat_pow2.func_operator(v: int): unsigned;
begin
  m_value := unsigned(v) and m_mask;
  Result := m_value;

end;

{ INC_OPERATOR }
function wrap_mode_repeat_pow2.inc_operator: unsigned;
begin
  Inc(m_value);

  if m_value > m_mask then
    m_value := 0;

  Result := m_value;

end;

{ INIT }
procedure wrap_mode_repeat_auto_pow2.init(size: unsigned);
begin
  m_size := size;
  m_add := size * ($3FFFFFFF div size);

  if m_size and (m_size - 1) <> 0 then
    m_mask := 0
  else
    m_mask := m_size - 1;

  m_value := 0;

end;

{ FUNC_OPERATOR }
function wrap_mode_repeat_auto_pow2.func_operator(v: int): unsigned;
begin
  if m_mask <> 0 then
  begin
    m_value := unsigned(unsigned(v) and m_mask);
    Result := m_value;

  end
  else
  begin
    m_value := unsigned((unsigned(v) + m_add) mod m_size);
    Result := m_value;

  end;

end;

{ INC_OPERATOR }
function wrap_mode_repeat_auto_pow2.inc_operator: unsigned;
begin
  Inc(m_value);

  if m_value >= m_size then
    m_value := 0;

  Result := m_value;

end;

{ INIT }
procedure wrap_mode_reflect.init(size: unsigned);
begin
  m_size := size;
  m_size2 := size * 2;
  m_add := m_size2 * ($3FFFFFFF div m_size2);
  m_value := 0;

end;

{ FUNC_OPERATOR }
function wrap_mode_reflect.func_operator(v: int): unsigned;
begin
  m_value := (unsigned(v) + m_add) mod m_size2;

  if m_value >= m_size then
    Result := m_size2 - m_value - 1
  else
    Result := m_value;

end;

{ INC_OPERATOR }
function wrap_mode_reflect.inc_operator: unsigned;
begin
  Inc(m_value);

  if m_value >= m_size2 then
    m_value := 0;

  if m_value >= m_size then
    Result := m_size2 - m_value - 1
  else
    Result := m_value;

end;

{ INIT }
procedure wrap_mode_reflect_pow2.init(size: unsigned);
begin
  m_mask := 1;
  m_size := 1;

  while m_mask < size do
  begin
    m_mask := (m_mask shl 1) or 1;
    m_size := m_size shl 1;

  end;

end;

{ FUNC_OPERATOR }
function wrap_mode_reflect_pow2.func_operator(v: int): unsigned;
begin
  m_value := unsigned(v) and m_mask;

  if m_value >= m_size then
    Result := m_mask - m_value
  else
    Result := m_value;

end;

{ INC_OPERATOR }
function wrap_mode_reflect_pow2.inc_operator: unsigned;
begin
  Inc(m_value);

  m_value := m_value and m_mask;

  if m_value >= m_size then
    Result := m_mask - m_value
  else
    Result := m_value;

end;

{ INIT }
procedure wrap_mode_reflect_auto_pow2.init(size: unsigned);
begin
  m_size := size;
  m_size2 := size * 2;
  m_add := m_size2 * ($3FFFFFFF div m_size2);

  if m_size2 and (m_size2 - 1) <> 0 then
    m_mask := 0
  else
    m_mask := m_size2 - 1;

  m_value := 0;

end;

{ FUNC_OPERATOR }
function wrap_mode_reflect_auto_pow2.func_operator(v: int): unsigned;
begin
  if m_mask <> 0 then
    m_value := unsigned(v) and m_mask
  else
    m_value := (unsigned(v) + m_add) mod m_size2;

  if m_value >= m_size then
    Result := m_size2 - m_value - 1
  else
    Result := m_value;

end;

{ INC_OPERATOR }
function wrap_mode_reflect_auto_pow2.inc_operator: unsigned;
begin
  Inc(m_value);

  if m_value >= m_size2 then
    m_value := 0;

  if m_value >= m_size then
    Result := m_size2 - m_value - 1
  else
    Result := m_value;

end;

end.

