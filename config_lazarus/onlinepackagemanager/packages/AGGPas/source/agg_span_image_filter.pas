
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}
unit
agg_span_image_filter;

interface

{$I agg_mode.inc }

uses
  agg_basics,
  agg_color,
  agg_image_filters,
  agg_rendering_buffer,
  agg_span_generator,
  agg_span_allocator,
  agg_span_interpolator_linear;

type
  span_image_filter_ptr = ^span_image_filter;

  span_image_filter = object(span_generator)
    m_src: rendering_buffer_ptr;
    m_back_color: aggclr;
    m_interpolator: span_interpolator_ptr;
    m_filter: image_filter_lut_ptr;

    m_dx_dbl, m_dy_dbl: double;
    m_dx_int, m_dy_int: unsigned;

    constructor Construct(alloc: span_allocator_ptr); overload;
    constructor Construct(alloc: span_allocator_ptr; src: rendering_buffer_ptr;
      back_color: aggclr_ptr; interpolator: span_interpolator_ptr;
      filter: image_filter_lut_ptr); overload;

    function _source_image: rendering_buffer_ptr;
    function _background_color: aggclr_ptr;
    function _filter: image_filter_lut_ptr;
    function _interpolator: span_interpolator_ptr;

    function filter_dx_int: int;
    function filter_dy_int: int;
    function filter_dx_dbl: double;
    function filter_dy_dbl: double;

    procedure source_image_(v: rendering_buffer_ptr);
    procedure background_color_(v: aggclr_ptr);
    procedure interpolator_(v: span_interpolator_ptr);
    procedure filter_(v: image_filter_lut_ptr);
    procedure filter_offset(dx, dy: double); overload;
    procedure filter_offset(d: double); overload;

  end;

implementation

constructor span_image_filter.Construct(alloc: span_allocator_ptr);
begin
  inherited Construct(alloc);

  m_back_color.Construct;

  m_src := nil;
  m_interpolator := nil;
  m_filter := nil;

  m_dx_dbl := 0.0;
  m_dy_dbl := 0.0;
  m_dx_int := 0;
  m_dy_int := 0;

end;

{ CONSTRUCT }
constructor span_image_filter.Construct(alloc: span_allocator_ptr; src: rendering_buffer_ptr;
  back_color: aggclr_ptr; interpolator: span_interpolator_ptr; filter: image_filter_lut_ptr);
begin
  inherited Construct(alloc);

  m_src := src;
  m_back_color := back_color^;
  m_interpolator := interpolator;
  m_filter := filter;

  m_dx_dbl := 0.5;
  m_dy_dbl := 0.5;
  m_dx_int := image_subpixel_size div 2;
  m_dy_int := image_subpixel_size div 2;

end;

{ _SOURCE_IMAGE }
function span_image_filter._source_image: rendering_buffer_ptr;
begin
  Result := m_src;

end;

{ _BACKGROUND_COLOR }
function span_image_filter._background_color: aggclr_ptr;
begin
  Result := @m_back_color;

end;

{ _FILTER }
function span_image_filter._filter: image_filter_lut_ptr;
begin
  Result := m_filter;

end;

{ _INTERPOLATOR }
function span_image_filter._interpolator: span_interpolator_ptr;
begin
  Result := m_interpolator;

end;

{ FILTER_DX_INT }
function span_image_filter.filter_dx_int: int;
begin
  Result := m_dx_int;

end;

{ FILTER_DY_INT }
function span_image_filter.filter_dy_int: int;
begin
  Result := m_dy_int;

end;

{ FILTER_DX_DBL }
function span_image_filter.filter_dx_dbl: double;
begin
  Result := m_dx_dbl;

end;

{ FILTER_DY_DBL }
function span_image_filter.filter_dy_dbl: double;
begin
  Result := m_dy_dbl;

end;

{ SOURCE_IMAGE_ }
procedure span_image_filter.source_image_(v: rendering_buffer_ptr);
begin
  m_src := v;

end;

{ BACKGROUND_COLOR_ }
procedure span_image_filter.background_color_(v: aggclr_ptr);
begin
  m_back_color := v^;

end;

{ INTERPOLATOR_ }
procedure span_image_filter.interpolator_(v: span_interpolator_ptr);
begin
  m_interpolator := v;

end;

{ FILTER_ }
procedure span_image_filter.filter_(v: image_filter_lut_ptr);
begin
  m_filter := v;

end;

{ FILTER_OFFSET }
procedure span_image_filter.filter_offset(dx, dy: double);
begin
  m_dx_dbl := dx;
  m_dy_dbl := dy;
  m_dx_int := trunc(dx * image_subpixel_size);
  m_dy_int := trunc(dy * image_subpixel_size);

end;

{ FILTER_OFFSET }
procedure span_image_filter.filter_offset(d: double);
begin
  filter_offset(d, d);

end;

end.

