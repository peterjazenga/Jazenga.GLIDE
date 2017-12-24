
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}
unit
agg_span_image_resample_rgba;

interface

{$I agg_mode.inc }

uses
  agg_basics,
  agg_color,
  agg_span_image_resample,
  agg_span_interpolator_linear,
  agg_rendering_buffer,
  agg_span_allocator,
  agg_image_filters;

const
  base_shift = agg_color.base_shift;
  base_mask = agg_color.base_mask;
  downscale_shift = image_filter_shift;

type
  span_image_resample_rgba_affine_ptr = ^span_image_resample_rgba_affine;

  span_image_resample_rgba_affine = object(span_image_resample_affine)
    m_order: order_type;

    constructor Construct(alloc: span_allocator_ptr; order: order_type); overload;
    constructor Construct(alloc: span_allocator_ptr; src: rendering_buffer_ptr;
      back_color: aggclr_ptr; interpolator: span_interpolator_ptr;
      filter: image_filter_lut_ptr; order: order_type); overload;

    function generate(x, y: int; len: unsigned): aggclr_ptr; virtual;

  end;

  span_image_resample_rgba_ptr = ^span_image_resample_rgba;

  span_image_resample_rgba = object(span_image_resample)
    m_order: order_type;

    constructor Construct(alloc: span_allocator_ptr; order: order_type); overload;
    constructor Construct(alloc: span_allocator_ptr; src: rendering_buffer_ptr;
      back_color: aggclr_ptr; interpolator: span_interpolator_ptr;
      filter: image_filter_lut_ptr; order: order_type); overload;

    function generate(x, y: int; len: unsigned): aggclr_ptr; virtual;

  end;

implementation

constructor span_image_resample_rgba_affine.Construct(alloc: span_allocator_ptr; order: order_type);
begin
  inherited Construct(alloc);

  m_order := order;

end;

{ CONSTRUCT }
constructor span_image_resample_rgba_affine.Construct(alloc: span_allocator_ptr;
  src: rendering_buffer_ptr; back_color: aggclr_ptr; interpolator: span_interpolator_ptr;
  filter: image_filter_lut_ptr; order: order_type);
begin
  inherited Construct(alloc, src, back_color, interpolator, filter);

  m_order := order;

end;

{ GENERATE }
function span_image_resample_rgba_affine.generate(x, y: int; len: unsigned): aggclr_ptr;
var
  fg: array[0..3] of int;

  back_r, back_g, back_b, back_a: int8u;

  span: aggclr_ptr;

  diameter, filter_size, radius_x, radius_y, maxx, maxy, y_lr, y_hr, total_weight, x_lr_ini, x_hr_ini, weight_y, x_lr, x_hr, weight: int;

  fg_ptr: int8u_ptr;

  weight_array: int16_ptr;

begin
  _interpolator^.begin_(x + filter_dx_dbl, y + filter_dy_dbl, len);

  back_r := _background_color^.r;
  back_g := _background_color^.g;
  back_b := _background_color^.b;
  back_a := _background_color^.a;

  span := _allocator^.span;

  diameter := _filter^.diameter;
  filter_size := diameter shl image_subpixel_shift;

  radius_x := shr_int32(diameter * m_rx, 1);
  radius_y := shr_int32(diameter * m_ry, 1);

  maxx := _source_image^._width - 1;
  maxy := _source_image^._height - 1;

  weight_array := _filter^.weight_array;

  repeat
    _interpolator^.coordinates(@x, @y);

    Inc(x, filter_dx_int - radius_x);
    Inc(y, filter_dy_int - radius_y);

    fg[0] := image_filter_size div 2;
    fg[1] := fg[0];
    fg[2] := fg[0];
    fg[3] := fg[0];

    y_lr := shr_int32(y, image_subpixel_shift);
    y_hr :=
      shr_int32((image_subpixel_mask - (y and image_subpixel_mask)) * m_ry_inv, image_subpixel_shift);

    total_weight := 0;

    x_lr_ini := shr_int32(x, image_subpixel_shift);
    x_hr_ini :=
      shr_int32((image_subpixel_mask - (x and image_subpixel_mask)) * m_rx_inv, image_subpixel_shift);

    repeat
      weight_y := int16_ptr(ptrcomp(weight_array) + y_hr * sizeof(int16))^;

      x_lr := x_lr_ini;
      x_hr := x_hr_ini;

      if (y_lr >= 0) and (y_lr <= maxy) then
      begin
        fg_ptr := int8u_ptr(ptrcomp(_source_image^.row(y_lr)) + (x_lr shl 2) * sizeof(int8u));

        repeat
          weight :=
            shr_int32(weight_y * int16_ptr(ptrcomp(weight_array) + x_hr * sizeof(int16))^ +
            image_filter_size div 2, downscale_shift);

          if (x_lr >= 0) and (x_lr <= maxx) then
          begin
            Inc(fg[0], fg_ptr^ * weight);
            Inc(ptrcomp(fg_ptr), sizeof(int8u));
            Inc(fg[1], fg_ptr^ * weight);
            Inc(ptrcomp(fg_ptr), sizeof(int8u));
            Inc(fg[2], fg_ptr^ * weight);
            Inc(ptrcomp(fg_ptr), sizeof(int8u));
            Inc(fg[3], fg_ptr^ * weight);
            Inc(ptrcomp(fg_ptr), sizeof(int8u));

          end
          else
          begin
            Inc(fg[m_order.R], back_r * weight);
            Inc(fg[m_order.G], back_g * weight);
            Inc(fg[m_order.B], back_b * weight);
            Inc(fg[m_order.A], back_a * weight);

            Inc(ptrcomp(fg_ptr), 4 * sizeof(int8u));

          end;

          Inc(total_weight, weight);
          Inc(x_hr, m_rx_inv);
          Inc(x_lr);

        until x_hr >= filter_size;

      end
      else
        repeat
          weight :=
            shr_int32(weight_y * int16_ptr(ptrcomp(weight_array) + x_hr * sizeof(int16))^ +
            image_filter_size div 2, downscale_shift);

          Inc(total_weight, weight);
          Inc(fg[m_order.R], back_r * weight);
          Inc(fg[m_order.G], back_g * weight);
          Inc(fg[m_order.B], back_b * weight);
          Inc(fg[m_order.A], back_a * weight);
          Inc(x_hr, m_rx_inv);

        until x_hr >= filter_size;

      Inc(y_hr, m_ry_inv);
      Inc(y_lr);

    until y_hr >= filter_size;

    fg[0] := fg[0] div total_weight;
    fg[1] := fg[1] div total_weight;
    fg[2] := fg[2] div total_weight;
    fg[3] := fg[3] div total_weight;

    if fg[0] < 0 then
      fg[0] := 0;

    if fg[1] < 0 then
      fg[1] := 0;

    if fg[2] < 0 then
      fg[2] := 0;

    if fg[3] < 0 then
      fg[3] := 0;

    if fg[m_order.A] > base_mask then
      fg[m_order.A] := base_mask;

    if fg[m_order.R] > fg[m_order.A] then
      fg[m_order.R] := fg[m_order.A];

    if fg[m_order.G] > fg[m_order.A] then
      fg[m_order.G] := fg[m_order.A];

    if fg[m_order.B] > fg[m_order.A] then
      fg[m_order.B] := fg[m_order.A];

    span^.r := int8u(fg[m_order.R]);
    span^.g := int8u(fg[m_order.G]);
    span^.b := int8u(fg[m_order.B]);
    span^.a := int8u(fg[m_order.A]);

    Inc(ptrcomp(span), sizeof(aggclr));

    _interpolator^.inc_operator;

    Dec(len);

  until len = 0;

  Result := _allocator^.span;

end;

{ CONSTRUCT }
constructor span_image_resample_rgba.Construct(alloc: span_allocator_ptr; order: order_type);
begin
  inherited Construct(alloc);

  m_order := order;

end;

{ CONSTRUCT }
constructor span_image_resample_rgba.Construct(alloc: span_allocator_ptr;
  src: rendering_buffer_ptr; back_color: aggclr_ptr; interpolator: span_interpolator_ptr;
  filter: image_filter_lut_ptr; order: order_type);
begin
  inherited Construct(alloc, src, back_color, interpolator, filter);

  m_order := order;

end;

{ GENERATE }
function span_image_resample_rgba.generate(x, y: int; len: unsigned): aggclr_ptr;
var
  span: aggclr_ptr;

  fg: array[0..3] of int;

  back_r, back_g, back_b, back_a: int8u;

  diameter, filter_size, rx, ry, rx_inv, ry_inv, radius_x, radius_y, maxx, maxy, y_lr, y_hr, total_weight, x_lr_ini, x_hr_ini, weight_y,
  x_lr, x_hr, weight: int;

  fg_ptr: int8u_ptr;

  weight_array: int16_ptr;

begin
  span := _allocator^.span;

  _interpolator^.begin_(x + filter_dx_dbl, y + filter_dy_dbl, len);

  back_r := _background_color^.r;
  back_g := _background_color^.g;
  back_b := _background_color^.b;
  back_a := _background_color^.a;

  diameter := _filter^.diameter;
  filter_size := diameter shl image_subpixel_shift;

  weight_array := _filter^.weight_array;

  repeat
    rx_inv := image_subpixel_size;
    ry_inv := image_subpixel_size;

    _interpolator^.coordinates(@x, @y);
    _interpolator^.local_scale(@rx, @ry);

    rx := shr_int32(rx * m_blur_x, image_subpixel_shift);
    ry := shr_int32(ry * m_blur_y, image_subpixel_shift);

    if rx < image_subpixel_size then
      rx := image_subpixel_size
    else
    begin
      if rx > image_subpixel_size * m_scale_limit then
        rx := image_subpixel_size * m_scale_limit;

      rx_inv := image_subpixel_size * image_subpixel_size div rx;

    end;

    if ry < image_subpixel_size then
      ry := image_subpixel_size
    else
    begin
      if ry > image_subpixel_size * m_scale_limit then
        ry := image_subpixel_size * m_scale_limit;

      ry_inv := image_subpixel_size * image_subpixel_size div ry;

    end;

    radius_x := shr_int32(diameter * rx, 1);
    radius_y := shr_int32(diameter * ry, 1);

    maxx := _source_image^._width - 1;
    maxy := _source_image^._height - 1;

    Inc(x, filter_dx_int - radius_x);
    Inc(y, filter_dy_int - radius_y);

    fg[0] := image_filter_size div 2;
    fg[1] := fg[0];
    fg[2] := fg[0];
    fg[3] := fg[0];

    y_lr := shr_int32(y, image_subpixel_shift);
    y_hr :=
      shr_int32((image_subpixel_mask - (y and image_subpixel_mask)) * ry_inv, image_subpixel_shift);

    total_weight := 0;

    x_lr_ini := shr_int32(x, image_subpixel_shift);
    x_hr_ini :=
      shr_int32((image_subpixel_mask - (x and image_subpixel_mask)) * rx_inv, image_subpixel_shift);

    repeat
      weight_y := int16_ptr(ptrcomp(weight_array) + y_hr * sizeof(int16))^;

      x_lr := x_lr_ini;
      x_hr := x_hr_ini;

      if (y_lr >= 0) and (y_lr <= maxy) then
      begin
        fg_ptr := int8u_ptr(ptrcomp(_source_image^.row(y_lr)) + (x_lr shl 2) * sizeof(int8u));

        repeat
          weight :=
            shr_int32(weight_y * int16_ptr(ptrcomp(weight_array) + x_hr * sizeof(int16))^ +
            image_filter_size div 2, downscale_shift);

          if (x_lr >= 0) and (x_lr <= maxx) then
          begin
            Inc(fg[0], fg_ptr^ * weight);
            Inc(ptrcomp(fg_ptr), sizeof(int8u));
            Inc(fg[1], fg_ptr^ * weight);
            Inc(ptrcomp(fg_ptr), sizeof(int8u));
            Inc(fg[2], fg_ptr^ * weight);
            Inc(ptrcomp(fg_ptr), sizeof(int8u));
            Inc(fg[3], fg_ptr^ * weight);
            Inc(ptrcomp(fg_ptr), sizeof(int8u));

          end
          else
          begin
            Inc(fg[m_order.R], back_r * weight);
            Inc(fg[m_order.G], back_g * weight);
            Inc(fg[m_order.B], back_b * weight);
            Inc(fg[m_order.A], back_a * weight);

            Inc(ptrcomp(fg_ptr), 4 * sizeof(int8u));

          end;

          Inc(total_weight, weight);
          Inc(x_hr, rx_inv);
          Inc(x_lr);

        until x_hr >= filter_size;

      end
      else
        repeat
          weight :=
            shr_int32(weight_y * int16_ptr(ptrcomp(weight_array) + x_hr * sizeof(int16))^ +
            image_filter_size div 2, downscale_shift);

          Inc(total_weight, weight);
          Inc(fg[m_order.R], back_r * weight);
          Inc(fg[m_order.G], back_g * weight);
          Inc(fg[m_order.B], back_b * weight);
          Inc(fg[m_order.A], back_a * weight);
          Inc(x_hr, rx_inv);

        until x_hr >= filter_size;

      Inc(y_hr, ry_inv);
      Inc(y_lr);

    until y_hr >= filter_size;

    fg[0] := fg[0] div total_weight;
    fg[1] := fg[1] div total_weight;
    fg[2] := fg[2] div total_weight;
    fg[3] := fg[3] div total_weight;

    if fg[0] < 0 then
      fg[0] := 0;

    if fg[1] < 0 then
      fg[1] := 0;

    if fg[2] < 0 then
      fg[2] := 0;

    if fg[3] < 0 then
      fg[3] := 0;

    if fg[m_order.A] > base_mask then
      fg[m_order.A] := base_mask;

    if fg[m_order.R] > fg[m_order.A] then
      fg[m_order.R] := fg[m_order.A];

    if fg[m_order.G] > fg[m_order.A] then
      fg[m_order.G] := fg[m_order.A];

    if fg[m_order.B] > fg[m_order.A] then
      fg[m_order.B] := fg[m_order.A];

    span^.r := int8u(fg[m_order.R]);
    span^.g := int8u(fg[m_order.G]);
    span^.b := int8u(fg[m_order.B]);
    span^.a := int8u(fg[m_order.A]);

    Inc(ptrcomp(span), sizeof(aggclr));

    _interpolator^.inc_operator;

    Dec(len);

  until len = 0;

  Result := _allocator^.span;

end;

end.


