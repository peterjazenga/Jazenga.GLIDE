
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}
unit agg_image_filters;

interface

{$I agg_mode.inc }
{$Q- }
{$R- }

uses
  Math,
  agg_basics,
  agg_math;

const
  image_filter_shift = 14;                       //----image_filter_shift
  image_filter_size = 1 shl image_filter_shift; //----image_filter_size
  image_filter_mask = image_filter_size - 1;    //----image_filter_mask

  image_subpixel_shift = 8;                          //----image_subpixel_shift
  image_subpixel_size = 1 shl image_subpixel_shift; //----image_subpixel_size
  image_subpixel_mask = image_subpixel_size - 1;    //----image_subpixel_mask

type

  image_filter_base_ptr = ^image_filter_base;

  image_filter_base = object
    constructor Construct;
    function radius: double; virtual; abstract;
    function calc_weight(x: double): double; virtual; abstract;
    procedure set_radius(r: double); virtual;
  end;

  image_filter_lut_ptr = ^image_filter_lut;

  image_filter_lut = object
    m_radius: double;
    m_diameter: unsigned;
    m_start: int;
    m_weight_array: int16_ptr;
    m_max_size: unsigned;
    constructor Construct; overload;
    constructor Construct(filter: image_filter_base_ptr; normalization: boolean = True); overload;
    destructor Destruct;
    procedure calculate(filter: image_filter_base_ptr; normalization: boolean = True);
    function radius: double;
    function diameter: unsigned;
    function start: int;
    function weight_array: int16_ptr;
    procedure normalize;
    procedure realloc_lut(radius_: double);
  end;

  image_filter_ptr = ^image_filter;

  image_filter = object(image_filter_lut)
    m_filter_function: image_filter_base_ptr;
    constructor Construct(filter: image_filter_base_ptr);
  end;

  image_filter_bilinear_ptr = ^image_filter_bilinear;

  image_filter_bilinear = object(image_filter_base)
    function radius: double; virtual;
    function calc_weight(x: double): double; virtual;
  end;

  image_filter_hanning_ptr = ^image_filter_hanning;

  image_filter_hanning = object(image_filter_base)
    function radius: double; virtual;
    function calc_weight(x: double): double; virtual;
  end;

  image_filter_hamming_ptr = ^image_filter_hamming;

  image_filter_hamming = object(image_filter_base)
    function radius: double; virtual;
    function calc_weight(x: double): double; virtual;
  end;

  image_filter_hermite_ptr = ^image_filter_hermite;

  image_filter_hermite = object(image_filter_base)
    function radius: double; virtual;
    function calc_weight(x: double): double; virtual;
  end;

  image_filter_quadric_ptr = ^image_filter_quadric;

  image_filter_quadric = object(image_filter_base)
    function radius: double; virtual;
    function calc_weight(x: double): double; virtual;
  end;

  image_filter_bicubic_ptr = ^image_filter_bicubic;

  image_filter_bicubic = object(image_filter_base)
    function pow3(x: double): double;
    function radius: double; virtual;
    function calc_weight(x: double): double; virtual;
  end;

  image_filter_kaiser_ptr = ^image_filter_kaiser;

  image_filter_kaiser = object(image_filter_base)
    a, i0a, epsilon: double;
    constructor Construct(b: double = 6.33);
    function radius: double; virtual;
    function calc_weight(x: double): double; virtual;
    function bessel_i0(x: double): double;
  end;

  image_filter_catrom_ptr = ^image_filter_catrom;

  image_filter_catrom = object(image_filter_base)
    function radius: double; virtual;
    function calc_weight(x: double): double; virtual;
  end;

  image_filter_mitchell_ptr = ^image_filter_mitchell;

  image_filter_mitchell = object(image_filter_base)
    p0, p2, p3, q0, q1, q2, q3: double;
    constructor Construct(b: double = 1.0 / 3.0; c: double = 1.0 / 3.0);
    function radius: double; virtual;
    function calc_weight(x: double): double; virtual;
  end;

  image_filter_spline16_ptr = ^image_filter_spline16;

  image_filter_spline16 = object(image_filter_base)
    function radius: double; virtual;
    function calc_weight(x: double): double; virtual;
  end;

  image_filter_spline36_ptr = ^image_filter_spline36;

  image_filter_spline36 = object(image_filter_base)
    function radius: double; virtual;
    function calc_weight(x: double): double; virtual;
  end;

  image_filter_gaussian_ptr = ^image_filter_gaussian;

  image_filter_gaussian = object(image_filter_base)
    function radius: double; virtual;
    function calc_weight(x: double): double; virtual;
  end;

  image_filter_bessel_ptr = ^image_filter_bessel;

  image_filter_bessel = object(image_filter_base)
    function radius: double; virtual;
    function calc_weight(x: double): double; virtual;
  end;

  image_filter_sinc_ptr = ^image_filter_sinc;

  image_filter_sinc = object(image_filter_base)
    m_radius: double;
    constructor Construct(r: double);
    function radius: double; virtual;
    function calc_weight(x: double): double; virtual;
    procedure set_radius(r: double); virtual;
  end;

  image_filter_lanczos_ptr = ^image_filter_lanczos;

  image_filter_lanczos = object(image_filter_base)
    m_radius: double;
    constructor Construct(r: double);
    function radius: double; virtual;
    function calc_weight(x: double): double; virtual;
    procedure set_radius(r: double); virtual;
  end;

  image_filter_blackman_ptr = ^image_filter_blackman;

  image_filter_blackman = object(image_filter_base)
    m_radius: double;
    constructor Construct(r: double);
    function radius: double; virtual;
    function calc_weight(x: double): double; virtual;
    procedure set_radius(r: double); virtual;
  end;

  image_filter_sinc36_ptr = ^image_filter_sinc36;

  image_filter_sinc36 = object(image_filter_sinc)
    constructor Construct;
  end;

  image_filter_sinc64_ptr = ^image_filter_sinc64;

  image_filter_sinc64 = object(image_filter_sinc)
    constructor Construct;
  end;

  image_filter_sinc100_ptr = ^image_filter_sinc100;

  image_filter_sinc100 = object(image_filter_sinc)
    constructor Construct;
  end;

  image_filter_sinc144_ptr = ^image_filter_sinc144;

  image_filter_sinc144 = object(image_filter_sinc)
    constructor Construct;
  end;

  image_filter_sinc196_ptr = ^image_filter_sinc196;

  image_filter_sinc196 = object(image_filter_sinc)
    constructor Construct;
  end;

  image_filter_sinc256_ptr = ^image_filter_sinc256;

  image_filter_sinc256 = object(image_filter_sinc)
    constructor Construct;
  end;

  image_filter_lanczos36_ptr = ^image_filter_lanczos36;

  image_filter_lanczos36 = object(image_filter_lanczos)
    constructor Construct;
  end;

  image_filter_lanczos64_ptr = ^image_filter_lanczos64;

  image_filter_lanczos64 = object(image_filter_lanczos)
    constructor Construct;
  end;

  image_filter_lanczos100_ptr = ^image_filter_lanczos100;

  image_filter_lanczos100 = object(image_filter_lanczos)
    constructor Construct;
  end;

  image_filter_lanczos144_ptr = ^image_filter_lanczos144;

  image_filter_lanczos144 = object(image_filter_lanczos)
    constructor Construct;
  end;

  image_filter_lanczos196_ptr = ^image_filter_lanczos196;

  image_filter_lanczos196 = object(image_filter_lanczos)
    constructor Construct;
  end;

  image_filter_lanczos256_ptr = ^image_filter_lanczos256;

  image_filter_lanczos256 = object(image_filter_lanczos)
    constructor Construct;
  end;

  image_filter_blackman36_ptr = ^image_filter_blackman36;

  image_filter_blackman36 = object(image_filter_blackman)
    constructor Construct;
  end;

  image_filter_blackman64_ptr = ^image_filter_blackman64;

  image_filter_blackman64 = object(image_filter_blackman)
    constructor Construct;
  end;

  image_filter_blackman100_ptr = ^image_filter_blackman100;

  image_filter_blackman100 = object(image_filter_blackman)
    constructor Construct;
  end;

  image_filter_blackman144_ptr = ^image_filter_blackman144;

  image_filter_blackman144 = object(image_filter_blackman)
    constructor Construct;
  end;

  image_filter_blackman196_ptr = ^image_filter_blackman196;

  image_filter_blackman196 = object(image_filter_blackman)
    constructor Construct;
  end;

  image_filter_blackman256_ptr = ^image_filter_blackman256;

  image_filter_blackman256 = object(image_filter_blackman)
    constructor Construct;
  end;


implementation

constructor image_filter_base.Construct;
begin
end;

{ SET_RADIUS }
procedure image_filter_base.set_radius(r: double);
begin
end;

{ CONSTRUCT }
constructor image_filter_lut.Construct;
begin
  m_weight_array := nil;
  m_max_size := 0;

end;

{ CONSTRUCT }
constructor image_filter_lut.Construct(filter: image_filter_base_ptr; normalization: boolean = True);
begin
  m_weight_array := nil;
  m_max_size := 0;

  calculate(filter, normalization);

end;

{ DESTRUCT }
destructor image_filter_lut.Destruct;
begin
  agg_freemem(pointer(m_weight_array), m_max_size * sizeof(int16));

end;

{ CALCULATE }
procedure image_filter_lut.calculate(filter: image_filter_base_ptr; normalization: boolean = True);
var
  r, x, y: double;

  i, pivot, end_: unsigned;

begin
  r := filter^.radius;

  realloc_lut(r);

  pivot := diameter shl (image_subpixel_shift - 1);

  i := 0;

  while i < pivot do
  begin
    x := i / image_subpixel_size;
    y := filter^.calc_weight(x);

    int16_ptr(ptrcomp(m_weight_array) + (pivot + i) * sizeof(int16))^ :=
      int16(trunc(y * image_filter_size + 0.5));

    int16_ptr(ptrcomp(m_weight_array) + (pivot - i) * sizeof(int16))^ :=
      int16_ptr(ptrcomp(m_weight_array) + (pivot + i) * sizeof(int16))^;

    Inc(i);

  end;

  end_ := (diameter shl image_subpixel_shift) - 1;

  int16_ptr(ptrcomp(m_weight_array) + 0 * sizeof(int16))^ :=
    int16_ptr(ptrcomp(m_weight_array) + end_ * sizeof(int16))^;

  if normalization then
    normalize;

end;

{ RADIUS }
function image_filter_lut.radius: double;
begin
  Result := m_radius;

end;

{ DIAMETER }
function image_filter_lut.diameter: unsigned;
begin
  Result := m_diameter;

end;

{ START }
function image_filter_lut.start: int;
begin
  Result := m_start;

end;

{ WEIGHT_ARRAY }
function image_filter_lut.weight_array: int16_ptr;
begin
  Result := m_weight_array;

end;

{ NORMALIZE }
// This function normalizes integer values and corrects the rounding
// errors. It doesn't do anything with the source floating point values
// (m_weight_array_dbl), it corrects only integers according to the rule
// of 1.0 which means that any sum of pixel weights must be equal to 1.0.
// So, the filter function must produce a graph of the proper shape.
procedure image_filter_lut.normalize;
var
  k: double;

  i, j, idx, pivot, end_: unsigned;

  flip, sum, inc_, v: int;

begin
  flip := 1;
  i := 0;

  while i < image_subpixel_size do
  begin
    repeat
      sum := 0;

      for j := 0 to m_diameter - 1 do
        Inc(
          sum,
          int16_ptr(ptrcomp(m_weight_array) + (j * image_subpixel_size + i) * sizeof(int16))^);

      if sum = image_filter_size then
        break;

      k := image_filter_size / sum;
      sum := 0;

      for j := 0 to m_diameter - 1 do
      begin
        int16_ptr(ptrcomp(m_weight_array) + (j * image_subpixel_size + i) * sizeof(int16))^ :=
          int16(trunc(int16_ptr(ptrcomp(m_weight_array) + (j * image_subpixel_size + i) * sizeof(int16))^ * k));

        Inc(sum, int16_ptr(ptrcomp(m_weight_array) + (j * image_subpixel_size + i) * sizeof(int16))^);

      end;

      Dec(sum, image_filter_size);

      if sum > 0 then
        inc_ := -1
      else
        inc_ := 1;

      j := 0;

      while (j < m_diameter) and (sum <> 0) do
      begin
        flip := flip xor 1;

        if flip <> 0 then
          idx := m_diameter div 2 + j div 2
        else
          idx := m_diameter div 2 - j div 2;

        v := int16_ptr(ptrcomp(m_weight_array) + (idx * image_subpixel_size + i) * sizeof(int16))^;

        if v < image_filter_size then
        begin
          Inc(
            int16_ptr(ptrcomp(m_weight_array) + (idx * image_subpixel_size + i) * sizeof(int16))^,
            inc_);

          Inc(sum, inc_);

        end;

        Inc(j);

      end;

    until False;

    Inc(i);

  end;

  pivot := m_diameter shl (image_subpixel_shift - 1);

  for i := 0 to pivot - 1 do
    int16_ptr(ptrcomp(m_weight_array) + (pivot + i) * sizeof(int16))^ :=
      int16_ptr(ptrcomp(m_weight_array) + (pivot - i) * sizeof(int16))^;

  end_ := (diameter shl image_subpixel_shift) - 1;

  int16_ptr(ptrcomp(m_weight_array) + 0 * sizeof(int16))^ :=
    int16_ptr(ptrcomp(m_weight_array) + end_ * sizeof(int16))^;

end;

{ REALLOC_LUT }
procedure image_filter_lut.realloc_lut(radius_: double);
var
  size: unsigned;

begin
  m_radius := radius_;
  m_diameter := unsigned(trunc(Ceil(radius_))) * 2;
  m_start := -int(m_diameter div 2 - 1);

  size := m_diameter shl image_subpixel_shift;

  if size > m_max_size then
  begin
    agg_freemem(pointer(m_weight_array), m_max_size * sizeof(int16));
    agg_getmem(pointer(m_weight_array), size * sizeof(int16));

    m_max_size := size;

  end;

end;

{ CONSTRUCT }
constructor image_filter.Construct(filter: image_filter_base_ptr);
begin
  inherited Construct;

  m_filter_function := filter;

  calculate(m_filter_function);

end;

{ RADIUS }
function image_filter_bilinear.radius: double;
begin
  Result := 1.0;
end;

{ CALC_WEIGHT }
function image_filter_bilinear.calc_weight(x: double): double;
begin
  Result := 1.0 - x;

end;

{ RADIUS }
function image_filter_hanning.radius: double;
begin
  Result := 1.0;

end;

{ CALC_WEIGHT }
function image_filter_hanning.calc_weight(x: double): double;
begin
  Result := 0.5 + 0.5 * Cos(pi * x);

end;

{ RADIUS }
function image_filter_hamming.radius: double;
begin
  Result := 1.0;

end;

{ CALC_WEIGHT }
function image_filter_hamming.calc_weight(x: double): double;
begin
  Result := 0.54 + 0.46 * Cos(pi * x);

end;

{ RADIUS }
function image_filter_hermite.radius: double;
begin
  Result := 1.0;

end;

{ CALC_WEIGHT }
function image_filter_hermite.calc_weight(x: double): double;
begin
  Result := (2.0 * x - 3.0) * x * x + 1.0;

end;

{ RADIUS }
function image_filter_quadric.radius: double;
begin
  Result := 1.5;

end;

{ CALC_WEIGHT }
function image_filter_quadric.calc_weight(x: double): double;
var
  t: double;

begin
  if x < 0.5 then
    Result := 0.75 - x * x
  else
  if x < 1.5 then
  begin
    t := x - 1.5;

    Result := 0.5 * t * t;

  end
  else
    Result := 0.0;

end;

{ POW3 }
function image_filter_bicubic.pow3(x: double): double;
begin
  if x <= 0.0 then
    Result := 0.0
  else
    Result := x * x * x;

end;

{ RADIUS }
function image_filter_bicubic.radius: double;
begin
  Result := 2.0;

end;

{ CALC_WEIGHT }
function image_filter_bicubic.calc_weight(x: double): double;
begin
  Result :=
    (1.0 / 6.0) * (pow3(x + 2) - 4 * pow3(x + 1) + 6 * pow3(x) - 4 * pow3(x - 1));

end;

{ CONSTRUCT }
constructor image_filter_kaiser.Construct(b: double = 6.33);
begin
  a := b;

  epsilon := 1e-12;

  i0a := 1.0 / bessel_i0(b);

end;

{ RADIUS }
function image_filter_kaiser.radius: double;
begin
  Result := 1.0;

end;

{ CALC_WEIGHT }
function image_filter_kaiser.calc_weight(x: double): double;
begin
  Result := bessel_i0(a * Sqrt(1.0 - x * x)) * i0a;

end;

{ BESSEL_I0 }
function image_filter_kaiser.bessel_i0(x: double): double;
var
  i: int;

  sum, y, t: double;

begin
  sum := 1;

  y := x * x / 4.0;
  t := y;
  i := 2;

  while t > epsilon do
  begin
    sum := sum + t;

    t := t * (y / (i * i));

    Inc(i);

  end;

  Result := sum;

end;

{ RADIUS }
function image_filter_catrom.radius: double;
begin
  Result := 2.0;

end;

{ CALC_WEIGHT }
function image_filter_catrom.calc_weight(x: double): double;
begin
  if x < 1.0 then
    Result := 0.5 * (2.0 + x * x * (-5.0 + x * 3.0))
  else
  if x < 2.0 then
    Result := 0.5 * (4.0 + x * (-8.0 + x * (5.0 - x)))
  else
    Result := 0.0;

end;

{ CONSTRUCT }
constructor image_filter_mitchell.Construct(b: double = 1.0 / 3.0; c: double = 1.0 / 3.0);
begin
  p0 := (6.0 - 2.0 * b) / 6.0;
  p2 := (-18.0 + 12.0 * b + 6.0 * c) / 6.0;
  p3 := (12.0 - 9.0 * b - 6.0 * c) / 6.0;
  q0 := (8.0 * b + 24.0 * c) / 6.0;
  q1 := (-12.0 * b - 48.0 * c) / 6.0;
  q2 := (6.0 * b + 30.0 * c) / 6.0;
  q3 := (-b - 6.0 * c) / 6.0;

end;

{ RADIUS }
function image_filter_mitchell.radius: double;
begin
  Result := 2.0;

end;

{ CALC_WEIGHT }
function image_filter_mitchell.calc_weight(x: double): double;
begin
  if x < 1.0 then
    Result := p0 + x * x * (p2 + x * p3)
  else
  if x < 2.0 then
    Result := q0 + x * (q1 + x * (q2 + x * q3))
  else
    Result := 0.0;

end;

{ RADIUS }
function image_filter_spline16.radius: double;
begin
  Result := 2.0;

end;

{ CALC_WEIGHT }
function image_filter_spline16.calc_weight(x: double): double;
begin
  if x < 1.0 then
    Result := ((x - 9.0 / 5.0) * x - 1.0 / 5.0) * x + 1.0
  else
    Result := ((-1.0 / 3.0 * (x - 1) + 4.0 / 5.0) * (x - 1) - 7.0 / 15.0) * (x - 1);

end;

{ RADIUS }
function image_filter_spline36.radius: double;
begin
  Result := 3.0;

end;

{ CALC_WEIGHT }
function image_filter_spline36.calc_weight(x: double): double;
begin
  if x < 1.0 then
    Result := ((13.0 / 11.0 * x - 453.0 / 209.0) * x - 3.0 / 209.0) * x + 1.0
  else
  if x < 2.0 then
    Result := ((-6.0 / 11.0 * (x - 1) + 270.0 / 209.0) * (x - 1) - 156.0 / 209.0) * (x - 1)
  else
    Result := ((1.0 / 11.0 * (x - 2) - 45.0 / 209.0) * (x - 2) + 26.0 / 209.0) * (x - 2);

end;

{ RADIUS }
function image_filter_gaussian.radius: double;
begin
  Result := 2.0;

end;

{ CALC_WEIGHT }
function image_filter_gaussian.calc_weight(x: double): double;
begin
  Result := Exp(-2.0 * x * x) * Sqrt(2.0 / pi);

end;

{ RADIUS }
function image_filter_bessel.radius: double;
begin
  Result := 3.2383;

end;

{ CALC_WEIGHT }
function image_filter_bessel.calc_weight(x: double): double;
begin
  if x = 0.0 then
    Result := pi / 4.0
  else
    Result := besj(pi * x, 1) / (2.0 * x);

end;

{ CONSTRUCT }
constructor image_filter_sinc.Construct(r: double);
begin
  if r < 2.0 then
    m_radius := 2.0
  else
    m_radius := r;

end;

{ RADIUS }
function image_filter_sinc.radius: double;
begin
  Result := m_radius;

end;

{ CALC_WEIGHT }
function image_filter_sinc.calc_weight(x: double): double;
begin
  if x = 0.0 then
    Result := 1.0
  else
  begin
    x := x * pi;

    Result := Sin(x) / x;

  end;

end;

{ SET_RADIUS }
procedure image_filter_sinc.set_radius(r: double);
begin
  if r < 2.0 then
    m_radius := 2.0
  else
    m_radius := r;

end;

{ CONSTRUCT }
constructor image_filter_lanczos.Construct(r: double);
begin
  if r < 2.0 then
    m_radius := 2.0
  else
    m_radius := r;

end;

{ RADIUS }
function image_filter_lanczos.radius: double;
begin
  Result := m_radius;

end;

{ CALC_WEIGHT }
function image_filter_lanczos.calc_weight(x: double): double;
var
  xr: double;

begin
  if x = 0.0 then
    Result := 1.0
  else
  if x > m_radius then
    Result := 0.0
  else
  begin
    x := x * pi;
    xr := x / m_radius;

    Result := (Sin(x) / x) * (Sin(xr) / xr);

  end;

end;

{ SET_RADIUS }
procedure image_filter_lanczos.set_radius(r: double);
begin
  if r < 2.0 then
    m_radius := 2.0
  else
    m_radius := r;

end;

{ CONSTRUCT }
constructor image_filter_blackman.Construct(r: double);
begin
  if r < 2.0 then
    m_radius := 2.0
  else
    m_radius := r;

end;

{ RADIUS }
function image_filter_blackman.radius: double;
begin
  Result := m_radius;

end;

{ CALC_WEIGHT }
function image_filter_blackman.calc_weight(x: double): double;
var
  xr: double;

begin
  if x = 0.0 then
    Result := 1.0
  else
  if x > m_radius then
    Result := 0.0
  else
  begin
    x := x * pi;
    xr := x / m_radius;

    Result := (Sin(x) / x) * (0.42 + 0.5 * Cos(xr) + 0.08 * Cos(2 * xr));

  end;

end;

{ SET_RADIUS }
procedure image_filter_blackman.set_radius(r: double);
begin
  if r < 2.0 then
    m_radius := 2.0
  else
    m_radius := r;

end;

{ CONSTRUCT }
constructor image_filter_sinc36.Construct;
begin
  inherited Construct(3.0);

end;

{ CONSTRUCT }
constructor image_filter_sinc64.Construct;
begin
  inherited Construct(4.0);

end;

{ CONSTRUCT }
constructor image_filter_sinc100.Construct;
begin
  inherited Construct(5.0);

end;

{ CONSTRUCT }
constructor image_filter_sinc144.Construct;
begin
  inherited Construct(6.0);

end;

{ CONSTRUCT }
constructor image_filter_sinc196.Construct;
begin
  inherited Construct(7.0);

end;

{ CONSTRUCT }
constructor image_filter_sinc256.Construct;
begin
  inherited Construct(8.0);

end;

{ CONSTRUCT }
constructor image_filter_lanczos36.Construct;
begin
  inherited Construct(3.0);

end;

{ CONSTRUCT }
constructor image_filter_lanczos64.Construct;
begin
  inherited Construct(4.0);

end;

{ CONSTRUCT }
constructor image_filter_lanczos100.Construct;
begin
  inherited Construct(5.0);

end;

{ CONSTRUCT }
constructor image_filter_lanczos144.Construct;
begin
  inherited Construct(6.0);

end;

{ CONSTRUCT }
constructor image_filter_lanczos196.Construct;
begin
  inherited Construct(7.0);

end;

{ CONSTRUCT }
constructor image_filter_lanczos256.Construct;
begin
  inherited Construct(8.0);

end;

{ CONSTRUCT }
constructor image_filter_blackman36.Construct;
begin
  inherited Construct(3.0);

end;

{ CONSTRUCT }
constructor image_filter_blackman64.Construct;
begin
  inherited Construct(4.0);

end;

{ CONSTRUCT }
constructor image_filter_blackman100.Construct;
begin
  inherited Construct(5.0);

end;

{ CONSTRUCT }
constructor image_filter_blackman144.Construct;
begin
  inherited Construct(6.0);

end;

{ CONSTRUCT }
constructor image_filter_blackman196.Construct;
begin
  inherited Construct(7.0);

end;

{ CONSTRUCT }
constructor image_filter_blackman256.Construct;
begin
  inherited Construct(8.0);

end;

end.
