
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}
unit
agg_span_gradient_image;

interface

{$I agg_mode.inc }

uses
  agg_basics,
  agg_color,
  agg_array,
  agg_span_gradient,
  agg_pixfmt,
  agg_pixfmt_rgba,
  agg_rendering_buffer;

{ GLOBAL VARIABLES & CONSTANTS }
{ TYPES DEFINITION }
type
  one_color_function = object(array_base)
    m_color: aggclr;

    constructor Construct;

    function size: unsigned; virtual;
    function entry: unsigned; virtual;
    function array_operator(i: unsigned): pointer; virtual;

  end;

  gradient_image = object(gradient)
  private
    m_buffer: pointer;
    m_alocdx, m_alocdy, m_width, m_height: int;

    m_renbuf: rendering_buffer_ptr;
    m_pixelf: pixel_formats;

    m_color: aggclr_ptr;

    m_color_function: one_color_function;

  public
    constructor Construct;
    destructor Destruct;

    function image_create(Width, Height: int): pointer;
    function image_buffer: pointer;
    function image_width: int;
    function image_height: int;
    function image_stride: int;

    function calculate(x, y, d: int): int; virtual;

    function pixel_format: pixel_formats_ptr;
    function color_function: array_base_ptr;

  end;

implementation

constructor one_color_function.Construct;
begin
  m_color.Construct;
end;

{ SIZE }
function one_color_function.size: unsigned;
begin
  Result := 1;

end;

{ ENTRY }
function one_color_function.entry: unsigned;
begin
  Result := sizeof(aggclr);

end;

{ ARRAY_OPERATOR }
function one_color_function.array_operator(i: unsigned): pointer;
begin
  Result := @m_color;

end;

{ CONSTRUCT }
constructor gradient_image.Construct;
begin
  m_color_function.Construct;

  m_buffer := nil;
  m_alocdx := 0;
  m_alocdy := 0;
  m_width := 0;
  m_height := 0;

  m_renbuf := nil;

  pixfmt_undefined(m_pixelf);

  m_color := m_color_function.array_operator(0);

end;

{ DESTRUCT }
destructor gradient_image.Destruct;
begin
  if m_buffer <> nil then
    agg_freemem(m_buffer, m_alocdx * m_alocdy * 4);

  if m_renbuf <> nil then
    dispose(m_renbuf, Destruct);

end;

{ IMAGE_CREATE }
function gradient_image.image_create(Width, Height: int): pointer;
var
  row: pointer;
  rows: unsigned;

begin
  Result := nil;

  if m_renbuf <> nil then
    dispose(m_renbuf, Destruct);

  m_renbuf := nil;

  if (Width > m_alocdx) or (Height > m_alocdy) then
  begin
    if m_buffer <> nil then
      agg_freemem(m_buffer, m_alocdx * m_alocdy * 4);

    m_buffer := nil;

    if agg_getmem(m_buffer, Width * Height * 4) then
    begin
      m_alocdx := Width;
      m_alocdy := Height;

    end
    else
    begin
      m_alocdx := 0;
      m_alocdy := 0;

    end;

  end;

  if m_buffer <> nil then
  begin
    m_width := Width;
    m_height := Height;

    row := m_buffer;
    rows := Height;

    while rows > 0 do
    begin
      FillChar(row^, m_width * 4, 0);

      Inc(ptrcomp(row), m_alocdx * 4);
      Dec(rows);

    end;

    Result := m_buffer;

  end
  else
  begin
    m_width := 0;
    m_height := 0;

  end;

end;

{ IMAGE_BUFFER }
function gradient_image.image_buffer: pointer;
begin
  Result := m_buffer;

end;

{ IMAGE_WIDTH }
function gradient_image.image_width: int;
begin
  Result := m_width;

end;

{ IMAGE_HEIGHT }
function gradient_image.image_height: int;
begin
  Result := m_height;

end;

{ IMAGE_STRIDE }
function gradient_image.image_stride: int;
begin
  Result := m_alocdx * 4;

end;

{ CALCULATE }
function gradient_image.calculate(x, y, d: int): int;
var
  px, py: int;

  pixel: rgba8_ptr;

begin
  Result := 0;

  if m_buffer <> nil then
  begin
    px := shr_int32(x, gradient_subpixel_shift);
    py := shr_int32(y, gradient_subpixel_shift);

    px := px mod m_width;

    if px < 0 then
      px := m_width + px;

    py := py mod m_height;

    if py < 0 then
      py := m_height + py;

    pixel := rgba8_ptr(ptrcomp(m_buffer) + py * (m_alocdx * 4) + px * 4);

    m_color^.r := pixel^.r;
    m_color^.g := pixel^.g;
    m_color^.b := pixel^.b;
    m_color^.a := pixel^.a;

  end
  else
  begin
    m_color^.r := 0;
    m_color^.g := 0;
    m_color^.b := 0;
    m_color^.a := 0;

  end;

end;

{ PIXEL_FORMAT }
function gradient_image.pixel_format: pixel_formats_ptr;
begin
  if (m_buffer <> nil) and (m_renbuf = nil) then
  begin
    new(m_renbuf, Construct);

    m_renbuf^.attach(m_buffer, m_width, m_height, m_alocdx * 4);
    pixfmt_rgba32(m_pixelf, m_renbuf);

  end;

  if m_renbuf = nil then
    Result := nil
  else
    Result := @m_pixelf;

end;

{ COLOR_FUNCTION }
function gradient_image.color_function: array_base_ptr;
begin
  Result := @m_color_function;

end;

end.






