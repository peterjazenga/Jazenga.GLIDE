
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}
unit
agg_rendering_buffer;

interface

{$I agg_mode.inc }

uses
  agg_basics;

{ TYPES DEFINITION }
type
  row_data_type_ptr = ^row_data_type;

  row_data_type = object
    x1, x2: int;

    ptr: int8u_ptr;

    constructor Construct; overload;
    constructor Construct(x1_, x2_: int; ptr_: int8u_ptr); overload;

  end;

  span_data_type = object
    x: int;
    len: byte;
    ptr: int8u_ptr;

    constructor Construct; overload;
    constructor Construct(x_: int; len_: byte; ptr_: int8u_ptr); overload;

  end;

{ row_ptr_cache_ptr = ^row_ptr_cache;
 row_ptr_cache = object(rendering_buffer )}

  rendering_buffer_ptr = ^rendering_buffer;

  rendering_buffer = object
    m_buf: int8u_ptr;  // Pointer to rendering buffer
    m_rows: ^int8u_ptr; // Pointers to each row of the buffer
    m_width,             // Width in pixels
    m_height: unsigned;   // Height in pixels
    m_stride: int;        // Number of bytes per row. Can be < 0

    m_max_height: unsigned; // The maximal height (currently allocated)

    constructor Construct; overload;
    constructor Construct(buf_: int8u_ptr; width_, height_: unsigned; stride_: int); overload;
    destructor Destruct; virtual;

    procedure attach(buf_: int8u_ptr; width_, height_: unsigned; stride_: int);

    function _buf: int8u_ptr;
    function _width: unsigned;
    function _height: unsigned;
    function _stride: int;

    function _stride_abs: unsigned;

    function row_xy(x, y: int; len: unsigned): int8u_ptr; virtual;
    function row(y: unsigned): int8u_ptr; virtual;
    function next_row(p: int8u_ptr): int8u_ptr; virtual;
    function rows: int8u_ptr;

    procedure copy_from(mtx: rendering_buffer_ptr);
    procedure Clear(Value: int8u);

  end;

implementation

{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor row_data_type.Construct;
begin
  x1 := 0;
  x2 := 0;

  ptr := nil;

end;

{ CONSTRUCT }
constructor row_data_type.Construct(x1_, x2_: int; ptr_: int8u_ptr);
begin
  x1 := x1_;
  x2 := x2_;

  ptr := ptr_;

end;

{ CONSTRUCT }
constructor span_data_type.Construct;
begin
  x := 0;
  len := 0;
  ptr := nil;

end;

{ CONSTRUCT }
constructor span_data_type.Construct(x_: int; len_: byte; ptr_: int8u_ptr);
begin
  x := x_;
  len := len_;
  ptr := ptr_;

end;

{ CONSTRUCT }
constructor rendering_buffer.Construct;
begin
  m_buf := nil;
  m_rows := nil;
  m_width := 0;
  m_height := 0;
  m_stride := 0;

  m_max_height := 0;

end;

{ CONSTRUCT }
constructor rendering_buffer.Construct(buf_: int8u_ptr; width_, height_: unsigned; stride_: int);
begin
  Construct;
  attach(buf_, width_, height_, stride_);

end;

{ DESTRUCT }
destructor rendering_buffer.Destruct;
begin
  agg_freemem(pointer(m_rows), m_max_height * sizeof(int8u_ptr));

end;

{ ATTACH }
procedure rendering_buffer.attach(buf_: int8u_ptr; width_, height_: unsigned; stride_: int);
var
  _rows: ^int8u_ptr;
  _row_ptr: int8u_ptr;

begin
  m_buf := buf_;
  m_width := width_;
  m_height := height_;
  m_stride := stride_;

  if height_ > m_max_height then
  begin
    agg_freemem(pointer(m_rows), m_max_height * sizeof(int8u_ptr));
    agg_getmem(pointer(m_rows), height_ * sizeof(int8u_ptr));

    m_max_height := height_;

  end;

  if stride_ < 0 then
    if height_ > 0 then
      _row_ptr := pointer(ptrcomp(m_buf) - (height_ - 1) * stride_)
    else
      _row_ptr := nil
  else
    _row_ptr := m_buf;

  _rows := pointer(m_rows);

  while height_ > 0 do
  begin
    _rows^ := _row_ptr;

    Inc(ptrcomp(_row_ptr), stride_);
    Inc(ptrcomp(_rows), sizeof(int8u_ptr));

    Dec(height_);

  end;

end;

{ _BUF }
function rendering_buffer._buf: int8u_ptr;
begin
  Result := m_buf;

end;

{ _WIDTH }
function rendering_buffer._width: unsigned;
begin
  Result := m_width;

end;

{ _HEIGHT }
function rendering_buffer._height: unsigned;
begin
  Result := m_height;

end;

{ _STRIDE }
function rendering_buffer._stride: int;
begin
  Result := m_stride;

end;

{ _STRIDE_ABS }
function rendering_buffer._stride_abs: unsigned;
begin
  if m_stride < 0 then
    Result := -m_stride
  else
    Result := m_stride;

end;

{ ROW_XY }
function rendering_buffer.row_xy(x, y: int; len: unsigned): int8u_ptr;
begin
  Result := pointer(pointer(ptrcomp(m_rows) + y * sizeof(int8u_ptr))^);

end;

{ ROW }
function rendering_buffer.row(y: unsigned): int8u_ptr;
begin
  Result := pointer(pointer(ptrcomp(m_rows) + y * sizeof(int8u_ptr))^);

end;

{ NEXT_ROW }
function rendering_buffer.next_row(p: int8u_ptr): int8u_ptr;
begin
  Result := pointer(ptrcomp(p) + m_stride);

end;

{ ROWS }
function rendering_buffer.rows: int8u_ptr;
begin
  Result := pointer(m_rows);

end;

{ COPY_FROM }
procedure rendering_buffer.copy_from(mtx: rendering_buffer_ptr);
var
  h, l, y: unsigned;

begin
  h := _height;

  if mtx^._height < h then
    h := mtx^._height;

  l := _stride_abs;

  if mtx^._stride_abs < l then
    l := mtx^._stride_abs;

  l := l * sizeof(int8u);

  if h > 0 then
    for y := 0 to h - 1 do
      move(mtx^.row(y)^, row(y)^, l);

end;

{ CLEAR }
procedure rendering_buffer.Clear(Value: int8u);
var
  y, x: unsigned;
  p: int8u_ptr;

begin
  if _height > 0 then
    for y := 0 to _height - 1 do
    begin
      p := row(y);

      if _stride_abs > 0 then
        for x := 0 to _stride_abs - 1 do
        begin
          p^ := Value;

          Inc(ptrcomp(p), sizeof(int8u));

        end;

    end;

end;

end.
