
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}
unit
agg_rendering_buffer_dynarow;

interface

{$I agg_mode.inc }

uses
  agg_basics,
  agg_rendering_buffer;

{ TYPES DEFINITION }
type
  //===============================================rendering_buffer_dynarow
  // Rendering buffer class with dynamic allocation of the rows.
  // The rows are allocated as needed when requesting for span_ptr().
  // The class automatically calculates min_x and max_x for each row.
  // Generally it's more efficient to use this class as a temporary buffer
  // for rendering a few lines and then to blend it with another buffer.
  rendering_buffer_dynarow_ptr = ^rendering_buffer_dynarow;

  rendering_buffer_dynarow = object(rendering_buffer)
  private
    m_buff: row_data_type_ptr; // Pointers to each row of the buffer
    m_alloc, m_byte_width: unsigned;      // Width in bytes

  public
    constructor Construct(Width, Height, byte_width: unsigned);
    destructor Destruct; virtual;

    procedure init(Width, Height, byte_width: unsigned);

    function _width: unsigned;
    function _height: unsigned;
    function _byte_width: unsigned;

    function row_xy(x, y: int; len: unsigned): int8u_ptr; virtual;
    function row(y: unsigned): int8u_ptr; virtual;

  end;

implementation

constructor rendering_buffer_dynarow.Construct(Width, Height, byte_width: unsigned);
begin
  m_alloc := sizeof(row_data_type) * Height;

  agg_getmem(pointer(m_buff), m_alloc);

  m_width := Width;
  m_height := Height;

  m_byte_width := byte_width;

  FillChar(m_buff^, m_alloc, 0);

end;

{ DESTRUCT }
destructor rendering_buffer_dynarow.Destruct;
begin
  init(0, 0, 0);

end;

{ INIT }
// Allocate and clear the buffer
procedure rendering_buffer_dynarow.init(Width, Height, byte_width: unsigned);
var
  i: unsigned;

begin
  i := 0;

  while i < m_height do
  begin
    agg_freemem(
      pointer(row_data_type_ptr(ptrcomp(m_buff) + i * sizeof(row_data_type))^.ptr),
      m_byte_width);

    Inc(i);

  end;

  agg_freemem(pointer(m_buff), m_alloc);

  m_buff := nil;

  if (Width <> 0) and (Height <> 0) then
  begin
    m_width := Width;
    m_height := Height;

    m_byte_width := byte_width;

    m_alloc := sizeof(row_data_type) * Height;

    agg_getmem(pointer(m_buff), m_alloc);
    FillChar(m_buff^, m_alloc, 0);

  end;

end;

{ _WIDTH }
function rendering_buffer_dynarow._width: unsigned;
begin
  Result := m_width;

end;

{ _HEIGHT }
function rendering_buffer_dynarow._height: unsigned;
begin
  Result := m_height;

end;

{ _BYTE_WIDTH }
function rendering_buffer_dynarow._byte_width: unsigned;
begin
  Result := m_byte_width;

end;

{ ROW_XY }
// The main function used for rendering. Returns pointer to the
// pre-allocated span. Memory for the row is allocated as needed.
function rendering_buffer_dynarow.row_xy(x, y: int; len: unsigned): int8u_ptr;
var
  r: row_data_type_ptr;
  p: int8u_ptr;

  x2: int;

begin
  r := row_data_type_ptr(ptrcomp(m_buff) + y * sizeof(row_data_type));
  x2 := x + len - 1;

  if r^.ptr <> nil then
  begin
    if x < r^.x1 then
      r^.x1 := x;

    if x2 > r^.x2 then
      r^.x2 := x2;

  end
  else
  begin
    agg_getmem(pointer(p), m_byte_width);

    r^.ptr := p;
    r^.x1 := x;
    r^.x2 := x2;

    FillChar(p^, m_byte_width, 0);

  end;

  Result := r^.ptr;

end;

{ ROW }
function rendering_buffer_dynarow.row(y: unsigned): int8u_ptr;
begin
  Result := row_xy(0, y, m_width);

end;

end.



