
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}
unit
agg_path_storage_integer;

interface

{$I agg_mode.inc }

uses
  agg_basics,
  agg_array,
  agg_vertex_source;

const

  cmd_move_to = 0;
  cmd_line_to = 1;
  cmd_curve3 = 2;
  cmd_curve4 = 3;

  coord_shift = 6;
  coord_scale = 1 shl coord_shift;

type

  vertex_int16_ptr = ^vertex_int16;

  vertex_int16 = object
    x, y: int16;
    constructor Construct; overload;
    constructor Construct(x_, y_: int16; flag: unsigned); overload;
    function vertex(x_, y_: double_ptr; dx: double = 0; dy: double = 0; scale: double = 1.0): unsigned;
  end;

  vertex_int32_ptr = ^vertex_int32;

  vertex_int32 = object
    x, y: int32;
    constructor Construct; overload;
    constructor Construct(x_, y_: int32; flag: unsigned); overload;
    function vertex(x_, y_: double_ptr; dx: double = 0; dy: double = 0; scale: double = 1.0): unsigned;
  end;

  path_storage_integer_ptr = ^path_storage_integer;

  path_storage_integer = object(vertex_source)
    procedure move_to(x, y: int32); virtual; abstract;
    procedure line_to(x, y: int32); virtual; abstract;
    procedure curve3(x_ctrl, y_ctrl, x_to, y_to: int32); virtual; abstract;
    procedure curve4(x_ctrl1, y_ctrl1, x_ctrl2, y_ctrl2, x_to, y_to: int32); virtual; abstract;
    function vertex_(idx: unsigned; x, y: int32_ptr): unsigned; virtual; abstract;
    procedure close_polygon; virtual; abstract;
  end;

  path_storage_int16_ptr = ^path_storage_int16;

  path_storage_int16 = object(path_storage_integer)
    m_storage: pod_deque;
    m_vertex_idx: unsigned;
    m_closed: boolean;
    constructor Construct;
    destructor Destruct; virtual;
    procedure remove_all; virtual;
    procedure move_to(x, y: int32); virtual;
    procedure line_to(x, y: int32); virtual;
    procedure curve3(x_ctrl, y_ctrl, x_to, y_to: int32); virtual;
    procedure curve4(x_ctrl1, y_ctrl1, x_ctrl2, y_ctrl2, x_to, y_to: int32); virtual;
    procedure close_polygon; virtual;
    function size: unsigned;
    function vertex_(idx: unsigned; x, y: int32_ptr): unsigned; virtual;
    function byte_size: unsigned;
    procedure serialize(ptr: int8u_ptr);
    procedure rewind(path_id: unsigned); virtual;
    function vertex(x, y: double_ptr): unsigned; virtual;
    function bounding_rect: rect_d;
  end;

  path_storage_int32_ptr = ^path_storage_int32;

  path_storage_int32 = object(path_storage_integer)
    m_storage: pod_deque;
    m_vertex_idx: unsigned;
    m_closed: boolean;
    constructor Construct;
    destructor Destruct; virtual;
    procedure remove_all; virtual;
    procedure move_to(x, y: int32); virtual;
    procedure line_to(x, y: int32); virtual;
    procedure curve3(x_ctrl, y_ctrl, x_to, y_to: int32); virtual;
    procedure curve4(x_ctrl1, y_ctrl1, x_ctrl2, y_ctrl2, x_to, y_to: int32); virtual;
    procedure close_polygon; virtual;
    function size: unsigned;
    function vertex_(idx: unsigned; x, y: int32_ptr): unsigned; virtual;
    function byte_size: unsigned;
    procedure serialize(ptr: int8u_ptr);
    procedure rewind(path_id: unsigned); virtual;
    function vertex(x, y: double_ptr): unsigned; virtual;
    function bounding_rect: rect_d;
  end;

  serialized_integer_path_adaptor_ptr = ^serialized_integer_path_adaptor;

  serialized_integer_path_adaptor = object(vertex_source)
    procedure init(Data: int8u_ptr; size: unsigned; dx, dy: double; scale: double = 1.0); virtual; abstract;
  end;

  serialized_int16_path_adaptor_ptr = ^serialized_int16_path_adaptor;

  serialized_int16_path_adaptor = object(serialized_integer_path_adaptor)
    m_data, m_end, m_ptr: int8u_ptr;
    m_dx, m_dy, m_scale: double;
    m_vertices: unsigned;
    constructor Construct; overload;
    constructor Construct(Data: int8u_ptr; size: unsigned; dx, dy: double); overload;
    procedure init(Data: int8u_ptr; size: unsigned; dx, dy: double; scale: double = 1.0); virtual;
    procedure rewind(path_id: unsigned); virtual;
    function vertex(x, y: double_ptr): unsigned; virtual;
  end;

  serialized_int32_path_adaptor_ptr = ^serialized_int32_path_adaptor;

  serialized_int32_path_adaptor = object(serialized_integer_path_adaptor)
    m_data, m_end, m_ptr: int8u_ptr;
    m_dx, m_dy, m_scale: double;
    m_vertices: unsigned;
    constructor Construct; overload;
    constructor Construct(Data: int8u_ptr; size: unsigned; dx, dy: double); overload;
    procedure init(Data: int8u_ptr; size: unsigned; dx, dy: double; scale: double = 1.0); virtual;
    procedure rewind(path_id: unsigned); virtual;
    function vertex(x, y: double_ptr): unsigned; virtual;
  end;


implementation

constructor vertex_int16.Construct;
begin
  x := 0;
  y := 0;

end;

{ CONSTRUCT }
constructor vertex_int16.Construct(x_, y_: int16; flag: unsigned);
begin
  x := ((x_ shl 1) and not 1) or (flag and 1);
  y := ((y_ shl 1) and not 1) or (flag shr 1);

end;

{ VERTEX }
function vertex_int16.vertex(x_, y_: double_ptr; dx: double = 0; dy: double = 0; scale: double = 1.0): unsigned;
begin
  x_^ := dx + (shr_int16(x, 1) / coord_scale) * scale;
  y_^ := dy + (shr_int16(y, 1) / coord_scale) * scale;

  case ((y and 1) shl 1) or (x and 1) of
    cmd_move_to:
      Result := path_cmd_move_to;

    cmd_line_to:
      Result := path_cmd_line_to;

    cmd_curve3:
      Result := path_cmd_curve3;

    cmd_curve4:
      Result := path_cmd_curve4;

    else
      Result := path_cmd_stop;

  end;

end;

{ CONSTRUCT }
constructor vertex_int32.Construct;
begin
  x := 0;
  y := 0;

end;

{ CONSTRUCT }
constructor vertex_int32.Construct(x_, y_: int32; flag: unsigned);
begin
  x := ((x_ shl 1) and not 1) or (flag and 1);
  y := ((y_ shl 1) and not 1) or (flag shr 1);

end;

{ VERTEX }
function vertex_int32.vertex(x_, y_: double_ptr; dx: double = 0; dy: double = 0; scale: double = 1.0): unsigned;
begin
  x_^ := dx + (shr_int32(x, 1) / coord_scale) * scale;
  y_^ := dy + (shr_int32(y, 1) / coord_scale) * scale;

  case ((y and 1) shl 1) or (x and 1) of
    cmd_move_to:
      Result := path_cmd_move_to;

    cmd_line_to:
      Result := path_cmd_line_to;

    cmd_curve3:
      Result := path_cmd_curve3;

    cmd_curve4:
      Result := path_cmd_curve4;

    else
      Result := path_cmd_stop;

  end;

end;

{ CONSTRUCT }
constructor path_storage_int16.Construct;
begin
  m_storage.Construct(sizeof(vertex_int16));

  m_vertex_idx := 0;
  m_closed := True;

end;

{ DESTRUCT }
destructor path_storage_int16.Destruct;
begin
  m_storage.Destruct;

end;

{ REMOVE_ALL }
procedure path_storage_int16.remove_all;
begin
  m_storage.remove_all;

end;

{ MOVE_TO }
procedure path_storage_int16.move_to(x, y: int32);
var
  v: vertex_int16;

begin
  v.Construct(int16(x), int16(y), cmd_move_to);
  m_storage.add(@v);

end;

{ LINE_TO }
procedure path_storage_int16.line_to(x, y: int32);
var
  v: vertex_int16;

begin
  v.Construct(int16(x), int16(y), cmd_line_to);
  m_storage.add(@v);

end;

{ CURVE3 }
procedure path_storage_int16.curve3(x_ctrl, y_ctrl, x_to, y_to: int32);
var
  v: vertex_int16;

begin
  v.Construct(int16(x_ctrl), int16(y_ctrl), cmd_curve3);
  m_storage.add(@v);

  v.Construct(int16(x_to), int16(y_to), cmd_curve3);
  m_storage.add(@v);

end;

{ CURVE4 }
procedure path_storage_int16.curve4(x_ctrl1, y_ctrl1, x_ctrl2, y_ctrl2, x_to, y_to: int32);
var
  v: vertex_int16;

begin
  v.Construct(int16(x_ctrl1), int16(y_ctrl1), cmd_curve4);
  m_storage.add(@v);

  v.Construct(int16(x_ctrl2), int16(y_ctrl2), int16(cmd_curve4));
  m_storage.add(@v);

  v.Construct(int16(x_to), int16(y_to), cmd_curve4);
  m_storage.add(@v);

end;

{ CLOSE_POLYGON }
procedure path_storage_int16.close_polygon;
begin
end;

{ SIZE }
function path_storage_int16.size: unsigned;
begin
  Result := m_storage.size;

end;

{ VERTEX_ }
function path_storage_int16.vertex_(idx: unsigned; x, y: int32_ptr): unsigned;
var
  v: vertex_int16_ptr;

begin
  v := m_storage.array_operator(idx);

  int16_ptr(x)^ := shr_int16(v^.x, 1);
  int16_ptr(y)^ := shr_int16(v^.y, 1);

  Result := ((v^.y and 1) shl 1) or (v^.x and 1);

end;

{ BYTE_SIZE }
function path_storage_int16.byte_size: unsigned;
begin
  Result := m_storage.size * sizeof(vertex_int16);

end;

{ SERIALIZE }
procedure path_storage_int16.serialize(ptr: int8u_ptr);
var
  i: unsigned;

begin
  i := 0;

  while i < m_storage.size do
  begin
    move(m_storage.array_operator(i)^, ptr^, sizeof(vertex_int16));

    Inc(ptrcomp(ptr), sizeof(vertex_int16));
    Inc(i);

  end;

end;

{ REWIND }
procedure path_storage_int16.rewind(path_id: unsigned);
begin
  m_vertex_idx := 0;
  m_closed := True;

end;

{ VERTEX }
function path_storage_int16.vertex(x, y: double_ptr): unsigned;
var
  cmd: unsigned;

begin
  if (m_storage.size < 2) or (m_vertex_idx > m_storage.size) then
  begin
    x^ := 0;
    y^ := 0;

    Result := path_cmd_stop;

    exit;

  end;

  if m_vertex_idx = m_storage.size then
  begin
    x^ := 0;
    y^ := 0;

    Inc(m_vertex_idx);

    Result := path_cmd_end_poly or path_flags_close;

    exit;

  end;

  cmd := vertex_int16_ptr(m_storage.array_operator(m_vertex_idx))^.vertex(x, y);

  if is_move_to(cmd) and not m_closed then
  begin
    x^ := 0;
    y^ := 0;

    m_closed := True;

    Result := path_cmd_end_poly or path_flags_close;

    exit;

  end;

  m_closed := False;

  Inc(m_vertex_idx);

  Result := cmd;

end;

{ BOUNDING_RECT }
function path_storage_int16.bounding_rect: rect_d;
var
  i: unsigned;

  x, y: double;

begin
  Result.Construct(1e100, 1e100, -1e100, -1e100);

  if m_storage.size = 0 then
    Result.Construct(0.0, 0.0, 0.0, 0.0)
  else
    for i := 0 to m_storage.size - 1 do
    begin
      vertex_int16_ptr(m_storage.array_operator(i))^.vertex(@x, @y);

      if x < Result.x1 then
        Result.x1 := x;

      if y < Result.y1 then
        Result.y1 := y;

      if x > Result.x2 then
        Result.x2 := x;

      if y > Result.y2 then
        Result.y2 := y;

    end;

end;

{ CONSTRUCT }
constructor path_storage_int32.Construct;
begin
  m_storage.Construct(sizeof(vertex_int32));

  m_vertex_idx := 0;
  m_closed := True;

end;

{ DESTRUCT }
destructor path_storage_int32.Destruct;
begin
  m_storage.Destruct;

end;

{ REMOVE_ALL }
procedure path_storage_int32.remove_all;
begin
  m_storage.remove_all;

end;

{ MOVE_TO }
procedure path_storage_int32.move_to(x, y: int32);
var
  v: vertex_int32;

begin
  v.Construct(x, y, cmd_move_to);
  m_storage.add(@v);

end;

{ LINE_TO }
procedure path_storage_int32.line_to(x, y: int32);
var
  v: vertex_int32;

begin
  v.Construct(x, y, cmd_line_to);
  m_storage.add(@v);

end;

{ CURVE3 }
procedure path_storage_int32.curve3(x_ctrl, y_ctrl, x_to, y_to: int32);
var
  v: vertex_int32;

begin
  v.Construct(x_ctrl, y_ctrl, cmd_curve3);
  m_storage.add(@v);

  v.Construct(x_to, y_to, cmd_curve3);
  m_storage.add(@v);

end;

{ CURVE4 }
procedure path_storage_int32.curve4(x_ctrl1, y_ctrl1, x_ctrl2, y_ctrl2, x_to, y_to: int32);
var
  v: vertex_int32;

begin
  v.Construct(x_ctrl1, y_ctrl1, cmd_curve4);
  m_storage.add(@v);

  v.Construct(x_ctrl2, y_ctrl2, cmd_curve4);
  m_storage.add(@v);

  v.Construct(x_to, y_to, cmd_curve4);
  m_storage.add(@v);

end;

{ CLOSE_POLYGON }
procedure path_storage_int32.close_polygon;
begin
end;

{ SIZE }
function path_storage_int32.size: unsigned;
begin
  Result := m_storage.size;

end;

{ VERTEX_ }
function path_storage_int32.vertex_(idx: unsigned; x, y: int32_ptr): unsigned;
var
  v: vertex_int32_ptr;

begin
  v := m_storage.array_operator(idx);

  x^ := shr_int32(v^.x, 1);
  y^ := shr_int32(v^.y, 1);

  Result := ((v^.y and 1) shl 1) or (v^.x and 1);

end;

{ BYTE_SIZE }
function path_storage_int32.byte_size: unsigned;
begin
  Result := m_storage.size * sizeof(vertex_int32);

end;

{ SERIALIZE }
procedure path_storage_int32.serialize(ptr: int8u_ptr);
var
  i: unsigned;

begin
  i := 0;

  while i < m_storage.size do
  begin
    move(m_storage.array_operator(i)^, ptr^, sizeof(vertex_int32));

    Inc(ptrcomp(ptr), sizeof(vertex_int32));
    Inc(i);

  end;

end;

{ REWIND }
procedure path_storage_int32.rewind(path_id: unsigned);
begin
  m_vertex_idx := 0;
  m_closed := True;

end;

{ VERTEX }
function path_storage_int32.vertex(x, y: double_ptr): unsigned;
var
  cmd: unsigned;

begin
  if (m_storage.size < 2) or (m_vertex_idx > m_storage.size) then
  begin
    x^ := 0;
    y^ := 0;

    Result := path_cmd_stop;

    exit;

  end;

  if m_vertex_idx = m_storage.size then
  begin
    x^ := 0;
    y^ := 0;

    Inc(m_vertex_idx);

    Result := path_cmd_end_poly or path_flags_close;

    exit;

  end;

  cmd := vertex_int32_ptr(m_storage.array_operator(m_vertex_idx))^.vertex(x, y);

  if is_move_to(cmd) and not m_closed then
  begin
    x^ := 0;
    y^ := 0;

    m_closed := True;

    Result := path_cmd_end_poly or path_flags_close;

    exit;

  end;

  m_closed := False;

  Inc(m_vertex_idx);

  Result := cmd;

end;

{ BOUNDING_RECT }
function path_storage_int32.bounding_rect: rect_d;
var
  i: unsigned;

  x, y: double;

begin
  Result.Construct(1e100, 1e100, -1e100, -1e100);

  if m_storage.size = 0 then
    Result.Construct(0.0, 0.0, 0.0, 0.0)
  else
    for i := 0 to m_storage.size - 1 do
    begin
      vertex_int32_ptr(m_storage.array_operator(i))^.vertex(@x, @y);

      if x < Result.x1 then
        Result.x1 := x;

      if y < Result.y1 then
        Result.y1 := y;

      if x > Result.x2 then
        Result.x2 := x;

      if y > Result.y2 then
        Result.y2 := y;

    end;

end;

{ CONSTRUCT }
constructor serialized_int16_path_adaptor.Construct;
begin
  m_data := nil;
  m_end := nil;
  m_ptr := nil;

  m_dx := 0.0;
  m_dy := 0.0;

  m_scale := 1.0;
  m_vertices := 0;

end;

{ CONSTRUCT }
constructor serialized_int16_path_adaptor.Construct(Data: int8u_ptr; size: unsigned; dx, dy: double);
begin
  m_data := Data;
  m_end := int8u_ptr(ptrcomp(Data) + size);
  m_ptr := Data;

  m_dx := dx;
  m_dy := dy;

  m_scale := 0.0;
  m_vertices := 0;

end;

{ INIT }
procedure serialized_int16_path_adaptor.init(Data: int8u_ptr; size: unsigned; dx, dy: double; scale: double = 1.0);
begin
  m_data := Data;
  m_end := int8u_ptr(ptrcomp(Data) + size);
  m_ptr := Data;

  m_dx := dx;
  m_dy := dy;

  m_scale := scale;
  m_vertices := 0;

end;

{ REWIND }
procedure serialized_int16_path_adaptor.rewind(path_id: unsigned);
begin
  m_ptr := m_data;

  m_vertices := 0;

end;

{ VERTEX }
function serialized_int16_path_adaptor.vertex(x, y: double_ptr): unsigned;
var
  v: vertex_int16;

  cmd: unsigned;

begin
  if (m_data = nil) or (ptrcomp(m_ptr) > ptrcomp(m_end)) then
  begin
    x^ := 0;
    y^ := 0;

    Result := path_cmd_stop;

    exit;

  end;

  if ptrcomp(m_ptr) = ptrcomp(m_end) then
  begin
    x^ := 0;
    y^ := 0;

    Inc(ptrcomp(m_ptr), sizeof(vertex_int16));

    Result := path_cmd_end_poly or path_flags_close;

    exit;

  end;

  move(m_ptr^, v, sizeof(vertex_int16));

  cmd := v.vertex(x, y, m_dx, m_dy, m_scale);

  if is_move_to(cmd) and (m_vertices > 2) then
  begin
    x^ := 0;
    y^ := 0;

    m_vertices := 0;

    Result := path_cmd_end_poly or path_flags_close;

    exit;

  end;

  Inc(m_vertices);
  Inc(ptrcomp(m_ptr), sizeof(vertex_int16));

  Result := cmd;

end;

{ CONSTRUCT }
constructor serialized_int32_path_adaptor.Construct;
begin
  m_data := nil;
  m_end := nil;
  m_ptr := nil;

  m_dx := 0.0;
  m_dy := 0.0;

  m_scale := 1.0;
  m_vertices := 0;

end;

{ CONSTRUCT }
constructor serialized_int32_path_adaptor.Construct(Data: int8u_ptr; size: unsigned; dx, dy: double);
begin
  m_data := Data;
  m_end := int8u_ptr(ptrcomp(Data) + size);
  m_ptr := Data;

  m_dx := dx;
  m_dy := dy;

  m_scale := 0.0;
  m_vertices := 0;

end;

{ INIT }
procedure serialized_int32_path_adaptor.init(Data: int8u_ptr; size: unsigned; dx, dy: double; scale: double = 1.0);
begin
  m_data := Data;
  m_end := int8u_ptr(ptrcomp(Data) + size);
  m_ptr := Data;

  m_dx := dx;
  m_dy := dy;

  m_scale := scale;
  m_vertices := 0;

end;

{ REWIND }
procedure serialized_int32_path_adaptor.rewind(path_id: unsigned);
begin
  m_ptr := m_data;

  m_vertices := 0;

end;

{ VERTEX }
function serialized_int32_path_adaptor.vertex(x, y: double_ptr): unsigned;
var
  v: vertex_int32;

  cmd: unsigned;

begin
  if (m_data = nil) or (ptrcomp(m_ptr) > ptrcomp(m_end)) then
  begin
    x^ := 0;
    y^ := 0;

    Result := path_cmd_stop;

    exit;

  end;

  if ptrcomp(m_ptr) = ptrcomp(m_end) then
  begin
    x^ := 0;
    y^ := 0;

    Inc(ptrcomp(m_ptr), sizeof(vertex_int32));

    Result := path_cmd_end_poly or path_flags_close;

    exit;

  end;

  move(m_ptr^, v, sizeof(vertex_int32));

  cmd := v.vertex(x, y, m_dx, m_dy, m_scale);

  if is_move_to(cmd) and (m_vertices > 2) then
  begin
    x^ := 0;
    y^ := 0;

    m_vertices := 0;

    Result := path_cmd_end_poly or path_flags_close;

    exit;

  end;

  Inc(m_vertices);
  Inc(ptrcomp(m_ptr), sizeof(vertex_int32));

  Result := cmd;

end;

end.









