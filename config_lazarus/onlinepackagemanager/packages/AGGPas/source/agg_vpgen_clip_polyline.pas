
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}
unit
agg_vpgen_clip_polyline;

interface

{$I agg_mode.inc }

uses
  agg_basics,
  agg_vertex_source;

{ TYPES DEFINITION }
const
  clip_x1 = 1;
  clip_x2 = 2;
  clip_y1 = 4;
  clip_y2 = 8;

type
  vpgen_clip_polyline_ptr = ^vpgen_clip_polyline;

  vpgen_clip_polyline = object(vertex_source)
    m_clip_box: rect_d;

    m_x1, m_y1: double;
    m_f1: unsigned;
    m_x2, m_y2: double;
    m_f2: unsigned;

    m_x, m_y: array[0..1] of double;
    m_cmd: array[0..1] of unsigned;

    m_num_vertices, m_vertex: unsigned;

    constructor Construct;

    procedure clip_box_(x1, y1, x2, y2: double);

    function _x1: double;
    function _y1: double;
    function _x2: double;
    function _y2: double;

    function _auto_close: boolean;
    function _auto_unclose: boolean;

    procedure reset;
    procedure move_to(x, y: double);
    procedure line_to(x, y: double);

    function vertex(x, y: double_ptr): unsigned; virtual;

    // Determine the clipping code of the vertex according to the
    // Cyrus-Beck line clipping algorithm
    function clipping_flags_x(x: double): unsigned;
    function clipping_flags_y(y: double): unsigned;
    function clipping_flags(x, y: double): unsigned;

    function move_point(x, y: double_ptr; flags: unsigned_ptr): boolean;

    procedure clip_line_segment;

  end;


implementation

const
  clip_epsilon = 1e-10;

constructor vpgen_clip_polyline.Construct;
begin
  m_clip_box.Construct(0, 0, 1, 1);

  m_x1 := 0;
  m_y1 := 0;
  m_f1 := 0;
  m_x2 := 0;
  m_y2 := 0;
  m_f2 := 0;

  m_num_vertices := 0;
  m_vertex := 0;

end;

{ CLIP_BOX_ }
procedure vpgen_clip_polyline.clip_box_(x1, y1, x2, y2: double);
begin
  m_clip_box.x1 := x1;
  m_clip_box.y1 := y1;
  m_clip_box.x2 := x2;
  m_clip_box.y2 := y2;

  m_clip_box.normalize;

end;

{ _X1 }
function vpgen_clip_polyline._x1: double;
begin
  Result := m_clip_box.x1;

end;

{ _Y1 }
function vpgen_clip_polyline._y1: double;
begin
  Result := m_clip_box.y1;

end;

{ _X2 }
function vpgen_clip_polyline._x2: double;
begin
  Result := m_clip_box.x2;

end;

{ _Y2 }
function vpgen_clip_polyline._y2: double;
begin
  Result := m_clip_box.y2;

end;

{ _AUTO_CLOSE }
function vpgen_clip_polyline._auto_close: boolean;
begin
  Result := False;

end;

{ _AUTO_UNCLOSE }
function vpgen_clip_polyline._auto_unclose: boolean;
begin
  Result := True;

end;

{ RESET }
procedure vpgen_clip_polyline.reset;
begin
  m_vertex := 0;
  m_num_vertices := 0;

end;

{ MOVE_TO }
procedure vpgen_clip_polyline.move_to(x, y: double);
begin
  m_vertex := 0;
  m_num_vertices := 0;

  m_f1 := clipping_flags(x, y);

  if m_f1 = 0 then
  begin
    m_x[0] := x;
    m_y[0] := y;

    m_cmd[0] := path_cmd_move_to;

    m_num_vertices := 1;

  end;

  m_x1 := x;
  m_y1 := y;

end;

{ LINE_TO }
procedure vpgen_clip_polyline.line_to(x, y: double);
var
  f: unsigned;

begin
  m_vertex := 0;
  m_num_vertices := 0;

  m_x2 := x;
  m_y2 := y;

  f := clipping_flags(m_x2, m_y2);
  m_f2 := f;

  if m_f2 = m_f1 then
    if m_f2 = 0 then
    begin
      m_x[0] := x;
      m_y[0] := y;

      m_cmd[0] := path_cmd_line_to;

      m_num_vertices := 1;

    end
    else
  else
    clip_line_segment;

  m_f1 := f;
  m_x1 := x;
  m_y1 := y;

end;

{ VERTEX }
function vpgen_clip_polyline.vertex(x, y: double_ptr): unsigned;
begin
  if m_vertex < m_num_vertices then
  begin
    x^ := m_x[m_vertex];
    y^ := m_y[m_vertex];

    Result := m_cmd[m_vertex];

    Inc(m_vertex);

  end
  else
    Result := path_cmd_stop;

end;

{ CLIPPING_FLAGS_X }
function vpgen_clip_polyline.clipping_flags_x(x: double): unsigned;
var
  f: unsigned;

begin
  f := 0;

  if x < m_clip_box.x1 then
    f := f or clip_x1;

  if x > m_clip_box.x2 then
    f := f or clip_x2;

  Result := f;

end;

{ CLIPPING_FLAGS_Y }
function vpgen_clip_polyline.clipping_flags_y(y: double): unsigned;
var
  f: unsigned;

begin
  f := 0;

  if y < m_clip_box.y1 then
    f := f or clip_y1;

  if y > m_clip_box.y2 then
    f := f or clip_y2;

  Result := f;

end;

{ CLIPPING_FLAGS }
function vpgen_clip_polyline.clipping_flags(x, y: double): unsigned;
begin
  Result := clipping_flags_x(x) or clipping_flags_y(y);

end;

{ MOVE_POINT }
function vpgen_clip_polyline.move_point(x, y: double_ptr; flags: unsigned_ptr): boolean;
var
  bound: double;

begin
  if flags^ and (clip_x1 or clip_x2) <> 0 then
  begin
    if flags^ and clip_x1 <> 0 then
      bound := m_clip_box.x1
    else
      bound := m_clip_box.x2;

    y^ := (bound - m_x1) * (m_y2 - m_y1) / (m_x2 - m_x1) + m_y1;
    x^ := bound;

    flags^ := clipping_flags_y(y^);

  end;

  if (Abs(m_y2 - m_y1) < clip_epsilon) and (Abs(m_x2 - m_x1) < clip_epsilon) then
  begin
    Result := False;

    exit;

  end;

  if flags^ and (clip_y1 or clip_y2) <> 0 then
  begin
    if flags^ and clip_y1 <> 0 then
      bound := m_clip_box.y1
    else
      bound := m_clip_box.y2;

    x^ := (bound - m_y1) * (m_x2 - m_x1) / (m_y2 - m_y1) + m_x1;
    y^ := bound;

  end;

  flags^ := 0;
  Result := True;

end;

{ CLIP_LINE_SEGMENT }
procedure vpgen_clip_polyline.clip_line_segment;
begin
  if (m_f1 and m_f2) = 0 then
  begin
    if m_f1 <> 0 then
    begin
      if not move_point(@m_x1, @m_y1, @m_f1) then
        exit;

      if m_f1 <> 0 then
        exit;

      m_x[0] := m_x1;
      m_y[0] := m_y1;

      m_cmd[0] := path_cmd_move_to;

      m_num_vertices := 1;

    end;

    if m_f2 <> 0 then // Move Point 2
      if not move_point(@m_x2, @m_y2, @m_f2) then
        exit;

    m_x[m_num_vertices] := m_x2;
    m_y[m_num_vertices] := m_y2;

    m_cmd[m_num_vertices] := path_cmd_line_to;

    Inc(m_num_vertices);

  end;

end;

end.





