
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}
unit
agg_vcgen_markers_term;

interface

{$I agg_mode.inc }

uses
  agg_basics,
  agg_array,
  agg_vertex_source,
  agg_vertex_sequence;

type
  coord_type_ptr = ^coord_type;

  coord_type = object
    x, y: double;

    constructor Construct; overload;
    constructor Construct(x_, y_: double); overload;

  end;

  vcgen_markers_term = object(vertex_source)
    m_markers: pod_deque;
    m_curr_id, m_curr_idx: unsigned;

    constructor Construct;
    destructor Destruct; virtual;

    // Vertex Generator Interface
    procedure remove_all; virtual;
    procedure add_vertex(x, y: double; cmd: unsigned); virtual;

    // Vertex Source Interface
    procedure rewind(path_id: unsigned); virtual;
    function vertex(x, y: double_ptr): unsigned; virtual;

  end;


implementation

constructor coord_type.Construct;
begin
  x := 0;
  y := 0;

end;

{ CONSTRUCT }
constructor coord_type.Construct(x_, y_: double);
begin
  x := x_;
  y := y_;

end;

{ CONSTRUCT }
constructor vcgen_markers_term.Construct;
begin
  m_markers.Construct(sizeof(coord_type), 6);

  m_curr_id := 0;
  m_curr_idx := 0;

end;

{ DESTRUCT }
destructor vcgen_markers_term.Destruct;
begin
  m_markers.Destruct;

end;

{ REMOVE_ALL }
procedure vcgen_markers_term.remove_all;
begin
  m_markers.remove_all;

end;

{ ADD_VERTEX }
procedure vcgen_markers_term.add_vertex(x, y: double; cmd: unsigned);
var
  ct: coord_type;

begin
  if is_move_to(cmd) then
    if m_markers.size and 1 <> 0 then
    begin
      // Initial state, the first coordinate was added.
      // If two of more calls of start_vertex() occures
      // we just modify the last one.
      ct.Construct(x, y);
      m_markers.modify_last(@ct);

    end
    else
    begin
      ct.Construct(x, y);
      m_markers.add(@ct);

    end
  else
  if is_vertex(cmd) then
    if m_markers.size and 1 <> 0 then
    begin
      // Initial state, the first coordinate was added.
      // Add three more points, 0,1,1,0
      ct.Construct(x, y);
      m_markers.add(@ct);
      m_markers.add(m_markers.array_operator(m_markers.size - 1));
      m_markers.add(m_markers.array_operator(m_markers.size - 3));

    end
    else
    if m_markers.size <> 0 then
    begin
      // Replace two last points: 0,1,1,0 -> 0,1,2,1
      ct.Construct(x, y);

      move(
        m_markers.array_operator(m_markers.size - 2)^,
        m_markers.array_operator(m_markers.size - 1)^,
        sizeof(coord_type));

      move(
        ct,
        m_markers.array_operator(m_markers.size - 2)^,
        sizeof(coord_type));

    end;

end;

{ REWIND }
procedure vcgen_markers_term.rewind(path_id: unsigned);
begin
  m_curr_id := path_id * 2;
  m_curr_idx := m_curr_id;

end;

{ VERTEX }
function vcgen_markers_term.vertex(x, y: double_ptr): unsigned;
var
  c: coord_type_ptr;

begin
  if (m_curr_id > 2) or (m_curr_idx >= m_markers.size) then
  begin
    Result := path_cmd_stop;

    exit;

  end;

  c := m_markers.array_operator(m_curr_idx);

  x^ := c^.x;
  y^ := c^.y;

  if m_curr_idx and 1 <> 0 then
  begin
    Inc(m_curr_idx, 3);

    Result := path_cmd_line_to;

    exit;

  end;

  Inc(m_curr_idx);

  Result := path_cmd_move_to;

end;

end.



