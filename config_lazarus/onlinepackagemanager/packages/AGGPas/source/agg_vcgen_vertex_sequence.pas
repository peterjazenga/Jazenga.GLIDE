
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}
unit
agg_vcgen_vertex_sequence;

interface

{$I agg_mode.inc }

uses
  agg_basics,
  agg_vertex_sequence,
  agg_shorten_path,
  agg_vertex_source;

type
  vcgen_vertex_sequence_ptr = ^vcgen_vertex_sequence;

  vcgen_vertex_sequence = object(vertex_source)
    m_src_vertices: vertex_sequence;
    m_flags, m_cur_vertex: unsigned;

    m_shorten: double;
    m_ready: boolean;

    constructor Construct;
    destructor Destruct; virtual;

    procedure shorten_(s: double);
    function _shorten: double;

    // Vertex Generator Interface
    procedure remove_all; virtual;
    procedure add_vertex(x, y: double; cmd: unsigned); virtual;

    // Vertex Source Interface
    procedure rewind(path_id: unsigned); virtual;
    function vertex(x, y: double_ptr): unsigned; virtual;

  end;

implementation

constructor vcgen_vertex_sequence.Construct;
begin
  m_src_vertices.Construct(sizeof(vertex_dist_cmd), 6);

  m_flags := 0;
  m_cur_vertex := 0;
  m_shorten := 0.0;
  m_ready := False;

end;

{ DESTRUCT }
destructor vcgen_vertex_sequence.Destruct;
begin
  m_src_vertices.Destruct;

end;

{ SHORTEN_ }
procedure vcgen_vertex_sequence.shorten_(s: double);
begin
  m_shorten := s;

end;

{ _SHORTEN }
function vcgen_vertex_sequence._shorten: double;
begin
  Result := m_shorten;

end;

{ REMOVE_ALL }
procedure vcgen_vertex_sequence.remove_all;
begin
  m_ready := False;

  m_src_vertices.remove_all;

  m_cur_vertex := 0;
  m_flags := 0;

end;

{ ADD_VERTEX }
procedure vcgen_vertex_sequence.add_vertex(x, y: double; cmd: unsigned);
var
  vc: vertex_dist_cmd;

begin
  m_ready := False;

  vc.x := x;
  vc.y := y;

  vc.dist := 0;
  vc.cmd := cmd;

  if is_move_to(cmd) then
    m_src_vertices.modify_last(@vc)
  else
  if is_vertex(cmd) then
    m_src_vertices.add(@vc)
  else
    m_flags := cmd and path_flags_mask;

end;

{ REWIND }
procedure vcgen_vertex_sequence.rewind(path_id: unsigned);
begin
  if not m_ready then
  begin
    m_src_vertices.Close(is_closed(m_flags));

    shorten_path(@m_src_vertices, m_shorten, get_close_flag(m_flags));

  end;

  m_ready := True;
  m_cur_vertex := 0;

end;

{ VERTEX }
function vcgen_vertex_sequence.vertex(x, y: double_ptr): unsigned;
var
  v: vertex_dist_cmd_ptr;

begin
  if not m_ready then
    rewind(0);

  if m_cur_vertex = m_src_vertices.size then
  begin
    Inc(m_cur_vertex);

    Result := path_cmd_end_poly or m_flags;

    exit;

  end;

  if m_cur_vertex > m_src_vertices.size then
  begin
    Result := path_cmd_stop;

    exit;

  end;

  v := m_src_vertices.array_operator(m_cur_vertex);

  Inc(m_cur_vertex);

  x^ := v^.x;
  y^ := v^.y;

  Result := v^.cmd;

end;

end.

