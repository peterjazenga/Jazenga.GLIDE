
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}
unit
agg_vcgen_contour;

interface

{$I agg_mode.inc }

uses
  agg_basics,
  agg_array,
  agg_math,
  agg_math_stroke,
  agg_vertex_source,
  agg_vertex_sequence;

{ TYPES DEFINITION }
type
  status_e = (initial, ready, outline, out_vertices, end_poly, stop);

  vcgen_contour_ptr = ^vcgen_contour;

  vcgen_contour = object(vertex_source)
    m_src_vertices: vertex_sequence;
    m_out_vertices: pod_deque;

    m_width: double;

    m_line_join, m_inner_join: unsigned;
    m_approx_scale, m_abs_width, m_signed_width, m_miter_limit, m_inner_miter_limit: double;

    m_status: status_e;

    m_src_vertex, m_out_vertex, m_closed, m_orientation: unsigned;
    m_auto_detect: boolean;

    constructor Construct;
    destructor Destruct; virtual;

    procedure line_join_(lj: unsigned);
    procedure inner_join_(ij: unsigned);

    procedure width_(w: double);

    procedure miter_limit_(ml: double);
    procedure miter_limit_theta_(t: double);
    procedure inner_miter_limit_(ml: double);
    procedure approximation_scale_(_as_: double);

    procedure auto_detect_orientation_(v: boolean);

    function _line_join: unsigned;
    function _inner_join: unsigned;
    function _width: double;
    function _miter_limit: double;
    function _inner_miter_limit: double;
    function _approximation_scale: double;
    function _auto_detect_orientation: boolean;

    // Generator interface
    procedure remove_all; virtual;
    procedure add_vertex(x, y: double; cmd: unsigned); virtual;

    // Vertex Source Interface
    procedure rewind(path_id: unsigned); virtual;
    function vertex(x, y: double_ptr): unsigned; virtual;

  end;

implementation

constructor vcgen_contour.Construct;
begin
  m_src_vertices.Construct(sizeof(vertex_dist));
  m_out_vertices.Construct(sizeof(point_type));

  m_width := 1.0;

  m_line_join := bevel_join;
  m_inner_join := inner_miter;
  m_approx_scale := 1.0;
  m_abs_width := 1.0;
  m_signed_width := 1.0;
  m_miter_limit := 4.0;

  m_inner_miter_limit := 1.0 + 1.0 / 64.0;

  m_status := initial;
  m_src_vertex := 0;
  m_closed := 0;
  m_orientation := 0;
  m_auto_detect := False;

end;

{ DESTRUCT }
destructor vcgen_contour.Destruct;
begin
  m_src_vertices.Destruct;
  m_out_vertices.Destruct;

end;

{ LINE_JOIN_ }
procedure vcgen_contour.line_join_(lj: unsigned);
begin
  m_line_join := lj;

end;

{ INNER_JOIN_ }
procedure vcgen_contour.inner_join_(ij: unsigned);
begin
  m_inner_join := ij;

end;

{ WIDTH_ }
procedure vcgen_contour.width_(w: double);
begin
  m_width := w * 0.5;

end;

{ MITER_LIMIT_ }
procedure vcgen_contour.miter_limit_(ml: double);
begin
  m_miter_limit := ml;

end;

{ MITER_LIMIT_THETA_ }
procedure vcgen_contour.miter_limit_theta_(t: double);
begin
  m_miter_limit := 1.0 / Sin(t * 0.5);

end;

{ INNER_MITER_LIMIT_ }
procedure vcgen_contour.inner_miter_limit_(ml: double);
begin
  m_inner_miter_limit := ml;

end;

{ APPROXIMATION_SCALE_ }
procedure vcgen_contour.approximation_scale_(_as_: double);
begin
  m_approx_scale := _as_;

end;

{ AUTO_DETECT_ORIENTATION_ }
procedure vcgen_contour.auto_detect_orientation_(v: boolean);
begin
  m_auto_detect := v;

end;

{ _LINE_JOIN }
function vcgen_contour._line_join: unsigned;
begin
  Result := m_line_join;

end;

{ _INNER_JOIN }
function vcgen_contour._inner_join: unsigned;
begin
  Result := m_inner_join;

end;

{ _WIDTH }
function vcgen_contour._width: double;
begin
  Result := m_width * 2.0;

end;

{ _MITER_LIMIT }
function vcgen_contour._miter_limit: double;
begin
  Result := m_miter_limit;

end;

{ _INNER_MITER_LIMIT }
function vcgen_contour._inner_miter_limit: double;
begin
  Result := m_inner_miter_limit;

end;

{ _APPROXIMATION_SCALE }
function vcgen_contour._approximation_scale: double;
begin
  Result := m_approx_scale;

end;

{ _AUTO_DETECT_ORIENTATION }
function vcgen_contour._auto_detect_orientation: boolean;
begin
  Result := m_auto_detect;

end;

{ REMOVE_ALL }
procedure vcgen_contour.remove_all;
begin
  m_src_vertices.remove_all;

  m_closed := 0;
  m_orientation := 0;
  m_abs_width := Abs(m_width);
  m_signed_width := m_width;
  m_status := initial;

end;

{ ADD_VERTEX }
procedure vcgen_contour.add_vertex(x, y: double; cmd: unsigned);
var
  vd: vertex_dist;

begin
  m_status := initial;

  vd.x := x;
  vd.y := y;

  vd.dist := 0;

  if is_move_to(cmd) then
    m_src_vertices.modify_last(@vd)
  else
  if is_vertex(cmd) then
    m_src_vertices.add(@vd)
  else
  if is_end_poly(cmd) then
  begin
    m_closed := get_close_flag(cmd);

    if m_orientation = path_flags_none then
      m_orientation := get_orientation(cmd);

  end;

end;

{ REWIND }
procedure vcgen_contour.rewind(path_id: unsigned);
begin
  if m_status = initial then
  begin
    m_src_vertices.Close(True);

    m_signed_width := m_width;

    if m_auto_detect then
      if not is_oriented(m_orientation) then
        if calc_polygon_area_vs(@m_src_vertices) > 0.0 then
          m_orientation := path_flags_ccw
        else
          m_orientation := path_flags_cw;

    if is_oriented(m_orientation) then
      if is_ccw(m_orientation) then
        m_signed_width := m_width
      else
        m_signed_width := -m_width;

  end;

  m_status := ready;
  m_src_vertex := 0;

end;

{ VERTEX }
function vcgen_contour.vertex(x, y: double_ptr): unsigned;
var
  cmd: unsigned;

  c: point_type_ptr;

label
  _next, _ready, _outline, _out_vertices;

begin
  cmd := path_cmd_line_to;

  _next:
    while not is_stop(cmd) do
      case m_status of
        initial:
        begin
          rewind(0);

          goto _ready;

        end;

        ready:
        _ready:
        begin
          if m_src_vertices.size < 2 + unsigned(m_closed <> 0) then
          begin
            cmd := path_cmd_stop;

            goto _next;

          end;

          m_status := outline;

          cmd := path_cmd_move_to;

          m_src_vertex := 0;
          m_out_vertex := 0;

          goto _outline;

        end;

        outline:
        _outline:
        begin
          if m_src_vertex >= m_src_vertices.size then
          begin
            m_status := end_poly;

            goto _next;

          end;

          stroke_calc_join(@m_out_vertices,
          m_src_vertices.prev(m_src_vertex),
          m_src_vertices.curr(m_src_vertex),
          m_src_vertices.Next(m_src_vertex),
          vertex_dist_ptr(m_src_vertices.prev(m_src_vertex))^.dist,
          vertex_dist_ptr(m_src_vertices.curr(m_src_vertex))^.dist,
          m_signed_width,
          m_line_join,
          m_inner_join,
          m_miter_limit,
          m_inner_miter_limit,
          m_approx_scale);

          Inc(m_src_vertex);

          m_status := out_vertices;
          m_out_vertex := 0;

          goto _out_vertices;

        end;

        out_vertices:
        _out_vertices:
          if m_out_vertex >= m_out_vertices.size then
            m_status := outline
          else
          begin
            c := m_out_vertices.array_operator(m_out_vertex);

            Inc(m_out_vertex);

            x^ := c^.x;
            y^ := c^.y;

            Result := cmd;

            exit;

          end;

        end_poly:
        begin
          if m_closed = 0 then
          begin
            Result := path_cmd_stop;

            exit;

          end;

          m_status := stop;
          Result := path_cmd_end_poly or path_flags_close or path_flags_ccw;

          exit;

        end;

        stop:
        begin
          Result := path_cmd_stop;

          exit;

        end;

      end;

  Result := cmd;

end;

end.

