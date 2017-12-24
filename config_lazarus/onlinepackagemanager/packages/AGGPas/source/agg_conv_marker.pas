
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}
unit
agg_conv_marker;

interface

{$I agg_mode.inc }

uses
  Math,
  agg_basics,
  agg_trans_affine,
  agg_vertex_source;

type
  status_e = (initial, markers, polygon, stop);

  conv_marker = object(vertex_source)
    m_marker_locator, m_marker_shapes: vertex_source_ptr;
    m_transform, m_mtx: trans_affine;
    m_status: status_e;
    m_marker, m_num_markers: unsigned;
    constructor Construct(ml, ms: vertex_source_ptr);
    function _transform: trans_affine_ptr;
    procedure rewind(path_id: unsigned); virtual;
    function vertex(x, y: double_ptr): unsigned; virtual;
  end;

implementation

constructor conv_marker.Construct(ml, ms: vertex_source_ptr);
begin
  m_transform.Construct;
  m_mtx.Construct;

  m_marker_locator := ml;
  m_marker_shapes := ms;

  m_status := initial;
  m_marker := 0;

  m_num_markers := 1;

end;

{ _TRANSFORM }
function conv_marker._transform: trans_affine_ptr;
begin
  Result := @m_transform;

end;

{ REWIND }
procedure conv_marker.rewind(path_id: unsigned);
begin
  m_status := initial;
  m_marker := 0;

  m_num_markers := 1;

end;

{ VERTEX }
function conv_marker.vertex(x, y: double_ptr): unsigned;
var
  cmd: unsigned;

  x1, y1, x2, y2: double;

  tar: trans_affine_rotation;
  tat: trans_affine_translation;

label
  _next, _markers, _polygon, _stop;

begin
  cmd := path_cmd_move_to;

  _next:
    while not is_stop(cmd) do
      case m_status of
        initial:
        begin
          if m_num_markers = 0 then
          begin
            cmd := path_cmd_stop;

            goto _next;

          end;

          m_marker_locator^.rewind(m_marker);

          Inc(m_marker);

          m_num_markers := 0;
          m_status := markers;

          goto _markers;

        end;

        markers:
        _markers:
        begin
          if is_stop(m_marker_locator^.vertex(@x1, @y1)) then
          begin
            m_status := initial;

            goto _next;

          end;

          if is_stop(m_marker_locator^.vertex(@x2, @y2)) then
          begin
            m_status := initial;

            goto _next;

          end;

          Inc(m_num_markers);

          m_mtx := m_transform;

          tar.Construct(ArcTan2(y2 - y1, x2 - x1));
          tat.Construct(x1, y1);

          m_mtx.multiply(@tar);
          m_mtx.multiply(@tat);

          m_marker_shapes^.rewind(m_marker - 1);

          m_status := polygon;

          goto _polygon;

        end;

        polygon:
        _polygon:
        begin
          cmd := m_marker_shapes^.vertex(x, y);

          if is_stop(cmd) then
          begin
            cmd := path_cmd_move_to;
            m_status := markers;

            goto _next;

          end;

          m_mtx.transform(@m_mtx, x, y);

          Result := cmd;

          exit;

        end;

        stop:
        begin
          cmd := path_cmd_stop;

          goto _next;

        end;

      end;

  Result := cmd;

end;

end.




