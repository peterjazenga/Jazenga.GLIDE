
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit
agg_conv_adaptor_vpgen;

interface

{$I agg_mode.inc }

uses
  agg_basics,
  agg_vertex_source,
  agg_vpgen_segmentator;

{ TYPES DEFINITION }
type
  conv_adaptor_vpgen = object(vertex_source)
    m_source, m_vpgen: vertex_source_ptr;
    m_start_x, m_start_y: double;

    m_poly_flags: unsigned;
    m_vertices: int;

    constructor Construct(Source, gen: vertex_source_ptr);

    procedure set_source(Source: vertex_source_ptr);

    function vpgen: vpgen_segmentator_ptr;

    procedure rewind(path_id: unsigned); virtual;
    function vertex(x, y: double_ptr): unsigned; virtual;

  end;

implementation

constructor conv_adaptor_vpgen.Construct(Source, gen: vertex_source_ptr);
begin
  m_source := Source;
  m_vpgen := gen;
  m_start_x := 0;
  m_start_y := 0;

  m_poly_flags := 0;
  m_vertices := 0;

end;

{ SET_SOURCE }
procedure conv_adaptor_vpgen.set_source(Source: vertex_source_ptr);
begin
  m_source := Source;

end;

{ VPGEN }
function conv_adaptor_vpgen.vpgen: vpgen_segmentator_ptr;
begin
  Result := vpgen_segmentator_ptr(m_vpgen);

end;

{ REWIND }
procedure conv_adaptor_vpgen.rewind(path_id: unsigned);
begin
  m_source^.rewind(path_id);

  vpgen_segmentator_ptr(m_vpgen)^.reset;

  m_start_x := 0;
  m_start_y := 0;
  m_poly_flags := 0;
  m_vertices := 0;

end;

{ VERTEX }
function conv_adaptor_vpgen.vertex(x, y: double_ptr): unsigned;
var
  cmd: unsigned;

  tx, ty: double;

begin
  cmd := path_cmd_stop;

  repeat
    cmd := m_vpgen^.vertex(x, y);

    if not is_stop(cmd) then
      break;

    if (m_poly_flags <> 0) and not vpgen_segmentator_ptr(m_vpgen)^._auto_unclose then
    begin
      x^ := 0.0;
      y^ := 0.0;
      cmd := m_poly_flags;

      m_poly_flags := 0;

      break;

    end;

    if m_vertices < 0 then
    begin
      if m_vertices < -1 then
      begin
        m_vertices := 0;

        Result := path_cmd_stop;

        exit;

      end;

      vpgen_segmentator_ptr(m_vpgen)^.move_to(m_start_x, m_start_y);

      m_vertices := 1;

      continue;

    end;

    cmd := m_source^.vertex(@tx, @ty);

    if is_vertex(cmd) then
      if is_move_to(cmd) then
      begin
        if vpgen_segmentator_ptr(m_vpgen)^._auto_close and (m_vertices > 2) then
        begin
          vpgen_segmentator_ptr(m_vpgen)^.line_to(m_start_x, m_start_y);

          m_poly_flags := path_cmd_end_poly or path_flags_close;
          m_start_x := tx;
          m_start_y := ty;
          m_vertices := -1;

          continue;

        end;

        vpgen_segmentator_ptr(m_vpgen)^.move_to(tx, ty);

        m_start_x := tx;
        m_start_y := ty;
        m_vertices := 1;

      end
      else
      begin
        vpgen_segmentator_ptr(m_vpgen)^.line_to(tx, ty);

        Inc(m_vertices);

      end
    else
    if is_end_poly(cmd) then
    begin
      m_poly_flags := cmd;

      if is_closed(cmd) or vpgen_segmentator_ptr(m_vpgen)^._auto_close then
      begin
        if vpgen_segmentator_ptr(m_vpgen)^._auto_close then
          m_poly_flags := m_poly_flags or path_flags_close;

        if m_vertices > 2 then
          vpgen_segmentator_ptr(m_vpgen)^.line_to(m_start_x, m_start_y);

        m_vertices := 0;

      end;

    end
    else
    begin
      // path_cmd_stop
      if vpgen_segmentator_ptr(m_vpgen)^._auto_close and (m_vertices > 2) then
      begin
        vpgen_segmentator_ptr(m_vpgen)^.line_to(m_start_x, m_start_y);

        m_poly_flags := path_cmd_end_poly or path_flags_close;
        m_vertices := -2;

        continue;

      end;

      break;

    end;

  until False;

  Result := cmd;

end;

end.

