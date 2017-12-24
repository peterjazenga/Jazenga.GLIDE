
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}
unit
agg_conv_adaptor_vcgen;

interface

{$I agg_mode.inc }

uses
  agg_basics,
  agg_vertex_source;

type
  //------------------------------------------------------------null_markers
  null_markers_ptr = ^null_markers;

  null_markers = object(vertex_source)
    m_markers: vertex_source_ptr;

    constructor Construct;

    procedure remove_all; virtual;
    procedure add_vertex(x, y: double; cmd: unsigned); virtual;
    procedure prepare_src;

    procedure rewind(path_id: unsigned); virtual;
    function vertex(x, y: double_ptr): unsigned; virtual;

    procedure set_markers(m: vertex_source_ptr);

  end;

  //------------------------------------------------------conv_adaptor_vcgen
  status = (initial, accumulate, generate);

  conv_adaptor_vcgen_ptr = ^conv_adaptor_vcgen;

  conv_adaptor_vcgen = object(vertex_source)
    m_source, m_generator: vertex_source_ptr;
    m_markers: null_markers;
    m_status: status;
    m_last_cmd: unsigned;
    m_start_x, m_start_y: double;

    constructor Construct(Source, gen: vertex_source_ptr);

    procedure set_source(Source: vertex_source_ptr);
    procedure set_markers(m: vertex_source_ptr);

    function generator: vertex_source_ptr;
    function markers: vertex_source_ptr;

    procedure rewind(path_id: unsigned); virtual;
    function vertex(x, y: double_ptr): unsigned; virtual;

  end;


implementation

constructor null_markers.Construct;
begin
  inherited Construct;

  m_markers := nil;

end;

procedure null_markers.remove_all;
begin
  if m_markers <> nil then
    m_markers^.remove_all;
end;

procedure null_markers.add_vertex(x, y: double; cmd: unsigned);
begin
  if m_markers <> nil then
    m_markers^.add_vertex(x, y, cmd);
end;

procedure null_markers.prepare_src;
begin
end;

procedure null_markers.rewind(path_id: unsigned);
begin
  if m_markers <> nil then
    m_markers^.rewind(path_id);

end;

function null_markers.vertex(x, y: double_ptr): unsigned;
begin
  if m_markers <> nil then
    Result := m_markers^.vertex(x, y)
  else
    Result := path_cmd_stop;

end;
procedure null_markers.set_markers(m: vertex_source_ptr);
begin
  m_markers := m;
end;

constructor conv_adaptor_vcgen.Construct(Source, gen: vertex_source_ptr);
begin
  inherited Construct;

  m_source := Source;
  m_status := initial;

  m_generator := gen;

  m_markers.Construct;

  m_last_cmd := 0;
  m_start_x := 0;
  m_start_y := 0;

end;

procedure conv_adaptor_vcgen.set_source(Source: vertex_source_ptr);
begin
  m_source := Source;

end;

{ SET_MARKERS }
procedure conv_adaptor_vcgen.set_markers(m: vertex_source_ptr);
begin
  m_markers.set_markers(m);

end;

{ GENERATOR }
function conv_adaptor_vcgen.generator: vertex_source_ptr;
begin
  Result := m_generator;

end;

{ MARKERS }
function conv_adaptor_vcgen.markers: vertex_source_ptr;
begin
  if m_markers.m_markers <> nil then
    Result := m_markers.m_markers
  else
    Result := @m_markers;

end;

{ REWIND }
procedure conv_adaptor_vcgen.rewind(path_id: unsigned);
begin
  m_source^.rewind(path_id);

  m_status := initial;

end;

{ VERTEX }
function conv_adaptor_vcgen.vertex(x, y: double_ptr): unsigned;
var
  cmd: unsigned;
  done: boolean;

label
  _acc, _gen, _end;

begin
  cmd := path_cmd_stop;
  done := False;

  while not done do
  begin
    case m_status of
      initial:
      begin
        m_markers.remove_all;

        m_last_cmd := m_source^.vertex(@m_start_x, @m_start_y);
        m_status := accumulate;

        goto _acc;

      end;

      accumulate:
      begin
        _acc:
          if is_stop(m_last_cmd) then
          begin
            Result := path_cmd_stop;

            exit;

          end;

        m_generator^.remove_all;
        m_generator^.add_vertex(m_start_x, m_start_y, path_cmd_move_to);
        m_markers.add_vertex(m_start_x, m_start_y, path_cmd_move_to);

        repeat
          cmd := m_source^.vertex(x, y);

          if is_vertex(cmd) then
          begin
            m_last_cmd := cmd;

            if is_move_to(cmd) then
            begin
              m_start_x := x^;
              m_start_y := y^;

              break;

            end;

            m_generator^.add_vertex(x^, y^, cmd);
            m_markers.add_vertex(x^, y^, path_cmd_line_to);

          end
          else
          begin
            if is_stop(cmd) then
            begin
              m_last_cmd := path_cmd_stop;

              break;

            end;

            if is_end_poly(cmd) then
            begin
              m_generator^.add_vertex(x^, y^, cmd);

              break;

            end;

          end;

        until False;

        m_generator^.rewind(0);

        m_status := generate;

        goto _gen;

      end;

      generate:
      begin
        _gen:
          cmd := m_generator^.vertex(x, y);

        if is_stop(cmd) then
        begin
          m_status := accumulate;

          goto _end;

        end;

        done := True;

      end;

    end;

    _end: ;
  end;

  Result := cmd;

end;

end.

