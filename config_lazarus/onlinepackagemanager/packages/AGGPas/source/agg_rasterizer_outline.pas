
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}
unit
agg_rasterizer_outline;

interface

{$I agg_mode.inc }

uses
  agg_basics,
  agg_renderer_primitives,
  agg_vertex_source;

type
  rasterizer_outline_ptr = ^rasterizer_outline;

  rasterizer_outline = object
    m_ren: renderer_primitives_ptr;

    m_start_x, m_start_y: int;

    m_vertices: unsigned;

    constructor Construct(ren: renderer_primitives_ptr);

    procedure move_to(x, y: int);
    procedure line_to(x, y: int);

    procedure move_to_d(x, y: double);
    procedure line_to_d(x, y: double);
    procedure Close;

    procedure add_vertex(x, y: double; cmd: unsigned);
    procedure add_path(vs: vertex_source_ptr; path_id: unsigned = 0);

  end;

implementation

constructor rasterizer_outline.Construct(ren: renderer_primitives_ptr);
begin
  m_ren := ren;

  m_start_x := 0;
  m_start_y := 0;

  m_vertices := 0;

end;

{ MOVE_TO }
procedure rasterizer_outline.move_to(x, y: int);
begin
  m_vertices := 1;

  m_start_x := x;
  m_start_y := y;

  m_ren^.move_to(x, y);

end;

{ LINE_TO }
procedure rasterizer_outline.line_to(x, y: int);
begin
  Inc(m_vertices);

  m_ren^.line_to(x, y);

end;

{ MOVE_TO_D }
procedure rasterizer_outline.move_to_d(x, y: double);
begin
  move_to(m_ren^.coord(x), m_ren^.coord(y));

end;

{ LINE_TO_D }
procedure rasterizer_outline.line_to_d(x, y: double);
begin
  line_to(m_ren^.coord(x), m_ren^.coord(y));

end;

{ CLOSE }
procedure rasterizer_outline.Close;
begin
  if m_vertices > 2 then
    line_to(m_start_x, m_start_y);

  m_vertices := 0;

end;

{ ADD_VERTEX }
procedure rasterizer_outline.add_vertex(x, y: double; cmd: unsigned);
begin
  if is_move_to(cmd) then
    move_to_d(x, y)
  else
  if is_end_poly(cmd) then
    if is_closed(cmd) then
      Close
    else
  else
    line_to_d(x, y);

end;

{ ADD_PATH }
procedure rasterizer_outline.add_path(vs: vertex_source_ptr; path_id: unsigned = 0);
var
  cmd: unsigned;
  x, y: double;

begin
  vs^.rewind(path_id);

  cmd := vs^.vertex(@x, @y);

  while not is_stop(cmd) do
  begin
    add_vertex(x, y, cmd);

    cmd := vs^.vertex(@x, @y);

  end;

end;

end.

