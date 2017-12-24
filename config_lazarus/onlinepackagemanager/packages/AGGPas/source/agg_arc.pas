
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit agg_arc;

interface

{$I agg_mode.inc }

uses
  Math,
  agg_basics,
  agg_vertex_source;

type

  arc = object(vertex_source)
    m_x, m_y, m_rx, m_ry, m_angle, m_start, m_end, m_scale, m_da: double;
    m_ccw, m_initialized: boolean;
    m_path_cmd: unsigned;
    constructor Construct; overload;
    constructor Construct(x, y, rx, ry, a1, a2: double; ccw: boolean = True); overload;
    procedure init(x, y, rx, ry, a1, a2: double; ccw: boolean = True);
    procedure approximation_scale_(s: double);
    function _approximation_scale: double;
    procedure rewind(path_id: unsigned); virtual;
    function vertex(x, y: double_ptr): unsigned; virtual;
    procedure normalize(a1, a2: double; ccw: boolean);
  end;


implementation

constructor arc.Construct;
begin
  m_x := 0;
  m_y := 0;
  m_rx := 0;
  m_ry := 0;
  m_angle := 0;
  m_start := 0;
  m_end := 0;
  m_da := 0;

  m_ccw := False;
  m_path_cmd := 0;

  m_scale := 1;

  m_initialized := False;

end;

{ CONSTRUCT }
constructor arc.Construct(x, y, rx, ry, a1, a2: double; ccw: boolean = True);
begin
  Construct;

  m_x := x;
  m_y := y;
  m_rx := rx;
  m_ry := ry;

  m_scale := 1;

  normalize(a1, a2, ccw);

end;

{ INIT }
procedure arc.init(x, y, rx, ry, a1, a2: double; ccw: boolean = True);
begin
  m_x := x;
  m_y := y;
  m_rx := rx;
  m_ry := ry;

  normalize(a1, a2, ccw);

end;

{ APPROXIMATION_SCALE_ }
procedure arc.approximation_scale_(s: double);
begin
  m_scale := s;

  if m_initialized then
    normalize(m_start, m_end, m_ccw);

end;

{ _APPROXIMATION_SCALE }
function arc._approximation_scale: double;
begin
  Result := m_scale;

end;

{ REWIND }
procedure arc.rewind(path_id: unsigned);
begin
  m_path_cmd := path_cmd_move_to;
  m_angle := m_start;

end;

{ VERTEX }
function arc.vertex(x, y: double_ptr): unsigned;
var
  pf: unsigned;

begin
 if is_stop(m_path_cmd ) then
  result:=path_cmd_stop

 else
  if (m_angle < (m_end - (m_da / 4)) ) <> m_ccw then
   begin
    x^:=m_x + (Cos(m_end ) * m_rx);
    y^:=m_y + (Sin(m_end ) * m_ry);

    m_path_cmd := path_cmd_stop;

    Result := path_cmd_line_to;

  end
  else
  begin
    x^ := m_x + Cos(m_angle) * m_rx;
    y^ := m_y + Sin(m_angle) * m_ry;

    m_angle := m_angle + m_da;

    pf := m_path_cmd;
    m_path_cmd := path_cmd_line_to;

    Result := pf;

  end;

end;

{ NORMALIZE }
procedure arc.normalize(a1, a2: double; ccw: boolean);
var
  ra: double;

begin
  ra := (Abs(m_rx) + Abs(m_ry)) / 2;
  m_da := ArcCos(ra / (ra + 0.125 / m_scale)) * 2;

  if ccw then
    while a2 < a1 do
      a2 := a2 + (pi * 2.0)
  else
  begin
    while a1 < a2 do
      a1 := a1 + (pi * 2.0);

    m_da := -m_da;

  end;

  m_ccw := ccw;
  m_start := a1;
  m_end := a2;

  m_initialized := True;

end;

end.

