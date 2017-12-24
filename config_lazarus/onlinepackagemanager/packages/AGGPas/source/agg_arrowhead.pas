
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit
agg_arrowhead;

interface

{$I agg_mode.inc }

uses
  agg_basics,
  agg_vertex_source;

type

  arrowhead = object(vertex_source)
    m_head_d1, m_head_d2, m_head_d3, m_head_d4, m_tail_d1, m_tail_d2, m_tail_d3, m_tail_d4: double;
    m_head_flag, m_tail_flag: boolean;
    m_coord: array[0..15] of double;
    m_cmd: array[0..7] of unsigned;
    m_curr_id, m_curr_coord: unsigned;
    constructor Construct;
    procedure head_(d1, d2, d3, d4: double);
    procedure head;
    procedure no_head;
    procedure tail_(d1, d2, d3, d4: double);
    procedure tail;
    procedure no_tail;
    procedure rewind(path_id: unsigned); virtual;
    function vertex(x, y: double_ptr): unsigned; virtual;
  end;



implementation

constructor arrowhead.Construct;
begin
  m_head_d1 := 1.0;
  m_head_d2 := 1.0;
  m_head_d3 := 1.0;
  m_head_d4 := 0.0;
  m_tail_d1 := 1.0;
  m_tail_d2 := 1.0;
  m_tail_d3 := 1.0;
  m_tail_d4 := 0.0;

  m_head_flag := False;
  m_tail_flag := False;

  m_curr_id := 0;
  m_curr_coord := 0;

end;

{ HEAD_ }
procedure arrowhead.head_(d1 ,d2 ,d3 ,d4 : double );
begin
  m_head_d1 := d1;
  m_head_d2 := d2;
  m_head_d3 := d3;
  m_head_d4 := d4;

  m_head_flag := True;

end;

{ HEAD }
procedure arrowhead.head;
begin
  m_head_flag := True;

end;

{ NO_HEAD }
procedure arrowhead.no_head;
begin
  m_head_flag := False;

end;

{ TAIL_ }
procedure arrowhead.tail_(d1 ,d2 ,d3 ,d4 : double );
begin
  m_tail_d1 := d1;
  m_tail_d2 := d2;
  m_tail_d3 := d3;
  m_tail_d4 := d4;

  m_tail_flag := True;

end;

{ TAIL }
procedure arrowhead.tail;
begin
  m_tail_flag := True;

end;

{ NO_TAIL }
procedure arrowhead.no_tail;
begin
  m_tail_flag := False;

end;

{ REWIND }
procedure arrowhead.rewind(path_id : unsigned );
begin
  m_curr_id := path_id;
  m_curr_coord := 0;

  if path_id = 0 then
  begin
    if not m_tail_flag then
    begin
      m_cmd[0] := path_cmd_stop;

      exit;

    end;

    m_coord[0] := m_tail_d1;
    m_coord[1] := 0.0;
    m_coord[2] := m_tail_d1 - m_tail_d4;
    m_coord[3] := m_tail_d3;
    m_coord[4] := -m_tail_d2 - m_tail_d4;
    m_coord[5] := m_tail_d3;
    m_coord[6] := -m_tail_d2;
    m_coord[7] := 0.0;
    m_coord[8] := -m_tail_d2 - m_tail_d4;
    m_coord[9] := -m_tail_d3;
    m_coord[10] := m_tail_d1 - m_tail_d4;
    m_coord[11] := -m_tail_d3;

    m_cmd[0] := path_cmd_move_to;
    m_cmd[1] := path_cmd_line_to;
    m_cmd[2] := path_cmd_line_to;
    m_cmd[3] := path_cmd_line_to;
    m_cmd[4] := path_cmd_line_to;
    m_cmd[5] := path_cmd_line_to;
    m_cmd[7] := path_cmd_end_poly or path_flags_close or path_flags_ccw;
    m_cmd[6] := path_cmd_stop;

    exit;

  end;

  if path_id = 1 then
  begin
    if not m_head_flag then
    begin
      m_cmd[0] := path_cmd_stop;

      exit;

    end;

    m_coord[0] := -m_head_d1;
    m_coord[1] := 0.0;
    m_coord[2] := m_head_d2 + m_head_d4;
    m_coord[3] := -m_head_d3;
    m_coord[4] := m_head_d2;
    m_coord[5] := 0.0;
    m_coord[6] := m_head_d2 + m_head_d4;
    m_coord[7] := m_head_d3;

    m_cmd[0] := path_cmd_move_to;
    m_cmd[1] := path_cmd_line_to;
    m_cmd[2] := path_cmd_line_to;
    m_cmd[3] := path_cmd_line_to;
    m_cmd[4] := path_cmd_end_poly or path_flags_close or path_flags_ccw;
    m_cmd[5] := path_cmd_stop;

    exit;

  end;

end;

{ VERTEX }
function arrowhead.vertex(x ,y : double_ptr ) : unsigned;
var
  curr_idx: unsigned;

begin
  if m_curr_id < 2 then
  begin
    curr_idx := m_curr_coord * 2;

    x^ := m_coord[curr_idx];
    y^ := m_coord[curr_idx + 1];

    Result := m_cmd[m_curr_coord];

    Inc(m_curr_coord);

  end
  else
    Result := path_cmd_stop;

end;

end.

