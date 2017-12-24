
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}
unit
agg_ellipse;

interface

{$I agg_mode.inc }

uses
  Math,
  agg_basics,
  agg_vertex_source;

type

  ellipse_ptr = ^ellipse;

  ellipse = object(vertex_source)
    m_x, m_y, m_rx, m_ry, m_scale: double;
    m_num, m_step: unsigned;
    m_cw: boolean;
    constructor Construct; overload;
    constructor Construct(x, y, rx, ry: double; num_steps: unsigned = 0; cw: boolean = False); overload;
    procedure init(x, y, rx, ry: double; num_steps: unsigned = 0; cw: boolean = False);
    procedure approximation_scale_(scale: double);
    procedure rewind(path_id: unsigned); virtual;
    function vertex(x, y: double_ptr): unsigned; virtual;
    procedure calc_num_steps;
  end;


implementation

constructor ellipse.Construct;
begin
  inherited Construct;

  m_x := 0.0;
  m_y := 0.0;
  m_rx := 1.0;
  m_ry := 1.0;

  m_scale := 1.0;

  m_num := 4;
  m_step := 0;
  m_cw := False;

end;

{ CONSTRUCT }
constructor ellipse.Construct(x, y, rx, ry: double; num_steps: unsigned = 0; cw: boolean = False);
begin
  inherited Construct;

  m_x := x;
  m_y := y;
  m_rx := rx;
  m_ry := ry;

  m_scale := 1.0;

  m_num := num_steps;
  m_step := 0;
  m_cw := cw;

  if m_num = 0 then
    calc_num_steps;

end;

{ INIT }
procedure ellipse.init(x ,y ,rx ,ry : double; num_steps : unsigned = 0; cw : boolean = false );
begin
  m_x := x;
  m_y := y;
  m_rx := rx;
  m_ry := ry;

  m_num := num_steps;
  m_step := 0;
  m_cw := cw;

  if m_num = 0 then
    calc_num_steps;

end;

{ APPROXIMATION_SCALE_ }
procedure ellipse.approximation_scale_(scale : double );
begin
  m_scale := scale;

  calc_num_steps;

end;

{ REWIND }
procedure ellipse.rewind(path_id : unsigned );
begin
  m_step := 0;

end;

{ VERTEX }
function ellipse.vertex(x ,y : double_ptr ) : unsigned;
var
  angle: double;

begin
  if m_step = m_num then
  begin
    Inc(m_step);

    Result := path_cmd_end_poly or path_flags_close or path_flags_ccw;

    exit;

  end;

  if m_step > m_num then
  begin
    Result := path_cmd_stop;

    exit;

  end;

  angle := m_step / m_num * 2.0 * pi;

  if m_cw then
    angle := 2.0 * pi - angle;

  x^ := m_x + Cos(angle) * m_rx;
  y^ := m_y + Sin(angle) * m_ry;

  Inc(m_step);

  if m_step = 1 then
    Result := path_cmd_move_to
  else
    Result := path_cmd_line_to;

end;

{ CALC_NUM_STEPS }
procedure ellipse.calc_num_steps;
var
  ra, da: double;

begin
  ra := (Abs(m_rx) + Abs(m_ry)) / 2;
  da := ArcCos(ra / (ra + 0.125 / m_scale)) * 2;

  m_num := trunc(2 * pi / da);

end;

end.

