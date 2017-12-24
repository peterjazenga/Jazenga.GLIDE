
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}
unit agg_conv_curve;

interface

{$I agg_mode.inc }

uses
  agg_basics,
  agg_curves,
  agg_vertex_source;

type
  // Curve converter class. Any path storage can have Bezier curves defined
  // by their control points. There're two types of curves supported: curve3
  // and curve4. Curve3 is a conic Bezier curve with 2 endpoints and 1 control
  // point. Curve4 has 2 control points (4 points in total) and can be used
  // to interpolate more complicated curves. Curve4, unlike curve3 can be used
  // to approximate arcs, both circular and elliptical. Curves are approximated
  // with straight lines and one of the approaches is just to store the whole
  // sequence of vertices that approximate our curve. It takes additional
  // memory, and at the same time the consecutive vertices can be calculated
  // on demand.

  // Initially, path storages are not suppose to keep all the vertices of the
  // curves (although, nothing prevents us from doing so). Instead, path_storage
  // keeps only vertices, needed to calculate a curve on demand. Those vertices
  // are marked with special commands. So, if the path_storage contains curves
  // (which are not real curves yet), and we render this storage directly,
  // all we will see is only 2 or 3 straight line segments (for curve3 and
  // curve4 respectively). If we need to see real curves drawn we need to
  // include this class into the conversion pipeline.

  // Class conv_curve recognizes commands path_cmd_curve3 and path_cmd_curve4
  // and converts these vertices into a move_to/line_to sequence.

  conv_curve_ptr = ^conv_curve;

  conv_curve = object(curve)
    m_source: vertex_source_ptr;
    m_last_x, m_last_y: double;
    m_curve3, m_curve4: curve_ptr;
    constructor Construct(Source: vertex_source_ptr; c3: curve_ptr = nil; c4: curve_ptr = nil);
    destructor Destruct; virtual;
    procedure set_source(Source: vertex_source_ptr);
    procedure approximation_method_(v: curve_approximation_method_e); virtual;
    function _approximation_method: curve_approximation_method_e; virtual;
    procedure approximation_scale_(s: double); virtual;
    function _approximation_scale: double; virtual;
    procedure angle_tolerance_(a: double); virtual;
    function _angle_tolerance: double; virtual;
    procedure cusp_limit_(v: double); virtual;
    function _cusp_limit: double; virtual;
    procedure rewind(path_id: unsigned); virtual;
    function vertex(x, y: double_ptr): unsigned; virtual;
  end;


implementation

constructor conv_curve.Construct(Source: vertex_source_ptr; c3: curve_ptr = nil; c4: curve_ptr = nil);
begin
  if c3 <> nil then
    m_curve3 := c3
  else
    m_curve3 := new(curve3_ptr, Construct);

  if c4 <> nil then
    m_curve4 := c4
  else
    m_curve4 := new(curve4_ptr, Construct);

  m_source := Source;
  m_last_x := 0.0;
  m_last_y := 0.0;

end;

{ DESTRUCT }
destructor conv_curve.Destruct;
begin
  if m_curve3 <> nil then
    dispose(m_curve3, Destruct);

  if m_curve4 <> nil then
    dispose(m_curve4, Destruct);

end;

{ SET_SOURCE }
procedure conv_curve.set_source(Source: vertex_source_ptr);
begin
  m_source := Source;

end;

{ APPROXIMATION_METHOD_ }
procedure conv_curve.approximation_method_(v: curve_approximation_method_e);
begin
  m_curve3^.approximation_method_(v);
  m_curve4^.approximation_method_(v);

end;

{ _APPROXIMATION_METHOD }
function conv_curve._approximation_method: curve_approximation_method_e;
begin
  Result := m_curve4^._approximation_method;

end;

{ APPROXIMATION_SCALE_ }
procedure conv_curve.approximation_scale_(s: double);
begin
  m_curve3^.approximation_scale_(s);
  m_curve4^.approximation_scale_(s);

end;

{ _APPROXIMATION_SCALE }
function conv_curve._approximation_scale: double;
begin
  Result := m_curve4^._approximation_scale;

end;

{ ANGLE_TOLERANCE_ }
procedure conv_curve.angle_tolerance_(a: double);
begin
  m_curve3^.angle_tolerance_(a);
  m_curve4^.angle_tolerance_(a);

end;

{ _ANGLE_TOLERANCE }
function conv_curve._angle_tolerance: double;
begin
  Result := m_curve4^._angle_tolerance;

end;

{ CUSP_LIMIT_ }
procedure conv_curve.cusp_limit_(v: double);
begin
  m_curve3^.cusp_limit_(v);
  m_curve4^.cusp_limit_(v);

end;

{ _CUSP_LIMIT }
function conv_curve._cusp_limit: double;
begin
  Result := m_curve4^._cusp_limit;

end;

{ REWIND }
procedure conv_curve.rewind(path_id: unsigned);
begin
  m_source^.rewind(path_id);

  m_last_x := 0.0;
  m_last_y := 0.0;

  m_curve3^.reset;
  m_curve4^.reset;

end;

{ VERTEX }
function conv_curve.vertex(x, y: double_ptr): unsigned;
var
  ct2_x, ct2_y, end_x, end_y: double;

  cmd: unsigned;

begin
  if not is_stop(m_curve3^.vertex(x, y)) then
  begin
    m_last_x := x^;
    m_last_y := y^;

    Result := path_cmd_line_to;

    exit;

  end;

  if not is_stop(m_curve4^.vertex(x, y)) then
  begin
    m_last_x := x^;
    m_last_y := y^;

    Result := path_cmd_line_to;

    exit;

  end;

  cmd := m_source^.vertex(x, y);

  case cmd of
    path_cmd_move_to, path_cmd_line_to:
    begin
      m_last_x := x^;
      m_last_y := y^;

    end;

    path_cmd_curve3:
    begin
      m_source^.vertex(@end_x, @end_y);
      m_curve3^.init3(m_last_x, m_last_y, x^, y^, end_x, end_y);

      m_curve3^.vertex(x, y); // First call returns path_cmd_move_to
      m_curve3^.vertex(x, y); // This is the first vertex of the curve

      cmd := path_cmd_line_to;

    end;

    path_cmd_curve4:
    begin
      m_source^.vertex(@ct2_x, @ct2_y);
      m_source^.vertex(@end_x, @end_y);

      m_curve4^.init4(m_last_x, m_last_y, x^, y^, ct2_x, ct2_y, end_x, end_y);

      m_curve4^.vertex(x, y); // First call returns path_cmd_move_to
      m_curve4^.vertex(x, y); // This is the first vertex of the curve

      cmd := path_cmd_line_to;

    end;

  end;

  Result := cmd;

end;

end.



