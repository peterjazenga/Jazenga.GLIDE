
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}
unit agg_gamma_functions;

interface

{$I agg_mode.inc }

uses
  Math,
  agg_basics,
  agg_vertex_source;

type

  gamma_none = object(vertex_source)
  end;

  gamma_power = object(vertex_source)
    m_gamma: double;
    constructor Construct; overload;
    constructor Construct(g: double); overload;
    procedure gamma_(g: double);
    function _gamma: double;
    function func_operator_gamma(x: double): double; virtual;
  end;

  gamma_threshold = object(vertex_source)
    m_threshold: double;
    constructor Construct; overload;
    constructor Construct(t: double); overload;
    procedure threshold_(t: double);
    function _threshold: double;
    function func_operator_gamma(x: double): double; virtual;
  end;

  gamma_linear = object(vertex_source)
    m_start, m_end: double;
    constructor Construct; overload;
    constructor Construct(s, e: double); overload;
    procedure set_(s, e: double);
    procedure start_(s: double);
    procedure end_(e: double);
    function _start: double;
    function _end: double;
    function func_operator_gamma(x: double): double; virtual;
  end;

  gamma_multiply = object(vertex_source)
    m_mul: double;
    constructor Construct; overload;
    constructor Construct(v: double); overload;
    procedure value_(v: double);
    function _value: double;
    function func_operator_gamma(x: double): double; virtual;
  end;

implementation

constructor gamma_power.Construct;
begin
  m_gamma := 1.0;

end;

{ CONSTRUCT }
constructor gamma_power.Construct(g: double);
begin
  m_gamma := g;

end;

{ GAMMA_ }
procedure gamma_power.gamma_(g: double);
begin
  m_gamma := g;

end;

{ _GAMMA }
function gamma_power._gamma: double;
begin
  Result := m_gamma;

end;

{ FUNC_OPERATOR_GAMMA }
function gamma_power.func_operator_gamma(x: double): double;
begin
  try
    Result := power(x, m_gamma);

  except
    Result := 1;

  end;

end;

{ CONSTRUCT }
constructor gamma_threshold.Construct;
begin
  m_threshold := 0.5;

end;

{ CONSTRUCT }
constructor gamma_threshold.Construct(t: double);
begin
  m_threshold := t;

end;

{ THRESHOLD_ }
procedure gamma_threshold.threshold_(t: double);
begin
  m_threshold := t;

end;

{ _THRESHOLD }
function gamma_threshold._threshold: double;
begin
  Result := m_threshold;

end;

{ FUNC_OPERATOR_GAMMA }
function gamma_threshold.func_operator_gamma(x: double): double;
begin
  if x < m_threshold then
    Result := 0.0
  else
    Result := 1.0;

end;

{ CONSTRUCT }
constructor gamma_linear.Construct;
begin
  m_start := 0;
  m_end := 1;

end;

{ CONSTRUCT }
constructor gamma_linear.Construct(s, e: double);
begin
  m_start := s;
  m_end := e;

end;

{ SET_ }
procedure gamma_linear.set_(s, e: double);
begin
  m_start := s;
  m_end := e;

end;

{ START_ }
procedure gamma_linear.start_(s: double);
begin
  m_start := s;

end;

{ END_ }
procedure gamma_linear.end_(e: double);
begin
  m_end := e;

end;

{ _START }
function gamma_linear._start: double;
begin
  Result := m_start;

end;

{ _END }
function gamma_linear._end: double;
begin
  Result := m_end;

end;

{ FUNC_OPERATOR_GAMMA }
function gamma_linear.func_operator_gamma(x: double): double;
begin
  if x < m_start then
    Result := 0
  else
  if x > m_end then
    Result := 1
  else
  if m_end - m_start <> 0 then
    Result := (x - m_start) / (m_end - m_start)
  else
    Result := 0;

end;

{ CONSTRUCT }
constructor gamma_multiply.Construct;
begin
  m_mul := 1.0;

end;

{ CONSTRUCT }
constructor gamma_multiply.Construct(v: double);
begin
  m_mul := v;

end;

{ VALUE_ }
procedure gamma_multiply.value_(v: double);
begin
  m_mul := v;

end;

{ _VALUE }
function gamma_multiply._value: double;
begin
  Result := m_mul;

end;

{ FUNC_OPERATOR_GAMMA }
function gamma_multiply.func_operator_gamma(x: double): double;
var
  y: double;

begin
  y := x * m_mul;

  if y > 1.0 then
    y := 1.0;

  Result := y;

end;

end.


