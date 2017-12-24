
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}
unit
agg_dda_line;

interface

{$I agg_mode.inc }
{$Q- }
{$R- }
uses
  agg_basics;

const
  subpixel_shift = 8;
  subpixel_size = 1 shl subpixel_shift;
  subpixel_mask = subpixel_size - 1;

type

  //===================================================dda_line_interpolator
  dda_line_interpolator = object
    m_y, m_inc, m_dy, FractionShift, YShift: int;
    constructor Construct(FS: int; YS: int = 0); overload;
    constructor Construct(y1, y2: int; Count: unsigned; FS: int; YS: int = 0); overload;
    procedure plus_operator;
    procedure minus_operator;
    procedure inc_operator(n: int);
    procedure dec_operator(n: int);
    function _y: int;
    function _dy: int;
  end;

  //---------------------------------------------line_bresenham_interpolator
  dda2_line_interpolator = object
    m_cnt, m_lft, m_rem, m_mod, m_y: int;
    constructor Construct(y1, y2, Count: int); overload;  // Forward-adjusted line
    constructor Construct(y, Count: int); overload;       // Backward-adjusted line
    procedure plus_operator;
    procedure minus_operator;
    function _mod: int;
    function _rem: int;
    function _lft: int;
    function _y: int;
    procedure adjust_forward;
    procedure adjust_backward;
  end;

  line_bresenham_interpolator = object
    m_x1_lr, m_y1_lr, m_x2_lr, m_y2_lr: int;
    m_ver: boolean;
    m_len: unsigned;
    m_inc: int;
    m_interpolator: dda2_line_interpolator;
    constructor Construct(x1, y1, x2, y2: int);
    function line_lr(v: int): int;
    function _is_ver: boolean;
    function _len: unsigned;
    function _inc: int;
    procedure hstep;
    procedure vstep;
    function _x1: int;
    function _y1: int;
    function _x2: int;
    function _y2: int;
    function _x2_hr: int;
    function _y2_hr: int;
  end;

implementation

constructor dda_line_interpolator.Construct(FS: int; YS: int = 0);
begin
  FractionShift := FS;

  YShift := YS;

end;

{ CONSTRUCT }
constructor dda_line_interpolator.Construct(y1, y2: int; Count: unsigned; FS: int; YS: int = 0);
begin
  Construct(FS, YS);

  m_y := y1;
  m_inc := ((y2 - y1) shl FractionShift) div Count;
  m_dy := 0;

end;

{ PLUS_OPERATOR }
procedure dda_line_interpolator.plus_operator;
begin
  Inc(m_dy, m_inc);

end;

{ MINUS_OPERATOR }
procedure dda_line_interpolator.minus_operator;
begin
  Dec(m_dy, m_inc);

end;

{ INC_OPERATOR }
procedure dda_line_interpolator.inc_operator(n: int);
begin
  Inc(m_dy, m_inc * n);

end;

{ DEC_OPERATOR }
procedure dda_line_interpolator.dec_operator(n: int);
begin
  Dec(m_dy, m_inc * n);

end;

{ _Y }
function dda_line_interpolator._y: int;
begin
  Result := m_y + (shr_int32(m_dy, FractionShift - YShift));

end;

{ _DY }
function dda_line_interpolator._dy: int;
begin
  Result := m_dy;

end;

{ CONSTRUCT }
constructor dda2_line_interpolator.Construct(y1, y2, Count: int);
begin
  if Count <= 0 then
    m_cnt := 1
  else
    m_cnt := Count;

  m_lft := trunc((y2 - y1) / m_cnt);
  m_rem := trunc((y2 - y1) mod m_cnt);
  m_mod := m_rem;
  m_y := y1;

  if m_mod <= 0 then
  begin
    m_mod := m_mod + Count;
    m_rem := m_rem + Count;

    Dec(m_lft);

  end;

  m_mod := m_mod - Count;

end;

{ CONSTRUCT }
constructor dda2_line_interpolator.Construct(y, Count: int);
begin
  if Count <= 0 then
    m_cnt := 1
  else
    m_cnt := Count;

  m_lft := y div m_cnt;
  m_rem := y mod m_cnt;
  m_mod := m_rem;
  m_y := 0;

  if m_mod <= 0 then
  begin
    Inc(m_mod, Count);
    Inc(m_rem, Count);
    Dec(m_lft);

  end;

end;

{ PLUS_OPERATOR }
procedure dda2_line_interpolator.plus_operator;
begin
  Inc(m_mod, m_rem);
  Inc(m_y, m_lft);

  if m_mod > 0 then
  begin
    Dec(m_mod, m_cnt);
    Inc(m_y);

  end;

end;

{ MINUS_OPERATOR }
procedure dda2_line_interpolator.minus_operator;
begin
  if m_mod <= m_rem then
  begin
    Inc(m_mod, m_cnt);
    Dec(m_y);

  end;

  Dec(m_mod, m_rem);
  Dec(m_y, m_lft);

end;

{ _MOD }
function dda2_line_interpolator._mod: int;
begin
  Result := m_mod;

end;

{ _REM }
function dda2_line_interpolator._rem: int;
begin
  Result := m_rem;

end;

{ _LFT }
function dda2_line_interpolator._lft: int;
begin
  Result := m_lft;

end;

{ _Y }
function dda2_line_interpolator._y: int;
begin
  Result := m_y;

end;

{ ADJUST_FORWARD }
procedure dda2_line_interpolator.adjust_forward;
begin
  Dec(m_mod, m_cnt);

end;

{ ADJUST_BACKWARD }
procedure dda2_line_interpolator.adjust_backward;
begin
  Inc(m_mod, m_cnt);

end;

{ CONSTRUCT }
constructor line_bresenham_interpolator.Construct(x1, y1, x2, y2: int);
begin
  m_x1_lr := line_lr(x1);
  m_y1_lr := line_lr(y1);
  m_x2_lr := line_lr(x2);
  m_y2_lr := line_lr(y2);

  m_ver := Abs(m_x2_lr - m_x1_lr) < Abs(m_y2_lr - m_y1_lr);

  if m_ver then
    m_len := Abs(m_y2_lr - m_y1_lr)
  else
    m_len := Abs(m_x2_lr - m_x1_lr);

  if m_ver then
    if y2 > y1 then
      m_inc := 1
    else
      m_inc := -1
  else
  if x2 > x1 then
    m_inc := 1
  else
    m_inc := -1;

  if m_ver then
    m_interpolator.Construct(x1, x2, m_len)
  else
    m_interpolator.Construct(y1, y2, m_len);

end;

{ LINE_LR }
function line_bresenham_interpolator.line_lr(v: int): int;
begin
  Result := shr_int32(v, subpixel_shift);

end;

{ _IS_VER }
function line_bresenham_interpolator._is_ver: boolean;
begin
  Result := m_ver;

end;

{ _LEN }
function line_bresenham_interpolator._len: unsigned;
begin
  Result := m_len;

end;

{ _INC }
function line_bresenham_interpolator._inc: int;
begin
  Result := m_inc;

end;

{ HSTEP }
procedure line_bresenham_interpolator.hstep;
begin
  m_interpolator.plus_operator;

  m_x1_lr := m_x1_lr + m_inc;

end;

{ VSTEP }
procedure line_bresenham_interpolator.vstep;
begin
  m_interpolator.plus_operator;

  m_y1_lr := m_y1_lr + m_inc;

end;

{ _X1 }
function line_bresenham_interpolator._x1: int;
begin
  Result := m_x1_lr;

end;

{ _Y1 }
function line_bresenham_interpolator._y1: int;
begin
  Result := m_y1_lr;

end;

{ _X2 }
function line_bresenham_interpolator._x2: int;
begin
  Result := line_lr(m_interpolator._y);

end;

{ _Y2 }
function line_bresenham_interpolator._y2: int;
begin
  Result := line_lr(m_interpolator._y);

end;

{ _X2_HR }
function line_bresenham_interpolator._x2_hr: int;
begin
  Result := m_interpolator._y;

end;

{ _Y2_HR }
function line_bresenham_interpolator._y2_hr: int;
begin
  Result := m_interpolator._y;

end;

end.


