
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}
unit
agg_ellipse_bresenham;

interface

{$I agg_mode.inc }

uses agg_basics;

type

  ellipse_bresenham_interpolator = object
    m_rx2, m_ry2, m_two_rx2, m_two_ry2, m_dx, m_dy, m_inc_x, m_inc_y, m_cur_f: int;
    constructor Construct(rx, ry: int);
    function _dx: int;
    function _dy: int;
    procedure inc_operator;
  end;

implementation

constructor ellipse_bresenham_interpolator.Construct(rx, ry: int);
begin
  m_rx2 := rx * rx;
  m_ry2 := ry * ry;

  m_two_rx2 := m_rx2 shl 1;
  m_two_ry2 := m_ry2 shl 1;

  m_dx := 0;
  m_dy := 0;

  m_inc_x := 0;
  m_inc_y := -ry * m_two_rx2;
  m_cur_f := 0;

end;

{ _DX }
function ellipse_bresenham_interpolator._dx: int;
begin
  Result := m_dx;

end;

{ _DY }
function ellipse_bresenham_interpolator._dy: int;
begin
  Result := m_dy;

end;

{ INC_OPERATOR }
procedure ellipse_bresenham_interpolator.inc_operator;
var
  mx, my, mxy, min_m, fx, fy, fxy: int;

  flag: boolean;

begin
  mx := m_cur_f + m_inc_x + m_ry2;
  fx := mx;

  if mx < 0 then
    mx := -mx;

  my := m_cur_f + m_inc_y + m_rx2;
  fy := my;

  if my < 0 then
    my := -my;

  mxy := m_cur_f + m_inc_x + m_ry2 + m_inc_y + m_rx2;
  fxy := mxy;

  if mxy < 0 then
    mxy := -mxy;

  min_m := mx;
  flag := True;

  if min_m > my then
  begin
    min_m := my;
    flag := False;

  end;

  m_dx := 0;
  m_dy := 0;

  if min_m > mxy then
  begin
    Inc(m_inc_x, m_two_ry2);
    Inc(m_inc_y, m_two_rx2);

    m_cur_f := fxy;

    m_dx := 1;
    m_dy := 1;

    exit;

  end;

  if flag then
  begin
    Inc(m_inc_x, m_two_ry2);

    m_cur_f := fx;
    m_dx := 1;

    exit;

  end;

  Inc(m_inc_y, m_two_rx2);

  m_cur_f := fy;
  m_dy := 1;

end;

end.


