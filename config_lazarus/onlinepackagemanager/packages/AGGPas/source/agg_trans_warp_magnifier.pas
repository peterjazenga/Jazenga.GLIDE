
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}
unit
agg_trans_warp_magnifier;

interface

{$I agg_mode.inc }

uses
  agg_basics,
  agg_trans_affine;

type
  trans_warp_magnifier_ptr = ^trans_warp_magnifier;

  trans_warp_magnifier = object(trans_affine)
    m_xc, m_yc, m_magn, m_radius: double;

    constructor Construct;

    procedure center(x, y: double);
    procedure magnification(m: double);
    procedure radius(r: double);

  end;

implementation

procedure _transform(this: trans_warp_magnifier_ptr; x, y: double_ptr);
var
  dx, dy, r, m: double;

begin
  dx := x^ - this^.m_xc;
  dy := y^ - this^.m_yc;
  r := Sqrt(dx * dx + dy * dy);

  if r < this^.m_radius then
  begin
    x^ := this^.m_xc + dx * this^.m_magn;
    y^ := this^.m_yc + dy * this^.m_magn;

    exit;

  end;

  m := (r + this^.m_radius * (this^.m_magn - 1.0)) / r;

  x^ := this^.m_xc + dx * m;
  y^ := this^.m_yc + dy * m;

end;

{ _inverse_transform }
procedure _inverse_transform(this: trans_warp_magnifier_ptr; x, y: double_ptr);
var
  t: trans_warp_magnifier;

begin
  t.Construct;

  t := this^;

  t.magnification(1.0 / this^.m_magn);
  t.radius(this^.m_radius * this^.m_magn);
  t.transform(@t, x, y);

end;

{ CONSTRUCT }
constructor trans_warp_magnifier.Construct;
begin
  inherited Construct;

  m_xc := 0.0;
  m_yc := 0.0;

  m_magn := 1.0;
  m_radius := 1.0;

  Pointer(transform) := @_transform;
  Pointer(inverse_transform) := @_inverse_transform;

end;

{ CENTER }
procedure trans_warp_magnifier.center(x, y: double);
begin
  m_xc := x;
  m_yc := y;

end;

{ MAGNIFICATION }
procedure trans_warp_magnifier.magnification(m: double);
begin
  m_magn := m;

end;

{ RADIUS }
procedure trans_warp_magnifier.radius(r: double);
begin
  m_radius := r;

end;

end.




