
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}
unit
agg_span_interpolator_trans;

interface

{$I agg_mode.inc }
{$Q- }
{$R- }
uses
  agg_basics,
  agg_trans_affine,
  agg_span_interpolator_linear;

type
  span_interpolator_trans = object(span_interpolator)
    m_trans: trans_affine_ptr;

    m_x, m_y: double;
    m_ix, m_iy: int;

    constructor Construct(SS: unsigned = 8); overload;
    constructor Construct(trans: trans_affine_ptr; SS: unsigned = 8); overload;
    constructor Construct(trans: trans_affine_ptr; x, y, z: unsigned; SS: unsigned = 8); overload;

    function _transformer: trans_affine_ptr; virtual;
    procedure transformer_(trans: trans_affine_ptr); virtual;

    procedure begin_(x, y: double; len: unsigned); virtual;

    procedure inc_operator; virtual;
    procedure coordinates(x, y: int_ptr); virtual;

  end;

implementation

constructor span_interpolator_trans.Construct(SS: unsigned = 8);
begin
  inherited Construct(SS);

  m_trans := nil;

end;

{ CONSTRUCT }
constructor span_interpolator_trans.Construct(trans: trans_affine_ptr; SS: unsigned = 8);
begin
  inherited Construct(SS);

  m_trans := trans;

end;

{ CONSTRUCT }
constructor span_interpolator_trans.Construct(trans: trans_affine_ptr; x, y, z: unsigned; SS: unsigned = 8);
begin
  inherited Construct(SS);

  m_trans := trans;

  begin_(x, y, 0);

end;

{ _TRANSFORMER }
function span_interpolator_trans._transformer: trans_affine_ptr;
begin
  Result := m_trans;

end;

{ TRANSFORMER_ }
procedure span_interpolator_trans.transformer_(trans: trans_affine_ptr);
begin
  m_trans := trans;

end;

{ BEGIN_ }
procedure span_interpolator_trans.begin_(x, y: double; len: unsigned);
begin
  m_x := x;
  m_y := y;

  m_trans^.transform(m_trans, @x, @y);

  m_ix := iround(x * subpixel_size);
  m_iy := iround(y * subpixel_size);

end;

{ INC_OPERATOR }
procedure span_interpolator_trans.inc_operator;
var
  x, y: double;

begin
  m_x := m_x + 1.0;

  x := m_x;
  y := m_y;

  m_trans^.transform(m_trans, @x, @y);

  m_ix := iround(x * subpixel_size);
  m_iy := iround(y * subpixel_size);

end;

{ COORDINATES }
procedure span_interpolator_trans.coordinates(x, y: int_ptr);
begin
  x^ := m_ix;
  y^ := m_iy;

end;

end.




