
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}
unit
agg_span_interpolator_linear;

interface

{$I agg_mode.inc }
{$Q- }
{$R- }
uses
  agg_basics,
  agg_dda_line,
  agg_trans_affine;

type
  span_interpolator_ptr = ^span_interpolator;

  span_interpolator = object
    subpixel_shift, subpixel_size: unsigned;

    constructor Construct(SS: unsigned = 8);

    function _transformer: trans_affine_ptr; virtual; abstract;
    procedure transformer_(trans: trans_affine_ptr); virtual; abstract;

    procedure begin_(x, y: double; len: unsigned); virtual; abstract;

    procedure resynchronize(xe, ye: double; len: unsigned); virtual; abstract;

    procedure inc_operator; virtual; abstract;
    procedure coordinates(x, y: int_ptr); virtual; abstract;

    procedure local_scale(x, y: int_ptr); virtual;

  end;

  //================================================span_interpolator_linear
  span_interpolator_linear_ptr = ^span_interpolator_linear;

  span_interpolator_linear = object(span_interpolator)
    m_trans: trans_affine_ptr;
    m_li_x, m_li_y: dda2_line_interpolator;

    constructor Construct(SS: unsigned = 8); overload;
    constructor Construct(trans: trans_affine_ptr; SS: unsigned = 8); overload;
    constructor Construct(trans: trans_affine_ptr; x, y: double; len: unsigned; SS: unsigned = 8); overload;

    function _transformer: trans_affine_ptr; virtual;
    procedure transformer_(trans: trans_affine_ptr); virtual;

    procedure begin_(x, y: double; len: unsigned); virtual;

    procedure resynchronize(xe, ye: double; len: unsigned); virtual;

    procedure inc_operator; virtual;
    procedure coordinates(x, y: int_ptr); virtual;

  end;

  //=====================================span_interpolator_linear_subdiv
  span_interpolator_linear_subdiv = object(span_interpolator)
    m_subdiv_shift, m_subdiv_size, m_subdiv_mask: unsigned;

    m_trans: trans_affine_ptr;
    m_li_x, m_li_y: dda2_line_interpolator;

    m_src_x: int;
    m_src_y: double;
    m_pos, m_len: unsigned;

    constructor Construct(SS: unsigned = 8); overload;
    constructor Construct(trans: trans_affine_ptr; subdiv_shift: unsigned = 4; SS: unsigned = 8); overload;
    constructor Construct(trans: trans_affine_ptr; x, y: double; len: unsigned; subdiv_shift: unsigned = 4; SS: unsigned = 8); overload;

    function _transformer: trans_affine_ptr; virtual;
    procedure transformer_(trans: trans_affine_ptr); virtual;

    function _subdiv_shift: unsigned;
    procedure subdiv_shift_(shift: unsigned);

    procedure begin_(x, y: double; len: unsigned); virtual;

    procedure inc_operator; virtual;
    procedure coordinates(x, y: int_ptr); virtual;

  end;


implementation

constructor span_interpolator.Construct(SS: unsigned = 8);
begin
  subpixel_shift := SS;
  subpixel_size := 1 shl subpixel_shift;

end;

{ LOCAL_SCALE }
procedure span_interpolator.local_scale(x, y: int_ptr);
begin
end;

{ CONSTRUCT }
constructor span_interpolator_linear.Construct(SS: unsigned = 8);
begin
  inherited Construct(SS);

end;

{ CONSTRUCT }
constructor span_interpolator_linear.Construct(trans: trans_affine_ptr; SS: unsigned = 8);
begin
  Construct(SS);

  m_trans := trans;

end;

{ CONSTRUCT }
constructor span_interpolator_linear.Construct(trans: trans_affine_ptr; x, y: double; len: unsigned; SS: unsigned = 8);
begin
  Construct(trans, SS);

  begin_(x, y, len);

end;

{ _TRANSFORMER }
function span_interpolator_linear._transformer: trans_affine_ptr;
begin
  Result := m_trans;

end;

{ TRANSFORMER_ }
procedure span_interpolator_linear.transformer_(trans: trans_affine_ptr);
begin
  m_trans := trans;

end;

{ BEGIN_ }
procedure span_interpolator_linear.begin_(x, y: double; len: unsigned);
var
  tx, ty: double;

  x1, y1, x2, y2: int;

begin
  tx := x;
  ty := y;

  m_trans^.transform(m_trans, @tx, @ty);

  x1 := trunc(tx * subpixel_size);
  y1 := trunc(ty * subpixel_size);

  tx := x + len;
  ty := y;

  m_trans^.transform(m_trans, @tx, @ty);

  x2 := trunc(tx * subpixel_size);
  y2 := trunc(ty * subpixel_size);

  m_li_x.Construct(x1, x2, len);
  m_li_y.Construct(y1, y2, len);

end;

{ RESYNCHRONIZE }
procedure span_interpolator_linear.resynchronize(xe, ye: double; len: unsigned);
begin
  m_trans^.transform(m_trans, @xe, @ye);

  m_li_x.Construct(m_li_x._y, trunc(xe * subpixel_size), len);
  m_li_y.Construct(m_li_y._y, trunc(ye * subpixel_size), len);

end;

{ INC_OPERATOR }
procedure span_interpolator_linear.inc_operator;
begin
  m_li_x.plus_operator;
  m_li_y.plus_operator;

end;

{ COORDINATES }
procedure span_interpolator_linear.Coordinates(x, y: int_ptr);
begin
  x^ := m_li_x._y;
  y^ := m_li_y._y;

end;

{ CONSTRUCT }
constructor span_interpolator_linear_subdiv.Construct(SS: unsigned = 8);
begin
  inherited Construct(SS);

  m_subdiv_shift := 4;
  m_subdiv_size := 1 shl m_subdiv_shift;
  m_subdiv_mask := m_subdiv_size - 1;

end;

{ CONSTRUCT }
constructor span_interpolator_linear_subdiv.Construct(trans: trans_affine_ptr; subdiv_shift: unsigned = 4; SS: unsigned = 8);
begin
  inherited Construct(SS);

  m_subdiv_shift := subdiv_shift;
  m_subdiv_size := 1 shl m_subdiv_shift;
  m_subdiv_mask := m_subdiv_size - 1;

  m_trans := trans;

end;

{ CONSTRUCT }
constructor span_interpolator_linear_subdiv.Construct(trans: trans_affine_ptr; x, y: double; len: unsigned;
  subdiv_shift: unsigned = 4; SS: unsigned = 8);
begin
  Construct(trans, subdiv_shift, SS);

  begin_(x, y, len);

end;

{ _TRANSFORMER }
function span_interpolator_linear_subdiv._transformer: trans_affine_ptr;
begin
  Result := m_trans;

end;

{ TRANSFORMER_ }
procedure span_interpolator_linear_subdiv.transformer_(trans: trans_affine_ptr);
begin
  m_trans := trans;

end;

{ _SUBDIV_SHIFT }
function span_interpolator_linear_subdiv._subdiv_shift: unsigned;
begin
  Result := m_subdiv_shift;

end;

{ SUBDIV_SHIFT_ }
procedure span_interpolator_linear_subdiv.subdiv_shift_(shift: unsigned);
begin
  m_subdiv_shift := shift;
  m_subdiv_size := 1 shl m_subdiv_shift;
  m_subdiv_mask := m_subdiv_size - 1;

end;

{ BEGIN_ }
procedure span_interpolator_linear_subdiv.begin_(x, y: double; len: unsigned);
var
  tx, ty: double;
  x1, y1: int;

begin
  m_pos := 1;
  m_src_x := trunc(x * subpixel_size) + subpixel_size;
  m_src_y := y;
  m_len := len;

  if len > m_subdiv_size then
    len := m_subdiv_size;

  tx := x;
  ty := y;

  m_trans^.transform(m_trans, @tx, @ty);

  x1 := trunc(tx * subpixel_size);
  y1 := trunc(ty * subpixel_size);

  tx := x + len;
  ty := y;

  m_trans^.transform(m_trans, @tx, @ty);

  m_li_x.Construct(x1, trunc(tx * subpixel_size), len);
  m_li_y.Construct(y1, trunc(ty * subpixel_size), len);

end;

{ INC_OPERATOR }
procedure span_interpolator_linear_subdiv.inc_operator;
var
  tx, ty: double;

  len: unsigned;

begin
  m_li_x.plus_operator;
  m_li_y.plus_operator;

  if m_pos >= m_subdiv_size then
  begin
    len := m_len;

    if len > m_subdiv_size then
      len := m_subdiv_size;

    tx := m_src_x / subpixel_size + len;
    ty := m_src_y;

    m_trans^.transform(m_trans, @tx, @ty);

    m_li_x.Construct(m_li_x._y, trunc(tx * subpixel_size), len);
    m_li_y.Construct(m_li_y._y, trunc(ty * subpixel_size), len);

    m_pos := 0;

  end;

  Inc(m_src_x, subpixel_size);
  Inc(m_pos);
  Dec(m_len);

end;

{ COORDINATES }
procedure span_interpolator_linear_subdiv.coordinates(x, y: int_ptr);
begin
  x^ := m_li_x._y;
  y^ := m_li_y._y;

end;

end.




