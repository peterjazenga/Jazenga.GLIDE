
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}
unit agg_gamma_spline;

interface

{$I agg_mode.inc }

uses
  agg_basics,
  agg_bspline,
  agg_vertex_source;

type
  // Class-helper for calculation gamma-correction arrays. A gamma-correction
  // array is an array of 256 unsigned chars that determine the actual values
  // of Anti-Aliasing for each pixel coverage value from 0 to 255. If all the
  // values in the array are equal to its index, i.e. 0,1,2,3,... there's
  // no gamma-correction. Class agg::polyfill allows you to use custom
  // gamma-correction arrays. You can calculate it using any approach, and
  // class gamma_spline allows you to calculate almost any reasonable shape
  // of the gamma-curve with using only 4 values - kx1, ky1, kx2, ky2.

  //                                      kx2
  //        +----------------------------------+
  //        |                 |        |    .  |
  //        |                 |        | .     | ky2
  //        |                 |       .  ------|
  //        |                 |    .           |
  //        |                 | .              |
  //        |----------------.|----------------|
  //        |             .   |                |
  //        |          .      |                |
  //        |-------.         |                |
  //    ky1 |    .   |        |                |
  //        | .      |        |                |
  //        +----------------------------------+
  //            kx1

  // Each value can be in range [0...2]. Value 1.0 means one quarter of the
  // bounding rectangle. Function values() calculates the curve by these
  // 4 values. After calling it one can get the gamma-array with call gamma().
  // Class also supports the vertex source interface, i.e rewind() and
  // vertex(). It's made for convinience and used in class gamma_ctrl.
  // Before calling rewind/vertex one must set the bounding box
  // box() using pixel coordinates.

  gamma_spline = object(vertex_source)
    m_gamma: array[0..255] of char;
    m_x, m_y: array[0..3] of double;
    m_x1, m_y1, m_x2, m_y2, m_cur_x: double;
    m_spline: bspline;
    constructor Construct;
    destructor Destruct; virtual;
    procedure values(kx1, ky1, kx2, ky2: double); overload;
    procedure values(kx1, ky1, kx2, ky2: double_ptr); overload;
    function gamma: char_ptr;
    function _y(x: double): double;
    procedure box(x1, y1, x2, y2: double);
    procedure rewind(path_id: unsigned); virtual;
    function vertex(x, y: double_ptr): unsigned; virtual;
  end;

implementation

constructor gamma_spline.Construct;
begin
  m_spline.Construct;

  m_x1 := 0;
  m_y1 := 0;
  m_x2 := 10;
  m_y2 := 10;

  m_cur_x := 0.0;

  values(1.0, 1.0, 1.0, 1.0);

end;

{ DESTRUCT }
destructor gamma_spline.Destruct;
begin
  m_spline.Destruct;

end;

{ VALUES }
procedure gamma_spline.values(kx1, ky1, kx2, ky2: double);
var
  i: int;

begin
  if kx1 < 0.001 then
    kx1 := 0.001;

  if kx1 > 1.999 then
    kx1 := 1.999;

  if ky1 < 0.001 then
    ky1 := 0.001;

  if ky1 > 1.999 then
    ky1 := 1.999;

  if kx2 < 0.001 then
    kx2 := 0.001;

  if kx2 > 1.999 then
    kx2 := 1.999;

  if ky2 < 0.001 then
    ky2 := 0.001;

  if ky2 > 1.999 then
    ky2 := 1.999;

  m_x[0] := 0.0;
  m_y[0] := 0.0;
  m_x[1] := kx1 * 0.25;
  m_y[1] := ky1 * 0.25;
  m_x[2] := 1.0 - kx2 * 0.25;
  m_y[2] := 1.0 - ky2 * 0.25;
  m_x[3] := 1.0;
  m_y[3] := 1.0;

  m_spline.init(4, @m_x, @m_y);

  for i := 0 to 255 do
    m_gamma[i] := char(trunc(_y(i / 255.0) * 255.0));

end;

{ VALUES }
procedure gamma_spline.values(kx1, ky1, kx2, ky2: double_ptr);
begin
  kx1^ := m_x[1] * 4.0;
  ky1^ := m_y[1] * 4.0;
  kx2^ := (1.0 - m_x[2]) * 4.0;
  ky2^ := (1.0 - m_y[2]) * 4.0;

end;

{ GAMMA }
function gamma_spline.gamma: char_ptr;
begin
  Result := @m_gamma[0];

end;

{ _Y }
function gamma_spline._y(x: double): double;
var
  val: double;

begin
  if x < 0.0 then
    x := 0.0;

  if x > 1.0 then
    x := 1.0;

  val := m_spline.get(x);

  if val < 0.0 then
    val := 0.0;

  if val > 1.0 then
    val := 1.0;

  Result := val;

end;

{ BOX }
procedure gamma_spline.box(x1, y1, x2, y2: double);
begin
  m_x1 := x1;
  m_y1 := y1;
  m_x2 := x2;
  m_y2 := y2;

end;

{ REWIND }
procedure gamma_spline.rewind(path_id: unsigned);
begin
  m_cur_x := 0.0;

end;

{ VERTEX }
function gamma_spline.vertex(x, y: double_ptr): unsigned;
begin
  if m_cur_x = 0.0 then
  begin
    x^ := m_x1;
    y^ := m_y1;

    m_cur_x := m_cur_x + (1.0 / (m_x2 - m_x1));
    Result := path_cmd_move_to;

    exit;

  end;

  if m_cur_x > 1.0 then
  begin
    Result := path_cmd_stop;

    exit;

  end;

  x^ := m_x1 + m_cur_x * (m_x2 - m_x1);
  y^ := m_y1 + _y(m_cur_x) * (m_y2 - m_y1);

  m_cur_x := m_cur_x + (1.0 / (m_x2 - m_x1));
  Result := path_cmd_line_to;

end;

end.







