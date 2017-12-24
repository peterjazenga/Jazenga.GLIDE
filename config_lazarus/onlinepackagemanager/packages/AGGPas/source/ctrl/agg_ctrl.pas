
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}
unit agg_ctrl;

interface

{$I agg_mode.inc }

uses
  agg_basics,
  agg_trans_affine,
  agg_rasterizer_scanline_aa,
  agg_scanline,
  agg_renderer_scanline,
  agg_render_scanlines,
  agg_vertex_source,
  agg_color;

type

  ctrl_ptr = ^ctrl;

  ctrl = object(vertex_source)
    m_x1, m_y1, m_x2, m_y2: double;
    m_flip_y: boolean;
    m_mtx: trans_affine_ptr;
    constructor Construct(x1, y1, x2, y2: double; flip_y: boolean);
    destructor Destruct; virtual;
    function in_rect(x, y: double): boolean; virtual;
    function on_mouse_button_down(x, y: double): boolean; virtual;
    function on_mouse_button_up(x, y: double): boolean; virtual;
    function on_mouse_move(x, y: double; button_flag: boolean): boolean; virtual;
    function on_arrow_keys(left, right, down, up: boolean): boolean; virtual;
    procedure transform(mtx: trans_affine_ptr);
    procedure no_transform;
    procedure transform_xy(x, y: double_ptr);
    procedure inverse_transform_xy(x, y: double_ptr);
    function scale: double;
    function _color(i: unsigned): aggclr_ptr; virtual;
  end;


procedure render_ctrl(ras: rasterizer_scanline_ptr; sl: scanline_ptr; r: renderer_scanline_ptr; c: ctrl_ptr);


implementation

constructor ctrl.Construct(x1, y1, x2, y2: double; flip_y: boolean);
begin
  inherited Construct;

  m_x1 := x1;
  m_y1 := y1;
  m_x2 := x2;
  m_y2 := y2;

  m_flip_y := flip_y;

  m_mtx := nil;

end;

{ DESTRUCT }
destructor ctrl.Destruct;
begin
  inherited Destruct;

end;

{ IN_RECT }
function ctrl.in_rect(x, y: double): boolean;
begin
  Result := False;

end;

{ ON_MOUSE_BUTTON_DOWN }
function ctrl.on_mouse_button_down(x, y: double): boolean;
begin
  Result := False;

end;

{ ON_MOUSE_BUTTON_UP }
function ctrl.on_mouse_button_up(x, y: double): boolean;
begin
  Result := False;

end;

{ ON_MOUSE_MOVE }
function ctrl.on_mouse_move(x, y: double; button_flag: boolean): boolean;
begin
  Result := False;

end;

{ ON_ARROW_KEYS }
function ctrl.on_arrow_keys(left, right, down, up: boolean): boolean;
begin
  Result := False;

end;

{ TRANSFORM }
procedure ctrl.transform(mtx: trans_affine_ptr);
begin
  m_mtx := mtx;

end;

{ NO_TRANSFORM }
procedure ctrl.no_transform;
begin
  m_mtx := nil;

end;

{ TRANSFORM_XY }
procedure ctrl.transform_xy(x, y: double_ptr);
begin
  if m_flip_y then
    y^ := m_y1 + m_y2 - y^;

  if m_mtx <> nil then
    m_mtx^.transform(m_mtx, x, y);

end;

{ INVERSE_TRANSFORM_XY }
procedure ctrl.inverse_transform_xy(x, y: double_ptr);
begin
  if m_mtx <> nil then
    m_mtx^.inverse_transform(m_mtx, x, y);

  if m_flip_y then
    y^ := m_y1 + m_y2 - y^;

end;

{ SCALE }
function ctrl.scale: double;
begin
  if m_mtx <> nil then
    Result := m_mtx^.scale
  else
    Result := 1.0;

end;

{ _COLOR }
function ctrl._color(i: unsigned): aggclr_ptr;
begin
  Result := nil;

end;

{ RENDER_CTRL }
procedure render_ctrl(ras: rasterizer_scanline_ptr; sl: scanline_ptr; r: renderer_scanline_ptr; c: ctrl_ptr);
var
  i: unsigned;

begin
  if c^.num_paths > 0 then
    for i := 0 to c^.num_paths - 1 do
    begin
      ras^.reset;
      ras^.add_path(c, i);

      r^.color_(c^._color(i));

      render_scanlines(ras, sl, r);

    end;

end;

end.



