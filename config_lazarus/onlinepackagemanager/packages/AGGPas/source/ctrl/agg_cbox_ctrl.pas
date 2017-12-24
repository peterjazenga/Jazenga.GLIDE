
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}
unit agg_cbox_ctrl;

interface

{$I agg_mode.inc }

uses
  SysUtils,
  agg_basics,
  agg_ctrl,
  agg_conv_stroke,
  agg_gsv_text,
  agg_color,
  agg_math_stroke;

type

  cbox_ctrl_impl = object(ctrl)
    m_text_thickness, m_text_height, m_text_width: double;
    m_label: array[0..127] of char;
    m_status: boolean;
    m_vx, m_vy: array[0..31] of double;
    m_text: gsv_text;
    m_text_poly: conv_stroke;
    m_idx, m_vertex: unsigned;
    constructor Construct(x, y: double; l: PChar; flip_y: boolean = False);
    destructor Destruct; virtual;
    procedure text_thickness_(t: double);
    procedure text_size_(h: double; w: double = 0);
    function _label: PChar;
    procedure label_(l: PChar);
    function _status: boolean;
    procedure status_(st: boolean);
    function in_rect(x, y: double): boolean; virtual;
    function on_mouse_button_down(x, y: double): boolean; virtual;
    function on_mouse_button_up(x, y: double): boolean; virtual;
    function on_mouse_move(x, y: double; button_flag: boolean): boolean; virtual;
    function on_arrow_keys(left, right, down, up: boolean): boolean; virtual;
    // Vertex source interface
    function num_paths: unsigned; virtual;
    procedure rewind(path_id: unsigned); virtual;
    function vertex(x, y: double_ptr): unsigned; virtual;
  end;

  cbox_ctrl_ptr = ^cbox_ctrl;

  cbox_ctrl = object(cbox_ctrl_impl)
    m_text_color, m_inactive_color, m_active_color: aggclr;
    m_colors: array[0..2] of aggclr_ptr;
    constructor Construct(x, y: double; l: PChar; flip_y: boolean = False);
    procedure text_color_(c: aggclr_ptr);
    procedure inactive_color_(c: aggclr_ptr);
    procedure active_color_(c: aggclr_ptr);
    function _color(i: unsigned): aggclr_ptr; virtual;
  end;


implementation

constructor cbox_ctrl_impl.Construct(x, y: double; l: PChar; flip_y: boolean = False);
begin
  inherited Construct(x, y, x + 9.0 * 1.5, y + 9.0 * 1.5, flip_y);

  m_text.Construct;
  m_text_poly.Construct(@m_text);

  m_text_thickness := 1.5;
  m_text_height := 9.0;
  m_text_width := 0.0;

  m_status := False;

  label_(l);

end;

{ DESTRUCT }
destructor cbox_ctrl_impl.Destruct;
begin
  m_text_poly.Destruct;
  m_text.Destruct;

end;

{ TEXT_THICKNESS_ }
procedure cbox_ctrl_impl.text_thickness_(t: double);
begin
  m_text_thickness := t;

end;

{ TEXT_SIZE_ }
procedure cbox_ctrl_impl.text_size_(h: double; w: double = 0);
begin
  m_text_width := w;
  m_text_height := h;

end;

{ _LABEL }
function cbox_ctrl_impl._label: PChar;
begin
  Result := @m_label[0];

end;

{ LABEL_ }
procedure cbox_ctrl_impl.label_(l: PChar);
var
  len: unsigned;

begin
  len := StrLen(l);

  if len > 127 then
    len := 127;

  move(l^, m_label[0], len);

  m_label[len] := #0;

end;

{ _STATUS }
function cbox_ctrl_impl._status: boolean;
begin
  Result := m_status;

end;

{ STATUS_ }
procedure cbox_ctrl_impl.status_(st: boolean);
begin
  m_status := st;

end;

{ IN_RECT }
function cbox_ctrl_impl.in_rect(x, y: double): boolean;
begin
  inverse_transform_xy(@x, @y);

  Result :=
    (x >= m_x1) and (y >= m_y1) and (x <= m_x2) and (y <= m_y2);

end;

{ ON_MOUSE_BUTTON_DOWN }
function cbox_ctrl_impl.on_mouse_button_down(x, y: double): boolean;
begin
  inverse_transform_xy(@x, @y);

  if (x >= m_x1) and (y >= m_y1) and (x <= m_x2) and (y <= m_y2) then
  begin
    m_status := not m_status;

    Result := True;

  end
  else
    Result := False;

end;

{ ON_MOUSE_BUTTON_UP }
function cbox_ctrl_impl.on_mouse_button_up(x, y: double): boolean;
begin
  Result := False;

end;

{ ON_MOUSE_MOVE }
function cbox_ctrl_impl.on_mouse_move(x, y: double; button_flag: boolean): boolean;
begin
  Result := False;

end;

{ ON_ARROW_KEYS }
function cbox_ctrl_impl.on_arrow_keys(left, right, down, up: boolean): boolean;
begin
  Result := False;

end;

{ NUM_PATHS }
function cbox_ctrl_impl.num_paths: unsigned;
begin
  Result := 3;

end;

{ REWIND }
procedure cbox_ctrl_impl.rewind(path_id: unsigned);
var
  d2, t: double;

begin
  m_idx := path_id;

  case path_id of
    0: // Border
    begin
      m_vertex := 0;

      m_vx[0] := m_x1;
      m_vy[0] := m_y1;
      m_vx[1] := m_x2;
      m_vy[1] := m_y1;
      m_vx[2] := m_x2;
      m_vy[2] := m_y2;
      m_vx[3] := m_x1;
      m_vy[3] := m_y2;
      m_vx[4] := m_x1 + m_text_thickness;
      m_vy[4] := m_y1 + m_text_thickness;
      m_vx[5] := m_x1 + m_text_thickness;
      m_vy[5] := m_y2 - m_text_thickness;
      m_vx[6] := m_x2 - m_text_thickness;
      m_vy[6] := m_y2 - m_text_thickness;
      m_vx[7] := m_x2 - m_text_thickness;
      m_vy[7] := m_y1 + m_text_thickness;

    end;

    1: // Text
    begin
      m_text.text_(@m_label[0]);
      m_text.start_point_(m_x1 + m_text_height * 2.0, m_y1 + m_text_height / 5.0);
      m_text.size_(m_text_height, m_text_width);

      m_text_poly.width_(m_text_thickness);
      m_text_poly.line_join_(round_join);
      m_text_poly.line_cap_(round_cap);

      m_text_poly.rewind(0);

    end;

    2: // Active item
    begin
      m_vertex := 0;

      d2 := (m_y2 - m_y1) / 2.0;
      t := m_text_thickness * 1.5;

      m_vx[0] := m_x1 + m_text_thickness;
      m_vy[0] := m_y1 + m_text_thickness;
      m_vx[1] := m_x1 + d2;
      m_vy[1] := m_y1 + d2 - t;
      m_vx[2] := m_x2 - m_text_thickness;
      m_vy[2] := m_y1 + m_text_thickness;
      m_vx[3] := m_x1 + d2 + t;
      m_vy[3] := m_y1 + d2;
      m_vx[4] := m_x2 - m_text_thickness;
      m_vy[4] := m_y2 - m_text_thickness;
      m_vx[5] := m_x1 + d2;
      m_vy[5] := m_y1 + d2 + t;
      m_vx[6] := m_x1 + m_text_thickness;
      m_vy[6] := m_y2 - m_text_thickness;
      m_vx[7] := m_x1 + d2 - t;
      m_vy[7] := m_y1 + d2;

    end;

  end;

end;

{ VERTEX }
function cbox_ctrl_impl.vertex(x, y: double_ptr): unsigned;
var
  cmd: unsigned;

begin
  cmd := path_cmd_line_to;

  case m_idx of
    0:
    begin
      if (m_vertex = 0) or (m_vertex = 4) then
        cmd := path_cmd_move_to;

      if m_vertex >= 8 then
        cmd := path_cmd_stop;

      x^ := m_vx[m_vertex];
      y^ := m_vy[m_vertex];

      Inc(m_vertex);

    end;

    1:
      cmd := m_text_poly.vertex(x, y);

    2:
      if m_status then
      begin
        if m_vertex = 0 then
          cmd := path_cmd_move_to;

        if m_vertex >= 8 then
          cmd := path_cmd_stop;

        x^ := m_vx[m_vertex];
        y^ := m_vy[m_vertex];

        Inc(m_vertex);

      end
      else
        cmd := path_cmd_stop;

    else
      cmd := path_cmd_stop;

  end;

  if not is_stop(cmd) then
    transform_xy(x, y);

  Result := cmd;

end;

{ CONSTRUCT }
constructor cbox_ctrl.Construct(x, y: double; l: PChar; flip_y: boolean = False);
begin
  inherited Construct(x, y, l, flip_y);

  m_text_color.ConstrDbl(0.0, 0.0, 0.0);
  m_inactive_color.ConstrDbl(0.0, 0.0, 0.0);
  m_active_color.ConstrDbl(0.4, 0.0, 0.0);

  m_colors[0] := @m_inactive_color;
  m_colors[1] := @m_text_color;
  m_colors[2] := @m_active_color;

end;

{ TEXT_COLOR_ }
procedure cbox_ctrl.text_color_(c: aggclr_ptr);
begin
  m_text_color := c^;

end;

{ INACTIVE_COLOR_ }
procedure cbox_ctrl.inactive_color_(c: aggclr_ptr);
begin
  m_inactive_color := c^;

end;

{ ACTIVE_COLOR_ }
procedure cbox_ctrl.active_color_(c: aggclr_ptr);
begin
  m_active_color := c^;

end;

{ _COLOR }
function cbox_ctrl._color(i: unsigned): aggclr_ptr;
begin
  Result := m_colors[i];

end;

end.

