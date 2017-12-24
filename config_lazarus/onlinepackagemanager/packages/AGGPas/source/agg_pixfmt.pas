
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}
unit
agg_pixfmt;

interface

{$I agg_mode.inc }
{$Q- }
{$R- }
uses
  agg_basics,
  agg_rendering_buffer,
  agg_color;

{ TYPES DEFINITION }
type
  pixel_formats_ptr = ^pixel_formats;

  func_blender = procedure(this: pixel_formats_ptr; op: unsigned; p: int8u_ptr; cr, cg, cb, ca, cover: unsigned);
  func_blend_pix = procedure(this: pixel_formats_ptr; p: int8u_ptr; cr, cg, cb, alpha, cover: unsigned);

  func_copy_pixel = procedure(this: pixel_formats_ptr; x, y: int; c: aggclr_ptr);
  func_blend_pixel = procedure(this: pixel_formats_ptr; x, y: int; c: aggclr_ptr; cover: int8u);

  func_pixel = function(this: pixel_formats_ptr; x, y: int): aggclr;
  func_row = function(this: pixel_formats_ptr; x, y: int): row_data_type;

  func_copy_hline = procedure(this: pixel_formats_ptr; x, y: int; len: unsigned; c: aggclr_ptr);
  func_copy_vline = procedure(this: pixel_formats_ptr; x, y: int; len: unsigned; c: aggclr_ptr);

  func_blend_hline = procedure(this: pixel_formats_ptr; x, y: int; len: unsigned; c: aggclr_ptr; cover: int8u);
  func_blend_vline = procedure(this: pixel_formats_ptr; x, y: int; len: unsigned; c: aggclr_ptr; cover: int8u);

  func_blend_solid_hspan = procedure(this: pixel_formats_ptr; x, y: int; len: unsigned; c: aggclr_ptr; covers: int8u_ptr);
  func_blend_solid_vspan = procedure(this: pixel_formats_ptr; x, y: int; len: unsigned; c: aggclr_ptr; covers: int8u_ptr);

  func_copy_color_hspan = procedure(this: pixel_formats_ptr; x, y: int; len: unsigned; colors: aggclr_ptr);
  func_copy_color_vspan = procedure(this: pixel_formats_ptr; x, y: int; len: unsigned; colors: aggclr_ptr);

  func_blend_color_hspan = procedure(this: pixel_formats_ptr; x, y: int; len: unsigned; colors: aggclr_ptr; covers: int8u_ptr; cover: int8u);
  func_blend_color_vspan = procedure(this: pixel_formats_ptr; x, y: int; len: unsigned; colors: aggclr_ptr; covers: int8u_ptr; cover: int8u);

  func_copy_from = procedure(this: pixel_formats_ptr; from: rendering_buffer_ptr; xdst, ydst, xsrc, ysrc: int; len: unsigned);
  func_blend_from = procedure(this: pixel_formats_ptr; from: pixel_formats_ptr; psrc_: int8u_ptr; xdst, ydst, xsrc, ysrc: int;
    len: unsigned; cover: int8u);

  func_blend_from_color = procedure(this: pixel_formats_ptr; from: pixel_formats_ptr; color: aggclr_ptr;
    xdst, ydst, xsrc, ysrc: int; len: unsigned; cover: int8u);
  func_blend_from_lut = procedure(this: pixel_formats_ptr; from: pixel_formats_ptr; color_lut: aggclr_ptr;
    xdst, ydst, xsrc, ysrc: int; len: unsigned; cover: int8u);

  func_apply_gamma = procedure(this: pixel_formats_ptr; p: int8u_ptr);
  func_for_each_pixel = procedure(this: pixel_formats_ptr; f: func_apply_gamma);

  pixel_formats = object
    m_rbuf: rendering_buffer_ptr;
    m_gamma, m_apply: gamma_ptr;
    m_order: order_type;

    m_comp_op, m_step, m_offset, m_pix_width: unsigned;

    blender: func_blender;

    copy_pixel: func_copy_pixel;
    blend_pixel: func_blend_pixel;

    pixel: func_pixel;
    row: func_row;

    copy_hline: func_copy_hline;
    copy_vline: func_copy_vline;

    blend_hline: func_blend_hline;
    blend_vline: func_blend_vline;

    blend_solid_hspan: func_blend_solid_hspan;
    blend_solid_vspan: func_blend_solid_vspan;

    copy_color_hspan: func_copy_color_hspan;
    copy_color_vspan: func_copy_color_vspan;

    blend_color_hspan: func_blend_color_hspan;
    blend_color_vspan: func_blend_color_vspan;

    copy_from: func_copy_from;
    blend_from: func_blend_from;

    blend_from_color: func_blend_from_color;
    blend_from_lut: func_blend_from_lut;

    for_each_pixel: func_for_each_pixel;
    gamma_dir_apply, gamma_inv_apply: func_apply_gamma;

    pixel_premultiply, pixel_demultiply: func_apply_gamma;

    constructor Construct(rb: rendering_buffer_ptr; st: unsigned = 1; off: unsigned = 0);

    function attach(pixf: pixel_formats_ptr; x1, y1, x2, y2: int): boolean;
    function pix_ptr(x, y: int): int8u_ptr;
    function row_ptr(y: int): int8u_ptr;

    function _width: unsigned; virtual;
    function _height: unsigned; virtual;
    function _stride: int;
    function _pix_width: unsigned;

    procedure apply_gamma_dir(g: gamma_ptr; order: order_type);
    procedure apply_gamma_inv(g: gamma_ptr; order: order_type);

    procedure comp_op_(op: unsigned);
    function _comp_op: unsigned;

    procedure premultiply;
    procedure demultiply;

  end;

  define_pixfmt = procedure(var pixf: pixel_formats; rb: rendering_buffer_ptr);
  define_pixfmt_gamma = procedure(var pixf: pixel_formats; rb: rendering_buffer_ptr; g: gamma_ptr);
  define_pixfmt_blender = procedure(var pixf: pixel_formats; rb: rendering_buffer_ptr; bl: func_blender; order: order_type);

  comp_op_e = (

    comp_op_clear,         //----comp_op_clear
    comp_op_src,           //----comp_op_src
    comp_op_dst,           //----comp_op_dst
    comp_op_src_over,      //----comp_op_src_over
    comp_op_dst_over,      //----comp_op_dst_over
    comp_op_src_in,        //----comp_op_src_in
    comp_op_dst_in,        //----comp_op_dst_in
    comp_op_src_out,       //----comp_op_src_out
    comp_op_dst_out,       //----comp_op_dst_out
    comp_op_src_atop,      //----comp_op_src_atop
    comp_op_dst_atop,      //----comp_op_dst_atop
    comp_op_xor,           //----comp_op_xor
    comp_op_plus,          //----comp_op_plus
    comp_op_minus,         //----comp_op_minus
    comp_op_multiply,      //----comp_op_multiply
    comp_op_screen,        //----comp_op_screen
    comp_op_overlay,       //----comp_op_overlay
    comp_op_darken,        //----comp_op_darken
    comp_op_lighten,       //----comp_op_lighten
    comp_op_color_dodge,   //----comp_op_color_dodge
    comp_op_color_burn,    //----comp_op_color_burn
    comp_op_hard_light,    //----comp_op_hard_light
    comp_op_soft_light,    //----comp_op_soft_light
    comp_op_difference,    //----comp_op_difference
    comp_op_exclusion,     //----comp_op_exclusion
    comp_op_contrast,      //----comp_op_contrast
    comp_op_invert,        //----comp_op_invert
    comp_op_invert_rgb,    //----comp_op_invert_rgb

    end_of_comp_op_e);


procedure pixfmt_undefined(var pixf: pixel_formats);

implementation

constructor pixel_formats.Construct(rb: rendering_buffer_ptr; st: unsigned = 1; off: unsigned = 0);
begin
  m_rbuf := rb;
  m_gamma := nil;
  m_apply := nil;
  m_order := bgra_order;

  m_comp_op := 3;
  m_step := st;
  m_offset := off;

  m_pix_width := 0;

  blender := nil;

  copy_pixel := nil;
  blend_pixel := nil;

  pixel := nil;
  row := nil;

  copy_hline := nil;
  copy_vline := nil;

  blend_hline := nil;
  blend_vline := nil;

  blend_solid_hspan := nil;
  blend_solid_vspan := nil;

  copy_color_hspan := nil;
  copy_color_vspan := nil;

  blend_color_hspan := nil;
  blend_color_vspan := nil;

  copy_from := nil;
  blend_from := nil;

  blend_from_color := nil;
  blend_from_lut := nil;

  for_each_pixel := nil;
  gamma_dir_apply := nil;
  gamma_inv_apply := nil;

  pixel_premultiply := nil;
  pixel_demultiply := nil;

end;


function pixel_formats.attach(pixf: pixel_formats_ptr; x1, y1, x2, y2: int): boolean;
var
  r, c: rect_i;

  stride, y: int;

begin
  r.Construct(x1, y1, x2, y2);
  c.Construct(0, 0, pixf^._width - 1, pixf^._height - 1);

  if r.clip(@c) then
  begin
    stride := pixf^.m_rbuf^._stride;

    if stride < 0 then
      y := r.y2
    else
      y := r.y1;

    m_rbuf^.attach(
      pixf^.pix_ptr(r.x1, y),
      (r.x2 - r.x1) + 1,
      (r.y2 - r.y1) + 1,
      stride);

    Result := True;

  end
  else
    Result := False;

end;

{ PIX_PTR }
function pixel_formats.pix_ptr(x, y: int): int8u_ptr;
begin
  Result := int8u_ptr(ptrcomp(m_rbuf^.row(y)) + x * m_pix_width + m_offset);

end;

{ ROW_PTR }
function pixel_formats.row_ptr(y: int): int8u_ptr;
begin
  Result := m_rbuf^.row(y);

end;

{ _WIDTH }
function pixel_formats._width: unsigned;
begin
  Result := m_rbuf^._width;

end;

{ _HEIGHT }
function pixel_formats._height: unsigned;
begin
  Result := m_rbuf^._height;

end;

{ _STRIDE }
function pixel_formats._stride: int;
begin
  Result := m_rbuf^._stride;

end;

{ _PIX_WIDTH }
function pixel_formats._pix_width: unsigned;
begin
  Result := m_pix_width;

end;

{ APPLY_GAMMA_DIR }
procedure pixel_formats.apply_gamma_dir(g: gamma_ptr; order: order_type);
begin
  m_apply := g;
  m_order := order;

  for_each_pixel(@self, gamma_dir_apply );

end;

{ APPLY_GAMMA_INV }
procedure pixel_formats.apply_gamma_inv(g: gamma_ptr; order: order_type);
begin
  m_apply := g;
  m_order := order;

 // for_each_pixel(@self, func_apply_gamma(@gamma_inv_apply));
  for_each_pixel(@self, gamma_inv_apply);

end;

{ COMP_OP_ }
procedure pixel_formats.comp_op_(op: unsigned);
begin
  m_comp_op := op;

end;

{ _COMP_OP }
function pixel_formats._comp_op: unsigned;
begin
  Result := m_comp_op;

end;

{ PREMULTIPLY }
procedure pixel_formats.premultiply;
begin
  for_each_pixel(@self, func_apply_gamma(@pixel_premultiply));

end;

{ DEMULTIPLY }
procedure pixel_formats.demultiply;
begin
  for_each_pixel(@self, func_apply_gamma(@pixel_demultiply));

end;

{ PIXFMT_UNDEFINED }
procedure pixfmt_undefined(var pixf: pixel_formats);
begin
  pixf.Construct(nil);

  pixf.copy_pixel := nil;
  pixf.blend_pixel := nil;

  pixf.pixel := nil;
  pixf.row := nil;

  pixf.copy_hline := nil;
  pixf.copy_vline := nil;

  pixf.blend_hline := nil;
  pixf.blend_vline := nil;

  pixf.blend_solid_hspan := nil;
  pixf.blend_solid_vspan := nil;

  pixf.copy_color_hspan := nil;
  pixf.copy_color_vspan := nil;

  pixf.blend_color_hspan := nil;
  pixf.blend_color_vspan := nil;

  pixf.copy_from := nil;
  pixf.blend_from := nil;

  pixf.blend_from_color := nil;
  pixf.blend_from_lut := nil;

  pixf.for_each_pixel := nil;
  pixf.gamma_dir_apply := nil;
  pixf.gamma_inv_apply := nil;

end;

end.




