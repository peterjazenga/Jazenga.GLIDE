
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}
unit
agg_pixfmt_transposer;

interface

{$I agg_mode.inc }

uses
  agg_basics,
  agg_pixfmt,
  agg_color,
  agg_rendering_buffer;

{ TYPES DEFINITION }
type
  pixel_formats_transposer_ptr = ^pixel_formats_transposer;

  pixel_formats_transposer = object(pixel_formats)
  private
    m_pixf: pixel_formats_ptr;

  public
    constructor Construct(src: pixel_formats_ptr);

    procedure attach(src: pixel_formats_ptr);

    function _width: unsigned; virtual;
    function _height: unsigned; virtual;

  end;

procedure pixfmt_transposer(var pixf: pixel_formats_transposer; src: pixel_formats_ptr);

implementation

constructor pixel_formats_transposer.Construct(src : pixel_formats_ptr );
begin
  inherited Construct(nil);

  attach(src);

end;

{ ATTACH }
procedure pixel_formats_transposer.attach(src : pixel_formats_ptr );
begin
  m_pixf := src;
  m_rbuf := src^.m_rbuf;

  m_gamma := src^.m_gamma;
  m_apply := src^.m_apply;
  m_order := src^.m_order;

  m_comp_op := src^.m_comp_op;
  m_step := src^.m_step;
  m_offset := src^.m_offset;
  m_pix_width := src^.m_pix_width;

  blender := src^.blender;
  row := src^.row;

  copy_from := src^.copy_from;
  blend_from := src^.blend_from;

  for_each_pixel := src^.for_each_pixel;
  gamma_dir_apply := src^.gamma_dir_apply;
  gamma_inv_apply := src^.gamma_inv_apply;

end;

{ _WIDTH }
function pixel_formats_transposer._width: unsigned;
begin
  Result := m_pixf^._height;

end;

{ _HEIGHT }
function pixel_formats_transposer._height: unsigned;
begin
  Result := m_pixf^._width;

end;

{ transposer_copy_pixel }
procedure transposer_copy_pixel(this: pixel_formats_transposer_ptr; x, y: int; c: aggclr_ptr);
begin
  this^.m_pixf^.copy_pixel(this^.m_pixf, y, x, c);

end;

{ transposer_blend_pixel }
procedure transposer_blend_pixel(this: pixel_formats_transposer_ptr; x, y: int; c: aggclr_ptr; cover: int8u);
begin
  this^.m_pixf^.blend_pixel(this^.m_pixf, y, x, c, cover);

end;

{ transposer_pixel }
function transposer_pixel(this: pixel_formats_transposer_ptr; x, y: int): aggclr;
begin
  Result := this^.m_pixf^.pixel(this^.m_pixf, y, x);

end;

{ transposer_copy_hline }
procedure transposer_copy_hline(this: pixel_formats_transposer_ptr; x, y: int; len: unsigned; c: aggclr_ptr);
begin
  this^.m_pixf^.copy_vline(this^.m_pixf, y, x, len, c);

end;

{ transposer_copy_vline }
procedure transposer_copy_vline(this: pixel_formats_transposer_ptr; x, y: int; len: unsigned; c: aggclr_ptr);
begin
  this^.m_pixf^.copy_hline(this^.m_pixf, y, x, len, c);

end;

{ transposer_blend_hline }
procedure transposer_blend_hline(this: pixel_formats_transposer_ptr; x, y: int; len: unsigned; c: aggclr_ptr; cover: int8u);
begin
  this^.m_pixf^.blend_vline(this^.m_pixf, y, x, len, c, cover);

end;

{ transposer_blend_vline }
procedure transposer_blend_vline(this: pixel_formats_transposer_ptr; x, y: int; len: unsigned; c: aggclr_ptr; cover: int8u);
begin
  this^.m_pixf^.blend_hline(this^.m_pixf, y, x, len, c, cover);

end;

{ transposer_blend_solid_hspan }
procedure transposer_blend_solid_hspan(this: pixel_formats_transposer_ptr; x, y: int; len: unsigned; c: aggclr_ptr; covers: int8u_ptr);
begin
  this^.m_pixf^.blend_solid_vspan(this^.m_pixf, y, x, len, c, covers);

end;

{ transposer_blend_solid_vspan }
procedure transposer_blend_solid_vspan(this: pixel_formats_transposer_ptr; x, y: int; len: unsigned; c: aggclr_ptr; covers: int8u_ptr);
begin
  this^.m_pixf^.blend_solid_hspan(this^.m_pixf, y, x, len, c, covers);

end;

{ transposer_copy_color_hspan }
procedure transposer_copy_color_hspan(this: pixel_formats_transposer_ptr; x, y: int; len: unsigned; colors: aggclr_ptr);
begin
  this^.m_pixf^.copy_color_vspan(this^.m_pixf, y, x, len, colors);

end;

{ transposer_copy_color_vspan }
procedure transposer_copy_color_vspan(this: pixel_formats_transposer_ptr; x, y: int; len: unsigned; colors: aggclr_ptr);
begin
  this^.m_pixf^.copy_color_hspan(this^.m_pixf, y, x, len, colors);

end;

{ transposer_blend_color_hspan }
procedure transposer_blend_color_hspan(this: pixel_formats_transposer_ptr; x, y: int; len: unsigned; colors: aggclr_ptr;
  covers: int8u_ptr; cover: int8u);
begin
  this^.m_pixf^.blend_color_vspan(this^.m_pixf, y, x, len, colors, covers, cover);

end;

{ transposer_blend_color_vspan }
procedure transposer_blend_color_vspan(this: pixel_formats_transposer_ptr; x, y: int; len: unsigned; colors: aggclr_ptr;
  covers: int8u_ptr; cover: int8u);
begin
  this^.m_pixf^.blend_color_hspan(this^.m_pixf, y, x, len, colors, covers, cover);

end;

{ PIXFMT_TRANSPOSER }
procedure pixfmt_transposer(var pixf: pixel_formats_transposer; src: pixel_formats_ptr);
begin
  pixf.Construct(src);

  Pointer(pixf.copy_pixel) := @transposer_copy_pixel;
  Pointer(pixf.blend_pixel) := @transposer_blend_pixel;

  Pointer(pixf.pixel) := @transposer_pixel;

  Pointer(pixf.copy_hline) := @transposer_copy_hline;
  Pointer(pixf.copy_vline) := @transposer_copy_vline;

  Pointer(pixf.blend_hline) := @transposer_blend_hline;
  Pointer(pixf.blend_vline) := @transposer_blend_vline;

  Pointer(pixf.blend_solid_hspan) := @transposer_blend_solid_hspan;
  Pointer(pixf.blend_solid_vspan) := @transposer_blend_solid_vspan;

  Pointer(pixf.copy_color_hspan) := @transposer_copy_color_hspan;
  Pointer(pixf.copy_color_vspan) := @transposer_copy_color_vspan;

  Pointer(pixf.blend_color_hspan) := @transposer_blend_color_hspan;
  Pointer(pixf.blend_color_vspan) := @transposer_blend_color_vspan;

end;

end.
