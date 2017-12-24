
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}
unit
agg_pixfmt_amask_adaptor;

interface

{$I agg_mode.inc }

uses
  agg_basics,
  agg_color,
  agg_rendering_buffer,
  agg_pixfmt,
  agg_alpha_mask_u8;

{ TYPES DEFINITION }
const
  span_extra_tail = 256;

type
  pixfmt_amask_adaptor_ptr = ^pixfmt_amask_adaptor;

  pixfmt_amask_adaptor = object(pixel_formats)
    m_pixf: pixel_formats_ptr;
    m_mask: alpha_mask_ptr;

    m_span: int8u_ptr;
    m_max_len: unsigned;

    constructor Construct(pixf: pixel_formats_ptr; mask: alpha_mask_ptr);
    destructor Destruct;

    procedure realloc_span(len: unsigned);

    procedure init_span(len: unsigned); overload;
    procedure init_span(len: unsigned; covers: int8u_ptr); overload;

  end;

implementation

procedure _copy_hline(this: pixfmt_amask_adaptor_ptr; x, y: int; len: unsigned; c: aggclr_ptr);
begin
  this^.realloc_span(len);
  this^.m_mask^.fill_hspan(x, y, this^.m_span, len);
  this^.m_pixf^.blend_solid_hspan(this^.m_pixf, x, y, len, c, this^.m_span);

end;

{ _blend_hline }
procedure _blend_hline(this: pixfmt_amask_adaptor_ptr; x, y: int; len: unsigned; c: aggclr_ptr; cover: int8u);
begin
  this^.init_span(len);
  this^.m_mask^.combine_hspan(x, y, this^.m_span, len);
  this^.m_pixf^.blend_solid_hspan(this^.m_pixf, x, y, len, c, this^.m_span);

end;

{ _blend_vline }
procedure _blend_vline(this: pixfmt_amask_adaptor_ptr; x, y: int; len: unsigned; c: aggclr_ptr; cover: int8u);
begin
  this^.init_span(len);
  this^.m_mask^.combine_vspan(x, y, this^.m_span, len);
  this^.m_pixf^.blend_solid_vspan(this^.m_pixf, x, y, len, c, this^.m_span);

end;

{ _blend_solid_hspan }
procedure _blend_solid_hspan(this: pixfmt_amask_adaptor_ptr; x, y: int; len: unsigned; c: aggclr_ptr; covers: int8u_ptr);
begin
  this^.init_span(len, covers);
  this^.m_mask^.combine_hspan(x, y, this^.m_span, len);
  this^.m_pixf^.blend_solid_hspan(this^.m_pixf, x, y, len, c, this^.m_span);

end;

{ _blend_solid_vspan }
procedure _blend_solid_vspan(this: pixfmt_amask_adaptor_ptr; x, y: int; len: unsigned; c: aggclr_ptr; covers: int8u_ptr);
begin
  this^.init_span(len, covers);
  this^.m_mask^.combine_vspan(x, y, this^.m_span, len);
  this^.m_pixf^.blend_solid_vspan(this^.m_pixf, x, y, len, c, this^.m_span);

end;

{ _blend_color_hspan }
procedure _blend_color_hspan(this: pixfmt_amask_adaptor_ptr; x, y: int; len: unsigned; colors: aggclr_ptr; covers: int8u_ptr; cover: int8u);
begin
  if covers <> nil then
  begin
    this^.init_span(len, covers);
    this^.m_mask^.combine_hspan(x, y, this^.m_span, len);

  end
  else
  begin
    this^.realloc_span(len);
    this^.m_mask^.fill_hspan(x, y, this^.m_span, len);

  end;

  this^.m_pixf^.blend_color_hspan(this^.m_pixf, x, y, len, colors, this^.m_span, cover);

end;

{ _blend_color_vspan }
procedure _blend_color_vspan(this: pixfmt_amask_adaptor_ptr; x, y: int; len: unsigned; colors: aggclr_ptr; covers: int8u_ptr; cover: int8u);
begin
  if covers <> nil then
  begin
    this^.init_span(len, covers);
    this^.m_mask^.combine_vspan(x, y, this^.m_span, len);

  end
  else
  begin
    this^.realloc_span(len);
    this^.m_mask^.fill_vspan(x, y, this^.m_span, len);

  end;

  this^.m_pixf^.blend_color_vspan(this^.m_pixf, x, y, len, colors, this^.m_span, cover);

end;

{ _blend_pixel }
procedure _blend_pixel(this: pixfmt_amask_adaptor_ptr; x, y: int; c: pointer; cover: int8u);
begin
  this^.m_pixf^.blend_pixel(this^.m_pixf, x, y, c, this^.m_mask^.combine_pixel(x, y, cover));

end;

{ CONSTRUCT }
constructor pixfmt_amask_adaptor.Construct(pixf: pixel_formats_ptr; mask: alpha_mask_ptr);
begin
  inherited Construct(pixf^.m_rbuf);

  m_pixf := pixf;
  m_mask := mask;

  m_span := nil;
  m_max_len := 0;

  Pointer(copy_hline) := @_copy_hline;
  Pointer(blend_hline) := @_blend_hline;
  Pointer(blend_vline) := @_blend_vline;

  Pointer(blend_solid_hspan) := @_blend_solid_hspan;
  Pointer(blend_solid_vspan) := @_blend_solid_vspan;
  Pointer(blend_color_hspan) := @_blend_color_hspan;
  Pointer(blend_color_vspan) := @_blend_color_vspan;

  Pointer(blend_pixel) := @_blend_pixel;

end;

{ DESTRUCT }
destructor pixfmt_amask_adaptor.Destruct;
begin
  agg_freemem(pointer(m_span), m_max_len * sizeof(int8u));

end;

{ REALLOC_SPAN }
procedure pixfmt_amask_adaptor.realloc_span(len: unsigned);
begin
  if len > m_max_len then
  begin
    agg_freemem(pointer(m_span), m_max_len * sizeof(int8u));

    m_max_len := len + span_extra_tail;

    agg_getmem(pointer(m_span), m_max_len * sizeof(int8u));

  end;

end;

{ INIT_SPAN }
procedure pixfmt_amask_adaptor.init_span(len: unsigned);
begin
  realloc_span(len);

  fillchar(m_span^, len * sizeof(int8u), cover_full);

end;

{ INIT_SPAN }
procedure pixfmt_amask_adaptor.init_span(len: unsigned; covers: int8u_ptr);
begin
  realloc_span(len);

  move(covers^, m_span^, len * sizeof(int8u));

end;

end.
