
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}
unit
agg_renderer_mclip;

interface

{$I agg_mode.inc }

uses
  agg_basics,
  agg_array,
  agg_color,
  agg_rendering_buffer,
  agg_renderer_base,
  agg_pixfmt;

type
  renderer_mclip = object(renderer_base)
    m_clip: pod_deque;
    m_curr_cb: unsigned;
    m_bounds: rect;

    constructor Construct(ren_: pixel_formats_ptr);
    destructor Destruct;

    function bounding_clip_box: rect_ptr; virtual;
    function bounding_xmin: int; virtual;
    function bounding_ymin: int; virtual;
    function bounding_xmax: int; virtual;
    function bounding_ymax: int; virtual;

    procedure first_clip_box; virtual;
    function next_clip_box: boolean; virtual;

    procedure reset_clipping(visibility: boolean); virtual;

    procedure add_clip_box(x1, y1, x2, y2: int);

    procedure copy_pixel(x, y: int; c: aggclr_ptr); virtual;
    procedure blend_pixel(x, y: int; c: aggclr_ptr; cover: int8u); virtual;
    function pixel(x, y: int): aggclr; virtual;

    procedure copy_hline(x1, y, x2: int; c: aggclr_ptr); virtual;
    procedure copy_vline(x, y1, y2: int; c: aggclr_ptr); virtual;

    procedure blend_hline(x1, y, x2: int; c: aggclr_ptr; cover: int8u); virtual;
    procedure blend_vline(x, y1, y2: int; c: aggclr_ptr; cover: int8u); virtual;

    procedure copy_bar(x1, y1, x2, y2: int; c: aggclr_ptr); virtual;
    procedure blend_bar(x1, y1, x2, y2: int; c: aggclr_ptr; cover: int8u); virtual;

    procedure blend_solid_hspan(x, y, len: int; c: aggclr_ptr; covers: int8u_ptr); virtual;
    procedure blend_solid_vspan(x, y, len: int; c: aggclr_ptr; covers: int8u_ptr); virtual;

    procedure copy_color_hspan(x, y, len: int; colors: aggclr_ptr); virtual;
    procedure blend_color_hspan(x, y, len: int; colors: aggclr_ptr; covers: int8u_ptr; cover: int8u = cover_full); virtual;
    procedure blend_color_vspan(x, y, len: int; colors: aggclr_ptr; covers: int8u_ptr; cover: int8u = cover_full); virtual;

    procedure copy_from(from: rendering_buffer_ptr; rc: rect_ptr = nil; x_to: int = 0; y_to: int = 0); virtual;

  end;



implementation

constructor renderer_mclip.Construct(ren_: pixel_formats_ptr);
begin
  inherited Construct(ren_);

  m_clip.Construct(sizeof(rect), 4);
  m_bounds.Construct(_xmin, _ymin, _xmax, _ymax);

  m_curr_cb := 0;

end;

{ DESTRUCT }
destructor renderer_mclip.Destruct;
begin
  m_clip.Destruct;

end;

{ BOUNDING_CLIP_BOX }
function renderer_mclip.bounding_clip_box: rect_ptr;
begin
  Result := @m_bounds;

end;

{ BOUNDING_XMIN }
function renderer_mclip.bounding_xmin: int;
begin
  Result := m_bounds.x1;

end;

{ BOUNDING_YMIN }
function renderer_mclip.bounding_ymin: int;
begin
  Result := m_bounds.y1;

end;

{ BOUNDING_XMAX }
function renderer_mclip.bounding_xmax: int;
begin
  Result := m_bounds.x2;

end;

{ BOUNDING_YMAX }
function renderer_mclip.bounding_ymax: int;
begin
  Result := m_bounds.y2;

end;

{ FIRST_CLIP_BOX }
procedure renderer_mclip.first_clip_box;
var
  cb: rect_ptr;

begin
  m_curr_cb := 0;

  if m_clip.size <> 0 then
  begin
    cb := m_clip.array_operator(0);

    clip_box_naked(cb^.x1, cb^.y1, cb^.x2, cb^.y2);

  end;

end;

{ NEXT_CLIP_BOX }
function renderer_mclip.next_clip_box: boolean;
var
  cb: rect_ptr;

begin
  Inc(m_curr_cb);

  if m_curr_cb < m_clip.size then
  begin
    cb := m_clip.array_operator(m_curr_cb);

    clip_box_naked(cb^.x1, cb^.y1, cb^.x2, cb^.y2);

    Result := True;

    exit;

  end;

  Result := False;

end;

{ RESET_CLIPPING }
procedure renderer_mclip.reset_clipping(visibility: boolean);
begin
  inherited reset_clipping(visibility);

  m_clip.remove_all;

  m_curr_cb := 0;

  m_bounds.Construct(_clip_box);

end;

{ ADD_CLIP_BOX }
procedure renderer_mclip.add_clip_box(x1, y1, x2, y2: int);
var
  cb, rc: rect;

begin
  cb.Construct(x1, y1, x2, y2);
  cb.normalize;
  rc.Construct(0, 0, Width - 1, Height - 1);

  if cb.clip(@rc) then
  begin
    m_clip.add(@cb);

    if cb.x1 < m_bounds.x1 then
      m_bounds.x1 := cb.x1;

    if cb.y1 < m_bounds.y1 then
      m_bounds.y1 := cb.y1;

    if cb.x2 > m_bounds.x2 then
      m_bounds.x2 := cb.x2;

    if cb.y2 > m_bounds.y2 then
      m_bounds.y2 := cb.y2;

  end;

end;

{ COPY_PIXEL }
procedure renderer_mclip.copy_pixel(x, y: int; c: aggclr_ptr);
begin
  first_clip_box;

  repeat
    if inbox(x, y) then
    begin
      m_ren^.copy_pixel(m_ren, x, y, c);

      break;

    end;

  until not next_clip_box;

end;

{ BLEND_PIXEL }
procedure renderer_mclip.blend_pixel(x, y: int; c: aggclr_ptr; cover: int8u);
begin
  first_clip_box;

  repeat
    if inbox(x, y) then
    begin
      m_ren^.blend_pixel(m_ren, x, y, c, cover);

      break;

    end;

  until not next_clip_box;

end;

{ PIXEL }
function renderer_mclip.pixel(x, y: int): aggclr;
begin
  first_clip_box;

  repeat
    if inbox(x, y) then
    begin
      Result := m_ren^.pixel(m_ren, x, y);

      exit;

    end;

  until not next_clip_box;

  Result.Clear;

end;

{ COPY_HLINE }
procedure renderer_mclip.copy_hline(x1, y, x2: int; c: aggclr_ptr);
begin
  first_clip_box;

  repeat
    inherited copy_hline(x1, y, x2, c);

  until not next_clip_box;

end;

{ COPY_VLINE }
procedure renderer_mclip.copy_vline(x, y1, y2: int; c: aggclr_ptr);
begin
  first_clip_box;

  repeat
    inherited copy_vline(x, y1, y2, c);

  until not next_clip_box;

end;

{ BLEND_HLINE }
procedure renderer_mclip.blend_hline(x1, y, x2: int; c: aggclr_ptr; cover: int8u);
begin
  first_clip_box;

  repeat
    inherited blend_hline(x1, y, x2, c, cover);

  until not next_clip_box;

end;

{ BLEND_VLINE }
procedure renderer_mclip.blend_vline(x, y1, y2: int; c: aggclr_ptr; cover: int8u);
begin
  first_clip_box;

  repeat
    inherited blend_vline(x, y1, y2, c, cover);

  until not next_clip_box;

end;

{ COPY_BAR }
procedure renderer_mclip.copy_bar(x1, y1, x2, y2: int; c: aggclr_ptr);
begin
  first_clip_box;

  repeat
    inherited copy_bar(x1, y1, x2, y2, c);

  until not next_clip_box;

end;

{ BLEND_BAR }
procedure renderer_mclip.blend_bar(x1, y1, x2, y2: int; c: aggclr_ptr; cover: int8u);
begin
  first_clip_box;

  repeat
    inherited blend_bar(x1, y1, x2, y2, c, cover);

  until not next_clip_box;

end;

{ BLEND_SOLID_HSPAN }
procedure renderer_mclip.blend_solid_hspan(x, y, len: int; c: aggclr_ptr; covers: int8u_ptr);
begin
  first_clip_box;

  repeat
    inherited blend_solid_hspan(x, y, len, c, covers);

  until not next_clip_box;

end;

{ BLEND_SOLID_VSPAN }
procedure renderer_mclip.blend_solid_vspan(x, y, len: int; c: aggclr_ptr; covers: int8u_ptr);
begin
  first_clip_box;

  repeat
    inherited blend_solid_vspan(x, y, len, c, covers);

  until not next_clip_box;

end;

{ COPY_COLOR_HSPAN }
procedure renderer_mclip.copy_color_hspan(x, y, len: int; colors: aggclr_ptr);
begin
  first_clip_box;

  repeat
    inherited copy_color_hspan(x, y, len, colors);

  until not next_clip_box;

end;

{ BLEND_COLOR_HSPAN }
procedure renderer_mclip.blend_color_hspan(x, y, len: int; colors: aggclr_ptr; covers: int8u_ptr; cover: int8u = cover_full);
begin
  first_clip_box;

  repeat
    inherited blend_color_hspan(x, y, len, colors, covers, cover);

  until not next_clip_box;

end;

{ BLEND_COLOR_VSPAN }
procedure renderer_mclip.blend_color_vspan(x, y, len: int; colors: aggclr_ptr; covers: int8u_ptr; cover: int8u = cover_full);
begin
  first_clip_box;

  repeat
    inherited blend_color_vspan(x, y, len, colors, covers, cover);

  until not next_clip_box;

end;

{ COPY_FROM }
procedure renderer_mclip.copy_from(from: rendering_buffer_ptr; rc: rect_ptr = nil; x_to: int = 0; y_to: int = 0);
begin
  first_clip_box;

  repeat
    inherited copy_from(from, rc, x_to, y_to);

  until not next_clip_box;

end;

end.


