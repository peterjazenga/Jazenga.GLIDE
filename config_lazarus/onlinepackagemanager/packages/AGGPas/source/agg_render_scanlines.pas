
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}
unit
agg_render_scanlines;

interface

{$I agg_mode.inc }
{$Q- }
{$R- }
uses
  agg_basics,
  agg_color,
  agg_rasterizer_scanline_aa,
  agg_scanline,
  agg_renderer_scanline,
  agg_vertex_source;

procedure render_scanlines(ras: rasterizer_scanline_ptr; sl: scanline_ptr; ren: renderer_scanline_ptr);
procedure render_all_paths(ras: rasterizer_scanline_ptr; sl: scanline_ptr;
                           r: renderer_scanline_ptr; vs: vertex_source_ptr; cs: aggclr_ptr;
                           path_id: unsigned_ptr; num_paths: unsigned);


implementation

procedure render_scanlines(ras: rasterizer_scanline_ptr; sl: scanline_ptr; ren: renderer_scanline_ptr);
begin
  if ras^.rewind_scanlines then
  begin
    sl^.reset(ras^._min_x, ras^._max_x);
    ren^.prepare(unsigned(ras^._max_x - ras^._min_x + 2));

    if sl^.is_embedded then
      while ras^.sweep_scanline_em(sl) do
        ren^.render(sl)
    else
      while ras^.sweep_scanline(sl) do
        ren^.render(sl);

  end;

end;

{ RENDER_ALL_PATHS }
procedure render_all_paths(ras: rasterizer_scanline_ptr; sl: scanline_ptr;
                           r: renderer_scanline_ptr; vs: vertex_source_ptr; cs: aggclr_ptr;
                           path_id: unsigned_ptr; num_paths: unsigned);
var
  i: unsigned;

begin
  i := 0;

  while i < num_paths do
  begin
    ras^.reset;
    ras^.add_path(vs, path_id^);
    r^.color_(cs);

    render_scanlines(ras, sl, r);

    Inc(ptrcomp(cs), sizeof(aggclr));
    Inc(ptrcomp(path_id), sizeof(unsigned));
    Inc(i);

  end;

end;

end.

