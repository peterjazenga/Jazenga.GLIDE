
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit agg_color_conv;

interface

{$I agg_mode.inc }

uses
  agg_basics,
  agg_color,
  agg_rendering_buffer;

type
  CopyRow = procedure(dst, src: int8u_ptr; Width: unsigned);


procedure color_conv(dst, src: rendering_buffer_ptr; copy_row_functor: CopyRow);

procedure color_conv_gray8_to_bgr24(dst, src: int8u_ptr; Width: unsigned);
procedure color_conv_gray8_to_rgb24(dst, src: int8u_ptr; Width: unsigned);

procedure color_conv_gray16_to_gray8(dst, src: int8u_ptr; Width: unsigned);

procedure color_conv_rgb555_to_rgb555(dst, src: int8u_ptr; Width: unsigned);
procedure color_conv_rgb555_to_rgb565(dst, src: int8u_ptr; Width: unsigned);
procedure color_conv_rgb555_to_rgb24(dst, src: int8u_ptr; Width: unsigned);
procedure color_conv_rgb555_to_bgr24(dst, src: int8u_ptr; Width: unsigned);
procedure color_conv_rgb555_to_abgr32(dst, src: int8u_ptr; Width: unsigned);
procedure color_conv_rgb555_to_argb32(dst, src: int8u_ptr; Width: unsigned);
procedure color_conv_rgb555_to_bgra32(dst, src: int8u_ptr; Width: unsigned);
procedure color_conv_rgb555_to_rgba32(dst, src: int8u_ptr; Width: unsigned);

procedure color_conv_rgb565_to_rgb555(dst, src: int8u_ptr; Width: unsigned);

procedure color_conv_bgr24_to_gray8(dst, src: int8u_ptr; Width: unsigned);
procedure color_conv_bgr24_to_gray16(dst, src: int8u_ptr; Width: unsigned);
procedure color_conv_bgr24_to_rgb555(dst, src: int8u_ptr; Width: unsigned);
procedure color_conv_bgr24_to_rgb565(dst, src: int8u_ptr; Width: unsigned);
procedure color_conv_bgr24_to_rgb24(dst, src: int8u_ptr; Width: unsigned);
procedure color_conv_bgr24_to_bgr24(dst, src: int8u_ptr; Width: unsigned);
procedure color_conv_bgr24_to_abgr32(dst, src: int8u_ptr; Width: unsigned);
procedure color_conv_bgr24_to_argb32(dst, src: int8u_ptr; Width: unsigned);
procedure color_conv_bgr24_to_bgra32(dst, src: int8u_ptr; Width: unsigned);
procedure color_conv_bgr24_to_rgba32(dst, src: int8u_ptr; Width: unsigned);
procedure color_conv_bgr24_to_rgb48(dst, src: int8u_ptr; Width: unsigned);
procedure color_conv_bgr24_to_bgr48(dst, src: int8u_ptr; Width: unsigned);
procedure color_conv_bgr24_to_abgr64(dst, src: int8u_ptr; Width: unsigned);
procedure color_conv_bgr24_to_argb64(dst, src: int8u_ptr; Width: unsigned);
procedure color_conv_bgr24_to_bgra64(dst, src: int8u_ptr; Width: unsigned);
procedure color_conv_bgr24_to_rgba64(dst, src: int8u_ptr; Width: unsigned);

procedure color_conv_rgb24_to_bgr24(dst, src: int8u_ptr; Width: unsigned);
procedure color_conv_rgb24_to_bgra32(dst, src: int8u_ptr; Width: unsigned);

procedure color_conv_bgra32_to_rgb555(dst, src: int8u_ptr; Width: unsigned);
procedure color_conv_bgra32_to_rgb565(dst, src: int8u_ptr; Width: unsigned);
procedure color_conv_bgra32_to_rgb24(dst, src: int8u_ptr; Width: unsigned);
procedure color_conv_bgra32_to_bgr24(dst, src: int8u_ptr; Width: unsigned);
procedure color_conv_bgra32_to_abgr32(dst, src: int8u_ptr; Width: unsigned);
procedure color_conv_bgra32_to_argb32(dst, src: int8u_ptr; Width: unsigned);
procedure color_conv_bgra32_to_bgra32(dst, src: int8u_ptr; Width: unsigned);
procedure color_conv_bgra32_to_rgba32(dst, src: int8u_ptr; Width: unsigned);
procedure color_conv_bgra64_to_bgra32(dst, src: int8u_ptr; Width: unsigned);

procedure color_conv_abgr32_to_argb32(dst, src: int8u_ptr; Width: unsigned);
procedure color_conv_abgr32_to_bgra32(dst, src: int8u_ptr; Width: unsigned);
procedure color_conv_abgr64_to_bgra32(dst, src: int8u_ptr; Width: unsigned);

procedure color_conv_rgba32_to_argb32(dst, src: int8u_ptr; Width: unsigned);
procedure color_conv_rgba32_to_bgra32(dst, src: int8u_ptr; Width: unsigned);

procedure color_conv_argb32_to_bgra32(dst, src: int8u_ptr; Width: unsigned);
procedure color_conv_argb64_to_bgra32(dst, src: int8u_ptr; Width: unsigned);

procedure color_conv_rgbAAA_to_bgr24(dst, src: int8u_ptr; Width: unsigned);

procedure color_conv_bgrAAA_to_bgr24(dst, src: int8u_ptr; Width: unsigned);

procedure color_conv_rgbBBA_to_bgr24(dst, src: int8u_ptr; Width: unsigned);

procedure color_conv_bgrABB_to_bgr24(dst, src: int8u_ptr; Width: unsigned);

procedure color_conv_rgb48_to_bgr24(dst, src: int8u_ptr; Width: unsigned);

procedure color_conv_bgr48_to_bgr24(dst, src: int8u_ptr; Width: unsigned);

procedure color_conv_rgba64_to_bgra32(dst, src: int8u_ptr; Width: unsigned);


implementation

procedure color_conv(dst, src: rendering_buffer_ptr; copy_row_functor: CopyRow);
var
  y, Width, Height: unsigned;

begin
  Width := src^._width;
  Height := src^._height;

  if dst^._width < Width then
    Width := dst^._width;

  if dst^._height < Height then
    Height := dst^._height;

  if Width > 0 then
    for y := 0 to Height - 1 do
      copy_row_functor(dst^.row(y), src^.row(y), Width);

end;

//color_conv_bgr24_to_gray8
procedure color_conv_bgr24_to_gray8(dst, src: int8u_ptr; Width: unsigned);
begin
end;

{ color_conv_gray8_to_bgr24 }
procedure color_conv_gray8_to_bgr24(dst, src: int8u_ptr; Width: unsigned);
begin
  repeat
    int8u_ptr(ptrcomp(dst) + bgr_order.R)^ := src^;
    int8u_ptr(ptrcomp(dst) + bgr_order.G)^ := src^;
    int8u_ptr(ptrcomp(dst) + bgr_order.B)^ := src^;

    Inc(ptrcomp(dst), 3);
    Inc(ptrcomp(src));
    Dec(Width);

  until Width = 0;

end;

{ color_conv_gray8_to_rgb24 }
procedure color_conv_gray8_to_rgb24(dst, src: int8u_ptr; Width: unsigned);
begin
  repeat
    int8u_ptr(ptrcomp(dst) + rgb_order.R)^ := src^;
    int8u_ptr(ptrcomp(dst) + rgb_order.G)^ := src^;
    int8u_ptr(ptrcomp(dst) + rgb_order.B)^ := src^;

    Inc(ptrcomp(dst), 3);
    Inc(ptrcomp(src));
    Dec(Width);

  until Width = 0;


end;

{ color_conv_bgr24_to_gray16 }
procedure color_conv_bgr24_to_gray16(dst, src: int8u_ptr; Width: unsigned);
begin
end;

{ color_conv_rgb555_to_rgb555 }
procedure color_conv_rgb555_to_rgb555(dst, src: int8u_ptr; Width: unsigned);
begin
end;

{ color_conv_bgr24_to_rgb555 }
procedure color_conv_bgr24_to_rgb555(dst, src: int8u_ptr; Width: unsigned);
begin
end;

{ color_conv_bgra32_to_rgb555 }
procedure color_conv_bgra32_to_rgb555(dst, src: int8u_ptr; Width: unsigned);
begin
end;

{ color_conv_rgb555_to_rgb565 }
procedure color_conv_rgb555_to_rgb565(dst, src: int8u_ptr; Width: unsigned);
begin
end;

{ color_conv_bgr24_to_rgb565 }
procedure color_conv_bgr24_to_rgb565(dst, src: int8u_ptr; Width: unsigned);
begin
end;

{ color_conv_bgra32_to_rgb565 }
procedure color_conv_bgra32_to_rgb565(dst, src: int8u_ptr; Width: unsigned);
begin
end;

{ color_conv_rgb555_to_rgb24 }
procedure color_conv_rgb555_to_rgb24(dst, src: int8u_ptr; Width: unsigned);
begin
end;

{ color_conv_bgr24_to_rgb24 }
procedure color_conv_bgr24_to_rgb24(dst, src: int8u_ptr; Width: unsigned);
begin
 repeat
  int8u_ptr(ptrcomp(dst ) + bgr_order.R )^:=int8u_ptr(ptrcomp(src ) + rgb_order.R )^;
  int8u_ptr(ptrcomp(dst ) + bgr_order.G )^:=int8u_ptr(ptrcomp(src ) + rgb_order.G )^;
  int8u_ptr(ptrcomp(dst ) + bgr_order.B )^:=int8u_ptr(ptrcomp(src ) + rgb_order.B )^;

  inc(ptrcomp(dst ) ,3 );
  inc(ptrcomp(src ) ,3 );
  dec(width );

 until width = 0;

end;

{ color_conv_bgra32_to_rgb24 }
procedure color_conv_bgra32_to_rgb24(dst, src: int8u_ptr; Width: unsigned);
begin
end;

{ color_conv_rgb555_to_bgr24 }
procedure color_conv_rgb555_to_bgr24(dst, src: int8u_ptr; Width: unsigned);
begin
end;

{ color_conv_bgr24_to_bgr24 }
procedure color_conv_bgr24_to_bgr24(dst, src: int8u_ptr; Width: unsigned);
begin
 move(src^ ,dst^ ,width * 3 );

end;

{ color_conv_bgra32_to_bgr24 }
procedure color_conv_bgra32_to_bgr24(dst, src: int8u_ptr; Width: unsigned);
begin
end;

{ color_conv_bgr24_to_rgb48 }
procedure color_conv_bgr24_to_rgb48(dst, src: int8u_ptr; Width: unsigned);
begin
end;

{ color_conv_bgr24_to_bgr48 }
procedure color_conv_bgr24_to_bgr48(dst, src: int8u_ptr; Width: unsigned);
begin
end;

{ color_conv_rgb555_to_abgr32 }
procedure color_conv_rgb555_to_abgr32(dst, src: int8u_ptr; Width: unsigned);
begin
end;

{ color_conv_bgr24_to_abgr32 }
procedure color_conv_bgr24_to_abgr32(dst, src: int8u_ptr; Width: unsigned);
begin
end;

{ color_conv_bgra32_to_abgr32 }
procedure color_conv_bgra32_to_abgr32(dst, src: int8u_ptr; Width: unsigned);
begin
end;

{ color_conv_rgb555_to_argb32 }
procedure color_conv_rgb555_to_argb32(dst, src: int8u_ptr; Width: unsigned);
begin
end;

{ color_conv_bgr24_to_argb32 }
procedure color_conv_bgr24_to_argb32(dst, src: int8u_ptr; Width: unsigned);
begin
end;

{ color_conv_bgra32_to_argb32 }
procedure color_conv_bgra32_to_argb32(dst, src: int8u_ptr; Width: unsigned);
begin
 repeat
  int8u_ptr(ptrcomp(dst ) + argb_order.R )^:=int8u_ptr(ptrcomp(src ) + bgra_order.R )^;
  int8u_ptr(ptrcomp(dst ) + argb_order.G )^:=int8u_ptr(ptrcomp(src ) + bgra_order.G )^;
  int8u_ptr(ptrcomp(dst ) + argb_order.B )^:=int8u_ptr(ptrcomp(src ) + bgra_order.B )^;
  int8u_ptr(ptrcomp(dst ) + argb_order.A )^:=int8u_ptr(ptrcomp(src ) + bgra_order.A )^;

  inc(ptrcomp(dst ) ,4 );
  inc(ptrcomp(src ) ,4 );
  dec(width );

 until width = 0;

end;

{ color_conv_abgr32_to_argb32 }
procedure color_conv_abgr32_to_argb32(dst, src: int8u_ptr; Width: unsigned);
begin
 repeat
  int8u_ptr(ptrcomp(dst ) + argb_order.R )^:=int8u_ptr(ptrcomp(src ) + abgr_order.R )^;
  int8u_ptr(ptrcomp(dst ) + argb_order.G )^:=int8u_ptr(ptrcomp(src ) + abgr_order.G )^;
  int8u_ptr(ptrcomp(dst ) + argb_order.B )^:=int8u_ptr(ptrcomp(src ) + abgr_order.B )^;
  int8u_ptr(ptrcomp(dst ) + argb_order.A )^:=int8u_ptr(ptrcomp(src ) + abgr_order.A )^;

  inc(ptrcomp(dst ) ,4 );
  inc(ptrcomp(src ) ,4 );
  dec(width );

 until width = 0;

end;

{ color_conv_rgba32_to_argb32 }
procedure color_conv_rgba32_to_argb32(dst, src: int8u_ptr; Width: unsigned);
begin
 repeat
  int8u_ptr(ptrcomp(dst ) + argb_order.R )^:=int8u_ptr(ptrcomp(src ) + rgba_order.R )^;
  int8u_ptr(ptrcomp(dst ) + argb_order.G )^:=int8u_ptr(ptrcomp(src ) + rgba_order.G )^;
  int8u_ptr(ptrcomp(dst ) + argb_order.B )^:=int8u_ptr(ptrcomp(src ) + rgba_order.B )^;
  int8u_ptr(ptrcomp(dst ) + argb_order.A )^:=int8u_ptr(ptrcomp(src ) + rgba_order.A )^;

  inc(ptrcomp(dst ) ,4 );
  inc(ptrcomp(src ) ,4 );
  dec(width );

 until width = 0;
 
end;

{ color_conv_rgb555_to_bgra32 }
procedure color_conv_rgb555_to_bgra32(dst, src: int8u_ptr; Width: unsigned);
begin
end;

{ color_conv_bgr24_to_bgra32 }
procedure color_conv_bgr24_to_bgra32(dst, src: int8u_ptr; Width: unsigned);
begin
 repeat
  int8u_ptr(ptrcomp(dst ) + 0 )^:=src^; inc(ptrcomp(src ) );
  int8u_ptr(ptrcomp(dst ) + 1 )^:=src^; inc(ptrcomp(src ) );
  int8u_ptr(ptrcomp(dst ) + 2 )^:=src^; inc(ptrcomp(src ) );
  int8u_ptr(ptrcomp(dst ) + 3 )^:=255;

  inc(ptrcomp(dst ) ,4 );
  dec(width );

 until width = 0;

end;

{ color_conv_bgra32_to_bgra32 }
procedure color_conv_bgra32_to_bgra32(dst, src: int8u_ptr; Width: unsigned);
begin
end;

{ color_conv_rgb555_to_rgba32 }
procedure color_conv_rgb555_to_rgba32(dst, src: int8u_ptr; Width: unsigned);
begin
end;

{ color_conv_bgr24_to_rgba32 }
procedure color_conv_bgr24_to_rgba32(dst, src: int8u_ptr; Width: unsigned);
begin
end;

{ color_conv_bgra32_to_rgba32 }
procedure color_conv_bgra32_to_rgba32(dst, src: int8u_ptr; Width: unsigned);
begin
end;

{ color_conv_bgr24_to_abgr64 }
procedure color_conv_bgr24_to_abgr64(dst, src: int8u_ptr; Width: unsigned);
begin
end;

{ color_conv_bgr24_to_argb64 }
procedure color_conv_bgr24_to_argb64(dst, src: int8u_ptr; Width: unsigned);
begin
end;

{ color_conv_bgr24_to_bgra64 }
procedure color_conv_bgr24_to_bgra64(dst, src: int8u_ptr; Width: unsigned);
begin
end;

{ color_conv_bgr24_to_rgba64 }
procedure color_conv_bgr24_to_rgba64(dst, src: int8u_ptr; Width: unsigned);
begin
end;

{ color_conv_gray16_to_gray8 }
procedure color_conv_gray16_to_gray8(dst, src: int8u_ptr; Width: unsigned);
begin
end;

{ color_conv_rgb565_to_rgb555 }
procedure color_conv_rgb565_to_rgb555(dst, src: int8u_ptr; Width: unsigned);
var
 rgb : int;

begin
 repeat
  rgb:=int16u(p32(src ).ptr^ );

  int16u(p32(dst ).ptr^ ):=((rgb shr 1 ) and $7FE0 ) or (rgb and $1F );

  inc(ptrcomp(src ) ,2 );
  inc(ptrcomp(dst ) ,2 );
  dec(width );

 until width = 0;

end;

{ color_conv_rgbAAA_to_bgr24 }
procedure color_conv_rgbAAA_to_bgr24(dst, src: int8u_ptr; Width: unsigned);
begin
end;

{ color_conv_bgrAAA_to_bgr24 }
procedure color_conv_bgrAAA_to_bgr24(dst, src: int8u_ptr; Width: unsigned);
begin
end;

{ color_conv_rgbBBA_to_bgr24 }
procedure color_conv_rgbBBA_to_bgr24(dst, src: int8u_ptr; Width: unsigned);
begin
end;

{ color_conv_bgrABB_to_bgr24 }
procedure color_conv_bgrABB_to_bgr24(dst, src: int8u_ptr; Width: unsigned);
begin
end;

{ color_conv_rgb24_to_bgr24 }
procedure color_conv_rgb24_to_bgr24(dst, src: int8u_ptr; Width: unsigned);
begin
 repeat
  int8u_ptr(p32(dst ).int + 0 )^:=int8u_ptr(p32(src ).int + 2 )^;
  int8u_ptr(p32(dst ).int + 1 )^:=int8u_ptr(p32(src ).int + 1 )^;
  int8u_ptr(p32(dst ).int + 2 )^:=int8u_ptr(p32(src ).int + 0 )^;

  inc(ptrcomp(src ) ,3 );
  inc(ptrcomp(dst ) ,3 );
  dec(width );

 until width = 0;

end;

{ color_conv_rgb48_to_bgr24 }
procedure color_conv_rgb48_to_bgr24(dst, src: int8u_ptr; Width: unsigned);
begin
end;

{ color_conv_bgr48_to_bgr24 }
procedure color_conv_bgr48_to_bgr24(dst, src: int8u_ptr; Width: unsigned);
begin
end;

{ color_conv_abgr32_to_bgra32 }
procedure color_conv_abgr32_to_bgra32(dst, src: int8u_ptr; Width: unsigned);
begin
 repeat
  int8u_ptr(p32(dst ).int + 0 )^:=int8u_ptr(p32(src ).int + 1 )^;
  int8u_ptr(p32(dst ).int + 1 )^:=int8u_ptr(p32(src ).int + 2 )^;
  int8u_ptr(p32(dst ).int + 2 )^:=int8u_ptr(p32(src ).int + 3 )^;
  int8u_ptr(p32(dst ).int + 3 )^:=int8u_ptr(p32(src ).int + 0 )^;

  inc(ptrcomp(src ) ,4 );
  inc(ptrcomp(dst ) ,4 );
  dec(width );

 until width = 0;

end;

{ color_conv_argb32_to_bgra32 }
procedure color_conv_argb32_to_bgra32(dst, src: int8u_ptr; Width: unsigned);
begin
 repeat
  int8u_ptr(p32(dst ).int + 0 )^:=int8u_ptr(p32(src ).int + 3 )^;
  int8u_ptr(p32(dst ).int + 1 )^:=int8u_ptr(p32(src ).int + 2 )^;
  int8u_ptr(p32(dst ).int + 2 )^:=int8u_ptr(p32(src ).int + 1 )^;
  int8u_ptr(p32(dst ).int + 3 )^:=int8u_ptr(p32(src ).int + 0 )^;

  inc(ptrcomp(src ) ,4 );
  inc(ptrcomp(dst ) ,4 );
  dec(width );

 until width = 0;

end;

{ color_conv_rgba32_to_bgra32 }
procedure color_conv_rgba32_to_bgra32(dst, src: int8u_ptr; Width: unsigned);
begin
 repeat
  int8u_ptr(p32(dst ).int + 0 )^:=int8u_ptr(p32(src ).int + 2 )^;
  int8u_ptr(p32(dst ).int + 1 )^:=int8u_ptr(p32(src ).int + 1 )^;
  int8u_ptr(p32(dst ).int + 2 )^:=int8u_ptr(p32(src ).int + 0 )^;
  int8u_ptr(p32(dst ).int + 3 )^:=int8u_ptr(p32(src ).int + 3 )^;

  inc(ptrcomp(src ) ,4 );
  inc(ptrcomp(dst ) ,4 );
  dec(width );

 until width = 0;

end;

{ color_conv_bgra64_to_bgra32 }
procedure color_conv_bgra64_to_bgra32(dst, src: int8u_ptr; Width: unsigned);
begin
end;

{ color_conv_abgr64_to_bgra32 }
procedure color_conv_abgr64_to_bgra32(dst, src: int8u_ptr; Width: unsigned);
begin
end;

{ color_conv_argb64_to_bgra32 }
procedure color_conv_argb64_to_bgra32(dst, src: int8u_ptr; Width: unsigned);
begin
end;

{ color_conv_rgba64_to_bgra32 }
procedure color_conv_rgba64_to_bgra32(dst, src: int8u_ptr; Width: unsigned);
begin
end;

{ color_conv_rgb24_to_bgra32 }
procedure color_conv_rgb24_to_bgra32(dst, src: int8u_ptr; Width: unsigned);
begin
 repeat
  int8u_ptr(ptrcomp(dst ) + bgra_order.R )^:=src^; inc(ptrcomp(src ) );
  int8u_ptr(ptrcomp(dst ) + bgra_order.G )^:=src^; inc(ptrcomp(src ) );
  int8u_ptr(ptrcomp(dst ) + bgra_order.B )^:=src^; inc(ptrcomp(src ) );
  int8u_ptr(ptrcomp(dst ) + bgra_order.A )^:=255;

  inc(ptrcomp(dst ) ,4 );
  dec(width );

 until width = 0;

end;

END.

