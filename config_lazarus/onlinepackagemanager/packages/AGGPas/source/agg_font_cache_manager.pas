
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}
unit agg_font_cache_manager;

interface

{$I agg_mode.inc }

uses
  SysUtils,
  agg_basics,
  agg_array,
  agg_font_engine,
  agg_path_storage_integer;

const
  glyph_data_invalid = 0;
  glyph_data_mono = 1;
  glyph_data_gray8 = 2;
  glyph_data_outline = 3;

  block_size = 16384 - 16;

type
  glyph_cache_ptr_ptr = ^glyph_cache_ptr;
  glyph_cache_ptr = ^glyph_cache;

  glyph_cache = record
    glyph_index: unsigned;

    Data: int8u_ptr;

    data_size,
    data_type: unsigned;//: int8u;

    bounds: rect;

    advance_x,
    advance_y: double;

  end;

  font_cache_ptr_ptr = ^font_cache_ptr;
  font_cache_ptr = ^font_cache;

  font_cache = object
    m_allocator: pod_allocator;
    m_glyphs: array[0..256] of glyph_cache_ptr_ptr;
    m_font_signature: PChar;

    constructor Construct(font_signature: PChar);
    destructor Destruct;

    function font_is(font_signature: PChar): boolean;

    function find_glyph(glyph_code: unsigned): glyph_cache_ptr;

    function cache_glyph(glyph_code, glyph_index, data_size: unsigned;
      data_type: int8u; bounds: rect_ptr; advance_x, advance_y: double): glyph_cache_ptr;

  end;

  font_cache_pool = object
    m_fonts: font_cache_ptr_ptr;
    m_max_fonts, m_num_fonts: unsigned;
    m_cur_font: font_cache_ptr;

    constructor Construct(max_fonts: unsigned = 32);
    destructor Destruct;

    procedure font_(font_signature: PChar; reset_cache: boolean = False);
    function _font: font_cache_ptr;

    function find_glyph(glyph_code: unsigned): glyph_cache_ptr;

    function cache_glyph(glyph_code, glyph_index, data_size: unsigned;
      data_type: int8u; bounds: rect_ptr; advance_x, advance_y: double): glyph_cache_ptr;

    function find_font(font_signature: PChar): int;

  end;

  glyph_rendering = (
    glyph_ren_native_mono,
    glyph_ren_native_gray8,
    glyph_ren_outline,
    glyph_ren_agg_mono,
    glyph_ren_agg_gray8);

  font_cache_manager_ptr = ^font_cache_manager;

  font_cache_manager = object
    m_fonts: font_cache_pool;
    m_engine: font_engine_ptr;

    m_change_stamp: int;

    m_dx, m_dy: double;

    m_prev_glyph, m_last_glyph: glyph_cache_ptr;

    m_path_adaptor: path_adaptor_type_ptr;
    m_gray8_adaptor: gray8_adaptor_type;
    m_gray8_scanline: gray8_scanline_type;
    m_mono_adaptor: mono_adaptor_type;
    m_mono_scanline: mono_scanline_type;

    constructor Construct(engine: font_engine_ptr; max_fonts: unsigned = 32);
    destructor Destruct;

    function glyph(glyph_code: unsigned): glyph_cache_ptr;

    procedure init_embedded_adaptors(gl: glyph_cache_ptr; x, y: double; scale: double = 1.0);

    function path_adaptor: path_adaptor_type_ptr;
    function gray8_adaptor: gray8_adaptor_type_ptr;
    function gray8_scanline: gray8_scanline_type_ptr;
    function mono_adaptor: mono_adaptor_type_ptr;
    function mono_scanline: mono_scanline_type_ptr;

    function prev_glyph: glyph_cache_ptr;
    function last_glyph: glyph_cache_ptr;

    function add_kerning(x, y: double_ptr): boolean;

    procedure precache(from, to_: unsigned);
    procedure reset_cache;
    procedure synchronize;

  end;


implementation

constructor font_cache.Construct(font_signature: PChar);
begin
  m_allocator.Construct(block_size);

  m_font_signature := PChar(m_allocator.allocate(StrLen(font_signature) + 1));

  StrCopy(m_font_signature, font_signature);
  fillchar(m_glyphs, sizeof(m_glyphs), 0);

end;

{ DESTRUCT }
destructor font_cache.Destruct;
begin
  m_allocator.Destruct;

end;

{ FONT_IS }
function font_cache.font_is(font_signature: PChar): boolean;
begin
  Result := StrComp(font_signature, m_font_signature) = 0;

end;

{ FIND_GLYPH }
function font_cache.find_glyph(glyph_code: unsigned): glyph_cache_ptr;
var
  msb: unsigned;

begin
  msb := (glyph_code shr 8) and $FF;

  if m_glyphs[msb] <> nil then
    Result :=
      glyph_cache_ptr_ptr(ptrcomp(m_glyphs[msb]) + (glyph_code and $FF) * sizeof(glyph_cache_ptr))^
  else
    Result := nil;

end;

{ CACHE_GLYPH }
function font_cache.cache_glyph(glyph_code, glyph_index, data_size: unsigned;
                                data_type: int8u; bounds: rect_ptr; advance_x, advance_y: double): glyph_cache_ptr;
var
  msb, lsb: unsigned;

  glyph: glyph_cache_ptr;

begin
  msb := (glyph_code shr 8) and $FF;

  if m_glyphs[msb] = nil then
  begin
    m_glyphs[msb] := glyph_cache_ptr_ptr(m_allocator.allocate(sizeof(glyph_cache_ptr) * 256, sizeof(glyph_cache_ptr)));

    fillchar(m_glyphs[msb]^, sizeof(glyph_cache_ptr) * 256, 0);

  end;

  lsb := glyph_code and $FF;

  if glyph_cache_ptr_ptr(ptrcomp(m_glyphs[msb]) + lsb * sizeof(glyph_cache_ptr))^ <> nil then
  begin
    Result := nil; // Already exists, do not overwrite

    exit;

  end;

  glyph := glyph_cache_ptr(m_allocator.allocate(sizeof(glyph_cache), sizeof(double)));

  glyph^.glyph_index := glyph_index;
  glyph^.Data := m_allocator.allocate(data_size);
  glyph^.data_size := data_size;
  glyph^.data_type := data_type;
  glyph^.bounds := bounds^;
  glyph^.advance_x := advance_x;
  glyph^.advance_y := advance_y;

  glyph_cache_ptr_ptr(ptrcomp(m_glyphs[msb]) + lsb * sizeof(glyph_cache_ptr))^ := glyph;

  Result := glyph;

end;

{ CONSTRUCT }
constructor font_cache_pool.Construct(max_fonts: unsigned = 32);
begin
  agg_getmem(pointer(m_fonts), max_fonts * sizeof(font_cache_ptr));

  m_max_fonts := max_fonts;
  m_num_fonts := 0;
  m_cur_font := nil;

end;

{ DESTRUCT }
destructor font_cache_pool.Destruct;
var
  i: unsigned;

  fnt: font_cache_ptr_ptr;

begin
  fnt := m_fonts;
  i := 0;

  while i < m_num_fonts do
  begin
    dispose(fnt^, Destruct);

    Inc(ptrcomp(fnt), sizeof(font_cache_ptr));
    Inc(i);

  end;

  agg_freemem(pointer(m_fonts), m_max_fonts * sizeof(font_cache_ptr));

end;

{ FONT_ }
procedure font_cache_pool.font_(font_signature: PChar; reset_cache: boolean = False);
var
  idx: int;
  fnt: font_cache_ptr_ptr;

begin
  idx := find_font(font_signature);

  if idx >= 0 then
  begin
    fnt := font_cache_ptr_ptr(ptrcomp(m_fonts) + idx * sizeof(font_cache_ptr));

    if reset_cache then
    begin
      dispose(fnt^, Destruct);

      fnt^ := new(font_cache_ptr, Construct(font_signature));

    end;

    m_cur_font := fnt^;

  end
  else
  begin
    if m_num_fonts >= m_max_fonts then
    begin
      fnt := font_cache_ptr_ptr(ptrcomp(m_fonts) + 1 * sizeof(font_cache_ptr));

      dispose(m_fonts^, Destruct);
      move(fnt^, m_fonts^, (m_max_fonts - 1) * sizeof(font_cache_ptr));

      m_num_fonts := m_max_fonts - 1;

    end;

    fnt := font_cache_ptr_ptr(ptrcomp(m_fonts) + m_num_fonts * sizeof(font_cache_ptr));
    fnt^ := new(font_cache_ptr, Construct(font_signature));

    m_cur_font := fnt^;

    Inc(m_num_fonts);

  end;

end;

{ _FONT }
function font_cache_pool._font: font_cache_ptr;
begin
  Result := m_cur_font;

end;

{ FIND_GLYPH }
function font_cache_pool.find_glyph(glyph_code: unsigned): glyph_cache_ptr;
begin
  if m_cur_font <> nil then
    Result := m_cur_font^.find_glyph(glyph_code)
  else
    Result := nil;

end;

{ CACHE_GLYPH }
function font_cache_pool.cache_glyph(glyph_code, glyph_index, data_size: unsigned;
      data_type: int8u; bounds: rect_ptr; advance_x, advance_y: double): glyph_cache_ptr;
begin
  if m_cur_font <> nil then
    Result :=
      m_cur_font^.cache_glyph(glyph_code, glyph_index, data_size, data_type, bounds, advance_x, advance_y)
  else
    Result := nil;

end;

{ FIND_FONT }
function font_cache_pool.find_font(font_signature: PChar): int;
var
  i: unsigned;
  f: font_cache_ptr_ptr;

begin
  i := 0;
  f := m_fonts;

  while i < m_num_fonts do
  begin
    if f^^.font_is(font_signature) then
    begin
      Result := i;

      exit;

    end;

    Inc(ptrcomp(f), sizeof(font_cache_ptr));
    Inc(i);

  end;

  Result := -1;

end;

{ CONSTRUCT }
constructor font_cache_manager.Construct(engine: font_engine_ptr; max_fonts: unsigned);
begin
  m_fonts.Construct(max_fonts);

  m_engine := engine;

  m_change_stamp := -1;

  m_prev_glyph := nil;
  m_last_glyph := nil;

  if m_engine^.flag32 then
    m_path_adaptor := new(serialized_int32_path_adaptor_ptr, Construct)
  else
    m_path_adaptor := new(serialized_int16_path_adaptor_ptr, Construct);

  m_gray8_adaptor.Construct;
  m_gray8_scanline.Construct(m_gray8_adaptor.m_sz);
  m_mono_adaptor.Construct;
  m_mono_scanline.Construct;

end;

{ DESTRUCT }
destructor font_cache_manager.Destruct;
begin
  m_fonts.Destruct;

  dispose(m_path_adaptor, Destruct);

end;

{ GLYPH }
function font_cache_manager.glyph(glyph_code: unsigned): glyph_cache_ptr;
var
  gl: glyph_cache_ptr;

begin
  synchronize;

  gl := m_fonts.find_glyph(glyph_code);

  if gl <> nil then
  begin
    m_prev_glyph := m_last_glyph;
    m_last_glyph := gl;

    Result := gl;

    exit;

  end
  else
  if m_engine^.prepare_glyph(glyph_code) then
  begin
    m_prev_glyph := m_last_glyph;
    m_last_glyph :=
      m_fonts.cache_glyph(glyph_code, m_engine^.glyph_index, m_engine^.data_size, m_engine^.data_type,
      m_engine^.bounds, m_engine^.advance_x, m_engine^.advance_y);

    m_engine^.write_glyph_to(m_last_glyph^.Data);

    Result := m_last_glyph;

    exit;

  end;

  Result := nil;

end;

{ INIT_EMBEDDED_ADAPTORS }
procedure font_cache_manager.init_embedded_adaptors(gl: glyph_cache_ptr; x, y: double; scale: double = 1.0);
begin
  if gl <> nil then
    case gl^.data_type of
      glyph_data_mono:
        m_mono_adaptor.init(gl^.Data, gl^.data_size, x, y);

      glyph_data_gray8:
        m_gray8_adaptor.init(gl^.Data, gl^.data_size, x, y);

      glyph_data_outline:
        m_path_adaptor^.init(gl^.Data, gl^.data_size, x, y, scale);

    end;

end;

{ PATH_ADAPTOR }
function font_cache_manager.path_adaptor: path_adaptor_type_ptr;
begin
  Result := m_path_adaptor;

end;

{ GRAY8_ADAPTOR }
function font_cache_manager.gray8_adaptor: gray8_adaptor_type_ptr;
begin
  Result := @m_gray8_adaptor;

end;

{ GRAY8_SCANLINE }
function font_cache_manager.gray8_scanline: gray8_scanline_type_ptr;
begin
  Result := @m_gray8_scanline;

end;

{ MONO_ADAPTOR }
function font_cache_manager.mono_adaptor: mono_adaptor_type_ptr;
begin
  Result := @m_mono_adaptor;

end;

{ MONO_SCANLINE }
function font_cache_manager.mono_scanline: mono_scanline_type_ptr;
begin
  Result := @m_mono_scanline;

end;

{ PREV_GLYPH }
function font_cache_manager.prev_glyph: glyph_cache_ptr;
begin
  Result := m_prev_glyph;

end;

{ LAST_GLYPH }
function font_cache_manager.last_glyph: glyph_cache_ptr;
begin
  Result := @m_last_glyph;

end;

{ ADD_KERNING }
function font_cache_manager.add_kerning(x, y: double_ptr): boolean;
begin
  if (m_prev_glyph <> nil) and (m_last_glyph <> nil) then
    Result :=
      m_engine^.add_kerning(m_prev_glyph^.glyph_index, m_last_glyph^.glyph_index, x, y)
  else
    Result := False;

end;

{ PRECACHE }
procedure font_cache_manager.precache(from, to_: unsigned);
begin
  while from <= to_ do
  begin
    glyph(from);
    Inc(from);

  end;

end;

{ RESET_CACHE }
procedure font_cache_manager.reset_cache;
begin
  m_fonts.font_(m_engine^.font_signature, True);

  m_change_stamp := m_engine^.change_stamp;

  m_prev_glyph := nil;
  m_last_glyph := nil;

end;

{ SYNCHRONIZE }
procedure font_cache_manager.synchronize;
begin
  if m_change_stamp <> m_engine^.change_stamp then
  begin
    m_fonts.font_(m_engine^.font_signature);

    m_change_stamp := m_engine^.change_stamp;

    m_prev_glyph := nil;
    m_last_glyph := nil;

  end;

end;

end.

