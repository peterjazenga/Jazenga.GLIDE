
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}
unit
agg_scanline_p;

interface

{$I agg_mode.inc }

uses
  agg_basics,
  agg_scanline;

type
  span_p8_ptr = ^span_p8;

  span_p8 = record
    x,
    len: int16; // If negative, it's a solid span, covers is valid

    covers: int8u_ptr;

  end;

  scanline_p8_ptr = ^scanline_p8;

  scanline_p8 = object(scanline)
    m_max_len: unsigned;
    m_last_x, m_y: int;

    m_covers, m_cover_ptr: int8u_ptr;

    m_spans, m_cur_span: span_p8_ptr;

    constructor Construct;
    destructor Destruct;

    procedure reset(min_x, max_x: int); virtual;
    procedure reset_spans; virtual;

    procedure finalize(y_: int); virtual;
    procedure add_cell(x: int; cover: unsigned); virtual;
    procedure add_cells(x: int; len: unsigned; covers: int8u_ptr); virtual;
    procedure add_span(x: int; len, cover: unsigned); virtual;

    function y: int; virtual;
    function num_spans: unsigned; virtual;
    function begin_: pointer; virtual;

    function sz_of_span: unsigned; virtual;

  end;


implementation

constructor scanline_p8.Construct;
begin
  m_max_len := 0;
  m_last_x := $7FFFFFF0;

  m_y := 0;

  m_covers := nil;
  m_cover_ptr := nil;

  m_spans := nil;
  m_cur_span := nil;

end;


destructor scanline_p8.Destruct;
begin
  agg_freemem(pointer(m_spans), m_max_len * sizeof(span_p8));
  agg_freemem(pointer(m_covers), m_max_len * sizeof(int8u));

end;


procedure scanline_p8.reset(min_x, max_x: int);
var
  max_len: unsigned;

begin
  max_len := max_x - min_x + 3;

  if max_len > m_max_len then
  begin
    agg_freemem(pointer(m_covers), m_max_len * sizeof(int8u));
    agg_freemem(pointer(m_spans), m_max_len * sizeof(span_p8));

    agg_getmem(pointer(m_covers), max_len * sizeof(int8u));
    agg_getmem(pointer(m_spans), max_len * sizeof(span_p8));

    m_max_len := max_len;

  end;

  m_last_x := $7FFFFFF0;

  m_cover_ptr := m_covers;
  m_cur_span := m_spans;

  m_cur_span^.len := 0;

end;


procedure scanline_p8.reset_spans;
begin
  m_last_x := $7FFFFFF0;

  m_cover_ptr := m_covers;
  m_cur_span := m_spans;

  m_cur_span^.len := 0;

end;


procedure scanline_p8.finalize(y_: int);
begin
  m_y := y_;

end;

procedure scanline_p8.add_cell(x: int; cover: unsigned);
begin
  m_cover_ptr^ := int8u(cover);

  if (x = m_last_x + 1) and (m_cur_span^.len > 0) then
    Inc(m_cur_span^.len)

  else
  begin
    Inc(ptrcomp(m_cur_span), sizeof(span_p8));

    m_cur_span^.covers := m_cover_ptr;

    m_cur_span^.x := int16(x);
    m_cur_span^.len := 1;

  end;

  m_last_x := x;

  Inc(ptrcomp(m_cover_ptr), sizeof(int8u));

end;


procedure scanline_p8.add_cells(x: int; len: unsigned; covers: int8u_ptr);
begin
  move(covers^, m_cover_ptr^, len * sizeof(int8u));

  if (x = m_last_x + 1) and (m_cur_span^.len > 0) then
    Inc(m_cur_span^.len, int16(len))

  else
  begin
    Inc(ptrcomp(m_cur_span), sizeof(span_p8));

    m_cur_span^.covers := m_cover_ptr;
    m_cur_span^.x := int16(x);
    m_cur_span^.len := int16(len);

  end;

  Inc(ptrcomp(m_cover_ptr), len * sizeof(int8u));

  m_last_x := x + len - 1;

end;


procedure scanline_p8.add_span(x: int; len, cover: unsigned);
begin
  if (x = m_last_x + 1) and (m_cur_span^.len < 0) and (cover = m_cur_span^.covers^) then
    Dec(m_cur_span^.len, int16(len))

  else
  begin
    m_cover_ptr^ := int8u(cover);

    Inc(ptrcomp(m_cur_span), sizeof(span_p8));

    m_cur_span^.covers := m_cover_ptr;
    m_cur_span^.x := int16(x);
    m_cur_span^.len := int16(len);
    m_cur_span^.len := -m_cur_span^.len;

    Inc(ptrcomp(m_cover_ptr), sizeof(int8u));

  end;

  m_last_x := x + len - 1;

end;

function scanline_p8.y: int;
begin
  Result := m_y;

end;


function scanline_p8.num_spans: unsigned;
begin
  Result := (ptrcomp(m_cur_span) - ptrcomp(m_spans)) div sizeof(span_p8);

end;


function scanline_p8.begin_: pointer;
begin
  Result := span_p8_ptr(ptrcomp(m_spans) + sizeof(span_p8));

end;


function scanline_p8.sz_of_span: unsigned;
begin
  Result := sizeof(span_p8);

end;

end.


