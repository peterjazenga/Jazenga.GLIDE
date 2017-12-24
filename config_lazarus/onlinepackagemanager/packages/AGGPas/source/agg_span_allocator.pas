
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}
unit
agg_span_allocator;

interface

{$I agg_mode.inc }

uses
  agg_basics,
  agg_color;

type
  span_allocator_ptr = ^span_allocator;

  span_allocator = object
    m_max_span_len: unsigned;

    m_span: aggclr_ptr;

    constructor Construct;
    destructor Destruct;

    function allocate(max_span_len: unsigned): aggclr_ptr;
    function span: aggclr_ptr;

  end;


implementation

constructor span_allocator.Construct;
begin
  m_max_span_len := 0;

  m_span := nil;

end;

{ DESTRUCT }
destructor span_allocator.Destruct;
begin
  agg_freemem(pointer(m_span), m_max_span_len * sizeof(aggclr));

end;

{ ALLOCATE }
function span_allocator.allocate(max_span_len: unsigned): aggclr_ptr;
begin
  if max_span_len > m_max_span_len then
  begin
    agg_freemem(pointer(m_span), m_max_span_len * sizeof(aggclr));

    // To reduce the number of reallocs we align the
    // span_len to 256 color elements.
    // Well, I just like this number and it looks reasonable.
    max_span_len := ((max_span_len + 255) shr 8) shl 8;

    agg_getmem(pointer(m_span), max_span_len * sizeof(aggclr));

    m_max_span_len := max_span_len;

  end;

  Result := m_span;

end;

{ SPAN }
function span_allocator.span: aggclr_ptr;
begin
  Result := m_span;

end;

end.
