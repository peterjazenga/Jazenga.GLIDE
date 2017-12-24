
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}
unit
agg_span_generator;

interface

{$I agg_mode.inc }

uses
  agg_basics,
  agg_span_allocator,
  agg_vertex_source,
  agg_color;

type
  span_generator_ptr = ^span_generator;

  span_generator = object(vertex_source)
    m_alloc: span_allocator_ptr;

    constructor Construct(alloc: span_allocator_ptr);

    procedure allocator_(alloc: span_allocator_ptr);
    function _allocator: span_allocator_ptr;

    procedure prepare(max_span_len: unsigned); virtual;
    function generate(x, y: int; len: unsigned): aggclr_ptr; virtual; abstract;

  end;

implementation

constructor span_generator.Construct(alloc: span_allocator_ptr);
begin
  m_alloc := alloc;
end;

procedure span_generator.allocator_(alloc: span_allocator_ptr);
begin
  m_alloc := alloc;
end;

function span_generator._allocator: span_allocator_ptr;
begin
  Result := m_alloc;
end;

procedure span_generator.prepare(max_span_len: unsigned);
begin
  m_alloc^.allocate(max_span_len);
end;

end.
