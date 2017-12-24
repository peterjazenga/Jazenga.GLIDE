
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}
unit
agg_span_solid;

interface

{$I agg_mode.inc }

uses
  agg_basics,
  agg_span_allocator,
  agg_span_generator,
  agg_color;

type
  span_solid_ptr = ^span_solid;

  span_solid = object(span_generator)
    m_color: aggclr;

    constructor Construct(alloc: span_allocator_ptr);

    procedure color_(c: aggclr_ptr);
    function _color: aggclr_ptr;

    function generate(x, y: int; len: unsigned): aggclr_ptr; virtual;

  end;



implementation

constructor span_solid.Construct(alloc: span_allocator_ptr);
begin
  inherited Construct(alloc);

  m_color.Construct;

end;

{ COLOR_ }
procedure span_solid.color_(c: aggclr_ptr);
begin
  m_color := c^;

end;

{ _COLOR }
function span_solid._color: aggclr_ptr;
begin
  Result := @m_color;

end;

{ GENERATE }
function span_solid.generate(x, y: int; len: unsigned): aggclr_ptr;
var
  span: aggclr_ptr;

begin
  span := _allocator^.span;

  repeat
    span^ := m_color;

    Inc(ptrcomp(span), sizeof(aggclr));
    Dec(len);

  until len = 0;

  Result := _allocator^.span;

end;

end.

