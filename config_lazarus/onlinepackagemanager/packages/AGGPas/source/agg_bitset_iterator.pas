
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit
agg_bitset_iterator;

interface

{$I agg_mode.inc }

uses agg_basics;

type

  bitset_iterator = object
    m_bits: int8u_ptr;
    m_mask: int8u;
    constructor Construct(bits: int8u_ptr; offset: unsigned = 0);
    procedure inc_operator;
    function bit: unsigned;
  end;

implementation

constructor bitset_iterator.Construct(bits: int8u_ptr; offset: unsigned = 0);
begin
  m_bits := int8u_ptr(ptrcomp(bits) + (offset shr 3) * sizeof(int8u));
  m_mask := ($80 shr (offset and 7));
end;

procedure bitset_iterator.inc_operator;
begin
  m_mask := m_mask shr 1;

  if m_mask = 0 then
  begin
    Inc(ptrcomp(m_bits), sizeof(int8u));

    m_mask := $80;

  end;
end;

function bitset_iterator.bit: unsigned;
begin
  Result := m_bits^ and m_mask;
end;

end.

