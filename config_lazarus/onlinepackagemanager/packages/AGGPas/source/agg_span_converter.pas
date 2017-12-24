
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}
unit
agg_span_converter;

interface

{$I agg_mode.inc }

uses
  agg_basics,
  agg_color,
  agg_span_generator;

type
  span_convertor_ptr = ^span_convertor;

  span_convertor = object
    procedure convert(span: aggclr_ptr; x, y: int; len: unsigned); virtual; abstract;

  end;

  span_converter = object(span_generator)
    m_span_gen: span_generator_ptr;
    m_conv: span_convertor_ptr;

    constructor Construct(span_gen: span_generator_ptr; conv: span_convertor_ptr);

    procedure prepare(max_span_len: unsigned); virtual;
    function generate(x, y: int; len: unsigned): aggclr_ptr; virtual;

  end;

implementation

constructor span_converter.Construct(span_gen: span_generator_ptr; conv: span_convertor_ptr);
begin
  m_span_gen := span_gen;
  m_conv := conv;

end;

{ PREPARE }
procedure span_converter.prepare(max_span_len: unsigned);
begin
  m_span_gen^.prepare(max_span_len);

end;

{ GENERATE }
function span_converter.generate(x, y: int; len: unsigned): aggclr_ptr;
var
  span: aggclr_ptr;

begin
  span := m_span_gen^.generate(x, y, len);

  m_conv^.convert(span, x, y, len);

  Result := span;

end;

end.
