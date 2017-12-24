
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}
unit agg_conv_concat;

interface

{$I agg_mode.inc }

uses
  agg_basics,
  agg_vertex_source;

type

  conv_concat = object(vertex_source)
    m_source1, m_source2: vertex_source_ptr;
    m_status: int;
    constructor Construct(source1, source2: vertex_source_ptr);
    procedure set_source1(Source: vertex_source_ptr);
    procedure set_source2(Source: vertex_source_ptr);
    procedure rewind(path_id: unsigned); virtual;
    function vertex(x, y: double_ptr): unsigned; virtual;
  end;


implementation

constructor conv_concat.Construct(source1, source2: vertex_source_ptr);
begin
  m_source1 := source1;
  m_source2 := source2;
  m_status := 2;

end;

procedure conv_concat.set_source1(Source: vertex_source_ptr);
begin
  m_source1 := Source;

end;

procedure conv_concat.set_source2(Source: vertex_source_ptr);
begin
  m_source2 := Source;

end;

procedure conv_concat.rewind(path_id: unsigned);
begin
  m_source1^.rewind(path_id);
  m_source2^.rewind(0);

  m_status := 0;

end;

function conv_concat.vertex(x, y: double_ptr): unsigned;
var
  cmd: unsigned;

begin
  if m_status = 0 then
  begin
    cmd := m_source1^.vertex(x, y);

    if not is_stop(cmd) then
    begin
      Result := cmd;

      exit;

    end;

    m_status := 1;

  end;

  if m_status = 1 then
  begin
    cmd := m_source2^.vertex(x, y);

    if not is_stop(cmd) then
    begin
      Result := cmd;

      exit;

    end;

    m_status := 2;

  end;

  Result := path_cmd_stop;

end;

end.


