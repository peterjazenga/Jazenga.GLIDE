
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}
unit
agg_conv_transform;

interface

{$I agg_mode.inc }

uses
  agg_basics,
  agg_trans_affine,
  agg_vertex_source;

type

  conv_transform_ptr = ^conv_transform;

  conv_transform = object(vertex_source)
    m_source: vertex_source_ptr;
    m_trans: trans_affine_ptr;
    constructor Construct(Source: vertex_source_ptr; tr: trans_affine_ptr);
    procedure set_source(Source: vertex_source_ptr);
    procedure rewind(path_id: unsigned); virtual;
    function vertex(x, y: double_ptr): unsigned; virtual;
    procedure transformer_(tr: trans_affine_ptr);
  end;

implementation

constructor conv_transform.Construct(Source: vertex_source_ptr; tr: trans_affine_ptr);
begin
  inherited Construct;

  m_source := Source;
  m_trans := tr;
end;

procedure conv_transform.set_source(Source: vertex_source_ptr);
begin
  m_source := Source;
end;

procedure conv_transform.rewind(path_id: unsigned);
begin
  m_source^.rewind(path_id);
end;

function conv_transform.vertex(x, y: double_ptr): unsigned;
var
  cmd: unsigned;

begin
  cmd := m_source^.vertex(x, y);

  if is_vertex(cmd) then
    m_trans^.transform(m_trans, x, y);

  Result := cmd;
end;

procedure conv_transform.transformer_(tr: trans_affine_ptr);
begin
  m_trans := tr;
end;

end.
