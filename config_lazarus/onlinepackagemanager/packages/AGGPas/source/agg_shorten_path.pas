
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}
unit
agg_shorten_path;

interface

{$I agg_mode.inc }

uses
  agg_basics,
  agg_vertex_sequence;

procedure shorten_path(vs: vertex_sequence_ptr; s: double; closed: unsigned = 0);

implementation

procedure shorten_path(vs: vertex_sequence_ptr; s: double; closed: unsigned = 0);
var
  n: int;

  d, x, y: double;

  prev, last: vertex_dist_ptr;

begin
  if (s > 0.0) and (vs^.size > 1) then
  begin
    n := vs^.size - 2;

    while n <> 0 do
    begin
      d := vertex_dist_ptr(vs^.array_operator(n))^.dist;

      if d > s then
        break;

      vs^.remove_last;

      s := s - d;

      Dec(n);

    end;

    if vs^.size < 2 then
      vs^.remove_all

    else
    begin
      n := vs^.size - 1;

      prev := vs^.array_operator(n - 1);
      last := vs^.array_operator(n);

      d := (prev^.dist - s) / prev^.dist;

      x := prev^.x + (last^.x - prev^.x) * d;
      y := prev^.y + (last^.y - prev^.y) * d;
      last^.x := x;
      last^.y := y;

      if not vs^.func_operator_vertex_sequence(prev, last) then
        vs^.remove_last;

      vs^.Close(boolean(closed <> 0));

    end;

  end;

end;

end.

