
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}
unit
agg_vertex_source;

interface

{$I agg_mode.inc }

uses
  agg_basics;

type
  vertex_source_ptr = ^vertex_source;

  vertex_source = object
    constructor Construct;
    destructor Destruct; virtual;
    procedure remove_all; virtual;
    procedure add_vertex(x, y: double; cmd: unsigned); virtual;
    function num_paths: unsigned; virtual;
    procedure rewind(path_id: unsigned); virtual;
    function vertex(x, y: double_ptr): unsigned; virtual;
    function func_operator_gamma(x: double): double; virtual;

    function operator_array(i: unsigned): unsigned; virtual; abstract;
  end;

implementation

constructor vertex_source.Construct;
begin
end;

destructor vertex_source.Destruct;
begin
end;

procedure vertex_source.remove_all;
begin
end;

procedure vertex_source.add_vertex(x, y: double; cmd: unsigned);
begin
end;

function vertex_source.num_paths: unsigned;
begin
  Result := 0;
end;

procedure vertex_source.rewind(path_id: unsigned);
begin
end;

function vertex_source.vertex(x, y: double_ptr): unsigned;
begin
end;

function vertex_source.func_operator_gamma(x: double): double;
begin
  Result := x;
end;

end.
