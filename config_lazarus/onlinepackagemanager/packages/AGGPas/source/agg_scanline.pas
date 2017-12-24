
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}
unit
agg_scanline;

interface

{$I agg_mode.inc }

uses
  agg_basics;

type
  span_ptr = ^span;

  span = record
    x,
    len: int16;

    covers: int8u_ptr;

  end;

  span_obj_ptr = ^span_obj;

  span_obj = object
    function x: int; virtual;
    function len: int; virtual;
    function covers: int8u_ptr; virtual;

    procedure inc_operator; virtual;

  end;

  scanline_ptr = ^scanline;

  scanline = object
    procedure reset(min_x, max_x: int); virtual; abstract;
    procedure reset_spans; virtual; abstract;

    procedure finalize(y_: int); virtual; abstract;
    procedure add_cell(x: int; cover: unsigned); virtual; abstract;
    procedure add_cells(x: int; len: unsigned; covers: int8u_ptr); virtual; abstract;
    procedure add_span(x: int; len, cover: unsigned); virtual; abstract;

    function y: int; virtual; abstract;
    function num_spans: unsigned; virtual; abstract;
    function begin_: pointer; virtual; abstract;

    function sz_of_span: unsigned; virtual; abstract;
    function is_plain_span: boolean; virtual;
    function is_embedded: boolean; virtual;

    procedure init(ptr: int8u_ptr; dx, dy: int); virtual; abstract;
    procedure setup(scanline_idx: unsigned); virtual; abstract;

  end;

implementation

function span_obj.x: int;
begin
  Result := 0;

end;

{ LEN }
function span_obj.len: int;
begin
  Result := 0;

end;

{ COVERS }
function span_obj.covers: int8u_ptr;
begin
  Result := nil;

end;

{ INC_OPERATOR }
procedure span_obj.inc_operator;
begin
end;

{ IS_PLAIN_SPAN }
function scanline.is_plain_span: boolean;
begin
  Result := True;

end;

{ IS_EMBEDDED }
function scanline.is_embedded: boolean;
begin
  Result := False;

end;

end.
