
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}
unit
 agg_vertex_sequence ;

INTERFACE

{$I agg_mode.inc}

uses
 agg_basics ,
 agg_array ;

type
 func_vertex_sequence = function(this ,val : pointer ) : boolean;

//----------------------------------------------------------vertex_sequence
// Modified agg::pod_deque. The data is interpreted as a sequence of vertices.
 vertex_sequence_ptr = ^vertex_sequence;
 vertex_sequence = object(pod_deque )
   func_operator_vertex_sequence : func_vertex_sequence;

   constructor Construct(entry_sz : unsigned; s_ : unsigned = 6; fovs : func_vertex_sequence = NIL );

   procedure add(val : pointer );

   procedure modify_last(val : pointer);

   procedure close(remove_flag : boolean );

  end;

// Coinciding points maximal distance (Epsilon)
const
 vertex_dist_epsilon : double = 1e-14;

//-------------------------------------------------------------vertex_dist
// Vertex (x, y) with the distance to the next one. The last vertex has
// distance between the last and the first points if the polygon is closed
// and 0.0 if it's a polyline.
type
 vertex_dist_ptr = ^vertex_dist;
 vertex_dist = record
   x ,y ,dist : double;

  end;

 vertex_dist_cmd_ptr = ^vertex_dist_cmd;
 vertex_dist_cmd = record
   x ,y ,dist : double;

   cmd : unsigned;

  end;

 function  vertex_dist_func_operator(this ,val : vertex_dist_ptr ) : boolean;


IMPLEMENTATION
uses
 agg_math ;

function vertex_dist_func_operator(this ,val : vertex_dist_ptr ) : boolean;
var
 ret : boolean;

begin
 this^.dist:=calc_distance(this^.x ,this^.y ,val^.x ,val^.y );

 ret:=this^.dist > vertex_dist_epsilon;

 if not ret then
  this^.dist:=1 / vertex_dist_epsilon;

 result:=ret; 

end;

{ CONSTRUCT }
constructor vertex_sequence.Construct(entry_sz : unsigned; s_ : unsigned = 6; fovs : func_vertex_sequence = NIL );
begin
 inherited Construct(entry_sz ,s_ );

 if fovs = NIL then
  Pointer(func_operator_vertex_sequence):=@vertex_dist_func_operator
 else
  func_operator_vertex_sequence:=fovs;

end;

{ ADD }
procedure vertex_sequence.add(val : pointer );
begin
 if size > 1 then
  if not func_operator_vertex_sequence(
          array_operator(size - 2 ) ,
          array_operator(size - 1 ) ) then
   remove_last;
   
 inherited add(val );

end;

{ MODIFY_LAST }
procedure vertex_sequence.modify_last(val : pointer);
begin
 remove_last;

 add(val );

end;

{ CLOSE }
procedure vertex_sequence.close(remove_flag : boolean );
var
 t : pointer;

begin
 while size > 1 do
  begin
   if func_operator_vertex_sequence(
       array_operator(size - 2 ) ,
       array_operator(size - 1 ) ) then
    break;

   t:=array_operator(size - 1 );

   remove_last;
   modify_last(t );

  end;

 if remove_flag then
  while size > 1 do
   begin
    if func_operator_vertex_sequence(
        array_operator(size - 1 ) ,
        array_operator(0 ) ) then
     break;

    remove_last; 

   end;

end;

END.

