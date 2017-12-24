
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit agg_bounding_rect ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_vertex_source ;


 function bounding_rect(
           vs : vertex_source_ptr;
           gi : unsigned_ptr;
           start ,num : unsigned;
           x1 ,y1 ,x2 ,y2 : double_ptr ) : boolean;

 function bounding_rect_vs(
           vs ,
           gi : vertex_source_ptr;
           start ,num : unsigned;
           x1 ,y1 ,x2 ,y2 : double_ptr ) : boolean;

 function bounding_rect_ul(
           vs : vertex_source_ptr;
           ul : unsigned_list_ptr;
           start ,num : unsigned;
           x1 ,y1 ,x2 ,y2 : double_ptr ) : boolean;

 function bounding_rect_single(
           vs : vertex_source_ptr;
           path_id : unsigned;
           x1 ,y1 ,x2 ,y2 : double_ptr ) : boolean;

 function bounding_rect_all_paths(
           vs : vertex_source_ptr;
           x1 ,y1 ,x2 ,y2 : double_ptr ) : boolean;

IMPLEMENTATION

function bounding_rect(
           vs : vertex_source_ptr;
           gi : unsigned_ptr;
           start ,num : unsigned;
           x1 ,y1 ,x2 ,y2 : double_ptr ) : boolean;
var
 i ,cmd : unsigned;

 x ,y : double;

 first : boolean;

begin
 first:=true;

 x1^:=1;
 y1^:=1;
 x2^:=0;
 y2^:=0;

 i:=0;

 while i < num do
  begin
   vs^.rewind(unsigned_ptr(ptrcomp(gi ) + (start + i ) * sizeof(unsigned ) )^ );

   cmd:=vs^.vertex(@x ,@y );

   while not is_stop(cmd ) do
    begin
     if is_vertex(cmd ) then
      if first then
       begin
        x1^:=x;
        y1^:=y;
        x2^:=x;
        y2^:=y;
        
        first:=false;

       end
      else
       begin
        if x < x1^ then
         x1^:=x;

        if y < y1^ then
         y1^:=y;

        if x > x2^ then
         x2^:=x;

        if y > y2^ then
         y2^:=y;

       end;

     cmd:=vs^.vertex(@x ,@y );

    end;

   inc(i );

  end;

 result:=(x1^ <= x2^ ) and (y1^ <= y2^ );

end;

{ BOUNDING_RECT_VS }
function bounding_rect_vs(
           vs ,
           gi : vertex_source_ptr;
           start ,num : unsigned;
           x1 ,y1 ,x2 ,y2 : double_ptr ) : boolean;
var
 i ,cmd : unsigned;

 x ,y : double;

 first : boolean;

begin
 first:=true;

 x1^:=1;
 y1^:=1;
 x2^:=0;
 y2^:=0;

 i:=0;

 while i < num do
  begin
   vs^.rewind(gi^.operator_array(start + i ) );

   cmd:=vs^.vertex(@x ,@y );

   while not is_stop(cmd ) do
    begin
     if is_vertex(cmd ) then
      if first then
       begin
        x1^:=x;
        y1^:=y;
        x2^:=x;
        y2^:=y;
        
        first:=false;

       end
      else
       begin
        if x < x1^ then
         x1^:=x;

        if y < y1^ then
         y1^:=y;

        if x > x2^ then
         x2^:=x;

        if y > y2^ then
         y2^:=y;

       end;

     cmd:=vs^.vertex(@x ,@y );

    end;

   inc(i );

  end;

 result:=(x1^ <= x2^ ) and (y1^ <= y2^ );

end;

{ BOUNDING_RECT_UL }
function bounding_rect_ul(
           vs : vertex_source_ptr;
           ul : unsigned_list_ptr;
           start ,num : unsigned;
           x1 ,y1 ,x2 ,y2 : double_ptr ) : boolean;
var
 i ,cmd : unsigned;

 x ,y : double;

 first : boolean;

begin
 first:=true;

 x1^:=1;
 y1^:=1;
 x2^:=0;
 y2^:=0;

 i:=0;

 while i < num do
  begin
   vs^.rewind(ul^.array_operator(start + i ) );

   cmd:=vs^.vertex(@x ,@y );

   while not is_stop(cmd ) do
    begin
     if is_vertex(cmd ) then
      if first then
       begin
        x1^:=x;
        y1^:=y;
        x2^:=x;
        y2^:=y;
        
        first:=false;

       end
      else
       begin
        if x < x1^ then
         x1^:=x;

        if y < y1^ then
         y1^:=y;

        if x > x2^ then
         x2^:=x;

        if y > y2^ then
         y2^:=y;

       end;

     cmd:=vs^.vertex(@x ,@y );

    end;

   inc(i );

  end;

 result:=(x1^ <= x2^ ) and (y1^ <= y2^ );

end;

{ BOUNDING_RECT_SINGLE }
function bounding_rect_single(
           vs : vertex_source_ptr;
           path_id : unsigned;
           x1 ,y1 ,x2 ,y2 : double_ptr ) : boolean;
var
 cmd   : unsigned;
 x ,y  : double;
 first : boolean;

begin
 first:=true;

 x1^:=1;
 y1^:=1;
 x2^:=0;
 y2^:=0;

 vs^.rewind(path_id );

 cmd:=vs^.vertex(@x ,@y );

 while not is_stop(cmd ) do
  begin
   if is_vertex(cmd ) then
    if first then
     begin
      x1^:=x;
      y1^:=y;
      x2^:=x;
      y2^:=y;

      first:=false;

     end
    else
     begin
      if x < x1^ then
       x1^:=x;

      if y < y1^ then
       y1^:=y;

      if x > x2^ then
       x2^:=x;

      if y > y2^ then
       y2^:=y;

     end;

   cmd:=vs^.vertex(@x ,@y );

  end;

 result:=
  (x1^ <= x2^ ) and
  (y1^ <= y2^ );

end;

{ BOUNDING_RECT_ALL_PATHS }
function bounding_rect_all_paths(
           vs : vertex_source_ptr;
           x1 ,y1 ,x2 ,y2 : double_ptr ) : boolean;
var
 i ,paths : unsigned;

 sx1 ,sy1 ,sx2 ,sy2 : double;

 first : boolean;

begin
 first:=true;
 paths:=vs^.num_paths;

 x1^:=1;
 y1^:=1;
 x2^:=0;
 y2^:=0;

 i:=0;

 while i < paths do
  begin
   if bounding_rect_single(vs ,i ,@sx1 ,@sy1 ,@sx2 ,@sy2 ) then
    begin
     if first then
      begin
       x1^:=sx1;
       y1^:=sy1;
       x2^:=sx2;
       y2^:=sy2;

      end
     else
      begin
       if sx1 < x1^ then
        x1^:=sx1;

       if sy1 < y1^ then
        y1^:=sy1;

       if sx2 > x2^ then
        x2^:=sx2;

       if sy2 > y2^ then
        y2^:=sy2;

      end;

     first:=false;

    end;

   inc(i );

  end;

 result:=
  (x1^ <= x2^ ) and
  (y1^ <= y2^ );

end;

END.

