
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit agg_svg_exception ;

INTERFACE

{$I agg_mode.inc }

uses
 SysUtils ,agg_basics ;


type
 svg_exception_ptr = ^svg_exception;
 svg_exception = class(Exception )
   m_msg : pointer;
   constructor Construct; overload;
   constructor Construct(fmt : PChar ); overload;
   constructor Construct(exc : svg_exception_ptr ); overload;
   procedure Free;
   function  _msg : char_ptr;
  end;


 function  get_double(ptr : agg_basics.char_ptr ) : double;
 

IMPLEMENTATION

constructor svg_exception.Construct;
begin
 m_msg:=NIL;

end;

{ CONSTRUCT }
constructor svg_exception.Construct(fmt : PChar );
var
 max : int;

begin
 m_msg:=NIL;

 if agg_getmem(m_msg ,4096 ) then
  begin
   max:=StrLen(fmt );

   if max > 4095 then
    max:=4095;

   move(fmt[0 ] ,m_msg^ ,max );

   int8_ptr(ptrcomp(m_msg ) + max )^:=0;

  end;

end;

{ CONSTRUCT }
constructor svg_exception.Construct(exc : svg_exception_ptr );
var
 max : int;

begin
 m_msg:=NIL;

 if (exc <> NIL ) and
    (exc^.m_msg <> NIL ) then
  if agg_getmem(m_msg ,4096 ) then
   begin
    max:=StrLen(exc^.m_msg );

    if max > 4095 then
     max:=4095;

    move(exc^.m_msg^ ,m_msg^ ,max );

    int8_ptr(ptrcomp(m_msg ) + max )^:=0;

   end;

end;

{ FREE }
procedure svg_exception.Free;
begin
 if m_msg <> NIL then
  agg_freemem(m_msg ,4096 );

end;

{ _MSG }
function svg_exception._msg : char_ptr;
begin
 result:=char_ptr(m_msg );

end;

{ GET_DOUBLE }
function get_double(ptr : agg_basics.char_ptr ) : double;
var
 buf : array[0..49 ] of char;
 dst ,
 max : char_ptr;
 err : integer;

begin
 dst:=@buf[0 ];
 max:=@buf[48 ];

 while ptr^ <> #0 do
  begin
   case ptr^ of
    '-' ,'.' ,'0'..'9' :
     if dst <> max then
      begin
       dst^:=ptr^;

       inc(ptrcomp(dst ) );

      end
     else
      break;

    else
     break;

   end;

   inc(ptrcomp(ptr ) );

  end;

 dst^:=#0; 

 val(PChar(@buf[0 ] ) ,result ,err );

end;

END.

