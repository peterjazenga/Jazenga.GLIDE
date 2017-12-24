
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit expat_basics;

{$MODE DELPHI}

INTERFACE

{$I expat_mode.inc }

{ GLOBAL PROCEDURES }
 function  expat_getmem (var ptr : pointer; sz : integer ) : boolean;
 function  expat_realloc(var ptr : pointer; old ,sz : integer ) : boolean;
 function  expat_freemem(var ptr : pointer; sz : integer ) : boolean;
 
 procedure NoP;

// SHR for signed integers is differently implemented in pascal compilers
// than in c++ compilers. On the assembler level, c++ is using the SAR and
// pascal is using SHR. That gives completely different result, when the
// number is negative. We have to be compatible with c++ implementation,
// thus instead of directly using SHR we emulate c++ solution.
 function  shr_int8 (i ,shift : shortint ) : shortint;
 function  shr_int16(i ,shift : smallint ) : smallint;
 function  shr_int32(i ,shift : longint ) : longint;


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ EXPAT_GETMEM }
function expat_getmem (var ptr : pointer; sz : integer ) : boolean;
begin
 result:=false;

 try
  getmem(ptr ,sz );

  result:=true;

 except
  ptr:=NIL;

 end;

end;

{ EXPAT_REALLOC }
function expat_realloc(var ptr : pointer; old ,sz : integer ) : boolean;
var
 nb  : pointer;
 max : integer;

begin
 if expat_getmem(nb ,sz ) then
  begin
   max:=old;

   if max > sz then
    max:=sz;

   move(ptr^ ,nb^ ,max );

   expat_freemem(ptr ,old );

   ptr   :=nb;
   result:=true;

  end
 else
  result:=false;

end;

{ EXPAT_FREEMEM }
function expat_freemem(var ptr : pointer; sz : integer ) : boolean;
begin
 if ptr = NIL then
  result:=true

 else
  try
   freemem(ptr ,sz );

   ptr   :=NIL;
   result:=true;

  except
   result:=false;

  end;

end;

{ NOP }
procedure NoP;
begin
end;

{ SHR_INT8 }
function shr_int8 (i ,shift : shortint ) : shortint;
begin
{$IFDEF EXPAT_CPU_386 }
 asm
  mov al ,byte ptr [i ]
  mov cl ,byte ptr [shift ]
  sar al ,cl
  mov byte ptr [result ] ,al

 end;

{$ENDIF }

{$IFDEF EXPAT_CPU_PPC }
 asm
  lbz   r2,i
  extsb r2,r2
  lbz   r3,shift
  extsb r3,r3
  sraw  r2,r2,r3
  extsb r2,r2
  stb   r2,result

 end;

{$ENDIF }

end;

{ SHR_INT16 }
function shr_int16(i ,shift : smallint ) : smallint;
begin
{$IFDEF EXPAT_CPU_386 }
 asm
  mov ax ,word ptr [i ]
  mov cx ,word ptr [shift ]
  sar ax ,cl
  mov word ptr [result ] ,ax

 end;

{$ENDIF }

{$IFDEF EXPAT_CPU_PPC }
 asm
  lha   r2,i
  lha   r3,shift
  sraw  r2,r2,r3
  extsh r2,r2
  sth   r2,result

 end;

{$ENDIF }

end;

{ SHR_INT32 }
function shr_int32(i ,shift : longint ) : longint;
begin
{$IFDEF EXPAT_CPU_386 }
 asm
  mov eax ,dword ptr [i ]
  mov ecx ,dword ptr [shift ]
  sar eax ,cl
  mov dword ptr [result ] ,eax

 end;

{$ENDIF }

{$IFDEF EXPAT_CPU_PPC }
 asm
  lwz  r3,i
  lwz  r2,shift
  sraw r3,r3,r2
  stw  r3,result

 end;

{$ENDIF }

end;

END.

