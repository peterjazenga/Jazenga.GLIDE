
{**********************************************************************
 Package pl_AGGPasVS.pkg
 This unit is part of CodeTyphon Studio (http://www.pilotlogic.com)
***********************************************************************}

{DEFINE AGG_DEBUG }


//--- CPU -------------------------
{$IFDEF CPU386}
 {$DEFINE AGG_CPU_386 }
 {$ASMMODE INTEL}
{$ENDIF }

{$IFDEF CPU64}
  {$IFDEF CPUaarch64 }
     {$DEFINE AGG_CPU_ARM }
  {$ELSE }
     {$DEFINE AGG_CPU_386 }
     {$ASMMODE INTEL}
  {$ENDIF }
{$ENDIF }

{$IFDEF CPUPOWERPC }
 {$UNDEF AGG_CPU_ARM }
 {$UNDEF AGG_CPU_386 }
 {$DEFINE AGG_CPU_PPC }
{$ENDIF }

{$IFDEF CPUARM }
 {$UNDEF AGG_CPU_PPC }
 {$UNDEF AGG_CPU_386 }
 {$DEFINE AGG_CPU_ARM }
{$ENDIF }

//--- OS --------------------------

{$IFDEF MSWINDOWS }
 {$DEFINE AGG_WINDOWS }
 {$DEFINE AGG2D_USE_WINFONTS}
{$ENDIF }

{$IFDEF WINCE }
 {$DEFINE AGG_WINDOWS }
 {$DEFINE AGG2D_NO_FONT}
{$ENDIF }

{$IF DEFINED(LINUX) or DEFINED(FREEBSD) or DEFINED(SUNOS)}
 {$DEFINE AGG_LINUX }
 {$DEFINE AGG2D_USE_FREETYPE }
{$ENDIF }

{$IFDEF DARWIN }
 {$DEFINE AGG_MACOSX }
 {$DEFINE AGG2D_USE_FREETYPE }
{$ENDIF }

//---- DEBUG -----------------------
{$IFDEF AGG_DEBUG }
 {$DEFINE AGG_FULL_DEBUG }
{$ENDIF }

//----------------------------------------------------------------------------
{ SWITCHES CONFIGURATION }

{$HINTS OFF }
{$WARNINGS OFF }

{$B- }{ Complete boolean evaluation }
{$V- }{ String type checking }
{$X+ }{ Extended syntax }

{$IFDEF AGG_FULL_DEBUG }
   {$R+ }{ Range checking }
   {$I+ }{ IO checking }
   {$Q+ }{ Overflow checking }
   {$D+ }{ Debug Info ON }
   {$Y+ }{ References Info ON }
{$ELSE }
   {$R- }{ Range checking }
   {$I- }{ IO checking }
   {$Q- }{ Overflow checking }
   {$D- }{ Debug Info OFF }
   {.$Y- }{ References Info OFF }
{$ENDIF }


{$IFNDEF AGG2D_USE_WINFONTS}
 {$IFNDEF AGG2D_USE_FREETYPE}
  {$DEFINE AGG2D_NO_FONT}
 {$ENDIF}
{$ENDIF}

