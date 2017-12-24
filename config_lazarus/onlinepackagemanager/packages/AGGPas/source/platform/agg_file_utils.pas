
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit agg_file_utils;

{$IFDEF WINDOWS}
  {$IFDEF WINCE}
       {$i wince_agg_file_utils.inc}
  {$ELSE}
       {$i win_agg_file_utils.inc}
  {$ENDIF}
{$ENDIF}

{$IFDEF MAC}
 {$i mac_agg_file_utils.inc}
{$ENDIF}

{$IFDEF UNIX}
 {$i lin_agg_file_utils.inc} 
{$ENDIF} 
