
{**********************************************************************
 Package pl_Win_DirectX.pkg
 is a modification of DirectX headers Library (http://www.clootie.ru/)
 For CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

  {$ASMMODE Intel}

  // Additional settings
  {$H+} // Long Strings
  {$BOOLEVAL OFF}
  {$MINENUMSIZE 4}
  {$ALIGN ON}
  {$PACKRECORDS 8}
  {$INLINE ON}

  {$UNDEF TYPE_IDENTITY}
  {$DEFINE SUPPORTS_EXCEPTIONS}
  {$DEFINE SUPPORTS_INLINE}
  {$DEFINE COMPILER5_UP} // Specially for DirectDraw.pas

// By default use most recent DirectX sub-version

{$IFNDEF DX81}
  {$IFNDEF DX80}
    {$DEFINE DX81}
  {$ENDIF}
{$ENDIF}

{$IFNDEF DX92}
  {$IFNDEF DX91}
    {$IFNDEF DX90}
      {$DEFINE DX92}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}




