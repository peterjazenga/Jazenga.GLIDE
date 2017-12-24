{**********************************************************************
 Package pl_OpenGL.pkg
 This unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}


{$IF DEFINED(LCLGTK) or DEFINED(GTK)}
    {$DEFINE UseGtkGLX}
    {$DEFINE HasRGBA}
    {$DEFINE HasRGBBits}
    {$DEFINE OpenGLTargetDefined}
{$ENDIF}

{$IF DEFINED(LCLGTK2) or DEFINED(GTK2)}
    {$DEFINE UseGtk2GLX}
    {$DEFINE UsesModernGL}
    {$DEFINE HasRGBA}
    {$DEFINE HasRGBBits}
    {$DEFINE HasDebugContext}
    {$DEFINE OpenGLTargetDefined}
{$ENDIF}

{$IF DEFINED(LCLCarbon) or DEFINED(Carbon)}
  {$DEFINE UseCarbonAGL}
  {$DEFINE HasRGBA}
  {$DEFINE HasRGBBits}
  {$DEFINE OpenGLTargetDefined}
{$ENDIF}

{$IF DEFINED(LCLCocoa) or DEFINED(Cocoa)}
  {$DEFINE UseCocoaNS}
  {$DEFINE UsesModernGL}
  {$DEFINE OpenGLTargetDefined}
  {$DEFINE HasMacRetinaMode}
{$ENDIF}

{$IF DEFINED(LCLWin32) or DEFINED(LCLWin64) or DEFINED(Win32) or DEFINED(Win64)}
  {$DEFINE UseWin32WGL}
  {$DEFINE HasRGBA}
  {$DEFINE HasRGBBits}
  {$DEFINE HasDebugContext}
  {$DEFINE OpenGLTargetDefined}
{$ENDIF}

{$IF DEFINED(LCLQT) or DEFINED(QT)}
  {$DEFINE UseQTGLX}
  {$DEFINE UsesModernGL}
  {$DEFINE HasRGBA}
  {$DEFINE HasRGBBits}
  {$DEFINE OpenGLTargetDefined}
{$ENDIF}

{$IF DEFINED(LCLQT5) or DEFINED(QT5)}
  {$DEFINE UseQT5GLX}
  {$DEFINE UsesModernGL}
  {$DEFINE HasRGBA}
  {$DEFINE HasRGBBits}
  {$DEFINE OpenGLTargetDefined}
{$ENDIF}

{$IFNDEF OpenGLTargetDefined}
  {$ERROR this LCL widgetset/OS is not yet supported}
{$ENDIF}
