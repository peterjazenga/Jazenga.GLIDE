{**********************************************************************
                   PilotLogic Software House.
  
 Package pl_OpenGL.pkg
 This unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit AllOpenGLRegister;

{$mode objfpc}{$H+}

interface

uses
  Classes,SysUtils,TypInfo, lresources, PropEdits, ComponentEditors,
  OpenGLPanel,
  OpenGLCanvas,
  {$IFDEF MSWINDOWS}
   ctOpenGL2DCanvas,
  {$ENDIF}
  ctOpenGLES1xCanvas,
  ctOpenGLES2xCanvas;

procedure Register;

implementation

{$R AllOpenGLRegister.res}

//==========================================================

procedure Register;
begin
 RegisterComponents ('OpenGL',[
                                TOpenGLPanel,
                                TOpenGLCanvas,
                               {$IFDEF MSWINDOWS}
                                TOpenGL2DCanvas,
                                {$ENDIF}
                                TOpenGLES11Canvas,
                                TOpenGLES20Canvas
                                 ]);



end;

end.

