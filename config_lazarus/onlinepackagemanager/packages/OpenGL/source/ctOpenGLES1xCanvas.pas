{**********************************************************************
 Package pl_OpenGL.pkg
 This unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit ctOpenGLES1xCanvas;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, Forms, Controls, LCLType, LCLIntf, LResources,
  Graphics, LMessages,
  ctOpenGLES1x;

type

  TOpenGLES11Canvas = class(TComponent)
  private
     feglDisplay: EGLDisplay;
     feglConfig: EGLConfig;
     feglSurface: EGLSurface;
     feglContext: EGLContext;
     eglWindow: EGLNativeWindowType;
     pi32ConfigAttribs: array[0..128] of EGLint;

     iMajorVersion: EGLint;
     iMinorVersion: EGLint;
     iConfigs: integer;
     FForm:TForm;
     FHandle:HWND;
     fdc : hDC;
  protected
     Procedure InitOpenGLES;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    Procedure SwapBuffers;
  published
    Property Form:Tform read FForm;
  end;


implementation


{ TOpenGLES11Canvas }

constructor TOpenGLES11Canvas.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  if AOwner is TForm then
   begin
     FForm:=TForm(AOwner);
     FHandle:= FForm.Handle;
   end;

  if (csDesigning in ComponentState) then exit;

  if FHandle<>0 then InitOpenGLES;
end;

destructor TOpenGLES11Canvas.Destroy;
begin
 if feglDisplay<>0 then
  begin
    eglMakeCurrent(feglDisplay, nil, nil, nil);
    eglTerminate(feglDisplay);
  end;

  inherited Destroy;
end;

Procedure TOpenGLES11Canvas.InitOpenGLES;
 var i: integer;
begin

  InitGLES11;

  feglDisplay:= 0;
  feglConfig:= 0;
  feglSurface:= nil;
  feglContext:= nil;

  eglWindow:= fHandle;

  fdc := GetDC(eglWindow);

  feglDisplay := eglGetDisplay(fdc);

  if eglInitialize(feglDisplay, @iMajorVersion, @iMinorVersion) = 0 then
    begin
      MessageBox(0, 'eglInitialize() failed.', 'Error', MB_OK or MB_ICONEXCLAMATION);
      Exit;
    end;

  i := 0;      pi32ConfigAttribs[i] := EGL_RED_SIZE;
  i := i + 1;  pi32ConfigAttribs[i] := 5;
  i := i + 1;  pi32ConfigAttribs[i] := EGL_GREEN_SIZE;
  i := i + 1;  pi32ConfigAttribs[i] := 6;
  i := i + 1;  pi32ConfigAttribs[i] := EGL_BLUE_SIZE;
  i := i + 1;  pi32ConfigAttribs[i] := 5;
  i := i + 1;  pi32ConfigAttribs[i] := EGL_ALPHA_SIZE;
  i := i + 1;  pi32ConfigAttribs[i] := 0;
  i := i + 1;  pi32ConfigAttribs[i] := EGL_SURFACE_TYPE;
  i := i + 1;  pi32ConfigAttribs[i] := EGL_WINDOW_BIT;
  i := i + 1;  pi32ConfigAttribs[i] := EGL_NONE;


  if eglChooseConfig(feglDisplay, @pi32ConfigAttribs, @feglConfig, 1, @iConfigs) = 0 then
	begin
          MessageBox(0, 'eglChooseConfig() failed.', 'Error', MB_OK or MB_ICONEXCLAMATION);
          Exit;
	end;


  feglSurface := eglCreateWindowSurface(feglDisplay, feglConfig, eglWindow, nil);
  feglContext := eglCreateContext(feglDisplay, feglConfig, nil, nil);

  If eglMakeCurrent(feglDisplay, feglSurface, feglSurface, feglContext)=0 then
	begin
          MessageBox(0, 'eglMakeCurrent() failed.', 'Error', MB_OK or MB_ICONEXCLAMATION);
          Exit;
	end;
 end;

Procedure TOpenGLES11Canvas.SwapBuffers;
 begin
   eglSwapBuffers(feglDisplay, feglSurface);
 end;

end.

