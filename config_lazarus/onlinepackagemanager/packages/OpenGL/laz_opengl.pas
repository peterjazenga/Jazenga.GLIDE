{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit laz_opengl;

{$warn 5023 off : no warning about unused units}
interface

uses
  AllOpenGLRegister, ctOpenGLES1x, ctOpenGLES1xCanvas, ctOpenGLES2x, 
  ctOpenGLES2xCanvas, dglOpenGL, dglShader, OpenGL_Particles, OpenGL_Textures, 
  OpenGLCanvas, OpenGLPanel, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('AllOpenGLRegister', @AllOpenGLRegister.Register);
end;

initialization
  RegisterPackage('laz_opengl', @Register);
end.
