{**********************************************************************
 Package pl_OpenGL.pkg
 This unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit dglShader;

interface

{$MODE Delphi}

uses sysutils,classes, dglOpenGL;

type
  TMatrix=array[0..3,0..3] of glFloat; //some simple matrix
  PMatrix=^TMatrix;

  TShader=class
  private
  protected
   FShaderObject: GLUint;
   FShaderSource: TStringList;
   FShaderText: ansistring;
   FShaderLength: integer;
   FShaderType: GLEnum;
  public
      constructor Create(); overload;
      destructor Destroy(); override;
      procedure Compile(); virtual; abstract;
      procedure Enable(); virtual; abstract;
      procedure Disable(); virtual; abstract;
      function Log(): string; virtual; abstract;
  end;

  TProgram=class
  private
  protected
    FProgramObject: GLHandleARB;
    FShaders: array of TShader;
    FNumShaders: integer;
  public
      constructor Create(); overload;
      procedure Attach(ashader: TShader); virtual;
      procedure Detach(ashader: TSHader); virtual;
      procedure Link(); virtual; abstract;
      procedure Enable(); virtual; abstract;
      procedure Disable(); virtual; abstract;
      function Log(): string; virtual; abstract;
      procedure SetI(aname: string; arg: GLInt); virtual; abstract;
      procedure SetF(aname: string; arg: GLFloat); virtual; abstract;
      procedure Set2I(aname: string; arg1: GLInt; arg2: GLInt); virtual; abstract;
      procedure Set2F(aname: string; arg1: GLFloat; arg2: GLFloat); virtual; abstract;
      procedure Set3I(aname: string; arg1: GLInt; arg2: GLInt; arg3: GLInt); virtual; abstract;
      procedure Set3F(aname: string; arg1: GLFloat; arg2: GLFloat; arg3: GLFloat); virtual; abstract;
      procedure Set4I(aname: string; arg1: GLInt; arg2: GLInt; arg3: GLInt; arg4: GLInt); virtual; abstract;
      procedure Set4F(aname: string; arg1: GLFloat; arg2: GLFloat; arg3: GLFloat; arg4: GLFloat); virtual; abstract;
      procedure Set3Iv(aname: string; const args: PGLInt); virtual; abstract;
      procedure Set3Fv(aname: string; const args: PGLFloat); virtual; abstract;
      procedure SetMatrix(aname: string; const args: PMatrix); virtual; abstract;
  end;

  TARBProgram=class (TProgram)
  private
  protected
  public
      constructor Create(); overload;
      procedure Attach(ashader: TShader); override;
      procedure Detach(ashader: TSHader); override;
      procedure Link(); override;
      procedure Enable(); override;
      procedure Disable(); override;
      function Log(): string; override;
  end;

  TARBShader= class (TShader)
  private
  protected
  public
    constructor Create(); overload;
    constructor Create(aname: string; atype: GLEnum = GL_VERTEX_PROGRAM_ARB); overload;
    procedure LoadFromFile(aname: string; atype: GLEnum= GL_VERTEX_PROGRAM_ARB);
    procedure Enable; override;
    procedure Disable; override;
    procedure Compile; override;
    function Log: string; override;
  end;

  TGLSLShader= class (TShader)
  private
    function GetInfoLog() : string;
  protected
  public
    constructor Create(); overload;
    constructor Create(aname: string; atype: GLEnum = GL_VERTEX_SHADER_ARB); overload;
    destructor Destroy(); reintroduce; override;
    procedure LoadFromFile(aname: string; atype: GLEnum= GL_VERTEX_SHADER_ARB);
    procedure Compile(); override;
    function Log: string; override;
  end;

  TGLSLProgram= class (TProgram)
  private
    function GetInfoLog() : string;
  protected
  public
    constructor Create();
    destructor Destroy(); reintroduce; override;
    procedure Attach(ashader: TShader); override;
    procedure Detach(ashader: TSHader); override;
    procedure Link(); override;
    procedure Enable(); override;
    procedure Disable(); override;
    function Log(): string; override;
    procedure SetI(aname: string; arg: GLInt); override;
    procedure SetF(aname: string; arg: GLFloat); override;
    procedure Set2I(aname: string; arg1: GLInt; arg2: GLInt); override;
    procedure Set2F(aname: string; arg1: GLFloat; arg2: GLFloat); override;
    procedure Set3I(aname: string; arg1: GLInt; arg2: GLInt; arg3: GLInt); override;
    procedure Set3F(aname: string; arg1: GLFloat; arg2: GLFloat; arg3: GLFloat); override;
    procedure Set4I(aname: string; arg1: GLInt; arg2: GLInt; arg3: GLInt; arg4: GLInt); override;
    procedure Set4F(aname: string; arg1: GLFloat; arg2: GLFloat; arg3: GLFloat; arg4: GLFloat); override;
    procedure Set3Iv(aname: string; const args: PGLInt); override;
    procedure Set3Fv(aname: string; const args: PGLFloat); override;
    procedure SetMatrix(aname: string; const args: PMatrix); override;
    procedure BindAttribLocation(aname: string; pos: GLInt);
    function  GetAttribLocation(aname: string): GlInt;
    function  GetUniformLocation(aname: string): GlInt;
	  procedure BindFragDataLocation(aname: string; pos: GLInt);
  end;

implementation

//TShader

constructor TShader.Create;
begin
end;

destructor TShader.Destroy;
begin
end;

//TProgram

constructor TProgram.Create;
begin
  FNumShaders := 0;
end;

procedure TProgram.Attach(ashader: TShader);
begin
  FNumShaders := FNumShaders + 1;
  SetLength(FShaders, FNumShaders);
  FShaders[FNumShaders-1]:=ashader;
  ashader.Compile();
end;

procedure TProgram.Detach(ashader: TShader);
begin
end;

//TGLSLShader

function TGLSLShader.GetInfoLog() : string;
var
 blen,slen : GLInt;
 InfoLog   : PGLCharARB;
begin
glGetObjectParameterivARB(FShaderObject, GL_OBJECT_INFO_LOG_LENGTH_ARB , @blen);
if blen > 1 then
 begin
 GetMem(InfoLog, blen*SizeOf(GLCharARB));
 glGetInfoLogARB(FShaderObject, blen, slen, InfoLog);
 Result := String(InfoLog);
 Dispose(InfoLog);
 end;
end;

function TGLSLProgram.GetInfoLog() : string;
var
 blen,slen : GLInt;
 InfoLog   : PGLCharARB;
begin
glGetObjectParameterivARB(FProgramObject, GL_OBJECT_INFO_LOG_LENGTH_ARB , @blen);
if blen > 1 then
 begin
 GetMem(InfoLog, blen*SizeOf(GLCharARB));
 glGetInfoLogARB(FProgramObject, blen, slen, InfoLog);
 Result := String(InfoLog);
 Dispose(InfoLog);
 end;
end;

constructor TGLSLShader.Create();
begin
  FShaderSource:= TStringList.Create();
end;

destructor TGLSLShader.Destroy();
begin
  FShaderText := '';
  FShaderSource.Free;
  inherited;
end;

constructor TGLSLShader.Create(aname: string; atype: GLEnum = GL_VERTEX_SHADER_ARB);
begin

self.Create();
self.LoadFromFile(aname,atype);

end;

procedure TGLSLShader.LoadFromFile(aname: string; atype: GLEnum= GL_VERTEX_SHADER_ARB);
begin
  if FileExists(aname)= True then
  begin
    FShaderObject:= glCreateShaderObjectARB(atype);

    FShaderSource.LoadFromFile(aname);
    FShaderText:= AnsiString(FShaderSource.Text);
    FShaderLength:= Length(FShaderText);

    glShaderSourceARB(FShaderObject, 1, @FShaderText, @FShaderLength);

  end
end;

procedure TGLSLShader.Compile();
begin
 glCompileShaderARB(FShaderObject);
end;

// TGLSLProgram

constructor TGLSLProgram.Create();
begin
  FProgramObject := glCreateProgramObjectARB;
end;

destructor TGLSLProgram.Destroy;
begin
  glDeleteObjectARB(FProgramObject);
  inherited;
end;

procedure TGLSLProgram.Attach(ashader: TShader);
begin
  inherited Attach(ashader);
  ashader.Compile();
  glAttachObjectARB(FProgramObject, ashader.FShaderObject);
  glDeleteObjectARB(ashader.FShaderObject);
end;

procedure TGLSLProgram.Detach(ashader: TShader);
begin
  glDetachObjectARB(FProgramObject, ashader.FShaderObject);
  inherited Detach(ashader);
end;

procedure TGLSLProgram.Link();
begin
  glLinkProgramARB(FProgramObject);
end;

procedure TGLSLProgram.Enable();
begin
  //glUseProgramObjectARB(FProgramObject);
  glUseProgram(FProgramObject);
end;

procedure TGLSLProgram.Disable();
begin
  //glUseProgramObjectARB(0);
  glUseProgram(0);
end;

procedure TGLSLProgram.SetI(aname: string; arg: GLInt);
begin
  //Enable();
  glUniform1iARB(glGetUniformLocationARB(FProgramObject, PGLcharARB(ansistring(aname)) ), arg);
end;

procedure TGLSLProgram.SetF(aname: string; arg: GLFloat);
begin
  //Enable();
  glUniform1fARB(glGetUniformLocationARB(FProgramObject, PGLcharARB(ansistring(aname)) ), arg);
end;

procedure TGLSLProgram.Set2I(aname: string; arg1: GLInt; arg2: GLInt);
begin
  //Enable();
  glUniform2iARB(glGetUniformLocationARB(FProgramObject, PGLcharARB(ansistring(aname)) ), arg1, arg2);
end;

procedure TGLSLProgram.Set2F(aname: string; arg1: GLFloat; arg2: GLFloat);
begin
  //Enable();
  glUniform2fARB(glGetUniformLocationARB(FProgramObject, PGLcharARB(ansistring(aname)) ), arg1, arg2);
end;

procedure TGLSLProgram.Set3I(aname: string; arg1: GLInt; arg2: GLInt; arg3: GLInt);
begin
  //Enable();
  glUniform3iARB(glGetUniformLocationARB(FProgramObject, PGLcharARB(ansistring(aname)) ), arg1, arg2, arg3);
end;

procedure TGLSLProgram.Set3F(aname: string; arg1: GLFloat; arg2: GLFloat; arg3: GLFloat);
begin
  //Enable();
  glUniform3fARB(glGetUniformLocationARB(FProgramObject, PGLcharARB(ansistring(aname)) ), arg1, arg2, arg3);
end;

procedure TGLSLProgram.Set4I(aname: string; arg1: GLInt; arg2: GLInt; arg3: GLInt; arg4: GLInt);
begin
  //Enable();
  glUniform4iARB(glGetUniformLocationARB(FProgramObject, PGLcharARB(ansistring(aname)) ), arg1, arg2, arg3, arg4);
end;

procedure TGLSLProgram.Set4F(aname: string; arg1: GLFloat; arg2: GLFloat; arg3: GLFloat; arg4: GLFloat);
begin
  //Enable();
  glUniform4fARB(glGetUniformLocationARB(FProgramObject, PGLcharARB(ansistring(aname)) ), arg1, arg2, arg3, arg4);
end;

procedure TGLSLProgram.Set3Iv(aname: string; const args: PGLInt);
begin
  //Enable();
  glUniform3ivARB(glGetUniformLocationARB(FProgramObject, PGLcharARB(ansistring(aname)) ), 1, args);
end;

procedure TGLSLProgram.Set3Fv(aname: string; const args: PGLFloat);
begin
  //Enable();
  glUniform3fvARB(glGetUniformLocationARB(FProgramObject, PGLcharARB(ansistring(aname)) ), 1, args);
end;

procedure TGLSLProgram.SetMatrix(aname: string; const args: PMatrix);
begin
  //Enable();
  glUniformMatrix4fvARB(glGetUniformLocationARB(FProgramObject, PGLcharARB(ansistring(aname)) ), 1, false, @args);
end;

procedure TGLSLProgram.BindAttribLocation(aname: string; pos: Integer);
begin
   glBindAttribLocation(FProgramObject, pos, PGLcharARB(ansistring(aname)) );
end;

function TGLSLProgram.GetAttribLocation(aname: string): integer;
begin
  result := glGetAttribLocation(FProgramObject, PGLcharARB(ansistring(aname)) );
end;

function TGLSLProgram.GetUniformLocation(aname: string): integer;
begin
  result := glGetUniformLocation(FProgramObject, PGLcharARB(ansistring(aname)) );
end;

procedure TGLSLProgram.BindFragDataLocation(aname: string; pos: Integer);
begin
	glBindFragDataLocation(FProgramObject, pos, PGLcharARB(ansistring(aname)) );
end;

function TGLSLProgram.Log(): String;
begin
  result:=GetInfoLog;
end;

function TGLSLShader.Log(): String;
begin
  result:=GetInfoLog;
end;

// TARBShader

constructor TARBShader.Create();
begin
  FShaderSource:= TStringList.Create();
end;

constructor TARBShader.Create(aname: string; atype: GLEnum = GL_VERTEX_PROGRAM_ARB);
begin

self.Create();
self.LoadFromFile(aname,atype);

end;

procedure TARBShader.LoadFromFile(aname: string; atype: GLEnum= GL_VERTEX_PROGRAM_ARB);
begin
  if FileExists(aname)= True then
  begin

    glGenProgramsARB( 1, @FShaderObject );
    glBindProgramARB( atype, FShaderObject );

    FShaderSource.LoadFromFile(aname);
    FShaderText:= AnsiString(FShaderSource.Text);
    FShaderLength:= Length(FShaderSource.Text);
    FShaderType:= atype;

    glProgramStringARB( FShaderType,GL_PROGRAM_FORMAT_ASCII_ARB,
                        FShaderLength, PAnsiChar(FShaderText) );

  end
end;

procedure TARBShader.Compile();
begin
end;

function TARBShader.Log: string;
var
  errorpos: integer;
begin
  if glGetError() <> GL_NO_ERROR then
  begin
    result := result + 'error at: ';
    glGetIntegerv(GL_PROGRAM_ERROR_POSITION_ARB, @errorPos);
    result := result + inttostr(errorPos);
    result := result + ': '+String(glGetString( GL_PROGRAM_ERROR_STRING_ARB ));
  end;
end;

procedure TARBShader.Enable;
begin
  glEnable( FShaderType );
	glBindProgramARB( FShaderType, FShaderObject );
end;

procedure TARBShader.Disable;
begin
  glDisable( FShaderType );
end;

// TARBProgram
constructor TARBProgram.Create();
begin
end;

procedure TARBProgram.Attach(ashader: TShader);
begin
  inherited Attach(ashader);
end;

procedure TARBProgram.Detach(ashader: TShader);
begin
  inherited Detach(ashader);
end;

procedure TARBProgram.Link;
begin
end;

procedure TARBProgram.Enable;
var
I: integer;
begin
  for I := 0 to FNumShaders - 1 do
  begin
    FShaders[I].Enable;
  end;
end;

procedure TARBProgram.Disable;
var
I: integer;
begin
  for I := 0 to FNumShaders - 1 do
  begin
    FShaders[I].Disable;
  end;
end;

function TARBProgram.Log: String;
begin
  result := '';
end;

end.
