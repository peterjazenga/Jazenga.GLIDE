program VirtualFileStream;

{$MODE Delphi}

{%File 'readme.txt'}

uses
  Forms, Interfaces,
  Main in 'Main.pas' {FormMain};

{.$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
