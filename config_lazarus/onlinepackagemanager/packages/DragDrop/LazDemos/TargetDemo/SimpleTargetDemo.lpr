program SimpleTargetDemo;

{$MODE Delphi}

{%File 'readme.txt'}

uses
  Forms, Interfaces,
  Main in 'Main.pas' {Form1};

{.$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Simple Target Demo';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
