program Demo;

{$MODE Delphi}

uses
  Forms, Interfaces,
  uDemo in 'uDemo.pas' {fDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfDemo, fDemo);
  Application.Run;
end.
