program Demo;

uses
  QForms,
  uDemo in 'uDemo.pas' {fDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfDemo, fDemo);
  Application.Run;
end.
