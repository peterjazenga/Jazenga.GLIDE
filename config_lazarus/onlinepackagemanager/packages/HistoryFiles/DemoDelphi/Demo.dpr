program Demo;

uses
  Forms,
  uDemo in 'uDemo.pas' {fDemo};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfDemo, fDemo);
  Application.Run;
end.
