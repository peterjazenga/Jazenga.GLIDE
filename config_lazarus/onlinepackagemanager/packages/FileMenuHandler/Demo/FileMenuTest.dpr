program FileMenuTest;

uses
  Forms,
  MainFrm in 'MainFrm.pas' {MainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'FileMenuTest';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
