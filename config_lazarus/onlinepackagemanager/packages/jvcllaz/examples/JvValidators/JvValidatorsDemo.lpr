program JvValidatorsDemo;

{$MODE Delphi}

uses
  {$IFDEF UNIX} cthreads, {$ENDIF}
  Forms, Interfaces,
  MainFrm in 'MainFrm.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
