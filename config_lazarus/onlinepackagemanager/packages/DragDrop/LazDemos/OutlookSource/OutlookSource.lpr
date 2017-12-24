program OutlookSource;

{$MODE Delphi}

uses
  Forms, Interfaces,
  Main in 'Main.pas' {FormOutlookSource};

{.$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Outlook Drop Source demo';
  Application.CreateForm(TFormOutlookSource, FormOutlookSource);
  Application.Run;
end.
