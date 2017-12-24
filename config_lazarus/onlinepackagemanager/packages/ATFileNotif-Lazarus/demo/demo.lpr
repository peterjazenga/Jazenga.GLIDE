program demo;

{$mode objfpc}

uses
  Forms, Interfaces,
  formmain {FormMain};

{.$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'ATFileNotification Demo';
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
