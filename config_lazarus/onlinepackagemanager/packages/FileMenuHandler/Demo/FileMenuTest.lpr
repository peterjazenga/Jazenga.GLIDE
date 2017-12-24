program FileMenuTest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainFrm, LResources
  { you can add units after this };

  {$IFDEF WINDOWS}{$R FileMenuTest.rc}{$ENDIF}

begin
  {.$I FileMenuTest.lrs}
  Application.Initialize;
  Application.Title := 'FileMenuTest';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
