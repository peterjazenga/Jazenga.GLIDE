program JvNavPaneDemo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here }, JvNavPaneDemoMainForm;

begin
  Application.Initialize;
  Application.CreateForm(TJvNavPaneDemoMainFrm, JvNavPaneDemoMainFrm);
  Application.Run;
end.

