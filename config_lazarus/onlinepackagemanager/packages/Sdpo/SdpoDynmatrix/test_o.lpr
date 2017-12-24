program test_o;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  heaptrc,
  Interfaces, // this includes the LCL widgetset
  Forms
  { you can add units after this }, main, sdpoDynmatrix;

begin
  Application.Initialize;
  Application.CreateForm(TFMain, FMain);
  Application.Run;

  SetHeapTraceOutput('heap_report.txt');
end.

