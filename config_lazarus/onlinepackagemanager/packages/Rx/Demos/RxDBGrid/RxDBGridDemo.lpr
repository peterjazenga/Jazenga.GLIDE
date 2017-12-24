program RxDBGridDemo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  sysutils,
  RxDBGridMainUnit;

{$R RxDBGridDemo.res}

begin
  LocalizeApp;
  DefaultFormatSettings.ThousandSeparator:=' ';
  //
  Application.Title:='RxDBGrid demo';
  Application.Initialize;
  Application.CreateForm(TRxDBGridMainForm, RxDBGridMainForm);
  Application.Run;
end.

