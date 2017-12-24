program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  Unit1,
  sysutils
  { you can add units after this };
{$R *.res}

begin
  DefaultFormatSettings.ShortDateFormat:='dd.mm.yyyy';
  DefaultFormatSettings.DateSeparator:='.';
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

