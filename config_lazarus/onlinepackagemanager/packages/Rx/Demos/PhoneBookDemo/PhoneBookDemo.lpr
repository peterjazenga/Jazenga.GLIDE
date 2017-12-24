program PhoneBookDemo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  pbMainUnit,
  pbEditDataUnit;

{$R PhoneBookDemo.res}

begin
  Application.Title:='Phone book demo';
  Application.Initialize;
  Application.CreateForm(TpbMainForm, pbMainForm);
  Application.Run;
end.

