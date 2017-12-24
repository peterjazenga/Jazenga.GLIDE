program updatepack;
{Note to self, Code Template: type newpascal then CTRL-J}
{$ifdef Linux}
  {$ifdef FPC_CROSSCOMPILING}
    {$ifdef CPUARM}
      //if GUI on RPi, then uncomment
      //{$linklib GLESv2}
    {$endif}
    {$linklib libc_nonshared.a}
  {$endif}
{$endif}

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, umainform, umemoform;

{$R *.res}
Const
  {$IFDEF WINDOWS}
  C_OS = 'win';
  {$ELSE}
  C_OS = 'linux';
  {$ENDIF}
  {$IFDEF CPU32}
  C_BITNESS = '32';
  {$ELSE}
  C_BITNESS = '64';
  {$ENDIF}
  C_PFX = C_OS + C_BITNESS;
begin
  Application.Title:='LazAutoUpdate Update Pack';
  // Line below gives unique folders in GetAppConfig call
  // Application.Title:=Application.Title + ' (' + C_PFX + ' edition)';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(Tmainform, mainform);
  Application.CreateForm(TMemoForm, MemoForm);
  Application.Run;
end.

