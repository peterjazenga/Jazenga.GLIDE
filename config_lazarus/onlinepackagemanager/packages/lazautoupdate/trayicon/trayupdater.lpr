program trayupdater;
{ LazAutoUpdate Tray Updater

  Copyright (C)2014 Gordon Bamber minesadorada@charcodelvalle.com

  An example of using LazAutoUpdate as a ´start when windows starts' way

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}
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
  Forms, umainform, uconfigform;

{$R *.res}
{
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
}
begin
  Application.Title:='LazAutoUpdater';
  // Application.Title:=Application.Title + ' (' + C_PFX + ' edition)';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(Tmainform, mainform);
  Application.CreateForm(Tconfigform, configform);
  Application.ShowMainForm := false;
  Application.Run;
end.

