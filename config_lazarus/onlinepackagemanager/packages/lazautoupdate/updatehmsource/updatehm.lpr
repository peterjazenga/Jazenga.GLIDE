program updatehm;
{$ifdef Linux}

  {$ifdef FPC_CROSSCOMPILING}

    {$ifdef CPUARM}
      //if GUI, then uncomment

      //{$linklib GLESv2}

    {$endif}

    {$linklib libc_nonshared.a}

  {$endif}

{$endif}

{ LazAutoUpdate companion application
  This application should be deployed in the same directory
  as any application that uses the LazAutoUpdate component.
  Actions:
  1. Checks for correct parameters
  2. Copies a whatsnew.txt file from the updates directory to the ProgramDirectory
  3. Waits for 2 seconds whilst the calling app shuts itself down
  4. If present, copies TrayNotifier info to the common data directory
     and adds a new entry: Location=<ProgramDirectory>
  5(a) If CopyTree=TRUE, copies the whole directory tree from
     the updates folder to the ProgramDirectory
  5(b) If CopyTree=FALSE, copies the new executable from the
     updates folder to the ProgramDirectory
  6. Uses TProcess to start up the new executable
  7. Closes.

  Note that an application log (C_LogFileName) is written anew, and
  can be checked for error information

  Copyright (C)2014 Gordon Bamber (minesadorada@charcodelvalle.com)

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

// Takes 5 parameters

uses
  SysUtils,
  LazUTF8,LazFileUtils,FileUtil,
  process,
  eventlog,
  DateUtils,
  inifiles,
  Classes;

{$R *.res}
const
  C_AppPrettyName = 'Lazarus Auto-Updater';
  C_WhatsNewFileName = 'whatsnew.txt';
  C_Version = '0.0.15';
  C_UpdatesDirectory = 'updates';
  C_LogFileName = 'updatehmlog.txt';
  C_LAUTRayINI = 'lauimport.ini';
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
{$IFDEF WINDOWS}
  C_Appname = 'lazautoupdateexample' + C_PFX + '.exe';
  C_LAUTrayApp = 'lautraynotify' + C_PFX + '.exe';
{$ELSE}
  C_Appname = 'lazautoupdateexample' + C_PFX;
  C_LAUTrayApp = 'lautraynotify' + C_PFX;
{$ENDIF}

var
  szAppName, szUpdatesFolder, szWhatsNewName, szPrettyName: string;
  szUpdatedEXEPath, szOldEXEPath: string;
  szUpdatedWNPath, szOldWNPath: string;
  szUpdatesFullPath: string;
  AppProcess: TProcess;
  Logger: TEventLog;
  bCopyTreeSuccess: boolean;
  szLAUTrayAppPath: string;
  INI: TINIFile;
  SectionStringList: TStrings;

  procedure WaitFor(const MillisecondDelay: longword);
  // Linux - this proc is intentionally thread-blocking
  var
    ThisSecond: longword;
  begin
    ThisSecond := MilliSecondOfTheDay(Now);
    while MilliSecondOfTheDay(Now) < (ThisSecond + MillisecondDelay) do ;
  end;

  procedure WriteAndLog(szText: string);
  begin
    Logger.Info(szText);
    writeln(szText);
  end;

begin
  if ParamCount = 0 then
    begin
    WriteLn('==========================================================');
    Writeln(LineEnding + '==== updatehm v' + C_Version +
      ' - an lazautoupdate application ====');
    Writeln('Usage: updatehm exename.exe [updatesfoldername] [whatnewfilename] [exePrettyName] [copytree]');
    WriteLn('==========================================================');
    WriteLn('Press any key to continue');
    ReadLn;
    Halt;
    end;

  if (ParamStrUTF8(1) = '-h') or (ParamStrUTF8(1) = '/h') then
    begin
    WriteLn('==========================================================');
    Writeln('Normal usage: updatehm exename.exe [updatesfoldername] [whatnewfilename] [exePrettyName] [copytree]');
    WriteLn('- where exename.exe is a single application to be updated');
    WriteLn(LineEnding);
    WriteLn('optional parameters are');
    WriteLn('-h or /h - this screen');
    WriteLn('==========================================================');
    WriteLn('Press any key to continue');
    ReadLn;
    Halt;
    end;

  Logger := TEventLog.Create(nil);
  Logger.LogType := ltFile;
  Logger.FileName := C_LogFileName;
  if FileExistsUTF8(C_LogFileName) then
    DeleteFile(C_LogFileName);
  Logger.Active := True;
  Logger.Info('Start of Log');

  bCopyTreeSuccess := False;

  szAppName := ParamStrUTF8(1);
  if szAppName = '' then
    szAppname := C_Appname;

  szUpdatesFolder := ParamStrUTF8(2);
  if szUpdatesFolder = '' then
    szUpdatesFolder := C_UpdatesDirectory;

  szWhatsNewName := ParamStrUTF8(3);
  if szWhatsNewName = '' then
    szWhatsNewName := C_WhatsNewFileName;

  szPrettyName := ParamStrUTF8(4);
  if szPrettyName = '' then
    szPrettyName := C_AppPrettyName;

  WriteAndLog('updatehm version ' + C_Version);
  Logger.Info('Given parameters follow:');
  Logger.Info('App name = ' + szAppName);
  Logger.Info('Updates folder = ' + szUpdatesFolder);
  Logger.Info('WhatsNew filename = ' + szWhatsNewName);
  Logger.Info('Pretty Name = ' + szPrettyName);
  if ParamStrUTF8(5) = 'copytree' then
    Logger.Info('CopyTree = TRUE')
  else
    Logger.Info('CopyTree = FALSE');
  WriteAndLog('Please wait.  Updating ' + szPrettyName + '....');
  // Set up paths

  szOldEXEPath := CleanAndExpandFilename(ProgramDirectory + PathDelim + szAppName);
  Logger.Info('Application path = ' + szOldEXEPath);
  szUpdatedEXEPath := CleanAndExpandFilename(ProgramDirectory +
    szUpdatesFolder + PathDelim + szAppName);
  Logger.Info('Updated application path = ' + szUpdatedEXEPath);
  szOldWNPath := CleanAndExpandFilename(ProgramDirectory + PathDelim + szWhatsNewName);
  Logger.Info('WhatsNew path = ' + szOldWNPath);
  szUpdatedWNPath := CleanAndExpandFilename(ProgramDirectory +
    szUpdatesFolder + PathDelim + szWhatsNewName);
  Logger.Info('Updated whatsNew path = ' + szUpdatedWNPath);

  if not FileExistsUTF8(szUpdatedEXEPath) then
    begin
    WriteAndLog('Couldn''t find szUpdatedEXEPath');
    Logger.Active := False;
    FreeAndNil(Logger);
    Halt(1);
    end;


  if FileExistsUTF8(szUpdatedWNPath) then
    begin
    // Write a file to disk that the app keeps checking for.
      try
      Fileutil.CopyFile(szUpdatedWNPath, szOldWNPath, [cffOverWriteFile]);
      WriteAndLog('Writing.. ' + szWhatsNewName);
      except
      On E: Exception do
        begin
        WriteAndLog(Format('There was a problem writing %s.  Reason: %s',
          [szWhatsNewName, E.Message]));
        Logger.Active := False;
        FreeAndNil(Logger);
        Halt(1);
        end;
      end;
    end;

  // The calling app is in a loop - waiting to detect
  // a Whatsnew file in it's home directory
  // As soon as the call: If FileExists(WhatsNew) returns TRUE
  // then the app will close itself
  // The following code is to give the App time to do all this..
{$IFDEF HAS_SLEEP}
  WriteAndLog('Sleeping...');
  Sleep(2000); // give time for calling app to close
{$ELSE}
  WriteAndLog('Waiting 2 seconds...');
  WaitFor(2000);
{$ENDIF}
  // App should now be closed.  OK to overwrite it.

  // Define Updates directory
  szUpdatesFullPath := CleanAndExpandDirectory(ProgramDirectory + szUpdatesFolder);

  // Deal with C_LAUTRayINI
  // Copied to the global application data folder
  // Add entry 'Location'
  if FileExistsUTF8(szUpdatesFullPath + C_LAUTRayINI) then
    begin
    szLAUTrayAppPath := GetAppConfigDirUTF8(False, True);;
      try
      ForceDirectory(szLAUTrayAppPath);
      Fileutil.CopyFile(szUpdatesFullPath + C_LAUTRayINI, szLAUTrayAppPath +
        C_LAUTRayINI, [cffOverWriteFile]);
      WriteAndLog(Format('Successfully copied %s to %s ',
        [C_LAUTRayINI, szLAUTrayAppPath]));
      if FileExistsUTF8(szLAUTrayAppPath + C_LAUTRayINI) then
        begin
        INI := TINIFile.Create(szLAUTrayAppPath + C_LAUTRayINI);
        SectionStringList := TStringList.Create;
          try
          INI.ReadSections(SectionStringList);
          if SectionStringList.Count > 0 then
            begin
            INI.WriteString(SectionStringList[0], 'Location', ProgramDirectory);
            WriteAndLog(Format('Wrote new entry in section %s.  Location=%s',
              [SectionStringList[0], ProgramDirectory]));
            end
          else
            WriteAndLog('Failed to find a valid section in ' + C_LAUTRayINI);
          finally
          FreeAndNil(SectionStringList);
          FreeAndNil(INI);
          end;
        end
      else
        WriteAndLog('Failed to copy ' + C_LAUTRayINI + ' to ' + szLAUTrayAppPath);
      except
      On E: Exception do
        WriteAndLog(Format('Could not copy %s.  Error: %s ', [C_LAUTrayApp, E.Message]));
      end;
    end;


  // If CopyTree, then just copy the whole of the /updates folder (inc directories) to the App Directory
    try
    if ParamStrUTF8(5) = 'copytree' then
      // Copy a whole directory tree?
      bCopyTreeSuccess := CopyDirTree(szUpdatesFullPath, ProgramDirectory,
        [cffOverwriteFile, cffCreateDestDirectory]);
    except
    On E: Exception do
      WriteAndLog('Copytree error: ' + E.Message);
    end;

  // CopyTree not specified, or operation failed
  // Just copy the updated app over the old one
  if FileExistsUTF8(szUpdatedEXEPath) then
    begin
    if (bCopyTreeSuccess = False) then
      // Copy over exe file
      if Fileutil.CopyFile(szUpdatedEXEPath, szOldEXEPath, [cffOverWriteFile]) then
        begin
        WriteAndLog('Copying over executeable, then sleeping...');
  {$IFDEF HAS_SLEEP}
        WriteAndLog('Sleeping...');
        Sleep(1000); // give more time for calling app to close
  {$ELSE}
        WriteAndLog('Waiting 1 second...');
        WaitFor(1000);
  {$ENDIF}
        end
      else
        WriteAndLog('Copy to ' + szUpdatedEXEPath + ' failed');

    // Restart updated app
    if FileExistsUTF8(szOldEXEPath) then
      begin
      WriteAndLog('Restarting ' + szPrettyName + '...');
      AppProcess := TProcess.Create(nil);
        try
        AppProcess.CurrentDirectory := ExtractFileDir(szOldEXEPath);
        AppProcess.Executable := szOldEXEPath;
        AppProcess.StartupOptions := [suoUseShowWindow];
        AppProcess.Execute;
        finally
        AppProcess.Free;
        end;
      end
    else
      WriteAndLog('Failure. Couldn''t find ' + szOldEXEPath);
    end
  else
    WriteAndLog('Failure. Couldn''t find ' + szUpdatedEXEPath);

  Logger.Info('End of Log');
  Logger.Active := False;
  FreeAndNil(Logger);
end.
