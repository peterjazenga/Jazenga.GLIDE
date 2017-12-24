unit umainform;

{
License (MIT)
=============
LazAutoUpdate Universal Installer (c)2017 Gordon Bamber (minesadorada@charcodelvalle.com)

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to
  deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
  IN THE SOFTWARE.
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazUTF8, FileUtil, LazFileUtils, Forms, Controls,
  Graphics, Dialogs, Buttons,
  Menus, StdCtrls, ExtCtrls, asyncprocess,
  ulazautoupdate, eventlog;

// resourcestring


type

  { Tmainform }

  Tmainform = class(TForm)
    cmd_DeleteShortcutIcon: TButton;
    cmd_MakeShortcutIcon: TButton;
    cmd_Run: TButton;
    cmd_Install: TButton;
    cmd_close: TBitBtn;
    grp_Action: TGroupBox;
    LazAutoUpdate1: TLazAutoUpdate;
    MainMenu1: TMainMenu;
    mnu_helpCheckForUpdates: TMenuItem;
    mnu_help: TMenuItem;
    mnu_fileExit: TMenuItem;
    mnu_file: TMenuItem;
    grp_Application: TRadioGroup;
    procedure cmd_DeleteShortcutIconClick(Sender: TObject);
    procedure cmd_InstallClick(Sender: TObject);
    procedure cmd_MakeShortcutIconClick(Sender: TObject);
    procedure cmd_RunClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure grp_ApplicationSelectionChanged(Sender: TObject);
    procedure LazAutoUpdate1DebugEvent(Sender: TObject;
      lauMethodName, lauMessage: string);
    procedure mnu_fileExitClick(Sender: TObject);
    procedure mnu_helpCheckForUpdatesClick(Sender: TObject);
  private
    Logger: TEventLog;
    procedure ConfigureLazAutoUpdate(const AItemIndex: integer);
    procedure RunInstalledApp;
  public

  end;

var
  mainform: Tmainform;

implementation

{$R *.lfm}
var
  sDirectoryToInstallTo: string;

{ Tmainform }

procedure Tmainform.FormCreate(Sender: TObject);
begin
  Caption := Application.Title;
  Icon := Application.Icon;
  sDirectoryToInstallTo := ProgramDirectory + 'installed';
  LazAutoUpdate1.DebugMode := True;
  LazAutoUpdate1.ShowUpdateInCaption := True;
  ConfigureLazAutoUpdate(2); // Default is TestApp
  Logger := TEventLog.Create(nil);
  Logger.LogType := ltFile;
  Logger.FileName := Application.Title + '.log';
  Logger.Active := True; // Logging uses OnDebugEvent of LazAutoUpdate
end;

procedure Tmainform.FormShow(Sender: TObject);
begin

end;

procedure Tmainform.cmd_InstallClick(Sender: TObject);
begin
  LazAutoUpdate1.WorkingMode := lauInstall;
  // Uses SilentUpdate method to install the app if it isn't running
  // Else uses UpdateToNewVersion to close/update it

  // Let the component do the work!
  if LazAutoUpdate1.DownloadNewVersion then
    if LazAutoUpdate1.UpdateToNewVersion then
    begin
      ShowMessageFmt('Success. %s installed.', [LazAutoUpdate1.AppFileWithPath]);
    end
    else
    begin
      ShowMessageFmt('Failure.  See logfile %s', [Logger.FileName]);
    end;
  // Restore standard mode
  LazAutoUpdate1.WorkingMode := lauUpdate;
end;

procedure Tmainform.cmd_DeleteShortcutIconClick(Sender: TObject);
begin
  if not FileExistsUTF8(LazAutoUpdate1.AppFileWithPath) then
  begin
    ShowMessageFmt('%s does not exist! Install it first.',
      [LazAutoUpdate1.AppFileWithPath]);
    Exit;
  end;
  if LazAutoUpdate1.DeleteShortCut then
    ShowMessage('Desktop shortcut and menu item are toast');
end;

procedure Tmainform.cmd_MakeShortcutIconClick(Sender: TObject);
begin
  if not FileExistsUTF8(LazAutoUpdate1.AppFileWithPath) then
  begin
    ShowMessageFmt('%s does not exist! Install it first.',
      [LazAutoUpdate1.AppFileWithPath]);
    Exit;
  end;
  if LazAutoUpdate1.MakeShortCut then
    ShowMessage('Desktop shortcut and menu item created');
end;

procedure Tmainform.cmd_RunClick(Sender: TObject);
begin
  RunInstalledApp;
end;

procedure Tmainform.FormActivate(Sender: TObject);
begin
  LazAutoUpdate1.ShowWhatsNewIfAvailable;
end;

procedure Tmainform.RunInstalledApp;
var
  AProcess: TAsyncProcess;
begin
  if not FileExistsUTF8(LazAutoUpdate1.AppFileWithPath) then
  begin
    ShowMessageFmt('%s does not exist! Install it first.',
      [LazAutoUpdate1.AppFileWithPath]);
    Exit;
  end;
  AProcess := TAsyncProcess.Create(nil);
  try
    AProcess.Executable := LazAutoUpdate1.AppFileWithPath;
    AProcess.Execute;
  finally
    Aprocess.Free;
  end;
end;

procedure Tmainform.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Assigned(Logger) then
  begin
    Logger.Active := False;
    FreeAndNil(Logger);
  end;
end;

procedure Tmainform.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if LazAutoUpdate1.DownloadInProgress then
  begin
    CanClose := False;
    ShowMessage('Please wait. Download is still in progress.');
  end;
end;

procedure Tmainform.grp_ApplicationSelectionChanged(Sender: TObject);
begin
  ConfigureLazAutoUpdate(grp_Application.ItemIndex);
end;

procedure Tmainform.LazAutoUpdate1DebugEvent(Sender: TObject;
  lauMethodName, lauMessage: string);
begin
  Logger.Log(lauMethodName + ' - ' + lauMessage);
end;

procedure Tmainform.mnu_fileExitClick(Sender: TObject);
begin
  Close;
end;

procedure Tmainform.mnu_helpCheckForUpdatesClick(Sender: TObject);
var
  OldItemIndex: integer;
begin
  OldItemIndex := grp_Application.ItemIndex;
  LazAutoUpdate1.ProjectType := auSourceForge; // can be auGitHubReleaseZip or auOther
  LazAutoUpdate1.SFProjectname := 'lazautoupdate';  // Or GitHub properties
  LazAutoUpdate1.UpdatesFolder := 'updates'; // Subfolder in repository
  LazAutoUpdate1.VersionsININame := 'lauinstaller' + C_PFX + '.ini'; // as specified
  LazAutoUpdate1.ZipfileName := 'lauinstaller' + C_PFX + '.zip'; // as specified
  LazAutoUpdate1.AppFileWithPath := Application.Exename;
  If NOT LazAutoUpdate1.AutoUpdate then
     ConfigureLazAutoUpdate(OldItemIndex); // Restore properties

end;

procedure Tmainform.ConfigureLazAutoUpdate(const AItemIndex: integer);
begin
  // Note: This routine relies on a consistent naming convention for your
  // executables in Windows 32/64-bit and Linux 32/64-bit (see Const C_PFX declaration)
  case AItemIndex of
    0: //Update Pack
    begin
      LazAutoUpdate1.ProjectType := auSourceForge;
      // can be auGitHubReleaseZip or auOther
      LazAutoUpdate1.SFProjectname := 'lazautoupdate';  // Or GitHub properties
      LazAutoUpdate1.UpdatesFolder := 'updates'; // Subfolder in repository
      LazAutoUpdate1.VersionsININame := 'updatepack' + C_PFX + '.ini'; // as specified
      LazAutoUpdate1.ZipfileName := 'updatepack' + C_PFX + '.zip'; // as specified
      // Note: sDirectoryToInstallTo does not end in a DirectorySeparator
      {$IFDEF WINDOWS}
      LazAutoUpdate1.AppFileWithPath := sDirectoryToInstallTo +
        DirectorySeparator + 'updatepack.exe';
      {$ELSE}
      LazAutoUpdate1.AppFileWithPath :=
        sDirectoryToInstallTo + DirectorySeparator + 'updatepack';
      {$ENDIF}
      // Our responsibility to make the folder
      if not DirectoryExistsUTF8(sDirectoryToInstallTo) then
        ForceDirectoriesUTF8(sDirectoryToInstallTo);
      LazAutoUpdate1.Appversion := '0.0.0.0';
      LazAutoUpdate1.ShortCut.Category := scUtility;
      LazAutoUpdate1.ShortCut.Target := LazAutoUpdate1.AppFileWithPath;
      LazAutoUpdate1.ShortCut.ShortcutName := 'LazAutoUpdate Update Pack';
    end;
    1: // Test Application (GitHub)
    begin
      LazAutoUpdate1.ProjectType := auGitHubReleaseZip;
      LazAutoUpdate1.GitHubProjectname := 'lazarusccr';
      LazAutoUpdate1.GitHubRepositoryName := 'TestApp';
      LazAutoUpdate1.GitHubBranchOrTag := 'updates';
      LazAutoUpdate1.UpdatesFolder := 'updates';
      LazAutoUpdate1.VersionsININame := 'testapp' + C_PFX + '.ini';
      LazAutoUpdate1.ZipfileName := 'testapp' + C_PFX + '.zip';
      {$IFDEF WINDOWS}
      LazAutoUpdate1.AppFileWithPath :=
        sDirectoryToInstallTo + DirectorySeparator + 'testapp' + C_PFX + '.exe';
      {$ELSE}
      LazAutoUpdate1.AppFileWithPath :=
        sDirectoryToInstallTo + DirectorySeparator + 'testapp' + C_PFX;
      {$ENDIF}
      if not DirectoryExistsUTF8(sDirectoryToInstallTo) then
        ForceDirectoriesUTF8(sDirectoryToInstallTo);
      LazAutoUpdate1.Appversion := '0.0.0.0';
      LazAutoUpdate1.ShortCut.Category := scUtility;
      LazAutoUpdate1.ShortCut.Target := LazAutoUpdate1.AppFileWithPath;
      LazAutoUpdate1.ShortCut.ShortcutName := 'LazAutoUpdate Test App';
    end;
    2: // Test Application (SourceForge)
    begin
      LazAutoUpdate1.ProjectType := auSourceForge;
      LazAutoUpdate1.SFProjectname := 'lazautoupdate';
      LazAutoUpdate1.UpdatesFolder := 'updates';
      LazAutoUpdate1.VersionsININame := 'testapp' + C_PFX + '.ini';
      LazAutoUpdate1.ZipfileName := 'testapp' + C_PFX + '.zip';
      {$IFDEF WINDOWS}
      LazAutoUpdate1.AppFileWithPath :=
        sDirectoryToInstallTo + DirectorySeparator + 'testapp' + C_PFX + '.exe';
      {$ELSE}
      LazAutoUpdate1.AppFileWithPath :=
        sDirectoryToInstallTo + DirectorySeparator + 'testapp' + C_PFX;
      {$ENDIF}
      if not DirectoryExistsUTF8(sDirectoryToInstallTo) then
        ForceDirectoriesUTF8(sDirectoryToInstallTo);
      LazAutoUpdate1.Appversion := '0.0.0.0';
      LazAutoUpdate1.ShortCut.Category := scUtility;
      LazAutoUpdate1.ShortCut.Target := LazAutoUpdate1.AppFileWithPath;
      LazAutoUpdate1.ShortCut.ShortcutName := 'LazAutoUpdate Test App';
    end;
    3: // Retro Ski Run
    begin
      LazAutoUpdate1.ProjectType := auSourceForge;
      LazAutoUpdate1.SFProjectname := 'lazautoupdate';
      LazAutoUpdate1.UpdatesFolder := 'updates';
      LazAutoUpdate1.VersionsININame := 'ski' + C_PFX + '.ini';
      LazAutoUpdate1.ZipfileName := 'ski' + C_PFX + '.zip';
      {$IFDEF WINDOWS}
      LazAutoUpdate1.AppFileWithPath :=
        sDirectoryToInstallTo + DirectorySeparator + 'ski' + C_PFX + '.exe';
      {$ELSE}
      LazAutoUpdate1.AppFileWithPath :=
        sDirectoryToInstallTo + DirectorySeparator + 'ski' + C_PFX;
      {$ENDIF}
      if not DirectoryExistsUTF8(sDirectoryToInstallTo) then
        ForceDirectoriesUTF8(sDirectoryToInstallTo);
      LazAutoUpdate1.Appversion := '0.0.0.0';
      LazAutoUpdate1.ShortCut.Category := scGame;
      LazAutoUpdate1.ShortCut.Target := LazAutoUpdate1.AppFileWithPath;
      LazAutoUpdate1.ShortCut.ShortcutName := 'Retro Ski Run';
    end;
  end;
end;

end.
