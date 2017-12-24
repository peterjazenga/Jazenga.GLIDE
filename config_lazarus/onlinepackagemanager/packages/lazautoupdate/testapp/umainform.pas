unit umainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, ComCtrls,
  Buttons, StdCtrls,LazFileUtils,FileUtil,
  ulazautoupdate,eventlog,Dialogs;
CONST
{$IFDEF WINDOWS}
   {$IFDEF CPU32}
     {$IFDEF DEBUGMODE}
      C_VERSIONSINNAME = 'testappwin32debug.ini';
      C_ZIPFILENAME = 'testappwin32debug.zip';
      C_LogFileName = 'testappwin32debuglog.txt';
     {$ELSE}
      C_VERSIONSINNAME = 'testappwin32.ini';
      C_ZIPFILENAME = 'testappwin32.zip';
      C_LogFileName = 'testappwin32log.txt';
      {$ENDIF}
   {$ENDIF}
   {$IFDEF CPU64}
      C_VERSIONSINNAME = 'testappwin64.ini';
      C_ZIPFILENAME = 'testappwin64.zip';
      C_LogFileName = 'testappwin64log.txt';
   {$ENDIF}
{$ENDIF}
  {$IFDEF LINUX}
   {$IFDEF CPU32}
      C_VERSIONSINNAME = 'testapplinux32.ini';
      C_ZIPFILENAME = 'testapplinux32.zip';
      C_LogFileName = 'testapplinux32log.txt';
   {$ENDIF}
   {$IFDEF CPU64}
      C_VERSIONSINNAME = 'testapplinux64.ini';
      C_ZIPFILENAME = 'testapplinux64.zip';
      C_LogFileName = 'testapplinux64log.txt';
   {$ENDIF}
{$ENDIF}

type

  { Tmainform }

  Tmainform = class(TForm)
    chk_DebugMode: TCheckBox;
    chk_ShowDialogs: TCheckBox;
    chk_ShowProgress: TCheckBox;
    cmd_DeleteDesktopShortcut: TButton;
    cmd_MakeDesktopShortcut: TButton;
    cmd_AutoUpdate: TButton;
    cmd_updateToNewVersion: TButton;
    cmd_DownloadNewVersion: TButton;
    cmd_NewVersionAvailable: TButton;
    cmd_close: TBitBtn;
    lbl_Version: TLabel;
    LazAutoUpdate1: TLazAutoUpdate;
    StatusBar1: TStatusBar;
    procedure chk_DebugModeChange(Sender: TObject);
    procedure chk_ShowDialogsChange(Sender: TObject);
    procedure chk_ShowProgressChange(Sender: TObject);
    procedure cmd_AutoUpdateClick(Sender: TObject);
    procedure cmd_DeleteDesktopShortcutClick(Sender: TObject);
    procedure cmd_DownloadNewVersionClick(Sender: TObject);
    procedure cmd_MakeDesktopShortcutClick(Sender: TObject);
    procedure cmd_NewVersionAvailableClick(Sender: TObject);
    procedure cmd_SilentUpdateClick(Sender: TObject);
    procedure cmd_updateToNewVersionClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LazAutoUpdate1DebugEvent(Sender: TObject; lauMethodName,
      lauMessage: string);
    procedure LazAutoUpdate1Downloaded(Sender: TObject; ResultCode,
      BytesDownloaded: integer);
    procedure LazAutoUpdate1NewVersionAvailable(Sender: TObject;
      Newer: boolean; OnlineVersion: string);
  private
     Logger: TEventLog;
     procedure WriteAndLog(szText: string);
     procedure CloseLog;
  public

  end;

var
  mainform: Tmainform;

implementation

{$R *.lfm}

{ Tmainform }
procedure Tmainform.CloseLog;
begin
  If Assigned(Logger) then
   begin
    If Logger.Active then Logger.Info('End of Log');
    Logger.Active:=False;
   end;
end;

procedure Tmainform.WriteAndLog(szText: string);
begin
  Logger.Info(szText);
end;

procedure Tmainform.FormCreate(Sender: TObject);
begin
  LazAutoUpdate1.VersionsININame:=C_VERSIONSINNAME;
  LazAutoUpdate1.ZipfileName:=C_ZIPFILENAME;
  LazAutoUpdate1.GitHubProjectname:='lazarusccr';
  LazAutoUpdate1.GitHubRepositoryName:='TestApp';
  LazAutoUpdate1.GitHubBranchOrTag:='updates';
  LazAutoUpdate1.ShowUpdateInCaption:=TRUE;
  Caption:=Application.Title;
  if FileExistsUTF8(C_LogFileName) then
    DeleteFile(C_LogFileName);
  Application.Processmessages;
  Logger := TEventLog.Create(nil);
  TRY
  Logger.LogType := ltFile;
  Logger.FileName := C_LogFileName;
  Logger.Active := True;
  Logger.Info('Testing Logging functionality');
  Except
    Raise Exception.Create('Trouble with the logger. Click OK to quit');
    If Assigned(Logger) then FreeAndNil(Logger);
    Application.Terminate;
  end;
  Logger.Active := False;
  if FileExistsUTF8(C_LogFileName) then
    DeleteFile(C_LogFileName);
  // FORCE AN UPDATE EVERY TIME HERE?
  // LazAutoUpdate1.AppVersion:='0.0.0.0';
  lbl_Version.Caption:='Version: ' + LazAutoUpdate1.AppVersion;
end;

procedure Tmainform.FormDestroy(Sender: TObject);
begin
  CloseLog;
  If Assigned(Logger) then FreeAndNil(Logger);
end;

procedure Tmainform.cmd_NewVersionAvailableClick(Sender: TObject);
begin
  LazAutoUpdate1.NewVersionAvailable;
end;

procedure Tmainform.cmd_SilentUpdateClick(Sender: TObject);
begin
end;

procedure Tmainform.cmd_updateToNewVersionClick(Sender: TObject);
begin
{$IFDEF DEBUGMODE}
  ShowMessage('Please do not try updating in DEBUG mode');
{$ELSE}
  CloseLog;
  LazAutoUpdate1.UpdateToNewVersion;
{$ENDIF}
end;

procedure Tmainform.FormActivate(Sender: TObject);
begin
   LazAutoUpdate1.ShowWhatsNewIfAvailable;
end;

procedure Tmainform.cmd_DownloadNewVersionClick(Sender: TObject);
begin
    LazAutoUpdate1.DownloadNewVersion;
end;

procedure Tmainform.cmd_MakeDesktopShortcutClick(Sender: TObject);
begin
  LazAutoUpdate1.ShortCut.ShortCutName:=Application.Title;
  LazAutoUpdate1.ShortCut.Target:=Application.EXEName;
  LazAutoUpdate1.ShortCut.IconFilename:=ChangeFileExt(Application.EXEName,'.ico');
  LazAutoUpdate1.ShortCut.Category:=scUtility;
  If LazAutoUpdate1.MakeShortCut then
   ShowMessage('Success! New shortcut is now on your desktop')
  else
   ShowMessage('MakeShortCut failed');
end;

procedure Tmainform.cmd_AutoUpdateClick(Sender: TObject);
begin
  {$IFDEF DEBUGMODE}
    ShowMessage('Please do not try updating in DEBUG mode');
  {$ELSE}
  CloseLog;
  LazAutoUpdate1.AutoUpdate;
  {$ENDIF}
end;

procedure Tmainform.chk_DebugModeChange(Sender: TObject);
begin
  If chk_DebugMode.Checked then
   begin
    Logger.Active := True;
    Logger.Info('Logging started');
    LazAutoUpdate1.DebugMode:=True;
   end
  else
  begin
    LazAutoUpdate1.DebugMode:=False;
    Logger.Info('Logging ended');
    Logger.Active := False;
  end;
end;

procedure Tmainform.chk_ShowDialogsChange(Sender: TObject);
begin
   LazAutoUpdate1.ShowDialogs:=chk_ShowDialogs.Checked;
end;

procedure Tmainform.chk_ShowProgressChange(Sender: TObject);
begin
    LazAutoUpdate1.ShowUpdateInCaption:=chk_ShowProgress.Checked;
end;

procedure Tmainform.cmd_DeleteDesktopShortcutClick(Sender: TObject);
begin
  LazAutoUpdate1.ShortCut.ShortCutName:=Application.Title;
  If LazAutoUpdate1.DeleteShortCut then
   ShowMessage('Success! New shortcut is toast')
  else
   ShowMessage('DeleteShortCut failed');

end;

procedure Tmainform.LazAutoUpdate1DebugEvent(Sender: TObject; lauMethodName,
  lauMessage: string);
begin
   StatusBar1.SimpleText:='Debug Message: (' + lauMethodName + ') ' + lauMessage;
   WriteAndLog(StatusBar1.SimpleText);
end;

procedure Tmainform.LazAutoUpdate1Downloaded(Sender: TObject; ResultCode,
  BytesDownloaded: integer);
begin
  StatusBar1.SimpleText:=Format('Downloaded. StatusCode=%d BytesDownloaded=%d',
  [ResultCode,BytesDownloaded]);
  WriteAndLog(StatusBar1.SimpleText);
end;

procedure Tmainform.LazAutoUpdate1NewVersionAvailable(Sender: TObject;
  Newer: boolean; OnlineVersion: string);
begin
  If Newer then
   StatusBar1.SimpleText:='New version available. Online Version is ' + OnlineVersion
  else
    StatusBar1.SimpleText:='Online version is not newer. Online Version is ' + OnlineVersion;

  WriteAndLog(StatusBar1.SimpleText);

end;

end.

