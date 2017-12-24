unit umainform;

{ LazAutoUpdate Pack system

  Copyright (C)2014 Gordon Bamber (minesadorada@charcodelvalle.com)

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

  This application is a partner to the LazAutoUpdate Lazarus visual component.

  If your SourceForge-hosted application uses LazAutoUpdate, then this app
  can be used to manage updates throughout the lifetime of your product.

  Developed using Lazarus 1.2.4 and FPC 2.6.4
  OS: Windows 32/64-bit, Linux 32/64-bit
  Version as per project setting
  Licence: GPL.  Contact the author for conditions.
  September 2014
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazUTF8, LazFileUtils, FileUtil, Forms, Controls, Dialogs,
  Buttons, Menus, StdCtrls, EditBtn, Spin, ComCtrls, ulazautoupdate, inifiles,
  eventlog, umemoform, Zipper, strutils, asyncprocess, lclintf, types,LResources,
  LCLVersion;

type
  // Use FillVersionString and FillVersionInteger
  // to convert between VString and VIntegers
  TVersionRec = record
    VString: string;
    v1, v2, v3, v4: integer;
  end;
  // Used as intermediate storage
  TProfileRec = record
    ProfileName: string;
    AppPath: string;
    NewVersion: TVersionRec;
    OutDir: string;
    VersionsINI: string;
    WhatsNewPath: string;
    SFProjectName: string;
    ZipFileName: string;
    CopyTree: boolean;
    CopyTreeDirectory: string;
    SFUpdatesDirectory: string;
    ShowUpdatesInCaption: boolean;
    ShowDialogs: boolean;
    DebugMode: boolean;
    OverRideUserPrefs: boolean;
    IntervalType: word;
    IntervalDay: word;
    IntervalDate: word;
    IntervalHour: word;
    DragFileStringList: TStringList;
  end;
  { Tmainform }

  Tmainform = class(TForm)
    chk_overrideUserPrefs: TCheckBox;
    chk_DebugMode: TCheckBox;
    chk_ShowDialogs: TCheckBox;
    chk_ShowUpdateInCaption: TCheckBox;
    chk_UseCopyTree: TCheckBox;
    cmb_IntervalDate: TComboBox;
    cmb_IntervalDay: TComboBox;
    cmb_IntervalHour: TComboBox;
    cmb_IntervalType: TComboBox;
    cmd_DownloadLazAutoUpdate: TBitBtn;
    cmd_UseAppnameVersionsINI: TButton;
    cmd_UseAppnameZipfileName: TButton;
    cmd_NewProfile: TButton;
    cmd_DeleteProfile: TButton;
    cmd_MakeUpdatePack: TBitBtn;
    cmd_SaveProfile: TBitBtn;
    cmd_close: TBitBtn;
    cmb_profile: TComboBox;
    edt_Updates: TEdit;
    edt_CopyTree: TDirectoryEdit;
    edt_SFProjectName: TEdit;
    edt_ZipFileName: TFileNameEdit;
    edt_WhatsNewTextFile: TFileNameEdit;
    edt_OutputDirectory: TDirectoryEdit;
    edt_AppFileName: TFileNameEdit;
    edt_VersionsINIFile: TFileNameEdit;
    EventLog1: TEventLog;
    grp_LazAutoUpdater: TGroupBox;
    grp_dragfiles: TGroupBox;
    grp_Tweaks: TGroupBox;
    grp_CopyTree: TGroupBox;
    Label1: TLabel;
    lbl_IntervalDate: TLabel;
    lbl_IntervalDay: TLabel;
    lbl_IntervalType: TLabel;
    lbl_SFUpdatesDirectory2: TLabel;
    lbl_Updates: TLabel;
    lbl_CreateWhatsNew: TLabel;
    lbl_versionsinifile: TLabel;
    lbl_Profile: TLabel;
    lbl_outputdirectory: TLabel;
    LazAutoUpdate1: TLazAutoUpdate;
    lbl_AppFilename: TLabel;
    lbl_NewVersion: TLabel;
    lbl_OnlineProjectname: TLabel;
    lbl_ZipFileName: TLabel;
    lst_dragfiles: TListBox;
    MainMenu1: TMainMenu;
    Memo_intro: TMemo;
    mnuoptionsShowOnlineWebsite: TMenuItem;
    mnuoptionsDisplayFileManger: TMenuItem;
    mnuoptionsShowCodeInfo: TMenuItem;
    mnuoptionsAfterCreatePack: TMenuItem;
    mnuOptions: TMenuItem;
    mnu_helpAbout: TMenuItem;
    mnu_helpCheckForUpdates: TMenuItem;
    mnu_help: TMenuItem;
    mnu_fileSaveProfile: TMenuItem;
    mnu_fileClose: TMenuItem;
    mnu_file: TMenuItem;
    PageControl1: TPageControl;
    spd_WhatsNewCreate: TSpeedButton;
    spn1: TSpinEdit;
    spn2: TSpinEdit;
    spn3: TSpinEdit;
    spn4: TSpinEdit;
    Tab_advanced: TTabSheet;
    tab_intro: TTabSheet;
    tab_configure: TTabSheet;
    procedure chk_DebugModeChange(Sender: TObject);
    procedure chk_overrideUserPrefsClick(Sender: TObject);
    procedure chk_ShowDialogsChange(Sender: TObject);
    procedure chk_ShowUpdateInCaptionChange(Sender: TObject);
    procedure chk_UseCopyTreeChange(Sender: TObject);
    procedure cmb_IntervalTypeChange(Sender: TObject);
    procedure cmb_profileClick(Sender: TObject);
    procedure cmb_profileCloseUp(Sender: TObject);
    procedure cmd_DownloadLazAutoUpdateClick(Sender: TObject);
    procedure cmd_MakeUpdatePackClick(Sender: TObject);
    procedure cmd_UseAppnameVersionsINIClick(Sender: TObject);
    procedure cmd_DeleteProfileClick(Sender: TObject);
    procedure cmd_NewProfileClick(Sender: TObject);
    procedure cmd_SaveProfileClick(Sender: TObject);
    procedure cmd_UseAppnameZipfileNameClick(Sender: TObject);
    procedure edt_AppFileNameChange(Sender: TObject);
    procedure edt_AppFileNameKeyDown(Sender: TObject; var Key: word;
      Shift: TShiftState);
    procedure edt_CopyTreeChange(Sender: TObject);
    procedure edt_CopyTreeEditingDone(Sender: TObject);
    procedure edt_UpdatesChange(Sender: TObject);
    procedure edt_ZipFileNameChange(Sender: TObject);
    procedure edt_ZipFileNameEditingDone(Sender: TObject);
    procedure edt_VersionsINIFileChange(Sender: TObject);
    procedure edt_OutputDirectoryChange(Sender: TObject);
    procedure edt_OutputDirectoryKeyDown(Sender: TObject; var Key: word;
      Shift: TShiftState);
    procedure edt_SFProjectNameEditingDone(Sender: TObject);
    procedure edt_WhatsNewTextFileChange(Sender: TObject);
    procedure edt_WhatsNewTextFileKeyDown(Sender: TObject; var Key: word;
      Shift: TShiftState);
    procedure edt_ZipFileNameoldEditingDone(Sender: TObject);
    procedure edt_VersionsINIFileEditingDone(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure FormShow(Sender: TObject);
    procedure grp_dragfilesDblClick(Sender: TObject);
    procedure LazAutoUpdate1DebugEvent(Sender: TObject; WhereAt, Message: string);
    procedure LazAutoUpdate1Downloaded(Sender: TObject; ResultCode,
      BytesDownloaded: integer);
    procedure LazAutoUpdate1NewVersionAvailable(Sender: TObject;
      Newer: boolean; OnlineVersion: string);
    procedure LazAutoUpdate1Updated(Sender: TObject; NewVersion,
      LauMessage: String);
    procedure lst_dragfilesDblClick(Sender: TObject);
    procedure mnuoptionsDisplayFileMangerClick(Sender: TObject);
    procedure mnuoptionsShowCodeInfoClick(Sender: TObject);
    procedure mnuoptionsShowOnlineWebsiteClick(Sender: TObject);
    procedure mnu_fileCloseClick(Sender: TObject);
    procedure mnu_fileSaveProfileClick(Sender: TObject);
    procedure mnu_helpAboutClick(Sender: TObject);
    procedure mnu_helpCheckForUpdatesClick(Sender: TObject);
    procedure spd_WhatsNewCreateClick(Sender: TObject);
    procedure spn1Change(Sender: TObject);
    procedure spn2Change(Sender: TObject);
    procedure spn3Change(Sender: TObject);
    procedure spn4Change(Sender: TObject);
    procedure tab_configureContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: boolean);
  private
    { private declarations }
    AppConfig: TINIFile; // Application config file.  Holds current Profile name
    ProfileConfig: TINIFile;
    // Contains settings for all the saved profiles.  Each profile name is a section.
    ProfilenameList: TStringList; // Used to populate the combo box
    szCurrentProfileName: string; // Name used to Save/Load profiles to the INI file
    bCurrentProfileSaved, bComponentDownloaded, bIsVirgin: boolean;
    bShowCodeWindow, bShowFileManager, bShowOnlineWebsite,
    bOverRideUserPrefs, DebugMode: boolean;
    //    DragFileStringList:TStringList;
    procedure ReadProfileFromINI(AProfileName: string); // Writes ProfileRec to disk
    procedure WriteProfileToINI(AProfileName: string); // Reads ProfileRec from disk
    procedure DeleteCurrentProfile; // Deletes whatever profile is selected (Not DEFAULT)
    procedure WriteToGUI(AProfileName: string); // Writes GUI elements to ProfileRec
    procedure ReadFromGUI(AProfileName: string); // Reads GUI elements into ProfileRec
    // Conversion routines Version String <--> 4 version integers
    procedure FillVersionString(var AVersionRec: TVersionRec);
    procedure FillVersionInteger(var AVersionRec: TVersionRec);
    // Used by 'Create Update Pack' button
    function MakeZipfile: boolean;
    function MakeINIFile: boolean;
    function MakeLAUTrayINIFile: boolean;
    procedure DoEnableDisableIntervalCombos;
  public
    // ProfileRec is working storage for all the elements of the current profile
    // Filled from the cfg file at startup or user profile change
    // Only written to the cfg file if user chooses to do so
    ProfileRec: TProfileRec;
    { public declarations }
  end;


var
  mainform: Tmainform;
  sReadMePath:String;
  aLRes: TLResource;
  S: TResourceStream;
  F: TFileStream;

implementation

{$R *.lfm}
const
  C_APPCONFIGNAME = 'updatepack.cfg';
  C_PROFILECONFIGNAME = 'updatepackprofiles.cfg';
  C_LAUTRayINI = 'lauimport.ini';

resourcestring
  rsDefaultProfileName = 'Default';
  rsConfigApplication = 'Application';
  rsConfigVersion = 'Version';
  rsProfileSaved = 'Profile ''%s'' saved successfully';
  rsCloseWithoutSaving = 'Save current profile to disk?';
  rsCannotDeleteDefault = 'Sorry, you cannot delete this profile';
  rsNewProfileCreated = 'New profile ''%s'' created successfully';
  rsUseAppnameVersionsQuestion = 'Update VersionsINI to %s.ini?';
  rsUseAppnameZipfileQuestion = 'Update Zipfile to %s.zip?';
  rsTheSourceFil = 'The source files are located in %s';
  rsCouldNotLoca = 'Could not locate %s on the server';
  rsYourCopyOfLa = 'Your copy of LazAutoUpdate v%s is the latest version.';
  rsDownloadIsIn = 'Download is in progress.  Please wait';

{TODO: Incorporate more text into here}

{ Tmainform }

procedure Tmainform.mnu_fileCloseClick(Sender: TObject);
begin
  // Deflower
  AppConfig.WriteBool('Current', 'IsVirgin', bIsVirgin);
  Close;
end;

procedure Tmainform.mnu_fileSaveProfileClick(Sender: TObject);
begin
  cmd_SaveProfile.Click;
end;

procedure Tmainform.mnu_helpAboutClick(Sender: TObject);
var
  sz: string;
begin
  sz := Application.Title + LineEnding + LineEnding;
  sz += Format('Version %s', [LazAutoUpdate1.AppVersion]) + LineEnding;
  sz += Format('LazAutoUpdate v%s', [LazAutoUpdate1.AutoUpdateVersion]) + LineEnding;
  sz += Format('ThreadedDownload v%s',
    [LazAutoUpdate1.ThreadDownload.ThreadDownloadVersion]) + LineEnding + LineEnding;
  sz += LazAutoUpdate1.LCLVersion + ' + ' + LazAutoUpdate1.FPCVersion + LineEnding;
  sz += Format('Last Compiled: %s', [LazAutoUpdate1.LastCompiled]) + LineEnding;
  sz += Format('for %s', [LazAutoUpdate1.WidgetSet]);
  //sz+=Format('',[]);
  MessageDlg(Format('About %s', [Application.Title]),
    sz, mtInformation, [mbOK], 0);

end;

procedure Tmainform.mnu_helpCheckForUpdatesClick(Sender: TObject);
begin
  with LazAutoUpdate1 do
  begin
    DebugMode := True; // Fire OnDebugEvent and log the results
    CopyTree := True;
    UpdatesFolder := 'updates';
    ShowUpdateInCaption := True;
    SFProjectName := 'lazautoupdate';
       {$IFDEF Win32}
    VersionsININame := 'updatepackwin32.ini';
    ZipfileName := 'updatepackwin32.zip';
       {$ENDIF}
       {$IFDEF Win64}
    VersionsININame := 'updatepackwin64.ini';
    ZipfileName := 'updatepackwin64.zip';
       {$ENDIF}
       {$IFDEF Linux64}
    VersionsININame := 'updatepacklunux64.ini';
    ZipfileName := 'updatepackl64.zip';
       {$ENDIF}
       {$IFDEF Linux32}
    VersionsININame := 'updatepacklinux32.ini';
    ZipfileName := 'updatepackl32.zip';
       {$ENDIF}
    AutoUpdate; // How simple is that?
    DebugMode := False;
  end;
end;

procedure Tmainform.spd_WhatsNewCreateClick(Sender: TObject);
begin
  memoform.MemoAction := maShowWhatsNew;
  memoform.szWhatsNewPath := ProfileRec.WhatsNewPath;
  memoform.ShowModal;
  ProfileRec.WhatsNewPath := memoform.szWhatsNewPath;
  WriteToGUI(szCurrentProfileName);
  bCurrentProfileSaved := False;
end;

procedure Tmainform.spn1Change(Sender: TObject);
begin
  ProfileRec.NewVersion.V1 := spn1.Value;
  FillVersionString(ProfileRec.NewVersion);
  bCurrentProfileSaved := False;
end;

procedure Tmainform.spn2Change(Sender: TObject);
begin
  ProfileRec.NewVersion.V2 := spn2.Value;
  FillVersionString(ProfileRec.NewVersion);
  bCurrentProfileSaved := False;
end;

procedure Tmainform.spn3Change(Sender: TObject);
begin
  ProfileRec.NewVersion.V3 := spn3.Value;
  FillVersionString(ProfileRec.NewVersion);
  bCurrentProfileSaved := False;
end;

procedure Tmainform.spn4Change(Sender: TObject);
begin
  ProfileRec.NewVersion.V4 := spn4.Value;
  FillVersionString(ProfileRec.NewVersion);
  bCurrentProfileSaved := False;
end;

procedure Tmainform.tab_configureContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: boolean);
begin

end;

procedure Tmainform.FormCreate(Sender: TObject);
begin
  if LowerCase(ParamStr(1)) = 'debug' then
    DebugMode := True
  else
    DebugMode := False;
  Caption := Application.Title;
  Icon := Application.Icon;
  AppConfig := TINIFile.Create(GetAppConfigDir(False) + C_APPCONFIGNAME);
  ProfileConfig := TINIFile.Create(GetAppConfigDir(False) + C_PROFILECONFIGNAME);
  ProfilenameList := TStringList.Create;
  ProfileRec.DragFileStringList := TStringList.Create;

  if FileExistsUTF8('readme.txt') then
  begin
    memo_intro.Lines.LoadFromFile('readme.txt');
    memo_intro.Lines[0] := AnsiReplaceText(memo_intro.Lines[0],
      '0.0.0.0', LazAutoUpdate1.AppVersion);
  end;

  // Fetch the last saved profile name
  with AppConfig do
  begin
    WriteString('ProgramInfo', rsConfigApplication, Application.Title);
    WriteString('ProgramInfo', rsConfigVersion, LazAutoUpdate1.AppVersion);
    szCurrentProfileName := ReadString('Current', 'Profilename', rsDefaultProfileName);
    bComponentDownloaded := ReadBool('Current', 'LazAutoUpdateDownloaded', False);
    bIsVirgin := ReadBool('Current', 'IsVirgin', False);
    bShowCodeWindow := ReadBool('Current', 'ShowCodeWindow', True);
    bShowFileManager := ReadBool('Current', 'ShowFileManager', False);
    bShowOnlineWebsite := ReadBool('Current', 'ShowSFWebsite', False);
  end;
  mnuoptionsShowCodeInfo.Checked := bShowCodeWindow;
  mnuoptionsDisplayFileManger.Checked := bShowFileManager;
  mnuoptionsShowOnlineWebsite.Checked := bShowOnlineWebsite;

  // LazAutoupdate - download or update?
  if bComponentDownloaded then
    cmd_DownloadLazAutoUpdate.Caption := 'Update Component'
  else
    cmd_DownloadLazAutoUpdate.Caption := 'Download Component';

  // Read it into ProfileRec
  ReadProfileFromINI(szCurrentProfileName);
  // Write back to disk (in case this is a virgin run)
  WriteProfileToINI(szCurrentProfileName);
  bCurrentProfileSaved := True;

  // Fill the GUI controls with contents of ProfileRec
  WriteToGUI(szCurrentProfileName);

  // Fill the profiles combo box
  ProfileConfig.ReadSections(ProfileNameList);
  cmb_profile.Clear;
  cmb_profile.Items := ProfileNameList;
  cmb_profile.ItemIndex := cmb_profile.Items.IndexOf(szCurrentProfileName);
  {
  if LazAutoUpdate1.CreateLocalLauImportFile then
    LazAutoUpdate1.RelocateLauImportFile;
  }
  if bIsVirgin then
    PageControl1.ActivePage := tab_intro
  else
    PageControl1.ActivePage := tab_configure;

  if DebugMode then
  BEGIN
    EventLog1.FileName := ChangeFileExt(ParamStr(0), '.log');
    if FileExistsUTF8(EventLog1.FileName) then
      SysUtils.DeleteFile(EventLog1.FileName);
    EventLog1.AppendContent := True;
    EventLog1.Active := True;
    LazAutoUpdate1.DebugMode := DebugMode;
  end;
end;

procedure Tmainform.FormDestroy(Sender: TObject);
begin
  // No memory leaks!
  If Assigned(EventLog1) then FreeAndNil(EventLog1);
  FreeAndNil(ProfileConfig);
  FreeAndNil(AppConfig);
  FreeAndNil(ProfilenameList);
  FreeAndNil(ProfileRec.DragFileStringList);
end;

procedure Tmainform.FormDropFiles(Sender: TObject; const FileNames: array of string);
// Not allowed:
// 1. The AppPath (already in the zipfile)
// 2. The WhatsNewPath (already in the zipfile)
// 3. Directories (use CopyTree instead)
// 4. Symlinks (both Windows and Linux)
// 5. The updating app - updatehm(.exe)
// ProfileRec.DragFileStringList is the master record
// lst_dragfiles is just for display
var
  i: integer;
begin
  for i := Low(FileNames) to High(FileNames) do
    if ((ProfileRec.DragFileStringList.IndexOf(FileNames[i]) = -1) and
      (FileNames[i] <> ProfileRec.AppPath) and
      (FileNames[i] <> ProfileRec.WhatsNewPath) and
      (FileNames[i] <> LazAutoUpdate1.UpdateExe) and
      (FileNames[i] <> LazAutoUpdate1.UpdateExeSilent)) then
      if (DirPathExists(FileNames[i]) = True) then
      begin // Not allowed to drag a directory into the list
        ShowMessage('You dragged a directory. For whole directories, use the CopyTree facility');
        continue;
      end
      else
      if (FileIsSymlink(FileNames[i]) = False) then // Symlinks not allowed
        ProfileRec.DragFileStringList.Add(FileNames[i]);

  lst_dragfiles.Items := ProfileRec.DragFileStringList;
end;

procedure Tmainform.FormShow(Sender: TObject);
begin
  Try
  LazAutoUpdate1.ShowWhatsNewIfAvailable;
  Except
    raise Exception.Create('Problem in FormShow');
  end;
  bCurrentProfileSaved := True;
end;

procedure Tmainform.grp_dragfilesDblClick(Sender: TObject);
begin
  ProfileRec.DragFileStringList.Clear;
  lst_dragfiles.Items := ProfileRec.DragFileStringList;
end;

procedure Tmainform.LazAutoUpdate1DebugEvent(Sender: TObject; WhereAt, Message: string);
begin
  if DebugMode and (EventLog1.Active = True) then
    EventLog1.Log(Format('LazAutoUpdate: Source=%s, Message=%s', [WhereAt, Message]));
end;

procedure Tmainform.LazAutoUpdate1Downloaded(Sender: TObject; ResultCode,
  BytesDownloaded: integer);
begin
   if DebugMode and (EventLog1.Active = True) then
    EventLog1.Log(Format('LazAutoUpdate: OnDownloaded ResultCode=%d BytesDownloaded=%d',
    [ResultCode, BytesDownloaded]));
end;

procedure Tmainform.LazAutoUpdate1NewVersionAvailable(Sender: TObject;
  Newer: boolean; OnlineVersion: string);
begin
 if DebugMode and (EventLog1.Active = True) then
    EventLog1.Log(Format('LazAutoUpdate: NewVersionAvailable OnlineVersion=%s',
    [OnlineVersion]));
end;

procedure Tmainform.LazAutoUpdate1Updated(Sender: TObject; NewVersion,
  LauMessage: String);
begin
 if DebugMode and (EventLog1.Active = True) then
    EventLog1.Log(Format('LazAutoUpdate: New Version=%s, Message=%s', [NewVersion, LauMessage]));
end;

procedure Tmainform.lst_dragfilesDblClick(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to ProfileRec.DragFileStringList.Count - 1 do
  begin
    if lst_dragfiles.Selected[i] then
      ProfileRec.DragFileStringList.Delete(i);
  end;
  lst_dragfiles.Items := ProfileRec.DragFileStringList;
end;

procedure Tmainform.mnuoptionsDisplayFileMangerClick(Sender: TObject);
begin
  bShowFileManager := mnuoptionsDisplayFileManger.Checked;
  AppConfig.WriteBool('Current', 'ShowFileManager', bShowFileManager);
end;

procedure Tmainform.mnuoptionsShowCodeInfoClick(Sender: TObject);
begin
  bShowCodeWindow := mnuoptionsShowCodeInfo.Checked;
  AppConfig.WriteBool('Current', 'ShowCodeWindow', bShowCodeWindow);
end;

procedure Tmainform.mnuoptionsShowOnlineWebsiteClick(Sender: TObject);
begin
  bShowOnlineWebsite := mnuoptionsShowOnlineWebsite.Checked;
  AppConfig.WriteBool('Current', 'ShowSFWebsite', bShowOnlineWebsite);
end;

procedure Tmainform.cmd_SaveProfileClick(Sender: TObject);
begin
  AppConfig.WriteString('Current', 'Profilename', szCurrentProfileName);
  ReadFromGUI(szCurrentProfileName);
  WriteProfileToINI(szCurrentProfileName);
  bCurrentProfileSaved := True;
  MessageDlg(Application.Title,
    Format(rsProfileSaved, [szCurrentProfileName]),
    mtInformation, [mbOK], 0);
end;

procedure Tmainform.cmd_UseAppnameZipfileNameClick(Sender: TObject);
var
  sz: string;
begin
  sz := ExtractFileNameOnly(ProfileRec.AppPath);
  if (MessageDlg(Application.Title, Format(rsUseAppnameZipfileQuestion, [sz]),
    mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
  begin
    ProfileRec.ZipFileName := ProfileRec.OutDir + ChangeFileExt(sz, '.zip');
    WriteToGUI(szCurrentProfileName);
    bCurrentProfileSaved := False;
  end;
end;

procedure Tmainform.edt_AppFileNameChange(Sender: TObject);
begin
  edt_AppFileName.InitialDir := ExtractFileDir(ProfileRec.AppPath);
  ProfileRec.AppPath := edt_AppFileName.FileName;
  WriteToGUI(szCurrentProfileName);
  bCurrentProfileSaved := False;
end;

procedure Tmainform.edt_AppFileNameKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  Key := 0;
end;

procedure Tmainform.edt_CopyTreeChange(Sender: TObject);
begin
  ProfileRec.CopyTreeDirectory := edt_CopyTree.Text;
  bCurrentProfileSaved := False;
end;

procedure Tmainform.edt_CopyTreeEditingDone(Sender: TObject);
begin
  ProfileRec.CopyTreeDirectory := edt_CopyTree.Text;
  bCurrentProfileSaved := False;
end;

procedure Tmainform.edt_UpdatesChange(Sender: TObject);
begin
  ProfileRec.SFUpdatesDirectory := edt_Updates.Text;
  bCurrentProfileSaved := False;

end;

procedure Tmainform.edt_ZipFileNameChange(Sender: TObject);
begin
  edt_ZipFileName.InitialDir := ExtractFileDir(ProfileRec.ZipFileName);
  ProfileRec.ZipFileName := ProfileRec.OutDir +
    ExtractFileName(edt_ZipFileName.FileName);
  WriteToGUI(szCurrentProfileName);
  bCurrentProfileSaved := False;
end;

procedure Tmainform.edt_ZipFileNameEditingDone(Sender: TObject);
begin
  if CompareFileExt(edt_ZipFileName.Text, '.zip', False) <> 0 then
    edt_ZipFileName.Text := ChangeFileExt(edt_ZipFileName.Text, '.zip');
  with ProfileRec do
  begin
    ZipFileName := edt_ZipFileName.Text;
    ZipFileName := OutDir + ZipFileName;
  end;
  bCurrentProfileSaved := False;

end;

procedure Tmainform.edt_VersionsINIFileChange(Sender: TObject);
begin
  edt_VersionsINIFile.InitialDir := ExtractFileDir(ProfileRec.VersionsINI);
  ProfileRec.VersionsINI := ProfileRec.OutDir +
    ExtractFileName(edt_VersionsINIFile.FileName);
  WriteToGUI(szCurrentProfileName);
  bCurrentProfileSaved := False;
end;

procedure Tmainform.edt_OutputDirectoryChange(Sender: TObject);
begin
  ProfileRec.OutDir := AppendPathDelim(edt_OutputDirectory.Directory);
  ForceDirectoriesUTF8(ProfileRec.OutDir);
  bCurrentProfileSaved := False;
end;

procedure Tmainform.edt_OutputDirectoryKeyDown(Sender: TObject;
  var Key: word; Shift: TShiftState);
begin
  Key := 0;
end;

procedure Tmainform.edt_SFProjectNameEditingDone(Sender: TObject);
begin
  ProfileRec.SFProjectName := LowerCase(edt_SFProjectName.Text);
  edt_SFProjectName.Text := ProfileRec.SFProjectName;
  bCurrentProfileSaved := False;
end;

procedure Tmainform.edt_WhatsNewTextFileChange(Sender: TObject);
begin
  edt_WhatsNewTextFile.InitialDir := ExtractFileDir(ProfileRec.WhatsNewPath);
  // ForceDirectoriesUTF8(ProfileRec.WhatsNewPath);
  ProfileRec.WhatsNewPath := edt_WhatsNewTextFile.Filename;
  bCurrentProfileSaved := False;
end;

procedure Tmainform.edt_WhatsNewTextFileKeyDown(Sender: TObject;
  var Key: word; Shift: TShiftState);
begin
  Key := 0;
end;

procedure Tmainform.edt_ZipFileNameoldEditingDone(Sender: TObject);
begin
  if CompareFileExt(edt_ZipFileName.Text, '.zip', False) <> 0 then
    edt_ZipFileName.Text := ChangeFileExt(edt_ZipFileName.Text, '.zip');
  with ProfileRec do
  begin
    ZipFileName := ExtractFileName(edt_ZipFileName.Text);
    ZipFileName := OutDir + ZipFileName;
  end;
  bCurrentProfileSaved := False;
end;

procedure Tmainform.edt_VersionsINIFileEditingDone(Sender: TObject);
begin
  if CompareFileExt(edt_VersionsINIFile.Text, '.ini', False) <> 0 then
    edt_VersionsINIFile.Text := ChangeFileExt(edt_VersionsINIFile.Text, '.ini');
  with ProfileRec do
  begin
    VersionsINI := ExtractFileName(edt_VersionsINIFile.Text);
    VersionsINI := OutDir + VersionsINI;
    bCurrentProfileSaved := False;
  end;
end;

procedure Tmainform.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if LazAutoUpdate1.DownloadInprogress then
  begin
    CanClose := False;
    ShowMessage(rsDownloadIsIn);
    Exit;
  end;
  if not bCurrentProfileSaved then
    if (MessageDlg(Application.Title, rsCloseWithoutSaving, mtWarning,
      [mbYes, mbNo], 0) = mrNo) then
      CanClose := True
    else
    begin
      cmd_SaveProfile.Click;
      CanClose := True;
    end;
  if CanClose then
    if DebugMode then
      EventLog1.Active := False;
end;

procedure Tmainform.cmd_UseAppnameVersionsINIClick(Sender: TObject);
var
  sz: string;
begin
  sz := ExtractFileNameOnly(ProfileRec.AppPath);
  if (MessageDlg(Application.Title, Format(rsUseAppnameVersionsQuestion, [sz]),
    mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
  begin
    ProfileRec.VersionsINI := ProfileRec.OutDir + ChangeFileExt(sz, '.ini');
    WriteToGUI(szCurrentProfileName);
    bCurrentProfileSaved := False;
  end;
end;

procedure Tmainform.cmd_DownloadLazAutoUpdateClick(Sender: TObject);
// TODO: Resourcestrings
var
  VersionINI: TINIFile;
begin
  with LazAutoUpdate1 do
  begin
    // Set component to download source package
    // Enable to debug LazAutoUpdate
    DebugMode := True; // Fire OnDebugEvent and log the results
    SFProjectName := 'lazautoupdate';
    CopyTree := True;
    VersionsININame := 'lazautoupdatesource.ini';
    UpdatesFolder := 'lazautoupdatesource';
    ZipFileName := 'packagesource.zip';
    ShowUpdateInCaption := True;
    VersionINI := TIniFile.Create('new' + VersionsININame);
    //    VersionINI := TIniFile.Create(VersionsININame);
    AppVersion := VersionINI.ReadString('versions', 'GUI', '0.0.0.0');
    VersionINI.Free;
    if NewVersionAvailable then
    begin
      if DownloadNewVersion then
      begin
        ShowMessageFmt(rsTheSourceFil,
          [ProgramDirectory + UpdatesFolder]);
        bComponentDownloaded := True;
        AppConfig.WriteBool('Current', 'LazAutoUpdateDownloaded', True);
        cmd_DownloadLazAutoUpdate.Caption := 'Update Component';
      end
      else
      begin
        ShowMessageFmt(rsCouldNotLoca, [ZipFileName]);
        cmd_DownloadLazAutoUpdate.Caption := 'Download Component';
      end;
    end
    else
      ShowMessageFmt(rsYourCopyOfLa, [GUIOnlineVersion]);
  end;

  LazAutoUpdate1.DebugMode := False;
  // Reset to this application
  LazAutoUpdate1.ResetAppVersion;
end;

procedure Tmainform.cmb_IntervalTypeChange(Sender: TObject);
begin
  DoEnableDisableIntervalCombos;
end;

procedure Tmainform.cmb_profileClick(Sender: TObject);
begin
  szCurrentProfileName := cmb_profile.Items[cmb_profile.ItemIndex];
  // Auto-save current profile
  AppConfig.WriteString('Current', 'Profilename', szCurrentProfileName);
  ReadFromGUI(szCurrentProfileName); // Read from GUI
  WriteProfileToINI(szCurrentProfileName); // Write to disk
  bCurrentProfileSaved := True;
end;

procedure Tmainform.cmb_profileCloseUp(Sender: TObject);
begin
  // Read selected profile into ProfileRec and GUI
  szCurrentProfileName := cmb_profile.Items[cmb_profile.ItemIndex];
  ReadProfileFromINI(szCurrentProfileName); // Read from disk
  WriteToGUI(szCurrentProfileName);  // Display in GUI
  AppConfig.WriteString('Current', 'Profilename', szCurrentProfileName);
  bCurrentProfileSaved := False;

end;

procedure Tmainform.DoEnableDisableIntervalCombos;
begin
  case cmb_IntervalType.ItemIndex of
    0:
    begin
      cmb_IntervalDay.Enabled := False;
      cmb_IntervalDate.Enabled := False;
    end;
    1:
    begin
      cmb_IntervalDay.Enabled := True;
      cmb_IntervalDate.Enabled := False;
    end;
    2:
    begin
      cmb_IntervalDay.Enabled := False;
      cmb_IntervalDate.Enabled := True;
    end;
  end;
end;

procedure Tmainform.chk_UseCopyTreeChange(Sender: TObject);
begin
  ProfileRec.CopyTree := chk_UseCopyTree.Checked;
  bCurrentProfileSaved := False;
end;

procedure Tmainform.chk_ShowUpdateInCaptionChange(Sender: TObject);
begin
  ProfileRec.ShowUpdatesInCaption := chk_ShowUpdateInCaption.Checked;
  bCurrentProfileSaved := False;

end;

procedure Tmainform.chk_ShowDialogsChange(Sender: TObject);
begin
  ProfileRec.ShowDialogs := chk_ShowDialogs.Checked;
  bCurrentProfileSaved := False;

end;

procedure Tmainform.chk_DebugModeChange(Sender: TObject);
begin
  ProfileRec.DebugMode := chk_DebugMode.Checked;
  bCurrentProfileSaved := False;

end;

procedure Tmainform.chk_overrideUserPrefsClick(Sender: TObject);
begin
  bOverRideUserPrefs := chk_overrideUserPrefs.Checked;
end;

procedure Tmainform.cmd_MakeUpdatePackClick(Sender: TObject);
var
  sz, szPathentry: string;
  i: integer;
  fFileManagerProcess: TAsyncProcess;
begin
  // Make the versionsini
  if ((MakeINIFile = True) and (FileExistsUTF8(ProfileRec.VersionsINI))) then
    ShowMessageFmt('VersionsINI ''%s'' is OK', [ProfileRec.VersionsINI]);
  // Make the lauimport file
  if ((MakeLAUTrayINIFile = True) and
    (FileExistsUTF8(ProfileRec.OutDir + C_LAUTRayINI))) then
    ShowMessageFmt('TrayNotify INI ''%s'' is OK', [ProfileRec.OutDir + C_LAUTRayINI]);
  // Make the zipfile
  if ((MakeZipfile = True) and (FileExistsUTF8(ProfileRec.ZipFileName))) then
    ShowMessageFmt('Zipfile ''%s'' is OK', [ProfileRec.ZipFileName])
  else
    ShowMessage('Unable to create Zipfile. Is the App path and/or WhatsNew path valid?');
  // Save the current Profile
  AppConfig.WriteString('Current', 'Profilename', szCurrentProfileName);
  ReadFromGUI(szCurrentProfileName);
  WriteProfileToINI(szCurrentProfileName);
  bCurrentProfileSaved := True;
  // Make up the text for the memo
  sz := 'Zipfile and INI file are located at:' + LineEnding;
  sz += ProfileRec.Outdir + LineEnding + LineEnding;

  sz += '-- Copy and Paste Pascal code --' + LineEnding + LineEnding;
  sz += 'Form1.FormCreate(Sender: TObject);' + LineEnding;
  sz += 'begin' + LineEnding;
  sz += '  LazAutoUpdate1.ProjectType:=auSourceForge;' + LineEnding;
  sz += Format('  LazAutoUpdate1.SFProjectName:=''%s'';%s',
    [ProfileRec.SFProjectName, LineEnding]);
  sz += Format('  LazAutoUpdate1.VersionsININame:=''%s'';%s',
    [ExtractFilename(ProfileRec.VersionsINI), LineEnding]);
  sz += Format('  LazAutoUpdate1.ZipfileName:=''%s'';%s',
    [ExtractFilename(ProfileRec.ZipfileName), LineEnding]);
  if ProfileRec.CopyTree = True then
    sz += '  LazAutoUpdate1.CopyTree:=True;' + LineEnding
  else
  if (ProfileRec.DragFileStringList.Count > 0) then
    sz += '  LazAutoUpdate1.CopyTree:=True;' + LineEnding
  else
    sz += '  LazAutoUpdate1.CopyTree:=False;' + LineEnding;
  if ProfileRec.ShowUpdatesInCaption = True then
    sz += '  LazAutoUpdate1.ShowUpdatesInCaption:=True;' + LineEnding
  else
    sz += '  LazAutoUpdate1.ShowUpdatesInCaption:=False;' + LineEnding;
  if ProfileRec.ShowDialogs = True then
    sz += '  LazAutoUpdate1.ShowDialogs:=True;' + LineEnding
  else
    sz += '  LazAutoUpdate1.ShowDialogs:=False;' + LineEnding;
  if ProfileRec.DebugMode = True then
    sz += '  LazAutoUpdate1.DebugMode:=True;' + LineEnding
  else
    sz += '  LazAutoUpdate1.DebugMode:=False;' + LineEnding;
  sz += Format('  LazAutoUpdate1.UpdatesFolder:=''%s'';',
    [ProfileRec.SFUpdatesDirectory]) + LineEnding;
  sz += 'end;' + LineEnding + LineEnding;
  sz += Format('The zipfile %s has the structure:',
    [ExtractFileName(profilerec.ZipfileName)]) + LineEnding;
  sz += '-' + ExtractFilename(ProfileRec.AppPath) + LineEnding;
  sz += '-' + ExtractFilename(ProfileRec.WhatsNewPath) + LineEnding;
  sz += '-' + C_LAUTRayINI + LineEnding;
  if (ProfileRec.DragFileStringList.Count > 0) then
    for i := 0 to ProfileRec.DragFileStringList.Count - 1 do
      sz += '-' + ExtractFilename(ProfileRec.DragFileStringList[i]) + LineEnding;
  // Deal with the copytree folder stuff
  if ProfileRec.CopyTree then
  begin
    i := RPos(PathDelim, ChompPathDelim(ProfileRec.CopyTreeDirectory));
    szPathEntry := MidStr(ProfileRec.CopyTreeDirectory, i + 1, Length(
      ProfileRec.CopyTreeDirectory));
    sz += '--' + PathDelim + szPathEntry + LineEnding + LineEnding;
  end;
  sz += LineEnding + 'This structure will be unzipped into the application directory, and'
    + LineEnding;
  sz += 'will overwtrite existing files and/or folders of the same name.' + LineEnding;
  sz += 'If you want other files or folders to be deployed, then edit the zipfile' +
    LineEnding;
  sz += '(or drag files into the [Advanced] tab list) before uploading it' + LineEnding;
  sz += 'to your SourceForge ''' + profilerec.SFUpdatesDirectory +
    ''' folder.' + LineEnding + LineEnding;
  sz += '-------------------------' + LineEnding;
  sz += 'Type your own notes below' + LineEnding;
  memoform.MemoAction := maShowCode;
  memoform.szCodeText := sz;
  if bShowCodeWindow then
    memoform.ShowModal;
  fFileManagerProcess := TAsyncProcess.Create(nil);
  try
  {$IFDEF WINDOWS}
    with fFileManagerProcess do
    begin
      Executable := 'explorer.exe';
      Parameters.Clear;
      Parameters.Add('"' + ProfileRec.Outdir + '"');
      if bShowFileManager then
        Execute;
    end;
    if bShowOnlineWebsite then
      if LazAutoUpdate1.ProjectType = auSourceForge then
        OpenURL('https://sourceforge.net/projects/' + ProfileRec.SFProjectName + '/');
    if LazAutoUpdate1.ProjectType = auGitHubReleaseZip then
      OpenURL('https://github.com/' + LazAutoUpdate1.GitHubProjectName +
        '/' + LazAutoUpdate1.GitHubBranchOrTag + '/');
    if LazAutoUpdate1.ProjectType = auOther then
      OpenURL(LazAutoUpdate1.auOtherSourceURL + '/');

  {$ELSE}
    // Use generic linux command
    if (FindDefaultExecutablePath('xdg-open') <> '') then
      with fFileManagerProcess do
      begin
        Executable := 'xdg-open';
        Parameters.Clear;
        Parameters.Add(ProfileRec.Outdir);
        if bShowFileManager then
          Execute;
        Parameters.Clear;
        if LazAutoUpdate1.ProjectType = auSourceForge then
          Parameters.Add('https://sourceforge.net/projects/' +
            ProfileRec.SFProjectName + '/');
        if LazAutoUpdate1.ProjectType = auGitHubReleaseZip then
          Parameters.Add('https://github.com/' + LazAutoUpdate1.GitHubProjectName +
            '/' + LazAutoUpdate1.GitHubBranchOrTag + '/');
        if LazAutoUpdate1.ProjectType = auOther then
          Parameters.Add(LazAutoUpdate1.auOtherSourceURL + '/');
        if bShowOnlineWebsite then
          Execute;
      end;
  {$ENDIF}
  finally
    fFileManagerProcess.Free;
  end;
end;

procedure Tmainform.cmd_DeleteProfileClick(Sender: TObject);
begin
  DeleteCurrentProfile;
end;

procedure Tmainform.cmd_NewProfileClick(Sender: TObject);
var
  szProfileName: string;
begin
  szProfileName := '';
  if not InputQuery('New Profile Name', 'Type new profile name here',
    False, szProfilename) then
    exit;
  if szProfileName = '' then
    exit;

  // We have a valid profile name...
  // Does it already exist?
  if (ProfilenameList.IndexOf(szProfilename) = -1) then
    // Unique new profile name entered by user
  begin
    ProfilenameList.Add(szProfilename);
    szCurrentProfileName := szProfilename;
    ReadFromGUI(szCurrentProfileName);  // Read current values into ProfileRec

    AppConfig.WriteString('Current', 'Profilename', szCurrentProfileName);
    cmb_profile.Clear;
    cmb_profile.Items := ProfileNameList;
    // Set last item in list as current
    // cmb_profile.ItemIndex := cmb_profile.Items.Count - 1;
    bCurrentProfileSaved := False;
  end
  else
  begin
    ShowMessage('This profile name already exists. Please choose a new name');
    Exit;
  end;
  if (ProfilenameList.IndexOf(cmb_profile.Text) >= 0) then
    MessageDlg(Application.Title,
      Format(rsNewProfileCreated, [szCurrentProfileName]),
      mtInformation, [mbOK], 0);
end;

procedure Tmainform.DeleteCurrentProfile;
begin
  // Cannot delete Default profile
  if (szCurrentProfileName = rsDefaultProfileName) then
  begin
    MessageDlg(Application.Title,
      rsCannotDeleteDefault, mtError, [mbOK], 0);
    Exit;
  end;
  // Delete the current profile and load the last one in the list
  ProfilenameList.Delete(cmb_profile.ItemIndex);
  cmb_profile.Clear;
  cmb_profile.Items := ProfileNameList;
  cmb_profile.ItemIndex := cmb_profile.Items.Count - 1;
  ProfileConfig.EraseSection(szCurrentProfileName);
  szCurrentProfileName := cmb_profile.Items[cmb_profile.ItemIndex];
  ReadProfileFromINI(szCurrentProfileName);
  WriteToGUI(szCurrentProfileName); // Reads profile from disk first
  AppConfig.WriteString('Current', 'Profilename', szCurrentProfileName);
  bCurrentProfileSaved := False;
end;

procedure Tmainform.ReadProfileFromINI(AProfileName: string);
var
  i, iNumEntries: integer;
begin
  with ProfileRec do
    with ProfileConfig do
    begin
      ProfileName := AProfileName;
      AppPath := ReadString(AProfileName, 'AppPath', ProgramDirectory + 'myapp.exe');
      OutDir := CleanAndExpandDirectory(ReadString(AProfileName,
        'OutDir', ProgramDirectory));
      NewVersion.VString := ReadString(AProfileName, 'NewVersion', '0.0.0.0');
      FillVersionInteger(NewVersion);
      VersionsINI := ReadString(AProfileName, 'VersionsINI', 'versions.ini');
      WhatsNewPath := ReadString(AProfileName, 'WhatsNewPath',
        ProgramDirectory + 'whatsnew.txt');
      SFProjectName := ReadString(AProfileName, 'SFProjectName', 'mysfproject');
      ZipFileName := ReadString(AProfileName, 'ZipFileName',
        ProgramDirectory + 'myappzipfile.zip');
      CopyTree := ReadBool(AProfileName, 'CopyTree', False);
      CopyTreeDirectory := ReadString(AProfileName, 'CopyTreeDirectory',
        ProgramDirectory);
      SFUpdatesDirectory := ReadString(AProfileName, 'SFUpdatesDirectory', 'updates');
      ShowUpdatesInCaption := ReadBool(AProfileName, 'ShowUpdatesInCaption', False);
      ShowDialogs := ReadBool(AProfileName, 'ShowDialogs', False);
      DebugMode := ReadBool(AProfileName, 'DebugMode', False);
      OverRideUserPrefs := ReadBool(AProfileName, 'OverRideUserPrefs', False);
      IntervalType := ReadInteger(AProfileName, 'IntervalType', 0);
      IntervalDate := ReadInteger(AProfileName, 'IntervalDate', 0);
      IntervalDay := ReadInteger(AProfileName, 'IntervalDay', 0);
      IntervalHour := ReadInteger(AProfileName, 'IntervalHour', 9);
      DragFileStringList.Clear;
      iNumEntries := ReadInteger(AProfileName, 'ExtraFiles', 0);
      if (iNumEntries > 0) then
        for i := 0 to iNumEntries - 1 do
          DragFileStringList.Add(
            ReadString(AProfileName, Format('ExtraFile%.3d', [i]), 'invalid file'));
    end;
end;

procedure Tmainform.WriteProfileToINI(AProfileName: string);
var
  i, iNumEntries: integer;
begin
  with ProfileRec do
    with ProfileConfig do
    begin
      WriteString(AProfileName, 'AppPath', AppPath);
      WriteString(AProfileName, 'OutDir', CleanAndExpandDirectory(OutDir));
      WriteString(AProfileName, 'NewVersion', NewVersion.VString);
      WriteString(AProfileName, 'VersionsINI', VersionsINI);
      WriteString(AProfileName, 'WhatsNewPath', WhatsNewPath);
      WriteString(AProfileName, 'SFProjectName', SFProjectName);
      WriteString(AProfileName, 'ZipFileName', ZipFileName);
      WriteBool(AProfileName, 'CopyTree', CopyTree);
      WriteString(AProfileName, 'CopyTreeDirectory', CopyTreeDirectory);
      WriteString(AProfileName, 'SFUpdatesDirectory', SFUpdatesDirectory);
      WriteBool(AProfileName, 'ShowUpdatesInCaption', ShowUpdatesInCaption);
      WriteBool(AProfileName, 'ShowDialogs', ShowDialogs);
      WriteBool(AProfileName, 'DebugMode', DebugMode);
      WriteBool(AProfileName, 'OverRideUserPrefs', OverRideUserPrefs);
      WriteInteger(AProfileName, 'IntervalType', IntervalType);
      WriteInteger(AProfileName, 'IntervalDate', IntervalDate);
      WriteInteger(AProfileName, 'IntervalDay', IntervalDay);
      WriteInteger(AProfileName, 'IntervalHour', IntervalHour);

      // Begin by deleting any existing ExtraFile Entries
      iNumEntries := ReadInteger(AProfileName, 'ExtraFiles', 0);
      if (iNumEntries > 0) then
        for i := 0 to iNumEntries - 1 do
          DeleteKey(AProfileName, Format('ExtraFile%.3d', [i]));

      // Now add any current ExtraFile entries (or zero)
      if DragFileStringList.Count > 0 then
      begin
        // Record the number of entries in the list
        WriteInteger(AProfileName, 'ExtraFiles', DragFileStringList.Count);
        for i := 0 to DragFileStringList.Count - 1 do
        begin
          WriteString(AProfileName, Format('ExtraFile%.3d', [i]), DragFileStringList[i]);
        end;
      end
      else
        WriteInteger(AProfileName, 'ExtraFiles', 0);
    end;
end;

procedure Tmainform.WriteToGUI(AProfileName: string);
// Only the filenames of Versionini and ZipFilename are displayed
// Full paths (with OutputDir) are stored in ProfileRec and Config

// Full path of WhatsNew.txt is displayed
// as it may not be stored in the OutputDir folder
begin
  with ProfileRec do
  begin
    if AppPath = '' then
      ReadProfileFromINI(AProfileName);
    edt_AppFileName.Text := AppPath;
    spn1.Value := NewVersion.V1;
    spn2.Value := NewVersion.V2;
    spn3.Value := NewVersion.V3;
    spn4.Value := NewVersion.V4;
    edt_OutputDirectory.Text := CleanAndExpandDirectory(TrimFilename(OutDir));
    // TODO Linux/Windows
    edt_VersionsINIFile.Text := ExtractFileName(VersionsINI); // Strip path info
    edt_WhatsNewTextFile.Text := WhatsNewPath;
    edt_SFProjectName.Text := SFProjectName;
    edt_ZipFileName.Text := ExtractFileName(ZipFileName); // Strip path info
    chk_UseCopyTree.Checked := CopyTree;
    edt_CopyTree.Text := CopyTreeDirectory;
    edt_Updates.Text := SFUpdatesDirectory;
    chk_ShowUpdateInCaption.Checked := ShowUpdatesInCaption;
    chk_ShowDialogs.Checked := ShowDialogs;
    chk_DebugMode.Checked := DebugMode;
    chk_overrideUserPrefs.Checked := OverrideUserPrefs;
    cmb_IntervalType.ItemIndex := IntervalType;
    cmb_IntervalDay.ItemIndex := IntervalDay;
    cmb_IntervalDate.ItemIndex := IntervalDate;
    cmb_IntervalHour.ItemIndex := IntervalHour;
    lst_dragfiles.Items.Clear;
    lst_dragfiles.Items := DragFileStringList;
  end;
end;

procedure Tmainform.ReadFromGUI(AProfileName: string);
// Prepend OutputDir to paths of of Versionini and ZipFilename
// So that full path is stored in ProfileRec and Config
// WhatsNew.txt has it's own path
begin
  with ProfileRec do
  begin
    ProfileName := AProfileName;
    AppPath := edt_AppFileName.Text;
    NewVersion.V1 := spn1.Value;
    NewVersion.V2 := spn2.Value;
    NewVersion.V3 := spn3.Value;
    NewVersion.V4 := spn4.Value;
    FillVersionString(NewVersion);
    OutDir := CleanAndExpandDirectory(TrimFilename(edt_OutputDirectory.Text));
    // TODO Linux/Windows
    VersionsINI := OutDir + edt_VersionsINIFile.Text; // restore path
    WhatsNewPath := edt_WhatsNewTextFile.Text;
    SFProjectName := edt_SFProjectName.Text;
    ZipFileName := OutDir + edt_ZipFileName.Text; // restore path
    CopyTree := chk_UseCopyTree.Checked;
    CopyTreeDirectory := edt_CopyTree.Text;
    SFUpdatesDirectory := edt_Updates.Text;
    ShowUpdatesInCaption := chk_ShowUpdateInCaption.Checked;
    ShowDialogs := chk_ShowDialogs.Checked;
    DebugMode := chk_DebugMode.Checked;
    OverrideUserPrefs := chk_overrideUserPrefs.Checked;
    IntervalType := cmb_IntervalType.ItemIndex;
    IntervalDay := cmb_IntervalDay.ItemIndex;
    IntervalDate := cmb_IntervalDate.ItemIndex;
    IntervalHour := cmb_IntervalHour.ItemIndex;
  end;
end;

procedure Tmainform.FillVersionString(var AVersionRec: TVersionRec);
begin
  AVersionRec.VString := IntToStr(AVersionRec.v1) + '.';
  AVersionRec.VString += IntToStr(AVersionRec.v2) + '.';
  AVersionRec.VString += IntToStr(AVersionRec.v3) + '.';
  AVersionRec.VString += IntToStr(AVersionRec.v4);
end;

procedure Tmainform.FillVersionInteger(var AVersionRec: TVersionRec);
// Converts a version string '0.1.2.3' into integers
var
  i: integer;
  s: string;
begin
  s := ExtractDelimited(1, AVersionRec.VString, ['.']);
  if TryStrToInt(s, i) then
    AVersionRec.v1 := i
  else
    AVersionRec.v1 := 0;
  s := ExtractDelimited(2, AVersionRec.VString, ['.']);
  if TryStrToInt(s, i) then
    AVersionRec.v2 := i
  else
    AVersionRec.v2 := 0;
  s := ExtractDelimited(3, AVersionRec.VString, ['.']);
  if TryStrToInt(s, i) then
    AVersionRec.v3 := i
  else
    AVersionRec.v3 := 0;
  s := ExtractDelimited(4, AVersionRec.VString, ['.']);
  if TryStrToInt(s, i) then
    AVersionRec.v4 := i
  else
    AVersionRec.v4 := 0;
end;

function Tmainform.MakeZipfile: boolean;
  // The ProfileRec elements AppPath and WhatsNewPath contain a full Path
  // TODO: Use whitelist of files to put into users app folder
var
  AZipper: TZipper;
  szPathEntry: string;
  i: integer;
  ZEntries: TZipFileEntries;
  TheFileList: TStringList;
begin
  Result := False;
  if not FileExistsUTF8(ProfileRec.AppPath) then
    exit;
  if not FileExistsUTF8(ProfileRec.WhatsNewPath) then
    exit;
  if FileExistsUTF8(ProfileRec.Zipfilename) then
  begin
    DeleteFile(ProfileRec.Zipfilename);
    Application.ProcessMessages;
    if FileExistsUTF8(ProfileRec.Zipfilename) then
      if DebugMode then
        EventLog1.Error('Unable to delete %s', [ProfileRec.Zipfilename]);
  end;

  AZipper := TZipper.Create;
  try
    try
      AZipper.Filename := ProfileRec.Zipfilename;
      AZipper.Clear;
      ZEntries := TZipFileEntries.Create(TZipFileEntry);

      // Add the exe and whatsnew.txt
      ZEntries.AddFileEntry(ProfileRec.AppPath,
        ExtractFileName(ProfileRec.AppPath));
      ZEntries.AddFileEntry(ProfileRec.WhatsNewPath, ExtractFileName(
        ProfileRec.WhatsNewPath));
      ZEntries.AddFileEntry(ProfileRec.OutDir + C_LAUTRayINI, C_LAUTRayINI);


      // Add any Extra files into the root of the zipfile
      if (ProfileRec.DragFileStringList.Count > 0) then
        for i := 0 to ProfileRec.DragFileStringList.Count - 1 do
        begin
          ZEntries.AddFileEntry(ProfileRec.DragFileStringList[i],
            ExtractFileName(ProfileRec.DragFileStringList[i]));
        end;
      if ProfileRec.CopyTree then
      begin
        // Verify valid directory
        if DirPathExists(ProfileRec.CopyTreeDirectory) then
        begin
          // Construct the path to the directory BELOW CopyTreeDirectory
          // If user specifies 'C:\MyFolder\Subfolder' it returns 'C:\MyFolder\'
          // If user specifies 'C:\MyFolder' it returns 'C:\'
          // If user specifies 'C:\' it returns 'C:\'
          i := RPos(PathDelim, ChompPathDelim(ProfileRec.CopyTreeDirectory));
          szPathEntry := LeftStr(ProfileRec.CopyTreeDirectory, i);

          // Use the FileUtils.FindAllFiles function to get everything (files and folders) recursively
          TheFileList := TStringList.Create;
          try
            TheFileList := FindAllFiles(ProfileRec.CopyTreeDirectory);
            for i := 0 to TheFileList.Count - 1 do
            begin
              // Make sure the CopyTreeDirectory files are not in the root of the ZipFile
              ZEntries.AddFileEntry(TheFileList[i], CreateRelativePath(
                TheFileList[i], szPathEntry));
            end;
          finally
            TheFileList.Free;
          end;
        end;
      end;
      if (ZEntries.Count > 0) then
        AZipper.ZipFiles(ZEntries);
    except
      On E: EZipError do
        E.CreateFmt('Zipfile could not be created%sReason: %s', [LineEnding, E.Message])
    end;
    Result := True;
  finally
    FreeAndNil(ZEntries);
    AZipper.Free;
  end;
end;

function Tmainform.MakeINIFile: boolean;
  // Makes an INI file for the TrayNotify application
var
  VersionINI: TINIFile;
begin
  Result := False;
  VersionINI := TINIFile.Create(ProfileRec.VersionsINI);
  try
    try
      VersionINI.WriteString('versions', 'GUI', ProfileRec.NewVersion.VString);
      VersionINI.UpdateFile;
    except
      On E: Exception do
        E.CreateFmt('VersionINI could not be created%sReason: %s',
          [LineEnding, E.Message])
    end;
    Result := True;
  finally
    VersionINI.Free;
  end;
end;

function Tmainform.MakeLAUTrayINIFile: boolean;
  // Use current ProfileRec
var
  LAUTRayINI: TINIFile;
  szSectionName: string;
begin
  Result := False;
  if FileExistsUTF8(ProfileRec.Outdir + C_LAUTRayINI) then
    DeleteFile(ProfileRec.Outdir + C_LAUTRayINI);
  Application.ProcessMessages;
  if FileExistsUTF8(ProfileRec.Outdir + C_LAUTRayINI) then
    if DebugMode then
      EventLog1.Error('Unable to delete %s', [ProfileRec.Outdir + C_LAUTRayINI]);

  LAUTRayINI := TINIFile.Create(ProfileRec.Outdir + C_LAUTRayINI);
  try
    try
      szSectionName := ProfileRec.ProfileName;
      LAUTRayINI.WriteString(szSectionName, 'AppPrettyName', szSectionName);
      LAUTRayINI.WriteString(szSectionName, 'AppPath',
        ExtractFilename(ProfileRec.AppPath));
      LAUTRayINI.WriteString(szSectionName, 'INIPath',
        ExtractFilename(ProfileRec.VersionsINI));
      LAUTRayINI.WriteString(szSectionName, 'ZipPath',
        ExtractFilename(ProfileRec.ZipFileName));
      LAUTRayINI.WriteString(szSectionName, 'AppVersion', ProfileRec.NewVersion.VString);
      LAUTRayINI.WriteString(szSectionName, 'SFProjectName', ProfileRec.SFProjectName);
      LAUTRayINI.WriteString(szSectionName, 'SFUpdatesDirectory',
        ProfileRec.SFUpdatesDirectory);
      if ProfileRec.OverrideUserPrefs then
      begin
        LAUTRayINI.WriteInteger(szSectionName, 'IntervalType', ProfileRec.IntervalType);
        LAUTRayINI.WriteInteger(szSectionName, 'IntervalDay', ProfileRec.IntervalDay);
        LAUTRayINI.WriteInteger(szSectionName, 'IntervalDate', ProfileRec.IntervalDate);
        LAUTRayINI.WriteInteger(szSectionName, 'IntervalHour', ProfileRec.IntervalHour);
      end;
      LAUTRayINI.UpdateFile;
    except
      On E: Exception do
        E.CreateFmt('LAUTRayINI could not be created%sReason: %s',
          [LineEnding, E.Message]);
    end;
    Result := True;
  finally
    LAUTRayINI.Free;
  end;
end;
initialization
// Unpack readme.txt
if (lcl_major > 0) and (lcl_minor > 6) then
 begin
    sReadMePath:=ProgramDirectory + 'readme.txt';
      // This uses a resource file added via Project/Options (Laz 1.7+)
    if not FileExistsUTF8(sReadMePath) then
    begin
      // create a resource stream which points to the file
      S := TResourceStream.Create(HInstance, 'README', MakeIntResource(10));
      try
        F := TFileStream.Create(sReadMePath, fmCreate);
        try
          F.CopyFrom(S, S.Size); // copy data from the resource stream to file stream
        finally
          F.Free; // destroy the file stream
        end;
      finally
        S.Free; // destroy the resource stream
      end;
    end;
  end;
end.
