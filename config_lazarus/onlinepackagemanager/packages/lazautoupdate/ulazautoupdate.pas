unit ulazautoupdate;

{
  Summary and Copyright
  =====================
  LazAutoUpdate (c)2015 Gordon Bamber (minesadorada@charcodelvalle.com)
  A Lazarus Visual component that enables an update service for Executables.

  Web References
  ==============
  Wiki Page: http://wiki.freepascal.org/LazAutoUpdater
  Forum thread: http://forum.lazarus.freepascal.org/index.php/topic,25444.0.html
  SourceForge project: https://sourceforge.net/projects/lazautoupdate/

  Other Credits
  =============
  VersionSupport:  Mike Thompson - mike.cornflake@gmail.com
  (added to and modified by minesadorada@charcodelvalle.com)
  Windows admin RunAs function:  Vincent at freepascal forum
  THpttpClient code: GetMem at freepascal forum

  License
  =======
  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

  Version Control and History
  ===========================
  Via SVN: https://svn.code.sf.net/p/lazarus-ccr/svn/components/lazautoupdate/
  Via SourceForge: https://sourceforge.net/projects/lazautoupdate/
  Also see 'Version History' below
}
{$mode objfpc}{$H+}

interface


uses
  Forms, Classes, SysUtils, lazautoupdate_httpclient, strutils, PropEdits,
  LazUTF8, FileUtil, LazFileUtils, Dialogs, StdCtrls,
  Buttons, DateUtils,{$IFDEF LINUX}process, asyncprocess,{$ENDIF}zipper, LResources,
  VersionSupport, inifiles, aboutlazautoupdateunit, uappisrunning, LCLProc,
  fileinfo, open_ssl, ushortcut, winpeimagereader {need this for reading exe info}
  , elfreader {needed for reading ELF executables}
  , machoreader {needed for reading MACH-O executables}
  {$IFDEF WINDOWS}, Windows, ShellAPI{$ENDIF}; // Thanks to Windows 10 and 704 error

const
  C_SOURCEFORGEURL =
    'https://sourceforge.net/projects/%s/files/%s/%s/download';
  // [updatepath,projectname,filename]
  C_GITHUBFILE_URL = 'https://raw.github.com/%s/%s/%s/%s';
  // https://raw.github.com/<username>/<repo>/<branch>/file
  //  GitHubUserName,GitHubProjectName,updatepath,filename
  C_GITHUBFILE_URL_UPDATES = 'https://raw.github.com/%s/%s/%s/%s/%s';
  // https://raw.github.com/<username>/<repo>/<branchname>/some_directory/file
  // https://raw.github.com/<username>/<repo>/<tagname>/some_directory/file


{
 Version History
 ===============
 V0.0.1: Initial alpha
 V0.0.2: Added auOther properties
 V0.0.3: Limit to download time
         auOther code working
 V0.0.4: Added Events
 V0.0.5: Improved error handling in DownloadNewVersion
         Added LastError property
         Added VersionCountLimit rpoerty
         Added DownloadCountLimit property
 V0.0.6: Linux implementation
         Property CopyTree added
 V0.0.7: Added Sleep(1) to download code
 V0.0.8: UpdateToNewVersion now shells AppFileWithPath
 V0.0.9: AutoUpdate method added
 V0.1.0: Added WaitFor routine to simulate Sleep in Linux
 V0.1.1: Tidied up Debugmode
         Improved version comparison
 V0.1.3: Fixed bug whereby Setting AppVersion property gave incorrect value
 V0.1.4: Added public AppVersionNumber property
 V0.1.5: Added Scrollbars to the WhatsNew memo
 V0.1.6: Added Public ResetAppVersion method
         Added Private VersionStringToNumber function
 V0.1.7: Added Public SilentUpdate method
         Added public properties:
         LCLVersion,WidgetSet, FPCVersion,LastCompiled,TargetOS
 V0.1.8: Bugfix: Removed Parent Form code if fSilentMode=TRUE
 V0.1.9: Added public AppRunningSilentUpdate method
 V0.1.10:Added uAppIsRunning unit
         Added IsAppActive public function
 V0.1.11:SilentInstall method
 V0.1.12:Moved ProgramDir references to fAppFileName references
 V0.1.13:Bugfix Update
 V0.1.14:Added debug messages in DoSilentUpdate method
 V0.1.15:DoSilentUpdate: Added code for CopyTree=TRUE/FALSE
         Changed some Ansi functions to UTF8
 V0.1.16:$IFDEF WINDOWS UpdateToNewVersion kills app if running
 V0.1.17:Added public methods CreateLocalLauImportFile and RelocateLauImportFile
 V0.1.18:Added public method RemoteUpdateToNewVersion
 V0.1.19:Improved VersionStringToNumber
 V0.1.20:Bugfix: 'No Build Information Available' -> '0.0.0.0'
 V0.1.21:Bugfix: RemoteUpdate killed app too soon
 V0.1.22:lauimport file is not re-created if it already exists
 V0.1.23:Bugfix: PrettyName in lauimport sometimes contained duplicated OS info
 V0.1.24:Bugfix to CreateLocalLauImportFile
         More checks on PrettyName
 V0.1.25:Changed default: CopyTree = TRUE
 V0.1.26:Updated uses clause for FileUtils.
 V0.2.0: Rewritten for 2017
 V0.2.4: GitHub integration with branches
 V0.2.5: IsWindowsAdministrator check added and property to control it
 V0.2.6: Enabled GitHub tags (GitHubBranchOrTag property)
 V0.2.7: Updates Tray Updater routines
 V0.2.8: Changed constants C_UPDATEHMNAME and C_LAUUPDATENAME
 V0.2.9: Added CreateLocalLauImportFile in UpdateToNewVersion
 V0.3.1: Added SetExecutePermission (LINUX only)
 V0.3.2: Bugfix for DoSilentUpdate
 V0.3.3: Added event OnUpdate
 V0.3.4: Added unit ushortcut (CreateDesktopShortCut) for installers
 V0.3.5: Rule #3:There is to be NO v0.3.5.0
 V0.3.6: Bugfixed CreateShortCut code
 V0.3.7: Added public property Mode=(lauUpdate|lauInstall)
 V0.3.7.1: Added (DoSilentUpdate) copy C_UPDATEHMNAME to installed folder
 V0.3.7.2: Unix: SetExecutePermissions on installed app
 V0.3.8: Shortcut Menu items now created/deleted
}
  C_TLazAutoUpdateComponentVersion = '0.3.8';
  C_TThreadedDownloadComponentVersion = '0.0.3.0';
{
 V0.0.1: Initial alpha
 V0.0.2: Added fDebugmode to all classes and functions
 V0.0.3: Changed to http_client
}
  C_OnlineVersionsININame = 'versions.ini'; // User can change
  C_UpdatesFolder = 'updates'; // User can change

  // Don't change these without some thought..
  C_LAUTRayINI = 'lauimport.ini'; // Name syncronises with TrayUpdater App
  C_WhatsNewFilename = 'whatsnew.txt';
  C_INISection = 'versions';
  C_GUIEntry = 'GUI';
  C_ModuleEntry = 'Module';
  C_MASTER = 'master';
  // Compiler mode directives
  // (note: nothing for Mac/Darwin)
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
  C_PFX = C_OS + C_BITNESS; // Used in file naming
  {$IFDEF WINDOWS}
  C_UPDATEHMNAME = 'updatehm' + C_PFX + '.exe';
  C_LAUUPDATENAME = 'lauupdate' + C_PFX + '.exe';
  {$ELSE}
  C_UPDATEHMNAME = 'updatehm' + C_PFX;
  C_LAUUPDATENAME = 'lauupdate' + C_PFX;
  {$ENDIF}
  // Windows Constants (unused)
  C_RUNONCEKEY = 'HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\CurrentVersion\RunOnce';
  C_RUNKEY = 'HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\CurrentVersion\Run';

resourcestring
  C_ComponentPrettyName = 'Lazarus Auto-Update Component';
  C_TempVersionsININame = 'new%s'; // [C_OnlineVersionsININame]
  C_Checking = 'Checking for updates...';
  C_NoSFTypes = 'Sorry only ProjectType = auSourceForge is supported in this version';
  C_Downloading = 'Please wait.  Downloading new version... ';
  C_ConsoleTitle = 'Updating %s'; //[fAppFileName]
  C_TakingTooLong =
    'Check is taking too long (bad/slow internet connection?). Try again later?';
  C_Error500 = 'There is a problem with the Internet connection (error %d)  Try again later?';
  C_Error404 = 'Cannot find the file at this time. (error %d)  Try again later?';
  C_UnableToDelete = 'Sorry, unable to delete %s%sPlease delete it manually';
  C_OK = 'OK';
  C_NotProperFileName = 'This is not a proper file name';
  C_WhatsNewInVersion = 'What''s new in version %s';
  C_PropIsEmpty = 'Property SFProjectName is empty!';
  C_ThreadDownloadCrash = 'ThreadDownloadHTTP Crashed! (NewVersionAvailable)';
  C_DownloadedBytes = 'Downloaded %s: %d bytes';
  C_UnableToDeleteOld = 'Unable to delete old files in %s';
  C_DirectoryProblems = 'Problems with the % directory';
  C_ThreadDownloadHTTPCrash = 'ThreadDownloadHTTP Crashed! (NewVersionAvailable)';
  C_DownloadSuccess = 'Downloaded new version %s sucessfully.';
  C_UnableToDownload = 'Unable to download new version%sReturn code was %d';
  C_PleaseWaitProcessing = 'Please wait. Processing....';
  C_UpdaterMissing = 'Missing %s';
  C_FolderMissing = 'Missing %s folder';
  C_NotApplicable = '<not applicable>';
  C_ThreadStarted = 'Thread Started';
  // C_SourceForgeDownload = 'SourceForge download';
  C_CannotLoadFromRemote = 'Cannot load document from remote server';
  C_DownloadIsEmpty = 'Downloaded document is empty.';
  C_DownloadFailedErrorCode = 'Download failed with error code ';
  rsANewVersionS = 'A new version %s is available.  Would you like to download it?';
  rsVewVersionSH = 'Vew version %s has downloaded. Click OK to update now.';
  rsCancelledYou = 'Cancelled.  You can download and update to the new version' +
    ' later.';
  rsDownloadFail = 'Download failed. (HTTP Errorcode %d) Try again later';
  rsCancelledYou2 = 'Cancelled.  You can download the new version later.';
  rsThisApplicat = 'This application is up-to-date';
  rsOnlyWindowsU = 'Only Windows users whith Administrator status can update ' +
    'this application.%sPlease log off, then log on as an administrator (or ' +
    'switch users to an administrator account),%sthen try again.  This ' +
    'restriction is for the safety and security of your Windows system.%' +
    'sClick OK to continue';
  rsApplicationU = 'Application update';
  rsSImportantMe = '%sImportant message from LazAutoUpdate component:%sThere ' +
    'is no version information in your project!%sClick [Continue], and/or [' +
    'Abort] to quit, and use%sIDE menu item Project/Project Options/Version ' +
    'Info%sto add Version Info by clicking the checkbox.';
  rsNoBuildInfor = 'No build information available';


type
  // Dummy thread to initialise the threading system
  tc = class(tthread)
    procedure Execute; override;
  end;

  // This type is currently unused
  TProjectType = (auSourceForge, auGitHubReleaseZip, auOther);
  // Array of these records used for multiple updates
  UpdateListRecord = record
    PrettyName: string;
    Path: string;
    VersionString: string;
    VersionNumber: cardinal;
  end;

  TWorkingMode = (lauUpdate, lauInstall);

  TThreadedDownload = class; // Forward declaration
  TShortCutClass = class; // Forward declaration
  {TLAZAUTOUPDATE}
  // Event declarations
  TOnNewVersionAvailable = procedure(Sender: TObject; Newer: boolean;
    OnlineVersion: string) of object;
  TOnDownloaded = procedure(Sender: TObject; ResultCode, BytesDownloaded: integer) of
    object;
  TOnDebugEvent = procedure(Sender: TObject; lauMethodName, lauMessage: string) of
    object;
  TOnUpdated = procedure(Sender: TObject; NewVersion, LauMessage: string) of object;

  TLazAutoUpdate = class(TAboutLazAutoUpdate)
  private
    fSourceForgeProjectName: string;
    fGitHubProjectName: string;
    fGitHubRepositoryName: string;
    fGitHubBranchOrTag: string;
    fApplicationVersionString: string;
    fApplicationVersionQuad: TVersionQuad;
    fGuiQuad: TVersionQuad;
    fProjectType: TProjectType;
    fThreadDownload: TThreadedDownload;
    fAppFileName: string;
    fComponentVersion: string;
    fShowUpdateInCaption: boolean;
    fUpdateList: array of UpdateListRecord;
    fUpdateListCount: integer;
    fUpdatesFolder: string;
    fDownloadZipName: string;
    fVersionsININame: string;
    fParentApplication: TApplication;
    fParentForm: TForm;
    fGUIOnlineVersion: string;
    fShowDialogs: boolean;
    fDownloadInprogress: boolean;
    fWindowsAdminCheck: boolean;
    fShortCutClass: TShortCutClass;
    fWorkingMode: TWorkingMode;
    {$IFDEF UNIX}
    FUpdateHMProcess: TAsyncProcess;
    {$ENDIF}
    fauOtherSourceURL: string;
    fauOtherSourceFilename: string;
    WhatsNewForm: TForm;
    WhatsNewMemo: TMemo;
    cmdClose: TBitBtn;
    FOnNewVersionAvailable: TOnNewVersionAvailable;
    FOnDownloaded: TOnDownloaded;
    fOnDebugEvent: TOnDebugEvent;
    fOnUpdated: TOnUpdated;
    fLastError: string;
    fVersionCountLimit, fDownloadCountLimit: cardinal;
    fZipfileName: string;
    fCopyTree: boolean;
    fDebugMode, fFireDebugEvent: boolean;
    fSilentMode: boolean;
    fLCLVersion, fWidgetSet, fFPCVersion, fLastCompiled, fTargetOS: string;
    // fQuad: TVersionQuad;
    fProgVersion: TProgramVersion;
    objFileVerInfo: TFileVersionInfo;
    fUpdateExe, fUpdateSilentExe: string;
    procedure SetProjectType(AValue: TProjectType);
    // projectype=auOther property Sets
    procedure SetauOtherSourceFilename(AValue: string);
    procedure SetauOtherSourceURL(AValue: string);

    procedure SetSourceForgeProjectName(Avalue: string);
    procedure SetAppFilename(Avalue: string);
    procedure SetApplicationVersionString(Avalue: string);
    procedure SetShowDialogs(AValue: boolean);
    procedure SetDebugMode(AValue: boolean);
    function GetThreadDownloadReturnCode: integer;
    function IsOnlineVersionNewer(const sznewINIPath: string): boolean;
    function DoSilentUpdate: boolean;
    function GetUpdateSilentExe: string;
    function GetUpdateExe: string;
  protected

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DebugTest;
    {Main functions}
    // If  NewVersionAvailable then DownloadNewVersion then UpdateToNewVersion
    // Returns TRUE if GUIVersion > AppVersion
    function NewVersionAvailable: boolean;
    // Returns TRUE if successful
    function DownloadNewVersion: boolean;
    // Returns TRUE if successful. Also creates a C_LAUTRayINI file in the GetAppConfig folder for TrayUpdater
    function UpdateToNewVersion: boolean;
    // Put in form.activate. Shows <whatsnew.txt> only if in ProgramDirectory then deletes it. Exits otherwise
    procedure ShowWhatsNewIfAvailable;
    // Checks for new version then shows dialogs to update
    Function AutoUpdate:Boolean;
    // No dialogs - what it says on the tin.
    function SilentUpdate: boolean;
    // Used in SilentUpdate. Shells to local lauupdate(.exe)
    function RemoteUpdateToNewVersion: boolean;
    // Returns TRUE if EXEName is running under Windows or Linux
    function AppIsActive(const ExeName: string): boolean;

    // Resets AppVersion property to the ownling application version
    procedure ResetAppVersion;
    // Create a new lauimport.ini in GetAppConfigDirUTF8 folder
    function CreateLocalLauImportFile: boolean;
    // If lauimport.ini is found in the app folder, move it to the AppData folder
    procedure RelocateLauImportFile;
    // Uses properties in TShortCutClass
    function MakeShortCut: boolean;
    function DeleteShortCut: boolean; // (use fShortCutClass.ShortCutName)

    // Download lists (now superceded by CopyTree)
    // TODO: Use Indexed properties to handle list access
    function AddToUpdateList(APrettyName, APath, AVersionString: string;
      AVersionNumber: cardinal): integer;
    procedure ClearUpdateList;
    property UpdateListCount: integer read fUpdateListCount;


    // GUI can use these properties during and after downloads
    // NewVersionAvailable sets this.  It is the online version
    property GUIOnlineVersion: string read fGUIOnlineVersion;
    // Set by NewVersionAvailable and DownLoadNewVersion
    property ReturnCode: integer read GetThreadDownloadReturnCode;
    // Set by NewVersionAvailable and DownLoadNewVersion when running
    property DownloadInprogress: boolean read fDownloadInprogress;

    // The name of the zipfile in the remote <updates> directory
    property DownloadZipName: string read fDownloadZipName;
    // The Path + Filename of the app to overwite and then run
    property AppFileWithPath: string read fAppFilename write SetAppFilename;
    // The version string of the app to be updated.  You can set this to '0.0.0.0' for a definite update.
    property AppVersion: string read fApplicationVersionString
      write SetApplicationVersionString;
    // Can be queried
    property LastError: string read fLastError;
    // Debugging use only
    property DebugMode: boolean read fDebugMode write SetDebugMode;
    //    property AppVersionNumber: integer read fApplicationVersionQuad;

    // Info useful for About dialogs
    property LCLVersion: string read fLCLVersion;
    property WidgetSet: string read fWidgetSet;
    property FPCVersion: string read fFPCVersion;
    property LastCompiled: string read fLastCompiled;
    property TargetOS: string read fTargetOS;
    property WindowsAdminCheck: boolean read fWindowsAdminCheck write fWindowsAdminCheck;
    {$IFDEF LINUX}
    // Used in UpdateToNewVersion
    function SetExecutePermission(const AFileName: string; var AErrMsg: string): boolean;
    {$ENDIF}
  published
    // Events
    property OnNewVersionAvailable: TOnNewVersionAvailable
      read FOnNewVersionAvailable write FOnNewVersionAvailable;
    property OnDownloaded: TOnDownloaded read fOnDownloaded write fOnDownloaded;
    property OnDebugEvent: TOnDebugEvent read fOnDebugEvent write fOnDebugEvent;
    property OnUpdated: TOnUpdated read fOnUpdated write fOnUpdated;

    // Embedded class
    property ThreadDownload: TThreadedDownload
      read fThreadDownload write fThreadDownload;
    // Set this property before using methods
    property SFProjectName: string read fSourceForgeProjectName
      write SetSourceForgeProjectName;
    // For when fProjectType = auOther
    // Fully qualified URL (not including the filename).
    property auOtherSourceURL: string read fauOtherSourceURL write SetauOtherSourceURL;
    // Just the filename of the file to be downloaded (can be zipped)
    property auOtherSourceFilename: string read fauOtherSourceFilename
      write SetauOtherSourceFilename;

    property ProjectType: TProjectType
      read fProjectType write SetProjectType default auSourceForge;
    // Version of this component
    property AutoUpdateVersion: string read fComponentVersion;
    // Zipfile contains a whole directory tree (relative to App Directory)
    property CopyTree: boolean read fCopyTree write fCopyTree default True;
    // Default is 'updates' *must be the same in SourceForge file section or GitHub Branch subfolder*
    property UpdatesFolder: string read fUpdatesFolder write fUpdatesFolder;
    // Default=versions.ini  File in SourceForge/GitHub /updates folder
    property VersionsININame: string read fVersionsININame write fVersionsININame;
    // Default is to modify parent form's caption during downloads
    property ShowUpdateInCaption: boolean read fShowUpdateInCaption
      write fShowUpdateInCaption default False;
    // Set to FALSE if you want to handle them in form code
    property ShowDialogs: boolean read fShowDialogs write SetShowDialogs default False;
    // How many counts to wait until 'Too long' meesage quits out
    property VersionCountLimit: cardinal read fVersionCountLimit
      write fVersionCountLimit;
    // How many counts to wait until 'Too long' meesage quits out
    property DownloadCountLimit: cardinal read fDownloadCountLimit
      write fDownloadCountLimit;
    // Default is application filename.zip
    property ZipfileName: string read fZipfileName write fZipfileName;
    // Name of Console app
    property UpdateExe: string read GetUpdateExe;
    // Name of Console app
    property UpdateExeSilent: string read GetUpdateSilentExe;
    // Main project name/UserName
    property GitHubProjectname: string read fGitHubProjectName write fGitHubProjectName;
    // Name of your GitHub repository within the project/username
    property GitHubRepositoryName: string read fGitHubRepositoryName
      write fGitHubRepositoryName;
    // Default=master but any branchname or tagname is OK
    property GitHubBranchOrTag: string read fGitHubBranchOrTag write fGitHubBranchOrTag;
    // Install or Update (default=Update)
    property WorkingMode: TworkingMode read fWorkingMode write fWorkingMode;
    // Subproperties available
    property ShortCut: TShortCutClass read fShortCutClass write fShortCutClass;
  end;

  TShortCutCategory = (scAudioVideo, scAudio, scDevelopment,
    scEducation, scGame, scGraphics, scNetwork, scOffice, scScience, scSettings,
    scSystem, scUtility);
  // TShortCutCategoryFlags = Set of TShortCutCategory;

  TShortCutClass = class(TPersistent)
  private
    // ShortCut stuff for CreateDesktopShortCut in ushortcut.pas
    fShortCutTarget: string;
    fShortCutTargetArguments: string;
    fShortCutShortcutName: string;
    fShortCutIconFileName: string;
    fShortCutCategory: TShortCutCategory; // For easier property access
    procedure SetShortCutCategoryString(ACategory: TShortCutCategory);
  public
    fShortCutCategoryString: string;
    constructor Create; // Constructor must be public
    destructor Destroy; override; // Destructor must be public
    property CategoryString: string read fShortCutCategoryString;
  published
    property Target: string read fShortCutTarget write fShortCutTarget;
    property TargetArguments: string read fShortCutTargetArguments
      write fShortCutTargetArguments;
    property ShortcutName: string read fShortCutShortcutName write fShortCutShortcutName;
    property IconFileName: string read fShortCutIconFileName write fShortCutIconFileName;
    property Category: TShortCutCategory read fShortCutCategory
      write SetShortCutCategoryString;
  end;

  {TThreadedDownload }
  TThreadedDownload = class(TPersistent)
  private
    fURL: string;
    fFileName: string;
    fReturnCode: integer;
    fThreadFinished: boolean;
    fDownloadSize: integer;
    fUnzipAfter: boolean;
    fComponentVersion: string;
    fApplicationVersionString: string;
    fIsSourceForge: boolean;
  public
    fDebugMode: boolean;
    fShowDialogs: boolean;
    fLastError: string;  // Propagated to TLazAutoUpdate
    constructor Create;
    // Starts the thread
    function ThreadDownloadHTTP: boolean;
    // Called when the thread is done
    procedure DownloadTerminiated(Sender: TObject);
    // Passed to the thread
    property URL: string read fURL write fURL;
    // Passed to the thread
    property Filename: string read fFileName write fFileName;
    // From TLazAutoUpdate
    property AppVersion: string read fApplicationVersionString
      write fApplicationVersionString;
    // From the thread
    property ReturnCode: integer read fReturnCode write fReturnCode;
    // From DownloadTerminated
    property ThreadFinished: boolean read fThreadFinished write fThreadFinished;
    // From the thread
    property DownloadSize: integer read fDownloadSize write fDownloadSize;
    // From TLazAutoUpdate
    property UnzipAfter: boolean read fUnzipAfter;
    // From TLazAutoUpdate
    property IsSourceForge: boolean read fIsSourceForge;
  published
    // Version of the underlying thread class
    property ThreadDownloadVersion: string read fComponentVersion;
  end;

  {TDownloadThreadClass }
  TDownloadThreadClass = class(TThread)
  private
    fURL: string;
    fFileName: string;
  public
    fIsSourceForge: boolean; // Propagated from TLazAutoUpdate
    fDebugMode: boolean; // propagated from TLazAutoUpdate
    fShowDialogs: boolean; // propagated from TLazAutoUpdate
    fDownloadSize: integer; // propagated to TThreadedDownload
    fReturnCode: integer; // Propagated to TThreadedDownload
    fLastError: string;  // Propagated to TThreadedDownload
    constructor Create(URL, FileName: string);
    procedure Execute; override; // Starts thread
    // Todo:
    {
    procedure GetDownloadSize;
    procedure ShowProgress;
    }
  end;

  // For the TShortCutClass filename properties (needs propedits unit)
  TMyFileNamePropertyEditor = class(TFileNamePropertyEditor)
  public
    // Override the Edit method for total control
    function GetFilter: string; override;
    function GetDialogOptions: TOpenOptions; override;
    function GetDialogTitle: string; override;
  end;


// Non-threaded function
function DownloadHTTP(URL, TargetFile: string; var ReturnCode, DownloadSize: integer;
  bIsSourceForge, fDebugMode: boolean): boolean;

procedure Register;

implementation

procedure Register;
begin
  {$I lazautoupdate_icon.lrs}
  RegisterComponents('System', [TLazAutoUpdate]);
  // Register the custom property editors for the TShortCutClass filename properties
  RegisterPropertyEditor(TypeInfo(string),
    TShortCutClass, 'Target', TMyFileNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(string),
    TShortCutClass, 'IconFileName', TMyFileNamePropertyEditor);
end;

// Start Property editors for File type properties in TShortCutClass
function TMyFileNamePropertyEditor.GetFilter: string;
begin
  {$IFDEF WINDOWS}
  Result := 'Windows executable|*.exe|All Files|*.*';
  {$ELSE}
    {$IFDEF LINUX}
  Result := 'Linux executable|*.|All Files|*.*';
    {$ELSE}
  Result := 'All Files|*.*';
    {$ENDIF}
  {$ENDIF}
end;

function TMyFileNamePropertyEditor.GetDialogOptions: TOpenOptions;
begin
  // To see the full list, drop an OpenDialog onto a form and see the Options property
  Result := [ofFileMustExist, ofPathMustExist];
end;

function TMyFileNamePropertyEditor.GetDialogTitle: string;
begin
  Result := 'Choose Shortcut Target Filename';
end;
// End Property editors for File type properties in TShortCutClass

// Dummy thread to initialise the threading process
procedure tc.Execute;
begin

end;

procedure WaitFor(const MillisecondDelay: longword);
// Linux - this proc is intentionally thread-blocking
var
  ThisSecond: longword;
begin
  ThisSecond := MilliSecondOfTheDay(Now);
  while MilliSecondOfTheDay(Now) < (ThisSecond + MillisecondDelay) do ;
end;

procedure TShortCutClass.SetShortCutCategoryString(ACategory: TShortCutCategory);
{
FreeDesktop Valid Categories:
TShortCutCategory = (scAudioVideo,scAudio,scDevelopment,
scEducation,scGame,scGraphics,scNetwork,scOffice,scScience,scSettings,
scSystem,scUtility);
}
begin
  if ACategory = fShortCutCategory then
    exit;

  fShortCutCategoryString := 'Unknown';
  case ACategory of
    scAudioVideo: fShortCutCategoryString := 'AudioVideo';
    scAudio: fShortCutCategoryString := 'Audio';
    scDevelopment: fShortCutCategoryString := 'Development';
    scEducation: fShortCutCategoryString := 'Education';
    scGame: fShortCutCategoryString := 'Game';
    scGraphics: fShortCutCategoryString := 'Graphics';
    scNetwork: fShortCutCategoryString := 'Network';
    scOffice: fShortCutCategoryString := 'Office';
    scScience: fShortCutCategoryString := 'Science';
    scSettings: fShortCutCategoryString := 'Settings';
    scSystem: fShortCutCategoryString := 'System';
    scUtility: fShortCutCategoryString := 'Utility';
  end;
end;

procedure TLazAutoUpdate.DebugTest;
begin
  ShowMessage(fApplicationVersionString);
end;

{$IFDEF WINDOWS}
// === START WINDOWS PROCS =====================================================
// This is all about permissions in Windows 10
procedure ShowAdminCheckMessage;
var
  sMessage: string;
begin
  sMessage := Format(rsOnlyWindowsU, [lineending, lineending, lineending]);
  MessageDlg(rsApplicationU, sMessage, mtInformation, [mbOK], 0);
end;

function IsXP: boolean;
var
  osVinfo: TOSVERSIONINFO;
begin
  ZeroMemory(@osVinfo, SizeOf(osVinfo));
  OsVinfo.dwOSVersionInfoSize := SizeOf(TOSVERSIONINFO);
  if ((GetVersionEx(osVInfo) = True) and (osVinfo.dwPlatformId =
    VER_PLATFORM_WIN32_NT) and (osVinfo.dwMajorVersion = 5) and
    (osVinfo.dwMinorVersion = 1)) then
    Result := True
  else
    Result := False;
end;

function IsWindowsAdmin: boolean;
const
  SECURITY_NT_AUTHORITY: TSIDIdentifierAuthority =
    (Value: (0, 0, 0, 0, 0, 5));
  SECURITY_BUILTIN_DOMAIN_RID = $00000020;
  DOMAIN_ALIAS_RID_ADMINS = $00000220;
var
  hAccessToken: THandle;
  ptgGroups: PTokenGroups;
  dwInfoBufferSize: DWORD;
  psidAdministrators: PSID;
  x: integer;
  bSuccess: BOOL;
  LastError: integer;
begin

  if Win32Platform <> VER_PLATFORM_WIN32_NT then
  begin
    Result := True;
    exit;
  end;

  Result := False;
  bSuccess := OpenThreadToken(GetCurrentThread, TOKEN_QUERY, True, hAccessToken);
  if not bSuccess then
  begin
    if GetLastError = ERROR_NO_TOKEN then
      bSuccess := OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, hAccessToken);
  end;
  if bSuccess then
  begin
    GetMem(ptgGroups, 1024);
    bSuccess := GetTokenInformation(hAccessToken, TokenGroups,
      ptgGroups, 1024, @dwInfoBufferSize);
    LastError := GetLastError;
    if not bSuccess then
      ShowMessage(format('GetLastError %d', [LastError]));
    CloseHandle(hAccessToken);
    if bSuccess then
    begin
      AllocateAndInitializeSid(SECURITY_NT_AUTHORITY, 2,
        SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_ADMINS,
        0, 0, 0, 0, 0, 0, psidAdministrators);
      {$R-}
      for x := 0 to ptgGroups^.GroupCount - 1 do
        if EqualSid(psidAdministrators, ptgGroups^.Groups[x].Sid) then
        begin
          Result := True;
          break;
        end;
      {$R+}
      FreeSid(psidAdministrators);
    end;
    FreeMem(ptgGroups);
  end;
end;

function IsWindowsAdminWinXP: boolean; // Currently unused
const
  GENERIC_READ = $80000000;
  GENERIC_WRITE = $40000000;
  GENERIC_EXECUTE = $20000000;
  GENERIC_ALL = $10000000;
var
  hSC: THandle;
begin
  Result := True;
  hSC := OpenSCManager(nil, nil, GENERIC_READ or GENERIC_WRITE or GENERIC_EXECUTE);

  if (hSC = 0) then
    Result := False;
  CloseServiceHandle(hSC);
end;

{$ENDIF}
// === END WINDOWS PROCS =======================================================
constructor TShortCutClass.Create;
begin
  inherited Create; // TComponent method;
end;

destructor TShortCutClass.Destroy;
begin
  inherited Destroy;
end;

constructor TLazAutoUpdate.Create(AOwner: TComponent);
var
  sz: string;
begin
  inherited Create(AOwner); // TComponent method;

  { initialise threading system }
  with tc.Create(False) do
  begin
    waitfor;
    Free;
  end;

  // Freed in Destroy
  fThreadDownload := TThreadedDownload.Create();

  fShortCutClass := TShortCutClass.Create();
  fShortCutClass.ShortcutName := 'MyShortcutName';
  fShortCutClass.TargetArguments := '';
  fShortCutClass.Category := scDevelopment;
  fShortCutClass.fShortCutCategoryString := 'Development';
  // Leave URL and Filename to be set via properties
  fComponentVersion := C_TLazAutoUpdateComponentVersion;
  // Unused
  ClearUpdateList;
  fUpdateListCount := 0;

  // Grab the application and form objects from the application
  fParentApplication := Tapplication(AOwner.Owner);
  fParentForm := TForm(AOwner);
  // Set default
  fApplicationVersionString := rsNoBuildInfor;
  // Get Versioninfo
  objFileVerInfo := TFileVersionInfo.Create(fParentApplication);
  try
    try
      objFileVerInfo.Filename := ParamStrUTF8(0);
      objFileVerInfo.ReadFileInfo;
      fApplicationVersionString := objFileVerInfo.VersionStrings.Values['FileVersion'];
      fileinfo.GetProgramVersion(fApplicationVersionQuad);
      fileinfo.GetProgramVersion(fProgVersion);
    except
      // EResNotFound raised if no versioninfo in project
      sz := rsSImportantMe;
      raise Exception.Createfmt(sz, [LineEnding, LineEnding,
        LineEnding, LineEnding, LineEnding]);
      FreeAndNil(fThreadDownload);
      FreeAndNil(fShortCutClass);
      Application.Terminate;
      // Eat other Exceptions?
    end;
  finally
    objFileVerInfo.Free;
  end;
  if (fApplicationVersionString = rsNoBuildInfor) then
    fApplicationVersionString := '0.0.0.0';

  fCopyTree := True; // User can change
  // UpdateList: Redundant?
  AddToUpdateList('', LazUTF8.ParamStrUTF8(0), GetFileVersion, 0);
  fWorkingMode := lauUpdate; // Default
  fProjectType := auSourceForge; // User can change
  fUpdatesFolder := C_UpdatesFolder; // User can change
  fVersionsININame := C_OnlineVersionsININame; // User can change
  fShowUpdateInCaption := False; // User can change
  fShowDialogs := False; // User can change
  fDebugMode := False;
  fFireDebugEvent := False;
  fSilentMode := False;

  // Propagate down
  fThreadDownload.fDebugmode := fDebugMode;
  if ((fProjectType = auSourceForge) or (fProjectType = auGitHubReleaseZip)) then
    fThreadDownload.fIsSourceForge := True
  else
    fThreadDownload.fIsSourceForge := False;

  fApplicationVersionQuad := StrToVersionQuad(fApplicationVersionString);

  fLastError := C_OK;

  fVersionCountLimit := 1000000; // default
  fDownloadCountLimit := 10000000; // default


  fZipfileName := ''; // assign later

  // BE SURE TO CHANGE THE CONSTANTS IF YOU CHANGE THE UPDATE EXE NAME
  GetUpdateSilentExe;
  GetUpdateExe;

  // Assorted versioninfo properties
  fLCLVersion := GetLCLVersion;
  fWidgetSet := GetWidgetSet;
  fFPCVersion := GetCompilerInfo;
  fLastCompiled := GetCompiledDate;
  fTargetOS := GetOS;
  fWindowsAdminCheck := True;


  // AboutBox properties
  AboutBoxComponentName := Format('Laz Auto-update v%s',
    [C_TLazAutoUpdateComponentVersion]);
  AboutBoxVersion := C_TLazAutoUpdateComponentVersion;
  AboutBoxWidth := 400;
  AboutBoxHeight := 450;
  sz := 'A component for updating your application' + LineEnding;
  sz += 'Designed for projects hosted by SourceForge and GitHub' +
    LineEnding + LineEnding;
  sz += 'Main methods:' + LineEnding;
  sz += 'Procedure AutoUpdate' + LineEnding;
  sz += 'Function NewVersionAvailable: Boolean' + LineEnding;
  sz += 'Function DownloadNewVersion: Boolean' + LineEnding;
  sz += 'Function UpdateToNewVersion: Boolean' + LineEnding;
  sz += 'Procedure ShowWhatsNewIfAvailable' + LineEnding;
  sz += 'For troubleshooting, set DebugMode=TRUE';
  AboutBoxTitle := 'LazAutoUpdate';
  AboutBoxDescription := sz;
  // AboutBoxBackgroundColor:=clWindow;
  //AboutBoxFontName (string)
  //AboutBoxFontSize (integer)
  AboutBoxAuthorname := 'Gordon Bamber';
  //AboutBoxOrganisation (string)
  AboutBoxAuthorEmail := 'minesadorada@charcodelvalle.com';
  AboutBoxLicenseType := 'MODIFIEDGPL';
end;

destructor TLazAutoUpdate.Destroy;
begin
  FreeAndNil(fThreadDownload);
  FreeAndNil(fShortCutClass);
  inherited Destroy;
end;

function TLazAutoUpdate.GetUpdateSilentExe: string;
begin
  fUpdateSilentExe := C_LAUUPDATENAME;
  if csDesigning in ComponentState then
    Result := 'lauupdate'
  else
    Result := fUpdateSilentExe;
end;

function TLazAutoUpdate.GetUpdateExe: string;
begin
  fUpdateExe := C_UPDATEHMNAME;
  if csDesigning in ComponentState then
    Result := 'updatehm'
  else
    Result := fUpdateExe;
end;

function TLazAutoUpdate.AppIsActive(const ExeName: string): boolean;
begin
  Result := AppIsRunning(ExeName);
end;

procedure TLazAutoUpdate.ResetAppVersion;
begin
  fApplicationVersionString := GetFileVersion;
  if (fApplicationVersionString = 'No build information available') then
    fApplicationVersionString := '0.0.0.0';
  fApplicationVersionQuad := StrToVersionQuad(fApplicationVersionString);
end;

procedure TLazAutoUpdate.SetShowDialogs(AValue: boolean);
begin
  fShowDialogs := AValue;
  if fThreadDownload <> nil then
    fThreadDownload.fShowDialogs := AValue;
end;

procedure TLazAutoUpdate.SetDebugMode(AValue: boolean);
begin
  fDebugMode := AValue;
  // Fire the OnDebugEvent event handler?
  if Assigned(fOndebugEvent) then
    fFireDebugEvent := fDebugMode;
  if fThreadDownload <> nil then
    fThreadDownload.fDebugMode := AValue;
end;

procedure TLazAutoUpdate.SetauOtherSourceURL(AValue: string);
// Must end in '/'
begin
  if ((AValue <> fauOtherSourceURL) and (AValue <> '')) then
  begin
    if not AnsiEndsStr('/', AValue) then
      AValue += '/';
    fauOtherSourceURL := AValue;
  end;
end;

procedure TLazAutoUpdate.SetauOtherSourceFilename(AValue: string);
begin
  if ((AValue <> fauOtherSourceFilename) and (AValue <> '')) then
  begin
    try
      fauOtherSourceFilename := ExtractFileName(AValue);
    except
      ShowMessage(C_NotProperFileName);
    end;
  end;
end;
// A couple of public functions for installer apps
function TLazAutoUpdate.MakeShortCut: boolean;
begin
  Result := False; // assume failure, look for success
  if Assigned(fOndebugEvent) then
    fFireDebugEvent := True;

  if fFireDebugEvent then
    fOndebugEvent(Self, 'MakeShortCut', 'MakeShortCut called');

  if fShortCutClass.Target = '' then
    fShortCutClass.Target := fAppFilename;

  if fFireDebugEvent then
    fOndebugEvent(Self, 'MakeShortCut', Format('Target=%s, TargetArguments=%s',
      [fShortCutClass.Target, fShortCutClass.TargetArguments]));

  if fFireDebugEvent then
    fOndebugEvent(Self, 'MakeShortCut',
      Format('Shortcut Name=%s, IconFileName=%s',
      [fShortCutClass.ShortcutName, fShortCutClass.IconFileName]));
  {$IFDEF LINUX}
  if fFireDebugEvent then
    fOndebugEvent(Self, 'MakeShortCut', Format('Category=%s',
      [fShortCutClass.CategoryString]));
  {$ENDIF}
  Result := CreateDesktopShortCut(fShortCutClass.Target,
    fShortCutClass.TargetArguments, fShortCutClass.ShortcutName,
    fShortCutClass.IconFileName, fShortCutClass.CategoryString);
  fLastError := GetShortCutDebugString;
  if fFireDebugEvent then
    if Result = True then
      fOndebugEvent(Self, 'MakeShortCut', 'MakeShortCut succeded.' +
        GetShortCutDebugString)
    else
      fOndebugEvent(Self, 'MakeShortCut', 'MakeShortCut failed.  Error(s): ' +
        GetShortCutDebugString);
end;

function TLazAutoUpdate.DeleteShortCut: boolean;
begin
  Result := False; // assume failure, look for success
  if Assigned(fOndebugEvent) then
    fFireDebugEvent := True;

  if fFireDebugEvent then
    fOndebugEvent(Self, 'DeleteShortCut',
      Format('DeleteShortCut called. Shortcut name=%s', [fShortCutClass.ShortcutName]));
  if fShortCutClass.ShortcutName = '' then
  begin
    if fFireDebugEvent then
      fOndebugEvent(Self, 'DeleteShortCut', 'ShortCut.ShortCutName was empty!');
    Exit;
  end;
  Result := DeleteDesktopShortcut(fShortCutClass.ShortCutName);

  if fFireDebugEvent then
    if Result = True then
      fOndebugEvent(Self, 'MakeShortCut', 'DeleteShortCut succeded.' +
        GetShortCutDebugString)
    else
      fOndebugEvent(Self, 'MakeShortCut', 'DeleteShortCut failed.  Error: ' +
        GetShortCutDebugString);

end;

procedure TLazAutoUpdate.ShowWhatsNewIfAvailable;
begin
  // Should be called on form.activate
  // Afer an update, the 'whatsnew.txt' is copied into the application's folder
  // This routine shows it, then deletes it
  // If it isn't there, then it exits early

  // read the VMT once
  if Assigned(fOndebugEvent) then
    fFireDebugEvent := True;

  if not FileExistsUTF8(ProgramDirectory + C_WhatsNewFilename) then
  begin
    if fFireDebugEvent then
      fOndebugEvent(Self, 'ShowWhatsNewIfAvailable', 'Unable to locate ' +
        C_WhatsNewFilename);
    Exit;
  end;
  // Linux fix
  if DirectoryExistsUTF8(C_WhatsNewFilename) then
  begin
    if fFireDebugEvent then
      fOndebugEvent(Self, 'ShowWhatsNewIfAvailable', 'Found directory ' +
        C_WhatsNewFilename);
    if RemoveDirUTF8(C_WhatsNewFilename) then
    begin
      if fFireDebugEvent then
        fOndebugEvent(Self, 'ShowWhatsNewIfAvailable', 'Deleted directory ' +
          C_WhatsNewFilename);
    end;
    Exit;
  end;

  // Create the form, memo and close button
  if fParentForm <> nil then
    WhatsNewForm := TForm.CreateNew(fParentForm)
  else
    WhatsNewForm := TForm.CreateNew(fParentApplication);

  WhatsNewMemo := TMemo.Create(WhatsNewForm);
  cmdClose := TBitBtn.Create(WhatsNewForm);

  try // ..finally destroy all
    with WhatsNewForm do
    begin
      Height := 480;
      Width := 640;
      // BorderStyle:=bsToolWindow;
      Caption := Format(C_WhatsNewInVersion, [GetFileVersion]);
      DefaultMonitor := dmActiveForm;
      // FormStyle:=fsStayOnTop;
      Position := poScreenCenter;
      ShowInTaskBar := stNever;
    end;
    with WhatsNewMemo do
    begin
      Height := WhatsNewForm.Height - 80;
      Width := WhatsNewForm.ClientWidth;
      ReadOnly := True;
      ScrollBars := ssAutoBoth;
      WordWrap := True;
      Parent := WhatsNewForm;
      try
        Lines.LoadFromFile(ProgramDirectory + C_WhatsNewFilename);
      except
        Clear;
        Lines.Add('Unable to show whats new');
      end;
    end;
    with cmdClose do
    begin
      Top := WhatsNewForm.Height - Height - 20;
      Left := (WhatsNewForm.Width div 2) - (Width div 2);
      Kind := bkClose;
      Parent := WhatsNewForm;
    end;
    // Show the window modally (cmdClose will close it)
    WhatsNewForm.ShowModal;
    try
      // Delete the whatsnew.txt now the user has seen it
      if not SysUtils.DeleteFile(ProgramDirectory + C_WhatsNewFilename) then
        if fShowDialogs then
          ShowMessageFmt(C_UnableToDelete,
            [C_WhatsNewFilename, LineEnding]);
    except
      // Ignore Exceptions
    end;
  finally
  {
  cmdClose.Free; // Not needed
  WhatsNewMemo.Free; // Not needed
  }
    FreeAndNil(WhatsNewForm); // Free the form and its minions
  end;
end;

function TLazAutoUpdate.SilentUpdate: boolean;
  // Part of the tray update system
begin
  // read the VMT once
  if Assigned(fOndebugEvent) then
    fFireDebugEvent := True;

  Result := False;
  fSilentMode := True;
  fShowUpdateInCaption := False;
  fShowDialogs := False;
  if fFireDebugEvent then
    fOndebugEvent(Self, 'SilentUpdate', 'Calling UpdateToNewVersion');

  // Use the local lauupdate if available
  if FileExistsUTF8(ProgramDirectory + C_LAUUPDATENAME) then
  begin
    if RemoteUpdateToNewVersion then
      // If IsAppRunning=FALSE, then calls DoSilentUpdate
    begin
      if fFireDebugEvent then
        fOndebugEvent(Self, 'SilentUpdate', 'UpdateToNewVersion succeeded');
      fSilentMode := False;
      Result := True;
    end;

  end
  else
  begin
    if UpdateToNewVersion then
      // If IsAppRunning=FALSE, then calls DoSilentUpdate
    begin
      if fFireDebugEvent then
        fOndebugEvent(Self, 'SilentUpdate', 'UpdateToNewVersion succeeded');
      fSilentMode := False;
      Result := True;
    end;
  end;
end;

Function TLazAutoUpdate.AutoUpdate:Boolean;
// Do-all proc that user can drop into a menu
begin
  if Assigned(fOndebugEvent) then
    fFireDebugEvent := True;
  Result:=False;
  if fFireDebugEvent then
    fOndebugEvent(Self, 'AutoUpdate', 'Calling NewVersionAvailable');
  if NewVersionAvailable then
  begin
    if fFireDebugEvent then
      fOndebugEvent(Self, 'AutoUpdate', 'NewVersionAvailable succeeded');

    if MessageDlg(fParentApplication.Title, Format(rsANewVersionS,
      [fGUIOnlineVersion]), mtConfirmation, [mbYes, mbNo], 0, mbYes) = 6 then
    begin
      if fFireDebugEvent then
        fOndebugEvent(Self, 'AutoUpdate', 'Calling DownloadNewVersion');

      if DownloadNewVersion then
      begin
        if fFireDebugEvent then
          fOndebugEvent(Self, 'AutoUpdate', 'DownloadNewVersion suceeded');
        if MessageDlg(fParentApplication.Title,
          Format(rsVewVersionSH, [fGUIOnlineVersion]), mtConfirmation,
          [mbOK, mbCancel], 0, mbOK) = 1 then
        begin
          if fFireDebugEvent then
            fOndebugEvent(Self, 'AutoUpdate', 'Calling UpdateToNewVersion');
          UpdateToNewVersion;
        end
        else
          MessageDlg(fParentApplication.Title,
            rsCancelledYou,
            mtInformation, [mbOK], 0);
      end
      else
        MessageDlg(fParentApplication.Title,
          Format(rsDownloadFail, [GetThreadDownloadReturnCode]),
          mtInformation, [mbOK], 0);
    end
    else
      MessageDlg(fParentApplication.Title,
        rsCancelledYou2,
        mtInformation, [mbOK], 0);
  end
  else
    begin
    MessageDlg(fParentApplication.Title,
      rsThisApplicat,
      mtInformation, [mbOK], 0);
    Result:=TRUE;
    end;
end;

function TLazAutoUpdate.IsOnlineVersionNewer(const sznewINIPath: string): boolean;
  // Compares version contained in szTempXMLPath INI file
  // to fApplicationVersionNumber
var
  VersionINI: TIniFile;
{
C_INISection = 'versions';
C_GUIEntry ='GUI';
C_ModuleEntry = 'Module';
}
begin
  // read the VMT once
  if Assigned(fOndebugEvent) then
    fFireDebugEvent := True;

  Result := False;
  VersionINI := TIniFile.Create(sznewINIPath);
  try
    fGUIOnlineVersion := VersionINI.ReadString(C_INISection, C_GUIEntry, '0.0.0.0');
    if not TryStrToVersionQuad(fGUIOnlineVersion, fGuiQuad) then
      fGUIQuad := StrToVersionQuad('0.0.0.0');
  finally
    VersionINI.Free;
  end;
  if fFireDebugEvent then
    fOndebugEvent(Self, 'IsOnlineVersionNewer',
      Format('fGUIOnlineVersion=%s, fApplicationVersionString=%s, szTempXMLPath=%s',
      [fGUIOnlineVersion, fApplicationVersionString, sznewINIPath]));

  // Fetch the 4 (or less) version elements and make into an Integer
  // so 1.10 > 1.9.9.9
  // iGUIVersion := VersionStringToNumber(fGUIOnlineVersion);
  // Test: Is the online version newer?
  if NewerVersion(fGUIQuad, fApplicationVersionQuad) then
    Result := True;
end;


function TLazAutoUpdate.NewVersionAvailable: boolean;
  // Returns TRUE is a new version is available
var
  szURL, szTargetPath: string;
  cCount: cardinal;
  szOldCaption: string;
begin
  Result := False;
  // read the VMT once
  if Assigned(fOndebugEvent) then
    fFireDebugEvent := True;

  if fZipFileName = '' then
  begin
    fZipfileName := ChangeFileExt(ExtractFilename(fAppFilename), '.zip');
    if fFireDebugEvent then
      fOndebugEvent(Self, 'NewVersionAvailable',
        Format('Assigning ZipFile name %s', [fZipfileName]));
  end;

  if fProjectType = auSourceForge then
  begin
    if fSourceForgeProjectName = '' then
    begin
      if fShowDialogs then
        ShowMessage(C_PropIsEmpty);
      if fFireDebugEvent then
        fOndebugEvent(Self, 'NewVersionAvailable (auSourceForge)', C_PropIsEmpty);
      Exit;
    end;
    szURL := Format(C_SOURCEFORGEURL, [fSourceForgeProjectName,
      fUpdatesFolder, fVersionsININame]);
  end;

  if fProjectType = auGitHubReleaseZip then
  begin
    if ((fGitHubProjectName = '') or (fGitHubRepositoryName = '')) then
    begin
      if fShowDialogs then
        ShowMessage(C_PropIsEmpty);
      if fFireDebugEvent then
        fOndebugEvent(Self, 'NewVersionAvailable (auGitHubReleaseZip)', C_PropIsEmpty);
      Exit;
    end;
    if ((fUpdatesFolder = C_NotApplicable) or (fUpdatesFolder = '')) then
      szURL := Format(C_GITHUBFILE_URL, [fGitHubProjectName,
        fGitHubRepositoryName, fGitHubBranchOrTag, fVersionsININame])
    else
      szURL := Format(C_GITHUBFILE_URL_UPDATES,
        [fGitHubProjectName, fGitHubRepositoryName, fGitHubBranchOrTag,
        fUpdatesFolder, fVersionsININame]);
  end;

  if fProjectType = auOther then
    // fauOtherSourceURL ends with '/'
  begin
    szURL := fauOtherSourceURL + fVersionsININame;
  end;


  szTargetPath := AppendPathDelim(ExtractFilePath(fAppFilename)) +
    Format(C_TempVersionsININame, [fVersionsININame]);
  if fFireDebugEvent then
    fOndebugEvent(Self, 'NewVersionAvailable',
      Format('URL is %s', [szURL]));

  if fFireDebugEvent then
    fOndebugEvent(Self, 'NewVersionAvailable',
      Format('Target Path %s', [szTargetPath]));


  // Delete any old versions
  try
    if FileExistsUTF8(szTargetPath) then
    begin
      SysUtils.DeleteFile(szTargetPath);
      if fFireDebugEvent then
        fOndebugEvent(Self, 'NewVersionAvailable',
          Format('Deleted old file %s', [szTargetPath]));
    end;
  except
    if fFireDebugEvent then
      fOndebugEvent(Self, 'NewVersionAvailable',
        Format('Failed to delete old file %s', [szTargetPath]));
    // No error if the delete can't be done
  end;
  with fThreadDownload do
  begin
    URL := szURL;
    Filename := szTargetPath;
    if not fSilentMode then
      szOldCaption := fParentForm.Caption;
    // Initialise fields
    ThreadFinished := False;
    ReturnCode := 0;
    DownloadSize := 0;
    fDownloadInprogress := True;
    DebugMode := fDebugMode;
    if not fSilentMode then
      fParentForm.Caption := C_Checking;
    CheckForOpenSSL;
    // Start the thread
    ThreadDownloadHTTP;
    if fFireDebugEvent then
      fOndebugEvent(Self, 'NewVersionAvailable',
        Format('ThreadDownloadHTTP return Code was %d', [fReturnCode]));
    if fFireDebugEvent then
      fOndebugEvent(Self, 'NewVersionAvailable',
        Format('ThreadDownloadHTTP Last Error was %s', [fLastError]));

    cCount := 0;
    // Update the GUI during the thread
    try
      while (ThreadFinished = False) do
      begin
        Inc(cCount);
        Sleep(1);
        fParentApplication.ProcessMessages;
        ThreadSwitch();
        {$IFDEF WINDOWS}
        if fShowUpdateInCaption then
          fParentForm.Caption := Format(C_Checking + ' %d', [cCount])
        else
          Sleep(10);
        {$ENDIF}
        fParentApplication.ProcessMessages;
        if (cCount > fVersionCountLimit) then
        begin
          if fShowDialogs then
            ShowMessage(C_TakingTooLong);
          ThreadFinished := True;
          fDownloadSize := 0;
          fDownloadInprogress := False;
          if not fSilentMode then
            fParentForm.Caption := szOldCaption;
          Exit;
        end;
      end;
    except
      ThreadFinished := True;
      fDownloadSize := 0;
      fDownloadInprogress := False;
      if not fSilentMode then
        fParentForm.Caption := szOldCaption;
      if fFireDebugEvent then
        fOndebugEvent(Self, 'NewVersionAvailable',
          C_ThreadDownloadCrash);
      Exit;
    end;
    if fFireDebugEvent then
      fOndebugEvent(Self, 'NewVersionAvailable',
        Format('After Threadfinished: Return Code was %d', [fReturnCode]));
    Sleep(1);
    fDownloadInprogress := False;
    if fDownloadSize > 0 then
    begin
      if fFireDebugEvent then
        fOndebugEvent(Self, 'NewVersionAvailable',
          Format('Downloaded %s OK', [szTargetPath]));
      fParentApplication.ProcessMessages;
      Result := IsOnlineVersionNewer(szTargetPath);
      if fFireDebugEvent then
        fOndebugEvent(Self, 'NewVersionAvailable',
          Format(C_DownloadedBytes, [szTargetPath, fDownloadSize]));
    end
    else
    if fFireDebugEvent then
      fOndebugEvent(Self, 'NewVersionAvailable',
        Format('DownloadSize was %d', [fDownloadSize]));

  end;
  if not fSilentMode then
    fParentForm.Caption := szOldCaption;
  if Assigned(fOnNewVersionAvailable) then
    fOnNewVersionAvailable(Self, Result, fGUIOnlineVersion);
end;

function TLazAutoUpdate.DownloadNewVersion: boolean;
  // Returns TRUE is download succeeded
  // If FALSE. then examine ReturnCode property
var
  szURL, szTargetPath, szUpdatesFolder: string;
  cCount: cardinal;
  szOldCaption: string;
  iDownloadedSize: integer;
  FileStringList: TStringList;
  iCount: integer;
begin
  Result := False;
  // read the VMT once
  if Assigned(fOndebugEvent) then
    fFireDebugEvent := True;
  iDownloadedSize := 0;
  if fZipFileName = '' then
  begin
    fZipfileName := ChangeFileExt(ExtractFilename(fAppFilename), '.zip');
    if fFireDebugEvent then
      fOndebugEvent(Self, 'DownloadNewVersion',
        Format('ZipFilename was empty.  Assigned %s', [fZipfileName]));
  end;
  szTargetPath := fZipfileName;
  if fProjectType = auSourceForge then
    szURL := Format(C_SOURCEFORGEURL, [fSourceForgeProjectName,
      fUpdatesFolder, ExtractFileName(szTargetPath)]);
  if fProjectType = auGitHubReleaseZip then
    if ((fUpdatesFolder = C_NotApplicable) or (fUpdatesFolder = '')) then
      szURL := Format(C_GITHUBFILE_URL, [fGitHubProjectName,
        fGitHubRepositoryName, fGitHubBranchOrTag, fZipfileName])
    else
      szURL := Format(C_GITHUBFILE_URL_UPDATES,
        [fGitHubProjectName, fGitHubRepositoryName, fGitHubBranchOrTag,
        fUpdatesFolder, fZipfileName]);
  if fProjectType = auOther then
    // fauOtherSourceURL ends with '/'
  begin
    szURL := fauOtherSourceURL + fVersionsININame;
  end;

  szUpdatesFolder := AppendPathDelim(ExtractFilePath(fAppFilename)) + fUpdatesFolder;
  if fFireDebugEvent then
    fOndebugEvent(Self, 'DownloadNewVersion',
      Format('Download parameters: TargetPath=%s%sURL=%s%sUpdatesFolder=%s',
      [szTargetPath, LineEnding, szURL, LineEnding, szUpdatesFolder]));


  // If updates folder exists, delete previous contents
  // If not, then create updates folder
  try
    if DirPathExists(szUpdatesFolder) then
    begin
      if fFireDebugEvent then
        fOndebugEvent(Self, 'DownloadNewVersion',
          Format('Files already exist in %s.  They will be deleted.',
          [szUpdatesFolder]));

      FileStringList := TStringList.Create;
      try
        FileStringList := FindAllFiles(szUpdatesFolder, '*.*', True);
        try
          for iCount := 0 to FileStringList.Count - 1 do
            SysUtils.DeleteFile(FileStringList[iCount]);
        except
          fLastError := Format(C_UnableToDeleteOld, [szUpdatesFolder]);
          if fDebugMode then
            ShowMessage(fLastError);
        end;
      finally
        FileStringList.Free;
      end;
    end
    else
    begin
      if ForceDirectory(szUpdatesFolder) then
        if fFireDebugEvent then
          fOndebugEvent(Self, 'DownloadNewVersion',
            Format('New directory %s was created', [szUpdatesFolder]));
    end;
  except
    if fFireDebugEvent then
      fOndebugEvent(Self, 'DownloadNewVersion',
        Format(C_DirectoryProblems, [szUpdatesFolder]));
    raise Exception.CreateFmt(C_DirectoryProblems, [szUpdatesFolder]);
  end;

  // Set target to the updates folder
  szTargetPath := AppendPathDelim(szUpdatesFolder) + ExtractFileName(szTargetPath);

  if fProjectType = auOther then
    // fauOtherSourceURL ends with PathDelimiter
  begin
    szURL := fauOtherSourceURL + fauOtherSourceFilename;
    szTargetPath := szUpdatesFolder + PathDelim + fauOtherSourceFilename;
  end;

  if fFireDebugEvent then
    fOndebugEvent(Self, 'DownloadNewVersion',
      Format('szURL=%s, szTargetPath=%s', [szURL, szTargetPath]));

  // Double-check: Delete any earlier updates?
  try
    if FileExistsUTF8(szTargetPath) then
    begin
      SysUtils.DeleteFile(szTargetPath);
      if fFireDebugEvent then
        fOndebugEvent(Self, 'DownloadNewVersion',
          Format('Deleting old file %s', [szTargetPath]));
    end;
  except
    // Ignore exceptions
  end;

  fDownloadInprogress := True;
  CheckForOpenSSL;
  // Do the download
  with fThreadDownload do
  begin
    // Initialise fields
    URL := szURL;
    Filename := szTargetPath;
    ThreadFinished := False;
    ReturnCode := 0;
    DownloadSize := 0;
    fUnzipAfter := True;
    DebugMode := fDebugMode;
    if not fSilentMode then
      szOldCaption := fParentForm.Caption;
    if not fSilentMode then
      fParentForm.Caption := C_Downloading;
    // Start the thread
    ThreadDownloadHTTP;
    cCount := 0;
    begin
      // The thread is running....
      cCount := 0;
      try
        while (ThreadFinished = False) do
        begin
          Inc(cCount);
          Sleep(1);
          fParentApplication.ProcessMessages;
        {$IFDEF WINDOWS}
          if fShowUpdateInCaption then
            fParentForm.Caption := Format(C_Downloading + ' %d', [cCount])
          else
            sleep(10);
          fParentApplication.ProcessMessages; // Keep GUI responsive

          if (cCount > fDownloadCountLimit) then // Download taking too long?
          begin
            fDownloadInprogress := False;
            if not fSilentMode then
              fParentForm.Caption := szOldCaption;
            if fFireDebugEvent then
              fOndebugEvent(Self, 'DownloadNewVersion', C_TakingTooLong);
            ThreadFinished := True;
            fDownloadSize := 0;
            Exit;
          end;
        {$ENDIF}
        end;
        iDownloadedSize := fDownloadSize;
      except
        if fFireDebugEvent then
          fOndebugEvent(Self, 'DownloadNewVersion', C_ThreadDownloadHTTPCrash);
      end;
      fDownloadInprogress := False;
      Sleep(1);
      // We have the HTTP return code in MyThreadDownload.ReturnCode.  Normal=200
      if (ReturnCode <> 0) and (ReturnCode < 400) then
      begin
        // Success! New version is unzipped and ready in the /updates folder
        // Shell into the updater app here
        // 1) Closes this app
        // 2) Copies the szUpdateFolder/downloadedexe to the .exe
        // 3) Restarts this (updated) app (showing C_WhatsNewFilename?)
        Result := (fDownloadSize > 0);
        if fFireDebugEvent then
        begin
          fOndebugEvent(Self, 'DownloadNewVersion',
            Format(C_DownloadedBytes, [ExtractFilename(szTargetPath),
            fDownloadSize]));
          fOndebugEvent(Self, 'DownloadNewVersion',
            Format(C_DownloadSuccess, [fGUIOnlineVersion]));
        end;
      end
      else
      begin
        Result := False;
        if fFireDebugEvent then
          fOndebugEvent(Self, 'DownloadNewVersion',
            Format(C_UnableToDownload, [LineEnding, ReturnCode]));
        Free;
      end;
    end;
  end;
  // Fire the event?
  if not fSilentMode then
    fParentForm.Caption := C_PleaseWaitProcessing;
  if Assigned(fOnDownloaded) then
    fOnDownloaded(Self, ReturnCode, iDownloadedSize);
  if not fSilentMode then
    fParentForm.Caption := szOldCaption;
end;

function UTF8StringReplace(const S, OldPattern, NewPattern: string;
  Flags: TReplaceFlags): string;
var
  uS, uOld: string;
  // Warning! Always changes string to UPPERCASE
begin
  if rfIgnoreCase in Flags then
  begin
    uS := LazUTF8.UTF8UpperCase(S);
    uOld := LazUTF8.UTF8UpperCase(OldPattern);
    Flags := Flags - [rfIgnoreCase]; //no point uppercasing again
    Result := StringReplace(uS, uOld, NewPattern, Flags);
  end
  else
    Result := StringReplace(S, OldPattern, NewPattern, Flags);
end;

function TLazAutoUpdate.CreateLocalLauImportFile: boolean;
  // Used in SysTray app
var
  LAUTRayINI: TIniFile;
  szSection: string;
  szSuffix: string;
begin
  // read the VMT once
  if Assigned(fOndebugEvent) then
    fFireDebugEvent := True;
  if fFireDebugEvent then
    fOndebugEvent(Self, 'CreateLocalLauImportFile', 'CreateLocalLauImportFile called');
  if FileExistsUTF8(ProgramDirectory + C_LAUTRayINI) then
  begin
    RelocateLauImportFile;
    Result := True;
    Exit;
  end;
  // Make up OS-Bitness suffix
  {$IFDEF WINDOWS}
  szSuffix := 'win';
  {$ELSE}
  szSuffix := 'linux';
  {$ENDIF}
  {$IFDEF CPU64}
  szSuffix += '64';
  {$ELSE}
  szSuffix += '32';
  {$ENDIF}
  Result := False;
  LAUTRayINI := TIniFile.Create(GetAppConfigDirUTF8(False, True) + C_LAUTRayINI);
  try
    with LAUTRayINI do
    begin
      if fParentApplication <> nil then
        szSection := fParentApplication.Title
      else
      if fParentForm <> nil then
        szSection := fParentForm.Caption
      else
        szSection := 'My Application';
      if ((AnsiContainsText(szSection,
{$I %FPCTARGETOS%}
        ) = False) and (AnsiContainsText(szSection, szSuffix) = False)) then
        szSection += szSuffix;
      WriteString(szSection, 'AppPrettyName', szSection);
      WriteString(szSection, 'AppPath', ExtractFilename(fAppFilename));
      WriteString(szSection, 'INIPath', fVersionsININame);
      WriteString(szSection, 'ZipPath', fZipfileName);
      WriteString(szSection, 'AppVersion', fApplicationVersionString);
      WriteString(szSection, 'SFProjectName', fSourceForgeProjectName);
      WriteString(szSection, 'SFUpdatesDirectory', fUpdatesFolder);
      WriteString(szSection, 'Location', ExtractFilePath(fAppFilename));
      //Suggest a schedule
      WriteInteger(szSection, 'IntervalType', 0);
      WriteInteger(szSection, 'IntervalDay', 0);
      WriteInteger(szSection, 'IntervalDate', 1);
      WriteInteger(szSection, 'IntervalHour', 9);
      WriteInteger(szSection, 'Update', 0);
      WriteString(szSection, 'LastCheckDateTime', '2000-01-01 00-00');
      UpdateFile;
      Result := True;
    end;
  finally
    FreeAndNil(LAUTRayINI);
  end;
end;

procedure TLazAutoUpdate.RelocateLauImportFile;
// If C_LAUTRayINI is found in the App Folder, it is moved to the <LocalAppData>/updatehm folder
var
  szSourceLAUTrayPath, szDestLAUTrayPath, szDestLAUTrayDirectory: string;
begin
  // read the VMT once
  if Assigned(fOndebugEvent) then
    fFireDebugEvent := True;
  szSourceLAUTrayPath := ExtractFilePath(fAppFilename) + C_LAUTRayINI;
  if fFireDebugEvent then
    fOndebugEvent(Self, 'RelocateLauImportFile',
      Format('Looking for %s.', [szSourceLAUTrayPath]));

  if FileExistsUTF8(szSourceLAUTrayPath) then
  begin
    if fFireDebugEvent then
      fOndebugEvent(Self, 'RelocateLauImportFile',
        Format('Found %s.', [szSourceLAUTrayPath]));
    // Make up local <AppData>/updatehm/C_LAUTRayINI path
    szDestLAUTrayPath := GetAppConfigDirUTF8(False, False); // Don't create it yet
    {$IFDEF WINDOWS}
    szDestLAUTrayPath := StringReplace(szDestLAUTrayPath, Application.Title,
      'updatehm' + C_PFX, [rfReplaceAll]);
    {$ELSE}
    szDestLAUTrayPath := UTF8StringReplace(szDestLAUTrayPath,
      Application.Title, 'updatehm' + C_PFX, [rfReplaceAll]);
    {$ENDIF}
  end
  else
    Exit; // Nothing to do
  // szDestLAUTrayPath := LowerCase(szDestLAUTrayPath);
  szDestLAUTrayDirectory := ExtractFilePath(szDestLAUTrayPath);

  if DirectoryExistsUTF8(szDestLAUTrayDirectory) then
  begin
    if fFireDebugEvent then
      fOndebugEvent(Self, 'RelocateLauImportFile',
        Format('%s already exists.', [szDestLAUTrayDirectory]));
  end
  else
  begin
    if fFireDebugEvent then
      fOndebugEvent(Self, 'RelocateLauImportFile',
        Format('%s does not previously exist.', [szDestLAUTrayDirectory]));
    if ForceDirectory(szDestLAUTrayDirectory) then
    begin
      if fFireDebugEvent then
        fOndebugEvent(Self, 'RelocateLauImportFile',
          Format('Created folder %s.', [szDestLAUTrayDirectory]));
    end
    else
    if fFireDebugEvent then
      fOndebugEvent(Self, 'RelocateLauImportFile',
        Format('Unable to create folder %s.', [szDestLAUTrayDirectory]));
  end;

  // Don't copy over an existing file
  if not FileExistsUTF8(szDestLAUTrayPath + C_LAUTRayINI) then
  begin
    // Move C_LAUTRayINI from app folder to local <AppData> folder
    if FileUtil.CopyFile(szSourceLAUTrayPath, szDestLAUTrayPath +
      C_LAUTRayINI, [cffOverwriteFile]) then
    begin
      if fFireDebugEvent then
        fOndebugEvent(Self, 'RelocateLauImportFile',
          Format('Relocated %s from %s to %s',
          [C_LAUTRayINI, szSourceLAUTrayPath, szDestLAUTrayPath]));
      SysUtils.DeleteFile(szSourceLAUTrayPath);
    end
    else
    if fFireDebugEvent then
      fOndebugEvent(Self, 'RelocateLauImportFile',
        Format('Failed to relocate %s from %s to %s',
        [C_LAUTRayINI, szSourceLAUTrayPath, szDestLAUTrayPath]));
  end;
end;

function TLazAutoUpdate.DoSilentUpdate: boolean;
  // Used in Systray app
  // Called from UpdateToNewVersion when the app is not running
  // Updates the app, and also copies over and updates C_LAUTRayINI
var
  szAppFolder: string;
  szLAUTrayAppPath: string;
  INI: TINIFile;
  SectionStringList: TStrings;
  szTempUpdatesFolder: string;
  ErrMsg:String;
begin
  // fWorkingMode=lauInstall or lauUpdate
  Result := False;
  // read the VMT once
  if Assigned(fOndebugEvent) then
    fFireDebugEvent := True;
  if fFireDebugEvent then
    fOndebugEvent(Self, 'DoSilentUpdate', 'Starting DoSilentUpdate');

  if fFireDebugEvent then
    if fWorkingMode = lauUpdate then
      fOndebugEvent(Self, 'DoSilentUpdate', 'Update mode')
    else
      fOndebugEvent(Self, 'DoSilentUpdate', 'Install mode');

  if fWorkingMode = lauUpdate then
  begin
    if not FileExistsUTF8(fAppFilename) then
    begin
      if fFireDebugEvent then
        fOndebugEvent(Self, 'DoSilentUpdate',
          Format('AppFilename %s is missing.  Exiting routine', [fAppFilename]));
      Exit;
    end;
  end;

  // uses fUpdatesFolder
  szTempUpdatesFolder := AppendPathDelim(ExtractFilePath(fAppFilename) + fUpdatesFolder);

  if not DirectoryExistsUTF8(szTempUpdatesFolder) then
  begin
    if fFireDebugEvent then
      fOndebugEvent(Self, 'DoSilentUpdate',
        Format('Updates folder %s was missing.', [szTempUpdatesFolder]));
    if ForceDirectory(szTempUpdatesFolder) then
      if fFireDebugEvent then
        fOndebugEvent(Self, 'DoSilentUpdate',
          Format('Created folder %s.', [szTempUpdatesFolder]));
  end;

  szAppFolder := AppendPathDelim(ExtractFilePath(fAppFilename));
  // Copy over everything from the updates folder
  if fFireDebugEvent then
    fOndebugEvent(Self, 'DoSilentUpdate',
      Format('About to copy from %s to %s', [szTempUpdatesFolder, szAppFolder]));

  if fCopyTree then
  begin
    if CopyDirTree(szTempUpdatesFolder, szAppFolder,
      [cffOverwriteFile, cffCreateDestDirectory]) then
    begin
      if fFireDebugEvent then
        fOndebugEvent(Self, 'DoSilentUpdate',
          Format('CopyTree successful from %s to %s',
          [szTempUpdatesFolder, szAppFolder]));
    end
    else
    if fFireDebugEvent then
      fOndebugEvent(Self, 'DoSilentUpdate',
        Format('Error: CopyTree unsuccessful from %s to %s',
        [szTempUpdatesFolder, szAppFolder]));
  end
  else
  begin
    // Copy over app
    if FileUtil.CopyFile(szTempUpdatesFolder + ExtractFileName(fAppFilename),
      szAppFolder + ExtractFileName(fAppFilename), [cffOverwriteFile]) then
    begin
      if fFireDebugEvent then
        fOndebugEvent(Self, 'DoSilentUpdate',
          Format('Copied app from %s to %s', [szTempUpdatesFolder +
          ExtractFileName(fAppFilename), szAppFolder + ExtractFileName(fAppFilename)]));

      {$IFDEF LINUX}
      if not SetExecutePermission(szAppFolder + ExtractFileName(fAppFilename),
        ErrMsg) then
      begin
        if fFireDebugEvent then
          fOndebugEvent(Self, 'DoSilentUpdate',
            Format('Unable to set permissions for %s because of %s',
            [szAppFolder + ExtractFileName(fAppFilename), ErrMsg]));
        if fShowDialogs then
          ShowMessageFmt('Unable to set permissions for %s because of %s',
            [szAppFolder + ExtractFileName(fAppFilename), ErrMsg]);
      end;
      {$ENDIF}
    end
    else
    if fFireDebugEvent then
      fOndebugEvent(Self, 'DoSilentUpdate',
        Format('Failed to copy app from %s to %s',
        [szTempUpdatesFolder + ExtractFileName(fAppFilename),
        szAppFolder + ExtractFileName(fAppFilename)]));

    // Copy over WhatsNew
    if FileUtil.CopyFile(szTempUpdatesFolder + 'whatsnew.txt',
      szAppFolder + 'whatsnew.txt', [cffOverwriteFile]) then
    begin
      if fFireDebugEvent then
        fOndebugEvent(Self, 'DoSilentUpdate',
          Format('Copied whatsnew.txt from %s to %s',
          [szTempUpdatesFolder + 'whatsnew.txt', szAppFolder + 'whatsnew.txt']));
    end
    else
    if fFireDebugEvent then
      fOndebugEvent(Self, 'DoSilentUpdate',
        Format('Failed to copy whatsnew.txt from %s to %s',
        [szTempUpdatesFolder + 'whatsnew.txt', szAppFolder + 'whatsnew.txt']));

  end;

  if (fWorkingMode = lauInstall) then
    if FileExistsUTF8(C_UPDATEHMNAME) then
    begin
      if FileUtil.CopyFile(C_UPDATEHMNAME, szAppFolder + C_UPDATEHMNAME) then
      begin
        if fFireDebugEvent then
          fOndebugEvent(Self, 'DoSilentUpdate',
            Format('Sucessfully copied %s to %s',
            [C_UPDATEHMNAME, szAppFolder]));
        {$IFDEF LINUX}
        if not SetExecutePermission(szAppFolder + C_UPDATEHMNAME, ErrMsg) then
        begin
          if fFireDebugEvent then
            fOndebugEvent(Self, 'DoSilentUpdate',
              Format('Unable to set permissions for %s because of %s',
              [szAppFolder + C_UPDATEHMNAME, ErrMsg]));
          if fShowDialogs then
            ShowMessageFmt('Unable to set permissions for %s because of %s',
              [szAppFolder + C_UPDATEHMNAME, ErrMsg]);
        end;
        {$ENDIF}
      end
      else
      if fFireDebugEvent then
        fOndebugEvent(Self, 'DoSilentUpdate',
          Format('Unabled to copy %s to %s', [C_UPDATEHMNAME, szAppFolder]));

    end
    else
    if fFireDebugEvent then
      fOndebugEvent(Self, 'DoSilentUpdate', 'Unable to locate ' + C_UPDATEHMNAME);



  // Deal with C_LAUTRayINI
  // Copied to the global application data folder
  // Add entry 'Location'
  if fFireDebugEvent then
    fOndebugEvent(Self, 'DoSilentUpdate', 'About to process ' +
      szTempUpdatesFolder + C_LAUTRayINI);

  if FileExistsUTF8(szTempUpdatesFolder + C_LAUTRayINI) then
  begin
    szLAUTrayAppPath := GetAppConfigDirUTF8(False, True); // Create it if necessary
    if fFireDebugEvent then
      fOndebugEvent(Self, 'DoSilentUpdate',
        Format('App data directory is %s', [szLAUTrayAppPath]));
    // AppDataDirectory/LazAutoUpdater/
    szLAUTrayAppPath := UTF8StringReplace(szLAUTrayAppPath, Application.Title,
      'updatehm' + C_PFX, [rfIgnoreCase, rfReplaceAll]);
    szLAUTrayAppPath := LowerCase(szLAUTrayAppPath);

    if fFireDebugEvent then
      fOndebugEvent(Self, 'DoSilentUpdate',
        Format('App data directory changed to %s', [szLAUTrayAppPath]));

    // Now AppDataDirectory/updatehm(+C_PFX)/
    try
      if ForceDirectory(szLAUTrayAppPath) then
        if fFireDebugEvent then
          fOndebugEvent(Self, 'DoSilentUpdate',
            Format('Made directory %s', [szLAUTrayAppPath]));

      if fFireDebugEvent then
        fOndebugEvent(Self, 'DoSilentUpdate',
          Format('About to copy %s to %s', [szTempUpdatesFolder +
          C_LAUTRayINI, szLAUTrayAppPath]));

      Fileutil.CopyFile(szTempUpdatesFolder + C_LAUTRayINI, szLAUTrayAppPath +
        C_LAUTRayINI, [cffOverWriteFile]);
      if fFireDebugEvent then
        fOndebugEvent(Self, 'DoSilentUpdate',
          Format('Successfully copied %s to %s ',
          [C_LAUTRayINI, szLAUTrayAppPath]));

      if FileExistsUTF8(szLAUTrayAppPath + C_LAUTRayINI) then
      begin
        INI := TINIFile.Create(szLAUTrayAppPath + C_LAUTRayINI);
        SectionStringList := TStringList.Create;
        try
          INI.ReadSections(SectionStringList);
          if SectionStringList.Count > 0 then
          begin
            INI.WriteString(SectionStringList[0], 'Location',
              ExtractFilePath(fAppFilename));
            if fFireDebugEvent then
              fOndebugEvent(Self, 'DoSilentUpdate',
                Format('Wrote new entry in section %s.  Location=%s',
                [SectionStringList[0], ExtractFilePath(fAppFilename)]));
          end
          else
          if fFireDebugEvent then
            fOndebugEvent(Self, 'DoSilentUpdate',
              'Failed to find a valid section in ' + C_LAUTRayINI);
        finally
          FreeAndNil(SectionStringList);
          FreeAndNil(INI);
        end;
        Result := True;
      end
      else
      if fFireDebugEvent then
        fOndebugEvent(Self, 'DoSilentUpdate',
          'Failed to copy ' + C_LAUTRayINI + ' to ' + szLAUTrayAppPath);
    except
      On E: Exception do
        if fFireDebugEvent then
          fOndebugEvent(Self, 'DoSilentUpdate',
            Format('Could not update %s.  Error: %s ', [C_LAUTRayINI, E.Message]));
    end;
  end;
  if fFireDebugEvent then
    fOndebugEvent(Self, 'DoSilentUpdate', 'Leaving DoSilentUpdate');

end;

function TLazAutoUpdate.RemoteUpdateToNewVersion: boolean;
  // Used in Systray app
  // Shells to 'lauupdate' console app in ProgramDirectory to remotely update an app
{$IFDEF WINDOWS}
  function RunAsAdmin(const Handle: THandle; const Path, Params: string): boolean;
  var
    sei: TShellExecuteInfoA;
  begin
    FillChar(sei, SizeOf(sei), 0);
    sei.cbSize := SizeOf(sei);
    sei.Wnd := Handle;
    sei.fMask := SEE_MASK_FLAG_DDEWAIT or SEE_MASK_FLAG_NO_UI;
    sei.lpVerb := 'runas';
    sei.lpFile := PAnsiChar(Path);
    sei.lpParameters := PAnsiChar(Params);
    sei.nShow := SW_SHOWNORMAL;
    Result := ShellExecuteExA(@sei);
  end;

{$ENDIF}

var
  cCount: cardinal;
  szAppDir, szParams: string;
begin
  Result := False;
  {$IFDEF WINDOWS}
  if fWindowsAdminCheck then
    if not IsWindowsAdmin then
    begin
      ShowAdminCheckMessage;
      Exit;
    end;
  {$ENDIF}
  // fWorkingMode=lauInstall or lauUpdate
  szAppDir := AppendPathDelim(ExtractFilePath(fAppFilename));

  // read the VMT once
  if Assigned(fOndebugEvent) then
    fFireDebugEvent := True;
  if fFireDebugEvent then
    fOndebugEvent(Self, 'RemoteUpdateToNewVersion',
      'Entering RemoteUpdateToNewVersion');

  // Running update using updatehm?
  if ((AppIsRunning(ExtractFileName(fAppFilename)) = False) and
    (ExtractFileName(fAppFilename) <> ExtractFileName(fparentApplication.EXEname))) then
    Result := DoSilentUpdate
  else
  begin
    cCount := 0;
    if not FileExistsUTF8(ProgramDirectory + C_LAUUPDATENAME) then
    begin
      if fShowDialogs then
        ShowMessageFmt(C_UpdaterMissing, [ProgramDirectory + C_LAUUPDATENAME]);
      if fFireDebugEvent then
        fOndebugEvent(Self, 'RemoteUpdateToNewVersion',
          Format(C_UpdaterMissing, [ProgramDirectory + C_LAUUPDATENAME]));
      Exit;
    end;

    if not DirectoryExistsUTF8(szAppDir + fUpdatesFolder) then
    begin
      if fShowDialogs then
        ShowMessageFmt(C_FolderMissing, [szAppDir + fUpdatesFolder]);
      if fFireDebugEvent then
        fOndebugEvent(Self, 'RemoteUpdateToNewVersion',
          Format(C_FolderMissing, [szAppDir + fUpdatesFolder]));
      Exit;
    end;


 {$IFDEF WINDOWS}
    szParams := ExtractFileName(fAppFilename);
    szParams := szParams + ' ' + fUpdatesFolder;
    szParams := szParams + ' ' + C_WhatsNewFilename;
    szParams := szParams + ' ' + fParentApplication.Title;
    if (fCopyTree = True) then
      szParams := szParams + ' copytree';
    if fFireDebugEvent then
      fOndebugEvent(Self, 'RemoteUpdateToNewVersion',
        Format('Executing %s', [ProgramDirectory + C_LAUUPDATENAME]));
    RunAsAdmin(fParentForm.Handle, ProgramDirectory + C_LAUUPDATENAME, szParams);

    // Check for C_WhatsNewFilename in the app directory in a LOOP
    if fFireDebugEvent then
      fOndebugEvent(Self, 'RemoteUpdateToNewVersion',
        Format('Waiting for %s', [szAppDir + C_WhatsNewFilename]));
    while not FileExistsUTF8(szAppDir + C_WhatsNewFilename) do
    begin
      fParentApplication.ProcessMessages;
      Inc(CCount);
      if cCount > 10000000 then
        Break; // Get out of jail in case updatehm.exe fails to copy file
    end;
{$ELSE}

    // Update and re-start the app
    FUpdateHMProcess := TAsyncProcess.Create(nil);
    FUpdateHMProcess.Executable := ProgramDirectory + C_LAUUPDATENAME;
    FUpdateHMProcess.CurrentDirectory := ProgramDirectory;
    if not fSilentMode then
      FUpdateHMProcess.ConsoleTitle :=
        Format(C_ConsoleTitle, [fParentApplication.Title]);
    FUpdateHMProcess.Parameters.Clear;
    FUpdateHMProcess.Parameters.Add(fAppFilename); //Param 1 = EXEname
    FUpdateHMProcess.Parameters.Add(fUpdatesFolder); // Param 2 = updates
    FUpdateHMProcess.Parameters.Add(C_WhatsNewFilename); // Param 3 = whatsnew.txt
    FUpdateHMProcess.Parameters.Add(fParentApplication.Title); // Param 4 = Prettyname
    if (fCopyTree = True) then
      FUpdateHMProcess.Parameters.Add('copytree');
    // Param 5 = Copy the whole of /updates to the App Folder
    if fFireDebugEvent then
      fOndebugEvent(Self, 'RemoteUpdateToNewVersion',
        Format('Executing %s', [ProgramDirectory + C_LAUUPDATENAME]));

    try
      FUpdateHMProcess.Execute;

      // Check for C_WhatsNewFilename in the app directory in a LOOP
      if fFireDebugEvent then
        fOndebugEvent(Self, 'RemoteUpdateToNewVersion',
          Format('Waiting for %s', [szAppDir + C_WhatsNewFilename]));
      while not FileExistsUTF8(szAppDir + C_WhatsNewFilename) do
      begin
        fParentApplication.ProcessMessages;
        Inc(CCount);
        if cCount > 10000000 then
          Break; // Get out of jail in case updatehm.exe fails to copy file
      end;
    finally
      FUpdateHMProcess.Free;
    end;
{$ENDIF}
    // remotely shut down the app?
    if fSilentMode then
    begin
      if AppIsRunning(ExtractFileName(fAppFilename)) then
        KillApp(ExtractFileName(fAppFilename));
      if fFireDebugEvent then
        fOndebugEvent(Self, 'RemoteUpdateToNewVersion',
          Format('Killing %s ready for update', [fAppFilename]));
    end;

    if not fSilentMode then
      fParentForm.Close;
    if fFireDebugEvent then
      fOndebugEvent(Self, 'RemoteUpdateToNewVersion',
        'Success');
    Result := True;
  end;
end;

{$IFDEF LINUX}
function TLazAutoUpdate.SetExecutePermission(const AFileName: string;
  var AErrMsg: string): boolean;
var
  SL: TStringList;
  Process: TProcess;
begin
  Result := False;
  Process := TProcess.Create(nil);
  try
    Process.Executable := '/bin/chmod';
    Process.Parameters.Add('+X');
    Process.Parameters.Add(AFileName);
    Process.Options := Process.Options + [poWaitOnExit, poUsePipes];
    Process.Execute;
    SL := TStringList.Create;
    try
      SL.LoadFromStream(Process.Stderr);
      AErrMsg := Trim(SL.Text);
      Result := Trim(AErrMsg) = '';
    finally
      SL.Free;
    end;
  finally
    Process.Free;
  end;
end;

{$ENDIF}
{
procedure CheckPermissions;
var
  ErrMsg: String;
begin
  if SetExecutePermission('/minesadorada/developer/updates/consoleupdater', ErrMsg) then
    MessageDlg('Permission successfully set.', mtInformation, [mbOk], 0)
  else
    MessageDlg('Cannot set permission. Error message: ' + ErrMsg, mtError, [mbOk], 0);
end;
}
function TLazAutoUpdate.UpdateToNewVersion: boolean;
  // Shells to updater console
  // Requires admin user in Win 10
{$IFDEF WINDOWS}
  function RunAsAdmin(const Handle: THandle; const Path, Params: string): boolean;
  var
    sei: TShellExecuteInfoA;
  begin
    FillChar(sei, SizeOf(sei), 0);
    sei.cbSize := SizeOf(sei);
    sei.Wnd := Handle;
    sei.fMask := SEE_MASK_FLAG_DDEWAIT or SEE_MASK_FLAG_NO_UI;
    sei.lpVerb := 'runas';
    sei.lpFile := PAnsiChar(Path);
    sei.lpParameters := PAnsiChar(Params);
    sei.nShow := SW_SHOWNORMAL;
    Result := ShellExecuteExA(@sei);
  end;

{$ENDIF}
var
  cCount: cardinal;
  szAppDir: string;
  szParams: string;
  {$IFDEF LINUX}
  ErrMsg: string;
{$ENDIF}
begin
  Result := False;
  {$IFDEF WINDOWS}
  if fWindowsAdminCheck then
    if not IsWindowsAdmin then
    begin
      ShowAdminCheckMessage;
      Exit;
    end;
  {$ENDIF}
  szAppDir := AppendPathDelim(ExtractFilePath(fAppFilename));

  // read the VMT once
  if Assigned(fOndebugEvent) then
    fFireDebugEvent := True;
  if fFireDebugEvent then
    fOndebugEvent(Self, 'UpdateToNewVersion',
      'Entering UpdateToNewVersion');

  // Running update using updatehm?
  if not AppIsRunning(ExtractFileName(fAppFilename)) then
  begin
    if fFireDebugEvent then
      fOndebugEvent(Self, 'UpdateToNewVersion', 'Doing SilentUpdate');
    Result := DoSilentUpdate;
  end
  else
  begin
    // Start Regular update
    cCount := 0;
    if not FileExistsUTF8(szAppDir + C_UPDATEHMNAME) then
    begin
      if fShowDialogs then
        ShowMessageFmt(C_UpdaterMissing, [szAppDir + C_UPDATEHMNAME]);
      if fFireDebugEvent then
        fOndebugEvent(Self, 'UpdateToNewVersion',
          Format(C_UpdaterMissing, [szAppDir + C_UPDATEHMNAME]));
      Exit;
    end;
    {$IFDEF LINUX}
    if not SetExecutePermission(szAppDir + C_UPDATEHMNAME, ErrMsg) then
    begin
      if fFireDebugEvent then
        fOndebugEvent(Self, 'UpdateToNewVersion',
          Format('Unable to set permissions for %s because of %s',
          [szAppDir + fUpdatesFolder, ErrMsg]));
      if fShowDialogs then
        ShowMessageFmt('Unable to set permissions for %s because of %s',
          [szAppDir + fUpdatesFolder, ErrMsg]);
      Result := False;
      Exit;
    end;
    {$ENDIF}

    if not DirectoryExistsUTF8(szAppDir + fUpdatesFolder) then
    begin
      if fShowDialogs then
        ShowMessageFmt(C_FolderMissing, [szAppDir + fUpdatesFolder]);
      if fFireDebugEvent then
        fOndebugEvent(Self, 'UpdateToNewVersion',
          Format(C_FolderMissing, [szAppDir + fUpdatesFolder]));
      Exit;
    end;


    // remotely shut down the app?
    if fSilentMode then
    begin
      if AppIsRunning(ExtractFileName(fAppFilename)) then
        KillApp(ExtractFileName(fAppFilename));
      if fFireDebugEvent then
        fOndebugEvent(Self, 'UpdateToNewVersion',
          Format('Killing %s ready for update', [fAppFilename]));
    end;
{$IFDEF WINDOWS}
    szParams := ExtractFileName(fAppFilename);
    szParams := szParams + ' ' + fUpdatesFolder;
    szParams := szParams + ' ' + C_WhatsNewFilename;
    szParams := szParams + ' ' + fParentApplication.Title;
    if (fCopyTree = True) then
      szParams := szParams + ' copytree';
    fOndebugEvent(Self, 'UpdateToNewVersion',
      Format('Executing %s', [szAppDir + C_UPDATEHMNAME]));
    if RunAsAdmin(fParentForm.Handle, szAppDir + C_UPDATEHMNAME, szParams) then
    begin
      if fFireDebugEvent then
        fOndebugEvent(Self, 'UpdateToNewVersion', 'RunAsAdmin succeeded');
    end
    else
    begin
      if fFireDebugEvent then
        fOndebugEvent(Self, 'UpdateToNewVersion', 'RunAsAdmin failed');
    end;
    // Check for C_WhatsNewFilename in the app directory in a LOOP
    if fFireDebugEvent then
      fOndebugEvent(Self, 'UpdateToNewVersion',
        Format('Waiting for %s', [szAppDir + C_WhatsNewFilename]));
    while not FileExistsUTF8(szAppDir + C_WhatsNewFilename) do
    begin
      fParentApplication.ProcessMessages;
      Inc(CCount);
      if cCount > 10000000 then
        Break; // Get out of jail in case updatehm.exe fails to copy file
    end;
{$ELSE}
    // Update and re-start the app
    FUpdateHMProcess := TAsyncProcess.Create(nil);
    try
      //      FUpdateHMProcess.Executable := AppendPathDelim(GetAppConfigDir(false)) + C_UPDATEHMNAME;
      FUpdateHMProcess.Executable := szAppDir + C_UPDATEHMNAME;
      //      FUpdateHMProcess.CurrentDirectory := AppendPathDelim(GetAppConfigDir(false));
      FUpdateHMProcess.CurrentDirectory := szAppDir;
      if not fSilentMode then
        FUpdateHMProcess.ConsoleTitle :=
          Format(C_ConsoleTitle, [fParentApplication.Title]);
      FUpdateHMProcess.Parameters.Clear;
      FUpdateHMProcess.Parameters.Add(ExtractFileName(fAppFilename)); //Param 1 = EXEname
      FUpdateHMProcess.Parameters.Add(fUpdatesFolder); // Param 2 = updates
      FUpdateHMProcess.Parameters.Add(C_WhatsNewFilename); // Param 3 = whatsnew.txt
      FUpdateHMProcess.Parameters.Add(fParentApplication.Title); // Param 4 = Prettyname
      if (fCopyTree = True) then
        FUpdateHMProcess.Parameters.Add('copytree');
      // Param 5 = Copy the whole of /updates to the App Folder
      if fFireDebugEvent then
        fOndebugEvent(Self, 'UpdateToNewVersion',
          Format('Executing %s', [szAppDir + C_UPDATEHMNAME]));
      try
        FUpdateHMProcess.Execute;
      except
        raise Exception.CreateFmt(
          'Error %d: Run this application in Administrator mode or turn off UAC',
          [GetLastOSError]);
      end;
      // Check for C_WhatsNewFilename in the app directory in a LOOP
      if fFireDebugEvent then
        fOndebugEvent(Self, 'UpdateToNewVersion',
          Format('Waiting for %s', [szAppDir + C_WhatsNewFilename]));
      while not FileExistsUTF8(szAppDir + C_WhatsNewFilename) do
      begin
        fParentApplication.ProcessMessages;
        Inc(CCount);
        if cCount > 100000 then
        begin
          // Fire the OnUpdated event
          if Assigned(fOnUpdated) then
          begin
            fOnUpdated(Self, fGUIOnlineVersion, 'Unsuccessful update');
            Application.ProcessMessages;
            Sleep(100);
          end;
          Break; // Get out of jail in case updatehm.exe fails to copy file
        end;
      end;
    finally
      FUpdateHMProcess.Free;
    end;
{$ENDIF}
    CreateLocalLauImportFile; // Creates a new import file in GetAppConfigDirUTF8

    // Fire the OnUpdated event
    if Assigned(fOnUpdated) then
    begin
      fOnUpdated(Self, fGUIOnlineVersion, 'Successful update');
      Application.ProcessMessages;
      Sleep(100);
    end;

    if fFireDebugEvent then
      fOndebugEvent(Self, 'UpdateToNewVersion',
        'Success');
    if not fSilentMode then
      fParentForm.Close;
    Result := True;
  end;
end;


procedure TLazAutoUpdate.ClearUpdateList;
// Unused
begin
  Setlength(fUpdateList, 0);
end;

function TLazAutoUpdate.AddToUpdateList(APrettyName, APath, AVersionString: string;
  AVersionNumber: cardinal): integer;
  // Unused
var
  iLast: integer;
  TheRec: UpdateListRecord;
begin
  Setlength(fUpdateList, 0);
  iLast := High(fUpdateList);
  if (iLast = -1) then
    iLast := 0; // For when array is empty
  if (iLast = 1) then
    Exit; // TEMP: Only one entry allowed


  try
    Inc(iLast);
    Setlength(fUpdateList, iLast);
    with TheRec do
    begin
      PrettyName := APrettyName;
      Path := APath;
      VersionString := AVersionString;
      VersionNumber := AVersionNumber;
    end;
    fUpdateList[iLast - 1] := TheRec; // Remember array is zero-based
  finally
    Result := High(fUpdateList); // 0 = one element
    fUpdateListCount := Result + 1; // 1 = one element
  end;
  if (Result = 0) then
  begin
    fAppFilename := fUpdateList[iLast - 1].Path;
    fDownloadZipName := ChangeFileExt(fAppFilename, '.zip');
    fZipfileName := fDownloadZipName;
    fApplicationVersionString := fUpdateList[iLast - 1].VersionString;
  end;

end;

function TLazAutoUpdate.GetThreadDownloadReturnCode: integer;
begin
  Result := 0;
  if ThreadDownload.ThreadFinished then
    Result := fThreadDownload.fReturnCode;
end;

procedure TLazAutoUpdate.SetProjectType(AValue: TProjectType);
// Set properties in a context-sensitive way
begin
  if (AValue <> fProjectType) then
    fProjectType := AValue;

  if fProjectType = auOther then
  begin
    fSourceForgeProjectName := C_NotApplicable;
    fGitHubRepositoryName := C_NotApplicable;
    fGitHubProjectName := C_NotApplicable;
    fGitHubBranchOrTag := C_NotApplicable;
    fauOtherSourceFilename := '';
    fauOtherSourceURL := '';
  end;
  if fProjectType = auSourceForge then
  begin
    fUpdatesFolder := C_UpdatesFolder;
    fSourceForgeProjectName := '';
    fauOtherSourceFilename := C_NotApplicable;
    fauOtherSourceURL := C_NotApplicable;
    fGitHubRepositoryName := C_NotApplicable;
    fGitHubProjectName := C_NotApplicable;
    fGitHubBranchOrTag := C_NotApplicable;
  end;
  if fProjectType = auGitHubReleaseZip then
  begin
    fZipFileName := ChangeFileExt(fVersionsININame, '.zip');
    fUpdatesFolder := C_UpdatesFolder;
    fSourceForgeProjectName := C_NotApplicable;
    fauOtherSourceFilename := C_NotApplicable;
    fauOtherSourceURL := C_NotApplicable;
    fGitHubBranchOrTag := C_MASTER;
    fGitHubRepositoryName := '';
    fGitHubProjectName := '';
    fUpdatesFolder := C_NotApplicable;
  end;

end;

procedure TLazAutoUpdate.SetSourceForgeProjectName(Avalue: string);
// Ensure lowercase
begin
  fSourceForgeProjectName := LowerCase(AValue);
end;

procedure TLazAutoUpdate.SetAppFilename(Avalue: string);
// Guess a default value
begin
  fAppFilename := AValue;
  // Set a default value?
  if (fDownloadZipName = '') then
    fDownloadZipName := ChangeFileExt(ExtractFilename(fAppFilename), '.zip');
  fThreadDownload.Filename := fUpdatesFolder + PathDelim + fDownloadZipName;
end;

procedure TLazAutoUpdate.SetApplicationVersionString(Avalue: string);
begin
  if AValue = '' then
    Exit;
  fApplicationVersionString := AValue;
  fApplicationVersionQuad := StrToVersionQuad(fApplicationVersionString);
end;

// Threaded version
// ================
// Var bDownloadIsPresent:Boolean;
//     MyTheadDownload:TThreadedDownload;
// Begin
// MyTheadDownload:=TThreadedDownload.Create(sourceforgedownloadURL,Localfilepath);
// {
//  Note the Localfilepath MUST be specified, and can be a different filename and path
//  than the filename specified in the sourceforgedownloadURL
// }
// bDownloadIsPresent:=MyTheadDownload.ThreadDownloadHTTP;
// MyTheadDownload.UnzipAfter:=FALSE; // True *by default* if targetfile is a zip file
// If NOT bDownloadIsPresent then Exit; {BailOut ->}
// WHILE NOT MyThreadDownload.ThreadFinished do
// begin
//  {.. show the user it is downloading in the background}
//  Application.ProcessMessages; // <- Very Important; else the app will freeze!
// end;
// {File has now downloaded OK to Localfilepath (and is optionally unzipped)}
// MyTheadDownload.Free;
// End;


{ TDownloadThreadClass }

constructor TDownloadThreadClass.Create(URL, FileName: string);
begin
  inherited Create(True);
  fURL := URL;
  fFileName := FileName;
  fReturnCode := 0; // Failure code
  fDownloadSize := 0;
  FreeOnTerminate := True;
  fLastError := C_OK;
end;

procedure TDownloadThreadClass.Execute;
begin
  // Start the download procedure
  DownloadHTTP(fURL, fFileName, fReturnCode, fDownloadSize, fIsSourceForge, fDebugMode);
end;

//constructor TThreadedDownload.Create(URL, FileName: string);
constructor TThreadedDownload.Create();
begin
  inherited Create;
  fThreadFinished := False;
  fAppLicationVersionString := '0.0.1';
  fComponentVersion := C_TThreadedDownloadComponentVersion;
  fLastError := C_OK;
end;

{ TThreadedDownload }

function TThreadedDownload.ThreadDownloadHTTP: boolean;
var
  download: TDownloadThreadClass;
begin
  if (CompareFileExt(ExtractFilename(fFileName), 'zip', False) = 0) then
    fUnzipAfter := True
  else
    fUnzipAfter := False;

  download := TDownloadThreadClass.Create(fURL, fFileName);
  download.OnTerminate := @DownloadTerminiated;
  download.fIsSourceForge := fIsSourceForge;
  download.fDebugMode := fDebugMode;
  download.fLastError := fLastError;
  download.FreeOnTerminate := True;
  download.start;
  Result := True;
end;

procedure TThreadedDownload.DownloadTerminiated(Sender: TObject);
// Unzips all files ready for updatehmxxx to copy them over
var
  UnZipper: TUnZipper;
begin
  fReturnCode := (Sender as TDownloadThreadClass).fReturnCode;
  fDownloadSize := (Sender as TDownloadThreadClass).fDownloadSize;
  fLastError := (Sender as TDownloadThreadClass).fLastError;
  fThreadFinished := True;
  if (FileExistsUTF8(fFileName) = True) and
    (CompareFileExt(fFileName, '.zip', False) = 0) then
    if fUnzipAfter then
    begin
      UnZipper := TUnZipper.Create;
      try
        UnZipper.FileName := fFileName;
        UnZipper.OutputPath := ExtractFileDir(fFileName);
        UnZipper.Examine;
        UnZipper.UnZipAllFiles;
        SysUtils.DeleteFile(fFileName);
      finally
        UnZipper.Free;
      end;
    end;
end;

{ End of class members}
function DownloadHTTP(URL, TargetFile: string; var ReturnCode, DownloadSize: integer;
  bIsSourceForge, fDebugmode: boolean): boolean;
  // Download file; retry if necessary.
  // Deals with https download links
var
  HTTPClient: TFPHTTPClient;
begin
  Result := False;
  HTTPClient := TFPHTTPClient.Create(nil);
  if bIsSourceForge then
  begin
    HTTPClient.AllowRedirect := True;
  end;
  // ReturnCode may not be useful, but it's provided here
  try
    try
      // Try to get the file
      HTTPClient.Get(URL, TargetFile);
      ReturnCode := HTTPClient.ResponseStatusCode;
      DownloadSize := Filesize(TargetFile);
      Result := True;
    except
      // We don't care for the reason for this error; the download failed.
      Result := False;
    end;
  finally
    HTTPClient.Free;
  end;

end;


end.
