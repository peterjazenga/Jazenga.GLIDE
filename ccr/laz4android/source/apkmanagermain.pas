unit apkmanagermain;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  FileUtil,
  LazFileUtils,
  Forms,
  Controls,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  Menus,
  ComCtrls,
  Buttons,
  Process,
  IniFiles,
  StrUtils;

const
  cVersion: string = 'ver. 0.9.0.38';
  cFPCVersion: string ='3.0.4';

type

  TAppState=(tas_init,
             tas_setup,
             tas_ready,
             tas_error);


  { TfrmApkManager }

  TfrmApkManager = class(TForm)
    BtnAllInOne: TButton;
    btnBuildProject: TButton;
    btnEditProject: TButton;
    btnCreateAndroidKey: TButton;
    btnCreateApkFile: TButton;
    btnInstallAPK: TButton;
    btnNewAndroidProject: TButton;
    BtnSelectProjectFolder: TButton;
    btnStartAdbLog: TButton;
    btnStartSDKManager: TButton;
    btnStartIDE: TButton;
    btnUnInstallAPK: TButton;
    btnStartAVDManager: TButton;
    btnOpenOutputFolder: TButton;
    cbxNDKPlatform: TComboBox;
    cbxSDKBuildTools: TComboBox;
    cbxCreateNewKey: TCheckBox;
    cbxAutoInstallAPK: TCheckBox;
    edtProjectFolder: TEdit;
    grpPathSettings: TGroupBox;
    lblAntPath: TLabel;
    lblProjectFolder: TLabel;
    lblJavaSDKPath: TLabel;
    lblAndroidSDKPath: TLabel;
    lblAndroidNDKPath: TLabel;
    lblLazarusPath: TLabel;
    lblNDKPlatform: TLabel;
    lblSDKBuildTool: TLabel;
    MainMenu1: TMainMenu;
    itmPathSetup: TMenuItem;
    itmSDKPath: TMenuItem;
    itmJDKPath: TMenuItem;
    itmBuildTool: TMenuItem;
    itmNDKPath: TMenuItem;
    itmAntPath: TMenuItem;
    mmoTrace: TMemo;
    mmoOutput: TMemo;
    OpenDialog1: TOpenDialog;
    pgcInfo: TPageControl;
    pnlState: TPanel;
    pnlTop: TPanel;
    pgcMain: TPageControl;
    rgrpADBLog: TRadioGroup;
    rgrpDevice: TRadioGroup;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    tabAutomatic: TTabSheet;
    tabManual: TTabSheet;
    AppSetupTimer: TTimer;
    tbsBuildOutput: TTabSheet;
    tbsTrace: TTabSheet;
    procedure btnBuildProjectClick(Sender: TObject);
    procedure btnCreateAndroidKeyClick(Sender: TObject);
    procedure btnEditProjectClick(Sender: TObject);
    procedure btnInstallAPKClick(Sender: TObject);
    procedure btnNewAndroidProjectClick(Sender: TObject);
    procedure btnOpenOutputFolderClick(Sender: TObject);
    procedure btnSelectProjectFolderClick(Sender: TObject);
    procedure btnStartAdbLogClick(Sender: TObject);
    procedure btnCreateApkFileClick(Sender: TObject);
    procedure btnStartAVDManagerClick(Sender: TObject);
    procedure btnStartIDEClick(Sender: TObject);
    procedure btnStartSDKManagerClick(Sender: TObject);
    procedure btnUnInstallAPKClick(Sender: TObject);
    procedure btnAllInOneClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure itmAntPathClick(Sender: TObject);
    procedure itmBuildToolClick(Sender: TObject);
    procedure itmJDKPathClick(Sender: TObject);
    procedure itmSDKPathClick(Sender: TObject);
    procedure itmNDKPathClick(Sender: TObject);
    procedure AppSetupTimerTimer(Sender: TObject);
  private
    FOnAppErrorEvent: TNotifyEvent;
    FOnAppNotReadyEvent: TNotifyEvent;
    FOnAppReadyEvent: TNotifyEvent;
    FOnAppSetupEvent: TNotifyEvent;
    FAppState:TAppState;
    FSettings: TIniFile;
    FLastUsedSearchDir: string;    // last used search path by any folder/file select dialog
    FAppConfigDir: string;
    FProcess: TProcess;
    FAndroidSDK: string;
    FAndroidNDK: string;
    FInstalledNDKPlatforms:TStrings;
    FInstalledSDKBuildTools:TStrings;
    FJDKPath: string;
    FAntPath: string;
    FTemplateProjectDir: string;    // folder of the template project
    FPassword: string;
    FProjectDir: string;            // project folder
    FProjectName:string;            // project name
    FProjectPathAndFilename:string; // contains full path and filename of the project.lpi file.
    FAPKFilename:string;            // full path and filename of the final apk-file.
    FBinaryFilename:string;         // full path and filename of the binary file liblclapp.so
    FKeyStoreFilename:string;       // full path and filename of the file LCLDebugKey.keystore.
    FBuildTool: string;             // full path and filename of the bulid tool. (lazbuild.exe or typhonbuild.exe)
    FNDKPlatform:string;            // NDK API Version
    FSDKBuildTool:string;           // SDK build tool version.
    FToolChainPath:string;          // path to the NDK tool chain path e.g. toolchains\arm-linux-androideabi-4.9\prebuilt\windows\bin\
    FNeeded3rdPartyToolsAvailable:boolean; // if JDK,NDK,SDK,xxxxbuild.exe is setup and available.
    FIsFirstAppStartUp:boolean;
    procedure DisplaySettings;
    procedure ReadSettings;
    procedure WriteSettings;
    function  SetupAndroidSDKPath:boolean; // show dialog to search for sdk path
    function  SetupAndroidNDKPath:boolean; // show dialog to search for ndk path
    function  SetupJDKPath:boolean;        // show dialog to search for jdk path
    function  SetupLazarusPath:boolean;    // show dialog to search for *build.exe path
    function  SetupApacheAntPath:boolean;  // show dialog to search for ant\bin path
    function  BuildLazarus:boolean; // copy build.bat into laz4android and start it.
    procedure CreateBuildBatchFile;   // create the build-batch file "build_app.bat".
    function  SetProjectSearchPath:boolean; // write the NDK path into the .lpi file.
    function  DetectInstalledNDKPlatforms:TStringList; // looks for the file libc.so in the NDK-folder
    function  DetectInstalledSDKBuildTools:TStringList; // looks for the folders in <sdk\build-tools>
    function  GetJDKPath:string;   // try to read the JDK folder from the registry.
    procedure PrepareNDKPlatformCombobox;  // prepare the content of the platforms/API selection combobox
    procedure ReadNDKPlatformFromCombobox;
    procedure PrepareSDKBuildToolsCombobox; // prepare the content of the build-tools selection combobox
    procedure ReadSDKBuildToolFromCombobox;
    procedure PrepareProject(_projectpath:string); // prepares the folder for a new project or does clean-up if the project already exists.
    function  BuildProject:boolean;   // compile&link the project
    function  CreateKey:boolean;      // create the key
    function  CreateAPK:boolean;      // create the final apk-file.
    function  RunScript(_CurrentDirectory,_Executable,_Parameters:string; _GetOutput: Boolean;_WaitOnExit:boolean=true):boolean; // run external batch file.
    procedure VerifyNeeded3rdPartyTools; // check if all needed 3rdParty-Tools are available. jdk,sdk,ndk,xxxxxbulid.exe
    function  CopyNDKFilesToFPC:boolean;  // copy the files ndk toolchains into the fpc folder
    procedure SetAppState(_newState:TAppState); // set new application state.
    procedure FireAppReadyEvent;
    procedure FireAppNotReadyEvent;
    procedure FireAppErrorEvent;
    procedure FireAppSetupEvent;
    procedure DoAppReadyEvent(Sender:TObject);
    procedure DoAppNotReadyEvent(Sender:TObject);
    procedure DoAppErrorEvent(Sender:TObject);
    procedure DoAppSetupEvent(Sender:TObject);
    procedure ShowInfoMessage(const _msg:string;const _params:array of const);
    procedure ShowWarnMessage(const _msg:string;const _params:array of const;const _ShowMessageBox:boolean=false);
    procedure StartLazarusIDE(_openProject:boolean);  // start laz4android and open the current project.
    procedure SetupApp;
    function  InstallAPK:boolean;   // install apk onto android device  --> runs %PROJECTDIR%\android\adb_install.bat
    function  UnInstallAPK:boolean; // un-install apk from android device  --> runs %PROJECTDIR%\android\adb_uninstall.bat
    procedure Trace(const _msg:string;_params:array of const);
  public
    destructor destroy;override;
    property OnAppReady:TNotifyEvent read FOnAppReadyEvent write FOnAppReadyEvent;
    property OnAppNotReady:TNotifyEvent read FOnAppNotReadyEvent write FOnAppNotReadyEvent;
    property OnAppError:TNotifyEvent read FOnAppErrorEvent write FOnAppErrorEvent;
    property OnAppSetup:TNotifyEvent read FOnAppSetupEvent write FOnAppSetupEvent;
  end;

var
  frmApkManager: TfrmApkManager;

implementation

uses
  Graphics,
  LCLIntf,
  Registry,
  FindFolderFrm;

{$R *.lfm}

{ TfrmApkManager }

procedure TfrmApkManager.FormCreate(Sender: TObject);
begin
  FIsFirstAppStartUp:=true;
  pgcMain.ActivePageIndex:=0;
  OnAppReady:=@DoAppReadyEvent;
  OnAppNotReady:=@DoAppNotReadyEvent;
  OnAppError:=@DoAppErrorEvent;
  OnAppSetup:=@DoAppSetupEvent;
  ShowInfoMessage('Initialize. Please wait...',[]);
  SetAppState(tas_init);
  FAppConfigDir := ExtractFilePath(Application.ExeName);
  FSettings := TIniFile.Create(FAppConfigDir + ChangeFileExt(ExtractFilename(Application.ExeName), '.ini'));
  Application.ShowHint := True;
  Caption := 'laztoapk ' + cVersion;
  ReadSettings;
  BuildLazarus;
end;

procedure TfrmApkManager.FormShow(Sender: TObject);
begin
  AppSetupTimer.enabled:=true;
end;

procedure TfrmApkManager.itmAntPathClick(Sender: TObject);
begin
  SetupApacheAntPath;
  DisplaySettings;
  VerifyNeeded3rdPartyTools;
end;

procedure TfrmApkManager.itmBuildToolClick(Sender: TObject);
begin
  SetupLazarusPath;
  DisplaySettings;
  VerifyNeeded3rdPartyTools;
end;

procedure TfrmApkManager.itmJDKPathClick(Sender: TObject);
begin
  SetupJDKPath;
  DisplaySettings;
  VerifyNeeded3rdPartyTools;
end;

procedure TfrmApkManager.itmSDKPathClick(Sender: TObject);
begin
  SetupAndroidSDKPath;
  DisplaySettings;
  VerifyNeeded3rdPartyTools;
end;

procedure TfrmApkManager.itmNDKPathClick(Sender: TObject);
begin
  SetupAndroidNDKPath;
  DisplaySettings;
  VerifyNeeded3rdPartyTools;
end;

procedure TfrmApkManager.AppSetupTimerTimer(Sender: TObject);
begin
  AppSetupTimer.enabled:=false;
  Screen.Cursor:=crHourGlass;
  SetAppState(tas_setup);
end;

procedure TfrmApkManager.DisplaySettings;
begin
  lblAndroidSDKPath.caption:=format('Android SDK Path: %s',[FAndroidSDK]);
  lblAndroidNDKPath.caption:=format('Android NDK Path: %s',[FAndroidNDK]);
  lblJavaSDKPath.caption:=format('Java JDK Path: %s',[FJDKPath]);
  lblLazarusPath.caption:=format('Build tool: %s',[FBuildTool]);
  lblAntPath.caption:=format('Apache-Ant Path: %s',[FAntPath]);
end;

procedure TfrmApkManager.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  WriteSettings;
  FreeAndNil(FSettings);
  if FProcess <> nil then FreeAndNil(FProcess);
end;

procedure TfrmApkManager.ReadSettings;
begin
  trace('Read settings from file <%s>.',[FSettings.FileName]);
  FIsFirstAppStartUp := FSettings.ReadBool('App','FirstApplicationStartUp',true);
  FLastUsedSearchDir := FSettings.ReadString('Path', 'LastUsedSearchDir', ExtractFilePath(Application.ExeName));
  FProjectDir        := FSettings.ReadString('Path', 'LastUsedProjectDir',ExtractFilePath(Application.ExeName)+'projects\project1');

  FJDKPath := FSettings.ReadString('Path', 'JDKPath', GetJDKPath);
  if (FJDKPath = '')  or
     (not directoryexistsUTF8(FJDKPath)) then SetupJDKPath;

  FAndroidSDK := FSettings.ReadString('Path', 'AndroidSDKPath', extractfilepath(application.exename)+'downloads\android-sdk-windows\');
  if (FAndroidSDK = '') or
     (not directoryExistsUTF8(FAndroidSDK)) then SetupAndroidSDKPath;

  FAndroidNDK := FSettings.ReadString('Path', 'AndroidNDKPath', extractfilepath(application.exename)+'downloads\android-ndk-r14b\');
  if (FAndroidNDK = '') or
     (not directoryexistsUTF8(FAndroidNDK)) then SetupAndroidNDKPath;

  FBuildTool:= FSettings.ReadString('Path', 'BuildTool', extractfilepath(application.exename)+'downloads\laz4android1.8\lazbuild.exe');
  if not FIsFirstAppStartUp then begin
    if (FBuildTool = '')   or
       (not fileexistsUTF8(FBuildTool)) then SetupLazarusPath;
  end;

  FAntPath:= FSettings.ReadString('Path', 'AntPath', extractfilepath(application.exename)+'downloads\apache-ant-1.10.1\bin');
  if not FIsFirstAppStartUp then begin
    if (FAntPath = '')   or
       (not DirectoryexistsUTF8(FAntPath)) then SetupApacheAntPath;
  end;

  FToolChainPath:= FSettings.ReadString('Path', 'NDKToolChainPath', 'toolchains\arm-linux-androideabi-4.9\prebuilt\windows\bin\');
  FTemplateProjectDir:= FSettings.ReadString('Path', 'TemplateProjectPath', ExtractFilePath(Application.ExeName) +'TemplateProject' + PathDelim);
  FPassword:=FSettings.ReadString('KeyStore', 'Password', 'test12');
  if not FileExistsUTF8(FTemplateProjectDir) then CreateDirUTF8(FTemplateProjectDir);
  DisplaySettings;
  SelectDirectoryDialog1.InitialDir := FLastUsedSearchDir;
  rgrpDevice.ItemIndex   := FSettings.ReadInteger('Config', 'Device', 1);
  rgrpADBLog.ItemIndex   := FSettings.ReadInteger('Config', 'AdbLog', 1);
  pgcMain.ActivePageIndex:= FSettings.ReadInteger('Config', 'LastUsedTab',0);
  cbxCreateNewKey.checked:= FSettings.ReadBool('Config', 'CreateKeyFile',true);
  cbxAutoInstallAPK.checked:=FSettings.ReadBool('Config', 'AutomaticInstallAPK',true);
  edtProjectFolder.text  :=  ExcludeTrailingPathDelimiter(FProjectDir);
end;

procedure TfrmApkManager.WriteSettings;
begin
  trace('Write settings to file <%s>.',[FSettings.FileName]);
  FSettings.WriteBool('App','FirstApplicationStartUp',false);
  FSettings.WriteString('Path', 'AndroidSDKPath', FAndroidSDK);
  FSettings.WriteString('Path', 'AndroidNDKPath', FAndroidNDK);
  FSettings.WriteString('Path', 'JDKPath', FJDKPath);
  FSettings.WriteString('Path', 'BuildTool', FBuildTool);
  FSettings.WriteString('Path', 'AntPath', FAntPath);
  FSettings.WriteString('Path', 'NDKToolChainPath', FToolChainPath);
  FSettings.WriteString('Path', 'TemplateProjectPath', FTemplateProjectDir);
  FSettings.WriteString('Path', 'LastUsedProjectDir', FProjectDir);
  FSettings.WriteString('Path', 'LastUsedSearchDir',FLastUsedSearchDir);
  FSettings.WriteString('KeyStore', 'Password',FPassword);
  FSettings.WriteInteger('Config', 'Device', rgrpDevice.ItemIndex);
  FSettings.WriteInteger('Config', 'AdbLog', rgrpADBLog.ItemIndex);
  FSettings.WriteInteger('Config', 'LastUsedTab', pgcMain.ActivePageIndex);
  FSettings.WriteInteger('Config', 'LastUsedAPI', cbxNDKPlatform.itemindex);
  FSettings.WriteInteger('Config', 'LastUsedBuildTool',cbxSDKBuildTools.itemindex);
  FSettings.WriteBool('Config', 'CreateKeyFile',cbxCreateNewKey.checked);
  FSettings.WriteBool('Config', 'AutomaticInstallAPK',cbxAutoInstallAPK.checked);
end;

function TfrmApkManager.SetupAndroidSDKPath:boolean;
var
_pathname:string;
begin
  result:=false;
  _pathname:=ShowFindFolderDlg(FLastUsedSearchDir,'sdk','Select Android SDK directory',tst_folder);
  if _pathname='' then exit;
  if not DirectoryExistsUTF8(_pathname) then exit;
  FAndroidSDK := IncludeTrailingPathDelimiter(_pathname);
  trace('Set Android SDK path to <%s>.',[FAndroidSDK]);
  FSettings.WriteString('Path', 'AndroidSDKPath', FAndroidSDK);
  if assigned(FInstalledSDKBuildTools) then FInstalledSDKBuildTools.free;
  FInstalledSDKBuildTools:=DetectInstalledSDKBuildTools;
  PrepareSDKBuildToolsCombobox;
  result:=true;
end;

function TfrmApkManager.SetupAndroidNDKPath: boolean;
var
_pathname:string;
begin
  result:=false;
  _pathname:=ShowFindFolderDlg(FLastUsedSearchDir,'ndk','Select Android NDK directory',tst_folder);
  if _pathname='' then exit;
  if not DirectoryExistsUTF8(_pathname) then exit;
  FAndroidNDK := IncludeTrailingPathDelimiter(_pathname);
  trace('Set Android NDK path to <%s>.',[FAndroidNDK]);
  FSettings.WriteString('Path', 'AndroidNDKPath', FAndroidNDK);
  if assigned(FInstalledNDKPlatforms) then FInstalledNDKPlatforms.free;
  FInstalledNDKPlatforms:=DetectInstalledNDKPlatforms;
  PrepareNDKPlatformCombobox;
  result:=CopyNDKFilesToFPC;
end;

function TfrmApkManager.SetupLazarusPath:boolean;
var
_filename:string;
begin
  result:=false;
  _filename:=ShowFindFolderDlg(FLastUsedSearchDir,'*build.exe','Select Build-Tool. e.g. lazbuild.exe, typhonbuild.exe',tst_file);
  if _filename='' then exit;
  FBuildTool := _filename;
  trace('Set Build Tool path to <%s>.',[FBuildTool]);
  FSettings.WriteString('Path', 'BuildTool', FBuildTool);
  result:=CopyNDKFilesToFPC;
  if not result then ShowWarnMessage('Problem in SetupLazarusPath: Failed to setup Lazarus Path. See trace for more info.',[]);
end;

function TfrmApkManager.SetupJDKPath: boolean;
var
_pathname:string;
begin
  result:=false;
  _pathname:=ShowFindFolderDlg(FLastUsedSearchDir,'jdk','Select JDK directory (e.g. c:\program files\java\jdkxxxx)',tst_folder);
  if _pathname='' then exit;
  if not DirectoryExistsUTF8(_pathname) then exit;
  FJDKPath := IncludeTrailingPathDelimiter(_pathname);
  trace('Set JDK path to <%s>.',[FJDKPath]);
  FSettings.WriteString('Path', 'JDKPath', FJDKPath);
  result:=true;
end;

function  TfrmApkManager.SetupApacheAntPath:boolean;  // show dialog to search for ant\bin path
var
_pathname:string;
begin
  result:=false;
  _pathname:=ShowFindFolderDlg(FLastUsedSearchDir,'ant','Select ant\bin directory',tst_folder);
  if _pathname='' then exit;
  if not DirectoryExistsUTF8(_pathname) then exit;
  FAntPath := IncludeTrailingPathDelimiter(_pathname);
  trace('Set Ant path to <%s>.',[FAntPath]);
  FSettings.WriteString('Path', 'AntPath', FAntPath);
  result:=true;
end;

procedure TfrmApkManager.btnNewAndroidProjectClick(Sender: TObject);
begin
  SelectDirectoryDialog1.Title := 'Create/Select project directory';
  if not SelectDirectoryDialog1.Execute then exit;
  PrepareProject(SelectDirectoryDialog1.FileName);
  btnEditProject.enabled:=true;
  btnBuildProject.Enabled:=true;
  btnCreateAndroidKey.Enabled:=true;
  btnInstallAPK.enabled:=true;
  btnUnInstallAPK.enabled:=true;
  btnCreateApkFile.Enabled:=FileExistsUTF8(FBinaryFilename) and FileExistsUTF8(FKeyStoreFilename);
  BringToFront;
  btnEditProject.setFocus;
end;

procedure TfrmApkManager.btnOpenOutputFolderClick(Sender: TObject);
begin
  OpenDocument(extractfilepath(FAPKFilename));
end;

procedure TfrmApkManager.PrepareProject(_projectpath:string);
var
  i: integer;
  sRec: TSearchRec;
  UpperProjectname, filename: string;
  tempList: TStringList;
  _temp:string;
begin
  ReadNDKPlatformFromCombobox;
  ReadSDKBuildToolFromCombobox;
  FProjectName     :=ExtractFileName(_projectpath);
  FProjectDir      :=IncludeTrailingPathDelimiter(_projectpath);
  if FProjectName='' then begin
    _projectpath:=ExcludeTrailingPathDelimiter(_projectpath);
    FProjectName:=ExtractFileName(_projectpath);
  end;
  FProjectPathAndFilename:= FProjectDir + FProjectName + 'android.lpi';
  FAPKFilename     :=FProjectDir + 'android\bin\'+FProjectName+'.apk';
  FBinaryFilename  :=FProjectDir + 'android\libs\armeabi\liblclapp.so';
  FKeyStoreFilename:=FProjectDir + 'android\bin\LCLDebugKey.keystore';
  UpperProjectname := UpperCase(Copy(FProjectName, 1, 1)) +Copy(FProjectName, 2, Length(FProjectName) - 1);
  if DirectoryExistsUTF8(FProjectDir) then begin
    DeleteFileUTF8(FProjectDir + 'android\bin\'+FProjectName+'.apk');
    DeleteFileUTF8(FProjectDir + 'android\bin\'+FProjectName+'-unsigned.apk');
    DeleteFileUTF8(FProjectDir + 'android\bin\'+FProjectName+'-unaligned.apk');
    DeleteFileUTF8(FProjectDir + 'android\bin\classes.dex');
  end;
  if not FileExistsUTF8(FProjectDir+FProjectName+'android.lpi') then begin
    if not CopyDirTree(ExcludeTrailingPathDelimiter(FTemplateProjectDir), ExcludeTrailingPathDelimiter(FProjectDir),[cffOverwriteFile,cffCreateDestDirectory,cffPreserveTime]) then begin // copy all files from template project
      ShowWarnMessage('Could not copy the folder <%s> to <%s>.',[FTemplateProjectDir, FProjectDir]);
      exit;
    end;
    ShowInfoMessage('Copied the folder <%s> to <%s>.',[FTemplateProjectDir, FProjectDir]);
    // set app-password
    repeat
      if not InputQuery('Android password', 'Set App password(min. 6 characters)',FPassword) then exit;
    until Length(FPassword) >= 6;

    // change text "template" in all files to current projectname
    tempList := TStringList.Create;
    try
      i := FindFirst(FProjectDir + 'template*.*', faAnyFile, sRec);
      while i = 0 do begin
        filename := AnsiReplaceText(sRec.Name, 'template', FProjectName);
        RenameFile(FProjectDir + sRec.Name, FProjectDir + filename);

        if PosEx('.ico', filename) = 0 then begin
          tempList.LoadFromFile(FProjectDir + filename);
          tempList.Text := AnsiReplaceText(tempList.Text, 'Template', UpperProjectname);
          tempList.Text := AnsiReplaceText(tempList.Text, 'template', FProjectName);
          tempList.Text := AnsiReplaceText(tempList.Text, 'lcltest',  FProjectName);
          tempList.SaveToFile(FProjectDir + filename);
          trace('Save file <%s>.',[FProjectDir + filename]);
        end;
        i := FindNext(sRec);
      end;
      FindClose(sRec);
    finally
      tempList.free;
    end;
  end;
    // adapt generate_debug_key.bat
  tempList := TStringList.Create;
  try
    tempList.LoadFromFile(FProjectDir + 'android\generate_debug_key.bat');
    tempList.Strings[1] := format('SET PATH=%stools;%splatform-tools;%sbin;',[FAndroidSDK,FAndroidSDK,FJDKPath]);
    tempList.Strings[2] := format('SET APP_NAME=%s',[FProjectName]);
    tempList.Strings[3] := format('SET ANDROID_HOME="%s"',[ExcludeTrailingPathDelimiter(FAndroidSDK)]);
    tempList.Strings[4] := format('SET APK_SDK_PLATFORM="%splatforms\android-%s"',[FAndroidSDK,FNDKPlatform]);
    tempList.Strings[5] := format('SET APK_PROJECT_PATH=%sandroid',[FProjectDir]);
    tempList.Strings[6] := format('SET SDK_BUILDTOOLS=%s',[FSDKBuildTool]);
    tempList.Strings[7] := format('SET PASSWORD=%s',[FPassword]);
    tempList.SaveToFile(FProjectDir + 'android\generate_debug_key.bat');
  // adapt build_debug_apk.bat
    tempList.LoadFromFile(FProjectDir + 'android\build_debug_apk.bat');
    tempList.Strings[1] := format('SET PATH=%stools;%splatform-tools\;%sbuild-tools\%s\;%sbin',[FAndroidSDK,FAndroidSDK,FAndroidSDK,FSDKBuildTool,FJDKPath]);
    tempList.Strings[2] := format('SET APP_NAME=%s', [FProjectName]);
    tempList.Strings[3] := format('SET ANDROID_HOME="%s"',[ExcludeTrailingPathDelimiter(FAndroidSDK)]);
    tempList.Strings[4] := format('SET APK_SDK_PLATFORM="%splatforms\android-%s"',[FAndroidSDK,FNDKPlatform]);
    tempList.Strings[5] := format('SET APK_PROJECT_PATH=%sandroid',[FProjectDir]);
    tempList.Strings[6] := format('SET SDK_BUILDTOOLS=%s',[FSDKBuildTool]);
    tempList.Strings[7] := format('SET PASSWORD=%s',[FPassword]);
    _temp:='com\pascal\'+FProjectName;
    tempList.Text := AnsiReplaceText(tempList.Text, 'com\pascal\lcltest',_temp);
    tempList.SaveToFile(FProjectDir + 'android\build_debug_apk.bat');
  // adapt strings.xml
    tempList.LoadFromFile(FProjectDir + 'android\res\values\strings.xml');
    tempList.Text := AnsiReplaceText(tempList.Text,'<string name="app_name">Template</string>', '<string name="app_name">' +UpperProjectname + '</string>');
    tempList.SaveToFile(FProjectDir + 'android\res\values\strings.xml');
  // adapt adb_install.bat
    tempList.LoadFromFile(FProjectDir + 'android\adb_install.bat ');
    tempList.Strings[1] := 'SET PATH=' + FAndroidSDK + 'tools;' +FAndroidSDK + 'platform-tools\;'+FAndroidSDK+'build-tools\'+FBuildTool+'\;' + FJDKPath + 'bin';
    tempList.Text := AnsiReplaceText(tempList.Text, 'lcltest', FProjectName);
    tempList.SaveToFile(FProjectDir + 'android\adb_install.bat ');
    // adapt adb_uninstall.bat
    tempList.LoadFromFile(FProjectDir + 'android\adb_uninstall.bat ');
    tempList.Strings[1] := 'SET PATH=' + FAndroidSDK + 'tools;' +FAndroidSDK + 'platform-tools\;'+FAndroidSDK+'build-tools\'+FBuildTool+'\;' + FJDKPath + 'bin';
    tempList.Text := AnsiReplaceText(tempList.Text, 'lcltest', FProjectName);
    tempList.SaveToFile(FProjectDir + 'android\adb_uninstall.bat ');
    // adapt adb_logcat.bat
    tempList.LoadFromFile(FProjectDir + 'android\adb_logcat.bat ');
    tempList.Strings[1] := 'SET PATH=' + FAndroidSDK + 'tools;' +FAndroidSDK + 'platform-tools\;'+FAndroidSDK+'build-tools\'+FBuildTool+'\;' + FJDKPath + 'bin';
    tempList.SaveToFile(FProjectDir + 'android\adb_logcat.bat ');
  // adapt build.sh
    tempList.LoadFromFile(FProjectDir + 'android\build.sh');
    tempList.Text := AnsiReplaceText(tempList.Text, 'lcltest', FProjectName);
    tempList.SaveToFile(FProjectDir + 'android\build.sh');
  // adapt AndroidManifest.xml
    tempList.LoadFromFile(FProjectDir + 'android\AndroidManifest.xml');
    tempList.Text := AnsiReplaceText(tempList.Text, 'com.pascal.lcltest','com.pascal.'+FProjectName);
    tempList.SaveToFile(FProjectDir + 'android\AndroidManifest.xml');
    if directoryexistsUTF8(FProjectDir + 'android\src\com\pascal\lcltest\') then begin
    // adapt LCLActivity.java
      tempList.LoadFromFile(FProjectDir +'android\src\com\pascal\lcltest\LCLActivity.java');
      tempList.Strings[0] := 'package com.pascal.' + FProjectName + ';';
      tempList.SaveToFile(FProjectDir + 'android\src\com\pascal\lcltest\LCLActivity.java');
      RenameFile(FProjectDir + 'android\src\com\pascal\lcltest\',FProjectDir + 'android\src\com\pascal\' + FProjectName + PathDelim);
    end;
  finally
    FreeAndNil(tempList);
  end;

  CreateBuildBatchFile;
  SetProjectSearchPath;
end;

procedure TfrmApkManager.btnSelectProjectFolderClick(Sender: TObject);
begin
  SelectDirectoryDialog1.Title := 'Select project directory';
  if not SelectDirectoryDialog1.Execute then exit;
  edtProjectFolder.Text := SelectDirectoryDialog1.FileName;
  FLastUsedSearchDir:=SelectDirectoryDialog1.FileName;
  cbxCreateNewKey.checked:=not fileexistsUTF8(FKeystoreFilename);
end;

procedure TfrmApkManager.btnCreateAndroidKeyClick(Sender: TObject);
begin
  if not CreateKey then exit;
  btnCreateApkFile.enabled:=true;
  BringToFront;
  btnCreateApkFile.setfocus;
end;

procedure TfrmApkManager.btnEditProjectClick(Sender: TObject);
begin
  StartLazarusIDE(true);
end;

function TfrmApkManager.CreateKey:boolean;
begin
  if fileexistsUTF8(FProjectDir + 'android\build_debug_apk.bat') then begin
    if FPassword = '' then begin
      repeat
        if not InputQuery('Android password', 'Set App password(min. 6 characters)',FPassword) then exit;
      until Length(FPassword) >= 6;
    end;
  end;

  if FileExistsUTF8(FKeystoreFilename) then begin
    if not DeleteFileUTF8(FKeystoreFilename) then begin
      showWarnMessage('Could not delete file <%s>. Please try to delete it manually.',[FKeystoreFilename],true);
      exit;
    end;
  end;

  RunScript(FProjectDir+'android\',FProjectDir+'android\generate_debug_key.bat','', False);
  result:=FileExistsUTF8(FKeystoreFilename);
  if result then trace('Created file <%s>.',[FKeystoreFilename]);
end;

procedure TfrmApkManager.CreateBuildBatchFile;
var
_BuildAppFile:TStrings;
_buildtool:string;
begin
  _BuildAppFile:=TStringList.create;
  try
    _buildtool:=FBuildTool;
    _BuildAppFile.Add('pushd '+extractfilepath(_buildtool));
    _BuildAppFile.Add(extractfilename(_buildtool)+' "'+FProjectPathAndFilename+'"');
    _BuildAppFile.Add('REM pause');
    _BuildAppFile.SaveToFile(FProjectDir + 'android\build_app.bat');
    trace('Saved file <%s>.',[FProjectDir + 'android\build_app.bat']);
  finally
    _BuildAppFile.free;
  end;
end;

function TfrmApkManager.SetProjectSearchPath: boolean;
var
_ProjectFile:TStrings;
_ndkLibPath:string;
begin
  result:=false;
// prepare the NDK Lib Path
   ReadNDKPlatformFromCombobox;
  _ndkLibPath:=format('%splatforms\android-%s\arch-arm\usr\lib\',[FAndroidNDK,FNDKPlatform]);
// prepare the lpi-Project filename
  _ProjectFile:=TStringList.create;
  try
    _ProjectFile.loadfromfile(FProjectPathAndFilename);
    _ProjectFile.Text := AnsiReplaceText(_ProjectFile.Text, '%NDKPATH%', _ndkLibPath);
    _ProjectFile.SaveToFile(FProjectPathAndFilename);
    trace('Saved search path <%s> to file <%s>.',[_ProjectFile.Text,FProjectPathAndFilename]);
  finally
    _ProjectFile.free;
  end;
end;

function TfrmApkManager.DetectInstalledNDKPlatforms: TStringList;
var
i:integer;
_pos:integer;
_temp:string;
_tempList:TStringList;
_sdkplatformfile:string;
begin
  ShowInfoMessage('Looking for installed NDK. Searching for <libc.so> in path <%s>. Please wait...',[FAndroidNDK]);
  result:=FindAllFiles(FAndroidNDK,'libc.so');
  if result.count=0 then begin
    ShowWarnMessage('No files with name <libc.so> found in NDK-path <%s>. Did you install the android NDK and setup the correct NDK-Path?',[FAndroidNDK],true);
    exit;
  end;
  _tempList:=TStringList.create;
  try
    _tempList.Duplicates:=dupIgnore;
    _tempList.sorted:=true;
    for i:=0 to result.count-1 do begin
      _pos:=pos('arch-arm',result[i]);
      if _pos=0 then continue;
      _pos:=pos('platforms'+PathDelim+'android-',result[i]);
      if _pos=0 then continue;
      _temp:=result[i];
      ShowInfoMessage('Found installed NDK <%s>.',[_temp]);
      delete(_temp,1,_pos+17);
      _pos:=pos(PathDelim,_temp);
      _temp:=copy(_temp,1,_pos-1);
      _sdkplatformfile:=FAndroidSDK+'platforms'+PathDelim+'android-'+_temp+PathDelim+'android.jar';
      ShowInfoMessage('Looking for installed SDK. Searching for <%s>. Please wait...',[_sdkplatformfile]);
      if not FileexistsUTF8(_sdkplatformfile) then begin
        trace('Could not find file <%s>. This NDK version can not be used. Skip it.',[_sdkplatformfile]);
        continue;
      end;
      _tempList.add(_temp);
      trace('Found file <%s>. Adding <%s>',[_sdkplatformfile,_temp]);
    end;
    result.assign(_tempList);
  finally
    _tempList.free;
  end;
end;

function TfrmApkManager.DetectInstalledSDKBuildTools: TStringList;
var
i:integer;
_pos:integer;
_temp:string;
_tempList:TStringList;
_searchpath:string;
begin
  _searchpath:=FAndroidSDK+'build-tools'+PathDelim;
  ShowInfoMessage('Looking for installed SDK. Searching for <dx.bat> in path <%s>. Please wait...',[_searchpath]);
  result:=FindAllFiles(_searchpath,'dx.bat');
  if (result.count=0) and
      (not FIsFirstAppStartUp) then begin
    ShowWarnMessage('No build-tools found in SDK-path <%s>. Did you install the android SDK and setup the correct SDK-Path?',[_searchpath],true);
    exit;
  end;
  _tempList:=TStringList.create;
  try
    _tempList.Duplicates:=dupIgnore;
    _tempList.sorted:=true;
    for i:=0 to result.count-1 do begin
      _pos:=pos('build-tools',result[i]);
      if _pos=0 then continue;
      _temp:=result[i];
      trace('Found <%s>.',[_temp]);
      delete(_temp,1,_pos+11);
      _pos:=pos(PathDelim,_temp);
      _temp:=copy(_temp,1,_pos-1);
      _tempList.add(_temp);
    end;
    result.assign(_tempList);
  finally
    _tempList.free;
  end;
end;

function TfrmApkManager.GetJDKPath: string;
var
_reg:TRegistry;
_CurrentVersion:string;
_path:string;
begin
  _reg:=TRegistry.create(KEY_READ OR KEY_WOW64_64KEY);
  try
    _reg.RootKey:=HKEY_LOCAL_MACHINE;
    if not _reg.KeyExists('SOFTWARE\JavaSoft\Java Development Kit\') then begin
      ShowWarnMessage('Could not find registry key <%s> for Java Development Kit (JDK) in Registry. Look''s like no JDK is installed!!',['HKEY_LOCAL_MACHINE\SOFTWARE\JavaSoft\Java Development Kit\'],true);
      exit;
    end;
    if not _reg.OpenKeyReadOnly('SOFTWARE\JavaSoft\Java Development Kit\') then begin
      ShowWarnMessage('Could not open registry key <%s> for Java Development Kit (JDK) in Registry. Look''s like you have no read access!!',['HKEY_LOCAL_MACHINE\SOFTWARE\JavaSoft\Java Development Kit\'],true);
      exit;
    end;
    _CurrentVersion:=_reg.ReadString('CurrentVersion');
    _reg.CloseKey;
    if not _reg.OpenKeyReadOnly('SOFTWARE\JavaSoft\Java Development Kit\'+_CurrentVersion) then exit;
    _path:=_reg.ReadString('JavaHome');
    _reg.CloseKey;
    if not DirectoryExistsUTF8(_path) then exit;
    result:=IncludeTrailingPathDelimiter(_path);
  finally
    _reg.free;
  end;
end;

procedure TfrmApkManager.PrepareNDKPlatformCombobox;
var
i:integer;
_platforms:TStringList;
_API:integer;
begin
  if FInstalledNDKPlatforms.count=0 then begin
    cbxNDKPlatform.clear;
    cbxNDKPlatform.Items.add('No NDK platform found');
    cbxNDKPlatform.ItemIndex:=0;
    cbxNDKPlatform.Enabled:=false;
    pgcMain.enabled:=false;
    Screen.Cursor:=crDefault;
    enabled:=true;
    exit;
  end;

  _platforms:=TStringList.create;
  _platforms.sorted:=true;
  try
    for i:=0 to FInstalledNDKPlatforms.count-1 do begin
      _API:=strtoint(FInstalledNDKPlatforms[i]);
      case _API of
        23:_platforms.add('6.0 (API 23)');
        22:_platforms.add('5.1.1 (API 22)');
        21:_platforms.add('5.0.1 (API 21)');
        20:_platforms.add('4.4W (API 20)');
        19:_platforms.add('4.4 (API 19)');
        18:_platforms.add('4.3 (API 18)');
        17:_platforms.add('4.2.2 (API 17)');
        16:_platforms.add('4.1.2 (API 16)');
        15:_platforms.add('4.0.3 (API 15)');
        14:_platforms.add('4.0 (API 14)');
        13:_platforms.add('3.2 (API 13)');
        12:_platforms.add('3.1 (API 12)');
        11:_platforms.add('3.0 (API 11)');
         8:_platforms.add('2.2 (API 8)');
      end;
    end;
    cbxNDKPlatform.Items.assign(_platforms);
    if FSettings.ReadInteger('Config', 'LastUsedAPI',0)<cbxNDKPlatform.Items.count then cbxNDKPlatform.ItemIndex:=FSettings.ReadInteger('Config', 'LastUsedAPI',0);
    cbxNDKPlatform.Enabled:=true;
  finally
    _platforms.free;
  end;
end;

procedure TfrmApkManager.ReadNDKPlatformFromCombobox;
var
  _VersionText:string;
  _APIVersion:string;
  _pos:integer;
begin
  _VersionText:=cbxNDKPlatform.text;
  _pos:=Pos('(API ',_VersionText);
  _APIVersion:=_VersionText;
  Delete(_APIVersion,1,_pos+4);
  _pos:=Pos(')',_APIVersion);
  _APIVersion:=copy(_APIVersion,1,_pos-1);
  FNDKPlatform:=_APIVersion;
end;

procedure TfrmApkManager.PrepareSDKBuildToolsCombobox;
var
_buildtools:TStringList;
begin
  if FInstalledSDKBuildTools.count=0 then begin
    cbxSDKBuildTools.clear;
    cbxSDKBuildTools.Items.add('No SDK Build-Tools found');
    cbxSDKBuildTools.ItemIndex:=0;
    cbxSDKBuildTools.Enabled:=false;
    pgcMain.enabled:=false;
    Screen.Cursor:=crDefault;
    enabled:=true;
    exit;
  end;
  _buildtools:=TStringList.create;
  _buildtools.assign(FInstalledSDKBuildTools);
  _buildtools.sorted:=true;
  cbxSDKBuildTools.Clear;
  try
    cbxSDKBuildTools.Items.assign(_buildtools);
    if FSettings.ReadInteger('Config', 'LastUsedBuildTool',0)<cbxSDKBuildTools.Items.count then cbxSDKBuildTools.ItemIndex:=FSettings.ReadInteger('Config', 'LastUsedBuildTool',0);
    cbxSDKBuildTools.Enabled:=true;
  finally
    _buildtools.free;
  end;
end;

procedure TfrmApkManager.ReadSDKBuildToolFromCombobox;
begin
  FSDKBuildTool:=cbxSDKBuildTools.text;
end;


function TfrmApkManager.BuildProject:boolean;
var
_CompiledFilename:string;
begin
  result:=false;
  if FileExistsUTF8(FBinaryFilename) then begin
    if not DeleteFileUTF8(FBinaryFilename) then begin
      ShowWarnMessage('Could not delete file <%s>. Please try to delete it manually.',[FBinaryFilename],true);
      exit;
    end;
  end;
  _CompiledFilename:=FProjectDir+'lib\arm-android\'+FProjectName+'android.compiled';
  if FileExistsUTF8(_CompiledFilename) then begin
    if not DeleteFileUTF8(_CompiledFilename) then begin
      ShowWarnMessage('Could not delete file <%s>. Please try to delete it manually.',[_CompiledFilename],true);
      exit;
    end;
  end;
  RunScript(extractfilepath(FBuildTool) ,FProjectDir + 'android\build_app.bat','', True);
  result:=FileExistsUTF8(FBinaryFilename);
end;

procedure TfrmApkManager.btnBuildProjectClick(Sender: TObject);
begin
  if not BuildProject then exit;
  btnCreateAndroidKey.enabled:=true;
  BringToFront;
  btnCreateAndroidKey.setfocus;
end;

procedure TfrmApkManager.btnCreateApkFileClick(Sender: TObject);
begin
  if not CreateAPK then exit;
  pnlState.Font.color:=clBlack;
  ShowInfoMessage('Done. The app is here: <%s>.',[FAPKFilename]);
  btnInstallAPK.enabled:=true;
  BringToFront;
end;

procedure TfrmApkManager.btnStartAVDManagerClick(Sender: TObject);
begin
  RunScript(FAndroidSDK,FAndroidSDK+'AVD Manager.exe','',false,false);
end;

procedure TfrmApkManager.btnStartIDEClick(Sender: TObject);
begin
  StartLazarusIDE(false);
end;

procedure TfrmApkManager.StartLazarusIDE(_openProject:boolean);
var
_CommandLine:string;
begin
  if (FProjectPathAndFilename<>'') and
     _openProject then _CommandLine:='"'+FProjectPathAndFilename+'"';
  RunScript(extractfilepath(FBuildTool),extractfilepath(FBuildTool)+'startlazarus.exe',_CommandLine+' --nsc',false,false);
end;

procedure TfrmApkManager.btnStartSDKManagerClick(Sender: TObject);
begin
  RunScript(FAndroidSDK,FAndroidSDK+'SDK Manager.exe','',false,true);
end;

function TfrmApkManager.CreateAPK:boolean;
begin
  if FileExistsUTF8(FAPKFilename) then begin
    if not DeleteFileUTF8(FAPKFilename) then begin
      ShowWarnMessage('Could not delete file <%s>. Please try to delete it manually.',[FAPKFilename],true);
      exit;
    end;
  end;
  RunScript(FProjectDir+'android\',FProjectDir+'android\build_debug_apk.bat','',false);
  result:=FileExistsUTF8(FAPKFilename);
  if not result then begin
    ShowWarnMessage('Could not build the Android apk <%s>. See batch file output to analyze the problem.',[FAPKFilename]);
    exit;
  end;
end;

function TfrmApkManager.RunScript(_CurrentDirectory,_Executable,_Parameters:string; _GetOutput: Boolean;_WaitOnExit:boolean=true):boolean;
var
_process: TProcess;
_Buffer: String;
_ReadCount: Integer;
_BytesAvailable: Integer;
begin
  result:=false;
  if PosEx('generate_debug_key.bat',_Executable) > 0 then MessageDlg('Remember to set password to: ' + FPassword + ' when prompted',mtInformation, [mbOK], 0);

  _process := TProcess.Create(nil);
  try
    _process.CurrentDirectory:=_CurrentDirectory;
    _process.Executable:=_Executable;
    _process.Parameters.text:=_Parameters;
    if _GetOutput then
      _process.Options := _process.Options + [poUsePipes]
    else begin
      if _WaitOnExit then _process.Options := _process.Options + [poWaitOnExit]
                     else _process.Options := _process.Options - [poWaitOnExit];
    end;
    trace('Try to run script <%s>.',[_Executable]);
    pgcInfo.ActivePage:=tbsBuildOutput;
    _process.Execute;

    while _GetOutput and
          ((_process.Running) or (_process.Output.NumBytesAvailable > 0)) and
          (not Application.terminated) do begin
      if _process.Output.NumBytesAvailable > 0 then begin
        _BytesAvailable := _process.Output.NumBytesAvailable;
        SetLength(_Buffer, _BytesAvailable);
        _ReadCount := _process.Output.Read(_Buffer[1], _BytesAvailable);
        if _ReadCount > 0 then mmoOutput.Lines.Add(_Buffer);
      end;
      Application.ProcessMessages;
    end;
    result:=(_process.ExitStatus=0);
  finally
    FreeAndNil(_process);
  end;
end;

// check if jdk,ndk,sdk and .....build.exe are available
procedure TfrmApkManager.VerifyNeeded3rdPartyTools;
begin
  ShowInfoMessage('Verify if 3rd-Party Tools are installed. Please wait...',[]);
  if (assigned(FInstalledNDKPlatforms) and (FInstalledNDKPlatforms.count=0))  then begin
    ShowWarnMessage('No NDK platform''s found.',[]);
    FNeeded3rdPartyToolsAvailable:=false;
    Screen.cursor:=crDefault;
    Enabled:=true;
    exit;
  end;
  if (assigned(FInstalledSDKBuildTools) and (FInstalledSDKBuildTools.count=0)) then begin
    ShowWarnMessage('SDK build tools not found.',[]);
    FNeeded3rdPartyToolsAvailable:=false;
    Screen.cursor:=crDefault;
    Enabled:=true;
    exit;
  end;
  if not fileexistsUTF8(FBuildTool) then begin
    ShowWarnMessage('Build tool <%s> not found.',[FBuildTool]);
    FNeeded3rdPartyToolsAvailable:=false;
    Screen.cursor:=crDefault;
    Enabled:=true;
    exit;
  end;
  if not fileexistsUTF8(extractfilepath(FBuildTool)+'startlazarus.exe') then begin
    ShowWarnMessage('App <%s> not found. Look''s like <\laz4android1.8\build.bat> did not work for some reason',[extractfilepath(FBuildTool)+'startlazarus.exe']);
    FNeeded3rdPartyToolsAvailable:=false;
    Screen.cursor:=crDefault;
    Enabled:=true;
    exit;
  end;
  if not fileexistsUTF8(FJDKPath+'bin\keytool.exe') then begin
    ShowWarnMessage('JDK keytool <%s> not found.',[FJDKPath+'bin\keytool.exe']);
    FNeeded3rdPartyToolsAvailable:=false;
    Screen.cursor:=crDefault;
    Enabled:=true;
    exit;
  end;
  if not directoryexistsUTF8(FAntPath) then begin
    ShowWarnMessage('ANT path <%s> not found.',[FAntPath]);
    FNeeded3rdPartyToolsAvailable:=false;
    Screen.cursor:=crDefault;
    Enabled:=true;
    exit;
  end;

  FNeeded3rdPartyToolsAvailable:=true;
  SetAppState(tas_ready);
  Screen.Cursor:=crDefault;
  btnStartAVDManager.enabled:=FileexistsUTF8(FAndroidSDK+'AVD Manager.exe');
  btnStartSDKManager.enabled:=FileexistsUTF8(FAndroidSDK+'SDK Manager.exe');
  btnStartIDE.enabled       :=FileexistsUTF8(extractfilepath(FBuildTool)+'startlazarus.exe');
  btnStartAdbLog.enabled    :=FileexistsUTF8(FAndroidSDK+'platform-tools\adb.exe');
end;

//copy all files with mask
//%FAndroidNDK%\toolchains\arm-linux-androideabi-4.9\prebuilt\windows\bin\arm-linux-androideabi-*.exe
// into folder
//laz4android\fpc\"cFPCVersion"\bin\i386-win32

function TfrmApkManager.CopyNDKFilesToFPC: boolean;
const
  cSearchMask='arm-linux-androideabi-*.exe';
var
i:integer;
_CompilerFiles:TStrings;
_searchfolder:string;
_sourcefilename:string;
_targetfilename:string;
begin
  result:=false;
  if not directoryexistsUTF8(FAndroidNDK) then exit;
  if not directoryexistsUTF8(extractfilepath(FBuildTool)) then exit;
  _searchfolder:=FAndroidNDK+FToolChainPath;
  if not DirectoryExistsUTF8(_searchfolder) then begin
    ShowWarnMessage('Problem in CopyNDKFilesToFPC: Could not find folder <%s>.',[_searchfolder],true);
    exit;
  end;
  _CompilerFiles:=TStringList.create;
  try
    if not AllFilesOfDrive(_searchfolder,cSearchMask,_CompilerFiles,nil) then begin
      ShowWarnMessage('Problem in CopyNDKFilesToFPC: Could not find files with searchmask <%s> in folder <%s>.',[cSearchMask,_searchfolder],true);
      exit;
    end;
    if _CompilerFiles.count=0 then begin
      ShowWarnMessage('Problem in CopyNDKFilesToFPC: Could not find any files that match search mask <%s> in folder <%s>.',[cSearchMask,_searchfolder],true);
      exit;
    end;
    for i:=0 to _CompilerFiles.count-1 do begin
      _sourcefilename:=_CompilerFiles[i];
      _targetfilename:=extractfilepath(FBuildTool)+format('fpc\%s\bin\i386-win32\',[cFPCVersion])+extractfilename(_CompilerFiles[i]);
      if not CopyFile(_sourcefilename,_targetfilename,true) then begin
        ShowWarnMessage('Could not copy <%s> to <%s>. Please try it manually.',[_sourcefilename,_targetfilename],true);
        exit;
      end;
      ShowInfoMessage('Copied <%s> to <%s>.',[_sourcefilename,_targetfilename]);
    end;
    result:=true;
  finally
    _CompilerFiles.free;
  end;
end;

// set new application state
procedure TfrmApkManager.SetAppState(_newState: TAppState);
begin
  FAppState:=_newState;
  case FAppState of
    tas_init :FireAppNotReadyEvent;
    tas_setup:FireAppSetupEvent;
    tas_ready:FireAppReadyEvent;
    tas_error:FireAppErrorEvent;
  end;
end;

procedure TfrmApkManager.FireAppReadyEvent;
begin
  if not assigned(FOnAppReadyEvent) then exit;
  FOnAppReadyEvent(self);
end;

procedure TfrmApkManager.FireAppNotReadyEvent;
begin
  if not assigned(FOnAppNotReadyEvent) then exit;
  FOnAppNotReadyEvent(self);
end;

procedure TfrmApkManager.FireAppErrorEvent;
begin
  if not assigned(FOnAppErrorEvent) then exit;
  FOnAppErrorEvent(self);
end;

procedure TfrmApkManager.FireAppSetupEvent;
begin
  if not assigned(FOnAppSetupEvent) then exit;
  FOnAppSetupEvent(self);
end;

procedure TfrmApkManager.DoAppReadyEvent(Sender: TObject);
begin
  Screen.Cursor:=crDefault;
  Enabled:=true;
  pgcMain.Enabled:=true;
  btnNewAndroidProject.Enabled:=true;
  BtnSelectProjectFolder.Enabled:=true;
  BtnAllInOne.Enabled:=true;
  ShowInfoMessage('LazToApk is ready now.',[]);
end;

procedure TfrmApkManager.DoAppNotReadyEvent(Sender: TObject);
begin
  Enabled:=false;
  Screen.Cursor:=crHourGlass;
  ShowInfoMessage('Setup application <%s>. This may take a while. Please wait...',[cVersion]);
end;

procedure TfrmApkManager.DoAppErrorEvent(Sender: TObject);
begin
  //
end;

procedure TfrmApkManager.DoAppSetupEvent(Sender: TObject);
begin
  SetupApp;
end;

procedure TfrmApkManager.ShowInfoMessage(const _msg: string;const _params:array of const);
begin
  pnlState.Font.color:=clblack;
  pnlState.Caption:=format(_msg,_params);
  pnlState.Invalidate;
  trace(_msg,_params);
end;

procedure TfrmApkManager.ShowWarnMessage(const _msg: string;const _params:array of const;const _ShowMessageBox:boolean=false);
begin
  pnlState.Font.color:=clred;
  pnlState.Caption:=format(_msg,_params);
  pnlState.Invalidate;
  trace(_msg,_params);
  if not _ShowMessageBox then exit;
  Screen.Cursor:=crDefault;
  MessageDlg(format(_msg,_Params),mtWarning, [mbOK], 0);
end;

procedure TfrmApkManager.SetupApp;
begin
  pgcMain.Enabled:=false;
  if assigned(FInstalledNDKPlatforms) then FInstalledNDKPlatforms.free;
  FInstalledNDKPlatforms:=DetectInstalledNDKPlatforms;
  if assigned(FInstalledSDKBuildTools) then FInstalledSDKBuildTools.free;
  FInstalledSDKBuildTools:=DetectInstalledSDKBuildTools;
  PrepareNDKPlatformCombobox;
  PrepareSDKBuildToolsCombobox;
  VerifyNeeded3rdPartyTools;
end;

function TfrmApkManager.InstallAPK: boolean;
begin
  result:=RunScript(FProjectDir+'android\',FProjectDir+'android\adb_install.bat','',false,true);
end;

function TfrmApkManager.UnInstallAPK: boolean;
begin
  result:=RunScript(FProjectDir+'android\',FProjectDir+'android\adb_uninstall.bat','',false,true);
end;

procedure TfrmApkManager.Trace(const _msg: string; _params: array of const);
begin
  if _msg='' then exit;
  if length(_params)>0 then mmoTrace.lines.insert(0,format(_msg,_params))
                       else mmoTrace.lines.insert(0,_msg);
end;

destructor TfrmApkManager.destroy;
begin
  if assigned(FInstalledNDKPlatforms) then FInstalledNDKPlatforms.free;
  if assigned(FInstalledSDKBuildTools) then FInstalledSDKBuildTools.free;
  inherited destroy;
end;

procedure TfrmApkManager.btnInstallAPKClick(Sender: TObject);
begin
{  OpenDialog1.InitialDir := FProjectDir + 'android\bin\';
  OpenDialog1.Title := 'Select apk';
  OpenDialog1.DefaultExt := 'apk';
  OpenDialog1.Filter := 'APK-file|*.apk';
  if not OpenDialog1.Execute then exit;
  FLastUsedSearchDir := ExtractFilePath(OpenDialog1.FileName);}
  InstallAPK;
end;

procedure TfrmApkManager.btnUnInstallAPKClick(Sender: TObject);
begin
{  OpenDialog1.InitialDir := FProjectDir + 'android\bin\';
  OpenDialog1.Title := 'Select apk';
  OpenDialog1.DefaultExt := 'apk';
  OpenDialog1.Filter := 'APK-file|*.apk';
  if not OpenDialog1.Execute then exit;
  FLastUsedSearchDir := ExtractFilePath(OpenDialog1.FileName);}
  UnInstallAPK;
end;

procedure TfrmApkManager.btnAllInOneClick(Sender: TObject);
begin
  mmoOutput.Clear;
  ShowInfoMessage('Set Project Folder...',[]);
  Application.ProcessMessages;
  PrepareProject(edtProjectFolder.Text);
  ShowInfoMessage('Build Project...',[]);
  Application.ProcessMessages;
  if not BuildProject then begin
    ShowWarnMessage('Failed to build Project <%s>. Please start the batch-file <%s> manually and analyze the output.',[FProjectName,FProjectDir + 'android\build_app.bat']);
    exit;
  end;
  if cbxCreateNewKey.checked then begin
    ShowInfoMessage('Create Key File...',[]);
    Application.ProcessMessages;
    if not CreateKey then begin
      ShowWarnMessage('Failed to build key file <%s>. Please start the batch-file <%s> manually and analyze the output.',[FKeystoreFilename,FProjectDir + 'android\generate_debug_key.bat']);
      exit;
    end;
  end;
  ShowInfoMessage('Create APK File...',[]);
  Application.ProcessMessages;
  if not CreateAPK then begin
    ShowWarnMessage('Failed to build APK <%s>. Please start the batch-file <%s> manually and analyze the output.',[FProjectName,FProjectDir + 'android\build_debug_apk.bat']);
    exit;
  end;
  if cbxAutoInstallAPK.checked then begin
    ShowInfoMessage('UnInstall APK...',[]);
    Application.ProcessMessages;
    UnInstallAPK;
    ShowInfoMessage('Install APK...',[]);
    Application.ProcessMessages;
    InstallAPK;
  end;
  pnlState.Font.color:=clBlack;
  ShowInfoMessage('Done. The app is here: <%s>.',[FAPKFilename]);
end;


procedure TfrmApkManager.btnStartAdbLogClick(Sender: TObject);
begin
  if FProcess <> nil then begin
//    FProcess.CommandLine := 'taskkill /PID ' + IntToStr(FProcess.ProcessID);
    FProcess.Executable:='taskkill';
    FProcess.Parameters.text:='/PID ' + IntToStr(FProcess.ProcessID);
    FProcess.Execute;
    FreeAndNil(FProcess);
    btnStartAdbLog.Caption := 'Start adblog';
    if rgrpADBLog.ItemIndex = 1 then MessageDlg('Logfile in: ' + ExtractFilePath(FProjectDir) + 'adb.log', mtInformation, [mbOK], 0);
    exit;
  end;
  FProcess := TProcess.Create(Self);
  FProcess.Executable:=FAndroidSDK+'platform-tools\adb';
  if rgrpDevice.ItemIndex = 0   then FProcess.Parameters.text :='-d logcat'
  else begin
    if rgrpADBLog.ItemIndex = 0 then FProcess.Parameters.text := 'logcat -d'
                                else FProcess.Parameters.text := 'logcat -f ' + ExtractFilePath(FProjectDir) + 'adb.log';
  end;
  FProcess.Execute;
  btnStartAdbLog.Caption := 'Stop adblog';
end;


function  TfrmApkManager.BuildLazarus:boolean; // copy build.bat into laz4android and start it.
var
  _lazarusPath:string;
  _sourcefilename,_targetfilename:string;
begin
  result:=false;
  _lazarusPath:=Extractfilepath(FBuildTool);
  if not DirectoryExistsUTF8(_lazarusPath) then begin
    ShowWarnMessage('The folder <%s> does not exist. Look''s like laz4android was not downloaded and unpacked.',[_lazarusPath],true);
    exit;
  end;
  if (FileExistsUTF8(_lazarusPath+'startlazarus.exe')) and
     (FileExistsUTF8(_lazarusPath+'lazarus.exe')) then begin
    result:=true;      // if this files exist already, then no rebuild is needed.
    exit;
  end;
  _sourcefilename:=ExtractFilepath(Application.ExeName)+'build.bat';
  if not FileExistsUTF8(_sourcefilename) then begin
    ShowWarnMessage('The file <%s> does not exist. Look''s like installation failed.',[_sourcefilename],true);
    exit;
  end;
  _targetfilename:=_lazarusPath+'build.bat';
  try
    if not copyfile(_sourcefilename,_targetfilename,true) then begin
      ShowWarnMessage('Problem in BuildLazarus: Could not copy the file <%s> to <%s>.',[_sourcefilename,_targetfilename],true);
      exit;
    end;
    ShowInfoMessage('Success: Copy file <%s> to <%s>.',[_sourcefilename,_targetfilename]);
    if not RunScript(extractfilepath(_targetfilename),_targetfilename,'',false,true) then begin
      ShowWarnMessage('Problem in BuildLazarus: Could not execute the file <%s>.',[_targetfilename],true);
      exit;
    end;
    ShowInfoMessage('Success: Executed the file <%s>.',[_targetfilename]);
    result:= (FileExistsUTF8(_lazarusPath+'startlazarus.exe')) and (FileExistsUTF8(_lazarusPath+'lazarus.exe'));
    ShowInfoMessage('Success: Built lazarus.',[]);
  except
    on e:exception do ShowWarnMessage('Could not copy the file <%s> to <%s>. Please check settings and user-rights. <%s>.',[_sourcefilename,_targetfilename,e.message],true);
  end;
end;


end.
