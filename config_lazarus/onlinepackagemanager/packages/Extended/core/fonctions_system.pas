// unité contenant des fonctions de traitements sytèmes
unit fonctions_system;

interface

{$I ..\dlcompilers.inc}
{$I ..\extends.inc}

{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}

uses
{$IFDEF FPC}
  Process,
{$IFDEF UNIX}
  Unix,
{$ENDIF}
{$ELSE}
  Windows, ShellApi,
{$ENDIF}
  SysUtils,
{$IFDEF VERSIONS}
  fonctions_version,
{$ENDIF}
  StrUtils,
  Classes ;

type TPackageType = ( ptExe, ptTar, ptRpm, ptDeb, ptPkg, ptDmg, ptZip );
     TProcessorType = ( ptUnknown, ptIntel, ptMIPS, ptAlpha, ptPPC, ptSHX, ptARM, ptIA64, ptAlpha64 );
     TSystemDirectory = ( sdUnknown, sdHome, sdUser, sdDocuments, sdImages, sdExe, sdTmp, sdSystemRoot, sdData );
     TGetDatabaseDirectory = function:String;


const
{$IFDEF VERSIONS}
  gVer_fonction_system : T_Version = ( Component : 'System management' ; FileUnit : 'fonctions_system' ;
                        	       Owner : 'Matthieu Giroux' ;
                        	       Comment : 'System Functions, with traducing and path management.' ;
                        	       BugsStory : 'Version 1.1.2.0 : Data Directory.' + #10
                                                 + 'Version 1.1.1.0 : Creating System Directory functions.' + #10
                                                 + 'Version 1.1.0.6 : Testing FileDir.' + #10
                                                 + 'Version 1.1.0.5 : Testing Open File.' + #10
                                                 + 'Version 1.1.0.4 : Testing command line on linux and windows.' + #10
                                                 + 'Version 1.1.0.3 : Renaming to fs_getappdir.' + #10
                                                 + 'Version 1.1.0.2 : Testing p_openfileordirectory on windows.' + #10
                                                 + 'Version 1.1.0.1 : Using explorer to open files, more secure.' + #10
                                                 + 'Version 1.1.0.0 : Linux and architecture functions.' + #10
                                                 + 'Version 1.0.2.0 : fs_DocDir and library''s extension.' + #10
                                                 + 'Version 1.0.1.0 : fs_GetCorrectPath function.' + #10
                                                 + 'Version 1.0.0.0 : Creating from fonctions_string.';
                        	       UnitType : 1 ;
                        	       Major : 1 ; Minor : 1 ; Release : 1 ; Build : 0 );
{$ENDIF}
{$IFDEF DELPHI}
  DirectorySeparator = '\' ;
{$ENDIF}
  CST_PROCESSOR_TYPE : TProcessorType = {$IFDEF DELPHI}ptIntel{$ELSE}{$IFDEF CPUI386}ptIntel{$ELSE}{$IFDEF CPUx86_64}ptIntel{$ELSE}{$IFDEF CPUARM}ptARM{$ELSE}{$IFDEF CPUPOWERPC}ptPPC{$ELSE}{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF};
  CST_EXTENSION_LIBRARY = {$IFDEF WINDOWS}'.dll'{$ELSE}{$IFDEF DARWIN}'.dylib'{$ELSE}'.so'{$ENDIF}{$ENDIF};
  CST_EXTENSION_LOG_FILE = '.log';
  CST_EXTENSION_BATCH_FILE = {$IFDEF WINDOWS}'.bat'{$ELSE}'.sh'{$ENDIF};
  CST_PackageTypeString : Array [ TPackageType ] of String = ( 'exe', 'tar.gz', 'rpm', 'deb', 'pkg', 'dmg', 'zip' );
  CST_ProcessorTypeString : Array [ TProcessorType ] of String = ( '?', 'Intel', 'MIPS', 'Alpha', 'PPC', 'SHX', 'ARM', 'IA64', 'Alpha64');
  CST_EXTENSION_SCRIPT            = {$IFDEF WINDOWS}'.bat'{$ELSE}'.sh'{$ENDIF} ;
  ge_GetDatabaseDirectory : TGetDatabaseDirectory = nil;
  CST_SUBDIR_IMAGES_SOFT = 'Images'+DirectorySeparator;

var
  GS_SUBDIR_IMAGES_SOFT : String = '' ;

{$IFDEF UNIX}
const
     CST_USER_BIN = '/usr/bin/';
     CST_VAR_LIB = '/var/lib/';
{$ENDIF}
type TArchitectureType = ( at32, at64 );
const  gat_ArchitectureType : TArchitectureType = {$IFDEF CPU64}at64{$ELSE}at32{$ENDIF};

function ExtractSubDir ( const as_FilePath : String ) :String;
function ExtractDirName ( const as_FilePath : String ) :String;
procedure p_OpenFileOrDirectory ( const AFilePath : String );
function fs_GetSoftName : String;
// Retourne le nom d'utilisateur (string) de la session WINDOWS
function fs_GetUserSession: string;
function fs_GetCorrectPath ( const as_Path :String ): string;

// Retourne le nom d'ordinateur (string)
function fs_GetComputerName: string;
function GetAppDir : String;
function GetImagesDir: string;
function GetRootDir: string;
function GetHomeDir: string;
function GetDocDir: string;
function GetDataDir (const as_DataDirectory : String = '') :String;
function GetDirectory(const ASystemDirectory : TSystemDirectory ): string;
function fs_getImagesSoftDir:String;
function fs_GetPackagesExtension : String;
function fpt_GetPackagesType ( const ab_NotInstalled : boolean = False ) : TPackageType;
{$IFDEF WINDOWS}
function GetWinDir ( const CSIDL : Integer ) : String ;
function ExtractFileNameOnly ( const as_Path : String ): String;
{$ELSE}
{$IFDEF UNIX}
{$ENDIF}
{$ENDIF}
function fs_GetArchitecture : String;
function fs_GetFullArchitecture : String;
function fat_GetArchitectureType : TArchitectureType;
function fs_ExecuteProcess ( const AExecutable : String; const AParameter : String = '' ; const HasOutput : Boolean = True{$IFDEF FPC}; const AOptions : TProcessOptions = [poWaitOnExit]{$ENDIF}):String;
{$IFNDEF FPC}
function GetTempDir: string;
function GetAppConfigDir ( const Global : Boolean ): string;
function GetUserDir: string;
function DirectoryExistsUTF8 ( const as_path : String ):Boolean;
function FileExistsUTF8 ( const as_path : String ):Boolean;
function FindFirstUTF8(const Path: string; Attr: Integer;
  var F: TSearchRec): Integer;
procedure FindCloseUTF8(var F: TSearchRec);
function FileOpenUTF8(const FileName: string; Mode: LongWord): Integer;
function FindNextUTF8(var SR: TSearchRec):integer;
procedure RemoveDirUTF8 ( const as_dir : String );
function ForceDirectoriesUTF8 ( const as_dir : String ):Boolean;
procedure CreateDirUTF8 ( const as_dir : String );
procedure DeleteFileUTF8 ( const as_File : String );
function FileCreateUTF8(const as_file: String):integer;
function FileSize(const as_file: String):int64;
function FileSizeUTF8(const as_file: String):int64;
{$ELSE}
function ExtractFileDir ( const as_FilePath : String ) :String;
{$ENDIF}
function fs_EraseNameSoft ( const as_Nomapp, as_Path : String ) : String ;
function fs_WithoutFirstDirectory ( const as_Path : String ) :String;
function fi_TailleFichier(NomFichier:String):Int64;

implementation


uses
{$IFDEF FPC}
  LazFileUtils, UTF8Process,
{$ENDIF}
{$IFDEF WINDOWS}
  ShlObj,
{$ENDIF}
{$IFDEF DELPHI}
  SHFolder,
{$ENDIF}
  fonctions_string;

{$IFNDEF LINUX}
const ENV_VARIABLE_ARCHITECTURE = {$IFDEF WINDOWS}'PROCESSOR_ARCHITECTURE'{$ELSE}'MACHTYPE'{$ENDIF};
      {$IFDEF WINDOWS}
      ENV_VARIABLE_OS = 'OS';
      {$ENDIF}
{$ENDIF}

{$IFDEF UNIX}
const UNIX_UNAME = 'uname';
      UNIX_ARCHITECTURE = '-m';
      UNIX_FULL_ARCHITECTURE = '-omrv';
      UNIX_PACKAGES     = 'lsb_release -si';
      UNIX_VERSION      = 'lsb_release -sr';
{$ENDIF}
{$IFDEF WINDOWS}
const
    WCSIDL_DESKTOP                   = $0000;        // <desktop>
    WCSIDL_PROGRAMS                  = $0002;        // Start Menu\Programs
    WCSIDL_PERSONAL                  = $0005;        // My Documents
    WCSIDL_FAVORITES                 = $0006;        // <user name>\Favorites
    WCSIDL_MYDOCUMENTS               = $000c;        // logical "My Documents" desktop icon
    WCSIDL_MYMUSIC                   = $000d;        // "My Music" folder
    WCSIDL_MYVIDEO                   = $000e;        // "My Videos" folder
    WCSIDL_APPDATA                   = $001a;        // <user name>\Application Data
    WCSIDL_TEMPLATES                 = $0015; { %USERPROFILE%\Templates                                          }
    WCSIDL_LOCAL_APPDATA             = $001c;        // <user name>\Local Settings\Applicaiton Data (non roaming)
    WCSIDL_COMMON_APPDATA            = $0023;        // All Users\Application Data
    WCSIDL_SYSTEM                    = $0025;        // GetSystemDirectory()
    WCSIDL_MYPICTURES                = $0027;        // C:\Program Files\My Pictures
    WCSIDL_SYSTEMX86                 = $0029;        // x86 system directory on RISC
    WCSIDL_COMMON_DOCUMENTS          = $002e;        // All Users\Documents
    WCSIDL_COMMON_MUSIC              = $0035;        // All Users\My Music
    WCSIDL_COMMON_PICTURES           = $0036;        // All Users\My Pictures
    WCSIDL_COMMON_VIDEO              = $0037;        // All Users\My Video
{$ENDIF}

{$IFNDEF FPC}
  ////////////////////////////////////////////////////////////////////////
 // DELPHI Functions                                                   //
////////////////////////////////////////////////////////////////////////

// delphi ini config session directory
function GetAppConfigDir ( const Global : Boolean ): string;
 begin
   if Global
    Then Result := GetWinDir ( WCSIDL_COMMON_APPDATA )
    Else Result := GetWinDir ( WCSIDL_APPDATA );
   Result := Result + DirectorySeparator + ExtractFileNameOnly ( ParamStr(0) );
 end;

// delphi user directory
function GetUserDir: string;
 begin
   Result := GetWinDir ( WCSIDL_PERSONAL ) + DirectorySeparator;
 end;

// no DirectoryExistsUTF8 on delphi
function DirectoryExistsUTF8 ( const as_path : String ):Boolean;
Begin
  Result:= DirectoryExists ( as_path );
End;

function GetTempDir: string;
Begin
  Result := GetWindir ( WCSIDL_TEMPLATES );
End;
// no FileExistsUTF8 on delphi
function FileExistsUTF8 ( const as_path : String ):Boolean;
Begin
  Result:= FileExists ( as_path );
End;
function FindFirstUTF8(const Path: string; Attr: Integer;
  var F: TSearchRec): Integer;
Begin
  Result := FindFirst( Path, Attr, F );
End;
procedure FindCloseUTF8(var F: TSearchRec);
Begin
  FindClose(F);
End;
function FileOpenUTF8(const FileName: string; Mode: LongWord): Integer;
Begin
  Result := FileOpen(FileName,Mode);
End;
function FindNextUTF8(var SR: TSearchRec):integer;
Begin
  Result := FindNext(SR);
End;
function FileCreateUTF8(const as_file: String):integer;
Begin
  Result := FileCreate(as_file);
End;
function FileSize(const as_file: String):int64;
var FileHandle : THandle;
Begin
  Result := 0;
  if not FileExists ( as_file ) then
    Exit;
  try
    FileHandle := CreateFile(PChar(as_file),
        GENERIC_READ,
        0, {exclusive}
        nil, {security}
        OPEN_EXISTING,
        FILE_ATTRIBUTE_NORMAL,
        0);
     Result := GetFileSize(FileHandle, nil);
   finally
     CloseHandle(FileHandle);
   End;
End;
function FileSizeUTF8(const as_file: String):int64;
var FileHandle : THandle;
Begin
  Result := 0;
  if not FileExistsUTF8 ( as_file ) then
    Exit;
  try
    FileHandle := CreateFile(PChar(as_file),
        GENERIC_READ,
        0, {exclusive}
        nil, {security}
        OPEN_EXISTING,
        FILE_ATTRIBUTE_NORMAL,
        0);
     Result := GetFileSize(FileHandle, nil);
   finally
     CloseHandle(FileHandle);
   End;
End;
procedure RemoveDirUTF8 ( const as_dir : String );
Begin
  RemoveDir(as_dir);
End;
function ForceDirectoriesUTF8 ( const as_dir : String ):Boolean;
Begin
  Result := ForceDirectories(as_dir);
End;
procedure CreateDirUTF8 ( const as_dir : String );
Begin
  CreateDir(as_dir);
End;
procedure DeleteFileUTF8 ( const as_File : String );
Begin
  DeleteFile(as_File);
End;
{$ENDIF}

function GetDataDir (const as_DataDirectory : String = '') :String;
Begin
  if assigned ( ge_GetDatabaseDirectory )
   Then
    Begin
     Result:=ge_GetDatabaseDirectory;
     if Result>'' Then Exit;
    end;
  if ( pos ( GetUserDir, as_DataDirectory ) > 0 )
  or ( pos ( GetAppConfigDir ( True ), as_DataDirectory ) > 0 )
   Then Result := GetAppDir
   Else Result := GetAppConfigDir ( True ) ;
End;

  ////////////////////////////////////////////////////////////////////////
 // Every OS Functions                                                 //
////////////////////////////////////////////////////////////////////////

function GetDirectory(const ASystemDirectory : TSystemDirectory ): string;
Begin
  case ASystemDirectory of
   sdDocuments  : Result:=GetDocDir;
   sdImages     : Result:=GetImagesDir;
   sdHome       : Result:=GetHomeDir;
   sdUser       : Result:=GetUserDir;
   sdData       : Result:=GetDataDir;
   sdExe        : Result:=GetAppDir;
   sdSystemRoot : Result:=GetRootDir;
   sdTmp        : Result:=GetTempDir;
  else
    Result:='';
  end;
end;

// application directory with Separator
function GetAppDir : String;
Begin
  Result := ExtractFileDir( ParamStr(0) ){$IFDEF DELPHI}+DirectorySeparator{$ENDIF} ;
End;

// Erase first Directory with Separator
function fs_WithoutFirstDirectory ( const as_Path : String ) :String;
Begin
 Result := copy ( as_Path, pos ( DirectorySeparator, as_Path ) + 1, length ( as_Path ) - pos ( DirectorySeparator, as_Path ));
end;

// Universal Images directory  with Separator, for Leonardi
function fs_getImagesSoftDir:String;
Begin
  if GS_SUBDIR_IMAGES_SOFT = ''
   Then GS_SUBDIR_IMAGES_SOFT := GetAppDir+CST_SUBDIR_IMAGES_SOFT;
  Result := GS_SUBDIR_IMAGES_SOFT;
End;

// Can change part of Directory to get universal file system
function fs_GetCorrectPath ( const as_Path :String ): string;
Begin
  {$IFNDEF FPC}
  Result := fs_RemplaceChar(as_Path,'/',DirectorySeparator);
  {$ELSE}
  {$IFDEF WINDOWS}
  Result := fs_RemplaceChar(as_Path,'/',DirectorySeparator);
  {$ELSE}
  Result := fs_RemplaceChar(as_Path,'\',DirectorySeparator);
  {$ENDIF}
  {$ENDIF}

end;
// document directory with Separator
// Problem for Unix
function GetDocDir: string;
Begin
  {$IFDEF WINDOWS}
  Result := GetWindir ( WCSIDL_PERSONAL );
  {$ELSE}
  Result := GetUserDir + 'Documents'+DirectorySeparator;
  {$ENDIF}

end;

// document directory with Separator
// Problem for Unix
function GetHomeDir: string;
Begin
  {$IFDEF WINDOWS}
  Result := GetWindir ( WCSIDL_PERSONAL );
  {$ELSE}
  Result := DirectorySeparator+'home'+DirectorySeparator;
  {$ENDIF}

end;

// images directory with Separator
// Problem for Unix
function GetImagesDir: string;
Begin
  {$IFDEF WINDOWS}
  Result := GetWindir ( WCSIDL_MYPICTURES );
  {$ELSE}
  Result := GetUserDir + 'Images'+DirectorySeparator;
  {$ENDIF}

end;

// root directory  with Separator
// Problem for Unix
function GetRootDir: string;
Begin
  {$IFDEF WINDOWS}
  Result := GetWindir ( WCSIDL_SYSTEM );
  {$ELSE}
  Result := '/';
  {$ENDIF}

end;

// Supprime le nom du fichier exe dans le chemin
function fs_EraseNameSoft ( const as_Nomapp, as_Path : String ) : String ;
Begin
  if pos ( LowerCase(as_Nomapp), LowerCase(as_Path) )=1
   then Result := copy ( as_Path,  length ( as_NomApp ) + 2, length ( as_Path ) - length (as_Nomapp)- 1)
   else Result := as_Path ;
End;



////////////////////////////////////////////////////////////////////////////////
// Lit le nom de la session
// universal ini session
////////////////////////////////////////////////////////////////////////////////
function fs_GetUserSession: string;
{$IFDEF FPC}
Begin
 Result := 'config';
{$ELSE}
var
  Buffer: array[0..255] of char;    // tableau de 255 caracteres
  BufferSize: DWORD;                // nombre 16 bits non signé  VL_B_Resultat : Boolean;
begin
  BufferSize := sizeOf(Buffer); // (= 256)
  if GetUserName(@buffer, BufferSize) then ; // (lpBuffer: PChar; var nSize: DWORD)
  result := Buffer; // MEP utilisateur
{$ENDIF}
end;

////////////////////////////////////////////////////////////////////////////////
// Lit le nom de la machine
// not working on fpc (don't worry)
////////////////////////////////////////////////////////////////////////////////
function fs_GetComputerName : string;
{$IFDEF FPC}
Begin
 //    Result := GetComputerName;
 Result := '';
{$ELSE}
var
  Buffer: array[0..255] of char;    // tableau de 255 caracteres
  BufferSize: DWORD;                // nombre 16 bits non signé  VL_B_Resultat : Boolean;
begin
  BufferSize := sizeOf(Buffer); // (= 256)
  if GetComputerName(@buffer, BufferSize) then ; // (lpBuffer: PChar; var nSize: DWORD)
  result := Buffer; // MEP utilisateur
{$ENDIF}
end;

// filename with no extension ( forced )
function ExtractFileNameOnly ( const as_Path : String ): String;
Begin
  Result := ExtractFileName(as_path);
  Result :=copy ( Result, 1 , length ( Result ) - length( ExtractFileExt(Result)));
End;

{$IFDEF WINDOWS}
// DELPHI and WINDOWS function
// used in this unit
function GetWinDir ( const CSIDL : Integer ) : string;
var
  path: array[0..Max_Path] of Char;
begin
  ShGetSpecialFolderPath(0, path, CSIDL, False) ;
  Result := Path;
end;
{$ELSE}
{$IFDEF UNIX}
{$ENDIF}
{$ENDIF}
// Packaging functions for Unix systems
function fs_GetPackagesExtension : String;
Begin
  Result:='.'+CST_PackageTypeString[fpt_GetPackagesType];
End;
function fpt_GetPackagesType ( const ab_NotInstalled : boolean = False ) : TPackageType;
Begin
  {$IFDEF WINDOWS}
  if ab_NotInstalled
   Then Result:=ptZip
   Else Result:=ptExe;
  {$ELSE}
  {$IFDEF MACOSX}
  Result:=ptDmg;
  {$ELSE}
  Result:=ptTar;
  if ab_NotInstalled Then Exit;
       if FileExists     (CST_VAR_LIB + 'dpkg/status'  ) Then Result:=ptDeb
  else if FileExists     (CST_VAR_LIB + 'rpm/Packages' ) Then Result:=ptRpm
  else if DirectoryExists(CST_VAR_LIB + 'pkgconfig'    ) Then Result:=ptPkg
  {$ENDIF}
  {$ENDIF}
End;

// universal name of exe with no extension ( for leonardi )
function fs_GetSoftName : String;
var li_Pos : Integer;
Begin
  Result := ExtractFileName(ParamStr(0));
  li_Pos := Pos ( '.', Result );
  if ( li_Pos > 0 ) then
    Begin
      while PosEx ( '.', Result, li_Pos + 1 )> 0 do
        li_Pos := PosEx ( '.', Result, li_Pos + 1 );
      Result := Copy ( Result, 1, PosEx ( '.', Result, li_Pos )-1 );
    End;
End;

// file size
function fi_TailleFichier(NomFichier:String):Int64;
var
  F:TSearchRec;
  R:Integer;
begin
  R:=FindFirst(NomFichier,faAnyFile,F);
  if R=0 then
    Result:=F.Size
  else
    Result:=0;
  FindClose(F);
end;

// dos or unix process executing
function fs_ExecuteProcess ( const AExecutable : String; const AParameter : String = '' ; const HasOutput : Boolean = True{$IFDEF FPC}; const AOptions : TProcessOptions = [poWaitOnExit]{$ENDIF}):String;
{$IFNDEF FPC}
const
     ReadBuffer = 2400;
{$ENDIF}
var {$IFDEF FPC}
    Process : TProcess;
    {$ELSE}
    Security : TSecurityAttributes;
    ReadPipe,WritePipe : THandle;
    start : TStartUpInfo;
    ProcessInfo : TProcessInformation;

    Buffer : PAnsichar;
    DosApp : String;
    BytesRead : DWord;
    Apprunning : DWord;
    {$ENDIF}
    lList: TStringList;
begin
{$IFDEF FPC}
  Result := '';
{$IFNDEF VER2_6}       // will be introduced in 2.7+
{$IFDEF UseTProcessAlias}
  Process := TProcessUTF8.Create(nil);
{$ELSE}
  Process := TProcess.Create(nil);
{$ENDIF}
{$ELSE}
  Process := TProcessUTF8.Create(nil);
{$ENDIF}
  if HasOutput Then
    lList := TStringList.create;
  with Process do
    try
      if HasOutput // get the result
        Then Options := Options + AOptions+[poNoConsole, poStderrToOutPut,poUsePipes]
        Else Options := Options + AOptions; // wait for result
      {$IFDEF WIeNDOWS}
      Executable      := AExecutable;
      if AParameter > '' Then
        Parameters.Add(AParameter);
      {$ELSE}
      // parameters property not read on linux and windows
      CommandLine:=AExecutable+' '+AParameter;
      {$ENDIF}
      Execute; // will the command work ?
      if HasOutput Then
       Begin // result
        lList.LoadFromStream(Output);
        Result := lList.Text;
       end;
    finally
      Destroy;
      if HasOutput Then
        lList.Destroy;
    end;
{$ELSE}
  if HasOutput then
   Begin
    DosApp:=AExecutable + ' ' + AParameter;
    With Security do begin
      nlength := SizeOf(TSecurityAttributes) ;
      binherithandle := true;
      lpsecuritydescriptor := nil;
     end;
     if Createpipe (ReadPipe, WritePipe,
                    @Security, 0) then begin
      Buffer := AllocMem(ReadBuffer + 1) ;
      FillChar(Start,Sizeof(Start),#0) ;
      start.cb := SizeOf(start) ;
      start.hStdOutput := WritePipe;
      start.hStdInput := ReadPipe;
      start.dwFlags := STARTF_USESTDHANDLES + STARTF_USESHOWWINDOW;
      start.wShowWindow := SW_HIDE;

      if CreateProcess(nil,
             PChar(DosApp),
             @Security,
             @Security,
             true,
             NORMAL_PRIORITY_CLASS,
             nil,
             nil,
             start,
             ProcessInfo)
      then
      begin
       repeat
        Apprunning := WaitForSingleObject
                     (ProcessInfo.hProcess,100) ;
       until (Apprunning <> WAIT_TIMEOUT) ;
        Repeat
          BytesRead := 0;
          ReadFile(ReadPipe,Buffer[0], ReadBuffer,BytesRead,nil) ;
          Buffer[BytesRead]:= #0;
          {$IFNDEF WIDECHAR}
          OemToAnsi(Buffer,Buffer) ;
          {$ENDIF}
          Result := String(Buffer) ;
        until (BytesRead < ReadBuffer) ;
     end;
     FreeMem(Buffer) ;
     CloseHandle(ProcessInfo.hProcess) ;
     CloseHandle(ProcessInfo.hThread) ;
     CloseHandle(ReadPipe) ;
     CloseHandle(WritePipe) ;
     end;
   End
  Else
   ShellExecute(0,PChar(AExecutable), PChar(Aparameter), nil, nil, SW_SHOWNORMAL) ;
{$ENDIF}
End;

// architecture info
function fs_GetArchitecture : String;
Begin
  Result := {$IFDEF UNIX}
            fs_ExecuteProcess ( UNIX_UNAME, UNIX_ARCHITECTURE );
            {$ELSE}
            GetEnvironmentVariable(ENV_VARIABLE_ARCHITECTURE);
            {$ENDIF}
End;

// architecture info
function fs_GetFullArchitecture : String;
Begin
  Result := {$IFDEF LINUX}
            fs_ExecuteProcess ( UNIX_UNAME, UNIX_FULL_ARCHITECTURE, False );
            {$ELSE}
            GetEnvironmentVariable(ENV_VARIABLE_OS)+' '+GetEnvironmentVariable(ENV_VARIABLE_ARCHITECTURE);
            {$ENDIF}
End;

// 32 or 64 bits architecture
function fat_GetArchitectureType : TArchitectureType;
var AString : String;
Begin
  AString:=fs_GetArchitecture;
  if pos ( '64', AString ) > 0
   Then Result := at64
   ELse Result := gat_ArchitectureType;
End;

// Universal file or directory open
procedure p_OpenFileOrDirectory ( const AFilePath : String );
Begin
{$IFNDEF FPC}
  ShellExecute(0,'open', PChar(AFilePath), nil, nil, SW_SHOWNORMAL) ;
{$ELSE}
  fs_ExecuteProcess (
      {$IFDEF LINUX}
      {$IFDEF LCLgtk}
      'gnome-open'
      {$ELSE}
        {$IFDEF LCLgtk2}
        'gnome-open'
        {$ELSE}
          {$IFDEF LCLgtk3}
          'gnome-open'
          {$ELSE}
        'xdg-open'
        {$ENDIF}
        {$ENDIF}
      {$ENDIF}
      {$ELSE}
      'open'
      {$ENDIF},
      AFilePath,
      False, []);
{$ENDIF}
End;



// function ExtractSubDir
// optimised SubDir Extracting
function ExtractSubDir ( const as_FilePath : String ) :String;
var lpch_Pos : PChar;
    lp_pointer : Pointer;
Begin
  Result := as_FilePath;
  if Result = '' Then
    Exit;
  lpch_Pos := @Result [ length ( Result )-1];
  lp_pointer := @Result [ 1 ];
  while ( lpch_Pos > lp_pointer ) do
    Begin
      if lpch_Pos^ = DirectorySeparator Then
        Break;
      dec ( lpch_Pos );
    End;
  Result:=copy(Result,1,lpch_Pos- lp_pointer+1);
End;

// function ExtractSubDir
// optimised SubDir Extracting
function ExtractDirName ( const as_FilePath : String ) :String;
var lpch_Pos : PChar;
    lp_pointer : Pointer;
Begin
  Result := as_FilePath;
  if Result = '' Then
    Exit;
  lpch_Pos := @Result [ length ( Result )-1];
  lp_pointer := @Result [ 1 ];
  while ( lpch_Pos > lp_pointer ) do
    Begin
      if lpch_Pos^ = DirectorySeparator Then
        Break;
      dec ( lpch_Pos );
    End;
  Result:=copy(Result,lpch_Pos- lp_pointer+2,@Result [ length ( Result )]-lpch_Pos-1);
End;


{$IFDEF FPC}
function ExtractFileDir ( const as_FilePath : String ) :String;
Begin
  Result := as_FilePath;
  while not DirectoryExistsUTF8(Result)
  and ( length(Result) > {$IFDEF WINDOWS}3{$ELSE}1{$ENDIF} ) do
   Result:=ExtractSubDir(Result);
End;
{$ENDIF}

{$IFDEF VERSIONS}
initialization
  // adding optional version infos
  p_ConcatVersion ( gVer_fonction_system );
{$ENDIF}
end.

