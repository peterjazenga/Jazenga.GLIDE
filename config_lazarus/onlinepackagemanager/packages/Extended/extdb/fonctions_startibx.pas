unit fonctions_startibx;

{$I ..\dlcompilers.inc}
{$I ..\extends.inc}

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  DB, IB,
  {$IFDEF VERSIONS}
  fonctions_version,
  {$ENDIF}
  fonctions_system;

resourcestring
   Gs_Charset_ibx =   'utf8';
var
   gb_ibxembed : Boolean = True;

const DEFAULT_FIREBIRD_SERVER_DIR = '/var/lib/firebird/2.5/';
      _REL_PATH_BACKUP='Backup'+DirectorySeparator;
      CST_FBLIB = 'lib';
      CST_FBCLIENT = 'fbclient';
      CST_FBEMBED = 'fbembed';
      CST_FBEMBED_SUB = CST_FBEMBED{$IFDEF CPU64}+'64'{$ENDIF};
      CST_FBCLIENT_SUB = CST_FBCLIENT{$IFDEF CPU64}+'64'{$ENDIF};
      CST_FBISQL = {$IFDEF WINDOWS}
                   'isql.exe';
                   {$ELSE}
                   'isql-fb';
                   {$ENDIF}

      CST_FBISQL_SUB = {$IFDEF WINDOWS}
                   'isql'{$IFDEF CPU64}+'64'{$ENDIF}+'.exe';
                   {$ELSE}
                   CST_FBISQL;
                   {$ENDIF}

{$IFDEF VERSIONS}
      gver_fonctions_ibx : T_Version = ( Component : 'IBX Connect package.' ;
                                         FileUnit : 'fonctions_ibx' ;
                        		 Owner : 'Matthieu Giroux' ;
                        		 Comment : 'Just add the package.' ;
                        		 BugsStory   : 'Version 1.0.1.0 : Upgrading for XML Frames.' +#10
                                                     + 'Version 1.0.0.1 : Testing IBX.' +#10
                                                     + 'Version 1.0.0.0 : IBX Version.';
                        		 UnitType : 1 ;
                        		 Major : 1 ; Minor : 0 ; Release : 1 ; Build : 0 );
{$ENDIF}
function fs_GetFirebirdExeorLib (const exeorlib_appdir,exeorlib_subdirectories: string):String;
{$IFDEF FPC}
procedure p_setLibrary (var libname: string);
{$ENDIF}
{$IFDEF UNIX}
function fs_GetDatabaseDirectory :String;
{$ENDIF}
implementation

uses IBQuery,
     IBSQL,
     IBServices,
     IBDatabase,
     IBCustomDataSet,
     fonctions_string,
     {$IFDEF FPC}
     unite_messages,
     LazFileUtils,
     {$ELSE}
     unite_messages_delphi,
     {$ENDIF}
     fonctions_ini,
     fonctions_db,
     fonctions_proprietes,
     fonctions_dbobjects,
     fonctions_file,
     fonctions_dialogs;

procedure p_ExecuteIBXAutoCommit ( const AQuery : TDataset ; const AOwner : TComponent );
Begin
  ( AQuery as TIBCustomDataSet ).Transaction := TIBTransaction.Create ( AOwner );
  ( AQuery as TIBCustomDataSet ).Transaction.DefaultDatabase := ( AQuery as TIBCustomDataSet ).Database;
  ( AQuery as TIBCustomDataSet ).Transaction.DefaultAction:=TACommit;
end;

function fb_OpenOrCloseDatabase  (  const AConnection  : TComponent ;
                                    const ab_Open : Boolean ;
                                    const ab_showError : Boolean    ):Boolean;
begin
  with AConnection as TIBDataBase do
   Begin
    if ab_Open Then
      try
       Open;
       DefaultTransaction.Active:=True;
      Except
        on e:Exception do
         if ab_showError Then
           MyShowMessage('Error:'+e.Message);
     end
    Else
     Begin
       if DefaultTransaction.Active Then
         try
          DefaultTransaction.Commit;

         Except
           DefaultTransaction.Rollback;
         end;
       try
         Close;
       except
         Destroy
       end;
     end;

     Result:=Connected;
   End;
End;


procedure p_ExecuteIBXQuery ( const adat_Dataset : TComponent  );
Begin
  if ( adat_Dataset is TIBQuery ) Then
    ( adat_Dataset as TIBQuery ).ExecSQL
  else if ( adat_Dataset is TIBSQL ) Then
    ( adat_Dataset as TIBSQL ).ExecQuery;

End ;
function fs_CreateDatabase  ( const as_base, as_user, as_password, as_host : String ):String;
Begin
  Result := 'CREATE DATABASE '''+as_base+''' USER '''+as_user+''' PASSWORD '''+as_password+''' PAGE_SIZE 16384 DEFAULT CHARACTER SET '+Gs_Charset_ibx+';'+#10
          + 'CONNECT '''+as_base+''' USER '''+as_user+''' PASSWORD '''+as_password+''';'+#10;
End;

function fs_GetFirebirdExeorLib (const exeorlib_appdir,exeorlib_subdirectories: string):String;
var APath : String;
Begin
  Result:= '';
  APath:=GetAppDir;
  if FileExistsUTF8(APath+exeorlib_appdir)
   Then Begin
         Result:= exeorlib_appdir;
         Exit;
        end
   Else while (length ( APath ) > 5) do
      Begin
        APath:=ExtractSubDir(APath);
        if FileExistsUTF8(APath+exeorlib_subdirectories) Then
          Begin
           Result:=APath+exeorlib_subdirectories;
           Exit;
          end;
      end;


end;

function fs_GetDefaultFirebirdExeorLib (const exeorlib_appdir,exeorlib_subdirectories: string):String;
Begin
  Result:=fs_GetFirebirdExeorLib(exeorlib_appdir,exeorlib_subdirectories);
  if Result='' Then
    Result:=exeorlib_appdir;
end;

{$IFDEF UNIX}
function fs_GetDatabaseDirectory :String;
Begin
  Result := DEFAULT_FIREBIRD_SERVER_DIR+'data/';
end;
{$ENDIF}

function fb_RestoreBase ( const AConnection : TComponent ;
                          const as_database, as_user, as_password, APathSave : String ;
                          const ASt_Messages : TStrings;
                          const acom_ControlMessage, acom_owner : TComponent):Boolean;
var
  DiskSize:Int64;
  sr:TSearchRec;
  TailleFichier:LongInt;
  Pos:integer;
  ibRestore:TIBRestoreService;
  IBBackup:TIBBackupService;
  FileNameGBK,PathNameGBK,BackFile,Serveur,NomBase:string;
  lecteur:char;
  Prot:TProtocol;
  lb_connected : Boolean;
begin
  if AConnection = nil
   Then lb_connected := False
   Else
    Begin
     lb_connected := fb_getComponentBoolProperty(AConnection,CST_DBPROPERTY_CONNECTED);
     p_SetComponentBoolProperty(AConnection,CST_DBPROPERTY_CONNECTED,False);
    end;
  {$IFDEF WINDOWS}
  Pos:=AnsiPos(':',as_database);
  if Pos>2 then
  {$ELSE}
  Pos:=AnsiPos(DirectorySeparator,as_database);
  if Pos <> 1 then
  {$ENDIF}
  begin
    Prot:=TCP;
    Serveur:=copy(as_database,1,Pos-1);
    NomBase:=copy(as_database,Pos+1,250);
    BackFile:='optimisation.fbk';
  end
  else
  begin
    Prot:=Local;
    Serveur:='';
    NomBase:=as_database;
    FileNameGBK:=ExtractFileNameOnly(as_database)+FormatDateTime('yymmddhh',Now);
    FileNameGBK:=ChangeFileExt(FileNameGBK,'.FBK');
{$IFDEF WINDOWS}
    if Pos=2 then
{$ELSE}
    if ( as_database <> '' )
    and ( as_database [1] = DirectorySeparator ) Then
{$ENDIF}
     begin
      lecteur:=UpperCase(APathSave)[1];
      DiskSize:=DiskFree({$IFDEF WINDOWS}ord(lecteur)-ord('A')+1{$ELSE}0{$ENDIF});
      FindFirstUTF8(NomBase, faAnyFile, sr); { *Converted from FindFirstUTF8*  }
      TailleFichier:=sr.Size;
      FindCloseUTF8(sr); { *Converted from FindCloseUTF8*  }
      if DiskSize>TailleFichier then
        PathNameGBK:=APathSave
      else
        PathNameGBK:=ExtractFilePath(NomBase)+_REL_PATH_BACKUP;
     end
      else
        PathNameGBK:=APathSave;
    if not ForceDirectoriesUTF8(PathNameGBK) then
    begin
      Result:=False;
      exit;
    end;
    BackFile:=ExcludeTrailingPathDelimiter(PathNameGBK)+DirectorySeparator+FileNameGBK;
  end;

  try
    IBBackup:=TIBBackupService.Create(acom_owner);
    with IBBackup do
    begin
      LoginPrompt:=False;
      with Params do
       Begin
        Add('user_name='+as_user);
        Add('password='+as_password);
        //Add('lc_ctype=ISO8859_1');
       end;
      Protocol:=Prot;
      ServerName:=Serveur;
      Active:=True;
      try
        Verbose:=True;
        Options:= [IgnoreLimbo];
        DatabaseName:=NomBase;
        BackupFile.Add(BackFile);
        p_SetComponentProperty(acom_ControlMessage,CST_PROPERTY_CAPTION,fs_RemplaceMsg(gs_Caption_Save_in,[BackFile]));
        ServiceStart;
        while not Eof do
          if ASt_Messages = nil
           Then GetNextLine
           Else ASt_Messages.Append(GetNextLine);
      finally
        Active:=False;
        IBBackup.Destroy;
      end;
    end;

    ibRestore:=TIBRestoreService.Create(acom_owner);
    with IBRestore do
    begin
      LoginPrompt:=False;
      Params.Add('user_name='+as_user);
      Params.Add('password='+as_password);
      Protocol:=Prot;
      ServerName:=Serveur;
      Active:=True;
      try
        Verbose:=True;
        Options:= [Replace];
        PageBuffers:=32000;
        PageSize:=4096;
        DatabaseName.Add(NomBase);
        BackupFile.Add(BackFile);
        p_SetComponentProperty(acom_ControlMessage,CST_PROPERTY_CAPTION,fs_RemplaceMsg(gs_Caption_Restore_database,[as_database]));
        ServiceStart;
        while not Eof do
          if ASt_Messages = nil
           Then GetNextLine
           Else ASt_Messages.Append(GetNextLine);
      finally
        Active:=False;
        ibRestore.Destroy;
      end;
    end;

  finally
    if lb_connected Then
      p_SetComponentBoolProperty(AConnection,CST_DBPROPERTY_CONNECTED,lb_connected);
  end;
end;

procedure p_saveFile ( const as_file : String ; const as_InsideFile : {$IFDEF DELPHI_9_UP} WideString {$ELSE} String{$ENDIF}  );
var lh_handleFile : THandle;
Begin
  if FileExistsUTF8(as_File) Then DeleteFileUTF8(as_File);
  lh_handleFile := FileCreateUTF8(as_File);
  try
    FileWriteln(lh_handleFile,as_InsideFile);
  finally
    FileClose(lh_handleFile);
  end;

end;

procedure p_ExecuteSQLCommand ( const as_SQL : {$IFDEF DELPHI_9_UP} WideString {$ELSE} String{$ENDIF}  );
var ls_exe : String;
Begin
  p_saveFile (fs_GetIniDir+'sql-firebird'+CST_EXTENSION_SQL_FILE,as_SQL);

  {$IFDEF WINDOWS}
  ls_exe := fs_GetDefaultFirebirdExeorLib(CST_FBISQL,CST_FBISQL_SUB);
  if not FileExistsUTF8(ls_exe) Then
    Begin
     MyShowMessage(fs_RemplaceMsg(GS_EXE_DO_NOT_EXISTS_EXITING,[ls_exe]));
     halt;
    end;
  ls_exe := fs_ExecuteProcess(ls_exe, ' -i '''+ fs_GetIniDir+'sql-firebird'+CST_EXTENSION_SQL_FILE
                           +''' -s 3');
  {$ELSE}
  ls_exe := fs_GetDefaultFirebirdExeorLib(CST_FBISQL,CST_FBISQL_SUB);
  if FileExistsUTF8(ls_exe)
   Then ls_exe := fs_ExecuteProcess(ls_exe, ' -i '''+ fs_GetIniDir+'sql-firebird'+CST_EXTENSION_SQL_FILE
                           +'''  -s 3')
   Else ls_exe := fs_ExecuteProcess(CST_FBISQL, ' -i '''+ fs_GetIniDir+'sql-firebird'+CST_EXTENSION_SQL_FILE
                           +'''  -s 3');
  {$ENDIF}
  if ls_exe > '' Then
    MyShowMessage('Erreur'+#10+ls_exe);
End ;

{$IFDEF FPC}
procedure p_setLibrary (var libname: string);
{$IFNDEF WINDOWS}
var LAppDir : String;
{$ENDIF}
const cst_usr_lib = '/usr/lib/';
Begin
  {$IFNDEF DARWIN}
  {$IFDEF WINDOWS}
  libname:=fs_GetFirebirdExeorLib(CST_FBEMBED+CST_EXTENSION_LIBRARY,CST_FBEMBED_SUB+CST_EXTENSION_LIBRARY);
  if not FileExistsUTF8(libname) Then
   libname:= CST_FBCLIENT+CST_EXTENSION_LIBRARY;
  {$ELSE}
  if FileExistsUTF8(GetAppDir + CST_FBLIB + CST_FBEMBED  + CST_EXTENSION_LIBRARY + '.2.5')
   Then LAppDir:=GetAppDir
   Else LAppDir:='';

  if    gb_ibxembed
  or FileExistsUTF8(GetAppDir+CST_FBLIB + CST_FBEMBED  + CST_EXTENSION_LIBRARY + '.2.5')
  or     (    ( pos ( DEFAULT_FIREBIRD_SERVER_DIR, gs_DefaultDatabase ) <> 1 )
          and ( gs_DefaultDatabase <> '' )
          and (   ( gs_DefaultDatabase [1] = '/' )
               or ( gs_DefaultDatabase [1] = '.' )))
  Then libname := LAppDir + CST_FBLIB + CST_FBEMBED  + CST_EXTENSION_LIBRARY + '.2.5' + ':' + CST_FBLIB + CST_FBEMBED  + CST_EXTENSION_LIBRARY + '.2.5.4'
  Else libname := 'libfbclient' + CST_EXTENSION_LIBRARY + '.2';
  if  FileExistsUTF8(LAppDir+'exec.sh') Then
     fs_ExecuteProcess('sh',' "'+LAppDir+'exec.sh"');
  {$ENDIF}
  {$ENDIF}
end;
{$ENDIF}
initialization
 {$IFDEF FPC}
 OnGetLibraryName:= TOnGetLibraryName( p_setLibrary);
 {$ENDIF}
 ge_OnExecuteQuery  :=TOnExecuteQuery({$IFNDEF FPC}@{$ENDIF}p_ExecuteIBXQuery);
 ge_OnSetAutoCommit  :=TOnSetQuery({$IFNDEF FPC}@{$ENDIF}p_ExecuteIBXAutoCommit);
 ge_OnOptimiseDatabase  :=TOnOptimiseDatabase({$IFNDEF FPC}@{$ENDIF}fb_RestoreBase );
 ge_OnExecuteCommand:=TOnExecuteCommand({$IFNDEF FPC}@{$ENDIF}p_ExecuteSQLCommand);
 ge_OnOpenOrCloseDatabase := TOnOpenCloseDatabase({$IFNDEF FPC}@{$ENDIF}fb_OpenOrCloseDatabase);
 {$IFDEF LINUX}
 ge_GetDatabaseDirectory :=TGetDatabaseDirectory({$IFNDEF FPC}@{$ENDIF}fs_GetDatabaseDirectory);
 {$ENDIF}
 {$IFDEF VERSIONS}
 p_ConcatVersion ( gver_fonctions_ibx );
 {$ENDIF}
end.

