unit fonctions_file;

interface


{$I ..\DLCompilers.inc}
{$I ..\extends.inc}

uses
  {$IFDEF WINDOWS}
  {$IFNDEF FPC}
   Windows,
  {$ELSE}
   LCLType,
  {$ENDIF}
  {$ENDIF}
  Classes,
{$IFDEF VERSIONS}
  fonctions_version,
{$ENDIF}
  SysUtils;

{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}

const
{$IFDEF VERSIONS}
  gVer_fonctions_file : T_Version = ( Component : 'Gestion des fichiers' ; FileUnit : 'fonctions_erreurs' ;
           Owner : '' ;
           Comment : 'Fonctions de gestion de fichiers' ;
           BugsStory :
           'Version 1.0.7.0 : Renaming fb_eraseDir to DeleteDirUTF8';
           'Version 1.0.6.0 : Creating fs_getBackupFileName';
           'Version 1.0.5.0 : Adding FileWriteln and FileCreateUTF8File.';
           'Version 1.0.4.0 : Adding FileReadln and FileCreateDeleteUTF8.';
           'Version 1.0.3.0 : Adding ExtractDirName and ExtractSubDir.';
           'Version 1.0.2.0 : UTF8 not tested.';
           'Version 1.0.1.0 : adding Windows drive verifying function.';
           'Version 1.0.0.0 : La gestion est en place, ne gérant pas tout.';
           UnitType : 1 ;
                     Major : 1 ; Minor : 0 ; Release : 7 ; Build : 0 );
{$ENDIF}
  CST_COPYFILES_ERROR_IS_READONLY = faReadOnly ;
  CST_COPYFILES_ERROR_UNKNOWN = -1 ;
  CST_COPYFILES_ERROR_IS_DIRECTORY = faDirectory ;
  CST_COPYFILES_ERROR_IS_FILE = 1 ;
  CST_COPYFILES_ERROR_DIRECTORY_CREATE = 2 ;
  CST_COPYFILES_ERROR_CANT_COPY = 3 ;
  CST_COPYFILES_ERROR_CANT_READ = 4 ;
  CST_COPYFILES_ERROR_CANT_CREATE = 5 ;
  CST_COPYFILES_ERROR_CANT_APPEND = 6 ;
  CST_COPYFILES_ERROR_FILE_DELETE = 7 ;
  CST_COPYFILES_ERROR_PARTIAL_COPY = 8 ;
  CST_COPYFILES_ERROR_PARTIAL_COPY_SEEK = 9 ;
  CST_COPYFILES_ERROR_CANT_CHANGE_DATE = 10 ;

type TDeleteDirOptions = ( ddoDeleteAll, ddoKeepDirOnly, ddoDeleteFirstFiles );

function fb_EraseFiles(  as_StartDir : String ):Boolean;
function FileCreateDeleteUTF8 ( const as_filename : String ) :THandle;
function FileCreateDeleteFile ( const as_filename : String ) :THandle;
function FileCreateDeleteUTF8File ( const as_filename : String ) :THandle;
function FileCreateUTF8File ( const as_filename : String ) :THandle;
Function FileReadln (Handle : THandle; var Buffer : String) : Longint;
function FileWriteln(const AFile : THandle; const as_chaine : String = '' ):Longint;
function FileWriteString(const AFile : THandle; const as_chaine : String; const ab_addAtEnd : Boolean = False; const ab_toadd : Byte = 0 ):Longint;
function DirSize( const as_Dir : String ):Int64;
function DeleteDirUTF8(  as_StartDir : String ; const ab_EraseOptions : TDeleteDirOptions = ddoDeleteFirstFiles ):Boolean;
function  fb_FindFiles( const astl_FilesList: TStrings; as_StartDir : String;
                        const ab_ListFiles : Boolean = True ; const ab_ListDirs : Boolean = True;
                        const ab_FullPath : Boolean = False;
                        const as_FileMask: String='*'; const as_DirMask: String='*'):Boolean;
Function fb_CopyFile ( const as_Source, as_Destination : String ; const ab_AppendFile : Boolean ; const ab_CreateBackup : Boolean = False ):Integer;
function fb_CreateDirectoryStructure ( const as_DirectoryToCreate : String ) : Boolean ;
function fb_IsFullPath ( const ASPath : String ):Boolean;
procedure p_FileNameDivision ( const as_FileNameWithExtension : String ; var as_FileName, as_Extension : String );
procedure p_LoadStrings ( const astl_StringList : TStrings; const as_FilePath,  as_message : String );
procedure p_SaveStrings ( const astl_StringList : TStrings; const as_FilePath,  as_message : String );
function fs_GetBackupFileName ( const as_File,as_extension : String;  const as_Namebackup : String=''):String;
{$IFDEF WINDOWS}
function fs_verifyAndReplaceDriveLetter ( const as_path : String ):String;
{$ENDIF}

implementation

uses StrUtils, Dialogs,
  {$IFDEF FPC}
    lazutf8classes,FileUtil,
    LConvEncoding,
  {$ELSE}
    fonctions_system, WideStrUtils,
  {$ENDIF}
    fonctions_string,
    Forms ;

function fs_GetBackupFileName ( const as_File,as_extension : String;  const as_Namebackup : String=''):String;
var i:Word;
Begin
  if not FileExistsUTF8 (as_File+as_Extension )
   Then
    Begin
     Result:=as_File+as_Extension;
     Exit;
    End;
  i:=2;
  while FileExistsUTF8 ( as_File+as_Namebackup+inttostr(i)+as_extension ) do
   inc ( i );
  Result:= as_File+as_Namebackup+inttostr(i)+as_extension;
End;

procedure DirSizeRecurse(  as_Dir : String; var ai64_size : Int64);
var lstl_Files : TStringList;
    ls_file : string;
    {$IFNDEF FPC}
    FileHandle: THandle;
    {$ENDIF}
Begin
  as_Dir := IncludeTrailingPathDelimiter(as_Dir);
  lstl_Files:=TStringList.Create;
  try
    fb_FindFiles(lstl_Files,as_Dir,True,True);
    with lstl_Files do
    while Count>0 do
     Begin
       ls_file:=Strings[0];
       Delete(0);
       if DirectoryExistsUTF8(as_Dir+ls_file)
        Then DirSizeRecurse(as_Dir+ls_file, ai64_size)
        Else
          inc ( ai64_size, FileSize(as_Dir+ls_file));
     end;
  finally
    lstl_Files.Destroy;
  end;
end;

function DirSize( const as_Dir : String ):Int64;
Begin
  Result:=0;
  DirSizeRecurse(as_Dir,Result);
end;

procedure p_SetStartDir ( var as_StartDir : String );
Begin
  if as_StartDir[length(as_StartDir)] <> DirectorySeparator
   then
    as_StartDir := as_StartDir + DirectorySeparator;

end;

// Recursive procedure to build a list of files
function  fb_FindFiles( const astl_FilesList: TStrings; as_StartDir : String;
                        const ab_ListFiles : Boolean = True ; const ab_ListDirs : Boolean = True;
                        const ab_FullPath : Boolean = False;
                        const as_FileMask: String='*'; const as_DirMask: String='*'):Boolean;
var
  SR: TSearchRec;
  IsFound: Boolean;
  ls_Path : String;
begin
  Result := False ;

  p_SetStartDir ( as_StartDir );

  if ab_FullPath
   Then ls_Path := as_StartDir
   Else ls_Path := '' ;

  { Build a list of the files in directory as_StartDir
     (not the directories!)                         }
  if ab_ListDirs Then
  try
    IsFound := FindFirstUTF8(as_StartDir + as_DirMask, faDirectory, SR) = 0 ;
    while IsFound do
     begin
      with SR do
      if (( Name > '' ) and ( Name <> '.' ) and ( Name <> DirectorySeparator ) and ( Name <> '..' ))
      and DirectoryExistsUTF8 ( as_StartDir + Name )
       then astl_FilesList.Add(ls_Path + Name);
      IsFound := FindNextUTF8(SR) = 0;
      Result := True ;
    end;
    FindCloseUTF8(SR);
  Except
    FindCloseUTF8(SR);
  End ;
  if ab_ListFiles Then
  try
    IsFound := FindFirstUTF8(as_StartDir+as_FileMask, faAnyFile-faDirectory, SR) = 0;
    while IsFound do
     begin
        with SR do
        if (( Name > '' ) and ( Name <> DirectorySeparator ))
         and  FileExistsUTF8 ( as_StartDir + Name )
         Then
          astl_FilesList.Add(ls_Path + Name);
        IsFound := FindNextUTF8(SR) = 0;
        Result := True ;
      end;
    FindCloseUTF8(SR);
  Except
    FindCloseUTF8(SR);
  End ;

end;

function fb_EraseSubDirs( const as_StartDir : String  ):Boolean;
var
  SR: TSearchRec;
  IsFound: Boolean;
  as_subdir : String;
begin
  Result := fb_EraseFiles ( as_StartDir );

  { Build a list of the files in directory as_StartDir
     (not the directories!)                         }
  try
    IsFound := FindFirstUTF8(as_StartDir + '*', faDirectory, SR) = 0 ;
    while IsFound do
     begin
      if (( SR.Name <> '.' ) and ( SR.Name <> '..' ))
      and DirectoryExistsUTF8 ( as_StartDir + SR.Name )
       then
        Begin
          as_subdir:= as_StartDir + SR.Name+DirectorySeparator;
          fb_EraseSubDirs(as_subdir);
          RemoveDirUTF8(as_subdir);
        End ;
      IsFound := FindNextUTF8(SR) = 0;
      Result := True ;
    end;
  Finally
    FindCloseUTF8(SR);
  End ;
end;

function DeleteDirUTF8(  as_StartDir : String ; const ab_EraseOptions : TDeleteDirOptions = ddoDeleteFirstFiles ):Boolean;
begin
  p_SetStartDir ( as_StartDir );
  Result := fb_EraseFiles ( as_StartDir );

  { Build a list of the files in directory as_StartDir
     (not the directories!)                         }
  if ab_EraseOptions in [ddoDeleteAll,ddoKeepDirOnly] Then
    Result:=fb_EraseSubDirs(as_StartDir);

  if ab_EraseOptions = ddoDeleteAll Then
    RemoveDirUTF8(as_StartDir);
end;

// Recursive procedure to build a list of files
function fb_EraseFiles(  as_StartDir : String ):Boolean;
var
  SR: TSearchRec;
  IsFound: Boolean;
begin
  Result := False ;

  { Build a list of the files in directory as_StartDir
     (not the directories!)                         }
  try
    IsFound := FindFirstUTF8(as_StartDir + '*', faAnyFile-faDirectory, SR) = 0 ;
    while IsFound do
     begin
      if (( SR.Name <> '.' ) and ( SR.Name <> '..' ))
      and FileExistsUTF8 ( as_StartDir + SR.Name )
       then
        Begin
          DeleteFileUTF8(as_StartDir + SR.Name);
        End ;
      IsFound := FindNextUTF8(SR) = 0;
      Result := True ;
    end;
    FindCloseUTF8(SR);
  Except
    FindCloseUTF8(SR);
  End ;
end;

// Function FileReadln
// reads a string from handle to buffer
Function FileReadln (Handle : THandle; var Buffer : String) : Longint;
var ABuffer : Char;
    ARead : Integer;
Begin
  Buffer:='';
  Result:=0;
  repeat
    ARead := FileRead(Handle,ABuffer,1);
    inc ( Result, ARead);
    if ABuffer in [#10,#13] then
     if Buffer > ''
      Then Exit
      Else Continue;
    if ARead > 0 Then
      AppendStr(Buffer,ABuffer);
  until ARead <= 0;
end;

// function FileWriteString
// Writes a string to handle
function FileWriteString(const AFile : THandle; const as_chaine : String; const ab_addAtEnd : Boolean = False; const ab_toadd : Byte = 0 ):Longint;
Begin
  if as_chaine>'' then
     Result := FileWrite(AFile,PChar(as_chaine)^,Length(as_chaine));
 if ab_addAtEnd then
    Result := FileWrite(AFile,ab_toadd,1);
End;

function FileWriteln(const AFile : THandle; const as_chaine : String = '' ):Longint;
const CST_10:Byte = 10;
Begin
 Result:=FileWriteString(AFile,as_chaine,True,13);
 FileWrite ( Afile,CST_10,1);
End;
// function FileCreateDeleteUTF8
// Deletes and create a file to result handle
function FileCreateDeleteUTF8 ( const as_filename : String ) :THandle;
Begin
  if FileExistsUTF8(as_filename) Then
    DeleteFileUTF8(as_filename);
  Result:=FileCreateUTF8(as_filename);
end;

// function FileCreateDeleteUTF8
// Deletes and create a file to result handle
function FileCreateDeleteUTF8File ( const as_filename : String ) :THandle;
Begin
  if FileExistsUTF8(as_filename) Then
    DeleteFileUTF8(as_filename);
  Result:=FileCreateUTF8File(as_filename);
end;

// function FileCreateDeleteUTF8
// Deletes and create a file to result handle
function FileCreateUTF8File ( const as_filename : String ) :THandle;
Begin
  Result:=FileCreateUTF8(as_filename);
{$IFDEF FPC}
  FileWrite(Result,UTF8BOM[1],Length(UTF8BOM));
{$ELSE}
  FileWriteString(Result,sUTF8BOMString[1],False);
{$ENDIF}
end;

// function FileCreateDeleteUTF8
// Deletes and create a file to result handle
function FileCreateDeleteFile ( const as_filename : String ) :THandle;
Begin
  if FileExistsUTF8(as_filename) Then
    DeleteFileUTF8(as_filename);
  Result:=FileCreateUTF8(as_filename);
end;

// copy a file from source to destination
Function fb_CopyFile ( const as_Source, as_Destination : String ; const ab_AppendFile : Boolean ; const ab_CreateBackup : Boolean = False ):Integer;
var
  li_SizeRead,li_SizeWrite,li_TotalW  : Longint;
  li_HandleSource,li_HandleDest, li_pos : integer;
  ls_FileName, ls_FileExt,ls_Destination : String ;
  lb_FoundFile,lb_Error : Boolean;
  lsr_data : Tsearchrec;
  FBuffer  : array[0..262143] of char;
begin
  Result := CST_COPYFILES_ERROR_UNKNOWN ;
  FindFirst(as_Source,faanyfile,lsr_data);
  li_TotalW := 0;
  findclose(lsr_data);
  li_HandleSource := FileOpenUTF8(as_Source,fmopenread);
  ls_Destination := as_Destination ;
  if  ab_AppendFile
  and FileExistsUTF8(as_Destination)
   then
    Begin
      FindFirstUTF8(as_Destination,faanyfile,lsr_data);
      li_HandleDest := FileOpenUTF8(as_Destination, fmopenwrite );
      FileSeek ( li_HandleDest, lsr_data.Size, 0 );
      FindCloseUTF8(lsr_data);
    End
   Else
     Begin
      If FileExistsUTF8(ls_Destination)
       then
        Begin
          FindFirstUTF8(as_Destination,faanyfile,lsr_data);
          if ( ab_CreateBackup )
           Then
            Begin
              ls_FileName := lsr_data.Name;
              ls_FileExt  := '' ;
              li_pos := 1;
              while ( PosEx ( '.', ls_FileName, li_pos + 1 ) > 0 ) Do
                li_pos := PosEx ( '.', ls_FileName, li_pos + 1 );
              if ( li_Pos > 1 ) Then
                Begin
                  ls_FileExt  := Copy ( ls_FileName, li_pos, length ( ls_FileName ) - li_pos + 1 );
                  ls_FileName := Copy ( ls_FileName, 1, li_pos - 1 );
                End ;
              li_pos := 0 ;
              while FileExistsUTF8 ( ls_Destination ) do
               Begin
                 inc ( li_pos );
                 ls_Destination := ExtractFilePath ( as_Destination ) + DirectorySeparator + ls_FileName + '-' + IntToStr ( li_pos ) + ls_FileExt ;
               End
            End
           Else
            DeleteFileUTF8(as_Destination);
          FindCloseUTF8(lsr_data);
        End ;
      li_HandleDest := FileCreateUTF8(ls_Destination);
     end ;
  lb_FoundFile := False;
  lb_Error := false;
  while not lb_FoundFile do
    begin
      li_SizeRead := FileRead(li_HandleSource,FBuffer,high ( Fbuffer ) + 1);
      if li_SizeRead < high ( Fbuffer ) + 1 then lb_FoundFile := True;
      li_SizeWrite := Filewrite(li_HandleDest,Fbuffer,li_SizeRead);
      inc( li_TotalW, li_SizeWrite );
      if li_SizeWrite < li_SizeRead then lb_Error := True;
    end;
  FileSetDate(li_HandleDest,filegetdate(li_HandleSource));
  FileClose(li_HandleSource);
  FileClose(li_HandleDest);
  if lb_Error = False then
    Begin
      Result := 0 ;
    End ;
  Application.ProcessMessages ;
end;

{$IFDEF WINDOWS}
function fs_verifyAndReplaceDriveLetter ( const as_path : String ):String;
var qwspace : {$IFDEF CPU64}Pint64{$ELSE}PLongInt{$ENDIF};
Begin
  Result:=UpperCase (as_path [1]) + copy ( as_path, 2, Length(as_path)-1);
  if ( Result [ 1 ] in [ '/', '\' ] )  Then
   Exit; // if path is unix path or translated
  while DiskFree(Byte(Result[1]))=0 do
   Begin
     if (Result[1]='C') Then Exit;
     Result[1] := chr ( ord ( Result[1] ) - 1 );
   end;
end;
{$ENDIF}

function fb_CreateDirectoryStructure ( const as_DirectoryToCreate : String ) : Boolean ;
var
  lsr_data : Tsearchrec;
  li_Pos : Integer ;
  ls_Temp : String ;
begin
  Result := False ;
  if DirectoryExistsUTF8 ( as_DirectoryToCreate )
   Then Result := True
   Else
    try
       li_Pos := 1 ;
       while ( Posex ( DirectorySeparator, as_DirectoryToCreate, li_pos + 1 ) > 1 ) do
         li_Pos := Posex ( DirectorySeparator, as_DirectoryToCreate, li_pos + 1 );
       if ( li_pos > 1 )
        Then ls_Temp := Copy ( as_DirectoryToCreate, 1 , li_pos - 1 )
        Else Exit ;
       if  not DirectoryExistsUTF8 ( ls_Temp )
        Then fb_CreateDirectoryStructure ( ls_Temp );
       if DirectoryExistsUTF8 ( ls_Temp )
        then
         Begin
           FindFirstUTF8 ( ls_Temp,faanyfile,lsr_data);
           if ( DirectoryExistsUTF8 ( ls_Temp )) Then
             try
               CreateDirUTF8 ( as_DirectoryToCreate );
               Result := True ;
             except
             End
            Else
             Result := False ;
           FindCloseUTF8 ( lsr_data );
         end;
     Finally
     End ;
End ;

procedure p_LoadStrings ( const astl_StringList : TStrings; const as_FilePath,  as_message : String );
var TheStream:{$IFDEF FPC}TFileStreamUTF8{$ELSE}TFileStream{$ENDIF};
Begin
  astl_StringList.Clear;
  TheStream:={$IFDEF FPC}TFileStreamUTF8{$ELSE}TFileStream{$ENDIF}.Create(as_FilePath,fmOpenRead);
  try
    try
      astl_StringList.LoadFromStream(TheStream);
    except
      On E: Exception do
      if as_message > '' then
        begin
          ShowMessage(as_message + CST_ENDOFLINE + E.Message);
          Abort;
        end;
    end;
  finally
    TheStream.Free;
  end;
end;

procedure p_SaveStrings ( const astl_StringList : TStrings; const as_FilePath,  as_message : String );
var TheStream:{$IFDEF FPC}TFileStreamUTF8{$ELSE}TFileStream{$ENDIF};
Begin
  if FileExistsUTF8(as_FilePath) Then
   DeleteFileUTF8(as_FilePath);
  TheStream:={$IFDEF FPC}TFileStreamUTF8{$ELSE}TFileStream{$ENDIF}.Create(as_FilePath,fmCreate);
  try
    try
      astl_StringList.SaveToStream(TheStream);
    except
      On E: Exception do
      if as_message > '' then
        begin
          ShowMessage(as_message + as_filePath + CST_ENDOFLINE + E.Message);
          Abort;
        end;
    end;
  finally
    TheStream.Free;
  end;
end;


procedure p_FileNameDivision ( const as_FileNameWithExtension : String ; var as_FileName, as_Extension : String );
var li_pos : Integer;
Begin
  as_FileName := as_FileNameWithExtension;
  as_Extension  := '' ;
  li_pos := 1;
  while ( PosEx ( '.', as_FileName, li_pos + 1 ) > 0 ) Do
    li_pos := PosEx ( '.', as_FileName, li_pos + 1 );
  if ( li_Pos > 1 ) Then
    Begin
      as_Extension := Copy ( as_FileName, li_pos, length ( as_FileName ) - li_pos + 1 );
      as_FileName  := Copy ( as_FileName, 1, li_pos - 1 );
    End ;
End ;

function fb_IsFullPath ( const ASPath : String ):Boolean;
Begin
  {$IFDEF WINDOWS}
  Result := Pos(':',ASPath)=2;
  {$ELSE}
  Result := Pos(DirectorySeparator,ASPath)=1;
  {$ENDIF}
end;

initialization
{$IFDEF VERSIONS}
  p_ConcatVersion ( gVer_fonctions_file );
{$ENDIF}

end.

