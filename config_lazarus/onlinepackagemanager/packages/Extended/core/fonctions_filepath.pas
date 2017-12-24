unit fonctions_filepath;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB;

function fs_GetFileField (const AValue : String ): String;
procedure p_VerifyField ( const Field : TField );
procedure p_DeleteFile ( const AFilesDir,AText : String );
function fb_VerifyFilesDir ( const AFilesDir : String):Boolean;
function fs_SaveFile ( const ALocalDir, AFilesDir, AText, ATextOld : String ): String;

implementation

uses FileUtil, fonctions_file, LazUTF8, LazFileUtils, fonctions_dialogs;

function fs_GetFileField (const AValue : String ): String;
Begin
  Result:='';
  {$IFDEF WINDOWS}
  if (length(Avalue)<4)
  or (Avalue[2]<>':')
  or (Avalue[3]<>DirectorySeparator)Then
   Exit;
  {$ELSE}
  if (Avalue='')
  or (Avalue[1]<>DirectorySeparator)Then
   Exit;
  {$ENDIF}
  Result:=IncludeTrailingPathDelimiter(ExtractFileDir(Avalue));
end;
procedure p_VerifyField ( const Field : TField );
Begin
  if Field is TBlobField Then
    MyShowMessage('Please do not use TExtDBFileEdit for BlobField.');
End;
procedure p_DeleteFile ( const AFilesDir,AText : String );
Begin
  if  (AText>'')
  and (FileExistsUTF8(AFilesDir+AText)) Then
    DeleteFileUTF8(AFilesDir+AText);
End;
function fs_SaveFile ( const ALocalDir, AFilesDir, AText, ATextOld : String ): String;
var ls_filePath,ls_file : String ;
    li_i : Integer;
Begin
  Result := '';
  if AFilesDir > '' Then
   Begin
    p_DeleteFile ( AFilesDir, AtextOld );
    ls_filePath := AFilesDir+AText;
    fb_CreateDirectoryStructure(AFilesDir);
    if FileExistsUTF8(ALocalDir+AText) Then
     Begin
      li_i := 1;
      ls_file := AText;
      repeat
        inc ( li_i );
        ls_filePath:=AFilesDir+ExtractFileNameOnly(AText)+IntToStr(li_i)+ExtractFileExt(AText);
      until not FileExistsUTF8(ls_filePath);
      CopyFile(ALocalDir+ls_file,ls_filePath,False);
      Result:=ExtractFileNameOnly(ls_file)+IntToStr(li_i)+ExtractFileExt(AText);
     end
    Else
     Begin
       CopyFile(ALocalDir+ls_file,AFilesDir+AText,False);
       Result:=ExtractFileName(AText);
     end;
   end;
End;
function fb_VerifyFilesDir ( const AFilesDir : String):Boolean;
Begin
  Result := AFilesDir > '';
  if not Result Then MyShowMessage('FilesDir not set. Can not save on TExtDBFileEdit.');
end;

end.

