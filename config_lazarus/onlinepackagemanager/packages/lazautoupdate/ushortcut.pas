unit ushortcut;

{
License
=======
LazAutoUpdate (c)2015 Gordon Bamber (minesadorada@charcodelvalle.com)

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

Credits
=======
Code adapted from fpcup (@BigChimp and @DonAlfredo at freepascal forum)

Use
===
Use public function 'GetShortCutErrorString' to show errors/info when debugging

Linux Shortcut Info
===================

1. FreeDesktop Valid Categories
===============================
AudioVideo                  Application for presenting, creating, or processing multimedia (audio/video)
Audio                       An audio application  Desktop entry must include AudioVideo as well
Audio                       A video application  Desktop entry must include AudioVideo as well
Development                 An application for development
Education                   Educational software
Game                        A game
Graphics                    Application for viewing, creating, or processing graphics
Network                     Network application such as a web browser
Office                      An office type application
Science                     Scientific software
Settings                    Settings applications  Entries may appear in a separate menu or as part of a "Control Center"
System                      System application, "System Tools" such as say a log viewer or network monitor
Utility                     Small utility application, "Accessories"

2. Example Desktop File
=======================
[Desktop Entry]
Version=1.0
Type=Application
Name=Foo Viewer
Comment=The best viewer for Foo objects available!
TryExec=fooview
Exec=fooview %F
Icon=fooview
MimeType=image/x-foo;
Actions=Gallery;Create;

[Desktop Action Gallery]
Exec=fooview --gallery
Name=Browse Gallery

[Desktop Action Create]
Exec=fooview --create-new
Name=Create a new Foo!
Icon=fooview-new
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazUTF8, FileUtil, LazFileUtils
  {$IFDEF LINUX}, process, strutils, LazUTF8Classes{$ENDIF}
  {$IFDEF WINDOWS}, Windows, shlobj {for special folders}, ActiveX,
  ComObj, ShellAPI{$ENDIF}  ;

function CreateDesktopShortCut(Target, TargetArguments, ShortcutName,
  IconFileName, Category: string): boolean;
function DeleteDesktopShortcut(ShortcutName: string): boolean;
function GetShortCutDebugString: string;

implementation

var
  sDebugString: string;
// *****************************************************************************
// Functions and procs to aid Debugging
// *****************************************************************************
function GetShortCutDebugString: string;
begin
  if (sDebugString = '') then
    Result := 'OK'
  else
    Result := sDebugString;
end;
// Builds up a string with linebreaks
procedure AddToDebugString(Astring: string);
begin
  if (sDebugString = '') then
    sDebugString := LineEnding + '* ' + Astring
  else
    sDebugString := sDebugString + LineEnding + '* ' + Astring;
end;
// *****************************************************************************

{$IFDEF UNIX}
//Adapted from sysutils; Unix/Linux only
function XdgConfigHome: string;
{ Follows base-dir spec,
  see [http://freedesktop.org/Standards/basedir-spec].
  Always ends with PathDelim. }
begin
  Result := GetEnvironmentVariable('XDG_CONFIG_HOME');
  if (Result = '') then
    Result := IncludeTrailingPathDelimiter(ExpandFileNameUTF8('~')) +
      '.config' + DirectorySeparator
  else
    Result := IncludeTrailingPathDelimiter(Result);
end;

{$ENDIF UNIX}
{$IFDEF MSWINDOWS}
function CreateDesktopShortCut(Target, TargetArguments, ShortcutName,
  IconFileName, Category: string): boolean;
{
IN:
   Target: Filename with full path
   TargetArguments: String of arguments
   ShortCutName: Simple string
   IconFileName: Filename with full path
   Category: Simple string (see header of this unit)
OUT:
   True = Success
   False = Failure
   Use function GetShortCutDebugString to get most recent error as a string
}
var
  IObject: IUnknown;
  ISLink: IShellLink;
  IPFile: IPersistFile;
  PIDL: PItemIDList;
  InFolder: array[0..MAX_PATH] of char;
  LinkName: WideString;
begin
  Result := True;
  sDebugString := '';
  // Simple failure check
  if not FileExistsUTF8(Target) then
  begin
    AddToDebugString('Filename ' + Target + ' does not exist');
    Result := False;
    Exit;
  end;

  try
    { Creates an instance of IShellLink }
    IObject := CreateComObject(CLSID_ShellLink);
    ISLink := IObject as IShellLink;
    IPFile := IObject as IPersistFile;
    try
      ISLink.SetPath(PChar(Target));
      ISLink.SetArguments(PChar(TargetArguments));
      ISLink.SetWorkingDirectory(PChar(ExtractFilePath(Target)));
      { Get the desktop location }
      SHGetSpecialFolderLocation(0, CSIDL_DESKTOPDIRECTORY, PIDL);
      SHGetPathFromIDList(PIDL, InFolder);

      LinkName := IncludeTrailingPathDelimiter(InFolder) + ShortcutName + '.lnk';
      { Get rid of any existing shortcut first }
      if not SysUtils.DeleteFile(LinkName) then
        AddToDebugString('Could not delete existing link ' + LinkName);
     {Create the link }
      IPFile.Save(PWChar(LinkName), False);
      {Notify the shell}
      SHChangeNotify(SHCNE_CREATE, SHCNF_PATH, PChar(LinkName), nil);
      SHChangeNotify(SHCNE_UPDATEDIR, SHCNF_PATH or SHCNF_FLUSH,
      PChar(ExtractFileDir(LinkName)), nil);

     {Menu Entry}
     SHGetSpecialFolderLocation(0, CSIDL_PROGRAMS, PIDL);
     SHGetPathFromIDList(PIDL, InFolder);
     If Not DirectoryExistsUTF8(IncludeTrailingPathDelimiter(InFolder) + ShortcutName) then
       ForceDirectoriesUTF8(IncludeTrailingPathDelimiter(InFolder) + ShortcutName);
     LinkName := IncludeTrailingPathDelimiter(InFolder) + ShortcutName + DirectorySeparator + ShortcutName + '.lnk';
     { Get rid of any existing shortcut first }
     if not SysUtils.DeleteFile(LinkName) then
       AddToDebugString('Could not delete existing link ' + LinkName);
     {Create the menu entry link }
     IPFile.Save(PWChar(LinkName), False);
     {Notify the shell}
     SHChangeNotify(SHCNE_MKDIR, SHCNF_PATH, PChar(LinkName), nil)

    finally
      // Not needed: FreeAndNil(IPFile);
    end;
  except
    Result := False;
  end;
end;

{$ENDIF MSWINDOWS}
{$IFDEF UNIX}
function CreateDesktopShortCut(Target, TargetArguments, ShortcutName,
  IconFileName, Category: string): boolean;
{
* Comprehensive debugging messages in this routine!
* So many flavours of Linux.. - if no desktop icon is created then
* call GetShortCutDebugString and log the result to a file.
IN:
   Target: Filename with full path
   TargetArguments: String of arguments
   ShortCutName: Simple string
   IconFileName: Filename with full path.  If not specifies Executable will be used
   Category: Simple string (see header of this unit)
OUT:
   True = Success
   False = Failure
   Use function GetShortCutDebugString to get errors as a string
}
var
  XdgDesktopStringList: TStringListUTF8;
  XdgDesktopFile: string;
  Aprocess: TProcess;
  sPathToShare: string;
  sDesktopFilename: string;
begin
  // Succeed by default:
  Result := True;
  sDebugString := '';
  // Simple failure checks
  if not FileExistsUTF8(Target) then // lethal
  begin
    AddToDebugString('File "' + Target + '" cannot be located. Quitting.');
    Result := False;
    Exit;
  end;
  if not FileExistsUTF8(IconFileName) then // non-lethal
  begin
    AddToDebugString('File "' + IconFileName + '" cannot be located. Using Target.');
    IconFileName := Target;
  end;
  if ShortCutName = '' then // lethal
  begin
    AddToDebugString('ShortcutName is blank. Quitting.');
    Result := False;
    Exit;
  end;
  if Category = '' then // non-lethal
  begin
    AddToDebugString('Category is blank. Using "Utility"');
    Category := 'Utility';
  end;
  // Make up an 8-character filename
  sDesktopFilename := DelSpace(shortcutname);
  sDesktopFilename := LeftStr(sDesktopFilename, 8);
  sDesktopFilename := LowerCase(sDesktopFilename);
  AddToDebugString('Desktop filename = ' + sDesktopFilename);
  // Note:  IncludeTrailingPathDelimiter(ExpandFileNameUTF8('~')) resolves to '/root/'

  // Standard path to DeskTop files
  sPathToShare := '/usr/share/applications' + DirectorySeparator +
    sDesktopFilename + '.desktop';
  // Directory check
  if not DirectoryExistsUTF8('/usr/share/applications') then // non-lethal
    AddToDebugString('Cannot find directory - ' + '/usr/share/applications');

  // Temp directory path
  XdgDesktopFile := IncludeTrailingPathDelimiter(GetTempDir(False)) +
    sDesktopFilename + '.desktop';
  // Directory check
  if not DirectoryExistsUTF8(GetTempDir(False)) then // lethal
  begin
    AddToDebugString('Failure: Invalid directory - ' + GetTempDir(False));
    Result := False;
    Exit;
  end;
  AddToDebugString('Success: XdgDesktopFile = ' + XdgDesktopFile);
  AddToDebugString('Success: sPathToShare = ' + sPathToShare);

  // Make up the desktop file
  XdgDesktopStringList := TStringListUTF8.Create;
  try
    XdgDesktopStringList.Add('[Desktop Entry]');
    XdgDesktopStringList.Add('Encoding=UTF-8');
    XdgDesktopStringList.Add('Type=Application');
    //XdgDesktopStringList.Add('NoDisplay=True');
    XdgDesktopStringList.Add('Icon=' + IconFileName);
    if TargetArguments <> '' then
      XdgDesktopStringList.Add('Exec=' + Target + ' ' + TargetArguments)
    else
      XdgDesktopStringList.Add('Exec=' + Target);
    XdgDesktopStringList.Add('Name=' + ShortcutName);
    XdgDesktopStringList.Add('Category=' + Category);
    // We're going to try and call xdg-desktop-icon
    // this may fail if shortcut exists already
    AProcess := TProcess.Create(nil);
    try
      try
        if FileExistsUTF8(XdgDesktopFile) then
          DeleteFile(XdgDesktopFile);
        Sleep(100);
        try
          XdgDesktopStringList.SaveToFile(XdgDesktopFile);
        except
          if not FileExistsUTF8(XdgDesktopFile) then
            AddToDebugString('Failure: XdgDesktopFile wasn''t saved');
        end;
        if FileExistsUTF8(XdgDesktopFile) then
        begin
          Aprocess.Executable := 'xdg-desktop-icon install';
          AProcess.CurrentDirectory := ProgramDirectory;
          AProcess.Parameters.Clear;
          AProcess.Parameters.Add(XdgDesktopFile);
          Aprocess.Execute;
          Sleep(100);
          AddToDebugString('xdg-desktop-icon install succeeded');
        end;
      except
        // xdg-desktop-icon install failed.
        Result := False;
        AddToDebugString('Failure: Exception running "xdg-desktop-icon install"');

        // OK. Try usr/share/applications
        if FileExistsUTF8(sPathToShare) then
        begin
          if SysUtils.DeleteFile(sPathToShare) then
            AddToDebugString('Successfully deleted existing ' + sPathToShare)
          else
            AddToDebugString('Failure: Unable to delete existing ' + sPathToShare);
        end;
        // Final Directory check
        if not DirectoryExistsUTF8('/usr/share/applications') then // lethal
        begin
          AddToDebugString(
            'Failure: Directory "/usr/share/applications" does not exist on this system');
          Result := False;
          Exit;
        end;
        // Save the stringlist directly to usr/share/applications
        try
          XdgDesktopStringList.SaveToFile(sPathToShare);
        except
          if not FileExistsUTF8(sPathToShare) then
          begin
            Result := False;
            AddToDebugString('Failure: SaveToFile(' + sPathToShare + ') failed');
          end;
        end;
      end;
    finally
      AProcess.Free;
    end;
    if Result = False then
      try
        if not (FileExistsUTF8(XdgDesktopFile)) then
          AddToDebugString('Unable to locate temporary ' + XdgDesktopFile);
        if (FileExistsUTF8(XdgDesktopFile)) and (not FileExistsUTF8(sPathToShare)) then
        begin
          // Last try to copy file to usr/share/applications
          if CopyFile(XdgDesktopFile, sPathToShare) then
          begin
            AddToDebugString(Format('Successfully copied %s file to %s',
              [XdgDesktopFile, sPathToShare]));
            Result := True;
          end
          else
            AddToDebugString(Format('Unable to copy %s file to %s',
              [XdgDesktopFile, sPathToShare]));
          // Temp file is no longer needed....
          if not SysUtils.DeleteFile(XdgDesktopFile) then
          begin
            AddToDebugString('Failure: Unable to delete temporary ' + XdgDesktopFile);
          end;
        end;
        if (FileExistsUTF8(sPathToShare)) then
        begin
          Result := True;
          AddToDebugString('Success: Desktop file - ' + sPathToShare);
        end;
      finally
        // Swallow, let filesystem maintenance clear it up
      end;
  finally
    XdgDesktopStringList.Free;
  end;
end;

{$ENDIF UNIX}
{$IFDEF MSWINDOWS}
function DeleteDesktopShortcut(ShortcutName: string): boolean;
var
  PIDL: PItemIDList;
  InFolder: array[0..MAX_PATH] of char;
  LinkName: WideString;
begin
  Result := False; // Assume failure; look for success
  sDebugString := '';
  try
    { Get the desktop location }
    SHGetSpecialFolderLocation(0, CSIDL_DESKTOPDIRECTORY, PIDL);
    SHGetPathFromIDList(PIDL, InFolder);
    LinkName := IncludeTrailingPathDelimiter(InFolder) + ShortcutName + '.lnk';
    if SysUtils.DeleteFile(LinkName) then
    begin
      Result := True;
      AddToDebugString('DeleteDesktopShortcut Success: Deleted ' + LinkName);
    end
    else
      AddToDebugString('DeleteDesktopShortcut Failure: Unable to delete ' + LinkName);
    { Get the menu location}
    SHGetSpecialFolderLocation(0, CSIDL_PROGRAMS, PIDL);
    SHGetPathFromIDList(PIDL, InFolder);
    LinkName := IncludeTrailingPathDelimiter(InFolder) + ShortcutName + DirectorySeparator + ShortcutName + '.lnk';
    { Get rid of any existing shortcut first }
    if SysUtils.DeleteFile(LinkName) then
    begin
      AddToDebugString('DeleteDesktopShortcut Success: Deleted ' + LinkName);
       Result := True;
    end
    else
      AddToDebugString('DeleteDesktopShortcut Failure: Unable to delete ' + LinkName);
    If DirectoryExistsUTF8(IncludeTrailingPathDelimiter(InFolder) + ShortcutName) then
      If RemoveDirUTF8(IncludeTrailingPathDelimiter(InFolder) + ShortcutName) then
         AddToDebugString('DeleteDesktopShortcut Success: Deleted menu entry')
       else
         AddToDebugString('DeleteDesktopShortcut Failure: Unable to delete menu entry');
  except
    AddToDebugString('Exception deleting ' + LinkName);
    // Eat the exception
  end;
end;

{$ELSE}
function DeleteDesktopShortcut(ShortcutName: string): boolean;
begin
  sDebugString := 'DeleteDesktopShortcut not implemented in Linux';
  Result := False;
end;

{$ENDIF MSWINDOWS}

end.
