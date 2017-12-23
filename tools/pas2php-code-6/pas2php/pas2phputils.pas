{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}

unit Pas2PhpUtils;

{$INCLUDE pas2php.inc}

interface

uses
  Classes, FileUtil, LazFileUtils, PasTree, SysUtils;

procedure LogMessage(const AMessage: string = '');

procedure RaiseException(const AMessage: string);
procedure RaiseException(const AMessage, ADetails: string);
procedure RaiseException(const AElement: TPasElement; const AMessage, AMethodName: string);

procedure Enforce(const ATrue: boolean; const AMessage: string);
procedure Enforce(const ATrue: boolean; const AMessage, ADetails: string);

procedure EnforceAbsolute(const AFileName: TFileName);
procedure EnforceFileExists(const AFileName: TFileName);
procedure EnforceDirectoryExists(const ADirectory: TFileName);

procedure IfThenUnsupported(const ABoolean: boolean; const AFeatures: string);

implementation

procedure LogMessage(const AMessage: string);
begin
  WriteLn(StdOut, AMessage);
end;

procedure RaiseException(const AMessage: string);
begin
  LogMessage('Exception: ' + AMessage);
  raise Exception.Create(AMessage);
end;

procedure RaiseException(const AMessage, ADetails: string);
begin
  RaiseException(AMessage + ' - ' + QuotedStr(ADetails));
end;

procedure RaiseException(const AElement: TPasElement; const AMessage, AMethodName: string);
begin
  Assert(Assigned(AElement));
  RaiseException(Format(
    '%s in method "%s" PathName="%s" ClassName="%s" SourceLinenumber="%d" ElementTypeName="%s"',
    [AMessage, AMethodName, AElement.PathName, AElement.ClassName, AElement.SourceLinenumber,
    AElement.ElementTypeName]));
end;

procedure Enforce(const ATrue: boolean; const AMessage: string);
begin
  if not ATrue then begin
    RaiseException(AMessage);
  end;
end;

procedure Enforce(const ATrue: boolean; const AMessage, ADetails: string);
begin
  if not ATrue then begin
    RaiseException(AMessage, ADetails);
  end;
end;

procedure EnforceAbsolute(const AFileName: TFileName);
begin
  Enforce(FilenameIsAbsolute(AFileName), 'Filename must be absolute', AFileName);
end;

procedure EnforceFileExists(const AFileName: TFileName);
begin
  Enforce(FileExists(AFileName), 'Filename dont not exist', AFileName);
end;

procedure EnforceDirectoryExists(const ADirectory: TFileName);
begin
  Enforce(DirectoryExists(ADirectory), 'Directory dont not exist', ADirectory);
end;

procedure IfThenUnsupported(const ABoolean: boolean; const AFeatures: string);
begin
  if ABoolean then begin
    RaiseException('Unsupported Feature: ' + AFeatures + LineEnding);
  end;
end;

end.
