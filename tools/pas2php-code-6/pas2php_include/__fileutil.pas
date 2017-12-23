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

unit __FileUtil;

interface

uses FileUtil, P2PCgi, PhpLib, StrUtils, SysUtils;

function FilenameIsAbsolute(const AFileName: string): boolean;
function FileIsReadable(const AFileName: string): boolean;
function FileIsWritable(const AFileName: string): boolean;
function ProgramDirectory: string;
function ReadFileToString(const AFileName: string): string;

implementation

function FilenameIsAbsolute(const AFileName: string): boolean;
var
  LPos: integer;
begin
  LPos := PosSet(AllowDirectorySeparators, AFileName);
  Result := (LPos = 1) or ((LPos > 1) and (AFileName[LPos - 1] = DriveSeparator));
end;

function FileIsReadable(const AFileName: string): boolean;
begin
  Result := php_is_readable(AFileName);
end;

function FileIsWritable(const AFileName: string): boolean;
begin
  Result := php_is_writable(AFileName);
end;

function ProgramDirectory: string;
begin
  Result := ExtractFilePath(CgiGetEnv(CGI_SCRIPT_FILENAME));
end;

function ReadFileToString(const AFileName: string): string;
begin
  Result := php_file_get_contents(AFileName);
end;

end.
