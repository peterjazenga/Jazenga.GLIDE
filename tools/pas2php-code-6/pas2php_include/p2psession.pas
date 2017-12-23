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

unit P2PSession;

interface

uses
  Classes, DateUtils, FileUtil, HttpDefs, P2PCgi, SysUtils;

function SessionGetString(const AName: string): string;
function SessionGetStringDef(const AName: string; const ADefault: string = ''): string;

procedure SessionSetString(const AName, AValue: string);
procedure SessionSetFromCgiDef(const AName: string; const ADefault: string = '');

implementation

{$IFNDEF PAS2PHP}
const
  GFolderSessions = 'sessions';
  GDotSession = '.session';
  GSessionLife = 20; // Minutes
  GCleanUpLife = 2; // Minutes

var
  GSession: TStringList;
  GLastCleanUp: TDateTime;

function SessionFilename: string;
begin
  Result := ProgramDirectory + GFolderSessions + DirectorySeparator +
    CgiGetParam(CGI_QUERY_STRING_PHPSESSID) + GDotSession;
end;

procedure SessionCleanUp;
var
  LSearchRec: TSearchRec;
begin
  if MinutesBetween(Now, GLastCleanUp) > GCleanUpLife then begin
    GLastCleanUp := Now;
    if FindFirst(ExtractFilePath(SessionFilename) + '*' + GDotSession, 0, LSearchRec) = 0 then begin
      try
        repeat
          if MinutesBetween(GLastCleanUp, FileDateToDateTime(LSearchRec.Time)) > GSessionLife then begin
            DeleteFile(ExtractFilePath(SessionFilename) + LSearchRec.Name);
          end;
        until FindNext(LSearchRec) <> 0;
      finally
        FindClose(LSearchRec);
      end;
    end;
  end;
end;

{$ENDIF}

function SessionGetString(const AName: string): string;
begin
{$IFDEF PAS2PHP}
  Result := _SESSION[AName];
  if not php_is_string(Result) then
  begin
    raise EArgumentException.Create('Unable to find "' + AName + '".');
  end;
{$ELSE}
  if GSession.IndexOfName(AName) < 0 then begin
    raise EArgumentException.Create('Unable to find "' + AName + '".');
  end;
  Result := HTTPDecode(GSession.Values[AName]);
{$ENDIF}
end;

function SessionGetStringDef(const AName: string; const ADefault: string): string;
begin
  try
    Result := SessionGetString(AName);
  except
    Result := ADefault;
  end;
end;

procedure SessionSetString(const AName, AValue: string);
begin
{$IFDEF PAS2PHP}
  _SESSION[AName] := AValue;
{$ELSE}
  GSession.Values[AName] := HTTPEncode(AValue);
  try
    GSession.SaveToFile(SessionFilename);
  except
  end;
  SessionCleanUp;
{$ENDIF}
end;

procedure SessionSetFromCgiDef(const AName: string; const ADefault: string);
begin
  SessionSetString(AName, CgiGetParamDef(AName, ADefault));
end;

initialization

{$IFNDEF PAS2PHP}
  GSession := TStringList.Create;
  try
    GSession.LoadFromFile(SessionFilename);
  except
  end;
{$ENDIF}

end.
