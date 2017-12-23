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

unit P2PCgi;

interface

uses
  HttpDefs, StrUtils, SysUtils;

const

  CGI_QUERY_STRING_PHPSESSID = 'PHPSESSID';

  CGI_AUTH_TYPE = 'AUTH_TYPE';
  CGI_CONTENT_LENGTH = 'CONTENT_LENGTH';
  CGI_CONTENT_TYPE = 'CONTENT_TYPE';
  CGI_GATEWAY_INTERFACE = 'GATEWAY_INTERFACE';
  CGI_HTTP_ACCEPT = 'HTTP_ACCEPT';
  CGI_HTTP_ACCEPT_LANGUAGE = 'HTTP_ACCEPT_LANGUAGE';
  CGI_HTTP_COOKIE = 'HTTP_COOKIE';
  CGI_HTTP_USER_AGENT = 'HTTP_USER_AGENT';
  CGI_PATH_INFO = 'PATH_INFO';
  CGI_PATH_TRANSLATED = 'PATH_TRANSLATED';
  CGI_QUERY_STRING = 'QUERY_STRING';
  CGI_REMOTE_ADDR = 'REMOTE_ADDR';
  CGI_REMOTE_HOST = 'REMOTE_HOST';
  CGI_REMOTE_IDENT = 'REMOTE_IDENT';
  CGI_REMOTE_USER = 'REMOTE_USER';
  CGI_REQUEST_METHOD = 'REQUEST_METHOD';
  CGI_SCRIPT_NAME = 'SCRIPT_NAME';
  CGI_SCRIPT_FILENAME = 'SCRIPT_FILENAME';
  CGI_SERVER_NAME = 'SERVER_NAME';
  CGI_SERVER_PORT = 'SERVER_PORT';
  CGI_SERVER_PROTOCOL = 'SERVER_PROTOCOL';
  CGI_SERVER_SOFTWARE = 'SERVER_SOFTWARE';

function CgiGetEnv(const AName: string): string;
function CgiGetParam(const AName: string): string;
function CgiHasParam(const AName: string): boolean;
function CgiGetParamDef(const AName: string; const ADefault: string = ''): string;

implementation

var
  GQueryStrings: array of string;

procedure CgiProcessQueryString;
var
  LPos, LEnd, LEqu: integer;
  LString, LLine: string;
begin
  LString := CgiGetEnv(CGI_QUERY_STRING);
  LPos := 1;
  repeat
    LEnd := PosEx('&', LString, LPos);
    if LEnd = 0 then begin
      LEnd := Length(LString) + 1;
    end;
    LLine := Copy(LString, LPos, LEnd - LPos);
    LEqu := Pos('=', LLine);
    if LEqu > 0 then begin
      SetLength(GQueryStrings, Length(GQueryStrings) + 2);
      GQueryStrings[Length(GQueryStrings) - 2] := HttpDecode(AnsiLeftStr(LLine, LEqu - 1));
      GQueryStrings[Length(GQueryStrings) - 1] := HttpDecode(Copy(LLine, LEqu + 1, MaxInt));
    end;
    LPos := LEnd + 1;
  until LPos > Length(LString);
end;

function CgiGetEnv(const AName: string): string;
begin
  Result := GetEnvironmentVariable(AName);
end;

function CgiGetParam(const AName: string): string;
var
  LIndex: integer;
begin
  LIndex := 0;
  while LIndex < High(GQueryStrings) do begin
    if SameText(GQueryStrings[LIndex], AName) then begin
      Exit(GQueryStrings[LIndex + 1]);
    end;
    LIndex := LIndex + 2;
  end;
  raise EArgumentException.Create('Unable to locate parameter.');
end;

function CgiHasParam(const AName: string): boolean;
begin
  try
    CgiGetParam(AName);
    Result := True;
  except
    Result := False;
  end;
end;

function CgiGetParamDef(const AName, ADefault: string): string;
begin
  try
    Result := CgiGetParam(AName);
  except
    Result := ADefault;
  end;
end;

initialization

  CgiProcessQueryString;

end.
