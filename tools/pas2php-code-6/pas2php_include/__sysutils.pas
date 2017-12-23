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

unit __SysUtils;

interface

uses PhpLib, StrUtils, SysUtils;

const

  EmptyStr = '';

  SBoolTrue = 'True';
  SBoolFalse = 'False';

type

  EAbort = class(Exception)
  end;

  EConvertError = class(Exception)
  end;

  EAssertionFailed = class(Exception)
  end;

  EArgumentException = class(Exception)
  end;

  EArgumentOutOfRangeException = class(EArgumentException)
  end;

  EAbstractError = class(Exception)
  end;

  EFinally = class(Exception)
  end;

procedure Abort;
function AnsiCompareStr(const AString1, AString2: string): integer;
function AnsiCompareText(const AString1, AString2: string): integer;
function BoolToStr(const ABoolean: boolean; const ATrue: string = 'True';
  const AFalse: string = 'False'): string;
function ChangeFileExt(const AFileName, AFileExt: string): string;
function DirectoryExists(const AFileName: string): boolean;
function ExtractFileDir(const AFileName: string): string;
function ExtractFileExt(const AFileName: string): string;
function ExtractFileName(const AFileName: string): string;
function ExtractFilePath(const AFileName: string): string;
function FileExists(const AFileName: string): boolean;
function FloatToStr(const AExtended: extended): string;
function GetEnvironmentVariable(const AName: string): string;
function IntToStr(const AInteger: integer): string;
function SameText(const AString1, AString2: string): boolean;
function SetDirSeparators(const AString: string): string;
function StrToBool(const AString: string): boolean;
function StrToBoolDef(const AString: string; const ADefault: boolean): boolean;
function StrToFloat(const AString: string): extended;
function StrToFloatDef(const AString: string; const ADefault: extended): extended;
function StrToInt(const AString: string): integer;
function StrToIntDef(const AString: string; const ADefault: integer): integer;
function Trim(const AString: string): string; external;
function TrimLeft(const AString: string): string;
function TrimRight(const AString: string): string;
function TryStrToBool(const AString: string; out ABoolean: boolean): boolean;
function TryStrToFloat(const AString: string; out AExtended: extended): boolean;
function TryStrToInt(AString: string; out AInteger: integer): boolean;
procedure FreeAndNil(var AVariant: variant);
function IncludeLeadingPathDelimiter(const AFileName: string): string;
function IncludeTrailingPathDelimiter(const AFileName: string): string;

implementation

procedure Abort;
begin
  raise EAbort.Create('Abort');
end;

function IntToStr(const AInteger: integer): string;
begin
  Result := php_strval(AInteger);
end;

function SetDirSeparators(const AString: string): string;
var
  LIndex: integer;
begin
  Result := AString;
  for LIndex := 1 to Length(Result) do begin
    if (Result[LIndex] = '\') or (Result[LIndex] = '/') then begin
      Result[LIndex] := DirectorySeparator;
    end;
  end;
end;

function TryStrToInt(AString: string; out AInteger: integer): boolean;
begin
  if (Length(AString) > 0) and (AString[1] = '$') then begin
    AString := php_base_convert(Copy(AString, 2, MaxInt), 16, 10);
  end;
  AInteger := php_intval(AString, 10);
  Result := php_strval(AInteger) = AString;
end;

function StrToInt(const AString: string): integer;
begin
  if not TryStrToInt(AString, Result) then begin
    raise EConvertError.Create(EmptyStr);
  end;
end;

function StrToIntDef(const AString: string; const ADefault: integer): integer;
begin
  try
    Result := StrToInt(AString);
  except
    Result := ADefault;
  end;
end;

function TryStrToBool(const AString: string; out ABoolean: boolean): boolean;
begin
  try
    if SameText(AString, SBoolTrue) then begin
      ABoolean := True;
    end else if SameText(AString, SBoolFalse) then begin
      ABoolean := False;
    end else begin
      ABoolean := StrToInt(AString) <> 0;
    end;
    Result := True;
  except
    Result := False;
  end;
end;

function StrToBool(const AString: string): boolean;
begin
  if not TryStrToBool(AString, Result) then begin
    raise EConvertError.Create(EmptyStr);
  end;
end;

function StrToBoolDef(const AString: string; const ADefault: boolean): boolean;
begin
  try
    Result := StrToBool(AString);
  except
    Result := ADefault;
  end;
end;

function BoolToStr(const ABoolean: boolean; const ATrue, AFalse: string): string;
begin
  if ABoolean then begin
    Result := ATrue;
  end else begin
    Result := AFalse;
  end;
end;

function FloatToStr(const AExtended: extended): string;
begin
  Result := php_strval(AExtended);
end;

function TryStrToFloat(const AString: string; out AExtended: extended): boolean;
begin
  AExtended := php_floatval(AString);
  Result := php_strval(AExtended) = AString;
end;

function StrToFloat(const AString: string): extended;
begin
  if not TryStrToFloat(AString, Result) then begin
    raise EConvertError.Create(EmptyStr);
  end;
end;

function StrToFloatDef(const AString: string; const ADefault: extended): extended;
begin
  try
    Result := StrToFloat(AString);
  except
    Result := ADefault;
  end;
end;

procedure FreeAndNil(var AVariant: variant);
begin
  AVariant := Null;
end;

function SameText(const AString1, AString2: string): boolean;
begin
  Result := php_strcasecmp(AString1, AString2) = 0;
end;

function TrimLeft(const AString: string): string;
begin
  Result := php_ltrim(AString);
end;

function TrimRight(const AString: string): string;
begin
  Result := php_rtrim(AString);
end;

function AnsiCompareStr(const AString1, AString2: string): integer;
begin
  Result := php_strcmp(AString1, AString2);
end;

function AnsiCompareText(const AString1, AString2: string): integer;
begin
  Result := php_strcasecmp(AString1, AString2);
end;

function GetEnvironmentVariable(const AName: string): string;
begin
  Result := php_getenv(AName);
end;

function ExtractFileDir(const AFileName: string): string;
begin
  Result := php_dirname(SetDirSeparators(AFileName), 1);
end;

function ExtractFileName(const AFileName: string): string;
begin
  Result := php_basename(SetDirSeparators(AFileName));
end;

function FileExists(const AFileName: string): boolean;
begin
  Result := php_is_file(AFileName);
end;

function DirectoryExists(const AFileName: string): boolean;
begin
  Result := php_is_dir(AFileName);
end;

function ExtractFileExt(const AFileName: string): string;
var
  LPos: integer;
begin
  Result := ExtractFilename(AFileName);
  LPos := RPos(ExtensionSeparator, Result);
  if LPos = 0 then begin
    Result := EmptyStr;
  end else begin
    Result := Copy(Result, LPos, MaxInt);
  end;
end;

function ExtractFilePath(const AFileName: string): string;
begin
  Result := ExtractFileDir(AFileName) + DirectorySeparator;
end;

function ChangeFileExt(const AFileName, AFileExt: string): string;
var
  LPos: integer;
begin
  Result := ExtractFilename(AFileName);
  LPos := RPos(ExtensionSeparator, Result);
  if LPos = 0 then begin
    Result := ExtractFilePath(AFileName) + Result + AFileExt;
  end else begin
    Result := ExtractFilePath(AFileName) + Copy(Result, 1, LPos - 1) + AFileExt;
  end;
end;

function IncludeLeadingPathDelimiter(const AFileName: string): string;
begin
  Result := AFileName;
  if (Length(Result) > 0) and not (Result[1] in AllowDirectorySeparators) then begin
    Result := DirectorySeparator + Result;
  end;
end;

function IncludeTrailingPathDelimiter(const AFileName: string): string;
begin
  Result := AFileName;
  if (Length(Result) > 0) and not (Result[Length(Result)] in AllowDirectorySeparators) then begin
    Result := Result + DirectorySeparator;
  end;
end;

end.
