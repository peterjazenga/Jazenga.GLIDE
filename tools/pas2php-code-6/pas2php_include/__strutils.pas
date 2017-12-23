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

unit __StrUtils;

interface

uses P2PVariants, PhpLib, SysUtils, Variants;

function AnsiEndsStr(const ASub, AStr: string): boolean;
function AnsiEndsText(const ASub, AStr: string): boolean;
function AnsiIndexStr(const AStr: string; const AArray: variant): integer;
function AnsiIndexText(const AStr: string; const AArray: variant): integer;
function AnsiLeftStr(const AStr: string; const ALen: integer): string;
function AnsiMatchStr(const AStr: string; const AArray: variant): boolean;
function AnsiMatchText(const AStr: string; const AArray: variant): boolean;
function AnsiReplaceStr(const AStr, AFrom, ATo: string): string;
function AnsiReplaceText(const AStr, AFrom, ATo: string): string;
function AnsiReverseString(const AStr: string): string;
function AnsiRightStr(const AStr: string; const ALen: integer): string;
function AnsiSameStr(const A, B: string): boolean;
function AnsiSameText(const A, B: string): boolean;
function AnsiStartsStr(const ASub, AStr: string): boolean;
function AnsiStartsText(const ASub, AStr: string): boolean;
function IfThen(const ABoolean: boolean; const A, B: string): string;
function ReplaceStr(const AStr, AFrom, ATo: string): string;
function ReplaceText(const AStr, AFrom, ATo: string): string;
function ReverseString(const AStr: string): string;
function RPos(const ASub, AStr: string): integer;
function RPosEx(const ASub, AStr: string; const AIdx: integer): integer;
function StuffString(const AStr: string; const APos, ALen: integer; const ASub: string): string;
function PosSet(const ASubString: variant; const AString: string): integer;
function PosSetEx(const ASubString: variant; const AString: string;
  const AOffset: integer): integer;
function PosEx(const ASubString: variant; const AString: string;
  const AOffset: integer = 1): integer;

implementation

function ReverseString(const AStr: string): string;
begin
  Result := php_strrev(AStr);
end;

function AnsiReverseString(const AStr: string): string;
begin
  Result := php_strrev(AStr);
end;

function AnsiSameStr(const A, B: string): boolean;
begin
  Result := php_strcmp(A, B) = 0;
end;

function AnsiSameText(const A, B: string): boolean;
begin
  Result := php_strcasecmp(A, B) = 0;
end;

function ReplaceText(const AStr, AFrom, ATo: string): string;
begin
  Result := php_str_ireplace(AFrom, ATo, AStr);
end;

function ReplaceStr(const AStr, AFrom, ATo: string): string;
begin
  Result := php_str_replace(AFrom, ATo, AStr);
end;

function AnsiReplaceText(const AStr, AFrom, ATo: string): string;
begin
  Result := php_str_ireplace(AFrom, ATo, AStr);
end;

function AnsiReplaceStr(const AStr, AFrom, ATo: string): string;
begin
  Result := php_str_replace(AFrom, ATo, AStr);
end;

function RPos(const ASub, AStr: string): integer;
begin
  Result := php_strrpos(AStr, ASub, 0);
  if php_is_bool(Result) then begin
    Result := 0;
  end else begin
    Result := Result + 1;
  end;
end;

function RPosEx(const ASub, AStr: string; const AIdx: integer): integer;
begin
  Result := php_strrpos(AStr, ASub, AIdx - 1);
  if php_is_bool(Result) then begin
    Result := 0;
  end else begin
    Result := Result + 1;
  end;
end;

function IfThen(const ABoolean: boolean; const A, B: string): string;
begin
  if ABoolean then begin
    Result := A;
  end else begin
    Result := B;
  end;
end;

function AnsiIndexStr(const AStr: string; const AArray: variant): integer;
begin
  Result := VarArrayIndexStr(AStr, AArray);
end;

function AnsiIndexText(const AStr: string; const AArray: variant): integer;
begin
  Result := VarArrayIndexText(AStr, AArray);
end;

function AnsiMatchStr(const AStr: string; const AArray: variant): boolean;
begin
  Result := VarArrayMatchStr(AStr, AArray);
end;

function AnsiMatchText(const AStr: string; const AArray: variant): boolean;
begin
  Result := VarArrayMatchText(AStr, AArray);
end;

function AnsiLeftStr(const AStr: string; const ALen: integer): string;
begin
  Result := Copy(AStr, 1, ALen);
end;

function AnsiRightStr(const AStr: string; const ALen: integer): string;
begin
  Result := Copy(AStr, Length(AStr) - ALen + 1, ALen);
end;

function AnsiStartsStr(const ASub, AStr: string): boolean;
begin
  Result := AnsiSameStr(ASub, AnsiLeftStr(AStr, Length(ASub)));
end;

function AnsiEndsStr(const ASub, AStr: string): boolean;
begin
  Result := AnsiSameStr(ASub, AnsiRightStr(AStr, Length(ASub)));
end;

function AnsiStartsText(const ASub, AStr: string): boolean;
begin
  Result := AnsiSameText(ASub, AnsiLeftStr(AStr, Length(ASub)));
end;

function AnsiEndsText(const ASub, AStr: string): boolean;
begin
  Result := AnsiSameText(ASub, AnsiRightStr(AStr, Length(ASub)));
end;

function StuffString(const AStr: string; const APos, ALen: integer; const ASub: string): string;
begin
  Result := Copy(AStr, 1, APos - 1) + ASub + Copy(AStr, APos + ALen, MaxInt);
end;

function PosEx(const ASubString: variant; const AString: string; const AOffset: integer): integer;
begin
  Result := php_strpos(AString, ASubString, AOffset - 1);
  if php_is_bool(Result) then begin
    Result := 0;
  end else begin
    Result := Result + 1;
  end;
end;

function PosSet(const ASubString: variant; const AString: string): integer;
begin
  Result := PosSetEx(ASubString, AString, 1);
end;

function PosSetEx(const ASubString: variant; const AString: string;
  const AOffset: integer): integer;
var
  LIndex: integer;
begin
  Result := 0;
  if VarIsStr(ASubString) then begin
    for LIndex := 1 to Length(ASubString) do begin
      Result := PosEx(VarToStr(ASubString[LIndex]), AString, AOffset);
      if Result > 0 then begin
        Break;
      end;
    end;
  end else if VarIsArray(ASubString) then begin
    for LIndex := 0 to VarArrayHigh(ASubString) do begin
      Result := PosEx(VarToStr(ASubString[LIndex]), AString, AOffset);
      if Result > 0 then begin
        Break;
      end;
    end;
  end else begin
    raise EArgumentException.Create('Invalid SubString.');
  end;
end;

end.
