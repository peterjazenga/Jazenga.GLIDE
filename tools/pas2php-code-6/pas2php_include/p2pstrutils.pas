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

unit P2PStrUtils;

interface

uses
  P2PVariants, StrUtils, SysUtils, Variants;

function AnsiSplit(const AStr: string; const ADelimiter: char): variant;
function AnsiJoin(const AArray: variant; const ADelimiter: char): string;

implementation

function AnsiSplit(const AStr: string; const ADelimiter: char): variant;
var
  LPos, LEnd: integer;
begin
  Result := VarArrayOf([]);
  LPos := 1;
  repeat
    LEnd := PosEx(ADelimiter, AStr, LPos);
    if LEnd = 0 then begin
      LEnd := Length(AStr) + 1;
    end;
    VarArraySetLength(Result, VarArrayLength(Result) + 1);
    Result[VarArrayHigh(Result)] := Copy(AStr, LPos, LEnd - LPos);
    LPos := LEnd + 1;
  until LPos > Length(AStr);
end;

function AnsiJoin(const AArray: variant; const ADelimiter: char): string;
var
  LIndex: integer;
begin
  Result := EmptyStr;
  if VarArrayLength(AArray) > 0 then begin
    Result := AArray[0];
    for LIndex := 1 to VarArrayHigh(AArray) do begin
      Result := Result + ADelimiter + AArray[LIndex];
    end;
  end;
end;

end.
