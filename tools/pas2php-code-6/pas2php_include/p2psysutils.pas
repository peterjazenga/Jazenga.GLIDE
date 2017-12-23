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

unit P2PSysUtils;

interface

uses
  SysUtils;

function AnsiCompare(const A, B: string; const ACaseSensitive: boolean): integer;
function AnsiSame(const A, B: string; const ACaseSensitive: boolean): boolean;

implementation

function AnsiCompare(const A, B: string; const ACaseSensitive: boolean): integer;
begin
  if ACaseSensitive then begin
    Result := AnsiCompareStr(A, B);
  end else begin
    Result := AnsiCompareText(A, B);
  end;
end;

function AnsiSame(const A, B: string; const ACaseSensitive: boolean): boolean;
begin
  if ACaseSensitive then begin
    Result := AnsiSameStr(A, B);
  end else begin
    Result := AnsiSameText(A, B);
  end;
end;

end.
