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

unit __Variants;

interface

uses PhpLib, SysUtils;

function VarArrayHighBound(const AVariant: variant; const ADim: integer): integer;
function VarArrayLowBound(const AVariant: variant; const ADim: integer): integer;
function VarArrayOf(const AVariant: variant): variant;
function VarIsArray(const AVariant: variant): boolean;
function VarIsBool(const AVariant: variant): boolean;
function VarIsFloat(const AVariant: variant): boolean;
function VarIsNull(const AVariant: variant): boolean;
function VarIsNumeric(const AVariant: variant): boolean;
function VarIsStr(const AVariant: variant): boolean;
function VarToStr(const AVariant: variant): string;
function VarToStrDef(const AVariant: variant; const ADefault: string): string;
procedure VarArrayRedim(var AVariant: variant; const AHigh: integer);

implementation

function VarIsArray(const AVariant: variant): boolean;
begin
  Result := php_is_array(AVariant);
end;

function VarIsBool(const AVariant: variant): boolean;
begin
  Result := php_is_bool(AVariant);
end;

function VarIsFloat(const AVariant: variant): boolean;
begin
  Result := php_is_float(AVariant);
end;

function VarIsNull(const AVariant: variant): boolean;
begin
  Result := php_is_null(AVariant);
end;

function VarIsNumeric(const AVariant: variant): boolean;
begin
  Result := php_is_numeric(AVariant);
end;

function VarIsStr(const AVariant: variant): boolean;
begin
  Result := php_is_string(AVariant);
end;

function VarToStrDef(const AVariant: variant; const ADefault: string): string;
begin
  if VarIsNull(AVariant) then begin
    Result := ADefault;
  end else begin
    Result := php_strval(AVariant);
  end;
end;

function VarToStr(const AVariant: variant): string;
begin
  Result := VarToStrDef(AVariant, EmptyStr);
end;

function VarArrayOf(const AVariant: variant): variant;
begin
  Result := AVariant;
end;

function VarArrayLowBound(const AVariant: variant; const ADim: integer): integer;
begin
  Assert(ADim = 1);
  Result := 0;
end;

function VarArrayHighBound(const AVariant: variant; const ADim: integer): integer;
begin
  Assert(ADim = 1);
  Result := php_count(AVariant) - 1;
end;

procedure VarArrayRedim(var AVariant: variant; const AHigh: integer);
var
  LIndex: integer;
begin
  for LIndex := php_count(AVariant) - 1 downto AHigh + 1 do begin
    php_unset(AVariant[LIndex]);
  end;
  for LIndex := php_count(AVariant) to AHigh do begin
    AVariant[LIndex] := Null;
  end;
end;

end.
