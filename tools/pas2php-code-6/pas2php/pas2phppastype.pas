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

unit Pas2PhpPasType;

{$INCLUDE pas2php.inc}

interface

uses
  Pas2PhpTranslate, PasTree, StrUtils, SysUtils;

type

  TPas2PhpVarType = (ptUnknown, ptString, ptFloat, ptInteger, ptBoolean, ptVariant, ptObject, ptArray);

  TPasTypeHelper = class helper for TPasType
    function _GetPhpVarType: TPas2PhpVarType;
  end;

implementation

var

  GPasBooleans: array[0..0] of string = ('Boolean');
  GPasStrings: array[0..4] of string = (OPasLang.SChar, OPasLang.SString, OPasLang.SAnsiString,
    OPasLang.SWideString, OPasLang.SShortString);
  GPasIntegers: array[0..3] of string = (OPasLang.SInteger, OPasLang.SSmallInt,
    OPasLang.SLongInt, OPasLang.SInt64);
  GPasFloats: array[0..3] of string = (OPasLang.SSingle, OPasLang.SDouble, OPasLang.SExtended,
    OPasLang.SReal);
  GPasVariants: array[0..1] of string = (OPasLang.SVariant, OPasLang.SOleVariant);

function TPasTypeHelper._GetPhpVarType: TPas2PhpVarType;
begin
  Assert(Assigned(Self));
  if Self is TPasArrayType then begin
    Result := ptArray;
  end else begin
    if AnsiMatchText(Name, GPasBooleans) then begin
      Result := ptBoolean;
    end else if AnsiMatchText(Name, GPasStrings) then begin
      Result := ptString;
    end else if AnsiMatchText(Name, GPasIntegers) then begin
      Result := ptInteger;
    end else if AnsiMatchText(Name, GPasFloats) then begin
      Result := ptFloat;
    end else if AnsiMatchText(Name, GPasVariants) then begin
      Result := ptVariant;
    end else begin
      Result := ptObject;
    end;
  end;
end;

end.



