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

unit Pas2PhpPasVariable;

{$INCLUDE pas2php.inc}

interface

uses
  Classes, Pas2PhpTranslate, Pas2PhpUtils, PasTree, SysUtils;

type

  TPasVariableHelper = class helper for TPasVariable
    function _TryGetValue(var AValue: string): boolean;
    function _AsPhpInteger: string;
    function _AsPhpFloat: string;
    function _AsPhpString: string;
    function _AsPhpValue: string;
    function _AsPhpBoolean: string;
  end;

implementation

function TPasVariableHelper._TryGetValue(var AValue: string): boolean;
begin
  Result := Assigned(Expr);
  if Result then begin
    AValue := Expr.GetDeclaration(True);
  end;
end;

function TPasVariableHelper._AsPhpValue: string;
begin
  Enforce(_TryGetValue(Result), 'Unknown variable value');
end;

function TPasVariableHelper._AsPhpInteger: string;
begin
  if _TryGetValue(Result) then begin
    Result := GPhpLang.EncodeInteger(OPasLang.DecodeInteger(Result));
  end else begin
    Result := '0';
  end;
end;

function TPasVariableHelper._AsPhpFloat: string;
begin
  if _TryGetValue(Result) then begin
    Result := GPhpLang.EncodeFloat(OPasLang.DecodeFloat(Result));
  end else begin
    Result := '0.0';
  end;
end;

function TPasVariableHelper._AsPhpString: string;
begin
  if _TryGetValue(Result) then begin
    Result := GPhpLang.EncodeString(OPasLang.DecodeString(Result));
  end else begin
    Result := GPhpLang.SStringQuote + GPhpLang.SStringQuote;
  end;
end;

function TPasVariableHelper._AsPhpBoolean: string;
begin
  if _TryGetValue(Result) then begin
    Result := GPhpLang.EncodeBoolean(OPasLang.DecodeBoolean(Result));
  end else begin
    Result := GPhpLang.SFalse;
  end;
end;

end.
