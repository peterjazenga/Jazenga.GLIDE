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

unit Pas2PhpPasExpr;

{$INCLUDE pas2php.inc}

interface

uses
  Classes, Pas2PhpPasElement, Pas2PhpTranslate, PasTree, SysUtils;

type

  TPasExprHelper = class helper for TPasExpr
    function _IsSubIdent: boolean;
    function _FindExprDefInScope(AScope: TPasElement): TPasElement;
  end;

implementation

// TODO: This needs to be re-written I think.
// NOTE: This is the original function. This the method below should be the same as this.
function _PasExprIsSubIdent(const AExpression: TPasExpr): boolean;
begin
  try
    with AExpression.CustomElement as TBinaryExpr do begin
      Result := ((left = AExpression) and (CustomElement is TBinaryExpr) and
        ((CustomElement as TBinaryExpr).OpCode = eopSubIdent) and ((CustomElement as TBinaryExpr).right =
        AExpression.CustomElement));
      if not Result then begin
        Result := ((right = AExpression) and ((OpCode = eopSubIdent) or (left is TSelfExpr) or
          (left is TInheritedExpr)));
      end;
    end;
  except
    if AExpression.CustomElement._IsParamsExprAndIsValue(AExpression) then begin
      Result := _PasExprIsSubIdent(AExpression.CustomElement as TPasExpr);
    end else begin
      Result := False;
    end;
  end;
end;

function TPasExprHelper._IsSubIdent: boolean;
var
  LBinaryExpr: TBinaryExpr;
begin
  try
    LBinaryExpr := CustomElement as TBinaryExpr;
    Result := ((LBinaryExpr.left = Self) and (LBinaryExpr.CustomElement is TBinaryExpr) and
      (TBinaryExpr(LBinaryExpr.CustomElement).OpCode = eopSubIdent) and
      (TBinaryExpr(LBinaryExpr.CustomElement).right = CustomElement));
    if not Result then begin
      Result := ((LBinaryExpr.right = Self) and ((LBinaryExpr.OpCode = eopSubIdent) or
        (LBinaryExpr.left is TSelfExpr) or (LBinaryExpr.left is TInheritedExpr)));
    end;
  except
    if CustomElement._IsParamsExprAndIsValue(Self) then begin
      Result := (CustomElement as TPasExpr)._IsSubIdent;
    end else begin
      Result := False;
    end;
  end;
end;

// ORIGINAL::: Note: This is one nasty function to work on!
function PasExprExprFindDef(const AExpr: TPasExpr; AScope: TPasElement): TPasElement;
var
  LBinaryExpr: TBinaryExpr;
begin
  try
    if AExpr.CustomElement._IsParamsExprAndIsValue(AExpr) then begin
      LBinaryExpr := AExpr.CustomElement.CustomElement as TBinaryExpr;
      if LBinaryExpr.OpCode in [eopSubIdent, eopAs] then begin
        if AExpr.CustomElement = LBinaryExpr.right then begin
          AScope := AScope._FindInScopeByName(OPasLang.AliasName(PasExprExprFindDef(
            LBinaryExpr.left, AScope)._GetPasType(True).Name));
        end else begin
          AScope := AScope._FindInScopeByName(OPasLang.AliasName(PasExprExprFindDef(
            (LBinaryExpr.CustomElement as TBinaryExpr).left, AScope)._GetPasType(True).Name));
        end;
      end;
    end else if AExpr.CustomElement is TBinaryExpr then begin
      LBinaryExpr := TBinaryExpr(AExpr.CustomElement);
      if LBinaryExpr.OpCode in [eopSubIdent, eopAs] then begin
        if AExpr = LBinaryExpr.right then begin
          AScope := AScope._FindInScopeByName(OPasLang.AliasName(
            PasExprExprFindDef(LBinaryExpr.left, AScope)._GetPasType(True).Name));
        end else begin
          AScope := AScope._FindInScopeByName(OPasLang.AliasName(PasExprExprFindDef(
            (LBinaryExpr.CustomElement as TBinaryExpr).left, AScope)._GetPasType(True).Name));
        end;
      end;
    end;
  except
  end;
  try
    Result := AScope._FindInScopeByName(OPasLang.AliasName((AExpr as TPrimitiveExpr).Value));
  except
    Result := AScope._FindInScopeByName(OPasLang.AliasName(
      ((AExpr as TParamsExpr).Value as TPrimitiveExpr).Value));
  end;
end;

function TPasExprHelper._FindExprDefInScope(AScope: TPasElement): TPasElement;
var
  LBinaryExpr: TBinaryExpr;
begin
  try
    if CustomElement._IsParamsExprAndIsValue(Self) then begin
      LBinaryExpr := CustomElement.CustomElement as TBinaryExpr;
      if LBinaryExpr.OpCode in [eopSubIdent, eopAs] then begin
        if CustomElement = LBinaryExpr.right then begin
          AScope := AScope._FindInScopeByName(OPasLang.AliasName(PasExprExprFindDef(
            LBinaryExpr.left, AScope)._GetPasType(True).Name));
        end else begin
          AScope := AScope._FindInScopeByName(OPasLang.AliasName(PasExprExprFindDef(
            (LBinaryExpr.CustomElement as TBinaryExpr).left, AScope)._GetPasType(True).Name));
        end;
      end;
    end else if CustomElement is TBinaryExpr then begin
      LBinaryExpr := TBinaryExpr(CustomElement);
      if LBinaryExpr.OpCode in [eopSubIdent, eopAs] then begin
        if Self = LBinaryExpr.right then begin
          AScope := AScope._FindInScopeByName(OPasLang.AliasName(
            PasExprExprFindDef(LBinaryExpr.left, AScope)._GetPasType(True).Name));
        end else begin
          AScope := AScope._FindInScopeByName(OPasLang.AliasName(PasExprExprFindDef(
            (LBinaryExpr.CustomElement as TBinaryExpr).left, AScope)._GetPasType(True).Name));
        end;
      end;
    end;
  except
  end;
  try
    Result := AScope._FindInScopeByName(OPasLang.AliasName((Self as TPrimitiveExpr).Value));
  except
    Result := AScope._FindInScopeByName(OPasLang.AliasName(
      ((Self as TParamsExpr).Value as TPrimitiveExpr).Value));
  end;
end;

end.
