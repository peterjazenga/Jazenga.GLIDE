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

unit Pas2PhpVariable;

{$INCLUDE pas2php.inc}

interface

uses
  Pas2PhpDefines, Pas2PhpPasConst, Pas2PhpPasElement, Pas2PhpPasType, Pas2PhpPasVariable, Pas2PhpStream,
  Pas2PhpTranslate, Pas2PhpUtils, PasTree, SysUtils;

type

  TPas2PhpVariable = class(TPas2PhpStream)
  protected
    procedure PasConstant(const APasConst: TPasConst);
    procedure PasExpr(const APasExpr: TPasExpr; const AParent, AScope: TPasElement); virtual; abstract;
    procedure PasVariable(const APasVariable: TPasVariable; const AScope: TPasElement;
      const AIncludeVar: boolean = False);
    procedure PasVariableArrayType(const APasVariable: TPasVariable; const AScope: TPasElement);
  end;

implementation

procedure TPas2PhpVariable.PasConstant(const APasConst: TPasConst);
begin
  Assert(Assigned(APasConst));
  if Length(APasConst.Value) > 0 then begin
    WriteDeclaration(APasConst);
    WritePhp(GPhpLang.SDefine + '(' + GPhpLang.EncodeString(APasConst.Name) + ', ');
    case APasConst._GetPhpVarType of
      ptBoolean: begin
        WritePhp(APasConst._AsPhpBoolean);
      end;
      ptInteger: begin
        WritePhp(APasConst._AsPhpInteger);
      end;
      ptFloat: begin
        WritePhp(APasConst._AsPhpFloat);
      end;
      ptString: begin
        WritePhp(APasConst._AsPhpString);
      end else begin
        WritePhp(APasConst._AsPhpValue);
      end;
    end;
    WritePhp(')' + OChar.SemiColon);
  end;
end;

procedure TPas2PhpVariable.PasVariableArrayType(const APasVariable: TPasVariable; const AScope: TPasElement);
var
  LIndex, LLow, LHigh: integer;
  LIndexRange: string;
  LArrayValues: TArrayValues;
begin
  Assert(Assigned(APasVariable));
  LIndexRange := (APasVariable.VarType as TPasArrayType).IndexRange;
  if LIndexRange = EmptyStr then begin
    WritePhp(GPhpLang.SArray + GPhpLang.SEmptyBrackets);
  end else begin
    try
      LArrayValues := APasVariable.Expr as TArrayValues;
      try
        LIndex := Pos('..', LIndexRange);
        LLow := StrToInt(Copy(LIndexRange, 1, LIndex - 1));
        Enforce(LLow = 0, GArrayLowBoundMustBeZeroDot);
        LHigh := StrToInt(Copy(LIndexRange, LIndex + 2, MaxInt));
        Enforce(LHigh = High(LArrayValues.Values), GArrayHighDoesntMatchValues);
        WritePhp(GPhpLang.SArray + '(');
        for LIndex := 0 to High(LArrayValues.Values) do begin
          if LIndex > 0 then begin
            WritePhp(', ');
          end;
          PasExpr(LArrayValues.Values[LIndex], APasVariable.Expr, AScope);
        end;
        WritePhp(')');
      except
        RaiseException(APasVariable, GUnsupportedArrayRangeDot, 'PasVariable');
      end;
    except
      RaiseException(APasVariable, GStaticArraysNotSupportedDot, 'PasVariable');
    end;
  end;
end;

procedure TPas2PhpVariable.PasVariable(const APasVariable: TPasVariable;
  const AScope: TPasElement; const AIncludeVar: boolean);
begin
  Assert(Assigned(APasVariable));
  if not (vmExternal in APasVariable.VarModifiers) then begin
    WriteDeclaration(APasVariable);
    if AIncludeVar then begin
      WritePhp(GPhpLang.SVar + OChar.Space);
    end;
    WritePhp(GPhpLang.EncodeIdent(APasVariable.Name) + ' = ');
    case APasVariable.VarType._GetPhpVarType of
      ptBoolean: begin
        WritePhp(APasVariable._AsPhpBoolean);
      end;
      ptInteger: begin
        WritePhp(APasVariable._AsPhpInteger);
      end;
      ptFloat: begin
        WritePhp(APasVariable._AsPhpFloat);
      end;
      ptString: begin
        WritePhp(APasVariable._AsPhpString);
      end;
      ptVariant: begin
        WritePhp(GPhpLang.SNull);
      end;
      ptArray: begin
        PasVariableArrayType(APasVariable, AScope);
      end else begin
        try
          if AScope._FindInScopeByName(APasVariable.VarType.Name) is TPasRecordType then begin
            WritePhp(GPhpLang.SNew + OChar.Space + APasVariable.VarType.Name + GPhpLang.SEmptyBrackets);
          end else begin
            WritePhp(GPhpLang.SNull);
          end;
        except
          WritePhp(GPhpLang.SNull);
        end;
      end;
    end;
    WritePhp(OChar.SemiColon);
  end;
end;

end.
