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

unit Pas2PhpElementExpr;

{$INCLUDE pas2php.inc}

interface

uses
  Pas2PhpDefines, Pas2PhpFPList, Pas2PhpPasElement, Pas2PhpPasExpr, Pas2PhpPasType,
  Pas2PhpTranslate, Pas2PhpUtils, Pas2PhpVariable, PasTree, StrUtils, SysUtils;

type

  TPas2PhpExpression = class(TPas2PhpVariable)
  strict private
    function PasExprResultIsString(const APasExpr: TPasExpr; const AScope: TPasElement): boolean;
    procedure PasArgument(const APasArgument: TPasArgument; const AScope: TPasProcedure);
    procedure PasBoolConstExpr(const ABoolConstExpr: TBoolConstExpr);
    procedure PasInheritedExpr(const AInheritedExpr: TInheritedExpr; const AScope: TPasElement);
    procedure PasSelfExpr(const ASelfExpr: TSelfExpr; const AScope: TPasElement);
    procedure PasUnaryExpr(const AUnaryExpr: TUnaryExpr; const AScope: TPasElement);
  protected
    procedure PasBinaryExpr(const ABinaryExpr: TBinaryExpr; const AParent, AScope: TPasElement);
    procedure PasExpr(const APasExpr: TPasExpr; const AParent, AScope: TPasElement); override;
    procedure PasParamsExpr(const AParamsExpr: TParamsExpr; const AParent, AScope: TPasElement);
    procedure PasPrimitiveExpr(const APrimitiveExpr: TPrimitiveExpr; const AParent, AScope: TPasElement);
    procedure PasProcedureType(const APasProcedureType: TPasProcedureType);
  end;

implementation

procedure TPas2PhpExpression.PasProcedureType(const APasProcedureType: TPasProcedureType);
var
  LIndex: integer;
begin
  Assert(Assigned(APasProcedureType));
  for LIndex := 0 to APasProcedureType.Args.High do begin
    if LIndex > 0 then begin
      WritePhp(', ');
    end;
    PasArgument(APasProcedureType.Args[LIndex] as TPasArgument, APasProcedureType.Parent as
      TPasProcedure);
  end;
end;

procedure TPas2PhpExpression.PasArgument(const APasArgument: TPasArgument; const AScope: TPasProcedure);
begin
  Assert(Assigned(APasArgument));
  if APasArgument.Access in [argVar, argOut, argConstRef] then begin
    WritePhp(OChar.Amp);
  end;
  WritePhp(GPhpLang.EncodeIdent(APasArgument.Name));
  if Assigned(APasArgument.ValueExpr) then begin
    WritePhp(' = ');
    PasExpr(APasArgument.ValueExpr, APasArgument, AScope);
  end;
end;

function TPas2PhpExpression.PasExprResultIsString(const APasExpr: TPasExpr;
  const AScope: TPasElement): boolean;
begin
  Assert(Assigned(APasExpr));
  Result := APasExpr.Kind = pekString;
  if not Result then begin
    try
      Result := AScope._GetPas2PhpVarType((APasExpr as TPrimitiveExpr).Value, False) = ptString;
    except
    end;
    if not Result then begin
      try
        Result := AScope._GetPas2PhpVarType(((APasExpr as TParamsExpr).Value as
          TPrimitiveExpr).Value, True) = ptString;
      except
      end;
      if not Result then begin
        try
          Result := PasExprResultIsString((APasExpr as TBinaryExpr).left, AScope);
        except
        end;
        if not Result then begin
          try
            Result := PasExprResultIsString((APasExpr as TBinaryExpr).right, AScope);
          except
          end;
        end;
      end;
    end;
  end;
end;

procedure TPas2PhpExpression.PasParamsExpr(const AParamsExpr: TParamsExpr;
  const AParent, AScope: TPasElement);
var
  LIndex: integer;
  LExprDefIsString: boolean;
  LExprDef: TPasElement;
begin
  Assert(Assigned(AParamsExpr));
  LExprDefIsString := False;
  AParamsExpr.CustomElement := AParent;

  if Assigned(AParamsExpr.Value) then begin
    if AParamsExpr.Value is TParamsExpr then begin
      PasParamsExpr(AParamsExpr.Value as TParamsExpr, AParamsExpr, AScope);
      try
        LExprDef := (AParamsExpr.Value as TParamsExpr)._FindExprDefInScope(AScope);
        LExprDefIsString := LExprDef._GetPas2PhpVarType(True) = ptString;
      except
      end;
    end else begin
      try
        try
          LExprDef := AParamsExpr._FindExprDefInScope(AScope);
        except
          LExprDef := nil;
        end;
        if Assigned(LExprDef) then begin
          if (LExprDef is TPasConst) and (AParamsExpr.Kind = pekArrayParams) then begin
            RaiseException(LExprDef, GIndexingConstantsNotSupportedDot, 'PasParamsExpr');
          end;
          try
            LExprDefIsString := (AParamsExpr.Kind = pekArrayParams) and
              (LExprDef._GetPas2PhpVarType(False) = ptString);
          except
          end;
          if not AParamsExpr._IsSubIdent then begin
            if LExprDef._IsClassMember then begin
              WritePhp(GPhpLang.SThisDeRef);
            end else if not (LExprDef is TPasProcedure) and not (LExprDef is TPasConst) then begin
              WritePhp(OChar.Dollar);
            end;
          end;
        end;
        PasPrimitiveExpr(AParamsExpr.Value as TPrimitiveExpr, AParamsExpr, AScope);
        if (AParamsExpr.Kind = pekArrayParams) and (LExprDef is TPasFunction) then begin
          WritePhp('()');
        end;
      except
        RaiseException(AParamsExpr.Value, GExpectedTPrimitiveExprDot, 'PasParamsExpr');
      end;
    end;
  end;
  case AParamsExpr.Kind of
    pekSet: begin
      WritePhp(GPhpLang.SArray + '(');
    end;
    pekArrayParams: begin
      WritePhp('[');
    end;
    pekFuncParams: begin
      WritePhp('(');
    end else begin
      RaiseException(AParamsExpr, GUnknownExprKindDot, 'PasParamsExpr');
    end;
  end;
  for LIndex := 0 to High(AParamsExpr.Params) do begin
    if LIndex > 0 then begin
      WritePhp(', ');
    end;
    if LExprDefIsString then begin
      WritePhp('(');
    end;
    PasExpr(AParamsExpr.Params[LIndex], AParamsExpr, AScope);
    if LExprDefIsString then begin
      WritePhp(') - 1');
    end;
  end;
  case AParamsExpr.Kind of
    pekSet: begin
      WritePhp(')');
    end;
    pekArrayParams: begin
      WritePhp(']');
    end;
    pekFuncParams: begin
      WritePhp(')');
    end else begin
      RaiseException(AParamsExpr, GUnknownExprKindDot, 'PasParamsExpr');
    end;
  end;
end;

procedure TPas2PhpExpression.PasPrimitiveExpr(const APrimitiveExpr: TPrimitiveExpr;
  const AParent, AScope: TPasElement);
var
  LExprDef: TPasElement;
begin
  Assert(Assigned(APrimitiveExpr));
  APrimitiveExpr.CustomElement := AParent;
  case APrimitiveExpr.Kind of
    pekIdent: begin
      try
        LExprDef := APrimitiveExpr._FindExprDefInScope(AScope);
      except
        LExprDef := nil;
      end;
      if Assigned(LExprDef) then begin
        if LExprDef is TPasProcedure then begin
          if AParent._IsBinaryLeft(TInheritedExpr) or AParent.CustomElement._IsBinaryLeft(
            TInheritedExpr) then begin
            if LExprDef is TPasConstructor then begin
              WritePhp(GPhpLang.SConstruct);
            end else if LExprDef is TPasDestructor then begin
              WritePhp(GPhpLang.SDestruct);
            end else begin
              WritePhp(LExprDef.Name);
            end;
          end else begin
            if not AParent._IsParamsExprAndIsValue(APrimitiveExpr) and
              LExprDef._IsClassMember and not APrimitiveExpr._IsSubIdent then begin
              WritePhp(GPhpLang.SThisDeRef);
            end;
            WritePhp(LExprDef.Name);
          end;
          if not AParent._IsParamsExprAndIsValue(APrimitiveExpr) then begin
            WritePhp(GPhpLang.SEmptyBrackets);
          end;
        end else if LExprDef is TPasVariable then begin
          if LExprDef is TPasProperty then begin
            if APrimitiveExpr._IsSubIdent then begin
              WritePhp(LExprDef.Name);
            end else begin
              WritePhp(GPhpLang.SThisDeRef + LExprDef.Name);
            end;
          end else if LExprDef is TPasConst then begin
            WritePhp(LExprDef.Name);
          end else begin
            if not AParent._IsParamsExprAndIsValue(APrimitiveExpr) and
              LExprDef._IsClassMember and not APrimitiveExpr._IsSubIdent then begin
              WritePhp(GPhpLang.SThisDeRef + GPhpLang.EncodeIdent(LExprDef.Name, True));
            end else begin
              WritePhp(GPhpLang.EncodeIdent(LExprDef.Name, AParent._IsParamsExprAndIsValue(APrimitiveExpr) or
                APrimitiveExpr._IsSubIdent));
            end;
          end;
        end else if LExprDef is TPasEnumValue then begin
          WritePhp(LExprDef.Name);
        end else if LExprDef is TPasArgument then begin
          WritePhp(GPhpLang.EncodeIdent(LExprDef.Name, AParent._IsParamsExprAndIsValue(APrimitiveExpr)));
        end else if LExprDef is TPasClassType then begin
          // This happends with (A is B)
          WritePhp(LExprDef.Name);
        end else begin
          RaiseException(LExprDef, GUnknownClassTypeDot, 'PasPrimitiveExpr');
        end;
      end else begin
        if not AnsiStartsStr(OPasLang.PhpLibNamePrefix, APrimitiveExpr.Value) then begin
          WriteLn('>>>>>>>>>>>>>>>>>>>>>>>> Unable to find "' + APrimitiveExpr.Value + '".');
          ReadLn;
        end;
        if (AParent is TParamsExpr) and ((AParent as TParamsExpr).Kind = pekFuncParams) then begin
          WritePhp(OPasLang.AliasName(APrimitiveExpr.Value));
        end else begin
          WritePhp(GPhpLang.EncodeIdent(APrimitiveExpr.Value, APrimitiveExpr._IsSubIdent));
        end;
      end;
    end;
    pekNumber: begin
      WritePhp(APrimitiveExpr.Value);
    end;
    pekString: begin
      WritePhp(GPhpLang.EncodeString(OPasLang.DecodeString(APrimitiveExpr.Value)));
    end;
  end;
end;

procedure TPas2PhpExpression.PasUnaryExpr(const AUnaryExpr: TUnaryExpr; const AScope: TPasElement);
begin
  Assert(Assigned(AUnaryExpr));
  WritePhp(GPhpLang.EncodeExprOpCode(AUnaryExpr.OpCode));
  PasExpr(AUnaryExpr.Operand, AUnaryExpr, AScope);
end;

procedure TPas2PhpExpression.PasSelfExpr(const ASelfExpr: TSelfExpr; const AScope: TPasElement);
begin
  Assert(Assigned(ASelfExpr));
  WritePhp(GPhpLang.SThis);
end;

procedure TPas2PhpExpression.PasInheritedExpr(const AInheritedExpr: TInheritedExpr;
  const AScope: TPasElement);
begin
  Assert(Assigned(AInheritedExpr));
  WritePhp(GPhpLang.SParent + '::');
end;

procedure TPas2PhpExpression.PasBinaryExpr(const ABinaryExpr: TBinaryExpr;
  const AParent, AScope: TPasElement);
var
  LPasExpr: TPasExpr;
  LParamsExpr: TParamsExpr;
  LClassType: TPasClassType;
begin
  Assert(Assigned(ABinaryExpr));
  ABinaryExpr.CustomElement := AParent;
  try
    LClassType := AScope._FindInScopeByName((ABinaryExpr.left as TPrimitiveExpr).Value) as TPasClassType;
    WritePhp(GPhpLang.SNew + OChar.Space + LClassType.Name);
    try
      LParamsExpr := ABinaryExpr.right as TParamsExpr;
      LPasExpr := LParamsExpr.Value;
      LParamsExpr.Value := nil;
      try
        PasParamsExpr(LParamsExpr, ABinaryExpr, AScope);
      finally
        LParamsExpr.Value := LPasExpr;
      end;
    except
      WritePhp(GPhpLang.SEmptyBrackets);
    end;
  except
    if ABinaryExpr.OpCode = eopIn then begin
      WritePhp(GPhpLang.SInArray + '(');
    end else if ABinaryExpr.OpCode = eopDiv then begin
      WritePhp('(' + GPhpLang.SInt + ')(');
    end;
    PasExpr(ABinaryExpr.left, ABinaryExpr, AScope);

    if ABinaryExpr.OpCode <> eopAs then begin
      // TODO: We have better searching functions. Can we use them here to find the element type?
      if (ABinaryExpr.OpCode = eopAdd) and (PasExprResultIsString(ABinaryExpr.left, AScope) or
        PasExprResultIsString(ABinaryExpr.right, AScope)) then begin
        WritePhp(' . ');
      end else begin
        WritePhp(GPhpLang.EncodeExprOpCode(ABinaryExpr.OpCode));
      end;
      PasExpr(ABinaryExpr.right, ABinaryExpr, AScope);
    end;
    if ABinaryExpr.OpCode = eopIn then begin
      WritePhp(')');
    end else if ABinaryExpr.OpCode = eopDiv then begin
      WritePhp(')');
    end;
  end;
end;

procedure TPas2PhpExpression.PasBoolConstExpr(const ABoolConstExpr: TBoolConstExpr);
begin
  Assert(Assigned(ABoolConstExpr));
  WritePhp(GPhpLang.EncodeBoolean(ABoolConstExpr.Value));
end;

procedure TPas2PhpExpression.PasExpr(const APasExpr: TPasExpr; const AParent, AScope: TPasElement);
begin
  Assert(Assigned(APasExpr));
  APasExpr.CustomElement := AParent;
  try
    PasBinaryExpr(APasExpr as TBinaryExpr, AParent, AScope);
  except
    try
      PasPrimitiveExpr(APasExpr as TPrimitiveExpr, AParent, AScope);
    except
      try
        PasParamsExpr(APasExpr as TParamsExpr, AParent, AScope);
      except
        try
          PasBoolConstExpr(APasExpr as TBoolConstExpr);
        except
          try
            PasUnaryExpr(APasExpr as TUnaryExpr, AScope);
          except
            try
              PasSelfExpr(APasExpr as TSelfExpr, AScope);
            except
              try
                PasInheritedExpr(APasExpr as TInheritedExpr, AScope);
              except
                if APasExpr is TNilExpr then begin
                  WritePhp(GPhpLang.SNull);
                end else begin
                  RaiseException(APasExpr, GUnknownClassTypeDot, 'PasExpr');
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

end.
