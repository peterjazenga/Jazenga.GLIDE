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

unit Pas2PhpImplementation;

{$INCLUDE pas2php.inc}

interface

uses Classes, Pas2PhpDefines, Pas2PhpElementExpr, Pas2PhpFPList, Pas2PhpPasClassType, Pas2PhpPasElement,
  Pas2PhpTranslate, Pas2PhpUtils, PasTree, StrUtils, SysUtils;

type

  TPas2PhpElementImpl = class(TPas2PhpExpression)
  protected
    procedure PasImplAssign(const APasImplAssign: TPasImplAssign; const AScope: TPasElement);
    procedure PasImplBlock(const APasImplBlock: TPasImplBlock; const AScope: TPasElement);
    procedure PasImplCaseOf(const APasImplCaseOf: TPasImplCaseOf; const AScope: TPasElement);
    procedure PasImplCaseStatement(const APasImplCaseStatement: TPasImplCaseStatement;
      const AScope: TPasElement);
    procedure PasImplElement(const APasImplElement: TPasImplElement; const AScope: TPasElement);
    procedure PasImplElementList(const AImplElementList: TFPList; const AScope: TPasElement);
    procedure PasImplForLoop(const APasImplForLoop: TPasImplForLoop; const AScope: TPasElement);
    procedure PasImplIfElse(const APasImplIfElse: TPasImplIfElse; const AScope: TPasElement);
    procedure PasImplRepeatUntil(const APasImplRepeatUntil: TPasImplRepeatUntil; const AScope: TPasElement);
    procedure PasImplSimple(const APasImplSimple: TPasImplSimple; const AScope: TPasElement);
    procedure PasImplRaise(const APasImplRaise: TPasImplRaise; const AScope: TPasElement);
    procedure PasImplTry(const APasImplTry: TPasImplTry; const AScope: TPasElement);
    procedure PasImplWhileDo(const APasImplWhileDo: TPasImplWhileDo; const AScope: TPasElement);
  end;

  TPas2PhpGlobals = class(TPas2PhpElementImpl)
  protected
    procedure PasSectionGlobalVars(const ASection: TPasSection; const AScope: TPasElement);
    procedure PasModuleGlobalVars(const AModule: TPasModule; const AScope: TPasElement);
  end;

  TPas2PhpProcedure = class(TPas2PhpGlobals)
  protected
    procedure PasElementLists(const AOwner: TPasElement;
      const AConsts, ATypes, AClasses, AVariables, AFunctions: TFPList; const AScope: TPasElement);
  protected
    procedure PasEnumType(const AEnumType: TPasEnumType; const AScope: TPasElement);
    procedure PasRecordType(const ARecordType: TPasRecordType; const AScope: TPasElement);
    procedure PasClassTypeProperties(const AClassType: TPasClassType; const AScope: TPasElement);
    procedure PasClassType(const AClassType: TPasClassType; const AScope: TPasElement);
    procedure PasProcedurePrototype(const AProcedure: TPasProcedure; const AScope: TPasElement);
    procedure PasProcedure(const AProcedure: TPasProcedure; const AScope: TPasElement);
  end;

  TPas2PhpSection = class(TPas2PhpProcedure)
  strict private
    FConfigFileName: TFileName;
  protected
    procedure PasUnitRef(const AUnitRef: TPasUnresolvedUnitRef);
    procedure PasSection(const APasSection: TPasSection);
    procedure PasModule(const AModule: TPasModule; const AStream: TStream; const AIsEntryModule: boolean);
  public
    property ConfigFileName: TFileName read FConfigFileName write FConfigFileName;
  end;

implementation

procedure TPas2PhpElementImpl.PasImplElement(const APasImplElement: TPasImplElement;
  const AScope: TPasElement);
begin
  Assert(Assigned(APasImplElement));
  try
    PasImplIfElse(APasImplElement as TPasImplIfElse, AScope);
  except
    try
      PasImplWhileDo(APasImplElement as TPasImplWhileDo, AScope);
    except
      try
        PasImplRepeatUntil(APasImplElement as TPasImplRepeatUntil, AScope);
      except
        try
          PasImplBlock(APasImplElement as TPasImplBeginBlock, AScope);
        except
          try
            PasImplSimple(APasImplElement as TPasImplSimple, AScope);
          except
            try
              PasImplAssign(APasImplElement as TPasImplAssign, AScope);
            except
              try
                PasImplForLoop(APasImplElement as TPasImplForLoop, AScope);
              except
                try
                  PasImplCaseOf(APasImplElement as TPasImplCaseOf, AScope);
                except
                  try
                    PasImplTry(APasImplElement as TPasImplTry, AScope);
                  except
                    try
                      PasImplRaise(APasImplElement as TPasImplRaise, AScope);
                    except
                      RaiseException(APasImplElement, GUnknownClassTypeDot, 'PasImplElement');
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TPas2PhpElementImpl.PasImplElementList(const AImplElementList: TFPList; const AScope: TPasElement);
var
  LIndex: integer;
begin
  Assert(Assigned(AImplElementList));
  for LIndex := 0 to AImplElementList.High do begin
    PasImplElement(AImplElementList[LIndex] as TPasImplElement, AScope);
  end;
end;

procedure TPas2PhpElementImpl.PasImplSimple(const APasImplSimple: TPasImplSimple; const AScope: TPasElement);
var
  LValue: string;
begin
  Assert(Assigned(APasImplSimple));
  try
    PasParamsExpr(APasImplSimple.expr as TParamsExpr, APasImplSimple, AScope);
    WritePhp(OChar.SemiColon);
  except
    try
      LValue := OPasLang.AliasName((APasImplSimple.expr as TPrimitiveExpr).Value);
      if SameText(LValue, GPhpLang.SReturn) then begin
        WritePhp(GPhpLang.SReturn + '($' + OPasLang.SResult + ')' + OChar.SemiColon);
      end else if SameText(LValue, GPhpLang.SBreak) then begin
        WritePhp(GPhpLang.SBreak + OChar.SemiColon);
      end else begin
        PasPrimitiveExpr(APasImplSimple.expr as TPrimitiveExpr, APasImplSimple, AScope);
        WritePhp(OChar.SemiColon);
      end;
    except
      try
        PasBinaryExpr(APasImplSimple.expr as TBinaryExpr, APasImplSimple, AScope);
        WritePhp(OChar.SemiColon);
      except
        IfThenUnsupported(APasImplSimple.expr is TInheritedExpr,
          'Unnamed inherited expr. (Include method name)');
        RaiseException(APasImplSimple.expr, GUnknownClassTypeDot, 'PasImplSimple');
      end;
    end;
  end;
end;

procedure TPas2PhpElementImpl.PasImplAssign(const APasImplAssign: TPasImplAssign; const AScope: TPasElement);
begin
  Assert(Assigned(APasImplAssign));
  PasExpr(APasImplAssign.left, APasImplAssign, AScope);
  WritePhp(' = ');
  PasExpr(APasImplAssign.right, APasImplAssign, AScope);
  WritePhp(OChar.SemiColon);
end;

procedure TPas2PhpElementImpl.PasImplBlock(const APasImplBlock: TPasImplBlock; const AScope: TPasElement);
begin
  if Assigned(APasImplBlock) then begin
    if WriteFPListTitle(APasImplBlock, APasImplBlock.Elements, GElements) then begin
      PasImplElementList(APasImplBlock.Elements, AScope);
    end;
  end;
end;

procedure TPas2PhpElementImpl.PasImplIfElse(const APasImplIfElse: TPasImplIfElse; const AScope: TPasElement);
begin
  Assert(Assigned(APasImplIfElse));
  WritePhp(GPhpLang.SIf + ' (');
  PasExpr(APasImplIfElse.ConditionExpr, APasImplIfElse, AScope);
  WritePhp(') {');
  PasImplElement(APasImplIfElse.IfBranch, AScope);
  WritePhp('}');
  if Assigned(APasImplIfElse.ElseBranch) then begin
    WritePhp(GPhpLang.SElse + ' {');
    PasImplElement(APasImplIfElse.ElseBranch, AScope);
    WritePhp('}');
  end;
end;

procedure TPas2PhpElementImpl.PasImplRaise(const APasImplRaise: TPasImplRaise; const AScope: TPasElement);
begin
  Assert(Assigned(APasImplRaise));
  WritePhp(GPhpLang.SThrow + OChar.Space);
  PasExpr(APasImplRaise.ExceptObject, APasImplRaise, AScope);
  WritePhp(OChar.SemiColon);
end;

procedure TPas2PhpElementImpl.PasImplTry(const APasImplTry: TPasImplTry; const AScope: TPasElement);
begin
  Assert(Assigned(APasImplTry));
  WritePhp(GPhpLang.STry + OChar.Space + '{');
  PasImplElementList(APasImplTry.Elements, AScope);
  if APasImplTry.FinallyExcept is TPasImplTryFinally then begin
    WritePhp('throw new ' + OPasLang.SEFinally + '();');
  end;
  WritePhp('}');
  WritePhp(GPhpLang.SCatch + OChar.Space + '(Exception $exception) {');
  PasImplElementList(APasImplTry.FinallyExcept.Elements, AScope);
  if APasImplTry.FinallyExcept is TPasImplTryFinally then begin
    WritePhp('if (!($exception instanceof ' + OPasLang.SEFinally + ')) throw $exception;');
  end;
  WritePhp('}');
end;

procedure TPas2PhpElementImpl.PasImplWhileDo(const APasImplWhileDo: TPasImplWhileDo;
  const AScope: TPasElement);
begin
  Assert(Assigned(APasImplWhileDo));
  WritePhp(GPhpLang.SWhile + ' (');
  PasExpr(APasImplWhileDo.ConditionExpr, APasImplWhileDo, AScope);
  WritePhp(') {');
  PasImplElementList(APasImplWhileDo.Elements, AScope);
  WritePhp('}');
end;

procedure TPas2PhpElementImpl.PasImplRepeatUntil(const APasImplRepeatUntil: TPasImplRepeatUntil;
  const AScope: TPasElement);
begin
  Assert(Assigned(APasImplRepeatUntil));
  WritePhp(GPhpLang.SDo + ' {');
  PasImplElementList(APasImplRepeatUntil.Elements, AScope);
  WritePhp('}');
  WritePhp(GPhpLang.SWhile + '(!(');
  PasExpr(APasImplRepeatUntil.ConditionExpr, APasImplRepeatUntil, AScope);
  WritePhp('))' + OChar.SemiColon);
end;

procedure TPas2PhpElementImpl.PasImplCaseStatement(const APasImplCaseStatement: TPasImplCaseStatement;
  const AScope: TPasElement);
var
  LIndex: integer;
begin
  if WriteFPListTitle(APasImplCaseStatement, APasImplCaseStatement.Expressions, GExpressions) then begin
    for LIndex := 0 to APasImplCaseStatement.Expressions.High do begin
      WritePhp(GPhpLang.SCase + OChar.Space);
      PasPrimitiveExpr(APasImplCaseStatement.Expressions[LIndex] as TPrimitiveExpr,
        APasImplCaseStatement, AScope);
      WritePhp(': {');
      PasImplBlock(APasImplCaseStatement.Elements[LIndex] as TPasImplBlock, AScope);
      WritePhp(GPhpLang.SBreak + OChar.SemiColon);
      WritePhp('}');
    end;
  end;
end;

procedure TPas2PhpElementImpl.PasImplCaseOf(const APasImplCaseOf: TPasImplCaseOf; const AScope: TPasElement);
var
  LIndex: integer;
begin
  Assert(Assigned(APasImplCaseOf));
  WritePhp(GPhpLang.SSwitch + ' (');
  PasExpr(APasImplCaseOf.CaseExpr, APasImplCaseOf, AScope);
  WritePhp(') {');
  if WriteFPListTitle(APasImplCaseOf, APasImplCaseOf.Elements, GElements) then begin
    for LIndex := 0 to APasImplCaseOf.Elements.High do begin
      try
        PasImplCaseStatement(APasImplCaseOf.Elements[LIndex] as TPasImplCaseStatement, AScope);
      except
        WritePhp(GPhpLang.SDefault + ': {');
        PasImplBlock(APasImplCaseOf.Elements[LIndex] as TPasImplCaseElse, AScope);
        WritePhp('}');
      end;
    end;
  end;
  WritePhp('}');
end;

procedure TPas2PhpElementImpl.PasImplForLoop(const APasImplForLoop: TPasImplForLoop;
  const AScope: TPasElement);
var
  LNameEnd: string;
begin
  Assert(Assigned(APasImplForLoop));
  LNameEnd := GPhpLang.EncodeIdent(APasImplForLoop.VariableName) + '_High_';
  WritePhp(GPhpLang.SFor + ' (' + GPhpLang.EncodeIdent(APasImplForLoop.VariableName) + ' = ');
  PasExpr(APasImplForLoop.StartExpr, APasImplForLoop, AScope);
  WritePhp(', ' + LNameEnd + ' = ');
  PasExpr(APasImplForLoop.EndExpr, APasImplForLoop, AScope);
  WritePhp(OChar.SemiColon + OChar.Space + GPhpLang.EncodeIdent(APasImplForLoop.VariableName) +
    IfThen(APasImplForLoop.LoopType = ltNormal, ' <= ', ' >= ') + LNameEnd + '; ' +
    GPhpLang.EncodeIdent(APasImplForLoop.VariableName) + IfThen(APasImplForLoop.LoopType =
    ltNormal, '++', '--') + ') {');
  PasImplElementList(APasImplForLoop.Elements, AScope);
  WritePhp('}');
end;

procedure TPas2PhpGlobals.PasSectionGlobalVars(const ASection: TPasSection; const AScope: TPasElement);
var
  LIndex: integer;
  LVariable: TPasVariable;
begin
  Assert(Assigned(ASection));
  Assert(Assigned(AScope));
  if WriteFPListTitle(ASection, ASection.Variables, GVariables) then begin
    for LIndex := 0 to ASection.Variables.High do begin
      try
        LVariable := ASection.Variables[LIndex] as TPasVariable;
        if not (vmExternal in LVariable.VarModifiers) then begin
          // Search the scope to make sure its not a local.
          if LVariable = AScope._FindInScopeByName(LVariable.Name) then begin
            WritePhp(GPhpLang.SGlobal + OChar.Space + OChar.Dollar + LVariable.Name + OChar.SemiColon);
          end;
        end;
      except
      end;
    end;
  end;
end;

procedure TPas2PhpGlobals.PasModuleGlobalVars(const AModule: TPasModule; const AScope: TPasElement);
var
  LIndex: integer;
  LPackage: TPasPackage;
begin
  Assert(Assigned(AModule));
  PasSectionGlobalVars(AModule.InterfaceSection, AScope);
  PasSectionGlobalVars(AModule.ImplementationSection, AScope);
  LPackage := AModule.Parent as TPasPackage;
  for LIndex := 0 to LPackage.Modules.High do begin
    if LPackage.Modules[LIndex] <> AModule then begin
      PasSectionGlobalVars((LPackage.Modules[LIndex] as TPasModule).InterfaceSection, AScope);
    end;
  end;
end;

procedure TPas2PhpProcedure.PasEnumType(const AEnumType: TPasEnumType; const AScope: TPasElement);
var
  LIndex: integer;
  LValue: TPasEnumValue;
begin
  Assert(Assigned(AEnumType));
  for LIndex := 0 to AEnumType.Values.High do begin
    LValue := AEnumType.Values[LIndex] as TPasEnumValue;
    IfThenUnsupported(Assigned(LValue.Value), 'Enum Values');
    WritePhp('@' + GPhpLang.SDefine + '(' + GPhpLang.EncodeString(LValue.Name) +
      ', ' + IntToStr(LIndex) + ');');
  end;
end;

procedure TPas2PhpProcedure.PasRecordType(const ARecordType: TPasRecordType; const AScope: TPasElement);
var
  LIndex: integer;
begin
  Assert(Assigned(ARecordType));
  WritePhp(GPhpLang.SClass + OChar.Space + ARecordType.Name + OChar.Space + '{');
  if WriteFPListTitle(ARecordType, ARecordType.Members, GMembers) then begin
    for LIndex := 0 to ARecordType.Members.High do begin
      try
        PasVariable(ARecordType.Members[LIndex] as TPasVariable, AScope, True);
      except
        RaiseException(ARecordType, GUnknownClassTypeDot, 'PasRecordType');
      end;
    end;
  end;
  WritePhp('}');
end;

procedure TPas2PhpProcedure.PasClassTypeProperties(const AClassType: TPasClassType;
  const AScope: TPasElement);
var
  LIndex: integer;
  LProperty: TPasProperty;
  LPropertyDef: TPasElement;
begin
  Assert(Assigned(AClassType));
  if WriteFPListTitle(AClassType, AClassType.Members, GMembers) then begin
    WritePhp('function __get($name) {');
    for LIndex := 0 to AClassType.Members.High do begin
      if AClassType.Members[LIndex] is TPasProperty then begin
        LProperty := TPasProperty(AClassType.Members[LIndex]);
        IfThenUnsupported(LProperty.Args.Count > 0, 'Property arguments');
        if LProperty.ReadAccessorName <> EmptyStr then begin
          WriteDeclaration(LProperty);
          WritePhp('if (strcasecmp($name, "' + LProperty.Name + '") === 0) {');
          LPropertyDef := AClassType._FindInScopeByName(LProperty.ReadAccessorName);
          WritePhp('return $this->' + LPropertyDef.Name);
          if LPropertyDef is TPasFunction then begin
            WritePhp('()');
          end else if LPropertyDef is TPasVariable then begin
          end else begin
            RaiseException(LPropertyDef, GUnknownClassTypeDot, 'PasClassTypeProperties');
          end;
          WritePhp(';');
          WritePhp('}');
        end;
      end;
    end;
    WritePhp('return parent::__get($name);');
    WritePhp('}');
    WritePhp('function __set($name, $value) {');
    for LIndex := 0 to AClassType.Members.High do begin
      if AClassType.Members[LIndex] is TPasProperty then begin
        LProperty := TPasProperty(AClassType.Members[LIndex]);
        IfThenUnsupported(LProperty.Args.Count > 0, 'Property arguments');
        if LProperty.WriteAccessorName <> EmptyStr then begin
          WriteDeclaration(LProperty);
          WritePhp('if (strcasecmp($name, "' + LProperty.Name + '") === 0) {');
          LPropertyDef := AClassType._FindInScopeByName(LProperty.WriteAccessorName);
          WritePhp('return $this->' + LPropertyDef.Name);
          if LPropertyDef is TPasProcedure then begin
            WritePhp('($value)');
          end else if LPropertyDef is TPasVariable then begin
            WritePhp(' = $value');
          end else begin
            RaiseException(LPropertyDef, GUnknownClassTypeDot, 'PasClassTypeProperties');
          end;
          WritePhp(';');
          WritePhp('}');
        end;
      end;
    end;
    WritePhp('return parent::__set($name, $value);');
    WritePhp('}');
  end;
end;

procedure TPas2PhpProcedure.PasClassType(const AClassType: TPasClassType; const AScope: TPasElement);
var
  LIndex: integer;
  LProcedureFace, LProcedureImpl: TPasProcedure;
  LMember: TPasElement;
begin
  Assert(Assigned(AClassType));
  WritePhp(GPhpLang.SClass + OChar.Space + AClassType.Name);
  if not SameText(AClassType.Name, OPasLang.STObject) then begin
    AClassType._FillAncestorType;
    WritePhp(OChar.Space + GPhpLang.SExtends + OChar.Space + AClassType.AncestorType.Name);
  end;
  WritePhp(OChar.Space + '{');
  PasClassTypeProperties(AClassType, AScope);
  if WriteFPListTitle(AClassType, AClassType.Members, GMembers) then begin
    for LIndex := 0 to AClassType.Members.High do begin
      LMember := AClassType.Members[LIndex];
      if LMember is TPasProcedure then begin
        LProcedureFace := LMember as TPasProcedure;
        if LProcedureFace.IsAbstract or LProcedureFace.IsDynamic then begin
          PasProcedure(LProcedureFace, AClassType);
        end else begin
          LProcedureImpl := AClassType._FindModule.ImplementationSection.Functions._FindElementByName(
            AClassType.Name + '.' + LProcedureFace.Name) as
            TPasProcedure;
          Assert(LProcedureFace.Body = nil);
          LProcedureFace.Body := LProcedureImpl.Body;
          try
            PasProcedure(LProcedureFace, AClassType);
          finally
            LProcedureFace.Body := nil
          end;
        end;
      end else if LMember is TPasVariable then begin
        if not (LMember is TPasProperty) then begin
          PasVariable(LMember as TPasVariable, AScope, True);
        end;
      end else begin
        RaiseException(LMember, GUnknownClassTypeDot, 'PasClassType');
      end;
    end;
  end;
  WritePhp('}');
end;

procedure TPas2PhpProcedure.PasElementLists(const AOwner: TPasElement;
  const AConsts, ATypes, AClasses, AVariables, AFunctions: TFPList; const AScope: TPasElement);
var
  LIndex: integer;
begin
  if WriteFPListTitle(AOwner, AConsts, GConstants) then begin
    for LIndex := 0 to AConsts.High do begin
      PasConstant(AConsts[LIndex] as TPasConst);
    end;
  end;
  if WriteFPListTitle(AOwner, ATypes, GTypes) then begin
    for LIndex := 0 to ATypes.High do begin
      try
        PasRecordType(ATypes[LIndex] as TPasRecordType, AScope);
      except
        try
          PasEnumType(ATypes[LIndex] as TPasEnumType, AScope);
        except
          RaiseException(ATypes[LIndex], GUnknownClassTypeDot, 'PasElementLists');
        end;
      end;
    end;
  end;
  if WriteFPListTitle(AOwner, AClasses, GClasses) then begin
    for LIndex := 0 to AClasses.High do begin
      PasClassType(AClasses[LIndex] as TPasClassType, AScope);
    end;
  end;
  if WriteFPListTitle(AOwner, AVariables, GVariables) then begin
    for LIndex := 0 to AVariables.High do begin
      PasVariable(AVariables[LIndex] as TPasVariable, AScope);
    end;
  end;
  if WriteFPListTitle(AOwner, AFunctions, GFunctions) then begin
    for LIndex := 0 to AFunctions.High do begin
      PasProcedure(AFunctions[LIndex] as TPasProcedure, AOwner);
    end;
  end;
end;

procedure TPas2PhpProcedure.PasProcedurePrototype(const AProcedure: TPasProcedure;
  const AScope: TPasElement);
var
  LName: string;
  LProcedureFace: TPasProcedure;
begin
  Assert(Assigned(AProcedure) and Assigned(AScope));
  try
    LProcedureFace := ((AScope as TImplementationSection).Parent as
      TPasModule).InterfaceSection.Functions._FindElementByName(AProcedure.Name) as TPasProcedure;
  except
    LProcedureFace := AProcedure;
  end;
  if AProcedure is TPasDestructor then begin
    LName := GPhpLang.SDestruct;
  end else if AProcedure is TPasConstructor then begin
    LName := GPhpLang.SConstruct;
  end else begin
    LName := Copy(AProcedure.Name, Pos('.', AProcedure.Name) + 1, MaxInt);
  end;
  WriteDeclaration(AProcedure);
  WritePhp(GPhpLang.SFunction + OChar.Space + LName + '(');
  PasProcedureType(LProcedureFace.ProcType);
  WritePhp(')');
end;

procedure TPas2PhpProcedure.PasProcedure(const AProcedure: TPasProcedure; const AScope: TPasElement);
var
  LPos: integer;
  LResult: TPasVariable;
begin
  Assert(Assigned(AProcedure));
  if not AProcedure.IsExternal then begin
    LPos := Pos('.', AProcedure.Name);
    if (AScope is TPasProcedure) or (AScope is TProcedureBody) or
      ((AScope is TImplementationSection) and (LPos = 0)) or (AScope is TPasClassType) then begin
      PasProcedurePrototype(AProcedure, AScope);
      WritePhp(OChar.Space + '{');
      if AProcedure.IsAbstract or AProcedure.IsDynamic then begin
        WritePhp('throw new EAbstractError("' + AProcedure.Name + '");');
      end else if Assigned(AProcedure.Body) then begin
        PasModuleGlobalVars(AProcedure._FindModule, AProcedure);
        if AProcedure is TPasFunction then begin
          try
            AProcedure.Body.Variables._FindElementByName(OPasLang.SResult);
          except
            LResult := TPasVariable.Create(OPasLang.SResult, nil);
            LResult.VarType := ((AProcedure as TPasFunction).FuncType as
              TPasFunctionType).ResultEl.ResultType;
            LResult.VarType.AddRef;
            AProcedure.Body.Variables.Add(LResult);
          end;
        end;
        PasElementLists(AProcedure, AProcedure.Body.Consts, AProcedure.Body.Types, AProcedure.Body.Classes,
          AProcedure.Body.Variables, AProcedure.Body.Functions, AProcedure);
        if WriteFPListTitle(AProcedure.Body.Body, AProcedure.Body.Body.Elements, GElements) then begin
          PasImplElementList(AProcedure.Body.Body.Elements, AProcedure);
        end;
        if AProcedure is TPasFunction then begin
          WritePhp(GPhpLang.SReturn + OChar.Space + GPhpLang.EncodeIdent(OPasLang.SResult) +
            OChar.SemiColon);
        end;
      end;
      WritePhp('}');
    end;
  end;
end;

procedure TPas2PhpSection.PasUnitRef(const AUnitRef: TPasUnresolvedUnitRef);
begin
  Assert(Assigned(AUnitRef));
  WritePhpIncludeOnce(UnitNameTranslate(AUnitRef.Name) + OFileName.ExtPhp);
end;

procedure TPas2PhpSection.PasSection(const APasSection: TPasSection);
var
  LIndex: integer;
begin
  if Assigned(APasSection) then begin
    if WriteFPListTitle(APasSection, APasSection.UsesList, GUsesList) then begin
      for LIndex := 0 to APasSection.UsesList.High do begin
        PasUnitRef(APasSection.UsesList[LIndex] as TPasUnresolvedUnitRef);
      end;
    end;
    PasElementLists(APasSection, APasSection.Consts, APasSection.Types, APasSection.Classes,
      APasSection.Variables,
      APasSection.Functions, nil);
  end;
end;

procedure TPas2PhpSection.PasModule(const AModule: TPasModule; const AStream: TStream;
  const AIsEntryModule: boolean);
begin
  Assert(Assigned(AModule));
  SetOutputStream(AStream);
  FPasModule := AModule;
  WritePhp('<?PHP');
  NewLine;
  if not (p2pSrcOutNoHeader in Options) then begin
    WritePhpComment(EmptyStr);
    WritePhpComment(GConvertedFromPascalBy + OChar.Space + OProduct.NameVersion);
    WritePhpComment(OProduct.Slogan);
    WritePhpComment(OProduct.Website);
    WritePhpComment(OProduct.NameVersion + ' is ' + OProduct.Copyright);
    WritePhpComment(EmptyStr);
  end;
  if AIsEntryModule then begin
    WritePhpIncludeOnce(FConfigFileName);
  end;
  PasSection(AModule.InterfaceSection);
  PasSection(AModule.ImplementationSection);
  PasImplBlock(AModule.InitializationSection, AModule);
  WritePhp('?>');
end;

end.
