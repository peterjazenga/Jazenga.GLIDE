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

unit Pas2PhpPasElement;

{$INCLUDE pas2php.inc}

interface

uses
  Pas2PhpPasConst, Pas2PhpPasType, PasTree, SysUtils;

type

  TPasElement = PasTree.TPasElement;

  TPasElementHelper = class helper for TPasElement
  strict private
    procedure _SetCustomElement(const AElement: TPasElement);
    function _GetCustomElement: TPasElement;
  public
    function _FindRootByClass(const AClass: TPTreeElement): TPasElement;
    function _IsName(const AName: string): boolean;
    function _IsParamsExprAndIsValue(const AValue: TPasExpr): boolean;
    function _IsBinaryLeft(const AClassLeft: TPTreeElement): boolean;
    function _IsClassMember: boolean;
    function _GetDescription: string;
    function _FindModule: TPasModule;
    function _GetPasType(const ADecodeArray: boolean): TPasType;
    function _GetPas2PhpVarType(const ADecodeArray: boolean): TPas2PhpVarType;
    function _GetPas2PhpVarType(const AName: string; const ADecodeArray: boolean): TPas2PhpVarType;
    function _FindInScopeByName(const AName: string): TPasElement;
  public
    property CustomElement: TPasElement read _GetCustomElement write _SetCustomElement;
  end;

implementation

uses Pas2PhpFPList, Pas2PhpPasDeclarations, Pas2PhpPasRecordType, Pas2PhpPasClassType,
  Pas2PhpPasModule;

function TPasElementHelper._FindRootByClass(const AClass: TPTreeElement): TPasElement;
begin
  Assert(Assigned(Self));
  Result := Self;
  while Assigned(Result) and not (Result is AClass) do begin
    Result := Result.Parent;
  end;
  if not Assigned(Result) then begin
    Abort;
  end;
end;

function TPasElementHelper._IsName(const AName: string): boolean;
  //var
  //LPos1, LPos2: Integer;
  //LTopName, LSubName: String;
begin
  Assert(Assigned(Self));
  //LPos2 := Pos('.', AName);
  //LPos1 := Pos('.', AElement.Name);
  //LTopName := Copy(AElement.Name, 1, LPos1 - 1);
  //LSubName := Copy(AElement.Name, LPos1 + 1, MaxInt);
  //  Result := ((LPos = 0) or SameText(Copy(AName, 1, LPos2 - 1), LTopName)) and
  Result := SameText(Name, AName);
end;

function TPasElementHelper._IsParamsExprAndIsValue(const AValue: TPasExpr): boolean;
begin
  Result := (Self is TParamsExpr) and (TParamsExpr(Self).Value = AValue);
end;

function TPasElementHelper._IsBinaryLeft(const AClassLeft: TPTreeElement): boolean;
begin
  try
    Result := (Self as TBinaryExpr).left is AClassLeft;
  except
    Result := False;
  end;
end;

function TPasElementHelper._IsClassMember: boolean;
begin
  Assert(Assigned(Self));
  Result := not (Self is TPasArgument);
  if Result then begin
    try
      _FindRootByClass(TPasClassType);
      Result := True;
    except
      Result := False;
    end;
  end;
end;

procedure TPasElementHelper._SetCustomElement(const AElement: TPasElement);
begin
  Assert(Assigned(Self));
  Assert(Assigned(AElement));
  Assert((CustomData = nil) or (CustomData = AElement));
  CustomData := AElement;
end;

function TPasElementHelper._GetCustomElement: TPasElement;
begin
  Result := CustomData as TPasElement;
end;

// UNUSED
function TPasElementHelper._GetDescription: string;
begin
  Assert(Assigned(Self));
  Result := '[' + Name + ':' + ClassName + ']';
  if Assigned(Parent) then begin
    Result := Parent._GetDescription + Result;
  end;
end;

function TPasElementHelper._FindModule: TPasModule;
begin
  Result := _FindRootByClass(TPasModule) as TPasModule;
end;

function TPasElementHelper._GetPasType(const ADecodeArray: boolean): TPasType;
begin
  try
    Result := (Self as TPasVariable).VarType;
    if ADecodeArray and (Result is TPasArrayType) then begin
      Result := TPasArrayType(Result).ElType._GetPasType(ADecodeArray);
    end;
  except
    try
      Result := (Self as TPasArgument).ArgType;
    except
      try
        Result := (Self as TPasFunction).FuncType.ResultEl.ResultType;
      except
        try
          Result := (Self as TPasResultElement).ResultType;
        except
          Result := Self as TPasType;
        end;
      end;
    end;
  end;
end;

function TPasElementHelper._GetPas2PhpVarType(const ADecodeArray: boolean): TPas2PhpVarType;
begin
  try
    Result := _GetPasType(ADecodeArray)._GetPhpVarType;
  except
    Result := (Self as TPasConst)._GetPhpVarType;
  end;
end;

function TPasElementHelper._GetPas2PhpVarType(const AName: string;
  const ADecodeArray: boolean): TPas2PhpVarType;
begin
  Result := _FindInScopeByName(AName)._GetPas2PhpVarType(ADecodeArray);
end;

// OLD Original function. WHAT A MESS!
function FindInScopeByName(const AScope: TPasElement; const AName: string): TPasElement;
begin
  Assert(Assigned(AScope));
  if not IsValidIdent(AName) then begin
    Abort;
  end;
  try
    Result := (AScope as TPasProcedure).ProcType.Args._FindElementByName(AName);
  except
    try
      Result := (AScope as TPasProcedure).Body._FindDeclarationsElementByName(AName);
    except
      try
        Result := (AScope as TPasDeclarations)._FindDeclarationsElementByName(AName);
      except
        try
          Result := (AScope as TPasRecordType)._FindRecordMemberByName(AName);
        except
          try
            Result := (AScope as TPasClassType)._FindClassMemberByName(AName);
          except
            try
              (AScope as TPasClassType)._FillAncestorType;
              Result := FindInScopeByName(FindInScopeByName(AScope.Parent,
                ((AScope as TPasClassType).AncestorType as TPasType).Name) as TPasClassType, AName);
            except
              try
                Result := (AScope as TPasModule)._ModuleFindElementByName(AName);
              except
                if Assigned(AScope.Parent) then begin
                  Result := FindInScopeByName(AScope.Parent, AName);
                end else begin
                  Abort;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

function TPasElementHelper._FindInScopeByName(const AName: string): TPasElement;
begin
  Assert(Assigned(Self));
  if not IsValidIdent(AName) then begin
    Abort;
  end;
  try
    Result := (Self as TPasProcedure).ProcType.Args._FindElementByName(AName);
  except
    try
      Result := (Self as TPasProcedure).Body._FindDeclarationsElementByName(AName);
    except
      try
        Result := (Self as TPasDeclarations)._FindDeclarationsElementByName(AName);
      except
        try
          Result := (Self as TPasRecordType)._FindRecordMemberByName(AName);
        except
          try
            Result := (Self as TPasClassType)._FindClassMemberByName(AName);
          except
            try
              (Self as TPasClassType)._FillAncestorType;
              Result :=
                (Parent._FindInScopeByName(
                ((Self as TPasClassType).AncestorType as TPasType).Name) as
                TPasClassType)._FindInScopeByName(AName);
            except
              try
                Result := (Self as TPasModule)._ModuleFindElementByName(AName);
              except
                if Assigned(Parent) then begin
                  Result := Parent._FindInScopeByName(AName);
                end else begin
                  Abort;
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
