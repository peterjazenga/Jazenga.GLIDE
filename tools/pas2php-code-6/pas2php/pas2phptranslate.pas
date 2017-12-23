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

unit Pas2PhpTranslate;

{$INCLUDE pas2php.inc}

interface

uses Pas2PhpDefines, Pas2PhpUtils, PasTree, StrUtils, SysUtils;

type

  OLangCommon = object
  const
    SBreak = 'break';
    SCase = 'case';
    SChar = 'char';
    SClass = 'class';
    SDefault = 'default';
    SDo = 'do';
    SDouble = 'double';
    SElse = 'else';
    SFalse = 'false';
    SFor = 'for';
    SFunction = 'function';
    SIf = 'if';
    SNull = 'null';
    SString = 'string';
    STrue = 'true';
    STry = 'try';
    SVar = 'var';
    SWhile = 'while';
  end;

  OPasLang = object(OLangCommon)
  const
    UnitNamePrefix = '__';
    PhpLibNamePrefix = 'php_';
  const
    SAnsiString = 'AnsiString';
    SAssert = 'Assert';
    SCopy = 'Copy';
    SEFinally = 'EFinally';
    SEnforce = 'Enforce';
    SExit = 'Exit';
    SExtended = 'Extended';
    SInt64 = 'Int64';
    SInteger = 'Integer';
    SLongInt = 'LongInt';
    SMidStr = 'MidStr';
    SOleVariant = 'OleVariant';
    SOrd = 'Ord';
    SPos = 'Pos';
    SPosEx = 'PosEx';
    SReal = 'Real';
    SResult = 'Result';
    SShortString = 'ShortString';
    SSingle = 'Single';
    SSmallInt = 'SmallInt';
    SSystem = 'System';
    SSystemOrd = 'SystemOrd';
    STObject = 'TObject';
    SVariant = 'Variant';
    SWideString = 'WideString';
  public
    class function AliasName(const AName: string): string; static;
  public
    class function DecodeBoolean(const AValue: string): boolean; static;
    class function DecodeInteger(const AValue: string): integer; static;
    class function DecodeFloat(const AValue: string): extended; static;
    class function DecodeString(const AValue: string): string; static;
  end;

  GPhpLang = object(OLangCommon)
  const
    SAbstract = 'abstract';
    SArray = 'array';
    SCatch = 'catch';
    SConstruct = '__construct';
    SDefine = 'define';
    SDeRef = '->';
    SDestruct = '__destruct';
    SEmptyBrackets = '()';
    SExtends = 'extends';
    SFloatVal = 'floatval';
    SGlobal = 'global';
    SInArray = 'in_array';
    SIncludeOnce = 'include_once';
    SInt = 'int';
    SIntVal = 'intval';
    SNew = 'new';
    SParent = 'parent';
    SReturn = 'return';
    SStringQuote = OChar.DoubleQuote;
    SStrVal = 'strval';
    SSwitch = 'switch';
    SThis = OChar.Dollar + 'this';
    SThisDeRef = SThis + SDeRef;
    SThrow = 'throw';
  public
    class function EncodeBoolean(const AValue: boolean): string; static;
    class function EncodeInteger(const AValue: integer): string; static;
    class function EncodeFloat(const AValue: extended): string; static;
    class function EncodeString(const AValue: string): string; static;
    class function EncodeIdent(const AString: string; const AIsSubIdent: boolean = False): string; static;
    class function EncodeExprOpCode(const AExprOpCode: TExprOpCode): string; static;
  end;

function UnitNameTranslate(const AUnitName: string): string;

implementation

function UnitNameTranslate(const AUnitName: string): string;
begin
  Result := LowerCase(AUnitName);
  if AnsiStartsText(OPasLang.UnitNamePrefix, Result) then begin
    Result := Copy(Result, Length(OPasLang.UnitNamePrefix) + 1, MaxInt);
  end;
end;

class function OPasLang.AliasName(const AName: string): string;
begin
  Result := AName;
  if AnsiStartsText(OPasLang.PhpLibNamePrefix, Result) then begin
    Result := Copy(Result, Length(OPasLang.PhpLibNamePrefix) + 1, MaxInt);
  end else if SameText(Result, OPasLang.SOrd) then begin
    Result := OPasLang.SSystemOrd;
  end else if SameText(Result, OPasLang.SPos) then begin
    Result := OPasLang.SPosEx;
  end else if SameText(Result, OPasLang.SCopy) then begin
    Result := OPasLang.SMidStr;
  end else if SameText(Result, OPasLang.SExit) then begin
    Result := GPhpLang.SReturn;
  end else if SameText(Result, OPasLang.SAssert) then begin
    Result := OPasLang.SEnforce;
  end;
end;

class function OPasLang.DecodeInteger(const AValue: string): integer;
begin
  Result := StrToInt(AValue);
end;

class function OPasLang.DecodeFloat(const AValue: string): extended;
begin
  Result := StrToFloat(AValue);
end;

class function OPasLang.DecodeBoolean(const AValue: string): boolean;
begin
  if SameText(AValue, STrue) then begin
    Result := True;
  end else if SameText(AValue, SFalse) then begin
    Result := False;
  end else begin
    RaiseException('Unable to convert boolean');
  end;
end;

class function OPasLang.DecodeString(const AValue: string): string;
var
  APtr, LToken: PChar;
  LString: string;
begin
  Result := EmptyStr;
  APtr := PChar(AValue);
  while APtr[0] <> #0 do begin
    case APtr[0] of
      #9, #10, #13, ' ', '+': begin
        Inc(APtr);
      end;
      '#': begin
        Inc(APtr);
        LToken := APtr;
        while APtr[0] in ['0'..'9'] do begin
          Inc(APtr);
        end;
        SetString(LString, LToken, APtr - LToken);
        Result += Chr(StrToInt(LString));
      end;
      OChar.SingleQuote: begin
        LToken := APtr;
        Inc(APtr);
        while (APtr[0] <> #0) and ((APtr[0] <> OChar.SingleQuote) or (APtr[1] = OChar.SingleQuote)) do begin
          if (APtr[0] = OChar.SingleQuote) and (APtr[1] = OChar.SingleQuote) then begin
            Inc(APtr, 2);
          end else begin
            Inc(APtr);
          end;
        end;
        if APtr[0] <> OChar.SingleQuote then begin
          RaiseException(GBadPascalString);
        end;
        Inc(APtr);
        SetString(LString, LToken, APtr - LToken);
        if Length(LString) > 2 then begin
          Result += AnsiDequotedStr(LString, OChar.SingleQuote);
        end;
      end else begin
        RaiseException(GBadPascalString);
      end;
    end;
  end;
end;

class function GPhpLang.EncodeBoolean(const AValue: boolean): string;
begin
  if AValue then begin
    Result := STrue;
  end else begin
    Result := SFalse;
  end;
end;

class function GPhpLang.EncodeInteger(const AValue: integer): string;
begin
  Result := IntToStr(AValue);
end;

class function GPhpLang.EncodeFloat(const AValue: extended): string;
begin
  Result := FloatToStr(AValue);
end;

// TODO: This needs work.
class function GPhpLang.EncodeString(const AValue: string): string;
begin
  Result := AValue;
  Result := ReplaceStr(Result, '\', '\\');
  Result := ReplaceStr(Result, '"', '\"');
  Result := ReplaceStr(Result, '$', '\$');
  Result := ReplaceStr(Result, #9, '\t');
  Result := ReplaceStr(Result, #10, '\n');
  Result := ReplaceStr(Result, #13, '\r');
  Result := GPhpLang.SStringQuote + Result + GPhpLang.SStringQuote;
end;

class function GPhpLang.EncodeIdent(const AString: string; const AIsSubIdent: boolean): string;
begin
  Result := Trim(AString);
  Assert(Result <> EmptyStr);
  if not AIsSubIdent then begin
    Result := OChar.Dollar + Result;
  end;
end;

class function GPhpLang.EncodeExprOpCode(const AExprOpCode: TExprOpCode): string;
const
  LExprOpCodeStrings: array[TExprOpCode] of string =
    ('', ' + ', ' - ', ' * ', ' / ', ' / ', ' % ', ' ** ', ' >> ', ' << ', '!',
    ' && ', ' || ', ' ^ ', ' == ',
    ' != ', ' < ', ' > ', ' <= ', ' >= ', ', ', ' instanceof ', ' as ', ' ????? ', ' & ', ' * ', '->');
begin
  Result := LExprOpCodeStrings[AExprOpCode];
end;

end.
