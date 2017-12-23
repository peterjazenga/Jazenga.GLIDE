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

unit P2PVariants;

interface

uses
  HttpDefs, P2PSysUtils, SysUtils, Variants;

type

  TVarCompareMode = (vcmStrict, vcmLoose, vcmCaseSensitive, vcmCaseInsensitive);

function VarIsSameType(const A, B: variant): boolean;
function VarIsSame(const A, B: variant; const AMode: TVarCompareMode): boolean;

function VarArrayLow(const AArray: variant): integer;
function VarArrayHigh(const AArray: variant): integer;
function VarArrayLength(const AArray: variant): integer;
procedure VarArraySetLength(var AArray: variant; const AD1: integer; const AD2: integer = -1;
  const AD3: integer = -1);
function VarArrayGetItem(var AArray: variant; const AIndex: integer; const ADefault: variant): variant;

function VarArrayGetValue(const AArray: variant; const AName: string): string;
function VarArrayGetValueHttp(const AArray: variant; const AName: string): string;

function VarArrayIndex(const AValue: variant; const AArray: variant; const AMode: TVarCompareMode): integer;
function VarArrayMatch(const AValue: variant; const AArray: variant; const AMode: TVarCompareMode): boolean;
function VarArrayMatchStr(const AValue: variant; const AArray: variant): boolean;
function VarArrayMatchText(const AValue: variant; const AArray: variant): boolean;
function VarArrayIndexStr(const AValue: variant; const AArray: variant): integer;
function VarArrayIndexText(const AValue: variant; const AArray: variant): integer;

implementation

function VarGetItem(const AVariant: variant; const AIndex: integer): variant;
begin
  if VarIsStr(AVariant) then begin
    // TODO: Check for PAS2PHP
    Result := AVariant[AIndex - 1];
  end else begin
    Result := AVariant[AIndex];
  end;
end;

procedure VarSetItem(var AVariant: variant; const AIndex: integer; const AItem: variant);
begin
  if VarIsStr(AVariant) then begin
    // TODO: Check for PAS2PHP
    AVariant[AIndex - 1] := AItem;
  end else begin
    AVariant[AIndex] := AItem;
  end;
end;

function VarArrayLow(const AArray: variant): integer;
begin
  Result := 0;
end;

function VarArrayHigh(const AArray: variant): integer;
begin
  if VarIsArray(AArray) then begin
    Result := VarArrayHighBound(AArray, 1);
  end else begin
    Result := -1;
  end;
end;

function VarArrayLength(const AArray: variant): integer;
begin
  Result := VarArrayHigh(AArray) + 1;
end;

procedure VarArraySetLength1(var AArray: variant; const ALength: integer);
begin
  if not VarIsArray(AArray) then begin
    AArray := VarArrayOf([]);
  end;
  VarArrayRedim(AArray, ALength - 1);
end;

procedure VarArraySetLength(var AArray: variant; const AD1, AD2, AD3: integer);
var
  I, J: integer;
begin
  VarArraySetLength1(AArray, AD1);
  if AD2 >= 0 then begin
    for I := 0 to AD1 - 1 do begin
      VarArraySetLength1(AArray[I], AD2);
    end;
    if AD3 >= 0 then begin
      for I := 0 to AD1 - 1 do begin
        for J := 0 to AD2 - 1 do begin
          VarArraySetLength1(AArray[I][J], AD2);
        end;
      end;
    end;
  end;
end;

function VarArrayGetItem(var AArray: variant; const AIndex: integer; const ADefault: variant): variant;
begin
  if (AIndex < 0) or (AIndex > VarArrayHigh(AArray)) then begin
    Result := ADefault;
  end else begin
    Result := AArray[AIndex];
  end;
end;

function VarIsSameType(const A, B: variant): boolean;
begin
{$IFDEF PAS2PHP}
  Result := php_gettype(A) = php_gettype(B);
{$ELSE}
  Result := VarType(A) = VarType(B);
{$ENDIF}
end;

function VarIsSame(const A, B: variant; const AMode: TVarCompareMode): boolean;
begin
  case AMode of
    vcmStrict: begin
      Result := VarIsSameType(A, B) and (A = B);
    end;
    vcmLoose: begin
      Result := A = B;
    end;
    vcmCaseInsensitive: begin
      Result := AnsiSame(VarToStr(A), VarToStr(B), False);
    end;
    vcmCaseSensitive: begin
      Result := AnsiSame(VarToStr(A), VarToStr(B), True);
    end;
  end;
end;

function VarArrayIndex(const AValue: variant; const AArray: variant; const AMode: TVarCompareMode): integer;
begin
  for Result := 0 to VarArrayHigh(AArray) do begin
    if VarIsSame(AValue, AArray[Result], AMode) then begin
      Exit;
    end;
  end;
  Result := -1;
end;

function VarArrayMatch(const AValue: variant; const AArray: variant; const AMode: TVarCompareMode): boolean;
var
  LIndex: integer;
begin
  for LIndex := 0 to VarArrayHigh(AArray) do begin
    if VarIsSame(AValue, AArray[LIndex], AMode) then begin
      Exit(True);
    end;
  end;
  Result := False;
end;

function VarArrayMatchStr(const AValue: variant; const AArray: variant): boolean;
begin
  Result := VarArrayMatch(AValue, AArray, vcmCaseSensitive);
end;

function VarArrayMatchText(const AValue: variant; const AArray: variant): boolean;
begin
  Result := VarArrayMatch(AValue, AArray, vcmCaseInsensitive);
end;

function VarArrayIndexStr(const AValue: variant; const AArray: variant): integer;
begin
  Result := VarArrayIndex(AValue, AArray, vcmCaseSensitive);
end;

function VarArrayIndexText(const AValue: variant; const AArray: variant): integer;
begin
  Result := VarArrayIndex(AValue, AArray, vcmCaseInsensitive);
end;

function VarArrayGetValue(const AArray: variant; const AName: string): string;
var
  LIndex, LPos: integer;
  LString: string;
begin
  Result := EmptyStr;
  for LIndex := 0 to VarArrayLength(AArray) - 1 do begin
    LString := AArray[LIndex];
    LPos := Pos('=', LString);
    if LPos > 0 then begin
      if SameText(Copy(LString, 1, LPos - 1), AName) then begin
        Result := Copy(LString, LPos + 1, MaxInt);
      end;
    end;
  end;
end;

function VarArrayGetValueHttp(const AArray: variant; const AName: string): string;
begin
  Result := HttpDecode(VarArrayGetValue(AArray, AName));
end;

end.
