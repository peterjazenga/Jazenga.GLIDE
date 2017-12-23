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

unit __System;

interface

uses P2PVariants, PhpLib, SysUtils, Variants;

{$IFDEF PAS2PHP}

const
  MaxInt = $7fffffff;
  DirectorySeparator = DIRECTORY_SEPARATOR;
  ExtensionSeparator = '.';
  DriveSeparator = ':';
  PathSeparator = ';';
  LineEnding = #13#10;
  MaxPathLen = 260;
  AllFilesMask = '*';

var GLOBALS: Variant; external;
var _SERVER: Variant; external;
var _GET: Variant; external;
var _POST: Variant; external;
var _COOKIE: Variant; external;
var _FILE: Variant; external;
var _ENV: Variant; external;
var _REQUEST: Variant; external;
var _SESSION: Variant; external;

const
  Null = null;
  Unassigned = null;

type

  TObject = class
    constructor Create;
    destructor Destroy; virtual;
    procedure Free;
    function ClassName: string;
  end;

procedure return(const AVariant: variant);
function string(const AVariant: variant): string;
function shortstring(const AVariant: variant): string;
function ansistring(const AVariant: variant): string;
function widestring(const AVariant: variant): string;

{$ENDIF}

function char(const AInteger: integer): string;
function integer(const AVariant: variant): integer;
function int64(const AVariant: variant): integer;
function smallint(const AVariant: variant): integer;
function longint(const AVariant: variant): integer;
function single(const AVariant: variant): extended;
function double(const AVariant: variant): extended;
function extended(const AVariant: variant): extended;
function real(const AVariant: variant): extended;
function variant(const AVariant: variant): variant;

function AllowDirectorySeparators: variant;
function Assigned(const AVariant: variant): boolean;
function High(const AVariant: variant): integer;
function Length(const AVariant: variant): integer;
function Low(const AVariant: variant): integer;
function MidStr(const AString: string; const AOffset, ALength: integer): string;
function Pred(const AVariant: variant): variant;
function Succ(const AVariant: variant): variant;
function SystemOrd(const AVariant: variant): integer;
function Trunc(const AExtended: extended): extended;
procedure Dec(var AInteger: integer; const AOffset: integer = 1);
procedure Enforce(const ATrue: boolean; const AMessage: string = 'Enforce');
procedure Inc(var AInteger: integer; const AOffset: integer = 1);
procedure SetLength(var AVariant: variant; const AD1: integer; const AD2: integer = -1;
  const AD3: integer = -1);
procedure Write;
procedure WriteLn;
function upcase(const AString: string): string;
function lowercase(const AString: string): string;

implementation

{$IFDEF PAS2PHP}

function string(const AVariant: variant): string;
begin
  Result := php_strval(AVariant);
end;

function shortstring(const AVariant: variant): string;
begin
  Result := php_strval(AVariant);
end;

function ansistring(const AVariant: variant): string;
begin
  Result := php_strval(AVariant);
end;

function widestring(const AVariant: variant): string;
begin
  Result := php_strval(AVariant);
end;

constructor TObject.Create;
begin
end;

destructor TObject.Destroy;
begin
end;

function TObject.ClassName: string;
begin
  Result := php_get_class(Self);
end;

procedure TObject.Free;
begin
end;

{$ENDIF}

function char(const AInteger: integer): string;
begin
  Result := php_chr(AInteger);
end;

function integer(const AVariant: variant): integer;
begin
  Result := php_intval(AVariant, 10);
end;

function int64(const AVariant: variant): integer;
begin
  Result := php_intval(AVariant, 10);
end;

function smallint(const AVariant: variant): integer;
begin
  Result := php_intval(AVariant, 10);
end;

function longint(const AVariant: variant): integer;
begin
  Result := php_intval(AVariant, 10);
end;

function single(const AVariant: variant): extended;
begin
  Result := php_floatval(AVariant);
end;

function double(const AVariant: variant): extended;
begin
  Result := php_floatval(AVariant);
end;

function extended(const AVariant: variant): extended;
begin
  Result := php_floatval(AVariant);
end;

function real(const AVariant: variant): extended;
begin
  Result := php_floatval(AVariant);
end;

function variant(const AVariant: variant): variant;
begin
  Result := AVariant;
end;

function Assigned(const AVariant: variant): boolean;
begin
  Result := not php_is_null(AVariant);
end;

procedure Enforce(const ATrue: boolean; const AMessage: string);
begin
  if not ATrue then begin
    raise EAssertionFailed.Create(AMessage);
  end;
end;

function Trunc(const AExtended: extended): extended;
begin
  Result := php_intval(AExtended, 10);
end;

function SystemOrd(const AVariant: variant): integer;
begin
  if php_is_string(AVariant) then begin
    Result := php_ord(AVariant);
  end else if php_is_int(AVariant) then begin
    Result := AVariant;
  end else begin
    raise EArgumentException.Create('Invalid Ordinal Type');
  end;
end;

function Pred(const AVariant: variant): variant;
var
  LString: string;
begin
  if php_is_string(AVariant) then begin
    Result := php_chr(php_ord(AVariant[0]) - 1);
  end else begin
    Result := AVariant - 1;
  end;
end;

function Succ(const AVariant: variant): variant;
begin
  if php_is_string(AVariant) then begin
    Result := php_chr(php_ord(AVariant[0]) + 1);
  end else begin
    Result := AVariant + 1;
  end;
end;

procedure Inc(var AInteger: integer; const AOffset: integer);
begin
  AInteger := AInteger + AOffset;
end;

procedure Dec(var AInteger: integer; const AOffset: integer);
begin
  AInteger := AInteger - AOffset;
end;

function AllowDirectorySeparators: variant;
begin
  Result := VarArrayOf(['\', '/']);
end;

function MidStr(const AString: string; const AOffset, ALength: integer): string;
begin
  Result := php_substr(AString, AOffset - 1, ALength);
end;

function Length(const AVariant: variant): integer;
begin
  if php_is_string(AVariant) then begin
    Result := php_strlen(AVariant);
  end else if php_is_array(AVariant) then begin
    Result := php_count(AVariant);
  end else begin
    Result := 0;
  end;
end;

procedure SetLength(var AVariant: variant; const AD1, AD2, AD3: integer);
begin
  if php_is_string(AVariant) then begin
    if AD1 < php_strlen(AVariant) then begin
      AVariant := php_substr(AVariant, 0, AD1);
    end else begin
      AVariant := php_str_pad(AVariant, AD1);
    end;
  end else if php_is_array(AVariant) then begin
    VarArraySetLength(AVariant, AD1, AD2, AD3);
  end;
end;

function Low(const AVariant: variant): integer;
begin
  if php_is_array(AVariant) then begin
    Result := 0;
  end else if php_is_string(AVariant) then begin
    Result := 1;
  end;
end;

function High(const AVariant: variant): integer;
begin
  Result := Length(AVariant);
  if php_is_array(AVariant) then begin
    Result := Result - 1;
  end;
end;

function upcase(const AString: string): string;
begin
  Result := php_strtoupper(AString);
end;

function lowercase(const AString: string): string;
begin
  Result := php_strtolower(AString);
end;

procedure Write;
var
  LIndex: integer;
  AArgs: variant;
begin
  AArgs := php_func_get_args();
  for LIndex := 0 to php_count(AArgs) - 1 do begin
    php_echo(AArgs[LIndex]);
  end;
end;

procedure WriteLn;
var
  LIndex: integer;
  LArgs: variant;
begin
  LArgs := php_func_get_args();
  for LIndex := 0 to php_count(LArgs) - 1 do begin
    php_echo(LArgs[LIndex]);
  end;
  php_echo(LineEnding);
end;

initialization

end.
