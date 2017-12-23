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

unit P2PClasses;

interface

uses
  Classes, HttpDefs, P2PContnrs, P2PSysUtils, StrUtils, SysUtils;

type

  CStringList = class
  strict private
    FItems: array of string;
    FNameValueSeparator: char;
    FDelimiter: char;
    FCaseSensitive: boolean;
  public
    constructor Create;
  protected
    function GetCount: integer;
    procedure SetCount(const ACount: integer);
  public
    procedure Clear;
    function Get(const AIndex: integer): string;
    function GetName(const AIndex: integer): string;
    function GetValueFromIndex(const AIndex: integer): string;
    //procedure SetValueFromIndex(const AIndex: integer; const AValue: string);
    function GetText: string;
    function GetValue(const AName: string): string;
    function IndexOf(const AString: string): integer;
    function IndexOfName(const AName: string): integer;
    function Add(const AString: string): integer;
    procedure Delete(AIndex: integer);
    procedure Remove(const AItem: string);
    procedure RemoveName(const AName: string);
    procedure Put(const AIndex: integer; const AString: string);
    procedure SetValue(const AName, AValue: string);
  public
    property Count: integer read GetCount write SetCount;
    property CaseSensitive: boolean read FCaseSensitive write FCaseSensitive;
    property Delimiter: char read FDelimiter write FDelimiter;
    property NameValueSeparator: char read FNameValueSeparator write FNameValueSeparator;
    //property Strings[const AIndex: integer]: string read Get write Put;
    //property ValueFromIndex[const AIndex: integer]: string read GetValueFromIndex;
    //property Values[const AName: string]: string read GetValue write SetValue;
  end;

  CStringListEx = class(CStringList)
    procedure SetValues(const AValues: array of string);
    procedure AddHttp(const AHttp: string);
  end;

  CComponent = class
  strict private
    FOwner: CComponent;
    FObjects: CObjectList;
    FName, FValue: string;
  protected
    function GetName: string; virtual;
    function GetOwner: CComponent; virtual;
    function GetValue: string; virtual;
    procedure SetName(const AName: string); virtual;
    procedure SetOwner(const AOwner: CComponent); virtual;
    procedure SetValue(const AValue: string); virtual;
  public
    constructor Create(const AOwner: CComponent; const AValue: string = ''; const AName: string = '');
    destructor Destroy; override;
  public
    function GetComponents(const AIndex: integer): CComponent;
    function ComponentCount: integer;
  public
    procedure DoCreate; virtual;
    function Spawn(const AValue: string = ''; const AName: string = ''): CComponent; virtual;
  published
    property Name: string read GetName write SetName;
    property Value: string read GetValue write SetValue;
    property Owner: CComponent read GetOwner write SetOwner;
  end;

implementation

constructor CStringList.Create;
begin
  inherited Create;
  FNameValueSeparator := '=';
  FDelimiter := #10;
  FCaseSensitive := False;
end;

function CStringList.GetCount: integer;
begin
  Result := Length(FItems);
end;

procedure CStringList.SetCount(const ACount: integer);
begin
  SetLength(FItems, ACount);
end;

procedure CStringList.Clear;
begin
  SetLength(FItems, 0);
end;

function CStringList.Get(const AIndex: integer): string;
begin
  Result := FItems[AIndex];
end;

procedure CStringList.Put(const AIndex: integer; const AString: string);
begin
  FItems[AIndex] := AString;
end;

function CStringList.GetName(const AIndex: integer): string;
var
  LPos: integer;
begin
  Result := FItems[AIndex];
  LPos := Pos(FNameValueSeparator, Result);
  if LPos > 0 then begin
    Result := AnsiLeftStr(Result, LPos - 1);
  end;
end;

function CStringList.GetValueFromIndex(const AIndex: integer): string;
var
  LPos: integer;
begin
  Result := FItems[AIndex];
  LPos := Pos(FNameValueSeparator, Result);
  if LPos > 0 then begin
    Result := Copy(Result, LPos + 1, MaxInt);
  end else begin
    Result := EmptyStr;
  end;
end;

function CStringList.GetValue(const AName: string): string;
var
  LIndex: integer;
begin
  LIndex := IndexOfName(AName);
  if LIndex < 0 then begin
    Result := EmptyStr;
  end else begin
    Result := GetValueFromIndex(LIndex);
  end;
end;

procedure CStringList.SetValue(const AName, AValue: string);
var
  LIndex, LPos: integer;
begin
  for LIndex := 0 to High(FItems) do begin
    LPos := Pos(FNameValueSeparator, FItems[LIndex]);
    if LPos > 0 then begin
      if SameText(AName, AnsiLeftStr(FItems[LIndex], LPos - 1)) then begin
        FItems[LIndex] := AName + FNameValueSeparator + AValue;
        Exit;
      end;
    end;
  end;
  Add(AName + FNameValueSeparator + AValue);
end;

function CStringList.IndexOf(const AString: string): integer;
begin
  for Result := 0 to High(FItems) do begin
    if AnsiSame(AString, FItems[Result], FCaseSensitive) then begin
      Exit;
    end;
  end;
  Result := -1;
end;

function CStringList.IndexOfName(const AName: string): integer;
begin
  for Result := 0 to High(FItems) do begin
    if AnsiSame(AName, GetName(Result), FCaseSensitive) then begin
      Exit;
    end;
  end;
  Result := -1;
end;

function CStringList.Add(const AString: string): integer;
begin
  SetLength(FItems, Length(FItems) + 1);
  Result := High(FItems);
  FItems[Result] := AString;
end;

procedure CStringList.Delete(AIndex: integer);
begin
  Assert((AIndex >= 0) and (AIndex < Length(FItems)));
  while AIndex < High(FItems) do begin
    FItems[AIndex] := FItems[AIndex + 1];
    AIndex := AIndex + 1;
  end;
  SetLength(FItems, Length(FItems) - 1);
end;

procedure CStringList.Remove(const AItem: string);
begin
  Delete(IndexOf(AItem));
end;

procedure CStringList.RemoveName(const AName: string);
begin
  Delete(IndexOfName(AName));
end;

function CStringList.GetText: string;
var
  LIndex: integer;
begin
  Result := EmptyStr;
  for LIndex := 0 to High(FItems) do begin
    Result := Result + FItems[LIndex] + LineEnding;
  end;
end;

procedure CStringListEx.SetValues(const AValues: array of string);
var
  LIndex: integer;
begin
  LIndex := 0;
  while LIndex < High(AValues) do begin
    SetValue(AValues[LIndex], AValues[LIndex + 1]);
    LIndex := LIndex + 2;
  end;
end;

procedure CStringListEx.AddHttp(const AHttp: string);
var
  LPos, LEqu, LEnd: integer;
  LLine: string;
begin
  LPos := 1;
  repeat
    LEnd := PosEx('&', AHttp, LPos);
    if LEnd = 0 then begin
      LEnd := Length(AHttp) + 1;
    end;
    LLine := Copy(AHttp, LPos, LEnd - LPos);
    LEqu := Pos('=', LLine);
    if LEqu > 0 then begin
      Add(HttpDecode(AnsiLeftStr(LLine, LEqu - 1)) + '=' + HttpDecode(Copy(LLine, LEqu + 1, MaxInt)));
    end;
    LPos := LEnd + 1;
  until LPos > Length(AHttp);
end;

constructor CComponent.Create(const AOwner: CComponent; const AValue, AName: string);
begin
  inherited Create;
  FObjects := CObjectList.Create;
  SetName(AName);
  SetValue(AValue);
  SetOwner(AOwner);
  DoCreate;
end;

destructor CComponent.Destroy;
begin
  FreeAndNil(FObjects);
  inherited Destroy;
end;

procedure CComponent.DoCreate;
begin

end;

function CComponent.Spawn(const AValue, AName: string): CComponent;
begin
  Result := CComponent.Create(Self, AValue, AName);
end;

function CComponent.GetName: string;
begin
  Result := FName;
end;

procedure CComponent.SetName(const AName: string);
begin
  FName := AName;
end;

function CComponent.GetValue: string;
begin
  Result := FValue;
end;

procedure CComponent.SetValue(const AValue: string);
begin
  FValue := AValue;
end;

function CComponent.GetOwner: CComponent;
begin
  Result := FOwner;
end;

procedure CComponent.SetOwner(const AOwner: CComponent);
begin
  if Assigned(FOwner) then begin
    FOwner.FObjects.Remove(Self);
  end;
  FOwner := AOwner;
  if Assigned(FOwner) then begin
    FOwner.FObjects.Add(Self);
  end;
end;

function CComponent.ComponentCount: integer;
begin
  Result := FObjects.Count;
end;

function CComponent.GetComponents(const AIndex: integer): CComponent;
begin
  Result := FObjects.GetItem(AIndex) as CComponent;
end;

end.
