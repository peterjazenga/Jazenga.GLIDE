{ rxConfigValues unit

  Copyright (C) 2005-2017 Lagunov Aleksey alexs75@yandex.ru and Lazarus team
  original conception from rx library for Delphi (c)

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit rxConfigValues;

{$I rx.inc}

interface

uses
  Classes, SysUtils; 

const
  cvtInteger  = 1; // целое
  cvtString   = 2; // строка
  cvtBoolean  = 3; // логическая
  cvtDateTime = 4; // дата
  cvtFloat    = 5; // вещественное

type
  TConfigValuesEnumerator = class;

  { TConfigValue }

  TConfigValue = class
  private
    FModified:boolean;
    FName: string;
    FDataType:byte;
    FValue:Variant;
    function GetAsBoolean: boolean;
    function GetAsDateTime: TDateTime;
    function GetAsFloat: Double;
    function GetAsInteger: integer;
    function GetAsString: string;
    procedure SetAsBoolean(const AValue: boolean);
    procedure SetAsDateTime(const AValue: TDateTime);
    procedure SetAsFloat(const AValue: Double);
    procedure SetAsInteger(const AValue: integer);
    procedure SetAsString(const AValue: string);
    function GetValue: string;
  public
    constructor Create;
    destructor Destroy; override;
    property Name:string read FName;
    property AsString:string read GetAsString write SetAsString;
    property AsInteger:integer read GetAsInteger write SetAsInteger;
    property AsFloat:Double read GetAsFloat write SetAsFloat;
    property AsBoolean:boolean read GetAsBoolean write SetAsBoolean;
    property AsDateTime:TDateTime read GetAsDateTime write SetAsDateTime;
    property Modified:boolean read FModified write FModified;
    property DataType:byte read FDataType;
    property Value:string read GetValue;
  end;
  
  { TConfigValues }

  TConfigValues = class
  private
    FItems:TList;
    function CreateValue(AName:string; AType:byte):TConfigValue;
    function GetCount: integer;
    function GetItem(Index:Integer): TConfigValue;
  public
    constructor Create;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Clear;
    function ParamByName(AName:string):TConfigValue;
    function ByNameAsInteger(AName:string; DefValue:integer):integer;
    function ByNameAsString(AName:string; DefValue:string):string;
    function ByNameAsFloat(AName:string; DefValue:Double):Double;
    function ByNameAsBoolean(AName:string; DefValue:Boolean):Boolean;
    function ByNameAsDateTime(AName:string; DefValue:TDateTime):TDateTime;
    procedure SetByNameAsInteger(AName:string; AValue:integer);
    procedure SetByNameAsString(AName:string; AValue:string);
    procedure SetByNameAsFloat(AName:string; ADefValue:Double);
    procedure SetByNameAsBoolean(AName:string; ADefValue:Boolean);
    procedure SetByNameAsDateTime(AName:string; ADefValue:TDateTime);
    function GetEnumerator: TConfigValuesEnumerator;
  public
    property Items[Index:Integer]:TConfigValue read GetItem;default;
    property Count:integer read GetCount;
  end;
  
  { TConfigValuesEnumerator }

  TConfigValuesEnumerator = class
  private
    FList: TConfigValues;
    FPosition: Integer;
  public
    constructor Create(AList: TConfigValues);
    function GetCurrent: TConfigValue;
    function MoveNext: Boolean;
    property Current: TConfigValue read GetCurrent;
  end;

implementation
uses rxconst;

{ TConfigValuesEnumerator }

constructor TConfigValuesEnumerator.Create(AList: TConfigValues);
begin
  FList := AList;
  FPosition := -1;
end;

function TConfigValuesEnumerator.GetCurrent: TConfigValue;
begin
  Result := FList[FPosition];
end;

function TConfigValuesEnumerator.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FList.Count;
end;

{ TConfigValues }

function TConfigValues.CreateValue(AName: string; AType: byte): TConfigValue;
begin
  Result:=TConfigValue.Create;
  Result.FDataType:=AType;
  Result.FName:=AName;
  FItems.Add(Result);
end;

function TConfigValues.GetCount: integer;
begin
  Result:=FItems.Count;
end;

function TConfigValues.GetItem(Index:Integer): TConfigValue;
begin
  Result:=TConfigValue(FItems[Index]);
end;

constructor TConfigValues.Create;
begin
  inherited Create;
  FItems:=TList.Create;
end;

destructor TConfigValues.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TConfigValues.BeginUpdate;
begin

end;

procedure TConfigValues.EndUpdate;
var
  i:integer;
begin
  for i:=0 to FItems.Count - 1 do
    TConfigValue(FItems[i]).FModified:=false;
end;

procedure TConfigValues.Clear;
var
  i:integer;
begin
  for i:=0 to FItems.Count - 1 do
    TConfigValue(FItems[i]).Free;
  FItems.Clear;
end;

function TConfigValues.ParamByName(AName: string): TConfigValue;
var
  i:integer;
begin
  AName:=AnsiUpperCase(AName);
  Result:=nil;
  for i:=0 to FItems.Count - 1 do
  begin
    if AnsiUpperCase(TConfigValue(FItems[i]).FName) = AName then
    begin
      Result:=TConfigValue(FItems[i]);
      exit;
    end;
  end;
end;

function TConfigValues.ByNameAsInteger(AName: string; DefValue: integer
  ): integer;
var
  P:TConfigValue;
begin
  P:=ParamByName(AName);
  if Assigned(P) then
    Result:=P.AsInteger
  else
    Result:=DefValue;
end;

function TConfigValues.ByNameAsString(AName: string; DefValue: string): string;
var
  P:TConfigValue;
begin
  P:=ParamByName(AName);
  if Assigned(P) then
    Result:=P.AsString
  else
    Result:=DefValue;
end;

function TConfigValues.ByNameAsFloat(AName: string; DefValue: Double): Double;
var
  P:TConfigValue;
begin
  P:=ParamByName(AName);
  if Assigned(P) then
    Result:=P.AsFloat
  else
    Result:=DefValue;
end;

function TConfigValues.ByNameAsBoolean(AName: string; DefValue: Boolean
  ): Boolean;
var
  P:TConfigValue;
begin
  P:=ParamByName(AName);
  if Assigned(P) then
    Result:=P.AsBoolean
  else
    Result:=DefValue;
end;

function TConfigValues.ByNameAsDateTime(AName: string; DefValue: TDateTime
  ): TDateTime;
var
  P:TConfigValue;
begin
  P:=ParamByName(AName);
  if Assigned(P) then
    Result:=P.AsDateTime
  else
    Result:=DefValue;
end;

procedure TConfigValues.SetByNameAsInteger(AName: string; AValue: integer);
var
  P:TConfigValue;
begin
  P:=ParamByName(AName);
  if not Assigned(P) then
    P:=CreateValue(AName, cvtInteger);
  P.AsInteger:=AValue;
end;

procedure TConfigValues.SetByNameAsString(AName: string; AValue: string);
var
  P:TConfigValue;
begin
  P:=ParamByName(AName);
  if not Assigned(P) then
    P:=CreateValue(AName, cvtString);
  P.AsString:=AValue;
end;

procedure TConfigValues.SetByNameAsFloat(AName: string; ADefValue: Double);
var
  P:TConfigValue;
begin
  P:=ParamByName(AName);
  if not Assigned(P) then
    P:=CreateValue(AName, cvtFloat);
  P.AsFloat:=ADefValue;
end;

procedure TConfigValues.SetByNameAsBoolean(AName: string; ADefValue: Boolean);
var
  P:TConfigValue;
begin
  P:=ParamByName(AName);
  if not Assigned(P) then
    P:=CreateValue(AName, cvtBoolean);
  P.AsBoolean:=ADefValue;
end;

procedure TConfigValues.SetByNameAsDateTime(AName: string; ADefValue: TDateTime
  );
var
  P:TConfigValue;
begin
  P:=ParamByName(AName);
  if not Assigned(P) then
    P:=CreateValue(AName, cvtDateTime);
  P.AsDateTime:=ADefValue;
end;

function TConfigValues.GetEnumerator: TConfigValuesEnumerator;
begin
  Result:=TConfigValuesEnumerator.Create(Self);
end;

{ TConfigValue }

function TConfigValue.GetAsBoolean: boolean;
begin
  if FDataType = cvtBoolean then
    Result:=FValue
  else
    raise Exception.CreateFmt(sVariableIsNotBoolean, [FName]);
end;

function TConfigValue.GetAsDateTime: TDateTime;
begin
  if FDataType = cvtDateTime then
    Result:=FValue
  else
    raise Exception.CreateFmt(sVariableIsNotDT, [FName]);
end;

function TConfigValue.GetAsFloat: Double;
begin
  if FDataType = cvtFloat then
    Result:=FValue
  else
    raise Exception.CreateFmt(sVariableIsNotFloat, [FName]);
end;

function TConfigValue.GetAsInteger: integer;
begin
  if FDataType = cvtInteger then
    Result:=FValue
  else
    raise Exception.CreateFmt(sVariableIsNotInteger, [FName]);
end;

function TConfigValue.GetAsString: string;
begin
  if FDataType = cvtString then
    Result:=FValue
  else
    raise Exception.CreateFmt(sVariableIsNotString, [FName]);
end;

procedure TConfigValue.SetAsBoolean(const AValue: boolean);
begin
  if FDataType = cvtBoolean then
  begin
    if FValue<>AValue then
    begin
      FValue:=AValue;
      FModified:=true;
    end
  end
  else
    raise Exception.CreateFmt(sVariableIsNotBoolean, [FName]);
end;

procedure TConfigValue.SetAsDateTime(const AValue: TDateTime);
begin
  if FDataType = cvtDateTime then
  begin
    if FValue<>AValue then
    begin
      FValue:=AValue;
      FModified:=true;
    end
  end
  else
    raise Exception.CreateFmt(sVariableIsNotDT, [FName]);
end;

procedure TConfigValue.SetAsFloat(const AValue: Double);
begin
  if FDataType = cvtFloat then
  begin
    if FValue<>AValue then
    begin
      FValue:=AValue;
      FModified:=true;
    end
  end
  else
    raise Exception.CreateFmt(sVariableIsNotFloat, [FName]);
end;

procedure TConfigValue.SetAsInteger(const AValue: integer);
begin
  if FDataType = cvtInteger then
  begin
    if (FValue = null) or (FValue<>AValue) then
    begin
      FValue:=AValue;
      FModified:=true;
    end
  end
  else
    raise Exception.CreateFmt(sVariableIsNotInteger, [FName]);
end;

procedure TConfigValue.SetAsString(const AValue: string);
begin
  if FDataType = cvtString then
  begin
    if FValue<>AValue then
    begin
      FValue:=AValue;
      FModified:=true;
    end
  end
  else
    raise Exception.CreateFmt(sVariableIsNotString, [FName]);
end;

constructor TConfigValue.Create;
begin
  inherited Create;
  FModified:=false;
  FValue:=null;
end;

destructor TConfigValue.Destroy;
begin
  inherited Destroy;
end;

function TConfigValue.GetValue: string;
begin
  case FDataType of
    cvtInteger : Result:=IntToStr(AsInteger);
    cvtString  : Result:=AsString;
    cvtBoolean : Result:=IntToStr(Ord(AsBoolean));
    cvtDateTime: Result:=DateTimeToStr(AsDateTime);
    cvtFloat   : Str(AsFloat, Result);
  end;
end;

end.

