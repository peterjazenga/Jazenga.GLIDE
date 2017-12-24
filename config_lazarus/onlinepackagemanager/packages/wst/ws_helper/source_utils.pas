{
    This file is part of the Web Service Toolkit
    Copyright (c) 2006-2014 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$INCLUDE wst_global.inc}
unit source_utils;

interface

uses
  Classes, SysUtils {$IFDEF WST_IDE}, LazFileUtils {$ENDIF};
  
Type

  EsourceException = class(Exception)
  end;

  ISourceStream = interface;
  ISourceManager = interface;
  ISavableSourceStream = interface;
  
  ISourceStream = interface
    ['{91EA7DA6-340C-477A-A6FD-06F2BAEA9A97}']
    function GetFileName():string;
    procedure SaveToFile(const APath : string);
    procedure Indent();
    function IncIndent():Integer;
    function DecIndent():Integer;
    procedure Write(AText : String);overload;
    procedure Write(AText : String; Const AArgs : array of const);overload;
    procedure WriteLn(AText : String);overload;
    procedure WriteLn(AText : String; Const AArgs : array of const);overload;
    procedure NewLine();
    procedure BeginAutoIndent();
    procedure EndAutoIndent();
    procedure Append(ASource : ISavableSourceStream);
  end;

  ISourceManager = Interface
    ['{91348FC9-C39E-45D4-A692-C8A363695D78}']
    function CreateItem(const AFileName : string):ISourceStream;
    function Find(const AFileName : string):ISourceStream;
    function Merge(
      const AFinalFileName : string;
      const ASourceList : array of ISourceStream
    ) : ISourceStream;
    procedure SaveToFile(const APath : string);
    function GetCount():Integer;
    function GetItem(const AIndex :Integer):ISourceStream;
  end;

  ISavableSourceStream = Interface(ISourceStream)
    ['{B5F03006-FD33-4DA8-A2E7-168BDABE8832}']
    procedure SaveToStream(AStream : TStream);
    function GetStream(): TStream;
  end;
  
  function CreateSourceManager():ISourceManager;
  
implementation
uses StrUtils, parserutils;

type

  { TSourceStream }

  TSourceStream = class(TInterfacedObject,ISourceStream,ISavableSourceStream)
  Private
    FStream : TMemoryStream;
    FIndentCount : Integer;
    FAutoIndentCount : Integer;
    FFileName : string;
  Protected
    function GetFileName():string;
    procedure SaveToStream(AStream : TStream);
    function GetStream(): TStream;
    procedure SaveToFile(const APath : string);
    procedure Indent();
    function IncIndent():Integer;
    function DecIndent():Integer;
    procedure Write(AText : String);overload;
    procedure Write(AText : String; Const AArgs : array of const);overload;
    procedure WriteLn(AText : String);overload;
    procedure WriteLn(AText : String; Const AArgs : array of const);overload;
    procedure NewLine();
    procedure BeginAutoIndent();
    procedure EndAutoIndent();
    function IsInAutoInden():Boolean;
    procedure Append(ASource : ISavableSourceStream);
  Public
    constructor Create(const AFileName:string);
    destructor Destroy();override;
  End;

  { TSourceManager }

  TSourceManager = class(TInterfacedObject,ISourceManager)
  Private
    FList : IInterfaceList;
  Private
    procedure Error(AText : String);overload;
    procedure Error(AText : String; Const AArgs : array of const);overload;
  Protected
    function CreateItem(const AFileName : string):ISourceStream;
    function Find(const AFileName : string):ISourceStream;
    procedure SaveToFile(const APath : string);
    function Merge(
      const AFinalFileName : string;
      const ASourceList : array of ISourceStream
    ) : ISourceStream;
    function GetCount():Integer;
    function GetItem(const AIndex:Integer):ISourceStream;
  Public
    constructor Create();
    destructor Destroy();override;
  End;
  
function CreateSourceManager():ISourceManager;
begin
  Result := TSourceManager.Create() as ISourceManager;
end;

{ TSourceManager }

procedure TSourceManager.Error(AText: String);
begin
  Raise EsourceException.Create(AText);
end;

procedure TSourceManager.Error(AText: String; const AArgs: array of const);
begin
  Raise EsourceException.CreateFmt(AText,AArgs);
end;

function TSourceManager.GetCount(): Integer;
begin
  Result := FList.Count;
end;

function TSourceManager.GetItem(const AIndex: Integer): ISourceStream;
begin
  Result := FList[AIndex] as ISourceStream;
end;

function TSourceManager.CreateItem(const AFileName: string): ISourceStream;
begin
  If Assigned(Find(AFileName)) Then
    Error('A file named "%s" allready exists.',[AFileName]);
  Result := TSourceStream.Create(AFileName) as ISourceStream;
  FList.Add(Result);
end;

function TSourceManager.Find(const AFileName: string): ISourceStream;
Var
  i : Integer;
  s : string;
begin
  s := LowerCase(AFileName);
  For i := 0 To Pred(GetCount()) Do Begin
    Result := GetItem(i);
    If AnsiSameText(s,Result.GetFileName()) Then
      Exit;
  End;
  Result := Nil;
end;

procedure TSourceManager.SaveToFile(const APath: string);
Var
  i : Integer;
begin
  For i := 0 To Pred(GetCount()) Do Begin
    (GetItem(i) As ISavableSourceStream).SaveToFile(APath);
  End;
end;

function TSourceManager.Merge(const AFinalFileName: string;const ASourceList: array of ISourceStream): ISourceStream;
Var
  i : Integer;
  s : TStream;
begin
  Result := CreateItem(AFinalFileName);
  s := (Result as ISavableSourceStream).GetStream();
  For i := Low(ASourceList) To High(ASourceList) Do Begin
    (ASourceList[i] as ISavableSourceStream).SaveToStream(s);
    FList.Remove(ASourceList[i]);
  End;
end;

constructor TSourceManager.Create();
begin
  FList := TInterfaceList.Create() as IInterfaceList;
end;

destructor TSourceManager.Destroy();
begin
  if ( FList <> nil ) then
    FList.Clear();
  FList := nil;
  inherited Destroy();
end;

{ TSourceStream }

function TSourceStream.GetFileName(): string;
begin
  Result := FFileName;
end;

procedure TSourceStream.SaveToStream(AStream: TStream);
begin
  AStream.CopyFrom(FStream,0);
end;

function TSourceStream.GetStream(): TStream;
begin
  Result := FStream;
end;

procedure TSourceStream.SaveToFile(const APath: string);
var
  locDir,locFileName: string;
begin
  locFileName := IncludeTrailingPathDelimiter(APath) + GetFileName();
  locDir := ExtractFileDir(locFileName);
  if not DirectoryExists(locDir) then
    ForceDirectories(locDir);
  FStream.SaveToFile(locFileName);
  {$IFDEF WST_IDE}
  LazFileUtils.InvalidateFileStateCache(locFileName);
  {$ENDIF}
end;

procedure TSourceStream.Indent();
Const INDENT_STR = '  ';
Var
  s : string;
begin
  If ( FIndentCount > 0 ) Then Begin
    s := DupeString(INDENT_STR,FIndentCount);
    FStream.Write(s[1],Length(s));
  End;
end;

function TSourceStream.IncIndent():Integer;
begin
  Inc(FIndentCount);
  Result := FIndentCount;
end;

function TSourceStream.DecIndent():Integer;
begin
  Assert(FIndentCount>0);
  Dec(FIndentCount);
  Result := FIndentCount;
end;

procedure TSourceStream.Write(AText: String);
Var
  i : Integer;
begin
  If IsInAutoInden() Then
    Indent();
  i := Length(AText);
  If ( i > 0 ) Then
    FStream.Write(AText[1],i);
end;

procedure TSourceStream.Write(AText: String; const AArgs: array of const);
begin
  Write(Format(AText,AArgs));
end;

procedure TSourceStream.WriteLn(AText: String);
begin
  Write(AText+sNEW_LINE);
end;

procedure TSourceStream.WriteLn(AText: String; const AArgs: array of const);
begin
  Write(AText+sNEW_LINE,AArgs);
end;

procedure TSourceStream.NewLine();
begin
  WriteLn('');
end;

procedure TSourceStream.BeginAutoIndent();
begin
  Inc(FAutoIndentCount);
end;

procedure TSourceStream.EndAutoIndent();
begin
  Assert(FAutoIndentCount>0);
  Dec(FAutoIndentCount);
end;

function TSourceStream.IsInAutoInden(): Boolean;
begin
  Result := ( FAutoIndentCount > 0 );
end;

procedure TSourceStream.Append(ASource : ISavableSourceStream);
begin
  if ( ASource <> nil ) then
    FStream.CopyFrom(ASource.GetStream(),0);
end;

constructor TSourceStream.Create(const AFileName: string);
begin
  FFileName := AFileName;
  FStream := TMemoryStream.Create();
  FIndentCount := 0;
  FAutoIndentCount := 0;
end;

destructor TSourceStream.Destroy();
begin
  FreeAndNil(FStream);
  inherited Destroy();
end;

end.
