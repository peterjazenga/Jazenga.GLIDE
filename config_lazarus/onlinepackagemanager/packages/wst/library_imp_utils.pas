{
    This file is part of the Web Service Toolkit
    Copyright (c) 2006 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$INCLUDE wst_global.inc}
unit library_imp_utils;

interface

uses
  Classes, SysUtils
{$IFDEF FPC}
  , DynLibs
{$ELSE}
  , Windows
{$ENDIF}
;

{$INCLUDE wst.inc}
{$INCLUDE wst_delphi.inc}

{$IFNDEF FPC}
const
  NilHandle = 0;
{$ENDIF}

type

{$IFNDEF FPC}
  TLibHandle = HMODULE;//Longint;
{$ENDIF}

  IwstModule = interface
    ['{A62A9A71-727E-47AD-9B84-0F7CA0AE51D5}']
    function GetFileName():string;
    function GetProc(const AProcName : string):Pointer;
  end;
  
  IwstModuleManager = interface
    ['{0A49D315-FF3E-40CD-BCA0-F958BCD5C57F}']
    function Get(const AFileName : string):IwstModule;
    procedure Clear();
    function GetCount() : PtrInt;
    function GetItem(const AIndex : PtrInt) : IwstModule;
  end;

  { TwstModule }

  TwstModule = class(TInterfacedObject,IwstModule)
  private
    FFileName : string;
    FHandle : TLibHandle;
  protected
    function GetFileName():string;
    function GetProc(const AProcName : string):Pointer;
    procedure Load(const ADoLoad : Boolean);virtual;
  public
    constructor Create(const AFileName : string);virtual;
    destructor Destroy();override;
  end;
  TwstModuleClass = class of TwstModule;

  { TwstModuleManager }

  TwstModuleManager = class(TInterfacedObject,IwstModuleManager)
  private
    FList : IInterfaceList;
    FItemClass : TwstModuleClass;
  private
    function Load(const AFileName : string):IwstModule;
    function IndexOf(const AFileName : string):Integer;
  protected
    function Get(const AFileName : string):IwstModule;
    procedure Clear();
    function GetCount() : PtrInt;
    function GetItem(const AIndex : PtrInt) : IwstModule;
  public
    constructor Create(AItemClass : TwstModuleClass);
    destructor Destroy();override;
  end;
  
var
  LibraryManager : IwstModuleManager = nil;

implementation


procedure TwstModule.Load(const ADoLoad : Boolean);
begin
  if ADoLoad then begin
    if ( FHandle = NilHandle ) then begin
      if not FileExists(FFileName) then
        raise Exception.CreateFmt('File not found : "%s".',[FFileName]);
      {$IFDEF FPC}
      FHandle := LoadLibrary(FFileName);
      {$ELSE}
      FHandle := LoadLibrary(PCHAR(FFileName));
      {$ENDIF}
      if ( FHandle = NilHandle ) then
        raise Exception.CreateFmt('Error while loading : "%s".',[FFileName]);
    end;
  end else begin
    if ( FHandle <> NilHandle ) then begin
      FreeLibrary(FHandle);
      FHandle := NilHandle;
    end;
  end;
end;

function TwstModule.GetFileName(): string;
begin
  Result := FFileName;
end;

function TwstModule.GetProc(const AProcName: string): Pointer;
begin
  {$IFDEF FPC}
  Result := GetProcAddress(FHandle,AProcName);
  {$ELSE}
  Result := GetProcAddress(FHandle,PCHAR(AProcName));
  {$ENDIF}
  if not Assigned(Result) then
    raise Exception.CreateFmt('Procedure "%s" not found in this module( "%s" ).',[AProcName,FFileName]);
end;

constructor TwstModule.Create(const AFileName: string);
begin
  FHandle := NilHandle;
  FFileName := AFileName;
  Load(True);
end;

destructor TwstModule.Destroy();
begin
  Load(False);
  inherited Destroy();
end;

{ TwstModuleManager }

function TwstModuleManager.Get(const AFileName: string): IwstModule;
var
  i : Integer;
begin
  i := IndexOf(AFileName);
  if ( i < 0 ) then begin
    FList.Lock();
    try
      i := IndexOf(AFileName);
      if ( i < 0 ) then begin
        Result := Load(AFileName);
        FList.Add(Result);
      end else begin
        Result := GetItem(i);;
      end;
    finally
      FList.Unlock();
    end;
  end else begin
    Result := GetItem(i);
  end;
end;

procedure TwstModuleManager.Clear();
begin
  FList.Clear();
end;

function TwstModuleManager.GetCount(): PtrInt;
begin
  Result := FList.Count;
end;

function TwstModuleManager.GetItem(const AIndex: PtrInt): IwstModule;
begin
  Result := FList[AIndex] as IwstModule;
end;

function TwstModuleManager.Load(const AFileName: string): IwstModule;
begin
  Result := FItemClass.Create(AFileName);
end;

function TwstModuleManager.IndexOf(const AFileName: string): Integer;
begin
  for Result := 0 to Pred(FList.Count) do begin
    if AnsiSameStr(AFileName,(FList[Result] as IwstModule).GetFileName()) then
      Exit;
  end;
  Result := -1;
end;

constructor TwstModuleManager.Create(AItemClass : TwstModuleClass);
begin
  Assert(Assigned(AItemClass));
  inherited Create();
  FItemClass := AItemClass;
  FList := TInterfaceList.Create();
end;

destructor TwstModuleManager.Destroy();
begin
  FList := nil;
  inherited Destroy();
end;

procedure InitLibraryManager();
begin
  LibraryManager := TwstModuleManager.Create(TwstModule);
end;

initialization
  InitLibraryManager();
  
finalization
  LibraryManager := nil;
  
end.

