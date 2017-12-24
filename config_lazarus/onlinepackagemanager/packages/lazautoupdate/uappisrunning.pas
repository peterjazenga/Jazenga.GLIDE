unit uappisrunning;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,LazUTF8,LazFileUtils,FileUtil
  {$IFDEF WINDOWS}, Windows, JwaTlHelp32{$ENDIF}
  {$IFDEF LINUX},process{$ENDIF};
// JwaTlHelp32 is in fpc\packages\winunits-jedi\src\jwatlhelp32.pas

// Returns TRUE if EXEName is running under Windows or Linux
// Don't pass an .exe extension to Linux!
function AppIsRunning(const ExeName: string):Boolean;
procedure KillApp(const ExeName: string);
Function GetUserName:String;

implementation
// These functions return Zero if app is NOT running
// Override them if you have a better implementation
{$IFDEF WINDOWS}

function WindowsGetUserName: string;
var
  nsize: DWORD;
  sz: ansistring;
begin
  Result := 'unknown';
  nsize := 255;
  SetLength(sz, nsize);
  windows.GetUsername(PChar(sz), nsize);
  SetLength(sz, nsize);
  Result := Trim(sz);
end;

function WindowsAppIsRunning(const ExeName: string): integer;
var
  ContinueLoop: BOOL;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
begin
  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  FProcessEntry32.dwSize := SizeOf(FProcessEntry32);
  ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);
  Result := 0;
  while integer(ContinueLoop) <> 0 do
    begin
    if ((UpperCase(ExtractFileName(FProcessEntry32.szExeFile)) =
      UpperCase(ExeName)) or (UpperCase(FProcessEntry32.szExeFile) =
      UpperCase(ExeName))) then
      begin
      Inc(Result);
      end;
    ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
    end;
  CloseHandle(FSnapshotHandle);
end;
Procedure KillWindowsApp(const ExeName:String);
var
  ContinueLoop: BOOL;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
  AHandle: THandle;
  ID: dword;
begin
  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  FProcessEntry32.dwSize := SizeOf(FProcessEntry32);
  ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);
  while integer(ContinueLoop) <> 0 do
    begin
    if ((UpperCase(ExtractFileName(FProcessEntry32.szExeFile)) =
      UpperCase(ExeName)) or (UpperCase(FProcessEntry32.szExeFile) =
      UpperCase(ExeName))) then
      begin
             ID:=FProcessEntry32.th32ProcessID;
             AHandle := OpenProcess(PROCESS_ALL_ACCESS,False,ID); //uses windows
             TerminateProcess(AHandle,255);
      end;
    ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
    end;
  CloseHandle(FSnapshotHandle);
end;
{$ENDIF}
{$IFDEF LINUX}
function LinuxGetUserName: string;
begin
  Result:=GetEnvironmentVariableUTF8('USER');
end;
function LinuxAppIsRunning(const ExeName: string): integer;
var
  t: TProcess;
  s: TStringList;
begin
  Result := 0;
  t := tprocess.Create(nil);
  t.CommandLine := 'ps -C ' + ExeName;
  t.Options := [poUsePipes, poWaitonexit];
    try
    t.Execute;
    s := TStringList.Create;
      try
      s.LoadFromStream(t.Output);
      Result := Pos(ExeName, s.Text);
      finally
      s.Free;
      end;
    finally
    t.Free;
    end;
end;
procedure KillLinuxApp(const ExeName: string);
// killall -9 processname
// or pidof EXEName gives PID then kill PID
var
  t: TProcess;
  s: TStringList;
begin
  t := tprocess.Create(nil);
  t.CommandLine := 'killall -9 ' + ExeName;
  t.Options := [poUsePipes, poWaitonexit];
    try
    t.Execute;
    {
    s := TStringList.Create;
      try
      s.LoadFromStream(t.Output);
      Result := Pos(ExeName, s.Text);
      finally
      s.Free;
      end;
    }
    finally
    t.Free;
    end;
end;
{$ENDIF}
Function GetUserName:String;
begin
{$IFDEF WINDOWS}
        Result:=WindowsGetUserName;
{$ENDIF}
{$IFDEF LINUX}
Result:=LinuxGetUserName;
{$ENDIF}
end;

procedure KillApp(const ExeName: string);
begin
{$IFDEF WINDOWS}
  KillWindowsApp(ExeName);
{$ENDIF}
{$IFDEF LINUX}
  KillLinuxApp(ExeName);
{$ENDIF}
end;

function AppIsRunning(const ExeName: string):Boolean;
begin
{$IFDEF WINDOWS}
  Result:=(WindowsAppIsRunning(ExeName) > 0);
{$ENDIF}
{$IFDEF LINUX}
  Result:=(LinuxAppIsRunning(ExeName) > 0);
{$ENDIF}
end;

end.

