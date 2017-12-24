{ rxapputils unit

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

unit rxapputils;

{$I rx.inc}

interface

uses
  Classes, SysUtils, Controls, IniFiles;

const
  {$IFNDEF LINUX}
  AllMask = '*.*';
  {$ELSE}
  AllMask = '*';
  {$ENDIF}

var
  DefCompanyName: string = '';
  RegUseAppTitle: Boolean = False;


function GetDefaultSection(Component: TComponent): string;
procedure GetDefaultIniData(Control: TControl; var IniFileName,
  Section: string; UseRegistry: Boolean = false);
function GetDefaultIniName: string;

type
  TOnGetDefaultIniName = function: string;

const
  OnGetDefaultIniName: TOnGetDefaultIniName = nil;

//Save to IniFile or TRegIniFile string value
procedure IniWriteString(IniFile: TObject; const Section, Ident,
  Value: string);
function IniReadString(IniFile: TObject; const Section, Ident,
  Value: string):string;

//Save to IniFile or TRegIniFile integer value
procedure IniWriteInteger(IniFile: TObject; const Section, Ident:string;
  const Value: integer);
function IniReadInteger(IniFile: TObject; const Section, Ident:string;
  const Value: integer):integer;

function GetDefaultIniRegKey: string;
Function RxGetAppConfigDir(Global : Boolean) : String;
implementation
uses
  {$IFDEF WINDOWS}
  windirs,
  {$ENDIF}
  Registry, Forms, FileUtil, LazUTF8, LazFileUtils;

{$IFDEF WINDOWS}
function RxGetAppConfigDir(Global: Boolean): String;
begin
  If Global then
    Result:=GetWindowsSpecialDir(CSIDL_COMMON_APPDATA)
  else
    Result:=GetWindowsSpecialDir(CSIDL_LOCAL_APPDATA);
  If (Result<>'') then
    begin
      if VendorName<>'' then
        Result:=IncludeTrailingPathDelimiter(Result+ UTF8ToSys(VendorName));
      Result:=IncludeTrailingPathDelimiter(Result+UTF8ToSys(ApplicationName));
    end
  else
    Result:=ExcludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))); //IncludeTrailingPathDelimiter(DGetAppConfigDir(Global));
end;
{$ELSE}
function RxGetAppConfigDir(Global: Boolean): String;
begin
  Result:=GetAppConfigDir(Global);
end;
{$ENDIF}

function GetDefaultSection(Component: TComponent): string;
var
  F: TCustomForm;
  Owner: TComponent;
begin
  if Component <> nil then begin
    if Component is TCustomForm then Result := Component.ClassName
    else begin
      Result := Component.Name;
      if Component is TControl then begin
        F := GetParentForm(TControl(Component));
        if F <> nil then Result := F.ClassName + Result
        else begin
          if TControl(Component).Parent <> nil then
            Result := TControl(Component).Parent.Name + Result;
        end;
      end
      else begin
        Owner := Component.Owner;
        if Owner is TForm then
          Result := Format('%s.%s', [Owner.ClassName, Result]);
      end;
    end;
  end
  else Result := '';
end;

function GetDefaultIniName: string;
var
  S:string;
begin
  if Assigned(OnGetDefaultIniName) then
    Result:= OnGetDefaultIniName()
  else
  begin
    Result := ExtractFileName(ChangeFileExt(Application.ExeName, '.ini'));
    S:=RxGetAppConfigDir(false);
    S:=SysToUTF8(S);
    ForceDirectoriesUTF8(S);
    Result:=S+Result;
  end;
end;

procedure GetDefaultIniData(Control: TControl; var IniFileName,
  Section: string; UseRegistry: Boolean );
var
  I: Integer;
begin
  IniFileName := EmptyStr;
{  with Control do
    if Owner is TCustomForm then
      for I := 0 to Owner.ComponentCount - 1 do
        if (Owner.Components[I] is TFormPropertyStorage) then
        begin
          IniFileName := TFormPropertyStorage(Owner.Components[I]).IniFileName;
          Break;
        end;}
  Section := GetDefaultSection(Control);
  if IniFileName = EmptyStr then
    if UseRegistry then IniFileName := GetDefaultIniRegKey
    else
      IniFileName := GetDefaultIniName;
end;

procedure IniWriteString(IniFile: TObject; const Section, Ident,
  Value: string);
var
  S: string;
begin
  if IniFile is TRegIniFile then
    TRegIniFile(IniFile).WriteString(Section, Ident, Value)
  else
  begin
    S := Value;
    if S <> '' then
    begin
      if ((S[1] = '"') and (S[Length(S)] = '"')) or
        ((S[1] = '''') and (S[Length(S)] = '''')) then
        S := '"' + S + '"';
    end;
    if IniFile is TIniFile then
      TIniFile(IniFile).WriteString(Section, Ident, S);
  end;
end;

function IniReadString(IniFile: TObject; const Section, Ident, Value: string
  ): string;
var
  S: string;
begin
  if IniFile is TRegIniFile then
    Result:=TRegIniFile(IniFile).ReadString(Section, Ident, Value)
  else
  begin
    S := Value;
    if S <> '' then begin
      if ((S[1] = '"') and (S[Length(S)] = '"')) or
        ((S[1] = '''') and (S[Length(S)] = '''')) then
        S := '"' + S + '"';
    end;
    if IniFile is TIniFile then
      Result:=TIniFile(IniFile).ReadString(Section, Ident, S);
  end;
end;

procedure IniWriteInteger(IniFile: TObject; const Section, Ident: string;
  const Value: integer);
begin
  if IniFile is TRegIniFile then
    TRegIniFile(IniFile).WriteInteger(Section, Ident, Value)
  else
  begin
    if IniFile is TIniFile then
      TIniFile(IniFile).WriteInteger(Section, Ident, Value);
  end;
end;

function IniReadInteger(IniFile: TObject; const Section, Ident: string;
  const Value: integer): integer;
begin
  if IniFile is TRegIniFile then
    Result:=TRegIniFile(IniFile).ReadInteger(Section, Ident, Value)
  else
  begin
    if IniFile is TIniFile then
      Result:=TIniFile(IniFile).ReadInteger(Section, Ident, Value);
  end;
end;

function GetDefaultIniRegKey: string;
begin
  if RegUseAppTitle and (Application.Title <> '') then
    Result := Application.Title
  else Result := ExtractFileName(ChangeFileExt(Application.ExeName, ''));
  if DefCompanyName <> '' then
    Result := DefCompanyName + '\' + Result;
  Result := 'Software\' + Result;
end;


end.

