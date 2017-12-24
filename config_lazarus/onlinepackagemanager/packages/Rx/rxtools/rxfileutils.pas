{ rxFileUtils is part of RxFPC library

  Copyright (C) 2005-2017 Lagunov Aleksey alexs@yandex.ru and Lazarus team
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

unit rxFileUtils;

{$I rx.inc}

interface

uses
  SysUtils;

function GetFileOwnerUser(const SearchDomain, FileName:String):String;
procedure GetFileOwnerData(const SearchDomain, FileName:String;out UserName, DomainName:string);
function NormalizeDirectoryName(const DirName:string):string;
function GetUserName:string;

function IsValidFileNameChar(const AChar: Char): Boolean;inline;
function NormalizeFileName(const FileName:string; AReplaceChar:char = '_'):string; //funtion only for filename - without folder name

const
  {$IFDEF WINDOWS}
  FileNameDisabledChars = [#0 .. #31, '"', '*', '/', ':', '<', '>', '?', '\' , '|'];
  {$ELSE}
  FileNameDisabledChars = [#0 .. #31, '/', '~'];
  {$ENDIF}

implementation

uses
{$IFDEF WINDOWS}
   Windows
{$ELSE}
  BaseUnix, users, strutils
{$ENDIF};
(*
 FileUtil, LazFileUtils, LazUTF8;
*)
{$IF DEFINED(WINDOWS) AND NOT DEFINED(WINCE)}
function LStrError(const Ernum: Longint; const UseUTF8: Boolean = False): string;
const
  MAX_ERROR = 1024;
var
  Tmp: string;
  TmpW: widestring;
begin
  Result := ' [' + IntToStr(Ernum) + ']: ';
  if USEUtf8 then begin
    SetLength(TmpW, MAX_ERROR);
    SetLength(TmpW, FormatMessageW(FORMAT_MESSAGE_FROM_SYSTEM or
                                   FORMAT_MESSAGE_IGNORE_INSERTS or
                                   FORMAT_MESSAGE_ARGUMENT_ARRAY,
                                   nil, Ernum, 0, @TmpW[1], MAX_ERROR, nil));
    Tmp := UTF8Encode(TmpW);
  end else begin
    SetLength(Tmp, MAX_ERROR);
    SetLength(Tmp, FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM or
                                 FORMAT_MESSAGE_IGNORE_INSERTS or
                                 FORMAT_MESSAGE_ARGUMENT_ARRAY,
                                 nil, Ernum, 0, @Tmp[1], MAX_ERROR, nil));
  end;
  if Length(Tmp) > 2 then
    Delete(Tmp, Length(Tmp)-1, 2);
  Result := Result + Tmp;
end;

procedure GetFileNameOwner(const SearchDomain, FileName: String; out UserName, DomainName: string);
var
  RCode, RC1:WINBOOL;
  SDSize:DWORD;      // Size of security descriptor

  FAccountName:PChar;   // Account name
  lngAccountLen:DWORD;  // Length of account name
  FDomainName:PChar;    // Domain name
  lngDomainLen:DWORD;   // Length of domain name

  ptrUse:SID_NAME_USE;         // Pointer to SID_NAME_USE
  ptrOwner:PSID;
  P:PByteArray;
begin
  ptrOwner:=nil;
  SDSize:=0;
  P:=nil;
  UserName:='';
  DomainName:='';

  RCode := GetFileSecurity(PChar(FileName), OWNER_SECURITY_INFORMATION, nil, 0, @SDSize);
  GetMem(P, SDSize);
  FillChar(P^, SDSize, 0);
  RCode := GetFileSecurity(PChar(FileName), OWNER_SECURITY_INFORMATION, Pointer(P), SDSize, @SDSize);
  if not RCode then
    raise Exception.Create(LStrError(GetLastError, true));

  RCode := GetSecurityDescriptorOwner(Pointer(P), ptrOwner, @RC1);
  if not RCode then
    raise Exception.Create(LStrError(GetLastError, true));

  lngAccountLen:=0;
  lngDomainLen:=0;
  RCode := LookupAccountSid(PChar(SearchDomain), ptrOwner, nil, lngAccountLen, nil, lngDomainLen, ptrUse);
  //' Configure the strings' buffer sizes
  GetMem(FAccountName, lngAccountLen);
  FillChar(FAccountName^, lngAccountLen, 0);
  GetMem(FDomainName, lngDomainLen);
  FillChar(FDomainName^, lngDomainLen, 0);

  RCode:=LookupAccountSid(PChar(SearchDomain), ptrOwner, FAccountName, lngAccountLen, FDomainName, lngDomainLen, ptrUse);

  if not RCode then
    raise Exception.Create(LStrError(GetLastError, true));

  UserName:=FAccountName;
  DomainName:=FDomainName;

  Freemem(P, SDSize);
  Freemem(FAccountName, lngAccountLen);
  Freemem(FDomainName, lngDomainLen);
end;
{$ELSE}
{$ENDIF}

function GetFileOwnerUser(const SearchDomain, FileName: String): String;
var
  S:string;
begin
  {$IF DEFINED(WINDOWS) AND NOT DEFINED(WINCE)}
(*  GetFileNameOwner(UTF8ToSys(SearchDomain), UTF8ToSys(FileName), Result, S);
  Result:=UTF8Encode(Result);*)
  GetFileNameOwner(SearchDomain, FileName, Result, S);
  {$ELSE}
  Result:='';
  {$ENDIF}
end;

procedure GetFileOwnerData(const SearchDomain, FileName: String; out UserName,
  DomainName: string);
{$IF DEFINED(WINDOWS) AND NOT DEFINED(WINCE)}
{$ELSE}
var
  SR: stat;
  {$ENDIF}
begin
  {$IF DEFINED(WINDOWS) AND NOT DEFINED(WINCE)}
{  GetFileNameOwner(UTF8ToSys(SearchDomain), UTF8ToSys(FileName), UserName, DomainName);
  UserName:=UTF8Encode(UserName);
  DomainName:=UTF8Encode(DomainName);}
  GetFileNameOwner(SearchDomain, FileName, UserName, DomainName);
  {$ELSE}
  FpStat(FileName, SR);
  UserName:=users.GetUserName(SR.uid);
  if Pos('\', UserName) > 0 then
    DomainName:=Copy2SymbDel(UserName, '\') //for unix samba WinBIND
  else
    DomainName:='';//IntToStr( SR.gid);
  {$ENDIF}
end;

{replase any dir separators '\' or '/' to system directory separator }
function NormalizeDirectoryName(const DirName: string): string;
var
  i:integer;
begin
  Result:=DirName;
  for i:=1 to Length(Result) do
    if Result[i] in ['/', '\'] then
      Result[i]:=DirectorySeparator;
end;

function GetUserName: string;
{$IF DEFINED(WINDOWS) AND NOT DEFINED(WINCE)}
var
  A:array [0..256] of Char;
  L:DWORD;
{$ENDIF}
begin
  {$IF DEFINED(WINDOWS) AND NOT DEFINED(WINCE)}
  FillChar(A, SizeOf(A), 0);
  L:=SizeOf(A)-1;
  if Windows.GetUserNameA(@A, L) then
  begin
(*    Result:=SysToUTF8(StrPas(@A)); *)
    Result:=StrPas(@A);
  end
  else
    (*Result:=GetEnvironmentVariableUTF8('USERNAME');*)
    Result:=SysUtils.GetEnvironmentVariable('USERNAME');
  {$ELSE}
  Result:=GetEnvironmentVariable('USER');
  {$ENDIF}
end;

function IsValidFileNameChar(const AChar: Char): Boolean;
begin
  Result:=not (AChar in FileNameDisabledChars);
end;

function NormalizeFileName(const FileName: string; AReplaceChar:char = '_'): string;
var
  i:integer;
begin
  Result:=FileName;
  for i:=1 to Length(Result) do
    if not IsValidFileNameChar(Result[i]) then
      Result[i]:=AReplaceChar;
end;

end.

