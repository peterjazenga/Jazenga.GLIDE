{ RxVersInfo is part of RxFPC library

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

unit RxVersInfo;

{$mode objfpc}

interface

uses
  Classes, SysUtils, versionresource, lclversion;

type
  TLongVersion = string;
  TVersionCharSet = string;
  TVersionLanguage = string;

  { TRxVersionInfo }

  TRxVersionInfo = class(TComponent)
  private
    FValid: Boolean;
    FValues:TStringList;
    FFileName: string;
    function GetComments: string;
    function GetCompanyName: string;
    function GetFileDescription: string;
    function GetFileLongVersion: TLongVersion;
    function GetFileName: string;
    function GetFileVersion: string;
    //function GetFixedFileInfo: PVSFixedFileInfo;
    function GetInternalName: string;
    function GetLegalCopyright: string;
    function GetLegalTrademarks: string;
    function GetOriginalFilename: string;
    function GetPrivateBuild: string;
    function GetProductLongVersion: TLongVersion;
    function GetProductName: string;
    function GetProductVersion: string;
    function GetSpecialBuild: string;
    function GetTranslation: Pointer;
    function GetVerFileDate: TDateTime;
    function GetVersionCharSet: TVersionCharSet;
    function GetVersionLanguage: TVersionLanguage;
    function GetVersionNum: Longint;
    function GetVerValue(const VerName: string): string;
    function GetWidgetName: string;
    procedure SetFileName(const AValue: string);
    procedure DoVersionInfo(V:TVersionResource);
  protected
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy; override;
    procedure LoadFromFile(const AFileName:string);

    property FileName: string read GetFileName write SetFileName;
    property Valid: Boolean read FValid;
    //property FixedFileInfo: PVSFixedFileInfo read GetFixedFileInfo;
    property FileLongVersion: TLongVersion read GetFileLongVersion;
    property ProductLongVersion: TLongVersion read GetProductLongVersion;
    property Translation: Pointer read GetTranslation;
    property VersionLanguage: TVersionLanguage read GetVersionLanguage;
    property VersionCharSet: TVersionCharSet read GetVersionCharSet;
    property VersionNum: Longint read GetVersionNum;
    property Comments: string read GetComments;
    property CompanyName: string read GetCompanyName;
    property FileDescription: string read GetFileDescription;
    property FileVersion: string read GetFileVersion;
    property InternalName: string read GetInternalName;
    property LegalCopyright: string read GetLegalCopyright;
    property LegalTrademarks: string read GetLegalTrademarks;
    property OriginalFilename: string read GetOriginalFilename;
    property ProductVersion: string read GetProductVersion;
    property ProductName: string read GetProductName;
    property SpecialBuild: string read GetSpecialBuild;
    property PrivateBuild: string read GetPrivateBuild;
    property Values[const VerName: string]: string read GetVerValue;
    property VerFileDate: TDateTime read GetVerFileDate;
  published
    property WidgetName:string read GetWidgetName;
  end;


implementation
uses FileUtil, resource, resreader, InterfaceBase, rxconst, LazFileUtils,
  LazUTF8
{$IFDEF WINDOWS}
  , winpeimagereader
{$ENDIF}
{$IFDEF LINUX}
  , elfreader
{$ENDIF}
{$IF (lcl_major > 0) and (lcl_minor > 6)}, LCLPlatformDef {$ENDIF};


{ TRxVersionInfo }

function TRxVersionInfo.GetComments: string;
begin
  Result:=FValues.Values['Comments'];
end;

function TRxVersionInfo.GetCompanyName: string;
begin
  Result:=FValues.Values['CompanyName'];
end;

function TRxVersionInfo.GetFileDescription: string;
begin
  Result:=FValues.Values['FileDescription'];
end;

function TRxVersionInfo.GetFileLongVersion: TLongVersion;
begin
  Result:=FValues.Values['FileVersion'];
end;

function TRxVersionInfo.GetFileName: string;
begin
  Result:=FValues.Values['OriginalFilename'];
  if Result = '' then
    Result:=FFileName;
end;

function TRxVersionInfo.GetFileVersion: string;
begin
  Result:=FValues.Values['FileVersion'];
end;

{function TRxVersionInfo.GetFixedFileInfo: PVSFixedFileInfo;
begin
  Result:='';
end;}

function TRxVersionInfo.GetInternalName: string;
begin
  Result:=FValues.Values['InternalName'];
end;

function TRxVersionInfo.GetLegalCopyright: string;
begin
  Result:=FValues.Values['LegalCopyright'];
end;

function TRxVersionInfo.GetLegalTrademarks: string;
begin
  Result:=FValues.Values['LegalTrademarks'];
end;

function TRxVersionInfo.GetOriginalFilename: string;
begin
  Result:=FValues.Values['LegalTrademarks'];
end;

function TRxVersionInfo.GetPrivateBuild: string;
begin
  Result:='';
end;

function TRxVersionInfo.GetProductLongVersion: TLongVersion;
begin
  Result:='';
end;

function TRxVersionInfo.GetProductName: string;
begin
  Result:=FValues.Values['ProductName'];
end;

function TRxVersionInfo.GetProductVersion: string;
begin
  Result:=FValues.Values['ProductVersion'];
end;

function TRxVersionInfo.GetSpecialBuild: string;
begin
  Result:='';
end;

function TRxVersionInfo.GetTranslation: Pointer;
begin
  Result:=nil;
end;

function TRxVersionInfo.GetVerFileDate: TDateTime;
begin
  Result:=0;
end;

function TRxVersionInfo.GetVersionCharSet: TVersionCharSet;
begin
  Result:='';
end;

function TRxVersionInfo.GetVersionLanguage: TVersionLanguage;
begin
  Result:='';
end;

function TRxVersionInfo.GetVersionNum: Longint;
begin
  Result:=0;
end;

procedure TRxVersionInfo.SetFileName(const AValue: string);
begin
  LoadFromFile(AValue);
end;

procedure TRxVersionInfo.DoVersionInfo(V: TVersionResource);
var
  i,j:integer;
begin
  for i:=0 to V.StringFileInfo.Count-1 do
  begin
    for j:=0 to V.StringFileInfo[i].Count-1 do
      FValues.Values[V.StringFileInfo[i].Keys[j]]:=SysToUTF8(V.StringFileInfo[i].ValuesByIndex[j]);
  end;
end;

constructor TRxVersionInfo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FValues:=TStringList.Create;
  LoadFromFile(ParamStr(0));
end;

destructor TRxVersionInfo.Destroy;
begin
  FreeAndNil(FValues);
  inherited Destroy;
end;

procedure TRxVersionInfo.LoadFromFile(const AFileName: string);
var
  Res:TResources;
  i:integer;
  Reader:TAbstractResourceReader;
  V:TVersionResource;
begin
  FFileName:=AFileName;
  FValues.Clear;
  FValid:=false;
  Reader:=nil;
  {$IFDEF WINDOWS}
  Reader:=TWinPEImageResourceReader.Create;
  {$ENDIF}
  {$IFDEF LINUX}
  Reader:=TElfResourceReader.Create;
  {$ENDIF}

  if Reader = nil then
    exit;

  Res:=TResources.Create;
  V:=nil;
  try
    Res.LoadFromFile(AFileName, Reader);
    for i:=0 to Res.Count-1 do
    begin
      if Res[i] is TVersionResource then
        V:=Res[i] as TVersionResource;
    end;
    FValid:=Assigned(V);
    if FValid then
      DoVersionInfo(V);
  finally
    Res.Free;
    Reader.Free;
  end;
end;

function TRxVersionInfo.GetVerValue(const VerName: string): string;
begin
  Result:=FValues.Values[VerName];
end;

function TRxVersionInfo.GetWidgetName: string;
begin
  Result:=sWidget + LCLPlatformDisplayNames[WidgetSet.LCLPlatform];
{  case WidgetSet.LCLPlatform of
    lpGtk:Result:=sGTKWidgetSet;
    lpGtk2:Result:=sGTK2WidgetSet;
    lpWin32:Result:=sWin32_64WidgetSet;
    lpWinCE:Result:=sWinCEWidgetSet;
    lpCarbon:Result:=sCarbonWidgetSet;
    lpQT:Result:=sQTWidgetSet;
    lpfpGUI:Result:=sFpGUIWidgetSet;
  else
    Result:=sOtherGUIWidgetSet;
  end;}
end;

end.
