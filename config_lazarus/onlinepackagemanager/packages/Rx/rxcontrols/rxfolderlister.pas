{ folderlister unit

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

unit rxfolderlister;

{$I rx.inc}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Menus;


type
  { TCustomFolderLister }

  TCustomFolderLister = class(TComponent)
  private
    FDefaultExt: string;
    FMenuItem: TMenuItem;
    FOnExecuteItem: TNotifyEvent;
    FFileFolder: string;
    FFileList:TStringList;
    procedure DoFind(S:string; MenuItem:TMenuItem);
    function GetCount: integer;
    function GetFiles(Item: integer): string;
    procedure SetMenuItem(const AValue: TMenuItem);
    procedure SetFileFolder(const AValue: string);
  protected
    property FileFolder:string read FFileFolder write SetFileFolder;
    property OnExecuteItem:TNotifyEvent read FOnExecuteItem write FOnExecuteItem;
    property MenuItem:TMenuItem read FMenuItem write SetMenuItem;
    property DefaultExt:string read FDefaultExt write FDefaultExt;
    procedure InternalExecute(Sender: TObject);virtual;
  public
    procedure Execute;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Files[Item:integer]:string read GetFiles;
    property Count:integer read GetCount;
  published
  end;

type
  TFolderLister = class(TCustomFolderLister)
  published
    property DefaultExt;
    property FileFolder;
    property OnExecuteItem;
    property MenuItem;
  end;
  
implementation
uses FileUtil, strutils, RxAppUtils, LazUTF8, LazFileUtils, rxconst;

function MenuItemStr(S:string):string;
var
  i:integer;
begin
  Result:=Copy2Symb(ExtractFileName(S), '.');
  if Result='' then exit;
  for i:=1 to Length(Result) do
  begin
    if Result[i]='\' then Result[i]:='/' else
    if Result[i]='_' then Result[i]:='.';
  end;
end;

{ TCustomFolderLister }
procedure TCustomFolderLister.DoFind(S: string; MenuItem: TMenuItem);
var
  Rec:TSearchRec;
  R:integer;
  AFileList,
  AFolderList:TStringList;

procedure CreateItems;
var
  i:integer;
  M:TMenuItem;
begin
  for I:=0 to AFileList.Count-1 do
  begin
    FFileList.Add(AFileList[i]);
    M:=TMenuItem.Create(Application.MainForm);
    M.Caption:=MenuItemStr(AFileList[i]);
    M.Hint:=MenuItemStr(AFileList[i]);
    MenuItem.Add(M);
    M.Tag:=FFileList.Count-1;
    M.OnClick:=@InternalExecute;
  end;
end;

procedure CreateSubItems;
var
  i:integer;
  M:TMenuItem;
  S:string;
begin
  for i:=0 to AFolderList.Count-1 do
  begin
    M:=TMenuItem.Create(MenuItem.Owner);//Application.MainForm);
    S:=AFolderList[i];
    M.Caption:=MenuItemStr(S);
    MenuItem.Add(M);
    DoFind(AFolderList[i]+DirectorySeparator,M);
  end;
end;

var
  SS:string;
begin
  AFolderList:=TStringList.Create;
  AFolderList.Sorted:=true;
  AFileList:=TStringList.Create;
  AFolderList.Sorted:=true;
  try
    R:=FindFirstUTF8(S+AllMask,faAnyFile, Rec);
    while R=0 do
    begin
      if ((Rec.Attr and faDirectory) <>0) and (Rec.Name<>'.') and (Rec.Name<>'..') then
      begin
        SS:=S+Rec.Name;
        AFolderList.Add(SS)
      end
      else
      begin
        if UTF8LowerCase(ExtractFileExt(Rec.Name))=UTF8LowerCase(FDefaultExt) then
        begin
          SS:=S+Rec.Name;
          AFileList.Add(SS);
        end;
      end;
      R:=FindNextUTF8(Rec);
    end;
    FindCloseUTF8(Rec);
    CreateSubItems;
    CreateItems;
  finally
    AFolderList.Free;
    AFileList.Free;
  end;
end;

function TCustomFolderLister.GetCount: integer;
begin
  Result:=FFileList.Count;
end;

function TCustomFolderLister.GetFiles(Item: integer): string;
begin
  Result:=FFileList[Item];
end;

procedure TCustomFolderLister.SetMenuItem(const AValue: TMenuItem);
begin
  if FMenuItem=AValue then exit;
  FMenuItem:=AValue;
end;

procedure TCustomFolderLister.SetFileFolder(const AValue: string);
begin
  if FFileFolder=AValue then exit;
  FFileFolder:=AValue;
  if FFileFolder<>'' then
    if FFileFolder[Length(FFileFolder)]<>DirectorySeparator then
      FFileFolder:=FFileFolder+DirectorySeparator;
end;

procedure TCustomFolderLister.InternalExecute(Sender: TObject);
begin
  if Assigned(FOnExecuteItem) then
    FOnExecuteItem(Sender)
end;

procedure TCustomFolderLister.Execute;
begin
  if Assigned(FMenuItem) then
    DoFind(FFileFolder, FMenuItem)
  else
    raise Exception.CreateFmt( sFolderListerErr, [Name]);
end;

constructor TCustomFolderLister.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFileList:=TStringList.Create;
  FFileList.Sorted:=false;
end;

destructor TCustomFolderLister.Destroy;
begin
  FFileList.Free;
  inherited Destroy;
end;

end.
