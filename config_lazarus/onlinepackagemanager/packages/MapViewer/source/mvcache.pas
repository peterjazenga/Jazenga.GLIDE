{
  Picture cache manager (c) 2014 ti_dic

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}

//=== ct9999 Modify for CodeTyphon Studio ================

unit mvCache;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,mvmapprovider,IntfGraphics,
  FPimage,GraphType,FPReadJPEG,
  syncObjs,mvtypes;

Type

   { TPictureCache }

   TPictureCache = Class(TComponent)
   private
     FMemMaxElem : integer;
     Crit : TCriticalSection;
     Cache : TStringList;
     FBasePath: String;
     FUseDisk: Boolean;
     FUseThreads: Boolean;
     procedure SetUseThreads(AValue: Boolean);
     Procedure EnterCrit;
     Procedure LeaveCrit;
   protected
     function GetNewImgFor(aStream : TStream) : TLazIntfImage;
     procedure FreeCache;
     Function MapProvider2FileName(MapProvider : TMapProvider) : String;
     Function DiskCached(const aFileName : String) : Boolean;
     procedure LoadFromDisk(const aFileName : String;out img : TLazIntfImage);
     Function GetFileName(MapProvider : TMapProvider;const TileId : TTileId) : String;
   public
     Procedure CheckCacheSize(sender : TObject);
     constructor Create(aOwner : TComponent);override;
     destructor destroy;override;
     Procedure Add(MapProvider : TMapProvider;const TileId : TTileId;Stream : TMemoryStream);
     Procedure GetFromCache(MapProvider : TMapProvider;const TileId : TTileId;out img : TLazIntfImage);
     function InCache(MapProvider : TMapProvider;const TileId : TTileId) : Boolean;

     property UseDisk : Boolean read FUseDisk write FUseDisk;
     property BasePath : String read FBasePath write FBasePath;
     property UseThreads : Boolean read FUseThreads write SetUseThreads;
   end;


implementation

{ TPictureCache }

function IsValidPNG(stream: TStream): Boolean;
var
  s: string;
  y: Int64;
begin
  if Assigned(stream) then
  begin
    SetLength(s, 3);
    y := stream.Position;
    stream.Position := 1;
    stream.Read(s[1], 3);
    stream.Position := y;
    Result := s = 'PNG';
  end
  else
    Result := False;
end;

function IsValidJPEG(stream: TStream): Boolean;
var
  s: string;
  y: Int64;
begin
  if Assigned(stream) then
  begin
    SetLength(s, 4);
    y := stream.Position;
    stream.Position := 6;
    stream.Read(s[1], 4);
    stream.Position := y;
    Result := (s = 'JFIF') or (s = 'Exif');
  end
  else
    Result := False;
end;

procedure TPictureCache.SetUseThreads(AValue: Boolean);
begin
  if FUseThreads=AValue then Exit;
  FUseThreads:=AValue;
  if aValue then
    Crit:=TCriticalSection.Create
  else
    FreeAndnil(Crit);
end;

procedure TPictureCache.EnterCrit;
begin
  if Assigned(Crit) then
     Crit.Enter;
end;

procedure TPictureCache.LeaveCrit;
begin
  if Assigned(Crit) then
     Crit.Leave;
end;

function TPictureCache.GetNewImgFor(aStream: TStream): TLazIntfImage;
var
  reader :   TFPCustomImageReader;
  rawImg : TRawImage;
begin
  result:=nil;
  Reader := nil;
  if not(assigned(aStream)) then
     exit;
  if IsValidJPEG(astream) then
    Reader := TFPReaderJPEG.create
  else
    if IsValidPNG(astream) then
      Reader  := TLazReaderPNG.create;
  if Assigned(reader) then
  Begin
    try
      rawImg.Init;
      rawImg.Description.Init_BPP24_B8G8R8_BIO_TTB(TILE_SIZE,TILE_SIZE);
      Result:=TLazIntfImage.create(rawImg,true);
      Try
         Result.LoadFromStream(aStream,reader);
      except
         FreeAndNil(result);
      end;
    finally
      FreeAndNil(Reader)
    end;
  end;
end;

procedure TPictureCache.FreeCache;
var i : integer;
begin
  EnterCrit;
  Try
    For i:=0 to pred(Cache.Count) do
    begin
      Cache.Objects[i].Free;
    end;
    Cache.Clear;
  finally
    LeaveCrit;
  end;
end;

function TPictureCache.MapProvider2FileName(MapProvider: TMapProvider): String;
var i : integer;
begin
  Result:='';
  if Assigned(MapProvider) then
  begin
    Result:=MapProvider.Name;
    For i:=1 to length(Result) do
        if not(result[i] in ['a'..'z','A'..'Z','0'..'9','_','.']) then
           Result[i]:='-';
  end;
end;

function TPictureCache.DiskCached(const aFileNAme: String): Boolean;
Var FullFileName : string;
begin
  if UseDisk then
  Begin
    FullFileName:=BasePath+aFileName;
    Result:=FileExists(FullFileName);
  end
  Else
    Result:=False;
end;

procedure TPictureCache.LoadFromDisk(const aFileName: String; out
  img: TLazIntfImage);
var FullFileName : String;
    aStream : TFileStream;
begin
  img:=nil;
  FullFileName:=BasePath+aFileName;
  if FileExists(fullFileName) then
  Begin
    aStream:=TFileStream.Create(FullFileName,fmOpenRead);
    try
      Try
        img:=GetNewImgFor(aStream);
      except
         FreeAndNil(img);
      end;
      if Assigned(img) then
      begin
        EnterCrit;
        Try
         Cache.AddObject(aFileName,img);
        finally
          LeaveCrit;
        end;
      end;
    finally
      aStream.Free;
    end;
  end;
end;

function TPictureCache.GetFileName(MapProvider: TMapProvider;const  TileId: TTileId
  ): String;
begin
  Result:=MapProvider2FileName(MapProvider)+'_'+inttostr(TileId.X)+'_'+inttostr(TileId.Y)+'_'+inttostr(TileId.Z);
end;

procedure TPictureCache.CheckCacheSize(Sender : TObject);
var i ,idx : integer;
begin
  EnterCrit;
  try
    if Cache.Count>FMemMaxElem then
    Begin
        For i:=1 to 10 do
        Begin
             idx:=pred(Cache.Count);
             if idx>1 then
             Begin
               Cache.Objects[idx].free;
               Cache.Delete(idx);
             end;
        end;
    end;
  finally
    LeaveCrit;
  end;
end;

constructor TPictureCache.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FMemMaxElem :=2048 div 256;
  Cache:=TStringList.create;
end;

destructor TPictureCache.destroy;
begin
  inherited destroy;
  FreeCache;
  FreeAndNil(Crit);
end;

procedure TPictureCache.Add(MapProvider: TMapProvider;const  TileId: TTileId;
  Stream: TMemoryStream);
var FileName : String;
    img : TLazIntfImage;
    aFile : TFileStream;
    idx : integer;
begin
  FileName:=GetFileName(MapProvider,TileId);
  EnterCrit;
  Try
    idx:=Cache.IndexOF(FileName);
    if idx<>-1 then
      Cache.Objects[idx].Free
    else
    Begin
      Cache.Insert(0,FileName);
      idx:=0;
    end;
    img:=GetNewImgFor(Stream);
    Cache.Objects[idx]:=img;
  finally
    LeaveCrit;
  end;
  if UseDisk then
  Begin
    if assigned(img) then
    Begin
      aFile:=TFileStream.Create(BasePath+FileName,fmCreate);
      Try
        Stream.Position:=0;
        aFile.CopyFrom(Stream,0);
      finally
        FreeAndNil(aFile);
      end;
    end;
  end;
  if Not(FUseThreads) then
    CheckCacheSize(self);
end;

procedure TPictureCache.GetFromCache(MapProvider: TMapProvider;const  TileId: TTileId; out img: TLazIntfImage);
var FileName : String;
    idx : integer;
begin
  img:=nil;
  FileName:=GetFileName(MapProvider,TileId);
  EnterCrit;
  Try
    idx:=Cache.IndexOF(FileName);
    if idx<>-1 then
    Begin
      img:=TLazIntfImage(Cache.Objects[idx]);
      if Idx>FMemMaxElem div 2 then
      Begin
        Cache.Delete(idx);
        Cache.Insert(0,FileName);
        Cache.Objects[0]:=img;
      end;
    end;

  finally
    leaveCrit;
  end;
  if idx=-1 then
  Begin
    if UseDisk then
       LoadFromDisk(FileName,img);
  end;
end;

function TPictureCache.InCache(MapProvider: TMapProvider;const  TileId: TTileId
  ): Boolean;
var FileName : String;
    idx : integer;
begin
  FileName:=GetFileName(MapProvider,TileId);
  EnterCrit;
  try
    idx:=Cache.IndexOF(FileNAme);
  finally
    leaveCrit;
  end;
  if idx<>-1 then
     Result:=True
  else
     Result:=DiskCached(FileName);
end;

end.

