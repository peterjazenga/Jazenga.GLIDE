 {$mode objfpc}{$h+}
{ $INCLUDE wst_global.inc}
unit imp_helper;

interface

uses
  Classes, SysUtils,
  base_service_intf,
  base_binary_formatter;


  procedure SaveObjectToStream(AObject : TPersistent; AStream : TStream);
  procedure LoadObjectFromStream(AObject : TPersistent; AStream : TStream);
  
  procedure SaveObjectToFile(AObject : TPersistent; const AFileName : string);
  procedure LoadObjectFromFile(AObject : TPersistent; const AFileName : string);

implementation
uses TypInfo;

procedure SaveObjectToFile(AObject : TPersistent; const AFileName : string);
var
  strm : TFileStream;
begin
  if FileExists(AFileName) then
    DeleteFile(AFileName);
  strm := TFileStream.Create(AFileName,fmCreate);
  try
    SaveObjectToStream(AObject,strm);
  finally
    strm.Free();
  end;
end;

procedure LoadObjectFromFile(AObject : TPersistent; const AFileName : string);
var
  strm : TFileStream;
begin
  if not FileExists(AFileName) then
    raise Exception.CreateFmt('File not found : "%s"',[AFileName]);
  strm := TFileStream.Create(AFileName,fmOpenRead);
  try
    LoadObjectFromStream(AObject,strm);
  finally
    strm.Free();
  end;
end;

procedure SaveObjectToStream(AObject : TPersistent; AStream : TStream);
var
  srlzr : IFormatterBase;
begin
  srlzr := TBaseBinaryFormatter.Create();
  srlzr.BeginObject('root',TypeInfo(TPersistent));
    srlzr.Put('object',PTypeInfo(AObject.ClassInfo),AObject);
  srlzr.EndScope();
  srlzr.SaveToStream(AStream);
end;

procedure LoadObjectFromStream(AObject : TPersistent; AStream : TStream);
var
  srlzr : IFormatterBase;
  nme : string;
begin
  srlzr := TBaseBinaryFormatter.Create();
  srlzr.LoadFromStream(AStream);
  nme := 'root';
  srlzr.BeginObjectRead(nme,TypeInfo(TPersistent));
    nme := 'object';
    srlzr.Get(PTypeInfo(AObject.ClassInfo),nme,AObject);
  srlzr.EndScopeRead();
end;


end.

