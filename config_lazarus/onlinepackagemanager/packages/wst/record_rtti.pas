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
unit record_rtti;

{$RANGECHECKS OFF}

interface

uses
  SysUtils, TypInfo, wst_types;

type

  PRecordFieldInfo = ^TRecordFieldInfo;
  TRecordFieldInfo = packed record
    Name : shortstring;
    TypeInfo : PPTypeInfo;
    Offset : PtrUInt;
    IsAttribute : Boolean;
    Visible : Boolean;
  end;

  PRecordTypeData = ^TRecordTypeData;
  TRecordTypeData = packed record
    Name : shortstring;
    RecordSize : PtrUInt;
    FieldCount: PtrUInt;
    Fields: array [0..0] of TRecordFieldInfo;
  end;
  
  { TRecordRttiDataObject }

  TRecordRttiDataObject = class(TDataObject)
  public
    constructor Create(const AData : PRecordTypeData; const AFieldList : string);
    destructor Destroy();override;
    function GetRecordTypeData() : PRecordTypeData;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function FindField(const AFieldName : shortstring) : PRecordFieldInfo;
    function GetField(const AFieldName : shortstring) : PRecordFieldInfo;
  end;

  function MakeRecordTypeInfo(ARawTypeInfo : PTypeInfo) : PRecordTypeData;
  procedure FreeRecordTypeInfo(ATypeInfo : PRecordTypeData);{$IFDEF USE_INLINE}inline;{$ENDIF}

{$IFDEF WST_RECORD_RTTI}
  function MakeRawTypeInfo(
    const ATypeName : string;
    const ATypeSize : PtrUInt;
    const AOffset   : array of PtrUInt;
    const ATypes    : array of PTypeInfo
  ):PTypeInfo ;
{$ENDIF WST_RECORD_RTTI}

  procedure initialize_record_rtti();
  procedure finalize_record_rtti();
  
implementation
uses 
  Classes, imp_utils, wst_consts;

{$IFDEF WST_RECORD_RTTI}

var
  RawTypeInfoList : TList = nil;

type
  PFieldInfo = ^TFieldInfo;
  TFieldInfo = packed record
    TypeInfo: PPTypeInfo;
    Offset: Cardinal;
  end;

  PFieldTable = ^TFieldTable;
  TFieldTable = packed record
    X: Word;
    Size: Cardinal;
    Count: Cardinal;
    Fields: array [0..0] of TFieldInfo;
  end;

function MakeRawTypeInfo(
  const ATypeName : string;
  const ATypeSize : PtrUInt;
  const AOffset   : array of PtrUInt;
  const ATypes    : array of PTypeInfo
):PTypeInfo ;
var
  i, j, bufferSize, count : LongInt;
  delphiFT : PFieldTable;
  resBuffer, tmp : PByte;
  fieldInfo : PFieldInfo;
  typ : PTypeInfo;
begin
  count := Length(AOffset);
  Assert(count = Length(ATypes));
  bufferSize :=
    1 + // Kind
    1 + Length(ATypeName) +
    SizeOf(Word) + // X
    SizeOf(Cardinal) + // Size
    SizeOf(Cardinal) + // Count
    ( count * SizeOf(TFieldInfo) );
  GetMem(resBuffer,bufferSize);
  FillChar(Pointer(resBuffer)^,bufferSize,#0);
  tmp := resBuffer;
  typ := PTypeInfo(resBuffer);
  typ^.Kind := tkRecord;
  PByte(@(typ^.Name[0]))^ := Length(ATypeName);
  Move(ATypeName[1],typ^.Name[1],Length(ATypeName));

  Inc(tmp,SizeOf(TTypeKind)); // Kind
  Inc(tmp,1 + Byte(typ^.Name[0])); // Name

  delphiFT := PFieldTable(tmp);
  delphiFT^.X := 0;
  delphiFT^.Size := ATypeSize;
  delphiFT^.Count := count;
  for i := 1 to count do begin
    j := i - 1;
    fieldInfo := @(delphiFT^.Fields[j]);
    fieldInfo^.Offset := AOffset[j];
    GetMem(fieldInfo^.TypeInfo,SizeOf(Pointer));
    fieldInfo^.TypeInfo^ := ATypes[j];
  end;
  Result := typ;
  RawTypeInfoList.Add(Result);
end;

procedure FreeRawTypeInfo(ARawTypeInfo : PTypeInfo);
var
  i : Cardinal;
  delphiFT : PFieldTable;
  tmp : PByte;
  fieldInfo : PFieldInfo;
begin
  if Assigned(ARawTypeInfo) then begin
    tmp := PByte(ARawTypeInfo);
    Inc(tmp,SizeOf(TTypeKind)); // Kind
    Inc(tmp,1 + Byte(ARawTypeInfo^.Name[0])); // Name

    delphiFT := PFieldTable(tmp);
    for i := 1 to delphiFT^.Count do begin
      fieldInfo := @(delphiFT^.Fields[(i - 1)]);
      FreeMem(fieldInfo^.TypeInfo);
      fieldInfo^.TypeInfo := nil;
    end;
    FreeMem(ARawTypeInfo);
  end;
end;

function MakeRecordTypeInfo(ARawTypeInfo : PTypeInfo) : PRecordTypeData;
var
  i, bufferSize, count : LongInt;
  delphiFT : PFieldTable;
  resBuffer : PRecordTypeData;
  fieldInfo : PRecordFieldInfo;
  fld : PFieldInfo;
  tmp : PByte;
begin
  tmp := PByte(ARawTypeInfo);
  Inc(tmp);
  Inc(tmp,1 + Byte(ARawTypeInfo^.Name[0]));
  delphiFT := PFieldTable(tmp);
  count := delphiFT^.Count;
  {calc buffer size}
  bufferSize :=
    SizeOf(shortstring) + // Name : shortstring;
    SizeOf(PtrUInt)     + // Size : PtrUInt;
    SizeOf(PtrUInt)     + // FieldCount: PtrUInt;
    ( count * SizeOf(TRecordFieldInfo) ); // Fields: array [0..0] of TRecordFieldInfo;
  GetMem(resBuffer,bufferSize);
  FillChar(Pointer(resBuffer)^,bufferSize,#0);
  resBuffer^.Name := PTypeInfo(ARawTypeInfo)^.Name;
  resBuffer^.RecordSize := delphiFT^.Size;
  resBuffer^.FieldCount := count;
  { Process elements }
  for i := 1 to Count do begin
    fld := @(delphiFT^.Fields[(i - 1)]);
    fieldInfo := @(resBuffer^.Fields[(i - 1)]);
    fieldInfo^.TypeInfo := fld^.TypeInfo;
    fieldInfo^.Offset := fld^.Offset;
    fieldInfo^.Visible := True;
  end;
  Result := resBuffer;
end;
{$ENDIF WST_RECORD_RTTI}

{$IFDEF FPC_XXXXXX}
function aligntoptr(p : pointer) : pointer;inline;
   begin
{$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
     result:=align(p,sizeof(p));
{$else FPC_REQUIRES_PROPER_ALIGNMENT}
     result:=p;
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
   end;

function MakeRecordTypeInfo(ARawTypeInfo : PTypeInfo) : PRecordTypeData;
{
  A record is designed as follows :
    1    : tkrecord
    2    : Length of name string (n);
    3    : name string;
    3+n  : record size;
    7+n  : number of elements (N)
    11+n : N times : Pointer to type info
                     Offset in record
}
var
  Temp : pbyte;
  namelen : byte;
  count,
  offset,
  i : longint;
  //info : pointer;

  resBuffer : PRecordTypeData;
  typName : shortstring;
  typSize : Cardinal;
  bufferSize : PtrUInt;
  fieldInfo : PRecordFieldInfo;
begin
  Temp := PByte(ARawTypeInfo);
  Inc(Temp);
  { Skip Name }
  namelen := Temp^;
  SetLength(typName,namelen);
  Inc(temp,1);
  Move(Temp^,typName[1],namelen);
  Inc(temp,namelen);
  temp:=aligntoptr(temp);
  { Skip size }
  typSize := PLongint(Temp)^;
  Inc(Temp,4);
  { Element count }
  Count := PLongint(Temp)^;
  Inc(Temp,sizeof(Count));

  {calc buffer size}
  bufferSize :=
    SizeOf(shortstring) + // Name : shortstring;
    SizeOf(PtrUInt)     + // Size : PtrUInt;
    SizeOf(PtrUInt)     + // FieldCount: PtrUInt;
    ( Count * SizeOf(TRecordFieldInfo) ); // Fields: array [0..0] of TRecordFieldInfo;

  GetMem(resBuffer,bufferSize);
  FillChar(Pointer(resBuffer)^,bufferSize,#0);
  resBuffer^.Name := typName;
  resBuffer^.RecordSize := typSize;
  resBuffer^.FieldCount := count;
  { Process elements }
  for i := 1 to Count do begin
    fieldInfo := @(resBuffer^.Fields[(i - 1)]);
    //Info := PPointer(Temp)^;
    fieldInfo^.TypeInfo := PPTypeInfo(Temp);
    Inc(Temp,sizeof(Pointer));
    Offset := PLongint(Temp)^;
    fieldInfo^.Offset := Offset;
    fieldInfo^.Visible := True;
    Inc(Temp,sizeof(Offset));
  end;
  Result := resBuffer;
end;
{$ENDIF FPC}

procedure FreeRecordTypeInfo(ATypeInfo : PRecordTypeData);
begin
  if ( ATypeInfo <> nil ) then
    FreeMem(ATypeInfo);
end;

{ TRecordRttiDataObject }

constructor TRecordRttiDataObject.Create(
  const AData : PRecordTypeData;
  const AFieldList : string
);
var
  locData : PRecordTypeData;
  i : Integer;
  ls, s : string;
begin
  locData := AData;
  inherited Create(locData);
  ls := Trim(AFieldList);
  s := '';
  i := 0;
  while ( i < locData^.FieldCount ) do begin
    s := GetToken(ls,';');
    if IsStrEmpty(s) then
      Break;
    locData^.Fields[i].Name := s;
    Inc(i);
  end;
end;

destructor TRecordRttiDataObject.Destroy();
begin
  FreeRecordTypeInfo(Data);
  inherited Destroy();
end;

function TRecordRttiDataObject.GetRecordTypeData() : PRecordTypeData;
begin
  Result := PRecordTypeData(Data);
end;

function TRecordRttiDataObject.FindField(const AFieldName : shortstring) : PRecordFieldInfo;
var
  i : Integer;
  locData : PRecordTypeData;
  locField : shortstring;
begin
  Result := nil;
  locData := PRecordTypeData(Data);
  locField := UpperCase(AFieldName);
  for i := 0 to Pred(locData^.FieldCount) do begin
    if ( locField = UpperCase(locData^.Fields[i].Name) ) then begin
      Result := @(locData^.Fields[i]);
      Break;
    end;
  end;
end;

function TRecordRttiDataObject.GetField(const AFieldName : shortstring) : PRecordFieldInfo;
begin
  Result := FindField(AFieldName);
  if ( Result = nil ) then
    raise Exception.CreateFmt(SERR_IsNotAFieldOf,[AFieldName,GetRecordTypeData()^.Name]);
end;

procedure initialize_record_rtti();
begin
{$IFDEF WST_RECORD_RTTI}
  if ( RawTypeInfoList = nil ) then
    RawTypeInfoList := TList.Create();
{$ENDIF WST_RECORD_RTTI}
end;

procedure finalize_record_rtti();
begin
{$IFDEF WST_RECORD_RTTI}
  if ( RawTypeInfoList <> nil ) then begin
    while ( RawTypeInfoList.Count > 0 ) do begin
      FreeRawTypeInfo(PTypeInfo(RawTypeInfoList.Items[0]));
      RawTypeInfoList.Delete(0);
    end;
    FreeAndNil(RawTypeInfoList);
  end;
{$ENDIF WST_RECORD_RTTI}
end;

initialization
  initialize_record_rtti();
  
finalization
  finalize_record_rtti();
  
end.
