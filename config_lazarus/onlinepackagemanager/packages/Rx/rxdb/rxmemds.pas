{ rxmemds unit

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

unit rxmemds;


{$I rx.inc}

interface


uses SysUtils, Classes, DB, ex_rx_datapacket, bufdataset_parser;

{ TRxMemoryData }

type
  TMemBlobData = string;
  TMemBlobArray = array[0..256] of TMemBlobData;
  PMemBlobArray = ^TMemBlobArray;
  TMemoryRecord = class;
  TLoadMode = (lmCopy, lmAppend);
  TCompareRecords = function (Item1, Item2: TMemoryRecord): Integer of object;

  TRxMemoryData = class(TDataSet)
  private
    FRecordPos: Integer;
    FRecordSize: Integer;
    FBookmarkOfs: Integer;
    FBlobOfs: Integer;
    FRecBufSize: Integer;
    FOffsets: PWordArray;
    FLastID: Integer;
    FAutoInc: Longint;
    FActive: Boolean;
    FRecords: TList;
    FIndexList: TList;
    FCaseInsensitiveSort: Boolean;
    FDescendingSort: Boolean;

    FFileName: string;
    FFileStream     : TFileStream;
    FDatasetReader  : TRxDataPacketReader;
    FPacketRecords: Integer;
    FFilterBuffer   : pchar;
    FNullmaskSize   : byte;
    FBRecordCount   : integer;
    FParser         : TBufDatasetParser;
    function  IntAllocRecordBuffer: PChar;
    procedure IntLoadFielddefsFromFile;
    procedure IntLoadRecordsFromFile;
    procedure SetPacketRecords(const AValue: Integer);

    function AddRecord: TMemoryRecord;
    procedure CopyRecord(RecordData, Buffer: PChar);
    function InsertRecord(Index: Integer): TMemoryRecord;
    function FindRecordID(ID: Integer): TMemoryRecord;
    procedure CreateIndexList(const FieldNames: string);
    procedure FreeIndexList;
    procedure QuickSort(L, R: Integer; Compare: TCompareRecords);
    procedure Sort;
    function CalcRecordSize: Integer;
    function FindFieldData(Buffer: Pointer; Field: TField): Pointer;overload;
    function FindFieldData(Buffer: Pointer; FieldNo:Integer): Pointer;overload;
    function GetMemoryRecord(Index: Integer): TMemoryRecord;
    function GetCapacity: Integer;
    function RecordFilter: Boolean;
    procedure SetCapacity(Value: Integer);
    procedure ClearRecords;
    procedure InitBufferPointers(GetProps: Boolean);
    procedure ParseFilter(const AFilter: string);
  protected
    procedure AssignMemoryRecord(Rec: TMemoryRecord; Buffer: PChar);
    function GetActiveRecBuf(var RecBuf: PChar): Boolean; virtual;
    procedure InitFieldDefsFromFields;
    procedure RecordToBuffer(Rec: TMemoryRecord; Buffer: PChar);
    procedure SetMemoryRecordData(Buffer: PChar; Pos: Integer); virtual;
    procedure SetAutoIncFields(Buffer: PChar); virtual;
    function CompareRecords(Item1, Item2: TMemoryRecord): Integer; virtual;
    function GetBlobData(Field: TField; Buffer: PChar): TMemBlobData;
    procedure SetBlobData(Field: TField; Buffer: PChar; Value: TMemBlobData);
    function AllocRecordBuffer: PChar; override;
    procedure FreeRecordBuffer(var Buffer: PChar); override;
    function BCDToCurr(BCD: Pointer; var Curr: Currency): Boolean;
    function CurrToBCD(const Curr: Currency; BCD: Pointer;
      Precision, Decimals: Integer): Boolean;
    procedure InternalInitRecord(Buffer: PChar); override;
    procedure ClearCalcFields(Buffer: PChar); override;
    function GetRecord(Buffer: PChar; GetMode: TGetMode;
      DoCheck: Boolean): TGetResult; override;
    function GetRecordSize: Word; override;
    procedure SetFiltered(Value: Boolean); override;
    procedure SetOnFilterRecord(const Value: TFilterRecordEvent); override;
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;
    procedure CloseBlob(Field: TField); override;
    procedure GetBookmarkData(Buffer: PChar; Data: Pointer); override;
    function GetBookmarkFlag(Buffer: PChar): TBookmarkFlag; override;
    {$IFDEF NoAutomatedBookmark}
    procedure InternalGotoBookmark(ABookmark: TBookmark); override;
    {$ELSE}
    procedure InternalGotoBookmark(ABookmark: Pointer); override;
    {$ENDIF}
    procedure InternalSetToRecord(Buffer: PChar); override;
    procedure SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag); override;
    procedure SetBookmarkData(Buffer: PChar; Data: Pointer); override;
    function GetIsIndexField(Field: TField): Boolean; override;
    procedure InternalFirst; override;
    procedure InternalLast; override;
    procedure InitRecord(Buffer: PChar); override;
    procedure InternalAddRecord(Buffer: Pointer; DoAppend: Boolean); override;
    procedure InternalDelete; override;
    procedure InternalPost; override;
    procedure InternalClose; override;
    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalOpen; override;
    procedure OpenCursor(InfoQuery: Boolean); override;
    function IsCursorOpen: Boolean; override;
    function GetRecordCount: Integer; override;
    function GetRecNo: Integer; override;
    procedure SetRecNo(Value: Integer); override;
    property Records[Index: Integer]: TMemoryRecord read GetMemoryRecord;
    function GetAnyRecField(SrcRecNo:integer; AField:TField):variant;
    procedure SetFilterText(const Value: String); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function BookmarkValid(ABookmark: TBookmark): Boolean; override;
    function CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer; override;
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    function GetCurrentRecord(Buffer: PChar): Boolean; override;
    function IsSequenced: Boolean; override;
    function Locate(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions): Boolean; override;
    procedure SortOnFields(const FieldNames: string;
      CaseInsensitive: Boolean = True; Descending: Boolean = False);
    procedure SortOnFieldsEx(const FieldNames: string;
      CaseInsensitive: Boolean = True; Asc: array of boolean);
    procedure EmptyTable;
    procedure CloseOpen;
    procedure CopyStructure(Source: TDataSet);
    function LoadFromDataSet(Source: TDataSet; ARecordCount: Integer;
      Mode: TLoadMode): Integer;
    function SaveToDataSet(Dest: TDataSet; ARecordCount: Integer): Integer;
    procedure AppendRecord(const Values: array of const);

    procedure SetDatasetPacket(AReader : TRxDataPacketReader);
    procedure GetDatasetPacket(AWriter : TRxDataPacketReader);
    procedure LoadFromStream(AStream : TStream; Format: TRxDataPacketFormat = dfBinary);
    procedure SaveToStream(AStream : TStream; Format: TRxDataPacketFormat = dfBinary);
    procedure LoadFromFile(AFileName: string = ''; Format: TRxDataPacketFormat = dfAny);
    procedure SaveToFile(AFileName: string = ''; Format: TRxDataPacketFormat = dfAny);
  published
    property Capacity: Integer read GetCapacity write SetCapacity default 0;
    property Active;
    property AutoCalcFields;
    property Filtered;
    property FieldDefs;
//    property ObjectView default False;
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnNewRecord;
    property OnPostError;

    property FileName : string read FFileName write FFileName;
    property PacketRecords : Integer read FPacketRecords write SetPacketRecords default 10;
  end;

{ TMemBlobStream }

  TMemBlobStream = class(TStream)
  private
    FField: TBlobField;
    FDataSet: TRxMemoryData;
    FBuffer: PChar;
    FMode: TBlobStreamMode;
    FOpened: Boolean;
    FModified: Boolean;
    FPosition: Longint;
    FCached: Boolean;
    function GetBlobSize: Longint;
    function GetBlobFromRecord(Field: TField): TMemBlobData;
  public
    constructor Create(Field: TBlobField; Mode: TBlobStreamMode);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    procedure Truncate;
  end;

{ TMemoryRecord }

  TMemoryRecord = class(TPersistent)
  private
    FMemoryData: TRxMemoryData;
    FID: Integer;
    FData: Pointer;
    FBlobs: PMemBlobArray;
    function GetIndex: Integer;
    procedure SetMemoryData(Value: TRxMemoryData; UpdateParent: Boolean);
  protected
    procedure SetIndex(Value: Integer); virtual;
  public
    constructor Create(MemoryData: TRxMemoryData); virtual;
    constructor CreateEx(MemoryData: TRxMemoryData; UpdateParent: Boolean); virtual;
    destructor Destroy; override;
    property MemoryData: TRxMemoryData read FMemoryData;
    property ID: Integer read FID write FID;
    property Index: Integer read GetIndex write SetIndex;
    property Data: Pointer read FData;
  end;


implementation


uses CustApp, rxdconst, LazUTF8, rxdbutils, dbconst, Variants, math, LResources;

const
  ftBlobTypes = [ftBlob, ftMemo, ftGraphic, ftFmtMemo, ftParadoxOle,
    ftDBaseOle, ftTypedBinary, ftOraBlob, ftOraClob];

  ftSupported = [ftString, ftSmallint, ftInteger, ftWord, ftBoolean, ftFloat,
    ftCurrency, ftDate, ftTime, ftDateTime, ftAutoInc, ftBCD, ftBytes,
    ftVarBytes, ftADT, ftFixedChar, ftWideString, ftLargeint, ftVariant, ftGuid] +
    ftBlobTypes;

  fkStoredFields = [fkData];

  GuidSize = 38;

{ Utility routines }

procedure FinalizeBlobFields(BlobArray:PMemBlobArray; BlobFieldCount:integer);
var
  i:integer;
begin
  for i:=0 to BlobFieldCount-1 do
    BlobArray^[i]:='';
end;

function CompareFields(Data1, Data2: Pointer; FieldType: TFieldType;
  CaseInsensitive: Boolean): Integer;
begin
  Result := 0;
  case FieldType of
    ftString:
      if CaseInsensitive then
        Result := UTF8CompareText(PChar(Data1), PChar(Data2))
      else
        Result := UTF8CompareStr(PChar(Data1), PChar(Data2));
    ftSmallint:
      if SmallInt(Data1^) > SmallInt(Data2^) then Result := 1
      else if SmallInt(Data1^) < SmallInt(Data2^) then Result := -1;
    ftInteger, ftDate, ftTime, ftAutoInc:
      if Longint(Data1^) > Longint(Data2^) then Result := 1
      else if Longint(Data1^) < Longint(Data2^) then Result := -1;
    ftWord:
      if Word(Data1^) > Word(Data2^) then Result := 1
      else if Word(Data1^) < Word(Data2^) then Result := -1;
    ftBoolean:
      if WordBool(Data1^) and not WordBool(Data2^) then Result := 1
      else if not WordBool(Data1^) and WordBool(Data2^) then Result := -1;
    ftFloat, ftCurrency:
      if Double(Data1^) > Double(Data2^) then Result := 1
      else if Double(Data1^) < Double(Data2^) then Result := -1;
    ftDateTime:
      if TDateTime(Data1^) > TDateTime(Data2^) then Result := 1
      else if TDateTime(Data1^) < TDateTime(Data2^) then Result := -1;
    ftFixedChar:
      if CaseInsensitive then
        Result := UTF8CompareText(PChar(Data1), PChar(Data2))
      else
        Result := UTF8CompareStr(PChar(Data1), PChar(Data2));
    ftWideString:
      if CaseInsensitive then
        Result := UTF8CompareText(WideCharToString(PWideChar(Data1)),
          WideCharToString(PWideChar(Data2)))
      else
        Result := UTF8CompareStr(WideCharToString(PWideChar(Data1)),
          WideCharToString(PWideChar(Data2)));
    ftLargeint: 
      if Int64(Data1^) > Int64(Data2^) then Result := 1
      else if Int64(Data1^) < Int64(Data2^) then Result := -1;
    ftVariant:
      Result := 0;
    ftGuid:
      Result := UTF8CompareText(PChar(Data1), PChar(Data2));
  end;
end;

function CalcFieldLen(FieldType: TFieldType; Size: Word): Word;
begin
  if not (FieldType in ftSupported) then
    Result := 0
  else
  if (FieldType in ftBlobTypes) then
    Result := SizeOf(Longint)
  else
  begin
    Result := Size;
    case FieldType of
      ftString: Inc(Result);
      ftSmallint: Result := SizeOf(SmallInt);
      ftInteger: Result := SizeOf(Longint);
      ftWord: Result := SizeOf(Word);
      ftBoolean: Result := SizeOf(WordBool);
      ftFloat: Result := SizeOf(Double);
      ftCurrency: Result := SizeOf(Double);
      ftBCD: Result := 34;
      ftDate, ftTime: Result := SizeOf(Longint);
      ftDateTime: Result := SizeOf(TDateTime);
      ftBytes: Result := Size;
      ftVarBytes: Result := Size + 2;
      ftAutoInc: Result := SizeOf(Longint);
      ftADT: Result := 0;
      ftFixedChar: Inc(Result);
      ftWideString: Result := (Result + 1) * 2;
      ftLargeint: Result := SizeOf(Int64);
      ftVariant: Result := SizeOf(Variant);
      ftGuid: Result := GuidSize + 1;
    end;
  end;
end;

procedure CalcDataSize(FieldDef: TFieldDef; var DataSize: Integer);
var
  I: Integer;
begin
  with FieldDef do begin
    if (DataType in ftSupported - ftBlobTypes) then
      Inc(DataSize, CalcFieldLen(DataType, Size) + 1);
{$IFDEF ENABLE_Child_Defs}
    for I := 0 to ChildDefs.Count - 1 do
      CalcDataSize(ChildDefs[I], DataSize);
{$ENDIF}
  end;
end;

procedure Error(const Msg: string);
begin
  DatabaseError(Msg);
end;

procedure ErrorFmt(const Msg: string; const Args: array of const);
begin
  DatabaseErrorFmt(Msg, Args);
end;

type
  TBookmarkData = Integer;
  PMemBookmarkInfo = ^TMemBookmarkInfo;
  TMemBookmarkInfo = packed record
    BookmarkData: TBookmarkData;
    BookmarkFlag: TBookmarkFlag;
  end;

{ TMemoryRecord }

constructor TMemoryRecord.Create(MemoryData: TRxMemoryData);
begin
  CreateEx(MemoryData, True);
end;

constructor TMemoryRecord.CreateEx(MemoryData: TRxMemoryData;
  UpdateParent: Boolean);
begin
  inherited Create;
  SetMemoryData(MemoryData, UpdateParent);
end;

destructor TMemoryRecord.Destroy;
begin
  SetMemoryData(nil, True);
  inherited Destroy;
end;

function TMemoryRecord.GetIndex: Integer;
begin
  if FMemoryData <> nil then Result := FMemoryData.FRecords.IndexOf(Self)
  else Result := -1;
end;

procedure TMemoryRecord.SetMemoryData(Value: TRxMemoryData; UpdateParent: Boolean);
var
  I: Integer;
  DataSize: Integer;
begin
  if FMemoryData <> Value then
  begin
    if FMemoryData <> nil then
    begin
      FMemoryData.FRecords.Remove(Self);
      if FMemoryData.BlobFieldCount > 0 then
      begin
        FinalizeBlobFields(FBlobs, FMemoryData.BlobFieldCount);
        Freemem(FBlobs, FMemoryData.BlobFieldCount * SizeOf(TMemBlobData));
      end;
      FBlobs:=nil;
      ReallocMem(FData, 0);
      FMemoryData := nil;
    end;
    if Value <> nil then
    begin
      if UpdateParent then
      begin
        Value.FRecords.Add(Self);
        Inc(Value.FLastID);
        FID := Value.FLastID;
      end;
      FMemoryData := Value;
      if Value.BlobFieldCount > 0 then
      begin
        GetMem(FBlobs, Value.BlobFieldCount * SizeOf(TMemBlobData));
        FillChar(FBlobs^, Value.BlobFieldCount * SizeOf(Pointer), 0);
        FinalizeBlobFields(FBlobs, Value.BlobFieldCount);
//        Initialize(PMemBlobArray(FBlobs)^[0]);//, Value.BlobFieldCount);
      end;
      DataSize := 0;
      for I := 0 to Value.FieldDefs.Count - 1 do
        CalcDataSize(Value.FieldDefs[I], DataSize);
      ReallocMem(FData, DataSize);
      FillChar(FData^, DataSize, 0);
    end;
  end;
end;

procedure TMemoryRecord.SetIndex(Value: Integer);
var
  CurIndex: Integer;
begin
  CurIndex := GetIndex;
  if (CurIndex >= 0) and (CurIndex <> Value) then
    FMemoryData.FRecords.Move(CurIndex, Value);
end;

{ TRxMemoryData }

constructor TRxMemoryData.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FParser := nil;
  FRecordPos := -1;
  FLastID := Low(Integer);
  FAutoInc := 1;
  FRecords := TList.Create;
end;

destructor TRxMemoryData.Destroy;
begin
  inherited Destroy;
  FreeIndexList;
  ClearRecords;
  FRecords.Free;
  ReallocMem(FOffsets, 0);
  if Assigned(FParser) then FreeAndNil(FParser);
end;

{ Records Management }

function TRxMemoryData.GetCapacity: Integer;
begin
  if FRecords <> nil then Result := FRecords.Capacity
  else Result := 0;
end;

procedure TRxMemoryData.SetCapacity(Value: Integer);
begin
  if FRecords <> nil then FRecords.Capacity := Value;
end;

function TRxMemoryData.AddRecord: TMemoryRecord;
begin
  Result := TMemoryRecord.Create(Self);
end;

function TRxMemoryData.FindRecordID(ID: Integer): TMemoryRecord;
var
  I: Integer;
begin
  for I := 0 to FRecords.Count - 1 do begin
    Result := TMemoryRecord(FRecords[I]);
    if Result.ID = ID then Exit;
  end;
  Result := nil;
end;

function TRxMemoryData.InsertRecord(Index: Integer): TMemoryRecord;
begin
  Result := AddRecord;
  Result.Index := Index;
end;

function TRxMemoryData.GetMemoryRecord(Index: Integer): TMemoryRecord;
begin
  Result := TMemoryRecord(FRecords[Index]);
end;

{ Field Management }

function TRxMemoryData.BCDToCurr(BCD: Pointer; var Curr: Currency): Boolean;
begin
  Move(BCD^, Curr, SizeOf(Currency));
  Result := True;
end;

function TRxMemoryData.CurrToBCD(const Curr: Currency; BCD: Pointer; Precision,
  Decimals: Integer): Boolean;
begin
  Move(Curr, BCD^, SizeOf(Currency));
  Result := True;
end;

procedure TRxMemoryData.InitFieldDefsFromFields;
var
  I: Integer;
  Offset: Word;
  FD:TFieldDef;
begin
  if FieldDefs.Count = 0 then
  begin
    for I := 0 to FieldCount - 1 do
    begin
      with Fields[I] do
        if (FieldKind in fkStoredFields) and not (DataType in ftSupported) then
          ErrorFmt(SUnknownFieldType, [DisplayName]);
    end;
    FreeIndexList;
  end;
  Offset := 0;
  { Create FieldDefs from persistent fields if needed }
  if FieldDefs.Count = 0 then
    for I := 0 to FieldCount - 1 do
    begin
      FD:=FieldDefs.AddFieldDef;
//      FD.DisplayName:=Fields[I].DisplayName;
      FD.Name:=Fields[I].FieldName;
      FD.Size:=Fields[I].Size;
      FD.DataType:=Fields[I].DataType;
      if Fields[I].Required then
         FD.Attributes:= FD.Attributes + [faRequired];
      if Fields[I] is TFloatField then
        FD.Precision:=TFloatField(Fields[I]).Precision;
    end;
  { Calculate fields offsets }
  ReallocMem(FOffsets, FieldDefs.Count * SizeOf(Word));
  for I := 0 to FieldDefs.Count - 1 do
  begin
    FOffsets^[I] := Offset;
    with FieldDefs[I] do
    begin
      if (DataType in ftSupported - ftBlobTypes) then
        Inc(Offset, CalcFieldLen(DataType, Size) + 1);
    end;
  end;
end;

function TRxMemoryData.FindFieldData(Buffer: Pointer; Field: TField): Pointer;
var
  Index: Integer;
begin
{.$IFDEF TEST_RXMDS}
  Index := FieldDefs.IndexOf(Field.FieldName);
//
  if Index <> Field.FieldNo - 1 then
     raise exception.Create('Index <> Field.FieldNo - 1');
{.$ENDIF}
//  Index := Field.FieldNo - 1;
//
  Result:=FindFieldData(Buffer, Index);
end;

function TRxMemoryData.FindFieldData(Buffer: Pointer; FieldNo: Integer): Pointer;
begin
  Result := nil;
  if (FieldNo >= 0) and (Buffer <> nil) and (FieldDefs[FieldNo].DataType in ftSupported - ftBlobTypes) then
    Result := Pointer(PtrInt(PChar(Buffer)) + FOffsets^[FieldNo]);
end;

{ Buffer Manipulation }

function TRxMemoryData.CalcRecordSize: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FieldDefs.Count - 1 do
    CalcDataSize(FieldDefs[I], Result);
end;

procedure TRxMemoryData.InitBufferPointers(GetProps: Boolean);
begin
  if GetProps then FRecordSize := CalcRecordSize;
  FBookmarkOfs := FRecordSize + CalcFieldsSize;
  FBlobOfs := FBookmarkOfs + SizeOf(TMemBookmarkInfo);
  FRecBufSize := FBlobOfs + BlobFieldCount * SizeOf(TMemBlobData);//Pointer);
end;

procedure TRxMemoryData.ParseFilter(const AFilter: string);
begin
  // parser created?
  if Length(AFilter) > 0 then
  begin
    if (FParser = nil) and IsCursorOpen then
    begin
      FParser := TBufDatasetParser.Create(Self);
    end;
    // is there a parser now?
    if FParser <> nil then
    begin
      // set options
      FParser.PartialMatch := not (foNoPartialCompare in FilterOptions);
      FParser.CaseInsensitive := foCaseInsensitive in FilterOptions;
      // parse expression
      FParser.ParseExpression(AFilter);
    end;
  end;
end;

procedure TRxMemoryData.ClearRecords;
begin
  while FRecords.Count > 0 do TObject(FRecords.Last).Free;
  FLastID := Low(Integer);
  FRecordPos := -1;
end;

function TRxMemoryData.AllocRecordBuffer: PChar;
begin
  Result := StrAlloc(FRecBufSize);
  InternalInitRecord(Result);
{  FillChar(Result^, FRecBufSize, 0);
  if BlobFieldCount > 0 then
  begin
//    Initialize(PMemBlobArray(Result + FBlobOfs)^[0]);//, BlobFieldCount);
//    FillChar(PMemBlobArray(Result + FBlobOfs)^, BlobFieldCount * SizeOf(Pointer),0);//, BlobFieldCount);
    FinalizeBlobFields(PMemBlobArray(Result + FBlobOfs), BlobFieldCount);

  end;}
end;

procedure TRxMemoryData.FreeRecordBuffer(var Buffer: PChar);
var
  n:integer;
  FieldPtr:PChar;
begin
  //correctly release field memory for complex types
  for n:=0 to FieldDefs.Count-1 do
    if FieldDefs.Items[n].DataType = ftVariant then
    begin
      FieldPtr:=FindFieldData(Buffer, n);
      if FieldPtr <> nil then
      begin
        PBoolean(FieldPtr)^:=False;
        Inc(FieldPtr);
        Finalize( PVariant(FieldPtr)^ );
      end;
    end;

  if BlobFieldCount > 0 then
    FinalizeBlobFields(PMemBlobArray(Buffer + FBlobOfs), BlobFieldCount);

  StrDispose(Buffer);
  Buffer := nil;
end;

procedure TRxMemoryData.ClearCalcFields(Buffer: PChar);
begin
  FillChar(Buffer[FRecordSize], CalcFieldsSize, 0);
end;

procedure TRxMemoryData.InternalInitRecord(Buffer: PChar);
var
  I: Integer;
begin
  FillChar(Buffer^, FBlobOfs, 0);
  FillChar(PByteArray(Buffer + FBlobOfs)^, BlobFieldCount * SizeOf(Pointer), 0);
  for I := 0 to BlobFieldCount - 1 do
  begin
    PMemBlobArray(Buffer + FBlobOfs)^[I] := '';
  end;
end;

procedure TRxMemoryData.InitRecord(Buffer: PChar);
begin
  inherited InitRecord(Buffer);
  with PMemBookmarkInfo(Buffer + FBookmarkOfs)^ do
  begin
    BookmarkData := Low(Integer);
    BookmarkFlag := bfInserted;
  end;
end;

procedure TRxMemoryData.CopyRecord(RecordData, Buffer:PChar);
var
  n, FieldSize:Integer;
  FieldPtr, BufPtr:PChar;
  DataType:TFieldType;
begin
  for n:=0 to FieldDefs.Count-1 do
  begin
    FieldPtr:=FindFieldData(RecordData, n);
    BufPtr:=FindFieldData(Buffer, n);
    if FieldPtr = nil then Continue;

    PBoolean(BufPtr)^:=PBoolean(FieldPtr)^;
    Inc(FieldPtr);
    Inc(BufPtr);

    DataType:=FieldDefs.Items[n].DataType;
    if DataType = ftVariant then
    begin
      PVariant(BufPtr)^:=PVariant(FieldPtr)^;
    end
    else
    begin
      FieldSize:=FieldDefs.Items[n].Size;
      Move( FieldPtr^, BufPtr^, CalcFieldLen(DataType, FieldSize) );
    end;
  end;
end;

function TRxMemoryData.GetCurrentRecord(Buffer: PChar): Boolean;
begin
  Result := False;
  if not IsEmpty and (GetBookmarkFlag(ActiveBuffer) = bfCurrent) then
  begin
    UpdateCursorPos;
    if (FRecordPos >= 0) and (FRecordPos < RecordCount) then
    begin
      //Move(Records[FRecordPos].Data^, Buffer^, FRecordSize);
      CopyRecord(Records[FRecordPos].Data, Buffer);

      Result := True;
    end;
  end;
end;

procedure TRxMemoryData.RecordToBuffer(Rec: TMemoryRecord; Buffer: PChar);
var
  I: Integer;
begin
  //Move(Rec.Data^, Buffer^, FRecordSize);
  CopyRecord(Rec.Data, Buffer);
  with PMemBookmarkInfo(Buffer + FBookmarkOfs)^ do
  begin
    BookmarkData := Rec.ID;
    BookmarkFlag := bfCurrent;
  end;
  for I := 0 to BlobFieldCount - 1 do
    PMemBlobArray(Buffer + FBlobOfs)^[I] := PMemBlobArray(Rec.FBlobs)^[I];
  GetCalcFields(Buffer);
end;

function TRxMemoryData.GetRecord(Buffer: PChar; GetMode: TGetMode;
  DoCheck: Boolean): TGetResult;
var
  Accept: Boolean;
begin
  Result := grOk;
  Accept := True;
  case GetMode of
    gmPrior:
      if FRecordPos <= 0 then begin
        Result := grBOF;
        FRecordPos := -1;
      end
      else begin
        repeat
          Dec(FRecordPos);
          if Filtered then Accept := RecordFilter;
        until Accept or (FRecordPos < 0);
        if not Accept then begin
          Result := grBOF;
          FRecordPos := -1;
        end;
      end;
    gmCurrent:
      if (FRecordPos < 0) or (FRecordPos >= RecordCount) then
        Result := grError
      else if Filtered then begin
        if not RecordFilter then Result := grError;
      end;
    gmNext:
      if FRecordPos >= RecordCount - 1 then Result := grEOF
      else begin
        repeat
          Inc(FRecordPos);
          if Filtered then Accept := RecordFilter;
        until Accept or (FRecordPos > RecordCount - 1);
        if not Accept then begin
          Result := grEOF;
          FRecordPos := RecordCount - 1;
        end;
      end;
  end;
  if Result = grOk then RecordToBuffer(Records[FRecordPos], Buffer)
  else if (Result = grError) and DoCheck then Error(SMemNoRecords);
end;

function TRxMemoryData.GetRecordSize: Word;
begin
  Result := FRecordSize;
end;

function TRxMemoryData.GetActiveRecBuf(var RecBuf: PChar): Boolean;
begin
  case State of
    dsBrowse:
      if IsEmpty then RecBuf := nil
      else RecBuf := ActiveBuffer;
    dsEdit, dsInsert: RecBuf := ActiveBuffer;
    dsCalcFields: RecBuf := CalcBuffer;
    dsFilter: RecBuf := TempBuffer;
    else RecBuf := nil;
  end;
  Result := RecBuf <> nil;
end;

{$IFDEF FIX_BUG_FieldNo}
function GetFieldNo(DS:TDataSet; Field:TField):integer;
var
  i:integer;
begin
  for i:=0 to DS.FieldDefs.Count-1 do
    if DS.FieldDefs[i].Name = Field.FieldName then
    begin
      Result:=i+1;
      exit;
    end;
  Result:=0;
end;
{$ENDIF}

function TRxMemoryData.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
var
  RecBuf, Data: PChar;
  VarData: Variant;
begin
  Result := False;
  if not GetActiveRecBuf(RecBuf) then Exit;
{$IFDEF FIX_BUG_FieldNo}
  if GetFieldNo(Self, Field) > 0 then
{$ELSE}
  if Field.FieldNo > 0 then
{$ENDIF}
  begin
    Data := FindFieldData(RecBuf, Field);
    if Data <> nil then begin
      Result := Boolean(Data[0]);
      Inc(Data);
      if Field.DataType in [ftString, ftFixedChar, ftWideString, ftGuid] then
        Result := Result and (StrLen(Data) > 0);
      if Result and (Buffer <> nil) then
        if Field.DataType = ftVariant then
        begin
          VarData := PVariant(Data)^;
          PVariant(Buffer)^ := VarData;
        end
        else
          Move(Data^, Buffer^, CalcFieldLen(Field.DataType, Field.Size));
    end;
  end
  else
  begin
    if State in [dsBrowse, dsEdit, dsInsert, dsCalcFields] then
    begin
      Inc(RecBuf, FRecordSize + Field.Offset);
      Result := Boolean(RecBuf[0]);
      if Result and (Buffer <> nil) then
        Move(RecBuf[1], Buffer^, Field.DataSize);
    end;
  end;
end;

procedure TRxMemoryData.SetFieldData(Field: TField; Buffer: Pointer);
var
  RecBuf, Data: PChar;
  VarData: Variant;
  PBl:PBoolean;
begin
  if not (State in dsWriteModes) then ErrorFmt(SNotEditing, [Name]);
  GetActiveRecBuf(RecBuf);
  with Field do
  begin
{$IFDEF FIX_BUG_FieldNo}
    if GetFieldNo(Self, Field) > 0 then
{$ELSE}
    if Field.FieldNo > 0 then
{$ENDIF}
    begin
      if State in [dsCalcFields, dsFilter] then ErrorFmt(SNotEditing, [Name]);
      if ReadOnly and not (State in [dsSetKey, dsFilter]) then
        ErrorFmt(SFieldReadOnly, [DisplayName]);
      Validate(Buffer);
      if FieldKind <> fkInternalCalc then
      begin
        Data := FindFieldData(RecBuf, Field);
        if Data <> nil then
        begin
          if DataType = ftVariant then
          begin
            if (Buffer = nil) or VarIsNull(PVariant(Buffer)^) or VarIsEmpty(PVariant(Buffer)^) or
               VarIsEmptyParam(PVariant(Buffer)^) then
              FillChar(Data^, CalcFieldLen(DataType, Size), 0)
            else
            begin
              Boolean(Data[0]):=True;
              Inc(Data);
              PVariant(Data)^ := PVariant(Buffer)^;
            end;
          end
          else
          begin
            PBl:=Pointer(Data);
//            Boolean(Data^{[0]}) := Assigned(Buffer);//LongBool(Buffer);
//            Pbl^:=Assigned(Buffer);
            PBoolean(Pointer(Data))^:= Assigned(Buffer);
            Inc(Data);
            if Assigned(Buffer) then
              Move(Buffer^, Data^, CalcFieldLen(DataType, Size))
            else
              FillChar(Data^, CalcFieldLen(DataType, Size), 0);
          end;
        end;
      end;
    end else {fkCalculated, fkLookup}
    begin
      Inc(RecBuf, FRecordSize + Offset);
      Boolean(RecBuf[0]) := LongBool(Buffer);
      if Boolean(RecBuf[0]) then Move(Buffer^, RecBuf[1], DataSize);
    end;
    if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
      DataEvent(deFieldChange, ptrint(Field));
  end;
end;

{ Filter }

procedure TRxMemoryData.SetFiltered(Value: Boolean);
begin
  if Active then
  begin
    CheckBrowseMode;
    if Filtered <> Value then
    begin
      inherited SetFiltered(Value);
      First;
    end;
  end
  else
    inherited SetFiltered(Value);
end;

procedure TRxMemoryData.SetOnFilterRecord(const Value: TFilterRecordEvent);
begin
  if Active then
  begin
    CheckBrowseMode;
    inherited SetOnFilterRecord(Value);
    if Filtered then First;
  end
  else
  inherited SetOnFilterRecord(Value);
end;

function TRxMemoryData.RecordFilter: Boolean;
var
  SaveState: TDataSetState;
  RecBuf: PChar;
begin
  Result := True;
  if Assigned(OnFilterRecord) then
  begin
    if (FRecordPos >= 0) and (FRecordPos < RecordCount) then
    begin
      SaveState := SetTempState(dsFilter);
      try
        RecordToBuffer(Records[FRecordPos], TempBuffer);
        OnFilterRecord(Self, Result);
      except
        CustomApplication.HandleException(Self);
      end;

      if Result and (Length(Filter) > 0) then
      begin
        if GetActiveRecBuf(RecBuf) then
          Result := Boolean((FParser.ExtractFromBuffer(RecBuf))^);
      end;

      RestoreState(SaveState);
    end
    else
      Result := False;
  end;
end;

{ Blobs }

function TRxMemoryData.GetBlobData(Field: TField; Buffer: PChar): TMemBlobData;
begin
  Result := PMemBlobArray(Buffer + FBlobOfs)^[Field.Offset];
end;

procedure TRxMemoryData.SetBlobData(Field: TField; Buffer: PChar;
  Value: TMemBlobData);
begin
  if (Buffer = ActiveBuffer) then begin
    if State = dsFilter then Error(SNotEditing);
    PMemBlobArray(Buffer + FBlobOfs)^[Field.Offset] := Value;
  end;
end;

procedure TRxMemoryData.CloseBlob(Field: TField);
begin
  if (FRecordPos >= 0) and (FRecordPos < FRecords.Count) and
    (State = dsEdit) then
    PMemBlobArray(ActiveBuffer + FBlobOfs)^[Field.Offset] :=
      PMemBlobArray(Records[FRecordPos].FBlobs)^[Field.Offset]
  else PMemBlobArray(ActiveBuffer + FBlobOfs)^[Field.Offset] := '';
end;

function TRxMemoryData.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
begin
  Result := TMemBlobStream.Create(Field as TBlobField, Mode);
end;

{ Bookmarks }

function TRxMemoryData.BookmarkValid(ABookmark: TBookmark): Boolean;
begin
  {$IFDEF NoAutomatedBookmark}
  Result := FActive and (TBookmarkData(ABookmark^) > Low(Integer)) and
    (TBookmarkData(ABookmark^) <= FLastID);
  {$ELSE}
  Result := FActive and (TBookmarkData(pointer(ABookmark)^) > Low(Integer)) and
    (TBookmarkData(pointer(ABookmark)^) <= FLastID);
  {$ENDIF}
end;

function TRxMemoryData.CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer;
begin
  if (Bookmark1 = nil) and (Bookmark2 = nil) then Result := 0
  else
  if (Bookmark1 <> nil) and (Bookmark2 = nil) then Result := 1
  else
  if (Bookmark1 = nil) and (Bookmark2 <> nil) then Result := -1
  {$IFDEF NoAutomatedBookmark}
  else
  if TBookmarkData(Bookmark1^) > TBookmarkData(Bookmark2^) then
  {$ELSE}
  else
  if TBookmarkData(pointer(Bookmark1)^) > TBookmarkData(pointer(Bookmark2)^) then
  {$ENDIF}
    Result := 1
  {$IFDEF NoAutomatedBookmark}
  else
  if TBookmarkData(Bookmark1^) < TBookmarkData(Bookmark2^) then
  {$ELSE}
  else
  if TBookmarkData(pointer(Bookmark1)^) < TBookmarkData(pointer(Bookmark2)^) then
  {$ENDIF}
    Result := -1
  else Result := 0;
end;

procedure TRxMemoryData.GetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  Move(PMemBookmarkInfo(Buffer + FBookmarkOfs)^.BookmarkData, Data^,
    SizeOf(TBookmarkData));
end;

procedure TRxMemoryData.SetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  Move(Data^, PMemBookmarkInfo(Buffer + FBookmarkOfs)^.BookmarkData,
    SizeOf(TBookmarkData));
end;

function TRxMemoryData.GetBookmarkFlag(Buffer: PChar): TBookmarkFlag;
begin
  Result := PMemBookmarkInfo(Buffer + FBookmarkOfs)^.BookmarkFlag;
end;

procedure TRxMemoryData.SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag);
begin
  PMemBookmarkInfo(Buffer + FBookmarkOfs)^.BookmarkFlag := Value;
end;

{$IFDEF NoAutomatedBookmark}
procedure TRxMemoryData.InternalGotoBookmark(ABookmark: TBookmark);
{$ELSE}
procedure TRxMemoryData.InternalGotoBookmark(ABookmark: Pointer);
{$ENDIF}
var
  Rec: TMemoryRecord;
  SavePos: Integer;
  Accept: Boolean;
begin
  Rec := FindRecordID(TBookmarkData(ABookmark^));
  if Rec <> nil then
  begin
    Accept := True;
    SavePos := FRecordPos;
    try
      FRecordPos := Rec.Index;
      if Filtered then Accept := RecordFilter;
    finally
      if not Accept then FRecordPos := SavePos;
    end;
  end;
end;

{ Navigation }

procedure TRxMemoryData.InternalSetToRecord(Buffer: PChar);
begin
  InternalGotoBookmark(@PMemBookmarkInfo(Buffer + FBookmarkOfs)^.BookmarkData);
end;

procedure TRxMemoryData.InternalFirst;
begin
  FRecordPos := -1;
end;

procedure TRxMemoryData.InternalLast;
begin
  FRecordPos := FRecords.Count;
end;

{ Data Manipulation }

procedure TRxMemoryData.AssignMemoryRecord(Rec: TMemoryRecord; Buffer: PChar);
var
  I: Integer;
begin
  //Move(Buffer^, Rec.Data^, FRecordSize);
  CopyRecord(Buffer, PChar(Rec.Data));

  for I := 0 to BlobFieldCount - 1 do
    PMemBlobArray(Rec.FBlobs)^[I] := PMemBlobArray(Buffer + FBlobOfs)^[I];
end;

procedure TRxMemoryData.SetMemoryRecordData(Buffer: PChar; Pos: Integer);
var
  Rec: TMemoryRecord;
begin
  if State = dsFilter then Error(SNotEditing);
  Rec := Records[Pos];
  AssignMemoryRecord(Rec, Buffer);
end;

procedure TRxMemoryData.SetAutoIncFields(Buffer: PChar);
var
  I, Count: Integer;
  Data: PChar;
begin
  Count := 0;
  for I := 0 to FieldCount - 1 do
    if (Fields[I].FieldKind in fkStoredFields) and
      (Fields[I].DataType = ftAutoInc) then
    begin
      Data := FindFieldData(Buffer, Fields[I]);
      if Data <> nil then begin
        Boolean(Data[0]) := True;
        Inc(Data);
        Move(FAutoInc, Data^, SizeOf(Longint));
        Inc(Count);
      end;
    end;
  if Count > 0 then Inc(FAutoInc);
end;

procedure TRxMemoryData.InternalAddRecord(Buffer: Pointer; DoAppend: Boolean);
var
  RecPos: Integer;
  Rec: TMemoryRecord;
begin
  if DoAppend then
  begin
    Rec := AddRecord;
    FRecordPos := FRecords.Count - 1;
  end
  else
  begin
    if FRecordPos = -1 then
      RecPos := 0
    else
      RecPos := FRecordPos;
    Rec := InsertRecord(RecPos);
    FRecordPos := RecPos;
  end;
  SetAutoIncFields(Buffer);
  SetMemoryRecordData(Buffer, Rec.Index);
end;

procedure TRxMemoryData.InternalDelete;
var
  Accept: Boolean;
begin
  Records[FRecordPos].Free;
  if FRecordPos >= FRecords.Count then Dec(FRecordPos);
  Accept := True;
  repeat
    if Filtered then Accept := RecordFilter;
    if not Accept then Dec(FRecordPos);
  until Accept or (FRecordPos < 0);
  if FRecords.Count = 0 then FLastID := Low(Integer);
end;

procedure TRxMemoryData.InternalPost;
var
  RecPos: Integer;
begin
  if State = dsEdit then
    SetMemoryRecordData(ActiveBuffer, FRecordPos)
  else begin
    if State in [dsInsert] then SetAutoIncFields(ActiveBuffer);
    if FRecordPos >= FRecords.Count then begin
      SetMemoryRecordData(ActiveBuffer, AddRecord.Index);
      FRecordPos := FRecords.Count - 1;
    end
    else begin
      if FRecordPos = -1 then RecPos := 0
      else RecPos := FRecordPos;
      SetMemoryRecordData(ActiveBuffer, InsertRecord(RecPos).Index);
      FRecordPos := RecPos;
    end;
  end;
end;

procedure TRxMemoryData.OpenCursor(InfoQuery: Boolean);
begin
  if not InfoQuery then begin
    if FieldCount > 0 then FieldDefs.Clear;
    InitFieldDefsFromFields;
  end;
  FActive := True;
  inherited OpenCursor(InfoQuery);
end;

procedure TRxMemoryData.InternalOpen;
begin
  BookmarkSize := SizeOf(TBookmarkData);
  if DefaultFields then CreateFields;
  BindFields(True);
  InitBufferPointers(True);
  InternalFirst;
end;

procedure TRxMemoryData.InternalClose;
begin
  ClearRecords;
  FAutoInc := 1;
  BindFields(False);
  if DefaultFields then DestroyFields;
  FreeIndexList;
  FActive := False;
end;

procedure TRxMemoryData.InternalHandleException;
begin
  CustomApplication.HandleException(Self);
end;

procedure TRxMemoryData.InternalInitFieldDefs;
begin
end;

function TRxMemoryData.IsCursorOpen: Boolean;
begin
  Result := FActive;
end;

{ Informational }

function TRxMemoryData.GetRecordCount: Integer;
begin
  Result := FRecords.Count;
end;

function TRxMemoryData.GetRecNo: Integer;
begin
  CheckActive;
  UpdateCursorPos;
  if (FRecordPos = -1) and (RecordCount > 0) then Result := 1
  else Result := FRecordPos + 1;
end;

procedure TRxMemoryData.SetRecNo(Value: Integer);
begin
  if (Value > 0) and (Value <= FRecords.Count) then
  begin
    FRecordPos := Value - 1;
    Resync([]);
  end;
end;

function TRxMemoryData.GetAnyRecField(SrcRecNo: integer; AField: TField
  ): variant;
var
  Data1: PChar;
  I: Integer;
  Item:TMemoryRecord;
begin
  Item:=Records[SrcRecNo];
  Data1 := FindFieldData(Item.Data, AField);
  Inc(Data1);  //Skip null flag

  case AField.DataType of
    ftString:Result := PChar(Data1);
    ftSmallint:Result:=SmallInt(Data1^);
    ftInteger,
    ftDate,
    ftTime,
    ftAutoInc:Result:=Longint(Data1^);
    ftWord:Result:=Word(Data1^);
    ftBoolean:Result:=WordBool(Data1^);
    ftFloat, ftCurrency:Result:=PDouble(Data1)^;
    ftDateTime:Result:=PDateTime(Data1)^;
    ftFixedChar:Result:=PChar(Data1);
    ftWideString:Result:=PWideChar(Data1);
    ftLargeint:Result:=Int64(Data1^);
    ftVariant:
      begin
        Result := PVariant(Data1)^;
      end;
    ftGuid:Result:=PChar(Data1);
  else
    Result:=null;
  end;
end;

procedure TRxMemoryData.SetFilterText(const Value: String);
begin
  if Value = Filter then
    exit;
  // parse
  ParseFilter(Value);
  // call dataset method
  inherited;
  // refilter dataset if filtered
  if IsCursorOpen and Filtered then Resync([]);
end;

function TRxMemoryData.IsSequenced: Boolean;
begin
  Result := not Filtered;
end;

{ DataSet locate routines }
function DataSetLocateThrough(DataSet: TDataSet; const KeyFields: string;
  const KeyValues: Variant; Options: TLocateOptions): Boolean;
var
  FieldCount: Integer;
  Fields: TList;

  function CompareField(Field: TField; Value: Variant): Boolean;
  var
    S,S1: string;

  begin
    if Field.DataType = ftString then
    begin
      S := Field.AsString;
      S1:=Value;
      if (loPartialKey in Options) then
        Delete(S, Length(S1) + 1, MaxInt);

      if (loCaseInsensitive in Options) then
        Result := UTF8CompareText(S, S1) = 0
      else
        Result := UTF8CompareStr(S, S1) = 0;
    end
    else Result := (Field.Value = Value);
  end;

  function CompareRecord: Boolean;
  var
    I: Integer;
  begin
    if FieldCount = 1 then
      Result := CompareField(TField(Fields.First), KeyValues)
    else begin
      Result := True;
      for I := 0 to FieldCount - 1 do
        Result := Result and CompareField(TField(Fields[I]), KeyValues[I]);
    end;
  end;
var
  Bookmark: TBookmark;
begin
  Result := False;
  with DataSet do begin
    CheckBrowseMode;
    if BOF and EOF then Exit;
  end;
  Fields := TList.Create;
  try
    DataSet.GetFieldList(Fields, KeyFields);
    FieldCount := Fields.Count;
    Result := CompareRecord;
    if Result then Exit;
    DataSet.DisableControls;
    try
      Bookmark := DataSet.GetBookmark;
      try
        with DataSet do begin
          First;
          while not EOF do begin
            Result := CompareRecord;
            if Result then Break;
            Next;
          end;
        end;
      finally
{$IFDEF NoAutomatedBookmark}
        if not Result and DataSet.BookmarkValid(PChar(Bookmark)) then
{$ELSE}
        if not Result and DataSet.BookmarkValid(Bookmark) then
{$ENDIF}
          DataSet.GotoBookmark(Bookmark);
      end;
    finally
      DataSet.FreeBookmark(Bookmark);
      DataSet.EnableControls;
    end;
  finally
    Fields.Free;
  end;
end;

function TRxMemoryData.Locate(const KeyFields: string;
  const KeyValues: Variant; Options: TLocateOptions): Boolean;
begin
  DoBeforeScroll;
  Result := DataSetLocateThrough(Self, KeyFields, KeyValues, Options);
  if Result then
  begin
    DataEvent(deDataSetChange, 0);
    DoAfterScroll;
  end;
end;

{ Table Manipulation }

procedure TRxMemoryData.EmptyTable;
begin
  if Active then
  begin
    CheckBrowseMode;
    ClearRecords;
    ClearBuffers;
    DataEvent(deDataSetChange, 0);
  end;
end;

procedure TRxMemoryData.CloseOpen;
begin
  Close;
  Open;
end;

procedure TRxMemoryData.CopyStructure(Source: TDataSet);

  procedure CheckDataTypes(FieldDefs: TFieldDefs);
  var
    I: Integer;
  begin
    for I := FieldDefs.Count - 1 downto 0 do begin
      if not (FieldDefs.Items[I].DataType in ftSupported) then
        FieldDefs.Items[I].Free
{$IFDEF ENABLE_Child_Defs}
      else CheckDataTypes(FieldDefs[I].ChildDefs);
{$ENDIF}
    end;
  end;

var
  I: Integer;
begin
  CheckInactive;
  for I := FieldCount - 1 downto 0 do Fields[I].Free;
  if (Source = nil) then Exit;
  Source.FieldDefs.Update;
//  FieldDefs.Assign(Source.FieldDefs);
//  FieldDefs := Source.FieldDefs;
  FieldDefs.Clear;
  for i:=0 to Source.FieldDefs.Count-1 do
    FieldDefs.Add(Source.FieldDefs[i].Name, Source.FieldDefs[i].DataType, Source.FieldDefs[i].Size, Source.FieldDefs[i].Required);
      
  CheckDataTypes(FieldDefs);
  CreateFields;
end;
(*
procedure AssignRecord(Source, Dest: TDataSet; ByName: Boolean);
var
  I: Integer;
  F, FSrc: TField;
begin
//  if not (Dest.State in dsEditModes) then DBError(SNotEditing);
  if ByName then begin
    for I := 0 to Source.FieldCount - 1 do begin
      F := Dest.FindField(Source.Fields[I].FieldName);
      if F <> nil then begin
        if (F.DataType = Source.Fields[I].DataType) and
          (F.DataSize = Source.Fields[I].DataSize) then
          F.Assign(Source.Fields[I])
        else F.AsString := Source.Fields[I].AsString;
      end;
    end;
  end
  else begin
    for I := 0 to Min(Source.FieldDefs.Count - 1, Dest.FieldDefs.Count - 1) do
    begin
      F := Dest.FindField(Dest.FieldDefs[I].Name);
      FSrc := Source.FindField(Source.FieldDefs[I].Name);
      if (F <> nil) and (FSrc <> nil) then begin
        if F.DataType = FSrc.DataType then F.Assign(FSrc)
        else F.AsString := FSrc.AsString;
      end;
    end;
  end;
end;
*)
function TRxMemoryData.LoadFromDataSet(Source: TDataSet; ARecordCount: Integer;
  Mode: TLoadMode): Integer;
var
  SourceActive: Boolean;
  MovedCount: Integer;
begin
  Result := 0;
  if Source = Self then Exit;
  SourceActive := Source.Active;
  Source.DisableControls;
  try
    DisableControls;
    try
      Filtered := False;
      with Source do begin
        Open;
        CheckBrowseMode;
        UpdateCursorPos;
      end;
      if Mode = lmCopy then begin
        Close;
        CopyStructure(Source);
      end;
      FreeIndexList;
      if not Active then Open;
      Resync([]);
      CheckBrowseMode;
      if ARecordCount > 0 then MovedCount := ARecordCount
      else begin
        Source.First;
        MovedCount := MaxInt;
      end;
      try
        while not Source.EOF do
        begin
          Append;
          AssignRecord(Source, Self, True);
          Post;
          Inc(Result);
          if Result >= MovedCount then Break;
          Source.Next;
        end;
      finally
        First;
      end;
    finally
      EnableControls;
    end;
  finally
    if not SourceActive then Source.Close;
    Source.EnableControls;
  end;
end;

function TRxMemoryData.SaveToDataSet(Dest: TDataSet; ARecordCount: Integer): Integer;
var
  MovedCount: Integer;
begin
  Result := 0;
  if Dest = Self then Exit;
  CheckBrowseMode;
  UpdateCursorPos;
  Dest.DisableControls;
  try
    DisableControls;
    try
      if not Dest.Active then Dest.Open
      else Dest.CheckBrowseMode;
      if ARecordCount > 0 then MovedCount := ARecordCount
      else
      begin
        First;
        MovedCount := MaxInt;
      end;
      try
        while not EOF do begin
          Dest.Append;
          AssignRecord(Self, Dest, True);
          Dest.Post;
          Inc(Result);
          if Result >= MovedCount then Break;
          Next;
        end;
      finally
        Dest.First;
      end;
    finally
      EnableControls;
    end;
  finally
    Dest.EnableControls;
  end;
end;

procedure TRxMemoryData.AppendRecord(const Values: array of const);
var
  I: Integer;
begin
  if State <> dsInsert then
    Append;
  for I := 0 to High(Values) do Fields[I].AssignValue(Values[I]);
  Post;
end;

{ Index Related }

procedure TRxMemoryData.SortOnFields(const FieldNames: string;
  CaseInsensitive: Boolean = True; Descending: Boolean = False);
begin
  CreateIndexList(FieldNames);
  FCaseInsensitiveSort := CaseInsensitive;
  FDescendingSort := Descending;
  try
    Sort;
  except
    FreeIndexList;
    raise;
  end;
end;

procedure TRxMemoryData.SortOnFieldsEx(const FieldNames: string;
  CaseInsensitive: Boolean; Asc: array of boolean);
begin

end;

procedure TRxMemoryData.Sort;
var
  Pos: TBookmark;
begin
  if Active and (FRecords <> nil) and (FRecords.Count > 0) then
  begin
    Pos := GetBookmark;
    try
      QuickSort(0, FRecords.Count - 1, @CompareRecords);
      SetBufListSize(0);
      InitBufferPointers(False);
      try
        RecalcBufListSize;
//        SetBufListSize(BufferCount + 1);
      except
        SetState(dsInactive);
        CloseCursor;
        raise;
      end;
    finally
      GotoBookmark(Pos);
      FreeBookmark(Pos);
    end;
    Resync([]);
  end;
end;

procedure TRxMemoryData.QuickSort(L, R: Integer; Compare: TCompareRecords);
var
  I, J: Integer;
  P: TMemoryRecord;
begin
  repeat
    I := L;
    J := R;
    P := Records[(L + R) shr 1];
    repeat
      while Compare(Records[I], P) < 0 do Inc(I);
      while Compare(Records[J], P) > 0 do Dec(J);
      if I <= J then begin
        FRecords.Exchange(I, J);
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSort(L, J, Compare);
    L := I;
  until I >= R;
end;

function TRxMemoryData.CompareRecords(Item1, Item2: TMemoryRecord): Integer;
var
  Data1, Data2: PChar;
  F: TField;
  I: Integer;
begin
  Result := 0;
  if FIndexList <> nil then begin
    for I := 0 to FIndexList.Count - 1 do begin
      F := TField(FIndexList[I]);
      Data1 := FindFieldData(Item1.Data, F);
      if Data1 <> nil then begin
        Data2 := FindFieldData(Item2.Data, F);
        if Data2 <> nil then begin
          if Boolean(Data1[0]) and Boolean(Data2[0]) then begin
            Inc(Data1);
            Inc(Data2);
            Result := CompareFields(Data1, Data2, F.DataType,
              FCaseInsensitiveSort);
          end
          else if Boolean(Data1[0]) then Result := 1
          else if Boolean(Data2[0]) then Result := -1;
          if FDescendingSort then Result := -Result;
        end;
      end;
      if Result <> 0 then Exit;
    end;
  end;
  if (Result = 0) then begin
    if Item1.ID > Item2.ID then Result := 1
    else if Item1.ID < Item2.ID then Result := -1;
    if FDescendingSort then Result := -Result;
  end;
end;

function TRxMemoryData.GetIsIndexField(Field: TField): Boolean;
begin
  if FIndexList <> nil then
    Result := FIndexList.IndexOf(Field) >= 0
  else Result := False;
end;

procedure TRxMemoryData.CreateIndexList(const FieldNames: string);
var
  Pos: Integer;
  F: TField;
begin
  if FIndexList = nil then FIndexList := TList.Create
  else FIndexList.Clear;
  Pos := 1;
  while Pos <= Length(FieldNames) do begin
    F := FieldByName(ExtractFieldName(FieldNames, Pos));
    if (F.FieldKind = fkData) and
      (F.DataType in ftSupported - ftBlobTypes) then
      FIndexList.Add(F)
    else ErrorFmt(SFieldTypeMismatch, [F.DisplayName]);
  end;
end;

procedure TRxMemoryData.FreeIndexList;
begin
  FIndexList.Free;
  FIndexList := nil;
end;

function TRxMemoryData.IntAllocRecordBuffer: PChar;
begin
  // do nothing
end;

procedure TRxMemoryData.IntLoadFielddefsFromFile;
begin
  FDatasetReader.LoadFielddefs(FieldDefs);
  if DefaultFields then CreateFields;
end;

procedure TRxMemoryData.IntLoadRecordsFromFile;
var StoreState      : TDataSetState;
    AddRecordBuffer : boolean;
    ARowState       : TRowState;
    AUpdOrder       : integer;

begin
  FDatasetReader.InitLoadRecords;
  StoreState:=SetTempState(dsFilter);

  while FDatasetReader.GetCurrentRecord do
    begin
    ARowState := FDatasetReader.GetRecordRowState(AUpdOrder);

      FDatasetReader.RestoreRecord(self);
      inc(FBRecordCount);

      FDatasetReader.GotoNextRecord;
    end;

  RestoreState(StoreState);

  if assigned(FFileStream) then
    begin
    FreeAndNil(FFileStream);
    FreeAndNil(FDatasetReader);
    end;

end;

procedure TRxMemoryData.SetPacketRecords(const AValue: Integer);
begin
  if FPacketRecords=AValue then exit;
  FPacketRecords:=AValue;
end;

procedure TRxMemoryData.SetDatasetPacket(AReader: TRxDataPacketReader);
var
    StoreDSState   : TDataSetState;
    ARowState       : TRowState;
    AUpdOrder       : integer;
begin
  FDatasetReader := AReader;

  DisableControls;
    try
      Filtered := False;

      Close;       // must be inactive to do IntLoadFielddefsFromFile
      // load fields defs
      IntLoadFielddefsFromFile;

      FreeIndexList;

      if not Active then Open;
      Resync([]);                   // clears buffers if empty dataset
      CheckBrowseMode;

      FDatasetReader.InitLoadRecords;

      try
        while FDatasetReader.GetCurrentRecord do
        begin
          Append;

          ARowState := FDatasetReader.GetRecordRowState(AUpdOrder);       // added for binary export

          FDatasetReader.RestoreRecord(TRxMemoryData(Self));

          Post;

          FDatasetReader.GotoNextRecord;
          inc(FBRecordCount);

        end;
      finally
        First;
      end;

    finally
      EnableControls;
    end;

    if assigned(FFileStream) then
    begin
      FreeAndNil(FFileStream);
      FreeAndNil(FDatasetReader);
    end;

end;

procedure TRxMemoryData.GetDatasetPacket(AWriter: TRxDataPacketReader);
var
   StoreDSState   : TDataSetState;
begin

  CheckBrowseMode;
  UpdateCursorPos;

  FDatasetReader := AWriter;
  try
    DisableControls;
    try

      FDatasetReader.StoreFieldDefs(FieldDefs);

      First;
      while not EOF do
      begin
        // ** NOTE ** had to cast self to TRxMemoryData just save current values
        // otherwise the as string value in ex_rx_datapacket would not write.
        FDatasetReader.StoreRecord(TRxMemoryData(Self),[]);
        Next;
      end;

      FDatasetReader.FinalizeStoreRecords;
    finally
      EnableControls;
    end;
  finally
    FDatasetReader := nil;
  end;
end;

procedure TRxMemoryData.LoadFromStream(AStream: TStream;
  Format: TRxDataPacketFormat);
var APacketReaderReg : TRxDatapacketReaderRegistration;
    APacketReader : TRxDataPacketReader;
begin
  if GetRegisterDatapacketReader(AStream,format,APacketReaderReg) then
    APacketReader := APacketReaderReg.ReaderClass.create(AStream)
  else
    DatabaseError(SStreamNotRecognised);
  try
    SetDatasetPacket(APacketReader);
  finally
    APacketReader.Free;
  end;
end;

procedure TRxMemoryData.SaveToStream(AStream: TStream;
  Format: TRxDataPacketFormat);
var APacketReaderReg : TRxDatapacketReaderRegistration;
    APacketWriter : TRxDataPacketReader;
begin
  if GetRegisterDatapacketReader(Nil,format,APacketReaderReg) then
    APacketWriter := APacketReaderReg.ReaderClass.create(AStream)
  else
    DatabaseError(SNoReaderClassRegistered);
  try
    GetDatasetPacket(APacketWriter);
  finally
    APacketWriter.Free;
  end;
end;

procedure TRxMemoryData.LoadFromFile(AFileName: string;
  Format: TRxDataPacketFormat);
var AFileStream : TFileStream;
begin
  if AFileName='' then AFileName := FFileName;
  AFileStream := TFileStream.Create(AFileName,fmOpenRead);
  try
    LoadFromStream(AFileStream, Format);
  finally
    AFileStream.Free;
  end;
end;

procedure TRxMemoryData.SaveToFile(AFileName: string;
  Format: TRxDataPacketFormat);
var AFileStream : TFileStream;
begin
  if AFileName='' then AFileName := FFileName;
  AFileStream := TFileStream.Create(AFileName,fmCreate);
  try
    SaveToStream(AFileStream, Format);
  finally
    AFileStream.Free;
  end;
end;

{ TMemBlobStream }

constructor TMemBlobStream.Create(Field: TBlobField; Mode: TBlobStreamMode);
begin
  FMode := Mode;
  FField := Field;
  FDataSet := FField.DataSet as TRxMemoryData;
  if not FDataSet.GetActiveRecBuf(FBuffer) then Exit;
  if not FField.Modified and (Mode <> bmRead) then begin
    if FField.ReadOnly then ErrorFmt(SFieldReadOnly, [FField.DisplayName]);
    if not (FDataSet.State in [dsEdit, dsInsert]) then Error(SNotEditing);
    FCached := True;
  end
  else FCached := (FBuffer = FDataSet.ActiveBuffer);
  FOpened := True;
  if Mode = bmWrite then Truncate;
end;

destructor TMemBlobStream.Destroy;
begin
  if FOpened and FModified then FField.Modified := True;
  if FModified then
  try
    FDataSet.DataEvent(deFieldChange, ptrint(FField));
  except
    CustomApplication.HandleException(Self);
//    Application.HandleException(Self);
  end;
end;

function TMemBlobStream.GetBlobFromRecord(Field: TField): TMemBlobData;
var
  Rec: TMemoryRecord;
  Pos: Integer;
begin
  Result := '';
  Pos := FDataSet.FRecordPos;
  if (Pos < 0) and (FDataSet.RecordCount > 0) then Pos := 0
  else if Pos >= FDataSet.RecordCount then Pos := FDataSet.RecordCount - 1;
  if (Pos >= 0) and (Pos < FDataSet.RecordCount) then begin
    Rec := FDataSet.Records[Pos];
    if Rec <> nil then 
      Result := PMemBlobArray(Rec.FBlobs)^[FField.Offset];
  end;
end;

function TMemBlobStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := 0;
  if FOpened then begin
    if Count > Size - FPosition then Result := Size - FPosition
    else Result := Count;
    if Result > 0 then begin
      if FCached then begin
        Move(PChar(FDataSet.GetBlobData(FField, FBuffer))[FPosition], Buffer,
          Result);
        Inc(FPosition, Result);
      end
      else begin
        Move(PChar(GetBlobFromRecord(FField))[FPosition], Buffer,
          Result);
        Inc(FPosition, Result);
      end;
    end;
  end;
end;

function TMemBlobStream.Write(const Buffer; Count: Longint): Longint;
var
  Temp: TMemBlobData;
begin
  Result := 0;
  if FOpened and FCached and (FMode <> bmRead) then begin
    Temp := FDataSet.GetBlobData(FField, FBuffer);
    if Length(Temp) < FPosition + Count then
      SetLength(Temp, FPosition + Count);
    Move(Buffer, PChar(Temp)[FPosition], Count);
    FDataSet.SetBlobData(FField, FBuffer, Temp);
    Inc(FPosition, Count);
    Result := Count;
    FModified := True;
  end;
end;

function TMemBlobStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  case Origin of
    0: FPosition := Offset;
    1: Inc(FPosition, Offset);
    2: FPosition := GetBlobSize + Offset;
  end;
  Result := FPosition;
end;

procedure TMemBlobStream.Truncate;
begin
  if FOpened and FCached and (FMode <> bmRead) then begin
    FDataSet.SetBlobData(FField, FBuffer, '');
    FModified := True;
  end;
end;

function TMemBlobStream.GetBlobSize: Longint;
begin
  Result := 0;
  if FOpened then
    if FCached then
      Result := Length(FDataSet.GetBlobData(FField, FBuffer))
    else
      Result := Length(GetBlobFromRecord(FField))
end;

initialization
  RegisterPropertyToSkip(TRxMemoryData, 'OnFilterRecordEx', 'Old property', '');
end.
