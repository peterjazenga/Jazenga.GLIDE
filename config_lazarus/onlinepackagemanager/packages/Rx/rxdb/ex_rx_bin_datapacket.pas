{ ex_rx_bin_datapacket unit

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


{
    TBinaryRxDatapacketReader implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit ex_rx_bin_datapacket;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dom, db, ex_rx_datapacket;

type
  TChangeLogEntry = record
       UpdateKind : TUpdateKind;
       OrigEntry  : integer;
       NewEntry   : integer;
  end;
  TChangeLogEntryArr = array of TChangeLogEntry;

type
  { TBinaryRxDatapacketReader }

  TBinaryRxDatapacketReader = class(TRxDataPacketReader)
  public
    procedure LoadFieldDefs(AFieldDefs : TFieldDefs); override;
    procedure StoreFieldDefs(AFieldDefs : TFieldDefs); override;
    function GetRecordRowState(out AUpdOrder : Integer) : TRowState; override;
    procedure FinalizeStoreRecords; override;
    function GetCurrentRecord : boolean; override;
    procedure GotoNextRecord; override;
    procedure InitLoadRecords; override;
    procedure RestoreRecord(ADataset : TDataset); override;
    procedure StoreRecord(ADataset : TDataset; ARowState : TRowState; AUpdOrder : integer = 0); override;
    class function RecognizeStream(AStream : TStream) : boolean; override;
  end;

implementation

uses
  dbconst;

{ TBinaryRxDatapacketReader }

const
  RxBinaryIdent = 'BinRxDataset';

procedure TBinaryRxDatapacketReader.LoadFieldDefs(AFieldDefs: TFieldDefs);
var
  FldCount : word;
  i : integer;
begin

  if not RecognizeStream(Stream) then
    DatabaseError(SStreamNotRecognised);

  FldCount:=Stream.ReadWord;
  AFieldDefs.Clear;
  for i := 0 to FldCount -1 do with TFieldDef.create(AFieldDefs) do
    begin
    Name := Stream.ReadAnsiString;
    Displayname := Stream.ReadAnsiString;
    Size := Stream.ReadWord;
    DataType := TFieldType(Stream.ReadWord);

    if Stream.ReadByte = 1 then
      Attributes := Attributes + [faReadonly];
    end;

end;

procedure TBinaryRxDatapacketReader.StoreFieldDefs(AFieldDefs: TFieldDefs);
var i : integer;
begin
  Stream.Write(RxBinaryIdent[1],length(RxBinaryIdent));

  Stream.WriteWord(AFieldDefs.Count);
  for i := 0 to AFieldDefs.Count -1 do with AFieldDefs[i] do
    begin
    Stream.WriteAnsiString(Name);
    Stream.WriteAnsiString(DisplayName);
    Stream.WriteWord(size);
    Stream.WriteWord(ord(DataType));

    if faReadonly in Attributes then
      Stream.WriteByte(1)
    else
      Stream.WriteByte(0);
    end;

end;

function TBinaryRxDatapacketReader.GetRecordRowState(out AUpdOrder: Integer
  ): TRowState;
var Buf : byte;
begin
  Buf := 0;
  AUpdOrder := 0;

  Stream.Read(Buf,1);
  Result := ByteToRowState(Buf);
  if Result<>[] then
    Stream.ReadBuffer(AUpdOrder,sizeof(integer));

end;

procedure TBinaryRxDatapacketReader.FinalizeStoreRecords;
begin
//  Do nothing
end;

function TBinaryRxDatapacketReader.GetCurrentRecord: boolean;
var
  Buf : byte;
begin
  Buf := 0;
  Result := (Stream.Read(Buf,1)=1) and (Buf=$fe);
end;

procedure TBinaryRxDatapacketReader.GotoNextRecord;
begin
//  Do Nothing
end;

procedure TBinaryRxDatapacketReader.InitLoadRecords;
begin
//  Do Nothing
end;

procedure TBinaryRxDatapacketReader.RestoreRecord(ADataset: TDataset);
begin
  Stream.ReadBuffer(ADataset.ActiveBuffer^,ADataset.RecordSize);
end;

procedure TBinaryRxDatapacketReader.StoreRecord(ADataset: TDataset;
  ARowState: TRowState; AUpdOrder: integer);
begin
  Stream.WriteByte($fe);
  Stream.WriteByte(RowStateToByte(ARowState));
  if ARowState<>[] then
    Stream.WriteBuffer(AUpdOrder,sizeof(integer));
  Stream.WriteBuffer(ADataset.ActiveBuffer^,ADataset.RecordSize);
end;

class function TBinaryRxDatapacketReader.RecognizeStream(AStream: TStream
  ): boolean;
var s        : string;
    len      : integer;
begin
  Len := length(RxBinaryIdent);
  setlength(s,len);
  if (AStream.Read (s[1],len) = len)
  and (s=RxBinaryIdent) then
    Result := True
  else
    Result := False;

end;

initialization
  RegisterDatapacketReader(TBinaryRxDatapacketReader,dfBinary);
end.

