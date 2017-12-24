{ ex_rx_datapacket unit

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

unit ex_rx_datapacket;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,db;

type
  TRowStateValue = (rsvOriginal, rsvDeleted, rsvInserted, rsvUpdated, rsvDetailUpdates);
  TRowState = set of TRowStateValue;

type
  TRxDataPacketFormat = (dfBinary,dfXML,dfXMLUTF8,dfAny);

type

  { TRxDatapacketReader }

  TRxDatapacketReaderClass = class of TRxDatapacketReader;
  TRxDatapacketReader = class(TObject)
    FStream : TStream;
  protected
    class function RowStateToByte(const ARowState : TRowState) : byte;
    class function ByteToRowState(const AByte : Byte) : TRowState;
  public
    constructor create(AStream : TStream); virtual;
    // Load a dataset from stream:
    // Load the field-definitions from a stream.
    procedure LoadFieldDefs(AFieldDefs : TFieldDefs); virtual; abstract;
    // Is called before the records are loaded
    procedure InitLoadRecords; virtual; abstract;
    // Return the RowState of the current record, and the order of the update
    function GetRecordRowState(out AUpdOrder : Integer) : TRowState; virtual; abstract;
    // Returns if there is at least one more record available in the stream
    function GetCurrentRecord : boolean; virtual; abstract;
    // Store a record from stream in the current record-buffer
    procedure RestoreRecord(ADataset : TDataset); virtual; abstract;
    // Move the stream to the next record
    procedure GotoNextRecord; virtual; abstract;

    // Store a dataset to stream:
    // Save the field-definitions to a stream.
    procedure StoreFieldDefs(AFieldDefs : TFieldDefs); virtual; abstract;
    // Save a record from the current record-buffer to the stream
    procedure StoreRecord(ADataset : TDataset; ARowState : TRowState; AUpdOrder : integer = 0); virtual; abstract;
    // Is called after all records are stored
    procedure FinalizeStoreRecords; virtual; abstract;
    // Checks if the provided stream is of the right format for this class
    class function RecognizeStream(AStream : TStream) : boolean; virtual; abstract;
    property Stream: TStream read FStream;
  end;

type
  TRxDatapacketReaderRegistration = record
    ReaderClass : TRxDatapacketReaderClass;
    Format      : TRxDatapacketFormat;
  end;

function GetRegisterDatapacketReader(AStream : TStream; AFormat : TRxDatapacketFormat; var ADataReaderClass : TRxDatapacketReaderRegistration) : boolean;
procedure RegisterDatapacketReader(ADatapacketReaderClass : TRxDatapacketReaderClass; AFormat : TRxDatapacketFormat);

implementation

var
  RxRegisteredDatapacketReaders : Array of TRxDatapacketReaderRegistration;


function GetRegisterDatapacketReader(AStream: TStream;
  AFormat: TRxDatapacketFormat;
  var ADataReaderClass: TRxDatapacketReaderRegistration): boolean;
var i : integer;
begin
  Result := False;
  for i := 0 to length(RxRegisteredDatapacketReaders)-1 do if ((AFormat=dfAny) or (AFormat=RxRegisteredDatapacketReaders[i].Format)) then
    begin

      if (AStream <> nil) then
        AStream.Seek(0,soFromBeginning); // ensure at start of stream to check value

      if (AStream=nil) or (RxRegisteredDatapacketReaders[i].ReaderClass.RecognizeStream(AStream)) then
      begin
        ADataReaderClass := RxRegisteredDatapacketReaders[i];
        Result := True;
        if (AStream <> nil) then
          AStream.Seek(0,soFromBeginning);
        break;
      end;
    end;
end;

procedure RegisterDatapacketReader(
  ADatapacketReaderClass: TRxDatapacketReaderClass; AFormat: TRxDatapacketFormat
  );
begin
  setlength(RxRegisteredDatapacketReaders,length(RxRegisteredDatapacketReaders)+1);
  with RxRegisteredDatapacketReaders[length(RxRegisteredDatapacketReaders)-1] do
    begin
    Readerclass := ADatapacketReaderClass;
    Format      := AFormat;
    end;
end;

{ TRxDatapacketReader }

class function TRxDatapacketReader.RowStateToByte(const ARowState: TRowState
  ): byte;
var RowStateInt : Byte;
begin
  RowStateInt:=0;
  if rsvOriginal in ARowState then RowStateInt := RowStateInt+1;
  if rsvDeleted in ARowState then RowStateInt := RowStateInt+2;
  if rsvInserted in ARowState then RowStateInt := RowStateInt+4;
  if rsvUpdated in ARowState then RowStateInt := RowStateInt+8;
  Result := RowStateInt;
end;

class function TRxDatapacketReader.ByteToRowState(const AByte: Byte
  ): TRowState;
begin
  result := [];
  if (AByte and 1)=1 then Result := Result+[rsvOriginal];
  if (AByte and 2)=2 then Result := Result+[rsvDeleted];
  if (AByte and 4)=4 then Result := Result+[rsvInserted];
  if (AByte and 8)=8 then Result := Result+[rsvUpdated];
end;

constructor TRxDatapacketReader.create(AStream: TStream);
begin
  FStream := AStream;
end;

initialization
  setlength(RxRegisteredDatapacketReaders,0);
finalization
  setlength(RxRegisteredDatapacketReaders,0);
end.

