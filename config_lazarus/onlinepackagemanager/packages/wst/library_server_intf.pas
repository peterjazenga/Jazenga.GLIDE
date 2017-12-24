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
unit library_server_intf;

interface

uses
  Classes, SysUtils,
  library_base_intf, wst_types;

  function wstHandleRequest(
    ARequestBuffer : IwstStream;
    AErrorBuffer : Pointer;
    var AErrorBufferLen : LongInt
  ):LongInt;

implementation
uses base_service_intf, server_service_intf, server_service_imputils, binary_streamer;

function wstHandleRequest(
  ARequestBuffer : IwstStream;
  AErrorBuffer : Pointer;
  var AErrorBufferLen : LongInt
):LongInt;

  procedure CopyErrMsg(const AMsg : string);
  var
    j,m : Integer;
  begin
    m := AErrorBufferLen;
    j := Length(AMsg) * SizeOf(Char);
    if ( j > 0 ) then begin
      if ( j > m ) then
        j := m;
      try
        Move(AMsg[1],AErrorBuffer^,j);
      except
      end;
    end;
  end;
  
Var
  trgt,ctntyp, frmt : TBinaryString;
  binBuff : TByteDynArray;
  rqst : IRequestBuffer;
  rdr : IDataStoreReader;
  inStream, bufStream : TMemoryStream;
  bs, bytesCount : LongWord;
begin
  Result := RET_FALSE;
  try
    inStream := nil;
    bufStream := nil;
    if Assigned(ARequestBuffer) then begin
      wstCheck(ARequestBuffer.GetSize(bs));
      if ( bs > 0 ) then begin
        try
          inStream := TMemoryStream.Create();
          bufStream := TMemoryStream.Create();

          bufStream.Size := bs;
          wstCheck(ARequestBuffer.SetPosition(0));
          wstCheck(ARequestBuffer.Read(bufStream.Memory,bs,bytesCount));
          if ( bs <> bytesCount ) then
            wstCheck(RET_FALSE,'Invalid buffer operation (READ)');
          wstCheck(ARequestBuffer.SetSize(0));

          bufStream.Position := 0;
          rdr := CreateBinaryReader(bufStream);
          if ( rdr.ReadInt32S() <> ( bs - 4 ) ) then
            wstCheck(RET_FALSE,'Invalid buffer.');
          trgt := rdr.ReadAnsiStr();
          ctntyp := rdr.ReadAnsiStr();
          frmt := rdr.ReadAnsiStr();
          binBuff := rdr.ReadBinary();
          rdr := nil;
          bufStream.Size := 0;
          bufStream.Position := 0;
          inStream.Write(binBuff[0],Length(binBuff));
          SetLength(binBuff,0);
          inStream.Position := 0;
          rqst := TRequestBuffer.Create(trgt,ctntyp,inStream,bufStream,frmt);
          HandleServiceRequest(rqst);
          bs := bufStream.Size;
          wstCheck(ARequestBuffer.SetSize(bs));
          wstCheck(ARequestBuffer.SetPosition(0));
          wstCheck(ARequestBuffer.Write(bufStream.Memory,bs,bytesCount));
          if ( bs <> bytesCount ) then
            wstCheck(RET_FALSE,'Invalid buffer operation (WRITE)');
          Result := RET_OK;
        finally
          bufStream.Free();
          inStream.Free();
        end;
      end;
    end;
  except
    on e : EwstCheckException do begin
      Result := e.ReturnCode;
      CopyErrMsg(e.Message);
    end;
    on e : Exception do begin
      Result := RET_FALSE;
      CopyErrMsg(e.Message);
    end else begin
      Result := RET_FALSE;
    end;
  end;
end;

end.
