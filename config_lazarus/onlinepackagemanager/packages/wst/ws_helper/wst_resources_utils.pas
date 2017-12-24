{
    This file is part of the Web Service Toolkit
    Copyright (c) 2006-2014 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$INCLUDE wst_global.inc}
unit wst_resources_utils;

interface

uses
  Classes, SysUtils; 

  procedure BinToWstRessource(
    const AResourceName : string;
          ABinStream,
          AWstRstream   : TStream
  );

implementation

procedure BinToWstRessource(
  const AResourceName : string;
        ABinStream,
        AWstRstream   : TStream
);
const MAX_LINE_LEN = 80; READ_LEN = 1024; WRITE_LEN = 1024;
type TWritingState = ( wsBegin, wsInString, wsOutString );
var
  locInBuffer, locOutBuffer : string;
  locInBufferLen, locOutBufferLen, locLineLen, locInIdx : Integer;
  locChar : Char;
  locState : TWritingState;
  locTotalRead : Integer;

  procedure FillInBuffer();
  begin
    locInIdx := 1;
    SetLength(locInBuffer,READ_LEN);
    locInBufferLen := ABinStream.Read(Pointer(locInBuffer)^,READ_LEN);
    SetLength(locInBuffer,locInBufferLen);
    Inc(locTotalRead,locInBufferLen);
  end;

  procedure FlushBuffer();
  begin
    locOutBufferLen := Length(locOutBuffer);
    if ( locOutBufferLen > 0 ) then begin
      AWstRstream.Write(Pointer(locOutBuffer)^,locOutBufferLen);
      locOutBuffer := '';
      locOutBufferLen := 0;
    end;
  end;

  function ReadChar():Boolean;
  begin
    if ( locInBufferLen = 0 ) or ( locInIdx > locInBufferLen ) then
      FillInBuffer();
    Result := ( locInBufferLen > 0 ) and ( locInIdx <= locInBufferLen );
    if Result then begin
      locChar := locInBuffer[locInIdx];
      Inc(locInIdx);
    end;
  end;

  procedure WriteChar(const AChar : Char);
  begin
    locOutBuffer := locOutBuffer + AChar;
    Inc(locLineLen);
  end;

var
  s : string;
begin
  locTotalRead := 0;
  locLineLen := 0;
  locInBufferLen := 0;
  locOutBufferLen := 0;
  locInIdx := 0;
  locState := wsBegin;
  locInBuffer := '';
  locOutBuffer := Format('  GetWSTResourceManager().AddResource(''%s'','+sLineBreak + '    ',[AResourceName]);
  while ReadChar() do begin
    if ( Ord(locChar) in [32..127] ) then begin
      case locState of
        wsBegin      : WriteChar('''');
        wsInString   : ;
        wsOutString  : WriteChar('''');
      end;
      WriteChar(locChar);
      if ( locChar = '''' ) then
        WriteChar(locChar);
      locState := wsInString;
    end else begin
      case locState of
        wsBegin      : ;
        wsInString   : WriteChar('''');
        wsOutString  : ;
      end;
      WriteChar('#');
      s := IntToStr(Ord(locChar));
      locOutBuffer := locOutBuffer + s;
      Inc(locLineLen,Length(s));
      locState := wsOutString;
    end;
    if ( ( locLineLen + 4 ) >= MAX_LINE_LEN ) then begin
      if ( locState = wsInString ) then
        WriteChar('''');
      locOutBuffer := locOutBuffer + sLineBreak + '    +';
      locLineLen := 0;
      locState := wsBegin;
    end;
    locOutBufferLen := Length(locOutBuffer);
    if ( locOutBufferLen >= WRITE_LEN ) then begin
      FlushBuffer();
    end;
  end;
  if ( locInBufferLen = 0 ) then begin
    locState := wsInString;
    WriteChar('''');
  end;
  if ( locState = wsInString ) then
    WriteChar('''');
  if ( locLineLen > 0 ) then
    locOutBuffer := locOutBuffer + sLineBreak;
  locOutBuffer := locOutBuffer + '  );';
  FlushBuffer();
end;

end.

