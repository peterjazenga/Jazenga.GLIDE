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
unit library_base_intf;

interface
uses SysUtils, Classes, base_service_intf, wst_types;

const
  RET_OK = 0;
  RET_FALSE = 1;
  WST_LIB_HANDLER = 'wstHandleRequest';

type

  EwstCheckException = class(EServiceException)
  private
    FReturnCode: Integer;
  public
    property ReturnCode : Integer read FReturnCode write FReturnCode;
  end;

  IwstStream = interface
    ['{95700F89-3E36-4678-AD84-347162E39288}']
    function Read(
            ABuffer    : Pointer;
      const ALenToRead : LongWord;
      out   AReadedLen : LongWord
    ):LongInt;
    function Write(
            ABuffer     : Pointer;
      const ALenToWrite : LongWord;
      out   AWrittenLen : LongWord
    ):LongInt;
    function GetSize(out ASize : LongWord):LongInt;
    function SetSize(const ANewSize : LongWord):LongInt;
    function GetPosition(out APos : LongWord):LongWord;
    function SetPosition(const ANewPos : LongWord):LongInt;
  end;

  { TwstStream }

  TwstStream = class(TInterfacedObject,IwstStream)
  private
    FStream : TStream;
  protected
    function Read(
            ABuffer    : Pointer;
      const ALenToRead : LongWord;
      out   AReadedLen : LongWord
    ):LongInt;
    function Write(
            ABuffer     : Pointer;
      const ALenToWrite : LongWord;
      out   AWrittenLen : LongWord
    ):LongInt;
    function GetSize(out ASize : LongWord):LongInt;
    function SetSize(const ANewSize : LongWord):LongInt;
    function GetPosition(out APos : LongWord):LongWord;
    function SetPosition(const ANewPos : LongWord):LongInt;
  public
    constructor Create(AStream : TStream);
  end;
  
  TwstLibraryHandlerFunction =
    function(
      ARequestBuffer : IwstStream;
      AErrorBuffer : Pointer;
      var AErrorBufferLen : LongInt
    ):LongInt;

  procedure wstCheck(const AReturn : LongInt);overload;
  procedure wstCheck(const AReturn : LongInt; const AMsg : string);overload;
  
implementation

procedure wstCheck(const AReturn : LongInt);
var
  e : EwstCheckException;
begin
  if ( AReturn <> RET_OK ) then begin
    e := EwstCheckException.CreateFmt('wst Check Exception , return = %d',[AReturn]);
    e.ReturnCode := AReturn;
    raise e;
  end;
end;

procedure wstCheck(const AReturn : LongInt; const AMsg : string);
var
  e : EwstCheckException;
begin
  if ( AReturn <> RET_OK ) then begin
    e := EwstCheckException.Create(AMsg);
    e.ReturnCode := AReturn;
    raise e;
  end;
end;

{ TwstStream }

function TwstStream.Read(
        ABuffer     : Pointer;
  const ALenToRead  : LongWord;
  out   AReadedLen  : LongWord
): LongInt;
begin
  try
    AReadedLen := FStream.Read(ABuffer^,ALenToRead);
    Result := RET_OK;
  except
    Result := RET_FALSE;
  end;
end;

function TwstStream.Write(
        ABuffer      : Pointer;
  const ALenToWrite  : LongWord;
  out   AWrittenLen  : LongWord
): LongInt;
begin
  try
    AWrittenLen := FStream.Write(ABuffer^,ALenToWrite);
    Result := RET_OK;
  except
    Result := RET_FALSE;
  end;
end;

function TwstStream.GetSize(out ASize: LongWord): LongInt;
begin
  ASize := FStream.Size;
  Result := RET_OK;
end;

function TwstStream.SetSize(const ANewSize: LongWord): LongInt;
begin
  FStream.Size := ANewSize;
  Result := RET_OK;
end;

function TwstStream.GetPosition(out APos: LongWord): LongWord;
begin
  APos := FStream.Position;
  Result := RET_OK;
end;

function TwstStream.SetPosition(const ANewPos: LongWord): LongInt;
begin
  FStream.Position := ANewPos;
  Result := RET_OK;
end;

constructor TwstStream.Create(AStream: TStream);
begin
  Assert(Assigned(AStream));
  FStream := AStream;
end;

end.

