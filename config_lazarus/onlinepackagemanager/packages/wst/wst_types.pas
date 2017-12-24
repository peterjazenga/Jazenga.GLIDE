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
unit wst_types;

interface
uses
  Types;

{$INCLUDE wst.inc}
{$INCLUDE wst_delphi.inc}

type

{$ifdef fpc}
  UInt32 = Cardinal;
{$endif fpc}

  { reprents an array of Byte }
{$IFDEF WST_UNICODESTRING}
  TBinaryString = {$IFDEF FPC}ansistring{$ELSE}RawByteString{$ENDIF};
{$ELSE WST_UNICODESTRING}
  TBinaryString = ansistring;
{$ENDIF}

  TByteDynArray = Types.TByteDynArray;

  { TDataObject }

  TDataObject = class
  private
    FData : Pointer;
  public
    constructor Create(const AData : Pointer);
    property Data : Pointer read FData write FData;
  end;
  
implementation

{ TDataObject }

constructor TDataObject.Create(const AData : Pointer);
begin
  FData := AData;
end;

end.

