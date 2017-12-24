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
unit library_protocol;

//{$DEFINE WST_DBG}

interface

uses
  Classes, SysUtils,
  service_intf, imp_utils, base_service_intf, library_base_intf,
  library_imp_utils, wst_types, client_utils;

const
  sTRANSPORT_NAME = 'LIB';

Type

{$M+}
  { TLIBTransport }
  TLIBTransport = class(TBaseTransport,ITransport)
  Private
    FModule : IwstModule;
    FHandler : TwstLibraryHandlerFunction;
  private
    FContentType: string;
    FFileName: string;
    FTarget: string;
    FFormat : string;
  private
    procedure SetFileName(const AValue: string);
    procedure LoadModule();
  public
    constructor Create();override;
    destructor Destroy();override;
    function GetTransportName() : string; override;
    procedure SendAndReceive(ARequest,AResponse:TStream); override;
  published
    property ContentType : string read FContentType write FContentType;
    property Target : string read FTarget write FTarget;
    property FileName : string read FFileName write SetFileName;
    property Format : string read FFormat write FFormat;
  end;
{$M+}

  procedure LIB_Register_Transport();

implementation
uses binary_streamer;

{ TLIBTransport }

procedure TLIBTransport.SetFileName(const AValue: string);
begin
  FFileName := AValue;
  if Assigned(FModule) and ( not AnsiSameStr(FFileName,FModule.GetFileName()) ) then begin
    FHandler := nil;
    FModule := nil;
  end;
end;

procedure TLIBTransport.LoadModule();
begin
  if ( FModule = nil ) then begin
    FModule := LibraryManager.Get(FFileName);
    FHandler := TwstLibraryHandlerFunction(FModule.GetProc(WST_LIB_HANDLER));
  end;
end;

constructor TLIBTransport.Create();
begin
  inherited Create();
  FModule := nil;
  FHandler := nil
end;

destructor TLIBTransport.Destroy();
begin
  FModule := nil;
  FHandler := nil;
  inherited Destroy();
end;

function TLIBTransport.GetTransportName() : string;
begin
  Result := sTRANSPORT_NAME;
end;

const MAX_ERR_LEN = 500;
procedure TLIBTransport.SendAndReceive(ARequest, AResponse: TStream);
Var
  wrtr : IDataStore;
  buffStream : TMemoryStream;
  buff : TByteDynArray;
  intfBuffer : IwstStream;
  bl : LongInt;
  errorStrBuffer : ansistring;
{$IFDEF WST_DBG}
  s : TBinaryString;
  i : Int64;
{$ENDIF WST_DBG}
begin
  LoadModule();
  buffStream := TMemoryStream.Create();
  try
    wrtr := CreateBinaryWriter(buffStream);
    wrtr.WriteInt32S(0);
    wrtr.WriteAnsiStr(Target);
    wrtr.WriteAnsiStr(ContentType);
    wrtr.WriteAnsiStr(Self.Format);
    SetLength(buff,ARequest.Size);
    ARequest.Position := 0;
    ARequest.Read(buff[0],Length(buff));
    wrtr.WriteBinary(buff);
    buffStream.Position := 0;
    wrtr.WriteInt32S(buffStream.Size-4);

    buffStream.Position := 0;
    intfBuffer := TwstStream.Create(buffStream);
    bl := MAX_ERR_LEN;
    errorStrBuffer := StringOfChar(#0,bl);
    if ( FHandler(intfBuffer,Pointer(errorStrBuffer),bl) <> RET_OK ) then
      raise Exception.Create(errorStrBuffer);

    buffStream.Position := 0;
    AResponse.Size := 0;
    AResponse.CopyFrom(buffStream,0);
    AResponse.Position := 0;
{$IFDEF WST_DBG}
    i := AResponse.Position;
    SetLength(s,AResponse.Size);
    AResponse.Read(s[1],AResponse.Size);
    AResponse.Position := i;
    if IsConsole then
      WriteLn(s);
{$ENDIF WST_DBG}
  finally
    buffStream.Free();
  end;
end;

procedure LIB_Register_Transport();
begin
  GetTransportRegistry().Register(sTRANSPORT_NAME,TSimpleItemFactory.Create(TLIBTransport));
end;

end.
