{
    This file is part of the Web Service Toolkit
    Copyright (c) 2008 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).
    

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$INCLUDE wst_global.inc}
unit indy_tcp_protocol;

interface

uses
  Classes, SysUtils,
  service_intf, imp_utils, base_service_intf, client_utils,
  IdTCPClient;

{.$DEFINE WST_DBG}

Const
  sTRANSPORT_NAME = 'TCP';

Type

  ETCPException = class(EServiceException)
  End;

  { TTCPTransport }
  TTCPTransport = class(TBaseTCPTransport,ITransport)
  Private
    FConnection : TIdTCPClient;
    FAddress : string;
    FPort : string;
    FDefaultTimeOut: Integer;
  private
    procedure Connect();
  protected
    procedure DoSend(const AData; const ALength : Int64); override;
    function DoReceive(var AData; const ALength : Int64) : Int64; override;
  public
    constructor Create();override;
    destructor Destroy();override;
    function GetTransportName() : string; override;
  Published
    property Address : string Read FAddress Write FAddress;
    property Port : string read FPort write FPort;
    property DefaultTimeOut : Integer read FDefaultTimeOut write FDefaultTimeOut;
  End;

  procedure INDY_RegisterTCP_Transport();

implementation
uses
  binary_streamer, wst_types,
  IdGlobal;

{ TTCPTransport }

procedure TTCPTransport.Connect();
var
  locReconnect : Boolean;
begin
  if not FConnection.Connected() then begin
    FConnection.ReadTimeout := DefaultTimeOut;
    FConnection.Connect(Address,StrToInt(Port));
  end else begin
    locReconnect := False;
    try
      locReconnect := not FConnection.Socket.Binding.Readable(0);
    except
      locReconnect := True;
    end;
    if locReconnect then begin
      FConnection.Disconnect();
      FConnection.ReadTimeout := DefaultTimeOut;
      FConnection.Connect(Address,StrToInt(Port));
    end;
  end;
end;

constructor TTCPTransport.Create();
begin
  inherited;
  FConnection := TIdTCPClient.Create(nil);
  //FConnection.ReadTimeout:=;
  FDefaultTimeOut := 90000;
end;

destructor TTCPTransport.Destroy();
begin
  FreeAndNil(FConnection);
  inherited Destroy();
end;

function TTCPTransport.DoReceive(var AData; const ALength: Int64): Int64;
const
  BUFFER_LEN = 8 * 1024;
var
  locBuffer : TIdBytes;
  p : PByte;
  k : Integer;
  len : Integer;
begin
  Result := 0;
  if (ALength=0) then
    exit;

  p := PByte(@AData);
  len := ALength;
  repeat
    if (len > BUFFER_LEN) then
      k := BUFFER_LEN
    else
      k := len;
    FConnection.IOHandler.ReadBytes(locBuffer,k,False);
    Move(locBuffer[0],p^,k);
    Inc(P,k);
    Dec(len,k);
  until (len=0);
  Result := ALength;
end;

procedure TTCPTransport.DoSend(const AData; const ALength: Int64);
const
  BUFFER_LEN = 8 * 1024;
var
  locBuffer : TIdBytes;
  p : PByte;
  k : Integer;
  len : Integer;
begin
  if (ALength < 1) then
    exit;

  Connect();
  SetLength(locBuffer,BUFFER_LEN);
  p := PByte(@AData);
  len := ALength;
  repeat
    if (len > BUFFER_LEN) then
      k := BUFFER_LEN
    else
      k := len;
    Move(p^,locBuffer[0],k);
    FConnection.IOHandler.Write(locBuffer,k);
    Inc(P,k);
    Dec(len,k);
  until (len=0);
end;

function TTCPTransport.GetTransportName() : string;
begin
  Result := sTRANSPORT_NAME;
end;

procedure INDY_RegisterTCP_Transport();
begin
  GetTransportRegistry().Register(sTRANSPORT_NAME,TSimpleItemFactory.Create(TTCPTransport));
end;

end.
