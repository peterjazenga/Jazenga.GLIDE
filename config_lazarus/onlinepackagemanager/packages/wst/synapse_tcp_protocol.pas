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
unit synapse_tcp_protocol;
                                                   
interface

uses
  Classes, SysUtils,
  service_intf, base_service_intf, client_utils,
  blcksock, synsock;

//{$DEFINE WST_DBG}

Const
  sTRANSPORT_NAME = 'TCP';

Type

  ETCPException = class(EServiceException)
  End;
  
{$M+}
  { TTCPTransport }
  TTCPTransport = class(TBaseTCPTransport,ITransport)
  Private
    FConnection : TTCPBlockSocket;
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
    property Port : string Read FPort Write FPort;
    property DefaultTimeOut : Integer read FDefaultTimeOut write FDefaultTimeOut;
  End;
{$M+}

  procedure SYNAPSE_RegisterTCP_Transport();

implementation

{ TTCPTransport }

procedure TTCPTransport.Connect();
var
  locReconnect : Boolean;
begin
  if ( FConnection.Socket = INVALID_SOCKET ) then begin
    FConnection.Connect(Address,Port);
  end else begin
    locReconnect := False;
    try
      locReconnect := not FConnection.CanRead(0);
    except
      locReconnect := True;
    end;
    if locReconnect then begin
      FConnection.CloseSocket();
      FConnection.Connect(Address,Port);
    end;
  end;
end;

procedure TTCPTransport.DoSend(const AData; const ALength : Int64);
begin
  Connect();
  FConnection.SendBuffer(@AData,ALength);
end;

function TTCPTransport.DoReceive(var AData; const ALength : Int64) : Int64;
begin
  Result := FConnection.RecvBufferEx(@AData,ALength,DefaultTimeOut);
  FConnection.ExceptCheck();
end;

constructor TTCPTransport.Create();
begin
  inherited Create();
  FConnection := TTCPBlockSocket.Create();
  FConnection.RaiseExcept := True;
  FDefaultTimeOut := 90000;
end;

destructor TTCPTransport.Destroy();
begin
  FreeAndNil(FConnection);
  inherited Destroy();
end;

function TTCPTransport.GetTransportName() : string;  
begin
  Result := sTRANSPORT_NAME;  
end;

procedure SYNAPSE_RegisterTCP_Transport();
begin
  GetTransportRegistry().Register(sTRANSPORT_NAME,TSimpleItemFactory.Create(TTCPTransport));
end;

end.
