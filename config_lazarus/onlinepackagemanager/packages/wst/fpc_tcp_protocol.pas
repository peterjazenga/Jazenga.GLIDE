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
unit fpc_tcp_protocol;
                                                   
interface

uses
  Classes, SysUtils,
  service_intf, imp_utils, base_service_intf, client_utils,
  ssockets;

//{$DEFINE WST_DBG}

Const
  sTRANSPORT_NAME = 'TCP';

Type

  ETCPException = class(EServiceException);

{$M+}
  { TTCPTransport }
  TTCPTransport = class(TBaseTCPTransport,ITransport)
  Private
    FConnection : TInetSocket;
    FAddress : string;
    FPort : string;
  private
    procedure Connect();
  protected
    procedure DoSend(const AData; const ALength : Int64); override;
    function DoReceive(var AData; const ALength : Int64) : Int64; override;
  public
    destructor Destroy();override;
    function GetTransportName() : string; override;      
  Published
    property Address : string Read FAddress Write FAddress;
    property Port : string Read FPort Write FPort;
  End;

procedure FPC_RegisterTCP_Transport();

implementation

uses
  wst_consts, binary_streamer, Math, wst_types;

{ TTCPTransport }

procedure TTCPTransport.Connect();

begin
  if FConnection=Nil then
    FConnection:=TInetSocket.Create(FAddress,StrToInt(Port));
end;

procedure TTCPTransport.DoSend(const AData; const ALength : Int64);
var
  c, len : integer;
  P : PByte;
begin
  Connect();
  P := PByte(@AData);
  len := ALength;
  Repeat
    C:=FConnection.Write(P^,len);
    if (C<0) then
       Raise ETCPException.CreateFmt(SERR_ErrorSendindDataToSocket,[FConnection.LastError]);
    If (C>0) then
      begin
        inc(P,C);
        Dec(len,C);
      end;
  Until (len=0);
end;

function TTCPTransport.DoReceive(var AData; const ALength : Int64) : Int64;
Var
  P : PByte;
  C : integer;
  len : Int64;
begin
  if (ALength=0) then
    exit;
  len := ALength;
  P:=PByte(@AData);
  repeat
    C:=FConnection.Read(P^,len);
    If (C<=0) then
      Raise ETCPException.CreateFmt(SERR_ErrorReadindDataToSocket,[FConnection.LastError]);
    If (C>0) then
      begin
        Inc(P,C);
        Dec(len,C);
      end
  Until (len=0);
  Result := ALength;
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

procedure FPC_RegisterTCP_Transport();
begin
  GetTransportRegistry().Register(sTRANSPORT_NAME,TSimpleItemFactory.Create(TTCPTransport));
end;

end.
