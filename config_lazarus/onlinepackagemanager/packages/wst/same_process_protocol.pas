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
unit same_process_protocol;

interface

uses
  Classes, SysUtils,
  service_intf, imp_utils,
  server_service_intf, server_service_imputils, base_service_intf, wst_types,
  client_utils;

Const
  sTRANSPORT_NAME = 'SAME_PROCESS';

Type

{$M+}

  { TInProcessTransport }

  TInProcessTransport = class(TBaseTransport,ITransport)
  Private
    FAddress: string;
    FContentType: string;
    FFormat : string;
  Public                             
    function GetTransportName() : string; override;    
    procedure SendAndReceive(ARequest,AResponse:TStream); override;
  Published
    property ContentType : string Read FContentType Write FContentType;
    property Address : string Read FAddress Write FAddress;
    property Format : string read FFormat write FFormat;
  End;
{$M+}

  procedure SAME_PROCESS_Register_Local_Transport();

implementation

{ TInProcessTransport }

function TInProcessTransport.GetTransportName() : string;  
begin
  Result := sTRANSPORT_NAME;  
end;

procedure TInProcessTransport.SendAndReceive(ARequest, AResponse: TStream);
Var
  bffr : IRequestBuffer;
{$IFDEF WST_DBG}
  s : TBinaryString;
  i : Int64;
{$ENDIF WST_DBG}
begin
  bffr := TRequestBuffer.Create(Address,ContentType,ARequest,AResponse,Format);
  HandleServiceRequest(bffr);
  {$IFDEF WST_DBG}
  i := AResponse.Position;
  SetLength(s,AResponse.Size);
  AResponse.Read(s[1],AResponse.Size);
  WriteLn(s);
  {$ENDIF WST_DBG}
end;

procedure SAME_PROCESS_Register_Local_Transport();
begin
  GetTransportRegistry().Register(sTRANSPORT_NAME,TSimpleItemFactory.Create(TInProcessTransport));
end;

end.
