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
unit ics_tcp_protocol;

interface

uses
  Classes, SysUtils,
  service_intf, imp_utils, base_service_intf, wst_types,
  WSocket;

  
Const
  sTRANSPORT_NAME = 'TCP';

Type

  ETCPException = class(EServiceException)
  End;
  
{$M+}
  { TTCPTransport }
  TTCPTransport = class(TSimpleFactoryItem,ITransport)
  Private
    FPropMngr : IPropertyManager;
    FConnection : TWSocket;
    FContentType : string;
    FTarget: string;
    function GetAddress: string;
    function GetPort: string;
    procedure SetAddress(const AValue: string);
    procedure SetPort(const AValue: string);
  private
    FDataLength : LongInt;
    FDataBuffer : string;
    FAllDataRead : Boolean;
    FBeginRead : Boolean;
    FFormat : string;
    FHasException : Boolean;
    FExceptionMessage : string;
    procedure DataAvailable(Sender: TObject; Error: Word);
    procedure BgExceptionHandler(Sender : TObject;E : Exception;var CanClose : Boolean);
  Public
    constructor Create();override;
    destructor Destroy();override;
    function GetPropertyManager():IPropertyManager;
    procedure SendAndReceive(ARequest,AResponse:TStream);
  Published
    property Target : string Read FTarget Write FTarget;
    property ContentType : string Read FContentType Write FContentType;
    property Address : string Read GetAddress Write SetAddress;
    property Port : string Read GetPort Write SetPort;
    property Format : string read FFormat write FFormat;
  End;
{$M+}

  procedure ICS_RegisterTCP_Transport();

implementation
uses binary_streamer, Math;

{ TTCPTransport }

function TTCPTransport.GetAddress: string;
begin
  Result := FConnection.Addr;
end;

function TTCPTransport.GetPort: string;
begin
  Result := FConnection.Port;
end;

procedure TTCPTransport.SetAddress(const AValue: string);
begin
  FConnection.Addr := AValue;
end;

procedure TTCPTransport.SetPort(const AValue: string);
begin
  FConnection.Port := AValue;
end;

procedure TTCPTransport.DataAvailable(Sender: TObject; Error: Word);
Var
  i,j : PtrInt;
  buff : string;
begin
  If Not FBeginRead Then Begin
    i := 1024;
    SetLength(buff,i);
    While ( FConnection.Receive(@(buff[1]),i) = i ) Do
      ;
    FDataBuffer := '';
    FDataLength := -1;
    Exit;
  End;

  If ( FDataLength < 0 ) Then Begin
    i := 4;
    if ( FConnection.Receive(@FDataLength,i) < i ) then
      raise ETCPException.Create('Error reading data length.');
    FDataLength := Reverse_32(FDataLength);
  End;
  If ( FDataLength > Length(FDataBuffer) ) Then Begin
    i := 1024;
    If ( i > FDataLength ) Then
      i := FDataLength;
    SetLength(buff,i);
    Repeat
      j := FConnection.Receive(@(buff[1]),i);
      FDataBuffer := FDataBuffer + Copy(buff,1,j);
      i := Min(1024,(FDataLength-Length(FDataBuffer)));
    Until ( i =0 ) or ( j <= 0 );
  End;
  FAllDataRead := ( FDataLength <= Length(FDataBuffer) );
end;

procedure TTCPTransport.BgExceptionHandler(Sender: TObject; E: Exception;var CanClose: Boolean);
begin
  CanClose := True;
  FHasException := True;
  FExceptionMessage := E.Message;
end;

constructor TTCPTransport.Create();
begin
  FDataLength := -1;
  FAllDataRead := False;
  FPropMngr := TPublishedPropertyManager.Create(Self);
  FConnection := TWSocket.Create(Nil);
  FConnection.OnDataAvailable := {$IFDEF FPC}@{$ENDIF}DataAvailable;
  FConnection.OnBgException := {$IFDEF FPC}@{$ENDIF}BgExceptionHandler;
end;

destructor TTCPTransport.Destroy();
begin
  FreeAndNil(FConnection);
  FPropMngr := Nil;
  inherited Destroy();
end;

function TTCPTransport.GetPropertyManager(): IPropertyManager;
begin
  Result := FPropMngr;
end;

procedure TTCPTransport.SendAndReceive(ARequest, AResponse: TStream);

Var
  wrtr : IDataStore;
  buffStream : TMemoryStream;
  strBuff : TBinaryString;
{$IFDEF WST_DBG}
  s : TBinaryString;
  i : Int64;
{$ENDIF WST_DBG}
begin
  buffStream := TMemoryStream.Create();
  Try
    wrtr := CreateBinaryWriter(buffStream);
    wrtr.WriteInt32S(0);
    wrtr.WriteAnsiStr(Target);
    wrtr.WriteAnsiStr(ContentType);
    wrtr.WriteAnsiStr(Self.Format);
    SetLength(strBuff,ARequest.Size);
    ARequest.Position := 0;
    ARequest.Read(strBuff[1],Length(strBuff));
    wrtr.WriteAnsiStr(strBuff);
    buffStream.Position := 0;
    wrtr.WriteInt32S(buffStream.Size-4);

    If ( FConnection.State = wsClosed ) Then Begin
      FConnection.Connect();
      While ( FConnection.State < wsConnected ) Do
        FConnection.ProcessMessage();
    End;

    FDataBuffer := '';
    FDataLength := -1;
    FAllDataRead := False;
    FHasException := False;
    FExceptionMessage := '';
    FBeginRead := True;

    FConnection.Send(buffStream.Memory,buffStream.Size);
    FConnection.Flush();
    While Not ( FAllDataRead Or FHasException ) Do
      FConnection.ProcessMessage();
    If FHasException Then
      Raise ETCPException.Create(FExceptionMessage);
    AResponse.Size := 0;
    AResponse.Write(FDataBuffer[1],Length(FDataBuffer));
    FDataBuffer := '';
    FDataLength := -1;
    FAllDataRead := False;
    AResponse.Position := 0;
    {$IFDEF WST_DBG}
    i := AResponse.Position;
    SetLength(s,AResponse.Size);
    AResponse.Read(s[1],AResponse.Size);
    WriteLn(s);
    {$ENDIF WST_DBG}
  Finally
    buffStream.Free();
  End;
end;

procedure ICS_RegisterTCP_Transport();
begin
  GetTransportRegistry().Register(sTRANSPORT_NAME,TSimpleItemFactory.Create(TTCPTransport));
end;

end.
