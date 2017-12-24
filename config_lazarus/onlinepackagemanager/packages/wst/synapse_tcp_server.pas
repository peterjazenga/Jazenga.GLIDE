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
unit synapse_tcp_server;

interface

uses
  Classes, SysUtils, blcksock, synsock, server_listener, wst_types;

const
  sSERVER_PORT = 1234;
  
type

  TwstSynapseTcpListener = class;

  { TClientHandlerThread }

  TClientHandlerThread = class(TThread)
  private
    FDefaultTimeOut: Integer;
    FSocketObject : TTCPBlockSocket;
    FSocketHandle : TSocket;
    FInputStream : TMemoryStream;
    FOutputStream : TMemoryStream;
    FOwner : TwstSynapseTcpListener;
  private
    procedure ClearBuffers();
    function ReadInputBuffer():Integer;
    procedure SendOutputBuffer();
  public
    constructor Create (ASocketHandle : TSocket; AOwner : TwstSynapseTcpListener);
    destructor Destroy();override;
    procedure Execute(); override;
    property DefaultTimeOut : Integer read FDefaultTimeOut write FDefaultTimeOut;
  end;
  
  { TServerListnerThread }

  TServerListnerThread = class(TThread)
  private
    FDefaultTimeOut: Integer;
    FSocketObject : TTCPBlockSocket;
    FSuspendingCount : Integer;
    FOwner : TwstSynapseTcpListener;
  public
    constructor Create(AOwner : TwstSynapseTcpListener);
    destructor Destroy(); override;
    procedure Execute(); override;
    procedure SuspendAsSoonAsPossible();
    procedure ResumeListening();
    property DefaultTimeOut : Integer read FDefaultTimeOut write FDefaultTimeOut;
  end;
  
  { TwstSynapseTcpListener }

  TwstSynapseTcpListener = class(TwstListener)
  private
    FServerThread : TServerListnerThread;
    FServerIpAddress : string;
    FListningPort : Integer;
    FDefaultClientPort : Integer;
    FServerSoftware : string;
  public
    constructor Create(
      const AServerIpAddress   : string  = '127.0.0.1';
      const AListningPort      : Integer = sSERVER_PORT;
      const ADefaultClientPort : Integer = 25000;
      const AServerSoftware    : string  = 'Web Service Toolkit Application'
    );
    destructor Destroy();override;
    procedure Start();override;
    procedure Stop();override;
    function IsActive : Boolean; override;
  end;
  
implementation
uses binary_streamer, server_service_intf, server_service_imputils
     {$IFNDEF FPC}, Windows, ActiveX, ComObj{$ENDIF};


{ TClientHandlerThread }

procedure TClientHandlerThread.ClearBuffers();
begin
  FInputStream.Size := 0;
  FOutputStream.Size := 0;
end;

function TClientHandlerThread.ReadInputBuffer(): Integer;
var
  binBuff : TByteDynArray;
  bufferLen : LongInt;
  i, j, c, readBufferLen : Integer;
begin
  FInputStream.Size := 0;
  Result := 0;
  bufferLen := 0;
  readBufferLen := FSocketObject.RecvBufferEx(@bufferLen,SizeOf(bufferLen),DefaultTimeOut);
  if ( readBufferLen = 0 ) and ( FSocketObject.LastError = WSAETIMEDOUT ) then begin
    Result := 0;
    //WriteLn('ReadInputBuffer() => TimeOut');
  end else begin
    FSocketObject.ExceptCheck();
    bufferLen := Reverse_32(bufferLen);
    FInputStream.Size := bufferLen;
    if ( bufferLen > 0 ) then begin
      c := 0;
      i := 1024;
      if ( i > bufferLen ) then
        i := bufferLen;
      SetLength(binBuff,i);
      repeat
        j := FSocketObject.RecvBufferEx(@(binBuff[0]),i,DefaultTimeOut);
        FSocketObject.ExceptCheck();
        FInputStream.Write(binBuff[0],j);
        Inc(c,j);
        if ( ( bufferLen - c ) > 1024 ) then
          i := 1024
        else
          i := bufferLen - c;
      until ( i = 0 ) or ( j <= 0 );
    end;
    FInputStream.Position := 0;
    Result := FInputStream.Size;
  end;
end;

procedure TClientHandlerThread.SendOutputBuffer();
begin
  FSocketObject.SendBuffer(FOutputStream.Memory,FOutputStream.Size);
end;

constructor TClientHandlerThread.Create(
  ASocketHandle : TSocket;
  AOwner : TwstSynapseTcpListener
);
begin
  FSocketHandle := ASocketHandle;
  FreeOnTerminate := True;
  FDefaultTimeOut := -1;//90000;
  FOwner := AOwner;
  inherited Create(False);
end;

destructor TClientHandlerThread.Destroy();
begin
  FreeAndNil(FOutputStream);
  FreeAndNil(FInputStream);
  inherited Destroy();
end;

function GetFormatForContentType(const AContentType : string):string ;
begin
  Result := Trim(AContentType);
  if AnsiSameText(Result,'text/xml') then
    Result := 'soap'
  else
    Result := 'binary';
end;

procedure TClientHandlerThread.Execute();
var
  wrtr : IDataStore;
  rdr : IDataStoreReader;
  trgt,ctntyp, frmt : TBinaryString;
  buff : TByteDynArray;
  rqst : IRequestBuffer;
  i : PtrUInt;
begin
{$IFNDEF FPC}
  CoInitialize(nil);
  try
{$ENDIF}
    FInputStream := TMemoryStream.Create();
    FOutputStream := TMemoryStream.Create();
    FSocketObject := TTCPBlockSocket.Create();
    try
      FSocketObject.RaiseExcept := True;
      try
        FSocketObject.Socket := FSocketHandle;
        FSocketObject.GetSins();
        while not Terminated do begin
          FOutputStream.Size := 0;
          if ( ReadInputBuffer() >= SizeOf(LongInt) ) then begin
            rdr := CreateBinaryReader(FInputStream);
            trgt := rdr.ReadAnsiStr();
            ctntyp := rdr.ReadAnsiStr();
            frmt := rdr.ReadAnsiStr();
            buff := rdr.ReadBinary();
            rdr := nil;
            FInputStream.Size := 0;
            FInputStream.Write(buff[0],Length(buff));
            SetLength(buff,0);
            FInputStream.Position := 0;
            rqst := TRequestBuffer.Create(trgt,ctntyp,FInputStream,FOutputStream,frmt);
            rqst.GetPropertyManager().SetProperty(sREMOTE_IP,FSocketObject.GetRemoteSinIP());
            rqst.GetPropertyManager().SetProperty(sREMOTE_PORT,IntToStr(FSocketObject.GetRemoteSinPort()));
            HandleServiceRequest(rqst);
            i := FOutputStream.Size;
            SetLength(buff,i);
            FOutputStream.Position := 0;
            FOutputStream.Read(buff[0],i);
            FOutputStream.Size := 0;
            wrtr := CreateBinaryWriter(FOutputStream);
            wrtr.WriteBinary(buff);
            SetLength(buff,0);
            SendOutputBuffer();
            ClearBuffers();
          end;
        end;
      except
        on e : Exception do begin
          FOwner.NotifyMessage(Format('Error : ThreadID = %d; Message = %s',[Self.ThreadID,e.Message]));
        end;
      end;
    finally
      FreeAndNil(FSocketObject);
    end;
{$IFNDEF FPC}
  finally
    CoUninitialize();
  end;
{$ENDIF}    
end;

{ TServerListnerThread }

constructor TServerListnerThread.Create(AOwner : TwstSynapseTcpListener);
begin
  FSocketObject := TTCPBlockSocket.Create();
  FreeOnTerminate := True;
  FDefaultTimeOut := 1000;
  FOwner := AOwner;
  inherited Create(false);
end;

destructor TServerListnerThread.Destroy();
begin
  FreeAndNil(FSocketObject);
  inherited Destroy();
end;

procedure TServerListnerThread.Execute();
var
  ClientSock : TSocket;
begin
{$IFNDEF FPC}
  CoInitialize(nil);
  try
{$ENDIF}
    try
      FSocketObject.RaiseExcept := True;
      FSocketObject.CreateSocket();
      FSocketObject.SetLinger(True,10);
      FSocketObject.Bind(FOwner.FServerIpAddress,IntToStr(FOwner.FListningPort));
      FSocketObject.Listen();
      while not Terminated do begin
        if ( FSuspendingCount > 0 ) then begin
          Suspend();
        end;
        if FSocketObject.CanRead(DefaultTimeOut) then begin
          ClientSock := FSocketObject.Accept();
          TClientHandlerThread.Create(ClientSock,FOwner);
        end;
      end;
    except
      on e : Exception do begin
        FOwner.NotifyMessage(Format('Listner Thread Error : ThreadID = %d; Message = %s',[Self.ThreadID,e.Message]));
        FOwner.NotifyMessage('Listner stoped.');
      end;
    end;
{$IFNDEF FPC}
  finally
    CoUninitialize();
  end;
{$ENDIF}
end;

procedure TServerListnerThread.SuspendAsSoonAsPossible();
begin
  InterLockedIncrement(FSuspendingCount);
end;

procedure TServerListnerThread.ResumeListening();
begin
  InterLockedDecrement(FSuspendingCount);
  if ( FSuspendingCount <= 0 ) then begin
    if Suspended then
      Resume();
  end;
end;

{ TwstSynapseTcpListener }

constructor TwstSynapseTcpListener.Create(
  const AServerIpAddress : string;
  const AListningPort : Integer;
  const ADefaultClientPort : Integer;
  const AServerSoftware : string
);
begin
  FServerIpAddress := AServerIpAddress;
  FListningPort := AListningPort;
  FDefaultClientPort := ADefaultClientPort;
  FServerSoftware := AServerSoftware;
end;

destructor TwstSynapseTcpListener.Destroy();
begin
  if ( FServerThread <> nil ) then begin
    FServerThread.Terminate();
    Start();
  end;
  inherited Destroy();
end;

function TwstSynapseTcpListener.IsActive: Boolean;
begin
  Result := (FServerThread <> nil) and (not FServerThread.Suspended);
end;

procedure TwstSynapseTcpListener.Start();
begin
  if ( FServerThread = nil ) then
    FServerThread := TServerListnerThread.Create(Self);
  if FServerThread.Suspended then
    FServerThread.ResumeListening();
end;

procedure TwstSynapseTcpListener.Stop();
begin
  if ( FServerThread <> nil ) and ( not FServerThread.Suspended ) then
    FServerThread.SuspendAsSoonAsPossible();
end;

end.

