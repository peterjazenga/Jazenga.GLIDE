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
unit fpc_tcp_server;

interface

uses
  Classes, SysUtils, ssockets, server_listener, wst_types;

const
  sSERVER_PORT = 1234;   
type

  TwstFPCTcpListener = class;

  { TClientHandlerThread }

  TClientHandlerThread = class(TThread)
  private
    FDefaultTimeOut: Integer;
    FSocket : TSocketStream;
    FOwner : TwstFPCTcpListener;
  private
    function ReadRequest(ARequest : TStream; var ABlockType : LongInt):Integer;
    procedure SendResponse(AResponse : TMemoryStream);
  public
    constructor Create (ASocket : TSocketStream; AOwner : TwstFPCTcpListener);
    destructor Destroy();override;
    procedure Execute(); override;
    property DefaultTimeOut : Integer read FDefaultTimeOut write FDefaultTimeOut;
  end;
  
  { TServerListnerThread }

  TServerListnerThread = class(TThread)
    procedure DoConnect(Sender: TObject; Data: TSocketStream);
  private
    FDefaultTimeOut: Integer;
    FSocketObject : TInetServer;
    FSuspendingCount : Integer;
    FOwner : TwstFPCTcpListener;
  public
    constructor Create(AOwner : TwstFPCTcpListener);
    destructor Destroy(); override;
    procedure Execute(); override;
    procedure SuspendAsSoonAsPossible();
    procedure ResumeListening();
    property DefaultTimeOut : Integer read FDefaultTimeOut write FDefaultTimeOut;
  end;    
  
  { TwstFPCTcpListener }

  TwstFPCTcpListener = class(TwstBaseTcpListener)
  private
    FServerThread : TServerListnerThread;
    FPort : Integer;
  public
    constructor Create(const APort : Integer = sSERVER_PORT);
    destructor Destroy();override;
    procedure Start();override;
    procedure Stop();override;
    function IsActive : Boolean; override;
  end;
  
implementation

uses 
  wst_consts, binary_streamer, server_service_intf, server_service_imputils, math;

{ TClientHandlerThread }


function TClientHandlerThread.ReadRequest(
      ARequest   : TStream; 
  var ABlockType : LongInt
): Integer;
var
  binBuff : TByteDynArray;
  bufferLen, bktype : TInt32S;
  i, j, c : PtrInt;

begin
  Result := 0;
  if (tloHandleBlockType in FOwner.Options) then begin
    bktype := 0;
    j:=FSocket.Read(bktype,SizeOf(bktype));
    if (j<0) then
      raise Exception.CreateFmt(SERR_ErrorReadindDataToSocket,[FSocket.LastError]);
    if (j=0) then
      Exit(0) // Closed gracefully
    else
      bktype:=Reverse_32(bktype); 
  end;
  
  bufferLen := 0;
  j:=FSocket.Read(bufferLen,SizeOf(bufferLen));
  if (j<0) then
    Raise Exception.CreateFmt(SERR_ErrorReadindDataToSocket,[FSocket.LastError]);
  if (j=0) then
    Exit(0) // Closed gracefully
  else
    begin
    bufferLen:=Reverse_32(bufferLen);
    ARequest.Size:=bufferLen;
    c:=0;
    i:=1024;
    I:=Min(BufferLen,1024);
    SetLength(binBuff,i);
    repeat
        j:=FSocket.Read(binBuff[0],i);
        If (J<=0) then
          Raise Exception.CreateFmt(SERR_ErrorReadindDataToSocket,[FSocket.LastError]);
        ARequest.Write(binBuff[0],j);
        Inc(c,j);
        I:=Min(1024,(bufferLen - c ))
    until (i=0) or (j=0);
    ARequest.Position:=0;
    Result:=C;
    if C<ARequest.Size then
      ARequest.Size:=C;
  end;
  
  if (tloHandleBlockType in FOwner.Options) then
    ABlockType := bktype;
end;

procedure TClientHandlerThread.SendResponse(AResponse : TMemoryStream);

Var
  P : PByte;
  W,C : Integer;

begin
  P:=AResponse.Memory;
  C:=AResponse.Size;
  Repeat
    W:=FSocket.Write(P^,C);
    if (W<0) then
      Raise Exception.CreateFmt(SERR_ErrorSendindDataToSocket,[FSocket.LastError]);
    Inc(P,W);
    Dec(C,W);
  Until (C=0) or (w=0);
end;

constructor TClientHandlerThread.Create(
  ASocket : TSocketStream;
  AOwner : TwstFPCTcpListener
);
begin
  FSocket:= ASocket;
  FreeOnTerminate := True;
  FOwner := AOwner;
  inherited Create(False);
end;

destructor TClientHandlerThread.Destroy();
begin
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
  ARequest,AResponse : TMemoryStream;
  i : PtrUInt;
  blocktype : TInt32S;

begin
  while not Terminated do begin
    Try
      blocktype := 0;
      AResponse := nil;
      ARequest := TMemoryStream.Create;
      try
        if ReadRequest(ARequest,blocktype)>SizeOf(LongInt) then begin
          AResponse := TMemoryStream.Create();
          if (tloHandleBlockType in FOwner.Options) and 
             (blocktype <> WST_BLOCK_TYPE) 
          then begin
            if (FOwner.UnknownBlockHandler <> nil) then 
              FOwner.UnknownBlockHandler.Execute(blocktype,ARequest,AResponse);
          end else begin
            rdr := CreateBinaryReader(ARequest);
            trgt := rdr.ReadAnsiStr();
            ctntyp := rdr.ReadAnsiStr();
            frmt := rdr.ReadAnsiStr();
            buff := rdr.ReadBinary();
            rdr := nil;
            ARequest.Size := 0;
            ARequest.Write(buff[0],Length(buff));
            SetLength(buff,0);
            ARequest.Position := 0;
            rqst := TRequestBuffer.Create(trgt,ctntyp,ARequest,AResponse,frmt);
            //rqst.GetPropertyManager().SetProperty(sREMOTE_IP,FSocketObject.GetRemoteSinIP());
            //rqst.GetPropertyManager().SetProperty(sREMOTE_PORT,IntToStr(FSocketObject.GetRemoteSinPort()));
            HandleServiceRequest(rqst);
            i := AResponse.Size;
            SetLength(buff,i);
            AResponse.Position := 0;
            AResponse.Read(buff[0],i);
            AResponse.Size := 0;
            wrtr := CreateBinaryWriter(AResponse);
            wrtr.WriteBinary(buff);
            SetLength(buff,0);
          end;
          if (AResponse.Size > 0) then
            SendResponse(AResponse);
        end;
      finally
        AResponse.Free;
        ARequest.Free;
      end;
    except
      on e : Exception do begin
        Terminate;
        FOwner.NotifyMessage(Format('Error : ThreadID = %d; Message = %s',[Self.ThreadID,e.Message]));
      end;
    end;
  end;
end;

{ TServerListnerThread }

procedure TServerListnerThread.DoConnect(Sender: TObject; Data: TSocketStream);
begin
  if (FSuspendingCount>0 ) then
    Suspend();
  if Not Terminated then
    TClientHandlerThread.Create(Data,FOwner)
  else
    Data.Free;
end;

constructor TServerListnerThread.Create(AOwner : TwstFPCTcpListener);
begin
  FOwner := AOwner;
  inherited Create(false);
end;

destructor TServerListnerThread.Destroy();
begin
  FreeAndNil(FSocketObject);
  inherited Destroy();
end;

procedure TServerListnerThread.Execute();

begin
  try
    FSocketObject:=TInetServer.Create(FOwner.FPort);
    try
      FSocketObject.Bind();
      FSocketObject.OnConnect:=@DoConnect;
      FSocketObject.StartAccepting();
    Finally
      FSocketObject.Free;
    end;
  except
     on e : Exception do
       begin
       Terminate;
       FOwner.NotifyMessage(Format('Listner Thread Error : ThreadID = %d; Message = %s',[Self.ThreadID,e.Message]));
       FOwner.NotifyMessage('Listner stoped.');
       end;
  end;
end;

procedure TServerListnerThread.SuspendAsSoonAsPossible();
begin
  InterLockedIncrement(FSuspendingCount);
end;

procedure TServerListnerThread.ResumeListening();
begin
  InterLockedDecrement(FSuspendingCount);
  if (FSuspendingCount<=0 ) then
    begin
    if Suspended then
      Resume();
    end;
end;

{ TwstFPCTcpListener }

constructor TwstFPCTcpListener.Create(const APort : Integer = sServer_Port);
begin
  FPort:=APort;
end;

destructor TwstFPCTcpListener.Destroy();
begin
  if (FServerThread<>nil ) then
    begin
    FServerThread.Terminate();
    Start();
    end;
  inherited Destroy();
end;

function TwstFPCTcpListener.IsActive: Boolean;
begin
  Result := (FServerThread <> nil) and (not FServerThread.Suspended);
end;

procedure TwstFPCTcpListener.Start();
begin
  if (FServerThread=nil) then
    FServerThread:=TServerListnerThread.Create(Self);
  if FServerThread.Suspended then
    FServerThread.ResumeListening();
end;

procedure TwstFPCTcpListener.Stop();
begin
  if (FServerThread<>nil) and (not FServerThread.Suspended) then
    FServerThread.SuspendAsSoonAsPossible();
end;

end.

