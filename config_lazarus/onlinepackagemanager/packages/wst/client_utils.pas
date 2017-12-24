{
    This file is part of the Web Service Toolkit
    Copyright (c) 2010 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$INCLUDE wst_global.inc}
unit client_utils;

interface
uses
  Classes, SysUtils,
  service_intf, imp_utils, base_service_intf, wst_types, filter_intf;

Type

{$M+}
  
  { TBaseTransport }

  TBaseTransport = class(TSimpleFactoryItem,ITransport)
  Private
    FPropMngr : IPropertyManager;
    FFilter : IDataFilter;
  protected
    function HasFilter() : Boolean;
    function GetFilter() : IDataFilter;
    function GetFilterString: string;
    procedure SetFilterString(const Value: string);
    procedure FilterInput(ASource, ADest : TStream);
    procedure FilterOutput(ASource, ADest : TStream);
    procedure DoSendAndReceive(ARequest,AResponse:TStream); virtual; abstract;
  public
    constructor Create();override;
    destructor Destroy();override;
    function GetTransportName() : string; virtual;
    function GetPropertyManager():IPropertyManager;
    procedure SendAndReceive(ARequest,AResponse:TStream); virtual; 
    function GetCookieManager() : ICookieManager; virtual; 
  published
    property FilterString : string read GetFilterString write SetFilterString;
  End;

  { TBaseTCPTransport }

  TBaseTCPTransport = class(TBaseTransport,ITransport)
  private
    FContentType : string;
    FFormat : string;
    FTarget : string;
    FUseBlockType : Boolean;
  protected
    procedure DoSend(const AData; const ALength : Int64); virtual; abstract;
    function DoReceive(var AData; const ALength : Int64) : Int64; virtual; abstract;
  public
    procedure SendAndReceive(ARequest,AResponse:TStream); override;
  Published
    property Target : string Read FTarget Write FTarget;
    property ContentType : string Read FContentType Write FContentType;
    property Format : string read FFormat write FFormat;
    property UseBlockType : Boolean read FUseBlockType write FUseBlockType default false;
  end;

{$M+}


implementation
uses
  wst_consts, binary_streamer, Math;

{ TBaseTCPTransport }

procedure TBaseTCPTransport.SendAndReceive(ARequest, AResponse : TStream);

  procedure ReadResponse(ADest : TStream);
  var
    bufferLen : LongInt;
    i, j, c : PtrInt;
    locBinBuff : TByteDynArray;
  begin
    bufferLen := 0;
    DoReceive(bufferLen,SizeOf(bufferLen));
    bufferLen := Reverse_32(bufferLen);
    ADest.Size := bufferLen;
    if ( bufferLen > 0 ) then begin
      c := 0;
      i := 1024;
      if ( i > bufferLen ) then
        i := bufferLen;
      SetLength(locBinBuff,i);
      repeat
        j := DoReceive(locBinBuff[0],i);
        ADest.Write(locBinBuff[0],j);
        Inc(c,j);
        i := Min(1024,(bufferLen-c));
      until ( i =0 ) or ( j <= 0 );
    end;
    ADest.Position := 0;
  end;

Var
  wrtr : IDataStore;
  buffStream : TMemoryStream;
  binBuff : TByteDynArray;
  locTempStream, locTempRes : TMemoryStream;
begin
  locTempStream := nil;
  locTempRes := nil;
  buffStream := TMemoryStream.Create();
  Try
    wrtr := CreateBinaryWriter(buffStream);
    if UseBlockType then
      wrtr.WriteInt32S(WST_BLOCK_TYPE); 
    wrtr.WriteInt32S(0);
    wrtr.WriteAnsiStr(Target);
    wrtr.WriteAnsiStr(ContentType);
    wrtr.WriteAnsiStr(Self.Format);
    if not HasFilter() then begin
      SetLength(binBuff,ARequest.Size);
      ARequest.Position := 0;
      ARequest.Read(binBuff[0],Length(binBuff));
    end else begin
      locTempStream := TMemoryStream.Create();
      FilterInput(ARequest,locTempStream);
{$IFDEF WST_DBG}
      TMemoryStream(locTempStream).SaveToFile('request.log.wire');
{$ENDIF WST_DBG}
      SetLength(binBuff,locTempStream.Size);
      locTempStream.Position := 0;
      locTempStream.Read(binBuff[0],Length(binBuff));
      locTempStream.Size := 0;
    end;
    wrtr.WriteBinary(binBuff);
    SetLength(binBuff,0);
    if UseBlockType then begin
      buffStream.Position := 4;
      wrtr.WriteInt32S(buffStream.Size-({BlockType}4+4));
    end else begin
      buffStream.Position := 0;
      wrtr.WriteInt32S(buffStream.Size-4);
    end;
    buffStream.Position := 0;

    DoSend(buffStream.Memory^,buffStream.Size);

    if not HasFilter() then begin
      ReadResponse(AResponse);
    end else begin
      locTempRes := TMemoryStream.Create();
      ReadResponse(locTempRes);
  {$IFDEF WST_DBG}
      TMemoryStream(locTempRes).SaveToFile('response.log.wire');
  {$ENDIF WST_DBG}
      FilterOutput(locTempRes,AResponse);
    end;

    {$IFDEF WST_DBG}
    TMemoryStream(AResponse).SaveToFile('response.log');
    {$ENDIF WST_DBG}
  Finally
    locTempStream.Free();
    locTempRes.Free();
    buffStream.Free();
  End;
end;

{ TBaseTransport }

constructor TBaseTransport.Create();
begin
  inherited;
  FPropMngr := TPublishedPropertyManager.Create(Self);
end;

destructor TBaseTransport.Destroy();
begin
  FPropMngr := Nil;
  inherited;
end;

procedure TBaseTransport.SendAndReceive(ARequest, AResponse : TStream);
var
  locTempStream, locTempRes : TMemoryStream;
begin
  if not HasFilter() then begin
    DoSendAndReceive(ARequest,AResponse);
  end else begin
    locTempRes := nil;
    locTempStream := TMemoryStream.Create();
    try
      FilterInput(ARequest,locTempStream);
{$IFDEF WST_DBG}
      TMemoryStream(locTempStream).SaveToFile('request.log.wire');
{$ENDIF WST_DBG}
      locTempRes := TMemoryStream.Create();
      DoSendAndReceive(locTempStream,locTempRes);
      locTempStream.Clear();
  {$IFDEF WST_DBG}
      TMemoryStream(locTempRes).SaveToFile('response.log.wire');
  {$ENDIF WST_DBG}
      FilterOutput(locTempRes,AResponse);
    finally
      locTempRes.Free();
      locTempStream.Free();
    end;
  end;
{$IFDEF WST_DBG}
  TMemoryStream(ARequest).SaveToFile('request.log');
  TMemoryStream(AResponse).SaveToFile('response.log');
{$ENDIF}
end;

function TBaseTransport.GetCookieManager() : ICookieManager;
begin
  raise ETransportExecption.CreateFmt(SERR_UnsupportedOperation,['GetCookieManager']);
end;

procedure TBaseTransport.FilterInput(ASource, ADest: TStream);
var
  locInBuffer, locBuffer : TByteDynArray;
  locOldPos : Int64;
begin
  if ASource.InheritsFrom(TMemoryStream) then begin
    locBuffer := FFilter.ExecuteInput(
                           TMemoryStream(ASource).Memory^,
                           ASource.Size,
                           FFilter.GetPropertyManager()
                         );
  end else begin
    SetLength(locInBuffer,ASource.Size);
    locOldPos := ASource.Position;
    ASource.Position := 0;
    try
      ASource.Read(locInBuffer[0],Length(locInBuffer));
    finally
      ASource.Position := locOldPos;
    end;
    locBuffer := FFilter.ExecuteInput(locInBuffer[0],Length(locInBuffer),FFilter.GetPropertyManager());
  end;
  ADest.Size := Length(locBuffer);
  ADest.Position := 0;
  ADest.Write(locBuffer[0],Length(locBuffer));
  ADest.Position := 0;
end;

procedure TBaseTransport.FilterOutput(ASource, ADest: TStream);
var
  locInBuffer, locBuffer : TByteDynArray;
  locOldPos : Int64;
begin
  if ASource.InheritsFrom(TMemoryStream) then begin
    locBuffer := FFilter.ExecuteOutput(TMemoryStream(ASource).Memory^,ASource.Size,FFilter.GetPropertyManager());
  end else begin
    SetLength(locInBuffer,ASource.Size);
    locOldPos := ASource.Position;
    ASource.Position := 0;
    try
      ASource.Read(locInBuffer[0],Length(locInBuffer));
    finally
      ASource.Position := locOldPos;
    end;
    locBuffer := FFilter.ExecuteOutput(locInBuffer[0],Length(locInBuffer),FFilter.GetPropertyManager());
  end;
  ADest.Size := Length(locBuffer);
  ADest.Position := 0;
  ADest.Write(locBuffer[0],Length(locBuffer));
  ADest.Position := 0;
end;

function TBaseTransport.GetFilter() : IDataFilter;
begin
  Result := FFilter;
end;

function TBaseTransport.GetFilterString: string;
begin
  Result := GenerateFilterString(FFilter);
end;

function TBaseTransport.GetTransportName() : string;
begin
  Result := ClassName();
end;

function TBaseTransport.GetPropertyManager() : IPropertyManager;
begin
  Result := FPropMngr;
end;

function TBaseTransport.HasFilter() : Boolean;
begin
  Result := (FFilter <> nil);
end;

procedure TBaseTransport.SetFilterString(const Value: string);
begin
  FFilter := ParseDataFilterString(Value);       
end;

end.
