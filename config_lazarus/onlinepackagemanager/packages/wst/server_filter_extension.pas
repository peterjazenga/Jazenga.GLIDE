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
unit server_filter_extension;

{.$DEFINE WST_DBG}

interface

uses
  SysUtils, Classes,
  wst_types, base_service_intf, server_service_intf, filter_intf;

type

  { TFilterExtension }
{$TYPEINFO ON}
  TFilterExtension = class(TSimpleFactoryItem,IServiceExtension)
  private
    FPropertyManager : IPropertyManager;
    FFilter : IDataFilter;
    function GetFilterString: string;
    procedure SetFilterString(const Value: string);
  private
    procedure FilterInput(
            ASource,
            ADest  : TStream;
      const AProps : IPropertyManager
    );
    procedure FilterOutput(
            ASource,
            ADest  : TStream;
      const AProps : IPropertyManager
    );
  protected
    procedure ProcessMessage(
      const AMessageStage  : TMessageStage;
            ACallContext   : ICallContext;
            AMsgData       : IInterface
              { The "AMsgData" parameter actual type depends on the message state
                on correspond to :
                  - IRequestBuffer on "msBeforeDeserialize" and "msAfterSerialize"
                  - IFormatterResponse on "msAfterDeserialize", "msBeforeSerialize"
              }
    );
    function GetPropertyManager():IPropertyManager;
  public
    constructor Create(); override;
  published
    property FilterString : string read GetFilterString write SetFilterString;
  end;
{$TYPEINFO OFF}

implementation
uses
  TypInfo, imp_utils, wst_consts;

{ TFilterExtension }

procedure TFilterExtension.ProcessMessage(
  const AMessageStage: TMessageStage;
        ACallContext: ICallContext;
        AMsgData: IInterface
);
var
  rb : IRequestBuffer;
  strm : TStream;
  locStream : TMemoryStream;
  locProps : IPropertyManager;
begin
  locProps := ACallContext.GetPropertyManager();
  case AMessageStage of
    msBeforeDeserialize :
      begin
        rb := AMsgData as IRequestBuffer;
        strm := rb.GetContent();
{$IFDEF WST_DBG}
        TMemoryStream(strm).SaveToFile('req.log.wire');
{$ENDIF WST_DBG}
        locStream := TMemoryStream.Create();
        try
          FilterOutput(strm,locStream,locProps);
{$IFDEF WST_DBG}
          locStream.SaveToFile('req.log');
{$ENDIF WST_DBG}
          strm.Size := locStream.Size;
          strm.Position := 0;
          strm.CopyFrom(locStream,0);
        finally
          locStream.Free();
        end;
        strm.Position := 0;
      end;
    msAfterSerialize :
      begin
        rb := AMsgData as IRequestBuffer;
        strm := rb.GetResponse();
        locStream := TMemoryStream.Create();
        try
          FilterInput(strm,locStream,locProps);
          strm.Size := locStream.Size;
          strm.Position := 0;
          strm.CopyFrom(locStream,0);
        finally
          locStream.Free();
        end;
        strm.Position := 0;
      end;
  end;
end;

procedure TFilterExtension.FilterInput(
         ASource,
         ADest  : TStream;
   const AProps : IPropertyManager
);
var
  locInBuffer, locBuffer : TByteDynArray;
  locOldPos : Int64;
begin
  if ASource.InheritsFrom(TMemoryStream) then begin
    locBuffer := FFilter.ExecuteInput(TMemoryStream(ASource).Memory^,ASource.Size,AProps);
  end else begin
    SetLength(locInBuffer,ASource.Size);
    locOldPos := ASource.Position;
    ASource.Position := 0;
    try
      ASource.Read(locInBuffer[0],Length(locInBuffer));
    finally
      ASource.Position := locOldPos;
    end;
    locBuffer := FFilter.ExecuteInput(locInBuffer[0],Length(locInBuffer),AProps);
  end;
  ADest.Size := Length(locBuffer);
  ADest.Position := 0;
  ADest.Write(locBuffer[0],Length(locBuffer));
  ADest.Position := 0;
end;

procedure TFilterExtension.FilterOutput(
        ASource,
        ADest  : TStream;
  const AProps : IPropertyManager
);
var
  locInBuffer, locBuffer : TByteDynArray;
  locOldPos : Int64;
begin
  if ASource.InheritsFrom(TMemoryStream) then begin
    locBuffer := FFilter.ExecuteOutput(TMemoryStream(ASource).Memory^,ASource.Size,AProps);
  end else begin
    SetLength(locInBuffer,ASource.Size);
    locOldPos := ASource.Position;
    ASource.Position := 0;
    try
      ASource.Read(locInBuffer[0],Length(locInBuffer));
    finally
      ASource.Position := locOldPos;
    end;
    locBuffer := FFilter.ExecuteOutput(locInBuffer[0],Length(locInBuffer),AProps);
  end;
  ADest.Size := Length(locBuffer);
  ADest.Position := 0;
  ADest.Write(locBuffer[0],Length(locBuffer));
  ADest.Position := 0;
end;

constructor TFilterExtension.Create;
begin
  inherited;
  FPropertyManager := TPublishedPropertyManager.Create(Self);
end;

function TFilterExtension.GetPropertyManager: IPropertyManager;
begin
  Result := FPropertyManager;
end;

function TFilterExtension.GetFilterString: string;
var
  locPM : IPropertyManager;
  ls : TStringList;
  locRes, s : string;
  i : Integer;
begin
  locRes := '';
  if ( FFilter <> nil ) then begin
    locRes := FFilter.GetName();
    locPM := FFilter.GetPropertyManager();
    ls := TStringList.Create();
    try
      if ( locPM.GetPropertyNames(ls) > 0 ) then begin
        for i := 0 to Pred(ls.Count) do begin
          s := ls[i];
          locRes := SysUtils.Format('%s,%s>%s',[locRes,s,locPM.GetProperty(s)]);
        end;
      end;
    finally
      ls.Free();
    end;
  end;
  Result := locRes;
end;

procedure TFilterExtension.SetFilterString(const Value: string);
var
  locBuffer, locName, locValue : string;
  locPM : IPropertyManager;
  locFilterManager : IDataFilterRegistry;
  locFilter : IDataFilter;
begin
  locBuffer := Value;
  if IsStrEmpty(locBuffer) then begin
    FFilter := nil;
    Exit;
  end;

  //The filter name
  locName := Trim(GetToken(locBuffer,','));
  locFilterManager := GetDataFilterRegistry();
  if not locFilterManager.Find(locName,locFilter) then
    raise EServiceExtensionException.CreateFmt(SERR_DataFilterNotFound,[locName]);
  locPM := locFilter.GetPropertyManager();
  while True do begin
    locName := GetToken(locBuffer,'>');
    if IsStrEmpty(locName) then
      Break;
    locValue := GetToken(locBuffer,',');
    locPM.SetProperty(locName,locValue);
  end;
  FFilter := locFilter;
end;

initialization
  GetServiceExtensionRegistry().Register(
    'TFilterExtension',
    TSimpleItemFactory.Create(TFilterExtension)
  );

end.
