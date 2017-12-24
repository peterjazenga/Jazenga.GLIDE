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
unit file_logger_extension;

interface

uses
  Classes, SysUtils,
  logger_extension;

type

  { TFileLoggerServiceExtension
      This class require the "LogFileCompleteName" to be set to a valid
      file place.
  }

  TFileLoggerServiceExtension = class(TLoggerServiceExtension)
  protected
    procedure TraceMessage(const AMsg : string);override;
  end;

var
  LogFileCompleteName : string;
  
implementation
uses
  syncobjs, base_service_intf, server_service_intf;
  
var
  StreamInstance : TStream = nil;
  StreamInstanceLock : TCriticalSection = nil;
  
{ TFileLoggerServiceExtension }

procedure TFileLoggerServiceExtension.TraceMessage(const AMsg : string);
begin
  if ( Length(AMsg) > 0 ) then begin
    StreamInstanceLock.Acquire();
    try
      if ( StreamInstance = nil ) then begin
        if ( Length(LogFileCompleteName) = 0 ) then
          raise Exception.Create('"LogFileCompleteName" must be set for the TFileLoggerServiceExtension to work.');
        StreamInstance := TFileStream.Create(LogFileCompleteName,fmCreate);
        StreamInstance.Seek(0,soEnd);
      end;
      StreamInstance.Write(AMsg[1],Length(AMsg));
    finally
      StreamInstanceLock.Release();
    end;
  end;
end;

initialization
  StreamInstanceLock := TCriticalSection.Create();
  GetServiceExtensionRegistry().Register('TFileLoggerServiceExtension',TSimpleItemFactory.Create(TFileLoggerServiceExtension));
  
finalization
  FreeAndNil(StreamInstance);
  FreeAndNil(StreamInstanceLock);
  
end.

