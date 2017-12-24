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
unit semaphore;

interface

uses                 
  {$IFDEF WINDOWS}
     Windows,
  {$ENDIF}
  SysUtils,
  SyncObjs;

type

  ESemaphoreException = class(Exception);

{$IFDEF WINDOWS}
  TWindowsSemaphoreObject = class
  private
    FHandle : THandle;
    FLimit: Integer;
  public
    constructor Create(const ALimit : Integer);
    destructor Destroy(); override;
    function WaitFor(ATimeout : Cardinal) : TWaitResult;
    procedure Release();
    property Limit : Integer read FLimit;
  end;
{$ENDIF WINDOWS}

  { TCsSemaphoreObject }

  TCsSemaphoreObject = class
  private
    FCriticalSection : SyncObjs.TCriticalSection;
    FLimit : Integer;
    FCount : Integer;
  public
    constructor Create(const ALimit : Integer);
    destructor Destroy(); override;
    function WaitFor(ATimeout : Cardinal) : TWaitResult;
    procedure Release();
    property Limit : Integer read FLimit;
  end;
                
{$IFDEF WINDOWS}
  TSemaphoreObject = TWindowsSemaphoreObject;
{$ELSE}                                      
  TSemaphoreObject = TCsSemaphoreObject;
{$ENDIF}

resourcestring
  SERR_InvalidSemaphoreCount = 'Invalid semaphore maximum count : %d.';

implementation

{ TCsSemaphoreObject }

constructor TCsSemaphoreObject.Create(const ALimit : Integer);
begin
  if (ALimit < 1) then
    raise ESemaphoreException.CreateFmt(SERR_InvalidSemaphoreCount,[ALimit]);
  FLimit := ALimit;
  FCriticalSection := SyncObjs.TCriticalSection.Create();
end;

destructor TCsSemaphoreObject.Destroy;
begin
  FCriticalSection.Free();
  inherited Destroy;
end;

function TCsSemaphoreObject.WaitFor(ATimeout : Cardinal) : TWaitResult;
begin
  FCriticalSection.Acquire();
  try
    if (FCount < FLimit) then begin
      FCount := FCount+1;
      Result := wrSignaled;
    end else begin
      Result := wrAbandoned;
    end;
  finally
    FCriticalSection.Release();
  end;
end;

procedure TCsSemaphoreObject.Release;
begin
  FCriticalSection.Acquire();
  try
    if (FCount > 0) then
      FCount := FCount-1;
  finally
    FCriticalSection.Release();
  end;
end;

{$IFDEF WINDOWS}
{ TWindowsSemaphoreObject }

constructor TWindowsSemaphoreObject.Create(const ALimit : Integer);
begin
  if (ALimit < 1) then
    raise ESemaphoreException.CreateFmt(SERR_InvalidSemaphoreCount,[ALimit]);
  FLimit := ALimit;
  FHandle := CreateSemaphore(nil,ALimit,ALimit,'');
  if (FHandle = THandle(0)) then
    RaiseLastOSError();
end;

destructor TWindowsSemaphoreObject.Destroy;
begin
  if ( FHandle <> THandle(0) ) then
    CloseHandle(FHandle);
  inherited Destroy;
end;

function TWindowsSemaphoreObject.WaitFor(ATimeout : Cardinal) : TWaitResult;
var
  intRes : DWORD;
begin
  intRes := WaitForSingleObject(FHandle,ATimeout);
  case intRes of
    WAIT_OBJECT_0  : Result := wrSignaled;
    WAIT_TIMEOUT   : Result := wrTimeout;
    WAIT_ABANDONED : Result := wrAbandoned;
    else
                     Result := wrTimeout;
  end;
end;

procedure TWindowsSemaphoreObject.Release;
begin
  ReleaseSemaphore(FHandle,1,nil);
end;
{$ENDIF WINDOWS}

end.


