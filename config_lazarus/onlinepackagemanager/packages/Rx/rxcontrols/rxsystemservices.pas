{ RxSystemServices unit

  Copyright (C) 2005-2017 Lagunov Aleksey alexs75@yandex.ru and Lazarus team
  original conception from rx library for Delphi (c)

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit RxSystemServices;

{$mode objfpc}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs;

type
  TRxServiceType = (sstAll, sstService, sstDrivers);
  TRxServiceStatus = (sssAll, sssActive, sssInactive);
  TRxServiceState = (srsStoped,          //SERVICE_STOPPED          : S := 'Сервис не запущен'
                     srsStartPending,    //SERVICE_START_PENDING    : S := 'Сервис в процессе запуска';
                     srsStopPending,     //SERVICE_STOP_PENDING     : S := 'Сервис в процессе завершения';
                     srsRunning,         //SERVICE_RUNNING          : S := 'Сервис запущен';
                     srsContinuePending, //SERVICE_CONTINUE_PENDING : S := 'Сервис в процессе запуска после временной оснановки';
                     srsPausePending,    //SERVICE_PAUSE_PENDING    : S := 'Сервис в процессе временной оснановки';
                     srsPaused           //SERVICE_PAUSED           : S := 'Сервис временно оснановлен';
                     );

  TRxServiceItem = record
    Name:string;
    Description:string;
    Status:TRxServiceState;
  end;

type

  { TRxSystemServices }

  TRxSystemServices = class(TComponent)
  private
    FItemCount: integer;
    FServerName: string;
    FServiceStatus: TRxServiceStatus;
    FServiceType: TRxServiceType;
    function GetItems(Index: integer): TRxServiceItem;
    procedure SetItemCount(const AValue: integer);
    procedure SetItems(Index: integer; const AValue: TRxServiceItem);
    procedure SetServerName(const AValue: string);
    procedure SetServiceStatus(const AValue: TRxServiceStatus);
    procedure SetServiceType(const AValue: TRxServiceType);
  protected
    procedure ClearItems;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Items[Index:integer]:TRxServiceItem read GetItems write SetItems;
    property ItemCount:integer read FItemCount write SetItemCount;
  published
    property ServerName:string read FServerName write SetServerName;
    property ServiceType:TRxServiceType read FServiceType write SetServiceType;     //(sstAll, sstService, sstDrivers);
    property ServiceStatus:TRxServiceStatus read FServiceStatus write SetServiceStatus; //(sssAll, sssActive, sssInactive);
  end;

implementation

{ TRxSystemServices }

procedure TRxSystemServices.SetServerName(const AValue: string);
begin
  if FServerName=AValue then exit;
  FServerName:=AValue;
end;

function TRxSystemServices.GetItems(Index: integer): TRxServiceItem;
begin

end;

procedure TRxSystemServices.SetItemCount(const AValue: integer);
begin
  if FItemCount=AValue then exit;
  FItemCount:=AValue;
end;

procedure TRxSystemServices.SetItems(Index: integer;
  const AValue: TRxServiceItem);
begin

end;

procedure TRxSystemServices.SetServiceStatus(const AValue: TRxServiceStatus);
begin
  if FServiceStatus=AValue then exit;
  FServiceStatus:=AValue;
end;

procedure TRxSystemServices.SetServiceType(const AValue: TRxServiceType);
begin
  if FServiceType=AValue then exit;
  FServiceType:=AValue;
end;

procedure TRxSystemServices.ClearItems;
begin
  FItemCount:=0;
end;

constructor TRxSystemServices.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TRxSystemServices.Destroy;
begin
  ClearItems;
  inherited Destroy;
end;

end.
