{ SdpoJoystick v0.1.1

  CopyRight (C) 2008 Paulo Malheiros

  This library is Free software; you can rediStribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is diStributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; withOut even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a Copy of the GNU Library General Public License
  along with This library; if not, Write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

  This license has been modified. See File LICENSE.ADDON for more inFormation.
  Should you find these sources without a LICENSE File, please contact
  me at paulo.malheiros@fe.up.pt
}
unit SdpoJoystick; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math,
  {$IFDEF LINUX}
  Forms, BaseUnix,
  {$ELSE}
  mmsystem, LCLIntf,
  {$ENDIF}
  LResources;

{$IFDEF LINUX}
{$I sdpojoysticklinuxtype.inc}
{$ENDIF}

const
  JOYSTICKID1 = 0;
  JOYSTICKID2 = 1;

  MaxAxis = 16;
  MaxButtons = 16;
  MaxWinAxis = 7;

type
  TJoystickID=(
    dwJoystickID1,
    dwJoystickID2);

const
  JoystickIDConsts: array[TJoystickID] of LongWord =(
    JOYSTICKID1,
    JOYSTICKID2);

type
  { TSdpoJoystick }
  TSdpoJoystick = class(TComponent)
  private
    { private declarations }
    FActive: boolean;
    FLinDevice: string;
    FWinDevice: TJoystickID;
    {$IFDEF MSWINDOWS}
    FLastUpdate: integer;
    Joy: TJOYINFOEX;
    ErrorResult: MMRESULT;
    {$ENDIF}
    FUpdateInterval: integer;

    {$IFDEF LINUX}
    JoystickThread : TSdpoJoystickThread;
    //FClosing: boolean;
    {$ENDIF}
    
    procedure DeviceOpen;
    procedure DeviceClose;
    procedure SetActive(state: boolean);

    procedure SetUpdateInterval(newInterval: integer);

    function GetAxis(idx: integer): integer;
    function GetButtons(idx: integer): integer;
  protected
    { protected declarations }
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Update;
    
    function Init: boolean;
    procedure Close;
    procedure Read(var LocalAxis, LocalButtons: array of integer);
    property UpdateInterval: integer read FUpdateInterval write SetUpdateInterval;

    property Axis[idx: integer]: integer read GetAxis;
    property Buttons[idx: integer]: integer read GetButtons;
  published
    property Active: boolean read FActive write SetActive;
    property DeviceLin: string read FLinDevice write FLinDevice;
    property DeviceWin: TJoystickID read FWinDevice write FWinDevice;
  end;

procedure Register;

implementation

{$IFDEF LINUX}
uses sdpojoysticklinux;
{$ENDIF}

procedure TSdpoJoystick.SetUpdateInterval(newInterval: integer);
begin
  FUpdateInterval:=newInterval;
end;

constructor TSdpoJoystick.Create(AOwner: TComponent);
begin
  FActive := false;
  {$IFDEF LINUX}
  JoystickThread := nil;
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  FUpdateInterval := 16;
  {$ENDIF}
  FLinDevice := '/dev/input/js0';
  FWinDevice := dwJoystickID1;

  inherited Create(AOwner);
end;

destructor TSdpoJoystick.Destroy;
begin
  {$IFDEF LINUX}
  CloseJoystick;
  {$ENDIF}
  inherited Destroy;
end;

function TSdpoJoystick.Init: boolean;
begin
  Active := true;
  result := Active;
end;

procedure TSdpoJoystick.Close;
begin
  Active := false;
end;


{$IFDEF LINUX}
{$I sdpojoysticklinux.inc}
{$ENDIF}

{$IFDEF MSWINDOWS}
{$I sdpojoystickwindows.inc}
{$ENDIF}

procedure Register;
begin
  RegisterComponents('5dpo',[TSdpoJoystick]);
end;

initialization
{$i TSdpoJoystick.lrs}

end.    

