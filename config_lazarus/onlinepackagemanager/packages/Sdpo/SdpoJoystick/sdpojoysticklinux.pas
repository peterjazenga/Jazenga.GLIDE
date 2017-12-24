unit sdpojoysticklinux;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BaseUnix, SdpoJoystick;

const
JS_EVENT_BUTTON =        $01;    // button pressed/released
JS_EVENT_AXIS   =        $02;    // joystick moved
JS_EVENT_INIT   =        $80;    // initial state of device

type

//struct js_event
//  __u32 time;     /* event timestamp in milliseconds */
//  __s16 value;    /* value */
//  __u8 type;      /* event type */
//  __u8 number;    /* axis/button number */

  js_event = record
    time: Longword;  //event timestamp in milliseconds
    value: Smallint; // value
    etype: byte;       // event type
    number: byte;     // axis/button number
  end;

function OpenJoystick(dev: string): integer;
procedure CloseJoystick;

var
  joyfd: integer;
  JoyEvent :js_event;
  lAxis: array[0..MaxAxis-1] of integer;
  lButs: array[0..MaxButtons-1] of integer;

implementation

function OpenJoystick(dev: string): integer;
begin
  result:=1;
  //joyfd := open(pchar(dev), O_NONBLOCK or O_RDONLY);
  //joyfd := fpopen(pchar(dev), O_RDONLY);
  joyfd := fpopen(dev, O_RDONLY);
  if joyfd < 0 then
    Result := -1;
end;

procedure CloseJoystick;
begin
  FpClose(joyfd);
end;

end.
