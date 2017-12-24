unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  SdpoJoystick, ExtCtrls, StdCtrls;

type

  { TFMain }

  TFMain = class(TForm)
    CBActive: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    RGReadMode: TRadioGroup;
    SdpoJoystick: TSdpoJoystick;
    TimerAll: TTimer;
    procedure CBActiveChange(Sender: TObject);
    procedure TimerAllTimer(Sender: TObject);
    procedure ProcJoystickAll;
    procedure ProcJoystickSingle;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  FMain: TFMain;

implementation

{ TFMain }

procedure TFMain.ProcJoystickAll;
var ax: array[0..3] of integer;
    bt: array[0..11] of integer;
    i: integer;
begin
  if SdpoJoystick.Active then begin
    SdpoJoystick.Read(ax,bt);
    Edit1.Text:='A ';
    Edit2.Text:='';
    for i:=0 to 3 do begin
      Edit1.Text:=Edit1.Text+IntToStr(ax[i])+' ';
    end;
    for i:=0 to 11 do begin
      Edit2.Text:=Edit2.Text+IntToStr(bt[i])+' ';
    end;
  end;
end;

procedure TFMain.ProcJoystickSingle;
begin
  if SdpoJoystick.Active then begin
    Edit1.Text:='S '+IntToStr(SdpoJoystick.Axis[0])+' '+IntToStr(SdpoJoystick.Axis[1])+' '+IntToStr(SdpoJoystick.Axis[2])+' '+
                IntToStr(SdpoJoystick.Axis[3])+' '+IntToStr(SdpoJoystick.Axis[4])+' '+IntToStr(SdpoJoystick.Axis[5])+' '+IntToStr(SdpoJoystick.Axis[6]);
    Edit2.Text:=IntToStr(SdpoJoystick.Buttons[0])+' '+IntToStr(SdpoJoystick.Buttons[1])+' '+IntToStr(SdpoJoystick.Buttons[2])+' '+
                IntToStr(SdpoJoystick.Buttons[3])+' '+IntToStr(SdpoJoystick.Buttons[4])+' '+IntToStr(SdpoJoystick.Buttons[5])+' '+
                IntToStr(SdpoJoystick.Buttons[6])+' '+IntToStr(SdpoJoystick.Buttons[7])+' '+IntToStr(SdpoJoystick.Buttons[8])+' '+
                IntToStr(SdpoJoystick.Buttons[9])+' '+IntToStr(SdpoJoystick.Buttons[10])+' '+IntToStr(SdpoJoystick.Buttons[11]);
  end;
end;

procedure TFMain.TimerAllTimer(Sender: TObject);
begin
  if RGReadMode.ItemIndex = 0 then
    ProcJoystickAll
  else
    ProcJoystickSingle;
end;

procedure TFMain.CBActiveChange(Sender: TObject);
begin
  if CBActive.Checked then begin
    SdpoJoystick.Init;
  end else begin
    SdpoJoystick.Close;
  end;
end;

initialization
  {$I main.lrs}

end.

