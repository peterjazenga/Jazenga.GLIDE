{**********************************************************************
 Package pl_Astronomy.pkg
 From PilotLogic Software House
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit MoonCompEditors;

interface

uses

  messages,
  graphics,
  classes,
  controls,
  extctrls, TypInfo,lresources,PropEdits,ComponentEditors,
  sysutils; 

type

t_ah_datetimeproperty=class(TFloatProperty)
protected
  function parsespecialvalue(const s:string; var return:TDateTime):boolean;
public
  procedure edit; override;
  function getattributes:TPropertyAttributes; override;
  function  GetValue:string;// override;
  procedure GetValues(Proc:TGetStrProc); //override;
  procedure SetValue(const value:string); //override;
  procedure Initialize; override;
  function AllEqual:boolean; override;
  end;

implementation

type
  T_Specialdates=(sd_Now,sd_Midnight,sd_Midday,sd_Tomorrow,sd_Yesterday);
const
  specialvalues: array[sd_Now..sd_Yesterday] of string =
    ( 'Now', 'Midnight', 'Midday', 'Tomorrow', 'Yesterday');

procedure t_ah_datetimeproperty.edit;
begin
  end;

function t_ah_datetimeproperty.getattributes:TPropertyAttributes;
begin
  result:=[paMultiSelect,paValueList,paSortlist];
  end;

function t_ah_datetimeproperty.GetValue:string;
begin
  result:=DateTimeToStr(GetFloatValue);
  end;

procedure t_ah_datetimeproperty.GetValues(Proc:TGetStrProc);
var
  i: T_Specialdates;
begin
  for i:=low(specialvalues) to high(specialvalues) do
    proc(specialvalues[i]);
  end;

function t_ah_datetimeproperty.parsespecialvalue(const s:string; var return:TDateTime):boolean;
var
  i: T_Specialdates;
begin
  result:=false;
  for i:=low(specialvalues) to high(specialvalues) do
    if lowercase(s)=lowercase(specialvalues[i]) then begin
      result:=true;
      case i of
        sd_Now:       return:=now;
        sd_Midnight:  return:=trunc(now);
        sd_Midday:    return:=trunc(now)+0.5;
        sd_Tomorrow:  return:=now+1.0;
        sd_Yesterday: return:=now-1.0;
        end;
      BREAK;
      end;
  end;

procedure t_ah_datetimeproperty.SetValue(const value:string);
var
  dt: TdateTime;
begin
  if not parsespecialvalue(value,dt) then
    dt:=StrToDateTime(value);
  SetFloatValue(dt);
  end;

procedure t_ah_datetimeproperty.Initialize;
begin
  inherited Initialize;
  end;

function t_ah_datetimeproperty.AllEqual:boolean;
var
  i: integer;
  dt: TdateTime;
begin
  result:=true;
  dt:=getfloatvalue;
  for i:=0 to propcount-1 do
    if dt<>getfloatvalueat(i) then begin
      result:=false;
      BREAK;
      end;
  end;

end.

