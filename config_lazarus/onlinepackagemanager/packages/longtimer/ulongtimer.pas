unit uLongTimer;

{ TlongTimer

  Based on TIdleTimer component
  1. Set the Interval type
  2. For all Interval Types, you can set the Hour
  3. The OnTimer event will only be fired at the specified intervals
  4. The underlying interval is 30 minutes (when idle)

  Copyright (C)2014 minesadorada@charcodelvalle.com
  Modified GPL
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

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, ExtCtrls, DateUtils, Dialogs, AboutLongTimerunit;

type
  TIntervalType = (lt1Daily, lt2Weekly, lt3Monthly);
  TSampleInterval = (lt1Everyminute, lt2Every5minutes, lt3Every10Minutes,
    lt4Every30Minutes, lt5Every45Minutes);
  TDay = (lt1Monday, lt2Tuesday, lt3Wednesday, lt4Thursday, lt5Friday, lt6Saturday, lt7Sunday);

  TLongTimer = class(TAboutLongTimer)
  private
    { Private declarations }
    fCurrentDateTime, fLastFiredDateTime: TDateTime;
    fIntervalType: TIntervalType;
    fHour, fDay, fDate: word;
    fTday: TDay;
    fHourDone, fDayDone, fDateDone: boolean;
    fSampleInterval: TSampleInterval;
    fVersion: string;
    fOnSample: TNotifyEvent;
    procedure SetDay(aDay: TDay);
    procedure SetDailyHour(aHour: word);
    procedure SetMonthlyDate(ADate: word);
    procedure SetSampleInterval(ASampleInterval: TSampleInterval);
  protected
    { Protected declarations }
    procedure DoOnIdle(Sender: TObject; var Done: boolean); override;
    procedure DoOnIdleEnd(Sender: TObject); override;
    procedure DoOnTimer; override;
  public
    { Public declarations }
    constructor Create(TheOwner: TComponent); override;
    // Until the first Timer event, this will be the component's creation time
    property LastFired: TDateTime read fLastFiredDateTime;
  published
    { Published declarations }
    // Default=false
    property AutoEnabled;
    // Same as TIdleTimer
    property AutoStartEvent;
    // Same as TIdleTimer
    property AutoEndEvent;
    // Same as TIdleTimer
    property Enabled;
    // This is fired only at the Long Intervals you set
    property OnTimer;
    // Same as TIdleTimer
    property OnStartTimer;
    // Same as TIdleTimer
    property OnStopTimer;
    // If Weekly or Monthly you can also set the Daily24Hour property
    property IntervalType: TIntervalType
      read fIntervalType write fIntervalType default lt1Daily;
    // Smaller = more accurate, larger = less CPU time
    property SampleInterval: TSampleInterval read fSampleInterval
      write SetSampleInterval default lt3Every10Minutes;
    // 0=Midnight, 4=4am, 16=4pm etc.
    property Daily24Hour: word read fHour write SetDailyHour;
    // You can also set the Hour as well as the Weekday
    property WeeklyDay: TDay read fTDay write SetDay;
    // You can also set the Hour as well as the date
    property MonthlyDate: word read fDate write SetMonthlyDate default 1;
    // Version string of this component
    property Version: string read fVersion;
    // Fired every time LongTimer samples
    property OnSample: TNotifyEvent read fOnSample write fOnSample;
  end;

procedure Register;

implementation

const
  C_OneMinute = 60000;
  C_Version = '0.0.3';

(*
  V0.0.1: Initial commit
  V0.0.2: Added OnSample event
  V0.0.3: ??
*)
procedure Register;
begin
  RegisterComponents('System', [TLongTimer]);
  {$I longtimer_icon.lrs}
end;

constructor TLongTimer.Create(TheOwner: TComponent);
var
  sz: string;
begin
  inherited;
  // Set About dialog properties
  AboutBoxComponentName := 'TLongTimer';
  AboutBoxTitle := 'TLongTimer Component';
  // AboutBoxWidth (integer)
  AboutBoxHeight := 380;
  sz := 'LongTimer is a descendent of TIdleTimer' + LineEnding;
  sz += 'and shares its properties and methods.' + LineEnding + LineEnding;
  sz += 'Additional properties affect when the' + LineEnding;
  sz += 'OnTimer event is fired.' + LineEnding + LineEnding;
  sz += 'With LongTimer, the OnTimer event' + LineEnding;
  sz += 'will be fired only ONCE - every time' + LineEnding;
  sz += 'the interval that you set is reached.';
  AboutBoxDescription := sz;
  // AboutBoxBackgroundColor (TColor, like clWhite)
  // AboutBoxFontName (string)
  // AboutBoxFontSize (integer)
  AboutBoxVersion := '0.0.1';
  AboutBoxAuthorname := 'Gordon Bamber';
  // AboutBoxOrganisation (string)
  AboutBoxAuthorEmail := 'minesadorada@charcodelvalle.com';
  AboutBoxLicenseType := 'MODIFIEDGPL';// (string e.g. 'GPL', ModifiedGPL' etc

  fHourDone := False;
  fDayDone := False;
  fDateDone := False;
  fCurrentDateTime := Now;
  fLastFiredDateTime := Now;

  // Set defaults for properties
  fDate := 1;
  fSampleInterval := lt3Every10Minutes;
  Interval := 10 * C_OneMinute;
  fIntervalType := lt1Daily;

  fVersion := C_Version;
end;

procedure TLongTimer.DoOnIdle(Sender: TObject; var Done: boolean);
begin
  // Do nothing special here
  inherited;
end;

procedure TLongTimer.DoOnIdleEnd(Sender: TObject);
begin
  // Do nothing special here
  inherited;
end;

procedure TLongTimer.DoOnTimer;
// Only allow this event to fire ONCE if datetime matches the interval set
var
  cDay, cD, cM, cY, cH, cMi, cS, cms: word;
  lDay, lD, lM, lY, lH, lMi, lS, lms: word;
  fTempDate: word;
begin
  // Split Current date into parts
  fCurrentDateTime := Now;
  DecodeDate(fCurrentDateTime, cY, cM, cD);
  DecodeTime(fCurrentDateTime, cH, cMi, cS, cmS);
  cDay := DayOfTheMonth(fCurrentDateTime);

  // Split LastFired date into parts
  DecodeDate(fLastFiredDateTime, lY, lM, lD);
  DecodeTime(fLastFiredDateTime, lH, lMi, lS, lmS);
  lDay := DayOfTheMonth(fLastFiredDateTime);

  // New hour?
  if (fIntervalType = lt1Daily) then
    if (cH <> lH) then
      fHourDone := False;
  // New Day?
  if (fIntervalType = lt2Weekly) then
    if (cDay <> lDay) then
      begin
      fDayDone := False;
      fHourDone := False;
      end;
  // New Date?
  if (fIntervalType = lt3Monthly) then
    if (cD <> lD) then
      begin
      fDateDone := False;
      fHourDone := False;
      end;

  // Fire the OnSample event?
  if Assigned(fOnSample) then
    fOnSample(Self);

  // Only proceed further at specified interval in specified hour - else exit
  if (fIntervalType = lt1Daily) and ((fHourDone = True) or (cH <> fHour)) then
    Exit;
  if (fIntervalType = lt2Weekly) and ((fDayDone = True) or (cH <> fHour)) then
    Exit;
  if (fIntervalType = lt3Monthly) and ((fDateDone = True) or (cH <> fHour)) then
    Exit;

  // Fire the OnTimer event for the user
  inherited; // Do whatever the user wants done
  fLastFiredDateTime := Now;// Record the DateTime the OnTimer was fired

  // Now make sure it doesn't fire more than once when resampled

  // Deal with Months where fDate has been set to an invalid date
  // (i.e. 31st February)
  // Simply temporarily decrement the fDate until it is valid
  fTempDate := fDate;
  if (fIntervalType = lt3Monthly) then
    while not IsValidDate(cY, cM, fTempDate) do
      Dec(fTempDate);

  // If ltDaily, then fDayDone and fDateDone are always FALSE
  if (fIntervalType = lt1Daily) and (cH = fHour) then
    begin
    fHourDone := True;
    end;

  // If ltWeekly, then fHourDone and fDateDone are always FALSE
  // Set only if on Correct Weekday and at specified hour
  if (fIntervalType = lt2Weekly) and ((cDay = fDay) and (ch = fHour)) then
    begin
    fDayDone := True;
    fHourDone := True;
    end;

  // If ltMonthly, then fDayDone and fHourDone are always FALSE
  // Set only if Correct day of month and at specified hour
  if (fIntervalType = lt3Monthly) and ((cD = fTempDate) and (ch = fHour)) then
    begin
    fDateDone := True;
    fHourDone := True;
    end;
end;

procedure TLongTimer.SetSampleInterval(ASampleInterval: TSampleInterval);
var
  TimerEnabled: boolean;
begin
  if ASampleInterval = fSampleInterval then
    exit;
  // Temporarily disable running timer?
  TimerEnabled := Enabled;
  Enabled := False;
  case ASampleInterval of
    lt1Everyminute: Interval := C_OneMinute;
    lt2Every5minutes: Interval := 5 * C_OneMinute;
    lt3Every10Minutes: Interval := 10 * C_OneMinute;
    lt4Every30Minutes: Interval := 30 * C_OneMinute;
    lt5Every45Minutes: Interval := 45 * C_OneMinute;
    end;
  Enabled := TimerEnabled;
end;

procedure TLongTimer.SetDay(aDay: TDay);
var
  TimerEnabled: boolean;
begin
  if ADay = fTDay then
    Exit;
  // Temporarily disable running timer?
  TimerEnabled := Enabled;
  Enabled := False;
  fTDay := aDay;
  fDay := Ord(aDay) + 1;
  Enabled := TimerEnabled;
 {
  // ISO day numbers.
  DayMonday    = 1;
  DayTuesday   = 2;
  DayWednesday = 3;
  DayThursday  = 4;
  DayFriday    = 5;
  DaySaturday  = 6;
  DaySunday    = 7;
}
end;

procedure TLongTimer.SetDailyHour(aHour: word);
var
  TimerEnabled: boolean;
begin
  if fHour = aHour then
    Exit;
  // Temporarily disable running timer?
  TimerEnabled := Enabled;
  Enabled := False;
  if ((aHour >= 0) and (aHour <= 24)) then
    fHour := aHour;
  Enabled := TimerEnabled;
end;

procedure TLongTimer.SetMonthlyDate(ADate: word);
var
  TimerEnabled: boolean;
begin
  if ADate = fDate then
    Exit;
  // Temporarily disable running timer?
  TimerEnabled := Enabled;
  Enabled := False;
  if (fDate > 0) and (fDate < 32) then
    fDate := ADate;
  // Invalid dates like 31st Feb are dealt with in DoOnTimer
  // e.g. 31 stands for the last day in any month (inc Feb in a Leap Year)
  Enabled := TimerEnabled;
end;

end.
