{**********************************************************************                   
 Package pl_Astronomy.pkg
 This unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit StDate;

interface

uses
  LCLIntf, LCLType, LMessages, SysUtils;

type
  TStDate = longint;
    {In STDATE, dates are stored in long integer format as the number of days
    since January 1, 1600}

  TDateArray = array[0..(MaxLongInt div SizeOf(TStDate)) - 1] of TStDate;
  {Type for StDate open array}

  TStDayType = (Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday);
  {An enumerated type used when representing a day of the week}

  TStBondDateType = (bdtActual, bdt30E360, bdt30360, bdt30360psa);
  {An enumerated type used for calculating bond date differences}

  TStTime = longint;
    {STDATE handles time in a manner similar to dates, representing a given
    time of day as the number of seconds since midnight}

  TStDateTimeRec = record
     {This record type simply combines the two basic date types defined by
      STDATE, Date and Time}
    D: TStDate;
    T: TStTime;
  end;

const
  MinYear = 1600;        {Minimum valid year for a date variable}
  MaxYear = 3999;        {Maximum valid year for a date variable}
  Mindate = $00000000;   {Minimum valid date for a date variable - 01/01/1600}
  Maxdate = $000D6025;   {Maximum valid date for a date variable - 12/31/3999}
  Date1900: longint = $0001AC05;  {Julian date for 01/01/1900}
  Date1970: longint = $00020FE4;  {Julian date for 01/01/1970}
  Date1980: longint = $00021E28;  {Julian date for 01/01/1980}
  Date2000: longint = $00023AB1;  {Julian date for 01/01/2000}
  Days400Yr: longint = 146097;    {days in 400 years}
  {This value is used to represent an invalid date, such as 12/32/1992}
  BadDate = longint($FFFFFFFF);

  DeltaJD = $00232DA8;   {Days between 1/1/-4173 and 1/1/1600}

  MinTime = 0;          {Minimum valid time for a time variable - 00:00:00 am}
  MaxTime = 86399;      {Maximum valid time for a time variable - 23:59:59 pm}
  {This value is used to represent an invalid time of day, such as 12:61:00}
  BadTime = longint($FFFFFFFF);
  SecondsInDay = 86400;      {Number of seconds in a day}
  SecondsInHour = 3600;      {Number of seconds in an hour}
  SecondsInMinute = 60;      {Number of seconds in a minute}
  HoursInDay = 24;           {Number of hours in a day}
  MinutesInHour = 60;        {Number of minutes in an hour}
  MinutesInDay = 1440;       {Number of minutes in a day}

var
  DefaultYear: integer;     {default year--used by DateStringToDMY}
  DefaultMonth: shortint;   {default month}

{-------julian date routines---------------}

function CurrentDate: TStDate;
{-returns today's date as a Julian date}

function ValidDate(Day, Month, Year, Epoch: integer): boolean;
{-Verify that day, month, year is a valid date}

function DMYtoStDate(Day, Month, Year, Epoch: integer): TStDate;
{-Convert from day, month, year to a Julian date}

procedure StDateToDMY(Julian: TStDate; var Day, Month, Year: integer);
{-Convert from a Julian date to day, month, year}

function IncDate(Julian: TStDate; Days, Months, Years: integer): TStDate;
{-Add (or subtract) the number of days, months, and years to a date}

function IncDateTrunc(Julian: TStDate; Months, Years: integer): TStDate;
{-Add (or subtract) the specified number of months and years to a date}


function WeekOfYear(Julian: TStDate): byte;
{-Returns the week number of the year given the Julian Date}

function AstJulianDate(Julian: TStDate): double;
{-Returns the Astronomical Julian Date from a TStDate}

function AstJulianDatetoStDate(AstJulian: double; Truncate: boolean): TStDate;
  {-Returns a TStDate from an Astronomical Julian Date.
  Truncate TRUE   Converts to appropriate 0 hours then truncates
           FALSE  Converts to appropriate 0 hours, then rounds to
                  nearest;}

function AstJulianDatePrim(Year, Month, Date: integer; UT: TStTime): double;
  {-Returns an Astronomical Julian Date for any year, even those outside
    MinYear..MaxYear}

function DayOfWeek(Julian: TStDate): TStDayType;
{-Return the day of the week for a Julian date}

function DayOfWeekDMY(Day, Month, Year, Epoch: integer): TStDayType;
{-Return the day of the week for the day, month, year}

function IsLeapYear(Year: integer): boolean;
{-Return True if Year is a leap year}

function DaysInMonth(Month: integer; Year, Epoch: integer): integer;
{-Return the number of days in the specified month of a given year}

function ResolveEpoch(Year, Epoch: integer): integer;
{-Convert 2 digit year to 4 digit year according to Epoch}


{-------time routines---------------}

function ValidTime(Hours, Minutes, Seconds: integer): boolean;
{-Return True if Hours:Minutes:Seconds is a valid time}

procedure StTimeToHMS(T: TStTime; var Hours, Minutes, Seconds: byte);
{-Convert a time variable to hours, minutes, seconds}

function HMStoStTime(Hours, Minutes, Seconds: byte): TStTime;
{-Convert hours, minutes, seconds to a time variable}

function CurrentTime: TStTime;
{-Return the current time in seconds since midnight}

procedure TimeDiff(Time1, Time2: TStTime; var Hours, Minutes, Seconds: byte);
{-Return the difference in hours, minutes, and seconds between two times}

function IncTime(T: TStTime; Hours, Minutes, Seconds: byte): TStTime;
{-Add the specified hours, minutes, and seconds to a given time of day}

function DecTime(T: TStTime; Hours, Minutes, Seconds: byte): TStTime;
{-Subtract the specified hours, minutes, and seconds from a given time of day}

function RoundToNearestHour(T: TStTime; Truncate: boolean): TStTime;
  {-Given a time, round it to the nearest hour, or truncate minutes and
  seconds}

function RoundToNearestMinute(const T: TStTime; Truncate: boolean): TStTime;
{-Given a time, round it to the nearest minute, or truncate seconds}

{-------- routines for DateTimeRec records ---------}

procedure IncDateTime(const DT1: TStDateTimeRec; var DT2: TStDateTimeRec;  {!!.02}
  Days: integer; Secs: longint);
  {-Increment (or decrement) a date and time by the specified number of days
  and seconds}

function DateTimeToStDate(DT: TDateTime): TStDate;
{-Convert Delphi TDateTime to TStDate}

function DateTimeToStTime(DT: TDateTime): TStTime;
{-Convert Delphi TDateTime to TStTime}

function StDateToDateTime(D: TStDate): TDateTime;
{-Convert TStDate to TDateTime}

function StTimeToDateTime(T: TStTime): TDateTime;
{-Convert TStTime to TDateTime}

function Convert2ByteDate(TwoByteDate: word): TStDate;
{-Convert an Object Professional two byte date into a SysTools date}

function Convert4ByteDate(FourByteDate: TStDate): word;
{-Convert a SysTools date into an Object Professional two byte date}


implementation

const
  First2Months = 59;           {1600 was a leap year}
  FirstDayOfWeek = Saturday;   {01/01/1600 was a Saturday}
  DateLen = 40;                {maximum length of Picture strings}
  MaxMonthName = 15;
  MaxDayName = 15;


//type
{  DateString = string[DateLen];}
//  SString = string[255];

function IsLeapYear(Year: integer): boolean;
  {-Return True if Year is a leap year}
begin
  Result := (Year mod 4 = 0) and (Year mod 4000 <> 0) and ((Year mod 100 <> 0) or (Year mod 400 = 0));
end;

function IsLastDayofMonth(Day, Month, Year: integer): boolean;
  {-Return True if date is the last day in month}
var
  Epoch: integer;
begin
  Epoch := (Year div 100) * 100;
  if ValidDate(Day + 1, Month, Year, Epoch) then
    Result := False
  else
    Result := True;
end;

function IsLastDayofFeb(Date: TStDate): boolean;
  {-Return True if date is the last day in February}
var
  Day, Month, Year: integer;
begin
  StDateToDMY(Date, Day, Month, Year);
  if (Month = 2) and IsLastDayOfMonth(Day, Month, Year) then
    Result := True
  else
    Result := False;
end;


function ResolveEpoch(Year, Epoch: integer): integer;
  {-Convert 2-digit year to 4-digit year according to Epoch}
var
  EpochYear, EpochCent: integer;
begin
  if word(Year) < 100 then
  begin
    EpochYear := Epoch mod 100;
    EpochCent := (Epoch div 100) * 100;
    if (Year < EpochYear) then
      Inc(Year, EpochCent + 100)
    else
      Inc(Year, EpochCent);
  end;
  Result := Year;
end;

function CurrentDate: TStDate;
  {-Returns today's date as a julian}
var
  Year, Month, Date: word;
begin
  DecodeDate(Now, Year, Month, Date);
  Result := DMYToStDate(Date, Month, Year, 0);
end;

function DaysInMonth(Month: integer; Year, Epoch: integer): integer;
  {-Return the number of days in the specified month of a given year}
begin
  Year := ResolveEpoch(Year, Epoch);

  if (Year < MinYear) or (Year > MaxYear) then
  begin
    Result := 0;
    Exit;
  end;

  case Month of
    1, 3, 5, 7, 8, 10, 12:
      Result := 31;
    4, 6, 9, 11:
      Result := 30;
    2:
      Result := 28 + Ord(IsLeapYear(Year));
    else
      Result := 0;
  end;
end;

function ValidDate(Day, Month, Year, Epoch: integer): boolean;
  {-Verify that day, month, year is a valid date}
begin
  Year := ResolveEpoch(Year, Epoch);

  if (Day < 1) or (Year < MinYear) or (Year > MaxYear) then
    Result := False
  else
    case Month of
      1..12:
        Result := Day <= DaysInMonth(Month, Year, Epoch);
      else
        Result := False;
    end;
end;

function DMYtoStDate(Day, Month, Year, Epoch: integer): TStDate;
  {-Convert from day, month, year to a julian date}
begin
  Year := ResolveEpoch(Year, Epoch);

  if not ValidDate(Day, Month, Year, Epoch) then
    Result := BadDate
  else if (Year = MinYear) and (Month < 3) then
    if Month = 1 then
      Result := Pred(Day)
    else
      Result := Day + 30
  else
  begin
    if Month > 2 then
      Dec(Month, 3)
    else
    begin
      Inc(Month, 9);
      Dec(Year);
    end;
    Dec(Year, MinYear);
    Result :=
      ((longint(Year div 100) * Days400Yr) div 4) + ((longint(Year mod 100) * 1461) div 4) + (((153 * Month) + 2) div 5) + Day + First2Months;
  end;
end;

function WeekOfYear(Julian: TStDate): byte;
  {-Returns the week number of the year given the Julian Date}
var
  Day, Month, Year: integer;
  FirstJulian: TStDate;
begin
  if (Julian < MinDate) or (Julian > MaxDate) then
  begin
    Result := 0;
    Exit;
  end;

  Julian := Julian + 3 - ((6 + Ord(DayOfWeek(Julian))) mod 7);
  StDateToDMY(Julian, Day, Month, Year);
  FirstJulian := DMYToStDate(1, 1, Year, 0);
  Result := 1 + (Julian - FirstJulian) div 7;
end;

function AstJulianDate(Julian: TStDate): double;
  {-Returns the Astronomical Julian Date from a TStDate}
begin
  {Subtract 0.5d since Astronomical JD starts at noon
   while TStDate (with implied .0) starts at midnight}
  Result := Julian - 0.5 + DeltaJD;
end;


function AstJulianDatePrim(Year, Month, Date: integer; UT: TStTime): double;
var
  A, B: integer;
  LY, GC: boolean;

begin
  Result := -MaxLongInt;
  if (not (Month in [1..12])) or (Date < 1) then
    Exit
  else if (Month in [1, 3, 5, 7, 8, 10, 12]) and (Date > 31) then
    Exit
  else if (Month in [4, 6, 9, 11]) and (Date > 30) then
    Exit
  else if (Month = 2) then
  begin
    LY := IsLeapYear(Year);
    if ((LY) and (Date > 29)) or (not (LY) and (Date > 28)) then
      Exit;
  end
  else if ((UT < 0) or (UT >= SecondsInDay)) then
    Exit;

  if (Month <= 2) then
  begin
    Year := Year - 1;
    Month := Month + 12;
  end;
  A := abs(Year div 100);

  if (Year > 1582) then
    GC := True
  else if (Year = 1582) then
  begin
    if (Month > 10) then
      GC := True
    else if (Month < 10) then
      GC := False
    else
    begin
      if (Date >= 15) then
        GC := True
      else
        GC := False;
    end;
  end
  else
    GC := False;
  if (GC) then
    B := 2 - A + abs(A div 4)
  else
    B := 0;

  Result := Trunc(365.25 * (Year + 4716)) + Trunc(30.6001 * (Month + 1)) + Date + B - 1524.5 + UT / SecondsInDay;
end;


function AstJulianDatetoStDate(AstJulian: double; Truncate: boolean): TStDate;
  {-Returns a TStDate from an Astronomical Julian Date.
    Truncate TRUE   Converts to appropriate 0 hours then truncates
             FALSE  Converts to appropriate 0 hours, then rounds to
                    nearest;}
begin
  {Convert to TStDate, adding 0.5d for implied .0d of TStDate}
  AstJulian := AstJulian + 0.5 - DeltaJD;
  if (AstJulian < MinDate) or (AstJulian > MaxDate) then
  begin
    Result := BadDate;
    Exit;
  end;

  if Truncate then
    Result := Trunc(AstJulian)
  else
    Result := Trunc(AstJulian + 0.5);
end;

procedure StDateToDMY(Julian: TStDate; var Day, Month, Year: integer);
{-Convert from a julian date to month, day, year}
var
  I, J: longint;
begin
  if Julian = BadDate then
  begin
    Day := 0;
    Month := 0;
    Year := 0;
  end
  else if Julian <= First2Months then
  begin
    Year := MinYear;
    if Julian <= 30 then
    begin
      Month := 1;
      Day := Succ(Julian);
    end
    else
    begin
      Month := 2;
      Day := Julian - 30;
    end;
  end
  else
  begin
    I := (4 * longint(Julian - First2Months)) - 1;

    J := (4 * ((I mod Days400Yr) div 4)) + 3;
    Year := (100 * (I div Days400Yr)) + (J div 1461);
    I := (5 * (((J mod 1461) + 4) div 4)) - 3;
    Day := ((I mod 153) + 5) div 5;

    Month := I div 153;
    if Month < 10 then
      Inc(Month, 3)
    else
    begin
      Dec(Month, 9);
      Inc(Year);
    end;
    Inc(Year, MinYear);
  end;
end;

function IncDate(Julian: TStDate; Days, Months, Years: integer): TStDate;
  {-Add (or subtract) the number of months, days, and years to a date.
    Months and years are added before days. No overflow/underflow
    checks are made}
var
  Day, Month, Year, Day28Delta: integer;
begin
  StDateToDMY(Julian, Day, Month, Year);
  Day28Delta := Day - 28;
  if Day28Delta < 0 then
    Day28Delta := 0
  else
    Day := 28;

  Inc(Year, Years);
  Inc(Year, Months div 12);
  Inc(Month, Months mod 12);
  if Month < 1 then
  begin
    Inc(Month, 12);
    Dec(Year);
  end
  else if Month > 12 then
  begin
    Dec(Month, 12);
    Inc(Year);
  end;

  Julian := DMYtoStDate(Day, Month, Year, 0);
  if Julian <> BadDate then
  begin
    Inc(Julian, Days);
    Inc(Julian, Day28Delta);
  end;
  Result := Julian;
end;

function IncDateTrunc(Julian: TStDate; Months, Years: integer): TStDate;
  {-Add (or subtract) the specified number of months and years to a date}
var
  Day, Month, Year: integer;
  MaxDay, Day28Delta: integer;
begin
  StDateToDMY(Julian, Day, Month, Year);
  Day28Delta := Day - 28;
  if Day28Delta < 0 then
    Day28Delta := 0
  else
    Day := 28;

  Inc(Year, Years);
  Inc(Year, Months div 12);
  Inc(Month, Months mod 12);
  if Month < 1 then
  begin
    Inc(Month, 12);
    Dec(Year);
  end
  else if Month > 12 then
  begin
    Dec(Month, 12);
    Inc(Year);
  end;

  Julian := DMYtoStDate(Day, Month, Year, 0);
  if Julian <> BadDate then
  begin
    MaxDay := DaysInMonth(Month, Year, 0);
    if Day + Day28Delta > MaxDay then
      Inc(Julian, MaxDay - Day)
    else
      Inc(Julian, Day28Delta);
  end;
  Result := Julian;
end;

function DayOfWeek(Julian: TStDate): TStDayType;
  {-Return the day of the week for the date. Returns TStDayType(7) if Julian =
    BadDate.}
var
  B: byte;
begin
  if Julian = BadDate then
  begin
    B := 7;
    Result := TStDayType(B);
  end
  else
    Result := TStDayType((Julian + Ord(FirstDayOfWeek)) mod 7);
end;

function DayOfWeekDMY(Day, Month, Year, Epoch: integer): TStDayType;
  {-Return the day of the week for the day, month, year}
begin
  Result := DayOfWeek(DMYtoStDate(Day, Month, Year, Epoch));
end;

procedure StTimeToHMS(T: TStTime; var Hours, Minutes, Seconds: byte);
{-Convert a Time variable to Hours, Minutes, Seconds}
begin
  if T = BadTime then
  begin
    Hours := 0;
    Minutes := 0;
    Seconds := 0;
  end
  else
  begin
    Hours := T div SecondsInHour;
    Dec(T, longint(Hours) * SecondsInHour);
    Minutes := T div SecondsInMinute;
    Dec(T, longint(Minutes) * SecondsInMinute);
    Seconds := T;
  end;
end;

function HMStoStTime(Hours, Minutes, Seconds: byte): TStTime;
  {-Convert Hours, Minutes, Seconds to a Time variable}
var
  T: TStTime;
begin
  Hours := Hours mod HoursInDay;
  T := (longint(Hours) * SecondsInHour) + (longint(Minutes) * SecondsInMinute) + Seconds;
  Result := T mod SecondsInDay;
end;

function ValidTime(Hours, Minutes, Seconds: integer): boolean;
  {-Return true if Hours:Minutes:Seconds is a valid time}
begin
  if (Hours < 0) or (Hours > 23) or (Minutes < 0) or (Minutes >= 60) or (Seconds < 0) or (Seconds >= 60) then
    Result := False
  else
    Result := True;
end;

function CurrentTime: TStTime;
  {-Returns current time in seconds since midnight}
begin
  Result := Trunc(SysUtils.Time * SecondsInDay);
end;

procedure TimeDiff(Time1, Time2: TStTime; var Hours, Minutes, Seconds: byte);
{-Return the difference in hours,minutes,seconds between two times}
begin
  StTimeToHMS(Abs(Time1 - Time2), Hours, Minutes, Seconds);
end;

function IncTime(T: TStTime; Hours, Minutes, Seconds: byte): TStTime;
  {-Add the specified hours,minutes,seconds to T and return the result}
begin
  Inc(T, HMStoStTime(Hours, Minutes, Seconds));
  Result := T mod SecondsInDay;
end;

function DecTime(T: TStTime; Hours, Minutes, Seconds: byte): TStTime;
  {-Subtract the specified hours,minutes,seconds from T and return the result}
begin
  Hours := Hours mod HoursInDay;
  Dec(T, HMStoStTime(Hours, Minutes, Seconds));
  if T < 0 then
    Result := T + SecondsInDay
  else
    Result := T;
end;

function RoundToNearestHour(T: TStTime; Truncate: boolean): TStTime;
  {-Round T to the nearest hour, or Truncate minutes and seconds from T}
var
  Hours, Minutes, Seconds: byte;
begin
  StTimeToHMS(T, Hours, Minutes, Seconds);
  Seconds := 0;
  if not Truncate then
    if Minutes >= (MinutesInHour div 2) then
      Inc(Hours);
  Minutes := 0;
  Result := HMStoStTime(Hours, Minutes, Seconds);
end;

function RoundToNearestMinute(const T: TStTime; Truncate: boolean): TStTime;
  {-Round T to the nearest minute, or Truncate seconds from T}
var
  Hours, Minutes, Seconds: byte;
begin
  StTimeToHMS(T, Hours, Minutes, Seconds);
  if not Truncate then
    if Seconds >= (SecondsInMinute div 2) then
      Inc(Minutes);
  Seconds := 0;
  Result := HMStoStTime(Hours, Minutes, Seconds);
end;



function DateTimeToStDate(DT: TDateTime): TStDate;
  {-Convert Delphi TDateTime to TStDate}
var
  Day, Month, Year: word;
begin
  DecodeDate(DT, Year, Month, Day);
  Result := DMYToStDate(Day, Month, Year, 0);
end;

function DateTimeToStTime(DT: TDateTime): TStTime;
  {-Convert Delphi TDateTime to TStTime}
var
  Hour, Min, Sec, MSec: word;
begin
  DecodeTime(DT, Hour, Min, Sec, MSec);
  Result := HMSToStTime(Hour, Min, Sec);
end;

function StDateToDateTime(D: TStDate): TDateTime;
  {-Convert TStDate to TDateTime}
var
  Day, Month, Year: integer;
begin
  Result := 0;
  if D <> BadDate then
  begin
    StDateToDMY(D, Day, Month, Year);
    Result := EncodeDate(Year, Month, Day);
  end;
end;

function StTimeToDateTime(T: TStTime): TDateTime;
  {-Convert TStTime to TDateTime}
var
  Hour, Min, Sec: byte;
begin
  Result := 0;
  if T <> BadTime then
  begin
    StTimeToHMS(T, Hour, Min, Sec);
    Result := EncodeTime(Hour, Min, Sec, 0);
  end;
end;

procedure IncDateTime(const DT1: TStDateTimeRec; var DT2: TStDateTimeRec; {!!.02}
  Days: integer; Secs: longint);
  {-Increment (or decrement) DT1 by the specified number of days and seconds
    and put the result in DT2}
begin
  DT2 := DT1;

  {date first}
  Inc(DT2.D, longint(Days));

  if Secs < 0 then
  begin
    {change the sign}
    Secs := -Secs;

    {adjust the date}
    Dec(DT2.D, Secs div SecondsInDay);
    Secs := Secs mod SecondsInDay;

    if Secs > DT2.T then
    begin
      {subtract a day from DT2.D and add a day's worth of seconds to DT2.T}
      Dec(DT2.D);
      Inc(DT2.T, SecondsInDay);
    end;

    {now subtract the seconds}
    Dec(DT2.T, Secs);
  end
  else
  begin
    {increment the seconds}
    Inc(DT2.T, Secs);

    {adjust date if necessary}
    Inc(DT2.D, DT2.T div SecondsInDay);

    {force time to 0..SecondsInDay-1 range}
    DT2.T := DT2.T mod SecondsInDay;
  end;
end;

function Convert2ByteDate(TwoByteDate: word): TStDate;
begin
  Result := longint(TwoByteDate) + Date1900;
end;

function Convert4ByteDate(FourByteDate: TStDate): word;
begin
  Result := word(FourByteDate - Date1900);
end;

procedure SetDefaultYear;
{-Initialize DefaultYear and DefaultMonth}
var
  Month, Day, Year: word;
  T: TDateTime;
begin
  T := Now;
  DecodeDate(T, Year, Month, Day);
  DefaultYear := Year;
  DefaultMonth := Month;
end;


initialization
  {initialize DefaultYear and DefaultMonth}
  SetDefaultYear;
end.
