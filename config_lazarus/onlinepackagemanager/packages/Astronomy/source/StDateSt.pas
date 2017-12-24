{**********************************************************************                   
 Package pl_Astronomy.pkg
 This unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}


unit StDateSt;

interface

uses
  LCLIntf, LCLType, LMessages, SysUtils,
  StConst,
  StUtils,
  StDate;

const
  {the following characters are meaningful in date Picture strings}
  MonthOnly = 'm';      {Formatting character for a date string picture mask}
  DayOnly = 'd';        {Formatting character for a date string picture mask}
  YearOnly = 'y';       {Formatting character for a date string picture mask}
  MonthOnlyU = 'M';     {Formatting character for a date string picture mask.
  Uppercase means pad with ' ' rather than '0'}
  DayOnlyU = 'D';       {Formatting character for a date string picture mask.
  Uppercase means pad with ' ' rather then '0'}
  DateSlash = '/';      {Formatting character for a date string picture mask}

  {'n'/'N' may be used in place of 'm'/'M' when the name of the month is
   desired instead of its number. E.g., 'dd/nnn/yyyy' -\> '01-Jan-1980'.
   'dd/NNN/yyyy' -\> '01-JAN-1980' (if SlashChar = '-'). The abbreviation used
   is based on the width of the subfield (3 in the example) and the current
   contents of the MonthString array.}
  NameOnly = 'n';       {Formatting character for a date string picture mask}
  NameOnlyU = 'N';      {Formatting character for a date string picture mask.
  Uppercase causes the output to be in uppercase}

  {'w'/'W' may be used to include the day of the week in a date string. E.g.,
  'www dd nnn yyyy' -\> 'Mon 01 Jan 1989'. The abbreviation used is based on
  the width of the subfield (3 in the example) and the current contents of the
  DayString array. Note that TurboPower Entry Fields will not allow the user to
  enter text into a subfield containing 'w' or 'W'. The day of the week will be
  supplied automatically when a valid date is entered.}
  WeekDayOnly = 'w';    {Formatting character for a date string picture mask}
  WeekDayOnlyU = 'W';   {Formatting character for a date string picture mask.
  Uppercase causes the output to be in uppercase}

  LongDateSub1 = 'f';   {Mask character used strictly for dealing with Window's
  long date format}
  LongDateSub2 = 'g';   {Mask character used strictly for dealing with Window's
  long date format}
  LongDateSub3 = 'h';   {Mask character used strictly for dealing with Window's
  long date format}

  HourOnly = 'h';       {Formatting character for a time string picture mask}
  MinOnly = 'm';        {Formatting character for a time string picture mask}
  SecOnly = 's';        {Formatting character for a time string picture mask}
  {if uppercase letters are used, numbers are padded with ' ' rather than '0'}
  HourOnlyU = 'H';      {Formatting character for a time string picture mask.
  Uppercase means pad with ' ' rather than '0'}
  MinOnlyU = 'M';       {Formatting character for a time string picture mask.
  Uppercase means pad with ' ' rather than '0'}
  SecOnlyU = 'S';       {Formatting character for a time string picture mask.
  Uppercase means pad with ' ' rather than '0'}
  {'hh:mm:ss tt' -\> '12:00:00 pm', 'hh:mmt' -\> '12:00p'}
  TimeOnly = 't';       {Formatting character for a time string picture mask.
  This generates 'AM' or 'PM'}
  TimeColon = ':';      {Formatting character for a time string picture mask}



function MonthToString(const Month: integer): string;
{-Return the month as a string}

function DayOfWeekToString(const WeekDay: TStDayType): string;
{-Return the day of the week specified by WeekDay as a string in Dest.}

function InternationalTime(ShowSeconds: boolean): string;
  {-Return a picture mask for a time string, based on Windows' international
    information}


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

var
  wLongDate: string;//[40];
  wldSub1: string[6];  //SZ: careful if converting to string; some code depends on sizeof (search for [*] around line 1021)
  wldSub2: string[6];
  wldSub3: string[6];
  wShortDate: string;//[31];
  w1159: string[7];
  w2359: string[7];
  wSlashChar: char;
  wColonChar: char;
  wTLZero: boolean;
  w12Hour: boolean;
  DefaultYear: integer;     {default year--used by DateStringToDMY}
  DefaultMonth: shortint;   {default month}

procedure AppendChar(var S: string; Ch: char);
begin
  SetLength(S, Succ(Length(S)));
  S[Length(S)] := Ch;
end;

function DayOfWeekToString(const WeekDay: TStDayType): string;
    {-Return the day of the week specified by WeekDay as a string in Dest.
    Will honor international names}
begin
  Result := LongDayNames[Ord(WeekDay) + 1];
end;

function MonthToString(const Month: integer): string;
  {-Return the month as a string. Will honor international names}
begin
  if (Month >= 1) and (Month <= 12) then
    Result := LongMonthNames[Month]
  else
    Result := '';
end;

function AstJulianDatePrim(Year, Month, Date: integer): double;
var
  A, B: integer;
begin
  if Month <= 2 then                                                  {!!.01}
  begin
    Dec(Year);
    Inc(Month, 12);
  end;
  A := Trunc(Year / 100);
  B := 2 - A + Trunc(A / 4);

  Result := Trunc(365.25 * (Year + 4716)) + Trunc(30.6001 * (Month + 1)) + Date + B - 1524.5;
end;


function MonthStringToMonth(const MSt: string; Width: byte): byte;{!!.02}
  {-Convert the month name in MSt to a month (1..12)}
var
  S: string;
  T: string;
  Len: byte;
  I: word;
begin
  S := UpperCase(MSt);
  Len := Length(S);
  //    SetLength(S,Width);
  //    if Width > Len then
  //      FillChar(S[Len+1], Length(S)-Len, ' ');
  S := S + StringOfChar(' ', Width - Len);

  for I := 1 to 12 do
  begin
    T := UpperCase(LongMonthNames[I]);
    Len := Length(T);
    //      SetLength(T,Width);
    //      if Width > Len then
    //        FillChar(T[Len+1], Length(T)-Len, ' ');
    T := T + StringOfChar(' ', Width - Len);

    if S = T then
    begin
      Result := I;
      Exit;
    end;
  end;
  Result := 0;
end;

function PackResult(const Picture, S: string): string;             {!!.02}
  {-Remove unnecessary blanks from S}
var
  step: integer;
begin
  Result := '';

  for step := 1 to Length(Picture) do
  begin
    case Picture[step] of
      MonthOnlyU, DayOnlyU, NameOnly, NameOnlyU, WeekDayOnly,
      WeekDayOnlyU, HourOnlyU, SecOnlyU:
        if S[step] <> ' ' then
          AppendChar(Result, S[Step]);
      TimeOnly:
        if S[step] <> ' ' then
          AppendChar(Result, S[step]);
      else
        AppendChar(Result, S[step]);
    end;
  end;
end;



function CurrentTime: TStTime;
  {-Returns current time in seconds since midnight}
begin
  Result := Trunc(SysUtils.Time * SecondsInDay);
end;


function InternationalTime(ShowSeconds: boolean): string;
  {-Return a picture mask for a time string, based on Windows' int'l info}
var
  ML, I: integer;
begin
  {format the default string}

  SetLength(Result, 21);
  Result := 'hh:mm:ss';
  if not wTLZero then
    Result[1] := HourOnlyU;

  {show seconds?}
  if not ShowSeconds then
    SetLength(Result, 5);

  {handle international AM/PM markers}
  if w12Hour then
  begin
    ML := Maxword(Length(w1159), Length(w2359));
    if (ML <> 0) then
    begin
      AppendChar(Result, ' ');
      for I := 1 to ML do
        AppendChar(Result, TimeOnly);
    end;
  end;
end;


end.
