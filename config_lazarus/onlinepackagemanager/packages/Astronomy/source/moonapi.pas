{**********************************************************************                   
 Package pl_Astronomy.pkg
 This unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit MoonAPI;

interface

uses
  MoonMath,
  MoonVSOP,
  sysutils;

type
  TMoonPhase=(Newmoon,WaxingCrescrent,FirstQuarter,WaxingGibbous,
              Fullmoon,WaningGibbous,LastQuarter,WaningCrescent);
  TSeason=(Winter,Spring,Summer,Autumn);
  TEclipse=(none, partial, noncentral, circular, circulartotal, total, halfshadow);
  E_NoRiseSet=class(Exception);
  E_OutOfAlgorithmRange=class(Exception);
  TSolarTerm=(st_z2,st_j3,st_z3,st_j4,st_z4,st_j5,st_z5,st_j6,st_z6,
              st_j7,st_z7,st_j8,st_z8,st_j9,st_z9,st_j10,st_z10,
              st_j11,st_z11,st_j12,st_z12,st_j1,st_z1,st_j2);
  TChineseZodiac=(ch_rat,ch_ox,ch_tiger,ch_rabbit,ch_dragon,ch_snake,
                      ch_horse,ch_goat,ch_monkey,ch_chicken,ch_dog,ch_pig);
  TChineseStem=(ch_jia,ch_yi,ch_bing,ch_ding,ch_wu,ch_ji,
                    ch_geng,ch_xin,ch_ren,ch_gui);

  TChineseCycle=record
    zodiac: TChineseZodiac;
    stem:   TChineseStem;
    end;

  TChineseDate = record
    cycle: integer;
    year: integer;
    epoch_years: integer;
    month: integer;
    leap: boolean;
    leapyear: boolean;
    day: integer;
    yearcycle: TChineseCycle;
    daycycle: TChineseCycle;
    monthcycle: TChineseCycle;
    end;

const
  // Date of calendar reformation - start of gregorian calendar
  calendar_change_standard: extended = 2299160.5;
  calendar_change_russia: extended = 2421638.5;
  calendar_change_england: extended = 2361221.5;
  calendar_change_sweden: extended = 2361389.5;
  // Jewish_Month_Name:array[1..13] of string
  Jewish_Month_Name:array[1..13] of string = (
    'Nisan',
    'Iyar',
    'Sivan',
    'Tammuz',
    'Av',
    'Elul',
    'Tishri',
    'Heshvan',
    'Kislev',
    'Tevet',
    'Shevat',
    'Adar',
    'Adar 2'
    );


{ Calendar algorithms }
function julian_date(date:TDateTime):extended;
function delphi_date(juldat:extended):TDateTime;
function EasterDate(year:integer):TDateTime;
function EasterDateJulian(year:integer):TDateTime;
function PesachDate(year:integer):TDateTime;
procedure DecodeDateJewish(date: TDateTime; var year,month,day: word);
function EncodeDateJewish(year,month,day: word):TDateTime;
function WeekNumber(date:TDateTime):integer;

{ Convert date to julian date and back }
function Calc_Julian_date_julian(year,month,day:word):extended;
function Calc_Julian_date_gregorian(year,month,day:word):extended;
function Calc_Julian_date_switch(year,month,day:word; switch_date:extended):extended;
function Calc_Julian_date(year,month,day:word):extended;
procedure Calc_Calendar_date_julian(juldat:extended; var year,month,day:word);
procedure Calc_Calendar_date_gregorian(juldat:extended; var year,month,day:word);
procedure Calc_Calendar_date_switch(juldat:extended; var year,month,day:word; switch_date:extended);
procedure Calc_Calendar_date(juldat:extended; var year,month,day:word);

{ corrected TDateTime functions }
function isleapyearcorrect(year:word):boolean;
function EncodedateCorrect(year,month,day: word):TDateTime;
procedure DecodedateCorrect(date:TDateTime; var year,month,day: word);
procedure DecodetimeCorrect(date:TDateTime; var hour,min,sec,msec: word);
function FalsifyTdateTime(date:TDateTime):TdateTime;

{ Sun and Moon }
function sun_distance(date:TDateTime): extended;
function moon_distance(date:TDateTime): extended;
function age_of_moon(date:TDateTime): extended;

function last_phase(date:TDateTime; phase:TMoonPhase):TDateTime;
function next_phase(date:TDateTime; phase:TMoonPhase):TDateTime;
function nearest_phase(date: TDateTime):TMoonPhase;
function next_blue_moon(date: TDateTime):TDateTime;
function is_blue_moon(lunation: integer):boolean;

function moon_phase_angle(date: TDateTime):extended;
function current_phase(date:TDateTime):extended;
function lunation(date:TDateTime):integer;

function sun_diameter(date:TDateTime):extended;
function moon_diameter(date:TDateTime):extended;

function Sun_Rise(date:TDateTime; latitude, longitude:extended):TDateTime;
function Sun_Set(date:TDateTime; latitude, longitude:extended):TDateTime;
function Sun_Transit(date:TDateTime; latitude, longitude:extended):TDateTime;
function Morning_Twilight_Civil(date:TDateTime; latitude, longitude:extended):TDateTime;
function Evening_Twilight_Civil(date:TDateTime; latitude, longitude:extended):TDateTime;
function Morning_Twilight_Nautical(date:TDateTime; latitude, longitude:extended):TDateTime;
function Evening_Twilight_Nautical(date:TDateTime; latitude, longitude:extended):TDateTime;
function Morning_Twilight_Astronomical(date:TDateTime; latitude, longitude:extended):TDateTime;
function Evening_Twilight_Astronomical(date:TDateTime; latitude, longitude:extended):TDateTime;
function Moon_Rise(date:TDateTime; latitude, longitude:extended):TDateTime;
function Moon_Set(date:TDateTime; latitude, longitude:extended):TDateTime;
function Moon_Transit(date:TDateTime; latitude, longitude:extended):TDateTime;

function nextperigee(date:TDateTime):TDateTime;
function nextapogee(date:TDateTime):TDateTime;
function nextperihel(date:TDateTime):TDateTime;
function nextaphel(date:TDateTime):TDateTime;

function NextEclipse(var date:TDateTime; sun:boolean):TEclipse;

procedure Moon_Position_Horizontal(date:TdateTime; longitude,latitude: extended; var elevation,azimuth: extended);
procedure Sun_Position_Horizontal(date:TdateTime; longitude,latitude: extended; var elevation,azimuth: extended);

{ Further useful functions }
function star_time(date:TDateTime):extended;
function StartSeason(year: integer; season:TSeason):TDateTime;
function CalcSolarTerm(year: integer; term: TSolarTerm):TDateTime;

{ Chinese calendar }
function ChineseNewYear(year: integer): TDateTime;
function ChineseDate(date: TdateTime): TChineseDate;
function EncodeDateChinese(date:TChineseDate):TDateTime;

implementation


const
  AU=149597869;             (* astronomical unit in km *)
  mean_lunation=29.530589;  (* Mean length of a month *)
  tropic_year=365.242190;   (* Tropic year length *)
  earth_radius=6378.15;     (* Radius of the earth *)

var
  (* Shortcuts to avoid calling Encodedate too often *)
  datetime_2000_01_01: extended = 0;
  datetime_1999_01_01: extended = 0;
  datetime_chinese_epoch: extended = 0;
  datetime_first_lunation: extended = 0;
  julian_offset: extended = 0;
  (* How broken is the TDateTime? *)
  negative_dates_broken: boolean = false;
  calendar_reform_supported: boolean = true;
  julian_calendar_before_1582: boolean = true;

const
  beijing_longitude = -(116+25/60);

type

  t_coord = record
    longitude, latitude, radius: extended; (* lambda, beta, R *)
    rektaszension, declination: extended;  (* alpha, delta *)
    parallax: extended;
    elevation, azimuth: extended;          (* h, A *)
    end;

  T_RiseSet=(_rise,_set,_transit,_rise_civil,_rise_nautical,_rise_astro,_set_civil,_set_nautical,_set_astro);
  TJewishYearStyle=(ys_common_deficient,ys_common_regular,ys_common_complete,
                    ys_leap_deficient,ys_leap_regular,ys_leap_complete);
const

  Jewish_Month_length:array[1..13,TJewishYearStyle] of word = (
   ( 30,30,30,30,30,30),
   ( 29,29,29,29,29,29),
   ( 30,30,30,30,30,30),
   ( 29,29,29,29,29,29),
   ( 30,30,30,30,30,30),
   ( 29,29,29,29,29,29),
   ( 30,30,30,30,30,30),
   ( 29,29,30,29,29,30),
   ( 29,30,30,29,30,30),
   ( 29,29,29,29,29,29),
   ( 30,30,30,30,30,30),
   ( 29,29,29,30,30,30),
   (  0, 0, 0,29,29,29)
   );

  Jewish_Month_Name_short:array[1..13] of string = (
    'Nis',
    'Iya',
    'Siv',
    'Tam',
    'Av' ,
    'Elu',
    'Tis',
    'Hes',
    'Kis',
    'Tev',
    'She',
    'Ada',
    'Ad2'
    );

  Jewish_year_length:array[TJewishYearStyle] of integer = (353,354,355,383,384,385);

{ Julian date }

function julian_date(date:TDateTime):extended;
begin
  julian_date:=julian_offset+date
  end;

function delphi_date(juldat:extended):TDateTime;
begin
  delphi_date:=juldat-julian_offset;
  end;

function isleapyearcorrect(year:word):boolean;
begin
  if year<=1582 then
    result:=((year mod 4)=0)
  else
    result:=(((year mod 4)=0) and ((year mod 100)<>0)) or
             ((year mod 400)=0);
  end;

function Calc_Julian_date_julian(year,month,day:word):extended;
begin
  if (year<1) or (year>9999) then
    raise EConvertError.Create('Invalid year');
  if month<3 then begin
    month:=month+12;
    year:=year-1;
    end;
  case month of
    3,5,7,8,10,12,13: if (day<1) or (day>31) then EConvertError.Create('Invalid day');
    4,6,9,11:         if (day<1) or (day>30) then EConvertError.Create('Invalid day');
    14: case day of
          1..28: ;
          29: if (year+1) mod 4<>0 then EConvertError.Create('Invalid day');
          else EConvertError.Create('Invalid day');
          end;
     else raise EConvertError.Create('Invalid month');
     end;
  result:=trunc(365.25*(year+4716))+trunc(30.6001*(month+1))+day-1524.5;
  end;

function Calc_Julian_date_gregorian(year,month,day:word):extended;
var
  a,b: longint;
begin
  if (year<1) or (year>9999) then
    raise EConvertError.Create('Invalid year');
  if month<3 then begin
    month:=month+12;
    year:=year-1;
    end;
  a:=year div 100;
  case month of
    3,5,7,8,10,12,13: if (day<1) or (day>31) then EConvertError.Create('Invalid day');
    4,6,9,11:         if (day<1) or (day>30) then EConvertError.Create('Invalid day');
    14: case day of
          1..28: ;
          29: if (((year mod 4)<>0) or ((year mod 100)=0)) and
                 ((year mod 400)<>0) then EConvertError.Create('Invalid day');
          else EConvertError.Create('Invalid day');
          end;
     else raise EConvertError.Create('Invalid month');
     end;
  b:=2-a+(a div 4);
  result:=trunc(365.25*(year+4716))+trunc(30.6001*(month+1))+day+b-1524.5;
  end;

function Calc_Julian_date_switch(year,month,day:word; switch_date:extended):extended;
begin
  result:=Calc_Julian_date_julian(year,month,day);
  if result>=switch_date then begin
    result:=Calc_Julian_date_gregorian(year,month,day);
    if result<switch_date then
      raise EConvertError.Create('Date invalid due to calendar change');
    end;
  end;

function Calc_Julian_date(year,month,day:word):extended;
begin
  result:=Calc_Julian_date_switch(year,month,day,calendar_change_standard);
  end;

procedure Calc_Calendar_date_julian(juldat:extended; var year,month,day:word);
var
  z,a,b,c,d,e: longint;
begin
  if juldat<0 then
    raise EConvertError.Create('Negative julian dates not supported');
  juldat:=juldat+0.5;
  z:=trunc(juldat);
  a:=z;
  b:=a+1524;
  c:=trunc((b-122.1)/365.25);
  d:=trunc(365.25*c);
  e:=trunc((b-d)/30.6001);
  day:=b-d-trunc(30.6001*e);
  year:=c-4716;
  if e<14 then
    month:=e-1
  else begin
    month:=e-13;
    year:=year+1;
    end;
  end;

procedure Calc_Calendar_date_gregorian(juldat:extended; var year,month,day:word);
var
  alpha,z,a,b,c,d,e: longint;
begin
  if juldat<0 then
    raise EConvertError.Create('Negative julian dates not supported');
  juldat:=juldat+0.5;
  z:=trunc(juldat);
  alpha:=trunc((z-1867216.25)/36524.25);
  a:=z+1+alpha-trunc(alpha/4);
  b:=a+1524;
  c:=trunc((b-122.1)/365.25);
  d:=trunc(365.25*c);
  e:=trunc((b-d)/30.6001);
  day:=b-d-trunc(30.6001*e);
  year:=c-4716;
  if e<14 then
    month:=e-1
  else begin
    month:=e-13;
    year:=year+1;
    end;
  end;

procedure Calc_Calendar_date_switch(juldat:extended; var year,month,day:word; switch_date:extended);
begin
  if juldat<0 then
    raise EConvertError.Create('Negative julian dates not supported');
  if juldat<switch_date then
    Calc_Calendar_date_julian(juldat,year,month,day)
  else
    Calc_Calendar_date_gregorian(juldat,year,month,day);
  end;

procedure Calc_Calendar_date(juldat:extended; var year,month,day:word);
begin
  Calc_Calendar_date_switch(juldat,year,month,day,calendar_change_standard);
  end;

{ TDateTime correction }
(* Check how many bugs the TDateTime has compare to julian date *)
procedure check_TDatetime;
var 
  d1,d2: TDateTime;
begin
  d1:=EncodeDate(1582,10,15);
  d2:=EncodeDate(1582,10,4);
  calendar_reform_supported:=((d1-d2)=1);
  d1:=EncodeDate(1500,3,1);
  d2:=EncodeDate(1500,2,28);
  julian_calendar_before_1582:=((d1-d2)=2);
  end;

function EncodedateCorrect(year,month,day: word):TDateTime;
begin
  result:=delphi_date(Calc_Julian_date(year,month,day));
  end;

procedure DecodedateCorrect(date:TDateTime; var year,month,day: word);
begin
  Calc_Calendar_date(julian_date(date),year,month,day);
  end;

procedure DecodetimeCorrect(date:TDateTime; var hour,min,sec,msec: word);
begin
  Decodetime(1+frac(date),hour,min,sec,msec);
  end;

function FalsifyTdateTime(date:TDateTime):TdateTime;
var
  d,m,y: word;
begin
  DecodedateCorrect(date,d,m,y);
  result:=Encodedate(d,m,y);
  result:=result+frac(date);
  if negative_dates_broken and (result<0) and (frac(result)<>0) then
    result:=int(result)-(1-abs(frac(result)));
  end;

{ Calendar functions }

function WeekNumber(date:TDateTime):integer;
var
  y,m,d: word;
  h: integer;
  FirstofJanuary,
  FirstThursday,
  FirstWeekStart: TDateTime;
begin
  DecodedateCorrect(date,y,m,d);
  FirstofJanuary:=EncodedateCorrect(y,1,1);
  h:=dayOfWeek(FirstofJanuary);
  FirstThursday:=FirstofJanuary+((12-h) mod 7);
  FirstWeekStart:=FirstThursday-3;
  if trunc(date)<FirstWeekStart then
    result:=WeekNumber(FirstofJanuary-1) (* 12-31 of previous year *)
  else
    result:=(round(trunc(date)-FirstWeekStart) div 7)+1;
  end;

function EasterDateGregorian(year:integer):TDateTime;
var
  a,b,c,d,e,m,n,day,month: integer;
begin
  case year of
    1583..1699:  begin  m:=22; n:=2;  end;
    1700..1799:  begin  m:=23; n:=3;  end;
    1800..1899:  begin  m:=23; n:=4;  end;
    1900..2099:  begin  m:=24; n:=5;  end;
    2100..2199:  begin  m:=24; n:=6;  end;
    2200..2399:  begin  m:=25; n:=0;  end;
    else raise E_OutOfAlgorithmRange.Create('Out of range of the algorithm');
    end;
  a:=year mod 19;
  b:=year mod 4;
  c:=year mod 7;
  d:=(19*a+m) mod 30;
  e:=(2*b+4*c+6*d+n) mod 7;
  day:=(22+d+e);
  if day<=31 then
    month:=3
  else begin
    day:=(d+e-9);
    month:=4;
    end;
  if (day=26) and (month=4) then  day:=19;
  if (day=25) and (month=4) and (d=28) and (e=6) and (a>10) then  day:=18;
  result:=EncodedateCorrect(year,month,day);
  end;

function EasterDate(year:integer):TDateTime;
begin
  if year<1583 then
    result:=EasterDateJulian(year)
  else
    result:=EasterDateGregorian(year);
  end;

function EasterDateJulian(year:integer):TDateTime;
var
  a,b,c,d,e,f,g: integer;
begin
  a:=year mod 4;
  b:=year mod 7;
  c:=year mod 19;
  d:=(19*c+15) mod 30;
  e:=(2*a+4*b-d+34) mod 7;
  f:=(d+e+114) div 31;
  g:=(d+e+114) mod 31;
  result:=EncodedateCorrect(year,f,g+1);
  end;

function PesachDate(year:integer):TDateTime;
var
  a,b,c,d,j,s: integer;
  q,r: extended;
begin
  if year<359 then
    raise E_OutOfAlgorithmRange.Create('Out of range of the algorithm');
  c:=year div 100;
  if year<1583 then
    s:=0
  else
    s:=(3*c-5) div 4;
  a:=(12*year+12) mod 19;
  b:=year mod 4;
  q:=-1.904412361576+1.554241796621*a+0.25*b-0.003177794022*year+s;
  j:=(trunc(q)+3*year+5*b+2-s) mod 7;
  r:=frac(q);
  if false then
  else if j in [2,4,6] then
    d:=trunc(q)+23
  else if (j=1) and (a>6) and (r>=0.632870370) then
    d:=trunc(q)+24
  else if (j=0) and (a>11) and (r>=0.897723765) then
    d:=trunc(q)+23
  else
    d:=trunc(q)+22;

  if d>31 then
    result:=EncodedateCorrect(year,4,d-31)
  else
    result:=EncodedateCorrect(year,3,d);
  end;

function JewishYearStyle(year:word):TJewishYearStyle;
var
  i: TJewishYearStyle;
  yearlength: integer;
begin
  yearlength:=round(pesachdate(year-3760)-pesachdate(year-3761));
  result:=low(TJewishYearStyle);
  for i:=low(TJewishYearStyle) to high(TJewishYearStyle) do
    if yearlength=Jewish_year_length[i] then
      result:=i;
  end;

function EncodeDateJewish(year,month,day: word):TDateTime;
var
  yearstyle: TJewishYearStyle;
  offset,i: integer;
begin
  yearstyle:=JewishYearStyle(year);
  if (month<1) or (month>13) then
    raise EConvertError.Create('Invalid month');
  if (month=13) and
     (yearstyle in [ys_common_deficient,ys_common_regular,ys_common_complete]) then
    raise EConvertError.Create('Invalid month');
  if (day<1) or (day>Jewish_Month_length[month,yearstyle]) then
    raise EConvertError.Create('Invalid day');
  offset:=day-1;
  (* count months from tishri *)
  month:=(month+6) mod 13 +1;
  for i:=1 to month-1 do
    offset:=offset+Jewish_Month_length[(i+5) mod 13 +1,yearstyle];
  result:=pesachdate(year-3761)+163+offset;
  end;

procedure DecodeDateJewish(date: TDateTime; var year,month,day: word);
var
  year_g,month_g,day_g: word;
  yearstyle: TJewishYearStyle;
  tishri1: TDateTime;
begin
  DecodedateCorrect(date,year_g,month_g,day_g);
  tishri1:=pesachdate(year_g)+163;
  if tishri1>date then begin
    tishri1:=pesachdate(year_g-1)+163;
    year:=year_g+3760;
    end
  else
    year:=year_g+3761;
  yearstyle:=JewishYearStyle(year);
  month:=7;
  day:=round(date-tishri1+1);
  while day>Jewish_Month_length[month,yearstyle] do begin
    dec(day,Jewish_Month_length[month,yearstyle]);
    month:=(month mod 13) +1;
    end;
  end;


{ Misc }
procedure calc_epsilon_phi(date:TDateTime; var delta_phi,epsilon:extended);

const

  arg_mul:array[0..30,0..4] of shortint = (
     ( 0, 0, 0, 0, 1),
     (-2, 0, 0, 2, 2),
     ( 0, 0, 0, 2, 2),
     ( 0, 0, 0, 0, 2),
     ( 0, 1, 0, 0, 0),
     ( 0, 0, 1, 0, 0),
     (-2, 1, 0, 2, 2),
     ( 0, 0, 0, 2, 1),
     ( 0, 0, 1, 2, 2),
     (-2,-1, 0, 2, 2),
     (-2, 0, 1, 0, 0),
     (-2, 0, 0, 2, 1),
     ( 0, 0,-1, 2, 2),
     ( 2, 0, 0, 0, 0),
     ( 0, 0, 1, 0, 1),
     ( 2, 0,-1, 2, 2),
     ( 0, 0,-1, 0, 1),
     ( 0, 0, 1, 2, 1),
     (-2, 0, 2, 0, 0),
     ( 0, 0,-2, 2, 1),
     ( 2, 0, 0, 2, 2),
     ( 0, 0, 2, 2, 2),
     ( 0, 0, 2, 0, 0),
     (-2, 0, 1, 2, 2),
     ( 0, 0, 0, 2, 0),
     (-2, 0, 0, 2, 0),
     ( 0, 0,-1, 2, 1),
     ( 0, 2, 0, 0, 0),
     ( 2, 0,-1, 0, 1),
     (-2, 2, 0, 2, 2),
     ( 0, 1, 0, 0, 1)
                   );

  arg_phi:array[0..30,0..1] of longint = (
     (-171996,-1742),
     ( -13187,  -16),
     (  -2274,   -2),
     (   2062,    2),
     (   1426,  -34),
     (    712,    1),
     (   -517,   12),
     (   -386,   -4),
     (   -301,    0),
     (    217,   -5),
     (   -158,    0),
     (    129,    1),
     (    123,    0),
     (     63,    0),
     (     63,    1),
     (    -59,    0),
     (    -58,   -1),
     (    -51,    0),
     (     48,    0),
     (     46,    0),
     (    -38,    0),
     (    -31,    0),
     (     29,    0),
     (     29,    0),
     (     26,    0),
     (    -22,    0),
     (     21,    0),
     (     17,   -1),
     (     16,    0),
     (    -16,    1),
     (    -15,    0)
    );

  arg_eps:array[0..30,0..1] of longint = (
     ( 92025,   89),
     (  5736,  -31),
     (   977,   -5),
     (  -895,    5),
     (    54,   -1),
     (    -7,    0),
     (   224,   -6),
     (   200,    0),
     (   129,   -1),
     (   -95,    3),
     (     0,    0),
     (   -70,    0),
     (   -53,    0),
     (     0,    0),
     (   -33,    0),
     (    26,    0),
     (    32,    0),
     (    27,    0),
     (     0,    0),
     (   -24,    0),
     (    16,    0),
     (    13,    0),
     (     0,    0),
     (   -12,    0),
     (     0,    0),
     (     0,    0),
     (   -10,    0),
     (     0,    0),
     (    -8,    0),
     (     7,    0),
     (     9,    0)
    );

var
  t,omega: extended;

  l,ls: extended;

  epsilon_0,delta_epsilon: extended;
begin
  t:=(julian_date(date)-2451545.0)/36525;

  (* longitude of rising knot *)
  omega:=put_in_360(125.04452+(-1934.136261+(0.0020708+1/450000*t)*t)*t);

  (* delta_phi and delta_epsilon - low accuracy *)
  (* mean longitude of sun (l) and moon (ls) *)
  l:=280.4665+36000.7698*t;
  ls:=218.3165+481267.8813*t;

  (* correction due to nutation *)
  delta_epsilon:=9.20*cos_d(omega)+0.57*cos_d(2*l)+0.10*cos_d(2*ls)-0.09*cos_d(2*omega);

  (* longitude correction due to nutation *)
  delta_phi:=(-17.20*sin_d(omega)-1.32*sin_d(2*l)-0.23*sin_d(2*ls)+0.21*sin_d(2*omega))/3600;

  (* angle of ecliptic *)
  epsilon_0:=84381.448+(-46.8150+(-0.00059+0.001813*t)*t)*t;

  epsilon:=(epsilon_0+delta_epsilon)/3600;
  end;

function star_time(date:TDateTime):extended;    // degrees
var
  jd, t: extended;
  delta_phi, epsilon: extended;
begin
  jd:=julian_date(date);
  t:=(jd-2451545.0)/36525;
  calc_epsilon_phi(date,delta_phi,epsilon);
  result:=put_in_360(280.46061837+360.98564736629*(jd-2451545.0)+
                     t*t*(0.000387933-t/38710000)+
                     delta_phi*cos_d(epsilon) );
  end;


{ Coordinate functions }

{ Based upon Chapter 13 (12) and 22 (21) of Meeus }

procedure calc_geocentric(var coord:t_coord; date:TDateTime);
var
  epsilon: extended;
  delta_phi: extended;
  alpha,delta: extended;
begin
  calc_epsilon_phi(date,delta_phi,epsilon);
  coord.longitude:=put_in_360(coord.longitude+delta_phi);

  (* geocentric coordinates *)
{   alpha:=arctan2_d(cos_d(epsilon)*sin_d(o),cos_d(o)); }
{   delta:=arcsin_d(sin_d(epsilon)*sin_d(o)); }
  alpha:=arctan2_d( sin_d(coord.longitude)*cos_d(epsilon)
                   -tan_d(coord.latitude)*sin_d(epsilon)
                  ,cos_d(coord.longitude));
  delta:=arcsin_d( sin_d(coord.latitude)*cos_d(epsilon)
                  +cos_d(coord.latitude)*sin_d(epsilon)*sin_d(coord.longitude));

  coord.rektaszension:=alpha;
  coord.declination:=delta;
  end;

procedure calc_horizontal(var coord:t_coord; date:TDateTime; longitude,latitude: extended);
var
  h: extended;
begin
  h:=put_in_360(star_time(date)-coord.rektaszension-longitude);
  coord.azimuth:=arctan2_d(sin_d(h),
                           cos_d(h)*sin_d(latitude)-
                           tan_d(coord.declination)*cos_d(latitude) );
  coord.elevation:=arcsin_d(sin_d(latitude)*sin_d(coord.declination)+
                            cos_d(latitude)*cos_d(coord.declination)*cos_d(h));
  end;


{ Based upon Chapter 25 (24) of Meeus - low accurancy }
function sun_coordinate_low(date:TDateTime):t_coord;
var
  t,e,m,c,nu: extended;
  l0,o,omega,lambda: extended;
begin
  t:=(julian_date(date)-2451545.0)/36525;

  (* geometrical mean longitude of the sun *)
  l0:=280.46645+(36000.76983+0.0003032*t)*t;

  (* excentricity of the earth orbit *)
  e:=0.016708617+(-0.000042037-0.0000001236*t)*t;

  (* mean anomaly of the sun *)
  m:=357.52910+(35999.05030-(0.0001559+0.00000048*t)*t)*t;

  (* mean point of sun *)
  c:= (1.914600+(-0.004817-0.000014*t)*t)*sin_d(m)
     +(0.019993-0.000101*t)*sin_d(2*m)
     +0.000290*sin_d(3*m);

  (* true longitude of the sun *)
  o:=put_in_360(l0+c);

  (* true anomaly of the sun *)
  nu:=m+c;

  (* distance of the sun in km *)
  result.radius:=(1.000001018*(1-e*e))/(1+e*cos_d(nu))*AU;

  (* apparent longitude of the sun *)
  omega:=125.04452+(-1934.136261+(0.0020708+1/450000*t)*t)*t;
  lambda:=put_in_360(o-0.00569-0.00478*sin_d(omega)
                     -20.4898/3600/(result.radius/AU));

  result.longitude:=lambda;
  result.latitude:=0;

  calc_geocentric(result,date);
  end;

function sun_coordinate(date:TDateTime):t_coord;
var
  l,b,r: extended;
  lambda,t: extended;
begin
  earth_coord(date,l,b,r);
  (* convert earth coordinate to sun coordinate *)
  l:=l+180;
  b:=-b;
  (* conversion to FK5 *)
  t:=(julian_date(date)-2451545.0)/365250.0*10;
  lambda:=l+(-1.397-0.00031*t)*t;
  l:=l-0.09033/3600;
  b:=b+0.03916/3600*(cos_d(lambda)-sin_d(lambda));
  (* aberration *)
  l:=l-20.4898/3600/r;
  (* correction of nutation - is done inside calc_geocentric *)
{   calc_epsilon_phi(date,delta_phi,epsilon); }
{   l:=l+delta_phi; }
  (* fill result and convert to geocentric *)
  result.longitude:=put_in_360(l);
  result.latitude:=b;
  result.radius:=r*AU;
  calc_geocentric(result,date);
  end;

{ Based upon Chapter 47 (45) of Meeus }

function moon_coordinate(date:TDateTime):t_coord;
const

  arg_lr:array[0..59,0..3] of shortint = (
     ( 0, 0, 1, 0),
     ( 2, 0,-1, 0),
     ( 2, 0, 0, 0),
     ( 0, 0, 2, 0),
     ( 0, 1, 0, 0),
     ( 0, 0, 0, 2),
     ( 2, 0,-2, 0),
     ( 2,-1,-1, 0),
     ( 2, 0, 1, 0),
     ( 2,-1, 0, 0),
     ( 0, 1,-1, 0),
     ( 1, 0, 0, 0),
     ( 0, 1, 1, 0),
     ( 2, 0, 0,-2),
     ( 0, 0, 1, 2),
     ( 0, 0, 1,-2),
     ( 4, 0,-1, 0),
     ( 0, 0, 3, 0),
     ( 4, 0,-2, 0),
     ( 2, 1,-1, 0),
     ( 2, 1, 0, 0),
     ( 1, 0,-1, 0),
     ( 1, 1, 0, 0),
     ( 2,-1, 1, 0),
     ( 2, 0, 2, 0),
     ( 4, 0, 0, 0),
     ( 2, 0,-3, 0),
     ( 0, 1,-2, 0),
     ( 2, 0,-1, 2),
     ( 2,-1,-2, 0),
     ( 1, 0, 1, 0),
     ( 2,-2, 0, 0),
     ( 0, 1, 2, 0),
     ( 0, 2, 0, 0),
     ( 2,-2,-1, 0),
     ( 2, 0, 1,-2),
     ( 2, 0, 0, 2),
     ( 4,-1,-1, 0),
     ( 0, 0, 2, 2),
     ( 3, 0,-1, 0),
     ( 2, 1, 1, 0),
     ( 4,-1,-2, 0),
     ( 0, 2,-1, 0),
     ( 2, 2,-1, 0),
     ( 2, 1,-2, 0),
     ( 2,-1, 0,-2),
     ( 4, 0, 1, 0),
     ( 0, 0, 4, 0),
     ( 4,-1, 0, 0),
     ( 1, 0,-2, 0),
     ( 2, 1, 0,-2),
     ( 0, 0, 2,-2),
     ( 1, 1, 1, 0),
     ( 3, 0,-2, 0),
     ( 4, 0,-3, 0),
     ( 2,-1, 2, 0),
     ( 0, 2, 1, 0),
     ( 1, 1,-1, 0),
     ( 2, 0, 3, 0),
     ( 2, 0,-1,-2)
                   );

  arg_b:array[0..59,0..3] of shortint = (
     ( 0, 0, 0, 1),
     ( 0, 0, 1, 1),
     ( 0, 0, 1,-1),
     ( 2, 0, 0,-1),
     ( 2, 0,-1, 1),
     ( 2, 0,-1,-1),
     ( 2, 0, 0, 1),
     ( 0, 0, 2, 1),
     ( 2, 0, 1,-1),
     ( 0, 0, 2,-1),  (* !!! Error in German Meeus *)
     ( 2,-1, 0,-1),
     ( 2, 0,-2,-1),
     ( 2, 0, 1, 1),
     ( 2, 1, 0,-1),
     ( 2,-1,-1, 1),
     ( 2,-1, 0, 1),
     ( 2,-1,-1,-1),
     ( 0, 1,-1,-1),
     ( 4, 0,-1,-1),
     ( 0, 1, 0, 1),
     ( 0, 0, 0, 3),
     ( 0, 1,-1, 1),
     ( 1, 0, 0, 1),
     ( 0, 1, 1, 1),
     ( 0, 1, 1,-1),
     ( 0, 1, 0,-1),
     ( 1, 0, 0,-1),
     ( 0, 0, 3, 1),
     ( 4, 0, 0,-1),
     ( 4, 0,-1, 1),
     ( 0, 0, 1,-3),
     ( 4, 0,-2, 1),
     ( 2, 0, 0,-3),
     ( 2, 0, 2,-1),
     ( 2,-1, 1,-1),
     ( 2, 0,-2, 1),
     ( 0, 0, 3,-1),
     ( 2, 0, 2, 1),
     ( 2, 0,-3,-1),
     ( 2, 1,-1, 1),
     ( 2, 1, 0, 1),
     ( 4, 0, 0, 1),
     ( 2,-1, 1, 1),
     ( 2,-2, 0,-1),
     ( 0, 0, 1, 3),
     ( 2, 1, 1,-1),
     ( 1, 1, 0,-1),
     ( 1, 1, 0, 1),
     ( 0, 1,-2,-1),
     ( 2, 1,-1,-1),
     ( 1, 0, 1, 1),
     ( 2,-1,-2,-1),
     ( 0, 1, 2, 1),
     ( 4, 0,-2,-1),
     ( 4,-1,-1,-1),
     ( 1, 0, 1,-1),
     ( 4, 0, 1,-1),
     ( 1, 0,-1,-1),
     ( 4,-1, 0,-1),
     ( 2,-2, 0, 1)
    );

  sigma_r: array[0..59] of longint = (
   -20905355,
    -3699111,
    -2955968,
     -569925,
       48888,
       -3149,
      246158,
     -152138,
     -170733,
     -204586,
     -129620,
      108743,
      104755,
       10321,
           0,
       79661,
      -34782,
      -23210,
      -21636,
       24208,
       30824,
       -8379,
      -16675,
      -12831,
      -10445,
      -11650,
       14403,
       -7003,
           0,
       10056,
        6322,
       -9884,
        5751,
           0,
       -4950,
        4130,
           0,
       -3958,
           0,
        3258,
        2616,
       -1897,
       -2117,
        2354,
           0,
           0,
       -1423,
       -1117,
       -1571,
       -1739,
           0,
       -4421,
           0,
           0,
           0,
           0,
        1165,
           0,
           0,
        8752
              );

  sigma_l: array[0..59] of longint = (
    6288774,
    1274027,
     658314,
     213618,
    -185116,
    -114332,
      58793,
      57066,
      53322,
      45758,
     -40923,
     -34720,
     -30383,
      15327,
     -12528,
      10980,
      10675,
      10034,
       8548,
      -7888,
      -6766,
      -5163,
       4987,
       4036,
       3994,
       3861,
       3665,
      -2689,
      -2602,
       2390,
      -2348,
       2236,
      -2120,
      -2069,
       2048,
      -1773,
      -1595,
       1215,
      -1110,
       -892,
       -810,
        759,
       -713,
       -700,
        691,
        596,
        549,
        537,
        520,
       -487,
       -399,
       -381,
        351,
       -340,
        330,
        327,
       -323,
        299,
        294,
          0
    );


  sigma_b: array[0..59] of longint = (
    5128122,
     280602,
     277693,
     173237,
      55413,
      46271,
      32573,
      17198,
       9266,
       8822,
       8216,
       4324,
       4200,
      -3359,
       2463,
       2211,
       2065,
      -1870,
       1828,
      -1794,
      -1749,
      -1565,
      -1491,
      -1475,
      -1410,
      -1344,
      -1335,
       1107,
       1021,
        833,
        777,
        671,
        607,
        596,
        491,
       -451,
        439,
        422,
        421,
       -366,
       -351,
        331,
        315,
        302,
       -283,
       -229,
        223,
        223,
       -220,
       -220,
       -185,
        181,
       -177,
        176,
        166,
       -164,
        132,
       -119,
        115,
        107
    );

var
  t,d,m,ms,f,e,ls : extended;
  sr,sl,sb,temp: extended;
  a1,a2,a3: extended;
  lambda,beta,delta: extended;
  i: integer;
begin
  t:=(julian_date(date)-2451545)/36525;

  (* mean elongation of the moon *)
  d:=297.8502042+(445267.1115168+(-0.0016300+(1/545868-1/113065000*t)*t)*t)*t;

  (* mean anomaly of the sun *)
  m:=357.5291092+(35999.0502909+(-0.0001536+1/24490000*t)*t)*t;

  (* mean anomaly of the moon *)
  ms:=134.9634114+(477198.8676313+(0.0089970+(1/69699-1/1471200*t)*t)*t)*t;

  (* argument of the longitude of the moon *)
  f:=93.2720993+(483202.0175273+(-0.0034029+(-1/3526000+1/863310000*t)*t)*t)*t;

  (* correction term due to excentricity of the earth orbit *)
  e:=1.0+(-0.002516-0.0000074*t)*t;

  (* mean longitude of the moon *)
  ls:=218.3164591+(481267.88134236+(-0.0013268+(1/538841-1/65194000*t)*t)*t)*t;

  (* arguments of correction terms *)
  a1:=119.75+131.849*t;
  a2:=53.09+479264.290*t;
  a3:=313.45+481266.484*t;

  (* sr := δ r_i cos(d,m,ms,f);   !!!  gives different value than in Meeus *)
  sr:=0;
  for i:=0 to 59 do begin
    temp:=sigma_r[i]*cos_d( arg_lr[i,0]*d
                           +arg_lr[i,1]*m
                           +arg_lr[i,2]*ms
                           +arg_lr[i,3]*f);
    if abs(arg_lr[i,1])=1 then temp:=temp*e;
    if abs(arg_lr[i,1])=2 then temp:=temp*e*e;
    sr:=sr+temp;
    end;

  (* sl := δ l_i sin(d,m,ms,f); *)
  sl:=0;
  for i:=0 to 59 do begin
    temp:=sigma_l[i]*sin_d( arg_lr[i,0]*d
                           +arg_lr[i,1]*m
                           +arg_lr[i,2]*ms
                           +arg_lr[i,3]*f);
    if abs(arg_lr[i,1])=1 then temp:=temp*e;
    if abs(arg_lr[i,1])=2 then temp:=temp*e*e;
    sl:=sl+temp;
    end;

  (* correction terms *)
  sl:=sl +3958*sin_d(a1)
         +1962*sin_d(ls-f)
          +318*sin_d(a2);

  (* sb := δ b_i sin(d,m,ms,f); *)
  sb:=0;
  for i:=0 to 59 do begin
    temp:=sigma_b[i]*sin_d( arg_b[i,0]*d
                           +arg_b[i,1]*m
                           +arg_b[i,2]*ms
                           +arg_b[i,3]*f);
    if abs(arg_b[i,1])=1 then temp:=temp*e;
    if abs(arg_b[i,1])=2 then temp:=temp*e*e;
    sb:=sb+temp;
    end;

  (* correction terms *)
  sb:=sb -2235*sin_d(ls)
          +382*sin_d(a3)
          +175*sin_d(a1-f)
          +175*sin_d(a1+f)
          +127*sin_d(ls-ms)
          -115*sin_d(ls+ms);


  lambda:=ls+sl/1000000;
  beta:=sb/1000000;
  delta:=385000.56+sr/1000;

  result.radius:=delta;
  result.longitude:=lambda;
  result.latitude:=beta;

  calc_geocentric(result,date);
  end;

{ Based upon chapter 40 (39) of Meeus }

procedure correct_position(var position:t_coord; date:TDateTime;
                           latitude,longitude,height:extended);
var
  u,h,delta_alpha: extended;
  rho_sin, rho_cos: extended;
const
  b_a=0.99664719;
begin
  u:=arctan_d(b_a*b_a*tan_d(latitude));
  rho_sin:=b_a*sin_d(u)+height/6378140*sin_d(latitude);
  rho_cos:=cos_d(u)+height/6378140*cos_d(latitude);

  position.parallax:=arcsin_d(sin_d(8.794/3600)/(moon_distance(date)/AU));
  h:=star_time(date)-longitude-position.rektaszension;
  delta_alpha:=arctan_d(
                (-rho_cos*sin_d(position.parallax)*sin_d(h))/
                (cos_d(position.declination)-
                  rho_cos*sin_d(position.parallax)*cos_d(h)));
  position.rektaszension:=position.rektaszension+delta_alpha;
  position.declination:=arctan_d(
      (( sin_d(position.declination)
        -rho_sin*sin_d(position.parallax))*cos_d(delta_alpha))/
      ( cos_d(position.declination)
       -rho_cos*sin_d(position.parallax)*cos_d(h)));
  end;

{ Moon phases and age of the moon }

{ Based upon Chapter 49 (47) of Meeus }
{ Both used for moon phases and moon and sun eclipses }

procedure calc_phase_data(date:TDateTime; phase:TMoonPhase; var jde,kk,m,ms,f,o,e: extended);
const
  phases = ord(high(TMoonPhase))+1;
var
  t: extended;
  k: longint;
  ts: extended;
begin
  k:=round((date-datetime_2000_01_01)/36525.0*1236.85);
  ts:=(date-datetime_2000_01_01)/36525.0;
  kk:=int(k)+ord(phase)/phases;
  t:=kk/1236.85;
  jde:=2451550.09765+29.530588853*kk
       +t*t*(0.0001337-t*(0.000000150-0.00000000073*t));
  m:=2.5534+29.10535669*kk-t*t*(0.0000218+0.00000011*t);
  ms:=201.5643+385.81693528*kk+t*t*(0.1017438+t*(0.00001239-t*0.000000058));
  f:= 160.7108+390.67050274*kk-t*t*(0.0016341+t*(0.00000227-t*0.000000011));
  o:=124.7746-1.56375580*kk+t*t*(0.0020691+t*0.00000215);
  e:=1-ts*(0.002516+ts*0.0000074);
  end;

function nextphase_approx(date: TDateTime; phase:TMoonphase):TDateTime;
const
  epsilon = 1E-7;
  phases = ord(high(TMoonPhase))+1;
var
  target_age: extended;
  h: extended;
begin
  target_age:=ord(phase)*mean_lunation/phases;
  result:=date;
  repeat
    h:=age_of_moon(result)-target_age;
    if h>mean_lunation/2 then
      h:=h-mean_lunation;
    result:=result-h;
    until abs(h)<epsilon;
  end;

{ Based upon Chapter 49 (47) of Meeus }

function nextphase_49(date:TDateTime; phase:TMoonPhase):TDateTime;
var
  t: extended;
  kk: extended;
  jde: extended;
  m,ms,f,o,e: extended;
  korr,w,akorr: extended;
  a:array[1..14] of extended;
begin
  if not (phase in [Newmoon,FirstQuarter,Fullmoon,LastQuarter]) then
    raise E_OutOfAlgorithmRange.Create('Invalid TMoonPhase');

  calc_phase_data(date,phase,jde,kk,m,ms,f,o,e);
{   k:=round((date-datetime_2000_01_01)/36525.0*1236.85); }
{   ts:=(date-datetime_2000_01_01)/36525.0; }
{   kk:=int(k)+ord(phase)/4.0; }
  t:=kk/1236.85;
{   m:=2.5534+29.10535669*kk-t*t*(0.0000218+0.00000011*t); }
{   ms:=201.5643+385.81693528*kk+t*t*(0.1017438+t*(0.00001239-t*0.000000058)); }
{   f:= 160.7108+390.67050274*kk-t*t*(0.0016341+t*(0.00000227-t*0.000000011)); }
{   o:=124.7746-1.56375580*kk+t*t*(0.0020691+t*0.00000215); }
{   e:=1-ts*(0.002516+ts*0.0000074); }
  case phase of
    (* Newmoon: *)
    Newmoon:  begin
      korr:= -0.40720*sin_d(ms)
             +0.17241*e*sin_d(m)
             +0.01608*sin_d(2*ms)
             +0.01039*sin_d(2*f)
             +0.00739*e*sin_d(ms-m)
             -0.00514*e*sin_d(ms+m)
             +0.00208*e*e*sin_d(2*m)
             -0.00111*sin_d(ms-2*f)
             -0.00057*sin_d(ms+2*f)
             +0.00056*e*sin_d(2*ms+m)
             -0.00042*sin_d(3*ms)
             +0.00042*e*sin_d(m+2*f)
             +0.00038*e*sin_d(m-2*f)
             -0.00024*e*sin_d(2*ms-m)
             -0.00017*sin_d(o)
             -0.00007*sin_d(ms+2*m)
             +0.00004*sin_d(2*ms-2*f)
             +0.00004*sin_d(3*m)
             +0.00003*sin_d(ms+m-2*f)
             +0.00003*sin_d(2*ms+2*f)
             -0.00003*sin_d(ms+m+2*f)
             +0.00003*sin_d(ms-m+2*f)
             -0.00002*sin_d(ms-m-2*f)
             -0.00002*sin_d(3*ms+m)
             +0.00002*sin_d(4*ms);
      end;

    (* FirstQuarter,LastQuarter: *)
    FirstQuarter,LastQuarter:  begin
      korr:= -0.62801*sin_d(ms)
             +0.17172*e*sin_d(m)
             -0.01183*e*sin_d(ms+m)
             +0.00862*sin_d(2*ms)
             +0.00804*sin_d(2*f)
             +0.00454*e*sin_d(ms-m)
             +0.00204*e*e*sin_d(2*m)
             -0.00180*sin_d(ms-2*f)
             -0.00070*sin_d(ms+2*f)
             -0.00040*sin_d(3*ms)
             -0.00034*e*sin_d(2*ms-m)
             +0.00032*e*sin_d(m+2*f)
             +0.00032*e*sin_d(m-2*f)
             -0.00028*e*e*sin_d(ms+2*m)
             +0.00027*e*sin_d(2*ms+m)
             -0.00017*sin_d(o)
             -0.00005*sin_d(ms-m-2*f)
             +0.00004*sin_d(2*ms+2*f)
             -0.00004*sin_d(ms+m+2*f)
             +0.00004*sin_d(ms-2*m)
             +0.00003*sin_d(ms+m-2*f)
             +0.00003*sin_d(3*m)
             +0.00002*sin_d(2*ms-2*f)
             +0.00002*sin_d(ms-m+2*f)
             -0.00002*sin_d(3*ms+m);
      w:=0.00306-0.00038*e*cos_d(m)
                +0.00026*cos_d(ms)
                -0.00002*cos_d(ms-m)
                +0.00002*cos_d(ms+m)
                +0.00002*cos_d(2*f);
      if phase = FirstQuarter then begin
        korr:=korr+w;
        end
      else begin
        korr:=korr-w;
        end;
      end;

    (* Fullmoon: *)
    Fullmoon:  begin
      korr:= -0.40614*sin_d(ms)
             +0.17302*e*sin_d(m)
             +0.01614*sin_d(2*ms)
             +0.01043*sin_d(2*f)
             +0.00734*e*sin_d(ms-m)
             -0.00515*e*sin_d(ms+m)
             +0.00209*e*e*sin_d(2*m)
             -0.00111*sin_d(ms-2*f)
             -0.00057*sin_d(ms+2*f)
             +0.00056*e*sin_d(2*ms+m)
             -0.00042*sin_d(3*ms)
             +0.00042*e*sin_d(m+2*f)
             +0.00038*e*sin_d(m-2*f)
             -0.00024*e*sin_d(2*ms-m)
             -0.00017*sin_d(o)
             -0.00007*sin_d(ms+2*m)
             +0.00004*sin_d(2*ms-2*f)
             +0.00004*sin_d(3*m)
             +0.00003*sin_d(ms+m-2*f)
             +0.00003*sin_d(2*ms+2*f)
             -0.00003*sin_d(ms+m+2*f)
             +0.00003*sin_d(ms-m+2*f)
             -0.00002*sin_d(ms-m-2*f)
             -0.00002*sin_d(3*ms+m)
             +0.00002*sin_d(4*ms);
      end;

    else
      korr:=0;

    end;
  (* Additional Corrections due to planets *)
  a[1]:=299.77+0.107408*kk-0.009173*t*t;
  a[2]:=251.88+0.016321*kk;
  a[3]:=251.83+26.651886*kk;
  a[4]:=349.42+36.412478*kk;
  a[5]:= 84.66+18.206239*kk;
  a[6]:=141.74+53.303771*kk;
  a[7]:=207.14+2.453732*kk;
  a[8]:=154.84+7.306860*kk;
  a[9]:= 34.52+27.261239*kk;
  a[10]:=207.19+0.121824*kk;
  a[11]:=291.34+1.844379*kk;
  a[12]:=161.72+24.198154*kk;
  a[13]:=239.56+25.513099*kk;
  a[14]:=331.55+3.592518*kk;
  akorr:=   +0.000325*sin_d(a[1])
            +0.000165*sin_d(a[2])
            +0.000164*sin_d(a[3])
            +0.000126*sin_d(a[4])
            +0.000110*sin_d(a[5])
            +0.000062*sin_d(a[6])
            +0.000060*sin_d(a[7])
            +0.000056*sin_d(a[8])
            +0.000047*sin_d(a[9])
            +0.000042*sin_d(a[10])
            +0.000040*sin_d(a[11])
            +0.000037*sin_d(a[12])
            +0.000035*sin_d(a[13])
            +0.000023*sin_d(a[14]);
  korr:=korr+akorr;

  result:=delphi_date(jde+korr);
  end;


(* nextphase_approx has similar accuracy as nextphase_49 *)
function nextphase(date:TDateTime; phase:TMoonPhase):TDateTime;
begin
  case phase of
    Newmoon,FirstQuarter,Fullmoon,LastQuarter:
      result:=nextphase_49(date,phase);
    WaxingCrescrent,WaxingGibbous,WaningGibbous,WaningCrescent:
      result:=nextphase_approx(date,phase);
    else
      result:=0;
      end;
  end;

function last_phase(date:TDateTime; phase:TMoonPhase):TDateTime;
var
  temp_date: TDateTime;
begin
  temp_date:=date+28;
  result:=temp_date;
  while result>date do begin
    result:=nextphase(temp_date,phase);
    if result=0 then
      raise E_OutOfAlgorithmRange.Create('No TDateTime possible');
    temp_date:=temp_date-28;
    end;
  end;

function next_phase(date:TDateTime; phase:TMoonPhase):TDateTime;
var
  temp_date: TDateTime;
begin
  temp_date:=date-28;
  result:=temp_date;
  while result<date do begin
    result:=nextphase(temp_date,phase);
    if result=0 then
      raise E_OutOfAlgorithmRange.Create('No TDateTime possible');
    temp_date:=temp_date+28;
    end;
  end;

function nearest_phase(date: TDateTime):TMoonPhase;
const
  phases = ord(high(TMoonPhase))+1;
begin
  result:=TMoonPhase(round(age_of_moon(date)/mean_lunation*phases) mod phases);
  end;

function next_blue_moon_bias(date: TDateTime; timezonebias:extended):TDateTime;
var
  h: TDateTime;
  y,m,d: word;
  y1,m1,d1: word;
begin
  h:=date-1+timezonebias;
  repeat
    h:=h+1;
    h:=next_phase(h,Fullmoon);
    DecodeDateCorrect(h-timezonebias,y,m,d);
    if d>27 then     (* only chance for a blue moon anyway *)
      DecodeDateCorrect(last_phase(h-5,FullMoon)-timezonebias,y1,m1,d1)
    else
      m1:=0;
    until m=m1;
  result:=h;
  end;

function next_blue_moon(date: TDateTime):TDateTime;
begin
  result:=next_blue_moon_bias(date,0);
  end;

function is_blue_moon(lunation: integer):boolean;
var
  date: TDateTime;
begin
  date:=next_phase(datetime_first_lunation+(lunation-1)*mean_lunation-5,NewMoon);
  result:=((next_blue_moon(date)-date)<mean_lunation);
  end;

{ Based upon Chapter 48 (46) of Meeus }
function moon_phase_angle(date: TDateTime):extended;
var
  sun_coord,moon_coord: t_coord;
  phi,i: extended;
begin
  sun_coord:=sun_coordinate(date);
  moon_coord:=moon_coordinate(date);
  phi:=arccos(cos_d(moon_coord.latitude)
             *cos_d(moon_coord.longitude-sun_coord.longitude));
  i:=arctan(sun_coord.radius*sin(phi)/
            (moon_coord.radius-sun_coord.radius*cos(phi)));
  if i<0 then  result:=i/pi*180+180
         else  result:=i/pi*180;

  if put_in_360(moon_coord.longitude-sun_coord.longitude)>180 then
    result:=-result;

  end;

function age_of_moon(date: TDateTime):extended;
var
  sun_coord,moon_coord: t_coord;
begin
  sun_coord:=sun_coordinate(date);
  moon_coord:=moon_coordinate(date);
  result:=put_in_360(moon_coord.longitude-sun_coord.longitude)/360*mean_lunation;
  end;

function current_phase(date:TDateTime):extended;
begin
  result:=(1+cos_d(moon_phase_angle(date)))/2;
  end;

function lunation(date:TDateTime):integer;
begin
  result:=round((last_phase(date,NewMoon)-datetime_first_lunation)/mean_lunation)+1;
  end;

{ The distances }
function sun_distance(date: TDateTime): extended;   // AU
begin
  result:=sun_coordinate(date).radius/au;
  end;

function moon_distance(date: TDateTime): extended;  // km
begin
  result:=moon_coordinate(date).radius;
  end;


{ The angular diameter (which is 0.5 of the subtent in moontool) }

function sun_diameter(date:TDateTime):extended;   // angular seconds
begin
  result:=959.63/(sun_coordinate(date).radius/au)*2;
  end;

function moon_diameter(date:TDateTime):extended;
begin
  result:=358473400/moon_coordinate(date).radius*2;
  end;


{ Perigee and Apogee }
{ Based upon Chapter 50 (48) of Meeus }

function nextXXXgee(date:TDateTime; apo: boolean):TDateTime;
const

  arg_apo:array[0..31,0..2] of shortint = (
     { D  F  M }
     ( 2, 0, 0),
     ( 4, 0, 0),
     ( 0, 0, 1),
     ( 2, 0,-1),
     ( 0, 2, 0),
     ( 1, 0, 0),
     ( 6, 0, 0),
     ( 4, 0,-1),
     ( 2, 2, 0),
     ( 1, 0, 1),
     ( 8, 0, 0),
     ( 6, 0,-1),
     ( 2,-2, 0),
     ( 2, 0,-2),
     ( 3, 0, 0),
     ( 4, 2, 0),
     ( 8, 0,-1),
     ( 4, 0,-2),
     (10, 0, 0),
     ( 3, 0, 1),
     ( 0, 0, 2),
     ( 2, 0, 1),
     ( 2, 0, 2),
     ( 6, 2, 0),
     ( 6, 0,-2),
     (10, 0,-1),
     ( 5, 0, 0),
     ( 4,-2, 0),
     ( 0, 2, 1),
     (12, 0, 0),
     ( 2, 2,-1),
     ( 1, 0,-1)
               );

  arg_per:array[0..59,0..2] of shortint = (
     { D  F  M }
     ( 2, 0, 0),
     ( 4, 0, 0),
     ( 6, 0, 0),
     ( 8, 0, 0),
     ( 2, 0,-1),
     ( 0, 0, 1),
     (10, 0, 0),
     ( 4, 0,-1),
     ( 6, 0,-1),
     (12, 0, 0),
     ( 1, 0, 0),
     ( 8, 0,-1),
     (14, 0, 0),
     ( 0, 2, 0),
     ( 3, 0, 0),
     (10, 0,-1),
     (16, 0, 0),
     (12, 0,-1),
     ( 5, 0, 0),
     ( 2, 2, 0),
     (18, 0, 0),
     (14, 0,-1),
     ( 7, 0, 0),
     ( 2, 1, 0),
     (20, 0, 0),
     ( 1, 0, 1),
     (16, 0,-1),
     ( 4, 0, 1),
     ( 2, 0,-2),
     ( 4, 0,-2),
     ( 6, 0,-2),
     (22, 0, 0),
     (18, 0,-1),
     ( 6, 0, 1),
     (11, 0, 0),
     ( 8, 0, 1),
     ( 4,-2, 0),
     ( 6, 2, 0),
     ( 3, 0, 1),
     ( 5, 0, 1),
     (13, 0, 0),
     (20, 0,-1),
     ( 3, 0, 2),
     ( 4, 2,-2),
     ( 1, 0, 2),
     (22, 0,-1),
     ( 0, 4, 0),
     ( 6,-2, 0),
     ( 2,-2, 1),
     ( 0, 0, 2),
     ( 0, 2,-1),
     ( 2, 4, 0),
     ( 0, 2,-2),
     ( 2,-2, 2),
     (24, 0, 0),
     ( 4,-4, 0),
     ( 9, 0, 0),
     ( 4, 2, 0),
     ( 2, 0, 2),
     ( 1, 0,-1)
               );

  koe_apo:array[0..31,0..1] of longint = (
     {    1   T }
     ( 4392,  0),
     (  684,  0),
     (  456,-11),
     (  426,-11),
     (  212,  0),
     ( -189,  0),
     (  144,  0),
     (  113,  0),
     (   47,  0),
     (   36,  0),
     (   35,  0),
     (   34,  0),
     (  -34,  0),
     (   22,  0),
     (  -17,  0),
     (   13,  0),
     (   11,  0),
     (   10,  0),
     (    9,  0),
     (    7,  0),
     (    6,  0),
     (    5,  0),
     (    5,  0),
     (    4,  0),
     (    4,  0),
     (    4,  0),
     (   -4,  0),
     (   -4,  0),
     (    3,  0),
     (    3,  0),
     (    3,  0),
     (   -3,  0)
                 );

  koe_per:array[0..59,0..1] of longint = (
     {     1   T }
     (-16769,  0),
     (  4589,  0),
     ( -1856,  0),
     (   883,  0),
     (  -773, 19),
     (   502,-13),
     (  -460,  0),
     (   422,-11),
     (  -256,  0),
     (   253,  0),
     (   237,  0),
     (   162,  0),
     (  -145,  0),
     (   129,  0),
     (  -112,  0),
     (  -104,  0),
     (    86,  0),
     (    69,  0),
     (    66,  0),
     (   -53,  0),
     (   -52,  0),
     (   -46,  0),
     (   -41,  0),
     (    40,  0),
     (    32,  0),
     (   -32,  0),
     (    31,  0),
     (   -29,  0),
     (   -27,  0),
     (    24,  0),
     (   -21,  0),
     (   -21,  0),
     (   -21,  0),
     (    19,  0),
     (   -18,  0),
     (   -14,  0),
     (   -14,  0),
     (   -14,  0),
     (    14,  0),
     (   -14,  0),
     (    13,  0),
     (    13,  0),
     (    11,  0),
     (   -11,  0),
     (   -10,  0),
     (    -9,  0),
     (    -8,  0),
     (     8,  0),
     (     8,  0),
     (     7,  0),
     (     7,  0),
     (     7,  0),
     (    -6,  0),
     (    -6,  0),
     (     6,  0),
     (     5,  0),
     (    27,  0),
     (    27,  0),
     (     5,  0),
     (    -4,  0)
                 );

var
  k, jde, t: extended;
  d,m,f,v: extended;
  i: integer;
begin
  k:=round(((date-datetime_1999_01_01)/365.25-0.97)*13.2555);
  if apo then k:=k+0.5;
  t:=k/1325.55;
  jde:=2451534.6698+27.55454988*k+(-0.0006886+
       (-0.000001098+0.0000000052*t)*t)*t*t;
  d:=171.9179+335.9106046*k+(-0.0100250+(-0.00001156+0.000000055*t)*t)*t*t;
  m:=347.3477+27.1577721*k+(-0.0008323-0.0000010*t)*t*t;
  f:=316.6109+364.5287911*k+(-0.0125131-0.0000148*t)*t*t;
  v:=0;
  if apo then
    for i:=0 to 31 do
      v:=v+sin_d(arg_apo[i,0]*d+arg_apo[i,1]*f+arg_apo[i,2]*m)*
         (koe_apo[i,0]*0.0001+koe_apo[i,1]*0.00001*t)
  else
    for i:=0 to 59 do
      v:=v+sin_d(arg_per[i,0]*d+arg_per[i,1]*f+arg_per[i,2]*m)*
         (koe_per[i,0]*0.0001+koe_per[i,1]*0.00001*t);
  result:=delphi_date(jde+v);
  end;

function nextperigee(date:TDateTime):TDateTime;
var
  temp_date: TDateTime;
begin
  temp_date:=date-28;
  result:=temp_date;
  while result<date do begin
    result:=nextXXXgee(temp_date,false);
    temp_date:=temp_date+28;
    end;
  end;

function nextapogee(date:TDateTime):TDateTime;
var
  temp_date: TDateTime;
begin
  temp_date:=date-28;
  result:=temp_date;
  while result<date do begin
    result:=nextXXXgee(temp_date,true);
    temp_date:=temp_date+28;
    end;
  end;

function nextxxxhel(date: TDateTime; apo: boolean):TDateTime;
var
  k, jde: extended;
begin
  k:=round(((date-datetime_2000_01_01)/365.25-0.01)*0.99997);
  if apo then k:=k+0.5;
  jde:=2451547.507+(365.2596358+0.0000000158*k)*k;
  result:=delphi_date(jde);
  end;

function nextperihel(date:TDateTime):TDateTime;
var
  temp_date: TDateTime;
begin
  temp_date:=date-365.25;
  result:=temp_date;
  while result<date do begin
    result:=nextXXXhel(temp_date,false);
    temp_date:=temp_date+365.25;
    end;
  end;

function nextaphel(date:TDateTime):TDateTime;
var
  temp_date: TDateTime;
begin
  temp_date:=date-365.25;
  result:=temp_date;
  while result<date do begin
    result:=nextXXXhel(temp_date,true);
    temp_date:=temp_date+365.25;
    end;
  end;

{ The seasons }

function CalcSolarTerm(year: integer; term: TSolarTerm):TDateTime;
function dist(degree1,degree2:extended):extended;
begin
  result:=put_in_360(degree2-degree1);
  if result>180 then
    result:=result-360;
  end;

const
  epsilon = 3E-10;
var
  degree: extended;
  coord: T_coord;
begin
  degree:=15*ord(term);
  result:=tropic_year/24*ord(term)+31+28+21;  (* approximate date of term *)
  if result>365 then
    result:=result+encodedate(year-1,1,1)
  else
    result:=result+encodedate(year,1,1);
  coord:=sun_coordinate(result);
  while abs(dist(coord.longitude,degree))>epsilon do begin
    result:=result+58*sin_d(degree-coord.longitude);
    coord:=sun_coordinate(result);
    end;
  end;

function StartSeason(year: integer; season:TSeason):TDateTime;
begin
  result:=0;
  case season of
    spring: result:=CalcSolarTerm(year,st_z2);
    summer: result:=CalcSolarTerm(year,st_z5);
    autumn: result:=CalcSolarTerm(year,st_z8);
    winter: result:=CalcSolarTerm(year,st_z11);
    end;
  end;


function MajorSolarTerm(month: integer):TSolarTerm;
var
  count: integer;
begin
  count:=(month-2)*2 + ord(st_z1);
  result:=TSolarTerm(count mod 24);
  end;

function MajorSolarTermAfter(date: TDateTime):TDateTime;
var
  y,m,d: word;
begin
  DecodeDateCorrect(date,y,m,d);
  repeat
    result:=CalcSolarTerm(y,MajorSolarTerm(m));
    inc(m);
    if m>12 then begin
      inc(y);
      m:=1;
      end;
  until result>=date;
  end;

function MajorSolarTermBefore(date: TDateTime):TDateTime;
var
  y,m,d: word;
begin
  DecodeDateCorrect(date,y,m,d);
  repeat
    result:=CalcSolarTerm(y,MajorSolarTerm(m));
    dec(m);
    if m<1 then begin
      dec(y);
      m:=12;
      end;
  until result<date;
  end;
(*@\\\*)


{ Rising and setting of moon and sun }

{ Based upon chapter 15 (14) of Meeus }
function Calc_Set_Rise(date:TDateTime; latitude, longitude:extended;
                       sun: boolean; kind: T_RiseSet):TDateTime;
var
  h: Extended;
  pos1, pos2, pos3: t_coord;
  h0, theta0, cos_h0, cap_h0: extended;
  m0,m1,m2: extended;

function interpolation(y1,y2,y3,n: extended):extended;
var
  a,b,c: extended;
begin
  a:=y2-y1;
  b:=y3-y2;
  if a>100 then  a:=a-360;
  if a<-100 then  a:=a+360;
  if b>100 then  b:=b-360;
  if b<-100 then  b:=b+360;
  c:=b-a;
  result:=y2+0.5*n*(a+b+n*c);
  end;

function correction(m:extended; kind:integer):extended;
var
  alpha,delta,h, height: extended;
begin
  alpha:=interpolation(pos1.rektaszension,
                       pos2.rektaszension,
                       pos3.rektaszension,
                       m);
  delta:=interpolation(pos1.declination,
                       pos2.declination,
                       pos3.declination,
                       m);
  h:=put_in_360((theta0+360.985647*m)-longitude-alpha);
  if h>180 then h:=h-360;

  height:=arcsin_d(sin_d(latitude)*sin_d(delta)
                   +cos_d(latitude)*cos_d(delta)*cos_d(h));

  case kind of
    0:   result:=-h/360;
    1,2: result:=(height-h0)/(360*cos_d(delta)*cos_d(latitude)*sin_d(h));
    else result:=0;   (* this cannot happen *)
    end;
  end;

const
  sun_diameter = 0.8333;
  civil_twilight_elevation = -6.0;
  nautical_twilight_elevation = -12.0;
  astronomical_twilight_elevation = -18.0;
begin
  case kind of
    _rise, _set: begin
      if sun then
        h0:=-sun_diameter
      else begin
        pos1:=moon_coordinate(date);
        correct_position(pos1,date,latitude,longitude,0);
        h0:=0.7275*pos1.parallax-34/60;
        end;
      end;
    _rise_civil,
    _set_civil:   h0:=civil_twilight_elevation;
    _rise_nautical,
    _set_nautical: h0:=nautical_twilight_elevation;
    _rise_astro,
    _set_astro:   h0:=astronomical_twilight_elevation;
    else          h0:=0;  (* don't care for _transit *)
    end;

  h:=int(date);
  theta0:=star_time(h);
  if sun then begin
    pos1:=sun_coordinate(h-1);
    pos2:=sun_coordinate(h);
    pos3:=sun_coordinate(h+1);
    end
  else begin
    pos1:=moon_coordinate(h-1);
    correct_position(pos1,h-1,latitude,longitude,0);
    pos2:=moon_coordinate(h);
    correct_position(pos2,h,latitude,longitude,0);
    pos3:=moon_coordinate(h+1);
    correct_position(pos3,h+1,latitude,longitude,0);
    end;

  cos_h0:=(sin_d(h0)-sin_d(latitude)*sin_d(pos2.declination))/
          (cos_d(latitude)*cos_d(pos2.declination));
  if (cos_h0<-1) or (cos_h0>1) then
    raise E_NoRiseSet.Create('No rises or sets calculable');
  cap_h0:=arccos_d(cos_h0);

  m0:=(pos2.rektaszension+longitude-theta0)/360;
  m1:=m0-cap_h0/360;
  m2:=m0+cap_h0/360;

  m0:=frac(m0);
  if m0<0 then m0:=m0+1;
  m1:=frac(m1);
  if m1<0 then m1:=m1+1;
  m2:=frac(m2);
  if m2<0 then m2:=m2+1;

  m0:=m0+correction(m0,0);
  m1:=m1+correction(m1,1);
  m2:=m2+correction(m2,2);

  case kind of
    _rise,
    _rise_civil,
    _rise_nautical,
    _rise_astro:    result:=h+m1;
    _set,
    _set_civil,
    _set_nautical,
    _set_astro:     result:=h+m2;
    _transit: result:=h+m0;
    else      result:=0;    (* this can't happen *)
    end;

  end;

function Sun_Rise(date:TDateTime; latitude, longitude:extended):TDateTime;
begin
  result:=Calc_Set_Rise(date,latitude,longitude,true,_rise);
  end;

function Sun_Set(date:TDateTime; latitude, longitude:extended):TDateTime;
begin
  result:=Calc_Set_Rise(date,latitude,longitude,true,_set);
  end;

function Sun_Transit(date:TDateTime; latitude, longitude:extended):TDateTime;
begin
  result:=Calc_Set_Rise(date,latitude,longitude,true,_transit);
  end;

function Moon_Rise(date:TDateTime; latitude, longitude:extended):TDateTime;
begin
  result:=Calc_Set_Rise(date,latitude,longitude,false,_rise);
  end;

function Moon_Set(date:TDateTime; latitude, longitude:extended):TDateTime;
begin
  result:=Calc_Set_Rise(date,latitude,longitude,false,_set);
  end;

function Moon_Transit(date:TDateTime; latitude, longitude:extended):TDateTime;
begin
  result:=Calc_Set_Rise(date,latitude,longitude,false,_transit);
  end;

function Morning_Twilight_Civil(date:TDateTime; latitude, longitude:extended):TDateTime;
begin
  result:=Calc_Set_Rise(date,latitude,longitude,true,_rise_civil);
  end;

function Evening_Twilight_Civil(date:TDateTime; latitude, longitude:extended):TDateTime;
begin
  result:=Calc_Set_Rise(date,latitude,longitude,true,_set_civil);
  end;

function Morning_Twilight_Nautical(date:TDateTime; latitude, longitude:extended):TDateTime;
begin
  result:=Calc_Set_Rise(date,latitude,longitude,true,_rise_nautical);
  end;

function Evening_Twilight_Nautical(date:TDateTime; latitude, longitude:extended):TDateTime;
begin
  result:=Calc_Set_Rise(date,latitude,longitude,true,_set_nautical);
  end;

function Morning_Twilight_Astronomical(date:TDateTime; latitude, longitude:extended):TDateTime;
begin
  result:=Calc_Set_Rise(date,latitude,longitude,true,_rise_astro);
  end;

function Evening_Twilight_Astronomical(date:TDateTime; latitude, longitude:extended):TDateTime;
begin
  result:=Calc_Set_Rise(date,latitude,longitude,true,_set_astro);
  end;


{ Checking for eclipses }

function Eclipse(var date:TDateTime; sun:boolean):TEclipse;
var
  jde,kk,m,ms,f,o,e: extended;
  t,f1,a1: extended;
  p,q,w,gamma,u: extended;
begin
  if sun then
    calc_phase_data(date,NewMoon,jde,kk,m,ms,f,o,e)
  else
    calc_phase_data(date,FullMoon,jde,kk,m,ms,f,o,e);
  t:=kk/1236.85;
  if abs(sin_d(f))>0.36 then
    result:=none
  (*@/// else *)
  else begin
    f1:=f-0.02665*sin_d(o);
    a1:=299.77+0.107408*kk-0.009173*t*t;
    if sun then
      jde:=jde - 0.4075     * sin_d(ms)
               + 0.1721 * e * sin_d(m)
    else
      jde:=jde - 0.4065     * sin_d(ms)
               + 0.1727 * e * sin_d(m);
    jde:=jde   + 0.0161     * sin_d(2*ms)
               - 0.0097     * sin_d(2*f1)
               + 0.0073 * e * sin_d(ms-m)
               - 0.0050 * e * sin_d(ms+m)
               - 0.0023     * sin_d(ms-2*f1)
               + 0.0021 * e * sin_d(2*m)
               + 0.0012     * sin_d(ms+2*f1)
               + 0.0006 * e * sin_d(2*ms+m)
               - 0.0004     * sin_d(3*ms)
               - 0.0003 * e * sin_d(m+2*f1)
               + 0.0003     * sin_d(a1)
               - 0.0002 * e * sin_d(m-2*f1)
               - 0.0002 * e * sin_d(2*ms-m)
               - 0.0002     * sin_d(o);
    p:=        + 0.2070 * e * sin_d(m)
               + 0.0024 * e * sin_d(2*m)
               - 0.0392     * sin_d(ms)
               + 0.0116     * sin_d(2*ms)
               - 0.0073 * e * sin_d(ms+m)
               + 0.0067 * e * sin_d(ms-m)
               + 0.0118     * sin_d(2*f1);
    q:=        + 5.2207
               - 0.0048 * e * cos_d(m)
               + 0.0020 * e * cos_d(2*m)
               - 0.3299     * cos_d(ms)
               - 0.0060 * e * cos_d(ms+m)
               + 0.0041 * e * cos_d(ms-m);
    w:=abs(cos_d(f1));
    gamma:=(p*cos_d(f1)+q*sin_d(f1))*(1-0.0048*w);
    u:= + 0.0059
        + 0.0046 * e * cos_d(m)
        - 0.0182     * cos_d(ms)
        + 0.0004     * cos_d(2*ms)
        - 0.0005     * cos_d(m+ms);
    (*@/// if sun then *)
    if sun then begin
      if abs(gamma)<0.9972 then begin
        if u<0 then
          result:=total
        else if u>0.0047 then
          result:=circular
        else if u<0.00464*sqrt(1-gamma*gamma) then
          result:=circulartotal
        else
          result:=circular;
        end
      else if abs(gamma)>1.5433+u then
        result:=none
      else if abs(gamma)<0.9972+abs(u) then
        result:=noncentral
      else
        result:=partial;
      end

    else begin
      if (1.0128 - u - abs(gamma)) / 0.5450 > 0 then
        result:=total
      else if (1.5573 + u - abs(gamma)) / 0.5450 > 0 then
        result:=halfshadow
      else
        result:=none;
      end;

    end;

  date:=delphi_date(jde);
  end;

function NextEclipse(var date:TDateTime; sun:boolean):TEclipse;
var
  temp_date: TDateTime;
begin
  result:=none;
  temp_date:=date-28*2;
  while temp_date<date do begin
    temp_date:=temp_date+28;
    result:=Eclipse(temp_date,sun);
    end;
  date:=temp_date;
  end;


{ Horizontal positions }

procedure Moon_Position_Horizontal(date:TdateTime; longitude,latitude: extended; var elevation,azimuth: extended);
var
  pos1: T_Coord;
begin
  pos1:=moon_coordinate(date);
  calc_horizontal(pos1,date,longitude,latitude);
  end;

procedure Sun_Position_Horizontal(date:TdateTime; longitude,latitude: extended; var elevation,azimuth: extended);
var
  pos1: T_Coord;
begin
  pos1:=sun_coordinate(date);
  calc_horizontal(pos1,date,longitude,latitude);
  end;

{ Chinese calendar }

function UTCtoChina(date: TdateTime):TDateTime;
var
  y,m,d: word;
begin
  decodedatecorrect(date,y,m,d);
  if y<1929 then
    result:=date-beijing_longitude/360
  else
    result:=date+120/360;
  end;

function ChinaToUTC(date: TdateTime):TDateTime;
var
  y,m,d: word;
begin
  decodedatecorrect(date,y,m,d);
  if y<1929 then
    result:=date+beijing_longitude/360
  else
    result:=date-120/360;
  end;

function ChinaNewMoonBefore(date:TDateTime):TdateTime;
begin
  result:=Trunc(UTCtoChina(last_phase(ChinaToUTC(date),Newmoon)));
  end;

function ChinaNewMoonAfter(date:TDateTime):TdateTime;
begin
  result:=Trunc(UTCtoChina(next_phase(ChinaToUTC(date),Newmoon)));
  end;

function ChinaSolarTermAfter(date:TDateTime):TdateTime;
begin
  result:=Trunc(UTCtoChina(MajorSolarTermAfter(ChinaToUTC(date))));
  end;

function ChinaSolarTermBefore(date:TDateTime):TdateTime;
begin
  result:=Trunc(UTCtoChina(MajorSolarTermBefore(ChinaToUTC(date))));
  end;

function hasnomajorsolarterm(date:TDateTime):boolean;
var
  term_before_date,
  term_before_next_month,
  next_month: TDateTime;
begin
  term_before_date:=ChinaSolarTermBefore(date);
  next_month:=ChinaNewMoonAfter(date+2);
  term_before_next_month:=ChinaSolarTermBefore(next_month);
  result:=term_before_date=term_before_next_month;
  end;

function haspriorleapmonth(date1,date2: TdateTime):boolean;
{ Recursive }
begin
  if (date2>date1) then
    result:=haspriorleapmonth(date1,ChinaNewMoonBefore(date2-1))
  else
    result:=false;
  if not result then
    result:=hasnomajorsolarterm(date2);
  end;

function ChineseDate(date: TdateTime): TChineseDate;
var
  s1,s2,s3: TDateTime;
  m0,m1,m2: TdateTime;
  y,m,d: word;
  daycycle, monthcycle: integer;
begin
  Decodedatecorrect(date,y,m,d);
  date:=trunc(date);
  (* Winter solstices (Z12) around the date *)
  s1:=ChinaSolarTermAfter(encodedatecorrect(y-1,12,15));
  s2:=ChinaSolarTermAfter(encodedatecorrect(y  ,12,15));
  s3:=ChinaSolarTermAfter(encodedatecorrect(y+1,12,15));
  (* Start of Months around winter solstices *)
  if (s1<=date) and (date<s2) then begin
    m1:=ChinaNewMoonAfter(s1+1);
    m2:=ChinaNewMoonBefore(s2+1);
    end
  else begin
    m1:=ChinaNewMoonAfter(s2+1);
    m2:=ChinaNewMoonBefore(s3+1);
    end;
  (* Start of current month *)
  m0:=ChinaNewMoonBefore(date+1);
  result.leapyear:=round((m2-m1)/mean_lunation)=12;
  result.month:=round((m0-m1)/mean_lunation);
  result.leap:=result.leapyear and hasnomajorsolarterm(m0) and
               (not haspriorleapmonth(m1,ChinaNewMoonBefore(m0)));
  if result.leapyear and (haspriorleapmonth(m1,m0) or result.leap) then
    result.month:=result.month-1;
  result.month:=adjusted_mod(result.month,12);
  result.day:=round(date-m0+1);
  result.epoch_years:=y+2636;
  if (result.month<11) or (date>EncodedateCorrect(y,7,1)) then
    inc(result.epoch_years);
  result.cycle:=((result.epoch_years-1) div 60)+1;
  result.year:=adjusted_mod(result.epoch_years,60);
  result.yearcycle.zodiac:=TChineseZodiac((result.year-1) mod 12);
  result.yearcycle.stem:=TChineseStem((result.year-1) mod 10);
  (* 2000-1-7 = daycycle jia-zi *)
  daycycle:=adjusted_mod(round(trunc(date)-datetime_2000_01_01-6),60);
  result.daycycle.zodiac:=TChineseZodiac(daycycle mod 12);
  result.daycycle.stem:=TChineseStem(daycycle mod 10);
  (* 1998-12-19 = monthcycle jia-zi *)
  monthcycle:=adjusted_mod(1+round((m0-datetime_1999_01_01-13)/mean_lunation),60);
  result.monthcycle.zodiac:=TChineseZodiac(monthcycle mod 12);
  result.monthcycle.stem:=TChineseStem(monthcycle mod 10);
  end;

function EncodeDateChinese(date:TChineseDate):TDateTime;
var
  y: integer;
  newyear, month_begin: TdateTime;
  chinese: TChineseDate;
begin
  y:=60*(date.cycle-1)+(date.year-1)-2636;
  newyear:=ChineseNewYear(y);
  month_begin:=ChinaNewMoonAfter(newyear+29*(date.month-1));
  chinese:=ChineseDate(month_begin);
  if (chinese.month=date.month) and (chinese.leap=date.leap) then
    result:=month_begin+date.day-1
  else
    result:=ChinaNewMoonAfter(month_begin+5)+date.day-1;
  (* check if the input date was valid *)
  chinese:=ChineseDate(result);
  if (chinese.day<>date.day) or (chinese.month<>date.month) or
     (chinese.leap<>date.leap) or (chinese.year<>date.year) or
     (chinese.cycle<>date.cycle) then
    raise EConvertError.Create('Invalid chinese date');
  end;

function ChineseNewYear(year: integer): TDateTime;
var
  s1,s2: TDateTime;
  m1,m2,m11: TdateTime;
begin
  (* Winter solstices (Z12) around the January 1st of the year *)
  s1:=ChinaSolarTermAfter(encodedatecorrect(year-1,12,15));
  s2:=ChinaSolarTermAfter(encodedatecorrect(year  ,12,15));
  m1:=ChinaNewMoonAfter(s1+1);
  m2:=ChinaNewMoonAfter(m1+4);
  m11:=ChinaNewMoonBefore(s2+1);
  if (round((m11-m1)/mean_lunation)=12) and
     (hasnomajorsolarterm(m1) or hasnomajorsolarterm(m2)) then
    result:=ChinaNewMoonAfter(m2+1)
  else
    result:=m2;
  end;

initialization

  julian_offset:=2451544.5-EncodeDate(2000,1,1);
  datetime_2000_01_01:=EncodedateCorrect(2000,1,1);
  datetime_1999_01_01:=EncodedateCorrect(1999,1,1);
  datetime_first_lunation:=EncodeDate(1923,1,17);
  check_TDatetime;

end.
