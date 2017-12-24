unit fonctions_date;

interface

{$IFNDEF FPC}
uses SysUtils, Windows;

function GetInternationalFormatSettings:TFormatSettings;
{$ENDIF}

implementation

{$IFNDEF FPC}
function GetInternationalFormatSettings:TFormatSettings;

var
  HourFormat, TimePrefix, TimePostfix: string;
  DefaultLCID: Integer;
begin
  DefaultLCID := $0409;
  with Result do
   Begin
    CurrencyString := GetLocaleStr(DefaultLCID, LOCALE_SCURRENCY, '');
    CurrencyFormat := StrToIntDef(GetLocaleStr(DefaultLCID, LOCALE_ICURRENCY, '0'), 0);
    NegCurrFormat := StrToIntDef(GetLocaleStr(DefaultLCID, LOCALE_INEGCURR, '0'), 0);
    ThousandSeparator := GetLocaleChar(DefaultLCID, LOCALE_STHOUSAND, ',');
    DecimalSeparator := GetLocaleChar(DefaultLCID, LOCALE_SDECIMAL, '.');
    CurrencyDecimals := StrToIntDef(GetLocaleStr(DefaultLCID, LOCALE_ICURRDIGITS, '0'), 0);
    DateSeparator := GetLocaleChar(DefaultLCID, LOCALE_SDATE, '/');
    ShortDateFormat := GetLocaleStr(DefaultLCID, LOCALE_SSHORTDATE, 'm/d/yy');
    LongDateFormat := GetLocaleStr(DefaultLCID, LOCALE_SLONGDATE, 'mmmm d, yyyy');
    TimeSeparator := GetLocaleChar(DefaultLCID, LOCALE_STIME, ':');
    TimeAMString := GetLocaleStr(DefaultLCID, LOCALE_S1159, 'am');
    TimePMString := GetLocaleStr(DefaultLCID, LOCALE_S2359, 'pm');
    TimePrefix := '';
    TimePostfix := '';
    if StrToIntDef(GetLocaleStr(DefaultLCID, LOCALE_ITLZERO, '0'), 0) = 0 then
      HourFormat := 'h' else
      HourFormat := 'hh';
    if StrToIntDef(GetLocaleStr(DefaultLCID, LOCALE_ITIME, '0'), 0) = 0 then
      if StrToIntDef(GetLocaleStr(DefaultLCID, LOCALE_ITIMEMARKPOSN, '0'), 0) = 0 then
        TimePostfix := ' AMPM'
      else
        TimePrefix := 'AMPM ';
    ShortTimeFormat := TimePrefix + HourFormat + ':mm' + TimePostfix;
    LongTimeFormat := TimePrefix + HourFormat + ':mm:ss' + TimePostfix;
    ListSeparator := GetLocaleChar(DefaultLCID, LOCALE_SLIST, ',');
   End;
end;
{$ENDIF}

end.
