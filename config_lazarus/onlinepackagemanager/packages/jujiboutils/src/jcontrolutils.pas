{ jcontrolutils

  Copyright (C) 2011 Julio Jiménez Borreguero
  Contact: jujibo at gmail dot com

  This library is free software; you can redistribute it and/or modify it
  under the same terms as the Lazarus Component Library (LCL)

  See the file license-jujiboutils.txt and COPYING.LGPL, included in this distribution,
  for details about the license.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}

unit jcontrolutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, LResources;

function CountChar(const s: string; ch: char): integer;
procedure Split(const Delimiter: char; Input: string; Strings: TStrings);
function NormalizeDate(const Value: string; theValue: TDateTime;
  const theFormat: string = ''): string;
function NormalizeTime(const Value: string; theValue: TTime;
  const theFormat: string = ''): string;
function NormalizeDateTime(const Value: string; theValue: TDateTime;
  const theFormat: string = ''): string;
function NormalizeDateSeparator(const s: string): string;
function IsValidDateString(const Value: string): boolean;
function IsValidTimeString(const Value: string): boolean;
function IsValidDateTimeString(const Value: string): boolean;

implementation

function CountChar(const s: string; ch: char): integer;
var
  i: integer;
begin
  Result := 0;
  for i := 1 to length(s) do
    if s[i] = ch then
      Inc(Result);
end;

procedure Split(const Delimiter: char; Input: string; Strings: TStrings);
begin
  Assert(Assigned(Strings));
  Strings.Clear;
  Strings.Delimiter := Delimiter;
  Strings.DelimitedText := Input;
end;

function NormalizeDate(const Value: string; theValue: TDateTime;
  const theFormat: string): string;
var
  texto: string;
  d, m, y: word;
  ds, ms, ys: string;
  aDate: TDateTime;
  tokens: TStringList;
  aDateFormat: string;
  aChar: char;

  procedure LittleEndianForm;
  begin
    // Note: only numeric input allowed (months names not implemented)
    if tokens[0] <> '' then
      ds := tokens[0];
    if (tokens.Count > 1) and (tokens[1] <> '') then
      ms := tokens[1];
    if (tokens.Count > 2) and (tokens[2] <> '') then
      ys := tokens[2];
    texto := ds + DateSeparator + ms + DateSeparator + ys;
  end;

  procedure MiddleEndianForm;
  begin
    if tokens[0] <> '' then
      ms := tokens[0];
    if (tokens.Count > 1) and (tokens[1] <> '') then
      ds := tokens[1];
    if (tokens.Count > 2) and (tokens[2] <> '') then
      ys := tokens[2];
    texto := ms + DateSeparator + ds + DateSeparator + ys;
  end;

  procedure BigEndianForm;
  begin
    if tokens[0] <> '' then
      ys := tokens[0];
    if (tokens.Count > 1) and (tokens[1] <> '') then
      ms := tokens[1];
    if (tokens.Count > 2) and (tokens[2] <> '') then
      ds := tokens[2];
    texto := ys + DateSeparator + ms + DateSeparator + ds;
  end;

begin
  if theFormat = '' then
    aDateFormat := ShortDateFormat
  else
    aDateFormat := theFormat;
  if theValue = 0 then
    DecodeDate(Now, y, m, d)
  else
    decodedate(theValue, y, m, d);
  ds := IntToStr(d);
  ms := IntToStr(m);
  ys := IntToStr(y);
  texto := Value;
  texto := NormalizeDateSeparator(texto);
  Result := texto;   // default value
  tokens := TStringList.Create;
  Split(DateSeparator, texto, tokens);
  if tokens.Count > 0 then
  begin
    aChar := aDateFormat[1];
    case aChar of
      'd', 'D': LittleEndianForm;
      'm', 'M': MiddleEndianForm;
      'y', 'Y': BigEndianForm;
    end;

    if IsValidDateString(texto) then
    begin
      aDate := StrToDate(texto);
      Result := FormatDateTime(aDateFormat, aDate);
    end;
  end;
  tokens.Free;
end;

function NormalizeTime(const Value: string; theValue: TTime;
  const theFormat: string): string;
var
  hh, mm, ss, ms: word;
  hhs, mms, sss, mss: string;
  texto: string;
  aTimeFormat: string;
  aTime: TTime;
  tokens: TStringList;

  function TimeString: string;
  begin
    Result := hhs + TimeSeparator + mms + TimeSeparator + sss;// + TimeSeparator + mss;
  end;

  procedure TimeForm;
  begin
    if tokens[0] <> '' then
      hhs := tokens[0];
    if (tokens.Count > 1) and (tokens[1] <> '') then
      mms := tokens[1];
    if (tokens.Count > 2) and (tokens[2] <> '') then
      sss := tokens[2];
    if (tokens.Count > 3) and (tokens[3] <> '') then
      mss := tokens[3];
    texto := TimeString;
  end;

begin
  if theFormat = '' then
    aTimeFormat := ShortTimeFormat
  else
    aTimeFormat := theFormat;
  if theValue = 0 then
  begin
    DecodeTime(Now, hh, mm, ss, ms);
  end
  else
  begin
    DecodeTime(theValue, hh, mm, ss, ms);
  end;

  hhs := IntToStr(hh);
  mms := IntToStr(mm);
  sss := IntToStr(ss);
  mss := IntToStr(ms);
  texto := Value;
  Result := texto; // default value
  tokens := TStringList.Create;
  Split(TimeSeparator, texto, tokens);
  if tokens.Count > 0 then
  begin
    TimeForm;
    if IsValidTimeString(texto) then
    begin
      aTime := StrToTime(texto);
      Result := TimeString;// FormatDateTime(aTimeFormat, aTime);
    end
  end;
  tokens.Free;
  //ShowMessage('Hora normalizada: ' + Result);
end;

function NormalizeDateTime(const Value: string; theValue: TDateTime;
  const theFormat: string): string;
var
  tokens: TStringList;
  theDateTime: TDateTime;
  dateString, timeString: string;
begin
  if theValue = 0 then
    theDateTime := Now
  else
    theDateTime := theValue;
  Result := '';
  tokens := TStringList.Create;
  Split(' ', Value, tokens);
  if tokens.Count > 1 then
  begin
    if tokens[0] <> '' then
      dateString := tokens[0];
    if tokens[1] <> '' then
      timeString := tokens[1];
    dateString := NormalizeDate(dateString, theDateTime);
    timeString := NormalizeTime(timeString, theDateTime);
    if IsValidDateString(dateString) and IsValidTimeString(timeString) then
      Result := dateString + ' ' + timeString;
  end;
  tokens.Free;
end;

function NormalizeDateSeparator(const s: string): string;
var
  i: integer;
begin
  Result := s;
  for i := 1 to length(Result) do
    if Result[i] in ['.', ',', '/', '-'] then  // valid date separators
      Result[i] := DateSeparator;
end;

function IsValidDateString(const Value: string): boolean;
var
  bTime: TDateTime;
begin
  Result:= TryStrToDate(Value, bTime);
end;

function IsValidTimeString(const Value: string): boolean;
var
  bTime: TDateTime;
begin
  Result := TryStrToTime(Value, bTime);
end;

function IsValidDateTimeString(const Value: string): boolean;
var
  bTime: TDateTime;
begin
  Result := TryStrToDateTime(Value, bTime);
end;

initialization
  LazarusResources.Add('JCalendarIcon','PNG',[
  #137'PNG'#13#10#26#10#0#0#0#13'IHDR'#0#0#0#16#0#0#0#16#8#6#0#0#0#31#243#255'a'
  +#0#0#0#4'gAMA'#0#0#175#200'7'#5#138#233#0#0#0#25'tEXtSoftware'#0'Adobe Image'
  +'Readyq'#201'e<'#0#0#1#206'IDAT8'#203#165#147#177'kTA'#16#135#191'w'#247#188
  +#20#167#133#16#13#136#24#21#130' &X'#5'!'#18#4'!`'#149'@'#210#8#150'ZD'#16'['
  +'+Q'#208#210#206'?A'#9'B'#130#130#157#160#16'='#177'Pl'#132' DL#Q'#137'E '
  +#228#240#237#236#204'X'#236'{'#151'w'#24#4#201#194#176#203'2|'#243#251#205
  +#236'f'#238#206'^V'#14#240#232#205#230'mw'#174#169#219'Q3C'#13#162#25#170#134
  +#154#19#213'j'#161'ht'#212#236#253#253'+#'#227'9'#128#185#223#152#29'?0'#248
  +'?'#149#239'<'#249'2'#214'S'#160'f'#131#0#237#165#235#224#14#26#193#20'D'#210
  +'9'#20' '#1#10#1#17#182'o'#189'BD'#7'j'#128#170#15#14'G'#14#129#197#20#30'KH'
  +#1'R@'#12#176#242#29#0#137#186#211#3'U+'#1#25#172'o'#128'[Rb'#150#148'h'#181
  +#3'CC'#0#132': '#150#128#133#225'{\>'#127#248#159#222#23#223#173's!'#196'~'#5
  +#162#134#185#147'7S'#210#179'O'#139't'#214#150#185'zn'#158#207'?Wx'#185#250
  +#130#139'#S'#204#140#206'Q'#136#179#213#21#130'$@'#163#178#224'@'#222#204#0
  +#152#25#157#163#27#186#0#252#218#222#224#193#244'C:k'#203#0#20'Q'#9'b'#196'h'
  +';'#0'Q'#195#13#242#198#223#146#187#161'K'#171#217#234#1#11#177#178#221'u@4'
  +#220#157'}'#165#133#206#215'T'#237#227#183#15#28';8'#204#205#167#243'L'#157
  +#186'D'#149#11#208#30#200'kM'#180't'#217'l$'#11#19'''&'#153'89Y'#206'%c'#250
  +#204',Y'#150#245#1#170#201'%@'#140'x)'#231#241#235#31'ij'#238#136#130#168#18
  +#212#8#146#188#183#154'U1'#250#167#224#238#140#29#223#207'i'#173#222#190#247
  +#254#194'n'#203#204#173#246#144#252#237#221#133#213#179'A'#173'-Q'#145#168'h'
  +'4D'#141#168'qW'#192#230#214#239#231#0#217'^'#191#243#31#2'<4&'#179'.'#211
  +#208#0#0#0#0'IEND'#174'B`'#130
]);
end.

