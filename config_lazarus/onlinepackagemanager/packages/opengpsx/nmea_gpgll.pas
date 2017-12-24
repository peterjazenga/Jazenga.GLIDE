{ <NMEA_GPGLL>

  Copyright (C) 2010 Prajuab Riabroy

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}



{***************** GLL - Lat/Lon data *****************}
unit nmea_gpgll;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gpsutilities;

type
TNMEAGPGLL = class(TObject)
private
  FSentence : string;
  FLatitudeDeg : string;
  FLatitudeDM : string;
  FLongitudeDeg : string;
  FLongitudeDM : string;
  FLatHemis : string;
  FLonHemis : string;
  FUTCTime : string;
  FStatus : string;
  FMode : string;
  FChecksum : string;
  FSuccess : boolean;
  procedure Interprete(GLL_Sentence : string);
  procedure InitVariables;
public
  property Sentence : string read FSentence write Interprete;
  property LatitudeDegree : string read FLatitudeDeg;
  property LongitudeDegree : string read FLongitudeDeg;
  property LatitudeDM : string read FLatitudeDM;
  property LongitudeDM : string read FLongitudeDM;
  property LatHemis : string read FLatHemis;
  property LongHemis : string read FLonHemis;
  property UTCTime : string read FUTCTime;
  property Status : string read FStatus;
  property Mode : string read FMode;
  property Checksum : string read FChecksum;
  property Success : boolean read FSuccess write FSuccess;
  constructor Create;
  destructor Destroy; override;
end;

implementation
constructor TNMEAGPGLL.Create;
begin
  inherited Create;
  FSuccess := false;
end;

destructor TNMEAGPGLL.Destroy;
begin
  inherited Destroy;
end;

procedure TNMEAGPGLL.InitVariables;
begin
  FLatitudeDeg := '';
  FLatitudeDM := '';
  FLongitudeDeg := '';
  FLongitudeDM := '';
  FLatHemis := '';
  FLonHemis := '';
  FUTCTime := '';
  FStatus := '';
  FMode := '';
  FChecksum := '';
end;

procedure TNMEAGPGLL.Interprete(GLL_Sentence : string);
var
  szLatitudeDeg : string;
  szLatitudeDM : string;
  szLatHemis : string;
  szLongitudeDeg : string;
  szLongitudeDM : string;
  szLonHemis : string;
  szUTCTime : string;
  szStatus : string;
  szMode : string;
  szChecksum : string;
  szSeparation : string;
  sztemp : string;
  dLatitude, dLongitude : double;
  cParts : integer;
begin
  InitVariables;
  if (Parse(GLL_Sentence, 1, ',') = '$GPGLL') then
  begin
  //Number of Parts = 7 is NMEA version earlier than 2.3.
    cParts := CountParts(GLL_Sentence, ',');
    if (cParts = 7) or (cParts = 8) then
    begin
      FSentence := GLL_Sentence;
      szLatitudeDM := Parse(FSentence, 2, ','); // Latitude
      FLatitudeDM := szLatitudeDM;
      if (IsNumeric(szLatitudeDM)) then
        dLatitude := DM2DD(szLatitudeDM);

      szLatHemis := Parse(FSentence, 3, ','); // N/S Indicator
      if (szLatHemis = 'N') or (szLatHemis = 'S') then
      begin
        if (szLatHemis = 'S') then dLatitude := -1 * dLatitude;
        FLatitudeDeg := format('%9.6f', [dLatitude]);
        FLatHemis := szLatHemis;
      end;

      szLongitudeDM := Parse(FSentence, 4, ','); //Longitude
      FLongitudeDM := szLongitudeDM;
      if (IsNumeric(szLatitudeDM)) then
        dLongitude := DM2DD(szLongitudeDM);

      szLonHemis := Parse(FSentence, 5, ','); // W/E Indicator
      if (szLonHemis = 'W') or (szLonHemis = 'E') then
      begin
        if (szLonHemis = 'W') then dLongitude := -1 * dLongitude;
        FLongitudeDeg := format('%9.6f', [dLongitude]);
        FLonHemis := szLonHemis;
      end;

      szUTCTime := Parse(FSentence, 6, ','); // UTC Time
      if (IsNumeric(szUTCTime)) then
      FUTCTime := szUTCTime;

      if (cParts = 7) then
      begin
        sztemp := Parse(FSentence, 7, ','); // Status
        szStatus := Parse(szTemp, 1, '*');
        if (szStatus = 'A') or (szStatus = 'V') then
          FStatus := szStatus;

        FMode := '';

        sztemp := Parse(FSentence, 7, ',');
        szChecksum := Parse(szTemp, 2, '*'); //Checksum
        FChecksum := szChecksum;
      end
      else if (cParts = 8) then
      begin
        szStatus := Parse(FSentence, 7, ','); // Status
        if (szStatus = 'A') or (szStatus = 'V') then
          FStatus := szStatus;
        //Mode is available for later of version 2.3
        sztemp := Parse(FSentence, 8, ','); // Mode
        szMode := Parse(szTemp, 1, '*');
        if (szMode = 'A') or (szMode = 'D') or (szMode = 'E') then
          FMode := szMode;

        sztemp := Parse(FSentence, 8, ',');
        szChecksum := Parse(szTemp, 2, '*'); //Checksum
        FChecksum := szChecksum;
      end;
      FSuccess := true;
    end;
  end
  else
  begin
    FSuccess := false;
  end;
end;


end.

