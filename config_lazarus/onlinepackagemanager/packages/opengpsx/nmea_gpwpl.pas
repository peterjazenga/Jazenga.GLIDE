{ <NMEA_GPWPL>

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


{********************** WPL - Waypoint Location information ******************}
unit nmea_gpwpl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gpsutilities;

type
TNMEAGPWPL = class(TObject)
private
  FSentence : string;
  FWPLatitude : string;
  FWPLongitude : string;
  FLatHemis : string;
  FLongHemis : string;
  FWPName : string;
  FChecksum : string;
  FSuccess : boolean;
  procedure Interprete(WPL_Sentence : string);
public
  property Sentence : string read FSentence write Interprete;
  property Latitude : string read FWPLatitude;
  property Longitude : string read FWPLongitude;
  property LatHemis : string read FLatHemis;
  property LongHemis : string read FLongHemis;
  property WayPointName : string read FWPName;
  property Checksum : string read FChecksum;
  property Success : boolean read FSuccess write FSuccess;
  constructor Create;
  destructor Destroy; override;
end;

implementation
constructor TNMEAGPWPL.Create;
begin
  inherited Create;
  FSuccess := false;
end;

destructor TNMEAGPWPL.Destroy;
begin
  inherited Destroy;
end;

procedure TNMEAGPWPL.Interprete(WPL_Sentence : string);
var
  szLatitude : string;
  szLatHemis : string;
  szLongitude : string;
  szLongHemis : string;
  szWaypointName : string;
  szChecksum : string;
  szSeparation : string;
  sztemp : string;
  dLatitude, dLongitude : double;
begin
  if (Parse(WPL_Sentence, 1, ',') = '$GPWPL') then
  begin
    if (CountParts(WPL_Sentence, ',') = 6)then
    begin
      FSentence := WPL_Sentence;
      szLatitude := Parse(FSentence, 2, ','); // Latitude
      if (IsNumeric(szLatitude)) then
        dLatitude := DM2DD(szLatitude);

      szLatHemis := Parse(FSentence, 3, ','); // N/S Indicator
      if (szLatHemis = 'N') or (szLatHemis = 'S') then
      begin
        if (szLatHemis = 'S') then dLatitude := -1 * dLatitude;
        FWPLatitude := format('%9.6f', [dLatitude]);
        FLatHemis := szLatHemis;
      end;

      szLongitude := Parse(FSentence, 4, ','); //Longitude
      if (IsNumeric(szLatitude)) then
        dLongitude := DM2DD(szLongitude);

      szLongHemis := Parse(FSentence, 5, ','); // W/E Indicator
      if (szLongHemis = 'W') or (szLongHemis = 'E') then
      begin
        if (szLongHemis = 'W') then dLongitude := -1 * dLongitude;
        FWPLongitude := format('%9.6f', [dLongitude]);
        FLongHemis := szLongHemis;
      end;

      szTemp := Parse(FSentence, 6, ','); // Waypoint Name
      szWaypointName := Parse(szTemp, 1, '*');
      FWPName := szWaypointName;

      szChecksum := Parse(szTemp, 2, '*'); //Checksum
      FChecksum := szChecksum;
      FSuccess := true;
    end;
  end
  else
  begin
    FSuccess := false;
  end;
end;


end.

