{ <NMEA_GPBOD>

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


unit nmea_gpbod;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gpsutilities;

type
TNMEAGPBOD = class(TObject)
private
  FSentence : string;
  FBearingTrue : string;
  FUnitTrue : string;
  FBearingMagnetic : string;
  FUnitMagnetic : string;
  FDestWPName : string;
  FStartWPName : string;
  FCheckSum : string;
  FSuccess : boolean;
  procedure Interprete(BOD_Sentence : string);
  procedure InitVariables;
public
  property Sentence : string read FSentence write Interprete;
  property BearingTrue : string read FBearingTrue;
  property UnitTrue : string read FUnitTrue;
  property BearingMagnetic : string read FBearingMagnetic;
  property UnitMagnetic : string read FUnitMagnetic;
  property DestWaypointName : string read FDestWPName;
  property StartWaypointName : string read FStartWPName;
  property Checksum : string read FChecksum;
  property Success : boolean read FSuccess write FSuccess;
  constructor Create;
  destructor Destroy; override;
end;

implementation
constructor TNMEAGPBOD.Create;
begin
  inherited Create;
  FSuccess := false;
end;

destructor TNMEAGPBOD.Destroy;
begin
  inherited Destroy;
end;

procedure TNMEAGPBOD.InitVariables;
begin
  FBearingTrue := '';
  FUnitTrue := '';
  FBearingMagnetic := '';
  FUnitMagnetic := '';
  FDestWPName := '';
  FStartWPName := '';
  FCheckSum := '';
end;

procedure TNMEAGPBOD.Interprete(BOD_Sentence : string);
var
  szBearingTrue : string;
  szUnitTrue : string;
  szBearingMagnetic : string;
  szUnitMagnetic : string;
  szDestWPName : string;
  szStartWPName : string;
  szChecksum : string;
  szSeparation : string;
  sztemp : string;
begin
  InitVariables;
  if (Parse(BOD_Sentence, 1, ',') = '$GPBOD') then
  begin
    if (CountParts(BOD_Sentence, ',') = 7)then
    begin
      FSentence := BOD_Sentence;
      szBearingTrue := Parse(FSentence, 2, ','); // Bearing (True)
      if (IsNumeric(szBearingTrue)) then
        FBearingTrue := szBearingTrue;

      szUnitTrue := Parse(FSentence, 3, ','); // Unit is T
      if (szUnitTrue = 'T') then
        FUnitTrue := szUnitTrue;

      szBearingMagnetic := Parse(FSentence, 4, ','); //Bearing (Magnetic)
      if (IsNumeric(szBearingMagnetic)) then
        FBearingMagnetic := szBearingMagnetic;

      szUnitMagnetic := Parse(FSentence, 5, ','); // Unit is M
      if (szUnitMagnetic = 'M') then
        FUnitMagnetic := szUnitMagnetic;

      szDestWPName := Parse(FSentence, 6, ','); // Destination Waypoint ID
      FDestWPName := szDestWPName;

      szTemp := Parse(FSentence, 7, ','); // Start Waypoint ID
      szStartWPName := Parse(szTemp, 1, '*');
      FStartWPName := szStartWPName;

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

