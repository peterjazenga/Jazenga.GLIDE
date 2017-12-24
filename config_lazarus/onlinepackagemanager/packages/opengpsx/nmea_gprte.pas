{ <NMEA_GPRTE>

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



{******************* RTE - route message ***********************}
unit nmea_gprte;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gpsutilities;

type
TNMEAGPRTE = class(TObject)
private
  FSentence : string;
  FMessageCount : string;
  FMessageNumber : string;
  FMessageType : string;
  FRouteId : string;
  FWaypointCount : string;
  FWaypointNames : TStrings;
  FCheckSum : string;
  FSuccess : boolean;
  procedure Interprete(RTE_Sentence : string);
  procedure InitVariables;
public
  property Sentence : string read FSentence write Interprete;
  property MessagesCount : string read FMessageCount;
  property MessageNumber : string read FMessageNumber;
  property MessageType : string read FMessageType;
  property RouteID : string read FRouteID;
  property WaypointCount : string read FWaypointCount;
  property WaypointNames : TStrings read FWaypointNames;
  property Checksum : string read FChecksum;
  property Success : boolean read FSuccess write FSuccess;
  constructor Create;
  destructor Destroy; override;
end;

implementation
constructor TNMEAGPRTE.Create;
begin
  inherited Create;
  FSuccess := false;
  FWaypointNames := TStringList.Create;
end;

destructor TNMEAGPRTE.Destroy;
begin
  FWaypointNames.Free;
  inherited Destroy;
end;

procedure TNMEAGPRTE.InitVariables;
begin
  FMessageCount := '';
  FMessageNumber := '';
  FMessageType := '';
  FRouteId := '';
  FWaypointCount := '';
  FWaypointNames.Clear;
  FCheckSum := '';
end;

procedure TNMEAGPRTE.Interprete(RTE_Sentence : string);
var
  szMessageCount : string;
  szMessageNumber : string;
  szMessageType : string;
  szRouteID : string;
  szWaypointCount : string;
  szWaypointName : string;
  szCheckSum : string;
  szTemp : string;
  i, j, cWPParts, cTotalParts : integer;
begin
  InitVariables;
  if (Parse(RTE_Sentence, 1, ',') = '$GPRTE') then
  begin
    cTotalParts := CountParts(RTE_Sentence, ',');
    cWPParts := cTotalParts - 4;
    FSentence := RTE_Sentence;
    szMessageCount := Parse(FSentence, 2, ','); // Total Messages of all waypoints.
    if (IsNumeric(szMessageCount)) then
      FMessageCount := szMessageCount;

    szMessageNumber := Parse(FSentence, 3, ','); // Current Message
    if (IsNumeric(szMessageNumber)) then
      FMessageNumber := szMessageNumber;

    szMessageType := Parse(FSentence, 4, ','); // Message Type
    if (szMessageType = 'c') or (szMessageType = 'w') then
      FMessageType := szMessageType;

    szRouteID := Parse(FSentence, 5, ','); // Route ID
    if (IsNumeric(szRouteID)) then
      FRouteID := szRouteID;

    for i := 6 to cTotalParts  do
    begin
      if (i < cWPParts) then
      begin
        szWaypointName := Parse(FSentence, i, ','); // Waypoint Name
        FWaypointNames.Add(szWaypointName);
      end
      else
      begin
        szTemp := Parse(FSentence, i, ','); // The last Waypoint Name.
        szWaypointName := Parse(szTemp, 1, '*');
        FWaypointNames.Add(szWaypointName);

        szCheckSum := Parse(szTemp, 2, '*');
        FCheckSum := szCheckSum;
      end;
    end;
    FSuccess := true;
  end
  else
  begin
    FSuccess := false;
  end;
end;


end.

