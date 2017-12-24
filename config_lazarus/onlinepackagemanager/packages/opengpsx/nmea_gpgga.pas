{ <NMEA_GPGGA>

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



{************GGA - Fix information*********************}
//This unit implement by NMEADecode unit.
unit nmea_gpgga;

interface

uses SysUtils, gpsutilities;

type
  TNMEAGPGGA = class(TObject)
  private
    //local variables to hold property values
    FSentence : string;
    FUTC : string;
    FLatitudeDeg : string; //convert to degree
    FLatitudeDM : string; //original format
    FLatHemis : string;
    FLongitudeDeg : string;
    FLongitudeDM : string;
    FLonHemis : string;
    FQuality : string;
    FSatsInUse : string;
    FHDOP : string;
    FAltitude : string;
    FSeparationUnits : string;
    FDiffAge : string;
    FStationID : string;
    FSeparation : string;
    FAltitudeUnits : string;
    FChecksum : string;
    FSuccess : boolean;
    procedure Interprete(GGA_Sentence : string);
    procedure InitVariables;
  public
    property CheckSum : string read FChecksum;
    property AltitudeUnits : string read FAltitudeUnits;
    property Separation : string read FSeparation;
    property StationID : string read FStationID;
    property DiffAge : string read FDiffAge;
    property SeparationUnits : string read FSeparationUnits;
    property Altitude : string read FAltitude;
    property HDOP : string read FHDOP;
    property SatsInUse : string read FSatsInUse;
    property FixQuality : string read FQuality;
    property LongHemis : string read FLonHemis;
    property LongitudeDegree : string read FLongitudeDeg;
    property LongitudeDM : string read FLongitudeDM;
    property LatHemis : string read FLatHemis;
    property LatitudeDegree : string read FLatitudeDeg;
    property LatitudeDM : string read FLatitudeDM;
    property UTC : string read FUTC;
    property Sentence : string read FSentence write Interprete;
    property Success : boolean read FSuccess write FSuccess;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

constructor TNMEAGPGGA.Create;
begin
  inherited Create;
  FSuccess := false;
end;

destructor TNMEAGPGGA.Destroy;
begin
  inherited Destroy;
end;

procedure TNMEAGPGGA.InitVariables;
begin
  FUTC := '';
  FLatitudeDeg := '';
  FLatitudeDM := '';
  FLatHemis := '';
  FLongitudeDeg := '';
  FLongitudeDM := '';
  FLonHemis := '';
  FQuality := '';
  FSatsInUse := '';
  FHDOP := '';
  FAltitude := '';
  FSeparationUnits := '';
  FDiffAge := '';
  FStationID := '';
  FSeparation := '';
  FAltitudeUnits := '';
  FChecksum := '';
end;

procedure TNMEAGPGGA.Interprete(GGA_Sentence : string);
var
  szUTC : string;
  szLatitude : string;
  szLatHemis : string;
  szLongitude : string;
  szLonHemis : string;
  szQuality : string;
  szSatsInUse : string;
  szHDOP : string;
  szAltitude : string;
  szSeparationUnits : string;
  szDiffAge : string;
  szStationID : string;
  szSeparation : string;
  szAltitudeUnits : string;
  szTempStationID : string;
  szChecksum : string;
  szTempChecksum : string;
  dLatitude, dLongitude : double;
begin
  InitVariables;
  if (Parse(GGA_Sentence, 1, ',') = '$GPGGA') And (CountParts(GGA_Sentence, ',') = 15) then
  begin
    FSentence := GGA_Sentence;
    //parse NMEA sentence and check quality
    //if qc conditions not met, leave var : empty...
    szUTC := Parse(FSentence, 2, ','); //UTC
    if (IsNumeric(szUTC)) then
      FUTC := szUTC;

    szLatitude := Parse(FSentence, 3, ',');        //Latitude
    if (IsNumeric(szLatitude)) then
    begin
      FLatitudeDM := szLatitude;
      dLatitude := DM2DD(szLatitude);
    end;

    szLatHemis := Parse(FSentence, 4, ',');        //LatHemis
    if (szLatHemis = 'N') Or (szLatHemis = 'S') then
    begin
      FLatHemis := szLatHemis;
      if (szLatHemis = 'S') then dLatitude := -1 * dLatitude;
      FLatitudeDeg := format('%9.6f', [dLatitude]);
    end;

    szLongitude := Parse(FSentence, 5, ',');       //Longitude
    if (IsNumeric(szLongitude)) then
    begin
      FLongitudeDM := szLongitude;
      dLongitude := DM2DD(szLongitude);
    end;

    szLonHemis := Parse(FSentence, 6, ',');        //LonHemis
    if (szLonHemis = 'E') Or (szLonHemis = 'W') then
    begin
      FLonHemis := szLonHemis;
      if (szLonHemis = 'W') then dLongitude := -1 * dLongitude;
      FLongitudeDeg := format('%9.6f', [dLongitude]);
    end;

    szQuality := Parse(FSentence, 7, ',');         //Quality
    if (IsNumeric(szQuality)) then
      FQuality := szQuality;

    szSatsInUse := Parse(FSentence, 8, ',');       //SatsInUse
    if (IsNumeric(szSatsInUse)) then
      FSatsInUse := szSatsInUse;

    szHDOP := Parse(FSentence, 9, ',');            //HDOP
    if (IsNumeric(szHDOP)) then
      FHDOP := szHDOP;

    szAltitude := Parse(FSentence, 10, ',');       //Altitude
    if (IsNumeric(szAltitude)) then
      FAltitude := szAltitude;

    szAltitudeUnits := Parse(FSentence, 11, ',');  //AltitudeUnits
    FAltitudeUnits := szAltitudeUnits;

    szSeparation := Parse(FSentence, 12, ',');     //Separation
    if (IsNumeric(szSeparation)) then
      FSeparation := szSeparation;

    szSeparationUnits := Parse(FSentence, 13, ','); //SeparationUnits
    FSeparationUnits := szSeparationUnits;

    szDiffAge := Parse(FSentence, 14, ',');        //DiffAge
    if (IsNumeric(szDiffAge)) then
      FDiffAge := szDiffAge;

    szTempStationID := Parse(FSentence, 15, ',');   //temp var
    szStationID := Parse(szTempStationID, 1, '*'); //StationID
    FStationID := szStationID;

    szTempChecksum := Parse(FSentence, 15, ',');   //temp var
    szChecksum := Parse(szTempChecksum, 2, '*');  //Checksum
    FChecksum := szChecksum;

    fSuccess := true;
  end
  else
  begin
    fSuccess := false;
    //raise Exception.Create('GPGGA::Sentence. Invalid or corrupt GPGGA sentence.');
  end;
end;

end.
 
