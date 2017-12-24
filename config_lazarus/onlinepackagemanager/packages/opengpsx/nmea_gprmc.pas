{ <NMEA_GPRMC>

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


{**************** RMC - recommended minimum data for gps *****************}
//This unit implement by NMEADecode unit.
unit nmea_gprmc;

interface

uses SysUtils, gpsutilities;

type
  TNMEAGPRMC = class(TObject)
  private
    FSentence : string;
    FUTC : string;
    FStatus : string;
    FLatitudeDeg : string;
    FLatitudeDM : string;
    FLongitudeDeg : string;
    FLongitudeDM : string;
    FLatHemis : string;
    FLonHemis : string;
    FSpeed : string;
    FTrueCourse : string;
    FUTCDate : string;
    FMagneticVariation : string;
    FMagDeviation : string;
    FChecksum : string;
    FSuccess : boolean;
    procedure Interprete(RMC_Sentence : string);
    procedure InitVariables;
  public
    property CheckSum : string read FCheckSum;
    property MagDeviation : string read FMagDeviation;
    property MagneticVariation : string read FMagneticVariation;
    property UTCDate : string read FUTCDate;
    property TrueCourse : string read FTrueCourse;
    property Speed : string read FSpeed;
    property LongHemis : string read FLonHemis;
    property LatHemis : string read FLatHemis;
    property LongitudeDegree : string read FLongitudeDeg;
    property LatitudeDegree : string read FLatitudeDeg;
    property LongitudeDM : string read FLongitudeDM;
    property LatitudeDM : string read FLatitudeDM;
    property Status : string read FStatus;
    property UTCTime : string read FUTC;
    property Sentence : string read FSentence write Interprete;
    property Success : boolean read FSuccess write FSuccess;
    constructor Create;
    destructor Destroy; override;
  end;
implementation

constructor TNMEAGPRMC.Create;
begin
  inherited create;
  FSuccess := false;
end;

destructor TNMEAGPRMC.Destroy;
begin
  inherited Destroy;
end;

procedure TNMEAGPRMC.InitVariables;
begin
  FUTC := '';
  FStatus := '';
  FLatitudeDeg := '';
  FLatitudeDM := '';
  FLongitudeDeg := '';
  FLongitudeDM := '';
  FLatHemis := '';
  FLonHemis := '';
  FSpeed := '';
  FTrueCourse := '';
  FUTCDate := '';
  FMagneticVariation := '';
  FMagDeviation := '';
  FChecksum := '';
end;

procedure TNMEAGPRMC.Interprete(RMC_Sentence : string);
var
  szUTC : string;
  szStatus : string;
  szLatitude : string;
  szLatHemis : string;
  szLongitude : string;
  szLonHemis : string;
  szSpeed : string;
  szTrueCourse : string;
  szUTCDate : string;
  szMagneticVariation : string;
  szMagDeviation : string;
  szChecksum : string;
  szTempMagDeviation : string;
  szTempChecksum : string;
  dLatitude : double;
  dLongitude : double;
  dtemp : double;
  cNoSentences : integer;
  szTemp : string;
begin
  InitVariables;
  cNoSentences := CountParts(RMC_Sentence, ',');
  if (Parse(RMC_Sentence, 1, ',') = '$GPRMC') And
     ((cNoSentences = 12) or (cNoSentences = 13)) then
  begin
    FSentence := RMC_Sentence;

    //parse NMEA sentence and check quality
    //if qc conditions not met, leave var : empty...
    szUTC := Parse(FSentence, 2, ',');             //Time of Fix (UTC)
    if (IsNumeric(szUTC)) then
      FUTC := szUTC;

    //Status Navigation receiver warning A = OK, V = warning
    szStatus := Parse(FSentence, 3, ',');
    FStatus := szStatus;

    szLatitude := Parse(FSentence, 4, ',');        //Latitude
    FLatitudeDM := szLatitude;
    szLatHemis := Parse(FSentence, 5, ',');        //LatHemis
    if (szLatHemis = 'N') Or (szLatHemis = 'S') then
      FLatHemis := szLatHemis;

    szLongitude := Parse(FSentence, 6, ',');       //Longitude
    FLongitudeDM := szLongitude;
    szLonHemis := Parse(FSentence, 7, ',');        //LonHemis
    if (szLonHemis = 'E') Or (szLonHemis = 'W') then
      FLonHemis := szLonHemis;

    if (IsNumeric(szLatitude)) then
    begin
      dLatitude := DM2DD(szLatitude);
      if (szLatHemis = 'S') then dLatitude := -1 * dLatitude;
      FLatitudeDeg := format('%9.6f', [dLatitude]);
    end;

    if (IsNumeric(szLongitude)) then
    begin
      dLongitude := DM2DD(szLongitude);
      if (szLonHemis = 'W') then dLongitude := -1 * dLongitude;
      FLongitudeDeg := format('%9.6f', [dLongitude]);
    end;
    //Speed over ground in Knots.
    szSpeed := Parse(FSentence, 8, ',');
    if (IsNumeric(szSpeed)) then
      FSpeed := szSpeed;

    //Course Made Good, True
    szTrueCourse := Parse(FSentence, 9, ',');
    if (IsNumeric(szTrueCourse)) then
      FTrueCourse := szTrueCourse
    else if (szTrueCourse = '') then
      FTrueCourse := '-9999'; //Sometime if Speed = 0 then can't compute trackmadegood.

    //Date of fix
    szUTCDate := Parse(FSentence, 10, ',');
    if (IsNumeric(szUTCDate)) then
      FUTCDate := szUTCDate;

    szMagneticVariation := Parse(FSentence, 11, ','); //Magnetic Variation
    if (IsNumeric(szMagneticVariation)) then
      FMagneticVariation := szMagneticVariation
    else if (szMagneticVariation = '') then
      FMagneticVariation := '-9999';

    if (cNoSentences = 12) then
    begin
      szTempMagDeviation := Parse(FSentence, 12, ',');
      szMagDeviation := Parse(szTempMagDeviation, 1, '*'); //Magnetic Deviation => 'E' or 'W'
      FMagDeviation := szMagDeviation;
    end
    else if (cNoSentences = 13) then
    begin
      szTemp := Parse(FSentence, 12, ',');
      if (szTemp = '') then
      begin
        FMagneticVariation := '-9999';
      end;
    end;

    if (cNoSentences = 12) then
    begin
      szTempChecksum := Parse(FSentence, 12, ',');
      szChecksum := Parse(szTempChecksum, 2, '*');
    end;

    if (cNoSentences = 13) then
    begin
      szTempChecksum := Parse(FSentence, 13, ',');
      szChecksum := Parse(szTempChecksum, 2, '*');
    end;
    FChecksum := szChecksum;
    FSuccess := true;
  end
  else
  begin
    FSuccess := false;
    //Raise Exception.Create('GPRMC::Sentence. Invalid or corrupt GPRMC sentence.');
  end;
end;

end.
 
