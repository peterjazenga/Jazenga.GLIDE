{ <NMEA_GPVTG>

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



{**************** VTG - Vector track an Speed over the Ground ***********}
unit nmea_gpvtg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gpsutilities;
type
  TNMEAGPVTG = class(TObject)
  private
    FTrueCourse : string;
    FReferenceTrue : string;
    FMagneticCourse : string;
    FReferenceMagnetic : string;
    FSpeedKnot : string;
    FUnitsKnot : string;
    FSpeedKmh : string;
    FUnitsKmh : string;
    FMode : string;
    FChecksum : string;
    FSuccess : boolean;
    FSentence : string;
    procedure Interprete(VTG_Sentence : string);
    procedure InitVariables;
  public
    property TrueCourse : string read FTrueCourse;
    property ReferenceTrue : string read FReferenceTrue;
    property MagneticCourse : string read FMagneticCourse;
    property ReferenceMagnetic : string read FReferenceMagnetic;
    property SpeedKnot : string read FSpeedKnot;
    property UnitsKnot : string read FUnitsKnot;
    property SpeedKmh : string read FSpeedKmh;
    property UnitsKmh : string read FUnitsKmh;
    property Mode : string read FMode;
    property Checksum : string read FChecksum;
    property Success : boolean read FSuccess write FSuccess;
    property Sentence : string read FSentence write Interprete;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

constructor TNMEAGPVTG.Create;
begin
  inherited Create;
  FSuccess := false;
end;

destructor TNMEAGPVTG.Destroy;
begin
  inherited Destroy;
end;

procedure TNMEAGPVTG.InitVariables;
begin
  FTrueCourse := '';
  FReferenceTrue := '';
  FMagneticCourse := '';
  FReferenceMagnetic := '';
  FSpeedKnot := '';
  FUnitsKnot := '';
  FSpeedKmh := '';
  FUnitsKmh := '';
  FMode := '';
  FChecksum := '';
end;

procedure TNMEAGPVTG.Interprete(VTG_Sentence : string);
var
  szTrueCourse : string;
  szReferenceTrue : string;
  szMagneticCourse : string;
  szReferenceMagnetic : string;
  szSpeedKnot : string;
  szUnitsKnot : string;
  szSpeedKmh : string;
  szUnitsKmh : string;
  szMode : string;
  szChecksum : string;
  szSeparation : string;
  sztemp : string;
  cParts : integer;
begin
  InitVariables;
  if (Parse(VTG_Sentence, 1, ',') = '$GPVTG') then
  begin
    FSentence := VTG_Sentence;
    cParts := CountParts(FSentence, ',');
    //Number of parts = 10 is NMEA version later than 2.3.
    if (cParts = 9) or (cParts = 10) then
    begin
      szTrueCourse := Parse(FSentence, 2, ','); //Course (True Heading)
      if (IsNumeric(szTrueCourse)) then
        FTrueCourse := szTrueCourse;

      szReferenceTrue := Parse(FSentence, 3, ','); //Reference (True Heading)
      if (szReferenceTrue = 'T') then
        FReferenceTrue := szReferenceTrue;

      szMagneticCourse := Parse(FSentence, 4, ','); //Course (Magnetic Heading)
      if IsNumeric(szMagneticCourse) then
        FMagneticCourse := szMagneticCourse;

      szReferenceMagnetic := Parse(FSentence, 5, ','); //Reference (Magnetic Heading)
      if szReferenceMagnetic = 'M' then
        FReferenceMagnetic := szReferenceMagnetic;

      szSpeedKnot := Parse(FSentence, 6, ','); //Speed in Knot
      if (IsNumeric(szSpeedKnot)) then
        FSpeedKnot := szSpeedKnot;

      szUnitsKnot := Parse(FSentence, 7, ','); //Units (Knot)
      if (szUnitsKnot = 'N') then
        FUnitsKnot := szUnitsKnot;

      szSpeedKmh := Parse(FSentence, 8, ','); //Speed in Km/h
      if (IsNumeric(szSpeedKmh)) then
        FSpeedKmh := szSpeedKmh;

      if (cParts = 9) then
      begin
        sztemp := Parse(FSentence, 9, ',');   //Units (Km/h)
        szUnitsKmh := Parse(szTemp, 1, '*');
        FUnitsKmh := szUnitsKmh;

        FMode := '';

        szTemp := Parse(FSentence, 9, ',');   //temp var
        szChecksum := Parse(szTemp, 2, '*');  //Checksum
        FChecksum := szChecksum;
      end
      else if (cParts = 10) then
      begin
        szUnitsKmh := Parse(FSentence, 9, ','); //Units (km/h)
        if (szUnitsKmh = 'K') then
          FUnitsKmh := szUnitsKmh;

        sztemp := Parse(FSentence, 10, ',');   //Mode => 'A', 'D' or 'E'
        szMode := Parse(szTemp, 1, '*');
        if (szMode = 'A') or (szMode = 'D') or (szMode = 'E') then
          FMode := szMode;

        szTemp := Parse(FSentence, 10, ',');
        szChecksum := Parse(szTemp, 2, '*');  //Checksum
        FChecksum := szChecksum;
      end;
      FSuccess := true;
    end
    else
    begin
      FSuccess := false;
    end;
  end;
end;
end.

