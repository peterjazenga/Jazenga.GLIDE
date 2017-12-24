{ <NMEA_GPGSA>

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




{*********************** GSA - Overall Satellite data *********************}
//This unit implement by NMEADecode unit.
unit nmea_gpgsa;

interface

uses Classes, SysUtils, gpsutilities;

type
  TNMEAGPGSA = class(TObject)
  private
    //local variables to hold property values
    FPosFixIDs : TStrings;
    FMode : string;
    FFixStatus : string;
    FPDOP : string;
    FVDOP : string;
    FHDOP : string;
    FSentence : string;
    FChecksum : string;
    FSuccess : boolean;
    procedure Interprete(GSA_Sentence : string);
    procedure InitVariables;
  public
    property CheckSum : string read FCheckSum;
    property HDOP : string read FHDOP;
    property VDOP : string read FVDOP;
    property PDOP : string read FPDOP;
    property FixStatus : string read FFixStatus;
    property Mode : string read FMode;
    property PosFixIDs : TStrings read FPosFixIDs;
    property Sentence : string read FSentence write Interprete;
    property Success : boolean read FSuccess write FSuccess;
    constructor Create;
    destructor Destroy; override;
  end;
implementation

constructor TNMEAGPGSA.Create;
begin
  inherited create;
  FSuccess := false;
  FPosFixIDs := TStringList.Create;
end;

destructor TNMEAGPGSA.Destroy;
begin
  FPosFixIDs.Free;
  inherited Destroy;
end;

procedure TNMEAGPGSA.InitVariables;
begin
  FMode := '';
  FFixStatus := '';
  FPDOP := '';
  FVDOP := '';
  FHDOP := '';
  FSentence := '';
  FChecksum := '';
end;

procedure TNMEAGPGSA.Interprete(GSA_Sentence : string);
var
  PosFixColl : TStrings;
  szMode : string;
  szFixStatus : string;
  szPDOP : string;
  szVDOP : string;
  szHDOP : string;
  szChecksum : string;
  {Use this var for each of the positional IDs...}
  szPosID : string;
  szTempChecksum : string;
  szTempVDOP : string;
  bCount : integer;
begin
  InitVariables;
  PosFixColl := TStringList.Create;
  if (Parse(GSA_Sentence, 1, ',') = '$GPGSA') And
     (CountParts(GSA_Sentence, ',') = 18) then
  begin
    FSentence := GSA_Sentence;
    szMode := Parse(FSentence, 2, ',');        //mode
    FMode := szMode;

    szFixStatus := Parse(FSentence, 3, ',');   //fix status
    if IsNumeric(szFixStatus) then
      FFixStatus := szFixStatus;

    szPDOP := Parse(FSentence, 16, ',');       //PDOP
    if IsNumeric(szPDOP) then
      FPDOP := szPDOP;

    szHDOP := Parse(FSentence, 17, ',');       //HDOP
    if IsNumeric(szHDOP) then
      FHDOP := szHDOP;

    szTempVDOP := Parse(FSentence, 18, ',');   //VDOP
    szVDOP := Parse(szTempVDOP, 1, '*');
    if IsNumeric(szVDOP) then
      FVDOP := szVDOP;

    szTempChecksum := Parse(FSentence, 18, ','); //checksum
    szChecksum := Parse(szTempChecksum, 2, '*');
    FChecksum := szChecksum;

    //Loop through the NMEA string to find all of the
    //sats that are used in the position fix...
    for bCount := 4 to 15 do
    begin
      szPosID := Parse(FSentence, bCount, ',');
      if (length(szPosID) <> 0) then
        PosFixColl.Add (szPosID);
    end;
    FPosFixIDs.Assign(PosFixColl);

    FSuccess := true;
  end
  else
  begin
    FSuccess := false;
    //raise Exception.Create('GPGSA::Sentence. Invalid or corrupt GPGSA sentence.');
  end;
  PosFixColl.Free;
end;
end.
