{ <NMEA_GPGSV>

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


{*********************** GSV - Detailed Satellite data ***********************}
//This unit is implemented by NMEADecode unit.
unit nmea_gpgsv;

interface

uses
  Classes, SysUtils, gpsutilities;

type
  TNMEAGPGSV = class(TObject)
  private
    //local variables to hold property values
    FSentence : string;
    FChecksum : string;
    FSatsInView : string;
    FMessageCount : string;
    FMessageNumber : string;
    FSatsInMessage : string;
    FPRN : string;
    FElevation : string;
    FAzimuth : string;
    FSNR : string;
    FSuccess : boolean;
    procedure ParseSNR(item : integer);
    procedure ParseAzimuth(item : integer);
    procedure ParseElevation(item : integer);
    procedure ParsePRN(item : integer);

    function GetSNR(item : integer) : string;
    function GetAzimuth(item : integer) : string;
    function GetElevation(item : integer) : string;
    function GetPRN(item : integer) : string;
    function GetSatsInMessage : string;
    procedure Interprete(GSV_Sentence : string);
    procedure InitVariables;
  public
    property SNR[item : integer] : string read GetSNR;
    property Azimuth[item : integer] : string read GetAzimuth;
    property Elevation[item : integer] : string read GetElevation;
    property PRN[item : integer] : string read GetPRN;
    property MessageNumber : string read FMessageNumber;
    property MessageCount : string read FMessageCount;
    property SatsInMessage : string read GetSatsInMessage;
    property SatsInView : string read FSatsInView;
    property CheckSum : string read FCheckSum;
    property Sentence : string read FSentence write Interprete;
    property Success : boolean read FSuccess write FSuccess;
    constructor Create;
    destructor Destroy; override;
  end;
implementation

constructor TNMEAGPGSV.Create;
begin
  inherited create;
  FSuccess := false;
end;

destructor TNMEAGPGSV.Destroy;
begin
  inherited Destroy;
end;

procedure TNMEAGPGSV.InitVariables;
begin
  FChecksum := '';
  FSatsInView := '';
  FMessageCount := '';
  FMessageNumber := '';
  FSatsInMessage := '';
  FPRN := '';
  FElevation := '';
  FAzimuth := '';
  FSNR := '';
end;

function TNMEAGPGSV.GetSNR(item : Integer) : string;
begin
  case item of
    0..3 : ParseSNR(Item);
    //else
    //  Raise Exception.Create('GPGSV::SNR. SNR ''Item'' parameter must be 0..3');
  end;
    result := FSNR;
end;

function TNMEAGPGSV.GetAzimuth(item : Integer) : string;
begin
  case item of
    0..3 : ParseAzimuth(item);
    //else
    //  Raise Exception.Create('GPGSV::Azimuth. Azimuth ''Item'' parameter must be 0..3');
  end;
  result := FAzimuth;
end;

function TNMEAGPGSV.GetElevation(item : integer) : string;
begin
  case item of
    0..3 : ParseElevation(item);
    //else
    //  Raise Exception.Create('GPGSV::Elevation. Elevation ''Item'' parameter must be 0..3');
  end;
  result := FElevation;
end;

function TNMEAGPGSV.GetPRN(item : integer) : string;
begin
  case item of
    0..3 : ParsePRN(item);
    //else
    //  Raise Exception.Create('GPGSV::PRN. PRN ''Item'' parameter must be 0..3');
  end;
  result := FPRN;
end;
function TNMEAGPGSV.GetSatsInMessage : string;
begin
   if(CountParts(FSentence, ',') = 20) then
   begin
     FSatsInMessage := inttostr(4);
     result := inttostr(4);
   end
   else if (CountParts(FSentence, ',') = 16) then
   begin
     FSatsInMessage := inttostr(3);
     result := inttostr(3);
   end
   else if (CountParts(FSentence, ',') = 12) then
   begin
     FSatsInMessage := inttostr(2);
     result := inttostr(2);
   end
   else if (CountParts(FSentence, ',') = 8) then
   begin
     FSatsInMessage := inttostr(1);
     result := inttostr(1);
   end;
end;

procedure  TNMEAGPGSV.Interprete(GSV_Sentence : string);
var
  szChecksum : string;
  szSatsInView : string;
  szMessageCount : string;
  szMessageNumber : string;
  szTempChecksum : string;
  i, count : integer;
begin
  //$GPGSV,3,1,10,06,81,017,44,07,74,335,41,21,47,288,25,18,35,207,42*7E
  //$GPGSV,3,2,10,10,32,043,39,29,26,100,36,15,24,270,27,26,23,115,37*79
  //$GPGSV,3,3,10,16,13,319,32,05,12,176,29*7A

  InitVariables;
  FSentence := GSV_Sentence;
  if (CountParts(FSentence, ',') >= 8)  then
  begin
    count := 0;
      // parse NMEA sentence and check quality
      szMessageCount := Parse(FSentence, 2, ',');        //MessageCount
      if IsNumeric(szMessageCount) then
        FMessageCount := szMessageCount;

      szMessageNumber := Parse(FSentence, 3, ',');       //MessageNumber
      if IsNumeric(szMessageNumber) then
        FMessageNumber := szMessageNumber;

      szSatsInView := Parse(FSentence, 4, ',');          //SatsInView
      if IsNumeric(szSatsInView) then
        FSatsInView := szSatsInView;


      szTempChecksum := Parse(FSentence, 20, ',');       //Checksum
      szChecksum := Parse(szTempChecksum, 2, '*');
      FChecksum := szChecksum;
    //end;
    FSuccess := true;
  end
  else
  begin
    FSuccess := false;
    //Raise Exception.Create('GPGSV::Sentence. Invalid or corrupt GPGSV sentence.');
  end;
end;

procedure TNMEAGPGSV.ParseSNR(Item : integer);
var
  szTempSNR : string;
  SNR_Array : array [0..3] of string;  //string array to hold SNR values
  cSkip : byte;
  i : byte;
begin
  cSkip := 0;
  for i := 0 to 3 do
  begin
    if (cSkip < 12) then
      SNR_Array[i] := Parse(FSentence, 8 + cSkip, ',')
    else
    begin
      szTempSNR := Parse(FSentence, 20, ',');
      SNR_Array[i] := Parse(szTempSNR, 1, '*');
    end;
    cSkip := cSkip + 4;  //szkip to next SNR value in the sentence...
  end;

  if (IsNumeric(SNR_Array[Item])) then
    FSNR := SNR_Array[Item];
end;

procedure TNMEAGPGSV.ParseAzimuth(item : integer);
var
  AZ_Array : array [0..3] of string;    //string array to hold Azimuth values
  szSkip : byte;
  i : byte;
begin
  szSkip := 0;
  for i := 0 to 3 do
  begin
    AZ_Array[i] := Parse(FSentence, 7 + szSkip, ',');
    szSkip := szSkip + 4;     //skip to next Azimuth value in the sentence...
  end;
        
  if (IsNumeric(AZ_Array[item])) then
    FAzimuth := AZ_Array[item];

end;

procedure TNMEAGPGSV.ParseElevation(item : integer);
var
  EL_Array : array [0..3] of string;    //string array to hold Elevation values
  szSkip : byte;
  i : byte;
begin
  szSkip := 0;
  for i := 0 to 3 do
  begin
    EL_Array[i] := Parse(FSentence, 6 + szSkip, ',');
    szSkip := szSkip + 4;     //szkip to next Elevation value in the sentence...
  end;
        
    if (IsNumeric(EL_Array[item])) then
      FElevation := EL_Array[Item];
end;

procedure TNMEAGPGSV.ParsePRN(item : integer);
var
  PRN_Array : array[0..3] of string;    //string array to hold PRN values
  szSkip : byte;
  i : byte;
begin
  szSkip := 0;
  for i := 0 to 3 do
  begin
    PRN_Array[i] := Parse(FSentence, 5 + szSkip, ',');
    szSkip := szSkip + 4;     //skip to next PRN value in the sentence...
  end;

  FPRN := PRN_Array[item];
end;
end.
 
