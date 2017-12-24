{ <NMEA_GPGarmin>

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




{**************** Garmin -  proprietary sentences ***********}
{     $PGRME - Estimated Position Error                      }
{     $PGRMM - Map Datum                                     }
{     $PGRMZ - Altitude Information                          }
{     $PSLIB - Differential Control                          }
unit nmea_gpgarmin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gpsutilities;
type
  TGarminPGRME = class(TObject)
  private
    FHozPosErr  : string;
    FUnitHPE    : string;
    FVertPosErr : string;
    FUnitVPE    : string;
    FSEPE : string;
    FUnitSEPE : string;
    FChecksum : string;
    FSuccess : boolean;
    FSentence : string;
    procedure Interprete(PGRME_Sentence : string);
  public
    property HPE : string read FHozPosErr;
    property VPE : string read FVertPosErr;
    property SEPE : string read FSEPE;
    property UnitHPE : string read FUnitHPE;
    property UnitVPE : string read FUnitVPE;
    property UnitSEPE : string read FUnitSEPE;
    property Checksum : string read FChecksum;
    property Success : boolean read FSuccess write FSuccess;
    property Sentence : string read FSentence write Interprete;
    constructor Create;
    destructor Destroy; override;
  end;

  TGarminPGRMM = class(TObject)
  private
    FMapDatum   : string;
    FChecksum : string;
    FSuccess : boolean;
    FSentence : string;
    procedure Interprete(PGRMM_Sentence : string);
  public
    property MapDatum : string read FMapDatum;
    property Checksum : string read FChecksum;
    property Success : boolean read FSuccess write FSuccess;
    property Sentence : string read FSentence write Interprete;
    constructor Create;
    destructor Destroy; override;
  end;

  TGarminPGRMZ = class(TObject)
  private
    FAltitude     : string;
    FUnitAltitude : string;
    FPosFix       : string;
    FChecksum : string;
    FSuccess : boolean;
    FSentence : string;
    procedure Interprete(PGRMZ_Sentence : string);
  public
    property Altitude : string read FAltitude;
    property UnitAltitude : string read FUnitAltitude;
    property PositionFix  : string read FPosFix;
    property Checksum : string read FChecksum;
    property Success : boolean read FSuccess write FSuccess;
    property Sentence : string read FSentence write Interprete;
    constructor Create;
    destructor Destroy; override;
  end;

  TGarminPSLIB = class(TObject)
  private
    FFrequency    : string;
    FBitRate      : string;
    FRequestType  : string;
    FChecksum : string;
    FSuccess : boolean;
    FSentence : string;
    procedure Interprete(PSLIB_Sentence : string);
  public
    property Frequency   : string read FFrequency;
    property BitRate     : string read FBitRate;
    property RequestType : string read FRequestType;
    property Checksum : string read FChecksum;
    property Success : boolean read FSuccess write FSuccess;
    property Sentence : string read FSentence write Interprete;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{TGarminPGRME}
constructor TGarminPGRME.Create;
begin
  inherited Create;
  FSuccess := false;
end;

destructor TGarminPGRME.Destroy;
begin
  inherited Destroy;
end;

procedure TGarminPGRME.Interprete(PGRME_Sentence : string);
var
  szHozPosErr  : string;
  szUnitHPE    : string;
  szVertPosErr : string;
  szUnitVPE    : string;
  szSEPE : string;
  szUnitSEPE : string;
  szChecksum : string;
  sztemp : string;
  cParts : integer;
begin
  if (Parse(PGRME_Sentence, 1, ',') = '$PGRME') then
  begin
    FSentence := PGRME_Sentence;
    cParts := CountParts(FSentence, ',');
    if (cParts = 7) then
    begin
      szHozPosErr := Parse(FSentence, 2, ','); //Horizontal Position Error.
      if (IsNumeric(szHozPosErr)) then
        FHozPosErr := szHozPosErr;

      szUnitHPE := Parse(FSentence, 3, ','); //HPE Unit.
      if (szUnitHPE = 'M') then
        FUnitHPE := szUnitHPE;

      szVertPosErr := Parse(FSentence, 4, ','); //Vertical Position Error.
      if IsNumeric(szVertPosErr) then
        FVertPosErr := szVertPosErr;

      szUnitVPE := Parse(FSentence, 5, ','); //VPE Unit.
      if szUnitVPE = 'M' then
        FUnitVPE := szUnitVPE;

      szSEPE := Parse(FSentence, 6, ','); //Overall spherical equivalent position error.
      if (IsNumeric(szSEPE)) then
        FSEPE := szSEPE;

      sztemp := Parse(FSentence, 7, ',');; //SEPE Unit.
      szUnitSEPE := Parse(szTemp, 1, '*');
      if (szUnitSEPE = 'M') then
        FUnitSEPE := szUnitSEPE;

      szChecksum := Parse(szTemp, 2, '*');  //Checksum
      FChecksum := szChecksum;
      FSuccess := true;
    end;
  end
  else
  begin
      FSuccess := false;
  end;
end;


{TGarminRGRMM}
constructor TGarminPGRMM.Create;
begin
  inherited Create;
  FSuccess := false;
end;

destructor TGarminPGRMM.Destroy;
begin
  inherited Destroy;
end;

procedure TGarminPGRMM.Interprete(PGRMM_Sentence : string);
var
  szMapDatum  : string;
  szChecksum : string;
  sztemp : string;
  cParts : integer;
begin
  if (Parse(PGRMM_Sentence, 1, ',') = '$PGRMM') then
  begin
    FSentence := PGRMM_Sentence;
    cParts := CountParts(FSentence, ',');
    if (cParts = 2) then
    begin
      szTemp := Parse(FSentence, 2, ','); //Map Datum
      szMapDatum := Parse(szTemp, 1, '*');
      FMapDatum := szMapDatum;

      szChecksum := Parse(szTemp, 2, '*');  //Checksum
      FChecksum := szChecksum;
      FSuccess := true;
    end;
  end
  else
  begin
      FSuccess := false;
  end;
end;


{TGarminRGRMZ}
constructor TGarminPGRMZ.Create;
begin
  inherited Create;
  FSuccess := false;
end;

destructor TGarminPGRMZ.Destroy;
begin
  inherited Destroy;
end;

procedure TGarminPGRMZ.Interprete(PGRMZ_Sentence : string);
var
  szAltitude     : string;
  szUnitAltitude : string;
  szPosFix       : string;
  szChecksum : string;
  sztemp : string;
  cParts : integer;
begin
  if (Parse(PGRMZ_Sentence, 1, ',') = '$PGRMZ') then
  begin
    FSentence := PGRMZ_Sentence;
    cParts := CountParts(FSentence, ',');
    if (cParts = 4) then
    begin
      szAltitude := Parse(FSentence, 2, ','); //Alititude
      if (IsNumeric(szAltitude)) then
        FAltitude := szAltitude;

      szUnitAltitude := Parse(FSentence, 3, ','); //Alititude Unit.
      //if (szUnitAltitude = 'f') then
        FUnitAltitude := szUnitAltitude;

      szTemp := Parse(FSentence, 4, ',');  //Position Fix Dimension
      szPosFix := Parse(szTemp, 1, '*');
      if (szPosFix = '2') or (szPosFix = '3') then
        FPosFix := szPosFix;

      szChecksum := Parse(szTemp, 2, '*');  //Checksum
      FChecksum := szChecksum;
      FSuccess := true;
    end;
  end
  else
  begin
      FSuccess := false;
  end;
end;

{TGarminPSLIB}
constructor TGarminPSLIB.Create;
begin
  inherited Create;
  FSuccess := false;
end;

destructor TGarminPSLIB.Destroy;
begin
  inherited Destroy;
end;

procedure TGarminPSLIB.Interprete(PSLIB_Sentence : string);
var
  szFrequency   : string;
  szBitrate     : string;
  szRequestType : string;
  szChecksum : string;
  sztemp : string;
  cParts : integer;
begin
  if (Parse(PSLIB_Sentence, 1, ',') = '$PSLIB') then
  begin
    FSentence := PSLIB_Sentence;
    cParts := CountParts(FSentence, ',');
    if (cParts = 3)then
    begin
      szFrequency := Parse(FSentence, 2, ','); //Frequency
      if (IsNumeric(szFrequency)) then
        FFrequency := szFrequency;

      szTemp := Parse(FSentence, 3, ','); //Bitrate
      szBitrate := Parse(szTemp, 1, '*');
      if (IsNumeric(szBitrate)) then
        FBitRate := szBitRate;

      FRequestType := '';

      szChecksum := Parse(szTemp, 2, '*');  //Checksum
      FChecksum := szChecksum;
      FSuccess := true;
    end
    else if (cParts = 4) then
    begin
      szFrequency := Parse(FSentence, 2, ','); //Frequency
      if (szFrequency = '') then
        FFrequency := '-9999';

      szBitrate := Parse(FSentence, 3, ','); //Bitrate
      if (szBitrate = '') then
        FBitrate := '-9999';

      szTemp := Parse(FSentence, 4, ','); //Bitrate
      szRequestType := Parse(szTemp, 1, '*');
      if (szRequestType = 'J') or (szRequestType = 'K') then
        FRequestType := szRequestType;

      szChecksum := Parse(szTemp, 2, '*');  //Checksum
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

