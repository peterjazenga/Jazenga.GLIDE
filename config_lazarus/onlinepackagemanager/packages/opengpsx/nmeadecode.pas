{ <NMEADecode>

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

unit nmeadecode;

interface

uses
  LCLType, LResources, LCLIntf, Classes, Forms, SysUtils, gpsdatadef, gpsutilities,
  nmea_gpgga, nmea_gpgsa, nmea_gpgsv, nmea_gprmc, nmea_gpvtg, nmea_gpgll,
  nmea_gpbod, nmea_gpwpl, nmea_gprte, nmea_gpgarmin, strutils;

const
  NMEAMessageTypes : array[0..11] of string = ('$GPGGA', '$GPGSV', '$GPGSA', '$GPRMC',
                     '$GPVTG', '$GPGLL', '$GPBOD', '$GPWPL', '$PGRME', '$PGRMM',
                     '$PGRMZ', '$PSLIB');
type
  TNMEAMessageType = (UnknownMsg, GGAMsg, GSVMsg, RMCMsg, GSAMsg, VTGMsg, GLLMsg,
                      BODMsg, WPLMsg, RTEMsg, PGRMEMsg, PGRMMsg, PGRMZMsg,
                      PSLIBMsg);
  TNMEADecode = class;

  //Created event to send out decoded NMEA data for faster computing.
  TNMEADecodeEvent = procedure (Sender : TObject; NMEA : TNMEADecode) of object;
  TGPSTargetEvent = procedure (Sender : TObject; NMEA : TNMEADecode) of object;
  TGPSSkyplotEvent = procedure (Sender : TObject; GSVInfos : TGSVInfos) of object;
  TGPSSignalplotEvent = procedure (Sender : TObject; GSVInfos : TGSVInfos) of object;
  TNMEAGPGGAEvent = procedure (Sender : TObject; GGA : TNMEAGPGGA) of Object;
  TNMEAGPRMCEvent = procedure (Sender : TObject; RMC : TNMEAGPRMC) of object;
  TNMEAGPGSAEvent = procedure (Sender : TObject; GSA : TNMEAGPGSA) of object;
  TNMEAGPGSVEvent = procedure (Sender : TObject; GSV : TNMEAGPGSV) of object;
  TNMEAGPVTGEvent = procedure (Sender : TObject; VTG : TNMEAGPVTG) of object;
  TNMEAGPGLLEvent = procedure (Sender : TObject; GLL : TNMEAGPGLL) of object;
  TNMEAGPBODEvent = procedure (Sender : TObject; BOD : TNMEAGPBOD) of object;
  TNMEAGPWPLEvent = procedure (Sender : TObject; WPL : TNMEAGPWPL) of object;
  TNMEAGPRTEEvent = procedure (Sender : TObject; RTE : TNMEAGPRTE) of object;
  TGarminPGRMEEvent = procedure (Sender : TObject; PGRME : TGarminPGRME) of object;
  TGarminPGRMMEvent = procedure (Sender : TObject; PGRMM : TGarminPGRMM) of object;
  TGarminPGRMZEvent = procedure (Sender : TObject; PGRMZ : TGarminPGRMZ) of object;
  TGarminPSLIBEvent = procedure (Sender : TObject; PSLIB : TGarminPSLIB) of object;


  TNMEADecode = class(TComponent)
  private
    FSentence    : string;
    FSuccess     : boolean;
    //Maximum channels for GPS, designed for future;
    FMaxChannel  : integer;
    FMessageType : TNMEAMessageType;
    //Special List keeps GSV items for using in TGPSKyPlot and TGPSSignalPlot.
    FGSVInfos : TGSVInfos;

    FGGA : TNMEAGPGGA;
    FGSV : TNMEAGPGSV;
    FGSA : TNMEAGPGSA;
    FRMC : TNMEAGPRMC;
    FVTG : TNMEAGPVTG;
    FGLL : TNMEAGPGLL;
    FBOD : TNMEAGPBOD;
    FWPL : TNMEAGPWPL;
    FRTE : TNMEAGPRTE;
    FPGRME : TGarminPGRME;
    FPGRMM : TGarminPGRMM;
    FPGRMZ : TGarminPGRMZ;
    FPSLIB : TGarminPSLIB;

    FOnNMEA : TNMEADecodeEvent;
    FOnGPSTarget : TGPSTargetEvent;
    FOnGPSSkyplot : TGPSSkyplotEvent;
    FOnGPSSignalplot : TGPSSignalplotEvent;
    FOnGGA : TNMEAGPGGAEvent;
    FOnRMC : TNMEAGPRMCEvent;
    FOnGSA : TNMEAGPGSAEvent;
    FOnGSV : TNMEAGPGSVEvent;
    FOnVTG : TNMEAGPVTGEvent;
    FOnGLL : TNMEAGPGLLEvent;
    FOnBOD : TNMEAGPBODEvent;
    FOnWPL : TNMEAGPWPLEvent;
    FOnRTE : TNMEAGPRTEEvent;
    FOnPGRME : TGarminPGRMEEvent;
    FOnPGRMM : TGarminPGRMMEvent;
    FOnPGRMZ : TGarminPGRMZEvent;
    FOnPSLIB : TGarminPSLIBEvent;

    procedure DecodeGPGGA;
    procedure DecodeGPGSA;
    procedure DecodeGPRMC;
    procedure DecodeGPGSV;
    procedure DecodeGPVTG;
    procedure DecodeGPGLL;
    procedure DecodeGPBOD;
    procedure DecodeGPWPL;
    procedure DecodeGPRTE;
    procedure DecodeGarminPGRME;
    procedure DecodeGarminPGRMM;
    procedure DecodeGarminPGRMZ;
    procedure DecodeGarminPSLIB;

    procedure SetSentence(Sentence : string);
    procedure SetMaxChannel(const Value : integer);
    function CheckSentenceTypeAndCheckSum(const sentence : string) : boolean;
  public
    property Success : boolean read FSuccess write FSuccess;
    //Received NMEA sentence by this property.
    property Sentence : string write SetSentence;
    //Checked NMEA sentence is it one of GGA, GSA, GSV, RMC, GLL, VTG, BOD, WPL, RTE
    property MessageType : TNMEAMessageType read FMessageType write FMessageType;
    //Decoded data with normal properties.
    property GGA : TNMEAGPGGA read FGGA;
    property GSA : TNMEAGPGSA read FGSA;
    property GSV : TNMEAGPGSV read FGSV;
    property GSVInfos : TGSVInfos read FGSVInfos;
    property RMC : TNMEAGPRMC read FRMC;
    property VTG : TNMEAGPVTG read FVTG;
    property GLL : TNMEAGPGLL read FGLL;
    property BOD : TNMEAGPBOD read FBOD;
    property WPL : TNMEAGPWPL read FWPL;
    property RTE : TNMEAGPRTE read FRTE;
    property PGRME : TGarminPGRME read FPGRME;
    property PGRMM : TGarminPGRMM read FPGRMM;
    property PGRMZ : TGarminPGRMZ read FPGRMZ;
    property PSLIB : TGarminPSLIB read FPSLIB;

    //Specific for TGPSTarget.
    property OnGPSTarget : TGPSTargetEvent read FOnGPSTarget write FOnGPSTarget;
    //Specific for TGPSSkyPlot.
    property OnGPSSkyplot : TGPSSkyplotEvent read FOnGPSSkyplot write FOnGPSSkyplot;
    //Specific for TGPSSignalPlot.
    property OnGPSSignalplot : TGPSSignalplotEvent read FOnGPSSignalplot write FOnGPSSignalplot;

    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  published
    property MaxChannel : integer read FMaxChannel write SetMaxChannel default 12;
    //Full set of all decoded NMEA sentences by OnNMEA event.
    property OnNMEA : TNMEADecodeEvent read FOnNMEA write FOnNMEA;
    //Separated decoded NMEA sentence by the following events.
    property OnGGA : TNMEAGPGGAEvent read FOnGGA write FOnGGA;
    property OnRMC : TNMEAGPRMCEvent read FOnRMC write FOnRMC;
    property OnGSV : TNMEAGPGSVEvent read FOnGSV write FOnGSV;
    property OnGSA : TNMEAGPGSAEvent read FOnGSA write FOnGSA;
    property OnVTG : TNMEAGPVTGEvent read FOnVTG write FOnVTG;
    property OnGLL : TNMEAGPGLLEvent read FOnGLL write FOnGLL;
    property OnBOD : TNMEAGPBODEvent read FOnBOD write FOnBOD;
    property OnWPL : TNMEAGPWPLEvent read FOnWPL write FOnWPL;
    property OnRTE : TNMEAGPRTEEvent read FOnRTE write FOnRTE;
    property OnPGRME : TGarminPGRMEEvent read FOnPGRME write FOnPGRME;
    property OnPGRMM : TGarminPGRMMEvent read FOnPGRMM write FOnPGRMM;
    property OnPGRMZ : TGarminPGRMZEvent read FOnPGRMZ write FOnPGRMZ;
    property OnPSLIB : TGarminPSLIBEvent read FOnPSLIB write FOnPSLIB;
end;

procedure Register;
implementation

procedure Register;
begin
  RegisterComponents('OpenGPSX', [TNMEADecode]);
end;

constructor TNMEADecode.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FGSVInfos := TGSVInfos.Create(NIL);
  FGGA := TNMEAGPGGA.Create;
  FGSA := TNMEAGPGSA.Create;
  FGSV := TNMEAGPGSV.Create;
  FRMC := TNMEAGPRMC.Create;
  FVTG := TNMEAGPVTG.Create;
  FGLL := TNMEAGPGLL.Create;
  FBOD := TNMEAGPBOD.Create;
  FWPL := TNMEAGPWPL.Create;
  FRTE := TNMEAGPRTE.Create;
  FPGRMM := TGarminPGRMM.Create;
  FPGRME := TGarminPGRME.Create;
  FPGRMZ := TGarminPGRMZ.Create;
  FPSLIB := TGarminPSLIB.Create;
  FMessageType := UnKnownMsg;
  FSuccess := false;
  FMaxChannel := 12;
end;

destructor TNMEADecode.Destroy;
begin
  FGGA.Free;
  FGSA.Free;
  FGSV.Free;
  FRMC.Free;
  FGSVInfos.Free;
  FVTG.Free;
  FGLL.Free;
  FBOD.Free;
  FWPL.Free;
  FRTE.Free;
  FPGRMM.Free;
  FPGRME.Free;
  FPGRMZ.Free;
  FPSLIB.Free;
  inherited Destroy;
end;

procedure TNMEADecode.SetSentence(Sentence : string);
var
  szChk : string;
begin
  FSentence := Sentence;
  FSuccess := false;
  FGGA.Success := false;
  FGSA.Success := false;
  FGSV.Success := false;
  FRMC.Success := false;
  FVTG.Success := false;
  FGLL.Success := false;
  FBOD.Success := false;
  FWPL.Success := false;
  FRTE.Success := false;
  FPGRMM.Success := false;
  FPGRME.Success := false;
  FPGRMZ.Success := false;
  FPSLIB.Success := false;
  if (Parse(FSentence, 1, ',') = '$GPGGA') and CheckSentenceTypeAndCheckSum(FSentence) then
    DecodeGPGGA
  else if(Parse(FSentence, 1, ',') = '$GPGSV') and CheckSentenceTypeAndCheckSum(FSentence) then
    DecodeGPGSV
  else if(Parse(FSentence, 1, ',') = '$GPRMC') and CheckSentenceTypeAndCheckSum(FSentence) then
    DecodeGPRMC
  else if(Parse(FSentence, 1, ',') = '$GPGSA') and CheckSentenceTypeAndCheckSum(FSentence) then
    DecodeGPGSA
  else if(Parse(FSentence, 1, ',') = '$GPVTG') and CheckSentenceTypeAndCheckSum(FSentence) then
    DecodeGPVTG
  else if(Parse(FSentence, 1, ',') = '$GPGLL') and CheckSentenceTypeAndCheckSum(FSentence) then
    DecodeGPGLL
  else if(Parse(FSentence, 1, ',') = '$GPBOD') and CheckSentenceTypeAndCheckSum(FSentence) then
    DecodeGPBOD
  else if(Parse(FSentence, 1, ',') = '$GPWPL') and CheckSentenceTypeAndCheckSum(FSentence) then
    DecodeGPWPL
  else if(Parse(FSentence, 1, ',') = '$GPRTE') and CheckSentenceTypeAndCheckSum(FSentence) then
    DecodeGPRTE
  else if(Parse(FSentence, 1, ',') = '$PGRMM') and CheckSentenceTypeAndCheckSum(FSentence) then
    DecodeGarminPGRMM
  else if(Parse(FSentence, 1, ',') = '$PGRME') and CheckSentenceTypeAndCheckSum(FSentence) then
    DecodeGarminPGRME
  else if(Parse(FSentence, 1, ',') = '$PGRMZ') and CheckSentenceTypeAndCheckSum(FSentence) then
    DecodeGarminPGRMZ
  else if(Parse(FSentence, 1, ',') = '$PSLIB') and CheckSentenceTypeAndCheckSum(FSentence) then
    DecodeGarminPSLIB
  else
  begin
    FSuccess := false;
    FMessageType := UnknownMsg;
  end;
end;

function TNMEADecode.CheckSentenceTypeAndCheckSum(const sentence : string) : boolean;
var
  szHead1, szChecksum, szChkCompute, szProcess : string;
  i, len1, len2, chksum : integer;
  by : byte;
  ch : char;
begin
  result := false;
  szHead1 := Parse(sentence, 1, '*');
  len1 := length(szHead1);
  szChecksum := Parse(sentence, 2, '*');
  szProcess := RightStr(szHead1, len1 - 1);
  len2 := length(szProcess);
  chksum := 0;
  for i := 1 to len2  do
  begin
    ch := szProcess[i];
    by := ord(ch);
    chksum := chksum Xor by;
  end;
  szChkCompute := inttohex(chksum, 2);
  if (szChkCompute = szChecksum) then
    result := true;
end;

procedure TNMEADecode.SetMaxChannel(const Value : integer);
begin
  FMaxChannel := Value;
end;

procedure TNMEADecode.DecodeGPGGA;
begin
  Application.ProcessMessages;
  FGGA.Sentence := FSentence;
  if (FGGA.Success) then
  begin
    FSuccess := true;
    FMessageType := GGAMsg;
  end;
  if(FGGA.Success) and (Assigned(FOnGGA)) then
    FOnGGA(self, GGA);
  if(FGGA.Success) and (Assigned(FOnNMEA)) then
    FOnNMEA(self, self);
  if (FGGA.Success) and (Assigned(FOnGPSTarget)) then
    FOnGPSTarget(self, self);
end;

procedure TNMEADecode.DecodeGPGSV;
var
  i, cMessages, cSatsInMessage, cSatsInView, cMessageNumber : integer;
  Sat : TGSVInfo;
  f1, f2 ,f3, f4 : boolean;
  sztemp : string;
begin
  //Clean existing var before new gsvs.
  for i := FGSVInfos.Count - 1 downto 0 do
  begin
    Sat := TGSVInfo(FGSVInfos.Items[i]);
    FGSVInfos.Remove(Sat);
  end;

  if (CountParts(FSentence, ',') >= 8) then
  begin
    with FGSV do
    begin
      Sentence := FSentence;
      f1 := trystrtoint(MessageCount, cMessages);
      f2 := trystrtoint(SatsInView, cSatsInView);
      f3 := trystrtoint(SatsInMessage, cSatsInMessage);
      f4 := trystrtoint(MessageNumber, cMessageNumber);
      if (f2) and (cSatsInView > 0) then
      begin
      for i := 0 to cSatsInMessage - 1 do
      begin
        sztemp := SNR[i];
        if (SNR[i] = '') then
        begin
          exit;
        end;
        Sat := TGSVInfo.Create;
        Sat.PRN := PRN[i];
        Sat.Elevation := strtofloat(Elevation[i]);
        Sat.Azimuth := strtofloat(Azimuth[i]);
        Sat.SNR := strtoint(SNR[i]);
        Sat.MessageCount := cMessages;
        Sat.MessageNumber := cMessageNumber;
        Sat.SatsInMessage := cSatsInMessage;
        Sat.SatsInView := cSatsInView;
        if (i = 0) and (cMessageNumber = 1) then
          Sat.FirstSat := true
        else
          Sat.FirstSat := false;

        if (i = cSatsInMessage - 1) and (cMessageNumber = cMessages) then
          Sat.LastSat := true
        else
          Sat.LastSat := false;
        //Add satellite in this message.
        FGSVInfos.Add(Sat);
      end;
      if (Success) then
      begin
        FSuccess := true;
        FMessageType := GSVMsg;
      end;

      if(FGSV.Success) and (Assigned(FOnGSV)) then
        FOnGSV(self, GSV);
      //Fire event for TGPSSkyPlot.
      if(Success) and (Assigned(FOnGPSSkyplot)) then
        FOnGPSSkyplot(self, FGSVInfos);
      //Fire event for TGPSSignalPlot.
      if(Success) and (Assigned(FOnGPSSignalplot)) then
        FOnGPSSignalplot(self, FGSVInfos);
      if(Success) and (Assigned(FOnNMEA)) then
        FOnNMEA(self, self);
    end;
    end;
  end;
end;

procedure TNMEADecode.DecodeGPRMC;
begin
  Application.ProcessMessages;
  FRMC.Sentence := FSentence;
  if (FRMC.Success) then
  begin
    FSuccess := true;
    FMessageType := RMCMsg;
  end;

  if(FRMC.Success) and (Assigned(FOnRMC)) then
    FOnRMC(self, RMC);
  if(FRMC.Success) and (Assigned(FOnNMEA)) then
    FOnNMEA(self, self);
  if (FRMC.Success) and (Assigned(FOnGPSTarget)) then
    FOnGPSTarget(self, self);
end;

procedure TNMEADecode.DecodeGPGSA;
begin
  Application.ProcessMessages;
  FGSA.Sentence := FSentence;
  if (FGSA.Success) then
  begin
    FSuccess := true;
    FMessageType := GSAMsg;
  end;
  if(FGSA.Success) and (Assigned(FOnGSA)) then
    FOnGSA(self, GSA);
  if(FGSA.Success) and (Assigned(FOnNMEA)) then
    FOnNMEA(self, self);
end;

procedure TNMEADecode.DecodeGPVTG;
begin
  Application.ProcessMessages;
  FVTG.Sentence := FSentence;
  if (FVTG.Success) then
  begin
    FSuccess := true;
    FMessageType := VTGMsg;
  end;
  if(FVTG.Success) and (Assigned(FOnVTG)) then
    FOnVTG(self, VTG);

  if(FVTG.Success) and (Assigned(FOnNMEA)) then
    FOnNMEA(self, self);
end;

procedure TNMEADecode.DecodeGPGLL;
begin
  Application.ProcessMessages;
  FGLL.Sentence := FSentence;
  if (FGLL.Success) then
  begin
    FSuccess := true;
    FMessageType := GLLMsg;
  end;
  if(FGLL.Success) and (Assigned(FOnGLL)) then
    FOnGLL(self, GLL);
  if(FGLL.Success) and (Assigned(FOnNMEA)) then
    FOnNMEA(self, self);
end;

procedure TNMEADecode.DecodeGPBOD;
begin
  Application.ProcessMessages;
  FBOD.Sentence := FSentence;
  if (FBOD.Success) then
  begin
    FSuccess := true;
    FMessageType := BODMsg;
  end;
  if(FBOD.Success) and (Assigned(FOnBOD)) then
    FOnBOD(self, BOD);
  if(FBOD.Success) and (Assigned(FOnNMEA)) then
    FOnNMEA(self, self);
end;

procedure TNMEADecode.DecodeGPWPL;
begin
  Application.ProcessMessages;
  FWPL.Sentence := FSentence;
  if (FWPL.Success) then
  begin
    FSuccess := true;
    FMessageType := WPLMsg;
  end;
  if(FWPL.Success) and (Assigned(FOnWPL)) then
    FOnWPL(self, WPL);
  if(FWPL.Success) and (Assigned(FOnNMEA)) then
    FOnNMEA(self, self);
  //Need $GPWPL for TGPSTarget also.
  if (FWPL.Success) and (Assigned(FOnGPSTarget)) then
    FOnGPSTarget(self, self);
end;

procedure TNMEADecode.DecodeGPRTE;
begin
  Application.ProcessMessages;
  FRTE.Sentence := FSentence;
  if (FRTE.Success) then
  begin
    FSuccess := true;
    FMessageType := RTEMsg;
  end;
  if(FRTE.Success) and (Assigned(FOnRTE)) then
    FOnRTE(self, RTE);
  if(FRTE.Success) and (Assigned(FOnNMEA)) then
    FOnNMEA(self, self);
  //if there are $GPRTE sentences, fire event for TGPSTarget.
  if (FRTE.Success) and (Assigned(FOnGPSTarget)) then
    FOnGPSTarget(self, self);
end;

procedure TNMEADecode.DecodeGarminPGRME;
begin
  Application.ProcessMessages;
  FPGRME.Sentence := FSentence;
  if (FPGRME.Success) then
  begin
    FSuccess := true;
    FMessageType := PGRMEMsg;
  end;

  if(FPGRME.Success) and (Assigned(FOnPGRME)) then
    FOnPGRME(self, PGRME);
  if(FPGRME.Success) and (Assigned(FOnNMEA)) then
    FOnNMEA(self, self);
end;

procedure TNMEADecode.DecodeGarminPGRMM;
begin
  Application.ProcessMessages;
  FPGRMM.Sentence := FSentence;
  if (FPGRMM.Success) then
  begin
    FSuccess := true;
    FMessageType := PGRMMsg;
  end;

  if(FPGRMM.Success) and (Assigned(FOnPGRMM)) then
    FOnPGRMM(self, PGRMM);
  if(FPGRMM.Success) and (Assigned(FOnNMEA)) then
    FOnNMEA(self, self);
end;

procedure TNMEADecode.DecodeGarminPGRMZ;
begin
  Application.ProcessMessages;
  FPGRMZ.Sentence := FSentence;
  if (FPGRMZ.Success) then
  begin
    FSuccess := true;
    FMessageType := PGRMZMsg;
  end;

  if(FPGRMZ.Success) and (Assigned(FOnPGRMZ)) then
    FOnPGRMZ(self, PGRMZ);
  if(FPGRMZ.Success) and (Assigned(FOnNMEA)) then
    FOnNMEA(self, self);
end;

procedure TNMEADecode.DecodeGarminPSLIB;
begin
  Application.ProcessMessages;
  FPSLIB.Sentence := FSentence;
  if (FPSLIB.Success) then
  begin
    FSuccess := true;
    FMessageType := PSLIBMsg;
  end;

  if(FPSLIB.Success) and (Assigned(FOnPSLIB)) then
    FOnPSLIB(self, PSLIB);
  if(FPSLIB.Success) and (Assigned(FOnNMEA)) then
    FOnNMEA(self, self);
end;

Initialization
{$i nmeadecode.lrs}


end.
