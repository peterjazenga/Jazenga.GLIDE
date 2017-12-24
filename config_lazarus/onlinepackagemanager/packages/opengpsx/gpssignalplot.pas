{ <GPSSignalPlot>

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



//TGSPSignalPlot uses TProgressbar which there are many bugs in Windows.
//Orientation = spVertical displaying is not correct.
//Orientation = spHorizontal displaying is OK.
//Orientation = spTopDown displaying is not correct.
//Orientation = spRightToLeft displaying is not correct.
unit gpssignalplot;

{$mode objfpc}{$H+}

interface

uses
  Classes, LResources, SysUtils, StdCtrls, ComCtrls, Controls, gpsdatadef,
  nmeadecode;

type
TSPOrientation = (spHorizontal, spVertical);

TGPSSignalPlot = class (TGroupBox)
private
  FMaxChannel : integer;
  FSignalBars : array[0..23] of TProgressBar;
  FPrnLabels  : array[0..23] of TLabel;
  FXPoss      : array[0..23] of integer;
  FYPoss      : array[0..23] of integer;
  FMargin     : integer;
  FOrientation : TSPOrientation;
  FBw, FBh, FBs, FLw, FLh, FLs : integer;
  procedure DrawGPSSignalPlot(GSVInfos : TList);
  procedure SetMargin(const Value : integer);
  procedure SetMaxChannel(const Value : integer);
  procedure SetOrientation(const Value : TSPOrientation);
  procedure SetChildPositions;
  procedure SetNMEADecode(const NMEA : TNMEADecode);
  procedure DoRxGSVInfos(Sender : TObject; GSVInfos : TGSVInfos);
  procedure CreateChilds(const OldChannel, MaxChannel : integer);
protected
  procedure DoOnResize; override;
  procedure Loaded; override;
public
  constructor Create (AOwner : TComponent); override;
  destructor Destroy; override;
  property  NMEADecode : TNMEADecode write SetNMEADecode;
published
  property Margin : integer read FMargin write SetMargin default 10;
  //property GSVInfos : TList write DrawGPSSignalPlot;
  //property MaxChannel : integer read FMaxChannel write SetMaxChannel default 16;
  {$IFDEF WINDOWS}
  property Orientation : TSPOrientation read FOrientation write SetOrientation default spHorizontal;
  {$ELSE}
  property Orientation : TSPOrientation read FOrientation write SetOrientation default spVertical;
  {$ENDIF}
  property Width default 280;
  property Height default 300;
end;

procedure Register;
implementation

procedure Register;
begin
  RegisterComponents('OpenGPSX', [TGPSSignalPlot]);
end;

procedure TGPSSignalPlot.SetOrientation(const Value : TSPOrientation);
begin
  FOrientation := Value;
  SetChildPositions;
end;

procedure TGPSSignalPlot.SetMargin(const Value : integer);
begin
  FMargin := Value;
  SetChildPositions;
end;

procedure TGPSSignalPlot.SetMaxChannel (const Value : integer);
var
  i, j : integer;
begin
  if (Value < FMaxChannel) then
  begin
    j := FMaxChannel - Value;
    for i := FMaxChannel - 1 downto Value do
    begin
      if Assigned(FSignalBars[i]) then FreeAndNIL(FSignalBars[i]);
      if Assigned(FPrnLabels[i]) then FreeAndNIL(FPrnLabels[i]);
    end;
  end;
  FMaxChannel := Value;
  SetChildPositions;
end;

procedure TGPSSignalPlot.SetNMEADecode(const NMEA : TNMEADecode);
var
  i, c : integer;
begin
  if (Assigned(NMEA)) then
  begin
    NMEA.OnGPSSignalPlot := @DoRxGSVInfos;
    c := FMaxChannel;
    FMaxChannel := NMEA.MaxChannel;
    CreateChilds(c, FMaxChannel);
    SetChildPositions;
    for i := 0 to FMaxChannel - 1 do
    begin
      FSignalBars[i].Step := 1;
      FSignalBars[i].Smooth := true;
      FSignalBars[i].Max := 99;
      FSignalBars[i].Min := 0;
      {$IFDEF WINDOWS}
      if (FOrientation = spVertical) then
        FSignalBars[i].Position := 100
      else if (FOrientation = spHorizontal) then
        FSignalBars[i].Position := 0;
      {$ELSE}
      FSignalBars[i].Position := 0;
      {$ENDIF}
      FSignalBars[i].Show;
      FPrnLabels[i].Caption := '';//inttostr(i);
      FPrnLabels[i].Show;
    end;
  end;
end;

procedure TGPSSignalPlot.DoRxGSVInfos(Sender : TObject; GSVInfos : TGSVInfos);
begin
  if (Assigned(GSVInfos)) then
  begin
    DrawGPSSignalPlot(GSVInfos);
  end;
end;

procedure TGPSSignalPlot.DrawGPSSignalPlot(GSVInfos : TList);
var
  i : integer;
  gsv : TGSVInfo;
  pos : integer;
begin
  if Assigned(GSVInfos) then
  begin
    for i := 0 to GSVInfos.Count - 1 do
    begin

      gsv := TGSVInfo(GSVInfos.Items[i]);
      {$IFDEF WINDOWS}
      if (FOrientation = spVertical) then
        pos := 99 - gsv.SNR
      else
        pos := trunc(gsv.SNR);
      {$ELSE}
      pos := gsv.SNR;
      {$ENDIF}
      if (gsv.MessageNumber = 1) then
      begin
        FSignalBars[i].Position := pos;
        FPrnLabels[i].Caption := gsv.PRN;
      end
      else if (gsv.MessageNumber = 2) then
      begin
        FSignalBars[i + 4].Position := pos;
        FPrnLabels[i + 4].Caption := gsv.PRN;
      end
      else if (gsv.MessageNumber = 3) then //Max Channels = 12
      begin
        FSignalBars[i + 8].Position := pos;
        FPrnLabels[i + 8].Caption := gsv.PRN;
      end
      else if (gsv.MessageNumber = 4) then //Max Channels = 16
      begin
        FSignalBars[i + 12].Position := pos;
        FPrnLabels[i + 12].Caption := gsv.PRN;
      end
      else if (gsv.MessageNumber = 5) then //Max channels = 20
      begin
        FSignalBars[i + 16].Position := pos;
        FPrnLabels[i + 16].Caption := gsv.PRN;
      end
      else if (gsv.MessageNumber = 6) then //Max channels = 24
      begin
        FSignalBars[i + 20].Position := pos;
        FPrnLabels[i + 20].Caption := gsv.PRN;
      end
    end;
  end;
end;

constructor TGPSSignalPlot.Create (AOwner : TComponent);
var
  i : integer;
begin
  inherited Create(AOwner);
  Parent := TWinControl(AOwner);
  FMargin := 10;
  FOrientation := spVertical;
  {$IFDEF LINUX}
  Font.Name := 'Sans';
  Font.Size := 8;
  {$ENDIF}
  Left := 0;
  Top := 0;
  Width := 280;
  Height := 300;
  FMaxChannel := 16;
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);

  FLs := 8;
  FBs := 8;
  FLw := 15;
  FLh := 20;
  CreateChilds (FMaxChannel, FMaxChannel);
end;

procedure TGPSSignalPlot.CreateChilds(const OldChannel, MaxChannel : integer);
var
  i : integer;
begin
  for i := FMaxChannel - 1 downto 0 do
  begin
    if (Assigned (FSignalBars[i])) then
      FreeAndNil(FSignalBars[i]);
    if (Assigned (FPrnLabels[i])) then
      FreeAndNil(FPrnLabels[i]);
  end;

  for i := 0 to MaxChannel - 1 do
  begin
    FSignalBars[i] := TProgressBar.Create(self);
    FSignalBars[i].Parent := self;
    FPrnLabels[i] := TLabel.Create(self);
    FPrnLabels[i].Parent := self;
    FPrnLabels[i].Font := Font;
  end;
end;

destructor TGPSSignalPlot.Destroy;
begin
  inherited Destroy;
end;

procedure TGPSSignalPlot.Loaded;
var
  i : integer;
begin
  SetChildPositions;
  for i := 0 to FMaxChannel - 1 do
  begin
    FSignalBars[i].Step := 1;
    FSignalBars[i].Max := 99;
    FSignalBars[i].Min := 0;
    {$IFDEF WINDOWS}
    if (FOrientation = spVertical) then
      FSignalBars[i].Position := 100
    else if (FOrientation = spHorizontal) then
      FSignalBars[i].Position := 0;
    {$ELSE}
    FSignalBars[i].Position := 0;
    {$ENDIF}
    FSignalBars[i].Show;
    FPrnLabels[i].Caption := '';//inttostr(i);
    FPrnLabels[i].Show;
  end;
  inherited Loaded;
end;


procedure TGPSSignalPlot.DoOnResize;
begin
  inherited DoOnResize;
  if csDesigning in ComponentState then
  begin
    SetChildPositions;
  end;
end;

procedure TGPSSignalPlot.SetChildPositions;
var
  i : integer;
begin
  if (FOrientation = spVertical) then
  begin
    FBh := (Height - 2 * FMargin - FLh - FLs);
    FBw := (Width - 2 * FMargin - (FMaxChannel- 1) * FBs) div FMaxChannel ;
  end
  else if (FOrientation = spHorizontal) then
  begin
    FBh := (Height - 2 * FMargin - (FMaxChannel - 1) * FBs - FLs) div FMaxChannel;
    FBw := Width - 2 * FMargin - FBs - FLw;
  end;

  for i := 0 to FMaxChannel -1 do
  begin
    if (FOrientation = spVertical) then
      FXPoss[i] := FMargin + FBs * i + FBw * i
    else if (FOrientation = spHorizontal) then
      FYPoss[i] := FMargin + FBs * i + FBh * i;
  end;

  if (FOrientation = spVertical) then
  begin
    FYPoss[0] := FMargin;
    FYPoss[1] := FMargin + FBh + FBs;
  end
  else if (FOrientation = spHorizontal) then
  begin
    FXPoss[0] := FMargin;
    FXPoss[1] := FMargin + FLw + FLs;
  end;

  for i := 0 to FMaxChannel - 1 do
  begin
    if (FOrientation = spVertical) then
    begin
      FPrnLabels[i].Caption := inttostr(i);
      FPrnLabels[i].Left := FXPoss[i];
      FPrnLabels[i].Top  := FYPoss[1];
    end
    else if (FOrientation = spHorizontal) then
    begin
      FPrnLabels[i].Caption := inttostr(i);
      FPrnLabels[i].Left := FXPoss[0];
      FPrnLabels[i].Top  := FYPoss[i];
    end;
    FPrnLabels[i].Width:= FLw;
    FPrnLabels[i].Height:= FLh;
    FSignalBars[i].Width:= FBw;
    FSignalBars[i].Height:= FBh;
    if (FOrientation = spVertical) then
    begin
      FSignalBars[i].Orientation := pbVertical;
      FSignalBars[i].Left:= FXPoss[i];
      FSignalBars[i].Top:= FYPoss[0];
    end
    else if (FOrientation = spHorizontal) then
    begin
      FSignalBars[i].Orientation := pbHorizontal;
      FSignalBars[i].Left:= FXPoss[1];
      FSignalBars[i].Top:= FYPoss[i];
    end;
    {$IFDEF WINDOWS}
    if (FOrientation = spVertical) then
      FSignalBars[i].Position := 100
    else if (FOrientation = spHorizontal) then
      FSignalBars[i].Position := 0;
    {$ELSE}
    FSignalBars[i].Position := 0;
    {$ENDIF}
  end;

end;

Initialization
{$i gpssignalplot.lrs}

end.

