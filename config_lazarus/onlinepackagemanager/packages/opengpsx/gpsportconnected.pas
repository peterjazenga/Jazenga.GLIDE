{ <GPSPortConnected>

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

unit gpsportconnected;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, StdCtrls, Dialogs, Controls, Graphics,
  sdposerial;

type
  TSynserStringDataEvent = procedure (Sender : TObject; OutString : string) of Object;

  {Derived TGPSPortConnected from TCommonDialog.}
  {For Set up Comport, Baudrate, Databits, Parity and Stopbits.}
  TGPSPortConnected = class(TCommonDialog)
  private
    FSdpoSerial  : TSdpoSerial;
    FFormLeft    : integer;
    FFormTop     : integer;
    FOnRxData    : TSynserStringDataEvent;
    FPortActive  : boolean;
    function  GetLeft: Integer;
    function  GetPosition: TPoint;
    function  GetTop: Integer;
    procedure SetLeft(const AValue: Integer);
    procedure SetPosition(const AValue: TPoint);
    procedure SetTop(const AValue: Integer);
    procedure SetSdpoSerial(const value : TSdpoSerial);
    procedure SetPortActive(const AValue : boolean);
  protected
    FComSetupForm : TForm;
    procedure  DoOKButtonClick(Sender : TObject);
    procedure  DoCancelButtonClick(Sender : TObject);
    procedure  UpdatePosition;
    procedure  DoCloseForm(Sender: TObject; var CloseAction: TCloseAction); virtual;
    function   CreateForm:TForm; virtual;
    procedure  SetFormValues; virtual;
    procedure  GetFormValues; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CloseDialog;
    procedure OpenPort;
    procedure ClosePort;
    function  SetupPort : Boolean;//override;
    property  Left : Integer read GetLeft write SetLeft;
    property  Position : TPoint read GetPosition write SetPosition;
    property  Top : Integer read GetTop write SetTop;
    property  Active : boolean read FPortActive write SetPortActive;
  published
    property Serial : TSdpoSerial read FSdpoSerial write SetSdpoSerial;
end;
  {TComPortSetupDialogForm read comportsetup.lrs (Lazarus Resource)}
  {at 'Initialization' section.}
  TComPortSetupDialogForm = class(TForm)
    FPortGroupBox  : TGroupBox;
    FCMBComport    : TComboBox;
    FComportLabel  : TLabel;
    FBaudRateLabel : TLabel;
    FCMBBaudRate    : TComboBox;
    FCMBDatabits   : TComboBox;
    FDatabitsLabel : TLabel;
    FParityLabel   : TLabel;
    FCMBParity     : TComboBox;
    FStopbitsLabel : TLabel;
    FCMBStopbits   : TComboBox;
    FOKButton      : TButton;
    FCancelButton  : TButton;
  private
  public
  end;


procedure Register;
implementation

procedure Register;
begin
  RegisterComponents('OpenGPSX', [TGPSPortConnected]);
end;

procedure TGPSPortConnected.UpdatePosition;
begin
  if Assigned(FComSetupForm) then
  begin
    FComSetupForm.Top := FFormTop;
    FComSetupForm.Left := FFormLeft;
  end;
end;

procedure TGPSPortConnected.DoCloseForm(Sender: TObject; var CloseAction: TCloseAction);
begin
  if csDesigning in ComponentState then exit;
  if Assigned(OnClose) then OnClose(Self);
end;

function TGPSPortConnected.GetLeft: Integer;
begin
  Result := FFormLeft;
end;

function TGPSPortConnected.GetPosition: TPoint;
begin
  Result := Point(FFormLeft, FFormTop);
end;

function TGPSPortConnected.GetTop: Integer;
begin
  Result := FFormTop;
end;

procedure TGPSPortConnected.SetLeft(const AValue: Integer);
begin
  if FFormLeft <> AValue then
  begin
    FFormLeft := AValue;
    UpdatePosition;
  end;
end;

procedure TGPSPortConnected.SetPortActive(const AValue : boolean);
begin
  FPortActive := AValue;
  FSdpoSerial.Active := AValue;
end;

procedure TGPSPortConnected.SetPosition(const AValue: TPoint);
begin
  if (FFormLeft<>AValue.x) or (FFormTop<>AValue.y) then
  begin;
    FFormLeft := AValue.x;
    FFormTop := AValue.y;
    UpdatePosition;
  end;
end;

procedure TGPSPortConnected.SetTop(const AValue: Integer);
begin
  if FFormTop <> AValue then
  begin
    FFormTop := AValue;
    UpdatePosition;
  end;
end;

procedure TGPSPortConnected.OpenPort;
begin
  FSdpoSerial.Open;
end;

procedure TGPSPortConnected.ClosePort;
begin
  FSdpoSerial.Close;
end;

function TGPSPortConnected.CreateForm: TForm;
begin
  Result := TComPortSetupDialogForm.Create(nil);// do not use Self as Owner, otherwise as desgntime this will not work
  with TComPortSetupDialogForm(Result) do
  begin
    {$IFDEF LINUX}
    FPortGroupBox.Width := FPortGroupBox.Width + 5;
    FCMBComport.Width := FCMBComport.Width + 13;
    FCMBBaudRate.Width := FCMBBaudRate.Width + 13;
    FCMBDatabits.Width := FCMBDatabits.Width + 13;
    FCMBParity.Width := FCMBParity.Width + 13;
    FCMBStopbits.Width := FCMBStopbits.Width + 13;
    {$ENDIF}
    FCMBComport.Items.Clear;
    with FCMBComPort do
    begin
      {$IFDEF WINDOWS}
      Items.Add('COM1');
      Items.Add('COM2');
      Items.Add('COM3');
      Items.Add('COM4');
      Items.Add('COM5');
      Items.Add('COM6');
      Items.Add('COM7');
      Items.Add('COM8');
      Items.Add('COM9');
      Items.Add('COM10');
      Items.Add('COM11');
      Items.Add('COM12');
      Items.Add('COM13');
      Items.Add('COM14');
      Items.Add('COM15');
      Items.Add('COM16');
      {$ELSE}
      Items.Add('/dev/ttyS0');
      Items.Add('/dev/ttyS1');
      Items.Add('/dev/ttyS2');
      Items.Add('/dev/ttyS3');
      Items.Add('/dev/ttyS4');
      Items.Add('/dev/ttyS5');
      Items.Add('/dev/ttyS6');
      Items.Add('/dev/ttyS7');
      Items.Add('/dev/ttyS8');
      Items.Add('/dev/ttyS9');
      Items.Add('/dev/ttyS10');
      Items.Add('/dev/ttyS11');
      Items.Add('/dev/ttyS12');
      Items.Add('/dev/ttyS13');
      Items.Add('/dev/ttyS14');
      Items.Add('/dev/ttyS15');
      {$ENDIF}
      ItemIndex := 0;
    end;

    FOKButton.OnClick     := @DoOKButtonClick;
    FCancelButton.OnClick := @DoCancelButtonClick;
  end;
end;

procedure TGPSPortConnected.SetFormValues;
var
  Dlg: TComPortSetupDialogForm;
begin
  Dlg := TComPortSetupDialogForm(FComSetupForm);
  {case FSdpoSerial.Device of}
    {$IFDEF WINDOWS}
  if (FSdpoSerial.Device = 'COM1') then
    Dlg.FCMBComport.ItemIndex := 0
  else if (FSdpoSerial.Device = 'COM2') then
    Dlg.FCMBComport.ItemIndex := 1
  else if (FSdpoSerial.Device = 'COM3') then
    Dlg.FCMBComport.ItemIndex := 2
  else if (FSdpoSerial.Device = 'COM4') then
    Dlg.FCMBComport.ItemIndex := 3
  else if (FSdpoSerial.Device = 'COM5') then
    Dlg.FCMBComport.ItemIndex := 4
  else if (FSdpoSerial.Device = 'COM6') then
    Dlg.FCMBComport.ItemIndex := 5
  else if (FSdpoSerial.Device = 'COM7') then
    Dlg.FCMBComport.ItemIndex := 6
  else if (FSdpoSerial.Device = 'COM8') then
    Dlg.FCMBComport.ItemIndex := 7
  else if (FSdpoSerial.Device = 'COM9') then
    Dlg.FCMBComport.ItemIndex := 8
  else if (FSdpoSerial.Device = 'COM10') then
    Dlg.FCMBComport.ItemIndex := 9
  else if (FSdpoSerial.Device = 'COM11') then
    Dlg.FCMBComport.ItemIndex := 10
  else if (FSdpoSerial.Device = 'COM12') then
    Dlg.FCMBComport.ItemIndex := 11
  else if (FSdpoSerial.Device = 'COM13') then
    Dlg.FCMBComport.ItemIndex := 12
  else if (FSdpoSerial.Device = 'COM14') then
    Dlg.FCMBComport.ItemIndex := 13
  else if (FSdpoSerial.Device = 'COM15') then
    Dlg.FCMBComport.ItemIndex := 14
  else if (FSdpoSerial.Device = 'COM16') then
    Dlg.FCMBComport.ItemIndex := 15;
    {$ELSE}
  if (FSdpoSerial.Device = '/dev/ttyS0') then
    Dlg.FCMBComport.ItemIndex := 0
  else if (FSdpoSerial.Device = '/dev/ttyS1') then
    Dlg.FCMBComport.ItemIndex := 1
  else if (FSdpoSerial.Device = '/dev/ttyS2') then
    Dlg.FCMBComport.ItemIndex := 2
  else if (FSdpoSerial.Device = '/dev/ttyS3') then
    Dlg.FCMBComport.ItemIndex := 3
  else if (FSdpoSerial.Device = '/dev/ttyS4') then
    Dlg.FCMBComport.ItemIndex := 4
  else if (FSdpoSerial.Device = '/dev/ttyS5') then
    Dlg.FCMBComport.ItemIndex := 5
  else if (FSdpoSerial.Device = '/dev/ttyS6') then
    Dlg.FCMBComport.ItemIndex := 6
  else if (FSdpoSerial.Device = '/dev/ttyS7') then
    Dlg.FCMBComport.ItemIndex := 7
  else if (FSdpoSerial.Device = '/dev/ttyS8') then
    Dlg.FCMBComport.ItemIndex := 8
  else if (FSdpoSerial.Device = '/dev/ttyS9') then
    Dlg.FCMBComport.ItemIndex := 9
  else if (FSdpoSerial.Device = '/dev/ttyS10') then
    Dlg.FCMBComport.ItemIndex := 10
  else if (FSdpoSerial.Device = '/dev/ttyS11') then
    Dlg.FCMBComport.ItemIndex := 11
  else if (FSdpoSerial.Device = '/dev/ttyS12') then
    Dlg.FCMBComport.ItemIndex := 12
  else if (FSdpoSerial.Device = '/dev/ttyS13') then
    Dlg.FCMBComport.ItemIndex := 13
  else if (FSdpoSerial.Device = '/dev/ttyS14') then
    Dlg.FCMBComport.ItemIndex := 14
  else if (FSdpoSerial.Device = '/dev/ttyS15') then
    Dlg.FCMBComport.ItemIndex := 15;
    {$ENDIF}

  case FSdpoSerial.BaudRate of
    br__1200 : Dlg.FCMBBaudRate.ItemIndex := 0;
    br__2400 : Dlg.FCMBBaudRate.ItemIndex := 1;
    br__4800 : Dlg.FCMBBaudRate.ItemIndex := 2;
    br__9600 : Dlg.FCMBBaudRate.ItemIndex := 3;
    br_19200 : Dlg.FCMBBaudRate.ItemIndex := 4;
    br_38400 : Dlg.FCMBBaudRate.ItemIndex := 5;
    br_57600 : Dlg.FCMBBaudRate.ItemIndex := 6;
    br115200 : Dlg.FCMBBaudRate.ItemIndex := 7;
  end;
  case FSdpoSerial.DataBits of
    db7bits : Dlg.FCMBDatabits.ItemIndex := 0;
    db8bits : Dlg.FCMBDatabits.ItemIndex := 1;
  end;
  case FSdpoSerial.Parity of
    pNone : Dlg.FCMBParity.ItemIndex := 0;
    pEven : Dlg.FCMBParity.ItemIndex := 1;
    pOdd  : Dlg.FCMBParity.ItemIndex := 2;
  end;
  case FSdpoSerial.StopBits of
    sbOne : Dlg.FCMBStopbits.ItemIndex := 0;
    sbTwo : Dlg.FCMBStopbits.ItemIndex := 1;
  end;
end;

procedure TGPSPortConnected.GetFormValues;
var
  Dlg : TComPortSetupDialogForm;
begin
  Dlg := TComPortSetupDialogForm(FComSetupForm);
  {case Dlg.FCMBComport.ItemIndex of
    0 : FSdpoSerial.Device := Dlg.FCMBComport.Items[0];
    1 : FSdpoSerial.Device := Dlg.FCMBComport.Items[1];
    2 : FSdpoSerial.Device := Dlg.FCMBComport.Items[2];
    3 : FSdpoSerial.Device := Dlg.FCMBComport.Items[3];
    4 : FSdpoSerial.Device := Dlg.FCMBComport.Items[4];
    5 : FSdpoSerial.Device := Dlg.FCMBComport.Items[5];
    6 : FSdpoSerial.Device := Dlg.FCMBComport.Items[6];
    7 : FSdpoSerial.Device := Dlg.FCMBComport.Items[7];
    8 : FSdpoSerial.Device := Dlg.FCMBComport.Items[8];
    9 : FSdpoSerial.Device := Dlg.FCMBComport.Items[9];
    10 : FSdpoSerial.Device := Dlg.FCMBComport.Items[10];
    11 : FSdpoSerial.Device := Dlg.FCMBComport.Items[11];
    12 : FSdpoSerial.Device := Dlg.FCMBComport.Items[12];
    13 : FSdpoSerial.Device := Dlg.FCMBComport.Items[13];
    14 : FSdpoSerial.Device := Dlg.FCMBComport.Items[14];
    15 : FSdpoSerial.Device := Dlg.FCMBComport.Items[15];
  end;}
  FSdpoSerial.Device := Dlg.FCMBComport.Text;
  case Dlg.FCMBBaudRate.ItemIndex of
    0 : FSdpoSerial.BaudRate := br__1200;
    1 : FSdpoSerial.BaudRate := br__2400;
    2 : FSdpoSerial.BaudRate := br__4800;
    3 : FSdpoSerial.BaudRate := br__9600;
    4 : FSdpoSerial.BaudRate := br_19200;
    5 : FSdpoSerial.BaudRate := br_38400;
    6 : FSdpoSerial.BaudRate := br_57600;
    7 : FSdpoSerial.BaudRate := br115200;
  end;
  case Dlg.FCMBDatabits.ItemIndex of
    0 : FSdpoSerial.DataBits := db7bits;
    1 : FSdpoSerial.DataBits := db8bits;
  end;
  case Dlg.FCMBParity.ItemIndex of
    0 : FSdpoSerial.Parity := pNone;
    1 : FSdpoSerial.Parity := pEven;
    2 : FSdpoSerial.Parity := pOdd;
  end;
  case Dlg.FCMBStopbits.ItemIndex of
    0 : FSdpoSerial.StopBits := sbOne;
    1 : FSdpoSerial.StopBits := sbTwo;
  end;
end;

constructor TGPSPortConnected.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{  FSdpoSerial := TSdpoSerial.Create(self);
  with FSdpoSerial do
  begin
    Active := false;
    {$IFDEF WINDOWS}
    Device := 'COM1';
    {$ELSE}
    Device := '/dev/ttyS0';
    {$ENDIF}
    BaudRate := br__9600;
    Databits := db8bits;
    Parity := pNone;
    Stopbits := sbOne;
    OnRxData := @DoOnRxData;
  end;}
end;

destructor TGPSPortConnected.Destroy;
begin
  FreeAndNil(FComSetupForm);
  {FSdpoSerial.Free;}
  inherited Destroy;
end;

procedure TGPSPortConnected.CloseDialog;
begin
  if Assigned(FComSetupForm) then
    FComSetupForm.Close;
end;

//CreateForm on SetupPort method.
function TGPSPortConnected.SetupPort : Boolean;
begin
  Result := False;
  if not Assigned(FComSetupForm) then
    FComSetupForm := CreateForm;

  if Assigned(FComSetupForm) then begin
    SetFormValues;
    FComSetupForm.OnClose := @DoCloseForm;
    FComSetupForm.OnShow := Self.OnShow;
    FComSetupForm.HelpContext := HelpContext;
    FComSetupForm.Caption := Title;
    //Once the issues concerning FormStyle = fsStayOnTop are resolved we can
    //make this a non-Modal window using FComSetupForm.Show;
    FComSetupForm.ShowModal;
    Result := true;
  end
  else
    inherited Execute;
end;

procedure TGPSPortConnected.DoOKButtonClick(Sender : TObject);
begin
  GetFormValues;
  CloseDialog;
end;

procedure TGPSPortConnected.DoCancelButtonClick(Sender : TObject);
begin
  CloseDialog;
end;

procedure  TGPSPortConnected.SetSdpoSerial(const value : TSdpoSerial);
begin
  if Assigned(value) then
    FSdpoSerial := value;
end;

Initialization
{$i comportsetup.lrs}
{$i gpsportconnected.lrs}
end.

