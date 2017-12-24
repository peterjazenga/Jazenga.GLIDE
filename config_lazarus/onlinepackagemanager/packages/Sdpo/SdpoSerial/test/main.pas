unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, SdpoSerial,
  StdCtrls;

type

  { TFMain }

  TFMain = class(TForm)
    BSend: TButton;
    BOpen: TButton;
    BClose: TButton;
    CBDTR: TCheckBox;
    EditSend: TEdit;
    Memo: TMemo;
    Serial: TSdpoSerial;
    procedure BCloseClick(Sender: TObject);
    procedure BOpenClick(Sender: TObject);
    procedure BSendClick(Sender: TObject);
    procedure CBDTRChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SerialRxData(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  FMain: TFMain;

implementation

{ TFMain }

procedure TFMain.FormCreate(Sender: TObject);
begin

end;

procedure TFMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Serial.active := false;
end;

procedure TFMain.FormShow(Sender: TObject);
begin
//  Serial.Active := true;
end;


procedure TFMain.BSendClick(Sender: TObject);
begin
  Serial.WriteData(EditSend.Text);
end;

procedure TFMain.CBDTRChange(Sender: TObject);
begin
  Serial.SetDTR(CBDTR.Checked);
end;

procedure TFMain.BOpenClick(Sender: TObject);
begin
  Serial.Open;
end;

procedure TFMain.BCloseClick(Sender: TObject);
begin
  Serial.Close;
end;

procedure TFMain.SerialRxData(Sender: TObject);
begin
  Memo.Lines.Add(Serial.ReadData);
end;


initialization
  {$I main.lrs}

end.

