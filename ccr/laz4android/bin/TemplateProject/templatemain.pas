unit templatemain;

{$mode objfpc}{$H+}

{$ifdef Linux}{$ifdef CPUARM}
  {$define Android}
{$endif}{$endif}

interface

uses
  Classes,
  SysUtils,
  FileUtil,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  StrUtils,
  customdrawnint,
  LCLIntf,
  customdrawncontrols,
  customdrawndrawers,
  customdrawn_common,
  lazdeviceapis;

type

  { TfrmTemplateMain }

  TfrmTemplateMain = class(TForm)
    CDButton1: TCDButton;
    CDButton2: TCDButton;
    CDButton3: TCDButton;
    Label1: TLabel;
    Label2: TLabel;
    Timer1: TTimer;
    procedure CDButton1Click(Sender: TObject);
    procedure CDButton2Click(Sender: TObject);
    procedure CDButton3Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    procedure MyOnListViewDialogResult(ASelectedItem: Integer);
  public
  end;

var
  frmTemplateMain: TfrmTemplateMain;
  tics, timerTics : integer;

implementation

{$R *.lfm}

{ TfrmTemplateMain }

procedure TfrmTemplateMain.FormCreate(Sender: TObject);
begin
  tics := 0;
  timerTics := 0;
end;

procedure TfrmTemplateMain.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  Timer1.Enabled:=false;
end;

procedure TfrmTemplateMain.CDButton1Click(Sender: TObject);
begin
  Inc(tics);
  Label1.Caption:='Counts:' + IntToStr(tics);
end;

procedure TfrmTemplateMain.CDButton2Click(Sender: TObject);
begin
  {$ifdef LCLCustomDrawn}
    LCLIntf.OnListViewDialogResult := @MyOnListViewDialogResult;
    CDWidgetSet.ShowListViewDialog('',
      ['StartTimer', 'StopTimer', 'Exit'],
      ['', '', '']);
  {$endif}
end;

procedure TfrmTemplateMain.CDButton3Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmTemplateMain.MyOnListViewDialogResult(ASelectedItem: Integer);
begin
  Timer1.Enabled:=ASelectedItem = 0;
end;

procedure TfrmTemplateMain.Timer1Timer(Sender: TObject);
begin
  Inc(timerTics);
  Label1.Caption:='TimerTics:' + IntToStr(timerTics);
end;

end.

