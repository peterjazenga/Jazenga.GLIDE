unit umainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms,
  Buttons, StdCtrls, uLongTimer;

{ Tmainform }
type
  Tmainform = class(TForm)
    cmd_clearmemo: TBitBtn;
    cmb_Daily24Hour: TComboBox;
    cmb_IntervalType: TComboBox;
    cmb_weekordate: TComboBox;
    cmd_Close: TBitBtn;
    cmd_StopTimer: TButton;
    cmd_StartTimer: TButton;
    cmb_SampleInterval: TComboBox;
    crp_SetTimer: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    LongTimer1: TLongTimer;
    memo_ReportTimerEvent: TMemo;
    procedure cmd_clearmemoClick(Sender: TObject);
    procedure cmb_SampleIntervalChange(Sender: TObject);
    procedure cmd_StopTimerClick(Sender: TObject);
    procedure cmd_StartTimerClick(Sender: TObject);
    procedure cmb_Daily24HourChange(Sender: TObject);
    procedure cmb_IntervalTypeChange(Sender: TObject);
    procedure cmb_weekordateChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LongTimer1Sample(Sender: TObject);
    procedure LongTimer1StartTimer(Sender: TObject);
    procedure LongTimer1StopTimer(Sender: TObject);
    procedure LongTimer1Timer(Sender: TObject);
  private
    { private declarations }
    procedure PopulateWeekOrDate(const i: integer);

  public
    { public declarations }
  end;

var
  mainform: Tmainform;

implementation

{$R *.lfm}

{ Tmainform }

procedure Tmainform.LongTimer1Timer(Sender: TObject);
begin
  // memo_ReportTimerEvent.Lines.Add('LastFired at ' + FormatDateTime('hh:nn:ss', LongTimer1.LastFired));
  memo_ReportTimerEvent.Lines.Add('Timer fired at ' + FormatDateTime('hh:nn:ss dd-mm-yyyy', Now));
  //memo_ReportTimerEvent.Lines.Add(LongTimer1.fDebugString);

end;

procedure Tmainform.FormCreate(Sender: TObject);
var i: integer;
begin
  Caption := Application.Title;
  memo_ReportTimerEvent.Clear;
  cmb_Daily24Hour.Clear;
  cmb_Daily24Hour.Items.Add('Midnight');
  for i := 1 to 23 do
    cmb_Daily24Hour.Items.Add(Format('%2.d:00', [i]));
  LongTimer1.IntervalType := lt1Daily;
  LongTimer1.Daily24Hour := 0;
  LongTimer1.Enabled := False;
  cmb_Daily24Hour.ItemIndex := 0;
  cmb_SampleInterval.Clear;
  cmb_SampleInterval.Items.Add('Every minute');
  cmb_SampleInterval.Items.Add('Every 5 minutes');
  cmb_SampleInterval.Items.Add('Every 10 minutes');
  cmb_SampleInterval.Items.Add('Every 30 minutes');
  cmb_SampleInterval.Items.Add('Every 45 minutes');
  memo_ReportTimerEvent.Lines.Add('Timer initially disabled');
  cmb_SampleInterval.ItemIndex := 2;
end;

procedure Tmainform.LongTimer1Sample(Sender: TObject);
begin
  memo_ReportTimerEvent.Lines.Add('Sampled at ' + FormatDateTime('hh:nn:ss', Now));
  //memo_ReportTimerEvent.Lines.Add(LongTimer1.fDebugString);
end;

procedure Tmainform.LongTimer1StartTimer(Sender: TObject);
begin
  Self.Caption := 'Timer was started at ' + FormatDateTime('dd/mm/yyyy hh:nn:ss', Now);
end;

procedure Tmainform.LongTimer1StopTimer(Sender: TObject);
begin
  Self.Caption := Application.Title;
end;

procedure Tmainform.cmd_StopTimerClick(Sender: TObject);
begin
  LongTimer1.Enabled := False;
  memo_ReportTimerEvent.Lines.Add('Timer disabled');
end;

procedure Tmainform.cmb_SampleIntervalChange(Sender: TObject);
begin
  LongTimer1.SampleInterval := TSampleInterval(cmb_SampleInterval.ItemIndex);
end;

procedure Tmainform.cmd_clearmemoClick(Sender: TObject);
begin
  memo_ReportTimerEvent.Lines.Clear;
end;

procedure Tmainform.cmd_StartTimerClick(Sender: TObject);
begin
  LongTimer1.Enabled := True;
  memo_ReportTimerEvent.Lines.Add('Timer enabled');
end;

procedure Tmainform.cmb_Daily24HourChange(Sender: TObject);
begin
  LongTimer1.Daily24Hour := cmb_Daily24Hour.ItemIndex;
end;
procedure Tmainform.PopulateWeekOrDate(const i: integer);
// 0=Weekly, 1=Monthly
var iMonthDay: integer;

begin
  cmb_weekordate.Clear;
  case i of
    0: begin
      cmb_weekordate.Items.Add('Monday');
      cmb_weekordate.Items.Add('Tuesday');
      cmb_weekordate.Items.Add('Wednesday');
      cmb_weekordate.Items.Add('Thursday');
      cmb_weekordate.Items.Add('Friday');
      cmb_weekordate.Items.Add('Saturday');
      cmb_weekordate.Items.Add('Sunday');
    end;
    1: begin
      for iMonthDay := 1 to 31 do
        cmb_weekordate.Items.Add(Format('%2.d', [iMonthDay]));
    end;
  end;
  cmb_weekordate.ItemIndex := 0;
end;

procedure Tmainform.cmb_IntervalTypeChange(Sender: TObject);
begin
  cmd_StopTimer.Click;
  case cmb_IntervalType.ItemIndex of
    0:
    begin
      LongTimer1.IntervalType := lt1Daily;
      cmb_weekordate.Enabled := False;
    end;
    1:
    begin
      LongTimer1.IntervalType := lt2Weekly;
      PopulateWeekOrDate(0);
      cmb_weekordate.Enabled := True;
    end;
    2:
    begin
      LongTimer1.IntervalType := lt3Monthly;
      PopulateWeekOrDate(1);
      cmb_weekordate.Enabled := True;
    end;
  end;
end;

procedure Tmainform.cmb_weekordateChange(Sender: TObject);
begin
  case LongTimer1.IntervalType of
    lt2Weekly: LongTimer1.WeeklyDay := TDay(cmb_weekordate.ItemIndex);
    lt3Monthly: LongTimer1.MonthlyDate := (cmb_weekordate.ItemIndex + 1);
  end;
end;

end.

