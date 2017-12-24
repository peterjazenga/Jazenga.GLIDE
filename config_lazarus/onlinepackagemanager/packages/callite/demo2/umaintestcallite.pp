unit uMainTestCalLite;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Graphics, ExtCtrls, StdCtrls, Spin, Dialogs,
  Controls, Menus, CalendarLite;

type

  { TForm1 }

  TForm1 = class(TForm)
    BtnFont: TButton;
    cbUseHolidays: TCheckBox;
    cgOptions: TCheckGroup;
    CbArrowBorder: TColorButton;
    CbTodayFrame: TColorButton;
    CbTopRow: TColorButton;
    CbTopRowText: TColorButton;
    CbWeekend: TColorButton;
    CbArrow: TColorButton;
    CbBackground: TColorButton;
    CbBorder: TColorButton;
    CbDayLine: TColorButton;
    CbHolidays: TColorButton;
    CbPastMonth: TColorButton;
    CbSelectedDate: TColorButton;
    CbText: TColorButton;
    CbPrepareCanvas: TCheckBox;
    CbDrawCell: TCheckBox;
    CbAddHolidayNameToCell: TCheckBox;
    CbShowHints: TCheckBox;
    CbMultiSelect: TCheckBox;
    CbUseBuiltinPopup: TCheckBox;
    FontDialog: TFontDialog;
    GroupBox1: TGroupBox;
    ImageList1: TImageList;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    PopupMenu1: TPopupMenu;
    SelDateListbox: TListBox;
    LTitle: TLabel;
    LWidth: TLabel;
    lHeight: TLabel;
    PSettings: TPanel;
    rgLanguage: TRadioGroup;
    rgStartingDOW: TRadioGroup;
    seWidth: TSpinEdit;
    seHeight: TSpinEdit;
    procedure BtnFontClick(Sender: TObject);
    procedure CbAddHolidayNameToCellChange(Sender: TObject);
    procedure CbDrawCellChange(Sender: TObject);
    procedure CbMultiSelectChange(Sender: TObject);
    procedure CbPrepareCanvasChange(Sender: TObject);
    procedure CbShowHintsChange(Sender: TObject);
    procedure CbUseBuiltinPopupChange(Sender: TObject);
    procedure ColorButtonChanged(Sender: TObject);
    procedure cbUseHolidaysChange(Sender: TObject);
    procedure cgOptionsItemClick(Sender: TObject; Index: integer);
    procedure FormCreate(Sender: TObject);
    procedure rgLanguageClick(Sender: TObject);
    procedure rgStartingDOWClick(Sender: TObject);
    procedure seHeightChange(Sender: TObject);
    procedure seWidthChange(Sender: TObject);
  private
    copyCal, demoCal: TCalendarLite;
    FNoHolidays: boolean;
    procedure RespondToDateChange(Sender: TObject);
    procedure RespondToMonthChange(Sender: TObject);
    procedure GetDayText(Sender: TObject; AYear, AMonth, ADay: Word; var AText: String);
    procedure GetHintText(Sender: TObject; AYear, AMonth, ADay: Word; var AText: String);
    procedure GetHolidays(Sender: TObject; AMonth, AYear: Integer;  // wp
      var Holidays: THolidays);
    procedure PrepareCanvas(Sender: TObject; ACanvas: TCanvas;
      AYear, AMonth, ADay: Word; AState: TCalCellStates);
    procedure DrawCell(Sender: TObject; ACanvas: TCanvas;
      AYear, AMonth, ADay: Word; AState: TCalCellStates; var ARect: TRect;
      var AContinueDrawing: Boolean);
  end;

var
  Form1: TForm1;


implementation

{$R *.lfm}

uses
  DateUtils;

function Easter(year:integer) : TDateTime;  // wp
var
  Day, Month    : integer;
  a,b,c,d,e,m,n : integer;
begin
  case Year div 100 of
    17    : begin m := 23; n := 3; end;
    18    : begin m := 23; n := 4; end;
    19,20 : begin m := 24; n := 5; end;
    21    : begin m := 24; n := 6; end;
    else    raise Exception.Create('Only years after 1700 supported.');
  end;
  a := Year mod 19;
  b := Year mod 4;
  c := Year mod 7;
  d := (19*a + m) mod 30;
  e := (2*b + 4*c + 6*d + n) mod 7;
  day := 22 + d + e;
  Month := 3;
  if Day>31 then begin
    Day := d + e - 9;
    Month := 4;
    if (d=28) and (e=6) and (a>10) then begin
      if day=26 then day := 19;
      if day=25 then day := 18;
    end;
  end;
  result := EncodeDate(year, month, day);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  opt: TCalOption;
begin
  demoCal := TCalendarLite.Create(Self);
  demoCal.Parent := Self;
  demoCal.Left := 10;
  demoCal.Top := PSettings.Height + 10;
  demoCal.Width := seWidth.Value;
  demoCal.Height := seHeight.Value;
  demoCal.OnGetHolidays := @GetHolidays;
  demoCal.OnDateChange:= @RespondToDateChange;
  demoCal.OnMonthChange := @RespondToMonthChange;
  demoCal.OnHint := @GetHintText;
  demoCal.ShowHint := true;
  demoCal.Hint := 'Calendar';
  if CbPrepareCanvas.Checked then
    demoCal.OnPrepareCanvas := @PrepareCanvas else
    demoCal.OnPrepareCanvas := nil;
  if CbDrawCell.Checked then
    demoCal.OnDrawCell := @DrawCell else
    demoCal.OnDrawCell := nil;
  FNoHolidays := False;
  for opt in demoCal.Options do
    if (opt in demoCal.Options) then cgOptions.Checked[integer(opt)] := True;
  seHeight.Value := demoCal.Height;
  seWidth.Value := demoCal.Width;
  rgStartingDOW.ItemIndex := integer(demoCal.StartingDayOfWeek)-1;

  copyCal:= TCalendarLite.Create(Self);
  copyCal.Parent := Self;
  copyCal.Width := 270;
  copyCal.Height := 205;
  copyCal.Left := Width - copyCal.Width;
  copyCal.Top := Height - copyCal.Height;
  copyCal.Font.Name := 'Lucida Calligraphy';
  copyCal.Colors.SelectedDateColor := clYellow;
  copyCal.Colors.ArrowBorderColor := clYellow;
  copyCal.Colors.ArrowColor := clYellow;
  copyCal.Colors.TodayFrameColor := clWhite;
  copyCal.Colors.BackgroundColor:= clGradientActiveCaption;
  copyCal.StartingDayOfWeek:= dowSaturday;
  copyCal.OnGetHolidays := @GetHolidays;
  copyCal.Options := copyCal.Options + [coShowBorder,coUseTopRowColors,coDayLine];

  CbArrowBorder.ButtonColor := demoCal.Colors.ArrowBorderColor;
  CbArrow.ButtonColor := demoCal.Colors.ArrowColor;
  CbBackground.ButtonColor := demoCal.Colors.BackgroundColor;
  CbBorder.ButtonColor := demoCal.Colors.BorderColor;
  CbDayLine.ButtonColor := demoCal.Colors.DayLineColor;
  CbHolidays.Buttoncolor := demoCal.colors.HolidayColor;
  CbPastMonth.ButtonColor := demoCal.Colors.PastMonthColor;
  CbSelectedDate.ButtonColor := demoCal.Colors.SelectedDateColor;
  CbText.ButtonColor := demoCal.Colors.TextColor;
  CbTodayFrame.ButtonColor := demoCal.Colors.TodayFrameColor;
  CbTopRow.ButtonColor := demoCal.Colors.TopRowColor;
  CbTopRowText.ButtonColor := democal.Colors.TopRowTextColor;
  CbWeekend.ButtonColor := demoCal.Colors.WeekendColor;
end;

procedure TForm1.rgLanguageClick(Sender: TObject);
begin
  demoCal.Languages := TLanguage(rgLanguage.ItemIndex);
  copyCal.Languages := demoCal.Languages;
end;

procedure TForm1.rgStartingDOWClick(Sender: TObject);
begin
  demoCal.StartingDayOfWeek := TDayOfWeek(rgStartingDOW.ItemIndex + 1);
end;

procedure TForm1.seHeightChange(Sender: TObject);
begin
  demoCal.Height := seHeight.Value;
end;

procedure TForm1.seWidthChange(Sender: TObject);
begin
  demoCal.Width := seWidth.Value;
end;

procedure TForm1.ColorButtonChanged(Sender: TObject);
var
  calendar: TCalendarLite;
  col: TColor;
begin
  calendar := demoCal;
  col := (Sender as TColorButton).ButtonColor;
  case (Sender as TColorButton).Name of
    'CbArrowBorder': calendar.Colors.ArrowBorderColor := col;
    'CbArrow': calendar.Colors.ArrowColor := col;
    'CbBackground': calendar.Colors.BackgroundColor := col;
    'CbBorder': calendar.Colors.BorderColor := col;
    'CbDayLine': calendar.Colors.DayLineColor := col;
    'CbHolidays': calendar.Colors.HolidayColor := col;
    'CbPastMonth': calendar.Colors.PastMonthColor := col;
    'CbSelectedDate': calendar.Colors.SelectedDateColor := col;
    'CbText': calendar.Colors.TextColor := col;
    'CbTodayFrame': calendar.Colors.TodayFrameColor := col;
    'CbTopRow': calendar.Colors.TopRowColor := col;
    'CbTopRowText': calendar.Colors.TopRowTextColor := col;
    'CbWeekend': calendar.Colors.WeekendColor := col;
  end;
  calendar.Invalidate;
end;

procedure TForm1.cbUseHolidaysChange(Sender: TObject);
begin
  FNoHolidays := not FNoHolidays;
end;

procedure TForm1.cgOptionsItemClick(Sender: TObject; Index: integer);
var opt: TCalOption;
begin
  opt := TCalOption(Index);
  if (opt in demoCal.Options) then
    demoCal.Options := demoCal.Options - [opt]
  else
    demoCal.Options := demoCal.Options + [opt];
  copyCal.Options := demoCal.Options;
end;

procedure TForm1.CbUseBuiltinPopupChange(Sender: TObject);
begin
  if CbUseBuiltinPopup.Checked then
    demoCal.PopupMenu := nil else
    demoCal.PopupMenu := PopupMenu1;
end;

procedure TForm1.CbAddHolidayNameToCellChange(Sender: TObject);
begin
  if CbAddHolidayNameToCell.Checked then
    demoCal.OnGetDayText := @GetDayText else
    demoCal.OnGetDayText := nil;
  demoCal.Invalidate;
end;

procedure TForm1.BtnFontClick(Sender: TObject);
begin
  FontDialog.Font.Assign(demoCal.Font);
  if FontDialog.Execute then
    demoCal.Font.Assign(FontDialog.Font);
end;

procedure TForm1.CbDrawCellChange(Sender: TObject);
begin
  if CbDrawCell.Checked then
    demoCal.OnDrawCell := @DrawCell else
    demoCal.OnDrawCell := nil;
  demoCal.Invalidate;
end;

procedure TForm1.CbMultiSelectChange(Sender: TObject);
begin
  demoCal.MultiSelect := CbMultiSelect.Checked;
end;

procedure TForm1.CbPrepareCanvasChange(Sender: TObject);
begin
  if CbPrepareCanvas.Checked then
    demoCal.OnPrepareCanvas := @PrepareCanvas else
    demoCal.OnPrepareCanvas := nil;
  demoCal.Invalidate;
end;

procedure TForm1.CbShowHintsChange(Sender: TObject);
begin
  demoCal.ShowHint := CbShowHints.Checked;
end;

procedure TForm1.RespondToDateChange(Sender: tObject);
var
  s: TCalDateArray;
  i: Integer;
begin
  copyCal.Date:= TCalendarLite(Sender).Date;

  s := demoCal.SelectedDates;
  SelDateListbox.Clear;
  for i:=0 to High(s) do
    SelDateListbox.Items.Add(DateToStr(s[i]));
end;

procedure TForm1.RespondToMonthChange(Sender: TObject);
begin
  Label1.Caption := 'Month changed to ' + demoCal.GetMonthName(MonthOf(democal.Date));
end;

procedure TForm1.GetDayText(Sender: TObject; AYear, AMonth, ADay: Word;
  var AText: String);
var
  s: String;
begin
  GetHintText(Sender, AYear, AMonth, ADay, s);
  if s <> '' then
    AText := IntToStr(ADay) + LineEnding + s;
end;

procedure TForm1.GetHintText(Sender: TObject; AYear, AMonth, ADay: Word;
  var AText: String);
var
  dt, e: TDate;
begin
  AText := '';
  case AMonth of
    1: if ADay = 1 then AText := 'New Year';
   12: if ADay = 25 then AText := 'Christmas';
   else
       e := Easter(AYear);
       dt := EncodeDate(AYear, AMonth, ADay);
       if (dt = e) then
         AText := 'Easter'
       else if (dt = e + 49) then
         AText := 'Whit Sunday';
  end;
end;

procedure TForm1.GetHolidays(Sender: TObject; AMonth, AYear: Integer;
  var Holidays: THolidays);
var
  d, m, y: Word;
  e: TDate;
begin
  ClearHolidays(Holidays);
  if not FNoHolidays then
  begin
    // Fixed holidays
    case AMonth of
      1: AddHoliday(1, Holidays);          // New Year
     12: AddHoliday(25, Holidays);         // Christmas
    end;
    // Easter
    e := Easter(AYear);
    DecodeDate(e, y,m,d);
    if m = AMonth then
      AddHoliday(d, Holidays);
    // Whit Sunday --> 49 days after easter
    DecodeDate(e+49, y,m,d);
    if m = AMonth then
      AddHoliday(d, Holidays);
  end;
end;

procedure TForm1.PrepareCanvas(Sender: TObject; ACanvas: TCanvas;
  AYear,AMonth,ADay: word; AState: TCalCellStates);
begin
  if (ADay = 1) and not (csOtherMonth in AState) then
  begin
    ACanvas.Font.Size := 12;
    ACanvas.Font.Style := [fsUnderline, fsItalic, fsBold];
    ACanvas.Font.Color := clGreen;
    ACanvas.Brush.Color := clSilver;
    ACanvas.Brush.Style := bsFDiagonal;
    ACanvas.Pen.Color := clSilver;
    ACanvas.Pen.Style := psSolid;
  end;
end;

procedure TForm1.DrawCell(Sender: TObject; ACanvas: TCanvas;
  AYear,AMonth,ADay: Word; AState: TCalCellStates; var ARect: TRect;
  var AContinueDrawing: Boolean);
var
  bmp: TBitmap;
begin
  if (AMonth = 11) and (ADay = 11) and not (csOtherMonth in AState) then begin
    bmp := TBitmap.Create;
    try
      ImageList1.GetBitmap(0, bmp);
      ACanvas.Draw(ARect.Left, (ARect.Top + ARect.Bottom - bmp.Height) div 2, bmp);
      inc(ARect.Left, bmp.Width + 2);
    finally
      bmp.Free;
    end;
  end;
end;

end.

