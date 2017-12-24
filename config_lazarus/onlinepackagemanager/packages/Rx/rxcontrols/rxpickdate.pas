{ pickdate unit

  Copyright (C) 2005-2017 Lagunov Aleksey alexs75@yandex.ru and Lazarus team
  original conception from rx library for Delphi (c)

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit rxpickdate;

{$I rx.inc}

interface

uses
  LCLType, Classes, Controls, SysUtils, Graphics, rxdateutil, Grids,
  LCLProc, LMessages, ExtCtrls, StdCtrls, Buttons, Forms, Menus;

{ TRxCalendar }

type
  TDayOfWeek = 0..6;

  TDaysItem = packed record
    DayNum:byte;
    DayDate:TDateTime;
    DayColor:TColor;
  end;
  
  TDaysArray = array[0..6, 1..6] of TDaysItem;

  { TCustomRxCalendar }

  TCustomRxCalendar = class(TCustomDrawGrid)
  private
    FDate: TDateTime;
    FMonthOffset: Integer;
    FNotInThisMonthColor: TColor;
    FOnChange: TNotifyEvent;
    FReadOnly: Boolean;
    FStartOfWeek: TDayOfWeekName;
    FUpdating: Boolean;
    FUseCurrentDate: Boolean;
    FWeekends: TDaysOfWeek;
    FWeekendColor: TColor;
    FDaysArray:TDaysArray;
    FShortDaysOfWeek: TStrings;
    function GetDateElement(Index: Integer): Integer;
    procedure FillDaysArray;
    function GetShortDaysOfWeek: TStrings;
    procedure SetCalendarDate(Value: TDateTime);
    procedure SetDateElement(Index: Integer; Value: Integer);
    procedure SetNotInThisMonthColor(const AValue: TColor);
    procedure SetShortDaysOfWeek(const AValue: TStrings);
    procedure SetStartOfWeek(Value: TDayOfWeekName);
    procedure SetUseCurrentDate(Value: Boolean);
    procedure SetWeekendColor(Value: TColor);
    procedure SetWeekends(Value: TDaysOfWeek);
    function IsWeekend(ACol, ARow: Integer): Boolean;
    procedure CalendarUpdate(DayOnly: Boolean);
    function StoreCalendarDate: Boolean;
    procedure AddWeek;
    procedure DecWeek;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Change; dynamic;
    procedure ChangeMonth(Delta: Integer);
    procedure Click; override;
    function DaysThisMonth: Integer;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure LMSize(var Message: TLMSize); message LM_SIZE;
    procedure RxCalendarMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure RxCalendarMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure UpdateShortDaysOfWeek; virtual;

    property CalendarDate: TDateTime read FDate write SetCalendarDate
      stored StoreCalendarDate;
    property Day: Integer index 3  read GetDateElement write SetDateElement stored False;
    property Month: Integer index 2  read GetDateElement write SetDateElement stored False;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
    property StartOfWeek: TDayOfWeekName read FStartOfWeek write SetStartOfWeek default Mon;
    property UseCurrentDate: Boolean read FUseCurrentDate write SetUseCurrentDate default True;
    property WeekendColor: TColor read FWeekendColor write SetWeekendColor default clRed;
    property Weekends: TDaysOfWeek read FWeekends write SetWeekends default [Sun];
    property Year: Integer index 1  read GetDateElement write SetDateElement stored False;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property NotInThisMonthColor:TColor read FNotInThisMonthColor write SetNotInThisMonthColor default clSilver;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure NextMonth;
    procedure NextYear;
    procedure PrevMonth;
    procedure PrevYear;
    procedure UpdateCalendar; virtual;
    property ShortDaysOfWeek: TStrings read GetShortDaysOfWeek write SetShortDaysOfWeek;
  end;

  { TRxCalendar1 }

  TRxCalendarGrid = class(TCustomRxCalendar)
  protected
    procedure SetBounds(aLeft, aTop, aWidth, aHeight: integer); override;
  published
    property Align;
    property Anchors;
    property BorderColor;
    property BorderSpacing;
    property CalendarDate;
    property Constraints;
    property Day;
    property Font;
    property Hint;
    property Month;
    property NotInThisMonthColor;
    property PopupMenu;
    property ReadOnly;
    property SelectedColor;
    property ShortDaysOfWeek; //
    property StartOfWeek;
    property TabStop;
    property UseCurrentDate;
    property Visible;
    property WeekendColor;
    property Weekends;
    property Year;

    property OnChange;
    property OnClick;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnPrepareCanvas;
  end;

{ TPopupCalendar }

type
  TCloseUpEvent = procedure (Sender: TObject; Accept: Boolean) of object;

  TPopupCalendar = class(TForm)
  private
    FCalendar: TCustomRxCalendar;
    FCloseUp: TCloseUpEvent;
    FTitleLabel: TLabel;
    FFourDigitYear: Boolean;
    FBtns: array[0..3] of TSpeedButton;
    FMonthMenu:TPopupMenu;
    FMonthNames: TStrings;
    procedure CalendarMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    function GetDate: TDateTime;
    procedure PrevMonthBtnClick(Sender: TObject);
    procedure NextMonthBtnClick(Sender: TObject);
    procedure PrevYearBtnClick(Sender: TObject);
    procedure NextYearBtnClick(Sender: TObject);
    procedure CalendarChange(Sender: TObject);
    procedure SetDate(const AValue: TDateTime);
    procedure SetMonthNames(const AValue: TStrings);
    procedure TopPanelDblClick(Sender: TObject);
    procedure MonthMenuClick(Sender: TObject);
    procedure CalendarDblClick(Sender: TObject);
  protected
    FCloseBtn:TBitBtn;
    FControlPanel:TPanel;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Paint;override;
    procedure Deactivate; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AutoSizeForm;
    property Date:TDateTime read GetDate write SetDate;
    property OnCloseUp: TCloseUpEvent read FCloseUp write FCloseUp;
    property Calendar: TCustomRxCalendar read FCalendar;
    property MonthNames: TStrings read FMonthNames write SetMonthNames;
  end;

{ TSelectDateDlg }

type
  TSelectDateDlg = class(TForm)
    Calendar: TCustomRxCalendar;
    TitleLabel: TLabel;
    FMonthMenu:TPopupMenu;
    procedure PrevMonthBtnClick(Sender: TObject);
    procedure NextMonthBtnClick(Sender: TObject);
    procedure PrevYearBtnClick(Sender: TObject);
    procedure NextYearBtnClick(Sender: TObject);
    procedure CalendarChange(Sender: TObject);
    procedure CalendarDblClick(Sender: TObject);
    procedure TopPanelDblClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure MonthMenuClick(Sender: TObject);
  private
    { Private declarations }
    FBtns: array[0..3] of TSpeedButton;
    procedure SetDate(Date: TDateTime);
    function GetDate: TDateTime;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    property Date: TDateTime read GetDate write SetDate;
  end;

{ Calendar dialog }

function SelectDate(var Date: TDateTime; const DlgCaption: TCaption;
  AStartOfWeek: TDayOfWeekName; AWeekends: TDaysOfWeek;
  AWeekendColor: TColor; BtnHints: TStrings): Boolean;
function SelectDateStr(var StrDate: string; const DlgCaption: TCaption;
  AStartOfWeek: TDayOfWeekName; AWeekends: TDaysOfWeek;
  AWeekendColor: TColor; BtnHints: TStrings): Boolean;
function PopupDate(var Date: TDateTime; Edit: TWinControl): Boolean;

{ Popup calendar }

function CreatePopupCalendar(AOwner: TComponent
  {$IFDEF USED_BiDi}; ABiDiMode: TBiDiMode = bdLeftToRight {$ENDIF}): TPopupCalendar;
procedure SetupPopupCalendar(PopupCalendar: TWinControl;
  AStartOfWeek: TDayOfWeekName; AWeekends: TDaysOfWeek;
  AWeekendColor: TColor; BtnHints: TStrings; FourDigitYear: Boolean);

const
  PopupCalendarSize: TPoint = (X: 187; Y: 124);

implementation

uses Messages, RXCtrls, rxconst, rxtooledit, rxvclutils, math, LCLStrConsts, LResources;

{$R pickdate.res}

const
  SBtnGlyphs: array[0..3] of PChar = ('PREV2', 'PREV1', 'NEXT1', 'NEXT2');

procedure FontSetDefault(AFont: TFont);
(*
{$IFDEF WIN32}
var
  NonClientMetrics: TNonClientMetrics;
{$ENDIF}
*)
begin
(*
{$IFDEF WIN32}
  NonClientMetrics.cbSize := SizeOf(NonClientMetrics);
  if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NonClientMetrics, 0) then
    AFont.Handle := CreateFontIndirect(NonClientMetrics.lfMessageFont)
  else
{$ENDIF}
*)
  with AFont do
  begin
    Color := clWindowText;
{$IFDEF WINDOWS}
    Name := 'MS Sans Serif';
    Size := 8;
{$ELSE}
    if Assigned(Application) and Assigned(Application.MainForm) then
      Size := Application.MainForm.Font.Size
    else
      Size := 9;
    Name := 'default';
{$ENDIF}
    Style := [];
  end;
end;

function CreateRxCalendarPopupMenu(AOwner:TComponent; AOnClick:TNotifyEvent):TPopupMenu;
var
  i:integer;
  MI:TMenuItem;
begin
  Result:=TPopupMenu.Create(AOwner);
  for i:=1 to 12 do
  begin
    MI:=TMenuItem.Create(Result);
    MI.Caption := LongMonthNames[i];
    MI.OnClick:=AOnClick;
    MI.Tag:=i;
    Result.Items.Add(MI);
  end;

  MI:=TMenuItem.Create(Result);
  MI.Caption:='-';
  Result.Items.Add(MI);

  MI:=TMenuItem.Create(Result);
  MI.Caption:=sToCurDate;
  MI.OnClick:=AOnClick;
  MI.Tag:=-1;
  Result.Items.Add(MI);
end;

{ TRxTimerSpeedButton }

type
  TRxTimerSpeedButton = class(TRxSpeedButton)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AllowTimer default True;
  end;

constructor TRxTimerSpeedButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AllowTimer := True;
  ControlStyle := ControlStyle + [csReplicatable];
end;


  { TCustomRxCalendar }
  
constructor TCustomRxCalendar.Create(AOwner: TComponent);
var
  ADefaultTextStyle: TTextStyle;
begin
  inherited Create(AOwner);
  FShortDaysOfWeek := TStringList.Create;
  FUseCurrentDate := True;
  FStartOfWeek := Mon;
  FWeekends := [Sun];
  FWeekendColor := clRed;
  FNotInThisMonthColor:=clSilver;
  FixedCols := 0;
  FixedRows := 1;
  ColCount := 7;
  RowCount := 7;
  ScrollBars := ssNone;
  Options := Options - [goRangeSelect] + [goDrawFocusSelected];
  ControlStyle := ControlStyle + [csFramed];
  FDate := Date;
  ADefaultTextStyle:=DefaultTextStyle;
  ADefaultTextStyle.Alignment:=taCenter;
  ADefaultTextStyle.Layout:=tlCenter;
  DefaultTextStyle:=ADefaultTextStyle;
  FocusRectVisible := False;
  OnMouseWheelUp := @RxCalendarMouseWheelUp;
  OnMouseWheelDown := @RxCalendarMouseWheelDown;
  UpdateShortDaysOfWeek;
  UpdateCalendar;
  TitleStyle:=tsNative;
end;

destructor TCustomRxCalendar.Destroy;
begin
  FShortDaysOfWeek.Free;
  inherited Destroy;
end;

procedure TCustomRxCalendar.CreateParams(var Params: TCreateParams);
const
  ClassStylesOff = CS_VREDRAW or CS_HREDRAW;
begin
  inherited CreateParams(Params);
  with Params do
  begin
    WindowClass.Style := WindowClass.Style and DWORD(not ClassStylesOff);
    Style := Style or WS_CLIPCHILDREN;
  end;
end;

procedure TCustomRxCalendar.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TCustomRxCalendar.Click;
var
  TheCellText: string;
begin
  FDate := FDaysArray[Col, Row].DayDate;
  FUseCurrentDate := False;
  CalendarUpdate(false);
  Change;
  inherited Click;
end;

function TCustomRxCalendar.DaysThisMonth: Integer;
begin
  Result := DaysPerMonth(Year, Month);
end;

procedure TCustomRxCalendar.DrawCell(ACol, ARow: Longint; ARect: TRect;
  AState: TGridDrawState);
var
  DayNum:integer;
  R: TRect;
begin
  PrepareCanvas(aCol, aRow, aState);

  if (gdSelected in aState) and (gdFocused in aState) then
    Canvas.Brush.Color:=SelectedColor;

  Canvas.FillRect(aRect);
  DrawCellGrid(aCol,aRow,aRect,aState);

  if ARow>0 then
  begin
    if not ((gdSelected in aState) and (gdFocused in aState)) then
    begin
      if (FDaysArray[ACol, ARow].DayDate = Date) and (FDaysArray[ACol, ARow].DayColor <> FNotInThisMonthColor) then
      begin
        R := ARect;
        // Variant 1
        //Dec(R.Bottom, 1);
        //Dec(R.Right, 1);
        //Canvas.Frame3d(R, 1, bvLowered);

        // Variant 2
        RxFrame3D(Canvas, R, clWindowFrame, clBtnHighlight, 1);
        RxFrame3D(Canvas, R, clBtnShadow, clBtnFace, 1);
      end;
      Canvas.Font.Color:=FDaysArray[ACol, ARow].DayColor;
    end
    else
      Canvas.Font.Color := clHighlightText  // clWindow
    ;
    DrawCellText(ACol, ARow, ARect, AState, IntToStr(FDaysArray[ACol, ARow].DayNum));

  end
  else
  begin
    Canvas.Font.Color:=clWindowText;
    //DrawCellText(ACol, ARow, ARect, AState, ShortDayNames[(Ord(StartOfWeek) + ACol) mod 7 + 1]);
    if FShortDaysOfWeek <> nil then begin
      if ACol <= FShortDaysOfWeek.Count - 1 then
        DrawCellText(ACol, ARow, ARect, AState, FShortDaysOfWeek.Strings[(Ord(StartOfWeek) + ACol) mod 7]);
    end;
  end;
end;

procedure TCustomRxCalendar.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Shift = [] then
    case Key of
      VK_UP:
        begin
          DecWeek;
          Exit;
        end;
      VK_DOWN:
        begin
          AddWeek;
          Exit;
        end;
      VK_LEFT, VK_SUBTRACT:
        begin
          if (Day > 1) then Day := Day - 1
          else CalendarDate := CalendarDate - 1;
          Exit;
        end;
      VK_RIGHT, VK_ADD:
        begin
          if (Day < DaysThisMonth) then Day := Day + 1
          else CalendarDate := CalendarDate + 1;
          Exit;
        end;
      VK_PRIOR:
        begin
          ChangeMonth(-1);
          Exit;
        end;
      VK_NEXT:
        begin
          ChangeMonth(+1);
          Exit;
        end;
    end;
  inherited KeyDown(Key, Shift);
end;

procedure TCustomRxCalendar.KeyPress(var Key: Char);
begin
  if Key in ['T', 't'] then begin
    CalendarDate := Trunc(Now);
    Key := #0;
  end;
  inherited KeyPress(Key);
end;

procedure TCustomRxCalendar.LMSize(var Message: TLMSize);
var
  GridLinesH, GridLinesW: Integer;
begin
  GridLinesH := 6 * GridLineWidth;
  if (goVertLine in Options) or (goFixedVertLine in Options) then
    GridLinesW := 6 * GridLineWidth
  else GridLinesW := 0;
  DefaultColWidth := (Message.Width - GridLinesW) div 7;
  DefaultRowHeight := (Message.Height - GridLinesH) div 7;
end;

procedure TCustomRxCalendar.RxCalendarMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  DecWeek;
  Handled := True;
end;

procedure TCustomRxCalendar.RxCalendarMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  AddWeek;
  Handled := True;
end;

procedure TCustomRxCalendar.SetCalendarDate(Value: TDateTime);
begin
  if FDate <> Value then
  begin
    FDate := Value;
    UpdateCalendar;
    Change;
  end;
end;

function TCustomRxCalendar.StoreCalendarDate: Boolean;
begin
  Result := not FUseCurrentDate;
end;

procedure TCustomRxCalendar.AddWeek;
begin
  if (Day + 7 <= DaysThisMonth) then
    Day := Day + 7
  else
    CalendarDate := CalendarDate + 7;
end;

procedure TCustomRxCalendar.DecWeek;
begin
  if (Day - 7 >= 1) then
    Day := Day - 7
  else
    CalendarDate := CalendarDate - 7;
end;

function TCustomRxCalendar.GetDateElement(Index: Integer): Integer;
var
  AYear, AMonth, ADay: Word;
begin
  DecodeDate(FDate, AYear, AMonth, ADay);
  case Index of
    1: Result := AYear;
    2: Result := AMonth;
    3: Result := ADay;
    else Result := -1;
  end;
end;

procedure TCustomRxCalendar.FillDaysArray;
var
  x,y:integer;
  DayNum: Integer;
  FirstDate:TDateTime;
  AYear, AMonth, ADay:Word;
begin
  DecodeDate(FDate, AYear, AMonth, ADay);
  FirstDate := EncodeDate(AYear, AMonth, 1) + FMonthOffset-1;
  DayNum:=FMonthOffset;
  for y:=1 to 6 do
  begin
    for x:=0 to 6 do
    begin
      FDaysArray[x,y].DayDate:=FirstDate;
      if DayNum < 1 then
      begin
        FDaysArray[x,y].DayColor:=FNotInThisMonthColor;
        DecodeDate(FirstDate, AYear, AMonth, ADay);
        FDaysArray[x,y].DayNum:=ADay;
      end
      else
      if DayNum > DaysThisMonth then
      begin
        FDaysArray[x,y].DayColor:=FNotInThisMonthColor;
        DecodeDate(FirstDate, AYear, AMonth, ADay);
        FDaysArray[x,y].DayNum:=ADay;
      end
      else
      begin
        if IsWeekend(x, y) then
          FDaysArray[x,y].DayColor:=WeekendColor
        else
          FDaysArray[x,y].DayColor:=clWindowText;
        FDaysArray[x,y].DayNum:=DayNum;
      end;
      FirstDate:=FirstDate+1;
      DayNum:=DayNum+1;
    end;
  end;
end;

procedure TCustomRxCalendar.UpdateShortDaysOfWeek;
var
  Ind: Integer;
  OldNotify: TNotifyEvent;
begin
  if (FShortDaysOfWeek <> nil) and (FShortDaysOfWeek.Count = 0) then
  begin
    OldNotify := TStringList(FShortDaysOfWeek).OnChange;
    TStringList(FShortDaysOfWeek).OnChange := nil;
    for Ind := 1 to 7 do
      FShortDaysOfWeek.Add(DefaultFormatSettings.ShortDayNames[Ind]);
    TStringList(FShortDaysOfWeek).OnChange := OldNotify;
  end;
end;

function TCustomRxCalendar.GetShortDaysOfWeek: TStrings;
begin
  Result := FShortDaysOfWeek;
end;

procedure TCustomRxCalendar.SetDateElement(Index: Integer; Value: Integer);
var
  AYear, AMonth, ADay: Word;
begin
  if Value > 0 then begin
    DecodeDate(FDate, AYear, AMonth, ADay);
    case Index of
      1: if AYear <> Value then AYear := Value else Exit;
      2: if (Value <= 12) and (Value <> AMonth) then begin
           AMonth := Value;
           if ADay > DaysPerMonth(Year, Value) then
             ADay := DaysPerMonth(Year, Value);
         end else Exit;
      3: if (Value <= DaysThisMonth) and (Value <> ADay) then
           ADay := Value
         else Exit;
      else Exit;
    end;
    FDate := EncodeDate(AYear, AMonth, ADay);
    FUseCurrentDate := False;
    CalendarUpdate(Index = 3);
    Change;
  end;
end;

procedure TCustomRxCalendar.SetNotInThisMonthColor(const AValue: TColor);
begin
  if AValue <> FNotInThisMonthColor then
  begin
    FNotInThisMonthColor:=AValue;
    FillDaysArray;
    Invalidate;
  end;
end;

procedure TCustomRxCalendar.SetShortDaysOfWeek(const AValue: TStrings);
begin
  if AValue.Text <> FShortDaysOfWeek.Text then begin
    FShortDaysOfWeek.Assign(AValue);
    Invalidate; //
  end;
end;

procedure TCustomRxCalendar.SetWeekendColor(Value: TColor);
begin
  if Value <> FWeekendColor then
  begin
    FWeekendColor := Value;
    FillDaysArray;
    Invalidate;
  end;
end;

procedure TCustomRxCalendar.SetWeekends(Value: TDaysOfWeek);
begin
  if Value <> FWeekends then
  begin
    FWeekends := Value;
    UpdateCalendar;
  end;
end;

function TCustomRxCalendar.IsWeekend(ACol, ARow: Integer): Boolean;
begin
  Result := TDayOfWeekName((Integer(StartOfWeek) + ACol) mod 7) in FWeekends;
end;

procedure TCustomRxCalendar.SetStartOfWeek(Value: TDayOfWeekName);
begin
  if Value <> FStartOfWeek then
  begin
    FStartOfWeek := Value;
    UpdateCalendar;
  end;
end;

procedure TCustomRxCalendar.SetUseCurrentDate(Value: Boolean);
begin
  if Value <> FUseCurrentDate then
  begin
    FUseCurrentDate := Value;
    if Value then
    begin
      FDate := Date; { use the current date, then }
      UpdateCalendar;
    end;
  end;
end;

{ Given a value of 1 or -1, moves to Next or Prev month accordingly }
procedure TCustomRxCalendar.ChangeMonth(Delta: Integer);
var
  AYear, AMonth, ADay: Word;
  NewDate: TDateTime;
  CurDay: Integer;
begin
  DecodeDate(FDate, AYear, AMonth, ADay);
  CurDay := ADay;
  if Delta > 0 then ADay := DaysPerMonth(AYear, AMonth)
  else ADay := 1;
  NewDate := EncodeDate(AYear, AMonth, ADay);
  NewDate := NewDate + Delta;
  DecodeDate(NewDate, AYear, AMonth, ADay);
  if DaysPerMonth(AYear, AMonth) > CurDay then
    ADay := CurDay
  else
    ADay := DaysPerMonth(AYear, AMonth);
  CalendarDate := EncodeDate(AYear, AMonth, ADay);
end;

procedure TCustomRxCalendar.PrevMonth;
begin
  ChangeMonth(-1);
end;

procedure TCustomRxCalendar.NextMonth;
begin
  ChangeMonth(1);
end;

procedure TCustomRxCalendar.NextYear;
begin
  if IsLeapYear(Year) and (Month = 2) and (Day = 29) then Day := 28;
  Year := Year + 1;
end;

procedure TCustomRxCalendar.PrevYear;
begin
  if IsLeapYear(Year) and (Month = 2) and (Day = 29) then Day := 28;
  Year := Year - 1;
end;

procedure TCustomRxCalendar.CalendarUpdate(DayOnly: Boolean);
var
  AYear, AMonth, ADay: Word;
  FirstDate: TDateTime;
begin
  FUpdating := True;
  try
    DecodeDate(FDate, AYear, AMonth, ADay);
    FirstDate := EncodeDate(AYear, AMonth, 1);
    FMonthOffset := 2 - ((DayOfWeek(FirstDate) - Ord(StartOfWeek) + 7) mod 7);
      { day of week for 1st of month }
    if FMonthOffset = 2 then FMonthOffset := -5;

    FillDaysArray;
    MoveExtend(false, (ADay - FMonthOffset) mod 7, (ADay - FMonthOffset) div 7 + 1);
    TopRow:=1; //Правим ошибку для автоскрола календаря после 15 числа...
    VisualChange;

    if DayOnly then Update else Invalidate;
  finally
    FUpdating := False;
  end;
end;

procedure TCustomRxCalendar.UpdateCalendar;
begin
  CalendarUpdate(False);
end;

{ TLocCalendar }

type
  TLocCalendar = class(TCustomRxCalendar)
  private
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMParentColorChanged(var Message: TMessage); message CM_PARENTCOLORCHANGED;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure MouseToCell(X, Y: Integer; var ACol, ARow: Longint);
    property GridLineWidth;
    property DefaultColWidth;
    property DefaultRowHeight;
  end;

constructor TLocCalendar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csCaptureMouse, csClickEvents, csDoubleClicks];
  ControlStyle := ControlStyle + [csReplicatable];
//  Enabled := False;
  BorderStyle := bsNone;
  ParentColor := True;
  CalendarDate := Trunc(Now);
  UseCurrentDate := False;
  FixedColor := Color;
  Options := [goFixedHorzLine];
  TabStop := False;
end;

procedure TLocCalendar.CMParentColorChanged(var Message: TMessage);
begin
  inherited;
  if ParentColor then FixedColor := Self.Color;
end;

procedure TLocCalendar.CMEnabledChanged(var Message: TMessage);
begin
  if HandleAllocated and not (csDesigning in ComponentState) then
//    EnableWindow(Handle, True);
end;

procedure TLocCalendar.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    Style := Style and not (WS_BORDER or WS_TABSTOP or WS_DISABLED);
end;

procedure TLocCalendar.MouseToCell(X, Y: Integer; var ACol, ARow: Longint);
var
  Coord: TGridCoord;
begin
  Coord := MouseCoord(X, Y);
  ACol := Coord.X;
  ARow := Coord.Y;
end;

procedure TLocCalendar.DrawCell(ACol, ARow: Longint; ARect: TRect;
  AState: TGridDrawState);
var
  D, M, Y: Word;
begin
  inherited DrawCell(ACol, ARow, ARect, AState);
  
  if FDaysArray[ACol, ARow].DayDate = SysUtils.Date then
      rxFrame3D(Canvas, ARect, clBtnShadow, clBtnHighlight, 1);
end;


function CreatePopupCalendar(AOwner: TComponent
  {$IFDEF USED_BiDi}; ABiDiMode: TBiDiMode = bdLeftToRight {$ENDIF}): TPopupCalendar;
begin
  Result := TPopupCalendar.Create(AOwner);

  if (AOwner <> nil) and not (csDesigning in AOwner.ComponentState) and
    (Screen.PixelsPerInch <> 96) then
  begin { scale to screen res }
//    Result.ScaleBy(Screen.PixelsPerInch, 96);
    { The ScaleBy method does not scale the font well, so set the
      font back to the original info. }
    TPopupCalendar(Result).FCalendar.ParentFont := True;
    FontSetDefault(TPopupCalendar(Result).Font);
{$IFDEF USED_BiDi}
    Result.BiDiMode := ABiDiMode;
{$ENDIF}
  end;
end;

procedure SetupPopupCalendar(PopupCalendar: TWinControl;
  AStartOfWeek: TDayOfWeekName; AWeekends: TDaysOfWeek;
  AWeekendColor: TColor; BtnHints: TStrings; FourDigitYear: Boolean);
var
  I: Integer;
begin
  if (PopupCalendar = nil) or not (PopupCalendar is TPopupCalendar) then
    Exit;
  TPopupCalendar(PopupCalendar).FFourDigitYear := FourDigitYear;
  if TPopupCalendar(PopupCalendar).FCalendar <> nil then
  begin
    with TPopupCalendar(PopupCalendar).FCalendar do
    begin
      StartOfWeek := AStartOfWeek;
      WeekendColor := AWeekendColor;
      Weekends := AWeekends;
    end;
    if (BtnHints <> nil) then
      for I := 0 to Min(BtnHints.Count - 1, 3) do
      begin
        if BtnHints[I] <> '' then
          TPopupCalendar(PopupCalendar).FBtns[I].Hint := BtnHints[I];
      end;
  end;
end;

constructor TPopupCalendar.Create(AOwner: TComponent);
const
  BtnSide = 14;
var
  BackPanel: TWinControl;
  MI:TMenuItem;
  i:integer;
  TmpBitmap:TBitmap;
begin
  inherited CreateNew(AOwner);

  BorderStyle:=bsNone;

  FFourDigitYear := FourDigitYear;
  Height := Max(PopupCalendarSize.Y, 120);
  Width := Max(PopupCalendarSize.X, 180);
  Color := clBtnFace;
  FontSetDefault(Font);
  KeyPreview:=true;

  if AOwner is TControl then ShowHint := TControl(AOwner).ShowHint
  else ShowHint := True;
  
//  if (csDesigning in ComponentState) then Exit;

  FMonthNames := TStringList.Create;
  if FMonthNames.Count = 0 then
  begin
    for i := Low(DefaultFormatSettings.LongMonthNames) to High(DefaultFormatSettings.LongMonthNames) do
      FMonthNames.Add(DefaultFormatSettings.LongMonthNames[i]);
  end;

  BackPanel := TPanel.Create(Self);
  BackPanel.Anchors:=[akLeft, akRight, akTop, akBottom];

  with BackPanel as TPanel do
  begin
    Parent := Self;
//    Align := alClient;
    ParentColor := True;
    ControlStyle := ControlStyle + [csReplicatable];
  end;

  FControlPanel := TPanel.Create(Self);
  with FControlPanel do
  begin
    Parent := BackPanel;
    Align := alTop;
    Width := Self.Width - 4;
    Height := 18;
    BevelOuter := bvNone;
    ParentColor := True;
    ControlStyle := ControlStyle + [csReplicatable];
    Color:=clSkyBlue;
  end;

  FCalendar := TLocCalendar.Create(Self);
  with TLocCalendar(FCalendar) do
  begin
    Parent := BackPanel;
    Align := alClient;
    OnChange := @CalendarChange;
    OnMouseUp := @CalendarMouseUp;
    OnDblClick := @CalendarDblClick;
  end;

  FCloseBtn:=TBitBtn.Create(Self);
  FCloseBtn.Parent := BackPanel;
  FCloseBtn.Kind:=bkCancel;
  FCloseBtn.Align:=alBottom;
  FCloseBtn.AutoSize:=true;

  BackPanel.Top:=2;
  BackPanel.Left:=2;
  BackPanel.Width:=Width - 4;

  BackPanel.Height:=Height - 4;

  FBtns[0] := TRxTimerSpeedButton.Create(Self);
  with FBtns[0] do
  begin
    Parent := FControlPanel;
    SetBounds(-1, -1, BtnSide, BtnSide);
    //loaded bitmap should be freed as Glyph just takes a copy of it
    //TmpBitmap:=LoadBitmapFromLazarusResource('prev2');
    TmpBitmap:=CreateResBitmap('rx_prev2');
    Glyph := TmpBitmap;
    FreeAndNil(TmpBitmap);

    OnClick := @PrevYearBtnClick;
    Hint := sPrevYear;
    Align:=alLeft;
  end;

  FBtns[1] := TRxTimerSpeedButton.Create(Self);
  with FBtns[1] do
  begin
    Parent := FControlPanel;
    SetBounds(BtnSide - 2, -1, BtnSide, BtnSide);

    //TmpBitmap:=LoadBitmapFromLazarusResource('prev1');
    TmpBitmap:=CreateResBitmap('rx_prev1');
    Glyph := TmpBitmap;
    FreeAndNil(TmpBitmap);

    OnClick := @PrevMonthBtnClick;
    Hint := sPrevMonth;
    Align:=alLeft;
  end;

  FBtns[2] := TRxTimerSpeedButton.Create(Self);
  with FBtns[2] do
  begin
    Parent := FControlPanel;
    SetBounds(FControlPanel.Width - 2 * BtnSide + 2, -1, BtnSide, BtnSide);
    //TmpBitmap:=LoadBitmapFromLazarusResource('next1');
    TmpBitmap:=CreateResBitmap('rx_next1');
    Glyph := TmpBitmap;
    FreeAndNil(TmpBitmap);
    OnClick := @NextMonthBtnClick;
    Hint := sNextMonth;
    Align:=alRight;
  end;

  FBtns[3] := TRxTimerSpeedButton.Create(Self);
  with FBtns[3] do
  begin
    Parent := FControlPanel;
    SetBounds(FControlPanel.Width - BtnSide + 1, -1, BtnSide, BtnSide);
    //TmpBitmap:=LoadBitmapFromLazarusResource('next2');
    TmpBitmap:=CreateResBitmap('rx_next2');
    Glyph := TmpBitmap;
    FreeAndNil(TmpBitmap);
    OnClick := @NextYearBtnClick;
    Hint := sNextYear;
    Align:=alRight;
  end;

  FTitleLabel := TLabel.Create(Self);
  with FTitleLabel do
  begin
    Parent := FControlPanel;
    AutoSize := False;
    Alignment := taCenter;
    SetBounds(BtnSide * 2 + 1, 1, FControlPanel.Width - 4 * BtnSide - 2, 14);
    Transparent := True;
    OnDblClick := @TopPanelDblClick;
    ControlStyle := ControlStyle + [csReplicatable];
    Align:=alClient;
  end;

  FMonthMenu:=CreateRxCalendarPopupMenu(Self, @MonthMenuClick);

  FTitleLabel.PopupMenu:=FMonthMenu;
  ActiveControl:=FCalendar;
  CalendarChange(nil);
end;

destructor TPopupCalendar.Destroy;
begin
  FMonthNames.Free;
  inherited Destroy;
end;

procedure TPopupCalendar.AutoSizeForm;
begin
  FControlPanel.Height:=FCalendar.Canvas.TextHeight('Wg')+4;
  Height:=(FCalendar.Canvas.TextHeight('Wg')+4)*7+FControlPanel.Height + FCloseBtn.Height;
  Width:=FCalendar.Canvas.TextWidth(' WWW')*7;
  FCalendar.AutoFillColumns:=true;
end;

procedure TPopupCalendar.CalendarMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Col, Row: Longint;
begin
  if (Button = mbLeft) and (Shift = []) then
  begin
    TLocCalendar(FCalendar).MouseToCell(X, Y, Col, Row);
    if (Row > 0) and (FCalendar.FDaysArray[Col, Row].DayColor <> FCalendar.FNotInThisMonthColor) then
      ModalResult:=mrOk;
  end;
end;

function TPopupCalendar.GetDate: TDateTime;
begin
  Result:=FCalendar.CalendarDate;
end;

procedure TPopupCalendar.TopPanelDblClick(Sender: TObject);
begin
  FCalendar.CalendarDate := Trunc(Now);
end;

procedure TPopupCalendar.MonthMenuClick(Sender: TObject);
var
  Cmd:integer;
begin
  Cmd:=(Sender as TComponent).Tag;
  if Cmd = -1 then
    FCalendar.SetCalendarDate(Sysutils.Date)
  else
    FCalendar.Month:=Cmd;
end;

procedure TPopupCalendar.CalendarDblClick(Sender: TObject);
begin
  ModalResult:=mrOk;
end;

procedure TPopupCalendar.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if FCalendar <> nil then
    case Key of
      VK_NEXT:
        begin
          if ssCtrl in Shift then FCalendar.NextYear;
        end;
      VK_PRIOR:
        begin
          if ssCtrl in Shift then FCalendar.PrevYear;
        end;
      VK_ESCAPE:ModalResult:=mrCancel;
    end;
  inherited KeyDown(Key, Shift);
end;

procedure TPopupCalendar.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if (FCalendar <> nil) and (Key <> #0) then
    FCalendar.KeyPress(Key);
end;

procedure TPopupCalendar.Paint;
var
  CR:TRect;
begin
  inherited Paint;
  
    CR:=ClientRect;
    RxFrame3D(Canvas, CR, clBtnHighlight, clWindowFrame, 1);
    RxFrame3D(Canvas, CR, clBtnFace, clBtnShadow, 1);
end;

procedure TPopupCalendar.Deactivate;
begin
  inherited Deactivate;
{  if Assigned(FOnPopUpCloseEvent) then
    FOnPopUpCloseEvent(FFindResult);}
//  Close;
end;

procedure TPopupCalendar.PrevYearBtnClick(Sender: TObject);
begin
  FCalendar.PrevYear;
  FCalendar.SetFocus;
end;

procedure TPopupCalendar.NextYearBtnClick(Sender: TObject);
begin
  FCalendar.NextYear;
  FCalendar.SetFocus;
end;

procedure TPopupCalendar.PrevMonthBtnClick(Sender: TObject);
begin
  FCalendar.PrevMonth;
  FCalendar.SetFocus;
end;

procedure TPopupCalendar.NextMonthBtnClick(Sender: TObject);
begin
  FCalendar.NextMonth;
  FCalendar.SetFocus;
end;

procedure TPopupCalendar.CalendarChange(Sender: TObject);
var
  AYear, AMonth, ADay: Word;
begin
  DecodeDate(FCalendar.CalendarDate, AYear, AMonth, ADay);
  FTitleLabel.Caption := Format('%s, %d', [DefaultFormatSettings.LongMonthNames[AMonth], AYear]);
end;

procedure TPopupCalendar.SetDate(const AValue: TDateTime);
begin
  FCalendar.CalendarDate:=AValue;
end;

procedure TPopupCalendar.SetMonthNames(const AValue: TStrings);
begin
  if AValue.Text <> FMonthNames.Text then
  begin
    FMonthNames.Assign(AValue);
    CalendarChange(Self);
  end;
end;

  { TSelectDateDlg }
  
constructor TSelectDateDlg.Create(AOwner: TComponent);
var
  Control: TWinControl;
  MI:TMenuItem;
  i:integer;
  TmpBitmap:TBitmap;
begin
  inherited CreateNew(AOwner, 0);
  Caption := sDateDlgTitle;

  BorderStyle := bsToolWindow;

  BorderIcons := [biSystemMenu];
  ClientHeight := 154;
  ClientWidth := 222;
  FontSetDefault(Font);
  Color := clBtnFace;
  Position := poScreenCenter;
  ShowHint := True;
  KeyPreview := True;

  Control := TPanel.Create(Self);
  with Control as TPanel do
  begin
    Parent := Self;
    SetBounds(0, 0, 222, 22);
    Align := alTop;
    BevelInner := bvLowered;
    ParentColor := True;
    ParentFont := True;
  end;

  TitleLabel := TLabel.Create(Self);
  with TitleLabel do
  begin
    Parent := Control;
    SetBounds(35, 4, 152, 14);
    Alignment := taCenter;
    AutoSize := False;
    Caption := '';
    ParentFont := True;
    Font.Color := clBlue;
    Font.Style := [fsBold];
    Transparent := True;
    OnDblClick := @TopPanelDblClick;
  end;

  FBtns[0] := TRxTimerSpeedButton.Create(Self);
  with FBtns[0] do
  begin
    Parent := Control;
    SetBounds(3, 3, 16, 16);

    TmpBitmap:=LoadBitmapFromLazarusResource('prev2');
    Glyph := TmpBitmap;
    FreeAndNil(TmpBitmap);

    OnClick := @PrevYearBtnClick;
    Hint := sPrevYear;
  end;

  FBtns[1] := TRxTimerSpeedButton.Create(Self);
  with FBtns[1] do begin
    Parent := Control;
    SetBounds(18, 3, 16, 16);

    TmpBitmap:=LoadBitmapFromLazarusResource('prev1');
    Glyph := TmpBitmap;
    FreeAndNil(TmpBitmap);

    OnClick := @PrevMonthBtnClick;
    Hint := sPrevMonth;
  end;

  FBtns[2] := TRxTimerSpeedButton.Create(Self);
  with FBtns[2] do
  begin
    Parent := Control;
    SetBounds(188, 3, 16, 16);

    TmpBitmap:=LoadBitmapFromLazarusResource('next1');
    Glyph := TmpBitmap;
    FreeAndNil(TmpBitmap);

    OnClick := @NextMonthBtnClick;
    Hint := sNextMonth;
  end;

  FBtns[3] := TRxTimerSpeedButton.Create(Self);
  with FBtns[3] do begin
    Parent := Control;
    SetBounds(203, 3, 16, 16);

    TmpBitmap:=LoadBitmapFromLazarusResource('next2');
    Glyph := TmpBitmap;
    FreeAndNil(TmpBitmap);

    OnClick := @NextYearBtnClick;
    Hint := sNextYear;
  end;

  Control := TPanel.Create(Self);
  with Control as TPanel do
  begin
    Parent := Self;
    SetBounds(0, 133, 222, 21);
    Align := alBottom;
    BevelInner := bvNone;
    BevelOuter := bvNone;
    ParentFont := True;
    ParentColor := True;
  end;

  with TButton.Create(Self) do
  begin
    Parent := Control;
    SetBounds(0, 0, 112, 21);
    Caption := rsmbOK;
    ModalResult := mrOk;
  end;

  with TButton.Create(Self) do
  begin
    Parent := Control;
    SetBounds(111, 0, 111, 21);
    Caption := rsmbCancel;
    ModalResult := mrCancel;
    Cancel := True;
  end;

  Calendar := TCustomRxCalendar.Create(Self);
  with Calendar do
  begin
    Parent := Self;
    Align := alClient;
    ParentFont := True;
    SetBounds(2, 2, 218, 113);
    Color := clWhite;
    TabOrder := 0;
    UseCurrentDate := False;
    OnChange := @CalendarChange;
    OnDblClick := @CalendarDblClick;
  end;

  OnKeyDown := @FormKeyDown;
  Calendar.CalendarDate := Trunc(Now);
  ActiveControl := Calendar;
  
  FMonthMenu:=CreateRxCalendarPopupMenu(Self, @MonthMenuClick);

  TitleLabel.PopupMenu:=FMonthMenu;
end;

procedure TSelectDateDlg.SetDate(Date: TDateTime);
begin
  if Date = NullDate then Date := SysUtils.Date;
  try
    Calendar.CalendarDate := Date;
    CalendarChange(nil);
  except
    Calendar.CalendarDate := SysUtils.Date;
  end;
end;

function TSelectDateDlg.GetDate: TDateTime;
begin
  Result := Calendar.CalendarDate;
end;

procedure TSelectDateDlg.TopPanelDblClick(Sender: TObject);
begin
  SetDate(Trunc(Now));
end;

procedure TSelectDateDlg.PrevYearBtnClick(Sender: TObject);
begin
  Calendar.PrevYear;
end;

procedure TSelectDateDlg.NextYearBtnClick(Sender: TObject);
begin
  Calendar.NextYear;
end;

procedure TSelectDateDlg.PrevMonthBtnClick(Sender: TObject);
begin
  Calendar.PrevMonth;
end;

procedure TSelectDateDlg.NextMonthBtnClick(Sender: TObject);
begin
  Calendar.NextMonth;
end;

procedure TSelectDateDlg.CalendarChange(Sender: TObject);
begin
  TitleLabel.Caption := FormatDateTime('MMMM, YYYY', Calendar.CalendarDate);
end;

procedure TSelectDateDlg.CalendarDblClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TSelectDateDlg.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_RETURN: ModalResult := mrOK;
    VK_ESCAPE: ModalResult := mrCancel;
    VK_NEXT:
      begin
        if ssCtrl in Shift then Calendar.NextYear;
        //else Calendar.NextMonth;
        TitleLabel.Update;
      end;
    VK_PRIOR:
      begin
        if ssCtrl in Shift then Calendar.PrevYear;
        //else Calendar.PrevMonth;
        TitleLabel.Update;
      end;
    VK_TAB:
      begin
        if Shift = [ssShift] then Calendar.PrevMonth
        else Calendar.NextMonth;
        TitleLabel.Update;
      end;
  end; {case}
end;

procedure TSelectDateDlg.MonthMenuClick(Sender: TObject);
var
  Cmd:integer;
begin
  Cmd:=(Sender as TComponent).Tag;
  if Cmd = -1 then
    Calendar.SetCalendarDate(Sysutils.Date)
  else
    Calendar.Month:=Cmd;
end;

{ SelectDate routines }

function CreateDateDialog(const DlgCaption: TCaption): TSelectDateDlg;
begin
  Result := TSelectDateDlg.Create(Application);
  try
    if DlgCaption <> '' then Result.Caption := DlgCaption;
{    if Screen.PixelsPerInch <> 96  then begin { scale to screen res }
//      Result.ScaleBy(Screen.PixelsPerInch, 96);
      { The ScaleBy method does not scale the font well, so set the
        font back to the original info. }
      Result.Calendar.ParentFont := True;
      FontSetDefault(Result.Font);
      Result.Left := (Screen.Width div 2) - (Result.Width div 2);
      Result.Top := (Screen.Height div 2) - (Result.Height div 2);
    end;}
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function PopupDate(var Date: TDateTime; Edit: TWinControl): Boolean;
var
  D: TSelectDateDlg;
  P: TPoint;
  W, H, X, Y: Integer;
begin
  Result := False;
  D := CreateDateDialog('');
  try
    D.BorderIcons := [];
    D.HandleNeeded;
    D.Position := poDesigned;
    W := D.Width;
    H := D.Height;
    P := (Edit.ClientOrigin);
    Y := P.Y + Edit.Height - 1;
    if (Y + H) > Screen.Height then Y := P.Y - H + 1;
    if Y < 0 then Y := P.Y + Edit.Height - 1;
    X := (P.X + Edit.Width) - W;
    if X < 0 then X := P.X;
    D.Left := X;
    D.Top := Y;
    D.Date := Date;
    
    if D.ShowModal = mrOk then
    begin
      Date := D.Date;
      Result := True;
    end;
  finally
    D.Free;
  end;
end;

function SelectDate(var Date: TDateTime; const DlgCaption: TCaption;
  AStartOfWeek: TDayOfWeekName; AWeekends: TDaysOfWeek;
  AWeekendColor: TColor; BtnHints: TStrings): Boolean;
var
  D: TSelectDateDlg;
  I: Integer;
begin
  Result := False;
  D := CreateDateDialog(DlgCaption);
  try
    D.Date := Date;
    with D.Calendar do begin
      StartOfWeek := AStartOfWeek;
      Weekends := AWeekends;
      WeekendColor := AWeekendColor;
    end;
    if (BtnHints <> nil) then
      for I := 0 to Min(BtnHints.Count - 1, 3) do begin
        if BtnHints[I] <> '' then
          D.FBtns[I].Hint := BtnHints[I];
      end;
    if D.ShowModal = mrOk then begin
      Date := D.Date;
      Result := True;
    end;
  finally
    D.Free;
  end;
end;

function SelectDateStr(var StrDate: string; const DlgCaption: TCaption;
  AStartOfWeek: TDayOfWeekName; AWeekends: TDaysOfWeek;
  AWeekendColor: TColor; BtnHints: TStrings): Boolean;
var
  DateValue: TDateTime;
begin
  if StrDate <> '' then begin
    try
      DateValue := StrToDateFmt(DefaultFormatSettings.ShortDateFormat, StrDate);
    except
      DateValue := Date;
    end;
  end
  else DateValue := Date;
  Result := SelectDate(DateValue, DlgCaption, AStartOfWeek, AWeekends,
    AWeekendColor, BtnHints);
  if Result then StrDate := FormatDateTime(DefaultFormatSettings.ShortDateFormat, DateValue);
end;

{ TRxCalendarGrid }

procedure TRxCalendarGrid.SetBounds(aLeft, aTop, aWidth, aHeight: integer);
var
  GridLinesH, GridLinesW: Integer;
begin
  inherited SetBounds(aLeft, aTop, aWidth, aHeight);

  GridLinesH := 6 * GridLineWidth;
  if (goVertLine in Options) or (goFixedVertLine in Options) then
    GridLinesW := 6 * GridLineWidth
  else GridLinesW := 0;
  DefaultColWidth := (aWidth - GridLinesW) div 7;
  DefaultRowHeight := (aHeight - GridLinesH) div 7;
end;

end.
