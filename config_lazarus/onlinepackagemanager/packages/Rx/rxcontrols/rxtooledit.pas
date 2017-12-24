{ tooledit unit

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

unit rxtooledit;

{$I rx.inc}

interface

uses
  Classes, SysUtils, LCLType, LMessages, Graphics, MaskEdit, Controls, EditBtn,
  rxpickdate, rxdateutil;

type
  { TCustomDateEdit }

  TYearDigits = (dyDefault, dyFour, dyTwo);
  TPopupAlign = (epaRight, epaLeft);
  TCalendarStyle = (csPopup, csDialog);

const
{$IFDEF DEFAULT_POPUP_CALENDAR}
  dcsDefault = csPopup;
{$ELSE}
  dcsDefault = csDialog;
{$ENDIF DEFAULT_POPUP_CALENDAR}

type

  { TCustomRxDateEdit }

  TCustomRxDateEdit = class(TCustomEditButton)
  private
    FCalendarHints: TStrings;
    FBlanksChar: Char;
    FCancelCaption: TCaption;
    FDefaultToday: Boolean;
    FDialogTitle: TCaption;
    FPopupColor: TColor;
    FNotInThisMonthColor:TColor;
    FOKCaption: TCaption;
    FOnAcceptDAte: TAcceptDateEvent;
    FStartOfWeek: TDayOfWeekName;
    FWeekendColor: TColor;
    FWeekends: TDaysOfWeek;
    FYearDigits: TYearDigits;
    FDateFormat: string[10];
    FFormatting: Boolean;
    FPopupVisible: Boolean;
    FPopupAlign: TPopupAlign;
    FCalendarStyle: TCalendarStyle;
    //function GetCalendarStyle: TCalendarStyle;
    function GetDate: TDateTime;
    function GetPopupColor: TColor;
    function GetPopupVisible: Boolean;
    function GetValidDate: boolean;
    function IsStoreTitle: boolean;
    procedure SetBlanksChar(const AValue: Char);
    procedure SetCalendarStyle(const AValue: TCalendarStyle);
    procedure SetDate(const AValue: TDateTime);
    procedure SetPopupColor(const AValue: TColor);
    procedure SetStartOfWeek(const AValue: TDayOfWeekName);
    procedure SetWeekendColor(const AValue: TColor);
    procedure SetWeekends(const AValue: TDaysOfWeek);
    procedure SetYearDigits(const AValue: TYearDigits);
    procedure CalendarHintsChanged(Sender: TObject);

    function AcceptPopup(var Value: TDateTime): Boolean;
    procedure AcceptValue(const AValue: TDateTime);
//    procedure SetPopupValue(const Value: Variant);
  protected
    FPopup: TPopupCalendar;
    procedure UpdateFormat;
    procedure UpdatePopup;
    function TextStored: Boolean;
    procedure PopupDropDown(DisableEdit: Boolean); virtual;
    procedure PopupCloseUp(Sender: TObject; Accept: Boolean);
    procedure HidePopup; virtual;
    procedure ShowPopup(AOrigin: TPoint); virtual;
    procedure ApplyDate(Value: TDateTime); virtual;
    procedure EditChange; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;

    procedure ButtonClick; override;
    procedure EditKeyDown(var Key: word; Shift: TShiftState); override;
    procedure EditKeyPress( var Key: char); override;

    function GetDefaultGlyphName: String; override;
    function CreatePopupForm:TPopupCalendar;
    procedure DoEnter; override;

    property BlanksChar: Char read FBlanksChar write SetBlanksChar default ' ';
    property DialogTitle:TCaption Read FDialogTitle Write FDialogTitle Stored IsStoreTitle;
    Property OnAcceptDate : TAcceptDateEvent Read FOnAcceptDAte Write FOnAcceptDate;
    property OKCaption:TCaption Read FOKCaption Write FOKCaption;
    property CancelCaption:TCaption Read FCancelCaption Write FCancelCaption;
    property DefaultToday: Boolean read FDefaultToday write FDefaultToday
      default False;
    property StartOfWeek: TDayOfWeekName read FStartOfWeek write SetStartOfWeek default Mon;
    property Weekends: TDaysOfWeek read FWeekends write SetWeekends default [Sun];
    property WeekendColor: TColor read FWeekendColor write SetWeekendColor default clRed;
    property YearDigits: TYearDigits read FYearDigits write SetYearDigits default dyDefault;
    property PopupColor: TColor read GetPopupColor write SetPopupColor
      default clBtnFace;
    property CalendarStyle: TCalendarStyle read FCalendarStyle//GetCalendarStyle
      write SetCalendarStyle default dcsDefault;
    property PopupVisible: Boolean read GetPopupVisible;
    property PopupAlign: TPopupAlign read FPopupAlign write FPopupAlign default epaLeft;
    property NotInThisMonthColor:TColor read FNotInThisMonthColor write FNotInThisMonthColor default clSilver;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CheckValidDate;
    function GetDateMask: string;
    procedure UpdateMask; virtual;
    property Date: TDateTime read GetDate write SetDate;
    property Formatting: Boolean read FFormatting;
    property ValidDate:boolean read GetValidDate;
  end;
  
type

  { TRxDateEdit }

  TRxDateEdit = class(TCustomRxDateEdit)
  public
    constructor Create(AOwner: TComponent); override;
    property PopupVisible;
  published
    property Action;
    property Align;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BlanksChar;
    property BorderSpacing;
    property ButtonOnlyWhenFocused;
    property ButtonWidth;
    property CalendarStyle;
    property CancelCaption;
    property CharCase;
    property Color;
    property Constraints;
    property DefaultToday;
    property DialogTitle;
    property DirectInput;
    property DragMode;
    property EchoMode;
    property Enabled;
    property Font;
    property Glyph;
    property MaxLength;
    property NotInThisMonthColor;
    property NumGlyphs;
    property OKCaption;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupAlign;
    property PopupColor;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property StartOfWeek;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property WeekendColor;
    property Weekends;
    property YearDigits;
    property Spacing default 0;

    property OnAcceptDate;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnEditingDone;
    property OnEnter;
    property OnExit;
    Property OnKeyDown;
    property OnKeyPress;
    Property OnKeyUp;
    Property OnMouseDown;
    Property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
  end;
  

function PaintComboEdit(Editor: TCustomMaskEdit; const AText: string;
  AAlignment: TAlignment; StandardPaint: Boolean;
  var ACanvas: TControlCanvas; var Message: TLMPaint): Boolean;
function EditorTextMargins(Editor: TCustomMaskEdit): TPoint;

implementation
uses lclintf, LCLStrConsts, rxconst, rxstrutils, LResources,
  Forms, LCLProc,
  variants;

{.$IFNDEF RX_USE_LAZARUS_RESOURCE}
{.$R tooledit.res}
{.$ENDIF}

type
  TPopupCalendarAccess = class(TPopupCalendar)
  end;

function EditorTextMargins(Editor: TCustomMaskEdit): TPoint;
var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
  with Editor do
  begin
(*    if NewStyleControls then
    begin
      if BorderStyle = bsNone then
        I := 0
      else
{      if Ctl3D then
        I := 1
      else}
        I := 2;
      Result.X := {SendMessage(Handle, LM_GETMARGINS, 0, 0) and $0000FFFF} + I;
      Result.Y := I;
    end
    else *)
    begin
      if BorderStyle = bsNone then
        I := 0
      else
      begin
        DC := GetDC(0);
        GetTextMetrics(DC, SysMetrics);
        SaveFont := SelectObject(DC, Font.Handle);
        GetTextMetrics(DC, Metrics);
        SelectObject(DC, SaveFont);
        ReleaseDC(0, DC);
        I := SysMetrics.tmHeight;
        if I > Metrics.tmHeight then
          I := Metrics.tmHeight;
        I := I div 4;
      end;
      Result.X := I;
      Result.Y := I div 4;
    end;
  end;
end;

function PaintComboEdit(Editor: TCustomMaskEdit; const AText: string;
  AAlignment: TAlignment; StandardPaint: Boolean;
  var ACanvas: TControlCanvas; var Message: TLMPaint): Boolean;
var
  AWidth, ALeft: Integer;
  Margins: TPoint;
  R: TRect;
  DC: HDC;
  PS: TPaintStruct;
  S: string;
{$IFDEF USED_BiDi}
  ExStyle: DWORD;
const
  AlignStyle: array[Boolean, TAlignment] of DWORD =
   ((WS_EX_LEFT, WS_EX_RIGHT, WS_EX_LEFT),
    (WS_EX_RIGHT, WS_EX_LEFT, WS_EX_LEFT));
{$ENDIF}
begin
  Result := True;
  with Editor do
  begin
{$IFDEF USED_BiDi}
    if UseRightToLeftAlignment then ChangeBiDiModeAlignment(AAlignment);
{$ENDIF}
    if StandardPaint and not(csPaintCopy in ControlState) then
    begin
{$IFDEF USED_BiDi}
      if SysLocale.MiddleEast and HandleAllocated and (IsRightToLeft) then
      begin { This keeps the right aligned text, right aligned }
        ExStyle := DWORD(GetWindowLong(Handle, GWL_EXSTYLE)) and (not WS_EX_RIGHT) and
          (not WS_EX_RTLREADING) and (not WS_EX_LEFTSCROLLBAR);
        if UseRightToLeftReading then
          ExStyle := ExStyle or WS_EX_RTLREADING;
        if UseRightToLeftScrollbar then
          ExStyle := ExStyle or WS_EX_LEFTSCROLLBAR;
        ExStyle := ExStyle or
          AlignStyle[UseRightToLeftAlignment, AAlignment];
        if DWORD(GetWindowLong(Handle, GWL_EXSTYLE)) <> ExStyle then
          SetWindowLong(Handle, GWL_EXSTYLE, ExStyle);
      end;
{$ENDIF USED_BiDi}
      Result := False;
      { return false if we need to use standard paint handler }
      Exit;
    end;
    { Since edit controls do not handle justification unless multi-line (and
      then only poorly) we will draw right and center justify manually unless
      the edit has the focus. }
    if ACanvas = nil then
    begin
      ACanvas := TControlCanvas.Create;
      ACanvas.Control := Editor;
    end;

    DC := Message.DC;
    if DC = 0 then DC := BeginPaint(Handle, PS);
    ACanvas.Handle := DC;

    try
      ACanvas.Font := Font;
      if not Enabled and NewStyleControls and not
        (csDesigning in ComponentState) and
        (ColorToRGB(Color) <> ColorToRGB(clGrayText)) then
        ACanvas.Font.Color := clGrayText;
      with ACanvas do
      begin
        R := ClientRect;
        Brush.Color := Color;
        S := AText;
        AWidth := TextWidth(S);
        Margins := EditorTextMargins(Editor);
        case AAlignment of
          taLeftJustify: ALeft := Margins.X;
          taRightJustify: ALeft := ClientWidth - AWidth - Margins.X - 2;
        else
          ALeft := (ClientWidth - AWidth) div 2;
        end;
{$IFDEF USED_BiDi}
        if SysLocale.MiddleEast then UpdateTextFlags;
{$ENDIF}
        Brush.Style := bsClear;
        TextRect(R, ALeft, Margins.Y, S);
      end;
    finally
      ACanvas.Handle := 0;
      if Message.DC = 0 then EndPaint(Handle, PS);
    end;
  end;
end;

{ TRxDateEdit }

constructor TRxDateEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Spacing:=0;
  UpdateMask;
end;

{ TCustomRxDateEdit }

function TCustomRxDateEdit.IsStoreTitle: boolean;
begin
  Result:=DialogTitle<>rsPickDate;
end;

procedure TCustomRxDateEdit.SetBlanksChar(const AValue: Char);
begin
  if FBlanksChar=AValue then exit;
  if (AValue < ' ') then
    FBlanksChar:=' '
  else
    FBlanksChar:=AValue;
  UpdateMask;
end;

{
function TCustomRxDateEdit.GetCalendarStyle: TCalendarStyle;
begin
  if FPopup <> nil then
    Result := csPopup
  else
    Result := csDialog;
end;
}
function TCustomRxDateEdit.GetDate: TDateTime;
begin
  if DefaultToday then Result := SysUtils.Date
  else Result := NullDate;
  if Text<>'' then
    Result := StrToDateFmtDef(FDateFormat, Text, Result);
end;

function TCustomRxDateEdit.GetPopupColor: TColor;
begin
  if FPopup <> nil then Result := TPopupCalendar(FPopup).Color
  else Result := FPopupColor;
end;

function TCustomRxDateEdit.GetPopupVisible: Boolean;
begin
  Result := (FPopup <> nil) and FPopupVisible;
end;

function TCustomRxDateEdit.GetValidDate: boolean;
begin
  try
    StrToDateFmt(FDateFormat, Text);
    Result:=true;
  except
    Result:=false;
  end;
end;

procedure TCustomRxDateEdit.SetCalendarStyle(const AValue: TCalendarStyle);
begin
  if AValue <> FCalendarStyle then
  begin
    FCalendarStyle:=AValue;
(*    case AValue of
      csPopup:
        begin
          if FPopup = nil then
          begin
            FPopup := CreatePopupCalendar(Self{$IFDEF USED_BiDi}, BiDiMode {$ENDIF});
          end;
          FPopup.OnCloseUp := @PopupCloseUp;
          FPopup.Color := FPopupColor;
          TRxCalendarGrid(FPopup.Calendar).NotInThisMonthColor:=FNotInThisMonthColor;
        end;
      csDialog:
        begin
          FPopup.Free;
          FPopup := nil;
        end;
    end;*)
  end;
end;

procedure TCustomRxDateEdit.SetDate(const AValue: TDateTime);
var
  D: TDateTime;
begin
  D := Date;
  if AValue = NullDate then
    Text := ''
  else
    Text := FormatDateTime(FDateFormat, AValue);
  Modified := D <> Date;
end;

procedure TCustomRxDateEdit.SetPopupColor(const AValue: TColor);
begin
  if AValue <> FPopupColor then
  begin
    if FPopup <> nil then FPopup.Color := AValue;
    FPopupColor := AValue;
    UpdatePopup;
  end;
end;

procedure TCustomRxDateEdit.SetStartOfWeek(const AValue: TDayOfWeekName);
begin
  if FStartOfWeek=AValue then exit;
  FStartOfWeek:=AValue;
  UpdatePopup;
  UpdateMask;
end;

procedure TCustomRxDateEdit.SetWeekendColor(const AValue: TColor);
begin
  if FWeekendColor=AValue then exit;
  FWeekendColor:=AValue;
  UpdatePopup;
end;

procedure TCustomRxDateEdit.SetWeekends(const AValue: TDaysOfWeek);
begin
  if FWeekends=AValue then exit;
  FWeekends:=AValue;
  UpdatePopup;
end;

procedure TCustomRxDateEdit.SetYearDigits(const AValue: TYearDigits);
begin
  if FYearDigits=AValue then exit;
  FYearDigits:=AValue;
//  UpdateFormat;
  UpdateMask;
end;

procedure TCustomRxDateEdit.CalendarHintsChanged(Sender: TObject);
begin
  TStringList(FCalendarHints).OnChange := nil;
  try
    while (FCalendarHints.Count > 4) do
      FCalendarHints.Delete(FCalendarHints.Count - 1);
  finally
    TStringList(FCalendarHints).OnChange := @CalendarHintsChanged;
  end;
  if not (csDesigning in ComponentState) then UpdatePopup;
end;

function TCustomRxDateEdit.AcceptPopup(var Value: TDateTime): Boolean;
var
  D: TDateTime;
begin
  Result := True;
  if Assigned(FOnAcceptDate) then
  begin
    D :=Value;
    FOnAcceptDate(Self, D, Result);
    if Result then
      Value := D;
  end;
end;

procedure TCustomRxDateEdit.AcceptValue(const AValue: TDateTime);
begin
  SetDate(AValue);
  if Modified then
    inherited EditChange;
end;

procedure TCustomRxDateEdit.UpdateFormat;
begin
  case YearDigits of
    dyDefault:FDateFormat :=DefDateFormat(FourDigitYear);
    dyFour:FDateFormat := DefDateFormat(true);
    dyTwo:FDateFormat := DefDateFormat(false);//DefDateMask(FBlanksChar, false);
  end;
end;

procedure TCustomRxDateEdit.UpdatePopup;
begin
  if FPopup <> nil then SetupPopupCalendar(FPopup, FStartOfWeek,
    FWeekends, FWeekendColor, FCalendarHints, FourDigitYear);
end;

function TCustomRxDateEdit.TextStored: Boolean;
begin
  Result := not IsEmptyStr(Text, [#0, ' ', DateSeparator, FBlanksChar]);
end;

procedure TCustomRxDateEdit.PopupDropDown(DisableEdit: Boolean);
var
  P: TPoint;
  ABounds:TRect;
  Y: Integer;

procedure DoTrySetDate;
var
  D:TDateTime;
begin
  if Text<>'' then
  begin
    try
      D:=StrToDate(Text);
      FPopup.Date:=D;
    except
      if FDefaultToday then
        FPopup.Date:=sysutils.Date;
    end;
  end
  else
  if FDefaultToday then
    FPopup.Date:=sysutils.Date;
end;

begin
  if not Assigned(FPopup) then
    FPopup:=CreatePopupForm;

  UpdatePopup;

  if (FPopup <> nil) and not (ReadOnly {or FPopupVisible}) then
  begin

    P := Parent.ClientToScreen(Point(Left, Top));

    ABounds := Screen.MonitorFromPoint(P).BoundsRect;

    Y := P.Y + Height;
    if Y + FPopup.Height > ABounds.Bottom then
      Y := P.Y - FPopup.Height;
    case FPopupAlign of
      epaRight:
        begin
          Dec(P.X, FPopup.Width - Width);
          if P.X < 0 then Inc(P.X, FPopup.Width - Width);
        end;
      epaLeft:
        begin
          if P.X + FPopup.Width > ABounds.Right then
            Dec(P.X, FPopup.Width - Width);
        end;
    end;
    if P.X < 0 then P.X := 0
    else if P.X + FPopup.Width > ABounds.Right then
      P.X := ABounds.Right - FPopup.Width;

    DoTrySetDate;

    ShowPopup(Point(P.X, Y));
//    FPopupVisible := True;
{    if DisableEdit then
    begin
      inherited ReadOnly := True;
      HideCaret(Handle);
    end;}
  end;
end;

procedure TCustomRxDateEdit.PopupCloseUp(Sender: TObject; Accept: Boolean);
var
  AValue: Variant;
begin
(*
  if (FPopup <> nil) and FPopupVisible then
  begin
{    if GetCapture <> 0 then
      SendMessage(GetCapture, WM_CANCELMODE, 0, 0);}
//    AValue := GetPopupValue;
    HidePopup;
    try
      try
        if CanFocus then
        begin
          SetFocus;
//          if GetFocus = Handle then SetShowCaret;
        end;
      except
        { ignore exceptions }
      end;
//      DirectInput:=DirectInput;
      Invalidate;
{      if Accept and AcceptPopup(AValue) and EditCanModify then
      begin
        AcceptValue(AValue);
        if FFocused then inherited SelectAll;
      end;}
    finally
      FPopupVisible := False;
    end;
  end;
*)
end;

procedure TCustomRxDateEdit.HidePopup;
begin
  FPopup.Hide;
end;

procedure TCustomRxDateEdit.ShowPopup(AOrigin: TPoint);
var
  FAccept:boolean;
  D:TDateTime;
begin
  if not Assigned(FPopup) then
    FPopup:=CreatePopupForm;
  FPopup.Left:=AOrigin.X;
  FPopup.Top:=AOrigin.Y;
  FPopup.AutoSizeForm;
  TRxCalendarGrid(FPopup.Calendar).NotInThisMonthColor := FNotInThisMonthColor;
  FAccept:=FPopup.ShowModal = mrOk;
  if CanFocus then SetFocus;

  if FAccept {and EditCanModify} then
  begin
    D:=FPopup.Date;
    if AcceptPopup(D) then
    begin
      FPopup.Date:=D;
      AcceptValue(D);
      if Focused then inherited SelectAll;
    end;
  end;

{  FPopup.Show(AOrigin);
  SetWindowPos(Handle, HWND_TOP, Origin.X, Origin.Y, 0, 0,
    SWP_NOACTIVATE or SWP_SHOWWINDOW or SWP_NOSIZE);
  Visible := True;}
end;

procedure TCustomRxDateEdit.ApplyDate(Value: TDateTime);
begin
  SetDate(Value);
  SelectAll;
end;

procedure TCustomRxDateEdit.EditChange;
begin
  if not FFormatting then
  inherited EditChange;
end;

procedure TCustomRxDateEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key in [VK_PRIOR, VK_NEXT, VK_LEFT, VK_UP, VK_RIGHT, VK_DOWN,
    VK_ADD, VK_SUBTRACT]) and
    PopupVisible then
  begin
    TPopupCalendarAccess(FPopup).KeyDown(Key, Shift);
    Key := 0;
  end
  else
  if (Shift = []) and DirectInput then
  begin
    case Key of
      VK_ADD:
        begin
          ApplyDate(NvlDate(Date, Now) + 1);
          Key := 0;
        end;
      VK_SUBTRACT:
        begin
          ApplyDate(NvlDate(Date, Now) - 1);
          Key := 0;
        end;
    end;
  end;
  inherited KeyDown(Key, Shift);
end;

procedure TCustomRxDateEdit.KeyPress(var Key: Char);
begin
  if (Key in ['T', 't', '+', '-']) and PopupVisible then
  begin
//    FPopup.KeyPress(Key);
    Key := #0;
  end
  else
  if DirectInput then
  begin
    case Key of
      'T', 't':
        begin
          ApplyDate(Trunc(Now));
          Key := #0;
        end;
      '+', '-':
        begin
          Key := #0;
        end;
    end;
  end;
  inherited KeyPress(Key);
end;

procedure TCustomRxDateEdit.EditKeyDown(var Key: word; Shift: TShiftState);
begin
  if (Key in [VK_PRIOR, VK_NEXT, VK_LEFT, VK_UP, VK_RIGHT, VK_DOWN,
    VK_ADD, VK_SUBTRACT]) and
    PopupVisible then
  begin
    TPopupCalendarAccess(FPopup).KeyDown(Key, Shift);
    Key := 0;
  end
  else
  if (Shift = []) and DirectInput then
  begin
    case Key of
      VK_ADD:
        begin
          ApplyDate(NvlDate(Date, Now) + 1);
          Key := 0;
        end;
      VK_SUBTRACT:
        begin
          ApplyDate(NvlDate(Date, Now) - 1);
          Key := 0;
        end;
    end;
  end;
  inherited EditKeyDown(Key, Shift);
end;

procedure TCustomRxDateEdit.EditKeyPress(var Key: char);
begin
  if (Key in ['T', 't', '+', '-']) and PopupVisible then
  begin
    Key := #0;
  end
  else
  if DirectInput then
  begin
    case Key of
      'T', 't':
        begin
          ApplyDate(Trunc(Now));
          Key := #0;
        end;
      '+', '-':
        begin
          Key := #0;
        end;
    end;
  end;
  inherited EditKeyPress(Key);
end;

procedure TCustomRxDateEdit.ButtonClick;
var
  D: TDateTime;
  A: Boolean;
begin
  inherited ButtonClick;
  if CalendarStyle <> csDialog then
    PopupDropDown(True)
  else
  if CalendarStyle = csDialog then
  begin
    D := Self.Date;
    A := SelectDate(D, DialogTitle, FStartOfWeek, FWeekends, FWeekendColor, FCalendarHints);
    if CanFocus then SetFocus;
    if A then
    begin
      if Assigned(FOnAcceptDate) then FOnAcceptDate(Self, D, A);
      if A then
      begin
        Self.Date := D;
        inherited SelectAll;
      end;
    end;
  end;
end;

function TCustomRxDateEdit.GetDefaultGlyphName: String;
begin
  {$IFDEF LINUX}
  Result:='picDateEdit';
  {$ELSE}
  {$IFDEF WINDOWS}
  Result:='picDateEdit';
  {$ELSE}
  Result:='';
  {$ENDIF}
  {$ENDIF}
end;

function TCustomRxDateEdit.CreatePopupForm: TPopupCalendar;
begin
  Result := CreatePopupCalendar(Self {$IFDEF USED_BiDi}, BiDiMode {$ENDIF});
  Result.OnCloseUp := @PopupCloseUp;
  Result.Color := FPopupColor;
  TRxCalendarGrid(Result.Calendar).NotInThisMonthColor:=FNotInThisMonthColor;
end;

procedure TCustomRxDateEdit.DoEnter;
begin
  if Enabled then
    inherited DoEnter;
end;

constructor TCustomRxDateEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBlanksChar := ' ';
  FDialogTitle := sDateDlgTitle;
  FPopupColor := clWindow;
  FNotInThisMonthColor := clSilver;
  FPopupAlign := epaLeft;
  FStartOfWeek := Mon;
  FWeekends := [Sun];
  FWeekendColor := clRed;
  FYearDigits := dyDefault;
  FCalendarHints := TStringList.Create;
  TStringList(FCalendarHints).OnChange := @CalendarHintsChanged;
  ControlState := ControlState + [csCreating];
  try
    UpdateFormat;
(*
{$IFDEF DEFAULT_POPUP_CALENDAR}
    FPopup := CreatePopupCalendar(Self {$IFDEF USED_BiDi}, BiDiMode {$ENDIF});
    FPopup.OnCloseUp := @PopupCloseUp;
    FPopup.Color := FPopupColor;
    FPopup.Visible:=false;
{$ELSE}
    FPopup:=nil;
{$ENDIF DEFAULT_POPUP_CALENDAR}
*)
    FPopup:=nil;
//    GlyphKind := gkDefault; { force update }
  finally
    ControlState := ControlState - [csCreating];
  end;
//  Glyph:=LoadBitmapFromLazarusResource('picDateEdit');
  NumGlyphs := 2;
end;

destructor TCustomRxDateEdit.Destroy;
begin
  if Assigned(FPopup) then
  begin
    FPopup.OnCloseUp := nil;
    FreeAndNil(FPopup);
  end;
  TStringList(FCalendarHints).OnChange := nil;
  FreeAndNil(FCalendarHints);
  inherited Destroy;
end;

procedure TCustomRxDateEdit.CheckValidDate;
begin
  if TextStored then
    try
      FFormatting := True;
      try
        SetDate(StrToDateFmt(FDateFormat, Text));
      finally
        FFormatting := False;
      end;
    except
      if CanFocus then SetFocus;
      raise;
    end;
end;

function TCustomRxDateEdit.GetDateMask: string;
begin
  case YearDigits of
    dyDefault:Result :=DefDateMask(FBlanksChar, FourDigitYear);
    dyFour:Result := DefDateMask(FBlanksChar, true);
    dyTwo:Result := DefDateMask(FBlanksChar, false);
  end;
end;

procedure TCustomRxDateEdit.UpdateMask;
var
  DateValue: TDateTime;
  OldFormat: string[10];
begin
  DateValue := GetDate;
  OldFormat := FDateFormat;
  UpdateFormat;
  if (GetDateMask <> EditMask) or (OldFormat <> FDateFormat) then
  begin
    { force update }
    EditMask := '';
    EditMask := GetDateMask;
  end;
  UpdatePopup;
  SetDate(DateValue);
end;

{$IFDEF RX_USE_LAZARUS_RESOURCE}
initialization
  {$I rxtooledit.lrs}
{$ENDIF}

end.

