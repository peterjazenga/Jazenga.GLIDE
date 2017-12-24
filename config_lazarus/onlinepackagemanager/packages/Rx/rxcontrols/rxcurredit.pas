{ curredit unit

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

unit rxcurredit;

{$I rx.inc}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, LMessages,
  MaskEdit;

type

  { TCustomNumEdit }

  TCustomNumEdit = class(TCustomMaskEdit)
  private
    FFocusedDisplay: boolean;
    FBeepOnError: Boolean;
    FCheckOnExit: Boolean;
    FDecimalPlaces: Cardinal;
    FDisplayFormat: string;
//    FFormatOnEditing: Boolean;
    FFormatting: Boolean;
    FMaxValue: Extended;
    FMinValue: Extended;
    FValue: Extended;
    FFocused: Boolean;
    FZeroEmpty: Boolean;
    function GetAsInteger: Longint;
    function GetIsNull: boolean;
    function GetText: string;
    function GetValue: Extended;
    procedure SetAsInteger(const AValue: Longint);
    procedure SetBeepOnError(const AValue: Boolean);
    procedure SetDecimalPlaces(const AValue: Cardinal);
    procedure SetDisplayFormat(const AValue: string);
//    procedure SetFormatOnEditing(const AValue: Boolean);
    procedure SetMaxValue(const AValue: Extended);
    procedure SetMinValue(const AValue: Extended);
    procedure SetText(const AValue: string);
    procedure SetValue(const AValue: Extended);
    procedure SetZeroEmpty(const AValue: Boolean);
    function TextToValText(const AValue: string): string;
    function CheckValue(NewValue: Extended; RaiseOnError: Boolean): Extended;
//    procedure SetFocused(Value: Boolean);
  protected
    //messages
{    procedure CMEnabledChanged(var Message: TLMessage); message CM_ENABLEDCHANGED;
    procedure CMEnter(var Message: TLMEnter); message LM_ENTER;
    procedure WMExit(var Message: TLMExit); message LM_EXIT; }

    procedure WMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
    procedure WMKillFocus(var Message: TLMKillFocus); message LM_KILLFOCUS;

//    procedure CMFontChanged(var Message: TLMessage); message CM_FONTCHANGED;
//    procedure WMPaint(var Message: TLMPaint); message LM_PAINT;
    procedure WMPaste(var Message: TLMessage); message LM_PASTE;
//    procedure GetSel(var ASelStart: Integer; var SelStop: Integer);
{    procedure DoEnter; override;
    procedure DoExit; override;}
//    procedure AcceptValue(const Value: Variant); override;

//    procedure Change; override;
//    procedure ReformatEditText; dynamic;
    procedure DataChanged; virtual;
    procedure KeyPress(var Key: Char); override;
    function IsValidChar(Key: Char): Boolean; virtual;
    function FormatDisplayText(Value: Extended): string;
    function GetDisplayText: string; virtual;
    procedure Reset; override;
    procedure CheckRange;
    procedure UpdateData;
    property Formatting: Boolean read FFormatting;
    property BeepOnError: Boolean read FBeepOnError write SetBeepOnError
      default True;
    property CheckOnExit: Boolean read FCheckOnExit write FCheckOnExit default False;
    property DecimalPlaces: Cardinal read FDecimalPlaces write SetDecimalPlaces
      default 2;
    property DisplayFormat: string read FDisplayFormat write SetDisplayFormat;
    property MaxValue: Extended read FMaxValue write SetMaxValue;
    property MinValue: Extended read FMinValue write SetMinValue;
//    property FormatOnEditing: Boolean read FFormatOnEditing write SetFormatOnEditing default False;
    property Text: string read GetText write SetText stored False;
    property MaxLength default 0;
    property ZeroEmpty: Boolean read FZeroEmpty write SetZeroEmpty default True;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Clear;
    property AsInteger: Longint read GetAsInteger write SetAsInteger;
    property DisplayText: string read GetDisplayText;
    property Value: Extended read GetValue write SetValue;
    property IsNull:boolean read GetIsNull;
  published
    { Published declarations }
  end;
  
  { TCurrencyEdit }

  TCurrencyEdit = class(TCustomNumEdit)
  protected
  public
  published
    property Alignment;
    property AutoSelect;
    property AutoSize;
    property BeepOnError;
    property BorderStyle;
    property BorderSpacing;
    property CheckOnExit;
    property Color;
    property DecimalPlaces;
    property DisplayFormat;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
//    property FormatOnEditing;
    property HideSelection;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
{$IFDEF WIN32}
  {$IFNDEF VER90}
//    property ImeMode;
//    property ImeName;
  {$ENDIF}
{$ENDIF}
    property MaxLength;
    property MaxValue;
    property MinValue;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Value;
    property Visible;
    property ZeroEmpty;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;

    property OnContextPopup;
    property OnStartDrag;
    property OnEndDock;
    property OnStartDock;
  end;

implementation
uses strutils, Math, rxtooledit, rxconst;

function IsValidFloat(const Value: string; var RetValue: Extended): Boolean;
var
  I: Integer;
  Buffer: array[0..63] of Char;
begin
  Result := False;
  for I := 1 to Length(Value) do
    if not (Value[I] in [DefaultFormatSettings.DecimalSeparator, '-', '+', '0'..'9', 'e', 'E']) then
      Exit;
  Result := TextToFloat(StrPLCopy(Buffer, Value,
    SizeOf(Buffer) - 1), RetValue, fvExtended);
end;

function FormatFloatStr(const S: string; Thousands: Boolean): string;
var
  I, MaxSym, MinSym, Group: Integer;
  IsSign: Boolean;
begin
  Result := '';
  MaxSym := Length(S);
  IsSign := (MaxSym > 0) and (S[1] in ['-', '+']);
  if IsSign then MinSym := 2
  else MinSym := 1;
  I := Pos(DefaultFormatSettings.DecimalSeparator, S);
  if I > 0 then MaxSym := I - 1;
  I := Pos('E', AnsiUpperCase(S));
  if I > 0 then MaxSym := Min(I - 1, MaxSym);
  Result := Copy(S, MaxSym + 1, MaxInt);
  Group := 0;
  for I := MaxSym downto MinSym do begin
    Result := S[I] + Result;
    Inc(Group);
    if (Group = 3) and Thousands and (I > MinSym) then begin
      Group := 0;
      Result := DefaultFormatSettings.ThousandSeparator + Result;
    end;
  end;
  if IsSign then Result := S[1] + Result;
end;

{ TCustomNumEdit }

function TCustomNumEdit.GetAsInteger: Longint;
begin
  Result := Trunc(Value);
end;

function TCustomNumEdit.GetIsNull: boolean;
begin
  Result:=false;
end;

function TCustomNumEdit.GetDisplayText: string;
begin
  Result := FormatDisplayText(Value);
end;

procedure TCustomNumEdit.Reset;
begin
  DataChanged;
  SelectAll;
end;

procedure TCustomNumEdit.CheckRange;
begin
  if not (csDesigning in ComponentState) and CheckOnExit then
    CheckValue(StrToFloat(TextToValText(EditText)), True);
end;

procedure TCustomNumEdit.UpdateData;
begin
  ValidateEdit;
  FValue := CheckValue(StrToFloat(TextToValText(EditText)), False);
end;

constructor TCustomNumEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption];
  MaxLength := 0;
  FBeepOnError := True;
  FDecimalPlaces := 2;
  FZeroEmpty := True;
  inherited Text := '';
  Alignment := taRightJustify;
  DataChanged;
  ControlState := ControlState + [csCreating];
end;

function TCustomNumEdit.GetText: string;
begin
  if (FValue = 0) and FZeroEmpty then
    Result:=''
  else
    Result := FloatToStr(FValue);
end;

function TCustomNumEdit.GetValue: Extended;
begin
  if (not (csDesigning in ComponentState)) and FFocusedDisplay then
  begin
    try
      UpdateData;
    except
      FValue := FMinValue;
    end;
  end;
  Result := FValue;
end;

procedure TCustomNumEdit.SetAsInteger(const AValue: Longint);
begin
  SetValue(AValue);
end;

procedure TCustomNumEdit.SetBeepOnError(const AValue: Boolean);
begin
  if FBeepOnError=AValue then exit;
  FBeepOnError:=AValue;
end;

procedure TCustomNumEdit.SetDecimalPlaces(const AValue: Cardinal);
begin
  if FDecimalPlaces=AValue then exit;
  FDecimalPlaces:=AValue;
  DataChanged;
  Invalidate;
end;

procedure TCustomNumEdit.SetDisplayFormat(const AValue: string);
begin
  if FDisplayFormat=AValue then exit;
  FDisplayFormat:=AValue;
  DataChanged;
end;

{
procedure TCustomNumEdit.SetFormatOnEditing(const AValue: Boolean);
begin
  if FFormatOnEditing=AValue then exit;
  FFormatOnEditing:=AValue;
  if FFormatOnEditing and FFocused then
    ReformatEditText
  else
  if FFocused then
  begin
    UpdateData;
    DataChanged;
  end;
end;
}
procedure TCustomNumEdit.SetMaxValue(const AValue: Extended);
begin
  if FMaxValue=AValue then exit;
  FMaxValue:=AValue;
  if Value > AValue then
    Value:=AValue;
end;

procedure TCustomNumEdit.SetMinValue(const AValue: Extended);
begin
  if FMinValue=AValue then exit;
  FMinValue:=AValue;
  if Value < AValue then
    Value:=AValue;
end;

procedure TCustomNumEdit.SetText(const AValue: string);
begin
  if not (csReading in ComponentState) then
  begin
    FValue := CheckValue(StrToFloat(TextToValText(AValue)), False);
    DataChanged;
    Invalidate;
  end;
end;

procedure TCustomNumEdit.SetValue(const AValue: Extended);
begin
  FValue := CheckValue(AValue, False);
  DataChanged;
  Invalidate;
end;

procedure TCustomNumEdit.SetZeroEmpty(const AValue: Boolean);
begin
  if FZeroEmpty=AValue then exit;
  FZeroEmpty:=AValue;
  DataChanged;
end;

function TCustomNumEdit.TextToValText(const AValue: string): string;
begin
  Result := Trim(AValue);
  if DefaultFormatSettings.DecimalSeparator <> DefaultFormatSettings.ThousandSeparator then begin
    Result := DelChars(Result, ThousandSeparator);
  end;
  if (DefaultFormatSettings.DecimalSeparator <> '.') and (DefaultFormatSettings.ThousandSeparator <> '.') then
    Result := StringReplace(Result, '.', DefaultFormatSettings.DecimalSeparator, [rfReplaceAll]);
  if (DefaultFormatSettings.DecimalSeparator <> ',') and (DefaultFormatSettings.ThousandSeparator <> ',') then
    Result := StringReplace(Result, ',', DefaultFormatSettings.DecimalSeparator, [rfReplaceAll]);
  if Result = '' then Result := '0'
  else if Result = '-' then Result := '-0';
end;

function TCustomNumEdit.CheckValue(NewValue: Extended; RaiseOnError: Boolean
  ): Extended;
begin
  Result := NewValue;
  if (FMaxValue <> FMinValue) then begin
    if (FMaxValue > FMinValue) then begin
      if NewValue < FMinValue then Result := FMinValue
      else if NewValue > FMaxValue then Result := FMaxValue;
    end
    else begin
      if FMaxValue = 0 then begin
        if NewValue < FMinValue then Result := FMinValue;
      end
      else if FMinValue = 0 then begin
        if NewValue > FMaxValue then Result := FMaxValue;
      end;
    end;
    if RaiseOnError and (Result <> NewValue) then
      raise ERangeError.CreateFmt(StringReplace(SOutOfRange, '%d', '%.*f', [rfReplaceAll]),
        [DecimalPlaces, FMinValue, DecimalPlaces, FMaxValue]);
  end;
end;
{
procedure TCustomNumEdit.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then
  begin
    FFocused := Value;
    Invalidate;
    FFormatting := True;
    try
      DataChanged;
    finally
      FFormatting := False;
    end;
  end;
end;
}
procedure TCustomNumEdit.WMSetFocus(var Message: TLMSetFocus);
begin
  inherited WMSetFocus(Message);
  // some widgetsets do not notify clipboard actions properly. Put at edit state at entry
  if FFocusedDisplay then
    exit;
  FFocusedDisplay := true;
  Reset;
end;

procedure TCustomNumEdit.WMKillFocus(var Message: TLMKillFocus);
begin
  inherited WMKillFocus(Message);
  FFocusedDisplay := False;
  UpdateData;
  if not Focused then
      DisableMask(GetDisplayText);
end;

{
procedure TCustomNumEdit.CMEnabledChanged(var Message: TLMessage);
begin
  inherited;
  if NewStyleControls and not FFocused then Invalidate;
end;

procedure TCustomNumEdit.CMEnter(var Message: TLMEnter);
begin
  SetFocused(True);
  if FFormatOnEditing then ReformatEditText;
  inherited;
end;

procedure TCustomNumEdit.WMExit(var Message: TLMExit);
begin
  inherited;
  try
    CheckRange;
    UpdateData;
  except
    SelectAll;
    if CanFocus then SetFocus;
    raise;
  end;
  SetFocused(False);
  Cursor:=0;
  DoExit;
end;

procedure TCustomNumEdit.CMFontChanged(var Message: TLMessage);
begin
  inherited;
  Invalidate;
end;

procedure TCustomNumEdit.WMPaint(var Message: TLMPaint);
var
  S: string;
begin
  S := GetDisplayText;
//  if not FFocused then
//  else
//  if not PaintComboEdit(Self, S, FAlignment, FFocused {and not PopupVisible}, FCanvas, Message) then
  inherited WMPaint(Message);
end;
}
procedure TCustomNumEdit.WMPaste(var Message: TLMessage);
var
  S: string;
begin
  S := EditText;
  try
    inherited;
    UpdateData;
  except
    EditText := S;
    SelectAll;
    if CanFocus then SetFocus;
//    if BeepOnError then MessageBeep(0);
  end;
end;
{
procedure TCustomNumEdit.GetSel(var ASelStart: Integer; var SelStop: Integer);
begin
  ASelStart:=SelStart;
  SelStop:=SelStart + SelLength;
end;


procedure TCustomNumEdit.DoEnter;
begin
  SetFocused(True);
  if FFormatOnEditing then ReformatEditText;
  inherited DoEnter;
end;

procedure TCustomNumEdit.DoExit;
begin
  try
    CheckRange;
    UpdateData;
  except
    SelectAll;
    if CanFocus then SetFocus;
    raise;
  end;
  SetFocused(False);
  Cursor:=0;

  inherited DoExit;
  Invalidate;
end;

procedure TCustomNumEdit.AcceptValue(const Value: Variant);
begin
  inherited AcceptValue(Value);
end;

procedure TCustomNumEdit.Change;
begin
  if not FFormatting then
  begin
    if FFormatOnEditing and FFocused then ReformatEditText;
    inherited Change;
  end;
end;

procedure TCustomNumEdit.ReformatEditText;
var
  S: string;
  IsEmpty: Boolean;
  OldLen, ASelStart, SelStop: Integer;
begin
  FFormatting := True;
  try
    S := inherited Text;
    OldLen := Length(S);
    IsEmpty := (OldLen = 0) or (S = '-');
    if HandleAllocated then GetSel(ASelStart, SelStop);
    if not IsEmpty then S := TextToValText(S);
    S := FormatFloatStr(S, Pos(',', DisplayFormat) > 0);
    inherited Text := S;
{    if HandleAllocated and (GetFocus = Handle) and not
      (csDesigning in ComponentState) then
    begin
      Inc(ASelStart, Length(S) - OldLen);
      SetCursor(ASelStart);
    end;}
  finally
    FFormatting := False;
  end;
end;
}
procedure TCustomNumEdit.DataChanged;
begin
  if FFocusedDisplay then
    RestoreMask(GetText)
  else
    DisableMask(GetDisplayText)
end;

procedure TCustomNumEdit.KeyPress(var Key: Char);
begin
  if Key in ['.', ','] - [DefaultFormatSettings.ThousandSeparator] then
    Key := DefaultFormatSettings.DecimalSeparator;
  inherited KeyPress(Key);
  if (Key in [#32..#255]) and not IsValidChar(Key) then
  begin
//    if BeepOnError then MessageBeep(0);
    Key := #0;
  end
  else
  if Key = #27 then
  begin
    Reset;
    Key := #0;
  end;
end;

function TCustomNumEdit.IsValidChar(Key: Char): Boolean;
var
  S: string;
  ASelStart, SelStop, DecPos: Integer;
  RetValue: Extended;
begin
  Result := False;
  S := EditText;
  GetSel(ASelStart, SelStop);
  System.Delete(S, ASelStart + 1, SelStop - ASelStart);
  System.Insert(Key, S, ASelStart + 1);
  S := TextToValText(S);
  DecPos := Pos(DefaultFormatSettings.DecimalSeparator, S);
  if (DecPos > 0) then
  begin
    ASelStart := Pos('E', UpperCase(S));
    if (ASelStart > DecPos) then
      DecPos := ASelStart - DecPos
    else
      DecPos := Length(S) - DecPos;
    if DecPos > Integer(FDecimalPlaces) then
      Exit;

    if S[1] = DefaultFormatSettings.DecimalSeparator then
      s := '0' + s;
  end;
  Result := IsValidFloat(S, RetValue);
  if Result and (FMinValue >= 0) and (FMaxValue > 0) and (RetValue < 0) then
    Result := False;
end;

function TCustomNumEdit.FormatDisplayText(Value: Extended): string;
var
  Digits : integer;
begin
  if FZeroEmpty and (Value = 0) then
    Result:=''
  else
  if DisplayFormat <> '' then
    Result:=FormatFloat(DisplayFormat, Value)
  else
  begin
    Digits := DefaultFormatSettings.CurrencyDecimals;
    Result:=FloatToStrF(Value, ffCurrency, DecimalPlaces, Digits);
  end;
end;

procedure TCustomNumEdit.Clear;
begin
  Text:='';
end;

initialization
  RegisterPropertyToSkip( TCustomNumEdit, 'FormatOnEditing', 'This property depricated', '');
end.
