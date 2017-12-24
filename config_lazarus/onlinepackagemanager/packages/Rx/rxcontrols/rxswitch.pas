{ rxswitch unit

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

unit rxswitch;

{$I rx.inc}

interface

uses SysUtils, LCLType, LCLProc, LCLIntf, LMessages, Classes, Graphics,
  Controls, Forms, ExtCtrls, Menus;

type

{ TRxSwitch }

  TTextPos = (tpNone, tpLeft, tpRight, tpAbove, tpBelow);
  TSwithState = (sw_off, sw_on);
  TSwitchBitmaps = set of TSwithState;

  TRxSwitch = class(TCustomControl)
  private
    FActive: Boolean;
    FBitmaps: array [TSwithState] of TBitmap;
    FDisableBitmaps: array [TSwithState] of TBitmap;
    FOnOn: TNotifyEvent;
    FOnOff: TNotifyEvent;
    FStateOn: TSwithState;
    FTextPosition: TTextPos;
    FBorderStyle: TBorderStyle;
    FToggleKey: TShortCut;
    FShowFocus: Boolean;
    FUserBitmaps: TSwitchBitmaps;
    function GetSwitchGlyphOff: TBitmap;
    function GetSwitchGlyphOn: TBitmap;
    procedure GlyphChanged(Sender: TObject);
    procedure SetStateOn(Value: TSwithState);
    procedure SetSwitchGlyphOff(const AValue: TBitmap);
    procedure SetSwitchGlyphOn(const AValue: TBitmap);
    procedure SetTextPosition(Value: TTextPos);
    procedure SetBorderStyle(Value: TBorderStyle);
    function GetSwitchGlyph(Index: TSwithState): TBitmap;
    procedure SetSwitchGlyph(Index: TSwithState; Value: TBitmap);
    function StoreBitmap(Index: TSwithState): Boolean;
    procedure SetShowFocus(Value: Boolean);
    procedure CreateDisabled(Index: TSwithState);
    procedure ReadBinaryData(Stream: TStream);
    function StoreBitmapOff: boolean;
    function StoreBitmapOn: boolean;
    procedure WriteBinaryData(Stream: TStream);
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMFocusChanged(var Message: TLMessage); message CM_FOCUSCHANGED;
    procedure CMTextChanged(var Message: TLMessage); message CM_TEXTCHANGED;
    procedure CMEnabledChanged(var Message: TLMessage); message CM_ENABLEDCHANGED;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DefineProperties(Filer: TFiler); override;
    function GetPalette: HPALETTE; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Paint; override;
    procedure DoOn; dynamic;
    procedure DoOff; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ToggleSwitch;
  published
    property Align;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle
      default bsNone;
    property Caption;
    property Color;
    property Cursor;
    property DragMode;
    property DragCursor;
    property Enabled;
    property Font;
    property GlyphOff: TBitmap read GetSwitchGlyphOff write SetSwitchGlyphOff
      stored StoreBitmapOff;
    property GlyphOn: TBitmap read GetSwitchGlyphOn write SetSwitchGlyphOn
      stored StoreBitmapOn;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowFocus: Boolean read FShowFocus write SetShowFocus default True;
    property ToggleKey: TShortCut read FToggleKey write FToggleKey
      default VK_SPACE;
    property ShowHint;
    property StateOn: TSwithState read FStateOn write SetStateOn default sw_off;
    property TabOrder;
    property TabStop default True;
    property TextPosition: TTextPos read FTextPosition write SetTextPosition
      default tpNone;
    property Anchors;
    property Constraints;
    property DragKind;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
    property OnDragOver;
    property OnDragDrop;
    property OnEndDrag;
    property OnStartDrag;
    property OnContextPopup;
    property OnEndDock;
    property OnStartDock;
    property OnOn: TNotifyEvent read FOnOn write FOnOn;
    property OnOff: TNotifyEvent read FOnOff write FOnOff;
  end;

{$I RXSWITCH.INC}

implementation

uses rxvclutils;

const
  BorderStyles: array[TBorderStyle] of Longint = (0, WS_BORDER);

{ TRxSwitch component }

constructor TRxSwitch.Create(AOwner: TComponent);
var
  I : TSwithState;
begin
  inherited Create(AOwner);
  ControlStyle := [csClickEvents, csSetCaption, csCaptureMouse,
    csOpaque, csDoubleClicks];
  Width := 50;
  Height := 60;
  for I := sw_off to sw_on do
  begin
    FBitmaps[I] := TBitmap.Create;
    SetSwitchGlyph(I, nil);
    FBitmaps[I].OnChange := @GlyphChanged;
  end;
  FUserBitmaps := [];
  FShowFocus := True;
  FStateOn := sw_off;
  FTextPosition := tpNone;
  FBorderStyle := bsNone;
  FToggleKey := VK_SPACE;
  TabStop := True;
end;

destructor TRxSwitch.Destroy;
var
  I: Byte;
begin
  for I := 0 to 1 do
  begin
    FBitmaps[TSwithState(I)].OnChange := nil;
    FDisableBitmaps[TSwithState(I)].Free;
    FBitmaps[TSwithState(I)].Free;
  end;
  inherited Destroy;
end;

procedure TRxSwitch.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do begin
    WindowClass.Style := WindowClass.Style or CS_HREDRAW or CS_VREDRAW;
    Style := Style or Longword(BorderStyles[FBorderStyle]);
  end;
end;

procedure TRxSwitch.DefineProperties(Filer: TFiler);

  function DoWrite: Boolean;
  begin
    if Assigned(Filer.Ancestor) then
      Result := FUserBitmaps <> TRxSwitch(Filer.Ancestor).FUserBitmaps
    else Result := FUserBitmaps <> [];
  end;

begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('Data', @ReadBinaryData, @WriteBinaryData,
    DoWrite);
end;

function TRxSwitch.GetPalette: HPALETTE;
begin
  if Enabled then Result := FBitmaps[FStateOn].Palette else Result := 0;
end;

procedure TRxSwitch.ReadBinaryData(Stream: TStream);
begin
  Stream.ReadBuffer(FUserBitmaps, SizeOf(FUserBitmaps));
end;

function TRxSwitch.StoreBitmapOff: boolean;
begin
  Result:=StoreBitmap(sw_off);
end;

function TRxSwitch.StoreBitmapOn: boolean;
begin
  Result:=StoreBitmap(sw_on);
end;

procedure TRxSwitch.WriteBinaryData(Stream: TStream);
begin
  Stream.WriteBuffer(FUserBitmaps, SizeOf(FUserBitmaps));
end;

function TRxSwitch.StoreBitmap(Index: TSwithState): Boolean;
begin
  Result := Index in FUserBitmaps;
end;

function TRxSwitch.GetSwitchGlyph(Index: TSwithState): TBitmap;
begin
  if csLoading in ComponentState then Include(FUserBitmaps, Index);
  Result := FBitmaps[Index]
end;

procedure TRxSwitch.CreateDisabled(Index: TSwithState);
begin
  if FDisableBitmaps[Index] <> nil then
    FDisableBitmaps[Index].Free;
  try
    FDisableBitmaps[Index] :=nil;
//      CreateDisabledBitmap(FBitmaps[Index], clBlack);
  except
    FDisableBitmaps[Index] := nil;
    raise;
  end;
end;

procedure TRxSwitch.GlyphChanged(Sender: TObject);
var
  I: TSwithState;
begin
  for I := sw_off to sw_on do
    if Sender = FBitmaps[I] then
    begin
      CreateDisabled(I);
    end;
  Invalidate;
end;

function TRxSwitch.GetSwitchGlyphOff: TBitmap;
begin
  Result:=GetSwitchGlyph(sw_off);
end;

function TRxSwitch.GetSwitchGlyphOn: TBitmap;
begin
  Result:=GetSwitchGlyph(sw_on);
end;

procedure TRxSwitch.SetSwitchGlyph(Index: TSwithState; Value: TBitmap);
begin
  if Value <> nil then
  begin
    FBitmaps[Index].Assign(Value);
    Include(FUserBitmaps, Index);
  end
  else
  begin
    case Index of
      sw_off: FBitmaps[Index].Handle:=CreatePixmapIndirect(@RXSWITCH_OFF[0],
                                          GetSysColor(COLOR_BTNFACE));
      sw_on: FBitmaps[Index].Handle:=CreatePixmapIndirect(@RXSWITCH_ON[0],
                                          GetSysColor(COLOR_BTNFACE));
    end;
    Exclude(FUserBitmaps, Index);
  end;
end;

procedure TRxSwitch.CMFocusChanged(var Message: TLMessage);
var
  Active: Boolean;
begin
{  with Message do Active := (Sender = Self);
  if Active <> FActive then
  begin
    FActive := Active;
    if FShowFocus then Invalidate;
  end;}
  inherited;
end;

procedure TRxSwitch.CMEnabledChanged(var Message: TLMessage);
begin
  inherited;
  Invalidate;
end;

procedure TRxSwitch.CMTextChanged(var Message: TLMessage);
begin
  inherited;
  Invalidate;
end;

procedure TRxSwitch.CMDialogChar(var Message: TCMDialogChar);
begin
  if IsAccel(Message.CharCode, Caption) and CanFocus then begin
    SetFocus;
    Message.Result := 1;
  end;
end;

procedure TRxSwitch.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    if TabStop and CanFocus then SetFocus;
    ToggleSwitch;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TRxSwitch.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if FToggleKey = ShortCut(Key, Shift) then begin
    ToggleSwitch;
    Key := 0;
  end;
end;

procedure TRxSwitch.Paint;
var
  ARect: TRect;
  Text1: array[0..255] of Char;
  FontHeight: Integer;

  procedure DrawBitmap(Bmp: TBitmap);
  var
    TmpImage: TBitmap;
    IWidth, IHeight, X, Y: Integer;
    IRect: TRect;
  begin
    IWidth := Bmp.Width;
    IHeight := Bmp.Height;
    IRect := Rect(0, 0, IWidth, IHeight);
    TmpImage := TBitmap.Create;
    try
      TmpImage.Width := IWidth;
      TmpImage.Height := IHeight;
      TmpImage.Canvas.Brush.Color := Self.Brush.Color;
//      TmpImage.Canvas.BrushCopy(IRect, Bmp, IRect, Bmp.TransparentColor);
      X := 0; Y := 0;
      case FTextPosition of
        tpNone:
          begin
            X := ((Width - IWidth) div 2);
            Y := ((Height - IHeight) div 2);
          end;
        tpLeft:
          begin
            X := Width - IWidth;
            Y := ((Height - IHeight) div 2);
            Dec(ARect.Right, IWidth);
          end;
        tpRight:
          begin
            X := 0;
            Y := ((Height - IHeight) div 2);
            Inc(ARect.Left, IWidth);
          end;
        tpAbove:
          begin
            X := ((Width - IWidth) div 2);
            Y := Height - IHeight;
            Dec(ARect.Bottom, IHeight);
          end;
        tpBelow:
          begin
            X := ((Width - IWidth) div 2);
            Y := 0;
            Inc(ARect.Top, IHeight);
          end;
      end;
//      Canvas.Draw(X, Y, TmpImage);
      Canvas.Draw(X, Y, Bmp);
//      if Focused and FShowFocus and TabStop and not (csDesigning in ComponentState) then
//        Canvas.DrawFocusRect(Rect(X, Y, X + IWidth, Y + IHeight));
//        Canvas.FrameRect(Rect(X, Y, X + IWidth, Y + IHeight));
    finally
      TmpImage.Free;
    end;
  end;

begin
  ARect := GetClientRect;
  with Canvas do
  begin
    Font := Self.Font;
    Brush.Color := Self.Color;
    FillRect(ARect);
    if not Enabled and (FDisableBitmaps[FStateOn] <> nil) then
      DrawBitmap(FDisableBitmaps[FStateOn])
    else
      DrawBitmap(FBitmaps[FStateOn]);
    if FTextPosition <> tpNone then
    begin
      FontHeight := TextHeight('W');
      with ARect do
      begin
        Top := ((Bottom + Top) - FontHeight) shr 1;
        Bottom := Top + FontHeight;
      end;
      StrPCopy(Text1, Caption);
      DrawText(Handle, Text1, StrLen(Text1), ARect, {DT_EXPANDTABS or }DT_VCENTER or DT_CENTER);
    end;
  end;
end;

procedure TRxSwitch.DoOn;
begin
  if Assigned(FOnOn) then FOnOn(Self);
end;

procedure TRxSwitch.DoOff;
begin
  if Assigned(FOnOff) then FOnOff(Self);
end;

procedure TRxSwitch.ToggleSwitch;
begin
  StateOn := TSwithState(not boolean(StateOn));
end;

procedure TRxSwitch.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd(Self);
  end;
end;

procedure TRxSwitch.SetStateOn(Value: TSwithState);
begin
  if FStateOn <> Value then
  begin
    FStateOn := Value;
    Invalidate;
    if Value = sw_on then
      DoOn
    else
      DoOff;
  end;
end;

procedure TRxSwitch.SetSwitchGlyphOff(const AValue: TBitmap);
begin
  SetSwitchGlyph(sw_off, AValue);
end;

procedure TRxSwitch.SetSwitchGlyphOn(const AValue: TBitmap);
begin
  SetSwitchGlyph(sw_on, AValue);
end;

procedure TRxSwitch.SetTextPosition(Value: TTextPos);
begin
  if FTextPosition <> Value then
  begin
    FTextPosition := Value;
    Invalidate;
  end;
end;

procedure TRxSwitch.SetShowFocus(Value: Boolean);
begin
  if FShowFocus <> Value then
  begin
    FShowFocus := Value;
    if not (csDesigning in ComponentState) then Invalidate;
  end;
end;

end.
