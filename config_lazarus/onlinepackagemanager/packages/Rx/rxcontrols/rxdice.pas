{ rxdice unit

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
unit rxdice;

interface

{$I rx.inc}

uses SysUtils, LCLType, LCLProc, LCLIntf, LMessages, Classes, Graphics,
  Controls, Forms, StdCtrls, ExtCtrls, Menus, rxVCLUtils;

type
  TRxDiceValue = 1..6;

{ TRxDice }

  TRxDice = class(TCustomControl)
  private
    { Private declarations }
    FActive: Boolean;
    FAutoSize: Boolean;
    FBitmap: TBitmap;
    FInterval: Cardinal;
    FAutoStopInterval: Cardinal;
    FOnChange: TNotifyEvent;
    FRotate: Boolean;
    FShowFocus: Boolean;
    FTimer: TTimer;
    FTickCount: Longint;
    FValue: TRxDiceValue;
    FOnStart: TNotifyEvent;
    FOnStop: TNotifyEvent;
    procedure CMFocusChanged(var Message: TLMessage); message CM_FOCUSCHANGED;
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
    procedure CreateBitmap;
    procedure SetAutoSize(Value: Boolean);
    procedure SetInterval(Value: Cardinal);
    procedure SetRotate(AValue: Boolean);
    procedure SetShowFocus(AValue: Boolean);
    procedure SetValue(Value: TRxDiceValue);
    procedure TimerExpired(Sender: TObject);
  protected
    { Protected declarations }
    function GetPalette: HPALETTE; override;
    procedure AdjustSize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
    procedure Change; dynamic;
    procedure DoStart; dynamic;
    procedure DoStop; dynamic;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RandomValue;
  published
    { Published declarations }
    property Align;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default True;
    property AutoStopInterval: Cardinal read FAutoStopInterval write FAutoStopInterval default 0;
    property Color;
    property Cursor;
    property DragMode;
    property DragCursor;
    property Enabled;
    property Interval: Cardinal read FInterval write SetInterval default 60;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property Rotate: Boolean read FRotate write SetRotate;
    property ShowFocus: Boolean read FShowFocus write SetShowFocus;
    property ShowHint;
    property Anchors;
    property Constraints;
    property DragKind;
    property TabOrder;
    property TabStop;
    property Value: TRxDiceValue read FValue write SetValue default 1;
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
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnStart: TNotifyEvent read FOnStart write FOnStart;
    property OnStop: TNotifyEvent read FOnStop write FOnStop;
    property OnEndDock;
    property OnStartDock;
  end;

{$I RXDICE.INC}

implementation

{ TRxDice }

constructor TRxDice.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Randomize;
  ControlStyle := [csClickEvents, csSetCaption, csCaptureMouse,
    csOpaque, csDoubleClicks];
  FValue := 1;
  FInterval := 60;
  CreateBitmap;
  FAutoSize := True;
  Width := FBitmap.Width + 2;
  Height := FBitmap.Height + 2;
end;

destructor TRxDice.Destroy;
begin
  FOnChange := nil;
  if FBitmap <> nil then FBitmap.Free;
  inherited Destroy;
end;

function TRxDice.GetPalette: HPALETTE;
begin
  if FBitmap <> nil then Result := FBitmap.Palette
  else Result := 0;
end;

procedure TRxDice.RandomValue;
var
  Val: Byte;
begin
  Val := Random(6) + 1;
  if Val = Byte(FValue) then
  begin
    if Val = 1 then Inc(Val)
    else Dec(Val);
  end;
  SetValue(TRxDiceValue(Val));
end;

procedure TRxDice.DoStart;
begin
  if Assigned(FOnStart) then FOnStart(Self);
end;

procedure TRxDice.DoStop;
begin
  if Assigned(FOnStop) then FOnStop(Self);
end;

procedure TRxDice.CMFocusChanged(var Message: TLMessage);
var
  Active: Boolean;
begin
{  with Message do Active := (Sender = Self);
  if Active <> FActive then begin
    FActive := Active;
    if FShowFocus then Invalidate;
  end;}
  inherited;
end;

procedure TRxDice.WMSize(var Message: TLMSize);
begin
  inherited;
  AdjustSize;
end;

procedure TRxDice.CreateBitmap;
begin
  if FBitmap = nil then FBitmap := TBitmap.Create;
  case FValue of
    1:FBitmap.Handle := CreatePixmapIndirect(@DICE1[0], GetSysColor(COLOR_BTNFACE));
    2:FBitmap.Handle := CreatePixmapIndirect(@DICE2[0], GetSysColor(COLOR_BTNFACE));
    3:FBitmap.Handle := CreatePixmapIndirect(@DICE3[0], GetSysColor(COLOR_BTNFACE));
    4:FBitmap.Handle := CreatePixmapIndirect(@DICE4[0], GetSysColor(COLOR_BTNFACE));
    5:FBitmap.Handle := CreatePixmapIndirect(@DICE5[0], GetSysColor(COLOR_BTNFACE));
    6:FBitmap.Handle := CreatePixmapIndirect(@DICE6[0], GetSysColor(COLOR_BTNFACE));
  end;
end;

procedure TRxDice.AdjustSize;
var
  MinSide: Integer;
begin
  if not (csReading in ComponentState) then
  begin
    if AutoSize and Assigned(FBitmap) and (FBitmap.Width > 0) and
      (FBitmap.Height > 0) then
        SetBounds(Left, Top, FBitmap.Width + 2, FBitmap.Height + 2)
    else
    begin
      { Adjust aspect ratio if control size changed }
      MinSide := Width;
      if Height < Width then MinSide := Height;
      SetBounds(Left, Top, MinSide, MinSide);
    end;
  end;
end;

procedure TRxDice.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and TabStop and CanFocus then SetFocus;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TRxDice.Paint;
var
  ARect: TRect;

  procedure DrawBitmap;
  var
    TmpImage: TBitmap;
    IWidth, IHeight: Integer;
    IRect: TRect;
  begin
    IWidth := FBitmap.Width;
    IHeight := FBitmap.Height;
    IRect := Rect(0, 0, IWidth, IHeight);
    TmpImage := TBitmap.Create;
    try
      TmpImage.Width := IWidth;
      TmpImage.Height := IHeight;
      TmpImage.Canvas.Brush.Color := Self.Brush.Color;
//      TmpImage.Canvas.BrushCopy(IRect, FBitmap, IRect, FBitmap.TransparentColor);
      InflateRect(ARect, -1, -1);
//      Canvas.StretchDraw(ARect, TmpImage);
      Canvas.StretchDraw(ARect, FBitmap);

    finally
      TmpImage.Free;
    end;
  end;

begin
  ARect := ClientRect;
  if FBitmap <> nil then DrawBitmap;
{  if Focused and FShowFocus and TabStop and not (csDesigning in ComponentState) then
  begin
    Canvas.DrawFocusRect(ARect);
  end;}
end;

procedure TRxDice.TimerExpired(Sender: TObject);
var
  ParentForm: TCustomForm;
  Now: Longint;
begin
  RandomValue;
  if not FRotate then
  begin
    FTimer.Free;
    FTimer := nil;
    if (csDesigning in ComponentState) then
    begin
      ParentForm := GetParentForm(Self);
      if ParentForm <> nil then ParentForm.Designer.Modified;
    end;
    DoStop;
  end
  else
  if AutoStopInterval > 0 then
  begin
    Now := GetTickCount;
    if (Now - FTickCount >= AutoStopInterval) or (Now < FTickCount) then
      Rotate := False;
  end;
end;

procedure TRxDice.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TRxDice.SetValue(Value: TRxDiceValue);
begin
  if FValue <> Value then
  begin
    FValue := Value;
    CreateBitmap;
    Invalidate;
    Change;
  end;
end;

procedure TRxDice.SetAutoSize(Value: Boolean);
begin
  if Value <> FAutoSize then
  begin
    FAutoSize := Value;
    AdjustSize;
    Invalidate;
  end;
end;

procedure TRxDice.SetInterval(Value: Cardinal);
begin
  if FInterval <> Value then
  begin
    FInterval := Value;
    if FTimer <> nil then FTimer.Interval := FInterval;
  end;
end;

procedure TRxDice.SetRotate(AValue: Boolean);
begin
  if FRotate <> AValue then
  begin
    if AValue then
    begin
      if FTimer = nil then FTimer := TTimer.Create(Self);
      try
        with FTimer do
        begin
          OnTimer := @TimerExpired;
          Interval := FInterval;
          Enabled := True;
        end;
        FRotate := AValue;
        FTickCount := GetTickCount;
        DoStart;
      except
        FTimer.Free;
        FTimer := nil;
        raise;
      end;
    end
    else
      FRotate := AValue;
  end;
end;

procedure TRxDice.SetShowFocus(AValue: Boolean);
begin
  if FShowFocus <> AValue then
  begin
    FShowFocus := AValue;
    if not (csDesigning in ComponentState) then Invalidate;
  end;
end;

end.
