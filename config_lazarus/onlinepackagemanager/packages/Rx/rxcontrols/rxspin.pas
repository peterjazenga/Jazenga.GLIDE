{ rxspin unit

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

unit rxspin;

interface

{$I rx.inc}

uses ComCtrls, LCLIntf, LCLType, Controls, ExtCtrls, Classes,
  Graphics, LMessages, Forms, StdCtrls, Menus, SysUtils, Messages;

type

{ TRxSpinButton }

  TSpinButtonState = (sbNotDown, sbTopDown, sbBottomDown);

  TRxSpinButton = class(TGraphicControl)
  private
    FDown: TSpinButtonState;
    FUpBitmap: TBitmap;
    FDownBitmap: TBitmap;
    FDragging: Boolean;
    FInvalidate: Boolean;
    FTopDownBtn: TBitmap;
    FBottomDownBtn: TBitmap;
    FRepeatTimer: TTimer;
    FNotDownBtn: TBitmap;
    FLastDown: TSpinButtonState;
    FFocusControl: TWinControl;
    FOnTopClick: TNotifyEvent;
    FOnBottomClick: TNotifyEvent;
    procedure TopClick;
    procedure BottomClick;
    procedure GlyphChanged(Sender: TObject);
    function GetUpGlyph: TBitmap;
    function GetDownGlyph: TBitmap;
    procedure SetUpGlyph(Value: TBitmap);
    procedure SetDownGlyph(Value: TBitmap);
    procedure SetDown(Value: TSpinButtonState);
    procedure SetFocusControl(Value: TWinControl);
    procedure DrawAllBitmap;
    procedure DrawBitmap(ABitmap: TBitmap; ADownState: TSpinButtonState);
    procedure TimerExpired(Sender: TObject);
    procedure CMEnabledChanged(var Message: TLMessage); message CM_ENABLEDCHANGED;
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Down: TSpinButtonState read FDown write SetDown default sbNotDown;
  published
    property DragCursor;
    property DragMode;
    property Enabled;
    property Visible;
    property DownGlyph: TBitmap read GetDownGlyph write SetDownGlyph;
    property UpGlyph: TBitmap read GetUpGlyph write SetUpGlyph;
    property FocusControl: TWinControl read FFocusControl write SetFocusControl;
    property ShowHint;
    property ParentShowHint;
{$IFDEF RX_D4}
    property Anchors;
    property Constraints;
    property DragKind;
{$ENDIF}
    property OnBottomClick: TNotifyEvent read FOnBottomClick write FOnBottomClick;
    property OnTopClick: TNotifyEvent read FOnTopClick write FOnTopClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
{$IFDEF RX_D4}
    property OnEndDock;
    property OnStartDock;
{$ENDIF}
  end;

{ TRxSpinEdit }

  TValueType = (vtInteger, vtFloat, vtHex);

  TRxSpinEdit = class(TCustomEdit)
  private
    FAlignment: TAlignment;
    FMinValue: Extended;
    FMaxValue: Extended;
    FIncrement: Extended;
    FDecimal: Byte;
    FChanging: Boolean;
    FEditorEnabled: Boolean;
    FValueType: TValueType;
    FButton: TRxSpinButton;
    FBtnWindow: TWinControl;
    FArrowKeys: Boolean;
    FOnTopClick: TNotifyEvent;
    FOnBottomClick: TNotifyEvent;
    function GetMinHeight: Integer;
    procedure GetTextHeight(var SysHeight, aHeight: Integer);
    function GetValue: Extended;
    function CheckValue(NewValue: Extended): Extended;
    function GetAsInteger: Longint;
    function IsIncrementStored: Boolean;
    function IsMaxStored: Boolean;
    function IsMinStored: Boolean;
    function IsValueStored: Boolean;
    procedure SetArrowKeys(Value: Boolean);
    procedure SetAsInteger(NewValue: Longint);
    procedure SetValue(NewValue: Extended);
    procedure SetValueType(NewType: TValueType);
    procedure SetDecimal(NewValue: Byte);
    function GetButtonWidth: Integer;
    procedure RecreateButton;
    procedure ResizeButton;
    procedure SetAlignment(Value: TAlignment);
    procedure LMSize(var Message: TLMSize); message LM_SIZE;
    procedure CMEnter(var Message: TLMessage); message CM_ENTER;
    procedure CMExit(var Message: TLMExit); message CM_EXIT;
    procedure WMPaste(var Message: TLMessage); message LM_PASTE;
    procedure WMCut(var Message: TLMessage); message LM_CUT;
//    procedure CMCtl3DChanged(var Message: TLMessage); message CM_CTL3DCHANGED;
    procedure CMEnabledChanged(var Message: TLMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TLMessage); message CM_FONTCHANGED;
    procedure CheckButtonVisible;
    procedure WMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
  protected
    procedure Change; override;
    function IsValidChar(Key: Char): Boolean; virtual;
    procedure UpClick(Sender: TObject); virtual;
    procedure DownClick(Sender: TObject); virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    // Added from TEditButton
    procedure SetParent(AParent: TWinControl); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Loaded; override;
    procedure CMVisibleChanged(var Msg: TLMessage); message CM_VISIBLECHANGED;
    //
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property AsInteger: Longint read GetAsInteger write SetAsInteger default 0;
    property Text;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment
      default taLeftJustify;
    property ArrowKeys: Boolean read FArrowKeys write SetArrowKeys default True;
    property Decimal: Byte read FDecimal write SetDecimal default 2;
    property EditorEnabled: Boolean read FEditorEnabled write FEditorEnabled default True;
    property Increment: Extended read FIncrement write FIncrement stored IsIncrementStored;
    property MaxValue: Extended read FMaxValue write FMaxValue stored IsMaxStored;
    property MinValue: Extended read FMinValue write FMinValue stored IsMinStored;
    property ValueType: TValueType read FValueType write SetValueType default vtInteger;
    property Value: Extended read GetValue write SetValue stored IsValueStored;
    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property Color;
//    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property MaxLength;
    property ParentColor;
//    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnBottomClick: TNotifyEvent read FOnBottomClick write FOnBottomClick;
    property OnTopClick: TNotifyEvent read FOnTopClick write FOnTopClick;
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
    property OnEndDock;
    property OnStartDock;
//{$ENDIF}
  end;

implementation

uses
  rxvclutils, LResources;

const
  sSpinUpBtn = 'RXSPINUP';
  sSpinDownBtn = 'RXSPINDOWN';

const
  InitRepeatPause = 400; { pause before repeat timer (ms) }
  RepeatPause     = 100;

{ TRxSpinButton }

constructor TRxSpinButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{  FUpBitmap := TBitmap.Create;
  FDownBitmap := TBitmap.Create;}
  FUpBitmap := LoadBitmapFromLazarusResource(sSpinUpBtn);
  FDownBitmap := LoadBitmapFromLazarusResource(sSpinDownBtn);
  FUpBitmap.OnChange := @GlyphChanged;
  FDownBitmap.OnChange := @GlyphChanged;
  Height := 20;
  Width := 20;
  FTopDownBtn := TBitmap.Create;
  FBottomDownBtn := TBitmap.Create;
  FNotDownBtn := TBitmap.Create;
  DrawAllBitmap;
  FLastDown := sbNotDown;
end;

destructor TRxSpinButton.Destroy;
begin
  FTopDownBtn.Free;
  FBottomDownBtn.Free;
  FNotDownBtn.Free;
  FUpBitmap.Free;
  FDownBitmap.Free;
  FRepeatTimer.Free;
  inherited Destroy;
end;

procedure TRxSpinButton.GlyphChanged(Sender: TObject);
begin
  FInvalidate := True;
  Invalidate;
end;

function TRxSpinButton.GetUpGlyph: TBitmap;
begin
  Result := FUpBitmap;
end;

procedure TRxSpinButton.SetUpGlyph(Value: TBitmap);
begin
  if Value <> nil then FUpBitmap.Assign(Value)
  else
    FUpBitmap := LoadBitmapFromLazarusResource(sSpinUpBtn);
end;

function TRxSpinButton.GetDownGlyph: TBitmap;
begin
  Result := FDownBitmap;
end;

procedure TRxSpinButton.SetDownGlyph(Value: TBitmap);
begin
  if Value <> nil then FDownBitmap.Assign(Value)
  else
    FDownBitmap := LoadBitmapFromLazarusResource(sSpinDownBtn);
end;

procedure TRxSpinButton.SetDown(Value: TSpinButtonState);
var
  OldState: TSpinButtonState;
begin
  OldState := FDown;
  FDown := Value;
  if OldState <> FDown then Repaint;
end;

procedure TRxSpinButton.SetFocusControl(Value: TWinControl);
begin
  FFocusControl := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

procedure TRxSpinButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FFocusControl) then
    FFocusControl := nil;
end;

procedure TRxSpinButton.Paint;
begin
  if not Enabled and not (csDesigning in ComponentState) then
    FDragging := False;
  if (FNotDownBtn.Height <> Height) or (FNotDownBtn.Width <> Width) or
    FInvalidate then DrawAllBitmap;
  FInvalidate := False;
  with Canvas do
    case FDown of
      sbNotDown: Draw(0, 0, FNotDownBtn);
      sbTopDown: Draw(0, 0, FTopDownBtn);
      sbBottomDown: Draw(0, 0, FBottomDownBtn);
    end;
end;

procedure TRxSpinButton.DrawAllBitmap;
begin
  DrawBitmap(FTopDownBtn, sbTopDown);
  DrawBitmap(FBottomDownBtn, sbBottomDown);
  DrawBitmap(FNotDownBtn, sbNotDown);
end;

procedure TRxSpinButton.DrawBitmap(ABitmap: TBitmap; ADownState: TSpinButtonState);
var
  R, RSrc: TRect;
  dRect: Integer;
  {Temp: TBitmap;}
begin
  ABitmap.Height := Height;
  ABitmap.Width := Width;
  with ABitmap.Canvas do begin
    R := Bounds(0, 0, Width, Height);
    Pen.Width := 1;
    Brush.Color := clBtnFace;
    Brush.Style := bsSolid;
    FillRect(R);
    { buttons frame }
    Pen.Color := clWindowFrame;
    Rectangle(0, 0, Width, Height);
    MoveTo(-1, Height);
    LineTo(Width, -1);
    { top button }
    if ADownState = sbTopDown then Pen.Color := clBtnShadow
    else Pen.Color := clBtnHighlight;
    MoveTo(1, Height - 4);
    LineTo(1, 1);
    LineTo(Width - 3, 1);
    if ADownState = sbTopDown then Pen.Color := clBtnHighlight
      else Pen.Color := clBtnShadow;
    if ADownState <> sbTopDown then begin
      MoveTo(1, Height - 3);
      LineTo(Width - 2, 0);
    end;
    { bottom button }
    if ADownState = sbBottomDown then Pen.Color := clBtnHighlight
      else Pen.Color := clBtnShadow;
    MoveTo(2, Height - 2);
    LineTo(Width - 2, Height - 2);
    LineTo(Width - 2, 1);
    if ADownState = sbBottomDown then Pen.Color := clBtnShadow
      else Pen.Color := clBtnHighlight;
    MoveTo(2, Height - 2);
    LineTo(Width - 1, 1);
    { top glyph }
    dRect := 1;
    if ADownState = sbTopDown then Inc(dRect);
    R := Bounds(Round((Width / 4) - (FUpBitmap.Width / 2)) + dRect,
      Round((Height / 4) - (FUpBitmap.Height / 2)) + dRect, FUpBitmap.Width,
      FUpBitmap.Height);
    RSrc := Bounds(0, 0, FUpBitmap.Width, FUpBitmap.Height);
    {
    if Self.Enabled or (csDesigning in ComponentState) then
      BrushCopy(R, FUpBitmap, RSrc, FUpBitmap.TransparentColor)
    else begin
      Temp := CreateDisabledBitmap(FUpBitmap, clBlack);
      try
        BrushCopy(R, Temp, RSrc, Temp.TransparentColor);
      finally
        Temp.Free;
      end;
    end;
    }
    //BrushCopy(R, FUpBitmap, RSrc, FUpBitmap.TransparentColor);
    StretchDraw(R, FUpBitmap);
    { bottom glyph }
    R := Bounds(Round((3 * Width / 4) - (FDownBitmap.Width / 2)) - 1,
      Round((3 * Height / 4) - (FDownBitmap.Height / 2)) - 1,
      FDownBitmap.Width, FDownBitmap.Height);
    RSrc := Bounds(0, 0, FDownBitmap.Width, FDownBitmap.Height);
    {
    if Self.Enabled or (csDesigning in ComponentState) then
      BrushCopy(R, FDownBitmap, RSrc, FDownBitmap.TransparentColor)
    else begin
      Temp := CreateDisabledBitmap(FDownBitmap, clBlack);
      try
        BrushCopy(R, Temp, RSrc, Temp.TransparentColor);
      finally
        Temp.Free;
      end;
    end;
    }
    //BrushCopy(R, FDownBitmap, RSrc, FDownBitmap.TransparentColor);
    StretchDraw(R, FDownBitmap);
    if ADownState = sbBottomDown then begin
      Pen.Color := clBtnShadow;
      MoveTo(3, Height - 2);
      LineTo(Width - 1, 2);
    end;
  end;
end;

procedure TRxSpinButton.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  FInvalidate := True;
  Invalidate;
end;

procedure TRxSpinButton.TopClick;
begin
  if Assigned(FOnTopClick) then begin
    FOnTopClick(Self);
    if not (csLButtonDown in ControlState) then FDown := sbNotDown;
  end;
end;

procedure TRxSpinButton.BottomClick;
begin
  if Assigned(FOnBottomClick) then begin
    FOnBottomClick(Self);
    if not (csLButtonDown in ControlState) then FDown := sbNotDown;
  end;
end;

procedure TRxSpinButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) and Enabled then begin
    if (FFocusControl <> nil) and FFocusControl.TabStop and
      FFocusControl.CanFocus and (GetFocus <> FFocusControl.Handle) then
        FFocusControl.SetFocus;
    if FDown = sbNotDown then begin
      FLastDown := FDown;
      if Y > (-(Height/Width) * X + Height) then begin
        FDown := sbBottomDown;
        BottomClick;
      end
      else begin
        FDown := sbTopDown;
        TopClick;
      end;
      if FLastDown <> FDown then begin
        FLastDown := FDown;
        Repaint;
      end;
      if FRepeatTimer = nil then FRepeatTimer := TTimer.Create(Self);
      FRepeatTimer.OnTimer := @TimerExpired;
      FRepeatTimer.Interval := InitRepeatPause;
      FRepeatTimer.Enabled := True;
    end;
    FDragging := True;
  end;
end;

procedure TRxSpinButton.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewState: TSpinButtonState;
begin
  inherited MouseMove(Shift, X, Y);
  if FDragging then begin
    if (X >= 0) and (X <= Width) and (Y >= 0) and (Y <= Height) then begin
      NewState := FDown;
      if Y > (-(Width / Height) * X + Height) then begin
        if (FDown <> sbBottomDown) then begin
          if FLastDown = sbBottomDown then FDown := sbBottomDown
          else FDown := sbNotDown;
          if NewState <> FDown then Repaint;
        end;
      end
      else begin
        if (FDown <> sbTopDown) then begin
          if (FLastDown = sbTopDown) then FDown := sbTopDown
          else FDown := sbNotDown;
          if NewState <> FDown then Repaint;
        end;
      end;
    end else
      if FDown <> sbNotDown then begin
        FDown := sbNotDown;
        Repaint;
      end;
  end;
end;

procedure TRxSpinButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FDragging then begin
    FDragging := False;
    if (X >= 0) and (X <= Width) and (Y >= 0) and (Y <= Height) then begin
      FDown := sbNotDown;
      FLastDown := sbNotDown;
      Repaint;
    end;
  end;
end;

procedure TRxSpinButton.TimerExpired(Sender: TObject);
begin
  FRepeatTimer.Interval := RepeatPause;
  if (FDown <> sbNotDown) and MouseCapture then begin
    try
      if FDown = sbBottomDown then BottomClick else TopClick;
    except
      FRepeatTimer.Enabled := False;
      raise;
    end;
  end;
end;

function DefBtnWidth: Integer;
begin
  Result := GetSystemMetrics(SM_CXVSCROLL);
  if Result > 15 then Result := 15;
end;

{ TRxSpinEdit }

constructor TRxSpinEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Text := '0';
  ControlStyle := ControlStyle - [csSetCaption];
  FIncrement := 1.0;
  FDecimal := 2;
  FEditorEnabled := True;
  FArrowKeys := True;
  RecreateButton;
  CheckButtonVisible
end;

destructor TRxSpinEdit.Destroy;
begin
  Destroying;
  FChanging := True;
  if FButton <> nil then
    FreeAndNil(FButton);
  if FBtnWindow <> nil then
    FreeAndNil(FBtnWindow);
  inherited Destroy;
end;

procedure TRxSpinEdit.RecreateButton;
begin
  if (csDestroying in ComponentState) then
    Exit;
  if FButton <> nil then
    FreeAndNil(FButton);

  if FBtnWindow <> nil then
    FreeAndNil(FBtnWindow);

  FBtnWindow := TWinControl.Create(Self);
//  FBtnWindow.ComponentStyle:=FBtnWindow.ComponentStyle + csSubComponent;
  with FBtnWindow do
  begin
    FreeNotification(Self);
    Height := Self.Height;
    Width := Self.Height;
    ControlStyle := ControlStyle + [csNoDesignSelectable];
  end;

  if FBtnWindow <> nil then
  begin
    FButton := TRxSpinButton.Create(Self);
    with FButton do
    begin
      FocusControl := Self;
      OnTopClick := @UpClick;
      OnBottomClick := @DownClick;
      Width := FBtnWindow.Height;
      Height := FBtnWindow.Height;
      FreeNotification(FBtnWindow);
    end;
  end;
  CheckButtonVisible;
end;

procedure TRxSpinEdit.SetArrowKeys(Value: Boolean);
begin
  FArrowKeys := Value;
  ResizeButton;
end;

function TRxSpinEdit.GetButtonWidth: Integer;
begin
  if FBtnWindow <> nil then
    Result := FBtnWindow.Width
  else
    Result := DefBtnWidth;
end;

procedure TRxSpinEdit.ResizeButton;
begin
  if FBtnWindow <> nil then begin
    FBtnWindow.Parent   := Parent;
    FBtnWindow.SetBounds(Width, Top, Height, Height);
    if FButton <> nil then
      FButton.SetBounds(0, 0, FBtnWindow.Width, FBtnWindow.Height);
  end;
end;

procedure TRxSpinEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if ArrowKeys and (Key in [VK_UP, VK_DOWN]) then begin
    if Key = VK_UP then UpClick(Self)
    else if Key = VK_DOWN then DownClick(Self);
    Key := 0;
  end;
end;

procedure TRxSpinEdit.Change;
begin
  if not FChanging then inherited Change;
end;

procedure TRxSpinEdit.KeyPress(var Key: Char);
begin
  if not IsValidChar(Key) then begin
    Key := #0;
    Beep;
  end;
  if Key <> #0 then begin
    inherited KeyPress(Key);
    if (Key = Char(VK_RETURN)) or (Key = Char(VK_ESCAPE)) then begin
      { must catch and remove this, since is actually multi-line }
      GetParentForm(Self).Perform(CM_DIALOGKEY, Byte(Key), 0);
      if Key = Char(VK_RETURN) then Key := #0;
    end;
  end;
end;

function TRxSpinEdit.IsValidChar(Key: Char): Boolean;
var
  ValidChars: set of Char;
begin
  ValidChars := ['+', '-', '0'..'9'];
  if ValueType = vtFloat then begin
    if Pos(DefaultFormatSettings.DecimalSeparator, Text) = 0 then
      ValidChars := ValidChars + [DefaultFormatSettings.DecimalSeparator];
    if Pos('E', AnsiUpperCase(Text)) = 0 then
      ValidChars := ValidChars + ['e', 'E'];
  end
  else if ValueType = vtHex then begin
    ValidChars := ValidChars + ['A'..'F', 'a'..'f'];
  end;
  Result := (Key in ValidChars) or (Key < #32);
  if not FEditorEnabled and Result and ((Key >= #32) or
    (Key = Char(VK_BACK)) or (Key = Char(VK_DELETE))) then Result := False;
end;

procedure TRxSpinEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
end;

procedure TRxSpinEdit.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if FBtnWindow <> nil then begin
    FBtnWindow.Parent := AParent;
    FBtnWindow.AnchorToCompanion(akLeft, 0, Self);
    FBtnWindow.Visible := True;
    if FButton <> nil then begin
      FButton.Parent := FBtnWindow;
      FButton.Visible:= True;
    end;
  end;
end;

procedure TRxSpinEdit.Notification(AComponent: TComponent; Operation: TOperation
  );
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FBtnWindow) and (Operation = opRemove) then begin
    if FButton <> nil then
      FreeAndNil(FButton);
    FreeAndNil(FBtnWindow);
  end;
end;

procedure TRxSpinEdit.Loaded;
begin
  inherited Loaded;
  CheckButtonVisible;
  ResizeButton;
end;

procedure TRxSpinEdit.CMVisibleChanged(var Msg: TLMessage);
begin
  inherited CMVisibleChanged(Msg);
  CheckButtonVisible;
end;

procedure TRxSpinEdit.CreateWnd;
begin
  inherited CreateWnd;
end;

procedure TRxSpinEdit.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then begin
    FAlignment := Value;
    RecreateWnd(Self);
  end;
end;

procedure TRxSpinEdit.LMSize(var Message: TLMSize);
var
  MinHeight: Integer;
begin
  inherited;
  ResizeButton;
end;

procedure TRxSpinEdit.GetTextHeight(var SysHeight, aHeight: Integer);
var
  DC: HDC;
  SaveFont: HFont;
  SysMetrics, Metrics: TTextMetric;
begin
  DC := GetDC(0);
  GetTextMetrics(DC, SysMetrics);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  SysHeight := SysMetrics.tmHeight;
  Height := Metrics.tmHeight;
end;

function TRxSpinEdit.GetMinHeight: Integer;
var
  I, H: Integer;
begin
  GetTextHeight(I, H);
  if I > H then I := H;
  Result := H +  (GetSystemMetrics(SM_CYBORDER) * 4) + 1;
end;

procedure TRxSpinEdit.UpClick(Sender: TObject);
var
  OldText: string;
begin
  if ReadOnly then Beep
  else begin
    FChanging := True;
    try
      OldText := inherited Text;
      Value := Value + FIncrement;
    finally
      FChanging := False;
    end;
    if CompareText(inherited Text, OldText) <> 0 then begin
      Modified := True;
      Change;
    end;
    if Assigned(FOnTopClick) then FOnTopClick(Self);
  end;
end;

procedure TRxSpinEdit.DownClick(Sender: TObject);
var
  OldText: string;
begin
  if ReadOnly then Beep
  else begin
    FChanging := True;
    try
      OldText := inherited Text;
      Value := Value - FIncrement;
    finally
      FChanging := False;
    end;
    if CompareText(inherited Text, OldText) <> 0 then begin
      Modified := True;
      Change;
    end;
    if Assigned(FOnBottomClick) then FOnBottomClick(Self);
  end;
end;

procedure TRxSpinEdit.CMFontChanged(var Message: TLMessage);
begin
  inherited;
  ResizeButton;
end;

procedure TRxSpinEdit.CheckButtonVisible;
begin
  if FBtnWindow <> nil then begin
    FBtnWindow.Visible := (csDesigning in ComponentState) or Visible;
    if FButton <> nil then
      FButton.Visible := FBtnWindow.Visible;
  end;
end;

procedure TRxSpinEdit.WMSetFocus(var Message: TLMSetFocus);
begin
  inherited;
end;

{procedure TRxSpinEdit.CMCtl3DChanged(var Message: TLMessage);
begin
  inherited;
  ResizeButton;
end;}

procedure TRxSpinEdit.CMEnabledChanged(var Message: TLMessage);
begin
  inherited;
  if FBtnWindow <> nil then
    FBtnWindow.Enabled := Enabled;
end;

procedure TRxSpinEdit.WMPaste(var Message: TLMessage);
begin
  if not FEditorEnabled or ReadOnly then Exit;
  inherited;
end;

procedure TRxSpinEdit.WMCut(var Message: TLMessage);
begin
  if not FEditorEnabled or ReadOnly then Exit;
  inherited;
end;

procedure TRxSpinEdit.CMExit(var Message: TLMExit);
begin
  inherited;
  if CheckValue(Value) <> Value then SetValue(Value);
end;

procedure TRxSpinEdit.CMEnter(var Message: TLMessage);
begin
  if AutoSelect and not (csLButtonDown in ControlState) then SelectAll;
  inherited;
end;

function TRxSpinEdit.GetValue: Extended;
begin
  try
    if ValueType = vtFloat then Result := StrToFloat(Text)
    else if ValueType = vtHex then Result := StrToInt('$' + Text)
    else Result := StrToInt(Text);
  except
    if ValueType = vtFloat then Result := FMinValue
    else Result := Trunc(FMinValue);
  end;
end;

procedure TRxSpinEdit.SetValue(NewValue: Extended);
begin
  if ValueType = vtFloat then
    Text := FloatToStrF(CheckValue(NewValue), ffFixed, 15, FDecimal)
  else if ValueType = vtHex then
    Text := IntToHex(Round(CheckValue(NewValue)), 1)
  else
    Text := IntToStr(Round(CheckValue(NewValue)));
end;

function TRxSpinEdit.GetAsInteger: Longint;
begin
  Result := Trunc(GetValue);
end;

procedure TRxSpinEdit.SetAsInteger(NewValue: Longint);
begin
  SetValue(NewValue);
end;

procedure TRxSpinEdit.SetValueType(NewType: TValueType);
begin
  if FValueType <> NewType then begin
    FValueType := NewType;
    Value := GetValue;
    if FValueType in [vtInteger, vtHex] then
    begin
      FIncrement := Round(FIncrement);
      if FIncrement = 0 then FIncrement := 1;
    end;
  end;
end;

function TRxSpinEdit.IsIncrementStored: Boolean;
begin
  Result := FIncrement <> 1.0;
end;

function TRxSpinEdit.IsMaxStored: Boolean;
begin
  Result := (MaxValue <> 0.0);
end;

function TRxSpinEdit.IsMinStored: Boolean;
begin
  Result := (MinValue <> 0.0);
end;

function TRxSpinEdit.IsValueStored: Boolean;
begin
  Result := (GetValue <> 0.0);
end;

procedure TRxSpinEdit.SetDecimal(NewValue: Byte);
begin
  if FDecimal <> NewValue then begin
    FDecimal := NewValue;
    Value := GetValue;
  end;
end;

function TRxSpinEdit.CheckValue(NewValue: Extended): Extended;
begin
  Result := NewValue;
  if (FMaxValue <> FMinValue) then begin
    if NewValue < FMinValue then
      Result := FMinValue
    else if NewValue > FMaxValue then
      Result := FMaxValue;
  end;
end;

initialization
  {$I rxspin.lrs}
end.
