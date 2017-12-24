{ RxTimeEdit unit

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

unit RxTimeEdit;

{$I rx.inc}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, rxspin,
  MaskEdit, LMessages, LCLType;

type

  { TCustomRxTimeEdit }

  TCustomRxTimeEdit = class(TCustomMaskEdit)
  private
    FButton: TRxSpinButton;
    FButtonNeedsFocus: Boolean;
    FOnButtonClick : TNotifyEvent;
    procedure CheckButtonVisible;
    function GetButtonHint: TTranslateString;
    function GetTime: TTime;
    procedure SetButtonHint(const AValue: TTranslateString);
    procedure SetButtonNeedsFocus(const AValue: Boolean);
    procedure SetTime(const AValue: TTime);
    procedure DoChangeValue(AValue:integer);
    procedure WMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
    procedure WMKillFocus(var Message: TLMKillFocus); message LM_KILLFOCUS;
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure DoPositionButton; virtual;
    procedure UpClick(Sender: TObject); virtual;
    procedure DownClick(Sender: TObject); virtual;
    property ButtonOnlyWhenFocused: Boolean read FButtonNeedsFocus write SetButtonNeedsFocus default False;
    property OnButtonClick : TNotifyEvent read FOnButtonClick write FOnButtonClick;
    property ButtonHint: TTranslateString read GetButtonHint write SetButtonHint;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Time:TTime read GetTime write SetTime;
  end;

type
  TRxTimeEdit = class(TCustomRxTimeEdit)
  published
    property AutoSize;
    property AutoSelect;
    property Align;
    property Anchors;
    property BorderSpacing;
    property ButtonOnlyWhenFocused;
    property ButtonHint;
    property CharCase;
    property Color;
//    property DirectInput;
    property DragCursor;
    property DragMode;
    property EchoMode;
    property Enabled;
//    property Flat;
    property Font;
//    property Glyph;
    property MaxLength;
//    property NumGlyphs;
    property OnButtonClick;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
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
    property OnStartDrag;
    property OnUTF8KeyPress;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
  end;

implementation


{ TCustomRxTimeEdit }

procedure TCustomRxTimeEdit.CheckButtonVisible;
begin
  If Assigned(FButton) then
    FButton.Visible:=(csdesigning in ComponentState) or
                     (Visible and (Focused or not FButtonNeedsFocus));
end;

function TCustomRxTimeEdit.GetButtonHint: TTranslateString;
begin
  Result:=FButton.Hint;
end;

function TCustomRxTimeEdit.GetTime: TTime;
begin
  Result:=StrToTimeDef(Text, 0);
end;

procedure TCustomRxTimeEdit.SetButtonHint(const AValue: TTranslateString);
begin
  if AValue = '' then
    FButton.Hint:=Hint
  else
    FButton.Hint:=AValue;
end;

procedure TCustomRxTimeEdit.SetButtonNeedsFocus(const AValue: Boolean);
begin
  if FButtonNeedsFocus<>AValue then
  begin
    FButtonNeedsFocus:=AValue;
    CheckButtonVisible;
  end;
end;

procedure TCustomRxTimeEdit.SetTime(const AValue: TTime);
begin
  Text:=TimeToStr(AValue);
end;

procedure TCustomRxTimeEdit.DoChangeValue(AValue: integer);
var
  S:ShortString;
  H1, M2, S3:Integer;
  i,p:integer;

procedure IncHour;
begin
  H1:=H1+AValue;
  if H1>23 then
    H1:=0
  else
  if H1<0 then
    H1:=23;
end;

procedure IncMin;
begin
  M2:=M2+AValue;
  if M2>59 then
    M2:=0
  else
  if M2<0 then
    M2:=59
  else
    exit;
  IncHour;
end;

procedure IncSec;
begin
  S3:=S3+AValue;
  if S3>59 then
    S3:=0
  else
  if S3<0 then
    S3:=59
  else
    exit;
  IncMin;
end;

begin
  S:=Text;
  for i:=1 to Length(S) do
    if S[i]=' ' then
      S[i]:='0';

  H1:=StrToInt(S[1]+S[2]);
  M2:=StrToInt(S[4]+S[5]);
  S3:=StrToInt(S[7]+S[8]);
  P:=GetSelStart;
  if P < 3 then IncHour
  else
  if P < 6 then IncMin
  else IncSec;
  Text:=Format('%2.2d'+ DefaultFormatSettings.TimeSeparator +'%2.2d'+ DefaultFormatSettings.TimeSeparator +'%2.2d', [H1, M2, S3]);
  SetSelStart(P);
end;

procedure TCustomRxTimeEdit.WMSetFocus(var Message: TLMSetFocus);
begin
  FButton.Visible:=True;
  inherited;
end;

procedure TCustomRxTimeEdit.WMKillFocus(var Message: TLMKillFocus);
begin
  if FButtonNeedsFocus then
    FButton.Visible:=False;
  inherited;
end;

procedure TCustomRxTimeEdit.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if FButton <> nil then
  begin
    DoPositionButton;
    CheckButtonVisible;
  end;
end;

procedure TCustomRxTimeEdit.DoPositionButton;
begin
  if FButton = nil then exit;
  FButton.Parent := Parent;
  FButton.Visible := Visible;
  FButton.AnchorToCompanion(akLeft,0,Self);
//  if FButton.Width = 0 then
    FButton.Width:=26;//Height;
end;

procedure TCustomRxTimeEdit.UpClick(Sender: TObject);
begin
  if not ReadOnly then
  begin
    DoChangeValue(1);
    if Assigned(FOnButtonClick) then
      FOnButtonClick(Self);
  end;
end;

procedure TCustomRxTimeEdit.DownClick(Sender: TObject);
begin
  if not ReadOnly then
  begin
    DoChangeValue(-1);
    if Assigned(FOnButtonClick) then
      FOnButtonClick(Self);
  end;
end;

constructor TCustomRxTimeEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButton := TRxSpinButton.Create(Self);
  FButton.FocusControl := Self;
  FButton.Width := Self.Height;
  FButton.Height := Self.Height;
  FButton.FreeNotification(Self);
  FButton.ControlStyle := FButton.ControlStyle + [csNoDesignSelectable];
  FButton.OnTopClick := @UpClick;
  FButton.OnBottomClick := @DownClick;

  EditMask:='!#0'+DefaultFormatSettings.TimeSeparator + '00'+DefaultFormatSettings.TimeSeparator + '00;1;_';
end;

destructor TCustomRxTimeEdit.Destroy;
begin
  if FButton <> nil then
    FreeAndNil(FButton);
  inherited Destroy;
end;

end.
