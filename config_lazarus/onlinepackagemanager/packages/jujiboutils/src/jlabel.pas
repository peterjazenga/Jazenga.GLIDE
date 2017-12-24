{ TJLabel

  Copyright (C) 2012 Julio Jiménez Borreguero
  Contact: jujibo at gmail dot com

  This library is free software; you can redistribute it and/or modify it
  under the same terms as the Lazarus Component Library (LCL)

  See the file license-jujiboutils.txt and COPYING.LGPL, included in this distribution,
  for details about the license.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}

unit JLabel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  LMessages;

type

  TLabelStyle = (lsNone, lsRaised, lsRecessed, lsShadow, lsShadow2, lsCustom);

  { TJCustomLabel }

  TJCustomLabel = class(TCustomLabel)
  private
    { Private declarations }
    FLabelStyle: TLabelStyle;
    FShadowColor: TColor;
    FShadowColor2: TColor;
    procedure SetLabelStyle(AValue: TLabelStyle);
    procedure SetShadowColor2(AValue: TColor);
  protected
    { Protected declarations }
    procedure SetShadowColor(Value: TColor);
    procedure Paint; override;

    property ShadowColor: TColor read FShadowColor write SetShadowColor;
    property ShadowColor2: TColor read FShadowColor2 write SetShadowColor2;
    property LabelStyle: TLabelStyle read FLabelStyle write SetLabelStyle;
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    property Caption;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FocusControl;
    property Font;
    property Layout;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property Transparent;
    property Visible;
    property WordWrap;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnChangeBounds;
    property OnContextPopup;
    property OnResize;
    property OnStartDrag;
    property OptimalFill;
  public
    { Public declarations }
    constructor Create(TheOwner: TComponent); override;
  end;

  TJLabel = class(TJCustomLabel)
  published
    property ShadowColor;
    property ShadowColor2;
    property LabelStyle;
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    property Caption;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FocusControl;
    property Font;
    property Layout;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property Transparent;
    property Visible;
    property WordWrap;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnChangeBounds;
    property OnContextPopup;
    property OnResize;
    property OnStartDrag;
    property OptimalFill;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I jlabel_icon.lrs}
  RegisterComponents('Jujibo', [TJLabel]);
end;

{ TJCustomLabel }

procedure TJCustomLabel.SetShadowColor2(AValue: TColor);
begin
  if FShadowColor2 <> AValue then
  begin
    FLabelStyle := lsCustom;
    FShadowColor2 := AValue;
    Perform(CM_COLORCHANGED, 0, 0);
    Invalidate;
  end;
end;

procedure TJCustomLabel.SetLabelStyle(AValue: TLabelStyle);
begin
  if FLabelStyle = AValue then
    Exit;
  FLabelStyle := AValue;
  case FLabelStyle of
    lsNone:
    begin
      FShadowColor := clNone;
      FShadowColor2 := clNone;
    end;
    lsRaised:
    begin
      FShadowColor := clBtnHighlight;
      FShadowColor2 := clBtnShadow;
    end;
    lsRecessed:
    begin
      FShadowColor := clBtnShadow;
      FShadowColor2 := clBtnHighlight;
    end;
    lsShadow:
    begin
      FShadowColor := clBtnHighlight;
      FShadowColor2 := clNone;
    end;
    lsShadow2:
    begin
      FShadowColor := clNone;
      FShadowColor2 := clBtnHighlight;
    end;
  end;
  Invalidate;
end;

procedure TJCustomLabel.SetShadowColor(Value: TColor);
begin
  if FShadowColor <> Value then
  begin
    FLabelStyle := lsCustom;
    FShadowColor := Value;
    Perform(CM_COLORCHANGED, 0, 0);
    Invalidate;
  end;
end;

procedure TJCustomLabel.Paint;
var
  TR: TTextStyle;
  R, R1, R2: TRect;
  TextLeft, TextTop: integer;
  LabelText: string;
  OldFontColor: TColor;
  Color1, Color2: TColor;
begin
  R := Rect(0, 0, Width, Height);
  R1 := Rect(1, 1, Width + 1, Height + 1);  //  Shadow
  R2 := Rect(-1, -1, Width - 1, Height - 1); // Shadow2

  with Canvas do
  begin
    Brush.Color := Self.Color;
    if (Color <> clNone) and not Transparent then
    begin
      Brush.Style := bsSolid;
      FillRect(R);
    end;
    Brush.Style := bsClear;
    Font := Self.Font;

    FillChar(TR, SizeOf(TR), 0);
    with TR do
    begin
      Alignment := BidiFlipAlignment(Self.Alignment, UseRightToLeftAlignment);
      Layout := Self.Layout;
      Opaque := (Color <> clNone) and not Transparent;
      WordBreak := wordWrap;
      SingleLine := not WordWrap and not HasMultiLine;
      Clipping := True;
      ShowPrefix := ShowAccelChar;
      SystemFont := False;
      RightToLeft := UseRightToLeftReading;
      ExpandTabs := True;
    end;
    DoMeasureTextPosition(TextTop, TextLeft);
    //debugln('TCustomLabel.Paint ',dbgs(Alignment=tacenter),' ',dbgs(Layout=tlCenter),' ',dbgs(TextLeft),' TextTop=',dbgs(TextTop),' ',dbgs(R));
    LabelText := GetLabelText;
    OldFontColor := Font.Color;
    if not IsEnabled then
    begin
      Font.Color := clBtnHighlight;
      if (Layout <> tlTop) then
        TextRect(R1, TextLeft, TextTop, LabelText, TR)
      else
        TextRect(R, TextLeft + 1, TextTop + 1, LabelText, TR);
      Font.Color := clBtnShadow;
    end;
    if ShadowColor2 <> clNone then
    begin
      Font.Color := ShadowColor2;
      if (Layout <> tlTop) then
        TextRect(R2, TextLeft, TextTop, LabelText, TR)
      else
        TextRect(R, TextLeft - 1, TextTop - 1, LabelText, TR);
      Font.Color := OldFontColor;
    end;
    if ShadowColor <> clNone then
    begin
      Font.Color := ShadowColor;
      if (Layout <> tlTop) then
        TextRect(R1, TextLeft, TextTop, LabelText, TR)
      else
        TextRect(R, TextLeft + 1, TextTop + 1, LabelText, TR);
      Font.Color := OldFontColor;
    end;
    TextRect(R, TextLeft, TextTop, LabelText, TR);
  end;
end;

constructor TJCustomLabel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  LabelStyle := lsRaised;
end;

end.
