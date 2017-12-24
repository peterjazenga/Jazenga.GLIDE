{   Component(s):
    tcyBaseLabel

    Description:
    Base for Label components


    * ***** BEGIN LICENSE BLOCK *****
    *
    * Version: MPL 1.1
    *
    * The contents of this file are subject to the Mozilla Public License Version
    * 1.1 (the "License"); you may not use this file except in compliance with the
    * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
    *
    * Software distributed under the License is distributed on an "AS IS" basis,
    * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
    * the specific language governing rights and limitations under the License.
    *
    * The Initial Developer of the Original Code is Mauricio
    * (https://sourceforge.net/projects/tcycomponents/).
    *
    * Alternatively, the contents of this file may be used under the terms of
    * either the GNU General Public License Version 2 or later (the "GPL"), or the
    * GNU Lesser General Public License Version 2.1 or later (the "LGPL"), in which
    * case the provisions of the GPL or the LGPL are applicable instead of those
    * above. If you wish to allow use of your version of this file only under the
    * terms of either the GPL or the LGPL, and not to allow others to use your
    * version of this file under the terms of the MPL, indicate your decision by
    * deleting the provisions above and replace them with the notice and other
    * provisions required by the LGPL or the GPL. If you do not delete the
    * provisions above, a recipient may use your version of this file under the
    * terms of any one of the MPL, the GPL or the LGPL.
    *
    * ***** END LICENSE BLOCK *****}
    


unit cyBaseLabel;

{$MODE Delphi}

interface

uses LCLIntf, LCLType, LMessages,
  cyTypes, cyClasses, cyGraphics, StdCtrls, Graphics, classes,  Controls, Dialogs;

  type
  TcyBaseLabel = class(TCustomLabel)
  private
    FIndentLeft: Word;
    FIndentRight: Word;
    FIndentTop: Word;
    FIndentBottom: Word;
    FCaptionRender: TCaptionRender;
    FCaptionOrientation: TCaptionOrientation;
    FShadow: TcyShadowText;
    procedure SetIndentLeft(AValue: Word);
    procedure SetIndentRight(AValue: Word);
    procedure SetIndentTop(AValue: Word);
    procedure SetIndentBottom(AValue: Word);
    procedure SetCaptionRender(const Value: TCaptionRender);
    procedure SetCaptionOrientation(const Value: TCaptionOrientation);
    procedure SetShadow(const Value: TcyShadowText);
  protected
    procedure DoDrawText(arect:Trect;aDrawStyle: Longint);  
    procedure DrawBackground(aRect: TRect); virtual;
    procedure DrawCaption(aRect: TRect); virtual;
    procedure SubPropertiesChanged(Sender: TObject);
    procedure Paint; override;
    procedure AdjustBounds; //override;
    property CaptionIndentLeft: Word read FIndentLeft write SetIndentLeft default 0;
    property CaptionIndentRight: Word read FIndentRight write SetIndentRight default 0;
    property CaptionIndentTop: Word read FIndentTop write SetIndentTop default 0;
    property CaptionIndentBottom: Word read FIndentBottom write SetIndentBottom default 0;
    property CaptionRender: TCaptionRender read FCaptionRender write SetCaptionRender default crNormal;
    property CaptionOrientation: TCaptionOrientation read FCaptionOrientation write SetCaptionOrientation default coHorizontal;
    property Shadow: TcyShadowText read FShadow write SetShadow;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
  end;

implementation

uses Types;

constructor TcyBaseLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FShadow := TcyShadowText.Create(self);
  FShadow.OnChange := SubPropertiesChanged;
  FIndentLeft := 0;
  FIndentRight := 0;
  FIndentTop := 0;
  FIndentBottom := 0;
  FCaptionRender := crNormal;
  Transparent := false;
  Autosize := false;
end;

destructor TcyBaseLabel.Destroy;
begin
  FShadow.Free;
  inherited Destroy;
end;

procedure TcyBaseLabel.SubPropertiesChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TcyBaseLabel.DrawBackground(aRect: TRect);
begin
  if not Transparent
  then begin
    Canvas.Brush.Color := Color;
    Canvas.Brush.Style := bsSolid;
    Canvas.FillRect(aRect);
  end;
end;

procedure TcyBaseLabel.DrawCaption(aRect: TRect);
var
  Text: String;
  CalcRect, ShadowRect: TRect;
  DrawStyle: Longint;
  TmpFont: TFont;
  SauvOnChange: TNotifyEvent;
  SauvColor: TColor;
  SauvHeight: Integer;
begin
  Canvas.Brush.Style := bsClear;

  if FCaptionOrientation = coHorizontal
  then begin
    { DoDrawText takes care of BiDi alignments }
    DrawStyle := DT_EXPANDTABS or WordWraps[WordWrap] or Alignments[Alignment] or CaptionRenders[FCaptionRender];

    { Calculate vertical layout }
    if Layout <> tlTop then
    begin
      CalcRect := aRect;
      DoDrawText(CalcRect, DrawStyle or DT_CALCRECT);
      if Layout = tlBottom
      then OffsetRect(aRect, 0, Height - CalcRect.Bottom)
      else OffsetRect(aRect, 0, (Height - CalcRect.Bottom) div 2);
    end;

    // Draw shadow caption :
    if Enabled and FShadow.Active
    then begin
      ShadowRect := FShadow.CalcShadowRect(aRect, Alignment, tlTop, Font.Height);
      SauvOnChange := self.Font.OnChange;
      self.Font.OnChange := nil;
      SauvColor := Font.Color;
      Font.Color := FShadow.Color;
      SauvHeight := Font.Height;
      Font.Height := Round(Font.Height * FShadow.ZoomPercent / 100);
      DoDrawText(ShadowRect, DrawStyle);
      // Restore defs in order to default text:
      Font.Color := SauvColor;
      Font.Height := SauvHeight;
      Font.OnChange := SauvOnChange;
    end;

    // Draw normal caption :
    DoDrawText(aRect, DrawStyle);
  end
  else begin
    Text := GetLabelText;
    DrawStyle := DT_EXPANDTABS or DT_SINGLELINE or CaptionRenders[FCaptionRender];
    if (DrawStyle and DT_CALCRECT <> 0) and ((Text = '') or ShowAccelChar and
      (Text[1] = '&') and (Text[2] = #0)) then Text := Text + ' ';
    if not ShowAccelChar then DrawStyle := DrawStyle or DT_NOPREFIX;

   // DrawStyle := DrawTextBiDiModeFlags(DrawStyle);  

    TmpFont := cyCreateFontIndirect(Font, FCaptionOrientation);
    try
      Canvas.Font.Assign(TmpFont);

      // Draw shadow :
      if Enabled and FShadow.Active
      then begin
        ShadowRect := FShadow.CalcShadowRect(aRect, Alignment, tlTop, Font.Height);
        Canvas.Font.Color := FShadow.Color;
        Canvas.Font.Height := Round(Font.Height * FShadow.ZoomPercent / 100);
        cyDrawVerticalText(Canvas, Text, ShadowRect, DrawStyle, FCaptionOrientation, Alignment, Layout);
        // Restore defs in order to default text:
        Canvas.Font.Color := Font.Color;
        Canvas.Font.Height := Font.Height;
      end;

      // Draw caption :
      cyDrawVerticalText(Canvas, Text, aRect, DrawStyle, FCaptionOrientation, Alignment, Layout);
    finally
      TmpFont.Free;
    end;
  end;
end;

procedure TcyBaseLabel.Paint;
var Rect: TRect;
begin
  Rect := ClientRect;
  DrawBackground(Rect);
  Rect := Classes.Rect(Rect.Left + FIndentLeft, Rect.Top + FIndentTop,
    Rect.Right - FIndentRight, Rect.Bottom - FIndentBottom);
  DrawCaption(Rect);
end;

procedure TcyBaseLabel.AdjustBounds;
const
  WordWraps: array[Boolean] of Word = (0, DT_WORDBREAK);
var
  DC: HDC;
  X, _Width, _Height: Integer;
  Rect: TRect;
  AAlignment: TAlignment;
begin
  if (AutoSize) and (csReading in ComponentState = false)
  then begin
    Rect := ClientRect;
    DC := GetDC(0);
    Canvas.Handle := DC;

    if FCaptionOrientation = coHorizontal
    then DoDrawText(Rect, (DT_EXPANDTABS or DT_CALCRECT) or WordWraps[WordWrap])
    else DoDrawText(Rect, DT_SINGLELINE or DT_EXPANDTABS or DT_CALCRECT);

    if FCaptionOrientation in [coHorizontal, coHorizontalReversed]
    then begin
      _Width := Rect.Right;
      _Height := Rect.Bottom;
    end
    else begin
      _Width := Rect.Bottom;
      _Height := Rect.Right;
    end;

    Canvas.Handle := 0;
    ReleaseDC(0, DC);
    X := Left;
    AAlignment := Alignment;
    if UseRightToLeftAlignment then ChangeBiDiModeAlignment(AAlignment);
    if AAlignment = taRightJustify then Inc(X, Width - Rect.Right);

    SetBounds(X, Top, _Width + FIndentRight + FIndentLeft, _Height + FIndentBottom + FIndentTop);
  end;
end;

procedure TcyBaseLabel.SetIndentLeft(AValue: Word);
begin
  if AValue <> FIndentLeft
  then begin
    FIndentLeft:= AValue;
    if AutoSize
    then AdjustBounds
    else Invalidate;
  end;
end;

procedure TcyBaseLabel.SetIndentRight(AValue: Word);
begin
  if AValue <> FIndentRight
  then begin
    FIndentRight:= AValue;
    if AutoSize
    then AdjustBounds
    else Invalidate;
  end;
end;

procedure TcyBaseLabel.SetIndentTop(AValue: Word);
begin
  if AValue <> FIndentTop
  then begin
    FIndentTop:= AValue;
    if AutoSize
    then AdjustBounds
    else Invalidate;
  end;
end;

procedure TcyBaseLabel.SetShadow(const Value: TcyShadowText);
begin
  FShadow := Value;
end;

procedure TcyBaseLabel.SetIndentBottom(AValue: Word);
begin
  if AValue <> FIndentBottom
  then begin
    FIndentBottom:= AValue;
    if AutoSize
    then AdjustBounds
    else Invalidate;
  end;
end;

procedure TcyBaseLabel.SetCaptionRender(const Value: TCaptionRender);
begin
  FCaptionRender := Value;
  Invalidate;
end;

procedure TcyBaseLabel.SetCaptionOrientation(const Value: TCaptionOrientation);
begin
  if FCaptionOrientation = Value then Exit;
  FCaptionOrientation := Value;

  if AutoSize
  then AdjustBounds;

  Invalidate;         // Always invalidate (bug report on 27/07/2010)

  if (csDesigning in ComponentState) and (not (csLoading in ComponentState))
  then
    if (FCaptionOrientation <> coHorizontal) and CaptionOrientationWarning
    then begin
      CaptionOrientationWarning := false;
      ShowMessage(cCaptionOrientationWarning);
    end;
end;

procedure TcyBaseLabel.DoDrawText(arect:Trect;aDrawStyle: Longint);
begin
  DrawText(self.Canvas.Handle,PChar(caption),length(caption),arect, aDrawStyle);
end;

end.
