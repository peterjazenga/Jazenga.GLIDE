{   Component(s):
    tcyBaseFlowPanel

    Description:
    A FlowPanel with Canvas, OnPaint, OnMouseEnter, OnMouseLeave, Multi-bevels properties
    
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
    * Donations: see Donation section on Description.txt
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
    


unit cyBaseFlowPanel;

{$MODE Delphi}

{$I cyCompilerDefines.inc}

interface

uses LCLIntf, LCLType, LMessages,
  Themes,
  cyBaseExtCtrls,
  Classes,  Graphics, StdCtrls, ExtCtrls, Controls, Messages, Dialogs,
  cyTypes, cyClasses, cyGraphics;

type

  TRunTimeDesignEvent = procedure (Sender: TObject; X, Y: Integer) of object;

  tcyBaseFlowPanel = class(TCustomFlowPanel)
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnPaint: TNotifyEvent;
    FBevels: TcyBevels;
    FShadow: TcyShadowText;
    FOnVisibleChanging: TNotifyEvent;
    FLayout: TTextLayout;
    FRunTimeDesign: TcyRunTimeDesign;
    FOnStartRunTimeDesign: TRunTimeDesignEvent;
    FOnDoRunTimeDesign: TRunTimeDesignEvent;
    FOnEndRunTimeDesign: TRunTimeDesignEvent;
    FWordWrap: Boolean;
    FCaptionOrientation: TCaptionOrientation;
    procedure CMMouseEnter(var Message :TLMessage); message CM_MOUSEENTER;   
    procedure CMMouseLeave(var Message :TLMessage); message CM_MOUSELEAVE;   
    procedure BevelsChange(Sender: TObject);
    procedure SubPropertiesChanged(Sender: TObject);
    procedure SetBevels(const Value: TcyBevels);
    procedure SetShadow(const Value: TcyShadowText);
    procedure SetLayout(const Value: TTextLayout);
    procedure SetRunTimeDesign(const Value: TcyRunTimeDesign);
    procedure SetWordWrap(const Value: Boolean);
    procedure SetCaptionOrientation(const Value: TCaptionOrientation);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure AdjustClientRect(var aRect: TRect); override;
    procedure Paint; override;
    procedure Loaded; override;
    procedure VisibleChanging; override;
    procedure DrawBackground(aRect: TRect); virtual;
    procedure DrawCaption(aRect: TRect); virtual;
    procedure MouseEnter(Sender: TObject); virtual;
    procedure MouseLeave(Sender: TObject); virtual;
    property Bevels: TcyBevels read FBevels write SetBevels;
    property CaptionOrientation: TCaptionOrientation read FCaptionOrientation write SetCaptionOrientation default coHorizontal;
    property Layout: TTextLayout read FLayout write SetLayout default tlCenter;
    property RunTimeDesign: TcyRunTimeDesign read FRunTimeDesign write SetRunTimeDesign;
    property Shadow: TcyShadowText read FShadow write SetShadow;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property OnVisibleChanging: TNotifyEvent read FOnVisibleChanging write FOnVisibleChanging;
    property OnStartRunTimeDesign: TRunTimeDesignEvent read FOnStartRunTimeDesign write FOnStartRunTimeDesign;
    property OnDoRunTimeDesign: TRunTimeDesignEvent read FOnDoRunTimeDesign write FOnDoRunTimeDesign;
    property OnEndRunTimeDesign: TRunTimeDesignEvent read FOnEndRunTimeDesign write FOnEndRunTimeDesign;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
  end;

implementation


constructor tcyBaseFlowPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBevels := TcyBevels.Create(self, TcyBevel);      
  FShadow := TcyShadowText.Create(self);            
  FRunTimeDesign := TcyRunTimeDesign.Create(Self);  

  BevelInner := bvNone;
  BevelOuter := bvNone;
  FLayout := tlCenter;
  FWordWrap := false;

  FBevels.OnChange := BevelsChange;               
  FShadow.OnChange := SubPropertiesChanged;       

  // Determine at design time if
  // the form is loading or if we have just added the component at design time :
  if csDesigning in ComponentState
  then
    if Owner <> nil
    then
      if not (csLoading in Owner.ComponentState)  // we have just added the component at design time
      then FBevels.Add;
end;

destructor tcyBaseFlowPanel.Destroy;
begin
  FRunTimeDesign.Free;
  FShadow.Free;
  FBevels.Free;
  FBevels := Nil;

  inherited Destroy;
end;

procedure tcyBaseFlowPanel.Loaded;
begin
  Inherited;
end;

procedure tcyBaseFlowPanel.BevelsChange(Sender: TObject);
begin
  if FBevels.NeedOwnerRealign then
    Realign;

  Invalidate;
end;

procedure tcyBaseFlowPanel.SubPropertiesChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure tcyBaseFlowPanel.AdjustClientRect(var aRect: TRect);
var w: Integer;
begin
  // inherited AdjustClientRect(aRect);

  w := BorderWidth + Bevels.BevelsWidth;
  InflateRect(aRect, -w, -w);

end;

procedure tcyBaseFlowPanel.Paint;
var Rect, TextRect: TRect;
begin
   inherited;
  Rect := GetClientRect;
  DrawBackground(Rect);
  InflateRect(Rect, -BorderWidth, -BorderWidth);

  if (Caption <> '')
  then begin
    TextRect := Rect;
    InflateRect(TextRect, -FBevels.BevelsWidth, -FBevels.BevelsWidth);
    DrawCaption(TextRect);
  end;

  FBevels.DrawBevels(Canvas, Rect, false);

  if Assigned(FOnPaint)
  then FOnPaint(Self);
end;

procedure tcyBaseFlowPanel.DrawBackground(aRect: TRect);
begin
  if not ThemeServices.ThemesEnabled or not ParentColor     
  then begin
    Canvas.Brush.Color := Color;
    Canvas.FillRect(aRect);
  end;
end;

procedure tcyBaseFlowPanel.DrawCaption(aRect: TRect);
var
  Flags: Longint;
  TmpFont: TFont;
begin
  with Canvas do
  begin
    Brush.Style := bsClear;
    Font := Self.Font;
    Flags := DrawTextFormatFlags(0, Alignment, FLayout, WordWrap);
  //  Flags := DrawTextBiDiModeFlags(Flags);    

    if FCaptionOrientation = coHorizontal
    then begin
      // Draw shadow caption :
      if Enabled and FShadow.Active
      then FShadow.DrawShadowText(Canvas, aRect, Caption, Alignment, FLayout, FCaptionOrientation, Flags);

      // Draw normal caption :
      cyDrawText(Handle, Caption, aRect, Flags);
    end
    else begin
      TmpFont := cyCreateFontIndirect(Font, FCaptionOrientation);
      try
        Font.Assign(TmpFont);

        // Draw shadow caption :
        if Enabled and FShadow.Active
        then FShadow.DrawShadowText(Canvas, aRect, Caption, Alignment, FLayout, FCaptionOrientation, Flags);

        // Draw normal caption :
        cyDrawVerticalText(Canvas, Caption, aRect, Flags, FCaptionOrientation, Alignment, FLayout);
      finally
        TmpFont.Free;
      end;
    end;
  end;
end;

procedure tcyBaseFlowPanel.SetBevels(const Value: TcyBevels);
begin
  FBevels := Value;
end;

procedure tcyBaseFlowPanel.SetCaptionOrientation(const Value: TCaptionOrientation);
begin
  if FCaptionOrientation = Value then Exit;

  FCaptionOrientation := Value;
  Invalidate;

  if (csDesigning in ComponentState) and (not (csLoading in ComponentState))
  then
    if (FCaptionOrientation <> coHorizontal) and CaptionOrientationWarning
    then begin
      CaptionOrientationWarning := false;
      ShowMessage(cCaptionOrientationWarning);
    end;
end;

procedure tcyBaseFlowPanel.SetLayout(const Value: TTextLayout);
begin
  FLayout := Value;
  Invalidate;
end;

procedure tcyBaseFlowPanel.SetRunTimeDesign(const Value: TcyRunTimeDesign);
begin
  FRunTimeDesign := Value;
end;

procedure tcyBaseFlowPanel.SetShadow(const Value: TcyShadowText);
begin
  FShadow := Value;
end;

procedure tcyBaseFlowPanel.SetWordWrap(const Value: Boolean);
begin
  FWordWrap := Value;
  Invalidate;
end;

procedure tcyBaseFlowPanel.VisibleChanging;
begin
  inherited;
  if Assigned(FOnVisibleChanging)
  then FOnVisibleChanging(Self);
end;

procedure tcyBaseFlowPanel.CMMouseEnter(var Message :TLMessage);
begin
  inherited;
  MouseEnter(Self);
end;

procedure tcyBaseFlowPanel.CMMouseLeave(var Message :TLMessage);
begin
  inherited;
  MouseLeave(Self);
end;

procedure tcyBaseFlowPanel.MouseEnter(Sender: TObject);
begin
  if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
end;

procedure tcyBaseFlowPanel.MouseLeave(Sender: TObject);
begin
  if Assigned(FonMouseLeave) then FOnMouseLeave(Self);
end;

procedure tcyBaseFlowPanel.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  FRunTimeDesign.StartJob(X, Y);

  if FRunTimeDesign.Job <> rjNothing
  then
    if Assigned(FOnStartRunTimeDesign)
    then FOnStartRunTimeDesign(Self, X, Y);

  inherited;
end;

procedure tcyBaseFlowPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if FRunTimeDesign.Job <> rjNothing
  then
    if Assigned(FOnDoRunTimeDesign)
    then FOnDoRunTimeDesign(Self, X, Y);

  FRunTimeDesign.DoJob(X, Y);

  inherited;
end;

procedure tcyBaseFlowPanel.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FRunTimeDesign.Job <> rjNothing
  then
    if Assigned(FOnEndRunTimeDesign)
    then FOnEndRunTimeDesign(Self, X, Y);

  FRunTimeDesign.EndJob(X, Y);
  inherited;
end;

end.
