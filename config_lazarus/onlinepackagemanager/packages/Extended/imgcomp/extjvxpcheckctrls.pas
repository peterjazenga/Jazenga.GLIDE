{-----------------------------------------------------------------------------
Traduction to LAZARUS : Matthieu GIROUX
http://matthieu.giroux.free.fr
http://developpement.rapide.free.fr
matthieu.giroux@free.fr

Version : 1.0.0.2
Convert XPM to BMP
Version : 1.0.0.1
Working at execution and implementation

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvXPCheckCtrls.PAS, released on 2004-01-01.

The Initial Developer of the Original Code is Marc Hoffman.
Portions created by Marc Hoffman are Copyright (C) 2002 APRIORI business solutions AG.
Portions created by APRIORI business solutions AG are Copyright (C) 2002 APRIORI business solutions AG
All Rights Reserved.

Contributor(s):
     ZENSan : State and AllowGrayed properties
     Anudedeus (Alexandre Pranke) : State and AllowGrayed properties

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id: JvXPCheckCtrls.pas 11400 2007-06-28 21:24:06Z ahuser $

unit ExtJvXPCheckCtrls;

{$MODE Delphi}


interface

uses
  LCLIntf, lresources, Classes, Controls, JvXPCore,
  StdCtrls,
  JvXPCoreUtils, ExtJvXPCoreUtils, LCLType, Graphics;

type

  { TJvXPCustomCheckControl }

  TJvXPCustomCheckControl = class(TJvXPCustomControl)
  private
    FBgGradient: TBitmap;
    FBoundLines: TJvXPBoundLines;
    FChecked: boolean;
    FCheckSize: byte;
    FCkGradient: TBitmap;
    FHlGradient: TBitmap;
    FSpacing: byte;
    FState: TCheckBoxState;
    FAllowGrayed: boolean;
    procedure SetState(const Value: TCheckBoxState);
    procedure SetAllowGrayed(const Value: boolean);
  protected
    procedure SetBoundLines(Value: TJvXPBoundLines); virtual;
    procedure SetChecked(Value: boolean); virtual;
    procedure SetSpacing(Value: byte); virtual;
    procedure DrawCheckSymbol(const R: TRect); virtual; abstract;
    procedure Click; override;
    procedure Paint; override;
    procedure HookResized; override;
    procedure KeyDown(var Key: word; Shift: TShiftState); override;
    property BoundLines: TJvXPBoundLines read FBoundLines write SetBoundLines default [];
    property AllowGrayed: boolean read FAllowGrayed write SetAllowGrayed default False;
    property Checked: boolean read FChecked write SetChecked default False;
    property Spacing: byte read FSpacing write SetSpacing default 3;
    property State: TCheckBoxState read FState write SetState default cbUnchecked;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TJvXPCheckbox = class(TJvXPCustomCheckControl)
  protected
    procedure DrawCheckSymbol(const R: TRect); override;
  published
    // common properties.
    property Caption;
    property AllowGrayed;
    property Enabled;
    property TabOrder;
    property TabStop default True;
    // advanced properties.
    property BoundLines;
    property Checked;
    property Spacing;
    property ParentColor;
    property State;
    property Color;
    //property BevelInner;
    //property BevelOuter;
    //property BevelWidth;
    property BiDiMode;
    {$IFNDEF FPC}
    property Ctl3D;
    property ParentCtl3D;
    property TabOrder;
    property TabStop;
    property Enabled;
    {$ENDIF}
    property DockSite;
    property ParentBiDiMode;
    property UseDockManager default True;
    property Align;
    property Anchors;
    property AutoSize;
    property Constraints;
    property DragCursor;
    property DragKind;
    //    property OnCanResize;
    property DragMode;
    property Name;
    property Font;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    //    property Style;
    //    property StyleManager;
    property Visible;
    property OnDockDrop;
    property OnDockOver;
    property OnEndDock;
    property OnGetSiteInfo;
    property OnStartDock;
    property OnUnDock;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
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
    property OnStartDrag;
  end;

{$IFDEF USEJVCL}
{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jvcl.svn.sourceforge.net/svnroot/jvcl/branches/JVCL3_33_PREPARATION/run/JvXPCheckCtrls.pas $';
    Revision: '$Revision: 11400 $';
    Date: '$Date: 2007-06-28 23:24:06 +0200 (jeu., 28 juin 2007) $';
    LogPath: 'JVCL\run'
    );
{$ENDIF UNITVERSIONING}
{$ENDIF USEJVCL}

implementation

//=== { TJvXPCustomCheckControl } ============================================

constructor TJvXPCustomCheckControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // set default properties.
  //  ControlStyle := ControlStyle - [csDoubleClicks];
  Height := 17;
  TabStop := True;
  Width := 161;

  // set custom properties.
  FBoundLines := [];
  FChecked := False;
  FCheckSize := 13;
  FSpacing := 3;

  // create ...
  FBgGradient := TBitmap.Create; // background gradient
  FCkGradient := TBitmap.Create; // clicked gradient
  FHlGradient := TBitmap.Create; // Highlight gradient
end;

destructor TJvXPCustomCheckControl.Destroy;
begin
  FBgGradient.Free;
  FCkGradient.Free;
  FHlGradient.Free;
  inherited Destroy;
end;

procedure TJvXPCustomCheckControl.Click;
begin
  if not AllowGrayed then
    Checked := not Checked
  else
    State := TCheckBoxState((byte(State) + 1) mod 3);
  inherited Click;
end;

procedure TJvXPCustomCheckControl.HookResized;
begin
  // create gradient rectangles for...

  // background.
  JvXPCreateGradientRect(FCheckSize - 2, FCheckSize - 2, dxColor_Btn_Enb_BgFrom_WXP,
    dxColor_Btn_Enb_BgTo_WXP, 16, gsTop, False, FBgGradient);

  // clicked.
  JvXPCreateGradientRect(FCheckSize - 2, FCheckSize - 2, dxColor_Btn_Enb_CkFrom_WXP,
    dxColor_Btn_Enb_CkTo_WXP, 16, gsTop, True, FCkGradient);

  // highlight.
  JvXPCreateGradientRect(FCheckSize - 2, FCheckSize - 2, dxColor_Btn_Enb_HlFrom_WXP,
    dxColor_Btn_Enb_HlTo_WXP, 16, gsTop, True, FHlGradient);

  LockedInvalidate;
end;

procedure TJvXPCustomCheckControl.KeyDown(var Key: word; Shift: TShiftState);
begin
  case Key of
    VK_SPACE:
      Checked := not Checked;
    end;
  inherited KeyDown(Key, Shift);
end;

procedure TJvXPCustomCheckControl.SetAllowGrayed(const Value: boolean);
begin
  FAllowGrayed := Value;
  if Value = False then
    if FState = cbGrayed then
      begin
      State := cbUnchecked;
      LockedInvalidate;
      end;
end;

procedure TJvXPCustomCheckControl.SetBoundLines(Value: TJvXPBoundLines);
begin
  if Value <> FBoundLines then
    begin
    FBoundLines := Value;
    LockedInvalidate;
    end;
end;

procedure TJvXPCustomCheckControl.SetChecked(Value: boolean);
begin
  if Value <> FChecked then
    begin
    FChecked := Value;
    if Value then
      FState := cbChecked
    else
      FState := cbUnchecked;
    LockedInvalidate;
    end;
end;

procedure TJvXPCustomCheckControl.SetSpacing(Value: byte);
begin
  if Value <> FSpacing then
    begin
    FSpacing := Value;
    LockedInvalidate;
    end;
end;

procedure TJvXPCustomCheckControl.SetState(const Value: TCheckBoxState);
begin
  // will not change FState if FAllowGrayed = false and passed Value is cbGrayed
  if (FState <> Value) and (FAllowGrayed or (Value <> cbGrayed)) then
    begin
    FState := Value;
    if FState = cbChecked then
      FChecked := True
    else
      FChecked := False;
    LockedInvalidate;
    end;
end;

procedure TJvXPCustomCheckControl.Paint;
var
  Rect: TRect;
  BoundColor: TColor;
  Trans: integer;
begin
  with Canvas do
    begin
    // clear background.
    Rect := GetClientRect;
    Brush.Color := Self.Color;
    FillRect(Rect);
    // draw designtime rect.
    if csDesigning in ComponentState then
      DrawFocusRect(Rect);

    // draw boundlines.
    if BoundLines <> [] then
      begin
      //      if Style.GetTheme = WindowsXP then
      //        BoundColor := dxColor_Btn_Enb_Border_WXP
      //      else
      BoundColor := dxColor_DotNetFrame;
      JvXPDrawBoundLines(Self.Canvas, BoundLines, BoundColor, Rect);
      end;

    // draw focusrect.
    if dsFocused in DrawState then
      begin
      Brush.Style := bsSolid;
      DrawFocusRect(Rect);
      end;

    // draw check symbol.
    DrawCheckSymbol(Rect);

    // draw caption.

    Trans := Transparent;
    SetBkMode(Handle, Trans);
    Font.Assign(Self.Font);
    if BiDiMode = bdRightToLeft then
      begin
      Dec(Rect.Right, FCheckSize + 4 + Spacing);
      JvXPPlaceText(Self, Canvas, Caption, Font, Enabled, True,
        taRightJustify, True, Rect);
      end
    else
      begin
      Inc(Rect.Left, FCheckSize + 4 + Spacing);
      JvXPPlaceText(Self, Canvas, Caption, Font, Enabled, True,
        taLeftJustify, True, Rect);
      end;
    end;
end;


procedure CreateCheck (const x,y:Integer; var apolygon :  Array of TPoint );
Begin
  apolygon [ 0 ].X := x;
  apolygon [ 0 ].y := y+2;
  apolygon [ 1 ].X := x+2;
  apolygon [ 1 ].y := y+4;
  apolygon [ 2 ].X := x+6;
  apolygon [ 2 ].y := y;
  apolygon [ 3 ].X := x+6;
  apolygon [ 3 ].y := y+2;
  apolygon [ 4 ].X := x+2;
  apolygon [ 4 ].y := y+6;
  apolygon [ 5 ].X := x;
  apolygon [ 5 ].y := y+4;
end;

//=== { TJvXPCheckbox } ======================================================

procedure TJvXPCheckbox.DrawCheckSymbol(const R: TRect);
var
  ClipW: integer;
  ap_Polygon : Array [0..5] of TPoint ;
  //  Theme: TJvXPTheme;

  procedure DrawGradient(const Bitmap: TBitmap);
  begin
{    if BiDiMode = bdRightToLeft then
      BitBlt(Canvas.Handle, R.Right - 1 - FCheckSize, (ClientHeight - FCheckSize) div 2 + 1,
        FCheckSize - 2, FCheckSize - 2, Bitmap.Canvas.Handle, 0, 0, SRCCOPY)
    else}
    BitBlt(Canvas.Handle, R.Left + 3, (ClientHeight - FCheckSize) div 2 + 1,
      FCheckSize - 2, FCheckSize - 2, Bitmap.Canvas.Handle, 0, 0, SRCCOPY);
  end;

begin
  // get current theme.
  //  Theme := Style.GetTheme;

  with Canvas do
    begin
    // check for highlight.
    ClipW := Ord(dsHighlight in DrawState);

    // draw border.
{    if (Theme = WindowsXP) or ((Theme = OfficeXP) and (ClipW = 0)) then
      Pen.Color := dxColor_Chk_Enb_Border_WXP
    else}
    Pen.Color := dxColor_BorderLineOXP;
    if BiDiMode = bdRightToLeft then
      Rectangle(Bounds(R.Right - 2 - FCheckSize, (ClientHeight - FCheckSize) div
        2, FCheckSize, FCheckSize))
    else
      Rectangle(Bounds(R.Left + 2, (ClientHeight - FCheckSize) div
        2, FCheckSize, FCheckSize));

    // draw background.
{    case Theme of
      WindowsXP:
        begin
          if not ((ClipW <> 0) and (dsClicked in DrawState)) then
          begin
            if ClipW <> 0 then
              DrawGradient(FHlGradient);
            if BiDiMode = bdRightToLeft then
              BitBlt(Handle, R.Right - 1 - FCheckSize + ClipW, (ClientHeight - FCheckSize) div 2 + 1 +
                ClipW, FCheckSize - 2 - ClipW * 2, FCheckSize - 2 - ClipW * 2,
                FBgGradient.Canvas.Handle, 0, 0, SRCCOPY)
            else
              BitBlt(Handle, R.Left + 3 + ClipW, (ClientHeight - FCheckSize) div 2 + 1 +
                ClipW, FCheckSize - 2 - ClipW * 2, FCheckSize - 2 - ClipW * 2,
                FBgGradient.Canvas.Handle, 0, 0, SRCCOPY);
          end
          else
            DrawGradient(FCkGradient);
        end;
      OfficeXP:
        begin}
    if ClipW <> 0 then
      begin
      if not (dsClicked in DrawState) then
        Brush.Color := dxColor_BgOXP
      else
        Brush.Color := dxColor_BgCkOXP;
      if BiDiMode = bdRightToLeft then
        FillRect(Bounds(R.Right - 1, (ClientHeight - FCheckSize) div 2 +
          1, FCheckSize - 2, FCheckSize - 2))
      else
        FillRect(Bounds(R.Left + 3, (ClientHeight - FCheckSize) div
          2 + 1, FCheckSize - 2, FCheckSize - 2));
      end;
    //        end;
    //    end;

    // draw checked or grayed symbols:
    if FState in [cbChecked, cbGrayed] then
      begin
        if (dsClicked in DrawState) or (dsHighlight in DrawState) then
          begin
            Brush.Color := clWhite;
            Pen  .Color := clWhite;
          end
        else
        if FState = cbChecked then
          begin
            Brush.Color := clWindowText;
            Pen  .Color := clWindowText;
          end
         else
          begin
            Brush.Color := clGray;
            Pen  .Color := clGray;
          end;
        if BiDiMode = bdRightToLeft then
          Begin
            CreateCheck ( R.Right - FCheckSize + 1, (ClientHeight - FCheckSize) div 2 + 3, ap_Polygon );
            Polygon(ap_Polygon);
          end
        else
        Begin
          CreateCheck ( FCheckSize div 2 - 1, (ClientHeight - FCheckSize) div 2 + 3, ap_Polygon );
          Polygon(ap_Polygon);
        end
      end;
    end;
end;

initialization
{$I *.lrs}
{$IFDEF USEJVCL}
{$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}
{$ENDIF USEJVCL}

end.
