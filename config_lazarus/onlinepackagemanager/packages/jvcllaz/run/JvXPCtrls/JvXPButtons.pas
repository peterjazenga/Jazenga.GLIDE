{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvXPButtons.PAS, released on 2004-01-01.

The Initial Developer of the Original Code is Marc Hoffman.
Portions created by Marc Hoffman are Copyright (C) 2002 APRIORI business solutions AG.
Portions created by APRIORI business solutions AG are Copyright (C) 2002 APRIORI business solutions AG
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id: JvXPButtons.pas 11167 2007-01-27 18:57:52Z obones $

{$MODE DELPHI}

unit JvXPButtons;

interface

uses
  Classes, TypInfo, LCLIntf, LCLType, LCLProc, LMessages, Graphics,
  Controls, Forms, ActnList, ImgList, Menus,
  JvXPCore, JvXPCoreUtils;

type
  TJvXPCustomButtonActionLink = class(TWinControlActionLink)
  protected
    function IsImageIndexLinked: Boolean; override;
    procedure AssignClient(AClient: TObject); override;
    procedure SetImageIndex(Value: Integer); override;
  public
    destructor Destroy; override;
  end;

  TJvXPLayout = (blGlyphLeft, blGlyphRight, blGlyphTop, blGlyphBottom);

  TJvXPCustomButton = class(TJvXPCustomStyleControl)
  private
    FAutoGray: Boolean;
    FBgGradient: TBitmap;
    FCancel: Boolean;
    FCkGradient: TBitmap;
    FDefault: Boolean;
    FFcGradient: TBitmap;
    FGlyph: TPicture;
    FHlGradient: TBitmap;
    FImageChangeLink: TChangeLink;
    FImageIndex: Integer;
    FLayout: TJvXPLayout;
    FShowAccelChar: Boolean;
    FShowFocusRect: Boolean;
    FSmoothEdges: Boolean;
    FSpacing: Byte;
    FWordWrap: Boolean;
    procedure CMDialogKey(var Msg: TCMDialogKey); message CM_DIALOGKEY;
    procedure GlyphChange(Sender: TObject);
    procedure ImageListChange(Sender: TObject);
  protected
    function GetActionLinkClass: TControlActionLinkClass; override;
    function IsSpecialDrawState(IgnoreDefault: Boolean = False): Boolean;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure SetAutoGray(Value: Boolean); virtual;
    procedure SetDefault(Value: Boolean); virtual;
    procedure SetGlyph(Value: TPicture); virtual;
    procedure SetLayout(Value: TJvXPLayout); virtual;
    procedure SetShowAccelChar(Value: Boolean); virtual;
    procedure SetShowFocusRect(Value: Boolean); virtual;
    procedure SetSmoothEdges(Value: Boolean); virtual;
    procedure SetSpacing(Value: Byte); virtual;
    procedure SetWordWrap(Value: Boolean); virtual;
    procedure Paint; override;
    procedure HookResized; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: integer); override;
    procedure UpdateBitmaps;

    // advanced properties.
    property AutoGray: Boolean read FAutoGray write SetAutoGray default True;
    property Cancel: Boolean read FCancel write FCancel default False;
    property Default: Boolean read FDefault write SetDefault default False;
    property Glyph: TPicture read FGlyph write SetGlyph;
    property Layout: TJvXPLayout read FLayout write SetLayout default blGlyphLeft;
    property ShowAccelChar: Boolean read FShowAccelChar write SetShowAccelChar default True;
    property ShowFocusRect: Boolean read FShowFocusRect write SetShowFocusRect default False;
    property SmoothEdges: Boolean read FSmoothEdges write SetSmoothEdges default True;
    property Spacing: Byte read FSpacing write SetSpacing default 3;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default True;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    procedure Loaded ; override;
  published
//    property TabStop default True;
//    property UseDockManager default True;
  end;

  TJvXPButton = class(TJvXPCustomButton)
  published
    // common properties.
    property Action;
    property Caption;
    property Enabled;
    property TabOrder;
    property TabStop default True;
    property Height default 21;
    property Width default 73;

    // advanced properties.
    property AutoGray;
    property Cancel;
    property Default;
    property Glyph;
    property Layout;
    property ModalResult;
    property ShowAccelChar;
    property ShowFocusRect;
    property SmoothEdges;
    property Spacing;
    property WordWrap;

    //property BevelInner;
    //property BevelOuter;
    //property BevelWidth;
    //property BiDiMode;
    //property Ctl3D;
    //property DockSite;
    //property ParentBiDiMode;
    //property ParentCtl3D;
    //property TabOrder;
    //property TabStop;
    property UseDockManager default True;
    property Align;
    property Anchors;
    //property AutoSize;
    property Constraints;
    property DragCursor;
    property DragKind;
//    property OnCanResize;
    property DragMode;
//    property Enabled;
    property Font;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Style;
    property StyleManager;
    property Visible;
    //property OnDockDrop;
    //property OnDockOver;
    //property OnEndDock;
    //property OnGetSiteInfo;
    //property OnStartDock;
    //property OnUnDock;
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

  TJvXPToolType =
    (ttArrowLeft, ttArrowRight, ttClose, ttMaximize, ttMinimize, ttPopup, ttRestore, ttImage);

  TJvXPCustomToolButton = class(TJvXPCustomStyleControl)
  private
    FToolType: TJvXPToolType;
    FDropDownMenu: TPopupMenu;
    FChangeLink: TChangeLink;
    FImages: TCustomImageList;
    FImageIndex: TImageIndex;
    procedure SetImages(const Value: TCustomImageList);
    procedure SetImageIndex(const Value: TImageIndex);
    procedure SetDropDownMenu(const Value: TPopupMenu);
    procedure DoImagesChange(Sender: TObject);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X: Integer; Y: Integer); override;
    procedure SetToolType(Value: TJvXPToolType); virtual;
    procedure Paint; override;
    procedure HookResized; override;

    property ToolType: TJvXPToolType read FToolType write SetToolType default ttClose;
    property DropDownMenu: TPopupMenu read FDropDownMenu write SetDropDownMenu;
    property Images: TCustomImageList read FImages write SetImages;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TJvXPToolButton = class(TJvXPCustomToolButton)
  published
    property Enabled;
    property Color default clBlack;
    property Height default 15;
    property ToolType;
    property Width default 15;

    //property BevelInner;
    //property BevelOuter;
    //property BevelWidth;
    //property BiDiMode;
    //property Ctl3D;
    //property DockSite;
    //property ParentBiDiMode;
    //property ParentCtl3D;
    //property TabOrder;
    //property TabStop;
    //property UseDockManager default True;
    property Align;
    property Anchors;
    //property AutoSize;
    property Constraints;
    property DragCursor;
    property DragKind;
    //property OnCanResize;
    property DragMode;
    property DropDownMenu;
    property Images;
    property ImageIndex;

//    property Enabled;
    property Font;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Style;
    property StyleManager;
    property Visible;
    //property OnDockDrop;
    //property OnDockOver;
    //property OnEndDock;
    //property OnGetSiteInfo;
    //property OnStartDock;
    //property OnUnDock;
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


implementation

//{$R ../resource/JvXPCore.res}

//=== { TJvXPCustomButtonActionLink } ========================================

destructor TJvXPCustomButtonActionLink.Destroy;
begin
  TJvXPCustomButton(FClient).Invalidate;
  inherited Destroy;
end;

procedure TJvXPCustomButtonActionLink.AssignClient(AClient: TObject);
begin
  inherited AssignClient(AClient);
  FClient := AClient as TJvXPCustomButton;
end;

function TJvXPCustomButtonActionLink.IsImageIndexLinked: Boolean;
begin
  Result := True;
end;

procedure TJvXPCustomButtonActionLink.SetImageIndex(Value: Integer);
begin
  inherited SetImageIndex(Value);
  (FClient as TJvXPCustomButton).FImageIndex := Value;
  (FClient as TJvXPCustomButton).Invalidate;
end;

//=== { TJvXPCustomButton } ==================================================

constructor TJvXPCustomButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // create ...
  FBgGradient := TBitmap.Create; // background gradient
  FCkGradient := TBitmap.Create; // clicked gradient
  FFcGradient := TBitmap.Create; // focused gradient
  FHlGradient := TBitmap.Create; // Highlight gradient

  // set default properties.
  ControlStyle := ControlStyle - [csDoubleClicks];
  Height := 21;
  Width := 73;
  TabStop := True;
  UseDockManager := true;

  // set custom properties.
  FAutoGray := True;
  FCancel := False;
  FDefault := False;
  FImageIndex := -1;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  FGlyph := TPicture.Create;
  FGlyph.OnChange := GlyphChange;
  FLayout := blGlyphLeft;
  FShowAccelChar := True;
  FShowFocusRect := False;
  FSmoothEdges := True;
  FSpacing := 3;
  FWordWrap := True;
end;

destructor TJvXPCustomButton.Destroy;
begin
  FBgGradient.Free;
  FCkGradient.Free;
  FFcGradient.Free;
  FHlGradient.Free;
  FGlyph.Free;
  FImageChangeLink.OnChange := nil;
  FImageChangeLink.Free;
  FImageChangeLink := nil;
  inherited Destroy;
end;

procedure TJvXPCustomButton.Click;
begin
  // Only there to make it public (Mantis 4015)
  inherited Click;
end;

procedure TJvXPCustomButton.Loaded;
begin
  inherited Loaded;
  //HookResized;
end;

function TJvXPCustomButton.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TJvXPCustomButtonActionLink;
end;

procedure TJvXPCustomButton.CMDialogKey(var Msg: TCMDialogKey);
begin
  inherited;
  with Msg do
    if (((CharCode = VK_RETURN) and (Focused or (FDefault and not (IsSibling)))) or
      ((CharCode = VK_ESCAPE) and FCancel) and (KeyDataToShiftState(KeyData) = [])) and
      CanFocus then
    begin
      Click;
      Result := 1;
    end
    else
      inherited;
end;

procedure TJvXPCustomButton.SetAutoGray(Value: Boolean);
begin
  if Value <> FAutoGray then
  begin
    FAutoGray := Value;
    LockedInvalidate;
  end;
end;

procedure TJvXPCustomButton.SetDefault(Value: Boolean);
begin
  if Value <> FDefault then
  begin
    FDefault := Value;
    if GetParentForm(Self) <> nil then
      with GetParentForm(Self) do
        Perform(CM_FOCUSCHANGED, 0, PtrInt(ActiveControl));
  end;
end;

procedure TJvXPCustomButton.SetGlyph(Value: TPicture);
begin
  FGlyph.Assign(Value);
  LockedInvalidate;
end;

procedure TJvXPCustomButton.SetLayout(Value: TJvXPLayout);
begin
  if Value <> FLayout then
  begin
    FLayout := Value;
    LockedInvalidate;
  end;
end;

procedure TJvXPCustomButton.SetShowAccelChar(Value: Boolean);
begin
  if Value <> FShowAccelChar then
  begin
    FShowAccelChar := Value;
    LockedInvalidate;
  end;
end;

procedure TJvXPCustomButton.SetShowFocusRect(Value: Boolean);
begin
  if Value <> FShowFocusRect then
  begin
    FShowFocusRect := Value;
    LockedInvalidate;
  end;
end;

procedure TJvXPCustomButton.SetSmoothEdges(Value: Boolean);
begin
  if Value <> FSmoothEdges then
  begin
    FSmoothEdges := Value;
    LockedInvalidate;
  end;
end;

procedure TJvXPCustomButton.SetSpacing(Value: Byte);
begin
  if Value <> FSpacing then
  begin
    FSpacing := Value;
    LockedInvalidate;
  end;
end;

procedure TJvXPCustomButton.SetWordWrap(Value: Boolean);
begin
  if Value <> FWordWrap then
  begin
    FWordWrap := Value;
    LockedInvalidate;
  end;
end;

procedure TJvXPCustomButton.ImageListChange(Sender: TObject);
begin
  if Assigned(Action) and (Sender is TCustomImageList) and
    Assigned(TAction(Action).ActionList.Images) and
    ((TAction(Action).ImageIndex < (TAction(Action).ActionList.Images.Count))) then
    FImageIndex := TAction(Action).ImageIndex
  else
    FImageIndex := -1;
  LockedInvalidate;
  Paint;
end;

procedure TJvXPCustomButton.GlyphChange(Sender: TObject);
begin
  LockedInvalidate;
  Paint;
end;

procedure TJvXPCustomButton.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Shift = []) and (Key = VK_SPACE) then
  begin
    DrawState := DrawState + [dsHighlight];
    HookMouseDown;
  end;
  inherited KeyDown(Key, Shift);
end;

procedure TJvXPCustomButton.KeyUp(var Key: Word; Shift: TShiftState);
var
  Pos: TPoint;
begin
  // it's not possible to call the 'HookMouseUp' or 'HookMouseLeave' methods,
  // because we don't want to call their event handlers.
  if dsClicked in DrawState then
  begin
    GetCursorPos(Pos);
    Pos := ScreenToClient(Pos);
    if not PtInRect(Bounds(0, 0, Width, Height), Pos) then
      DrawState := DrawState - [dsHighlight];
    DrawState := DrawState - [dsClicked];
    LockedInvalidate;
    Click;
  end;
  inherited KeyUp(Key, Shift);
end;

function TJvXPCustomButton.IsSpecialDrawState(IgnoreDefault: Boolean = False): Boolean;
begin
  if dsClicked in DrawState then
    Result := not (dsHighlight in DrawState)
  else
    Result := (dsHighlight in DrawState) or (dsFocused in DrawState);
  if not IgnoreDefault then
    Result := Result or (FDefault and CanFocus) and not IsSibling;
end;

procedure TJvXPCustomButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited ActionChange(Sender, CheckDefaults);
  if Sender is TCustomAction then
    with TCustomAction(Sender) do
    begin
      if Assigned(TCustomAction(Sender).ActionList.Images) and
        (FImageChangeLink.Sender <> TCustomAction(Sender).ActionList.Images) then
        TCustomAction(Sender).ActionList.Images.RegisterChanges(FImageChangeLink);
      if (ActionList <> nil) and (ActionList.Images <> nil) and
        (ImageIndex >= 0) and (ImageIndex < ActionList.Images.Count) then
        FImageIndex := ImageIndex;
      LockedInvalidate;
    end;
end;

procedure TJvXPCustomButton.HookResized;
begin
  inherited HookResized;
  UpdateBitmaps;
end;

procedure TJvXPCustomButton.UpdateBitmaps;
const
  ColSteps = 64;
  Dithering = True;
var
  Offset: Integer;
begin
  // calculate offset
  Offset := 4 * (Integer(IsSpecialDrawState(True)));

  // create gradient rectangles for...

  // background
  if  (Width  - (2 + Offset) > 0) and (Height - (2 + Offset) > 0)
  then
    JvXPCreateGradientRect(Width - (2 + Offset), Height - (2 + Offset),
      dxColor_Btn_Enb_BgFrom_WXP, dxColor_Btn_Enb_BgTo_WXP, ColSteps, gsTop, Dithering,
      FBgGradient);

  if  (Width  - 2 > 0) and (Height - 2 > 0) then
  begin
    // clicked
    JvXPCreateGradientRect(Width - 2, Height - 2, dxColor_Btn_Enb_CkFrom_WXP,
      dxColor_Btn_Enb_CkTo_WXP, ColSteps, gsTop, Dithering, FCkGradient);

    // focused
    JvXPCreateGradientRect(Width - 2, Height - 2, dxColor_Btn_Enb_FcFrom_WXP,
      dxColor_Btn_Enb_FcTo_WXP, ColSteps, gsTop, Dithering, FFcGradient);

    // highlight
    JvXPCreateGradientRect(Width - 2, Height - 2, dxColor_Btn_Enb_HlFrom_WXP,
      dxColor_Btn_Enb_HlTo_WXP, ColSteps, gsTop, Dithering, FHlGradient);
   end;
                {
  Invalidate;
  Paint;
  }
end;

{
procedure TJvXPCustomButton.HookResized;
begin
  UpdateBitmaps;
  inherited;
end;
 }

procedure TJvXPCustomButton.SetBounds(ALeft, ATop, AWidth, AHeight: integer);
begin
  inherited;
  UpdateBitmaps;
end;

procedure TJvXPCustomButton.Paint;
var
  Rect: TRect;
  Offset, Flags: Integer;
  DrawPressed: Boolean;
  lImage: TPicture;
  lBitmap: TBitmap;
begin
  with Canvas do
  begin
    // clear background.
    Rect := GetClientRect;
    Brush.Color := Self.Color;
    FillRect(Rect);

    // draw gradient borders.
    if IsSpecialDrawState and
      ( ((dsHighlight in DrawState) and (FHlGradient.Canvas.Handle <> 0)) or
        (not (dsHighlight in DrawState) and (FFcGradient.Canvas.Handle <> 0))
      )
    then begin
      lBitmap := TBitmap.Create;
      try
        if dsHighlight in DrawState then
          lBitmap.Assign(FHlGradient)
        else
          lBitmap.Assign(FFcGradient);
        BitBlt(Handle, 1, 1, Self.Width, Self.Height, lBitmap.Canvas.Handle, 0, 0, SRCCOPY);
      finally
        lBitmap.Free;
      end;
    end;

    // draw background gradient...
    if not ((dsHighlight in DrawState) and (dsClicked in DrawState)) then
    begin
      if (FBgGradient.Canvas.Handle <> 0) Then
      begin
        Offset := 2 * Integer(IsSpecialDrawState);
        if (Self.Width - 3 * Offset > 1 + Offset) and
           (Self.Height - 3 * Offset > 1 + Offset)
        then
          BitBlt(Handle, 1 + Offset, 1 + Offset, self.Width - 3 * Offset, Self.Height - 3 * Offset,
            FBgGradient.Canvas.Handle, 0, 0, SRCCOPY);
      end;
    end
    // ...or click gradient.
    else
    if (FCkGradient.Canvas.Handle <> 0) then
    begin
      BitBlt(Handle, 1, 1, Self.Width, Self.Height, FCkGradient.Canvas.Handle, 0, 0, SRCCOPY);
    end;

    // draw border lines.
    if Enabled then
      Pen.Color := dxColor_Btn_Enb_Border_WXP
    else
      Pen.Color := dxColor_Btn_Dis_Border_WXP;
    Brush.Style := bsClear;
    RoundRect(0, 0, Self.Width, Self.Height, 5, 5);

    // draw border edges.
    if FSmoothEdges and (Self.Width > 2) and (Self.Height > 2) then
    begin
      if Enabled then
        Pen.Color := dxColor_Btn_Enb_Edges_WXP
      else
        Pen.Color := dxColor_Btn_Dis_Edges_WXP;
      JvXPDrawLine(Canvas, 0, 1, 2, 0);
      JvXPDrawLine(Canvas, Self.Width - 2, 0, Self.Width, 2);
      JvXPDrawLine(Canvas, 0, Self.Height - 2, 2, Self.Height);
      JvXPDrawLine(Canvas, Self.Width - 3, Self.Height, Self.Width, Self.Height - 3);
    end;

    // set drawing flags.
    Flags := {DT_VCENTER or } DT_END_ELLIPSIS;
    if FWordWrap then
      Flags := Flags or DT_WORDBREAK;

    // draw image & caption.
    lImage := TPicture.Create;
    try
      // get image from action or glyph property.
      if Assigned(Action) and Assigned(TAction(Action).ActionList.Images) and
        (FImageIndex > -1) and (FImageIndex < TAction(Action).ActionList.Images.Count)
      then
        TAction(Action).ActionList.Images.GetBitmap(FImageIndex, lImage.Bitmap)
      else
        lImage.Assign(FGlyph);

      // autogray image (if allowed).
      if (lImage.Bitmap.Canvas.Handle <> 0) and FAutoGray and not Enabled then
        JvXPConvertToGray2(lImage.Bitmap);

      // assign canvas font (change HotTrack-Color, if necessary).
      Font.Assign(Self.Font);

      // calculate textrect.
      if Assigned(lImage.Graphic) and not lImage.Graphic.Empty then
        if Length(Caption) > 0 then
        begin
          case FLayout of
            blGlyphLeft:
              Inc(Rect.Left, lImage.Width + FSpacing);
            blGlyphRight:
              begin
                Dec(Rect.Left, lImage.Width + FSpacing);
                Dec(Rect.Right, (lImage.Width + FSpacing) * 2);
                Flags := Flags or DT_RIGHT;
              end;
            blGlyphTop:
              Inc(Rect.Top, lImage.Height + FSpacing);
            blGlyphBottom:
              Dec(Rect.Top, lImage.Height + FSpacing);
          end;
        end;

      if Length(Caption) > 0 then
      begin
        JvXPRenderText(Self, Canvas, Caption, Font, Enabled, FShowAccelChar, Rect, Flags or DT_CALCRECT);
        OffsetRect(Rect, (Self.Width - Rect.Right) div 2, (Self.Height - Rect.Bottom) div 2);
      end;

      // should we draw the pressed state?
      DrawPressed := (dsHighlight in DrawState) and (dsClicked in DrawState);
      if DrawPressed then
        OffsetRect(Rect, 1, 1);

      // draw image - if available.
      if Assigned(lImage.Graphic) and not lImage.Graphic.Empty then
      begin
        lImage.Graphic.Transparent := True;
        if Length(Caption) > 0 then
          case FLayout of
            blGlyphLeft:
              Draw(Rect.Left - (lImage.Width + FSpacing), (Self.Height - lImage.Height) div 2 +
                Integer(DrawPressed), lImage.Graphic);
            blGlyphRight:
              Draw(Rect.Right + FSpacing, (Self.Height - lImage.Height) div 2 +
                Integer(DrawPressed), lImage.Graphic);
            blGlyphTop:
              Draw((Self.Width - lImage.Width) div 2 + Integer(DrawPressed),
                Rect.Top - (lImage.Height + FSpacing), lImage.Graphic);
            blGlyphBottom:
              Draw((Self.Width - lImage.Width) div 2 + Integer(DrawPressed),
                Rect.Bottom + FSpacing, lImage.Graphic);
          end
        else
          // draw the glyph into the center
          Draw((Self.Width - lImage.Width) div 2 + Integer(DrawPressed),
            (Self.Height - lImage.Height) div 2 + Integer(DrawPressed), lImage.Graphic);
      end;

      // draw focusrect (if enabled).
      if (dsFocused in DrawState) and FShowFocusRect then
      begin
        Brush.Style := bsSolid;
        DrawFocusRect(Bounds(3, 3, Self.Width - 6, Self.Height - 6));
      end;

      // draw caption.
      SetBkMode(Handle, Transparent);
      JvXPRenderText(Self, Canvas, Caption, Font, Enabled, FShowAccelChar, Rect, Flags);
      SetBkMode(Handle, OPAQUE);
    finally
      lImage.Free;
    end;
  end;
end;

// TJvXPCustomToolButton =====================================================

constructor TJvXPCustomToolButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csDoubleClicks];
  Color := clBlack;
  FToolType := ttClose;
  FChangeLink := TChangeLink.Create;
  FChangeLink.OnChange := DoImagesChange;
  HookResized;
end;

destructor TJvXPCustomToolButton.Destroy;
begin
  FChangeLink.Free;
  inherited Destroy;
end;

procedure TJvXPCustomToolButton.HookResized;
begin
  if ToolType <> ttImage then
  begin
    Height := 15;
    Width := 15;
  end;
end;

procedure TJvXPCustomToolButton.SetToolType(Value: TJvXPToolType);
begin
  if Value <> FToolType then
  begin
    FToolType := Value;
    LockedInvalidate;
  end;
end;

procedure TJvXPCustomToolButton.Paint;
var
  Rect: TRect;
  lBitmap: TBitmap;
  //Theme: TJvXPTheme;
  Shifted: Boolean;
begin
  with Canvas do
  begin
    Rect := GetClientRect;
    Brush.Color := TJvXPWinControl(Parent).Color;
    Brush.Style := bsSolid;
    FillRect(Rect);
    if csDesigning in ComponentState then
      DrawFocusRect(Rect);
    Brush.Style := bsClear;
    {
    Theme := Style.GetTheme;
    if (Theme = WindowsXP) and (dsClicked in DrawState) and
      not (dsHighlight in DrawState)
    then
      JvXPFrame3d(Self.Canvas, Rect, clWhite, clBlack);
      }
    if dsHighlight in DrawState then
    begin
      {
      if Theme = WindowsXP then
        JvXPFrame3d(Self.Canvas, Rect, clWhite, clBlack, dsClicked in DrawState)
      else
      }
      begin
        Pen.Color := dxColor_BorderLineOXP;
        Rectangle(Rect);
        InflateRect(Rect, -1, -1);
        if dsClicked in DrawState then
          Brush.Color := dxColor_BgCkOXP
        else
          Brush.Color := dxColor_BgOXP;
        FillRect(Rect);
      end;
    end;
//    Shifted := (Theme = WindowsXP) and (dsClicked in DrawState);  wp
    Shifted := dsClicked in DrawState;
    if ToolType = ttImage then
    begin
      if (Images = nil) or (ImageIndex < 0) or (ImageIndex >= Images.Count) then
        Exit;
      Images.Draw(Canvas,
        (Width - Images.Width) div 2 + Integer(Shifted),
        (Height - Images.Height) div 2 + Integer(Shifted),
        ImageIndex,
        {$IFDEF COMPILER6_UP}
        {$IFDEF VCL}
        dsTransparent,
        {$ENDIF VCL}
        itImage,
        {$ENDIF COMPILER6_UP}
        Enabled);
    end
    else
    begin
      lBitmap := TBitmap.Create;
      try
        {
        lBitmap.LoadFromLazarusResource(
          PChar('JvXPCustomToolButton' + Copy(GetEnumName(TypeInfo(TJvXPToolType),
          Ord(FToolType)), 3, MaxInt)));
          }
        lBitmap.Assign(nil); // fixes GDI resource leak
        lBitmap.LoadFromResourceName(HInstance,
          PChar('JvXPCustomToolButton' + Copy(GetEnumName(TypeInfo(TJvXPToolType),
          Ord(FToolType)), 3, MaxInt)));
        if (dsClicked in DrawState) and (dsHighlight in DrawState) then
          JvXPColorizeBitmap(lBitmap, clWhite)
        else
        if not Enabled then
          JvXPColorizeBitmap(lBitmap, clGray)
        else
        if Color <> clBlack then
          JvXPColorizeBitmap(lBitmap, Color);
        lBitmap.Transparent := True;
        Draw((ClientWidth - lBitmap.Width) div 2 + Integer(Shifted),
          (ClientHeight - lBitmap.Height) div 2 + Integer(Shifted), lBitmap);
      finally
        lBitmap.Free;
      end;
    end;
  end;
end;

procedure TJvXPCustomToolButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = DropDownMenu then
      DropDownMenu := nil
    else
    if AComponent = Images then
      Images := nil;
  end;
end;

procedure TJvXPCustomToolButton.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Assigned(DropDownMenu) then
  begin
    P := ClientToScreen(Point(0, Height));
    DropDownMenu.Popup(P.X, P.Y);
//    while PeekMessage(Msg, HWND_DESKTOP, WM_MOUSEFIRST, WM_MOUSELAST, PM_REMOVE) do
      {nothing};
    if GetCapture <> 0 then
      SendMessage(GetCapture, LM_CANCELMODE, 0, 0);
  end;
end;

procedure TJvXPCustomToolButton.SetImages(const Value: TCustomImageList);
begin
  if FImages <> Value then
  begin
    if FImages <> nil then
      FImages.UnRegisterChanges(FChangeLink);
    FImages := Value;
    if FImages <> nil then
    begin
      FImages.FreeNotification(Self);
      FImages.RegisterChanges(FChangeLink);
    end;
    LockedInvalidate;
  end;
end;

procedure TJvXPCustomToolButton.SetImageIndex(const Value: TImageIndex);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    LockedInvalidate;
  end;
end;

procedure TJvXPCustomToolButton.SetDropDownMenu(const Value: TPopupMenu);
begin
  if FDropDownMenu <> Value then
  begin
    FDropDownMenu := Value;
    if FDropDownMenu <> nil then
      FDropDownMenu.FreeNotification(Self);
    LockedInvalidate;
  end;
end;

procedure TJvXPCustomToolButton.DoImagesChange(Sender: TObject);
begin
  LockedInvalidate;
end;


end.

