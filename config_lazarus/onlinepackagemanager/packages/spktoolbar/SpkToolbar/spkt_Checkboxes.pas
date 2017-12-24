unit spkt_Checkboxes;

{$mode objfpc}{$H+}

interface

uses
  Graphics, Classes, SysUtils, Controls, StdCtrls, ActnList,
  SpkMath, SpkGUITools, spkt_BaseItem, spkt_Buttons;

type
  TSpkCustomCheckbox = class;

  TSpkCheckboxActionLink = class(TSpkButtonActionLink)
  private
  protected
    procedure SetChecked(Value: Boolean); override;
  public
    function IsCheckedLinked: Boolean; override;
  end;

  TSpkCustomCheckBox = class(TSPkBaseButton)
  private
    FState: TCheckboxState;              // unchecked, checked, grayed
    FCheckboxState: TSpkCheckboxState;   // incl Hot, Pressed, Disabled
    FHideFrameWhenIdle : boolean;
    FTableBehaviour : TSpkItemTableBehaviour;
    FGroupBehaviour : TSPkItemGroupBehaviour;
    FCheckboxStyle: TSpkCheckboxStyle;
    function  GetChecked: Boolean;
    procedure SetChecked(AValue: Boolean);
    procedure SetGroupBehaviour(const Value: TSpkItemGroupBehaviour);
    procedure SetTableBehaviour(const Value: TSpkItemTableBehaviour);
  protected
    procedure ActionChange(Sender : TObject);
    procedure BtnStateToCheckboxState;
    procedure CalcRects; override;
    procedure Click; override;
    procedure ConstructRect(var BtnRect: T2DIntRect);
    function GetDefaultCaption: String; override;
    procedure SetAction(const AValue: TBasicAction); override;
    procedure SetEnabled(const AValue: Boolean); override;
    procedure SetState(AValue: TCheckboxState); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Draw(ABuffer: TBitmap; ClipRect: T2DIntRect); override;
    function GetGroupBehaviour : TSpkItemGroupBehaviour; override;
    function GetSize: TSpkItemSize; override;
    function GetTableBehaviour : TSpkItemTableBehaviour; override;
    function GetWidth : integer; override;
    procedure MouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  published
    property Checked: Boolean read GetChecked write SetChecked;
    property State: TCheckboxState read FState write SetState;
    property TableBehaviour : TSpkItemTableBehaviour read FTableBehaviour write SetTableBehaviour;
    property GroupBehaviour : TSpkItemGroupBehaviour read FGroupBehaviour write SetGroupBehaviour;
  end;

  TSpkCheckbox = class(TSpkCustomCheckbox)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TSpkRadioButton = class(TSpkCustomCheckbox)
  protected
    function GetDefaultCaption: String; override;
    procedure SetState(AValue: TCheckboxState); override;
    procedure UncheckSiblings;
  public
    constructor Create(AOwner: TComponent); override;
  end;


implementation

uses
  LCLType, LCLIntf, Math, Themes,
  SpkGraphTools, spkt_Const, spkt_Tools, spkt_Pane, spkt_Appearance;


{ TSpkCheckboxActionLink }

function TSpkCheckboxActionLink.IsCheckedLinked: Boolean;
var
  cb: TSpkCustomCheckbox;
begin
  cb := FClient as TSpkCustomCheckbox;
  result := (inherited IsCheckedLinked) and
    Assigned(cb) and (cb.Checked = (Action as TCustomAction).Checked);
end;

procedure TSpkCheckboxActionLink.SetChecked(Value: Boolean);
var
  cb: TSpkCustomCheckbox;
begin
  if IsCheckedLinked then begin
    cb := TSpkCustomCheckbox(FClient);
    cb.Checked := Value;
  end;
end;


{ TSpkCustomCheckbox }

constructor TSpkCustomCheckbox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHideFrameWhenIdle := true;
  FTableBehaviour := tbContinuesRow;
  FGroupBehaviour := gbSingleItem;
  FCheckboxStyle := cbsCheckbox;
  FState := cbUnchecked;
end;

procedure TSpkCustomCheckbox.ActionChange(Sender: TObject);
begin
  if Sender is TCustomAction then
    with TCustomAction(Sender) do begin
      if (Self.Caption = '') or (Self.Caption = GetDefaultCaption) then
        Self.Caption := Caption;
      if (Self.Enabled = True) then
        Self.Enabled := Enabled;
      if (Self.Visible = True) then
        Self.Visible := Visible;
      if not Assigned(Self.OnClick) then
        Self.OnClick := OnExecute;
      if (Self.Checked = false) then
        Self.Checked := Checked;
    end;
end;

procedure TSpkCustomCheckbox.BtnStateToCheckboxState;
begin
  if FEnabled then
    case FButtonState of
      bsIdle        : FCheckboxState := cbsIdle;
      bsBtnHotTrack : FCheckboxState := cbsHotTrack;
      bsBtnPressed  : FCheckboxState := cbsPressed;
    end
  else
    FCheckboxState := cbsDisabled;
end;

procedure TSpkCustomCheckbox.CalcRects;
var
  RectVector : T2DIntVector;
begin
  ConstructRect(FButtonRect);
 {$IFDEF EnhancedRecordSupport}
  FDropdownRect := T2DIntRect.Create(0, 0, 0, 0);
  RectVector := T2DIntVector.Create(FRect.Left, FRect.Top);
 {$ELSE}
  FDropdownRect.Create(0, 0, 0, 0);
  RectVector.Create(FRect.Left, FRect.Top);
 {$ENDIF}
  FButtonRect := FButtonRect + RectVector;
end;

procedure TSpkCustomCheckbox.Click;
begin
  if Enabled then begin
    case FState of
      cbGrayed    : Checked := true;
      cbChecked   : Checked := false;
      cbUnchecked : Checked := true;
    end;
    if not (csDesigning in ComponentState) and (FActionLink <> nil) then
      FActionLink.Execute(Self)
    else
    if Assigned(FOnClick) and ((Action = nil) or (FOnClick <> Action.OnExecute)) then
      FOnClick(Self);
  end;
end;

procedure TSpkCustomCheckbox.ConstructRect(var BtnRect: T2DIntRect);
var
  BtnWidth : integer;
  Bitmap : TBitmap;
  TextWidth: Integer;
begin
 {$IFDEF EnhancedRecordSupport}
  BtnRect:=T2DIntRect.Create(0, 0, 0, 0);
 {$ELSE}
  BtnRect.Create(0, 0, 0, 0);
 {$ENDIF}

  if not(Assigned(FToolbarDispatch)) then
    exit;
  if not(Assigned(FAppearance)) then
    exit;

  Bitmap := FToolbarDispatch.GetTempBitmap;
  if not(assigned(Bitmap)) then
    exit;

  Bitmap.Canvas.Font.Assign(FAppearance.Element.CaptionFont);
  TextWidth := Bitmap.Canvas.TextWidth(FCaption);

  BtnWidth := SmallButtonPadding + SmallButtonGlyphWidth +
    SmallButtonPadding + TextWidth + SmallButtonPadding;
  BtnWidth := Max(SmallButtonMinWidth, BtnWidth);

  if FGroupBehaviour in [gbContinuesGroup, gbEndsGroup] then
    BtnWidth := BtnWidth + SmallButtonHalfBorderWidth
  else
    BtnWidth := BtnWidth + SmallButtonBorderWidth;

  // Prawa krawêdŸ przycisku
  if (FGroupBehaviour in [gbBeginsGroup, gbContinuesGroup]) then
    BtnWidth := BtnWidth + SmallButtonHalfBorderWidth
  else
    BtnWidth := BtnWidth + SmallButtonBorderWidth;

 {$IFDEF EnhancedRecordSupport}
  BtnRect := T2DIntRect.Create(0, 0, BtnWidth - 1, PaneRowHeight - 1);
 {$ELSE}
  BtnRect.Create(0, 0, BtnWidth - 1, PaneRowHeight - 1);
 {$ENDIF}
end;

procedure TSpkCustomCheckbox.Draw(ABuffer: TBitmap; ClipRect: T2DIntRect);
var
  fontColor: TColor;
  x, y: Integer;
  h: Integer;
  te: TThemedElementDetails;
  cornerRadius: Integer;
begin
  if FToolbarDispatch = nil then
    exit;
  if FAppearance = nil then
    exit;
  if (FRect.Width < 2*LargeButtonRadius) or (FRect.Height < 2*LargeButtonRadius) then
    exit;

  case FAppearance.Element.Style of
    esRounded:
      cornerRadius := SmallButtonRadius;
    esRectangle:
      cornerRadius := 0;
  end;

  // Border
  if (FButtonState = bsIdle) and (not(FHideFrameWhenIdle)) then begin
    with FAppearance.Element do
      TButtonTools.DrawButton(
        ABuffer,
        FButtonRect,
        IdleFrameColor,
        IdleInnerLightColor,
        IdleInnerDarkColor,
        IdleGradientFromColor,
        IdleGradientToColor,
        IdleGradientType,
        (FGroupBehaviour in [gbContinuesGroup, gbEndsGroup]),
        (FGroupBehaviour in [gbBeginsGroup, gbContinuesGroup]) or (FButtonKind = bkButtonDropdown),
        false,
        false,
        cornerRadius,
        ClipRect
      );
  end else
  if (FButtonState=bsBtnHottrack) then begin
    with FAppearance.Element do
      TButtonTools.DrawButton(
        ABuffer,
        FButtonRect,
        HotTrackFrameColor,
        HotTrackInnerLightColor,
        HotTrackInnerDarkColor,
        HotTrackGradientFromColor,
        HotTrackGradientToColor,
        HotTrackGradientType,
        (FGroupBehaviour in [gbContinuesGroup, gbEndsGroup]),
        (FGroupBehaviour in [gbBeginsGroup, gbContinuesGroup]) or (FButtonKind = bkButtonDropdown),
        false,
        false,
        cornerRadius,
        ClipRect
      );
  end else
  if (FButtonState = bsBtnPressed) then begin
    with FAppearance.Element do
      TButtonTools.DrawButton(
        ABuffer,
        FButtonRect,
        ActiveFrameColor,
        ActiveInnerLightColor,
        ActiveInnerDarkColor,
        ActiveGradientFromColor,
        ActiveGradientToColor,
        ActiveGradientType,
        (FGroupBehaviour in [gbContinuesGroup, gbEndsGroup]),
        (FGroupBehaviour in [gbBeginsGroup, gbContinuesGroup]) or (FButtonKind = bkButtonDropdown),
        false,
        false,
        cornerRadius,
        ClipRect
      );
  end;

  // Checkbox
  if ThemeServices.ThemesEnabled then begin
    te := ThemeServices.GetElementDetails(tbCheckboxCheckedNormal);
    h := ThemeServices.GetDetailSize(te).cy;
  end else
    h := GetSystemMetrics(SM_CYMENUCHECK);
  if (FGroupBehaviour in [gbContinuesGroup, gbEndsGroup]) then
    x := FButtonRect.Left + SmallButtonHalfBorderWidth + SmallButtonPadding
  else
    x := FButtonRect.Left + SmallButtonBorderWidth + SmallButtonPadding;
  y := FButtonRect.Top + (FButtonRect.Height - h) div 2;

  TGUITools.DrawCheckbox(
    ABuffer.Canvas,
    x,y,
    FState,
    FCheckboxState,
    FCheckboxStyle,
    ClipRect
  );

  // Text
  ABuffer.Canvas.Font.Assign(FAppearance.Element.CaptionFont);

  case FButtonState of
    bsIdle             : fontColor := FAppearance.Element.IdleCaptionColor;
    bsBtnHottrack,
    bsDropdownHottrack : fontColor := FAppearance.Element.HotTrackCaptionColor;
    bsBtnPressed,
    bsDropdownPressed  : fontColor := FAppearance.ELement.ActiveCaptionColor;
  end;
  if not(FEnabled) then
    fontColor := TColorTools.ColorToGrayscale(fontColor);

  if (FGroupBehaviour in [gbContinuesGroup, gbEndsGroup]) then
    x := FButtonRect.Left + SmallButtonHalfBorderWidth
  else
    x := FButtonRect.Left + SmallButtonBorderWidth;
  x := x + 2 * SmallButtonPadding + SmallButtonGlyphWidth;
  y := FButtonRect.Top + (FButtonRect.Height - ABuffer.Canvas.TextHeight('Wy')) div 2;

  TGUITools.DrawText(ABuffer.Canvas, x, y, FCaption, fontColor, ClipRect);
end;

function TSpkCustomCheckbox.GetChecked: Boolean;
begin
  result := (FState = cbChecked);
end;

function TSpkCustomCheckbox.GetDefaultCaption: String;
begin
  result := 'Checkbox';
end;

function TSpkCustomCheckbox.GetGroupBehaviour: TSpkItemGroupBehaviour;
begin
  result := FGroupBehaviour;
end;

function TSpkCustomCheckbox.GetSize: TSpkItemSize;
begin
  result := isNormal;
end;

function TSpkCustomCheckbox.GetTableBehaviour: TSpkItemTableBehaviour;
begin
  result := FTableBehaviour;
end;

function TSpkCustomCheckbox.GetWidth: integer;
var
  BtnRect, DropRect : T2DIntRect;
begin
  result := -1;
  if FToolbarDispatch = nil then
    exit;
  if FAppearance = nil then
    exit;
  ConstructRect(BtnRect);
  result := BtnRect.Right + 1;
end;

procedure TSpkCustomCheckbox.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  BtnStateToCheckboxState;
end;

procedure TSpkCustomCheckbox.MouseLeave;
begin
  inherited MouseLeave;
  if FEnabled then
    FCheckboxState := cbsIdle
  else
    FCheckboxState := cbsDisabled;
end;

procedure TSpkCustomCheckbox.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  BtnStateToCheckboxState;
end;

procedure TSpkCustomCheckbox.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  BtnStateToCheckboxState;
end;

procedure TSpkCustomCheckbox.SetAction(const AValue: TBasicAction);
begin
  if AValue = nil then begin
    FActionLink.Free;
    FActionLink := nil;
  end else begin
    if FActionLink = nil then
      FActionLink := TSpkCheckboxActionLink.Create(self);
    FActionLink.Action := AValue;
    FActionLink.OnChange := @ActionChange;
    ActionChange(AValue);
  end;
end;

procedure TSpkCustomCheckbox.SetChecked(AValue: Boolean);
begin
  if AValue then
    SetState(cbChecked)
  else
    SetState(cbUnchecked);
end;

procedure TSpkCustomCheckbox.SetEnabled(const AValue: Boolean);
begin
  inherited SetEnabled(AValue);
  BtnStateToCheckboxState;
end;

procedure TSpkCustomCheckbox.SetGroupBehaviour(const Value: TSpkItemGroupBehaviour);
begin
  FGroupBehaviour := Value;
  if Assigned(FToolbarDispatch) then
    FToolbarDispatch.NotifyMetricsChanged;
end;

procedure TSpkCustomCheckbox.SetState(AValue:TCheckboxState);
begin
  if AValue <> FState then begin
    FState := AValue;
    if Assigned(FToolbarDispatch) then
      FToolbarDispatch.NotifyVisualsChanged;
  end;
end;

procedure TSpkCustomCheckbox.SetTableBehaviour(const Value: TSpkItemTableBehaviour);
begin
  FTableBehaviour := Value;
  if Assigned(FToolbarDispatch) then
     FToolbarDispatch.NotifyMetricsChanged;
end;


{ TSpkCheckbox }

constructor TSpkCheckbox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCheckboxStyle := cbsCheckbox;
end;


{ TSpkRadioButton }
constructor TSpkRadioButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCheckboxStyle := cbsRadioButton;
end;

function TSpkRadioButton.GetDefaultCaption: string;
begin
  result := 'RadioButton';
end;

procedure TSpkRadioButton.SetState(AValue: TCheckboxState);
begin
  inherited SetState(AValue);
  if (AValue = cbChecked) then
    UncheckSiblings;
end;

procedure TSpkRadioButton.UncheckSiblings;
var
  i: Integer;
  pane: TSpkPane;
begin
  if (Parent is TSpkPane) then begin
    pane := TSpkPane(Parent);
    for i:=0 to pane.Items.Count-1 do
      if (pane.items[i] is TSpkRadioButton) and (pane.items[i] <> self) then
        TSpkRadioButton(pane.items[i]).State := cbUnchecked;
  end;
end;

end.

