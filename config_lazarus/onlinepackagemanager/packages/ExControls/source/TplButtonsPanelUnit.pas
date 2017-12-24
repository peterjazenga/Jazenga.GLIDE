
{**********************************************************************
 Package pl_ExControls.pkg
 From PilotLogic Software House(http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}


unit TplButtonsPanelUnit;

interface

uses
  LCLIntf, LCLType, LMessages,
  Classes, Graphics, Controls, ExtCtrls, SysUtils, ImgList, Messages,
  Forms, Themes;

type
  TplButtonsDirection = (bdHorizontal, bdVertical);

  TplButtonsPanel = class(TCustomControl)
  private
    FShowCaptions: boolean;
    FButtonHeight: integer;
    FItemIndex: integer;
    FButtonWidth: integer;
    FDirection: TplButtonsDirection;
    FImages: TCustomImageList;
    FOnChange: TNotifyEvent;
    FHints: TStringList;
    FButtons: TStringList;
    FChangeLink: TChangeLink;
    FAutoSize: boolean;
    LastHover: integer;
    procedure SetButtonHeight(const Value: integer);
    procedure SetButtons(const Value: TStringList);
    procedure SetButtonWidth(const Value: integer);
    procedure SetDirection(const Value: TplButtonsDirection);
    procedure SetHints(const Value: TStringList);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetItemIndex(const Value: integer);
    procedure SetShowCaptions(const Value: boolean);
    procedure ButtonsChange(Sender: TObject);
    function  GetRect(Index: integer): TRect;
    procedure DrawItem(Index: integer);
    procedure ChangeLinkChange(Sender: TObject);
    procedure SetAutoSize(const Value: boolean); reintroduce;
    procedure AutoResizeControl;
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetIndexFromXY(const X, Y: integer): integer;
  published
    property ButtonWidth: integer read FButtonWidth write SetButtonWidth;
    property ButtonHeight: integer read FButtonHeight write SetButtonHeight;
    property Images: TCustomImageList read FImages write SetImages;
    property Buttons: TStringList read FButtons write SetButtons;
    property Hints: TStringList read FHints write SetHints;
    property ItemIndex: integer read FItemIndex write SetItemIndex;
    property ShowCaptions: boolean read FShowCaptions write SetShowCaptions;
    property Direction: TplButtonsDirection read FDirection write SetDirection;
    property AutoSize: boolean read FAutoSize write SetAutoSize;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Align;
    property Anchors;
    property ShowHint;
  end;

implementation

// ============= TplButtonsPanel ======================================

constructor TplButtonsPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButtonWidth := 80;
  FButtonHeight := 80;
  FItemIndex := -1;
  FShowCaptions := True;
  FDirection := bdVertical;
  FButtons := TStringList.Create;
  FButtons.OnChange := @ButtonsChange;
  FHints := TStringList.Create;
  self.SetInitialBounds(0,0,120,100);
  Color := clBtnFace;
  LastHover := -1;
  FChangeLink := TChangeLink.Create;
  FChangeLink.OnChange := @ChangeLinkChange;
end;

destructor TplButtonsPanel.Destroy;
begin
  if Assigned(FImages) then
    FImages.UnRegisterChanges(FChangeLink);
  FChangeLink.Free;
  FButtons.Free;
  FHints.Free;
  inherited;
end;

procedure TplButtonsPanel.WMSize(var Message: TLMSize);
begin
  inherited;
  if FAutoSize then
    AutoResizeControl;
end;

procedure TplButtonsPanel.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (AComponent = FImages) and (Operation = opRemove) then
  begin
    if Assigned(FImages) then
      FImages.UnRegisterChanges(FChangeLink);
    FImages := nil;
    Invalidate;
  end;
  inherited;
end;

procedure TplButtonsPanel.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);


var
  i: integer;
begin
  if (Button = mbLeft) and (Shift = [ssLeft]) then
  begin
    i := GetIndexFromXY(X, Y);
    if (i <> -1) and (FItemIndex <> i) then
    begin
      SetItemIndex(i);
      if Assigned(FOnChange) then
        FOnChange(Self);
    end;
  end;
  inherited;
end;

procedure TplButtonsPanel.MouseMove(Shift: TShiftState; X, Y: integer);
var
  Index: integer;
  Rc: TRect;
  Pt: TPoint;
begin
  if ShowHint then
  begin
    Index := GetIndexFromXY(X, Y);
    if (Index <> LastHover) then
    begin
      if (Index < FHints.Count) and (Index >= 0) then
        Hint := FHints[Index]
      else
        Hint := '';
      if (Index <> -1) and (Hint <> '') then
      begin
        Rc := GetRect(Index);
        Pt := ClientToScreen(Point(Rc.Left, Rc.Bottom));
        Application.CancelHint;
        Application.ActivateHint(Pt);
      end;
      LastHover := Index;
    end;
  end;
end;

procedure TplButtonsPanel.SetButtonHeight(const Value: integer);
begin
  if (FButtonHeight <> Value) then
  begin
    FButtonHeight := Value;
    Invalidate;
  end;
end;

procedure TplButtonsPanel.SetButtonWidth(const Value: integer);
begin
  if (FButtonWidth <> Value) then
  begin
    FButtonWidth := Value;
    Invalidate;
  end;
end;

procedure TplButtonsPanel.SetDirection(const Value: TplButtonsDirection);
begin
  if (FDirection <> Value) then
  begin
    FDirection := Value;
    Invalidate;
  end;
end;

procedure TplButtonsPanel.SetButtons(const Value: TStringList);
begin
  FButtons.Assign(Value);
end;

procedure TplButtonsPanel.SetHints(const Value: TStringList);
begin
  FHints.Assign(Value);
end;

procedure TplButtonsPanel.SetImages(const Value: TCustomImageList);
begin
  if (FImages <> Value) then
  begin
    if Assigned(FImages) then
      FImages.UnRegisterChanges(FChangeLink);
    FImages := Value;
    if Assigned(FImages) then
      FImages.RegisterChanges(FChangeLink);
    Invalidate;
  end;
end;

procedure TplButtonsPanel.ChangeLinkChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TplButtonsPanel.ButtonsChange(Sender: TObject);
begin
  if (FButtons.Count = 0) then
    FItemIndex := -1;
  if (FItemIndex >= FButtons.Count) then
    FItemIndex := -1;
  AutoResizeControl;
  Invalidate;
end;

procedure TplButtonsPanel.SetItemIndex(const Value: integer);
var
  Old: integer;
begin
  if (FItemIndex <> Value) then
  begin
    Old := FItemIndex;
    FItemIndex := Value;
    if (FButtons.Count = 0) then
      FItemIndex := -1;
    if (FItemIndex >= FButtons.Count) then
      FItemIndex := -1;
    if (Old <> -1) then
      DrawItem(Old);
    if (FItemIndex <> -1) then
      DrawItem(FItemIndex);
  end;
end;

procedure TplButtonsPanel.SetShowCaptions(const Value: boolean);
begin
  if (FShowCaptions <> Value) then
  begin
    FShowCaptions := Value;
    Invalidate;
  end;
end;

function TplButtonsPanel.GetRect(Index: integer): TRect;
var
  l, t: integer;
begin
  if (FDirection = bdHorizontal) then
  begin
    l := Index * FButtonWidth;
    t := 0;
  end
  else
  begin
    l := 0;
    t := Index * FButtonHeight;
  end;
  Result := Rect(l, t, l + FButtonWidth, t + FButtonHeight);
end;

function TplButtonsPanel.GetIndexFromXY(const X, Y: integer): integer;
begin
  Result := -1;
  if (FDirection = bdHorizontal) then
  begin
    if (Y < FButtonHeight) then
    begin
      Result := (X div FButtonWidth) - 1;
      if ((X mod FButtonWidth) > 0) then
        Inc(Result);
    end;
  end
  else
  begin
    if (X < FButtonWidth) then
    begin
      Result := (Y div FButtonHeight) - 1;
      if ((Y mod FButtonHeight) > 0) then
        Inc(Result);
    end;
  end;
  if (FButtons.Count = 0) or (Result >= FButtons.Count) then
    Result := -1;
end;

procedure TplButtonsPanel.AutoResizeControl;
begin
  if FAutoSize and (FButtons.Count > 0) then
  begin
    if (FDirection = bdHorizontal) then
    begin
      Width := FButtons.Count * FButtonWidth;
      Height := FButtonHeight;
    end
    else
    begin
      Width := FButtonWidth;
      Height := FButtons.Count * FButtonHeight;
    end;
  end;
end;

procedure TplButtonsPanel.SetAutoSize(const Value: boolean);
begin
  if (FAutoSize <> Value) then
  begin
    FAutoSize := Value;
    AutoResizeControl;
  end;
end;

procedure TplButtonsPanel.Paint;
var
  x: integer;
begin
  for x := 0 to Buttons.Count - 1 do
    DrawItem(x);
end;

procedure TplButtonsPanel.DrawItem(Index: integer);
var
  Rc: TRect;
  l, t, h, ih: integer;
  //Details: TThemedElementDetails;
begin
  Rc := GetRect(Index);
  with Canvas do
  begin
    Brush.Style := bsSolid;

    if (Index = FItemIndex) then
    begin
      Brush.Color := clBtnShadow;
      FillRect(Rc);
      Canvas.Frame3D(Rc, clBtnShadow, clBtnHighlight, 1);  // ct9999
    end
    else
    begin
      Brush.Color := clBtnFace;
      FillRect(Rc);
      Canvas.Frame3D(Rc, clBtnHighlight, clBtnShadow, 1);  // ct9999
    end;

    h := 0;
    ih := 0;
    if Assigned(FImages) then
    begin
      ih := FImages.Height;
      h := ih;
    end;
    if FShowCaptions then
      h := h + TextHeight('Ag');
    t := Rc.Top + ((FButtonHeight - h) div 2);
    if Assigned(FImages) then
    begin
      l := Rc.Left + ((FButtonWidth - FImages.Width) div 2);
      FImages.Draw(Canvas, l, t, Index);
    end;
    if FShowCaptions then
    begin
      t := t + ih;
      l := Rc.Left + ((FButtonWidth - TextWidth(FButtons[Index])) div 2);
      Brush.Style := bsClear;
      TextRect(Rc, l, t, FButtons[Index]);
    end;
  end;
end;

end.
