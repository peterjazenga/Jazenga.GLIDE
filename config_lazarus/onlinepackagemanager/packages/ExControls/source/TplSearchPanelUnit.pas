
{**********************************************************************
 Package pl_ExControls.pkg
 From PilotLogic Software House(http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit TplSearchPanelUnit;

{$H+}

interface

uses
  LCLType, LCLIntf,
  Classes, SysUtils, StdCtrls, ExtCtrls, Controls, Buttons, Graphics;

type

  TFindState = set of (fsFound, fsLoopAround);


  TplSearchPanelSubControl = (fscCloseButton,     // This is a close button at the left side
    fscLabelText,       // This is a label next to the close button and before the search edit
    fscSelectLocation,  // This is a combobox next to the search edit
    fscSearchForward,   // This is a forward search button next to the location combobox
    fscSearchBackwards, // This is a backward search button next to the forward one
    fscHighlight,       // This is a highlight all button (which changes its down state)
    fscStatus);         // This is a label showing the search result state

  TplSearchPanelSubControls = set of TplSearchPanelSubControl;

  TSearchEvent = procedure(Sender: TObject; incremental, backwards: boolean) of object;

  TplSearchPanel = class(TPanel)
  private
    FFindState: TFindState;
    FFoundColor: TColor;
    FHighlightChanged: TNotifyEvent;
    FHighlighting: boolean;
    FIgnoreNextKey: longint;
    FLoopAroundState: string;
    FNotFoundColor: TColor;
    FNotFoundState: string;
    FOldHeight: longint;
    FLabelText: string;
    FOnClose: TNotifyEvent;
    FOnSearch: TSearchEvent;
    FOnShow: TNotifyEvent;
    FsearchBackwardText: string;
    FSearchForwardText: string;
    FHighlightText: string;
    FSubComponents: TplSearchPanelSubControls;
    function GetHighlighting: boolean;
    function GetSearchLocation: longint;
    function GetSearchLocations: TStrings;
    function GetSearchText: string;
    procedure SetLabelText(const AValue: string);
    procedure SetFindState(const AValue: TFindState);
    procedure SetHighlightText(const AValue: string);
    procedure SetsearchBackwardText(const AValue: string);
    procedure SetSearchForwardText(const AValue: string);
    procedure SetSearchLocation(const AValue: longint);
    procedure SetSubComponents(const AValue: TplSearchPanelSubControls);

    procedure CloseBtnClick(Sender: TObject);
    procedure searchButtonClick(Sender: TObject);
    procedure searchEdtKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure searchEdtKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure highlightClick(Sender: TObject);
    procedure highlightBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
  protected
    ctCloseBtn, ctSearchForwardBtn, ctSearchBackwardBtn, ctHighlightBtn: TSpeedButton;
    ctTextLabel, ctStatusLabel: TLabel;
    ctLocationsCmb: TComboBox;
    ctSearchEdt: TEdit;
    procedure updateComponents;
    procedure moveComponents;
    procedure DoOnResize; override;
    procedure SetVisible(Value: boolean); override;
    procedure DoSearch(incremental, backwards: boolean);
  public
    property SearchText: string read GetSearchText; //Text to search, entered by the user
    property SearchLocation: longint read GetSearchLocation write SetSearchLocation;
    property SearchLocations: TStrings read GetSearchLocations;
    property Highlighting: boolean read GetHighlighting;
    property FindState: TFindState read FFindState write SetFindState;
    procedure SetFocus; override;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnSearch: TSearchEvent read FOnSearch write FOnSearch;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property OnHighlightChanged: TNotifyEvent read FHighlightChanged write FHighlightChanged;
    property OnKeyDown;

    property SubComponents: TplSearchPanelSubControls read FSubComponents write SetSubComponents;
    property LabelText: string read FLabelText write SetLabelText;
    property SearchForwardText: string read FSearchForwardText write SetSearchForwardText;
    property SearchBackwardText: string read FsearchBackwardText write SetsearchBackwardText;
    property HighlightText: string read FHighlightText write SetHighlightText;
    property FoundColor: TColor read FFoundColor write FFoundColor;
    property NotFoundColor: TColor read FNotFoundColor write FNotFoundColor;
    property NotFoundState: string read FNotFoundState write FNotFoundState;
    property LoopAroundState: string read FLoopAroundState write FLoopAroundState;
  end;


implementation


constructor TplSearchPanel.Create(TheOwner: TComponent);
var
  tempBitmap: Graphics.TBitmap;
begin
  inherited Create(TheOwner);
  Align := alBottom;
  FsearchBackwardText := '&Previous';
  FSearchForwardText := '&Next';
  FHighlightText := '&Highlight all';
  FLabelText := 'Find:';
  FNotFoundState := 'Not found!';
  FLoopAroundState := 'Loop around';
  FSubComponents := [];
  SubComponents := [fscCloseButton, fscLabelText, fscSearchForward, fscSearchBackwards, fscStatus];
  //............
  tempBitmap := Graphics.TBitmap.Create;
  tempBitmap.Width := 8;
  tempBitmap.Height := 8;
  tempBitmap.Canvas.Font := font;
  tempBitmap.Canvas.TextOut(0, 0, 'load font');
  Height := 2 * tempBitmap.Canvas.TextHeight('^MH,') + 3;
  tempBitmap.Free;
  FHighlighting := False;
  FFoundColor := $77DD77;
  FNotFoundColor := rgb($DD, $77, $77);
end;

destructor TplSearchPanel.Destroy;
begin
  ctCloseBtn.Free;
  ctSearchForwardBtn.Free;
  ctSearchBackwardBtn.Free;
  ctHighlightBtn.Free;
  ctTextLabel.Free;
  ctStatusLabel.Free;
  ctLocationsCmb.Free;
  ctSearchEdt.Free;
  inherited Destroy;
end;

//------------------

procedure TplSearchPanel.SetLabelText(const AValue: string);
begin
  if FLabelText = AValue then
    exit;
  FLabelText := AValue;
end;

procedure TplSearchPanel.SetVisible(Value: boolean);
var
  xchanged: boolean;
begin
  xchanged := Visible <> Value;
  inherited SetVisible(Value);
  if xchanged then
    if not Visible and assigned(OnClose) then
      OnClose(self)
    else if Visible and assigned(OnShow) then
      OnShow(self);
end;

procedure TplSearchPanel.SetFocus;
begin
  inherited SetFocus;
  if assigned(ctSearchEdt) then
    ctSearchEdt.SetFocus;
end;

procedure TplSearchPanel.SetHighlightText(const AValue: string);
begin
  if FHighlightText = AValue then
    exit;
  FHighlightText := AValue;
  updateComponents;
end;

procedure TplSearchPanel.SetFindState(const AValue: TFindState);
begin
  FFindState := AValue;
  if assigned(ctSearchEdt) then
    if ctSearchEdt.Text = '' then
      ctSearchEdt.Color := clWindow
    else if fsFound in AValue then
      ctSearchEdt.Color := FFoundColor
    else
      ctSearchEdt.Color := FNotFoundColor;
  if assigned(ctStatusLabel) then
    if not (fsFound in AValue) then
      ctStatusLabel.Caption := FNotFoundState
    else if (fsLoopAround in AValue) then
      ctStatusLabel.Caption := FLoopAroundState
    else
      ctStatusLabel.Caption := '';
end;

procedure TplSearchPanel.SetsearchBackwardText(const AValue: string);
begin
  if FsearchBackwardText = AValue then
    exit;
  FsearchBackwardText := AValue;
  updateComponents;
end;

procedure TplSearchPanel.SetSearchForwardText(const AValue: string);
begin
  if FSearchForwardText = AValue then
    exit;
  FSearchForwardText := AValue;
  updateComponents;
end;

procedure TplSearchPanel.SetSearchLocation(const AValue: longint);
begin
  if ctLocationsCmb <> nil then
    ctLocationsCmb.ItemIndex := AValue;
end;

procedure TplSearchPanel.SetSubComponents(const AValue: TplSearchPanelSubControls);
begin
  if FSubComponents = AValue then
    exit;
  FSubComponents := AValue;
  updateComponents;
end;

//------------------------

function TplSearchPanel.GetSearchText: string;
begin
  if assigned(ctSearchEdt) then
    Result := ctSearchEdt.Text
  else
    Result := '';
end;

function TplSearchPanel.GetSearchLocation: longint;
begin
  if ctLocationsCmb = nil then
    Result := -1
  else
    Result := ctLocationsCmb.ItemIndex;
end;

function TplSearchPanel.GetHighlighting: boolean;
begin
  Result := (ctHighlightBtn <> nil) and (ctHighlightBtn.Down);
end;

function TplSearchPanel.GetSearchLocations: TStrings;
begin
  if ctLocationsCmb = nil then
    Result := nil
  else
    Result := ctLocationsCmb.Items;
end;

procedure TplSearchPanel.CloseBtnClick(Sender: TObject);
begin
  Visible := False;
  if assigned(OnClose) then
    OnClose(self);
end;

//------------------------

procedure TplSearchPanel.SearchButtonClick(Sender: TObject);
begin
  DoSearch(False, Sender = ctSearchBackwardBtn);
end;

procedure TplSearchPanel.SearchEdtKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
var
  oldKey: word;
begin
  oldKey := key;
  if assigned(OnKeyDown) then
  begin
    OnKeyDown(self, key, shift);
    if (Key = 0) and not (key in [VK_SHIFT, VK_CONTROL, VK_MENU]) then
      FIgnoreNextKey := oldkey;
  end;
  if key = 13 then
  begin
    key := 0;
    DoSearch(False, ssShift in shift);
  end;
end;

procedure TplSearchPanel.SearchEdtKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if key = FIgnoreNextKey then
  begin
    FIgnoreNextKey := 0;
    exit;
  end;
  case key of
    0: ;
    13:
    begin
      key := 0;
      //DoSearch(false,ssShift in shift);
    end;
    vk_escape: ctCloseBtn.Click;
    //VK_DOWN,VK_UP,VK_NEXT,VK_PRIOR:; //see key down
    VK_SHIFT, VK_CONTROL, VK_MENU: ;
    else
    begin
      if assigned(OnKeyDown) then
        OnKeyDown(self, key, shift);
      DoSearch(True, False);
    end;
  end;
end;

procedure TplSearchPanel.HighlightClick(Sender: TObject);
begin
  if GetHighlighting = FHighlighting then
    exit;
  if assigned(FHighlightChanged) then
    FHighlightChanged(self);
  FHighlighting := GetHighlighting;
end;

procedure TplSearchPanel.HighlightBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if GetHighlighting = FHighlighting then
    exit;
  if assigned(FHighlightChanged) then
    FHighlightChanged(self);
  FHighlighting := GetHighlighting;
end;

//-----------------------

procedure TplSearchPanel.UpdateComponents;
const
  HSPACING: longint = 3;
var
  cx: longint;
  tempBitmap: Graphics.TBitmap;

  function setControl(Show: boolean; var control: TControl; controlClass: TControlClass; cap: string; wid: longint = -1): boolean;
  begin
    Result := False;
    if Show then
    begin
      if control = nil then
      begin
        control := controlClass.Create(Self);
        control.parent := Self;
        Result := True;
      end;
      control.Visible := True;
      control.Caption := cap;
      if tempBitmap.Canvas.TextWidth(cap) > wid + 5 then
        wid := tempBitmap.Canvas.TextWidth(cap) + 5;
      control.Left := cx;
      if wid <> -1 then
        control.Width := wid;
      cx := cx + control.Width + HSPACING;
    end
    else if control <> nil then
    begin
      control.Free;
      control := nil;
    end;
  end;

begin
  tempBitmap := Graphics.TBitmap.Create;
  tempBitmap.Width := 8;
  tempBitmap.Height := 8;
  tempBitmap.Canvas.Font := font;
  tempBitmap.Canvas.TextOut(0, 0, 'load font');
  try
    cx := HSPACING;
    if setControl(fscCloseButton in SubComponents, tcontrol(ctCloseBtn), TSpeedButton, 'X', 20) then
      ctCloseBtn.OnClick := @CloseBtnClick;
    SetControl(fscLabelText in SubComponents, tcontrol(ctTextLabel), TLabel, fLabelText); // 7777
    if SetControl(True, tcontrol(ctSearchEdt), TEdit, '', 150) then
    begin
      ctSearchEdt.OnKeyDown := @SearchEdtKeyDown;
      ctSearchEdt.OnKeyUp   := @SearchEdtKeyUp;
    end;
    if SetControl(fscSelectLocation in SubComponents, tcontrol(ctLocationsCmb), TComboBox, '', 100) then
      ctLocationsCmb.Style := csDropDownList;
    if SetControl(fscSearchForward in SubComponents, tcontrol(ctSearchForwardBtn), TSpeedButton, SearchForwardText, 75) then
      ctSearchForwardBtn.OnClick := @searchButtonClick;
    if setControl(fscSearchBackwards in SubComponents, tcontrol(ctSearchBackwardBtn), TSpeedButton, searchBackwardText, 75) then
      ctSearchBackwardBtn.OnClick := @searchButtonClick;
    if setControl(fscHighlight in SubComponents, tcontrol(ctHighlightBtn), TSpeedButton, HighlightText, 75) then
    begin
      ctHighlightBtn.OnClick   := @highlightClick;
      ctHighlightBtn.OnMouseUp := @HighlightBtnMouseUp;
      ctHighlightBtn.AllowAllUp := True;
      ctHighlightBtn.GroupIndex := 1;
    end;

    setControl(fscStatus in SubComponents, tcontrol(ctStatusLabel), TLabel, '');
    moveComponents;
  finally
    tempBitmap.Free;
  end;
end;

procedure TplSearchPanel.MoveComponents;
var
  i, maxHeight: longint;
begin
  maxHeight := 0;
  for i := 0 to ControlCount - 1 do
    if Controls[i].Height > maxHeight then
      maxHeight := Controls[i].Height;
  for i := 0 to ControlCount - 1 do
  begin
    if Controls[i] is TSpeedButton then
      Controls[i].Height := maxHeight;
    Controls[i].Top := (Height - Controls[i].Height) div 2;
  end;
end;

//-----------------------

procedure TplSearchPanel.DoOnResize;
begin
  if Height <> FOldHeight then
    moveComponents;
  FOldHeight := Height;
  inherited DoOnResize;
end;

procedure TplSearchPanel.DoSearch(incremental, backwards: boolean);
begin
  if assigned(OnSearch) then
    OnSearch(self, incremental, backwards);
end;

end.
