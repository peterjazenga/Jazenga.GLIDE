
{**********************************************************************
 Package pl_ExControls.pkg
 From PilotLogic Software House(http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit TplSideBarUnit;

interface

uses
  Messages, LMessages, Graphics, Controls, Classes, LCLIntf, ImgList, Math, Dialogs,
  ExtCtrls, Forms;

const
  clDefaultHover = $00AAFFFF;
  clDefaultSelected = clBtnHighlight;
  clDefaultHoverFont = clRed;

type
  TSideBarState = (ssNormal, ssHover, ssSelected);

  TSideBarStates = set of TSideBarState;

  TSideBarAlign = (saLeft, saCenter, saRight);

  TSideBarBullet = (sbRound, sbRectangle, sbDiamond);

  TSideBarEvent = procedure(Sender: TObject; Index, SubIndex: integer; aCaption: string) of object;

  TSideBarCustomDrawItem = procedure(Sender: TObject; aCanvas: TCanvas; Rc: TRect; Str: string;
                                     States: TSideBarStates; ImageIndex: integer) of object;

  TSideBarCustomDrawSubItem = procedure(Sender: TObject; ACanvas: TCanvas; Rc: TRect; Str: string; States: TSideBarStates) of object;

  TSideBarCustomDrawNonItem = procedure(Sender: TObject; ACanvas: TCanvas; Rc: TRect) of object;

  TSideBarCustomDrawScroller = procedure(Sender: TObject; ACanvas: TCanvas; Rc: TRect; Up: boolean; Hover: boolean) of object;

  TplSideBar = class;

  TSideBarItem = class(TCollectionItem)
  private
    FCaption: string;
    FImageIndex: TImageIndex;
    FItems: TStringList;
    FTag: integer;
    FExpanded: boolean;
    function GetSideBar: TplSideBar;
    procedure SetCaption(Value: string);
    procedure SetImageIndex(Value: TImageIndex);
    procedure SetItems(Value: TStringList);
    procedure SetExpanded(const Value: boolean);
    procedure ItemsChange(Sender: TObject);
  public
    constructor Create(aCollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function  GetDisplayName: string; //override;
    procedure Expand;
    procedure Collapse;
  published
    property SideBar: TplSideBar read GetSideBar;
    property Caption: string read FCaption write SetCaption;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
    property Items: TStringList read FItems write SetItems;
    property Tag: integer read FTag write FTag default 0;
    property Expanded: boolean read FExpanded write SetExpanded default True;
  end;

  TSideBarItems = class(TCollection)
  private
    FSideBar: TplSideBar;
    function GetItem(Index: integer): TSideBarItem;
    procedure SetItem(Index: integer; Value: TSideBarItem);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(ASideBar: TplSideBar);
    property SideBar: TplSideBar read FSideBar;
    property Items[Index: integer]: TSideBarItem read GetItem write SetItem; default;
    function Add: TSideBarItem;
    function AddItem(Item: TSideBarItem; Index: integer): TSideBarItem;
    function Insert(Index: integer): TSideBarItem;
  end;

  TSideBarItemStyle = class(TPersistent)
  private
    FSideBar: TplSideBar;
    FSelectedColor: TColor;
    FHoverColor: TColor;
    FNormalColor: TColor;
    FSelectedFont: TFont;
    FHoverFont: TFont;
    FNormalFont: TFont;
    FLineColor: TColor;
    procedure FontChange(Sender: TObject);
    procedure SetHoverFont(const Value: TFont);
    procedure SetNormalColor(const Value: TColor);
    procedure SetNormalFont(const Value: TFont);
    procedure SetSelectedColor(const Value: TColor);
    procedure SetSelectedFont(const Value: TFont);
    procedure SetLineColor(const Value: TColor);
    procedure Deactivate;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(SideBar: TplSideBar);
    destructor Destroy; override;
    procedure Activate;
  published
    property NormalFont: TFont read FNormalFont write SetNormalFont;
    property HoverFont: TFont read FHoverFont write SetHoverFont;
    property SelectedFont: TFont read FSelectedFont write SetSelectedFont;
    property NormalColor: TColor read FNormalColor write SetNormalColor default clBtnFace;
    property HoverColor: TColor read FHoverColor write FHoverColor default clDefaultHover;
    property SelectedColor: TColor read FSelectedColor write SetSelectedColor default clDefaultSelected;
    property LineColor: TColor read FLineColor write SetLineColor default clWindowText;
  end;

  TSideBarBulletStyle = class(TPersistent)
  private
    FSideBar: TplSideBar;
    FHoverColor: TColor;
    FSelectedPenColor: TColor;
    FNormalColor: TColor;
    FHoverPenColor: TColor;
    FNormalPenColor: TColor;
    FSelectedColor: TColor;
    FVisible: boolean;
    FKind: TSideBarBullet;
    FSize: integer;
    procedure SetNormalColor(const Value: TColor);
    procedure SetNormalPenColor(const Value: TColor);
    procedure SetSelectedColor(const Value: TColor);
    procedure SetSelectedPenColor(const Value: TColor);
    procedure SetKind(const Value: TSideBarBullet);
    procedure SetVisible(const Value: boolean);
    procedure SetSize(const Value: integer);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(SideBar: TplSideBar);
  published
    property Visible: boolean read FVisible write SetVisible default True;
    property Size: integer read FSize write SetSize default 5;
    property Kind: TSideBarBullet read FKind write SetKind default sbRound;
    property NormalColor: TColor read FNormalColor write SetNormalColor default clWindowText;
    property HoverColor: TColor read FHoverColor write FHoverColor default clDefaultHoverFont;
    property SelectedColor: TColor read FSelectedColor write SetSelectedColor default clWindowText;
    property NormalPenColor: TColor read FNormalPenColor write SetNormalPenColor default clWindowText;
    property HoverPenColor: TColor read FHoverPenColor write FHoverPenColor default clDefaultHoverFont;
    property SelectedPenColor: TColor read FSelectedPenColor write SetSelectedPenColor default clWindowText;
  end;

  TSideBarScrollerStyle = class(TPersistent)
  private
    FSideBar: TplSideBar;
    FHoverColor: TColor;
    FNormalArrowColor: TColor;
    FNormalColor: TColor;
    FHoverArrowColor: TColor;
    procedure SetNormalArrowColor(const Value: TColor);
    procedure SetNormalColor(const Value: TColor);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(SideBar: TplSideBar);
  published
    property NormalColor: TColor read FNormalColor write SetNormalColor default clBlack;
    property HoverColor: TColor read FHoverColor write FHoverColor default clWhite;
    property NormalArrowColor: TColor read FNormalArrowColor write SetNormalArrowColor default clWhite;
    property HoverArrowColor: TColor read FHoverArrowColor write FHoverArrowColor default clBlack;
  end;

  TplSideBar = class(TCustomPanel)
  private
    FList: TList;
    FItems: TSideBarItems;
    FAlignment: TSideBarAlign;
    FHandPointCursor: boolean;
    FItemIndex: integer;
    FSubItemIndex: integer;
    FItemHeight: integer;
    FSubItemHeight: integer;
    FImages: TImageList;
    FHoverImages: TImageList;
    FSelectedImages: TImageList;
    FOnHover: TSideBarEvent;
    FOnSelect: TSideBarEvent;
    FOnCustomDrawItem: TSideBarCustomDrawItem;
    FOnCustomDrawSubItem: TSideBarCustomDrawSubItem;
    FOnCustomDrawNonItem: TSideBarCustomDrawNonItem;
    FOnCustomDrawScroller: TSideBarCustomDrawScroller;
    LastIndex: integer;
    LastSubIndex: integer;
    LastHover, HoverIndex: integer;
    ScTop, ScBottom: TRect;
    ScTopVisible, ScBottomVisible: boolean;
    TopIndex: integer;
    FMargin: integer;
    FGroupSeparator: integer;
    IsUpdating: boolean;
    FIndent: integer;
    FAlwaysExpand: boolean;
    FItemStyle: TSideBarItemStyle;
    FSubItemStyle: TSideBarItemStyle;
    FBullets: TSideBarBulletStyle;
    FScrollers: TSideBarScrollerStyle;

    procedure CMColorChanged(var Msg: TLMessage); message CM_COLORCHANGED;
    procedure CMMouseLeave(var Msg: TLMessage); message CM_MouseLeave;
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
    procedure WMMouseWheel(var Msg: TLMMouseEvent); message LM_MOUSEWHEEL;

    procedure SetItems(Value: TSideBarItems);
    procedure SetItemIndex(Value: integer);
    procedure SetSubItemIndex(Value: integer);
    procedure SetItemHeight(Value: integer);
    procedure SetAlignment(Value: TSideBarAlign);
    procedure SetSubItemHeight(Value: integer);
    procedure SetImages(Value: TImageList);
    procedure SetHoverImages(Value: TImageList);
    procedure SetSelectedImages(Value: TImageList);
    procedure SetHandPointCursor(Value: boolean);
    procedure ClearList;
    procedure ListChange;
    procedure DoDrawItem(Index: integer);
    function  GetIndexAtPos(X, Y: integer): integer;
    function  CreateItem: TSideBarItem;
    procedure UpdateItem(Index: integer);
    procedure UpdateItems;
    procedure SetMargin(const Value: integer);
    procedure SetGroupSeparator(const Value: integer);
    procedure SetIndent(const Value: integer);
    procedure SetAlwaysExpand(const Value: boolean);
    procedure SetItemStyle(const Value: TSideBarItemStyle);
    procedure SetSubItemStyle(const Value: TSideBarItemStyle);
    procedure SetBullets(const Value: TSideBarBulletStyle);
    procedure SetScrollers(const Value: TSideBarScrollerStyle);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure Paint; override;
    procedure DrawItem(ACanvas: TCanvas; Rc: TRect; Str: string; States: TSideBarStates; ImageIndex: integer); virtual;
    procedure DrawSubItem(ACanvas: TCanvas; Rc: TRect; Str: string; States: TSideBarStates); virtual;
    procedure DrawNonItem(ACanvas: TCanvas; Rc: TRect); virtual;
    procedure DrawScroller(ACanvas: TCanvas; Rc: TRect; Up: boolean; Hover: boolean); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
  published
    property ItemStyle: TSideBarItemStyle read FItemStyle write SetItemStyle;
    property SubItemStyle: TSideBarItemStyle read FSubItemStyle write SetSubItemStyle;
    property Bullets: TSideBarBulletStyle read FBullets write SetBullets;
    property Scrollers: TSideBarScrollerStyle read FScrollers write SetScrollers;
    property Items: TSideBarItems read FItems write SetItems;
    property ItemIndex: integer read FItemIndex write SetItemIndex default -1;
    property SubItemIndex: integer read FSubItemIndex write SetSubItemIndex default -1;
    property ItemHeight: integer read FItemHeight write SetItemHeight default 30;
    property SubItemHeight: integer read FSubItemHeight write SetSubItemHeight default 18;
    property Alignment: TSideBarAlign read FAlignment write SetAlignment default saLeft;
    property Margin: integer read FMargin write SetMargin default 8;
    property GroupSeparator: integer read FGroupSeparator write SetGroupSeparator default 10;
    property Indent: integer read FIndent write SetIndent default 10;
    property AlwaysExpand: boolean read FAlwaysExpand write SetAlwaysExpand;
    property Images: TImageList read FImages write SetImages;
    property HoverImages: TImageList read FHoverImages write SetHoverImages;
    property SelectedImages: TImageList read FSelectedImages write SetSelectedImages;
    property HandPointCursor: boolean read FHandPointCursor write SetHandPointCursor default False;
    property OnHover: TSideBarEvent read FOnHover write FOnHover;
    property OnSelect: TSideBarEvent read FOnSelect write FOnSelect;
    property OnCustomDrawItem: TSideBarCustomDrawItem read FOnCustomDrawItem write FOnCustomDrawItem;
    property OnCustomDrawSubItem: TSideBarCustomDrawSubItem read FOnCustomDrawSubItem write FOnCustomDrawSubItem;
    property OnCustomDrawNonItem: TSideBarCustomDrawNonItem read FOnCustomDrawNonItem write FOnCustomDrawNonItem;
    property OnCustomDrawScroller: TSideBarCustomDrawScroller read FOnCustomDrawScroller write FOnCustomDrawScroller;
    property Anchors;
    property BevelInner;
    property BevelOuter;
    property BorderStyle default bsSingle;
    property ParentColor;
    property Color;
    property Align default alLeft;
  end;


implementation


type
  TSBInfo = record
    ItemIndex: integer;
    SubIndex: integer;
    Level: integer;
    Rc: TRect;
    Caption: string;
  end;
  PSBInfo = ^TSBInfo;

const
  SCTOPINDEX = MaxInt;
  SCBOTTOMINDEX = MaxInt - 1;




//================= TplSideBar ========================================

constructor TplSideBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  IsUpdating := True;
  Width := 200;
  Align := alLeft;
  Color := clBtnFace;
  BorderStyle := bsSingle;
  ParentColor := False;
  ParentFont := False;

  LastIndex := -1;
  LastSubIndex := -1;
  LastHover := -1;
  HoverIndex := -1;
  ScTop := Rect(0, 0, 0, 0);
  ScBottom := Rect(0, 0, 0, 0);
  ScTopVisible := False;
  ScBottomVisible := False;
  TopIndex := 0;

  FItemIndex := -1;
  FSubItemIndex := -1;
  FItemHeight := 30;
  FSubItemHeight := 18;
  FAlignment := saLeft;
  FHandPointCursor := False;
  FMargin := 8;
  FGroupSeparator := 10;
  FIndent := 10;
  FAlwaysExpand := True;

  FItemStyle := TSideBarItemStyle.Create(Self);
  FItemStyle.FNormalFont.Style := [fsBold];
  FItemStyle.FHoverFont.Style := [fsBold];
  FItemStyle.FSelectedFont.Style := [fsBold];
  FItemStyle.Activate;

  FSubItemStyle := TSideBarItemStyle.Create(Self);
  FSubItemStyle.Activate;

  FBullets := TSideBarBulletStyle.Create(Self);
  FScrollers := TSideBarScrollerStyle.Create(Self);

  FList := TList.Create;
  FItems := TSideBarItems.Create(Self);

  IsUpdating := False;

end;

destructor TplSideBar.Destroy;
begin
  ClearList;
  FList.Free;
  FScrollers.Free;
  FBullets.Free;
  FSubItemStyle.Free;
  FItemStyle.Free;
  inherited Destroy;
end;

procedure TplSideBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  i: integer;
  P: TSBInfo;//PSBInfo;
  Str: string;
  xChanged: boolean;

begin
  Str := '';
  SetFocus;

  if ScTopVisible then
  begin
    if PtInRect(ScTop, Point(X, Y)) then
    begin
      TopIndex := TopIndex - 1;
      LastIndex := LastIndex + 1;
      LastSubIndex := LastSubIndex + 1;
      Invalidate;
      Exit;
    end;
  end;

  if ScBottomVisible then
  begin
    if PtInRect(ScBottom, Point(X, Y)) then
    begin
      TopIndex := TopIndex + 1;
      LastIndex := LastIndex - 1;
      LastSubIndex := LastSubIndex - 1;
      Invalidate;
      Exit;
    end;
  end;

  i := GetIndexAtPos(X, Y);
  if (i = -1) then
  begin
    inherited;
    Exit;
  end;

  P := TSBInfo(FList[i]^);

  if (P.Level = 0) and FAlwaysExpand then
  begin
    inherited;
    Exit;
  end;

  xChanged := True;

  Str := P.Caption;

  if (P.ItemIndex = FItemIndex) then
  begin
    // on header
    if (P.SubIndex = -1) and not FAlwaysExpand then
    begin
      FSubItemIndex := -1;
      FItems[P.ItemIndex].Expanded := not FItems[P.ItemIndex].Expanded;
      LastSubIndex := -1;
    end
    else
      // on sub items
    begin
      xChanged := P.SubIndex <> FSubItemIndex;
      if xChanged then
      begin
        FItemIndex := P.ItemIndex;
        FSubItemIndex := P.SubIndex;
        DoDrawItem(LastSubIndex);
        DoDrawItem(i);
        LastSubIndex := i;
      end;
    end;
  end
  else
  begin
    FItemIndex := P.ItemIndex;
    FSubItemIndex := P.SubIndex;
    // on header
    if (FSubItemIndex = -1) then
    begin
      if FItems[FItemIndex].FExpanded then
      begin
        DoDrawItem(LastIndex);
        DoDrawItem(LastSubIndex);
        DoDrawItem(i);
        LastIndex := i;
        LastSubIndex := -1;
      end
      else
      begin
        LastIndex := i;
        LastSubIndex := -1;
        FItems[FItemIndex].FExpanded := True;
        Invalidate;
      end;
    end
    else
      // on sub items
    begin
      DoDrawItem(LastIndex);
      DoDrawItem(LastSubIndex);
      DoDrawItem(i);
      DoDrawItem(i - FSubItemIndex - 1);
      LastSubIndex := i;
      LastIndex := i - FSubItemIndex - 1;
    end;
  end;

  if xChanged then
  begin
    if Assigned(FOnSelect) then
      FOnSelect(Self, FItemIndex, FSubItemIndex, Str);
  end;

  inherited;
end;

procedure TplSideBar.MouseMove(Shift: TShiftState; X, Y: integer);
var
  i: integer;
  P: TSBInfo;//PSBInfo;
begin

  if ScTopVisible then
  begin
    if PtInRect(ScTop, Point(X, Y)) then
    begin
      if (HoverIndex <> SCTOPINDEX) then
      begin
        HoverIndex := SCTOPINDEX;
        DoDrawItem(LastHover);
        DoDrawItem(HoverIndex);
        LastHover := SCTOPINDEX;
      end;
      Exit;
    end;
  end;

  if ScBottomVisible then
  begin
    if PtInRect(ScBottom, Point(X, Y)) then
    begin
      if (HoverIndex <> SCBOTTOMINDEX) then
      begin
        HoverIndex := SCBOTTOMINDEX;
        DoDrawItem(LastHover);
        DoDrawItem(HoverIndex);
        LastHover := SCBOTTOMINDEX;
      end;
      Exit;
    end;
  end;

  i := GetIndexAtPos(X, Y);

  if (i > -1) then
  begin
    P := TSBInfo(FList[i]^);
    if (P.Level = 0) and FAlwaysExpand then
      i := -1;
  end;

  if FHandPointCursor then
  begin
    if (i = -1) then
      Cursor := crDefault
    else
      Cursor := crHandPoint;
  end;

  if (i <> HoverIndex) then
  begin
    HoverIndex := i;
    DoDrawItem(LastHover);
    if (HoverIndex > -1) then
    begin
      DoDrawItem(HoverIndex);
      P := TSBInfo(FList[i]^);

      if Assigned(FOnHover) then
        FOnHover(Self, P.ItemIndex, P.SubIndex, P.Caption);
    end;
    LastHover := HoverIndex;
  end;

  inherited;

end;

procedure TplSideBar.CMMouseLeave(var Msg: TLMessage);
begin
  if (HoverIndex <> -1) then
  begin
    HoverIndex := -1;
    DoDrawItem(LastHover);
    LastHover := HoverIndex;
  end;
  if Assigned(FOnHover) then
    FOnHover(Self, -1, -1, '');
end;

procedure TplSideBar.ClearList;
var
  x: integer;
begin
  for x := 0 to FList.Count - 1 do
    Dispose(PSBInfo(FList[x]));
  FList.Clear;
end;

procedure TplSideBar.ListChange;
var
  P: PSBInfo;
  x, y, v: integer;
  DeltaY: integer;
begin
  if IsUpdating then
    Exit;
  ClearList;
  v := 0;
  for x := 0 to FItems.Count - 1 do
  begin
    P := New(PSBInfo);
    P^.Caption := FItems[x].FCaption;
    P^.ItemIndex := x;
    P^.SubIndex := -1;
    P^.Level := 0;
    P^.Rc := Rect(FMargin, v, ClientWidth - FMargin, v + FItemHeight + FGroupSeparator);
    FList.Add(P);
    Inc(v, FItemHeight + 1 + FGroupSeparator);
    if FItems[x].FExpanded then
    begin
      for y := 0 to FItems[x].FItems.Count - 1 do
      begin
        P := New(PSBInfo);
        P^.Caption := FItems[x].FItems[y];
        P^.Level := 1;
        P^.ItemIndex := x;
        P^.SubIndex := y;
        P^.Rc := Rect(FMargin, v, ClientWidth - FMargin, v + FSubItemHeight);
        FList.Add(P);
        Inc(v, FSubItemHeight + 1);
      end;
    end;
  end;
  if (FList.Count > 0) then
  begin
    if (FList.Count > TopIndex) then
    begin
      for x := 1 to TopIndex do
      begin
        Dispose(PSBInfo(FList[0]));
        FList.Delete(0);
      end;
    end;
    DeltaY := PSBInfo(FList[0])^.Rc.Top;
    for x := 0 to FList.Count - 1 do
    begin
      P := PSBInfo(FList[x]);
      P^.Rc.Top := P^.Rc.Top - DeltaY;
      P^.Rc.Bottom := P^.Rc.Bottom - DeltaY;
    end;
    if (FAlignment = saRight) then
    begin
      ScTop := Rect(FMargin + 10, 10, FMargin + 21, 21);
      ScBottom := Rect(FMargin + 10, ClientHeight - 21, FMargin + 21, ClientHeight - 10);
    end
    else
    begin
      ScTop := Rect(ClientWidth - FMargin - 21, 10, ClientWidth - FMargin - 10, 21);
      ScBottom := Rect(ClientWidth - FMargin - 21, ClientHeight - 21, ClientWidth - FMargin - 10, ClientHeight - 10);
    end;
    ScTopVisible := TopIndex > 0;
    ScBottomVisible := PSBInfo(FList[FList.Count - 1])^.Rc.Bottom > ClientHeight;
  end;
end;

procedure TplSideBar.DoDrawItem(Index: integer);
var
  Info: PSBInfo;
  States: TSideBarStates;
  Tmp: TRect;

begin

  if (Index = SCTOPINDEX) then
  begin
    if ScTopVisible then
    begin
      if Assigned(FOnCustomDrawScroller) then
        FOnCustomdrawScroller(Self, Canvas, ScTop, True, HoverIndex = SCTOPINDEX)
      else
        DrawScroller(Canvas, ScTop, True, HoverIndex = SCTOPINDEX);
    end;
    Exit;
  end;

  if (Index = SCBOTTOMINDEX) then
  begin
    if ScBottomVisible then
    begin
      if Assigned(FOnCustomDrawScroller) then
        FOnCustomDrawScroller(Self, Canvas, ScBottom, False, HoverIndex = SCBOTTOMINDEX)
      else
        DrawScroller(Canvas, ScBottom, False, HoverIndex = SCBOTTOMINDEX);
    end;
    Exit;
  end;

  if (Index < 0) then
    Exit;

  Info := PSBInfo(FList[Index]);
  if (Index = HoverIndex) then
    States := [ssHover]
  else
    States := [ssNormal];
  if (Info^.Level = 1) then
  begin
    if (Info^.SubIndex = FSubItemIndex) and (Info^.ItemIndex = FItemIndex) then
      Include(States, ssSelected);
  end
  else
  begin
    if (Info^.ItemIndex = FItemIndex) then
      Include(States, ssSelected);
  end;

  if (Info^.Level = 1) then
  begin
    if Assigned(FOnCustomDrawSubItem) then
      FOnCustomDrawSubItem(Self, Canvas, Info^.Rc, Info^.Caption, States)
    else
      DrawSubItem(Canvas, Info^.Rc, Info^.Caption, States);
  end
  else
  begin
    if Assigned(FOnCustomDrawItem) then
      FOnCustomDrawItem(Self, Canvas, Info^.Rc, Info^.Caption, States, FItems[Info^.ItemIndex].FImageIndex)
    else
      DrawItem(Canvas, Info^.Rc, Info^.Caption, States, FItems[Info^.ItemIndex].FImageIndex);
  end;

  if IntersectRect(Tmp, Info^.Rc, ScTop) and ScTopVisible then
  begin
    if Assigned(FOnCustomDrawScroller) then
      FOnCustomdrawScroller(Self, Canvas, ScTop, True, HoverIndex = SCTOPINDEX)
    else
      DrawScroller(Canvas, ScTop, True, HoverIndex = SCTOPINDEX);
  end;

  if IntersectRect(Tmp, Info^.Rc, ScBottom) and ScBottomVisible then
  begin
    if Assigned(FOnCustomDrawScroller) then
      FOnCustomDrawScroller(Self, Canvas, ScBottom, False, HoverIndex = SCBOTTOMINDEX)
    else
      DrawScroller(Canvas, ScBottom, False, HoverIndex = SCBOTTOMINDEX);
  end;

end;

procedure TplSideBar.DrawItem(ACanvas: TCanvas; Rc: TRect; Str: string; States: TSideBarStates; ImageIndex: integer);
var
  w, h, x, y: integer;
  Img: TImageList;
  RcItem: TRect;
begin
  CopyRect(RcItem, Rc);
  with ACanvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := Color;
    FillRect(Rect(RcItem.Left, RcItem.Top, RcItem.Right, RcItem.Top + FGroupSeparator));
    RcItem.Top := RcItem.Top + FGroupSeparator;

    if (ssNormal in States) then
    begin
      if (ssSelected in States) then
      begin
        Brush.Color := FItemStyle.FSelectedColor;
        Font.Assign(FItemStyle.FSelectedFont);
      end
      else
      begin
        Brush.Color := FItemStyle.FNormalColor;
        Font.Assign(FItemStyle.FNormalFont);
      end;
    end
    else
    if (ssHover in States) then
    begin
      if (ssSelected in States) then
        Brush.Color := FItemStyle.FSelectedColor
      else
        Brush.Color := FItemStyle.FHoverColor;
      Font.Assign(FItemStyle.FHoverFont);
    end;

    FillRect(RcItem);
    Brush.Style := bsClear;
    Pen.Color := FItemStyle.FLineColor;
    MoveTo(RcItem.Left, RcItem.Bottom);
    LineTo(RcItem.Right, RcItem.Bottom);
    MoveTo(RcItem.Left, RcItem.Bottom - 1);
    LineTo(RcItem.Right, RcItem.Bottom - 1);

    InflateRect(RcItem, -8, 0);

    Img := nil;
    if (ssSelected in States) then
    begin
      if Assigned(FSelectedImages) then
        Img := FSelectedImages
      else
      if Assigned(FImages) then
        Img := FImages;
    end
    else
    if (ssNormal in States) then
    begin
      if Assigned(FImages) then
        Img := FImages;
    end
    else
    if (ssHover in States) then
    begin
      if Assigned(FHoverImages) then
        Img := FHoverImages
      else
      if Assigned(FImages) then
        Img := FImages;
    end;

    w := TextWidth(Str);
    h := TextHeight('Ag');
    x := 0;

    if Assigned(Img) and (ImageIndex > -1) then
      w := w + Img.Width + FIndent;

    case FAlignment of
      saLeft: x := RcItem.Left;
      saCenter: x := RcItem.Left + (((RcItem.Right - RcItem.Left) - w) div 2);
      saRight: x := RcItem.Right - w;
    end;

    if Assigned(Img) then
    begin
      if (ImageIndex > -1) then
      begin
        y := RcItem.Top + ((FItemHeight - Img.Height) div 2);
        if (FAlignment <> saRight) then
        begin
          // Img.Draw(ACanvas, x, y, ImageIndex, dsTransparent, itImage);
          Img.Draw(ACanvas, x, y, ImageIndex);
          Inc(x, Img.Width + FIndent);
        end
        else
          //  Img.Draw(ACanvas, RcItem.Right - Img.Width, y, ImageIndex, dsTransparent, itImage);
          Img.Draw(ACanvas, RcItem.Right - Img.Width, y, ImageIndex);
      end;
    end;

    y := RcItem.Top + (((RcItem.Bottom - RcItem.Top) - h) div 2);
    TextRect(RcItem, x, y, Str);

  end;

end;

procedure TplSideBar.DrawSubItem(ACanvas: TCanvas; Rc: TRect; Str: string; States: TSideBarStates);
const
  Separator = 7;
var
  RcItem, Rc2: TRect;
  x, y, w, h, i: integer;
  Old: TColor;
begin
  CopyRect(RcItem, Rc);
  CopyRect(Rc2, Rc);
  Rc2.Bottom := Rc2.Bottom + 1;
  case FAlignment of
    saLeft:
    begin
      Rc2.Right := Rc2.Left + FIndent;
      RcItem.Left := Rc2.Right;
    end;
    saCenter: ;
    saRight:
    begin
      Rc2.Left := Rc2.Right - FIndent;
      RcItem.Right := Rc2.Left;
    end;
  end;
  with ACanvas do
  begin
    Brush.Style := bsSolid;
    if (FAlignment <> saCenter) then
    begin
      Brush.Color := Color;
      FillRect(Rc2);
    end;

    if (ssNormal in States) then
    begin
      if (ssSelected in States) then
      begin
        Brush.Color := FSubItemStyle.FSelectedColor;
        Font.Assign(FSubItemStyle.FSelectedFont);
      end
      else
      begin
        Brush.Color := FSubItemStyle.FNormalColor;
        Font.Assign(FSubItemStyle.FNormalFont);
      end;
    end
    else
    if (ssHover in States) then
    begin
      if (ssSelected in States) then
        Brush.Color := FSubItemStyle.FSelectedColor
      else
        Brush.Color := FSubItemStyle.FHoverColor;
      Font.Assign(FSubItemStyle.FHoverFont);
    end;

    FillRect(RcItem);
    Brush.Style := bsClear;
    Pen.Color := FSubItemStyle.FLineColor;
    MoveTo(RcItem.Left, RcItem.Bottom);
    LineTo(RcItem.Right, RcItem.Bottom);
    InflateRect(RcItem, -8, 0);

    w := TextWidth(Str);
    h := TextHeight('Ag');
    x := 0;

    if FBullets.Visible then
      w := w + FBullets.Size + Separator;

    case FAlignment of
      saLeft: x := RcItem.Left;
      saCenter: x := RcItem.Left + (((RcItem.Right - RcItem.Left) - w) div 2);
      saRight: x := RcItem.Right - w;
    end;

    if FBullets.Visible then
    begin
      y := RcItem.Top + ((FSubItemHeight - FBullets.Size) div 2);
      if (FAlignment <> saRight) then
      begin
        Rc2 := Rect(x, y, x + FBullets.Size, y + FBullets.Size);
        Inc(x, FBullets.Size + Separator);
      end
      else
      begin
        i := RcItem.Right - FBullets.Size;
        Rc2 := Rect(i, y, i + FBullets.Size, y + FBullets.Size);
      end;

      Old := Pen.Color;
      Brush.Style := bsSolid;

      if (ssHover in States) then
      begin
        Brush.Color := FBullets.FHoverColor;
        Pen.Color := FBullets.FHoverPenColor;
      end
      else
      if (ssSelected in States) then
      begin
        Brush.Color := FBullets.FSelectedColor;
        Pen.Color := FBullets.FSelectedPenColor;
      end
      else
      if (ssNormal in States) then
      begin
        Brush.Color := FBullets.FNormalColor;
        Pen.Color := FBullets.FNormalPenColor;
      end;

      case FBullets.Kind of
        sbRound:
          Ellipse(Rc2);
        sbRectangle:
          Rectangle(Rc2);
        sbDiamond:
        begin
          i := FBullets.Size div 2;
          Polygon([Point(Rc2.Left + i, Rc2.Top), Point(Rc2.Left, Rc2.Top + i),
            Point(Rc2.Left + i, Rc2.Top + (i * 2)), Point(Rc2.Left + (i * 2), Rc2.Top + i)]);
        end;
      end;

      Pen.Color := Old;
      Brush.Style := bsClear;
    end;

    y := RcItem.Top + (((RcItem.Bottom - RcItem.Top) - h) div 2);
    TextRect(RcItem, x, y, Str);

  end;
end;

procedure TplSideBar.DrawNonItem(ACanvas: TCanvas; Rc: TRect);
begin
  with ACanvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := Color;
    FillRect(Rc);
  end;
end;

procedure TplSideBar.DrawScroller(ACanvas: TCanvas; Rc: TRect; Up: boolean; Hover: boolean);
var
  Old: TColor;
begin
  with ACanvas do
  begin
    Old := Pen.Color;
    Brush.Style := bsSolid;
    if Hover then
    begin
      Brush.Color := FScrollers.FHoverColor;
      Pen.Color := FScrollers.FHoverColor;
    end
    else
    begin
      Brush.Color := FScrollers.FNormalColor;
      Pen.Color := FScrollers.FNormalColor;
    end;
    RoundRect(Rc.Left, Rc.Top, Rc.Right, Rc.Bottom, 3, 3);
    if Hover then
    begin
      Brush.Color := FScrollers.FHoverArrowColor;
      Pen.Color := FScrollers.FHoverArrowColor;
    end
    else
    begin
      Brush.Color := FScrollers.FNormalArrowColor;
      Pen.Color := FScrollers.FNormalArrowColor;
    end;
    if Up then
    begin
      Polygon([Point(Rc.Left + 3, Rc.Bottom - 5), Point(Rc.Right - 4, Rc.Bottom - 5), Point(Rc.Left + 5, Rc.Top + 3)]);
    end
    else
    begin
      Polygon([Point(Rc.Left + 3, Rc.Top + 4), Point(Rc.Right - 4, Rc.Top + 4), Point(Rc.Left + 5, Rc.Bottom - 4)]);
    end;
    Pen.Color := Old;
  end;
end;

procedure TplSideBar.Paint;
var
  x, v: integer;
  Rc: TRect;

begin
  if IsUpdating then
    Exit;

  ListChange;

  if (FMargin > 0) then
  begin
    with Canvas do
    begin
      Brush.Style := bsSolid;
      Brush.Color := Color;
      FillRect(Rect(0, 0, FMargin, ClientHeight));
      FillRect(Rect(ClientWidth - FMargin, 0, ClientWidth, ClientHeight));
    end;
  end;

  v := 0;
  if (FList.Count > 0) then
  begin
    for x := 0 to FList.Count - 1 do
      DoDrawItem(x);
    v := PSBInfo(FList[FList.Count - 1])^.Rc.Bottom + 1;
  end;
  if (ClientHeight > v) then
  begin
    Rc := Rect(0, v, ClientWidth, ClientHeight);
    if Assigned(FOnCustomDrawNonItem) then
      FOnCustomDrawNonItem(Self, Canvas, Rc)
    else
      DrawNonItem(Canvas, Rc);
  end;
  DoDrawItem(SCTOPINDEX);
  DoDrawItem(SCBOTTOMINDEX);
end;

function TplSideBar.GetIndexAtPos(X, Y: integer): integer;
var
  i: integer;
  Pt: TPoint;
begin
  Result := -1;
  Pt := Point(X, Y);
  for i := 0 to FList.Count - 1 do
  begin
    if PtInRect(PSBInfo(FList[i])^.Rc, Pt) then
    begin
      Result := i;
      Break;
    end;
  end;
end;

procedure TplSideBar.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (AComponent = FImages) then
  begin
    if (Operation = opRemove) then
    begin
      FImages := nil;
      Invalidate;
    end;
  end
  else
  if (AComponent = FHoverImages) then
  begin
    if (Operation = opRemove) then
    begin
      FHoverImages := nil;
      Invalidate;
    end;
  end
  else
  if (AComponent = FSelectedImages) then
  begin
    if (Operation = opRemove) then
    begin
      FSelectedImages := nil;
      Invalidate;
    end;
  end;
end;

function TplSideBar.CreateItem: TSideBarItem;
begin
  Result := TSideBarItem.Create(FItems);
end;

procedure TplSideBar.UpdateItem(Index: integer);
var
  x, i: integer;
  P: PSBInfo;
begin
  i := -1;
  for x := 0 to FList.Count - 1 do
  begin
    P := PSBInfo(FList[x]);
    if (P^.ItemIndex = Index) and (P^.SubIndex = -1) then
    begin
      i := x;
      Break;
    end;
  end;
  DoDrawItem(i);
end;

procedure TplSideBar.UpdateItems;
begin
  Invalidate;
end;

procedure TplSideBar.SetItemIndex(Value: integer);
var
  x: integer;
  Redraw: boolean;
begin
  if (FItemIndex <> Value) then
  begin
    FItemIndex := Value;
    FSubItemIndex := -1;

    Redraw := True;
    if (FItemIndex <> -1) then
    begin
      if FItems[FItemIndex].FExpanded then
      begin
        DoDrawItem(LastIndex);
        DoDrawItem(LastSubIndex);
      end
      else
      begin
        FItems[FItemIndex].Expand;
        Redraw := False;
      end;
    end
    else
    begin
      DoDrawItem(LastIndex);
      DoDrawItem(LastSubIndex);
    end;

    if IsUpdating then
    begin
      IsUpdating := False;
      ListChange;
      IsUpdating := True;
    end;

    LastSubIndex := -1;
    LastIndex := -1;
    for x := 0 to FList.Count - 1 do
    begin
      if (PSBInfo(FList[x])^.ItemIndex = FItemIndex) then
      begin
        LastIndex := x;
        Break;
      end;
    end;
    if Redraw then
      DoDrawItem(LastIndex);
  end;
end;

procedure TplSideBar.SetSubItemIndex(Value: integer);
var
  x, i: integer;
  P: PSBInfo;
begin
  if (FItemIndex = -1) then
    Exit;
  if (FSubItemIndex <> Value) then
  begin
    FSubItemIndex := Value;
    if IsUpdating then
    begin
      IsUpdating := False;
      ListChange;
      IsUpdating := True;
    end;
    i := -1;
    for x := 0 to FList.Count - 1 do
    begin
      P := PSBInfo(FList[x]);
      if (P^.ItemIndex = FItemIndex) then
      begin
        if (P^.SubIndex = Value) then
        begin
          i := x;
          Break;
        end;
      end;
    end;
    DoDrawItem(LastSubIndex);
    LastSubIndex := i;
    if (i > -1) then
      DoDrawItem(i);
  end;
end;

procedure TplSideBar.SetItemHeight(Value: integer);
begin
  if (FItemHeight <> Value) then
  begin
    FItemHeight := Value;
    Invalidate;
  end;
end;

procedure TplSideBar.SetSubItemHeight(Value: integer);
begin
  if (FSubItemHeight <> Value) then
  begin
    FSubItemHeight := Value;
    Invalidate;
  end;
end;

procedure TplSideBar.SetAlignment(Value: TSideBarAlign);
begin
  if (FAlignment <> Value) then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;

procedure TplSideBar.SetItems(Value: TSideBarItems);
begin
  FItems.Assign(Value);
end;

procedure TplSideBar.SetImages(Value: TImageList);
begin
  if (FImages <> Value) then
  begin
    FImages := Value;
    Invalidate;
  end;
end;

procedure TplSideBar.SetHoverImages(Value: TImageList);
begin
  if (FHoverImages <> Value) then
  begin
    FHoverImages := Value;
    Invalidate;
  end;
end;

procedure TplSideBar.SetSelectedImages(Value: TImageList);
begin
  if (FSelectedimages <> Value) then
  begin
    FSelectedImages := Value;
    Invalidate;
  end;
end;

procedure TplSideBar.SetHandPointCursor(Value: boolean);
begin
  if (FHandPointCursor <> Value) then
  begin
    FHandPointCursor := Value;
    Cursor := crDefault;
  end;
end;

procedure TplSideBar.SetMargin(const Value: integer);
begin
  if (FMargin <> Value) then
  begin
    FMargin := Value;
    Invalidate;
  end;
end;

procedure TplSideBar.SetGroupSeparator(const Value: integer);
begin
  if (FGroupSeparator <> Value) then
  begin
    FGroupSeparator := Value;
    Invalidate;
  end;
end;

procedure TplSideBar.BeginUpdate;
begin
  IsUpdating := True;
end;

procedure TplSideBar.EndUpdate;
begin
  IsUpdating := False;
  Invalidate;
end;

procedure TplSideBar.SetIndent(const Value: integer);
begin
  if (FIndent <> Value) then
  begin
    FIndent := Value;
    Invalidate;
  end;
end;

procedure TplSideBar.SetAlwaysExpand(const Value: boolean);
begin
  if (FAlwaysExpand <> Value) then
  begin
    FAlwaysExpand := Value;
    if FAlwaysExpand then
    begin
      Invalidate;
    end;
  end;
end;

procedure TplSideBar.SetItemStyle(const Value: TSideBarItemStyle);
begin
  FItemStyle.Assign(Value);
end;

procedure TplSideBar.SetSubItemStyle(const Value: TSideBarItemStyle);
begin
  FSubItemStyle.Assign(Value);
end;

procedure TplSideBar.SetBullets(const Value: TSideBarBulletStyle);
begin
  FBullets := Value;
end;

procedure TplSideBar.SetScrollers(const Value: TSideBarScrollerStyle);
begin
  FScrollers := Value;
end;

procedure TplSideBar.WMMouseWheel(var Msg: TLMMouseEvent);
begin
  if (Msg.WheelDelta > 0) and ScTopVisible then
  begin
    TopIndex := TopIndex - 1;
    LastIndex := LastIndex + 1;
    LastSubIndex := LastSubIndex + 1;
    Invalidate;
    Exit;
  end
  else
  if (Msg.WheelDelta < 0) and ScBottomVisible then
  begin
    TopIndex := TopIndex + 1;
    LastIndex := LastIndex - 1;
    LastSubIndex := LastSubIndex - 1;
    Invalidate;
  end
  else
    inherited;
end;


procedure TplSideBar.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TplSideBar.WMSize(var Message: TLMSize);
begin
  LastIndex := LastIndex + TopIndex;
  LastSubIndex := LastSubIndex + TopIndex;
  TopIndex := 0;
  Invalidate;
end;

procedure TplSideBar.CMColorChanged(var Msg: TMessage);
begin
  Invalidate;
end;



//========================= TSideBarItem ===========================================

constructor TSideBarItem.Create(ACollection: TCollection);
begin
  FItems := TStringList.Create;
  FItems.OnChange := @ItemsChange;
  FImageIndex := -1;
  FExpanded := True;
  FTag := 0;
  FCaption := '';
  inherited Create(ACollection);
end;

destructor TSideBarItem.Destroy;
begin
  inherited Destroy;
  FItems.Free;
end;

procedure TSideBarItem.Assign(Source: TPersistent);
begin
  if (Source is TSideBarItem) then
  begin
    FCaption := TSideBarItem(Source).Caption;
    FImageIndex := TSideBarItem(Source).ImageIndex;
    FTag := TSideBarItem(Source).Tag;
    FExpanded := TSideBarItem(Source).Expanded;
    FItems.Assign(TSideBarItem(Source).Items);
    Changed(True);
  end;
end;

procedure TSideBarItem.ItemsChange(Sender: TObject);
begin
  Changed(True);
end;

function TSideBarItem.GetSideBar: TplSideBar;
begin
  Result := TSideBarItems(Collection).FSideBar;
end;

function TSideBarItem.GetDisplayName: string;
begin
  if (FCaption <> '') then
    Result := FCaption
  else
    Result := inherited GetDisplayName;
end;

procedure TSideBarItem.SetCaption(Value: string);
begin
  if (FCaption <> Value) then
  begin
    FCaption := Value;
    Changed(True);
  end;
end;

procedure TSideBarItem.SetImageIndex(Value: TImageIndex);
begin
  if (FImageIndex <> Value) then
  begin
    FImageIndex := Value;
    Changed(False);
  end;
end;

procedure TSideBarItem.SetItems(Value: TStringList);
begin
  FItems.Assign(Value);
  Changed(GetSideBar.ItemIndex = Index);
end;

procedure TSideBarItem.Collapse;
begin
  SetExpanded(False);
end;

procedure TSideBarItem.Expand;
begin
  SetExpanded(True);
end;

procedure TSideBarItem.SetExpanded(const Value: boolean);
begin
  if (FExpanded <> Value) then
  begin
    FExpanded := Value;
    Changed(True);
  end;
end;

//========================= TSideBarItems ===========================================

constructor TSideBarItems.Create(ASideBar: TplSideBar);
begin
  inherited Create(TSideBarItem);
  FSideBar := ASideBar;
end;

function TSideBarItems.Add: TSideBarItem;
begin
  Result := TSideBarItem(inherited Add);
end;

function TSideBarItems.AddItem(Item: TSideBarItem; Index: integer): TSideBarItem;
begin
  if (Item = nil) then
    Result := FSideBar.CreateItem
  else
  begin
    Result := Item;
    if Assigned(Item) then
    begin
      Result.Collection := Self;
      if (Index < 0) then
        Index := Count - 1;
      Result.Index := Index;
    end;
  end;
end;

function TSideBarItems.GetItem(Index: integer): TSideBarItem;
begin
  Result := TSideBarItem(inherited GetItem(Index));
end;

function TSideBarItems.GetOwner: TPersistent;
begin
  Result := FSideBar;
end;

function TSideBarItems.Insert(Index: integer): TSideBarItem;
begin
  Result := AddItem(nil, Index);
end;

procedure TSideBarItems.SetItem(Index: integer; Value: TSideBarItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TSideBarItems.Update(Item: TCollectionItem);
begin
  if (Count = 0) then
  begin
    FSideBar.LastIndex := -1;
    FSideBar.LastSubIndex := -1;
    FSideBar.LastHover := -1;
  end;
  if (Item <> nil) then
    FSideBar.UpdateItem(Item.Index)
  else
    FSideBar.UpdateItems;
end;

//======================== TSideBarItemStyle ==============================

constructor TSideBarItemStyle.Create(SideBar: TplSideBar);
begin
  inherited Create;
  FSideBar := SideBar;
  FNormalFont := TFont.Create;
  FNormalFont.Name := 'Arial';
  FHoverFont := TFont.Create;
  FHoverFont.Name := 'Arial';
  FHoverFont.Color := clDefaultHoverFont;
  FSelectedFont := TFont.Create;
  FSelectedFont.Name := 'Arial';
  FNormalColor := clBtnFace;
  FHoverColor := clDefaultHover;
  FSelectedColor := clDefaultSelected;
  FLineColor := clWindowText;
end;

destructor TSideBarItemStyle.Destroy;
begin
  FNormalFont.Free;
  FHoverFont.Free;
  FSelectedFont.Free;
  inherited Destroy;
end;

procedure TSideBarItemStyle.AssignTo(Dest: TPersistent);
begin
  if (Dest is TSideBarItemStyle) then
  begin
    with TSideBarItemStyle(Dest) do
    begin
      Deactivate;
      FNormalFont.Assign(Self.FNormalFont);
      FHoverFont.Assign(Self.FHoverFont);
      FSelectedFont.Assign(Self.FSelectedFont);
      FNormalColor := Self.FNormalColor;
      FHoverColor := Self.FHoverColor;
      FSelectedColor := Self.FSelectedColor;
      FLineColor := Self.FLineColor;
      Activate;
      FSideBar.Invalidate;
    end;
  end
  else
    inherited AssignTo(Dest);
end;

procedure TSideBarItemStyle.Activate;
begin
  FNormalFont.OnChange := @FontChange;
  FSelectedFont.OnChange := @FontChange;
end;

procedure TSideBarItemStyle.Deactivate;
begin
  FNormalFont.OnChange := nil;
  FSelectedFont.OnChange := nil;
end;

procedure TSideBarItemStyle.FontChange(Sender: TObject);
begin
  FSideBar.Invalidate;
end;

procedure TSideBarItemStyle.SetHoverFont(const Value: TFont);
begin
  FHoverFont.Assign(Value);
end;

procedure TSideBarItemStyle.SetLineColor(const Value: TColor);
begin
  if (FLineColor <> Value) then
  begin
    FLineColor := Value;
    FSideBar.Invalidate;
  end;
end;

procedure TSideBarItemStyle.SetNormalColor(const Value: TColor);
begin
  if (FNormalColor <> Value) then
  begin
    FNormalColor := Value;
    FSideBar.Invalidate;
  end;
end;

procedure TSideBarItemStyle.SetNormalFont(const Value: TFont);
begin
  FNormalFont.Assign(Value);
end;

procedure TSideBarItemStyle.SetSelectedColor(const Value: TColor);
begin
  if (FSelectedColor <> Value) then
  begin
    FSelectedColor := Value;
    FSideBar.Invalidate;
  end;
end;

procedure TSideBarItemStyle.SetSelectedFont(const Value: TFont);
begin
  FSelectedFont.Assign(Value);
end;

//================= TSideBarBulletStyle ======================================

constructor TSideBarBulletStyle.Create(SideBar: TplSideBar);
begin
  inherited Create;
  FSideBar := SideBar;
  FKind := sbRound;
  FVisible := True;
  FSize := 5;
  FNormalColor := clWindowText;
  FHoverColor := clDefaultHoverFont;
  FSelectedColor := clWindowText;
  FNormalPenColor := clWindowText;
  FHoverPenColor := clDefaultHoverFont;
  FSelectedPenColor := clWindowText;
end;

procedure TSideBarBulletStyle.AssignTo(Dest: TPersistent);
begin
  if (Dest is TSideBarBulletStyle) then
  begin
    with TSideBarBulletStyle(Dest) do
    begin
      FKind := Self.FKind;
      FVisible := Self.FVisible;
      FSize := Self.FSize;
      FNormalColor := Self.FNormalColor;
      FHoverColor := Self.FHoverColor;
      FSelectedColor := Self.FSelectedColor;
      FNormalPenColor := Self.FNormalPenColor;
      FHoverPenColor := Self.FHoverPenColor;
      FSelectedPenColor := Self.FSelectedPenColor;
      FSideBar.Invalidate;
    end;
  end
  else
    inherited AssignTo(Dest);
end;

procedure TSideBarBulletStyle.SetKind(const Value: TSideBarBullet);
begin
  if (FKind <> Value) then
  begin
    FKind := Value;
    FSideBar.Invalidate;
  end;
end;

procedure TSideBarBulletStyle.SetNormalColor(const Value: TColor);
begin
  if (FNormalColor <> Value) then
  begin
    FNormalColor := Value;
    FSideBar.Invalidate;
  end;
end;

procedure TSideBarBulletStyle.SetNormalPenColor(const Value: TColor);
begin
  if (FNormalPenColor <> Value) then
  begin
    FNormalPenColor := Value;
    FSideBar.Invalidate;
  end;
end;

procedure TSideBarBulletStyle.SetSelectedColor(const Value: TColor);
begin
  if (FSelectedColor <> Value) then
  begin
    FSelectedColor := Value;
    FSideBar.Invalidate;
  end;
end;

procedure TSideBarBulletStyle.SetSelectedPenColor(const Value: TColor);
begin
  if (FSelectedPenColor <> Value) then
  begin
    FSelectedPenColor := Value;
    FSideBar.Invalidate;
  end;
end;

procedure TSideBarBulletStyle.SetSize(const Value: integer);
begin
  if (FSize <> Value) then
  begin
    FSize := Value;
    FSideBar.Invalidate;
  end;
end;

procedure TSideBarBulletStyle.SetVisible(const Value: boolean);
begin
  if (FVisible <> Value) then
  begin
    FVisible := Value;
    FSideBar.Invalidate;
  end;
end;

//======================= TSideBarScrollerStyle ============================================

constructor TSideBarScrollerStyle.Create(SideBar: TplSideBar);
begin
  inherited Create;
  FSideBar := SideBar;
  FNormalColor := clBlack;
  FNormalArrowColor := clWhite;
  FHoverColor := clWhite;
  FHoverArrowColor := clBlack;
end;

procedure TSideBarScrollerStyle.AssignTo(Dest: TPersistent);
begin
  if (Dest is TSideBarScrollerStyle) then
  begin
    with TSideBarScrollerStyle(Dest) do
    begin
      FNormalColor := Self.FNormalColor;
      FNormalArrowColor := Self.FNormalArrowColor;
      FHoverColor := Self.FHoverColor;
      FHoverArrowColor := Self.FHoverArrowColor;
      FSideBar.Invalidate;
    end;
  end
  else
    inherited AssignTo(Dest);
end;

procedure TSideBarScrollerStyle.SetNormalArrowColor(const Value: TColor);
begin
  if (FNormalArrowColor <> Value) then
  begin
    FNormalArrowColor := Value;
    FSideBar.Invalidate;
  end;
end;

procedure TSideBarScrollerStyle.SetNormalColor(const Value: TColor);
begin
  if (FNormalColor <> Value) then
  begin
    FNormalColor := Value;
    FSideBar.Invalidate;
  end;
end;

end.
