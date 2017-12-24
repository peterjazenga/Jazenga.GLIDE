{ RxViewsPanel unit

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


unit RxViewsPanel;

{$I rx.inc}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls, LCLType;

type
  TRxViewsPanel = class;
  TRxViewsPanelItem = class;

  TSelectViewEvent = procedure (Sender: TObject; ItemIndex:integer; const Item:TRxViewsPanelItem) of object;
  { TRxViewsPanelItem }

  TRxViewsPanelItem = class(TCollectionItem)
  private
    FButton: TSpeedButton;
    FImageIndex: integer;
    FLabel:TLabel;
    function GetAction: TBasicAction;
    function GetCaption: string;
    function GetEnabled: Boolean;
    function GetHint: TTranslateString;
    function GetImageIndex: integer;
    function GetTag: Longint;
    function GetVisible: boolean;
    procedure SetAction(const AValue: TBasicAction);
    procedure SetCaption(const AValue: string);
    procedure SetEnabled(const AValue: Boolean);
    procedure SetHint(const AValue: TTranslateString);
    procedure SetImageIndex(const AValue: integer);
    procedure SetTag(const AValue: Longint);
    procedure SetVisible(const AValue: boolean);
    procedure UpdatePosition;
    procedure UpdateImage;
    procedure DoViewButtonClick(Sender:TObject);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  published
    property Action:TBasicAction read GetAction write SetAction;
    property Visible:boolean read GetVisible write SetVisible;
    property Caption:string read GetCaption Write SetCaption;
    property Tag: Longint read GetTag write SetTag default 0;
    property ImageIndex:integer read GetImageIndex write SetImageIndex;
    property Hint:TTranslateString read GetHint write SetHint;
    property Enabled: Boolean read GetEnabled write SetEnabled default True;
  end;


  { TRxViewsPanelItems }

  TRxViewsPanelItems = class(TCollection)
  private
    FRxViewsPanel:TRxViewsPanel;
    function GetPanelItem(Index: Integer): TRxViewsPanelItem;
    procedure SetPanelItem(Index: Integer; const AValue: TRxViewsPanelItem);
  protected
    procedure Update(Item: TCollectionItem);override;
  public
    constructor Create(ARxViewsPanel: TRxViewsPanel);
    property Items[Index: Integer]: TRxViewsPanelItem read GetPanelItem write SetPanelItem; default;
    procedure UpdateImages;
  end;

  { TRxViewsPanel }

  TRxViewsPanel = class(TCustomPanel)
  private
    FButtonHeght: integer;
    FImageList: TImageList;
    FItemIndex: integer;
    FItems:TRxViewsPanelItems;
    FOnSelectViewEvent: TSelectViewEvent;
    function GetItems: TRxViewsPanelItems;
    procedure SetButtonHeght(const AValue: integer);
    procedure SetImageList(const AValue: TImageList);
    procedure SetItemIndex(const AValue: integer);
    procedure SetItems(const AValue: TRxViewsPanelItems);
    procedure InternalSelectView(Item:TRxViewsPanelItem);
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property Anchors;
    property ButtonHeght:integer read FButtonHeght write SetButtonHeght;
    property Color default clGrayText;
    property Items:TRxViewsPanelItems read GetItems write SetItems;
    property ImageList:TImageList read FImageList write SetImageList;
    property OnSelectViewEvent:TSelectViewEvent read FOnSelectViewEvent write FOnSelectViewEvent;
    property ItemIndex:integer read FItemIndex write SetItemIndex;

    property Alignment;
    property AutoSize;
    property BorderSpacing;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BidiMode;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FullRepaint;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UseDockManager default True;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnGetDockCaption;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;

  end;


implementation


{ TRxViewsPanel }

function TRxViewsPanel.GetItems: TRxViewsPanelItems;
begin
  Result:=FItems;
end;

procedure TRxViewsPanel.SetButtonHeght(const AValue: integer);
var
  I:integer;
begin
  if FButtonHeght=AValue then exit;
  FButtonHeght:=AValue;
  for i:=0 to FItems.Count - 1 do
    Items[i].FButton.Height:=AValue;
end;

procedure TRxViewsPanel.SetImageList(const AValue: TImageList);
begin
  if FImageList=AValue then exit;
  FImageList:=AValue;
  FItems.UpdateImages;
end;

procedure TRxViewsPanel.SetItemIndex(const AValue: integer);
begin
  if FItemIndex=AValue then exit;
  if (AValue < 0) or (AValue > FItems.Count - 1) then exit;
  FItemIndex:=AValue;
  Items[AValue].FButton.Click;
  Items[AValue].FButton.Down:=true;
end;

procedure TRxViewsPanel.SetItems(const AValue: TRxViewsPanelItems);
begin
  FItems.Assign(AValue);
end;

procedure TRxViewsPanel.InternalSelectView(Item: TRxViewsPanelItem);
begin
  FItemIndex:=Item.Index;
  if Assigned(FOnSelectViewEvent) then
    FOnSelectViewEvent(Self, Item.Index, Item);
end;

procedure TRxViewsPanel.Loaded;
begin
  inherited Loaded;
  FItems.Update(nil);
  FItems.UpdateImages;
  if (FItems.Count>0) and (FItemIndex>-1) and (FItemIndex < FItems.Count) then
    FItems[FItemIndex].FButton.Down:=true;
end;

constructor TRxViewsPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BevelOuter:=bvLowered;
  Caption:='';
  if Assigned(AOwner) then
    Align:=alLeft;
  Color:=clGrayText;
  FItems:=TRxViewsPanelItems.Create(Self);

  ControlStyle:=ControlStyle - [csSetCaption, csAcceptsControls];
  FButtonHeght:=50;
end;

destructor TRxViewsPanel.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

{ TRxViewsPanelItem }

function TRxViewsPanelItem.GetAction: TBasicAction;
begin
  Result:=FButton.Action;
end;

function TRxViewsPanelItem.GetCaption: string;
begin
  Result:=FLabel.Caption;
end;

function TRxViewsPanelItem.GetEnabled: Boolean;
begin
  Result:=FButton.Enabled;
end;

function TRxViewsPanelItem.GetHint: TTranslateString;
begin
  Result:=FButton.Hint;
end;

function TRxViewsPanelItem.GetImageIndex: integer;
begin
{  if Assigned(FButton.Action) then
    Result:=FButton.Action.;}
  Result:=FImageIndex;
//  FButton.Glyph.;
end;

function TRxViewsPanelItem.GetTag: Longint;
begin
  Result:=FButton.Tag;
end;

function TRxViewsPanelItem.GetVisible: boolean;
begin
  Result:=FButton.Visible;
end;

procedure TRxViewsPanelItem.SetAction(const AValue: TBasicAction);
begin
  FButton.Action:=AValue;
end;

procedure TRxViewsPanelItem.SetCaption(const AValue: string);
begin
  FLabel.Caption:=AValue;
end;

procedure TRxViewsPanelItem.SetEnabled(const AValue: Boolean);
begin
  FButton.Enabled:=AValue;
  FLabel.Enabled:=AValue;
end;

procedure TRxViewsPanelItem.SetHint(const AValue: TTranslateString);
begin
  FButton.Hint:=AValue;
end;

procedure TRxViewsPanelItem.SetImageIndex(const AValue: integer);
begin
  if FImageIndex=AValue then exit;
  FImageIndex:=AValue;
  UpdateImage;
end;

procedure TRxViewsPanelItem.SetTag(const AValue: Longint);
begin
  FButton.Tag:=AValue;
end;

procedure TRxViewsPanelItem.SetVisible(const AValue: boolean);
begin
  FButton.Visible:=AValue;
  FLabel.Visible:=AValue;
end;

procedure TRxViewsPanelItem.UpdatePosition;
var
  PP:TRxViewsPanelItem;
begin
  if Index <> 0 then
  begin
    PP:=TRxViewsPanelItems(Collection).GetPanelItem(Index - 1);
    if Assigned(PP.FLabel) then
    begin
      FButton.Top:=PP.FLabel.Top + PP.FLabel.Height;
    end;
  end;
  FLabel.Top:=FButton.Top + FButton.Height;
end;

procedure TRxViewsPanelItem.UpdateImage;
var
  VP:TRxViewsPanel;
begin
  VP:=TRxViewsPanelItems(Collection).FRxViewsPanel;
  if Assigned(VP.FImageList) then
    VP.FImageList.GetBitmap(FImageIndex, FButton.Glyph);
end;

procedure TRxViewsPanelItem.DoViewButtonClick(Sender: TObject);
begin
  TRxViewsPanelItems(Collection).FRxViewsPanel.InternalSelectView(Self);
end;

function TRxViewsPanelItem.GetDisplayName: string;
begin
  if FLabel.Caption<> '' then
    Result:=FLabel.Caption
  else
    Result:=inherited GetDisplayName;
end;

constructor TRxViewsPanelItem.Create(ACollection: TCollection);
var
  VP:TRxViewsPanel;
begin
  inherited Create(ACollection);
  VP:=TRxViewsPanelItems(ACollection).FRxViewsPanel;
  FImageIndex:=-1;

  FButton:=TSpeedButton.Create(VP);
//  FButton.Align:=alTop;
  FButton.ShowCaption:=false;
  FButton.Transparent:=true;
  FButton.GroupIndex:=1;
  FButton.Height:=VP.FButtonHeght;
  FButton.Parent:=VP;


  FLabel:=TLabel.Create(VP);
//  FLabel.Align:=alTop;
  FLabel.WordWrap:=true;
  FLabel.Alignment:=taCenter;
  FLabel.AutoSize:=true;
  FLabel.Parent:=VP;

  FButton.BorderSpacing.Around:=6;
  FLabel.BorderSpacing.Around:=6;

  FButton.AnchorSide[akLeft].Control:=VP;
  FButton.AnchorSide[akRight].Control:=VP;
  FButton.AnchorSide[akRight].Side:=asrBottom;
  FButton.Anchors:=[akTop, akLeft, akRight];
  FButton.OnClick:=@DoViewButtonClick;

  FLabel.AnchorSide[akTop].Control:=FButton;
  FLabel.AnchorSide[akLeft].Control:=VP;
  FLabel.AnchorSide[akRight].Control:=VP;
  FLabel.AnchorSide[akRight].Side:=asrBottom;
  FLabel.Anchors:=[akTop, akLeft, akRight];
  FLabel.Top:=FButton.Top + FButton.Height;

  UpdatePosition;
end;

destructor TRxViewsPanelItem.Destroy;
begin
  FreeAndNil(FButton);
  FreeAndNil(FLabel);
  inherited Destroy;
end;

{ TRxViewsPanelItems }

function TRxViewsPanelItems.GetPanelItem(Index: Integer): TRxViewsPanelItem;
begin
  result := TRxViewsPanelItem( inherited Items[Index] );

end;

procedure TRxViewsPanelItems.SetPanelItem(Index: Integer;
  const AValue: TRxViewsPanelItem);
begin
  Items[Index].Assign( AValue );
end;

procedure TRxViewsPanelItems.Update(Item: TCollectionItem);
var
  i:integer;
  P, P1:TRxViewsPanelItem;
begin
  inherited Update(Item);
  if not Assigned(Item) then
  begin
    for i:=0 to Count - 1 do
    begin
      P:=GetPanelItem(I);
      if Assigned(P.FButton) and Assigned(P.FLabel) then
      begin
        if i=0 then
        begin
          P.FButton.AnchorSide[akTop].Control:=FRxViewsPanel;
          P.FButton.AnchorSide[akTop].Side:=asrTop;

          P.FLabel.AnchorSide[akTop].Control:=P.FButton;
          P.FLabel.AnchorSide[akTop].Side:=asrBottom;


        end
        else
        begin
          P1:=GetPanelItem(I-1);
          if Assigned(P1.FButton) and Assigned(P1.FLabel) then
          begin
            P.FButton.AnchorSide[akTop].Control:=P1.FLabel;
            P.FButton.AnchorSide[akTop].Side:=asrBottom;

            P.FLabel.AnchorSide[akTop].Control:=P.FButton;
            P.FLabel.AnchorSide[akTop].Side:=asrBottom;
          end;
        end;
        P.FButton.AnchorSide[akLeft].Control:=FRxViewsPanel;
        P.FButton.AnchorSide[akRight].Control:=FRxViewsPanel;
        P.FButton.AnchorSide[akRight].Side:=asrBottom;

        P.FLabel.AnchorSide[akTop].Control:=P.FButton;
        P.FLabel.AnchorSide[akLeft].Control:=FRxViewsPanel;
        P.FLabel.AnchorSide[akRight].Control:=FRxViewsPanel;
        P.FLabel.AnchorSide[akRight].Side:=asrBottom;
      end;
    end;
  end;
end;

constructor TRxViewsPanelItems.Create(ARxViewsPanel: TRxViewsPanel);
begin
  inherited Create(TRxViewsPanelItem);
  FRxViewsPanel:=ARxViewsPanel;
end;

procedure TRxViewsPanelItems.UpdateImages;
var
  i:integer;
begin
  for I:=0 to Count - 1 do
    Items[i].UpdateImage;
end;

end.
