{ RxHistoryNavigator unit

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

unit RxHistoryNavigator;

{$I rx.inc}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, rxtoolbar,
  Menus;

type
  PNavigateRec = ^TNavigateRec;
  TNavigateRec = record
    Name:string;
    Cond:string;
    Next:PNavigateRec;
  end;

type
  TRxHistoryNavigator = class;
  THistoryNavigateEvent = procedure(Sender:TRxHistoryNavigator; AInfo:string; AProcessed:boolean) of object;

  { TRxHistoryNavigator }

  TRxHistoryNavigator = class(TComponent)
  private
    FForwardBtnItem:TToolbarItem;
    FForwardBtn: string;

    FBackBtnItem:TToolbarItem;
    FBackBtn: string;
    First:PNavigateRec;
    Curr:PNavigateRec;
    FMaxPopupItems: integer;

    FOnHistoryNavigate: THistoryNavigateEvent;
    FToolPanel: TToolPanel;
    PMBack:TPopupMenu;
    PMForw:TPopupMenu;
    function GetBackBtn: string;
    function GetForwardBtn: string;
    procedure SetBackBtn(AValue: string);
    procedure SetForwardBtn(AValue: string);
    procedure SetToolPanel(AValue: TToolPanel);
    procedure ClearFromCurrent(var C:PNavigateRec);

    procedure CreateBackMenu;
    procedure CreateRetrMenu;

    function Last:PNavigateRec;
    function Prior(R:PNavigateRec):PNavigateRec;
    procedure CheckTop;
    procedure CheckBottom;
    procedure EnableAction(ActName:byte; Enable:boolean);
    procedure BackProc(Sender: TObject);
    procedure ForwardProc(Sender: TObject);
    procedure DoSetItems;
  protected
    function Navigate(ToTop:boolean; Count:integer):boolean;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ClearHistory;
    procedure AddToHistory(AHistoryCaption, AInfo:string);
  published
    property ToolPanel: TToolPanel read FToolPanel write SetToolPanel;
    property BackBtn:string read GetBackBtn write SetBackBtn;
    property ForwardBtn:string read GetForwardBtn write SetForwardBtn;
    property OnHistoryNavigate:THistoryNavigateEvent read FOnHistoryNavigate write FOnHistoryNavigate;
    property MaxPopupItems:integer read FMaxPopupItems write FMaxPopupItems default 10;
  end;


implementation
uses ActnList, rxconst;

  { TRxHistoryNavigator }

procedure TRxHistoryNavigator.SetToolPanel(AValue: TToolPanel);
begin
  if FToolPanel=AValue then Exit;
  FToolPanel:=AValue;
end;

procedure TRxHistoryNavigator.ClearFromCurrent(var C: PNavigateRec);
var
  R:PNavigateRec;
begin
  while C<>nil do
  begin
    R:=C;
    C:=C^.Next;
    Dispose(R);
  end;
end;

procedure TRxHistoryNavigator.CreateRetrMenu;
var i:integer;
    S:PNavigateRec;
    Item:TMenuItem;
begin
  if Curr=nil then exit;
  PMForw.Items.Clear;
  i:=0;
  S:=Curr^.Next;
  while (i<FMaxPopupItems) and (S<>nil) do
  begin
    Item := TMenuItem.Create(Self);
    Item.Caption := S^.Name;
    Item.OnClick := @ForwardProc;
    Item.Hint:=Format(sHistoryDesc, [S^.Cond]);
    Item.Tag:=i;
    PMForw.Items.Add(Item);
    inc(i);
    S:=S^.Next;
  end;
end;

function TRxHistoryNavigator.Last: PNavigateRec;
begin
  if First=nil then Result:=nil
  else
  begin
    Result:=First;
    while Result^.Next<>nil do Result:=Result^.Next;
  end;
end;

function TRxHistoryNavigator.Prior(R: PNavigateRec): PNavigateRec;
var
  L:PNavigateRec;
begin
  if First=nil then Result:=nil
  else
  begin
    L:=First;
    while (L^.Next<>nil) and (L^.Next<>R) do
    begin
      L:=L^.Next;
    end;
    if L^.Next=nil then Result:=nil else Result:=l;
  end;
end;

procedure TRxHistoryNavigator.CheckTop;
begin
  EnableAction(0, (Curr<>nil) and (Curr<>First));
end;


procedure TRxHistoryNavigator.CheckBottom;
begin
  EnableAction(1, (Curr<>nil) and (Curr^.Next<>nil));
end;

procedure TRxHistoryNavigator.EnableAction(ActName: byte; Enable: boolean);
begin
  if First=nil then Enable:=false;
  if ActName = 0 then
  begin
    if Assigned(FBackBtnItem) then
      (FBackBtnItem.Action as TAction).Enabled:=Enable
  end
  else
    if Assigned(FForwardBtnItem) then
      (FForwardBtnItem.Action as TAction).Enabled:=Enable
end;

procedure TRxHistoryNavigator.BackProc(Sender: TObject);
begin
  Navigate(true, (Sender as TComponent).Tag);
end;

procedure TRxHistoryNavigator.ForwardProc(Sender: TObject);
begin
  Navigate(false, (Sender as TComponent).Tag);
end;

procedure TRxHistoryNavigator.DoSetItems;
begin
  if Assigned(FToolPanel) then
  begin
    FForwardBtnItem:=FToolPanel.Items.ByActionName[FForwardBtn];
    if Assigned(FForwardBtnItem) then
    begin
      FForwardBtnItem.DropDownMenu:=PMForw;
      FForwardBtnItem.Action.OnExecute:=@ForwardProc;
    end;

    FBackBtnItem:=FToolPanel.Items.ByActionName[FBackBtn];
    if Assigned(FBackBtnItem) then
    begin
      FBackBtnItem.DropDownMenu:=PMBack;
      FBackBtnItem.Action.OnExecute:=@BackProc;
    end;
  end;
end;

function TRxHistoryNavigator.Navigate(ToTop: boolean; Count: integer): boolean;
var
  F:boolean;
  Condit: string;
begin
  Result:=false;
  if First=nil then exit;
  if ToTop then
  begin
    inc(Count);
    repeat
      Curr:=Prior(Curr);
      if Curr=nil then Curr:=First;
      Dec(Count);
    until (Count=0) or (Curr=First);
    Result:=true;
    Condit:=Curr^.Cond;
    CreateBackMenu;
    CreateRetrMenu;
  end
  else
  begin
    inc(Count);
    repeat
      if Curr^.Next<>nil then Curr:=Curr^.Next;
      Dec(Count);
    until (Count=0) or (Curr^.Next=nil);
    Result:=true;
    Condit:=Curr^.Cond;
    CreateRetrMenu;
    CreateBackMenu;
  end;
  CheckTop;
  CheckBottom;
  F:=true;
  if Assigned(FOnHistoryNavigate) and Assigned(Curr) then
    FOnHistoryNavigate(Self, Condit, F);
end;

procedure TRxHistoryNavigator.Loaded;
begin
  inherited Loaded;
  DoSetItems;
  CheckTop;
  CheckBottom;
end;

procedure TRxHistoryNavigator.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FToolPanel then
    begin
      if Assigned(FForwardBtnItem) then
      begin
        FForwardBtnItem.Action.OnExecute:=nil;
        FForwardBtnItem:=nil;
      end;

      if Assigned(FBackBtnItem) then
      begin
        FBackBtnItem.Action.OnExecute:=nil;
        FBackBtnItem:=nil;
      end;
    end
    else
    if AComponent = Self then
    begin
      if Assigned(FForwardBtnItem) then
      begin
        FForwardBtnItem.Action.OnExecute:=nil;
        FForwardBtnItem.DropDownMenu:=nil;
        FForwardBtnItem:=nil;
      end;

      if Assigned(FBackBtnItem) then
      begin
        FBackBtnItem.Action.OnExecute:=nil;
        FBackBtnItem.DropDownMenu:=nil;
        FBackBtnItem:=nil;
      end;
    end;
  end;
end;

procedure TRxHistoryNavigator.CreateBackMenu;
var i:integer;
    S:PNavigateRec;
    Item:TMenuItem;
begin
  if Curr=nil then exit;
  PMBack.Items.Clear;
  i:=0;
  S:=Curr;
  while (i<FMaxPopupItems) and (S<>First) do
  begin
    Item := TMenuItem.Create(Self);
    Item.Caption := S^.Name;
    Item.OnClick := @BackProc;
    Item.Hint:=Format(sHistoryDesc, [S^.Cond]);
    Item.Tag:=i;
    PMBack.Items.Add(Item);
    inc(i);
    S:=Prior(S);
  end;
end;

constructor TRxHistoryNavigator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  PMBack:=TPopupMenu.Create(Self);
  PMBack.Parent:=Self;
  PMForw:=TPopupMenu.Create(Self);
  PMForw.Parent:=Self;
  FMaxPopupItems:=10;
end;

destructor TRxHistoryNavigator.Destroy;
begin
  Curr:=nil;
  ClearFromCurrent(First);
  inherited Destroy;
end;

procedure TRxHistoryNavigator.ClearHistory;
begin

end;

procedure TRxHistoryNavigator.AddToHistory(AHistoryCaption, AInfo: string);
var
  R, L:PNavigateRec;
begin
  New(R);
  FillChar(R^, SizeOf(TNavigateRec), 0);
  R^.Name:=AHistoryCaption;
  R^.Cond:=AInfo;
  if First<>nil then
  begin
    L:=Last;
    if Curr<>L then ClearFromCurrent(Curr^.Next);
    Curr^.Next:=R;
    Curr:=R;
  end
  else
  begin
    First:=R;
    Curr:=R;
  end;
  CreateBackMenu;
  CreateRetrMenu;
  CheckTop;
  CheckBottom;
end;

procedure TRxHistoryNavigator.SetBackBtn(AValue: string);
begin
  if FBackBtn=AValue then Exit;

  if ForwardBtn = AValue then
    ForwardBtn:='';

  FBackBtn:=AValue;

  if Assigned(FBackBtnItem) then
  begin
    FBackBtnItem.DropDownMenu:=nil;
    FBackBtnItem.Action.OnExecute:=nil;
  end;

  if Assigned(FToolPanel) and (FBackBtn<>'') then
  begin
    FBackBtnItem:=FToolPanel.Items.ByActionName[FBackBtn];
    if Assigned(FBackBtnItem) then
    begin
      FBackBtnItem.DropDownMenu:=PMBack;
      FBackBtnItem.Action.OnExecute:=@BackProc;
    end;
  end
  else
    FBackBtnItem:=nil;
end;

function TRxHistoryNavigator.GetBackBtn: string;
begin
  if Assigned(FBackBtnItem) then
    Result:=FBackBtnItem.Action.Name
  else
    Result:=FBackBtn;
end;

function TRxHistoryNavigator.GetForwardBtn: string;
begin
  if Assigned(FForwardBtnItem) then
    Result:=FForwardBtnItem.Action.Name
  else
    Result:=FForwardBtn;
end;

procedure TRxHistoryNavigator.SetForwardBtn(AValue: string);
begin
  if FForwardBtn=AValue then Exit;

  if BackBtn = AValue then
    BackBtn:='';

  FForwardBtn:=AValue;


  if Assigned(FForwardBtnItem) then
  begin
    FForwardBtnItem.DropDownMenu:=nil;
    FForwardBtnItem.Action.OnExecute:=nil;
  end;

  if Assigned(FToolPanel) and (AValue <>'') then
  begin
    FForwardBtnItem:=FToolPanel.Items.ByActionName[FForwardBtn];
    if Assigned(FForwardBtnItem) then
    begin
      FForwardBtnItem.DropDownMenu:=PMForw;
      FForwardBtnItem.Action.OnExecute:=@ForwardProc;
    end;
  end
  else
    FForwardBtnItem:=nil;
end;

end.
