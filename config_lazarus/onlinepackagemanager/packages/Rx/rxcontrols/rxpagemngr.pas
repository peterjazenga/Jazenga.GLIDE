{ pagemngr unit

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

unit rxpagemngr;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls;

const
  pageNull = -1;
  DefStatusMessage = 'Step %d from %d';
  
type
  TPageOwner = TPageControl;
  TCheckPageEnabled = function (APageIndex:integer): Boolean of object;
  TPageManagerOption = (pmoSetFormCaption, pmoSetInfoControl);
  TPageManagerOptions = set of TPageManagerOption;
  { TPageManager }

  TPageManager = class(TComponent)
  private
    FNextBtn: TControl;
    FOnCheckPageEnabled: TCheckPageEnabled;
    FOnPageChanged: TNotifyEvent;
    FOptions: TPageManagerOptions;
    FPageOwner: TPageOwner;
    FPriorBtn: TControl;
    FSaveBtnNextClick: TNotifyEvent;
    FSaveBtnPriorClick: TNotifyEvent;
    FStatusControl: TControl;
    FStatusMessage: string;
    function GetPageCount: Integer;
    function GetPageIndex: Integer;
    procedure SetNextBtn(const AValue: TControl);
    procedure SetOnCheckPageEnabled(const AValue: TCheckPageEnabled);
    procedure SetOptions(const AValue: TPageManagerOptions);
    procedure SetPageIndex(const AValue: Integer);
    procedure SetPageOwner(const AValue: TPageOwner);
    procedure SetPriorBtn(const AValue: TControl);
    procedure BtnClickNext(Sender: TObject);
    procedure BtnClickPrior(Sender: TObject);
    procedure SetStatusControl(const AValue: TControl);
    procedure SetStatusMessage(const AValue: string);
    procedure SyncBtnNextClick(Sync: Boolean);
    procedure SyncBtnPriorClick(Sync: Boolean);
  protected
    function GetPriorPageIndex(Page: Integer): Integer; virtual;
    function GetNextPageIndex(Page: Integer): Integer; virtual;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure CheckBtnEnabled;
    procedure NextPage;
    procedure PriorPage;
    procedure PageChanged;virtual;
    property PageCount: Integer read GetPageCount;
    property PageIndex: Integer read GetPageIndex write SetPageIndex;
  published
    property PageOwner: TPageOwner read FPageOwner write SetPageOwner;
    property NextBtn: TControl read FNextBtn write SetNextBtn;
    property PriorBtn: TControl read FPriorBtn write SetPriorBtn;
    property OnCheckPageEnabled:TCheckPageEnabled read FOnCheckPageEnabled write SetOnCheckPageEnabled;
    property OnPageChanged: TNotifyEvent read FOnPageChanged write FOnPageChanged;
    property Options:TPageManagerOptions read FOptions write SetOptions default [];
    property StatusControl:TControl read FStatusControl write SetStatusControl;
    property StatusMessage:string read FStatusMessage write SetStatusMessage;
  end;

implementation

{ TPageManager }

procedure TPageManager.SetNextBtn(const AValue: TControl);
begin
  if FNextBtn=AValue then exit;
  SyncBtnNextClick(false);
  FNextBtn:=AValue;
  SyncBtnNextClick(true);
end;

function TPageManager.GetPageCount: Integer;
begin
  if Assigned(FPageOwner) then
    Result := FPageOwner.PageCount
  else
    Result := 0;
end;

function TPageManager.GetPageIndex: Integer;
begin
  if Assigned(PageOwner) then Result := PageOwner.ActivePageIndex
  else Result := pageNull;
end;

procedure TPageManager.SetOnCheckPageEnabled(const AValue: TCheckPageEnabled);
begin
  if FOnCheckPageEnabled=AValue then exit;
  FOnCheckPageEnabled:=AValue;
end;

procedure TPageManager.SetOptions(const AValue: TPageManagerOptions);
begin
  if FOptions=AValue then exit;
  FOptions:=AValue;
end;

procedure TPageManager.SetPageIndex(const AValue: Integer);
begin
  if Assigned(FPageOwner) and (FPageOwner.ActivePageIndex <> AValue) then
  begin
    FPageOwner.ActivePageIndex:=AValue;
    PageChanged;
  end;
end;

procedure TPageManager.SetPageOwner(const AValue: TPageOwner);
begin
  if FPageOwner=AValue then exit;
  FPageOwner:=AValue;
end;

procedure TPageManager.SetPriorBtn(const AValue: TControl);
begin
  if FPriorBtn=AValue then exit;
  SyncBtnPriorClick(false);
  FPriorBtn:=AValue;
  SyncBtnPriorClick(true);
end;

procedure TPageManager.BtnClickNext(Sender: TObject);
begin
  if Assigned(FPageOwner) then
  begin
    FPageOwner.ActivePageIndex:=GetNextPageIndex(FPageOwner.ActivePageIndex);
    PageChanged;
  end;
end;

procedure TPageManager.BtnClickPrior(Sender: TObject);
begin
  if Assigned(FPageOwner) then
  begin
    FPageOwner.ActivePageIndex:=GetPriorPageIndex(FPageOwner.ActivePageIndex);
    PageChanged;
  end;
end;

procedure TPageManager.SetStatusControl(const AValue: TControl);
begin
  if FStatusControl=AValue then exit;
  FStatusControl:=AValue;
end;

procedure TPageManager.SetStatusMessage(const AValue: string);
begin
  if FStatusMessage=AValue then exit;
  FStatusMessage:=AValue;
end;

procedure TPageManager.SyncBtnNextClick(Sync: Boolean);
begin
  if Assigned(FNextBtn) and not (csDesigning in ComponentState) then
  begin
    if Sync then
    begin
      FSaveBtnNextClick := FNextBtn.OnClick;
      FNextBtn.OnClick := @BtnClickNext;
    end
    else
    begin
      FNextBtn.OnClick := FSaveBtnNextClick;
      FSaveBtnNextClick := nil;
    end;
  end;
end;

procedure TPageManager.SyncBtnPriorClick(Sync: Boolean);
begin
  if Assigned(FPriorBtn) and not (csDesigning in ComponentState) then
  begin
    if Sync then
    begin
      FSaveBtnPriorClick := FPriorBtn.OnClick;
      FPriorBtn.OnClick := @BtnClickPrior;
    end
    else
    begin
      FPriorBtn.OnClick := FSaveBtnPriorClick;
      FSaveBtnPriorClick := nil;
    end;
  end;
end;

function TPageManager.GetPriorPageIndex(Page: Integer): Integer;
begin
  Result:=Page;
  while Page > 0 do
  begin
    Dec(Page);
    if Assigned(FOnCheckPageEnabled) then
    begin
      if FOnCheckPageEnabled(Page) then
        break
      else
      if Page = 0 then
        exit;
    end
    else
      break;
  end;
  Result:=Page;
end;

function TPageManager.GetNextPageIndex(Page: Integer): Integer;
begin
  Result:=Page;
  if not Assigned(FPageOwner) then exit;
  while Page < FPageOwner.PageCount-1  do
  begin
    Inc(Page);
    if Assigned(FOnCheckPageEnabled) then
    begin
      if FOnCheckPageEnabled(Page) then
        break
      else
      if Page = FPageOwner.PageCount then
        exit;
    end
    else
      break;
  end;
  Result:=Page;
end;

procedure TPageManager.PageChanged;
var
  S:string;
begin
  if Assigned(OnPageChanged) then
    OnPageChanged(Self);
  if FStatusMessage <> '' then
  begin
    S:=Format(FStatusMessage, [PageIndex+1, PageCount]);
    if (pmoSetFormCaption in Options) and Assigned(Owner) and (Owner is TCustomForm) then
      TCustomForm(Owner).Caption:=S;
    if (pmoSetInfoControl in Options) and Assigned(FStatusControl) then
      FStatusControl.Caption:=S;
  end;
  CheckBtnEnabled;
end;

procedure TPageManager.Loaded;
begin
  inherited Loaded;
  if not (csDesigning in ComponentState) then
  begin
    SyncBtnNextClick(true);
    SyncBtnPriorClick(true);
    PageChanged;
  end;
end;

procedure TPageManager.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FNextBtn then
    begin
      FNextBtn:=nil;
      FSaveBtnNextClick:=nil;
    end
    else
    if AComponent = FPriorBtn then
    begin
      FPriorBtn:=nil;
      FSaveBtnPriorClick:=nil;
    end
    else
    if AComponent = FPageOwner then
      FPageOwner:=nil
    else
    if AComponent = FStatusControl then
      FStatusControl:=nil;
  end;
end;

constructor TPageManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStatusMessage:=DefStatusMessage;
end;

procedure TPageManager.CheckBtnEnabled;
var
  P:integer;
begin
  P:=PageIndex;
  if Assigned(FNextBtn) then
    FNextBtn.Enabled:=GetNextPageIndex(P)>P;
  if Assigned(FPriorBtn) then
    FPriorBtn.Enabled:=GetPriorPageIndex(P)<P;
end;

procedure TPageManager.NextPage;
begin
  BtnClickNext(nil);
end;

procedure TPageManager.PriorPage;
begin
  BtnClickPrior(nil);
end;

end.
