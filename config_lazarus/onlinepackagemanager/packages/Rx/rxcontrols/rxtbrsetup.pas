{ rxtbrsetup unit

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

unit rxtbrsetup;

{$I rx.inc}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  rxtoolbar, StdCtrls, ComCtrls, ExtCtrls, ButtonPanel;

type

  { TToolPanelSetupForm }

  TToolPanelSetupForm = class(TForm)
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    BitBtn6: TBitBtn;
    ButtonPanel1: TButtonPanel;
    cbShowHint: TCheckBox;
    cbTransp: TCheckBox;
    cbFlatBtn: TCheckBox;
    cbShowCaption: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    ListBtnAvaliable: TListBox;
    ListBtnVisible: TListBox;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ListBox1DrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure ListBtnAvaliableClick(Sender: TObject);
    procedure cbShowCaptionChange(Sender: TObject);
    procedure ListBtnVisibleDblClick(Sender: TObject);
  private
    procedure FillItems(List:TStrings; AVisible:boolean);
    procedure UpdateStates;
    procedure Localize;
  public
    FToolPanel:TToolPanel;
    constructor CreateSetupForm(AToolPanel:TToolPanel);
  end; 

var
  ToolPanelSetupForm: TToolPanelSetupForm;

implementation
uses rxvclutils, ActnList, rxboxprocs, rxconst, LCLProc, rxShortCutUnit;

{$R *.lfm}

type
  THackToolPanel = class(TToolPanel);
{ TToolPanelSetupForm }

procedure TToolPanelSetupForm.FormDestroy(Sender: TObject);
begin
  if Assigned(FToolPanel) then
  begin
    THackToolPanel(FToolPanel).SetCustomizing(false);
    THackToolPanel(FToolPanel).FCustomizer:=nil;
  end;
end;

procedure TToolPanelSetupForm.FormResize(Sender: TObject);
begin
  ListBtnVisible.Width:=BitBtn6.Left - 4 - ListBtnVisible.Left;
  ListBtnAvaliable.Left:=BitBtn6.Left + BitBtn6.Width + 4;
  ListBtnAvaliable.Width:=Width - ListBtnAvaliable.Left - 4;
  Label1.Left:=ListBtnAvaliable.Left;
end;

procedure TToolPanelSetupForm.ListBox1DrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  Offset:integer;
  P:TToolbarItem;
  BtnRect:TRect;
  Cnv:TCanvas;
  C: TColor;
  S: String;
begin
  Cnv:=(Control as TListBox).Canvas;
  C:=Cnv.Brush.Color;
  Cnv.FillRect(ARect);       { clear the rectangle }
  P:=TToolbarItem((Control as TListBox).Items.Objects[Index]);
  if Assigned(P) then
  begin
    if Assigned(FToolPanel.ImageList) and Assigned(P.Action) then
    begin
      if (P.Action is TCustomAction) and
         (TCustomAction(P.Action).ImageIndex>-1) and
         (TCustomAction(P.Action).ImageIndex < FToolPanel.ImageList.Count) then
      begin
        Offset := 2;
        BtnRect.Top:=ARect.Top + 2;
        BtnRect.Left:=ARect.Left + Offset;
        BtnRect.Right:=BtnRect.Left + FToolPanel.BtnWidth;
        BtnRect.Bottom:=BtnRect.Top + FToolPanel.BtnHeight;
        Cnv.Brush.Color := clBtnFace;
        Cnv.FillRect(BtnRect);
        DrawButtonFrame(Cnv, BtnRect, false, false);
        FToolPanel.ImageList.Draw(Cnv, BtnRect.Left + (FToolPanel.BtnWidth - FToolPanel.ImageList.Width) div 2,
                                       BtnRect.Top + (FToolPanel.BtnHeight - FToolPanel.ImageList.Height) div 2,
                                       TCustomAction(P.Action).ImageIndex, True);
        Offset:=BtnRect.Right;
      end;
      Offset := Offset + 6;
      Cnv.Brush.Color:=C;
      Cnv.TextOut(ARect.Left + Offset, (ARect.Top + ARect.Bottom  - Cnv.TextHeight('Wg')) div 2, TCustomAction(P.Action).Caption);  { display the text }
      if (P.Action is TAction) then
        if TAction(P.Action).ShortCut <> 0 then
        begin
          S:=ShortCutToText(TAction(P.Action).ShortCut);
          if S<> '' then
            Cnv.TextOut(ARect.Right - Cnv.TextWidth(S) - 2, (ARect.Top + ARect.Bottom  - Cnv.TextHeight('Wg')) div 2, S);  { display the shortut caption }
        end;
    end;
  end;
end;

procedure TToolPanelSetupForm.ListBtnAvaliableClick(Sender: TObject);
begin
  with (Sender as TListBox) do
  begin
    if (ItemIndex>-1) and (ItemIndex<Items.Count) then
    begin
      Panel1.Caption:=TCustomAction(TToolbarItem(Items.Objects[ItemIndex]).Action).Hint;
      if Sender = ListBtnVisible then
        cbShowCaption.Checked:=TToolbarItem(Items.Objects[ItemIndex]).ShowCaption;
    end;
  end;
  UpdateStates;
end;

procedure TToolPanelSetupForm.cbShowCaptionChange(Sender: TObject);
begin
  if (ListBtnVisible.ItemIndex>-1) and (ListBtnVisible.ItemIndex<ListBtnVisible.Items.Count) then
    TToolbarItem(ListBtnVisible.Items.Objects[ListBtnVisible.ItemIndex]).ShowCaption:=cbShowCaption.Checked;
end;

procedure TToolPanelSetupForm.ListBtnVisibleDblClick(Sender: TObject);
var
  Act: TBasicAction;
  A: TShortCut;
begin
  if FToolPanel.CustomizeShortCut then
  if (TListBox(Sender).ItemIndex>-1) and (TListBox(Sender).ItemIndex<TListBox(Sender).Items.Count) then
  begin
    Act:=TToolbarItem(TListBox(Sender).Items.Objects[TListBox(Sender).ItemIndex]).Action;
    if Act is TCustomAction then
    begin
      A:=TCustomAction(Act).ShortCut;
      Hide;
      if RxSelectShortCut(A) then
      begin
        TCustomAction(Act).ShortCut:=A;
        TListBox(Sender).Invalidate;
      end;
      Show;
    end;
  end;
end;

procedure TToolPanelSetupForm.FillItems(List: TStrings; AVisible: boolean);
var
  i, p:integer;
begin
  List.Clear;
  for i:=0 to FToolPanel.Items.Count - 1 do
  begin
    if (FToolPanel.Items[i].Visible = AVisible) and Assigned(FToolPanel.Items[i].Action) then
    begin
      P:=List.Add(FToolPanel.Items[i].Action.Name);
      List.Objects[P]:=FToolPanel.Items[i];
    end;
  end;
end;

procedure TToolPanelSetupForm.UpdateStates;
var
  i:integer;
begin
  for I:=0 to ListBtnVisible.Items.Count - 1 do
    TToolbarItem(ListBtnVisible.Items.Objects[i]).Visible:=true;

  for I:=0 to ListBtnAvaliable.Items.Count - 1 do
    TToolbarItem(ListBtnAvaliable.Items.Objects[i]).Visible:=false;
    
  BitBtn6.Enabled:=ListBtnVisible.Items.Count>0;
  BitBtn5.Enabled:=ListBtnVisible.Items.Count>0;
  cbShowCaption.Enabled:=(ListBtnVisible.Items.Count>0) and (ListBtnVisible.ItemIndex>=0);

  BitBtn4.Enabled:=ListBtnAvaliable.Items.Count>0;
  BitBtn3.Enabled:=ListBtnAvaliable.Items.Count>0;

  cbFlatBtn.Checked:=tpFlatBtns in FToolPanel.Options;
end;

procedure TToolPanelSetupForm.Localize;
begin
  Caption:=sToolPanelSetup;
  TabSheet1.Caption:=sVisibleButtons;
  TabSheet2.Caption:=sOptions;
  Label2.Caption:=sVisibleButtons;
  Label2.Caption:=sVisibleButtons;
  Label1.Caption:=sAvaliableButtons;
  cbShowCaption.Caption:=sShowCaption;
  RadioGroup2.Caption:=sToolBarStyle;
  RadioGroup2.Items.Clear;
  RadioGroup2.Items.Add(sToolBarStyle1);
  RadioGroup2.Items.Add(sToolBarStyle2);
  RadioGroup2.Items.Add(sToolBarStyle3);
  cbFlatBtn.Caption:=sFlatButtons;
  cbTransp.Caption:=sTransparent;
  cbShowHint.Caption:=sShowHint;
  RadioGroup1.Caption:=sButtonAlign;
  RadioGroup1.Items.Clear;
  RadioGroup1.Items.Add(sButtonAlign1);
  RadioGroup1.Items.Add(sButtonAlign2);
  RadioGroup1.Items.Add(sButtonAlign3);
end;

procedure TToolPanelSetupForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction:=caFree;
end;

procedure TToolPanelSetupForm.CheckBox1Change(Sender: TObject);
var
  tpo:TToolPanelOptions;
begin
  tpo:=FToolPanel.Options;
  if cbTransp.Checked then
    tpo:=tpo + [tpTransparentBtns]
  else
    tpo:=tpo - [tpTransparentBtns];

  FToolPanel.ToolBarStyle:=TToolBarStyle(RadioGroup2.ItemIndex);

  if cbFlatBtn.Checked then
    tpo:=tpo + [tpFlatBtns]
  else
    tpo:=tpo - [tpFlatBtns];

  FToolPanel.ShowHint:=cbShowHint.Checked;
  FToolPanel.Options:=tpo;
  
  FToolPanel.ButtonAllign:=TToolButtonAllign(RadioGroup1.ItemIndex);
  cbFlatBtn.Checked:=tpFlatBtns in FToolPanel.Options;
end;

procedure TToolPanelSetupForm.BitBtn4Click(Sender: TObject);
begin
  BoxMoveSelectedItems(ListBtnAvaliable, ListBtnVisible);
  UpdateStates;
end;

procedure TToolPanelSetupForm.BitBtn3Click(Sender: TObject);
begin
  BoxMoveAllItems(ListBtnAvaliable, ListBtnVisible);
  UpdateStates;
end;

procedure TToolPanelSetupForm.BitBtn5Click(Sender: TObject);
begin
  BoxMoveSelectedItems(ListBtnVisible, ListBtnAvaliable);
  UpdateStates;
end;

procedure TToolPanelSetupForm.BitBtn6Click(Sender: TObject);
begin
  BoxMoveAllItems(ListBtnVisible, ListBtnAvaliable);
  UpdateStates;
end;

constructor TToolPanelSetupForm.CreateSetupForm(AToolPanel: TToolPanel);
begin
  inherited Create(AToolPanel);
  Localize;
  PageControl1.ActivePageIndex:=0;
  FormResize(nil);
  FToolPanel:=AToolPanel;


  cbFlatBtn.Checked:=tpFlatBtns in FToolPanel.Options;
  cbTransp.Checked:=tpTransparentBtns in FToolPanel.Options;
  cbShowHint.Checked:=FToolPanel.ShowHint;

  ListBtnAvaliable.ItemHeight:=FToolPanel.BtnHeight + 4;
  ListBtnVisible.ItemHeight:=FToolPanel.BtnHeight + 4;

  FillItems(ListBtnVisible.Items, true);
  FillItems(ListBtnAvaliable.Items, false);

  RadioGroup1.ItemIndex:=Ord(FToolPanel.ButtonAllign);
  RadioGroup2.ItemIndex:=Ord(FToolPanel.ToolBarStyle);

  UpdateStates;

  cbFlatBtn.OnChange:=@CheckBox1Change;
  cbTransp.OnChange:=@CheckBox1Change;
  cbShowHint.OnChange:=@CheckBox1Change;
  RadioGroup1.OnClick:=@CheckBox1Change;
  RadioGroup2.OnClick:=@CheckBox1Change;

end;

end.

