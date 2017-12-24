{ fduallst unit

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

unit rxfduallst;

{$I RX.INC}

interface

uses SysUtils, LCLIntf, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, LResources, LCLType, ButtonPanel;

type

  { TDualListForm }

  TDualListForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    SrcList: TListBox;
    DstList: TListBox;
    SrcLabel: TLabel;
    DstLabel: TLabel;
    IncBtn: TButton;
    IncAllBtn: TButton;
    ExclBtn: TButton;
    ExclAllBtn: TButton;
    procedure IncBtnClick(Sender: TObject);
    procedure IncAllBtnClick(Sender: TObject);
    procedure ExclBtnClick(Sender: TObject);
    procedure ExclAllBtnClick(Sender: TObject);
    procedure SrcListDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure DstListDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure SrcListDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure DstListDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure SrcListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure DstListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure HelpBtnClick(Sender: TObject);
    procedure ListClick(Sender: TObject);
  private
    { Private declarations }
    function GetShowHelp: Boolean;
    procedure SetShowHelp(AValue: Boolean);
  public
    { Public declarations }
    procedure SetButtons;
    property ShowHelp: Boolean read GetShowHelp write SetShowHelp
      default True;
end;

implementation

uses rxvclutils, rxboxprocs;

{$R *.lfm}

{ TDualListForm }

procedure TDualListForm.SetButtons;
var
  SrcEmpty, DstEmpty: Boolean;
begin
  SrcEmpty := (SrcList.Items.Count = 0);
  DstEmpty := (DstList.Items.Count = 0);
  IncBtn.Enabled := not SrcEmpty and (SrcList.SelCount > 0);
  IncAllBtn.Enabled := not SrcEmpty;
  ExclBtn.Enabled := not DstEmpty and (DstList.SelCount > 0);
  ExclAllBtn.Enabled := not DstEmpty;
end;

function TDualListForm.GetShowHelp: Boolean;
begin
  Result := pbHelp in ButtonPanel1.ShowButtons;
end;

procedure TDualListForm.SetShowHelp(AValue: Boolean);
begin
  if AValue then
    ButtonPanel1.ShowButtons:=ButtonPanel1.ShowButtons + [pbHelp]
  else
    ButtonPanel1.ShowButtons:=ButtonPanel1.ShowButtons - [pbHelp];
end;

procedure TDualListForm.IncBtnClick(Sender: TObject);
begin
  BoxMoveSelectedItems(SrcList, DstList);
  SetButtons;
end;

procedure TDualListForm.IncAllBtnClick(Sender: TObject);
begin
  BoxMoveAllItems(SrcList, DstList);
  SetButtons;
end;

procedure TDualListForm.ExclBtnClick(Sender: TObject);
begin
  BoxMoveSelectedItems(DstList, SrcList);
  SetButtons;
end;

procedure TDualListForm.ExclAllBtnClick(Sender: TObject);
begin
  BoxMoveAllItems(DstList, SrcList);
  SetButtons;
end;

procedure TDualListForm.SrcListDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  BoxDragOver(SrcList, Source, X, Y, State, Accept, SrcList.Sorted);
  if State = dsDragLeave then
    (Source as TListBox).DragCursor := crDrag;
  if (State = dsDragEnter) and ((Source as TListBox).SelCount > 1) then
    (Source as TListBox).DragCursor := crMultiDrag;
end;

procedure TDualListForm.DstListDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  BoxDragOver(DstList, Source, X, Y, State, Accept, DstList.Sorted);
  if State = dsDragLeave then
    (Source as TListBox).DragCursor := crDrag;
  if (State = dsDragEnter) and ((Source as TListBox).SelCount > 1) then
    (Source as TListBox).DragCursor := crMultiDrag;
end;

procedure TDualListForm.SrcListDragDrop(Sender, Source: TObject; X,
  Y: Integer);
begin
  if Source = DstList then ExclBtnClick(SrcList)
  else if Source = SrcList then begin
    BoxMoveFocusedItem(SrcList, SrcList.ItemAtPos(Point(X, Y), True));
  end;
end;

procedure TDualListForm.DstListDragDrop(Sender, Source: TObject; X,
  Y: Integer);
begin
  if Source = SrcList then IncBtnClick(DstList)
  else if Source = DstList then begin
    BoxMoveFocusedItem(DstList, DstList.ItemAtPos(Point(X, Y), True));
  end;
end;

procedure TDualListForm.SrcListKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Incr: Integer;
begin
  if not SrcList.Sorted then begin
    if (ssCtrl in Shift) and ((Key = VK_DOWN) or (Key = VK_UP)) then begin
      if Key = VK_DOWN then Incr := 1
      else Incr := -1;
      BoxMoveFocusedItem(SrcList, SrcList.ItemIndex + Incr);
      Key := 0;
    end;
  end;
end;

procedure TDualListForm.DstListKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Incr: Integer;
begin
  if not DstList.Sorted then begin
    if (ssCtrl in Shift) and ((Key = VK_DOWN) or (Key = VK_UP)) then begin
      if Key = VK_DOWN then Incr := 1
      else Incr := -1;
      BoxMoveFocusedItem(DstList, DstList.ItemIndex + Incr);
      Key := 0;
    end;
  end;
end;

procedure TDualListForm.HelpBtnClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TDualListForm.ListClick(Sender: TObject);
begin
  SetButtons;
end;

end.
