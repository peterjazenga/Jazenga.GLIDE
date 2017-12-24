{ duallist unit

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

unit rxduallist;

interface

{$I rx.inc}

uses Classes, Controls;

type

{ TDualListDialog }

  TDualListDialog = class(TComponent)
  private
    FCtl3D: Boolean;
    FSorted: Boolean;
    FTitle:string;
    FLabel1Caption: TCaption;
    FLabel2Caption: TCaption;
    FOkBtnCaption: TCaption;
    FCancelBtnCaption: TCaption;
    FHelpBtnCaption: TCaption;
    FHelpContext: THelpContext;
    FList1: TStrings;
    FList2: TStrings;
    FShowHelp: Boolean;
    procedure SetList1(Value: TStrings);
    procedure SetList2(Value: TStrings);
    function IsLabel1Custom: Boolean;
    function IsLabel2Custom: Boolean;
    function IsOkBtnCustom: Boolean;
    function IsCancelBtnCustom: Boolean;
    function IsHelpBtnCustom: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean;
  published
    property Ctl3D: Boolean read FCtl3D write FCtl3D default True;
    property Sorted: Boolean read FSorted write FSorted;
    property Title: string read FTitle write FTitle;
    property Label1Caption: TCaption read FLabel1Caption write FLabel1Caption
      stored IsLabel1Custom;
    property Label2Caption: TCaption read FLabel2Caption write FLabel2Caption
      stored IsLabel2Custom;
    property OkBtnCaption: TCaption read FOkBtnCaption write FOkBtnCaption
      stored IsOkBtnCustom;
    property CancelBtnCaption: TCaption read FCancelBtnCaption write FCancelBtnCaption
      stored IsCancelBtnCustom;
    property HelpBtnCaption: TCaption read FHelpBtnCaption write FHelpBtnCaption
      stored IsHelpBtnCustom;
    property HelpContext: THelpContext read FHelpContext write FHelpContext;
    property List1: TStrings read FList1 write SetList1;
    property List2: TStrings read FList2 write SetList2;
    property ShowHelp: Boolean read FShowHelp write FShowHelp default True;
  end;

implementation

uses SysUtils, Forms, rxfduallst, LCLStrConsts, rxconst;

{ TDualListDialog }

constructor TDualListDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCtl3D := True;
  FShowHelp := True;
  FList1 := TStringList.Create;
  FList2 := TStringList.Create;
  FLabel1Caption := SDualListSrcCaption;
  FLabel2Caption := SDualListDestCaption;
  OkBtnCaption := rsmbOK;
  CancelBtnCaption := rsmbCancel;
  HelpBtnCaption := rsmbHelp;
  Title:=SDualListCaption;
end;

destructor TDualListDialog.Destroy;
begin
  List1.Free;
  List2.Free;
  inherited Destroy;
end;

procedure TDualListDialog.SetList1(Value: TStrings);
begin
  FList1.Assign(Value);
end;

procedure TDualListDialog.SetList2(Value: TStrings);
begin
  FList2.Assign(Value);
end;

function TDualListDialog.IsLabel1Custom: Boolean;
begin
  Result := CompareStr(Label1Caption, SDualListSrcCaption) <> 0;
end;

function TDualListDialog.IsLabel2Custom: Boolean;
begin
  Result := CompareStr(Label2Caption, SDualListDestCaption) <> 0;
end;

function TDualListDialog.IsOkBtnCustom: Boolean;
begin
  Result := CompareStr(OkBtnCaption, rsmbOK) <> 0;
end;

function TDualListDialog.IsCancelBtnCustom: Boolean;
begin
  Result := CompareStr(CancelBtnCaption, rsmbCancel) <> 0;
end;

function TDualListDialog.IsHelpBtnCustom: Boolean;
begin
  Result := CompareStr(HelpBtnCaption, rsmbHelp) <> 0;
end;

function TDualListDialog.Execute: Boolean;
var
  Form: TDualListForm;
begin
  Form := TDualListForm.Create(Application);
  try
    with Form do
    begin
      Ctl3D := Self.Ctl3D;
      if NewStyleControls then Font.Style := [];
      ShowHelp := Self.ShowHelp;
      SrcList.Sorted := Sorted;
      DstList.Sorted := Sorted;
      SrcList.Items := List1;
      DstList.Items := List2;
      if Self.Title <> '' then Form.Caption := Self.Title;
      if Label1Caption <> '' then SrcLabel.Caption := Label1Caption;
      if Label2Caption <> '' then DstLabel.Caption := Label2Caption;
      ButtonPanel1.OKButton.Caption := OkBtnCaption;
      ButtonPanel1.CancelButton.Caption := CancelBtnCaption;
      ButtonPanel1.HelpButton.Caption := HelpBtnCaption;

      HelpContext := Self.HelpContext;
      ButtonPanel1.HelpButton.HelpContext := HelpContext;
    end;
    Result := (Form.ShowModal = mrOk);
    if Result then
    begin
      List1 := Form.SrcList.Items;
      List2 := Form.DstList.Items;
    end;
  finally
    Form.Free;
  end;
end;

end.
