{ rxShortCutUnit unit

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

unit rxShortCutUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  StdCtrls;

type

  { TrxShortCutForm }

  TrxShortCutForm = class(TForm)
    Button1: TButton;
    ButtonPanel1: TButtonPanel;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    ComboBox1: TComboBox;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    function GetShortCut: TShortCut;
    procedure SetShortCut(AValue: TShortCut);
  public
    property ShortCut:TShortCut read GetShortCut write SetShortCut;
  end;


function RxSelectShortCut(var AShortCut:TShortCut):boolean;

implementation
uses LCLProc, LCLType, LCLStrConsts;

{$R *.lfm}

function RxSelectShortCut(var AShortCut: TShortCut): boolean;
var
  rxShortCutForm: TrxShortCutForm;
begin
  rxShortCutForm:=TrxShortCutForm.Create(Application);
  rxShortCutForm.ShortCut:=AShortCut;
  if rxShortCutForm.ShowModal = mrOk then
    AShortCut:=rxShortCutForm.ShortCut;
  rxShortCutForm.Free;
end;

type

  { TGrabForm }

  TGrabForm = class(TForm)
  private
    FShortCutEdt:TShortCut;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor CreateNew(AOwner: TComponent; Num: Integer = 0); override;
  end;


{ TGrabForm }

procedure TGrabForm.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if (Key = VK_ESCAPE) and (Shift = []) then
    ModalResult:=mrCancel
  else
  if (Key <> VK_CONTROL) and (Key <> VK_SHIFT) and (Key <> VK_MENU) then
  begin
    FShortCutEdt:=KeyToShortCut(Key, Shift);
    ModalResult:=mrOK;
  end;
end;

constructor TGrabForm.CreateNew(AOwner: TComponent; Num: Integer);
var
  L: TLabel;
begin
  inherited CreateNew(AOwner, Num);
  Position:=poScreenCenter;
  Width:=200;
  Height:=80;
  Caption:='Press the key';
  BorderStyle:=bsDialog;
  KeyPreview:=true;

  L:=TLabel.Create(Self);
  L.Parent:=Self;
  L.Caption:=Caption;
  L.AnchorSide[akTop].Control:=Self;
  L.AnchorSide[akTop].Side:=asrCenter;

  L.AnchorSide[akLeft].Control:=Self;
  L.AnchorSide[akLeft].Side:=asrCenter;
end;

{ TrxShortCutForm }

procedure TrxShortCutForm.FormCreate(Sender: TObject);
var
  S: String;
  i:Word;
begin
  for i:=0 to $FF do
  begin
    S:=ShortCutToText(i);
    if S<>'' then
      ComboBox1.Items.Add(S);
  end;
end;

procedure TrxShortCutForm.Button1Click(Sender: TObject);
var
  F:TGrabForm;
begin
  F:=TGrabForm.CreateNew(Self);
  if F.ShowModal = mrOk then
    SetShortCut(F.FShortCutEdt);
  F.Free;
end;

procedure TrxShortCutForm.SetShortCut(AValue: TShortCut);
begin
  ComboBox1.Text:=ShortCutToText(AValue and $FF);
  CheckBox1.Checked:=AValue and scShift <> 0;
  CheckBox2.Checked:=AValue and scAlt <> 0;
  CheckBox3.Checked:=AValue and scCtrl <> 0;
  ///if ShortCut and scMeta <> 0 then Result := Result + MenuKeyCaps[mkcMeta];
end;

function TrxShortCutForm.GetShortCut: TShortCut;
var
  S: String;
begin
  S:='';

  if CheckBox1.Checked then
    S:=SmkcShift + S;

  if CheckBox2.Checked then
    S:=SmkcAlt + S;

  if CheckBox3.Checked then
    S:=SmkcCtrl + S;
  //SmkcMeta = 'Meta+';

  S:=S + ComboBox1.Text;
  Result:=TextToShortCut(S);
end;

end.

