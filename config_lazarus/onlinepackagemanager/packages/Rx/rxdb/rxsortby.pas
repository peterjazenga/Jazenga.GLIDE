{ RXDBGrid unit

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

unit rxsortby;

{$I rx.inc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, ButtonPanel, rxdbgrid, db, types;

type

  { TrxSortByForm }

  TrxSortByForm = class(TForm)
    AddBtn: TBitBtn;
    ButtonPanel1: TButtonPanel;
    CheckBox1: TCheckBox;
    DownBtn: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    ListBox1: TListBox;
    ListBox2: TListBox;
    RemoveBtn: TBitBtn;
    UpBtn: TBitBtn;
    procedure AddBtnClick(Sender: TObject);
    procedure DownBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
    procedure ListBox1DrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure ListBox2DblClick(Sender: TObject);
    procedure RemoveBtnClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure UpBtnClick(Sender: TObject);
  private
    FDBGrid:TRxDBGrid;
  public
    { public declarations }
    function Execute(ADBGrid:TRxDBGrid; SortNames:TStringList):Boolean;
  end;

var
  rxSortByForm: TrxSortByForm;

implementation
uses rxdconst, DBGrids;

{$R *.lfm}

{ TrxSortByForm }

procedure TrxSortByForm.DownBtnClick(Sender: TObject);
var
  TmpField:String;
  C1:TObject;
  Poz: Integer;
begin
  if ListBox1.ItemIndex < ListBox1.Items.Count-1 Then
  begin
    Poz:=ListBox1.ItemIndex;

    TmpField:=ListBox1.Items[Poz+1];
    C1:=ListBox1.Items.Objects[Poz+1];

    ListBox1.Items[Poz+1]:=ListBox1.Items[Poz];
    ListBox1.Items.Objects[Poz+1]:=ListBox1.Items.Objects[Poz];

    ListBox1.Items[Poz]:=TmpField;
    ListBox1.Items.Objects[Poz]:=C1;
    ListBox1.ItemIndex:=Poz+1;
  end;
end;

procedure TrxSortByForm.FormCreate(Sender: TObject);
begin
{  ComboBox1.Clear;
  ComboBox1.Items.Add(sRxAscendign);
  ComboBox1.Items.Add(sRxDescending);}
  Caption:=sRxSortByFormCaption;
  Label2.Caption:=sRxSortByFormAllFields;
  Label1.Caption:=sRxSortByFormSortFields;
//  Label4.Caption:=sRxSortByFormSortOrder;
  AddBtn.Caption:=sRxSortByFormAddField;
  RemoveBtn.Caption:=sRxSortByFormRemoveField;
  UpBtn.Caption:=sRxSortByFormMoveUpField;
  DownBtn.Caption:=sRxSortByFormMoveDnField;
  CheckBox1.Caption:=sRxSortByFormCaseInsens;
end;

procedure TrxSortByForm.ListBox1DblClick(Sender: TObject);
begin
  RemoveBtn.Click;
end;

procedure TrxSortByForm.ListBox1DrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  X, Y:integer;
  //P:TRxColumn;
  S1, S2:string;
  Cnv:TCanvas;
begin
  Cnv:=ListBox1.Canvas;
  Cnv.FillRect(ARect);       { clear the rectangle }
  //P:=TRxColumn(ListBox1.Items.Objects[Index]);
  S1:=ListBox1.Items[Index];
  S2:=Copy(S1, 1, 1);
  Delete(S1, 1, 1);

  X := aRect.Left + 2;
  Y := Trunc((aRect.Top + aRect.Bottom - UpBtn.Glyph.Height) / 2);

  if S2 = '1' then
    Cnv.Draw(X, Y, UpBtn.Glyph)
  else
    Cnv.Draw(X, Y, DownBtn.Glyph);

  Cnv.TextOut(ARect.Left + UpBtn.Glyph.Width + 6, (ARect.Top + ARect.Bottom  - Cnv.TextHeight('Wg')) div 2, S1);
end;

procedure TrxSortByForm.ListBox2DblClick(Sender: TObject);
begin
  AddBtn.Click;
end;


procedure TrxSortByForm.AddBtnClick(Sender: TObject);
var
  S:string;
begin
  if ListBox2.ItemIndex <> -1 Then
  begin
    S:='1'+ListBox2.Items[ListBox2.ItemIndex];
    ListBox1.Items.Objects[ListBox1.Items.Add(S)]:=ListBox2.Items.Objects[ListBox2.ItemIndex];
    ListBox2.Items.Delete(ListBox2.ItemIndex);
    ListBox1.ItemIndex:=ListBox1.Items.Count-1;
  end;
end;

procedure TrxSortByForm.RemoveBtnClick(Sender: TObject);
var
  S:string;
begin
  if ListBox1.ItemIndex <> -1 Then
  begin
    S:=TRxColumn(ListBox1.Items.Objects[ListBox1.ItemIndex]).Title.Caption;
    ListBox2.Items.Objects[ListBox2.Items.Add(S)]:=ListBox1.Items.Objects[ListBox1.ItemIndex];
    ListBox1.Items.Delete(ListBox1.ItemIndex);
  end;
end;

procedure TrxSortByForm.SpeedButton1Click(Sender: TObject);
var
  S:string;
begin
  if (ListBox1.ItemIndex <> -1) then
  begin
    S:=ListBox1.Items[ListBox1.ItemIndex];
    if S[1] = '1' then
      S[1] := '0'
    else
      S[1] := '1';
    ListBox1.Items[ListBox1.ItemIndex]:=S;
  end;
end;

procedure TrxSortByForm.UpBtnClick(Sender: TObject);
var
  TmpField:String;
  Poz     : Integer;
  C1:TObject;
begin
  if ListBox1.ItemIndex > 0 Then
  begin
    Poz:=ListBox1.ItemIndex;
    TmpField:=ListBox1.Items[Poz-1];
    C1:=ListBox1.Items.Objects[Poz-1];

    ListBox1.Items[Poz-1]:=ListBox1.Items[Poz];
    ListBox1.Items.Objects[Poz-1]:=ListBox1.Items.Objects[Poz];

    ListBox1.Items[Poz]:=TmpField;
    ListBox1.Items.Objects[Poz]:=C1;

    ListBox1.ItemIndex:=Poz-1;
  end;
end;


function TrxSortByForm.Execute(ADBGrid: TRxDBGrid; SortNames: TStringList
  ): Boolean;
var
  i : Integer;
  S             : String;
  C:TRxColumn;
begin
  Result:=False;
  if not (Assigned(ADBGrid.DataSource) and Assigned(ADBGrid.DataSource.DataSet) and ADBGrid.DataSource.DataSet.Active) then  exit;

  FDBGrid:=ADBGrid;

  ListBox1.Clear;
  ListBox2.Clear;


  for i:=0 to ADBGrid.Columns.Count-1 do
  begin
    C:=TRxColumn(ADBGrid.Columns[i]);
    if C.SortOrder = smNone then
      ListBox2.Items.Objects[ListBox2.Items.Add(C.Title.Caption)]:=C;
  end;

  for i:=0 to ADBGrid.SortColumns.Count-1 do
  begin
    C:=ADBGrid.SortColumns[i];

    if C.SortOrder = smUp then
      S:='1'+C.Title.Caption
    else
      S:='0'+C.Title.Caption;

    ListBox1.Items.Objects[ListBox1.Items.Add(S)]:=C
  end;

  if ShowModal = mrOK Then
  begin
    SortNames.Clear;
    for i:=0 to ListBox1.Items.Count-1 do
    begin
      C:=ListBox1.Items.Objects[i] as TRxColumn;
      SortNames.Add(Copy(ListBox1.Items[i], 1, 1) + C.FieldName);
    end;

    Result:=True;
  end;
end;

end.


