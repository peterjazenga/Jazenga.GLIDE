{ rxfilterby unit

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

unit rxfilterby;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, rxdbgrid, LResources, Forms, Controls, Graphics,
  Dialogs, StdCtrls, db;

type

  { TrxFilterByForm }

  TrxFilterByForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    ComboBox1: TComboBox;
    ComboBox10: TComboBox;
    ComboBox11: TComboBox;
    ComboBox12: TComboBox;
    ComboBox13: TComboBox;
    ComboBox14: TComboBox;
    ComboBox15: TComboBox;
    ComboBox16: TComboBox;
    ComboBox17: TComboBox;
    ComboBox18: TComboBox;
    ComboBox19: TComboBox;
    ComboBox2: TComboBox;
    ComboBox20: TComboBox;
    ComboBox21: TComboBox;
    ComboBox22: TComboBox;
    ComboBox23: TComboBox;
    ComboBox24: TComboBox;
    ComboBox25: TComboBox;
    ComboBox26: TComboBox;
    ComboBox27: TComboBox;
    ComboBox3: TComboBox;
    ComboBox4: TComboBox;
    ComboBox5: TComboBox;
    ComboBox6: TComboBox;
    ComboBox7: TComboBox;
    ComboBox8: TComboBox;
    ComboBox9: TComboBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    Edit9: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure ComboBoxChange(Sender: TObject);
    procedure EditChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    Combo_1 : Array[1..9] of TComboBox;
    Combo_2 : Array[1..9] of TComboBox;
    Edit_1  : Array[1..9] of TEdit;
    Combo_3 : Array[1..9] of TComboBox;

    FGrid : TRxDBGrid;
    procedure ClearALL(AGrid : TRxDBGrid);
    function  FindCombo(CB:TComboBox):Integer;
    function  FindEdit(ED:TEdit):Integer;
  public
    function Execute(AGrid : TRxDBGrid; var FilterStr : String; var LastFilter : TstringList):Boolean;
  end;

var
  rxFilterByForm: TrxFilterByForm;

implementation
uses rxdconst, rxstrutils, DBGrids;

{$R *.lfm}

{ TrxFilterByForm }

procedure TrxFilterByForm.Button2Click(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TrxFilterByForm.Button3Click(Sender: TObject);
begin
  ClearALL(FGrid);
end;

procedure TrxFilterByForm.ComboBoxChange(Sender: TObject);
var
  CBN : Integer;
  CB  : TComboBox;
begin
  CB  := (Sender AS TComboBox);
  CBN := FindCombo(CB);
  if CBN=0 Then Exit;
  if (CB.Text=' IS NULL ') Or (CB.Text=' IS NOT NULL ') Then
  begin
     Edit_1[CBN].Text    := '';
     Edit_1[CBN].Enabled := False;
     Edit_1[CBN].Color   := clInactiveCaption;
  end
  else
  begin
     Edit_1[CBN].Enabled := True;
     Edit_1[CBN].Color   := clWindow;
  end;
end;

procedure TrxFilterByForm.EditChange(Sender: TObject);
var
  EDN : Integer;
  ED  : TEdit;
begin
  ED  := (Sender AS TEdit);
  EDN := FindEdit(ED);
  if EDN=0 Then Exit;
  if ED.Text='' Then Combo_1[EDN].ItemIndex:=-1;
end;

procedure TrxFilterByForm.FormCreate(Sender: TObject);
begin
  Label1.Caption:=sRxFilterFormSelectExp;
  Label2.Caption:=sRxFilterFormOnField;
  Label3.Caption:=sRxFilterFormOperaion;
  Label4.Caption:=sRxFilterFormCondition;
  Label5.Caption:=sRxFilterFormOperand;
  Label6.Caption:=sRxFilterFormEnd;
  Button3.Caption:=sRxFilterFormClear;
  Button2.Caption:=sRxFilterFormCancel;
  Button1.Caption:=sRxFilterFormApply;
end;

procedure TrxFilterByForm.Button1Click(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TrxFilterByForm.ClearALL(AGrid: TRxDBGrid);
var
  i : Integer;
begin
  //*****************************************************************************
  Combo_1[1].Items.Clear;
  Combo_1[1].Items.Add('');
  for i := 0 To AGrid.Columns.Count-1 do
  begin
    if Assigned(AGrid.Columns[i].Field) and (AGrid.Columns[i].Field.FieldKind=fkData) and (AGrid.Columns[i].Visible) then
      Combo_1[1].Items.Objects[Combo_1[1].Items.Add(AGrid.Columns[i].Title.Caption)]:=AGrid.Columns[i].Field;
  end;

  Combo_1[1].ItemIndex := 0;
  for i := 2 To 9 do
  begin
    Combo_1[i].Items.Assign(Combo_1[1].Items);
    Combo_1[i].ItemIndex := 0;
  end;

  Combo_2[1].Items.Clear;
  Combo_2[1].Items.Add(' = ');
  Combo_2[1].Items.Add(' > ');
  Combo_2[1].Items.Add(' < ');
  Combo_2[1].Items.Add(' >= ');
  Combo_2[1].Items.Add(' <= ');
  Combo_2[1].Items.Add(' <> ');
  Combo_2[1].Items.Add(' LIKE ');
  Combo_2[1].Items.Add(' IS NULL ');
  Combo_2[1].Items.Add(' IS NOT NULL ');
  Combo_2[1].ItemIndex := 0;
  for i := 2 To 9 do
  begin
    Combo_2[i].Items.Assign(Combo_2[1].Items);
    Combo_2[i].ItemIndex := 0;
  end;
  for i := 1 To 9 do
  begin
    Combo_3[i].ItemIndex := 0;
  end;
  for i := 1 To 9 do Edit_1[i].Text := '';
 //*****************************************************************************
end;

function TrxFilterByForm.Execute(AGrid: TRxDBGrid; var FilterStr: String;
  var LastFilter: TstringList): Boolean;
var
  X  : Integer;
  P  : Integer;
  S, S1 : String;
  SD : String;
  C : TColumn;
begin
  Result := False;
  //*****************************************************************************
  Combo_1[1]:= ComboBox1;
  Combo_1[2]:= ComboBox4;
  Combo_1[3]:= ComboBox7;
  Combo_1[4]:= ComboBox10;
  Combo_1[5]:= ComboBox13;
  Combo_1[6]:= ComboBox16;
  Combo_1[7]:= ComboBox19;
  Combo_1[8]:= ComboBox22;
  Combo_1[9]:= ComboBox25;

  Combo_2[1]:= ComboBox2;
  Combo_2[2]:= ComboBox5;
  Combo_2[3]:= ComboBox8;
  Combo_2[4]:= ComboBox11;
  Combo_2[5]:= ComboBox14;
  Combo_2[6]:= ComboBox17;
  Combo_2[7]:= ComboBox20;
  Combo_2[8]:= ComboBox23;
  Combo_2[9]:= ComboBox26;

  Combo_3[1]:= ComboBox3;
  Combo_3[2]:= ComboBox6;
  Combo_3[3]:= ComboBox9;
  Combo_3[4]:= ComboBox12;
  Combo_3[5]:= ComboBox15;
  Combo_3[6]:= ComboBox18;
  Combo_3[7]:= ComboBox21;
  Combo_3[8]:= ComboBox24;
  Combo_3[9]:= ComboBox27;
  Combo_3[9].Visible := False;

  Edit_1[1] := Edit1;
  Edit_1[2] := Edit2;
  Edit_1[3] := Edit3;
  Edit_1[4] := Edit4;
  Edit_1[5] := Edit5;
  Edit_1[6] := Edit6;
  Edit_1[7] := Edit7;
  Edit_1[8] := Edit8;
  Edit_1[9] := Edit9;

 //*****************************************************************************
  FGrid := AGrid;
  ClearALL(FGrid);
  if LastFilter.Count > 0 Then
  begin
    for X := 0 To LastFilter.Count-1 do
    begin
      S := LastFilter.Strings[X];
      P := Pos('|||',S);
      if P > 0 Then
      begin
        S1:=System.Copy(S,1,P-1);
        C:=FGrid.ColumnByFieldName(S1);
        Combo_1[X+1].ItemIndex := Combo_1[X+1].Items.IndexOf(C.Title.Caption);
        System.Delete(S,1,P+2);
      end;

      P := Pos('|||',S);
      if P > 0 Then
      begin
        SD:=System.Copy(S,1,P-1);
        Combo_2[X+1].ItemIndex :=  Combo_2[X+1].Items.IndexOf(System.Copy(S,1,P-1));
        System.Delete(S,1,P+2);
        if (SD=' IS NULL ') or (SD=' IS NOT NULL ') Then
        Begin
          Edit_1[X+1].Text:= '';
          Edit_1[X+1].Enabled := False;
          Edit_1[X+1].Color   := clInactiveCaption;
        End;
      end;

      P := Pos('|||',S);
      if P > 0 then
      begin
        Edit_1[X+1].Text := System.Copy(S,1,P-1);
        System.Delete(S,1,P+2);
      end;
      Combo_3[X+1].ItemIndex := Combo_3[X+1].Items.IndexOf(S);

      if Combo_3[X+1].ItemIndex = -1 Then Combo_3[X+1].ItemIndex := 0;
    end;
  end;

  if ShowModal = mrOK Then
  begin
    Result    := True;
    FilterStr := '';
    LastFilter.Clear;
    for X := 1 to 9 Do
    begin
      if  (Combo_1[X].Text <> '') and (Combo_2[X].Text <> '') then
      begin
        if (Edit_1[X].Enabled=False) or (Edit_1[X].Text <> '') Then
        begin
          if X>1 Then
            FilterStr := FilterStr+Combo_3[X-1].Text+' ';

          C:=FGrid.ColumnByCaption(Combo_1[X].Text);
          if Pos('NULL', Combo_2[X].Text) > 0 then
            FilterStr := FilterStr+'('+C.FieldName+Combo_2[X].Text+') '
          else
          case C.Field.DataType of
            ftDateTime   ,
            ftDate       : FilterStr := FilterStr+'('+C.FieldName+Combo_2[X].Text+Char(39)+Copy(Edit_1[X].Text,7,4)+Copy(Edit_1[X].Text,3,4)+Copy(Edit_1[X].Text,1,2)+Copy(Edit_1[X].Text,11,9)+Char(39)+') ';
            ftUnknown    : FilterStr := FilterStr+'('+C.FieldName+Combo_2[X].Text+Edit_1[X].Text+') ';
            ftTime,
            ftString,
            ftMemo       : FilterStr := FilterStr+'('+C.FieldName+Combo_2[X].Text+QuotedString(Edit_1[X].Text, '''')+') ';
          else
            FilterStr := FilterStr+'('+C.FieldName+Combo_2[X].Text+Edit_1[X].Text+') ';
          end;
          LastFilter.Add(C.FieldName+'|||'+Combo_2[X].Text+'|||'+Edit_1[X].Text+'|||'+Combo_3[X].Text);
        end;
      end;
    end;
  end;
end;

Function  TrxFilterByForm.FindCombo(CB:TComboBox):Integer;
var
  X : Integer;
begin
  Result :=0;
  for X := 1 to 9 do
  begin
    if Combo_2[X]=CB Then
    begin
      Result := X;
      Exit;
    end;
  end;
end;

function  TrxFilterByForm.FindEdit(ED:TEdit):Integer;
var
 X : Integer;
begin
  Result :=0;
  for X := 1 to 9 do
  begin
    if Edit_1[X]=ED then
    begin
      Result := X;
      Exit;
    end;
  end;
end;

end.

