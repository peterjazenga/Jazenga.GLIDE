{ rxdbgrid_columsunit unit

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

unit rxdbgrid_columsunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, CheckLst,
  StdCtrls, Buttons, ButtonPanel, Grids, rxdbgrid;

type

  { TrxDBGridColumsForm }

  TrxDBGridColumsForm = class(TForm)
    btnApply: TBitBtn;
    ButtonPanel1: TButtonPanel;
    sbUp: TSpeedButton;
    sbDown: TSpeedButton;
    StringGrid1: TStringGrid;
    procedure btnApplyClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure sbUpClick(Sender: TObject);
    procedure sbDownClick(Sender: TObject);
    procedure StringGrid1Click(Sender: TObject);
    procedure StringGrid1ValidateEntry(sender: TObject; aCol, aRow: Integer;
      const OldValue: string; var NewValue: String);
  private
    FGrid:TRxDBGrid;
    procedure SetGrid(AGrid:TRxDBGrid);
    procedure SetGridColumnsParams;
  public
    { public declarations }
  end; 


procedure ShowRxDBGridColumsForm(Grid:TRxDBGrid);
implementation
uses rxdconst, math;

{$R *.lfm}

procedure ShowRxDBGridColumsForm(Grid: TRxDBGrid);
var
  rxDBGridColumsForm: TrxDBGridColumsForm;
begin
  rxDBGridColumsForm:=TrxDBGridColumsForm.Create(Application);
  rxDBGridColumsForm.SetGrid(Grid);
  if rxDBGridColumsForm.ShowModal = mrOk then
  begin
    if Assigned(Grid) then
      rxDBGridColumsForm.SetGridColumnsParams;
  end;
  rxDBGridColumsForm.Free;
end;

{ TrxDBGridColumsForm }

procedure TrxDBGridColumsForm.FormCreate(Sender: TObject);
begin
  sbUp.AnchorSideLeft.Control:=ButtonPanel1.HelpButton;
  sbUp.AnchorSideTop.Control:=ButtonPanel1.HelpButton;
  sbUp.AnchorSideBottom.Control:=ButtonPanel1.HelpButton;

  btnApply.AnchorSideTop.Control:=ButtonPanel1.OKButton;
  btnApply.AnchorSideBottom.Control:=ButtonPanel1.OKButton;

  {$IFDEF UNIX}
  btnApply.AnchorSideRight.Control:=ButtonPanel1.CancelButton;
  {$ELSE}
  btnApply.AnchorSideRight.Control:=ButtonPanel1.OKButton;
  {$ENDIF}

  Caption:=sRxDbGridSelColCaption;
  sbUp.Hint:=sRxDbGridSelColHint1;
  sbDown.Hint:=sRxDbGridSelColHint2;

  btnApply.Caption:=sRxDbGridSelApplyCaption;
  btnApply.Hint:=sRxDbGridSelApplyHint;
end;

procedure TrxDBGridColumsForm.btnApplyClick(Sender: TObject);
begin
  SetGridColumnsParams;
end;

procedure TrxDBGridColumsForm.sbUpClick(Sender: TObject);
var
  S, W, C:string;
begin
  if (StringGrid1.RowCount > 1) and (StringGrid1.Row > 1) then
  begin
    C:=StringGrid1.Cells[0, StringGrid1.Row-1];
    S:=StringGrid1.Cells[1, StringGrid1.Row-1];
    W:=StringGrid1.Cells[2, StringGrid1.Row-1];

    StringGrid1.Cells[0, StringGrid1.Row-1]:=StringGrid1.Cells[0, StringGrid1.Row];
    StringGrid1.Cells[1, StringGrid1.Row-1]:=StringGrid1.Cells[1, StringGrid1.Row];
    StringGrid1.Cells[2, StringGrid1.Row-1]:=StringGrid1.Cells[2, StringGrid1.Row];

    StringGrid1.Cells[0, StringGrid1.Row]:=C;
    StringGrid1.Cells[1, StringGrid1.Row]:=S;
    StringGrid1.Cells[2, StringGrid1.Row]:=W;

    StringGrid1.Row:=StringGrid1.Row-1;
  end;
end;

procedure TrxDBGridColumsForm.sbDownClick(Sender: TObject);
var
  S, W, C:string;
  i:integer;
begin
  if (StringGrid1.RowCount > 1) and (StringGrid1.Row < StringGrid1.RowCount - 1) then
  begin
    C:=StringGrid1.Cells[0, StringGrid1.Row+1];
    S:=StringGrid1.Cells[1, StringGrid1.Row+1];
    W:=StringGrid1.Cells[2, StringGrid1.Row+1];

    StringGrid1.Cells[0, StringGrid1.Row+1]:=StringGrid1.Cells[0, StringGrid1.Row];
    StringGrid1.Cells[1, StringGrid1.Row+1]:=StringGrid1.Cells[1, StringGrid1.Row];
    StringGrid1.Cells[2, StringGrid1.Row+1]:=StringGrid1.Cells[2, StringGrid1.Row];

    StringGrid1.Cells[0, StringGrid1.Row]:=C;
    StringGrid1.Cells[1, StringGrid1.Row]:=S;
    StringGrid1.Cells[2, StringGrid1.Row]:=W;

    StringGrid1.Row:=StringGrid1.Row+1;
  end;
end;

procedure TrxDBGridColumsForm.StringGrid1Click(Sender: TObject);
var
  i:integer;
  C:TRxColumn;
begin
  i:=StringGrid1.Row;

  C:=FGrid.ColumnByCaption(StringGrid1.Cells[1, i]);
  if coCustomizeVisible in C.Options then
    StringGrid1.Options:=StringGrid1.Options + [goEditing]
  else
    StringGrid1.Options:=StringGrid1.Options - [goEditing]
  ;
end;

procedure TrxDBGridColumsForm.StringGrid1ValidateEntry(sender: TObject; aCol,
  aRow: Integer; const OldValue: string; var NewValue: String);
begin
  if aCol = 2 then
    NewValue:=IntToStr(Max(StrToIntDef(NewValue, StrToIntDef(OldValue, 0)), 0));
end;

procedure TrxDBGridColumsForm.SetGrid(AGrid: TRxDBGrid);
var
  i:integer;
  C:TRxColumn;
begin
  if AGrid=FGrid then exit;
  FGrid:=AGrid;
  if Assigned(AGrid) then
  begin
    StringGrid1.RowCount:=AGrid.Columns.Count + 1;

    for i:=0 to AGrid.Columns.Count-1 do
    begin
      C:=AGrid.Columns[i] as TRxColumn;

      StringGrid1.Cells[0, i + 1]:=BoolToStr(C.Visible, '1', '0');
      StringGrid1.Cells[1, i + 1]:=C.Title.Caption;
      if C.Width = 0 then
        StringGrid1.Cells[2, i + 1]:=IntToStr(AGrid.DefaultColWidth)
      else
        StringGrid1.Cells[2, i + 1]:=IntToStr(C.Width);
    end;
  end
  else
    StringGrid1.RowCount:=1;
end;

procedure TrxDBGridColumsForm.SetGridColumnsParams;
var
  i:integer;
  Col:TRxColumn;
begin
  for i:=1 to StringGrid1.RowCount-1 do
  begin
    Col:=FGrid.ColumnByCaption(StringGrid1.Cells[1, i]);
    if Assigned(Col) then
    begin
      Col.Visible:=StringGrid1.Cells[0, i] = '1';
      Col.Index:=i-1;
      Col.Width:=StrToIntDef(StringGrid1.Cells[2, i], 65);
    end
  end;
end;

end.

