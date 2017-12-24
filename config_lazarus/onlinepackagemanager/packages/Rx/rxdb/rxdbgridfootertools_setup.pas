{ RxDBGridPrintGrid unit

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

unit rxdbgridfootertools_setup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  ButtonPanel, ComCtrls, StdCtrls, ColorBox, rxdbgrid, rxdconst;

type

  { TRxDBGridFooterTools_SetupForm }

  TRxDBGridFooterTools_SetupForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    ColorBox1: TColorBox;
    Label1: TLabel;
    PageControl1: TPageControl;
    StringGrid1: TStringGrid;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure FormCreate(Sender: TObject);
  private
    FRxDBGrid:TRxDBGrid;
  public
    procedure InitData(ARxDBGrid:TRxDBGrid);
    procedure SetData;
  end;

var
  RxDBGridFooterTools_SetupForm: TRxDBGridFooterTools_SetupForm;

const
  rxFooterFunctionNames : array [TFooterValueType] of string =
    (sfvtNon, sfvtSum, sfvtAvg, sfvtCount, sfvtFieldValue, sfvtStaticText, sfvtMax, sfvtMin, sfvtRecNo);
implementation
uses rxdbutils;

{$R *.lfm}

{ TRxDBGridFooterTools_SetupForm }

procedure TRxDBGridFooterTools_SetupForm.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePageIndex:=0;
  Caption:=sSetupTotalRow;
  TabSheet1.Caption:=sFunction;
  TabSheet2.Caption:=sOtherOptions;
  Label1.Caption:=sFooterRowColor;
  StringGrid1.Columns[0].Title.Caption:=sCollumnName;
  StringGrid1.Columns[1].Title.Caption:=sFunction;
end;

procedure TRxDBGridFooterTools_SetupForm.InitData(ARxDBGrid: TRxDBGrid);
var
  i: Integer;
  c: TFooterValueType;
begin
  FRxDBGrid:=ARxDBGrid;
  if not Assigned(FRxDBGrid) then exit;
  StringGrid1.TitleStyle:=FRxDBGrid.TitleStyle;

  StringGrid1.Columns[1].PickList.Clear;
  for c:=Low(TFooterValueType) to High(TFooterValueType) do
    StringGrid1.Columns[1].PickList.Add(rxFooterFunctionNames[c]);


  StringGrid1.RowCount:=FRxDBGrid.Columns.Count+1;

  for i:=0 to FRxDBGrid.Columns.Count-1 do
  begin
    StringGrid1.Cells[0, i+1]:=FRxDBGrid.Columns[i].Title.Caption;
    if FRxDBGrid.Columns[i].Footer.ValueType <> fvtNon then
      StringGrid1.Cells[1, i+1]:=rxFooterFunctionNames[FRxDBGrid.Columns[i].Footer.ValueType];
  end;
  ColorBox1.Selected:=FRxDBGrid.FooterOptions.Color;
end;

procedure TRxDBGridFooterTools_SetupForm.SetData;
var
  B,C: TFooterValueType;
  Col: TRxColumn;
  i: Integer;
begin
  for i:=1 to StringGrid1.RowCount-1 do
  begin
    Col:=FRxDBGrid.ColumnByCaption(StringGrid1.Cells[0, i]);
    B:=fvtNon;
    for c:=Low(TFooterValueType) to High(TFooterValueType) do
      if StringGrid1.Cells[1, i] = rxFooterFunctionNames[c] then
      begin
        B:=C;
        break;
      end;

    if B<>fvtNon then
    begin
      if not (Col.Field.DataType in NumericDataTypes) then
        if not (B in [fvtCount, fvtFieldValue, fvtStaticText, fvtRecNo]) then
          B:=fvtNon;

      if B<>fvtNon then
      begin
        Col.Footer.FieldName:=Col.FieldName;
        Col.Footer.Alignment:=Col.Alignment;
        Col.Footer.DisplayFormat:=Col.DisplayFormat;
      end;
    end;

    Col.Footer.ValueType:=B;
  end;
  FRxDBGrid.FooterOptions.Color:=ColorBox1.Selected;
end;

end.

