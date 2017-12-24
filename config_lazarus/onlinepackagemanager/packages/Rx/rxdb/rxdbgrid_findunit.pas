{ rxdbgrid_findunit unit

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

unit rxdbgrid_findunit;

{$I rx.inc}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, ExtCtrls, rxdbgrid, DB;

type

  { TrxDBGridFindForm }

  TrxDBGridFindForm = class(TForm)
    BtnFind: TButton;
    Button2: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    ComboBox1: TComboBox;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    RadioGroup1: TRadioGroup;
    procedure BtnFindClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FGrid:TRxDBGrid;
    FDataSet:TDataSet;
    procedure SetGrid(AGrid:TRxDBGrid);
  public
    { public declarations }
  end; 

procedure ShowRxDBGridFindForm(Grid:TRxDBGrid);

implementation
uses rxdbutils, DBGrids, rxdconst, LCLStrConsts;

{$R *.lfm}

procedure ShowRxDBGridFindForm(Grid: TRxDBGrid);
var
  rxDBGridFindForm: TrxDBGridFindForm;
begin
  rxDBGridFindForm:=TrxDBGridFindForm.Create(Application);
  rxDBGridFindForm.SetGrid(Grid);
  rxDBGridFindForm.ShowModal;
  rxDBGridFindForm.Free;
end;

{ TrxDBGridFindForm }

procedure TrxDBGridFindForm.Button2Click(Sender: TObject);
begin
  Close;
end;

procedure TrxDBGridFindForm.FormCreate(Sender: TObject);
begin
  Caption:=sRxDbGridFindCaption;
  Label1.Caption:=sRxDbGridFindText;
  Label2.Caption:=sRxDbGridFindOnField;
  CheckBox1.Caption:=sRxDbGridFindCaseSens;
  CheckBox2.Caption:=sRxDbGridFindPartial;
  RadioGroup1.Caption:=sRxDbGridFindDirecion;
  RadioGroup1.Items.Clear;
  RadioGroup1.Items.Add(sRxDbGridFindRangeAll);
  RadioGroup1.Items.Add(sRxDbGridFindRangeForw);
  RadioGroup1.Items.Add(sRxDbGridFindRangeBack);
  BtnFind.Caption:=sRxFindMore;
  Button2.Caption:=rsMbClose;

  RadioGroup1.ItemIndex:=0;
end;

procedure TrxDBGridFindForm.FormShow(Sender: TObject);
begin
  Edit1.SetFocus;
end;

procedure TrxDBGridFindForm.BtnFindClick(Sender: TObject);
var
  FieldName:string;
  LOptions: TLocateOptions;
  SearchOrigin:TRxSearchDirection;
  P:TBookMark;
  R:boolean;
begin
  { TODO -oalexs : Необходимо переделать поиск по колонке - искать всегда по строковому представлению. Иначе не ищет по дате-времени }
  if Edit1.Text<>'' then
  begin
    try

      FieldName:=FGrid.Columns[ComboBox1.ItemIndex].FieldName;
      LOptions:=[];
      if not CheckBox1.Checked then
        LOptions:=LOptions+[loCaseInsensitive];

      if CheckBox2.Checked then
        LOptions:=LOptions+[loPartialKey];

      SearchOrigin:=TRxSearchDirection(RadioGroup1.ItemIndex);
      {$IFDEF NoAutomatedBookmark}
      P:=FDataSet.GetBookmark;
      {$ELSE}
      P:=FDataSet.Bookmark;
      {$ENDIF}
      if SearchOrigin = rsdForward then
        FDataSet.Next
      else
      if SearchOrigin = rsdBackward then
        FDataSet.Prior;
      R:=DataSetLocateThrough(FDataSet, FieldName, Edit1.Text, LOptions, SearchOrigin);
    finally
      {$IFDEF NoAutomatedBookmark}
      if not R then
        FDataSet.GotoBookmark(P);
      FDataSet.FreeBookmark(P);
      {$ELSE}
      if not R then
        FDataSet.Bookmark:=P;
      {$ENDIF}
    end;
  end;
end;

type
  THckGrid = class(TCustomDBGrid)
  end;

procedure TrxDBGridFindForm.SetGrid(AGrid: TRxDBGrid);
var
  i:integer;
begin
  if AGrid=FGrid then exit;
  FGrid:=AGrid;
  ComboBox1.Items.Clear;
  if Assigned(AGrid) then
  begin
    for i:=0 to AGrid.Columns.Count-1 do
    begin
      if not (coDisableDialogFind in AGrid.Columns[i].Options) then
        ComboBox1.Items.Add(AGrid.Columns[i].Title.Caption);
    end;

    ComboBox1.ItemIndex:=ComboBox1.Items.IndexOf(AGrid.SelectedColumn.Title.Caption);
  end;

  FDataSet:=nil;
  if Assigned(FGrid) and Assigned(THckGrid(FGrid).DataSource) then
    FDataSet:=THckGrid(FGrid).DataSource.DataSet;
  BtnFind.Enabled:=Assigned(FDataSet) and FDataSet.Active
end;

end.

