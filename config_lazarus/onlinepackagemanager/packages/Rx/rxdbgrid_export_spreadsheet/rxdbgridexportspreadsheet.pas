{ RxDBGridExportSpreadSheet unit

  Copyright (C) 2005-2016 Lagunov Aleksey alexs@yandex.ru
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

unit RxDBGridExportSpreadSheet;

{$I rx.inc}

interface

uses
  Classes, SysUtils, rxdbgrid, DB, fpspreadsheet, Graphics, fpsTypes;

type
  TRxDBGridExportSpreadSheetOption = (ressExportTitle,
    ressExportColors,
    ressExportFooter,
    ressExportFormula,
    ressOverwriteExisting,
    ressExportSelectedRows
    );

  TRxDBGridExportSpreadSheetOptions = set of TRxDBGridExportSpreadSheetOption;

type

  { TRxDBGridExportSpreadSheet }

  TRxDBGridExportSpreadSheet = class(TRxDBGridAbstractTools)
  private
    FFileName: string;
    FOpenAfterExport: boolean;
    FOptions: TRxDBGridExportSpreadSheetOptions;
    FPageName: string;
    function ColIndex(ACol:TRxColumn):integer;
    procedure ExpCurRow(AFont: TFont);
    procedure ExpAllRow;
    procedure ExpSelectedRow;
  protected
    FDataSet:TDataSet;
    FWorkbook: TsWorkbook;
    FWorksheet: TsWorksheet;
    FCurRow : integer;
    FFirstDataRow : integer;
    FLastDataRow : integer;
    FCurCol : integer;
    scColorBlack:TsColor;

    procedure DoExportTitle;
    procedure DoExportBody;
    procedure DoExportFooter;
    procedure DoExportColWidth;
    function DoExecTools:boolean;override;
    function DoSetupTools:boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property FileName:string read FFileName write FFileName;
    property PageName:string read FPageName write FPageName;
    property Options:TRxDBGridExportSpreadSheetOptions read FOptions write FOptions;
    property OpenAfterExport:boolean read FOpenAfterExport write FOpenAfterExport default false;
  end;

procedure Register;

implementation
uses fpsallformats, LCLType, Forms, math, LazUTF8, rxdconst, Controls, LCLIntf,
  RxDBGridExportSpreadSheet_ParamsUnit, rxdbutils, fpsutils, DBGrids;

{$R rxdbgridexportspreadsheet.res}

procedure Register;
begin
  RegisterComponents('RX DBAware',[TRxDBGridExportSpreadSheet]);
end;

const
  ssAligns : array [TAlignment] of TsHorAlignment = (haLeft, haRight, haCenter);

{ TRxDBGridExportSpeadSheet }

function TRxDBGridExportSpreadSheet.ColIndex(ACol: TRxColumn): integer;
var
  C: TRxColumn;
  i: Integer;
begin
  Result:=-1;
  if (not Assigned(ACol)) or (not ACol.Visible) then exit;
  for i:=0 to FRxDBGrid.Columns.Count - 1 do
  begin
    C:=FRxDBGrid.Columns[i] as TRxColumn;
    if C.Visible then
      Inc(Result);
    if ACol = C then
      Exit;
  end;
end;

procedure TRxDBGridExportSpreadSheet.ExpCurRow(AFont:TFont);
var
  i, J: Integer;
  C: TRxColumn;
  CT: TRxColumnTitle;
  S: String;
  CC: TColor;
begin
  FCurCol:=0;
  for i:=0 to FRxDBGrid.Columns.Count - 1 do
  begin
    C:=FRxDBGrid.Columns[i] as TRxColumn;
    CT:=C.Title as TRxColumnTitle;
    if C.Visible then
    begin
      if Assigned(C.Field) then
      begin
        S:=C.Field.DisplayText;
        if (C.KeyList.Count > 0) and (C.PickList.Count > 0) then
        begin
          J := C.KeyList.IndexOf(S);
          if (J >= 0) and (J < C.PickList.Count) then
            S := C.PickList[j];
          FWorksheet.WriteUTF8Text(FCurRow, FCurCol, S);
        end
        else
        if Assigned(C.Field.OnGetText) then
          FWorksheet.WriteUTF8Text(FCurRow, FCurCol, S)
        else
        if C.Field.DataType in [ftCurrency] then
          FWorksheet.WriteCurrency(FCurRow, FCurCol, C.Field.AsCurrency, nfCurrency, '')
        else
        if C.Field.DataType in IntegerDataTypes then
          FWorksheet.WriteNumber(FCurRow, FCurCol, C.Field.AsInteger, nfFixed, 0)
        else
        if C.Field.DataType in NumericDataTypes then
          FWorksheet.WriteNumber(FCurRow, FCurCol, C.Field.AsFloat, nfFixed, 2)
        else
          FWorksheet.WriteUTF8Text(FCurRow, FCurCol, S);
      end;

      if ressExportColors in FOptions then
      begin
        CC:=C.Color;
        if Assigned(RxDBGrid.OnGetCellProps) then
          RxDBGrid.OnGetCellProps(RxDBGrid, C.Field, AFont, CC);
        if (CC and SYS_COLOR_BASE) = 0  then
        begin
          {$IFDEF OLD_fpSPREADSHEET}
          scColor:=FWorkbook.AddColorToPalette(CC);
          FWorksheet.WriteBackgroundColor(FCurRow,FCurCol, scColor);
          {$ELSE}
          FWorksheet.WriteBackgroundColor(FCurRow,FCurCol, CC);
          {$ENDIF}
        end;
      end;

      FWorksheet.WriteBorders(FCurRow,FCurCol, [cbNorth, cbWest, cbEast, cbSouth]);
      FWorksheet.WriteBorderColor(FCurRow,FCurCol, cbNorth, scColorBlack);
      FWorksheet.WriteBorderColor(FCurRow,FCurCol, cbWest, scColorBlack);
      FWorksheet.WriteBorderColor(FCurRow,FCurCol, cbEast, scColorBlack);
      FWorksheet.WriteBorderColor(FCurRow,FCurCol, cbSouth, scColorBlack);

      FWorksheet.WriteHorAlignment(FCurRow, FCurCol, ssAligns[C.Alignment]);
      inc(FCurCol);
    end;
  end;
end;

procedure TRxDBGridExportSpreadSheet.ExpAllRow;
var
  F: TFont;
begin
  F:=TFont.Create;
  FDataSet.First;
  while not FDataSet.EOF do
  begin
    ExpCurRow(F);
    inc(FCurRow);
    FDataSet.Next;
  end;
  F.Free;
end;

procedure TRxDBGridExportSpreadSheet.ExpSelectedRow;
var
  F: TFont;
  k: Integer;
begin
  F:=TFont.Create;
  FDataSet.First;
  for k:=0 to FRxDBGrid.SelectedRows.Count-1 do
  begin
    FDataSet.Bookmark:=FRxDBGrid.SelectedRows[k];
    ExpCurRow(F);
    inc(FCurRow);
  end;
  F.Free;
end;

procedure TRxDBGridExportSpreadSheet.DoExportTitle;
var
  i, k  : Integer;
  C  : TRxColumn;
  CT : TRxColumnTitle;
  CC : TColor;
{$IFDEF OLD_fpSPREADSHEET}
  scColor : TsColor;
{$ENDIF}
  CB:TsCellBorders;
  FMaxTitleHeight : integer;
  P: TMLCaptionItem;
begin
  FCurCol:=0;
  FMaxTitleHeight:=1;
  for i:=0 to FRxDBGrid.Columns.Count - 1 do
  begin
    C:=FRxDBGrid.Columns[i] as TRxColumn;
    CT:=C.Title as TRxColumnTitle;
    FMaxTitleHeight:=Max(FMaxTitleHeight, CT.CaptionLinesCount);
    if C.Visible then
    begin
      if CT.CaptionLinesCount > 0 then
      begin
        for k:=0 to CT.CaptionLinesCount - 1 do
        begin
          CC:=C.Title.Color;
          if (CC and SYS_COLOR_BASE) = 0  then
          begin
{$IFDEF OLD_fpSPREADSHEET}
            scColor:=FWorkbook.AddColorToPalette(CC);
            FWorksheet.WriteBackgroundColor(FCurRow, FCurCol, scColor);
{$ELSE}
            FWorksheet.WriteBackgroundColor(FCurRow, FCurCol, CC);
{$ENDIF}
          end;

          CB:=[cbNorth, cbWest, cbEast, cbSouth];

          FWorksheet.WriteBorderColor(FCurRow + k, FCurCol, cbNorth, scColorBlack);

          if not Assigned(CT.CaptionLine(k).Next) then
            FWorksheet.WriteBorderColor(FCurRow + k, FCurCol, cbWest, scColorBlack)
          else
            CB:=CB - [cbWest];

          if not Assigned(CT.CaptionLine(k).Prior) then
            FWorksheet.WriteBorderColor(FCurRow + k, FCurCol, cbEast, scColorBlack)
          else
            CB:=CB - [cbEast];


          FWorksheet.WriteBorderColor(FCurRow + k ,FCurCol, cbSouth, scColorBlack);

          FWorksheet.WriteBorders(FCurRow + k, FCurCol, CB);

          FWorksheet.WriteHorAlignment(FCurRow + k, FCurCol, ssAligns[C.Title.Alignment]);

          FWorksheet.WriteUTF8Text(FCurRow + k, FCurCol, CT.CaptionLine(k).Caption);

          if Assigned(CT.CaptionLine(k).Next) and not Assigned(CT.CaptionLine(k).Prior) then
          begin
            //Merge title cell
            P:=CT.CaptionLine(k);
            CC:=FCurCol;
            while Assigned(P.Next) do
            begin
              Inc(CC);
              P:=P.Next;
            end;
            if CC<>FCurCol then
              FWorksheet.MergeCells(FCurRow, FCurCol, FCurRow, CC);
          end;
        end;
      end
      else
      begin
        CC:=C.Title.Color;
        if (CC and SYS_COLOR_BASE) = 0  then
        begin
          {$IFDEF OLD_fpSPREADSHEET}
          scColor:=FWorkbook.AddColorToPalette(CC);
          FWorksheet.WriteBackgroundColor( FCurRow, FCurCol, scColor);
          {$ELSE}
          FWorksheet.WriteBackgroundColor( FCurRow, FCurCol, CC);
          {$ENDIF}
        end;

        FWorksheet.WriteBorders(FCurRow,FCurCol, [cbNorth, cbWest, cbEast, cbSouth]);
        FWorksheet.WriteBorderColor(FCurRow,FCurCol, cbNorth, scColorBlack);
        FWorksheet.WriteBorderColor(FCurRow,FCurCol, cbWest, scColorBlack);
        FWorksheet.WriteBorderColor(FCurRow,FCurCol, cbEast, scColorBlack);
        FWorksheet.WriteBorderColor(FCurRow,FCurCol, cbSouth, scColorBlack);

        FWorksheet.WriteHorAlignment(FCurRow, FCurCol, ssAligns[C.Title.Alignment]);

        FWorksheet.WriteUTF8Text(FCurRow, FCurCol, C.Title.Caption);

      end;

      inc(FCurCol);
    end;
  end;


  if FMaxTitleHeight > 1 then
  begin
    for i:=0 to FRxDBGrid.Columns.Count-1 do
    begin
      C:=FRxDBGrid.Columns[i] as TRxColumn;
      CT:=C.Title as TRxColumnTitle;
      if CT.CaptionLinesCount < FMaxTitleHeight then
      begin
        FWorksheet.MergeCells( FCurRow + CT.CaptionLinesCount, I, FCurRow + FMaxTitleHeight - 1, I);
      end;
    end;
  end;

  inc(FCurRow, FMaxTitleHeight);
  FFirstDataRow:=FCurRow;
end;

procedure TRxDBGridExportSpreadSheet.DoExportBody;
begin
  if (dgMultiselect in RxDBGrid.Options) and (RxDBGrid.SelectedRows.Count > 0) and (ressExportSelectedRows in FOptions) then
    ExpSelectedRow
  else
    ExpAllRow;

  FLastDataRow:=FCurRow-1;
end;

procedure TRxDBGridExportSpreadSheet.DoExportFooter;
var
  FooterColor:TColor;

procedure OutFooterCellProps;
{$IFDEF OLD_fpSPREADSHEET}
var
  scColor : TsColor;
{$ENDIF}
begin
  if (FRxDBGrid.FooterOptions.Color and SYS_COLOR_BASE) = 0  then
  begin
    {$IFDEF OLD_fpSPREADSHEET}
    scColor:=FWorkbook.AddColorToPalette(CC);
    FWorksheet.WriteBackgroundColor(FCurRow,FCurCol, scColor);}
    {$ELSE}
    FWorksheet.WriteBackgroundColor(FCurRow,FCurCol, FRxDBGrid.FooterOptions.Color);
    {$ENDIF}
  end;
  FWorksheet.WriteBorders(FCurRow,FCurCol, [cbNorth, cbWest, cbEast, cbSouth]);
  FWorksheet.WriteBorderColor(FCurRow,FCurCol, cbNorth, scColorBlack);
  FWorksheet.WriteBorderColor(FCurRow,FCurCol, cbWest, scColorBlack);
  FWorksheet.WriteBorderColor(FCurRow,FCurCol, cbEast, scColorBlack);
  FWorksheet.WriteBorderColor(FCurRow,FCurCol, cbSouth, scColorBlack);
end;

procedure OutFooterCell(Footer: TRxColumnFooterItem);
var
  D: Integer;
  SF: String;
begin
  if (Footer.ValueType <> fvtNon) then
  begin
    if (ressExportFormula in FOptions) and (Footer.ValueType in [fvtSum, fvtMax, fvtMin]) and (FFirstDataRow <= FLastDataRow) {and (Footer.DisplayFormat = '')} then
    begin
      D:=ColIndex(RxDBGrid.ColumnByFieldName(Footer.FieldName));

      if D>=0 then
      begin
        case Footer.ValueType of
          fvtSum:SF:='SUM';
          fvtMax:SF:='MIN';
          fvtMin:SF:='MAX';
        else
          SF:='Error!(';
        end;

        FWorksheet.WriteFormula(FCurRow, FCurCol,
          Format('=%s(%s%d:%s%d)', [SF, GetColString(D), FFirstDataRow+1, GetColString(D), FLastDataRow+1]));
      end
      else
      begin
        FWorksheet.WriteNumber(FCurRow, FCurCol, Footer.NumericValue, nfFixed, 2);
      end;
    end
    else
      FWorksheet.WriteUTF8Text(FCurRow, FCurCol, Footer.DisplayText);
    FWorksheet.WriteHorAlignment(FCurRow, FCurCol, ssAligns[Footer.Alignment]);
  end;
end;

var
  i , j: Integer;
  C : TRxColumn;
begin
  for j:=0 to FRxDBGrid.FooterOptions.RowCount-1 do
  begin
    FCurCol:=0;
    for i:=0 to FRxDBGrid.Columns.Count - 1 do
    begin
      C:=FRxDBGrid.Columns[i] as TRxColumn;
      if C.Visible then
      begin
        OutFooterCellProps;
        if C.Footers.Count>j then
          OutFooterCell(C.Footers[j])
        else
        if J=0 then
          OutFooterCell(C.Footer);
        inc(FCurCol);
      end;
    end;
    Inc(FCurRow);
  end;
end;

procedure TRxDBGridExportSpreadSheet.DoExportColWidth;
var
  //FW:integer;
  C:TRxColumn;
  i: Integer;
begin
  //FW:=FRxDBGrid.Canvas.TextWidth('W');
  FCurCol:=0;
  for i:=0 to FRxDBGrid.Columns.Count - 1 do
  begin
    C:=FRxDBGrid.Columns[i] as TRxColumn;
    if C.Visible then
    begin
      //FWorksheet.WriteColWidth(FCurCol, Max(C.Width div FW, 20));
      FWorksheet.WriteColWidth(FCurCol, C.Width, suPoints);
      inc(FCurCol);
    end;
  end;
end;


constructor TRxDBGridExportSpreadSheet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaption:=sToolsExportSpeadSheet;
  FOpenAfterExport:=false;
end;

function TRxDBGridExportSpreadSheet.DoExecTools: boolean;
var
  P:TBookMark;
begin
  Result:=false;
  if (not Assigned(FRxDBGrid)) or (not Assigned(FRxDBGrid.DataSource)) or (not Assigned(FRxDBGrid.DataSource.DataSet)) then
    exit;
  FDataSet:=FRxDBGrid.DataSource.DataSet;
  FDataSet.DisableControls;
  {$IFDEF NoAutomatedBookmark}
  P:=FDataSet.GetBookmark;
  {$ELSE}
  P:=FDataSet.Bookmark;
  {$ENDIF}

  FWorkbook := TsWorkbook.Create;
  FWorksheet := FWorkbook.AddWorksheet(FPageName);
  try
    scColorBlack:=FRxDBGrid.GridLineColor;
    FCurRow:=0;
    FFirstDataRow:=0;
    FLastDataRow:=-1;

    if ressExportTitle in FOptions then
      DoExportTitle;
    DoExportBody;

    if (ressExportFooter in FOptions) and (RxDBGrid.FooterOptions.Active) and (RxDBGrid.FooterOptions.RowCount>0) then
      DoExportFooter;

    DoExportColWidth;

    FWorkbook.WriteToFile(UTF8ToSys(FileName), true);
    Result:=true;
  finally
    FWorkbook.Free;
    {$IFDEF NoAutomatedBookmark}
    FDataSet.GotoBookmark(P);
    FDataSet.FreeBookmark(P);
    {$ELSE}
    FDataSet.Bookmark:=P;
    {$ENDIF}
    FDataSet.EnableControls;
  end;

  if Result and FOpenAfterExport then
    OpenDocument(FileName);
end;

function TRxDBGridExportSpreadSheet.DoSetupTools: boolean;
var
  F:TRxDBGridExportSpreadSheet_ParamsForm;
begin
  F:=TRxDBGridExportSpreadSheet_ParamsForm.Create(Application);
  F.FileNameEdit1.FileName:=FFileName;
  F.cbOpenAfterExport.Checked:=FOpenAfterExport;
  F.cbExportColumnFooter.Checked:=ressExportFooter in FOptions;
  F.cbExportColumnHeader.Checked:=ressExportTitle in FOptions;
  F.cbExportCellColors.Checked:=ressExportColors in FOptions;
  F.cbOverwriteExisting.Checked:=ressOverwriteExisting in FOptions;
  F.cbExportFormula.Checked:=ressExportFormula in FOptions;
  F.cbExportSelectedRows.Checked:=ressExportSelectedRows in FOptions;
  F.cbExportSelectedRows.Enabled:=(dgMultiselect in RxDBGrid.Options) and (RxDBGrid.SelectedRows.Count > 0);

  F.edtPageName.Text:=FPageName;

  Result:=F.ShowModal = mrOk;
  if Result then
  begin
    FOpenAfterExport:=F.cbOpenAfterExport.Checked;
    FFileName:=F.FileNameEdit1.FileName;
    FPageName:=F.edtPageName.Text;

    FOptions:=[];
    if F.cbExportColumnFooter.Checked then
      FOptions :=FOptions + [ressExportFooter];
    if F.cbExportColumnHeader.Checked then
      FOptions :=FOptions + [ressExportTitle];
    if F.cbExportCellColors.Checked then
      FOptions :=FOptions + [ressExportColors];
    if F.cbOverwriteExisting.Checked then
      FOptions :=FOptions + [ressOverwriteExisting];
    if F.cbExportFormula.Checked then
      FOptions :=FOptions + [ressExportFormula];
    if F.cbExportSelectedRows.Enabled and F.cbExportSelectedRows.Checked then
      FOptions :=FOptions + [ressExportSelectedRows];
  end;
  F.Free;
end;

end.
