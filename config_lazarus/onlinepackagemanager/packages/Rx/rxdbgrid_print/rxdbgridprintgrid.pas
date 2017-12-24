{ RxDBGridPrintGrid unit

  Copyright (C) 2005-2014 Lagunov Aleksey alexs@yandex.ru and Lazarus team
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
unit RxDBGridPrintGrid;

{$I rx.inc}

interface

uses
  Classes, SysUtils, DB, rxdbgrid, LR_Class, LR_DSet, LR_DBSet, contnrs,
  Graphics, Printers, rxvclutils;

type
  TRxDBGridPrintOption =
    (rxpoShowTitle,
     rxpoShowFooter,
     rxpoShowGridColor,
     rxpoShowFooterColor,
     rxpoShowReportTitle,
     rxpoHideZeroValues
     );
  TRxDBGridPrintOptions = set of TRxDBGridPrintOption;


  { TRxColInfo }

  TRxColInfo = class
    Col:TRxColumn;
    ColWidth:integer;
    ColTitles:TStringList;
    constructor Create;
    destructor Destroy; override;
  end;

  { TRxDBGridPrint }

  TRxDBGridPrint = class(TRxDBGridAbstractTools)
  private
    FModifyPrepared: boolean;
    FOptions: TRxDBGridPrintOptions;
    FOrientation: TPrinterOrientation;
    FPageMargin: TRxPageMargin;
    FReport : TfrReport;
    FReportDataSet : TfrDBDataSet;
    FColumnDataSet : TfrUserDataSet;
    FDataSet : TDataset;
    FPage : TfrPage;
    FReportTitle: string;
    FShowColumnHeaderOnAllPage: boolean;

    FShowProgress : Boolean;
    FTitleRowCount : integer;
    FRxColInfoList : TObjectList;

    FYPos: Integer;
    FXPos: Integer;
    procedure DoCreateReport;

    procedure DoShowReportTitle;
    procedure DoSetupColumns;
    procedure DoShowColumnsTitle;
    procedure DoShowFooter;
    procedure OnPrintColumn(ColNo: Integer; var Width: Integer);
    procedure OnEnterRect(Memo: TStringList; View: TfrView);
    procedure SetPageMargin(AValue: TRxPageMargin);
  protected
    function DoExecTools:boolean;override;
    function DoSetupTools:boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure PreviewReport;
  published
    property Orientation: TPrinterOrientation read FOrientation write FOrientation default poPortrait;
    property Options:TRxDBGridPrintOptions read FOptions write FOptions;
    property ShowProgress : Boolean read FShowProgress write FShowProgress default false;
    property PageMargin:TRxPageMargin read FPageMargin write SetPageMargin;
    property ReportTitle:string read FReportTitle write FReportTitle;
    property ShowColumnHeaderOnAllPage:boolean read FShowColumnHeaderOnAllPage write FShowColumnHeaderOnAllPage default false;
    property ModifyPrepared:boolean read FModifyPrepared write FModifyPrepared default false;
  end;

procedure Register;
implementation

uses math, RxDBGridPrintGrid_SetupUnit, Forms, Controls, rxdconst;

{$R rxdbgridprintgrid.res}

procedure Register;
begin
  RegisterComponents('RX DBAware',[TRxDBGridPrint]);
end;

{ TRxColInfo }

constructor TRxColInfo.Create;
begin
  inherited Create;
  ColTitles:=TStringList.Create;
end;

destructor TRxColInfo.Destroy;
begin
  ColTitles.Clear;
  FreeAndNil(ColTitles);
  inherited Destroy;
end;

{ TRxDBGridPrint }

procedure TRxDBGridPrint.DoShowReportTitle;
var
  FBand: TfrBandView;
  FView: TfrMemoView;
begin
  FBand := TfrBandView(frCreateObject(gtBand, '', FPage));
  FBand.SetBounds(10, FYPos, 1000, 25);
  FBand.BandType := btReportTitle;
//  FPage.Objects.Add(FBand);

  FView := frCreateObject(gtMemo, '', FPage) as TfrMemoView;
  FView.SetBounds(FXPos, FYPos, FPage.PrnInfo.PgW - 40, 25);
  FView.Alignment:=taCenter;
  FView.Font.Size:=12;
//    FView.Font.Assign(FTitleFont);
  FView.Memo.Add(FReportTitle);

//  FPage.Objects.Add(FView);

  Inc(FYPos, 27)
end;

procedure TRxDBGridPrint.DoCreateReport;
var
  FBand: TfrBandView;
  FView: TfrMemoView;
begin
  if FReport.Pages.Count=0 then
      FReport.Pages.add;
  FPage := FReport.Pages[FReport.Pages.Count-1];
  FPage.ChangePaper(FPage.pgSize, FPage.Width, FPage.Height, FOrientation);

  FPage.Margins.Top:=FPageMargin.Top;
  FPage.Margins.Left:=FPageMargin.Left;
  FPage.Margins.Bottom:=FPageMargin.Bottom;
  FPage.Margins.Right:=FPageMargin.Right;

  FYPos:=FPageMargin.Top;
  FXPos:=FPageMargin.Left;

  if rxpoShowReportTitle in FOptions then
    DoShowReportTitle;

  if rxpoShowTitle in FOptions then
    DoShowColumnsTitle;

  FBand := TfrBandView(frCreateObject(gtBand, '', FPage));
  FBand.BandType := btMasterData;
  FBand.Dataset := FReportDataSet.Name;
  FBand.SetBounds(0, FYPos, 1000, 18);
  FBand.Flags:=FBand.Flags or flStretched;
//  FPage.Objects.Add(FBand);

  FBand := TfrBandView(frCreateObject(gtBand, '', FPage));
  FBand.BandType := btCrossData;
  FBand.Dataset := FColumnDataSet.Name;
  FBand.SetBounds(FXPos, 0, 20, 1000);
//  FPage.Objects.Add(FBand);

  FView := frCreateObject(gtMemo, '', FPage) as TfrMemoView;
  FView.SetBounds(FXPos, FYPos, 20, 18);
  FView.Memo.Add('[Cell]');
  FView.Flags:=FView.Flags or flStretched;
  FView.Font.Size:=10;
//  FView.Font.Assign(FFont);
  FView.Frames:=frAllFrames;
  FView.Layout:=tlTop;
//  FPage.Objects.Add(FView);

  FYPos := FYPos + 22;

  if (RxDBGrid.FooterOptions.Active) and (RxDBGrid.FooterOptions.RowCount>0) then
    DoShowFooter;
end;

procedure TRxDBGridPrint.DoSetupColumns;
var
  P:TRxColInfo;
  i: Integer;
  j: Integer;
begin
  FTitleRowCount:=1;
  FRxColInfoList.Clear;
  for i:=0 to RxDBGrid.Columns.Count-1 do
  begin
    if RxDBGrid.Columns[i].Visible then
    begin
      P:=TRxColInfo.Create;
      FRxColInfoList.Add(P);
      P.Col:=RxDBGrid.Columns[i] as TRxColumn;
      P.ColWidth:=RxDBGrid.Columns[i].Width;
      for j:=0 to TRxColumnTitle(RxDBGrid.Columns[i].Title).CaptionLinesCount-1 do
        P.ColTitles.Add(TRxColumnTitle(RxDBGrid.Columns[i].Title).CaptionLine(j).Caption);
      FTitleRowCount:=Max(FTitleRowCount, P.ColTitles.Count)
    end;
  end;
end;

procedure TRxDBGridPrint.DoShowColumnsTitle;
var
  FBand: TfrBandView;
  FView: TfrMemoView;
  i: Integer;
begin
  FBand := TfrBandView(frCreateObject(gtBand, '', FPage));
  FBand.BandType := btMasterHeader;
  FBand.SetBounds(0, FYPos, 1000, 20 * FTitleRowCount);
  FBand.Flags:=FBand.Flags or flStretched;
//  FPage.Objects.Add(FBand);
  if FShowColumnHeaderOnAllPage then
    FBand.Flags:=FBand.Flags + flBandRepeatHeader;

  for i:=0 to FTitleRowCount-1 do
  begin
    FView := frCreateObject(gtMemo, '', FPage) as TfrMemoView;
    FView.SetBounds(FXPos, FYPos, 20, 20);
    FView.Alignment:=taCenter;
    FView.FillColor := clSilver;
  //  FView.Font.Assign(FTitleFont);
    FView.Font.Size:=12;
    FView.Frames:=frAllFrames;
    FView.Layout:=tlTop;
    FView.Memo.Add(Format('Header_%d', [i]));
//    FPage.Objects.Add(FView);
    FYPos:=FYPos + 20
  end;
  FYPos := FYPos + 2;
end;

procedure TRxDBGridPrint.DoShowFooter;
var
  FBand: TfrBandView;
  FView: TfrMemoView;
begin
  FBand := TfrBandView(frCreateObject(gtBand, '', FPage));
  FBand.BandType := btMasterFooter;

  FBand.SetBounds(FXPos, FYPos, 1000, 20);
  FBand.Flags:=FBand.Flags or flStretched;

  FView := frCreateObject(gtMemo, '', FPage) as TfrMemoView;
  FView.SetBounds(FXPos, FYPos, 20, 20);

  if rxpoShowFooterColor in FOptions then
    FView.FillColor := RxDBGrid.FooterOptions.Color;

  FView.Font.Size:=12;
  FView.Frames:=frAllFrames;
  FView.Layout:=tlTop;
  FView.Memo.Add('Footer');

  FYPos := FYPos + 22;
end;

procedure TRxDBGridPrint.OnPrintColumn(ColNo: Integer; var Width: Integer);
begin
  if (ColNo > 0) and (ColNo <= FRxColInfoList.Count) then
    Width := TRxColInfo(FRxColInfoList[ColNo-1]).ColWidth;
end;

procedure TRxDBGridPrint.OnEnterRect(Memo: TStringList; View: TfrView);
var
  i, k: Integer;
  F:TRxColInfo;
  S: String;
  C:TColor;
  J: Integer;
begin
  i := FColumnDataset.RecNo;

  if (i >= 0) and (i < FRxColInfoList.Count) then
  begin
    F:=TRxColInfo(FRxColInfoList[i]);
    View.dx := F.ColWidth;

    if Assigned(F.Col) and (Memo.Count>0) then
    begin
      S:=Memo[0];
      if (S='[Cell]') and Assigned(F.Col.Field) then
      begin
        if rxpoShowGridColor in FOptions then
        begin
          C:=F.Col.Color;
          if Assigned(RxDBGrid.OnGetCellProps) then
            RxDBGrid.OnGetCellProps(RxDBGrid, F.Col.Field, TfrMemoView(View).Font, C);
          if C = clWindow then
            C := clNone;
          TfrMemoView(View).FillColor:=C;
        end;

        S:=F.Col.Field.DisplayText;
        if Assigned(F.Col) and (F.Col.KeyList.Count > 0) and (F.Col.PickList.Count > 0) then
        begin
          J := F.Col.KeyList.IndexOf(S);
          if (J >= 0) and (J < F.Col.PickList.Count) then
            S := F.Col.PickList[j];
        end
        else
        if (rxpoHideZeroValues in FOptions) and Assigned(F.Col.Field) and (F.Col.Field.DataType in [ftSmallint, ftInteger, ftWord,
               ftFloat, ftCurrency, ftLargeint]) and (F.Col.Field.AsFloat = 0) then
          S:='';

        Memo[0] := S;
        TfrMemoView(View).Alignment:=F.Col.Alignment;
      end                                            else
      if Copy(S, 1, 7) = 'Header_' then
      begin
        TfrMemoView(View).Alignment:=F.Col.Title.Alignment;
        K:=StrToIntDef(Copy(S, 8, Length(S)), 0);
        if TRxColumnTitle(F.Col.Title).CaptionLinesCount = 0 then
        begin
          S:=TRxColumnTitle(F.Col.Title).Caption;
          if K = 0 then
            Memo[0] := TRxColumnTitle(F.Col.Title).Caption
          else
            Memo[0] := '';
        end
        else
        if K<TRxColumnTitle(F.Col.Title).CaptionLinesCount then
        begin;
          Memo[0] :=TRxColumnTitle(F.Col.Title).CaptionLine(k).Caption; //F.Col.Title.Caption;
        end
        else
          Memo[0] := '';
      end
      else
      if S = 'Footer' then
      begin
        Memo[0] :=F.Col.Footer.DisplayText;
        TfrMemoView(View).Alignment:=F.Col.Footer.Alignment;
      end;
    end;

  end;
end;

procedure TRxDBGridPrint.SetPageMargin(AValue: TRxPageMargin);
begin
  FPageMargin.Assign(AValue);
end;

function TRxDBGridPrint.DoExecTools: boolean;
var
  SaveDesign: TfrReportDesigner;
begin
  Result:=false;
  if (RxDBGrid = nil) or (RxDBGrid.DataSource = nil) or (RxDBGrid.DataSource.Dataset = nil) then
    Exit;

  SaveDesign:=frDesigner;
  frDesigner:=nil;

  FDataSet := RxDBGrid.Datasource.Dataset;
  FReport:=TfrReport.Create(Self);
  FReport.OnPrintColumn:=@OnPrintColumn;
  FReport.OnEnterRect:=@OnEnterRect;
  FReport.ModifyPrepared:=FModifyPrepared;
  FReportDataSet := TfrDBDataSet.Create(Self);
  FColumnDataSet := TfrUserDataSet.Create(Self);

  try
    DoSetupColumns;

    FReportDataSet.Name := 'frGridDBDataSet1';
    FReportDataSet.DataSet := FDataSet;
//    FReportDataSet.DataSource := RxDBGrid.DataSource;

    FColumnDataSet.Name := 'frGridUserDataSet1';
    FColumnDataSet.RangeEnd := reCount;

    FColumnDataSet.RangeEndCount := FRxColInfoList.Count;

    FReport.ShowProgress:=FShowProgress;
    DoCreateReport;

    frDesigner:=SaveDesign;

    FReport.ShowReport;
    Result:=true;
  finally
    FreeAndNil(FColumnDataSet);
    FreeAndNil(FReportDataSet);
    FreeAndNil(FReport);
//    frDesigner:=SaveDesign;
  end;
end;

function TRxDBGridPrint.DoSetupTools: boolean;
var
  RxDBGridPrintGrid_SetupForm: TRxDBGridPrintGrid_SetupForm;
begin
  RxDBGridPrintGrid_SetupForm:=TRxDBGridPrintGrid_SetupForm.Create(Application);

  RxDBGridPrintGrid_SetupForm.Edit1.Text:=FReportTitle;
  RxDBGridPrintGrid_SetupForm.RadioGroup1.ItemIndex:=ord(FOrientation);

  RxDBGridPrintGrid_SetupForm.SpinEdit2.Value:=FPageMargin.Left;
  RxDBGridPrintGrid_SetupForm.SpinEdit1.Value:=FPageMargin.Top;
  RxDBGridPrintGrid_SetupForm.SpinEdit3.Value:=FPageMargin.Right;
  RxDBGridPrintGrid_SetupForm.SpinEdit4.Value:=FPageMargin.Bottom;

  RxDBGridPrintGrid_SetupForm.CheckBox1.Checked:=FShowColumnHeaderOnAllPage;

  RxDBGridPrintGrid_SetupForm.CheckGroup1.Checked[0]:=rxpoShowTitle in FOptions;
  RxDBGridPrintGrid_SetupForm.CheckGroup1.Checked[1]:=rxpoShowFooter in FOptions;
  RxDBGridPrintGrid_SetupForm.CheckGroup1.Checked[2]:=rxpoShowFooterColor in FOptions;
  RxDBGridPrintGrid_SetupForm.CheckGroup1.Checked[3]:=rxpoShowGridColor in FOptions;
  RxDBGridPrintGrid_SetupForm.CheckGroup1.Checked[4]:=rxpoShowReportTitle in FOptions;
  RxDBGridPrintGrid_SetupForm.CheckGroup1.Checked[5]:=rxpoHideZeroValues in FOptions;

  Result:=RxDBGridPrintGrid_SetupForm.ShowModal = mrOk;
  if Result then
  begin
    FReportTitle                    := RxDBGridPrintGrid_SetupForm.Edit1.Text;
    FOrientation                    := TPrinterOrientation(RxDBGridPrintGrid_SetupForm.RadioGroup1.ItemIndex);

    FPageMargin.Left                := RxDBGridPrintGrid_SetupForm.SpinEdit2.Value;
    FPageMargin.Top                 := RxDBGridPrintGrid_SetupForm.SpinEdit1.Value;
    FPageMargin.Right               := RxDBGridPrintGrid_SetupForm.SpinEdit3.Value;
    FPageMargin.Bottom              := RxDBGridPrintGrid_SetupForm.SpinEdit4.Value;

    FOptions:=[];
    if RxDBGridPrintGrid_SetupForm.CheckGroup1.Checked[0] then
      FOptions:=FOptions + [rxpoShowTitle];

    if RxDBGridPrintGrid_SetupForm.CheckGroup1.Checked[1] then
      FOptions:=FOptions + [rxpoShowFooter];

    if RxDBGridPrintGrid_SetupForm.CheckGroup1.Checked[2] then
      FOptions:=FOptions + [rxpoShowFooterColor];

    if RxDBGridPrintGrid_SetupForm.CheckGroup1.Checked[3] then
      FOptions:=FOptions + [rxpoShowGridColor];

    if RxDBGridPrintGrid_SetupForm.CheckGroup1.Checked[4] then
      FOptions:=FOptions + [rxpoShowReportTitle];

    if RxDBGridPrintGrid_SetupForm.CheckGroup1.Checked[5] then
      FOptions:=FOptions + [rxpoHideZeroValues];

    FShowColumnHeaderOnAllPage:=RxDBGridPrintGrid_SetupForm.CheckBox1.Checked;
  end;
  RxDBGridPrintGrid_SetupForm.Free;
end;

constructor TRxDBGridPrint.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPageMargin:=TRxPageMargin.Create;

  FCaption:=sPrintGrid;
  FShowProgress:=false;
  FModifyPrepared:=false;
  FRxColInfoList:=TObjectList.Create(true);
  FOrientation:=poPortrait;
  ShowSetupForm:=false;
  FOptions:=[rxpoShowTitle,
     rxpoShowFooter,
     rxpoShowGridColor,
     rxpoShowFooterColor,
     rxpoShowReportTitle];
  FShowColumnHeaderOnAllPage:=false;
end;

destructor TRxDBGridPrint.Destroy;
begin
  FreeAndNil(FRxColInfoList);
  FreeAndNil(FPageMargin);
  inherited Destroy;
end;

procedure TRxDBGridPrint.PreviewReport;
begin
  Execute;
end;

end.

{ DONE -oalexs : Необходимо настраивать отступы в печатной форме}
{ DONE -oalexs : Необходимо настроить отображение раскраски ячеек }
{ DONE -oalexs : Необходимо правильно выгружать лукапные значение KeyList/PickList }
{ TODO -oalexs : Необходимо реализовать настройку шрифтов }

