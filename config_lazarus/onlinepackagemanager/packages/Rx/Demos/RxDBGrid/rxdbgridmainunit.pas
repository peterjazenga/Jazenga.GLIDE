unit RxDBGridMainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, rxmemds,
  DB, rxdbgrid, RxAboutDialog, RxIniPropStorage, RxDBGridPrintGrid,
  RxDBGridExportSpreadSheet, RxDBGridFooterTools, rxtooledit, RxDBGridExportPdf,
  ExtCtrls, Buttons, Menus, ActnList, StdCtrls, DBGrids, Types, Grids;

type

  { TRxDBGridMainForm }

  TRxDBGridMainForm = class(TForm)
    actCalcTotal: TAction;
    CheckBox5: TCheckBox;
    Label2: TLabel;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    RxAboutDialog1: TRxAboutDialog;
    RxDateEdit1: TRxDateEdit;
    RxDBGridExportPDF1: TRxDBGridExportPDF;
    RxDBGridExportSpreadSheet1: TRxDBGridExportSpreadSheet;
    RxDBGridFooterTools1: TRxDBGridFooterTools;
    RxDBGridPrint1: TRxDBGridPrint;
    RxIniPropStorage1: TRxIniPropStorage;
    RxMemoryData1RAIT: TStringField;
    SpeedButton1: TSpeedButton;
    sysExit: TAction;
    hlpAbout: TAction;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    showColumnsDialog: TAction;
    showFindDialog: TAction;
    actOptimizeWidthCol1: TAction;
    actOptimizeColumnsWidthAll: TAction;
    ActionList1: TActionList;
    Button1: TButton;
    Button2: TButton;
    CheckBox1: TCheckBox;
    ComboBox1: TComboBox;
    Datasource1: TDatasource;
    Datasource2: TDatasource;
    Label1: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    RxDBGrid1: TRxDBGrid;
    RxMemoryData1: TRxMemoryData;
    RxMemoryData1Date_Present1: TDateField;
    RxMemoryData1Developer1: TStringField;
    RxMemoryData1DEVELOPER_ID1: TLongintField;
    RxMemoryData1ID1: TLongintField;
    RxMemoryData1NAME1: TStringField;
    RxMemoryData1PRICE1: TFloatField;
    RxMemoryData2: TRxMemoryData;
    RxMemoryData2DEVELOPER_ID1: TLongintField;
    RxMemoryData2DEVELOPER_NAME1: TStringField;
    procedure actCalcTotalExecute(Sender: TObject);
    procedure actOptimizeColumnsWidthAllExecute(Sender: TObject);
    procedure actOptimizeWidthCol1Execute(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure CheckBox3Change(Sender: TObject);
    procedure CheckBox4Change(Sender: TObject);
    procedure CheckBox5Change(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure hlpAboutExecute(Sender: TObject);
    procedure RxDBGrid1DataHintShow(Sender: TObject; CursorPos: TPoint;
      Cell: TGridCoord; Column: TRxColumn; var HintStr: string;
      var Processed: boolean);
    procedure RxDBGrid1Filtred(Sender: TObject);
    procedure RxDBGrid1GetCellProps(Sender: TObject; Field: TField;
      AFont: TFont; var Background: TColor);
    procedure showColumnsDialogExecute(Sender: TObject);
    procedure showFindDialogExecute(Sender: TObject);
    procedure sysExitExecute(Sender: TObject);
    procedure TRxColumnEditButtons0Click(Sender: TObject);
    procedure TRxColumnEditButtons1Click(Sender: TObject);
    procedure TRxColumnEditButtons2Click(Sender: TObject);
  private
    procedure DoFillFilters;
  public
    { public declarations }
  end; 

var
  RxDBGridMainForm: TRxDBGridMainForm;

procedure LocalizeApp;
implementation
uses rxsortmemds, FileUtil, gettext, translations, rxFileUtils, LazFileUtils, LazUTF8;

procedure LocalizeApp;
var
  Lang, FallbackLang: String;
begin
  GetLanguageIDs(Lang{%H-},FallbackLang{%H-}); // in unit gettext
  TranslateUnitResourceStrings('rxconst',NormalizeDirectoryName('../../languages/rxconst.%s.po'), Lang, FallbackLang);
  TranslateUnitResourceStrings('rxdconst',NormalizeDirectoryName('../../languages/rxdconst.%s.po'), Lang, FallbackLang);
end;

{$R *.lfm}

{ TRxDBGridMainForm }

procedure TRxDBGridMainForm.FormCreate(Sender: TObject);
begin
  RxAboutDialog1.LicenseFileName:=AppendPathDelim(AppendPathDelim(ExtractFileDir(ParamStrUTF8(0))) + 'docs')+'COPYING.GPL.txt';

  RxMemoryData2.Open;
  RxMemoryData2.AppendRecord([1, 'Open source']);
  RxMemoryData2.AppendRecord([2, 'Borland']);
  RxMemoryData2.AppendRecord([3, 'Microsoft']);

  RxMemoryData1.Open;
  RxMemoryData1.AppendRecord([1, 'Lazarus 0.9.23', 0, 'Open source', EncodeDate(2006, 1, 1), 1]);
  RxMemoryData1.AppendRecord([2, 'Delphi 7.0 Prof', 990, 'Borland', EncodeDate(2002, 1, 1), 2]);
  RxMemoryData1.AppendRecord([3, 'Open Office 2.2.0', 0, 'Open source', EncodeDate(2006, 10, 1), 1]);
  RxMemoryData1.AppendRecord([4, 'Microsoft Office', 150, 'Microsoft', EncodeDate(1997, 8, 12), 3]);
  RxMemoryData1.AppendRecord([5, 'Microsoft Windows 95', 50, 'Microsoft', EncodeDate(1995, 8, 12), 3]);
  RxMemoryData1.AppendRecord([6, 'Microsoft Windows 98', 90, 'Microsoft', EncodeDate(1997, 12, 12), 3]);
  RxMemoryData1.AppendRecord([7, 'Microsoft Windows ME', 90, 'Microsoft', EncodeDate(1999, 10, 25), 3]);
  RxMemoryData1.AppendRecord([8, 'Microsoft Windows NT 4.0', 250, 'Microsoft', EncodeDate(1996, 2, 3), 3]);
  RxMemoryData1.AppendRecord([9, 'Microsoft Windows 2000', 150, 'Microsoft', EncodeDate(1999, 11, 13), 3]);
  RxMemoryData1.AppendRecord([10, 'Microsoft Windows XP', 130, 'Microsoft', EncodeDate(2003, 10, 1), 3]);
  RxMemoryData1.AppendRecord([11, 'Microsoft Windows Vista', 180, 'Microsoft', EncodeDate(2007, 2, 1), 3]);
  RxMemoryData1.AppendRecord([12, 'Поисковая системя Яндекс', 0, 'Yandex', EncodeDate(2007, 2, 1), 3]);
  RxMemoryData1.AppendRecord([13, 'Бухгалтерия 1С', 280, '1С', EncodeDate(1994, 2, 1), 3]);
  RxMemoryData1.AppendRecord([14, 'Бух. комплекс "45-я Параллель"', 180, 'ООО "Boot"', EncodeDate(2007, 2, 1), 3]);
  RxMemoryData1.AppendRecord([15, 'Консультант+', 380, 'Консультант+', EncodeDate(2007, 2, 1), 3]);
  RxMemoryData1.AppendRecord([16, 'Гарант', 480, 'Гарант', EncodeDate(2007, 2, 1), 3]);

  RxMemoryData1.First;

end;

procedure TRxDBGridMainForm.hlpAboutExecute(Sender: TObject);
begin
  RxAboutDialog1.Execute;
end;

procedure TRxDBGridMainForm.RxDBGrid1DataHintShow(Sender: TObject;
  CursorPos: TPoint; Cell: TGridCoord; Column: TRxColumn; var HintStr: string;
  var Processed: boolean);
begin
  if Assigned(Column.Field) and  (Column.Field = RxMemoryData1RAIT) then
  begin
    Processed:=true;
    if HintStr = '' then
      HintStr:='Not defined'
    else
    if HintStr = 'Positive' then
      HintStr:='A very good result'
    else
    if HintStr = 'Negative' then
      HintStr:='It''s too bad';
  end;
end;

procedure TRxDBGridMainForm.RxDBGrid1Filtred(Sender: TObject);
begin
  RxMemoryData1.First;
end;

procedure TRxDBGridMainForm.RxDBGrid1GetCellProps(Sender: TObject;
  Field: TField; AFont: TFont; var Background: TColor);
begin
  if (Field = RxMemoryData1PRICE1) and (RxMemoryData1PRICE1.AsFloat>99) then
    Background:=clRed;
end;


procedure TRxDBGridMainForm.showColumnsDialogExecute(Sender: TObject);
begin
  RxDBGrid1.ShowColumnsDialog;
end;

procedure TRxDBGridMainForm.showFindDialogExecute(Sender: TObject);
begin
  RxDBGrid1.ShowFindDialog;
end;

procedure TRxDBGridMainForm.sysExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TRxDBGridMainForm.TRxColumnEditButtons0Click(Sender: TObject);
begin
  ShowMessage('Click first button');
end;

procedure TRxDBGridMainForm.TRxColumnEditButtons1Click(Sender: TObject);
begin
  ShowMessage('Click next button');
end;

procedure TRxDBGridMainForm.TRxColumnEditButtons2Click(Sender: TObject);
begin
  if RxMemoryData1.State <> dsEdit then
    RxMemoryData1.Edit;
  RxMemoryData1PRICE1.Clear;
  RxMemoryData1.Post;
end;

procedure TRxDBGridMainForm.DoFillFilters;
var
  C:TRxColumn;
  i:integer;
begin
  for i:=0 to RxDBGrid1.Columns.Count-1 do
  begin
    C:=TRxColumn(RxDBGrid1.Columns[i]);
    C.Filter.EmptyValue:='None...';
    C.Filter.ValueList.Add(C.Filter.EmptyValue);
  end;

  RxMemoryData1.First;
  while not RxMemoryData1.EOF do
  begin
    for i:=0 to RxDBGrid1.Columns.Count-1 do
    begin
      C:=TRxColumn(RxDBGrid1.Columns[i]);
      if C.Filter.ValueList.IndexOf(C.Field.AsString)<0 then
        C.Filter.ValueList.Add(C.Field.AsString);
    end;
    RxMemoryData1.Next;
  end;
end;

procedure TRxDBGridMainForm.actCalcTotalExecute(Sender: TObject);
begin
  RxDBGrid1.CalcStatTotals;
end;

procedure TRxDBGridMainForm.actOptimizeColumnsWidthAllExecute(Sender: TObject);
begin
  RxDBGrid1.OptimizeColumnsWidthAll;
end;

procedure TRxDBGridMainForm.actOptimizeWidthCol1Execute(Sender: TObject);
begin
  TRxColumn(RxDBGrid1.SelectedColumn).OptimizeWidth;
end;

procedure TRxDBGridMainForm.CheckBox1Change(Sender: TObject);
begin
  if CheckBox1.Checked then
    RxDBGrid1.OptionsRx:=RxDBGrid1.OptionsRx + [rdgFilter]
  else
    RxDBGrid1.OptionsRx:=RxDBGrid1.OptionsRx - [rdgFilter];
  RxMemoryData1.Filtered:=CheckBox1.Checked;
end;

procedure TRxDBGridMainForm.CheckBox2Change(Sender: TObject);
begin
  RxDBGrid1.AutoFillColumns:=CheckBox2.Checked;
end;

procedure TRxDBGridMainForm.CheckBox3Change(Sender: TObject);
begin
  if CheckBox3.Checked then
    RxDBGrid1.Options:=RxDBGrid1.Options + [dgIndicator]
  else
    RxDBGrid1.Options:=RxDBGrid1.Options - [dgIndicator];
end;

procedure TRxDBGridMainForm.CheckBox4Change(Sender: TObject);
begin
  if CheckBox4.Checked then
    RxDBGrid1.Options:=RxDBGrid1.Options + [dgTitles]
  else
    RxDBGrid1.Options:=RxDBGrid1.Options - [dgTitles];
end;

procedure TRxDBGridMainForm.CheckBox5Change(Sender: TObject);
begin
  if CheckBox5.Checked then
    RxDBGrid1.OptionsRx:=RxDBGrid1.OptionsRx + [rdgWordWrap]
  else
    RxDBGrid1.OptionsRx:=RxDBGrid1.OptionsRx - [rdgWordWrap];
end;

procedure TRxDBGridMainForm.ComboBox1Change(Sender: TObject);
begin
  RxDBGrid1.ReadOnly:=ComboBox1.ItemIndex <> 0;
end;

end.

