unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, rxmemds, rxdbgrid, RxDBGridExportPdf,
  Forms, Controls,
  Graphics, Dialogs, ExtCtrls, StdCtrls, ComCtrls, Spin, db;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    DataSource1: TDataSource;
    ImageList1: TImageList;
    Memo1: TMemo;
    PageControl1: TPageControl;
    Panel1: TPanel;
    RxDBGrid1: TRxDBGrid;
    RxDBGridExportPDF1: TRxDBGridExportPDF;
    RxMemoryData1: TRxMemoryData;
    RxMemoryData1Country: TStringField;
    RxMemoryData1FLAG: TLongintField;
    RxMemoryData1ID: TAutoIncField;
    RxMemoryData1NAME: TStringField;
    RxMemoryData1PDATE: TDateField;
    RxMemoryData1Sity: TStringField;
    RxMemoryData1SUM: TCurrencyField;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    FontDirList: TStrings;
    procedure InitFonts;
    procedure ShowInfo(AText:string; AParams : array of const);
    procedure DebugFonts;
    procedure CreateFontDirList;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation
uses fpTTF, LazFileUtils;

{$R *.lfm}
const
  TestText = 'Образец текста';

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  RxDBGridExportPDF1.ShowSetupForm:=true;
  PageControl1.ActivePageIndex:=0;
  RxMemoryData1.Open;
  RxMemoryData1.AppendRecord([1, 'Строка с длинным текстом 1', now, 100, 'Россия', 'Москва', 0]);
  RxMemoryData1.AppendRecord([2, 'Строка с длинным текстом 2', now - 1, 100, 'Россия', 'Ставрополь', 1]);
  RxMemoryData1.AppendRecord([3, 'Строка с длинным текстом 3', now - 2, 110, 'Россия', 'Калининград', 2]);
  RxMemoryData1.AppendRecord([4, 'Строка с длинным текстом 4', now - 3, 5000, 'Россия', 'Владивасток', 0]);
  RxMemoryData1.AppendRecord([5, 'Строка с длинным текстом 5', now - 4, 123.31, 'USA', 'New-York', 0]);
  RxMemoryData1.AppendRecord([6, 'Строка с длинным текстом 6', now, 100, 'Россия', 'Москва', 0]);
  RxMemoryData1.AppendRecord([7, 'Строка с длинным текстом 7', now - 1, 100, 'Россия', 'Ставрополь', 2]);
  RxMemoryData1.AppendRecord([8, 'Строка с длинным текстом 8', now - 2, 110, 'Россия', 'Калининград', 1]);
  RxMemoryData1.AppendRecord([9, 'Строка с длинным текстом 9', now - 3, 5000, 'Россия', 'Владивасток', 0]);
  RxMemoryData1.AppendRecord([10,'Строка с длинным текстом 10', now - 4, 123.31, 'USA', 'New-York', 3]);
  RxMemoryData1.AppendRecord([11,'Строка с длинным текстом 11', now, 100, 'Россия', 'Москва', 2]);
  RxMemoryData1.AppendRecord([12,'Строка с длинным текстом 12', now - 1, 100, 'Россия', 'Ставрополь', 1]);
  RxMemoryData1.AppendRecord([13,'Строка с длинным текстом 13', now - 2, 110, 'Россия', 'Калининград', 0]);
  RxMemoryData1.AppendRecord([14,'Строка с длинным текстом 14', now - 3, 5000, 'Россия', 'Владивасток', 3]);
  RxMemoryData1.AppendRecord([15,'Строка с длинным текстом 15', now - 4, 123.31, 'USA', 'New-York', 2]);
  RxMemoryData1.AppendRecord([16,'Строка с длинным текстом 16', now, 100, 'Россия', 'Москва', 1]);
  RxMemoryData1.AppendRecord([17,'Строка с длинным текстом 17', now - 1, 100, 'Россия', 'Ставрополь', 0]);
  RxMemoryData1.AppendRecord([18,'Строка с длинным текстом 18', now - 2, 110, 'Россия', 'Калининград', 3]);
  RxMemoryData1.AppendRecord([19,'Строка с длинным текстом 19', now - 3, 5000, 'Россия', 'Владивасток', 2]);
  RxMemoryData1.AppendRecord([20,'Строка с длинным текстом 20', now - 4, 123.31, 'USA', 'New-York', 1]);
  RxMemoryData1.First;

  CreateFontDirList;
  DebugFonts;
end;

procedure TForm1.CreateFontDirList;
var
  s: String;
begin
  FontDirList := TStringList.Create;
 {$IFDEF WINDOWS}
  s := SHGetFolderPathUTF8(20); // CSIDL_FONTS = 20
  if s <> '' then
    FontDirList.Add(s);
 {$ENDIF}
 {$IFDEF linux}
  FontDirList.Add('/usr/share/cups/fonts/');
  FontDirList.Add('/usr/share/fonts/');
  FontDirList.Add('/usr/local/lib/X11/fonts/');
  FontDirList.Add(GetUserDir + '.fonts/');
 {$ENDIF}
end;

procedure TForm1.InitFonts;
begin
  if FontDirList = nil then
    CreateFontDirList;
end;

procedure TForm1.ShowInfo(AText: string; AParams: array of const);
begin
  Memo1.Lines.Add(Format(AText, AParams));
end;

procedure TForm1.DebugFonts;
var
  i: Integer;
begin
  Memo1.Lines.Clear;
  gTTFontCache.BuildFontFacheIgnoresErrors:=true;
  gTTFontCache.SearchPath.Assign(FontDirList);
  gTTFontCache.BuildFontCache;

  for i:=0 to gTTFontCache.Count-1 do
  begin
    ShowInfo('%s - %s - %s', [gTTFontCache.Items[i].FileName, gTTFontCache.Items[i].FamilyName,  gTTFontCache.Items[i].PostScriptName]);
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  RxDBGridExportPDF1.ShowSetupForm:=false;
  RxDBGridExportPDF1.Execute;
  RxDBGridExportPDF1.ShowSetupForm:=true;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FreeAndNil(FontDirList);
end;

end.

{
'Conakry'
'DejaVu Sans'
'DejaVu Sans Condensed'
'DejaVu Sans Light'
'DejaVu Sans Mono'
'DejaVu Serif'
'DejaVu Serif Condensed'

'Denemo'

'FreeSans'
'Caladea'
'Carlito'
}
