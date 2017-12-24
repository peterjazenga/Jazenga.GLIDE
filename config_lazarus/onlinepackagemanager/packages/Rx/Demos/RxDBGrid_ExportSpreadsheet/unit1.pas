unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, rxdbgrid, rxmemds, RxDBGridExportSpreadSheet,
  Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, EditBtn, db, Types, Grids;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    DataSource1: TDataSource;
    FileNameEdit1: TFileNameEdit;
    Label1: TLabel;
    Panel1: TPanel;
    RxDBGrid1: TRxDBGrid;
    RxDBGridExportSpreadSheet1: TRxDBGridExportSpreadSheet;
    RxMemoryData1: TRxMemoryData;
    RxMemoryData1CODE: TLongintField;
    RxMemoryData1NAME: TStringField;
    RxMemoryData1PRICE: TCurrencyField;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RxDBGrid1DataHintShow(Sender: TObject; CursorPos: TPoint;
      Cell: TGridCoord; Column: TRxColumn; var HintStr: string;
      var Processed: boolean);
    procedure RxDBGridExportSpreadSheet1BeforeExecute(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

uses LCLIntf;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  i:integer;
begin
  RxMemoryData1.Open;
  //fill test values
  for i:=1 to 50 do
  begin
    RxMemoryData1.Append;
    RxMemoryData1CODE.AsInteger:=i;
    RxMemoryData1NAME.AsString:=Format('Line %d', [i]);
    RxMemoryData1PRICE.AsFloat:=Random * 100 + 5;
    RxMemoryData1.Post;
  end;
  RxMemoryData1.First;

  FileNameEdit1.Text:='test1.ods';
end;

procedure TForm1.RxDBGrid1DataHintShow(Sender: TObject; CursorPos: TPoint;
  Cell: TGridCoord; Column: TRxColumn; var HintStr: string;
  var Processed: boolean);
begin
  HintStr:='Это очень чётная строка! ' + HintStr;
  Processed:=true;
end;

procedure TForm1.RxDBGridExportSpreadSheet1BeforeExecute(Sender: TObject);
begin
  RxDBGridExportSpreadSheet1.FileName:=FileNameEdit1.Text;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  RxDBGridExportSpreadSheet1.Execute;
end;

end.

