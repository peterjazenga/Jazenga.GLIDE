unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, BufDataset, FileUtil, Forms, Controls, Graphics,
  Dialogs, StdCtrls, DBCtrls, Buttons, jdblabeledcurrencyedit,
  jdblabeledintegeredit, jdblabeleddateedit;

type

  { TForm1 }

  TForm1 = class(TForm)
    BitBtn1: TBitBtn;
    BufDataset1: TBufDataset;
    Datasource1: TDatasource;
    DBNavigator1: TDBNavigator;
    JDBLabeledCurrencyEdit1: TJDBLabeledCurrencyEdit;
    JDBLabeledDateEdit1: TJDBLabeledDateEdit;
    JDBLabeledIntegerEdit1: TJDBLabeledIntegerEdit;
    JDBLabeledIntegerEdit2: TJDBLabeledIntegerEdit;
    Label1: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  i: integer;
begin
  // Create BufDataset
  with BufDataset1.FieldDefs do
  begin
    Add('ID', ftInteger, 0, False);
    Add('DATE', ftDate, 0, False);
    Add('TOTAL', ftCurrency, 0, False);
    Add('ID2', ftInteger, 0, False);
  end;
  BufDataset1.CreateDataset;
  // populate the memDataset
  for i := 1 to 10 do
  begin
    BufDataset1.Append;
    BufDataset1.FieldByName('ID').AsInteger := i;
    BufDataset1.FieldByName('DATE').AsDateTime := Now;
    BufDataset1.FieldByName('ID2').AsInteger := i * i;
    BufDataset1.FieldByName('TOTAL').AsCurrency := i * i * i;
    BufDataset1.Post;
  end;
  BufDataset1.First;
end;

end.

