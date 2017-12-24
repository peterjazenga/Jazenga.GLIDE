unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, BufDataset, FileUtil, Forms, Controls, Graphics,
  Dialogs, DbCtrls, DBGrids, StdCtrls, JDBGridControl;

type

  { TForm1 }

  TForm1 = class(TForm)
    BufDataset1: TBufDataset;
    Datasource1: TDatasource;
    DBNavigator1: TDBNavigator;
    JDBGridControl1: TJDBGridControl;
    Label1: TLabel;
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
    Add('QUANTITY', ftCurrency, 0, False);
  end;
  BufDataset1.CreateDataset;
  // populate
  for i := 1 to 10 do
  begin
    BufDataset1.Append;
    BufDataset1.FieldByName('ID').AsInteger := i;
    BufDataset1.FieldByName('DATE').AsDateTime := Now;
    BufDataset1.FieldByName('QUANTITY').AsFloat := i * i * i;
    BufDataset1.Post;
  end;
  BufDataset1.First;
end;

end.

