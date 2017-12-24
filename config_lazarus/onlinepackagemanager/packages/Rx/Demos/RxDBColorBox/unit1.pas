unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, DbCtrls,
  StdCtrls, db, rxmemds, rxdbgrid, RxDBColorBox;

type

  { TForm1 }

  TForm1 = class(TForm)
    Datasource1: TDatasource;
    DBNavigator1: TDBNavigator;
    Label1: TLabel;
    Label2: TLabel;
    RxDBColorBox1: TRxDBColorBox;
    RxDBColorBox2: TRxDBColorBox;
    RxDBGrid1: TRxDBGrid;
    RxMemoryData1: TRxMemoryData;
    RxMemoryData1COLOR_INT: TLongintField;
    RxMemoryData1COLOR_STR: TStringField;
    RxMemoryData1FIELD_ID: TAutoIncField;
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
begin
  RxMemoryData1.Open;
  RxMemoryData1.Append;
  RxMemoryData1COLOR_STR.AsString:='clWhite';
  RxMemoryData1COLOR_INT.AsInteger:=Integer(clGreen);
  RxMemoryData1.Post;
  RxMemoryData1.Append;
  RxMemoryData1COLOR_STR.AsString:='clGreen';
  RxMemoryData1COLOR_INT.AsInteger:=Integer(clRed);
  RxMemoryData1.Post;
end;

end.

