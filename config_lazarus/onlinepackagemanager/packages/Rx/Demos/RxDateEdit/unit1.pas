unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, FileUtil, LResources, Forms, Controls, Graphics,
  Dialogs, StdCtrls, rxmemds, rxdbdateedit, rxcurredit, rxtooledit;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    CurrencyEdit1: TCurrencyEdit;
    dsData: TDatasource;
    Label1: TLabel;
    Label2: TLabel;
    rxData: TRxMemoryData;
    rxDataDOC_DATE: TDateField;
    RxDateEdit1: TRxDateEdit;
    RxDBDateEdit1: TRxDBDateEdit;
    procedure Button1Click(Sender: TObject);
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
  rxData.Open;
  rxData.Append;
  rxDataDOC_DATE.AsDateTime:=Now;
  CurrencyEdit1.Value:=1214.55
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  CurrencyEdit1.Invalidate;
end;

end.

