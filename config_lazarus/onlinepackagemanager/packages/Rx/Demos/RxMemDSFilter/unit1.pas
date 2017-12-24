unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, rxdbgrid, rxmemds, RxIniPropStorage, Forms,
  Controls, Graphics, Dialogs, StdCtrls, db;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    CheckBox1: TCheckBox;
    DataSource1: TDataSource;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Memo1: TMemo;
    RxDBGrid1: TRxDBGrid;
    RxIniPropStorage1: TRxIniPropStorage;
    RxMemoryData1: TRxMemoryData;
    RxMemoryData1CODE: TLongintField;
    RxMemoryData1ID: TLongintField;
    RxMemoryData1NAME: TStringField;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure RxDBGridInfoBox(Sender: TObject);
  public

  end;

var
  Form1: TForm1;

implementation
uses DBGrids;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  RxDBGrid1.Options:=RxDBGrid1.Options + [dgAlwaysShowEditor];
  RxMemoryData1.Open;
  for i:=1 to 20 do
  begin
    RxMemoryData1.Append;
    RxMemoryData1ID.AsInteger:=i;
    RxMemoryData1NAME.AsString:='Line '+IntToStr(I);
    if i mod 4 = 0 then
      RxMemoryData1CODE.Clear
    else
      RxMemoryData1CODE.AsInteger:=100 + i * 10;
    RxMemoryData1.Post;

  end;
  RxDBGrid1.CreateToolMenuItem(#0, '-', nil);
  RxDBGrid1.CreateToolMenuItem(#0, 'Show info box', @RxDBGridInfoBox);
//  RxDBGrid1.DefaultRowHeight:=;
end;

procedure TForm1.RxDBGridInfoBox(Sender: TObject);
begin
  ShowMessage('Hello world form RXDBGrid tools menu');
end;

procedure TForm1.CheckBox1Change(Sender: TObject);
begin
  Edit1.Enabled:=not CheckBox1.Checked;

  try
    if CheckBox1.Checked then
      RxMemoryData1.Filter:=Edit1.Text;
    RxMemoryData1.Filtered:=CheckBox1.Checked;
  except
    on E:Exception do
      Memo1.Lines.Text:=E.Message;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  RxDBGrid1.DefaultRowHeight:=StrToInt(Edit2.Text);
end;

type
  THackGrid = class(TRxDBGrid);

procedure TForm1.Button1Click(Sender: TObject);
begin
  RxDBGrid1.DefaultRowHeight:=THackGrid(RxDBGrid1).GetDefaultRowHeight;
end;

end.

