unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls, rxdbgrid, rxvclutils, rxmemds, db, IniPropStorage;

type

  { TForm1 }

  TForm1 = class(TForm)
    Datasource1: TDatasource;
    Edit1: TEdit;
    IniPropStorage1: TIniPropStorage;
    PaintBox1: TPaintBox;
    RadioGroup1: TRadioGroup;
    RxDBGrid1: TRxDBGrid;
    RxMemoryData1: TRxMemoryData;
    RxMemoryData1Demo21: TStringField;
    RxMemoryData1DEMO_11: TLongintField;
    procedure CheckBox1Change(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
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

procedure TForm1.PaintBox1Paint(Sender: TObject);
var
  FOri:TTextOrientation;
begin

  PaintBox1.Canvas.TextOut(1,1, 'Text for test');
  case RadioGroup1.ItemIndex of
    0:FOri:=toHorizontal;
    1:FOri:=toVertical90;
    2:FOri:=toHorizontal180;
    3:FOri:=toVertical270;
    4:FOri:=toHorizontal360;
  end;

  OutTextXY90(PaintBox1.Canvas, 1, 20, Edit1.Text, FOri);
end;

procedure TForm1.CheckBox1Change(Sender: TObject);
var
  FOri:TTextOrientation;
begin
  PaintBox1.Invalidate;
  case RadioGroup1.ItemIndex of
    0:FOri:=toHorizontal;
    1:FOri:=toVertical90;
    2:FOri:=toHorizontal180;
    3:FOri:=toVertical270;
    4:FOri:=toHorizontal360;
  end;
  (RxDBGrid1.Columns[0].Title as TRxColumnTitle).Orientation:=FOri;
end;

procedure TForm1.Edit1Change(Sender: TObject);
begin
  PaintBox1.Invalidate;
  (RxDBGrid1.Columns[0].Title as TRxColumnTitle).Caption:=Edit1.Text;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  RxMemoryData1.Open;
end;

end.

