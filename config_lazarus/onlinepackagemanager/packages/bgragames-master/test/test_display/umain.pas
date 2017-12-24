unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  bgTools;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    ComboBox1: TComboBox;
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
  GetDisplaySettings(TStringList(ComboBox1.Items));
  ComboBox1.ItemIndex := ComboBox1.Items.Count - 1;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  SetDisplaySettings(ComboBox1.ItemIndex);
end;

end.
