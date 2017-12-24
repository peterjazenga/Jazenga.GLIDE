unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, ScrollingText, Forms, Controls, Graphics,
  Dialogs, Buttons, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    BitBtn1: TBitBtn;
    Button1: TButton;
    ScrollingText1: TScrollingText;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

resourcestring
  scrolltext='Hello';
{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  ScrollingText1.Active:=NOT ScrollingText1.Active;
end;

end.

