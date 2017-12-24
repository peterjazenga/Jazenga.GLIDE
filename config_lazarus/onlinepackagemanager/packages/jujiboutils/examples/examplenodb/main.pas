unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, JLabeledCurrencyEdit, JLabeledDateEdit, JLabeledFloatEdit,
  JLabeledIntegerEdit;

type

  { TForm1 }

  TForm1 = class(TForm)
    BitBtn1: TBitBtn;
    GroupBox1: TGroupBox;
    JLabeledCurrencyEdit1: TJLabeledCurrencyEdit;
    JLabeledDateEdit1: TJLabeledDateEdit;
    JLabeledFloatEdit1: TJLabeledFloatEdit;
    JLabeledIntegerEdit1: TJLabeledIntegerEdit;
    JLabeledIntegerEdit2: TJLabeledIntegerEdit;
    Label1: TLabel;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

end.

