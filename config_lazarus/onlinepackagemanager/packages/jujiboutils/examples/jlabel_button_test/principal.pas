unit principal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, JButton, JLabel;

type

  { TForm1 }

  TForm1 = class(TForm)
    JButton1: TJButton;
    JButton10: TJButton;
    JButton11: TJButton;
    JButton12: TJButton;
    JButton14: TJButton;
    JButton15: TJButton;
    JButton16: TJButton;
    JButton17: TJButton;
    JButton18: TJButton;
    JButton19: TJButton;
    JButton20: TJButton;
    JButton21: TJButton;
    JButton22: TJButton;
    JButton23: TJButton;
    JButton24: TJButton;
    JButton8: TJButton;
    JButton9: TJButton;
    JLabel1: TJLabel;
    JLabel10: TJLabel;
    JLabel11: TJLabel;
    JLabel12: TJLabel;
    JLabel13: TJLabel;
    JLabel14: TJLabel;
    JLabel15: TJLabel;
    JLabel16: TJLabel;
    JLabel17: TJLabel;
    JLabel18: TJLabel;
    JLabel19: TJLabel;
    JLabel2: TJLabel;
    JLabel3: TJLabel;
    JLabel4: TJLabel;
    JLabel5: TJLabel;
    JLabel6: TJLabel;
    JLabel7: TJLabel;
    JLabel8: TJLabel;
    JLabel9: TJLabel;
    PanelNumCentral: TPanel;
    PanelNumDerecho: TPanel;
    PanelNumInferior: TPanel;
    procedure JButton1Click(Sender: TObject);
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

procedure TForm1.JButton1Click(Sender: TObject);
begin
  ShowMessage('Hello World!');
end;

end.

