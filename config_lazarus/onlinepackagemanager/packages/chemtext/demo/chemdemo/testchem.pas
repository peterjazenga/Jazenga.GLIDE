unit testchem;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, chemtext;

type

  { TForm1 }

  TForm1 = class(TForm)
    Label1: TChemLabel;
    Label2: TChemLabel;
    Label3: TChemLabel;
    Label4: TLabel;
    Spacer: TBevel;
    ChemLabel1: TChemLabel;
    ChemLabel2: TChemLabel;
    ChemLabel3: TChemLabel;
    ChemLabel4: TChemLabel;
    ChemLabel5: TChemLabel;
    Edit1: TEdit;
    Panel1: TPanel;
    procedure Edit1Change(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.Edit1Change(Sender: TObject);
begin
  ChemLabel4.Caption := Edit1.Text;
end;

end.
