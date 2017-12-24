unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Spin, ChemText;

type

  { TForm1 }

  TForm1 = class(TForm)
    CbDefaultFontSize: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    RadioGroup3: TRadioGroup;
    RadioGroup4: TRadioGroup;
    SpinEdit1: TSpinEdit;
    procedure CbDefaultFontSizeChange(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure CheckBox3Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure RadioGroup3Click(Sender: TObject);
    procedure RadioGroup4Click(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
  private
    ChemLabel: TChemLabel;

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.CbDefaultFontSizeChange(Sender: TObject);
begin
  if CbDefaultFontSize.Checked then
    Chemlabel.Font.Size := 0
  else
    ChemLabel.Font.Size := SpinEdit1.Value;
  SpinEdit1.Visible := not CbDefaultFontSize.Checked;
end;

procedure TForm1.CheckBox2Change(Sender: TObject);
begin
  ChemLabel.AutoSize := Checkbox2.Checked;
  if not ChemLabel.AutoSize then begin
    ChemLabel.Width := Width - 2*ChemLabel.Left;
    ChemLabel.Height := Radiogroup1.Top - 8 - 8;
  end;
end;

procedure TForm1.CheckBox3Change(Sender: TObject);
begin
  ChemLabel.Enabled := Checkbox3.Checked;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ChemLabel := TChemLabel.Create(self);
  ChemLabel.Parent := self;
  ChemLabel.Left := 8;
  ChemLabel.Top := 8;
  ChemLabel.AutoSize := false;
  Chemlabel.Height := 50;
  ChemLabel.Width := ClientWidth - 2*Chemlabel.Left;
//  ChemLabel.Anchors := [akLeft, akRight, akTop];
  ChemLabel.Color := clWindow;

  RadioGroup1Click(nil);
  RadioGroup2Click(nil);
end;

procedure TForm1.RadioGroup1Click(Sender: TObject);
begin
  Chemlabel.Caption := RadioGroup1.Items[Radiogroup1.ItemIndex];
end;

procedure TForm1.RadioGroup2Click(Sender: TObject);
begin
  ChemLabel.Arrow := TChemArrow(RadioGroup2.ItemIndex);
end;

procedure TForm1.RadioGroup3Click(Sender: TObject);
begin
  case Radiogroup3.ItemIndex of
    0: ChemLabel.Alignment := taLeftJustify;
    1: ChemLabel.Alignment := taCenter;
    2: Chemlabel.Alignment := taRightJustify;
  end;
end;

procedure TForm1.RadioGroup4Click(Sender: TObject);
begin
  case Radiogroup4.ItemIndex of
    0: ChemLabel.Layout := tlTop;
    1: Chemlabel.Layout := tlCenter;
    2: Chemlabel.Layout := tlBottom;
  end;
end;

procedure TForm1.SpinEdit1Change(Sender: TObject);
begin
  if not CbDefaultFontSize.Checked then
    Chemlabel.Font.Size := SpinEdit1.Value;
  ChemLabel.Invalidate;
end;

end.

