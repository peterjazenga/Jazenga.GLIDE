unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, RxIniPropStorage, Forms, Controls, Graphics,
  Dialogs, StdCtrls, Spin;

type

  { TForm1 }

  TForm1 = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    RxIniPropStorage1: TRxIniPropStorage;
    SpinEdit1: TSpinEdit;
    procedure RxIniPropStorage1RestoreProperties(Sender: TObject);
    procedure RxIniPropStorage1SavingProperties(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.RxIniPropStorage1RestoreProperties(Sender: TObject);
begin
  Edit3.Text:=RxIniPropStorage1.StoredValue['STRING_VALUE1'];

  if RxIniPropStorage1.StoredValue['INT_VALUE2'] <> '' then
    SpinEdit1.Value:=StrToIntDef(RxIniPropStorage1.StoredValue['INT_VALUE2'], 0);
end;

procedure TForm1.RxIniPropStorage1SavingProperties(Sender: TObject);
begin
  RxIniPropStorage1.StoredValue['STRING_VALUE1']:=Edit3.Text;
  RxIniPropStorage1.StoredValue['INT_VALUE2']:=IntToStr(SpinEdit1.Value);
end;

end.

