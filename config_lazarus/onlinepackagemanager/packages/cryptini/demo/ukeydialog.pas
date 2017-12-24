unit ukeydialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, StdCtrls,
  Buttons;

type

  { Tkeydialog }

  Tkeydialog = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    edt_key: TEdit;
    lbl_info: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public
     Var sKeyPhrase:String;
  end;

var
  keydialog: Tkeydialog;

implementation

{$R *.lfm}

{ Tkeydialog }

procedure Tkeydialog.FormCreate(Sender: TObject);
begin
  Caption := Application.Title + ' Key Chooser';
  Icon := Application.Icon;
  sKeyPhrase:='Type a phrase or number';
  edt_key.Text:=sKeyPhrase;
end;

procedure Tkeydialog.FormShow(Sender: TObject);
begin
  edt_key.Text:=sKeyPhrase;
end;

end.

