unit ueditoptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ActnList,
  ExtCtrls, StdCtrls;

type

  { TfEditOptions }

  TfEditOptions = class(TForm)
    actOK: TAction;
    AL: TActionList;
    Button1: TButton;
    Button2: TButton;
    edtCaseSensitive: TCheckBox;
    Panel1: TPanel;
    Panel2: TPanel;
    edtStringType: TRadioGroup;
    procedure actOKExecute(Sender: TObject);
    procedure actOKUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  fEditOptions: TfEditOptions;

implementation
uses
  pascal_parser_intf, udm;

{$R *.lfm}

{ TfEditOptions }

procedure TfEditOptions.actOKExecute(Sender: TObject);
var
  i : Integer;
begin
  DM.CaseSensitive := edtCaseSensitive.Checked;
  i := edtStringType.ItemIndex;
  if (i >= Ord(Low(TXSDStringMaping))) and (i <= Ord(High(TXSDStringMaping))) then
    DM.XsdStringMaping := TXSDStringMaping(i);
  ModalResult := mrOk;
end;

procedure TfEditOptions.actOKUpdate(Sender: TObject);
var
  i : Integer;
begin
  i := edtStringType.ItemIndex;
  TCustomAction(Sender).Enabled :=
    (i >= Ord(Low(TXSDStringMaping))) and (i <= Ord(High(TXSDStringMaping)));
end;

procedure TfEditOptions.FormCreate(Sender: TObject);
begin
  edtCaseSensitive.Checked := DM.CaseSensitive;
  edtStringType.ItemIndex := Ord(DM.XsdStringMaping);
end;

end.

