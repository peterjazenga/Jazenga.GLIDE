unit pbEditDataUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, RxCloseFormValidator, Forms, Controls, Graphics,
  Dialogs, StdCtrls, DbCtrls, ButtonPanel;

type

  { TpbEditDataForm }

  TpbEditDataForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    DBEdit1: TDBEdit;
    DBEdit2: TDBEdit;
    DBEdit3: TDBEdit;
    DBEdit4: TDBEdit;
    DBEdit5: TDBEdit;
    DBMemo1: TDBMemo;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    RxCloseFormValidator1: TRxCloseFormValidator;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  pbEditDataForm: TpbEditDataForm;

implementation

{$R *.lfm}

end.

