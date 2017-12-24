unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, rxtoolbar, RxIniPropStorage, RxAboutDialog, ActnList,
  Menus;


type
  { TMainForm }

  TMainForm = class(TForm)
    actExit: TAction;
    actSysMenu: TAction;
    BitBtn1: TBitBtn;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    PopupMenu2: TPopupMenu;
    RxAboutDialog1: TRxAboutDialog;
    RxIniPropStorage1: TRxIniPropStorage;
    SpeedButton1: TSpeedButton;
    sysAbout: TAction;
    actNew: TAction;
    actNext: TAction;
    actPrior: TAction;
    actCustom: TAction;
    ActionList1: TActionList;
    ImageList1: TImageList;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    PopupMenu1: TPopupMenu;
    ToolPanel1: TToolPanel;
    procedure Action1Execute(Sender: TObject);
    procedure actCustomExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure sysAboutExecute(Sender: TObject);
  private
   //
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;


implementation

uses rxShortCutUnit;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.Action1Execute(Sender: TObject);
begin
  ShowMessage('Hi');
end;

procedure TMainForm.actCustomExecute(Sender: TObject);
begin
  ToolPanel1.Customize(0);
end;


procedure TMainForm.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.BitBtn1Click(Sender: TObject);
var
  A: TShortCut;
begin
  A:=sysAbout.ShortCut;
  if RxSelectShortCut(A) then
    sysAbout.ShortCut:=A;
end;

procedure TMainForm.sysAboutExecute(Sender: TObject);
begin
  RxAboutDialog1.Execute;
end;

end.

