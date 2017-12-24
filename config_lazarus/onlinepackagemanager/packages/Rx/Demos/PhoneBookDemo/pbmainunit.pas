unit pbMainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, rxtoolbar,
  rxdbgrid, rxmemds, RxAboutDialog, ComCtrls, ActnList, Menus, db;

type

  { TpbMainForm }

  TpbMainForm = class(TForm)
    hlpAbout: TAction;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    RxAboutDialog1: TRxAboutDialog;
    sysExit: TAction;
    edtFind: TAction;
    edtPrint: TAction;
    edtDelete: TAction;
    edtEdit: TAction;
    edtNew: TAction;
    ActionList1: TActionList;
    Datasource1: TDatasource;
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    PopupMenu1: TPopupMenu;
    RxDBGrid1: TRxDBGrid;
    rxPhoneBook: TRxMemoryData;
    rxPhoneBookICQ: TStringField;
    rxPhoneBookID: TAutoIncField;
    rxPhoneBookMEMO: TMemoField;
    rxPhoneBookNAME: TStringField;
    rxPhoneBookPATRONYMIC: TStringField;
    rxPhoneBookPHONE: TStringField;
    rxPhoneBookSURNAME: TStringField;
    StatusBar1: TStatusBar;
    ToolPanel1: TToolPanel;
    procedure edtDeleteExecute(Sender: TObject);
    procedure edtNewExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure hlpAboutExecute(Sender: TObject);
    procedure RxDBGrid1DblClick(Sender: TObject);
    procedure sysExitExecute(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  pbMainForm: TpbMainForm;

implementation
uses FileUtil, pbEditDataUnit, LCLType;

{$R *.lfm}

{ TpbMainForm }

procedure TpbMainForm.FormCreate(Sender: TObject);
begin
  rxPhoneBook.Open;
end;

procedure TpbMainForm.edtNewExecute(Sender: TObject);
begin
  if (Sender as TComponent).Tag = 1 then
    rxPhoneBook.Append
  else
    rxPhoneBook.Edit;
  pbEditDataForm:=TpbEditDataForm.Create(Application);
  if pbEditDataForm.ShowModal = mrOk then
    rxPhoneBook.Post
  else
    rxPhoneBook.Cancel;
  pbEditDataForm.Free;
end;

procedure TpbMainForm.edtDeleteExecute(Sender: TObject);
begin
  if Application.MessageBox('Delete record', 'Delete this record?', MB_YESNO + MB_ICONQUESTION) = ID_YES then
    rxPhoneBook.Delete;
end;

procedure TpbMainForm.hlpAboutExecute(Sender: TObject);
begin
  RxAboutDialog1.Execute;
end;

procedure TpbMainForm.RxDBGrid1DblClick(Sender: TObject);
begin
  edtEdit.Execute;
end;

procedure TpbMainForm.sysExitExecute(Sender: TObject);
begin
  Close;
end;

end.

