unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ActnList,
  Menus, rxFolderLister, Buttons, EditBtn, StdCtrls;

type

  { TMainForm }

  TMainForm = class(TForm)
    Edit1: TEdit;
    FolderLister1: TFolderLister;
    Label2: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    sysScan: TAction;
    Button1: TButton;
    DirectoryEdit1: TDirectoryEdit;
    hlpAbout: TAction;
    Label1: TLabel;
    sysExit: TAction;
    ActionList1: TActionList;
    MainMenu1: TMainMenu;
    SystemItems: TMenuItem;
    HelpItems: TMenuItem;
    FolderItems: TMenuItem;
    procedure FolderLister1ExecuteItem(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure hlpAboutExecute(Sender: TObject);
    procedure sysExitExecute(Sender: TObject);
    procedure sysRescanExecute(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;

implementation
uses AboutUnit;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.hlpAboutExecute(Sender: TObject);
begin
  AboutForm:=TAboutForm.Create(Application);
  try
    AboutForm.ShowModal;
  finally
    AboutForm.Free;
  end;
end;

procedure TMainForm.sysExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.FolderLister1ExecuteItem(Sender: TObject);
var
  S:string;
begin
  S:='Selected: '+FolderLister1.Files[(Sender as TComponent).Tag];
  MessageDlg('Result', S, mtInformation, [mbOK], 0);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  DirectoryEdit1.Directory:=ExtractFileDir(ParamStr(0));
end;

procedure TMainForm.sysRescanExecute(Sender: TObject);
begin
  FolderLister1.FileFolder:=DirectoryEdit1.Text;
  FolderLister1.DefaultExt:=Edit1.Text;
  FolderLister1.Execute;
end;

end.

