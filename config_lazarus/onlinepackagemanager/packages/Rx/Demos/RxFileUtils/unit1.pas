unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DividerBevel, Forms, Controls, Graphics, Dialogs,
  StdCtrls, EditBtn;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    DividerBevel1: TDividerBevel;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    FileNameEdit1: TFileNameEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation
uses rxFileUtils;

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  UD, UN: string;
begin

  Edit1.Text:=GetEnvironmentVariable('USERNAME');
  Edit2.Text:=GetUserName;

  GetFileOwnerData('', FileNameEdit1.FileName, UN, UD);
  Edit3.Text:=UN;
  Edit4.Text:=UD;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FileNameEdit1.FileName:=ParamStr(0);
end;

end.

