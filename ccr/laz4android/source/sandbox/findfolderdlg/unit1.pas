unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFrmMain }

  TFrmMain = class(TForm)
    Button1: TButton;
    edtSelectedFolder: TEdit;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.lfm}

{ TFrmMain }

uses
  FindFolderFrm;

procedure TFrmMain.Button1Click(Sender: TObject);
var
  _selectedFolder:string;
begin
  _selectedFolder:=ShowFindFolderDlg('c:\','sdk','Please select sdk-folder');
  edtSelectedFolder.Text:=_selectedFolder;
end;

end.

