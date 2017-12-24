unit MainFrm;

{
----------------------------------------------------------------
Test program for TFileMenuHandler v1.0 by Harry Kakoulidis 11/1999
Ported to Lazarus by Juha Manninen 11/2009
  <juha dot manninen (at) phnet dot fi>
----------------------------------------------------------------
}
{$MODE DELPHI}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Menus, StdCtrls,
  ComCtrls, LResources, Interfaces, FileMenuHdl;

type
  TMainForm = class(TForm)
    Label1: TLabel;
    Memo1: TMemo;
    CheckBox1: TCheckBox;
    UpDown1: TUpDown;
    Edit1: TEdit;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    MainMenu1: TMainMenu;
    FileMenu: TMenuItem;
    NewItem: TMenuItem;
    OpenItem: TMenuItem;
    SaveItem: TMenuItem;
    SaveAsItem: TMenuItem;
    N3: TMenuItem;
    ExitItem: TMenuItem;
    FMH: TFileMenuHandler;
    Label2: TLabel;
    Label3: TLabel;
    procedure NewItemClick(Sender: TObject);
    procedure OpenItemClick(Sender: TObject);
    procedure SaveItemClick(Sender: TObject);
    procedure SaveAsItemClick(Sender: TObject);
    procedure ExitItemClick(Sender: TObject);
    function FMHNew(const Filename: String): Boolean;
    procedure FMHNewFormCaption(const FileName: String);
    function FMHOpen(const Filename: String): Boolean;
    function FMHSave(const Filename: String): Boolean;
    procedure Memo1Change(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation


{
---------------------------------------------------------------
STEP 1 : Write handlers of your mainmenu-actionlist etc
         for the commands Open,New,Save,SaveAs.
         The only thing needed in the handler is a single
         call to the corresponding TFileMenuHandler procedure
---------------------------------------------------------------
}
procedure TMainForm.NewItemClick(Sender: TObject);
begin  fmh.new;  end;

procedure TMainForm.OpenItemClick(Sender: TObject);
begin  fmh.open;  end;

procedure TMainForm.SaveItemClick(Sender: TObject);
begin  fmh.save;  end;

procedure TMainForm.SaveAsItemClick(Sender: TObject);
begin  fmh.saveas;  end;

procedure TMainForm.ExitItemClick(Sender: TObject);
begin
  Close; // This is the normal close or application.terminate
end;

{
---------------------------------------------------------------
STEP 2 : You have to tell TFileMenuHandler that the file has
         changed. Do this by setting IsDirty to true
---------------------------------------------------------------
}

procedure TMainForm.Memo1Change(Sender: TObject);
begin  Fmh.IsDirty := true; end;

{
---------------------------------------------------------------
STEP 3 : Here come TFileMenuHandler' s Event handlers, that
         actually save,load and create new files.

   OnNew(Filename : string)  : Code needed for a new file
   OnSave(Filename : string) : Code to save your file
   OnOpen(Filename : string) : Code to open your file

         For all of these, all checks are made by
         TFileMenuHandler. They are called only when needed.
         Optionally return false if they where not succesfull

   Common error : Do not forget to return something.
   (You will get a compiler warning if not)
---------------------------------------------------------------
}

function TMainForm.FMHOpen(const Filename: String): Boolean;
begin
  try
    memo1.Lines.LoadFromFile(filename);
    result := true;
  except
    on EFOpenError do result := false;
  end;
end;

function TMainForm.FMHSave(const Filename: String): Boolean;
begin
  try
    memo1.Lines.savetoFile(filename);
    result := true;
  except
    on EFCreateError do result := false;
  end;
end;

function TMainForm.FMHNew(const Filename: String): Boolean;
begin
  result := true;
  memo1.Lines.Clear;
end;

{
---------------------------------------------------------------
STEP 4 : Optionally, write code that changes the form caption
         Not done automatically so you can support other things
         like for example RxGradientCaption
---------------------------------------------------------------
}

procedure TMainForm.FMHNewFormCaption(const FileName: String);
var s:string;
begin
  s:=Extractfilename(filename);
  mainform.caption := 'TFileMenuHandler - ' +
    Copy(s, 1, pos('.', s) - 1);
//  Filename without path & extension
end;

{
---------------------------------------------------------------
OTHER : Some properties may be set at runtime
---------------------------------------------------------------
}

procedure TMainForm.CheckBox1Click(Sender: TObject);
begin
  fmh.EnableMenuList := Checkbox1.Checked;
end;

procedure TMainForm.Edit1Change(Sender: TObject);
begin
  fmh.MaxList := StrToInt(Edit1.text);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
 label2.Caption:='IniPath: '+FMh.IniPath;
 // IniPath tells us the INI Filepath.
 // Because we didn't set it here (OnShow handler),
 // it points to the EXE dir.
end;

initialization
  {$i MainFrm.lrs}

end.
