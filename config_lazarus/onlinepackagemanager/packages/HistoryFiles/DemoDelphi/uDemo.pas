unit uDemo;
{Works from D4 to D2009}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  HistoryFiles, Menus, StdCtrls;

type
  TfDemo = class(TForm)
    MainMenu: TMainMenu;
    Memo: TMemo;
    GroupBox: TGroupBox;
    chkShowFullPath: TCheckBox;
    chkShowNumber: TCheckBox;
    chkSorted: TCheckBox;
    mnuFile: TMenuItem;
    mnuExit: TMenuItem;
    HistoryFiles: THistoryFiles;
    OpenDialog: TOpenDialog;
    mnuOpen: TMenuItem;
    EditDeleteItem: TEdit;
    btnDeleteItem: TButton;
    lblInfo: TLabel;
    chkCheckLastItem: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure chkShowFullPathClick(Sender: TObject);
    procedure chkShowNumberClick(Sender: TObject);
    procedure chkSortedClick(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure mnuOpenClick(Sender: TObject);
    procedure HistoryFilesClickHistoryItem(Sender: TObject;
      Item: TMenuItem; const Filename: String);
    procedure HistoryFilesCreateItem(Sender: TObject; Item: TMenuItem;
      const Filename: String);
    procedure btnDeleteItemClick(Sender: TObject);
    procedure chkCheckLastItemClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fDemo: TfDemo;

implementation

{$R *.DFM}

procedure TfDemo.FormCreate(Sender: TObject);
var
 sPath : string;
 bBitmap : TBitmap;
begin

  //Adds the bitmaps
  bBitmap := TBitmap.Create;
  bBitmap.LoadFromFile('new.bmp');
  bBitmap.Transparent := true;

  HistoryFiles.ItemBitmap := bBitmap;

  bBitmap := TBitmap.Create;
  bBitmap.LoadFromFile('open.bmp');
  bBitmap.Transparent := true;

  HistoryFiles.ItemSelectedBitmap := bBitmap;

  // Update the check boxes
  chkShowFullPath.Checked := HistoryFiles.ShowFullPath;
  chkShowNumber.Checked := HistoryFiles.ShowNumber;
  chkSorted.Checked := HistoryFiles.Sorted;
  chkCheckLastItem.Checked := HistoryFiles.CheckLastItem;

  // Store local path
  sPath := ExtractFilePath(Application.ExeName);
  HistoryFiles.LocalPath := sPath;

  // Define the ini filename and where it is.
  HistoryFiles.IniFile := sPath + 'Demo.ini';

  // Reset the LastItemIndex
  //HistoryFiles.ClearLastItem;
  
  // Add the history on the parent menu
  HistoryFiles.UpdateParentMenu;
  lblInfo.Caption := '(0..' + inttostr(HistoryFiles.Count-1) + ')';

  // Load the file
  with HistoryFiles do
    if LastItemIndex>=0 then
      Memo.Lines.LoadFromFile(GetItemValue(LastItemIndex));
end;

procedure TfDemo.chkShowFullPathClick(Sender: TObject);
begin
  HistoryFiles.ShowFullPath := chkShowFullPath.Checked;
  // Refresh the menu
  if fDemo.Visible then
    HistoryFiles.UpdateParentMenu;
end;

procedure TfDemo.chkShowNumberClick(Sender: TObject);
begin
  HistoryFiles.ShowNumber := chkShowNumber.Checked;
  // Refresh the menu
  if fDemo.Visible then
    HistoryFiles.UpdateParentMenu;
end;

procedure TfDemo.chkSortedClick(Sender: TObject);
begin
  HistoryFiles.Sorted := chkSorted.Checked;
  // Refresh the menu
  if fDemo.Visible then
    HistoryFiles.UpdateParentMenu;
end;

procedure TfDemo.chkCheckLastItemClick(Sender: TObject);
begin
  HistoryFiles.CheckLastItem := chkCheckLastItem.Checked;
  // Refresh the menu
  if fDemo.Visible then
    HistoryFiles.UpdateParentMenu;
end;

procedure TfDemo.mnuExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfDemo.mnuOpenClick(Sender: TObject);
begin
  with OpenDialog do
  begin
    Execute;
    if FileName <> '' then
    begin
      Memo.Lines.LoadFromFile(FileName);

      // Add the filename into the history and refresh the menu
      HistoryFiles.UpdateList(FileName);

      lblInfo.Caption := '(0..' + inttostr(HistoryFiles.Count-1) + ')';
    end;
  end;
end;

procedure TfDemo.HistoryFilesClickHistoryItem(Sender: TObject;
  Item: TMenuItem; const Filename: String);
begin
  // When an Item is clicked the file is loaded on the Memo
  Memo.Lines.LoadFromFile(FileName);
end;

procedure TfDemo.HistoryFilesCreateItem(Sender: TObject; Item: TMenuItem;
  const Filename: String);
begin
    // When a new Item is created I can
    // change the properties like I want
end;

procedure TfDemo.btnDeleteItemClick(Sender: TObject);
begin
  HistoryFiles.DeleteItem(strtoint(EditDeleteItem.Text));
  lblInfo.Caption := '(0..' + inttostr(HistoryFiles.Count-1) + ')';
  EditDeleteItem.Text := '';
end;

end.
