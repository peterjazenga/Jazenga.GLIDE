unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ButtonPanel, ExtCtrls, DBGrids, JvHtControls, JvHint, JvDBHTLabel,
  memds, db;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ButtonPanel1: TButtonPanel;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    JvDBHTLabel1: TJvDBHTLabel;
    JvHTComboBox1: TJvHTComboBox;
    JvHTLabel1: TJvHTLabel;
    JvHTListBox1: TJvHTListBox;
    MemDataset1: TMemDataset;
    Memo1: TMemo;
    Memo2: TMemo;
    Memo3: TMemo;
    PageControl1: TPageControl;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure JvDBHTLabel1HyperLinkClick(Sender: TObject; LinkName: string);
    procedure JvHTLabel1HyperLinkClick(Sender: TObject; LinkName: string);
    procedure JvHTListBox1HyperLinkClick(Sender: TObject; LinkName: string);
    procedure Memo1Change(Sender: TObject);
    procedure Memo3Change(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Memo1Change(Sender: TObject);
begin
  JvHTLabel1.Caption := Memo1.Text;
  JvHTLabel1.Hint := Memo1.Text;
end;

procedure TForm1.Memo3Change(Sender: TObject);
begin
  JvDBHTLabel1.Mask := Memo3.Text;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  RegisterHtHints;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  MemDataset1.AppendRecord([1, 'asdfxc', 'wertfx']);
  MemDataset1.AppendRecord([2, 'brdrgrsdgx', 'sdfwetrcx']);
  MemDataset1.AppendRecord([3, 'bhtesdxcv', 'wytsfsv']);
  MemDataset1.AppendRecord([4, 'sdgrdthc', 'klvbsdfwe85']);
  MemDataset1.AppendRecord([5, 'trcbxg', 'her4fekg']);
end;

procedure TForm1.JvDBHTLabel1HyperLinkClick(Sender: TObject; LinkName: string);
begin
  MessageDlg('TJvDBHTLabel', 'Hyperlink: ' + LinkName, mtInformation, [mbOK], 0);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  JvHTListBox1.Items.Add(Memo2.Text);
  JvHTComboBox1.Items.Add(Memo2.Text);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  JvHTComboBox1.Items.Clear;
  JvHTListBox1.Items.Clear;
end;

procedure TForm1.JvHTLabel1HyperLinkClick(Sender: TObject; LinkName: string);
begin
  MessageDlg('TJvHTLabel', 'Hyperlink: ' + LinkName, mtInformation, [mbOK], 0);
end;

procedure TForm1.JvHTListBox1HyperLinkClick(Sender: TObject; LinkName: string);
begin
  MessageDlg('JvHTListBox', 'Hyperlink: ' + LinkName, mtInformation, [mbOK], 0);
end;

end.

