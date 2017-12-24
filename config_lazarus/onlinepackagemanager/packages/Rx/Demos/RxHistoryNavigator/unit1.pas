unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, RxHistoryNavigator, rxtoolbar, Forms, Controls,
  Graphics, Dialogs, ActnList, StdCtrls, Buttons, ComCtrls, Menus;

type

  { TForm1 }

  TForm1 = class(TForm)
    actExit: TAction;
    actFind: TAction;
    actBack: TAction;
    ActionList1: TActionList;
    actForward: TAction;
    ApplicationProperties1: TApplicationProperties;
    BitBtn1: TBitBtn;
    Edit1: TEdit;
    ImageList1: TImageList;
    Label1: TLabel;
    Memo1: TMemo;
    PopupMenu1: TPopupMenu;
    RxHistoryNavigator1: TRxHistoryNavigator;
    StatusBar1: TStatusBar;
    ToolPanel1: TToolPanel;
    procedure actExitExecute(Sender: TObject);
    procedure actFindExecute(Sender: TObject);
    procedure ApplicationProperties1Hint(Sender: TObject);
    procedure RxHistoryNavigator1HistoryNavigate(Sender: TRxHistoryNavigator;
      AInfo: string; AProcessed: boolean);
  private
    procedure DoFind(S:string);
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TForm1.actFindExecute(Sender: TObject);
begin
  RxHistoryNavigator1.AddToHistory('Find text', Edit1.Text);
  DoFind(Edit1.Text);
  Edit1.Text:='';
  Edit1.SetFocus;
end;

procedure TForm1.ApplicationProperties1Hint(Sender: TObject);
begin
  StatusBar1.SimpleText:=Application.Hint;
end;

procedure TForm1.RxHistoryNavigator1HistoryNavigate(
  Sender: TRxHistoryNavigator; AInfo: string; AProcessed: boolean);
begin
  Memo1.Lines.Add('History Find text ' + AInfo);
end;

procedure TForm1.DoFind(S: string);
begin
  //Что-то ищем - имитация бурной деятельности
  Memo1.Lines.Add('Find text ' + S);
end;

end.

