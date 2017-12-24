unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls, Grids, ComCtrls, rxtoolbar, RxViewsPanel, ExtendedNotebook;

type

  { TForm1 }

  TForm1 = class(TForm)
    ExtendedNotebook1: TExtendedNotebook;
    ImageList2: TImageList;
    Memo1: TMemo;
    Page1: TPage;
    Page2: TPage;
    Page3: TPage;
    Page4: TPage;
    PaintBox1: TPaintBox;
    RxViewsPanel1: TRxViewsPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    StatusBar1: TStatusBar;
    StringGrid1: TStringGrid;
    StringGrid2: TStringGrid;
    StringGrid3: TStringGrid;
    StringGrid4: TStringGrid;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    ToolPanel1: TToolPanel;
    procedure FormCreate(Sender: TObject);
    procedure RxViewsPanel1SelectViewEvent(Sender: TObject; ItemIndex: integer;
      const Item: TRxViewsPanelItem);
  private
    //
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  RxViewsPanel1.ItemIndex:=1;
end;

procedure TForm1.RxViewsPanel1SelectViewEvent(Sender: TObject;
  ItemIndex: integer; const Item: TRxViewsPanelItem);
begin
  ExtendedNotebook1.PageIndex:=ItemIndex
end;

end.

