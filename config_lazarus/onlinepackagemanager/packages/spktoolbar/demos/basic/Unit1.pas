unit Unit1;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SpkToolbar, StdCtrls, ExtCtrls, SpkGUITools, SpkMath, SpkGraphTools,
  Spin, spkt_Tab, spkt_Pane, ActnList, {ButtonGroup,} Menus, spkt_Types,
  spkt_Tools, ImgList, spkt_BaseItem, spkt_Buttons;

type
  TForm2 = class(TForm)
    ActionList1: TActionList;
    Action1: TAction;
    PopupMenu1: TPopupMenu;
    LargeImages: TImageList;
    Images: TImageList;
    SpkToolbar1: TSpkToolbar;
    SpkTab1: TSpkTab;
    CUsersSpookDokumenty1: TMenuItem;
    DDokumenty1: TMenuItem;
    SpkPane2: TSpkPane;
    SpkSmallButton2: TSpkSmallButton;
    SpkSmallButton3: TSpkSmallButton;
    SpkSmallButton4: TSpkSmallButton;
    SpkLargeButton4: TSpkLargeButton;
    SpkPane3: TSpkPane;
    SpkSmallButton1: TSpkSmallButton;
    SpkSmallButton5: TSpkSmallButton;
    SpkSmallButton6: TSpkSmallButton;
    SpkSmallButton7: TSpkSmallButton;
    SpkSmallButton8: TSpkSmallButton;
    SpkPane4: TSpkPane;
    SpkSmallButton10: TSpkSmallButton;
    SpkLargeButton5: TSpkLargeButton;
    SpkSmallButton9: TSpkSmallButton;
    SpkTab2: TSpkTab;
    SpkPane5: TSpkPane;
    SpkLargeButton6: TSpkLargeButton;
    SpkLargeButton7: TSpkLargeButton;
    SpkLargeButton8: TSpkLargeButton;
    PopupMenu2: TPopupMenu;
    Recent11: TMenuItem;
    Recent21: TMenuItem;
    Recent31: TMenuItem;
    SpkPane1: TSpkPane;
    SpkLargeButton1: TSpkLargeButton;
    SpkLargeButton2: TSpkLargeButton;
    SpkLargeButton3: TSpkLargeButton;
    SpkPane6: TSpkPane;
    SpkSmallButton11: TSpkSmallButton;
    SpkSmallButton12: TSpkSmallButton;
    SpkSmallButton13: TSpkSmallButton;
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.lfm}

procedure TForm2.Button2Click(Sender: TObject);

var i,j,k : integer;                              
    Item : TSpkSmallButton;
    Pane : TSpkPane;
    Tab : TSpkTab;

begin
SpkToolbar1.BeginUpdate;

for i := 0 to 20 do
    Tab:=SpkToolbar1.Tabs.add;

for k := 0 to 6 do
    begin
    Pane:=SpkTab1.Panes.Add;
    for j := 0 to 2 do
        begin
        Item:=Pane.Items.AddSmallButton;
        Item.TableBehaviour:=tbBeginsRow;
        //Item.GroupBehaviour:=gbBeginsGroup;
        Item.ShowCaption:=false;
        Item.ImageIndex:=random(50);
        //Item.DropdownMenu:=PopupMenu1;

        for i := 0 to 4 do
            begin
            Item:=Pane.Items.AddSmallButton;
            Item.ShowCaption:=false;
            Item.ImageIndex:=random(50);
            //Item.GroupBehaviour:=gbContinuesGroup;
            //Item.DropdownMenu:=PopupMenu1;
            end;

        Item:=Pane.Items.AddSmallButton;
        Item.TableBehaviour:=tbContinuesRow;
        //Item.GroupBehaviour:=gbEndsGroup;
        Item.ShowCaption:=false;
        Item.ImageIndex:=random(50);
        //Item.DropdownMenu:=PopupMenu1;
        end;
    end;

SpkToolbar1.EndUpdate;
end;

end.
