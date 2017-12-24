unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, SpkToolbar,
  SpkGUITools, SpkMath, SpkGraphTools, spkt_Tab, spkt_Pane, spkt_Types,
  spkt_Tools, ImgList, ComCtrls, Menus, Grids, ExtCtrls, spkt_BaseItem,
  spkt_Buttons, spkt_Checkboxes;

type

  { TForm1 }

  TForm1 = class(TForm)
    ImageList: TImageList;
    LargeImageList: TImageList;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    Panel1: TPanel;
    RecentFilesPopupMenu: TPopupMenu;
    StatusBar1: TStatusBar;
    StringGrid1: TStringGrid;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    SpkToolbar : TSpkToolbar;
    CbHorizGrid : TSpkCheckbox;
    CbVertGrid: TSpkCheckbox;
    CbRowSelect: TSpkCheckbox;
    procedure FileOpenHandler(Sender: TObject);
    procedure FileSaveHandler(Sender: TObject);
    procedure FileQuitHandler(Sender: TObject);
    procedure HorizontalGridLinesHandler(Sender: TObject);
    procedure VerticalGridLinesHandler(Sender: TObject);
    procedure RowSelectHandler(Sender: TObject);
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FileOpenHandler(Sender: TObject);
begin
  Statusbar1.SimpleText := '"File" / "Open" clicked';
end;

procedure TForm1.FileSaveHandler(Sender: TObject);
begin
  Statusbar1.SimpleText := '"File" / "Save" clicked';
end;

procedure TForm1.FileQuitHandler(Sender: TObject);
begin
  Close;
end;

procedure TForm1.HorizontalGridLinesHandler(Sender: TObject);
begin
  if CbHorizGrid.Checked then
    StringGrid1.Options := StringGrid1.Options + [goHorzLine]
  else
    StringGrid1.Options := StringGrid1.Options - [goHorzLine];
end;

procedure TForm1.VerticalGridLinesHandler(Sender: TObject);
begin
  if CbVertGrid.Checked then
    StringGrid1.Options := StringGrid1.Options + [goVertLine]
  else
    StringGrid1.Options := StringGrid1.Options - [goVertLine];
end;

procedure TForm1.RowSelectHandler(Sender: TObject);
begin
  if CbRowSelect.Checked then
    StringGrid1.Options := StringGrid1.Options + [goRowSelect]
  else
    StringGrid1.Options := StringGrid1.Options - [goRowSelect];
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SpkToolbar := TSpkToolbar.Create(self);
  with SpkToolbar do begin
    Parent := self;
    Appearance.Pane.CaptionFont.Style := [fsBold, fsItalic];
    Color := clSkyBlue;
    Images := ImageList;
    LargeImages := LargeImageList;
    ShowHint := true;
    with Tabs.Add do begin
      Caption := 'File';
      with Panes.Add do begin
        Caption := 'File commands';
        with Items.AddLargeButton do begin
          Caption := 'Open';
          ButtonKind := bkButtonDropdown;
          DropdownMenu := RecentFilesPopupMenu;
          LargeImageIndex := 1;
//          Hint := 'Open a file';
          OnClick := @FileOpenHandler;
        end;
        with Items.AddLargeButton do begin
          Caption := 'Save';
          LargeImageIndex := 2;
//          Hint := 'Save file';
          OnClick := @FileSaveHandler;
        end;
        with Items.AddLargeButton do begin
          Caption := 'Quit';
          LargeImageIndex := 0;
//          Hint := 'Close application';
          OnClick := @FileQuitHandler;
        end;
      end;
    end;
    with Tabs.Add do begin
      Caption := 'Edit';
      with Panes.Add do begin
        Caption := 'Edit commands';
        with Items.AddSmallButton do begin
          Caption := 'Cut';
          HideFrameWhenIdle := true;
          TableBehaviour := tbBeginsRow;
          ImageIndex := 3;
//          Hint := 'Cut to clipboard';
        end;
        with Items.AddSmallButton do begin
          Caption := 'Copy';
          HideFrameWhenIdle := true;
          TableBehaviour := tbBeginsRow;
          ImageIndex := 4;
//          Hint := 'Copy to clipboard';
        end;
        with Items.AddSmallButton do begin
          Caption := 'Paste';
          HideFrameWhenIdle := true;
          TableBehaviour := tbBeginsColumn;
          ImageIndex := 5;
//          Hint := 'Paste from clipboard';
        end;
      end;
    end;
    with Tabs.Add do begin
      Caption := 'Options';
      with Panes.Add do begin
        Caption := 'Grid settings';
        CbHorizGrid := Items.AddCheckbox;
        with CbHorizGrid do begin
          Caption := 'Horizontal grid lines';
          TableBehaviour := tbBeginsRow;
          Checked := true;
//          Hint := 'Show/hide horizontal grid lines';
          OnClick := @HorizontalGridLinesHandler;
        end;
        CbVertGrid := Items.AddCheckbox;
        with CbVertGrid do begin
          Caption := 'Vertical grid lines';
//          Hint := 'Show/hide vertical grid lines';
          TableBehaviour := tbBeginsRow;
          Checked := true;
          OnClick := @VerticalGridLinesHandler;
        end;
        CbRowSelect := Items.AddCheckbox;
        with CbRowSelect do begin
          Caption := 'Row select';
          TableBehaviour := tbBeginsRow;
          Checked := false;
//          Hint := 'Select entire row';
          OnClick := @RowSelectHandler;
        end;
      end;
      with Panes.Add do begin
        Caption := 'Save settings';
        with Items.AddSmallButton do begin
          Caption := 'Save now';
//          Hint := 'Save settings now';
          ImageIndex := 2;
        end;
        with Items.AddCheckbox do begin
          Caption := 'Auto-save settings';
          Checked := true;
//          Hint := 'Automatically save settings when program closes';
        end;
      end;
    end;
  end;

end;

end.

