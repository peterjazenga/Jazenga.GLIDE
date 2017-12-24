program test_kaaj_controls;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,
  fpg_base,
  fpg_main,
  fpg_form,
  fpg_menu,
  fpg_trackbar,
  fpg_checkbox,
  fpg_radiobutton,
  kaaj_theme,
  fpg_stylemanager,
  fpg_widget,
  kaaj_button,
  kaaj_panel,
  kaaj_edit,
  kaaj_combobox,
  kaaj_progressbar,
  BGRABitmap,
  BGRABitmapTypes;

type

  { TMainForm }

  TMainForm = class(TfpgForm)
  private
    Button1: TKaajButton;
    Button2: TKaajButton;
    Panel1: TKaajPanel;
    Edit1: TKaajEdit;
    Edit2: TKaajEdit;
    ComboBox1: TKaajComboBox;
    ComboBox2: TKaajComboBox;
    ProgressBar1: TKaajProgressBar;
    TrackBar1: TfpgTrackBar;
    MainMenu: TfpgMenuBar;
    pmFile: TfpgPopupMenu;
    pmEdit: TfpgPopupMenu;
    pmHelp: TfpgPopupMenu;
    pmSubMenu1: TfpgPopupMenu;
    miSubMenu: TfpgMenuItem;
    CheckBox1: TfpgCheckBox;
    RadioButton1: TfpgRadioButton;
  public
    //Background: TBGRABitmap;
    procedure AfterCreate; override;
    procedure OnButton1Click(Sender: TObject);
    procedure OnChangeTrackbar(Sender: TObject; APosition: integer);
    procedure OnFormCreate(Sender: TObject);
    procedure OnRedrawPanel(Sender: TObject; Bitmap: TBGRABitmap);
    procedure OnFormDestroy(Sender: TObject);
  end;

  procedure TMainForm.AfterCreate;
  begin
    Name := 'MainForm';
    SetPosition(316, 186, 500, 400);
    WindowTitle := 'MainForm';
    OnCreate:=@OnFormCreate;
    OnDestroy := @OnFormDestroy;

    MainMenu := TfpgMenuBar.Create(self);
    with MainMenu do
    begin
      Name := 'MainMenu';
      SetPosition(8, 4, 120, 24);
      Align := alTop;
    end;

    pmFile := TfpgPopupMenu.Create(self);
    with pmFile do
    begin
      Name := 'pmFile';
      SetPosition(204, 148, 120, 24);
      AddMenuItem('&Open', 'Ctrl+O', nil);
      AddMenuItem('&Save', 'Ctrl+S', nil);
      AddMenuItem('S&ave As', 'Ctrl+A', nil);
      AddMenuItem('-', '', nil);
      AddMenuItem('Save && Reload', '', nil);
      AddMenuItem('-', '', nil);
      AddMenuItem('&Quit', 'Ctrl+Q', nil);
    end;

    pmEdit := TfpgPopupMenu.Create(self);
    with pmEdit do
    begin
      Name := 'pmEdit';
      SetPosition(204, 172, 120, 24);
      AddMenuItem('Cut', '', nil);
      AddMenuItem('Copy', '', nil);
      AddMenuItem('Paste', '', nil);
      AddMenuItem('-', '', nil);
      AddMenuItem('Some selected item', '', nil).Checked := True;
      miSubMenu := AddMenuItem('My sub-menu', '', nil);
    end;

    pmHelp := TfpgPopupMenu.Create(self);
    with pmHelp do
    begin
      Name := 'pmHelp';
      SetPosition(204, 196, 120, 24);
      AddMenuItem('About...', '', nil);
    end;

    pmSubMenu1 := TfpgPopupMenu.Create(self);
    with pmSubMenu1 do
    begin
      Name := 'pmSubMenu1';
      SetPosition(204, 220, 120, 24);
      AddMenuItem('Item 1', '', nil);
      AddMenuItem('Item 2', '', nil);
      AddMenuItem('Item 3', '', nil).Enabled := False;
    end;

    MainMenu.AddMenuItem('File', nil).SubMenu := pmFile;
    MainMenu.AddMenuItem('Edit', nil).SubMenu := pmEdit;
    MainMenu.AddMenuItem('Help', nil).SubMenu := pmHelp;

    miSubMenu.SubMenu := pmSubMenu1;

    Panel1 := TKaajPanel.Create(Self);
    with Panel1 do
    begin
      Name := 'Panel1';
      Anchors := [anLeft, anTop, anRight, anBottom];
      SetPosition(10, 30, 450, 350);
      OnRedraw:=@OnRedrawPanel;
    end;

    Button1 := TKaajButton.Create(Panel1);
    with Button1 do
    begin
      Name := 'Button1';
      SetPosition(10, 10, 100, 50);
      OnClick := @OnButton1Click;
    end;

    Button2 := TKaajButton.Create(Panel1);
    with Button2 do
    begin
      Name := 'Button2';
      SetPosition(120, 10, 100, 50);
      Enabled := False;
    end;

    Edit1 := TKaajEdit.Create(Panel1);
    with Edit1 do
    begin
      Name := 'Edit1';
      SetPosition(10, 70, 100, 30);
      Text := 'Test';
    end;

    Edit2 := TKaajEdit.Create(Panel1);
    with Edit2 do
    begin
      Name := 'Edit2';
      SetPosition(120, 70, 100, 30);
      Text := 'Test';
      Enabled := False;
    end;

    ComboBox1 := TKaajComboBox.Create(Panel1);
    with ComboBox1 do
    begin
      Name := 'ComboBox1';
      SetPosition(10, 105, 100, 30);
      Items.Add('List1');
      Items.Add('List2');
    end;

    ComboBox2 := TKaajComboBox.Create(Panel1);
    with ComboBox2 do
    begin
      Name := 'ComboBox2';
      SetPosition(120, 105, 100, 30);
      Items.Add('List1');
      Items.Add('List2');
      Enabled := False;
    end;

    ProgressBar1 := TKaajProgressBar.Create(Panel1);
    with ProgressBar1 do
    begin
      Name := 'ProgressBar1';
      SetPosition(10, 145, 100, 30);
      Position := 50;
    end;

    TrackBar1 := TfpgTrackBar.Create(Panel1);
    with TrackBar1 do
    begin
      Name := 'TrackBar1';
      SetPosition(10, 180, 100, 30);
      Max := 255;
      Min := 0;
      Position := 255;
      OnChange := @OnChangeTrackbar;
    end;

    CheckBox1 := TfpgCheckBox.Create(Panel1);
    with CheckBox1 do
    begin
      Name := 'CheckBox1';
      SetPosition(10, 220, 100, 30);
    end;

    RadioButton1 := TfpgRadioButton.Create(Panel1);
    with RadioButton1 do
    begin
      Name := 'RadioButton1';
      SetPosition(10, 260, 100, 30);
    end;

    Button1.SetFocus;
  end;

  procedure TMainForm.OnButton1Click(Sender: TObject);
  begin
    Button2.Enabled := not Button2.Enabled;
    Edit2.Enabled := not Edit2.Enabled;
    ComboBox2.Enabled := not ComboBox2.Enabled;
    Panel1.DiscardBitmap;
  end;

  procedure TMainForm.OnChangeTrackbar(Sender: TObject; APosition: integer);
  begin
    Button1.Alpha := APosition;
    Button2.Alpha := APosition;
    //Panel1.Alpha := APosition;
    Edit1.Alpha := APosition;
    Edit2.Alpha := APosition;
    ComboBox1.Alpha := APosition;
    ComboBox2.Alpha := APosition;
    ProgressBar1.Alpha := APosition;
    Self.Invalidate;
  end;

procedure TMainForm.OnRedrawPanel(Sender: TObject; Bitmap: TBGRABitmap);
begin
  //if Button2.Enabled then
  //  Bitmap.StretchPutImage(Rect(0, 0, Panel1.Width, Panel1.Height), Background, dmSet);
end;

procedure TMainForm.OnFormCreate(Sender: TObject);
begin
  //Background := TBGRABitmap.Create('..\Files\back1.jpg');
end;

procedure TMainForm.OnFormDestroy(Sender: TObject);
begin
  //Background.Free;
end;

  procedure MainProc;
  var
    frm: TMainForm;
  begin
    fpgApplication.Initialize;

    AddImages();

    if fpgStyleManager.SetStyle('Kaaj') then
      fpgStyle := fpgStyleManager.Style;

    fpgApplication.CreateForm(TMainForm, frm);
    try
      frm.Show;
      fpgApplication.Run;
    finally
      frm.Free;
    end;
  end;

{$R *.res}

begin
  MainProc;
end.
