unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Buttons, ComCtrls, ExtCtrls, MPlayerCtrl;

type

  { TForm1 }

  TForm1 = class(TForm)
    MPlayerControl1: TMPlayerControl;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    PlaySpeedButton: TSpeedButton;
    PauseSpeedButton: TSpeedButton;
    OpenSpeedButton: TSpeedButton;
    StatusBar1: TStatusBar;
    StopSpeedButton: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure OpenSpeedButtonClick(Sender: TObject);
    procedure PauseSpeedButtonClick(Sender: TObject);
    procedure PlaySpeedButtonClick(Sender: TObject);
    procedure StopSpeedButtonClick(Sender: TObject);
  private
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.PlaySpeedButtonClick(Sender: TObject);
begin
  if not FileExistsUTF8(MPlayerControl1.Filename) then begin
    MessageDlg('File not found',
      'Please choose a file before playing',mtError,[mbCancel],0);
    exit;
  end;
  MPlayerControl1.Play;
end;

procedure TForm1.StopSpeedButtonClick(Sender: TObject);
begin
  MPlayerControl1.Stop;
end;

procedure TForm1.PauseSpeedButtonClick(Sender: TObject);
begin
  MPlayerControl1.Paused:=not MPlayerControl1.Paused;
end;

procedure TForm1.OpenSpeedButtonClick(Sender: TObject);
begin
  if not OpenDialog1.Execute then exit;
  MPlayerControl1.Stop;
  MPlayerControl1.Filename:=OpenDialog1.FileName;
  StatusBar1.SimpleText:=CleanAndExpandFilename(MPlayerControl1.Filename);
  MPlayerControl1.Play;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  if Paramcount>0 then begin
    MPlayerControl1.Filename:=CleanAndExpandFilename(ParamStrUTF8(1));
    StatusBar1.SimpleText:=MPlayerControl1.Filename;
  end;
  {$IFDEF Linux}
   MPlayerControl1.MPlayerPath:='';
   MPlayerControl1.StartParam:='-vo x11 -zoom -fs';
  {$else $IFDEF Windows}
   // Download MPlayer generic for Windows and save under Programm Folder Directory
   // http://sourceforge.net/projects/mplayer-win32/
   MPlayerControl1.MPlayerPath:=extractfilepath(application.exename)+'MPlayer\mplayer.exe' ;
   //MPlayerControl1.StartParam:='-zoom -fs';
  {$endif}
end;

end.

