Unit FormMain;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, ExtCtrls, ComCtrls,
  StdCtrls, MPlayerCtrl, Process, types;

Type

  { TfrmMain }
  TfrmMain = Class(TForm)
    btnFrameGrab: TToolButton;
    btnFWD: TToolButton;
    btnLoad: TToolButton;
    btnNudgeBack: TToolButton;
    btnNudgeForward: TToolButton;
    btnPause: TToolButton;
    btnPlay: TToolButton;
    btnRewind: TToolButton;
    btnRunCommand: TButton;
    btnStop: TToolButton;
    cboCommand: TComboBox;
    cboStartParams: TComboBox;
    ilTools: TImageList;
    lblCommand: TLabel;
    lblPos: TLabel;
    lblStartParams: TLabel;
    memResults: TMemo;
    MPlayerControl1: TMPlayerControl;
    OpenDialog1: TOpenDialog;
    dlgFindmplayer: TOpenDialog;
    pnlCommands: TPanel;
    pnlFeedback: TPanel;
    pnlPos: TPanel;
    pnlTrackbar: TPanel;
    pnlVideo: TPanel;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    tbMain: TToolBar;
    ToolButton2: TToolButton;
    ToolButton4: TToolButton;
    ToolButton6: TToolButton;
    ToolButton9: TToolButton;
    TrackBarPlaying: TTrackBar;
    TrackBarVolume: TTrackBar;
    procedure btnFrameGrabClick(Sender: TObject);
    procedure btnFWDClick(Sender: TObject);
    Procedure btnLoadClick(Sender: TObject);
    procedure btnNudgeBackClick(Sender: TObject);
    procedure btnNudgeForwardClick(Sender: TObject);
    Procedure btnPauseClick(Sender: TObject);
    Procedure btnPlayClick(Sender: TObject);
    Procedure btnRunCommandClick(Sender: TObject);
    Procedure btnStopClick(Sender: TObject);
    Procedure FormCreate(Sender: TObject);

      procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
      var Handled: Boolean);

        procedure MPlayerControl1MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
        var Handled: Boolean);
    procedure OnGrabImage(ASender: TObject; AFilename: String);
    Procedure OnError(ASender: TObject; AStrings: TStringList);
    Procedure OnFeedback(ASender: TObject; AStrings: TStringList);
    Procedure OnPlay(Sender: TObject);
    Procedure OnPlaying(ASender: TObject; APosition: Single);
    Procedure OnStop(Sender: TObject);
    Procedure TrackBarPlayingChange(Sender: TObject);
    Procedure TrackBarPlayingMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure TrackBarPlayingMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure TrackBarVolumeChange(Sender: TObject);
  Private
    Function GetUpdatingPosition: Boolean;
    Procedure SetUpdatingPosition(AValue: Boolean);

    Procedure PopulateCommands(ARunning: Boolean);

    Procedure RefreshUI;
  Private
    FUpdatingPosition: Integer;
    FLastPosition: Integer;

    Property UpdatingPosition: Boolean read GetUpdatingPosition write SetUpdatingPosition;
  End;

Var
  frmMain: TfrmMain;

Implementation

Uses
  FileUtil;

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
Begin
  FUpdatingPosition := 0;
  FLastPosition := -1;
  TrackBarPlaying.Max := 50;

  MPlayerControl1.Volume := 50;

  // Have a go at finding where mplayer is installed
  If Not MPlayerControl1.FindMPlayerPath Then
    MPlayerControl1.MPlayerPath :=
      IncludeTrailingBackslash(ExtractFileDir(Application.ExeName)) +
      IncludeTrailingBackSlash('mplayer') + 'mplayer' + GetExeExt;

  {$IFDEF Linux}
  MPlayerControl1.StartParam := '-vo x11 -zoom -fs';
  {$else $IFDEF Windows}
  MPlayerControl1.StartParam := '-vo direct3d -nofontconfig';
  {$ENDIF}

  cboStartParams.Text := MPlayerControl1.StartParam;

  PopulateCommands(False);
End;

procedure TfrmMain.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  Caption := Format('WheelDelta %d', [WheelDelta]);
end;

procedure TfrmMain.MPlayerControl1MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  if MPlayerControl1.Running Then
  begin
    MPlayerControl1.Paused := True;

    if WheelDelta>0 Then
      MPlayerControl1.Position := MPlayerControl1.Position + 1/3
    Else
      MPlayerControl1.Position := MPlayerControl1.Position - 1/3;
  end;
end;

procedure TfrmMain.OnGrabImage(ASender: TObject; AFilename: String);
begin
  memResults.Lines.Add('Grabbed image: '+AFilename);
end;

procedure TfrmMain.btnLoadClick(Sender: TObject);
Begin
  // If we didn't find the mplayer install, then ask the user if they know instead...
  If not MPlayerControl1.FindMPlayerPath Then
  begin
    dlgFindmplayer.Filename := 'mplayer'+GetExeExt;
    If dlgFindmplayer.Execute Then
      MPlayerControl1.MPlayerPath:=dlgFindmplayer.FileName;
  end;

  If Not FileExists(MPlayerControl1.MPlayerPath) Then
    ShowMessage('mplayer not found!');

  If OpenDialog1.Execute Then
  Begin
    MPlayerControl1.Stop;
    memResults.Lines.Clear;
    MPlayerControl1.StartParam := cboStartParams.Text;
    MPlayerControl1.Filename := OpenDialog1.Filename;
    MPlayerControl1.Play;

    btnPlay.Enabled := True;
  End;
End;

procedure TfrmMain.btnNudgeBackClick(Sender: TObject);
begin
  MPlayerControl1.Paused := True;
  MPlayerControl1.Position := MPlayerControl1.Position - 1;
end;

procedure TfrmMain.btnNudgeForwardClick(Sender: TObject);
begin
  MPlayerControl1.Paused := True;
  MPlayerControl1.Position := MPlayerControl1.Position + 1;
end;

procedure TfrmMain.btnFWDClick(Sender: TObject);
begin
  MPlayerControl1.Rate := MPlayerControl1.Rate * sqrt(2);
end;

procedure TfrmMain.btnFrameGrabClick(Sender: TObject);
begin
  MPlayerControl1.ImagePath:=ExtractFilePath(MPlayerControl1.Filename);
  MPlayerControl1.GrabImage;
  //memResults.Lines.Add('Grabbed '+MPlayerControl1.LastImageFilename);
end;

procedure TfrmMain.btnPauseClick(Sender: TObject);
Begin
  MPlayerControl1.Paused := Not MPlayerControl1.Paused;
  btnPause.Down := MPlayerControl1.Paused;
End;

procedure TfrmMain.btnPlayClick(Sender: TObject);
Begin
  MPlayerControl1.Play;
End;

procedure TfrmMain.btnRunCommandClick(Sender: TObject);
Var
  sOutput: String;
  slCommands : TStringList;
  arrCommands : Array Of String;
  i : Integer;

Begin
  If MPlayerControl1.Running Then
  Begin
    memResults.Lines.Add(cboCommand.Text);

    MPlayerControl1.SendMPlayerCommand(cboCommand.Text);
  End
  Else
  Begin
    sOutput := '';
    slCommands := TStringList.Create;
    slCommands.Delimiter:=' ';
    Try
      CommandToList(cboCommand.Text, slCommands);

      SetLength(arrCommands, slCommands.Count);
      For i := 0 To slCommands.Count-1 Do
        arrCommands[i] := slCommands[i];

      RunCommand(MplayerControl1.MPlayerPath, arrCommands, sOutput);

      memResults.Lines.Add(MplayerControl1.MPlayerPath + ' ' + slCommands.DelimitedText);
      memResults.Append(sOutput);
    finally
      slCommands.Free;
    end;
  End;
End;

procedure TfrmMain.btnStopClick(Sender: TObject);
Begin
  MPlayerControl1.Stop;
End;

procedure TfrmMain.OnFeedback(ASender: TObject; AStrings: TStringList);
Begin
  memResults.Lines.AddStrings(AStrings);

  memResults.SelStart := Length(memResults.Text);
End;

procedure TfrmMain.OnError(ASender: TObject; AStrings: TStringList);
Var
  i: Integer;
Begin
  For i := 0 To AStrings.Count - 1 Do
    memResults.Lines.Add(' Err: ' + AStrings[i]);
End;

procedure TfrmMain.OnPlaying(ASender: TObject; APosition: Single);
Begin
  If (MPlayerControl1.Duration>0) Then
  Begin
    UpdatingPosition := True;
    Try
      btnPause.Down := MPlayerControl1.Paused;

      TrackBarPlaying.SelEnd := Trunc(TrackBarPlaying.Max * APosition / MPlayerControl1.Duration);
      If ActiveControl <> TrackBarPlaying Then
        TrackBarPlaying.Position := TrackBarPlaying.SelEnd;

      lblPos.Caption := FormatDateTime('nnn:ss', APosition / (24 * 60 * 60)) +
        ' / ' + FormatDateTime('nnn:ss', MPlayerControl1.Duration / (24 * 60 * 60));

      pnlPos.Width := lblPos.Width + 3;
    Finally
      UpdatingPosition := False;
    End;
  End;

  UpdatingPosition:=True;
  Try
    // Reversed := True doesn't seem to apply for SelStart/SelEnd...
    // TODO: Talk about on Forum/Consider lodging item on Bugtracker...
    TrackBarVolume.SelEnd := TrackBarVolume.Max;
    TrackBarVolume.SelStart := TrackBarVolume.Max - Trunc(TrackBarVolume.Max *
      MPlayerControl1.Volume / 100);

    If ActiveControl <> TrackBarVolume Then
      TrackBarVolume.Position := TrackBarVolume.SelEnd - TrackBarVolume.SelStart;
  finally
    UpdatingPosition := False;
  end;

  If MPlayerControl1.Paused Then
    StatusBar1.SimpleText := 'Paused'
  Else
    StatusBar1.SimpleText := Format('Playing at rate %.3f', [MPlayerControl1.Rate]);
End;

procedure TfrmMain.TrackBarPlayingChange(Sender: TObject);
Begin
  If (MPlayerControl1.Duration <> -1) And Not UpdatingPosition Then
    If TrackBarPlaying.Position <> FLastPosition Then
    Begin
      MPlayerControl1.Position := MPlayerControl1.Duration * TrackBarPlaying.Position /
        TrackBarPlaying.Max;
      FLastPosition := TrackBarPlaying.Position;
    End;
End;

procedure TfrmMain.TrackBarPlayingMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Begin
  MPlayerControl1.Paused := True;
End;

procedure TfrmMain.TrackBarPlayingMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Begin
  MPlayerControl1.Paused := False;
  Self.ActiveControl := memResults;
End;

procedure TfrmMain.TrackBarVolumeChange(Sender: TObject);
Begin
  If (TrackBarVolume.Position <> TrackBarVolume.Tag) And Not UpdatingPosition Then
  Begin
    MPlayerControl1.Volume := Trunc(100 * TrackBarVolume.Position / TrackBarVolume.Max);

    TrackBarVolume.Tag := TrackBarVolume.Position;
  End;
End;

function TfrmMain.GetUpdatingPosition: Boolean;
Begin
  Result := FUpdatingPosition <> 0;
End;

procedure TfrmMain.SetUpdatingPosition(AValue: Boolean);
Begin
  If AValue Then
    Inc(FUpdatingPosition)
  Else
    Dec(FUpdatingPosition);
End;

procedure TfrmMain.PopulateCommands(ARunning: Boolean);
Begin
  cboCommand.Items.Clear;
  If ARunning Then
  Begin
    lblCommand.Caption := 'Input Commands';
    cboCommand.Items.Add('get_audio_bitrate');
    cboCommand.Items.Add('get_audio_codec');
    cboCommand.Items.Add('get_audio_samples');
    cboCommand.Items.Add('get_file_name');
    cboCommand.Items.Add('get_meta_comment');
    cboCommand.Items.Add('get_time_length');
    cboCommand.Items.Add('get_time_pos');
    cboCommand.Items.Add('get_video_bitrate');
    cboCommand.Items.Add('get_video_codec');
    cboCommand.Items.Add('get_video_resolution');
    cboCommand.Items.Add('mute');
    cboCommand.Items.Add('stop');
    cboCommand.Items.Add('osd [level]');
    cboCommand.Items.Add('osd_show_progression');
    cboCommand.Items.Add('osd_show_text <string> [duration] [level]');
    cboCommand.Items.Add('exit');
    cboCommand.Items.Add('frame_step');
    cboCommand.Items.Add('seek <seconds_From_Start> 2');
    cboCommand.Items.Add('seek <percent> 1');
    cboCommand.Items.Add('screenshot 0');
    cboCommand.Items.Add('speed_mult <value>');
    cboCommand.Items.Add('get_property <property>');
    cboCommand.Items.Add('set_property <property> <value>');
  End
  Else
  Begin
    lblCommand.Caption := 'mplayer Parameters';
    cboCommand.Items.Add('-help');
    cboCommand.Items.Add('-vo help');
    cboCommand.Items.Add('-input cmdlist');
  End;
  cboCommand.ItemIndex := 0;
End;

procedure TfrmMain.RefreshUI;
var
  bRunning: Boolean;
begin
  bRunning := MPlayerControl1.Running;

  If Not bRunning Then
  begin
    UpdatingPosition := True;
    Try
      TrackBarPlaying.Position := 0;
      TrackBarPlaying.SelStart := 0;
      TrackBarPlaying.SelEnd := 0;

      TrackBarVolume.Position := 0;
      TrackBarVolume.SelStart := 0;
      TrackBarVolume.SelEnd := 0;
    Finally
      UpdatingPosition := False;
    End;

    StatusBar1.SimpleText := '';
    lblPos.Caption := '';
  end;

  btnStop.Enabled := bRunning;
  btnPause.Enabled := bRunning;
  btnFWD.Enabled := bRunning;
  btnFrameGrab.Enabled := bRunning;
  btnNudgeBack.Enabled := bRunning;
  btnNudgeForward.Enabled := bRunning;

  lblStartParams.Enabled := Not bRunning;
  cboStartParams.Enabled := Not bRunning;

  PopulateCommands(bRunning);
end;

procedure TfrmMain.OnPlay(Sender: TObject);
Begin
  memResults.Lines.Add('OnPlay message received');
  Caption := Application.Name + ': ' + MPlayerControl1.Filename;

  RefreshUI;
End;

procedure TfrmMain.OnStop(Sender: TObject);
Begin
  If csDestroying In ComponentState Then
    exit;

  memResults.Lines.Add('OnStop message received');
  Caption := Application.Name;

  RefreshUI;
End;

End.
