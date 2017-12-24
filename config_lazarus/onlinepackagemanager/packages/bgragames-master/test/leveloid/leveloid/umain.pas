unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, ExtCtrls,
  LCLType, BGRABitmap, uos_flat, SdpoJoystick, bgTools, bgTileMapGL,
  bgFilters, uPSComponent, BGRABitmapTypes, BGRAOpenGL, uPSUtils, uleveloid,
  Dialogs, OpenGLContext;

type

  { TfrmMain }

  TGameStyle = (gsNormal, gsGameBoy, gsBlackAndWhite);
  THitWall = (hwEmpty, hwHitSolid, hwResetPlayer, hwGoNextLevel);

  TfrmMain = class(TForm)
    mainTimer: TTimer;
    bgMap: TOpenGLControl;
    psscript: TPSScript;
    procedure bgMapPaint(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure mainTimerTimer(Sender: TObject);
    procedure psscriptCompile(Sender: TPSScript);
    procedure psscriptExecute(Sender: TPSScript);
    function psscriptNeedFile(Sender: TObject; const OrginFileName: tbtstring;
      var FileName, Output: tbtstring): Boolean;
  private
    FormRect: TRect;
  private
    Sound: boolean;
    Keys: TStringList;
    Joystick: TSdpoJoystick;
    { Sound Library }
    procedure InitUOS;
    procedure EndUOS;
    { Keyboard }
    procedure InitKeys;
    procedure EndKeys;
    procedure KeysArrows(out p_up, p_down, p_left, p_right: boolean);
    { Joystick }
    procedure InitJoystick;
    procedure EndJoystick;
    procedure JoystickPOV(out p_up, p_down, p_left, p_right: boolean);
  private
    TileMap: TLeveloid;
    CurrentLevel, NextLevel: string;
    { Level }
    GameLoaded: boolean;
    procedure LoadGame;
    procedure InitTileMap;
    procedure EndTileMap;
    procedure InitLevel(FileName: string);
    procedure GoNextLevel;
    { Sound }
    procedure PlayRestartSound;
    procedure PlayBackgroundMusic;
  private
    PlayerPosition: TPoint;
    PlayerOffsetAccumulator: TPointF;
    PosX, PosY: NativeInt;
    PlayerTile: NativeInt;
    AccX, AccY, Gravity, SpdX, SpdY, BncX, BncY, SlwX, SlwY, Stop: single;
    { Player }
    procedure InitPlayer;
  private
    { Level & Player }
    function HitWall: boolean;
  private
    { Game Settings }
    GameStyle: TGameStyle;
    procedure ResizeAll;
  public
    procedure ChangeData(id: integer; Value: string);
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.bgMapPaint(Sender: TObject);
begin
  LoadGame;

  if not mainTimer.Enabled then
  begin
    BGLViewPort(bgMap.Width, bgMap.Height, BGRABlack);
    bgMap.SwapBuffers;
    exit;
  end;

  TileMap.DrawMap(bgMap.Width, bgMap.Height);
  TileMap.DrawTile(PlayerPosition.x, PlayerPosition.y, PlayerTile, 255);

  { in unit bgFilters }
  {case GameStyle of
    gsGameBoy: GameBoy(CacheLevel);
    gsBlackAndWhite: BlackAndWhite(CacheLevel);
  end;}

  bgMap.SwapBuffers;
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  mainTimer.Enabled := False;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  GameLoaded := False;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  EndUOS;
  EndKeys;
  EndJoystick;
  EndTileMap;
end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  Keys.Values[IntToStr(Key)] := 'True';
end;

procedure TfrmMain.FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  Keys.Values[IntToStr(Key)] := 'False';

  if Key = VK_ESCAPE then
    Self.Close;

  if Key = VK_F1 then
    if GameStyle = gsNormal then
      GameStyle := gsGameBoy
    else if GameStyle = gsGameBoy then
      GameStyle := gsBlackAndWhite
    else
      GameStyle := gsNormal;
end;

procedure TfrmMain.FormResize(Sender: TObject);
begin
  CenterControl(bgMap);
end;

procedure TfrmMain.mainTimerTimer(Sender: TObject);
var
  p_up, p_down, p_left, p_right, p_btn1: boolean;
  p_origin: TPoint;
begin
  if not GameLoaded then
    exit;

  p_origin := PlayerPosition;

  if Joystick.Active then
    JoystickPOV(p_up, p_down, p_left, p_right)
  else
    KeysArrows(p_up, p_down, p_left, p_right);

  if Joystick.Active then
  begin
    if Joystick.Buttons[0] <> 0 then
      p_btn1 := True;
  end
  else
  begin
    if Keys.Values[IntToStr(VK_SPACE)] = 'True' then
      p_btn1 := True;
  end;

  if p_up then
    AccY := AccY - SpdY;
  if p_down then
    AccY := AccY + SpdY;
  if p_left then
    AccX := AccX - SpdX;
  if p_right then
    AccX := AccX + SpdX;

  if p_btn1 then
  begin
    AccY := AccY * SlwY;
    AccX := AccX * SlwX;
  end;

  AccY := AccY + Gravity;

  PlayerOffsetAccumulator.x += AccX;
  PlayerPosition.x += round(PlayerOffsetAccumulator.x);
  PlayerOffsetAccumulator.x -= round(PlayerOffsetAccumulator.x);

  if (not p_left) and (not p_right) then
  begin
    AccX := AccX * Stop;
    if abs(AccX) < 0.1 then
      AccX := 0;
  end;

  if HitWall then
  begin
    while PlayerPosition.x <> p_origin.x do
    begin
      PlayerPosition.x := IntTowards(PlayerPosition.x, p_origin.x);
      if not HitWall then
        break;
    end;
    AccX := -BncX * AccX;
  end;

  PlayerOffsetAccumulator.y += AccY;
  PlayerPosition.y += round(PlayerOffsetAccumulator.y);
  PlayerOffsetAccumulator.y -= round(PlayerOffsetAccumulator.y);

  if HitWall then
  begin
    while PlayerPosition.y <> p_origin.y do
    begin
      PlayerPosition.y := IntTowards(PlayerPosition.y, p_origin.y);
      if not HitWall then
        break;
    end;
    AccY := -BncY * AccY;
  end;

  bgMap.DoOnPaint;
end;

procedure TfrmMain.psscriptCompile(Sender: TPSScript);
begin
  Sender.AddMethod(Self, @TfrmMain.ChangeData,
    'procedure ChangeData(id: Integer; value: string);');
  Sender.comp.AddTypeS('THitWall',
    '(hwEmpty, hwHitSolid, hwResetPlayer, hwGoNextLevel)');
  Sender.AddRegisteredPTRVariable('PlayerTile', 'Integer');
  Sender.AddRegisteredPTRVariable('PosX', 'Integer');
  Sender.AddRegisteredPTRVariable('PosY', 'Integer');
  Sender.AddRegisteredPTRVariable('AccX', 'Single');
  Sender.AddRegisteredPTRVariable('AccY', 'Single');
  Sender.AddRegisteredPTRVariable('SpdX', 'Single');
  Sender.AddRegisteredPTRVariable('SpdY', 'Single');
  Sender.AddRegisteredPTRVariable('BncX', 'Single');
  Sender.AddRegisteredPTRVariable('BncY', 'Single');
  Sender.AddRegisteredPTRVariable('SlwX', 'Single');
  Sender.AddRegisteredPTRVariable('SlwY', 'Single');
  Sender.AddRegisteredPTRVariable('Stop', 'Single');
  Sender.AddRegisteredPTRVariable('Gravity', 'Single');
  Sender.AddRegisteredPTRVariable('NextLevel', 'String');
end;

procedure TfrmMain.psscriptExecute(Sender: TPSScript);
begin
  PSScript.SetPointerToData('PlayerTile', @PlayerTile, PSScript.FindBaseType(btS32));
  PSScript.SetPointerToData('PosX', @PosX, PSScript.FindBaseType(btS32));
  PSScript.SetPointerToData('PosY', @PosY, PSScript.FindBaseType(btS32));
  PSScript.SetPointerToData('AccX', @AccX, PSScript.FindBaseType(btSingle));
  PSScript.SetPointerToData('AccY', @AccY, PSScript.FindBaseType(btSingle));
  PSScript.SetPointerToData('SpdX', @SpdX, PSScript.FindBaseType(btSingle));
  PSScript.SetPointerToData('SpdY', @SpdY, PSScript.FindBaseType(btSingle));
  PSScript.SetPointerToData('BncX', @BncX, PSScript.FindBaseType(btSingle));
  PSScript.SetPointerToData('BncY', @BncY, PSScript.FindBaseType(btSingle));
  PSScript.SetPointerToData('SlwX', @SlwX, PSScript.FindBaseType(btSingle));
  PSScript.SetPointerToData('SlwY', @SlwY, PSScript.FindBaseType(btSingle));
  PSScript.SetPointerToData('Stop', @Stop, PSScript.FindBaseType(btSingle));
  PSScript.SetPointerToData('Gravity', @Gravity, PSScript.FindBaseType(btSingle));
  PSScript.SetPointerToData('NextLevel', @NextLevel, PSScript.FindBaseType(btString));
end;

function TfrmMain.psscriptNeedFile(Sender: TObject;
  const OrginFileName: tbtstring; var FileName, Output: tbtstring): Boolean;
var
  aData: TStringList;
begin
  FileName := GetCurrentDir + '\' + FileName;

  aData := TStringList.Create;
  try
    aData.LoadFromFile(FileName);
    Output := aData.Text;
    Result := True;
  finally
    aData.Free;
  end;
end;

procedure TfrmMain.InitUOS;
var
  path0, uos1, uos2: string;
begin
  Sound := False;

  path0 := ExtractFilePath(ParamStr(0));
  uos1 := path0 + '\LibPortaudio-32.dll';
  uos2 := path0 + '\LibSndFile-32.dll';

  Sound := uos_loadlib(PChar(uos1), PChar(uos2), PChar(''), PChar(''), PChar('')) = 0;
end;

procedure TfrmMain.EndUOS;
begin
  uos_unloadlib;
  uos_free;
end;

procedure TfrmMain.InitKeys;
begin
  Keys := TStringList.Create;
end;

procedure TfrmMain.EndKeys;
begin
  Keys.Free;
end;

procedure TfrmMain.KeysArrows(out p_up, p_down, p_left, p_right: boolean);
begin
  p_left := Keys.Values[IntToStr(VK_LEFT)] = 'True';
  p_right := Keys.Values[IntToStr(VK_RIGHT)] = 'True';
  p_up := Keys.Values[IntToStr(VK_UP)] = 'True';
  p_down := Keys.Values[IntToStr(VK_DOWN)] = 'True';
end;

procedure TfrmMain.InitJoystick;
begin
  Joystick := TSdpoJoystick.Create(Self);
  try
    Joystick.Active := True;
  except
    Joystick.Active := False;
  end;
end;

procedure TfrmMain.EndJoystick;
begin
  Joystick.Free;
end;

procedure TfrmMain.JoystickPOV(out p_up, p_down, p_left, p_right: boolean);
begin
  p_up := False;
  p_down := False;
  p_left := False;
  p_right := False;
  case Joystick.Axis[6] of
    27000: p_left := True;
    9000: p_right := True;
    0: p_up := True;
    18000: p_down := True;
    31500:
    begin
      p_up := True;
      p_left := True;
    end;
    4500:
    begin
      p_up := True;
      p_right := True;
    end;
    22500:
    begin
      p_down := True;
      p_left := True;
    end;
    13500:
    begin
      p_down := True;
      p_right := True;
    end;
  end;
end;

procedure TfrmMain.LoadGame;
begin
  if GameLoaded then
  exit;

  FormRect := Self.ClientRect;
  ToggleFullScreen(Self, FormRect);

  InitUOS;
  InitJoystick;
  InitKeys;
  InitTileMap;

  GameLoaded := True;
end;

procedure TfrmMain.InitTileMap;
begin
  if ParamStr(1) <> '' then
  begin
    CurrentLevel := ParamStr(1);
    SetCurrentDir(ExtractFilePath(ParamStr(1)));
  end
  else
  begin
    CurrentLevel := 'main.leveloid';
    SetCurrentDir(ExtractFilePath(ParamStr(0)) + '\levels\');
  end;

  InitLevel(CurrentLevel);
  PlayBackgroundMusic;
end;

procedure TfrmMain.EndTileMap;
begin
  TileMap.Free;
end;

procedure TfrmMain.InitLevel(FileName: string);
var
  i: integer;
  s: string;
begin
  if (FileName <> '') then
  begin
    mainTimer.Enabled := False;

    if TileMap <> nil then
      EndTileMap;
    TileMap := TLeveloid.Create(FileName);

    CurrentLevel := FileName;
    NextLevel := TileMap.NextLevel;

    psscript.Script := TileMap.Script;

    if psscript.Compile then
      psscript.Execute
    else
    begin
      s := 'Compile error.' + LineEnding;
      for i := 0 to PSScript.CompilerMessageCount - 1 do
        with PSScript.CompilerMessages[i] do
          s += '(' + IntToStr(Row) + ',' + IntToStr(Col) + ') ' +
            MessageToString + LineEnding;
      ShowMessage(s);
      Self.Visible := False;
      Application.ShowMainForm := False;
      Application.Terminate;
      exit;
    end;

    uos_Stop(0);

    ResizeAll;

    mainTimer.Enabled := True;
  end;

  InitPlayer;
end;

procedure TfrmMain.GoNextLevel;
begin
  InitLevel(NextLevel);
end;

procedure TfrmMain.ChangeData(id: integer; Value: string);
begin
  TileMap.Layers[0].Data[id] := Value;
end;

procedure TfrmMain.PlayRestartSound;
begin
  if Sound then
    PlaySound(1, psscript.ExecuteFunction([], 'OnPlayRestartSound'));
end;

procedure TfrmMain.PlayBackgroundMusic;
begin
  if Sound then
    PlaySoundLoop(0, psscript.ExecuteFunction([], 'OnPlayBackgroundMusic'),
      @PlayBackgroundMusic);
end;

procedure TfrmMain.InitPlayer;
begin
  PlayRestartSound;
  PlayerPosition := Point(PosX, PosY);
end;

function TfrmMain.HitWall: boolean;
var
  x, x1, x2, y, y1, y2, tx, ty, n: NativeInt;
  Player, Wall: TRectangle;
  mode: THitWall;
begin
  Result := False;

  tx := TileMap.Map.TileWidth;
  ty := TileMap.Map.TileHeight;

  Player := DoRectangle(PlayerPosition.x, PlayerPosition.y, tx, ty);

  y1 := BoundInt(Player.y div ty, 0, TileMap.Map.Height - 1);
  y2 := BoundInt((Player.y + Player.Height - 1) div ty, 0, TileMap.Map.Height - 1);

  n := y1 * TileMap.Map.Width;

  x1 := BoundInt(Player.x div tx, 0, TileMap.Map.Width - 1);
  x2 := BoundInt((Player.x + Player.Width - 1) div tx, 0, TileMap.Map.Width - 1);

  for y := y1 to y2 do
  begin
    for x := x1 to x2 do
    begin
      Wall := DoRectangle(x * tx, y * ty, tx, ty);

      mode := psscript.ExecuteFunction([TileMap.Layers[0].Data[n + x], n + x],
        'OnHitWall');

      case mode of
        hwResetPlayer: InitPlayer;
        hwGoNextLevel: GoNextLevel;
        hwHitSolid:
          if HitTest(Wall, Player) then
            Result := True;
      end;

    end;
    n += TileMap.Map.Width;
  end;
end;

procedure TfrmMain.ResizeAll;
begin
  FormRect.Right := TileMap.Map.TileWidth * TileMap.Map.Width;
  FormRect.Bottom := TileMap.Map.TileHeight * TileMap.Map.Height;
  Self.Constraints.MinWidth := FormRect.Right;
  Self.Constraints.MinHeight := FormRect.Bottom;
  //Self.SetBounds(FormRect.Left, FormRect.Top, FormRect.Right, FormRect.Bottom);
  bgMap.SetBounds(FormRect.Left, FormRect.Top, FormRect.Right, FormRect.Bottom);
  CenterControl(bgMap);
end;

end.
