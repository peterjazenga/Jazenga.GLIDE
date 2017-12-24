unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, ExtCtrls, LCLType, types,
  bgPanel, bgTileMap, bgTools, bgFilters,
  uos_flat, SdpoJoystick, BGRABitmap, BGRABitmapTypes;

const
  GameBoyStyle = False;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    bgMap: TBGPanel;
    mainTimer: TTimer;
    procedure bgMapRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure mainTimerTimer(Sender: TObject);
  private
    FSound: boolean;
    FTileMap: TBGTileMap;
    FCacheBitmap: TBGRABitmap;
    FBackground: TBGRABitmap;
    FKeys: TStringList;
    PlayerPosition: TPoint;
    PlayerOffsetAccumulator: TPointF;
    FAccX, FAccY, FGravity, FAccumX, FAccumY, FStopX, FStopY: real;
    FRestart: boolean;
    FMaps: array [0..1] of string;
    FJoystick: TSdpoJoystick;
    FCurrentMap: NativeInt;
    procedure SetFAccX(AValue: real);
    procedure SetFAccY(AValue: real);
    procedure SetFBackground(AValue: TBGRABitmap);
    procedure SetFCacheBitmap(AValue: TBGRABitmap);
    procedure SetFCurrentMap(AValue: NativeInt);
    procedure SetFGravity(AValue: real);
    procedure SetFKeys(AValue: TStringList);
    procedure SetFRestart(AValue: boolean);
    procedure SetFSound(AValue: boolean);
    procedure SetFTileMap(AValue: TBGTileMap);
  public
    { Property }
    property TileMap: TBGTileMap read FTileMap write SetFTileMap;
    property Keys: TStringList read FKeys write SetFKeys;
    property AccX: real read FAccX write SetFAccX;
    property AccY: real read FAccY write SetFAccY;
    property Gravity: real read FGravity write SetFGravity;
    property CacheBitmap: TBGRABitmap read FCacheBitmap write SetFCacheBitmap;
    property Background: TBGRABitmap read FBackground write SetFBackground;
    property Restart: boolean read FRestart write SetFRestart;
    property Sound: boolean read FSound write SetFSound;
    property Joystick: TSdpoJoystick read FJoystick write FJoystick;
    property CurrentMap: NativeInt read FCurrentMap write SetFCurrentMap;
  public
    { Game }
    procedure InitGame;
    procedure EndGame;
    procedure InitMap(id: NativeInt);
    procedure RestartPlayer;
    function HitWall(AOnlyHitTest: boolean = False): boolean;
    { Sound }
    procedure PlayBackgroundSound;
    procedure PlayRestartSound;
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
  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.bgMapRedraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  Bitmap.PutImage(0, 0, CacheBitmap, dmSet);
  TileMap.DrawTile(Bitmap, PlayerPosition.x, PlayerPosition.y, 3, 255);
end;

procedure TfrmMain.FormActivate(Sender: TObject);
begin
  PlayBackgroundSound;
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  mainTimer.Enabled := False;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  InitUOS;
  InitKeys;
  InitJoystick;
  InitGame;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  EndUOS;
  EndKeys;
  EndJoystick;
  EndGame;
end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  keys.Values[IntToStr(key)] := 'True';
end;

procedure TfrmMain.FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  keys.Values[IntToStr(key)] := 'False';

  if (Key = VK_F11) or (Key = VK_ESCAPE) then
    ToggleFullScreen(Self, Self.BoundsRect);
end;

procedure TfrmMain.FormResize(Sender: TObject);
begin
  InitMap(CurrentMap);
end;

procedure TfrmMain.mainTimerTimer(Sender: TObject);
var
  orig: TPoint;
  p_up, p_down, p_left, p_right: boolean;
begin
  orig := PlayerPosition;

  { If Joystick is Active use Joystick else use Keyboard }
  if Joystick.Active then
    JoystickPOV(p_up, p_down, p_left, p_right)
  else
    KeysArrows(p_up, p_down, p_left, p_right);

  { Acceleration }
  if p_left then
    AccX := AccX - FAccumX;
  if p_right then
    AccX := AccX + FAccumX;
  if p_up then
  begin
    Dec(PlayerPosition.y);
    if HitWall(True) then
      AccY := AccY - FAccumY;
    Inc(PlayerPosition.y, 2);
    if HitWall(True) then
      AccY := Accy - 5;
    Dec(PlayerPosition.Y);
  end;
  if p_down then
    AccY := AccY + FAccumY;

  { Gravity }
  AccY := AccY + Gravity;

  { PlayerPosition X }
  PlayerOffsetAccumulator.x += AccX;
  PlayerPosition.x += round(PlayerOffsetAccumulator.x);
  PlayerOffsetAccumulator.x -= round(PlayerOffsetAccumulator.x);
  AccX := AccX * 0.95;
  if abs(AccX) < 0.1 then
    AccX := 0;

  if HitWall then
  begin
    while PlayerPosition.x <> orig.x do
    begin
      PlayerPosition.x := IntTowards(PlayerPosition.x, orig.x);
      if not HitWall then
        break;
    end;
    AccX := 0;
  end;

  { PlayerPosition Y }
  PlayerOffsetAccumulator.y += AccY;
  PlayerPosition.y += round(PlayerOffsetAccumulator.y);
  PlayerOffsetAccumulator.y -= round(PlayerOffsetAccumulator.y);

  if HitWall then
  begin
    while PlayerPosition.y <> orig.y do
    begin
      PlayerPosition.y := IntTowards(PlayerPosition.y, orig.y);
      if not HitWall then
        break;
    end;
    if AccY > 0 then
      AccY := -FStopY * AccY
    else
      AccY := 0;
  end;

  { Restart Player }
  if Restart then
  begin
    PlayRestartSound;
    RestartPlayer;
  end;

  { Redraw }
  bgMap.RedrawBitmap;
end;

procedure TfrmMain.SetFTileMap(AValue: TBGTileMap);
begin
  if FTileMap = AValue then
    Exit;
  FTileMap := AValue;
end;

procedure TfrmMain.InitGame;
begin
  { Init TileMap }
  TileMap := TBGTileMap.Create;

  { Set Map Names }
  FMaps[0] := 'map1.ini';
  FMaps[1] := 'map.ini';

  { Load Background }
  Background := TBGRABitmap.Create('background.png');
  CacheBitmap := TBGRABitmap.Create;

  { Apply Filter }
  if GameBoyStyle then
    GameBoy(Background);

  { Init Map 0 }
  CurrentMap := 0;
  InitMap(CurrentMap);

  { Go FullScreen }
  ToggleFullScreen(Self, Self.ClientRect);
end;

procedure TfrmMain.EndGame;
begin
  TileMap.Free;
  CacheBitmap.Free;
  Background.Free;
end;

procedure TfrmMain.InitMap(id: NativeInt);
begin
  { In the calculations here we assume that the tiles are square and the map too }

  TileMap.LoadFromINIFile(FMaps[id]);

  { Custom Size }
  TileMap.Map.TileWidth := frmMain.Height div TileMap.Map.Width;
  TileMap.Map.TileHeight := frmMain.Height div TileMap.Map.Height;
  bgMap.Width := TileMap.Map.TileWidth * TileMap.Map.Width;
  bgMap.Height := TileMap.Map.TileHeight * TileMap.Map.Height;
  CenterControl(bgMap);

  { Apply Filter }
  if GameBoyStyle then
    GameBoy(TileMap.TileSet.Bitmap);
  if GameBoyStyle then
    TileMap.Map.BackgroundColor := GameBoy(TileMap.Map.BackgroundColor);

  { Draw Cache }
  CacheBitmap.SetSize(bgMap.Width, bgMap.Height);
  CacheBitmap.StretchPutImage(Rect(0, 0, CacheBitmap.Width, CacheBitmap.Height),
    Background, dmSet);
  TileMap.DrawMap(CacheBitmap);

  FGravity := (TileMap.Map.TileWidth div TileMap.TileSet.TileWidth) / 8;
  FAccumX := (TileMap.Map.TileWidth div TileMap.TileSet.TileWidth) / 2;
  FAccumY := (TileMap.Map.TileHeight div TileMap.TileSet.TileHeight) / 2;
  FStopX := (TileMap.Map.TileWidth div TileMap.TileSet.TileWidth) / 2;
  FStopY := (TileMap.Map.TileHeight div TileMap.TileSet.TileHeight) / 2;

  RestartPlayer;
end;

procedure TfrmMain.RestartPlayer;
begin
  PlayerPosition := Point(TileMap.Map.TileWidth, TileMap.Map.TileHeight);
  AccX := 0;
  AccY := 0;
  Restart := False;
end;

function TfrmMain.HitWall(AOnlyHitTest: boolean = False): boolean;
var
  x, x1, x2, y, y1, y2, tx, ty, n: NativeInt;
  Player, Wall, Deep: TRectangle;
begin
  Result := False;
  tx := TileMap.Map.TileWidth;
  ty := TileMap.Map.TileHeight;

  { Player Position and Size }
  Player := DoRectangle(PlayerPosition.x, PlayerPosition.y, tx, ty);
  Deep := DoRectangle(x * tx, y * ty + ty div 2, tx, ty div 2);

  { Layer Data ID }
  y1 := BoundInt(Player.y div ty, 0, TileMap.Map.Height - 1);
  y2 := BoundInt((Player.y + Player.Height - 1) div ty, 0, TileMap.Map.Height - 1);

  n := y1 * TileMap.Map.Width;

  x1 := BoundInt(Player.x div tx, 0, TileMap.Map.Width - 1);
  x2 := BoundInt((Player.x + Player.Width - 1) div tx, 0, TileMap.Map.Width - 1);

  for y := y1 to y2 do
  begin
    for x := x1 to x2 do
    begin
      { Wall Position and Size }
      Wall := DoRectangle(x * tx, y * ty, tx, ty);

      { If is not empty space }
      case TileMap.Layers[0].Data[n + x] of
        '1': if not AOnlyHitTest and HitTest(Wall, Player) then
          begin
            CurrentMap := 1;
            InitMap(CurrentMap);
            Exit;
          end;
        '4', '5': if not AOnlyHitTest and HitTest(Deep, Player) then
          begin
            Restart := True;
            Exit;
          end;
        '-1': ;
        else
          if HitTest(Wall, Player) then
            Result := True;
      end;

    end; // x
    n += TileMap.Map.Width;
  end; // y

end;

procedure TfrmMain.PlayBackgroundSound;
begin
  if Sound then
    PlaySoundLoop(0, 'background.ogg', @PlayBackgroundSound);
end;

procedure TfrmMain.PlayRestartSound;
begin
  if Sound then
    PlaySound(1, 'restart.wav');
end;

procedure TfrmMain.InitUOS;
var
  path0, uos1, uos2, uos3, uos4: string;
begin
  Sound := False;

  {$IFDEF Windows}
  path0 := '..\..\sound\examples\lib\Windows\';

  {$if defined(cpu64)}
  uos1 := path0 + '64bit\LibPortaudio-64.dll';
  uos2 := path0 + '64bit\LibSndFile-64.dll';
  uos3 := path0 + '64bit\LibMpg123-64.dll';
  uos4 := path0 + '64bit\libSoundTouch-64.dll';
  {$else}
  uos1 := path0 + '32bit\LibPortaudio-32.dll';
  uos2 := path0 + '32bit\LibSndFile-32.dll';
  uos3 := path0 + '32bit\LibMpg123-32.dll';
  uos4 := path0 + '32bit\libSoundTouch-32.dll';
  {$endif}

  { Load UOS }
  //if uos_loadlib(PChar(uos1), PChar(uos2), PChar(uos3), PChar(uos4)) = 0 then
  //  Sound := True;
  {$ENDIF}
end;

procedure TfrmMain.EndUOS;
begin
  uos_unloadlib();
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
  { Left Arrow }
  p_left := Keys.Values[IntToStr(VK_LEFT)] = 'True';
  { Right Arrow }
  p_right := Keys.Values[IntToStr(VK_RIGHT)] = 'True';
  { Up Arrow }
  p_up := Keys.Values[IntToStr(VK_UP)] = 'True';
  { Down Arrow }
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
    { POV Left }
    27000: p_left := True;
    { POV Right }
    9000: p_right := True;
    { POV Up }
    0: p_up := True;
    { POV Down }
    18000: p_down := True;
    { POV UpLeft }
    31500:
    begin
      p_up := True;
      p_left := True;
    end;
    { POV UpRight }
    4500:
    begin
      p_up := True;
      p_right := True;
    end;
    { POV DownLeft }
    22500:
    begin
      p_down := True;
      p_left := True;
    end;
    { POV DownRight }
    13500:
    begin
      p_down := True;
      p_right := True;
    end;
  end;
end;

procedure TfrmMain.SetFAccX(AValue: real);
begin
  if FAccX = AValue then
    Exit;
  FAccX := AValue;
end;

procedure TfrmMain.SetFAccY(AValue: real);
begin
  if FAccY = AValue then
    Exit;
  FAccY := AValue;
end;

procedure TfrmMain.SetFBackground(AValue: TBGRABitmap);
begin
  if FBackground = AValue then
    Exit;
  FBackground := AValue;
end;

procedure TfrmMain.SetFCacheBitmap(AValue: TBGRABitmap);
begin
  if FCacheBitmap = AValue then
    Exit;
  FCacheBitmap := AValue;
end;

procedure TfrmMain.SetFCurrentMap(AValue: NativeInt);
begin
  if FCurrentMap = AValue then
    Exit;
  FCurrentMap := AValue;
end;

procedure TfrmMain.SetFGravity(AValue: real);
begin
  if FGravity = AValue then
    Exit;
  FGravity := AValue;
end;

procedure TfrmMain.SetFKeys(AValue: TStringList);
begin
  if FKeys = AValue then
    Exit;
  FKeys := AValue;
end;

procedure TfrmMain.SetFRestart(AValue: boolean);
begin
  if FRestart = AValue then
    Exit;
  FRestart := AValue;
end;

procedure TfrmMain.SetFSound(AValue: boolean);
begin
  if FSound = AValue then
    Exit;
  FSound := AValue;
end;

end.
