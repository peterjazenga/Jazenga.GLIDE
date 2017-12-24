unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  BGLVirtualScreen, BGRAOpenGL, BGRABitmapTypes, INIFiles, LCLType, ExtCtrls,
  Types, Math;

type

  { TTileSet }

  TTileSet = class
  private
    FBitmapFile: string;
    FTexture: IBGLTexture;
    FTileHeight: integer;
    FTileWidth: integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(AFileName: string);
    procedure StretchDraw(index: integer; x, y, w, h: integer);
  published
    property TileWidth: integer read FTileWidth;
    property TileHeight: integer read FTileHeight;
    property Texture: IBGLTexture read FTexture;
  end;

  TTileMapLayer = array of array of integer;

  { TTileMap }

  TTileMap = class
  private
    FData: TTileMapLayer;
    FHorizontalTiles: integer;
    FVerticalTiles: integer;
  protected
    procedure LoadData(const s: TStrings);
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(AFileName: string);
    procedure SaveToFile(AFileName: string);
    property Data: TTileMapLayer read FData write FData;
  published
    property HorizontalTiles: integer read FHorizontalTiles;
    property VerticalTiles: integer read FVerticalTiles;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    BGLVirtualScreen1: TBGLVirtualScreen;
    procedure BGLVirtualScreen1Elapse(Sender: TObject; BGLContext: TBGLContext;
      ElapsedMs: integer);
    procedure BGLVirtualScreen1FramesPerSecond(Sender: TObject;
      BGLContext: TBGLContext; FramesPerSecond: integer);
    procedure BGLVirtualScreen1LoadTextures(Sender: TObject;
      BGLContext: TBGLContext);
    procedure BGLVirtualScreen1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure BGLVirtualScreen1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure BGLVirtualScreen1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure BGLVirtualScreen1MouseWheelDown(Sender: TObject;
      Shift: TShiftState; MousePos: TPoint; var Handled: boolean);
    procedure BGLVirtualScreen1MouseWheelUp(Sender: TObject;
      Shift: TShiftState; MousePos: TPoint; var Handled: boolean);
    procedure BGLVirtualScreen1Redraw(Sender: TObject; BGLContext: TBGLContext);
    procedure BGLVirtualScreen1Resize(Sender: TObject);
    procedure BGLVirtualScreen1UnloadTextures(Sender: TObject;
      BGLContext: TBGLContext);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: boolean);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: boolean);
  private
    ElapseAccumulator: integer;
    MyFont: IBGLFont;
    Sprite: TTileSet;
    TileMap: TTileMap;
    Hover: IBGLTexture;
    VisibleHorizontalTiles, VisibleVerticalTiles: integer;
    HorizontalTiles, VerticalTiles: integer;
    TileWidth, TileHeight: integer;
    MouseIsDown: boolean;
    DataLoaded: boolean;
    CenterOffset, Mouse, HoverPos: TPoint;
    CameraOffset: TPointF;
    CurrentTile: integer;
    ScaleFactor: single;
    p_up, p_down, p_left, p_right, m_up, m_down, m_left, m_right: boolean;
    FramesPerSecond, MsPerFrame: integer;
    procedure SwitchTile(x, y: integer);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TTileMap }

procedure TTileMap.LoadData(const s: TStrings);
var
  px, py, i: integer;
begin
  i := 0;
  SetLength(FData, VerticalTiles, HorizontalTiles);
  for py := 0 to VerticalTiles - 1 do
  begin
    for px := 0 to HorizontalTiles - 1 do
    begin
      FData[py, px] := StrToInt(s[i]);
      i := i + 1;
    end;
  end;
end;

constructor TTileMap.Create;
begin

end;

destructor TTileMap.Destroy;
begin
  inherited Destroy;
end;

procedure TTileMap.LoadFromFile(AFileName: string);
var
  ini: TIniFile;
  s: TStringList;
begin
  ini := TIniFile.Create(AFileName);
  FHorizontalTiles := ini.ReadInteger('TileMap', 'HorizontalTiles', 0);
  FVerticalTiles := ini.ReadInteger('TileMap', 'VerticalTiles', 0);

  s := TStringList.Create;
  s.CommaText := ini.ReadString('TileMap', 'Data', '');
  LoadData(s);
  s.Free;

  ini.Free;
end;

procedure TTileMap.SaveToFile(AFileName: string);
var
  ini: TIniFile;
  x, y: integer;
  s: string;
begin
  ini := TIniFile.Create(AFileName);

  ini.WriteInteger('TileMap', 'HorizontalTiles', FHorizontalTiles);
  ini.WriteInteger('TileMap', 'VerticalTiles', VerticalTiles);

  s := '';

  for y := 0 to FVerticalTiles - 1 do
  begin
    for x := 0 to FHorizontalTiles - 1 do
    begin
      s := s + IntToStr(FData[y, x]) + ',';
    end;
  end;

  ini.WriteString('TileMap', 'Data', s);

  ini.Free;
end;

{ TTileSet }

constructor TTileSet.Create;
begin

end;

destructor TTileSet.Destroy;
begin
  FTexture.FreeMemory;
  FTexture := nil;
  inherited Destroy;
end;

procedure TTileSet.LoadFromFile(AFileName: string);
var
  ini: TIniFile;
begin
  ini := TIniFile.Create(AFileName);
  FTileWidth := ini.ReadInteger('TileSet', 'TileWidth', 0);
  FTileHeight := ini.ReadInteger('TileSet', 'TileHeight', 0);
  FBitmapFile := ini.ReadString('TileSet', 'BitmapFile', '');

  if FileExists(FBitmapFile) then
  begin
    FTexture := BGLTexture(FBitmapFile);
    FTexture.SetFrameSize(TileWidth, TileHeight);
    FTexture.ResampleFilter := orfBox;
  end;
  ini.Free;
end;

procedure TTileSet.StretchDraw(index: integer; x, y, w, h: integer);
begin
  Texture.SetFrame(index);
  BGLCanvas.StretchPutImage(x, y, w, h, Texture);
end;

{ TForm1 }

procedure TForm1.BGLVirtualScreen1LoadTextures(Sender: TObject;
  BGLContext: TBGLContext);
var
  bmp: TBGLBitmap;
begin
  Sprite := TTileSet.Create;
  Sprite.LoadFromFile('tileset.ini');
  TileWidth := Sprite.TileWidth;
  TileHeight := Sprite.TileHeight;

  TileMap := TTileMap.Create;
  TileMap.LoadFromFile('tilemap.ini');
  HorizontalTiles := TileMap.HorizontalTiles;
  VerticalTiles := TileMap.VerticalTiles;

  bmp := TBGLBitmap.Create(Sprite.TileWidth, Sprite.TileHeight);
  bmp.Rectangle(0, 0, bmp.Width, bmp.Height, BGRABlack, BGRA(255, 255, 255, 127),
    dmDrawWithTransparency);
  Hover := bmp.MakeTextureAndFree;
  Hover.ResampleFilter := orfBox;

  MyFont := BGLFont('Arial', 20);

  DataLoaded := True;
  BGLVirtualScreen1Resize(Self);
end;

procedure TForm1.BGLVirtualScreen1FramesPerSecond(Sender: TObject;
  BGLContext: TBGLContext; FramesPerSecond: integer);
begin
  Self.FramesPerSecond := FramesPerSecond;
end;

procedure TForm1.BGLVirtualScreen1Elapse(Sender: TObject; BGLContext: TBGLContext;
  ElapsedMs: integer);
var
  mouse_h, mouse_w: integer;
begin
  MsPerFrame := ElapsedMs;

  mouse_h := Min(50, Height div 3);
  mouse_w := Min(50, Width div 3);

  // Left - Right
  m_left := (Mouse.x < mouse_w);
  if m_left then
    m_right := False
  else
    m_right := (Mouse.x > Width - mouse_w);

  // Up - Down
  m_up := (Mouse.y < mouse_h);
  if m_up then
    m_down := False
  else
    m_down := (Mouse.y > Height - mouse_h);

  if p_left or m_left then
    CameraOffset.x := CameraOffset.x + ElapsedMs div 2;

  if p_right or m_right then
    CameraOffset.x := CameraOffset.x - ElapsedMs div 2;

  if p_up or m_up then
    CameraOffset.y := CameraOffset.y + ElapsedMs div 2;

  if p_down or m_down then
    CameraOffset.y := CameraOffset.y - ElapsedMs div 2;

  SwitchTile(Mouse.x, Mouse.y);
end;

procedure TForm1.BGLVirtualScreen1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  MouseIsDown := True;
end;

procedure TForm1.BGLVirtualScreen1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: integer);
begin
  Mouse := Point(X, Y);
end;

procedure TForm1.BGLVirtualScreen1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  MouseIsDown := False;
end;

procedure TForm1.BGLVirtualScreen1MouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: boolean);
begin

end;

procedure TForm1.BGLVirtualScreen1MouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: boolean);
begin

end;

procedure TForm1.BGLVirtualScreen1Redraw(Sender: TObject; BGLContext: TBGLContext);
var
  px, py: integer;
  fps_text: string;
  fps_width: integer;
  offset: TPoint;
begin
  offset := Point(round(CameraOffset.x) + round(CenterOffset.x),
    round(CameraOffset.y) + round(CenterOffset.y));
  for py := 0 to VerticalTiles - 1 do
  begin
    for px := 0 to HorizontalTiles - 1 do
    begin
      Sprite.StretchDraw(TileMap.Data[py, px], offset.x + px * TileWidth,
        offset.y + py * TileHeight, TileWidth, TileHeight);
    end;
  end;
  BGLCanvas.StretchPutImage(HoverPos.x, HoverPos.y, TileWidth, TileHeight, Hover);
  Sprite.StretchDraw(CurrentTile, 0, 0, TileWidth, TileHeight);

  fps_text := IntToStr(FramesPerSecond) + ' FPS, ' + IntToStr(MsPerFrame) + ' ms';
  fps_width := round(MyFont.TextWidth(fps_text));
  MyFont.TextOut(BGLContext.Width - fps_width + 1, 1, fps_text, BGRABlack);
  MyFont.TextOut(BGLContext.Width - fps_width + 0, 0, fps_text, BGRAWhite);
end;

procedure TForm1.BGLVirtualScreen1Resize(Sender: TObject);
begin
  if not DataLoaded then
    exit;

  TileWidth := trunc(Width * scalefactor) div VisibleHorizontalTiles;
  TileHeight := trunc(Height * scalefactor) div VisibleVerticalTiles;

  // Height based
  if TileWidth < TileHeight then
    TileHeight := TileWidth
  else
    TileWidth := TileHeight;

  CenterOffset.x := (Width - (TileWidth * HorizontalTiles)) div 2;
  CenterOffset.y := (Height - (TileHeight * VerticalTiles)) div 2;
end;

procedure TForm1.BGLVirtualScreen1UnloadTextures(Sender: TObject;
  BGLContext: TBGLContext);
begin
  MyFont := nil;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  TileMap.SaveToFile('tilemap.ini');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  HoverPos := Point(0, 0);
  ScaleFactor := 1;
  p_up := False;
  p_down := False;
  p_left := False;
  p_right := False;
  CameraOffset := PointF(0, 0);
  VisibleHorizontalTiles := 32;
  VisibleVerticalTiles := 18;
  CurrentTile := 1;
  DataLoaded := False;
  MouseIsDown := False;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Sprite.Free;
  TileMap.Free;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  case key of
    vk_Escape: Self.Close;
    vk_left, vk_a: p_left := True;
    vk_right, vk_d: p_right := True;
    vk_up, vk_w: p_up := True;
    vk_down, vk_s: p_down := True;
    vk_space: CameraOffset := PointF(0, 0);
    vk_0, vk_numpad0: if ssCtrl in Shift then
      begin
        ScaleFactor := 1;
        BGLVirtualScreen1Resize(Self);
      end;
  end;
end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  case key of
    vk_left, vk_a: p_left := False;
    vk_right, vk_d: p_right := False;
    vk_up, vk_w: p_up := False;
    vk_down, vk_s: p_down := False;
  end;
end;

procedure TForm1.FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: boolean);
begin
  if ssCtrl in Shift then
  begin
    ScaleFactor := ScaleFactor - 0.25;
    if ScaleFactor < 0.25 then
      ScaleFactor := 0.25;
    BGLVirtualScreen1Resize(Self);
  end
  else
  begin
    Dec(CurrentTile);
    if CurrentTile < 1 then
      CurrentTile := 1;
    Caption := IntToStr(CurrentTile);
  end;
  Handled := True;
end;

procedure TForm1.FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: boolean);
begin
  if ssCtrl in Shift then
  begin
    ScaleFactor := ScaleFactor + 0.25;
    if ScaleFactor > 4 then
      ScaleFactor := 4;
    BGLVirtualScreen1Resize(Self);
  end
  else
  begin
    Inc(CurrentTile);
    if CurrentTile > Sprite.Texture.FrameCount then
      CurrentTile := Sprite.Texture.FrameCount;
    Caption := IntToStr(CurrentTile);
  end;
  Handled := True;
end;

procedure TForm1.SwitchTile(x, y: integer);
var
  TileX, TileY: integer;
begin
  x := x - CenterOffset.x - round(CameraOffset.x);
  y := y - CenterOffset.y - round(CameraOffset.y);

  TileX := x div TileWidth;
  TileY := y div TileHeight;

  if (TileX < HorizontalTiles) and (TileY < VerticalTiles) and
    (TileX >= 0) and (TileY >= 0) then
  begin
    HoverPos := Point((TileX * TileWidth) + CenterOffset.x +
      round(CameraOffset.x), (TileY * TileHeight) + CenterOffset.y +
      round(CameraOffset.y));

    if not MouseIsDown then
      exit;
    TileMap.Data[TileY, TileX] := CurrentTile;
  end
  else
    HoverPos := Point(-TileWidth, -TileHeight);
end;

end.
