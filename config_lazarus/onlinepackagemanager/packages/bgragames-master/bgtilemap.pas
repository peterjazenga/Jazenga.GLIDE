unit bgTileMap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, IniFiles,
  BGRABitmap, BGRABitmapTypes;

type

  { TBGMap }

  TBGMap = class
  private
    FWidth: NativeInt;
    FHeight: NativeInt;
    FTileWidth: NativeInt;
    FTileHeight: NativeInt;
    FLayerCount: NativeInt;
    FBackgroundColor: TBGRAPixel;
    procedure SetFBackgroundColor(AValue: TBGRAPixel);
    procedure SetFHeight(AValue: NativeInt);
    procedure SetFLayerCount(AValue: NativeInt);
    procedure SetFTileHeight(AValue: NativeInt);
    procedure SetFTileWidth(AValue: NativeInt);
    procedure SetFWidth(AValue: NativeInt);
  public
    constructor Create;
    constructor Create(MemIniFile: TMemIniFile);
    destructor Destroy; override;
  public
    procedure LoadFromINIFile(MemIniFile: TMemIniFile);
    procedure SaveToINIFile(MemIniFile: TMemIniFile);
  public
    property BackgroundColor: TBGRAPixel read FBackgroundColor write SetFBackgroundColor;
  published
    property Width: NativeInt read FWidth write SetFWidth;
    property Height: NativeInt read FHeight write SetFHeight;
    property TileWidth: NativeInt read FTileWidth write SetFTileWidth;
    property TileHeight: NativeInt read FTileHeight write SetFTileHeight;
    property LayerCount: NativeInt read FLayerCount write SetFLayerCount;
  end;

  { TBGTileSet }

  TBGTileSet = class
  private
    FBitmap: TBGRABitmap;
    FSource: string;
    FTileWidth: NativeInt;
    FTileHeight: NativeInt;
    procedure SetFBitmap(AValue: TBGRABitmap);
    procedure SetFSource(AValue: string);
    procedure SetFTileHeight(AValue: NativeInt);
    procedure SetFTileWidth(AValue: NativeInt);
  public
    constructor Create;
    constructor Create(MemIniFile: TMemIniFile);
    destructor Destroy; override;
  public
    procedure LoadFromINIFile(MemIniFile: TMemIniFile);
    procedure SaveToINIFile(MemIniFile: TMemIniFile);
  public
    property Bitmap: TBGRABitmap read FBitmap write SetFBitmap;
  published
    property Source: string read FSource write SetFSource;
    property TileWidth: NativeInt read FTileWidth write SetFTileWidth;
    property TileHeight: NativeInt read FTileHeight write SetFTileHeight;
  end;

  { TBGLayer }

  TBGLayer = class
  private
    FOpacity: real;
    FVisible: boolean;
    FData: TStringList;
    procedure SetFData(AValue: TStringList);
    procedure SetFOpacity(AValue: real);
    procedure SetFVisible(AValue: boolean);
  public
    constructor Create;
    constructor Create(MemIniFile: TMemIniFile; Index: NativeInt);
    destructor Destroy; override;
  public
    procedure LoadFromINIFile(MemIniFile: TMemIniFile; Index: NativeInt);
    procedure SaveToINIFile(MemIniFile: TMemIniFile; Index: NativeInt);
  published
    property Opacity: real read FOpacity write SetFOpacity;
    property Visible: boolean read FVisible write SetFVisible;
    property Data: TStringList read FData write SetFData;
  end;

  TBGLayers = array of TBGLayer;
  TRects = array of TRect;

  { TBGTileMap }

  TBGTileMap = class
  private
    FMap: TBGMap;
    FTileSet: TBGTileSet;
    FLayers: TBGLayers;
    FRects: TRects;
    procedure SetFLayers(AValue: TBGLayers);
    procedure SetFMap(AValue: TBGMap);
    procedure SetFTileSet(AValue: TBGTileSet);
    procedure InitFRects;
  public
    constructor Create;
    constructor Create(FileName: string);
    destructor Destroy; override;
  public
    procedure LoadFromINIFile(FileName: string);
    procedure SaveToINIFile(FileName: string);
  public
    procedure DrawMap(Bitmap: TBGRABitmap);
    procedure DrawTile(Bitmap: TBGRABitmap; x, y, id: NativeInt; opacity: byte);
  published
    property Map: TBGMap read FMap write SetFMap;
    property TileSet: TBGTileSet read FTileSet write SetFTileSet;
    property Layers: TBGLayers read FLayers write SetFLayers;
  end;

implementation

{ TBGTileMap }

procedure TBGTileMap.SetFLayers(AValue: TBGLayers);
begin
  if FLayers = AValue then
    Exit;
  FLayers := AValue;
end;

procedure TBGTileMap.SetFMap(AValue: TBGMap);
begin
  if FMap = AValue then
    Exit;
  FMap := AValue;
end;

procedure TBGTileMap.SetFTileSet(AValue: TBGTileSet);
begin
  if FTileSet = AValue then
    Exit;
  FTileSet := AValue;
end;

procedure TBGTileMap.InitFRects;
var
  x, y, n, tw, th: NativeInt;
begin
  tw := FTileSet.Bitmap.Width div FTileSet.FTileWidth;
  th := FTileSet.Bitmap.Height div FTileSet.FTileHeight;
  SetLength(FRects, tw * th);
  n := 0;
  for y := 0 to th - 1 do
  begin
    for x := 0 to tw - 1 do
    begin
      FRects[n] := Rect(x * FTileSet.TileWidth, y * FTileSet.TileHeight,
        x * FTileSet.TileWidth + FTileSet.TileWidth, y * FTileSet.TileHeight +
        FTileSet.TileHeight);
      Inc(n);
    end;
  end;
end;

constructor TBGTileMap.Create;
begin
  inherited Create;
  FMap := TBGMap.Create;
  FTileSet := TBGTileSet.Create;
end;

constructor TBGTileMap.Create(FileName: string);
begin
  Create;
  LoadFromINIFile(FileName);
  InitFRects;
end;

destructor TBGTileMap.Destroy;
var
  i: NativeInt;
begin
  FMap.Free;
  FTileSet.Free;

  for i := 0 to High(FLayers) do
    if FLayers[i] <> nil then
      FLayers[i].Free;

  inherited Destroy;
end;

procedure TBGTileMap.LoadFromINIFile(FileName: string);
var
  ini: TMemIniFile;
  i: NativeInt;
begin
  ini := TMemIniFile.Create(FileName);

  FMap.LoadFromINIFile(ini);
  FTileSet.LoadFromINIFile(ini);

  if FMap.LayerCount <> 0 then
  begin
    SetLength(FLayers, FMap.LayerCount);
    for i := 0 to FMap.LayerCount - 1 do
      FLayers[i] := TBGLayer.Create(ini, i);
  end;

  InitFRects;

  ini.Free;
end;

procedure TBGTileMap.SaveToINIFile(FileName: string);
var
  ini: TMemIniFile;
  i: NativeInt;
begin
  ini := TMemIniFile.Create(FileName);

  FMap.SaveToINIFile(ini);
  FTileSet.SaveToINIFile(ini);

  for i := 0 to High(FLayers) do
    FLayers[i].SaveToINIFile(ini, i);

  ini.UpdateFile;
  ini.Free;
end;

procedure TBGTileMap.DrawMap(Bitmap: TBGRABitmap);
var
  x, y, z, n, id: NativeInt;
  opacity: byte;
begin
  if FMap.BackgroundColor.alpha <> 0 then
    Bitmap.Fill(FMap.BackgroundColor);

  for z := 0 to High(FLayers) do
  begin
    if FLayers[z].Visible then
    begin
      opacity := round(255 * FLayers[z].Opacity);
      n := 0;
      for y := 0 to FMap.Height - 1 do
      begin
        for x := 0 to FMap.Width - 1 do
        begin
          {$ifdef cpu64}
          id := StrToInt64(FLayers[z].Data[n]);
          {$else}
          id := StrToInt(FLayers[z].Data[n]);
          {$endif}
          if id <> -1 then
            DrawTile(Bitmap, x * FMap.TileWidth, y * FMap.TileHeight, id, opacity);
          Inc(n);
        end; // x
      end; // y
    end; // layers[z] visible
  end; // layers
end;

procedure TBGTileMap.DrawTile(Bitmap: TBGRABitmap; x, y, id: NativeInt; opacity: byte);
var
  oldClip, dest, fullDest: TRect;
begin
  oldClip := Bitmap.ClipRect;
  dest := RectWithSize(x, y, FMap.TileWidth, FMap.TileHeight);
  if not IntersectRect(dest, dest, oldClip) then
    exit;
  Bitmap.ClipRect := dest;
  fullDest := RectWithSize(x - FRects[id].Left * FMap.TileWidth div
    FTileSet.TileWidth, y - FRects[id].Top * FMap.TileHeight div
    FTileSet.TileHeight, FTileSet.Bitmap.Width * FMap.TileWidth div
    FTileSet.TileWidth, FTileSet.Bitmap.Height * FMap.TileHeight div
    FTileSet.TileHeight);
  Bitmap.StretchPutImage(fullDest, FTileSet.Bitmap, dmDrawWithTransparency, opacity);
  Bitmap.ClipRect := oldClip;
end;

{ TBGLayer }

procedure TBGLayer.SetFData(AValue: TStringList);
begin
  if FData = AValue then
    Exit;
  FData := AValue;
end;

procedure TBGLayer.SetFOpacity(AValue: real);
begin
  if FOpacity = AValue then
    Exit;
  FOpacity := AValue;
end;

procedure TBGLayer.SetFVisible(AValue: boolean);
begin
  if FVisible = AValue then
    Exit;
  FVisible := AValue;
end;

constructor TBGLayer.Create;
begin
  inherited Create;
  FData := TStringList.Create;
end;

constructor TBGLayer.Create(MemIniFile: TMemIniFile; Index: NativeInt);
begin
  Create;
  LoadFromINIFile(MemIniFile, Index);
end;

destructor TBGLayer.Destroy;
begin
  FData.Free;
  inherited Destroy;
end;

procedure TBGLayer.LoadFromINIFile(MemIniFile: TMemIniFile; Index: NativeInt);
begin
  FOpacity := MemIniFile.ReadFloat('Layer' + IntToStr(Index), 'Opacity', 1);
  FVisible := MemIniFile.ReadBool('Layer' + IntToStr(Index), 'Visible', True);
  FData.CommaText := MemIniFile.ReadString('Layer' + IntToStr(Index), 'Data', '');
end;

procedure TBGLayer.SaveToINIFile(MemIniFile: TMemIniFile; Index: NativeInt);
begin
  MemIniFile.WriteFloat('Layer' + IntToStr(Index), 'Opacity', FOpacity);
  MemIniFile.WriteBool('Layer' + IntToStr(Index), 'Visible', FVisible);
  MemIniFile.WriteString('Layer' + IntToStr(Index), 'Data', FData.CommaText);
end;

{ TBGTileSet }

procedure TBGTileSet.SetFBitmap(AValue: TBGRABitmap);
begin
  if FBitmap = AValue then
    Exit;
  FBitmap := AValue;
end;

procedure TBGTileSet.SetFSource(AValue: string);
begin
  if FSource = AValue then
    Exit;
  FSource := AValue;
end;

procedure TBGTileSet.SetFTileHeight(AValue: NativeInt);
begin
  if FTileHeight = AValue then
    Exit;
  FTileHeight := AValue;
end;

procedure TBGTileSet.SetFTileWidth(AValue: NativeInt);
begin
  if FTileWidth = AValue then
    Exit;
  FTileWidth := AValue;
end;

constructor TBGTileSet.Create;
begin
  inherited Create;
  FBitmap := TBGRABitmap.Create;
end;

constructor TBGTileSet.Create(MemIniFile: TMemIniFile);
begin
  Create;
  LoadFromINIFile(MemIniFile);
end;

destructor TBGTileSet.Destroy;
begin
  FBitmap.Free;
  inherited Destroy;
end;

procedure TBGTileSet.LoadFromINIFile(MemIniFile: TMemIniFile);
begin
  FSource := MemIniFile.ReadString('TileSet', 'Source', '');
  FBitmap.LoadFromFile(FSource);
  {$ifdef cpu64}
  FTileWidth := MemIniFile.ReadInt64('TileSet', 'TileWidth', 0);
  FTileHeight := MemIniFile.ReadInt64('TileSet', 'TileHeight', 0);
  {$else}
  FTileWidth := MemIniFile.ReadInteger('TileSet', 'TileWidth', 0);
  FTileHeight := MemIniFile.ReadInteger('TileSet', 'TileHeight', 0);
  {$endif}
end;

procedure TBGTileSet.SaveToINIFile(MemIniFile: TMemIniFile);
begin
  MemIniFile.WriteString('TileSet', 'Source', FSource);
  {$ifdef cpu64}
  MemIniFile.WriteInt64('TileSet', 'TileWidth', FTileWidth);
  MemIniFile.WriteInt64('TileSet', 'TileHeight', FTileHeight);
  {$else}
  MemIniFile.WriteInteger('TileSet', 'TileWidth', FTileWidth);
  MemIniFile.WriteInteger('TileSet', 'TileHeight', FTileHeight);
  {$endif}
end;

{ TBGMap }

procedure TBGMap.SetFBackgroundColor(AValue: TBGRAPixel);
begin
  if FBackgroundColor = AValue then
    Exit;
  FBackgroundColor := AValue;
end;

procedure TBGMap.SetFHeight(AValue: NativeInt);
begin
  if FHeight = AValue then
    Exit;
  FHeight := AValue;
end;

procedure TBGMap.SetFLayerCount(AValue: NativeInt);
begin
  if FLayerCount = AValue then
    Exit;
  FLayerCount := AValue;
end;

procedure TBGMap.SetFTileHeight(AValue: NativeInt);
begin
  if FTileHeight = AValue then
    Exit;
  FTileHeight := AValue;
end;

procedure TBGMap.SetFTileWidth(AValue: NativeInt);
begin
  if FTileWidth = AValue then
    Exit;
  FTileWidth := AValue;
end;

procedure TBGMap.SetFWidth(AValue: NativeInt);
begin
  if FWidth = AValue then
    Exit;
  FWidth := AValue;
end;

constructor TBGMap.Create;
begin
  inherited Create;
end;

constructor TBGMap.Create(MemIniFile: TMemIniFile);
begin
  Create;
  LoadFromINIFile(MemIniFile);
end;

destructor TBGMap.Destroy;
begin
  inherited Destroy;
end;

procedure TBGMap.LoadFromINIFile(MemIniFile: TMemIniFile);
begin
  FBackgroundColor := StrToBGRA(
    MemIniFile.ReadString('Map', 'BackgroundColor', 'rgba(0,0,0,255)'), BGRABlack);
  {$ifdef cpu64}
  FWidth := MemIniFile.ReadInt64('Map', 'Width', 0);
  FHeight := MemIniFile.ReadInt64('Map', 'Height', 0);
  FTileWidth := MemIniFile.ReadInt64('Map', 'TileWidth', 0);
  FTileHeight := MemIniFile.ReadInt64('Map', 'TileHeight', 0);
  FLayerCount := MemIniFile.ReadInt64('Map', 'LayerCount', 0);
  {$else}
  FWidth := MemIniFile.ReadInteger('Map', 'Width', 0);
  FHeight := MemIniFile.ReadInteger('Map', 'Height', 0);
  FTileWidth := MemIniFile.ReadInteger('Map', 'TileWidth', 0);
  FTileHeight := MemIniFile.ReadInteger('Map', 'TileHeight', 0);
  FLayerCount := MemIniFile.ReadInteger('Map', 'LayerCount', 0);
  {$endif}
end;

procedure TBGMap.SaveToINIFile(MemIniFile: TMemIniFile);
begin
  MemIniFile.WriteString('Map', 'BackgroundColor', BGRAToStr(FBackgroundColor));
  {$ifdef cpu64}
  MemIniFile.WriteInt64('Map', 'Width', FWidth);
  MemIniFile.WriteInt64('Map', 'Height', FHeight);
  MemIniFile.WriteInt64('Map', 'TileWidth', FTileWidth);
  MemIniFile.WriteInt64('Map', 'TileHeight', FTileHeight);
  MemIniFile.WriteInt64('Map', 'LayerCount', FLayerCount);
  {$else}
  MemIniFile.WriteInteger('Map', 'Width', FWidth);
  MemIniFile.WriteInteger('Map', 'Height', FHeight);
  MemIniFile.WriteInteger('Map', 'TileWidth', FTileWidth);
  MemIniFile.WriteInteger('Map', 'TileHeight', FTileHeight);
  MemIniFile.WriteInteger('Map', 'LayerCount', FLayerCount);
  {$endif}
end;

initialization
  { Change DecimalSeparator to a single dot, needed for streaming Float values }
  DecimalSeparator := '.';

end.
