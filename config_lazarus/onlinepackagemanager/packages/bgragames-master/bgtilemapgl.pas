unit bgTileMapGL;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, IniFiles,
  BGRABitmap, BGRABitmapTypes, BGRAOpenGL;

type

  { TBGMapGL }

  TBGMapGL = class
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

  { TBGTileSetGL }

  TBGTileSetGL = class
  private
    FBitmap: IBGLTexture;
    FSource: string;
    FTileWidth: NativeInt;
    FTileHeight: NativeInt;
    procedure SetFBitmap(AValue: IBGLTexture);
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
    property Bitmap: IBGLTexture read FBitmap write SetFBitmap;
  published
    property Source: string read FSource write SetFSource;
    property TileWidth: NativeInt read FTileWidth write SetFTileWidth;
    property TileHeight: NativeInt read FTileHeight write SetFTileHeight;
  end;

  { TBGLayerGL }

  TBGLayerGL = class
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

  TBGLayersGL = array of TBGLayerGL;
  TRects = array of TRect;

  { TBGTileMapGL }

  TBGTileMapGL = class
  private
    FMap: TBGMapGL;
    FTileSet: TBGTileSetGL;
    FLayers: TBGLayersGL;
    procedure SetFLayers(AValue: TBGLayersGL);
    procedure SetFMap(AValue: TBGMapGL);
    procedure SetFTileSet(AValue: TBGTileSetGL);
  public
    constructor Create;
    constructor Create(FileName: string);
    destructor Destroy; override;
  public
    procedure LoadFromINIFile(FileName: string);
    procedure SaveToINIFile(FileName: string);
  public
    procedure DrawMap(Width, Height: integer);
    procedure DrawTile(x, y, id: NativeInt; opacity: byte);
  published
    property Map: TBGMapGL read FMap write SetFMap;
    property TileSet: TBGTileSetGL read FTileSet write SetFTileSet;
    property Layers: TBGLayersGL read FLayers write SetFLayers;
  end;

implementation

{ TBGTileMapGL }

procedure TBGTileMapGL.SetFLayers(AValue: TBGLayersGL);
begin
  if FLayers = AValue then
    Exit;
  FLayers := AValue;
end;

procedure TBGTileMapGL.SetFMap(AValue: TBGMapGL);
begin
  if FMap = AValue then
    Exit;
  FMap := AValue;
end;

procedure TBGTileMapGL.SetFTileSet(AValue: TBGTileSetGL);
begin
  if FTileSet = AValue then
    Exit;
  FTileSet := AValue;
end;

constructor TBGTileMapGL.Create;
begin
  inherited Create;
  FMap := TBGMapGL.Create;
  FTileSet := TBGTileSetGL.Create;
end;

constructor TBGTileMapGL.Create(FileName: string);
begin
  Create;
  LoadFromINIFile(FileName);
end;

destructor TBGTileMapGL.Destroy;
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

procedure TBGTileMapGL.LoadFromINIFile(FileName: string);
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
      FLayers[i] := TBGLayerGL.Create(ini, i);
  end;

  FTileSet.Bitmap.SetFrameSize(FTileSet.TileWidth, FTileSet.TileHeight);

  ini.Free;
end;

procedure TBGTileMapGL.SaveToINIFile(FileName: string);
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

procedure TBGTileMapGL.DrawMap(Width, Height: integer);
var
  x, y, z, n, id: NativeInt;
  opacity: byte;
begin
  BGLViewPort(Width, Height, FMap.BackgroundColor);

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
            DrawTile(x * FMap.TileWidth, y * FMap.TileHeight, id, opacity);
          Inc(n);
        end; // x
      end; // y
    end; // layers[z] visible
  end; // layers
end;

procedure TBGTileMapGL.DrawTile(x, y, id: NativeInt; opacity: byte);
begin
  FTileSet.Bitmap.SetFrame(id + 1);
  FTileSet.Bitmap.ResampleFilter := orfBox;
  FTileSet.Bitmap.StretchDraw(x, y, FMap.TileWidth, FMap.TileHeight, opacity);
end;

{ TBGLayerGL }

procedure TBGLayerGL.SetFData(AValue: TStringList);
begin
  if FData = AValue then
    Exit;
  FData := AValue;
end;

procedure TBGLayerGL.SetFOpacity(AValue: real);
begin
  if FOpacity = AValue then
    Exit;
  FOpacity := AValue;
end;

procedure TBGLayerGL.SetFVisible(AValue: boolean);
begin
  if FVisible = AValue then
    Exit;
  FVisible := AValue;
end;

constructor TBGLayerGL.Create;
begin
  inherited Create;
  FData := TStringList.Create;
end;

constructor TBGLayerGL.Create(MemIniFile: TMemIniFile; Index: NativeInt);
begin
  Create;
  LoadFromINIFile(MemIniFile, Index);
end;

destructor TBGLayerGL.Destroy;
begin
  FData.Free;
  inherited Destroy;
end;

procedure TBGLayerGL.LoadFromINIFile(MemIniFile: TMemIniFile; Index: NativeInt);
begin
  FOpacity := MemIniFile.ReadFloat('Layer' + IntToStr(Index), 'Opacity', 1);
  FVisible := MemIniFile.ReadBool('Layer' + IntToStr(Index), 'Visible', True);
  FData.CommaText := MemIniFile.ReadString('Layer' + IntToStr(Index), 'Data', '');
end;

procedure TBGLayerGL.SaveToINIFile(MemIniFile: TMemIniFile; Index: NativeInt);
begin
  MemIniFile.WriteFloat('Layer' + IntToStr(Index), 'Opacity', FOpacity);
  MemIniFile.WriteBool('Layer' + IntToStr(Index), 'Visible', FVisible);
  MemIniFile.WriteString('Layer' + IntToStr(Index), 'Data', FData.CommaText);
end;

{ TBGTileSetGL }

procedure TBGTileSetGL.SetFBitmap(AValue: IBGLTexture);
begin
  if FBitmap = AValue then
    Exit;
  FBitmap := AValue;
end;

procedure TBGTileSetGL.SetFSource(AValue: string);
begin
  if FSource = AValue then
    Exit;
  FSource := AValue;
end;

procedure TBGTileSetGL.SetFTileHeight(AValue: NativeInt);
begin
  if FTileHeight = AValue then
    Exit;
  FTileHeight := AValue;
end;

procedure TBGTileSetGL.SetFTileWidth(AValue: NativeInt);
begin
  if FTileWidth = AValue then
    Exit;
  FTileWidth := AValue;
end;

constructor TBGTileSetGL.Create;
begin
  inherited Create;
end;

constructor TBGTileSetGL.Create(MemIniFile: TMemIniFile);
begin
  Create;
  LoadFromINIFile(MemIniFile);
end;

destructor TBGTileSetGL.Destroy;
begin
  inherited Destroy;
end;

procedure TBGTileSetGL.LoadFromINIFile(MemIniFile: TMemIniFile);
begin
  FSource := MemIniFile.ReadString('TileSet', 'Source', '');
  FBitmap := BGLTexture(FSource);
  {$ifdef cpu64}
  FTileWidth := MemIniFile.ReadInt64('TileSet', 'TileWidth', 0);
  FTileHeight := MemIniFile.ReadInt64('TileSet', 'TileHeight', 0);
  {$else}
  FTileWidth := MemIniFile.ReadInteger('TileSet', 'TileWidth', 0);
  FTileHeight := MemIniFile.ReadInteger('TileSet', 'TileHeight', 0);
  {$endif}
end;

procedure TBGTileSetGL.SaveToINIFile(MemIniFile: TMemIniFile);
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

{ TBGMapGL }

procedure TBGMapGL.SetFBackgroundColor(AValue: TBGRAPixel);
begin
  if FBackgroundColor = AValue then
    Exit;
  FBackgroundColor := AValue;
end;

procedure TBGMapGL.SetFHeight(AValue: NativeInt);
begin
  if FHeight = AValue then
    Exit;
  FHeight := AValue;
end;

procedure TBGMapGL.SetFLayerCount(AValue: NativeInt);
begin
  if FLayerCount = AValue then
    Exit;
  FLayerCount := AValue;
end;

procedure TBGMapGL.SetFTileHeight(AValue: NativeInt);
begin
  if FTileHeight = AValue then
    Exit;
  FTileHeight := AValue;
end;

procedure TBGMapGL.SetFTileWidth(AValue: NativeInt);
begin
  if FTileWidth = AValue then
    Exit;
  FTileWidth := AValue;
end;

procedure TBGMapGL.SetFWidth(AValue: NativeInt);
begin
  if FWidth = AValue then
    Exit;
  FWidth := AValue;
end;

constructor TBGMapGL.Create;
begin
  inherited Create;
end;

constructor TBGMapGL.Create(MemIniFile: TMemIniFile);
begin
  Create;
  LoadFromINIFile(MemIniFile);
end;

destructor TBGMapGL.Destroy;
begin
  inherited Destroy;
end;

procedure TBGMapGL.LoadFromINIFile(MemIniFile: TMemIniFile);
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

procedure TBGMapGL.SaveToINIFile(MemIniFile: TMemIniFile);
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
