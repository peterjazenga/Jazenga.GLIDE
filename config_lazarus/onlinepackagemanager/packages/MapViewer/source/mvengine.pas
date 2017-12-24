{
  (c) 2014 ti_dic
  Parts of this component are based on :
    Map Viewer Copyright (C) 2011 Maciej Kaczkowski / keit.co

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}

//=== ct9999 Modify for CodeTyphon Studio ================

unit mvEngine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,mvJobQueue,mvmapprovider,mvDownloadEngine,IntfGraphics,
  Math,mvJobs,forms,mvgpsobj,
  mvCache,mvdragobj,controls,mvtypes;

const
  EARTH_RADIUS = 6378137;
  MIN_LATITUDE = -85.05112878;
  MAX_LATITUDE = 85.05112878;
  MIN_LONGITUDE = -180;
  MAX_LONGITUDE = 180;
  SHIFT = 2 * pi * EARTH_RADIUS / 2.0;


Type
  TDrawTileEvent = Procedure (const TileId : TTileId;X,Y : integer;TileImg : TLazIntfImage) of object;

  TTileIdArray = Array of TTileId;

  { TMapWindow }
  TMapWindow = Record
    MapProvider: TMapProvider;
    X: Int64;
    Y: Int64;
    Center : TRealPoint;
    Zoom: integer;
    Height: integer;
    Width: integer;
  end;


  { TMapViewerEngine }

  TMapViewerEngine = Class(TComponent)
    private
      DragObj : TDragObj;
      Cache : TPictureCache;
      FActive: boolean;
      FDownloadEngine: TCustomDownloadEngine;
      FDrawTitleInGuiThread: boolean;
      FOnCenterMove: TNotifyEvent;
      FOnChange: TNotifyEvent;
      FOnDrawTile: TDrawTileEvent;
      FOnZoomChange: TNotifyEvent;
      lstProvider : TStringList;
      Queue : TJobQueue;
      MapWin : TMapWindow;
      procedure ConstraintZoom(var aWin: TMapWindow);
      function GetCacheOnDisk: Boolean;
      function GetCachePath: String;
      function GetCenter: TRealPoint;
      function GetHeight: integer;
      function GetMapProvider: String;
      function GetUseThreads: Boolean;
      function GetWidth: integer;
      function GetZoom: integer;
      function IsValidTile(const aWin: TMapWindow; const aTile: TTIleId
        ): boolean;
      procedure MoveMapCenter(Sender: TDragObj);
      procedure SetActive(AValue: boolean);
      procedure SetCacheOnDisk(AValue: Boolean);
      procedure SetCachePath(AValue: String);
      procedure SetDownloadEngine(AValue: TCustomDownloadEngine);
      procedure SetHeight(AValue: integer);
      procedure SetMapProvider(AValue: String);
      procedure SetUseThreads(AValue: Boolean);
      procedure SetWidth(AValue: integer);
      procedure SetZoom(AValue: integer);
      function LonLatToMapWin(const aWin: TMapWindow; aPt: TRealPoint): TPoint;
      Function MapWinToLonLat(const aWin: TMapWindow; aPt : TPoint) : TRealPoint;
      Procedure CalculateWin(var aWin : TMapWindow);
      Procedure Redraw(const aWin : TmapWindow);
      function CalculateVisibleTiles(const aWin : TMapWindow) : TArea;
      function IsCurrentWin(const aWin : TMapWindow) : boolean;
    protected
      function GetTileName(const Id : TTileId) : String;
      procedure evDownload(Data : TObject;Job : TJob);
      procedure TileDownloaded(Data: PtrInt);
      Procedure RegisterProviders;
      Procedure DrawTile(const TileId : TTileId;X,Y : integer;TileImg : TLazIntfImage);

      function GetLetterSvr(id: integer): String;
      function GetYahooSvr(id: integer): String;
      function GetYahooY(const Tile : TTileId): string;
      function GetYahooZ(const Tile : TTileId): string;
      function GetQuadKey(const Tile : TTileId): string;

      Procedure DoDrag(Sender : TDragObj);
    public
      Procedure CancelCurrentDrawing;
      Procedure Redraw;
      function AddMapProvider(OpeName: String; Url: String;   MinZoom : integer;MaxZoom : integer;NbSvr: integer; GetSvrStr: TGetSvrStr =nil; GetXStr: TGetValStr =nil; GetYStr: TGetValStr =nil; GetZStr: TGetValStr =nil) :  TMapProvider;
      Procedure GetMapProviders(lst : TStrings);

      Constructor Create(aOwner : TComponent);override;
      destructor Destroy;override;
      Function ScreenToLonLat(aPt : TPoint) : TRealPoint;
      Function LonLatToScreen(aPt : TRealPoint) : TPoint;
      Function WorldScreenToLonLat(aPt : TPoint) : TRealPoint;
      Function LonLatToWorldScreen(aPt : TRealPoint) : TPoint;

      Procedure SetSize(aWidth,aHeight : integer);

      Procedure MouseDown(Sender: TObject; Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
      Procedure MouseUp(Sender: TObject; Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
      Procedure MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
      Procedure DblClick(Sender: TObject);
      Procedure MouseWheel(Sender: TObject; Shift: TShiftState;WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
      Procedure SetCenter(aCenter : TRealPoint);
      Procedure ZoomOnArea(const aArea : TRealArea);


      property Center : TRealPoint read GetCenter write SetCenter;
      property Zoom : integer read GetZoom write SetZoom;
      property Width : integer read GetWidth write SetWidth;
      property Height : integer read GetHeight write SetHeight;
      property UseThreads : Boolean read GetUseThreads write SetUseThreads;
      property MapProvider : String read GetMapProvider write SetMapProvider;
      property DownloadEngine : TCustomDownloadEngine read FDownloadEngine write SetDownloadEngine;
      property CacheOnDisk : Boolean read GetCacheOnDisk write SetCacheOnDisk;
      property CachePath : String read GetCachePath write SetCachePath;
      property Active : boolean read FActive write SetActive;

      property OnDrawTile :TDrawTileEvent read FOnDrawTile write FOnDrawTile;
      property DrawTitleInGuiThread : boolean read FDrawTitleInGuiThread write FDrawTitleInGuiThread;
      property OnCenterMove : TNotifyEvent read FOnCenterMove write FOnCenterMove;
      property OnZoomChange : TNotifyEvent read FOnZoomChange write FOnZoomChange;
      property Jobqueue : TJobQueue read Queue;
      property OnChange : TNotifyEvent Read FOnChange write FOnchange; //called when visiable area change
  End;

implementation

Type

  { TLaunchDownloadJob }

  TLaunchDownloadJob = Class(TJob)
  private
    AllRun : boolean;
    Win : TMapWindow;
    Engine : TMapViewerEngine;
    FRunning : boolean;
    FTiles : TTileIdArray;
    FStates : Array of integer;
  protected
    function pGetTask : integer;override;
    procedure pTaskStarted(aTask: integer);override;
    procedure pTaskEnded(aTask : integer;aExcept : Exception);override;
  public
    procedure ExecuteTask(aTask : integer;FromWaiting : boolean);override;
    function Running : boolean;override;
  public
    Constructor Create(Eng : TMapViewerEngine;const Tiles : TTileIdArray;const aWin : TMapWindow);
  end;


{ TEnvTile }

 TEnvTile = Class
     private
       Tile : TTileId;
       Win : TMapWindow;
     public
       constructor Create(const aTile : TTileId;Const aWin : TMapWindow);
     End;

{ TLaunchDownloadJob }

function TLaunchDownloadJob.pGetTask: integer;
var i : integer;
begin
  if not(AllRun) and not(Cancelled) then
  Begin
    For i:=low(FStates) to high(FStates) do
        if FStates[i]=0 then
        Begin
          result:=i+1;
          Exit;
        end;
    AllRun:=True;
  end;
  Result:=ALL_TASK_COMPLETED;
  For i:=low(FStates) to high(FStates) do
      if FStates[i]=1 then
      Begin
          Result:=NO_MORE_TASK;
          Exit;
      end;
end;

procedure TLaunchDownloadJob.pTaskStarted(aTask: integer);
begin
  FRunning:=True;
  FStates[aTask-1]:=1;
end;

procedure TLaunchDownloadJob.pTaskEnded(aTask: integer; aExcept: Exception);
begin
  if Assigned(aExcept) then
    FStates[aTask-1]:=3
  Else
    FStates[aTask-1]:=2;
end;

procedure TLaunchDownloadJob.ExecuteTask(aTask: integer; FromWaiting: boolean);
var iTile : integer;
begin
  iTile:=aTask-1;
  Queue.AddUniqueJob(TEventJob.Create(@Engine.evDownload,TEnvTile.Create(FTiles[iTile],Win),false,Engine.GetTileName(FTiles[iTile])),Launcher);
end;

function TLaunchDownloadJob.Running: boolean;
begin
  Result:=FRunning;
end;

constructor TLaunchDownloadJob.Create(Eng : TMapViewerEngine;const Tiles: TTileIdArray;const aWin : TMapWindow);
var i : integer;
begin
  Engine:=Eng;
  SetLength(FTiles,Length(Tiles));
  For i:=low(FTiles) to high(FTiles) do
   FTiles[i]:=Tiles[i];
  SetLength(FStates,length(Tiles));
  AllRun:=false;
  Name:='LaunchDownload';
  Win:=aWin;
end;

{ TEnvTile }

constructor TEnvTile.Create(const aTile : TTileId;Const aWin : TMapWindow);
begin
  Tile:=aTile;
  Win:=aWin;
end;


{ TMapViewerEngine }

procedure TMapViewerEngine.SetZoom(AValue: integer);
begin
  if MapWin.Zoom=AValue then Exit;
  MapWin.Zoom:=AValue;
  ConstraintZoom(MapWin);
  CalculateWin(MapWin);
  Redraw(MapWin);
  if assigned(OnZoomChange) then
      OnZoomChange(Self);
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TMapViewerEngine.SetWidth(AValue: integer);
begin
  if MapWin.Width=AValue then Exit;
  MapWin.Width:=AValue;
  CalculateWin(MapWin);
  Redraw(MapWin);
end;

procedure TMapViewerEngine.SetHeight(AValue: integer);
begin
  if MapWin.Height=AValue then Exit;
  MapWin.Height:=AValue;
  CalculateWin(MapWin);
  Redraw(MapWin);
end;

procedure TMapViewerEngine.ConstraintZoom(var aWin : TMapWindow);
var zMin,zMax :integer;
Begin
    if Assigned(aWin.MapProvider) then
    Begin
      aWin.MapProvider.GetZoomInfos(zMin,zMax);
      if aWin.Zoom<zMin then
        aWin.Zoom:=zMin;
      if aWin.Zoom>zMax then
        aWin.Zoom:=zMax;
     End;
end;

procedure TMapViewerEngine.SetMapProvider(AValue: String);
var idx : integer;
    zMin,zMax : integer;
begin
  idx:=lstProvider.IndexOf(aValue);
  if not((aValue='') or (idx<>-1)) then
    Raise Exception.Create('Unknow Provider : '+aValue);
  if Assigned(MapWin.MapProvider) and (MapWin.MapProvider.Name=AValue) then Exit;
  if idx<>-1 then
  Begin
    MapWin.MapProvider:=TMapProvider(lstProvider.Objects[idx]);
    ConstraintZoom(MapWin);
  end
  else
    MapWin.MapProvider:=nil;
  if Assigned(MapWin.MapProvider) then
    Redraw(MapWin);
end;


procedure TMapViewerEngine.SetUseThreads(AValue: Boolean);
begin
  if Queue.UseThreads=AValue then Exit;
  Queue.UseThreads:=AValue;
  Cache.UseThreads:=AValue;
end;

function TMapViewerEngine.GetZoom: integer;
begin
  Result:=MapWin.zoom;
end;

procedure TMapViewerEngine.SetCacheOnDisk(AValue: Boolean);
begin
  if Cache.UseDisk=AValue then Exit;
  Cache.UseDisk:=AValue;
end;

procedure TMapViewerEngine.SetCachePath(AValue: String);
begin
  Cache.BasePath:=aValue;
end;

procedure TMapViewerEngine.SetDownloadEngine(AValue: TCustomDownloadEngine);
begin
  if FDownloadEngine=AValue then Exit;
  FDownloadEngine:=AValue;
  if Assigned(FDownloadEngine) then
    FDownloadEngine.FreeNotification(self);
end;


function TMapViewerEngine.GetHeight: integer;
begin
  Result:=MapWin.Height
end;

function TMapViewerEngine.GetCacheOnDisk: Boolean;
begin
  Result:=Cache.UseDisk;
end;

function TMapViewerEngine.GetCachePath: String;
begin
  Result:=Cache.BasePath;
end;

function TMapViewerEngine.GetCenter: TRealPoint;
begin
  Result:=MapWin.Center;
end;

function TMapViewerEngine.GetMapProvider: String;
begin
  if Assigned(MapWin.MapProvider) then
    Result:=MapWin.MapProvider.Name
  else
    Result:='';
end;

function TMapViewerEngine.GetUseThreads: Boolean;
begin
  Result:=Queue.UseThreads;
end;

function TMapViewerEngine.GetWidth: integer;
begin
  Result:=MapWin.Width
end;

function TMapViewerEngine.ScreenToLonLat(aPt: TPoint): TRealPoint;
begin
  Result:=MapWinToLonLat(MapWin,aPt);
end;

function TMapViewerEngine.LonLatToScreen(aPt: TRealPoint): TPoint;
Begin
  Result:=LonLatToMapWin(MapWin,aPt);
end;

function TMapViewerEngine.WorldScreenToLonLat(aPt: TPoint): TRealPoint;
begin
  aPt.X:=aPt.X-MapWin.X;
  aPt.Y:=aPt.Y-MapWin.Y;
  Result:=ScreenToLonLat(aPt);
end;

function TMapViewerEngine.LonLatToWorldScreen(aPt: TRealPoint): TPoint;
begin
  Result:=LonLatToScreen(aPt);
  Result.X:=Result.X+MapWin.X;
  Result.Y:=Result.Y+MapWin.Y;
end;

procedure TMapViewerEngine.SetSize(aWidth, aHeight: integer);
begin
  if (MapWin.Width=aWidth) and (MapWin.Height=aHeight) then Exit;
  CancelCurrentDrawing;
  MapWin.Width:=aWidth;
  MapWin.Height:=aHeight;
  CalculateWin(MapWin);
  Redraw(MapWin);
  if Assigned(Onchange) then
    OnChange(Self);
end;

procedure TMapViewerEngine.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button=mbLeft then
    DragObj.MouseDown(self,X,Y);
end;

procedure TMapViewerEngine.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button=mbLeft then
    DragObj.MouseUp(X,Y);
end;

procedure TMapViewerEngine.MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  DragObj.MouseMove(X,Y);
end;

procedure TMapViewerEngine.DblClick(Sender: TObject);
var pt: TPoint;
begin
  pt.X:=DragObj.MouseX;
  pt.Y:=DragObj.MouseY;
  SetCenter(ScreenToLonLat(pt));
end;

procedure TMapViewerEngine.MouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
var Val : Integer;
    nZoom : integer;
begin
  Val:=0;
  if WheelDelta>0 then
    Val:=1;
  if WheelDelta<0 then
    Val:=-1;
  nZoom:=Zoom+Val;
  if (nZoom>0) and (nZoom<20) then
    Zoom:=nZoom;
  Handled:=true;
end;

procedure TMapViewerEngine.SetCenter(aCenter: TRealPoint);
begin
  if (MapWin.Center.lon<>aCenter.Lon) and (MapWin.Center.lat<>aCenter.Lat) then
  Begin
    Mapwin.center:=aCenter;
    CalculateWin(MapWin);
    Redraw(MapWin);
    if assigned(OnCenterMove) then
      OnCenterMove(Self);
    if Assigned(OnChange) then
      OnChange(Self);
  end;
end;

procedure TMapViewerEngine.ZoomOnArea(const aArea: TRealArea);
var tmpWin : TMapWindow;
    visArea : TRealArea;
    TopLeft,BottomRight : TPoint;
begin
  tmpWin:=MapWin;
  tmpWin.Center.Lon:=(aArea.TopLeft.Lon+aArea.BottomRight.Lon)/2;
  tmpWin.Center.Lat:=(aArea.TopLeft.Lat+aArea.BottomRight.Lat)/2;
  tmpWin.Zoom:=15;
  TopLeft.X:=0;
  TopLeft.Y:=0;
  BottomRight.X:=tmpWin.Width;
  BottomRight.Y:=tmpWin.Height;
  Repeat
    CalculateWin(tmpWin);
    visArea.TopLeft:=MapWinToLonLat(tmpWin,TopLeft);
    visArea.BottomRight:=MapWinToLonLat(tmpWin,BottomRight);
    if (AreaInsideArea(aArea,visArea)) then
    begin
      break;
    end;
    Dec(tmpWin.Zoom);
  until (tmpWin.Zoom=2);
  MapWin:=tmpWin;
  Redraw(MapWin);
end;

procedure TMapViewerEngine.GetMapProviders(lst: TStrings);
begin
  lst.Assign(lstProvider);
end;

function TMapViewerEngine.LonLatToMapWin(const aWin : TMapWindow;aPt: TRealPoint): TPoint;
var
  tiles: Int64;
  circumference: Int64;
  lat: Extended;
  res: Extended;
  tmpX,tmpY : Double;
begin
  tiles := 1 shl aWin.Zoom;
  circumference := tiles * TILE_SIZE;
  tmpX := ((aPt.Lon+ 180.0)*circumference)/360.0;

  res := (2 * pi * EARTH_RADIUS) / circumference;


  tmpY := -aPt.Lat;
  tmpY := ln(tan((degToRad(tmpY) + pi / 2.0) / 2)) *180 / pi;
  tmpY:= (((tmpY /180.0)*SHIFT)+SHIFT)/res;

  tmpX := tmpX + aWin.X;
  tmpY := tmpY + aWin.Y;
  Result.X:=trunc(tmpX);
  Result.Y:=trunc(tmpY);
End;

function TMapViewerEngine.MapWinToLonLat(const aWin: TMapWindow; aPt: TPoint): TRealPoint;
var
  tiles: Int64;
  circumference: Int64;
  lat: Extended;
  res: Extended;
  mPoint : TPoint;
begin
  tiles := 1 shl aWin.Zoom;
  circumference := tiles * TILE_SIZE;

  mPoint.X := aPt.X - aWin.X;
  mPoint.Y := aPt.Y - aWin.Y;

  if mPoint.X < 0 then
    mPoint.X := 0
  else
  if mPoint.X > circumference then
    mPoint.X := circumference;

  if mPoint.Y < 0 then
    mPoint.Y := 0
  else
  if mPoint.Y > circumference then
    mPoint.Y := circumference;


  Result.Lon := ((mPoint.X * 360.0) / circumference) - 180.0;

  res := (2 * pi * EARTH_RADIUS) / circumference;
  lat := ((mPoint.Y * res - SHIFT) / SHIFT) * 180.0;

  lat := radtodeg (2 * arctan( exp( lat * pi / 180.0)) - pi / 2.0);
  Result.Lat := -lat;

  if Result.Lat > MAX_LATITUDE then
    Result.Lat := MAX_LATITUDE
  else
  if Result.Lat < MIN_LATITUDE then
    Result.Lat := MIN_LATITUDE;

  if Result.Lon > MAX_LONGITUDE then
    Result.Lon := MAX_LONGITUDE
  else
  if Result.Lon < MIN_LONGITUDE then
    Result.Lon := MIN_LONGITUDE;
end;

procedure TMapViewerEngine.CalculateWin(var aWin: TMapWindow);
var
  mx, my: Extended;
  res: Extended;
  px, py: Int64;

Begin

  mx := aWin.Center.Lon * SHIFT / 180.0;
  my := ln( tan((90 - aWin.Center.Lat) * pi / 360.0 )) / (pi / 180.0);

  my := my * SHIFT / 180.0;

  res := (2 * pi * EARTH_RADIUS) / (TILE_SIZE * (1 shl aWin.Zoom));
  px := Round((mx + shift) / res);
  py := Round((my + shift) / res);

  aWin.X := aWin.Width div 2 - px;
  aWin.Y := aWin.Height div 2 - py;

end;

function TMapViewerEngine.IsValidTile(const aWin: TMapWindow;const aTile : TTIleId) : boolean;
var tiles : int64;
begin
  tiles := 1 shl aWin.Zoom;
  Result:=(aTile.X>=0) and (aTile.X<=tiles-1) and (aTile.Y>=0) and (aTile.Y<=tiles-1);
end;


procedure TMapViewerEngine.Redraw(const aWin: TmapWindow);
var TilesVis : TArea;
    x,y :longint;//int64;  //=== ct9999 ==================
    Tiles : TTileIdArray;
    iTile : Integer;
begin
  if not(Active) then
     Exit;
  Queue.CancelAllJob(self);
  TilesVis:=CalculateVisibleTiles(aWin);
  SetLength(Tiles,(TilesVis.Bottom-TilesVis.top+1)*(TilesVis.Right-TilesVis.Left+1));
  iTile:=low(Tiles);
  For y:=TilesVis.top to TilesVis.Bottom do
    For X:=TilesVis.Left to TilesVis.Right do
      Begin
        Tiles[iTile].X:=X;
        Tiles[iTile].Y:=Y;
        Tiles[iTile].Z:=aWin.Zoom;
        if IsValidTile(aWin,Tiles[iTile]) then
          iTile+=1;
      end;
  SetLength(Tiles,iTile);
  if length(Tiles)>0 then
  Begin
    Queue.AddJob(TLaunchDownloadJob.Create(self,Tiles,aWin),self);
  end;
end;

function TMapViewerEngine.CalculateVisibleTiles(const aWin: TMapWindow): TArea;
var MaxX,MAxY,Startx,StartY : int64;
begin
  MaxX := (aWin.Width div TILE_SIZE) + 1;
  MaxY := (aWin.Height div TILE_SIZE) + 1;
  startX := (-(aWin.X)) div TILE_SIZE;
  startY := (-(aWin.Y)) div TILE_SIZE;
  Result.Left := startX;
  Result.right := startX + MaxX;
  Result.top := startY;
  Result.bottom := startY + MaxY;
end;

function TMapViewerEngine.IsCurrentWin(const aWin: TMapWindow): boolean;
begin
  Result:=(aWin.Zoom=MapWin.Zoom) and (aWin.Center.Lat=MapWin.Center.Lat) and (aWin.Center.Lon=MapWin.Center.Lon) and (aWin.Width=MapWin.Width) and (aWin.Height=MapWin.Height);
end;

function TMapViewerEngine.GetTileName(const Id: TTileId): String;
begin
  Result:=Inttostr(Id.X)+'.'+inttostr(Id.Y)+'.'+inttostr(Id.Z);
end;

procedure TMapViewerEngine.evDownload(Data : TObject;Job : TJob);
var Id : TTileId;
    Url : String;
    Env : TEnvTile;
    MapO : TMapProvider;
    FStream : TMemoryStream;
Begin
   Env:=TEnvTile(Data);
   Id:=Env.Tile;
   MapO:=Env.Win.MapProvider;
   if Assigned(MapO) then
   Begin
     if not(Cache.InCache(MapO,Id)) then
     Begin
         if Assigned(FDownloadEngine) then
         begin
           Url:=MapO.GetUrlForTile(Id);
           if Url<>'' then
           begin
             FStream:=TMemoryStream.Create;
             Try
                Try
                   FDownloadEngine.DownloadFile(Url,Fstream);
                   Cache.Add(MapO,Id,FStream);
                except
                end;
             finally
                FreeAndNil(FStream);
             end;
           end;
         end;
     end;
   end;
   if Job.Cancelled then
     Exit;
   if DrawTitleInGuiThread then
     Queue.QueueAsyncCall(@TileDownloaded,PtrInt(Env))
   else
     TileDownloaded(PtrInt(Env));
end;

procedure TMapViewerEngine.TileDownloaded(Data: PtrInt);
var EnvTile : TEnvTile;
    img : TLazIntfImage;
    X,Y : integer;
begin
  EnvTile:=TEnvTile(Data);
  Try
    if IsCurrentWin(EnvTile.Win)then
    Begin
       Cache.GetFromCache(EnvTile.Win.MapProvider,EnvTile.Tile,img);
       X := EnvTile.Win.X + EnvTile.Tile.X * TILE_SIZE; // begin of X
       Y := EnvTile.Win.Y + EnvTile.Tile.Y * TILE_SIZE; // begin of X
       DrawTile(EnvTile.Tile,X,Y,img);
    end;
  finally
    FreeAndNil(EnvTile);
  end;
end;

function TMapViewerEngine.GetLetterSvr(id: integer): String;
Begin
  Result:=Char(Ord('a')+id);
end;

function TMapViewerEngine.GetYahooSvr(id: integer): String;
Begin
  Result:=inttostr(id+1);
end;

function TMapViewerEngine.GetYahooY(const Tile : TTileId): string;
Begin
    Result :=inttostr( - (Tile.Y - (1 shl Tile.Z) div 2) - 1);
end;

function TMapViewerEngine.GetYahooZ(const Tile : TTileId): string;
Begin
  result:=inttostr(Tile.Z+1);
end;

function TMapViewerEngine.GetQuadKey(const Tile : TTileId): string;
var
  i, d, m: Longword;
begin
  {
    Bing Maps Tile System
    http://msdn.microsoft.com/en-us/library/bb259689.aspx
  }
  Result := '';
  for i := Tile.Z downto 1 do
  begin
    d := 0;
    m := 1 shl (i - 1);
    if (Tile.x and m) <> 0 then
      Inc(d, 1);
    if (Tile.y and m) <> 0 then
      Inc(d, 2);
    Result := Result + IntToStr(d);
  end;
end;

Type

{ TMemObj }

 TMemObj = Class
     private
       FWin : TMapWindow;
     public
       constructor Create(const aWin : TMapWindow);
     End;

{ TMemObj }

constructor TMemObj.Create(const aWin: TMapWindow);
begin
  FWin:=aWin;
end;


procedure TMapViewerEngine.MoveMapCenter(Sender: TDragObj);
var old : TMemObj;
    nCenter : TRealPoint;
    Job : TJob;
    aPt : TPoint;
Begin
  if Sender.LnkObj=nil then
  Begin
    Sender.LnkObj:=TMemObj.Create(MapWin);
  end;
  old:=TMemObj(Sender.LnkObj);
  aPt.X:=old.FWin.Width DIV 2-Sender.OfsX;
  aPt.Y:=old.FWin.Height DIV 2-Sender.OfsY;
  nCenter:=MapWinToLonLat(old.FWin,aPt);
  SetCenter(nCenter);
end;

procedure TMapViewerEngine.SetActive(AValue: boolean);
begin
  if FActive=AValue then Exit;
  FActive:=AValue;
  if not(FActive) then
     Queue.CancelAllJob(self)
  else
     Redraw(MapWin);
end;

procedure TMapViewerEngine.DoDrag(Sender: TDragObj);
begin
   if Sender.DragSrc=self then
   Begin
     MoveMapCenter(Sender);
   end;
end;

procedure TMapViewerEngine.CancelCurrentDrawing;
var Jobs : TJobArray;
begin
  Jobs:=Queue.CancelAllJob(self);
  Queue.WaitForTerminate(Jobs);
end;

procedure TMapViewerEngine.Redraw;
begin
  Redraw(MapWin);
end;


function TMapViewerEngine.AddMapProvider(OpeName: String; Url: String;
  MinZoom : integer;MaxZoom : integer;
  NbSvr: integer; GetSvrStr: TGetSvrStr; GetXStr: TGetValStr;
  GetYStr: TGetValStr; GetZStr: TGetValStr) : TMapProvider;
var idx :integer;
Begin
  idx:=lstProvider.IndexOf(OpeName);
  if idx=-1 then
  Begin
    result:=TMapProvider.Create(OpeName);
    lstProvider.AddObject(OpeName,result);
  end
  else
    result:=TMapProvider(lstProvider.Objects[idx]);
  result.AddUrl(Url,NbSvr,MinZoom,MaxZoom,GetSvrStr,GetXStr,GetYStr,GetZStr);
end;

procedure TMapViewerEngine.RegisterProviders;
begin
 AddMapProvider('Aucun','',0,30, 0);
 {
  AddMapProvider('Google Satellite','http://khm%d.google.com/kh/v=82&x=%x%&y=%y%&z=%z%&s=Ga',4);
  AddMapProvider('Google Hybrid','http://khm%d.google.com/kh/v=82&x=%x%&y=%y%&z=%z%&s=Ga',4);
  AddMapProvider('Google Hybrid','http://mt%d.google.com/vt/lyrs=h@145&v=w2.104&x=%d&y=%d&z=%z%',4);
  AddMapProvider('Google physical','http://mt%d.google.com/vt/lyrs=t@145&v=w2.104&x=%d&y=%d&z=%z%',4);
  AddMapProvider('Google Physical Hybrid','http://mt%d.google.com/vt/lyrs=t@145&v=w2.104&x=%x%&y=%y%&z=%z%',4);
  AddMapProvider('Google Physical Hybrid','http://mt%d.google.com/vt/lyrs=h@145&v=w2.104&x=%x%&y=%y%&z=%z%',4);}
  //AddMapProvider('OpenStreetMap Osmarender','http://%serv%.tah.openstreetmap.org/Tiles/tile/%z%/%x%/%y%.png',0,20,3, @getLetterSvr); // [Char(Ord('a')+Random(3)), Z, X, Y]));
  //AddMapProvider('Yahoo Normal','http://maps%serv%.yimg.com/hx/tl?b=1&v=4.3&.intl=en&x=%x%&y=%y%d&z=%d&r=1'        , 0,20,3,@GetYahooSvr, nil, @getYahooY, @GetYahooZ); //(Z+1]));
  //AddMapProvider('Yahoo Satellite','http://maps%serv%.yimg.com/ae/ximg?v=1.9&t=a&s=256&.intl=en&x=%d&y=%d&z=%d&r=1', 0,20,3,@GetYahooSvr, nil, @getYahooY, @GetYahooZ); //[Random(3)+1, X, YahooY(Y), Z+1]));
  //AddMapProvider('Yahoo Hybrid','http://maps%serv%.yimg.com/ae/ximg?v=1.9&t=a&s=256&.intl=en&x=%x%&y=%y%&z=%z%&r=1', 0,20,3,@GetYahooSvr, nil, @getYahooY, @GetYahooZ); //[Random(3)+1, X, YahooY(Y), Z+1]));
  //AddMapProvider('Yahoo Hybrid','http://maps%serv%.yimg.com/hx/tl?b=1&v=4.3&t=h&.intl=en&x=%x%&y=%y%&z=%z%&r=1'    , 0,20,3,@GetYahooSvr, nil, @getYahooY, @GetYahooZ); //[Random(3)+1, X, YahooY(Y), Z+1]));

  MapWin.MapProvider:=AddMapProvider('OpenStreetMap Mapnik','http://%serv%.tile.openstreetmap.org/%z%/%x%/%y%.png',0,19, 3, @getLetterSvr);
  AddMapProvider('Open Cycle Map','http://%serv%.tile.opencyclemap.org/cycle/%z%/%x%/%y%.png',0,18,3, @getLetterSvr);
  AddMapProvider('Virtual Earth Bing','http://ecn.t%serv%.tiles.virtualearth.net/tiles/r%x%?g=671&mkt=en-us&lbl=l1&stl=h&shading=hill',1,19,8,nil,@GetQuadKey);
  AddMapProvider('Virtual Earth Road','http://r%serv%.ortho.tiles.virtualearth.net/tiles/r%x%.png?g=72&shading=hill',1,19,4,nil,@getQuadKey);
  AddMapProvider('Virtual Earth Aerial','http://a%serv%.ortho.tiles.virtualearth.net/tiles/a%x%.jpg?g=72&shading=hill',1,19,4,nil,@getQuadKey);
  AddMapProvider('Virtual Earth Hybrid','http://h%serv%.ortho.tiles.virtualearth.net/tiles/h%x%.jpg?g=72&shading=hill',1,19,4,nil,@getQuadKey);
  AddMapProvider('Ovi Normal', 'http://%serv%.maptile.maps.svc.ovi.com/maptiler/v2/maptile/newest/normal.day/%z%/%x%/%y%/256/png8', 0,20,5,@getLetterSvr);
  AddMapProvider('Ovi Satellite','http://%serv%.maptile.maps.svc.ovi.com/maptiler/v2/maptile/newest/satellite.day/%z%/%x%/%y%/256/png8', 0,20,5,@getLetterSvr);
  AddMapProvider('Ovi Hybrid','http://%serv%.maptile.maps.svc.ovi.com/maptiler/v2/maptile/newest/hybrid.day/%z%/%x%/%y%/256/png8', 0,20,5,@getLetterSvr);
  AddMapProvider('Ovi Physical','http://%serv%.maptile.maps.svc.ovi.com/maptiler/v2/maptile/newest/terrain.day/%z%/%x%/%y%/256/png8', 0,20,5,@getLetterSvr);
end;

procedure TMapViewerEngine.DrawTile(const TileId : TTileId;X, Y: integer; TileImg: TLazIntfImage);
begin
  if Assigned(FOnDrawTile) then
    FOnDrawTile(TileId,X,Y,TileImg);
end;

constructor TMapViewerEngine.Create(aOwner: TComponent);
begin
  DrawTitleInGuiThread := true;
  DragObj := TDragObj.Create;
  DragObj.OnDrag:=@DoDrag;
  Cache := TPictureCache.Create(self);
  lstProvider:=TStringList.Create;
  RegisterProviders;
  Queue:=TJobQueue.Create(8);
  Queue.OnIdle:= @Cache.CheckCacheSize;
  inherited Create(aOwner);
  ConstraintZoom(MapWin);
  CalculateWin(mapWin);
end;

destructor TMapViewerEngine.Destroy;
begin
  FreeAndNil(DragObj);
  FreeAndNil(lstProvider);
  FreeAndNil(Queue);
  inherited Destroy;
end;

end.

