{ (c) 2014 ti_dic MapViewer component for lazarus
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

unit mvmapviewer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, mvengine, controls, IntfGraphics, mvmapprovider, graphics,
  GraphType, mvjobqueue, mvextradata, LResources, RGBGraphics,
  mvCache, mvDLESynapse, mvgpsobj, mvtypes;

Type

  { TMVMapViewer }

  TMVMapViewer = class(TCustomControl)
    private
      dl : TMVDESynapse;
      FEngine : TMapViewerEngine;
      Buffer : TRGB32Bitmap;
      FActive: boolean;
      FGPSItems: TGPSObjectList;
      FInactiveColor: TColor;
      FPOIImage: TBitmap;
      procedure DoAsyncInvalidate(Data: PtrInt);
      procedure CallAsyncInvalidate;
      procedure DrawObjects(const TileId: TTileId; aLeft, aTop, aRight,aBottom: integer);
      procedure DrawTrk(const Area : TRealArea;trk: TGPSTrack);
      procedure DrawPt(const Area : TRealArea;aPOI: TGPSPoint);
      function GetCacheOnDisk: boolean;
      function GetCachePath: String;
      function GetCenter: TRealPoint;
      function GetMapProvider: String;
      function GetOnCenterMove: TNotifyEvent;
      function GetOnChange: TNotifyEvent;
      function GetOnZoomChange: TNotifyEvent;
      function GetUseThreads: boolean;
      function GetZoom: integer;
      procedure SetActive(AValue: boolean);
      procedure SetCacheOnDisk(AValue: boolean);
      procedure SetCachePath(AValue: String);
      procedure SetCenter(AValue: TRealPoint);
      procedure SetInactiveColor(AValue: TColor);
      Procedure ActivateEngine;
      procedure SetMapProvider(AValue: String);
      procedure SetOnCenterMove(AValue: TNotifyEvent);
      procedure SetOnChange(AValue: TNotifyEvent);
      procedure SetOnZoomChange(AValue: TNotifyEvent);
      procedure SetUseThreads(AValue: boolean);
      procedure SetZoom(AValue: integer);
    protected
      AsyncInvalidate : boolean;
      function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
      procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
      procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
      procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
      procedure DblClick; override;
      procedure DoOnResize; override;
      procedure Paint; override;
      procedure OnGPSItemsModified(Sender : TObject;objs : TGPSObjList;Adding : boolean);
      Procedure DoDrawTile(const TileId : TTileId;X,Y : integer;TileImg : TLazIntfImage);
      Function IsActive : Boolean;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy;override;

      Function ScreenToLonLat(aPt : TPoint) : TRealPoint;
      Function LonLatToScreen(aPt : TRealPoint) : TPoint;

      procedure GetMapProviders(lstProviders : TStrings);
      property Center : TRealPoint read GetCenter write SetCenter;
      property Engine : TMapViewerEngine read FEngine;
      property GPSItems : TGPSObjectList read FGPSItems;
      Procedure WaitEndOfRendering;
      Procedure CenterOnObj(obj : TGPSObj);
      procedure ZoomOnObj(obj : TGPSObj);
      procedure ZoomOnArea(const aArea : TRealArea);
      Function GetVisibleArea : TRealArea;
    published
      property Width;
      property Height;

      property Active : boolean read FActive write SetActive;
      property Zoom : integer read GetZoom write SetZoom;
      property InactiveColor : TColor read FInactiveColor write SetInactiveColor;
      property CacheOnDisk : boolean read GetCacheOnDisk write SetCacheOnDisk;
      property CachePath : String read GetCachePath write SetCachePath;
      property UseThreads : boolean read GetUseThreads write SetUseThreads;
      property OnMouseMove;
      property OnMouseLeave;
      property PopupMenu;
      property OnCenterMove : TNotifyEvent  read GetOnCenterMove write SetOnCenterMove;
      property OnZoomChange : TNotifyEvent  read GetOnZoomChange write SetOnZoomChange;
      property POIImage : TBitmap read FPOIImage write FPOIImage;
      property MapProvider : String read GetMapProvider write SetMapProvider;
      property OnChange : TNotifyEvent Read GetOnChange write SetOnChange;
  end;


implementation

Type

  { TDrawObjJob }

  TDrawObjJob = Class(TJob)
  private
    AllRun : boolean;
    Viewer : TMVMapViewer;
    FRunning : boolean;
    FLst : TGPSObjList;
    FStates : Array of integer;
    FArea : TRealArea;
  protected
    function pGetTask : integer;override;
    procedure pTaskStarted(aTask: integer);override;
    procedure pTaskEnded(aTask : integer;aExcept : Exception);override;
  public
    procedure ExecuteTask(aTask : integer;FromWaiting : boolean);override;
    function Running : boolean;override;
  public
    Constructor Create(aViewer : TMVMapViewer;aLst : TGPSObjList;const aArea : TRealArea);
    destructor Destroy;override;
  end;

{ TDrawObjJob }

function TDrawObjJob.pGetTask: integer;
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

procedure TDrawObjJob.pTaskStarted(aTask: integer);
begin
  FRunning:=True;
  FStates[aTask-1]:=1;
end;

procedure TDrawObjJob.pTaskEnded(aTask: integer; aExcept: Exception);
begin
  if Assigned(aExcept) then
    FStates[aTask-1]:=3
  Else
    FStates[aTask-1]:=2;
end;

procedure TDrawObjJob.ExecuteTask(aTask: integer; FromWaiting: boolean);
var iObj : integer;
    Obj : TGpsObj;
begin
    iObj:=aTask-1;
    Obj:=FLst[iObj];
    if Obj.InheritsFrom(TGPSTrack) then
    Begin
      Viewer.DrawTrk(FArea,TGPSTrack(Obj));
    End;
    if Obj.InheritsFrom(TGPSPoint) then
    Begin
      Viewer.DrawPt(FArea,TGPSPoint(Obj));
    end;
end;

function TDrawObjJob.Running: boolean;
begin
  Result:=FRunning;
end;

constructor TDrawObjJob.Create(aViewer: TMVMapViewer; aLst: TGPSObjList;
  const aArea: TRealArea);
begin
  FArea:=aArea;
  FLst:=aLst;
  SetLEngth(FStates,FLst.Count);
  Viewer:=aViewer;
  AllRun:=false;
  Name:='DrawObj';
end;

destructor TDrawObjJob.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FLst);
  if not(Cancelled) then
    Viewer.CallAsyncInvalidate;
end;


{ TMVMapViewer }

procedure TMVMapViewer.SetActive(AValue: boolean);
begin
  if FActive=AValue then Exit;
  FActive:=AValue;
  if FActive then
    ActivateEngine
  else
    Engine.Active:=false;
end;

function TMVMapViewer.GetCacheOnDisk: boolean;
begin
  Result:=Engine.CacheOnDisk;
end;

function TMVMapViewer.GetCachePath: String;
begin
  Result:=Engine.CachePath;
end;

function TMVMapViewer.GetCenter: TRealPoint;
begin
  Result:=Engine.Center;
end;

function TMVMapViewer.GetMapProvider: String;
begin
  result:=Engine.MapProvider;
end;

function TMVMapViewer.GetOnCenterMove: TNotifyEvent;
begin
  result:=Engine.OnCenterMove;
end;

function TMVMapViewer.GetOnChange: TNotifyEvent;
begin
  Result:=Engine.OnChange;
end;

function TMVMapViewer.GetOnZoomChange: TNotifyEvent;
begin
  Result:=Engine.OnZoomChange;
end;

function TMVMapViewer.GetUseThreads: boolean;
begin
  Result:=Engine.UseThreads;
end;

function TMVMapViewer.GetZoom: integer;
begin
  result:=Engine.Zoom;
end;

procedure TMVMapViewer.SetCacheOnDisk(AValue: boolean);
begin
  Engine.CacheOnDisk:=AValue;
end;

procedure TMVMapViewer.SetCachePath(AValue: String);
begin
  Engine.CachePath:=CachePath;
end;

procedure TMVMapViewer.SetCenter(AValue: TRealPoint);
begin
    Engine.Center:=AValue;
end;

procedure TMVMapViewer.SetInactiveColor(AValue: TColor);
begin
  if FInactiveColor=AValue then Exit;
  FInactiveColor:=AValue;
  if not(IsActive) then
     invalidate;
end;

procedure TMVMapViewer.ActivateEngine;
begin
  Engine.SetSize(ClientWidth,ClientHeight);
  if IsActive then
    Engine.Active:=true
  else
    Engine.Active:=false;
end;

procedure TMVMapViewer.SetMapProvider(AValue: String);
begin
  Engine.MapProvider:=AValue;
end;

procedure TMVMapViewer.SetOnCenterMove(AValue: TNotifyEvent);
begin
  Engine.OnCenterMove:=AValue;
end;

procedure TMVMapViewer.SetOnChange(AValue: TNotifyEvent);
begin
  Engine.OnChange:=AValue;
end;

procedure TMVMapViewer.SetOnZoomChange(AValue: TNotifyEvent);
begin
    Engine.OnZoomChange:=AValue;
end;

procedure TMVMapViewer.SetUseThreads(AValue: boolean);
begin
  Engine.UseThreads:=aValue;
end;

procedure TMVMapViewer.SetZoom(AValue: integer);
begin
  Engine.Zoom:=AValue;
end;

function TMVMapViewer.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Result:=inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  if IsActive then
    Engine.MouseWheel(self,Shift,WheelDelta,MousePos,Result);
end;

procedure TMVMapViewer.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if IsActive then
    Engine.MouseDown(self,Button,Shift,X,Y);
end;

procedure TMVMapViewer.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X: Integer; Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if IsActive then
    Engine.MouseUp(self,Button,Shift,X,Y);
end;

procedure TMVMapViewer.MouseMove(Shift: TShiftState; X: Integer; Y: Integer);
var aPt : TPoint;
begin
  inherited MouseMove(Shift, X, Y);
  if IsActive then
    Engine.MouseMove(self,Shift,X,Y);
end;

procedure TMVMapViewer.DblClick;
begin
  inherited DblClick;
  if IsActive then
    Engine.DblClick(self);
end;

procedure TMVMapViewer.DoOnResize;
begin
  inherited DoOnResize;
  //cancel all rendering thread
  Engine.CancelCurrentDrawing;
  FreeAndNil(Buffer);
  Buffer := TRGB32Bitmap.Create(ClientWidth,ClientHeight);
  if IsActive then
  Begin
    Engine.SetSize(ClientWidth,ClientHeight);
  end;
end;

procedure TMVMapViewer.Paint;
begin
  inherited Paint;
  if IsActive and Assigned(Buffer) then
  Begin
    Buffer.Canvas.DrawTo(Canvas,0,0);
  end
  else
  begin
      Canvas.Brush.Color:=InactiveColor;
      Canvas.Brush.Style:=bsSolid;
      Canvas.FillRect(0,0,ClientWidth,ClientHeight);
  end;
end;

procedure TMVMapViewer.OnGPSItemsModified(Sender: TObject; objs: TGPSObjList;
  Adding: boolean);
var Area,ObjArea,vArea : TRealArea;
begin
  if Adding and assigned(Objs) then
  Begin
    ObjArea:=GetAreaOf(Objs);
    vArea:=GetVisibleArea;
    if hasIntersectArea(ObjArea,vArea) then
    Begin
      Area:=IntersectArea(ObjArea,vArea);
      Engine.Jobqueue.AddJob(TDrawObjJob.Create(self,Objs,Area),Engine);
    end
    else
      objs.Free;
  end
  else
  Begin
    Engine.Redraw;
    Objs.free;
  end;
end;

procedure TMVMapViewer.DrawTrk(const Area : TRealArea;trk : TGPSTrack);
var Old,New : TPoint;
    i : integer;
    aPt : TRealPoint;
    LastInside,IsInside : boolean;
    trkColor : TColor;
Begin
     if trk.Points.Count>0 then
     Begin
       trkColor:=clRed;
       if trk.ExtraData<>nil then
       Begin
         if trk.ExtraData.inheritsFrom(TDrawingExtraData) then
           trkColor:=TDrawingExtraData(trk.ExtraData).Color;
       end;
       LastInside:=false;
       For i:=0 to pred(trk.Points.Count) do
       Begin
           aPt:=trk.Points[i].RealPoint;
           IsInside:=PtInsideArea(aPt,Area);
           if IsInside or LastInside then
           Begin
             New:=Engine.LonLatToScreen(aPt);
             if i>0 then
             Begin
               if not(LastInside) then
                 Old:=Engine.LonLatToScreen(trk.Points[pred(i)].RealPoint);
               Buffer.canvas.OutlineColor:=trkColor;
               Buffer.canvas.Line(Old.X,Old.y,New.X,New.Y);
             end;
             Old:=New;
             LastInside:=IsInside;
           end;
       end;
     end;
end;

procedure TMVMapViewer.DrawPt(const Area: TRealArea; aPOI: TGPSPoint);
var PT : TPoint;
    PtColor : TColor;
begin
  Pt:=Engine.LonLatToScreen(aPOI.RealPoint);
  PtColor:=clRed;
  if aPOI.ExtraData<>nil then
  Begin
     if aPOI.ExtraData.inheritsFrom(TDrawingExtraData) then
       PtColor:=TDrawingExtraData(aPOI.ExtraData).Color;
  end;
  Buffer.canvas.OutlineColor:=ptColor;
  Buffer.canvas.Line(Pt.X,Pt.y-5,Pt.X,Pt.Y+5);
  Buffer.canvas.Line(Pt.X-5,Pt.y,Pt.X+5,Pt.Y);

//  Buffer.Draw();
end;

procedure TMVMapViewer.CallAsyncInvalidate;
Begin
  if not(AsyncInvalidate) then
  Begin
    AsyncInvalidate:=true;
    Engine.Jobqueue.QueueAsyncCall(@DoAsyncInvalidate,0);
  end;
end;

procedure TMVMapViewer.DrawObjects(const TileId: TTileId; aLeft, aTop,aRight,aBottom: integer);
var aPt : TPoint;
    Area : TRealArea;
    lst  : TGPSObjList;
    i : integer;
    trk : TGPSTrack;
begin
  aPt.X:=aLeft;
  aPt.Y:=aTop;
  Area.TopLeft:=Engine.ScreenToLonLat(aPt);
  aPt.X:=aRight;
  aPt.Y:=aBottom;
  Area.BottomRight:=Engine.ScreenToLonLat(aPt);
  if GPSItems.count>0 then
  begin
    lst:=GPSItems.GetObjectsInArea(Area);
    if lst.Count>0 then
      Engine.Jobqueue.AddJob(TDrawObjJob.Create(self,lst,Area),Engine)
    else
    begin
        freeAndNil(Lst);
        CallAsyncInvalidate;
    end;
  end
  Else
    CallAsyncInvalidate;
end;

procedure TMVMapViewer.DoAsyncInvalidate(Data: PtrInt);
Begin
  Invalidate;
  AsyncInvalidate:=false;
end;

procedure TMVMapViewer.DoDrawTile(const TileId: TTileId; X, Y: integer;
  TileImg: TLazIntfImage);
var temp : TRGB32Bitmap;
    ri : TRawImage;
    BuffLaz : TLazIntfImage;
begin
  if Assigned(Buffer) then
  begin
    if Assigned(TileImg) then
    Begin

      if (X>=0) and (Y>=0) then //http://mantis.freepascal.org/view.php?id=27144
      begin
        ri.Init;
        ri.Description.Init_BPP32_R8G8B8A8_BIO_TTB(Buffer.Width,Buffer.Height);
        ri.Data:=Buffer.Pixels;
        BuffLaz := TLazIntfImage.Create(ri,false);
        try
          BuffLaz.CopyPixels(TileImg,X,y);
          ri.Init;
        finally
          FreeandNil(BuffLaz);
        end;
      end
      else
      begin
        //i think it take more memory then the previous method but work in all case
        temp:=TRGB32Bitmap.CreateFromLazIntfImage(TileImg);
        try
          Buffer.Draw(X,Y,temp);
        finally
          FreeAndNil(Temp);
        end;
      end;
    end
    else
      Buffer.Canvas.FillRect(X,Y,X+TILE_SIZE,Y+TILE_SIZE);
  end;
  DrawObjects(TileId,X,Y,X+TILE_SIZE,Y+TILE_SIZE);
end;

function TMVMapViewer.IsActive: Boolean;
begin
  if not(csDesigning in ComponentState) then
    Result:=FActive
  else
    Result:=false;
end;

constructor TMVMapViewer.Create(AOwner: TComponent);
begin
  Active:=false;
  FGPSItems:=TGPSObjectList.Create;
  FGPSItems.OnModified:=@OnGPSItemsModified;
  FInactiveColor:=clWhite;
  FEngine:=TMapViewerEngine.Create(self);
  dl:=TMVDESynapse.Create(self);
  Buffer := TRGB32Bitmap.Create(Width,Height);
  Engine.CachePath:='cache/';
  Engine.CacheOnDisk:=true;
  Engine.OnDrawTile:=@DoDrawTile;
  Engine.DrawTitleInGuiThread:=false;
  Engine.DownloadEngine:=dl;
  inherited Create(AOwner);
  Width:=50;
  Height:=50;
end;

destructor TMVMapViewer.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FGPSItems);
end;

function TMVMapViewer.ScreenToLonLat(aPt: TPoint): TRealPoint;
begin
  Result:=Engine.ScreenToLonLat(aPt);
end;

function TMVMapViewer.LonLatToScreen(aPt: TRealPoint): TPoint;
begin
  Result:=LonLatToScreen(aPt);
end;

procedure TMVMapViewer.GetMapProviders(lstProviders: TStrings);
begin
  Engine.GetMapProviders(lstProviders);
end;

procedure TMVMapViewer.WaitEndOfRendering;
begin
  Engine.Jobqueue.WaitAllJobTerminated(Engine);
end;

procedure TMVMapViewer.CenterOnObj(obj: TGPSObj);
var Area : TRealArea;
    Pt : TRealPoint;
begin
  obj.GetArea(Area);
  Pt.Lon:=(Area.TopLeft.Lon+Area.BottomRight.Lon) /2;
  Pt.Lat:=(Area.TopLeft.Lat+Area.BottomRight.Lat) /2;
  Center:=Pt;
end;

procedure TMVMapViewer.ZoomOnObj(obj: TGPSObj);
var Area : TRealArea;
begin
  obj.GetArea(Area);
  Engine.ZoomOnArea(Area);
end;

procedure TMVMapViewer.ZoomOnArea(const aArea: TRealArea);
begin
  Engine.ZoomOnArea(aArea);
end;

function TMVMapViewer.GetVisibleArea: TRealArea;
var aPt : TPoint;
begin
  aPt.X:=0;
  aPt.Y:=0;
  Result.TopLeft:=Engine.ScreenToLonLat(aPt);
  aPt.X:=Width;
  aPt.Y:=Height;
  Result.BottomRight:=Engine.ScreenToLonLat(aPt);;
end;


end.

