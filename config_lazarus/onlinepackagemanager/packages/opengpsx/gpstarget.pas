{ <GPSTarget>

  Copyright (C) 2010 Prajuab Riabroy

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

unit gpstarget;

interface

uses
  LCLType, LResources, LCLIntf, Classes, ExtCtrls, Graphics, SysUtils, Types,
  gpsutilities, Controls, LMessages, Forms, gpsdatadef, geoellipsoids,
  geocompute, nmeadecode;

const
  {$IFDEF LINUX}
  TargetBackground : array[0..3] of TRotationText = (
             (FaceName:'Sans';Color:clBlue;Height:8;Weight:fsBold;
              Text:'N';RotationAngle:90;HozAlignment:haCenter;VerAlignment:vaBottom),
             (FaceName:'Sans';Color:clBlue;Height:8;Weight:fsBold;
              Text:'E';RotationAngle:90;HozAlignment:haLeft;VerAlignment:vaCenter),
             (FaceName:'Sans';Color:clBlue;Height:8;Weight:fsBold;
              Text:'S';RotationAngle:90;HozAlignment:haCenter;VerAlignment:vaTop),
             (FaceName:'Sans';Color:clBlue;Height:8;Weight:fsBold;
              Text:'W';RotationAngle:90;HozAlignment:haRight;VerAlignment:vaCenter)
  );
  {$ELSE}
  TargetBackground : array[0..3] of TRotationText = (
             (FaceName:'Arial';Color:clBlue;Height:10;Weight:fsBold;
              Text:'N';RotationAngle:90;HozAlignment:haCenter;VerAlignment:vaBottom),
             (FaceName:'Arial';Color:clBlue;Height:10;Weight:fsBold;
              Text:'E';RotationAngle:90;HozAlignment:haLeft;VerAlignment:vaCenter),
             (FaceName:'Arial';Color:clBlue;Height:10;Weight:fsBold;
              Text:'S';RotationAngle:90;HozAlignment:haCenter;VerAlignment:vaTop),
             (FaceName:'Arial';Color:clBlue;Height:10;Weight:fsBold;
              Text:'W';RotationAngle:90;HozAlignment:haRight;VerAlignment:vaCenter)
  );
  {$ENDIF}
type
  TCoordinateType = (ctGeo, ctUTM);
  TArrowType = (atArrow1, atArrow2, atArrow3);
  TTargetCoordinateType = (ttUseWPL, ttUserdefined);

  //Interface
  IGraphic = interface
    ['{93180E0A-AA2D-41D3-9F93-CF15983B3E7D}']
    procedure SetTransform(const TransformPara : TTransformPara);
    procedure draw;
  end;

  TPolyline = class(TObject)
  private
    FLines : TCoordinateArray;
    FBrushColor : integer;
    FBrushStyle : TBrushStyle;
    FPenColor : integer;
    FPenWidth : integer;
    FCanvas : TCanvas;
    FPenStyle : TPenStyle;
    FTransfrm : TTransformPara;

    procedure SetPolyline(ALine : TCoordinateArray);
  public
    property Lines : TCoordinateArray read FLines write SetPolyLine;
    property PenColor : integer read FPenColor write FPenColor;
    property PenWidth : integer read FPenWidth write FPenWidth;
    property Canvas : TCanvas read FCanvas write FCanvas;
    property PenStyle : TPenStyle read FPenStyle write FPenStyle;
    property BrushColor : integer read FBrushColor write FBrushColor;
    property BrushStyle : TBrushStyle read FBrushStyle write FBrushStyle;
  end;

  TCxPolyline = class(TPolyline, IGraphic)
    procedure SetTransform(const TransformPara : TTransformPara);
    procedure Draw;
  protected
    {$IFDEF FPC_HAS_CONSTREF}
    function QueryInterface(constref iid: TGuid; out obj): longint; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    function _AddRef: longint; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    function _Release: longint; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    {$ELSE}
    function QueryInterface(const IID: TGUID; out Obj): longint; stdcalL;
    function _AddRef: longint; stdcalL;
    function _Release: longint; stdcalL;
    {$ENDIF}
  end;

  TArc = class(TObject)
  private
    FCenter : TCoordinate;
    FRadius : double;
    FStartAngle : double;
    FEndAngle : double;
    FPenColor : integer;
    FPenWidth : integer;
    FCanvas : TCanvas;
    FPenStyle : TPenStyle;
    FTransfrm : TTransformPara;
  public
    property Center : TCoordinate read FCenter write FCenter;
    property Radius : double read FRadius write FRadius;
    property StartAngle : double read FStartAngle write FStartAngle;
    property EndAngle : double read FEndAngle write FEndAngle;
    property PenColor : integer read FPenColor write FPenColor;
    property PenWidth : integer read FPenWidth write FPenWidth;
    property Canvas : TCanvas read FCanvas write FCanvas;
    property PenStyle : TPenStyle read FPenStyle write FPenStyle;
  end;

  TCxArc = class(TArc, IGraphic)
    procedure SetTransform(const TransformPara : TTransformPara);
    procedure Draw;
  protected
    {$IFDEF FPC_HAS_CONSTREF}
    function QueryInterface(constref iid: TGuid; out obj): longint; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    function _AddRef: longint; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    function _Release: longint; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    {$ELSE}
    function QueryInterface(const IID: TGUID; out Obj): longint; stdcalL;
    function _AddRef: longint; stdcalL;
    function _Release: longint; stdcalL;
    {$ENDIF}
  end;

  TGPSTarget = class(TPaintBox)
  private
    FBmpBackground : TBitmap;
    FBmpBuffer     : TBitmap;
    FMargin    : integer;
    FTransfrm  : TTransformPara;
    FOnpaint   : TNotifyEvent;
    FPolylines : TList;   //Stores TCxPolyline in the list.
    FArcs      : TList;   //Stores TCxArc in the list.
    FAzimuth   : double;  //Need azimuth data for drawing direction of arrow.
    FDistance  : double;
    FWidth     : integer;
    FHeight    : integer;
    FArrowType : TArrowType;
    FUseTarCoor : TTargetCoordinateType;
    FTargetGeoCoor : TGeoCoordinate;
    FCoorType      : TCoordinateType;
    FEllipsoidList : TCollection;
    FEllipsoid     : TEllipsoidCollectionItem;
    function   DrawCircle(cv : TCanvas; Center : TPoint; cRadius : integer): boolean; virtual;
    function   DrawArc(cv : TCanvas; Center : TPoint; cRadius : integer;
                  dStartDegrees, dSweepDegrees : double): boolean; virtual;
    function   SetTransformationPara(Tx, Ty, Scale, RotatedAng : double) : TTransformPara;
    procedure  CreateBmpBackgroundElements;
    procedure  CreateArrow1;
    procedure  CreateArrow2;
    procedure  CreateArrow3;
    procedure  ScaleArrow;
    procedure  DrawArrow;
    procedure  SetTargetCoordinate (const coor : TGeoCoordinate);
    procedure  SetDrawMargin(AMargin : integer);
    procedure  SetArrowType(ArrowType : TArrowType);
    procedure  SetEllipsoid(const Value : TGeoEllipsoidType);
    procedure  DrawAzimuthDistText(cv : TCanvas);
    function   DrawRotatedText(cv : TCanvas; txt : string; InsertPt : TPoint;
                              RotateAngle : double; HozAlign : THozAlignment;
                              VerAlign : TVerAlignment): boolean; virtual;
    procedure  DrawGPSTarget (GPSCoor : TGeoCoordinate);
    procedure  SetNMEADecode(const NMEA : TNMEADecode);
    procedure  DoRxNMEA(Sender : TObject; NMEA : TNMEADecode);
  protected
    procedure ChangeBounds(ALeft, ATop, AWidth, AHeight: integer; KeepBase: boolean); override;
    procedure Resize; override;
    procedure Loaded; override;
    procedure WMEraseBkgnd(var Msg: TLMessage); message LM_ERASEBKGND;
  public
    procedure Paint; override;
    property  TargetAzimuth : double read FAzimuth write FAzimuth;
    property  TargetDistance : double read FDistance write FDistance;
    property  NMEADecode : TNMEADecode write SetNMEADecode;
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  published
    property TargetCoordinateType : TTargetCoordinateType read FUseTarCoor write FUseTarCoor;
    //if no $GPWPL is defined we can use TargetGeocoordinate instead.
    property TargetGeoCoordinate : TGeoCoordinate read FTargetGeoCoor write SetTargetCoordinate;
    property DrawMargin : integer read FMargin write SetDrawMargin default 24;
    property Arrow : TArrowType read FArrowType write SetArrowType default atArrow2;
    property OnPaint : TNotifyEvent read FOnPaint write FOnPaint;
    property Align;
    property Width default 200;
    property Height default 200;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('OpenGPSX', [TGPSTarget]);
end;

constructor TGPSTarget.Create(AOwner: TComponent);
var
  NewItem : TEllipsoidCollectionItem;
begin
  inherited Create(AOwner);
  Parent := TWinControl(AOwner);
  ControlStyle := ControlStyle + [csOpaque];

  FEllipsoidList := TCollection.Create(TEllipsoidCollectionItem);

  NewItem := TEllipsoidCollectionItem.Create(FEllipsoidList);
  NewItem.EllipsoidName := 'WGS84';
  NewItem.MajorAxis := 6378137;
  NewItem.InvFlattening := 298.257223563;

  NewItem := TEllipsoidCollectionItem.Create(FEllipsoidList);
  NewItem.EllipsoidName := 'GRS80';
  NewItem.MajorAxis := 6378137;
  NewItem.InvFlattening := 298.257222101;

  NewItem := TEllipsoidCollectionItem.Create(FEllipsoidList);
  NewItem.EllipsoidName := 'WGS72';
  NewItem.MajorAxis := 6378135;
  NewItem.InvFlattening := 298.260;

  NewItem := TEllipsoidCollectionItem.Create(FEllipsoidList);
  NewItem.EllipsoidName := 'Australian 1965';
  NewItem.MajorAxis := 6378160;
  NewItem.InvFlattening := 298.250;

  NewItem := TEllipsoidCollectionItem.Create(FEllipsoidList);
  NewItem.EllipsoidName := 'Krasovsky 1940';
  NewItem.MajorAxis := 6378245;
  NewItem.InvFlattening := 298.3;

  NewItem := TEllipsoidCollectionItem.Create(FEllipsoidList);
  NewItem.EllipsoidName := 'International (1924)';
  NewItem.MajorAxis := 6378388;
  NewItem.InvFlattening := 297;

  NewItem := TEllipsoidCollectionItem.Create(FEllipsoidList);
  NewItem.EllipsoidName := 'Clake 1880';
  NewItem.MajorAxis := 6378249.1;
  NewItem.InvFlattening := 293.460;

  NewItem := TEllipsoidCollectionItem.Create(FEllipsoidList);
  NewItem.EllipsoidName := 'Clarke 1866';
  NewItem.MajorAxis := 6378206.4;
  NewItem.InvFlattening := 294.980;


  NewItem := TEllipsoidCollectionItem.Create(FEllipsoidList);
  NewItem.EllipsoidName := 'Airy 1830';
  NewItem.MajorAxis := 6377563.4;
  NewItem.InvFlattening := 299.320;

  NewItem := TEllipsoidCollectionItem.Create(FEllipsoidList);
  NewItem.EllipsoidName := 'Bessel 1841';
  NewItem.MajorAxis := 6377397.2;
  NewItem.InvFlattening := 299.150;

  NewItem := TEllipsoidCollectionItem.Create(FEllipsoidList);
  NewItem.EllipsoidName := 'Everest 1830';
  NewItem.MajorAxis := 6377276.345;
  NewItem.InvFlattening := 300.8017;

  NewItem := TEllipsoidCollectionItem.Create(FEllipsoidList);
  NewItem.EllipsoidName := 'Hayford 1909';
  NewItem.MajorAxis := 6378388.0;
  NewItem.InvFlattening := 296.999362081575;


  NewItem := TEllipsoidCollectionItem.Create(FEllipsoidList);
  NewItem.EllipsoidName := 'North American 1927';
  NewItem.MajorAxis := 6378206.4;
  NewItem.InvFlattening := 294.978698213898;

  NewItem := TEllipsoidCollectionItem.Create(FEllipsoidList);
  NewItem.EllipsoidName := 'NAD 83';
  NewItem.MajorAxis := 6378137;
  NewItem.InvFlattening := 298.257223563;

  FEllipsoid := TEllipsoidCollectionItem(FEllipsoidList.Items[0]);

  FMargin := 24;
  Color := clWhite;
  Width := 200;
  Height := 200;
  FWidth := Width;
  FHeight := Height;
  FArrowType := atArrow2;
  FAzimuth := 0;
  FDistance := 0;
  Constraints.MinWidth := 100;
  Constraints.MinHeight := 100;
  Constraints.MaxWidth := 1000;
  Constraints.MaxHeight := 1000;
  FTargetGeoCoor  := TGeoCoordinate.Create;

  FPolyLines := TList.Create;
  FArcs := TList.Create;
  FBmpBackground := TBitmap.Create;
  FBmpBuffer := TBitmap.Create;

  FBmpBackground.SetSize(FWidth, FHeight);
  FBmpBuffer.SetSize(FWidth, FHeight);
end;

destructor TGPSTarget.Destroy;
var
  i : integer;
  cx : TCxPolyline;
  ac : TCxArc;
begin
  FEllipsoid.Free;
  FEllipsoidList.Free;
  FBmpBackground.Free;
  for i := FPolyLines.Count - 1 downto 0 do
  begin
    cx := TCxPolyline(FPolyLines.Items[i]);
    FPolyLines.Remove(cx);
  end;

  for i := FArcs.Count - 1 downto 0 do
  begin
    ac := TCxArc(FArcs.Items[i]);
    FArcs.Remove(ac);
  end;

  FPolyLines.Free;
  FArcs.Free;
  FTargetGeoCoor.Free;
  inherited Destroy;
end;

procedure TGPSTarget.Loaded;
begin
  FWidth := Width;
  FHeight := Height;
  FBmpBackground.SetSize(FWidth, FHeight);
  FBmpBuffer.SetSize(FWidth, FHeight);
  CreateBmpBackgroundElements;
  if(FArrowType = atArrow1) then
    CreateArrow1
  else if(FArrowType = atArrow2) then
    CreateArrow2
  else if(FArrowType = atArrow3) then
    CreateArrow3
  else
    CreateArrow2;
  ScaleArrow;

  inherited Loaded;
end;

procedure  TGPSTarget.DoRxNMEA(Sender : TObject; NMEA : TNMEADecode);
var
  geocoor, wpcoor : TGeoCoordinate;
  dLat, dLong : double;
  f1, f2 : boolean;
begin
  if Assigned(NMEA) then
  begin
    if (NMEA.MessageType = RMCMsg) then
    begin
      try
        geocoor := TGeoCoordinate.Create;
        if Not((NMEA.RMC.LatitudeDM = '') and (NMEA.RMC.LongitudeDM = '')) then
        begin
          f1 := trystrtofloat(NMEA.RMC.LatitudeDegree, dLat);
          f2 := trystrtofloat(NMEA.RMC.LongitudeDegree, dLong);
          if (f1 and f2) then
          begin
            geocoor.Latitude := dLat;
            geocoor.Longitude := dLong;
            DrawGPSTarget(geocoor);
          end;
        end;
      finally
        geocoor.Free;
      end;
    end
    else if (NMEA.MessageType = GGAMsg) then
    begin
      try
        geocoor := TGeoCoordinate.Create;
        if Not((NMEA.GGA.LatitudeDM = '') and (NMEA.GGA.LongitudeDM = '')) then
        begin
          f1 := trystrtofloat(NMEA.GGA.LatitudeDegree, dLat);
          f2 := trystrtofloat(NMEA.GGA.LongitudeDegree, dLong);
          if (f1 and f2) then
          begin
            geocoor.Latitude := dLat;
            geocoor.Longitude := dLong;
            DrawGPSTarget(geocoor);
          end;
        end;
      finally
        geocoor.Free;
      end;
    end
    else if (NMEA.MessageType = WPLMsg) and (FUseTarCoor = ttUseWPL) then
    begin
      try
       wpcoor := TGeoCoordinate.Create;
       f1 := trystrtofloat(NMEA.WPL.Latitude, dLat);
       f2 := trystrtofloat(NMEA.WPL.Longitude, dLong);
       if (f1 and f2) then
       begin
         wpcoor.Latitude := dLat;
         wpcoor.Longitude := dLong;
         TargetGeoCoordinate := wpcoor;
       end;
     finally
       wpcoor.Free;
     end;
    end;
  end;
end;

procedure TGPSTarget.Paint;
var
  r : TRect;
begin
  r := Rect(0, 0, FWidth, FHeight);

  if csDesigning in ComponentState then
  with Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := Color;
    FillRect(r);
    CreateBmpBackgroundElements;
    FBmpBuffer.Canvas.CopyRect(r, FBmpBackground.Canvas, r);
    DrawArrow;
    Draw(0, 0, FBmpBuffer);
    exit;
  end;

  if Assigned(FOnPaint) then
  begin
    FBmpBuffer.Canvas.CopyRect(r, FBmpBackground.Canvas, r);
    DrawArrow;
    DrawAzimuthDistText(FBmpBuffer.Canvas);
    Canvas.Draw(0, 0, FBmpBuffer);
  end
  else
  begin
    FBmpBuffer.Canvas.CopyRect(r, FBmpBackground.Canvas, r);
    DrawArrow;
    DrawAzimuthDistText(FBmpBuffer.Canvas);
    Canvas.Draw(0, 0, FBmpBuffer);
  end;
  inherited Paint;
end;

procedure TGPSTarget.SetNMEADecode(const NMEA : TNMEADecode);
begin
  if Assigned(NMEA) then
    NMEA.OnGPSTarget := @DoRxNMEA;
end;

procedure TGPSTarget.SetEllipsoid(const Value : TGeoEllipsoidType);
begin
  case Value of
    geWGS84 : FEllipsoid := TEllipsoidCollectionItem(FEllipsoidList.Items[0]);
    geGRS80 : FEllipsoid := TEllipsoidCollectionItem(FEllipsoidList.Items[1]);
    geWGS72 : FEllipsoid := TEllipsoidCollectionItem(FEllipsoidList.Items[2]);
    geAustraliean_1965 : FEllipsoid := TEllipsoidCollectionItem(FEllipsoidList.Items[3]);
    geKrasovsky_1940 : FEllipsoid := TEllipsoidCollectionItem(FEllipsoidList.Items[4]);
    geInternational_1924 : FEllipsoid := TEllipsoidCollectionItem(FEllipsoidList.Items[5]);
    geClake_1880 : FEllipsoid := TEllipsoidCollectionItem(FEllipsoidList.Items[6]);
    geClake_1866 : FEllipsoid := TEllipsoidCollectionItem(FEllipsoidList.Items[7]);
    geAiry_1830 : FEllipsoid := TEllipsoidCollectionItem(FEllipsoidList.Items[8]);
    geBessel_1841 : FEllipsoid := TEllipsoidCollectionItem(FEllipsoidList.Items[9]);
    geEverest_1830 : FEllipsoid := TEllipsoidCollectionItem(FEllipsoidList.Items[10]);
    geHayford_1909 : FEllipsoid := TEllipsoidCollectionItem(FEllipsoidList.Items[11]);
    geNorth_American_1927 : FEllipsoid := TEllipsoidCollectionItem(FEllipsoidList.Items[12]);
    geNAD_83 : FEllipsoid := TEllipsoidCollectionItem(FEllipsoidList.Items[13]);
  end;
end;

procedure TGPSTarget.WMEraseBkgnd(var Msg: TLMessage);
begin
  Msg.Result := 1;
end;

procedure TGPSTarget.DrawGPSTarget(GPSCoor : TGeoCoordinate);
var
  dGPSLat, dGPSLong : double;
  geogpscoor : TGeoCoordinate;
  ellcalc : TSpheroidCompute;
  ell : TEllipsoid;
begin
  try
    ell := TEllipsoid.Create;
    geogpscoor := TGeoCoordinate.Create;
    ellcalc := TSpheroidCompute.Create;
    dGPSLat := GPSCoor.Latitude;
    dGPSLong := GPSCoor.Longitude;
    geogpscoor.Latitude := dGPSLat;
    geogpscoor.Longitude := dGPSLong;
    if (FCoorType = ctGeo) then
    begin
      ell.EllipsoidName := FEllipsoid.EllipsoidName;
      ell.MajorAxis := FEllipsoid.MajorAxis;
      ell.InvFlattening := FEllipsoid.InvFlattening;
      ellcalc.Ellipsoid := ell;
      ellcalc.GeoCoordinate1 := Coordinate(geogpscoor.Longitude, geogpscoor.Latitude);
      ellcalc.GeoCoordinate2 := Coordinate(FTargetGeoCoor.Longitude, FTargetGeoCoor.Latitude);
      ellcalc.Compute;
      FDistance := ellcalc.EllipseDistance;
      FAzimuth  := ellcalc.ForwardAzimuth;
    end;
  finally
    ell.Free;
    ellcalc.Free;
    geogpscoor.Free;
  end;
  Invalidate;
end;

procedure TGPSTarget.SetDrawMargin(AMargin : integer);
begin
  FMargin := AMargin;
  CreateBmpBackgroundElements;
  invalidate;
end;

procedure TGPSTarget.SetArrowType(ArrowType : TArrowType);
var
  i : integer;
begin
  FArrowType := ArrowType;
  for i := FPolylines.Count - 1 downto 0 do
    FPolylines.Delete(i);
  for i:= FArcs.Count - 1 downto 0 do
    FArcs.Delete(i);

  CreateBmpBackgroundElements;
  if(FArrowType = atArrow1) then
    CreateArrow1
  else if(FArrowType = atArrow2) then
    CreateArrow2
  else if(FArrowType = atArrow3) then
    CreateArrow3
  else
    CreateArrow2;

  ScaleArrow;
  invalidate;
end;

procedure TGPSTarget.DrawArrow;
var
  i : integer;
  tfm : TTransformPara;
  ig : IGraphic;
begin
  with tfm do
  begin
    TX1 := (FWidth - 2 * FMargin) / 2.0 + FMargin;
    TY1 := (FHeight - 2 * FMargin) / 2.0 + FMargin;
    ScaleX := 1;
    RotationAngle := -FAzimuth;
  end;


  for i := 0 to FPolylines.Count - 1 do
  begin
    ig := TCxPolyline(FPolylines.Items[i]);
    ig.SetTransform(tfm);
    ig.draw;
  end;

  for i := 0 to FArcs.Count - 1 do
  begin
    ig := TCxArc(FArcs.Items[i]);
    ig.SetTransform(tfm);
    ig.draw;
  end;

end;

procedure  TGPSTarget.DrawAzimuthDistText(cv : TCanvas);
var
  r3, r4 : TRect;
  x3, y3, x4, y4 : integer;
  szAz, szDst : string;
  ts : TSize;
begin
  cv.Brush.Style := bsClear;
  cv.Font.Style := [fsBold, fsItalic];
  {$IFDEF WINDOWS}
  cv.Font.Name := 'ARIAL';
  cv.Font.Size := trunc(3.3/100 * FHeight);
  {$ELSE}
  cv.Font.Name := 'Sans';
  cv.Font.Size := trunc(3.3/100 * FHeight);
  {$ENDIF}
  cv.Font.Color := clBlack;
  szAz := format('Bearing : %5.1f ', [FAzimuth]);
  ts := cv.TextExtent(szAz);
  x3 := 0;
  y3 := FHeight - ts.cy;
  r3 := Rect(x3, y3, x3 + ts.cx, y3 + ts.cy);
  cv.TextRect(r3, x3, y3 , szAz);
  szDst := format('Distance : %6.2f Km.', [FDistance/1000]);
  ts := cv.TextExtent(szDst);
  x4 := FWidth - ts.cx;
  y4 := FHeight - ts.cy;
  r4 := Rect(x4, y4, FWidth, FHeight);
  cv.TextRect(r4, x4, y4 , szDst);

end;

procedure TGPSTarget.CreateBmpBackgroundElements;
var
  fnt : TFont;
  sk1, sk2, img1, img2 : TPoint;
  i, ArrowHalfW, ArrowHalfH : integer;
  rad : integer;
  dtx, dty, dscale, dAngs : double;
  w, h : integer;
begin
  w := FWidth;
  h := FHeight;
  FBmpBuffer.SetSize(FWidth, FHeight);
  FBmpBackground.SetSize(FWidth, FHeight);
  ArrowHalfW := (w - 2 * FMargin) div 2;
  ArrowHalfH := (h - 2 * FMargin) div 2;
  dtx := ArrowHalfW + FMargin;
  dty := ArrowHalfH + FMargin;
  dScale := 1.0;
  dAngs := 0.0;
  FTransfrm := SetTransformationPara(dtx, dty, dscale, dAngs);

  with FBmpBackground do
  begin
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := Color;
    Canvas.FillRect(Rect(0, 0, w, h));
    Canvas.Pen.Color := clBlack;
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Width := 1;

    rad := ArrowHalfW;
    //Middle horizontal line
    sk1 := Polar2Cartesian(rad, 90);
    sk2 := Polar2Cartesian(rad, 270);
    img1 := Cartesian2Pixel(sk1, FTransfrm);
    img2 := Cartesian2Pixel(sk2, FTransfrm);
    Canvas.MoveTo(img1.X, img1.y);
    Canvas.LineTo(img2.X, img2.y);

    //Middle vertical line
    sk1 := Polar2Cartesian(rad, 0);
    sk2 := Polar2Cartesian(rad, 180);
    img1 := Cartesian2Pixel(sk1, FTransfrm);
    img2 := Cartesian2Pixel(sk2, FTransfrm);
    Canvas.MoveTo(img1.X, img1.y);
    Canvas.LineTo(img2.X, img2.y);

    dAngs := 0; //Accumulated angle.
    for i := Low(TargetBackground) to High(TargetBackground) do
    begin
      try
        with TargetBackground[i] do
        begin
          fnt := TFont.Create;
          fnt.Name := FaceName;
          fnt.Style := fnt.Style + [Weight];
          fnt.Color := Color;
          {fnt.Size := trunc(3.5/100 * h);}
          fnt.Size := Height;
          Canvas.Font := fnt;
          sk1 :=  Polar2Cartesian(rad, dAngs);
          img1 := Cartesian2Pixel(sk1, FTransfrm);
          DrawRotatedText(Canvas, Text, sk1, RotationAngle, HozAlignment, VerAlignment);
          dAngs := dAngs + 90;
        end;
      finally
        fnt.Free;
      end;
    end;

    //Elevation circle
    Canvas.Brush.Style := bsClear;
    Canvas.Pen.Width := 1;
    Canvas.Pen.Color := clBlack;
    Canvas.Pen.Style := psSolid;
    //Center of Sky Plot
    sk1 := Point(0, 0);
    img1 := Cartesian2Pixel(sk1, FTransfrm);
    //Draw elevation circle
    DrawCircle(Canvas, img1, ArrowHalfW);
  end; //with
end;

procedure TGPSTarget.ScaleArrow;
var
  cx : TCxPolyline;
  ac : TCxArc;
  i, j : integer;
  c : TCoordinate;
  ap : TCoordinateArray;
begin
  for i := 0 to FPolylines.Count - 1 do
  begin
    cx := TCxPolyline(FPolylines.Items[i]);
    ap := cx.Lines;
    for j := Low(ap) to High(ap) do
    begin
      c := ap[j];
      c.X := trunc(FWidth / 39 * c.x);
      c.Y := trunc(FHeight / 39 * c.y);
      ap[j] := c;
    end;
  end;

  for i := 0 to FArcs.Count - 1 do
  begin
    ac := TCxArc(FArcs.Items[i]);
    ac.Radius := (FWidth / 39) * ac.Radius;
  end;

end;

//Write to use instead of Canvas.Arc that can not specify center point.
function TGPSTarget.DrawArc(cv : TCanvas; Center : TPoint; cRadius : integer;
               dStartDegrees, dSweepDegrees : double) : boolean;
var
  StartPt : TPoint;  // End point of starting radial line
  EndPt : TPoint;        // End point of ending radial line
  dStartRadians : double;   // Start angle in radians
  dEndRadians : double;     // End angle in radians
  dDep,dLat : double;
begin
  // Get the starting and ending angle in radians
  if (dSweepDegrees > 0.0) then
  begin
    dStartRadians := dStartDegrees * DEGREETORADIANS;
    dEndRadians := (dStartDegrees + dSweepDegrees) * DEGREETORADIANS;
  end
  else
  begin
    dStartRadians:=(dStartDegrees + dSweepDegrees) * DEGREETORADIANS;
    dEndRadians:= dStartDegrees * DEGREETORADIANS;
  end;

  //Calculate a point on the starting radial line via
  // polar -> cartesian conversion
  dDep := cRadius * cos(dStartRadians);
  dLat := cRadius * sin(dStartRadians);
  StartPt.X := Center.X + strtoint(formatfloat('0',dDep));
  StartPt.Y := Center.Y - strtoint(formatfloat('0',dLat));

  // Calculate a point on the ending radial line via
  // polar -> cartesian conversion
  dDep := cRadius * cos(dEndRadians);
  dLat := cRadius * sin(dEndRadians);
  EndPt.X := Center.X + strtoint(formatfloat('0',dDep));
  EndPt.Y := Center.Y - strtoint(formatfloat('0',dLat));

  // Draw the arc
  cv.Arc(Center.x-cRadius,Center.Y-cRadius,Center.x+cRadius,Center.Y+cRadius,
                StartPt.x, StartPt.y, EndPt.X, EndPt.Y);

end;

function TGPSTarget.SetTransformationPara(Tx, Ty, Scale, RotatedAng : double) : TTransformPara;
begin
  with result do
  begin
    TX1 := Tx;//(Width - 2 * FMargin) / 2.0 + FMargin;
    TY1 := Ty;//(Height - 2 * FMargin) / 2.0 + FMargin;
    ScaleX := Scale;
    RotationAngle := RotatedAng;
  end;
end;

procedure TGPSTarget.CreateArrow1;
var
  pl : TCxPolyline;
  ap : TCoordinateArray;
begin
  pl := TCxPolyline.Create;
  SetLength(ap, 10);
  ap[0].X :=   2.4951;  ap[0].Y :=  -0.5000;
  ap[1].X :=   2.4951;  ap[1].Y :=   0.5000;
  ap[2].X :=   5.3499;  ap[2].Y :=   0.5000;
  ap[3].X :=  -0.0920;  ap[3].Y :=  10.0233;
  ap[4].X :=  -5.5339;  ap[4].Y :=   0.5000;
  ap[5].X :=  -2.8083;  ap[5].Y :=   0.5000;
  ap[6].X :=  -2.8083;  ap[6].Y :=  -0.5000;
  ap[7].X :=  -7.2571;  ap[7].Y :=  -0.5000;
  ap[8].X :=  -0.0920;  ap[8].Y :=  12.0389;
  ap[9].X :=   7.0731;  ap[9].Y :=  -0.5000;
  pl.Lines := ap;
  pl.PenColor := clRed;
  pl.BrushColor := clRed;
  pl.BrushStyle := bsSolid;
  pl.PenWidth := 1;
  pl.Canvas := FBmpBuffer.Canvas;
  FPolylines.Add(pl);

  pl := TCxPolyline.Create;
  //This polyline has only 2 points (begin and end point).
  SetLength(ap, 2);
  //Set coordinates of line.
  ap[0].X :=  -1.4729;  ap[0].Y := 0.0000;
  ap[1].X := -11.0315;  ap[1].Y := 0.0000;
  pl.Lines := ap;
  //Assign some properties.
  pl.PenColor := clRed;
  pl.PenWidth := 1;
  //Assign to buffer bitmap(Canvas of FBmpBuffer bitmap).
  pl.Canvas := FBmpBuffer.Canvas;
  //Then add polyline to TList.
  FPolylines.Add(pl);

  pl := TCxPolyline.Create;
  SetLength(ap, 2);
  ap[0].X := -0.0920;  ap[0].Y := 11.0311;
  ap[1].X := -0.0920;  ap[1].Y := 16.2056;
  pl.Lines := ap;
  pl.PenColor := clRed;
  pl.PenWidth := 1;
  pl.Canvas := FBmpBuffer.Canvas;
  FPolylines.Add(pl);

  pl := TCxPolyline.Create;
  SetLength(ap, 2);
  ap[0].X :=  1.2889;  ap[0].Y := 0.0000;
  ap[1].X := 11.0315;  ap[1].Y := 0.0000;
  pl.Lines := ap;
  pl.PenColor := clRed;
  pl.PenWidth := 1;
  pl.Canvas := FBmpBuffer.Canvas;
  FPolylines.Add(pl);

  pl := TCxPolyline.Create;
  SetLength(ap, 8);
  ap[0].X :=  -3.8083; ap[0].Y :=  -0.5000;
  ap[1].X :=  -3.8083; ap[1].Y :=  -9.7501;
  ap[2].X :=   3.4951; ap[2].Y :=  -9.7501;
  ap[3].X :=   3.4951; ap[3].Y :=  -0.5000;
  ap[4].X :=   2.4951; ap[4].Y :=  -0.5000;
  ap[5].X :=   2.4951; ap[5].Y :=  -8.7501;
  ap[6].X :=  -2.8083; ap[6].Y :=  -8.7501;
  ap[7].X :=  -2.8083; ap[7].Y :=  -0.5000;
  pl.Lines := ap;
  pl.PenColor := clRed;
  pl.BrushColor := clRed;
  pl.BrushStyle := bsSolid;
  pl.PenWidth := 1;
  pl.Canvas := FBmpBuffer.Canvas;
  FPolylines.Add(pl);
end;

procedure TGPSTarget.CreateArrow2;
var
  //p : TCoordinate;
  pl : TCxPolyline;
  ac : TCxArc;
  ap : TCoordinateArray;
begin
  pl := TCxPolyline.Create;
  SetLength(ap, 3);
  ap[0].X :=   0.0000;; ap[0].Y :=  13.3746;
  ap[1].X :=   0.0000;; ap[1].Y :=   0.0000;
  ap[2].X :=  -5.0139;; ap[2].Y :=  -4.9565;
  pl.Lines := ap;
  pl.PenColor := clRed;
  pl.BrushColor := clRed;
  pl.BrushStyle := bsSolid;
  pl.PenWidth := 1;
  pl.Canvas := FBmpBuffer.Canvas;
  FPolylines.Add(pl);

  pl := TCxPolyline.Create;
  SetLength(ap, 3);
  ap[0].X :=  0.0000;; ap[0].Y :=  13.3746;
  ap[1].X :=  0.0000;; ap[1].Y :=   0.0000;
  ap[2].X :=  5.0139;; ap[2].Y :=  -4.9565;
  pl.Lines := ap;
  pl.PenColor := clRed;
  pl.BrushColor := clRed;
  pl.BrushStyle := bsClear;
  pl.PenWidth := 1;
  pl.Canvas := FBmpBuffer.Canvas;
  FPolylines.Add(pl);

  
  ac := TCxArc.Create;
  ac.Center := Coordinate(0, 0);
  ac.StartAngle := 349.1250;
  ac.EndAngle := 10.8794;
  ac.Radius := 8;
  ac.PenColor := clRed;
  ac.PenWidth := 2;
  ac.Canvas := FBmpBuffer.Canvas;
  FArcs.Add(ac);

  ac := TCxArc.Create;
  ac.Center := Coordinate(0, 0);
  ac.StartAngle := 345.0268;
  ac.EndAngle := 14.9794;
  ac.Radius := 7.0;
  ac.PenColor := clRed;
  ac.PenWidth := 1;
  ac.Canvas := FBmpBuffer.Canvas;
  FArcs.Add(ac);

  pl := TCxPolyline.Create;
  SetLength(ap, 2);
  ap[0].X := -4.9781;  ap[0].Y := -4.9212;
  ap[1].X := -7.5525;  ap[1].Y := -7.4661;
  pl.Lines := ap;
  pl.PenColor := clRed;
  pl.PenWidth := 1;
  pl.Canvas := FBmpBuffer.Canvas;
  FPolylines.Add(pl);

  pl := TCxPolyline.Create;
  SetLength(ap, 2);
  ap[0].X := 5.0000;  ap[0].Y := -4.8990;
  ap[1].X := 7.5857;  ap[1].Y := -7.4324;
  pl.Lines := ap;
  pl.PenColor := clRed;
  pl.PenWidth := 1;
  pl.Canvas := FBmpBuffer.Canvas;
  FPolylines.Add(pl);

  pl := TCxPolyline.Create;
  SetLength(ap, 2);
  ap[0].X :=  -3.6617;  ap[0].Y := 0.0000;
  ap[1].X := -10.6199;  ap[1].Y := 0.0000;
  pl.Lines := ap;
  pl.PenColor := clRed;
  pl.PenWidth := 1;
  pl.Canvas := FBmpBuffer.Canvas;
  FPolylines.Add(pl);

  pl := TCxPolyline.Create;
  SetLength(ap, 2);
  ap[0].X :=  -2.8733;  ap[0].Y :=  2.8733;
  ap[1].X :=  -7.3676;  ap[1].Y :=  7.3676;
  pl.Lines := ap;
  pl.PenColor := clRed;
  pl.BrushColor := clRed;
  pl.BrushStyle := bsDiagCross;
  pl.PenWidth := 1;
  pl.Canvas := FBmpBuffer.Canvas;
  FPolylines.Add(pl);

  pl := TCxPolyline.Create;
  SetLength(ap, 2);
  ap[0].X :=  2.8733;  ap[0].Y :=  2.8733;
  ap[1].X :=  7.3676;  ap[1].Y :=  7.3676;
  pl.Lines := ap;
  pl.PenColor := clRed;
  pl.BrushColor := clRed;
  pl.BrushStyle := bsDiagCross;
  pl.PenWidth := 1;
  pl.Canvas := FBmpBuffer.Canvas;
  FPolylines.Add(pl);

  pl := TCxPolyline.Create;
  SetLength(ap, 2);
  ap[0].X :=  3.6617;  ap[0].Y := 0.0000;
  ap[1].X := 10.6199;  ap[1].Y := 0.0000;
  pl.Lines := ap;
  pl.PenColor := clRed;
  pl.PenWidth := 1;
  pl.Canvas := FBmpBuffer.Canvas;
  FPolylines.Add(pl);
end;

procedure TGPSTarget.CreateArrow3;
var
  //p : TCoordinate;
  pl : TCxPolyline;
  ap : TCoordinateArray;
begin
  pl := TCxPolyline.Create;
  SetLength(ap, 4);
  ap[0].X :=   0.0000; ap[0].Y :=   8.0078;
  ap[1].X :=   0.0000; ap[1].Y :=  11.0311;
  ap[2].X :=  -7.4464; ap[2].Y :=  -2.0000;
  ap[3].X :=  -4.8616; ap[3].Y :=  -0.5000;
  pl.Lines := ap;
  pl.PenColor := clBlack;
  pl.BrushColor := clBlack;
  pl.BrushStyle := bsClear;
  pl.PenWidth := 1;
  pl.Canvas := FBmpBuffer.Canvas;
  FPolylines.Add(pl);

  pl := TCxPolyline.Create;
  SetLength(ap, 4);
  ap[0].X :=   0.0000; ap[0].Y :=  11.0311;
  ap[1].X :=   7.4464; ap[1].Y :=  -2.0000;
  ap[2].X :=   4.8616; ap[2].Y :=  -0.5000;
  ap[3].X :=   0.0000; ap[3].Y :=   8.0078;
  pl.Lines := ap;
  pl.PenColor := clBlack;
  pl.BrushColor := clBlack;
  pl.BrushStyle := bsSolid;
  pl.PenWidth := 1;
  pl.Canvas := FBmpBuffer.Canvas;
  FPolylines.Add(pl);

  pl := TCxPolyline.Create;
  SetLength(ap, 4);
  ap[0].X :=  -2.5000; ap[0].Y :=  -0.5000;
  ap[1].X :=  -4.8616; ap[1].Y :=  -0.5000;
  ap[2].X :=  -7.4464; ap[2].Y :=  -2.0000;
  ap[3].X :=  -4.0000; ap[3].Y :=  -2.0000;
  pl.Lines := ap;
  pl.PenColor := clBlack;
  pl.BrushColor := clGray;
  pl.BrushStyle := bsSolid;
  pl.PenWidth := 1;
  pl.Canvas := FBmpBuffer.Canvas;
  FPolylines.Add(pl);

  pl := TCxPolyline.Create;
  SetLength(ap, 4);
  ap[0].X :=  2.5000; ap[0].Y :=  -0.5000;
  ap[1].X :=  4.8616; ap[1].Y :=  -0.5000;
  ap[2].X :=  7.4464; ap[2].Y :=  -2.0000;
  ap[3].X :=  4.0000; ap[3].Y :=  -2.0000;
  pl.Lines := ap;
  pl.PenColor := clBlack;
  pl.BrushColor := clGray;
  pl.BrushStyle := bsSolid;
  pl.PenWidth := 1;
  pl.Canvas := FBmpBuffer.Canvas;
  FPolylines.Add(pl);

  pl := TCxPolyline.Create;
  SetLength(ap, 4);
  ap[0].X :=  -2.5000; ap[0].Y :=  -0.5000;
  ap[1].X :=  -2.5000; ap[1].Y :=  -4.5000;
  ap[2].X :=  -4.0000; ap[2].Y :=  -6.0000;
  ap[3].X :=  -4.0000; ap[3].Y :=  -2.0000;
  pl.Lines := ap;
  pl.PenColor := clBlack;
  pl.BrushColor := clBlack;
  pl.BrushStyle := bsClear;
  pl.PenWidth := 1;
  pl.Canvas := FBmpBuffer.Canvas;
  FPolylines.Add(pl);

  pl := TCxPolyline.Create;
  SetLength(ap, 4);
  ap[0].X :=  2.5000; ap[0].Y :=  -0.5000;
  ap[1].X :=  2.5000; ap[1].Y :=  -4.5000;
  ap[2].X :=  4.0000; ap[2].Y :=  -6.0000;
  ap[3].X :=  4.0000; ap[3].Y :=  -2.0000;
  pl.Lines := ap;
  pl.PenColor := clBlack;
  pl.BrushColor := clBlack;
  pl.BrushStyle := bsSolid;
  pl.PenWidth := 1;
  pl.Canvas := FBmpBuffer.Canvas;
  FPolylines.Add(pl);

  pl := TCxPolyline.Create;
  SetLength(ap, 4);
  ap[0].X :=  -2.5000; ap[0].Y :=  -4.5000;
  ap[1].X :=   2.5000; ap[1].Y :=  -4.5000;
  ap[2].X :=   4.0000; ap[2].Y :=  -6.0000;
  ap[3].X :=  -4.0000; ap[3].Y :=  -6.0000;
  pl.Lines := ap;
  pl.PenColor := clBlack;
  pl.BrushColor := clGray;
  pl.BrushStyle := bsSolid;
  pl.PenWidth := 1;
  pl.Canvas := FBmpBuffer.Canvas;
  FPolylines.Add(pl);

  pl := TCxPolyline.Create;
  SetLength(ap, 7);
  ap[0].X :=  -2.0000; ap[0].Y :=   0.0000;
  ap[1].X :=  -2.0000; ap[1].Y :=  -4.0000;
  ap[2].X :=   2.0000; ap[2].Y :=  -4.0000;
  ap[3].X :=   2.0000; ap[3].Y :=   0.0000;
  ap[4].X :=   4.0000; ap[4].Y :=   0.0000;
  ap[5].X :=   0.0000; ap[5].Y :=   7.0000;
  ap[6].X :=  -4.0000; ap[6].Y :=   0.0000;
  pl.Lines := ap;
  pl.PenColor := clBlack;
  pl.BrushColor := clBlack;
  pl.BrushStyle := bsClear;
  pl.PenWidth := 1;
  pl.Canvas := FBmpBuffer.Canvas;
  FPolylines.Add(pl);
end;

procedure  TGPSTarget.SetTargetCoordinate (const coor : TGeoCoordinate);
begin
  FTargetGeoCoor.Latitude := coor.Latitude;
  FTargetGeoCoor.Longitude := coor.Longitude;
end;

{procedure  TGPSTarget.SetTargetCoordinate (const coor : TUTMGridCoordinate);
begin
  FTargetGridCoor := coor;
end;}

//Draw circle
function TGPSTarget.DrawCircle(cv : TCanvas; Center : TPoint; cRadius : integer) : boolean;
var
  StartPt, EndPt : TPoint;
  dStartRadians : double;   // Start angle in radians
  dEndRadians : double;     // End angle in radians
  dDep,dLat : double;
  r : TRect;
begin
  dStartRadians := 0 * DEGREETORADIANS;
  dEndRadians := 360 * DEGREETORADIANS;
  //Calculate a point on the starting radial line via
  // polar -> cartesian conversion
  dDep := cRadius * cos(dStartRadians);
  dLat := cRadius * sin(dStartRadians);
  StartPt.X := Center.X + trunc(dDep);
  StartPt.Y := Center.Y - trunc(dLat);

  // Calculate a point on the ending radial line via
  // polar -> cartesian conversion
  dDep := cRadius * cos(dEndRadians);
  dLat := cRadius * sin(dEndRadians);
  EndPt.X := Center.X + trunc(dDep);
  EndPt.Y := Center.Y - trunc(dLat);

  r := Rect(Center.x-cRadius,Center.Y-cRadius,Center.x+cRadius,Center.Y+cRadius);
  // Draw the circle
  cv.Ellipse(r);
end;

function TGPSTarget.DrawRotatedText(cv : TCanvas; Txt : string; InsertPt : TPoint;
                                     RotateAngle : double; HozAlign : THozAlignment;
                                     VerAlign : TVerAlignment): boolean;
var
  dRad : double; //Rotation angle in radian mode.
  dinx, diny ,dtsx, dtsy : double;
  dshfx, dshfy : double; //Shift x, Shift y to align text.
  dtx, dty : double; //Translation x, Translation Y.
  dxp, dyp : double;
  dnewx, dnewy : double;
  dtang : double;  //rotation text angle to lforientation.
  cnew : TPoint;   //user coordinates (cartesian coordinates).
  img : TPoint;    //pixel coordinates.
  orient : longint;//property of ratation text (counter-clockwise x 10).
  txts : TSize;
begin
  //We need double type for math computation.
  dinx := InsertPt.X; //Insertion point (x) of text.
  diny := InsertPt.y; //Insertion point (y) of text.
  txts := cv.TextExtent(txt); //Text size of rotation text.
  dtsx := txts.cx; //Convert text size from integer to double.
  dtsy := txts.cy;

  dtx := dinx; //Convert insertion point from integer to double.
  dty := diny;

  //Manage shift cooridantes to the user alignment.
  //First check the horizontal alignment.
  case HozAlign of
    haLeft : dshfx := 0; //default of Canvas.Textout is horizontal left align.
    haRight : dshfx := -1 * dtsx;//Right alignment shift insertpoint point to right direction = length of text size (cx property).
    haCenter : dshfx := -1 * dtsx / 2.0;
  end;
  //Secord check the vertical alignment.
  case VerAlign of
    vaTop : dshfy := 0; //default of Canvas.Textout is vertical top align.
    vaBottom : dshfy := dtsy;
    VaCenter : dshfy := dtsy / 2.0;
  end;

  dxp := dshfx;
  dyp := dshfy;

  dRad := (90 - Rotateangle);
  if (dRad < 0) then
    dRad := 360 + dRad
  else if (dRad > 540) then
    dRad := dRad - 540
  else if (dRad > 360) then
    dRad := dRad - 360;

  dRad := dRad * DEGREETORADIANS;
  dnewx := dxp * cos(dRad) - dyp * sin(dRad) + dtx;
  dnewy := dxp * sin(dRad) + dyp * cos(dRad) + dty;
  cnew.X := trunc(dnewx);
  cnew.y := trunc(dnewy);
  img := Cartesian2Pixel(cnew, FTransfrm);
  dtang := 90 - RotateAngle; //counter-closewise angle
  if (dtang < 0) then dtang := 360 + dtang;
  orient := trunc(dtang * 10);
  cv.Font.Orientation := orient;
  cv.TextOut(img.x, img.y, txt);
end;

procedure TGPSTarget.Resize;
begin
  if csDesigning in ComponentState then
  begin
    Width := Height;//We need square size.
    FWidth := Height;
    FHeight := Height;
    Invalidate;
  end;
  inherited Resize;

end;

procedure TGPSTarget.ChangeBounds(ALeft, ATop, AWidth, AHeight: integer; KeepBase: boolean);
begin
  if csDesigning in ComponentState then
  begin
    AWidth := AHeight;//We need square size.
    FWidth := AWidth;
    FHeight := AHeight;
    Invalidate;
  end;
  inherited ChangeBounds(ALeft, ATop, AWidth, AWidth, KeepBase);
end;

{ TPolyline }
procedure TPolyline.SetPolyline(ALine: TCoordinateArray);
var
  i : integer;
begin
  SetLength(FLines, High(ALine) - Low(ALine) + 1);
  for i := Low(ALine) to High(ALine) do
    FLines[i] := ALine[i];
end;

{TCxLine}
procedure TCxPolyline.SetTransform(const TransformPara : TTransformPara);
begin
  FTransfrm := TransformPara;
end;

procedure TCxPolyline.draw;
var
  ap : array of TPoint;
  i : integer;
  p1 : TPoint;
begin
  FCanvas.Pen.Color := FPenColor;
  FCanvas.Pen.Width := FPenWidth;
  FCanvas.Pen.Style := FPenStyle;
  FCanvas.Brush.Color := FBrushColor;
  FCanvas.Brush.Style := FBrushStyle;

  SetLength(ap, High(FLines) - Low(FLines) + 1);
  for i := Low(FLines) to High(FLines) do
  begin
    p1.X := trunc(FLines[i].X);
    p1.Y := trunc(FLines[i].Y);
    ap[i] := Cartesian2Pixel(p1, FTransfrm);
  end;
  FCanvas.Polygon(ap);
end;

{$IFDEF FPC_HAS_CONSTREF}
function TCxPolyline.QueryInterface(constref iid: TGuid; out obj): longint; {$IFDEF WINDOWs}stdcall{$ELSE}cdecl{$ENDIF};
{$ELSE}
function TCxPolyline.QueryInterface(const IID: TGUID; out Obj): longint; stdcall;
{$ENDIF}
begin
  if GetInterface(iid, obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

{$IFDEF FPC_HAS_CONSTREF}
function TCxPolyline._AddRef: longint; {$IFDEF WINDOWs}stdcall{$ELSE}cdecl{$ENDIF};
{$ELSE}
function TCxPolyline._AddRef: longint; stdcall;
{$ENDIF}
begin
  Result := -1;
end;

{$IFDEF FPC_HAS_CONSTREF}
function TCxPolyline._Release: longint; {$IFDEF WINDOWs}stdcall{$ELSE}cdecl{$ENDIF};
{$ELSE}
function TCxPolyline._Release: longint; stdcall;
{$ENDIF}
begin
  Result := -1;
end;


{TCxArc}
procedure TCxArc.SetTransform(const TransformPara : TTransformPara);
begin
  FTransfrm := TransformPara;
end;

procedure TCxArc.draw;
var
  x1, y1, x2, y2, x3, y3, x4, y4 : integer;
  dStartRadians : double;   // Start angle in radians
  dEndRadians : double;     // End angle in radians
  dY, dX : double;
  p1, p2, p3, p4 : TPoint;
  temp : TTransformPara;
begin
  FCanvas.Pen.Color := FPenColor;
  FCanvas.Pen.Width := FPenWidth;
  FCanvas.Pen.Style := FPenStyle;
  dStartRadians := FStartAngle * DEGREETORADIANS;
  dEndRadians := (FEndAngle) * DEGREETORADIANS;

  x1 := trunc(FCenter.X - FRadius);
  y1 := trunc(FCenter.Y + FRadius);

  x2 := trunc(FCenter.X + FRadius);
  y2 := trunc(FCenter.Y - FRadius);

  //Calculate a point on the starting radial line via
  // polar -> cartesian conversion
  dY := FRadius * cos(dStartRadians);
  dX := FRadius * sin(dStartRadians);
  x3 := trunc(FCenter.X + dX);
  y3 := trunc(FCenter.Y + dY);

  // Calculate a point on the ending radial line via
  // polar -> cartesian conversion
  dY := FRadius * cos(dEndRadians);
  dX := FRadius * sin(dEndRadians);
  x4 := trunc(FCenter.X + dX);
  y4 := trunc(FCenter.Y + dY);

  Temp := FTransfrm;
  Temp.RotationAngle := 0;
  p1 := Cartesian2Pixel(Point(x1, y1), Temp);
  p2 := Cartesian2Pixel(Point(x2, y2), Temp);
  p3 := Cartesian2Pixel(Point(x3, y3), FTransfrm);
  p4 := Cartesian2Pixel(Point(x4, y4), FTransfrm);

  FCanvas.Arc(p1.X, p1.Y, p2.X, p2.Y, p3.X, p3.Y, p4.X, p4.Y);

end;

{$IFDEF FPC_HAS_CONSTREF}
function TCxArc.QueryInterface(constref iid: TGuid; out obj): longint; {$IFDEF WINDOWs}stdcall{$ELSE}cdecl{$ENDIF};
{$ELSE}
function TCxArc.QueryInterface(const IID: TGUID; out Obj): longint; stdcall;
{$ENDIF}
begin
  if GetInterface(iid, obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

{$IFDEF FPC_HAS_CONSTREF}
function TCxArc._AddRef: longint; {$IFDEF WINDOWs}stdcall{$ELSE}cdecl{$ENDIF};
{$ELSE}
function TCxArc._AddRef: longint; stdcall;
{$ENDIF}
begin
  Result := -1;
end;

{$IFDEF FPC_HAS_CONSTREF}
function TCxArc._Release: longint; {$IFDEF WINDOWs}stdcall{$ELSE}cdecl{$ENDIF};
{$ELSE}
function TCxArc._Release: longint; stdcall;
{$ENDIF}
begin
  Result := -1;
end;

Initialization

{$i gpstarget.lrs}

end.
