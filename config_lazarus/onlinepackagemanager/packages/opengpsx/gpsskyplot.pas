{ <GPSSkyPlot>

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

unit gpsskyplot;

interface

uses
  LResources, LCLType, LCLIntf, Graphics, Classes, ExtCtrls, SysUtils,
  Types, LMessages, Controls, Forms, gpsdatadef, gpsutilities, nmeadecode;


const
  {$IFDEF LINUX}
  SkyPlotBackground : array[0..11] of TRotationText = (
             (FaceName:'Sans';Color:clBlue;Height:9;Weight:fsBold;
              Text:'N';RotationAngle:90;HozAlignment:haCenter;VerAlignment:vaBottom),
             (FaceName:'Sans';Color:clBlack;Height:8;Weight:fsBold;
              Text:' 30';RotationAngle:30;HozAlignment:haLeft;VerAlignment:vaCenter),
             (FaceName:'Sans';Color:clBlack;Height:8;Weight:fsBold;
              Text:' 60';RotationAngle:60;HozAlignment:haLeft;VerAlignment:vaCenter),
             (FaceName:'Sans';Color:clBlue;Height:9;Weight:fsBold;
              Text:' E';RotationAngle:90;HozAlignment:haLeft;VerAlignment:vaCenter),
             (FaceName:'Sans';Color:clBlack;Height:8;Weight:fsBold;
              Text:' 120';RotationAngle:120;HozAlignment:haLeft;VerAlignment:vaCenter),
             (FaceName:'Sans';Color:clBlack;Height:8;Weight:fsBold;
              Text:' 150';RotationAngle:150;HozAlignment:haLeft;VerAlignment:vaCenter),
             (FaceName:'Sans';Color:clBlue;Height:9;Weight:fsBold;
              Text:'S';RotationAngle:90;HozAlignment:haCenter;VerAlignment:vaTop),
             (FaceName:'Sans';Color:clBlack;Height:8;Weight:fsBold;
              Text:'210 ';RotationAngle:30;HozAlignment:haRight;VerAlignment:vaCenter),
             (FaceName:'Sans';Color:clBlack;Height:8;Weight:fsBold;
              Text:'240 ';RotationAngle:60;HozAlignment:haRight;VerAlignment:vaCenter),
             (FaceName:'Sans';Color:clBlue;Height:9;Weight:fsBold;
              Text:'W ';RotationAngle:90;HozAlignment:haRight;VerAlignment:vaCenter),
             (FaceName:'Sans';Color:clBlack;Height:8;Weight:fsBold;
              Text:'300 ';RotationAngle:120;HozAlignment:haRight;VerAlignment:vaCenter),
             (FaceName:'Sans';Color:clBlack;Height:8;Weight:fsBold;
              Text:'330 ';RotationAngle:150;HozAlignment:haRight;VerAlignment:vaCenter)
  );
  {$ENDIF}
  {$IFDEF WINDOWS}
  SkyPlotBackground : array[0..11] of TRotationText = (
             (FaceName:'Arial';Color:clBlue;Height:10;Weight:fsBold;
              Text:'N';RotationAngle:90;HozAlignment:haCenter;VerAlignment:vaBottom),
             (FaceName:'Arial';Color:clBlack;Height:9;Weight:fsBold;
              Text:' 30';RotationAngle:30;HozAlignment:haLeft;VerAlignment:vaCenter),
             (FaceName:'Arial';Color:clBlack;Height:9;Weight:fsBold;
              Text:' 60';RotationAngle:60;HozAlignment:haLeft;VerAlignment:vaCenter),
             (FaceName:'Arial';Color:clBlue;Height:10;Weight:fsBold;
              Text:' E';RotationAngle:90;HozAlignment:haLeft;VerAlignment:vaCenter),
             (FaceName:'Arial';Color:clBlack;Height:9;Weight:fsBold;
              Text:' 120';RotationAngle:120;HozAlignment:haLeft;VerAlignment:vaCenter),
             (FaceName:'Arial';Color:clBlack;Height:9;Weight:fsBold;
              Text:' 150';RotationAngle:150;HozAlignment:haLeft;VerAlignment:vaCenter),
             (FaceName:'Arial';Color:clBlue;Height:10;Weight:fsBold;
              Text:'S';RotationAngle:90;HozAlignment:haCenter;VerAlignment:vaTop),
             (FaceName:'Arial';Color:clBlack;Height:9;Weight:fsBold;
              Text:'210 ';RotationAngle:30;HozAlignment:haRight;VerAlignment:vaCenter),
             (FaceName:'Arial';Color:clBlack;Height:9;Weight:fsBold;
              Text:'240 ';RotationAngle:60;HozAlignment:haRight;VerAlignment:vaCenter),
             (FaceName:'Arial';Color:clBlue;Height:9;Weight:fsBold;
              Text:'W ';RotationAngle:90;HozAlignment:haRight;VerAlignment:vaCenter),
             (FaceName:'Arial';Color:clBlack;Height:9;Weight:fsBold;
              Text:'300 ';RotationAngle:120;HozAlignment:haRight;VerAlignment:vaCenter),
             (FaceName:'Arial';Color:clBlack;Height:9;Weight:fsBold;
              Text:'330 ';RotationAngle:150;HozAlignment:haRight;VerAlignment:vaCenter)
  );

  {$ENDIF}
type

  TGPSSkyPlot = class(TPaintBox)
  private
    //Off-screen bitmap keeps background elements (lines, circles, texts)
    FBmpBackground : TBitmap;
    FRedSatImg, FGreenSatImg : TCustomBitmap; //GPS legend bitmap.
    FBmpMaskSat : TBitmap; //Mask for GPS legend bitmap.
    FBmpRedSpriteSat, FBmpGreenSpriteSat : TBitmap;
    FBmpSatDest : array [0..11] of TBitmap;
    FBmpTxtDest : array[0..11] of TCustomBitmap;

    FBmpBuffer : TBitmap; //Memory copy of image used in paint routine to build image.
    FSatSize : integer; //GPS legend size
    FTextW, FTextH : integer; //GPS PRN size.

    FDrawing : boolean;
    FMargin : integer;
    FSkyHalfW, FSkyHalfH : integer;
    FGSVInfos : TGSVInfos;
    FTransfrm : TTransformPara;
    FImgShift : integer;
    FOnpaint : TNotifyEvent;

    //FRefreshedState : integer;

    procedure CreateBackgroundImg;
    procedure CreateSatImg;
    function  SetTransformationPara(Tx, Ty, Scale, RotatedAng : double) : TTransformPara;
    function  DrawArc(cv : TCanvas; Center : TPoint; cRadius : integer;
                      dStartDegrees, dSweepDegrees : double): boolean; virtual;
    function  DrawCircle(cv : TCanvas; Center : TPoint; cRadius : integer): boolean; virtual;
    function  DrawRotatedText(cv : TCanvas; txt : string; InsertPt : TPoint;
                              RotateAngle : double; HozAlign : THozAlignment;
                              VerAlign : TVerAlignment): boolean; virtual;
    procedure DrawGPSSkyPlot(ASatInfos : TGSVInfos);
    procedure SetDrawMargin(AMargin : integer);
    procedure SetSatSize(ASize : integer);
    procedure CreateSatImage(const AColor : TColor; const TransColor : TColor;
                             var SatImg : TCustomBitmap);
    procedure CreateMask(TransColor : TColor; const BMPSprite : TCustomBitmap; var BMPMask:  TBitmap);
    procedure CreateSprite(TransColor : TColor; const BMPPrototype : TCustomBitmap; var BMPSprite :  TBitmap);
    procedure SetNMEADecode(const NMEA : TNMEADecode);
    procedure DoRxGSVInfos(Sender : TObject; GSVInfos : TGSVInfos);
  protected
    procedure WMEraseBkgnd(var Msg: TLMessage); message LM_ERASEBKGND;
    procedure Loaded; override;
    procedure ChangeBounds(ALeft, ATop, AWidth, AHeight: integer; KeepBase: boolean); override;
    procedure Resize; override;
  public
    procedure Paint; override;
    property NMEADecode : TNMEADecode write SetNMEADecode;
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  published
    property DrawMargin : integer read FMargin write SetDrawMargin default 20;
    property SatSize : integer read FSatSize write SetSatSize default 20;
    property Width default 200;
    property Height default 200;
    property Align;
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
  RegisterComponents('OpenGPSX',[TGPSSkyPlot]);
end;

constructor TGPSSkyPlot.Create(AOwner: TComponent);
var
  i : integer;
begin
  inherited Create(AOwner);

  //set component parent to the owner (usually form).
  Parent := TWinControl(AOwner);
  ControlStyle := ControlStyle + [csOpaque];
  FDrawing := false;
  Width := 200;
  Height := 200;
  FMargin := 20;
  FSatSize := 20;
  FImgShift := FSatSize div 2;
  FTextW := 15;
  FTextH := 13;
  Color := clWhite;
  Constraints.MinWidth := 100;
  Constraints.MinHeight := 100;
  Constraints.MaxWidth := 1000;
  Constraints.MaxHeight := 1000;

  FBmpBuffer := TBitmap.Create;
  FBmpBackground := TBitmap.Create;
  FBmpMaskSat := TBitmap.Create;

  FRedSatImg := TPortableNetworkGraphic.Create;
  FGreenSatImg := TPortableNetworkGraphic.Create;
  FRedSatImg.TransparentColor := clWhite;
  FRedSatImg.Transparent := true;
  FGreenSatImg.TransparentColor := clWhite;
  FGreenSatImg.Transparent := true;


  FBmpRedSpriteSat := TBitmap.Create;
  FBmpGreenSpriteSat := TBitmap.Create;

  for i := low(FBmpSatDest) to high(FBmpSatDest) do
  begin
    FBmpSatDest[i] := TBitmap.Create;
    FBmpSatDest[i].Width := FSatSize;
    FBmpSatDest[i].Height := FSatSize;
  end;

  for i := low(FBmpTxtDest) to high(FBmpTxtDest) do
  begin
    FBmpTxtDest[i] := TPortableNetworkGraphic.Create;
    FBmpTxtDest[i].Transparent := true;
    FBmpTxtDest[i].TransparentColor := self.Color;
  end;

  FBmpBuffer.SetSize(Width, Height);
  FBmpBackground.SetSize(Width, Height);

  CreateSatImg;
end;

destructor TGPSSkyPlot.Destroy;
var
  i : integer;
begin
  for i := low(FBmpSatDest) to high(FBmpSatDest) do
    FBmpSatDest[i].Free;
  for i := low(FBmpTxtDest) to high(FBmpTxtDest) do
    FBmpTxtDest[i].Free;

  FBmpBuffer.Free;
  FBmpBackground.Free;
  FRedSatImg.Free;
  FGreenSatImg.Free;
  FBmpMaskSat.Free;
  FBmpGreenSpriteSat.Free;
  FBmpRedSpriteSat.Free;
  inherited Destroy;
end;


procedure TGPSSkyPlot.Loaded;
var
  i : integer;
begin
  FBmpBuffer.Width := Width;
  FBmpBuffer.Height := Height;
  for i := low(FBmpSatDest) to high(FBmpSatDest) do
  begin
    FBmpSatDest[i].Width := FSatSize;
    FBmpSatDest[i].Height := FSatSize;
  end;
  FSkyHalfW := (Width - 2 * FMargin) div 2;
  FSkyHalfH := (Height - 2 * FMargin) div 2;
  CreateBackgroundImg;

  inherited Loaded;
end;


procedure TGPSSkyPlot.Paint;
begin
  if csDesigning in ComponentState then
  begin
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := Color;
    Canvas.FillRect(Rect(0, 0, Width, Height));
    CreateBackgroundImg;
    Canvas.Draw(0, 0, FBmpBackground);
    exit;
  end;

  if Assigned(FOnPaint) then
  begin
    FOnPaint(self);
    Canvas.Draw(0, 0, FBmpBackground);
  end
  else
    Canvas.Draw(0, 0, FBmpBackground);
  inherited Paint;
end;

procedure TGPSSkyplot.CreateSatImg;
var
  rSat : TRect;
begin
  FRedSatImg.SetSize(FSatSize, FSatSize);
  FGreenSatImg.SetSize(FSatSize, FSatSize);
  FBmpMaskSat.SetSize(FSatSize, FSatSize);
  FBmpRedSpriteSat.SetSize(FSatSize, FSatSize);
  FBmpGreenSpriteSat.SetSize(FSatSize, FSatSize);

  CreateSatImage(clRed, clwhite, FRedSatImg);
  CreateSatImage(clGreen, clWhite, FGreenSatImg);
  rSat := Rect(0, 0, FSatSize, FSatSize);
  //Create Mask of satelite legend. Black satelite legend on white background.
  CreateMask(clWhite, FGreenSatImg, FBmpMaskSat);
  //Create Red Sprite of satelite legend. Red satelite legend for unactive GPS status on black background.
  CreateSprite(clWhite, FRedSatImg, FBmpRedSpriteSat);
  //Create Green Sprite of satelite legend. Green satelite legend for active GPS status on black background.
  CreateSprite(clWhite, FGreenSatImg, FBmpGreenSpriteSat);
end;

procedure TGPSSkyPlot.SetNMEADecode(const NMEA : TNMEADecode);
begin
  if (Assigned(NMEA)) then
  begin
    NMEA.OnGPSSkyplot := @DoRxGSVInfos;
  end;
end;

procedure TGPSSkyplot.DoRxGSVInfos(Sender : TObject; GSVInfos : TGSVInfos);
begin
  if (Assigned(GSVInfos)) then
  begin
    DrawGPSSkyplot(GSVInfos);
  end;
end;

procedure TGPSSkyPlot.SetDrawMargin(AMargin: integer);
begin
  FMargin := AMargin;
  CreateBackgroundImg;
  invalidate;
end;

procedure TGPSSkyPlot.SetSatSize(ASize : integer);
begin
  FSatSize := ASize;
  CreateSatImg;
  invalidate;
end;

procedure TGPSSkyPlot.CreateSatImage(const AColor : TColor; const TransColor : TColor;
                                     var SatImg : TCustomBitmap);
var
  r : TRect;
  x1, y1, x2, y2 : double;
begin
  with SatImg do
  begin
    Transparent := true;
    Canvas.Brush.Color := TransColor;
    Canvas.Brush.Style := bsSolid;
    Canvas.FillRect(Rect(0, 0, FSatSize, FSatSize));
    Canvas.Brush.Color := AColor;
    Canvas.Pen.Color := AColor;
    Canvas.Pen.Style := psSolid;
    x1 := 6/20 * FSatSize;
    y1 := 6/20 * FSatSize;
    x2 := 14/20 * FSatSize;
    y2 := 14/20 * FSatSize;
    r := Rect(trunc(x1), trunc(y1), trunc(x2), trunc(y2));
    Canvas.Ellipse(r);
    x1 := 3/20 * FSatSize;
    y1 := 3/20 * FSatSize;
    x2 := 7/20 * FSatSize;
    y2 := 17/20 * FSatSize;
    r := Rect(trunc(x1), trunc(y1), trunc(x2), trunc(y2));
    Canvas.Rectangle(r);
    x1 := 13/20 * FSatSize;
    y1 := 3/20 * FSatSize;
    x2 := 17/20 * FSatSize;
    y2 := 17/20 * FSatSize;
    r := Rect(trunc(x1), trunc(y1), trunc(x2), trunc(y2));
    Canvas.Rectangle(r);
  end;
end;


//Create off-screen bitmap.
procedure TGPSSkyPlot.CreateBackgroundImg;
var
  fnt : TFont;
  sk1, sk2, img1, img2, cen : TPoint;
  rad: integer;
  i : longint;
  dtx,dty, dscale, dAngs : double;
  h, w : integer;
begin
  FSkyHalfW := (Width - 2 * FMargin) div 2;
  FSkyHalfH := (Height - 2 * FMargin) div 2;

  //Translation to center of image.
  dtx := FSkyHalfW + FMargin;
  dty := FSkyHalfH + FMargin;
  dscale := 1.0;
  dAngs := 0.0;
  FTransfrm := SetTransformationPara(dtx, dty, dscale, dAngs);
  h := Height;
  w := Width;
  FBmpBackground.Width := Width;
  FBmpBackground.Height := Height;
  with FBmpBackground do
  begin
    Transparent := false;
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := Color;

    Canvas.FillRect(Rect(0, 0, w, h));
    Canvas.Pen.Color := clRed;
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Width := 2;

    //Draw elevation circle of GPS.
    rad := FSkyHalfW;  //radius of elevation circle.

    Canvas.Brush.Style := bsClear;
    Canvas.Pen.Width := 1;
    Canvas.Pen.Style := psSolid;
    //Center of Sky Plot
    sk1 := Point(0, 0);
    cen := Cartesian2Pixel(sk1, FTransfrm);
    Canvas.Pen.Color := clRed;
    //Elevation = 0 (horizone line)
    DrawCircle(Canvas, cen, rad);
    Canvas.Pen.Color := clRed;
    //Elevation at 30 degree
    DrawCircle(Canvas, cen, rad div 3);
    Canvas.Pen.Color := clBlack;
    //Elevation at 60 degree
    DrawCirCle(Canvas, cen, rad * 2 div 3);

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

    //Azimuth line from 30 degree to 210 degree.
    sk1 :=  Polar2Cartesian(rad, 30); //Azimuth 30 degree.
    img1 := Cartesian2Pixel(sk1, FTransfrm);
    //the opposite of point sk1 which azimuth = 30 is 210 (30 + 180) degree.
    sk2 :=  Polar2Cartesian(rad, 210); //Azimuth 210 degree.
    img2 := Cartesian2Pixel(sk2, FTransfrm);
    Canvas.Pen.Color := clBlack;
    Canvas.MoveTo(img1.X, img1.y);
    Canvas.LineTo(img2.X, img2.Y);

    //Azimuth line from 60 degree to 240 degree.
    sk1 :=  Polar2Cartesian(rad, 60); //Azimuth 60
    img1 := Cartesian2Pixel(sk1, FTransfrm);
    sk2 :=  Polar2Cartesian(rad, 240); //Azimuth 240
    img2 := Cartesian2Pixel(sk2, FTransfrm);
    Canvas.MoveTo(img1.X, img1.y);
    Canvas.LineTo(img2.X, img2.Y);

    //Azimuth line from 120 degree to 300 degree.
    sk1 :=  Polar2Cartesian(rad, 120); //Azimuth 120
    img1 := Cartesian2Pixel(sk1, FTransfrm);
    sk2 :=  Polar2Cartesian(rad, 300); //Azimuth 300
    img2 := Cartesian2Pixel(sk2, FTransfrm);
    Canvas.MoveTo(img1.X, img1.y);
    Canvas.LineTo(img2.X, img2.Y);

    //Azimuth line from 150 degree to 330 degree.
    sk1 :=  Polar2Cartesian(rad, 150); //Azimuth 150
    img1 := Cartesian2Pixel(sk1, FTransfrm);
    sk2 :=  Polar2Cartesian(rad, 330); //Azimuth 330
    img2 := Cartesian2Pixel(sk2, FTransfrm);
    Canvas.MoveTo(img1.X, img1.y);
    Canvas.LineTo(img2.X, img2.Y);

    dAngs := 0; //Accumulated angle.
    for i := Low(SkyplotBackground) to High(SkyplotBackground) do
    begin
      try
        with SkyplotBackground[i] do
        begin
          fnt := TFont.Create;
          fnt.Name := FaceName;
          fnt.Style := fnt.Style + [Weight];
          fnt.Color := Color;
          fnt.Size := Height;
          Canvas.Font := fnt;
          sk1 :=  Polar2Cartesian(rad, dAngs);
          //img1 := Cartesian2Pixel(sk1, FTransfrm);
          DrawRotatedText(Canvas, Text, sk1, RotationAngle, HozAlignment, VerAlignment);
          dAngs := dAngs + 30;
        end;
      finally
        fnt.Free;
      end;
    end;
  end; //with
end;

//Using instead of Canvas.Arc which can not specify center point.
function TGPSSkyPlot.DrawArc(cv : TCanvas; Center : TPoint; cRadius : integer;
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
    dStartRadians := (dStartDegrees + dSweepDegrees) * DEGREETORADIANS;
    dEndRadians := dStartDegrees * DEGREETORADIANS;
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
  EndPt.X := Center.X + strtoint(formatfloat('0', dDep));
  EndPt.Y := Center.Y - strtoint(formatfloat('0', dLat));

  // Draw the arc
  cv.Arc(Center.x-cRadius,Center.Y-cRadius,Center.x+cRadius,Center.Y+cRadius,
         StartPt.x, StartPt.y, EndPt.X, EndPt.Y);

end;

//Draw circle
function TGPSSkyPlot.DrawCircle(cv : TCanvas; Center : TPoint; cRadius : integer) : boolean;
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

function TGPSSkyPlot.DrawRotatedText(cv : TCanvas; Txt : string; InsertPt : TPoint;
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
  dinx := InsertPt.X; //Insertion point (x) of original text.
  diny := InsertPt.y; //Insertion point (y) of original text.
  txts := cv.TextExtent(txt); //Text size of rotation text.
  dtsx := txts.cx; //Convert text size from integer to double.
  dtsy := txts.cy;

  dtx := dinx; //Convert insertion point from integer to double.
  dty := diny;

  //Manage shift cooridantes to the user alignment.
  //First check the horizontal alignment.
  case HozAlign of
   //default of Canvas.Textout is horizontal left align.
    haLeft : dshfx := 0;
   //Right alignment shift insertpoint point to left direction with the length of text size.
    haRight : dshfx := -1 * dtsx;
   //Center alignment shift insertpoint point to left direction with the half length of text size.
    haCenter : dshfx := -1 * dtsx / 2.0;
  end;
  //Secord check the vertical alignment.
  case VerAlign of
    //default of Canvas.Textout is vertical top align.
    vaTop : dshfy := 0;
   //Bottom alignment shift insertpoint point to up direction with the height of text size.
    vaBottom : dshfy := dtsy;
   //Center alignment shift insertpoint point to up direction with the half height of text size.
    VaCenter : dshfy := dtsy / 2.0;
  end;

  dxp := dshfx;
  dyp := dshfy;

  //Need the angle in azimuth term. Clockwise angle from North(Y-Axis)
  dRad := (90 - Rotateangle);
  if (dRad < 0) then
    dRad := 360 + dRad
  else if (dRad > 540) then
    dRad := dRad - 540
  else if (dRad > 360) then
    dRad := dRad - 360;

  dRad := dRad * DEGREETORADIANS;
  //Calculate the new insert point for new alignment by Multiply the matrix.
  dnewx := dxp * cos(dRad) - dyp * sin(dRad) + dtx;
  dnewy := dxp * sin(dRad) + dyp * cos(dRad) + dty;
  cnew.X := trunc(dnewx);
  cnew.y := trunc(dnewy);
  //From X,Y origin at the center of image convert to pixel coordinates.
  img := Cartesian2Pixel(cnew, FTransfrm);

  //RotateAngle is counter-closewise angle convert to clockwise angle.
  dtang := 90 - RotateAngle;
  //if rotation is minus angle, change to plus angle.
  if (dtang < 0) then dtang := 360 + dtang;
  orient := trunc(dtang * 10);
  cv.Font.Orientation := orient;
  cv.TextOut(img.x, img.y, txt);
end;

function TGPSSkyPlot.SetTransformationPara(Tx, Ty, Scale, RotatedAng : double) : TTransformPara;
begin
  with result do
  begin
    TX1 := Tx;//(Width - 2 * FMargin) / 2.0 + FMargin;
    TY1 := Ty;//(Height - 2 * FMargin) / 2.0 + FMargin;
    ScaleX := Scale;
    RotationAngle := RotatedAng;
  end;
end;

procedure TGPSSkyPlot.DrawGPSSkyPlot(ASatInfos : TGSVInfos);
var
  rSat, rFull : TRect;
  r : TRect;
  sk1, img1 : TPoint;
  x, y : integer;
  i : integer;
  dDist : double;
  Sat : TGSVInfo;
  ts : TSize;
begin
  if Assigned(ASatInfos) then
  begin
    FGSVInfos := ASatInfos;

    rSat := Rect(0, 0, FSatSize, FSatSize);
    rFull := Rect(0, 0, FBmpBackground.Width, FBmpBackground.Height);
    //First Sat, Copy FBufferBackgroundImg to FBmpBuffer.
    if (TGSVInfo(FGSVInfos.items[0]).FirstSat) then
      FBmpBuffer.Canvas.CopyRect(rFull, FBmpBackground.Canvas, rFull);

    for i := 0 to FGSVInfos.Count - 1 do
    begin
      Sat := TGSVInfo(FGSVInfos.Items[i]);
      dDist := (90 - Sat.Elevation) * FSkyHalfW / 90;
      sk1 := Polar2Cartesian(trunc(dDist),Sat.Azimuth);
      img1 := Cartesian2Pixel(sk1, FTransfrm);
      //Keep image coordinate system into TSatInfo.
      Sat.X := img1.X;
      Sat.Y := img1.Y;
      {Draw Sprites works very well in WINDOWS, Not in Linux}
      {$IFDEF WINDOWS}
      BitBlt(FBmpSatDest[i].Canvas.Handle, 0, 0, FBmpSatDest[i].Width, FBmpSatDest[i].Height,
             FBmpBuffer.Canvas.Handle, img1.X - FImgShift, img1.Y - FImgShift, SRCCOPY);
      BitBlt(FBmpSatDest[i].Canvas.Handle, 0, 0, FBmpSatDest[i].Width, FBmpSatDest[i].Height,
             FBmpMaskSat.Canvas.Handle, 0, 0, SRCAND);
      if (Sat.SNR = 0) then
        BitBlt(FBmpSatDest[i].Canvas.Handle, 0, 0, FBmpSatDest[i].Width, FBmpSatDest[i].Height,
               FBmpRedSpriteSat.Canvas.Handle, 0, 0, SRCINVERT)
      else
        BitBlt(FBmpSatDest[i].Canvas.Handle, 0, 0, FBmpSatDest[i].Width, FBmpSatDest[i].Height,
               FBmpGreenSpriteSat.Canvas.Handle, 0, 0, SRCINVERT);
      {$ENDIF}
      {$IFDEF LINUX}
      BitBlt(FBmpSatDest[i].Canvas.Handle, 0, 0, FBmpSatDest[i].Width, FBmpSatDest[i].Height,
             FBmpBuffer.Canvas.Handle, img1.X - FImgShift, img1.Y - FImgShift, SRCCOPY);
      if (Sat.SNR = 0) then
        FBmpSatDest[i].Canvas.Draw(0, 0, FRedSatImg)
      else
        FBmpSatDest[i].Canvas.Draw(0, 0, FGreenSatImg);
      {$ENDIF}
      BitBlt(FBmpBuffer.Canvas.Handle, img1.X - FImgShift, img1.Y - FImgShift,
             FBmpSatDest[i].Width, FBmpSatDest[i].Height, FBmpSatDest[i].Canvas.Handle, 0, 0, SRCCOPY);

    //************* Draw PRN ********************
      with FBmpTxtDest[i] do
      begin
        //Assumed image size to valid for Canvas.Font.
        Width := 10;
        Height := 10;
        Canvas.Font.Color := clBlack;
        Canvas.Font.Style := [fsBold];
        Canvas.Font.Name := 'Sans';
        Canvas.Font.Size := trunc(8/20 * FSatSize);
        ts := Canvas.TextExtent( Sat.Prn);
        //The real size.
        Width := ts.cx;
        Height := ts.cy;
        Canvas.Brush.Style := bsClear;
        Canvas.Brush.Color := self.Color;
        x := img1.X - Width div 2 - 2;
        y := img1.Y + FSatSize div 2 - 2;
        r := Rect(0 , 0, Width, Height);
        BitBlt(Canvas.Handle, 0, 0, Width, Height,
               FBmpBuffer.Canvas.Handle, x, y, SRCCOPY);
        Canvas.TextOut(0, 0, Sat.PRN);
        BitBlt(FBmpBuffer.Canvas.Handle, x, y, Width, Height, Canvas.Handle, 0, 0, SRCCOPY);
        //Last Sat, Copy FBmpBuffer to TGPSSkyplot Canvas.
        if (Sat.LastSat) then
          BitBlt(self.Canvas.Handle, 0, 0, FBmpBuffer.Width, FBmpBuffer.Height, FBmpBuffer.Canvas.Handle, 0, 0, SRCCOPY);
      end;
    end; //for
  end;
end;

procedure TGPSSkyPlot.WMEraseBkgnd(var Msg: TLMessage);
begin
  Msg.Result := 1;
end;

procedure TGPSSkyPlot.CreateMask(TransColor : TColor; const BMPSprite : TCustomBitmap; var BMPMask:  TBitmap);
var
  i, j : integer;

begin
  // loop through the entire sprite
  for i := 0 to BMPSprite.Width - 1 do
     for j:=0 to BMPSprite.Height - 1 do
         // if the pixel is the transparent color
         if (BMPSprite.Canvas.Pixels[i, j] = TransColor) then
         begin
         // white in the mask (white=transparent)
           BMPMask.Canvas.Pixels[i,j] := Rgb(255, 255, 255);
         end
         // if the pixel aint transparent
         // put black in the mask (black=solid)
         else BMPMask.Canvas.Pixels[i,j] := Rgb(0, 0, 0);
end;

procedure TGPSSkyPlot.CreateSprite(TransColor : TColor; const BMPPrototype : TCustomBitmap; var BMPSprite:  TBitmap);
var
  i, j : integer;
  cltemp : TColor;
begin
  // loop through the entire sprite
  BMPSprite.Transparent := true;
  BMPSprite.TransparentColor := TransColor;
  for i := 0 to BMPPrototype.Width do
     for j := 0 to BMPPrototype.Height do
     begin
         cltemp := BMPPrototype.Canvas.Pixels[i, j];
         // if the pixel is the transparent color
         if (BMPPrototype.Canvas.Pixels[i, j] = TransColor) then
         begin
         // make black that pixel in the sprite
         //  BMPSprite.Canvas.Pixels[i, j] := RGB(0, 0, 0);
         // and white in the mask (white=transparent)
           BMPSprite.Canvas.Pixels[i, j] := 0;//RGB(255, 255, 255);
         end
         // if the pixel aint transparent
         // put black in the mask (black=solid)
         else
           BMPSprite.Canvas.Pixels[i,j] := cltemp;//RGB (0, 0, 0);
     end;
end;

procedure TGPSSkyplot.Resize;
begin
  if csDesigning in ComponentState then
  begin
    Width := Height;//We need square size.
    Invalidate;
  end;
  inherited Resize;

end;

procedure TGPSSkyplot.ChangeBounds(ALeft, ATop, AWidth, AHeight: integer; KeepBase: boolean);
begin
  if csDesigning in ComponentState then
  begin
    AWidth := AHeight;//We need square size.
    Invalidate;
  end;
  inherited ChangeBounds(ALeft, ATop, AWidth, AWidth, KeepBase);
end;


Initialization
{$i gpsskyplot.lrs}

end.





