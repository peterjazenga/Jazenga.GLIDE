{**********************************************************************
 Package pl_ExGeographic
 This unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit flagcomponentbaseunit;
{
  Copyright (C) 2004- Seppo S Finland
  Licence: modifiedLGPL (Same as FreePascal)
}

{$mode objfpc}{$H+}

interface

uses
  Controls,Graphics,Classes,ExtGraphics,LCLProc, LCLtype,types, graphmath;

type

   TDiagonalDirection= (DDupDown,DDDownUp);


  { TFlagBase }

  TFlagBase = class(TGraphicControl)
  protected
    Xleft,Yup,XRight,Ylow:integer;

    procedure BiColorFlagHorizontal(UpColor,LowColor:TColor);
    procedure BiColorFlagVertical(LeftColor,RightColor:TColor);
    procedure TriColorBasicVertical(LeftColor, MidColor, RightColor: TColor;
      X1, Y1, x2, y2: integer);
    procedure TriColorFlagHorizontal(UpColor,MidColor,LowColor:TColor);
    procedure TriColorFlagVertical(LeftColor,MidColor,RightColor:TColor);
    procedure QuadColorFlag(UpColor,UpMidColor,LowMidColor,LowColor:TColor);
    procedure QuadColorFlagVertical(LeftColor,LeftMidColor,
      RightMidColor,RightColor:TColor);
    procedure QuadColor2SFlag(SquareUpColor,UpColor,SquareLowColor,
      LowColor:TColor);
    procedure FiveColorFlag(UpCl,UpMidCl,CenterCl,LowMidCl,LowCl:TColor);
    procedure FiveColorVertical(c1, c2, c3, c4, c5: TColor);
    procedure SixColorFlag(c1,c2,c3,c4,c5,c6:TColor);
    procedure SevenColorFlag(c1, c2, c3, c4, c5, c6, c7: TColor);
    procedure EightColorFlag(c1, c2, c3, c4, c5, c6, c7, c8: TColor);
    procedure NineColorFlag(c1, c2, c3, c4, c5, c6, c7, c8, c9: TColor);
    procedure StripeBase13(c1, c2: TColor);

    procedure CrossBasic(BaseColor,CrossColor:Tcolor;
                         RatioX,RatioY,CrossXLeft,CrossWidth,CrossYTop:integer);
    procedure CrossPaint(CrossColor:Tcolor;
                         RatioX,RatioY,CrossXLeft,CrossWidth,CrossYTop:integer);
    procedure CrossBasicPaint(CrossColor:Tcolor;
                         X1,Y1,x2,y2,CrossX1,Crossx2,CrossY1,CrossY2:integer);
    procedure TwoPlusTwoColor(UpLeft, UpRight, LowLeft, LowRight: TColor);
    procedure TwoRightTriangleDownUp(x1, y1, x2, y2: integer; Upcl,
      Downcl: TColor);
    procedure TwoRightTriangleDownUpFlag(c1, c2: TColor);
    Procedure TwoRightTriangleUpDown(x1,y1,x2,y2:integer;Upcl,Downcl:TColor);
    procedure TwoRightTriangleUpDownFlag(c1, c2: TColor);
    procedure CantonBasic( upcolor, downcolor, cantoncolor: TColor);

    procedure PaintDiagonals(X1,Y1,X2,Y2:integer;dx,dy:Real);
    procedure PaintXDiagonals(x1,y1,x2,y2,dx,dy:integer;DColor:TColor);
    procedure PaintDiagonal(ATDD: TDiagonalDirection; dcolor: TColor; x1, y1,
      x2, y2, dx, dy: integer);
    procedure DiagonalDraw(ATDD: TDiagonalDirection; dcolor: TColor; RtX, RtY,
      xa, ya: integer);
    procedure DiagonalBicolorTriband(bcl, dcl: TColor; RtX, RtY, xa, ya: integer
      );
    procedure DiagonalTricolorTriband(bclUp, bclmid, bcldown, dcl: TColor; RtX,
      RtY, xa, ya: integer);
    procedure DiagonalTricolorTriband2(cl1, dcl, cl2: TColor; RtX, RtY, xa,
      ya: integer);
    procedure DiagonalTricolorTriband3(cl1, dcl, cl2: TColor; RtX, RtY, xa,
      ya: integer);

    procedure RightRatioSize(xRatio,yRatio:Integer);
    function DistCorner(w,h,divider:integer;d:double):integer;
    procedure FivePointStarBasicAngle(cx, cy, r: Integer; fii: double;
      StarColor: TColor);
    procedure Line(x1, y1, x2, y2: integer; LineColor:TColor);
    procedure Rectangular(X1,Y1,X2,Y2:Integer; RecColor:TColor);
    procedure Triagle(X1,Y1,X2,Y2:Integer;Direction:TShapeDirection;
      TriagleColor:TColor);
    procedure Triangular(X1,Y1,X2,Y2:Integer;Direction,RLf:extended;
      TriagleColor:TColor);
    procedure Ball(XCentre,YCentre,radius:Integer; BallColor:TColor);
    procedure HalfBall(X1,Y1,X2,Y2:Integer;Direction:TShapeDirection;
      BColor:TColor);
    procedure TrianglePoints(Po1, Po2, Po3: TPoint; DColor: TColor);
    procedure FourCornered(Po1,Po2,Po3,Po4:TPoint; DColor:TColor);
    procedure FiveCornered(Po1, Po2, Po3, Po4, Po5: TPoint; DColor: TColor);
    procedure SixCornered(Po1, Po2, Po3, Po4, Po5, Po6: TPoint; DColor: TColor);
    procedure SevenCornered(Po1,Po2,Po3,Po4,Po5,Po6,Po7:TPoint; DColor: TColor);
    procedure SevenCornered2(Po1, Po2, Po3, Po4, Po5, Po6, Po7: TPoint;
      PenColor, BrushColor: TColor);
    procedure EightCornered(Po1, Po2, Po3, Po4, Po5, Po6, Po7, Po8: TPoint;
      Cl: TColor);

    procedure FivePointStar(x1,y1,x2,y2:Integer; StarColor:TColor;
      RadAngle :Extended=0.0);
    procedure FivePointStarBasic(cx,cy,r:Integer;StarColor:TColor);
    procedure FivePointStarSmall(cx,cy,r:Integer;StarColor:TColor);
    procedure FivePointStarEdge(cx,cy,r:Integer;EdgeColor,StarColor:TColor);
    procedure SixPointStar(cx, cy, r: Integer; StarColor: TColor);
    procedure SixPointStarEdge(cx,cy,r: Integer; EdgeColor,StarColor:TColor);
    procedure SevenPointStar(cx,cy,r:Integer;StarColor:TColor);
    procedure PaintNPointStar(cx,cy,r,p:Integer;StarColor:TColor);
    procedure CpXYCalc(x1,y1,x2,y2:integer; var cp:TPoint; var x,y:integer);
    procedure Diamond(x1, x2, x3, uy, cy, ly: integer; DColor: TColor);
    procedure LineStarOfDavid(x1, y1, x2, y2: integer; StarColor: TColor);
    procedure StarOfDavid(x1, y1, x2, y2: integer; StarColor,BaseColor: TColor);
    procedure SunRay(cp:TPoint; r:integer; SunRayColor:TColor);
    
    procedure XSaltires(x1,y1,x2,y2,CrossLineWidth:integer;
      CrossLineColor:TColor);
  end;


implementation


procedure PolycRotate
  (var Pts:array of TPoint;cntPoint:TPoint; fii:Extended);
var i,dx,dy:Integer;
    x,y:Extended;
begin
  for i:= low(Pts) to high(Pts) do
    begin
      dx:=Pts[i].x-cntPoint.x;
      dy:=Pts[i].y-cntPoint.y;
      x:=dx*cos(fii)+dy*sin(fii);
      y:=dy*cos(fii)-dx*sin(fii);
      Pts[i].x:=cntPoint.x+Round(x);
      Pts[i].y:=cntPoint.y+Round(y);
    end;
end;

procedure SetAspectRatio(var x1,y1,x2,y2,cx,cy:Integer; mx,my:Integer);
var ih,iw:Integer;
    h,w,m1,m2,rw,rh:Real;
begin
  w:=x2-x1; h:=y2-y1;
  cx:=x1+Round(w/2); cy:=y1+Round(h/2);
  rw:=mx; rh:=my;

  m1:=rh/rw; m2:=h/w;
  if m1<m2 then
    begin
      h:=m1*w; ih:=Round(h/2);
      y1:=cy-ih; y2:=cy+ih;
    end
    else begin
           w:=h/m1; iw:=Round(w/2);
           x1:=cx-iw; x2:=cx+iw;
         end;
end;

procedure PointsOfStar(var P:array of TPoint; cx,cy:Integer; r,a:Double);
var r1,r0,alfa,fii,f2,f4,theta:Double;
   i,cs,n,n2:Integer;
begin
 n:=High(P); n2:=(n+1) div 2;
 fii:=2*Pi/n2; f2:=fii/2; f4:=fii/4;

 theta:=Pi-3*f4;
 if n2 mod 2 =0 then r1:=r/(2*cos(f2))
   else r1:=r*sin(f4)/sin(theta);

 for i:=0 to n do
    begin
      if (i mod 2)=0 then r0:=r else r0:=r1;
      alfa:=a+(0.5+i/n2)*Pi;
      cs:=Round(r0*cos(alfa));
      P[i].x:=cx+cs;
      P[i].y:=cy-Round(r0*sin(alfa));
   end;
end;

procedure MinMaxCoordinatesOfPoints(var x1,y1,x2,y2:Integer; P:array of TPoint);
var i:Integer;
begin
 x1:=P[0].x; x2:=P[0].x; y1:=P[0].y; y2:=P[0].y;
 for i:=1 to High(P) do
   begin
     if P[i].x<x1 then x1:=P[i].x;
     if P[i].y<y1 then y1:=P[i].y;
     if P[i].x>x2 then x2:=P[i].x;
     if P[i].y>y2 then y2:=P[i].y;
   end;
end;

procedure PaintHalfEllipsis(canvas:TCanvas; T:TRect; fii:Extended;Color:TColor);
var cx,cy,sx,sy,ex,ey,x1,y1,x2,y2,dx,dy:Integer;
    sf,ef,r:Extended;
begin
  with T do
    begin
      x1:=Left; x2:=Right; y1:=Top; y2:=Bottom;
    end;
  cx:=(x1+x2) div 2; cy:=(y1+y2) div 2; dx:=x2-x1; dy:=y2-y1;
  r:=sqrt(dx*dx+dy*dy)/2;
  sf:=fii-Pi/2; ef:=fii+Pi/2;
  sx:=cx+Round(r*cos(sf)); sy:=cy-Round(r*sin(sf));
  ex:=cx+Round(r*cos(ef)); ey:=cy-Round(r*sin(ef));
  Canvas.Pen.color:=Color;
  Canvas.Brush.color:=Color;
  canvas.Chord(x1,y1,x2,y2,sx,sy,ex,ey);
end;




function TFlagBase.DistCorner(w,h,divider:integer;d:double):integer;
begin
  result:=round(sqrt(w*w+h*h)*d/divider);
end;

procedure TFlagBase.FivePointStarEdge(cx,cy,r:Integer;EdgeColor,StarColor:TColor);
begin
  if (r<4)
   then FivePointStarSmall(cx,cy,r,StarColor)
   else begin
       Canvas.Pen.color:=EdgeColor;
       Canvas.Brush.color:=StarColor;
       PaintStarN(Canvas,cx,cy,r,5,0);
   end;
end;

procedure TFlagBase.SixPointStarEdge(cx,cy,r:Integer;EdgeColor,StarColor:TColor);
begin
  if (r<4)
   then SixPointStar(cx,cy,r,StarColor)
   else begin
       Canvas.Pen.color:=EdgeColor;
       Canvas.Brush.color:=StarColor;
       PaintStarN(Canvas,cx,cy,r,6,0);
   end;
end;

procedure TFlagBase.FivePointStarBasic(cx,cy,r:Integer;StarColor:TColor);
var P:array[0..9] of TPoint;
    x1,y1,x2,y2:integer;
begin
 FivePointStarBasicAngle(cx,cy,r,0.0,StarColor);
  //if (r<4)
   //then FivePointStarSmall(cx,cy,r,StarColor)
   //else begin
      //PointsOfStar(P,cx,cy,r,0.0);
      //MinMaxCoordinatesOfPoints(x1,y1,x2,y2,P);
      //FivePointStar(x1,y1,x2,y2,StarColor, 0.0);
   //end;
end;
procedure TFlagBase.FivePointStarBasicAngle(cx,cy,r:Integer;
   fii:double;StarColor:TColor);
var P:array[0..9] of TPoint;
    x1,y1,x2,y2:integer;
begin
  if (r<4)
   then FivePointStarSmall(cx,cy,r,StarColor)
   else begin
      PointsOfStar(P,cx,cy,r,fii);
      MinMaxCoordinatesOfPoints(x1,y1,x2,y2,P);
      FivePointStar(x1,y1,x2,y2,StarColor, fii);
   end;
end;

procedure TFlagBase.Line(x1,y1,x2,y2:integer;LineColor:TColor);
begin
  canvas.Pen.color:=LineColor;
  canvas.Brush.color:=Linecolor;
  canvas.Line(x1,y1,x2,y2);
end;

procedure TFlagBase.FivePointStarSmall(cx,cy,r:Integer;StarColor:TColor);
var PaintRect: TRect;
begin
  with canvas do
    begin
      Pen.color:=StarColor;
      Brush.color:=starcolor;
      PaintRect:=Rect(cx-r,cy-r,cx+r,cy+r);
      if r>1
        then PaintFivePointLineStar(Canvas,PaintRect)
        else Pixels[cx,cy]:=starcolor;
    end;
end;

procedure TFlagBase.SixPointStar(cx,cy,r:Integer;StarColor:TColor);
begin
  PaintNPointStar(cx,cy,r,6,StarColor);
end;

procedure TFlagBase.SevenPointStar(cx,cy,r:Integer;StarColor:TColor);
begin
  PaintNPointStar(cx,cy,r,7,StarColor);
end;

procedure TFlagBase.PaintNPointStar(cx,cy,r,p:Integer;StarColor:TColor);
begin
  Canvas.Pen.color:=StarColor;
  Canvas.Brush.color:=StarColor;
  PaintStarN(Canvas,cx,cy,r,p,0);
end;

procedure TFlagBase.Rectangular(X1,Y1,X2,Y2:Integer; RecColor:TColor);
begin
  with canvas do
    begin
      Pen.color:=RecColor;
      Brush.color:=RecColor;
      Rectangle(X1,Y1,X2,Y2);
    end;
end;

procedure TFlagBase.FivePointStar(x1,y1,x2,y2:Integer; StarColor:TColor;
  RadAngle :Extended=0.0);
var PaintRect: TRect; small:boolean;
begin
  Canvas.Pen.color:=StarColor;
  Canvas.Brush.color:=StarColor;
  small:=false;
  PaintRect:=Rect(x1,y1,x2,y2);
  if x2-x1<4 then small:=true;
  if y2-y1<4 then small:=true;
  if small
    then begin
      if x1+1 < x2
        then PaintFivePointLineStar(Canvas,PaintRect)
        else FivePointStarSmall(x1,y1,x2-x1,StarColor);
    end else PaintFivePointStar(Canvas,PaintRect,RadAngle);
end;

procedure TFlagBase.TrianglePoints(Po1,Po2,Po3:TPoint; DColor:TColor);
var  P:array[0..2] of TPoint;
begin
  Canvas.Pen.color:=DColor;
  Canvas.Brush.color:=DColor;
  P[0]:=Po1;
  P[1]:=Po2;
  P[2]:=Po3;
  Canvas.Polygon(P);
end;

procedure TFlagBase.FourCornered(Po1,Po2,Po3,Po4:TPoint; DColor:TColor);
var  P:array[0..3] of TPoint;
begin
  Canvas.Pen.color:=DColor;
  Canvas.Brush.color:=DColor;
  P[0]:=Po1;
  P[1]:=Po2;
  P[2]:=Po3;
  P[3]:=Po4;
  Canvas.Polygon(P);
end;

procedure TFlagBase.FiveCornered(Po1,Po2,Po3,Po4,Po5:TPoint; DColor:TColor);
var  P:array[0..4] of TPoint;
begin
  Canvas.Pen.color:=DColor;
  Canvas.Brush.color:=DColor;
  P[0]:=Po1;
  P[1]:=Po2;
  P[2]:=Po3;
  P[3]:=Po4;
  P[4]:=Po5;
  Canvas.Polygon(P);
end;

procedure TFlagBase.SixCornered(Po1,Po2,Po3,Po4,Po5,Po6:TPoint;DColor:TColor);
var  P:array[0..5] of TPoint;
begin
  Canvas.Pen.color:=DColor;
  Canvas.Brush.color:=DColor;
  P[0]:=Po1;
  P[1]:=Po2;
  P[2]:=Po3;
  P[3]:=Po4;
  P[4]:=Po5;
  P[5]:=Po6;
  Canvas.Polygon(P);
end;

procedure TFlagBase.SevenCornered(Po1,Po2,Po3,Po4,Po5,Po6,Po7:TPoint;DColor:TColor);
begin
  SevenCornered2(Po1,Po2,Po3,Po4,Po5,Po6,Po7,DColor,DColor);
end;

procedure TFlagBase.SevenCornered2(Po1,Po2,Po3,Po4,Po5,Po6,Po7:TPoint;
  PenColor,BrushColor:TColor);
var  P:array[0..6] of TPoint;
begin
  Canvas.Pen.color:=PenColor;
  Canvas.Brush.color:=BrushColor;
  P[0]:=Po1;
  P[1]:=Po2;
  P[2]:=Po3;
  P[3]:=Po4;
  P[4]:=Po5;
  P[5]:=Po6;
  P[6]:=Po7;
  Canvas.Polygon(P);
end;

procedure TFlagBase.EightCornered(Po1,Po2,Po3,Po4,Po5,Po6,Po7,Po8:TPoint;Cl:TColor);
var  P:array[0..7] of TPoint;
begin
  Canvas.Pen.color:=Cl;
  Canvas.Brush.color:=cl;
  P[0]:=Po1;
  P[1]:=Po2;
  P[2]:=Po3;
  P[3]:=Po4;
  P[4]:=Po5;
  P[5]:=Po6;
  P[6]:=Po7;
  P[7]:=Po8;
  Canvas.Polygon(P);
end;

procedure TFlagBase.Ball(XCentre,YCentre,radius:Integer; BallColor:TColor);
begin
  with canvas do
    begin
      Pen.color:=BallColor;
      Brush.color:=BallColor;
      Ellipse(XCentre-radius,YCentre-radius,XCentre+radius,YCentre+radius);
    end;
end;

procedure TFlagBase.HalfBall(X1,Y1,X2,Y2:Integer;Direction:TShapeDirection;
  BColor:TColor);
var PaintRect: TRect; fii:extended;
begin
  case  Direction of
     atUp   :begin
               fii:=pi/2;
               y2:=2*y2-y1;
             end;
     atDown :begin
               fii:=1.5*pi;
               y1:=2*y1-y2;
             end;
     atLeft :begin
               fii:=pi;
               x2:=2*x2-x1;
             end;
     atRight:begin
               fii:=0.0;
               x1:=2*x1-x2
             end;
  end;
  PaintRect:=Rect(x1,y1,x2,y2);
  PaintHalfEllipsis(Canvas,PaintRect,fii,BColor);
end;

procedure TFlagBase.Triagle(X1,Y1,X2,Y2:Integer;Direction:TShapeDirection;
  TriagleColor:TColor);
var PaintRect: TRect;
begin
  Canvas.Pen.color:=TriagleColor;
  Canvas.Brush.color:=TriagleColor;
  PaintRect:=Rect(x1,y1,x2,y2);
  case  Direction of
     atUp   :PaintTriangle(Canvas,PaintRect,pi/2);
     atDown :PaintTriangle(Canvas,PaintRect,1.5*pi);
     atLeft :PaintTriangle(Canvas,PaintRect,pi);
     atRight:PaintTriangle(Canvas,PaintRect,0.0);
  end;
end;

procedure TFlagBase.Triangular(X1,Y1,X2,Y2:Integer;Direction,RLf:extended;
  TriagleColor:TColor);
var PaintRect: TRect;
begin
  Canvas.Pen.color:=TriagleColor;
  Canvas.Brush.color:=TriagleColor;
  PaintRect:=Rect(x1,y1,x2,y2);
  PaintTriangular(Canvas,PaintRect,Direction,RLf);
end;

Procedure TFlagBase.TwoRightTriangleUpDown(x1,y1,x2,y2:integer;Upcl,Downcl:TColor);
var  P:array[0..2] of TPoint;
begin
  Canvas.Pen.color:=UPcl;
  Canvas.Brush.color:=UPcl;
  P[0]:=point(X1,Y1);
  P[1]:=point(X2,Y1);
  P[2]:=point(X1,Y2);
  Canvas.Polygon(P);
  Canvas.Pen.color:=Downcl;
  Canvas.Brush.color:=Downcl;
  P[0]:=point(X2,Y2);
  P[1]:=point(X2,Y1);
  P[2]:=point(X1,Y2);
  Canvas.Polygon(P);
end;

Procedure TFlagBase.TwoRightTriangleDownUp(x1,y1,x2,y2:integer;Upcl,Downcl:TColor);
var  P:array[0..2] of TPoint;
begin
  Canvas.Pen.color:=UPcl;
  Canvas.Brush.color:=UPcl;
  P[0]:=point(X2,Y1);
  P[1]:=point(X1,Y1);
  P[2]:=point(X2,Y2);
  Canvas.Polygon(P);
  Canvas.Pen.color:=Downcl;
  Canvas.Brush.color:=Downcl;
  P[0]:=point(X1,Y1);
  P[1]:=point(X1,Y2);
  P[2]:=point(X2,Y2);
  Canvas.Polygon(P);
end;

procedure TFlagBase.CpXYCalc(x1,y1,x2,y2:integer;var cp:TPoint;Var x,y:integer);
begin
  x:=x2-x1;
  y:=y2-y1;
  cp:=CenterPoint(Rect(x1,y1,x2,y2));
end;

procedure TFlagBase.RightRatioSize(xRatio,yRatio:Integer);
var cx,cy:integer;
begin
  cx:=(XRight-XLeft) div 2+XLeft;
  cy:=(YLow-YUp) div 2 + YUp;
  SetAspectRatio(XLeft,YUp,XRight,YLow,cx,cy,xRatio,yRatio);
end;

procedure TFlagBase.BiColorFlagHorizontal(UpColor,LowColor:TColor);
var     OneHalf:integer;
begin
  OneHalf:= ((Ylow-Yup)div 2)+Yup;
  Rectangular(Xleft,Yup,XRight,OneHalf,UpColor);
  Rectangular(Xleft,OneHalf,XRight,Ylow,LowColor);
end;

procedure TFlagBase.BiColorFlagVertical(LeftColor,RightColor:TColor);
var     OneHalf:integer;
begin
  OneHalf:= ((XRight-Xleft)div 2)+Xleft;
  Rectangular(Xleft,Yup,OneHalf,Ylow,LeftColor);
  Rectangular(OneHalf,Yup,XRight,Ylow,RightColor);
end;

procedure TFlagBase.TriColorFlagHorizontal(UpColor,MidColor,LowColor:TColor);
var     OnePerThree,TwoPerThree:integer;
begin
  OnePerThree:= ((Ylow-Yup)div 3)+Yup;
  TwoPerThree:= Ylow-((Ylow-Yup)div 3);
  Rectangular(Xleft,Yup,XRight,OnePerThree,UpColor);
  Rectangular(Xleft,OnePerThree,XRight,TwoPerThree,MidColor);
  Rectangular(Xleft,TwoPerThree,XRight,Ylow,LowColor);
end;

procedure TFlagBase.TriColorBasicVertical(LeftColor,MidColor,RightColor:TColor;
  X1,Y1,x2,y2:integer);
var     OnePerThree,TwoPerThree:integer;
begin
  OnePerThree:= ((x2-x1)div 3)+X1;
  TwoPerThree:= X2-((X2-X1)div 3);
  Rectangular(X1,Y1,OnePerThree,Y2,LeftColor);
  Rectangular(OnePerThree,Y1,TwoPerThree,Y2,MidColor);
  Rectangular(TwoPerThree,Y1,X2,Y2,RightColor);
end;

procedure TFlagBase.TriColorFlagVertical(LeftColor,MidColor,RightColor:TColor);
begin
  TriColorBasicVertical(LeftColor,MidColor,RightColor,XLeft,Yup,XRight,YLow);
end;

procedure TFlagBase.QuadColorFlag(UpColor,UpMidColor,LowMidColor,LowColor:TColor);
begin
  BiColorFlagHorizontal(UpMidColor,LowMidColor);
  Rectangular(Xleft,YUp,XRight,YUp+(Ylow-YUp)div 4,UpColor);
  Rectangular(Xleft,YLow-(Ylow-YUp)div 4,XRight,YLow,LowColor);
end;

procedure TFlagBase.QuadColorFlagVertical(LeftColor,LeftMidColor,
  RightMidColor,RightColor:TColor);
begin
  BiColorFlagVertical(LeftMidColor,RightMidColor);
  Rectangular(Xleft,Yup,((XRight-XLeft)div 4)+Xleft,YLow,LeftColor);
  Rectangular(XRight-((XRight-XLeft)div 4),Yup,XRight,Ylow,RightColor);
end;

procedure TFlagBase.QuadColor2SFlag(SquareUpColor,UpColor,SquareLowColor,LowColor:TColor);
var y,x:integer;
begin
  x:=XLeft+(XRight-XLeft)div 3;
  y:=YUp+(Ylow-YUp)div 2;
  BiColorFlagHorizontal(UpColor,LowColor);
  Rectangular(Xleft,YUp,X,Y,SquareUpColor);
  Rectangular(Xleft,y,x,YLow,SquareLowColor);
end;

procedure TFlagBase.TwoPlusTwoColor(UpLeft,UpRight,LowLeft,LowRight:TColor);
var cp:TPoint;
begin
  cp:=CenterPoint(Rect(Xleft,Yup,XRight,Ylow));
  Rectangular(Xleft,Yup,cp.x,cp.y,UpLeft);
  Rectangular(Xleft,cp.y,cp.x,YLow,LowLeft);
  Rectangular(cp.x,Yup,XRight,cp.y,UpRight);
  Rectangular(cp.x,cp.y,XRight,Ylow,LowRight);
end;

procedure TFlagBase.FiveColorVertical(c1,c2,c3,c4,c5:TColor);
var x1,x2:integer;
begin
  TriColorFlagVertical(c1,c3,c5);
  x1:=MulDiv(XRight-Xleft,1,5);
  x2:=MulDiv(XRight-Xleft,2,5);
  Rectangular(Xleft+x1,Yup,Xleft+x2,YLow,c2);
  Rectangular(XRight-x2,Yup,XRight-x1,YLow,c4);
end;

procedure TFlagBase.FiveColorFlag(UpCl,UpMidCl,CenterCl,LowMidCl,LowCl:TColor);
var y1,y2:integer;
begin
  TriColorFlagHorizontal(UpCl,CenterCl,LowCl);
  y1:=MulDiv(Ylow-YUp,1,5);
  y2:=MulDiv(Ylow-YUp,2,5);
  Rectangular(Xleft,Yup+Y1,XRight,YUp+y2,UpMidCl);
  Rectangular(Xleft,YLow-y2,XRight,YLow-y1,LowMidCl);
end;

procedure TFlagBase.SixColorFlag(c1,c2,c3,c4,c5,c6:TColor);
var y1,y2:integer;
begin
  TriColorFlagHorizontal(c1,c4,c6);
  y1:=MulDiv(Ylow-YUp,1,6);
  y2:=MulDiv(Ylow-YUp,2,6);
  Rectangular(Xleft,Yup+Y1,XRight,YUp+y2,C2);
  Rectangular(Xleft,YLow-y2,XRight,YLow-y1,c5);
  Rectangular(Xleft,Yup+Y2,XRight,YUp+(Ylow-YUp)div 2,C3);
end;

procedure TFlagBase.SevenColorFlag(c1,c2,c3,c4,c5,c6,c7:TColor);
var y1,y2,y3:integer;
begin
  TriColorFlagHorizontal(c1,c4,c7);
  y1:=MulDiv(Ylow-YUp,1,7);
  y2:=MulDiv(Ylow-YUp,2,7);
  y3:=MulDiv(Ylow-YUp,3,7);
  Rectangular(Xleft,Yup+Y1,XRight,YUp+y2,C2);
  Rectangular(Xleft,Yup+Y2,XRight,YUp+y3,C3);
  Rectangular(Xleft,YLow-y3,XRight,YLow-y2,c5);
  Rectangular(Xleft,YLow-y2,XRight,YLow-y1,c6);
end;

procedure TFlagBase.EightColorFlag(c1,c2,c3,c4,c5,c6,c7,c8:TColor);
var y1:integer; cp:TPoint;
begin
  cp:=CenterPoint(Rect(Xleft,Yup,XRight,Ylow));
  QuadColorFlag(c2,c3,c6,c7);
  y1:=MulDiv(Ylow-YUp,1,8);
  Rectangular(Xleft,Yup,XRight,YUp+y1,c1);
  Rectangular(Xleft,cp.y-y1,XRight,cp.y,c4);
  Rectangular(Xleft,cp.y,XRight,cp.y+y1,c5);
  Rectangular(Xleft,YLow-y1,XRight,YLow,c8);
end;

procedure TFlagBase.NineColorFlag(c1,c2,c3,c4,c5,c6,c7,c8,c9:TColor);
var y,y1:integer;
begin
  SixColorFlag(c1,c3,c4,c6,c7,c9);
  y:=Ylow-YUp;
  y1:=MulDiv(y,1,9);
  Rectangular(Xleft,Yup+y1,XRight,YUp+y1*2,c2);
  Rectangular(Xleft,YUp+MulDiv(y,4,9),XRight,YUp+MulDiv(y,5,9),c5);
  Rectangular(Xleft,YLow-y1*2,XRight,YLow-y1,c8);
end;

procedure TFlagBase.TwoRightTriangleUpDownFlag(c1,c2:TColor);
begin
  TwoRightTriangleUpDown(Xleft,Yup,XRight,Ylow,c1,c2);
end;

procedure TFlagBase.TwoRightTriangleDownUpFlag(c1,c2:TColor);
begin
  TwoRightTriangleDownUp(Xleft,Yup,XRight,Ylow,c1,c2);
end;
procedure TFlagBase.CrossBasicPaint(CrossColor:Tcolor;
                         X1,Y1,x2,y2,CrossX1,Crossx2,CrossY1,CrossY2:integer);
begin
  with canvas do
    begin
      Pen.color:=CrossColor;
      Brush.color:=CrossColor;
      PaintCross(Canvas,X1,Y1,X2,Y2,CrossX1,CrossX2,CrossY1,CrossY2);
    end;
end;

procedure TFlagBase.CrossPaint(CrossColor:Tcolor;
                         RatioX,RatioY,CrossXLeft,CrossWidth,CrossYTop:integer);
var     CrossX1,CrossX2,CrossY1,CrossY2:integer;
begin
  RightRatioSize(RatioX,RatioY);
  CrossX1:=XLeft+(XRight-XLeft) * CrossXLeft div RatioX ;
  CrossX2:=XLeft+(XRight-XLeft) * (CrossXLeft+CrossWidth) div RatioX;
  CrossY1:=YUp+(YLow-YUp) * CrossYTop div RatioY ;
  CrossY2:=YUp+(YLow-YUp) * (CrossYTop+CrossWidth) div RatioY ;
  CrossBasicPaint(CrossColor,XLeft,YUp,XRight,YLow,CrossX1,CrossX2,CrossY1,CrossY2);
end;

procedure TFlagBase.CrossBasic(BaseColor,CrossColor:Tcolor;
                         RatioX,RatioY,CrossXLeft,CrossWidth,CrossYTop:integer);
begin
  RightRatioSize(RatioX,RatioY);
  Rectangular(XLeft,Yup,XRight,Ylow,BaseColor);
  CrossPaint(CrossColor,RatioX,RatioY,CrossXLeft,CrossWidth,CrossYTop);
end;

procedure TFlagBase.PaintDiagonal(ATDD:TDiagonalDirection;dcolor:TColor;
    x1,y1,x2,y2,dx,dy:integer);
var Po1,Po2,Po3,Po4,Po5,Po6:TPoint;
    xx1,xx2:integer;
begin
  if ATDD=DDupDown then begin
      xx1:=X1;
      xx2:=X2;
    end else begin
      xx2:=X1;
      xx1:=x2;
  end;
  Po1:=Point(xx1,y1+dy);
  Po2:=Point(xx1,y1);
  if ATDD=DDupDown
    then Po3:=Point(xx1+dx,y1)
    else Po3:=Point(xx1-dx,y1);
  Po4:=Point(xx2,y2-dy);
  Po5:=Point(xx2,y2);
  if ATDD=DDupDown
    then Po6:=Point(xx2-dx,Y2)
    else Po6:=Point(xx2+dx,Y2);
  SixCornered(Po1,Po2,Po3,Po4,Po5,Po6,dcolor);
end;

procedure TFlagBase.PaintXDiagonals(x1,y1,x2,y2,dx,dy:integer;DColor:TColor);
begin
  PaintDiagonal(DDupDown,dcolor, x1,y1,x2,y2,dx,dy);
  PaintDiagonal(DDDownUp,dcolor, x1,y1,x2,y2,dx,dy);
end;

procedure TFlagBase.PaintDiagonals(x1,y1,x2,y2:integer;dx,dy:Real);
var    Q:array[0..3] of TPoint;
       ctx,cty:Integer;
begin
  with canvas do
    begin
      PaintXDiagonals(x1,y1,x2,y2,round(dx),round(dy),clWhite);
      Pen.Color:=clRed;
      Brush.Color:=clRed;
      ctx:=(x1+x2) div 2; cty:=(y1+y2) div 2;

      Q[0].x:=x1; Q[0].y:=y1;
      Q[1].x:=x1; Q[1].y:=y1+Round(2*dy/3);
      Q[2].x:=ctx; Q[2].y:=cty+Round(2*dy/3);
      Q[3].x:=ctx; Q[3].y:=cty;
      Polygon(Q);

      Q[0].x:=ctx; Q[0].y:=cty;
      Q[1].x:=x2; Q[1].y:=y2;
      Q[2].x:=x2; Q[2].y:=y2-Round(2*dy/3);
      Q[3].x:=ctx; Q[3].y:=cty-Round(2*dy/3);
      Polygon(Q);

      Q[0].x:=x1; Q[0].y:=y2;
      Q[1].x:=x1+Round(2*dx/3); Q[1].y:=y2;
      Q[2].x:=ctx; Q[2].y:=cty+Round(2*dy/3);
      Q[3].x:=ctx; Q[3].y:=cty;
      Polygon(Q);

      Q[0].x:=ctx; Q[0].y:=cty;
      Q[1].x:=x2; Q[1].y:=y1;
      Q[2].x:=x2-Round(2*dx/3); Q[2].y:=y1;
      Q[3].x:=ctx; Q[3].y:=cty-Round(2*dy/3);
      Polygon(Q);
    end;
end;

procedure TFlagBase.DiagonalDraw(ATDD:TDiagonalDirection;dcolor:TColor;
      RtX,RtY,xa,ya:integer);
var  x,y,x1,y1:integer;
begin
    x:=XRight-Xleft;
    y:=ylow-YUp;
    x1:=MulDiv(x,xa,RtX);
    y1:=MulDiv(y,ya,RtY);
    PaintDiagonal(ATDD,dcolor,XLeft,YUp,XRight,YLow, x1,y1);
end;

procedure TFlagBase.DiagonalBicolorTriband(bcl,dcl:TColor;
  RtX,RtY,xa,ya:integer);
begin
  RightRatioSize(RtX,RtY);
  Rectangular(XLeft,YUp,XRight,YLow,bcl);
  DiagonalDraw(DDupDown,dcl,RtX,RtY,xa,ya);
end;
procedure TFlagBase.DiagonalTricolorTriband(bclUp,bclmid,bcldown,dcl:TColor;
                RtX,RtY,xa,ya:integer);
begin
  RightRatioSize(RtX,RtY);
  TriColorFlagHorizontal(bclUp,bclmid,bcldown);
  DiagonalDraw(DDupDown,dcl,RtX,RtY,xa,ya);
end;
procedure TFlagBase.DiagonalTricolorTriband2(cl1,dcl,cl2:TColor;
  RtX,RtY,xa,ya:integer);
begin
  RightRatioSize(RtX,RtY);
  TwoRightTriangleUpDown(Xleft,Yup,XRight,Ylow,cl1,cl2);
  DiagonalDraw(DDDownUp,dcl,RtX,RtY,xa,ya);
end;
procedure TFlagBase.DiagonalTricolorTriband3(cl1,dcl,cl2:TColor;
  RtX,RtY,xa,ya:integer);
begin
  RightRatioSize(RtX,RtY);
  TwoRightTriangleDownUp(Xleft,Yup,XRight,Ylow,cl1,cl2);
  DiagonalDraw(DDUpDown,dcl,RtX,RtY,xa,ya);
end;


procedure TFlagBase.StripeBase13(c1,c2:TColor);
var i:integer; yHeight:real;
begin
  Rectangular(Xleft,Yup,XRight,Ylow,c1);
  i:=1;
  yHeight:=(YLow-Yup)/13;
  repeat
    Rectangular(Xleft,Yup+round(yHeight*i),XRight,
      Yup+round(yHeight*(i+1)-1),c2);
    i:=i+2;
  until i>12;
end;

procedure TFlagBase.CantonBasic( upcolor, downcolor, cantoncolor: TColor);
begin
  BiColorFlagHorizontal(upcolor,downcolor);
  Rectangular(Xleft,Yup,((XRight-XLeft) div 3)+Xleft,
    Yup+(YLow-Yup)div 2,cantoncolor);
end;

procedure TFlagBase.LineStarOfDavid(x1,y1,x2,y2:integer;StarColor:TColor);
var dx,dy:integer;
begin
  canvas.pen.color:=StarColor;
  canvas.brush.color:=StarColor;
  dy:=(y2-y1) div 4;
  dx:=x1+(x2-x1) div 2;
  Canvas.Line(x1,y1+dy,x2,y1+dy);
  Canvas.Line(x1,y1+dy,dx,y2);
  Canvas.Line(x1,y2-dy,dx,y1);
  Canvas.Line(x1,y2-dy,x2,y2-dy);
  Canvas.Line(x2,y1+dy,dx,y2);
  Canvas.Line(x2,y2-dy,dx,y1);
end;
procedure TFlagBase.StarOfDavid(x1,y1,x2,y2:integer;StarColor,BaseColor:TColor);
var PaintRect : TRect;
   dy,t,dx,x,y:integer;
begin
  y:=y2-y1;
  t:=y div 12;
  dy:=y div 4;
  canvas.pen.color:=StarColor;
  canvas.brush.color:=StarColor;
  PaintRect:=Rect(x1,y1,x2,y2-dy);
  PaintTriangle(Canvas,PaintRect,pi/2);
  PaintRect:=Rect(x1,y1+dy,x2,y2);
  PaintTriangle(Canvas,PaintRect,pi*3/2);
  canvas.pen.color:=BaseColor;
  canvas.brush.color:=BaseColor;
  dx:=Round(((y2-y1)/2+4*t)*sqrt(3)/6);
  PaintRect:=Rect(x1+dx,y1+dy+t,x2-dx,y2-dy-t);
  PaintHexagon(Canvas,PaintRect,0.0);
  dx:=round(t*sqrt(3));
  x:=round(2*t*sqrt(3)/3+t*sqrt(3));
  PaintRect:=Rect(x1+dx,y1+4*t,x1+x,y1+5*t);
  PaintTriangle(Canvas,PaintRect,pi*3/2);
  PaintRect:=Rect(x2-x,y1+4*t,x2-dx,y1+5*t);
  PaintTriangle(Canvas,PaintRect,pi*3/2);
  PaintRect:=Rect(x1+dx,y2-5*t,x1+x,y2-4*t);
  PaintTriangle(Canvas,PaintRect,pi/2);
  PaintRect:=Rect(x2-x,y2-5*t,x2-dx,y2-4*t);
  PaintTriangle(Canvas,PaintRect,pi/2);
  dx:=round((2*t*sqrt(3)/3)/2);
  x:=x1+(x2-x1) div 2;
  PaintRect:=Rect(x-dx,y1+2*t,x+dx,y1+3*t);
  PaintTriangle(Canvas,PaintRect,pi/2);
  PaintRect:=Rect(x-dx,y2-3*t,x+dx,y2-2*t);
  PaintTriangle(Canvas,PaintRect,pi*3/2);
end;
procedure TFlagBase.Diamond(x1,x2,x3,uy,cy,ly:integer;DColor:Tcolor);
var  po1,po2,po3,po4:TPoint;
begin
    Po1.x:=x1; Po1.y:=cy;
    Po2.x:=x2; Po2.y:=ly;
    po3.x:=x3; Po3.y:=Po1.y;
    Po4.x:=Po2.x; Po4.y:=uy;
    FourCornered(Po1,Po2,Po3,Po4,DColor);
end;
procedure TFlagBase.SunRay(cp:TPoint; r:integer; SunRayColor:TColor);
var Q:array[0..1] of TPoint;
   i,x1,y1,x2,y2,sx,sy,ex,ey,r2,r4:Integer;
   fii:Double;
begin
  fii:=Pi/3;
   r2:=r div 2; r4:=round(r*sqrt(61)/10);

   Q[0]:=Point(cP.x,cP.y-r);
   Q[1]:=Point(cP.x+Round(0.6*r),cP.y-r2);
   x1:=Q[1].x-r4; y1:=Q[1].y-r4;
   x2:=Q[1].x+r4; y2:=Q[1].y+r4;
   sx:=Q[0].x; sy:=Q[0].y;
   ex:=cP.x; ey:=cP.y;
   canvas.pen.color:=SunRayColor;
   canvas.brush.color:=SunRayColor;
   canvas.Line(cp.x,cp.Y-r2,cp.x,cp.y+r2);
   for i:=0 to 5 do
     begin
       canvas.Chord(x1,y1,x2,y2,sx,sy,ex,ey);
       PolycRotate(Q,cP,fii);
       x1:=Q[1].x-r4; y1:=Q[1].y-r4;
       x2:=Q[1].x+r4; y2:=Q[1].y+r4;
       sx:=Q[0].x; sy:=Q[0].y;
     end;

   Q[0]:=Point(cP.x,cP.y-r);
   Q[1]:=Point(cP.x-Round(0.6*r),cP.y-r2);
   x1:=Q[1].x-r4; y1:=Q[1].y-r4;
   x2:=Q[1].x+r4; y2:=Q[1].y+r4;
   ex:=Q[0].x; ey:=Q[0].y;
   sx:=cP.x; sy:=cP.y;
   for i:=0 to 5 do
     begin
       canvas.Chord(x1,y1,x2,y2,sx,sy,ex,ey);
       PolycRotate(Q,cP,fii);
       x1:=Q[1].x-r4; y1:=Q[1].y-r4;
       x2:=Q[1].x+r4; y2:=Q[1].y+r4;
       ex:=Q[0].x; ey:=Q[0].y;
     end;
end;

procedure TFlagBase.XSaltires(x1,y1,x2,y2,CrossLineWidth:integer;
     CrossLineColor:TColor);
var Po1,Po2,Po3,Po4:TPoint;
    Wdelta:integer;
begin
  Wdelta:= round(CrossLineWidth / sqrt(2));
  Po1:= point(x1,y1+wdelta);
  Po2:= point(x1+wdelta,y1);
  Po3:= point(x2,y2-wdelta);
  Po4:= point(x2-wdelta,y2);
  FourCornered(Po1,Po2,Po3,Po4,CrossLineColor);
  Po1:= point(x1,y2-wdelta);
  Po2:= point(x1+wdelta,y2);
  Po3:= point(x2,y1+wdelta);
  Po4:= point(x2-wdelta,y1);
  FourCornered(Po1,Po2,Po3,Po4,CrossLineColor);
end;


end.

