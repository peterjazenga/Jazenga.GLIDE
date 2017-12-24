{
/***************************************************************************
                             Ellipse.pp
                             ------------
         Ellipse (and circle) routines for use within RingChart.pp


***************************************************************************/

*****************************************************************************
*
*  This file is part of the Lazarus Component Library (LCL)
*
*  See the file COPYING.LCL, included in this distribution,
*  for details about the copyright.
*
*  This program is distributed in the hope that it will be useful,
*  but WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*
*****************************************************************************
Autor: Salvatore Coppola

}



unit Ellipse;
{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, GraphMath;

const toll=0.0000001;

//P0=Ellipse center;
//a=semi-axe along x;
//b=semi-axe along y;
//theta azimut inverse counter-clockwise from x axe
function EllipseRho(a,b,theta:extended):extended;
function EllipsePoint(P0:TFloatPoint;a,b,theta:extended):TFloatPoint;
function EllipseArea(a,b,theta:extended):extended;
function EllipseArea(a,b,theta1,theta0:extended):extended;
function EllipseGetAnglePercentageArea(a,b,Per100:extended):extended;

function EllipseToEllipseConcentric(p0:TFloatPoint;a1,b1,a2,b2:extended;var p1,p2,p3,p4:TFloatPoint):boolean;
function EllipseToEllipseYShift(p01,p02:TFloatPoint;a1,b1,a2,b2:extended; var p1,p2,p3,p4:TFloatPoint):boolean;
function EllipseToEllipse(p01,p02:TFloatPoint;a1,b1,a2,b2:extended; var p1,p2,p3,p4:TFloatPoint):boolean;

implementation

function Equal(arg1,arg2,eps:extended):boolean;
begin
  Result:=abs(arg1-arg2)<eps;
end;

function EllipseRho(a,b,theta:extended):extended;
var c_e,e_e:extended;
begin
  if a=b then begin  //circle
    Result:=a;
    exit
  end;
  if a>=b then begin
    c_e:=sqrt(a*a-b*b);
    e_e:=c_e/a;
    Result:=sqrt(b*b/(1-sqr(e_e*cos(theta))));
  end else begin
    c_e:=sqrt(-a*a+b*b);
    e_e:=c_e/b;
    Result:=sqrt(a*a/(abs(1-sqr(e_e*cos(theta-pi/2)))));
  end;
end;

(*canvas coordinate  +x -y*)
function EllipsePoint(p0:TFloatPoint;a,b,theta:extended):TFloatPoint;
var rho:extended;
begin
  rho:=EllipseRho(a,b,theta);
  Result.X:=p0.X+rho*cos(theta);
  Result.Y:=p0.Y-rho*sin(theta);
end;

function EllipseArea(a,b,theta:extended):extended;
var alpha:extended;
begin
  if a=b then begin //circle
    Result:=0.5*a*b*theta;
    exit
  end;
  if theta=0 then begin
    Result:=0;
    exit
  end;
  if Equal(theta,0.5*pi,toll) then begin
    Result:=0.25*pi*a*b;
    exit
  end;
  if Equal(theta,pi,toll) then begin
    Result:=0.5*pi*a*b;
    exit
  end;
  if Equal(theta,1.5*pi,toll) then begin
    Result:=0.75*pi*a*b;
    exit
  end;
  if Equal(theta,2*pi,toll) then begin
    Result:=pi*a*b;
    exit
  end;
  if theta<0.5*pi then begin
    try
      alpha:=arctan(a*sin(theta)/(b*cos(theta)))
    except
      if Equal(theta,0.5*pi,toll) then
        alpha:=0.5*pi;
    end;
    Result:=1/2*a*b*abs(alpha);
    exit
  end;
  if theta<pi then begin
    try
      alpha:=arctan(b*sin(theta-pi/2)/(a*cos(theta-pi/2)))
    except
      if Equal(theta-pi/2,0.5*pi,toll) then
        alpha:=0.5*pi;
    end;
    Result:=1/2*a*b*abs(alpha)+0.25*pi*a*b;
    exit
  end;
  if theta<1.5*pi then begin
    try
      alpha:=arctan(a*sin(theta-pi)/(b*cos(theta-pi)))
    except
      if Equal(theta-pi,0.5*pi,toll) then
        alpha:=0.5*pi;
    end;
    Result:=1/2*a*b*abs(alpha)+0.5*pi*a*b;
    exit
  end;
  if theta<2*pi then begin
    try
      alpha:=arctan(b*sin(theta-1.5*pi)/(a*cos(theta-1.5*pi)))
    except
      if Equal(theta-1.5*pi,0.5*pi,toll) then
        alpha:=0.5*pi;
    end;
    Result:=1/2*a*b*abs(alpha)+0.75*pi*a*b;
    exit
  end;
  raise Exception.Create('Error in EllipseArea: Can''t get Result');
end;

function EllipseArea(a,b,theta1,theta0:extended):extended;
begin
  Result:=EllipseArea(a,b,theta1)-EllipseArea(a,b,theta0);
end;

function EllipseGetAnglePercentageArea(a,b,Per100:extended):extended;
begin
  if Equal(Per100,1.00,toll) then
    Per100:=1.00;
  if a=b then begin //circle
    Result:=2*pi*Per100;
    exit
  end;
  if Per100<0.25 then begin
    Result:=arctan(b*sin(2*pi*Per100)/(a*cos(2*pi*Per100)));
    exit
  end;
  if Per100=0.25 then begin
    Result:=0.5*pi;
    exit
  end;
  if Per100<0.50 then begin
    Result:=arctan(a*sin(2*pi*(Per100-0.25))/(b*cos(2*pi*(Per100-0.25))))+0.5*pi;
    exit
  end;
  if Per100=0.50 then begin
    Result:=pi;
    exit
  end;
  if Per100<0.75 then begin
    Result:=arctan(b*sin(2*pi*(Per100-0.50))/(a*cos(2*pi*(Per100-0.50))))+pi;
    exit
  end;
  if Per100=0.75 then begin
    Result:=1.5*pi;
    exit
  end;
  if Per100<1.00 then begin
    Result:=arctan(a*sin(2*pi*(Per100-0.75))/(b*cos(2*pi*(Per100-0.75))))+1.5*pi;
    exit
  end;
  if Per100=1.00 then begin
    Result:=2*pi;
    exit
  end;
  raise Exception.Create('Error in EllipseGetAnglePercentage: Can''t get Result');
end;

procedure dist2coseni(x1,y1,x2,y2:extended;var dist,teta:extended);
var dx,dy:extended;
    Quadrante:1..4;
begin
  dx:=x2-x1;
  dy:=y2-y1;
  dist:=sqrt(sqr(dx)+sqr(dy));
  if (dx>0)and(dy>=0)then
    Quadrante:=1;
  if (dx<0)and(dy>=0)then
    Quadrante:=2;
  if (dx<0)and(dy<0)then
    Quadrante:=3;
  if (dx>0)and(dy<0)then
    Quadrante:=4;
  case Quadrante of
    1:teta:=arctan(dy/dx);
    2:teta:=arctan(dy/dx)+pi;
    3:teta:=arctan(dy/dx)+pi;
    4:teta:=arctan(dy/dx)+2*pi
  end;
  if dx=0 then begin
    if dy>0 then
      teta:=pi/2;
    if dy<0 then
      teta:=3/2*pi;
    if dy=0 then
      raise Exception.Create('Error in dist2coseni: dx=0 e dy=0');
  end;
end;

function EllipseToEllipseConcentric(p0:TFloatPoint;a1,b1,a2,b2:extended;var p1,p2,p3,p4:TFloatPoint):boolean;
var num1,num2,den:extended;
begin
  if (a1=b2)and(a2=b1) then begin
    p1:=EllipsePoint(p0,a1,b1,pi/4);
    p2:=EllipsePoint(p0,a1,b1,pi/4+pi/2);
    p3:=EllipsePoint(p0,a1,b1,pi/4+pi);
    p4:=EllipsePoint(p0,a1,b1,pi/4+3*pi/2);
    Result:=true;
    exit
  end;
  if a1=a2 then begin
    p1:=p0;
    p1.X:=p0.X+a1;
    p2:=p0;
    p2.X:=p0.X-a1;
    p3:=p2;
    p4:=p1;
    Result:=true;
    exit
  end;
  if b1=b2 then begin
    p1:=p0;
    p1.Y:=p0.Y+b1;
    p2:=p1;
    p3:=p0;
    p3.Y:=p0.Y-b1;
    p4:=p3;
    Result:=true;
    exit
  end;

  if (a1-a2)/(b1-b2)>0 then begin //concentrici senza intesezioni a1>a2 e b1>b2 e viceversa
    Result:=false;
    exit
  end;

  num1:=sqr(b2)-sqr(b1);
  num2:=sqr(a1)-sqr(a2);
  den:=sqr(a1*b2)-sqr(a2*b1);
  if Equal(den,0.0,toll)then begin
    Result:=false;
    exit
  end;
  Result:=((num1/den)>=0)and((num2/den)>=0);
  if Result then begin
    p1.X:=p0.X+a1*a2*sqrt(num1/den);
    p1.Y:=p0.Y+b1*b2*sqrt(num2/den);
    p2.X:=p0.X-a1*a2*sqrt(num1/den);
    p2.Y:=p0.Y+b1*b2*sqrt(num2/den);
    p3.X:=p0.X-a1*a2*sqrt(num1/den);
    p3.Y:=p0.Y-b1*b2*sqrt(num2/den);
    p4.X:=p0.X+a1*a2*sqrt(num1/den);
    p4.Y:=p0.Y-b1*b2*sqrt(num2/den);
  end;
end;

function EllipseToEllipseYShift(p01,p02:TFloatPoint;a1,b1,a2,b2:extended; var p1,p2,p3,p4:TFloatPoint):boolean;
var n1,n2:extended;
    m1,m2,m3,m4:extended;
    d,mntmp:extended;
begin
  Result:=false;
  if not Equal(p01.X,p02.X,toll) then begin
    raise Exception.Create('Error in EllipseToEllipseYShift: ellipses not YShifted');
    exit;
  end;
  
  if not Equal(sqr(a1)*sqr(b2)-sqr(a2)*sqr(b1),0,toll) then begin
    d:=p02.Y-p01.y;
    try
      n1:=b1*(b2*sqrt(sqr(a1)*sqr(a1)*sqr(b2)-sqr(a1)*sqr(a2)*(sqr(b1)+sqr(b2)-sqr(d))+sqr(a2)*sqr(a2)*sqr(b1))+sqr(a2)*b1*d)/(sqr(a2)*sqr(b1)-sqr(a1)*sqr(b2));
      n2:=b1*(b2*sqrt(sqr(a1)*sqr(a1)*sqr(b2)-sqr(a1)*sqr(a2)*(sqr(b1)+sqr(b2)-sqr(d))+sqr(a2)*sqr(a2)*sqr(b1))-sqr(a2)*b1*d)/(sqr(a1)*sqr(b2)-sqr(a2)*sqr(b1));

      if n2>n1 then begin
        mntmp:=n1;
        n1:=n2;
        n2:=mntmp
      end;

      m1:=a1*sqrt(sqr(b1)-sqr(n1))/b1;
      m2:=-a1*sqrt(sqr(b1)-sqr(n1))/b1;

      if m2>m1 then begin
        mntmp:=m1;
        m1:=m2;
        m2:=mntmp
      end;

      m3:=a1*sqrt(sqr(b1)-sqr(n2))/b1;
      m4:=-a1*sqrt(sqr(b1)-sqr(n2))/b1;

      if m4>m3 then begin
        mntmp:=m3;
        m3:=m4;
        m4:=mntmp
      end;

      p1.X:=m2+p01.X;
      p1.Y:=n2+p01.Y;

      p2.X:=m1+p01.X;
      p2.Y:=n2+p01.Y;

      p3.X:=m1+p01.X;
      p3.Y:=n1+p01.Y;

      p4.X:=m2+p01.X;
      p4.Y:=n1+p01.Y;

      Result:=true;
    except
      raise Exception.Create('Error in EllipseToEllipseYShift: Can''t get intersection');
    end;
  end else begin
    //todo
  end;
end;

function EllipseToEllipse(p01,p02:TFloatPoint;a1,b1,a2,b2:extended; var p1,p2,p3,p4:TFloatPoint):boolean;
begin
  Result:=false;
  //todo
end;
end.

