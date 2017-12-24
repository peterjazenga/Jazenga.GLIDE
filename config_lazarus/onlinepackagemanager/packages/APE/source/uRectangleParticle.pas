{
APE (Actionscript Physics Engine) is an AS3 open source 2D physics engine
Copyright 2006, Alec Cove

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

Contact: ape@cove.org

Converted to ObjectPascal by Vincent Gsell vincent.gsell@gmail.com
}
unit uRectangleParticle;

interface

uses uAbstractParticle, uMathUtil, uVector, uInterval, uRender, sysUtils;

Type

TRctTypeDouble = Array[0..1] of Double;
TRctTypeVector = Array[0..1] of Vector;

RectangleParticle = Class(AbstractParticle)
Private
  _extents : TRctTypeDouble;
  _axes : TRctTypeVector;
  _radian : Double;
    function GetAngle: Double;
    function GetHEight: Double;
    function GetRadian: Double;
    function GetWidth: Double;
    procedure SetAngle(const Value: Double);
    procedure SetAxes(const Value: Double);
    procedure SetHeight(const Value: Double);
    procedure SetRadian(const Value: Double);
    procedure SetWidth(const Value: Double);
Public

  Constructor Create(x,y,awidth,aheight, arotation : double; aFixed : Boolean; aMass : Double = 1; aElasticity : Double = 0.3; aFriction: Double = 0); reintroduce;

  Procedure Init; Override;
  Procedure Paint; OVerride;
  Function GetProjection(AnAxis : Vector) : Interval;

  Property Radian : Double read GetRadian Write SetRadian;
  Property Angle : Double read GetAngle Write SetAngle;
  Property Width : Double read GetWidth Write SetWidth;
  Property Height : Double read GetHEight Write SetHeight;
  Property Axes : TRctTypeVector read _axes;
  Property Extents : TRctTypeDouble read _Extents Write _Extents;

end;

implementation

uses uAbstractItem;

{ RectangleParticle }


constructor RectangleParticle.Create(x,y,awidth,aheight, arotation : double; aFixed : Boolean; aMass : Double = 1; aElasticity : Double = 0.3; aFriction: Double = 0);

begin
  inherited Create(x,y,amass,aelasticity,afriction,afixed);
  _Extents[0]:=awidth/2;
  _Extents[1]:=aheight/2;
  _axes[0]:=Vector.Create(0,0);
  _axes[1]:=Vector.Create(0,0);
  Radian:=aRotation;
end;

function RectangleParticle.GetAngle: Double;
begin
  Result :=Radian * ONE_EIGHTY_OVER_PI;
end;

function RectangleParticle.GetHEight: Double;
begin
  result:= _Extents[1] * 2;
end;

function RectangleParticle.GetProjection(AnAxis: Vector): Interval;
var radius : Double;
    c : Double;
begin
  Radius := extents[0] * Abs(AnAxis.Dot(axes[0]))+
            extents[1] * Abs(AnAxis.Dot(axes[1]));
  c := Samp.Dot(AnAxis);
  aInterval.min:=c-Radius;
  aInterval.max:=c+Radius;
  Result:=aInterval;
end;

function RectangleParticle.GetRadian: Double;
begin
  result :=_Radian;
end;

function RectangleParticle.GetWidth: Double;
begin
  Result:= _extents[0] * 2;
end;

procedure RectangleParticle.Init;
begin
  //CleanUp;
  Paint;
end;

procedure RectangleParticle.Paint;
begin
  // hop:
  aRenderer.Rectangle(px,py,Width,Height,Angle);
  //aRenderer.Text(px,py,FloatToStr(Angle));

end;

procedure RectangleParticle.SetAngle(const Value: Double);
begin
  Radian := Value * PI_OVER_ONE_EIGHTY;
end;

procedure RectangleParticle.SetAxes(const Value: Double);
var s : Double;
    c : Double;
begin
  s:= Sin(Value);
  c:= Cos(Value);
  axes[0].x:=c;
  axes[0].y:=s;
  axes[1].x:=-s;
  axes[1].y:=c;
end;

procedure RectangleParticle.SetHeight(const Value: Double);
begin
  _Extents[1] := Value /2;
end;

procedure RectangleParticle.SetRadian(const Value: Double);
begin
  _Radian := Value;
  SetAxes(Value);
end;

procedure RectangleParticle.SetWidth(const Value: Double);
begin
  _Extents[0] := Value /2;
end;


end.
