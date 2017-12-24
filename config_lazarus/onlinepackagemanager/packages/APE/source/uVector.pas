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
unit uVector;

interface

Uses Math; //For sinCos

Type


Vector = class
Public
  x : Double;
  y : Double;

  Constructor Create(px : Double = 0; py : Double = 0);
  Procedure SetTo(px,py : double);
  Procedure Copy(FromV : Vector);
  Function Dot(v : Vector) : Double;
  Function Cross(v : Vector) : double;
  Function Plus(v : Vector) : Vector;
  Function PlusEquals(v : Vector) : Vector;
  Function Minus(v : Vector) : Vector;
  Function MinusEquals(v : Vector) : Vector;
  Function Mult(s : Double) : Vector;
  Function MultEquals(s : Double) : Vector;
  Function DivEquals(s : Double) : Vector;
  Function Distance(v : Vector) : Double;
  Function Times(v : Vector) : Vector;
  Function Magnitude : Double;
  Function Normalyze : Vector;
  Function ToString : String;

  procedure TurnAngle(a : Double);
  Procedure ResetAngle;
end;

implementation

uses SysUtils;

{ Vector }

procedure Vector.Copy(FromV: Vector);
begin
  x:=FromV.x;
  y:=FromV.y;
end;

constructor Vector.Create(px : Double = 0; py: Double = 0);
begin
  x:=px;
  y:=py;
end;

function Vector.Cross(v: Vector): double;
begin
  result:= x * v.y - y * v.x;
end;

function Vector.Distance(v: Vector): Double;
var delta : Vector;
begin
  delta:=Minus(v);
  Result:=Delta.Magnitude;
end;

function Vector.DivEquals(s: Double): Vector;
begin
  if s=0 then
    s:=0.0001;
  x:=x/s;
  y:=y/s;
  Result:=Self;
end;

function Vector.Dot(v: Vector): Double;
begin
  result:= x * v.x + y * v.y;
end;

function Vector.Magnitude: Double;
begin
  result:=sqrt(x*x+y*y);
end;

function Vector.Minus(v: Vector): Vector;
begin
  Result:=Vector.Create(x-v.x,y-v.y);
end;

function Vector.MinusEquals(v: Vector): Vector;
begin
  x:=x-v.x;
  y:=y-v.y;
  Result:=Self;
end;

function Vector.Mult(s : Double): Vector;
begin
  Result:=Vector.Create(x * s,y * s);
end;

function Vector.MultEquals(s : Double): Vector;
begin
  x:=x*s;
  y:=y*s;
  Result:=Self;
end;

function Vector.Normalyze: Vector;
var m : Double;
begin
  m:=Magnitude;
  if m=0 then
    m:=0.0001;
  result:=Mult(1/m);
end;

function Vector.Plus(v: Vector): Vector;
begin
  Result:=Vector.Create(x+v.x,y+v.y);
end;

function Vector.PlusEquals(v: Vector): Vector;
begin
  x:=x+v.x;
  y:=y+v.y;
  Result:=Self;
end;

procedure Vector.ResetAngle;
begin
  x:=Magnitude;
  y:=0;
end;

procedure Vector.SetTo(px, py: double);
begin
  x:=px;
  y:=py;
end;

function Vector.Times(v: Vector): Vector;
begin
  Result:=Vector.Create(x*v.x,y*v.y);
end;

function Vector.ToString: String;
begin
  Result:=(FloatToStr(x)+' : '+FloatToStr(y));
end;

Procedure PolarToCartesian(const R, Phi: Extended; var X, Y: Double);
var
  Sine, CoSine: Extended;
begin
  SinCos(Phi, Sine, CoSine);
  X := R * CoSine*-1;
  Y := R * Sine *-1;
end;

procedure Vector.TurnAngle(a: Double);
var n : Extended;
begin
  n:=Magnitude;
  PolarToCartesian(n,a,X,Y);
end;

end.
