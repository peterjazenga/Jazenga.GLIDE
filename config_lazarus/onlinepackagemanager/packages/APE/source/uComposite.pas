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
unit uComposite;

interface

Uses uVector, uAbstractCollection, MAth, uAbstractPArticle, uMathUtil, Classes,
     uSpringConstraint;

Type

Composite = Class(AbstractCollection)
Private
  delta : Vector;
  function GetFixed: Boolean;
  procedure SetFixed(const Value: Boolean);
Public

  Constructor Create; Override;

  Procedure RotateByRadian(AngleRadian : double; Center : Vector);
  Procedure RotateByAngle(AngleDegree : Double; Center : Vector);
  Function GetRelativeAngle(Center, p : Vector) : Double;

  Property Fixed : Boolean read GetFixed Write SetFixed;

end;

implementation

{ Composite }

constructor Composite.Create;
begin
  inherited Create;
  Delta := Vector.Create(0,0);
end;

function Composite.GetFixed: Boolean;
var i : integer;
begin
  result:=true;
  for i:=0 to Particles.Count-1 do
  begin
    if not AbstractPArticle(Particles[i]).Fixed then
    begin
      Result:=False;
      exit;
    end;
  end;
end;

function Composite.GetRelativeAngle(Center, p: Vector): Double;
begin
  Delta.SetTo(p.x-Center.x,p.y-center.y);
  Result := ArcTan2(delta.y,delta.x);
end;


procedure Composite.RotateByAngle(AngleDegree: Double; Center: Vector);
var angleRadians : Double;
begin
  AngleRadians := AngleDegree * PI_OVER_ONE_EIGHTY;
  RotateByRadian(angleRadians, Center);
end;

procedure Composite.RotateByRadian(AngleRadian: double; Center: Vector);
var p : AbstractPArticle;
    pa : TList;
    len : Integer;
    radius, angle : Double;
    i : integer;
begin
  pa:=Particles;
  len:=pa.Count;
  For i:=0 to len-1 do
  begin
    p:=AbstractPArticle(pa[i]);
    Radius := p.center.distance(center);
    Angle := GetRelativeAngle(Center,p.Center) + AngleRadian;
    p.Px := (Cos(Angle) * Radius) + center.x;
    p.Py := (Sin(Angle) * Radius) + center.y;
  end;
end;

procedure Composite.SetFixed(const Value: Boolean);
var i : integer;
begin
  for i:=0 to Particles.Count-1 do
    AbstractPArticle(Particles[i]).Fixed:=Value;
end;

end.
