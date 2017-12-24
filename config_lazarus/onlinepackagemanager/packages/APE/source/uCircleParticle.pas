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
unit uCircleParticle;

interface

Uses uAbstractParticle, uVector, uInterval;

Type

CircleParticle = class(AbstractParticle)
Private
  _Radius : Double;
Public

		{*
		 * @param x The initial x position of this particle.
		 * @param y The initial y position of this particle.
		 * @param radius The radius of this particle.
		 * @param fixed Determines if the particle is fixed or not. Fixed particles
		 * are not affected by forces or collisions and are good to use as surfaces.
		 * Non-fixed particles move freely in response to collision and forces.
		 * @param mass The mass of the particle.
		 * @param elasticity The elasticity of the particle. Higher values mean more elasticity or 'bounciness'.
		 * @param friction The surface friction of the particle.
		 *}
  Constructor Create( x,y,radius : Double;
                      aFixed : Boolean = False;
                      aMass : Double = 1;
                      aElasticity : Double = 0.3;
                      aFriction : Double = 0); Reintroduce;

  Procedure Init; Override;
  Procedure Paint; OVerride;
  Function GetProjection(AnAxis : Vector) : Interval;
  Function GetIntervalX : Interval;
  Function GetIntervalY : Interval;

  Property Radius : Double read _Radius Write _Radius;
End;

implementation

{ CircleParticle }

constructor CircleParticle.Create( x,y,radius : Double;
                                  aFixed : Boolean = False;
                                  aMass : Double = 1;
                                  aElasticity : Double = 0.3;
                                  aFriction : Double = 0);
begin
  inherited Create(x,y,amass,aelasticity,afriction,afixed);
  _Radius := Radius;
end;

function CircleParticle.GetIntervalX: Interval;
begin
  ainterval.min := curr.x - _radius;
  ainterval.max := curr.x + _radius;
	result := ainterval;
end;

function CircleParticle.GetIntervalY: Interval;
begin
  ainterval.min := curr.y - _radius;
  ainterval.max := curr.y + _radius;
  Result := ainterval;
end;

function CircleParticle.GetProjection(AnAxis: Vector): Interval;
var c : Double;
begin
  c := Samp.Dot(anAxis);
  aInterval.min := c - _radius;
  ainterval.max := c + _radius;
  Result := aInterval;
end;

procedure CircleParticle.Init;
begin
  //inherited ;
  //none ?
end;

procedure CircleParticle.Paint;
begin
 // inherited;
  aRenderer.Circle(curr.x,curr.y,_Radius,0);
end;

end.
