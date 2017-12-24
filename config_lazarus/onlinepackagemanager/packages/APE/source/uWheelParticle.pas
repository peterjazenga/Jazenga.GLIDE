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
unit uWheelParticle;

interface

Uses uRimParticle, uCircleParticle, uApeEngine, uVector, Math, uMAthUtil, uAbstractParticle;

Type

//* A particle that simulates the behavior of a wheel
WheelParticle = class(CircleParticle)
Private
		rp : RimParticle;
		tan : Vector;
		normSlip : Vector;
		orientation : Vector;
		_traction: Double;
    function GetAngularVelocity: Double;
    function GetSpeed: Double;
    function GetTraction: Double;
    procedure SetAngularVelocity(const Value: Double);
    procedure SetSpeed(const Value: Double);
    procedure SetTraction(const Value: Double);
    function GetRadian: Double;
    function GetAngle: Double;
Public
{
		 * @param x The initial x position.
		 * @param y The initial y position.
		 * @param radius The radius of this particle.
		 * @param fixed Determines if the particle is fixed or not. Fixed particles
		 * are not affected by forces or collisions and are good to use as surfaces.
		 * Non-fixed particles move freely in response to collision and forces.
		 * @param mass The mass of the particle
		 * @param elasticity The elasticity of the particle. Higher values mean more elasticity.
		 * @param friction The surface friction of the particle.
		 * @param traction The surface traction of the particle.
		 * <p>
		 * Note that WheelParticles can be fixed but rotate freely.
		 * </p>
		 */
}

  Constructor Create(anApeEngine : ApeEngine; x,y,
                     aRadius : Double;
                     afixed : Boolean = False;
                     aMass : Double = 1;
                     aElasticity : Double = 0.3;
                     aFriction : Double = 0;
                     Traction : Double = 1); reintroduce;

  Procedure Update(dt2 : Double; Force, MassLEssForce : Vector; Damping : Double); Override;

  Procedure ResolveCollision(mtd,vel,n : vector; d : Double; o : Integer; p : AbstractParticle); Override;
  Procedure Resolve(n : Vector);

  Property Speed : Double read GetSpeed Write SetSpeed;
  Property AngularVelocity : Double Read GetAngularVelocity Write SetAngularVelocity;
  Property Traction : Double read GetTraction Write SetTraction;

  property Angle : Double read GetAngle;
  property Radian : Double Read GetRadian;
End;


implementation

{ WheelParticle }

constructor WheelParticle.Create(anApeEngine : ApeEngine; x,y,
                     aRadius : Double;
                     afixed : Boolean = False;
                     aMass : Double = 1;
                     aElasticity : Double = 0.3;
                     aFriction : Double = 0;
                     Traction : Double = 1);
begin
  Inherited Create(x,y,aRadius,afixed,aMass,aElasticity, aFriction);
	tan := Vector.Create(0,0);
	normSlip := Vector.Create(0,0);
	rp := RimParticle.Create(anApeEngine,Radius, 2);
	Self.Traction := Traction;
	orientation := Vector.Create(0,0);
end;

function WheelParticle.GetAngle: Double;
begin
  Result := Radian * ONE_EIGHTY_OVER_PI;
end;

function WheelParticle.GetAngularVelocity: Double;
begin
  Result := rp.AngularVelocity;
end;

function WheelParticle.GetRadian: Double;
begin
  Orientation.SetTo(rp.Curr.X,rp.Curr.Y);
  Result := ArcTan2(orientation.x, orientation.y) + Pi;
end;

function WheelParticle.GetSpeed: Double;
begin
  Result := rp.Speed;
end;

function WheelParticle.GetTraction: Double;
begin
  Result := 1 - _traction;
end;

procedure WheelParticle.Resolve(n: Vector);
var cp : Double;
    wheelSurfaceVelocity, combinedVelocity : Vector;
    SlipSpeed : Double;
begin
	// this is the tangent vector at the rim particle
	tan.setTo(-rp.curr.y, rp.curr.x);

	// normalize so we can scale by the rotational speed
	tan := tan.Normalyze;

	// velocity of the wheel's surface
	wheelSurfaceVelocity := tan.mult(rp.speed);

	// the velocity of the wheel's surface relative to the ground
	combinedVelocity := velocity.plusEquals(wheelSurfaceVelocity);

	// the wheel's comb velocity projected onto the contact normal
	cp := combinedVelocity.cross(n);

	// set the wheel's spinspeed to track the ground
  tan.multEquals(cp);
	rp.prev.copy(rp.curr.minus(tan));

  // some of the wheel's torque is removed and converted into linear displacement
	slipSpeed := (1 - _traction) * rp.speed;
	normSlip.setTo(slipSpeed * n.y, slipSpeed * n.x);
	curr.plusEquals(normSlip);
	rp.speed := rp.speed * _traction;
end;

procedure WheelParticle.ResolveCollision(mtd, vel, n: vector; d: Double;
  o: Integer; p: AbstractParticle);
begin
  inherited ResolveCollision(mtd,vel,n,d,o,p);
  Resolve(n.Mult(Sign(d*o)));
end;

procedure WheelParticle.SetAngularVelocity(const Value: Double);
begin
  rp.AngularVelocity:=Value;
end;

procedure WheelParticle.SetSpeed(const Value: Double);
begin
  rp.Speed:=Value;
end;

procedure WheelParticle.SetTraction(const Value: Double);
begin
  _traction:= 1 - Value;
end;

procedure WheelParticle.Update(dt2 : Double; Force, MassLEssForce : Vector; Damping : Double);
begin
  Inherited Update(dt2,Force, MassLessForce,Damping);
  rp.Update(dt2);
end;

end.
