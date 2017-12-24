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
unit uAbstractParticle;

interface

Uses uVector, uInterval, uCollision, uAbstractItem, uRender;

Type

AbstractParticle = Class(AbstractITem)
Private
  Forces, Temp : Vector;
  aCollision : Collision;


  _kfr, _mass, _invMass, _Friction : Double;
  _Fixed, _Collidable : Boolean;

  _Center : Vector;
  _Collision_vn, _Collision_vt : Vector;
  _MultiSample : Integer;


Protected
  aRenderer : AbstractRenderer;
Public
  Curr, Prev, Samp : Vector;
  aInterval : Interval;

  function GetFriction: Double; virtual;
  procedure SetFriction(const Value: Double); Virtual;

  function GetElasticity: Double; virtual;
  procedure SetElasticity(const Value: Double); virtual;

  function GetMass: Double; virtual;
  procedure SetMass(value : Double); Virtual;

  Function GetVelocity : Vector; Virtual;
  Procedure SetVelocity(value : Vector); Virtual;

  Function getPx : Double;  Virtual;
  Procedure SetPx(value : Double);  Virtual;

  Function getPy : Double;  Virtual;
  Procedure setPy(value : Double);  Virtual;


  Constructor Create(x,y,mass,Elasticity, Friction : Double; IsFixed : Boolean); Reintroduce; Virtual;
  Destructor Destroy; OVerride;

  Procedure SetRenderer(TheRenderer : AbstractRenderer);

  Function GetInvMass : Double; Virtual;

  Function Center : Vector;
  Function Position : Vector; Overload;
  Procedure Position(Value : Vector); Overload;

  Procedure Update(dt2 : Double; Force, MassLEssForce : Vector; Damping : Double); Virtual;

  Procedure AddForce(f : Vector);
  Procedure AddMassLessForce(f : Vector);

  Function GetComponents(CollisionNormal : Vector) : Collision;
  Procedure ResolveCollision(mtd,vel,n : vector; d : Double; o : Integer; p : AbstractParticle); Virtual;
  Function ParticleType : Integer;

  Property px : Double read GetPx write SetPx;
  Property py : double read GetPy Write SetPy;
  Property Fixed : Boolean read _Fixed Write _Fixed;

  Property Mass : Double read GetMass Write SetMass;
  Property Velocity : Vector read GetVelocity Write SetVelocity;
  Property Collidable : Boolean read _Collidable Write _Collidable;
  Property Elasticity : Double Read GetElasticity Write SetElasticity;
  Property Friction : Double read GetFriction Write SetFriction;
  Property MultiSample : Integer read _MultiSample Write _MultiSample;
  Property InvMass : Double read GetInvMass;

end;

implementation

uses SysUtils;

{ AbstractPArticle }

procedure AbstractPArticle.AddForce(f: Vector);
var tmp : Vector;
begin
  tmp := f.Mult(InvMass);
  Forces.PlusEquals(tmp);
end;

procedure AbstractPArticle.AddMassLessForce(f: Vector);
begin
  Forces.PlusEquals(f)
end;

function AbstractPArticle.Center: Vector;
begin
  _Center.SetTo(Px,Py);
  Result:= _Center;
end;

constructor AbstractPArticle.Create(x, y, mass, Elasticity,
  Friction: Double; IsFixed: Boolean);
begin
  inherited Create;
  aInterval := Interval.Create(0,0);

  curr := Vector.Create(x,y);
  prev := Vector.Create(x,y);
	samp := Vector.Create(0, 0);
	temp := Vector.Create(0, 0);
	fixed:=isFixed;

	forces := Vector.Create (0, 0);
	_collision_vn := Vector.Create (0, 0);
	_collision_vt := Vector.Create (0, 0);
	aCollision := Collision.Create( _collision_vn, _collision_vt);
	Collidable:=true;

	Self.mass:=mass;
	Self.elasticity:=elasticity;
	Self.friction:=friction;

	//setStyle();

	_center := Vector.Create (0, 0);
	_multisample := 0;
end;

destructor AbstractPArticle.Destroy;
begin
  FreeAndNil(aInterval);
  FreeAndNil(aCollision);
	//delete samp;
	//delete temp;

	//delete forces;
	//delete _collision_vn;
	//delete _collision_vt;
	//delete _center;
end;


function AbstractPArticle.GetComponents(
  CollisionNormal: Vector): Collision;
var vel : Vector;
    vdotn : Double;
begin
  vel := Velocity;
	vdotn := CollisionNormal.dot(vel);
  aCollision.vn:=CollisionNormal.Mult(vdotn);
  aCollision.vt:=vel.Minus(aCollision.vn);
  Result:=aCollision;
end;

function AbstractPArticle.GetInvMass: Double;
begin
  //Original code : "return (fixed()) ? 0 : _invMass;" :/
  Result:=_invMass;
  if Fixed then
    Result:=0;
end;

procedure AbstractPArticle.SetMass(value: Double);
begin
  If Value<=0 then
    Raise Exception.Create('Mass must be not less or equals than 0.');
  _mass := Value;
  _invMass := 1 / _mass;
end;

function AbstractPArticle.ParticleType: Integer;
begin
  Result:=0;
end;

function AbstractPArticle.Position: Vector;
begin
  Result := Vector.Create(Curr.x,Curr.y);
end;

procedure AbstractPArticle.Position(Value: Vector);
begin
  curr.Copy(Value);
  Prev.Copy(Value);
end;

procedure AbstractPArticle.setPx(value: Double);
begin
 	curr.x := Value;
	prev.x := Value;
end;

function AbstractPArticle.getPx: Double;
begin
  Result := curr.x;
end;

procedure AbstractPArticle.setPy(value: Double);
begin
 	curr.y := Value;
	prev.y := Value;
end;

function AbstractPArticle.getPy: Double;
begin
  Result := Curr.y;
end;

procedure AbstractPArticle.ResolveCollision(mtd, vel, n: vector; d: Double;
  o: Integer; p: AbstractParticle);
begin
  curr.PlusEquals(mtd);
  Velocity:=vel;
end;

procedure AbstractPArticle.SetVelocity(Value: Vector);
begin
  prev := curr.Minus(Value);
end;

function AbstractPArticle.GetVelocity: Vector;
begin
  result := curr.Minus(Prev);
end;

procedure AbstractPArticle.Update(dt2: Double; Force,
  MassLEssForce: Vector; Damping: Double);
var nv : Vector;
begin
	if (fixed) then
    Exit;

  //Global forces
	addForce(force);
	addMasslessForce(masslessForce);

  //Integrate
	temp.copy(curr);

	nv := velocity.plus(forces.multEquals(dt2));
	curr.plusEquals(nv.multEquals(damping));
	prev.copy(temp);
	// clear the forces
	forces.setTo(0,0);

  nv.Free;
end;


procedure AbstractPArticle.SetRenderer(TheRenderer: AbstractRenderer);
begin
  aRenderer:=TheRenderer;
end;

function AbstractParticle.GetMass: Double;
begin
  Result := _mass;
end;

function AbstractParticle.GetElasticity: Double;
begin
  Result:=_kfr
end;

procedure AbstractParticle.SetElasticity(const Value: Double);
begin
  _kfr := Value;
end;

function AbstractParticle.GetFriction: Double;
begin
  result := _Friction;
end;

procedure AbstractParticle.SetFriction(const Value: Double);
begin
  _Friction := Value;
end;

end.
