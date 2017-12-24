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
unit uAbstractCollection;

interface

Uses uVector, uAbstractPArticle, uAbstractConstraint, uSpringConstraint, uSpringConstraintParticle,uCollisionDetector, Classes;

Type

AbstractCollection = Class
Private
  _IsPArented : Boolean;
Public
  Particles : TList;
  Constraints : TList;


  constructor Create; Virtual;
  Destructor Destroy; Override;

  Function GetIsParented : Boolean; Overload;
  Procedure SetIsParented(value : Boolean); Overload;

  Procedure Init; Virtual;

  Procedure AddParticle(p : AbstractPArticle);
  Procedure AddConstraint(c : AbstractConstraint);
  Procedure RemoveParticle(p : AbstractPArticle);
  Procedure RemoveConstraint(c : AbstractConstraint);

  Procedure Paint; Virtual;
  Procedure Integrate(dt2 : Double; Force, MassLessForce : Vector; Damping : Double); Virtual;
  Procedure SatisfyConstraints; Virtual;
  Procedure CheckCollisionVsCollection(ac : AbstractCollection);
  Procedure CheckInternalCollision;

  Property IsPArented : Boolean read GetIsPArented Write SetIsParented;

end;


implementation

{ AbstractCollection }

procedure AbstractCollection.AddConstraint(c: AbstractConstraint);
begin
  constraints.Add(c);
  if IsPArented then
    c.Init;
end;

procedure AbstractCollection.AddPArticle(p: AbstractPArticle);
begin
  Particles.Add(p);
  if IsPArented then
    p.Init;
end;

procedure AbstractCollection.CheckCollisionVsCollection(ac: AbstractCollection);
var clen, plen, acplen, Acclen, j,x,n : Integer;
    pga,pgb : AbstractParticle;
    cgb,cga : SpringConstraint;
begin
  //Every particle in this collection...
  plen := PArticles.Count;
  for j:=0 to plen-1 do
  begin
    pga := AbstractPArticle(particles[j]);
    if not(pga.Collidable) then
      continue;

    //...vs every particle of the other collection
    acplen := ac.Particles.Count;
    for x:=0 to acplen-1 do
    begin
      pgb:=AbstractPArticle(ac.Particles[x]);
      if pgb.Collidable then
        CollisionDetectorInstance.Test(pga,pgb);
    end;

    //...vs every constraint of the other collection
    acclen := ac.Constraints.Count;
		for x := 0 to acclen-1 do
    begin
		  cgb := SpringConstraint(ac.constraints[x]);
		  if ((cgb.collidable) and not(cgb.isConnectedTo(pga))) then
      begin
		    SpringConstraintParticle(cgb.scp).UpdatePosition;
		    CollisionDetectorInstance.test(pga, SpringConstraintParticle(cgb.scp));
      end;
    end;

  end;

  //Every constraint of the collection
	clen := constraints.Count;
	for j:=0 to clen-1 do
  begin
    cga := SpringConstraint(constraints[j]);
    if not(cga.Collidable) then
      Continue;
    //...vs every particle of the other collection
    // ...vs every particle in the other collection
	  acplen := ac.particles.Count;
		for n:=0 to acplen-1 do
    begin
	    pgb := AbstractParticle(ac.particles[n]);
	    if ((pgb.collidable) And Not(cga.isConnectedTo(pgb))) Then
      begin
	      SpringConstraintParticle(cga.scp).updatePosition;
	  		CollisionDetectorInstance.test(pgb, SpringConstraintParticle(cga.scp));
	    end;
    end;
  end;
end;

procedure AbstractCollection.CheckInternalCollision;
var clen, plen, i,j,n : Integer;
    pa,pb : AbstractParticle;
    c : SpringConstraint;
begin
  plen := PArticles.Count;
  // every particle in this AbstractCollection...
  for j:=0 to plen-1 do
  begin
    pa := AbstractPArticle(particles[j]);
    if not(pa.Collidable) then
      continue;
    // ...vs every other particle in this AbstractCollection
    for i:=j+1 to plen-1 do
    begin
      pb:=AbstractPArticle(PArticles[i]);
      if pb.Collidable then
        CollisionDetectorInstance.Test(pa,pb);
    end;

		// ...vs every other constraint in this AbstractCollection
   	clen := Constraints.Count;
		for n := 0 to clen-1 do
    begin
  		c := SpringConstraint(Constraints[n]);
			if (c.collidable and not(c.isConnectedTo(pa))) then
      begin
				SpringConstraintParticle(c.scp).UpdatePosition;
				CollisionDetectorInstance.test(pa, SpringConstraintParticle(c.scp));
      end;
    end;

  end;
end;

constructor AbstractCollection.Create;
begin
  _IsPArented:=False;
  Particles:=TList.Create;
  Constraints:=TList.Create;
end;

destructor AbstractCollection.Destroy;
begin
  Particles.Free;
  Constraints.Free;
  inherited;
end;

procedure AbstractCollection.Init;
var i : integer;
    p : AbstractPArticle;
//    c : Springconstraint;
begin
  For I:=0 to Particles.Count-1 do
  begin
    p:=AbstractPArticle(Particles[i]);
    p.Init;
  end;

{
  For I:=0 to Constraints.Count-1 do
  begin
    c:=Constraints[i];
    c.Init;
  end;
}
end;

procedure AbstractCollection.Integrate(dt2: Double; Force,
  MassLessForce: Vector; Damping: Double);
var i : integer;
begin
  for i:=0 to Particles.Count-1 do
    AbstractParticle(Particles[i]).Update(dt2,Force,MasslessForce,Damping);
end;

procedure AbstractCollection.setIsPArented(value: Boolean);
begin
  _IsPArented:=Value;
end;

function AbstractCollection.getIsPArented: Boolean;
begin
  Result:=_IsPArented;
end;

procedure AbstractCollection.RemovePArticle(p: AbstractPArticle);
begin
  Particles.Remove(p);
  //p.Free; ?
end;

procedure AbstractCollection.RemoveConstraint(c: AbstractConstraint);
begin
  Constraints.Remove(c);
  //c.Free; ?
end;

procedure AbstractCollection.SatisfyConstraints;
var len : Integer;
    i : integer;
    c : AbstractConstraint;
begin
  len := Constraints.Count;
  for i:=0 to len-1 do
  begin
    c:=  Abstractconstraint(Constraints[i]);
    c.Resolve;
  end;
end;


procedure AbstractCollection.Paint;
var p : AbstractParticle;
    c : SpringConstraint;
    i : integer;
begin
  for i:=0 to Particles.Count-1 do
  begin
    p := AbstractParticle(Particles[i]);
    //if not(p.Fixed) then
    p.Paint;
  end;

  for i:=0 to Constraints.Count-1 do
  begin
    c := SpringConstraint(Constraints[i]);
    //if Not(c.Fixed) then
    c.Paint;
  end;

end;

end.
