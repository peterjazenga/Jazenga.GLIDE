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
unit uGroup;

interface

uses uAbstractCollection, uvector, Classes, uComposite, uAbstractPArticle, uAbstractConstraint;

Type

Group = Class(AbstractCollection)
Private
  _CollideInternal : Boolean;

  Procedure CheckCollisionGroupInternal;
  Procedure CheckCollisionVsGroup(g : Group);

public
  Composites : TList;
  CollisionList : TList;

  constructor Create(CollideInternal : Boolean = False); Reintroduce; Virtual;
  Procedure Init; Override;

  Procedure AddComposite(c : Composite);
  Procedure RemoveComposite(c : Composite);

  Procedure Paint; Override;

  Procedure AddCollidable(g : Group);
  Procedure RemoveCollidable(g : Group);

  Procedure AddCollidableList(list : Tlist);
  Function GetAll : Tlist;

  //Function Cleanup ------> destructor.

  Procedure Integrate(dt2 : Double; Force, MassLessForce : Vector; Damping : Double); Override;
  Procedure SatisfyConstraints; Override;
  Procedure CheckCollision;

  Property CollideInternal : Boolean read _CollideInternal Write _CollideInternal;

end;

implementation

uses MaskUtils;

{ Group }

procedure Group.AddCollidable(g: Group);
begin
  CollisionList.Add(g);
end;

procedure Group.AddCollidableList(list: Tlist);
var i : Integer;
begin
  for i:=0 to list.Count-1 do
    CollisionList.Add(List[i]);
end;

procedure Group.AddComposite(c: Composite);
begin
  Composites.Add(c);
  c.IsParented := true;
  if IsParented then
    c.Init;
end;

procedure Group.CheckCollision;
var i : integer;
begin
  if CollideInternal then
    CheckCollisionGroupInternal;

  for i:=0 to CollisionList.Count-1 do
  begin
    CheckCollisionVsGroup(Group(CollisionList[i]));
  end;
end;

procedure Group.CheckCollisionGroupInternal;
var i,j : Integer;
    ca : Composite;
begin
  CheckInternalCollision;
  for j:=0 to Composites.Count-1 do
  begin
    ca:=Composite(Composites[j]);
    ca.CheckCollisionVsCollection(Self);
    for i:=j+1 to Composites.Count-1 do
      ca.CheckCollisionVsCollection(Composite(Composites[i]));
  end;
end;

procedure Group.CheckCollisionVsGroup(g : Group);
var clen, gclen : Integer;
    i,j : integer;
    c : Composite;
begin
  CheckCollisionVsCollection(g);
  clen := Composites.Count;
  gclen := g.Composites.Count;
  for i:=0 to clen-1 do
  begin
    c:=Composite(Composites[i]);
    c.CheckCollisionVsCollection(g);
    for j:=0 to gclen -1 do
      c.CheckCollisionVsCollection(AbstractCollection(g.Composites[j]));
  end;

  For j:=0 to gclen-1 do
    CheckCollisionVsCollection(Composite(g.Composites[j]));

end;

constructor Group.Create(CollideInternal: Boolean);
begin
  inherited Create;
  _CollideInternal:=CollideInternal;
  Composites := TList.Create;
  CollisionList := Tlist.Create;
end;

function Group.GetAll: Tlist;
var i : integer;
begin
  Result:=Tlist.create;
  for i:=0 to Particles.Count-1 do
    Result.Add(Particles[i]);
  for i:=0 to Constraints.Count-1 do
    Result.Add(Constraints[i]);
  for i:=0 to Composites.Count-1 do
    Result.Add(Composites[i]);
end;

procedure Group.Init;
var i : integer;
begin
  //inherited Init;
  For i:=0 to Composites.Count-1 do
    Composite(Composites[i]).Init;
end;

procedure Group.Integrate(dt2 : Double; Force, MassLessForce : Vector; Damping : Double);
var i : integer;
begin
  inherited Integrate(dt2,Force,MasslessForce,Damping);
  for i:=0 to Composites.Count-1 do
    Composite(Composites[i]).Integrate(dt2,Force,MAssLessForce,Damping);
end;

procedure Group.Paint;
var i : integer;
begin
  //Inherited Paint;
  for i:=0 to Composites.Count-1 do
    Composite(Composites[i]).Paint;

  for i:=0 to Particles.Count-1 do
    AbstractParticle(Particles[i]).Paint;

  for i:=0 to Constraints.Count-1 do
    AbstractConstraint(Constraints[i]).Paint;

end;

procedure Group.RemoveCollidable(g: Group);
begin
  CollisionList.Remove(g);
end;

procedure Group.removeComposite(c: Composite);
begin
  Composites.Remove(c);
  c.IsParented:=False;
  //c.Free; ?
end;

procedure Group.SatisfyConstraints;
var i : integer;
begin
  inherited SatisfyConstraints;
  for i:=0 to Composites.Count-1 do
    Composite(Composites[i]).SatisfyConstraints;
end;

end.
