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
unit uApeEngine;

interface

Uses uVector, uGroup, Classes;

Type

ApeEngine = Class
Private
  Force : Vector;
  MasslessForce : Vector;
  TimeSteps : Double;
  _Damping : Double;
  _ConstraintCycles : Integer;
  _ConstraintCollisionCycle : Integer;

  function GetNumGroups: Integer;
  function GetConstraintCollisionCycle: Integer;
  function GetConstraintCycle: Integer;
  procedure SetConstraintCollisionCycle(const Value: Integer);
  procedure SetConstraintCycle(const Value: Integer);
  function GetDamping: Double;
  procedure SetDamping(const Value: Double);

  Procedure Integrate;
  Procedure SatisfyConstraints;
  Procedure CheckCollisions;


Public
  Groups : TList;

  Procedure Init(dt : Double = 0.25);

  Procedure AddForce(v : Vector);
  Procedure AddMasslessForce(v : Vector);
  Procedure AddGroup(g : Group);
  Procedure RemoveGroup(g : Group);

  Procedure Step;
  Procedure Paint;


  Property NumGroups : Integer read GetNumGroups;
  Property ConstraintCycles : Integer read GetConstraintCycle Write SetConstraintCycle;
  Property ConstraintCollisionCycles : Integer read GetConstraintCollisionCycle Write SetConstraintCollisionCycle;
  Property Damping : Double read GetDamping Write SetDamping;
end;

implementation

{ ApeEngine }

procedure ApeEngine.AddForce(v: Vector);
begin
  Force.PlusEquals(v);
end;

procedure ApeEngine.AddGroup(g: Group);
begin
  Groups.Add(g);
  g.IsPArented:=True;
  g.Init;
end;

procedure ApeEngine.AddMasslessForce(v: Vector);
begin
  MasslessForce.PlusEquals(v);
end;

procedure ApeEngine.CheckCollisions;
var j : integer;
begin
  for j:= 0 to Groups.Count-1 do
    Group(Groups[j]).CheckCollision;
end;

function ApeEngine.GetConstraintCollisionCycle: Integer;
begin
  Result:=_ConstraintCollisionCycle;
end;

function ApeEngine.GetConstraintCycle: Integer;
begin
  Result:=_ConstraintCycles;
end;

function ApeEngine.GetDamping: Double;
begin
  Result:=_Damping;
end;

function ApeEngine.GetNumGroups: Integer;
begin
  result:=Groups.count;
end;

procedure ApeEngine.Init(dt: Double);
begin
  TimeSteps := dt * dt;
  groups := TList.Create;
  force:=Vector.Create(0,0);
  MasslessForce:=Vector.Create(0,0);
  Damping := 1;
  _ConstraintCycles:=0;
  _ConstraintCollisionCycle:=1;
end;

procedure ApeEngine.Integrate;
var j : integer;
begin
  for j:= 0 to Groups.Count-1 do
    Group(Groups[j]).Integrate(TimeSteps,Force,MasslessForce,Damping);
end;

procedure ApeEngine.Paint;
var i : integer;
begin
  for i:=0 to Groups.Count-1 do
    Group(Groups[i]).Paint;
end;

procedure ApeEngine.RemoveGroup(g: Group);
begin
  groups.Remove(g);
  g.IsPArented:=false;
  //g.free; ?
end;

procedure ApeEngine.SatisfyConstraints;
var j : integer;
begin
  for j:= 0 to Groups.Count-1 do
    Group(Groups[j]).SatisfyConstraints;
end;

procedure ApeEngine.SetConstraintCollisionCycle(const Value: Integer);
begin
  _ConstraintCollisionCycle:=Value;
end;

procedure ApeEngine.SetConstraintCycle(const Value: Integer);
begin
  _ConstraintCycles:=Value;
end;

procedure ApeEngine.SetDamping(const Value: Double);
begin
  _Damping:=Value;
end;

procedure ApeEngine.Step;
var i : integer;
begin
  Integrate;
  For i:=0 to _ConstraintCycles -1 do
  begin
    SatisfyConstraints;
  end;

  For i:=0 to _ConstraintCollisionCycle -1 do
  begin
    SatisfyConstraints;
    CheckCollisions;
  end;
end;

end.
