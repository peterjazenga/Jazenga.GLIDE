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
unit ucar;

interface

Uses uGroup, uWheelParticle, uSpringConstraint, uApeEngine, uRender;

type

Car = class(Group)
private
    wheelparticleA : WheelParticle;
    wheelparticleB : WheelParticle;
    wheelconnector : SpringConstraint;

    function GetSpeed: Double;
    procedure SetSpeed(const Value: Double);

public
   Constructor Create(aRenderer : AbstractRenderer; anApeEngine : ApeEngine); Reintroduce;

   property Speed : Double read GetSpeed Write SetSpeed;

end;

implementation

uses uAbstractCollection;

{ Car }

constructor Car.Create(aRenderer : AbstractRenderer; anApeEngine : ApeEngine);
begin
  inherited Create(True);
  wheelparticleA := WheelParticle.Create(anApeEngine,140,10,14,False,2);
  wheelparticleA.SetRenderer(aRenderer);
  wheelparticleB := WheelParticle.Create(anApeEngine,200,10,14,False,2);
  wheelparticleB.SetRenderer(aRenderer);
  wheelconnector := SpringConstraint.Create(wheelparticleA,wheelparticleB,0.5,True,8);
  wheelconnector.SetRenderer(aRenderer);

  AddParticle(wheelparticleA);
  AddParticle(wheelparticleB);
  AddConstraint(wheelconnector);
end;

function Car.GetSpeed: Double;
begin
  result :=(wheelparticleA.AngularVelocity + wheelparticleB.AngularVelocity) / 2;
end;

procedure Car.SetSpeed(const Value: Double);
begin
  wheelparticleA.AngularVelocity:=Value;
  wheelparticleb.AngularVelocity:=Value;
end;

end.
