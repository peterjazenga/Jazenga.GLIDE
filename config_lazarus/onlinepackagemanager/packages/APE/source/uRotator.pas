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
unit uRotator;

interface


Uses uGroup, uCircleParticle, uSpringConstraint, uApeEngine,
    uRectangleParticle, uRender, uRectComposite, uVector;

Type

Rotator = class(Group)
Private
  ctr : Vector;
  arectcomposite : RectComposite;
Public
  Constructor Create(Render : AbstractRenderer; aEngine : ApeEngine); reintroduce; Virtual;

  function RotateByRadian( a : Double) : Double;
End;


implementation

{ Rotator }

constructor Rotator.Create(Render: AbstractRenderer; aEngine: ApeEngine);
var circA : CircleParticle;
  rectA, rectB : RectanglePArticle;
  ConnectorA, ConnectorB : SpringConstraint;
begin
  inherited Create;
	collideInternal := true;

  ctr := Vector.Create(555,175);
	arectComposite := RectComposite.Create(Render, aEngine, ctr);
	addComposite(arectComposite);

  circA := CircleParticle.create(ctr.x,ctr.y,5);
	circA.SetRenderer(Render);
	addParticle(circA);

	rectA := RectangleParticle.Create(555,160,10,10,0,false,3);
	rectA.SetRenderer(Render);
	addParticle(rectA);

	connectorA := SpringConstraint.create(arectComposite.CpA, rectA, 1);
	connectorA.SetRenderer(Render);
	addConstraint(connectorA);

	rectB := RectangleParticle.Create(555,190,10,10,0,false,3);
	rectB.SetRenderer(Render);
	addParticle(rectB);

	connectorB := SpringConstraint.Create(arectComposite.cpc, rectB, 1);
	connectorB.SetRenderer(render);
	addConstraint(connectorB);
end;

function Rotator.RotateByRadian(a: Double): Double;
begin
  arectcomposite.RotateByRadian(a,ctr);
end;

end.
