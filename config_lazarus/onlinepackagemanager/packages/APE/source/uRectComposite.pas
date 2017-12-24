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
unit uRectComposite;

interface


Uses uGroup, uCircleParticle, uSpringConstraint, uApeEngine, uRender, uComposite, uVector;

Type

RectCOmposite = class(Composite)
Private
Public
  CpA : CircleParticle;
  CpC : CircleParticle;

  Constructor Create(Render : AbstractRenderer; aEngine : ApeEngine; Ctr : Vector); reintroduce; Virtual;
End;


implementation

{ RectCOmposite }

constructor RectComposite.Create(Render: AbstractRenderer;
  aEngine: ApeEngine; Ctr: Vector);
var rw, rh, rad : Double;
    cpb, cpd : CircleParticle;
    spra, sprb, sprc, sprd : SpringConstraint;
begin
  Inherited Create;
  // just hard coding here for the purposes of the demo, you should pass
  // everything in the constructor to do it right.
  rw := 75;
  rh := 18;
  rad := 4;

  // going clockwise from left top..
  cpA := CircleParticle.Create(ctr.x-rw/2, ctr.y-rh/2, rad, true);
  cpB := CircleParticle.Create(ctr.x+rw/2, ctr.y-rh/2, rad, true);
  cpC := CircleParticle.Create(ctr.x+rw/2, ctr.y+rh/2, rad, true);
  cpD := CircleParticle.Create(ctr.x-rw/2, ctr.y+rh/2, rad, true);

  cpA.SetRenderer(Render);
  cpB.SetRenderer(Render);
  cpC.SetRenderer(Render);
  cpD.SetRenderer(Render);

  sprA := SpringConstraint.Create(cpA,cpB,0.5,true,rad * 2);
  sprB := SpringConstraint.Create(cpB,cpC,0.5,true,rad * 2);
  sprC := SpringConstraint.Create(cpC,cpD,0.5,true,rad * 2);
  sprD := SpringConstraint.Create(cpD,cpA,0.5,true,rad * 2);

  sprA.SetRenderer(Render);
  sprB.SetRenderer(Render);
  sprC.SetRenderer(Render);
  sprD.SetRenderer(Render);

  addParticle(cpA);
  addParticle(cpB);
  addParticle(cpC);
  addParticle(cpD);

  addConstraint(sprA);
  addConstraint(sprB);
  addConstraint(sprC);
  addConstraint(sprD);
end;

end.
