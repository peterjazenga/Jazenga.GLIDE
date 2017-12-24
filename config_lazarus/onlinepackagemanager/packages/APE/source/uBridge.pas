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
unit uBridge;

interface

Uses uGroup, uCircleParticle, uSpringConstraint, uApeEngine, uRender;

Type

Bridge = class(Group)
Private
Public
  Constructor Create(Render : AbstractRenderer; aEngine : ApeEngine); reintroduce; Virtual;
End;

implementation



{ Bridge }

constructor Bridge.Create(Render : AbstractRenderer; aEngine : ApeEngine);
var bx : Double;
    By : Double;
    bsize : Double;
    yslope : Double;
    ParticleSize : Double;

    bridgePAA, bridgePA, bridgePB, bridgePC, bridgePD, bridgePDD : CircleParticle;
    bridgeConnA, bridgeConnB, bridgeConnC, bridgeConnD, bridgeConnE : SpringConstraint;
begin
  inherited Create(False);
  bx := 170;
  By := 40;
  bsize := 51.5;
  yslope := 2.4;
  ParticleSize := 4;

	bridgePAA:= CircleParticle.Create(bx,by,particleSize,true);
	addParticle(bridgePAA);

	bx := bx + bsize;
	by := By + yslope;

	bridgePA := CircleParticle.Create(bx,by,particleSize);
	addParticle(bridgePA);

	bx := bx + bsize;
	by := By + yslope;
	bridgePB := CircleParticle.Create(bx,by,particleSize);
	addParticle(bridgePB);

	bx := bx + bsize;
	by := By + yslope;
	bridgePC := CircleParticle.Create(bx,by,particleSize);
	addParticle(bridgePC);

	bx := bx + bsize;
	by := By + yslope;
  bridgePD := CircleParticle.Create(bx,by,particleSize);
	addParticle(bridgePD);

	bx := bx + bsize;
	by := By + yslope;
	bridgePDD := CircleParticle.Create(bx,by,particleSize,true);
	addParticle(bridgePDD);


  bridgeConnA := SpringConstraint.Create(bridgePAA, bridgePA, 0.9, true, 10, 0.8);

	// collision response on the bridgeConnA will be ignored on
	// on the first 1/4 of the constraint. this avoids blow ups
	// particular to springcontraints that have 1 fixed particle.
	bridgeConnA.FixedEndLimit := 0.25;
	addConstraint(bridgeConnA);

	bridgeConnB := SpringConstraint.Create(bridgePA, bridgePB,0.9, true, 10, 0.8);
	addConstraint(bridgeConnB);

  bridgeConnC := SpringConstraint.Create(bridgePB, bridgePC,0.9, true, 10, 0.8);
	addConstraint(bridgeConnC);

  bridgeConnD := SpringConstraint.Create(bridgePC, bridgePD,	0.9, true, 10, 0.8);
	addConstraint(bridgeConnD);

  bridgeConnE := SpringConstraint.Create(bridgePD, bridgePDD,	0.9, true, 10, 0.8);
	bridgeConnE.fixedEndLimit := 0.25;
  addConstraint(bridgeConnE);

  bridgePAA.SetRenderer(Render);
  bridgePA.SetRenderer(Render);
  bridgePB.SetRenderer(Render);
  bridgePC.SetRenderer(Render);
  bridgePD.SetRenderer(Render);
  bridgePDD.SetRenderer(Render);

  bridgeConnA.SetRenderer(Render);
  bridgeConnB.SetRenderer(Render);
  bridgeConnC.SetRenderer(Render);
  bridgeConnD.SetRenderer(Render);
  bridgeConnE.SetRenderer(Render);


end;

end.
