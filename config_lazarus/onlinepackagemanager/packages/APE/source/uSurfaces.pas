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
unit uSurfaces;

interface

Uses uGroup, uRectangleParticle, uCircleParticle, uSpringConstraint, uApeEngine, uRender;

Type

Surfaces = class(Group)
Private
Public
  Constructor Create(Render : AbstractRenderer; aEngine : ApeEngine); reintroduce; Virtual;
End;

implementation

{ Surfaces }

constructor Surfaces.Create(Render: AbstractRenderer; aEngine: ApeEngine);
var Floor, Ceil, RampRight, RampLeft, rampLeft2, BouncePad : RectangleParticle;
    RampCircle, FloorBump : CircleParticle;
    leftWall, leftWallChannelInner, leftWallChannel, leftWallChannelAng,
    topLeftAng, rightWall, bridgeStart,bridgeEnd:RectangleParticle;

begin
  Inherited Create(False);
  floor := RectangleParticle.Create(340,327,550,50,0,true);
  Floor.SetRenderer(Render);
  addParticle(floor);

  ceil := RectangleParticle.Create(325,-33,649,80,0,true);
  ceil.SetRenderer(Render);
  addParticle(ceil);

  rampRight := RectangleParticle.Create(375,220,390,20,0.405,true);
  rampRight.SetRenderer(Render);
  addParticle(rampRight);

  rampLeft := RectangleParticle.Create(90,200,102,20,-0.7,true);
  rampLeft.SetRenderer(Render);
  addParticle(rampLeft);

  rampLeft2 := RectangleParticle.Create(96,129,102,20,-0.7,true);
  rampLeft2.SetRenderer(Render);
  addParticle(rampLeft2);

  rampCircle := CircleParticle.Create(175,190,60,true);
  rampCircle.SetRenderer(Render);
  addParticle(rampCircle);

  floorBump := CircleParticle.Create(600,660,400,true);
  floorBump.SetRenderer(Render);
  addParticle(floorBump);

  bouncePad := RectangleParticle.Create(35,370,40,60,0,true);
  bouncePad.SetRenderer(Render);
  bouncePad.elasticity := 4;
  addParticle(bouncePad);

  leftWall := RectangleParticle.Create(1,99,30,500,0,true);
  leftWall.SetRenderer(Render);
  addParticle(leftWall);

  leftWallChannelInner := RectangleParticle.Create(54,300,20,150,0,true);
  leftWallChannelInner.SetRenderer(Render);
  addParticle(leftWallChannelInner);

  leftWallChannel := RectangleParticle.Create(54,122,20,94,0,true);
  leftWallChannel.SetRenderer(Render);
  addParticle(leftWallChannel);

  leftWallChannelAng := RectangleParticle.Create(75,65,60,25,- 0.7,true);
  leftWallChannelAng.SetRenderer(Render);
  addParticle(leftWallChannelAng);

  topLeftAng := RectangleParticle.Create(23,11,65,40,-0.7,true);
  topLeftAng.SetRenderer(Render);
  addParticle(topLeftAng);

  rightWall := RectangleParticle.Create(654,230,50,500,0,true);
  rightWall.SetRenderer(Render);
  addParticle(rightWall);

  bridgeStart := RectangleParticle.Create(127,49,75,25,0,true);
  bridgeStart.SetRenderer(Render);
  addParticle(bridgeStart);

  bridgeEnd := RectangleParticle.Create(483,55,100,15,0,true);
  bridgeEnd.SetRenderer(Render);
  addParticle(bridgeEnd);
end;

end.
