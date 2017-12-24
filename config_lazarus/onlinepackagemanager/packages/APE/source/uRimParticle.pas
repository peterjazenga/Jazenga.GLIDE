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
unit uRimParticle;

interface

Uses uVector, MAth, uApeEngine;

type

RimPArticle = class(TObject)
Private
		wr : Double;
		av : Double ;
		sp : Double;
		maxTorque : Double;
    FApeEngine : ApeEngine;
Public
  	curr : Vector;
		prev : Vector;

    Constructor Create(anApeEngine : ApeEngine; Ar : Double; Mt : Double);
    Procedure Update(dt : Double);

    Property Speed : Double read sp Write sp;
    property AngularVelocity : Double read av Write av;
End;

implementation

{ RimPArticle }

constructor RimPArticle.Create(anApeEngine : ApeEngine; Ar : Double; Mt : Double);
begin
  Curr := Vector.Create(ar,0);
  prev := Vector.Create(0,0);
  sp := 0;
  av := 0;
  FApeEngine := anApeEngine;

  maxTorque := mt;
  wr := ar;
end;

procedure RimParticle.Update(dt: Double);
var dx, dy, Len, ox, oy, px, py, clen, diff : Double;
begin
  //Origins of this code are from Raigan Burns, Metanet Software
  //Clamp torques to valid range
  sp := max(-maxTorque, min(maxTorque, sp + av));

  //Apply torque
	//This is the tangent vector at the rim particle
	dx := -curr.y;
	dy := curr.x;

	//Normalize so we can scale by the rotational speed
	len := sqrt(dx * dx + dy * dy);

  If Len <>0 then
  begin
	  dx := dx/len;
	  dy := dy/len;
  end;

	curr.x := curr.x + sp * dx;
	curr.y := curr.y + sp * dy;

	ox := prev.x;
	oy := prev.y;
  prev.x := curr.x;
  prev.y := curr.y;
	px := prev.x;
	py := prev.y;

	curr.x := FApeEngine.damping * (px - ox);
	curr.y := FApeEngine.damping * (py - oy);

	// hold the rim particle in place
	clen := sqrt(curr.x * curr.x + curr.y * curr.y);
  if clen<>0 then
  Begin
	  diff := (clen - wr) / clen;
	  curr.x := curr.x - curr.x * diff;
	  curr.y := curr.y - curr.y * diff;
  end;
end;

end.
