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
unit uCollisionResolver;

Interface

Uses uAbstractParticle, uVector, uCollision, uMathUtil;

Type

CollisionResolver = Class
Public
  Procedure ResolveParticleParticle(pa,pb : AbstractPArticle; normal : Vector; Depth : Double);
  //Function Clamp(input,min,max : Double) : Double; Yet in mathutils !
end;

var CollisionResolverInstance : CollisionResolver;


implementation

{ CollisionResolver }

procedure CollisionResolver.ResolveParticleParticle(pa,
  pb: AbstractPArticle; normal: Vector; Depth: Double);
var mtd,vna,vnb,mtda,mtdb : Vector;
    te,suminvmass,tf : Double;
    ca,cb : Collision;
begin
	// a collision has occured. set the current positions to sample locations
  pa.Curr.Copy(pa.Samp);
  pb.Curr.Copy(pb.Samp);

  mtd:=normal.Mult(depth);
  te:=pa.Elasticity+pb.Elasticity;
  suminvmass:=pa.InvMass+pb.InvMass;

  // the total friction in a collision is combined but clamped to [0,1]
  tf := Clamp(1- (pa.Friction + pb.Friction),0,1);

  // get the collision components, vn and vt
  ca:=pa.GetComponents(normal);
  cb:=pb.GetComponents(normal);

  // calculate the coefficient of restitution based on the mass, as the normal component
  vnA:=(cb.vn.mult((te + 1) * pa.invMass).plus(
     		ca.vn.mult(pb.invMass - te * pa.invMass))).divEquals(sumInvMass);
  vnB:=(ca.vn.mult((te + 1) * pb.invMass).plus(
     		cb.vn.mult(pa.invMass - te * pb.invMass))).divEquals(sumInvMass);

  // apply friction to the tangental component
  ca.vt.multEquals(tf);
  cb.vt.multEquals(tf);

  // scale the mtd by the ratio of the masses. heavier particles move less
  mtdA:= mtd.mult( pa.invMass / sumInvMass);
  mtdB:= mtd.mult(-pb.invMass / sumInvMass);

  // add the tangental component to the normal component for the new velocity
  vnA.plusEquals(ca.vt);
  vnB.plusEquals(cb.vt);

  if not pa.Fixed then
    pa.ResolveCollision(mtdA, vnA, normal, depth, -1, pb);
  if not pb.Fixed then
    pb.resolveCollision(mtdB, vnB, normal, depth,  1, pa);
end;

Initialization
CollisionResolverInstance:=CollisionResolver.Create;

Finalization
CollisionResolverInstance.Free;

end.
