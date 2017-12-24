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
unit uCollisionDetector;

interface

Uses uAbstractParticle, uVector, uRectangleParticle, uCircleParticle, uInterval, uCollisionResolver;

Type

CollisionDetector = Class
Public

  Procedure Test(ObjA, ObjB : AbstractPArticle);
  Procedure NormVsNorm(ObjA,OBjB : AbstractPArticle);
  Procedure SampVsNorm(ObjA,OBjB : AbstractPArticle);
  Procedure SampVsSamp(ObjA,OBjB : AbstractPArticle);
  Function TestTypes(ObjA,OBjB : AbstractPArticle) : Boolean;
  Function TestOBBvsOBB(rA,rB : RectanglePArticle) : Boolean;
  Function TestCirclevsCircle(ca, cb : CircleParticle) : Boolean;
  Function TestOBBvsCircle(ra : RectangleParticle; ca : CircleParticle) : Boolean;

  Function TestIntervals(IntervalA, IntervalB : Interval) : Double;
  Function ClosestVertexOnOBB(p : Vector; r : RectanglePArticle) : Vector;

end;

var CollisionDetectorInstance : CollisionDetector;

implementation

{ CollisionDetector }

function CollisionDetector.ClosestVertexOnOBB(p: Vector;
  r: RectanglePArticle): Vector;
var d,q : Vector;
    i : integer;
    dist : Double;
begin
  d:=p.Minus(r.Samp);
  q:=vector.Create(r.Samp.x,r.Samp.y);
  for i:=0 to 1 do
  begin
    dist := d.Dot(r.axes[i]);
    if dist>=0 then
      dist := r.extents[i]
    else
      dist := - r.extents[i];

    q.PlusEquals(r.axes[i].Mult(dist));
  end;
  result:=q;
end;


procedure CollisionDetector.NormVsNorm(ObjA, OBjB: AbstractPArticle);
begin
  obja.Samp.Copy(obja.Curr);
  objb.Samp.Copy(objb.Curr);
  TestTypes(obja,objb);
end;

procedure CollisionDetector.SampVsNorm(ObjA, OBjB: AbstractPArticle);
var s,t : Double;
    i : integer;
begin
  s:=1/ (ObjA.MultiSample+1);
  t :=s;
  objb.Samp.Copy(objb.Curr);

  for i:=0 to ObjA.MultiSample-1 do
  begin
    ObjA.Samp.SetTo( Obja.Prev.x + t * (ObjA.Curr.x - obja.Prev.x),
                     Obja.Prev.y + t * (ObjA.Curr.y - obja.Prev.y));
    if TestTypes(obja,objb) then
      Exit;
    t:=t+s;
  end;
end;

procedure CollisionDetector.SampVsSamp(ObjA, OBjB: AbstractPArticle);
var s,t : Double;
    i : integer;
begin
  s:=1/ (ObjA.MultiSample+1);
  t :=s;
  objb.Samp.Copy(objb.Curr);

  for i:=0 to ObjA.MultiSample-1 do
  begin
    ObjA.Samp.SetTo( Obja.Prev.x + t * (ObjA.Curr.x - obja.Prev.x),
                     Obja.Prev.y + t * (ObjA.Curr.y - obja.Prev.y));
    Objb.Samp.SetTo( Objb.Prev.x + t * (Objb.Curr.x - objb.Prev.x),
                     Objb.Prev.y + t * (Objb.Curr.y - objb.Prev.y));
    if TestTypes(obja,objb) then
      Exit;
    t:=t+s;
  end;
end;

procedure CollisionDetector.Test(ObjA, ObjB: AbstractPArticle);
begin
  if OBja.Fixed And Objb.Fixed then
    Exit;

  if (ObjA.MultiSample = 0) And (ObjB.MultiSample = 0) then
    NormVsNorm(obja,objb)
  else
  if (ObjA.MultiSample > 0) And (ObjB.MultiSample = 0) then
    SampVsNorm(Obja,Objb)
  else
  if (ObjB.MultiSample > 0) And (ObjA.MultiSample = 0) then
    SampVsNorm(Objb,Obja)
  else
  if Obja.MultiSample=objb.MultiSample then
    SampVsSamp(obja,objb)
  else
    NormVsNorm(obja,objb);

end;

function CollisionDetector.TestCirclevsCircle(ca,
  cb: CircleParticle): Boolean;
var DepthX, DepthY : Double;
    CollisionNormal : Vector;
    CollisionDepth : Double;
    Mag : Double;
begin
  Result := False;

  DepthX := TestIntervals(ca.GetIntervalX,cb.GetIntervalX);
  if DepthX = 0 then
  begin
    Exit;
  end;

   DepthY := TestIntervals(ca.GetIntervalY, cb.GetIntervalY);
   if DepthY = 0 then
   begin
     Exit;
   end;

   CollisionNormal := ca.Samp.Minus(cb.Samp);
   mag := CollisionNormal.Magnitude;
   CollisionDepth := (ca.Radius + cb.Radius) - mag;

   if CollisionDepth>0 then
   begin
     CollisionNormal.DivEquals(mag);
     CollisionResolverInstance.ResolveParticleParticle(ca,cb,CollisionNormal,CollisionDepth);
     Result := true;
   end;
end;

function CollisionDetector.TestIntervals(IntervalA,
  IntervalB: Interval): Double;
var lena,lenB : Double;
begin
  if IntervalA.Max<IntervalB.Min then
    Result:=0
  else
    if IntervalB.Max<IntervalA.Min then
      Result:=0
    else
    begin
      lenA:=IntervalB.max-IntervalA.min;
      lenb:=IntervalB.min-IntervalA.max;
      result:=lenB;
      if abs(lenA) < abs(lenb) then
        Result:=lenA;
    end;
end;

function CollisionDetector.TestOBBvsCircle(ra: RectangleParticle;
  ca: CircleParticle): Boolean;
var CollisionNormal : Vector;
    CollisionDepth : Double;
    Depths : Array[0..1] of Double;
    i : Integer;
    BoxAxis : Vector;
    Depth : Double;
    r : Double;
    Vertex : Vector;
    mag : Double;
begin

  CollisionDepth := High(Integer);
  CollisionNormal := ra.Axes[0]; //Initialisation

  // first go through the axes of the rectangle
  for i:= 0 to 1 do
  begin
    BoxAxis:=ra.axes[i];
    Depth := TestIntervals(ra.GetProjection(BoxAxis), ca.GetProjection(BoxAxis));

    If Depth = 0 then
    begin
      Result := False;
      Exit;
    end;

    if Abs(depth) < Abs(CollisionDepth) then
    begin
      CollisionNormal:= BoxAxis;
      CollisionDepth := Depth;
    end;

    Depths[i]:=Depth;
  end;

  // determine if the circle's center is in a vertex region
  r := ca.Radius;

  if (abs(Depths[0]) < r) And (Abs(Depths[1])<r) Then
  begin
    Vertex := ClosestVertexOnOBB(ca.Samp,ra);
    // get the distance from the closest vertex on rect to circle center
    CollisionNormal := Vertex.Minus(ca.Samp);
    mag := CollisionNormal.Magnitude;
    CollisionDepth := r - mag;

    If CollisionDepth > 0 then
    begin
			// there is a collision in one of the vertex regions
      CollisionNormal.DivEquals(mag);
    end
    else
    begin
      Result := False;
      Exit;
    end;
  end;

  CollisionResolverInstance.resolveParticleParticle(ra, ca, collisionNormal, collisionDepth);
  Result :=True;

end;

function CollisionDetector.TestOBBvsOBB(rA,
  rB: RectanglePArticle): Boolean;
var CollisionNormal : Vector;
    CollisionDepth : Double;
    i : integer;
    axisA, Axisb : Vector;
    absA,AbsB : Double;
    DepthA,DepthB : Double;
    altb : Boolean;
begin
  CollisionDepth := High(Integer);
  CollisionNormal := ra.Axes[0]; //Initialisation

  For i:=0 to 1 do
  begin
    axisA:=ra.axes[i];
    depthA:=TestIntervals(ra.GetProjection(AxisA),rb.GetProjection(axisA));
    if DepthA = 0 then
    begin
      Result:=False;
      Exit;
    end;

    axisB:=rb.axes[i];
    DepthB:=TestIntervals(ra.GetProjection(AxisB),rb.GetProjection(axisB));
    if DepthB = 0 then
    begin
      Result:=False;
      Exit;
    end;

    absa:=abs(deptha);
    absb:=abs(depthb);

    if (absa < abs(CollisionDepth)) or (absb < abs(CollisionDepth)) then
    begin
      altb:=absa<absb;
      CollisionNormal:=axisB;
      CollisionDepth:=depthB;

      if altb then
      begin
        CollisionNormal:=axisA;
        CollisionDepth:=DepthA;
      end;
    end;
  end;

  CollisionResolverInstance.ResolveParticleParticle(ra,rB,CollisionNormal,CollisionDepth);
  Result:=true;
end;

function CollisionDetector.TestTypes(ObjA,
  OBjB: AbstractPArticle): Boolean;
begin
  Result:=False;
  if (ObjA is RectangleParticle) And (Objb is RectangleParticle) then
    Result := TestOBBvsOBB(RectangleParticle(obja),RectangleParticle(objb))
  else
  if (ObjA is CircleParticle) And (OBjB is CircleParticle) then
    Result := TestCirclevsCircle(CircleParticle(ObjA),CircleParticle(OBjB))
  else
  if (ObjA is RectangleParticle) And (OBjB is CircleParticle) then
    Result := TestOBBvsCircle(RectangleParticle(ObjA),CircleParticle(OBjB))
  else
  if (ObjA is CircleParticle) And (OBjB is RectangleParticle) then
    Result := TestOBBvsCircle(RectangleParticle(ObjB),CircleParticle(OBjA));
end;

Initialization
CollisionDetectorInstance:=CollisionDetector.Create;


Finalization
CollisionDetectorInstance.Free;



end.
