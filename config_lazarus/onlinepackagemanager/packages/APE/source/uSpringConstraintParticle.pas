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
unit uSpringConstraintParticle;

interface

Uses uRectangleParticle, uAbstractParticle, uSpringConstraint, uVector, uMathUtil, uCircleParticle, SysUtils;

Type

SpringConstraintParticle = class(RectangleParticle)
Private

		p1:AbstractParticle;
		p2:AbstractParticle;

		avgVelocity:Vector;
		lambda:Vector;
		parent:SpringConstraint;
		scaleToLength:Boolean;
    s : Double;

		rca:Vector;
		rcb:Vector;

		_rectScale:Double;
		_rectHeight:Double;
		_fixedEndLimit:Double;
Public
    function GetMass: Double; Override;
    function GetElasticity: Double; override;
    function GetFriction: Double; Override;
    function GetVelocity: Vector; Override;
    function GetInvMass: Double; Override;

    Constructor Create( ap1,ap2 : AbstractParticle; p : SpringConstraint;
                        RectHeight, RectScale : Double; aScaleToLength : Boolean); Reintroduce;

    Procedure init; Override;
    Procedure Paint; Override;
    Procedure UpdatePosition;
    Procedure ResolveCollision(mtd,vel,n : vector; d : Double; o : Integer; p : AbstractParticle); Override;
    Function ClosestParamPoint(c : Vector) : Double;
    Function GetContactParamPoint(p : AbstractParticle) : Double;
    Procedure SetCorners(r : RectangleParticle; i : Integer);
    Function ClosestPtSegmentSegment : Double;

    Property RectScale : Double read _rectScale Write _rectScale;
    Property RectHeight : Double Read _rectHeight Write _rectHeight;
    Property FixedEndLimit : Double Read _fixedEndLimit Write _fixedEndLimit;
    property Mass : Double Read GetMass;
    property InvMass : Double Read GetInvMass;
    Property Friction : Double Read GetFriction;
    property Elasticity : Double Read GetElasticity;
    Property Velocity : Vector Read GetVelocity;
End;

implementation

{ SpringConstraintParticle }

function SpringConstraintParticle.ClosestParamPoint(c: Vector): Double;
var ab : Vector;
    t : Double;
begin
  ab := p2.curr.minus(p1.curr);
  t := ab.dot(c.minus(p1.curr)) / (ab.dot(ab));
	Result := Clamp(t, 0, 1);
end;

function SpringConstraintParticle.ClosestPtSegmentSegment: Double;
var pp1,pq1,pp2,pq2 : Vector;
    d1,d2,r : Vector;
    t,a,e,f : Double;
    c,b,denom : Double;
    c1,c2,c1mc2 : Vector;

begin
	pp1 := p1.curr;
	pq1 := p2.curr;
	pp2 := rca;
	pq2 := rcb;

	d1 := pq1.minus(pp1);
	d2 := pq2.minus(pp2);
	r := pp1.minus(pp2);

	a := d1.dot(d1);
	e := d2.dot(d2);
	f := d2.dot(r);

	c := d1.dot(r);
	b := d1.dot(d2);
	denom := a * e - b * b;

	if (denom <> 0.0) then
    s := Clamp((b*f-c*e) / denom,0,1)
  else
  begin
    s := 0.5
  end;
  t:=(b*s+f) / e;

  if (t<0) then
  begin
    t:=0;
    s:= Clamp(-c / a,0,1);
  end
  else
  if (t>0) then
  begin
    t:=1;
    s:=clamp((b-c) / a,0,1)
  end;

  c1 := pp1.plus(d1.mult(s));
  c2 := pp2.plus(d2.mult(t));
  c1mc2 := c1.minus(c2);
	Result := c1mc2.dot(c1mc2);
end;

constructor SpringConstraintParticle.Create( ap1,ap2 : AbstractParticle; p : SpringConstraint;
                        RectHeight, RectScale : Double; aScaleToLength : Boolean);
begin
  inherited Create(0,0,0,0,0,False,0.1,0,0);
  Self.p1 := ap1;
  Self.p2 := ap2;
  lambda := Vector.Create(0,0);
  avgVelocity := Vector.Create(0,0);

  Parent := p;

  Self.RectScale := RectScale;
  Self.RectHeight := RectHeight;
  Self.ScaleToLength := aScaleToLength;
  FixedEndLimit := 0;

  rca := Vector.Create;
  rcb := Vector.Create;
end;

//* returns the average friction of the two connected particles
function SpringConstraintParticle.GetContactParamPoint(
  p: AbstractParticle): Double;
var t,d : Double;
    ShortestIndex : Integer;
    ShortestDistance : Double;
    ParamList : array[0..3] of Double;
    i : integer;
begin
	if (p is CircleParticle) then
  begin
    t := closestParamPoint(p.curr);
  end
  else
  if (p is RectangleParticle) then
  begin
    shortestDistance := High(Integer);
  	// go through the sides of the colliding rectangle as line segments
		for i:=0 to 3 do
    begin
      shortestIndex:=0; //Because compil warning.
  		setCorners(RectangleParticle(p), i);
      // check for closest points on SCP to side of rectangle
			d := closestPtSegmentSegment;
			if (d < shortestDistance) then
      begin
				shortestDistance := d;
				shortestIndex := i;
				paramList[i] := s;
      end;
    end;
  	t := paramList[shortestIndex];
  end
  else
  begin
    raise Exception.Create('SpringConstraintParticle.GetContactParamPoint : Unknown Particle Type');
  end;
	Result :=  t;
end;

function SpringConstraintParticle.GetElasticity: Double;
begin
  Result := (p1.Elasticity + p2.Elasticity) / 2;
end;

//* returns the average friction of the two connected particles
function SpringConstraintParticle.GetFriction: Double;
begin
  Result := (p1.Friction + p2.Friction) / 2;
end;

//* returns the average mass of the two connected particles
function SpringConstraintParticle.GetInvMass: Double;
begin
  if (p1.Fixed And p2.Fixed) then
    Result:=0
  else
    Result:= 1 / ((p1.Mass+p2.Mass) /2)
end;

function SpringConstraintParticle.GetMass: Double;
begin
  Result := (p1.Mass + p2.Mass) / 2;
end;

//* returns the average velocity of the two connected particles
function SpringConstraintParticle.GetVelocity: Vector;
var p1v, p2v : Vector;
begin
  p1v := p1.Velocity;
  p2v := p2.Velocity;

  avgVelocity.SetTo(((p1v.x + p2v.x) / 2), ((p1v.y + p2v.y) / 2));

  Result := avgVelocity;
end;

procedure SpringConstraintParticle.init;
begin
  inherited;

end;

procedure SpringConstraintParticle.Paint;
var w,h : Double;
    c : Vector;
begin
  //inherited;
  c := Parent.center;
  w := parent.CurrLength * RectScale;
  h := RectHeight;
  parent.aRenderer.Rectangle(c.x,c.y,w,h,Parent.angle);
  //parent.aRenderer.Text(c.x,c.y,FloatToStr(Parent.angle));
end;

procedure SpringConstraintParticle.ResolveCollision(mtd, vel, n: vector;
  d: Double; o: Integer; p: AbstractParticle);
var t,c1,c2 : Double;
    Denom : Double;
    corrParticle : AbstractParticle;
begin
  inherited;

  t := GetContactParamPoint(p);
  c1 := 1-t;
  c2 := t;

  // if one is fixed then move the other particle the entire way out of collision.
	// also, dispose of collisions at the sides of the scp. The higher the fixedEndLimit
	// value, the more of the scp not be effected by collision.
  if (p1.fixed) Then
  begin
		if (c2 <= fixedEndLimit) then
      Exit;

		lambda.setTo(mtd.x / c2, mtd.y / c2);
		p2.curr.plusEquals(lambda);
		p2.velocity := vel;

	End
  else
  if (p2.fixed) then
  begin
  	if (c1 <= fixedEndLimit) then
      Exit;
  	lambda.setTo(mtd.x / c1, mtd.y / c1);
		p1.curr.plusEquals(lambda);
		p1.velocity := vel;
  end
  else
  begin
		// else both non fixed - move proportionally out of collision
    denom := c1 * c1 + c2 * c2;
		if (denom = 0) Then
      Exit;
		lambda.setTo(mtd.x / denom, mtd.y / denom);

		p1.curr.plusEquals(lambda.mult(c1));
		p2.curr.plusEquals(lambda.mult(c2));

    //if collision is in the middle of SCP set the velocity of both end particles
    if (t = 0.5) then
    begin
		  p1.velocity := vel;
			p2.velocity := vel;
    end
		else
    begin
  		// otherwise change the velocity of the particle closest to contact
      If t<0.5 then
        corrParticle := p1
      else
        corrParticle := p2;

       corrParticle.Velocity := Vel;
    end;
  end;
end;

procedure SpringConstraintParticle.SetCorners(r: RectangleParticle;
  i: Integer);
var rx,ry,ae0_x,ae0_y,ae1_x,ae1_y,emx,emy, epx,epy : Double;
    xaxes : TRctTypeVector;
    xExtents : TRctTypeDouble;
begin
  rx := r.curr.x;
  ry := r.curr.y;

  xaxes := r.axes;
  xextents := r.extents;

  ae0_x := xaxes[0].x * xextents[0];
  ae0_y  := xaxes[0].y * xextents[0];
  ae1_x  := xaxes[1].x * xextents[1];
  ae1_y  := xaxes[1].y * xextents[1];

  emx := ae0_x - ae1_x;
  emy := ae0_y - ae1_y;
  epx := ae0_x + ae1_x;
  epy := ae0_y + ae1_y;

  if (i = 0) then
  begin	// 0 and 1
		rca.x := rx - epx;
		rca.y := ry - epy;
		rcb.x := rx + emx;
		rcb.y := ry + emy;
  end
  else
  if (i = 1) Then
  Begin	// 1 and 2
		rca.x := rx + emx;
		rca.y := ry + emy;
		rcb.x := rx + epx;
		rcb.y := ry + epy;
  end
  else
  if (i = 2) Then
  begin	// 2 and 3
		rca.x := rx + epx;
		rca.y := ry + epy;
		rcb.x := rx - emx;
		rcb.y := ry - emy;
	end
  else
  if (i = 3) then
  begin	// 3 and 0
		rca.x := rx - emx;
		rca.y := ry - emy;
		rcb.x := rx - epx;
		rcb.y := ry - epy;
  end;
end;

procedure SpringConstraintParticle.UpdatePosition;
var c : Vector;
begin
  c := parent.center;
	curr.setTo(c.x, c.y);

  if scaleToLength then
    Width := Parent.CurrLength * RectScale
  else
    Width := parent.RestLength * RectScale;

  Height := RectHeight;
  Radian := Parent.Radian;
end;

end.
