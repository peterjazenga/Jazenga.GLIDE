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
unit uSpringConstraint;

interface

uses uAbstractConstraint, uAbstractParticle, uMathUtil, uVector, MAth, SysUtils;

Type
SpringConstraint = Class(AbstractConstraint)
Private
	p1:AbstractParticle;
	p2:AbstractParticle;

	_restLength : Double;
	_collidable : Boolean;
	_scp : AbstractParticle;

  function GetCurrLenght: Double;
  function GetCenter: Vector;
    function GetAngle: Double;
    function GetRadian: Double;
    function GetRectHeight: Double;
    function GetRectScale: Double;
    function GetDelta: Vector;
    procedure SetRestLength(const Value: Double);
    function GetFixedEndLimit: Double;
    procedure SetFixedEndLimit(const Value: Double);
    function GetFixed: Boolean;
Public
  constructor Create( ap1,ap2 : AbstractParticle; aStiffness : Double = 0.5; Collidable : Boolean = False;
                    rectHeight : Double = 1; rectScale : Double = 1; ScaleToLEngth : Boolean = False);

  Procedure checkParticlesLocation;
  Procedure Resolve; OVerride;
  procedure SetCollidable(b : boolean; aRectHeight, aRectScale : Double; ScaleLen : Boolean = false);
  Function IsConnectedTo(p : AbstractParticle) : Boolean;

  Procedure Paint; OverridE;

  Property CurrLength : Double read GetCurrLenght;
  property Center : Vector read GetCenter;
  property Angle : Double read GetAngle;
  property Radian : Double Read GetRadian;
  property RectScale : Double read GetRectScale;
  Property RectHeight : Double Read GetRectHEight;
  property Delta : Vector read GetDelta;
  property RestLength : Double read _restLength Write SetRestLength;
  property Collidable : Boolean read _collidable Write _collidable;
  property SCP : AbstractParticle read _scp;
  property FixedEndLimit : Double read GetFixedEndLimit Write SetFixedEndLimit;
  PRoperty Fixed : Boolean read GetFixed; 

end;


implementation

uses uSpringConstraintParticle, uAbstractItem;

{ SpringConstraint }


//* if the two particles are at the same location offset slightly
procedure SpringConstraint.checkParticlesLocation;
begin
  if ((p1.curr.x = p2.curr.x) and (p1.curr.y = p2.curr.y)) Then
	  p2.curr.x := p2.curr.x + 0.0001;
end;

constructor SpringConstraint.Create( ap1,ap2 : AbstractParticle; aStiffness : Double = 0.5; Collidable : Boolean = False;
                    rectHeight : Double = 1; rectScale : Double = 1; ScaleToLEngth : Boolean = False);
begin
  inherited Create(aStiffness);
  Self.p1 := ap1;
  Self.p2 := ap2;
  checkParticlesLocation;
  _restLength := CurrLength;
  SetCollidable(Collidable,rectHeight,rectScale,ScaleToLength);
end;

function SpringConstraint.GetAngle: Double;
begin
  Result := radian * ONE_EIGHTY_OVER_PI;
end;

function SpringConstraint.GetCenter: Vector;
begin
  Result :=(p1.curr.plus(p2.curr)).divEquals(2);
end;

function SpringConstraint.GetCurrLenght: Double;
begin
  result := p1.Curr.Distance(p2.Curr);
end;


function SpringConstraint.GetDelta: Vector;
begin
  Result := p1.curr.minus(p2.curr);
end;

function SpringConstraint.GetFixed: Boolean;
begin
  Result := p1.Fixed And p2.Fixed;
end;

function SpringConstraint.GetFixedEndLimit: Double;
begin
  Result := SpringConstraintParticle(_scp).FixedEndLimit;
end;

function SpringConstraint.GetRadian: Double;
var d : Vector;
begin
  d := Delta;
	Result := Arctan2(d.y, d.x);
end;

function SpringConstraint.GetRectHeight: Double;
begin
  Result := SpringConstraintPArticle(_scp).RectHeight;
end;

function SpringConstraint.GetRectScale: Double;
begin
  Result := SpringConstraintPArticle(_scp).RectScale;
end;

function SpringConstraint.IsConnectedTo(p: AbstractParticle): Boolean;
begin
  Result := (p = p1) Or (p=p2);
end;

procedure SpringConstraint.Paint;
begin
//  inherited;
  if Collidable then
  begin
    //aRenderer.Line(p1.px,p1.py,p2.px,p2.py);
    SpringConstraintParticle(_scp).Paint
  end
  else
    aRenderer.Line(p1.px,p1.py,p2.px,p2.py);
end;

procedure SpringConstraint.Resolve;
var DeltaLength : Double;
    Diff : Double;
    dmds : Vector;
begin
  if p1.Fixed and p2.Fixed then
    Exit;

  deltaLength := currLength;
  diff := (deltaLength - restLength) / (deltaLength * (p1.invMass + p2.invMass));
	dmds := delta.mult(diff * stiffness);

  p1.curr.minusEquals(dmds.mult(p1.invMass));
	p2.curr.plusEquals (dmds.mult(p2.invMass));
end;

procedure SpringConstraint.SetCollidable(b: boolean; aRectHeight,
  aRectScale: Double; ScaleLen: Boolean);
begin
  _collidable := b;
  _scp := nil;
  if _collidable then
  begin
    _scp := SpringConstraintPArticle.Create(p1, p2, self, arectHeight, aRectScale, ScaleLen);
  end;
end;

procedure SpringConstraint.SetFixedEndLimit(const Value: Double);
begin
  if Assigned(_scp) then
    SpringConstraintParticle(_scp).FixedEndLimit := Value;
end;

procedure SpringConstraint.SetRestLength(const Value: Double);
begin
  if Value<=0 then
    raise Exception.Create('SpringConstant.RestLength must be grater than 0');
  _RestLength := Value;
end;

end.
