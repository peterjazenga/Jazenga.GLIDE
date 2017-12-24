unit uRender;

interface

uses uVector;

Type

AbstractRenderer = Class
private
public
  Procedure Rectangle(xcenter,ycenter,Width, height, Rotate : Double);
  Procedure Circle(xcenter,ycenter,Radius, Rotate : Double); Virtual; Abstract;
  Procedure Line(x,y,xx,yy : Double); virtual; abstract;
  Procedure Text(x,y : Double; Text : String); Virtual; Abstract;
end;

Tracer = class
  private
    FAngle: Double;
    procedure Setangle(const Value: Double);
public
  posx,posy : Double;
  dir : Vector;

  constructor create(startX,StartY : double);

  procedure stepby(value : Double);

  property Angle : Double read FAngle write Setangle;
end;

implementation

uses Math;

{ AbstractRenderer }

procedure AbstractRenderer.Rectangle(xcenter, ycenter, Width, height,
  Rotate: Double);
var a : Tracer;
  x1,y1,x2,y2,x3,y3,x4,y4 : Double;
begin

  a:=Tracer.create(xcenter,ycenter);
  a.Angle:=DegToRad(Rotate+180);
  a.stepby(width/2);
  a.Angle:=DegToRad(Rotate+90);
  a.stepby(height/2);

  x1:=a.posx;
  y1:=a.posy;

  a.Angle:=DegToRad(Rotate);
  a.stepby(width);
  x2:=a.posx;
  y2:=a.posy;

  a.Angle:=DegToRad(Rotate-90);
  a.stepby(height);
  x3:=a.posx;
  y3:=a.posy;

  a.Angle:=DegToRad(Rotate+180);
  a.stepby(width);
  x4:=a.posx;
  y4:=a.posy;

  Line(x1,y1,x2,y2);
  Line(x2,y2,x3,y3);
  Line(x3,y3,x4,y4);
  Line(x4,y4,x1,y1);

end;

{ Tracer }

constructor Tracer.create(startX, StartY: double);
begin
  posx:=startX;
  posy:=StartY;
  dir:=Vector.Create(1,0);
end;

procedure Tracer.Setangle(const Value: Double);
begin
  FAngle := Value;
  dir.ResetAngle;
  dir.TurnAngle(Value);
end;

procedure Tracer.stepby(value: Double);
var a : Double;
begin
  a:=FAngle;
  dir.x:=value;
  dir.y:=0;
  dir.TurnAngle(a);
  //dir.PlusEquals(dir);
  posx:=posx+dir.x;
  posy:=posy+dir.y;
end;

end.
