
{**********************************************************************
 Package pl_Graphics32EXT.pkg
 this unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit XGR32_ParticleStar;

interface

uses

   Classes, Graphics, Controls, SysUtils,
   GR32, XGR32_Sprites, XGR32_ParticleAniEffects;

type
  TGRStarParticle = class(TGRCustomParticle)
  private
    angle: TFloat;
    rot: Integer;
    sfacx: TFloat;
    sfacy: TFloat;
    sp: Integer;
    speed: TFloat;
    xpos: TFloat;
    ypos: TFloat;
    zpos: TFloat;
  protected
    procedure Reset; override;
  public
    constructor Create(aOwner: TGRSprites); override;
    class procedure InitParticleEngine(const aParticleEngine: TGRParticlesEffect); override;
    procedure Move(const MoveCount: TFloat); override;
  end;
  

implementation

constructor TGRStarParticle.Create(aOwner: TGRSprites);
begin
  inherited Create(aOwner);
  sfacx := 0.5;
  sfacy := 0.5;
  rot := 1;
  //rot := random(3);
  sp := 200 + random(100);
end;

class procedure TGRStarParticle.InitParticleEngine(const aParticleEngine:
  TGRParticlesEffect);
begin
  with aParticleEngine.Particle do
  begin
    //AccelX := 10 + random(100);
    //AccelY := 0;
    //X := -5;
    Decay := 0;
    LifeTime := 1;
  end;
end;

procedure TGRStarParticle.Move(const MoveCount: TFloat);
var
  tempx, tempy: Integer;
begin
  zpos := zpos - speed;
   if (zpos < 0) then
   begin
    //zpos := -10;
    reset;
   end;
   Rotation := Rotation + rot;
   tempx := round(xpos / zpos + Engine.Width / 2);
   tempy := round(ypos / zpos + Engine.Height / 2);
   if (tempx < -5) or (tempx > Engine.Width)
     or (tempy < -5) or (tempy > Engine.Height)
   then
     reset;
   Left := tempx;
   Top := tempy;
   Alpha := 256 - round(zpos);
   ScaleX := (alpha / 200) * sfacx;
   ScaleY := (alpha / 200) * sfacy;
end;

procedure TGRStarParticle.Reset;
begin
   zpos := 200;
   xpos := (-15 + random * 30 ) * zpos * 10;
   ypos := (-15 + random * 30) * zpos * 10;
  
   speed := 0.2 + random * 2;
   speed := speed *  (sp / 50);
   //Alpha := 0;
   //ScaleX := 40;
   //ScaleY := 40;
   Rotation := random(360);
  {
   if colored then
   begin
    red := random(256);
    green := random(256);
    blue := random(256);
   end else
   begin
    red := 255;
    green := 255;
    blue := 255;
   end;
  //}
end;


end.
