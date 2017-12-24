
{**********************************************************************
 Package pl_Graphics32EXT.pkg
 this unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit XGR32_ParticleSnow;

interface
uses
   Classes, Graphics, Controls, SysUtils,
   GR32, XGR32_Sprites, XGR32_ParticleAniEffects;

type
  TGRSnowParticle = class(TGRCustomParticle)
  protected
    procedure Reset; override;
  public
    MaxA: Integer;
    MaxScaleX: TFloat;
    MaxScaleY: TFloat;
    MaxWobble: Integer;
    MaxX: TFloat;
    MaxY: TFloat;
    MinA: Integer;
    MinScaleX: TFloat;
    MinScaleY: TFloat;
    MinWobble: Integer;
    MinX: TFloat;
    MinY: TFloat;
    constructor Create(aOwner: TGRSprites); override;
    class procedure InitParticleEngine(const aParticleEngine: TGRParticlesEffect); override;
    procedure Move(const MoveCount: TFloat); override;
  end;

implementation

constructor TGRSnowParticle.Create(aOwner: TGRSprites);
begin
  inherited Create(aOwner);
  // the snowflakes' movement/behaviour/appearance!
  MinX := 1.6; MaxX := 1;
  MinY := 1.4; MaxY := 1.8;
  
  // MinX := 1.6; MaxX := 1;
  // MinY := 1.4; MaxY := 1.8;
  MinA := 150; MaxA := 256;
  MinScaleX := 0.2; MaxScaleX := 0.8;
  
  MinWobble := -5;
  MaxWobble := 5;
end;

class procedure TGRSnowParticle.InitParticleEngine(const aParticleEngine:
  TGRParticlesEffect);
begin
  with aParticleEngine.Particle do
  begin
    AccelX := 0; AccelY := 0;
    X := -5;
    Decay := 0.1;
    LifeTime := 500;
  end;
end;

procedure TGRSnowParticle.Move(const MoveCount: TFloat);
begin
  BeginUpdate;
  try
  inherited Move(MoveCount);
  Z := Z - 1;
  Left := Left + 0.1 * (MinWobble + random(MaxWobble - MinWobble));

  
  if Left > Engine.Width + 10 then
  begin
    Left := -5;
    Top := random(Engine.Height);
  end;
  if Left < -10 then
  begin
    Left := Engine.Width + 5;
    Top := random(Engine.Height);
  end;
  if Top > Engine.Height + 10 then
  begin
    Top := -5;
    Left := random(Engine.Width);
  end;
  if Top < -10 then
  begin
    Top := Engine.Height + 5;
    Left := random(Engine.Width);
  end;
  finally
    EndUpdate;
  end;

end;

procedure TGRSnowParticle.Reset;
begin
  inherited Reset;
  // change these parameters to influence
  VelocityX := MinX + random * (MaxX - MinX);
  VelocityY := MinY + random * (MaxY - MinY);
  ScaleX := MinScaleX + random * (MaxScaleX - MinScaleX);
  ScaleY := ScaleX;
  Alpha := MinA + random(MaxA - MinA);
  
  
  //Decay := 0;
  Left := random(Engine.Width);
  //Top := random(Engine.Height);
end;


end.
