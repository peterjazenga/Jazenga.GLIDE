
{**********************************************************************
 Package pl_Graphics32EXT.pkg
 this unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit XGR32_ParticleAniEffects;

interface

uses

  LCLIntf, LCLType, LMessages,
  Messages, Classes, SysUtils, Graphics, Controls, Math,
  GR32, XGR32_AniEffects, XGR32_Sprites;

const
  cMaxParticles = 255;
  cDefaultLifeTime = 100;
  cDefaultDecay = 1;
  cDefaultZOrder = 500;
  
type
  TGRParticleClass = class of TGRCustomParticle;
  TGRParticlesEffect = class;
  TParticleProperty = class(TObject)
  private
    FAccelX: TFloat;
    FAccelY: TFloat;
    FAlpha: Byte;
    FDecay: TFloat;
    FLifeTime: TFloat;
    FLooped: Boolean;
    FPicture: TPictureEx;
    FRandomLocation: Boolean;
    FRepeatCount: Integer;
    FRotation: TFloat;
    FScaleX: TFloat;
    FScaleY: TFloat;
    FVelocityX: TFloat;
    FVelocityY: TFloat;
    FX: TFloat;
    FY: TFloat;
    FZ: Integer;
    procedure SetPicture(const Value: TPictureEx);
  public
    constructor Create;
    destructor Destroy; override;
    property AccelX: TFloat read FAccelX write FAccelX;
    property AccelY: TFloat read FAccelY write FAccelY;
    property Alpha: Byte read FAlpha write FAlpha default $FF;
    property Decay: TFloat read FDecay write FDecay;
    property LifeTime: TFloat read FLifeTime write FLifeTime;
    property Looped: Boolean read FLooped write FLooped;
    property Picture: TPictureEx read FPicture write SetPicture;
    property RandomLocation: Boolean read FRandomLocation write FRandomLocation;
    property RepeatCount: Integer read FRepeatCount write FRepeatCount;
    property Rotation: TFloat read FRotation write FRotation;
    property ScaleX: TFloat read FScaleX write FScaleX;
    property ScaleY: TFloat read FScaleY write FScaleY;
    property VelocityX: TFloat read FVelocityX write FVelocityX;
    property VelocityY: TFloat read FVelocityY write FVelocityY;
    property X: TFloat read FX write FX;
    property Y: TFloat read FY write FY;
    property Z: Integer read FZ write FZ default cDefaultZOrder;
  end;
  
  TGRCustomParticle = class(TGRImageSprite)
  private
    FAccelX: TFloat;
    FAccelY: TFloat;
    FDecay: TFloat;
    FEngine: TGRParticlesEffect;
    FLifeTime: TFloat;
    FLooped: Boolean;
    FRepeatCount: Integer;
    FVelocityX: TFloat;
    FVelocityY: TFloat;
  protected
    FLifeUnderZero: Boolean;
    procedure Reset; virtual;
  public
    destructor Destroy; override;
    class procedure InitParticleEngine(const aParticleEngine: TGRParticlesEffect); virtual;
    procedure Move(const MoveCount: TFloat); override;
    property Engine: TGRParticlesEffect read FEngine;
  published
    property AccelX: TFloat read FAccelX write FAccelX;
    property AccelY: TFloat read FAccelY write FAccelY;
    property Decay: TFloat read FDecay write FDecay;
    property LifeTime: TFloat read FLifeTime write FLifeTime;
    property Looped: Boolean read FLooped write FLooped;
    property RepeatCount: Integer read FRepeatCount write FRepeatCount;
    property VelocityX: TFloat read FVelocityX write FVelocityX;
    property VelocityY: TFloat read FVelocityY write FVelocityY;
  end;
  
  TGRParticlesEffect = class(TGRCustomAnimationEffect)
  private
    FMaxParticles: Integer;
    FNumOfParticles: Integer;
    FParticle: TParticleProperty;
    FSprites: TGRSprites;
    procedure SetNumOfParticles(Value: Integer);
  protected
    FBuffer: TBitmap32;
    FParticleClass: TGRParticleClass;
    FParticles: TList;
    procedure DoMouseMove(Shift: TShiftState; X, Y: Integer); overload;override;
    procedure DoPaint(Sender: TControl; DC: HDC); overload;override;
    procedure DoPaint(Sender: TBitmap32); override;
    procedure DoResize(Sender: TControl); override;
    procedure DoTimer(MoveCount: TFloat); override;
    procedure NewParticle(aParticle: TGRCustomParticle);
    procedure SizeChanged(Sender: TControl); override;
  public
    constructor Create(aOwner: TGRCustomAnimationEffects); override;
    destructor Destroy; override;
    procedure UseCustomParticle(aParticleClass: TGRParticleClass);
    property NumOfParticles: Integer read FNumOfParticles write SetNumOfParticles;
    property Particle: TParticleProperty read FParticle write FParticle;
    property Sprites: TGRSprites read FSprites write FSprites;
  published
    property MaxParticles: Integer read FMaxParticles write FMaxParticles default cMaxParticles;
  end;
  

implementation

type
  TGRSpritesAccess = class(TGRSprites);
  
constructor TParticleProperty.Create;
begin
  inherited Create;
  FPicture := TPictureEx.Create;
  
  FPicture.DrawMode := dmBlend;
  
  FLifeTime := cDefaultLifeTime;
  FDecay := cDefaultDecay;
  FAlpha := $FF;
  
  FZ := cDefaultZOrder;
  
  FScaleX := 1;
  FScaleY := 1;
end;

destructor TParticleProperty.Destroy;
begin
  FreeAndNil(FPicture);
  inherited Destroy;
end;

procedure TParticleProperty.SetPicture(const Value: TPictureEx);
begin
  if Value <> FPicture then
    FPicture.Assign(Value);
end;

destructor TGRCustomParticle.Destroy;
begin

  inherited Destroy;
end;

class procedure TGRCustomParticle.InitParticleEngine(const aParticleEngine:
  TGRParticlesEffect);
begin
end;

procedure TGRCustomParticle.Move(const MoveCount: TFloat);
var
  I: Integer;
  randTemp: Integer;
begin
  Left := Left + FVelocityX * MoveCount;
  Top := Top + FVelocityY * MoveCount;
  FVelocityX := FVelocityX + FAccelX * MoveCount;
  FVelocityY := FVelocityY + FAccelY * MoveCount;

  
  if FDecay <> 0 then
    FLifeTime := FLifeTime - FDecay * MoveCount;
  if FLifeTime <= 0 then
  begin
    if not FLooped then
    begin
      if FRepeatCount > 0 then
      begin
        Dec(FRepeatCount);
        Reset;
      end
      else
      begin
        Die;
      end;
    end
    else
    begin
      Reset;
    end;
  end;
end;

procedure TGRCustomParticle.Reset;
begin
  Left := FEngine.Particle.X;
  top := FEngine.Particle.Y;
  Z := FEngine.Particle.Z;
  
  AccelX := FEngine.Particle.AccelX;
  AccelY := FEngine.Particle.AccelY;
  VelocityX := FEngine.Particle.VelocityX;
  VelocityY := FEngine.Particle.VelocityY;
  
  if FLifeTime < -1 then
    FLifeUnderZero := True;
  
  
  FLifeTime := FEngine.Particle.FLifeTime;
  FDecay := FEngine.Particle.FDecay;
end;

constructor TGRParticlesEffect.Create(aOwner: TGRCustomAnimationEffects);
begin
  inherited Create(aOwner);
  FParticle := TParticleProperty.Create;
  FBuffer := TBitmap32.Create;

  FMaxParticles := cMaxParticles;
  
  FBuffer.DrawMode := dmBlend;
end;

destructor TGRParticlesEffect.Destroy;
begin
  FreeAndNil(FParticle);
  FreeAndNil(FBuffer);
  inherited Destroy;
end;

procedure TGRParticlesEffect.DoMouseMove(Shift: TShiftState; X, Y: Integer);
begin
  {if PtInRect(Rect(Left, Top, Left+Width, Top+Height), Point(X, Y)) then
  begin
    if ssLeft in Shift then
      Blob(X-Left, Y-Top, 1, FClickBlob)
    else
      Blob(X-Left, Y-Top, 1, FTrackBlob);
  end;
  //}
end;

procedure TGRParticlesEffect.DoPaint(Sender: TBitmap32);
begin
  try
    Sprites.Draw(Sender);

  except

  end;
end;

procedure TGRParticlesEffect.DoPaint(Sender: TControl; DC: HDC);
begin
  
  BitBlt(FBuffer.Handle, 0, 0, FBuffer.Width, FBuffer.Height, DC, FLeft, FTop, SRCCOPY);
  FBuffer.ResetAlpha;
  try
    Sprites.Draw(FBuffer);
  except

  end;

  BitBlt(DC, FLeft, FTop, FBuffer.Width, FBuffer.Height, FBuffer.Handle, 0, 0, SRCCOPY);

end;

procedure TGRParticlesEffect.DoResize(Sender: TControl);
begin

  
  FWidth := Sender.ClientWidth;
  FHeight := Sender.ClientHeight;
  
  SizeChanged(Sender);
end;

procedure TGRParticlesEffect.DoTimer(MoveCount: TFloat);
begin
  FSprites.Move(MoveCount);
  
  if FSprites.Count <> MaxParticles then
    NumOfParticles := MaxParticles;

  
  if TGRSpritesAccess(FSprites).FDeadList.Count > 0 then
  begin
    FSprites.CleanDeadSprites;
  end;

end;

procedure TGRParticlesEffect.NewParticle(aParticle: TGRCustomParticle);
begin
  aParticle.BeginUpdate;
  try
  aParticle.FEngine := Self;
  aParticle.FLooped := FParticle.Looped;
  
  aParticle.Picture := FParticle.Picture;
  aParticle.FRotation  := FParticle.Rotation;
  aParticle.FScaleX    := FParticle.ScaleX;
  aParticle.FScaleY    := FParticle.ScaleY;
  
  aParticle.Reset;
  finally
    aParticle.EndUpdate;
  end;
end;

procedure TGRParticlesEffect.SetNumOfParticles(Value: Integer);
var
  p: TGRCustomParticle;
  I: Integer;
  randTemp: Integer;
  I2: Integer;
begin
  if Value < 0 then Value := 0;  // this way there is no error if the user accidently set then number of particles below zero
  
  if not Assigned(FSprites) then Exit;
  
  
  FNumOfParticles := FSprites.Count;

  
  if Value > FNumOfParticles then
  begin
    for i := FNumOfParticles to Value -1 do
    begin
      p := FParticleClass.Create(FSprites);

  
      NewParticle(p);

  
      if FParticle.FRandomLocation then
      begin
        randTemp := RoundNormal(FParticle.FLifeTime);
        randTemp := Random(randTemp);
        for i2 := 0 to randTemp do
            p.Move(1);
      end;

    end;
    FNumOfParticles := Value;
  

  end
  else
  if Value < FNumOfParticles then
  begin
    if FNumOfParticles > FSprites.Count then
      FNumOfParticles := FSprites.Count;
    for i := FNumOfParticles - 1 downto Value do
    begin
      if TGRCustomParticle(FSprites.Items[i]) <> nil then
        TGRCustomParticle(FSprites.Items[i]).Die;

    end;
    FNumOfParticles := Value;

  end;
end;

procedure TGRParticlesEffect.SizeChanged(Sender: TControl);
begin
  FBuffer.SetSize(FWidth, FHeight);
end;

procedure TGRParticlesEffect.UseCustomParticle(aParticleClass:
  TGRParticleClass);
begin
  FParticleClass := aParticleClass;
  if Assigned(FParticleClass) then
    FParticleClass.InitParticleEngine(Self);
end;


end.
