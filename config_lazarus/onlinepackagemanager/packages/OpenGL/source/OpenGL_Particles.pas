{**********************************************************************
 Package pl_OpenGL.pkg
 This unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit OpenGL_Particles;

interface
uses gl, math;

const
  RND_NUANCES = 100;
  GRAV_MIN_RADIUS = 0.2;

type

TFxVector = record
  X     : single;
  Y     : single;
  Z     : single;
end;

TFxColor = record
  Red     : single;
  Green   : single;
  Blue    : single;
end;


TFxBoundingBox = record
  Min   : TFxVector;
  Max   : TFxVector;
end;

//Bottom-Left Edge = 0,0 Top-Right Edge = 1,1 (ogl-coordinate-system)
TFxTexTile = record
  MinU  : single;
  MinV  : single;
  MaxU  : single;
  MaxV  : single;
end;

//Size = 15*Single + 2 * Integer + boolean = 68 Byte
TFxParticle = record
  Position : TFxVector;
  Velocity : TFxVector;
  Density  : single; //1 standard
  Mass     : single;
  Size     : single;
  Spin     : single; //rotation per sec. no effect if FX_(Tex)Spark or FX_Points
  Rotation : single; //rotation in deg.
  Color    : TFxColor;
  Sat      : single;        //[0..1]
  LiveSpan : integer;
  Age      : integer;
  Junk     : boolean;
end;

pFxParticle = ^TFxParticle;

TFxGsMode = (FX_POINT, FX_QUAD, FX_SPARK, FX_TEXQUAD, FX_TEXSPARK, FX_SPRITE);//FX_SPRITE = Textured Quad with AlphaMask
TFxGsBlending = (FX_OPAQUE, FX_GLOW, FX_TRANSPARENT, FX_MASKED, FX_LIGHT);
TFxGsFading = (FX_NO_FADE, FX_FADE_IN, FX_FADE_OUT, FX_FADE_IN_AND_OUT);

TFxGroupSettings = record
  Mode        : TFxGsMode;
  Blending    : TFxGsBlending;
  Texture     : GluInt; //used only for TEXQUAD, TEXSPARK or SPRITE
  TexTile     : TFxTexTile;
  Elongation  : Single; //used only for SPARK or TEXSPARK.
  FadeInStop  : Single; //ranges from 0..1*
  FadeOutStart: Single; //ranges from 0..1*
  ZSort       : Boolean;
end;

{ How to use FadeIn and FadeOut...
 The Particles Lifetime is scaled down to 1.
 From 0 to FadeInStop the Particle fades in.
 From FadeInStop to FadeOutStart the Particle stays constant.
 From FadeOutStart to 1 the Particle fades out.
 Thus both need to range between 0..1 while FadeOutStart needs to be bigger than
 FadeInStop.}


const

FX_STD_TEMPLATE : TFxParticle = (Position : (X : 0; Y : 0; Z : 0;);
                                 Velocity : (X : 0; Y : 0; Z : 0;);
                                 Density  : 1;
                                 Mass     : 1;
                                 Size     : 0.1;
                                 Spin     : 0;
                                 Rotation : 0;
                                 Color    : (Red: 1; Green : 1; Blue : 1;);
                                 Sat      : 1;
                                 LiveSpan : 1000;
                                 Age      : 0;
                                 Junk     : false;);


FX_EMPTY_BOUNDINGBOX : TFxBoundingBox = (Min : (X : 0; Y : 0; Z : 0;);
                                         Max : (X : 0; Y : 0; Z : 0;));

                                         
//Particle-Attribute-Bitmask
FX_POSITION = $0001;
FX_VELOCITY = $0002;
FX_COLOR    = $0004;
FX_MASS     = $0008;
FX_SIZE     = $0010;
FX_DENSITY  = $0020;
FX_SPIN     = $0040;
FX_ROTATION = $0080;
FX_SAT      = $0100;
FX_LIVESPAN = $0200;

//Vector-Parts
FX_POS_X    = $0400;
FX_POS_Y    = $0800;
FX_POS_Z    = $1000;
FX_VEL_X    = $2000;
FX_VEL_Y    = $4000;
FX_VEL_Z    = $8000;

FX_ALL      = $03FF; //All Attrbutes

//====================== CLASSES ====================================

type

TFxGroup = class(TObject)
protected
  FJunkCount : word;
  JunkIndices : array of word;
  FTransparency : single; //can be used to blend whole groups in and out. 0 = invisible. 1 = fully visible
  FHighestActive : integer;
  FCalcBoundingBox :Boolean;
  FBoundingBox : TFxBoundingBox;
  function getSize : word; //how many Particle-Slots?
  function getActiveCount : word;
  procedure setTransparency(transp : single);
public
  Particles : array of TFXParticle; //don't add Particles directly... use Add() instead
  MinimizeSpread : boolean; //advance is slightly faster - deleteing is slower due to a loop through all particles
  CalcBoundingBox : boolean;
  Visible : boolean;
  Update : boolean;
  Settings  : TFxGroupSettings; //Rendering, additional common Particle attributes etc

  constructor create(aSize : word);
  function add(aParticle : TFxParticle) : integer; overload;//returns Index of created Particle
  procedure add(aParticle: TFxParticle; const count : integer); overload;
  procedure delete(aIndex : integer);
  procedure advance(aTime : integer); // Updating all Particles up to HeighestActive (time in ms)
  procedure clear;

  property BoundingBox : TFxBoundingBox read FBoundingBox;
  property Size : word read GetSize;
  property ActiveCount : word read GetActiveCount;
  property JunkCount   : word read FJunkCount;
  property HighestActive : integer read FHighestActive; //Update and Render only Particles up too HighestActive
  property Transparency : single read FTransparency write setTransparency;
end;


TFxDepthEntry = record
  GroupIdx : integer;
  Particle : pFxParticle;
  Z : single;
end;


TFxDepthList = class(TObject)
protected
  FCount : integer;
  FSize : word; //Slots
  Temp  : array of TFxDepthEntry;
  procedure setSize(aSize : word);
  procedure mergesort(const low, high : integer);
public
  Entrys : array of TFxDepthEntry;

  constructor Create(aSize : word);
  procedure add(aGroupIdx : integer; aParticle : pFxParticle);
  procedure sort;
  procedure calcZ;
  procedure clear;

  property Count : integer read FCount;
  property Size : word read FSize write setSize;
end;


TFxTemplateEntry = record
  Template : TFxParticle;
  Name  : string;
end;


TFxTemplateLib = class(TObject)
protected
  FTemplCount : integer;
  function getTemplateByName(aName : string) : pFxParticle;
public
  Templates : array of TFxTemplateEntry; //don't add Particles directly... use Add() instead

  constructor create;
  function add(aName : string) : integer; //returns Index
  procedure delete(aName : string); overload;
  procedure delete(index : integer); overload;
  procedure clear;

  property TemplatesByName[Name : string] : pFxParticle read GetTemplateByName; default;
  property TemplCount : integer read FTemplCount;
end;


TFxGroupEntry = record
  Group : TFxGroup;
  Name  : string;
end;


TFxGroupLib = class(TObject)
protected
  FGroups : array of TFxGroupEntry;
  FGroupCount : integer;
  FCalcBoundingBox :Boolean;
  function getBoundingBox : TFxBoundingBox;
  procedure setCalcBoundingBox(arg : Boolean);
  function getGroup(Index : Integer) : TFxGroup;
  function getGroupByName(aName : string) : TFxGroup;
public
  constructor create;
  destructor destroy; override;
  function  add(aName : string; aSize : word) : integer; //returns Index
  procedure delete(aName : string); overload;
  procedure delete(index : integer); overload;
  procedure clear;

  property Groups[Index: Integer] : TFxGroup read GetGroup;
  property GroupsByName[Name: String] : TFxGroup read GetGroupByName; default; //TEMP
  property GroupCount : integer read FGroupCount;
  property CalcBoundingBox : boolean read FCalcBoundingBox write setCalcBoundingBox;
  property BoundingBox : TFxBoundingBox read getBoundingBox;
end;


TFxSystem = class(TObject)
private
  DepthList : TFxDepthList;
protected
  Groups : TFxGroupLib;
  Templates : TFxTemplateLib;
  FCalcBoundingBox :Boolean;
  FAge : LongInt;
  function getBoundingBox : TFxBoundingBox;
  procedure setCalcBoundingBox(arg : Boolean);
  procedure init; virtual; //Override when Implementing FX!
public
  constructor create;
  destructor destroy; override;
  procedure render; virtual;
  procedure restart; virtual;
  procedure advance(aTime : integer); virtual; //Override when Implementing FX!
  property CalcBoundingBox : boolean read FCalcBoundingBox write setCalcBoundingBox;
  property BoundingBox : TFxBoundingBox read getBoundingBox;
  property Age : longInt read FAge;
end;


//=================================================================

//Billboarding

procedure fxBillboardBegin;
procedure fxBillboardEnd;
procedure fxBillboard;

//Creation of Record-Types

function fxVector(const X, Y, Z : single) : TFxVector;
function fxColor(const Red, Green, Blue : single) : TFxColor;
function fxTexTile(MinU, MinV, MaxU, MaxV : single) : TFxTexTile;
function fxParticle(const Position, Velocity : TFxVector;
                    const Density, Mass, Size, Spin, Rotation, Sat : single;
                    const Color : TFxColor;
                    const LiveSpan, Age : integer) : TFxParticle;


procedure ApplyRenderSettings(var GS : TFxGroupSettings);
function  fxFade(const LiveSpan, Age : Integer;const Settings : TFxGroupSettings) : single;
procedure fxRenderPoint(const Particle : TFxParticle;const Settings : TFxGroupSettings; const Transp : single);
procedure fxRenderQuad(const Particle : TFxParticle;const Settings : TFXGroupSettings; const Transp : single);
procedure fxRenderTexQuad(const Particle : TFxParticle;const Settings : TFXGroupSettings; const Transp : single);
procedure fxRenderSpark(const Particle : TFxParticle;const Settings : TFXGroupSettings; Transp : single);
procedure fxRenderTexSpark(const Particle : TFxParticle;const Settings : TFXGroupSettings; Transp : single);
procedure fxRenderSprite(const Particle : TFxParticle;const Settings : TFXGroupSettings; Transp : single);


//FUNCTIONS
function fxGenRandParticle(const aMinTmpl, aMaxTmpl : TFxParticle) : TFxParticle;
function fxGenInterpParticle(const aTmpl1, aTmpl2 : TFxParticle; const IFactor : single) : TFxParticle;  //IFactor: 0 -> result = aTmpl1; 1 -> result = aTmpl2

//PROCEDURES
procedure fxScale(const Attributes : word; var aParticle : TFxParticle; const scale1, scale2, scale3 : single); overload;
procedure fxScale(const Attributes : word; var aParticle : TFxParticle; const scale : single); overload;
procedure fxScale(const Attributes : word; aGroup : TFxGroup; const scale1, scale2, scale3 : single); overload;
procedure fxScale(const Attributes : word; aGroup : TFxGroup; const scale : single); overload;

procedure fxAddValues(const Attributes : word; var aDestParticle : TFxParticle; const aSrcParticle : TFxParticle); overload;
procedure fxAddValues(const Attributes : word; aGroup : TFxGroup; const aSrcParticle : TFxParticle); overload;

procedure fxSubValues(Attributes : word; var aDestParticle : TFxParticle; const aSrcParticle : TFxParticle); overload;
procedure fxSubValues(const Attributes : word; aGroup : TFxGroup; const aSrcParticle : TFxParticle); overload;

procedure fxReplaceValues(const Attributes : word; var aDestParticle : TFxParticle; const aSrcParticle : TFxParticle); overload;
procedure fxReplaceValues(const Attributes : word; aGroup : TFxGroup; const aSrcParticle : TFxParticle); overload;

procedure fxInterpValues(const Attributes : word; var aDestParticle : TFxParticle; const aSrcParticle : TFxParticle; IFactor : single); overload;
procedure fxInterpValues(const Attributes : word; aGroup : TFxGroup; const aSrcParticle : TFxParticle; IFactor : single); overload;

procedure fxInvert(const Attributes : word; var aParticle : TFxParticle); overload;
procedure fxInvert(const Attributes : word; aGroup : TFxGroup); overload;

procedure fxSetIntoSphere(var aParticle : TFxParticle; const center : TFxVector; const radius : single); overload; //index of added particle
procedure fxSetIntoSphere(var aParticle : TFxParticle; const cx, cy, cz, radius : single); overload; //index of added particle

procedure fxSetIntoCube(var aParticle : TFxParticle; const center : TFxVector; const size : single); overload;
procedure fxSetIntoCube(var aParticle : TFxParticle; const cx, cy, cz, size : single); overload;

procedure fxMoveDest(var aParticle : TFxParticle; const target : TFxVector); overload;
procedure fxMoveDest(var aParticle : TFxParticle; const tx, ty, tz : single); overload;
procedure fxMoveDest(aGroup : TFxGroup; const target : TFxVector); overload;
procedure fxMoveDest(aGroup : TFxGroup; const tx, ty, tz : single); overload;

procedure fxLinearGrav(var aParticle : TFxParticle; const direction : TFxVector; const gravity, time : single); overload;
procedure fxLinearGrav(var aParticle : TFxParticle; const dx, dy, dz : single; const gravity, time : single); overload;
procedure fxLinearGrav(aGroup : TFxGroup; const direction : TFxVector; const gravity, time : single); overload;
procedure fxLinearGrav(aGroup : TFxGroup; const dx, dy, dz : single; const gravity, time : single); overload;

procedure fxCircularGrav(var aParticle : TFxParticle; const point : TFxVector; const gravity, time : single); overload;
procedure fxCircularGrav(var aParticle : TFxParticle; const px, py, pz : single; const gravity, time : single); overload;
procedure fxCircularGrav(aGroup : TFxGroup; const point : TFxVector; const gravity, time : single); overload;
procedure fxCircularGrav(aGroup : TFxGroup; const px, py, pz : single; const gravity, time : single); overload;

procedure fxPointGrav(var aParticle : TFxParticle; const point : TFxVector; const minRadius, attrMass, time : single); overload;
procedure fxPointGrav(var aParticle : TFxParticle; const px, py, pz : single; const minRadius, attrMass, time : single); overload;
procedure fxPointGrav(aGroup : TFxGroup; const point : TFxVector; const minRadius, attrMass, time : single); overload;
procedure fxPointGrav(aGroup : TFxGroup; const px, py, pz : single; const minRadius, attrMass, time : single); overload;

procedure fxLineGrav(var aParticle : TFxParticle; const p1, p2 : TFxVector; const minRadius, attrMass, time : single); overload;
procedure fxLineGrav(var aParticle : TFxParticle; const p1x, p1y, p1z, p2x, p2y, p2z : single; const minRadius, attrMass, time : single); overload;
procedure fxLineGrav(aGroup : TFxGroup; const p1, p2 : TFxVector; const minRadius, attrMass, time : single); overload;
procedure fxLineGrav(aGroup : TFxGroup; const p1x, p1y, p1z, p2x, p2y, p2z : single; const minRadius, attrMass, time : single); overload;

procedure fxParticleGrav(aGroup : TFxGroup; const time : single); overload;
procedure fxParticleGrav(aGroup : TFxGroup; const minradius, time : single); overload;

procedure fxFriction(var aParticle : TFxParticle; const viscosity, time : single); overload;
procedure fxFriction(aGroup : TFxGroup; const viscosity, time : single); overload;

procedure fxSimpleFlow(var aParticle : TFxParticle; const direction : TFxVector; const viscosity, time : single); overload;
procedure fxSimpleFlow(var aParticle : TFxParticle; const dx, dy, dz : single; const viscosity, time : single); overload;
procedure fxSimpleFlow(aGroup : TFxGroup; const direction : TFxVector; const viscosity, time : single); overload;
procedure fxSimpleFlow(aGroup : TFxGroup; const dx, dy, dz : single; const viscosity, time : single); overload;


implementation

//************************ Particle Rendering ************************************

procedure ApplyRenderSettings(var GS : TFxGroupSettings);
var texactiv : boolean;
begin
  //Texturing
  if GS.Mode in [FX_TEXQUAD, FX_TEXSPARK, FX_SPRITE] then
  begin
    glEnable(GL_TEXTURE_2D);
    glBindTexture(GL_TEXTURE_2D,GS.Texture);
  end
  else
     glDisable(GL_TEXTURE_2D);
  //BlendMode
  glEnable(GL_BLEND);
  case GS.Mode of
    FX_SPRITE:
      begin
        case GS.Blending of
          FX_OPAQUE : begin
            glBlendFunc(GL_ONE, GL_ZERO);
          end;
          FX_GLOW, FX_LIGHT : begin
            glBlendFunc(GL_SRC_ALPHA, GL_ONE);
          end;
          FX_TRANSPARENT : begin
            glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
          end;
        end;
      end;
  else
    case GS.Blending of
      FX_OPAQUE : begin
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      end;
      FX_GLOW : begin
        glBlendFunc(GL_ONE_MINUS_DST_COLOR, GL_ONE);
      end;
      FX_TRANSPARENT : begin
        glBlendFunc(GL_SRC_COLOR, GL_ONE);
      end;
      FX_LIGHT : begin
        glBlendFunc(GL_ONE, GL_ONE);
      end;
    end;
  end;
end;


function fxFade(const LiveSpan, Age : Integer;const Settings : TFxGroupSettings) : single;
var now : single;
begin
  result := 1;
  //Check for the correct Range
  if (Settings.FadeInStop > 1) or
     (Settings.FadeOutStart > 1) or
     (Settings.FadeInStop < 0) or
     (Settings.FadeOutStart < 0) then exit;
  //Check that FadeIn and FadeOut doesn't cross
  if (Settings.FadeInStop > Settings.FadeOutStart) then exit;

  now := Age / (LiveSpan+Age);
  //Fade In
  if now < Settings.FadeInStop then result := now / (Settings.FadeInStop);
  //Fade Out
  if now > Settings.FadeOutStart then result := (1 - now) / (1 - Settings.FadeOutStart);
end;


procedure fxRenderPoint(const Particle : TFxParticle;const Settings : TFxGroupSettings; const Transp : single);
var Trsp : single;
begin
  with Particle do begin
    glPushMatrix;
    Trsp := 1;
    if Settings.Blending <> FX_OPAQUE then
       Trsp := Sat * Transp * fxFade(LiveSpan, Age, Settings);
    glColor4f(Color.Red*Trsp,Color.Green*Trsp,Color.Blue*Trsp,Transp);
    glPointSize(2);
    glBegin(GL_POINTS);
      glVertex3f(Position.x,Position.y,Position.z);
    glEnd;
    glPopMatrix;
  end;
end;

procedure fxRenderQuad(const Particle : TFxParticle;const Settings : TFXGroupSettings; const Transp : single);
var Trsp : single;
begin
  with Particle do begin
    glPushMatrix;
    if Settings.Blending = FX_OPAQUE then Trsp := 1 else Trsp := Sat * Transp * fxFade(LiveSpan, Age, Settings);
    glColor4f(Color.Red*Trsp,Color.Green*Trsp,Color.Blue*Trsp,Transp);
    glTranslatef(Position.x,Position.y,Position.z);
    fxBillboard;
    if Rotation <> 0 then glRotatef(Rotation,0,0,1);
    glBegin(GL_QUADS);
      glVertex3f(size,-size,0);
      glVertex3f(-size,-size,0);
      glVertex3f(-size,size,0);
      glVertex3f(+size,+size,0);
    glEnd;
    glPopMatrix;
  end;
end;

procedure fxRenderTexQuad(const Particle : TFxParticle;const Settings : TFXGroupSettings; const Transp : single);
var Trsp : single;
begin
  with Particle do begin
    glPushMatrix;
    if Settings.Blending = FX_OPAQUE then Trsp := 1 else Trsp := Sat * Transp * fxFade(LiveSpan, Age, Settings);
    glColor4f(Color.Red*Trsp,Color.Green*Trsp,Color.Blue*Trsp,Transp);
    glTranslatef(Position.x,Position.y,Position.z);
    fxBillboard;
    if Rotation <> 0 then glRotatef(Rotation,0,0,1);
    glBegin(GL_QUADS);
      glTexCoord2f(Settings.TexTile.MaxU,Settings.TexTile.MinV);
      glVertex3f(size,-size,0);
      glTexCoord2f(Settings.TexTile.MinU,Settings.TexTile.MinV);
      glVertex3f(-size,-size,0);
      glTexCoord2f(Settings.TexTile.MinU,Settings.TexTile.MaxV);
      glVertex3f(-size,size,0);
      glTexCoord2f(Settings.TexTile.MaxU,Settings.TexTile.MaxV);
      glVertex3f(+size,+size,0);
    glEnd;
    glPopMatrix;
  end;
end;

procedure fxRenderSpark(const Particle : TFxParticle;const Settings : TFXGroupSettings; Transp : single);
var v            : TFXVector;
    vLength      : single;
    Alpha        : single;
    Tail         : single;
    Matrix       : array[0..15] of single;
    Trsp         : single;
begin
  with Particle do begin
    glPushMatrix;
    if Settings.Blending = FX_OPAQUE then Trsp := 1 else Trsp := Sat * Transp * fxFade(LiveSpan, Age, Settings);
    glTranslatef(Position.x,Position.y,Position.z);
    //**Calculate Tail
    //rotate velocity-vector
    glGetFloatv(GL_MODELVIEW_MATRIX, @Matrix);
    v.x := velocity.x * Matrix[0] + velocity.y * Matrix[4] + velocity.z * Matrix[8];
    v.y := velocity.x * Matrix[1] + velocity.y * Matrix[5] + velocity.z * Matrix[9];
    vLength := sqrt(v.x*v.x+v.y*v.y);
    Tail := 1 + vLength*Settings.Elongation;
    //calc rotation (angle between v mapped to the viewplane and the y-axis)
    Alpha := arccos( v.y  / vlength) * 360 / (2*pi);//cos(alpha) = v * yaxis / vlength;
    if v.x < 0 then Alpha := - Alpha;
    //**Render
    fxBillboard;
    glRotatef(-Alpha,0,0,1);
    glBegin(GL_QUADS);
      glColor4f(0,0,0,0);
      glVertex3f(size, - size * Tail, 0);
      glVertex3f(-size, -size * Tail, 0);
      glColor4f(Color.Red*Trsp,Color.Green*Trsp,Color.Blue*Trsp,Transp);
      glVertex3f(-size, size, 0);
      glVertex3f(size, size, 0);
    glEnd;
    glPopMatrix;
  end;
end;


procedure fxRenderTexSpark(const Particle : TFxParticle;const Settings : TFXGroupSettings; Transp : single);
var v            : TFXVector;
    vLength      : single;
    Alpha        : single;
    Tail         : single;
    Matrix       : array[0..15] of single;
    Trsp         : single;
begin
  with Particle do begin
    glPushMatrix;
    if Settings.Blending = FX_OPAQUE then Trsp := 1 else Trsp := Sat * Transp * fxFade(LiveSpan, Age, Settings);
    glTranslatef(Position.x,Position.y,Position.z);
    //**Calculate Tail
    //rotate velocity-vector
    glGetFloatv(GL_MODELVIEW_MATRIX, @Matrix);
    v.x := velocity.x * Matrix[0] + velocity.y * Matrix[4] + velocity.z * Matrix[8];
    v.y := velocity.x * Matrix[1] + velocity.y * Matrix[5] + velocity.z * Matrix[9];
    vLength := sqrt(v.x*v.x+v.y*v.y);
    Tail := 1 + vLength*Settings.Elongation;
    //calc rotation (angle between v mapped to the viewplane and the y-axis)
    Alpha := arccos( v.y  / vlength) * 360 / (2*pi);//cos(alpha) = v * yaxis / vlength;
    if v.x < 0 then Alpha := - Alpha;
    //**Render
    fxBillboard;
    glRotatef(-Alpha,0,0,1);
    glBegin(GL_QUADS);
      glColor4f(Color.Red*Trsp,Color.Green*Trsp,Color.Blue*Trsp,Transp);
      glTexCoord2f(Settings.TexTile.MaxU,Settings.TexTile.MinV);
      glVertex3f(size, - size * Tail, 0);
      glTexCoord2f(0,0);
      glTexCoord2f(Settings.TexTile.MinU,Settings.TexTile.MinV);
      glVertex3f(-size, -size * Tail, 0);
      glTexCoord2f(Settings.TexTile.MinU,Settings.TexTile.MaxV);
      glVertex3f(-size, size, 0);
      glTexCoord2f(Settings.TexTile.MaxU,Settings.TexTile.MaxV);
      glVertex3f(size, size, 0);
    glEnd;
    glPopMatrix;
  end;
end;

procedure fxRenderSprite(const Particle : TFxParticle;const Settings : TFXGroupSettings; Transp : single);
begin
  with Particle do begin
    //NEEDS A TEXTURE WITH ALPHA CHANNEL!!!
    glPushMatrix;
    if Settings.Blending = FX_OPAQUE then glColor4f(Color.Red,Color.Green,Color.Blue, Transp)
    else glColor4f(Color.Red,Color.Green,Color.Blue,sat * Transp * fxFade(LiveSpan, Age, Settings));
    glTranslatef(Position.x,Position.y,Position.z);
    fxBillboard;
    if Rotation <> 0 then glRotatef(Rotation,0,0,1);
    glBegin(GL_QUADS);
      glTexCoord2f(Settings.TexTile.MaxU,Settings.TexTile.MinV);
      glVertex3f(size,-size,0);
      glTexCoord2f(Settings.TexTile.MinU,Settings.TexTile.MinV);
      glVertex3f(-size,-size,0);
      glTexCoord2f(Settings.TexTile.MinU,Settings.TexTile.MaxV);
      glVertex3f(-size,size,0);
      glTexCoord2f(Settings.TexTile.MaxU,Settings.TexTile.MaxV);
      glVertex3f(+size,+size,0);
    glEnd;
    glPopMatrix;
  end;
end;


//**************************** FXBillBoard *************************************

procedure fxBillboard;
var x,y : byte;
    Matrix : array[0..15] of single;
begin
 glGetFloatv(GL_MODELVIEW_MATRIX, @Matrix);
 for x := 0 to 2 do
  for y := 0 to 2 do
   if x=y then Matrix[x*4+y] := 1 else Matrix[x*4+y] := 0;
 glLoadMatrixf(@Matrix);
end;


procedure fxBillboardBegin;
var x,y : byte;
    Matrix : array[0..15] of single;
begin
//save original Matrix
 glPushMatrix;
 glGetFloatv(GL_MODELVIEW_MATRIX, @Matrix);
 for x := 0 to 2 do
  for y := 0 to 2 do
   if x=y then Matrix[x*4+y] := 1 else Matrix[x*4+y] := 0;
 glLoadMatrixf(@Matrix);
end;


procedure fxBillboardEnd;
begin
  //restore original Matrix
  glPopMatrix;
end;



//************************ Type-Data-Creation***********************************

function fxVector(const X, Y, Z : single) : TFxVector;
begin
  result.X := X;
  result.Y := Y;
  result.Z := Z;
end;

function fxColor(const Red, Green, Blue : single) : TFxColor;
begin
  result.Red := Red;
  result.Green := Green;
  result.Blue := Blue;
end;

function fxTexTile(MinU, MinV, MaxU, MaxV : single) : TFxTexTile;
begin
  result.MinU := MinU;
  result.MaxU := MaxU;
  result.MinV := MinV;
  result.MaxV := MaxV;
end;

function fxParticle(const Position, Velocity : TFxVector; const Density, Mass, Size, Spin, Rotation, Sat : single;
                    const Color : TFxColor; const LiveSpan, Age : integer) : TFxParticle;
begin
  result.Position := Position;
  result.Velocity := Velocity;
  result.Density  := Density;
  result.Mass     := Mass;
  result.Size     := Size;
  result.Spin     := Spin;
  result.Rotation := Rotation;
  result.Color    := Color;
  result.Sat      := Sat;
  result.LiveSpan := LiveSpan;
  result.Age      := Age;
  result.Junk     := false;
end;

//****************************** FXGroup ***************************************

function TFxGroup.getActiveCount : word;
begin
  result := length(Particles) - FJunkCount;
end;


function TFxGroup.getSize : word;
begin
  result := length(Particles);
end;


procedure TFxGroup.setTransparency(transp : single);
begin
  if (transp >= 0) and (transp <= 1) then FTransparency := transp;
end;

constructor TFxGroup.create(aSize : word);
var i : integer;
begin
  SetLength(Particles, aSize);
  SetLength(JunkIndices, aSize);
  for i := 0 to aSize-1 do begin
    JunkIndices[i]:= aSize - i - 1;
    Particles[i].Junk := true;
  end;
  FJunkCount := aSize;
  FHighestActive := -1;
  FTransparency := 1;
  Visible := true;
  Update := true;
  MinimizeSpread := false;
  //Set default RI
  with Settings do begin
    Mode         := FX_POINT;
    Blending     := FX_OPAQUE;
    Texture      := 0;
    TexTile      := fxTexTile(0,0,1,1);
    Elongation   := 2;
    FadeInStop   := 0;
    FadeOutStart := 1;
    ZSort        := false;
  end;
end;


function TFxGroup.add(aParticle : TFxParticle) : integer;
begin
  result := 0;
  if FJunkCount = 0 then exit;  //No Space for another Particle
  Particles[JunkIndices[FJunkCount-1]] := aParticle;
  Particles[JunkIndices[FJunkCount-1]].Junk := false;
  result := JunkIndices[FJunkCount-1];
  if FHighestActive < JunkIndices[FJunkCount-1] then FHighestActive := JunkIndices[FJunkCount-1];
  dec(FJunkCount);
end;


procedure TFxGroup.add(aParticle: TFxParticle; const count : integer);
var i : integer;
begin
  for i := 1 to count do begin
    if FJunkCount = 0 then exit; //No Space for further Particles!
    Particles[JunkIndices[FJunkCount-1]] := aParticle;
    Particles[JunkIndices[FJunkCount-1]].Junk := false;
    if FHighestActive < JunkIndices[FJunkCount-1] then FHighestActive := JunkIndices[FJunkCount-1];
    dec(FJunkCount);
  end;
end;


procedure TFxGroup.delete(aIndex : integer);
var index, temp : word;
begin
  if Particles[aIndex].Junk then exit; //Exit because Particle is allready deleted

  Particles[aIndex].Junk := True; //Kill Particle
  //Add to JunkIndices
  JunkIndices[FJunkCount] := aIndex;

  if MinimizeSpread then begin
    //OPTIMIZE - aIndex is sorted into JunkIndices. Smallest Indices are on top.
    for index := (FJunkCount) downto (FJunkCount - (FHighestActive - ActiveCount)) do
     if JunkIndices[index] > JunkIndices[index-1] then begin
       //swap
       temp := JunkIndices[index];
       JunkIndices[index] := JunkIndices[index-1];
       JunkIndices[index-1] := temp;
    end;
  end;

  inc(FJunkCount);
end;


procedure TFxGroup.advance(aTime : integer);
var i, hiactv : integer;
    maxSize : single;
    TimeBase : single;
begin
  TimeBase := 0.001 * aTime;
  if CalcBoundingBox then begin
    FBoundingBox := FX_EMPTY_BOUNDINGBOX;
    maxSize := 0;
  end;
  //if not Update then exit;
  hiactv := 0;
  for i := 0 to FHighestActive do
   with Particles[i] do if not Junk then begin
     //Update Position
     Position.X := Position.X + Velocity.X * TimeBase;
     Position.Y := Position.Y + Velocity.Y * TimeBase;
     Position.Z := Position.Z + Velocity.Z * TimeBase;
     //Rotate
     Rotation   := Rotation - Spin * TimeBase;
     if Rotation > 360 then Rotation := Rotation - 360;
     if Rotation < 0 then Rotation := Rotation + 360;
     //Update LiveSpan and Age
     LiveSpan   := LiveSpan - aTime;
     Age := Age + aTime;
     if LiveSpan > 0 then hiactv := i else Delete(i);

     //Calculate the BoundingBox
     if CalcBoundingBox then begin
       if maxSize < Size then maxSize := Size;
       if Position.X > FBoundingBox.Max.X then FBoundingBox.Max.X := Position.X;
       if Position.Y > FBoundingBox.Max.Y then FBoundingBox.Max.Y := Position.Y;
       if Position.Z > FBoundingBox.Max.Z then FBoundingBox.Max.Z := Position.Z;
       if Position.X < FBoundingBox.Min.X then FBoundingBox.Min.X := Position.X;
       if Position.Y < FBoundingBox.Min.Y then FBoundingBox.Min.Y := Position.Y;
       if Position.Z < FBoundingBox.Min.Z then FBoundingBox.Min.Z := Position.Z;
     end;
  end;

  if CalcBoundingBox then with FBoundingBox do begin
    //Enlarge BB so the Billboard-Expression of Particles are inside. (Won't work with FX_POINT and FX_SPARK)
    Max.X := Max.X + maxSize;
    Max.Y := Max.Y + maxSize;
    Max.Z := Max.Z + maxSize;
    Min.X := Min.X - maxSize;
    Min.Y := Min.Y - maxSize;
    Min.Z := Min.Z - maxSize;
  end;

  FHighestActive := hiactv;
end;


procedure TFxGroup.clear;
var i : word;
begin
  for i := 0 to Size-1 do begin
    Particles[i].Junk := true;
    JunkIndices[i]:= Size - i;
  end;
end;

//***************************** FXDepthList ************************************

constructor TFxDepthList.create(aSize : word);
begin
  FSize := aSize;
  FCount := 0;
  SetLength(Entrys,aSize);
  SetLength(Temp,aSize);
end;

Procedure TFxDepthList.setSize(aSize : word);
begin
  FSize := aSize;
  FCount := 0;
  SetLength(Entrys,FSize);
  SetLength(Temp,FSize);
end;


procedure TFxDepthList.add(aGroupIdx : integer; aParticle : pFxParticle);
begin
  if FCount >= FSize then exit;
  inc(FCount);
  Entrys[Count-1].GroupIdx := aGroupIdx;
  Entrys[Count-1].Particle := aParticle;
end;


procedure TFxDepthList.mergesort(const low, high : integer);
var pivot   : integer;
    length  : integer;
    i,pos1,pos2 : integer;
begin
  if low = high then exit;
  pivot := trunc((low+high) shr 1);
  length := high-low+1;
  mergesort(low,pivot);
  mergesort(pivot+1, high);
  for i := 0 to length-1 do temp[i] := Entrys[low+i];
  //Merge
  pos1 := 0;
  pos2 := pivot-low+1;
  for i := 0 to length-1 do if pos2 < length then begin
    if pos1 <= pivot-low then begin
      if temp[pos1].z > temp[pos2].z then begin
        Entrys[low+i] := temp[pos2];
        inc(pos2)
      end else begin
        Entrys[low+i] := temp[pos1];
        inc(pos1);
      end;
    end else begin //only upper Entrys left
      Entrys[i+low] := temp[pos2];
      inc(pos2);
    end;
  end else begin //only superior Entrys left
    Entrys[i+low] := temp[pos1];
    inc(pos1);
  end;
end;


procedure TFxDepthList.sort;
begin
  mergesort(0, Count-1);
end;


procedure TFxDepthList.calcZ;
var Matrix : array [0..15] of single;
    i      : integer;
begin
  glGetFloatv(GL_MODELVIEW_MATRIX, @Matrix);
  for i := 0 to Count-1 do
  with Entrys[i].Particle^.Position do
  Entrys[i].z := x * Matrix[2] + y * Matrix[6] + z * Matrix[10] + Matrix[14];
end;


procedure TFxDepthList.clear;
begin
  FCount := 0;
end;

//*************************** FXTemplateList ***********************************

function TFXTemplateLib.getTemplateByName(aName : string) : pFxParticle;
var i : integer;
begin
  result := nil;
  for i := 0 to TemplCount -1 do if Templates[i].Name = aName then begin
    result := @Templates[i].Template;
    exit;
  end;
  //Exception - Template not found!
end;


constructor TFXTemplateLib.create;
begin
  FTemplCount := 0;
  setLength(Templates, TemplCount);
end;


function TFXTemplateLib.add(aName : string) : integer; //returns Index
begin
  inc(FTemplCount);
  SetLength(Templates, TemplCount);
  Templates[TemplCount - 1].Name := aName;
  Templates[TemplCount - 1].Template := FX_STD_TEMPLATE;
  result := TemplCount - 1;
end;


procedure TFXTemplateLib.delete(aName : string);
var i, j : integer;
begin
  for i := 0 to TemplCount -1 do
   if Templates[i].Name = aName then begin
     {Delete}
     for j := i+1 to TemplCount-1 do Templates[j-1] := Templates[j];
     dec(FTemplCount);
     setLength(Templates, TemplCount);
  end;
end;


procedure TFXTemplateLib.delete(index : integer);
var i : integer;
begin
  if index >= TemplCount then exit;
  for i := index+1 to TemplCount-1 do Templates[i-1] := Templates[i];
  dec(FTemplCount);
  setLength(Templates, TemplCount);
end;


procedure TFXTemplateLib.clear;
begin
  FTemplCount := 0;
  setLength(Templates, TemplCount);
end;


//***************************** FXGroupLib ************************************

function TFxGroupLib.getGroup(Index : Integer) : TFxGroup;
begin
  if Index >= FGroupCount then begin
    result := nil;
    //Exception - Group not found!
  end else result := FGroups[Index].Group;
end;


function TFxGroupLib.getGroupByName(aName : String) : TFxGroup;
var i : integer;
begin
  result := nil;
  for i := 0 to GroupCount -1 do if FGroups[i].Name = aName then begin
    result := FGroups[i].Group;
    exit;
  end;
  //Exception - Group not found!
end;


procedure TFxGroupLib.setCalcBoundingBox(arg : boolean);
var i : integer;
begin
  FCalcBoundingBox := arg;
  //update Groups
  for i := 0 to GroupCount - 1 do FGroups[i].Group.CalcBoundingBox := arg;
end;


function TFxGroupLib.getBoundingBox : TFxBoundingBox;
var i : integer;
    BB: TFxBoundingBox;
begin
  BB := FX_EMPTY_BOUNDINGBOX;
  //Is BoundingBox enabled?
  if not CalcBoundingBox then begin
    result := BB;
    exit;
  end;

  //Merge Groups BoundingBoxes into BB
  for i := 0 to GroupCount - 1 do with FGroups[i].Group.BoundingBox do begin
    if Max.X > BB.Max.X then BB.Max.X := Max.X;
    if Max.Y > BB.Max.Y then BB.Max.Y := Max.Y;
    if Max.Z > BB.Max.Z then BB.Max.Z := Max.Z;

    if Min.X < BB.Min.X then BB.Min.X := Min.X;
    if Min.Y < BB.Min.Y then BB.Min.Y := Min.Y;
    if Min.Z < BB.Min.Z then BB.Min.Z := Min.Z;
  end;
  result := BB;
end;


constructor TFxGroupLib.create;
begin
  FGroupCount := 0;
  FCalcBoundingBox := false;
  setLength(FGroups, GroupCount);
end;


destructor TFxGroupLib.destroy;
var i : integer;
begin
  for i := 0 to GroupCount - 1 do FGroups[i].Group.Free;
end;


function TFxGroupLib.add(aName : string; aSize : word) : integer; //returns Index
begin
  inc(FGroupCount);
  setLength(FGroups, GroupCount);
  FGroups[GroupCount - 1].Name := aName;
  FGroups[GroupCount - 1].Group := TFxGroup.Create(aSize);
  FGroups[GroupCount - 1].Group.CalcBoundingBox := FCalcBoundingBox;
  result := GroupCount -1;
end;


procedure TFxGroupLib.delete(aName : string);
var i, j : integer;
begin
  for i := 0 to GroupCount -1 do
   if FGroups[i].Name = aName then begin
     {Delete}
     FGroups[i].Group.Free;
     for j := i+1 to GroupCount-1 do FGroups[j-1] := FGroups[j];
     dec(FGroupCount);
     setLength(FGroups, GroupCount);
   end;
end;


procedure TFxGroupLib.delete(index : integer);
var j : integer;
begin
  if index >= FGroupCount then exit;
  FGroups[index].Group.Free;
  for j := index+1 to GroupCount-1 do FGroups[j-1] := FGroups[j];
  dec(FGroupCount);
  setLength(FGroups, GroupCount);
end;


procedure TFxGroupLib.clear;
var i : integer;
begin
   for i := 0 to GroupCount - 1 do FGroups[i].Group.Free;
   FGroupCount := 0;
   SetLength(FGroups, GroupCount);
end;

//****************************** FXSystem **************************************

constructor TFxSystem.create;
begin
  Groups := TFxGroupLib.create;
  Templates := TFxTemplateLib.create;
  DepthList := TFxDepthList.create(0);
  FCalcBoundingBox := false;
  Init;
end;


destructor TFxSystem.destroy;
begin
  Groups.free;
  Templates.free;
  DepthList.free;
end;


procedure TFxSystem.setCalcBoundingBox(arg : boolean);
begin
  FCalcBoundingBox := arg;
  Groups.CalcBoundingBox := arg;
end;


function TFxSystem.getBoundingBox : TFxBoundingBox;
begin
  result := Groups.BoundingBox;
end;


procedure TFxSystem.render;
var i,j       : word;
    GS        : TFxGroupSettings;
    GIdx      : integer;
    Transp    : single;
begin
  glPushAttrib(GL_ALL_ATTRIB_BITS);
  //PREPARE DEPTHLIST
  DepthList.clear;
  j := 0;
  for i := 0 to Groups.GroupCount-1 do if Groups.Groups[i].Settings.ZSort then inc(j,Groups.Groups[i].ActiveCount);
  DepthList.Size := j;
  //RENDER
  for i := 0 to Groups.GroupCount-1 do
  with Groups.Groups[i] do if (Visible AND (ActiveCount > 0)) then begin
    //RENDER UNSORTED PARTICLES
    if not Settings.zSort then begin
      Transp := Groups.Groups[i].Transparency;
      applyRenderSettings(Settings);
      for j := 0 to HighestActive do if not Particles[j].Junk then begin
        case Settings.Mode of
          FX_TEXQUAD : FXRenderTexQuad(Particles[j], Settings, Transp);
          FX_SPRITE  : FxRenderSprite(Particles[j], Settings, Transp); //needs special blending modes
          FX_QUAD    : FXRenderQuad(Particles[j],  Settings, Transp);
          FX_POINT   : FXRenderPoint(Particles[j], Settings, Transp);
          FX_SPARK   : FXRenderSpark(Particles[j], Settings, Transp);
          FX_TEXSPARK: FXRenderTexSpark(Particles[j], Settings, Transp);
        end;
      end;
    //PUT INTO DEPTHLIST
    end else begin
      //Add to DepthList for z-ordered Rendering
      for j := 0 to HighestActive do if not Particles[j].Junk then DepthList.add(i, @Particles[j]);
    end;
  end;

  //RENDER PARTICLES FROM DEPTHLIST
  if DepthList.Count = 0 then begin
    glPopAttrib;
    exit;
  end;
  //Sort DepthList
  DepthList.calcZ;
  DepthList.sort;
  GIdx := -1;
  for i := 0 to Depthlist.Count-1 do begin
    //Set RI if not allready set
    if Depthlist.Entrys[i].GroupIdx <> GIdx then begin
      GIdx := Depthlist.Entrys[i].GroupIdx;
      GS := Groups.Groups[Depthlist.Entrys[i].GroupIdx].Settings;
      //Set Group-Transparency
      Transp := Groups.Groups[Depthlist.Entrys[i].GroupIdx].Transparency;
      applyRenderSettings(GS);
    end;
    //Render
    case GS.Mode of
      FX_TEXQUAD : fxRenderTexQuad(Depthlist.Entrys[i].Particle^, GS, Transp);
      FX_SPRITE  : fxRenderSprite(Depthlist.Entrys[i].Particle^, GS, Transp); //needs special blending modes
      FX_QUAD    : fxRenderQuad(Depthlist.Entrys[i].Particle^, GS, Transp);
      FX_POINT   : fxRenderPoint(Depthlist.Entrys[i].Particle^, GS, Transp);
      FX_SPARK   : fxRenderSpark(Depthlist.Entrys[i].Particle^, GS, Transp);
      FX_TEXSPARK: fxRenderTexSpark(Depthlist.Entrys[i].Particle^, GS, Transp);
    end;
  end;
  glPopAttrib;
end;


procedure TFxSystem.restart;
begin
  FAge := 0;
  Groups.clear;
  Templates.clear;

  init;
end;


procedure TFxSystem.init;
begin

end;


procedure TFxSystem.advance(aTime : integer);
var i : integer;
begin
  inc(FAge, aTime);

  for i := 0 to Groups.GroupCount -1 do with Groups.GetGroup(i) do
   if Update then advance(aTime);
end;

//===========================================================

//******************************************************************************
//*                             PARTICLE-ACTIONS                               *
//******************************************************************************


function fxGenRandParticle(const aMinTmpl, aMaxTmpl : TFxParticle) : TFxParticle;
begin
  with result do begin
  //Random-Value      Lower Bound                      Range                                 Factor (0..1)
    Position.x := aMinTmpl.Position.x + (aMaxTmpl.Position.x - aMinTmpl.Position.x) * Random(RND_NUANCES)/RND_NUANCES;
    Position.y := aMinTmpl.Position.y + (aMaxTmpl.Position.y - aMinTmpl.Position.y) * Random(RND_NUANCES)/RND_NUANCES;
    Position.z := aMinTmpl.Position.z + (aMaxTmpl.Position.z - aMinTmpl.Position.z) * Random(RND_NUANCES)/RND_NUANCES;
    Velocity.x := aMinTmpl.Velocity.x + (aMaxTmpl.Velocity.x - aMinTmpl.Velocity.x) * Random(RND_NUANCES)/RND_NUANCES;
    Velocity.y := aMinTmpl.Velocity.y + (aMaxTmpl.Velocity.y - aMinTmpl.Velocity.y) * Random(RND_NUANCES)/RND_NUANCES;
    Velocity.z := aMinTmpl.Velocity.z + (aMaxTmpl.Velocity.z - aMinTmpl.Velocity.z) * Random(RND_NUANCES)/RND_NUANCES;
    Color.Red  := aMinTmpl.Color.Red  + (aMaxTmpl.Color.Red - aMinTmpl.Color.Red)   * Random(RND_NUANCES)/RND_NUANCES;
    Color.Green:= aMinTmpl.Color.Green+ (aMaxTmpl.Color.Green - aMinTmpl.Color.Green)* Random(RND_NUANCES)/RND_NUANCES;
    Color.Blue := aMinTmpl.Color.Blue + (aMaxTmpl.Color.Blue - aMinTmpl.Color.Blue) * Random(RND_NUANCES)/RND_NUANCES;
    Density    := aMinTmpl.Density    + (aMaxTmpl.Density - aMinTmpl.Density)       * Random(RND_NUANCES)/RND_NUANCES;
    Mass       := aMinTmpl.Mass       + (aMaxTmpl.Mass - aMinTmpl.Mass)             * Random(RND_NUANCES)/RND_NUANCES;
    Size       := aMinTmpl.Size       + (aMaxTmpl.Size - aMinTmpl.Size)             * Random(RND_NUANCES)/RND_NUANCES;
    Spin       := aMinTmpl.Spin       + (aMaxTmpl.Spin - aMinTmpl.Spin)             * Random(RND_NUANCES)/RND_NUANCES;
    Rotation   := aMinTmpl.Rotation   + (aMaxTmpl.Rotation - aMinTmpl.Rotation)     * Random(RND_NUANCES)/RND_NUANCES;
    Sat        := aMinTmpl.Sat        + (aMaxTmpl.Sat - aMinTmpl.Sat)               * Random(RND_NUANCES)/RND_NUANCES;
    LiveSpan   := aMinTmpl.LiveSpan   + Round((aMaxTmpl.LiveSpan - aMinTmpl.LiveSpan)*Random(RND_NUANCES)/RND_NUANCES);
    Age        := 0;
    Junk       := false;
  end;
end;


function fxGenInterpParticle(const aTmpl1, aTmpl2 : TFxParticle;const IFactor : single) : TFxParticle;
var k1, k2 : single;
begin
  //Add a Particle and Set it's values...
  k2 := IFactor;
  if k2 > 1 then k2 := 1;
  if k2 < 0 then k2 := 0;
  k1 := 1 - k2;
  with result do begin
  //Random-Value
    Position.x := aTmpl1.Position.x*k1 + aTmpl2.Position.x*k2;
    Position.y := aTmpl1.Position.y*k1 + aTmpl2.Position.y*k2;
    Position.z := aTmpl1.Position.z*k1 + aTmpl2.Position.z*k2;
    Velocity.x := aTmpl1.Velocity.x*k1 + aTmpl2.Velocity.x*k2;
    Velocity.y := aTmpl1.Velocity.y*k1 + aTmpl2.Velocity.y*k2;
    Velocity.z := aTmpl1.Velocity.z*k1 + aTmpl2.Velocity.z*k2;
    Color.Red  := aTmpl1.Color.Red*k1  + aTmpl2.Color.Red*k2;
    Color.Green:= aTmpl1.Color.Green*k1+ aTmpl2.Color.Green*k2;
    Color.Blue := aTmpl1.Color.Blue*k1 + aTmpl2.Color.Blue*k2;
    Density    := aTmpl1.Density*k1    + aTmpl2.Density*k2;
    Mass       := aTmpl1.Mass*k1       + aTmpl2.Mass*k2;
    Size       := aTmpl1.Size*k1       + aTmpl2.Size*k2;
    Spin       := aTmpl1.Spin*k1       + aTmpl2.Spin*k2;
    Rotation   := aTmpl1.Rotation*k1   + aTmpl2.Rotation*k2;
    Sat        := aTmpl1.Sat*k1        + aTmpl2.Sat*k2;
    LiveSpan   := round(aTmpl1.LiveSpan*k1 + aTmpl2.LiveSpan*k2);
    Age        := 0;
    Junk       := false;
  end;
end;


//*** fxScale ***

procedure fxScale(const Attributes : word;var aParticle : TFxParticle;const scale1, scale2, scale3 : single);
begin
  with aParticle do begin
    if (Attributes and FX_POSITION = FX_POSITION) then begin
      Position.X := Position.X * scale1;
      Position.Y := Position.Y * scale2;
      Position.Z := Position.Z * scale3;
    end else begin
      if (Attributes and FX_POS_X = FX_POS_X) then Position.X := Position.X * scale1;
      if (Attributes and FX_POS_Y = FX_POS_Y) then Position.Y := Position.Y * scale2;
      if (Attributes and FX_POS_Z = FX_POS_Z) then Position.Z := Position.Z * scale3;
    end;
    if (Attributes and FX_VELOCITY = FX_VELOCITY) then begin
      Velocity.X := Velocity.X * scale1;
      Velocity.Y := Velocity.Y * scale2;
      Velocity.Z := Velocity.Z * scale3;
    end else begin
      if (Attributes and FX_VEL_X = FX_VEL_X) then Velocity.X := Velocity.X * scale1;
      if (Attributes and FX_VEL_Y = FX_VEL_Y) then Velocity.Y := Velocity.Y * scale2;
      if (Attributes and FX_VEL_Z = FX_VEL_Z) then Velocity.Z := Velocity.Z * scale3;
    end;
    if (Attributes and FX_COLOR = FX_COLOR) then begin
      Color.Red := Color.Red * scale1;
      if Color.Red > 1 then Color.Red := 1; if Color.Red < 0 then Color.Red := 0;
      Color.Green := Color.Green * scale2;
      if Color.Green > 1 then Color.Green := 1; if Color.Green < 0 then Color.Green := 0;
      Color.Blue := Color.Blue * scale3;
      if Color.Blue > 1 then Color.Blue := 1; if Color.Blue < 0 then Color.Blue := 0;
    end;
    if (Attributes and FX_MASS = FX_MASS) then Mass := Mass * scale1;
    if (Attributes and FX_SIZE = FX_SIZE) then Size := Size * scale1;
    if (Attributes and FX_SPIN = FX_SPIN) then Spin := Spin * scale1;
    if (Attributes and FX_ROTATION = FX_ROTATION) then Rotation := Rotation * scale1;
    if (Attributes and FX_DENSITY = FX_DENSITY) then Density := Density * scale1;
    if (Attributes and FX_SAT = FX_SAT) then Sat := Sat * scale1;
  end;
end;


procedure fxScale(const Attributes : word;var aParticle : TFxParticle;const scale : single);
begin
  with aParticle do begin
    if (Attributes and FX_POSITION = FX_POSITION) then begin
      Position.X := Position.X * scale;
      Position.Y := Position.Y * scale;
      Position.Z := Position.Z * scale;
    end else begin
      if (Attributes and FX_POS_X = FX_POS_X) then Position.X := Position.X * scale;
      if (Attributes and FX_POS_Y = FX_POS_Y) then Position.Y := Position.Y * scale;
      if (Attributes and FX_POS_Z = FX_POS_Z) then Position.Z := Position.Z * scale;
    end;
    if (Attributes and FX_VELOCITY = FX_VELOCITY) then begin
      Velocity.X := Velocity.X * scale;
      Velocity.Y := Velocity.Y * scale;
      Velocity.Z := Velocity.Z * scale;
    end else begin
      if (Attributes and FX_VEL_X = FX_VEL_X) then Velocity.X := Velocity.X * scale;
      if (Attributes and FX_VEL_Y = FX_VEL_Y) then Velocity.Y := Velocity.Y * scale;
      if (Attributes and FX_VEL_Z = FX_VEL_Z) then Velocity.Z := Velocity.Z * scale;
    end;
    if (Attributes and FX_COLOR = FX_COLOR) then begin
      Color.Red := Color.Red * scale;
      if Color.Red > 1 then Color.Red := 1; if Color.Red < 0 then Color.Red := 0;
      Color.Green := Color.Green * scale;
      if Color.Green > 1 then Color.Green := 1; if Color.Green < 0 then Color.Green := 0;
      Color.Blue := Color.Blue * scale;
      if Color.Blue > 1 then Color.Blue := 1; if Color.Blue < 0 then Color.Blue := 0;
    end;
    if (Attributes and FX_MASS = FX_MASS) then Mass := Mass * scale;
    if (Attributes and FX_SIZE = FX_SIZE) then Size := Size * scale;
    if (Attributes and FX_SPIN = FX_SPIN) then Spin := Spin * scale;
    if (Attributes and FX_ROTATION = FX_ROTATION) then Rotation := Rotation * scale;
    if (Attributes and FX_DENSITY = FX_DENSITY) then Density := Density * scale;
    if (Attributes and FX_SAT = FX_SAT) then Sat := Sat * scale;
  end;
end;


procedure fxScale(const Attributes : word; aGroup : TFxGroup;const scale1, scale2, scale3 : single);
var i : integer;
begin
  for i := 0 to aGroup.HighestActive do if not aGroup.Particles[i].Junk then
   fxScale(Attributes, aGroup.Particles[i], scale1, scale2, scale3);
end;


procedure fxScale(const Attributes : word; aGroup : TFxGroup;const scale : single);
var i : integer;
begin
  for i := 0 to aGroup.HighestActive do if not aGroup.Particles[i].Junk then
   fxScale(Attributes, aGroup.Particles[i], scale);
end;


//*** fxAddValues ***

procedure fxAddValues(const Attributes : word; var aDestParticle : TFxParticle;const aSrcParticle : TFxParticle);
begin
  with aDestParticle do begin
    if (Attributes and FX_POSITION = FX_POSITION) then begin
      Position.x := Position.x + aSrcParticle.Position.x;
      Position.y := Position.y + aSrcParticle.Position.y;
      Position.z := Position.z + aSrcParticle.Position.z;
    end else begin
      if (Attributes and FX_POS_X = FX_POS_X) then Position.X := Position.X + aSrcParticle.Position.x;
      if (Attributes and FX_POS_Y = FX_POS_Y) then Position.Y := Position.Y + aSrcParticle.Position.Y;
      if (Attributes and FX_POS_Z = FX_POS_Z) then Position.Z := Position.Z + aSrcParticle.Position.Z;
    end;
    if (Attributes and FX_VELOCITY = FX_VELOCITY) then begin
      Velocity.x := Velocity.x + aSrcParticle.Velocity.x;
      Velocity.y := Velocity.y + aSrcParticle.Velocity.y;
      Velocity.z := Velocity.z + aSrcParticle.Velocity.z;
    end else begin
      if (Attributes and FX_VEL_X = FX_VEL_X) then Velocity.X := Velocity.X + aSrcParticle.Velocity.x;
      if (Attributes and FX_VEL_Y = FX_VEL_Y) then Velocity.Y := Velocity.Y + aSrcParticle.Velocity.y;
      if (Attributes and FX_VEL_Z = FX_VEL_Z) then Velocity.Z := Velocity.Z + aSrcParticle.Velocity.z;
    end;
    if (Attributes and FX_COLOR = FX_COLOR) then begin
      Color.Red := Color.Red + aSrcParticle.Color.Red;
      if Color.Red > 1 then Color.Red := 1; if Color.Red < 0 then Color.Red := 0;
      Color.Green := Color.Green + aSrcParticle.Color.Green;
      if Color.Green > 1 then Color.Green := 1; if Color.Green < 0 then Color.Green := 0;
      Color.Blue := Color.Blue + aSrcParticle.Color.Blue;
      if Color.Blue > 1 then Color.Blue := 1; if Color.Blue < 0 then Color.Blue := 0;
    end;
    if (Attributes and FX_MASS = FX_MASS) then Mass := Mass + aSrcParticle.Mass;
    if (Attributes and FX_SIZE = FX_SIZE) then Size := Size + aSrcParticle.Size;
    if (Attributes and FX_SPIN = FX_SPIN) then Spin := Spin + aSrcParticle.Spin;
    if (Attributes and FX_ROTATION = FX_ROTATION) then Rotation := Rotation + aSrcParticle.Rotation;
    if (Attributes and FX_DENSITY = FX_DENSITY) then Density := Density + aSrcParticle.Density;
    if (Attributes and FX_SAT = FX_SAT) then Sat := Sat + + aSrcParticle.Sat;
  end;
end;


procedure fxAddValues(const Attributes : word; aGroup : TFxGroup;const aSrcParticle : TFxParticle);
var i : integer;
begin
  for i := 0 to aGroup.HighestActive do if not aGroup.Particles[i].Junk then
   fxAddValues(Attributes, aGroup.Particles[i], aSrcParticle);
end;


//*** fxSubValues

procedure fxSubValues(Attributes : word; var aDestParticle : TFxParticle;const aSrcParticle : TFxParticle);
begin
  with aDestParticle do begin
    if (Attributes and FX_POSITION = FX_POSITION) then begin
      Position.x := Position.x - aSrcParticle.Position.x;
      Position.y := Position.y - aSrcParticle.Position.y;
      Position.z := Position.z - aSrcParticle.Position.z;
    end else begin
      if (Attributes and FX_POS_X = FX_POS_X) then Position.X := Position.X - aSrcParticle.Position.x;
      if (Attributes and FX_POS_Y = FX_POS_Y) then Position.Y := Position.Y - aSrcParticle.Position.Y;
      if (Attributes and FX_POS_Z = FX_POS_Z) then Position.Z := Position.Z - aSrcParticle.Position.Z;
    end;
    if (Attributes and FX_VELOCITY = FX_VELOCITY) then begin
      Velocity.x := Velocity.x - aSrcParticle.Velocity.x;
      Velocity.y := Velocity.y - aSrcParticle.Velocity.y;
      Velocity.z := Velocity.z - aSrcParticle.Velocity.z;
    end else begin
      if (Attributes and FX_VEL_X = FX_VEL_X) then Velocity.X := Velocity.X - aSrcParticle.Velocity.x;
      if (Attributes and FX_VEL_Y = FX_VEL_Y) then Velocity.Y := Velocity.Y - aSrcParticle.Velocity.y;
      if (Attributes and FX_VEL_Z = FX_VEL_Z) then Velocity.Z := Velocity.Z - aSrcParticle.Velocity.z;
    end;
    if (Attributes and FX_COLOR = FX_COLOR) then begin
      Color.Red := Color.Red - aSrcParticle.Color.Red;
      if Color.Red > 1 then Color.Red := 1; if Color.Red < 0 then Color.Red := 0;
      Color.Green := Color.Green - aSrcParticle.Color.Green;
      if Color.Green > 1 then Color.Green := 1; if Color.Green < 0 then Color.Green := 0;
      Color.Blue := Color.Blue - aSrcParticle.Color.Blue;
      if Color.Blue > 1 then Color.Blue := 1; if Color.Blue < 0 then Color.Blue := 0;
    end;
    if (Attributes and FX_MASS = FX_MASS) then Mass := Mass - aSrcParticle.Mass;
    if (Attributes and FX_SIZE = FX_SIZE) then Size := Size - aSrcParticle.Size;
    if (Attributes and FX_SPIN = FX_SPIN) then Spin := Spin - aSrcParticle.Spin;
    if (Attributes and FX_ROTATION = FX_ROTATION) then Rotation := Rotation - aSrcParticle.Rotation;
    if (Attributes and FX_DENSITY = FX_DENSITY) then Density := Density - aSrcParticle.Density;
    if (Attributes and FX_SAT = FX_SAT) then Sat := Sat - aSrcParticle.Sat;
  end;
end;


procedure fxSubValues(const Attributes : word; aGroup : TFxGroup;const aSrcParticle : TFxParticle);
var i : integer;
begin
  for i := 0 to aGroup.HighestActive do if not aGroup.Particles[i].Junk then
   fxSubValues(Attributes, aGroup.Particles[i], aSrcParticle);
end;


//*** fxReplaceValues

procedure fxReplaceValues(const Attributes : word; var aDestParticle : TFxParticle; const aSrcParticle : TFxParticle);
begin
  with aDestParticle do begin
    if (Attributes and FX_POSITION = FX_POSITION) then begin
      Position.x := aSrcParticle.Position.x;
      Position.y := aSrcParticle.Position.y;
      Position.z := aSrcParticle.Position.z;
    end else begin
      if (Attributes and FX_POS_X = FX_POS_X) then Position.X := aSrcParticle.Position.X;
      if (Attributes and FX_POS_Y = FX_POS_Y) then Position.Y := aSrcParticle.Position.Y;
      if (Attributes and FX_POS_Z = FX_POS_Z) then Position.Z := aSrcParticle.Position.Z;
    end;
    if (Attributes and FX_VELOCITY = FX_VELOCITY) then begin
      Velocity.x := aSrcParticle.Velocity.x;
      Velocity.y := aSrcParticle.Velocity.y;
      Velocity.z := aSrcParticle.Velocity.z;
    end else begin
      if (Attributes and FX_VEL_X = FX_VEL_X) then Velocity.X := aSrcParticle.Velocity.x;
      if (Attributes and FX_VEL_Y = FX_VEL_Y) then Velocity.Y := aSrcParticle.Velocity.y;
      if (Attributes and FX_VEL_Z = FX_VEL_Z) then Velocity.Z := aSrcParticle.Velocity.z;
    end;
    if (Attributes and FX_COLOR = FX_COLOR) then begin
      Color.Red := aSrcParticle.Color.Red;
      if Color.Red > 1 then Color.Red := 1; if Color.Red < 0 then Color.Red := 0;
      Color.Green := aSrcParticle.Color.Green;
      if Color.Green > 1 then Color.Green := 1; if Color.Green < 0 then Color.Green := 0;
      Color.Blue := aSrcParticle.Color.Blue;
      if Color.Blue > 1 then Color.Blue := 1; if Color.Blue < 0 then Color.Blue := 0;
    end;
    if (Attributes and FX_MASS = FX_MASS) then Mass := aSrcParticle.Mass;
    if (Attributes and FX_SIZE = FX_SIZE) then Size := aSrcParticle.Size;
    if (Attributes and FX_SPIN = FX_SPIN) then Spin := aSrcParticle.Spin;
    if (Attributes and FX_ROTATION = FX_ROTATION) then Rotation := aSrcParticle.Rotation;
    if (Attributes and FX_DENSITY = FX_DENSITY) then Density := aSrcParticle.Density;
    if (Attributes and FX_SAT = FX_SAT) then Sat := aSrcParticle.Sat;
  end;
end;


procedure fxReplaceValues(const Attributes : word; aGroup : TFxGroup;const aSrcParticle : TFxParticle);
var i : integer;
begin
  for i := 0 to aGroup.HighestActive do if not aGroup.Particles[i].Junk then
   fxReplaceValues(Attributes, aGroup.Particles[i], aSrcParticle);
end;


//*** fxInterpValues

procedure fxInterpValues(const Attributes : word; var aDestParticle : TFxParticle; const aSrcParticle : TFxParticle; IFactor : single);
var k1, k2 : single;
begin
  k2 := IFactor;
  if k2 > 1 then k2 := 1;
  if k2 < 0 then k2 := 0;
  k1 := 1 - k2;
  with aDestParticle do begin
    if (Attributes and FX_POSITION = FX_POSITION) then begin
      Position.x := Position.x * k1 + aSrcParticle.Position.x * k2;
      Position.y := Position.y * k1 + aSrcParticle.Position.y * k2;
      Position.z := Position.z * k1 + aSrcParticle.Position.z * k2;
    end else begin
      if (Attributes and FX_POS_X = FX_POS_X) then Position.x := Position.x * k1 + aSrcParticle.Position.x * k2;
      if (Attributes and FX_POS_Y = FX_POS_Y) then Position.y := Position.y * k1 + aSrcParticle.Position.y * k2;
      if (Attributes and FX_POS_Z = FX_POS_Z) then Position.z := Position.z * k1 + aSrcParticle.Position.z * k2;
    end;
    if (Attributes and FX_VELOCITY = FX_VELOCITY) then begin
      Velocity.x := Velocity.x * k1 + aSrcParticle.Velocity.x * k2;
      Velocity.y := Velocity.y * k1 + aSrcParticle.Velocity.y * k2;
      Velocity.z := Velocity.z * k1 + aSrcParticle.Velocity.z * k2;
    end else begin
      if (Attributes and FX_VEL_X = FX_VEL_X) then Velocity.X := Velocity.x * k1 + aSrcParticle.Velocity.x * k2;
      if (Attributes and FX_VEL_Y = FX_VEL_Y) then Velocity.Y := Velocity.y * k1 + aSrcParticle.Velocity.y * k2;
      if (Attributes and FX_VEL_Z = FX_VEL_Z) then Velocity.Z := Velocity.z * k1 + aSrcParticle.Velocity.z * k2;
    end;
    if (Attributes and FX_COLOR = FX_COLOR) then begin
      Color.Red := Color.Red * k1 + aSrcParticle.Color.Red * k2;
      if Color.Red > 1 then Color.Red := 1; if Color.Red < 0 then Color.Red := 0;
      Color.Green := Color.Green * k1 + aSrcParticle.Color.Green * k2;
      if Color.Green > 1 then Color.Green := 1; if Color.Green < 0 then Color.Green := 0;
      Color.Blue := Color.Blue * k1 + aSrcParticle.Color.Blue * k2;
      if Color.Blue > 1 then Color.Blue := 1; if Color.Blue < 0 then Color.Blue := 0;
    end;
    if (Attributes and FX_MASS = FX_MASS) then Mass := Mass * k1 + aSrcParticle.Mass * k2;
    if (Attributes and FX_SIZE = FX_SIZE) then Size := Size * k1 + aSrcParticle.Size * k2;
    if (Attributes and FX_SPIN = FX_SPIN) then Spin := Spin * k1 + aSrcParticle.Spin * k2;
    if (Attributes and FX_ROTATION = FX_ROTATION) then Rotation := Rotation * k1 + aSrcParticle.Rotation * k2;
    if (Attributes and FX_DENSITY = FX_DENSITY) then Density :=  Density * k1 + aSrcParticle.Density * k2;
    if (Attributes and FX_SAT = FX_SAT) then Sat := Sat * k1 + aSrcParticle.Sat * k2;
  end;
end;


procedure fxInterpValues(const Attributes : word; aGroup : TFxGroup;const aSrcParticle : TFxParticle; IFactor : single);
var i : integer;
begin
  for i := 0 to aGroup.HighestActive do if not aGroup.Particles[i].Junk then
   fxInterpValues(Attributes, aGroup.Particles[i], aSrcParticle, IFactor);
end;


//*** fxInvert ***

procedure fxInvert(const Attributes : word; var aParticle : TFxParticle);
begin
  with aParticle do begin
    if (Attributes and FX_POSITION = FX_POSITION) then begin
      Position.x := -Position.x;
      Position.y := -Position.y;
      Position.z := -Position.z;
    end else begin
      if (Attributes and FX_POS_X = FX_POS_X) then Position.X := -Position.X;
      if (Attributes and FX_POS_Y = FX_POS_Y) then Position.Y := -Position.Y;
      if (Attributes and FX_POS_Z = FX_POS_Z) then Position.Z := -Position.Z;
    end;
    if (Attributes and FX_VELOCITY = FX_VELOCITY) then begin
      Velocity.x := -Velocity.x;
      Velocity.y := -Velocity.y;
      Velocity.z := -Velocity.z;
    end else begin
      if (Attributes and FX_VEL_X = FX_VEL_X) then Velocity.X := -Velocity.X;
      if (Attributes and FX_VEL_Y = FX_VEL_Y) then Velocity.Y := -Velocity.Y;
      if (Attributes and FX_VEL_Z = FX_VEL_Z) then Velocity.Z := -Velocity.Z;
    end;
    if (Attributes and FX_COLOR = FX_COLOR) then begin
      Color.Red := 1-Color.Red;
      Color.Green := 1-Color.Green;
      Color.Blue := 1-Color.Blue;
    end;
    if (Attributes and FX_SAT = FX_SAT) then Sat := 1-Sat;
    if (Attributes and FX_SPIN = FX_SPIN) then Spin := -Spin;
    if (Attributes and FX_ROTATION = FX_ROTATION) then Rotation := -Rotation;
  end;
end;


procedure fxInvert(const Attributes : word; aGroup : TFxGroup);
var i : integer;
begin
  for i := 0 to aGroup.HighestActive do if not aGroup.Particles[i].Junk then
   fxInvert(Attributes, aGroup.Particles[i]);
end;


//*** fxSetIntoSphere ***

procedure fxSetIntoSphere(var aParticle : TFxParticle;const center : TFxVector;const radius : single);
begin
  fxSetIntoSphere(aParticle, center.x, center.y, center.z, radius);
end;


procedure fxSetIntoSphere(var aParticle : TFxParticle;const cx, cy, cz, radius : single);
begin
  with aParticle.Position do repeat
   x := cx + random*radius*2-radius;
   y := cy + random*radius*2-radius;
   z := cz + random*radius*2-radius;
  until ((cx-x)*(cx-x)+(cy-y)*(cy-y)+(cz-z)*(cz-z)) < radius; //Radius small enough (Pythagoras)
end;


//*** fxSetIntoCube

procedure fxSetIntoCube(var aParticle : TFxParticle;const center : TFxVector;const size : single);
begin
  fxSetIntoCube(aParticle, center.x, center.y, center.z, size);
end;


procedure fxSetIntoCube(var aParticle : TFxParticle;const cx, cy, cz, size : single);
begin
  aParticle := aParticle;
  with aParticle.Position do begin
   x := cx + random*size*2-size;
   y := cy + random*size*2-size;
   z := cz + random*size*2-size;
  end;
end;

//*** fxMoveDest

procedure fxMoveDest(var aParticle : TFxParticle;const target : TFxVector);
begin
  fxMoveDest(aParticle, target.x, target.y, target.z);
end;


procedure fxMoveDest(var aParticle : TFxParticle;const tx, ty, tz : single);
var f : single;
begin

  with aParticle.Velocity do begin
    //calculate f := Velocity / Distance
    f := sqrt(x*x + y*y + z*z) / sqrt(sqr(tx -aParticle.Position.x) +  sqr(ty -aParticle.Position.y) + sqr(tz -aParticle.Position.z));
    x := f * (tx -aParticle.Position.x);
    y := f * (ty -aParticle.Position.y);
    z := f * (tz -aParticle.Position.z);
  end;
end;


procedure fxMoveDest(aGroup : TFxGroup; const target : TFxVector);
begin
  fxMoveDest(aGroup, target.x, target.y, target.z);
end;


procedure fxMoveDest(aGroup : TFxGroup; const tx, ty, tz : single);
var f : single;
    i   : integer;
begin
  for i := 0 to aGroup.HighestActive do if not aGroup.Particles[i].Junk then
  with aGroup.Particles[i] do begin
    //calculate f := Velocity / Distance
    f := sqrt(sqr(Velocity.x) + sqr(Velocity.y) + sqr(Velocity.z)) / sqrt(sqr(tx -Position.x) +  sqr(ty -Position.y) + sqr(tz -Position.z));
    Velocity.x := f * (tx -Position.x);
    Velocity.y := f * (ty -Position.y);
    Velocity.z := f * (tz -Position.z);
  end;
end;

//*** fxLinearGrav ***

procedure fxLinearGrav(var aParticle : TFxParticle; const direction : TFxVector; const gravity, time : single);
begin
  fxLinearGrav(aParticle, direction.x, direction.y, direction.z, gravity, time);
end;


procedure fxLinearGrav(var aParticle : TFxParticle; const dx, dy, dz : single; const gravity, time : single);
var dlength : single;
begin
  //normalize Vector
  dlength := sqrt(dx*dx + dy*dy + dz*dz);
  if dlength = 0 then dlength := 0.0001;
  //GlobalGravity pulls in the given direction (parallel)
  with aParticle.Velocity do begin
    //v = v0 + a * t
    x := x + (dx / dlength) * gravity * time / 1000;
    y := y + (dy / dlength) * gravity * time / 1000;
    z := z + (dz / dlength) * gravity * time / 1000;
  end;
end;


procedure fxLinearGrav(aGroup : TFxGroup;const direction : TFxVector;const gravity, time : single);
begin
  fxLinearGrav(aGroup, direction.x, direction.y, direction.z, gravity, time);
end;


procedure fxLinearGrav(aGroup : TFxGroup;const dx, dy, dz : single;const gravity, time : single);
var i : integer;
    dlength : single;
    dirx, diry, dirz : single;
begin
  //normalize Vector
  dlength := sqrt(dx*dx + dy*dy + dz*dz);
  if dlength = 0 then dlength := 0.0001;
  dirx := dx / dlength;
  diry := dy / dlength;
  dirz := dz / dlength;
  //GlobalGravity pulls in the given direction (parallel)
  for i := 0 to aGroup.HighestActive do if not aGroup.Particles[i].Junk then
  with aGroup.Particles[i].Velocity do begin
    //v = v0 + a * t
    x := x + dirx * gravity * time / 1000;
    y := y + diry * gravity * time / 1000;
    z := z + dirz * gravity * time / 1000;
  end;
end;


//*** fxCircularGrav ***

procedure fxCircularGrav(var aParticle : TFxParticle; const point : TFxVector; const gravity, time : single);
begin
  fxCircularGrav(aParticle, point.x, point.y, point.z, gravity, time);
end;


procedure fxCircularGrav(var aParticle : TFxParticle; const px, py, pz : single; const gravity, time : single);
var dlength : single;
    dirx, diry, dirz : single;
begin
  //CircularGravity pulls constantly to the given Point
  with aParticle do begin
    //get direction
    dirx := px - Position.x;
    diry := py - Position.y;
    dirz := pz - Position.z;
    //get dir-length
    dlength := sqrt(dirx*dirx + diry*diry + dirz*dirz);
    if dlength = 0 then dlength := 0.0001;
    //v = v0 + a * t
    Velocity.x := Velocity.x + (dirx / dlength) * gravity * time / 1000;
    Velocity.y := Velocity.y + (diry / dlength) * gravity * time / 1000;
    Velocity.z := Velocity.z + (dirz / dlength) * gravity * time / 1000;
  end;
end;


procedure fxCircularGrav(aGroup : TFxGroup; const point : TFxVector; const gravity, time : single);
begin
  fxCircularGrav(aGroup, point.x, point.y, point.z, gravity, time);
end;


procedure fxCircularGrav(aGroup : TFxGroup; const px, py, pz : single; const gravity, time : single);
var i : integer;
    gf : single;
    dlength : single;
    dirx, diry, dirz : single;
begin
  //CircularGravity pulls constantly to the given Point
  gf := gravity * time / 1000;//precalc
  for i := 0 to aGroup.HighestActive do if not aGroup.Particles[i].Junk then with aGroup.Particles[i] do begin
    //get direction
    dirx := px - Position.x;
    diry := py - Position.y;
    dirz := pz - Position.z;
    //get dir-length
    dlength := sqrt(dirx*dirx + diry*diry + dirz*dirz);
    if dlength = 0 then dlength := 0.0001;
    //v = v0 + a * t
    Velocity.x := Velocity.x + (dirx / dlength) * gf;
    Velocity.y := Velocity.y + (diry / dlength) * gf;
    Velocity.z := Velocity.z + (dirz / dlength) * gf;
  end;
end;


//*** fxPointGrav ***

procedure fxPointGrav(var aParticle : TFxParticle; const point : TFxVector; const minRadius, attrMass, time : single);
begin
  fxPointGrav(aParticle, point.x, point.y, point.z, minRadius, attrMass, time);
end;

procedure fxPointGrav(var aParticle : TFxParticle; const px, py, pz : single; const minRadius, attrMass, time : single);
var dlength, gf : single;
    dirx, diry, dirz : single;
    gravity : double;
begin
  gf := (time * attrMass) / 1000000;
  with aParticle do begin
    //get direction
    dirx := px - Position.x;
    diry := py - Position.y;
    dirz := pz - Position.z;
    //normalize vector
    dlength := sqrt(dirx*dirx + diry*diry + dirz*dirz);
    //calc Gravity (Newtons Law of Gravity)
    if dlength < minradius then exit;
    gravity := gf / (dlength*dlength*dlength);
    //v = v0 + a * t
    Velocity.x := Velocity.x + dirx * gravity;
    Velocity.y := Velocity.y + diry * gravity;
    Velocity.z := Velocity.z + dirz * gravity;
  end;
end;


procedure fxPointGrav(aGroup : TFxGroup;const point : TFxVector;const minradius, attrMass, time : single);
begin
  fxPointGrav(aGroup, point.x, point.y, point.z, minradius, attrMass, time);
end;


procedure fxPointGrav(aGroup : TFxGroup;const px, py, pz : single;const minradius, attrMass, time : single);
var i : integer;
    dlength, f : single;
    dirx, diry, dirz : single;
    gravity : double;
begin
  //PointGravity pulls to the given point. (F proportional 1/R^2)
  f := time / 1000000;
  for i := 0 to aGroup.HighestActive do if not aGroup.Particles[i].Junk then
  with aGroup.Particles[i] do begin
    //get direction
    dirx := px - Position.x;
    diry := py - Position.y;
    dirz := pz - Position.z;
    //normalize vector
    dlength := sqrt(dirx*dirx + diry*diry + dirz*dirz);
    //calc Gravity (Newtons Law of Gravity)
    if dlength < minradius then dlength := minradius;
    gravity := f * attrMass / (dlength*dlength*dlength);
    //v = v0 + a * t
    Velocity.x := Velocity.x + dirx * gravity;
    Velocity.y := Velocity.y + diry * gravity;
    Velocity.z := Velocity.z + dirz * gravity;
  end;
end;

//*** fxLineGrav

procedure fxLineGrav(var aParticle : TFxParticle; const p1, p2 : TFxVector; const minRadius, attrMass, time : single); overload;
var i,vLength : single;
    u,v : TfxVector;
begin
   //calculate the point on the line this particle is atracted to
   u.x := aParticle.Position.x - p1.x; //u = particle - p1;
   u.y := aParticle.Position.y - p1.y;
   u.z := aParticle.Position.z - p1.z;
   v.x := p2.x - p1.x; //v = p2 - p1;
   v.y := p2.y - p1.y;
   v.z := p2.z - p1.z;
   //length of v
   vLength := sqrt(sqr(v.x) + sqr(v.y) + sqr(v.z));
   //project u on a...
   //v dot u / |v|^2
   i := (v.x * u.x + v.y * u.y + v.z * u.z) / sqr(vLength);
   //pAtrr = p1 + i * v
   fxPointGrav(aParticle, p1.x + i*v.x, p1.y + i*v.y, p1.z + i*v.z, minRadius, attrMass, time);
end;

procedure fxLineGrav(var aParticle : TFxParticle; const p1x, p1y, p1z, p2x, p2y, p2z : single; const minRadius, attrMass, time : single); overload;
var i,vLength : single;
    u,v : TfxVector;
begin
   //calculate the point on the line this particle is atracted to
   u.x := aParticle.Position.x - p1x; //u = particle - p1;
   u.y := aParticle.Position.y - p1y;
   u.z := aParticle.Position.z - p1z;
   v.x := p2x - p1x; //v = p2 - p1;
   v.y := p2y - p1y;
   v.z := p2z - p1z;
   //length of v
   vLength := sqrt(sqr(v.x) + sqr(v.y) + sqr(v.z));
   //project u on a...
   //v dot u / |v|^2
   i := (v.x * u.x + v.y * u.y + v.z * u.z) / sqr(vLength);
   //pAtrr = p1 + i * v
   fxPointGrav(aParticle, p1x + i*v.x, p1y + i*v.y, p1z + i*v.z, minRadius, attrMass, time);
end;


procedure fxLineGrav(aGroup : TFxGroup; const p1, p2 : TFxVector; const minRadius, attrMass, time : single); overload;
var i : integer;
begin
  for i := 0 to aGroup.HighestActive do if not aGroup.Particles[i].Junk then
    fxLineGrav(aGroup.Particles[i],p1,p2,minRadius, attrMass, time);
end;

procedure fxLineGrav(aGroup : TFxGroup; const p1x, p1y, p1z, p2x, p2y, p2z : single; const minRadius, attrMass, time : single); overload;
var i : integer;
begin
   for i := 0 to aGroup.HighestActive do if not aGroup.Particles[i].Junk then
    fxLineGrav(aGroup.Particles[i],p1x, p1y, p1z, p2x, p2y, p2z, minRadius, attrMass, time);
end;

//*** fxParticleGrav

procedure fxParticleGrav(aGroup : TFxGroup;const time : single);
begin
  fxParticleGrav(aGroup, GRAV_MIN_RADIUS, time);
end;


procedure fxParticleGrav(aGroup : TFxGroup;const minradius, time : single);
var i, j : integer;
    dlength : single;
    dirx, diry, dirz : single;
    gravity, f : double;
begin
  //OPTIMISED BUT UN_UNDERSTANDABLE ;)
  //GlobalGravity pulls constantly in the given direction (parallel)
  f := time / 100000;
  with aGroup do begin
    for i := 0 to HighestActive do if not Particles[i].Junk then
     for j := 0 to HighestActive do if not Particles[j].Junk then begin
      //get direction
      dirx := Particles[j].Position.x - Particles[i].Position.x;
      diry := Particles[j].Position.y - Particles[i].Position.y;
      dirz := Particles[j].Position.z - Particles[i].Position.z;
      //get length
      dlength := sqrt(dirx*dirx + diry*diry + dirz*dirz);
      //calc Gravity (Newtons Law of Gravity)
      if dlength < minradius then dlength := minradius;
      gravity := Particles[j].mass * f / (dlength*dlength*dlength);

      Particles[i].Velocity.x := Particles[i].Velocity.x + dirx * gravity;
      Particles[i].Velocity.y := Particles[i].Velocity.y + diry * gravity;
      Particles[i].Velocity.z := Particles[i].Velocity.z + dirz * gravity;
    end;
  end;
end;


//*** fxFriction

procedure fxFriction(var aParticle : TFxParticle; const viscosity, time : single);
var k : single;
    speed, difspeed : single;
begin
  {Derivation:
   F = k * Volume * viscosity * |v|
   a = k * viscosity * |v| / density
   -> |v'| = k * viscosity * |v| * aTime / density
   -> |v| := |v| - |v'| }
  with aParticle do begin
   speed := sqrt(Velocity.x*Velocity.x + Velocity.y*Velocity.y + Velocity.z*Velocity.z); // |v|
   if speed = 0 then exit;
   difspeed := 18 * viscosity * speed * time / 1000 / density; //k ought to be about 6 * pi
   if difspeed < speed then k := (speed - difspeed) / speed else k := 0;
   Velocity.x := Velocity.x * k;
   Velocity.y := Velocity.y * k;
   Velocity.z := Velocity.z * k;
  end;
end;


procedure fxFriction(aGroup : TFxGroup; const viscosity, time : single);
var i               : integer;
    k               : single;
    speed, difspeed : single;
begin
  for i := 0 to aGroup.HighestActive do if not aGroup.Particles[i].Junk then with aGroup.Particles[i] do begin
   speed := sqrt(Velocity.x*Velocity.x + Velocity.y*Velocity.y + Velocity.z*Velocity.z); // |v|
   if speed = 0 then exit;
   difspeed := 18 * viscosity * speed * time / 1000 / density; //k ought to be about 6 * pi  = 18
   if difspeed < speed then k := (speed - difspeed) / speed else k := 0;
   Velocity.x := Velocity.x * k;
   Velocity.y := Velocity.y * k;
   Velocity.z := Velocity.z * k;
  end;
end;


//*** fxSimpleFlow

procedure fxSimpleFlow(var aParticle : TFxParticle; const direction : TFxVector; const viscosity, time : single);
begin
  fxSimpleFlow(aParticle, direction.x, direction.y, direction.z, viscosity, time);
end;


procedure fxSimpleFlow(var aParticle : TFxParticle; const dx, dy, dz : single; const viscosity, time : single);
var k : double;
    relVel : TFxVector;
    speed, difspeed : single;
begin
  {Derivation: see friction
   but v is a relative Velocity according to the surrounding
   so what i do:
   - calc relVel. (vrel = v - dir)
   - apply the friction
   - calc v' = vrel + dir
  }
  with aParticle do begin
   relVel.x := Velocity.x - dx;
   relVel.y := Velocity.y - dy;
   relVel.z := Velocity.z - dz;

   speed := sqrt(relVel.x*relVel.x + relVel.y*relVel.y + relVel.z*relVel.z); // |v|
   if speed = 0 then exit;
   difspeed := 18 * viscosity * speed * time / 1000 / density; //k ought to be about 6 * pi  = 18
   if difspeed < speed then k := (speed - difspeed) / speed else k := 0;
   relVel.x := relVel.x * k;
   relVel.y := relVel.y * k;
   relVel.z := relVel.z * k;

   Velocity.x := relVel.x + dx;
   Velocity.y := relVel.y + dy;
   Velocity.z := relVel.z + dz;
  end;
end;


procedure fxSimpleFlow(aGroup : TFxGroup; const direction : TFxVector; const viscosity, time : single);
begin
  fxSimpleFlow(aGroup, direction.x, direction.y, direction.z, viscosity, time);
end;


procedure fxSimpleFlow(aGroup : TFxGroup; const dx, dy, dz : single; const viscosity, time : single);
var i : integer;
    k : double;
    relVel : TFxVector;
    speed, difspeed : single;
begin
  for i := 0 to aGroup.HighestActive do if not aGroup.Particles[i].Junk then with aGroup.Particles[i] do begin
   relVel.x := Velocity.x - dx;
   relVel.y := Velocity.y - dy;
   relVel.z := Velocity.z - dz;

   speed := sqrt(relVel.x*relVel.x + relVel.y*relVel.y + relVel.z*relVel.z); // |v|
   if speed = 0 then exit;
   difspeed := 18 * viscosity * speed * time / 1000 / density; //k ought to be about 6 * pi  = 18
   if difspeed < speed then k := (speed - difspeed) / speed else k := 0;
   relVel.x := relVel.x * k;
   relVel.y := relVel.y * k;
   relVel.z := relVel.z * k;

   Velocity.x := relVel.x + dx;
   Velocity.y := relVel.y + dy;
   Velocity.z := relVel.z + dz;
  end;
end;

end.

