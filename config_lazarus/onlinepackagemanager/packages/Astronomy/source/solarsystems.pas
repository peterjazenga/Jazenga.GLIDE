{**********************************************************************
                PilotLogic Software House.
                   
Package pl_Astronomy 
This unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)

Initial source by Zozito Pelegrin

This is a library that implements a realistic simulator of planetary systems. 
You can design your own solar system, 
create custom planets or other objects and 
establish their gravitational relations using groups

***********************************************************************}

unit solarsystems;

interface

uses
  LCLIntf, LCLType, LMessages,
  SysUtils,
  Classes,
  Graphics,
  Forms,
  Math;

const

  G_GRAVITATIONAL_CONSTANT = 6.67384E-11;                        // N*m2/Kg2

  EARTH_MASS = 5.9726E24;                                        // kg
  EARTH_MEANDENSITY = 5514;                                      // kg/m3

  MOBJECT_SPEED_MAX = 300000 * 1000 - 0.001;                     // < 300.000 Km/s

  MOBJECT_INIT_COLOR = TColor(clBlack);
  MOBJECT_INIT_MASS = EARTH_MASS;
  MOBJECT_INIT_MEANDENSITY = EARTH_MEANDENSITY;

  MSYSTEM_INIT_PULSE_TIME = 1000;                                // 1 second
  MSYSTEM_INIT_USE_REAL_TIME = true;                             // use real time
  MSYSTEM_INIT_TIME_FACTOR = 1;                                  // no scale
  MSYSTEM_INIT_TIME_FACTOR_AUTO = false;                         // time factor is object count
  MSYSTEM_INIT_TIME_LIMIT = - 1;                                 // duration ( < 0.0 -> not limit)

  MSYSTEM_INIT_CHECKCOLLISIONS = true;
  MSYSTEM_INIT_PROCESSMESSAGES = true;
  MSYSTEM_INIT_FRAMERATE = 40;                                   // fps (frame rate)
  MSYSTEM_INIT_RESETSTATSONCOLLISION = true;

type

  munit = double; // precision type

  // random funcions
  function RandomByte(const AFromValue, AToValue: byte): byte;
  function RandomMunit(const AFromValue, AToValue: munit): munit; 

  // color functions
  procedure ColorToRgb(Color: TColor; var ValueRed, ValueGreen, ValueBlue: byte);
  function RgbToColor(const ValueRed, ValueGreen, ValueBlue: byte): TColor;
  function SetBrightColor(const Color: TColor; const BrightPercent: real): TColor;

type

  TMObjectPosition = class{(TObject)}
  private
    FX: munit; // global coordinate
    FY: munit; // global coordinate
    FZ: munit; // global coordinate
  public
    constructor Create; virtual;
    function DistanceTo(APosition: TMObjectPosition): munit;
    procedure Assign(ASource: TObject); virtual;
    procedure Clear;
    property X: munit read FX write FX;
    property Y: munit read FY write FY;
    property Z: munit read FZ write FZ;
  end;

  TMObjectVector = class{(TObject)}
  private
    FX: munit; // vector local coordinate
    FY: munit; // vector local coordinate
    FZ: munit; // vector local coordinate
    function GetAltitude: munit; // in radians
    function GetLatitude: munit; // in radians
    function GetSpeed: munit; // in m/s
  public
    constructor Create; virtual;
    procedure Assign(ASource: TObject); virtual;
    procedure Clear;
    property Altitude: munit read GetAltitude;
    property Latitude: munit read GetLatitude;
    property Speed: munit read GetSpeed;
    property X: munit read FX write FX;
    property Y: munit read FY write FY;
    property Z: munit read FZ write FZ;
  end;

  TMGroup = class; // forward

  TMObject = class{(TObject)}
  private
    FColor: TColor;
    FDensity: munit; // kg/m3
    FEnabled: boolean;
    FG: munit;
    FGroup: TMGroup;
    FId: cardinal;
    FMass: munit;        // kg
    FName: string;
    FPosition: TMObjectPosition;
    FRadius: munit;
    FVector: TMObjectVector;
    FVisible: boolean;
    FVolume: munit;
    procedure SetDensity(const ADensity: munit);
    procedure SetMass(const AMass: munit);
  protected
    procedure SetGroup(AGroup: TMGroup);
    procedure SetId(const AId: cardinal);
    procedure UpdateParams; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Color: TColor read FColor write FColor default MOBJECT_INIT_COLOR; //**//
    property Density: munit read FDensity write SetDensity;                     //**//
    property Enabled: boolean read FEnabled write FEnabled default true;
    property G: munit read FG;
    property Group: TMGroup read FGroup;
    property Id: cardinal read FId;
    property Mass: munit read FMass write SetMass;                              //**//
    property Name: string read FName write FName;                               //**//
    property Position: TMObjectPosition read FPosition;
    property Radius: munit read FRadius;
    property Vector: TMObjectVector read FVector;
    property Visible: boolean read FVisible write FVisible default true;
    property Volume: munit read FVolume;
  end;

  TMObjectMoveable = class(TMObject)
  private
    FAge: munit; // in seconds
    FDistance: munit; // in m
    FInducedVector: TMObjectVector;
    FNewPosition: TMObjectPosition;
    FPulseCount: cardinal;
    FPulseDistance: munit; // in m
    FPulseTime: munit; // in seconds
    FPulseVelocity: munit; // m/s
    FOldPosition: TMObjectPosition;
    FVelocityMax: munit; // m/s
    FVelocityMean: munit; // m/s
    FVelocityMin: munit; // m/s
  protected
    procedure AssignNewPosition(const APulseTime: munit); virtual;
    procedure ResetStats; virtual; 
  public
    constructor Create; override;
    destructor Destroy; override;
    property Age: munit read FAge;
    property Distance: munit read FDistance;
    property InducedVector: TMObjectVector read FInducedVector;
    property NewPosition: TMObjectPosition read FNewPosition;
    property PulseCount: cardinal read FPulseCount;
    property PulseDistance: munit read FPulseDistance;
    property PulseTime: munit read FPulseTime;
    property PulseVelocity: munit read FPulseVelocity;
    property OldPosition: TMObjectPosition read FOldPosition;
    property VelocityMax: munit read FVelocityMax;
    property VelocityMean: munit read FVelocityMean;
    property VelocityMin: munit read FVelocityMin;
  end;

  TMGroup = class{(TObject)}
  private
    FEnabled: boolean;
    FId: cardinal;
    FIdGen: cardinal;
    FList: TList; // of TMObjectMoveableClass instance
    FName: string;
    FVisible: boolean;
    function GetItem(const AIndex: integer): TMObjectMoveable;
    function GetObject(const AIndex: integer): TMObject;
  protected
    procedure SetId(const AId: cardinal);
    procedure Erase(const AIndex: integer);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Add: integer;
    function Count: integer;
    function IndexOf(const AId: cardinal): integer;
    procedure Clear;
    procedure Delete(const AIndex: integer);
    property Enabled: boolean read FEnabled write FEnabled default true;
    property Id: cardinal read FId;
    property Items[const AIndex: integer]: TMObjectMoveable read GetItem;
    property Objects[const AIndex: integer]: TMObject read GetObject; default;
    property Name: string read FName write FName;
    property Visible: boolean read FVisible write FVisible default true;
  end;

  TMGroups = class{(TObject)}
  private
    FIdGen: cardinal;
    FList: TList; // of TMGroup
    function GetGroup(const AIndex: integer): TMGroup;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Add: integer;
    function Count: integer;
    function IndexOf(const AId: cardinal): integer;
    procedure Clear;
    procedure Delete(const AIndex: integer);
    property Groups[const AIndex: integer]: TMGroup read GetGroup; default;
  end;

  TMGroupRelationType = (grtIndifferent, grtAttraction, grtRejection);

  TMGroupRelation = class{(TObject)}
  private
    FEnabled: boolean;
    FInfluenced: TMGroup;
    FInfluencing: TMGroup;
    FRelation: TMGroupRelationType;
  public
    constructor Create; virtual;
    property Enabled: boolean read FEnabled write FEnabled default true;
    property Influenced: TMGroup read FInfluenced write FInfluenced;
    property Influencing: TMGroup read FInfluencing write FInfluencing;
    property Relation: TMGroupRelationType read FRelation write FRelation;
  end;

  TMGroupRelations = class{(TObject)}
  private
    FGroups: TMGroups;
    FList: TList; // of TMGroupRelation
    function GetRelation(const AIndex: integer): TMGroupRelation;
  public
    constructor Create(AGroups: TMGroups); virtual;
    destructor Destroy; override;
    function Add(const AEnabled: boolean; AInfluenced, AInfluencing: TMGroup;
       const ARelation: TMGroupRelationType): integer;
    function Count: integer;
    function IndexOf(AInfluenced, AInfluencing: TMGroup): integer;
    procedure Clear;
    procedure Delete(const AIndex: integer);
    property Relations[const AIndex: integer]: TMGroupRelation read GetRelation; default;
  end;

  TObjectEvent = procedure(Sender: TObject; MObject: TMObject) of object;
  TObjectMoveableEvent = procedure(Sender: TObject; const ElapsedTime: munit;
     MObjectMoveable: TMObjectMoveable) of object;
  TObjectMoveablesEvent = procedure(Sender: TObject; const ElapsedTime: munit;
     MObjectMoveableA, MObjectMoveableB: TMObjectMoveable) of object;
  TObjectPulseEvent = procedure(Sender: TObject; const ALapTime: munit) of object;

  TMSystem = class{(TObject)}
  private
    FGroups: TMGroups;
    FGroupRelations: TMGroupRelations;
    FInPulse: boolean;
    function GetGroup(const AIndex: integer): TMGroup;
    function GetObject(const AIndexGroup, AIndex: integer): TMObject;
    function GetRelation(const AIndex: integer): TMGroupRelation;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function ObjectAdd(const AIndexGroup: integer; const AColor: TColor; const AName: string;
       const AEnabled, AVisible: boolean; const ADensity, AMass, APositionX, APositionY, APositionZ,
       AVectorX, AVectorY, AVectorZ: munit): integer;
    function ObjectCount: integer;
    procedure Clear;

  // group relations
    function AddRelation(const AEnabled: boolean; AInfluenced, AInfluencing: TMGroup; const ARelation: TMGroupRelationType): integer;
    function CountRelations: integer;
    function IndexOfRelation(AInfluenced, AInfluencing: TMGroup): integer;
    procedure ClearRelations;
    procedure DeleteRelation(const AIndex: integer);
    property Relations[const AIndex: integer]: TMGroupRelation read GetRelation; default;

  // groups
    function AddGroup: integer;
    function CountGroups: integer;
    function IndexOfGroup(const AId: cardinal): integer;
    procedure DeleteGroup(const AIndex: integer);
    property Groups[const AIndex: integer]: TMGroup read GetGroup;

  // objects
    function AddObject(const AIndexGroup: integer): integer;
    function CountObjects(const AIndexGroup: integer): integer;
    function IndexOfObject(const AIndexGroup: integer; const AId: cardinal): integer;
    procedure ClearObjects(const AIndexGroup: integer);
    procedure DeleteObject(const AIndexGroup, AIndex: integer);
    property Objects[const AIndexGroup, AIndex: integer]: TMObject read GetObject;

  // handling active, pulses and time
  private
    FActive: boolean;
    FElapsedTime: munit; // in seconds
    FFrameRate: byte; // frames per second
    FLastTick: cardinal;
    FLimitTime: munit;
    FOnProcessMessages: TNotifyEvent;
    FOnPulse: TObjectPulseEvent;
    FOnRenderBegin: TNotifyEvent;
    FOnRenderEnd: TNotifyEvent;
    FOnRenderObject: TObjectEvent;
    FOnTerminate: TNotifyEvent;
    FProcessMessages: boolean;
    FPulseCount: cardinal;
    FPulseTime: cardinal; // Time elapsed in every pulse (in milliseconds) if not FUseRealTime
    FTimeFactor: real;  // scale
    FTimeFactorAuto: boolean; // time factor is object count
    FUseRealTime: boolean;  // set whether uses real time or not
    procedure Render;
    procedure SetActive(const AActive: boolean);
    procedure SetUseRealTime(const AUseRealTime: boolean);
  protected
    procedure DoProcessMessages; virtual;
    procedure DoPulse; virtual;
    procedure DoRenderBegin; virtual;
    procedure DoRenderEnd; virtual;
    procedure DoRenderObject(AMObject: TMObject); virtual;
    procedure DoTerminate; virtual;
  public
    property Active: boolean read FActive write SetActive default false;
    property ElapsedTime: munit read FElapsedTime;
    property FrameRate: byte read FFrameRate write FFrameRate default MSYSTEM_INIT_FRAMERATE;
    property LimitTime: munit read FLimitTime write FLimitTime;
    property OnProcessMessages: TNotifyEvent read FOnProcessMessages write FOnProcessMessages;
    property OnPulse: TObjectPulseEvent read FOnPulse write FOnPulse;
    property OnRenderBegin: TNotifyEvent read FOnRenderBegin write FOnRenderBegin;
    property OnRenderEnd: TNotifyEvent read FOnRenderEnd write FOnRenderEnd;
    property OnRenderObject: TObjectEvent read FOnRenderObject write FOnRenderObject;
    property OnTerminate: TNotifyEvent read FOnTerminate write FOnTerminate;
    property ProcessMessages: boolean read FProcessMessages write FProcessMessages default MSYSTEM_INIT_PROCESSMESSAGES;
    property PulseCount: cardinal read FPulseCount;
    property PulseTime: cardinal read FPulseTime write FPulseTime default MSYSTEM_INIT_PULSE_TIME;
    property TimeFactor: real read FTimeFactor write FTimeFactor;
    property TimeFactorAuto: boolean read FTimeFactorAuto write FTimeFactorAuto default MSYSTEM_INIT_TIME_FACTOR_AUTO;
    property UseRealTime: boolean read FUseRealTime write SetUseRealTime default MSYSTEM_INIT_USE_REAL_TIME;

  // performing calcs
  private
    procedure CalcVector(AMObjectInfluenced, AMObjectInfluencing: TMObject;
       const AAtraction: boolean; var AObjectVectorX, AObjectVectorY, AObjectVectorZ: munit);
    procedure CalcList(AMObject: TMObject; var AObjectVectorX, AObjectVectorY, AObjectVectorZ: munit);
    procedure CalcObject(const AElapsedTime: munit; AMObjectMoveable: TMObjectMoveable);
    procedure PassCalcs(const AElapsedTime: munit);

  // performing updates & data event
  private
    FOnObjectData: TObjectMoveableEvent;
    procedure PassUpdates(const AElapsedTime: munit);
  protected
    procedure DoObjectData(const AElapsedTime: munit; AMObjectMoveable: TMObjectMoveable); virtual;
  public
    property OnObjectData: TObjectMoveableEvent read FOnObjectData write FOnObjectData;

  // collisions
  private
    FCheckCollisions: boolean;
    FOnCollision: TObjectMoveablesEvent;
    FResetStatsOnCollision: boolean;
    procedure CheckMObjectCollisions(const AElapsedTime: munit; AMObjectMoveable: TMObjectMoveable;
       const AGroupIndex, AMObjectMoveableIndex, AUpToGroupIndex, AUpToMObjectMoveableIndex: integer;
       var ACountCollisions, ACountObjectsErased: integer);
    function CheckMObjectsCollision(const AGroupIndexA, AMObjectMoveableIndexA: integer; AMObjectMoveableA: TMObjectMoveable;
       const AGroupIndexB, AMObjectMoveableIndexB: integer; AMObjectMoveableB: TMObjectMoveable): boolean;
    function MObjectToNil(const AGroupIndex, AMObjectMoveableIndex: integer): boolean;
    function PassCollisions(const AElapsedTime: munit): integer;
  protected
    procedure DoCollision(const AElapsedTime: munit; AMObjectMoveableA, AMObjectMoveableB: TMObjectMoveable); virtual;
  public
    procedure DoOnCollision(const AElapsedTime: munit; AMObjectMoveableA, AMObjectMoveableB: TMObjectMoveable);
    property CheckCollisions: boolean read FCheckCollisions write FCheckCollisions default MSYSTEM_INIT_CHECKCOLLISIONS;
    property OnCollision: TObjectMoveablesEvent read FOnCollision write FOnCollision;
    property ResetStatsOnCollision: boolean read FResetStatsOnCollision write FResetStatsOnCollision
       default MSYSTEM_INIT_RESETSTATSONCOLLISION;

  // random stuff
  public
    function AddRandomObject(const AIndexGroup: integer; const AName: string; const AEnabled, AVisible: boolean;
       const AFromColorR, AToColorR, AFromColorG, AToColorG, AFromColorB, AToColorB: byte;
       const AFromDensity, AToDensity, AFromMass, AToMass,
       AFromPositionX, AToPositionX, AFromPositionY, AToPositionY, AFromPositionZ, AToPositionZ,
       AFromVectorX, AToVectorX, AFromVectorY, AToVectorY, AFromVectorZ, AToVectorZ: munit): integer;
  end;

  TMObjectMoveableClass = class of TMObjectMoveable;

var

  MObjectMoveableClass: TMObjectMoveableClass;

implementation

//=========================================

function RandomByte(const AFromValue, AToValue: byte): byte;
var
   R: byte;
begin
if AToValue < AFromValue then
   raise Exception.Create('to value is less than from value');
R := AToValue - AFromValue + 1;
Result := AFromValue + random(R);
end;

function RandomMunit(const AFromValue, AToValue: munit): munit; // todo
var
   R: munit;
begin
if AToValue < AFromValue then
   raise Exception.Create('to value is less than from value');
R := AToValue - AFromValue;
Result := random * R;
Result := AFromValue + Result;
end;

procedure ColorToRgb(Color: TColor; var ValueRed, ValueGreen, ValueBlue: byte);
begin
ValueRed := Color and $0000FF;

Color := Color shr 8;
ValueGreen := Color and $0000FF;

Color := Color shr 8;
ValueBlue := Color and $0000FF;
end;

function RgbToColor(const ValueRed, ValueGreen, ValueBlue: byte): TColor;
begin
Result := TColor($000000);

Result := Result + TColor(ValueBlue);

Result := Result shl 8;
Result := result and $FFFF00;
Result := Result + TColor(ValueGreen);

Result := Result shl 8;
Result := result and $FFFF00;
Result := Result + TColor(ValueRed);
end;

function SetBrightColor(const Color: TColor; const BrightPercent: real): TColor;
var
   ValueBlue, ValueGreen, ValueRed: byte;
   RangeColor, Increment: byte;
begin
Result := Color;
if not(BrightPercent > 0) then Exit;

Result := clWhite;
if not(BrightPercent < 100) then Exit;

ColorToRgb(Color, ValueBlue, ValueGreen, ValueRed);

RangeColor := 255 - ValueBlue;
Increment := round(RangeColor * BrightPercent / 100);
ValueBlue := ValueBlue + Increment;

RangeColor := 255 - ValueGreen;
Increment := round(RangeColor * BrightPercent / 100);
ValueGreen := ValueGreen + Increment;

RangeColor := 255 - ValueRed;
Increment := round(RangeColor * BrightPercent / 100);
ValueRed := ValueRed + Increment;

Result := RgbToColor(ValueBlue, ValueGreen, ValueRed);
end;

//======================= TMObjectPosition ===============================

procedure TMObjectPosition.Assign(ASource: TObject);
var
   MObjectPosition: TMObjectPosition;
begin
if not Assigned(ASource) then
   begin
   Clear;
   Exit;
   end;

if ASource is TMObjectPosition then
   begin
   MObjectPosition := ASource as TMObjectPosition;
   if not(MObjectPosition = Self) then
      begin
      FX := MObjectPosition.X;
      FY := MObjectPosition.Y;
      FZ := MObjectPosition.Z;
      end;
   Exit;
   end;

if ASource is TMObject then
   begin
   Assign((ASource as TMObject).Position);
   Exit;
   end;

raise Exception.Create('assigned source object is not comptatible with TMObjectPosition: ' + ASource.ClassName);
end;

procedure TMObjectPosition.Clear;
begin
FX := 0.0;
FY := 0.0;
FZ := 0.0;
end;

constructor TMObjectPosition.Create;
begin
inherited Create;
Clear;
end;

function TMObjectPosition.DistanceTo(APosition: TMObjectPosition): munit;
var
   Vx, Vy, Vz: munit;
begin
Result := 0.0;
if not Assigned(APosition) then Exit;
if APosition = Self then Exit;

Vx := APosition.X - FX;
Vy := APosition.Y - FY;
Vz := APosition.Z - FZ;

Result := sqrt(sqr(Vx) + sqr(Vy));
Result := sqrt(sqr(Result) + sqr(Vz));
end;

//======================= TMObjectVector =================================

constructor TMObjectVector.Create;
begin
inherited Create;
Clear;
end;

function TMObjectVector.GetSpeed: munit;
begin
Result := sqrt(sqr(FX) + sqr(FY));
Result := sqrt(sqr(Result) + sqr(FZ));
if Result > MOBJECT_SPEED_MAX then
   Result := MOBJECT_SPEED_MAX;
end;

function TMObjectVector.GetAltitude: munit;
begin
Result := sqrt(sqr(FX) + sqr(FY));
Result := cos(FX / Result);
Result := arccos(Result);
if FX < 0 then
   if FY < 0 then
      Result := Result + Pi
   else
      Result := Pi - Result
else
   if FY < 0 then
      Result := 2 * Pi - Result
   else
      begin
      // nothing to do
      end
end;

function TMObjectVector.GetLatitude: munit;
begin
Result := sqrt(sqr(FX) + sqr(FZ));
Result := cos(FX / Result);
Result := arccos(Result);
if FX < 0 then
   if FZ < 0 then
      Result := Result + Pi
   else
      Result := Pi - Result
else
   if FZ < 0 then
      Result := 2 * Pi - Result
   else
      begin
      // nothing to do
      end
end;

procedure TMObjectVector.Clear;
begin
FX := 0.0;
FY := 0.0;
FZ := 0.0;
end;

procedure TMObjectVector.Assign(ASource: TObject);
var
   MObjectVector: TMObjectVector;
begin
if not Assigned(ASource) then
   begin
   Clear;
   Exit;
   end;

if ASource is TMObjectVector then
   begin
   MObjectVector := ASource as TMObjectVector;
   if not(MObjectVector = Self) then
      begin
      FX := MObjectVector.X;
      FY := MObjectVector.Y;
      FZ := MObjectVector.Z;
      end;
   Exit;
   end;

if ASource is TMObject then
   begin
   Assign((ASource as TMObject).Vector);
   Exit;
   end;

raise Exception.Create('assigned source object is not comptatible with TMObjectVector: ' + ASource.ClassName);
end;

//======================= TMObject ===========================

constructor TMObject.Create;
begin
inherited Create;
FColor := MOBJECT_INIT_COLOR;
FDensity := MOBJECT_INIT_MEANDENSITY;
FEnabled := true;
FMass := MOBJECT_INIT_MASS;
FName := '';
FPosition := TMObjectPosition.Create;
FVector := TMObjectVector.Create;
FVisible := true;
UpdateParams;
end;

destructor TMObject.Destroy;
begin
FPosition.Free;
FVector.Free;
inherited Destroy;
end;

procedure TMObject.SetDensity(const ADensity: munit);
begin
if ADensity < 0 then
   FDensity := 0
else
   FDensity := ADensity;
UpdateParams;
end;

procedure TMObject.SetGroup(AGroup: TMGroup);
begin
FGroup := AGroup;
end;

procedure TMObject.SetId(const AId: cardinal);
begin
FId := AId;
end;

procedure TMObject.SetMass(const AMass: munit);
begin
if AMass < 0 then
   FMass := 0
else
   FMass := AMass;
UpdateParams;
end;

procedure TMObject.UpdateParams;
const
  AThird = 1 / 3;
var
  r: munit;
begin
// get volume
FVolume := FMass / FDensity;

// get radius -> r ^ 3 = v * 3 / 4 * pi
r := (FVolume * 3) / (4 * pi);
FRadius := power(r, AThird);

{
get g
   g  = - G * M / r ^ 2 + w ^ 2 * r
   where:
        g is the acceleration of apparent gravity
        G is the gravitational constant (m3 s-2 kg-1)
        M is the mass (kg)
        r is the distance from that point to the center (m),
        w is the rotation speed (radian/s).
}
FG := (G_GRAVITATIONAL_CONSTANT * FMass) / sqr(r);
//FG := FG - (sqr(FRotationSpeed) * r);
end;

{ TMObjectMoveable }

procedure TMObjectMoveable.AssignNewPosition(const APulseTime: munit);
begin
// pulse
Inc(FPulseCount);

// position
FOldPosition.Assign(Position);
Position.Assign(FNewPosition);

// time
FPulseTime := APulseTime;
FAge := FAge + FPulseTime;

// distance
FPulseDistance := FOldPosition.DistanceTo(Position);
FDistance := FDistance + FPulseDistance;

// velocity
FPulseVelocity := FPulseDistance / FPulseTime;
if FPulseCount = 1 then
   begin
   FVelocityMax := FPulseVelocity;
   FVelocityMin := FPulseVelocity;
   end
else
   begin
   if FPulseVelocity > FVelocityMax then
      FVelocityMax := FPulseVelocity;
   if FPulseVelocity < FVelocityMin then
      FVelocityMin := FPulseVelocity;
   end;
FVelocityMean := FDistance / FAge;
end;

constructor TMObjectMoveable.Create;
begin
inherited Create;
FAge := 0;
FDistance := 0.0;
FInducedVector := TMObjectVector.Create;
FNewPosition := TMObjectPosition.Create;
FPulseCount := 0;
FPulseDistance := 0.0;
FPulseTime := 0.0;
FPulseVelocity := 0.0;
FOldPosition := TMObjectPosition.Create;
FVelocityMax := 0.0;
FVelocityMean := 0.0;
FVelocityMin := 0.0;
end;

destructor TMObjectMoveable.Destroy;
begin
FOldPosition.Free;
FNewPosition.Free;
FInducedVector.Free;
inherited Destroy;
end;

procedure TMObjectMoveable.ResetStats;
begin
FAge := 0.0;
FDistance := 0.0;
FPulseCount := 0;
FVelocityMax := 0.0;
FVelocityMin := 0.0;
end;

//======================= TMGroup ==============================

function TMGroup.Add: integer;
var
   MObjectMoveable: TMObjectMoveable;
begin
MObjectMoveable := MObjectMoveableClass.Create;
try
Inc(FIdGen);
MObjectMoveable.SetId(FIdGen);
MObjectMoveable.SetGroup(Self);
Result := FList.Add(MObjectMoveable);

   except
   MObjectMoveable.Free;
   raise;
   end;
end;

procedure TMGroup.Clear;
begin
while FList.Count > 0 do
   Delete(FList.Count - 1);
end;

function TMGroup.Count: integer;
begin
Result := FList.Count;
end;

constructor TMGroup.Create;
begin
inherited Create;
FEnabled := true;
FIdGen := 0;
FList := TList.Create;
FName := '';
FVisible := true;
end;

procedure TMGroup.Delete(const AIndex: integer);
begin
GetItem(AIndex).Free;
FList.Delete(AIndex);
end;

destructor TMGroup.Destroy;
begin
Clear;
FList.Free;
inherited Destroy;
end;

procedure TMGroup.Erase(const AIndex: integer);
begin
GetItem(AIndex).Free;
FList.Items[AIndex] := nil;
end;

function TMGroup.GetItem(const AIndex: integer): TMObjectMoveable;
begin
 Result := TMObjectMoveable(FList.Items[AIndex]);
end;

function TMGroup.GetObject(const AIndex: integer): TMObject;
begin
 Result := TMObject(FList.Items[AIndex]);
end;

function TMGroup.IndexOf(const AId: cardinal): integer;
var
   T, I: integer;
   MObject: TMObject;
begin
Result := - 1;
if not(AId > 0) then Exit;
T := FList.Count - 1;
for I := 0 to T do
   begin
   MObject := GetObject(I);
   if not Assigned(MObject) then Continue;
   if not(MObject.Id = AId) then Continue;
   Result := I;
   Break;
   end;
end;

procedure TMGroup.SetId(const AId: cardinal);
begin
FId := AId;
end;

//======================= TMGroups ====================================

function TMGroups.Add: integer;
var
   MGroup: TMGroup;
begin
MGroup := TMGroup.Create;
try
Inc(FIdGen);
MGroup.SetId(FIdGen);
Result := FList.Add(MGroup);

   except
   MGroup.Free;
   raise;
   end;
end;

procedure TMGroups.Clear;
begin
while FList.Count > 0 do
   Delete(FList.Count - 1);
end;

function TMGroups.Count: integer;
begin
Result := FList.Count;
end;

constructor TMGroups.Create;
begin
inherited Create;
FIdGen := 0;
FList := TList.Create;
end;

procedure TMGroups.Delete(const AIndex: integer);
begin
GetGroup(AIndex).Free;
FList.Delete(AIndex);
end;

destructor TMGroups.Destroy;
begin
Clear;
FList.Free;
inherited Destroy;
end;

function TMGroups.GetGroup(const AIndex: integer): TMGroup;
begin
Result := TMGroup(FList.Items[AIndex]);
end;

function TMGroups.IndexOf(const AId: cardinal): integer;
var
   T, I: integer;
   MGroup: TMGroup;
begin
Result := - 1;
if not(AId > 0) then Exit;
T := FList.Count - 1;
for I := 0 to T do
   begin
   MGroup := GetGroup(I);
   if not Assigned(MGroup) then Continue;
   if not(MGroup.Id = AId) then Continue;
   Result := I;
   Break;
   end;
end;

//======================= TMGroupRelation =====================

constructor TMGroupRelation.Create;
begin
 inherited Create;
 FEnabled := true;
end;

//======================= TMGroupRelations ====================

function TMGroupRelations.Add(const AEnabled: boolean; AInfluenced, AInfluencing: TMGroup;
  const ARelation: TMGroupRelationType): integer;
var
   Relation: TMGroupRelation;
begin
if not Assigned(AInfluenced) then
   raise Exception.Create('unassigned influenced group in relation');
if not Assigned(AInfluencing) then
   raise Exception.Create('unassigned influencing group in relation');

if not(IndexOf(AInfluenced, AInfluencing) < 0) then
   raise Exception.Create('relation exist');

Relation := TMGroupRelation.Create;
try
Relation.Enabled := AEnabled;
Relation.Influenced := AInfluenced;
Relation.Influencing := AInfluencing;
Relation.Relation := ARelation;
Result := FList.Add(Relation);

   except
   Relation.Free;
   raise;
   end;
end;

procedure TMGroupRelations.Clear;
begin
while FList.Count > 0 do
   Delete(FList.Count - 1);
end;

function TMGroupRelations.Count: integer;
begin
Result := FList.Count;
end;

constructor TMGroupRelations.Create(AGroups: TMGroups);
begin
inherited Create;
if not Assigned(AGroups) then
   raise Exception.Create('unassigned groups in the creation of relations object');
FGroups := AGroups;
FList := TList.Create;
end;

procedure TMGroupRelations.Delete(const AIndex: integer);
begin
GetRelation(AIndex).Free;
FList.Delete(AIndex);
end;

destructor TMGroupRelations.Destroy;
begin
Clear;
FList.Free;
inherited Destroy;
end;

function TMGroupRelations.GetRelation(const AIndex: integer): TMGroupRelation;
begin
Result := TMGroupRelation(FList.Items[AIndex]);
end;

function TMGroupRelations.IndexOf(AInfluenced, AInfluencing: TMGroup): integer;
var
   T, I: integer;
   Relation: TMGroupRelation;
begin
Result := - 1;

if not Assigned(AInfluenced) then Exit;
if not Assigned(AInfluencing) then Exit;

T := FList.Count - 1;
for I := 0 to T do
   begin
   Relation := GetRelation(I);
   if not Assigned(Relation) then Continue;
   if not(Relation.Influenced = AInfluenced) then Continue;
   if not(Relation.Influencing = AInfluencing) then Continue;
   Result := I;
   Break;
   end;
end;

//======================= TMSystem =================================

function TMSystem.AddGroup: integer;
begin
Result := FGroups.Add;
end;

function TMSystem.AddObject(const AIndexGroup: integer): integer;
begin
Result := FGroups[AIndexGroup].Add;
end;

function TMSystem.AddRandomObject(const AIndexGroup: integer; const AName: string; const AEnabled, AVisible: boolean;
  const AFromColorR, AToColorR, AFromColorG, AToColorG, AFromColorB, AToColorB: byte;
  const AFromDensity, AToDensity, AFromMass, AToMass,
  AFromPositionX, AToPositionX, AFromPositionY, AToPositionY, AFromPositionZ, AToPositionZ,
  AFromVectorX, AToVectorX, AFromVectorY, AToVectorY, AFromVectorZ, AToVectorZ: munit): integer;
var
   R, G, B: byte;
   Color: TColor;
   Density, Mass: munit;
   PositionX, PositionY, PositionZ: munit;
   VectorX, VectorY, VectorZ: munit;
begin
R := RandomByte(AFromColorR, AToColorR);
G := RandomByte(AFromColorG, AToColorG);
B := RandomByte(AFromColorB, AToColorB);
Color := RgbToColor(R, G, B);

Density := RandomMunit(AFromDensity, AToDensity);
Mass := RandomMunit(AFromMass, AToMass);

PositionX := RandomMunit(AFromPositionX, AToPositionX);
PositionY := RandomMunit(AFromPositionY, AToPositionY);
PositionZ := RandomMunit(AFromPositionZ, AToPositionZ);

VectorX := RandomMunit(AFromVectorX, AToVectorX);
VectorY := RandomMunit(AFromVectorY, AToVectorY);
VectorZ := RandomMunit(AFromVectorZ, AToVectorZ);

Result := ObjectAdd(AIndexGroup, Color, AName, AEnabled, AVisible, Density, Mass,
   PositionX, PositionY, PositionZ, VectorX, VectorY, VectorZ);
end;

function TMSystem.AddRelation(const AEnabled: boolean; AInfluenced, AInfluencing: TMGroup;
   const ARelation: TMGroupRelationType): integer;
begin
Result := FGroupRelations.Add(AEnabled, AInfluenced, AInfluencing, ARelation);
end;

procedure TMSystem.CalcList(AMObject: TMObject; var AObjectVectorX, AObjectVectorY, AObjectVectorZ: munit);
var
   GroupT, GroupI, I: integer;
   ObjectMoveableT, ObjectMoveableI: integer;
   Group: TMGroup;
   ObjectMoveable: TMObjectMoveable;
   GroupRelation: TMGroupRelation;
   Relation: TMGroupRelationType;
begin
GroupT := FGroups.Count - 1;
for GroupI := 0 to GroupT do
   begin
   Group := FGroups[GroupI];
   if not Assigned(Group) then Continue;
   if not Group.Enabled then Continue;
   I := FGroupRelations.IndexOf(AMObject.Group, Group);
   if (I < 0) then Continue; // no influence relation established
   GroupRelation := FGroupRelations[I];
   if not Assigned(GroupRelation) then Continue; // unassigned relation
   Relation := GroupRelation.Relation;
   if Relation = grtIndifferent then Continue; // relation is indifferent
   ObjectMoveableT := Group.Count - 1;
   for ObjectMoveableI := 0 to ObjectMoveableT do
      begin
      ObjectMoveable := Group.Items[ObjectMoveableI];
      if not Assigned(ObjectMoveable) then Continue;
      if not ObjectMoveable.Enabled then Continue;
      if ObjectMoveable = AMObject then Continue; // i dont influence to myself
      CalcVector(AMObject, ObjectMoveable, Relation = grtAttraction,
         AObjectVectorX, AObjectVectorY, AObjectVectorZ);
      end;
   end;
end;

procedure TMSystem.CalcObject(const AElapsedTime: munit; AMObjectMoveable: TMObjectMoveable);
var
   CalcVectorX, CalcVectorY, CalcVectorZ: munit; // calculated vector coordinates
   SpeedVectorX, SpeedVectorY, SpeedVectorZ: munit;
   m: munit;
begin
// get the induced force as a vector
CalcVectorX := 0.0;
CalcVectorY := 0.0;
CalcVectorZ := 0.0;
CalcList(AMObjectMoveable, CalcVectorX, CalcVectorY, CalcVectorZ);

// store vector data
AMObjectMoveable.InducedVector.X := CalcVectorX;
AMObjectMoveable.InducedVector.Y := CalcVectorY;
AMObjectMoveable.InducedVector.Z := CalcVectorZ;

// get m
m := AMObjectMoveable.Mass;
if m = 0 then m := 0.0000001; // to avoid div by zero error

// calculate new position by current speed vector: v := e/t -> e := v * t
AMObjectMoveable.NewPosition.X := AMObjectMoveable.Position.X + AMObjectMoveable.Vector.X * AElapsedTime;
AMObjectMoveable.NewPosition.Y := AMObjectMoveable.Position.Y + AMObjectMoveable.Vector.Y * AElapsedTime;
AMObjectMoveable.NewPosition.Z := AMObjectMoveable.Position.Z + AMObjectMoveable.Vector.Z * AElapsedTime;

// calculate new position by induced force: f = m * a -> a = f / m
SpeedVectorX := CalcVectorX / m;
AMObjectMoveable.NewPosition.X := AMObjectMoveable.NewPosition.X + SpeedVectorX / 2 * AElapsedTime; // Speed / 2 -> is the average
SpeedVectorY := CalcVectorY / m;
AMObjectMoveable.NewPosition.Y := AMObjectMoveable.NewPosition.Y + SpeedVectorY / 2 * AElapsedTime; // Speed / 2 -> is the average
SpeedVectorZ := CalcVectorZ / m;
AMObjectMoveable.NewPosition.Z := AMObjectMoveable.NewPosition.Z + SpeedVectorZ / 2 * AElapsedTime; // Speed / 2 -> is the average

// update curent vector
AMObjectMoveable.Vector.X := AMObjectMoveable.Vector.X + SpeedVectorX * AElapsedTime;
AMObjectMoveable.Vector.Y := AMObjectMoveable.Vector.Y + SpeedVectorY * AElapsedTime;
AMObjectMoveable.Vector.Z := AMObjectMoveable.Vector.Z + SpeedVectorZ * AElapsedTime;
end;

procedure TMSystem.CalcVector(AMObjectInfluenced, AMObjectInfluencing: TMObject;
  const AAtraction: boolean; var AObjectVectorX, AObjectVectorY, AObjectVectorZ: munit);
var
   F, m1, m2, r, u, v: munit;
   Vx, Vy, Vz: munit;
   VectorX, VectorY, VectorZ: munit;
begin
// get object's masses
m1 := AMObjectInfluenced.Mass;
m2 := AMObjectInfluencing.Mass;

// get object's distance
Vx := AMObjectInfluencing.Position.X - AMObjectInfluenced.Position.X;
Vy := AMObjectInfluencing.Position.Y - AMObjectInfluenced.Position.Y;
Vz := AMObjectInfluencing.Position.Z - AMObjectInfluenced.Position.Z;
u := sqrt(sqr(Vx) + sqr(Vy));
r := sqrt(sqr(u) + sqr(Vz));
if u = 0 then u := 0.0000001; // to avoid div by zero error
if r = 0 then r := 0.0000001; // to avoid div by zero error

// get force between the masses: F = G * ((m1 * m2) / r^2)
F := G_GRAVITATIONAL_CONSTANT * ((m1 * m2) / sqr(r)); // F in newton (vector module)

// get vector as coordinates

// if   r ----- now is a --- F
//then  Vz --- will be ---- VectorZ
VectorZ := Vz * F / r;
// if   r ----- now is a --- F
// then u ----- will be --- v
v := u * F / r;
// if   u ----- now is a --- Vy
// then v ----- will be ---- VectorY
VectorY := v * Vy / u;
// if   u ----- now is a --- Vx
// then v ----- will be ---- VectorX
VectorX := v * Vx / u;

// add to main vector
if AAtraction then
   begin
   AObjectVectorX := AObjectVectorX + VectorX;
   AObjectVectorY := AObjectVectorY + VectorY;
   AObjectVectorZ := AObjectVectorZ + VectorZ;
   end
else
   begin
   AObjectVectorX := AObjectVectorX - VectorX;
   AObjectVectorY := AObjectVectorY - VectorY;
   AObjectVectorZ := AObjectVectorZ - VectorZ;
   end;
end;

procedure TMSystem.CheckMObjectCollisions(const AElapsedTime: munit; AMObjectMoveable: TMObjectMoveable;
  const AGroupIndex, AMObjectMoveableIndex, AUpToGroupIndex, AUpToMObjectMoveableIndex: integer;
  var ACountCollisions, ACountObjectsErased: integer);
var
   Done: boolean;
   GroupT, GroupI: integer;
   ObjectMoveableT, ObjectMoveableI: integer;
   Group: TMGroup;
   ObjectMoveable: TMObjectMoveable;
   RadiusA, RadiusB: munit;
   CountCollisions, CountObjectsErased: integer;
begin
ACountCollisions := 0;
ACountObjectsErased := 0;
GroupT := FGroups.Count - 1;
for GroupI := 0 to GroupT do
   begin
   Done := GroupI > AUpToGroupIndex;
   if Done then Break;
   Group := FGroups[GroupI];
   if not Assigned(Group) then Continue;
   if not Group.Enabled then Continue;
   ObjectMoveableT := Group.Count - 1;
   for ObjectMoveableI := 0 to ObjectMoveableT do
      begin
      Done := (GroupI = AUpToGroupIndex) and (ObjectMoveableI = AUpToMObjectMoveableIndex);
      if Done then Break;
      ObjectMoveable := Group.Items[ObjectMoveableI];
      if not Assigned(ObjectMoveable) then Continue;
      if not ObjectMoveable.Enabled then Continue;
      if not CheckMObjectsCollision(AGroupIndex, AMObjectMoveableIndex, AMObjectMoveable,
         GroupI, ObjectMoveableI, ObjectMoveable) then Continue;

      // collision detected //
      Inc(ACountCollisions);

      // get radius before event
      RadiusA := AMObjectMoveable.Radius;
      RadiusB := ObjectMoveable.Radius;

      // fire event
      DoCollision(AElapsedTime, AMObjectMoveable, ObjectMoveable);

      // look referenced object new radius
      if ObjectMoveable.Radius > 1 then
         begin
         if ObjectMoveable.Radius > RadiusB then
            begin
            // referenced object increases radius, re-check up to current
            CheckMObjectCollisions(AElapsedTime, ObjectMoveable, GroupI, ObjectMoveableI,
               AUpToGroupIndex, AUpToMObjectMoveableIndex, CountCollisions, CountObjectsErased);
            ACountCollisions := ACountCollisions + CountCollisions;
            ACountObjectsErased := ACountObjectsErased + CountObjectsErased;
            end
         end
      else
         // referenced object is erased
         if MObjectToNil(GroupI, ObjectMoveableI) then
            Inc(ACountObjectsErased);

      // read AMObjectMoveable again, because it can have been erased
      AMObjectMoveable := FGroups[AGroupIndex].Items[AMObjectMoveableIndex];
      if not Assigned(AMObjectMoveable) then
         begin
         Done := true;
         Break;
         end;

      // look current object new radius
      if AMObjectMoveable.Radius > 1 then
         begin
         if AMObjectMoveable.Radius > RadiusA then
            begin
            // current object increases radius, check again
            CheckMObjectCollisions(AElapsedTime, AMObjectMoveable, AGroupIndex, AMObjectMoveableIndex,
               AUpToGroupIndex, AUpToMObjectMoveableIndex, CountCollisions, CountObjectsErased);
            ACountCollisions := ACountCollisions + CountCollisions;
            ACountObjectsErased := ACountObjectsErased + CountObjectsErased;
            Done := true;
            end
         end
      else
         begin
         // current object is erased
         AMObjectMoveable := nil;
         if MObjectToNil(AGroupIndex, AMObjectMoveableIndex) then
            Inc(ACountObjectsErased);
         Done := true;
         end;

      if Done then break;
      end;

   if Done then Break;
   end;
end;

function TMSystem.CheckMObjectsCollision(const AGroupIndexA, AMObjectMoveableIndexA: integer; AMObjectMoveableA: TMObjectMoveable;
   const AGroupIndexB, AMObjectMoveableIndexB: integer; AMObjectMoveableB: TMObjectMoveable): boolean; // private
var
   Distance, CollisionDistance: munit;
begin
Result := false;
// i do not collision with myself
if (AGroupIndexA = AGroupIndexB) and
   (AMObjectMoveableIndexA = AMObjectMoveableIndexB) then Exit;

// get current collision distance
CollisionDistance := AMObjectMoveableA.Radius + AMObjectMoveableB.Radius;

// look distance in x-axis (optimization)
Distance := abs(AMObjectMoveableA.Position.X - AMObjectMoveableB.Position.X);
if Distance > CollisionDistance then Exit;

// look distance in y-axis (optimization)
Distance := abs(AMObjectMoveableA.Position.Y - AMObjectMoveableB.Position.Y);
if Distance > CollisionDistance then Exit;

// look distance in z-axis (optimization)
Distance := abs(AMObjectMoveableA.Position.Z - AMObjectMoveableB.Position.Z);
if Distance > CollisionDistance then Exit;

// get real distance and compare
Distance := AMObjectMoveableA.Position.DistanceTo(AMObjectMoveableB.Position);
Result := not(Distance > CollisionDistance);
end;

procedure TMSystem.Clear;
begin
FActive := false;
FElapsedTime := 0.0;
FGroupRelations.Clear;
FGroups.Clear;
end;

procedure TMSystem.ClearObjects(const AIndexGroup: integer);
begin
FGroups[AIndexGroup].Clear;
end;

procedure TMSystem.ClearRelations;
begin
FGroupRelations.Clear;
end;

function TMSystem.CountGroups: integer;
begin
Result := FGroups.Count;
end;

function TMSystem.CountObjects(const AIndexGroup: integer): integer;
begin
Result := FGroups[AIndexGroup].Count;
end;

function TMSystem.CountRelations: integer;
begin
Result := FGroupRelations.Count;
end;

constructor TMSystem.Create;
begin
FActive := false;
FCheckCollisions := MSYSTEM_INIT_CHECKCOLLISIONS;
FFrameRate := MSYSTEM_INIT_FRAMERATE;
FGroups := TMGroups.Create;
FGroupRelations := TMGroupRelations.Create(FGroups);
FInPulse := false;
FLimitTime := MSYSTEM_INIT_TIME_LIMIT;
FProcessMessages := MSYSTEM_INIT_PROCESSMESSAGES;
FPulseTime := MSYSTEM_INIT_PULSE_TIME;
FResetStatsOnCollision := MSYSTEM_INIT_RESETSTATSONCOLLISION;
FTimeFactor := MSYSTEM_INIT_TIME_FACTOR;
FTimeFactorAuto := MSYSTEM_INIT_TIME_FACTOR_AUTO;
FUseRealTime := MSYSTEM_INIT_USE_REAL_TIME;
end;

procedure TMSystem.DeleteGroup(const AIndex: integer);
var
   DelIt: boolean;
   I: integer;
   Group: TMGroup;
   Relation: TMGroupRelation;
begin
Group := FGroups[AIndex];

if not Assigned(Group) then
   begin
   FGroups.Delete(AIndex);
   Exit;
   end;

for I := FGroupRelations.Count - 1 downto 0 do
   begin
   Relation := FGroupRelations[I];
   DelIt := not Assigned(Relation);
   DelIt := DelIt or (Relation.Influenced = Group);
   DelIt := DelIt or (Relation.Influencing = Group);
   if DelIt then
      FGroupRelations.Delete(I);
   end;

FGroups.Delete(AIndex);
end;

procedure TMSystem.DeleteObject(const AIndexGroup, AIndex: integer);
begin
FGroups[AIndexGroup].Delete(AIndex);
end;

procedure TMSystem.DeleteRelation(const AIndex: integer);
begin
FGroupRelations.Delete(AIndex);
end;

destructor TMSystem.Destroy;
begin
FGroupRelations.Free;
FGroups.Free;
inherited Destroy;
end;

procedure TMSystem.DoCollision(const AElapsedTime: munit; AMObjectMoveableA, AMObjectMoveableB: TMObjectMoveable);
begin
if Assigned(FOnCollision) then
   FOnCollision(Self, AElapsedTime, AMObjectMoveableA, AMObjectMoveableB)
else
   DoOnCollision(AElapsedTime, AMObjectMoveableA, AMObjectMoveableB);
end;

procedure TMSystem.DoObjectData(const AElapsedTime: munit; AMObjectMoveable: TMObjectMoveable);
begin
if not Assigned(FOnObjectData) then Exit;
FOnObjectData(Self, AElapsedTime, AMObjectMoveable);
end;

procedure TMSystem.DoOnCollision(const AElapsedTime: munit;
  AMObjectMoveableA, AMObjectMoveableB: TMObjectMoveable);
var
   DeleteA: boolean;
   M, D, X, Y, Z, VX, VY, VZ, V: munit;
   Name: string;
   MObjectMoveable: TMObjectMoveable;
begin
// in this case the less mass object merges with the more mass object

// get the merged object
DeleteA := AMObjectMoveableB.Mass > AMObjectMoveableA.Mass;

// get the sum of masses
M := AMObjectMoveableA.Mass + AMObjectMoveableB.Mass;

// get the new density
D := M / (AMObjectMoveableA.Volume + AMObjectMoveableB.Volume);

// get the new position of the big mass object
X := AMObjectMoveableB.Position.X - AMObjectMoveableA.Position.X;
Y := AMObjectMoveableB.Position.Y - AMObjectMoveableA.Position.Y;
Z := AMObjectMoveableB.Position.Z - AMObjectMoveableA.Position.Z;

// a.mass + b.mass ........... X
// a.mass          ........... v
V := AMObjectMoveableA.Mass * X / M;
V := X - V;
X := AMObjectMoveableA.Position.X + V;

// a.mass + b.mass ........... Y
// a.mass          ........... v
V := AMObjectMoveableA.Mass * Y / M;
V := Y - V;
Y := AMObjectMoveableA.Position.Y + V;

// a.mass + b.mass ........... Z
// a.mass          ........... v
V := AMObjectMoveableA.Mass * Z / M;
V := Z - V;
Z := AMObjectMoveableA.Position.Z + V;

// get the new vector of the big mass object
VX := (AMObjectMoveableA.Mass * AMObjectMoveableA.Vector.X) + (AMObjectMoveableB.Mass * AMObjectMoveableB.Vector.X);
VX := VX / M;

VY := (AMObjectMoveableA.Mass * AMObjectMoveableA.Vector.Y) + (AMObjectMoveableB.Mass * AMObjectMoveableB.Vector.Y);
VY := VY / M;

VZ := (AMObjectMoveableA.Mass * AMObjectMoveableA.Vector.Z) + (AMObjectMoveableB.Mass * AMObjectMoveableB.Vector.Z);
VZ := VZ / M;

// get the new name
if DeleteA then
   Name := AMObjectMoveableB.Name + '+' + AMObjectMoveableA.Name
else
   Name := AMObjectMoveableA.Name + '+' + AMObjectMoveableB.Name;

// erase object
if DeleteA then
   begin
   AMObjectMoveableA.Mass := 0.0;
   if FResetStatsOnCollision then
      AMObjectMoveableB.ResetStats;
   end
else
   begin
   if FResetStatsOnCollision then
      AMObjectMoveableA.ResetStats;
   AMObjectMoveableB.Mass := 0.0;
   end;

// assign new values
if DeleteA then
   MObjectMoveable := AMObjectMoveableB
else
   MObjectMoveable := AMObjectMoveableA;

MObjectMoveable.Mass := M;
MObjectMoveable.Name := Name;
MObjectMoveable.Density := D;
MObjectMoveable.Position.X := X;
MObjectMoveable.Position.Y := Y;
MObjectMoveable.Position.Z := Z;
MObjectMoveable.Vector.X := VX;
MObjectMoveable.Vector.Y := VY;
MObjectMoveable.Vector.Z := VZ;
end;

procedure TMSystem.DoProcessMessages;
begin
if not Assigned(FOnProcessMessages) then Exit;
FOnProcessMessages(Self);
end;

procedure TMSystem.DoPulse;
var
   TimeEnd: boolean; // whether reached time limit
   LapTime, CurTick: cardinal; // in milliseconds
   TimeLap: munit; // in seconds
begin
if FInPulse then Exit;
FInPulse := true;
try
// check active
if not FActive then Exit;

// update pulse count
Inc(FPulseCount);

// get current tick
CurTick := GetTickCount;

// get elapsed time
if FUseRealTime then
   LapTime := CurTick - FLastTick
else
   LapTime := FPulseTime;

// update last tick
FLastTick := CurTick;

// apply factor
if FTimeFactorAuto and not FUseRealTime then
   TimeLap := cardinal(ObjectCount) * LapTime / 1000
else
   TimeLap := FTimeFactor * LapTime / 1000;
if not(TimeLap > 0) then Exit;

// check end of time
TimeEnd := false;
if not(FLimitTime < 0.0) and (FElapsedTime + TimeLap > FLimitTime) then
   begin
   TimeLap := FLimitTime - FElapsedTime;
   if TimeLap > 0 then
      TimeEnd := true
   else
      begin
      DoTerminate;
      Exit;
      end;
   end;

// update elpased time
FElapsedTime := FElapsedTime + TimeLap;

// calculate new possitions, diameters and colors
PassCalcs(TimeLap);

// update objects properties
PassUpdates(TimeLap);

// check object collisions
PassCollisions(TimeLap);

// fire pulse event
if Assigned(FOnPulse) then
   FOnPulse(Self, TimeLap);

if TimeEnd then
   DoTerminate;

   finally
   FInPulse := false;
   end;
end;

procedure TMSystem.DoRenderBegin;
begin
if not Assigned(FOnRenderBegin) then Exit;
FOnRenderBegin(Self);
end;

procedure TMSystem.DoRenderEnd;
begin
if not Assigned(FOnRenderEnd) then Exit;
FOnRenderEnd(Self);
end;

procedure TMSystem.DoRenderObject(AMObject: TMObject);
begin
if not Assigned(FOnRenderObject) then Exit;
FOnRenderObject(Self, AMObject);
end;

procedure TMSystem.DoTerminate;
begin
FActive := false;
if not Assigned(FOnTerminate) then Exit;
FOnTerminate(Self);
end;

function TMSystem.GetGroup(const AIndex: integer): TMGroup;
begin
Result := FGroups[AIndex];
end;

function TMSystem.GetObject(const AIndexGroup, AIndex: integer): TMObject;
begin
Result := FGroups[AIndexGroup][AIndex];
end;

function TMSystem.GetRelation(const AIndex: integer): TMGroupRelation;
begin
Result := FGroupRelations[AIndex];
end;

function TMSystem.IndexOfGroup(const AId: cardinal): integer;
begin
Result := FGroups.IndexOf(AId);
end;

function TMSystem.IndexOfObject(const AIndexGroup: integer; const AId: cardinal): integer;
begin
Result := FGroups[AIndexGroup].IndexOf(AId);
end;

function TMSystem.IndexOfRelation(AInfluenced, AInfluencing: TMGroup): integer;
begin
Result := FGroupRelations.IndexOf(AInfluenced, AInfluencing);
end;

function TMSystem.MObjectToNil(const AGroupIndex, AMObjectMoveableIndex: integer): boolean;
var
   MGroup: TMGroup;
   MObject: TMObject;
begin
Result := false;
if AGroupIndex < 0 then Exit;
if not(AGroupIndex < FGroups.Count) then Exit;
MGroup := FGroups[AGroupIndex];
if not Assigned(MGroup) then Exit;

if AMObjectMoveableIndex < 0 then Exit;
if not(AMObjectMoveableIndex < MGroup.Count) then Exit;
MObject := MGroup.Objects[AMObjectMoveableIndex];
if not Assigned(MObject) then Exit;

MGroup.Erase(AMObjectMoveableIndex);
Result := true;
end;

function TMSystem.ObjectAdd(const AIndexGroup: integer; const AColor: TColor; const AName: string;
  const AEnabled, AVisible: boolean; const ADensity, AMass, APositionX, APositionY,
  APositionZ, AVectorX, AVectorY, AVectorZ: munit): integer;
var
   MObject: TMObject;
   MGroup: TMGroup;
begin
if AIndexGroup < 0 then
   raise Exception.Create('index group less than zero');
if not(AIndexGroup < FGroups.Count) then
   raise Exception.Create('index group out of bounds');

MGroup := FGroups[AIndexGroup];
if not Assigned(MGroup) then
   raise Exception.Create('unassigned group: ' + IntToStr(AIndexGroup));

Result := MGroup.Add;
MObject := MGroup.Objects[Result];
if not Assigned(MObject) then
   raise Exception.Create('failed to create object: ' + IntToStr(Result));

MObject.Color := AColor;
MObject.Name := AName;
MObject.Enabled := AEnabled;
MObject.Visible := AVisible;
MObject.Density := ADensity;
MObject.Mass := AMass;
MObject.Position.X := APositionX;
MObject.Position.Y := APositionY;
MObject.Position.Z := APositionZ;
MObject.Vector.X := AVectorX;
MObject.Vector.Y := AVectorY;
MObject.Vector.Z := AVectorZ;
end;

function TMSystem.ObjectCount: integer;
var
   GroupT, GroupI: integer;
   Group: TMGroup;
begin
Result := 0;
GroupT := FGroups.Count - 1;
for GroupI := 0 to GroupT do
   begin
   Group := FGroups[GroupI];
   if not Assigned(Group) then Continue;
   Result := Result + Group.Count;
   end;
end;

procedure TMSystem.PassCalcs(const AElapsedTime: munit);
var
   GroupT, GroupI: integer;
   ObjectMoveableT, ObjectMoveableI: integer;
   Group: TMGroup;
   ObjectMoveable: TMObjectMoveable;
begin
GroupT := FGroups.Count - 1;
for GroupI := 0 to GroupT do
   begin
   Group := FGroups[GroupI];
   if not Assigned(Group) then Continue;
   if not Group.Enabled then Continue;
   ObjectMoveableT := Group.Count - 1;
   for ObjectMoveableI := 0 to ObjectMoveableT do
      begin
      ObjectMoveable := Group.Items[ObjectMoveableI];
      if not Assigned(ObjectMoveable) then Continue;
      if not ObjectMoveable.Enabled then Continue;
      CalcObject(AElapsedTime, ObjectMoveable);
      end;
   end;
end;

function TMSystem.PassCollisions(const AElapsedTime: munit): integer;
var
   GroupT, GroupI: integer;
   ObjectMoveableT, ObjectMoveableI: integer;
   Group: TMGroup;
   ObjectMoveable: TMObjectMoveable;
   CountObjectsErased, C, O: integer;
begin
Result := 0;
if not FCheckCollisions then Exit;
CountObjectsErased := 0;
GroupT := FGroups.Count - 1;
for GroupI := 0 to GroupT do
   begin
   Group := FGroups[GroupI];
   if not Assigned(Group) then Continue;
   if not Group.Enabled then Continue;
   ObjectMoveableI := - 1;
   while true do
      begin
      Inc(ObjectMoveableI);
      if not(ObjectMoveableI < Group.Count) then Break;
      ObjectMoveable := Group.Items[ObjectMoveableI];
      if not Assigned(ObjectMoveable) then Continue;
      if not ObjectMoveable.Enabled then Continue;
      CheckMObjectCollisions(AElapsedTime, ObjectMoveable, GroupI, ObjectMoveableI,
         GroupI, ObjectMoveableI, C, O);
      Result := Result + C;
      CountObjectsErased := CountObjectsErased + O;
      end;
   end;

if not(CountObjectsErased > 0) then Exit;

// clear unassigned objects
for GroupI := GroupT downto 0 do
   begin
   Group := FGroups[GroupI];
   if not Assigned(Group) then
      begin
      DeleteGroup(GroupI);
      Continue;
      end;
   ObjectMoveableT := Group.Count - 1;
   for ObjectMoveableI := ObjectMoveableT downto 0 do
      begin
      ObjectMoveable := Group.Items[ObjectMoveableI];
      if Assigned(ObjectMoveable) then Continue;
      Group.Delete(ObjectMoveableI);
      end;
   end;
end;

procedure TMSystem.PassUpdates(const AElapsedTime: munit);
var
   GroupT, GroupI: integer;
   ObjectMoveableT, ObjectMoveableI: integer;
   Group: TMGroup;
   ObjectMoveable: TMObjectMoveable;
begin
GroupT := FGroups.Count - 1;
for GroupI := 0 to GroupT do
   begin
   Group := FGroups[GroupI];
   if not Assigned(Group) then Continue;
   if not Group.Enabled then Continue;
   ObjectMoveableT := Group.Count - 1;
   for ObjectMoveableI := 0 to ObjectMoveableT do
      begin
      ObjectMoveable := Group.Items[ObjectMoveableI];
      if not Assigned(ObjectMoveable) then Continue;
      if not ObjectMoveable.Enabled then Continue;
      DoObjectData(AElapsedTime, ObjectMoveable);
      ObjectMoveable.AssignNewPosition(AElapsedTime);
      end;
   end;
end;

procedure TMSystem.Render;
var
   GroupT, GroupI: integer;
   ObjectMoveableT, ObjectMoveableI: integer;
   Group: TMGroup;
   ObjectMoveable: TMObjectMoveable;
begin
DoRenderBegin;
try
GroupT := FGroups.Count - 1;
for GroupI := 0 to GroupT do
   begin
   Group := FGroups[GroupI];
   if not Assigned(Group) then Continue;
   if not Group.Enabled then Continue;
   if not Group.Visible then Continue;
   ObjectMoveableT := Group.Count - 1;
   for ObjectMoveableI := 0 to ObjectMoveableT do
      begin
      ObjectMoveable := Group.Items[ObjectMoveableI];
      if not Assigned(ObjectMoveable) then Continue;
      if not ObjectMoveable.Enabled then Continue;
      if not ObjectMoveable.Visible then Continue;
      DoRenderObject(ObjectMoveable);
      end;
   end;

   finally
   DoRenderEnd;
   end;
end;

procedure TMSystem.SetActive(const AActive: boolean);
var
   TickNow, TickLastRender: cardinal;
begin
if AActive = FActive then Exit;
FActive := AActive;
if not FActive then Exit;
FElapsedTime := 0.0;
FPulseCount := 0;
FLastTick := GetTickCount;

// loop
TickLastRender := 0;
while true do
   begin
   if FProcessMessages then
      DoProcessMessages;
   DoPulse;
   if not FActive then
      begin
      Render;
      Break;
      end;
   TickNow := GetTickCount;
   if (FFrameRate > 0) and (TickNow - TickLastRender < 1000 / FFrameRate) then Continue;
   Render;
   TickLastRender := TickNow;
   end;
end;

procedure TMSystem.SetUseRealTime(const AUseRealTime: boolean);
begin
FUseRealTime := AUseRealTime;
FLastTick := GetTickCount;
end;

initialization

randomize;

MObjectMoveableClass := TMObjectMoveable;

end.
