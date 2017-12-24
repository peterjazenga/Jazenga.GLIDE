{**********************************************************************
                PilotLogic Software House.
  
Package pl_Vulkan
this unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit vulkanutils;

{$mode objfpc}{$H+}

{$I vulkanapi.inc}

interface

uses
  Classes, SysUtils, math,
  vulkanapi;

type   
  VkUint8   = Byte;
  VkUint16  = Word;
  VkUint32  = Cardinal;
  VkUint64  = QWord;
  VkInt16   = SmallInt;
  VkInt32   = Integer;
  VkInt64   = Int64;
  VkFloat   = Single;
  VkDouble  = Double;
  VkChar    = AnsiChar;
  VkByte    = Byte;
  VkSize    = SizeUInt;
  VkHandle  = Pointer;
  VkNonDispatchableHandle   = {$IFDEF VK_CPU64}Pointer;{$ELSE}VkUint64;{$ENDIF}
  VKenum    = VkUint32;
  VKint     = Integer;

  TvkuVectorub2 = array[0..1] of VkByte;
  TvkuVectori2  = array[0..1] of VKint;
  TvkuVectorf2  = array[0..1] of VKfloat;
  TvkuVectord2  = array[0..1] of VKdouble;
  TvkuVectorp2  = array[0..1] of Pointer;

  TvkuVectorub3 = array[0..2] of VkByte;
  TvkuVectori3  = array[0..2] of VKint;
  TvkuVectorf3  = array[0..2] of VKfloat;
  TvkuVectord3  = array[0..2] of VKdouble;
  TvkuVectorp3  = array[0..2] of Pointer;

  TvkuVectorub4 = array[0..3] of VkByte;
  TvkuVectori4  = array[0..3] of VKint;
  TvkuVectorf4  = array[0..3] of VKfloat;
  TvkuVectord4  = array[0..3] of VKdouble;
  TvkuVectorp4  = array[0..3] of Pointer;

  TvkuVector2ub = TvkuVectorub2;
  TvkuVector3ub = TvkuVectorub3;
  TvkuVector4ub = TvkuVectorub4;

  TvkuVector2i = TvkuVectori2;
  TvkuVector3i = TvkuVectori3;
  TvkuVector4i = TvkuVectori4;

  TvkuVector2e = array[0..1] of VKenum;
  TvkuVector3e = array[0..2] of VKenum;
  TvkuVector4e = array[0..3] of VKenum;

  TvkuVector2f = TvkuVectorf2;
  TvkuVector3f = TvkuVectorf3;
  TvkuVector4f = TvkuVectorf4;

  TvkuVector2d = TvkuVectord2;
  TvkuVector3d = TvkuVectord3;
  TvkuVector4d = TvkuVectord4;

  TvkuVector2p = TvkuVectorp2;
  TvkuVector3p = TvkuVectorp3;
  TvkuVector4p = TvkuVectorp4;

  TvkuPlanef = TvkuVector4f;

  TvkuQuaternion = type TvkuVector4f;

const
  vkuVectorNull : TvkuVector3f = (0,0,0);
  vkuVectorUnitX: TvkuVector3f = (1,0,0);
  vkuVectorUnitY: TvkuVector3f = (0,1,0);
  vkuVectorUnitZ: TvkuVector3f = (0,0,1);

type
  TvkuVector3fArr8 = array[0..7] of TvkuVector4f;
  TvkuRayf = packed record
    p, v: TvkuVector3f;
  end;

  TvkuRecord2ub = packed record
    case Integer of
      0: (x, y: VkByte);
      1: (s, t: VkByte);
      2: (u, v: VkByte);
      3: (vec: TvkuVector2ub);
  end;
  TvkuRecord3ub = packed record
    case Integer of
      0: (x, y, z: VkByte);
      1: (r, g, b: VkByte);
      2: (u, v, w: VkByte);
      3: (vec: TvkuVector3ub);
  end;
  TvkuRecord4ub = packed record
    case Integer of
      0: (x, y, z, w: VkByte);
      1: (r, g, b, a: VkByte);
      2: (vec: TvkuVector4ub);
  end;

  TvkuRecord2i = packed record
    case Integer of
      0: (x, y: VKint);
      1: (s, t: VKint);
      2: (u, v: VKint);
      3: (vec: TvkuVector2i);
  end;
  TvkuRecord3i = packed record
    case Integer of
      0: (x, y, z: VKint);
      1: (r, g, b: VKint);
      2: (u, v, w: VKint);
      3: (vec: TvkuVector3i);
  end;
  TvkuRecord4i = packed record
    case Integer of
      0: (x, y, z, w: VKint);
      1: (r, g, b, a: VKint);
      2: (vec: TvkuVector4i);
  end;

  TvkuRecord2f = packed record
    case Integer of
      0: (x, y: VKfloat);
      1: (s, t: VKfloat);
      2: (u, v: VKfloat);
      3: (vec: TvkuVector2f);
  end;
  TvkuRecord3f = packed record
    case Integer of
      0: (x, y, z: VKfloat);
      1: (r, g, b: VKfloat);
      2: (u, v, w: VKfloat);
      3: (vec: TvkuVector3f);
  end;
  TvkuRecord4f = packed record
    case Integer of
      0: (x, y, z, w: VKfloat);
      1: (r, g, b, a: VKfloat);
      2: (vec4: TvkuVector4f);
      3: (vec3: TvkuVector3f);
  end;

  TvkuRecord2d = packed record
    case Integer of
      0: (x, y: VKdouble);
      1: (s, t: VKdouble);
      2: (u, v: VKdouble);
      3: (vec: TvkuVector2d);
  end;
  TvkuRecord3d = packed record
    case Integer of
      0: (x, y, z: VKdouble);
      1: (r, g, b: VKdouble);
      2: (u, v, w: VKdouble);
      3: (vec: TvkuVector3d);
  end;
  TvkuRecord4d = packed record
    case Integer of
      0: (x, y, z, w: VKdouble);
      1: (r, g, b, a: VKdouble);
      2: (vec: TvkuVector4d);
  end;

  PvkuVector2i = ^TvkuVector2i;
  PvkuVector3i = ^TvkuVector3i;
  PvkuVector4i = ^TvkuVector4i;

  PvkuVector2e = ^TvkuVector2e;
  PvkuVector3e = ^TvkuVector3e;
  PvkuVector4e = ^TvkuVector4e;

  PvkuVector2ub = ^TvkuVector2ub;
  PvkuVector3ub = ^TvkuVector3ub;
  PvkuVector4ub = ^TvkuVector4ub;

  PvkuVector2f = ^TvkuVector2f;
  PvkuVector3f = ^TvkuVector3f;
  PvkuVector4f = ^TvkuVector4f;

  PvkuVector2d = ^TvkuVector2d;
  PvkuVector3d = ^TvkuVector3d;
  PvkuVector4d = ^TvkuVector4d;

  PvkuVector2p = ^TvkuVector2p;
  PvkuVector3p = ^TvkuVector3p;
  PvkuVector4p = ^TvkuVector4p;

  TVectorColor = -$7FFFFFFF-1..$7FFFFFFF;

  TvkuMatrix2ub = array[0..1] of TvkuVector2ub;
  TvkuMatrix2i  = array[0..1] of TvkuVector2i;
  TvkuMatrix2f  = array[0..1] of TvkuVector2f;
  TvkuMatrix2d  = array[0..1] of TvkuVector2d;

  TvkuMatrix3ub = array[0..2] of TvkuVector3ub;
  TvkuMatrix3i  = array[0..2] of TvkuVector3i;
  TvkuMatrix3f  = array[0..2] of TvkuVector3f;
  TvkuMatrix3d  = array[0..2] of TvkuVector3d;

  TvkuMatrix4ub = array[0..3] of TvkuVector4ub;
  TvkuMatrix4i  = array[0..3] of TvkuVector4i;
  TvkuMatrix4f  = array[0..3] of TvkuVector4f;
  TvkuMatrix4d  = array[0..3] of TvkuVector4d;

  PvkuMatrix2ub = ^TvkuMatrix2ub;
  PvkuMatrix2i  = ^TvkuMatrix2i;
  PvkuMatrix2f  = ^TvkuMatrix2f;
  PvkuMatrix2d  = ^TvkuMatrix2d;

  PvkuMatrix3ub = ^TvkuMatrix3ub;
  PvkuMatrix3i  = ^TvkuMatrix3i;
  PvkuMatrix3f  = ^TvkuMatrix3f;
  PvkuMatrix3d  = ^TvkuMatrix3d;

  PvkuMatrix4ub = ^TvkuMatrix4ub;
  PvkuMatrix4i  = ^TvkuMatrix4i;
  PvkuMatrix4f  = ^TvkuMatrix4f;
  PvkuMatrix4d  = ^TvkuMatrix4d;


const
  maAxisX = 0;
  maAxisY = 1;
  maAxisZ = 2;
  maPos   = 3;
  vkuMatrixIdentity: TvkuMatrix4f = ((1,0,0,0),(0,1,0,0),(0,0,1,0),(0,0,0,1));

  quW = 0;
  quX = 1;
  quY = 2;
  quZ = 3;
  vkuQuaternionIdentity: TvkuQuaternion = (1,0,0,0);


 //==================================================================================

  //Stream
  procedure vkuVector2fWrite(const vec: TvkuVector2f; const aStream: TStream);
  procedure vkuVector3fWrite(const vec: TvkuVector3f; const aStream: TStream);
  procedure vkuVector4fWrite(const vec: TvkuVector4f; const aStream: TStream);
  function  vkuVector2fRead(const aStream: TStream): TvkuVector2f;
  function  vkuVector3fRead(const aStream: TStream): TvkuVector3f;
  function  vkuVector4fRead(const aStream: TStream): TvkuVector4f;

  //Vektor
  function vkuVector4f(const X, Y, Z, W: Single): TvkuVector4f;
  function vkuVector4f(const aVec: TvkuVector3f; const W: Single): TvkuVector4f;
  function vkuVector4f(const aVec: TvkuVector2f; const Z, W: Single): TvkuVector4f;
  function vkuVector4d(const X, Y, Z, W: Single): TvkuVector4d;
  function vkuVector3f(const X, Y, Z: Single): TvkuVector3f; overload;
  function vkuVector3f(const v: TvkuVector4f): TvkuVector3f; overload;
  function vkuVector3f(const v: TvkuVector2f; const z: Single): TvkuVector3f; overload;
  function vkuVector3f(const p1, p2: TvkuVector3f): TvkuVector3f; overload;
  function vkuVector2f(const X, Y: Single): TvkuVector2f;
  function vkuVector2f(const v3: TvkuVector3f): TvkuVector2f;
  function vkuVector2f(const v4: TvkuVector4f): TvkuVector2f;
  function vkuVector4i(const W, X, Y, Z: Integer): TvkuVector4i;
  function vkuVector2i(const X, Y: Integer): TvkuVector2i;
  function vkuVector2e(const X, Y: VKenum): TvkuVector2e;
  function vkuVector3e(const X, Y, Z: VKenum): TvkuVector3e;
  function vkuVector4e(const X, Y, Z, W: VKenum): TvkuVector4e;

  //Vektor Functions
  function vkuVectorNormalize(const v: TvkuVector4f): TvkuVector4f; overload;
  function vkuVectorNormalize(const v: TvkuVector3f): TvkuVector3f; overload;
  function vkuVectorNormalize(const v: TvkuVector2f): TvkuVector2f; overload;
  function vkuVectorLength(const v: TvkuVector3f): Single; overload;
  function vkuVectorLength(const v: TvkuVector2f): Single; overload;
  function vkuVectorProduct(const v1, v2: TvkuVector3f): TvkuVector3f;
  function vkuVectorScalar(const v1, v2: TvkuVector4f): Single; overload;
  function vkuVectorScalar(const v1, v2: TvkuVector3f): Single; overload;
  function vkuVectorScalar(const v1, v2: TvkuVector2f): Single; overload;
  function vkuVectorAngle(const v1, v2: TvkuVector3f): Single; overload;
  function vkuVectorAngle(const v1, v2: TvkuVector2f): Single; overload;
  function vkuVectorAngle2(const v1, v2: TvkuVector2f): Single;
  function vkuVectorEquals(const v1, v2: TvkuVector2f): Boolean; overload;
  function vkuVectorEquals(const v1, v2: TvkuVector3f): Boolean; overload;
  function vkuVectorEquals(const v1, v2: TvkuVector4f): Boolean; overload;
  function vkuVectorMult(const v: TvkuVector2f; const s: Single): TvkuVector2f;
  function vkuVectorMult(const v: TvkuVector3f; const s: Single): TvkuVector3f;
  function vkuVectorMult(const v: TvkuVector4f; const s: Single): TvkuVector4f;
  function vkuVectorDivide(const v: TvkuVector3f; const s: Single): TvkuVector3f;
  function vkuVectorClamp(const v: TvkuVector3f; const aMin, aMax: Single): TvkuVector3f; overload;
  function vkuVectorClamp(const v: TvkuVector4f; const aMin, aMax: Single): TvkuVector4f; overload;
  function vkuVectorAdd(const v1, v2: TvkuVector3f): TvkuVector3f;
  function vkuVectorSubtract(const v1, v2: TvkuVector3f): TvkuVector3f;
  function vkuVectorAdd(const v1, v2: TvkuVector2f): TvkuVector2f;
  function vkuVectorSubtract(const v1, v2: TvkuVector2f): TvkuVector2f;
  procedure vkuVectorOrthoNormalize(var reference, tangent: TvkuVector3f);

  function vkuPlanef(const p1, p2, p3: TvkuVector3f): TvkuPlanef;
  function vkuPlanef(const n, p: TvkuVector3f): TvkuPlanef;
  function vkuPlaneNormalize(const p: TvkuPlanef): TvkuPlanef;
  function vkuPlaneCrossRay(const aPlane: TvkuPlanef; const aRay: TvkuRayf; out aPoint: TvkuVector3f): Boolean;

  function vkuRayf(const p, v: TvkuVector3f): TvkuRayf;
  function vkuRayNormalize(const r: TvkuRayf): TvkuRayf;
  function vkuRayPoint(const r: TvkuRayf; const lambda: Single): TvkuVector3f;

  function vkuVector4fToStr(const v: TvkuVector4f; const round: Integer = 3): String;
  function vkuVector3fToStr(const v: TvkuVector3f; const round: Integer = 3): String;
  function vkuVector2fToStr(const v: TvkuVector2f; const round: Integer = 3): String;
  function vkuTryStrToVector4f(str: String; out aVec: TvkuVector4f): Boolean;
  function vkuTryStrToVector3f(str: String; out aVec: TvkuVector3f): Boolean;
  function vkuTryStrToVector2f(str: String; out aVec: TvkuVector2f): Boolean;
  function vkuStrToVector4f(str: String): TvkuVector4f;
  function vkuStrToVector3f(str: String): TvkuVector3f;
  function vkuStrToVector2f(str: String): TvkuVector2f;
  function vkuVector4iToStr(const v: TvkuVector4i): String;
  function vkuVector3iToStr(const v: TvkuVector3i): String;
  function vkuVector2iToStr(const v: TvkuVector2i): String;
  function vkuStrToVector4i(const str: String): TvkuVector4i;
  function vkuStrToVector3i(const str: String): TvkuVector3i;
  function vkuStrToVector2i(const str: String): TvkuVector2i;
  function vkuVectorToColor(const v: TvkuVector4f): TVectorColor; overload;
  function vkuVectorToColor(const v: TvkuVector3f): TVectorColor; overload;
  function vkuColorToVector3f(const c: TVectorColor): TvkuVector3f;
  function vkuColorToVector4f(const c: TVectorColor; const a: Single): TvkuVector4f;
  function vkuVectorHSVColor(v: TvkuVector3f): TvkuVector3f; overload;
  function vkuVectorHSVColor(v: TvkuVector4f): TvkuVector4f; overload;

  operator >< (const v1, v2: TvkuVector3f): TvkuVector3f; inline;

  operator * (const v1, v2: TvkuVector4f): Single; inline; overload;
  operator * (const v1, v2: TvkuVector3f): Single; inline; overload;
  operator * (const v1, v2: TvkuVector2f): Single; inline; overload;

  operator * (const v: TvkuVector2f; const s: Single): TvkuVector2f; inline; overload;
  operator * (const v: TvkuVector3f; const s: Single): TvkuVector3f; inline; overload;
  operator * (const v: TvkuVector4f; const s: Single): TvkuVector4f; inline; overload;

  operator * (const s: Single; const v: TvkuVector2f): TvkuVector2f; inline; overload;
  operator * (const s: Single; const v: TvkuVector3f): TvkuVector3f; inline; overload;
  operator * (const s: Single; const v: TvkuVector4f): TvkuVector4f; inline; overload;

  operator / (const v: TvkuVector3f; const s: Single): TvkuVector3f; inline; overload;

  operator = (const v1, v2: TvkuVector2f): Boolean; inline; overload;
  operator = (const v1, v2: TvkuVector3f): Boolean; inline; overload;
  operator = (const v1, v2: TvkuVector4f): Boolean; inline; overload;

  operator + (const v1, v2: TvkuVector3f): TvkuVector3f; inline;
  operator - (const v1, v2: TvkuVector3f): TvkuVector3f; inline;

  operator + (const v1, v2: TvkuVector2f): TvkuVector2f; inline;
  operator - (const v1, v2: TvkuVector2f): TvkuVector2f; inline;

  function vkuMatrix4d(const m: TvkuMatrix4f): TvkuMatrix4d;

  function vkuMatrixTranslate(const v: TvkuVector3f): TvkuMatrix4f;
  function vkuMatrixScale(const v: TvkuVector3f): TvkuMatrix4f; overload;
  function vkuMatrixScale(const s: Single): TvkuMatrix4f; overload;
  function vkuMatrixRotate(axis: TvkuVector3f; const angle: Single): TvkuMatrix4f;
  function vkuMatrixMult(const m1, m2: TvkuMatrix4f): TvkuMatrix4f;
  function vkuMatrixMultVec(const m: TvkuMatrix4f; const v: TvkuVector4f): TvkuVector4f;
  function vkuMatrixTranspose(const m: TvkuMatrix3f): TvkuMatrix3f; overload;
  function vkuMatrixTranspose(const m: TvkuMatrix4f): TvkuMatrix4f; overload;
  function vkuMatrixSubMatrix(const m:TvkuMatrix4f; const s, z: Integer): TvkuMatrix3f;
  function vkuMatrixDeterminant(const m: TvkuMatrix3f): Single; overload;
  function vkuMatrixDeterminant(const m: TvkuMatrix4f): Single; overload;
  function vkuMatrixAdjoint(const m: TvkuMatrix4f): TvkuMatrix4f;
  function vkuMatrixInvert(const m: TvkuMatrix4f): TvkuMatrix4f;

  operator * (const m1, m2: TvkuMatrix4f): TvkuMatrix4f;
  operator * (const m: TvkuMatrix4f; const v: TvkuVector4f): TvkuVector4f;
  operator * (const m: TvkuMatrix4f; const v: TvkuVector3f): TvkuVector3f;

  function vkuQuaternion(const W, X, Y, Z: Single): TvkuQuaternion;
  function vkuQuaternionNormalize(const q: TvkuQuaternion): TvkuQuaternion;
  procedure vkuQuaternionNormalizeInplace(var q: TvkuQuaternion);
  function vkuQuaternionToVector(const q: TvkuQuaternion): TvkuVector3f;
  function vkuVectorToQuaternion(const v: TvkuVector3f): TvkuQuaternion;

  function vkuQuaternionConjugate(const q: TvkuQuaternion): TvkuQuaternion;
  function vkuQuaternionMultiply(const l,r: TvkuQuaternion): TvkuQuaternion;
  function vkuQuaternionAdd(const a,b: TvkuQuaternion): TvkuQuaternion;
  function vkuQuaternionSubtract(const l,r: TvkuQuaternion): TvkuQuaternion;
  function vkuQuaternionScale(const q: TvkuQuaternion; const f: Single): TvkuQuaternion;


  function vkuQuaternionToMatrix(const q: TvkuQuaternion): TvkuMatrix4f;
  function vkuMatrixToQuaternion(const m: TvkuMatrix4f): TvkuQuaternion;

  function vkuQuaternionToRotation(const q: TvkuQuaternion; out angle: Single): TvkuVector3f;
  function vkuRotationToQuaternion(const angle: Single; const axis: TvkuVector3f): TvkuQuaternion;

  function vkuQuaternionTransformVec(const q: TvkuQuaternion; const v: TvkuVector3f): TvkuVector3f;

  function vkuQuaternionLookAt(const Location, Target, UpVector: TvkuVector3f): TvkuQuaternion;

  function vkuVectorRotationTo(const a, b: TvkuVector3f): TvkuQuaternion;

  function vkuQuaternionHalfAngle(const q: TvkuQuaternion): TvkuQuaternion;
  function vkuQuaternionAngleBetween(const a, b: TvkuQuaternion): double;
  function vkuQuaternionSlerpOrientation(const a, b: TvkuQuaternion; const t: single): TvkuQuaternion;
  function vkuQuaternionNlerpOrientation(const a, b: TvkuQuaternion; const t: single): TvkuQuaternion;

  operator +(const a, b: TvkuQuaternion): TvkuQuaternion;
  operator -(const l, r: TvkuQuaternion): TvkuQuaternion;
  operator *(const l, r: TvkuQuaternion): TvkuQuaternion;
  operator *(const q: TvkuQuaternion; const s: Single): TvkuQuaternion;


 type

 TvkuFrustum = class(TObject)
  private
    fProjMatrix: TvkuMatrix4f;
    function  GetProjMatrixPtr: Pointer;
    function  GetWidth: Single;
    function  GetHeight: Single;
    function  GetFOVAngle: Single;
    function  GetAspectRatio: Single;
    procedure UpdateProjMatrix;
  protected
    fIsOrthogonal: Boolean;
    fTop, fBottom, fLeft, fRight, fNear, fFar: Single;
  public      
    constructor Create;
    procedure Frustum(const aLeft, aRight, aBottom, aTop, aNear, aFar: Single);
    procedure Perspective(const aFOVAngle, aAspectRatio, aNear, aFar: Single);
    procedure Ortho(const aLeft, aRight, aBottom, aTop, aNear, aFar: Single);
    property Top:           Single        read fTop;
    property Bottom:        Single        read fBottom;
    property Left:          Single        read fLeft;
    property Right:         Single        read fRight;
    property Near:          Single        read fNear;
    property Far:           Single        read fFar;
    property Width:         Single        read GetWidth;
    property Height:        Single        read GetHeight;
    property FOVAngle:      Single        read GetFOVAngle;
    property AspectRatio:   Single        read GetAspectRatio;
    property IsOrthogonal:  Boolean       read fIsOrthogonal;
    property ProjMatrix:    TvkuMatrix4f  read fProjMatrix;
    property ProjMatrixPtr: Pointer       read GetProjMatrixPtr;
  end;

  TvkuCamera = class(TvkuFrustum)
  private
    fPosition: TvkuMatrix4f;
    fInvertPos: TvkuMatrix4f;
    fInvertValid: Boolean;
    function  GetInvertPos: TvkuMatrix4f;
    function  GetPositionPtr: Pointer;
    procedure SetPosition(aValue: TvkuMatrix4f);
  public   
    constructor Create;
    procedure Move(const aVec: TvkuVector3f);
    procedure Tilt(const aAngle: Single);
    procedure Turn(const aAngle: Single);
    procedure Roll(const aAngle: Single);
    function GetRay(const aPos: TvkuVector2f): TvkuRayf;
    property Position:    TvkuMatrix4f read fPosition write SetPosition;
    property InvertPos:   TvkuMatrix4f read GetInvertPos;
    property PositionPtr: Pointer      read GetPositionPtr;
  end;


implementation

operator >< (const v1, v2: TvkuVector3f): TvkuVector3f;
begin
  result := vkuVectorProduct(v1, v2);
end;

operator * (const v1, v2: TvkuVector4f): Single;
begin
  result := vkuVectorScalar(v1, v2);
end;

operator * (const v1, v2: TvkuVector3f): Single;
begin
  result := vkuVectorScalar(v1, v2);
end;

operator * (const v1, v2: TvkuVector2f): Single;
begin
  result := vkuVectorScalar(v1, v2);
end;

operator * (const v: TvkuVector2f; const s: Single): TvkuVector2f;
begin
  result := vkuVectorMult(v, s);
end;

operator * (const v: TvkuVector3f; const s: Single): TvkuVector3f;
begin
  result := vkuVectorMult(v, s);
end;

operator * (const v: TvkuVector4f; const s: Single): TvkuVector4f;
begin
  result := vkuVectorMult(v, s);
end;

operator * (const s: Single; const v: TvkuVector2f): TvkuVector2f;
begin
  result := vkuVectorMult(v, s);
end;

operator * (const s: Single; const v: TvkuVector3f): TvkuVector3f;
begin
  result := vkuVectorMult(v, s);
end;

operator * (const s: Single; const v: TvkuVector4f): TvkuVector4f;
begin
  result := vkuVectorMult(v, s);
end;

operator / (const v: TvkuVector3f; const s: Single): TvkuVector3f;
begin
  result := vkuVectorDivide(v, s);
end;

operator = (const v1, v2: TvkuVector2f): Boolean;
begin
  result := vkuVectorEquals(v1, v2);
end;

operator = (const v1, v2: TvkuVector3f): Boolean;
begin
  result := vkuVectorEquals(v1, v2);
end;

operator = (const v1, v2: TvkuVector4f): Boolean;
begin
  result := vkuVectorEquals(v1, v2);
end;

operator + (const v1, v2: TvkuVector3f): TvkuVector3f;
begin
  result := vkuVectorAdd(v1, v2);
end;

operator - (const v1, v2: TvkuVector3f): TvkuVector3f;
begin
  result := vkuVectorSubtract(v1, v2);
end;

operator + (const v1, v2: TvkuVector2f): TvkuVector2f;
begin
  result := vkuVectorAdd(v1, v2);
end;

operator - (const v1, v2: TvkuVector2f): TvkuVector2f;
begin
  result := vkuVectorSubtract(v1, v2);
end;

procedure vkuVector2fWrite(const vec: TvkuVector2f; const aStream: TStream);
begin
  aStream.Write(vec[0], SizeOf(vec));
end;

procedure vkuVector3fWrite(const vec: TvkuVector3f; const aStream: TStream);
begin
  aStream.Write(vec[0], SizeOf(vec));
end;

procedure vkuVector4fWrite(const vec: TvkuVector4f; const aStream: TStream);
begin
  aStream.Write(vec[0], SizeOf(vec));
end;

function  vkuVector2fRead(const aStream: TStream): TvkuVector2f;
begin
  if aStream.Read(result{%H-}, SizeOf(result)) < SizeOf(result) then
    raise Exception.Create('vkuVector2fRead - unexpected stream size');
end;

function vkuVector3fRead(const aStream: TStream): TvkuVector3f;
begin
  if aStream.Read(result{%H-}, SizeOf(result)) < SizeOf(result) then
    raise Exception.Create('vkuVector3fRead - unexpected stream size');
end;

function vkuVector4fRead(const aStream: TStream): TvkuVector4f;
begin
  if aStream.Read(result{%H-}, SizeOf(result)) < SizeOf(result) then
    raise Exception.Create('vkuVector4fRead - unexpected stream size');
end;

function vkuVector4f(const X,Y,Z,W: Single): TvkuVector4f;
begin
  result[0] := X;
  result[1] := Y;
  result[2] := Z;
  result[3] := W;
end;

function vkuVector4f(const aVec: TvkuVector3f; const W: Single): TvkuVector4f;
begin
  PvkuVector3f(@result[0])^ := aVec;
  result[3] := W;
end;

function vkuVector4f(const aVec: TvkuVector2f; const Z, W: Single): TvkuVector4f;
begin
  PvkuVector2f(@result[0])^ := aVec;
  result[2] := Z;
  result[3] := W;
end;

function vkuVector4d(const X,Y,Z,W: Single): TvkuVector4d;
begin
  result[0] := X;
  result[1] := Y;
  result[2] := Z;
  result[3] := W;
end;

function vkuVector3f(const X,Y,Z: Single): TvkuVector3f;
begin
  result[0] := X;
  result[1] := Y;
  result[2] := Z;
end;

function vkuVector3f(const v: TvkuVector4f): TvkuVector3f;
begin
  result := PvkuVector3f(@v[0])^;
end;

function vkuVector3f(const v: TvkuVector2f; const z: Single): TvkuVector3f;
begin
  result[0] := v[0];
  result[1] := v[1];
  result[2] := z;
end;

function vkuVector3f(const p1, p2: TvkuVector3f): TvkuVector3f;
var
  i: Integer;
begin
  for i := 0 to 2 do
    result[i] := p2[i] - p1[i];
end;

function vkuVector2f(const X,Y: Single): TvkuVector2f;
begin
  result[0] := X;
  result[1] := Y;
end;

function vkuVector2f(const v3: TvkuVector3f): TvkuVector2f;
begin
  result[0] := v3[0];
  result[1] := v3[1];
end;

function vkuVector2f(const v4: TvkuVector4f): TvkuVector2f;
begin
  result[0] := v4[0];
  result[1] := v4[1];
end;

function  vkuVector4i(const W, X, Y, Z: Integer): TvkuVector4i;
begin
  result[0] := W;
  result[1] := X;
  result[2] := Y;
  result[3] := Z;
end;

function  vkuVector2i(const X, Y: Integer): TvkuVector2i;
begin
  result[0] := X;
  result[1] := Y;
end;

function vkuVector2e(const X, Y: VKenum): TvkuVector2e;
begin
  result[0] := X;
  result[1] := Y;
end;

function vkuVector3e(const X, Y, Z: VKenum): TvkuVector3e;
begin
  result[0] := X;
  result[1] := Y;
  result[2] := Z;
end;

function vkuVector4e(const X, Y, Z, W: VKenum): TvkuVector4e;
begin
  result[0] := X;
  result[1] := Y;
  result[2] := Z;
  result[3] := W;
end;

function vkuVectorNormalize(const v: TvkuVector4f): TvkuVector4f;
begin
  result := v;
  if (result[3] <> 0) then
    result := vkuVectorMult(result, result[3]);
  PvkuVector3f(@result[0])^ := vkuVectorNormalize(PvkuVector3f(@result[0])^);
end;

function vkuVectorNormalize(const v: TvkuVector3f): TvkuVector3f;
var len: Single;
begin
  len := vkuVectorLength(v);
  if (len > 0) then begin
    result[0] := v[0]/len;
    result[1] := v[1]/len;
    result[2] := v[2]/len;
  end;
end;

function vkuVectorNormalize(const v: TvkuVector2f): TvkuVector2f;
var len: Single;
begin
  len := vkuVectorLength(v);
  result[0] := v[0]/len;
  result[1] := v[1]/len;
end;

function vkuVectorLength(const v: TvkuVector3f): Single;
begin
  result := SQRT(SQR(v[0])+SQR(v[1])+SQR(v[2]));
end;

function vkuVectorLength(const v: TvkuVector2f): Single;
begin
  result := SQRT(SQR(v[0])+SQR(v[1]));
end;

function vkuVectorProduct(const v1, v2: TvkuVector3f): TvkuVector3f;
begin
  result[0] := v1[1]*v2[2] - v1[2]*v2[1];
  result[1] := v1[2]*v2[0] - v1[0]*v2[2];
  result[2] := v1[0]*v2[1] - v1[1]*v2[0];
end;

function vkuVectorScalar(const v1, v2: TvkuVector4f): Single; overload;
begin
  result := v1[0]*v2[0] + v1[1]*v2[1] + v1[2]*v2[2] + v1[3]*v2[3];
end;

function vkuVectorScalar(const v1, v2: TvkuVector3f): Single;
begin
  result := v1[0]*v2[0] + v1[1]*v2[1] + v1[2]*v2[2];
end;

function vkuVectorScalar(const v1, v2: TvkuVector2f): Single;
begin
  result := v1[0]*v2[0] + v1[1]*v2[1];
end;

function vkuVectorAngle(const v1, v2: TvkuVector3f): Single;
begin
  result := ArcCos(vkuVectorScalar(v1, v2)/(vkuVectorLength(v1)*vkuVectorLength(v2)));
end;

function vkuVectorAngle(const v1, v2: TvkuVector2f): Single;
begin
  result := ArcCos(vkuVectorScalar(v1, v2)/(vkuVectorLength(v1)*vkuVectorLength(v2)));
end;

function vkuVectorAngle2(const v1, v2: TvkuVector2f): Single;
begin
  result := arctan2(
    v2[0] * v1[1] - v2[1] * v1[0],
    v2[0] * v1[0] + v2[1] * v1[1]);
end;

function vkuVectorEquals(const v1, v2: TvkuVector2f): Boolean;
begin
  result := (v1[0] = v2[0]) and (v1[1] = v2[1]);
end;

function vkuVectorEquals(const v1, v2: TvkuVector3f): Boolean;
begin
  result := (v1[2] = v2[2]) and vkuVectorEquals(PvkuVector2f(@v1[0])^, PvkuVector2f(@v2[0])^);
end;

function vkuVectorEquals(const v1, v2: TvkuVector4f): Boolean;
begin
  result := (v1[3] = v2[3]) and vkuVectorEquals(PvkuVector3f(@v1[0])^, PvkuVector3f(@v2[0])^);
end;

function vkuVectorMult(const v: TvkuVector2f; const s: Single): TvkuVector2f;
begin
  result[0] := v[0] * s;
  result[1] := v[1] * s;
end;

function vkuVectorMult(const v: TvkuVector3f; const s: Single): TvkuVector3f;
var
  i: Integer;
begin
  for i := 0 to 2 do
    result[i] := v[i] * s;
end;

function vkuVectorMult(const v: TvkuVector4f; const s: Single): TvkuVector4f;
var
  i: Integer;
begin
  for i := 0 to 3 do
    result[i] := v[i] * s;
end;

function vkuVectorDivide(const v: TvkuVector3f; const s: Single): TvkuVector3f;
var
  i: Integer;
begin
  for i := 0 to 3 do
    result[i] := v[i] / s;
end;

function vkuVectorClamp(const v: TvkuVector3f; const aMin, aMax: Single): TvkuVector3f;
var i: Integer;
begin
  for i := 0 to High(v) do
    result[i] := Min(Max(v[i], aMin), aMax);
end;

function vkuVectorClamp(const v: TvkuVector4f; const aMin, aMax: Single): TvkuVector4f;
var i: Integer;
begin
  for i := 0 to High(v) do
    result[i] := Min(Max(v[i], aMin), aMax);
end;

function vkuVectorAdd(const v1, v2: TvkuVector3f): TvkuVector3f;
var
  i: Integer;
begin
  for i := 0 to 2 do
    result[i] := v1[i] + v2[i];
end;

function vkuVectorSubtract(const v1, v2: TvkuVector3f): TvkuVector3f;
var
  i: Integer;
begin
  for i := 0 to 2 do
    result[i] := v1[i] - v2[i];
end;

function vkuVectorAdd(const v1, v2: TvkuVector2f): TvkuVector2f;
begin
  result[0] := v1[0] + v2[0];
  result[1] := v1[1] + v2[1];
end;

function vkuVectorSubtract(const v1, v2: TvkuVector2f): TvkuVector2f;
begin
  result[0] := v1[0] - v2[0];
  result[1] := v1[1] - v2[1];
end;

procedure vkuVectorOrthoNormalize(var reference, tangent: TvkuVector3f);
var
  proj: TvkuVector3f;
begin
  reference:= vkuVectorNormalize(reference);

  proj:= vkuVectorMult(reference, vkuVectorScalar(tangent, reference));
  tangent:= vkuVectorSubtract(tangent, proj);
  tangent:= vkuVectorNormalize(tangent);
end;

function vkuPlanef(const p1, p2, p3: TvkuVector3f): TvkuPlanef;
var
  n, v1, v2: TvkuVector3f;
begin
  v1 := vkuVector3f(p2, p1);
  v2 := vkuVector3f(p2, p3);
  n := vkuVectorProduct(v1, v2);
  result := vkuPlanef(n, p2);
end;

function vkuPlanef(const n, p: TvkuVector3f): TvkuPlanef;
var
  d: Single;
begin
  d := vkuVectorScalar(n, p);
  PvkuVector3f(@result)^ := n;
  result[3] := -d;
end;

function vkuPlaneNormalize(const p: TvkuPlanef): TvkuPlanef;
var
  m: Single;
  i: Integer;
begin
  m := Sqrt(Sqr(p[0]) + Sqr(p[1]) + Sqr(p[2]));
  for i := 0 to 3 do
    result[i] := p[i] / m;
end;

function vkuPlaneCrossRay(const aPlane: TvkuPlanef; const aRay: TvkuRayf; out aPoint: TvkuVector3f): Boolean;
var
  lambda, real: Double;
  i: Integer;
begin
  result := false;
  lambda := 0;
  real   := 0;
  for i := 0 to 2 do begin
    lambda := lambda + aRay.v[i] * aPlane[i];
    real   := real   + aRay.p[i] * aPlane[i];
  end;
  if (lambda = 0) then begin
    aPoint := vkuVector3f(0, 0, 0);
    exit;
  end;
  lambda := (aPlane[3] - real) / lambda;
  aPoint := vkuRayPoint(aRay, -lambda);
  result := true;
end;

function vkuRayf(const p, v: TvkuVector3f): TvkuRayf;
begin
  result.p := p;
  result.v := v;
end;

function vkuRayNormalize(const r: TvkuRayf): TvkuRayf;
begin
  result.p := r.p;
  result.v := vkuVectorNormalize(r.v);
end;

function vkuRayPoint(const r: TvkuRayf; const lambda: Single): TvkuVector3f;
begin
  result := vkuVectorAdd(r.p, vkuVectorMult(r.v, lambda));
end;

function vkuVector4fToStr(const v: TvkuVector4f; const round: Integer): String;
var
  f: TFormatSettings;
begin
  f.DecimalSeparator := '.';
  if (round >= 0) then
    result := Format('%.*f; %.*f; %.*f; %.*f;', [round, v[0], round, v[1], round, v[2], round, v[3]], f)
  else
    result := Format('%f; %f; %f; %f;', [v[0], v[1], v[2], v[3]], f);
end;

function vkuVector3fToStr(const v: TvkuVector3f; const round: Integer): String;
var
  f: TFormatSettings;
begin
  f.DecimalSeparator := '.';
  if (round >= 0) then
    result := Format('%.*f; %.*f; %.*f;', [round, v[0], round, v[1], round, v[2]], f)
  else
    result := Format('%f; %f; %f;', [v[0], v[1], v[2]], f);
end;

function vkuVector2fToStr(const v: TvkuVector2f; const round: Integer): String;
var
  f: TFormatSettings;
begin
  f.DecimalSeparator := '.';
  if (round >= 0) then
    result := Format('%.*f; %.*f;', [round, v[0], round, v[1]], f)
  else
    result := Format('%f; %f;', [v[0], v[1]], f);
end;

function vkuStrToVectorIntern(str: String; const aAbortOnFailure: Boolean; out aVec: TvkuVector4f): Boolean;
var
  i, j, p, l: Integer;
  s: String;
  format: TFormatSettings;
  v: Single;
begin
  result := false;
  FillChar(aVec{%H-}, SizeOf(aVec), 0);
  FillChar(format{%H-}, SizeOf(format), 0);
  format.DecimalSeparator := '.';
  if (Length(str) > 0) and (str[Length(str)] <> ';') then
    str := str + ';';
  j := 0;
  i := 1;
  p := 1;
  l := Length(str);
  while (i <= l) do begin
    if str[i] = ';' then begin
      s := Trim(copy(str, p, i-p));
      if not TryStrToFloat(s, v, format) then begin
        if aAbortOnFailure then
          exit;
        v := 0;
      end;
      aVec[j] := v;
      inc(j);
      p := i+1;
    end;
    inc(i);
  end;
  result := true;
end;

function vkuTryStrToVector4f(str: String; out aVec: TvkuVector4f): Boolean;
begin
  result := vkuStrToVectorIntern(str, true, aVec);
end;

function vkuTryStrToVector3f(str: String; out aVec: TvkuVector3f): Boolean;
var
  v: TvkuVector4f;
begin
  if (Length(str) > 0) and (str[Length(str)] <> ';') then
    str := str + ';';
  result := vkuTryStrToVector4f(str+'0;', v);
  aVec := PvkuVector3f(@v[0])^;
end;

function vkuTryStrToVector2f(str: String; out aVec: TvkuVector2f): Boolean;
var
  v: TvkuVector4f;
begin
  if (Length(str) > 0) and (str[Length(str)] <> ';') then
    str := str + ';';
  result := vkuTryStrToVector4f(str+'0;0;', v);
  aVec := PvkuVector2f(@v[0])^;
end;

function vkuStrToVector4f(str: String): TvkuVector4f;
begin
  vkuStrToVectorIntern(str, false, result);
end;

function vkuStrToVector3f(str: String): TvkuVector3f;
var
  v: TvkuVector4f;
begin
  if (Length(str) > 0) and (str[Length(str)] <> ';') then
    str := str + ';';
  v := vkuStrToVector4f(str+'0;');
  result := PvkuVector3f(@v[0])^;
end;

function vkuStrToVector2f(str: String): TvkuVector2f;
var
  v: TvkuVector3f;
begin
  if (Length(str) > 0) and (str[Length(str)] <> ';') then
    str := str + ';';
  v := vkuStrToVector3f(str+'0;');
  result := PvkuVector2f(@v[0])^;
end;

function vkuVector4iToStr(const v: TvkuVector4i): String;
begin
  Result:= Format('%d;%d;%d;%d;',[v[0],v[1],v[2],v[3]]);
end;

function vkuVector3iToStr(const v: TvkuVector3i): String;
begin
  Result:= Format('%d;%d;%d;',[v[0],v[1],v[2]]);
end;

function vkuVector2iToStr(const v: TvkuVector2i): String;
begin
  Result:= Format('%d;%d;',[v[0],v[1]]);
end;

function vkuStrToVector4i(const str: String): TvkuVector4i;
var
  i, j, p, l: Integer;
  v: integer;
begin
  FillChar(result{%H-}, SizeOf(result), 0);
  j := 0;
  i := 1;
  p := 1;
  l := Length(str);
  while (i <= l) do begin
    if str[i] = ';' then begin
      if not TryStrToInt(copy(str, p, i-p), v) then
        v := 0;
      result[j] := v;
      inc(j);
      p := i+1;
    end;
    inc(i);
  end;
end;

function vkuStrToVector3i(const str: String): TvkuVector3i;
var
  v: TvkuVector4i;
begin
  v := vkuStrToVector4i(str+'0;');
  result := PvkuVector3i(@v[0])^;
end;

function vkuStrToVector2i(const str: String): TvkuVector2i;
var
  v: TvkuVector3i;
begin
  v := vkuStrToVector3i(str+'0;');
  result := PvkuVector2i(@v[0])^;
end;

function vkuVectorToColor(const v: TvkuVector4f): TVectorColor; overload;
begin
  result := vkuVectorToColor(PvkuVector3f(@v[0])^);
end;

function vkuVectorToColor(const v: TvkuVector3f): TVectorColor;
var
  r, g, b: Byte;
begin
  r := round(255*v[0]);
  g := round(255*v[1]);
  b := round(255*v[2]);
  result := r + (g shl 8) + (b shl 16);
end;

function vkuColorToVector3f(const c: TVectorColor): TvkuVector3f;
begin
  result[0] := ( c         and $FF) / 255;
  result[1] := ((c shr  8) and $FF) / 255;
  result[2] := ((c shr 16) and $FF) / 255;
end;

function vkuColorToVector4f(const c: TVectorColor; const a: Single): TvkuVector4f;
begin
  PvkuVector3f(@result[0])^ := vkuColorToVector3f(c);
  result[3] := a;
end;

function vkuVectorHSVColor(v: TvkuVector3f): TvkuVector3f;
const
  _H = 0;
  _S = 1;
  _V = 2;
var
  h: Integer;
  f, p, q, t: Single;
begin
  v[_H] := 360*v[_H];
//H normieren
  while (v[_H] < 0) do
    v[_H] := v[_H] + 360;
  while (v[_H] > 360) do
    v[_H] := v[_H] - 360;
//V normieren
  if (v[_V] < 0) then
    v[_V] := 0;
  if (v[_V] > 1) then
    v[_V] := 1;

  h := Floor(v[_H] / 60);
  f := v[_H]/60 - h;
  p := v[_V] * (1 - v[_S]);
  q := v[_V] * (1 - v[_S] * f);
  t := v[_V] * (1 - v[_S] * (1 - f));
  case h of
    1: result := vkuVector3f(q, v[_V], p);
    2: result := vkuVector3f(p, v[_V], t);
    3: result := vkuVector3f(p, q, v[_V]);
    4: result := vkuVector3f(t, p, v[_V]);
    5: result := vkuVector3f(v[_V], p, q);
  else
    result := vkuVector3f(v[_V], t, p);
  end;
end;

function vkuVectorHSVColor(v: TvkuVector4f): TvkuVector4f;
begin
  PvkuVector3f(@result)^ := vkuVectorHSVColor(PvkuVector3f(@v)^);
  result[3] := v[3];
end;


operator * (const m1, m2: TvkuMatrix4f): TvkuMatrix4f;
begin
  result := vkuMatrixMult(m1, m2);
end;

operator * (const m: TvkuMatrix4f; const v: TvkuVector4f): TvkuVector4f;
begin
  result := vkuMatrixMultVec(m, v);
end;

operator * (const m: TvkuMatrix4f; const v: TvkuVector3f): TvkuVector3f;
begin
  result := vkuVector3f(vkuMatrixMultVec(m, vkuVEctor4f(v, 1.0)));
end;

function vkuMatrix4d(const m: TvkuMatrix4f): TvkuMatrix4d;
var
  i, j: Integer;
begin
  for i := 0 to 3 do
    for j := 0 to 3 do
      result[i, j] := m[i, j];
end;

function vkuMatrixTranslate(const v: TvkuVector3f): TvkuMatrix4f;
var
  i: Integer;
begin
  result := vkuMatrixIdentity;
  for i := 0 to 2 do
    result[3, i] := v[i];
end;
function vkuMatrixScale(const v: TvkuVector3f): TvkuMatrix4f;
var
  i: Integer;
begin
  result := vkuMatrixIdentity;
  for i := 0 to 2 do
    result[i, i] := v[i];
end;

function vkuMatrixScale(const s: Single): TvkuMatrix4f;
var
  i: Integer;
begin
  result := vkuMatrixIdentity;
  for i := 0 to 2 do
    result[i, i] := s;
end;

function vkuMatrixRotate(axis: TvkuVector3f; const angle: Single): TvkuMatrix4f;
var
  X, Y, Z, a, s, c: Single;
begin
  axis := vkuVectorNormalize(axis);
  X := axis[0];
  Y := axis[1];
  Z := axis[2];
  a := angle/180*Pi;
  s := sin(a);
  c := cos(a);
  result := vkuMatrixIdentity;
  result[maAxisX] := vkuVector4f(
    SQR(X) + (1-SQR(X))*c,
    X*Y*(1-c) + Z*s,
    X*Z*(1-c) - Y*s,
    0);
  result[maAxisY] := vkuVector4f(
    X*Y*(1-c) - Z*s,
    SQR(Y) + (1-SQR(Y))*c,
    Y*Z*(1-c) + X*s,
    0);
  result[maAxisZ] := vkuVector4f(
    X*Z*(1-c) + Y*s,
    Y*Z*(1-c) - X*s,
    SQR(Z) + (1-SQR(Z))*c,
    0);
end;

function vkuMatrixMult(const m1, m2: TvkuMatrix4f): TvkuMatrix4f;
var
  x, y, i: Integer;
  sum: Single;
begin
  for x := 0 to 3 do begin
    for y := 0 to 3 do begin
      sum := 0;
      for i := 0 to 3 do
        sum := sum + m1[i, y] * m2[x, i];
      result[x, y] := sum;
    end;
  end;
end;

function vkuMatrixMultVec(const m: TvkuMatrix4f; const v: TvkuVector4f): TvkuVector4f;
var
  i, j: Integer;
  sum: Single;
begin
  for i := 0 to 3 do begin
    sum := 0;
    for j := 0 to 3 do
      sum := sum + m[j,i] * v[j];
    result[i] := sum;
  end;
end;

function vkuMatrixTranspose(const m: TvkuMatrix3f): TvkuMatrix3f;
var
  i, j: Integer;
begin
  for i := 0 to 2 do
    for j := 0 to 2 do
      result[i, j] := m[j, i];
end;

function vkuMatrixTranspose(const m: TvkuMatrix4f): TvkuMatrix4f;
var
  i, j: Integer;
begin
  for i := 0 to 3 do
    for j := 0 to 3 do
      result[i, j] := m[j, i];
end;

function vkuMatrixSubMatrix(const m: TvkuMatrix4f; const s, z: Integer): TvkuMatrix3f;
var
  x, y, i, j: Integer;
begin
  for i := 0 to 2 do
    for j := 0 to 2 do begin
      x := i;
      y := j;
      if (i >= s) then
        inc(x);
      if (j >= z) then
        inc(y);
      result[i, j] := m[x, y];
    end;
end;

function vkuMatrixDeterminant(const m: TvkuMatrix3f): Single;
begin
  result :=
    m[0,0] * m[1,1] * m[2,2] +
    m[1,0] * m[2,1] * m[0,2] +
    m[2,0] * m[0,1] * m[1,2] -
    m[2,0] * m[1,1] * m[0,2] -
    m[1,0] * m[0,1] * m[2,2] -
    m[0,0] * m[2,1] * m[1,2];
end;

function vkuMatrixDeterminant(const m: TvkuMatrix4f): Single;
var
  i: Integer;
begin
  result := 0;
  for i := 0 to 3 do
    result := result + power(-1, i) * m[i, 0] * vkuMatrixDeterminant(vkuMatrixSubMatrix(m, i, 0));
end;

function vkuMatrixAdjoint(const m: TvkuMatrix4f): TvkuMatrix4f;
var
  i, j: Integer;
begin
  for i := 0 to 3 do
    for j := 0 to 3 do
      result[i, j] := power(-1, i+j) * vkuMatrixDeterminant(vkuMatrixSubMatrix(m, i, j));
  result := vkuMatrixTranspose(result);
end;

function vkuMatrixInvert(const m: TvkuMatrix4f): TvkuMatrix4f;
var
  d: Single;
  i, j: Integer;
begin
  d := vkuMatrixDeterminant(m);
  result := vkuMatrixAdjoint(m);
  for i := 0 to 3 do
    for j := 0 to 3 do
      result[i,j] := result[i,j] / d;
end;


operator +(const a, b: TvkuQuaternion): TvkuQuaternion;
begin
  result := vkuQuaternionAdd(a, b);
end;

operator -(const l, r: TvkuQuaternion): TvkuQuaternion;
begin
  result := vkuQuaternionSubtract(l, r);
end;

operator *(const l, r: TvkuQuaternion): TvkuQuaternion;
begin
  result := vkuQuaternionMultiply(l, r);
end;

operator *(const q: TvkuQuaternion; const s: Single): TvkuQuaternion;
begin
  result := vkuQuaternionScale(q, s);
end;

function vkuQuaternion(const W, X, Y, Z: Single): TvkuQuaternion;
begin
  Result:= vkuVector4f(W,X,Y,Z);
end;

function vkuQuaternionNormalize(const q: TvkuQuaternion): TvkuQuaternion;
begin
  Result:= q;
  vkuQuaternionNormalizeInplace(Result);
end;

procedure vkuQuaternionNormalizeInplace(var q: TvkuQuaternion);
var
  s: Double;
begin
  s:= sqr(q[quX])+sqr(q[quY])+sqr(q[quZ])+sqr(q[quW]);

  if IsZero(s - 1) then
    exit;
  s:= 1/sqrt(s);
  q[quX]:= q[quX] * s;
  q[quY]:= q[quY] * s;
  q[quZ]:= q[quZ] * s;
  q[quW]:= q[quW] * s;
end;

function vkuQuaternionToVector(const q: TvkuQuaternion): TvkuVector3f;
begin
  Result:= vkuVector3f(q[quX], q[quY], q[quZ]);
end;

function vkuVectorToQuaternion(const v: TvkuVector3f): TvkuQuaternion;
begin
  Result:= vkuQuaternion(0, v[0], v[1], v[2]);
end;


function vkuQuaternionConjugate(const q: TvkuQuaternion): TvkuQuaternion;
begin
  Result[quW] := q[quW];
  Result[quX] := -q[quX];
  Result[quY] := -q[quY];
  Result[quZ] := -q[quZ];
end;

function vkuQuaternionMultiply(const l, r: TvkuQuaternion): TvkuQuaternion;
begin
  Result[quW] := -l[qux] * r[qux] - l[quy] * r[quy] - l[quz] * r[quz] + l[quw] * r[quw];
  Result[quX] :=  l[qux] * r[quw] + l[quy] * r[quz] - l[quz] * r[quy] + l[quw] * r[qux];
  Result[quY] := -l[qux] * r[quz] + l[quy] * r[quw] + l[quz] * r[qux] + l[quw] * r[quy];
  Result[quZ] :=  l[qux] * r[quy] - l[quy] * r[qux] + l[quz] * r[quw] + l[quw] * r[quz];
end;

function vkuQuaternionAdd(const a, b: TvkuQuaternion): TvkuQuaternion;
begin
  Result[quW] := a[quW] + b[quW];
  Result[quX] := a[quX] + b[quX];
  Result[quY] := a[quY] + b[quY];
  Result[quZ] := a[quZ] + b[quZ];
end;

function vkuQuaternionSubtract(const l, r: TvkuQuaternion): TvkuQuaternion;
begin
  Result[quW] := l[quW] - r[quW];
  Result[quX] := l[quX] - r[quX];
  Result[quY] := l[quY] - r[quY];
  Result[quZ] := l[quZ] - r[quZ];
end;

function vkuQuaternionScale(const q: TvkuQuaternion; const f: Single): TvkuQuaternion;
begin
  Result[quW] := q[quW] * f;
  Result[quX] := q[quX] * f;
  Result[quY] := q[quY] * f;
  Result[quZ] := q[quZ] * f;
end;

function vkuQuaternionToMatrix(const q: TvkuQuaternion): TvkuMatrix4f;
var
  qx,qy,qz,qw: Single;
begin
  qw:= q[quW];
  qx:= q[quX];
  qy:= q[quY];
  qz:= q[quZ];
  Result:= vkuMatrixIdentity;
  Result[maAxisX] := vkuVector4f(
    1 - 2*SQR(qy) - 2*SQR(qz),
    2*qx*qy + 2*qz*qw,
    2*qx*qz - 2*qy*qw,
    0);
  Result[maAxisY] := vkuVector4f(
    2*qx*qy - 2*qz*qw,
    1 - 2*SQR(qx) - 2*SQR(qz),
    2*qy*qz + 2*qx*qw,
    0);
  Result[maAxisZ] := vkuVector4f(
    2*qx*qz + 2*qy*qw,
    2*qy*qz - 2*qx*qw,
   	1 - 2*SQR(qx) - 2*SQR(qy),
    0);
end;

function vkuMatrixToQuaternion(const m: TvkuMatrix4f): TvkuQuaternion;
var
  trace, s: double;
  q: TvkuQuaternion;

begin
  trace := m[0][0] + m[1][1] + m[2][2];
  if( trace > 0 ) then begin
    s := 0.5 / SQRT(trace+ 1.0);
    q[quW] := 0.25 / s;
    q[quX] := ( m[2][1] - m[1][2] ) * s;
    q[quY] := ( m[0][2] - m[2][0] ) * s;
    q[quZ] := ( m[1][0] - m[0][1] ) * s;
  end else begin
    if ( m[0][0] > m[1][1]) and (m[0][0] > m[2][2] ) then begin
      s := 2.0 * SQRT( 1.0 + m[0][0] - m[1][1] - m[2][2]);
      q[quW] := (m[2][1] - m[1][2] ) / s;
      q[quX] := 0.25 * s;
      q[quY] := (m[0][1] + m[1][0] ) / s;
      q[quZ] := (m[0][2] + m[2][0] ) / s;
    end else if (m[1][1] > m[2][2]) then begin
      s := 2.0 * SQRT( 1.0 + m[1][1] - m[0][0] - m[2][2]);
      q[quW] := (m[0][2] - m[2][0] ) / s;
      q[quX] := (m[0][1] + m[1][0] ) / s;
      q[quY] := 0.25 * s;
      q[quZ] := (m[1][2] + m[2][1] ) / s;
    end else begin
      s := 2.0 * SQRT( 1.0 + m[2][2] - m[0][0] - m[1][1] );
      q[quW] := (m[1][0] - m[0][1] ) / s;
      q[quX] := (m[0][2] + m[2][0] ) / s;
      q[quY] := (m[1][2] + m[2][1] ) / s;
      q[quZ] := 0.25 * s;
    end;
  end;
  Result:= q;
end;

function vkuQuaternionToRotation(const q: TvkuQuaternion; out angle: Single): TvkuVector3f;
var
  s: double;
begin
  angle := radtodeg(2 * arccos(q[quW]));
  s := sqrt(1-q[quW]*q[quW]);
  if (s < 0.001) then begin

    Result[0] := q[quX];
    Result[1] := q[quY];
    Result[2] := q[quZ];
  end else begin
    Result[0] := q[quX] / s;
    Result[1] := q[quY] / s;
    Result[2] := q[quZ] / s;
  end;
end;

function vkuRotationToQuaternion(const angle: Single; const axis: TvkuVector3f): TvkuQuaternion;
var
  a: single;
begin
  a:= degtorad(angle) / 2;
  Result:= vkuQuaternion(
    cos(a),
    sin(a) * axis[0],
    sin(a) * axis[1],
    sin(a) * axis[2]);
end;

function vkuQuaternionTransformVec(const q: TvkuQuaternion; const v: TvkuVector3f): TvkuVector3f;
var
  p: TvkuQuaternion;
begin
  p:= vkuQuaternionMultiply(q, vkuVectorToQuaternion(v));
  p:= vkuQuaternionMultiply(p, vkuQuaternionConjugate(q));
  Result:= vkuQuaternionToVector(p);
end;

function vkuQuaternionHalfAngle(const q: TvkuQuaternion): TvkuQuaternion;
begin
  Result:= q;
  Result[quW]:= Result[quW] + 1;
  vkuQuaternionNormalizeInplace(Result);
end;

function vkuQuaternionAngleBetween(const a, b: TvkuQuaternion): double;
var
  cosHalfTheta: double;
begin
  cosHalfTheta:= a[quW] * b[quW] + a[quX] * b[quX] + a[quY] * b[quY] + a[quZ] * b[quZ];
  Result:= arccos(cosHalfTheta) * 2;
end;

function vkuQuaternionSlerpOrientation(const a, b: TvkuQuaternion; const t: single): TvkuQuaternion;
var
  qa,qb: TvkuQuaternion;
  cosHalfTheta, sinHalfTheta,
    halfTheta,
    ratioA, ratioB: double;
begin
  qa:= a;
  qb:= b;

  cosHalfTheta:= a[quW] * b[quW] + a[quX] * b[quX] + a[quY] * b[quY] + a[quZ] * b[quZ];
  if (cosHalfTheta < 0) then begin
    qb:= vkuQuaternion(
      -b[quW],
      -b[quX],
      -b[quY],
       b[quZ]
    );
    cosHalfTheta:= -cosHalfTheta;
  end;

	if abs(cosHalfTheta) >= 1.0 then begin
		Result:= qa;
    Exit;
	end;


	halfTheta := arccos(cosHalfTheta);
	sinHalfTheta := sqrt(1.0 - sqr(cosHalfTheta));

	if (abs(sinHalfTheta) < 0.001) then begin
    Result:= vkuQuaternionAdd(vkuQuaternionScale(qa, 0.5), vkuQuaternionScale(qb, 0.5));
    exit
  end;
	ratioA := sin((1 - t) * halfTheta) / sinHalfTheta;
	ratioB := sin(t * halfTheta) / sinHalfTheta;

  Result:= vkuQuaternionAdd(vkuQuaternionScale(qa, ratioA), vkuQuaternionScale(qb, ratioB));
end;

function vkuQuaternionNlerpOrientation(const a, b: TvkuQuaternion; const t: single): TvkuQuaternion;
begin
  Result:= vkuQuaternionAdd(a, vkuQuaternionScale(vkuQuaternionSubtract(b,a), t));
  vkuQuaternionNormalizeInplace(Result);
end;

function vkuQuaternionLookAt(const Location, Target, UpVector: TvkuVector3f): TvkuQuaternion;
var
  front, up, right: TvkuVector3f;
  w4_recip: Single;
begin
  front:= vkuVectorSubtract(Location, Target);
  up:= UpVector;
  vkuVectorOrthoNormalize(front, up);
  right:= vkuVectorProduct(up, front);

  Result[quW]:= SQRT(1 + right[0] + up[1] + front[2]) * 0.5;
  w4_recip:= 1 / (4 * Result[quW]);

  Result[quX]:= (front[1] - up[2]) * w4_recip;
	Result[quY]:= (right[2] - front[0]) * w4_recip;
	Result[quZ]:= (up[0] - right[1]) * w4_recip;
end;

function vkuVectorRotationTo(const a, b: TvkuVector3f): TvkuQuaternion;
var
  d, qw: single;
  ax: TvkuVector3f;
begin
  d:=vkuVectorScalar(a, b);
  ax:= vkuVectorProduct(a, b);
  qw:= vkuVectorLength(a) * vkuVectorLength(b) + d;
	if (qw < 0.0001) then begin
    Result:= vkuQuaternion(0, -a[2],a[1],a[0]);
  end else begin
    Result:= vkuQuaternion(qw, ax[0],ax[1],ax[2]);
  end;
  vkuQuaternionNormalizeInplace(Result);
end;


procedure TvkuFrustum.UpdateProjMatrix;
begin
  if fIsOrthogonal then begin
    fProjMatrix[maAxisX] := vkuVector4f(
      2 / (fRight - fLeft),
      0,
      0,
      0);
    fProjMatrix[maAxisY] := vkuVector4f(
      0,
      2 / (fTop - fBottom),
      0,
      0);
    fProjMatrix[maAxisZ] := vkuVector4f(
      0,
      0,
      -2 / (fFar - fNear),
      0);
    fProjMatrix[maPos] := vkuVector4f(
      -(fRight + fLeft)   / (fRight - fLeft),
      -(fTop   + fBottom) / (fTop   - fBottom),
      -(fFar   + fNear)   / (fFar   - fNear),
      1);
  end else begin
    fProjMatrix[maAxisX] := vkuVector4f(
      2 * fNear / (fRight - fLeft),
      0,
      0,
      0);
    fProjMatrix[maAxisY] := vkuVector4f(
      0,
      2 * fNear / (fTop   - fBottom),
      0,
      0);
    fProjMatrix[maAxisZ] := vkuVector4f(
      (fRight + fLeft) / (fRight - fLeft),
      (fTop + fBottom) / (fTop - fBottom),
      -(fFar + fNear) / (fFar - fNear),
      -1);
    fProjMatrix[maPos] := vkuVector4f(
      0,
      0,
      -2 * fFar * fNear / (fFar - fNear),
      0);
  end;
end;

function TvkuFrustum.GetWidth: Single;
begin
  result := (fRight - fLeft);
end;

function TvkuFrustum.GetProjMatrixPtr: Pointer;
begin
  result := @fProjMatrix[0, 0];
end;

function TvkuFrustum.GetHeight: Single;
begin
  result := (fTop - fBottom);
end;

function TvkuFrustum.GetFOVAngle: Single;
begin
  result := arctan2(Height/2, fNear)/Pi*360;
end;

function TvkuFrustum.GetAspectRatio: Single;
begin
  result := Height / Width;
end;

procedure TvkuFrustum.Frustum(const aLeft, aRight, aBottom, aTop, aNear, aFar: Single);
begin
  fIsOrthogonal := false;
  fTop          := aRight;
  fBottom       := aLeft;
  fLeft         := aBottom;
  fRight        := aTop;
  fNear         := aNear;
  fFar          := aFar;
  UpdateProjMatrix;
end;

procedure TvkuFrustum.Perspective(const aFOVAngle, aAspectRatio, aNear, aFar: Single);
begin
  fIsOrthogonal := false;
  fNear         := aNear;
  fFar          := aFar;
  fTop          := fNear * tan(aFOVAngle / 360 * Pi);
  fBottom       := -fTop;
  fRight        := aAspectRatio * fTop;
  fLeft         := -fRight;
  UpdateProjMatrix;
end;

procedure TvkuFrustum.Ortho(const aLeft, aRight, aBottom, aTop, aNear, aFar: Single);
begin
  fIsOrthogonal := true;
  fLeft         := aLeft;
  fRight        := aRight;
  fTop          := aTop;
  fBottom       := aBottom;
  fNear         := aNear;
  fFar          := aFar;
  UpdateProjMatrix;
end;

constructor TvkuFrustum.Create;
begin
  inherited Create;
  fTop    := 0;
  fBottom := 0;
  fLeft   := 0;
  fRight  := 0;
  fNear   := 0;
  fFar    := 0;
end;

function TvkuCamera.GetPositionPtr: Pointer;
begin
  result := @fPosition[0, 0];
end;

function TvkuCamera.GetInvertPos: TvkuMatrix4f;
begin
  if not fInvertValid then begin
    fInvertValid := true;
    fInvertPos   := vkuMatrixInvert(fPosition);
  end;
  result := fInvertPos;
end;

procedure TvkuCamera.SetPosition(aValue: TvkuMatrix4f);
begin
  fPosition    := aValue;
  fInvertValid := false;
end;

procedure TvkuCamera.Move(const aVec: TvkuVector3f);
begin
  fPosition := vkuMatrixMult(vkuMatrixTranslate(aVec), fPosition);
end;

procedure TvkuCamera.Tilt(const aAngle: Single);
begin
  fPosition := vkuMatrixMult(vkuMatrixRotate(vkuVector3f(1,0,0), aAngle), fPosition);
end;

procedure TvkuCamera.Turn(const aAngle: Single);
begin
  fPosition := vkuMatrixMult(vkuMatrixRotate(vkuVector3f(0,1,0), aAngle), fPosition);
end;

procedure TvkuCamera.Roll(const aAngle: Single);
begin
  fPosition := vkuMatrixMult(vkuMatrixRotate(vkuVector3f(0,0,1), aAngle), fPosition);
end;

function TvkuCamera.GetRay(const aPos: TvkuVector2f): TvkuRayf;
var
  p: TvkuVector3f;
begin
  if (aPos[0] < 0) then
    p[0] := -aPos[0] * fLeft
  else
    p[0] := aPos[0] * fRight;
  if (aPos[1] < 0) then
    p[1] := -aPos[1] * fBottom
  else
    p[1] := aPos[1] * fTop;
  if (fIsOrthogonal) then begin
    p[2] := 0;
    result.p := fPosition * p;
    result.v := fPosition * vkuVector3f(0, 0, -1);
  end else begin
    p[2] := -fNear;
    result.p := vkuVector3f(0, 0, 0);
    result.v := fPosition * p;
  end;
  result := vkuRayNormalize(result);
end;

constructor TvkuCamera.Create;
begin
  inherited Create;
  fPosition    := vkuMatrixIdentity;
  fInvertValid := false
end;

end.

