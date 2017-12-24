
{**********************************************************************
 Package pl_Graphics32EXT.pkg
 this unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit XGR32_Algebra;

interface

uses   LCLIntf, LCLType, Types, Graphics, SysUtils;


const
{ PI constants }
  HalfPIS : Single = 1.5707963267948966192313216916398;     // = PI/2
  PIS : Single = 3.1415926535897932384626433832795;         //
  PI2S : Single = 6.283185307179586476925286766559;         // = 2 * PI
  InvPIS : Single = 0.31830988618379067153776752674503;     // = 1 / PI
  Inv2PIS : Single = 0.15915494309189533576888376337251;    // = 1 / 2PI
  InvSqrPIS : Single = 0.10132118364233777144387946320973;  // = 1 / PI²
  InvHalfPIS : Single = 0.63661977236758134307553505349006;     // = PI/2

type 
  RealType = Single; 

  TComplex = packed record 
    X : RealType;
    Y : RealType;
   end; 
  TVector = packed record 
    Ampl  : RealType;
    Phase : RealType;
   end; 


function ArcTan2(const Y, X: RealType): RealType;
function ArcSin(const X: Single): Single;
function ArcCos(const X: Single): Single;

function PtInRound(const Center, Pt: TPoint; Radius: Single): Boolean; overload;
function Distance(const Pt1, Pt2: TPoint): Single;

function Complex(X, Y: RealType): TComplex; 
function Vector(Ampl, Phase: RealType): TVector; 

function VectorToComplex(const V : TVector): TComplex; 
function ComplexToVector(const C : TComplex): TVector;overload; 
function ComplexToVector(const C1, C2 : TComplex): TVector;overload; 

function VectorAmpl(const C : TComplex): RealType; 
function VectorPhase(const C : TComplex): RealType;

function RadToDegree360(Rad: RealType): RealType;
function DegreeToRad(Degree: RealType): RealType;
function RadNormalize(Rad: RealType)  : RealType;
function DegreeNormalize(Degree: RealType): RealType;

procedure CartesianToPolar(const X, Y: Single;var Ray, Phi: Single);
procedure PolarToCartesian(const Ray,Phi: Single;var X, Y: Single);

implementation 


uses Math;

function PtInRound(const Center, Pt: TPoint; Radius: Single): boolean;
begin
  Result := Radius > Math.Hypot(Pt.X - Center.X, Pt.Y - Center.Y);
end;

function Distance(const Pt1, Pt2: TPoint): single;
begin
  Result := Math.Hypot(Pt2.X - Pt1.X, Pt2.Y - Pt1.Y);
end;

function Complex(X, Y: RealType): TComplex; 
begin 
  Result.X := X;
  Result.Y := Y; 
end; 

function Vector(Ampl, Phase: RealType): TVector; 
begin 
  Result.Ampl := Ampl; 
  Result.Phase := Phase; 
end; 

function ComplexToVector(const C: TComplex): TVector; 
begin
  Result.Ampl :=  sqrt(C.X*C.X + C.Y*C.Y);
  Result.Phase := ArcTan2(C.Y, C.X); 
end; 

function VectorAmpl(const C : TComplex): RealType;
begin
  Result :=  sqrt(C.X*C.X + C.Y*C.Y); 
end; 

function VectorPhase(const C : TComplex): RealType; 
begin
  Result := ArcTan2(C.Y, C.X); 
end; 

function ComplexToVector(const C1, C2: TComplex): TVector;
begin 

  Result.Ampl := sqrt(sqr(C2.Y-C1.Y)+sqr(C2.X-C1.X));
  Result.Phase := ArcTan2(C2.Y-C1.Y, C2.X-C1.X); 
end; 

function VectorToComplex(const V: TVector): TComplex; 
begin
  Result.X := V.Ampl * cos(V.Phase); 
  Result.Y := V.Ampl * sin(V.Phase); 
end; 

function ArcTan2(const Y, X: RealType): RealType; 
begin 
   Result := 0; 
   if Y = 0 then 
    if X >= 0 then Result := 0 
              else Result := Pi; 
   if IsZero(X, abs(Y)/1E6 ) then 
    begin 
     if Y > 0 then  Result :=  Pi/2; 
     if Y < 0 then  Result := -Pi/2; 
    end 
   else Result := Math.ArcTan2(Y, X); 
end; 

function RadToDegree360(Rad: RealType): RealType; 
const xz = 57.29577951308232;// 180/Pi; 
      zz = 6.283185307179586;// 2*Pi 
var c : integer; 
begin
 c := Floor(Rad/zz);
 Result:= (Rad-zz*c)*xz; 
end; 

function DegreeToRad(Degree: RealType): RealType;
begin 
 Result := Degree * ( Pi/ 180);
end; 

function RadNormalize(Rad: RealType): RealType;
const zz = 6.283185307179586;// 2*Pi 
var c : integer; 
begin
 c := Floor(Rad/zz); 
 Result:= Rad-zz*c; 
end; 

function DegreeNormalize(Degree: RealType):RealType;
var c : integer; 
begin
 c := Floor(Degree/360); 
 Result:= Degree-360*c; 
end;

procedure CartesianToPolar(const X, Y: Single;var Ray, Phi: Single);
begin
  phi := ArcTan2(Y,X);
  Ray := Hypot(X,Y);
end;

procedure PolarToCartesian(const Ray,Phi: Single;var X, Y: Single);
var
  FSin,FCos:float;
begin
 SinCos(phi,FSin,FCos);
 Y := Ray * FSin;
 X := Ray * FCos;
end;

function ArcSin(const X: Single): Single;
begin
  Result := ArcTan2(X, Sqrt(1 - X * X));
end;

function ArcCos(const X: Single): Single;
begin
  Result := ArcTan2(Sqrt(1 - X * X), X) ;
end;

end. 

