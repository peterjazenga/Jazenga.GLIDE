{ <GeoCompute>

  Copyright (C) 2010 Prajuab Riabroy

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

unit geocompute;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GeoEllipsoids, gpsutilities, Math;

  type
    TZoneHemisphere = (hemEast, hemWest, hemNorth, hemSouth);

    TUTMProjection = class(TPersistent)
    private
      FZoneNo : integer;
      FLongHemis : TZoneHemisphere;
      FLatHemis  : TZoneHemisphere;
      FCM : double; //Central Meridian of Zone.
      FK0 : double; //Scale Factor normally = 0.9996
      FFE : double; //False Easting
      FFN : double; //False Northing = 0 if latitude hemisphere is north, = 10000000 if latitude hemisphere is south.
    public
      property ZoneNo : integer read FZoneNo write FZoneNo;
      property LongHemisphere : TZoneHemisphere read FLongHemis write FLongHemis;
      property LatHemisphere : TZoneHemisphere read FLatHemis write FLatHemis;
      property CentralMeridian : double read FCM write FCM;
      property K0 : double read FK0 write FK0;
      property FalseEasting : double read FFE write FFE;
      property FalseNorthing : double read FFN write FFN;
      constructor Create;
      destructor Destroy; override;
    end;

    TGeo2UTM = class
    private
      FEllipsoid : TEllipsoid;
      FGeoCoor : TCoordinate;
      FUTMCoor : TCoordinate;
      FUTMProj : TUTMProjection;
      procedure SetGeoCoordinate (const geocoor : TCoordinate);
      procedure SetEllipsoid(const Ellipsoid : TEllipsoid);
    public
      property Ellipsoid : TEllipsoid write SetEllipsoid;
      property GeoCoordinate : TCoordinate write SetGeoCoordinate; //input
      property UTMCoordinate : TCoordinate read FUTMCoor;  //output
      procedure Compute;
      constructor Create;
      destructor Destroy; override;
    end;

    //On UTM Grid coordianates we need to know the zone no, latitude hemisphere and
    //longitude hemisphere also. Then we can convert utm coordinates to geographic
    //coordinate (latitude/longitude).
    TUTM2Geo = class
    private
      FEllipsoid : TEllipsoid;
      FGeoCoor : TCoordinate;
      FUTMCoor : TCoordinate;
      FUTMProj : TUTMProjection;
      procedure SetUTMProjection (const utmproj : TUTMProjection);
      procedure SetUTMCoordinate (const utmcoor : TCoordinate);
      procedure SetEllipsoid(Ellipsoid : TEllipsoid);
    public
      property UTMProjection : TUTMProjection read FUTMProj write SetUTMProjection;
      property Ellipsoid : TEllipsoid write SetEllipsoid;
      property UTMCoordinate : TCoordinate write SetUTMCoordinate;  //input
      property GeoCoordinate : TCoordinate read FGeoCoor; //output
      procedure Compute;
      constructor Create;
      destructor Destroy; override;
    end;

    // Great circle methods
    //How to compute GC Azimuth & Distance.
    TGCCompute = class
    private
      FEllipsoid : TEllipsoid;
      FGeoCoor1  : TCoordinate;
      FGeoCoor2  : TCoordinate;
      FGCDist, FGCAzimuth : double;
      procedure SetGeoCoordinate1 (const geocoor1 : TCoordinate);
      procedure SetGeoCoordinate2 (const geocoor2 : TCoordinate);
      procedure SetEllipsoid(const Ellipsoid : TEllipsoid);
    public
      property GeoCoordinate1 : TCoordinate read FGeoCoor1 write SetGeoCoordinate1;
      property GeoCoordinate2 : TCoordinate read FGeoCoor2 write SetGeoCoordinate2;
      property GCDistance : double read FGCDist;
      property GCAzimuth  : double read FGCAzimuth;
      procedure Compute;
      constructor Create;
      destructor Destroy; override;
    end;

    //Ellipsoid/Spheroid methods
    //How to compute Ellipsoid Azimuth & Distance.
    TSpheroidCompute = class
    private
      FEllipsoid : TEllipsoid;
      FGeoCoor1  : TCoordinate;
      FGeoCoor2  : TCoordinate;
      FEllDist, FEllAzimuth12, FEllAzimuth21 : double;
      procedure SetGeoCoordinate1 (const geocoor1 : TCoordinate);
      procedure SetGeoCoordinate2 (const geocoor2 : TCoordinate);
      procedure SetEllipsoid(const Ellipsoid : TEllipsoid);
      function  ApproxDistance(lat1, lon1, lat2, lon2, a, f : double) : double;
      function  EllipseArcLength(lat1, lon1, lat2, a, f : double) : double;
      function  EllipseInverse(lat1, lon1, lat2, lon2 : double) : double;
    public
      property GeoCoordinate1 : TCoordinate read FGeoCoor1 write SetGeoCoordinate1;
      property GeoCoordinate2 : TCoordinate read FGeoCoor2 write SetGeoCoordinate2;
      property EllipseDistance : double read FEllDist;
      property ForwardAzimuth  : double read FEllAzimuth12;
      property ReverseAzimuth  : double read FEllAzimuth21;
      property Ellipsoid : TEllipsoid read FEllipsoid write SetEllipsoid;
      procedure Compute;
      constructor Create;
      destructor Destroy; override;
    end;

implementation
//TUTMProjection
constructor TUTMProjection.Create;
begin
  inherited create;
  FK0 := 0.9996;
  FFE := 500000;
end;

destructor TUTMProjection.Destroy;
begin
  inherited Destroy;
end;

//TGeo2UTM
procedure TGeo2UTM.SetEllipsoid(const Ellipsoid : TEllipsoid);
begin
  FEllipsoid.EllipsoidName := Ellipsoid.EllipsoidName;
  FEllipsoid.MajorAxis := Ellipsoid.MajorAxis;
  FEllipsoid.InvFlattening := Ellipsoid.InvFlattening;
end;

procedure TGeo2UTM.SetGeoCoordinate(const geocoor : TCoordinate);
var
  dLat, dLong : double;
begin
  FGeoCoor := geocoor;
  dLat := FGeoCoor.Y;
  dLong := FGeoCoor.X;

  //set hemisphere
  if (dLat >=0) then
    FUTMProj.LatHemisphere := hemNorth
  else
    FUTMProj.LatHemisphere := hemSouth;

  if (dLong >=0) then
    FUTMProj.LongHemisphere := hemEast
  else
    FUTMProj.LongHemisphere := hemWest;

  //compute zone
   if (FUTMProj.LongHemisphere = hemWest) then
     FUTMProj.ZoneNo := trunc((180 + dLong) / 6) + 1
   else if (FUTMProj.LongHemisphere = hemEast) then
     FUTMProj.ZoneNo := trunc(dLong/6) + 31;

  //compute zone
   if (FUTMProj.LatHemisphere = hemNorth) then
     FUTMProj.FalseNorthing := 0
   else if (FUTMProj.LatHemisphere = hemSouth) then
     FUTMProj.FalseNorthing := 10000000;

   FUTMProj.CentralMeridian := 6 * FUTMProj.ZoneNo - 183;
end;

procedure TGeo2UTM.Compute;
var
  K0, FE, FN : extended;
  lat, long : extended; //in radian.
  a, b, f, e, et2, rm, n, rho, nu, p, cm : extended;
  M, A0, B0, C0, D0, E0 : extended;
  Ki, Kii, Kiii, Kiv, Kv, A6 : extended;

begin
  //Assign to new variable for easy looking at mathematic formula.
  K0 := FUTMProj.K0;
  FE := FUTMProj.FalseEasting;
  FN := FUTMProj.FalseNorthing;
  a := FEllipsoid.MajorAxis;
  f := 1 / FEllipsoid.InvFlattening;
  long := PI/180 * FGeoCoor.X; //convert latitude to radian.
  lat := PI/180 * FGeoCoor.Y;
  cm := PI/180 * FUTMProj.CentralMeridian;
  p := long - cm; //already in radian.

  b := a * (1 - f); //Minor axis;

  // =.08 approximately. This is the eccentricity of the earth's elliptical crosssection.
  e := sqrt(1 - b*b / (a*a));

  //= .007 approximately. The quantity e' only occurs in even powers so it need only be calculated as e'2
  et2 := power((e * a/b), 2);

  n := (a - b) / (a + b);

  //This is the radius of curvature of the earth in the meridian plane.
  rho := a * (1 - e*e) / power (1 - power(e*sin(lat),2), 3/2);

  //This is the radius of curvature of the earth perpendicular to the meridian plane.
  nu := a / sqrt(1 - sqr(e*sin(lat)));

  //=================== compute Meridian Arc===================================
  //A' = a(1 - n + (5/4)(n2- n3) + (81/64)(n4- n5))
  A0 := a * (1 - n + (5.0/4.0)*(n*n - n*n*n) + (81.0/64.0)*(power(n,4) - power(n,5)));
  //B' = (3an/2)[1 - n + (7/8)(n2- n3) + (55/64)(n4- n5) ...]
  B0 := (3*a*n/2) * (1 - n + (7/8)*(n*n - n*n*n) + (55/64)* power(n,4) - power(n,5));
  //C' = (15 tan2/16)[1 - n + (3/4)(n2- n3) ...]
  C0 := (15*a*n*n/16) * (1 - n + (3/4)*(n*n - n*n*n));
  //D' = (35 tan3/48)[1 - n + (11/16)(n2- n3) ...]
  D0 := (35*a*n*n*n/48) * (1 - n + (11/16)*(n*n - n*n*n));
  //E' = (315 tan4/512)[1 - n ...]
  E0 := (315*a*power(n,4)/512) * (1-n);
  //M = A'lat - B'sin(2lat) + C'sin(4lat) - D'sin(6lat) + E'sin(8lat).
  M := A0*lat - B0*sin(2*lat) + C0*sin(4*lat) - D0*sin(6*lat) + E0*sin(8*lat);
  //===========================================================================

  //===================Converting Lat/Long to UTM Grid Coordinate==============
  Ki := M * K0;
  Kii := K0 * nu * sin(2*lat) / 4;
  Kiii := (K0 * nu * sin(lat) * power(cos(lat),3) / 24) * ((5 - tan(lat)*tan(lat)
           + 9*et2 * cos(lat)*cos(lat) + 4 * et2*et2 * power(cos(lat),4)));
  Kiv := K0 * nu * cos(lat);
  Kv := (K0 * nu * power(cos(lat),3)/6) * (1 - tan(lat)*tan(lat) + et2*cos(lat)*cos(lat));
  //Now we 'v got Grid UTM Coordinates.
  FUTMCoor.Y := FN + Ki + Kii*p*p + Kiii*power(p,4);
  FUTMCoor.X := FE + Kiv*p + Kv*p*p*p;
end;

constructor TGeo2UTM.Create;
begin
  inherited create;
  FUTMProj := TUTMProjection.Create;
  FEllipsoid := TEllipsoid.Create;
end;

destructor TGeo2UTM.Destroy;
begin
  FEllipsoid.Free;
  FUTMProj.Free;
  inherited Destroy;
end;


//TUTM2Geo
procedure TUTM2Geo.SetEllipsoid(Ellipsoid : TEllipsoid);
begin
  FEllipsoid.EllipsoidName := Ellipsoid.EllipsoidName;
  FEllipsoid.MajorAxis := Ellipsoid.MajorAxis;
  FEllipsoid.InvFlattening := Ellipsoid.InvFlattening;
end;

procedure TUTM2Geo.SetUTMProjection(const utmproj : TUTMProjection);
begin
   FUTMProj.LongHemisphere := utmproj.LongHemisphere;
   FUTMProj.LatHemisphere := utmproj.LatHemisphere;
   if (utmproj.LatHemisphere = hemNorth) then
     FUTMProj.FalseNorthing := 0
   else if (utmproj.LatHemisphere = hemSouth) then
     FUTMProj.FalseNorthing := 10000000;

   FUTMProj.CentralMeridian := 6 * utmproj.ZoneNo - 183;

end;

procedure TUTM2Geo.SetUTMCoordinate(const utmcoor : TCoordinate);
var
  dN, dE : double;
begin
  FUTMCoor := utmcoor;
  dN := FGeoCoor.Y;
  dE := FGeoCoor.X;
end;

procedure TUTM2Geo.Compute;
var
  K0, FE, FN, a, b, f, e, et2, cm : double;
  mu, M, e1 : double;
  J1, J2, J3, J4 : double;
  C1, T1, R1, N1, D : double;
  fp, Q1, Q2, Q3, Q4, Q5, Q6, Q7 : double;
  lat, long : double;
  x, y : double;
begin
  //Assign to new variable for easy looking at mathematic formula.
  K0 := FUTMProj.K0;
  FE := FUTMProj.FalseEasting;
  FN := FUTMProj.FalseNorthing;
  a := FEllipsoid.MajorAxis;
  f := 1 / FEllipsoid.InvFlattening;
  x := (FE - FUTMCoor.X);
  if (FUTMProj.LatHemisphere = hemNorth) then
    y := FUTMCoor.Y
  else if (FUTMProj.LatHemisphere = hemSouth) then
    y := (10000000 - FUTMCoor.Y);  //temporary

  cm := PI/180 * FUTMProj.CentralMeridian;
  b := a * (1 - f); //Minor axis;

  // =.08 approximately. This is the eccentricity of the earth's elliptical crosssection.
  e := sqrt(1 - b*b / (a*a));

  //= .007 approximately. The quantity e' only occurs in even powers so it need only be calculated as e'2
  et2 := power((e * a/b), 2);

  M := y / K0; // M is Meridian Arc;
  //Compute footprint Latitude.
  //mu = M/[a(1 - e2/4 - 3e4/64 - 5e6/256...)
  mu := M/(a*(1 - e*e/4 - 3/64*power(e,4) - 5/256*power(e,6)));
  //e1 = [1 - (1 - e2)1/2]/[1 + (1 - e2)1/2]
  e1 := (1 - sqrt(1 - e*e))/(1 + sqrt(1 - e*e));
  //J1 = (3e1/2 - 27e13/32 ..)
  J1 := (3*e1/2 - 27*e1*e1*e1/32);
  //J2 = (21e12/16 - 55e14/32 ..)
  J2 := 21*e1*e1/16 - 55*power(e1,4)/32;
  //J3=(151e13/96 ..)
  J3 := 151*e1*e1*e1/96;
  //J4 = (1097e14/512 ..)
  J4 := 1097*power(e1,4)/512;
  //Footprint Latitude fp = mu + J1sin(2mu) + J2sin(4mu) + J3sin(6mu) + J4sin(8mu)
  fp := mu + J1*sin(2*mu) + J2*sin(4*mu) + J3*sin(6*mu) + J4 * sin(8*mu);


  //C1=e'2cos2(fp)
  C1 := et2*cos(fp)*cos(fp);
  //T1 = tan2(fp)
  T1 := tan(fp)*tan(fp);
  //R1 = a(1-e2)/(1-e2sin2(fp))3/2
  R1 := a*(1-e*e)/power(1-e*e*sin(fp)*sin(fp), 1.5);
  //N1 = a/(1-e2sin2(fp))1/2
  N1 := a / sqrt(1 -e*e*sin(fp)*sin(fp));
  //D = x/(N1k0)
  D := x / (N1 * K0);
  //Compute Latitude
  //Q1 = N1 tan(fp)/R1
  Q1 := N1 * tan(fp)/R1;
  //Q2 = (D2/2)
  Q2 := D*D/2;
  //Q3 = (5 + 3T1 + 10C1 - 4C12-9e'2)D4/24
  Q3 := (5 + 3*T1 + 10*C1 - 4*C1*C1-9*et2)*power(D,4)/24;
  //Q4 = (61 + 90T1 + 298C1 +45T12- 3C12-252e'2)D6/720
  Q4 := (61 + 90*T1 + 298*C1 + 45*T1*T1 - 3*C1*C1 - 252*et2)* power(D,6)/720;


  Q5 := D;
  //Q6 = (1 + 2T1 + C1)D3/6
  Q6 := (1 + 2*T1 + C1) *D*D*D/6;
  //Q7 = (5 - 2C1 + 28T1 - 3C12+ 8e'2+ 24T12)D5/120
  Q7 := (5 - 2*C1 + 28*T1 - 3*C1*C1 + 8*et2 + 24*T1*T1)*power(D,5)/120;

  //lat
  lat := 180/PI *(fp - Q1*(Q2 + Q3 + Q4));

  //long = long0 + (Q5 - Q6 + Q7)/cos(fp)
  long := 180/PI *(CM - (Q5 - Q6 + Q7)/cos(fp));
  FGeoCoor.Y := lat;
  FGeoCoor.X := long;
end;

constructor TUTM2Geo.Create;
begin
  inherited create;
  FEllipsoid := TEllipsoid.Create;
  FUTMProj := TUTMProjection.Create;
end;

destructor TUTM2Geo.Destroy;
begin
  FUTMProj.Free;
  FEllipsoid.Free;
  inherited Destroy;
end;

//TGCCompute
procedure TGCCompute.SetGeoCoordinate1 (const geocoor1 : TCoordinate);
begin
  FGeoCoor1 := geocoor1;
end;

procedure TGCCompute.SetGeoCoordinate2 (const geocoor2 : TCoordinate);
begin
  FGeoCoor2 := geocoor2;
end;

procedure TGCCompute.SetEllipsoid(const Ellipsoid : TEllipsoid);
begin
  FEllipsoid.EllipsoidName := Ellipsoid.EllipsoidName;
  FEllipsoid.MajorAxis := Ellipsoid.MajorAxis;
  FEllipsoid.InvFlattening := Ellipsoid.InvFlattening;
end;

procedure TGCCompute.Compute;
begin

end;

constructor TGCCompute.Create;
begin
  inherited create;
  FEllipsoid := TEllipsoid.Create;
end;

destructor TGCCompute.Destroy;
begin
  FEllipsoid.Free;
  inherited Destroy;
end;

// TSpheroidCompute
procedure TSpheroidCompute.SetGeoCoordinate1 (const geocoor1 : TCoordinate);
begin
  FGeoCoor1 := geocoor1;
end;

procedure TSpheroidCompute.SetGeoCoordinate2 (const geocoor2 : TCoordinate);
begin
  FGeoCoor2 := geocoor2;
end;

procedure TSpheroidCompute.SetEllipsoid(const Ellipsoid : TEllipsoid);
begin
  FEllipsoid.EllipsoidName := Ellipsoid.EllipsoidName;
  FEllipsoid.MajorAxis := Ellipsoid.MajorAxis;
  FEllipsoid.InvFlattening := Ellipsoid.InvFlattening;
end;

function  TSpheroidCompute.ApproxDistance(lat1, lon1, lat2, lon2, a, f : double) : double;
var
  FF, G, L, sing, cosl, cosf, sinl, sinf, cosg : double;
  S, C, W, R, H1, H2, D : double;
begin
  lat1 := PI/180 * FGeoCoor1.Y;
  lon1 := PI/180 * FGeoCoor1.X;
  lat2 := PI/180 * FGeoCoor2.Y;
  lon2 := PI/180 * FGeoCoor2.X;

  FF := (lat1 + lat2) / 2.0;
  G := (lat1 - lat2) / 2.0;
  L := (lon1 - lon2) / 2.0;

  sing := sin(G);
  cosl := cos(L);
  cosf := cos(FF);
  sinl := sin(L);
  sinf := sin(FF);
  cosg := cos(G);

  S := sing*sing*cosl*cosl + cosf*cosf*sinl*sinl;
  C := cosg*cosg*cosl*cosl + sinf*sinf*sinl*sinl;
  W := arctan2(sqrt(S),sqrt(C));
  R := sqrt((S*C))/W;
  H1 := (3 * R - 1.0) / (2.0 * C);
  H2 := (3 * R + 1.0) / (2.0 * S);
  //ERAD = a / 1000;
  D := 2 * W * a / 1000;
  result := (D * (1 + f * H1 * sinf*sinf*cosg*cosg -
	 	 f*H2*cosf*cosf*sing*sing));
end;

function  TSpheroidCompute.EllipseArcLength(lat1, lon1, lat2, a, f : double) : double;
var
  b : double;
  numParts : longint;
  multLat : double;
  steps : integer;
  snLat1, snLat2, twoFF, x1, x2, dx, x : double;
  aOneF, dydx, adx, a2, oneF, dlat1, dlat2 : double;
  i, part : integer;
begin
  result := 0.0;
  // if this path crosses the equator (i.e. lat1 & lat2 are different signs)
  // then we have to break the path up into 2 parts, from the southern point
  // up to the equator, then from the equator to the northern point.
  // if this path is entirely on one side of the equator we can do it in one
  // part.
  b := a*(1 - f);

  lat1 := PI/180 * FGeoCoor1.Y;
  lon1 := PI/180 * FGeoCoor1.X;

  numParts := 1;
  multLat := lat1 * lat2;
  if (multLat < 0.0) then
    numParts := 2;

  a2 := a * a;
  oneF := 1 - f;
  aOneF := a * oneF;

  for part := 0 to numParts - 1 do
  begin
    if (numParts = 2) then
    begin
      if (part = 0) then
      begin
        if (lat1 < lat2) then
          dlat1 := lat1
        else
          dlat1 := lat2;
	dlat2 := 0.0;
      end
      else
      begin
	dlat1 := 0.0;
        if (lat1 > lat2) then
          dlat2 := lat1
        else
          dlat2 := lat2;
      end;
    end
    else
    begin
      if (lat1 < lat2) then
        dlat1 := lat1
      else
        dlat1 := lat2;
      if (lat1 > lat2) then
        dlat2 := lat1
      else
        dlat2 := lat2;
    end;

    steps := 100;
    steps := steps + 100 * trunc(0.50 + (dlat2-dlat1));
    if (steps > 4000) then
      steps := 4000;
    snLat1 := sin(dlat1);
    snLat2 := sin(dlat2);
    twoFF := 2 * f - f * f;
    x1 := a * cos(dlat1) / sqrt(1-twoFF*snLat1*snLat1);
    x2 := a * cos(dlat2) / sqrt(1-twoFF*snLat2*snLat2);

    dx := (x2 - x1) / (steps - 1);
    if (dx < 0.0) then
      adx := -dx
    else
      adx := dx;
    for i := 0 to steps - 1 do    //check i until steps-1 or steps-2!!!!!!!!!11
    begin
      x := x1 + dx * i;
      dydx := ((aOneF * sqrt((1.0 - ((x+dx)*(x+dx))/a2))) -
				(aOneF * sqrt((1.0 - (x*x)/a2)))) / dx;
      result := result + adx * sqrt(1.0 + dydx*dydx);
    end;
  end;
end;


procedure TSpheroidCompute.Compute;
begin
  FEllDist := EllipseInverse(FGeoCoor1.Y, FGeoCoor1.X, FGeoCoor2.Y, FGeoCoor2.X);
end;

function TSpheroidCompute.EllipseInverse(lat1, lon1, lat2, lon2 : double) : double;
var
  a, b, f , e : double;
  L, U1, U2 : double;
  sinU1, cosU1, sinU2, cosU2 : double;
  lambda, lambdaP, sinlambda, coslambda, sinsigma : double;
  cossigma, sinalpha, sigma, cosSqalpha, cos2sigmaM : double;
  cossigmaM, C, uSq, AA, BB, deltaSigma : double;
  az1, az2 : double;
  //va : variant;
  iterLimit : integer;
  diff : double;
begin
  lat1 := PI/180 * FGeoCoor1.Y;
  lon1 := PI/180 * FGeoCoor1.X;
  lat2 := PI/180 * FGeoCoor2.Y;
  lon2 := PI/180 * FGeoCoor2.X;

  a := FEllipsoid.MajorAxis;
  f := 1/FEllipsoid.InvFlattening;
  b := a*(1 - f);
  L := lon2-lon1;
  U1 := arctan((1-f) * tan(lat1));
  U2 := arctan((1-f) * tan(lat2));
  sinU1 := sin(U1);
  cosU1 := cos(U1);
  sinU2 := sin(U2);
  cosU2 := cos(U2);
  lambda := L;
  iterLimit := 100;
  repeat
  begin
    sinlambda := sin(lambda);
    coslambda := cos(lambda);
    sinsigma  := sqrt((cosU2*sinLambda) * (cosU2*sinlambda) +
                (cosU1*sinU2-sinU1*cosU2*coslambda) * (cosU1*sinU2-sinU1*cosU2*coslambda));
    if (sinsigma = 0) then
    begin
      result := 0; // co-incident points
      exit;
    end;
    cossigma := sinU1*sinU2 + cosU1*cosU2*coslambda;
    sigma := arctan2(sinsigma, cossigma);
    sinalpha := cosU1 * cosU2 * sinlambda / sinsigma;
    cosSqAlpha := 1 - sinAlpha*sinAlpha;
    cos2SigmaM := cosSigma - 2*sinU1*sinU2/cosSqAlpha;
    if (IsNaN(cos2sigmaM)) then cosSigmaM := 0; // equatorial line: cosSqAlpha=0 (§6)
    C := f/16*cosSqalpha*(4+f*(4-3*cosSqalpha));
    lambdaP := lambda;
    lambda := L + (1-C) * f * sinAlpha *
              (sigma + C*sinSigma*(cos2SigmaM+C*cosSigma*(-1+2*cos2SigmaM*cos2SigmaM)));
    dec(iterLimit);
  end;
  diff := abs(lambda-lambdaP);
  Until ((diff < 1e-12) and (iterLimit > 0));

  if (iterLimit = 0) then
  begin
    result := NaN;   // formula failed to converge
    exit;
  end;

  uSq := cosSqAlpha * (a*a - b*b) / (b*b);
  AA := 1 + uSq/16384*(4096+uSq*(-768+uSq*(320-175*uSq)));
  BB := uSq/1024 * (256+uSq*(-128+uSq*(74-47*uSq)));
  deltaSigma := BB*sinsigma*(cos2sigmaM+BB/4*(cossigma*(-1+2*cos2sigmaM*cos2sigmaM)-
    BB/6*cos2sigmaM*(-3+4*sinsigma*sinsigma)*(-3+4*cos2sigmaM*cos2sigmaM)));
  result := b*AA*(sigma-deltaSigma);  //return Ellipse Distance.
  az1 := arctan2((cosU2*sinlambda),(cosU1*sinU2 - sinU1*cosU2*coslambda));
  az2 := arctan2((cosU1*sinlambda),(-sinU1*cosU2 + cosU1*sinU2*coslambda));
  FEllAzimuth12 := az1 * 180/PI;
  FEllAzimuth21 := az2 * 180/PI;
  if (FEllAzimuth12 < 0) then FEllAzimuth12 := 360 + FEllAzimuth12;
  if (FEllAzimuth21 < 0) then FEllAzimuth21 := 360 + FEllAzimuth21;
end;


constructor TSpheroidCompute.Create;
begin
  inherited create;
  FEllipsoid := TEllipsoid.Create;
end;

destructor TSpheroidCompute.Destroy;
begin
  FEllipsoid.Free;
  inherited Destroy;
end;

end.
                                      .
