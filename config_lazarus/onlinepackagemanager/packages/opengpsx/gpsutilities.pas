unit gpsutilities;

interface

uses
LCLType, StrUtils, Math, SysUtils, Types;

     
Const
  DEGREETORADIANS = 0.0174532925199433;
  PI = 3.14159265358979;

type


  //Transformation coordinates
  //Use when transforming from User system <==> windows image system (Pixel)
  // pixel system                   SkyPlot system  +Y
  // +---------------> +x                            ^
  // |                                               |
  // |                                               |
  // |                  <===>                        |
  // |                               -X  <-----------+-----------> X
  // |                                               |
  // |                                               |
  //                                                 |
  //+y                                               |
  //                                                 -Y
  //

  PCoordinate = ^TCoordinate;
  //We need more accuracy than TPoint.
  TCoordinate = record
    X : double;
    Y : double;
  end;

  TCoordinateArray = array of TCoordinate;

  TTransformPara = record
    TX1 : double; //Translation X1
    TY1 : double; //Translation Y1
    RotationAngle : double;
    ScaleX : double;
  end;

function CountParts(sString : string; sDelim : string) : byte;
function Parse(sString : string; iReq : integer; sDelim : string) : string;
function IsNumeric(s : string) : boolean;
function Polar2Cartesian(dist : double; azimuth : double) : TPoint;
function DM2DD(DegreeMinutes : string) : double;
function Cartesian2Pixel(sp : TPoint; tsf : TTransformPara) : TPoint; overload;
function Cartesian2Pixel(sp : TCoordinate; tsf : TTransformPara) : TPoint; overload;
function Coordinate(x, y : double) : TCoordinate;
function DMSToDegree(dDMS : double) : double;
function DegreeToDMS(dDegree : double) : double;
function FindGridAzimuth(dE1, dN1, dE2, dN2 : double) : double;
function FindGridDistance(dE1, dN1, dE2, dN2 : double) : double;
function GetChecksum(const sentence : string) : string;

implementation

function CountParts(sString : string; sDelim : string) : byte;
// returns the number of parts in the string.
var
  textPos : integer;
  maxPos : integer;
  cnt : integer;
begin
    if (length(sDelim) = 0) then sDelim := ',';

    if (Length(sString) = 0) then
    begin
      CountParts := 0;
      exit;
    end;

    textPos := 1;
    maxPos := length(sString);
    cnt := 0;

    while (textPos <= maxPos) do
    begin
      if (MidStr(sString, textPos, 1) = sDelim) then
         cnt := cnt + 1;
      textPos := textPos + 1;
    end;

    if (cnt = 0) then
      result := cnt
    else
      result := cnt + 1;
end;

function Parse(sString : string; iReq : integer; sDelim : string) : string;
var
  sSt : string;
  iCnt : integer;
  iPos : integer;
begin
  if (length(sDelim) = 0) then sDelim := ',';
  sSt := sString + sDelim;
  for iCnt := 1 to iReq do
  begin
    iPos := Pos(sDelim, sSt);
    if (iPos > 0) then
    begin
      if (iCnt = iReq) then     //Requested string
      begin
        result := leftstr(sSt, iPos - 1);
        break;
      end;

      if (iPos = length(sSt)) then //No string left
      begin
        result := '';
        break;
      end;
      sSt := midstr(sSt, iPos + length(sDelim), length(sSt) - iPos);
    end
    else
    begin
      result := sSt;
      break;
    end;
  end;
end;

function IsNumeric(s : string) : boolean;
var
  Code: Integer;
  Value: Double;
begin
  val(s, Value, Code);
  Result := (Code = 0);
end;

function Polar2Cartesian(dist : double; Azimuth : double) : TPoint;
var
  x, y : double;
begin
  x := Sin(PI / 180 * Azimuth) * dist;
  y := Cos(PI / 180 * Azimuth) * dist;
  result.X := trunc(x);
  result.Y := trunc(y);
end;

//Converts Degree Minutes to Decimal Degrees.
function DM2DD(DegreeMinutes : string) : double;
begin
  if (IsNumeric(DegreeMinutes) And (CountParts(DegreeMinutes, '.') = 2)) then
  begin
    if (length(Parse(DegreeMinutes, 1, '.')) = 4) then
    begin
      result := strtofloat(leftstr(Parse(DegreeMinutes, 1, '.'), 2))
                + (strtofloat(rightstr(DegreeMinutes, length(DegreeMinutes) - 2)) / 60);
      exit;
    end;
    if (length(Parse(DegreeMinutes, 1, '.')) = 5) then
    begin
      result := strtofloat(leftstr(Parse(DegreeMinutes, 1, '.'), 3))
                + (strtofloat(rightstr(DegreeMinutes, length(DegreeMinutes) - 3)) / 60);
    end;
  end;
end;

function Cartesian2Pixel(sp : TPoint; tsf : TTransformPara) : TPoint;
var
  dQ, dS : double;
  dY, dX : double;
begin
  dS := tsf.ScaleX;
  dQ := DEGREETORADIANS * tsf.RotationAngle;
  dY := -1 * (dS * sp.X * Sin(dQ) + dS * sp.Y * Cos(dQ)) + tsf.TY1;
  dX := dS * sp.X * Cos(dQ) - dS * sp.Y * Sin(dQ) + tsf.TX1;
  result.X := trunc(dX);
  result.Y := trunc(dY);
end;

function Cartesian2Pixel(sp : TCoordinate; tsf : TTransformPara) : TPoint;
var
  dQ, dS : double;
  dY, dX : double;
begin
  dS := tsf.ScaleX;
  dQ := DEGREETORADIANS * tsf.RotationAngle;
  dY := -1 * (dS * sp.X * Sin(dQ) + dS * sp.Y * Cos(dQ)) + tsf.TY1;
  dX := dS * sp.X * Cos(dQ) - dS * sp.Y * Sin(dQ) + tsf.TX1;
  result.X := trunc(dX);
  result.Y := trunc(dY);
end;

function Coordinate(x, y : double) : TCoordinate;
begin
  result.X := x;
  result.Y := y;
end;

//Converts Degree Minutes Secondto Decimal Degrees.
function DMSToDegree(dDMS : double) : double;
var
  szDMS : string;
  dD, dM, dS, dTmep : double;
  szTemp1, szTemp2 : string;
begin
  szDMS := formatfloat('0.00000000', dDMS);
  if (CountParts(szDMS, '.') = 2) then
  begin
    if (length(Parse(szDMS, 1, '.')) = 3) then               //Ex. '185.42356'
    begin
      dD := strtofloat(leftstr(Parse(szDMS, 1, '.'), 3));    // '185' => 185.0
      szTemp1 := rightstr(szDMS, length(szDMS) - 4);          // '42356'
      Insert('.', szTemp1, 3);                                // '42.356'
      dM := strtofloat(leftstr(Parse(szTemp1, 1, '.'), 2));   // '42' => 42.0
      szTemp2 := rightstr(szTemp1, length(szTemp1) - 3);      // '356'
      Insert('.', szTemp2, 3);                                // '35.6'
      dS := strtofloat(szTemp2);                              // '35.6' => 35.6
      result := dD + dM / 60.0 + dS / 3600.0;
    end;
  end;
end;


function DegreeToDMS(dDegree : double) : double;
var
  szDegree : string;
  dD, dM, dS, dTeMp : double;
  szTemp1, szTemp2 : string;
begin
  szDegree := formatfloat('0.00000000', dDegree);
  if (CountParts(szDegree, '.') = 2) then
  begin
    if (length(Parse(szDegree, 1, '.')) = 3) then              //Ex. '100.933560'
    begin
      dD := strtofloat(leftstr(Parse(szDegree, 1, '.'), 3));   // '100' => 100.0
      szTemp1 := rightstr(szDegree, length(szDegree) - 3);     // '.933560'
      dTemp := 60 * strtofloat(szTemp1);                       // 60 x 0.933560 = 56.0136
      szTemp1 := formatfloat('0.00000000', dTemp);              // '56.013600'
      dM := strtofloat(leftstr(Parse(szDegree, 1, '.'), 2));   // '56' => 56.0
      szTemp2 := rightstr(szTemp1, length(szTemp1) - 2);       // '.013600'
      dS := 60 * strtofloat(szTemp2);                          // 60 x 0.0136 = 0.816
      result := dD + dM / 100.0 + dS / 10000.0;
    end;
  end;
end;

function FindGridAzimuth(dE1, dN1, dE2, dN2 : double) : double;
var
  dDelX, dDelY, dAzi : double;
begin
  dDelX := dN2 - dN1;
  dDelY := dE2 - dE1;

  dAzi := arctan2(dDelY, dDelX) * 180 / PI;
  if (dAzi < 0) then dAzi := dAzi + 360;
  result := dAzi;
end;

function FindGridDistance(dE1, dN1, dE2, dN2 : double) : double;
begin
  result := sqrt(sqr(dN1 - dN2) + sqr(dE1 - dE2));
end;

function strToFloatEx(szTar:string):extended;
begin
  if Trim(szTar)='' then
    result:=0.0
  else
    result:=strToFloat(szTar);
end;

// Calculates the checksum for a sentence
function GetChecksum(const sentence : string) : string;
// Loop through all chars to get a checksum
var
  szTemp, szXor : string;
  checksum : integer;
  i : integer;
  ch : char;
begin
  if (sentence[1] = '$') then
  begin
    checksum := 0;
    szTemp := Parse(sentence, 1, '*');
    szXor := RightStr(szTemp, length(szTemp) - 1);
    for i := 1 to length(szXor) do
    begin
      ch := szXor[i];
      checksum := checksum Xor ord(ch);
    end;
    result := inttohex(checksum, 2);
  end;
end;

end.

