{**********************************************************************                   
 Package pl_Astronomy.pkg
 This unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}


unit StMath;


interface

uses
  LCLIntf, LCLType, LMessages,
  SysUtils, StDate, StConst;

const
  RadCor : Double = 57.29577951308232;    {number of degrees in a radian}


  function IntPower(Base : Extended; Exponent : Integer): Extended;
    {-Raise Base to an integral power Exponent}

  function Power(Base, Exponent : Extended) : Extended;
    {-Raise Base to an arbitrary power Exponent}


function StInvCos(X : Double): Double;
  {-Returns the ArcCos of Y}

function StInvSin(Y : Double): Double;
  {-Returns the ArcSin of Y}

  function StInvTan2(X, Y : Double) : Double;
  {-Returns the ArcTangent of Y / X}

function StTan(A : Double) : Double;
  {-Returns the Tangent of A}


{-------------------------------------------------------}

implementation


  function IntPower(Base : Extended; Exponent : Integer): Extended;
  var
    Y : Integer;
  begin
    Y := Abs(Exponent);
    Result := 1;
    while (Y > 0) do begin
      while (not Odd(Y)) do begin
        Y := Y shr 1;
        Base := Base * Base;
      end;
      Dec(Y);
      Result := Result * Base;
    end;
    if (Exponent < 0) then
      Result := 1 / Result;
  end;

  {-------------------------------------------------------}

  function Power(Base, Exponent: Extended): Extended;
  begin
    if (Exponent = 0) then
      Result := 1
    else if (Base = 0) and (Exponent > 0) then
      Result := 0
    else if (Frac(Exponent) = 0) and (Abs(Exponent) <= MaxInt) then
      Result := IntPower(Base, Trunc(Exponent))
    else
      Result := Exp(Exponent * Ln(Base));
  end;


{-------------------------------------------------------}

function StTan(A : Double) : Double;
var
  C, S : Double;
begin
  C := Cos(A);
  S := Sin(A);
  if (Abs(C) >= 5E-12) then
    Result := S / C
  else if (C < 0) then
    Result := 5.0e-324
  else
    Result := 1.7e+308;
end;

{-------------------------------------------------------}

function StInvTan2(X, Y : Double) : Double;
begin
  if (Abs(X) < 5.0E-12) then begin
    if (X < 0) then
      Result := 3 * Pi / 2
    else
      Result := Pi / 2;
  end else begin
    Result := ArcTan(Y / X);
    if (X < 0) then
      Result := Result + Pi
    else if (Y < 0) then
      Result := Result + 2 * Pi;
  end;
end;

{-------------------------------------------------------}

function StInvSin(Y : Double): Double;
begin
  if (Abs(Abs(Y) - 1) > 5.0E-12) then
    Result := ArcTan(Y / Sqrt(1 - Y * Y))
  else begin
    if (Y < 0) then
      Result := 3 * Pi / 2
    else
      Result := Pi / 2;
  end;
end;

{-------------------------------------------------------}

function StInvCos(X : Double): Double;
begin
  if (Abs(Abs(X) - 1) > 5.0E-12) then
    Result := (90 / RadCor) - ArcTan(X / Sqrt(1 - X * X))
  else begin
    if ((X - Pi / 2) > 0) then
      Result := 0
    else
      Result := Pi;
  end;
end;


end.
