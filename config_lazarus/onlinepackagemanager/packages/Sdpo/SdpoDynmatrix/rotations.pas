{ rotations v1

  Copyright (C) 2012 Paulo Costa

  This library is Free software; you can rediStribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; version 2 of the License.

  This program is diStributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; withOut even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a Copy of the GNU Library General Public License
  along with This library; if not, Write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

  This license has been modified. See File LICENSE.ADDON for more information.
}

unit rotations;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dynmatrix, svd3x3, math;

function ZRotMat(angle: double): TDMatrix;
function YRotMat(angle: double): TDMatrix;
function XRotMat(angle: double): TDMatrix;

function det3(a: pdouble): double;
function optimalRotMat(P, Q: TDMatrix; out Po, Qo: TDMatrix): TDMatrix; overload;
function optimalRotMat(P, Q: TDMatrix): TDMatrix; overload;
function optimalRotMat(A: TDMatrix): TDMatrix; overload;
function BracketX(V: TDMatrix): TDMatrix;

implementation


function BracketX(V: TDMatrix): TDMatrix;
var a: double;
begin
  if (V.NumCols <> 1) or (V.NumRows <> 3) then
    raise Exception.Create(format('Matrix (%d,%d) should be 3x1',[V.NumRows, V.NumCols]));
  result := Mzeros(3, 3);
  a := V.getv(2, 0);
  result.setv(0, 1, -a);
  result.setv(1, 0,  a);

  a := V.getv(1, 0);
  result.setv(0, 2,  a);
  result.setv(2, 0, -a);

  a := V.getv(0, 0);
  result.setv(1, 2, -a);
  result.setv(2, 1,  a);
end;


function ZRotMat(angle: double): TDMatrix;
var s, c: double;
begin
  result := Meye(3);
  s := sin(angle);
  c := cos(angle);
  Result.setv(0, 0, c);
  Result.setv(0, 1,-s);
  Result.setv(1, 0, s);
  Result.setv(1, 1, c);
end;

function YRotMat(angle: double): TDMatrix;
var s, c: double;
begin
  result := Meye(3);
  s := sin(angle);
  c := cos(angle);
  Result.setv(0, 0, c);
  Result.setv(0, 2, s);
  Result.setv(2, 0,-s);
  Result.setv(2, 2, c);
end;

function XRotMat(angle: double): TDMatrix;
var s, c: double;
begin
  result := Meye(3);
  s := sin(angle);
  c := cos(angle);
  Result.setv(1, 1, c);
  Result.setv(1, 2,-s);
  Result.setv(2, 1, s);
  Result.setv(2, 2, c);
end;


function det3(a: pdouble): double;
begin
  result := a[3*0+0] * a[3*1+1] * a[3*2+2]
          + a[3*0+1] * a[3*1+2] * a[3*2+0]
          + a[3*0+2] * a[3*1+0] * a[3*2+1]
          - a[3*0+2] * a[3*1+1] * a[3*2+0]
          - a[3*1+2] * a[3*2+1] * a[3*0+0]
          - a[3*2+2] * a[3*0+1] * a[3*1+0];
end;

// The Kabsch algorithm, named after Wolfgang Kabsch, is a method for calculating
// the optimal rotation matrix that minimizes the RMSD (root mean squared deviation)
// between two paired sets of points.
// http://en.wikipedia.org/wiki/Kabsch_algorithm
function optimalRotMat(P, Q: TDMatrix; out Po, Qo: TDMatrix): TDMatrix; overload;
var A, U, V, Sv, S: TDMatrix;
    d: double;
begin
  P := MColCenter(P, Po);
  Q := MColCenter(Q, Qo);
  A := P.t * Q;
  Sv := MZeros(3, 1);
  U := MZeros(3, 3);
  V := MZeros(3, 3);
  svd3(U.GetPointer, Sv.GetPointer, V.GetPointer, A.t.GetPointer);

  d := sign(det3(A.GetPointer));
  if d = 0 then d := 1;
  S := Meye(3);
  S.setv(2, 2, d);
  result := U.t * S * V;
end;


function optimalRotMat(P, Q: TDMatrix): TDMatrix; overload;
var A, U, V, Sv, S: TDMatrix;
    Po, Qo: TDMatrix;
    d: double;
begin
  P := MColCenter(P, Po);
  Q := MColCenter(Q, Qo);
  A := P.t * Q;
  Sv := MZeros(3, 1);
  U := MZeros(3, 3);
  V := MZeros(3, 3);
  svd3(U.GetPointer, Sv.GetPointer, V.GetPointer, A.t.GetPointer);

  d := sign(det3(A.GetPointer));
  if d = 0 then d := 1;
  S := Meye(3);
  S.setv(2, 2, d);
  result := U.t * S * V;
end;


function optimalRotMat(A: TDMatrix): TDMatrix; overload;
var U, V, Sv, S: TDMatrix;
    d: double;
begin
  Sv := MZeros(3, 1);
  U := MZeros(3, 3);
  V := MZeros(3, 3);
  svd3(U.GetPointer, Sv.GetPointer, V.GetPointer, A.t.GetPointer);

  d := sign(det3(A.GetPointer));
  if d = 0 then d := 1;
  S := Meye(3);
  S.setv(2, 2, d);
  result := U.t * S * V;
end;

end.

