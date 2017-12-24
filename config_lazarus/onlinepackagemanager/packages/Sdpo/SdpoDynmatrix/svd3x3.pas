{*-
 * Free Pascal version Copyright (c) 2012 Paulo Costa:
 * This is the pascal version of a svd implementation for a 3x3 matrix
 * originaly done in C * by Nathan Lay.
 * Below, is the copyright notice from the original C file.
 * Copyright (c) 2010 Nathan Lay
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR(S) ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR(S) BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *}

unit svd3x3;

{$mode objfpc}

interface

uses
  Classes, SysUtils, math;


///* Computes cross product of 3D vectors x, y and stores the result in z */
//static inline void cross(double * restrict z, const double * restrict x, 	const double * restrict y);
procedure cross(z, x, y: pdouble);

///* Sorts 3 elements */
//static inline void sort3(double * restrict x);
procedure sort3(x: pdouble);

///* Normalizes a 3D vector (with respect to L2) */
//static inline void unit3(double * restrict x);
procedure unit3(x: pdouble);

///*
// * Solves for the roots of a monic cubic polynomial with 3 coefficients
// * ordered by degree that is assumed to have 3 real roots (D <= 0)
// */
//void solvecubic(double * restrict c);
procedure solvecubic(c: pdouble);

///* Computes the LDUP decomposition in-place */
//void ldu3(double * restrict A, int * restrict P);
procedure ldu3(A: pdouble; P: pinteger);

///* Does the backward-solve step, or U*x = y */
//static inline void ldubsolve3(double * restrict x, const double * restrict y,
//	const double * restrict LDU, const int * restrict P);
procedure ldubsolve3(x, y, LDU: pdouble; P: pinteger);

///* Explicitly computes the SVD of a 3x3 matrix */
//void svd3(double * restrict U, double * restrict S, double * restrict V,
//	const double * restrict A);
procedure svd3(U, S, V, A: pdouble);

///* Computes the matrix multiplication C = A*B */
//static inline void matmul3(double * restrict C, const double * restrict A,
//	const double * restrict B);
procedure matmul3(C, A, B: pdouble);

///* Computes the matrix multiplication y = A*x */
//static inline void matvec3(double * restrict y, const double * restrict A,
//	const double * restrict x);
procedure matvec3(y, A, x: pdouble);

///* Computes the matrix multiplication AA = A^T*A */
//static inline void ata3(double * restrict AA, const double * restrict A);
procedure ata3(AA, A: pdouble);

///* Computes the matrix multiplication AA = A*A^T */
//static inline void aat3(double * restrict AA, const double * restrict A);
procedure aat3(AA, A: pdouble);

///* Computes the matrix transpose of A */
//static inline void trans3(double * restrict A);
procedure trans3(A: pdouble);

function cbrt(v: double): double;

implementation

///* Computes cross product of 3D vectors x, y and stores the result in z */
//static inline void cross(double * restrict z, const double * restrict x, 	const double * restrict y);
procedure cross(z, x, y: pdouble);
begin
	z[0] := x[1]*y[2]-x[2]*y[1];
	z[1] := -(x[0]*y[2]-x[2]*y[0]);
	z[2] := x[0]*y[1]-x[1]*y[0];
end;

///* Sorts 3 elements */
//static inline void sort3(double * restrict x);
procedure sort3(x: pdouble);
var tmp: double;
begin
	if (x[0] < x[1]) then begin
		tmp := x[0];
		x[0] := x[1];
		x[1] := tmp;
	end;
	if (x[1] < x[2]) then begin
		if (x[0] < x[2]) then begin
			tmp := x[2];
			x[2] := x[1];
			x[1] := x[0];
			x[0] := tmp;
		end	else begin
			tmp := x[1];
			x[1] := x[2];
			x[2] := tmp;
		end;
	end;
end;

///* Normalizes a 3D vector (with respect to L2) */
//static inline void unit3(double * restrict x);
procedure unit3(x: pdouble);
var tmp: double;
begin
  tmp := sqrt(x[0]*x[0] + x[1]*x[1] + x[2]*x[2]);
  if tmp = 0 then exit;
	x[0] /= tmp;
	x[1] /= tmp;
	x[2] /= tmp;
end;

///* Does the backward-solve step, or U*x = y */
//static inline void ldubsolve3(double * restrict x, const double * restrict y,
//	const double * restrict LDU, const int * restrict P);
procedure ldubsolve3(x, y, LDU: pdouble; P: pinteger);
begin
	x[P[2]] := y[2];
	x[P[1]] := y[1] - LDU[3*P[2]+1]*x[P[2]];
	x[P[0]] := y[0] - LDU[3*P[2]+0]*x[P[2]] - LDU[3*P[1]+0]*x[P[1]];
end;

///* Computes the matrix multiplication C = A*B */
//static inline void matmul3(double * restrict C, const double * restrict A,
//	const double * restrict B);
procedure matmul3(C, A, B: pdouble);
begin
  C[3*0+0] := A[3*0+0]*B[3*0+0] + A[3*1+0]*B[3*0+1] + A[3*2+0]*B[3*0+2];
	C[3*1+0] := A[3*0+0]*B[3*1+0] + A[3*1+0]*B[3*1+1] + A[3*2+0]*B[3*1+2];
	C[3*2+0] := A[3*0+0]*B[3*2+0] + A[3*1+0]*B[3*2+1] + A[3*2+0]*B[3*2+2];

	C[3*0+1] := A[3*0+1]*B[3*0+0] + A[3*1+1]*B[3*0+1] + A[3*2+1]*B[3*0+2];
	C[3*1+1] := A[3*0+1]*B[3*1+0] + A[3*1+1]*B[3*1+1] + A[3*2+1]*B[3*1+2];
	C[3*2+1] := A[3*0+1]*B[3*2+0] + A[3*1+1]*B[3*2+1] + A[3*2+1]*B[3*2+2];

	C[3*0+2] := A[3*0+2]*B[3*0+0] + A[3*1+2]*B[3*0+1] + A[3*2+2]*B[3*0+2];
	C[3*1+2] := A[3*0+2]*B[3*1+0] + A[3*1+2]*B[3*1+1] + A[3*2+2]*B[3*1+2];
	C[3*2+2] := A[3*0+2]*B[3*2+0] + A[3*1+2]*B[3*2+1] + A[3*2+2]*B[3*2+2];
end;

///* Computes the matrix multiplication y = A*x */
//static inline void matvec3(double * restrict y, const double * restrict A,
//	const double * restrict x);
procedure matvec3(y, A, x: pdouble);
begin
	y[0] := A[3*0+0]*x[0] + A[3*1+0]*x[1] + A[3*2+0]*x[2];
	y[1] := A[3*0+1]*x[0] + A[3*1+1]*x[1] + A[3*2+1]*x[2];
	y[2] := A[3*0+2]*x[0] + A[3*1+2]*x[1] + A[3*2+2]*x[2];
end;

///* Computes the matrix multiplication AA = A^T*A */
//static inline void ata3(double * restrict AA, const double * restrict A);
procedure ata3(AA, A: pdouble);
begin
	AA[3*0+0] := A[3*0+0]*A[3*0+0] + A[3*0+1]*A[3*0+1] + A[3*0+2]*A[3*0+2];
	AA[3*1+0] := A[3*0+0]*A[3*1+0] + A[3*0+1]*A[3*1+1] + A[3*0+2]*A[3*1+2];
	AA[3*2+0] := A[3*0+0]*A[3*2+0] + A[3*0+1]*A[3*2+1] + A[3*0+2]*A[3*2+2];

	AA[3*0+1] := AA[3*1+0];
	AA[3*1+1] := A[3*1+0]*A[3*1+0] + A[3*1+1]*A[3*1+1] + A[3*1+2]*A[3*1+2];
	AA[3*2+1] := A[3*1+0]*A[3*2+0] + A[3*1+1]*A[3*2+1] + A[3*1+2]*A[3*2+2];

	AA[3*0+2] := AA[3*2+0];
	AA[3*1+2] := AA[3*2+1];
	AA[3*2+2] := A[3*2+0]*A[3*2+0] + A[3*2+1]*A[3*2+1] + A[3*2+2]*A[3*2+2];
end;

///* Computes the matrix multiplication AA = A*A^T */
//static inline void aat3(double * restrict AA, const double * restrict A);
procedure aat3(AA, A: pdouble);
begin
	AA[3*0+0] := A[3*0+0]*A[3*0+0] + A[3*1+0]*A[3*1+0] + A[3*2+0]*A[3*2+0];
	AA[3*1+0] := A[3*0+0]*A[3*0+1] + A[3*1+0]*A[3*1+1] + A[3*2+0]*A[3*2+1];
	AA[3*2+0] := A[3*0+0]*A[3*0+2] + A[3*1+0]*A[3*1+2] + A[3*2+0]*A[3*2+2];

	AA[3*0+1] := AA[3*1+0];
	AA[3*1+1] := A[3*0+1]*A[3*0+1] + A[3*1+1]*A[3*1+1] + A[3*2+1]*A[3*2+1];
	AA[3*2+1] := A[3*0+1]*A[3*0+2] + A[3*1+1]*A[3*1+2] + A[3*2+1]*A[3*2+2];

	AA[3*0+2] := AA[3*2+0];
	AA[3*1+2] := AA[3*2+1];
	AA[3*2+2] := A[3*0+2]*A[3*0+2] + A[3*1+2]*A[3*1+2] + A[3*2+2]*A[3*2+2];
end;

///* Computes the matrix transpose of A */
//static inline void trans3(double * restrict A);
procedure trans3(A: pdouble);
var	tmp: double;
begin
	tmp := A[3*1+0];
	A[3*1+0] := A[3*0+1];
	A[3*0+1] := tmp;

	tmp := A[3*2+0];
	A[3*2+0] := A[3*0+2];
	A[3*0+2] := tmp;

	tmp := A[3*2+1];
	A[3*2+1] := A[3*1+2];
	A[3*1+2] := tmp;
end;


///*
// * Solves for the roots of a monic cubic polynomial with 3 coefficients
// * ordered by degree that is assumed to have 3 real roots (D <= 0)
// */
//void solvecubic(double * restrict c);
procedure solvecubic(c: pdouble);
var tmp, t, sint, cost: double;
    sq3d2, c2d3, c2sq, Q, R: double;
begin
  sq3d2 := 0.86602540378443864676;
  c2d3 := c[2]/3;
  c2sq := c[2]*c[2];
  Q := (3*c[1]-c2sq)/9;
  R := (c[2]*(9*c[1]-2*c2sq)-27*c[0])/54;

  if (Q < 0) then begin
		///*
		// * Instead of computing
		// * c_0 = A cos(t) - B
		// * c_1 = A cos(t + 2 pi/3) - B
		// * c_2 = A cos(t + 4 pi/3) - B
		// * Use cos(a+b) = cos(a) cos(b) - sin(a) sin(b)
		// * Keeps t small and eliminates 1 function call.
		// * cos(2 pi/3) = cos(4 pi/3) = -0.5
		// * sin(2 pi/3) = sqrt(3)/2
		// * sin(4 pi/3) = -sqrt(3)/2
		// */

		tmp := 2*sqrt(-Q);
		t := arccos(max(-1, min(1, R/sqrt(-Q*Q*Q))))/3;
		//t := arccos(R/sqrt(-Q*Q*Q))/3;
		cost := tmp*cos(t);
		sint := tmp*sin(t);

		c[0] := cost - c2d3;

		cost := -0.5*cost - c2d3;
		sint := sq3d2*sint;

		c[1] := cost - sint;
		c[2] := cost + sint;
	end	else begin
		tmp := cbrt(R);
    //tmp := power(R, 1/3);
		c[0] := -c2d3 + 2*tmp;
		c[1] := -c2d3 - tmp;
    c[2] := c[1];
	end;
end;

function cbrt(v: double): double;
begin
  if v > 0 then begin
    result := exp(1/3 * ln(v));
  end else if v < 0 then begin
    result := - exp(1/3 * ln(abs(v)));
  end else begin
    result := 0;
  end;
end;

{function iff(cond: boolean; TrueValue, FalseValue: double): double;
begin
  if cond then begin
    result := TrueValue;
  end else begin
    result := F
  end;
end;}

///* Computes the LDUP decomposition in-place */
//void ldu3(double * restrict A, int * restrict P);
procedure ldu3(A: pdouble; P: pinteger);
var	tmp: integer;
begin
	P[1] := 1;
	P[2] := 2;


	P[0] := ifthen(abs(A[3*1+0]) > abs(A[3*0+0]),
            		(ifthen(abs(A[3*2+0]) > abs(A[3*1+0]), 2, 1)),
		            (ifthen(abs(A[3*2+0]) > abs(A[3*0+0]), 2, 0)));
	P[round(P[0])] := 0;

	if (abs(A[3*P[2]+1]) > abs(A[3*P[1]+1])) then begin
		tmp := P[1];
		P[1] := P[2];
		P[2] := tmp;
	end;

	if (A[3*P[0]+0] <> 0) then begin
		A[3*P[1]+0] := A[3*P[1]+0]/A[3*P[0]+0];
		A[3*P[2]+0] := A[3*P[2]+0]/A[3*P[0]+0];
		A[3*P[0]+1] := A[3*P[0]+1]/A[3*P[0]+0];
		A[3*P[0]+2] := A[3*P[0]+2]/A[3*P[0]+0];
	end;

	A[3*P[1]+1] := A[3*P[1]+1] - A[3*P[0]+1]*A[3*P[1]+0]*A[3*P[0]+0];

	if (A[3*P[1]+1] <> 0) then begin
		A[3*P[2]+1] := (A[3*P[2]+1] - A[3*P[0]+1]*A[3*P[2]+0]*A[3*P[0]+0])/A[3*P[1]+1];
		A[3*P[1]+2] := (A[3*P[1]+2] - A[3*P[0]+2]*A[3*P[1]+0]*A[3*P[0]+0])/A[3*P[1]+1];
	end;

	A[3*P[2]+2] := A[3*P[2]+2] - A[3*P[0]+2]*A[3*P[2]+0]*A[3*P[0]+0] - A[3*P[1]+2]*A[3*P[2]+1]*A[3*P[1]+1];

end;

///* Explicitly computes the SVD of a 3x3 matrix */
//void svd3(double * restrict U, double * restrict S, double * restrict V,
//	const double * restrict A);
procedure svd3(U, S, V, A: pdouble);
var thr: double;
	//int P[3], j, k;
	//double y[3], AA[3][3], LDU[3][3], tmp;
  k: integer;
  P: array[0..2] of integer;
  Y: array[0..2] of double;
  AA: array[0..2, 0..2] of double;
  LDU: array[0..2, 0..2] of double;
begin
  thr := 1e-10;
	///*
	// * Steps:
	// * 1) Use eigendecomposition on A^T A to compute V.
	// * Since A = U S V^T then A^T A = V S^T S V^T with D = S^T S and V the
	// * eigenvalues and eigenvectors respectively (V is orthogonal).
	// * 2) Compute U from A and V.
	// * 3) Normalize columns of U and V and root the eigenvalues to obtain
	// * the singular values.
	// */

	///* Compute AA = A^T A */
	ata3(pdouble(@AA[0, 0]), A);

	///* Form the monic characteristic polynomial */
	S[2] := -AA[0][0] - AA[1][1] - AA[2][2];
	S[1] := AA[0][0]*AA[1][1] + AA[2][2]*AA[0][0] + AA[2][2]*AA[1][1] -
		      AA[2][1]*AA[1][2] - AA[2][0]*AA[0][2] - AA[1][0]*AA[0][1];
	S[0] := AA[2][1]*AA[1][2]*AA[0][0] + AA[2][0]*AA[0][2]*AA[1][1] + AA[1][0]*AA[0][1]*AA[2][2] -
		      AA[0][0]*AA[1][1]*AA[2][2] - AA[1][0]*AA[2][1]*AA[0][2] - AA[2][0]*AA[0][1]*AA[1][2];

	///* Solve the cubic equation. */
	solvecubic(S);

	///* All roots should be positive */
	if (S[0] < 0) then
		S[0] := 0;
	if (S[1] < 0) then
		S[1] := 0;
	if (S[2] < 0) then
		S[2] := 0;

	///* Sort from greatest to least */
	sort3(S);

	///* Form the eigenvector system for the first (largest) eigenvalue */
	//memcpy(LDU,AA,sizeof(LDU));
  LDU := AA;
	LDU[0][0] -= S[0];
	LDU[1][1] -= S[0];
	LDU[2][2] -= S[0];

	///* Perform LDUP decomposition */
	ldu3(pdouble(@LDU[0, 0]), P);

	///*
	// * Write LDU = AA-I*lambda.  Then an eigenvector can be
	// * found by solving LDU x = LD y = L z = 0
	// * L is invertible, so L z = 0 implies z = 0
	// * D is singular since det(AA-I*lambda) = 0 and so
	// * D y = z = 0 has a non-unique solution.
	// * Pick k so that D_kk = 0 and set y = e_k, the k'th column
	// * of the identity matrix.
	// * U is invertible so U x = y has a unique solution for a given y.
	// * The solution for U x = y is an eigenvector.
	// */

	///* Pick the component of D nearest to 0 */
	y[0] := 0;
  y[1] := 0;
  y[2] := 0;
	k := ifthen(abs(LDU[P[1]][1]) < abs(LDU[P[0]][0]),
              ifthen(abs(LDU[P[2]][2]) < abs(LDU[P[1]][1]), 2, 1),
              ifthen(abs(LDU[P[2]][2]) < abs(LDU[P[0]][0]), 2, 0));
	y[k] := 1;

	///* Do a backward solve for the eigenvector */
	ldubsolve3(V+(3*0+0), y, pdouble(@LDU[0, 0]), P);

	///* Form the eigenvector system for the last (smallest) eigenvalue */
	//memcpy(LDU,AA,sizeof(LDU));
  LDU := AA;
	LDU[0][0] -= S[2];
	LDU[1][1] -= S[2];
	LDU[2][2] -= S[2];

	///* Perform LDUP decomposition */
	ldu3(pdouble(@LDU[0, 0]), P);

	///*
	// * NOTE: The arrangement of the ternary operator output is IMPORTANT!
	// * It ensures a different system is solved if there are 3 repeat eigenvalues.
	// */

	///* Pick the component of D nearest to 0 */
	y[0] := 0;
  y[1] := 0;
  y[2] := 0;
	k := ifthen(abs(LDU[P[0]][0]) < abs(LDU[P[2]][2]),
              ifthen(abs(LDU[P[0]][0]) < abs(LDU[P[1]][1]), 0, 1),
              ifthen(abs(LDU[P[1]][1]) < abs(LDU[P[2]][2]), 1, 2));
	y[k] := 1;

	///* Do a backward solve for the eigenvector */
	ldubsolve3(V+(3*2+0), y, pdouble(@LDU), P);

	 ///* The remaining column must be orthogonal (AA is symmetric) */
	cross(V+(3*1+0), V+(3*2+0), V+(3*0+0));

	///* Count the rank */
	k := ifthen(S[0] > thr, 1, 0) + ifthen(S[1] > thr, 1, 0) + ifthen(S[2] > thr, 1, 0);

	case k of
		0: begin
			///*
			// * Zero matrix.
			// * Since V is already orthogonal, just copy it into U.
			// */
			//memcpy(U,V,9*sizeof(double));
      move(V^, U^, 9 * sizeof(double));
      end;
		1: begin
			///*
			// * The first singular value is non-zero.
			// * Since A = U S V^T, then A V = U S.
			// * A V_1 = S_11 U_1 is non-zero. Here V_1 and U_1 are
			// * column vectors. Since V_1 is known, we may compute
			// * U_1 = A V_1.  The S_11 factor is not important as
			// * U_1 will be normalized later.
			// */
			matvec3(U+(3*0+0), A, V+(3*0+0));

			///*
			// * The other columns of U do not contribute to the expansion
			// * and we may arbitrarily choose them (but they do need to be
			// * orthogonal). To ensure the first cross product does not fail,
			// * pick k so that U_k1 is nearest 0 and then cross with e_k to
			// * obtain an orthogonal vector to U_1.
			// */
			y[0] := 0;
      y[1] := 0;
      y[2] := 0;
			k := ifthen(abs(U[3*0+0]) < abs(U[3*0+2]),
		 	            ifthen(abs(U[3*0+0]) < abs(U[3*0+1]), 0, 1),
				          ifthen(abs(U[3*0+1]) < abs(U[3*0+2]), 1, 2));
			y[k] := 1;

			cross(U+(3*1+0), y, U+(3*0+0));

			///* Cross the first two to obtain the remaining column */
			cross(U+(3*2+0), U+(3*0+0), U+(3*1+0));
      end;
		2: begin
			///*
			// * The first two singular values are non-zero.
			// * Compute U_1 = A V_1 and U_2 = A V_2. See case 1
			// * for more information.
			// */
			matvec3(U+(3*0+0), A, V+(3*0+0));
			matvec3(U+(3*1+0), A, V+(3*1+0));

			///* Cross the first two to obtain the remaining column */
			cross(U+(3*2+0), U+(3*0+0), U+(3*1+0));
      end;
		3: begin
			///*
			// * All singular values are non-zero.
			// * We may compute U = A V. See case 1 for more information.
			// */
			matmul3(U, A, V);
      end;
	end;

	///* Normalize the columns of U and V */
	unit3(V+(3*0+0));
	unit3(V+(3*1+0));
	unit3(V+(3*2+0));

	unit3(U+(3*0+0));
	unit3(U+(3*1+0));
	unit3(U+(3*2+0));

	///* S was initially the eigenvalues of A^T A = V S^T S V^T which are squared. */
	S[0] := sqrt(S[0]);
	S[1] := sqrt(S[1]);
	S[2] := sqrt(S[2]);
end;


end.



