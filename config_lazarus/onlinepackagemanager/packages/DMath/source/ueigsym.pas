{ ******************************************************************
  Eigenvalues and eigenvectors of a symmetric matrix (SVD method)
  ****************************************************************** }

unit ueigsym;

interface

uses
  utypes, uminmax, usvd;

procedure EigenSym(A      : TMatrix;
                   Lb, Ub : Integer;
                   Lambda : TVector;
                   V      : TMatrix);
{ ------------------------------------------------------------------
  Eigenvalues and eigenvectors of a symmetric matrix by
  singular value decomposition
  ------------------------------------------------------------------
  Input parameters  : A      = matrix
                      Lb     = index of first matrix element
                      Ub     = index of last matrix element
  ------------------------------------------------------------------
  Output parameters : Lambda = eigenvalues in decreasing order
                      V      = matrix of eigenvectors (columns)
  ------------------------------------------------------------------
  Possible results  : MatOk
                      MatNonConv
  ------------------------------------------------------------------
  The eigenvectors are normalized, with their first component > 0
  This procedure destroys the original matrix A
  ------------------------------------------------------------------ }

implementation

procedure EigenSym(A      : TMatrix;
                   Lb, Ub : Integer;
                   Lambda : TVector;
                   V      : TMatrix);
var
  I, J, K : Integer;
  R       : Float;
begin
  SV_Decomp(A, Lb, Ub, Ub, Lambda, V);

  if MathErr <> 0 then Exit;

  { Sort eigenvalues and eigenvectors }
  for I := Lb to Pred(Ub) do
    begin
      K := I;
      R := Lambda[I];
      for J := Succ(I) to Ub do
        if Lambda[J] > R then
          begin
            K := J;
            R := Lambda[J];
          end;

      FSwap(Lambda[I], Lambda[K]);
      for J := Lb to Ub do
        FSwap(V[J,I], V[J,K]);
    end;

  { Make sure that the first component of each eigenvector is > 0 }
  for J := Lb to Ub do
    if V[Lb,J] < 0.0 then
      for I := Lb to Ub do
        V[I,J] := - V[I,J];
end;

end.
