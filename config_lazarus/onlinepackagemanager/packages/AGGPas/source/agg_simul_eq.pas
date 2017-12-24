
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}
unit
agg_simul_eq;

interface

{$I agg_mode.inc }

uses
  agg_basics;

procedure swap_arrays(a1, a2: double_ptr; n: unsigned);
function matrix_pivot(m: double_ptr; row, Rows, Cols: unsigned): int;
function simul_eq_solve(left, right, result_: double_ptr; Size, RightCols: unsigned): boolean;


implementation

procedure swap_arrays(a1, a2: double_ptr; n: unsigned);
var
  i: unsigned;
  tmp: double;

begin
  i := 0;

  while i < n do
  begin
    tmp := a1^;
    a1^ := a2^;
    a2^ := tmp;

    Inc(ptrcomp(a1), sizeof(double));
    Inc(ptrcomp(a2), sizeof(double));
    Inc(i);

  end;

end;

{ MATRIX_PIVOT }
function matrix_pivot(m: double_ptr; row, Rows, Cols: unsigned): int;
var
  i: unsigned;
  k: int;

  max_val, tmp: double;

begin
  k := row;

  max_val := -1.0;

  i := row;

  while i < Rows do
  begin
    tmp := Abs(double_ptr(ptrcomp(m) + (i * Cols + row) * sizeof(double))^);

    if (tmp > max_val) and (tmp <> 0.0) then
    begin
      max_val := tmp;

      k := i;

    end;

    Inc(i);

  end;

  if double_ptr(ptrcomp(m) + (k * Cols + row) * sizeof(double))^ = 0.0 then
  begin
    Result := -1;

    exit;

  end;

  if k <> row then
  begin
    swap_arrays(
      double_ptr(ptrcomp(m) + k * Cols * sizeof(double)),
      double_ptr(ptrcomp(m) + row * Cols * sizeof(double)),
      Cols);

    Result := k;

    exit;

  end;

  Result := 0;

end;

{ SIMUL_EQ_SOLVE }
function simul_eq_solve(left, right, result_: double_ptr; Size, RightCols: unsigned): boolean;
var
  m: int;

  i, j, k, adx: unsigned;

  a1: double;
  tmp: double_ptr;

label
  return_false, Free;

begin
  // Alloc
  adx := Size + RightCols;

  agg_getmem(pointer(tmp), Size * adx * sizeof(double));

  for i := 0 to Size - 1 do
  begin
    for j := 0 to Size - 1 do
      double_ptr(ptrcomp(tmp) + (i * adx + j) * sizeof(double))^ :=
        double_ptr(ptrcomp(left) + (i * Size + j) * sizeof(double))^;

    for j := 0 to RightCols - 1 do
      double_ptr(ptrcomp(tmp) + (i * adx + Size + j) * sizeof(double))^ :=
        double_ptr(ptrcomp(right) + (i * RightCols + j) * sizeof(double))^;

  end;

  for k := 0 to Size - 1 do
  begin
    if matrix_pivot(tmp, k, Size, Size + RightCols) < 0 then
      goto return_false; // Singularity....

    a1 := double_ptr(ptrcomp(tmp) + (k * adx + k) * sizeof(double))^;
    j := k;

    while j < Size + RightCols do
    begin
      double_ptr(ptrcomp(tmp) + (k * adx + j) * sizeof(double))^ :=
        double_ptr(ptrcomp(tmp) + (k * adx + j) * sizeof(double))^ / a1;

      Inc(j);

    end;

    i := k + 1;

    while i < Size do
    begin
      a1 := double_ptr(ptrcomp(tmp) + (i * adx + k) * sizeof(double))^;
      j := k;

      while j < Size + RightCols do
      begin
        double_ptr(ptrcomp(tmp) + (i * adx + j) * sizeof(double))^ :=
          double_ptr(ptrcomp(tmp) + (i * adx + j) * sizeof(double))^ - a1 * double_ptr(ptrcomp(tmp) + (k * adx + j) * sizeof(double))^;

        Inc(j);

      end;

      Inc(i);

    end;

  end;

  for k := 0 to RightCols - 1 do
  begin
    m := int(Size - 1);

    while m >= 0 do
    begin
      double_ptr(ptrcomp(result_) + (m * RightCols + k) * sizeof(double))^ :=
        double_ptr(ptrcomp(tmp) + (m * adx + Size + k) * sizeof(double))^;

      j := m + 1;

      while j < Size do
      begin
        double_ptr(ptrcomp(result_) + (m * RightCols + k) * sizeof(double))^ :=
          double_ptr(ptrcomp(result_) + (m * RightCols + k) * sizeof(double))^ -
          (double_ptr(ptrcomp(tmp) + (m * adx + j) * sizeof(double))^ * double_ptr(ptrcomp(result_) +
          (j * RightCols + k) * sizeof(double))^);

        Inc(j);

      end;

      Dec(m);

    end;

  end;

  // Done
  Result := True;

  goto Free;

  return_false:
    Result := False;

  // Free
  Free:
    agg_freemem(pointer(tmp), Size * adx * sizeof(double));

end;

end.




