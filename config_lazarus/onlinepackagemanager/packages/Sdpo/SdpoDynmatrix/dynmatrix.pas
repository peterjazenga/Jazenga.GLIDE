{ dynmatrix v3.0

  Copyright (C) 2008-2016 Paulo Costa

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

unit dynmatrix;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  SysUtils;

type

  { TDMatrix }

  TDMatrix = record
  private
    data: array of double;
    rows, cols: integer;

    procedure Init(newrows, newcols: integer);
    procedure TestData(out NumRows, NumCols: integer);

  public
    procedure SaveBin(fname: string);
    procedure LoadBin(fname: string);
    procedure Load(fname: string);
    procedure Save(fname: string);
    procedure SetSize(newrows, newcols: integer);
    procedure setv(r, c: integer; v: double);
    function getv(r, c: integer): double;
    procedure Usetv(r, c: integer; v: double);
    function Ugetv(r, c: integer): double;
    function GetPointer: pdouble;
    function getv1b(r, c: integer): double;

    function IsGood: boolean;
    function NumCols: integer;
    function NumRows: integer;

    function t: TDMatrix;
    property element[r, c : integer]: double read getv write setv; default;
  end;

  TDoubleFunc = function(v: double): double;

function Mzeros(numrows, numcols: integer): TDMatrix;
function Mones(numrows, numcols: integer): TDMatrix;
function Meye(n: integer): TDMatrix;
function Mrandom(numrows, numcols: integer): TDMatrix;
function Minc(numrows, numcols: integer): TDMatrix;
function Mdiag(const D: TDMatrix): TDMatrix;
function MFromArray(numrows, numcols: integer; const D: array of double): TDMatrix;

function Mpow(const M: TDMatrix; n: longword): TDMatrix;
function Mtran(const M: TDMatrix): TDMatrix;
function Minv(const M: TDMatrix): TDMatrix;
function Minv_fast(const M: TDMatrix): TDMatrix;

function MelementMult(const A, B: TDMatrix): TDMatrix;

function MisEqual(const A, B: TDMatrix; eps: double): boolean;
function Mmin(const M: TDMatrix): double;
function Mmax(const M: TDMatrix): double;
function MmaxAbs(const M: TDMatrix): double;
function MTrace(const M: TDMatrix): double;
function MGetDiag(const M: TDMatrix): TDMatrix;

function Mfunc(const A: TDMatrix; f: TDoubleFunc): TDMatrix;

operator + (const A, B: TDMatrix): TDMatrix;
operator + (const A: TDMatrix; k: double): TDMatrix;
operator + (k: double; const A: TDMatrix): TDMatrix;
operator - (const A: TDMatrix): TDMatrix;
operator - (const A, B: TDMatrix): TDMatrix;
operator - (const A: TDMatrix; k: double): TDMatrix;
operator - (k: double; const A: TDMatrix): TDMatrix;
operator * (const A: TDMatrix; k: double): TDMatrix;
operator * (k: double; const A: TDMatrix): TDMatrix;
operator * (const A, B: TDMatrix): TDMatrix;
operator ** (const M: TDMatrix; const n: integer): TDMatrix;

function MHflip(const M: TDMatrix): TDMatrix;
function MConv(const A, B: TDMatrix): TDMatrix;

function MCrop(const M: TDMatrix; uprow, leftcol, downrow, rightcol: integer): TDMatrix;
function MOneCol(const M:TDMatrix; col: integer): TDMatrix;
function MOneRow(const M:TDMatrix; row: integer): TDMatrix;

function MStamp(const M, S: TDMatrix; drow, dcol: integer): TDMatrix;
function MStampCol(const M, S: TDMatrix; col: integer): TDMatrix;
function MStampRow(const M, S: TDMatrix; row: integer): TDMatrix;

function MColSum(const M: TDMatrix): TDMatrix;
function MRowSum(const M: TDMatrix): TDMatrix;

function MColAbsSum(const M: TDMatrix): TDMatrix;
function MColSquareSum(const M: TDMatrix): TDMatrix;
function MColMax(const M: TDMatrix): TDMatrix;
function MColMin(const M: TDMatrix): TDMatrix;
function MColAbsMax(const M: TDMatrix): TDMatrix;

function MColMean(const M: TDMatrix): TDMatrix;
function MColAdd(const M: TDMatrix; const A: TDMatrix): TDMatrix;
function MColSub(const M: TDMatrix; const S: TDMatrix): TDMatrix;

function MColCenter(const M: TDMatrix; out Mean: TDMatrix): TDMatrix;

function MStack(const T, B: TDMatrix): TDMatrix;
function MJoin(const L, R: TDMatrix): TDMatrix;


// Matrix data internal format
// | dynamic Array of Double |
// |<- rows * cols doubles ->|
//  Row-major order

// Missing:
//function Mvflip(M:Matrix): Matrix;

//function MColNorm2(M:Matrix): Matrix;

//procedure MShape(M:Matrix; newrow,newcol: integer);
//procedure MShape2col(M:Matrix);
//procedure MShape2row(M:Matrix);

//function VDotProduct(A: TDMatrix; B: TDMatrix): TDMatrix;
//function VExtProduct(A: TDMatrix; B: TDMatrix): TDMatrix;
//function VNorm1(A: TDMatrix; B: TDMatrix): TDMatrix;
//function VNorm2(A: TDMatrix; B: TDMatrix): TDMatrix;
//function VNormInf(A: TDMatrix; B: TDMatrix): TDMatrix;


implementation

uses math;

// <- Transpose of M
function MTran(const M: TDMatrix): TDMatrix;
var
  r, c: integer;
begin
  result.Init(M.cols, M.rows);
  for c:=0 to M.cols-1 do
    for r:=0 to M.rows-1 do begin
      result.data[r + c * M.rows] := M.data[c + r * M.cols];
    end;
end;

// Zeros matrix
function Mzeros(numrows, numcols: integer): TDMatrix;
begin
  result.Init(numrows, numcols);
end;

// Ones Matrix
function Mones(numrows, numcols: integer): TDMatrix;
var i: integer;
begin
  result.Init(numrows, numcols);
  for i := 0 to numrows * numcols - 1 do begin
    result.data[i] := 1;
  end;
end;


// Identity matrix
function Meye(n: integer): TDMatrix;
var i: integer;
begin
  result.Init(n, n);
  for i := 0 to n - 1 do begin
    result.data[i + i * n] := 1;
  end;
end;

// Returns a Matrix with (numrows, numcols) elements with random values between 0 e 1
function Mrandom(numrows, numcols: integer): TDMatrix;
var i: integer;
begin
  result.Init(numrows, numcols);
  for i := 0 to numrows * numcols - 1 do begin
    result.data[i] := random;
  end;
end;

function Minc(numrows, numcols: integer): TDMatrix;
var i: integer;
begin
  result.Init(numrows, numcols);
  for i := 0 to numrows * numcols - 1 do begin
    result.data[i] := i;
  end;
end;


function Mdiag(const D: TDMatrix): TDMatrix;
var i, n: integer;
begin
  n := D.rows * D.cols;
  result.Init(n, n);
  for i := 0 to n - 1 do begin
    result.data[i + i * n] := D.data[i];
  end;
end;

function MTrace(const M: TDMatrix): double;
var i, n: integer;
begin
  result := 0;
  n := min(M.rows, M.cols);
  for i := 0 to n - 1 do begin
    result += M.data[i + i * M.cols];
  end;
end;

function MGetDiag(const M: TDMatrix): TDMatrix;
var i, n: integer;
begin
  n := min(M.rows, M.cols);
  result.Init(n, 1);
  for i := 0 to n - 1 do begin
    result.data[i] := M.data[i + i * M.cols];
  end;
end;



// <- M^n (power n of a square matrix M) with non-negative, integer n.
function Mpow(const M: TDMatrix; n: longword): TDMatrix;
begin
  result := M ** n;
end;


operator+(const A, B: TDMatrix): TDMatrix;
var i : integer;
begin
  if (A.rows <> B.rows) or (A.cols <> B.cols) then
    raise  Exception.Create(format('Cannot add matrix (%d,%d) with matrix (%d,%d)',[A.rows, A.Cols, B.rows, B.cols]));

  result.Init(A.rows,A.cols);

  for i := 0 to A.rows * A.cols - 1 do begin
    result.data[i] := A.data[i] + B.data[i];
  end;
  
end;


operator+(const A: TDMatrix; k: double): TDMatrix;
var i: integer;
begin
  result.Init(A.rows, A.cols);

  for i := 0 to A.rows * A.cols - 1 do begin
    result.data[i] := A.data[i] + k;
  end;

end;


operator + (k: double; const A: TDMatrix): TDMatrix;
begin
  result := A + k;
end;


// <- -A  ie R(i,j) := -A(i,j)
operator-(const A: TDMatrix): TDMatrix;
var i: integer;
begin
  result.Init(A.rows, A.cols);

  for i := 0 to A.rows * A.cols - 1 do begin
    result.data[i] := - A.data[i];
  end;

end;

// <- A-B  ie R(i,j) := A(i,j) - B(i,j)
operator-(const A, B: TDMatrix): TDMatrix;
var i: integer;
begin
  if (A.rows <> B.rows) or (A.cols <> B.cols) then
    raise  Exception.Create(format('Cannot subtract matrix (%d,%d) with matrix (%d,%d)',[A.rows, A.Cols, B.rows, B.cols]));

  result.Init(A.rows, A.cols);

  for i := 0 to A.rows * A.cols - 1 do begin
    result.data[i] := A.data[i] - B.data[i];
  end;

end;

operator-(const A: TDMatrix; k: double): TDMatrix;
var i: integer;
begin
  result.Init(A.rows, A.cols);

  for i := 0 to A.rows * A.cols - 1 do begin
    result.data[i] := A.data[i] - k;
  end;

end;

operator-(k: double; const A: TDMatrix): TDMatrix;
var i: integer;
begin
  result.Init(A.rows, A.cols);

  for i := 0 to A.rows * A.cols - 1 do begin
    result.data[i] := k - A.data[i];
  end;

end;

// <- A * k (k: double) ie R(i,j) := A(i,j) * k
operator*(const A: TDMatrix; k: double): TDMatrix;
var i: integer;
begin
  result.Init(A.rows, A.cols);

  for i := 0 to A.rows * A.cols - 1 do begin
    result.data[i] := A.data[i] * k;
  end;

end;

// <- k * A (k: double) ie R(i,j) := A(i,j) * k
operator*(k: double; const A: TDMatrix): TDMatrix;
begin
  result := A * k;
end;


// <- A*B
operator*(const A, B: TDMatrix): TDMatrix;
var r,c,i: integer;
  sum: double;
begin
  if A.cols <> B.rows then
    raise Exception.Create(format('Cannot multiply matrix (%d,%d) with matrix (%d,%d)',[A.rows, A.Cols, B.rows, B.cols]));

  result.Init(A.rows, B.cols);

  for r := 0 to A.rows-1 do begin
    for c := 0 to B.cols-1 do begin
      sum := 0;
      for i :=0 to A.cols-1 do begin
        sum := sum + A.data[r*A.cols + i] * B.data[c + i*B.cols];
      end;
      result.data[c + r*B.cols] := sum;
    end;
  end;
end;


// <- M^n (power n of a square matrix M) with non-negative, integer n.
operator**(const M: TDMatrix; const n: integer): TDMatrix;
var np: integer;
    P: TDMatrix;
begin
  if n < 0 then begin
    result := Minv(M)**(-n);
    exit;
  end;
  // Must handle special cases: n = 0, and n = 1
  if n = 0 then begin
    result := Meye(n);
    exit;
  end;

  result := M;

  if n = 1 then exit;

  // General case: n >= 2
  P := M;                         // P holds the current square
  np := n - 1;
  while (np >= 1) do begin
    if (np and 1) = 0 then begin  // np is even, we have a zero in the binary expansion
      np := np div 2;
    end else begin                // np is odd, we have a one in the binary expansion
      np := (np - 1) div 2;
      result := result * P;
    end;
    P := P * P;
  end;
end;


{ TDMatrix }

procedure TDMatrix.Init(newrows, newcols: integer);
begin
  if (newrows < 0) or (newcols < 0) then
    raise Exception.Create(format('Invalid (row = %d, col = $d) value.',[newrows, newcols]));

  rows := NewRows;
  cols := NewCols;
  fillbyte(data, sizeof(data), 0);  // data may be garbage that just looks like an existing array
  Setlength(data, rows * cols);     // Setlength fills the array with zeros
end;

// Inicializes a matrix with numrows lines and numcols columns
procedure TDMatrix.SetSize(NewRows, NewCols: integer);
begin
  rows := NewRows;
  cols := NewCols;
  Setlength(data, rows * cols);
end;

// get a pointer to the double array
function TDMatrix.GetPointer: pdouble;
begin
  Setlength(data, rows * cols); // Make unique
  result := @data[0];
end;


// Write v to Element [r,c]
procedure TDMatrix.setv(r, c: integer; v: double);
begin
  if (r >= Rows) or (c >= Cols) or (r < 0) or (c < 0) then
    raise Exception.Create(format('Invalid (row,col) value. Matrix is (%d,%d), element required is (%d,%d)',[Rows, Cols, r,c]));
  Setlength(data, rows * cols); // Make unique
  data[c + r*Cols] := v;
end;

// Get Element [r,c]
function TDMatrix.getv(r, c: integer): double;
begin
  if (r >= Rows) or (c >= Cols) or (r < 0) or (c < 0) then
    raise Exception.Create(format('Invalid (row,col) value. Matrix is (%d,%d), element required is (%d,%d)',[Rows, Cols, r,c]));
  result := data[c + r*Cols];
end;

// Get Element [r,c] (one based)
function TDMatrix.getv1b(r, c: integer): double;
begin
  if (r > Rows) or (c > Cols) or (r <= 0) or (c <= 0) then
    raise Exception.Create(format('Invalid (row,col) value. Matrix is (%d,%d), element required is (%d,%d)',[Rows, Cols, r - 1, c - 1]));
  result := data[c - 1 + (r - 1) * Cols];
end;


// Write to v Element [r,c] , ignore operation if r,c is out of bounds
procedure TDMatrix.Usetv(r, c: integer; v: double);
begin
  if (r >= Rows) or (c >= Cols) or (r < 0) or (c < 0) then exit;
  Setlength(data, rows * cols);  // Make unique
  data[c + r*Cols] := v;
end;

// Get Element [r,c], 0 if r,c out of bounds
function TDMatrix.Ugetv(r, c: integer): double;
begin
  if (r >= Rows) or (c >= Cols) or (r < 0) or (c < 0) then begin
    result := 0;
    exit;
  end;
  result := data[c + r*Cols];
end;


procedure TDMatrix.TestData(out NumRows, NumCols: integer);
begin
  NumRows := rows;
  NumCols := cols;
  
  if data=nil then
    raise Exception.Create('Invalid matrix: nil data');

  if not (rows > 0) then
    raise  Exception.Create('Invalid number of rows:'+inttostr(rows));

  if not (cols > 0) then
    raise  Exception.Create('Invalid number of columns:'+inttostr(cols));

  if (length(data)) <> (rows * cols) * sizeof(double) then
    raise  Exception.Create('Invalid matrix: incompatible data size');
end;


// Get total number of columns
function TDMatrix.NumCols: integer;
begin
  result := cols;
end;


// Get total number of rows
function TDMatrix.NumRows: integer;
begin
  result := rows;
end;

function TDMatrix.t: TDMatrix;
begin
  result := MTran(Self);
end;


// Test the matrix "goodness":
//  if the number of row and cols is not zero
//  if the declared size is compatible with dynamic array
// Returns true if it is good
function TDMatrix.IsGood: boolean;
begin
  result := false;
  if (pointer(data) = nil) then exit;

  if (rows > 0) and (cols > 0) and ((length(data)) = (rows*cols)) then
    result := True;
end;


procedure TDMatrix.Load(fname: string);
var
  r,c: integer;
  lines,rxc: integer;
  F: TextFile;
  dum: double;
begin
  //result := Mzeros(0,0);
  AssignFile(F, fname);
  Reset(F);
  lines:=0;
  while not eof(F) do begin
    readln(F);
    inc(lines);
  end;
  CloseFile(F);

  AssignFile(F, fname);
  Reset(F);
  rxc := -1;
  dum := 0;  // no warning
  while not eof(F) do begin
    read(F,dum);
    inc(rxc);
  end;
  CloseFile(F);

  if (lines <= 0) or (rxc <= 0) or (((rxc div lines) * lines) <> rxc) then
     raise  Exception.Create('File: ' + fname + ' Bad file format: can not load matrix');

  //Mzeros(lines,rxc div lines);
  rows := lines;
  cols := rxc div lines;
  Setlength(data, rows * cols);

  AssignFile(F, fname);
  Reset(F);
  for r := 0 to lines - 1 do begin
    for c := 0 to (rxc div lines) - 1 do begin
      read(F, dum);
        //setv(r,c,dum);
      data[c + r * Cols] := dum;
    end;
  end;
  CloseFile(F);
end;


procedure TDMatrix.Save(fname: string);
var r, c: integer;
    F: TextFile;
begin
  AssignFile(F, fname);
  Rewrite(F);
  for r := 0 to rows - 1 do begin
    for c := 0 to cols - 1 do begin
      write(F, data[c + r * Cols]);
      write(F,' ');
    end;
    write(F, chr($0d) + chr($0a));
  end;
  CloseFile(F);
end;


procedure TDMatrix.LoadBin(fname: string);
var r, c, row_count, col_count, id, ver: integer;
    F: THandle;
    dum: double;
begin
  if not FileExists(fname) then
    raise Exception.Create('File: ' + fname + ' could not be read');

  F := FileOpen (fname, fmOpenRead);

  FileRead(F, id, sizeof(id));
  ver := id and $FF;
  if id and $FFFFFF00 <> $5DB0B100 then
    raise  Exception.Create('File: ' + fname + ' Incompatible file format: can not load matrix');

  if ver = 0 then
    raise  Exception.Create('File: ' + fname + ' Incompatible file format: can not load matrix');

  row_count := 0;
  col_count := 0;
  FileRead(F, row_count, sizeof(row_count));
  FileRead(F, col_count, sizeof(col_count));

  if (row_count <= 0) or (col_count <= 0) then
     raise  Exception.Create('File: ' + fname + ' Bad file format: can not load matrix');

  //Mzeros(lines,rxc div lines);
  rows := row_count;
  cols := col_count;
  Setlength(data, rows * cols);

  for r := 0 to rows - 1 do begin
    for c := 0 to cols - 1 do begin
      FileRead(F, dum, sizeof(dum));
      data[c + r * Cols] := dum;
    end;
  end;
  FileClose(F);
end;


procedure TDMatrix.SaveBin(fname: string);
var r, c, id: integer;
    F: THandle;
begin
  F := FileCreate(fname);
  if F < 0 then
     raise Exception.Create('File: ' + fname + ' could not be created/opened');

  id := $5DB0B101;
  FileWrite(F, id, sizeof(id));

  FileWrite(F, rows, sizeof(rows));
  FileWrite(F, cols, sizeof(cols));
  for r := 0 to rows - 1 do begin
    for c := 0 to cols - 1 do begin
      FileWrite(F, data[c + r * Cols], sizeof(double));
    end;
  end;
  FileClose(F);
end;


// Returns A+B
function MAdd(A: TDMatrix; B: TDMatrix): TDMatrix; inline;
begin
  result := A + B;
end;


// Returns M^-1
function Minv(const M: TDMatrix): TDMatrix;
var
  ROW, COL: array of integer;
  MatINV, MatTMP: TDmatrix;
  HOLD , I_pivot , J_pivot: integer;
  fv, pivot, abs_pivot, rel_eps: double;
  n, i, j, k, {r, c,} rin, rkn, ck, cj: integer;
begin
//  M.GetData(r, c, Mda);
  if M.cols <> M.rows then // c:= M.cols r := M.rows
    raise Exception.Create('Cannot invert non-square matrix');

  n := M.cols;
  SetLength(ROW, n);
  SetLength(COL, n);
  MatTMP := MZeros(n, n);
  MatINV := M;

  SetLength(MatINV.data, MatINV.rows * MatINV.cols);  // Make unique

  // Set up row and column interchange vectors
  for k := 0  to n-1 do begin
    ROW[k] := k;
    COL[k] := k;
  end;

  // Find largest element
  rel_eps := 0;
  for i := 0 to n-1 do begin
    for j := 0  to n-1 do begin
      fv := abs(MatINV.data[ROW[i]*n + COL[j]]);
      if  fv > rel_eps then begin
        rel_eps := fv ;
      end;
    end;
  end;
  rel_eps := rel_eps * 1e-15;


  // Begin main reduction loop
  for k := 0  to n-1 do begin
    // Find largest element for pivot
    pivot := MatINV.data[ROW[k]*n+COL[k]];
    abs_pivot := abs(pivot);
    I_pivot := k;
    J_pivot := k;
    for i := k to n-1 do begin
      for j := k  to n-1 do begin
        //abs_pivot := abs(pivot);
        fv := MatINV.data[ROW[i]*n+COL[j]];
        if  abs(fv) > abs_pivot then begin
          I_pivot := i;
          J_pivot := j;
          pivot := fv;
          abs_pivot := abs(pivot);
        end;
      end;
    end;
    if abs(pivot) < rel_eps then
      raise Exception.Create(format('Singular matrix: Pivot is %g, max element = %g',[pivot, rel_eps]));

    HOLD := ROW[k];
    ROW[k] := ROW[I_pivot];
    ROW[I_pivot] := HOLD;

    HOLD := COL[k];
    COL[k] := COL[J_pivot];
    COL[J_pivot] := HOLD;

    rkn := ROW[k]*n;
    ck := COL[k];

    // Reduce around pivot
    MatINV.data[rkn + ck] := 1.0 / pivot ;
    for j :=0 to n-1 do begin
      if j <> k  then begin
        cj := COL[j];
        MatINV.data[rkn + cj] := MatINV.data[rkn + cj] * MatINV.data[rkn + ck];
      end;
    end;

    // Inner reduction loop
    for i := 0 to n-1 do begin
      rin := ROW[i]*n;
      if k <> i then begin
        fv := MatINV.data[rin + ck];
        for j := 0 to n-1 do begin
          if  k <> j then begin
            cj := COL[j];
            MatINV.data[rin + cj] := MatINV.data[rin + cj] - fv * MatINV.data[rkn + cj] ;
          end;
        end;
        MatINV.data[rin + ck] := - MatINV.data[rin + ck] * MatINV.data[rkn + ck];
      end;
    end;
  end; // end of main reduction loop

  // Unscramble rows
  for j := 0  to n-1 do begin
    for i := 0 to n-1 do begin
      MatTMP.data[COL[i]] := MatINV.data[ROW[i]*n + j];
    end;
    for i := 0 to n-1 do begin
      MatINV.data[i*n + j] := MatTMP.data[i];
    end;
  end;

  // Unscramble columns
  for i := 0 to n-1 do begin
    for j := 0 to n-1 do begin
      MatTMP.data[ROW[j]] := MatINV.data[i*n + COL[j]];
    end;
    for j := 0 to n-1 do begin
      MatINV.data[i*n+j] := MatTMP.data[j];
    end;
  end;

  result := MatInv;
end;


// Returns M^-1
// Faster and less acurate version
function Minv_fast(const M: TDMatrix): TDMatrix;
var dim,r,c,t,pivrow,k: integer;
  pivmax,pivot: double;
  INV,TMP: TDmatrix;
  ex,pdisp,cdisp:integer;
  dtmp,victim,rk,norm,invnorm: double;
  Mzero : double;
begin
  if M.cols <> M.rows then
    raise Exception.Create('Cannot invert non-square matrix');

  dim := M.rows;
  INV := Meye(dim);
  TMP := M;

  setlength(TMP.data, TMP.cols * TMP.rows);  // Make unique

  MZero := 1e-10;
  for c := 0 to dim - 1 do begin
    // find the greatest pivot in the remaining columns
    pivmax := abs(TMP.data[c + c*dim]);
    pivrow := c;
    for k := c + 1 to dim - 1 do begin
      if abs(TMP.data[c + k*dim]) > pivmax then begin
        pivmax := abs(TMP.data[c + k*dim]);
	pivrow:=k;
      end;
    end;
    pivot:= TMP.data[c + pivrow*dim];
    if abs(pivot) < Mzero then
      raise Exception.Create('Singular matrix: Pivot is '+floattostr(pivot));

    if pivrow <> c then begin
      // swap lines
      pdisp:=pivrow*dim;
      cdisp:=c*dim;
      for ex:=c to dim-1 do begin
        dtmp:=TMP.data[cdisp+ex];
        TMP.data[cdisp+ex]:=TMP.data[pdisp+ex];
        TMP.data[pdisp+ex]:=dtmp;
      end;
      for ex:=0 to dim-1 do begin
	dtmp:=INV.data[cdisp+ex];
        INV.data[cdisp+ex]:=INV.data[pdisp+ex];
        INV.data[pdisp+ex]:=dtmp;
      end;
    end;

    for r:=0 to dim-1 do begin
      if r<>c then begin
        victim:=TMP.data[c+r*dim];
        rk:=-victim/pivot;
        for t:=0 to dim-1 do INV.data[r*dim+t]:= INV.data[r*dim+t] + rk * INV.data[c*dim+t];
        for t:=c+1 to dim-1 do TMP.data[r*dim+t]:= TMP.data[r*dim+t] + rk * TMP.data[c*dim+t];
      end;
    end;
  end;

  // normalize the pivots
  for r := 0 to dim - 1 do begin
    norm := TMP.data[r + r*dim];
    if abs(norm) < Mzero then
       raise Exception.Create('Singular matrix: Pivot has been '+floattostr(norm));
    invnorm := 1.0 / norm;
    for c := 0 to dim - 1 do
	    INV.data[c + r*dim] := INV.data[c + r*dim] * invnorm;
  end;
  result:=INV;
end;

// Compare Matrices: tests if every elements' difference is smaller than eps
function MisEqual(const A, B: TDMatrix; eps: double): boolean;
var i : integer;
begin
  result := false;
  if (A.rows <> B.rows) or (A.cols <> B.cols) then exit;

  for i := 0 to A.rows * A.cols - 1 do begin
    if abs(A.data[i] - B.data[i]) > eps then exit;
  end;
  result := true;
end;


// Returns max M(i,j)
function Mmax(const M: TDMatrix): double;
var i: integer;
begin
  result := M.data[0];
  for i := 1 to M.rows * M.cols - 1 do begin
    if (result < M.data[i]) then result := M.data[i];
  end;
end;


// Returns max |M(i,j)|
function MmaxAbs(const M: TDMatrix): double;
var i: integer;
begin
  result := abs(M.data[0]);
  for i := 1 to M.rows * M.cols - 1 do begin
    if (result < abs(M.data[i])) then result := abs(M.data[i]);
  end;
end;


// Returns A .* B (Element-wise mutiplication)
function MelementMult(const A, B: TDMatrix): TDMatrix;
var i: integer;
begin
  if (A.rows <> B.rows) or (A.cols <> B.cols) then
    raise  Exception.Create(format('Cannot Element-wise mutiply matrix (%d,%d) with matrix (%d,%d)',[A.rows, A.Cols, B.rows, B.cols]));

  result.Init(A.rows,A.cols);

  for i := 0 to A.rows * A.cols - 1 do begin
    result.data[i] := A.data[i] * B.data[i];
  end;

end;

// Returns min M(i,j)
function Mmin(const M: TDMatrix): double;
var i: integer;
begin
  result := M.data[0];
  for i := 1 to M.rows * M.cols - 1 do begin
    if (result > M.data[i]) then result := M.data[i];
  end;
end;


function Mfunc(const A: TDMatrix; f: TDoubleFunc): TDMatrix;
var i: integer;
begin
  result := A;
  SetLength(result.data, result.rows * result.cols);  // Make unique

  for i := 0 to result.rows * result.cols - 1 do begin
    result.data[i] := f(result.data[i]);
  end;
end;




// <- Reverse Columns
function Mhflip(const M: TDMatrix): TDMatrix;
var r,c: integer;
begin
  result.Init(M.rows, M.cols);
  for r := 0 to M.rows - 1 do begin
    for c:= 0 to M.cols-1 do begin
      result.data[c + r * M.rows] := M.data[(M.cols - 1) - c + r * M.cols];
    end;
  end;
end;


// Returns row convolution between A and B
function MConv(const A, B: TDMatrix): TDMatrix;
var ar,br,disp,r,c,i: integer;
    pivot,prod: double;
begin
  result := MZeros(A.rows * B.rows, A.cols + B.cols - 1);

  for ar:=0 to A.rows-1 do begin
    for br:=0 to B.rows-1 do begin
      for disp:=0 to A.cols-1 do begin
        r:=ar*B.rows+br;
        pivot:= A.data[disp+ar*A.cols];
        for i:=0 to B.cols-1 do begin
          prod := pivot * B.data[i+br*B.cols];
          c:=disp+i;
          result.data[c+r*result.cols]:=result.data[c+r*result.cols] + prod;
        end;
      end;
    end;
  end;
end;


// Create a matrix with the elements from array D and with numrows rows and numcols cols
// The number of elements in the array must match the number of elements in the matrix
function MFromArray(numrows, numcols: integer; const D: array of double): TDMatrix;
var i: integer;
begin
  if numrows * numcols <> length(D) then
    raise Exception.Create('Const Array size does not match Matrix size');

  result.init(numrows, numcols);

  for i := 0 to result.cols * result.rows - 1 do begin
    result.data[i] := D[i];
  end;
end;


// Returns a submatrix from M
function MCrop(const M: TDMatrix; uprow, leftcol, downrow, rightcol: integer): TDMatrix;
var rowsize, colsize, r, c: integer;
begin
  rowsize := downrow - uprow + 1;
  colsize := rightcol - leftcol + 1;
  if (rowsize < 1) or (colsize < 1) then
     raise  Exception.Create('Invalid number of rows/cols:' + inttostr(rowsize) + '/' + inttostr(colsize));

  if (downrow > M.rows-1) or (rightcol > M.cols-1) then
     raise  Exception.Create('Invalid number of rows/cols:' + inttostr(downrow) + '/' + inttostr(rightcol));

  result.init(rowsize, colsize);

  for r:=0 to result.rows-1 do begin
    for c:=0 to result.cols-1 do begin
      result.data[c+r*result.cols]:= M.data[c+leftcol+(r+uprow)*M.cols];
    end
  end;
end;


// Returns one col from M
function MOneCol(const M:TDMatrix; col: integer): TDMatrix;
begin
  result := Mcrop(M, 0, col, M.rows - 1, col)
end;


// Returns one row from M
function MOneRow(const M:TDMatrix; row: integer): TDMatrix;
begin
  result := Mcrop(M, row, 0, row, M.cols - 1)
end;



// Returns a matrix with part of matrix M replaced with matrix S
function MStamp(const M, S: TDMatrix; drow, dcol: integer): TDMatrix;
var r,c: integer;
begin
  if (drow + S.rows > M.rows) or (dcol + S.cols > M.cols) then
     raise  Exception.Create(format('Matrix(%d,%d) does not fit im matrix(%d,%d)!',[M.rows, M.cols, S.rows, S.cols]));

  result := M;
  SetLength(result.data, result.rows * result.cols);  // Make unique

  for c:=0 to S.cols-1 do begin
    for r:=0 to S.rows-1 do begin
      result.data[c+dcol+(r+drow)*result.cols]:= S.data[c+r*S.cols];
    end
  end;
end;

// Returns a matrix where one column of M with index col was replaced by S
function MStampCol(const M, S: TDMatrix; col: integer): TDMatrix;
begin
  result := MStamp(M, S, 0, col);
end;

// Returns a matrix where one row of M with index row was replaced by S
function MStampRow(const M, S: TDMatrix; row: integer): TDMatrix;
begin
  result := MStamp(M, S, row, 0);
end;



// Returns a matrix with the sum of all M columns
function MColsum(const M: TDMatrix): TDMatrix;
var r,c: integer;
begin
  result := MZeros(1, M.cols);

  for c:=0 to M.cols-1 do begin
    for r:=0 to M.rows-1 do begin
      result.data[c] := result.data[c] + M.data[c + r * M.cols];
    end
  end;
end;

// Returns a matrix with the sum of all M rows
function MRowSum(const M: TDMatrix): TDMatrix;
var r,c: integer;
begin
  result := MZeros(M.rows, 1);

  for r:=0 to M.rows-1 do begin
    for c:=0 to M.cols-1 do begin
      result.data[r] := result.data[r]+ M.data[c + r * M.cols];
    end
  end;
end;

// Returns a matrix with the sum of squares for all M columns
function MColSquareSum(const M: TDMatrix): TDMatrix;
var r,c: integer;
begin
  result := MZeros(1, M.cols);

  for c:=0 to M.cols-1 do begin
    for r:=0 to M.rows-1 do begin
      result.data[c] := result.data[c] + sqr(M.data[c + r * M.cols]);
    end
  end;
end;


// Returns a matrix with the sum of the absolute value of escah cell for all M columns
function MColAbsSum(const M: TDMatrix): TDMatrix;
var r,c: integer;
begin
  result := MZeros(1, M.cols);

  for c:=0 to M.cols-1 do begin
    for r:=0 to M.rows-1 do begin
      result.data[c] := result.data[c] + abs(M.data[c + r * M.cols]);
    end
  end;
end;


// Returns a matrix with the mean for each M column
function MColMean(const M: TDMatrix): TDMatrix;
var r,c: integer;
begin
  result := MZeros(1, M.cols);

  // Column Sum
  for c:=0 to M.cols - 1 do begin
    for r:=0 to M.rows - 1 do begin
      result.data[c] := result.data[c] + M.data[c + r * M.cols];
    end;
    // Column Mean
    result.data[c] := result.data[c] / M.rows;
  end;

end;


// Returns a matrix with where A[c] is added to the c-ith column of M
function MColAdd(const M: TDMatrix; const A: TDMatrix): TDMatrix;
var r,c: integer;
begin
  if (M.cols <> A.cols) then
    raise Exception.Create(format('MRowAdd error: matrix M(%d,%d) must have the same number of columns as A(%d,%d)',[M.rows, M.Cols, A.rows, A.cols]));
  result := MZeros(M.rows, M.cols);

  for c:=0 to M.cols - 1 do begin
    for r:=0 to M.rows - 1 do begin
      result.data[c + r * M.cols] := M.data[c + r * M.cols] + A.data[c];
    end;
  end;

end;


// Returns a matrix with where S[c] is subtracted to the c-ith column of M
function MColSub(const M: TDMatrix; const S: TDMatrix): TDMatrix;
var r,c: integer;
begin
  if (M.cols <> S.cols) then
    raise Exception.Create(format('MRowSub error: matrix M(%d,%d) must have the same number of columns as S(%d,%d)',[M.rows, M.Cols, S.rows, S.cols]));
  result := MZeros(M.rows, M.cols);

  for c:=0 to M.cols - 1 do begin
    for r:=0 to M.rows - 1 do begin
      result.data[c + r * M.cols] := M.data[c + r * M.cols] - S.data[c];
    end;
  end;

end;

// Returns a matrix with zero mean for each M column
function MColCenter(const M: TDMatrix; out Mean: TDMatrix): TDMatrix;
begin
  Mean := MColMean(M);
  Result := MColSub(M, Mean)
end;


// Returns a matrix with the max value for each M column
function MColMax(const M: TDMatrix): TDMatrix;
var r,c: integer;
begin
  result := MZeros(1, M.cols);

  for c:=0 to M.cols-1 do begin
    result.data[c] := M.data[c];
    for r:=1 to M.rows-1 do begin
      result.data[c] := max(result.data[c], M.data[c + r * M.cols]);
    end
  end;
end;

// Returns a matrix with the min value for each M column
function MColMin(const M: TDMatrix): TDMatrix;
var r,c: integer;
begin
  result := MZeros(1, M.cols);

  for c:=0 to M.cols-1 do begin
    result.data[c] := M.data[c];
    for r:=1 to M.rows-1 do begin
      result.data[c] := min(result.data[c], M.data[c + r * M.cols]);
    end
  end;
end;

// Returns a matrix with the max absolute value for each M column
function MColAbsMax(const M: TDMatrix): TDMatrix;
var r,c: integer;
begin
  result := MZeros(1, M.cols);

  for c:=0 to M.cols-1 do begin
    result.data[c] := M.data[c];
    for r:=1 to M.rows-1 do begin
      result.data[c] := max(result.data[c], abs(M.data[c + r * M.cols]));
    end
  end;
end;


// Returns a matrix where S is Stacked over M
function MStack(const T, B: TDMatrix): TDMatrix;
var i, off: integer;
begin
  if T.cols <> B.cols then
    raise Exception.Create(format('Cannot stack matrix (%d,%d) over matrix (%d,%d)',[T.rows, T.Cols, B.rows, B.cols]));

  result.Init(T.rows + B.rows, T.cols);

  for i := 0 to T.rows * T.cols - 1 do begin
    result.data[i] := T.data [i];
  end;

  off := T.rows * T.cols;
  for i := 0 to B.Rows * T.cols - 1 do begin
    result.data[off + i] := B.data [i];
  end;

end;

// Returns a matrix where S is joined to the left of M
function MJoin(const L, R: TDMatrix): TDMatrix;
var off, row, c: integer;
begin
  if L.rows <> R.rows then
    raise Exception.Create(format('Cannot join matrix (%d,%d) to matrix (%d,%d)',[L.Rows, L.Cols, R.Rows, R.cols]));

  result.Init(L.rows, L.cols + R.cols);


  for row := 0 to result.rows-1 do begin
    for c := 0 to L.cols-1 do begin
      result.data[c + row * result.cols] := L.data [c + row * L.cols];
    end;
    off := L.cols;
    for c := 0 to R.cols-1 do begin
      result.data[off + c + row * result.cols] := R.data [c + row * R.cols];
    end;
  end;

end;


initialization

end.
