program least_squares;

uses sysutils, sdpodynmatrix, dynmatrix, dynmatrixutils;


var Input, X, Y, BetaHat: TDMatrix;
    m: integer;

begin
  input := MFromArray(4, 2,
           [1, 6,
            2, 5,
            3, 7,
            4, 10]);
  m := Input.NumRows;
  X := MJoin(Mones(m, 1), MOneCol(input, 0));
  Y := MOneCol(input, 1);
  BetaHat := Minv(X.t * X) * X.t * Y;

  WriteLn(DMatrixToString(BetaHat));
  ReadLn();
end.

