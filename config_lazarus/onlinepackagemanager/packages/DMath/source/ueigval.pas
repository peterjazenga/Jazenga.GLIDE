{ ******************************************************************
  Eigenvalues of a general square matrix
  ****************************************************************** }

unit ueigval;

interface

uses
  utypes, ubalance, uelmhes, uhqr;

procedure EigenVals(A      : TMatrix;
                    Lb, Ub : Integer;
                    Lambda : TCompVector);

implementation

procedure EigenVals(A      : TMatrix;
                    Lb, Ub : Integer;
                    Lambda : TCompVector);
  var
    I_low, I_igh : Integer;
    Scale        : TVector;
    I_int        : TIntVector;
  begin
    DimVector(Scale, Ub);
    DimIntVector(I_Int, Ub);

    Balance(A, Lb, Ub, I_low, I_igh, Scale);
    ElmHes(A, Lb, Ub, I_low, I_igh, I_int);
    Hqr(A, Lb, Ub, I_low, I_igh, Lambda);
  end;

end.
