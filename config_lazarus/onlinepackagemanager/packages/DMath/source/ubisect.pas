{ ******************************************************************
  Bisection method for nonlinear equation
  ****************************************************************** }

unit ubisect;

interface

uses
  utypes;

procedure RootBrack(Func             : TFunc;
                    var X, Y, FX, FY : Float);
{ ------------------------------------------------------------------
  Expands the interval [X,Y] until it contains a root of Func,
  i. e. Func(X) and Func(Y) have opposite signs. The corresponding
  function values are returned in FX and FY.
  ------------------------------------------------------------------ }

procedure Bisect (Func     : TFunc;
                  var X, Y : Float;
                  MaxIter  : Integer;
                  Tol      : Float;
                  var F    : Float);

implementation

procedure RootBrack(Func             : TFunc;
                    var X, Y, FX, FY : Float);
const
  MaxIter = 50;
var
  Iter : Integer;  
begin
  FX := Func(X);
  FY := Func(Y);

  SetErrCode(OptOk);

  Iter := 0;
  
  repeat
    Iter := Iter + 1;
    if (X = Y) or (Iter > MaxIter) then
      begin
        SetErrCode(OptNonConv);
        Exit;
      end;  
          
    if FX * FY < 0.0 then Exit;  { Range is OK }
          
    if Abs(FX) < Abs(FY) then
      begin
        X := X + Gold * (X - Y);
        FX := Func(X)
      end
    else
      begin
        Y := Y + Gold * (Y - X);
        FY := Func(Y)
      end;
  until False;  
end;

procedure Bisect (Func     : TFunc;
                  var X, Y : Float;
                  MaxIter  : Integer;
                  Tol      : Float;
                  var F    : Float);

var
  Iter     : Integer;
  G, Z, FZ : Float;

begin
  Iter := 0;
  SetErrCode(OptOk);

  F := Func(X);

  if MaxIter < 1 then Exit;

  G := Func(Y);

  if F * G >= 0 then
    begin
      RootBrack(Func, X, Y, F, G);
      if MathErr <> OptOk then Exit;
    end;  

  repeat
    Iter := Iter + 1;
    if Iter > MaxIter then
      begin
        SetErrCode(OptNonConv);
        Exit;
      end;

    Z := 0.5 * (X + Y);
    FZ := Func(Z);

    if F * FZ > 0 then
      begin
        X := Z;
        F := FZ;
      end
    else
      begin
        Y := Z;
        G := FZ;
      end;
  until Abs(X - Y) < Tol * (Abs(X) + Abs(Y));

  X := 0.5 * (X + Y);
  F := Func(X);
end;

end.
