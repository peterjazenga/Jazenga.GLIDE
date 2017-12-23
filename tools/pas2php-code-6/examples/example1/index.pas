unit index;

{$mode objfpc}{$H+}

interface


implementation

procedure Main;
var
  LIndex, LSum: integer;
begin
  LSum := 0;
  for LIndex := 0 to 10 do begin
    Inc(LSum, LIndex);
    WriteLn(LIndex, '</BR>');
  end;
  WriteLn('Sum = ', LSum);
end;

initialization

{$IFDEF PAS2PHP}
  Main;
{$ENDIF}

end.
