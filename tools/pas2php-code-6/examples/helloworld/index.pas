unit index;

{$mode objfpc}{$H+}

interface


implementation

procedure Main;
begin
  WriteLn('<H1>hello, world</H1>');
end;

initialization

{$IFDEF PAS2PHP}
  Main;
{$ENDIF}

end.
