unit index;

{$mode objfpc}{$H+}

interface

implementation

{this program plays the 99 bottles of beer song}

const
  BOTTLESSTART = 99;
  BOTTLESEND = 1;

var
  bottles: integer;

const
  LineBreak = '<BR>';

procedure Main;
begin
  for bottles := BOTTLESSTART downto BOTTLESEND do begin
    if bottles > 1 then begin
      Write(bottles, ' bottles of beer on the wall, ', bottles, ' bottles of beer.' + LineBreak);
      Write('Take one down, pass it around, ');
      Write(bottles - 1, ' bottles of beer on the wall.' + LineBreak);
      Write(LineBreak);
    end else begin
      Write('1 bottle of beer on the wall, one bottle of beer.' + LineBreak);
      Write('Take one down, pass it around, no more bottles of beer on the wall' + LineBreak);
      Write(LineBreak);
      Write('No more bottles of beer on the wall, no more bottles of beer.' + LineBreak);
      Write('Go to the store and buy some more, 99 bottles of beer on the wall.' + LineBreak);
    end;
  end;
end;

initialization

{$IFDEF PAS2PHP}
  Main;
{$ENDIF}

end.
