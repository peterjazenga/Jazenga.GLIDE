{ dynmatrixutils v0.5

  CopyRight (C) 2008-2012 Paulo Costa, Armando Sousa

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

  This license has been modified. See File LICENSE.ADDON for more inFormation.
}

unit dynmatrixutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dynmatrix;

// Fill StringGrid with matrix elements
//procedure DMatrixToGrid(SG: TStringGrid; M: TDMatrix);

// Fill matrix with StringGrid values
//function StringGridToMDatrix(SG: TStringGrid) : TDMatrix;

// Converts string representation to a TDMatrix
function StringListToDMatrix(SL: TStrings; ItemSeparators: string = ' '): TDMatrix;

// Adds to TS all the elements from matrix A, line by line
procedure MAddToStringList(M: TDMatrix; TS: TStrings; FormatString: string = '%.6g'; ItemSeparator: string = ' ');

// Adds to TS the specified label
// Then adds all the elements from matrix A, line by line
procedure MAddToStringListWL(caption: string; M: TDMatrix; TS: TStrings; FormatString: string = '%.6g'; ItemSeparator: string = ' ');

// Converts a String representation to a TDMatrix
// a11 a12 a13; a12 a22 a23;
function StringToDMatrix(str: string): TDMatrix;
function DMatrixToString(M: TDMatrix; FormatString: string = '%.6g'; ItemSeparator: string = ' '; LineSeparator: string = ';' + chr($0d) + chr($0A)): string;


function ArrayToDMatrixRow(d: array of double): TDMatrix;
function ArrayToDMatrixCol(d: array of double): TDMatrix;


implementation



{// Fill StringGrid with matrix elements
procedure DMatrixToGrid(SG: TStringGrid; M: TDMatrix);
var r,c: integer;
begin
  SG.RowCount := integer(M.NumRows) + SG.FixedRows;
  SG.ColCount := integer(M.NumCols) + SG.FixedCols;

  for r := 0 to M.NumRows - 1 do begin
    for c := 0 to M.NumCols - 1 do begin
      SG.cells[c + SG.FixedCols, r + SG.FixedRows] := FloatToStr(M.getv(r, c));
    end;
  end;
end;


// Fill matrix with StringGrid values
function StringGridToMDatrix(SG: TStringGrid) : TDMatrix;
var r,c: integer;
begin
  result := Mzeros(SG.RowCount - SG.FixedRows, SG.ColCount - SG.FixedCols);

  for r := 0 to result.NumRows - 1 do begin
    for c := 0 to result.NumCols - 1 do begin
      result.setv(r, c, StrToFloat(SG.Cells[c + SG.Fixedcols, r + SG.Fixedrows]));
    end;
  end;
end;}


// Fill each line of a TStringList with the text that is found between the separator
// chars given by separatorList.
// Consecutive separators are treated as one.
procedure ParseString(s, separatorList: string; sl: TStringList);
var p, i, last: integer;
begin
  sl.Clear;
  last := 1;
  for i := 1 to length(s) do begin
    p := Pos(s[i], separatorList);
    if p > 0 then begin
      if i <> last then
        sl.add(copy(s,last,i-last));
      last := i + 1;
    end;
  end;
  if last <= length(s) then
    sl.add(copy(s, last, length(s) - last + 1));
end;


// Converts a StringList representation to a TDMatrix
function StringListToDMatrix(SL: TStrings; ItemSeparators: string): TDMatrix;
var
  r,c,lines,rxc : integer;
  s : string;
  SLRow : TStringList;
begin
  result := Mzeros(0,0);
  lines := SL.Count;
  if lines < 1 then
    raise Exception.Create('StringListToDMatrix error: stringlist with zero lines');

  s := SL.Strings[0];
  slRow := TStringList.Create;

  try
    ParseString(s, ItemSeparators, slRow);
    rxc := slRow.Count;
    if rxc < 1 then
      raise Exception.Create('StringListToDMatrix error: first line with zero columns');

    result := Mzeros(lines, rxc);

    for r := 0 to SL.Count - 1 do begin
      s := SL.Strings[r];
      ParseString(s, ItemSeparators, slRow);
      if slRow.Count <> rxc then
        raise Exception.Create(format('StringListToDMatrix error: line %d with %d columns instead of %d',[r, slRow.Count, rxc]));

      for c := 0 to slRow.Count - 1 do begin
        result.setv(r, c, StrToFloat(slRow[c]));
      end;
    end;
  finally
    slRow.Free;
  end;

end;

// Returns a string with all the elements from matrix M, line by line
function DMatrixToString(M: TDMatrix; FormatString: string; ItemSeparator: string; LineSeparator: string): string;
var r,c,rows, cols: Longword;
    sr: string;
begin
  rows := m.NumRows;
  cols := m.NumCols;
  result := '';

  for r:=0 to rows-1 do begin
    sr:='';
    for c:=0 to cols-1 do begin
      if sr <> '' then sr := sr + ItemSeparator;
      sr := sr + format(FormatString, [m.getv(r,c)]);
    end;
    result := result + sr + LineSeparator;
  end;
end;


// Adds to TS all the elements from matrix A, line by line
procedure MAddToStringList(M: TDMatrix; TS: TStrings; FormatString: string; ItemSeparator: string);
var
    r,c,rows, cols: Longword;
    sr: string;
begin
  rows := m.NumRows;
  cols := m.NumCols;

  TS.BeginUpdate;
  try
    for r:=0 to rows-1 do begin
      sr:='';
      for c:=0 to cols-1 do begin
        if sr <> '' then sr := sr + ItemSeparator;
        sr := sr + format(FormatString, [m.getv(r,c)]);
      end;
      TS.add(sr);
    end;
  finally
    TS.EndUpdate;
  end;
end;

// Adds to TS the specified label
// Then adds all the elements from matrix A, line by line
procedure MAddToStringListWL(caption: string; M: TDMatrix; TS: TStrings; FormatString: string; ItemSeparator: string);
begin
  TS.Add(caption);
  MAddToStringList(M, TS, FormatString, ItemSeparator);
end;

// Converts a String representation to a TDMatrix
// a11 a12 a13; a12 a22 a23;
function StringToDMatrix(str: string): TDMatrix;
var SL: TStringList;
begin
  if str = '' then
    raise Exception.Create('StringToDMatrix error: empty string');

  SL := TStringList.Create;
  ParseString(str, ';', SL);
  try
    result := StringListToDMatrix(SL);

  finally
    SL.Free;
  end;

end;


function ArrayToDMatrixRow(d: array of double): TDMatrix;
begin
  result := MFromArray(1, length(d), d);
end;

function ArrayToDMatrixCol(d: array of double): TDMatrix;
begin
  result := MFromArray(length(d), 1, d);
end;


end.



