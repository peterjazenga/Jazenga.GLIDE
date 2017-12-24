{**********************************************************************                   
 Package pl_Astronomy.pkg
 This unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit StStrS;

interface

uses
  LCLIntf, LCLType, LMessages,
  Classes,
  SysUtils,
  StConst,
  StBase;

{-------- Numeric conversion -----------}

function HexBS(B: byte): ShortString;
{-Return the hex string for a byte.}

function HexWS(W: word): ShortString;
{-Return the hex string for a word.}

function HexLS(L: longint): ShortString;
{-Return the hex string for a long integer.}

function HexPtrS(P: Pointer): ShortString;
{-Return the hex string for a pointer.}

function BinaryBS(B: byte): ShortString;
{-Return a binary string for a byte.}

function BinaryWS(W: word): ShortString;
{-Return the binary string for a word.}

function BinaryLS(L: longint): ShortString;
{-Return the binary string for a long integer.}

function OctalBS(B: byte): ShortString;
{-Return an octal string for a byte.}

function OctalWS(W: word): ShortString;
{-Return an octal string for a word.}

function OctalLS(L: longint): ShortString;
{-Return an octal string for a long integer.}


{-Convert a string to a real.}
function Str2RealS(const S: ShortString; var R: real): boolean;


function Str2ExtS(const S: ShortString; var R: extended): boolean;
{-Convert a string to an extended.}

function Long2StrS(L: longint): ShortString;
{-Convert an integer type to a string.}

function Real2StrS(R: double; Width: byte; Places: shortint): ShortString;
{-Convert a real to a string.}

function Ext2StrS(R: extended; Width: byte; Places: shortint): ShortString;
{-Convert an extended to a string.}

function ValPrepS(const S: ShortString): ShortString;
{-Prepares a string for calling Val.}


{-------- General purpose string manipulation --------}

function CharStrS(C: AnsiChar; Len: cardinal): ShortString;
{-Return a string filled with the specified character.}

function PadChS(const S: ShortString; C: AnsiChar; Len: cardinal): ShortString;
{-Pad a string on the right with a specified character.}

function PadS(const S: ShortString; Len: cardinal): ShortString;
{-Pad a string on the right with spaces.}

function LeftPadChS(const S: ShortString; C: AnsiChar; Len: cardinal): ShortString;
{-Pad a string on the left with a specified character.}

function LeftPadS(const S: ShortString; Len: cardinal): ShortString;
{-Pad a string on the left with spaces.}

function TrimLeadS(const S: ShortString): ShortString;
{-Return a string with leading white space removed.}

function TrimTrailS(const S: ShortString): ShortString;
{-Return a string with trailing white space removed.}

function TrimS(const S: ShortString): ShortString;
{-Return a string with leading and trailing white space removed.}

function TrimSpacesS(const S: ShortString): ShortString;
{-Return a string with leading and trailing spaces removed.}

function CenterChS(const S: ShortString; C: AnsiChar; Len: cardinal): ShortString;
{-Pad a string on the left and right with a specified character.}

function CenterS(const S: ShortString; Len: cardinal): ShortString;
{-Pad a string on the left and right with spaces.}


function ScrambleS(const S, Key: ShortString): ShortString;
{-Encrypt / Decrypt string with enhanced XOR encryption.}

function SubstituteS(const S, FromStr, ToStr: ShortString): ShortString;
{-Map the characters found in FromStr to the corresponding ones in ToStr.}

function FilterS(const S, aFilters: ShortString): ShortString;
  {-Remove characters from a string. The characters to remove are specified in
    ChSet.}

{--------------- Word / Char manipulation -------------------------}

function CharExistsS(const S: string; C: char): boolean; overload;
{-Determines whether a given character exists in a string. }


function WordCountS(const S, WordDelims: ShortString): cardinal;
{-Given an array of word delimiters, return the number of words in a string.}

function WordPositionS(N: cardinal; const S, WordDelims: ShortString; var Pos: cardinal): boolean;
  {-Given an array of word delimiters, set Pos to the start position of the
    N'th word in a string.  Result indicates success/failure.}

function ExtractWordS(N: cardinal; const S, WordDelims: ShortString): ShortString;
{-Given an array of word delimiters, return the N'th word in a string.}

function AsciiCountS(const S, WordDelims: ShortString; Quote: AnsiChar): cardinal;
{-Return the number of words in a string.}

function AsciiPositionS(N: cardinal; const S, WordDelims: ShortString; Quote: AnsiChar; var Pos: cardinal): boolean;
{-Return the position of the N'th word in a string.}

function ExtractAsciiS(N: cardinal; const S, WordDelims: ShortString; Quote: AnsiChar): ShortString;
  {-Given an array of word delimiters, return the N'th word in a string. Any
    text within Quote characters is counted as one word.}

procedure WordWrapS(const InSt: ShortString; var OutSt, Overlap: ShortString; Margin: cardinal; PadToMargin: boolean);
{-Wrap a text string at a specified margin.}


{--------------- DOS pathname parsing -----------------}

function DefaultExtensionS(const Name, Ext: ShortString): ShortString;
{-Return a file name with a default extension attached.}

function ForceExtensionS(const Name, Ext: ShortString): ShortString;
{-Force the specified extension onto the file name.}

function JustFilenameS(const PathName: ShortString): ShortString;
{-Return just the filename and extension of a pathname.}

function JustNameS(const PathName: ShortString): ShortString;
{-Return just the filename (no extension, path, or drive) of a pathname.}

function JustExtensionS(const Name: ShortString): ShortString;
{-Return just the extension of a pathname.}

function JustPathnameS(const PathName: ShortString): ShortString;
{-Return just the drive and directory portion of a pathname.}

function AddBackSlashS(const DirName: ShortString): ShortString;
{-Add a default backslash to a directory name.}

function CleanPathNameS(const PathName: ShortString): ShortString;
{-Return a pathname cleaned up as DOS does it.}

function HasExtensionS(const Name: ShortString; var DotPos: cardinal): boolean;
  {-Determine if a pathname contains an extension and, if so, return the
    position of the dot in front of the extension.}

{------------------ Formatting routines --------------------}

function CommaizeS(L: longint): ShortString;
{-Convert a long integer to a string with commas.}

function CommaizeChS(L: longint; Ch: AnsiChar): ShortString;
{-Convert a long integer to a string with Ch in comma positions.}

function FloatFormS(const Mask: ShortString; R: TstFloat; const LtCurr, RtCurr: ShortString;
  Sep, DecPt: AnsiChar): ShortString;
{-Return a formatted string with digits from R merged into mask.}

function LongIntFormS(const Mask: ShortString; L: longint; const LtCurr, RtCurr: ShortString;
  Sep: AnsiChar): ShortString;
{-Return a formatted string with digits from L merged into mask.}

function StrChPosS(const P: string; C: char; var Pos: cardinal): boolean; overload;

{-Return the position of a specified character within a string.}

function StrStPosS(const P, S: ShortString; var Pos: cardinal): boolean;
{-Return the position of a specified substring within a string.}

function StrStCopyS(const S: ShortString; Pos, Count: cardinal): ShortString;
{-Copy characters at a specified position in a string.}

function StrChInsertS(const S: ShortString; C: AnsiChar; Pos: cardinal): ShortString;
{-Insert a character into a string at a specified position.}

function StrStInsertS(const S1, S2: ShortString; Pos: cardinal): ShortString;
{-Insert a string into another string at a specified position.}

function StrChDeleteS(const S: ShortString; Pos: cardinal): ShortString;
{-Delete the character at a specified position in a string.}

function StrStDeleteS(const S: ShortString; Pos, Count: cardinal): ShortString;
{-Delete characters at a specified position in a string.}


{--------------------------  New Functions -----------------------------------}

function ContainsOnlyS(const S, Chars: ShortString; var BadPos: cardinal): boolean;

function ContainsOtherThanS(const S, Chars: ShortString; var BadPos: cardinal): boolean;

function CopyLeftS(const S: ShortString; Len: cardinal): ShortString;
{-Return the left Len characters of a string}

function CopyMidS(const S: ShortString; First, Len: cardinal): ShortString;
{-Return the mid part of a string}

function CopyRightS(const S: ShortString; First: cardinal): ShortString;
{-Return the right Len characters of a string}

function CopyRightAbsS(const S: ShortString; NumChars: cardinal): ShortString;
{-Return NumChar characters starting from end}

function CopyFromNthWordS(const S, WordDelims: ShortString; const AWord: ShortString; N: cardinal;     {!!.02}
  var SubString: ShortString): boolean;

function DeleteFromNthWordS(const S, WordDelims: ShortString; AWord: ShortString;
  N: cardinal; var SubString: ShortString): boolean;

function CopyFromToWordS(const S, WordDelims, Word1, Word2: ShortString; N1, N2: cardinal;
  var SubString: ShortString): boolean;

function DeleteFromToWordS(const S, WordDelims, Word1, Word2: ShortString; N1, N2: cardinal;
  var SubString: ShortString): boolean;

function CopyWithinS(const S, Delimiter: ShortString; Strip: boolean): ShortString;

function DeleteWithinS(const S, Delimiter: ShortString): ShortString;

function ExtractTokensS(const S, Delims: ShortString; QuoteChar: AnsiChar;
  AllowNulls: boolean; Tokens: TStrings): cardinal;


function IsChNumericS(C: AnsiChar; const Numbers: ShortString): boolean;
{-Returns true if Ch in numeric set}


function IsStrNumericS(const S, Numbers: ShortString): boolean;
{-Returns true if all characters in string are in numeric set}


function LastWordS(const S, WordDelims, AWord: ShortString; var Position: cardinal): boolean;
{-returns the position in a string of the last instance of a given word}

function LastWordAbsS(const S, WordDelims: ShortString; var Position: cardinal): boolean;
{-returns the position in a string of the last word}

function LastStringS(const S, AString: ShortString; var Position: cardinal): boolean;
{-returns the position in a string of the last instance of a given string}

function LeftTrimCharsS(const S, Chars: ShortString): ShortString;
{-strips given characters from the beginning of a string}

function KeepCharsS(const S, Chars: ShortString): ShortString;
{-returns a string containing only those characters in a given set}

function RepeatStringS(const RepeatString: ShortString; var Repetitions: cardinal;
  MaxLen: cardinal): ShortString;
{-creates a string of up to Repetition instances of a string}

function ReplaceStringS(const S, OldString, NewString: ShortString; N: cardinal;
  var Replacements: cardinal): ShortString;
{-replaces a substring with up to Replacements instances of a string}

function ReplaceStringAllS(const S, OldString, NewString: ShortString; var Replacements: cardinal): ShortString;
{-replaces all instances of a substring with one or more instances of a string}

function ReplaceWordS(const S, WordDelims, OldWord, NewWord: ShortString; N: cardinal;
  var Replacements: cardinal): ShortString;
{-replaces a given word with one or more instances of a string}

function ReplaceWordAllS(const S, WordDelims, OldWord, NewWord: ShortString; var Replacements: cardinal): ShortString;
{-replaces all instances of a word with one or more instances of a string}

function RightTrimCharsS(const S, Chars: ShortString): ShortString;
{-removes those characters at the end of a string contained in a set of characters}

function StrWithinS(const S, SearchStr: ShortString; Start: cardinal; var Position: cardinal): boolean;
{-finds the position of a substring within a string starting at a given point}

function TrimCharsS(const S, Chars: ShortString): ShortString;
{-removes trailing and leading characters defined by a string from a string}

function WordPosS(const S, WordDelims, AWord: ShortString; N: cardinal; var Position: cardinal): boolean;
{-returns the Nth instance of a word within a string}


implementation


{-------- Numeric conversion -----------}

function HexBS(B: byte): ShortString;
  {-Return the hex string for a byte.}
begin
  Result[0] := #2;
  Result[1] := StHexDigits[B shr 4];
  Result[2] := StHexDigits[B and $F];
end;

function HexWS(W: word): ShortString;
  {-Return the hex string for a word.}
begin
  Result[0] := #4;
  Result[1] := StHexDigits[hi(W) shr 4];
  Result[2] := StHexDigits[hi(W) and $F];
  Result[3] := StHexDigits[lo(W) shr 4];
  Result[4] := StHexDigits[lo(W) and $F];
end;

function HexLS(L: longint): ShortString;
  {-Return the hex string for a long integer.}
begin
  Result := HexWS(HiWord(DWORD(L))) + HexWS(LoWord(DWORD(L)));         {!!.02}
end;

function HexPtrS(P: Pointer): ShortString;
  {-Return the hex string for a pointer.}
begin
  Result := HexLS(longint(P));                                         {!!.02}
end;

function BinaryBS(B: byte): ShortString;
  {-Return a binary string for a byte.}
var
  I, N: cardinal;
begin
  N := 1;
  Result[0] := #8;
  for I := 7 downto 0 do
  begin
    Result[N] := StHexDigits[Ord(B and (1 shl I) <> 0)]; {0 or 1}
    Inc(N);
  end;
end;

function BinaryWS(W: word): ShortString;
  {-Return the binary string for a word.}
var
  I, N: cardinal;
begin
  N := 1;
  Result[0] := #16;
  for I := 15 downto 0 do
  begin
    Result[N] := StHexDigits[Ord(W and (1 shl I) <> 0)]; {0 or 1}
    Inc(N);
  end;
end;

function BinaryLS(L: longint): ShortString;
  {-Return the binary string for a long integer.}
var
  I: longint;
  N: byte;
begin
  N := 1;
  Result[0] := #32;
  for I := 31 downto 0 do
  begin
    Result[N] := StHexDigits[Ord(L and longint(1 shl I) <> 0)]; {0 or 1}
    Inc(N);
  end;
end;

function OctalBS(B: byte): ShortString;
  {-Return an octal string for a byte.}
var
  I: cardinal;
begin
  Result[0] := #3;
  for I := 0 to 2 do
  begin
    Result[3 - I] := StHexDigits[B and 7];
    B := B shr 3;
  end;
end;

function OctalWS(W: word): ShortString;
  {-Return an octal string for a word.}
var
  I: cardinal;
begin
  Result[0] := #6;
  for I := 0 to 5 do
  begin
    Result[6 - I] := StHexDigits[W and 7];
    W := W shr 3;
  end;
end;

function OctalLS(L: longint): ShortString;
  {-Return an octal string for a long integer.}
var
  I: cardinal;
begin
  Result[0] := #12;
  for I := 0 to 11 do
  begin
    Result[12 - I] := StHexDigits[L and 7];
    L := L shr 3;
  end;
end;


{-Convert a string to a real.}
function Str2RealS(const S: ShortString; var R: real): boolean;

  {-Convert a string to a real.}
var
  Code: integer;
  St: ShortString;
  SLen: byte absolute St;
begin
  St := S;
  {trim trailing blanks}
  while St[SLen] = ' ' do
    Dec(SLen);
  Val(ValPrepS(St), R, Code);
  if Code <> 0 then
  begin
    R := Code;
    Result := False;
  end
  else
    Result := True;
end;

function Str2ExtS(const S: ShortString; var R: extended): boolean;
  {-Convert a string to an extended.}
var
  Code: integer;
  P: ShortString;
  PLen: byte absolute P;
begin
  P := S;
  {trim trailing blanks}
  while P[PLen] = ' ' do
    Dec(PLen);
  Val(ValPrepS(P), R, Code);
  if Code <> 0 then
  begin
    R := Code;
    Result := False;
  end
  else
    Result := True;
end;

function Long2StrS(L: longint): ShortString;
  {-Convert an integer type to a string.}
begin
  Str(L, Result);
end;

function Real2StrS(R: double; Width: byte; Places: shortint): ShortString;
  {-Convert a real to a string.}
begin
  Str(R: Width: Places, Result);
end;

function Ext2StrS(R: extended; Width: byte; Places: shortint): ShortString;
  {-Convert an extended to a string.}
begin
  Str(R: Width: Places, Result);
end;

function ValPrepS(const S: ShortString): ShortString;
  {-Prepares a string for calling Val.}
var
  P: cardinal;
begin
  Result := TrimSpacesS(S);
  if Result <> '' then
  begin
    if StrChPosS(Result, DecimalSeparator, P) then
    begin
      Result[P] := '.';
      if P = byte(Result[0]) then
        Result[0] := AnsiChar(Pred(P));
    end;
  end
  else
  begin
    Result := '0';
  end;
end;

{-------- General purpose string manipulation --------}

function CharStrS(C: AnsiChar; Len: cardinal): ShortString;
  {-Return a string filled with the specified character.}
begin
  if Len = 0 then
    Result[0] := #0
  else
  begin
    Result[0] := AnsiChar(Len);
    FillChar(Result[1], Len, C);
  end;
end;

function PadChS(const S: ShortString; C: AnsiChar; Len: cardinal): ShortString;
  {-Pad a string on the right with a specified character.}
var
  SLen: byte absolute S;
begin
  if Length(S) >= Len then
    Result := S
  else
  begin
    if Len > 255 then
      Len := 255;
    Result[0] := AnsiChar(Len);
    Move(S[1], Result[1], SLen);
    if SLen < 255 then
      FillChar(Result[Succ(SLen)], Len - SLen, C);
  end;
end;

function PadS(const S: ShortString; Len: cardinal): ShortString;
  {-Pad a string on the right with spaces.}
begin
  Result := PadChS(S, ' ', Len);
end;

function LeftPadChS(const S: ShortString; C: AnsiChar; Len: cardinal): ShortString;
  {-Pad a string on the left with a specified character.}
begin
  if Length(S) >= Len then
    Result := S
  else if Length(S) < 255 then
  begin
    if Len > 255 then
      Len := 255;
    Result[0] := AnsiChar(Len);
    Move(S[1], Result[Succ(word(Len)) - Length(S)], Length(S));
    FillChar(Result[1], Len - Length(S), C);
  end;
end;

function LeftPadS(const S: ShortString; Len: cardinal): ShortString;
  {-Pad a string on the left with spaces.}
begin
  Result := LeftPadChS(S, ' ', Len);
end;

function TrimLeadS(const S: ShortString): ShortString;
  {-Return a string with leading white space removed}
var
  I: cardinal;
begin
  {!!.03 - added }
  if S = '' then
  begin
    Result := '';
    Exit;
  end;
  {!!.03 - added end }
  I := 1;
  while (I <= Length(S)) and (S[I] <= ' ') do
    Inc(I);
  Move(S[I], Result[1], Length(S) - I + 1);
  Result[0] := AnsiChar(Length(S) - I + 1);
end;

function TrimTrailS(const S: ShortString): ShortString;
  {-Return a string with trailing white space removed.}
begin
  Result := S;
  while (Length(Result) > 0) and (Result[Length(Result)] <= ' ') do
    Dec(Result[0]);
end;

function TrimS(const S: ShortString): ShortString;
  {-Return a string with leading and trailing white space removed.}
var
  I: cardinal;
  SLen: byte absolute Result;
begin
  Result := S;
  while (SLen > 0) and (Result[SLen] <= ' ') do
    Dec(SLen);

  I := 1;
  while (I <= SLen) and (Result[I] <= ' ') do
    Inc(I);
  Dec(I);
  if I > 0 then
    Delete(Result, 1, I);
end;

function TrimSpacesS(const S: ShortString): ShortString;
  {-Return a string with leading and trailing spaces removed.}
var
  I: word;
begin
  Result := S;
  while (Length(Result) > 0) and (Result[Length(Result)] = ' ') do
    Dec(Result[0]);
  I := 1;
  while (I <= Length(Result)) and (S[I] = ' ') do
    Inc(I);
  Dec(I);
  if I > 0 then
    Delete(Result, 1, I);
end;

function CenterChS(const S: ShortString; C: AnsiChar; Len: cardinal): ShortString;
  {-Pad a string on the left and right with a specified character.}
begin
  if Length(S) >= Len then
    Result := S
  else if Length(S) < 255 then
  begin
    if Len > 255 then
      Len := 255;
    Result[0] := AnsiChar(Len);
    FillChar(Result[1], Len, C);
    Move(S[1], Result[Succ((Len - Length(S)) shr 1)], Length(S));
  end;
end;

function CenterS(const S: ShortString; Len: cardinal): ShortString;
  {-Pad a string on the left and right with spaces.}
begin
  Result := CenterChS(S, ' ', Len);
end;

function ScrambleS(const S, Key: ShortString): ShortString;
  {-Encrypt / Decrypt string with enhanced XOR encryption.}
var
  J, LKey, LStr: byte;
  I: cardinal;
begin
  Result := S;
  LKey := Length(Key);
  LStr := Length(S);
  if LKey = 0 then
    Exit;
  if LStr = 0 then
    Exit;
  I := 1;
  J := LKey;
  while I <= LStr do
  begin
    if J = 0 then
      J := LKey;
    if (S[I] <> Key[J]) then
      Result[I] := AnsiChar(byte(S[I]) xor byte(Key[J]));
    Inc(I);
    Dec(J);
  end;
end;

function SubstituteS(const S, FromStr, ToStr: ShortString): ShortString;
  {-Map the characters found in FromStr to the corresponding ones in ToStr.}
var
  P: cardinal;
  I: byte;
begin
  Result := S;
  if Length(FromStr) = Length(ToStr) then
    for I := 1 to Length(Result) do
    begin
      if StrChPosS(FromStr, S[I], P) then
        Result[I] := ToStr[P];
    end;
end;

function FilterS(const S, aFilters: ShortString): ShortString;
  {-Remove characters from a string. The characters to remove are specified in
    ChSet.}
var
  I: cardinal;
  Len: cardinal;
begin
  Len := 0;
  for I := 1 to Length(S) do
    if not CharExistsS(aFilters, S[I]) then
    begin
      Inc(Len);
      Result[Len] := S[I];
    end;
  Result[0] := AnsiChar(Len);
end;

{--------------- Word / Char manipulation -------------------------}

function CharExistsS(const S: string; C: char): boolean; overload;
var
  I: integer;
begin
  Result := False;
  for I := 1 to Length(S) do
  begin
    if S[I] = C then
    begin
      Result := True;
      Break;
    end;
  end;
end;


function WordCountS(const S, WordDelims: ShortString): cardinal;
  {-Given an array of word delimiters, return the number of words in a string.}
var
  I: integer;
  SLen: byte;
begin
  Result := 0;
  I := 1;
  SLen := Length(S);

  while I <= SLen do
  begin
    {skip over delimiters}
    while (I <= SLen) and CharExistsS(WordDelims, S[I]) do
      Inc(I);

    {if we're not beyond end of S, we're at the start of a word}
    if I <= SLen then
      Inc(Result);

    {find the end of the current word}
    while (I <= SLen) and not CharExistsS(WordDelims, S[I]) do
      Inc(I);
  end;
end;

function WordPositionS(N: cardinal; const S, WordDelims: ShortString; var Pos: cardinal): boolean;
  {-Given an array of word delimiters, set Pos to the start position of the
    N'th word in a string.  Result indicates success/failure.}
var
  I: cardinal;
  Count: byte;
  SLen: byte absolute S;
begin
  Count := 0;
  I := 1;
  Result := False;

  while (I <= SLen) and (Count <> N) do
  begin
    {skip over delimiters}
    while (I <= SLen) and CharExistsS(WordDelims, S[I]) do
      Inc(I);

    {if we're not beyond end of S, we're at the start of a word}
    if I <= SLen then
      Inc(Count);

    {if not finished, find the end of the current word}
    if Count <> N then
      while (I <= SLen) and not CharExistsS(WordDelims, S[I]) do
        Inc(I)
    else
    begin
      Pos := I;
      Result := True;
    end;
  end;
end;

function ExtractWordS(N: cardinal; const S, WordDelims: ShortString): ShortString;
  {-Given an array of word delimiters, return the N'th word in a string.}
var
  I: cardinal;
  Len: byte;
  SLen: byte absolute S;
begin
  Len := 0;
  if WordPositionS(N, S, WordDelims, I) then
    {find the end of the current word}
    while (I <= SLen) and not CharExistsS(WordDelims, S[I]) do
    begin
      {add the I'th character to result}
      Inc(Len);
      Result[Len] := S[I];
      Inc(I);
    end;
  Result[0] := AnsiChar(Len);
end;

function AsciiCountS(const S, WordDelims: ShortString; Quote: AnsiChar): cardinal;
  {-Return the number of words in a string.}
var
  I: cardinal;
  InQuote: boolean;
  SLen: byte absolute S;
begin
  Result := 0;
  I := 1;
  InQuote := False;
  while I <= SLen do
  begin
    {skip over delimiters}
    while (I <= SLen) and (S[i] <> Quote) and CharExistsS(WordDelims, S[I]) do
      Inc(I);
    {if we're not beyond end of S, we're at the start of a word}
    if I <= SLen then
      Inc(Result);
    {find the end of the current word}
    while (I <= SLen) and (InQuote or not CharExistsS(WordDelims, S[I])) do
    begin
      if S[I] = Quote then
        InQuote := not InQuote;
      Inc(I);
    end;
  end;
end;

function AsciiPositionS(N: cardinal; const S, WordDelims: ShortString; Quote: AnsiChar; var Pos: cardinal): boolean;
  {-Return the position of the N'th word in a string.}
var
  I: cardinal;
  Count: byte;
  InQuote: boolean;
  SLen: byte absolute S;
begin
  Count := 0;
  InQuote := False;
  Result := False;
  I := 1;
  while (I <= SLen) and (Count <> N) do
  begin
    {skip over delimiters}
    while (I <= SLen) and (S[I] <> Quote) and CharExistsS(WordDelims, S[I]) do
      Inc(I);
    {if we're not beyond end of S, we're at the start of a word}
    if I <= SLen then
      Inc(Count);
    {if not finished, find the end of the current word}
    if Count <> N then
      while (I <= SLen) and (InQuote or not CharExistsS(WordDelims, S[I])) do
      begin
        if S[I] = Quote then
          InQuote := not InQuote;
        Inc(I);
      end
    else
    begin
      Pos := I;
      Result := True;
    end;
  end;
end;

function ExtractAsciiS(N: cardinal; const S, WordDelims: ShortString; Quote: AnsiChar): ShortString;
  {-Given an array of word delimiters, return the N'th word in a string. Any
    text within Quote characters is counted as one word.}
var
  I: cardinal;
  Len: byte;
  SLen: byte absolute S;
  InQuote: boolean;
begin
  Len := 0;
  InQuote := False;
  if AsciiPositionS(N, S, WordDelims, Quote, I) then
    {find the end of the current word}
    while (I <= SLen) and ((InQuote) or not CharExistsS(WordDelims, S[I])) do
    begin
      {add the I'th character to result}
      Inc(Len);
      if S[I] = Quote then
        InQuote := not (InQuote);
      Result[Len] := S[I];
      Inc(I);
    end;
  Result[0] := AnsiChar(Len);
end;

procedure WordWrapS(const InSt: ShortString; var OutSt, Overlap: ShortString; Margin: cardinal; PadToMargin: boolean);
{-Wrap a text string at a specified margin.}
var
  EOS, BOS: cardinal;
  InStLen: byte;
  OutStLen: byte absolute OutSt;
  OvrLen: byte absolute Overlap;
begin
  InStLen := Length(InSt);

  {!!.02 - Added }
  { handle empty string on input }
  if InStLen = 0 then
  begin
    OutSt := '';
    Overlap := '';
    Exit;
  end;
  {!!.02 - End Added }

  {find the end of the output string}
  if InStLen > Margin then
  begin
    {find the end of the word at the margin, if any}
    EOS := Margin;
    while (EOS <= InStLen) and (InSt[EOS] <> ' ') do
      Inc(EOS);
    if EOS > InStLen then
      EOS := InStLen;

    {trim trailing blanks}
    while (InSt[EOS] = ' ') and (EOS > 0) do
      Dec(EOS);

    if EOS > Margin then
    begin
      {look for the space before the current word}
      while (EOS > 0) and (InSt[EOS] <> ' ') do
        Dec(EOS);

      {if EOS = 0 then we can't wrap it}
      if EOS = 0 then
        EOS := Margin
      else
        {trim trailing blanks}
        while (InSt[EOS] = ' ') and (EOS > 0) do
          Dec(EOS);
    end;
  end
  else
    EOS := InStLen;

  {copy the unwrapped portion of the line}
  OutStLen := EOS;
  Move(InSt[1], OutSt[1], OutStLen);

  {find the start of the next word in the line}
  BOS := EOS + 1;
  while (BOS <= InStLen) and (InSt[BOS] = ' ') do
    Inc(BOS);

  if BOS > InStLen then
    OvrLen := 0
  else
  begin
    {copy from the start of the next word to the end of the line}
    OvrLen := Succ(InStLen - BOS);
    Move(InSt[BOS], Overlap[1], OvrLen);
  end;

  {pad the end of the output string if requested}
  if PadToMargin and (OutStLen < Margin) then
  begin
    FillChar(OutSt[OutStLen + 1], Margin - OutStLen, ' ');
    OutStLen := Margin;
  end;
end;

{--------------- DOS pathname parsing -----------------}

function DefaultExtensionS(const Name, Ext: ShortString): ShortString;
  {-Return a file name with a default extension attached.}
var
  DotPos: cardinal;
begin
  if HasExtensionS(Name, DotPos) then
    Result := Name
  else if Name = '' then
    Result := ''
  else
    Result := Name + '.' + Ext;
end;

function ForceExtensionS(const Name, Ext: ShortString): ShortString;
  {-Force the specified extension onto the file name.}
var
  DotPos: cardinal;
begin
  if HasExtensionS(Name, DotPos) then
    Result := Copy(Name, 1, DotPos) + Ext
  else if Name = '' then
    Result := ''
  else
    Result := Name + '.' + Ext;
end;

function JustFilenameS(const PathName: ShortString): ShortString;
  {-Return just the filename and extension of a pathname.}
var
  I: longint;
begin
  Result := '';
  if PathName = '' then
    Exit;
  I := Succ(Length(PathName));
  repeat
    Dec(I);
  until (I = 0) or (PathName[I] in DosDelimSet);                         {!!.01}
  Result := Copy(PathName, Succ(I), StMaxFileLen);
end;

function JustNameS(const PathName: ShortString): ShortString;
  {-Return just the filename (no extension, path, or drive) of a pathname.}
var
  DotPos: cardinal;
begin
  Result := JustFileNameS(PathName);
  if HasExtensionS(Result, DotPos) then
    Result := Copy(Result, 1, DotPos - 1);
end;

function JustExtensionS(const Name: ShortString): ShortString;
  {-Return just the extension of a pathname.}
var
  DotPos: cardinal;
begin
  if HasExtensionS(Name, DotPos) then
    Result := Copy(Name, Succ(DotPos), StMaxFileLen)
  else
    Result := '';
end;

function JustPathnameS(const PathName: ShortString): ShortString;
  {-Return just the drive and directory portion of a pathname.}
var
  I: longint;
begin
  I := Succ(Length(PathName));
  repeat
    Dec(I);
  until (I = 0) or (PathName[I] in DosDelimSet);                         {!!.01}

  if I = 0 then
    {Had no drive or directory name}
    Result[0] := #0
  else if I = 1 then
    {Either the root directory of default drive or invalid pathname}
    Result := PathName[1]
  else if (PathName[I] = '\') then
  begin
    if PathName[Pred(I)] = ':' then
      {Root directory of a drive, leave trailing backslash}
      Result := Copy(PathName, 1, I)
    else
      {Subdirectory, remove the trailing backslash}
      Result := Copy(PathName, 1, Pred(I));
  end
  else
    {Either the default directory of a drive or invalid pathname}
    Result := Copy(PathName, 1, I);
end;

function AddBackSlashS(const DirName: ShortString): ShortString;
  {-Add a default backslash to a directory name}
begin
  Result := DirName;
  if (Length(Result) = 0) then
    Exit;
  if ((Length(Result) = 2) and (Result[2] = ':')) or ((Length(Result) > 2) and (Result[Length(Result)] <> '\')) then
    Result := Result + '\';
end;

function CleanFileNameS(const FileName: ShortString): ShortString;
  {-Return filename with at most 8 chars of name and 3 of extension}
var
  DotPos: cardinal;
  NameLen: cardinal;
begin
  if HasExtensionS(FileName, DotPos) then
  begin
    {Take the first 8 chars of name and first 3 chars of extension}
    NameLen := Pred(DotPos);
    if NameLen > 8 then
      NameLen := 8;
    Result := Copy(FileName, 1, NameLen) + Copy(FileName, DotPos, 4);
  end
  else
    {Take the first 8 chars of name}
    Result := Copy(FileName, 1, 8);
end;

function CleanPathNameS(const PathName: ShortString): ShortString;
  {-Return a pathname cleaned up as DOS does it.}
var
  I: longint;
  S: ShortString;
begin
  Result[0] := #0;
  S := PathName;

  I := Succ(Length(S));
  repeat
    Dec(I);
    if I > 2 then
      if (S[I] = '\') and (S[I - 1] = '\') then
        if (S[I - 2] <> ':') then
          Delete(S, I, 1);
  until I <= 0;

  I := Succ(Length(S));
  repeat
    {Get the next directory or drive portion of pathname}
    repeat
      Dec(I);
    until (I = 0) or (S[I] in DosDelimSet);                            {!!.02}

    {Clean it up and prepend it to output string}
    Result := CleanFileNameS(Copy(S, Succ(I), StMaxFileLen)) + Result;
    if I > 0 then
    begin
      Result := S[I] + Result;
      Delete(S, I, 255);
    end;
  until I <= 0;

end;

function HasExtensionS(const Name: ShortString; var DotPos: cardinal): boolean;
  {-Determine if a pathname contains an extension and, if so, return the
    position of the dot in front of the extension.}
var
  I: cardinal;
begin
  DotPos := 0;
  for I := Length(Name) downto 1 do
    if (Name[I] = '.') and (DotPos = 0) then
      DotPos := I;
  Result := (DotPos > 0)
    {and (Pos('\', Copy(Name, Succ(DotPos), MaxFileLen)) = 0);} and not CharExistsS(Copy(Name, Succ(DotPos), StMaxFileLen), '\');
end;

{------------------ Formatting routines --------------------}


function CommaizeChS(L: longint; Ch: AnsiChar): ShortString;
  {-Convert a long integer to a string with Ch in comma positions}
var
  NumCommas, I, Len: cardinal;
  Neg: boolean;
begin
  if L < 0 then
  begin
    Neg := True;
    L := Abs(L);
  end
  else
    Neg := False;
  Result := Long2StrS(L);
  Len := Length(Result);
  NumCommas := (Len - 1) div 3;
  for I := 1 to NumCommas do
    System.Insert(Ch, Result, Len - (I * 3) + 1);
  if Neg then
    System.Insert('-', Result, 1);
end;

function CommaizeS(L: longint): ShortString;
  {-Convert a long integer to a string with commas}
begin
  Result := CommaizeChS(L, ',');
end;

function FormPrimS(const Mask: ShortString; R: TstFloat; const LtCurr, RtCurr: ShortString;
  Sep, DecPt: AnsiChar; AssumeDP: boolean): ShortString;
  {-Returns a formatted string with digits from R merged into the Mask}
const
  Blank = 0;
  Asterisk = 1;
  Zero = 2;
const
{$IFOPT N+}
  MaxPlaces = 18;
{$ELSE}
  MaxPlaces = 11;
{$ENDIF}
  FormChars: string[8] = '#@*$-+,.';
  PlusArray: array[boolean] of AnsiChar = ('+', '-');
  MinusArray: array[boolean] of AnsiChar = (' ', '-');
  FillArray: array[Blank..Zero] of AnsiChar = (' ', '*', '0');
var
  S: ShortString;         {temporary string}
  Filler: integer;        {char for unused digit slots: ' ', '*', '0'}
  WontFit,                 {true if number won't fit in the mask}
  AddMinus,                {true if minus sign needs to be added}
  Dollar,                  {true if floating dollar sign is desired}
  Negative: boolean;      {true if B is negative}
  StartF,                  {starting point of the numeric field}
  EndF: word;             {end of numeric field}
  RtChars,                 {# of chars to add to right}
  LtChars,                 {# of chars to add to left}
  DotPos,                  {position of '.' in Mask}
  Digits,                  {total # of digits}
  Places,                  {# of digits after the '.'}
  Blanks,                  {# of blanks returned by Str}
  FirstDigit,              {pos. of first digit returned by Str}
  Extras,                  {# of extra digits needed for special cases}
  DigitPtr: byte;         {pointer into temporary string of digits}
  I: word;
label
  EndFound,
  RedoCase,
  Done;
begin
  {assume decimal point at end?}
  Result := Mask;
  if (not AssumeDP) and (not CharExistsS(Result, '.')) then
    AssumeDP := True;
  if AssumeDP and (Result <> '') and (Length(Result) < 255) then
  begin
    Inc(Result[0]);
    Result[Length(Result)] := '.';
  end;

  RtChars := 0;
  LtChars := 0;

  {check for empty string}
  if Length(Result) = 0 then
    goto Done;

  {initialize variables}
  Filler := Blank;
  DotPos := 0;
  Places := 0;
  Digits := 0;
  Dollar := False;
  AddMinus := True;
  StartF := 1;

  {store the sign of the real and make it positive}
  Negative := (R < 0);
  R := Abs(R);

  {strip and count c's}
  for I := Length(Result) downto 1 do
  begin
    if Result[I] = 'C' then
    begin
      Inc(RtChars);
      System.Delete(Result, I, 1);
    end
    else if Result[I] = 'c' then
    begin
      Inc(LtChars);
      System.Delete(Result, I, 1);
    end;
  end;

  {find the starting point for the field}
  while (StartF <= Length(Result)) and not CharExistsS(FormChars, Result[StartF]) do
    Inc(StartF);
  if StartF > Length(Result) then
    goto Done;

  {find the end point for the field}
  EndF := StartF;
  for I := StartF to Length(Result) do
  begin
    EndF := I;
    case Result[I] of
      '*': Filler := Asterisk;
      '@': Filler := Zero;
      '$': Dollar := True;
      '-',
      '+': AddMinus := False;
      '#': {ignore};
      ',',
      '.': DotPos := I;
      else
        goto EndFound;
    end;
    {Inc(EndF);}
  end;

  {if we get here at all, the last char was part of the field}
  Inc(EndF);

  EndFound:
    {if we jumped to here instead, it wasn't}
    Dec(EndF);

  {disallow Dollar if Filler is Zero}
  if Filler = Zero then
    Dollar := False;

  {we need an extra slot if Dollar is True}
  Extras := Ord(Dollar);

  {get total # of digits and # after the decimal point}
  for I := StartF to EndF do
    case Result[I] of
      '#', '@',
      '*', '$':
      begin
        Inc(Digits);
        if (I > DotPos) and (DotPos <> 0) then
          Inc(Places);
      end;
    end;

  {need one more 'digit' if Places > 0}
  Inc(Digits, Ord(Places > 0));

  {also need an extra blank if (1) Negative is true, and (2) Filler is Blank,
   and (3) AddMinus is true}
  if Negative and AddMinus and (Filler = Blank) then
    Inc(Extras)
  else
    AddMinus := False;

  {translate the real to a string}
  Str(R: Digits: Places, S);

  {add zeros that Str may have left out}
  if Places > MaxPlaces then
  begin
    FillChar(S[Length(S) + 1], Places - MaxPlaces, '0');
    Inc(S[0], Places - MaxPlaces);
    while (Length(S) > Digits) and (S[1] = ' ') do
      System.Delete(S, 1, 1);
  end;

  {count number of initial blanks}
  Blanks := 1;
  while S[Blanks] = ' ' do
    Inc(Blanks);
  FirstDigit := Blanks;
  Dec(Blanks);

  {the number won't fit if (a) S is longer than Digits or (b) the number of
   initial blanks is less than Extras}
  WontFit := (Length(S) > Digits) or (Blanks < Extras);

  {if it won't fit, fill decimal slots with '*'}
  if WontFit then
  begin
    for I := StartF to EndF do
      case Result[I] of
        '#', '@', '*', '$': Result[I] := '*';
        '+': Result[I] := PlusArray[Negative];
        '-': Result[I] := MinusArray[Negative];
      end;
    goto Done;
  end;

  {fill initial blanks in S with Filler; insert floating dollar sign}
  if Blanks > 0 then
  begin
    FillChar(S[1], Blanks, FillArray[Filler]);

    {put floating dollar sign in last blank slot if necessary}
    if Dollar then
    begin
      S[Blanks] := LtCurr[1];
      Dec(Blanks);
    end;

    {insert a minus sign if necessary}
    if AddMinus then
      S[Blanks] := '-';
  end;

  {put in the digits / signs}
  DigitPtr := Length(S);
  for I := EndF downto StartF do
  begin
    RedoCase:
      case Result[I] of
        '#', '@', '*', '$':
          if DigitPtr <> 0 then
          begin
            Result[I] := S[DigitPtr];
            Dec(DigitPtr);
            if (DigitPtr <> 0) and (S[DigitPtr] = '.') then                {!!.01}
              Dec(DigitPtr);
          end
          else
            Result[I] := FillArray[Filler];
        ',':
        begin
          Result[I] := Sep;
          if (I < DotPos) and (DigitPtr < FirstDigit) then
          begin
            Result[I] := '#';
            goto RedoCase;
          end;
        end;
        '.':
        begin
          Result[I] := DecPt;
          if (I < DotPos) and (DigitPtr < FirstDigit) then
          begin
            Result[I] := '#';
            goto RedoCase;
          end;
        end;
        '+': Result[I] := PlusArray[Negative];
        '-': Result[I] := MinusArray[Negative];
      end;
  end;

  Done:
    if AssumeDP then
      Dec(Result[0]);
  if RtChars > 0 then
  begin
    S := RtCurr;
    if byte(S[0]) > RtChars then
      S[0] := AnsiChar(RtChars)
    else
      S := LeftPadS(S, RtChars);
    Result := Result + S;
  end;
  if LtChars > 0 then
  begin
    S := LtCurr;
    if byte(S[0]) > LtChars then
      S[0] := AnsiChar(LtChars)
    else
      S := PadS(S, LtChars);
    Result := S + Result;
  end;
end;

function FloatFormS(const Mask: ShortString; R: TstFloat; const LtCurr, RtCurr: ShortString;
  Sep, DecPt: AnsiChar): ShortString;
  {-Return a formatted string with digits from R merged into mask.}
begin
  Result := FormPrimS(Mask, R, LtCurr, RtCurr, Sep, DecPt, False);
end;

function LongIntFormS(const Mask: ShortString; L: longint; const LtCurr, RtCurr: ShortString;
  Sep: AnsiChar): ShortString;
  {-Return a formatted string with digits from L merged into mask.}
begin
  Result := FormPrimS(Mask, L, LtCurr, RtCurr, Sep, '.', True);
end;

function StrChPosS(const P: string; C: char; var Pos: cardinal): boolean;
var
  I: integer;
  {-Return the position of a specified character within a string.}
begin
  Result := False;
  for I := 1 to Length(P) do
  begin
    if P[I] = C then
    begin
      Result := True;
      Pos := I;
      Break;
    end;
  end;
end;

function StrStPosS(const P, S: ShortString; var Pos: cardinal): boolean;
  {-Return the position of a specified substring within a string.}
begin
  Pos := System.Pos(S, P);
  Result := Pos <> 0;
end;

function StrStCopyS(const S: ShortString; Pos, Count: cardinal): ShortString;
  {-Copy characters at a specified position in a string.}
begin
  Result := System.Copy(S, Pos, Count);
end;

function StrChInsertS(const S: ShortString; C: AnsiChar; Pos: cardinal): ShortString;
  {-Insert a character into a string at a specified position.}
var
  Temp: string[2];
begin
  Temp[0] := #1;
  Temp[1] := C;
  Result := S;
  System.Insert(Temp, Result, Pos);
end;

function StrStInsertS(const S1, S2: ShortString; Pos: cardinal): ShortString;
  {-Insert a string into another string at a specified position.}
begin
  Result := S1;
  System.Insert(S2, Result, Pos);
end;

function StrChDeleteS(const S: ShortString; Pos: cardinal): ShortString;
  {-Delete the character at a specified position in a string.}
begin
  Result := S;
  System.Delete(Result, Pos, 1);
end;

function StrStDeleteS(const S: ShortString; Pos, Count: cardinal): ShortString;
  {-Delete characters at a specified position in a string.}
begin
  Result := S;
  System.Delete(Result, Pos, Count);
end;

{-----------------------------  NEW FUNCTIONS (3.00) -------------------------}

function CopyLeftS(const S: ShortString; Len: cardinal): ShortString;
  {-Return the left Len characters of a string}
begin
  if (Len < 1) or (S = '') then
    Result := ''
  else
    Result := Copy(S, 1, Len);
end;



function CopyMidS(const S: ShortString; First, Len: cardinal): ShortString;
  {-Return the mid part of a string}
begin
  if (First > Length(S)) or (Len < 1) or (S = '') then
    Result := ''
  else
    Result := Copy(S, First, Len);
end;



function CopyRightS(const S: ShortString; First: cardinal): ShortString;
  {-Return the right Len characters of a string}
begin
  if (First > Length(S)) or (First < 1) or (S = '') then
    Result := ''
  else
    Result := Copy(S, First, Length(S));
end;

function CopyRightAbsS(const S: ShortString; NumChars: cardinal): ShortString;
  {-Return NumChar characters starting from end}
begin
  if (Length(S) > NumChars) then
    Result := Copy(S, (Length(S) - NumChars) + 1, NumChars)
  else
    Result := S;
end;


function CopyFromNthWordS(const S, WordDelims: ShortString; const AWord: ShortString; N: cardinal;     {!!.02}
  var SubString: ShortString): boolean;
var
  P: cardinal;
begin
  if (WordPosS(S, WordDelims, AWord, N, P)) then
  begin
    SubString := Copy(S, P, Length(S));
    Result := True;
  end
  else
  begin
    SubString := '';
    Result := False;
  end;
end;



function DeleteFromNthWordS(const S, WordDelims: ShortString; AWord: ShortString;
  N: cardinal; var SubString: ShortString): boolean;
var
  P: cardinal;
begin
  if (WordPosS(S, WordDelims, AWord, N, P)) then
  begin
    Result := True;
    SubString := Copy(S, 1, P - 1);
  end
  else
  begin
    Result := False;
    SubString := '';
  end;
end;



function CopyFromToWordS(const S, WordDelims, Word1, Word2: ShortString; N1, N2: cardinal;
  var SubString: ShortString): boolean;
var
  P1, P2: cardinal;
begin
  if (WordPosS(S, WordDelims, Word1, N1, P1)) then
  begin
    if (WordPosS(S, WordDelims, Word2, N2, P2)) then
    begin
      Dec(P2);
      if (P2 > P1) then
      begin
        Result := True;
        SubString := Copy(S, P1, P2 - P1);
      end
      else
      begin
        Result := False;
        SubString := '';
      end;
    end
    else
    begin
      Result := False;
      SubString := '';
    end;
  end
  else
  begin
    Result := False;
    SubString := '';
  end;
end;



function DeleteFromToWordS(const S, WordDelims, Word1, Word2: ShortString; N1, N2: cardinal;
  var SubString: ShortString): boolean;
var
  P1, P2: cardinal;
begin
  SubString := S;
  if (WordPosS(S, WordDelims, Word1, N1, P1)) then
  begin
    if (WordPosS(S, WordDelims, Word2, N2, P2)) then
    begin
      Dec(P2);
      if (P2 > P1) then
      begin
        Result := True;
        System.Delete(SubString, P1, P2 - P1 + 1);
      end
      else
      begin
        Result := False;
        SubString := '';
      end;
    end
    else
    begin
      Result := False;
      SubString := '';
    end;
  end
  else
  begin
    Result := False;
    SubString := '';
  end;
end;



function CopyWithinS(const S, Delimiter: ShortString; Strip: boolean): ShortString;
var
  P1, P2: cardinal;
  TmpStr: ShortString;
begin
  if (S = '') or (Delimiter = '') or (Pos(Delimiter, S) = 0) then
    Result := ''
  else
  begin
    if (StrStPosS(S, Delimiter, P1)) then
    begin
      TmpStr := Copy(S, P1 + Length(Delimiter), Length(S));
      if StrStPosS(TmpStr, Delimiter, P2) then
      begin
        Result := Copy(TmpStr, 1, P2 - 1);
        if (not Strip) then
          Result := Delimiter + Result + Delimiter;
      end
      else
      begin
        Result := TmpStr;
        if (not Strip) then
          Result := Delimiter + Result;
      end;
    end;
  end;
end;



function DeleteWithinS(const S, Delimiter: ShortString): ShortString;
var
  P1, P2: cardinal;
  TmpStr: ShortString;
begin
  if (S = '') or (Delimiter = '') or (Pos(Delimiter, S) = 0) then
    Result := ''
  else
  begin
    if (StrStPosS(S, Delimiter, P1)) then
    begin
      TmpStr := Copy(S, P1 + Length(Delimiter), Length(S));
      if (Pos(Delimiter, TmpStr) = 0) then
        Result := Copy(S, 1, P1 - 1)
      else
      begin
        if (StrStPosS(TmpStr, Delimiter, P2)) then
        begin
          Result := S;
          P2 := P2 + (2 * Length(Delimiter));
          System.Delete(Result, P1, P2);
        end;
      end;
    end;
  end;
end;



function ReplaceWordS(const S, WordDelims, OldWord, NewWord: ShortString; N: cardinal;
  var Replacements: cardinal): ShortString;
var
  I, C, P1: cardinal;
begin
  if (S = '') or (WordDelims = '') or (OldWord = '') or (Pos(OldWord, S) = 0) then
  begin
    Result := S;
    Replacements := 0;
  end
  else
  begin
    if (WordPosS(S, WordDelims, OldWord, N, P1)) then
    begin
      Result := S;
      System.Delete(Result, P1, Length(OldWord));
      C := 0;
      for I := 1 to Replacements do
      begin
        if ((Length(NewWord) + Length(Result)) <= 255) then
        begin
          Inc(C);
          System.Insert(NewWord, Result, P1);
          Inc(P1, Length(NewWord) + 1);
        end
        else
        begin
          Replacements := C;
          Exit;
        end;
      end;
    end
    else
    begin
      Result := S;
      Replacements := 0;
    end;
  end;
end;


function ReplaceWordAllS(const S, WordDelims, OldWord, NewWord: ShortString; var Replacements: cardinal): ShortString;
var
  I, C, P1: cardinal;
begin
  if (S = '') or (WordDelims = '') or (OldWord = '') or (Pos(OldWord, S) = 0) then
  begin
    Result := S;
    Replacements := 0;
  end
  else
  begin
    Result := S;
    C := 0;
    while (WordPosS(Result, WordDelims, OldWord, 1, P1)) do
    begin
      System.Delete(Result, P1, Length(OldWord));
      for I := 1 to Replacements do
      begin
        if ((Length(NewWord) + Length(Result)) <= 255) then
        begin
          Inc(C);
          System.Insert(NewWord, Result, P1);
        end
        else
        begin
          Replacements := C;
          Exit;
        end;
      end;
    end;
    Replacements := C;
  end;
end;


function ReplaceStringS(const S, OldString, NewString: ShortString; N: cardinal;
  var Replacements: cardinal): ShortString;
var
  I, C, P1: cardinal;
  TmpStr: ShortString;
begin
  if (S = '') or (OldString = '') or (Pos(OldString, S) = 0) then
  begin
    Result := S;
    Replacements := 0;
    Exit;
  end;
  TmpStr := S;

  I := 1;
  P1 := Pos(OldString, TmpStr);
  C := P1;
  while (I < N) and (C < Length(TmpStr)) do
  begin
    Inc(I);
    System.Delete(TmpStr, 1, P1 + Length(OldString));
    Inc(C, P1 + Length(OldString));
  end;
  Result := S;
  System.Delete(Result, C, Length(OldString));

  C := 0;
  for I := 1 to Replacements do
  begin
    if ((Length(NewString) + Length(Result)) <= 255) then
    begin
      Inc(C);
      System.Insert(NewString, Result, P1);
      Inc(P1, Length(NewString) + 1);
    end
    else
    begin
      Replacements := C;
      Exit;
    end;
  end;
end;


function ReplaceStringAllS(const S, OldString, NewString: ShortString; var Replacements: cardinal): ShortString;
var
  I, C, P1: cardinal;
  Tmp: string;
begin
  Result := S;
  if (S = '') or (OldString = '') or (Pos(OldString, S) = 0) then
    Replacements := 0
  else
  begin
    Tmp := S;
    P1 := Pos(OldString, S);
    if (P1 > 0) then
    begin
      Result := Copy(Tmp, 1, P1 - 1);
      C := 0;
      while (P1 > 0) do
      begin
        for I := 1 to Replacements do
        begin
          Inc(C);
          Result := Result + NewString;
        end;
        Tmp := Copy(Tmp, P1 + Length(OldString), MaxInt);
        P1 := Pos(OldString, Tmp);
        if (P1 > 0) then
        begin
          Result := Result + Copy(Tmp, 1, P1 - 1);
          {Tmp := Copy(Tmp, P1, MaxInt)};
        end
        else
          Result := Result + Tmp;
      end;
      Replacements := C;
    end
    else
    begin
      Result := S;
      Replacements := 0;
    end;
  end;
end;

function LastWordS(const S, WordDelims, AWord: ShortString; var Position: cardinal): boolean;
var
  TmpStr: ShortString;
  I: cardinal;
begin
  if (S = '') or (WordDelims = '') or (AWord = '') or (Pos(AWord, S) = 0) then
  begin
    Result := False;
    Position := 0;
    Exit;
  end;

  TmpStr := S;
  I := Length(TmpStr);
  while (Pos(TmpStr[I], WordDelims) > 0) do
  begin
    System.Delete(TmpStr, I, 1);
    I := Length(TmpStr);
  end;

  Position := Length(TmpStr);
  repeat
    while (Pos(TmpStr[Position], WordDelims) = 0) and (Position > 1) do
      Dec(Position);
    if (Copy(TmpStr, Position + 1, Length(AWord)) = AWord) then
    begin
      Inc(Position);
      Result := True;
      Exit;
    end;
    System.Delete(TmpStr, Position, Length(TmpStr));
    Position := Length(TmpStr);
  until (Length(TmpStr) = 0);
  Result := False;
  Position := 0;
end;



function LastWordAbsS(const S, WordDelims: ShortString; var Position: cardinal): boolean;
begin
  if (S = '') or (WordDelims = '') then
  begin
    Result := False;
    Position := 0;
    Exit;
  end;

  {find first non-delimiter character, if any. If not a "one-word wonder"}
  Position := Length(S);
  while (Position > 0) and (Pos(S[Position], WordDelims) > 0) do
    Dec(Position);

  if (Position = 0) then
  begin
    Result := True;
    Position := 1;
    Exit;
  end;

  {find next delimiter character}
  while (Position > 0) and (Pos(S[Position], WordDelims) = 0) do
    Dec(Position);
  Inc(Position);
  Result := True;
end;



function LastStringS(const S, AString: ShortString; var Position: cardinal): boolean;
var
  TmpStr: ShortString;
  I, C: cardinal;
begin
  if (S = '') or (AString = '') or (Pos(AString, S) = 0) then
  begin
    Result := False;
    Position := 0;
    Exit;
  end;

  TmpStr := S;
  C := 0;
  I := Pos(AString, TmpStr);
  while (I > 0) do
  begin
    Inc(C, I + Length(AString));
    System.Delete(TmpStr, 1, I + Length(AString));
    I := Pos(AString, TmpStr);
  end;
  {Go back the length of AString since the while loop deletes the last instance}
  Dec(C, Length(AString));
  Position := C;
  Result := True;
end;



function KeepCharsS(const S, Chars: ShortString): ShortString;
var
  FromInx: cardinal;
  ToInx: cardinal;
begin
  {if either the input string or the list of acceptable chars is empty
   the destination string will also be empty}
  if (S = '') or (Chars = '') then
  begin
    Result := '';
    Exit;
  end;

  {set the maximum length of the result string (it could be less than
   this, of course}
  Result[0] := AnsiChar(length(S));

  {start off the to index}
  ToInx := 0;

  {in a loop, copy over the chars that match the list}
  for FromInx := 1 to length(S) do
    if CharExistsS(Chars, S[FromInx]) then
    begin
      Inc(ToInx);
      Result[ToInx] := S[FromInx];
    end;

  {make sure that the length of the result string is correct}
  Result[0] := AnsiChar(ToInx);
end;



function RepeatStringS(const RepeatString: ShortString; var Repetitions: cardinal;
  MaxLen: cardinal): ShortString;
var
  i: cardinal;
  Len: cardinal;
  ActualReps: cardinal;
begin
  Result := '';
  if (MaxLen <> 0) and (Repetitions <> 0) and (RepeatString <> '') then
  begin
    if (MaxLen > 255) then
      MaxLen := 255;
    Len := length(RepeatString);
    ActualReps := MaxLen div Len;
    if (ActualReps > Repetitions) then
      ActualReps := Repetitions
    else
      Repetitions := ActualReps;
    if (ActualReps > 0) then
    begin
      Result[0] := AnsiChar(ActualReps * Len);
      for i := 0 to pred(ActualReps) do
        Move(RepeatString[1], Result[i * Len + 1], Len);
    end;
  end;
end;



function TrimCharsS(const S, Chars: ShortString): ShortString;
begin
  Result := RightTrimCharsS(S, Chars);
  Result := LeftTrimCharsS(Result, Chars);
end;



function RightTrimCharsS(const S, Chars: ShortString): ShortString;
var
  CutOff: integer;
begin
  CutOff := length(S);
  while (CutOff > 0) do
  begin
    if not CharExistsS(Chars, S[CutOff]) then
      Break;
    Dec(CutOff);
  end;
  if (CutOff = 0) then
    Result := ''
  else
    Result := Copy(S, 1, CutOff);
end;



function LeftTrimCharsS(const S, Chars: ShortString): ShortString;
var
  CutOff: integer;
  LenS: integer;
begin
  LenS := length(S);
  CutOff := 1;
  while (CutOff <= LenS) do
  begin
    if not CharExistsS(Chars, S[CutOff]) then
      Break;
    Inc(CutOff);
  end;
  if (CutOff > LenS) then
    Result := ''
  else
    Result := Copy(S, CutOff, LenS - CutOff + 1);
end;



function ExtractTokensS(const S, Delims: ShortString; QuoteChar: AnsiChar;
  AllowNulls: boolean; Tokens: TStrings): cardinal;
var
  State: (ScanStart, ScanQuotedToken, ScanQuotedTokenEnd, ScanNormalToken, ScanNormalTokenWithQuote);
  CurChar: AnsiChar;
  TokenStart: integer;
  Inx: integer;
begin
  {Notes: this routine implements the following state machine
    start ----> ScanStart
    ScanStart-----quote----->ScanQuotedToken
    ScanStart-----delim----->ScanStart (1)
    ScanStart-----other----->ScanNormalToken
    ScanQuotedToken-----quote----->ScanQuotedTokenEnd
    ScanQuotedToken-----other----->ScanQuotedToken
    ScanQuotedTokenEnd-----quote----->ScanNormalTokenWithQuote
    ScanQuotedTokenEnd-----delim----->ScanStart (2)
    ScanQuotedTokenEnd-----other----->ScanNormalToken
    ScanNormalToken-----quote----->ScanNormalTokenWithQuote
    ScanNormalToken-----delim----->ScanStart (3)
    ScanNormalToken-----other----->ScanNormalToken
    ScanNormalTokenWithQuote-----quote----->ScanNormalTokenWithQuote
    ScanNormalTokenWithQuote-----other----->ScanNormalToken

    (1) output a null token if allowed
    (2) output a token, stripping quotes (if the dequoted token is
        empty, output a null token if allowed)
    (3) output a token; no quote stripping

    If the quote character is #0, it's taken to mean that the routine
    should not check for quoted substrings.}

  {clear the tokens string list, set the return value to zero}
  Tokens.Clear;
  Result := 0;

  {if the input string is empty or the delimiter list is empty or
   the quote character is found in the delimiter list, return zero
   tokens found}
  if (S = '') or (Delims = '') or CharExistsS(Delims, QuoteChar) then
    Exit;

  {start off in the normal scanning state}
  State := ScanStart;

  {the first token starts at position 1}
  TokenStart := 1;

  {read through the entire string}
  for Inx := 1 to length(S) do
  begin

    {get the current character}
    CurChar := S[Inx];

    {process the character according to the current state}
    case State of
      ScanStart:
      begin
        {if the current char is the quote character, switch states}
        if (QuoteChar <> #0) and (CurChar = QuoteChar) then
          State := ScanQuotedToken

        {if the current char is a delimiter, output a null token}
        else if CharExistsS(Delims, CurChar) then
        begin

          {if allowed to, output a null token}
          if AllowNulls then
          begin
            Tokens.Add('');
            Inc(Result);
          end;

            {set the start of the next token to be one character after
             this delimiter}
          TokenStart := succ(Inx);
        end

          {otherwise, the current char is starting a normal token, so
           switch states}
        else
          State := ScanNormalToken;
      end;

      ScanQuotedToken:
      begin
        {if the current char is the quote character, switch states}
        if (CurChar = QuoteChar) then
          State := ScanQuotedTokenEnd;
      end;

      ScanQuotedTokenEnd:
      begin
          {if the current char is the quote character, we have a token
           consisting of two (or more) quoted substrings, so switch
           states}
        if (CurChar = QuoteChar) then
          State := ScanNormalTokenWithQuote

          {if the current char is a delimiter, output the token
           without the quotes}
        else if CharExistsS(Delims, CurChar) then
        begin

            {if the token is empty without the quotes, output a null
             token only if allowed to}
          if ((Inx - TokenStart) = 2) then
          begin
            if AllowNulls then
            begin
              Tokens.Add('');
              Inc(Result);
            end;
          end

          {else output the token without the quotes}
          else
          begin
            Tokens.Add(Copy(S, succ(TokenStart), Inx - TokenStart - 2));
            Inc(Result);
          end;

            {set the start of the next token to be one character after
             this delimiter}
          TokenStart := succ(Inx);

          {switch states back to the start state}
          State := ScanStart;
        end

        {otherwise it's a (complex) normal token, so switch states}
        else
          State := ScanNormalToken;
      end;

      ScanNormalToken:
      begin
          {if the current char is the quote character, we have a
           complex token with at least one quoted substring, so switch
           states}
        if (QuoteChar <> #0) and (CurChar = QuoteChar) then
          State := ScanNormalTokenWithQuote

        {if the current char is a delimiter, output the token}
        else if CharExistsS(Delims, CurChar) then
        begin
          Tokens.Add(Copy(S, TokenStart, Inx - TokenStart));
          Inc(Result);

            {set the start of the next token to be one character after
             this delimiter}
          TokenStart := succ(Inx);

          {switch states back to the start state}
          State := ScanStart;
        end;
      end;

      ScanNormalTokenWithQuote:
      begin
          {if the current char is the quote character, switch states
           back to scanning a normal token}
        if (CurChar = QuoteChar) then
          State := ScanNormalToken;
      end;

    end;
  end;

  {we need to process the (possible) final token: first assume that
   the current character index is just beyond the end of the string}
  Inx := succ(length(S));

  {if we are in the scanning quoted token state, we've read an opening
   quote, but no closing one; increment the token start value}
  if (State = ScanQuotedToken) then
    Inc(TokenStart)

  {if we've finished scanning a quoted token, we've read both quotes;
   increment the token start value, and decrement the current index}
  else if (State = ScanQuotedTokenEnd) then
  begin
    Inc(TokenStart);
    Dec(Inx);
  end;

  {if the final token is not empty, output the token}
  if (TokenStart < Inx) then
  begin
    Tokens.Add(Copy(S, TokenStart, Inx - TokenStart));
    Inc(Result);
  end
  {otherwise the final token is empty, so output a null token if
   allowed to}
  else if AllowNulls then
  begin
    Tokens.Add('');
    Inc(Result);
  end;
end;



function ContainsOnlyS(const S, Chars: ShortString; var BadPos: cardinal): boolean;
var
  I: cardinal;
begin
  if (S = '') then
  begin
    Result := False;
    BadPos := 0;
  end
  else
  begin
    for I := 1 to Length(S) do
    begin
      if (not CharExistsS(Chars, S[I])) then
      begin
        BadPos := I;
        Result := False;
        Exit;
      end;
    end;
    Result := True;
    BadPos := 0;
  end;
end;



function ContainsOtherThanS(const S, Chars: ShortString; var BadPos: cardinal): boolean;
var
  I: cardinal;
begin
  if (S = '') then
  begin
    Result := False;
    BadPos := 0;
  end
  else
  begin
    for I := 1 to Length(S) do
    begin
      if (CharExistsS(Chars, S[I])) then
      begin
        BadPos := I;
        Result := True;
        Exit;
      end;
    end;
    Result := False;
    BadPos := 0;
  end;
end;


function IsChNumericS(C: AnsiChar; const Numbers: ShortString): boolean;
  {-Returns true if Ch in numeric set}
begin
  Result := CharExistsS(Numbers, C);
end;



function IsStrNumericS(const S, Numbers: ShortString): boolean;
  {-Returns true if all characters in string are in numeric set}
var
  i: cardinal;
begin
  Result := False;
  if (length(S) > 0) then
  begin
    for i := 1 to Length(S) do
      if not CharExistsS(Numbers, S[i]) then
        Exit;
    Result := True;
  end;
end;

function StrWithinS(const S, SearchStr: ShortString; Start: cardinal; var Position: cardinal): boolean;
var
  TmpStr: ShortString;
begin
  TmpStr := S;
  if (Start > 1) then
    System.Delete(TmpStr, 1, Start - 1);
  Position := pos(SearchStr, TmpStr);
  if (Position > 0) then
  begin
    Position := Position + Start - 1;
    Result := True;
  end
  else
    Result := False;
end;


function WordPosS(const S, WordDelims, AWord: ShortString; N: cardinal; var Position: cardinal): boolean;
  {-returns the Nth instance of a given word within a string}
var
  TmpStr: ShortString;
  Len, I, P1, P2: cardinal;
begin
  if (S = '') or (AWord = '') or (Pos(AWord, S) = 0) or (N < 1) then
  begin
    Result := False;
    Position := 0;
    Exit;
  end;

  Result := False;
  Position := 0;

  TmpStr := S;
  I := 0;
  Len := Length(AWord);
  P1 := Pos(AWord, TmpStr);

  while (P1 > 0) and (Length(TmpStr) > 0) do
  begin
    P2 := P1 + pred(Len);
    if (P1 = 1) then
    begin
      if (Pos(TmpStr[P2 + 1], WordDelims) > 0) then
      begin
        Inc(I);
      end
      else
        System.Delete(TmpStr, 1, P2);
    end
    else if (Pos(TmpStr[P1 - 1], WordDelims) > 0) and ((Pos(TmpStr[P2 + 1], WordDelims) > 0) or (P2 + 1 = Length(TmpStr))) then
    begin
      Inc(I);
    end
    else if ((P1 + pred(Len)) = Length(TmpStr)) then
    begin
      if (P1 > 1) and (Pos(TmpStr[P1 - 1], WordDelims) > 0) then
        Inc(I);
    end;

    if (I = N) then
    begin
      Result := True;
      Position := Position + P1;
      Exit;
    end;
    System.Delete(TmpStr, 1, P2);
    Position := Position + P2;
    P1 := Pos(AWord, TmpStr);
  end;
end;


end.
