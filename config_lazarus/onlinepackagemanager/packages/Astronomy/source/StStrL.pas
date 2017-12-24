{**********************************************************************                   
 Package pl_Astronomy.pkg
 This unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit StStrL;

interface

uses
  LCLIntf, LCLType, LMessages,
  Classes,
  SysUtils,
  StConst,
  StBase;

{.Z+}
type
  LStrRec = record
    AllocSize : Longint;
    RefCount  : Longint;
    Length    : Longint;
  end;

const
  StrOffset = SizeOf(LStrRec);
{.Z-}

  {-------- Numeric conversion -----------}

function HexBL(B : Byte) : AnsiString;
  {-Return the hex string for a byte.}

function HexWL(W : Word) : AnsiString;
  {-Return the hex string for a word.}

function HexLL(L : LongInt) : AnsiString;
  {-Return the hex string for a long integer.}

function HexPtrL(P : Pointer) : AnsiString;
  {-Return the hex string for a pointer.}

function BinaryBL(B : Byte) : AnsiString;
  {-Return a binary string for a byte.}

function BinaryWL(W : Word) : AnsiString;
  {-Return the binary string for a word.}

function BinaryLL(L : LongInt) : AnsiString;
  {-Return the binary string for a long integer.}

function OctalBL(B : Byte) : AnsiString;
  {-Return an octal string for a byte.}

function OctalWL(W : Word) : AnsiString;
  {-Return an octal string for a word.}

function OctalLL(L : LongInt) : AnsiString;
  {-Return an octal string for a long integer.}


function Long2StrL(L : LongInt) : String;
  {-Convert an integer type to a string.}

function Real2StrL(R : Double; Width : Byte; Places : ShortInt) : String;
  {-Convert a real to a string.}

function Ext2StrL(R : Extended; Width : Byte; Places : ShortInt) : String;
  {-Convert an extended to a string.}


  {-------- General purpose string manipulation --------}

function CharStrL(C : Char; Len : Cardinal) : String;
  {-Return a string filled with the specified character.}

function PadChL(const S : String; C : Char; Len : Cardinal) : String;
  {-Pad a string on the right with a specified character.}

function PadL(const S : String; Len : Cardinal) : String;
  {-Pad a string on the right with spaces.}

function LeftPadChL(const S : String; C : Char; Len : Cardinal) : String;
  {-Pad a string on the left with a specified character.}

function LeftPadL(const S : String; Len : Cardinal) : String;
  {-Pad a string on the left with spaces.}

function TrimLeadL(const S : String) : String;
  {-Return a string with leading white space removed.}

function TrimTrailL(const S : String) : String;
  {-Return a string with trailing white space removed.}

function TrimL(const S : String) : String;
  {-Return a string with leading and trailing white space removed.}

function TrimSpacesL(const S : String) : String;
  {-Return a string with leading and trailing spaces removed.}

function CenterChL(const S : String; C : Char; Len : Cardinal) : String;
  {-Pad a string on the left and right with a specified character.}

function CenterL(const S : String; Len : Cardinal) : String;
  {-Pad a string on the left and right with spaces.}


function ScrambleL(const S, Key : AnsiString) : AnsiString;
  {-Encrypt / Decrypt string with enhanced XOR encryption.}


  {--------------- Word / Char manipulation -------------------------}


procedure WordWrapL(const InSt : String; var OutSt, Overlap : String;
                   Margin : Cardinal; PadToMargin : Boolean);
  {-Wrap a text string at a specified margin.}

function JustFilenameL(const PathName : String) : String;
  {-Return just the filename and extension of a pathname.}


function JustPathnameL(const PathName : String) : String;
  {-Return just the drive and directory portion of a pathname.}

function AddBackSlashL(const DirName : String) : String;
  {-Add a default backslash to a directory name.}



  {------------------ Formatting routines --------------------}

function CommaizeL(L : LongInt) : String;
  {-Convert a long integer to a string with commas.}

function CommaizeChL(L : Longint; Ch : Char) : String;
  {-Convert a long integer to a string with Ch in comma positions.}

function StrStPosL(const P, S : String; var Pos : Cardinal) : Boolean;
  {-Return the position of a specified substring within a string.}

function StrStCopyL(const S : String; Pos, Count : Cardinal) : String;
  {-Copy characters at a specified position in a string.}

function StrChInsertL(const S : String; C : Char; Pos : Cardinal) : String;
  {-Insert a character into a string at a specified position.}

function StrStInsertL(const S1, S2 : String; Pos : Cardinal) : String;
  {-Insert a string into another string at a specified position.}

function StrChDeleteL(const S : String; Pos : Cardinal) : String;
  {-Delete the character at a specified position in a string.}

function StrStDeleteL(const S : String; Pos, Count : Cardinal) : String;
  {-Delete characters at a specified position in a string.}


{--------------------------  New Functions -----------------------------------}



function CopyLeftL(const S : String; Len : Cardinal) : String;
  {-Return the left Len characters of a string}

function CopyMidL(const S : String; First, Len : Cardinal) : String;
  {-Return the mid part of a string}

function CopyRightL(const S : String; First : Cardinal) : String;
  {-Return the right Len characters of a string}

function CopyRightAbsL(const S : String; NumChars : Cardinal) : String;
  {-Return NumChar characters starting from end}

function CopyFromNthWordL(const S, WordDelims : String;
                         const AWord : String; N : Cardinal;       {!!.02}
                         var SubString : String) : Boolean;

function CopyFromToWordL(const S, WordDelims, Word1, Word2 : String;
                         N1, N2 : Cardinal;
                         var SubString : String) : Boolean;

function CopyWithinL(const S, Delimiter : String;
                    Strip : Boolean) : String;

function DeleteFromNthWordL(const S, WordDelims : String;
                            const AWord : String; N : Cardinal;    {!!.02}
                            var SubString : String) : Boolean;

function DeleteFromToWordL(const S, WordDelims, Word1, Word2 : String;
                           N1, N2 : Cardinal;
                           var SubString : String) : Boolean;

function DeleteWithinL(const S, Delimiter : String) : String;



function LastWordL(const S, WordDelims, AWord : String;
                   var Position : Cardinal) : Boolean;

function LastWordAbsL(const S, WordDelims : String;
                        var Position : Cardinal) : Boolean;

function LastStringL(const S, AString : String;
                     var Position : Cardinal) : Boolean;


function ReplaceWordL(const S, WordDelims, OldWord, NewWord : String;
                      N : Cardinal;
                      var Replacements : Cardinal) : String;

function ReplaceWordAllL(const S, WordDelims, OldWord, NewWord : String;
                         var Replacements : Cardinal) : String;

function ReplaceStringL(const S, OldString, NewString : String;
                        N : Cardinal;
                        var Replacements : Cardinal) : String;

function ReplaceStringAllL(const S, OldString, NewString : String;
                           var Replacements : Cardinal) : String;

function RepeatStringL(const RepeatString : String;
                       var Repetitions : Cardinal;
                       MaxLen : Cardinal) : String;


function StrWithinL(const S, SearchStr : string;
                    Start : Cardinal;
                    var Position : Cardinal) : boolean;
  {-finds the position of a substring within a string starting at a given point}


function WordPosL(const S, WordDelims, AWord : String;
                  N : Cardinal; var Position : Cardinal) : Boolean;
  {-returns the Occurrence instance of a word within a string}


implementation

  {-------- Numeric conversion -----------}

function HexBL(B : Byte) : AnsiString;
  {-Return the hex string for a byte.}
begin
  SetLength(Result, 2);
  Result[1] := StHexDigits[B shr 4];
  Result[2] := StHexDigits[B and $F];
end;

function HexWL(W : Word) : AnsiString;
  {-Return the hex string for a word.}
begin
  SetLength(Result, 4);
  Result[1] := StHexDigits[hi(W) shr 4];
  Result[2] := StHexDigits[hi(W) and $F];
  Result[3] := StHexDigits[lo(W) shr 4];
  Result[4] := StHexDigits[lo(W) and $F];
end;

function HexLL(L : LongInt) : AnsiString;
  {-Return the hex string for a long integer.}
begin
  SetLength(Result, 8);
  Result := HexWL(HiWord(DWORD(L))) + HexWL(LoWord(DWORD(L)));         {!!.02}
end;

function HexPtrL(P : Pointer) : AnsiString;
  {-Return the hex string for a pointer.}
begin
  SetLength(Result, 9);
  Result := ':' + HexLL(LongInt(P));
end;

function BinaryBL(B : Byte) : AnsiString;
  {-Return a binary string for a byte.}
var
  I, N : Word;
begin
  N := 1;
  SetLength(Result, 8);
  for I := 7 downto 0 do begin
    Result[N] := StHexDigits[Ord(B and (1 shl I) <> 0)]; {0 or 1}
    Inc(N);
  end;
end;

function BinaryWL(W : Word) : AnsiString;
  {-Return the binary string for a word.}
var
  I, N : Word;
begin
  N := 1;
  SetLength(Result, 16);
  for I := 15 downto 0 do begin
    Result[N] := StHexDigits[Ord(W and (1 shl I) <> 0)]; {0 or 1}
    Inc(N);
  end;
end;

function BinaryLL(L : LongInt) : AnsiString;
  {-Return the binary string for a long integer.}
var
  I : Longint;
  N : Byte;
begin
  N := 1;
  SetLength(Result, 32);
  for I := 31 downto 0 do begin
    Result[N] := StHexDigits[Ord(L and LongInt(1 shl I) <> 0)]; {0 or 1}
    Inc(N);
  end;
end;

function OctalBL(B : Byte) : AnsiString;
  {-Return an octal string for a byte.}
var
  I : Word;
begin
  SetLength(Result, 3);
  for I := 0 to 2 do begin
    Result[3-I] := StHexDigits[B and 7];
    B := B shr 3;
  end;
end;

function OctalWL(W : Word) : AnsiString;
  {-Return an octal string for a word.}
var
  I : Word;
begin
  SetLength(Result, 6);
  for I := 0 to 5 do begin
    Result[6-I] := StHexDigits[W and 7];
    W := W shr 3;
  end;
end;

function OctalLL(L : LongInt) : AnsiString;
  {-Return an octal string for a long integer.}
var
  I : Word;
begin
  SetLength(Result, 12);
  for I := 0 to 11 do begin
    Result[12-I] := StHexDigits[L and 7];
    L := L shr 3;
  end;
end;



function Long2StrL(L : LongInt) : String;
  {-Convert an integer type to a string.}
begin
  Str(L, Result);
end;

function Real2StrL(R : Double; Width : Byte; Places : ShortInt) : String;
  {-Convert a real to a string.}
begin
  Str(R:Width:Places, Result);
end;

function Ext2StrL(R : Extended; Width : Byte; Places : ShortInt) : String;
  {-Convert an extended to a string.}
begin
  Str(R:Width:Places, Result);
end;


  {-------- General purpose string manipulation --------}

function CharStrL(C : Char; Len : Cardinal) : String;
  {-Return a string filled with the specified character.}
begin
  Result := StringOfChar(C, Len)
end;

function PadChL(const S : String; C : Char; Len : Cardinal) : String;
  {-Pad a string on the right with a specified character.}
{$IFDEF UNICODE}
begin
  Result := S;
  if Length(Result) < Len then
    Result := Result + StringOfChar(C, Len - Length(Result));
end;
{$ELSE}
begin
  if Length(S) >= LongInt(Len) then
    Result := S
  else begin
    SetLength(Result, Len);
    { copy current contents (if any) of S to Result }
    if (Length(S) > 0) then                                              {!!.01}
      Move(S[1], Result[1], Length(S));

    { add pad chars }
    FillChar(Result[Succ(Length(S))], LongInt(Len)-Length(S), C);
  end;
end;
{$ENDIF}

function PadL(const S : String; Len : Cardinal) : String;
  {-Pad a string on the right with spaces.}
begin
  Result := PadChL(S, ' ', Len);
end;

function LeftPadChL(const S : String; C : Char; Len : Cardinal) : String;
  {-Pad a string on the left with a specified character.}
begin
  {$IFDEF UNICODE}
  if Length(S) > Len then
    Result := S
  else
    Result := StringOfChar(C, Len - Length(S)) + S;
  {$ELSE}
  if Length(S) >= LongInt(Len) then
    Result := S
  else if Length(S) < MaxLongInt then begin
    SetLength(Result, Len);

    { copy current contents (if any) of S to Result }
    if (Length(S) > 0) then                                              {!!.01}
      Move(S[1], Result[Succ(Word(Len))-Length(S)], Length(S));

    { add pad chars }
    FillChar(Result[1], LongInt(Len)-Length(S), C);
  end;
  {$ENDIF}
end;

function LeftPadL(const S : String; Len : Cardinal) : String;
  {-Pad a string on the left with spaces.}
begin
  Result := LeftPadChL(S, ' ', Len);
end;

function TrimLeadL(const S : String) : String;
  {-Return a string with leading white space removed}
begin
  Result := TrimLeft(S);
end;

function TrimTrailL(const S : String) : String;
  {-Return a string with trailing white space removed.}
begin
  Result := TrimRight(S);
end;

function TrimL(const S : String) : String;
  {-Return a string with leading and trailing white space removed.}
var
  I : Longint;
begin
  Result := S;
  while (Length(Result) > 0) and (Result[Length(Result)] <= ' ') do
    SetLength(Result, Pred(Length(Result)));

  I := 1;
  while (I <= Length(Result)) and (Result[I] <= ' ') do
    Inc(I);
  Dec(I);
  if I > 0 then
    System.Delete(Result, 1, I);
end;

function TrimSpacesL(const S : String) : String;
  {-Return a string with leading and trailing spaces removed.}
var
  I : Longint;
begin
  Result := S;
  while (Length(Result) > 0) and (Result[Length(Result)] = ' ') do
    SetLength(Result, Pred(Length(Result)));
  I := 1;
  while (I <= Length(Result)) and (S[I] = ' ') do
    Inc(I);
  Dec(I);
  if I > 0 then
    System.Delete(Result, 1, I);
end;

function CenterChL(const S : String; C : Char; Len : Cardinal) : String;
  {-Pad a string on the left and right with a specified character.}
begin
  if Length(S) >= LongInt(Len) then
    Result := S
  else if Length(S) < MaxLongInt then begin
//    SetLength(Result, Len);
//    FillChar(Result[1], Len, C);
    Result := StringOfChar(C, Len);
    if Length(S) > 0 then                                                {!!.01}
      Move(S[1], Result[Succ((LongInt(Len)-Length(S)) shr 1)], Length(S)*SizeOf(Char));
  end;
end;

function CenterL(const S : String; Len : Cardinal) : String;
  {-Pad a string on the left and right with spaces.}
begin
  Result := CenterChL(S, ' ', Len);
end;

function ScrambleL(const S, Key : AnsiString) : AnsiString;
  {-Encrypt / Decrypt string with enhanced XOR encryption.}
var
  I, J, LKey, LStr : Cardinal;
begin
  Result := S;
  if Key = '' then Exit;
  if S = '' then Exit;
  LKey := Length(Key);
  LStr := Length(S);
  I := 1;
  J := LKey;
  while I <= LStr do begin
    if J = 0 then
      J := LKey;
    if (S[I] <> Key[J]) then
      Result[I] := AnsiChar(Byte(S[I]) xor Byte(Key[J]));
    Inc(I);
    Dec(J);
  end;
end;

  {--------------- Word / Char manipulation -------------------------}




procedure WordWrapL(const InSt : String; var OutSt, Overlap : String;
                   Margin : Cardinal; PadToMargin : Boolean);
  {-Wrap a text string at a specified margin.}
var
  InStLen  : Cardinal;
  EOS, BOS : Cardinal;
  Len : Integer;                                                       {!!.02}
begin
  InStLen := Length(InSt);

{!!.02 - Added }
  { handle empty string on input }
  if InStLen = 0 then begin
    OutSt := '';
    Overlap := '';
    Exit;
  end;
{!!.02 - End Added }

  {find the end of the output string}
  if InStLen > Margin then begin
    {find the end of the word at the margin, if any}
    EOS := Margin;
    while (EOS <= InStLen) and (InSt[EOS] <> ' ') do
      Inc(EOS);
    if EOS > InStLen then
      EOS := InStLen;

    {trim trailing blanks}
    while (EOS > 0) and (InSt[EOS] = ' ') do                           {!!.04}
      Dec(EOS);

    if EOS > Margin then begin
      {look for the space before the current word}
      while (EOS > 0) and (InSt[EOS] <> ' ') do
        Dec(EOS);

      {if EOS = 0 then we can't wrap it}
      if EOS = 0 then
        EOS := Margin
      else
        {trim trailing blanks}
        while (EOS > 0) and (InSt[EOS] = ' ') do                       {!!.04}
          Dec(EOS);
    end;
  end else
    EOS := InStLen;

  {copy the unwrapped portion of the line}
  if EOS > 0 then begin                                                {!!.04}
    SetLength(OutSt, EOS);
    Move(InSt[1], OutSt[1], Length(OutSt) * SizeOf(Char));
  end;                                                                 {!!.04}

  {find the start of the next word in the line}
  BOS := Succ(EOS);
  while (BOS <= InStLen) and (InSt[BOS] = ' ') do
    Inc(BOS);

  if BOS > InStLen then
    SetLength(OverLap, 0)
  else begin
    {copy from the start of the next word to the end of the line}

    SetLength(OverLap, InStLen);
    Move(InSt[BOS], Overlap[1], Succ(InStLen-BOS) * SizeOf(Char));
    SetLength(OverLap, Succ(InStLen-BOS));
  end;

  {pad the end of the output string if requested}
{!!.02 - Rewritten}
  Len := Length(OutSt);
  if PadToMargin and (Len < LongInt(Margin)) then begin
//    SetLength(OutSt, Margin);
//    FillChar(OutSt[Succ(Len)], LongInt(Margin)-Length(OutSt), ' ');
    OutSt := OutSt + StringOfChar(' ', Margin - Length(OutSt));
  end;
{!!.02 - End Rewritten}
end;


function JustFilenameL(const PathName : String) : String;
  {-Return just the filename and extension of a pathname.}
var
  I : Cardinal;
begin
  Result := '';
  if PathName = '' then Exit;
  I := Succ(Cardinal(Length(PathName)));
  repeat
    Dec(I);
  until (I = 0) or (PathName[I] in DosDelimSet);                         {!!.01}
  Result := System.Copy(PathName, Succ(I), StMaxFileLen);
end;



function JustPathnameL(const PathName : String) : String;
  {-Return just the drive and directory portion of a pathname.}
var
  I : Cardinal;
begin
  if PathName = '' then Exit;

  I := Succ(Cardinal(Length(PathName)));
  repeat
    Dec(I);
  until (I = 0) or (PathName[I] in DosDelimSet);                         {!!.01}

  if I = 0 then
    {Had no drive or directory name}
    SetLength(Result, 0)
  else if I = 1 then
    {Either the root directory of default drive or invalid pathname}
    Result := PathName[1]
  else if (PathName[I] = '\') then begin
    if PathName[Pred(I)] = ':' then
      {Root directory of a drive, leave trailing backslash}
      Result := System.Copy(PathName, 1, I)
    else
      {Subdirectory, remove the trailing backslash}
      Result := System.Copy(PathName, 1, Pred(I));
  end else
    {Either the default directory of a drive or invalid pathname}
    Result := System.Copy(PathName, 1, I);
end;

function AddBackSlashL(const DirName : String) : String;
  {-Add a default backslash to a directory name}
begin
  Result := DirName;
  if (Length(Result) = 0) then
    Exit;
  if ((Length(Result) = 2) and (Result[2] = ':')) or
     ((Length(Result) > 2) and (Result[Length(Result)] <> '\')) then
    Result := Result + '\';
end;


  {------------------ Formatting routines --------------------}


function CommaizeChL(L : Longint; Ch : Char) : String;
  {-Convert a long integer to a string with Ch in comma positions}
var
  Temp : string;
  NumCommas, I, Len : Cardinal;
  Neg : Boolean;
begin
  SetLength(Temp, 1);
  Temp[1] := Ch;
  if L < 0 then begin
    Neg := True;
    L := Abs(L);
  end else
    Neg := False;
  Result := Long2StrL(L);
  Len := Length(Result);
  NumCommas := (Pred(Len)) div 3;
  for I := 1 to NumCommas do
    System.Insert(Temp, Result, Succ(Len-(I * 3)));
  if Neg then
    System.Insert('-', Result, 1);
end;

function CommaizeL(L : LongInt) : String;
  {-Convert a long integer to a string with commas}
begin
  Result := CommaizeChL(L, ',');
end;


function StrStPosL(const P, S : String; var Pos : Cardinal) : Boolean;
  {-Return the position of a specified substring within a string.}
begin
  Pos := System.Pos(S, P);
  Result := Pos <> 0;
end;

function StrStCopyL(const S : String; Pos, Count : Cardinal) : String;
  {-Copy characters at a specified position in a string.}
begin
  Result := System.Copy(S, Pos, Count);
end;

function StrChInsertL(const S : String; C : Char; Pos : Cardinal) : String;
var
  Temp : string;
begin
  SetLength(Temp, 1);
  Temp[1] := C;
  Result := S;
  System.Insert(Temp, Result, Pos);
end;

function StrStInsertL(const S1, S2 : String; Pos : Cardinal) : String;
  {-Insert a string into another string at a specified position.}
begin
  Result := S1;
  System.Insert(S2, Result, Pos);
end;

function StrChDeleteL(const S : String; Pos : Cardinal) : String;
  {-Delete the character at a specified position in a string.}
begin
  Result := S;
  System.Delete(Result, Pos, 1);
end;

function StrStDeleteL(const S : String; Pos, Count : Cardinal) : String;
  {-Delete characters at a specified position in a string.}
begin
  Result := S;
  System.Delete(Result, Pos, Count);
end;


{----------------------------------------------------------------------------}

function CopyLeftL(const S : String; Len : Cardinal) : String;
  {-Return the left Len characters of a string}
begin
  if (Len < 1) or (S = '') then
    Result := ''
  else
    Result := Copy(S, 1, Len);
end;

{----------------------------------------------------------------------------}

function CopyMidL(const S : String; First, Len : Cardinal) : String;
  {-Return the mid part of a string}
begin
  if (LongInt(First) > Length(S)) or (LongInt(Len) < 1) or (S = '') then
    Result := ''
  else
    Result := Copy(S, First, Len);
end;

{----------------------------------------------------------------------------}

function CopyRightL(const S : String; First : Cardinal) : String;
  {-Return the right Len characters of a string}
begin
  if (LongInt(First) > Length(S)) or (First < 1) or (S = '') then
    Result := ''
  else
    Result := Copy(S, First, Length(S));
end;

{----------------------------------------------------------------------------}

function CopyRightAbsL(const S : String; NumChars : Cardinal) : String;
  {-Return NumChar characters starting from end}
begin
  if (Cardinal(Length(S)) > NumChars) then
    Result := Copy(S, (Cardinal(Length(S)) - NumChars)+1, NumChars)
  else
    Result := S;
end;

{----------------------------------------------------------------------------}

function WordPosL(const S, WordDelims, AWord : String;
                  N : Cardinal; var Position : Cardinal) : Boolean;
  {-returns the Nth instance of a given word within a string}
var
  TmpStr : String;
  Len,
  I,
  P1,
  P2      : Cardinal;
begin
  if (S = '') or (AWord = '') or (pos(AWord, S) = 0) or (N < 1) then begin
    Result := False;
    Position := 0;
    Exit;
  end;

  Result := False;
  Position := 0;

  TmpStr := S;
  I      := 0;
  Len    := Length(AWord);
  P1     := pos(AWord, TmpStr);

  while (P1 > 0) and (Length(TmpStr) > 0) do begin
    P2 := P1 + pred(Len);
    if (P1 = 1) then begin
      if (pos(TmpStr[P2+1], WordDelims) > 0) then begin
        Inc(I);
      end else
        System.Delete(TmpStr, 1, P2);
    end else if (pos(TmpStr[P1-1], WordDelims) > 0) and
//                ((pos(TmpStr[P2+1], WordDelims) > 0) or              {!!.02}
//                 (LongInt(P2+1) = Length(TmpStr))) then begin        {!!.02}
                ((LongInt(P2+1) >= Length(TmpStr)) or                  {!!.02}
                (pos(TmpStr[P2+1], WordDelims) > 0)) then begin        {!!.02}
      Inc(I);
    end else if ((LongInt(P1 + pred(Len))) = Length(TmpStr)) then begin
      if (P1 > 1) and (pos(TmpStr[P1-1], WordDelims) > 0) then
        Inc(I);
    end;

    if (I = N) then begin
      Result := True;
      Position := Position + P1;
      Exit;
    end;
    System.Delete(TmpStr, 1, P2);
    Position := Position + P2;
    P1 := pos(AWord, TmpStr);
  end;
end;


{----------------------------------------------------------------------------}

function CopyFromNthWordL(const S, WordDelims : String;
                          const AWord : String; N : Cardinal;      {!!.02}
                          var SubString : String) : Boolean;
var
  P : Cardinal;
begin
  if (WordPosL(S, WordDelims, AWord, N, P)) then begin
    SubString := Copy(S, P, Length(S));
    Result := True;
  end else begin
    SubString := '';
    Result := False;
  end;
end;

{----------------------------------------------------------------------------}

function DeleteFromNthWordL(const S, WordDelims : String;
                            const AWord : String; N : Cardinal;    {!!.02}
                            var SubString : String) : Boolean;
var
  P : Cardinal;
begin
  SubString := S;
  if (WordPosL(S, WordDelims, AWord, N, P)) then begin
    Result := True;
    SubString := Copy(S, 1, P-1);
  end else begin
    Result := False;
    SubString := '';
  end;
end;

{----------------------------------------------------------------------------}

function CopyFromToWordL(const S, WordDelims, Word1, Word2 : String;
                         N1, N2 : Cardinal;
                         var SubString : String) : Boolean;
var
  P1,
  P2  : Cardinal;
begin
  if (WordPosL(S, WordDelims, Word1, N1, P1)) then begin
    if (WordPosL(S, WordDelims, Word2, N2, P2)) then begin
      Dec(P2);
      if (P2 > P1) then begin
        Result := True;
        SubString := Copy(S, P1, P2-P1);
      end else begin
        Result := False;
        SubString := '';
      end;
    end else begin
      Result := False;
      SubString := '';
    end;
  end else begin
    Result := False;
    SubString := '';
  end;
end;

{----------------------------------------------------------------------------}

function DeleteFromToWordL(const S, WordDelims, Word1, Word2 : String;
                           N1, N2 : Cardinal;
                           var SubString : String) : Boolean;
var
  P1,
  P2  : Cardinal;
begin
  SubString := S;
  if (WordPosL(S, WordDelims, Word1, N1, P1)) then begin
    if (WordPosL(S, WordDelims, Word2, N2, P2)) then begin
      Dec(P2);
      if (P2 > P1) then begin
        Result := True;
        System.Delete(SubString, P1, P2-P1+1);
      end else begin
        Result := False;
        SubString := '';
      end;
    end else begin
      Result := False;
      SubString := '';
    end;
  end else begin
    Result := False;
    SubString := '';
  end;
end;

{----------------------------------------------------------------------------}

function CopyWithinL(const S, Delimiter : String;
                     Strip : Boolean) : String;
var
  P1,
  P2     : Cardinal;
  TmpStr : String;
begin
  if (S = '') or (Delimiter = '') or (pos(Delimiter, S) = 0) then
    Result := ''
  else begin
    if (StrStPosL(S, Delimiter, P1)) then begin
      TmpStr := Copy(S, LongInt(P1) + Length(Delimiter), Length(S));
      if StrStPosL(TmpStr, Delimiter, P2) then begin
        Result := Copy(TmpStr, 1, P2-1);
        if (not Strip) then
          Result := Delimiter + Result + Delimiter;
      end else begin
        Result := TmpStr;
        if (not Strip) then
          Result := Delimiter + Result;
      end;
    end;
  end;
end;

{----------------------------------------------------------------------------}

function DeleteWithinL(const S, Delimiter : String) : String;
var
  P1,
  P2     : Cardinal;
  TmpStr : String;
begin
  if (S = '') or (Delimiter = '') or (pos(Delimiter, S) = 0) then
    Result := ''
  else begin
    if (StrStPosL(S, Delimiter, P1)) then begin
      TmpStr := Copy(S, LongInt(P1) + Length(Delimiter), Length(S));
      if (pos(Delimiter, TmpStr) = 0) then
        Result := Copy(S, 1, P1-1)
      else begin
        if (StrStPosL(TmpStr, Delimiter, P2)) then begin
          Result := S;
          P2 := LongInt(P2) + (2*Length(Delimiter));
          System.Delete(Result, P1, P2);
        end;
      end;
    end;
  end;
end;

{----------------------------------------------------------------------------}

function ReplaceWordL(const S, WordDelims, OldWord, NewWord : String;
                      N : Cardinal;
                      var Replacements : Cardinal) : String;
var
  I,
  C,
  P1 : Cardinal;
begin
  if (S = '') or (WordDelims = '') or (OldWord = '') or
     (pos(OldWord, S) = 0) then begin
    Result := S;
    Replacements := 0;
    Exit;
  end;

  if (WordPosL(S, WordDelims, OldWord, N, P1)) then begin
    Result := S;
    System.Delete(Result, P1, Length(OldWord));

    C := 0;
    for I := 1 to Replacements do begin
      if ((Length(NewWord)) + Length(Result)) < MaxLongInt then begin
        Inc(C);
        System.Insert(NewWord, Result, P1);
        Inc(P1, Length(NewWord) + 1);
      end else begin
        Replacements := C;
        Exit;
      end;
    end;
  end else begin
    Result := S;
    Replacements := 0;
  end;
end;


function ReplaceWordAllL(const S, WordDelims, OldWord, NewWord : String;
                         var Replacements : Cardinal) : String;
var
  I,
  C,
  P1 : Cardinal;
begin
  if (S = '') or (WordDelims = '') or (OldWord = '') or
     (Pos(OldWord, S) = 0) then begin
    Result := S;
    Replacements := 0;
  end else begin
    Result := S;
    C := 0;
    while (WordPosL(Result, WordDelims, OldWord, 1, P1)) do begin
      System.Delete(Result, P1, Length(OldWord));
      for I := 1 to Replacements do begin
        if ((Length(NewWord) + Length(Result)) < MaxLongInt) then begin
          Inc(C);
          System.Insert(NewWord, Result, P1);
        end else begin
          Replacements := C;
          Exit;
        end;
      end;
    end;
    Replacements := C;
  end;
end;


{----------------------------------------------------------------------------}

function ReplaceStringL(const S, OldString, NewString : String;
                        N : Cardinal;
                        var Replacements : Cardinal) : String;
var
  I,
  C,
  P1 : Cardinal;
  TmpStr : String;
begin
  if (S = '') or (OldString = '') or (pos(OldString, S) = 0) then begin
    Result := S;
    Replacements := 0;
    Exit;
  end;
  TmpStr := S;

  I  := 1;
  P1 := pos(OldString, TmpStr);
  C  := P1;
  while (I < N) and (LongInt(C) < Length(TmpStr)) do begin
    Inc(I);
    System.Delete(TmpStr, 1, LongInt(P1) + Length(OldString));
    Inc(C, LongInt(P1) + Length(OldString));
  end;
  Result := S;
  System.Delete(Result, C, Length(OldString));

  C := 0;
  for I := 1 to Replacements do begin
    if (((Length(NewString)) + Length(Result)) < MaxLongInt) then begin
      Inc(C);
      System.Insert(NewString, Result, P1);
      Inc(P1, Length(NewString) + 1);
    end else begin
      Replacements := C;
      Exit;
    end;
  end;
end;


function ReplaceStringAllL(const S, OldString, NewString : String;
                           var Replacements : Cardinal) : String;
var
  I,
  C  : Cardinal;
  P1 : longint;
  Tmp: String;
begin
  if (S = '') or (OldString = '') or (Pos(OldString, S) = 0) then
  begin
    Result := S;
    Replacements := 0;
  end
  else begin
    Tmp := S;
    P1 := AnsiPos(OldString, S);
    if (P1 > 0) then begin
      Result := Copy(Tmp, 1, P1-1);
      C := 0;
      while (P1 > 0) do begin
        for I := 1 to Replacements do begin
          Inc(C);
          Result := Result + NewString;
        end;
        Tmp := Copy(Tmp, P1+Length(OldString), MaxLongInt);
        P1 := AnsiPos(OldString, Tmp);
        if (P1 > 0) then begin
          Result := Result + Copy(Tmp, 1, P1-1);
        end else
          Result := Result + Tmp;
      end;
      Replacements := C;
    end else begin
      Result := S;
      Replacements := 0;
    end;
  end;
end;


function LastWordL(const S, WordDelims, AWord : String;
                   var Position : Cardinal) : Boolean;
var
  TmpStr : String;
  I      : Cardinal;
begin
  if (S = '') or (WordDelims = '') or
     (AWord = '') or (pos(AWord, S) = 0) then begin
    Result := False;
    Position := 0;
    Exit;
  end;

  TmpStr := S;
  I := Length(TmpStr);
  while (pos(TmpStr[I], WordDelims) > 0) do begin
    System.Delete(TmpStr, I, 1);
    I := Length(TmpStr);
  end;

  Position := Length(TmpStr);
  repeat
    while (pos(TmpStr[Position], WordDelims) = 0) and (Position > 1) do
      Dec(Position);
    if (Copy(TmpStr, Position + 1, Length(AWord)) = AWord) then begin
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



function LastWordAbsL(const S, WordDelims : String;
                      var Position : Cardinal) : Boolean;
begin
  if (S = '') or (WordDelims = '') then begin
    Result := False;
    Position := 0;
    Exit;
  end;

{find first non-delimiter character, if any. If not a "one-word wonder"}
  Position := Length(S);
  while (Position > 0) and (pos(S[Position], WordDelims) > 0) do
    Dec(Position);

  if (Position = 0) then begin
    Result := True;
    Position := 1;
    Exit;
  end;

{find next delimiter character}
  while (Position > 0) and (pos(S[Position], WordDelims) = 0) do
    Dec(Position);
  Inc(Position);
  Result := True;
end;



function LastStringL(const S, AString : String;
                     var Position : Cardinal) : Boolean;
var
  TmpStr : String;
  I, C   : Cardinal;
begin
  if (S = '') or (AString = '') or (pos(AString, S) = 0) then begin
    Result := False;
    Position := 0;
    Exit;
  end;

  TmpStr := S;
  C := 0;
  I := pos(AString, TmpStr);
  while (I > 0) do begin
    Inc(C, LongInt(I) + Length(AString));
    System.Delete(TmpStr, 1, LongInt(I) + Length(AString));
    I := pos(AString, TmpStr);
  end;
{Go back the length of AString since the while loop deletes the last instance}
  Dec(C, Length(AString));
  Position := C;
  Result := True;
end;




function RepeatStringL(const RepeatString : String;
                       var Repetitions : Cardinal;
                       MaxLen : Cardinal) : String;
var
  i    : Cardinal;
  Len  : Cardinal;
  ActualReps : Cardinal;
begin
  Result := '';
  if (MaxLen <> 0) and
     (Repetitions <> 0) and
     (RepeatString <> '') then begin
    Len := length(RepeatString);
    ActualReps := MaxLen div Len;
    if (ActualReps > Repetitions) then
      ActualReps := Repetitions
    else
      Repetitions := ActualReps;
    if (ActualReps > 0) then begin
      SetLength(Result, ActualReps * Len);
      for i := 0 to pred(ActualReps) do
        Move(RepeatString[1], Result[i * Len + 1], Len * SizeOf(Char));
    end;
  end;
end;



function StrWithinL(const S, SearchStr : string;
                    Start : Cardinal;
                    var Position : Cardinal) : boolean;
var
  TmpStr : string;
begin
  TmpStr := S;
  if (Start > 1) then
    System.Delete(TmpStr, 1, Start-1);
  Position := pos(SearchStr, TmpStr);
  if (Position > 0) then begin
    Position := Position + Start - 1;
    Result := True;
  end else
    Result := False;
end;


end.
