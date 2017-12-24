{**********************************************************************                   
 Package pl_Astronomy.pkg
 This unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit StUtils;

interface

uses
  LCLIntf, LCLType, LMessages, SysUtils, Classes,
  StConst, StDate;

function SignL(L: longint): integer;
{-return sign of LongInt value}
function SignF(F: extended): integer;
{-return sign of floating point value}

function MinWord(A, B: word): word;
{-Return the smaller of A and B}
function MidWord(W1, W2, W3: word): word;
{-return the middle of three Word values}
function MaxWord(A, B: word): word;
{-Return the greater of A and B}

function MinLong(A, B: longint): longint;
{-Return the smaller of A and B}
function MidLong(L1, L2, L3: longint): longint;
{-return the middle of three LongInt values}
function MaxLong(A, B: longint): longint;
{-Return the greater of A and B}

function MinFloat(F1, F2: extended): extended;
{-return the lesser of two floating point values}
function MidFloat(F1, F2, F3: extended): extended;
{-return the middle of three floating point values}
function MaxFloat(F1, F2: extended): extended;
{-return the greater of two floating point values}

{-Assorted utility routines. }

function MakeInteger16(H, L: byte): smallint;
{-Construct an integer from two bytes}

function MakeWord(H, L: byte): word;
{-Construct a word from two bytes}

function SwapNibble(B: byte): byte;
{-Swap the high and low nibbles of a byte}

procedure SetFlag(var Flags: word; FlagMask: word);
{-Set bit(s) in the parameter Flags. The bits to set are specified in FlagMask}

procedure ClearFlag(var Flags: word; FlagMask: word);
{-Clear bit(s) in the parameter Flags. The bits to clear are specified in Flagmask}

function FlagIsSet(Flags, FlagMask: word): boolean;
{-Return True if the bit specified by FlagMask is set in Flags}

procedure SetByteFlag(var Flags: byte; FlagMask: byte);
{-Set bit(s) in the parameter Flags. The bits to set are specified in FlagMask}

procedure ClearByteFlag(var Flags: byte; FlagMask: byte);
{-Clear bit(s) in the parameter Flags. The bits to clear are specified in FlagMask}

function ByteFlagIsSet(Flags, FlagMask: byte): boolean;
{-Return True if the bit specified by FlagMask is set in the Flags parameter}

procedure SetLongFlag(var Flags: longint; FlagMask: longint);
{-Set bit(s) in the parameter Flags. The bits to set are specified in FlagMask}


procedure ClearLongFlag(var Flags: longint; FlagMask: longint);
{-Clear bit(s) in the parameter Flags. The bits to clear are specified in FlagMask}


function LongFlagIsSet(Flags, FlagMask: longint): boolean;
{-Return True if the bit specified by FlagMask is set in Flags}


function AddWordToPtr(P: Pointer; W: word): Pointer;
{-Add a word to a pointer.}

implementation

const
  ecOutOfMemory = 8;

function MakeInteger16(H, L: byte): smallint;
begin
  word(Result) := (H shl 8) or L;  {!!.02}
end;

function SwapNibble(B: byte): byte;
begin
  Result := (B shr 4) or (B shl 4);
end;

procedure SetFlag(var Flags: word; FlagMask: word);
begin
  Flags := Flags or FlagMask;
end;

procedure ClearFlag(var Flags: word; FlagMask: word);
begin
  Flags := Flags and (not FlagMask);
end;


function FlagIsSet(Flags, FlagMask: word): boolean;
begin
  Result := (FlagMask and Flags <> 0);
end;

procedure SetByteFlag(var Flags: byte; FlagMask: byte);
begin
  Flags := Flags or FlagMask;
end;

procedure ClearByteFlag(var Flags: byte; FlagMask: byte);
begin
  Flags := Flags and (not FlagMask);
end;

function ByteFlagIsSet(Flags, FlagMask: byte): boolean;
begin
  Result := (FlagMask and Flags <> 0);
end;

procedure SetLongFlag(var Flags: longint; FlagMask: longint);
begin
  Flags := Flags or FlagMask;
end;

procedure ClearLongFlag(var Flags: longint; FlagMask: longint);
begin
  Flags := Flags and (not FlagMask);
end;

function LongFlagIsSet(Flags, FlagMask: longint): boolean;
begin
  Result := FlagMask = (Flags and FlagMask);
end;


function AddWordToPtr(P: Pointer; W: word): Pointer;
begin
  Result := Pointer(longint(P) + W);
end;

function MakeWord(H, L: byte): word;
begin
  Result := (word(H) shl 8) or L;
end;

function MinWord(A, B: word): word;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

function MaxWord(A, B: word): word;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

function MinLong(A, B: longint): longint;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

function MaxLong(A, B: longint): longint;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

function SignL(L: longint): integer;
  {-return sign of LongInt value}
begin
  if L < 0 then
    Result := -1
  else if L = 0 then
    Result := 0
  else
    Result := 1;
end;

function SignF(F: extended): integer;
  {-return sign of floating point value}
begin
  if F < 0 then
    Result := -1
  else if F = 0 then
    Result := 0
  else
    Result := 1;
end;

function MidWord(W1, W2, W3: word): word;
  {return the middle of three Word values}
begin
  Result := StUtils.MinWord(StUtils.MinWord(StUtils.MaxWord(W1, W2), StUtils.MaxWord(W2, W3)), StUtils.MaxWord(W1, W3));
end;

function MidLong(L1, L2, L3: longint): longint;
  {return the middle of three LongInt values}
begin
  Result := StUtils.MinLong(StUtils.MinLong(StUtils.MaxLong(L1, L2), StUtils.MaxLong(L2, L3)), StUtils.MaxLong(L1, L3));
end;

function MidFloat(F1, F2, F3: extended): extended;
  {return the middle of three floating point values}
begin
  Result := MinFloat(MinFloat(MaxFloat(F1, F2), MaxFloat(F2, F3)), MaxFloat(F1, F3));
end;

function MinFloat(F1, F2: extended): extended;
  {-return the lesser of two floating point values}
begin
  if F1 <= F2 then
    Result := F1
  else
    Result := F2;
end;

function MaxFloat(F1, F2: extended): extended;
  {-return the greater of two floating point values}
begin
  if F1 > F2 then
    Result := F1
  else
    Result := F2;
end;


end.






