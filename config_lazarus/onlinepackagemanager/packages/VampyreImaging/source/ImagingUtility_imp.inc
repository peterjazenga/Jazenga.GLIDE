{%MainUnit ImagingCore.pas}


var
  FloatFormatSettings: TFormatSettings;

constructor ENotImplemented.Create;
begin
  inherited Create('Not implemented');
end;

procedure FreeAndNil(var Obj);
var
  Temp: TObject;
begin
  Temp := TObject(Obj);
  Pointer(Obj) := nil;
  Temp.Free;
end;

procedure FreeMemNil(var P);
begin
  FreeMem(Pointer(P));
  Pointer(P) := nil;
end;

procedure FreeMem(P: Pointer);
begin
  if P <> nil then
    System.FreeMem(P);
end;

function GetExceptObject: Exception;
begin
  Result := Exception(ExceptObject);
end;

{$IF Defined(MSWINDOWS)}
var
  PerfFrequency: Int64;
  InvPerfFrequency: Extended;

function GetTimeMicroseconds: Int64;
var
  Time: Int64;
begin
  QueryPerformanceCounter(Time);
  Result := Round(1000000 * InvPerfFrequency * Time);
end;
{$ELSEIF Defined(DELPHI)}
function GetTimeMicroseconds: Int64;
var
  Time: TimeVal;
begin
  Posix.SysTime.GetTimeOfDay(Time, nil);
  Result := Int64(Time.tv_sec) * 1000000 + Time.tv_usec;
end;
{$ELSEIF Defined(FPC)}
function GetTimeMicroseconds: Int64;
var
  TimeVal: TTimeVal;
begin
  fpGetTimeOfDay(@TimeVal, nil);
  Result := Int64(TimeVal.tv_sec) * 1000000 + TimeVal.tv_usec;
end;
{$IFEND}

function GetTimeMilliseconds: Int64;
begin
  Result := GetTimeMicroseconds div 1000;
end;

function GetFileExt(const FileName: string): string;
begin
  Result := ExtractFileExt(FileName);
  if Length(Result) > 1 then
    Delete(Result, 1, 1);
end;

function GetAppExe: string;
{$IF Defined(MSWINDOWS)}
var
  FileName: array[0..MAX_PATH] of Char;
begin
  SetString(Result, FileName,
    Windows.GetModuleFileName(MainInstance, FileName, SizeOf(FileName)));
{$ELSEIF Defined(DELPHI)} // Delphi non Win targets
var
  FileName: array[0..1024] of Char;
begin
  SetString(Result, FileName,
    System.GetModuleFileName(MainInstance, FileName, SizeOf(FileName)));
{$ELSE}
begin
  Result := ParamStr(0);
{$IFEND}
end;

function GetAppDir: string;
begin
  Result := ExtractFileDir(GetAppExe);
end;

function GetFileName(const FileName: string): string;
var
  I: Integer;
begin
  I := LastDelimiter('\/' + DriveDelim, FileName);
  Result := Copy(FileName, I + 1, MaxInt);
end;

function GetFileDir(const FileName: string): string;
const
  Delims = '\/' + DriveDelim;
var
  I: Integer;
begin
  I := LastDelimiter(Delims, Filename);
  if (I > 1) and
    ((FileName[I] = Delims[1]) or (FileName[I] = Delims[2])) and
    (not IsDelimiter(Delims, FileName, I - 1)) then Dec(I);
  Result := Copy(FileName, 1, I);
end;

function StrMaskMatch(const Subject, Mask: string; CaseSensitive: Boolean): Boolean;
var
  MaskLen, KeyLen : LongInt;

  function CharMatch(A, B: Char): Boolean;
  begin
    if CaseSensitive then
      Result := A = B
    else
      Result := AnsiUpperCase (A) = AnsiUpperCase (B);
  end;

  function MatchAt(MaskPos, KeyPos: LongInt): Boolean;
  begin
    while (MaskPos <= MaskLen) and (KeyPos <= KeyLen) do
    begin
      case Mask[MaskPos] of
        '?' :
          begin
            Inc(MaskPos);
            Inc(KeyPos);
          end;
        '*' :
          begin
            while (MaskPos <= MaskLen) and (Mask[MaskPos] = '*') do
              Inc(MaskPos);
            if MaskPos > MaskLen then
            begin
              Result := True;
              Exit;
            end;
            repeat
              if MatchAt(MaskPos, KeyPos) then
              begin
                Result := True;
                Exit;
              end;
              Inc(KeyPos);
            until KeyPos > KeyLen;
            Result := False;
            Exit;
          end;
        else
          if not CharMatch(Mask[MaskPos], Subject[KeyPos]) then
          begin
            Result := False;
            Exit;
          end
          else
          begin
            Inc(MaskPos);
            Inc(KeyPos);
          end;
      end;
    end;  

    while (MaskPos <= MaskLen) and (AnsiChar(Mask[MaskPos]) in ['?', '*']) do
      Inc(MaskPos);
    if (MaskPos <= MaskLen) or (KeyPos <= KeyLen) then
    begin
      Result := False;
      Exit;
    end;

    Result := True;
  end;

begin
  MaskLen := Length(Mask);
  KeyLen := Length(Subject);
  if MaskLen = 0 then
  begin
    Result := True;
    Exit;
  end;
  Result := MatchAt(1, 1);
end;

function BuildFileList(Path: string; Attr: LongInt;
  Files: TStrings; Options: TFileListOptions): Boolean;
var
  FileMask: string;
  RootDir: string;
  Folders: TStringList;
  CurrentItem: LongInt;
  Counter: LongInt;
  LocAttr: LongInt;

  procedure BuildFolderList;
  var
    FindInfo: TSearchRec;
    Rslt: LongInt;
  begin
    Counter := Folders.Count - 1;
    CurrentItem := 0;
    while CurrentItem <= Counter do
    begin
      // Searching for subfolders
      Rslt := SysUtils.FindFirst(Folders[CurrentItem] + '*', faDirectory, FindInfo);
      try
        while Rslt = 0 do
        begin
          if (FindInfo.Name <> '.') and (FindInfo.Name <> '..') and
            (FindInfo.Attr and faDirectory = faDirectory) then
            Folders.Add(Folders[CurrentItem] + FindInfo.Name + PathDelim);
          Rslt := SysUtils.FindNext(FindInfo);
        end;
      finally
        SysUtils.FindClose(FindInfo);
      end;
      Counter := Folders.Count - 1;
      Inc(CurrentItem);
    end;
  end;

  procedure FillFileList(CurrentCounter: LongInt);
  var
    FindInfo: TSearchRec;
    Res: LongInt;
    CurrentFolder: string;
  begin
    CurrentFolder := Folders[CurrentCounter];
    Res := SysUtils.FindFirst(CurrentFolder + FileMask, LocAttr, FindInfo);
    if flRelNames in Options then
      CurrentFolder := ExtractRelativePath(RootDir, CurrentFolder);
    try
      while Res = 0 do
      begin
        if (FindInfo.Name <> '.') and (FindInfo.Name <> '..') then
        begin
          if (flFullNames in Options) or (flRelNames in Options) then
            Files.Add(CurrentFolder + FindInfo.Name)
          else
            Files.Add(FindInfo.Name);
        end;
        Res := SysUtils.FindNext(FindInfo);
      end;
    finally
      SysUtils.FindClose(FindInfo);
    end;
  end;

begin
  FileMask := ExtractFileName(Path);
  RootDir := ExtractFilePath(Path);
  Folders := TStringList.Create;
  Folders.Add(RootDir);
  Files.Clear;
{$IFDEF DCC}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}
  if Attr = faAnyFile then
    LocAttr := faSysFile or faHidden or faArchive or faReadOnly
  else
    LocAttr := Attr;
{$IFDEF DCC}
  {$WARN SYMBOL_PLATFORM ON}
{$ENDIF}
  // Here's the recursive search for nested folders
  if flRecursive in Options then
    BuildFolderList;
  if Attr <> faDirectory then
    for Counter := 0 to Folders.Count - 1 do
      FillFileList(Counter)
  else
    Files.AddStrings(Folders);
  Folders.Free;
  Result := True;
end;

function PosEx(const SubStr, S: string; Offset: LongInt = 1): LongInt;
var
  I, X: LongInt;
  Len, LenSubStr: LongInt;
begin
  I := Offset;
  LenSubStr := Length(SubStr);
  Len := Length(S) - LenSubStr + 1;
  while I <= Len do
  begin
    if S[I] = SubStr[1] then
    begin
      X := 1;
      while (X < LenSubStr) and (S[I + X] = SubStr[X + 1]) do
        Inc(X);
      if (X = LenSubStr) then
      begin
        Result := I;
        Exit;
      end;
    end;
    Inc(I);
  end;
  Result := 0;
end;

function PosNoCase(const SubStr, S: string; Offset: LongInt): LongInt;
begin
  Result := PosEx(AnsiLowerCase(SubStr), AnsiLowerCase(S), Offset);
end;

function StrToken(var S: string; Sep: Char): string;
var
  I: LongInt;
begin
  I := Pos(Sep, S);
  if I <> 0 then
  begin
    Result := Copy(S, 1, I - 1);
    Delete(S, 1, I);
  end
  else
  begin
    Result := S;
    S := '';
  end;
end;

function StrTokenEnd(var S: string; Sep: Char): string;
var
  I, J: LongInt;
begin
  J := 0;
  I := Pos(Sep, S);
  while I <> 0 do
  begin
    J := I;
    I := PosEx(Sep, S, J + 1);
  end;
  if J <> 0 then
  begin
    Result := Copy(S, J + 1, MaxInt);
    Delete(S, J, MaxInt);
  end
  else
  begin
    Result := S;
    S := '';
  end;
end;

procedure StrTokensToList(const S: string; Sep: Char; Tokens: TStrings);
var
  Token, Str: string;
begin
  Tokens.Clear;
  Str := S;
  while Str <> '' do
  begin
    Token := StrToken(Str, Sep);
    Tokens.Add(Token);
  end;
end;

function IntToStrFmt(const I: Int64): string;
begin
  Result := Format('%.0n', [I * 1.0]);
end;

function FloatToStrFmt(const F: Double; Precision: Integer): string;
begin
  Result := Format('%.' + IntToStr(Precision) + 'n', [F]);
end;

function GetFormatSettingsForFloats: TFormatSettings;
begin
  Result := FloatFormatSettings;
end;

function ContainsAnySubStr(const S: string; const SubStrs: array of string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to High(SubStrs) do
  begin
    Result := Pos(SubStrs[I], S) > 0;
    if Result then
      Exit;
  end;
end;

function SubString(const S: string; IdxStart, IdxEnd: Integer): string;
begin
  Result := Copy(S, IdxStart, IdxEnd - IdxStart);
end;

function ClampInt(Number: LongInt; Min, Max: LongInt): LongInt;
begin
  Result := Number;
  if Result < Min then
    Result := Min
  else if Result > Max then
    Result := Max;
end;

function ClampFloat(Number: Single; Min, Max: Single): Single;
begin
  Result := Number;
  if Result < Min then
    Result := Min
  else if Result > Max then
    Result := Max;
end;

function ClampToByte(Value: LongInt): LongInt;
begin
  Result := Value;
  if Result > 255 then
    Result := 255
  else if Result < 0 then
    Result := 0;
end;

function ClampToWord(Value: LongInt): LongInt;
begin
  Result := Value;
  if Result > 65535 then
    Result := 65535
  else if Result < 0 then
    Result := 0;
end;

function IsPow2(Num: LongInt): Boolean;
begin
  Result := (Num and -Num) = Num;
end;

function NextPow2(Num: LongInt): LongInt;
begin
  Result := Num and -Num;
  while Result < Num do
    Result := Result shl 1;
end;

function Pow2Int(Exponent: LongInt): LongInt;
begin
  Result := 1 shl Exponent;
end;

function Power(const Base, Exponent: Single): Single;
begin
  if Exponent = 0.0 then
    Result := 1.0
  else if (Base = 0.0) and (Exponent > 0.0) then
    Result := 0.0
  else
    Result := Exp(Exponent * Ln(Base));
end;

function Log2Int(X: LongInt): LongInt;
begin
  case X of
    1: Result := 0;
    2: Result := 1;
    4: Result := 2;
    8: Result := 3;
    16: Result := 4;
    32: Result := 5;
    64: Result := 6;
    128: Result := 7;
    256: Result := 8;
    512: Result := 9;
    1024: Result := 10;
    2048: Result := 11;
    4096: Result := 12;
    8192: Result := 13;
    16384: Result := 14;
    32768: Result := 15;
    65536: Result := 16;
    131072: Result := 17;
    262144: Result := 18;
    524288: Result := 19;
    1048576: Result := 20;
    2097152: Result := 21;
    4194304: Result := 22;
    8388608: Result := 23;
    16777216: Result := 24;
    33554432: Result := 25;
    67108864: Result := 26;
    134217728: Result := 27;
    268435456: Result := 28;
    536870912: Result := 29;
    1073741824: Result := 30;
  else
    Result := -1;
  end;
end;

function Log2(X: Single): Single;
{$IFDEF USE_ASM}
asm
  FLD1
  FLD     X
  FYL2X
  FWAIT
end;
{$ELSE}
const
  Ln2: Single = 0.6931471;
begin
  Result := Ln(X) / Ln2;
end;
{$ENDIF}

function Log10(X: Single): Single;
{$IFDEF USE_ASM}
asm
  FLDLG2
  FLD     X
  FYL2X
  FWAIT
end;
{$ELSE}
const
  Ln10: Single = 2.30258509299405;
begin
  Result := Ln(X) / Ln10;
end;
{$ENDIF}

function Floor(Value: Single): LongInt;
begin
  Result := Trunc(Value);
  if Frac(Value) < 0.0 then
    Dec(Result);
end;

function Ceil(Value: Single): LongInt;
begin
  Result := Trunc(Value);
  if Frac(Value) > 0.0 then
    Inc(Result);
end;

procedure Switch(var Value: Boolean);
begin
  Value := not Value;
end;

function Iff(Condition: Boolean; TruePart, FalsePart: LongInt): LongInt;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

function IffUnsigned(Condition: Boolean; TruePart, FalsePart: LongWord): LongWord;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

function Iff(Condition, TruePart, FalsePart: Boolean): Boolean;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

function Iff(Condition: Boolean; const TruePart, FalsePart: string): string;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

function Iff(Condition: Boolean; TruePart, FalsePart: Char): Char;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

function Iff(Condition: Boolean; TruePart, FalsePart: Pointer): Pointer;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

function Iff(Condition: Boolean; const TruePart, FalsePart: Int64): Int64;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

function IffFloat(Condition: Boolean; TruePart, FalsePart: Single): Single;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

procedure SwapValues(var A, B: Boolean);
var
  Tmp: Boolean;
begin
  Tmp := A;
  A := B;
  B := Tmp;
end;

procedure SwapValues(var A, B: Byte);
var
  Tmp: Byte;
begin
  Tmp := A;
  A := B;
  B := Tmp;
end;

procedure SwapValues(var A, B: Word);
var
  Tmp: Word;
begin
  Tmp := A;
  A := B;
  B := Tmp;
end;

procedure SwapValues(var A, B: LongInt);
var
  Tmp: LongInt;
begin
  Tmp := A;
  A := B;
  B := Tmp;
end;

procedure SwapValues(var A, B: Single);
var
  Tmp: Single;
begin
  Tmp := A;
  A := B;
  B := Tmp;
end;

procedure SwapMin(var Min, Max: LongInt);
var
  Tmp: LongInt;
begin
  if Min > Max then
  begin
    Tmp := Min;
    Min := Max;
    Max := Tmp;
  end;
end;

function Min(A, B: LongInt): LongInt;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

function MinFloat(A, B: Single): Single;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

function Max(A, B: LongInt): LongInt;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

function MaxFloat(A, B: Single): Single;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

function MulDiv(Number, Numerator, Denominator: Word): Word;
{$IF Defined(USE_ASM) and (not Defined(USE_INLINE))}
asm
         MUL DX
         DIV CX
end;
{$ELSE}
begin
  Result := Number * Numerator div Denominator;
end;
{$IFEND}

function IsLittleEndian: Boolean;
var
  W: Word;
begin
  W := $00FF;
  Result := PByte(@W)^ = $FF;
end;

function SwapEndianWord(Value: Word): Word;
{$IF Defined(USE_ASM) and (not Defined(USE_INLINE))}
asm
  XCHG   AH, AL
end;
{$ELSE}
begin
  TWordRec(Result).Low := TWordRec(Value).High;
  TWordRec(Result).High := TWordRec(Value).Low;
end;
{$IFEND}

procedure SwapEndianWord(P: PWordArray; Count: LongInt);
{$IFDEF USE_ASM}
asm
@Loop:
  MOV    CX, [EAX]
  XCHG   CH, CL
  MOV    [EAX], CX
  ADD    EAX, 2
  DEC    EDX
  JNZ    @Loop
end;
{$ELSE}
var
  I: LongInt;
  Temp: Word;
begin
  for I := 0 to Count - 1 do
  begin
    Temp := P[I];
    TWordRec(P[I]).Low := TWordRec(Temp).High;
    TWordRec(P[I]).High := TWordRec(Temp).Low;
  end;
end;
{$ENDIF}

function SwapEndianLongWord(Value: LongWord): LongWord;
{$IF Defined(USE_ASM) and (not Defined(USE_INLINE))}
asm
  BSWAP   EAX
end;
{$ELSE}
begin
  TLongWordRec(Result).Bytes[0] := TLongWordRec(Value).Bytes[3];
  TLongWordRec(Result).Bytes[1] := TLongWordRec(Value).Bytes[2];
  TLongWordRec(Result).Bytes[2] := TLongWordRec(Value).Bytes[1];
  TLongWordRec(Result).Bytes[3] := TLongWordRec(Value).Bytes[0];
end;
{$IFEND}

procedure SwapEndianLongWord(P: PLongWord; Count: LongInt);
{$IFDEF USE_ASM}
asm
@Loop:
  MOV    ECX, [EAX]
  BSWAP  ECX
  MOV    [EAX], ECX
  ADD    EAX, 4
  DEC    EDX
  JNZ    @Loop
end;
{$ELSE}
var
  I: LongInt;
  Temp: LongWord;
begin
  for I := 0 to Count - 1 do
  begin
    Temp := PLongWordArray(P)[I];
    TLongWordRec(PLongWordArray(P)[I]).Bytes[0] := TLongWordRec(Temp).Bytes[3];
    TLongWordRec(PLongWordArray(P)[I]).Bytes[1] := TLongWordRec(Temp).Bytes[2];
    TLongWordRec(PLongWordArray(P)[I]).Bytes[2] := TLongWordRec(Temp).Bytes[1];
    TLongWordRec(PLongWordArray(P)[I]).Bytes[3] := TLongWordRec(Temp).Bytes[0];
  end;
end;
{$ENDIF}

type
  TCrcTable = array[Byte] of LongWord;
var
  CrcTable: TCrcTable;

procedure InitCrcTable;
const
  Polynom = $EDB88320;
var
  I, J: LongInt;
  C: LongWord;
begin
  for I := 0 to 255 do
  begin
    C := I;
    for J := 0 to 7 do
    begin
      if (C and $01) <> 0 then
        C := Polynom xor (C shr 1)
      else
        C := C shr 1;
    end;
    CrcTable[I] := C;
  end;
end;

procedure CalcCrc32(var Crc: LongWord; Data: Pointer; Size: LongInt);
var
  I: LongInt;
  B: PByte;
begin
  B := Data;
  for I := 0 to Size - 1 do
  begin
    Crc := (Crc shr 8) xor CrcTable[B^ xor Byte(Crc)];
    Inc(B);
  end
end;

procedure FillMemoryByte(Data: Pointer; Size: LongInt; Value: Byte);
{$IFDEF USE_ASM}
asm
  PUSH   EDI
  MOV    EDI, EAX
  MOV    EAX, ECX
  MOV    AH, AL
  MOV    CX, AX
  SHL    EAX, 16
  MOV    AX, CX
  MOV    ECX, EDX
  SAR    ECX, 2
  JS     @Exit
  REP    STOSD
  MOV    ECX, EDX
  AND    ECX, 3
  REP    STOSB
  POP    EDI
@Exit:
end;
{$ELSE}
begin
  FillChar(Data^, Size, Value);
end;
{$ENDIF}

procedure FillMemoryWord(Data: Pointer; Size: LongInt; Value: Word);
{$IFDEF USE_ASM}
asm
  PUSH   EDI
  PUSH   EBX
  MOV    EBX, EDX
  MOV    EDI, EAX
  MOV    EAX, ECX
  MOV    CX, AX
  SHL    EAX, 16
  MOV    AX, CX
  MOV    ECX, EDX
  SHR    ECX, 2
  JZ     @Word
  REP    STOSD
@Word:
  MOV    ECX, EBX
  AND    ECX, 2
  JZ     @Byte
  MOV    [EDI], AX
  ADD    EDI, 2
@Byte:
  MOV    ECX, EBX
  AND    ECX, 1
  JZ     @Exit
  MOV    [EDI], AL
@Exit:
  POP    EBX
  POP    EDI
end;
{$ELSE}
var
  I, V: LongWord;
begin
  V := Value * $10000 + Value;
  for I := 0 to Size div 4 - 1 do
    PLongWordArray(Data)[I] := V;
  case Size mod 4 of
    1: PByteArray(Data)[Size - 1] := Lo(Value);
    2: PWordArray(Data)[Size div 2] := Value;
    3:
      begin
        PWordArray(Data)[Size  div 2 - 1] := Value;
        PByteArray(Data)[Size - 1] := Lo(Value);
      end;
  end;
end;
{$ENDIF}

procedure FillMemoryLongWord(Data: Pointer; Size: LongInt; Value: LongWord);
{$IFDEF USE_ASM}
asm
  PUSH   EDI
  PUSH   EBX
  MOV    EBX, EDX
  MOV    EDI, EAX
  MOV    EAX, ECX
  MOV    ECX, EDX
  SHR    ECX, 2
  JZ     @Word
  REP    STOSD
@Word:
  MOV    ECX, EBX
  AND    ECX, 2
  JZ     @Byte
  MOV    [EDI], AX
  ADD    EDI, 2
@Byte:
  MOV    ECX, EBX
  AND    ECX, 1
  JZ     @Exit
  MOV    [EDI], AL
@Exit:
  POP    EBX
  POP    EDI
end;
{$ELSE}
var
  I: LongInt;
begin
  for I := 0 to Size div 4 - 1 do
    PLongWordArray(Data)[I] := Value;
  case Size mod 4 of
    1: PByteArray(Data)[Size - 1] := TLongWordRec(Value).Bytes[0];
    2: PWordArray(Data)[Size div 2] := TLongWordRec(Value).Words[0];
    3:
      begin
        PWordArray(Data)[Size div 2 - 1] := TLongWordRec(Value).Words[0];
        PByteArray(Data)[Size - 1] := TLongWordRec(Value).Bytes[0];
      end;
  end;
end;
{$ENDIF}

procedure ZeroMemory(Data: Pointer; Size: Integer);
begin
  FillMemoryByte(Data, Size, 0);
end;

function GetNumMipMapLevels(Width, Height: LongInt): LongInt;
begin
  Result := 0;
  if (Width > 0) and (Height > 0) then
  begin
    Result := 1;
    while (Width <> 1) or (Height <> 1) do
    begin
      Width := Width div 2;
      Height := Height div 2;
      if Width < 1 then Width := 1;
      if Height < 1 then Height := 1;
      Inc(Result);
    end;
  end;
end;

function GetVolumeLevelCount(Depth, MipMaps: LongInt): LongInt;
var
  I: LongInt;
begin
  Result := Depth;
  for I := 1 to MipMaps - 1 do
    Inc(Result, ClampInt(Depth shr I, 1, Depth));
end;

function BoundsToRect(X, Y, Width, Height: LongInt): TRect;
begin
  Result.Left := X;
  Result.Top := Y;
  Result.Right := X + Width;
  Result.Bottom := Y + Height;
end;

function BoundsToRect(const R: TRect): TRect;
begin
  Result.Left := R.Left;
  Result.Top := R.Top;
  Result.Right := R.Left + R.Right;
  Result.Bottom := R.Top + R.Bottom;
end;

function RectToBounds(const R: TRect): TRect;
begin
  Result.Left := R.Left;
  Result.Top := R.Top;
  Result.Right := R.Right - R.Left;
  Result.Bottom := R.Bottom - R.Top;
end;

procedure ClipRectBounds(var X, Y, Width, Height: LongInt; const Clip: TRect);

  procedure ClipDim(var AStart, ALength: LongInt; ClipMin, ClipMax: LongInt);
  begin
    if AStart < ClipMin then
    begin
      ALength := ALength - (ClipMin - AStart);
      AStart := ClipMin;
    end;
    if AStart + ALength > ClipMax then ALength := Max(0, ClipMax - AStart);
  end;

begin
  ClipDim(X, Width, Clip.Left, Clip.Right);
  ClipDim(Y, Height, Clip.Top, Clip.Bottom);
end;

procedure ClipCopyBounds(var SrcX, SrcY, Width, Height, DstX, DstY: LongInt; SrcImageWidth, SrcImageHeight: LongInt; const DstClip: TRect);

  procedure ClipDim(var SrcPos, DstPos, Size: LongInt; SrcClipMax,
    DstClipMin, DstClipMax: LongInt);
  var
    OldDstPos: LongInt;
    Diff: LongInt;
  begin
    OldDstPos := Iff(DstPos < 0, DstPos, 0);
    if DstPos < DstClipMin then
    begin
      Diff := DstClipMin - DstPos;
      Size := Size - Diff;
      SrcPos := SrcPos + Diff;
      DstPos := DstClipMin;
    end;
    if SrcPos < 0 then
    begin
      Size := Size + SrcPos - OldDstPos;
      DstPos := DstPos - SrcPos + OldDstPos;
      SrcPos := 0;
    end;
    if SrcPos + Size > SrcClipMax then Size := SrcClipMax - SrcPos;
    if DstPos + Size > DstClipMax then Size := DstClipMax - DstPos;
  end;

begin
  ClipDim(SrcX, DstX, Width, SrcImageWidth, DstClip.Left, DstClip.Right);
  ClipDim(SrcY, DstY, Height, SrcImageHeight, DstClip.Top, DstClip.Bottom);
end;

procedure ClipStretchBounds(var SrcX, SrcY, SrcWidth, SrcHeight, DstX, DstY,
  DstWidth, DstHeight: LongInt; SrcImageWidth, SrcImageHeight: LongInt; const DstClip: TRect);

  procedure ClipDim(var SrcPos, DstPos, SrcSize, DstSize: LongInt; SrcClipMax,
    DstClipMin, DstClipMax: LongInt);
  var
    OldSize: LongInt;
    Diff: LongInt;
    Scale: Single;
  begin
    Scale := DstSize / SrcSize;
    if DstPos < DstClipMin then
    begin
      Diff := DstClipMin - DstPos;
      DstSize := DstSize - Diff;
      SrcPos := SrcPos + Round(Diff / Scale);
      SrcSize := SrcSize - Round(Diff / Scale);
      DstPos := DstClipMin;
    end;
    if SrcPos < 0 then
    begin
      SrcSize := SrcSize + SrcPos;
      DstPos := DstPos - Round(SrcPos * Scale);
      DstSize := DstSize + Round(SrcPos * Scale);
      SrcPos := 0;
    end;
    if SrcPos + SrcSize > SrcClipMax then
    begin
      OldSize := SrcSize;
      SrcSize := SrcClipMax - SrcPos;
      DstSize := Round(DstSize * (SrcSize / OldSize));
    end;
    if DstPos + DstSize > DstClipMax then
    begin
      OldSize := DstSize;
      DstSize := DstClipMax - DstPos;
      SrcSize := Round(SrcSize * (DstSize / OldSize));
    end;
  end;

begin
  ClipDim(SrcX, DstX, SrcWidth, DstWidth, SrcImageWidth, DstClip.Left, DstClip.Right);
  ClipDim(SrcY, DstY, SrcHeight, DstHeight, SrcImageHeight, DstClip.Top, DstClip.Bottom);
end;

function ScaleRectToRect(const SourceRect, TargetRect: TRect): TRect;
var
  SourceWidth: LongInt;
  SourceHeight: LongInt;
  TargetWidth: LongInt;
  TargetHeight: LongInt;
  ScaledWidth: LongInt;
  ScaledHeight: LongInt;
begin
  SourceWidth := SourceRect.Right - SourceRect.Left;
  SourceHeight := SourceRect.Bottom - SourceRect.Top;
  TargetWidth := TargetRect.Right - TargetRect.Left;
  TargetHeight := TargetRect.Bottom - TargetRect.Top;

  if SourceWidth * TargetHeight < SourceHeight * TargetWidth then
  begin
    ScaledWidth := (SourceWidth * TargetHeight) div SourceHeight;
    Result := BoundsToRect(TargetRect.Left + ((TargetWidth - ScaledWidth) div 2),
      TargetRect.Top, ScaledWidth, TargetHeight);
  end
  else
  begin
    ScaledHeight := (SourceHeight * TargetWidth) div SourceWidth;
    Result := BoundsToRect(TargetRect.Left, TargetRect.Top + ((TargetHeight - ScaledHeight) div 2),
      TargetWidth, ScaledHeight);
  end;
end;

function ScaleSizeToFit(const CurrentSize, MaxSize: Types.TSize): Types.TSize;
var
  SR, TR, ScaledRect: TRect;
begin
  SR := Types.Rect(0, 0, CurrentSize.CX, CurrentSize.CY);
  TR := Types.Rect(0, 0, MaxSize.CX, MaxSize.CY);
  ScaledRect := ScaleRectToRect(SR, TR);
  Result.CX := ScaledRect.Right - ScaledRect.Left;
  Result.CY := ScaledRect.Bottom - ScaledRect.Top;
end;

function RectWidth(const Rect: TRect): Integer;
begin
  Result := Rect.Right - Rect.Left;
end;

function RectHeight(const Rect: TRect): Integer;
begin
  Result := Rect.Bottom - Rect.Top;
end;

function RectInRect(const R1, R2: TRect): Boolean;
begin
  Result:=
    (R1.Left >= R2.Left) and
    (R1.Top >= R2.Top) and
    (R1.Right <= R2.Right) and
    (R1.Bottom <= R2.Bottom);
end;

function RectIntersects(const R1, R2: TRect): Boolean;
begin
  Result :=
    not (R1.Left > R2.Right) and
    not (R1.Top > R2.Bottom) and
    not (R1.Right < R2.Left) and
    not (R1.Bottom < R2.Top);
end;

function PixelSizeToDpi(SizeInMicroMeters: Single): Single;
begin
  Result := 25400 / SizeInMicroMeters;
end;

function DpiToPixelSize(Dpi: Single): Single;
begin
  Result := 1e03 / (Dpi / 25.4);
end;

function FloatRect(ALeft, ATop, ARight, ABottom: Single): TFloatRect;
begin
  with Result do
  begin
    Left := ALeft;
    Top := ATop;
    Right := ARight;
    Bottom := ABottom;
  end;
end;

function FloatRectWidth(const R: TFloatRect): Single;
begin
  Result := R.Right - R.Left;
end;

function FloatRectHeight(const R: TFloatRect): Single;
begin
  Result := R.Bottom - R.Top;
end;

function FormatExceptMsg(const Msg: string; const Args: array of const): string;
begin
  Result := Format(Msg + SLineBreak + 'Message: ' + GetExceptObject.Message, Args);
end;

procedure DebugMsg(const Msg: string; const Args: array of const);
var
  FmtMsg: string;
begin
  FmtMsg := Format(Msg, Args);
{$IFDEF MSWINDOWS}
  if IsConsole then
    WriteLn('DebugMsg: ' + FmtMsg)
  else
    MessageBox(GetActiveWindow, PChar(FmtMsg), 'DebugMsg', MB_OK);
{$ENDIF}
{$IFDEF UNIX}
  WriteLn('DebugMsg: ' + FmtMsg);
{$ENDIF}
{$IFDEF MSDOS}
  WriteLn('DebugMsg: ' + FmtMsg);
{$ENDIF}
end;
