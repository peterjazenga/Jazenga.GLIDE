{%MainUnit ImagingCore.pas}


const
  STrue = 'True';
  SFalse = 'False';

type
  TByteArray = array[0..MaxInt - 1] of Byte;
  PByteArray = ^TByteArray;
  TWordArray = array[0..MaxInt div 2 - 1] of Word;
  PWordArray = ^TWordArray;
  TLongIntArray = array[0..MaxInt div 4 - 1] of LongInt;
  PLongIntArray = ^TLongIntArray;
  TLongWordArray = array[0..MaxInt div 4 - 1] of LongWord;
  PLongWordArray = ^TLongWordArray;
  TInt64Array = array[0..MaxInt div 8 - 1] of Int64;
  PInt64Array = ^TInt64Array;
  TSingleArray = array[0..MaxInt div 4 - 1] of Single;
  PSingleArray = ^TSingleArray;
  TBooleanArray = array[0..MaxInt - 1] of Boolean;
  PBooleanArray = ^TBooleanArray;

  TDynByteArray = array of Byte;
  TDynIntegerArray = array of Integer;
  TDynBooleanArray = array of Boolean;
  TDynStringArray = array of string;
  
  TWordRec = packed record
    case Integer of
      0: (WordValue: Word);
      1: (Low, High: Byte);
  end;
  PWordRec = ^TWordRec;
  TWordRecArray = array[0..MaxInt div 2 - 1] of TWordRec;
  PWordRecArray = ^TWordRecArray;

  TLongWordRec = packed record
    case Integer of
      0: (LongWordValue: LongWord);
      1: (Low, High: Word);
      { Array variants - Index 0 means lowest significant byte (word, ...).}
      2: (Words: array[0..1] of Word);
      3: (Bytes: array[0..3] of Byte);
  end;
  PLongWordRec = ^TLongWordRec;
  TLongWordRecArray = array[0..MaxInt div 4 - 1] of TLongWordRec;
  PLongWordRecArray = ^TLongWordRecArray;

  TInt64Rec = packed record
    case Integer of
      0: (Int64Value: Int64);
      1: (Low, High: LongWord);
      { Array variants - Index 0 means lowest significant byte (word, ...).}
      2: (Words: array[0..3] of Word);
      3: (Bytes: array[0..7] of Byte);
  end;
  PInt64Rec = ^TInt64Rec;
  TInt64RecArray = array[0..MaxInt div 8 - 1] of TInt64Rec;
  PInt64RecArray = ^TInt64RecArray;

  TFloatHelper = record
    Data: Int64;
    case Integer of
      0: (Data64: Int64);
      1: (Data32: LongWord);
  end;
  PFloatHelper = ^TFloatHelper;

  TFloatRect = record
    Left, Top, Right, Bottom: Single;
  end;

  TChar2 = array[0..1] of AnsiChar;
  TChar3 = array[0..2] of AnsiChar;
  TChar4 = array[0..3] of AnsiChar;
  TChar8 = array[0..7] of AnsiChar;
  TChar16 = array[0..15] of AnsiChar;
  TAnsiCharSet = set of AnsiChar;

  ENotImplemented = class(Exception)
  public
    constructor Create;
  end;

  { Options for BuildFileList function:
    flFullNames - file names in result will have full path names
                (ExtractFileDir(Path) + FileName)
    flRelNames  - file names in result will have names relative to
                ExtractFileDir(Path) dir
    flRecursive - adds files in subdirectories found in Path.}
  TFileListOption = (flFullNames, flRelNames, flRecursive);
  TFileListOptions = set of TFileListOption;


{ Frees class instance and sets its reference to nil.}
procedure FreeAndNil(var Obj);
{ Frees pointer and sets it to nil.}
procedure FreeMemNil(var P); {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Replacement of standard System.FreeMem procedure which checks if P is nil
  (this is only needed for Free Pascal, Delphi makes checks in its FreeMem).}
procedure FreeMem(P: Pointer); {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Returns current exception object. Do not call outside exception handler.}
function GetExceptObject: Exception; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Returns time value with microsecond resolution.}
function GetTimeMicroseconds: Int64;
{ Returns time value with milisecond resolution.}
function GetTimeMilliseconds: Int64;

{ Returns file extension (without "." dot)}
function GetFileExt(const FileName: string): string;
{ Returns file name of application's executable.}
function GetAppExe: string;
{ Returns directory where application's exceutable is located without
  path delimiter at the end.}
function GetAppDir: string;
{ Works like SysUtils.ExtractFileName but supports '/' and '\' dir delimiters
  at the same time (whereas ExtractFileName supports on default delimiter on current platform).}
function GetFileName(const FileName: string): string;
{ Works like SysUtils.ExtractFileDir but supports '/' and '\' dir delimiters
  at the same time (whereas ExtractFileDir supports on default delimiter on current platform).}
function GetFileDir(const FileName: string): string;
{ Returns True if Subject matches given Mask with optional case sensitivity.
  Mask can contain ? and * special characters: ? matches
  one character, * matches zero or more characters.}
function StrMaskMatch(const Subject, Mask: string; CaseSensitive: Boolean = False): Boolean;
{ This function fills Files string list with names of files found
  with FindFirst/FindNext functions (See details on Path/Atrr here).
  - BuildFileList('c:\*.*', faAnyFile, List, [flRecursive]) returns
    list of all files (only name.ext - no path) on C drive
  - BuildFileList('d:\*.*', faDirectory, List, [flFullNames]) returns
    list of all directories (d:\dirxxx) in root of D drive.}
function BuildFileList(Path: string; Attr: LongInt; Files: TStrings;
  Options: TFileListOptions = []): Boolean;
{ Similar to RTL's Pos function but with optional Offset where search will start.
  This function is in the RTL StrUtils unit but }
function PosEx(const SubStr, S: string; Offset: LongInt = 1): LongInt;
{ Same as PosEx but without case sensitivity.}
function PosNoCase(const SubStr, S: string; Offset: LongInt = 1): LongInt; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Returns a sub-string from S which is followed by
  Sep separator and deletes the sub-string from S including the separator.}
function StrToken(var S: string; Sep: Char): string;
{ Same as StrToken but searches from the end of S string.}
function StrTokenEnd(var S: string; Sep: Char): string;
{ Fills instance of TStrings with tokens from string S where tokens are separated by
  one of Seps characters.}
procedure StrTokensToList(const S: string; Sep: Char; Tokens: TStrings);
{ Returns string representation of integer number (with digit grouping).
  Uses current locale.}
function IntToStrFmt(const I: Int64): string; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Returns string representation of float number (with digit grouping).
  Uses current locale.}
function FloatToStrFmt(const F: Double; Precision: Integer = 2): string; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Returns format settings for parsing floats (dot as decimal separator).
  Useful when fomatting/parsing floats etc.}
function GetFormatSettingsForFloats: TFormatSettings;
{ Returns True if S contains at least one of the substrings in SubStrs array. Case sensitive.}
function ContainsAnySubStr(const S: string; const SubStrs: array of string): Boolean;
{ Extracts substring starting at IdxStart ending at IdxEnd.
  S[IdxEnd] is not included in the result.}
function SubString(const S: string; IdxStart, IdxEnd: Integer): string; {$IFDEF USE_INLINE}inline;{$ENDIF}

{ Clamps integer value to range <Min, Max>}
function ClampInt(Number: LongInt; Min, Max: LongInt): LongInt; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Clamps float value to range <Min, Max>}
function ClampFloat(Number: Single; Min, Max: Single): Single; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Clamps integer value to Byte boundaries.}
function ClampToByte(Value: LongInt): LongInt; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Clamps integer value to Word boundaries.}
function ClampToWord(Value: LongInt): LongInt; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Returns True if Num is power of 2.}
function IsPow2(Num: LongInt): Boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Returns next power of 2 greater than or equal to Num
  (if Num itself is power of 2 then it retuns Num).}
function NextPow2(Num: LongInt): LongInt; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Raises 2 to the given integer power (in range [0, 30]).}
function Pow2Int(Exponent: LongInt): LongInt; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Raises Base to any power.}
function Power(const Base, Exponent: Single): Single;
{ Returns log base 2 of integer X (max 2^30) or -1 if X is not power of 2.}
function Log2Int(X: LongInt): LongInt; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Returns log base 2 of X.}
function Log2(X: Single): Single;
{ Returns log base 10 of X.}
function Log10(X: Single): Single;
{ Returns largest integer <= Val (for 5.9 returns 5).}
function Floor(Value: Single): LongInt; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Returns smallest integer >= Val (for 5.1 returns 6).}
function Ceil(Value: Single): LongInt; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Returns lesser of two integer numbers.}
function Min(A, B: LongInt): LongInt; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Returns lesser of two float numbers.}
function MinFloat(A, B: Single): Single; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Returns greater of two integer numbers.}
function Max(A, B: LongInt): LongInt; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Returns greater of two float numbers.}
function MaxFloat(A, B: Single): Single; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Returns result from multiplying Number by Numerator and then dividing by Denominator.
  Denominator must be greater than 0.}
function MulDiv(Number, Numerator, Denominator: Word): Word; {$IFDEF USE_INLINE}inline;{$ENDIF}

{ Switches Boolean value.}
procedure Switch(var Value: Boolean); {$IFDEF USE_INLINE}inline;{$ENDIF}
{ If Condition is True then TruePart is retured, otherwise
  FalsePart is returned.}
function Iff(Condition: Boolean; TruePart, FalsePart: LongInt): LongInt; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ If Condition is True then TruePart is retured, otherwise
  FalsePart is returned.}
function IffUnsigned(Condition: Boolean; TruePart, FalsePart: LongWord): LongWord; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ If Condition is True then TruePart is retured, otherwise
  FalsePart is returned.}
function Iff(Condition, TruePart, FalsePart: Boolean): Boolean; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ If Condition is True then TruePart is retured, otherwise
  FalsePart is returned.}
function Iff(Condition: Boolean; const TruePart, FalsePart: string): string; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ If Condition is True then TruePart is retured, otherwise
  FalsePart is returned.}
function Iff(Condition: Boolean; TruePart, FalsePart: Char): Char; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ If Condition is True then TruePart is retured, otherwise
  FalsePart is returned.}
function Iff(Condition: Boolean; TruePart, FalsePart: Pointer): Pointer; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ If Condition is True then TruePart is retured, otherwise
  FalsePart is returned.}
function Iff(Condition: Boolean; const TruePart, FalsePart: Int64): Int64; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ If Condition is True then TruePart is retured, otherwise
  FalsePart is returned.}
function IffFloat(Condition: Boolean; TruePart, FalsePart: Single): Single; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Swaps two Boolean values}
procedure SwapValues(var A, B: Boolean); overload;
{ Swaps two Byte values}
procedure SwapValues(var A, B: Byte); overload;
{ Swaps two Word values}
procedure SwapValues(var A, B: Word); overload;
{ Swaps two LongInt values}
procedure SwapValues(var A, B: LongInt); overload;
{ Swaps two Single values}
procedure SwapValues(var A, B: Single); overload;
{ Swaps two LongInt values if necessary to ensure that Min <= Max.}
procedure SwapMin(var Min, Max: LongInt); {$IFDEF USE_INLINE}inline;{$ENDIF}
{ This function returns True if running on little endian machine.}
function IsLittleEndian: Boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Swaps byte order of Word value.}
function SwapEndianWord(Value: Word): Word; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Swaps byte order of multiple Word values.}
procedure SwapEndianWord(P: PWordArray; Count: LongInt); overload;
{ Swaps byte order of LongWord value.}
function SwapEndianLongWord(Value: LongWord): LongWord; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Swaps byte order of multiple LongWord values.}
procedure SwapEndianLongWord(P: PLongWord; Count: LongInt); overload;

{ Calculates CRC32 for the given data.}
procedure CalcCrc32(var Crc: LongWord; Data: Pointer; Size: LongInt);
{ Fills given memory with given Byte value. Size is size of buffer in bytes.}
procedure FillMemoryByte(Data: Pointer; Size: LongInt; Value: Byte);
{ Fills given memory with given Word value. Size is size of buffer in bytes.}
procedure FillMemoryWord(Data: Pointer; Size: LongInt; Value: Word);
{ Fills given memory with given LongWord value. Size is size of buffer in bytes.}
procedure FillMemoryLongWord(Data: Pointer; Size: LongInt; Value: LongWord);
{ Fills given memory zeroes.}
{$EXTERNALSYM ZeroMemory} // Conflicts with WinAPI ZeroMemory in C++ Builder
procedure ZeroMemory(Data: Pointer; Size: Integer); {$IFDEF USE_INLINE}inline;{$ENDIF}

{ Returns how many mipmap levels can be created for image of given size.}
function GetNumMipMapLevels(Width, Height: LongInt): LongInt;
{ Returns total number of levels of volume texture with given depth and
  mipmap count (this is not depth * mipmaps!).}
function GetVolumeLevelCount(Depth, MipMaps: LongInt): LongInt;
{ Returns rectangle (X, Y, X + Width, Y + Height).}
function BoundsToRect(X, Y, Width, Height: LongInt): TRect; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Returns rectangle (R.Left, R.Top, R.Left + R.Right, R.Top + R.Bottom).}
function BoundsToRect(const R: TRect): TRect; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Returns rectangle (R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top).}
function RectToBounds(const R: TRect): TRect; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Clips given bounds to Clip rectangle.}
procedure ClipRectBounds(var X, Y, Width, Height: LongInt; const Clip: TRect);
{ Clips given source bounds and dest position. It is used by various CopyRect
  functions that copy rect from one image to another. It handles clipping the same way
  as Win32 BitBlt function. }
procedure ClipCopyBounds(var SrcX, SrcY, Width, Height, DstX, DstY: LongInt;
  SrcImageWidth, SrcImageHeight: LongInt; const DstClip: TRect);
{ Clips given source bounds and dest bounds. It is used by various StretchRect
  functions that stretch rectangle of pixels from one image to another.
  It handles clipping the same way as Win32 StretchBlt function. }
procedure ClipStretchBounds(var SrcX, SrcY, SrcWidth, SrcHeight, DstX, DstY,
  DstWidth, DstHeight: LongInt; SrcImageWidth, SrcImageHeight: LongInt; const DstClip: TRect);
{ Scales one rectangle to fit into another. Proportions are preserved so
  it could be used for 'Stretch To Fit Window' image drawing for instance.}
function ScaleRectToRect(const SourceRect, TargetRect: TRect): TRect;
{ Scales given size to fit into max size while keeping the original ascpect ration.
  Useful for calculating thumbnail dimensions etc.}
function ScaleSizeToFit(const CurrentSize, MaxSize: Types.TSize): Types.TSize; //=== ct9999 =====
{ Returns width of given rect. Part of RTL in newer Delphi.}
function RectWidth(const Rect: TRect): Integer;
{ Returns height of given rect. Part of RTL in newer Delphi.}
function RectHeight(const Rect: TRect): Integer;
{ Returns True if R1 fits into R2.}
function RectInRect(const R1, R2: TRect): Boolean;
{ Returns True if R1 and R2 intersects.}
function RectIntersects(const R1, R2: TRect): Boolean;

{ Converts pixel size in micrometers to corrensponding DPI.}
function PixelSizeToDpi(SizeInMicroMeters: Single): Single;
{ Converts DPI to corrensponding pixel size in micrometers.}
function DpiToPixelSize(Dpi: Single): Single;

function FloatRect(ALeft, ATop, ARight, ABottom: Single): TFloatRect;
function FloatRectWidth(const R: TFloatRect): Single;
function FloatRectHeight(const R: TFloatRect): Single;

{ Formats given message for usage in Exception.Create(..). Use only
  in except block - returned message contains message of last raised exception.}
function FormatExceptMsg(const Msg: string; const Args: array of const): string;
{ Outputs debug message - shows message dialog in Windows and writes to console
  in Linux/Unix.}
procedure DebugMsg(const Msg: string; const Args: array of const);
