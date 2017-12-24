
uses Windows, SysUtils;

{ ------------------------------------------------------------------------------------------------ }

type
  TMD5Data = array[0..15] of Byte;

{ ------------------------------------------------------------------------------------------------ }

// Renvois la representation du MD5 d'une chaine de caracteres
function MD5(const S : string) : string; overload;
// Renvois la representation du MD5 d'un buffer quelquonque
function MD5(const Buffer; const Len: integer): string; overload;
// REnvois la representation du MD5 d'un fichier
function MD5FromFile(const FileName : string) : string;

// Renvois une donnée MD5 d'une chaine de caracteres
function MD5DataFromString(const S : string) : TMD5Data;
// Renvois une donnée MD5 d'un buffer quelquonque
function MD5DataFromBuffer(const Buffer; const Len: integer) : TMD5Data;
// Renvois une donnée MD5 d'un fichier
function MD5DataFromFile(const FileName: string): TMD5Data;

// Convertis une donnée MD5 vers une chaine de caracteres
function MD5DataToStr(const Data : TMD5Data) : string;
// Convertis une chaine de caracteres representant un MD5 vers une donnée MD5
function MD5StrToMD5Data(const S : String) : TMD5Data;

// Verifie la validitée d'une chaine representant un MD5
function MD5StrCheck(const S : string) : boolean;
// Compare deux données MD5
function MD5Equal(const A, B: TMD5Data) : Boolean;
// Compare deux données MD5 grace a CompareMem
function MD5MemEqual(const A, B : TMD5Data) : boolean;

// Inverse le sens des données d'une donnée MD5
function MD5Reverse(const Data : TMD5Data) : TMD5Data;
// Inverse l'ordre des octets Pair et Impair d'une donnée MD5
function MD5OddSwap(const Data : TMD5Data) : TMD5Data;

{ ------------------------------------------------------------------------------------------------ }

implementation

uses MD5Core;

{ ------------------------------------------------------------------------------------------------ }

function MD5(const Buffer; const Len: integer): string;
begin
  result := MD5DataToStr( MD5DataFromBuffer(buffer,len) );
end;

function MD5(const S : string) : string;
begin
  result := MD5DataToStr( MD5DataFromBuffer(PChar(S)^, Length(S)) );
end;

function MD5FromFile(const FileName : string) : string;
begin
  result := MD5DataToStr( MD5DataFromFile(FileName) );
end;

{ ------------------------------------------------------------------------------------------------ }

function MD5DataFromString(const S : string) : TMD5Data;
begin
  Result := TMD5Data( MD5DataFromBuffer(PChar(S)^, Length(S)));
end;

function MD5DataFromBuffer(const Buffer; const Len: integer): TMD5Data;
var
  Context: TMD5Context;
begin
  MD5CoreInitialize(Context);
  MD5CoreUpdate(Context, Buffer, Len);
  Result := TMD5Data(MD5CoreFinalize(Context));
end;

function MD5DataFromFile(const FileName: string): TMD5Data;
var
	FileHandle,
	MapHandle   : THandle;
	ViewPointer : pointer;
	Context     : TMD5Context;
begin
  if not FileExists(FileName) then
     exit;

	MD5CoreInitialize(Context);

	FileHandle := CreateFile(pChar(FileName), GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE,
		            nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL or FILE_FLAG_SEQUENTIAL_SCAN, 0);

	if FileHandle <> INVALID_HANDLE_VALUE then
     try
		   MapHandle := CreateFileMapping(FileHandle, nil, PAGE_READONLY, 0, 0, nil);
		   if MapHandle <> 0 then
          try
			      ViewPointer := MapViewOfFile(MapHandle, FILE_MAP_READ, 0, 0, 0);
			      if ViewPointer <> nil then
               try
				         MD5CoreUpdate(Context, ViewPointer^, GetFileSize(FileHandle, nil));
               finally
				         UnmapViewOfFile(ViewPointer);
               end;
          finally
			      CloseHandle(MapHandle);
          end;
     finally
		   CloseHandle(FileHandle);
     end;

  result := TMD5Data(MD5CoreFinalize(Context));
end;


{ ------------------------------------------------------------------------------------------------ }

function MD5DataToStr(const Data : TMD5Data) : string;
var
  P: PChar;
  I: Integer;
const
  Digits: array[0..15] of Char = '0123456789abcdef';
begin
  SetLength(result, 32);
  P := PChar(result);
  for I := 0 to 15 do begin
    P[0] := Digits[Data[I] shr 4];
    P[1] := Digits[Data[I] and $F];
    Inc(P,2);
  end;
end;

{ ------------------------------------------------------------------------------------------------ }

function MD5StrToMD5Data(const S : String) : TMD5Data;
var
  N,SP : integer;
begin
  if (Length(s) <> 32) or (not MD5StrCheck(S)) then
     exit;
  for N := 0 to 15 do begin
      SP := (N shl 1)+1;
      Result[N] := byte(StrToInt('$'+S[SP]+S[SP+1]));
  end;
end;

{ ------------------------------------------------------------------------------------------------ }

function MD5StrCheck(const S : string) : boolean;
var N,L : integer;
begin
  N      := 1;
  L      := Length(S)+1;
  result := L = 33;
  while Result and (N < L) do begin
     Result := S[N] in ['0'..'9','a'..'f'];
     inc(N);
  end;
end;

function MD5Equal(const A, B: TMD5Data): Boolean;
var
  I : Integer;
begin
  I      := 0;
  result := true;
  while result and (I < 16) do begin
        result := A[I] = B[I];
        inc(I);
  end;
end;

function MD5MemEqual(const A, B : TMD5Data) : boolean;
begin
  result := CompareMem(@A,@B,SizeOf(TMD5Data));
end;

{ ------------------------------------------------------------------------------------------------ }

function MD5Reverse(const Data : TMD5Data) : TMD5Data;
var N : integer;
begin
  for N := 0 to 15 do
      result[15-N] := Data[N];
end;

function MD5OddSwap(const Data : TMD5Data) : TMD5Data;
var N : integer;
begin
  for N := 0 to 15 do
      if odd(N) then
         result[N] := Data[N-1]
      else
         result[N] := Data[N+1];
end;  

end.
