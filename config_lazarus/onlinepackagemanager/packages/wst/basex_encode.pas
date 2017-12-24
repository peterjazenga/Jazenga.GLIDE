{   This file is part of the Web Service Toolkit
    Copyright (c) 2006, 2007 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$INCLUDE wst_global.inc}
{$RANGECHECKS OFF}
unit basex_encode;

interface
uses
  SysUtils, wst_types;

type

  EBaseXException = class(Exception);
  EBase16Exception = class(EBaseXException);
  EBase32Exception = class(EBaseXException);     
  EBase64Exception = class(EBaseXException);

  TBaseXOption = ( xoDecodeIgnoreIllegalChar );
  TBaseXOptions = set of TBaseXOption;

  function Base64Encode(const ALength : PtrInt; const AInBuffer) : string;overload;
  function Base64Encode(const AInBuffer : TBinaryString) : string;overload;
  function Base64Decode(const AInBuffer : string; const AOptions : TBaseXOptions) : TByteDynArray;overload;

  function Base32Encode(const ALength : PtrInt; const AInBuffer) : string; overload;
  function Base32Encode(const AInBuffer : TBinaryString) : string; overload;
  function Base32Decode(const AInBuffer : string; const AOptions : TBaseXOptions) : TByteDynArray;

  procedure Base16Encode(const ABin; const ALen : Integer; AOutBuffer : PAnsiChar); overload;
  procedure Base16Encode(const ABin; const ALen : Integer; AOutBuffer : PWideChar); overload;
  function Base16Encode(const ABin; const ALen : Integer) : string; overload;
  function Base16Encode(const AInBuffer : TBinaryString) : string;overload;
  function Base16Decode(
    const AHex    : PAnsiChar;
    var   ABin;
    const ABinLen : Integer;
    const AOptions : TBaseXOptions = [xoDecodeIgnoreIllegalChar]
  ) : Integer;overload;
  function Base16Decode(
    const AHex    : PWideChar;
    var   ABin;
    const ABinLen : Integer;
    const AOptions : TBaseXOptions = [xoDecodeIgnoreIllegalChar]
  ) : Integer;overload;
  function Base16Decode(const AInBuffer : ansistring; const AOptions : TBaseXOptions) : TByteDynArray;overload;


implementation
uses
  wst_consts;

const
  Base64_CHAR_TABLE : array[0..63] of char = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
  IM = 255; INVALID_MARKER = IM;
  Base64_CHAR_INDEX_TABLE : array[Byte] of Byte = (
    IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,
    IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,
    IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,62,IM,IM,IM,63,
    52,53,54,55,56,57,58,59,60,61,IM,IM,IM,00,IM,IM,
    IM,00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,
    15,16,17,18,19,20,21,22,23,24,25,IM,IM,IM,IM,IM,
    IM,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
    41,42,43,44,45,46,47,48,49,50,51,IM,IM,IM,IM,IM,
    IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,
    IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,
    IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,
    IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,
    IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,
    IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,
    IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,
    IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM
  );
  //Base64_CHAR_SET = ['A'..'Z','a'..'z','0'..'9','+','/'];

function Base64Encode(const ALength : PtrInt; const AInBuffer) : string;
var
  locBuffer : PByte;
  locCopied, locBlockCount, i, locAtualLen : PtrInt;
  locInQuantom : array[0..2] of Byte;
  locOutQuantom : array[0..3] of Char;
begin
  Result := '';
  if ( ALength > 0 ) then begin
    locBuffer := @AInBuffer;
    locBlockCount := ALength div 3;
    SetLength(Result,(locBlockCount + 1 ) * 4);
    locAtualLen := 0;
    for i := 1 to locBlockCount do begin
      Move(locBuffer^,locInQuantom[0],3);
      Inc(locBuffer,3);
      locOutQuantom[0] := Base64_CHAR_TABLE[( locInQuantom[0] shr 2 )];
      locOutQuantom[1] := Base64_CHAR_TABLE[( ( locInQuantom[0] and 3 ) shl 4 ) or ( locInQuantom[1] shr 4 )];
      locOutQuantom[2] := Base64_CHAR_TABLE[( ( locInQuantom[1] and 15 ) shl 2 ) or ( locInQuantom[2] shr 6 )];
      locOutQuantom[3] := Base64_CHAR_TABLE[( locInQuantom[2] and 63 )];
      Move(locOutQuantom[0],Result[locAtualLen + 1],( 4 * SizeOf(Char) ));
      Inc(locAtualLen,4);
    end;
    locCopied := ALength mod 3;
    if ( locCopied > 0 ) then begin
      case locCopied of
        1 :
          begin
            Move(locBuffer^,locInQuantom[0],1);
            locInQuantom[1] := 0;
            locOutQuantom[0] := Base64_CHAR_TABLE[( locInQuantom[0] shr 2 )];
            locOutQuantom[1] := Base64_CHAR_TABLE[( ( locInQuantom[0] and 3 ) shl 4 ) or ( locInQuantom[1] shr 4 )];
            locOutQuantom[2] := '=';
            locOutQuantom[3] := '=';
          end;
        2 :
          begin
            Move(locBuffer^,locInQuantom[0],2);
            locInQuantom[2] := 0;
            locOutQuantom[0] := Base64_CHAR_TABLE[( locInQuantom[0] shr 2 )];
            locOutQuantom[1] := Base64_CHAR_TABLE[( ( locInQuantom[0] and 3 ) shl 4 ) or ( locInQuantom[1] shr 4 )];
            locOutQuantom[2] := Base64_CHAR_TABLE[( ( locInQuantom[1] and 15 ) shl 2 ) or ( locInQuantom[2] shr 6 )];
            locOutQuantom[3] := '=';
          end;
      end;
      Move(locOutQuantom[0],Result[locAtualLen + 1],( 4 * SizeOf(Char) ));
      Inc(locAtualLen,4);
    end;
    SetLength(Result,locAtualLen);
  end;
end;

function Base64Encode(const AInBuffer : TBinaryString) : string;
begin
  if ( Length(AInBuffer) = 0 ) then
    Result := ''
  else
    Result := Base64Encode(Length(AInBuffer),AInBuffer[1]);
end;

function Base64Decode(const AInBuffer : string; const AOptions : TBaseXOptions) : TByteDynArray;
var
  locBuffer : PChar;
  locInLen, locInIndex, i, locPadded : PtrInt;
  locOutQuantom : array[0..2] of Byte;
  locInQuantom : array[0..3] of Byte;
  ok : Boolean;
  locAtualLen : PtrInt;
  locInValue, locReadedValidChars : Byte;
  locFailOnIllegalChar : Boolean;
begin
  if ( AInBuffer = '' ) then begin
    Result := nil;
  end else begin
    locInIndex := 0;
    locAtualLen := 0;
    locPadded := 0;
    locInLen := Length(AInBuffer);
    SetLength(Result,locInLen);
    locBuffer := @(AInBuffer[1]);
    locFailOnIllegalChar := not ( xoDecodeIgnoreIllegalChar in AOptions );
    while ( locInIndex < locInLen ) do begin
      locReadedValidChars := 0;
      for i := 0 to 3 do begin
        ok := False;
        while ( locInIndex <= locInLen ) do begin
{$IF SizeOf(Char) > SizeOf(Byte) }
          if ( Ord(locBuffer^) > High(Byte) ) then
            locInValue := INVALID_MARKER
          else
            locInValue := Base64_CHAR_INDEX_TABLE[Ord(locBuffer^)];
{$ELSE}
          locInValue := Base64_CHAR_INDEX_TABLE[Ord(locBuffer^)];
{$IFEND}
          Inc(locBuffer);
          Inc(locInIndex);          
          if ( locInValue <> INVALID_MARKER ) then begin
            locInQuantom[i] := locInValue;
            if ( locBuffer^ = '=' ) then begin
              Inc(locPadded);
            end;
            ok := True;
            Inc(locReadedValidChars);
            Break;
          end else begin
            if locFailOnIllegalChar then
              raise EBase64Exception.Create(SERR_InvalidEncodedData);
          end;
        end;
        if ( not ok ) and locFailOnIllegalChar then
          raise EBase64Exception.CreateFmt(SERR_IllegalChar,[Char(locBuffer^)]);
      end;
      if ( locReadedValidChars > 0 ) then begin
        locOutQuantom[0] := ( locInQuantom[0] shl 2 ) or ( locInQuantom[1] shr 4 );
        locOutQuantom[1] := ( locInQuantom[1] shl 4 ) or ( locInQuantom[2] shr 2 );
        locOutQuantom[2] := ( locInQuantom[2] shl 6 ) or ( locInQuantom[3] );
        Move(locOutQuantom[0],Result[locAtualLen],3 - locPadded);
        Inc(locAtualLen,3 - locPadded);
      end;
    end;
    SetLength(Result,locAtualLen);
  end;
end;

function Base64DecodeStr(const AInBuffer : string; const AOptions : TBaseXOptions) : TBinaryString;
var
  locRes : TByteDynArray;
begin
  locRes := Base64Decode(AInBuffer,AOptions);
  SetLength(Result,Length(locRes));
  if ( Length(Result) > 0 ) then
    Move(locRes[0],Result[1],Length(Result));
end;

const
  Base32_CHAR_TABLE : array[0..31] of char = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ234567'; 

function Base32Encode(const ALength : PtrInt; const AInBuffer) : string;
var
  locBuffer : PByte;
  locCopied, locBlockCount, i, locAtualLen : PtrInt;
  locInQuantom : array[0..4] of Byte;
  locOutQuantom : array[0..7] of Char;
begin
  Result := '';
  if ( ALength > 0 ) then begin
    locBuffer := @AInBuffer;
    locBlockCount := ALength div 5;
    SetLength(Result,(locBlockCount + 1 ) * 8);
    locAtualLen := 0;
    for i := 1 to locBlockCount do begin
      Move(locBuffer^,locInQuantom[0],5);
      Inc(locBuffer,5);
      locOutQuantom[0] := Base32_CHAR_TABLE[(locInQuantom[0] shr 3)];
      locOutQuantom[1] := Base32_CHAR_TABLE[( (locInQuantom[0] and 7) shl 2 ) or ( locInQuantom[1] shr 6 )];
      locOutQuantom[2] := Base32_CHAR_TABLE[( (locInQuantom[1] and 62) shr 1 )];
      locOutQuantom[3] := Base32_CHAR_TABLE[( (locInQuantom[1] and 1) shl 4 ) or ( locInQuantom[2] shr 4 )];
      locOutQuantom[4] := Base32_CHAR_TABLE[( (locInQuantom[2] and 15) shl 1 ) or ( locInQuantom[3] shr 7 )];
      locOutQuantom[5] := Base32_CHAR_TABLE[( (locInQuantom[3] and 124) shr 2 )]; 
      locOutQuantom[6] := Base32_CHAR_TABLE[( (locInQuantom[3] and 3) shl 3 ) or ( locInQuantom[4] shr 5 )];
      locOutQuantom[7] := Base32_CHAR_TABLE[(locInQuantom[4] and 31)];  
      Move(locOutQuantom[0],Result[locAtualLen + 1],( 8 * SizeOf(Char) ));
      Inc(locAtualLen,8);
    end;
    locCopied := ALength mod 5;
    if ( locCopied > 0 ) then begin
      case locCopied of
        1 :
          begin
            Move(locBuffer^,locInQuantom[0],1);
            locInQuantom[1] := 0;
            locOutQuantom[0] := Base32_CHAR_TABLE[(locInQuantom[0] shr 3)];
            locOutQuantom[1] := Base32_CHAR_TABLE[( (locInQuantom[0] and 7) shl 2 ) or ( locInQuantom[1] shr 6 )];
            locOutQuantom[2] := '=';
            locOutQuantom[3] := '=';
            locOutQuantom[4] := '=';
            locOutQuantom[5] := '=';
            locOutQuantom[6] := '=';
            locOutQuantom[7] := '=';
          end;
        2 :
          begin
            Move(locBuffer^,locInQuantom[0],2);
            locInQuantom[2] := 0;
            locOutQuantom[0] := Base32_CHAR_TABLE[(locInQuantom[0] shr 3)];
            locOutQuantom[1] := Base32_CHAR_TABLE[( (locInQuantom[0] and 7) shl 2 ) or ( locInQuantom[1] shr 6 )];
            locOutQuantom[2] := Base32_CHAR_TABLE[( (locInQuantom[1] and 62) shr 1 )];
            locOutQuantom[3] := Base32_CHAR_TABLE[( (locInQuantom[1] and 1) shl 4 ) or ( locInQuantom[2] shr 4 )];
            locOutQuantom[4] := '=';
            locOutQuantom[5] := '=';
            locOutQuantom[6] := '=';
            locOutQuantom[7] := '='; 
          end;   
        3 :
          begin
            Move(locBuffer^,locInQuantom[0],3);
            locInQuantom[3] := 0;
            locOutQuantom[0] := Base32_CHAR_TABLE[(locInQuantom[0] shr 3)];
            locOutQuantom[1] := Base32_CHAR_TABLE[( (locInQuantom[0] and 7) shl 2 ) or ( locInQuantom[1] shr 6 )];
            locOutQuantom[2] := Base32_CHAR_TABLE[( (locInQuantom[1] and 62) shr 1 )];
            locOutQuantom[3] := Base32_CHAR_TABLE[( (locInQuantom[1] and 1) shl 4 ) or ( locInQuantom[2] shr 4 )];
            locOutQuantom[4] := Base32_CHAR_TABLE[( (locInQuantom[2] and 15) shl 1 ) or ( locInQuantom[3] shr 7 )];
            locOutQuantom[5] := '=';
            locOutQuantom[6] := '=';
            locOutQuantom[7] := '='; 
          end;     
        4 :
          begin
            Move(locBuffer^,locInQuantom[0],4);
            locInQuantom[4] := 0;
            locOutQuantom[0] := Base32_CHAR_TABLE[(locInQuantom[0] shr 3)];
            locOutQuantom[1] := Base32_CHAR_TABLE[( (locInQuantom[0] and 7) shl 2 ) or ( locInQuantom[1] shr 6 )];
            locOutQuantom[2] := Base32_CHAR_TABLE[( (locInQuantom[1] and 62) shr 1 )];
            locOutQuantom[3] := Base32_CHAR_TABLE[( (locInQuantom[1] and 1) shl 4 ) or ( locInQuantom[2] shr 4 )];
            locOutQuantom[4] := Base32_CHAR_TABLE[( (locInQuantom[2] and 15) shl 1 ) or ( locInQuantom[3] shr 7 )]; 
            locOutQuantom[5] := Base32_CHAR_TABLE[( (locInQuantom[3] and 124) shr 2 )]; 
            locOutQuantom[6] := Base32_CHAR_TABLE[( (locInQuantom[3] and 3) shl 3 ) or ( locInQuantom[4] shr 5 )];  
            locOutQuantom[7] := '='; 
          end;     
      end;
      Move(locOutQuantom[0],Result[locAtualLen + 1],(8 * SizeOf(Char)));
      Inc(locAtualLen,8);
    end;
    SetLength(Result,locAtualLen);
  end;
end;    

function Base32Encode(const AInBuffer : TBinaryString) : string;
begin
  if ( Length(AInBuffer) = 0 ) then
    Result := ''
  else
    Result := Base32Encode(Length(AInBuffer),AInBuffer[1]);
end; 

function Base32Decode(const AInBuffer : string; const AOptions : TBaseXOptions) : TByteDynArray;
const
  ALPHA_UP_MAP : array['A'..'Z'] of Byte = (  
    0 ,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 
    16, 17, 18, 19, 20, 21, 22, 23, 24, 25 
  );
  ALPHA_LOW_MAP : array['a'..'z'] of Byte = (  
    0 ,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 
    16, 17, 18, 19, 20, 21, 22, 23, 24, 25 
  );                                   
  DIGIT_MAP : array['2'..'7'] of Byte = ( 26, 27, 28, 29, 30, 31);
var
  locBuffer : PChar;
  locInLen, locInIndex, i, locPadded : PtrInt;
  locOutQuantom : array[0..4] of Byte;
  locInQuantom : array[0..7] of Byte;
  ok : Boolean;
  locAtualLen : PtrInt;
  locInValue, locReadedValidChars : Byte;
  locFailOnIllegalChar : Boolean;
  locTrueData : Integer;
begin
  if ( AInBuffer = '' ) then begin
    Result := nil;
  end else begin
    locInIndex := 0;
    locAtualLen := 0;
    locPadded := 0;
    locInValue := 0;
    locInLen := Length(AInBuffer);
    SetLength(Result,locInLen);
    locBuffer := @(AInBuffer[1]);
    locFailOnIllegalChar := not ( xoDecodeIgnoreIllegalChar in AOptions );
    while ( locInIndex < locInLen ) do begin
      locReadedValidChars := 0;
      for i := 0 to 7 do begin
        ok := False;
        while ( locInIndex <= locInLen ) do begin
          case locBuffer^ of
            'A'..'Z' : locInValue := ALPHA_UP_MAP[locBuffer^];
            'a'..'z' : locInValue := ALPHA_LOW_MAP[locBuffer^];
            '2'..'7' : locInValue := DIGIT_MAP[locBuffer^];
            '='      : locInValue := 0;
            else begin
              if locFailOnIllegalChar then
                raise EBase32Exception.CreateFmt(SERR_IllegalChar,[Char(locBuffer^)]);
              Inc(locBuffer);
              Inc(locInIndex);                
              Continue;
            end;
          end;  
          Inc(locBuffer);
          Inc(locInIndex);          
          locInQuantom[i] := locInValue;
          if ( locBuffer^ = '=' ) then 
            Inc(locPadded);
          ok := True;
          Inc(locReadedValidChars);
          Break;
        end;
        if ( not ok ) and locFailOnIllegalChar then
          raise EBase64Exception.CreateFmt(SERR_IllegalChar,[Char(locBuffer^)]);
      end;          
      if ( locReadedValidChars > 0 ) then begin
        locOutQuantom[0] := ( locInQuantom[0] shl 3 ) or ( locInQuantom[1] shr 2 );
        locOutQuantom[1] := ( (locInQuantom[1] shl 6) and 192 ) or 
                            ( (locInQuantom[2] shl 1) and 62) or
                            (locInQuantom[3] shr 4);
        locOutQuantom[2] := ((locInQuantom[3] shl 4) and 240) or ( locInQuantom[4] shr 1);
        locOutQuantom[3] := ((locInQuantom[4] shl 7) and 128) or 
                            ((locInQuantom[5] shl 2) and 124) or
                            (locInQuantom[6] shr 3);
        locOutQuantom[4] := ((locInQuantom[6] shl 5) and 224) or locInQuantom[7]; 
        case locPadded of
          6 : locTrueData := 1;
          4 : locTrueData := 2;
          3 : locTrueData := 3;
          1 : locTrueData := 4;
          else
              locTrueData := 5;
        end;
        Move(locOutQuantom[0],Result[locAtualLen],locTrueData);
        Inc(locAtualLen,locTrueData);
      end;
    end;
    SetLength(Result,locAtualLen);
  end;
end;

procedure Base16Encode(const ABin; const ALen : Integer; AOutBuffer : PAnsiChar);
const
  HEX_MAP : array[0..15] of AnsiChar = '0123456789ABCDEF';
var
  p : PByte;
  pres : PAnsiChar;
  i : Integer;
begin
  if ( ALen > 0 ) then begin
    pres := AOutBuffer;
    p := PByte(@Abin);
    for i := 1 to ALen do begin
      pres^ := HEX_MAP[p^ shr 4];
      PAnsiChar(PtrUInt(pres) + SizeOf(AnsiChar))^ := HEX_MAP[p^ and $F];
      Inc(pres,2);
      Inc(p);
    end;
  end;
end;

procedure Base16Encode(const ABin; const ALen : Integer; AOutBuffer : PWideChar);
const
  HEX_MAP : array[0..15] of WideChar = ('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F');
var
  p : PByte;
  pres : PWideChar;
  i : Integer;
begin
  if ( ALen > 0 ) then begin
    pres := AOutBuffer;
    p := PByte(@Abin);
    for i := 1 to ALen do begin
      pres^ := HEX_MAP[p^ shr 4];
      PWideChar(PtrUInt(pres) + SizeOf(WideChar))^ := HEX_MAP[p^ and $F];
      Inc(pres,2);
      Inc(p);
    end;
  end;
end;

function Base16Encode(const ABin; const ALen : Integer) : string;
begin
  if ( ALen > 0 ) then begin
    SetLength(Result,(2 * ALen));
    Base16Encode(ABin,ALen,PChar(@Result[1]));
  end else begin
    Result := '';
  end;
end;

function Base16Encode(const AInBuffer : TBinaryString) : string;
begin
  Result := Base16Encode(AInBuffer[1],Length(AInBuffer));
end;

// Returns the actual bytes count.
function Base16Decode(
  const AHex    : PAnsiChar;
  var   ABin;
  const ABinLen : Integer;
  const AOptions : TBaseXOptions = [xoDecodeIgnoreIllegalChar]
) : Integer;
const
  DIGIT_MAP : array['0'..'9'] of Byte = ( 0, 1, 2, 3, 4, 5, 6, 7, 8, 9);
  ALPHA_UP_MAP : array['A'..'F'] of Byte = ( 10, 11, 12, 13, 14, 15 );
  ALPHA_LOW_MAP : array['a'..'f'] of Byte = ( 10, 11, 12, 13, 14, 15 );
var
  i : Integer;
  hp : PAnsiChar;
  bp : PByte;
  binVal : Byte;
  locFailOnIllegalChar : Boolean;
begin
  Result := 0;
  if ( ABinLen > 0 ) then begin
    binVal := 0;
    bp := PByte(@Abin);
    hp := AHex;
    locFailOnIllegalChar := not ( xoDecodeIgnoreIllegalChar in AOptions );
    for i := 0 to Pred(ABinLen) do begin
      if ( hp^ = #0 ) then
        Break;
      while ( hp^ <> #0 ) do begin
        case hp^ of
          '0'..'9' :
            begin
              binVal := ( DIGIT_MAP[hp^] shl 4);
              Break;
            end;
          'A'..'Z' :
            begin
              binVal := ( ALPHA_UP_MAP[hp^] shl 4);
              Break;
            end;
          'a'..'z' :
            begin
              binVal := ( ALPHA_LOW_MAP[hp^] shl 4);
              Break;
            end;
          else
            begin
              if locFailOnIllegalChar then
                raise EBase16Exception.Create(SERR_UnexpectedEndOfData);
            end;
        end;
        Inc(hp);
      end;
      if ( hp^ = #0 ) then
        raise EBase16Exception.Create(SERR_UnexpectedEndOfData);
      Inc(hp);
      while ( hp^ <> #0 ) do begin
        case hp^ of
          '0'..'9' :
            begin
              bp^ := binVal or DIGIT_MAP[hp^];
              Break;
            end;
          'A'..'Z' :
            begin
              bp^ := binVal or ALPHA_UP_MAP[hp^];
              Break;
            end;
          'a'..'z' :
            begin
              bp^ := binVal or ALPHA_LOW_MAP[hp^];
              Break;
            end;
          else
            begin
              if locFailOnIllegalChar then
                raise EBase16Exception.CreateFmt(SERR_IllegalChar,[hp^]);
            end;
        end;
        Inc(hp);
      end;
      if ( hp^ = #0 ) then
        raise EBase16Exception.Create(SERR_UnexpectedEndOfData);
      Inc(hp);
      Inc(bp);
    end;
    Result := PtrUInt(bp) - PtrUInt(@Abin);
  end;
end;

// Returns the actual bytes count.
function Base16Decode(
  const AHex    : PWideChar;
  var   ABin;
  const ABinLen : Integer;
  const AOptions : TBaseXOptions = [xoDecodeIgnoreIllegalChar]
) : Integer;
const
  DIGIT_MAP : array['0'..'9'] of Byte = ( 0, 1, 2, 3, 4, 5, 6, 7, 8, 9);
  ALPHA_UP_MAP : array['A'..'F'] of Byte = ( 10, 11, 12, 13, 14, 15 );
  ALPHA_LOW_MAP : array['a'..'f'] of Byte = ( 10, 11, 12, 13, 14, 15 );
var
  i : Integer;
  hp : PWideChar;
  bp : PByte;
  binVal : Byte;
  locFailOnIllegalChar : Boolean;
begin
  Result := 0;
  if ( ABinLen > 0 ) then begin
    binVal := 0;
    bp := PByte(@Abin);
    hp := AHex;
    locFailOnIllegalChar := not ( xoDecodeIgnoreIllegalChar in AOptions );
    for i := 0 to Pred(ABinLen) do begin
      if ( hp^ = #0 ) then
        Break;
      while ( hp^ <> #0 ) do begin
        case hp^ of
          '0'..'9' :
            begin
              binVal := ( DIGIT_MAP[AnsiChar(hp^)] shl 4);
              Break;
            end;
          'A'..'Z' :
            begin
              binVal := ( ALPHA_UP_MAP[AnsiChar(hp^)] shl 4);
              Break;
            end;
          'a'..'z' :
            begin
              binVal := ( ALPHA_LOW_MAP[AnsiChar(hp^)] shl 4);
              Break;
            end;
          else
            begin
              if locFailOnIllegalChar then
                raise EBase16Exception.Create(SERR_UnexpectedEndOfData);
            end;
        end;
        Inc(hp);
      end;
      if ( hp^ = #0 ) then
        raise EBase16Exception.Create(SERR_UnexpectedEndOfData);
      Inc(hp);
      while ( hp^ <> #0 ) do begin
        case hp^ of
          '0'..'9' :
            begin
              bp^ := binVal or DIGIT_MAP[AnsiChar(hp^)];
              Break;
            end;
          'A'..'Z' :
            begin
              bp^ := binVal or ALPHA_UP_MAP[AnsiChar(hp^)];
              Break;
            end;
          'a'..'z' :
            begin
              bp^ := binVal or ALPHA_LOW_MAP[AnsiChar(hp^)];
              Break;
            end;
          else
            begin
              if locFailOnIllegalChar then
                raise EBase16Exception.CreateFmt(SERR_IllegalChar,[hp^]);
            end;
        end;
        Inc(hp);
      end;
      if ( hp^ = #0 ) then
        raise EBase16Exception.Create(SERR_UnexpectedEndOfData);
      Inc(hp);
      Inc(bp);
    end;
    Result := PtrUInt(bp) - PtrUInt(@Abin);
  end;
end;

function Base16Decode(const AInBuffer : ansistring; const AOptions : TBaseXOptions) : TByteDynArray;
var
  i : Integer;
begin
  if ( Length(AInBuffer) > 0 ) then begin
    SetLength(Result,Length(AInBuffer) div 2);
    i := Base16Decode(PAnsiChar(AInBuffer),Result[0],Length(Result),AOptions);
    if ( i <> Length(Result) ) then
      SetLength(Result,i);
  end;
end;

end.
