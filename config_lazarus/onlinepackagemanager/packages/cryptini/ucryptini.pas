unit ucryptini;
{$mode objfpc}{$H+}
{$DEFINE USE_DCPCRYPT} // Delete this if you don't have the DCrypt library
                       // and CryptINI will use built-in BASE64 instead
                       // (but turn all project debugging checks off!)
                       // DCrypt can be downloaded via OnlinePackageManager
                       // And is vastly preferable to BASE64
{$IF FPC_VERSION = 3} // inifiles.pp changed in fpc v3.1.? (17 Jan 2016)
  {$IF FPC_RELEASE > 0}
   {$IF FPC_PATCH > 0}
     {$DEFINE FPC311}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{ 'Paranoid' version of TIniFile

  Copyright (C) 2016 Gordon Bamber minesadorada@gmail.com
  Base 64 code by David Barton

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

PURPOSE
=======
This is a TiniFile implementation that is resistant to tampering.
In normal (PlainTextMode = FALSE) mode, any calls to Write values are
accompanied by an embedded MD5 hash value (and also reversed then Base64/IDEA Encrypted)
This is invisible in normal use (i.e. read methods return normal results)
but there are added methods to internally verify any entries.
It also is able to write a standard ident section containing various
details including authorship and copyright.  A single function allows
you to check on app startup whether this section has been altered.
It also includes a useful 'First Run' functionality.
It's intended purpose is to store information that cannot be easily altered
in a text editor (such as HiScores etc) by a weekend scripter.
The WriteInteger method is the most secure as it double-encrypts as well as
embedding an MD5Hash value as a checksum.  Very handy to save scores etc.
It is paired with ReadInteger and VerifyInteger

DISCLAIMER
==========
This unit does not claim to pass any security tests nor be used in
any environment where a moderate-level hacker could circumvent it.

ENCRYPTION
==========
By Default CryptINI uses the DCPCrypt package for string encryption.
The mode is IDEA cipher with an MD5 hash for the key. The EncryptINI
and DecryptINI methods use DCPCrypt RC4 Cipher (With SHA hash)
There is a $DEFINE USE_DCPCRYPT directive at the top of this file.

If this DEFINE is commented out, then CryptINI will default
to BASE64 string encryption, which is weaker. Encrypt/DecryptINI methods
will be unavailable.
You can then delete any requirement for dcpcrypt in the project inspector.

FREEPASCAL VERSIONS
===================
Starting in FPC V3.1.1 there are additional options in TINIFile which
are implemented in TCryptINI if the FPC Version >= 3.1.1 is detected.

USE AND EXAMPLE CODE
====================
** You can hard-code an Ident Section in your INI file
** and check if it has been altered
** Typical Form.Create()
procedure TForm1.FormCreate(Sender: TObject);
const
  C_KEYPHRASE = 'I do like to be beside the seaside'; // Move this to the top of the unit
Begin
 ...other code
 // Initialise the encrypted config file
 INI := TCryptIniFile.Create(ChangeFileExt(Application.EXEName, '.cfg'));

 // First ever run.  INI is absent
 If INI.IsVirgin then INI.WriteIdent('minesadorada','(c)2016','minesadorada@charcodelvalle.com','Creative Commons',TRUE);l

 if NOT INI.VerifyIdent('5b319674f5cb55f3ed1e404e33c25868') then // I got this from the INI file
   ShowMessage('This is not a genuine copy of ' + Application.Title + '!')
 else INI.Deflower; // If not Deflowered then the default ini is used.

 // After first run, use the encrypted version
 If NOT INI.IsVirgin then
 begin
   INI.DecryptINI(TRUE,C_KEYPHRASE);
 // Check the unencrypted version..
   if NOT INI.VerifyIdent('5b319674f5cb55f3ed1e404e33c25868') then // I got this from the INI file
   ShowMessage('This is not a genuine copy of ' + Application.Title + '!');
 end;
 INI.KeyPhrase:=C_KEYPHRASE; // for subsequent read/writes
 ...other code
end;

procedure Tmainform.FormDestroy(Sender: TObject);
const
  C_KEYPHRASE = 'I do like to be beside the seaside'; // Move this to the top of the unit
Begin
 ...other code
  INI.EncryptINI(TRUE,C_KEYPHRASE);
  ...other code
end;


** Has the Ident been tampered with?  Put this in Form.Create
// Use the MD5 value from the INI file (use your own!)
If INI.VerifyIdent('92abf0deecbb25c435bff507a396d92a') then
  ShowMessage('Ident verified OK') // do nothing
else
  ShowMessage('Ident failed verification'); // Warning message/exit

** Test for first run
If INI.IsVirgin then // note that doing the test Deflowers the app by default
   ShowMessage('First time run of this app');

** Toggle to normal UnEncrypted INI use
(by default it is set to FALSE)
INI.PlainTextMode:=TRUE;
INI.WriteString('MySection', 'String', 'MyString'); // just writes normally

** When PlainTextMode = FALSE (default),Write<anytype> encrypts the value
   and prefixes the encrypted value with an MD5Hash
INI.WriteInteger('MySection', 'Integer',1000);
** Note WriteInteger adds a '+' (INTEGER_MARKER) to the written Key
   This is so that CryptINI can deal with Integers specially
   Using CryptINI in either mode, this is invisible in use
   (i.e. don't add a + to the ident for any methods)

** When PlainTextMode = FALSE (default), use these convenient methods if you like:
INI.ReadUnencryptedString
INI.WriteUnencryptedString
INI.ReadUnEncryptedInteger
INI.WriteUnencryptedInteger

** Store 'First Run' status using these methods:
INI.IsVirgin
INI.Deflower
INI.ReFlower (useful for testing)

** You can encrypt and decrypt the entire INI file usine EncryptINI and DecryptINI
   This uses a different cipher and hash method, so is extra-secure
   cryptINI methods and properties an only work with an unencrypted INI file
   So; Decrypt on startup, and Encrypt before exit.
   By default the routines use the KeyPhrase property, but you can override this
   in the method call along with whether to delete the "old" file and what
   file extension to use for the encrypted file.

** Original IniFiles methods and properties
   Just use them as normal. Note: you can mix-and-match Plaintext and Encrypted
   in the same INI file. Just toggle the property PlainTextMode before Writing/Reading
}

interface

uses
  Forms, SysUtils, LazUTF8, LazFileUtils,StrUtils, INIFiles, md5, LCLVersion,
  Classes,fileinfo
  // DEBUGGING for ShowMessage:,Dialogs
  // ,Dialogs
  {$IFDEF USE_DCPCRYPT},DCPidea, DCPmd5,DCPrc4, DCPsha1{$ENDIF}
  ;


const
  // Fixed Ident section values for Read/Write/Verify Ident methods
  // Change the values here if you like.
  IDENT_SECTION = 'ProgramInfo';
  IDENT_APPNAME = 'App name';
  IDENT_APPVERSION = 'App version';
  IDENT_EXE = 'App Path';
  IDENT_AUTHOR = 'Author';
  IDENT_COPYRIGHT = 'Copyright';
  IDENT_LICENSE = 'License';
  IDENT_CONTACT = 'Contact';
  IDENT_LCLVERSION = 'LCL Version';
  IDENT_FPCVERSION = 'FPC Version';
  IDENT_TARGET = 'Target System';
  IDENT_LASTCOMPILED = 'Last Compiled';
  IDENT_FIRSTRUN = 'FirstRun';
  IDENT_MD5HASH = 'MD5Hash';

  // You can change these Consts here if you like
  INTEGER_MARKER = '+'; // Appended to Crypted Integer Idents
  // so that ReadSectionValues,ValueExists and DeleteKey methods will work
  DEFAULT_KEYPHRASE = 'Choose a better keyphrase than this!'; // If none specified
  {$IFDEF WINDOWS}
  DEFAULT_EXT = '.enc';
  {$ELSE}
  DEFAULT_EXT = 'enc';
  {$ENDIF}

  {$IFNDEF USE_DCPCRYPT}
  // Base64 array of bytes. CHANGE AT YOUR PERIL!
  B64: array[0..63] of byte = (65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80,
    81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108,
    109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 48, 49, 50, 51, 52, 53,
    54, 55, 56, 57, 43, 47);
  {$ENDIF}

C_VERSION = '0.1.0';
{
  VERSION HISTORY
  ===============
  V 0.0.1: Initial alpha release by (c)2016 minesadorada@charcodelvalle.com (GB)
  V 0.0.2: Improved Read/Write/Verify integer values (GB)
  V 0.0.3: Section functions added.  Integer marker added. (GB)
  V 0.0.4: Compatibility test for fpc 3 additions to TIniFiles (GB)
  V 0.0.5: Added DCPCrypt units and conditional DEFINES.  Added Encrypt/Decrypt INI (GB)
  V 0.0.6: ReadBinaryStream and WriteBinaryStream added (GB)
  V 0.0.7: ReadSection debugged (GB)
  V 0.0.8: Bugfix: FPC Version {DEFINE FPC3} corrected (GB)
  V 0.0.9: Added SplitKeyValue procedure.  Other fixes and improvements (GB)
  V 0.1.0: Added Program Version info to Ident section
  V 0.1.2: ??
}
type
  TCryptIniFile = class(TIniFile)
  private
    // Md5 Stuff
    fMyDigest: TMDDigest; // defined in md5 unit
    fMD5String: string;
    // For Read/write Integers
    fKey: longint;
    fKeyPhrase: string;
    // Encryption on/off
    fPlainText: boolean;
    fSectionHashing: boolean;
    fVersion: string;
    {$IFDEF USE_DCPCRYPT}
    DCP_idea1: TDCP_idea;
    DCP_md5_1: TDCP_md5;
    {$ENDIF}
    fHashType:String;
    fCipherType:String;
    // Functions to convert values
    function EncodeString(const sValue: string): string;
    function DecodeString(const sValue: string): string;
    // Functions to convert Integers only
    function EncodeInteger(const iValue: longint): longint;
    function DecodeInteger(const iValue: longint): longint;
    // Either..
    procedure SetKeyPhrase(AValue: string); // Sets fKey from a string (Preferrred method)
    Procedure SetKey(aValue:Integer); // Also sets Keyphrase (weaker)
  public
  {$IFDEF FPC311}
    constructor Create(const AFileName: string; AOptions: TIniFileOptions = []);
      override;
  {$ELSE}
      constructor Create(const AFileName: string; AEscapeLineFeeds : Boolean = False);override;
  {$ENDIF}
    destructor Destroy; override;
    // Overridden TINIFile methods
    function ReadInteger(const Section, Ident: string; Default: longint): longint;
      override;
    procedure WriteInteger(const Section, Ident: string; Value: longint); override;
    function ReadString(const Section, Ident, Default: string): string; override;
    procedure WriteString(const Section, Ident, Value: string); override;
    procedure ReadSection(const Section: string; Strings: TStrings); override;
    function ValueExists(const Section, Ident: string): Boolean; override;
    procedure DeleteKey(const Section, Ident: String); override;
{$IFDEF FPC311}
    procedure ReadSectionValues(const Section: string; Strings: TStrings;
      AOptions: TSectionValuesOptions = []); override;
{$ELSE}
    procedure ReadSectionValues(const Section: string; Strings: TStrings); override;
{$ENDIF}
     function ReadBinaryStream(const Section, Name: string; Value: TStream): Integer; override;
     procedure WriteBinaryStream(const Section, Name: string; Value: TStream); override;

    // Added functions
    // Compares TestValue's value and MD5Hash with the INI file's version
    function VerifyString(const Section, Ident, TestValue: string): boolean;
    function VerifyBool(const Section, Ident: string; TestValue: boolean): boolean;
    function VerifyInt64(const Section, Ident: string; TestValue: int64): boolean;
    function VerifyFloat(const Section, Ident: string; TestValue: double): boolean;
    function VerifyDate(const Section, Ident: string; TestValue: TDateTime): boolean;
    function VerifyTime(const Section, Ident: string; TestValue: TDateTime): boolean;
    function VerifyDateTime(const Section, Ident: string; TestValue: TDateTime): boolean;
    // VerifyInteger is more strict
    function VerifyInteger(const Section, Ident: string; TestValue: longint): boolean;

    // New functionality.  Write a whole section in one go.
    procedure WriteSectionValues(const Section: string; Strings: TStrings);
    // If SectionHashing = TRUE then MakeSectionHash is called for every write
    // Best practioe is to do it only after a bunch of writes
    procedure MakeSectionHash(const Section: string; WriteToINI: boolean = True);
    function VerifySectionHash(const Section, TestMD5Hash: string): boolean;
    // Separates key=value into Skey and sValue
    function SplitKeyValue(Const sValueEntry:String;var sKey,sValue:String):Boolean;
    // Sets property PlainTextMode to FALSE temporarily to Read/Write normally
    // I included these functions for simplicity. They work when PlainTextMode=TRUE or FALSE
    function ReadUnencryptedString(const Section, Ident, Default: string): string;
    procedure WriteUnencryptedString(const Section, Ident, Value: string);
    function ReadUnEncryptedInteger(const Section, Ident: string;
      Default: longint): longint;
    procedure WriteUnencryptedInteger(const Section, Ident: string; Value: longint);

    // Writes a standard Ident section with MD5 Hash
    // This can be done in Form.Create() on program first run
    // Or can be done during development, then distribute the prewritten INI file with the exe
    procedure WriteIdent(const sAuthor, sCopyright, sLicense, sContact: string; Force: boolean=False);
    // Read the MD5Hash from the INI file and pass it as a parameter
    // ANY tampering with the [ProgramInfo] ident section info returns a FALSE
    // Note not all Key/Values are verified (i.e. Appname etc.)
    function VerifyIdent(const sMD5Hash: string): boolean;

    {$IFDEF USE_DCPCRYPT}
      // Encrypt and Decrypt the whole file in one go.  Very secure via RC4 cipher.
      // You can call:
      // EncryptINI; (use DEFAULT_KEYPHRASE as the passkey, and delete the old file)
      // EncryptINI(FALSE); (don't delete old ini file)
      // EncryptINI(TRUE,'bananas'); (delete old file; use "bananas" as the passkey)
      // EncryptINI(TRUE,'bananas','.xxx'); (delete old file; use bananas as the passkey, and save as an .xxx file)
      // bSuccess:=EncryptINI(optional parameters); (returns FALSE if anything goes wrong)
      function EncryptINI(DeleteOld:Boolean = TRUE;sPassword: string = DEFAULT_KEYPHRASE;NewExt:String = DEFAULT_EXT):Boolean;
      function DecryptINI(DeleteOld:Boolean = TRUE;sPassword: string = DEFAULT_KEYPHRASE;NewExt:String = DEFAULT_EXT):Boolean;
    {$ENDIF}

    // Use to detect or set 'First Run' status
    function IsVirgin: boolean;
    procedure Deflower;
    procedure Reflower;

    // By Default, PlainTextMode = FALSE (Encrypted mode)
    property PlainTextMode: boolean read fPlainText write fPlainText;
    property MD5Hash: string read fMD5String write fMD5String;

    // Either..
    property KeyPhrase: string read fKeyPhrase write SetKeyPhrase; // also sets Key (preferred property)
    // or..
    property Key: longint read fKey write SetKey; // Also sets KeyPhrase (weaker)

    // If SectionHashing=TRUE then a Section hash is written each time any value
    // is written.  This enables VerifySection.
    property SectionHashing: boolean read fSectionHashing write fSectionHashing;

    property HashType:String read fHashType; // Info
    property CipherType:String read fCipherType; // Info
    property CryptINIVersion: string read fVersion; // Info
    property FileName; //inherited
{$IFNDEF FPC311}
    // Reflect changes in fpc 3x
    property EscapeLineFeeds;
    Property CaseSensitive;
    Property StripQuotes;
{$ENDIF}

  end;
resourcestring
  rsNotYetInitia = 'Not yet initialised';
  rsNothingWasEn = 'Nothing was Entered!';
  rsUnknown = 'unknown';

implementation

{$IFDEF FPC311}
  constructor TCryptIniFile.Create(const AFileName: string;
  AOptions: TIniFileOptions = []);
  {
  TIniFileOption = (ifoStripComments,    // Strip comments when reading file
                  ifoStripInvalid,     // Strip invalid lines when reading file.
                  ifoEscapeLineFeeds, // Escape linefeeds when reading file.
                  ifoCaseSensitive,   // Use Case sensitive section/key names
                  ifoStripQuotes,     // Strip quotes when reading string values.
                  ifoFormatSettingsActive, // Use format settings when writing date/float etc.
                  ifoWriteStringBoolean // Write booleans as string
                  );
  }
begin
  // AOptions := AOptions And NOT [ifoWriteStringBoolean];
  inherited Create(AFileName, AOptions);
{$ELSE}
constructor TCryptIniFile.Create(const AFileName: string; AEscapeLineFeeds : Boolean = False);
begin
  inherited Create(AFileName,AEscapeLineFeeds);
{$ENDIF}
  CacheUpdates := False;
  fPlainText := False;
  fSectionHashing := True;
  fVersion := C_VERSION;
  fMD5String := rsNotYetInitia;
  fKey := 300554; // Random default key in case user doesn't set one
  fKeyPhrase := DEFAULT_KEYPHRASE;
{$IFDEF USE_DCPCRYPT}
   DCP_idea1:=TDCP_idea.Create(Nil);
   DCP_md5_1:=TDCP_md5.Create(Nil);
  If DCP_idea1.SelfTest then
    DCP_Idea1.Burn;  { Clear all information }
  DCP_Idea1.InitStr(fKeyPhrase,TDCP_md5);
  fCipherType:=DCP_Idea1.GetAlgorithm;
  fHashType:=DCP_md5_1.GetAlgorithm;
{$ELSE}
   fHashType:='MD5';
   fCiperType:='BASE64';
{$ENDIF}
end;

// ***************************************************************************//

destructor TCryptIniFile.Destroy;
begin
  // Anything to free here?
  {$IFDEF USE_DCPCRYPT}
     DCP_idea1.Burn; // Clear key info from memory
     DCP_idea1.Free;
     DCP_md5_1.Burn; // Clear key info from memory
     DCP_md5_1.Free;
  {$ENDIF}
  inherited Destroy;
end;

// ***************************************************************************//
// Start of internal routines
// ***************************************************************************//
{$IFNDEF USE_DCPCRYPT} // Fallback routines
function B64Encode(pInput: pointer; pOutput: pointer; Size: longint): longint;
var
  i, iptr, optr: longint;
  Input, Output: PByteArray;
begin
  Input := PByteArray(pInput);
  Output := PByteArray(pOutput);
  iptr := 0;
  optr := 0;
  for i := 1 to (Size div 3) do
  begin
    Output^[optr + 0] := B64[Input^[iptr] shr 2];
    Output^[optr + 1] := B64[((Input^[iptr] and 3) shl 4) + (Input^[iptr + 1] shr 4)];
    Output^[optr + 2] := B64[((Input^[iptr + 1] and 15) shl 2) + (Input^[iptr + 2] shr 6)];
    Output^[optr + 3] := B64[Input^[iptr + 2] and 63];
    Inc(optr, 4);
    Inc(iptr, 3);
  end;
  case (Size mod 3) of
    1:
    begin
      Output^[optr + 0] := B64[Input^[iptr] shr 2];
      Output^[optr + 1] := B64[(Input^[iptr] and 3) shl 4];
      Output^[optr + 2] := byte('=');
      Output^[optr + 3] := byte('=');
    end;
    2:
    begin
      Output^[optr + 0] := B64[Input^[iptr] shr 2];
      Output^[optr + 1] := B64[((Input^[iptr] and 3) shl 4) + (Input^[iptr + 1] shr 4)];
      Output^[optr + 2] := B64[(Input^[iptr + 1] and 15) shl 2];
      Output^[optr + 3] := byte('=');
    end;
  end;
  Result := ((Size + 2) div 3) * 4;
end;

// ***************************************************************************//

function Base64Encode(const Value: ansistring): ansistring;
begin
  SetLength(Result, ((Length(Value) + 2) div 3) * 4);
  B64Encode(@Value[1], @Result[1], Length(Value));
end;

// ***************************************************************************//

function B64Decode(pInput: pointer; pOutput: pointer; Size: longint): longint;
var
  i, j, iptr, optr: longint;
  Temp: array[0..3] of byte;
  Input, Output: PByteArray;
begin
  Input := PByteArray(pInput);
  Output := PByteArray(pOutput);
  iptr := 0;
  optr := 0;
  Result := 0;
  for i := 1 to (Size div 4) do
  begin
    for j := 0 to 3 do
    begin
      case Input^[iptr] of
        65..90: Temp[j] := Input^[iptr] - Ord('A');
        97..122: Temp[j] := Input^[iptr] - Ord('a') + 26;
        48..57: Temp[j] := Input^[iptr] - Ord('0') + 52;
        43: Temp[j] := 62;
        47: Temp[j] := 63;
        61: Temp[j] := $FF;
      end;
      Inc(iptr);
    end;
    Output^[optr] := (Temp[0] shl 2) or (Temp[1] shr 4);
    Result := optr + 1;
    if (Temp[2] <> $FF) and (Temp[3] = $FF) then
    begin
      Output^[optr + 1] := (Temp[1] shl 4) or (Temp[2] shr 2);
      Result := optr + 2;
      Inc(optr);
    end
    else if (Temp[2] <> $FF) then
    begin
      try
        Output^[optr + 1] := (Temp[1] shl 4) or (Temp[2] shr 2);
        Output^[optr + 2] := (Temp[2] shl 6) or Temp[3]; // Throws error
        Result := optr + 3;
        Inc(optr, 2);
      except
        On E: Exception do
          Continue;
      end;
    end;
    Inc(optr);
  end;
end;

// ***************************************************************************//

function Base64Decode(const Value: ansistring): string;
begin
  SetLength(Result, (Length(Value) div 4) * 3);
  SetLength(Result, B64Decode(@Value[1], @Result[1], Length(Value)));
end;
{$ENDIF}
// ***************************************************************************//

function CharToBool(AChar: char): boolean;
begin
  Result := (Achar = '1');
end;

// ***************************************************************************//

function BoolToChar(ABool: boolean): char;
begin
  if ABool then
    Result := '1'
  else
    Result := '0';
end;

// ***************************************************************************//
// End of internal routines
// ***************************************************************************//

Procedure TCryptIniFile.SetKey(aValue:Integer); // Also sets KeyPhrase
begin
   fKey:=AValue;
   {$IFNDEF USE_DCPCRYPT}
     SetKeyPhrase(fKey);  // VERY WEAK!
   {$ENDIF}
end;

// ***************************************************************************//

procedure TCryptIniFile.SetKeyPhrase(AValue: string);
// Hash the phrase to a standard 32 chars
// Then sum the ASCII values to seed the fKey (it never exceeds LongInt)
// Difficult to reverse-engineer even if you have the KeyPhrase
var
  aDigest: TMDDigest; // defined in md5 unit
  iCount: integer;
  lTry: longint;
begin
  {$IFDEF USE_DCPCRYPT}
    // Use the string directly
    fKeyPhrase:=AValue;
    if fKeyPhrase = '' then
      fKeyPhrase := DEFAULT_KEYPHRASE; // don't allow empty string
    DCP_Idea1.Burn;  { Clear all stored key information }
    DCP_Idea1.InitStr(fKeyPhrase,TDCP_md5);
    Exit; // bail out
  {$ENDIF}
  lTry := 0;
  // If it's already a number, just use it. (unless it's zero!)
  if ((TryStrToInt(AValue, lTry)) and (lTry <> 0)) then
    fKey := lTry
  else // It's a string
  begin
    if AValue = '' then
      AValue := rsNothingWasEn; // don't allow empty string
    aDigest := MDString(AValue, MD_VERSION_5);
    fKeyPhrase := MDPrint(aDigest); // 32 chars
    // Mess it up to make a big enough number (96 chars)
    fKeyPhrase := fKeyPhrase + ReverseString(fKeyPhrase) + fKeyPhrase;
    fKey := 0;
    for iCount := 1 to (Length(fKeyPhrase)) do
    begin
      fKey := fKey + Ord(fKeyPhrase[iCount]);
    end;
    // Make the number bigger
    fKey := fKey * 7;
  end;
end;


// ***************************************************************************//

function TCryptIniFile.EncodeString(const sValue: string): string;
  // Muddle the String up and encode it in IDEA or Base64
var
  s: string;
begin
  s := ReverseString(sValue);
  {$IFDEF USE_DCPCRYPT}
  DCP_Idea1.Reset;{ Reset any stored chaining information }
  Result:=DCP_Idea1.EncryptString(s);
  {$ELSE}
  Result := Base64Encode(s);
  {$ENDIF}
end;

// ***************************************************************************//

function TCryptIniFile.DecodeString(const sValue: string): string;
  // Decode the IDEA or Base64 then Unmuddle
var
  s: string;
begin
  {$IFDEF USE_DCPCRYPT}
  DCP_Idea1.Reset;{ Reset any stored chaining information }
  s:=DCP_Idea1.DecryptString(sValue);
  {$ELSE}
  s := Base64Decode(sValue);
  {$ENDIF}
  Result := ReverseString(s);
end;

// ***************************************************************************//

function TCryptIniFile.EncodeInteger(const iValue: longint): longint;
begin
  //DEBUG:  ShowMessageFmt('EncodeInteger value = %d',[iValue]);
  // B64Encode(@iValue,@Result,SizeOf(iValue)); deprecated
  Result := iValue xor fKey;
  // Already IDEA or BASE64 encoded after writing
  //DEBUG:  ShowMessageFmt('EncodeInteger Result = %d',[Result]);
end;

// ***************************************************************************//

function TCryptIniFile.DecodeInteger(const iValue: longint): longint;
begin
  // B64Decode(@iValue,@Result,SizeOf(iValue)); deprecated
  // Already IDEA or BASE64 decoded before reading
  Result := iValue xor fKey;
end;

// ***************************************************************************//

procedure TCryptIniFile.Deflower;
// Called by IsVirgin to set FirstRun status to FALSE
var
  tempPlainText: boolean;
begin
  tempPlainText := fPlainText;
  fPlainText := True;
  WriteBool(IDENT_SECTION, IDENT_FIRSTRUN, False);
  fPlainText := tempPlainText;
end;

// ***************************************************************************//

procedure TCryptIniFile.Reflower;
// Set FirstRun status to TRUE
var
  tempPlainText: boolean;
begin
  tempPlainText := fPlainText;
  fPlainText := True;
  WriteBool(IDENT_SECTION, IDENT_FIRSTRUN, True);
  fPlainText := tempPlainText;
end;

// ***************************************************************************//

function TCryptIniFile.IsVirgin: boolean;
  // On FirstRun returns TRUE
  // Then calls Deflower so subsequent runs will return FALSE
var
  tempPlainText: boolean;
begin
  tempPlainText := fPlainText;
  fPlainText := True;
  Result := ReadBool(IDENT_SECTION, IDENT_FIRSTRUN, True);
  if Result = True then
    Deflower;
  fPlainText := tempPlainText;
end;

// ***************************************************************************//

function TCryptIniFile.VerifyIdent(const sMD5Hash: string): boolean;
  // The sMD5Hash has been previously generated by WriteIdent
  // and stored in INI file.  Get it from there.
var
  s: string;
  tempPlainText: boolean;
begin
  Result := False;
  tempPlainText := fPlainText; // Store old value
  fPlainText := True; // Set PlainTextMode ON
  // Make a string of all the authorship entries
  s := ReadString(IDENT_SECTION, IDENT_AUTHOR, rsUnknown);
  s := s + ReadString(IDENT_SECTION, IDENT_COPYRIGHT, rsUnknown);
  s := s + ReadString(IDENT_SECTION, IDENT_LICENSE, rsUnknown);
  s := s + ReadString(IDENT_SECTION, IDENT_CONTACT, rsUnknown);
  // Make up an MD5 hash from the INI values read
  fMyDigest := MDString(s, MD_VERSION_5);
  fMD5String := MDPrint(fMyDigest);
  // Compare it with the value passed in the parameter
  if fMD5String = sMD5Hash then
    Result := True;
  fPlainText := tempPlainText; // Restore old value
end;

// ***************************************************************************//

procedure TCryptIniFile.WriteIdent(const sAuthor, sCopyright, sLicense, sContact: string; Force: boolean=False);
// This proc will only write anything if IsVirgin (First Run status) is TRUE
// Or the INI file is empty or altered
var
  s: string;
  tempPlainText: boolean;
  VInfo:TFileVersionInfo;
  Version:TProgramVersion;
begin
  tempPlainText := fPlainText; // Store old value
  fPlainText := True; // Set PlainTextMode ON
  if ((ReadBool(IDENT_SECTION, IDENT_FIRSTRUN, True) = True) OR (Force = True)) then
  begin // IsVirgin=TRUE so write the section in plaintext
    WriteString(IDENT_SECTION, IDENT_AUTHOR, sAuthor);
    WriteString(IDENT_SECTION, IDENT_COPYRIGHT, sCopyright);
    WriteString(IDENT_SECTION, IDENT_LICENSE, sLicense);
    WriteString(IDENT_SECTION, IDENT_CONTACT, sContact);
    WriteString(IDENT_SECTION, IDENT_LCLVERSION, lcl_version);
    WriteString(IDENT_SECTION, IDENT_FPCVERSION, {$I %FPCVERSION%});
    s:={$I %FPCTARGETCPU%}+' - '+{$I %FPCTARGETOS%};
    WriteString(IDENT_SECTION, IDENT_TARGET, s);
    // If you don't need this you can delete 'Forms' from the Uses clause
    WriteString(IDENT_SECTION, IDENT_APPNAME,Application.Title);
    s:='0.0.0.0'; // Default
    VInfo:=TFileVersionInfo.Create(Application);
    TRY
      VInfo.ReadFileInfo;
      If fileinfo.GetProgramVersion(Version) then
         s:=ProgramversionToStr(Version);
    finally
      VInfo.free;
    end;
    WriteString(IDENT_SECTION,IDENT_APPVERSION,s);
    WriteString(IDENT_SECTION, IDENT_EXE, ParamStrUTF8(0));
    WriteBool(IDENT_SECTION,IDENT_FIRSTRUN,TRUE);
    s:={$I %DATE%} + ' at ' + {$I %TIME%};
    WriteString(IDENT_SECTION, IDENT_LASTCOMPILED, s);
    // Make up the MD5Hash of all the authorship entries
    s := sAuthor + sCopyright + sLicense + sContact;
    fMyDigest := MDString(s, MD_VERSION_5);
    fMD5String := MDPrint(fMyDigest);
    WriteString(IDENT_SECTION, IDENT_MD5HASH, fMD5String);
    UpdateFile;
  end;
  fPlainText := tempPlainText;  // Restore old value
end;

// ***************************************************************************//

procedure TCryptIniFile.WriteUnencryptedString(const Section, Ident, Value: string);
var
  tempPlainText: boolean;
  // Temporarily sets PlainTextMode = TRUE then restores previous status
begin
  tempPlainText := fPlainText; // Store old value
  fPlainText := True; // Set PlainTextMode ON
  WriteString(Section, Ident, Value);
  fPlainText := tempPlainText;  // Restore old value
end;

// ***************************************************************************//

function TCryptIniFile.ReadUnencryptedString(
  const Section, Ident, Default: string): string;
  // Temporarily sets PlainTextMode = TRUE then restores previous status
var
  tempPlainText: boolean;
begin
  tempPlainText := fPlainText; // Store old value
  fPlainText := True; // Set PlainTextMode ON
  Result := ReadString(Section, Ident, Default);
  fPlainText := tempPlainText;  // Restore old value
end;

// ***************************************************************************//

function TCryptIniFile.ReadUnEncryptedInteger(const Section, Ident: string;
  Default: longint): longint;
  // Temporarily sets PlainTextMode = TRUE then restores previous status
var
  tempPlainText: boolean;
begin
  tempPlainText := fPlainText; // Store old value
  fPlainText := True; // Set PlainTextMode ON
  Result := ReadInteger(Section, Ident, Default);
  fPlainText := tempPlainText;  // Restore old value
end;

// ***************************************************************************//

procedure TCryptIniFile.WriteUnencryptedInteger(const Section, Ident: string;
  Value: longint);
// Temporarily sets PlainTextMode = TRUE then restores previous status
var
  tempPlainText: boolean;
begin
  tempPlainText := fPlainText; // Store old value
  fPlainText := True; // Set PlainTextMode ON
  WriteInteger(Section, Ident, Value);
  fPlainText := tempPlainText;  // Restore old value
end;

(******************************************************************************)
// Subclass of TIniFiles to enable obfuscation of entries
(******************************************************************************)
function TCryptIniFile.ReadInteger(const Section, Ident: string;
  Default: longint): longint;
  // Unobfuscates the number if PlainTextMode=FALSE
var
  s: string;
  iTemp: longint;
begin
  if fPlainText = True then
  begin
    // Read result using TiniFile
    Result := inherited ReadInteger(Section, Ident, Default);
    Exit;
  end;
  Result := Default; // If all else fails..
  s := ReadString(Section, Ident + INTEGER_MARKER, IntToStr(Default));
  //DEBUG: ShowMessage(s);
  if s = IntToStr(Default) then
    Exit // nothing there - return Default value
  else
  begin
    if not TryStrToInt(s, iTemp) then
      Exit; // Error - return Default value
    Result := DecodeInteger(iTemp); // Un-Scramble it
  end;
end;

(******************************************************************************)

procedure TCryptIniFile.WriteInteger(const Section, Ident: string; Value: longint);
// Obfuscates the number if PlainTextMode=FALSE
begin
  if fPlainText = False then
    begin
    Value := EncodeInteger(Value); // Scramble it
    inherited WriteInteger(Section, Ident + INTEGER_MARKER, Value); // Write+ using TiniFile
    end
    else inherited WriteInteger(Section, Ident, Value); // Write using TiniFile
  if SectionHashing = True then
    MakeSectionHash(Section, True);
  UpdateFile;
end;

(******************************************************************************)

function TCryptIniFile.ReadString(const Section, Ident, Default: string): string;
  // Behaviour depends on PlainTextMode = FALSE/TRUE
var
  s: string;
begin
  fMD5String:='';
  // Read value via TIniFile
  s := inherited ReadString(Section, Ident, Default);
  Result := s;
  if fPlainText = True then // Use unencrypted version
    Exit;
  If (Length(s) < 32) then Exit;

  fMD5String := LeftStr(s, 32); // Grab the MD5 string and store (unencoded)
  Result := RightStr(s, Length(s) - 32); // Use the rest
  if Result <> EncodeString(Default) then
  begin
    Result := DecodeString(Result); // Un-Encrypt it
  end;
end;

(******************************************************************************)

function TCryptIniFile.ValueExists(const Section, Ident: string): Boolean;
// In Encrypted mode Integers have a special marker
Var s:String;
begin
  Result:= inherited ValueExists(Section,Ident); //Normal result
  if fPlainText = False then
  begin
    If Ident = '' then // bail out
    begin
      Result:=FALSE;
      exit;
    end;
    // Test against both Ident and (Ident + INTEGER_MARKER)
    s:=Ident;
    s := s + INTEGER_MARKER; // Add INTEGER_MARKER
    Result:=Result OR inherited ValueExists(Section,s);
  end;
end;

(******************************************************************************)

procedure TCryptIniFile.DeleteKey(const Section, Ident: String);
// In Encrypted mode Integers have a special marker
Var s:String;
begin
  If Ident = '' then exit; // Bail out
  inherited DeleteKey(Section,Ident); //Normal behaviour
  // Ident might have an INTEGER_MARKER so preceding code didn't delete it
  // So delete (Ident + INTEGER_MARKER) if its there
  s:=Ident; // Make a copy
  // DEBUG: ShowMessage(s);
  s := s + INTEGER_MARKER; // Add INTEGER_MARKER
  inherited DeleteKey(Section,s);
  If SectionHashing then MakeSectionHash(Section,TRUE);
  UpdateFile;
end;

(******************************************************************************)

 procedure TCryptIniFile.WriteString(const Section, Ident, Value: string);
// Writes MD5Digest string + BASE64  or IDEA string
// Behaviour depends on PlainTextMode = FALSE/TRUE
var
  s: string;
begin
  if fPlainText = True then // Write regular string
  begin
    // Use regular TiniFile
    inherited WriteString(Section, Ident, Value);
    UpdateFile;
    Exit;
  end;
  s := EncodeString(Value); // Encode the string (IDEA/BASE64)
  fMyDigest := MDString(s, MD_VERSION_5);
  fMD5String := MDPrint(fMyDigest); // Get the MD5Hash of the encoded string
  s := fMD5String + s; // Prefix it to the encoded string
  inherited WriteString(Section, Ident, s); // Write the result using TiniFile
  if SectionHashing = True then
    MakeSectionHash(Section, True);
  UpdateFile;
end;

(******************************************************************************)

Function TCryptIniFile.SplitKeyValue(Const sValueEntry:String;var sKey,sValue:String):Boolean;
Var iEqualsPos:Integer;
begin
  Result:=FALSE;
  If Length(sValueEntry) < 4 then exit;
  iEqualsPos := Pos('=', sValueEntry);
 if (iEqualsPos >= 2) then
 begin
   sKey := LeftStr(sValueEntry, iEqualsPos - 1);
   sValue := RightStr(sValueEntry, Length(sValueEntry) - iEqualsPos);
   Result:=TRUE;
 end;
end;

(******************************************************************************)

procedure TCryptIniFile.ReadSection(const Section: string; Strings: TStrings);
// If PlainText = FALSE then
// 1. Remove any INTEGER_MARKER
// 2. Remove MD5Hash entry
Var
  TempStrings:TStrings;
  iCount,iMD5HashEntry: integer;
  s: string;
begin
  inherited ReadSection(Section, Strings); // Use regular TiniFile
  if fPlainText = True then exit;// Return faithful copy

  // Deal with PlainText = FALSE
  iMD5HashEntry := -1; // dummy value
  TempStrings := TStringList.Create; // Use a copy
  TempStrings.BeginUpdate;
  try
    // Make a Copy
    TempStrings.Clear;
    TempStrings.AddStrings(Strings); // contains encrypted versions
    for iCount := 0 to Pred(TempStrings.Count) do
    begin
      // Do not retrieve MD5Hash entry
      if (Pos(IDENT_MD5HASH, TempStrings[iCount]) = 0) then
      begin
        s := TempStrings[iCount];
        If RightStr(s,1) = INTEGER_MARKER then
         TempStrings[iCount]:=LeftStr(s,Length(s)-1);
      end
      else
        iMD5HashEntry := iCount;
    end;
  finally
     TempStrings.EndUpdate;
     // Copy decoded versions back into the calling StringList
     if (iMD5HashEntry >= 0) then
       TempStrings.Delete(iMD5HashEntry); // Dont return the MD5Hash entry
     Strings.Clear;
     Strings.AddStrings(TempStrings);
     TempStrings.Free;
   end;
end;

procedure TCryptIniFile.WriteSectionValues(const Section: string; Strings: TStrings);
// Writes either normal or Encrypted values depending on property PlainTextMode
// If SectionHashing = TRUE, also writes the MD5Hash entry at the end (for VerifySection method)
var
  iCount, iTemp: integer;
  s, sKey, sValue: string;
  f: double;
  TempSectionHashing: boolean;
begin
  TempSectionHashing := fSectionHashing; // Preserve property
  fSectionHashing := False; // Write section without on-the-fly hashing
  for iCount := 0 to Strings.Count - 1 do
  begin
    s := Strings[iCount];
    // Split key and value
    If NOT SplitKeyValue(s,sKey,sValue) then continue;
    if TryStrToInt(sValue, iTemp) = True then
    begin
      WriteInteger(Section, sKey, iTemp);
      Continue;
    end;
    if ((LowerCase(sValue) = 'true') or (LowerCase(sValue) = 'false')) then
    begin
      if (LowerCase(sValue) = 'true') then
        WriteBool(Section, sKey, True)
      else
        WriteBool(Section, sKey, False);
      Continue;
    end;
    if TryStrToFloat(sValue, f) = True then
    begin
      WriteFloat(Section, sKey, f);
      Continue;
    end;
    // All other values
    WriteString(Section, sKey, sValue);
  end;
  // Now is the time to write the MD5Hash entry
  if TempSectionHashing = True then
  begin
    fSectionHashing := True;
    MakeSectionHash(Section, True);
  end;
  UpdateFile;
  fSectionHashing := TempSectionHashing; // Restore property
end;

// ***************************************************************************//
{$IFDEF FPC311}
   //   TSectionValuesOption = (svoIncludeComments,svoIncludeInvalid, svoIncludeQuotes);
    procedure TCryptIniFile.ReadSectionValues(const Section: string;
              Strings: TStrings; AOptions: TSectionValuesOptions = []);
{$ELSE}
   procedure TCryptIniFile.ReadSectionValues(const Section: string; Strings: TStrings);
{$ENDIF}
var
  TempStrings: TStrings;
  iCount, iMD5HashEntry: integer;
  s, sKey, sValue: string;
  iTemp: longint;
begin
{$IFDEF FPC311}
      inherited ReadSectionValues(Section, Strings, AOptions);
{$ELSE}
      inherited ReadSectionValues(Section, Strings);
{$ENDIF}
  if fPlainText = True then // Use regular method
  begin
    // Use regular TiniFile
    UpdateFile;
    Exit;
  end;
  iMD5HashEntry := -1; // dummy value
  TempStrings := TStringList.Create; // Use a copy
  TempStrings.BeginUpdate;
  try
    // Make a Copy
    TempStrings.Clear;
    TempStrings.AddStrings(Strings); // contains encrypted versions
    // Iterate through the list
    for iCount := 0 to Pred(TempStrings.Count) do
    begin
      // Do not retrieve MD5Hash entry
      if (Pos(IDENT_MD5HASH, TempStrings[iCount]) = 0) then
      begin
        s := TempStrings[iCount];
        // Split key and value
        If NOT SplitKeyValue(s,sKey,sValue) then Exit;
        // Deal with Integers, which are IDEA or BASE64 encoded then Encrypted
        if RightStr(sKey, 1) = INTEGER_MARKER then // It's an Integer value
        begin
          s := DecodeString(RightStr(sValue, Length(sValue) - 32));
          // Decode (IDEA/BASE64) to a string
          if TryStrToInt(s, iTemp) then
          begin
            iTemp := DecodeInteger(iTemp); // Decrypt the integer
            s := IntToStr(iTemp); // String value of decoded integer
          end;
          sKey := LeftStr(sKey, Length(sKey) - 1); // Trim off the INTEGER_MARKER
        end
        else
        begin
          // All but Integers are simply IDEA or BASE64 encoded
          s := DecodeString(RightStr(sValue, Length(sValue) - 32));
        end;
        // Replace encoded version with decoded version
        TempStrings[iCount] := sKey + '=' + s;
      end
      else
        iMD5HashEntry := iCount;
      // Grab the sKey and sValue (we only want to decrypt the Value)
    end;
  finally
    TempStrings.EndUpdate;
    // Copy decoded versions back into the calling StringList
    if (iMD5HashEntry >= 0) then
      TempStrings.Delete(iMD5HashEntry); // Dont return the MD5Hash entry
    Strings.Clear;
    Strings.AddStrings(TempStrings);
    TempStrings.Free;
  end;
end;

// ***************************************************************************//

function TCryptIniFile.ReadBinaryStream(const Section, Name: string; Value: TStream): Integer;
// Force normal TINIFile mode
Var
  tempPlainTextMode:Boolean;
  tempSectionHashing:Boolean;
begin
   tempPlainTextMode:=PlainTextMode;
   tempSectionHashing:=SectionHashing;
   PlainTextMode:=TRUE;
   SectionHashing:=FALSE;
   Result:=inherited ReadBinaryStream(Section, Name,Value);
   PlainTextMode:=tempPlainTextMode;
   SectionHashing:=tempSectionHashing;
end;

// ***************************************************************************//

procedure TCryptIniFile.WriteBinaryStream(const Section, Name: string; Value: TStream);
// Force normal TINIFile mode
Var
  tempPlainTextMode:Boolean;
  tempSectionHashing:Boolean;
begin
   tempPlainTextMode:=PlainTextMode;
   tempSectionHashing:=SectionHashing;
   PlainTextMode:=TRUE;
   SectionHashing:=FALSE;
   inherited WriteBinaryStream(Section, Name,Value);
   PlainTextMode:=tempPlainTextMode;
   SectionHashing:=tempSectionHashing;
end;

// ***************************************************************************//
// ***************************************************************************//

  procedure TCryptIniFile.MakeSectionHash(const Section: string; WriteToINI: boolean);
// VerifySection uses WriteToINI=FALSE
var
  TempStrings: TStrings;
  iCount: integer;
  s: string;
  tempPlainText: boolean;
begin
  if fSectionHashing = False then Exit;
  tempPlainText:=fPlainText; // Store value
  TempStrings := TStringList.Create;
  TempStrings.BeginUpdate;
  try
    s := '';
    ReadSectionValues(Section, TempStrings);
    for iCount := 0 to Pred(TempStrings.Count) do
      // Ignore the MD5Hash entry
      if (Pos(IDENT_MD5HASH, TempStrings[iCount]) = 0) then
        s := s + TempStrings[iCount];
    //DEBUG:  else ShowMessage('Found ' + IDENT_MD5HASH);
    fMyDigest := MDString(s, MD_VERSION_5);
    fMD5String := MDPrint(fMyDigest);
    //DEBUG: ShowMessage(fMD5String);
    TempPlainText := fPlainText; // Store old value
    fPlainText := True; // Set PlainTextMode ON
    // Default WriteToINI = TRUE
    if WriteToINI then
      WriteString(Section, IDENT_MD5HASH, fMD5String);
    fPlainText := tempPlainText;  // Restore old value
  finally
    TempStrings.EndUpdate;
    TempStrings.Free;
  end;
end;

function TCryptIniFile.VerifySectionHash(const Section, TestMD5Hash: string): boolean;
begin
  Result := False;
  if fSectionHashing = False then Exit;
  {
  if ((fSectionHashing = False) or (fPlainText = True)) then
    Exit;
  }
  fMD5String := '';
  // Rehash, but without writing in order to set fMD5String correctly
  MakeSectionHash(Section, False);
  //DEBUG: ShowMessage(fMD5String);
  //DEBUG: ShowMessage(TestMD5Hash);
  if (fMD5String = TestMD5Hash) then
    Result := True;
end;

function TCryptIniFile.VerifyString(const Section, Ident, TestValue: string): boolean;
  // Matches both value and Hash
var
  sCorrect, sCorrectMD5: string;
begin
  Result := False;
  if fPlainText = True then
    Exit;
  // Fetch the MD5 hash of the TestValue string
  sCorrect := EncodeString(TestValue);
  fMyDigest := MDString(sCorrect, MD_VERSION_5);
  sCorrectMD5 := MDPrint(fMyDigest);

  // Read the MD5 hash of the INI string (sets fMD5String) and get the value;
  fMD5String := '';
  sCorrect := ReadString(Section, Ident, '');
  // DEBUG
  {
  ShowMessageFmt('sCorrectMD5=%s, fMD5String=%s, sCorrect=%s, TestValue=%s',
  [sCorrectMD5,fMD5String,sCorrect,TestValue]);
  }
  if ((sCorrectMD5 = fMD5String) and (sCorrect = TestValue)) then
    Result := True;
end;

// ***************************************************************************//

function TCryptIniFile.VerifyInteger(const Section, Ident: string;
  TestValue: longint): boolean;
  // Matches both value and Hash
var
  iCorrect: longint;
  sCorrect, sCorrectMD5: string;
begin
  Result := False;
  if fPlainText = True then
    Exit;
  iCorrect:=0;
  if TestValue <> 0 then
    iCorrect := EncodeInteger(TestValue); // Use fKey to scramble it
  // Encode it as a string
  sCorrect := EncodeString(IntToStr(iCorrect));
  // Make an MD5 Hash of it (sets fMD5String)
  fMyDigest := MDString(sCorrect, MD_VERSION_5);
  sCorrectMD5 := MDPrint(fMyDigest);
  // Use ReadInteger to get the fMD5String of the INI file entry
  // And read the value
  iCorrect := ReadInteger(Section, Ident, 9999); // Automatically uses fKey to unscramble
  // Value and MD5Hash match?
  if ((sCorrectMD5 = fMD5String) and (iCorrect = TestValue)) then
    Result := True;
end;

// ***************************************************************************//

function TCryptIniFile.VerifyBool(const Section, Ident: string;
  TestValue: boolean): boolean;
begin
  Result := False;
  if fPlainText = True then
    Exit;
  Result := VerifyString(Section, Ident, BoolToChar(TestValue));
end;

// ***************************************************************************//

function TCryptIniFile.VerifyInt64(const Section, Ident: string;
  TestValue: int64): boolean;
begin
  Result := False;
  if fPlainText = True then
    Exit;
  Result := VerifyString(Section, Ident, IntToStr(TestValue));
end;

// ***************************************************************************//

function TCryptIniFile.VerifyFloat(const Section, Ident: string;
  TestValue: double): boolean;
begin
  Result := False;
  if fPlainText = True then
    Exit;
  Result := VerifyString(Section, Ident, FloatToStr(TestValue));
end;

// ***************************************************************************//

function TCryptIniFile.VerifyDate(const Section, Ident: string;
  TestValue: TDateTime): boolean;
begin
  Result := False;
  if fPlainText = True then
    Exit;
  Result := VerifyString(Section, Ident, DateToStr(TestValue));
end;

// ***************************************************************************//

function TCryptIniFile.VerifyTime(const Section, Ident: string;
  TestValue: TDateTime): boolean;
begin
  Result := False;
  if fPlainText = True then
    Exit;
  Result := VerifyString(Section, Ident, TimeToStr(TestValue));
end;

// ***************************************************************************//

function TCryptIniFile.VerifyDateTime(const Section, Ident: string;
  TestValue: TDateTime): boolean;
begin
  Result := False;
  if fPlainText = True then
    Exit;
  Result := VerifyString(Section, Ident, DateTimeToStr(TestValue));
end;

// ***************************************************************************//

{$IFDEF USE_DCPCRYPT}
// TODO? use Stream property (Filename='')
Function TCryptIniFile.EncryptINI(DeleteOld:Boolean = TRUE;sPassword: string = DEFAULT_KEYPHRASE;NewExt:String = DEFAULT_EXT):Boolean;
var
  Cipher: TDCP_rc4;
  Source,Dest: TFileStream;
begin
  CacheUpdates:=FALSE;
  UpdateFile;
  Result:=FALSE;
  If NOT FileExistsUTF8(Filename) then exit;
  try
    If sPassword = '' then sPassword:=fKeyPhrase;
    Source := TFileStream.Create(Filename, fmOpenRead);
    // INI.Filename, PATH SOURCE OF FILE
    Dest := TFileStream.Create(ChangeFileExt(Filename, NewExt), fmCreate);
    // ChangeFileExt(INI.Filename, NewExt), PATH DESTINATION OF FILE
    Cipher := TDCP_rc4.Create(Nil);
    Cipher.InitStr(sPassword, TDCP_sha1);
    // initialize the cipher with a hash of the passphrase
    Cipher.EncryptStream(Source, Dest, Source.Size);
    // encrypt the contents of the file
    Cipher.Burn;
    Cipher.Free;
  finally
    Dest.Free;
    Source.Free;
    Result:=TRUE;
    If DeleteOld then Result:=Result AND DeleteFile(Filename);
  end;
end;

// ***************************************************************************//

Function TCryptIniFile.DecryptINI(DeleteOld:Boolean = TRUE;sPassword: string = DEFAULT_KEYPHRASE;NewExt:String = DEFAULT_EXT):Boolean;
var
  Cipher: TDCP_rc4;
  Source,Dest: TFileStream;
begin
  Result:=FALSE;
  If NOT FileExistsUTF8(ChangeFileExt(Filename, NewExt)) then exit;
  try
    If sPassword = '' then sPassword:=fKeyPhrase;
    Source := TFileStream.Create(ChangeFileExt(Filename, NewExt), fmOpenRead);
    // ChangeFileExt(INI.Filename, NewExt), PATH SOURCE OF FILE
    Dest := TFileStream.Create(Filename, fmCreate);
    // INI.Filename, PATH DESTINATION OF FILE
    Cipher := TDCP_rc4.Create(Nil);
    Cipher.InitStr(sPassword, TDCP_sha1);
    // initialize the cipher with a hash of the passphrase
    Cipher.DecryptStream(Source, Dest, Source.Size);
    // decrypt the contents of the file
    Cipher.Burn;
    Cipher.Free;
  finally
    Dest.Free;
    Source.Free;
    Result:=TRUE;
    If DeleteOld then  Result:=Result AND DeleteFile(ChangeFileExt(Filename, NewExt));
    // Re-initialise Decrypted INI from disk
    Try
    {$IFDEF FPC311}
    Create(Filename,Options);
    {$else}
    Create(Filename,EscapeLineFeeds);
    {$endif}
    CacheUpdates:=FALSE;
    Except
    // Eat exception
      Result:=FALSE;
    end;
  end;
end;
{$ENDIF}

// ***************************************************************************//
// ENDOFFILE
// ***************************************************************************//

end.
