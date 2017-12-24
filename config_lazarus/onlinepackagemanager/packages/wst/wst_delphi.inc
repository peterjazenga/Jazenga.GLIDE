{$IFDEF DELPHI}
  type
  {$IFDEF CompilerVersion}
    {$IF ( CompilerVersion > 16.0 )}
      QWord = UInt64;
    {$IFEND}
  {$ENDIF CompilerVersion}
  {$IF Not Declared(QWord) }
    QWord = type Int64;
  {$IFEND}
    DWORD = LongWord;
    PByteArray = ^ByteArray;
    ByteArray = array[0..$effffff] of Byte;
  {$IFDEF CompilerVersion}
    {$IF ( CompilerVersion > 19.0 )}   //Delphi 2007=18.5; Delphi 2009 = 20
      PtrInt  = NativeInt;
      PtrUInt = NativeUInt;
    {$IFEND}
  {$ENDIF CompilerVersion}
  {$IF Not Declared(PtrInt) }
      PtrInt = Integer;
      PtrUInt = Cardinal;
  {$IFEND}
    SizeInt = PtrInt;
    UnicodeChar = WideChar;
{$ENDIF DELPHI}
