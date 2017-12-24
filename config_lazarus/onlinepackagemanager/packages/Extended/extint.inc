{$ifdef FPC}
  {$ifdef CPU64}
    {$define PUREPASCAL}
  {$endif}
{$else}
type
  /// a CPU-dependent unsigned integer type cast of a pointer / register
  // - used for 64 bits compatibility, native under Free Pascal Compiler
  PtrUInt = cardinal;
  /// a CPU-dependent unsigned integer type cast of a pointer of pointer
  // - used for 64 bits compatibility, native under Free Pascal Compiler
  PPtrUInt = ^PtrUInt;
 
  /// a CPU-dependent signed integer type cast of a pointer / register
  // - used for 64 bits compatibility, native under Free Pascal Compiler
  PtrInt = integer;
  /// a CPU-dependent signed integer type cast of a pointer of pointer
  // - used for 64 bits compatibility, native under Free Pascal Compiler
  PPtrInt = ^PtrInt;
 
  /// unsigned Int64 doesn't exist under Delphi, but is defined in FPC
  QWord = Int64;
{$endif}

