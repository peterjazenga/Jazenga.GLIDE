{%MainUnit ImagingCore.pas}


type
  TMemoryIORec = record
    Data: ImagingCore.PByteArray;  //=== ct9999 ========
    Position: LongInt;
    Size: LongInt;
  end;
  PMemoryIORec = ^TMemoryIORec;

var
  OriginalFileIO: TIOFunctions;
  FileIO: TIOFunctions;
  StreamIO: TIOFunctions;
  MemoryIO: TIOFunctions;

{ Helper function that returns size of input (from current position to the end)
  represented by Handle (and opened and operated on by members of IOFunctions).}
function GetInputSize(const IOFunctions: TIOFunctions; Handle: TImagingHandle): Int64;
{ Helper function that initializes TMemoryIORec with given params.}
function PrepareMemIO(Data: Pointer; Size: LongInt): TMemoryIORec;
{ Reads one text line from input (CR+LF, CR, or LF as line delimiter).}
function ReadLine(const IOFunctions: TIOFunctions; Handle: TImagingHandle;
  out Line: AnsiString; FailOnControlChars: Boolean = False): Boolean;
{ Writes one text line to input with optional line delimiter.}
procedure WriteLine(const IOFunctions: TIOFunctions; Handle: TImagingHandle;
  const Line: AnsiString; const LineEnding: AnsiString = sLineBreak);

type
  TReadMemoryStream = class(TCustomMemoryStream)
  public
    constructor Create(Data: Pointer; Size: Integer);
    class function CreateFromIOHandle(const IOFunctions: TIOFunctions; Handle: TImagingHandle): TReadMemoryStream;
  end;

  TImagingIOStream = class(TStream)
  private
    FIO: TIOFunctions;
    FHandle: TImagingHandle;
  public
    constructor Create(const IOFunctions: TIOFunctions; Handle: TImagingHandle);
  end;
