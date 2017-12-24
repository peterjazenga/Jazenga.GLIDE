{**********************************************************************                   
 Package pl_Astronomy.pkg
 This unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}


unit StBase;

interface

uses
  LCLIntf, LCLType, LMessages,
  Classes, SysUtils, Messages, StdCtrls,
  StConst;

const
  {.Z+}
  StMaxBlockSize = MaxLongInt;
{.Z-}

type
  {!!.01 - moved from StBase.pas }
  TStLineTerminator = ( {possible line terminators...}
    ltNone,            {..no terminator, ie fixed length lines}
    ltCR,              {..carriage return (#13)}
    ltLF,              {..line feed (#10)}
    ltCRLF,            {..carriage return/line feed (#13/#10)}
    ltOther);          {..another character}
{!!.01 - end moved }

type

  TStHwnd = HWND;

{-SysTools exception class tree}
type
  EStException = class(Exception)     {ancestor to all SysTools exceptions}
  protected {private}
    FErrorCode: longint;

  public
    constructor CreateResTP(Ident: longint; Dummy: word);
    constructor CreateResFmtTP(Ident: longint; const Args: array of const; Dummy: word);
    property ErrorCode: longint read FErrorCode write FErrorCode;
  end;

  EStExceptionClass = class of EStException;

  EStContainerError = class(EStException);   {container exceptions}
  EStSortError = class(EStException);        {sorting exceptions}
  EStRegIniError = class(EStException);      {registry/INI file exceptions}
  EStBCDError = class(EStException);         {Bcd exceptions}
  EStStringError = class(EStException);      {String class exceptions}
  EStVersionInfoError = class(EStException); {Version info exception}
  EStNetException = class(EStException);     {Network exception}
  EStBarCodeError = class(EStException);     {BarCode exception}
  EStPNBarCodeError = class(EStException);   {PostNet BarCode exception}
  EStStatError = class(EStException);        {statistics exceptions}
  EStFinError = class(EStException);         {Financial exceptions}
  EStMimeError = class(EStException);        {Mime exceptions}
  EStToHTMLError = class(EStException);      {ToHTML exceptions}
  EStSpawnError = class(EStException);       {SpawnApplication errors}
  EStMMFileError = class(EStException);      {MemoryMappedFile errors}
  EStBufStreamError = class(EStException);    {Buffered stream errors}
  EStRegExError = class(EStException);       {RegEx errors}
  EStDecMathError = class(EStException);     {Decimal math errors}
  EStPRNGError = class(EStException);        {Random number errors}

  EStExprError = class(EStException) {expression evaluator exceptions}
  protected {private}
    FErrorCol: integer;
  public
    constructor CreateResTPCol(Ident: longint; Column: integer; Dummy: integer);
    property ErrorColumn: integer
      {-Returns the string position at the start of the token where
        the error was detected} read FErrorCol;
  end;


const
  {.Z+}
  StMaxFileLen = 260;

  StRLEMaxCount = 127;      { Used by RLE }
  StRLERunMode = $80;       { Used by RLE }
{.Z-}

const
  {.Z+}
  {used by CompareLetterSets for estimating word similarity}
  StLetterValues: array['A'..'Z'] of byte = (
    3 {A}, 6 {B}, 5 {C}, 4 {D}, 3 {E}, 5 {F}, 5 {G}, 4 {H}, 3 {I},
    8 {J}, 7 {K}, 4 {L}, 5 {M}, 3 {N}, 3 {O}, 5 {P}, 7 {Q}, 4 {R},
    3 {S}, 3 {T}, 4 {U}, 6 {V}, 5 {W}, 8 {X}, 8 {Y}, 9 {Z});

  StHexDigits: array[0..$F] of AnsiChar = '0123456789ABCDEF';
  DosDelimSet: set of AnsiChar = ['\', ':', #0];
{$IFDEF VERSION4}{ Delphi/Builder 3 doesn't like widestring typed constants }
  StHexDigitsW: WideString = '0123456789ABCDEF';
  DosDelimSetW: WideString = '\:';
{$ENDIF}

{.Z-}

type
  {.Z+}
  TSmallArrayA = array[0..StMaxFileLen - 1] of AnsiChar;
  TSmallArray = array[0..StMaxFileLen - 1] of char;
  BTable = array[0..255] of byte;  {Table used by Boyer-Moore search routines}
  {$IFDEF UNICODE}
  BTableU = array[0..$FFFF] of byte;
  {$ENDIF}
{.Z-}

type
  {.Z+}
  PDouble = ^double;
  TDoubleArray = array[0..(stMaxBlockSize div SizeOf(double)) - 1] of double;
  PDoubleArray = ^TDoubleArray;
  TIntArray = array[0..(StMaxBlockSize div SizeOf(integer)) - 1] of integer;
  PIntArray = ^TIntArray;
{.Z-}

type
  {the SysTools floating point type}
    {$IFOPT N+}
  TStFloat = extended;
    {$ELSE}
  TStFloat = real;
    {$ENDIF}

const
  WMCOPYID: DWORD = $AFAF;

type
  TStNode = class(TPersistent)
    {.Z+}
  protected {private}
    FData: Pointer;
    {.Z-}
  public
    constructor Create(AData: Pointer);
      virtual;
    property Data: Pointer read FData write FData;
  end;

  {.Z+}
  TStNodeClass = class of TStNode;
  {.Z-}

  TStContainer = class;

  TCompareFunc =
    function(Data1, Data2: Pointer): integer;
  TStCompareEvent =
    procedure(Sender: TObject; Data1, Data2: Pointer; var Compare: integer) of object;

  TDisposeDataProc =
    procedure(Data: Pointer);
  TStDisposeDataEvent =
    procedure(Sender: TObject; Data: Pointer) of object;

  TLoadDataFunc =
    function(Reader: TReader): Pointer;
  TStLoadDataEvent =
    procedure(Sender: TObject; Reader: TReader; var Data: Pointer) of object;

  TStoreDataProc =
    procedure(Writer: TWriter; Data: Pointer);
  TStStoreDataEvent =
    procedure(Sender: TObject; Writer: TWriter; Data: Pointer) of object;

  TStringCompareFunc =
    function(const String1, String2: string): integer;
  TStStringCompareEvent =
    procedure(Sender: TObject; const String1, String2: string; var Compare: integer) of object;

  TUntypedCompareFunc =
    function(const El1, El2): integer;
  TStUntypedCompareEvent =
    procedure(Sender: TObject; const El1, El2; var Compare: integer) of object;

  TIterateFunc =
    function(Container: TStContainer; Node: TStNode; OtherData: Pointer): boolean;
  TIteratePointerFunc =
    function(Container: TStContainer; Data, OtherData: Pointer): boolean;
  TIterateUntypedFunc =
    function(Container: TStContainer; var Data; OtherData: Pointer): boolean;

  TStContainer = class(TPersistent)
    {.Z+}
  protected {private}
    {property instance variables}
    FCompare: TCompareFunc;
    FDisposeData: TDisposeDataProc;
    FLoadData: TLoadDataFunc;
    FStoreData: TStoreDataProc;

    {event variables}
    FOnCompare: TStCompareEvent;
    FOnDisposeData: TStDisposeDataEvent;
    FOnLoadData: TStLoadDataEvent;
    FOnStoreData: TStStoreDataEvent;

    {private instance variables}
    {$IFDEF ThreadSafe}
    conThreadSafe: TRTLCriticalSection;
    {$ENDIF}

    procedure SetCompare(C: TCompareFunc);
    procedure SetDisposeData(D: TDisposeDataProc);
    procedure SetLoadData(L: TLoadDataFunc);
    procedure SetStoreData(S: TStoreDataProc);

  protected
    conNodeClass: TStNodeClass;
    conNodeProt: integer;
    FCount: longint;

    {protected undocumented methods}
    function AssignPointers(Source: TPersistent; AssignData: TIteratePointerFunc): boolean;
    function AssignUntypedVars(Source: TPersistent; AssignData: TIterateUntypedFunc): boolean;
    procedure ForEachPointer(Action: TIteratePointerFunc; OtherData: Pointer);
      virtual;
    procedure ForEachUntypedVar(Action: TIterateUntypedFunc; OtherData: pointer);
      virtual;
    procedure GetArraySizes(var RowCount, ColCount, ElSize: cardinal);
      virtual;
    procedure SetArraySizes(RowCount, ColCount, ElSize: cardinal);
      virtual;
    function StoresPointers: boolean;
      virtual;
    function StoresUntypedVars: boolean;
      virtual;

    {protected documented}
    procedure IncNodeProtection;
    {-Prevent container Destroy from destroying its nodes}
    procedure DecNodeProtection;
    {-Allow container Destroy to destroy its nodes}
    procedure EnterCS;
    {-Enter critical section for this instance}
    procedure LeaveCS;
    {-Leave critical section for this instance}
    {.Z-}
  public
    constructor CreateContainer(NodeClass: TStNodeClass; Dummy: integer);
    {-Create an abstract container (called by descendants)}
    destructor Destroy;
      override;
    {-Destroy a collection, and perhaps its nodes}
    procedure Clear;
      virtual; abstract;
    {-Remove all elements from collection}
    procedure DisposeNodeData(P: TStNode);
    {-Destroy the data associated with a node}

    {wrapper methods for using events or proc/func pointers}
    function DoCompare(Data1, Data2: Pointer): integer;
      virtual;
    procedure DoDisposeData(Data: Pointer);
      virtual;
    function DoLoadData(Reader: TReader): Pointer;
      virtual;
    procedure DoStoreData(Writer: TWriter; Data: Pointer);
      virtual;

    procedure LoadFromFile(const FileName: string);
      dynamic;
    {-Create a container and its data from a file}
    procedure LoadFromStream(S: TStream);
      dynamic; abstract;
    {-Create a container and its data from a stream}
    procedure StoreToFile(const FileName: string);
      dynamic;
    {-Create a container and its data from a file}
    procedure StoreToStream(S: TStream);
      dynamic; abstract;
    {-Write a container and its data to a stream}

    property Count: longint
    {-Return the number of elements in the collection} read FCount;

    property Compare: TCompareFunc
    {-Set or read the node comparison function} read FCompare write SetCompare;

    property DisposeData: TDisposeDataProc
    {-Set or read the node data dispose function} read FDisposeData write SetDisposeData;

    property LoadData: TLoadDataFunc
    {-Set or read the node data load function} read FLoadData write SetLoadData;

    property StoreData: TStoreDataProc
    {-Set or read the node data load function} read FStoreData write SetStoreData;

    {events}
    property OnCompare: TStCompareEvent read FOnCompare write FOnCompare;

    property OnDisposeData: TStDisposeDataEvent read FOnDisposeData write FOnDisposeData;

    property OnLoadData: TStLoadDataEvent read FOnLoadData write FOnLoadData;

    property OnStoreData: TStStoreDataEvent read FOnStoreData write FOnStoreData;
  end;

  TAssignRowData = record
    RowNum: integer;
    Data: array [0..0] of byte;
  end;

  {.Z+}
  { base component for SysTools non-visual components}
  TStComponent = class(TComponent)
  protected {private}
  end;

  { base component for TStExpressionEdit component }
  TStBaseEdit = class(TEdit)
  end;

{.Z-}

{---Generic node routines---}
function DestroyNode(Container: TStContainer; Node: TStNode; OtherData: Pointer): boolean;
{-Generic function to pass to iterator to destroy a container node}


{---WIN32 short string routines---}
{$IFDEF WStrings}
function AnsiUpperCaseShort32(const S: string): string;
{-Ansi uppercase for H- strings in WIN32}

function AnsiCompareTextShort32(const S1, S2: string): integer;
{-Case-insensitive compare function for H- strings in WIN32}

function AnsiCompareStrShort32(const S1, S2: string): integer;
{-Case-sensitive compare function for H- strings in WIN32}
{$ENDIF}


{.Z+}

procedure HugeFillChar(var Dest; Count: longint; Value: byte);
{-Fill huge memory block with byte value}

procedure HugeMove(const Src; var Dest; Count: longint);
{-Copy huge memory block to another}

procedure HugeGetMem(var P: Pointer; Size: longint);
{-Get huge memory block allocation}

procedure HugeFreeMem(var P: Pointer; Size: longint);
{-Free huge memory block allocation}
{.Z-}


{---Miscellaneous---}

{.Z+}
function IsOrInheritsFrom(Root, Candidate: TClass): boolean;
{-Return true if the classes are equal or Candidate is a descendant of Root}

procedure RaiseContainerError(Code: longint);
{-Internal routine: raise an exception for a container}

procedure RaiseContainerErrorFmt(Code: longint; Data: array of const);
{-Internal routine: raise an exception for a container}

{$IFNDEF HStrings}
function StNewStr(S: string): PShortString;
{-Allocate a short string on the heap}

procedure StDisposeStr(PS: PShortString);
{-Deallocate a short string from the heap}
{$ENDIF}

{general routine to raise a specific class of SysTools exception}
procedure RaiseStError(ExceptionClass: EStExceptionClass; Code: longint);
{.Z-}

{.Z+}
{general routines to raise a specific Win32 exception in SysTools}
procedure RaiseStWin32Error(ExceptionClass: EStExceptionClass; Code: longint);
procedure RaiseStWin32ErrorEx(ExceptionClass: EStExceptionClass; Code: longint; Info: string);
{.Z-}

{$IFDEF VERSION3ONLY}
var
  StHexDigitsW: WideString;
  DosDelimSetW: WideString;
{$ENDIF}


implementation

procedure RaiseStError(ExceptionClass: EStExceptionClass; Code: longint);
var
  E: EStException;
begin
  E := ExceptionClass.CreateResTP(Code, 0);
  E.ErrorCode := Code;
  raise E;
end;

procedure RaiseStWin32Error(ExceptionClass: EStExceptionClass; Code: longint);
var
  E: EStException;
begin
  E := ExceptionClass.Create(SysErrorMessage(Code));
  E.ErrorCode := Code;
  raise E;
end;

procedure RaiseStWin32ErrorEx(ExceptionClass: EStExceptionClass; Code: longint; Info: string);
var
  E: EStException;
begin
  E := ExceptionClass.Create(SysErrorMessage(Code) + ' [' + Info + ']');
  E.ErrorCode := Code;
  raise E;
end;

constructor EStException.CreateResTP(Ident: longint; Dummy: word);
begin
  inherited Create(SysToolsStr(Ident));
end;

constructor EStException.CreateResFmtTP(Ident: longint; const Args: array of const; Dummy: word);
begin
  inherited CreateFmt(SysToolsStr(Ident), Args);
end;

constructor EStExprError.CreateResTPCol(Ident: longint; Column: integer; Dummy: integer);
begin
  inherited CreateResTP(Ident, 0);

  FErrorCol := Column;
end;


function AbstractCompare(Data1, Data2: Pointer): integer; far;
begin
  raise ESTContainerError.CreateResTP(stscNoCompare, 0);
end;

{$IFDEF WStrings}
function AnsiCompareStrShort32(const S1, S2: ansistring): integer; assembler;
asm
         PUSH    ESI
         PUSH    EDI
         MOV     ESI,S1
         MOV     EDI,S2
         XOR     EAX,EAX
         XOR     EDX,EDX
         XOR     ECX,ECX
         MOV     DL,[ESI]
         INC     ESI
         MOV     DH,[EDI]
         INC     EDI
         MOV     CL,DL
         CMP     CL,DH
         JBE     @1
         MOV     CL,DH
         @1:
         OR      ECX, ECX
         JE      @CheckLengths
         REPE    cmpsb
         JB      @LT
         JA      @GT
         @CheckLengths:
         CMP     DL, DH
         JE      @Exit
         JB      @LT
         @GT:
         INC     EAX
         INC     EAX
         @LT:
         DEC     EAX
         @Exit:
         POP     EDI
         POP     ESI
end;

function AnsiCompareTextShort32(const S1, S2: string): integer;
begin
  Result := AnsiCompareStrShort32(AnsiUpperCaseShort32(S1), AnsiUpperCaseShort32(S2));
end;

function AnsiUpperCaseShort32(const S: string): string;
begin
  Result := S;
  AnsiUpperBuff(PChar(@Result[1]), Length(S));
end;

{$ENDIF}

function DestroyNode(Container: TStContainer; Node: TStNode; OtherData: Pointer): boolean;
begin
  Container.DisposeNodeData(Node);
  Node.Free;
  Result := True;
end;

procedure HugeFillChar(var Dest; Count: longint; Value: byte);
begin
  FillChar(Dest, Count, Value);
end;

procedure HugeFreeMem(var P: Pointer; Size: longint);
begin
  if Assigned(P) then
  begin
    FreeMem(P, Size);
    P := nil;
  end;
end;

procedure HugeGetMem(var P: Pointer; Size: longint);
begin
  GetMem(P, Size);
end;

procedure HugeMove(const Src; var Dest; Count: longint);
begin
  Move(Src, Dest, Count);
end;

{---------------------------------------------------}


function IsOrInheritsFrom(Root, Candidate: TClass): boolean;
begin
  Result := (Root = Candidate) or Candidate.InheritsFrom(Root);
end;

procedure RaiseContainerError(Code: longint);
var
  E: ESTContainerError;
begin
  E := ESTContainerError.CreateResTP(Code, 0);
  E.ErrorCode := Code;
  raise E;
end;

procedure RaiseContainerErrorFmt(Code: longint; Data: array of const);
var
  E: ESTContainerError;
begin
  E := ESTContainerError.CreateResFmtTP(Code, Data, 0);
  E.ErrorCode := Code;
  raise E;
end;

{$IFNDEF HStrings}
function StNewStr(S: string): PShortString;
begin
  GetMem(Result, succ(length(S)));
  Result^ := S;
end;

procedure StDisposeStr(PS: PShortString);
begin
  if (PS <> nil) then
    FreeMem(PS, succ(length(PS^)));
end;

{$ENDIF}

{----------------------------------------------------------------------}

constructor TStNode.Create(AData: Pointer);
begin
  Data := AData;
end;

{----------------------------------------------------------------------}

function TStContainer.AssignPointers(Source: TPersistent; AssignData: TIteratePointerFunc): boolean;
begin
  Result := False;
  if (Source is TStContainer) then
    if TStContainer(Source).StoresPointers then
    begin
      Clear;
      TStContainer(Source).ForEachPointer(AssignData, Self);
      Result := True;
    end;
end;

function TStContainer.AssignUntypedVars(Source: TPersistent; AssignData: TIterateUntypedFunc): boolean;
var
  RowCount: cardinal;
  ColCount: cardinal;
  ElSize: cardinal;
begin
  Result := False;
  if (Source is TStContainer) then
    if TStContainer(Source).StoresUntypedVars then
    begin
      Clear;
      TStContainer(Source).GetArraySizes(RowCount, ColCount, ElSize);
      SetArraySizes(RowCount, ColCount, ElSize);
      TStContainer(Source).ForEachUntypedVar(AssignData, Self);
      Result := True;
    end;
end;

procedure TStContainer.ForEachPointer(Action: TIteratePointerFunc; OtherData: pointer);
begin
  {do nothing}
end;

procedure TStContainer.ForEachUntypedVar(Action: TIterateUntypedFunc; OtherData: pointer);
begin
  {do nothing}
end;

procedure TStContainer.GetArraySizes(var RowCount, ColCount, ElSize: cardinal);
begin
  RowCount := 0;
  ColCount := 0;
  ElSize := 0;
end;

procedure TStContainer.SetArraySizes(RowCount, ColCount, ElSize: cardinal);
begin
  {do nothing}
end;

procedure TStContainer.SetCompare(C: TCompareFunc);
begin
  FCompare := C;
end;

procedure TStContainer.SetDisposeData(D: TDisposeDataProc);
begin
  FDisposeData := D;
end;

procedure TStContainer.SetLoadData(L: TLoadDataFunc);
begin
  FLoadData := L;
end;

procedure TStContainer.SetStoreData(S: TStoreDataProc);
begin
  FStoreData := S;
end;

function TStContainer.StoresPointers: boolean;
begin
  Result := False;
end;

function TStContainer.StoresUntypedVars: boolean;
begin
  Result := False;
end;

constructor TStContainer.CreateContainer(NodeClass: TStNodeClass; Dummy: integer);
begin
{$IFDEF ThreadSafe}
  Windows.InitializeCriticalSection(conThreadSafe);
{$ENDIF}

  FCompare := @AbstractCompare;
  conNodeClass := NodeClass;

  inherited Create;
end;

procedure TStContainer.DecNodeProtection;
begin
  Dec(conNodeProt);
end;

destructor TStContainer.Destroy;
begin
  if conNodeProt = 0 then
    Clear;
{$IFDEF ThreadSafe}
  Windows.DeleteCriticalSection(conThreadSafe);
{$ENDIF}
  inherited Destroy;
end;

procedure TStContainer.DisposeNodeData(P: TStNode);
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if Assigned(P) then
      DoDisposeData(P.Data);
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

function TStContainer.DoCompare(Data1, Data2: Pointer): integer;
begin
  Result := 0;
  if Assigned(FOnCompare) then
    FOnCompare(Self, Data1, Data2, Result)
  else if Assigned(FCompare) then
    Result := FCompare(Data1, Data2);
end;

procedure TStContainer.DoDisposeData(Data: Pointer);
begin
  if Assigned(FOnDisposeData) then
    FOnDisposeData(Self, Data)
  else if Assigned(FDisposeData) then
    FDisposeData(Data);
end;

function TStContainer.DoLoadData(Reader: TReader): Pointer;
begin
  Result := nil;
  if Assigned(FOnLoadData) then
    FOnLoadData(Self, Reader, Result)
  else if Assigned(FLoadData) then
    Result := FLoadData(Reader)
  else
    RaiseContainerError(stscNoLoadData);
end;

procedure TStContainer.DoStoreData(Writer: TWriter; Data: Pointer);
begin
  if Assigned(FOnStoreData) then
    FOnStoreData(Self, Writer, Data)
  else if Assigned(FStoreData) then
    FStoreData(Writer, Data)
  else
    RaiseContainerError(stscNoStoreData);
end;

procedure TStContainer.EnterCS;
begin
{$IFDEF ThreadSafe}
  EnterCriticalSection(conThreadSafe);
{$ENDIF}
end;

procedure TStContainer.IncNodeProtection;
begin
  Inc(conNodeProt);
end;

procedure TStContainer.LeaveCS;
begin
{$IFDEF ThreadSafe}
  LeaveCriticalSection(conThreadSafe);
{$ENDIF}
end;

procedure TStContainer.LoadFromFile(const FileName: string);
var
  S: TStream;
begin
  S := TFileStream.Create(FileName, fmOpenRead + fmShareDenyWrite);
  try
    LoadFromStream(S);
  finally
    S.Free;
  end;
end;

procedure TStContainer.StoreToFile(const FileName: string);
var
  S: TStream;
begin
  S := TFileStream.Create(FileName, fmCreate);
  try
    StoreToStream(S);
  finally
    S.Free;
  end;
end;




initialization
{$IFDEF VERSION3ONLY}{ Delphi/Builder 3 doesn't like widestring typed constants }
  StHexDigitsW := '0123456789ABCDEF';
  DosDelimSetW := '\:';
{$ENDIF}
end.



