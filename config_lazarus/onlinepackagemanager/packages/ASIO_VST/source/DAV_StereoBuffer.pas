unit DAV_StereoBuffer;

interface

{$I DAV_Compiler.inc}

uses
  DAV_Types;

type
  TStereoBuffer = class
  private
    FOutput     : array [0..1] of PDAVSingleFixedArray;
    FBufferPos  : array [0..1] of Integer;
    FBufferSize : Integer;
    procedure SetBufferSize(const Value: Integer);
    procedure BufferSizeChanged;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Append(Channel: Cardinal; Value: Single);
    procedure Reset;
    procedure Clear;

    property BufferSize: Integer read FBufferSize write SetBufferSize;
    property OutputLeft: PDAVSingleFixedArray read FOutput[0];
    property OutputRight: PDAVSingleFixedArray read FOutput[1];
  end;

implementation

const
  COutputBufferSize  = 1152; // max. 2 * 1152 samples per frame

constructor TStereoBuffer.Create;
begin
 inherited;
 FBufferSize := COutputBufferSize;
 GetMem(FOutput[0], FBufferSize * SizeOf(Single));
 GetMem(FOutput[1], FBufferSize * SizeOf(Single));
 Reset;
end;

destructor TStereoBuffer.Destroy;
begin
 Dispose(FOutput[0]);
 Dispose(FOutput[1]);
 inherited;
end;

procedure TStereoBuffer.Append(Channel: Cardinal; Value: Single);
begin
 FOutput[Channel, FBufferPos[Channel]] := Value;
 FBufferPos[Channel] := FBufferPos[Channel] + 1;
 assert(FBufferPos[Channel] <= FBufferSize);
end;

procedure TStereoBuffer.Clear;
begin
 FillChar(FOutput[0]^, FBufferSize * SizeOf(Single), 0);
 FillChar(FOutput[1]^, FBufferSize * SizeOf(Single), 0);
end;

procedure TStereoBuffer.Reset;
begin
 FBufferPos[0] := 0;
 FBufferPos[1] := 0;
end;

procedure TStereoBuffer.SetBufferSize(const Value: Integer);
begin
 if FBufferSize <> Value then
  begin
   FBufferSize := Value;
   BufferSizeChanged;
  end;
end;

procedure TStereoBuffer.BufferSizeChanged;
begin
 ReallocMem(FOutput[0], FBufferSize * SizeOf(Single));
 ReallocMem(FOutput[1], FBufferSize * SizeOf(Single));
end;

end.
