unit DAV_ProcessingComponent;

interface

{$I DAV_Compiler.inc}

uses
  Classes, DAV_Types;

type
  TDspBaseProcessFuncS   = procedure(var Data: Single; const channel: integer) of object;
  TDspBaseProcessFuncD   = procedure(var Data: Double; const channel: integer) of object;
  TDspBaseProcessFuncSA  = procedure(var ProcessBuffer: TDAVSingleDynArray; const channel, SampleFrames: integer) of object;
  TDspBaseProcessFuncDA  = procedure(var ProcessBuffer: TDAVDoubleDynArray; const channel, SampleFrames: integer) of object;
  TDspBaseProcessFuncSAA = procedure(var ProcessBuffer: TDAVArrayOfSingleDynArray; const SampleFrames: integer) of object;
  TDspBaseProcessFuncDAA = procedure(var ProcessBuffer: TDAVArrayOfDoubleDynArray; const SampleFrames: integer) of object;

  TDAVProcessingComponent = class(TComponent)
  protected
    FBypass:     Boolean;
    FEnabled:    Boolean;
    FSampleRate: Single;
    FChannels:   Integer;

    FTrailingSamples: Integer;

    FProcessS:   TDspBaseProcessFuncS;
    FProcessD:   TDspBaseProcessFuncD;
    FProcessSA:  TDspBaseProcessFuncSA;
    FProcessDA:  TDspBaseProcessFuncDA;
    FProcessSAA: TDspBaseProcessFuncSAA;
    FProcessDAA: TDspBaseProcessFuncDAA;

    FProcessQueueS:   TDspBaseProcessFuncS;
    FProcessQueueD:   TDspBaseProcessFuncD;
    FProcessQueueSA:  TDspBaseProcessFuncSA;
    FProcessQueueDA:  TDspBaseProcessFuncDA;
    FProcessQueueSAA: TDspBaseProcessFuncSAA;
    FProcessQueueDAA: TDspBaseProcessFuncDAA;

    function GetTrailingSamplesQueue: integer; virtual; abstract;

    procedure SetBypass(const Value: Boolean); virtual; abstract;
    procedure SetEnabled(const Value: Boolean); virtual; abstract;
    procedure SetSampleRate(const Value: Single); virtual; abstract;
    procedure SetChannels(const Value: Integer); virtual; abstract;
    procedure SetTrailingSamples(const Value: Integer); virtual; abstract;
  public
    procedure Init; virtual; abstract;
    procedure Reset; virtual; abstract;
    procedure ResetQueue; virtual; abstract;

    procedure NoteOff; virtual; abstract;
    procedure NoteOffQueue; virtual; abstract;

    procedure ProcessMidiEvent(MidiEvent: TDAVMidiEvent; var FilterEvent: Boolean); virtual; abstract;
    procedure ProcessMidiEventQueue(MidiEvent: TDAVMidiEvent; var FilterEvent: Boolean); virtual; abstract;

    
    property Enabled: Boolean   read FEnabled    write SetEnabled    default true;
    property Bypass: Boolean    read FBypass     write SetBypass     default true;
    property Channels: Integer  read FChannels   write SetChannels   default 2;
    property SampleRate: Single read FSampleRate write SetSampleRate;    

    property TrailingSamples: Integer read FTrailingSamples write SetTrailingSamples default 0;
    property TrailingSamplesQueue: Integer read GetTrailingSamplesQueue;

    property ProcessS:   TDspBaseProcessFuncS   read FProcessS;
    property ProcessD:   TDspBaseProcessFuncD   read FProcessD;
    property ProcessSA:  TDspBaseProcessFuncSA  read FProcessSA;
    property ProcessDA:  TDspBaseProcessFuncDA  read FProcessDA;
    property ProcessSAA: TDspBaseProcessFuncSAA read FProcessSAA;
    property ProcessDAA: TDspBaseProcessFuncDAA read FProcessDAA;

    property ProcessQueueS:   TDspBaseProcessFuncS   read FProcessQueueS;
    property ProcessQueueD:   TDspBaseProcessFuncD   read FProcessQueueD;
    property ProcessQueueSA:  TDspBaseProcessFuncSA  read FProcessQueueSA;
    property ProcessQueueDA:  TDspBaseProcessFuncDA  read FProcessQueueDA;
    property ProcessQueueSAA: TDspBaseProcessFuncSAA read FProcessQueueSAA;
    property ProcessQueueDAA: TDspBaseProcessFuncDAA read FProcessQueueDAA;
  end;

  TDAVProcessingComponentList = class(TList)
  protected
    function Get(Index: Integer): TDAVProcessingComponent;
    procedure Put(Index: Integer; Item: TDAVProcessingComponent);

    function GetTrailingSamplesQueue: integer;
  public
    function Add(Item: TDAVProcessingComponent): Integer;
    function Extract(Item: TDAVProcessingComponent): TDAVProcessingComponent;
    function First: TDAVProcessingComponent;
    function IndexOf(Item: TDAVProcessingComponent): Integer;
    function Last: TDAVProcessingComponent;
    function Remove(Item: TDAVProcessingComponent): Integer;
    procedure Insert(Index: Integer; Item: TDAVProcessingComponent);

    procedure SetSampleRate(Value: Single);
    procedure SetChannels(Value: Integer);
    procedure SetEnabled(Value: Boolean);
    procedure SetBypass(Value: Boolean);

    procedure ProcessMidiEventQueue(MidiEvent: TDAVMidiEvent; var FilterEvent: Boolean);
    procedure NoteOffQueue;

    property TrailingSamplesQueue: integer read GetTrailingSamplesQueue;
    property Items[Index: Integer]: TDAVProcessingComponent read Get write Put;
  end;

implementation

uses
  Math;

{ TDAVProcessingComponentList }

procedure TDAVProcessingComponentList.Insert(Index: Integer; Item: TDAVProcessingComponent);
begin
  inherited Insert(Index, Item);
end;

procedure TDAVProcessingComponentList.Put(Index: Integer; Item: TDAVProcessingComponent);
begin
   inherited Put(Index, Item);
end;

function TDAVProcessingComponentList.Add(Item: TDAVProcessingComponent): Integer;
begin
  Result := inherited Add(Item);
end;

function TDAVProcessingComponentList.Extract(Item: TDAVProcessingComponent): TDAVProcessingComponent;
begin
  Result := TDAVProcessingComponent(inherited Extract(Item));
end;

function TDAVProcessingComponentList.First: TDAVProcessingComponent;
begin
  Result := TDAVProcessingComponent(inherited First);
end;

function TDAVProcessingComponentList.Get(Index: Integer): TDAVProcessingComponent;
begin
  Result := TDAVProcessingComponent(inherited Get(Index));
end;

function TDAVProcessingComponentList.IndexOf(Item: TDAVProcessingComponent): Integer;
begin
  Result := inherited IndexOf(Item);
end;

function TDAVProcessingComponentList.Last: TDAVProcessingComponent;
begin
  Result := TDAVProcessingComponent(inherited Last);
end;

function TDAVProcessingComponentList.Remove(Item: TDAVProcessingComponent): Integer;
begin
  Result := inherited Remove(Item);
end;

procedure TDAVProcessingComponentList.SetSampleRate(Value: Single);
var i: integer;
begin
  for i := Count - 1 downto 0 do
    Items[i].SampleRate := Value;
end;

procedure TDAVProcessingComponentList.SetChannels(Value: Integer);
var i: integer;
begin
  for i := Count - 1 downto 0 do
    Items[i].Channels := Value;
end;

procedure TDAVProcessingComponentList.SetEnabled(Value: Boolean);
var i: integer;
begin
  for i := Count - 1 downto 0 do
    Items[i].Enabled := Value;
end;

procedure TDAVProcessingComponentList.SetBypass(Value: Boolean);
var i: integer;
begin
  for i := Count-1 downto 0 do
    Items[i].Bypass := Value;
end;

procedure TDAVProcessingComponentList.ProcessMidiEventQueue(MidiEvent: TDAVMidiEvent; var FilterEvent: Boolean);
var i: integer; filter: boolean;
begin
  FilterEvent:=false;
  for i := Count-1 downto 0 do
  begin
    filter:=false;
    Items[i].ProcessMidiEventQueue(MidiEvent, filter);
    FilterEvent:=FilterEvent or filter;
  end;
end;

procedure TDAVProcessingComponentList.NoteOffQueue;
var i: integer;
begin
  for i := Count-1 downto 0 do
    Items[i].NoteOffQueue;
end;

function TDAVProcessingComponentList.GetTrailingSamplesQueue: integer;
var i: integer;
begin
  result:=0;
  for i := Count-1 downto 0 do
    result:=max(result, Items[i].TrailingSamplesQueue);
end;


end.
