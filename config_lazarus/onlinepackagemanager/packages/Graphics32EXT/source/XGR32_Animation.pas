
{**********************************************************************
 Package pl_Graphics32EXT.pkg
 this unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit XGR32_Animation;

{$MODE Delphi}

interface

uses

  LCLIntf, LCLType, LMessages,
  SysUtils, Classes
  , Graphics
  , GR32
  ;

type
  TGRSpeed = 10..1000;

  TGRAnimationFrameClass = class of TGRAnimationFrame;
  TGRAnimationFrame = class(TCollectionItem)
  protected
    //for transparent
    FBackgroundColor: TColor;
    FBitmap: TBitmap32;
    FDelayTime: LongWord;
    procedure SetBitmap(const aBitmap: TBitmap32);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property BackgroundColor: TColor read FBackgroundColor write FBackgroundColor;
    property Bitmap: TBitmap32 read FBitmap write SetBitmap;
    property DelayTime: LongWord read FDelayTime write FDelayTime;
  end;

  TGRAnimationFrames = class(TCollection)
  private
    FOwner: TPersistent;
    function  GetItem(Index: Integer): TGRAnimationFrame;
    procedure SetItem(Index: Integer; Value: TGRAnimationFrame);
  protected
    function  GetOwner: TPersistent; override;
  public
    constructor Create(aOwner: TPersistent);
    function Add: TGRAnimationFrame;
    property Items[Index: Integer]: TGRAnimationFrame read GetItem write SetItem; default;
  end;

  TGRAniDisplayFrameEvent = procedure(Sender: TObject; aFrame: TGRAnimationFrame) of object;
  TGRAnimationDirection = (adForward, adRewind);
  TGRAniLoopEvent = procedure(Sender: TObject; var Continued: Boolean) of object;
  TGRAnimationClass = class of TGRAnimation;

  TGRAnimation = class(TPersistent)
  protected
    FOwner: TPersistent;
    FFrames: TGRAnimationFrames;
    FSpeed: TGRSpeed;
    FLooped: Boolean;
    FRunning: Boolean;
    FEnabled: Boolean;
    FFlipAlpha: Boolean;
    FCurrentIndex: Integer;
    FDirection: TGRAnimationDirection;
    FBackgroundColor: TColor;
    FOnLoop: TGRAniLoopEvent;
    FOnDisplayFrame: TGRAniDisplayFrameEvent;
    procedure SetFrames(const Value: TGRAnimationFrames);
    function GetOwner: TPersistent;override;
    function GetInterval(const FrameIndex: Integer): Integer; virtual;
    procedure SetSpeed(const Value: TGRSpeed);
    function GetFrameDelay(const FrameIndex: Integer; const SafeMode: Boolean=True): Integer;virtual;
    function GetNextIndex(var aIndex: Integer; const CanLoop: Boolean): Boolean;
    function IndexIsValid(const aIndex: Integer): Boolean;
    procedure DoLoop(var aContinued: Boolean);

    procedure DoFlipAlphaChannel(BMP32: TBitmap32);
    procedure DoFlipAlphaChannels;
  public
    procedure RequestFlipAlphaChannel;
    function DisplayFirstFrame(): Boolean;
    function DisplayFrame(const FrameIndex: Integer): Boolean;
    procedure LoadFromFile(const Filename: string);
    procedure SaveToFile(const Filename: string);
    procedure LoadFromStream(const aStream: TStream);virtual;abstract;
    procedure SaveToStream(const aStream: TStream);virtual;abstract;
    procedure Assign(Source: TPersistent);override;
    constructor Create(aOwner: TPersistent);virtual;
    destructor Destroy; override;

    property BackgroundColor: TColor read FBackgroundColor write FBackgroundColor;
    property Enabled: Boolean read FEnabled write FEnabled;
    property Frames: TGRAnimationFrames read FFrames write SetFrames;
    property FlipAlphaChannel: Boolean read FFlipAlpha write FFlipAlpha;
    property IsRunning: Boolean read FRunning;
    property Owner: TPersistent read FOwner;
    property Speed: TGRSpeed read FSpeed write SetSpeed;
    property Looped: Boolean read FLooped write FLooped;
    property OnLoop: TGRAniLoopEvent read FOnLoop write FOnLoop;
    property OnDisplayFrame: TGRAniDisplayFrameEvent read FOnDisplayFrame write FOnDisplayFrame;
  end;

  //to play the TGRAnimation frames.
  // abondon, use the New Animator-Effects Arch Framework.
  TGRAnimationThread = class(TThread)
  protected
    FInterval: LongWord;
    FAni: TGRAnimation;
    FStop: THandle;
    FEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);
    procedure SetInterval(const Value: LongWord);
    procedure Execute; override;
  public
    constructor Create(const aAnimation: TGRAnimation);
    destructor Destroy; override;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Interval: LongWord read FInterval write SetInterval;
  end;

type
  PAniFileFormat = ^TAniFileFormat;
  TAniFileFormat = record
    AnimationClass: TGRAnimationClass;
    Extension: string;
    Description: string;
    DescResID: Integer;
  end;

  TAniFileFormatsList = class(TList)
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const Ext, Desc: String; DescID: Integer; AClass: TGRAnimationClass);
    function FindExt(Ext: string): TGRAnimationClass;
    function FindClassName(const Classname: string): TGRAnimationClass;
    procedure Remove(AClass: TGRAnimationClass);
    procedure BuildFilterStrings(AnimationClass: TGRAnimationClass;
      var Descriptions, Filters: string);
  end;


procedure RegisterAnimation(const AExtension, ADescription: string; const aClass: TGRAnimationClass);
function  GAniFileFormats: TAniFileFormatsList;

implementation



const
  G32DefaultDelay: ShortInt = 100; // Time in ms.
  G32MinimumDelay: ShortInt = 10;  // Time in ms.

{ TAniFileFormatsList }
constructor TAniFileFormatsList.Create;
begin
  inherited Create;
end;

destructor TAniFileFormatsList.Destroy;
var
  I: Integer;
begin
  for I := 0 to Count-1 do
    Dispose(PAniFileFormat(Items[I]));
  inherited Destroy;
end;

procedure TAniFileFormatsList.Add(const Ext, Desc: String; DescID: Integer;
  AClass: TGRAnimationClass);
var
  NewRec: PAniFileFormat;
begin
  New(NewRec);
  with NewRec^ do
  begin
    Extension := AnsiLowerCase(Ext);
    AnimationClass := AClass;
    Description := Desc;
    DescResID := DescID;
  end;
  inherited Add(NewRec);
end;

function TAniFileFormatsList.FindExt(Ext: string): TGRAnimationClass;
var
  I: Integer;
begin
  Ext := AnsiLowerCase(Ext);
  for I := Count-1 downto 0 do
    with PAniFileFormat(Items[I])^ do
      if Extension = Ext then
      begin
        Result := AnimationClass;
        Exit;
      end;
  Result := nil;
end;

function TAniFileFormatsList.FindClassName(const ClassName: string): TGRAnimationClass;
var
  I: Integer;
begin
  for I := Count-1 downto 0 do
  begin
    Result := PAniFileFormat(Items[I])^.AnimationClass;
    if Result.ClassName = Classname then Exit;
  end;
  Result := nil;
end;

procedure TAniFileFormatsList.Remove(AClass: TGRAnimationClass);
var
  I: Integer;
  P: PAniFileFormat;
begin
  for I := Count-1 downto 0 do
  begin
    P := PAniFileFormat(Items[I]);
    if P^.AnimationClass.InheritsFrom(AClass) then
    begin
      Dispose(P);
      Delete(I);
    end;
  end;
end;

procedure TAniFileFormatsList.BuildFilterStrings(AnimationClass: TGRAnimationClass;
  var Descriptions, Filters: string);
var
  C, I: Integer;
  P: PAniFileFormat;
begin
  Descriptions := '';
  Filters := '';
  C := 0;
  for I := Count-1 downto 0 do
  begin
    P := PAniFileFormat(Items[I]);
    if P^.AnimationClass.InheritsFrom(AnimationClass) and (P^.Extension <> '') then
      with P^ do
      begin
        if C <> 0 then
        begin
          Descriptions := Descriptions + '|';
          Filters := Filters + ';';
        end;
        if (Description = '') and (DescResID <> 0) then
          Description := LoadStr(DescResID);
        FmtStr(Descriptions, '%s%s (*.%s)|*.%2:s', [Descriptions, Description, Extension]);
        FmtStr(Filters, '%s*.%s', [Filters, Extension]);
        Inc(C);
      end;
  end;
  //if C > 1 then
   // FmtStr(Descriptions, '%s (%s)|%1:s|%s', [sAllFilter, Filters, Descriptions]);
end;

var
  FAniFileFormats: TAniFileFormatsList = nil;

function GAniFileFormats: TAniFileFormatsList;
begin
  if FAniFileFormats = nil then FAniFileFormats := TAniFileFormatsList.Create;
  Result := FAniFileFormats;
end;


procedure RegisterAnimation(const AExtension, ADescription: string; const aClass: TGRAnimationClass);
begin
  GAniFileFormats.Add(AExtension, ADescription, 0, aClass);
end;

{ TGRAnimationFrame }
constructor TGRAnimationFrame.Create(Collection: TCollection);
begin
  inherited;
  FBitmap := TBitmap32.Create;
end;

destructor TGRAnimationFrame.Destroy;
begin
  FBitmap.Free;
  inherited;
end;

procedure TGRAnimationFrame.SetBitmap(const aBitmap: TBitmap32);
begin
  FBitmap.Assign(aBitmap);
end;

{ TGRAnimationFrames }
function TGRAnimationFrames.Add: TGRAnimationFrame;
begin
  Result := TGRAnimationFrame(inherited Add);
end;

constructor TGRAnimationFrames.Create(aOwner: TPersistent);
begin
  inherited Create(TGRAnimationFrame);
  FOwner := aOwner;
end;

function TGRAnimationFrames.GetItem(Index: Integer): TGRAnimationFrame;
begin
  Result := TGRAnimationFrame(inherited GetItem(Index));
end;

function TGRAnimationFrames.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TGRAnimationFrames.SetItem(Index: Integer; Value: TGRAnimationFrame);
begin
  inherited SetItem(Index, Value);
end;

{ TGRAnimation }
constructor TGRAnimation.Create(aOwner: TPersistent);
begin
  inherited Create();
  FOwner := aOwner;
  FFrames := TGRAnimationFrames.Create(Self);
  FSpeed := 100;
end;

destructor TGRAnimation.Destroy;
begin
  FFrames.Free;
  inherited;
end;
procedure TGRAnimation.Assign(Source: TPersistent);
begin
  if Source is TGRAnimation then
  with Source as TGRAnimation do
  begin
    Self.FFrames.Assign(FFrames);
    Self.FSpeed := FSpeed;
  end;
  inherited;
end;

function TGRAnimation.DisplayFirstFrame(): Boolean;
begin
  Result := DisplayFrame(0);
end;

function TGRAnimation.DisplayFrame(const FrameIndex: Integer): Boolean;
begin
  Result := FEnabled and IndexIsValid(FrameIndex);
  if Result then
  begin
    if Assigned(FOnDisplayFrame) then
      FOnDisplayFrame(Self, FFrames[FrameIndex]);
    if Frames.Count > 1 then
      GetNextIndex(FCurrentIndex, FLooped);
  end;
end;

procedure TGRAnimation.DoLoop(var aContinued: Boolean);
begin
  if Assigned(FOnLoop) then
    FOnLoop(Self, aContinued);
end;

function TGRAnimation.GetFrameDelay(const FrameIndex: Integer; const SafeMode: Boolean): Integer;
begin
  Result := 0;
  if IndexIsValid(FrameIndex) then
  Result := Frames.Items[FrameIndex].DelayTime;
  if (Result < G32MinimumDelay) and SafeMode then
  begin
    if (Result = 0) then
      Result := G32DefaultDelay
    else
      Result := G32MinimumDelay;
  end;
end;

function TGRAnimation.GetInterval(const FrameIndex: Integer): Integer;
begin
  Result := GetFrameDelay(FrameIndex);
  Result := Result * 100 div FSpeed;
end;

function TGRAnimation.GetNextIndex(var aIndex: Integer; const CanLoop: Boolean): Boolean;
begin
  Result := False;
  if (Frames.Count > 0) then
  begin
    if (FDirection = adForward) then
    begin
      if (aIndex < Frames.Count -1) then
      begin
        inc(aIndex);
        Result := true;
      end
      else begin
        aIndex := 0;
        Result := CanLoop and (Frames.Count > 1);
        if Result then DoLoop(Result);
      end;
    end
    else begin
      if (aIndex > 0) then
      begin
        dec(aIndex);
        Result := true;
      end
      else begin
        aIndex := Frames.Count -1; // decrement it right here !
        Result := CanLoop and (Frames.Count > 1);
        if Result then DoLoop(Result);
      end;
    end;
  end;
end;

function TGRAnimation.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TGRAnimation.IndexIsValid(const aIndex: Integer): Boolean;
begin
 Result := (aIndex >= 0) and (aIndex < Frames.Count);
end;


procedure TGRAnimation.LoadFromFile(const Filename: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TGRAnimation.DoFlipAlphaChannel(BMP32: TBitmap32);
var
 X: Integer;
 P: PColor32;
begin
 P := @(BMP32).Bits[0];
 for X := 0 to BMP32.Width * BMP32.Height -1 do
  begin
   P^ := P^ XOR $FF000000;
   inc(P);
  end;
end;

procedure TGRAnimation.DoFlipAlphaChannels;
var
  I: Integer;
begin
  for I := 0 to FFrames.Count -1 do
    DoFlipAlphaChannel( FFrames.Items[I].Bitmap );
end;

procedure TGRAnimation.RequestFlipAlphaChannel;
begin
  if FFlipAlpha then
    DoFlipAlphaChannels;
end;

procedure TGRAnimation.SaveToFile(const Filename: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(Filename, fmCreate or fmShareDenyWrite);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TGRAnimation.SetFrames(const Value: TGRAnimationFrames);
begin
  FFrames.Assign(Value);
end;

procedure TGRAnimation.SetSpeed(const Value: TGRSpeed);
begin
  if (FSpeed <> Value) then
   begin
     FSpeed := Value;
   end;
end;

{ TGRAnimationThread }

constructor TGRAnimationThread.Create(const aAnimation: TGRAnimation);
begin
  inherited Create(true);
// FStop := EventCreate(nil, False, False, nil);
  FStop := THandle(BasicEventCreate(nil, False, False, ''));   // ct9999 For CodeTyphon Studio
  FAni := aAnimation;
  Enabled := false;
  FreeOnTerminate:= false;
  FInterval := G32DefaultDelay;
end;

destructor TGRAnimationThread.Destroy;
begin
  Enabled := false;
  FileClose(FStop);
  Terminate;
  inherited Destroy;
end;

procedure TGRAnimationThread.SetEnabled(const Value: Boolean);
begin
  if (Value <> FEnabled) and (not Terminated) then
  begin
    FEnabled := Value;
    if FEnabled and (FInterval > 0) then
      Resume
    else
      Suspend;
  end;
end;

procedure TGRAnimationThread.SetInterval(const Value: LongWord);
var
  WasEnabled: Boolean;
begin
  if (Value <> FInterval) and (not Terminated) then
  begin
    WasEnabled := FEnabled;
    FInterval := Value;
    Enabled := WasEnabled and (FInterval > 0);
  end;
end;

procedure TGRAnimationThread.Execute;
begin
  repeat
    if  (not Terminated) then  // ct9999
    begin
      if FAni.Enabled then
      begin
        FAni.DisplayFrame(FAni.FCurrentIndex);
        FAni.FRunning := FAni.GetNextIndex(FAni.FCurrentIndex, FAni.FLooped);
        if FAni.IsRunning then
        begin
          Interval := FAni.GetInterval(FAni.FCurrentIndex);
        end;
        Enabled := FAni.IsRunning;
      end;
    end;
  until Terminated;
end;

end.
