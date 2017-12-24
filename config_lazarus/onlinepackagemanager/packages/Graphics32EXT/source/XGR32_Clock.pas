
{**********************************************************************
 Package pl_Graphics32EXT.pkg
 this unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

{.$DEFINE ThreadTimer_Supported}
{Switch it on the Clock Component uses Thread Timer or Windows Timer.}
{Use the SimpleTimer}


unit XGR32_Clock;

{$MODE Delphi}

interface

uses

  LCLIntf, LCLType,

  Classes, SysUtils,
  {$IFDEF ThreadTimer_Supported}
    ThreadTimer,
  {$ELSE}
    ExtCtrls,
  {$ENDIF}

  Controls,
  Messages,
  Graphics,
  XGR32_Controls;


const
  //Seconds per minute
  SecsPerMinute = 60;
  //Seconds per hour
  SecsPerHour   = SecsPerMinute * 60;

const
  DefaultHourClockHandWidth = 4;
  DefaultHourClockHandLength = 60;
  DefaultMinClockHandWidth = 3;
  DefaultMinClockHandLength = 80;
  DefaultSecondClockHandWidth = 1;
  DefaultSecondClockHandLength = 90;
  DefaultSecondClockHandColor = clRed;


type
  TNumPercent = 0..100;
  TClockMode = (cmClock, cmStopWatch);

    { Summary The Clockmark is minute or hour marks around the clock face.}
{ Description
When Clockmark.Visible is true, markers are drawn around the clock border.
The markers consists of dots at the minute points and blue squares
at the hour points. Generally you will set DrawMarks to false if
you are using a bitmap as the clock face.

See also: ClockFace
    }

  TGRCustomClockMark = class(TPersistent)
  protected
    FOwner    : TPersistent;
    FColor    : TColor;
    FLColor   : TColor;
    FRColor   : TColor;
    FWidth    : Integer;
    FHeight   : Integer;
    FSolid    : Boolean;
    FVisible  : Boolean;
    FOnChange : TNotifyEvent;
    FPicture  : TPicture;

    procedure SetColor(Value: TColor);
    procedure SetLColor(Value: TColor);
    procedure SetRColor(Value: TColor);
    procedure SetVisible(Value: Boolean);
    procedure SetSolid(Value: Boolean);
    procedure SetHeight(Value: Integer );
    procedure SetWidth(Value: Integer);
    procedure SetPicture(Value: TPicture);
    {## internal methods ##}
    procedure DoChange;virtual;
    procedure Draw(Canvas: TCanvas; MarkX, MarkY : Integer);virtual;
  public
    constructor Create(aOwner: TPersistent);
    destructor Destroy; override;
    procedure Assign(Source : TPersistent); override;

    property Owner    : TPersistent read FOwner;
    property Color    : TColor read FColor write SetColor;
    property LColor   : TColor read FLColor write SetLColor;
    property RColor   : TColor read FRColor write SetRColor;
    property Visible  : Boolean read FVisible write SetVisible default true;
    property Height   : Integer read FHeight write SetHeight;
    property Width    : Integer read FWidth write SetWidth;
    property Solid    : Boolean read FSolid write SetSolid default true;
    { Summary : the Clock Mark Picture }
    property Picture  : TPicture read FPicture  Write SetPicture;

    property OnChange : TNotifyEvent read FOnChange write FOnChange;
  published
  end;

  { Summary a mark every five minutes. }
  TGRHourMark = class(TGRCustomClockMark)
  protected
    procedure Draw(Canvas: TCanvas; MarkX, MarkY : Integer);override;
  public
    constructor Create(aOwner: TPersistent);
  published
    property LColor default clBlue;
    property Color default clGray;
    property RColor default clBlack;
    property Visible;
    property Height;
    property Width;
    property Solid;
    property Picture;
  end;

  { Summary a mark every minute. }
  TGRMinuteMark = class(TGRCustomClockMark)
  public
    constructor Create(aOwner: TPersistent);
  published
    property LColor default clGray;
    property Color default clSilver;
    property RColor default clWhite;
    property Visible;
    property Height;
    property Width;
    property Solid;
    property Picture;
  end;

const
  MaxPoints = 7;

type
  TGRCustomClockHand = class(TPersistent)
  protected
    Hour          : Word;
    Minute        : Word;
    Second        : Word;
    HalfWidth     : Integer;
    HalfHeight    : Integer;
    HandBase      : Integer;
    HandAngle     : Double;
    HandRealLen   : Integer;
    I          : Integer;
    Hip        : Integer;
    Points     : array[1..MaxPoints] of TPoint;
    HandPoints : array[1..MaxPoints] of TPoint;

    FOwner    : TPersistent;
    FColor    : TColor;
    FLength   : TNumPercent;
    FWidth    : Integer;
    FSolid    : Boolean;
    FVisible  : Boolean;
    FClockRect: TRect;
    FTexture: TPicture;
    FOnChange : TNotifyEvent;

    procedure SetColor(Value: TColor);
    procedure SetVisible(Value: Boolean);
    procedure SetSolid(Value: Boolean);
    procedure SetLength(Value: TNumPercent);
    procedure SetWidth(Value: Integer);
    procedure SetClockRect(Value: TRect);
    procedure SetTexture(Value: TPicture);

    {internal methods}
    procedure DoChange;virtual;

    procedure RotatePoint(OldPoint : TPoint; var NewPoint : TPoint);
    procedure LoadPoints(aCanvas: TCanvas; Pt1, Pt2, Pt3 : Integer);

    procedure DrawHand(Canvas: TCanvas);virtual;
    procedure Draw(Canvas: TCanvas; aClientRect: TRect; aTime: TDateTime);virtual;
  public
    constructor Create(aOwner: TPersistent);
    destructor Destroy;override;
    procedure Assign(Source : TPersistent); override;

    property Owner    : TPersistent read FOwner;
    property Color    : TColor read FColor write SetColor default clBlack;
    property Visible  : Boolean read FVisible write SetVisible default true;
    property Length   : TNumPercent read FLength write SetLength;
    property Width    : Integer read FWidth write SetWidth;
    property Solid    : Boolean read FSolid write SetSolid default true;
    { Summary : the Clock Rect to draw the clock hand in the center of ClockRect. }
    property ClockRect: TRect read FClockRect write SetClockRect;
    { Summary : The Clock hand's Texture. }
    property Texture: TPicture read FTexture write SetTexture;

    property OnChange : TNotifyEvent read FOnChange write FOnChange;
  published
  end;

  TGRHourClockHand = Class(TGRCustomClockHand)
  protected
    procedure DrawHand(Canvas: TCanvas);override;
  public
    constructor Create(aOwner: TPersistent);
  published
    property Color;
    property Visible;
    property Length default DefaultHourClockHandLength;
    property Width default DefaultHourClockHandWidth;
    property Solid;
   // property ClockRect;
  end;

  TGRMinuteClockHand = Class(TGRCustomClockHand)
  protected
    procedure DrawHand(Canvas: TCanvas);override;
  public
    constructor Create(aOwner: TPersistent);
  published
    property Color;
    property Visible;
    property Length default DefaultMinClockHandLength;
    property Width default DefaultMinClockHandWidth;
    property Solid;
   // property ClockRect;
  end;

  TGRSecondClockHand = Class(TGRCustomClockHand)
  protected
    procedure DrawHand(Canvas: TCanvas);override;
  public
    constructor Create(aOwner: TPersistent);
  published
    property Color  default DefaultSecondClockHandColor;
    property Visible;
    property Length default DefaultSecondClockHandLength;
    property Width default DefaultSecondClockHandWidth;
    property Solid;
  //  property ClockRect;
  end;

  TGRCustomClock = class(TGRGraphicControl)
  private
  protected
    FActive           : Boolean;
    FClockFace        : TPicture;
    FClockMode        : TClockMode;
    FDrawMarks        : Boolean;
    FHourMarks        : TGRHourMark;
    FMinuteMarks      : TGRMinuteMark;
    FHourHand         : TGRHourClockHand;
    FMinuteHand       : TGRMinuteClockHand;
    FSecondHand       : TGRSecondClockHand;
    FTime             : TDateTime;
    FTimeOffset       : Integer;   {Hours}
    FMinuteOffset     : Integer;   {Minutes}

    {event variables}
    FOnHourChange   : TNotifyEvent;
    FOnMinuteChange : TNotifyEvent;
    FOnSecondChange : TNotifyEvent;

    {internal variables}
    FTimer        : {$IFDEF ThreadTimer_Supported} TThreadTimer {$ELSE} TTimer{$ENDIF};
    ckDraw        : TBitMap;
    OldHour       : Integer;
    OldMinute     : Integer;
    OldSecond     : Integer;

    {property methods}
    function GetElapsedDays : Integer;
    function GetElapsedHours : Integer;
    function GetElapsedMinutes : LongInt;
    function GetElapsedSeconds : LongInt;
    function GetElapsedSecondsTotal : LongInt;
    procedure SetActive(Value : Boolean);
    procedure SetClockFace(Value : TPicture);
    procedure SetClockMode(Value : TClockMode);
    procedure SetDrawMarks(Value : Boolean);
    procedure SetMinuteOffset(Value : Integer);
    procedure SetTimeOffset(Value : Integer);
    procedure SetHourHand(Value: TGRHourClockHand);
    procedure SetMinuteHand(Value: TGRMinuteClockHand);
    procedure SetSecondHand(Value: TGRSecondClockHand);
    procedure SetHourMarks(Value: TGRHourMark);
    procedure SetMinuteMarks(Value: TGRMinuteMark);

    {internal methods}
    procedure DoTimerEvent(Sender : TObject);

    procedure DoOnHourChange;
    procedure DoOnMinuteChange;
    procedure DoOnSecondChange;
    procedure PaintHands(ACanvas : TCanvas);

  protected
    procedure Loaded; override;
    procedure Paint;  override;

    procedure SetTime(Value : TDateTime); virtual;

    {
Active determines whether or not the clock shows the system time in real time.

The behavior of the Active property depends on the current ClockMode.
When ClockMode is cmClock, set Active to true to display the system
time in realtime. Set Active to false if you want the clock to display
a static time. Assign a TDateTime value to the Time property to set the
static time. When ClockMode is cmStopWatch, setting Active to true starts
the timer and setting Active to false stops the timer. Read the elapsed
time to determine the elapsed time of the clock in timer mode.

See also: ClockMode, Time
    }
    property Active : Boolean read FActive write SetActive;

    {
ClockFace is a picture that is displayed for the clock face.

Assign a pictute to ClockFace to change the appearance of the clock.
You can load a bitmap at design time using the ClockFace property
editor, or at runtime by assigning a picture to ClockFace. The clock
hands are drawn on top of the clock face. For aesthetic reasons you
may want to set the DrawMarks property to false when supplying a clock
face. Several clock faces are provided for you. You will find the clock
face bitmaps in the Examples directory.

See also: DrawMarks, HandOptions
    }
    property ClockFace : TPicture read FClockFace write SetClockFace;

    {
Default: cmClock

ClockMode is the mode in which the clock operates.

When ClockMode is cmClock the clock displays a time of day.
When ClockMode is cmStopWatch, the clock shows the current elapsed
time of a timer. To start a timer set ClockMode to cmStopWatch and
the Active property to true. To stop the timer, set Active to
false. Time is automatically set to 12:00 AM when the Active
property is set to true if ClockMode is cmStopWatch.

The ElapsedSeconds property can be used to determine the
elapsed time when the clock is used in timer mode.

See also: Active, Time
     }
    property ClockMode : TClockMode read FClockMode write SetClockMode;

    {
Default: true

DrawMarks determines whether or not minute and hour marks are
drawn around the clock face.

When DrawMarks is true, markers are drawn around the clock border.
The markers consists of dots at the minute points and blue squares
at the hour points. Generally you will set DrawMarks to false if
you are using a bitmap as the clock face.

See also: ClockFace
    }
    property DrawMarks : Boolean read FDrawMarks write SetDrawMarks default True;

    property HourHand     : TGRHourClockHand read FHourHand write SetHourHand;
    property MinuteHand   : TGRMinuteClockHand read FMinuteHand write SetMinuteHand;
    property SecondHand   : TGRSecondClockHand read FSecondHand write SetSecondHand;
    property HourMarks    : TGRHourMark read FHourMarks write SetHourMarks;
    property MinuteMarks  : TGRMinuteMark read FMinuteMarks write SetMinuteMarks;

    property MinuteOffset : Integer read FMinuteOffset write SetMinuteOffset;

    {
Default: 0

TimeOffset is the hour offset from the current time zone.

The clock's time is taken from the system's current time.
When TimeOffset is 0, the current local time is displayed.
To display times in other time zones, set TimeOffset to
the relative time zone you wish to display. If, for example,
you are in the United States' Mountain time zone and you want
to display a clock showing Pacific time you would set
TimeOffset to -1. To display Eastern time (two time zones earlier)
you would set TimeOffset to 2.

See also: Time
    }
    property TimeOffset : Integer read FTimeOffset write SetTimeOffset;

    {events}
    {
OnHourChange defines an event that is generated when the hour changes.

Provide an event handler for the OnHourChange event if you want to be
notified at the top of each hour.

See also: OnMinuteChange, OnSecondChange
    }
    property OnHourChange : TNotifyEvent read FOnHourChange write FOnHourChange;

    {
OnMinuteChange defines an event that is generated when the minute changes.

Provide an event handler for the OnMinuteChange event if you want to be
notified when the minute changes.

See also: OnHourChange, OnSecondChange
    }
    property OnMinuteChange : TNotifyEvent read FOnMinuteChange write FOnMinuteChange;

    {
OnSecondChange defines an event that is generated each second.

Provide an event handler for the OnSecondChange event if you want to be
notified each time the clock's second hand changes.

See also: OnHourChange, OnMinuteChange
    }
    property OnSecondChange : TNotifyEvent
      read FOnSecondChange write FOnSecondChange;

  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight : Integer);
      override;

    {
ElapsedDays returns the number of elapsed days when ClockMode is cmStopWatch.

Read ElapsedDays to determine the number of elapsed days since the timer
was started. The elapsed time (in milliseconds) is stored internally in
an unsigned long integer variable so the number of elapsed days will reset
to 0 after 49.7 days have elapsed.
    }
    property ElapsedDays : Integer read GetElapsedDays;

    {
ElapsedHours returns the number of elapsed hours when ClockMode is cmStopWatch.

ElapsedHours does not return the total elapsed hours, but rather is the
hours component of elapsed days, hours, minutes, and seconds. To determine
the total elapsed time in hours, read the ElapsedSecondsTotal property and
divide by 360.

See also: ElapsedDays, ElapsedMinutes, ElapsedSeconds, ElapsedSecondsTotal
    }
    property ElapsedHours : Integer read GetElapsedHours;

    {
ElapsedMinutes returns the number of elapsed minutes when ClockMode is cmStopWatch.

ElapsedMinutes does not return the total elapsed minutes, but rather is the
minutes component of elapsed days, hours, minutes, and seconds. To determine the
total elapsed time in minutes, read the ElapsedSecondsTotal property and divide
by 60.

See also: ElapsedDays, ElapsedHours, ElapsedSeconds, ElapsedSecondsTotal
    }
    property ElapsedMinutes : LongInt read GetElapsedMinutes;

    {
ElapsedSeconds returns the number of elapsed seconds when ClockMode is cmStopWatch.

ElapsedSeconds does not return the total elapsed seconds, but rather is the
seconds component of elapsed days, hours, minutes, and seconds. To determine
the total elapsed time in seconds, read the ElapsedSecondsTotal property. The
elapsed time (in milliseconds) is stored internally in an unsigned long integer
variable so the number of elapsed seconds will reset to 0 after 4,294,967 seconds
have elapsed (49.7 days).

See also: ElapsedDays, ElapsedHours, ElapsedMinutes, ElapsedSecondsTotal
    }
    property ElapsedSeconds : LongInt read GetElapsedSeconds;

    {
ElapsedSecondsTotal returns the total number of elapsed seconds when
ClockMode is cmStopWatch.

The elapsed time (in milliseconds) is stored internally in an unsigned
long integer variable so the total number of elapsed seconds will reset
to 0 after 4,294,967 seconds have elapsed (49.7 days).
    }
    property ElapsedSecondsTotal : LongInt read GetElapsedSecondsTotal;

    {
Time is the clock's current time.

Read Time to determine the clock's current time. Set Time to a TDateTime
value to display a static time on the clock when the Active property is false.

See also: Active, ClockMode
    }
    property Time : TDateTime read FTime write SetTime;

  end;

  TGRClock = class(TGRCustomClock)
  published
    {properties}
    {$IFDEF COMPILER4_UP}
    property Anchors;
    property Constraints;
    {$ENDIF}
    property Active;
    property Align;
    property Color;
    property ClockFace;
    property ClockMode;
    property DrawMarks;
    property Hint;
    //property HandOptions;
    property MinuteOffset;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TimeOffset;
    property Visible;
    property HourHand;
    property MinuteHand;
    property SecondHand;
    property HourMarks;
    property MinuteMarks;

    {events}
    property OnClick;
    property OnDblClick;
    property OnHourChange;
    property OnMinuteChange;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnSecondChange;
  end;


//milliseconds to DateTime
function MsToDateTime(Value : LongInt) : TDateTime;


implementation

uses
  Math;

const
  cDegToRad       = (Pi / 180);
  DefaultInterval = 500;



//milliseconds to DateTime
function MsToDateTime(Value : LongInt) : TDateTime;
var
  S, Days : LongInt;
  Hour, Minute, Second, MS : Word;
begin
  S := Value div 1000;
  MS := Value mod 1000;
  Days := S div SecsPerDay;
  S := S mod SecsPerDay;
  Hour := S div SecsPerHour;
  S := S mod SecsPerHour;
  Minute := S div SecsPerMinute;
  Second := S mod SecsPerMinute;
  Result := EncodeTime(Hour, Minute, Second, MS) + Days;
end;

{ TGRCustomClockMark }

constructor TGRCustomClockMark.Create(aOwner: TPersistent);
begin
  inherited Create;
  FOwner   := aOwner;
  FVisible := True;
  FSolid   := True;
  FPicture := TPicture.Create;
end;

destructor TGRCustomClockMark.Destroy;
begin
  FreeAndNil(FPicture);
  inherited;
end;

procedure TGRCustomClockMark.Assign(Source : TPersistent);
begin
  if Source is TGRCustomClockMark then
  with TGRCustomClockMark(Source) do
  begin
    Self.FColor    := FColor;
    Self.FHeight   := FHeight;
    Self.FWidth    := FWidth;
    Self.FVisible  := FVisible;
    Self.FSolid    := FSolid;
    Self.FOnChange := FOnChange;
    Self.Picture   := FPicture;
    Self.DoChange;
  end else
    inherited Assign(Source);
end;

procedure TGRCustomClockMark.DoChange;
begin
  if Assigned(FOwner) then
    TGRCustomClock(FOwner).Invalidate;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TGRCustomClockMark.Draw(Canvas: TCanvas; MarkX, MarkY : Integer);
begin
  with Canvas do
  begin
    If not Assigned(FPicture.Graphic) Then
    Begin
      Pixels[MarkX, MarkY]     := FColor;
      Pixels[MarkX-1, MarkY-1] := FLColor;
      Pixels[MarkX,   MarkY-1] := FLColor;
      Pixels[MarkX-1, MarkY]   := FLColor;

      Pixels[MarkX+1, MarkY+1] := FRColor;
      Pixels[MarkX+1, MarkY]   := FRColor;
      Pixels[MarkX, MarkY+1]   := FRColor;
    End
    Else begin
      StretchDraw(Bounds(MarkX, MarkY, FWidth, FHeight), FPicture.Graphic);
    end;
  end;
end;

procedure TGRCustomClockMark.SetColor(Value : TColor);
begin
  if Value <> FColor then
  begin
    FColor := Value;
    DoChange;
  end;
end;

procedure TGRCustomClockMark.SetLColor(Value: TColor);
begin
  if Value <> FLColor then
  begin
    FLColor := Value;
    DoChange;
  end;
end;

procedure TGRCustomClockMark.SetPicture(Value: TPicture);
begin
  If FPicture <> Value Then
  Begin
    FPicture.Assign(Value);
    DoChange;
  End; // If
end;

procedure TGRCustomClockMark.SetRColor(Value: TColor);
begin
  if Value <> FRColor then
  begin
    FRColor := Value;
    DoChange;
  end;
end;

procedure TGRCustomClockMark.SetHeight(Value : integer);
begin
  if Value <> FHeight then begin
    FHeight := Value;
    DoChange;
  end;
end;

procedure TGRCustomClockMark.SetWidth(Value : Integer);
begin
  if (Value <> FWidth) and (Value > 0) then
  begin
    FWidth := Value;
    DoChange;
  end;
end;

procedure TGRCustomClockMark.SetVisible(Value : Boolean);
begin
  if Value <> FVisible then
  begin
    FVisible := Value;
    DoChange;
  end;
end;

procedure TGRCustomClockMark.SetSolid(Value : Boolean);
begin
  if Value <> FSolid then
  begin
    FSolid := Value;
    DoChange;
  end;
end;

{ TGRHourMark }

constructor TGRHourMark.Create(aOwner: TPersistent);
begin
  inherited Create(aOwner);
  FLColor := clBlue;
  FColor  := clGray;
  FRColor := clBlack;
end;

procedure TGRHourMark.Draw(Canvas: TCanvas; MarkX, MarkY : Integer);
begin
  inherited;
  If not Assigned(FPicture.Graphic) Then
  with Canvas do
  begin
    Pixels[MarkX-1, MarkY+1] := FLColor;
    Pixels[MarkX+1, MarkY-1] := FRColor;
  end;
end;

{ TGRMinuteMark }

constructor TGRMinuteMark.Create(aOwner: TPersistent);
begin
  inherited Create(aOwner);
  FLColor := clGray;
  FColor  := clSilver;
  FRColor := clWhite;
end;

{ TGRCustomClockHand }

constructor TGRCustomClockHand.Create(aOwner: TPersistent);
begin
  inherited Create;
  FOwner   := aOwner;
  FColor   := clBlack;
  FVisible := True;
  FSolid   := True;
  FTexture := TPicture.Create;
end;

destructor TGRCustomClockHand.Destroy;
begin
  FreeAndNil(FTexture);
  inherited;
end;

procedure TGRCustomClockHand.Assign(Source : TPersistent);
begin
  if Source is TGRCustomClockHand then
  with TGRCustomClockHand(Source) do
  begin
    Self.FColor    := FColor;
    Self.FLength   := FLength;
    Self.FWidth    := FWidth;
    Self.FVisible  := FVisible;
    Self.FSolid    := FSolid;
    Self.FOnChange := FOnChange;
    Self.DoChange;
  end else
    inherited Assign(Source);
end;

procedure TGRCustomClockHand.DoChange;
begin
  if Assigned(FOwner) then
    TGRCustomClock(FOwner).Invalidate;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TGRCustomClockHand.Draw(Canvas: TCanvas; aClientRect: TRect; aTime: TDateTime);
var
  MSecond: Word;
begin
  DecodeTime(aTime, Hour, Minute, Second, MSecond);
  {if FClockRect.Left <> 0 then
    HalfWidth := FClockRect.Left + HalfWidth;
  if FClockRect.Top <> 0 then
    HalfHeight := FClockRect.Top + HalfHeight; //}
  If (FClockRect.Bottom <> 0) or (FClockRect.Top <> 0)
    or (FClockRect.Right <> 0) or (FClockRect.Left <> 0)
  Then
  Begin
    {TODO: BUG About the custom Clock Hand position!}
    aClientRect := FClockRect;
  End; // If
  HalfWidth   := (aClientRect.Right - aClientRect.Left) shr 1;
  HalfHeight  := (aClientRect.Bottom - aClientRect.Top) shr 1;
  if HalfWidth < 1 then HalfWidth := 1;
  if HalfHeight < 1 then HalfHeight := 1;
  {based on the Height or Width of the Clock, set the Hand Lengths}
  HandBase    := Min(HalfWidth, HalfHeight);
  HandRealLen := Trunc(HandBase * FLength / 100);
  //HalfWidth   := aClientRect.Left + HalfWidth;
  //HalfHeight  := aClientRect.Top + HalfHeight;
  DrawHand(Canvas);
  MSecond := Round(HandBase * 0.04) + 1;
  Canvas.Ellipse(aClientRect.Left + HalfWidth-MSecond, aClientRect.Top + HalfHeight-MSecond, aClientRect.Left + HalfWidth+MSecond, aClientRect.Top + HalfHeight+MSecond);
end;

procedure TGRCustomClockHand.DrawHand(Canvas: TCanvas);
var
  xI: integer;
begin
  {where to put Crossbar}
  if FWidth > 1 then
    Hip := Trunc(HandRealLen * 0.25)
  else
    Hip := 0;

  {start at Center Point}
  Points[1].X := FClockRect.Left + HalfWidth;
  Points[1].Y := FClockRect.Top + HalfHeight;


  {up Center to Hip}
  Points[2].X := FClockRect.Left + HalfWidth;
  Points[2].Y := FClockRect.Top + HalfHeight-Hip;

  {up Center to Top}
  Points[3].X := FClockRect.Left + HalfWidth;
  Points[3].Y := FClockRect.Top + HalfHeight-HandRealLen;

  {angle Left}
  Points[4].X := FClockRect.Left + HalfWidth - FWidth;
  Points[4].Y := FClockRect.Top + HalfHeight - Hip;

  {start at Center Point}
  Points[5].X := FClockRect.Left + HalfWidth;
  Points[5].Y := FClockRect.Top + HalfHeight;

  {angle Left}
  Points[6].X := FClockRect.Left + HalfWidth + FWidth;
  Points[6].Y := FClockRect.Top + HalfHeight - Hip;

  {up Center to Top}
  Points[7].X := FClockRect.Left + HalfWidth;
  Points[7].Y := FClockRect.Top + HalfHeight-HandRealLen;

  for xI :=1 to 7 do
    RotatePoint(Points[xI], HandPoints[xI]);

  Canvas.Pen.Width := 1;
  Canvas.Pen.Color := FColor;
  if FSolid then
  begin
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := FColor;
  end
  else
  begin
    If Assigned(FTexture.Graphic) Then
    Begin
      Canvas.Brush.Bitmap := FTexture.Bitmap;
    End
    Else
    begin
      Canvas.Brush.Style := bsClear;
    end;
  end;

  //Canvas.MoveTo(FClockRect.Top + HalfWidth, FClockRect.Left + HalfHeight);
    ///Canvas.MoveTo(HalfWidth, HalfHeight);
  Canvas.Polygon(HandPoints);
  Canvas.Brush.Bitmap := nil;
end;

procedure TGRCustomClockHand.LoadPoints(aCanvas: TCanvas; Pt1, Pt2, Pt3 : Integer);
Var
  CPoints : array[1..3] of TPoint;
begin
  CPoints[1] := HandPoints[Pt1];
  CPoints[2] := HandPoints[Pt2];
  CPoints[3] := HandPoints[Pt3];
  aCanvas.Polygon(CPoints);
end;

procedure TGRCustomClockHand.RotatePoint(OldPoint : TPoint; var NewPoint : TPoint);
begin
  OldPoint.X := OldPoint.X - FClockRect.Left - HalfWidth;
  OldPoint.Y := OldPoint.Y - FClockRect.Top - HalfHeight;
  NewPoint.X := Round(OldPoint.X * Cos(HandAngle-Pi/2) - OldPoint.Y * Sin(HandAngle-Pi/2));
  NewPoint.Y := Round(OldPoint.X * Sin(HandAngle-Pi/2) + OldPoint.Y * Cos(HandAngle-Pi/2));
  if (HalfHeight < HalfWidth) then
    NewPoint.X := Round(NewPoint.X * (HalfWidth/HalfHeight))
  else
    NewPoint.Y := Round(NewPoint.Y * (HalfHeight/HalfWidth));
  NewPoint.X := NewPoint.X + HalfWidth + FClockRect.Left;
  NewPoint.Y := NewPoint.Y + HalfHeight + FClockRect.Top;
end;

procedure TGRCustomClockHand.SetClockRect(Value: TRect);
begin
  If (FClockRect.Bottom <> Value.Bottom) or (FClockRect.Top <> Value.Top)
    or (FClockRect.Right <> Value.Right) or (FClockRect.Left <> Value.Left)
  then
  begin
    FClockRect := Value;
    DoChange;
  end;
end;

procedure TGRCustomClockHand.SetColor(Value : TColor);
begin
  if Value <> FColor then
  begin
    FColor := Value;
    DoChange;
  end;
end;

procedure TGRCustomClockHand.SetLength(Value : TNumPercent);
begin
  if Value <> FLength then
  begin
    FLength := Value;
    DoChange;
  end;
end;

procedure TGRCustomClockHand.SetTexture(Value: TPicture);
begin
  if Value <> FTexture then
  begin
    FTexture.Assign(Value);
    DoChange;
  end;
end;

procedure TGRCustomClockHand.SetWidth(Value : Integer);
begin
  if (Value <> FWidth) and (Value > 0) then
  begin
    FWidth := Value;
    DoChange;
  end;
end;

procedure TGRCustomClockHand.SetVisible(Value : Boolean);
begin
  if Value <> FVisible then
  begin
    FVisible := Value;
    DoChange;
  end;
end;

procedure TGRCustomClockHand.SetSolid(Value : Boolean);
begin
  if Value <> FSolid then
  begin
    FSolid := Value;
    DoChange;
  end;
end;

{ TGRHourClockHand }

constructor TGRHourClockHand.Create(aOwner: TPersistent);
begin
  inherited;
  FLength := DefaultHourClockHandLength;
  FWidth  := DefaultHourClockHandWidth;
end;

procedure TGRHourClockHand.DrawHand(Canvas: TCanvas);
begin
  HandAngle := cDegToRad * (((Round((((Hour * 5) + (Minute div 12)) / 60) * 360)) + 90) mod 360);
  inherited;
  if not FSolid then
  begin
    Canvas.Brush.Color := clWhite;
    //Shade Hand
    case Hour of
      0..3,
     12..15 : begin
                LoadPoints(Canvas, 2,3,4);
                LoadPoints(Canvas, 1,2,4);
              end;
      4..5,
     16..17 : begin
                LoadPoints(Canvas, 1,2,4);
                LoadPoints(Canvas, 1,2,6);
              end;
      6..9,
     18..21 : begin
                LoadPoints(Canvas, 1,2,6);
                LoadPoints(Canvas, 2,3,6);
              end;
     10..11,
     22..23 : begin
                LoadPoints(Canvas, 2,3,4);
                LoadPoints(Canvas, 2,3,6);
              end;
    end;
  end;
end;

{ TGRMinuteClockHand }

constructor TGRMinuteClockHand.Create(aOwner: TPersistent);
begin
  inherited;
  FLength := DefaultMinClockHandLength;
  FWidth  := DefaultMinClockHandWidth;
end;

procedure TGRMinuteClockHand.DrawHand(Canvas: TCanvas);
begin
  HandAngle := cDegToRad * (((Round((Minute / 60) * 360)) + 90) mod 360);
  inherited;
  if not FSolid then
  begin
    //Shade Hand
    Canvas.Brush.Color := clWhite;
    case Minute of
      0..15 : begin
                LoadPoints(Canvas, 2,3,4);
                LoadPoints(Canvas, 1,2,4);
              end;
      16..25: begin
                LoadPoints(Canvas, 1,2,4);
                LoadPoints(Canvas, 1,2,6);
              end;
      26..50: begin
                LoadPoints(Canvas, 1,2,6);
                LoadPoints(Canvas, 2,3,6);
              end;
      51..59: begin
                LoadPoints(Canvas, 2,3,4);
                LoadPoints(Canvas, 2,3,6);
              end;
    end;
  end;
end;

{ TGRSecondClockHand }

constructor TGRSecondClockHand.Create(aOwner: TPersistent);
begin
  inherited;
  FLength := DefaultSecondClockHandLength;
  FWidth  := DefaultSecondClockHandWidth;
  FColor  := DefaultSecondClockHandColor;
end;

procedure TGRSecondClockHand.DrawHand(Canvas: TCanvas);
begin
  HandAngle := cDegToRad * (((Round((Second / 60) * 360)) + 90) mod 360);
  inherited;
  if not FSolid then
  begin
    //Shade Hand
    Canvas.Brush.Color := clWhite;
    case Second of
      0..15 : begin
                LoadPoints(Canvas, 2,3,4);
                LoadPoints(Canvas, 1,2,4);
              end;
      16..25: begin
                LoadPoints(Canvas, 1,2,4);
                LoadPoints(Canvas, 1,2,6);
              end;
      26..50: begin
                LoadPoints(Canvas, 1,2,6);
                LoadPoints(Canvas, 2,3,6);
              end;
      51..59: begin
                LoadPoints(Canvas, 2,3,4);
                LoadPoints(Canvas, 2,3,6);
              end;
    end; // case
  end; // if}
end;

{ TGRCustomClock }

procedure TGRCustomClock.DoTimerEvent(Sender : TObject);
var
  Hour, Minute, Second, MSecond : Word;
  C, D                          : Integer;
begin
  if FClockMode = cmClock then
  begin
    DecodeTime(Now, Hour, Minute, Second, MSecond);
    D := Minute + FMinuteOffset;
    Minute := Abs(D mod 60);
    C := Hour + FTimeOffset + (D div 60);
    if C > 23 then
      Dec(C, 24);
    if C < 0 then
      Inc(C, 24);
    Hour := C;
    SetTime(EncodeTime(Hour, Minute, Second, MSecond));
  end else
  begin
    //SetTime(MsToDateTime(FTimer.ElapsedTime)); // SOS 9999
  end;
end;

constructor TGRCustomClock.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  Width      := 150;
  Height     := 150;

  FClockMode := cmClock;
  FDrawMarks := True;

  FClockFace        := TPicture.Create;
  FHourHand         := TGRHourClockHand.Create(Self);
  FMinuteHand       := TGRMinuteClockHand.Create(Self);
  FSecondHand       := TGRSecondClockHand.Create(Self);
  FHourMarks        := TGRHourMark.Create(Self);
  FMinuteMarks      := TGRMinuteMark.Create(Self);
  {$IFDEF ThreadTimer_Supported}
  FTimer            := TThreadTimer.Create(Self);
  {$ELSE}
  FTimer            := TTimer.Create(Self);
  {$ENDIF}
  ckDraw            := TBitMap.Create;
  ckDraw.Width      := Width;
  ckDraw.Height     := Height;

  FTimer.Interval   := DefaultInterval;
  FTimer.OnTimer    := DoTimerEvent;

end;

destructor TGRCustomClock.Destroy;
begin
  Active := False;

  FreeAndNil(FClockFace);

  FreeAndNil(FHourHand);
  FreeAndNil(FMinuteHand);
  FreeAndNil(FSecondHand);
  FreeAndNil(FMinuteMarks);
  FreeAndNil(FHourMarks);
  FreeAndNil(FTimer);


  FreeAndNil(ckDraw);

  inherited Destroy;
end;

procedure TGRCustomClock.DoOnHourChange;
begin
  if Assigned(FOnHourChange) then
    FOnHourChange(Self);
end;

procedure TGRCustomClock.DoOnMinuteChange;
begin
  if Assigned(FOnMinuteChange) then
    FOnMinuteChange(Self);
end;

procedure TGRCustomClock.DoOnSecondChange;
begin
  if Assigned(FOnSecondChange) then
    FOnSecondChange(Self);
end;

function TGRCustomClock.GetElapsedDays: Integer;
var
  aDate : TDateTime;
begin
 // aDate := MsToDateTime(FTimer.ElapsedTime); // SOS 9999
  Result := Trunc(aDate);
end;

function TGRCustomClock.GetElapsedHours: Integer;
var
  Hour  : Word;
  t     : Word;
  aTime : TDateTime;
begin
 // aTime := MsToDateTime(FTimer.ElapsedTime);  // SOS 9999
  DecodeTime(aTime, Hour, t, t, t);
  Result := Hour;
end;

function TGRCustomClock.GetElapsedMinutes: LongInt;
var
  t     : Word;
  Min   : Word;
  aTime : TDateTime;
begin
 // aTime := MsToDateTime(FTimer.ElapsedTime);  // SOS 9999
  DecodeTime(aTime, t, Min, t, t);
  Result := Min;
end;

function TGRCustomClock.GetElapsedSeconds : LongInt;
var
  Second : Word;
  t      : Word;
  aTime  : TDateTime;
begin
//  aTime := MsToDateTime(FTimer.ElapsedTime); // SOS 9999
  DecodeTime(aTime, t, t, Second, t);
  Result := Second;
end;

function TGRCustomClock.GetElapsedSecondsTotal : LongInt;
begin
  //Result := FTimer.ElapsedTime div 1000;  // SOS 9999
end;

procedure TGRCustomClock.Loaded;
var
  HA : Boolean;
begin
  inherited Loaded;

  HA := FActive;
  FActive := False;
  SetActive(HA);
end;

procedure TGRCustomClock.Paint;
var
  HalfWidth   : Integer;
  HalfHeight  : Integer;
  ElRgn       : HRgn;
  R           : TRect;

  procedure xDrawMarks(const aClockMark: TGRCustomClockMark; Step: integer);
  var
    i: Integer;
    MarkX     : Integer;
    MarkY     : Integer;
    MarkAngle : Double;
  begin
    if aClockMark.Visible then
    begin
      i := 0;
      while i <= 59 do
      begin
        MarkAngle := cDegToRad * (((Round((i / 60) * 360)) + 90) mod 360);
        MarkX := Round(HalfWidth * (1 - (((100 - 2) / 100) * Cos(MarkAngle))));
        MarkY := Round(HalfHeight * (1 - (((100 - 2) / 100) * Sin(MarkAngle))));
        ckDraw.Canvas.MoveTo(MarkX, MarkY);
        aClockMark.Draw(ckDraw.Canvas, MarkX, MarkY);
        inc(i, Step);
      end;
    end;
  end;

begin
  with ckDraw.Canvas do
  begin
    Pen.Color   := FHourHand.Color;
    Pen.Width   := 1;

    if Assigned(FClockFace.Graphic) then
    begin
      R := ClientRect;
      //if FDrawMarks then
        //InflateRect(R, -3, -3);
      StretchDraw(R, FClockFace.Graphic);
      {ElRgn := CreateEllipticRgn(R.Left, R.Top, R.Right, R.Bottom);
      try
        SelectClipRgn(ckDraw.Canvas.Handle, ElRgn);
        StretchDraw(R, FClockFace.Graphic);
      finally
        DeleteObject(ElRgn);
      end;
      SelectClipRgn(ckDraw.Canvas.Handle, 0); // remove clipping region }
    end
    else
    begin
      Brush.Color := Color;
      FillRect(ClientRect);
    end;

    {draw marks}
    if FDrawMarks then
    begin
      with ClientRect do
      begin
        HalfWidth   := (Right - Left) shr 1;
        HalfHeight  := (Bottom - Top) shr 1;
      end;
      if HalfWidth < 1 then
        HalfWidth := 1;
      if HalfHeight < 1 then
        HalfHeight := 1;

      xDrawMarks(FMinuteMarks, 1);
      xDrawMarks(FHourMarks, 5);
    end;
  end;

  PaintHands(ckDraw.Canvas);

  begin
  Canvas.CopyMode := cmSrcCopy;
  Canvas.CopyRect(ClientRect, ckDraw.Canvas, ClientRect);
  end;
end;

procedure TGRCustomClock.PaintHands(ACanvas : TCanvas);
begin
  if FHourHand.Visible then
    FHourHand.Draw(aCanvas, ClientRect, FTime);

  if FMinuteHand.Visible then
    FMinuteHand.Draw(aCanvas, ClientRect, FTime);

  if FSecondHand.Visible then
    FSecondHand.Draw(aCanvas, ClientRect, FTime);
end;

procedure TGRCustomClock.SetActive(Value : Boolean);
begin
  if csLoading in ComponentState then
  begin
    FActive := Value;
    Exit;
  end;

  if Value <> FActive then
  begin
    FActive := Value;
    if Value then
    begin
      if FClockMode = cmClock then
        FTime := Now
      else
        FTime := 0;
    end;
    FTimer.Enabled := Value;
    Invalidate;
  end;
end;

procedure TGRCustomClock.SetBounds(ALeft, ATop, AWidth, AHeight : Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);

  if Assigned(ckDraw) then begin
    ckDraw.Width  := AWidth;
    ckDraw.Height := AHeight;
  end;

  Invalidate;
end;

procedure TGRCustomClock.SetClockFace(Value : TPicture);
begin
  FClockFace.Assign(Value);
  Invalidate;
end;

procedure TGRCustomClock.SetClockMode(Value : TClockMode);
begin
  if Value <> FClockMode then begin
    FClockMode := Value;
    Invalidate;
  end;
end;

procedure TGRCustomClock.SetDrawMarks(Value : Boolean);
begin
  if Value <> FDrawMarks then begin
    FDrawMarks := Value;
    Invalidate;
  end;
end;

procedure TGRCustomClock.SetTime(Value : TDateTime);
var
  Hour1, Minute1, Second1 : Word;
  Hour2, Minute2, Second2 : Word;
  MSecond : Word;
begin
  DecodeTime(Value, Hour1, Minute1, Second1, MSecond);
  DecodeTime(FTime, Hour2, Minute2, Second2, MSecond);
  if (Hour1 <> Hour2) or (Minute1 <> Minute2) or (Second1 <> Second2) then
  begin
    FTime := Value;

    if (Hour1 <> OldHour) then
      DoOnHourChange;
    OldHour := Hour1;
    if (Minute1 <> OldMinute) then
      DoOnMinuteChange;
    OldMinute := Minute1;
    if (Second1 <> OldSecond) then
      DoOnSecondChange;
    OldSecond := Second1;

    Invalidate;
  end;
end;

procedure TGRCustomClock.SetHourHand(Value: TGRHourClockHand);
begin
  if Value <> FHourHand then
  begin
    FHourHand.Assign(Value);
  end;
end;

procedure TGRCustomClock.SetMinuteHand(Value: TGRMinuteClockHand);
begin
  if Value <> FMinuteHand then
  begin
    FMinuteHand.Assign(Value);
  end;
end;

procedure TGRCustomClock.SetSecondHand(Value: TGRSecondClockHand);
begin
  if Value <> FSecondHand then
  begin
    FSecondHand.Assign(Value);
  end;
end;

procedure TGRCustomClock.SetHourMarks(Value: TGRHourMark);
begin
  if Value <> FHourMarks then
  begin
    FHourMarks.Assign(Value);
  end;
end;

procedure TGRCustomClock.SetMinuteMarks(Value: TGRMinuteMark);
begin
  if Value <> FMinuteMarks then
  begin
    FMinuteMarks.Assign(Value);
  end;
end;

procedure TGRCustomClock.SetMinuteOffset(Value : Integer);
begin
  if (Value <> FMinuteOffset) and (Abs(Value) <= 60) then begin
    FMinuteOffset := Value;
    Invalidate;
  end;
end;

procedure TGRCustomClock.SetTimeOffset(Value : Integer);
begin
  if (Value <> FTimeOffset) and (Abs(Value) <= 12) then begin
    FTimeOffset := Value;
    Invalidate;
  end;
end;

end.

